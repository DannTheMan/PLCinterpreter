; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env))))



; eval-exp is the main component of the interpreter
(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
        (apply-env env id ;look up its value.
          (lambda (x) (unbox x)) ; procedure to call if id is in the environment 
          (lambda () (apply-env global-env id
            (lambda (x) (unbox x))
            (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
              "variable not found in environment: ~s"
         id)))))] 
    [if-exp (cond then else)
        (if (eval-exp cond env)
          (eval-exp then env)
          (eval-exp else env))]
    [if-exp-no-else (cond then)
        (if (eval-exp cond env)
          (eval-exp then env))]
    [let-exp (args exps bodies) 
              (let  ([extended-env (extend-env args
                (map (lambda (x) (eval-exp x env)) exps) env)])
              (last (map (lambda (x) (eval-exp x extended-env)) bodies)))]
      [lambda-improper (args arg bodies) (improper-closure args  arg bodies env)]
    [lambda-single (arg bodies) (single-arg-closure arg bodies env)]
    [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
      (if (expression? proc-value)
        (apply-proc (eval-exp proc-value env) args)
      (if (proc-val? proc-value)
        (apply-proc proc-value args)
        proc-value)))]
    [lambda-exp (args body) (closure args body env)]
    [lambda-multi-bodies-exp (args body) (multi-body-closure args body env)]
    [while-exp (case bodies) (if (eval-exp case env)
                  (eval-last  (append bodies (list exp)) env))]
    [letrec-exp (proc-names idss bodies letrec-body)
      (eval-exp letrec-body
        (extend-env-recursively 
          proc-names idss bodies env))]
    [set!-exp(var val)
        (env-edit-val var (eval-exp val env) env)]
    [define-exp(var val)
        (grow-global-env! var (eval-exp val env))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

(define (last ls)
  (if (not (null? ls))
    (if (null? (cdr ls))
      (car ls)
      (last (cdr ls)))))

; evaluate the list of operands, putting results into a list
(define (rand-eval x env)
  (cases expression x
    [var-exp (id) 
      (apply-env env id
        (lambda (x) x)
        (lambda () (apply-env global-env id
          (lambda (x) x)
          (lambda () (eopl:error 'apply-env
              "variable not found in environment: ~s"
         id)))))]
    [else (box (eval-exp x env))]))

(define eval-rands
  (lambda (rands env)
  (map (lambda (x) (rand-eval x env)) rands)))


; Apply a procedure to its arguments.
; At this point, we only have primitive procedures. 
; User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op (map unbox args))]
    [closure (vars body env) 
      (eval-exp  body (extend-env vars args env))]
    [multi-body-closure (vars bodies env)
      (eval-last bodies (extend-env (map 2nd vars) (map choose-box vars args) env))]
    [single-arg-closure (var bodies env)
      (last (map (lambda (x) (eval-exp x  (extend-env (list var) (list (map unbox args)) env))) bodies))]
      [improper-closure   (vars var body env ) 
      (last (map (lambda (x) (eval-exp x (extend-env (append vars (list var)) (correct-args vars (map unbox args)) env))) body))]
      [else (eopl:error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define (choose-box var arg)
  (cases lambda-args var
    [ref-arg (id) arg]
    [var-arg (id) (unbox arg)]))

(define (eval-last bodies env)
  (if (null? (cdr bodies))
    (eval-exp (car bodies) env)
    (begin (eval-exp (car bodies) env)
         (eval-last (cdr bodies) env))))

(define (correct-args vars args)
  (if (null? vars)
    (list args)
    (append (list (1st args)) (correct-args (cdr vars) (cdr args)))))


(define *prim-proc-names* '(+ - * / quotient or and add1 sub1 zero? not = < > <= >= cons car cdr list null? assq
       eq? equal? atom? length list->vector list? pair? procedure? vector->list
       vector make-vector vector-ref vector? number? symbol? set-car! set-cdr!
       vector-set! display newline caar cadr cdar cddr caaar caadr cadar cdaar
       caddr cdadr cddar cdddr quote apply map eqv? append list-tail call/cc exit))

(define make-init-env
  (lambda()
    (global-env-record           
    *prim-proc-names*
    (map box (map prim-proc      
    *prim-proc-names*)))))


; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(zero?) (zero? (1st args))]
      [(not) (not (1st args))]
      [(=) (apply = args)]
      [(<) (apply < args)]
      [(<=) (apply <= args)]
      [(>) (apply > args)]
      [(>=) (apply >= args)]
      [(cons) (cons (1st args) (2nd args))]
      [(car) (car (1st args))]
      [(cdr) (cdr (1st args))]
      [(caar) (caar (1st args))]
      [(cadr) (cadr (1st args))]
      [(cdar) (cdar (1st args))]
      [(cddr) (cddr (1st args))]
      [(caaar) (caaar (1st args))]
      [(caadr) (caadr (1st args))]
      [(cadar) (cadar (1st args))]
      [(cdaar) (cdaar (1st args))]
      [(caddr) (caddr (1st args))]
      [(cdadr) (cdadr (1st args))]
      [(cddar) (cddar (1st args))]
      [(cdddr) (cdddr (1st args))]
      [(list) args]
      [(null?) (null? (1st args))]
      [(assq) (assq (1st args) (2nd args))]
      [(eq?) (eq? (1st args) (2nd args))]
      [(equal?) (equal? (1st args) (2nd args))]
      [(atom?) (atom? (1st args))]
      [(length) (length (1st args))]
      [(list->vector) (list->vector (1st args))]
      [(list?) (list? (1st args))]
      [(pair?) (pair? (1st args))]
      [(procedure?) (proc-val? (1st args))]
      [(vector->list) (vector->list (1st args))]
      [(vector) (apply vector args)]
      [(make-vector) (apply make-vector args)]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(vector?) (vector? (1st args))]
      [(number?) (number? (1st args))]
      [(symbol?) (symbol? (1st args))]
      [(set-car!) (set-car! (1st args) (2nd args))]
      [(set-cdr!) (set-cdr! (1st args) (2nd args))]
      [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
      [(display) (display (1st args))]
      [(newline) (newline)]
      [(quotient) (apply quotient args)]
      [(map) (map-proc (1st args) (cdr args))]
      [(apply) (apply-proc (1st args) (2nd args))]
      [(or) (if (= (length args) 0) #f
               (if (= (length args) 1) (1st args)
               (or (1st args) (apply-prim-proc prim-proc (cdr args)))))]
      [(and) (if (= (length args) 0) #t
                (if (> (length args) 1) (and (1st args) (apply-prim-proc prim-proc (cdr args)))
                    (if (1st args) (1st args) #f)))]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-proc)])))

(define map-proc
 (lambda (proc args)
  (if (null? args)
   args
   (cons (apply-proc proc (list (car args))) (map-proc proc (cdr args))))))

(define rep  ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (syntax-expand (parse-exp (read))))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))

(define reset-global-env
 (lambda () (set! global-env (make-init-env))))

(define global-env (make-init-env))
