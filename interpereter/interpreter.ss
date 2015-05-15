; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env (lambda (x) x))))



; eval-exp is the main component of the interpreter
(define eval-exp
  (let ([identity-proc (lambda (x) x)])
    (lambda (exp env k)
      (cases expression exp
        [lit-exp (datum) (apply-k k datum)]
        [var-exp (id) (apply-k k
        (apply-env env id
          (lambda (x) x)
          (lambda () (apply-env global-env id
          (lambda (x) x) 
          (lambda () (eopl:error 'apply-env "variable not found in environment: ~s" id))))))] 
        [app-exp (rator rands)
          (eval-exp rator env (rator-k rands env k))]
        [let-exp (vars exps bodies)
          (let ([new-env
                  (extend-env vars 
                    (map (lambda (e) (eval-exp e env))
                      exps)
                    env)])
          (eval-bodies bodies new-env))]

        [letrec-exp (proc-names idss bodies letrec-body)
          (eval-exp letrec-body
            (extend-env-recursively 
              proc-names idss bodies env) k)]

        [if-exp (test-exp then-exp else-exp)
            (eval-exp test-exp env (if-k then-exp else-exp env k))]
        [if-exp-no-else (test-exp then-exp)
            (eval-exp test-exp env (if-no-else-k then-exp env k))]
        [lambda-exp (ids bodies)
          (apply-k k (closure ids bodies env))]
        [lambda-improper (args arg bodies) 
          (apply-k k (improper-closure args  arg bodies env))]
        [lambda-single (arg bodies) 
          (apply-k k (single-arg-closure arg bodies env))]
        [lambda-multi-bodies-exp (args body) 
          (apply-k k (multi-body-closure args body env))]
        [while-exp (test-exp body)
          (eval-exp test-exp env (while-k bodies exp env k))]
        [begin-exp (body) (eval-bodies body env)]
        [quote-exp (args)
          (apply-k k args)]
        [set!-exp (var val)
          (eval-exp val env (set!-k var env k))]
        [define-exp (var val)
          (eval-exp val env (define-k var k))]
        [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))))

(define apply-k
  (lambda (k val)
    (cases continuation k
      [identity-k () val]
      [if-k (then-exp else-exp env k)
        (if val 
          (eval-exp then-exp env k)
          (eval-exp else-exp env k))]
      [if-no-else-k (then-exp env k)
        (if val 
          (eval-exp then-exp env k)
          (apply-k k (void)))]
      [while-k (bodies loop env k)
        (if val 
          (eval-last (append bodies (list loop)) env k)
          (apply-k k (void)))]
      [rator-k (rands env k)
        (if (null? rands)
          (apply-k (rands-k val k) '())
          (eval-exp (car rands) env 
            (eval-rands-k (cdr rands) '() env (rands-k val k))))]
      [rands-k (proc-value k) 
        (apply-proc proc-value val k)]
      [set!-k (var env k)
        (env-edit-val var val env)
        (apply-k k (void))]
      [define-k (var k)
        (grow-global-env! var val)
        (apply-k k (void))]
      [eval-rands-k (rands args env k)
        (if (null? rands)
          (apply-k k (append args (list val)))
          (eval-exp (car rands) env (eval-rands-k (cdr rands) (append args (list val) ) env k)))]
      [eval-last-k (bodies env k)
        (if (null? bodies)
          (apply-k k val)
          (eval-exp (car bodies) env (eval-last-k (cdr bodies) env k)))]
      )))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env k)
    (map (lambda (e)
      (eval-exp e env k)) rands)))

(define eval-begin
  (lambda (ls env)
    (if (null? (cdr ls))
      (begin (eval-exp (car ls) env))
      (begin (eval-exp (car ls) env)
             (eval-begin (cdr ls) env)))))

(define eval-bodies
  (lambda (bodies env)
    (if (null? (cdr bodies))
        (eval-exp (car bodies) env)
        (begin (eval-exp (car bodies) env)
                (eval-bodies (cdr bodies) env)))))

(define eval-exp-while
  (lambda (test bodies env)
    (letrec ([helper (lambda (test entire-body rest env)
      (if (null? (cdr rest))
        (begin (eval-exp (car rest) env)
               (if (not (equal? #f (eval-exp test env)))
                   (helper test entire-body entire-body env)))
        (begin (eval-exp (car rest) env)
               (helper test entire-body (cdr rest) env))))])
    (if (not (equal? #f (eval-exp test env)))
      (helper test bodies bodies env)))))



; Apply a procedure to its arguments.
; At this point, we only have primitive procedures. 
; User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args k)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args k)]
      [closure (ids bodies env)
        (eval-exp bodies (extend-env vars args env) k)]
      [multi-body-closure (vars bodies env)
        (eval-last bodies (extend-env vars args env) k)]
      [single-arg-closure (var bodies env)
        (eval-last bodies (extend-env (list var) (list args) env) k)]
      [improper-closure   (vars var body env ) 
        (eval-last body (extend-env (append vars (list var)) (correct-args vars args) env) k)]
      [continuation-proc (con) (apply-k con (car args))]
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define (eval-last bodies env)
  (if (null? (cdr bodies))
    (eval-exp (car bodies) env)
    (begin (eval-exp (car bodies) env)
         (eval-last (cdr bodies) env))))

(define last
  (lambda (ls)
    (car (reverse ls))))

(define (correct-args vars args)
  (if (null? vars)
    (list args)
    (append (list (1st args)) (correct-args (cdr vars) (cdr args)))))


(define *prim-proc-names* '(+ - * / add1 sub1 zero? not = < <= > >= cons car cdr
                            caar cadr cdar cddr caaar caadr cadar cdaar caddr
                            cdadr cddar cdddr list null? assq eq? equal? atom?
                            length list->vector list? pair? procedure? vector->list
                            vector make-vector vector-ref vector? number? symbol?
                            set-car! set-cdr! vector-set! display newline map apply
                            or and quotient))



; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args k)
    (case prim-proc
      [(+) (apply-k k (apply + args))]
      [(-) (apply-k k (apply - args))]
      [(*) (apply-k k (apply * args))]
      [(/) (apply-k k (apply / args))]
      [(add1) (apply-k k (+ (1st args) 1))]
      [(sub1) (apply-k k (- (1st args) 1))]
      [(zero?) (apply-k k (zero? (1st args)))]
      [(not) (apply-k k (not (1st args)))]
      [(=) (apply-k k (apply = args))]
      [(<) (apply-k k (apply < args))]
      [(<=) (apply-k k (apply <= args))]
      [(>) (apply-k k (apply > args))]
      [(>=) (apply-k k (apply >= args))]
      [(cons) (apply-k k (cons (1st args) (2nd args)))]
      [(car) (apply-k k (car (1st args)))]
      [(cdr) (apply-k k (cdr (1st args)))]
      [(caar) (apply-k k (caar (1st args)))]
      [(cadr) (apply-k k (cadr (1st args)))]
      [(cdar) (apply-k k (cdar (1st args)))]
      [(cddr) (apply-k k (cddr (1st args)))]
      [(caaar) (apply-k k (caaar (1st args)))]
      [(caadr) (apply-k k (caadr (1st args)))]
      [(cadar) (apply-k k (cadar (1st args)))]
      [(cdaar) (apply-k k (cdaar (1st args)))]
      [(caddr) (apply-k k (caddr (1st args)))]
      [(cdadr) (apply-k k (cdadr (1st args)))]
      [(cddar) (apply-k k (cddar (1st args)))]
      [(cdddr) (apply-k k (cdddr (1st args)))]
      [(list) (apply-k k args)]
      [(null?) (apply-k k (null? (1st args)))]
      [(assq) (apply-k k (assq (1st args) (2nd args)))]
      [(eq?) (apply-k k (eq? (1st args) (2nd args)))]
      [(equal?) (apply-k k (equal? (1st args) (2nd args)))]
      [(atom?) (apply-k k (atom? (1st args)))]
      [(length) (apply-k k (length (1st args)))]
      [(list->vector) (apply-k k (list->vector (1st args)))]
      [(list?) (apply-k k (list? (1st args)))]
      [(pair?) (apply-k k (pair? (1st args)))]
      [(procedure?) (apply-k k (proc-val? (1st args)))]
      [(vector->list) (apply-k k (vector->list (1st args)))]
      [(vector) (apply-k k (apply vector args))]
      [(make-vector) (apply-k k (apply make-vector args))]
      [(vector-ref) (apply-k k (vector-ref (1st args) (2nd args)))]
      [(vector?) (apply-k k (vector? (1st args)))]
      [(number?) (apply-k k (number? (1st args)))]
      [(symbol?) (apply-k k (symbol? (1st args)))]
      [(set-car!) (apply-k k (set-car! (1st args) (2nd args)))]
      [(set-cdr!) (apply-k k (set-cdr! (1st args) (2nd args)))]
      [(vector-set!) (apply-k k (vector-set! (1st args) (2nd args) (3rd args)))]
      [(display) (apply-k k (display (1st args)))]
      [(newline) (apply-k k (newline))]
      [(quotient) (apply-k k (apply quotient args))]
      [(map) (apply-k k (map-proc (1st args) (cdr args)))]
      [(apply) (apply-k k (apply-proc (1st args) (2nd args)))]
      [(or) (apply-k k (if (null? args) #f
              (if (= (length args) 1) (1st args)
                  (or (1st args) (apply-prim-proc prim-proc (cdr args) (identity-k))))))]
      [(and) (apply-k k (if (null? args) #t
                (if (> (length args) 1)
                    (and (1st args) (apply-prim-proc prim-proc (cdr args)(identity-k)))
                    (if (1st args) (1st args) #f))))]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-proc)])))

(define map-proc
 (lambda (proc args)
  (if (null? args)
   args
   (cons (apply-proc proc (list (car args)) (identity-k)) (map-proc proc (cdr args))))))


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










