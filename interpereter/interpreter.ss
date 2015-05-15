; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env (lambda (x) x))))

(define global-env init-env)

; eval-exp is the main component of the interpreter
(define eval-exp
  (let ([identity-proc (lambda (x) x)])
    (lambda (exp env k)
      (cases expression exp
        [lit-exp (datum) (apply-k k datum)]
        [var-exp (id) ;look up its value.
          (apply-env env 
            id 
            k
            (lambda () (error 'apply-env "variable ~s is not bound" id)))] 

        [app-exp (rator rands)
          (eval-exp rator (rator-k rands env k))]

        [let-exp (vars exps bodies)
          (let ([new-env
                  (extend-env vars 
                    (map (lambda (e) (eval-exp e env))
                      exps)
                    env)])
          (eval-bodies bodies new-env))]

        [letrec-exp (proc-names idss bodies letrec-body)
          (car (map (lambda (e) (eval-exp e
            (extend-env-recursively
              proc-names idss bodies env))) letrec-body))]

        [if-exp (text-exp then-exp else-exp)
            (apply-k (test-k (then-exp) (else-exp) (env) (k)) text-exp)]
        [if-exp-no-else (test-exp then-exp)
            (apply-k (test-no-else-k (then-exp) (env) (k)) test-exp)]
        [lambda-exp (ids bodies)
          (apply-k k (closure ids bodies env))]
        [lambda-improper (args arg bodies) 
          (apply-k k (improper-closure args  arg bodies env))]
        [lambda-single (arg bodies) 
          (apply-k k (single-arg-closure arg bodies env))]
        [lambda-multi-bodies-exp (args body) 
          (apply-k k (multi-body-closure args body env))]
        [while-exp (test-exp body) (eval-exp-while test-exp body env)]
        [begin-exp (body) (eval-bodies body env)]
        [quote-exp (args)
          (apply-k k args)]
        [set!-exp (variable new-val)
          (set-box!
            (apply-env-ref env variable (lambda (x) x) 
              (lambda () (eopl:error "set! unfound input variable")))
            (eval-exp new-val env k))]
        [define-exp (new-variable new-val)
          (apply-env-ref env new-variable (lambda (x) eopl:error "attempt tp redefine a variable using define")
            (lambda ()
              (cases enviorment global-env
                (extended-env-record (syms vals env)
                  (begin 
                    (set! global-env (extend-env (cons new-variable syms)
                      (cons (eval-exp new-value env k) (map unbox vals))
                      (empty-env))))))))]
        [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))))

(define apply-k
  (lambda (k val)
    (cases continuation k
      [rator-k (rands env k)
        (eval-rands rands
                    env
                    (rands-k val k))]
      [rands-k (proc-value k)
        (apply-proc proc-value val k)]
      [test-k (then-exp else-exp env k)
        (if val 
          (eval-exp then-exp env k)
          (eval-exp else-exp env k))]
      [test-no-else-k (then-exp env k)
        (if val (eval-exp then-exp env k))]
      [else (k val)]
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

(define reset-global-env
  (lambda ()
    (set! global-env (extend-env
                        *prim-proc-names*
                        (map prim-proc *prim-proc-names* (empty-env))))))

; Apply a procedure to its arguments.
; At this point, we only have primitive procedures. 
; User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
      [closure (ids bodies env)
        (eval-begin bodies (extend-env params args env))]
      [applied-continuation (k) (apply-continuation k (car args))]
      [multi-body-closure (vars bodies env)
        (eval-last bodies (extend-env vars args env))]
      [single-arg-closure (var bodies env)
        (eval-last bodies (extend-env (list var) (list args) env))]
      [improper-closure   (vars var body env ) 
        (if (null? vars)
          (last (eval-exp body (extend-env (append vars (list var)) (correct-args vars args) env)))
          (eval-last body (extend-env (append vars (list var)) (correct-args vars args) env)))]

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

(define init-env         ; for now, our initial global environment only contains 
  (extend-env   ; procedure names. Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc  
          *prim-proc-names*)
     (empty-env)))

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
      [(map) (map-prim (1st args) (cdr args))]
      [(apply) (apply-proc (1st args) (2nd args))]
      [(or) (if (null? args) #f
              (if (= (length args) 1) (1st args)
                  (or (1st args) (apply-prim-proc prim-proc (cdr args)))))]
      [(and) (if (null? args) #t
                (if (> (length args) 1)
                    (and (1st args) (apply-prim-proc prim-proc (cdr args)))
                    (if (1st args) (1st args) #f)))]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-proc)])))

(define map-prim
  (lambda (fn ls . adt)
    (if (null? adt)
      [let map-noadt ((ls (car ls)))
        (if (null? ls)
          '()
          (cons (apply-proc fn (list (car ls)))
                (map-noadt (cdr ls))))]
      [let map-adt ((ls (car ls)) (adt adt))
        (if (null? ls)
          '()
          (cons (apply-proc fn (list (car ls)) (map-prim car adt))
                (map-prim (cdr ls)
                          (map-prim cdr adt))))])))

(define build-args-list
  (lambda (args)
    (if (null? args)
      '()
      (if (null? (cdr args))
        (car args)
        (cons (car args) (build-args-list))))))

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










