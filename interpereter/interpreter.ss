; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form
              (empty-env))))

; eval-exp is the main component of the interpreter
(define eval-exp
  (let ([identity-proc (lambda (x) x)])
    (lambda (exp env)
      (cases expression exp
        [lit-exp (datum) datum]
        [var-exp (id)
          (apply-env env 
            id; look up its value.
            (lambda (x) x) ; procedure to call if id is in the environment 
            (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
                  "variable not found in environment: ~s"
          id)))]
        [app-exp (rator rands)
          (let ([proc-value (eval-exp rator env)]
                [args (eval-rands rands env)])
          (if (proc-val? proc-value)
            (apply-proc proc-value args)
            proc-value))]
        [let-exp (vars exps bodies)
          (let ([new-env
                  (extend-env vars 
                    (eval-rands exps env)
                    env)])
          (eval-bodies bodies new-env))]
        [if-exp (test-exp then-exp else-exp)
            (if (eval-exp test-exp env)
                (eval-exp then-exp env)
                (eval-exp else-exp env))]
        [lambda-exp (ids bodies)
          (closure ids bodies env)]
        [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map 
        (lambda (x) eval-exp x env)
      rands)))

(define eval-bodies
  (lambda (bodies env)
    (if (null? (cdr bodies))
        (eval-exp (car bodies) env)
        (begin (eval-exp (car bodies) env)
                (eval-bodies (cdr bodies) env)))))

; Apply a procedure to its arguments.
; At this point, we only have primitive procedures. 
; User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
      [closure (ids bodies env)
        (let ([new-env
            (extend-env id
                args env)])
        (eval-bodies bodies new-env))]
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * / add1 sub1 zero? not = < <= > >= cons car cdr
                            caar cadr cdar cddr caaar caadr cadar cdaar caddr
                            cdadr cddar cdddr list null? assq eq? equal? atom?
                            length list->vector list? pair? procedure? vector->list
                            vector make-vector vector-ref vector? number? symbol?
                            set-car! set-cdr! vector-set! display newline))

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
      [(procedure?) (proc-val? (1st-args))]
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
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))

(define rep  ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))










