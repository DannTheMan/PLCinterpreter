
;; Parsed expression datatypes

(define-datatype expression expression?
  [var-exp        ; variable references
   (id symbol?)]

  [lit-exp        ; "Normal" data.  Did I leave out any types?
   (datum literal?)]

  [app-exp        ; applications
   (rator expression?)
   (rands (list-of expression?))]

  [let-exp
    (vars (list-of symbol?))
    (exps (list-of expression?))
    (bodies (list-of expression?))]

  [named-let-exp
    (name symbol?)
    (args (list-of symbol?))
    (exps (list-of expression?))
    (body (list-of expression?))]
  [letrec-exp
    (proc-names (list-of symbol?))
    (idss (list-of (list-of symbol?)))
    (bodies (list-of expression?))
    (letrec-body (list-of expression?))]
  [if-exp
        (text-exp expression?)
        (then-exp expression?)
        (else-exp expression?)]
  [if-exp-no-else
        (test-exp expression?)
        (then-exp expression?)]
  [else-exp
    (bodies (list-of expression?))]
  [while-exp
    (test-exp expression?)
    (body (list-of expression?))]
  [begin-exp
    (bodies (list-of expression?))]
  [case-exp
    (key expression?)
    (conds (list-of list?))
    (bodies (list-of expression?))
    (else-clause expression?)]
  [cond-exp
    (conds (list-of expression?))
    (bodies (list-of expression?))
    (else-exp expression?)]
  [lambda-exp
    (ids list?)
    (body expression?)]
  [lambda-multi-bodies-exp
    (args list?)
    (body (list-of expression?))]
  [lambda-single
    (arg symbol?)
    (body (list-of expression?))]
  [lambda-improper
    (args list?)
    (arg symbol?)
    (body (list-of expression?))]
  [lambda-sym-exp
    (syms (list-of symbol?))
    (body (list-of expression?))
    (vals (list-of number?))]
  [set!-exp
    (variable symbol?)
    (new-val expression?)]
  [define-exp
    (new-variable symbol?)
    (new-val expression?)]
  [quote-exp
    (args scheme-value?)]
  )

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc (name symbol?)]
  [closure
    (args sym-or-ls?)
    (body exp-or-ls?)
    (env environment?)]
  [multi-body-closure
    (args (list-of symbol?))
    (body (list-of expression?))
    (env environment?)]
  [single-arg-closure
    (arg symbol?)
    (body (list-of expression?))
    (env environment?)]
  [improper-closure
    (args (list-of symbol?))
    (arg symbol?)
    (body (list-of expression?))
    (env environment?)]
  [applied-continuation
    (k continuation?)])
	 
	(define sym-or-ls?
    (lambda (x)
      (or (symbol? x) (list-of symbol? x))))

  (define exp-or-ls?
    (lambda (x)
      (or (expression? x) ((list-of expression?) x))))
	 
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
    (syms improper-or-regular)
    (vals (list-of box?))
    (env environment?)]
  [recursively-extended-env-record
    (proc-names (list-of symbol?))
    (idss (list-of (list-of symbol?)))
    (bodies (list-of expression?))
    (old-env environment?)])

(define improper?
  (lambda (x)
    (and (not (list? x)) (pair? x))))

(define improper-or-regular
  (lambda (x)
    (or ((list-of symbol?) x) (improper? x))))

;Continuation Datatype

(define-datatype continuation continuation?
  [test-k 
    (then-exp expression?)
  (else-exp expression?)
  (env environment?)
  (k continuation?)]
  [rator-k
    (rands (list-of expression?))
  (env environment?)
  (k continuation?)]
  
  [rands-k
    (proc-value scheme-value?)
  (k continuation?)]
  [test-no-else-k
    (then-exp expression?)
  (env environment?)
  (k continuation?)]
  
  )
