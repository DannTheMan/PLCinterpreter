
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
  [let*-exp
    (args (list-of symbol?))
    (exps (list-of expression?))
    (body (list-of expression?))]
  [letrec-exp
    (proc-names (list-of symbol?))
    (idss (list-of (list-of symbol?)))
    (bodies (list-of expression?))
    (letrec-body expression?)]
  [if-exp
        (test-exp expression?)
        (then-exp expression?)
        (else-exp expression?)]
  [if-exp-no-else
        (test-exp expression?)
        (then-exp expression?)]
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
    (args (list-of symbol?))
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
    (id symbol?)
    (value expression?)]
  [define-exp
    (var symbol?)
    (val expression?)]
  [quote-exp
    (args scheme-value?)]
  )

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc 
    (name symbol?)]
  [closure
    (args (list-of symbol?))
    (body expression?)
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
  [continuation-proc
    (k continuation?)])
	 
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
    (syms (list-of symbol?))
    (vals (list-of scheme-value?))
    (env environment?)]
  [recursively-extended-env-record
    (proc-names (list-of symbol?))
    (idss (list-of (list-of symbol?)))
    (bodies (list-of expression?))
    (old-env environment?)]
  [global-env-record
    (syms (list-of symbol?))
    (vals (list-of scheme-value?))])

;Continuation Datatype

(define-datatype continuation continuation?
  [if-k 
    (then-exp expression?)
    (else-exp expression?)
    (env environment?)
    (k continuation?)]
  [if-no-else-k 
    (then-exp expression?)
    (env environment?)
    (k continuation?)]
  [while-k
    (bodies (list-of expression?))
    (loop expression?)
    (env environment?)
    (k continuation?)]
  [rator-k 
    (rands (list-of expression?))
    (env environment?)
    (k continuation?)]
  [rands-k
    (proc-value scheme-value?)
    (k continuation?)]
  [set!-k
    (var symbol?)
    (env environment?)
    (k continuation?)]
  [define-k
    (var symbol?)
    (k continuation?)]
  [eval-rands-k
    (rands list?)
    (args list?)
    (env environment?)
    (k continuation?)]
  [eval-last-k
    (bodies list?)
    (env environment?)
    (k continuation?)]
  [identity-k]
  
  )
