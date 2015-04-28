
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
  [if-exp
        (text-exp expression?)
        (then-exp expression?)
        (else-exp expression?)]
  [if-exp-no-else
        (test-exp expression?)
        (then-exp expression?)]
  [lambda-exp
    (ids list?)
    (body expression?)]
  )

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc (name symbol?)]
  [closure
    (args (list-of symbol?))
    (body expression?)
    (env environment?)])
	 
	 
	 
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vals (list-of scheme-value?))
    (env environment?)))