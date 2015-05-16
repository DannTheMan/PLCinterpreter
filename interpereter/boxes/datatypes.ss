
;; Parsed expression datatypes

(define-datatype  expression  expression? 
  [var-exp
    (id symbol?)]
  [lambda-exp
    (args list?)
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
  [lit-exp
    (id literal?)]
  [let-exp
    (args (list-of symbol?))
    (exps (list-of expression?))
    (body (list-of expression?))]
  [named-let
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
    (cond expression?)
    (then expression?)
    (else expression?)]
  [if-exp-no-else
    (cond expression?)
    (then expression?)]
  [set!-exp
    (id symbol?)
    (value expression?)]
  [cond-exp
    (conds (list-of expression?))
    (bodies (list-of expression?))
    (else expression?)]
  [case-exp
    (case expression?)
    (conds (list-of list?))
    (bodies (list-of expression?))
    (else expression?)]
  [while-exp
    (case expression?)
    (body (list-of expression?))]
  [app-exp
    (rator expression?)
    (rand (list-of  expression?))]
  [begin-exp
    (body (list-of expression?))]
  [define-exp
    (var symbol?)
    (val expression?)]
  [or-exp
    (body (list-of expression?))])

(define (literal? x)
  (or (number? x) (string? x) (null? x) (vector? x)(equal? #f x) 
  (equal? #t x) (symbol? x) (list? x)))
	
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
  (empty-env-record)
  (recursively-extended-env-record
  (proc-names (list-of symbol?))
  (idss (list-of (list-of symbol?)))
  (bodies (list-of expression?))
  (env environment?))
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?))
  (global-env-record
    (syms (list-of symbol?))
    (vals (list-of scheme-value?))))

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
  [identity-k])
