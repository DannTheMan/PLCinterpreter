; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
      proc-names idss bodies old-env)))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env   ; procedure names. Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc  
          *prim-proc-names*)
     (empty-env)))

(define reset-global-env
  (lambda ()
    (set! global-env (init-env))))

(define global-env init-env)

(define grow-global-env!
  (lambda (var val)
    (cases environment global-env
    [global-env-record(syms vals)
    (set! global-env (global-env-record (append (list var) syms)
                     (append (list val) vals)))]
    [else
    eopl:error 'grow-global-env! "Global Environment Faild on Load"])))

(define env-edit-val
  (lambda (var val env)
    (cases environment env
      [empty-env-record()
        (env-edit-val var val global-env)]
      [global-env-record(syms vals)
        (let ([pos (list-find-position var syms)])
          (if (number? pos)
            (begin 
              (set-val-at-pos! pos val vals) #t)
              #f))]
      [extended-env-record(syms vals env)
        (or (env-edit-val var val env)
            (let ([pos (list-find-position var syms)])
              (if (number? pos)
                (begin (set-val-at-pos! pos val vals) #t)
                #f)))]
      [recursively-extended-env-record (proc-names idss bodies old-env)
        (env-edit-val var val old-env)]
      [else #f])))

(define set-val-at-pos!
  (lambda (pos val ls)
    (if (eq? pos 0)
      (set-car! ls val)
      (set-val-at-pos! (- pos 1) val (cdr ls)))))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	   (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      [empty-env-record ()
        (fail)]
      [global-env-record(syms vals)
        (let ([pos (list-find-position sym syms)])
          (if (number? pos)
            (succeed (list-ref vals pos))
            (fail)))]
      [recursively-extended-env-record 
        (procnames idss bodies old-env)
          (let ([pos 
                  (list-find-position sym procnames)])
            (if (number? pos)
                (apply-proc (closure (list-ref idss pos)
                  (list-ref bodies pos)
                  env) procnames (identity-k))
                (apply-env old-env sym succeed fail)))]
      [extended-env-record (syms vals env)
      	(let ((pos (list-find-position sym syms)))
            	 (if (number? pos)
      	         (succeed (list-ref vals pos))
      	         (apply-env-ref env sym succeed fail)))])))

(define global-env-checker
  (lambda (element succed fail)
    (cases environment global-env
      (extended-env-record (syms vals env)
        (begin 
          (let ([pos (list-find-position element syms)])
            (if (number? pos)
              (succeed (list-ref vals pos))
              (fail))))))))

