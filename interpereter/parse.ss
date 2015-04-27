; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp
	(lambda (datum)
		(cond
		[(symbol? datum) (var-exp datum)]
		[(literal? datum) (lit-exp datum)]
		[(pair? datum)
			(cond
		[(eqv? 'lambda (1st datum)) 
		 (cond 
			[(or (null? (cdr datum)) (null? (cddr datum)))
			 (eopl:error 'parse-exp "lambda-expression: incorrect length ~s" datum)]
			[(and (list? (2nd datum))(not (andmap (lambda (x) (symbol? x)) (2nd datum))))
			 (eopl:error 'parse-exp "lambda's formal arguments ~s must all be symbols" (2nd datum))]
			[(lambda-exp 
				(parse-exp (2nd datum))
				(parse-exp (cddr datum)))])]
		[(eqv? 'let (1st datum))
		 (cond
			[(null? (cddr datum))
			 (eopl:error 'parse-exp "let-expression has incorrect length ~s" datum)]
			[(not (or (list? (2nd datum)) (null?(2nd datum))))
			 (eopl:error 'parse-exp "declarations in let-expression not a list ~s" datum)] 
			[(not (andmap (lambda (x) (list? x)) (2nd datum))) 
			 (eopl:error 'parse-exp "declaration in let-exp is not a proper list ~s" datum)]
			[(not (andmap (lambda (x) (and (not (null? (cdr x)))(null? (cddr x)))) (2nd datum)))
			 (eopl:error 'parse-exp "declaration in let-exp must be a list of length 2 ~s" datum)]
			[(not (andmap (lambda (x) (symbol? (car x))) (2nd datum)))
			 (eopl:error 'parse-exp "vars in let-exp must be symbols ~s" datum)]
			[else (let-exp (map parse-exp (2nd datum)) (parse-exp (cddr datum)))])]
		[(eqv? 'let* (1st datum))
		 (cond
			[(null? (cddr datum))
			 (eopl:error 'parse-exp "let*-expression has incorrect length ~s" datum)]
			[(not (or (list? (2nd datum)) (null?(2nd datum))))
			 (eopl:error 'parse-exp "declarations in let*-expression not a list ~s" datum)]
			[(not (andmap (lambda (x) (list? x)) (2nd datum))) 
			 (eopl:error 'parse-exp "declaration in let*-exp is not a proper list ~s" datum)]
			[(not (andmap (lambda (x) (and (not (null? (cdr x)))(null? (cddr x)))) (2nd datum)))
			 (eopl:error 'parse-exp "declaration in let*-exp must be a list of length 2 ~s" datum)]
			[(not (andmap (lambda (x) (symbol? (car x))) (2nd datum)))
			 (eopl:error 'parse-exp "vars in let*-exp must be symbols ~s" datum)]
			[else (let*-exp (map parse-exp (2nd datum)) (parse-exp (cddr datum)))])]
		[(eqv? 'letrec (1st datum))
		 (cond
			[(null? (cddr datum))
			 (eopl:error 'parse-exp "letrec-expression has incorrect length ~s" datum)]
			[(not (or (list? (2nd datum)) (null?(2nd datum))))
			 (eopl:error 'parse-exp "declarations in letrec-expression not a list ~s" datum)] 
			[(not (andmap (lambda (x) (list? x)) (2nd datum))) 
			 (eopl:error 'parse-exp "declaration in letrec-exp is not a proper list ~s" datum)]
			[(not (andmap (lambda (x) (and (not (null? (cdr x)))(null? (cddr x)))) (2nd datum)))
			 (eopl:error 'parse-exp "declaration in letrec-exp must be a list of length 2 ~s" datum)]
			[(not (andmap (lambda (x) (symbol? (car x))) (2nd datum)))
			 (eopl:error 'parse-exp "vars in letrec-exp must be symbols ~s" datum)]
			[else (letrec-exp (map parse-exp (2nd datum)) (parse-exp (cddr datum)))])]
		[(eqv? 'if (1st datum)) 
		 (cond 
		 [(or (null? (cddr datum)) (null? (cdr datum)))
			(eopl:error 'parse-exp "if-expression ~s does not have (only) test, then, and else" datum)]
		 [(null? (cdddr datum))
			(if-exp 
			(parse-exp (2nd datum))
			(parse-exp (3rd datum))
			(parse-exp '()))]
		 [else
			(if-exp 
			(parse-exp (2nd datum))
			(parse-exp (3rd datum))
			(parse-exp (4th datum)))])]
		[(eqv? 'set! (1st datum))
		 (cond 
			[(or (null? (cddr datum)) (not(null?(cdddr datum))))
			 (eopl:error 'parse-exp "set! expression ~s does not have (only) variable and expression" datum)]
			[else
			 (set!-exp 
			 (2nd datum)
			 (parse-exp (3rd datum)))])]
				 [else (if (not (list? datum))
		 (eopl:error 'parse-exp "Error in parse-exp: application ~s is not a proper list" datum)
		 (app-exp (parse-exp (1st datum))(parse-exp (cdr datum))))])]
			 [else (eopl:error 'parse-exp "bad expression: ~s" datum)]))) 

(define (unparse-exp exp)
	(cases expression exp
		[var-exp (id) id]
		[lambda-exp (id body) (append (list 'lambda) 
 			(append (list (unparse-exp id)) (unparse-exp body)))]
		[lit-exp (id) id]
 		[let-exp (args body) (append (list 'let (map unparse-exp args)) (unparse-exp body))]
 		[let*-exp (args body) (append (list 'let* (map unparse-exp args)) (unparse-exp body))]
 		[letrec-exp (args body) (append (list 'letrec (map unparse-exp args)) (unparse-exp body))]
 		[if-exp (cond then else) (if (null?(unparse-exp else))
			(list 'if
	 			(unparse-exp cond)
	 			(unparse-exp then))
			(list 'if
	 			(unparse-exp cond)
	 			(unparse-exp then)
	 			(unparse-exp else)))]
 		[set!-exp (id value) (list 'set! id value)]
 		[app-exp (rator rand) (append (list (unparse-exp rator)) (unparse-exp rand))]))

(define (literal? x)
 	(or (number? x) (list? x) (string? x) (symbol? x) (null? x) (vector? x)(equal? #f x) (equal? #t x)))









