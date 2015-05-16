; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

(define parse-exp         
  (lambda (datum)
    (cond
	 [(symbol? datum) (var-exp datum)]
	 [(or (number? datum)(boolean? datum)(string? datum)) (lit-exp datum)]
     [(pair? datum)
      (cond
		[(eqv? 'quote (1st datum)) (lit-exp (2nd datum))]
		[(eqv? 'lambda (1st datum)) 
			(cond 
				[(or (null? (cdr datum)) (null? (cddr datum)))
					(eopl:error 'parse-exp "lambda-expression: incorrect length ~s" datum)]
				
				[(cond
					[(symbol? (2nd datum))(lambda-single
							(2nd datum)
							(map parse-exp (cddr datum)))]
					[(list? (2nd datum))
							(lambda-multi-bodies-exp
							(map 
								(lambda (x) 
									(if (list? x)
										(ref-arg (2nd x)) 
										(var-arg x))) 
								(2nd datum))
							(map parse-exp (cddr datum)))]
					[else (lambda-improper
							(get-car-of-pair (2nd datum))
							(get-last-of-pair (2nd datum))
							(map parse-exp (cddr datum)))])])]
		[(eqv? 'let (1st datum))
			(cond
				[(null? (cddr datum))
					(eopl:error 'parse-exp "let-expression has incorrect length ~s" datum)]
				[(symbol? (2nd datum))
					(named-let (2nd datum)(map car (3rd datum)) (map (lambda (x) (parse-exp (2nd x))) (3rd datum)) (map parse-exp (cdddr datum)))]
				[(not (or (list? (2nd datum)) (null?(2nd datum))))
					(eopl:error 'parse-exp "declarations in let-expression not a list ~s" datum)]	
				[(not (andmap (lambda (x) (list? x)) (2nd datum))) 
					(eopl:error 'parse-exp "declaration in let-exp is not a proper list ~s" datum)]
				[(not (andmap (lambda (x) (and (not (null? (cdr x)))(null? (cddr x)))) (2nd datum)))
					(eopl:error 'parse-exp "declaration in let-exp must be a list of length 2 ~s" datum)]
				[(not (andmap (lambda (x) (symbol? (car x))) (2nd datum)))
					(eopl:error 'parse-exp "vars in let-exp must be symbols ~s" datum)]
				[else (let-exp (map car (2nd datum)) (map (lambda (x) (parse-exp (2nd x))) (2nd datum)) (map parse-exp (cddr datum)))])]
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
				[else (let*-exp (map car (2nd datum)) (map (lambda (x) (parse-exp (2nd x))) (2nd datum)) (map parse-exp (cddr datum)))])]
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
				[else (letrec-exp 
						(map car (2nd datum)) 
						(map 2nd (map (lambda (x) (2nd x)) (2nd datum))) 
						(map (lambda (x) (parse-exp (2nd x))) (2nd datum)) 
						(parse-exp (3rd datum)))])]
		[(eqv? 'if (1st datum)) 
			(cond 
			[(or (null? (cddr datum)) (null? (cdr datum)))
				(eopl:error 'parse-exp "if-expression ~s does not have (only) test, then, and else" datum)]
			[(null? (cdddr datum))
				(if-exp-no-else
				(parse-exp (2nd datum))
				(parse-exp (3rd datum)))]
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
		[(eqv? 'cond (1st datum))
			(if (null? (cddr datum))
				(cond-exp 	(list (lit-exp #f))
							(list (lit-exp #f))
						    (parse-exp (2nd (2nd datum))))
				(cond-exp 
					(remove-last (map (lambda (x) (parse-exp (1st x))) (cdr datum)))
					(remove-last (map (lambda (x) (parse-exp (2nd x))) (cdr datum)))
					(last (map (lambda (x) (if (eqv? 'else (1st x)) (parse-exp (2nd x))))  (cdr datum)))))]
		[(eqv? 'while (1st datum))(while-exp 
			(parse-exp (2nd datum))
			(map parse-exp (cddr datum)))]
		[(eqv? 'case  (1st datum))(case-exp
			(parse-exp (2nd datum))
			(remove-last (map (lambda (x) (1st x)) (cddr datum)))
			(remove-last (map (lambda (x) (parse-exp (2nd x))) (cddr datum)))
			(parse-exp (last (cddr datum))))]
		[(eqv? 'begin (1st datum))
			(begin-exp (map parse-exp (cdr datum)))]
		[(eqv? 'define (1st datum))
			(define-exp (2nd datum) (parse-exp (3rd datum)))]
		[(eqv? 'or (1st datum))
			(or-exp (map parse-exp (cdr datum)))]
       [else (if (not (list? datum))
			(eopl:error 'parse-exp "Error in parse-exp: application ~s is not a proper list" datum)
			(app-exp (parse-exp (1st datum)) 
							(map parse-exp (cdr datum))))])]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define (remove-last ls)
		(if (null? (cdr ls))
			'()
			(cons (car ls) (remove-last (cdr ls)))))
(define (get-car-of-pair p)
	(if (pair?  p)
		(append (list (car p)) (get-car-of-pair (cdr p)))
		'() ))
(define (get-last-of-pair p)
	(if (pair? p)
		(get-last-of-pair (cdr p))
		p))

(define (unparse-exp exp)
	(cases expression exp
		[var-exp (id) id]
		[lambda-exp (ids body) (append (list 'lambda) 
 			(append (list (unparse-exp ids)) (unparse-exp body)))]
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
 		[app-exp (rator rand) (append (list (unparse-exp rator)) (unparse-exp rand))]
 		[while-exp (test-exp body) (cons (unparse-exp test-exp) (map unparse-exp body))]))

(define (literal? x)
	(or (number? x) (string? x) (null? x) (vector? x)(equal? #f x) 
	(equal? #t x) (symbol? x) (list? x)))






;-----------------------+
;                       |
;   SYNTAX EXPANSION    |
;                       |
;-----------------------+

(define (syntax-expand exp)
	(cases expression exp
		[let-exp (args exps body) 
			(app-exp 
				(lambda-multi-bodies-exp (map var-arg args) (map syntax-expand body))
				(map syntax-expand exps))]
		[let*-exp (args exps body)
			(if (null? args)
				(syntax-expand (begin-exp (map syntax-expand body)))
				(app-exp (lambda-exp (list (car args)) 
							(syntax-expand (let*-exp (cdr args) (cdr exps) body)))
						 (list (syntax-expand (car exps)))))]
		[lambda-exp (args body)
			(lambda-exp	args (syntax-expand body))]
		[lambda-multi-bodies-exp (args body)
			(lambda-multi-bodies-exp args (map syntax-expand body))]
		[lambda-single (arg body)
			(lambda-single arg (map syntax-expand body))]
		[lambda-improper (args arg body)
			(lambda-improper args arg (map syntax-expand body))]
		[if-exp (cond then else)
			(if-exp (syntax-expand cond)
					(syntax-expand then)
					(syntax-expand else))]
		[if-exp-no-else (cond then)
			(if-exp-no-else (syntax-expand cond)
							(syntax-expand then))]
		[app-exp (rator rands) 
			(app-exp (syntax-expand rator)
					 (map syntax-expand rands))]
		[while-exp (case body)
			(while-exp	(syntax-expand case) (map syntax-expand body))]
		[cond-exp (conds bodies else)
			(if (null? (cdr conds))
				(if-exp (syntax-expand (car conds))
						(syntax-expand (car bodies))
						(syntax-expand else))
				(if-exp (syntax-expand (car conds))
						(syntax-expand (car bodies))
						(syntax-expand (cond-exp (cdr conds) (cdr bodies) else))))]
		[case-exp (case conds bodies else)
		(if (null? conds)
			(syntax-expand (caaddr else))
			(let ([a (case-to-cond (syntax-expand case) (1st conds) (1st bodies))]) 
				(syntax-expand 
					(cond-exp 
						(map (lambda (x) (1st x)) a)
						(map (lambda (x) (2nd x)) a)
						(syntax-expand (case-exp case (cdr conds) (cdr bodies) else))))))]
		[begin-exp (body)  (app-exp (lambda-multi-bodies-exp '() (map syntax-expand body))
									'())]
		[letrec-exp (args idss exps body) (letrec-exp args idss (map syntax-expand exps) 
										(syntax-expand body))]
		[named-let (name args exps body) 
			(letrec-exp 
				(list name) 
				(list args) 
				(list (lambda-multi-bodies-exp (map var-arg args) (map syntax-expand body)))
				(app-exp (lambda-multi-bodies-exp (map var-arg args) (map syntax-expand body)) (map syntax-expand exps)))]
		[define-exp (sym val)
			(define-exp sym (syntax-expand val))]
		[or-exp (bodies)
			(if (null? (cdr bodies))
			(app-exp 
				(lambda-exp (list 'a) (if-exp (var-exp 'a) (var-exp 'a) (lit-exp #f)))
				(list (syntax-expand (car bodies))))
			(app-exp 
				(lambda-exp (list 'a) (if-exp (var-exp 'a) (var-exp 'a) (syntax-expand (or-exp (cdr bodies)))))
				(list (syntax-expand (car bodies)))))]
		[else exp]))
	
(define (case-to-cond case cases body)
	(if (null? cases)
		'()
		(append 
			(list (list 
				(app-exp (var-exp 'equal?) (list  case (lit-exp (car cases))))
				(syntax-expand body)))
			(case-to-cond case (cdr cases) body))))



