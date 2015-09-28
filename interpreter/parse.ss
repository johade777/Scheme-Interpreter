; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 2-list?
	(lambda (ls)
		(cond
			[(not (list? ls)) #f]
			[(null? ls) #f]
			[(null? (cdr ls)) #f]
			[(not (null? (cddr ls))) #f]
			[else #t])))			
		
; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp         
  (lambda (datum)
    (cond
     [(symbol? datum) 
		(var-exp datum)]
	[(string? datum)
		(lit-exp datum)]
     [(number? datum) 
		(lit-exp datum)]
	[(boolean? datum)
		(lit-exp datum)]
	[(vector? datum)
		(lit-exp datum)]
     [(pair? datum)
      (cond
		[(not (list? datum)) (eopl:error 'parse-exp "Improper List Exception: ~s" datum) ]
		[(eqv? (car datum) 'quote) (lit-exp (cadr datum))] 
		[(eqv? (car datum) 'lambda)
			(cond
				[(list? (cadr datum))
					(cond
						[(not ((list-of symbol?) (cadr datum))) (eopl:error 'parse-exp "Invalid Lambda variables: ~s" datum)]
						[(null? (cddr datum)) (eopl:error 'parse-exp "Lambda expression missing body: ~s" datum)]
						[else (lambda-exp (cadr datum) (map parse-exp (cddr datum)))])]
						
				[(symbol? (cadr datum))
					(cond
						[(null? (cddr datum)) (eopl:error 'parse-exp "Lambda expression missing body: ~s" datum)]
						[else (lambda-many-exp (cadr datum) (map parse-exp (cddr datum)))])]
				[else 
					(cond 
						[(not (pair? (cadr datum))) (eopl:error 'parse-exp "Invalid Lambda variables: ~s" datum)]
						[(null? (cddr datum)) (eopl:error 'parse-exp "Lambda expression missing body: ~s" datum)]
						[else (lambda-improp-exp (cadr datum) (map parse-exp (cddr datum)))])])]

		[(eqv? (car datum) 'let)
			(cond
				[(list? (cadr datum))  ; regular let
					(cond
						[(not ((list-of 2-list?) (cadr datum))) (eopl:error 'parse-exp "Invalid let variables: ~s" datum)]
						[(not (andmap symbol? (map car (cadr datum)))) (eopl:error 'parse-exp "Invalid let symbols: ~s" datum)]
						[(not (> (length datum) 2)) (eopl:error 'parse-exp "Invalid let length: ~s" datum)]
						[else (let-exp (map car (cadr datum)) (map parse-exp (map cadr (cadr datum))) (map parse-exp (cddr datum)))])]					
				[else				   ; named let
					(cond 
						[(not ((list-of 2-list?) (caddr datum))) (eopl:error 'parse-exp "Invalid let variables: ~s" datum)]
						[(not (andmap symbol? (map car (caddr datum)))) (eopl:error 'parse-exp "Invalid let symbols: ~s" datum)]
						[(not (> (length datum) 2)) (eopl:error 'parse-exp "Invalid let length: ~s" datum)]
						[else (named-let-exp (cadr datum) (map car (caddr datum)) (map parse-exp (map cadr (caddr datum))) (map parse-exp (cdddr datum)))])])]
						
		[(eqv? (car datum) 'let*)
			(cond
				[(not ((list-of 2-list?) (cadr datum))) (eopl:error 'parse-exp "Invalid let* variables: ~s" datum)]
				[(not (andmap symbol? (map car (cadr datum)))) (eopl:error 'parse-exp "Invalid let* symbols: ~s" datum)]
				[(not (> (length datum) 2)) (eopl:error 'parse-exp "Invalid let length: ~s" datum)]
				[else (let*-exp (map car (cadr datum)) (map parse-exp (map cadr (cadr datum))) (map parse-exp (cddr datum)))])]

		[(eqv? (car datum) 'letrec)
			(cond
				[(not ((list-of list?) (cadr datum))) (eopl:error 'parse-exp "Invalid letrec syntax: ~s" datum)]
				[(not (andmap symbol? (map car (cadr datum)))) (eopl:error 'parse-exp "Invalid letrec proc-names: ~s" datum)]
				[(not (> (length datum) 2)) (eopl:error 'parse-exp "Invalid letrec length: ~s" datum)]
				[else (letrec-exp (map car (cadr datum)) (map cadadr (cadr datum)) (map parse-exp (map car (map cddadr (cadr datum)))) (map parse-exp (cddr datum)))])]

		[(eqv? (car datum) 'cond)
			(cond-exp (map parse-exp (map car (cdr datum))) (map parse-exp (map cadr (cdr datum))))]  
			
		[(eqv? (car datum) 'if)
			(cond 
				[(eqv? (length datum) 4) (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (cadddr datum)))]
				[(eqv? (length datum) 3) (if-no-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
				[else (eopl:error 'parse-exp "Invalid if syntax: ~s" datum)])]
				
		[(eqv? (car datum) 'or)
			(or-exp (map parse-exp (cdr datum)))]
			
		[(eqv? (car datum) 'begin)
			(begin-exp (map parse-exp (cdr datum)))]
			
		[(eqv? (car datum) 'while)
			(while-exp (parse-exp (cadr datum)) (map parse-exp (cddr datum)))]

		[(eqv? (car datum) 'when)
			(when-exp (parse-exp (cadr datum)) (map parse-exp (cddr datum)))]
				
		[(eqv? (car datum) 'set!)
			(cond 
				[(not (eqv? (length datum) 3)) (eopl:error 'parse-exp "Invalid set! variable error: ~s" datum)]
				[(not (symbol? (cadr datum))) (eopl:error 'parse-exp "Invalid set! First is not a symbol: ~s" datum)]
				[else (set!-exp (cadr datum) (parse-exp (caddr datum)))])]
		
		[(eqv? (car datum) 'for)
			(cond 
				[(not (number? (caddr datum))) (for-exp-2 (cadr datum) (map parse-exp (cddr datum)))]
				[(for-exp (cadr datum) (caddr datum) (map parse-exp (cdddr datum)))])]	

		[(eqv? (car datum) 'define)
			(cond 
				[(define-exp (cadr datum) (parse-exp (caddr datum)))])]
			
		[else (app-exp (parse-exp (1st datum)) (map parse-exp (cdr datum)))])]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))			

(define unparse-exp
	(lambda (express)
		(cases expression express
			[lit-exp (id) id]
			[var-exp (id) id]
			[lambda-exp (id body) (cons* 'lambda id (map unparse-exp body))]
			[lambda-many-exp (id body) (cons* 'lambda id (map unparse-exp body))]
			[lambda-improp-exp (id body) (cons* 'lambda id (map unparse-exp body))]	
			[let-exp (symlist varlist body) (cons* 'let (map list symlist (map unparse-exp varlist)) (map unparse-exp body))]
			[if-exp (ifCase thenCase elseCase) (cons* 'if (unparse-exp ifCase) (unparse-exp thenCase) (unparse-exp elseCase))]
			[if-no-else-exp (ifCase thenCase) (cons* 'if (unparse-exp ifCase) (unparse-exp thenCase))]
			[let*-exp (symlist varlist body) (cons* 'let* (map list symlist (map unparse-exp varlist)) (map unparse-exp body))]
			[set!-exp (var body) (cons* 'set! var (map unparse-exp body))]
			[named-let-exp (name symlist varlist body) (cons* 'let name (map list symlist (map unparse-exp varlist)) (map unparse-exp body))]
			[letrec-exp (symlist varlist body) (cons* 'letrec (map list symlist (map unparse-exp varlist)) (map unparse-exp body))]
			[app-exp (rator rand) (cons (unparse-exp rator) (map unparse-exp rand))])))









