
;; Parsed expression datatypes

(define literal?
	(lambda (x)
		(or (number? x) (boolean? x) (string? x) (vector? x) (pair? x) (symbol? x) (null? x))))
							
(define-datatype expression expression?
	[lit-exp
		(id literal?)]
		
	[var-exp
		(id symbol?)]
		
	[lambda-exp
		(id (list-of symbol?)) 
		(body (list-of expression?))]
		
	[lambda-many-exp
		(id symbol?) 
		(body (list-of expression?))]
		
	[lambda-improp-exp
		(id pair?) 
		(body (list-of expression?))]
		
	[let-exp
		(symlist (list-of symbol?))
		(varlist (list-of expression?))
		(body (list-of expression?))]
		
	[or-exp
		(conds (list-of expression?))]
		
	[if-exp
		(if-case expression?)
		(then-case expression?)
		(else-case expression?)]
		
	[if-no-else-exp
		(if-case expression?)
		(then-case expression?)]
		
	[cond-exp
		(caselist (list-of expression?))
		(dolist (list-of expression?))]
		
	[let*-exp
		(symlist (list-of symbol?))
		(varlist (list-of expression?))
		(body (list-of expression?))]
		
	[named-let-exp
		(name symbol?)
		(symlist (list-of symbol?))
		(varlist (list-of expression?))
		(body (list-of expression?))]
		
	[letrec-exp
		(proc-names (list-of symbol?))
		(idss (list-of (lambda (x) (or ((list-of symbol?) x) (pair? x)))))
		(bodies (list-of expression?))
		(letrec-body (list-of expression?))]
		
	[begin-exp
		(body (list-of expression?))]
		
	[while-exp
		(check expression?)
		(doBody (list-of expression?))]
		
	[when-exp
		(check list?)
		(body (list-of expression?))]
		
	[for-exp-2
		(first number?)
		(body (list-of expression?))]
		
	[for-exp
		(first number?)
		(last number?)
		(body (list-of expression?))]
		
	[set!-exp
		(var symbol?)
		(body expression?)]
		
	[define-exp
		(name symbol?)
		(body expression?)]
		
	[app-exp
		(rator expression?)
		(rand (list-of expression?))])

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
	[empty-env-record]
	[extended-env-record
		(syms (list-of symbol?))
		(vals (list-of cell?))
		(env environment?)]
	[recursively-extended-env-record
		(proc-names (list-of symbol?))
		(idds (list-of cell?))
		;(idss (list-of (lambda (x) (or ((list-of symbol?) x) (pair? x)))))
		(bodies (list-of expression?))
		(env environment?)])
		
(define cell 
	(lambda (v) 
		(box v)))
	
(define cell? 
	(lambda (obj) 
		(box? obj)))
	
(define cell-ref 
	(lambda (cell) 
		(unbox cell)))
	
(define cell-set! 
	(lambda (cell v) 
		(set-box! cell v)))

(define deref cell-ref)
(define set-ref! cell-set!)

(define-datatype continuation continuation?
	[if-k 
		(then-case expression?)
		(else-case expression?)
		(newEnv environment?)
		(k continuation?)]
	[if-no-else-k 
		(then-case expression?)
		(newEnv environment?)
		(k continuation?)]
	[eval-bodies-k
		(bodyCdr scheme-value?)
		(env environment?)
		(k continuation?)]
	[mappedCdr-k
		(proc-cps scheme-value?)
		(carLs scheme-value?)
		(k continuation?)]
	[mapped-result
		(mappedcdr scheme-value?)
		(k continuation?)]
	[eval-args 
		(rands scheme-value?)
		(env environment?)
		(k continuation?)]
	[app-exp-k
		(rators scheme-value?)
		(k continuation?)]
	[set-k 
		(id scheme-value?)
		(env environment?)
		(k continuation?)]
	[define-k 
		(name scheme-value?)
		(k continuation?)]
	[iden-k]
)
		
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
 	[prim-proc
		(name symbol?)]
	[continuation-proc 
		(k continuation?)]
	[closure
		(ids (list-of symbol?))
		(bodies (list-of expression?))
		(env environment?)]
	[closure-many
		(ids symbol?)
		(bodies (list-of expression?))
		(env environment?)]
	[closure-improp
		(ids pair?)
		(bodies (list-of expression?))
		(env environment?)])