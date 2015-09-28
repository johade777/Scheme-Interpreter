; top-level-eval evaluates a form in the global environment

(define (last ls)
	(if (= (length ls) 1)
    	(car ls)
    	(last (cdr ls))))

(define top-level-eval
	(lambda (form)
    ; later we may add things that are not expressions.
		(eval-exp form (empty-env) (iden-k))))
		
(define apply-k 
  (lambda (k v)
    (cases continuation k
	   [if-k (then-case else-case env cont)
			(if v
				(eval-exp then-case env cont)
				(eval-exp else-case env cont))]
		[if-no-else-k (then-case newEnv cont)
			(if v
				(eval-exp then-case newEnv cont))]
		[eval-bodies-k (bodyCdr env cont)
			(eval-bodies bodyCdr env cont)]
		[mappedCdr-k (proc-cps carLs cont)
			(proc-cps carLs (mapped-result v cont))]
		[mapped-result (mappedCdr cont)
			(apply-k cont (cons v mappedCdr))]
		[eval-args (rands env cont)
			(eval-rands rands env (app-exp-k v cont))]
		[app-exp-k (rators cont)
			(apply-proc rators v cont)]
		[set-k (id env cont)
			(apply-k cont (set-ref! (apply-env-ref env id (lambda (x) x) (lambda () (apply-env-ref global-env id (lambda (x) x)
																	(lambda () (error 'apply-env "variable ~s is not bound" id))))) 
					v))]
		[define-k (name cont)
			(begin (set! global-env (extend-env (list name) (list v) global-env)) (apply-k cont (void)))]
		[iden-k ()
			v]
		)))
	
; eval-exp is the main component of the interpreter
(define eval-exp
	(lambda (exp newEnv k)
		(cases expression exp
			[lit-exp (datum) (apply-k k datum)]
			
			[var-exp (id)
				(apply-k k (apply-env 
					newEnv id (lambda (x) x)  ; look up its value. (lambda (x) x) ; procedure to call if id is in the environment 
					(lambda () (apply-env-ref global-env id (lambda (x) x)
						(lambda () (error 'apply-env "variable ~s is not bound" id))))))]
						
			[if-exp (if-case then-case else-case)
				(eval-exp if-case newEnv (if-k then-case else-case newEnv k))]
				
			[if-no-else-exp (if-case then-case)
				(eval-exp if-case newEnv (if-no-else-k then-case newEnv k))]
					
			[app-exp (rator rands)					
				(eval-exp rator newEnv (eval-args rands newEnv k))]
					
			[lambda-exp (id body)
				(apply-k k (closure id body newEnv))]
				
			[letrec-exp (proc-names idss bodies letrec-bodies)
				(eval-bodies letrec-bodies (extend-env-recursively proc-names idss bodies newEnv) k)]
						
			[lambda-many-exp (id body)
				(apply-k k (closure-many id body newEnv))]
				
			[begin-exp (body)
				(eval-bodies body newEnv k)]
				 
			[lambda-improp-exp (id body)
				(apply-k k (closure-improp id body newEnv))]
				
			[set!-exp (id exp)
				(eval-exp exp newEnv (set-k id newEnv k))]
				
			[define-exp (name body)
				(eval-exp body newEnv (define-k name k))]
								
			[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

(define eval-bodies
	(lambda (bodies env k)
			(if (null? (cdr bodies))
				(eval-exp (car bodies) env k)
				(eval-exp (car bodies) env (eval-bodies-k (cdr bodies) env k)))))
			
; evaluate the list of operands, putting results into a list
(define eval-rands
	(lambda (rands env k)	
		(map-cps (lambda (x k) 
			(eval-exp x env k)) rands k)))
				
(define map-cps
	(lambda (proc-cps ls continuation)
		(cond
			[(null? ls) (apply-k continuation '())]
			[else (map-cps proc-cps (cdr ls) (mappedCdr-k proc-cps (car ls) continuation))])))
																							
;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.
(define apply-proc
	(lambda (proc-value args k)
		(cases proc-val proc-value
			[prim-proc (op) (apply-prim-proc op args k)]
			[continuation-proc (k)
				(apply-k k (car args))]
			[closure (ids bodies env)
				(let ([new-env (extend-env ids args env)])
					(eval-bodies bodies new-env k))]
			[closure-many (ids bodies env)
				(let ([new-env (extend-env (list ids) (list args) env)])
					(eval-bodies bodies new-env k))]
			[closure-improp (ids bodies env)
				(let* ([ids (improp-ids ids)]
					[new-env (extend-env ids (improp-args args (length ids)) env)])
					(eval-bodies bodies new-env k))]
			[else (error 'apply-proc
				"Attempt to apply bad procedure: ~s" 
				proc-value)])))
				
(define improp-ids 
    (lambda (ls)
      (cond
		[(null? ls) ls]
		[(or (symbol? ls) (number? ls)) (list ls)]
        [else (cons (car ls) (improp-ids (cdr ls)))])))
		
(define improp-args
	(lambda (ls len)
		(if (<= len 1)
			(list ls)
			(cons (car ls) (improp-args (cdr ls) (- len 1))))))

(define *prim-proc-names* '(+ - * / add1 sub1 cons = zero? not < > <= >= list null? assq eq? equal? atom? length list->vector
							list? pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set!
							display newline car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr append vector product apply map quotient 
							call/cc exit-list list-tail eqv?))

(define init-env			; for now, our initial global environment only contains 
	(extend-env				; procedure names.  Recall that an environment associates
		*prim-proc-names*	;  a value (not an expression) with an identifier.
		(map prim-proc *prim-proc-names*)
		(empty-env)))
	 
(define global-env init-env)
		
; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
	(lambda (prim-proc args k)
		(case prim-proc
			[(+) (apply-k k (apply + args))]
			[(vector) (apply-k k (apply vector args))]
			[(product) (apply-k k (apply product args))]
			[(eqv?) (apply-k k (apply eqv? args))]
			[(-) (apply-k k (apply - args))]
			[(*) (apply-k k (apply * args))]
			[(/) (apply-k k (apply / args))]
			[(exit-list) args]
			[(append) (apply-k k (apply append args))]
			[(add1) (apply-k k (+ (1st args) 1))]
			[(sub1) (apply-k k (- (1st args) 1))]
			[(call/cc) (apply-proc (car args) (list (continuation-proc k)) k)]
			[(cons) (apply-k k (apply cons args))]
			[(=) (apply-k k (apply = args))]
			[(zero?) (apply-k k (apply zero? args))]
			[(list-tail) (apply-k k (apply list-tail args))]
			[(not) (apply-k k (apply not args))]
			[(<) (apply-k k (apply < args))]
			[(>) (apply-k k (apply > args))]
			[(<=) (apply-k k (apply <= args))]
			[(>=) (apply-k k (apply >= args))]
			[(car) (apply-k k (apply car args))]
			[(cdr) (apply-k k (apply cdr args))]
			[(list) (apply-k k args)]
			[(null?) (apply-k k (apply null? args))]
			[(assq) (apply-k k (apply assq args))] ; ( 'b '((a . 1) (b .2)))
			[(eq?) (apply-k k (apply eq? args))]
			[(equal?) (apply-k k (apply equal? args))]
			[(atom?) (apply-k k (apply atom? args))]
			[(quotient) (apply-k k (apply quotient args))]
			[(length) (apply-k k (apply length args))]
			[(list->vector) (apply-k k (apply list->vector args))]
			[(list?) (apply-k k (apply list? args))]
			[(pair?) (apply-k k (apply pair? args))]
			[(procedure?) (apply-k k (proc-val? (car args)))]
			[(vector->list) (apply-k k (apply vector->list args))]
			[(vector) (apply-k k (apply vector args))]
			[(make-vector) (apply-k k (apply make-vector args))]
			[(vector-ref) (apply-k k (apply vector-ref args))]
			[(vector?) (apply-k k (apply vector? args))]
			[(number?) (apply-k k (apply number? args))]
			[(symbol?) (apply-k k (apply symbol? args))]
			[(set-car!) (apply-k k (set-car! (car args) (cadr args)))]
			[(set-cdr!) (apply-k k (set-cdr! (car args) (cadr args)))]
			[(vector-set!) (apply-k k (apply vector-set! args))]
			[(display) (apply display args)]
			[(newline) (newline)]
			[(caar) (apply-k k (apply caar args))]
			[(cadr) (apply-k k (apply cadr args))]
			[(cdar) (apply-k k (apply cdar args))]
			[(cddr) (apply-k k (apply cddr args))]
			[(caaar) (apply-k k (apply caaar args))]
			[(caadr) (apply-k k (apply caadr args))]
			[(cadar) (apply-k k (apply cadar args))]
			[(caddr) (apply-k k (apply caddr args))]
			[(cdaar) (apply-k k (apply cdaar args))]
			[(cdadr) (apply-k k (apply cdadr args))]
			[(cddar) (apply-k k (apply cddar args))]
			[(cdddr) (apply-k k (apply cdddr args))]
;			[(map) (map (lambda (x) (apply-proc (car args) (list x) k)) (cadr args))]
			[(map) (map-cps (lambda (x cont) (apply-proc (car args) (list x) cont)) (cadr args) k)]
			[(apply) (apply-proc (car args) (cadr args) k)]
			[else (error 'apply-prim-proc 
					"Bad primitive procedure name: ~s" 
					prim-op)])))
					
		

(define rep      ; "read-eval-print" loop.
	(lambda ()
		(display "--> ")
    ;; notice that we don't save changes to the environment...
		(let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
			(eopl:pretty-print answer) (newline)
			(rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
	(lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))
	
(define reset-global-env
	(lambda ()
		(set! global-env init-env)))

(define syntax-expand
	(lambda (parsed-exp)
		(cases expression parsed-exp
			[lit-exp (datum) (lit-exp datum)]
			
			[var-exp (id) (var-exp id)]
			
			[if-exp (if-case then-case else-case)
				(if-exp (syntax-expand if-case) (syntax-expand then-case) (syntax-expand else-case))]
				
			[if-no-else-exp (if-case then-case)
				(if-no-else-exp (syntax-expand if-case) (syntax-expand then-case))]
				
			[cond-exp (lscases lsdo)
				(let loop ([cases lscases] [doCond lsdo])
					(cond 
						[(null? (cdr cases)) (if (eqv? (car cases) 'else) 
												 (if-no-else-exp (syntax-expand (car cases)) (syntax-expand (car doCond)))
												 (syntax-expand (car doCond)))]
						[else (if-exp (syntax-expand (car cases)) (syntax-expand (car doCond)) (loop (cdr cases) (cdr doCond)))]))]
						
			[or-exp (conds)
				(let loop ([cases (map syntax-expand conds)])
					(cond 
						[(null? cases) (lit-exp #f)]
						[else (if-exp (car cases) (car cases) (loop (cdr cases)))]))]
						
			[let*-exp (symlist varlist body)
					(if (null? symlist)
						(let-exp '() '() (syntax-expand body))
						(let-exp (list (car symlist)) (list (car varlist)) (syntax-expand (let*-exp (cdr symlist) (cdr varlist) body))))] 
						
;			[while-exp (expess doBody)
				
						
			[when-exp (check bodies)
				(when-exp check (map syntax-expand bodies))]
						
			[let-exp (symlist varlist body)
				(app-exp (lambda-exp symlist (map syntax-expand body)) (map syntax-expand varlist))]
				
			[letrec-exp (proc-names idss bodies letrec-body)
				(letrec-exp proc-names idss (map syntax-expand bodies) (map syntax-expand letrec-body))]	

			[named-let-exp (name symlist varlist body)
				(syntax-expand (letrec-exp (list name) (list symlist) body (list (app-exp (var-exp name) varlist))))]
			
			[lambda-exp (id body)
				(lambda-exp id (map syntax-expand body))]
			
			[lambda-many-exp (id body)
				(lambda-many-exp id (map syntax-expand body))]
				
			[lambda-improp-exp (id body)
				(lambda-improp-exp id (map syntax-expand body))]

			[define-exp (name body)
				(define-exp name (syntax-expand body))]
							
			[app-exp (rator rands) 
				(app-exp rator (map syntax-expand rands))]
			[else parsed-exp])))

