; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
	(lambda ()
		(empty-env-record)))

(define extend-env
	(lambda (syms vals env)
		(extended-env-record syms (map cell vals) env)))
		
(define extend-env-recursively
	(lambda (proc-names idss bodies env)
		(recursively-extended-env-record proc-names (map cell idss) bodies env)))

(define list-find-position
	(lambda (sym los)
		(list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
	(lambda (pred ls)
		(cond ((null? ls) #f)
			((pred (car ls)) 0)
			(else (let ((list-index-r (list-index pred (cdr ls))))
				(if (number? list-index-r)
					(+ 1 list-index-r)
					#f))))))
										
(define apply-env
	(lambda (env var succeed fail)
		(deref (apply-env-ref env var succeed fail))))		

(define apply-env-ref
	(lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
		(cases environment env
			[empty-env-record () (fail)]
			[extended-env-record (syms vals env)
				(let ((pos (list-find-position sym syms)))
					(if (number? pos)
						(succeed (list-ref vals pos))
						(apply-env-ref env sym succeed fail)))]
			[recursively-extended-env-record (procnames idss bodies old-env)
				(let ([pos (list-find-position sym procnames)])
				(if (number? pos)
					(if ((list-of symbol?) (unbox (list-ref idss pos)))
						(box (closure (unbox (list-ref idss pos)) (list (list-ref bodies pos)) env))
						(box (closure-improp (unbox (list-ref idss pos)) (list (list-ref bodies pos)) env)))
					(apply-env-ref old-env sym succeed fail)))])))

