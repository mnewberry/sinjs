;;; Simplify post-CPS code

(use (srfi 1))

(define reds 0)

;;; if global-set!s is #f, then make no assumptions about global
;;; assignments.
(define (simplify form global-set!s local-set!s)
  (define (local-set? name)
    (memq name local-set!s))

  (define (global-set? name)
    (or (not global-set!s)
	(memq name global-set!s)))

  ;; We are considering beta-reducing EXPR; check if ACTUAL is an
  ;; acceptable actual parameter when expanded into FORMAL.
  (define (reducable? actual formal expr)
    (or (and (pair? actual)		;constants
	     (or (eq? (car actual) 'quote)
		 (eq? (car actual) 'lambda))
	     (not (local-set? formal)))
	(and (symbol? actual)	;actual is unassigned local var
	     (not (local-set? actual))
	     (not (local-set? formal)))
	(and (pair? actual)		;actual is unassigned global var
	     (eq? (car actual) 'top-level-ref)
	     (not (global-set? (cadr actual)))
	     (not (local-set? formal)))
	(and (symbol? actual)
	     (not (setwithin? actual expr)) ;formal is not captured
	     (not (captured? formal expr)))
	(and (pair? actual)
	     (eq? (car actual) 'top-level-ref)
	     (not (setwithin? (cadr actual) expr))
	     (not (captured? formal expr)))))

  (define (simplify-1 form)
    (cond
     ((symbol? form) form)
     ((pair? form)
      (case (car form)
	((quote) form)
	((top-level-ref) form)

	((foreign-inline)
	 (let ((code (cadr form))
	       (args (cddr form)))
	   `(foreign-inline ,code ,@(map simplify-1 args))))

	((set!) 
	 (let ((var (cadr form))
	       (val (caddr form)))
	   `(set! ,var ,(simplify-1 val))))

	((top-level-set!)
	 (let ((var (cadr form))
	       (val (caddr form)))
	   `(top-level-set! ,var ,(simplify-1 val))))

	((if)
	 (let ((test (cadr form))
	       (true-branch (caddr form))
	       (false-branch (cadddr form)))
	   `(if ,(simplify-1 test) 
		,(simplify-1 true-branch) 
		,(simplify-1 false-branch))))

	((lambda) 
	 (let ((args (cadr form))
	       (value (caddr form)))
	   `(lambda ,args ,(simplify-1 value))))

	(else
	 (let ((procedure (simplify-1 (car form)))
	       (args (map simplify-1 (cdr form))))
	   (cond
	    ;; if the operator position is a literal procedure, perhaps
	    ;; we can do a beta reduction.
	    ((and (list? procedure)
		  (eq? (car procedure) 'lambda)
		  (list? (cadr procedure)) ;no rest parameter
		  (= (length (cadr procedure)) (length args)) ;correct # of args
		  (every (cut reducable? <> <> (caddr procedure))
			 args (cadr procedure)))
	     #;(display (format "form ~s\n" form))
	     #;(display (format "reducing with ~s\n\n"
			      (map cons (cadr procedure) args)))
	     (set! reds (+ reds 1))
	     ;; try reduction; abandon if code size goes up
	     (let ((b (beta-reduce (caddr procedure) 
			  (map cons (cadr procedure) args))))
	       (if (> (complexity b) (complexity form))
		   form
		   b)))

	    (else (cons procedure args)))))))))

  (set! reds 0)
  (display (format "comp -> ~s\n" (complexity form)))
  (let ((f (simplify-1 form)))
    (display (format "reds: ~s\n\n" reds))
    (if (equal? f form)
	form
	(simplify f global-set!s local-set!s))))

(define (complexity form)
  (if (list? form)
      (apply + (length form) (map complexity form))
      1))

;;; We can beta reduce if every argument fits any of the following tests:
;;;   Actual parm is any constant (QUOTE or LAMBDA)
;;;   Actual parm is a variable which is never assigned in the program
;;;   Actual parm is a variable which is somewhere assigned, but
;;;       formal parm is not captured within the body of the procedure
;;;       being reduced, and the actual parm is not assigned within 
;;;	  the body of the procedure assigned.
;;; 
;;;   In addition, the formal parm must never be assigned.

;;; Define a "saved lambda" as a lambda which is being used other than
;;; in operator position.  A variable is used in a saved lambda if it
;;; occurs (at any depth or nesting or sub-lambda) within it.

;;; A variable is captured, then, if it is used in a saved lambda.
;;; Such variables cannot be beta reduced because (since they are set
;;; somewhere), the value may be different between the lookup at the
;;; call we are reducing, and the use later inside the saved lambda.
;;; If a variable is set within the procedure being reduced, but with
;;; no captures, we could in principle still beta-reduce it, but that would
;;; require a flow control analysis we don't care to deal with here, so
;;; we skip that possibility.

;;; In addition, we leave the call alone if the procedure uses a rest
;;; parameter, or there is an argument count mismatch.
;;;
;;; All these rules are implemented by the functions below, and above
;;; in the body of SIMPLIFY.

;; return true if SYMBOL is captured by a lambda in EXPR, which means
;; that we must preserve it (and not beta reduce) because it is the
;; value at EXPR evaluation time which must be preserved.
;;
;; also return true if SYMBOL is assigned anywhere in EXPR.
(define (captured? symbol expr)
  (cond
   ((symbol? expr) #f)
   ((pair? expr)
    (case (car expr)
      ((quote) #f)
      ((foreign-inline) (any (cut captured? symbol <>) (cddr expr)))
      ((set!) (or (eq? symbol (cadr expr))
		  (captured? symbol (caddr expr))))
      ((top-level-ref) #f)
      ((top-level-set!) (or (eq? symbol (cadr expr))
			    (captured? symbol (caddr expr))))
      ((if) (or (captured? symbol (cadr expr))
		(captured? symbol (caddr expr))
		(captured? symbol (cadddr expr))))
      ((lambda) (used? symbol (caddr expr))) ;this is a lambda to check...
      ;; note here that we do not need to check operator position
      (else (any (cut captured? symbol <>) (cdr expr)))))))

;;; return true if SYMBOL is used or assigned anywhere in EXPR
(define (used? symbol expr)
  (cond
   ((and (symbol? expr)
	 (eq? symbol expr)) #t)
   ((pair? expr)
    (case (car expr)
      ((quote) #f)
      ((foreign-inline) (any (cut used? symbol <>) (cddr expr)))
      ((set!) (or (eq? symbol (cadr expr))
		  (used? symbol (caddr expr))))
      ((top-level-ref) (eq? symbol (cadr expr)))
      ((top-level-set!) (or (eq? symbol (cadr expr))
			    (used? symbol (caddr expr))))
      ((if) (or (used? symbol (cadr expr))
		(used? symbol (caddr expr))
		(used? symbol (cadddr expr))))
      ((lambda) (used? symbol (caddr expr)))
      ;; here we *do* check operator position of course
      (else (any (cut used? symbol <>) expr))))))
  
;; is VAR set within EXPR?
(define (setwithin? var expr)
  (cond
   ((symbol? expr) #f)
   ((pair? expr)
    (case (car expr)
      ((quote) #f)
      ((top-level-ref) #f)
      ((foreign-inline) (any (cut setwithin? var <>) (cddr expr)))
      ((set!) (or (eq? var (cadr expr))
		  (setwithin? var (caddr expr))))
      ((top-level-set!) (or (eq? var (cadr expr))
			    (setwithin? var (caddr expr))))
      ((if (or (setwithin? var (cadr expr))
	       (setwithin? var (caddr expr))
	       (setwithin? var (cadddr expr)))))
      ((lambda) (setwithin? var (caddr expr)))
      (else (any (cut setwithin? var <>) expr))))))

;;; Actually reduce FORM with variable->value pairs in the alist MAPPINGS
(define (beta-reduce form mappings)
  (cond
   ((assq form mappings) => cdr)
   ((symbol? form) form)
   ((pair? form)
    (case (car form)
      ((quote) form)
      ((foreign-inline) form
       `(foreign-inline ,(cadr form)
			,@(map (cut beta-reduce <> mappings) (cddr form))))
      ((set!) (if (assq (cadr form) mappings)
		  (error "set! in beta reduction")
		  `(set! ,(cadr form) 
			 ,(beta-reduce (caddr form) mappings))))
      ((top-level-set!) `(top-level-set! 
			  ,(cadr form)
			  ,(beta-reduce (caddr form) mappings)))
      ((if) `(if ,(beta-reduce (cadr form) mappings)
		 ,(beta-reduce (caddr form) mappings)
		 ,(beta-reduce (cadddr form) mappings)))
      ((lambda)
       (let ((formals (cadr form))
	     (value (caddr form)))
	 `(lambda ,formals ,(beta-reduce value mappings))))

      (else
       (map (cut beta-reduce <> mappings) form))))))
