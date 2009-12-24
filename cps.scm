;;; CPS transformation for scheme-to-javascript

;;; All macro expansions must have occurred.  In addition, the upper-level
;;; syntax agent is responsible for having already completed the following
;;; work.
;;;   All literals are embedded in QUOTE forms.
;;;   All variable names are globally unique.
;;;   IF expressions are double-branched.
;;; The following special forms are allowed in input:
;;;   QUOTE SET! IF LAMBDA TOP-LEVEL-REF TOP-LEVEL-SET! BEGIN

;;; When we have completed, all continuations have been fully exposed,
;;; and the code observes the following constraints:
;;;   The implicit BEGIN has been removed from lambda expressions.
;;;   All lambda expressions accept an additional continuation first argument.
;;;   Every element of a combination is a variable, lambda expression, 
;;;     or literal.

(define unique-id 0)
(define (uniquify name)
  (set! unique-id (+ unique-id 1))
  (string->symbol (string-append (symbol->string name)
				 (number->string unique-id))))

;;; Transform FORM into CPS style; passing its result to the code K.
;;; This generates lots of extra functions and calls, which we will
;;; later reduce.
(define (cps-transform form k)
  (cond
   ;; variable references
   ((symbol? form) `(,k ,form))

   ((pair? form)
    (case (car form)
      ((quote) `(,k ,form))
      ((top-level-ref) `(,k ,form))

      ((set!)
       (let ((var (cadr form))
	     (val (caddr form))
	     (setvar (uniquify 's)))
	 (cps-transform val
			`(lambda (,setvar)
			   (,k (set! ,var ,setvar))))))
      ((top-level-set!) 
       (let ((var (cadr form))
	     (val (caddr form))
	     (setvar (uniquify 's)))
	 (cps-transform val
			`(lambda (,setvar)
			   (,k (top-level-set! ,var ,setvar))))))
      
      ((define)
       (error cps-transform "illegal use of define"))

      ((if) (let ((test (cadr form))
		  (true-branch (caddr form))
		  (false-branch (cadddr form))
		  (testvar (uniquify 'q)))
	      (cps-transform test
			     `(lambda (,testvar)
				(if ,testvar
				    ,(cps-transform true-branch k)
				    ,(cps-transform false-branch k))))))

      ((begin)
       (cps-transform-body (cdr form) k))

      ((lambda) (let ((formals (cadr form))
		      (forms (cddr form))
		      (kvar (uniquify 'l)))
		  `(,k (lambda ,(cons kvar formals)
			 ,(cps-transform-body forms kvar)))))

      (else
       (let ((temps (map (lambda ignored (uniquify 't)) form)))
	 (cps-transform-combination form temps
				    `(,(car temps) ,k ,@(cdr temps)))))))))

;;; Apply CPS transformation to the successive elements of
;;; FORMS returning a single CPS form which invokes K at the end.
(define (cps-transform-body forms k)
  (if (null? (cdr forms))
      (cps-transform (car forms) k)
      (let ((ignored (uniquify 'i)))
	(cps-transform (car forms)
		       `(lambda ,ignored 
			  ,(cps-transform-body (cdr forms) k))))))

;;; Apply CPS transformation to the elements of FORM, turning
;;; them into the specified series of temporaries in a curry-ish fashion.
;;; Then drop END into the middle once all the temps are expanded.
(define (cps-transform-combination combo temps end)
  (if (null? (cdr combo))
      (cps-transform (car combo)
		     `(lambda ,temps ,end))
      (cps-transform (car combo)
		     `(lambda (,(car temps))
			,(cps-transform-combination (cdr combo) (cdr temps)
						    end)))))
       
