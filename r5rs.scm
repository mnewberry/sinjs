(library r5rs
  (export quote lambda if set!
	  cond case and or let let* letrec begin do delay quasiquote
	  let-syntax letrec-syntax
	  define define-syntax
	  eqv? eq? equal?
	  number? complex? real? rational? integer? exact? inexact?
	  zero? positive? negative? odd? even?
	  max min + * - / abs quotient remainder modulo gcd lcm
	  numerator denominator floor ceiling truncate round rationalize 
	  exp log sin cos tan asin acos atan sqrt expt
	  exact->inexact inexact->exact number->string string->number
	  not boolean?
	  pair cons car cdr set-car! set-cdr! caar cadr cdar cddr
	  caaar caadr cadar caddr cdaar cdadr cddar cdddr
	  caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
	  cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
	  null? list? list length append reverse list-tail list-ref
	  memq memv member assq assv assoc
	  symbol? symbol->string string->symbol
	  char? char=? char<? char>? char<=? char>=?
	  char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
	  char-alphabetic? char-numeric? char-whitespace? char-upper-case?
	  char-lower-case? char->integer integer->char char-upcase char-downcase
	  string? make-string string string-length string-ref string-set!
	  string=? string-ci=? string<? string>? string<=? string>=?
	  string-ci<? string-ci>? string-ci<=? string-ci>=?
	  substring string-append string->list list->string
	  string-copy string-fill!
	  vector? make-vector vector vector-length
	  vector-ref vector-set! vector->list list->vector vector-fill!
	  procedure? apply map for-each force 
	  call-with-current-continuation values call-with-values dynamic-wind
	  eval scheme-report-environment 
	  null-environment interaction-environment
	  call-with-input-file call-with-output-file 
	  input-port? output-port?
	  current-input-port current-output-port
	  with-input-from-file with-output-to-file
	  open-input-file open-output-file close-input-port close-output-port
	  read read-char peek-char eof-object? char-ready?
	  write display newline write-char)
  (import (primitive-syntax if lambda set! quote begin 
			    define define-syntax let-syntax letrec-syntax
			    define-integrable foreign-inline foreign-prologue
			    foreign-lambda foreign-lambda-cps))

;;; R5RS 4.2.1
(define-syntax cond
  (syntax-rules (=> else)
    ;; one clause forms
    ((_ (else expr exprs ...)) (begin 'not-defn expr exprs ...))
    ((_ (test)) test)
    ((_ (test => proc)) (let ((tmp test)) (if tmp (proc tmp))))
    ((_ (test exprs ...)) (if test (begin exprs ...)))

    ;; multiple clause forms [except else]
    ((_ (test) more ...) (let ((tmp test)) (if tmp tmp (cond more ...))))
    ((_ (test => proc) more ...)
     (let ((tmp test)) (if tmp (proc tmp) (cond more ...))))
    ((_ (test exprs ...) more ...)
     (let ((tmp test)) (if tmp (begin exprs ...) (cond more ...))))))

(define-syntax case
  (syntax-rules (else)
    ;; this matches any KEY which cannot bear multiple evaluation.
    ((_ (key ...) clauses ...)
     (let ((tmp (key ...))) (case tmp clauses ...)))

    ;; one clause forms
    ((_ key (else exprs ...))
     (begin 'not-defn exprs ...))
    ((_ key ((datums ...) exprs ...))
     (if (memv key '(datums ...)) (begin exprs ...)))

    ;; multiple clause form
    ((_ key ((datums ...) exprs ...) more ...)
     (if (memv key '(datums ...)) (begin exprs ...) (case key more ...)))))

(define-syntax and
  (syntax-rules ()
    ((_) #t)
    ((_ x) x)
    ((_ x y ...) (if x (and y ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((_) #f)
    ((_ x) x)
    ((_ x y ...) (let ((tmp x)) (if tmp tmp (or y ...))))))

;;; R5RS 4.2.2 and 4.2.4

(define-syntax let
  (syntax-rules ()
    ((_ ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))
    ((_ name ((var val) ...) body ...)
     ((letrec ((name (lambda 
			 (var ...)
		       body ...)))
	name)
      val ...))))

(define-syntax let*
  (syntax-rules ()
    ((_ () body ...) (let () body ...))
    ((_ ((var val) more ...) body ...)
     (let ((var val)) (let* (more ...) body ...)))))

;; As Al Petrofsky points out, this is fine since DEFINE
;; must be handled by the compiler anyhow.
(define-syntax letrec
  (syntax-rules ()
    ((_ ((var val) ...) body ...)
     (let ()
       (begin (define var val) ...)
       (let () body ...)))))

;;; 4.2.4 [named LET is above]
(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
	 (test expr ...)
       command ...)
     (let-syntax ((last (syntax-rules ()
			  ((_ x) x)
			  ((_ x y) y))))
       (let loop ((var init) ...)
	 (if test
	     (begin 'undefined-do-result expr ...)
	     (begin (begin command ...) (loop (last var step ...) ...))))))))

;;; 4.2.5
;;; The R5RS sample implementation requires an auxiliary procedure
;;; which we'd like to avoid; if you put the procedure into the
;;; syntax-rules then users of promise get forced inlining of a big
;;; procedure.  So instead, we capture the expression here in a cons
;;; (FORCED . proc/val), and all the rest of the work is in FORCE.

(define-syntax delay
  (syntax-rules ()
    ((_ expr) (cons #f (lambda () expr)))))

;;; 4.2.6
;;; xxx tracks depth with secret extra arguments that users could screw with

(define-syntax quasiquote
  (syntax-rules (unquote unquote-splicing quasiquote)
    ;; basic top-level unquoting rules
    ((_ (unquote item)) item)
    ((_ ((unquote-splicing item) items ...)
	(append item (quasiquote (items ...)))))

    ;; increment depth if we are nesting
    ((_ (quasiquote item) tail ...
     (list 'quasiquote (quasiquote item #f tail ...))))
    
    ;; undo depth if unquoting and we are nested
    ((_ (unquote item) tail more-tail ...)
     (list 'unquote (quasiquote item more-tail ...)))
    ((_ (unquote-splicing item) tail more-tail ...)
     (list 'unquote-splicing (quasiquote item more-tail ...)))

    ;; deconstruct and reconstruct list and vector structure
    ((_ (item . items) tail ...)
     (cons (quasiquote item tail ...) (quasiquote items tail ...)))
    ((_ #(items ...) tail ...)
     (list->vector (quasiquote (items ...) tail ...)))

    ;; anything else is just quoted, whatever the depth
    ((_ anything tail ...) (quote anything))))

;;;
;;; Procedure definitions
;;;

;;;
;;; Names: Anything with a %% is a version with
;;; unchecked arguments, and does not accept 
;;; variable numbers of arguments.

;;; PROCEDURES (equivalence)

(define-integrable (eq? x y)
  (foreign-inline "~a===~a" x y))

(define eqv? eq?)	       ;ok b/c we know chars and numbers are eq

(define (equal? a b)
  (or (eqv? a b)
      (and (pair? a)
	   (pair? b)
	   (equal? (%%car a) (%%car b))
	   (equal? (%%cdr a) (%%cdr b)))
      (and (vector? a)
	   (vector? b)
	   (= (%%vector-length a) (%%vector-length b))
	   (let next-elt ((i 0))
	     (or (%%= i (%%vector-length a))
		 (and (equal? (%%vector-ref a i) (%%vector-ref b i))
		      (next-elt (%%+ i 1))))))
      (and (string? a)
	   (string? b)
	   (%%string=? a b))))

;;;
;;; PROCEDURES (numbers)
;;;
;;; Javascript only provides floating point numbers.  We cheat as follows,
;;; skating the edge of R5RS.  Scheme numbers will be Javascript numbers,
;;; and we will take advantage of permission to inexactify numbers by
;;; inexactifying *all* numbers, at every operation.  That is, no exact
;;; numbers actually exist, and even inexact->exact will produce an 
;;; inexact result.  This seems to actually be ok, with the following 
;;; curious reasoning.  We are permitted to reduce the supported range of
;;; exact numbers as we wish.  What of 6.2.3's admonishment that we must
;;; support them for valid data structure indexes and lengths?  Ah, well,
;;; we'll just extend those procedures to accept inexact arguments, and the
;;; result is that no correctly written Scheme program should be able to
;;; tell quite what we're up to.  Except, that is, for the curious behavior
;;; of the exact? and inexact->exact procedures.

;;; The following are not provided, in accord with the permission
;;; of R5RS to omit them if we don't support general complex numbers:
;;;   make-rectangular make-polar real-part imag-part magnitude angle

(define-integrable (number? obj)
  (foreign-inline "typeof(~a)==='number'" obj))
(define complex? number?)
(define real? number?)
(define rational? number?)
(define (integer? obj)
  (and (number? obj)
       (%%integer? obj)))
(define-integrable (%%integer? obj)
  (%%=obj (%%floor obj)))

(define-integrable (exact? z) #f)
(define-integrable (inexact? z) #t)

(define-syntax num-compare
  (syntax-rules ()
    ((_ unchecked-scheme-name scheme-name js-expr)
     (begin
       (define-integrable (unchecked-scheme-name z1 z2)
	 (foreign-inline js-expr z1 z2))
       (define (scheme-name z1 z2 . zs)
	 (and (unchecked-scheme-name (%%check-number z1) (%%check-number z2))
	      (or (null? zs)
		  (%%apply scheme-name z2 zs))))))))
(num-compare %%= = "~a===~a")
(num-compare %%< < "~a<~a")
(num-compare %%> > "~a>~a")
(num-compare %%<= <= "~a<=~a")
(num-compare %%>= >= "~a>=~a")

(define (zero? z) (= z 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))
(define (odd? n) (%%= 1 (remainder n 2)))
(define (even? n) (%%= 0 (remainder n 2)))

(define-integrable (%%zero? z) (%%= z 0))
(define-integrable (%%positive? x) (%%> x 0))
(define-integrable (%%negative? x) (%%< x 0))
(define-integrable (%%even? x) (%%= 0 (%%remainder n 2)))

(define (max z1 . zs)
  (let loop ((l zs)
	     (top z1))
    (cond
     ((null? l) top)
     ((> (%%car l) top) (loop (%%cdr l) (%%car l)))
     (else (loop (%%cdr l) top)))))

(define (min z1 . zs)
  (let loop ((l zs)
	     (bottom z1))
    (cond
     ((null? l) bottom)
     ((< (%%car l) bottom) (loop (%%cdr l) (%%car l)))
     (else (loop (%%cdr l) bottom)))))

(define-integrable (%%min z1 z2)
  (if (%%< z1 z2) z1 z2))

(define-syntax comm-op
  (syntax-rules ()
    ((_ unchecked-scheme-name scheme-name base-val js-expr)
     (begin
       (define-integrable (unchecked-scheme-name z1 z2)
	 (foreign-inline js-expr z1 z2))
       (define (scheme-name . zs)
	 (cond
	  ((null? zs) base-val)
	  ((null? (%%cdr zs)) (%%check-number (%%car zs)))
	  (else  (%%apply scheme-name
			(unchecked-scheme-name z1 z2)
			(%%cddr zs)))))))))
(comm-op %%+ + 0 "~a+~a")
(comm-op %%* * 1 "~a*~a")

(define-syntax dim-op
  (syntax-rules ()
    ((_ unchecked-scheme-name scheme-name unary-js-expr binary-js-expr)
     (begin
       (define-integrable (unchecked-scheme-name z1 z2)
	 (foreign-inline binary-js-expr z1 z2))
       (define (scheme-name z1 . zs)
	 (if (null? zs)
	     (foreign-inline unary-js-expr (%%check-number z1))
	     (let ((first (unchecked-scheme-name (%%check-number z1)
						 (%%check-number (%%car zs)))))
	       (if (null? (%%cdr zs))
		   first
		   (%%apply scheme-name first (%%cdr zs))))))))))
(dim-op %%- - "-~a" "~a-~a")
(dim-op %%/ / "1/~a" "~a/~a")

(define (abs z) (%%abs (%%check-number z)))
(define-integrable (%%abs z) (foreign-inline "Math.abs(~a)" z))

;;; JS has no builtin integer division!
(define (quotient n1 n2)
  (%%check-number n1)
  (%%check-number n2)
  (%%/ (%%- n1 (%%remainder n1 n2)) n2))

(define-integrable (%%remainder n1 n2)
  (foreign-inline "~a%~a" n1 n2))
(define (remainder n1 n2)
  (%%remainder (%%check-integer n1) (%%check-integer n2)))

(define (modulo n1 n2)
  (let ((r (remainder n1 n2)))
    (cond
     ((and (%%negative? n2) (%%positive? r))
      (%%- r n2))
     ((and (%%positive? n2) (%%negative? r))
      (%%+ r n2))
     (else r))))

(define (%%gcd a b)
  (if (%%zero? b)
      (%%abs a)
      (%%gcd b (%%remainder a b))))

(define (gcd . ns)
  (let next ((ns ns) (g 0))
    (if (null? ns)
	g
	(next (%%cdr ns) (%%gcd g (%%check-number (%%car ns)))))))

(define (lcm . ns)
  (define (lcm* a b)
    (%%* (%%/ (%%abs a) (%%gcd a b)) (%%abs b)))
  (let next ((ns ns) (m 1))
    (if (null? ns)
	m
	(next (%%cdr ns) (lcm* m (%%check-number (%%car ns)))))))

(define (denominator q)
  ;; assume we're dealing with binary floating point
  ;; and search for the right denominator.
  (if (%%integer? (%%check-number q))
      1
      (let next-ub-guess ((ub 1))
	(if (%%integer? (%%* q ub))
	    (let next-search ((lb 1)
			      (ub ub))
	      ;; binary search for the minimal denominator
	      ;; invariant: LB is too small; UB is (maybe) too big
	      (if (%%= ub (%%+ lb 1))
		  ub
		  (let ((midpoint (if (%%even? (%%- ub lb))
				      (%%+ lb (%%/ (%%- ub lb) 2))
				      (%%+ lb (%%/ (%%- ub lb) 2) -0.5))))
		    (if (%%integer? (%%* q midpoint))
			(next-search lb midpoint)
			(next-search midpoint ub)))))
	    (next-ub-guess (%%* ub 2))))))

(define (numerator q)
  (%%* (denominator q) q))

(define-syntax unary-math-op
  (syntax-rules ()
    ((_ checked-scheme-name js-expr)
     (define-integrable (checked-scheme-name x)
       (foreign-inline js-expr (%%check-number x))))
    ((_ unchecked-scheme-name checked-scheme-name js-expr)
     (begin
       (define-integrable (unchecked-scheme-name x)
	 (foreign-inline js-expr x))
       (define (checked-scheme-name x)
	 (unchecked-scheme-name (%%check-number x)))))))

(unary-math-op %%floor floor "Math.floor(~a)")    
(unary-math-op %%ceiling ceiling "Math.ceil(~a)")

(define (truncate x)
  (%%check-number x)
  (cond
   ((%%positive? x) (%%floor x))
   ((%%negative? x) (%%ceiling x))
   (else x)))

;;; note that it is not clear if JS's Math.round is guaranteed
;;; to round even on midpoints.
(define (round x)
  (let ((diff (%%- x (floor x))))
    (cond
     ((%%< diff 0.5) (%%floor x))
     ((%%> diff 0.5) (%%ceiling x))
     ((%%even? (%%floor x)) (%%floor x))
     (else (%%ceiling x)))))

;;; Thanks to Alan Bawden.  This is snarfed from the MIT/GNU Scheme source.
(define (rationalize x e)
  (define (loop x y)
    (let ((fx (%%floor x))
	  (fy (%%floor y)))
      (cond ((not (%%< fx x)) fx)
	    ((%%= fx fy)
	     (%%+ fx
		(%%/ 1
		   (loop (%%/ 1 (%%- y fy))
			 (%%/ 1 (%%- x fx))))))
	    (else (%%+ fx 1)))))

  (define (find-it x y)
    (cond ((%%positive? x) (loop x y))
	  ((%%negative? y) (%%- 0 (loop (%%- 0 y) (%%- 0 x))))
	  (else 0)))
  (%%check-number x)
  (%%check-number e)
  (cond
   ((%%positive? e) (find-it (%%- x e) (%%+ x e)))
   ((%%negative? e) (find-it (%%+ x e) (%%- x e)))
   (else x)))

(unary-math-op exp "Math.exp(~a)")
(unary-math-op log "Math.log(~a)")
(unary-math-op sin "Math.sin(~a)")
(unary-math-op cos "Math.cos(~a)")
(unary-math-op tan "Math.tan(~a)")
(unary-math-op asin "Math.asin(~a)")
(unary-math-op acos "Math.acos(~a)")

(define (atan x . y*)
  (if (null? y*)
      (foreign-inline "Math.tan(~a)" (%%check-number x))
      (foreign-inline "Math.tan2(~a,~a)" 
		      (%%check-number x) (%%check-number (%%car y*)))))

(unary-math-op sqrt "Math.sqrt(~a)")

(define (expt z1 z2)
  (foreign-inline "Math.pow(~a,~a)" (%%check-number z1) (%%check-number z2)))

(define-integrable (exact->inexact z) z)
(define-integrable (inexact->exact z) z)

(unary-math-op %%number->string number->string "new SchemeString(String(~a))")

(define (%%string->number s)
  (let ((val (foreign-inline "Number(~a.val)" s)))
    (and (not (foreign-inline "isNaN(~a)" val))
	 val)))

(define (string->number s)
  (%%string->number (%%check-string s)))

;;;
;;; PROCEDURES (other data types)  
;;;

(define-integrable (boolean? obj)
  (or (eq? obj #t)
      (eq? obj #f)))

(define-integrable (not obj)
  (if obj #f #t))

(define-integrable (pair? obj)
  (foreign-inline "~a.constructor===Pair" obj))

(define-integrable (cons a d)
  (foreign-inline "new Pair(~a,~a)" a d))

(define-integrable (%%car p) (foreign-inline "~a.car" p))
(define-integrable (%%cdr p) (foreign-inline "~a.cdr" p))
(define (car p) (%%car (%%check-pair p)))
(define (cdr p) (%%cdr (%%check-pair p)))

(define (set-car! p obj)
  (foreign-inline "~a.car=~a" (%%check-pair p) obj)
  'set-car!-undefined-value)
(define (set-cdr! p obj)
  (foreign-inline "~a.cdr=~a" (%%check-pair p) obj)
  'set-cdr!-undefined-value)

(define (caar obj) (car (car obj)))
(define (cadr obj) (car (cdr obj)))
(define (cdar obj) (cdr (car obj)))
(define (cddr obj) (cdr (cdr obj)))

(define-integrable (%%caar obj) (%%car (%%car obj)))
(define-integrable (%%cdar obj) (%%cdr (%%car obj)))
(define-integrable (%%cddr obj) (%%cdr (%%cdr obj)))

(define (caaar obj) (car (caar obj)))
(define (caadr obj) (car (cadr obj)))
(define (cadar obj) (car (cdar obj)))
(define (caddr obj) (car (cddr obj)))
(define (cdaar obj) (cdr (caar obj)))
(define (cdadr obj) (cdr (cadr obj)))
(define (cddar obj) (cdr (cdar obj)))
(define (cdddr obj) (cdr (cddr obj)))

(define (caaaar obj) (caar (caar obj)))
(define (caaadr obj) (caar (cadr obj)))
(define (caadar obj) (caar (cdar obj)))
(define (caaddr obj) (caar (cddr obj)))
(define (cadaar obj) (cadr (caar obj)))
(define (cadadr obj) (cadr (cadr obj)))
(define (caddar obj) (cadr (cdar obj)))
(define (cadddr obj) (cadr (cddr obj)))
(define (cdaaar obj) (cdar (caar obj)))
(define (cdaadr obj) (cdar (cadr obj)))
(define (cdadar obj) (cdar (cdar obj)))
(define (cdaddr obj) (cdar (cddr obj)))
(define (cddaar obj) (cddr (caar obj)))
(define (cddadr obj) (cddr (cadr obj)))
(define (cdddar obj) (cddr (cdar obj)))
(define (cddddr obj) (cddr (cddr obj)))

(define-integrable (null? obj)
  (eq? obj '()))

(define (list? obj)
  (or (null? obj)
      (and (pair? obj)
	   (list? (%%cdr obj)))))

(define (list . objs) objs)

(define (length list)
  (if (null? list)
      0
      (%%+ 1 (length (cdr list)))))
(define (%%length list)
  (if (null? list)
      0
      (%%+ 1 (%%length (%%cdr list)))))


(define (append . lists)
  (let append* ((lists lists))
    (cond
     ((null? lists) '())
     ((null? (%%car lists)) (append* (%%cdr lists)))
     (else (cons (%%caar lists) (append* (cons (%%cdar lists) 
					       (%%cdr lists))))))))

(define (reverse list)
  (let next ((list list) 
	     (so-far '()))
    (if (null? list)
	so-far
	(next (cdr list) (cons (car list) so-far)))))
(define (%%reverse list)
  (let next ((list list) 
	     (so-far '()))
    (if (null? list)
	so-far
	(next (%%cdr list) (cons (%%car list) so-far)))))

(define list-tail
  (let ()
    (define (list-tail list k)
      (if (zero? k)
	  list
	  (list-tail (cdr list) (%%- k 1))))
    list-tail))

(define (list-ref list k)
  (if (zero? k)
      (car list)
      (list-ref (cdr list) (%%- k 1))))

(define (memq obj list)
  (and (pair? list)
       (if (eq? obj (%%car list))
	   list
	   (memq obj (%%cdr list)))))

(define (memv obj list)
  (and (pair? list)
       (if (eqv? obj (%%car list))
	   list
	   (memv obj (%%cdr list)))))

(define (member obj list)
  (and (pair? list)
       (if (equal? obj (%%car list))
	   list
	   (member obj (%%cdr list)))))

(define (assq obj alist)
  (and (pair? alist)
       (if (eq? obj (car (%%car alist)))
	   (%%car alist)
	   (assq obj (%%cdr alist)))))

(define (assv obj alist)
  (and (pair? alist)
       (if (eqv? obj (car (%%car alist)))
	   (%%car alist)
	   (assv obj (%%cdr alist)))))

(define (assoc obj alist)
  (and (pair? alist)
       (if (equal? obj (car (%%car alist)))
	   (%%car alist)
	   (assoc obj (%%cdr alist)))))

(define-integrable (symbol? obj)
  (foreign-inline "typeof(~a)==='string'" obj))

(define-integrable (%%symbol->string sym)
  (foreign-inline "new SchemeString(~a)" sym))
(define (symbol->string sym)
  (%%symbol->string (%%check-symbol sym)))

(define-integrable (%%string->symbol sym)
  (foreign-inline "~a.val" sym))
(define (string->symbol sym)
  (%%string->symbol (%%check-string sym)))

(define-integrable (char? obj)
  (foreign-inline "~a.constructor===SchemeChar" obj))

(define-syntax char-comp
  (syntax-rules ()
    ((_ unchecked-scheme-name scheme-name js-expr)
     (begin
       (define-integrable (unchecked-scheme-name c1 c2)
	 (foreign-inline js-expr c1 c2))
       (define (scheme-name c1 c2)
	 (unchecked-scheme-name (%%check-char c1) (%%check-char c2)))))))

(char-comp %%char=? char=? "~a.val===~a.val")
(char-comp %%char<? char<? "~a.val<~a.val")
(char-comp %%char>? char>? "~a.val>~a.val")
(char-comp %%char<=? char<=? "~a.val<=~a.val")
(char-comp %%char>=? char>=? "~a.val>=~a.val")

(define-syntax ci-char-comp
  (syntax-rules ()
    ((_ ci-scheme-name simple-scheme-name)
     (define (ci-scheme-name c1 c2)
       (simple-scheme-name (char-upcase c1) (char-upcase c2))))))
(define-syntax %%ci-char-comp
  (syntax-rules ()
    ((_ ci-scheme-name simple-scheme-name)
     (define (ci-scheme-name c1 c2)
       (simple-scheme-name (%%char-upcase c1) (%%char-upcase c2))))))

(ci-char-comp char-ci=? %%char=?)
(ci-char-comp char-ci<? %%char<?)
(ci-char-comp char-ci>? %%char>?)
(ci-char-comp char-ci<=? %%char<=?)
(ci-char-comp char-ci>=? %%char>=?)
(%%ci-char-comp %%char-ci=? %%char=?)
(%%ci-char-comp %%char-ci<? %%char<?)
(%%ci-char-comp %%char-ci>? %%char>?)
(%%ci-char-comp %%char-ci<=? %%char<=?)
(%%ci-char-comp %%char-ci>=? %%char>=?)

(define (%%char-alphabetic? c)
  (or (%%char-upper-case? c)
      (%%char-lower-case? c)))
(define (char-alphabetic? c)
  (%%char-alphabetic? (%%check-char c)))

(define (%%char-numeric? c)
  (let ((n (%%char->integer c)))
    (and (%%>= n 48) (%%<= n 57))))
(define (char-numeric? c)
  (%%char-numeric? (%%check-char c)))

(define (char-whitespace? c)
  (let ((n (char->integer c)))
    (or (%%= n 32)			;space
	(%%= n 9)			;tab
	(%%= n 10)			;linefeed
	(%%= n 12)			;formfeed
	(%%= n 13))))			;carriage return

(define-integrable (%%char-upper-case? c)
  (let ((n (%%char->integer c)))
    (and (%%>= n 65) (%%<= n 90))))
(define (char-upper-case? c)
  (%%char-upper-case? (%%check-char c)))

(define-integrable (%%char-lower-case? c)
  (let ((n (%%char->integer c)))
    (and (%%>= n 97) (%%<= n 122))))
(define (char-lower-case? c)
  (%%char-lower-case? (%%check-char c)))
  
(define-integrable (%%char->integer c)
  (foreign-inline "~a.val.charCodeAt(0)" c))
(define (char->integer c)
  (%%char->integer (%%check-char c)))

(define-integrable (%%integer->char n)
  (foreign-inline "intern_char(String.fromCharCode(~a))" n))
(define (integer->char n)
  (%%integer->char (%%check-integer n)))

(define-integrable (%%char-upcase c)
  (if (%%char-lower-case? c)
      (%%integer->char (%%- (%%char->integer c) 32))
      c))
(define (char-upcase c)
  (%%char-upcase (%%check-char c)))

(define-integrable (%%char-downcase c)
  (if (%%char-upper-case? c)
      (%%integer->char (%%+ (%%char->integer c) 32))
      c))
(define (char-downcase c)
  (%%char-downcase (%%check-char c)))

(define-integrable (string? obj)
  (foreign-inline "~a.constructor===SchemeString" obj))

;;; MAKE-STRING in runtime
;;; STRING in runtime

(define-integrable (%%string-length s) (foreign-inline "~a.val.length" s))
(define (string-length s) (%%string-length (%%check-string s)))

(define-integrable (%%string-ref s k)
  (foreign-inline "intern_char(~a.val.charAt(~a))" s k))
(define (string-ref s k)
  (%%string-ref s (%%check-string-len s k)))

(define (string-set! s k c)
  (foreign-inline "~a.val=~a.val.slice(0,~a)+~a.val+~a.val.slice(~a+1)"
		  s s (%%check-string-len s k) (%%check-char c) s k)
  'string-set!-undefined-value)

(define-syntax string-comp
  (syntax-rules ()
    ((_ scheme-name js-expr)
     (define (scheme-name s1 s2)
       (foreign-inline js-expr (%%check-string s1) (%%check-string s2))))
    ((_ unchecked-scheme-name scheme-name js-expr)
     (begin
       (define-integrable (unchecked-scheme-name s1 s2)
	 (foreign-inline js-expr s1 s2))
       (define (scheme-name s1 s2)
	 (unchecked-scheme-name (%%check-string s1) (%%check-string s2)))))))

(string-comp %%string=? string=? "~a.val===~a.val")

(define (%%string-ci=? s1 s2)
  (let ((len (%%string-length s1)))
    (and (%%= len (%%string-length s2))
	 (let next ((i 0))
	   (or (%%= i len)
	       (and (%%char-ci=? (%%string-ref s1 i)
				 (%%string-ref s2 i))
		    (next (%%+ i 1))))))))
(define (string-ci=? s1 s2)
  (%%string-ci=? (%%check-string s1) (%%check-string s2)))

(string-comp string<? "~a.val<~a.val")
(string-comp string>? "~a.val>~a.val")
(string-comp string<=? "~a.val<=~a.val")
(string-comp string>=? "~a.val>=~a.val")

(define-syntax string-compare-ci
  (syntax-rules ()
    ((_ name lentest? chartest?)
     (define (name s1 s2)
       (let ((len1 (string-length s1))
	     (len2 (string-length s2)))
	 (let ((minlen (%%min len1 len2)))
	   (let next ((i 0))
	     (if (%%= i minlen)
		 (%%lentest? len1 len2)
		 (or (%%chartest? (%%string-ref s1 i)
				  (%%string-ref s2 i))
		     (and (%%char-ci=? (%%string-ref s1 i)
				       (%%string-ref s2 i))
			  (next (%%+ i 1))))))))))))

(string-compare-ci string-ci<? < char-ci<?)
(string-compare-ci string-ci>? > char-ci>?)
(string-compare-ci string-ci<=? <= char-ci<=?)
(string-compare-ci string-ci>=? >= char-ci>=?)

(define (substring s start end)
  (%%check-substring s start end)
  (foreign-inline "new SchemeString(~a.val.substring(~a,~a))" s start end))

;;; STRING-APPEND in runtime

(define (string->list s)
  (let ((len (string-length s)))
    (let next-char ((i len)
		    (lis '()))
      (if (%%= i 0)
	  lis
	  (next-char (%%- i 1) (cons (%%string-ref s (%%- i 1)) lis))))))

;;; LIST->STRING in runtime

(define (string-copy s)
  (foreign-inline "new SchemeString(~a.val)" (%%check-string s)))

;;; STRING-FILL! in runtime

(define-integrable (vector? obj)
  (foreign-inline "~a.constructor===Array" obj))

;;; MAKE-VECTOR in runtime
;;; VECTOR in runtime

(define-integrable (%%vector-length v) (foreign-inline "~a.length" v))
(define (vector-length v) (%%vector-length (%%check-vector v)))

(define-integrable (%%vector-ref v k) (foreign-inline "~a[~a]" v k))
(define (vector-ref v k) (%%vector-ref v (%%check-vector-len v k)))

(define-integrable (%%vector-set! v k obj) (foreign-inline "~a[~a]=~a" v k obj))
(define (vector-set! v k obj) (%%vector-set k (%%check-vector-len v k) obj))

(define (%%vector->list v)
  (let ((len (%%vector-length v)))
    (let next-elt ((i len)
		   (lis '()))
      (if (%%= i 0)
	  lis
	  (next-elt (%%- i 1) (cons (%%vector-ref v (%%- i 1)) lis))))))
(define (vector->list v)
  (%%vector->list (%%check-vector v)))

;; LIST->VECTOR in runtime

(define (vector-fill! v elt)
  (let ((len (vector-length v)))
    (do ((i 0 (%%+ i 1)))
	((%%= i len))
      (%%vector-set! v i c))))

;;;
;;; PROCEDURES (control features)

(define-integrable (procedure? obj)
  (foreign-inline "typeof(~a)==='function'" obj))

;;; APPLY in runtime

(define (%%map proc list)
  (let map1 ((inlist list)
	     (outlist '()))
    (if (null? inlist)
	(%%reverse outlist)
	(map1 (%%cdr inlist) (cons (proc (%%car inlist)) outlist)))))

(define (map proc list . lists)
  (if (null? lists)
      (let map1 ((inlist list)
		 (outlist '()))
	(if (null? inlist)
	    (%%reverse outlist)
	    (map1 (cdr inlist) (cons (proc (car inlist)) outlist))))
      (let map1 ((inlists (cons list lists))
		 (outlist '()))
	(if (null? (%%car inlists))
	    (%%reverse outlist)
	    (map1 (%%map cdr inlists)
		  (cons (%%apply proc (%%map car inlists)) outlist))))))

(define (for-each proc . lists)
  (if (not (null? (%%car lists)))
      (begin
	(%%apply proc (%%map car lists))
	(for-each proc (%%map cdr lists)))))

(define (force promise)
  (if (car promise)
      (%%cdr promise)
      (let ((val ((%%cdr promise))))
	(if (%%car promise)
	    (%%cdr promise)
	    (begin (set-car! promise #t)
		   (set-cdr! promise val)
		   val)))))

;;; CALL-WITH-CURRENT-CONTINUATION in runtime

(define (values . vals)
  (if (%%= (%%length vals) 1)
      (%%car vals)
      (foreign-inline "new MultipleValues(~a)" vals)))

(define (call-with-values producer consumer)
  (let ((vals (producer)))
    (if (foreign-inline "~a.constructor===MultipleValues" vals)
	(apply consumer (foreign-inline "~a.val" vals))
	(consumer vals))))

;;;
;;; EVAL
;;;
;;; XXX not implemented yet:
;;;   eval scheme-report-environment null-environment interaction-environment


(define (call-with-input-file str proc)
  (let ((p (open-input-file str)))
    (proc p)
    (%%close-input-port p)))

(define (call-with-output-file str proc)
  (let ((p (open-output-file str)))
    (proc p)
    (%%close-output-port p)))

(define-integrable (input-port? p)
  (foreign-inline "~a.constructor===SchemeInputPort" p))

(define-integrable (output-port? p)
  (foreign-inline "~a.constructor===SchemeOutputPort" p))

;;; the runtime must provide JS vars named
;;; sinjs_current_input_port and sinks_current_output_port
(define-integrable (current-input-port)
  (foreign-inline "sinjs_current_input_port"))
(define-integrable (current-output-port)
  (foreign-inline "sinjs_current_output_port"))
(define-integrable (%%set-current-input-port! p)
  (foreign-inline "sinjs_current_input_port=~a" p))
(define-integrable (%%set-current-output-port! p)
  (foreign-inline "sinjs_current_output_port=~a" p))
(define (set-current-input-port! p)
  (%%set-current-input-port (%%check-input-port p)))
(define (set-current-output-port! p)
  (%%set-current-output-port (%%check-output-port p)))

(define (%%close-input-port p)
  (let ((closer (foreign-inline "~a.closefn" p)))
    (if closer
	(begin (foreign-inline "~a.closefn=~a" p #f)
	       (foreign-inline "~a.readfn=~a" p #f)
	       (foreign-inline "~a.peekfn=~a" p #f)
	       (foreign-inline "~a.readyfn=~a" p #f)
	       (foreign-inline "~a(~a)" closer p)))))

(define (%%close-output-port p)
  (let ((closer (foreign-inline "~a.closefn" p)))
    (if closer
	(begin (foreign-inline "~a.closefn=~a" p #f)
	       (foreign-inline "~a.writefn=~a" p #f)
	       (foreign-inline "~a(~a)" closer p)))))

(define (close-input-port p)
  (%%close-input-port (%%check-input-port p)))
(define (close-output-port p)
  (%%close-output-port (%%check-output-port p)))

(define (with-input-from-file str proc)
  (call-with-input-file str (lambda (p)
			      (let ((old (current-input-port)))
				(%%set-current-input-port! p)
				(proc)
				(%%set-current-input-port! old)))))

(define (with-output-to-file str proc)
  (call-with-output-file str (lambda (p)
			       (let ((old (current-output-port)))
				 (%%set-current-output-port! p)
				 (proc)
				 (%%set-current-output-port! old)))))

(define-integrable (%%read-char p)
  (foreign-inline "~a.readfn(~a)" p p))
(define (read-char . p*)
  (if (null? p*)
      (%%read-char (current-input-port))
      (%%read-char (%%check-input-port (%%car p*)))))

(define-integrable (%%peek-char p)
  (foreign-inline "~a.peekfn(~a)" p p))
(define (peek-char . p*)
  (if (null? p*)
      (%%peek-char (current-input-port))
      (%%peek-char (%%check-input-port (%%car p*)))))

(define-integrable (eof-object? obj)
  (foreign-inline "~a===theEOF" obj))

(define-integrable (%%char-ready? p)
  (foreign-inline "~a.readyfn(~a)" p p))
(define (char-ready? . p*)
  (if (null? p*)
      (%%char-ready? (current-input-port))
      (%%char-ready? (%%check-input-port (%%car p*)))))

(define-integrable (%%write-char c p)
  (foreign-inline "~a.writefn(~a,~a)" p p c))
(define (write-char c . p*)
  (if (null? p*)
      (%%write-char (%%check-char c) (current-output-port))
      (%%write-char (%%check-char c) (%%check-output-port (%%car p*)))))

(define (read . port*)
  (%%read (if (null? port*) (current-input-port)
	      (%%check-input-port (%%car port*)))))

(define (%%read port)
  (define gathered #f)
  (define delimiters '(#\( #\) #\" #\' #\` #\,
		       #\; #\space #\tab #\newline #\page #\return))
  (define (read*)
    (if gathered
	(let ((c (%%peek-char port)))
	  (if (or (eof-object? c) 
		  (memv c delimiters))
	      (let ((tok (parse-long-token gathered)))
		(set! gathered #f)
		tok)
	      (begin
		(set! gathered 
		      (%%string-append gathered 
				       (%%string (%%char-downcase 
						  (%%read-char port)))))
		(read*))))
	(let ((c (%%read-char port)))
	  (if (eof-object? c)
	      c
	      (case c
		((#\() (read-list #t))
		((#\)) (error read "unexpected list terminator"))
		((#\") (read-string))
		((#\') (quote-obj 'quote))
		((#\`) (quote-obj 'quasiquote))
		((#\,) (if (eqv? (%%peek-char port) #\@)
			   (begin (%%read-char port)
				  (quote-obj 'unquote-splicing))
			   (quote-obj 'unquote)))
		((#\#)
		 (let ((tag (%%read-char port)))
		   (if (eof-object? tag)
		       (error read "eof after #")
		       (case (%%char-downcase tag)
			 ((#\t) #t)
			 ((#\f) #f)
			 ((#\() (%%list->vector (read-list #f)))
			 ((#\\) 
			  (let ((c (%%read-char port)))
			    (cond
			     ((eof-object? c)
			      (error read "eof in character literal"))
			     ((%%char-alphabetic? c)
			      (begin 
				(set! gathered  
				      (%%string-append "#\\" (%%string c)))
				(read*)))
			     (else
			      (parse-long-token
			       (%%string-append "#\\" (%%string c)))))))
			 ((#\i) (set! gathered "#i") (read*))
			 ((#\e) (set! gathered "#e") (read*))
			 ((#\b) (set! gathered "#b") (read*))
			 ((#\o) (set! gathered "#o") (read*))
			 ((#\d) (set! gathered "#d") (read*))
			 ((#\x) (set! gathered "#x") (read*))
			 (else (error read "impromper # syntax"))))))
		((#\space #\newline #\page #\tab #\return) (read*))
		((#\;) (skip-comment) (read*))
		(else
		 (if (or (%%char-alphabetic? c)
			 (%%char-numeric? c)
			 (memv c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\=
				   #\> #\? #\^ #\_ #\~ #\+ #\- #\. #\@)))
		     (begin (set! gathered (%%string (%%char-downcase c)))
			    (read*))
		     (error read 
			    (%%string-append "unexpected character '" 
					     (%%string c)
					     "'")))))))))

  (define (quote-obj mark)
    (let ((obj (read*)))
      (if (eof-object? obj)
	  (error read (%%string-append "end of file in " 
				       (%%symbol->string name)))
	  (list mark obj))))
	
  ;; peek char, but skip white space on the way.
  ;; called only from list syntax, so throw appropriate error if eof.
  (define (skip-white-peek)
    (let ((c (%%peek-char port)))
      (cond
       ((eof-object? c)
	(error read "eof in list"))
       ((memv c '(#\space #\tab #\newline #\page #\return))
	(%%read-char port) 
	(skip-white-peek))
       ((%%char=? c #\;)
	(skip-comment)
	(skip-white-peek))
       (else c))))
  
  (define (skip-comment)
    (let ((c (%%read-char port)))
      (if (and (not (eof-object? c))
	       (not (%%char=? c #\newline)))
	  (skip-comment))))

  (define (read-list allow-improper-list)
    (if (%%char=? (skip-white-peek) #\))
	(begin (%%read-char port) 
	       '())
	(let ((a (read*)))
	  ;; dotted lists are a pain!
	  (if (%%char=? (skip-white-peek) #\.)
	      (begin (%%read-char port)
		     (if (or (eof-object? (%%peek-char port))
			     (memv (%%peek-char port) delimiters))
			 ;; legitimate dot
			 (begin (if (not allow-improper-list)
				    (error read "invalid vector syntax"))
				(%%read-char port) ;consume delimiter
				(let ((b (read*)))
				  (if (eof-object? b)
				      (error read "eof in list"))
				  (skip-white-peek)
				  (if (eof-object? (%%peek-char port))
				      (error read "eof in list"))
				  (if (not (%%char=? (%%read-char port) #\)))
				      (error read "missing list terminator"))
				  (cons a b)))
			 ;; not really a dot, keep trying
			 (begin (set! gathered ".")
				(cons a (read-list allow-improper-list)))))
	      (cons a (read-list allow-improper-list))))))

  ;; parse a long token, defined specifically as a number, identifier,
  ;; or character.
  (define (parse-long-token str)
    (if (and (%%char=? (%%string-ref str 0) #\#)
	     (%%char=? (%%string-ref str 1) #\\))
	(cond
	 ((%%string-ci=? str "#\\newline") #\newline)
	 ((%%string-ci=? str "#\\space") #\space)
	 ((%%string-ci=? str "#\\return") #\return)
	 ((%%string-ci=? str "#\\page") #\page)
	 ((%%string-ci=? str "#\\tab") #\tab)
	 (else (if (> (%%string-length str) 3)
		   (error read
			  (%%string-append "invalid character syntax " str))
		   (%%string-ref str 2))))
	(or (%%string->number str)
	    (%%string->symbol str))))

  (define (read-string)
    (let more ((str ""))
      (let ((c (%%read-char port)))
	(if (eof-object? c)
	    (error read "eof in string")
	    (case c
	      ((#\\) (more (%%string-append str (%%string (%%read-char port)))))
	      ((#\") str)
	      (else (more (%%string-append str (%%string c)))))))))

  (read*))

(define (write obj . port*)
  (%%write obj (if (null? port*) 
		   (current-output-port) 
		   (%%check-output-port (%%car port*)))))

(define (%%write obj port)
  (define (write-slashify str)
    (%%write-char #\" port)
    (let ((len (%%string-length str)))
      (do ((i 0 (%%+ i 1)))
	  ((%%>= i len))
	(if (or (%%char=? (%%string-ref str i) #\")
		(%%char=? (%%string-ref str i) #\\))
	    (%%write-char #\\ port))
	(%%write-char (%%string-ref str i) port)))
    (%%write-char #\" port))

  (define (write-rest tail)
    (cond
     ((null? tail) (%%write-char #\) port))
     ((pair? tail)
      (%%write-char #\space port)
      (%%write (%%car tail) port)
      (write-rest (%%cdr tail)))
     (else (%%display " . " port)
	   (%%write tail port)
	   (%%write-char #\) port))))

  (cond
   ((eq? obj #t) (%%display "#t" port))
   ((eq? obj #f) (%%display "#f" port))
   ((symbol? obj) (%%display (%%symbol->string obj) port))
   ((char? obj) (%%display (case obj
			     ((#\newline) "#\\newline")
			     ((#\return) "#\\return")
			     ((#\page) "#\\page")
			     ((#\tab) "#\\tab")
			     ((#\space) "#\\space")
			     (else (%%string #\# #\\ obj))) 
			   port))
   ((vector? obj) (%%write-char #\# port) (%%write (%%vector->list obj) port))
   ((pair? obj) 
    (%%write-char #\( port)
    (%%write (%%car obj) port) 
    (write-rest (%%cdr obj)))
   ((null? obj) (%%display "()" port))
   ((number? obj) (%%display (%%number->string obj) port))
   ((string? obj) (write-slashify obj))
   ((procedure? obj) (%%display "#<procedure>" port))
   ((eof-object? obj) (%%display "#<eof>" port))
   ((input-port? obj) (%%display "#<input port>" port))
   ((output-port? obj) (%%display "#<output port>" port))))

(define (display obj . port*)
  (%%display obj (if (null? port*) 
		     (current-output-port) 
		     (%%check-output-port (%%car port*)))))

(define (%%display obj port)
  (define (display-rest tail)
    (cond
     ((null? tail) (%%write-char #\) port))
     ((pair? tail)
      (%%write-char #\space port)
      (%%display (%%car tail) port)
      (display-rest (%%cdr tail)))
     (else (%%display " . " port)
	   (%%display tail port)
	   (%%write-char #\) port))))

  (cond
   ((eq? obj #t) (%%display "#t" port))
   ((eq? obj #f) (%%display "#f" port))
   ((symbol? obj) (%%display (%%symbol->string obj) port))
   ((char? obj) (%%write-char obj port))
   ((vector? obj) (%%write-char #\# port) (%%display (%%vector->list obj)
						     port))
   ((pair? obj) (%%write-char #\( port)
    (%%display (%%car obj) port) 
    (display-rest (%%cdr obj)))
   ((null? obj) (%%display "()" port))
   ((number? obj) (%%display (%%number->string obj) port))
   ((string? obj) (let ((len (%%string-length obj)))
		    (do ((i 0 (%%+ i 1)))
			((%%>= i len))
		      (%%write-char (%%string-ref obj i) port))))
   ((procedure? obj) (%%display "#<procedure>" port))
   ((eof-object? obj) (%%display "#<eof>" port))
   ((port? obj) (%%display "#<port>" port))))
    
(define (newline . port*)
  (%%write-char #\newline 
		(if (null? port*) (current-output-port) 
		    (%%check-output-port (%%car port*)))))

;;; SYSTEM INTERFACE
;;;
;;; Not implemented:
;;; load transcript-on transcript-off

;;; EXTENSIONS AND SUPPORT
(define (js-throw name obj message)
  (foreign-inline "(function (){throw{name:~a,obj:~a,message:~a};})()"
		  name obj (string->symbol message)))
(define (type-error obj type-name)
  (js-throw 'SINJStypeerror obj (string-append "not a " type-name)))
(define (length-error k name)
  (js-throw 'SINJSlengtherror k
	    (string-append "out of bounds for access to " name)))
(define-syntax type-checker
  (syntax-rules ()
    ((_ scheme-name predicate? name)
     (define (scheme-name obj)
       (if (predicate? obj)
	   obj
	   (type-error obj name))))))
		
(type-checker %%check-number number? "number")
(type-checker %%check-integer integer? "integer")
(type-checker %%check-pair pair? "pair")
(type-checker %%check-symbol symbol? "symbol")
(type-checker %%check-string string? "string")
(type-checker %%check-char char? "character")
(type-checker %%check-vector vector? "vector")
(type-checker %%check-output-port output-port? "output port")
(type-checker %%check-input-port input-port? "input port")

(define-syntax length-checker
  (syntax-rules ()
    ((_ scheme-name len-proc name)
     (define (scheme-name obj k)
       (if (and (>= k 0) 
		(%%< k (len-proc obj)))
	   k
	   (length-error k name))))))
(length-checker %%check-string-len string-length "string")
(length-checker %%check-vector-len vector-length "vector")

(define (%%check-substring s start end)
  (if (< start 0)
      (length-error start "string"))
  (if (> start end)
      (length-error start "string"))
  (if (%%> end (string-length s))
      (length-error end "string")))

(define (error name str)
  (js-throw 'SINJSerror name str))
