;;; SINJS Scheme library

;;;
;;; SYNTAX
;;;
;;; Syntax bindings declared here are present for all
;;; programs, and are thus specified at top level with define-syntax.
;;;
;;; The compiler handles all definitions, as well as
;;; IF, LAMBDA, SET!, QUOTE, BEGIN, LET-SYNTAX, and LETREC-SYNTAX.

;;; R5RS 4.2.1
(define-syntax cond
  (syntax-rules (=> else)
    ;; one clause forms
    ((_ (else expr exprs ...)) (begin expr exprs ...))
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
     (begin exprs ...))
    ((_ key ((datums ...) exprs ...))
     (if (memv key 'datums) (begin exprs ...)))

    ;; multiple clause form
    ((_ key ((datums ...) exprs ...) more ...)
     (if (memv key 'datums) (begin exprs ...) (case key more ...)))))

(define-syntax and
  (syntax-rules ()
    ((_) #t)
    ((_ x) x)
    ((_ x y ...) (if x (and y ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((_) #f)
    ((_ x) x)
    ((_ x y ...) (let ((tmp x)) if tmp tmp (or y ...)))))

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
	     (loop (last var step ...) ...)))))))

;;; 4.2.5
;;; The R5RS sample implementation requires an auxiliary procedure which
;;; we'd like to avoid; if you put the procedure into the syntax-rules then
;;; users of promise get forced inlining.  So instead, we capture the 
;;; expression here in a cons (FORCED . proc/val), and all the rest
;;; of the work is in FORCE.
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
;;; To avoid violating the inviolability rules which require
;;; standard procedures to always do the standard thing, even if other
;;; procedure name bindings have been assigned, we embed all of 
;;; them in a big let, and then return an alist for the procedures.
;;; The system then does the right thing to turn this alist into
;;; top level bindings.
;;;

       



;;;
;;; PROCEDURES (equivalence)
;;;
;;; EQ? is builtin.

(define (eqv? a b)
  (or (eq? a b)
      (and (char? a)
	   (char? b)
	   (char=? a b))
      (and (number? a)
	   (number? b)
	   (= a b))))

(define (equal? a b)
  (or (eqv? a b)
      (and (pair? a)
	   (pair? b)
	   (equal? (car a) (car b))
	   (equal? (cdr a) (cdr b)))
      (and (vector? a)
	   (vector? b)
	   (= (vector-length a) (vector-length b))
	   (let next-elt ((i 0))
	     (or (= i (vector-length a))
		 (and (equal? (vector-ref a i) (vector-ref b i))
		      (next-elt (+ i 1))))))
      (and (string? a)
	   (string? b)
	   (string=? a b))))

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

;;;
;;; Builtin are the following procedures:
;;;
;;; number? < > <= >= + * - / abs quotient remainder modulo
;;; gcd lcm numerator denominator floor ceiling truncate round
;;; rationalize exp log sin cos tan asin acos atan sqrt expt

;;; The following are not provided, in accord with the permission
;;; of R5RS to omit them if we don't support general complex numbers:
;;; 
;;; make-rectangular make-polar real-part imag-part magnitude angle

;;; XXX Not yet implemented: number->string string->number


(define complex? number?)
(define real? number?)
(define rational? number?)
(define (integer? obj)
  (and (number? obj)
       (= obj (floor obj))))

(define (exact? z) #f)
(define (inexact? z) #t)

;;; for now, this is correct
(define = eq?)

(define (zero? z)
  (= n 0))
(define (positive? x)
  (> n 0))
(define (negative? x)
  (< n 0))
(define (odd? n)
  (= 1 (remainder n 2)))
(define (even? n)
  (= 0 (remainder n 2)))

(define (max z1 . zs)
  (if (null? zs)
      z1
      (if (> z1 (car zs))
	  (apply max z1 (cdr zs))
	  (apply max zs))))

(define (min z1 . zs)
  (if (null? zs)
      z1
      (if (< z1 (car zs))
	  (apply min z1 (cdr zs))
	  (apply min zs))))

(define (exact->inexact z) z)
(define (inexact->exact z) z)

;;;
;;; PROCEDURES (other data types)  
;;;
;;; Builtin are:
;;;
;;; pair? cons car cdr set-car! set-cdr!
;;; symbol? symbol->string string->symbol 
;;; char? char->integer integer->char
;;; string? make-string string-length string-ref string-set!
;;; vector? make-vector vector-length vector-ref vector-set!

;;; Not yet defined are:
;;; char-alphabetic? char-numeric? char-whitespace? 
;;; char-upper-case? char-lower-case? char-upcase char-downcase

(define (boolean? obj)
  (or (eq? obj #t)
      (eq? obj #f)))

(define (not obj)
  (if obj #f #t))

(define (caar obj) (car (car obj)))
(define (cadr obj) (car (cdr obj)))
(define (cdar obj) (cdr (car obj)))
(define (cddr obj) (cdr (cdr obj)))

(define (caaar obj) (car (caar obj)))
(define (caadr obj) (car (cadr obj)))
(define (cadar obj) (car (cdar obj)))
(define (caddr obj) (car (cddr obj)))
(define (cdaar obj) (cdr (caar obj)))
(define (cdadr obj) (cdr (cadr obj)))
(define (cddar obj) (cdr (cdar obj)))
(define (cdddr obj) (cdr (cddr obj)))

(define (caaaar obj) (car (caaar obj)))
(define (caaadr obj) (car (caadr obj)))
(define (caadar obj) (car (cadar obj)))
(define (caaddr obj) (car (caddr obj)))
(define (cadaar obj) (car (cdaar obj)))
(define (cadadr obj) (car (cdadr obj)))
(define (caddar obj) (car (cddar obj)))
(define (cadddr obj) (car (cdddr obj)))
(define (cdaaar obj) (cdr (caaar obj)))
(define (cdaadr obj) (cdr (caadr obj)))
(define (cdadar obj) (cdr (cadar obj)))
(define (cdaddr obj) (cdr (caddr obj)))
(define (cddaar obj) (cdr (cdaar obj)))
(define (cddadr obj) (cdr (cdadr obj)))
(define (cdddar obj) (cdr (cddar obj)))
(define (cddddr obj) (cdr (cdddr obj)))

(define (null? obj)
  (eq? obj '()))

(define (list? obj)
  (or (null? obj)
      (and (pair? obj)
	   (list? (cdr obj)))))

(define (list . objs) objs)

(define (length list)
  (if (null? list)
      0
      (+ 1 (length (cdr list)))))

(define (append . lists)
  (cond
   ((null? lists) '())
   ((null? (car lists)) (apply append (cdr lists)))
   (else (cons (caar lists) (apply append (cdar lists) (cdr lists))))))

(define (reverse lyst)
  (if (null? lyst)
      '()
      (append (reverse lyst) (list (car lyst)))))

(define (list-tail list k)
  (if (zero? k)
      list
      (list-tail (cdr list) (- k 1))))

(define (list-ref list k)
  (if (zero? k)
      (car list)
      (list-ref (cdr list) (- k 1))))

(define (memq obj list)
  (and (pair? list)
       (if (eq? obj (car list))
	   list
	   (memq obj (cdr list)))))

(define (memv obj list)
  (and (pair? list)
       (if (eqv? obj (car list))
	   list
	   (memv obj (cdr list)))))

(define (member obj list)
  (and (pair? list)
       (if (equal? obj (car list))
	   list
	   (member obj (cdr list)))))

(define (assq obj alist)
  (and (pair? alist)
       (if (eq? obj (caar alist))
	   (car alist)
	   (assq obj (cdr alist)))))

(define (assv obj alist)
  (and (pair? alist)
       (if (eqv? obj (caar alist))
	   (car alist)
	   (assv obj (cdr alist)))))

(define (assoc obj alist)
  (and (pair? alist)
       (if (equal? obj (caar alist))
	   (car alist)
	   (assoc obj (cdr alist)))))

(define-syntax char-compare
  (syntax-rules ()
    ((_ name test? cvt)
     (define (name char1 char2)
       (test? (cvt char1) (cvt char2))))))

(char-compare char=? = char->integer)
(char-compare char<? < char->integer)
(char-compare char>? > char->integer)
(char-compare char<=? <= char->integer)
(char-compare char>=? >= char->integer)
(char-compare char-ci=? char=? char-upcase)
(char-compare char-ci<? char<? char-upcase)
(char-compare char-ci>? char>? char-upcase)
(char-compare char-ci<=? char<=? char-upcase)
(char-compare char-ci>=? char>=? char-upcase)

;; this is bad, given that string-set! is so slow given
;; the immutability of JS strings.
(define (string . chars)
  (let ((s (make-string (length chars))))
    (let next ((i 0)
	       (l chars))
      (if (null? l)
	  s
	  (begin (string-set! s 0 (car l))
		 (next (+ i 1) (cdr l)))))))

(define-syntax string-equality
  (syntax-rules ()
    ((_ name chartest?)
     (define (name string1 string2)
       (let ((len (string-len string1)))
	 (and (= len (string-len string2))
	      (let next ((i 0))
		(or (= i len)
		    (and (chartest? (string-ref string1 i)
				    (string-ref string2 i))
			 (next (+ i 1)))))))))))
       
(string-equality string=? char=?)
(string-equality string-ci=? char-ci=?)

(define-syntax string-compare
  (syntax-rules ()
    ((_ name lentest? chartest? chareqtest?)
     (define (name string1 string2)
       (let ((len1 (string-len string1))
	     (len2 (string-len string2)))
	 (let ((minlen (min len1 len2)))
	   (let next ((i 0))
	     (if (= i minlen)
		 (lentest? len1 len2)
		 (or (chartest? (string-ref string1 i)
				(string-ref string2 i))
		     (and (chareqtest? (string-ref string1 i))
			  (chareqtest? (string-ref string2 i))
			  (next (+ i 1))))))))))))

(string-compare string<? < char<? char=?)
(string-compare string>? > char>? char=?)
(string-compare string<=? <= char<=? char=?)
(string-compare string>=? >= char>=? char=?)
(string-compare string-ci<? < char-ci<? char-ci=?)
(string-compare string-ci>? > char-ci>? char-ci=?)
(string-compare string-ci<=? <= char-ci<=? char-ci=?)
(string-compare string-ci>=? >= char-ci>=? char-ci=?)

(define (substring s start end)
  (let ((news (make-string (- end start))))
    (do ((i start (+ i 1))
	 (j 0 (+ j 1)))
	((= i end) news)
      (string-set! news j (string-ref s i)))))

(define (string-append . strings)
  (let ((s (make-string (apply + (map string-len strings)))))
    (let next-string ((i 0)
		      (strings strings))
      (if (null? strings)
	  s
	  (next-string
	   (+ i (let* ((this-string (car strings))
		       (this-len (string-len this-string)))
		  (do ((j 0 (+ j 1)))
		      ((= j len) len)
		    (string-set! s i (string-ref this-string j)))))
	   (cdr strings))))))

(define (string->list s)
  (let ((len (string-len s)))
    (let next-char ((i len)
		    (lis '()))
      (if (= len 0)
	  lis
	  (next-char (- i 1) (cons (string-ref s (- i 1)) lis))))))

(define (list->string lis)
  (let* ((len (length lis))
	 (s (make-string len)))
    (let next-char ((i 0)
		    (lis lis))
      (if (= i len)
	  s
	  (begin (string-set! s i (car lis))
		 (next-char (+ i 1) (cdr lis)))))))

(define (string-copy s)
  (substring s 0 (string-len s)))

(define (string-fill! s c)
  (let ((len (string-len s)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (string-set! s i c))))

(define (vector . objs)
  (list->vector objs))

(define (vector->list v)
  (let ((len (vector-length v)))
    (let next-elt ((i len)
		   (lis '()))
      (if (= len 0)
	  lis
	  (next-elt (- i 1) (cons (vector-ref v (- i 1)) lis))))))

(define (list->vector lis)
  (let ((v (make-vector (length lis))))
    (do ((i 0 (+ i 1))
	 (lis lis (cdr lis)))
	((null? lis) v)
      (vector-set! v i (car lis)))))

(define (vector-fill! v elt)
  (let ((len (vector-len v)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (vector-set! v i c))))

;;;
;;; PROCEDURES (control features)
;;;
;;; Builtin are:
;;; procedure? apply call-with-current-continuation call-with-values
;;; dynamic-wind

(define (map proc list . lists)
  (if (null? lists)
      (let map1 ((inlist list)
		 (outlist '()))
	(if (null? inlist)
	    (reverse outlist)
	    (map1 (cdr inlist) (cons (proc (car inlist)) outlist))))
      (let map1 ((inlists (cons list lists))
		 (outlist '()))
	(if (null? (car inlists))
	    (reverse outlist)
	    (map1 (map cdr inlists)
		  (cons (apply proc (map car inlists)) outlist))))))

(define (for-each proc . lists)
  (if (not (null? (car lists)))
      (begin
	(apply proc (map car lists))
	(for-each proc (map cdr lists)))))

;; Recall that a promise is a cons (FORCED . PROC/VAL).  If FORCED, 
;; the cdr holds the value; if not forced, it holds a thunk.
(define (force promise)
  (if (car promise)
      (cdr promise)
      (let ((val ((cdr promise))))
	(if (car promise)
	    (cdr promise)
	    (begin (set-car! promise #t)
		   (set-cdr! promise val)
		   val)))))

(define (values . vals)
  (call-with-current-continuation
   (lambda (c)
     (apply c vals))))

;;;
;;; EVAL
;;;
;;; Not implemented:
;;; eval scheme-report-environment null-environment interaction-environment

;;; INPUT AND OUTPUT
;;;
;;; Not implemented:
;;; call-with-input-file call-with-output-file input-port? output-port?
;;; current-input-port current-output-port 
;;; with-input-from-file with-output-to-file open-input-file open-output-file
;;; close-input-port close-output-port
;;; read read-char peek-char eof-object? char-ready?
;;; write display newline write-char

;;; SYSTEM INTERFACE
;;;
;;; Not implemented:
;;; load transcript-on transcript-off
