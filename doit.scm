(load "compile")
(load "expand")
(load "integrate")
(load "cps")
(load "simplify")
(load "toplevel")

(define (compile-to forms out)
  (call-with-output-file out
    (lambda (p)
      (display (top-level-compile-forms forms) p)
      (newline p))))

(define (gather-forms in)
  (call-with-input-file in
    (lambda (p)
      (let more ((so-far '()))
	(let ((obj (read p)))
	  (if (eof-object? obj)
	      (reverse! so-far)
	      (more (cons obj so-far))))))))
  
(define (compile-naked in out)
  (compile-to (gather-forms in) out))

(define (compile in out)
  (compile-to (append (gather-forms "library.scm")
		      (gather-forms in))
	      out))

