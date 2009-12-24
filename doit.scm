(load "compile")
(load "expand")
(load "cps")
(load "simplify")
(load "toplevel")

(define (compile file)
  (display 
   (top-level-compile-forms
    (call-with-input-file file
      (lambda (p)
	(let more ((so-far '()))
	  (let ((obj (read p)))
	    (if (eof-object? obj)
		(reverse! so-far)
		(more (cons obj so-far)))))))))
  (newline))


