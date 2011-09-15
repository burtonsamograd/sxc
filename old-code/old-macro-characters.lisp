(defparameter *saved-readtable* *readtable*)
(defparameter *sxc-readtable* (copy-readtable nil))
; disable # as macro character so we can handle (#include ...) and such
(set-macro-character #\# (lambda (stream closec)
			   (declare (ignore stream closec))
			   '|#|)
		     nil *sxc-readtable*)
(set-macro-character #\\ (lambda (stream closec)
			    (declare (ignore stream closec))
			    (format nil "\\~A" (read-char stream)))
		     nil *sxc-readtable*)
(set-macro-character #\: (lambda (stream closec)
			   (declare (ignore stream closec))
			   '|:|)
		     nil *sxc-readtable*)
(set-macro-character #\, (lambda (stream closec)
			   (declare (ignore closec))
			   (let ((c (peek-char nil stream)))
			     (if (char= c #\ )
				 (progn
				   (read-char stream)
				   '|,|)
				 (read stream))))
		     
				 ;(let ((*readtable* *saved-readtable*))
				  ; (eval (read stream))))))
		     nil *sxc-readtable*)
(require 'sb-cltl2)
#|(set-macro-character #\$ (lambda (stream closec)
			   (declare (ignore closec))
			   (let ((c (peek-char nil stream)))
			     (error (macroexpand (read stream)))
			     (if (char= c #\ )
				 (progn
				   (read-char stream)
				   '|$|)
				 (macroexpand (read stream)))))
;				 (sb-cltl2:macroexpand-all (read stream)))))
		     nil *sxc-readtable*)
|#
(set-macro-character #\| (lambda (stream closec)
			   (declare (ignore closec))
			   (let ((c (peek-char nil stream)))
			     (if (char= c #\|)
				 (progn
				   (read-char stream)
				   '|\|\||)
				 '|\||)))
		     nil *sxc-readtable*)
; handle single quote (') for characters, '  (quote space) for space character
(set-macro-character #\' (lambda (stream closec)
			   (declare (ignore closec))
			   (let ((c (peek-char nil stream)))
			     (if (or (char= c #\ ) (char= c #\.) (char= c #\,))
				 (progn
				   (read-char stream)
				   `(quote ,(intern (make-array 1 :element-type 'character :initial-element c))))
				 `(quote ,(read stream)))))
		     nil *sxc-readtable*)
; handle c/c++ style comments
(set-macro-character #\/ (lambda (stream closec) 
			   (declare (ignore closec))
			   (block nil
			     (vars ((character c (peek-char nil stream)))
			       (case c
				 (#\/ (read-line stream) (return 'comment)) ; c++ comments
				 (#\*			    ; c comments
				  (vars ((fixnum nesting 1))
					(loop
					   (vars ((character c (peek-char nil stream)))
						 (case c
						   (#\/
						    (read-char stream)
						    (vars ((character c2 (peek-char nil stream)))
							  (when (char= c2 #\*)
							    (read-char stream)
							    (incf nesting))))
						   (#\*
						    (read-char stream)
						    (vars ((character c2 (peek-char nil stream)))
							  (when (char= c2 #\/)
							    (read-char stream)
							    (decf nesting))))
						   (otherwise 
						    (read-char stream))))
					   (when (= nesting 0)
					     (return 'comment)))))
			       (otherwise
				(vars ((character c (peek-char nil stream)))
				  (if (not (char= c #\ ))
				      (intern (concatenate 'string "/" (symbol-name (read stream))))
				      '|/|)))))))
		     nil *sxc-readtable*)

