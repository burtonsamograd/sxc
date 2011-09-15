#!/usr/local/bin/sbcl --script

;(defpackage :sxc
;  (:use :cl :sb-ext))
;(in-package :sxc)


(load "typed-cl.lisp")

(defmacro while (condition &body body)
  `(do ()
       ((not ,condition))
     ,@body))

(def t remove-tree ((t value) (t tree))
     "like remove but works on a tree"
     (ldef ((t remove-tree-helper ((symbol value) (t tree) (symbol s))
	       (if (equal tree value)
		   s
		   (if (atom tree)
		       tree
		       (remove s (cons (remove-tree-helper value (car tree) s)
				       (remove-tree-helper value (cdr tree) s)))))))
	   (remove-tree-helper value tree (gensym))))

(def list sxc-read-file ((simple-string filename))
  "read while file from s and return a list of forms, with each form containing a
a combination of the form a it's line number. ie:
 ((1 |#include| |<stdio.h>|)
   (3 |float| |square| (3 (3 |float| |x|))
   (4 |return| (4 * |x| |x|)))
   (6 |int| |main| (6 (6 |int| |argc|) (6 |char| (6 ** |argv|)))
   (7 |int| (7 = |i| |0|) (8 = |j| |1|)))

after reading:

  (#include <stdio.h>)

  (float square ((float x))
       (return (* x x)))

  (int main ((int argc) (char (** argv)))
       (int (= i 0)
	    (= j 1)))
"

  (with-open-file (s filename)
     (vars ((fixnum curline 1)
	    (symbol nil-form (gensym))
	    (symbol comment-form (gensym)))
       (ldef ((character getc ()
			 (vars ((character c (read-char s t)))
			   (when (char= c #\Newline)
			     (incf curline))
			   c))
	      (character peekc ()
			 (peek-char nil s))
	      (nil skip-whitespace ()
		     (loop
			(vars ((character c (peekc)))
			  (if (or (char= c #\ ) (char= c #\Newline)
				  (char= c #\Return) (char= c #\Tab))
			      (getc)
			      (return-from skip-whitespace nil)))))
	      (simple-string read-string ((stream s))
			     ;; strings are delimited by "..." with the only escaped characters being \" and \\
			     (vars ((array result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
				    (character c (getc)))
			       (while (not (char= c #\"))
				 (when (char= c #\\)
				   (vars ((character c2 (peekc)))
				     (when (or (char= c2 #\") (char= c2 #\\))
				       (vector-push-extend c result)
				       (setf c (getc)))))
				 (vector-push-extend c result)
				 (setf c (getc)))
			       result))
	      (symbol read-symbol ((character start-char))
		      (vars ((array result (make-array 1 :initial-element start-char
						       :element-type 'character :adjustable t :fill-pointer 1))
			     (character c (peekc))
			     (string terminators (format nil "()\"' ~C~C~C" #\Newline #\Return #\Tab)))
			(while (not (find c terminators))
			  (vector-push-extend (getc) result)
			  (setf c (peekc)))
			(intern result)))
	      (symbol skip-comment ()
		      ;; skip /* comments */, return comment-form
		      (vars ((fixnum lineno curline)
			     (fixnum nesting 1)
			     (character c (getc)))
			(handler-case 
			    (loop
			       (case c
				 (#\/
				  (vars ((character c2 (getc)))
				    (when (char= c2 #\*)
				      (incf nesting))))
				 (#\*
				  (vars ((character c2 (getc)))
				    (when (char= c2 #\/)
				      (decf nesting)))))
			       (when (= nesting 0)
				 (return-from skip-comment comment-form))
			       (setf c (getc)))
			  (end-of-file (e)
			      (format *error-output* "~A: ~A: unterminated /* ... */ comment starting on line ~A.~%"
			       filename lineno lineno)))))
	      (symbol skip-line-comment ()
		      ;; skip // comments, return comment-form
		      (vars ((character c (getc)))
			(loop
			     (when (eq c #\Newline)
			       (return-from skip-line-comment comment-form))
			   (setf c (getc)))))
	      ((or list symbol string) read-form ()
		 ; read a form, returning list, symbol or string
		 (skip-whitespace)
		 (vars ((character c (getc))
			(fixnum lineno curline))
		   (case c
		     (#\(
		      (skip-whitespace)
		      (if (char= (peekc) #\))
			  (progn
			    (getc)
			    nil-form)
			  (progn
			    (vars ((list result nil)
				   (t form nil))
			      (loop
				 (setf form (read-form))
				 (if form
				     (push form result)
				     (return-from read-form `(,lineno .  ,(subst nil nil-form (nreverse result))))))))))
		     (#\) nil)
		     (#\'
		      (vars ((character c2 (peekc)))
			     (if (eq c2 #\ )
				 `(,lineno . (quote | |))
				 `(,lineno . (quote ,(read-symbol (getc)))))))
		     (#\" (read-string s))
		     (#\/ ; start of comment possibly
		      (vars ((character c2 (peekc)))
			(case c2
			  (#\* (getc) (skip-comment))
			  (#\/ (skip-line-comment))
			  (otherwise ; not comment, read symbol
			   (read-symbol c)))))
		     ;(#\/ (read-comment s)
		     (otherwise
		      (read-symbol c))))))
	 (vars ((list retval nil))
	   (handler-case 
	       (loop
		  (vars (((or list symbol) form (read-form)))
		    (push (if (eq form nil-form) nil form) retval)))
	     (end-of-file (e)
	       (remove-tree comment-form (nreverse retval)))))))))

;(format t "~A~%" (sxc-read-file "test.sxc"))

(def simple-string read-whole-file ((simple-string filename))
  (vars ((simple-string contents "")
	 (simple-string line ""))
    (with-open-file (file filename)
      (handler-case
	  (for (line (read-line file)) t (setf line (read-line file))
	    (setf contents (concatenate 'simple-string contents line (format nil "~A" #\Newline))))
	(end-of-file (e)
	    (return-from read-whole-file contents))))))

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

#| c language keywords

auto
break
case
char
const
continue
default
do
double
else
enum
extern
float
for
goto
if
int
long
register
return
short
signed
sizeof
static
struct
switch
typedef
union
unsigned
void
volatile
while 

|#

(def simple-string c-output-while ((list form)  (simple-string filename) (stream output-stream))
     (destructuring-bind (while-form condition &rest body) form
       (with-output-to-string (s)
	 (format s "while (~A) {" (output-c-helper condition filename output-stream))
	 (mapcar (lambda (expr)
		   (format s "~A;~%" (output-c-helper expr filename output-stream)))
		 body)
	 (format s "}")
	 s)))

(def simple-string c-output-for ((list form)  (simple-string filename) (stream output-stream))
     (ldef ((nil output-multiple-forms ((list forms) (stream s) (boolean print-final-semicolon))
		 (vars ((fixnum nforms (1- (length forms)))
			(fixnum curform 0))
		       (map nil (lambda (form)
				  (incf curform)
				  (if (= curform nforms)
				      (if print-final-semicolon
					  (format s "~A; " (output-c-helper form filename output-stream))
					  (format s "~A" (output-c-helper form filename output-stream)))
				      (format s "~A, " (output-c-helper form filename output-stream))))
			    (cdr forms)))))
	   (destructuring-bind (for-form init-forms condition-forms update-forms &rest body) form
	     (with-output-to-string (s)
	       (format s "for(")
	       (if (atom (cadr init-forms))
		   (format s "~A; " (output-c-helper init-forms filename output-stream))
		   (output-multiple-forms init-forms s t))
	       (if (atom (cadr condition-forms))
		   (format s "~A; " (output-c-helper condition-forms filename output-stream))
		   (output-multiple-forms condition-forms s t))
	       (if (atom (cadr update-forms))
		   (format s "~A " (output-c-helper update-forms filename output-stream))
		   (output-multiple-forms update-forms s nil))
	       (format s ") {~%")
	       (mapcar (lambda (body-form)
			 (format s "~A;~%" (output-c-helper body-form filename output-stream)))
		       body)
	       (format s "}")
	       s))))

(def simple-string c-output-function-call ((list form) (simple-string filename) (stream output-stream))
     (vars ((fixnum numargs (length (cdr form)))
	    (fixnum curarg 0))
	   (if (= numargs 0)
	       (format nil "~A()" (car form))
	       (with-output-to-string (s)
		 (format s "~A(" (car form))
		 (mapcar (lambda (arg)
			   (incf curarg)
			   (if (= curarg numargs)
			       (format s "~A)" (output-c-helper arg filename output-stream))
			       (format s "~A, " (output-c-helper arg filename output-stream))))
			 (cdr form))
		 s))))

(def simple-string c-output-variable-decleration ((list form) (simple-string filename) (stream output-stream))
  (with-output-to-string (s)
    (vars* ((fixnum nvars (- (length form) 2))
	    (fixnum curvar 0))
      (format s "~A ~A " (if (eq (first form) '|var|)
			     ""
			     (first form))
	      (output-c-type-helper (strip-lineno (second form) filename output-stream)  filename output-stream))
      (mapcar (lambda (var)
		(incf curvar)
		(if (= curvar nvars)
		    (format s "~A" (output-c-type-helper (strip-lineno var filename output-stream) filename output-stream))
		    (format s "~A, " (output-c-type-helper (strip-lineno var filename output-stream) filename output-stream))))
	      (cddr form))
      s)))

(def simple-string c-output-if ((list form) (simple-string filename) (stream output-stream))
     (with-output-to-string (s)
       (destructuring-bind (if-atom condition &rest body) form
	 (format s "if(~A) {" (output-c-helper condition filename output-stream))
	 (mapcar (lambda (body-form)
		   (if (and (listp body-form)
			    (eq (cadr body-form) '|else|))
		       ;; else forms are inside the if body; solves dangling else problem
		       ;; but that means you can't use a function named else in the body of 
		       ;; an if statement (which you can't do in C anyways)
		       (progn
			 (format s "} else {~%")
			 (mapcar (lambda (else-body-form)
				   ;(print else-body-form)
				   (format s "~A;~%" (output-c-helper else-body-form filename output-stream)))
				 (cddr body-form)))
		       (format s "~A;~%" (output-c-helper body-form filename output-stream))))
		 body)
	 (format s "}")
	 s)))

(def simple-string c-output-infix-operator ((list form) (simple-string filename) (stream output-stream))
     "output c code for infix operators, with special cases for urinary - and ~"
     (when (and (or (eq (car form) '*)( eq (car form) '**)
		    (eq (car form) '***) (eq (car form) '****))
		(= (length form) 2))
       ;; special case pointer dereference (*... var)
       (return-from c-output-infix-operator (format nil "(~A~A)"
						    (first form) (output-c-helper (second form) filename output-stream))))
     ;;(when (and (< (length (rest form) 2) ; TODO: pass in filename and lineno to this function
     ;;           (not (eq (car form) '-))) ; special case for urinary -
     ;; (error "~A: ~A: infix operator ~A has less than 2 arguments" filename lineno (car form)
     (if (and (or (eq (car form) '-)
		  (eq (car form) '~))
	      (= (length (rest form)) 1))
	 (format nil "(~A~A)" (car form) (output-c-helper (second form) filename output-stream))
	 (if (= (length (rest form)) 2)
	     (format nil "((~A) ~A (~A))" ;; infix with 2 args
		     (output-c-helper (second form) filename output-stream)
		     (car form)
		     (output-c-helper (third form) filename output-stream))
	     (with-output-to-string (s) ;; make infix operators more lispy ie. (+ 1 2 3 4) => (1 + 2 + 3 + 4)
	       (vars* ((symbol op (first form))
		       (t first-arg (second form))
		       (list rest-args (cddr form))
		       (fixnum num-rest-args (length rest-args))
		       (fixnum cur-rest-arg 0))
		      (format s "((~A) ~A " (output-c-helper (second form) filename output-stream) (first form))
		      (mapcar (lambda (arg)
				(incf cur-rest-arg)
				(if (< cur-rest-arg num-rest-args)
				    (format s "(~A) ~A " (output-c-helper arg filename output-stream) (first form))
				    (format s "(~A)) " (output-c-helper arg filename output-stream) (first form))))
			      (cddr form))
		      s)))))

(def simple-string c-output-equals ((list form) (simple-string filename) (stream output-stream))
     ;; make = more lispy ie. (= x y z 0) => x = y = z = 0;
     (vars* ((t lhs (second form))
	     (list args (cddr form))
	     (fixnum n-args (length args))
	     (fixnum cur-arg 0))
	    (with-output-to-string (s)
	      (format s "(~A) = " (output-c-helper lhs filename output-stream))
	      (mapcar (lambda (arg)
			(incf cur-arg)
			(if (and (listp arg)
				 (eq (cadr arg) '|,|))
			    (if (= cur-arg n-args) ; special case for comma operator, no parens around rhs
				(format s "~A" (output-c-helper arg filename output-stream))
				(format s "~A = " (output-c-helper arg filename output-stream)))
			    (if (= cur-arg n-args)
				(format s "(~A)" (output-c-helper arg filename output-stream))
				(format s "(~A) = " (output-c-helper arg filename output-stream)))))
		      args)
	      s)))

(def simple-string c-output-switch ((list form) (simple-string filename) (stream output-stream))
  (destructuring-bind (switch-atom expression &body body) form
    (with-output-to-string (s)
      (format s "switch (~A) {~%" (output-c-helper expression filename output-stream))
      (mapcar (lambda (switch-case)
		(if (and (listp (cadr switch-case))
			 (listp (cdadr switch-case)))
		    (if (eq (cadadr switch-case) 'quote) ; handle character constant expressions which start with (quote ...)
			(format s "case ~A: " (output-c-helper (cadr switch-case) filename output-stream))
			(mapcar (lambda (const-expr)
				  (format s "case ~A: " (output-c-helper const-expr filename output-stream)))
				(cdadr switch-case)))
		    (if (eq (cadr switch-case) '|default|)
			(format s "default: ~%")
			(format s "case ~A: " (output-c-helper (cadr switch-case) filename output-stream))))
		(format s "{~%")
		(mapcar (lambda (switch-case-expr)
			  (format s "~A;~%" (output-c-helper switch-case-expr filename output-stream)))
			(cddr switch-case))
		(format s "}~%"))
	      body)
	 (format s "}~%")
	 s)))
			       
(def simple-string c-output-comma ((list form) (simple-string filename) (stream output-stream))
     (with-output-to-string (s)
       (vars ((fixnum numexpr (length (rest form)))
	      (fixnum curexpr 0))
	     (mapcar (lambda (expr)
		       (incf curexpr)
		       (if (= curexpr numexpr)
			   (format s "~A" (output-c-helper expr filename output-stream))
			   (format s "~A, " (output-c-helper expr filename output-stream))))
		     (rest form)))
       s))

(def simple-string c-output-do-while ((list form) (simple-string filename) (stream output-stream))
     (with-output-to-string (s)
       (format s "do {~%")
       (mapcar (lambda (expr)
		 (if (and (listp expr)
			  (eq (cadr expr) '|while|))
		     (format s "} while(~A)" (output-c-helper (third expr) filename output-stream))
		     (format s "~A;~%" (output-c-helper expr filename output-stream))))
	       (cdr form))
       s))
		      
(def simple-string output-c-type-helper ((t form) (simple-string filename) (stream output-stream))
     "called by above functions when we are expecting a type decleration.
These can be of the form 'symbol (eg. char) or a list such as (unsigned char)"
;     (setf form (strip-lineno form filename output-stream))
     (with-output-to-string (s)
       (if (listp form)
	   (case (car form)
	     ((* ** *** ****) ; types with pointers need the *'s after the type, not before
	      (format s "~A~A" (output-c-type-helper (second form) filename output-stream) (first form)))
	     ([] ; there was a problem with declerations being surrounded by () so this needs to be handled specially
	      (if (= (length form) 3) 
		  (format s "~A[~A]"
			  (remove #\) (remove #\( (output-c-helper (second form) filename output-stream)))
			  (output-c-helper (third form) filename output-stream))
		  (format s "~A[]" (remove #\) (remove #\( (output-c-helper (second form) filename output-stream))))))
	     (=
	      (format s "~A" (c-output-equals form filename output-stream)))
	     (otherwise
	      (mapcar (lambda (symbol)
			(format s "~A " (output-c-helper symbol filename output-stream)))
		      form)))
	   (format s "~A " form))
       s))

(defparameter current-line 0)
(def (or symbol list string) strip-lineno (((or list symbol string) form) (simple-string filename) (stream s))
  (when (consp form)
    (vars ((fixnum curline (car form)))
      (when (not (= curline current-line))
	(setf current-line (car form))
	(format s "#line ~A \"~A\"~%" current-line filename)))
    (return-from strip-lineno (cdr form)))
  form)
     
(def simple-string output-c-helper (((or list symbol string fixnum float) form) (simple-string filename) (stream output-stream))
  (setf form (strip-lineno form filename output-stream))
;  (format t "***'~A'~%" form)
     (if (listp form)
	 (if (and (>= (length form) 2) ; special case for (x ++) or (x --)
		  (or (eq (second form) '++)
		      (eq (second form) '--)))
	     (format nil "(~A~A)" (output-c-helper (first form) filename output-stream) (second form))
		 
	     (if (listp (car form)) ; variable decleration
		 (format nil "~A ~A"
			 (output-c-helper (car form) filename output-stream)
			 (output-c-helper (second form) filename output-stream))
		 (case (car form) ; keyword, operator or function (or macro)
		   (=
		    (c-output-equals form filename output-stream))
		   ((+= -= *= /= ^= ~=)
		    (format nil "~A ~A ~A"
			    (output-c-helper (second form) filename output-stream)
			    (car form)
			    (output-c-helper (third form) filename output-stream)))
		   ((+ - * / % ^ ~ == < > <= >= != && |\|\|| |\|| |&| ** *** ****) ; infix + special operators
		    (c-output-infix-operator form filename output-stream))
		   (|,| ; comma operator
		    (c-output-comma form filename output-stream))
		   (|[]| ; array reference
		    (if (= (length form) 2)
			(format nil "~A[]" (output-c-helper (second form) filename output-stream))
			(format nil "(~A[~A])"
				(output-c-helper (second form) filename output-stream)
				(output-c-helper (third form) filename output-stream))))
		   (|if| ; if statement
		    (c-output-if form filename output-stream))
		   (|switch| ; switch statement
		    (c-output-switch form filename output-stream))
		   (|while| ;while statement
		    (c-output-while form filename output-stream))
		   (|do| ; do/while statement
		    (c-output-do-while form filename output-stream))
		   (|for| ; for statement
		    (c-output-for form filename output-stream))
		   (|cast| ; special case of cast funcall operator (cast type var)
		    (format nil "((~A)~A)"
			    (output-c-helper (second form) filename output-stream)
			    (output-c-helper (third form) filename output-stream)))
		   ((|var| |auto| |static| |extern|) ; variable decleration
		    (c-output-variable-decleration form filename output-stream))
		   (|goto| ; goto statement
		    (format nil "goto ~A" (second form) filename output-stream))
		   (|:| ; labels are precedded by a colon as a function call
		    (format nil "~A:" (second form) filename output-stream))
		   (*?++ ; hack function for performing *ptr++
		    (format nil "(*~A++)" (second form) filename output-stream))
		   (*?-- ; hack function for performing *ptr--
		    (format nil "(*~A--)" (second form) filename output-stream))
		   ('quote ; characters are single quoted, using double backslashes when required
		    (if (eq '|space| (second form))
			(format nil "' '" )
			(format nil "'~A'" (second form))))
		   (otherwise ; function call
		    (c-output-function-call form filename output-stream)))))
	     (typecase form
	       (string (format nil "\"~A\"" form))
	       (fixnum (format nil "~A" form))
	       (float (format nil "~A" form))
	       (character (format nil "'~A'" form))
	       (symbol (if (not (eq form 'comment))
			 (format nil "~A" form)
			 "__comment__")))))

(def t output-c ((list form) (simple-string filename) &optional (stream s *standard-input*))
  (case (cadr form)
    ((|#include| |#define|)
     (progn
       (setf form (strip-lineno form filename s))
       (format s "~A " (car form))
       (mapcar (lambda (x) (format s "~A " x)) (cdr form))
       (format s "~%")))
    ((|var| |static| |extern|)
     (format s "~A;~%" (output-c-helper form filename s)))
    (otherwise
     (setf form (strip-lineno form filename s))
     (handler-case ; try for function definition
	 (destructuring-bind (rettype name args &body body) form
	   ; found function definition
	   (setf args (strip-lineno args filename s))
	   (vars ((fixnum numvardecl (length args))
		  (fixnum curvardecl 0))
	     (format s "~A ~A(" (output-c-helper rettype filename s) name)
	     ; print argument list
	     (if args 
		 (mapcar (lambda (vardecl)
			   (setf vardecl (strip-lineno vardecl filename s))
			   (incf curvardecl)
			   (if (= curvardecl numvardecl)
			       (if (listp vardecl)
				   (if body
				       (format s "~A ~A) {~%"
					       (output-c-type-helper (first vardecl) filename s)
					       (output-c-helper (second vardecl) filename s))
				       (format s "~A ~A)~%"
					       (output-c-type-helper (first vardecl) filename s)
					       (output-c-helper (second vardecl) filename s)))
				   (if body
				       (format s "~A) {" (output-c-helper vardecl filename s)) ; special case for (void)
				       (format s "~A)" (output-c-helper vardecl filename s)))) ; special case for (void)
			       (format s "~A ~A, "
				       (output-c-helper (first vardecl) filename s)
				       (output-c-helper (second vardecl) filename s))))
			 args)
		 (if body
		     (format s ") {")
		     (format s ")~%")))
	     (if body
		 (progn
		   (mapcar (lambda (form)
			     (vars ((simple-string output (output-c-helper form filename s)))
				   (unless (string= output "__comment__")
				     (format s "~A;~%" output))))
			   body)
		   (format s "~%}~%"))
		 (format s ";~%"))))
       (error (e)
	 (print "--------------------------------------------------------------------------------")
	 (print e)
	 (print "--------------------------------------------------------------------------------"))))))
#|	 (vars ((simple-string output (output-c-helper form filename s)))
	       (when (not (string= output "__comment__"))
		 (format s "~A;~%" output))))))))|#

#|(def t main ()
  (vars (((list-of simple-string) files-to-process (cdr *posix-argv*)))
    (dolist (filename files-to-process)
      (multiple-value-bind (form curpos)
	  (vars ((simple-string contents (read-whole-file filename))
		 (fixnum curpos 0)
		 (fixnum i 0)
		 (fixnum curline 0))
		(setf *saved-readtable* *readtable*)
		(let ((*readtable* *sxc-readtable*))
		  (loop
		     (let ((saved-readtable-case (readtable-case *readtable*)))
		       (setf (readtable-case *readtable*) :preserve)
		       (HANDLER-CASE  ; this needs to be uppercase as we have just changed the readtable-case
			   (MULTIPLE-VALUE-BIND (FORM POS)
			       (READ-FROM-STRING CONTENTS NIL NIL :START CURPOS)
			     (SETF (READTABLE-CASE *READTABLE*) SAVED-READTABLE-CASE) ; stop requiring upcase
			     (when (not form)
			       (return))
			     (for (i curpos) (< i pos) (incf i) ; calculate curline
				  (when (char= (char contents i) #\Newline)
				    (incf curline)))
			     (setf curpos pos)
			     (setf form (remove-tree 'comment form)) ; remove 'comment symbols
			     (unless (typep form 'symbol) ; don't output top level comments
			       (output-c form filename curline *standard-output*)))
			 (END-OF-FILE (E)
			   (FORMAT *ERROR-OUTPUT* "~S: end of file while reading form at or after line ~A~%" FILENAME CURLINE)
			   (RETURN-FROM  MAIN NIL)))))))))))
|#
(def t main ()
  (vars (((list-of simple-string) files-to-process (cdr *posix-argv*)))
    (dolist (filename files-to-process)
      (vars ((list forms (sxc-read-file filename)))
;	(print forms)
	(handler-case 
	    (mapcar (lambda (form)
		      (output-c form filename *standard-output*))
		    forms)
					;	(sxc-read-file filename)))
	  (sb-int:simple-file-error (e)
	    (format *error-output* "~A: error: file not found.~%" filename)))))))

(main)