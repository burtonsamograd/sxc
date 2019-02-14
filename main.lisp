;; main.lisp: sxc
;;
;; sxc is a command line C-like s-expression to 'C' transpiler.
;;
;; Burton Samograd
;; 2018
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

(defpackage :main
  (:use cl)
  (:export main))

(load "typed-cl.lisp")

(defparameter *macros* (make-hash-table :test #'equalp))

(def (list-of symbol) expand-macro (((list-of symbol) body))
     body)

(declaim (ftype (function ((or list symbol string fixnum float) simple-string stream) t) output-c-helper))
(declaim (ftype (function (t simple-string stream) t) output-c-type-helper))

(defmacro while (condition &body body)
  `(do ()
       ((not ,condition))
     ,@body))

(defmacro emit-warning (filename lineno format-string &rest format-args)
  `(format *error-output* ,(concatenate 'string
					"~A: ~A: warning: "
					format-string) ,filename ,lineno ,@format-args))

(defmacro emit-error (filename lineno format-string &rest format-args)
  `(format *error-output* ,(concatenate 'string
					"~A: ~A: error: "
					format-string) ,filename ,lineno ,@format-args))

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
	    (symbol comment-form (gensym))
	    ((list-of fixnum) reading-form-lines ()))
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
	      (simple-string read-string ()
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
			    (declare (ignore e))
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
		    (push lineno reading-form-lines)
		    (if (char= (peekc) #\))
			(progn
			  (getc)
			  (pop reading-form-lines)
			  nil-form)
			(progn
			  (vars ((list result nil)
				 (t form nil))
			    (loop
			       (setf form (read-form))
			       (if form
				   (push form result)
				   (return-from read-form `(,lineno .  ,(subst nil nil-form (nreverse result))))))))))
		   (#\) (pop reading-form-lines) nil)
		   (#\'
		    (vars ((character c2 (peekc)))
		      (if (eq c2 #\ )
			  `(,lineno . (quote | |))
			  `(,lineno . (quote ,(read-symbol (getc)))))))
		   (#\" (read-string))
		   (#\/ ; start of comment possibly
		    (vars ((character c2 (peekc)))
		      (case c2
			(#\* (getc) (skip-comment))
			(#\/ (skip-line-comment))
			(otherwise ; not comment, read symbol
			 (read-symbol c)))))
		   (otherwise
		    (read-symbol c))))))
	 (vars ((list retval nil))
	   (handler-case 
	       (loop
		  (vars (((or list symbol) form (read-form)))
		    (push (if (eq form nil-form) nil form) retval)))
	     (end-of-file (e)
	       (declare (ignore e))
	       (if reading-form-lines
		   (emit-error filename (car reading-form-lines) "end of file when reading form.~%")
		   (remove-tree comment-form (nreverse retval))))))))))

;(format t "~A~%" (sxc-read-file "test.sxc"))

(def simple-string read-whole-file ((simple-string filename))
  (vars ((simple-string contents "")
	 (simple-string line ""))
    (with-open-file (file filename)
      (handler-case
	  (for (line (read-line file)) t (setf line (read-line file))
	    (setf contents (concatenate 'simple-string contents line (format nil "~A" #\Newline))))
	(end-of-file (e)
	  (declare (ignore e))
	  (return-from read-whole-file contents))))))

(defparameter current-line 0)
(def (or symbol list string) strip-lineno (((or list symbol string) form) (simple-string filename) (stream s))
  (when (consp form)
    (vars ((fixnum curline (car form)))
      (when (not (= curline current-line))
	(setf current-line (car form))))
    ;; FIXME:  for some reason this is causing problems, uncomment to see what it is.
    ;;         the newline isn't being printed at the end of all lines!  causes compilation errors
    ;(format s "#line ~A \"~A\"~%" current-line filename)
    (return-from strip-lineno (cdr form)))
  form)
     
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

(def t c-output-while ((list form)  (simple-string filename) (stream s))
  (destructuring-bind (while-form condition &rest body) form
    (declare (ignore while-form))
    (format s "while ((")
    (output-c-helper condition filename s)
    (format s ")) {~%")
    (mapcar (lambda (expr)
	      (output-c-helper expr filename s)
	      (format s ";~%" ))
	    body)
    (format s "}")))


(def t c-output-for ((list form)  (simple-string filename) (stream s))
     (ldef ((nil output-multiple-forms ((list forms) (stream s) (boolean print-final-semicolon))
		 (vars ((fixnum nforms (1- (length forms)))
			(fixnum curform 0))
		       (map nil (lambda (form)
				  (incf curform)
				  (if (= curform nforms)
				      (if print-final-semicolon
					  (progn
					    (output-c-helper form filename s)
					    (format s "; "))
					  (output-c-helper form filename s))
				      (progn
					(output-c-helper form filename s)
					(format s ", "))))
			    (cdr forms)))))
	   (destructuring-bind (for-form init-forms condition-forms update-forms &rest body) form
	     (declare (ignore for-form))
	     (format s "for(")
	     (if (atom (cadr init-forms))
		 (progn
		   (output-c-helper init-forms filename s)
		   (format s "; " ))
		 (output-multiple-forms init-forms s t))
	     (if (atom (cadr condition-forms))
		 (progn
		   (output-c-helper condition-forms filename s)
		   (format s "; " ))
		 (output-multiple-forms condition-forms s t))
	     (if (atom (cadr update-forms))
		 (progn
		   (output-c-helper update-forms filename s))
		 (output-multiple-forms update-forms s nil))
	     (format s ") {~%")
	     (mapcar (lambda (body-form)
		       (output-c-helper body-form filename s)
		       (format s ";~%" ))
		     body)
	     (format s "}"))))

(def t c-output-function-call ((list form) (simple-string filename) (stream s))
     (vars ((fixnum numargs (length (cdr form)))
	    (fixnum curarg 0))
	   (if (= numargs 0)
	       (format s "~A()" (car form))
	       (progn
		 (format s "~A(" (car form))
		 (mapcar (lambda (arg)
			   (incf curarg)
			   (if (= curarg numargs)
			       (progn
				 (output-c-helper arg filename s)
				 (format s ")"))
			       (progn
				 (output-c-helper arg filename s)
				 (format s ", "))))
			 (cdr form))))))
		 

(def t c-output-variable-decleration ((list form) (simple-string filename) (stream s))
;  (print form)
  (if (eq (second form) '|struct|)
      (progn
	(format s "struct ~A " (third form))
	(vars* ((list decls (cdddr form))
		(fixnum ndecls (length decls))
		(fixnum curdecl 0))
	       (mapcar (lambda (form)
			 (incf curdecl)
			 (output-c-helper form filename s)
			 (when (< curdecl ndecls)
			   (format s ", ")))
		       decls)))
      (vars* ((fixnum nvars (- (length form) 2))
	      (fixnum curvar 0))
	(format s "~A " (if (eq (first form) '|var|)
			    ""
			    (first form)))
	(output-c-type-helper (strip-lineno (second form) filename s)  filename s)
	(mapcar (lambda (var)
		  (incf curvar)
		  (if (= curvar nvars)
		      (output-c-type-helper (strip-lineno var filename s) filename s)
		      (progn
			(output-c-type-helper (strip-lineno var filename s) filename s)
			(format s ", " ))))
		(cddr form)))))

(def t c-output-if ((list form) (simple-string filename) (stream s))
  (destructuring-bind (if-atom condition &rest body) form
    (declare (ignore if-atom))
    (format s "if(")
    (output-c-helper condition filename s)
    (format s ") {")
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
			      (output-c-helper else-body-form filename s)
			      (format s ";~%"))
			    (cddr body-form)))
		  (progn
		    (output-c-helper body-form filename s)
		    (format s ";~%"))))
	    body)
    (format s "}")))
    

(def t c-output-infix-operator ((list form) (simple-string filename) (stream s))
  "output c code for infix operators, with special cases for urinary - and ~"
  (when (and (or (eq (car form) '*)( eq (car form) '**)
		 (eq (car form) '***) (eq (car form) '****))
	     (= (length form) 2))
    ;; special case pointer dereference (*... var)
    (format s "(~A" (first form))
    (output-c-helper (second form) filename s)
    (format s ")")
    (return-from c-output-infix-operator nil))
  ;;(when (and (< (length (rest form) 2) ; TODO: pass in filename and lineno to this function
  ;;           (not (eq (car form) '-))) ; special case for urinary -
  ;; (error "~A: ~A: infix operator ~A has less than 2 arguments" filename lineno (car form)
  (if (and (or (eq (car form) '-)
	       (eq (car form) '~))
	   (= (length (rest form)) 1))
      (progn
	(format s "(~A" (car form))
	(output-c-helper (second form) filename s)
	(format s ")"))
      (if (= (length (rest form)) 2)
	  (progn ;; infix with 2 args
	    (format s "((")
	    (output-c-helper (second form) filename s)
	    (format s ") ~A (" (car form))
	    (output-c-helper (third form) filename s)
	    (format s "))"))
	  ;; make infix operators more lispy ie. (+ 1 2 3 4) => (1 + 2 + 3 + 4)
	  (vars* ((symbol op (first form))
		  (t first-arg (second form))
		  (list rest-args (cddr form))
		  (fixnum num-rest-args (length rest-args))
		  (fixnum cur-rest-arg 0))
	    (format s "((")
	    (output-c-helper first-arg filename s)
	    (format s ")")
	    (output-c-helper op filename s)
	    (format s " ")
	    (mapcar (lambda (arg)
		      (incf cur-rest-arg)
		      (if (< cur-rest-arg num-rest-args)
			  (progn
			    (format s "(")
			    (output-c-helper arg filename s)
			    (format s ") ~A" (first form)))
			  (progn
			    (format s "(")
			    (output-c-helper arg filename s)
			    (format s "))"))))
		    (cddr form))))))

;; here
(def t c-output-equals ((list form) (simple-string filename) (stream s))
     ;; make = more lispy ie. (= x y z 0) => x = y = z = 0;
     (vars* ((t lhs (second form))
	     (list args (cddr form))
	     (fixnum n-args (length args))
	     (fixnum cur-arg 0))
       (format s "(")
       (output-c-helper lhs filename s)
       (format s ") = ")
       (mapcar (lambda (arg)
		 (incf cur-arg)
		 (if (and (listp arg)
			  (or (eq (cadr arg) '|,|)
			      (eq (cadr arg) '|{,}|)))
		     (if (= cur-arg n-args) ; special case for comma and {,} operator, no parens around rhs
			 (output-c-helper arg filename s)
			 (progn
			   (output-c-helper arg filename s)
			   (format s " = ")))
		     (if (= cur-arg n-args)
			 (progn
			   (format s "(")
			   (output-c-helper arg filename s)
			   (format s ")"))
			 (progn
			   (format s "(")
			   (output-c-helper arg filename s)
			   (format s ") = ")))))
	       args)))

(def t c-output-switch ((list form) (simple-string filename) (stream s))
  (destructuring-bind (switch-atom expression &body body) form
    (declare (ignore switch-atom))
    (format s "switch (")
    (output-c-helper expression filename s)
    (format s ") {~%")
    (mapcar (lambda (switch-case)
	      (if (and (listp (cadr switch-case))
		       (listp (cdadr switch-case)))
		  (if (eq (cadadr switch-case) 'quote) ; handle character constant expressions which start with (quote ...)
		      (progn
			(format s "case ")
			(output-c-helper (cadr switch-case) filename s)
			(format s ": "))
		      (mapcar (lambda (const-expr)
				(format s "case ")
				(output-c-helper const-expr filename s)
				(format s ": "))
			      (cdadr switch-case)))
		  (if (eq (cadr switch-case) '|default|)
		      (format s "default: ~%")
		      (progn
			(format s "case ")
			(output-c-helper (cadr switch-case) filename s)
			(format s ": "))))
	      (format s "{~%")
	      (mapcar (lambda (switch-case-expr)
			(output-c-helper switch-case-expr filename s)
			(format s ";~%"))
		      (cddr switch-case))
	      (format s "}~%"))
	    body)
    (format s "}~%")))
			       
(def t c-output-comma ((list form) (simple-string filename) (stream s))
  (vars ((fixnum numexpr (length (rest form)))
	 (fixnum curexpr 0))
    (mapcar (lambda (expr)
	      (incf curexpr)
	      (if (= curexpr numexpr)
		  (output-c-helper expr filename s)
		  (progn
		    (output-c-helper expr filename s)
		    (format s ", "))))
	    (rest form))))

(def t c-output-do-while ((list form) (simple-string filename) (stream s))
  (format s "do {~%")
  (mapcar (lambda (expr)
	    (if (and (listp expr)
		     (eq (cadr expr) '|while|))
		(progn
		  (format s "} while(")
		  (output-c-helper (third expr) filename s)
		  (format s ")"))
		(progn
		  (output-c-helper expr filename s)
		  (format s ";~%"))))
	  (cdr form)))
		      
(def t c-output-block-scope ((t form) (simple-string filename) (stream s))
  (format s "{~%")
  (mapcar (lambda (form)
	    (output-c-helper form filename s)
	    (format s ";~%"))
	  (rest form))
  (format s "~%}"))

(def t c-output-cast ((t form) (simple-string filename) (stream s))
  (format s "((")
  (output-c-type-helper (second form) filename s)
  (format s ")")
  (output-c-helper (third form) filename s)
  (format s ") "))

(def t c-output-array-reference ((t form) (simple-string filename) (stream s))
  (if (= (length form) 2)
      (progn
	(output-c-helper (second form) filename s)
	(format s "[]" ))
      (progn
	(format s "(")
	(output-c-helper (second form) filename s)
	(mapcar (lambda (form)
		  (format s "[")
		  (output-c-helper form filename s)
		  (format s "]"))
		(cddr form))
	(format s ")"))))

(def t c-output-character ((t form) (simple-string filename) (stream s))
     (declare (ignore filename))
     (if (eq '|space| (second form))
	 (format s "' '" )
	 (format s "'~A'" (second form))))

(def t c-output-bracketed-initialization ((t form) (simple-string filename) (stream s))
  (format s "{ ")
  (vars ((fixnum narg (length (rest form)))
	 (fixnum curarg 0))
    (mapcar (lambda (form)
	      (incf curarg)
	      (output-c-helper form filename s)
	      (if (= curarg narg)
		  (format s " ")
		  (format s ", ")))
	  (rest form)))
  (format s " }"))

(defun pair (l &optional a)
  (if l
      (pair (cddr l) (cons `(,(car l) . ,(cadr l)) a))
    (nreverse a)))

(def t c-output-struct ((list form) (simple-string filename) (stream s))
     (destructuring-bind (struct-form name &rest fields) form
       (declare (ignore struct-form))
       (format s "struct ~A {" name)
       (let ((pairs (pair fields)))
	 (dolist (pair pairs)
	   (output-c-type-helper (car pair) filename s)
	   (output-c-helper (cdr pair) filename s)
	   (format s "; ")))
       (format s "}")))
     
(def t output-c-type-helper ((t form) (simple-string filename) (stream s))
  "called by above functions when we are expecting a type decleration.
These can be of the form 'symbol (eg. char) or a list such as (unsigned char)"

  (when (and (listp form) (numberp (car form))) ;; hack, but fixes a bug
    (setq form (strip-lineno form filename s)))

  (if (listp form)
      (case (car form)
	((* ** *** ****) ; types with pointers need the *'s after the type, not before
	 (output-c-type-helper (second form) filename s)
	 (format s "~A" (first form)))
	([] ; there was a problem with declerations being surrounded by () so this needs to be handled specially
	 (if (>= (length form) 3)
	     (progn
	       (format s "~A"
		       (let ((string-output-stream (make-string-output-stream)))
			 (remove #\( (remove #\)
					     (progn (output-c-helper (second form) filename string-output-stream)
						    (get-output-stream-string string-output-stream))))))
	       (mapcar (lambda (form)
			 (format s "[")
			 (output-c-helper form filename s)
			 (format s "]"))
		       (cddr form)))
	     (format s "~A[]"
		     (let ((string-output-stream (make-string-output-stream)))
		       (remove #\( (remove #\)
					   (progn (output-c-helper (second form) filename string-output-stream)
						    (get-output-stream-string string-output-stream))))))))
	(=
	 (c-output-equals form filename s))
	(otherwise
	 (mapcar (lambda (symbol)
		   (output-c-helper symbol filename s))
		 form)))
      (format s "~A " form)))

(def t output-c-helper (((or list symbol string fixnum float) form) (simple-string filename) (stream s))
  (setf form (strip-lineno form filename s))
;  (format t "***'~A'~%" form)
     (if (listp form)
	 (if (and (>= (length form) 2) ; special case for (x ++) or (x --)
		  (or (eq (second form) '++)
		      (eq (second form) '--)))
	     (progn
	       (format s "(")
	       (output-c-helper (first form) filename s)
	       (format s "~A)"  (second form)))
	     (if (listp (car form)) ; variable decleration
		 (progn
		   (output-c-helper (car form) filename s)
		   (output-c-helper (second form) filename s))
		 (case (car form) ; keyword, operator or function (or macro)
		   (|struct|
		    (c-output-struct form filename s))
		   (=
		    (c-output-equals form filename s))
		   ((+= -= *= /= ^= ~= ==)
		    (output-c-helper (second form) filename s)
		    (format s "~A" (first form))
		    (output-c-helper (third form) filename s))
		   ((+ - * / % ^ ~ < > <= >= != &&  << >> |\|\|| |\|| ** *** ****) ; infix + special operators
		    (c-output-infix-operator form filename s))
		   (|&| ; and or address of operator
		    (if (= (length (rest form)) 1)
			(progn
			  (format s "(&(")
			  (output-c-helper (second form) filename s)
			  (format s "))"))
			(c-output-infix-operator form filename s)))
		   (|,| ; comma operator
		    (c-output-comma form filename s))
		   (|[]| ; array reference
		    (c-output-array-reference form filename s))
		   ({} ; block scoping
		    (c-output-block-scope form filename s))
		   (|{,}| ; bracketed inititalization
		    (c-output-bracketed-initialization form filename s))
		   (|if| ; if statement
		    (c-output-if form filename s))
		   (|switch| ; switch statement
		    (c-output-switch form filename s))
		   (|while| ;while statement
		    (c-output-while form filename s))
		   (|do| ; do/while statement
		    (c-output-do-while form filename s))
		   (|for| ; for statement
		    (c-output-for form filename s))
		   (|cast| ; special case of cast funcall operator (cast type var)
		    (c-output-cast form filename s))
		   ((|var| |auto| |static| |extern| |register|) ; variable decleration
		    (c-output-variable-decleration form filename s))
		   (|goto| ; goto statement
		    (format s "goto ~A" (second form)))
		   (|:| ; labels are precedded by a colon as a function call
		    (format s "~A:" (second form)))
		   (*?++ ; hack function for performing *ptr++
		    (format s "(*~A++)" (second form)))
		   (*?-- ; hack function for performing *ptr--
		    (format s "(*~A--)" (second form)))
		   ('quote ; characters are single quoted
		    (c-output-character form filename s))
		   (|unsigned| ; special case for unsigned int/char/short/long
		    (format s "unsigned ")
		    (output-c-type-helper (cadr form) filename s))
		   (|?| ; short form conditional
		    (destructuring-bind (op condition true-form false-form) form
		      (declare (ignore op))
		      (format s "(")
		      (output-c-helper condition filename s)
		      (format s " ? ")
		      (output-c-helper true-form filename s)
		      (format s " : ")
		      (output-c-helper false-form filename s)
		      (format s ")")))
		   ((|.| |->|)
		    (format s "~A~A~A" (second form) (first form) (third form)))
		   (|#define|
		    (format s "#define ")
		    (mapcar (lambda (form)
			      (output-c-helper form filename s))
			    (rest form))
		    (format s "~%"))
		   (|macro|
		    (print |in macro|)
		    (eval `(defmacro ,(car form) ,(cadr form) 
			     ,@(cddr form)))
		    (setf (gethash (car form) *macros*) t))
		   (otherwise ; function call
		    (if (gethash (car form) *macros*)
			(output-c-helper (macroexpand-1 (cdr form)))
		      (c-output-function-call form filename s))))))
	     (typecase form
	       (string (format s "\"~A\"" form))
	       (symbol (format s "~A " form))
	       (otherwise
		(error "sxc: found type other than string or symbol in code stream: '~A'" form)))))

(def t output-c ((list form) (simple-string filename) &optional (stream s *standard-input*))
  (case (cadr form)
    ((|#include| |#if| |#ifdef| |#else| |#endif|)
     (progn
       (setf form (strip-lineno form filename s))
       (format s "~A " (car form))
       (mapcar (lambda (x) (format s "~A " x)) (cdr form))
       (format s "~%")))
    (|#define|
     (output-c-helper form filename s))
    ((|var| |auto| |static| |extern| |register| |struct|)
     (output-c-helper form filename s)
     (format s ";~%"))
;     (format s "~A;~%" (output-c-helper form filename s)))
    (otherwise
     (setf form (strip-lineno form filename s))
     (handler-case ; try for function definition
	 (destructuring-bind (rettype name args &body body) form
	   ; found function definition
	   (setf args (strip-lineno args filename s))
	   (vars ((fixnum numvardecl (length args))
		  (fixnum curvardecl 0))
	     (output-c-helper rettype filename s)
	     (format s " ~A(" name)
	     ; print argument list
	     (if args 
		 (mapcar (lambda (vardecl)
			   (setf vardecl (strip-lineno vardecl filename s))
			   (incf curvardecl)
			   (if (= curvardecl numvardecl)
			       (if (listp vardecl)
				   (progn
				     (when (> (length vardecl) 2)
				       (emit-warning filename current-line
						     "function argument decleration with more than 2 elements~%"))
				     (if body
					 (progn
					   (output-c-type-helper (first vardecl) filename s)
					   (when (= (length vardecl) 2)
					     (output-c-helper (second vardecl) filename s))
					   (format s ") {~%"))
					 (progn
					   (if (eq (car vardecl) '[]) ; special case for (int getop (([] char)))
					       (output-c-type-helper vardecl filename s)
					       (progn
						 (output-c-type-helper (first vardecl) filename s)
						 (when (= (length vardecl) 2)
						   (output-c-helper (second vardecl) filename s))))
					   (format s ")~%"))))
				   (if body
				       (progn
					 (output-c-helper vardecl filename s)
					 (format s ") {" )) ; special case for (void)
				       (progn
					 (output-c-helper vardecl filename s)
					 (format s ")")))) ; special case for (void)
			       (progn
				 (output-c-helper (first vardecl) filename s)
				 (output-c-helper (second vardecl) filename s)
				 (format s ", "))))
			 args)
		 (if body
		     (format s ") {")
		     (format s ")~%")))
	     (if body
		 (progn
		   (mapcar (lambda (form)
			     (output-c-helper form filename s)
			     (format s ";~%"))
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

(def t main ()
     (vars (((list-of simple-string) files-to-process (cdr *posix-argv*)))
	   (dolist (filename files-to-process)
	     (vars ((list forms (sxc-read-file filename)))
		   ;;(print forms)
		   (handler-case 
		       (mapcar (lambda (form)
				 (output-c form filename *standard-output*))
			       forms)
					;	(sxc-read-file filename)))
		     (;;sb-int:simple-file-error (e)
		      error (e)
					       (declare (ignore e))
					       (format *error-output*
						       "~A: error: file not found.~%"
						       filename)))))))
