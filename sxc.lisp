#!/usr/local/bin/sbcl --script

;(defpackage :sxc
;  (:use :cl :sb-ext))
;(in-package :sxc)


(load "typed-cl.lisp")

(def simple-string read-whole-file ((simple-string filename))
  (vars ((simple-string contents "")
	 (simple-string line ""))
    (with-open-file (file filename)
      (handler-case
	  (for (line (read-line file)) t (setf line (read-line file))
	    (setf contents (concatenate 'simple-string contents line (format nil "~A" #\Newline))))
	(end-of-file (e)
	    (return-from read-whole-file contents))))))

; disable # as macro character so we can handle (#include ...) and such
(defparameter sxc-read-table (copy-readtable nil))
(set-macro-character #\# (lambda (stream closec)
			   (declare (ignore stream closec))
			   #\#)
		     nil sxc-read-table)
(set-macro-character #\\ (lambda (stream closec)
			    (declare (ignore stream closec))
			    (format nil "\\~A" (read-char stream)))
		     nil sxc-read-table)
(set-macro-character #\: (lambda (stream closec)
			   (declare (ignore stream closec))
			   '|:|)
		     nil sxc-read-table)
(set-macro-character #\| (lambda (stream closec)
			   (declare (ignore closec))
			   (let ((c (peek-char nil stream)))
			     (if (char= c #\|)
				 (progn
				   (read-char stream)
				   '|\|\||)
				 '|\||)))
		     nil sxc-read-table)
(set-macro-character #\' (lambda (stream closec)
			   (declare (ignore closec))
			   (let ((c (peek-char nil stream)))
			     (if (char= c #\ )
				 (progn
				   (read-char stream)
				   '(quote | |))
				 `(quote ,(read stream)))))
		     nil sxc-read-table)

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

(def simple-string c-output-while ((list form))
     (destructuring-bind (while-form condition &rest body) form
       (with-output-to-string (s)
	 (format s "while (~A) {" (output-c-helper condition))
	 (mapcar (lambda (expr)
		   (format s "~A;~%" (output-c-helper expr)))
		 body)
	 (format s "}")
	 s)))

(def simple-string c-output-for ((list form))
     (ldef ((nil output-multiple-forms ((list forms) (stream s) (boolean print-final-semicolon))
		 (vars ((fixnum nforms (length forms))
			(fixnum curform 0))
		       (map nil (lambda (form)
				  (incf curform)
				  (if (= curform nforms)
				      (if print-final-semicolon
					  (format s "~A; " (output-c-helper form))
					  (format s "~A" (output-c-helper form)))
				      (format s "~A, " (output-c-helper form))))
			    forms))))
	   (destructuring-bind (for-form init-forms condition-forms update-forms &rest body) form
	     (with-output-to-string (s)
	       (format s "for(")
	       (if (atom (car init-forms))
		   (format s "~A; " (output-c-helper init-forms))
		   (output-multiple-forms init-forms s t))
	       (if (atom (car condition-forms))
		   (format s "~A; " (output-c-helper condition-forms))
		   (output-multiple-forms condition-forms s t))
	       (if (atom (car update-forms))
		   (format s "~A " (output-c-helper update-forms))
		   (output-multiple-forms update-forms s nil))
	       (format s ") {~%")
	       (mapcar (lambda (body-form)
			 (format s "~A;~%" (output-c-helper body-form)))
		       body)
	       (format s "}")
	       s))))

(def simple-string c-output-function-call ((list form))
     (vars ((fixnum numargs (length (cdr form)))
	    (fixnum curarg 0))
	   (if (= numargs 0)
	       (format nil "~A()" (car form))
	       (with-output-to-string (s)
		 (format s "~A(" (car form))
		 (mapcar (lambda (arg)
			   (incf curarg)
			   (if (= curarg numargs)
			       (format s "~A)" (output-c-helper arg))
			       (format s "~A, " (output-c-helper arg))))
			 (cdr form))
		 s))))

(def simple-string c-output-variable-decleration ((list form))
     (with-output-to-string (s)
       (vars* ((fixnum nvars (- (length form) 2))
	       (fixnum curvar 0))
	      (format s "~A ~A " (if (eq (first form) '|var|)
				     ""
				     (first form))
		      (output-c-helper (second form)))
	      (mapcar (lambda (var)
			(incf curvar)
			(if (= curvar nvars)
			    (format s "~A" (output-c-helper var))
			    (format s "~A, " (output-c-helper var))))
		      (cddr form))
	      s)))

(def simple-string c-output-if ((list form))
     (with-output-to-string (s)
       (destructuring-bind (if-atom condition &rest body) form
	 (format s "if(~A) {" (output-c-helper condition))
	 (mapcar (lambda (body-form)
		   (if (eq (car body-form) '|else|)
		       ;; else forms are inside the if body; solves dangling else problem
		       ;; but that means you can't use a function named else in the body of 
		       ;; an if statement (which you can't do in C anyways)
		       (progn
			 (format s "} else {~%")
			 (mapcar (lambda (else-body-form)
				   (format s "~A;~%" (output-c-helper else-body-form)))
				 (cdr body-form)))
		       (format s "~A;~%" (output-c-helper body-form))))
		 body)
	 (format s "}")
	 s)))

(def simple-string c-output-infix-operator ((list form))
     "output c code for infix operators, with special cases for urinary - and ~"
     (when (and (or (eq (car form) '*)( eq (car form) '**)
		    (eq (car form) '***) (eq (car form) '****))
		(= (length form) 2))
       ;; special case pointer dereference (*... var)
       (return-from c-output-infix-operator (format nil "(~A~A)" (first form) (output-c-helper (second form)))))
     ;;(when (and (< (length (rest form) 2) ; TODO: pass in filename and lineno to this function
     ;;           (not (eq (car form) '-))) ; special case for urinary -
     ;; (error "~A: ~A: infix operator ~A has less than 2 arguments" filename lineno (car form)
     (if (and (or (eq (car form) '-)
		  (eq (car form) '~))
	      (= (length (rest form)) 1))
	 (format nil "(~A~A)" (car form) (output-c-helper (second form)))
	 (if (= (length (rest form)) 2)
	     (format nil "((~A) ~A (~A))" ;; infix with 2 args
		     (output-c-helper (second form))
		     (car form)
		     (output-c-helper (third form)))
	     (with-output-to-string (s) ;; make infix operators more lispy ie. (+ 1 2 3 4) => (1 + 2 + 3 + 4)
	       (vars* ((symbol op (first form))
		       (t first-arg (second form))
		       (list rest-args (cddr form))
		       (fixnum num-rest-args (length rest-args))
		       (fixnum cur-rest-arg 0))
		      (format s "((~A) ~A " (output-c-helper (second form)) (first form))
		      (mapcar (lambda (arg)
				(incf cur-rest-arg)
				(if (< cur-rest-arg num-rest-args)
				    (format s "(~A) ~A " (output-c-helper arg) (first form))
				    (format s "(~A)) " (output-c-helper arg) (first form))))
			      (cddr form))
		      s)))))

(def simple-string c-output-equals ((list form))
     ;; make = more lispy ie. (= x y z 0) => x = y = z = 0;
     (vars* ((t lhs (second form))
	     (list args (cddr form))
	     (fixnum n-args (length args))
	     (fixnum cur-arg 0))
	    (with-output-to-string (s)
	      (format s "(~A) = " (output-c-helper lhs))
	      (mapcar (lambda (arg)
			(incf cur-arg)
			(if (= cur-arg n-args)
			    (format s "(~A)" (output-c-helper arg))
			    (format s "(~A) = " (output-c-helper arg))))
		      args)
	      s)))

(def simple-string output-c-helper (((or list symbol string fixnum float) form))
;  (format t "***'~A' ~A~%" form is-type-or-var-decl)
     (if (listp form)
	 (if (listp (car form)) ; variable decleration
	     (format nil "~A ~A" (output-c-helper (car form)) (output-c-helper (second form)))
	     (case (car form) ; keyword, operator or function (or macro)
	       (=
		(c-output-equals form))
	       ((=+ =- =* =/ =^ =~)
		(format nil "~A ~A ~A"
			(output-c-helper (second form))
			(car form)
			(output-c-helper (third form))))
	       ((+ - * / ^ ~ == < > <= >= != && |\|\|| |\|| |&| ** *** ****) ; infix + special operators
		(c-output-infix-operator form))
	       (|[]| ; array reference
		(if (= (length form) 2)
		    (format nil "~A[]" (output-c-helper (second form)))
		    (format nil "(~A[~A])" (output-c-helper (second form)) (output-c-helper (third form)))))
	       (|if| ; if statement
		(c-output-if form))
	       (|while| ;while statement
		(c-output-while form))
	       (|for| ; for statement
		(c-output-for form))
	       (|cast| ; special case of cast funcall operator (cast type var)
		(format nil "((~A)~A)" (output-c-helper (second form)) (output-c-helper (third form))))
	       ((|var| |auto| |static| |extern|) ; variable decleration
		(c-output-variable-decleration form))
	       (|goto| ; goto statement
		(format nil "goto ~A" (second form)))
	       (|:| ; labels are precedded by a colon as a function call
		(format nil "~A:" (second form)))
	       ('quote ; characters are single quoted, using double backslashes when required
		(if (eq '|space| (second form))
		    (format nil "' '" )
		    (format nil "'~A'" (second form))))
	       (otherwise ; function call or post-increment/decriment
		(if (or (eq (second form) '++)
			(eq (second form) '--))
		    (format nil "(~A)~A" (output-c-helper (first form)) (second form))
		    (c-output-function-call form)))))
	 (typecase form
	   (string (format nil "\"~A\"" form))
	   (fixnum (format nil "~A" form))
	   (float (format nil "~A" form))
	   (symbol (format nil "~A" form)))))

(def t output-c ((list form) (simple-string filename) (fixnum line) &optional (stream s *standard-input*))
  (case (car form)
    (#\# (progn
	   (format s "~A" #\#)
	   (mapcar (lambda (x) (format s "~A " x)) (cdr form))
	   (format s "~%")))
    ((|var| |static| |extern|)
     (format s "~A;~%" (output-c-helper form)))
    (otherwise
     (handler-case ; try for function definition
	 (destructuring-bind (rettype name args &body body) form
	   ; found function definition
	   (vars ((fixnum numvardecl (length args))
		  (fixnum curvardecl 0))
	     (format s "~A ~A(" (output-c-helper rettype) name)
	     ; print argument list
	     (if args
		 (mapcar (lambda (vardecl)
			   (incf curvardecl)
			   (if (= curvardecl numvardecl)
			       (if (listp vardecl)
				   (if body
				       (format s "~A ~A) {~%" (output-c-helper (first vardecl)) (output-c-helper (second vardecl)))
				       (format s "~A ~A)~%" (output-c-helper (first vardecl)) (output-c-helper (second vardecl))))
				   (if body
				       (format s "~A) {" (output-c-helper vardecl)) ; special case for (void)
				       (format s "~A)" (output-c-helper vardecl)))) ; special case for (void)
			       (format s "~A ~A, " (output-c-helper (first vardecl)) (output-c-helper (second vardecl)))))
			 args)
		 (if body
		     (format s ") {")
		     (format s ")~%")))
	     (if body
		 (progn
		   (mapcar (lambda (form)
			     (format s "~A;~%" (output-c-helper form)))
			   body)
		   (format s "~%}~%"))
		 (format s ";~%"))))
       (error (e)
	 (format s "~A;~%" (output-c-helper form)))))))


     

(def t main ()
  (vars (((list-of simple-string) files-to-process (cdr *posix-argv*)))
    (dolist (filename files-to-process)
      (multiple-value-bind (form curpos)
	  (vars ((simple-string contents (read-whole-file filename))
		 (fixnum curpos 0)
		 (fixnum i 0)
		 (fixnum curline 0))
	    (let ((*readtable* sxc-read-table))
	      (loop
		 (let ((saved-readtable-case (readtable-case *readtable*)))
		   (setf (readtable-case *readtable*) :preserve)
		   (HANDLER-CASE 
		       (MULTIPLE-VALUE-BIND (FORM POS)
			   (READ-FROM-STRING CONTENTS NIL NIL :START CURPOS)
			 (SETF (READTABLE-CASE *READTABLE*) SAVED-READTABLE-CASE)
			 (when (not form)
			   (return))
			 (for (i curpos) (< i pos) (incf i)
			      (when (char= (char contents i) #\Newline)
				(incf curline)))
			 (setf curpos pos)
			 (output-c form filename curline *standard-output*))
		     (END-OF-FILE (E)
		       (FORMAT *ERROR-OUTPUT* "~S: end of file while reading form at or after line ~A~%" FILENAME CURLINE)
		       (RETURN-FROM  MAIN NIL)))))))))))
			 

(main)