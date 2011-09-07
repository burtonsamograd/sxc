(deftype any () t)

(defun make-keyword (symbol) (read-from-string (concatenate 'string ":" (symbol-name symbol))))

;(defun is-type (s) (nth-value 1 (subtypep t s)))
(defun is-type (s)
  (declare (ignore s))
  t)

(defun generate-arg-names-types-and-declerations (fname args &key (caller-name "unknown"))
  "given a function name and argument list, generate the argument names,
   argument types and argument declerations and return them as 3 values"
  (let* (in-optional
	 in-key
	 in-body
	 (arg-names (mapcar (lambda (arg) ; generate list of argument names
			      (if (consp arg)
				  (let ((type (first arg))
					(name (second arg))
					(default (third arg)))
				    (if (or in-optional in-key)
					(if (typep (eval default) type)
					    (list name default)
					    (warn "~A of ~A: default value for ~A is not of the correct type: ~A"
						   caller-name fname name type))
					name))
				  (progn
				    (case arg
				      (&optional (setf in-optional t))
				      (&key (setf in-key t))
				      ((&body &rest) (setf in-body t))
				      (otherwise (error "~A of ~A: bad variable decleration in arglist: ~A"
							caller-name fname arg)))
				    arg)))
			    args))
	 (reset-in-vars (setf in-optional nil
			      in-key nil
			      in-body nil))
	 (arg-types (mapcar
		     (lambda (arg) ; generate list of argument types
		       (if in-body
			   (car arg)
			   (if (consp arg)
			       (let ((type (first arg))
				     (name (second arg)))
				 (if (is-type type)
				     (if in-key
					 (list (make-keyword name) type)
					 type)
				     (error "~A of ~A: unknown type name in variable decleration: ~A"
					    caller-name fname type)))
			       (case arg
				 (&optional arg)
				 (&key (progn (setf in-key t) arg))
				 ((&body &rest) (progn (setf in-body t) arg))
				 (otherwise
					 (error "~A of ~A: bad variable decleration in arglist: ~A"
						caller-name fname arg))))))
		     args))
	 (arg-declerations (remove nil ; generate list of argument declerations
				   (mapcar 
				    (lambda (name type)
				      (case name
					((&optional &key &body &rest) nil)
					(otherwise
					 (if (consp name)
					     (copy-list `(declare (type ,type ,(car name))))
					     (copy-list `(declare (type ,type ,name)))))))
				    arg-names arg-types))))
    (declare (ignore reset-in-vars))
    (values arg-names arg-types arg-declerations)))

;(declaim (ftype (function (list &optional list) fixnum) fun))
(defmacro def (rettype fname args &body body)
  "declare a function with a typed return value and arg list, like so:
   
    (def float fun ((float x) &optional (float y 1) &key (float z 2) &rest args) (apply * (+ x y z) args))

    *note: the above is actually invalid, but only gives an example of how to use all the parameters

  values after &optional are of the form: (type name default)
  values after &key are of the form: (type name default)

  all &optional and &key variables must have default values.
  "
  (let ((has-docstring (typep (car body) 'string)))
    (unless (is-type rettype)
      (error "def: return type is not a type: ~A" rettype))
    (multiple-value-bind (arg-names arg-types arg-declerations)
	(generate-arg-names-types-and-declerations fname args :caller-name 'def)
      `(progn
	 (declaim (ftype (function ,arg-types ,rettype) ,fname))
	 (defun ,fname ,arg-names
	   ,@(if has-docstring (list (car body)) nil)
	   ,@arg-declerations
	   ,@(if has-docstring (cdr body) body))))))

; Number Types
; System Class: float, integer, number, rational, real
; Type: bignum, bit, fixnum, short-float, single-float, double-float, long-float, signed-byte, unsigned-byte
  
(deftype list-of (type)
  "defines a type (list-of type) that can be used in type declerations"
  (labels ((elements-are-of-type (seq type)
	     (every (lambda (x) (typep x type)) seq)))
    (let ((predicate (gensym "list-of-type")))
      (setf (symbol-function predicate)
	    #'(lambda (seq) (elements-are-of-type seq type)) )
      `(and list (satisfies ,predicate)) )))

(defmacro ldef (decls &body body)
  "Typed local function definitions like def, but using labels"
  (let (labels-def)
    (dolist (decl decls)
      (destructuring-bind (rettype fname args &body decl-body) decl
	(let ((has-docstring (typep (car decl-body) 'string)))
	  (multiple-value-bind (arg-names arg-types arg-declerations)
	      (generate-arg-names-types-and-declerations fname args :caller-name 'ldef)
	    (push `(,fname ,arg-names
			   ,@(if has-docstring (car decl-body) nil)
			   (declare (ftype (function ,arg-types ,rettype) ,fname))
			   ,@arg-declerations
			   ,@(if has-docstring (cdr decl-body) decl-body))
		labels-def)))))
    `(labels ,(nreverse labels-def) ,@body)))
	      
(defmacro define-vars-macro (name let-form)
  "expand to vars/vars* macro definition"
  `(defmacro ,name (bindings &body body)
     (let ((var-list (mapcar (lambda (var)
			       (if (and (consp var)
					(= (length var) 3))
				   (let ((type (first var))
					 (name (second var))
					 (value (third var)))
				     (if (is-type type)
					 (progn 
					   ;(unless (typep value type) ; makes too much noise when using programmatically set values
					   ;  (warn "~A: variable: ~A value: ~A is not of correct type: ~A" ',name name value type))
					   (list name value))
					 (error "~A: not a type name: ~A" ',let-form type)))
				   (error (format nil "~A: invalid variable specification: ~A" ',name var))))
			     bindings))
	   (declerations (let (decls)
			   (dolist (var bindings)
			     (push `(declare (,(first var) ,(second var))) decls))
			   decls))
	   (let-binding ',let-form))
       `(,let-binding ,var-list
		      ,@declerations
		      ,@body))))
(define-vars-macro vars let)
(define-vars-macro vars* let*)

(defmacro for (init-args condition update-vars &body body)
  "C-like for looping macro, variables must be defined prior to use in this macro"
    (let ((processed-init-args
	   (let (tmp-list)
	     (if (consp (car init-args))
		 (dolist (init-arg init-args)
		   (setf tmp-list (append tmp-list init-arg)))
		 (setf tmp-list init-args))
	     tmp-list)))
    `(progn
       (setf ,@processed-init-args)
       (loop
	  ,@body
	  ,@(if (consp (car update-vars))
		update-vars
		(list update-vars))
	  (unless ,condition
	    (return))))))

