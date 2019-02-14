;;
;; meta-circular interpreter
;;
;; From paper.txt
;;
;; Todo:
;;
;; Implement or find implementation of dlet* (destructuring let*)
;; For some reason it's not in the code...must have been in some
;; fancy lisp Baker was used to.
;;

(defmacro free (x) `(setq ,x nil))	; just for testing...

(defmacro mif (be te ee)
  (let ((vs (car (freevars be))))
    `(if-function
       #'(lambda () ,be)
       #'(lambda ,vs ,te)
       #'(lambda ,vs ,ee))))

(defun if-function (be te ee)
  (dlet* (((pval . stuff) (funcall be)))
    (if pval (apply te stuff) (apply ee stuff))))

(defmacro mcond (&rest clauses)
  (dlet* ((((be . rest) . clauses) clauses))
    (if (eq be 't) `(progn ,@rest)
      `(mif ,be (progn ,@rest)
         (mcond ,@clauses)))))

(defun matom (x) `(,(atom x) ,x))

(defun meq (x y) `(,(eq x y) ,x))

(defun meq2 (x y) `(,(eq x y) ,x ,y))

(defun msymbolp (x) `(,(symbolp x) ,x))

(defun mnull (x) `(,(null x) ,x))

(defun mcopy (x)		; Pedagogical non-primitive copy function.
  (mif (matom x) `(,x ,@x)	; primitive is allowed to copy symbol refs.
    (dlet* (((carx . cdrx) x)
            ((ncar1 . ncar2) (mcopy carx))
            ((ncdr1 . ncdr2) (mcopy cdrx)))
      `((,ncar1 ,@ncdr1) . (,ncar2 ,@ncdr2)))))

(defun massoc (x e)		; return binding, else nil.
  (mif (mnull e) (progn (free x) `(nil ,@e))
    (dlet* ((((var . val) . reste) e))
      (mif (meq2 x var) (progn (free x) `((,var ,@val) ,@reste))
        (dlet* (((nbinding . nreste) (massoc x reste)))
          `(,nbinding . ((,var ,@val) ,@nreste)))))))

(defun mevprogn (xl e)
  (dlet* (((x . xl) xl))
    (mif (mnull xl) (progn (assert (null xl)) (meval x e))
      (dlet* (((xval . ne) (meval x e))
              ((xlval . nne) (mevprogn xl ne)))
        (free xval)
        `(,xlval ,@nne)))))

(defun meval (x e)
  (mcond
   ((msymbolp x) (dlet* ((((var . val) . ne) (massoc x e)))
                   (free var)
                   `(,val ,@ne)))
   ((matom x) `(,x ,@e))
   (t (dlet* ((fn . args) x))
        (mcond
         ((meq fn 'progn) (free fn) (mevprogn args e))
         ((meq fn 'function) (free fn)
          (dlet* (((lambda) args)
                  ((nlambda . lambda) (mcopy lambda))
                  ((fvars . bvars) (freevars `(function ,nlambda) nil nil))
                  ((e1 . e2) (split fvars e)))
            `((funarg ,lambda ,e1) ,@e2)))
         ((meq fn 'funcall) (free fn)
          (dlet* ((((nfn . nargs) . ne) (mevlis args e)))
            `(,(mapply nfn nargs) ,@ne)))
         (t (dlet* (((nargs . ne) (mevlis args e)))
              `(,(mapply (symbol-function fn) nargs) ,@ne))))))))

(defun mapply (fn args)
  (mif (matom fn) (apply fn args)
    (dlet* (((ffn . rfn) fn))
      (mcond
       ((meq ffn 'lambda) (free ffn)
        (dlet* (((bvlist . body) rfn)
                ((v . ne) (mevprogn body (mpairlis bvlist args nil))))
          (assert (null ne))
          v))
       ((meq ffn 'funarg) (free ffn)
        (dlet* ((((lambda bvlist . body) ce) rfn)
                ((v . ne) (mevprogn body (mpairlis bvlist args ce))))
          (free lambda) (assert (null ne))
          v))
       (t (error "mapply: bad fn ~S" fn))))))

(defun mpairlis (vars vals e)
  (mif (mnull vars) (progn (assert (null vals)) e)
    (dlet* (((var . vars) vars)
            ((val . vals) vals))
      `((,var ,@val) ,@(mpairlis vars vals e)))))

(defun mevlis (args e)
  (mif (mnull args) (progn (assert (null args)) `(nil ,@e))
    (dlet* (((x . args) args)
            ((xval . e) (meval x e))
            ((argvals . e) (mevlis args e)))
      `((,xval ,@argvals) ,@e))))

(defun split (vars e)		; split env. into 2 segments.
  (mif (mnull vars) (progn (assert (null vars)) `(nil ,@e))
    (dlet* (((var . nvars) vars)
            ((binding . ne) (massoc var e))
            ((e1 . e2) (split nvars ne)))
      `((,binding ,@e1) ,@e2))))

(defun mmember (x ls)		; return truth value & rest of list.
  (mif (mnull ls) (progn (free x) `(nil ,@ls))
    (dlet* (((carls . ls) ls))
      (mif (meq2 x carls) (progn (free x) `(,carls ,@ls))
        (dlet* (((tval . rest) (mmember x ls)))
          `(,tval . (,carls ,@rest)))))))

(defun freevars (x bvars fvars)	; return new fvars and new bvars.
  (mcond
   ((msymbolp x)
    (dlet* (((x1 . x2) (mcopy x))
            ((x2 . x3) (mcopy x2))
            ((p1val . nbvars) (mmember x1 bvars))
            ((p2val . nfvars) (mmember x2 fvars)))
      (mif p1val (progn (free x3) `(,nfvars ,@nbvars))
        (mif p2val (progn (free x3) `(,nfvars ,@nbvars))
          `((,x ,@nfvars) ,@nbvars)))))
   ((matom x) (free x) `(,fvars ,@bvars))
   (t (dlet* (((fn . args) x))
        (mcond
         ((meq fn 'function) (free fn)
          (dlet* ((((lambda bvlist . body)) args))
            (free lambda)
            (freelistvars body `(,@bvlist ,@bvars) fvars)))
         (t (freelistvars `(,fn ,@args) bvars fvars)))))))

(defun freelistvars (xl bvars fvars)
  (mif (mnull xl) (progn (assert (null xl)) `(,fvars ,@bvars))
       (dlet* (((x . xl) xl)
               ((nfvars . nbvars) (freelistvars xl bvars fvars)))
	      (freevars x nbvars nfvars))))
