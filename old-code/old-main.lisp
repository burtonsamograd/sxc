(def t main ()
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
