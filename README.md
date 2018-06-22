EXAMPLE:

    $ ./sxcc.sh hello.sxc
    $ ./a.out            
    Hello, World!
    $ make fire # TODO: make fire come out
    $ C-c # Control-C to get your terminal back

sxc - S-Expression C
--------------------

sxc is an an s-expression based language source transpiler for C.  By
providing an S-expression based syntax, sxc will attempt to add 
Lisp-like macro and code generation capabilities to the C language (WIP).

Motivation
----------

Lisp as a language uses lists as data structures for it's code
represenation and is a language designed to work with lists.  This
combination has allowed for the creation of 'macros', or programs that
take an input program, analyze it and then transform or generate
arbitrary code from it.  This gives great power to the programmer to
create new abstraction and control structures that are not available
in the original language.

sxc makes C a programmable language, just like a Lisp, with a the
mental programming model of C.

Features
--------

    - transpilation from .sxc to .c sources
    - macros
    - source line gdb debugging of original sources (currently broken)


Building
--------

Install sbcl from http://sbcl.org.

Type make.  Output will be in ./sxc.

Running
-------

Use the wrapper scripts ./sxc.sh and ./sxcc.sh to compile and build an
executable from one or more .sxc files:

	   ./sxcc.sh main.sxc lib.sxc

The result goes into ./a.out.

Examples
--------

The following is a basic "Hello World" program:

    ;;
    ;; hello.sxc
    ;;
    
    (#include <stdio.h>)
    
    (int main ((int argc) (char (** argv)))
         (printf "Hello, World!\n")
    
         (return 0))

This can be compiled and run using the following:

    ./sxcc.sh -o hello hello.sxc && ./hello

Future Design
--

A more complex example is adding a new control structure to the
language, like a 'string switch', which is like a standard C switch
but works with strings as arguments:

    ;;
    ;; sswitch - string switch
    ;;
    ;; a switch statement that works with strings
    ;;
    (macro sswitch (val &body cases)
           (labels ((genswitch (cases)
    		  (let* ((case (car cases))
    			 (cmd (first case))
    			 (val (second case))
    			 (statements (cddr case))
    			 condition)
    		    (when case
    		      (unless (listp val)
    			(setf val (list val)))
    		      (mapcar (lambda (v)
    				(setf condition (append conditiion `((strcmp ,v ,val) &&))))
    			      val)
    		      (setf condition (append condition '(1)))
    		      `(if ,condition
    			   ,@statements
    			   (else
    			    ,@(genswitch (cdr cases))))))))
           (genswitch cases))
    		     
    
    (int main ((int argc) (char (** argv)))
         (sswitch ([] argv 1)
    	      (case ("a" "c")
    		(printf "The value is \"a\" or \"c\"\n"))
    	      (case "d"
    		(goto e-label))
    	      (case "b"
    		(printf "The value is \"b\"\n"))
    	      (case "e"
    		(: e-label)
    		(printf "The value is \"d\" or \"e\"\n"))
    	      (default 
    		  (printf "The value is neither \"a\", \"b\", \"c\", \"d\", or \"e\"\n")))
         (return 0))

Syntax
------

SEE ./tests/ FOR MORE SYNTAX EXAMPLES.

--

The basic syntax of sxc follows that of the Lisp family of languages.

The first argument to an s-expression is the function or keyword to be
called or used, followed by it's arguments.

There are a some exceptions for convenience:

(*?++ ...)	- post increment
(++?* ...)	- pre increment

(*?-- ...)	- post decrement
(--?* ...)	- pre decrement

Utilities
---------

A wrapper script 'sxcc' is provided to simplify building and
compilation of program sources into an executable.

TODO:
--

You can find some things to translate in to SXC in the to-translate/ directory.

--
Burton Samograd
kruhft@icloud.com
2018
