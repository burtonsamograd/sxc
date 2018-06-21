all: sxc

sxc: main.lisp
	sbcl --no-userinit --load main.lisp --eval "(sb-ext:save-lisp-and-die \"sxc\" :toplevel 'main :executable t)"
	# echo "(load \"main.lisp\")(system::save \"./sxc\")" | gcl # DFW
	# echo "(load \"main.lisp\")(ext:saveinitmem \"./sxc.clisp\" :init-function #'sxc::main :executable t)" | clisp # DFW

fire: fire.sxc sxc
	./sxcc.sh fire.sxc -framework GLUT -framework OpenGL

test:
	./run-tests.sh

clean:
	rm -f sxc && ./rd
