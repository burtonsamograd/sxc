all: sxc

sxc: sxc.lisp
	sbcl --no-userinit --load sxc.lisp --eval "(sb-ext:save-lisp-and-die \"sxc\" :toplevel 'main :executable t)"

fire: fire.sxc sxc
	./sxcc.sh fire.sxc -framework GLUT -framework OpenGL

test:
	./run-tests.sh

clean:
	rm -f sxc
