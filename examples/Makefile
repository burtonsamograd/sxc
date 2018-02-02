all: fire struct hash-djb2

fire: fire.sxc
	sxcc -o fire fire.sxc -framework GLUT -framework OpenGL

hash-djb2: hash-djb2.sxc
	sxcc -o hash-djb2 hash-djb2.sxc

struct: struct.sxc
	sxcc -o struct struct.sxc

clean: 
	rm -f fire struct hash-djb2