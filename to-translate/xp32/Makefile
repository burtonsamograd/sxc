TARGET = XP32.exe
TARGETDIR = .
SRCDIR = $(TARGETDIR)
INCDIR = $(TARGETDIR)

CC = gcc
LD = gcc

CFLAGS = -c -Wall -g -O4
LDFLAGS = -o$(TARGET) -O4
OBJS =	main.o 		\
		line.o		\
		screen.o	\
		goo.o		\
		font.o		\
		fbanim.o	\
		list.o		\
		window.o	\
		random.o	\
		one.o		\
		two.o		\
		three.o		\
		vector.o	\
		matrix.o	\
		camera.o	\
		terrain.o	\
		smoke.o		\
		fire.o

LIBS = -lm

$(TARGET): $(DEP) $(OBJS)
	$(LD) $(LDFLAGS) $(OBJS) $(ASMOBJS) $(LIBS)

dep: _depend
_depend:
	gcc -MM $(SRCDIR)/*.c > _depend

clean:
	-rm *.o _depend

-include _depend





