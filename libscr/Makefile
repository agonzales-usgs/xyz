FC= gfortran
CC= gcc
FFLAGS= -O
CFLAGS= -O
LIB= scr
lib$(LIB) : scr1.o leolib.o psscreendump.o
	ar cr lib$(LIB).a scr1.o leolib.o psscreendump.o
	rm -f *.o 
	mv lib$(LIB).a ../libs

scr1.o : scr1.f
	$(FC) -c scr1.f

leolib.o : leolib.c
	$(CC) -c leolib.c -I -lX11 -lm

psscreendump.o : psscreendump.f
	$(FC) -c psscreendump.f

clean: 
	rm *.o libscr.a
