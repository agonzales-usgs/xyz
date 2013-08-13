LIB=gfs
FC= gfortran
CC= gcc
FCFLAGS= -O -fno-align-commons
CCFLAGS= 
gfs : gfs.o usr.o disk.o
	ar cr lib$(LIB).a gfs.o usr.o disk.o
	rm -f gfs.o usr.o disk.o
	

gfs.o : gfs.f
	$(FC) $(FCFLAGS) -c gfs.f

usr.o : usr.f
	$(FC) $(FCFLAGS) -c usr.f

disk.o : disk.c
	$(CC) $(CCFLAGS) -c disk.c

install:
	mv lib$(LIB).a ../libs

clean: 
	rm gfs.o usr.o disk.o lib$(LIB).a
