FC= gfortran
CC= gcc
FCFLAGS= -O -fno-align-commons
CCFLAGS= 
gfs : gfs.o usr.o disk.o
	ar cr libqc_gfs.a gfs.o usr.o disk.o

gfs.o : gfs.f
	$(FC) $(FCFLAGS) -c gfs.f

usr.o : usr.f
	$(FC)  $(FCFLAGS) -c usr.f

disk.o : disk.c
	$(CC)  $(CCFLAGS) -c disk.c

clean: 
	rm gfs.o usr.o disk.o libqc_gfs.a
