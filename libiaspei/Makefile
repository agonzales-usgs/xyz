LIB= iaspei
FC= gfortran
FFLAGS= -libmil
LFLAGS= -L$(LIBDIR)

.f.o:
	$(FC) $(FFLAGS) -c $*.f

.f.a:
	$(FC) $(FFLAGS) -c   $<
	ar rv $@ $*.o
	rm -f $*.o
	mv lib$(LIB).a ../libs

lib$(LIB).a: \
	lib$(LIB).a(libtau.o) \
	lib$(LIB).a(libbuland.o) 

lib$(LIB).a(libtau.o): ttlim.inc

clean: 
	rm *.o lib$(LIB).a
