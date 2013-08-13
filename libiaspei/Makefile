FC= gfortran
FFLAGS= -libmil
LFLAGS= -L$(LIBDIR)

.f.o:
	$(FC) $(FFLAGS) -c $*.f

.f.a:
	$(FC) $(FFLAGS) -c   $<
	ar rv $@ $*.o

libqc_iaspei.a: \
	libqc_iaspei.a(libtau.o) \
	libqc_iaspei.a(libbuland.o) 

libqc_iaspei.a(libtau.o): ttlim.inc

clean: 
	rm *.o libqc_iaspei.a
