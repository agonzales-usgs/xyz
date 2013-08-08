FC= gfortran
FFLAGS= -O3 -libmil -ffixed-line-length-none
LIB=qc_lfsubs

.f.a:
	$(FC) -c $(FFLAGS) $<
	ar rv $@ $*.o

MEMBERS = \
	lib$(LIB).a(facdwn.o) \
	lib$(LIB).a(factor.o) \
	lib$(LIB).a(fft.o) \
	lib$(LIB).a(fftdp.o) \
	lib$(LIB).a(fftl.o) \
	lib$(LIB).a(fftldp.o) \
	lib$(LIB).a(filtsp.o) \
	lib$(LIB).a(filwts.o) \
	lib$(LIB).a(panlsp.o) \
	lib$(LIB).a(realtr.o) \
	lib$(LIB).a(rltrdp.o) \
	lib$(LIB).a(sroida.o) \
	lib$(LIB).a(allsta_new.o)

lib$(LIB).a:: $(MEMBERS)

clean:
	rm *.o lib$(LIB).a
