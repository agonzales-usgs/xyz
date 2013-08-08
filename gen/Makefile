# GTM Nov. 24, 2003 - removed source_ram
LIB=qc_gen
FC= gfortran
FFLAGS= -O3 -libmil -std=legacy

.f.a:
	$(FC) -c  $(FFLAGS) $<
	ar rv $@ $*.o

MEMBERS = \
	lib$(LIB).a(calmo.o) \
	lib$(LIB).a(colat.o) \
	lib$(LIB).a(colon.o) \
	lib$(LIB).a(getstat.o) \
	lib$(LIB).a(shell.o) \
	lib$(LIB).a(geoc4.o) \
	lib$(LIB).a(geog4.o) \
	lib$(LIB).a(gname.o) \
	lib$(LIB).a(givloc.o) \
	lib$(LIB).a(numsubs.o) \
	lib$(LIB).a(demean.o) \
	lib$(LIB).a(minmax.o) \
	lib$(LIB).a(rmblnk.o) \
	lib$(LIB).a(rmnull.o) \
	lib$(LIB).a(stadis.o) \
	lib$(LIB).a(upcase.o) \
	lib$(LIB).a(ipoch.o) \
	lib$(LIB).a(ipoch8.o) \
	lib$(LIB).a(dat2ep.o) \
	lib$(LIB).a(ep2dat.o) \
	lib$(LIB).a(bipoch8.o) \
	lib$(LIB).a(caljul.o) \
	lib$(LIB).a(julcal.o) \
	lib$(LIB).a(strcmp.o) \
	lib$(LIB).a(strloc.o) \
	lib$(LIB).a(updat.o) \
	lib$(LIB).a(updat2.o) \
	lib$(LIB).a(update.o) \
	lib$(LIB).a(timer.o)

lib$(LIB).a:: $(MEMBERS)

clean:
	rm *.o lib$(LIB).a
