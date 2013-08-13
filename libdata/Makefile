#
# note use of "-e" for extended ling length
#
LIB= data
FC= gfortran
FFLAGS= -libmil -O3 -fcray-pointer -ffixed-line-length-none -std=legacy

.f.a:
	$(FC) -c  $(FFLAGS) $<
	ar rv $@ $*.o
	rm -f $*.o

MEMBERS = \
	lib$(LIB).a(atant.o) \
	lib$(LIB).a(bdf2gfs.o) \
	lib$(LIB).a(bdf2sac.o) \
	lib$(LIB).a(beachball.o) \
	lib$(LIB).a(cktime.o) \
	lib$(LIB).a(do_filt.o) \
	lib$(LIB).a(dotdat.o) \
	lib$(LIB).a(eigen.o) \
	lib$(LIB).a(eiinp.o) \
	lib$(LIB).a(eival1.o) \
	lib$(LIB).a(eqpar.o) \
	lib$(LIB).a(filt.o) \
	lib$(LIB).a(gfs2sac.o) \
	lib$(LIB).a(hnm.o) \
	lib$(LIB).a(iasptab.o) \
	lib$(LIB).a(iasptim.o) \
	lib$(LIB).a(iir_filt.o) \
	lib$(LIB).a(lnm.o) \
	lib$(LIB).a(momen1.o) \
	lib$(LIB).a(ppfp1.o) \
	lib$(LIB).a(readcmt.o) \
	lib$(LIB).a(sac2asc.o) \
	lib$(LIB).a(sac2bin.o) \
	lib$(LIB).a(sac2gfs2.o) \
	lib$(LIB).a(sac2gfs10.o) \
	lib$(LIB).a(sac2gfs3.o) \
	lib$(LIB).a(sac2gfs4.o) \
	lib$(LIB).a(sac2gfs4h.o) \
	lib$(LIB).a(sacq2gfs.o) \
	lib$(LIB).a(sac_merge.o) \
	lib$(LIB).a(screen_decim.o) \
	lib$(LIB).a(setname.o) \
	lib$(LIB).a(siftcmt.o) \
	lib$(LIB).a(wsach.o)

lib$(LIB).a:: $(MEMBERS)

clean: 
	rm *.o lib$(LIB).a
