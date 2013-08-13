c This subroutine generates Peterson's *new* low noise model. This low
c noise model "is a composite of station spectra obtained from many
c different instruments, vaults, geologic environments, and geographic
c regions. It is a hypothetical background spectrum that is unlikely to
c be duplicated at any single location on the Earth."
c 
c The complete reference for this noise model is:
c
c  Observations and Modeling of Seismic Background Noise, Jon Peterson,
c  Open-File Report 93-322, U.S. Department of Interior, U.S. Geological
c  Survey, Albuquerque, NM, USA, 1993, 
c
c subroutine arguments are as follows
c
c input:
c
c   p    = the period in seconds (real*4)
c   resp = determines the spectral density to be returned (char*1)
c	where: 'd' = displacement, m**2/Hz
c	       'v' = velocity, m**2/s**2/Hz
c	       'a' = acceleration, m**2/s**4/Hz
c output:
c
c   psddb = the power spectral density relative to unit spectral density
c
c calls:
c
c   calls no other routines
c
c
	subroutine lnm(p,resp,psddb)
	character*1 resp
	dimension pab(3,21)
	data pi/3.141592654/, pmin/0.10/, pmax/100000.00/

	data ((pab(i,j), i=1,3), j=1,16)/
     & 0.10,     -162.36,     5.64,
     & 0.17,     -166.70,     0.00,
     & 0.40,     -170.00,    -8.30,
     & 0.80,     -166.40,    28.90,
     & 1.24,     -168.60,    52.48,
     & 2.40,     -159.98,    29.81,
     & 4.30,     -141.10,     0.00,
     & 5.00,      -71.36,   -99.77,
     & 6.00,      -97.26,   -66.49,
     & 10.00,    -132.18,   -31.57,
     & 12.00,    -205.27,    36.16,
     & 15.60,     -37.65,  -104.33,
     & 21.90,    -114.37,   -47.10,
     & 31.60,    -160.58,   -16.28,
     & 45.00,    -187.50,     0.00,
     & 70.00,    -216.47,    15.70/

	data ((pab(i,j), i=1,3), j=17,21)/
     & 101.00,   -185.00,     0.00,
     & 154.00,   -168.34,    -7.61,
     & 328.00,   -217.43,    11.90,
     & 600.00,   -258.28,    26.60,
     & 10000.00, -346.88,    48.75/
c
c check that we are in range
c
	if (p.lt.pmin .or. p.gt.pmax) then
	  write(*,*) 'ERROR - period out of range'
	  psddb = 0.0
	  return
       endif
c
c compute psddb - step down through periods until we find the right one
c       
       do 10 i=21,1,-1
	 if (p .ge. pab(1,i)) then
           psddb = pab(2,i) + pab(3,i) * alog10( p )
           goto 100
         endif
 10    continue
c
c convert units
c 
 100   if(resp.eq.'v' .or. resp.eq.'V')  
     &     psddb = psddb + 20.0 * alog10(p/(2.0*pi))
	if(resp.eq.'d' .or. resp.eq.'D')  
     &     psddb = psddb + 20.0 * alog10( (p/(2.0*pi))**2 )
	return
	end
