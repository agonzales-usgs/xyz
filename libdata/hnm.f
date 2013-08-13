c This subroutine generates Peterson's *new* high noise model. This
c model is "a spectrum of *average* high background noise power in the
c (global) network."
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
	subroutine hnm(p,resp,psddb)
	character*1 resp
	dimension pab(3,11)
	data pi/3.141592654/, pmin/0.10/, pmax/100000.00/

	data ((pab(i,j), i=1,3), j=1,11)/
     & 0.10,      -108.73,   -17.23,
     & 0.22,      -150.34,   -80.50,
     & 0.32,      -122.31,   -23.87,
     & 0.80,      -116.85,    32.51,
     & 3.80,      -108.48,    18.08,
     & 4.60,       -74.66,   -32.95,
     & 6.30,         0.66,  -127.18,
     & 7.90,       -93.37,   -22.42,
     & 15.40,       73.54,  -162.98,
     & 20.00,     -151.52,    10.01,
     & 354.80,    -206.66,    31.63/
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
       do 10 i=11,1,-1
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
