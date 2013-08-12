      subroutine spwwss(x,npts,dt,ierr)
c
c SPWWSS VBB 2 simulation of short-period world-wide from VBB: GAIN X 10
c
c Note the changes relative to the way these IIR coefficients are
c listed in a Quanterra:
c 
c    1) "a" and "b" notation is reversed 
c    2) the first "a" ("b" in quanterra notation) coefficient is dropped
c    3) the signs  of the "a" coeffs are reversed
c    4) we use Stearn's notation, indexing the "a" coeffs from 1, and
c       the b coeffs from 0
c
c input:
c   x()  = array containing input time series
c   npts = number of samples in x()
c   dt   = sample interval of time series, in seconds
c
c output:
c  x()  = filter time series
c  ierr = 0 no errors
c      != 0 errors occurred
c
c NOTES:
c   The spfilt subroutine supports block filtering (calling it
c   sequentially for contiguous segments of a time series) but I do not
c   implement this.
c
      dimension x(*)
      parameter (maxcof=2)
      parameter (dt0=0.05)
      parameter (na=2, nb=2, nc=2, nd=2)
      dimension a(na), b(0:nb), c(nc), d(0:nd), px(0:maxcof), py(maxcof)

c
c 2 rate=20, cut=0.95238, Butt, high pass
c
      data (b(i),i=0,nb)/8.091424120355e-00, -1.618284824071e+01, 
     &     8.091424120355e-00/
      data (a(i),i=1,na)/  -1.581520297586e+00, 6.550493505566e-01/
c
c  2 rate=20, cut=1.33333333, Butt, low pass
c
      data (d(i),i=0,nd)/ 3.357179459164e-02,  6.714358918328e-02, 
     &     3.357179459164e-02/
      data (c(i),i=1,nc)/ -1.418982792925e+00, 5.532699712920e-01/
c
c      
      ierr = 1
      if (dt.ne.dt0) then
        write(0,*) 'ERROR - in spwwss -- mismatched sample intervals'
        return
      endif
      
      do 10 i=1,maxcof
        px(i) = 0.0
 10     py(i) = 0.0
c
      call spfilt(b,a,nb,na,x,npts,px,py,ierr)
      if (ierr.ne.0) then
        write(0,*) 'ERROR - in spwwss, pass 1'
        return
      endif
c
      do 30 i=1,maxcof
        px(i) = 0.0
 30     py(i) = 0.0
      call spfilt(d,c,nd,nc,x,npts,px,py,ierr)
      if (ierr.ne.0) then
        write(0,*) 'ERROR - in spwwss, pass 2'
      endif
c      
      return
      end
c
c------------------------------------------------------------------
c
      subroutine lpwwss(x,npts,dt,ierr)
c
c LPWWSS VBB 2 simulation of long-period world-wide from VBB (20 sps).
c
c These coefficients were given to me directly from Bob Hutt, but his
c coefficient files seemed to use the same convention as the IIR
c coefficient files in the Quanterra.
c
c Bob gave me the following gains:
c  0.8711 at 0.05 Hz
c  0.966 at 0.02 Hz
c
c Note the changes relative to the way these IIR coefficients are
c listed in a Quanterra:
c 
c    1) "a" and "b" notation is reversed 
c    2) the first "a" ("b" in quanterra notation) coefficient is dropped
c    3) the signs  of the "a" coeffs are reversed
c    4) we use Stearn's notation, indexing the "a" coeffs from 1, and
c       the b coeffs from 0
c
c input:
c   x()  = array containing input time series
c   npts = number of samples in x()
c   dt   = sample interval of time series, in seconds
c
c output:
c  x()  = filter time series
c  ierr = 0 no errors
c      != 0 errors occurred
c
c NOTES:
c   The spfilt subroutine supports block filtering (calling it
c   sequentially for contiguous segments of a time series) but I do not
c   implement this.
c
      dimension x(*)
      parameter (maxcof=2)
      parameter (dt0=0.05)
      parameter (na=2, nb=2, nc=2, nd=2)
      dimension a(na), b(0:nb), c(nc), d(0:nd), px(0:maxcof), py(maxcof)

c
c 2 rate=20, high pass
c
      data (b(i),i=0,nb)/0.9977810241032E+00,
     &                   -.1995562048206E+01,
     &                   0.9977810241032E+00/
      data (a(i),i=1,na)/  -.1995557124346E+01,
     &                   0.9955669720664E+00/
c
c  2 rate=20, low pass
c
      data (d(i),i=0,nd)/0.1081653735993E-03,
     &                   0.2163307471986E-03,
     &                   0.1081653735993E-03/
      data (c(i),i=1,nc)/-.1970368091922E+01,
     &                 0.9708007534167E+00/
c
c      
      ierr = 1
      if (dt.ne.dt0) then
        write(0,*) 'ERROR - in lpwwss -- mismatched sample intervals'
        return
      endif
      
      do 10 i=1,maxcof
        px(i) = 0.0
 10     py(i) = 0.0
c
      call spfilt(b,a,nb,na,x,npts,px,py,ierr)
      if (ierr.ne.0) then
        write(0,*) 'ERROR - in lpwwss, pass 1'
        return
      endif
c
      do 30 i=1,maxcof
        px(i) = 0.0
 30     py(i) = 0.0
      call spfilt(d,c,nd,nc,x,npts,px,py,ierr)
      if (ierr.ne.0) then
        write(0,*) 'ERROR - in lpwwss, pass 2'
      endif
      
      do 40 i=1,npts
 40     x(i) = x(i) / 0.8711
c      
      return
      end
c
c------------------------------------------------------------------
c   
      subroutine lpsro(x,npts,dt,ierr)
c
c LPSRO LP 2 simulation of long-period SRO from LP
c
c Note the changes relative to the way these IIR coefficients are
c listed in a Quanterra:
c 
c    1) "a" and "b" notation is reversed 
c    2) the first "a" ("b" in quanterra notation) coefficient is dropped
c    3) the signs  of the "a" coeffs are reversed
c    4) we use Stearn's notation, indexing the "a" coeffs from 1, and
c       the b coeffs from 0
c
c input:
c   x()  = array containing input time series
c   npts = number of samples in x()
c   dt   = sample interval of time series, in seconds
c
c output:
c  x()  = filter time series
c  ierr = 0 no errors
c      != 0 errors occurred
c
c NOTES:
c   The spfilt subroutine supports block filtering (calling it
c   sequentially for contiguous segments of a time series) but I do not
c   implement this.
c
      dimension x(*)
      parameter (maxcof=6)
      parameter (dt0=1.0)
      parameter (na=2, nb=2, nc=6, nd=6)
      dimension a(na), b(0:nb), c(nc), d(0:nd), px(0:maxcof), py(maxcof)
c
c  2 rate=1, cut=0.02, Butt, high pass
c
      data (b(i),i=0,nb)/9.149717E-01, -1.829943E+00,  9.149717E-01/
      data (a(i),i=1,na)/-1.822696E+00, 8.371820E-01/
c
c  6 rate=1, cut=0.04, Butt, low pass
c
      data (d(i),i=0,nd)/ 2.497222E-06,  1.498333E-05,  3.745834E-05,
     &   4.994445E-05, 3.745834E-05,  1.498333E-05,  2.497222E-06/
      data (c(i),i=1,nc)/ -5.029436E+00, 1.060704E+01,  
     &   -1.199931E+01, 7.675468E+00,  -2.631054E+00, 3.774528E-01/   
      
      ierr = 1
      if (dt.ne.dt0) then
        write(0,*) 'ERROR - in lpsro -- mismatched sample intervals'
        return
      endif
      
      do 10 i=1,maxcof
        px(i) = 0.0
 10     py(i) = 0.0
c
      call spfilt(b,a,nb,na,x,npts,px,py,ierr)
      if (ierr.ne.0) then
        write(0,*) 'ERROR - in lpsro, pass 1'
        return
      endif
c
      do 30 i=1,maxcof
        px(i) = 0.0
 30     py(i) = 0.0
      call spfilt(d,c,nd,nc,x,npts,px,py,ierr)
      if (ierr.ne.0) then
        write(0,*) 'ERROR - in lpsro, pass 2'
      endif
c      
      return
      end
c
c------------------------------------------------------------------
c   
      subroutine spfilt(b,a,lb,la,x,n,px,py,ierror)
c IIR filter routine from Stearns and David (this is an 
c implementation of equation 6.3)
c 
c-latest date: 11/13/85
c-filters n-point data sequence in place using array x
c-transfer function coefficients are in arrays b and a
c-           b(0)+b(1)*z**(-1)+.......+b(lb)*z**(-lb)
c-    h(z) = ----------------------------------------
c-             1+a(1)*z**(-1)+.......+a(la)*z**(-la)
c-px saves past values of input x
c-py saves past values of output y
c-ierror=0    no errors detected
c-       1    filter response exceeds 1.e10
c
c NB:
c  zero out px and py before first call. If doing filtering
c by block (chunk), save px and py between calls.
c
      dimension x(0:n-1),b(0:lb),a(la),px(0:lb),py(la)
      ierror=1
      do 5 k=0,n-1
        px(0)=x(k)
        x(k)=0.
        do 1 l=0,lb
          x(k)=x(k)+b(l)*px(l)
    1   continue
        do 2 l=1,la
          x(k)=x(k)-a(l)*py(l)
    2   continue
        if(abs(x(k)).gt.1.e20) return
        do 3 l=lb,1,-1
          px(l)=px(l-1)
    3   continue
        do 4 l=la,2,-1
          py(l)=py(l-1)
    4   continue
        py(1)=x(k)
    5 continue
      ierror=0
      return
      end
c
c------------------------------------------------------------------
c  
      subroutine spnotch(x,npts,dt,ierr)
c
c SPWWSS VBB 2 simulation of short-period world-wide from VBB: GAIN X 10
c
c Note the changes relative to the way these IIR coefficients are
c listed in a Quanterra:
c 
c    1) "a" and "b" notation is reversed 
c    2) the first "a" ("b" in quanterra notation) coefficient is dropped
c    3) the signs  of the "a" coeffs are reversed
c    4) we use Stearn's notation, indexing the "a" coeffs from 1, and
c       the b coeffs from 0
c
c input:
c   x()  = array containing input time series
c   npts = number of samples in x()
c   dt   = sample interval of time series, in seconds
c
c output:
c  x()  = filter time series
c  ierr = 0 no errors
c      != 0 errors occurred
c
c NOTES:
c   The spfilt subroutine supports block filtering (calling it
c   sequentially for contiguous segments of a time series) but I do not
c   implement this.
c
      dimension x(*)
      parameter (maxcof=4)
      parameter (dt0=0.05)
      parameter (na=4, nb=4, nc=2, nd=2)
      dimension a(na), b(0:nb), c(nc), d(0:nd), px(0:maxcof), py(maxcof)

c
c BANDSTOP VBB 2 0.07 to 0.4Hz BS+0.05Hz HP, for 20 sps input
c
      data (b(i),i=0,nb)/0.9266608e+00, -3.701353e+00, 5.549392e+00, 
     &    -3.701353e+00, 0.9266608e+00/
      data (a(i),i=1,na)/-3.842444e+00,  5.544007e+00,-3.560262e+00,
     &     0.8587076e+00/
c
c  2 rate=20, cut=0.05, Butt, High Pass
c
      data (d(i),i=0,nd)/0.98895425e+00,-0.19779085e+01,0.98895425e+00/
      data (c(i),i=1,nc)/ -0.19777865e+01, 0.9780351e+00/
c
c      
      ierr = 1
      if (dt.ne.dt0) then
        write(0,*) 'ERROR - in spwwss -- mismatched sample intervals'
        return
      endif
      
      do 10 i=1,maxcof
        px(i) = 0.0
 10     py(i) = 0.0
c
      call spfilt(b,a,nb,na,x,npts,px,py,ierr)
      if (ierr.ne.0) then
        write(0,*) 'ERROR - in spnotch, pass 1'
        return
      endif
c
      do 30 i=1,maxcof
        px(i) = 0.0
 30     py(i) = 0.0
      call spfilt(d,c,nd,nc,x,npts,px,py,ierr)
      if (ierr.ne.0) then
        write(0,*) 'ERROR - in spnotch, pass 2'
      endif
c      
      return
      end
