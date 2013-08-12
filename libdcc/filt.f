      subroutine lpdes(fc,t,ns,a,b,c,graf)
c
c Low-pass Butterworth filter design routine. 
c
c Input:
c  fc = cutoff frequency in Hz, 
c  t  = sampling interval in seconds, 
c  ns = number of sections (two poles per section) 
c
c Output:
c  a(k), b(k). c(k) = filter coefficients (for k=1 thru ns), 
c  graf() = 10 pairs of frequency and power gain 
c            (graf(1,k) and graf(2,k) for k=1 thru 10)
c
c  Algorithm is from Stearns, 1975.
c 
      dimension a(*),b(*),c(*),graf(2,*)
      pi=3.1415926
      wcp=sin(fc*pi*t)/cos(fc*pi*t)
      do 120 k=1,ns
      cs=cos(float(2*(k+ns)-1)*pi/float(4*ns))
      x=1./(1.+wcp*wcp-2.*wcp*cs)
      a(k)=wcp*wcp*x
      b(k)=2.*(wcp*wcp-1.)*x
120   c(k)=(1.+wcp*wcp+2.*wcp*cs)*x

      do 130 k=1,10
      graf(2,k)=0.99-.98*float(k-1)/9.0
      x=atan(wcp*(1./graf(2,k)-1.)**(1./float(4*ns)))
130   graf(1,k)=x/(pi*t)
      return
      end
c
c-----------------------------------------------------------
c
	subroutine hpdes(fc,t,ns,a,b,c,graf)
c
c High-pass Butterworth filter design routine.
c See comments on lpdes
c
	dimension a(*),b(*),c(*),graf(2,*)
	pi=3.1415926535
	wcp=sin(fc*pi*t)/cos(fc*pi*t)
	do 120 k=1,ns
	cs=cos(float(2*(k+ns)-1)*pi/float(4*ns))
	a(k)=1./(1.+wcp*wcp-2.*wcp*cs)
	b(k)=2.*(wcp*wcp-1.)*a(k)
120	c(k)=(1.+wcp*wcp+2.*wcp*cs)*a(k)
	do 130 k=1,10
	graf(2,k)=.01+.98*float(k-1)/9.
	x=atan(wcp*(1./graf(2,k)-1.)**(-1./float(4*ns)))
130	graf(1,k)=x/(pi*t)
	return
	end
c
c-----------------------------------------------------------
c
      subroutine bpdes(f1,f2,t,ns,a,b,c,d,e,graf)
c
c Bandpass butterworth digital filter design subroutine
c
c Input:
c  f1, f2 = corner (3-db) frequencies in hz
c  t      = sampling interval in seconds.
c  ns     = number of sections (each section = 4 poles
c           2 low freq poles and 2 hi freq poles)
c
c Output:
c  a(k),b(k),c(k),d(k),e(k) = filter coefficients for each section, k  
c  graf(1,k) = 20 pairs of frequency and power gain,
c              (i.e. graf(1,k) and graf(2,k) for k = 1 thru 20)
c
c The digital filter has ns filter sections in the cascade. 
c The k'th section has the transfer function with the transfer
c function:
c
c                 a(k)*(z**4-2*z**2+1)
c      h(z) = -------------------------
c              z**4+b(k)*z**3+c(k)*z**2+d(k)*z+e(k)
c
c Thus, if f(m) and g(m) are the input and output at time m*t, then
c
c  g(m) = a(k)*(f(m)-2*f(m-2)+f(m-4))
c         -b(k)*g(m-1)-c(k)*g(m-2)-d(k)*g(m-3)-e(k)*g(m-4)
c
c
	dimension a(*),b(*),c(*),d(*),e(*)
	dimension graf(2,*)
	pi=3.14159265
	w1=sin(f1*pi*t)/cos(f1*pi*t)
	w2=sin(f2*pi*t)/cos(f2*pi*t)
	wc=w2-w1
	q=wc*wc+2.*w1*w2
	s=w1*w1*w2*w2
	do 150 k=1,ns
	cs=cos(float(2*(k+ns)-1)*pi/float(4*ns))
	p=-2.*wc*cs
	r=p*w1*w2
	x=1.+p+q+r+s
	a(k)=wc*wc/x
	b(k)=(-4.-2.*p+2.*r+4.*s)/x
	c(k)=(6.-2.*q+6.*s)/x
	d(k)=(-4.+2.*p-2.*r+4.*s)/x
150	e(k)=(1.-p+q-r+s)/x
	graf(2,1)=.0001
	graf(2,2)=.01
	graf(2,3)=.0316228
	graf(2,4)=.1
	graf(2,5)=.251189
	graf(2,6)=.5
	graf(2,7)=.794328
	graf(2,8)=.891251
	graf(2,9)=.954993
	graf(2,10)=.977237
	do 151 j=1,10
	k=21-j
	graf(2,k)=graf(2,j)
151	continue
	do 170 j=1,2
	do 160 i=1,10
	k=i*(2-j)+(21-i)*(j-1)
	x=(1./graf(2,k)-1.)**(1./float(4*ns))
	sq=sqrt(wc*wc*x*x+4.*w1*w2)
	graf(1,k)=abs(atan(.5*(wc*x+float(2*j-3)*sq)))/(pi*t)
160	continue
170	continue
	return
	end
c
c-----------------------------------------------------------
c
	subroutine butter_bp(data,npts,ns,a,b,c,d,e)
c
c Performs band-pass Butterworth filtering of a time series. 
c Must first call routine bpdes to get the filter coefficients.
c
c input:
c  data() = data array
c  npts   = number of samples in data array
c  ns     = number of filter sections (1 section = 2 poles)
c  a(), b(), c(),
c  d(), e()       = filter coefficients (from subroutine bpdes)
c
c output
c  data() = filtered data array
c                   
	dimension data(*),a(*),b(*),c(*),d(*),e(*)
	real*8 temp, f(6,5)

	if (ns.gt.5) stop 'ERROR - in butter bp'
c initialize	
	do 10 n=1,ns+1
	do 10 m=1,5
 10      f(n,m) = 0.d0
c
c loop 15  - over each sample in input series
c 
       do 15 i=1,npts
         f(1,5) = data(i)
c
c Go through ns filter sections.
c
	do 20 n=1,ns
	temp=a(n)*(f(n,5)-f(n,3)-f(n,3)+f(n,1))
	f(n+1,5)=temp-b(n)*f(n+1,4)-c(n)*f(n+1,3)-d(n)*f(n+1,2)-e(n)*f(n+1,1)
 20	continue
c
c Update all past values of signals.
c
	do 30 n=1,ns+1
	  f(n,1)=f(n,2)
	  f(n,2)=f(n,3)
	  f(n,3)=f(n,4)
 30	  f(n,4)=f(n,5)
 
 15    data(i) = f(ns+1,5)
c       
	return
	end
c
c-----------------------------------------------------------
c
	subroutine butter_lp(data,npts,ns,a,b,c)
c
c Performs low-pass Butterworth filtering of a time series. 
c Must first call routine lpdes to get the filter coefficients.
c
c input:
c  data() = data array
c  npts   = number of samples in data array
c  ns     = number of filter sections (1 section = 2 poles)
c  a(), b(), c() = filter coefficients (from subroutine lpdes)
c
c output
c  data() = filtered data array
c                   
	dimension data(*),a(*),b(*),c(*)
	real*8 temp, f(6,3)
c
	if (ns.gt.5) stop 'ERROR - in butter lp'
c initialize	
	do 10 n=1,ns+1
	do 10 m=1,3
 10      f(n,m) = 0.d0
c
c loop 15  - over each sample in input series
c 
       do 15 i=1,npts
         f(1,3) = data(i)
c
c Go through ns filter sections.
c
	  do 20 n=1,ns
	    temp = a(n) * (f(n,3) + f(n,2) + f(n,2) + f(n,1))
 20	    f(n+1,3)=temp - b(n)*f(n+1,2) - c(n)*f(n+1,1)
c
c Update all past values of signals.
c
	  do 30 n=1,ns+1
           f(n,1) = f(n,2)
 30        f(n,2) = f(n,3)
 
 15    data(i) = f(ns+1,3)
c       
	return
	end
c
c-----------------------------------------------------------
c
	subroutine butter_hp(data,npts,ns,a,b,c)
c
c Performs hi-pass Butterworth filtering of a time series. 
c Must first call routine hpdes to get the filter coefficients.
c
c input:
c  data() = data array
c  npts   = number of samples in data array
c  ns     = number of filter sections (1 section = 2 poles)
c  a(), b(), c() = filter coefficients (from subroutine lpdes)
c
c output
c  data() = filtered data array
c                   
	dimension data(*),a(*),b(*),c(*)
	real*8 temp, f(6,3)
c
	if (ns.gt.5) stop 'ERROR - in butter hp'
c initialize		
	do 10 n=1,ns+1
	do 10 m=1,3
 10      f(n,m) = 0.d0
c
c loop 15  - over each sample in input series
c 
        do 15 i=1,npts
          f(1,3) = data(i)
c
c Go through ns filter sections.
c
	   do 20 n=1,ns
	     temp = a(n) * (f(n,3) - f(n,2) - f(n,2) + f(n,1))
 20	     f(n+1,3)=temp - b(n)*f(n+1,2) - c(n)*f(n+1,1)
c
c Update all past values of signals.
c
	   do 30 n=1,ns+1
            f(n,1) = f(n,2)
 30         f(n,2) = f(n,3)
 
 15    data(i) = f(ns+1,3)
c       
	return
	end
