      subroutine filwts(w,nw,fc,fl,db,nd,ifil)
c  makes convolution filter wts in w(1)...w(nw). fc is the corner
c  frequency,fl is frequency at which filter is 'db' db down
c  from fc. nd is the decimation factor. If ifil.le.0 wts for
c  a low pass filter are generated, if ifil.gt.0 wts for a high
c  pass filter are generated. If ifil.gt.0 nd is set to 1 as
c  decimation is not allowed when high-passing.
c  nw will be lt.300 unless limit on nmax is removed.
      implicit real*8(a-h,o-z)
      dimension w(1),p(8),xiq(8)
      data p,xiq/7.d0,9.d0,14.d0,20.d0,40.d0,60.d0,80.d0,100.d0,
     1 -1.d0,-.523d0,-.222d0,-.0223d0,.301d0,.58d0,.903d0,1.3d0/
      data pi/3.14159265358979d0/
      id=max0(nd,1)
      if(ifil.gt.0) id=1
      db0=dmax1(dmin1(db+p(1),p(8)),p(1))
      do 1 i=2,8
    1 if(db0.le.p(i)) goto 2
    2 j=i-1
      x=(db0-p(j))*(xiq(i)-xiq(j))/(p(i)-p(j))+xiq(j)
      iqfac=(10.d0**x)/(id*dabs(fc-fl))+1.d0/id+.99
      nmax=150/id
      iq=id*min0(iqfac,nmax)
      s=2.d0
      w(iq)=s
      iqm1=iq-1
      ff=pi*fc/id
      con=pi/iq
      do 10 i=2,iq
      f=ff*(i-1)
      j=iqm1+i
      w(j)=(1.d0+dcos(con*(i-1)))*dsin(f)/f
   10 s=w(j)+s
      s=1.d0/(2.d0*s-2.d0)
      nj=2*iq
      nw=nj-1
      do 20 i=iq,nw
      w(i)=s*w(i)
      j=nj-i
   20 w(j)=w(i)
      if(ifil.le.0) return
      ns=nw/2
      do 5 i=1,nw
    5 w(i)=-w(i)
      w(ns+1)=1.d0+w(ns+1)
      return
      end                     

