c
c------------------------------------------------------------------
c
      subroutine updat(n,jy,jd,jh,jm,ssj)
c input:
c   n = number of points relative to time in common head$
c output:
c   jy,jd,jh,jm,ssj = time at point n
c
      real*8 t,sec,c1,c2,c3,c4,dt
      common/headX/nsta,nchn,iy,id,ih,im,ss,si,nscan,title
      character*80 title
      data c1,c2,c3,c4/60.d0,24.d0,3600.d0,86400.d0/
      sec=ss
      dt=si
      t=((id*c2+ih)*c1+im)*c1+sec+n*dt-dt
      jd=t/c4
      t=t-jd*c4
      jh=t/c3
      t=t-jh*c3
      jm=t/c1
      ssj=t-jm*c1
      jy=iy
    1 idpy=365
      if(jy-4*(jy/4).eq.0) idpy=366
      if(jd.le.idpy) return
      jy=jy+1
      jd=jd-idpy
      goto 1
      end
