c
c--------------------------------------------------------------------
c
      subroutine updat2(xn,jy,jd,jh,jm,ssj)
c INPUT:
c   common dhead$ containing the source time
c   xn = the travel time of the ray in seconds
c OUTPUT:
c   jy,jd,jh,jm,ssj = arrival time of the ray
c
      real*8 t,sec,c1,c2,c3,c4
      common/dheadX/d0,th,ph,iy,id,ih,im,ss
      data c1,c2,c3,c4/60.d0,24.d0,3600.d0,86400.d0/
      sec=ss
      t=((id*c2+ih)*c1+im)*c1+sec+xn
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
