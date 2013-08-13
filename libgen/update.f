      subroutine update(n)
c updates header in myhed when start point of data is changed to n
c i.e. n=1 has no effect, n>1 makes start time later, n<1 makes
c start time earlier.
      real*8 t,ss,c1,c2,c3,c4,si
      common/myhed/nscan,nsta,nchn,ntyp,iy,id,ih,im,sec,dt,spare(20)
      data c1,c2,c3,c4/60.d0,24.d0,3600.d0,86400.d0/
      if(n.eq.1) return
      
      nscan = nscan - n
      
      ss=sec
      si=dt
      t=((id*c2+ih)*c1+im)*c1+ss+(n-1)*si
    5 if(t.gt.0) goto 10
      iy=iy-1
      idpy=365
      if(iy-4*(iy/4).eq.0) idpy=366
      t=t+idpy*c4
      goto 5
   10 id=t/c4
      t=t-id*c4
      ih=t/c3
      t=t-ih*c3
      im=t/c1
      sec=t-im*c1
   15 idpy=365
      if(iy-4*(iy/4).eq.0) idpy=366
      if(id.le.idpy) return
      iy=iy+1
      id=id-idpy
      goto 15
      end
