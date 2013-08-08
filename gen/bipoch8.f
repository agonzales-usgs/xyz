      subroutine bipoch8(time,iy,id,ih,im,ss)
c Converts yr,day,hr,mn,sec to epochal time which is defined by CSS to
c be the time in secs from 1970 day 0.
c Note that this version computes the real*8 number of seconds since
c 1970.
c On input, sec can be greater than 60.
c
      real*8 time,sec
      
      kd  = time / 86400.
      sec = time - (86400. * kd)
      ky  = 1970
      
 5    idpy = 365
      if (ky-4*(ky/4).eq.0) idpy=366
      if (kd.ge.idpy) then
        ky = ky + 1
        kd = kd - idpy
        goto 5
      endif
      iy = ky
      id = kd + 1
      ih = sec / 3600.d0
      im = (sec - (3600.d0 * dfloat(ih))) / 60.d0
      ss = sec - 3600.d0*dfloat(ih) - 60.d0*dfloat(im)
      return
      end 
