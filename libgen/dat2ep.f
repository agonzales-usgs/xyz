      subroutine dat2ep(iy,id,ih,im,sec,time)
c Converts yr,day,hr,mn,sec to epochal time which is defined by CSS to
c be the time in secs from 1970 day 0.
c Note that this version computes the real*8 number of seconds since
c 1970.
c On input, sec can be greater than 60.
c
      real*8 time
      
      ky = iy
      if (ky.lt.100.and.ky.ge.50) ky = ky + 1900
      if (ky.lt.50) ky = ky + 2000
      if (ky.lt.1970) then
        time = -1.0
        stop 'ERROR: year < 1970 in dat2ep'
      endif
cc      time = (((id-1)*24+ih)*60+im)*60+sec
      time=((dfloat(id-1)*24.d0+dfloat(ih))*60.d0+dfloat(im))*60.d0+sec
    5 if (ky.eq.1970) goto 10
      ky   = ky-1
      idpy = 365
      if (ky-4*(ky/4).eq.0) idpy=366
      time = time + 86400.d0 * dfloat(idpy)
      goto 5
   10 continue
      return
      end
