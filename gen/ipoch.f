      function ipoch(iy,id,ih,im,isec)
c Converts yr,day,hr,mn,sec to epochal time which is defined by CSS to
c be the time in secs from 1970 day 0.
c Note that this version computes the integer number of seconds since
c 1970.
      ky = iy
      if (ky.lt.100.and.ky.ge.50) ky = ky + 1900
      if (ky.lt.50) ky = ky + 2000
      if (ky.lt.1970) stop 'ERROR: year < 1970 in ipoch'
      itime = (((id-1)*24+ih)*60+im)*60+isec
    5 if (ky.eq.1970) goto 10
      ky   = ky-1
      idpy = 365
      if (ky-4*(ky/4).eq.0) idpy=366
      itime = itime + 86400 * idpy
      goto 5
   10 ipoch = itime
      return
      end
