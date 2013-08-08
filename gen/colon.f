      function colon(qlon)
c
c input:
c  qlon = longitude, in degrees
c
c output:
c  colon = co-longitude (i.e. 0 to 360), in degrees
c      
      if (qlon.lt.0.0) then
        colon = 360.0 + qlon
      else
        colon = qlon
      endif
      return
      end
