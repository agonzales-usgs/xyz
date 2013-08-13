      function colat(qlat)
c
c input:
c  qlat = latitude, in degrees
c
c output:
c  colat = co-latitude (i.e. 0 to 180), in degrees
c     
      colat = 90.0 - qlat 
      return
      end
