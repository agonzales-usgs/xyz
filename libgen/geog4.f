c
c---------------------------------------------------------------------
c
      real function geog4(arg)
c input:
c   arg    = geocentric colatitude (radians)
c output:
c   geog4 = geographic colatitude (radians
c (n.b. fac=(1-f)**2)
c
      data pi2,fac/1.570796326794895,0.993305621334896/
      geog4=pi2-atan(cos(arg)/(fac*amax1(1.e-30,sin(arg))))
      return
      end
