c
c---------------------------------------------------------------------
c
      real function geoc4(arg)
c input:
c   arg    = geographic colatitude (radians)
c output:
c   geoc4 = geocentric colatitude (radians)
c (n.b. fac=(1-f)**2)
c
      data pi2,fac/1.570796326794895,0.993305621334896/
      geoc4=pi2-atan(fac*cos(arg)/amax1(1.e-30,sin(arg)))
      return
      end
