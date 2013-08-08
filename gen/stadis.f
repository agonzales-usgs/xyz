c
c-----------------------------------------------------------------
c
      subroutine stadis(colat,colon,t1,p1,del,az)
c Computes the epicentral distance and azimuth from source to receiver.
c Latitudes are converted to geocentric latitudes prior to performing
c the computations (it is assumed that input latitudes are geographic).
c  input:
c    colat = source colatitude, usually from subroutine source (degrees)
c    colon =   "    colongitude,  "      "        "       "       "
c    t1    = station colatidue, from subroutine allsta (radians)
c    p1    =    "    colongitude, "      "        "        "
c  output:
c    del   = epicentral distance (degrees)
c    az    = azimuth from source to receiver, measure from North (degrees)
c
      real*8 co,si,caz,saz
      data rad/57.29578/
c  first do eq coords.
c                             { use geocentric e.q.colat }
      t0=geoc4(colat/rad)
      p0=colon/rad
      c0=cos(t0)
      s0=sin(t0)
c  now do station coords.
c                             { use geocentric station colat }
      t2=geoc4(t1)
      c1=cos(t2)
      s1=sin(t2)
c  now calculate distance
      dp=p1-p0
      co=c0*c1+s0*s1*cos(dp)
      si=dsqrt(1.d0-co*co)
      del=datan2(si,co)*rad
c  now calculate azimuth
      caz=(c1-c0*co)/(si*s0)
      dp2=-dp
      saz=-s1*sin(dp2)/si
      az=datan2(saz,caz)*rad
c                                  {change az to be between 0 and 360}
      if(az.lt.0.0) az=360.0 + az
      return
      end
