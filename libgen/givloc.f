c
c-----------------------------------------------------------------------
c
      subroutine givloc(colat,colon,del,az,t1,p1)
c input:
c   colat,colon = source colatitude and colongitude from sub. source
c   del         = distance in degrees
c   az          = azimuth of station from source
c output:
c   t1 = station latitude  ( + = N, - = S)
c   p1 = station longitude ( + = E, - = W)
c
      data rad/57.29578/
      delr=del/rad
      azr=az/rad
c                             {convert to geocentric}
      t0=geoc4(colat/rad)
      ctheta=sin(delr)*sin(t0)*cos(azr) + cos(t0)*cos(delr)
      t1=acos(ctheta)
      if (t0.eq.0.0) then
        p1=az
      elseif (t1.eq.0.0) then
        p1=0.0
      else
        sphi=sin(delr)*sin(azr)/sin(t1)
        cphi=(cos(delr) - cos(t0)*ctheta)/(sin(t0)*sin(t1))
        p1=colon + atan2(sphi,cphi)*rad
      endif
c                                 {convert colatitude to geograf. latitude}
      t1=90.0-geog4(t1)*rad
c                                        { assume p1 never > 720 }
      if (p1.gt.360.0) p1 = p1 - 360.0
c                                        {convert colongitude to longitude}
      if (p1.gt.180.0) p1 = p1 - 360.0
      return
      end
