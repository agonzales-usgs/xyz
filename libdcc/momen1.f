c
c-------------------------------------------------------------------
c
      subroutine momen1(dip,slip,strike,xr)
c     this subroutine returns moment tensor components
c     input: fault plane parameters; aki's convention
      dimension xr(6)
      data pi/3.1415926536/
      del=dip*pi/180.
      gam=slip*pi/180.
      sig=strike*pi/180.
      s2d=sin(2.*del)
      sg=sin(gam)
      c2d=cos(2.*del)
      cg=cos(gam)
      ss=sin(sig)
      cs=cos(sig)
      c2s=cos(2.*sig)
      s2s=sin(2.*sig)
      sd=sin(del)
      cd=cos(del)
      xr(1)=s2d*sg
      xr(2)=-(s2d*ss*ss*sg+sd*s2s*cg)
      xr(3)=-s2d*cs*cs*sg+sd*s2s*cg
      xr(4)=-c2d*ss*sg-cd*cs*cg
      xr(5)=-c2d*cs*sg+cd*ss*cg
      xr(6)=-(.5*s2d*s2s*sg+sd*c2s*cg)
      return
      end
