c
c-------------------------------------------------------------------
c
      subroutine ppfp1(lu,kin,ishft,u,fmom,az,sin_i)
c
c     subroutine for printer plotting nodal plane
c     beach balls
c
c     lu      logical unit
c     kin     width and height of plot in inches
c     ishft   number of characters from left margin
c             to start plot
c     u(3,3)  matrix whose columns are the eigenvectors
c             of the moment tensor (t,n,p) as returned
c             by subroutine eqpar
c     fmom(6) six elements of moment tensor
c
c
      dimension u(3,3),fmom(6)
      character*1 work(133)
      character*16 form
      data pi/3.14159265/
c
      write(lu,11)
   11 format(//)
      rt2=sqrt(2.)
      ix=1+ishft
      ix2=131-ix
      write(form,1)  ix,ix2
    1 format(1h(,i2,2hx,i3,3ha1))
c
c
      ncol=10*kin
      nrow=6*kin
      rk5=float(5*kin)
      rk3=float(3*kin)
      
      rad = pi / 180.0
      rr = sin_i
      x = rr * sin(rad*az)
      y = rr * cos(rad*az)
      icol_p = x * ncol / 2 + ncol / 2 + 1
      irow_p = nrow / 2 - y * nrow / 2 + 1 
c
      do 20 ip=1,3
      uu=u(1,ip)
      fac=sqrt(2.*(1.+uu)/(1.00001-uu*uu))
      x=u(3,ip)*fac
      y=u(2,ip)*fac
      icol=rk5*(x/rt2+1.)+1.
      irow=rk3*(y/rt2+1.)+1.
      irow=max0(1,min0(irow,nrow))
      icol=max0(1,min0(icol,ncol))
      if(ip.eq.3) icolp=icol
      if(ip.eq.3) irowp=irow
      if(ip.eq.1) icolt=icol
      if(ip.eq.1) irowt=irow
      if(ip.eq.2) irown=irow
      if(ip.eq.2) icoln=icol
   20 continue
c
c
      do 10 irow=1,nrow
      if(irow.eq.1) go to 8
      yn=rt2*((float(irow-1 )-.5)/rk3-1.)
      ys=rt2*((float(irow+1)-.5)/rk3-1.)
    8 if(irow.eq.1.) yn=y
      y=rt2*((float(irow)-.5)/rk3-1.)
      testto=0
      testpo=0
      vro=0
      do 30 icol=1,ncol
      x=rt2*((float(icol)-.5)/rk5-1.)
      xe=rt2*((float(icol+1)-.5)/rk5-1.)
      r2=x*x+y*y
      r2n=x*x+yn*yn
      r2s=x*x+ys*ys
      r2e=xe*xe+y*y
      r=sqrt(r2)
      rn=sqrt(r2n)
      rs=sqrt(r2s)
      re=sqrt(r2e)
      vr=-1.+.5*r2
      vrn=-1.+.5*r2n
      vrs=-1.+.5*r2s
      vre=-1.+.5*r2e
      if(vr.le.0.) goto 40
      work(icol)=' '
      goto 50
   40 st=sqrt(1.-vr*vr)
      stn=sqrt(1.-vrn*vrn)
      sts=sqrt(1.-amin1(1.,vrs*vrs))
      ste=sqrt(1.-vre*vre)
      vt=st*y/r
      vtn=stn*yn/rn
      vts=sts*ys/rs
      vte=ste*y/re
      vp=st*x/r
      vpn=stn*x/rn
      vps=sts*x/rs
      vpe=ste*xe/re
      testt=abs(u(1,1)*vr+u(2,1)*vt+u(3,1)*vp)
      testtn=abs(u(1,1)*vrn+u(2,1)*vtn+u(3,1)*vpn)
      testts=abs(u(1,1)*vrs+u(2,1)*vts+u(3,1)*vps)
      testte=abs(u(1,1)*vre+u(2,1)*vte+u(3,1)*vpe)
      testp=abs(u(1,3)*vr+u(2,3)*vt+u(3,3)*vp)
      testps=abs(u(1,3)*vrs+u(2,3)*vts+u(3,3)*vps)
      testpe=abs(u(1,3)*vre+u(2,3)*vte+u(3,3)*vpe)
      testpn=abs(u(1,3)*vrn+u(2,3)*vtn+u(3,3)*vpn)
c     if(testp.gt.testt) work(icol)='-'
c     if(testt.ge.testp) work(icol)='#'
c
      test=fmom(1)*vr*vr+fmom(2)*vt*vt+fmom(3)*vp*vp
     1  +2.*(fmom(4)*vr*vt+fmom(5)*vr*vp+fmom(6)*vt*vp)
      if(test.gt.0.) work(icol)='#'
      if(test.le.0.) work(icol)='-'
      if((testt.gt.testp).and.(testpn.gt.testtn).and.
     1 (vrn.le.0.)) go to 9
      if((testt.gt.testp).and.(testps.gt.testts).and.
     1 (vrs.le.0.)) go to 9
      if((testt.gt.testp).and.(testpe.gt.testte).and.
     1 (vre.le.0.)) go to 9
      if((testt.gt.testp).and.(testpo.gt.testto).and.
     1 (vro.le.0.)) go to 9
      go to 2
ccc    9 work(icol)='*'
    9 continue
    2 continue
ccc   50 if((iabs(icol-icolp).le.1.and.iabs(irow-irowp).le.1)
ccc     1 .or.(iabs(icol-icolt).le.1.and.iabs(irow-irowt).le.1)
ccc     2 .or.(iabs(icol-icoln).le.1.and.iabs(irow-irown).le.1))
ccc     3 work(icol)=' '
ccc      if(icol.eq.icolp.and.irow.eq.irowp) work(icol)='p'
ccc      if(icol.eq.icolt.and.irow.eq.irowt) work(icol)='t'
ccc      if(icol.eq.icoln.and.irow.eq.irown) work(icol)='n'
   50 if((iabs(icol-icolp).le.1.and.iabs(irow-irowp).le.1)
     1 .or.(iabs(icol-icolt).le.1.and.iabs(irow-irowt).le.1))
     3 work(icol)=' '
      if(icol.eq.icolp.and.irow.eq.irowp) work(icol)='p'
      if(icol.eq.icolt.and.irow.eq.irowt) work(icol)='t'

      testto=testt
      testpo=testp
      vro=vr
   30 continue
c
c
      if (irow.eq.irow_p ) work(icol_p) = 'X'
      write(lu,form)(work(icol),icol=1,ncol)
   10 continue
      return
      end
