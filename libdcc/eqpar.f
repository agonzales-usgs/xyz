c
c-------------------------------------------------------------------
c
      subroutine eqpar(fmom,scmom,phs,del,rlam,eivals,eivecs
     1                 ,plungs,azims)
c
c   fmom(6)     input moment tensor components
c   scmom       output scalar moment
c   phs(2)      output strike azimuths
c   del(2)      output dips
c   rlam(2)     output 'rake' angles
c   eivals(3)   output eigenvalues (t-axis,inter.,p-axis)
c   eivecs(3,3) output: columns are normalized principal axes
c   plungs(3)   output plunges of principal axes
c   azims(3)    output azimuths of principal axes
c
c   all angles are in degrees
c
      dimension fmom(6),phs(2),del(2),rlam(2),eivals(3),eivecs(3,3)
     1    ,v(3,3),rn(3),e(3),plungs(3),azims(3)
      data radian/57.29577951/
      data hsq2/.70710678/
c
      scale=0.
      do 800 i=1,6
        scale=amax1(scale,abs(fmom(i)))
  800 continue
      if(scale.eq.0.) then
        write(6,"('no moment tensor')")
        return
      endif
      eivecs(1,1)=fmom(1)/scale
      eivecs(2,2)=fmom(2)/scale
      eivecs(3,3)=fmom(3)/scale
      eivecs(1,2)=fmom(4)/scale
      eivecs(2,1)=fmom(4)/scale
      eivecs(1,3)=fmom(5)/scale
      eivecs(3,1)=fmom(5)/scale
      eivecs(2,3)=fmom(6)/scale
      eivecs(3,2)=fmom(6)/scale
      call eigen(eivecs,v,3,1)
      eimax=eivecs(1,1)
      eimin=eivecs(1,1)
      imax=1
      imin=1
      do 10 i=2,3
      if(eivecs(i,i).le.eimax) goto 20
      imax=i
      eimax=eivecs(i,i)
   20 if(eivecs(i,i).gt.eimin) goto 10
      imin=i
      eimin=eivecs(i,i)
   10 continue
      if(imax.eq.imin) pause ' error 1 in eqpar '
      scmom=.5*(abs(eimax)+abs(eimin))
      iint=6-imax-imin
      eivals(1)=eivecs(imax,imax)
      eivals(2)=eivecs(iint,iint)
      eivals(3)=eivecs(imin,imin)
      do 30 i=1,3
      eivecs(i,1)=v(i,imax)
      eivecs(i,2)=v(i,iint)
   30 eivecs(i,3)=v(i,imin)
      do 40 j=1,3
      sum=0.
      do 45 i=1,3
   45 sum=sum+eivecs(i,j)*eivecs(i,j)
      sum=1./sqrt(sum)
      if(eivecs(1,j).gt.0.) sum=-sum
      do 50 i=1,3
   50 eivecs(i,j)=eivecs(i,j)*sum
   40 continue
c
      do 200 i=1,3
      plungs(i)=atant(-eivecs(1,i)
     1                ,sqrt(eivecs(2,i)**2+eivecs(3,i)**2))*radian
      azims(i)=atant(-eivecs(3,i),eivecs(2,i))*radian+180.
  200 continue
c
      sgn=-1.
      do 100 isgn=1,2
      sgn=-sgn
      do 110 i=1,3
      rn(i)=hsq2*(eivecs(i,1)+sgn*eivecs(i,3))
  110 e(i)=hsq2*(eivecs(i,1)-sgn*eivecs(i,3))
      if(rn(1).gt.0.) goto 120
      do 130 i=1,3
      rn(i)=-rn(i)
  130 e(i)=-e(i)
  120 sind=sqrt(rn(2)*rn(2)+rn(3)*rn(3))
      del(isgn)=atant(sind,rn(1))*radian
      phs(isgn)=atant(-rn(2),-rn(3))*radian+180.
      rlam(isgn)=atant(e(1),rn(2)*e(3)-rn(3)*e(2))*radian
  100 continue
      do 801 i=1,3
        eivals(i)=eivals(i)*scale
  801 continue
      scmom=scmom*scale
      return
      end
