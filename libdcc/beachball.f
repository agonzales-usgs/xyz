      subroutine beachball(az,sin_i)
      character string*80
      dimension fmom(6),phs(2),del(2),rlam(2),eivals(3),eivecs(3,3)
     1    ,plungs(3),azims(3)
      real mom
      save fmom,eivecs,inp
      data inp/-99/
c
c if we have already entered eq info then we simply jump to the
c plotting part
c
      if (inp.ge.0 .and. inp.le.2) goto 100
      if (inp.eq.-1) return
c      
 10   write(6,501)
  501 format(' input:',5x,'moment tensor = 0'/12x,
     1                    'fault angles  = 1'/12x, 
     2                    'eigenvectors  = 2'/12x,
     #                    '<ret> to skip')
      read(*,'(a)') string
      if (lnblnk(string).le.0) then
        inp = -1
        return
      endif
      read(string,*) inp
      if (inp.lt.-1 .or. inp.gt.2) goto 10
c
c begin main block-if statement for the different types of input
c
      if(inp.eq.-1) then
        return

      elseif (inp.eq.2) then
        call eiinp(fmom)
      
      elseif (inp.eq.1) then
        write(6,511)
  511   format('type strike,dip,slip')
        read(5,*) strike,dip,slip
        mom = 0.0
        if(mom.eq.0.0) mom=1.
        mom=mom*10.**25
ccc        write(6,513) strike,dip,slip,mom
  513   format(3f8.2,e10.2)
        call momen1(dip,slip,strike,fmom)
        do 20 i=1,6
   20   fmom(i)=mom*fmom(i)
cc        write(6,"(6e9.2)") (fmom(i),i=1,6)
        call eqpar(fmom,scmom,phs,del,rlam,eivals,eivecs,plungs,azims)

      elseif (inp.eq.0) then
        write(6,"('type moment tensor components '
     #    'and exponent (* format)')")
        read(5,*) (fmom(i),i=1,6),iscale
        do 212 i=1,6
  212   fmom(i)=fmom(i)*10.**iscale
        write(6,"(6e9.2)") (fmom(i),i=1,6)
        call eqpar(fmom,scmom,phs,del,rlam,eivals,eivecs,plungs,azims)
      endif
c
c make beachball plot
c
 100  call ppfp1(6,3,1,eivecs,fmom,az,sin_i)
c      
      return
      end
