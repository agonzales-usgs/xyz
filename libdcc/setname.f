c
c---------------------------------------------------------
c
      subroutine setname(indx,filename,jj)
      character filename*80,fileid*80,nchn*3,nsta*4
      parameter(nmax=20000)
      common/files/nfiles,nnet(nmax),nsta(nmax),nchn(nmax),
     &   iy(nmax),id(nmax),ih(nmax),im(nmax),ss(nmax),
     &    nscan(nmax),fileid(nmax)

      idum = lnblnk(nsta(jj))
      if (idum.eq.3) then
        write(filename,31) nnet(jj),'_',nsta(jj),'_',
     &        nchn(jj),'_',indx,'.sac'
      else
        write(filename,32) nnet(jj),'_',nsta(jj),'_',
     &        nchn(jj),'_',indx,'.sac'
      endif
 31   format(a2,a1,a3,a1,a3,a1,i3,a4)
 32   format(a2,a1,a4,a1,a3,a1,i3,a4)
      
      idum = lnblnk(filename)
      do 10 i=1,idum
 10     if (filename(i:i).eq.' ') filename(i:i)='0'
      return
      end
