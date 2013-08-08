c
c-----------------------------------------------------------
c
      subroutine sac2bin
c
c Converts SAC files to a super-simple binary format. The write
c statement could be changed to write ascii, if desired. 
c
c Assumes the SAC file name ends with the extension ".SAC".
c
      parameter (maxpts=2000000)
      character sacname*80
      dimension rdata(maxpts)
c
c
      do 5 j=1,80
          sacname(j:j) = ' '
 5    continue
c
c Loop over getting input filename. 
c Terminate with blank line
c
10    write(*,'(a,$)') 'input SAC file: '
      read(*,'(a)',err=10) sacname
      ilen = lnblnk(sacname)
      if (ilen.eq.0) goto 100
c
c open output file
c
      if (sacname(ilen-3:ilen) .eq. '.SAC') then
        open(8,file=sacname(1:ilen-3)//'.BIN',form='unformatted')
      else
        write(*,*) 'error - constructing output file name'
      endif
      
c
c read SAC binary file
c
      call rsac1(sacname,rdata,npts,beg,dt,maxpts,nerr)
      if (nerr.ne.0) pause 'error reading SAC file'
c
c write out simple file
c
      write(8) npts
      do 20 i=1,npts
        write(8) rdata(i)
 20   continue
      close(8)
      goto 10
c
 100  call gfs_close(1)
      return
      end
