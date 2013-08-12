c
c-----------------------------------------------------------
c     
      subroutine gfs2sac2(io,itype,indx,lo,nwd,sacname)
c Converts GFS files to SAC binary format.  If the SAC output
c file already exists, you cannot overwrite it.
c
      character ktemp*8,nsta*4, nchn*4, ntyp*4, sacname*80
      dimension head(30)
      real*4 rdata
      common/myhed/nscan,nsta,nchn,ntyp,iy,id,ih,im,ss,dt,
     &            qlat,qlon,qdep,iys,ids,ihs,ims,sss,spare(12)
      real*8 ipoch8,tt
      equivalence (head,nscan)
      logical exists
      pointer (ptr,rdata)
c
c error checking
c
      inquire(file=sacname,exist=exists)
      if (exists) then
        write(*,*) 'ERROR - file already exists: ',
     &        sacname(1:lnblnk(sacname))
        return
      endif
c
c read GFS header and data
c
      call gfs_rwdir(io,head,indx,'r',ierr)
      ptr = malloc(4*nwd)
      if (ptr.eq.0) then
        write(*,*) 'ERROR - cannot malloc in gfs2sac2'
        return
      endif
      call gfs_rwdata(io,rdata,indx,lo,nwd,'r',ierr)
c
c set nscan and get new start time
      nscan = nwd
      tt = ipoch8(iy,id,ih,im,ss+(lo-1)*dt)
      call bipoch8(tt,iy,id,ih,im,ss)
      isec = ss
      msec = 1000. * float(int(ss) - isec)
c
c write out SAC binary file
c
      call newhdr
      call setnhv('NPTS',nscan,nerr)
      call setfhv('B',0.0,nerr)
      call setihv('IFTYPE','ITIME',nerr)
      call setlhv('LEVEN',.true.,nerr)
      call setfhv('DELTA',dt,nerr)
      call setnhv('NZYEAR',iy,nerr)
      call setnhv('NZJDAY',id,nerr)
      call setnhv('NZHOUR',ih,nerr)
      call setnhv('NZMIN', im,nerr)
      call setnhv('NZSEC', isec,nerr)
      call setnhv('NZMSEC',msec,nerr)
      call setfhv ('O',0.0,nerr)
      call setihv('IZTYPE','IO',nerr)
      write(ktemp,'(2a4)') ntyp,'    '
      call setkhv('KNETWK',ktemp,nerr)
      write(ktemp,'(2a4)') nsta,'    '
      call setkhv('KSTNM',ktemp,nerr)
      write(ktemp,'(2a4)') nchn,'    '
      call setkhv('KCMPNM',ktemp,nerr)
      if (itype.eq.3) then
        call setfhv('EVLA',qlat,nerr)
        call setfhv('EVLO',qlon,nerr)
        call setfhv('EVDP',qdep*1000,nerr)
      endif
      call setlhv('LOVROK',.false.,nerr)
      call wsac0(sacname(1:lnblnk(sacname)),rdata,rdata,nerr)
      if (nerr.ne.0) pause 'error writing SAC file'
      call free(ptr)
c
      return
      end
