      subroutine bdf2gfs(io_bdf,io_gfs,itype,indx_end,imode)
c Converts ASL "bdf" files to GFS format.
c
c input:
c  io_bdf = unit number of input BDF file (can be standard in)
c  io_gfs = unit number of gfs file
c  itype  = type of GFS file
c  indx_end   = index of last entry in output GFS file
c  imode  = mode for putting bdf time series into output file
c         = 1: string them as we go
c         = 2: no stringing whatsover
c         = 3: string and splice (i.e. deal with overlaps, etc.)
c output:
c   indx_end = index of last entry in output GFS file
c
c NOTES: 
c   In mode 2: each individual data segment in the input BDF file is
c   written as a single GFS time series.
c
c   User must open bdf and gfs files before calling this routine.
c
c   User must close the files after calling this routine.
c 
      character label*4,string*80,nnet*4,nsta*4,nloc*4,nchn*4
      dimension head(100)
      common/myhed/nscan,nnet,nsta,nloc,nchn,iy,id,ih,im,ss,dt,
     &             spare(89)
      equivalence (head,nscan)
      pointer (p1,rdata)
c
c set verbosity, for troubleshooting purposes
c
      iverb = 0
c
c 
 10   format(a4,a80)
 4    continue
        nsta = '    '
        nnet = '    '
        nloc = '    '
        nchn = '    '
        stla = 0.0
        stlo = 0.0
        stele = 0.0
        stdep = 0.0
        dt = 0.0
        iy = 0
        id = 0
        ih = 0
        im = 0
        ss = 0.0
        nscan = 0
c      
 5    read(io_bdf,10,end=20) label,string
      if (label.eq.'STA ') then
c Sept. 18, 2008 bug fix.  Need to reset the location code every time a STA field is encountered.
c This prevents mislabeling the location code when transitioning from a record with a LOC field
c to a record without one
        nloc = '    '
        nsta = string(1:4)
c use this syntax to catch both "NET " and "NETW"
      elseif (label(1:3).eq.'NET') then 
        nnet = string(1:2)
      elseif (label(1:3).eq.'LOC') then 
        nloc = string(1:2)
      elseif (label.eq.'COMP') then 
        nchn = string(1:4)
      elseif (label.eq.'LATT')  then
        read(string,*,err=5) stla
      elseif (label.eq.'LONG') then 
        read(string,*,err=5) stlo
      elseif (label.eq.'ALTI')  then
        read(string,*,err=5) stele
      elseif (label.eq.'DEPT') then 
        read(string,*,err=5) stdep
      elseif (label.eq.'RATE') then
        dt = 0.0
        read(string,*,err=5) rate
        if (rate.gt.0.0) dt = 1.0 / rate
      elseif (label.eq.'TIME') then
        read(string,'(i4,1x,i3,1x,i2,1x,i2,1x,f7.4)',err=5) 
     &       iy,id,ih,im,ss
      elseif (label.eq.'NSAM') then
        nscan = 0
        read(string,*,err=5) nscan
      elseif (label.eq.'DATA' .and. nscan.gt.0 ) then
        p1 = malloc(4*nscan)
        if (p1.eq.0) then
          write(0,*) 'ERROR - unable to malloc in bdf2gfs'
          return
        endif
        npts = nscan
        call rd_data(io_bdf,rdata,npts)
c
c write out GFS file
c note: we read data even if dt=0.0 (i.e. LOG channel), but we don't
c want to try to write such data, so test first
c
        if ( dt.ne.0.0 ) then
          if (npts.eq.nscan) then
            call dump_gfs(io_gfs,head,rdata,indx_end,itype,iverb)
          elseif (npts.ne.nscan .and. npts.gt.0) then
            write(0,*) 'WARNING -- incorrect number of samples:'
            write(0,*) '  wanted: ',nscan,'  got: ',npts
            write(0,21)   nsta, nchn,iy,id,ih,im,ss
            write(0,*) '  writing this time series anyway'
            nscan = npts
            if (imode.eq.1) then
              call dump_gfs(io_gfs,head,rdata,indx_end,itype,iverb)
            elseif (imode.eq.2) then
              call dump_gfs_nostr(io_gfs,head,rdata,indx_end,itype,iverb)
            elseif (imode.eq.3) then
              call dump_gfs_splice(io_gfs,head,rdata,indx_end,itype,iverb)
            else
              write(0,*) 'ERROR -- wrong write mode in bdf2gfs'
              call exit(1)
              stop
            endif
          else
            write(0,*) 'ERROR -- no data read, skipping time series :'
            call prhdr(0,head,itype)
          endif
        endif
        call free(p1)
        nscan = 0
c After processing a "DATA" label, we goto 4, so all header
c variables can be reinitialized
c
        goto 4
      endif
c otherwise, we goto 5 and read some more
      goto 5
c
c finished reading a bdf file
c 
 21   format(a4,1x,a4,1x,i4.4,',',i3.3,',',2(i2.2,':'),f7.4)
 20   continue
      return
      end
c
c----------------------------------------
c
      subroutine rd_data(io_bdf,rdata,nscan)
      dimension rdata(*)
c
ccc      read(io_bdf,*,err=10) (rdata(i),i=1,nscan)
c
c read using a slower method, but allows us to 
c catch errors more effectively
c
      j = 0
      do 10 i=1,nscan
        read(io_bdf,*,err=20) rdata(i)
 10     j = i
      return
c
c  error exit
c
 20   continue
      nscan = j
      return
c 
      end
c 
c-------------------------------------------------------------
c
      subroutine dump_gfs(io,head,rdata,indx_end,itype,iverb)
c
c purpose:
c   To write a GFS time series to an output GFS file. Checks
c   to see if each new time series can be appended to a time
c   series already in the GFS file.
c   It does consider time series already in the GFS file when it 
c   was opened.
c
c input:
c   io     = GFS unit number
c   head() = GFS time series header
c   rdata() = GFS time series data array
c   indx_end = index number of last entry in GFS file
c   itype    = GFS file type
c   iverb    = verbosity (0 or 1)
c
c output:
c   indx_end = updated index to last entry in GFS file
c 
c notes:
c   common block myhed1 is used to pass info from this routine
c   to "append" subroutine
c   
      dimension head(*), rdata(*)
      dimension head1(100), head2(100)
c
      common/myhed1/nscan,nnet,nsta,nloc,nchn,iy,id,ih,im,ss,dt,spare(89)
      common/myhed2/nscan2,nnet2,nsta2,nloc2,nchn2,spare2(95)
      equivalence (head1,nscan)
      equivalence (head2,nscan2)
      data iflag/0/
c
c scan existing gfs file so we can append to data already
c in the file -- uses "myhed1" as scratch space
c      
      if (iflag.eq.0) then
        call scan_gfs(io,indx_end)
        iflag = 1
      endif
c
c load elements of common block myhed1 -- for use by "append"
      do 10 i=1,100
 10     head1(i) = head(i)
c
c routine "append" uses common myhed1 
      call append(indx_end,indx)
c      
      if (indx.ne.0) then
        call gfs_rwdir(io,head2,indx,'r',ierr)
        ibeg   = nscan2 + 1
        nscan2 = nscan2 + nscan
        call gfs_rwdir(io,head2,indx,'w',ierr)
        call gfs_rwdata(io,rdata,indx,ibeg,nscan,'w',ierr)
        if (iverb.eq.1) call prhdr(indx,head2,itype)
      else
        call gfs_rwentry(io,head,rdata,indx_end,'w')
        if (iverb.eq.1) call prhdr(indx_end,head,itype)
      endif
      return
      end
c 
c-------------------------------------------------------------
c
      subroutine scan_gfs(io,indx_end)
c
c purpose:
c   Scans entries in existing GFS file, so that the "append" 
c   subroutine can append to these already existing traces.
c
c input:
c  io  = GFS unit number
c  indx_end = number of entries already in GFS file
c
c output:
c  fills in information in common block appinfo (used by "append")
c
c notes:
c   uses head1() as sort of scratch space. This same array is also
c   used by "dump_gfs" to pass info to "append"
c
      dimension head1(100)
      common/myhed1/nscan,nnet,nsta,nloc,nchn,iy,id,ih,im,ss,dt,spare(89)
      equivalence (head1,nscan)
c
      parameter (maxseis=40000)
      real*8 time_ref
      common /appinfo/indx_ref(maxseis),
     &                nnet_ref(maxseis),nsta_ref(maxseis),
     &                nloc_ref(maxseis),nchn_ref(maxseis),
     &                dt_ref(maxseis),time_ref(maxseis),
     &                nscan_ref(maxseis),iseis
c      
      iseis = 0
      do 10 i=1,indx_end
        call gfs_rwdir(io,head1,i,'r',ierr)
        iseis = iseis + 1
        nnet_ref(iseis) = nnet
        nsta_ref(iseis) = nsta
        nloc_ref(iseis) = nloc
        nchn_ref(iseis) = nchn
        dt_ref(iseis)   = dt
        indx_ref(iseis) = i
        nscan_ref(i)    = nscan
        call dat2ep(iy,id,ih,im,ss,time_ref(iseis))
        if (iseis.eq.maxseis) then
          write(*,*) 'WARNING from scan_gfs -- maxseis exceeded'
          return
        endif
 10   continue      
      return
      end
c 
c-------------------------------------------------------------
c
      subroutine append(indx_end,indx)
c
c purpose:
c   This routine keeps track of what dump_gfs has written,
c   so it can determine if the current time series can be appended
c   to one that dump_gfs has already written.
c
c input:
c    indx_end = index of last entry in GFS file
c 
c output:
c    indx_end = updated index of last entry in GFS file
c    indx     = 0 if time series cannot be appened to another
c            != 0, then we append this time series to entry indx
c
      character*4 nnet, nsta, nloc, nchn
      common/myhed1/nscan,nnet,nsta,nloc,nchn,iy,id,ih,im,ss,dt,spare(89)
c
c we use the appinfo common block to store info about time
c series we have already written. Note that time_ref stores
c the end time of each seismogram
c
      parameter (maxseis=40000)
      real*8 timex, time_ref
      character*4 nnet_ref, nsta_ref, nloc_ref, nchn_ref
      common /appinfo/indx_ref(maxseis),
     &                nnet_ref(maxseis),nsta_ref(maxseis),
     &                nloc_ref(maxseis),nchn_ref(maxseis),
     &                dt_ref(maxseis),time_ref(maxseis),
     &                nscan_ref(maxseis),iseis
c      
      call dat2ep(iy,id,ih,im,ss,timex)
      
      indx = 0
      
      do 10 i=iseis,1,-1
        delta = (time_ref(i) + dt_ref(i) * nscan_ref(i)) - timex
        if (nnet  .eq. nnet_ref(i) .and.
     &      nsta  .eq. nsta_ref(i) .and.
     &      nloc  .eq. nloc_ref(i) .and.
     &      nchn  .eq. nchn_ref(i) .and.
     &      dt    .eq. dt_ref(i)   .and.
     &      abs(delta) .lt. 0.5 * dt) then
           indx = indx_ref(i)
c update end time for this time series
           nscan_ref(i) = nscan_ref(i) + nscan
           return
        endif
 10   continue
 15   continue
      
      if (iseis.ge.maxseis) then
        write(*,*) 'WARNING from append -- maxseis exceeded'
        return
      endif
      
      iseis = iseis + 1
      indx_end = indx_end + 1
      nnet_ref(iseis) = nnet
      nsta_ref(iseis) = nsta
      nloc_ref(iseis) = nloc
      nchn_ref(iseis) = nchn
      dt_ref(iseis)   = dt
      indx_ref(iseis) = indx_end
      time_ref(iseis) = timex
      nscan_ref(iseis) = nscan
      
      return
      end
c
c--------------------------------------------------------------
c--------------------------------------------------------------
c--------------------------------------------------------------
c--------------------------------------------------------------
c     
      subroutine dump_gfs_nostr(io,head,rdata,indx_end,itype,iverb)
c
c purpose:
c   To write a GFS time series to an output GFS file. Checks
c   to see if each new time series can be appended to a time
c   series already in the GFS file.
c   It does consider time series already in the GFS file when it 
c   was opened.
c
c input:
c   io     = GFS unit number
c   head() = GFS time series header
c   rdata() = GFS time series data array
c   indx_end = index number of last entry in GFS file
c   itype    = GFS file type
c   iverb    = verbosity (0 or 1)
c
c output:
c   indx_end = updated index to last entry in GFS file
c   
      dimension head(*), rdata(*)
c      
      indx_end = indx_end + 1
      call gfs_rwentry(io,head,rdata,indx_end,'w')
      if (iverb.eq.1) call prhdr(indx_end,head,itype)
      return
      end
c 
c--------------------------------------------------------------
c--------------------------------------------------------------
c--------------------------------------------------------------
c--------------------------------------------------------------
c
      subroutine dump_gfs_splice(io,head,rdata,indx_end,itype,iverb)
c
c purpose:
c   To write a GFS time series to an output GFS file. Checks
c   to see if each new time series can be appended to a time
c   series already in the GFS file.
c   It does consider time series already in the GFS file when it 
c   was opened.
c
c input:
c   io     = GFS unit number
c   head() = GFS time series header
c   rdata() = GFS time series data array
c   indx_end = index number of last entry in GFS file
c   itype    = GFS file type
c   iverb    = verbosity (0 or 1)
c
c output:
c   indx_end = updated index to last entry in GFS file
c 
c notes:
c   common block myhed1 is used to pass info from this routine
c   to "append" subroutine
c   
      dimension head(*), rdata(*)
      dimension head1(100), head2(100)
c
      common/myhed1/nscan,nnet,nsta,nloc,nchn,iy,id,ih,im,ss,dt,spare(89)
      common/myhed2/nscan2,nnet2,nsta2,nloc2,nchn2,spare2(95)
      equivalence (head1,nscan)
      equivalence (head2,nscan2)
      data iflag/0/
c
c scan existing gfs file so we can append to data already
c in the file -- uses "myhed1" as scratch space
c      
      if (iflag.eq.0) then
        call scan_gfs(io,indx_end)
        iflag = 1
      endif
c
c load elements of common block myhed1 -- for use by "append"
      do 10 i=1,100
 10     head1(i) = head(i)
c
c routine "append" uses common myhed1 
      call append_splice(io,indx_end,rdata,indx,ioff)
c      
      if (indx.gt.0) then
        call gfs_rwdir(io,head2,indx,'r',ierr)
        ibeg   = nscan2 + 1
        nscan2 = nscan2 + nscan
        call gfs_rwdir(io,head2,indx,'w',ierr)
        call gfs_rwdata(io,rdata(ioff),indx,ibeg,nscan,'w',ierr)
        if (iverb.eq.1) call prhdr(indx,head2,itype)
      elseif (indx.eq.0) then
        call gfs_rwentry(io,head,rdata,indx_end,'w')
        if (iverb.eq.1) call prhdr(indx_end,head,itype)
      endif
      return
      end
c
c-------------------------------------------------------------
c
      subroutine append_splice(io,indx_end,rdata,indx,ibeg)
c
c purpose:
c   This routine keeps track of what dump_gfs has written,
c   so it can determine if the current time series can be appended
c   to one that dump_gfs has already written. If the new time series
c   overlaps an existing one, then dump_gfs_splice will only
c   append the non-overlapped portion. If the new time series is
c   contained within a previous one, the new one is discarded.
c
c input:
c    io       = unit number of GFS output file
c    indx_end = index of last entry in GFS file
c 
c output:
c    indx_end = updated index of last entry in GFS file
c    indx     = 0 if time series cannot be appended to another
c             > 0, then we append this time series to entry indx
c             < 0, then discard this time series, it is duplicated
c    ibeg     = starting sample to use, when indx > 0
c             = 1 when indx=0
c
      dimension rdata(*),head(100)
      character*4 nnet, nsta, nloc, nchn
      common/myhed1/nscan,nnet,nsta,nloc,nchn,iy,id,ih,im,ss,dt,spare(89)
c
c we use the appinfo common block to store info about time
c series we have already written. Note that time_ref stores
c the end time of each seismogram
c
      parameter (maxseis=40000)
      real*8 t0, t1, t2, t3, time_ref
      character*4 nnet_ref, nsta_ref, nloc_ref, nchn_ref
      common /appinfo/indx_ref(maxseis),
     &                nnet_ref(maxseis),nsta_ref(maxseis),
     &                nloc_ref(maxseis),nchn_ref(maxseis),
     &                dt_ref(maxseis),time_ref(maxseis),
     &                nscan_ref(maxseis),iseis
     
      equivalence (head,nscan)
      data tol/0.5/
      
      
      itest = 1
      
      
c 
c t1 = time of first sample in the new series
c t3 = time of last sample in the new series 
c    
      call dat2ep(iy,id,ih,im,ss,t1)
      t3 = t1 + (nscan - 1) * dt
      
      indx = 0
      ibeg = 1
      
      do 10 i=iseis,1,-1
        if (nnet  .eq. nnet_ref(i) .and.
     &      nsta  .eq. nsta_ref(i) .and.
     &      nloc  .eq. nloc_ref(i) .and.
     &      nchn  .eq. nchn_ref(i) .and.
     &      dt    .eq. dt_ref(i)        ) then
c
c Compute the time of the ideal next sample after the end of the 
c reference series.
c     
          t0 = time_ref(i) + dt_ref(i) * nscan_ref(i)
c
c Does our new time series fit right on the end?
c (this is just a special case of the next block-if,
c  but we test for it directly to speed up the code, since 
c  we expect it to happen the most frequently )
c   
          if ( dabs(t0-t1) .lt. tol * dt ) then
            indx = indx_ref(i)
            ibeg = 1
            nscan_ref(i) = nscan_ref(i) + nscan
            return
          endif
c
c i2 = closest sample of our new time series to t0 
c t2   =  time of sample i2
c         
          i2 = 1 + nint( (t0-t1) / dt )
          if (i2.lt.1) goto 10
          t2   = t1 + (i2-1) * dt
c
c does part of our new time series fit?
c (if we return, note we don't update header time)
          if ( dabs(t0-t2) .lt. tol * dt .and. i2.le.nscan) then
            if (itest.eq.1) then
              ncomp = i2 - 1
              call test_splice(io,indx_ref(i),nscan_ref(i)-ncomp+1,
     &                         rdata,ncomp,ierr)
              if (ierr.ne.0) then
                write(0,*) 'ERROR -- data overlap comparison failed'
                call wrhdr(0,0,head,10)
c note that at this point we could "goto 10" to keep looking for a 
c place to splice this time series, but since we have an error condition
c it is probably better to just flush this time series to the output.
                goto 30
              endif
            endif
            indx  = indx_ref(i)
            nscan = nscan - i2 + 1
            ibeg  = i2
            nscan_ref(i) = nscan_ref(i) + nscan
            return
          endif
c
c is our new time series completely included in a reference series?
c
          if ( t1.ge.time_ref(i) .and. t3.le.t0-dt+tol*dt ) then
            if (itest.eq.1) then
              ncomp = nscan
c i2 = sample of ref time series which is closest to start of new series 
              i2 = 1 + nint( (t1-time_ref(i)) / dt )
              call test_splice(io,indx_ref(i),i2,rdata,ncomp,ierr)
              if (ierr.ne.0) then
                write(0,*) 'ERROR -- data inclusion comparison failed'
                call wrhdr(0,0,head,10)
c see comment in previous block-if
                goto 30
              endif
            endif
            indx = -1
            return
          endif
c
        endif
 10   continue
c 
c
c We come to statement 30 if:
c  a) time series (or some fraction of it) does not splice onto previous series
c  b) time series is not contained within a previous series
c  c) time series overlap or inclusion tests above are failed
c   
 30   continue 
c
      ibeg = 1
      indx_end = indx_end + 1
c
c do we have the space to store info about this time series?
c
      if (iseis.ge.maxseis) then
        write(*,*) 'WARNING from append -- maxseis exceeded'
        return
      endif
c      
      iseis = iseis + 1
      nnet_ref(iseis) = nnet
      nsta_ref(iseis) = nsta
      nloc_ref(iseis) = nloc
      nchn_ref(iseis) = nchn
      dt_ref(iseis)   = dt
      indx_ref(iseis) = indx_end
      time_ref(iseis) = t1
      nscan_ref(iseis) = nscan
      
      return
      end
c
c-------------------------------------------------------------
c
      subroutine test_splice(io,indx,ibeg,rdata,ncomp,ierr)
c
c Compares samples in a buffer to samples already in a GFS file.
c
c input:
c  io   = GFS file unit number
c  indx = index of entry in gfs file
c  ibeg = starting sample to use, for data in GFS file
c  rdata() = array of data to be compared
c  ncomp = number of samples to be compared
c
c output:
c  ierr  = 0 comparison is okay
c       != 0 comparison failed
c
      dimension rdata(*)
      pointer (p0,fdata)
      
      ierr = 1
      p0 = malloc(4*ncomp)
      if (p0.eq.0) then
        write(*,*) 'ERROR - unable to malloc in test_splice'
        ierr = 1
        return
      endif
      call gfs_rwdata(io,fdata,indx,ibeg,ncomp,'r',ierr)
      
      call test_comp(fdata,rdata,ncomp,ierr)
      call free(p0)
      return
      end
c
c-------------------------------------------------------------
c
      subroutine test_comp(fdata,rdata,ncomp,ierr)
      dimension fdata(*), rdata(*)
c
      ierr = 1
      
      do 10 i=1,ncomp
        if ( fdata(i).ne.rdata(i) ) return
 10   continue
      
      ierr = 0
      return
      end
      
      
      
