c
c---------------------------------------------------------
c
      subroutine sac_merge(tol,jchn,indx,jfile,jsta,istart,iend)
      character filename*80,jchn*3,nchn*3,fileid*80,jfile(*)*80,
     & jsta*4, nsta*4

      parameter (maxpts=864500)
      dimension rdata(maxpts)

      parameter (nmax=20000)
      dimension idx_list(nmax),ibeg(nmax), tbeg(nmax)
      common/files/nfiles,nnet(nmax),nsta(nmax),nchn(nmax),
     &   iy(nmax),id(nmax),ih(nmax),im(nmax),ss(nmax),
     &    nscan(nmax),fileid(nmax)

c
c build a list of the files that contain the desired channel and station
c
      indx = 0
      j = 0
      do 10 i=1,nfiles
        if (nchn(i) .eq. jchn .and. nsta(i) .eq. jsta) then
          j = j + 1
          idx_list(j) = i
        endif
 10   continue
      num_files = j
c
c zero matches
c 
      if (num_files.eq.0) return
c
c
c Read the first file to start building the buffer for the first 
c output file. (this also gets the sample interval)
c
      call rsac1(fileid(idx_list(1)),rdata,num,beg1,dt,maxpts,nerr)
      iref = idx_list(1)
      npts = num
c
c one match
c no need to copy this file over
c      
      if (num_files.eq.1) then
        j = idx_list(1)
        jfile(1) = fileid(j)
        indx = 1
        istart = ipoch(iy(j),id(j),ih(j),im(j),int(ss(j)))
        iend   = istart + dt * nscan(j)
        return
      endif
c
c more than one match
c
c now loop over target files and compute epochal start times
c       
      do 20 i=1,num_files
        jj = idx_list(i)
        iss = ss(jj)
        ibeg(i) = ipoch(iy(jj),id(jj),ih(jj),im(jj),iss)
        tbeg(i) = ss(jj) - iss
 20   continue
c
c check that files are in time sort order
c 
      do 25 i=1,num_files-1
        if (ibeg(i) .gt. ibeg(i+1)) then
          write(*,*) 'ERROR - .dat file not in time sort order'
          stop
        endif
 25   continue
c
c get first and last time of the time series - for later use
c by main program
c
      istart = ibeg(1)
      iend   = ibeg(num_files) + dt * nscan(idx_list(num_files))
c
c loop over target files, attempting to merge them together
c
      indx = 1
      do 30 i=1,num_files-1
        jj1 = idx_list(i)
        jj2 = idx_list(i+1)
c compute if gap or reversal
        tdiff = ibeg(i+1) - ibeg(i)
        tdiff = tdiff - dt*nscan(jj1)
        tdiff = tdiff - (tbeg(i+1) - tbeg(i))
c
c for now, pad across gaps. The padding will allow the next block-if
c to think we are within the time tolerance
c        
        if (tdiff .gt. tol*dt) then
          npt_add = anint(tdiff/dt)
          write(*,*) '  ',jchn,': gap, # of padding samples: ',npt_add
          do 31 k=1,npt_add
 31         rdata(npts+k) = rdata(npts)
          npts = npts + npt_add
        endif
c
c check for gaps and reversals
c
c for now, only break time series for large reversals
c        
        if (abs(tdiff) .gt. tol*dt .and. tdiff.lt.0.0) then
ccc        if (abs(tdiff) .gt. tol*dt) then
          write(*,*) '  ',jchn,
     &     ': tolerance exceeded, gap(+)/rev(-) = ',tdiff
          call setname(indx,filename,jj1)
c
c re-read the first file, so as to set the header elements in 
c memory
          call rsac1(fileid(iref),ydum,num,beg1,dt,1,nerr)
          if (nerr.ne.0) write(*,*) 'nerr = ',nerr
c update header and write the new sac file  
          call setnhv('NPTS',npts,nerr)
          call wsac0(filename,xdum,rdata,nerr)
          jfile(indx) = filename
c now, re-load buffer          
          indx = indx + 1
          call rsac1(fileid(jj2),rdata,num,beg1,dt,maxpts,nerr)
          iref = jj2
          npts = num
          
        else
c
c within time tolerance, so add this file to the buffer
c
          call rsac1(fileid(jj2),rdata(npts+1),num,beg,dt,
     &               maxpts-npts,nerr)
          npts = npts + num
        endif
 30   continue
c
c flush out buffer
c  first re-read the first file, to set the header, etc. 
c 
      call rsac1(fileid(iref),ydum,num,beg1,dt,1,nerr)   
      call setname(indx,filename,jj2)   
      call setnhv('NPTS',npts,nerr)
c
cc      write(*,*) ' writing file = ',filename(1:lnblnk(filename))
      call wsac0(filename,xdum,rdata,nerr)
      jfile(indx) = filename
      return
      end
