c
c---------------------------------------------------------------
c
      subroutine screen_decim(buf,npts,dt,max_laser_pts)
c Decimates a times series in a simplistic way appropriate
c for subsequent plotting on a laser printer or screen.
c
c NB: one cannot do any time series analysis on the decimated
c data -- the decimation process is only for display purposes.
c
c input:
c  buf() = data array (gets overwritten)
c  npts  = input # of samples (gets overwritten)
c  dt    = input sample interval (gets overwritten)
c  max_laser_pts = total number of pixels on output device
c                  avaliable for plotting the time series
c
c output:
c  buf() 
c  npts
c  dt
c
      dimension buf(*)
      
      if (npts.le.max_laser_pts) return
      
      idfact  = npts / max_laser_pts + 1
      idec    = 2 * idfact
      numpt     = 0
      do 10 j=1,npts,idec
        ldec = min0(idec,npts-j+1)
        if (numpt+2.ge.j+ldec) stop 'ERROR -- in screen_decim'
        call decim(buf,j,ldec,buf(numpt+1),buf(numpt+2))
 10     numpt = numpt + 2
      dt = ( (npts-1) * dt ) / (numpt - 1)
      npts = numpt
      return
      end
c
c-----------------------------------------------------------
c
      subroutine decim(buf,m,n,rfirst,rlast)
c  Find max and min of a section of a buffer and put in order of
c  occurence.
c
c input:
c  buf() = the buffer to be worked on
c  m     = starting point in buffer
c  n     = number of points to examine
c
c output:
c  rfirst = the min or max value of this section of the buffer
c  rlast  = the min or max value of this section of the buffer
c
      dimension buf(*)
      imin=m
      imax=m
c        
      do 5 i=m,m+n-1
        if(buf(i).gt.buf(imax)) imax=i
    5   if(buf(i).lt.buf(imin)) imin=i
c
      if (imin.gt.imax) then
        rfirst=buf(imax)
        rlast=buf(imin)
      else
        rfirst=buf(imin)
        rlast=buf(imax)
      endif 
      return
      end
