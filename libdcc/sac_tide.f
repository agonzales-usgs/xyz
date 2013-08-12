c
c---------------------------------------------------
c      
      subroutine sac_tide(numevt,nspevt,jfile)
c 
c Generates a SAC command file which organizes data and
c synthetic plots on a single page (data as solid lines, 
c syntheics as dashed lines, all seismograms for a given 
c event in one column, several columns per page, all traces 
c with same time axis.
c
c input:
c  numevt = # of events
c  nspevt() = # of seismograms for each event 
c             (e.g. nspevt(1) = # of seismograms for event # 1)
c  nspts()  = # of points in each seismogram
c
c output:
c  file "sac.view"
c
      parameter (nmax=20000)
      character jfile(nmax)*80

      dimension nspevt(1),nspts(nmax),tmin(nmax),tmax(nmax)
      data icol/1/, iflag/0/
c
c ncol = # of columns per page
c xsep = separation between columns (in SAC units)
c xtot = total plotting width used (SAC units)
c xmax = maximum x dimension for any trace
c x0   = starting value for plotting
c ytot and y0 = similar to x0 and xtot
c
      ncol  = 1
      xsep  = 0.025
      xtot  = 0.9
      xmax= ( xtot - (ncol-1)*xsep ) / ncol
      x0    = 0.05
      ytot  = 0.9
      y0    = 0.95
c
      if (iflag.eq.0) then
        iflag = 1  
        open(8,file='tide.cmd')
        write(8,*) 'qdp off'
        write(8,*) 'line i on'
        write(8,*) 'axes off all'
        write(8,*) 'border off'
        write(8,*) 'ticks off all'
        write(8,*) 'gtext size tiny'
        write(8,*) 'fileid off'
        write(8,*) 'beginframe'
      endif

      maxpts = 0
      do 2 i=1,numevt
      do 2 j=1,nspevt(i)
        call syntide(jfile(j),nspts(j),tmin(j),tmax(j))
        if (nspts(j).gt.maxpts) maxpts = nspts(j)
 2    continue      
c
c find event with most seismograms and compute vertical scale
c so all seismograms for this event will fit in a single column
c (i.e. find y dimension for each trace)
c
      maxseis = 0
      do 5 i=1,numevt
 5      if (nspevt(i).gt.maxseis) maxseis = nspevt(i)
      ylen = ytot / maxseis
c
c loop over all the events
      k = 0
      x1   = x0
      do 10 i=1,numevt
        if (icol.gt.ncol) then
           write(8,*) 'endframe'
           write(8,*) 'pause'
           write(8,*) 'beginframe'
           icol = 1
           x1   = x0
        endif
        itop = 1
        y1   = y0
c
c loop over the seismograms for this event
        do 20 j=1,nspevt(i)
          k = k + 1
          if (nspts(k) .eq. 0) goto 20
          xlen = nspts(k) * xmax / maxpts
          write(8,'(a,2f5.2)') 'xvport ',x1,x1+xlen
          write(8,'(a,2f5.2)') 'yvport ',y1-ylen,y1
      
          write(8,*) 'r ',jfile(k)(1:lnblnk(jfile(k)))
	   write(8,*) 'r more ',jfile(k)(1:lnblnk(jfile(k)))//'_TIDE'
          write(8,*) "ylabel ","'","&1,kstnm&","'"," size tiny"
          if (itop.eq.1) then
            itop = 0
            write(8,*) "title ","'","&1,kzdate&","'"," size tiny"
          else
            write(8,*) 'title off'
          endif
          write(8,*) 'rmean'
          write(8,*) 'ylim ', 1.5*tmin(k), 1.5*tmax(k)
          write(8,*) 'p2'
ccc          write(8,*) 'pause'
          y1 = y1 - ylen
 20     continue
        x1 = x1 + xmax + xsep
        icol = icol + 1
 10   continue
      write(*,*) 'saccmd is finished'
      return
      end
