      subroutine do_filt(rdata,npts,dt,ifil,fl,fh,ns,ierr)
c
c Actually does filtering on array rdata().
c
c input:
c  rdata() = array to be filtered
c  npts  = length of dat()
c  dt    = sample interval
c  ifil  = filter type (1=lp, 2=hp, 3=bp)
c  fl,fh = low and high freq corners
c  ns    = number of segments (# of poles/2)
c 
c output:
c  dat() = filtered array
c  ierr  = 0 if no errors
c
      dimension rdata(*),a(5),b(5),c(5),d(5),e(5)
      dimension graf(2,21)
      
      ierr = 0
c
c design then do filter
c
      if (ifil.eq.1) then
        if (fl.ge.0.5/dt) then
          ierr = -1
          return
        endif
        call demean(rdata,npts)
        call lpdes(fl,dt,ns,a,b,c,graf)
        call butter_lp(rdata,npts,ns,a,b,c)
        
      elseif (ifil.eq.2) then
        if (fh.ge.0.5/dt) then
          ierr = -1
          return
        endif
        call demean(rdata,npts)
        call hpdes(fh,dt,ns,a,b,c,graf)
        call butter_hp(rdata,npts,ns,a,b,c)
        
      elseif (ifil.eq.3) then
        if (fl.ge.fh .or. fh.ge.0.5/dt) then
          ierr = -1
          return
        endif
        call demean(rdata,npts)
        call bpdes(fl,fh,dt,ns,a,b,c,d,e,graf)
        call butter_bp(rdata,npts,ns,a,b,c,d,e)
        
      elseif (ifil.eq.4) then
        call demean(rdata,npts)
        call spwwss(rdata,npts,dt,ierr)
        
      elseif (ifil.eq.5) then
        call demean(rdata,npts)
        call lpwwss(rdata,npts,dt,ierr)
        
      elseif (ifil.eq.6) then
        call demean(rdata,npts)
        call lpsro(rdata,npts,dt,ierr)
        
      elseif (ifil.eq.7) then
        call demean(rdata,npts)
        call spnotch(rdata,npts,dt,ierr)

      endif
c
      return
      end
