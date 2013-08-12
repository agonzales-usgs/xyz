c
c---------------------------------------------------------
c
      subroutine iasptim(depth,delta,ttime,phase)
      character*8 phase
c
      call setupbuland(33,depth)
      call evalbuland(delta,narrivals)
c
c we could loop over all the phases (narrivals), but for now
c we will just get the first arrival (force narrivals=1)
c
      narrivals = 1
      do 10 i=1,narrivals
        call getbuland(i,ttime,pray,dtddep,ddeldp,phase)
 10   continue      
      
      return
      end
