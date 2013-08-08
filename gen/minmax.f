c-----------------------------------------------------------------
      subroutine minmax(buf,n,rmin,rmax)
c  find max and min of a buffer
      dimension buf(*)
      if (n.le.0) then
        rmin = 0.0
        rmax = 0.0
        return
      endif
      rmin = buf(1)
      rmax = buf(1)
      do 5 i=2,n
        rmax=amax1(rmax,buf(i))
        rmin=amin1(rmin,buf(i))
    5 continue
      return
      end
