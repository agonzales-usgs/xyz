c
c----------------------------------------------------------------
c
      subroutine getstat(d,num,dmed,errmed,dmean,dstd,errmean,ierr)
c Computes statistics of array d.
c  input:
c    d   = array of length num
c    num = length of array d
c  output:
c    dmed    = median value of d
c    errmed  = error of median
c    dmean   = mean of array d
c    dstd    = standard deviation of array d
c    errmean = uncertainty of estimated mean of d
c    ierr    = error flag = 0 if no errors in input or output
c
      dimension d(1),e(10000)
      real*8 avg,var
c
      ierr = -1
      if (num.le.1) return
      if (num.gt.10000) then
        write(*,*) 'warning -- max dim of stat subroutine exceeded'
        return
      endif
c
      pts = float(num)
      do 5 i=1,num
    5 e(i)=d(i)
c
c get mean
      avg=0.d0
      do 10 i=1,num
   10 avg = avg + e(i)
      dmean = avg / pts
c
c get variance
      var = 0.d0
      do 20 i=1,num
   20 var = var + (e(i)-dmean)**2
      var = var / (pts-1.0)
c
c standard deviation of distribution
      dstd = dsqrt(var)
c
c error of mean
      errmean = dsqrt(var/pts)
c
c median
      call shell(e,num)
      j = num / 2
      if ( (j*2).eq.num ) then 
        dmed = ( e(num/2)+e(num/2+1) )/2.0
      else
        dmed = e((num/2) + 1)
      endif
c
c error of median (median absolute devitaion)
      do 30 i=1,num
   30 e(i) = abs(dmed-e(i))
      call shell(e,num)
      j = num / 2
      if ( (j*2).eq.num ) then 
        errmed = ( e(num/2)+e(num/2+1) )/2.0
      else
        errmed = e((num/2) + 1)
      endif
c
      ierr = 0
      return
      end
