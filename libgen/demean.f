c-----------------------------------------------------------------
      subroutine demean(buf,n)
c remove mean from a buffer
c
      dimension buf(*)
      real*8 sum
      
      sum=0.0d0
      do 5 i=1,n
 5      sum = sum + buf(i)
      sum = sum / n
      do 10 i=1,n
 10     buf(i) = buf(i) - sum
      return
      end
