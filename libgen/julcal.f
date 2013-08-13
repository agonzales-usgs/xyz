c
c----------------------------------------------------------------------
c
      subroutine julcal(iy,jd,im,id)
c
c input:
c  iy = year
c  jd = julian day
c output:
c  im = month
c  id = day of month
c
      dimension md(12),nd(12)
      data md/31,28,31,30,31,30,31,31,30,31,30,31/
      data nd/31,29,31,30,31,30,31,31,30,31,30,31/
      id = jd
      ly = mod(iy,4)
      im = 1 
      if (ly.eq.0) then
         do 35 i=1,12               
            if (id.gt.nd(i)) then
              im = im+1
              id = id-nd(i)
            else
              return         
            endif
   35    continue
      else                
         do 40 i=1,12
            if (id.gt.md(i)) then
              im = im+1
              id = id-md(i)
            else
              return
            endif
   40    continue
      endif
      return
      end
