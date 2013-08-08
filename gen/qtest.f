      real*8 ipoch8,time
      iy = 1996
      id = 13
      ih = 13
      im = 5
      ss = 37.629
      
      write(*,10) iy,id,ih,im,ss
      
      call dat2ep(iy,id,ih,im,ss,time)
      write(*,*) 'time = ',time
      
      time = time + 1896 * 100.0
      
      call ep2dat(time,iy,id,ih,im,ss)
      write(*,10) iy,id,ih,im,ss
      
 10   format(i5,i4,i3,i3,f7.3)
      stop
      end
      
