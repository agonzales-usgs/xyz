      real*8 time1,time2

      write(*,*) 'enter earlier time: yy ddd hh mm ss:'
      read(*,*) iy1,id1,ih1,im1,iss1
      
      write(*,*) 'enter later time: yy ddd hh mm ss:'
      read(*,*) iy2,id2,ih2,im2,iss2
      
      write(*,10) iy1,id1,ih1,im1,float(iss1)
      write(*,10) iy2,id2,ih2,im2,float(iss2)
      
      call dat2ep(iy1,id1,ih1,im1,float(iss1),time1)
      call dat2ep(iy2,id2,ih2,im2,float(iss2),time2)
      
      write(*,*) 'minutes difference: ',(time2-time1)/60.0
      
c      call ep2dat(time,iy,id,ih,im,ss)
c      write(*,10) iy,id,ih,im,ss
      
 10   format(i5,i4,i3,i3,f7.3)
      stop
      end
      
