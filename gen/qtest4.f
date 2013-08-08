      real*8 time1,time2
      character string*80

      ishift = 6561634
      
      write(*,*) 'enter time: yyyy ddd hh mm ss'
      read(*,*) iy1,id1,ih1,im1,iss1
      
      write(*,'(a,$)') 'enter time shift (in minutes)'
      write(*,*) ' (<ret> for current default of: ',ishift,')'
      read(*,'(a)') string
      if (lnblnk(string).gt.0) then
        read(string,*) ishift
      endif
      
      write(*,10) iy1,id1,ih1,im1,float(iss1)
      
      call dat2ep(iy1,id1,ih1,im1,float(iss1),time1)
      time2 = time1 + ishift * 60
      call ep2dat(time2,iy2,id2,ih2,im2,ss2)
      
      write(*,*) 'new time:'
      write(*,10) iy2,id2,ih2,im2,ss2
      
 10   format(i5,i4,i3,i3,f7.3)
      stop
      end
      
