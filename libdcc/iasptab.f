      subroutine iasptab(depth,delta,ih,im,ss,sin_i)
c
c note use of real*8 arguments to subroutine getvel
      character*8 phase
      real*8 r,vel
      data pi/3.14159265358979e0/
      rad = 180.0 / pi
c
      sin_i = 0.0
      r = 6371.0 - depth
      call setupbuland(33,depth)
      call evalbuland(delta,narrivals)
c
c print table header
c
        write(6,"('Phase     Delta   T-time   time')")
c
c loop over all the phases
c
      do 10 i=1,narrivals
        call getbuland(i,ttime,pray,dtddep,ddeldp,phase)
c
c Get takeoff angle of P wave, so we can pass it to the beachball
c plotter.  Note that we get the ray param for the IASPEI model, but we
c are using PREM Vp's (from subroutine getvel) to compute the takeoff
c angle -- not strictly legal, but should be adequate for the purpose
c here.
c       
        if (phase.eq.'P       ') then
          call getvel(1,r,vel)
          sin_i = rad*pray*vel/r
cc          ang = rad * asin(sin_i)
cc          write(*,*) 'takeoff angle = ',ang
        endif
c
c       
        call cktime(ih,im,ss,ih2,im2,ss2,ttime)
        write(6,20) phase,delta,ttime,ih2,im2,ss2
         
 10     continue      
c
c for surface waves, use 360 degrees/10800 s (3 hours) as velocity
c 
      ttime = delta * 10800.0 / 360.0
      call cktime(ih,im,ss,ih2,im2,ss2,ttime)
      write(6,20) 'R1      ',delta,ttime,ih2,im2,ss2

      ttime = (360.0 - delta) * 10800.0 / 360.0
      call cktime(ih,im,ss,ih2,im2,ss2,ttime)
      write(6,20) 'R2      ',delta,ttime,ih2,im2,ss2
      
 20   format(a8,f7.2,f9.2,2x,i2,':',i2,':',f4.1)
      return
      end
