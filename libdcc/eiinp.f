      subroutine eiinp(xm)
      dimension xm(6)
      data rad/0.01745329/
      do 10 i=1,6
   10 xm(i)=0.
      do 20 n=1,3
        write(6,101) n
  101   format('eigenvector',i2,/,
     .  'type value/10**25, dip ,azimuth (* format)')
        read(5,*) value,dip,azim
  102   format(e10.2,f5.2,f7.2)
        value=value*10.**25
        write(6,102) value,dip,azim
        eta=dip*rad
        xsi=azim*rad
        xm(1)=xm(1)+value*(sin(eta)**2)
        xm(2)=xm(2)+value*((cos(eta)*cos(xsi))**2)
        xm(3)=xm(3)+value*((cos(eta)*sin(xsi))**2)
        xm(4)=xm(4)+0.5*value*sin(2.*eta)*cos(xsi)
        xm(5)=xm(5)-0.5*value*sin(2.*eta)*sin(xsi)
        xm(6)=xm(6)-0.5*value*(cos(eta)**2)*sin(2.*xsi)
   20 continue
      return
      end
