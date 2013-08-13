      subroutine eival1(valuea,dipa,azima,xm)
      dimension xm(6),azima(3),dipa(3),valuea(3)
      data rad/0.01745329/
      do 10 i=1,6
   10 xm(i)=0.
      do 20 j=1,3
      azim=azima(j)
      dip=dipa(j)
      value=valuea(j)
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
