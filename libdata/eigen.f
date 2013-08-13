c
c-------------------------------------------------------------------
c
      subroutine eigen(q,v,n,jvec)
c
c     computes eigenvalues and eigenvectors of a real symmetric matrix
c         using jacobi diagonalization
c
c     q=symmetric matrix - destroyed during computation and replaced by
c        a matrix with eigenvalues along diagonal
c     v=matrix of eigenvectors -  order corresponds to eigenvalues along
c        diagonal of q
c     n=order of q and v, n  greater or equal 2
c     jvec=0 to compute eigenvalues only
c         =non-zero integer to compute eigenvalues and eigenvectors
c
      dimension q(3,3),v(3,3),x(3),ih(3)
c
c      set initial values of v
c
      if(jvec) 10,15,10
   10 do 14 i=1,n
      do 14 j=1,n
      if(i-j) 12,11,12
   11 v(i,j)=1.0
      goto 14
   12 v(i,j)=0.0
   14 continue
   15 m=0
c
c     scan for largest off diagonal element in each row
c     x(i) contains largest element in ith row
c     ih(i) holds second subscript defining position of element
c
      mi=n-1
      do 30 i=1,mi
      x(i)=0.0
      mj=i+1
      do 30 j=mj,n
      if(x(i)-abs(q(i,j))) 20,20,30
   20 x(i)=abs(q(i,j))
      ih(i)=j
   30 continue
c
c     search for maximum of x(i)'s for pivot element
c
   40 do 70 i=1,mi
      if(i-1) 60,60,45
   45 if(xmax-x(i)) 60,70,70
   60 xmax=x(i)
      ip=i
      jp=ih(i)
   70 continue
c
c     test for xmax, if less than 10**-8 goto 1000
c
      epsi=1.0e-8
      if(xmax-epsi) 1000,1000,148
  148 m=m+1
c
c     compute tang,sin,cos,q(i,i),q(j,j)
c
      if(q(ip,ip)-q(jp,jp)) 150,151,151
c234567890123456789012345678901234567890123456789012345678901234567890
  150 tang=-2.*q(ip,jp)/(abs(q(ip,ip)-q(jp,jp))+sqrt((q(ip,ip)-q(jp,jp
     1  ))**2+4.*q(ip,jp)**2))
      goto 160
  151 tang=+2.*q(ip,jp)/(abs(q(ip,ip)-q(jp,jp))+sqrt((q(ip,ip)-q(jp,jp
     1  ))**2+4.*q(ip,jp)**2))
  160 cosn=1.0/sqrt(1.0+tang**2)
      sine=tang*cosn
      qii=q(ip,ip)
      q(ip,ip)=cosn**2*(qii+tang*(2.*q(ip,jp)+tang*q(jp,jp)))
      q(jp,jp)=cosn**2*(q(jp,jp)-tang*(2.*q(ip,jp)-tang*qii))
      q(ip,jp)=0.0
c
c     pseudo rank the eigenvalues
c
      if(q(ip,ip)-q(jp,jp)) 152,153,153
  152 temp=q(ip,ip)
      q(ip,ip)=q(jp,jp)
      q(jp,jp)=temp
c
c     adjust sin and cos for computation of q(i,k) and v(i,k)
c
      if(sine) 154,155,155
  154 temp=cosn
      goto 170
  155 temp=-cosn
  170 cosn=abs(sine)
      sine=temp
c
c     inspect the ih's between i+1 and n-1 to determine whether a new maximum
c     value suould be computed since the present maximum is in the i or j row
c
  153 do 350 i=1,mi
      if(i-ip) 210,350,200
  200 if (i-jp) 210,350,210
  210 if(ih(i)-ip) 230,240,230
  230 if(ih(i)-jp) 350,240,350
  240 k=ih(i)
      temp=q(i,k)
      q(i,k)=0.0
      mj=i+1
      x(i)=0.0
c
c     search for new maximum in depleted row
c
      do 320 j=mj,n
      if(x(i)-abs(q(i,j))) 300,300,320
  300 x(i)=abs(q(i,j))
      ih(i)=j
  320 continue
      q(i,k)=temp
  350 continue
      x(ip)=0.0
      x(jp)=0.0
c
c     change the other elements of q
c
      do 530 i=1,n
      if(i-ip) 370,530,420
  370 temp=q(i,ip)
      q(i,ip)=cosn*temp+sine*q(i,jp)
      if(x(i)-abs(q(i,ip))) 380,390,390
  380 x(i)=abs(q(i,ip))
      ih(i)=ip
  390 q(i,jp)=-sine*temp+cosn*q(i,jp)
      if(x(i)-abs(q(i,jp))) 400,530,530
  400 x(i)=abs(q(i,jp))
      ih(i)=jp
      goto 530
  420 if(i-jp) 430,530,480
  430 temp=q(ip,i)
      q(ip,i)=cosn*temp+sine*q(i,jp)
      if(x(ip)-abs(q(ip,i))) 440,450,450
  440 x(ip)=abs(q(ip,i))
      ih(ip)=i
  450 q(i,jp)=-sine*temp+cosn*q(i,jp)
      if(x(i)-abs(q(i,jp))) 400,530,530
  480 temp=q(ip,i)
      q(ip,i)=cosn*temp+sine*q(jp,i)
      if(x(ip)-abs(q(ip,i))) 490,500,500
  490 x(ip)=abs(q(ip,i))
      ih(ip)=i
  500 q(jp,i)=-sine*temp+cosn*q(jp,i)
      if(x(jp)-abs(q(jp,i))) 510,530,530
  510 x(jp)=abs(q(jp,i))
      ih(jp)=i
  530 continue
c
c     test for computation of eigenvectors
c
      if(jvec) 540,40,540
  540 do 550 i=1,n
      temp=v(i,ip)
      v(i,ip)=cosn*temp+sine*v(i,jp)
  550 v(i,jp)=-sine*temp+cosn*v(i,jp)
      goto 40
 1000 return
      end
