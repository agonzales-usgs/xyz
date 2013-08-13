      subroutine filtsp(a,len,b,nlen,ns,w,nw,nd,n1,n2,np,flag)
c  filters series a(1..len) into b(1...nlen) using filter wts
c  in w(1..nw), nd is the decimation factor. The panel structure
c  in n1(1..np),n2(1..np) is accounted for in the convolution
c  ns is the number of points removed from front of original
c  series to account for convolution length and can be used for
c  updating the header.
      implicit real*8(a-h,o-z)
      real*4 a,b
      dimension a(1),b(1),n1(1),n2(1),w(1)
      nlen=(len-nw)/nd+1
      m1=-nd
      do 10 n=1,nlen
      m1=m1+nd
      sum=0.d0
      do 40 i=1,nw
      j=m1+i
   40 sum=sum+a(j)*w(i)
   10 b(n)=sum
      ns=nw/2
      l1=ns+1-nd
      do 25 n=1,nlen
      l=n*nd+l1
      m1=l-ns
      m2=l+ns
      ip=1
   15 if(l.ge.n1(ip).and.l.le.n2(ip)) go to 20
      ip=ip+1
      if(ip.le.np) go to 15
      b(n)=flag
      go to 25
   20 if(m1.lt.n1(ip).or.m2.gt.n2(ip))b(n)=flag
   25 continue
      return
      end
