c
c--------------------------------------------------------------
c
      subroutine shell(a,ncol)
c  shell sorts a(nc) into increasing order
      dimension a(ncol)
      ig=ncol
    5 if(ig.le.1) return
      ig=ig/2
      im=ncol-ig
   10 iex=0
      do 20 i=1,im
      ipg=i+ig
      if(a(i).le.a(ipg)) go to 20
      te=a(i)
      a(i)=a(ipg)
      a(ipg)=te
      iex=iex+1
   20 continue
      if(iex.gt.0) go to 10
      go to 5
      end
