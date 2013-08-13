      subroutine panlsp(a,len,n1,n2,np)
c   find gaps of bad data, previously flagged, zero them and index them.
c   the resulting time series consists of np panels of good data indexed
c   from n1(i) to n2(i), i=1 to np.
      dimension a(1),n1(1),n2(1)
      data zero/0./,badun/3.5e6/
      id=2
      np=0
      do 30 i=1,len
      go to (15,20),id
   15 if(a(i).lt.badun) goto 30
      a(i)=zero
      n2(np)=i-1
      id=2
      go to 30
   20 if(a(i).gt.badun) goto 25
      np=np+1
      n1(np)=i
      id=1     
      goto 30
   25 a(i)=zero
   30 continue
      if(id.eq.2) return
      n2(np)=len
      return
      end

