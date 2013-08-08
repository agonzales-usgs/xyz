c
c
c
        subroutine timer
c    timing routine for the Hoffman Suns.
        character*24 date
      character*2 day,hour,min,sec
        dimension earray(2),darray(2)
      common/date0/iday0,ihour0,imin0,isec0,walttl,numdate
      if(numdate.eq.0) walttl=0.
        string=etime(earray)
        string2=dtime(darray)
        call fdate(date)
C..cputtl = total CPU time in seconds since process initiated
        cputtl=earray(1)
C..dcpu   = elapsed CPU time in seconds since last call to TIMER
        dcpu=darray(1)
c             compute elapsed wall time (dwal) and total wall time (walttl)
c                     first read date from character array date
      day=date(9:10)
      hour=date(12:13)
      min=date(15:16)
      sec=date(18:19)
c                     convert date to integer
      read(day,1) iday
      read(hour,1) ihour
      read(min,1) imin
      read(sec,1) isec
 1    format(i2)
c                     calculate elapsed wall time
      wal=isec+60.*(imin+60.*(ihour+24.*iday))
      if(numdate.eq.0) then 
       wal0=wal
      else
       wal0=isec0+60.*(imin0+60.*(ihour0+24.*iday0))
      endif

      dwal=wal-wal0
      walttl=walttl+dwal
c                     update values
      numdate=numdate+1
      iday0=iday
      ihour0=ihour
      imin0=imin
      isec0=isec
      write (*,10) date,dcpu,cputtl,dwal,walttl
10    format(a24,' CPU = [',F7.1,'/',F8.1,']    Wall = [',F6.0,'/',
     +f7.0,']')
      return
      end
