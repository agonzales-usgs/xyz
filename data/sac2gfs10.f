c
c-----------------------------------------------------------
c
      subroutine sac2gfs10
c
c Converts SAC files to GFS type 10 format.
c Prompts user for GFS output file name and SAC input file names.
c Will keep prompting for input names until a blank is entered.
c
      parameter (maxpts=864000)
      character string*256, string2*256,sacname*80,gname*80,ktemp*8,
     &          nsta*4, nchn*4, nnet*4, nloc*4,sq*1,d2*2,
     &          item*20
      dimension rdata(maxpts),head(100)
      common/myhed/nscan,nnet,nsta,nloc,nchn,iy,id,ih,im,ss,dt,
     &             qlat,qlon,qdep,jy,jd,jh,jm,sss,spare(81)
      logical flag
      real*8 tt
      equivalence (head,nscan)
c
      itype = 10
      sq = "'"
      call gfs_opena(1,'GFS output filename: ',itype,iret)
c
 5    sacname = gname('input SAC file (<ret> to quit): ')
      ilen = lnblnk(sacname)
      if (ilen.eq.0) goto 100
      do 6 j = 1,256
          string(j:j) = ' '
 6    continue
      nloc = '    '
      nnet(1:2) = sacname(24:25)
      nstart = 1
      do 13 i=1,9
        call nextdot(sacname,item,nstart,nlen)
c       print*,sacname(1:ilen)
c       print*,'item= ',item(1:nlen)
 13   continue
      if(item(2:2).ne.'H') nloc(1:2) = item(1:nlen)
c     print*,'item= ',item(1:nlen)
        
c     string = 'echo '//sacname(1:ilen)//' | awk -F. '//sq//
c    *'{print $9}'//sq//' >! tmp'
c     string2 = 'wc tmp | awk '//sq//'{print $2}'//sq//
c    *' >! tmp2'
c     jlen = lnblnk(string)
c     ierr =  system(string(1:jlen))
c     jlen2 = lnblnk(string2)
c     ierr =  system(string2(1:jlen2))
c     open(7,file='tmp2')
c     read(7,'(a)') d2
c     close(7)
c     if(d2.ne.'0') then
c       open(7,file='tmp')
c       read(7,'(a)') nloc(1:2)
c       close(7)
c     endif

c
c read SAC binary file
c
      call rsac1(sacname,rdata,nscan,beg,dt,maxpts,nerr)
      if (nerr.ne.0) then
        write(*,*) 'ERROR - reading SAC file: ',sacname(1:ilen)
      do 7 j=1,80
          sacname(j:j) = ' '
 7    continue
        goto 5
      endif
      call getlhv('LEVEN',flag,nerr)
      call getnhv('NZYEAR',iy,nerr)
      call getnhv('NZJDAY',id,nerr)
      call getnhv('NZHOUR',ih,nerr)
      call getnhv('NZMIN', im,nerr)
      call getnhv('NZSEC', isec,nerr)
      call getnhv('NZMSEC',msec,nerr)
      ss = float(isec) + float(msec) / 1000.
c
c The start of the data is "beg" seconds after the
c time "iy,id:ih:im:ss".
c So, we convert time to epochal then back again
c to put the actual start time into "iy,id:ih:im:ss".
c
      call dat2ep(iy,id,ih,im,ss+beg,tt)
      call ep2dat(tt,iy,id,ih,im,ss)
c
      call getkhv('KSTNM',ktemp,nerr)
      nsta = ktemp(1:4)
ccc      call getfhv('STLA',stla,nerr)
ccc      call getfhv('STLO',stlo,nerr)
ccc      call getfhv('STEL',stele,nerr)
ccc      call getfhv('STDP',stdep,nerr)
      call getkhv('KCMPNM',ktemp,nerr)
      nchn = ktemp(1:4)
c
c we are using itype = 1, so we will assume that the
c EQ fields are not filled in.
c      
cc      qlat  = 0.0
cc      qlon  = 0.0
cc      qdep  = 0.0
cc      delta = 0.0
cc      jy = 0
cc      jd = 0
cc      jh = 0
cc      jm = 0
cc      sss = 0.0
cc      call getfhv('EVLA',qlat,nerr)
cc      call getfhv('EVLO',qlon,nerr)
cc      call getfhv('EVDP',qdep,nerr)
cc      qdep = qdep / 1000.
cc      call getfhv('GCARC',delta,nerr)
c
c write out GFS file
c
      call gfs_rwentry(1,head,rdata,0,'w')
      do 4 j=1,80
          sacname(j:j) = ' '
 4    continue
      goto 5
c
 100  call gfs_close(1)
      return
      end


      subroutine nextdot(line,item,nstart,nlen)
c*** return an item in a list of items in a line separated
c*** by blank spaces. nstart points at the beginning of the
c*** item you want (should initially be 1) and is incremented
c*** so that the next call to next will give the next item!
c*** nlen is the character length of item.
      character*(*) line,item
      jchar=len(line)
      nlen=0
      do 5 i=nstart,jchar
        k=i
        if(line(i:i).eq.' ') goto 5
        if(line(i:i).ne.'.') goto 10
    5   continue
      return
   10 do 20 i=k,jchar
        if(line(i:i).eq.'.'.or.line(i:i).eq.' ') goto 30
   20   nlen=nlen+1
   30 do 40 i=1,nlen
        j=k+i-1
   40   item(i:i)=line(j:j)
      nstart=k+nlen
      return
      end
