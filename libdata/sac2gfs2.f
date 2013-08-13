c
c-----------------------------------------------------------
c
      subroutine sac2gfs2
c
c Converts SAC files to GFS type 1 format.
c Prompts user for GFS output file name and SAC input file names.
c Will keep prompting for input names until a blank is entered.
c
      parameter (maxpts=864000)
      character sacname*80,gname*80,ktemp*8,
     &          nsta*4, nchn*4, ntyp*4
      dimension rdata(maxpts),head(30)
      common/myhed/nscan,nsta,nchn,ntyp,iy,id,ih,im,ss,dt,
     &             qlat,qlon,qdep,jy,jd,jh,jm,sss,spare(12)
      logical flag
      real*8 tt
      equivalence (head,nscan)
c
      itype = 1
      call gfs_opena(1,'GFS output filename: ',itype,iret)
c
      do 4 j=1,80
          sacname(j:j) = ' '
 4    continue
 5    sacname = gname('input SAC file (<ret> to quit): ')
      ilen = lnblnk(sacname)
      if (ilen.eq.0) goto 100
c
c read SAC binary file
c
      call rsac1(sacname,rdata,nscan,beg,dt,maxpts,nerr)
      if (nerr.ne.0) then
        write(*,*) 'ERROR - reading SAC file: ',sacname(1:ilen)
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
      ntyp = '    '
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
      goto 5
c
 100  call gfs_close(1)
      return
      end
