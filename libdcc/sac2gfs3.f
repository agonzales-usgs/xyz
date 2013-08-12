c
c-----------------------------------------------------------
c
      subroutine sac2gfs3(io,sacname,ky,kd,kh,km,sec,elat,elon,edep)
c Converts SAC files which are part of a SPYDER directory
c to GFS type 3 format.
c
c Gets event time and coords. from argument list (i.e. assumes you
c have read the SPYDER "summary" file to get EQ info).
c
c Reads station lat and long from SPYDER header in order to check these
c against our database.
c
c input:
c  io = GFS file unit number
c  sacname = name of SAC file to read
c  ky,kd,kh,km,sec = EQ time
c  elat,elon,edep  = EQ coords
c
      character sacname*80,ktemp*8,
     &          nsta*4, nchn*4, ntyp*4, place*50
      dimension head(30)
      common/myhed/nscan,nsta,nchn,ntyp,iy,id,ih,im,ss,dt,
     &             qlat,qlon,qdep,jy,jd,jh,jm,sss,spare(12)
      real*8 tt,ipoch8
      parameter (maxpts=1728000)
      real rdata(maxpts)
c      
      equivalence (head,nscan)
      data pi/3.14159265358979e0/
c
c get source time and location
c
      jy = ky
      jd = kd
      jh = kh
      jm = km
      sss = sec
      qlat = elat
      qlon = elon
      qdep = edep
c
c read SAC binary file
c
      call rsac1(sacname,rdata,nscan,beg,dt,maxpts,nerr)
      if (nerr.ne.0) then
        write(*,*) 'error reading SAC file: ',sacname(1:lnblnk(sacname))
        write(*,*) 'error code = ',nerr
ccc        pause
        return
      endif
c
c read the SAC header variables
c
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
      tt = ipoch8(iy,id,ih,im,ss+beg)
      call bipoch8(tt,iy,id,ih,im,ss)
c
      call getkhv('KSTNM',ktemp,nerr)
      nsta = ktemp(1:4)
      call getkhv('KCMPNM',ktemp,nerr)
      nchn = ktemp(1:4)

      ntyp = '    '
c
c Could get EQ coords from SAC header, but we use those
c from the calling arguments.
c
cc      call getfhv('EVLA',qlat,nerr)
cc      call getfhv('EVLO',qlon,nerr)
cc      call getfhv('EVDP',qdep,nerr)
cc      qdep = qdep / 1000.
c 
c check header station location against database
c
      call getfhv('STLA',stla,nerr)
      call getfhv('STLO',stlo,nerr)
      call upcase(nsta)
      call allsta2_new(nsta,'    ',0,0,t1,p1,tdum,tdum,place)
      if (t1.eq.9999.0 .and. p1.eq.9999.0) then
        write(*,*) 'WARNING - no (database) station coords for: ',nsta
      else
        stlat = t1 * 180.0 / pi
        stlat = 90.0 - stlat
        stlon = p1 * 180.0 /pi
        if (stlon.gt.180.0) stlon = stlon - 360.0
      
        if (abs(stla-stlat).gt.0.01 .or. abs(stlo-stlon).gt.0.01) then
          write(*,*) 'WARNING - station location discrepancy: ',nsta
          write(*,*) '  SAC header lat and long: ',stla,stlo
          write(*,*) '  database lat and long  : ',stlat,stlon
        endif
      endif
c
c write out GFS file
c
      call gfs_rwentry(io,head,rdata,0,'w')
c
      return
      end
