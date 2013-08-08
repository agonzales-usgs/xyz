c
c---------------------------------------------------------
c
      subroutine readcmt
c
c Calls readallcmt to read the comprehensive listing of CMT's:
c allorder.bin. The file allorder.bin" is prepared by the program dek2bin
c and contains both the CMT's from the "allorder.dek" file as well as
c CMT's from e-mail.
c
c One can then obtain the indices of the EQ's which fall within a
c desired date range using the subroutine "siftcmt".
c
c One can then get the info for a specific EQ by accessing the common
c block directly, or, the preferred way is to call getcmt and pass it the
c index of the EQ you want (this way your calling program does not need
c to declare the common block).
c
      parameter (maxevts=40000)
      common/eqinfo/inum,jy(maxevts),jd(maxevts),
     & jh(maxevts),jm(maxevts),ess(maxevts),eqlat(maxevts),
     & eqlon(maxevts),eqdep(maxevts),eqmb(maxevts),eqms(maxevts),
     & eqmw(maxevts)
c     
      inum = 0
      call readallcmt
      return
      end
c
c---------------------------------------------------------
c
      subroutine readcmt_slow
c
c This calls the rotuine to read the ".dek" version of the big catalog
c and the routine to read the quick CMT's from e-mail.
c
c Assumes that the big CMT catalog (read by readallcmt)
c is in chronological order. Thus, after calling this routine
c we can look at the last EQ in eqinfo and not bother to get
c any events before this date out of readqcmt.
c
      parameter (maxevts=40000)
      common/eqinfo/inum,jy(maxevts),jd(maxevts),
     & jh(maxevts),jm(maxevts),ess(maxevts),eqlat(maxevts),
     & eqlon(maxevts),eqdep(maxevts),eqmb(maxevts),eqms(maxevts),
     & eqmw(maxevts)
c     
      inum = 0
      call readallcmt_dek
      call readqcmt(jy(inum),jd(inum))
      return
      end
c
c---------------------------------------------------------
c
      subroutine readallcmt
c
c Reads the ".bin" version of the entire big CMT catalog. 
c
c  NB: inum should be set appropriately before calling
c      (i.e. inum = 0 if nothing in eqinfo common block,
c       otherwise set to proper value)
c
      character dbsdir*80,cmt_file*256
      parameter (maxevts=40000)
      common/eqinfo/inum,jy(maxevts),jd(maxevts),
     & jh(maxevts),jm(maxevts),ess(maxevts),eqlat(maxevts),
     & eqlon(maxevts),eqdep(maxevts),eqmb(maxevts),eqms(maxevts),
     & eqmw(maxevts)
      logical ex
      data iflag/0/
      
      if (iflag.ne.0) return
c     
c construct file name and open file
c
      call getenv('HRVDBS',dbsdir)
      cmt_file = dbsdir(1:lnblnk(dbsdir))//'/allorder.bin'
      inquire(file=cmt_file,exist=ex)
      if (.not.ex) then
        write(0,*) 'ERROR in readallcmt_bin -- cannot open file'
        stop
      endif
      iflag = 1
c
c read through file
c
      open(20,file=cmt_file,form='unformatted')
      inum_beg = inum
      i = inum + 1
   5  read(20,end=99) jy(i),jd(i),jh(i),jm(i),ess(i),eqlat(i),
     &            eqlon(i),eqdep(i),eqmb(i),eqms(i),eqmw(i)
c
      i = i + 1
      goto 5
c
 99   continue
      inum = i - 1
      close(20)
      write(*,*) ' '
      write(*,*) '  # of events in big CMT catalog: ',inum-inum_beg
      write(*,*) ' '
100   continue
      return
      end
c
c---------------------------------------------------------
c
      subroutine readallcmt_dek
c
c Reads the ".dek" version of the entire big CMT catalog.
c
c Typically, this is only used by the program that converts the
c .dek format to the .bin format. 
c
c  NB: inum should be set appropriately before calling
c      (i.e. inum = 0 if nothing in eqinfo common block,
c       otherwise set to proper value)
c
      character dbsdir*80,cmt_file*256
      parameter (maxevts=40000)
      common/eqinfo/inum,jy(maxevts),jd(maxevts),
     & jh(maxevts),jm(maxevts),ess(maxevts),eqlat(maxevts),
     & eqlon(maxevts),eqdep(maxevts),eqmb(maxevts),eqms(maxevts),
     & eqmw(maxevts)
      real*8 ept
      logical ex
      data iflag/0/
      
      if (iflag.ne.0) return
c     
c construct file name and open file
c
      call getenv('HRVDBS',dbsdir)
      cmt_file = dbsdir(1:lnblnk(dbsdir))//'/allorder.dek'
      inquire(file=cmt_file,exist=ex)
      if (.not.ex) then
        write(0,*) 'ERROR in readallcmt -- cannot open file'
        stop
      endif
c
c read through file
c
      iflag = 1
      open(20,file=cmt_file)
      inum_beg = inum
      i = inum + 1
   5  read(20,20,end=99) jmo,jmd,jy(i),jh(i),jm(i),
     &   ess(i),eqlat(i),eqlon(i),eqdep(i),eqmb(i),eqms(i)
      read(20,21,end=98) delt,err1,cmtlat,err2,cmtlon,err3,cmtdep
      read(20,22,end=98) dur,iex,(f,j=1,6)
      read(20,*,end=98) (x,m,n,j=1,3), xm,strike,dip,rake
      eqmw(i) = 2.0 * alog10(xm*10.0**iex) / 3.0 - 10.7
c
      if(jy(i).lt.50) jy(i) = jy(i) + 2000
      if(jy(i).lt.100.and.jy(i).ge.50) jy(i) = jy(i) + 1900
      call caljul(jy(i),jmo,jmd,jd(i))
c
c use centroid source time, lat, long, and depth
c
      call dat2ep(jy(i),jd(i),jh(i),jm(i),ess(i),ept)
      ept = ept + delt
      call ep2dat(ept,jy(i),jd(i),jh(i),jm(i),ess(i))
      eqlat(i) = cmtlat
      eqlon(i) = cmtlon
      eqdep(i) = cmtdep
c      
      i = i + 1
      if (i.gt.maxevts) then
        write(0,*) 'ERROR -- readallcmt_dek: too many events'
        stop
      endif
      goto 5
c
 20   format(9x,5(i2,1x),f4.1,f7.2,f8.2,f6.1,2f3.1)
 21   format(33x,f6.1,f4.1,f7.2,f5.2,f8.2,f5.2,f6.1)
 22   format(4x,f4.1,3x,i3,6(f6.2,5x))     
c 
 98   write(0,*) 'WARNING in readallcmt - encountered unexpected EOF'
 99   continue
      inum = i - 1
      close(20)
      write(*,*) ' '
      write(*,*) '  # of events in big CMT catalog: ',inum-inum_beg
      write(*,*) ' '
100   continue
      return
      end
c
c---------------------------------------------------------
c
      subroutine readqcmt(iy,id)
c
c Reads a portion of an e-mail folder containing Harvard quick CMT's. Only
c reads CMT's which are after the year and day specified. 
c This routine is really designed to just augment a complete
c CMT catalog by reading recent CMT's from e-mail.
c
c One can then extract the source parameters for a
c desired date range using the subroutine "siftcmt".
c
c input:
c
c  iy,id = year and julian day cutoff. Do not read CMT's
c          which fall before or on this date.
c
c NB: expects inum to be initialized to the appropriate value (i.e.
c     this routine will just append to eqinfo.
c
      character ins*1,iew*1,string*256
      parameter (maxevts=40000)
      common/eqinfo/inum,jy(maxevts),jd(maxevts),
     & jh(maxevts),jm(maxevts),ess(maxevts),eqlat(maxevts),
     & eqlon(maxevts),eqdep(maxevts),eqmb(maxevts),eqms(maxevts),
     & eqmw(maxevts)
      data iflag/0/
c  
c only read catalog if necessary 
c  
      if (iflag.ne.0) return
c
      iflag = 1
      open(20,file='/home/woodward/mail.dir/CMT')
      inum_beg = inum
 10   read(20,'(a)',end=99) string
      if (string(1:23).eq.'HARVARD EVENT-FILE NAME') then
        inum = inum + 1
        read(string,11,end=98) km,kd,jy(inum)
        if (jy(inum).ge.50.and.jy(inum).lt.100) jy(inum) = jy(inum) + 1900
        if (jy(inum).lt.50) jy(inum) = jy(inum) + 2000
        call caljul(jy(inum),km,kd,jd(inum))
        if (jy(inum).lt.iy .or. 
     &      (jy(inum).eq.iy .and. jd(inum).le.id) ) then
          inum = inum - 1
          goto 10
        endif
 20     read(20,'(a)',end=98) string
        if (string(1:11).ne.'ORIGIN TIME') goto 20
        read(string,12) jh(inum),jm(inum),ess(inum)
        read(20,'(a)',end=98) string
        read(string,13) eqlat(inum),ins,eqlon(inum),iew
        read(20,'(a)',end=98) string
        read(string,14) eqdep(inum)
        do 21 i=1,9
 21        read(20,'(a)',end=98) string
        read(string,15) eqm0,iexp
        eqmw(inum) = 2.0 * alog10(eqm0*10.0**iexp) / 3.0 - 10.7
        if (ins.eq.'S') eqlat(inum) = -1.0 * eqlat(inum)
        if (iew.eq.'W') eqlon(inum) = -1.0 * eqlon(inum)
        eqmb(inum) = 0.0
        eqms(inum) = 0.0
      endif
      if (inum.eq.maxevts) then
        write(0,*) 'ERROR -- readqcmt: too many cmts'
        stop
      endif
      goto 10
      
 11   format(25x,3i2)     
 12   format(18x,i2,1x,i2,1x,f4.1)
 13   format(3x,f6.2,a1,9x,f7.2,a1)
 14   format(3x,f6.1)
 15   format(22x,f3.1,5x,i2)
 
 98   write(0,*) 'ERROR in readcmt - encountered unexpected EOF'
 99   continue
      close(20)
      write(*,*) ' '
      write(*,*) '  # of events in quick CMT catalog: ',inum-inum_beg
      write(*,*) ' '
100   continue
      return
      end
c
c---------------------------------------------------------
c
      subroutine getcmt(indx,iy,id,ih,im,ss,qlat,qlon,qdep,qmb,qms,qmw)
c
c reads the specified event from the catalog stored in common "eqinfo".
c Returns the parameters for this event.
c      
      parameter (maxevts=40000)
      common/eqinfo/inum,jy(maxevts),jd(maxevts),
     & jh(maxevts),jm(maxevts),ess(maxevts),eqlat(maxevts),
     & eqlon(maxevts),eqdep(maxevts),eqmb(maxevts),eqms(maxevts),
     & eqmw(maxevts)
c
      if (indx.lt.1 .or. indx.gt.inum) then
        write(0,*) 'ERROR -- index in getcmt out of range'
        stop
      endif
      iy = jy(indx)
      id = jd(indx)
      ih = jh(indx)
      im = jm(indx)
      ss = ess(indx)
      qlat = eqlat(indx)
      qlon = eqlon(indx)
      qdep = eqdep(indx)
      qmb  = eqmb(indx)
      qms  = eqms(indx)
      qmw  = eqmw(indx)
      return
      end
      
