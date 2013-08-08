c
c-----------------------------------------------------------
c     
      subroutine bdf2sac
c Converts ASL "bdf" files to SAC binary format.  You can overwrite the
c input file if desired.
c
      parameter (maxpts=1000000)
      character label*4,string*80,gname*80,ktemp*8,
     &          nsta*4, nchn*4, sacname*80, bdfname*80
      dimension rdata(maxpts)
      logical exists
c
c
 1    bdfname = gname('input .bdf file name: ')
      inquire(file=bdfname,exist=exists)
      if(.not.exists) then
        write(*,*) 'input file does not exist'
        goto 1
      endif
      open(8,file=bdfname,status='old')
 10   format(a4,a80)
c
c note that TIME, NSAM, and DATA can appear more than one in 
c a bdf file.
c 
      nscan = 0
      itime = 0
  5   read(8,10,end=20) label,string
      if (label.eq.'STA ') nsta = string(1:4)
      if (label.eq.'COMP') nchn = string(1:4)
      if (label.eq.'LATT') read(string,*) stla
      if (label.eq.'LONG') read(string,*) stlo
      if (label.eq.'ALTI') read(string,*) stele
      if (label.eq.'DEPT') read(string,*) stdep
      if (label.eq.'RATE') read(string,*) rate
      if (label.eq.'DIP ') read(string,*) dip
      if (label.eq.'AZIM') read(string,*) azim
      if (label.eq.'CHNK') read(string,*) ichnk
      if (label.eq.'TIME'.and.itime.eq.0) then
        read(string,'(i4,1x,i3,1x,i2,1x,i2,1x,f6.3)') iy,id,ih,im,ss
        itime = 1
      endif
      if (label.eq.'NSAM') read(string,*) nsam
      if (label.eq.'DATA') then
        if (nscan+nsam.gt.maxpts) then
          write(*,*) 'ERROR, cannot read ',nscan+nsam,' samples'
          pause 'too many points in bdf file'
        endif
        read(8,*) (rdata(i+nscan),i=1,nsam)
        nscan = nscan + nsam
        write(*,*) nsam
      endif
      goto 5
 20   close(8)
      if (ichnk.gt.1) then
        write(*,*) 'WARNING -- multiple data segments were merged'
        write(*,*) '           into a single time series'
      endif
c
c do some conversions
      dt = 1.0 / rate
      isec = ss
      msec = 1000. * float(int(ss) - isec)
      write(*,*) 'number of samples: ',nscan
      write(*,'(a12,i4,1x,i3,1x,i2,1x,i2,1x,f6.3)') 'start time: ',
     &      iy,id,ih,im,ss
c
c write out SAC binary file
c
      sacname = gname('output file: ')
      call newhdr
      call setnhv('NPTS',nscan,nerr)
      call setfhv('B',0.0,nerr)
cc      call setfhv('E',0.0,nerr)
      call setihv('IFTYPE','ITIME',nerr)
      call setlhv('LEVEN',.true.,nerr)
      call setfhv('DELTA',1.0/rate,nerr)
      call setfhv('CMPAZ',azim,nerr)
      call setfhv('CMPINC',dip+90.0,nerr)
      call setnhv('NZYEAR',iy,nerr)
      call setnhv('NZJDAY',id,nerr)
      call setnhv('NZHOUR',ih,nerr)
      call setnhv('NZMIN', im,nerr)
      call setnhv('NZSEC', isec,nerr)
      call setnhv('NZMSEC',msec,nerr)
      call setfhv ('O',0,nerr)
      call setihv('IZTYPE','IO',nerr)
cc      write(ktemp,'(2a4)') ntyp,'    '
cc      call setkhv('KNETWK',ktemp,nerr)
      write(ktemp,'(2a4)') nsta,'    '
      call setkhv('KSTNM',ktemp,nerr)
      call setfhv('STLA',stla,nerr)
      call setfhv('STLO',stlo,nerr)
      call setfhv('STEL',stele,nerr)
      call setfhv('STDP',stdep,nerr)
      write(ktemp,'(2a4)') nchn,'    '
      call setkhv('KCMPNM',ktemp,nerr)

cc      call setfhv('EVLA',qlat0,nerr)
cc      call setfhv('EVLO',qlon0,nerr)
cc      call setfhv('DIST',delta0,nerr)
      call setlhv('LOVROK',.false.,nerr)
      call wsac0(sacname(1:lnblnk(sacname)),rdata,rdata,nerr)
      if (nerr.ne.0) pause 'error writing SAC file'
c
      return
      end
