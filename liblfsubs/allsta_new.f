      subroutine allsta_new(nsta,nnet,iy,id,stla,stlo,elev,depth,place)
c
c accesses a file to obtain station coordinates and place name
c
c input:
c  nsta = station name (i*4)
c  nnet = network code (i*4) optional
c  iy   = year         (i*4) optional
c  id   = day          (i*4) optional
c
c output:
c  stla  = station colatitude (radians)
c  stlo  = station longitude (radians, 0 to 2*pi)
c  elev  = elevation (meters)
c  depth = burial depth (meters)
c  place  = place name   char*50
c
c
      character*4 nsta,nnet,namest,namenet
      character*50 nmplace,place
      parameter (mxsta=1000)
      common/stdtXXX/numsta,namest(mxsta),namenet(mxsta),nmplace(mxsta),
     &  idate(mxsta,4),slat(mxsta),slon(mxsta),selev(mxsta),sdep(mxsta)
      data iflag /0/
      data rad/.0174532/
      
      stla  = 9999.
      stlo  = 9999.
      elev  = 9999.
      depth = 9999.
      place = ''
c
c read the station list file, if necessary
c
      if (iflag.eq.0) then
        iflag = 1
        call stalist
      endif
c
c find all entries with matching station name
c
      ibeg = 0
      iend = 0
      do 40 i=1,numsta
        if (nsta .eq. namest(i)) then
          if (ibeg.eq.0) ibeg = i
          iend = i
        endif          
 40   continue
c
c if no matches, get station location from user and return
c
      if (ibeg.eq.0) then
        write(6,"('station : ',a4,' not found')") nsta
        write(*,'(a,X)') 'enter lat and long (0, 0 to bypass): '
        read(*,*) stla, stlo
        if (stla.eq.0.0 .and. stlo.eq.0.0) return
        stla = (90.0 - stla) * rad
        if (stlo.lt.0.0) stlo = stlo + 360.0
        stlo = stlo * rad
        return
      endif
c
c check network name
c 
      if (nnet(1:2).ne.'  ') then
c not implemented yet
      endif          
c
c check date
c       
      if (iy.ne.0 .and. id.ne.0) then
c not implemented yet
      endif
      
c at present, simply take the most recent station location
      
 50   stla  = (90.0 - slat(iend)) * rad
      stlo  = slon(iend)
      if (stlo.lt.0.0) stlo = stlo + 360.0
      stlo  = stlo  * rad
      elev  = selev(iend)
      depth = sdep(iend)
      place = nmplace(iend)
      return
      end
c
c----------------------------------------------------------------
c
      subroutine allsta2_new(nsta,nnet,iy,id,stla,stlo,elev,depth,place)
c
c same as allsta_new, except if it doesn't find a station it does a
c return with an error condition, rather than prompting user for station
c coords.
c
c accesses a file to obtain station coordinates and place name
c
c input:
c  nsta = station name (i*4)
c  nnet = network code (i*4) optional
c  iy   = year         (i*4) optional
c  id   = day          (i*4) optional
c
c output:
c  stla  = station colatitude (radians)
c  stlo  = station longitude (radians, 0 to 2*pi)
c  elev  = elevation (meters)
c  depth = burial depth (meters)
c  place  = place name   char*50
c
c
      character*4 nsta,nnet,namest,namenet
      character*50 nmplace,place
      parameter (mxsta=1000)
      common/stdtXXX/numsta,namest(mxsta),namenet(mxsta),nmplace(mxsta),
     &  idate(mxsta,4),slat(mxsta),slon(mxsta),selev(mxsta),sdep(mxsta)
      data iflag /0/
      data rad/.0174532/
      
      stla  = 9999.
      stlo  = 9999.
      elev  = 9999.
      depth = 9999.
      place = ''
c
c read the station list file, if necessary
c
      if (iflag.eq.0) then
        iflag = 1
        call stalist
      endif
c
c find all entries with matching station name
c
      ibeg = 0
      iend = 0
      do 40 i=1,numsta
        if (nsta .eq. namest(i)) then
          if (ibeg.eq.0) ibeg = i
          iend = i
        endif          
 40   continue
c
c if no matches, do an error exit, rather than getting station location
c from user and return
c
      if (ibeg.eq.0) then
        return
      endif
c
c check network name
c 
      if (nnet(1:2).ne.'  ') then
c not implemented yet
      endif          
c
c check date
c       
      if (iy.ne.0 .and. id.ne.0) then
c not implemented yet
      endif
      
c at present, simply take the most recent station location
 50   stla  = (90.0 - slat(iend)) * rad
      stlo  = slon(iend)
      if (stlo.lt.0.0) stlo = stlo + 360.0
      stlo  = stlo  * rad
      elev  = selev(iend)
      depth = sdep(iend)
      place = nmplace(iend)
      return
      end
c
c----------------------------------------------------------------
c
      subroutine stalist
c
c Actually reads the file containing station coords, etc.. File contents
c are stored in the common block "stdtXXX".
c
c
      character*4 namest,namenet
      character*50 nmplace
      character dbsdir*80,station_file*256
      logical ex
      parameter (mxsta=1000)
      common/stdtXXX/numsta,namest(mxsta),namenet(mxsta),nmplace(mxsta),
     &  idate(mxsta,4),slat(mxsta),slon(mxsta),selev(mxsta),sdep(mxsta)
c
c construct file name and open file
c
      call getenv('HRVDBS',dbsdir)
c
c get station file
c      
      station_file = dbsdir(1:lnblnk(dbsdir))//
     &   '/gsn_sta_list'
      inquire(file=station_file,exist=ex)
      if (.not.ex) stop 'ERROR - no station database file'
c
      open(99,file=station_file)

      do 5 j=1,4
        do 5 i=1,mxsta
 5        idate(i,j) = 0
        ii = 1
c
c read through file
c
 10   read(99,20,err=10,end=30) namest(ii),namenet(ii),nmplace(ii),
     &  (idate(ii,j),j=1,4), slat(ii),slon(ii),selev(ii),sdep(ii)

 20   format(a4,3x,a2,2x,a50,i4,1x,i3,2x,i4,1x,i3,1x,4f11.0)
      ii = ii + 1
      if (ii.ge.mxsta) then
        write(*,*) 'ERROR - mxsta exceeded in subroutine stalist'
        goto 30
      endif
      goto 10
c
c finish up
c
 30   numsta = ii - 1
      close(99)
ccc      do 31 ii=1,numsta
ccc 31     write(*,21) namest(ii),namenet(ii),nmplace(ii),
ccc     &  (idate(ii,j),j=1,4), slat(ii),slon(ii),selev(ii),sdep(ii)
ccc 21   format(a4,3x,a2,2x,a50,i4,1x,i3,2x,i4,1x,i3,1x,4f11.3)

      return
      end
c
c-----------------------------------------------------
c
      subroutine getnetcode(nsta,ntyp)
c
c Assigns a network code to a given station name.
c Reads the station database file, which is ordered chronologically.
c Chooses most recent network code for the given station
c
c input:
c   nsta = station name (char*4)
c   ntyp = network code on input (char*4) -- usually blank
c
c output:
c   ntyp = two letter network code
c
c notes:
c   If ntype is not blank on input, it will be compared to the 
c   network code that is found. If the two netcodes disagree a 
c   warning is printed and the user's netcode is returned unchanged.
c
c
      character*4 nsta,ntyp,nnet,namest,namenet
      character*50 nmplace
      parameter (mxsta=1000)
      common/stdtXXX/numsta,namest(mxsta),namenet(mxsta),nmplace(mxsta),
     &  idate(mxsta,4),slat(mxsta),slon(mxsta),selev(mxsta),sdep(mxsta)
      data iflag /0/
      
      call rmnull(ntyp)
      call rmblnk(ntyp)
c
c read the station list file, if necessary
c
      if (iflag.eq.0) then
        iflag = 1
        call stalist
      endif
c
c find last (most recent) entry for this station
c
      indx = 0
      do 40 i=1,numsta
        if (nsta .eq. namest(i)) indx = i
 40   continue
      
      if (indx.eq.0) then
        write(0,*) 'WARNING: station not found in station list'
      else
        nnet = namenet(indx)
        if (ntyp.ne.'    ' .and. nnet.ne.ntyp) then
          write(0,*) 'WARNING: disagreement on network type'
          write(0,*) ntyp,' versus ',nnet
          return
        endif
        ntyp = nnet
      endif
      
      return
      end
c
c----------------------------------------------------------------
c
      subroutine allsta_isc (stanam,t1,p1,istnum,elev)
c Returns ISC station info
c input:
c  stanam = 4 character station name
c output:
c  t1,p1  = station colat and colon in radians
c  istnum = station number
c  elev   = elevation (units?)
c
      parameter (numsta=3368)
      integer*4 istdat(numsta),istdatb(numsta)
      character*4 stanam,buf,buffer(numsta)
      character*80 dbsdir
      dimension ielev(4000),sdata(4000,4)
      equivalence (buffer,istdatb),(buf,ista)
      logical getsta
      save getsta,ielev,sdata,istdat
      data getsta/.true./,rad/57.29579/
c
c read disk file if necessary
      if (getsta) then
        getsta=.false.
        call getenv('HRVDBS',dbsdir)
        open (99,file=dbsdir(1:lnblnk(dbsdir))//'/isc.sta.list')
        do 51 i=1,3368
          read (99,1,end=909) buffer(i),(sdata(i,j),j=1,2),ielev(i)
    1     format (a4,2f11.5,i5:i6)
   51   continue
  909   close (99)
        do 200 i=1,numsta
          istdat(i) = istdatb(i)
  200   continue
      endif
c
c now get station info
      buf = stanam
      ii  = numsta/2
      int = ii
      do 300 i=1,12
      if (istdat(ii).eq.ista) then
       stlat  = sdata(ii,1)
       stlong = sdata(ii,2)
       elev   = ielev(ii)
       istnum = ii
       t1 = (90.0 - stlat) / rad
       p1 =  stlong
       if (p1.lt.0.0) p1 = p1 + 360.0
       p1 = p1 / rad
       if (stlat.eq.0.0.and.stlong.eq.0.0) goto 400
       return
      else if (istdat(ii).gt.ista) then
       ii=max(ii-int/2,1)
      else
       ii=min(ii+int/2,numsta)
      endif
      int=int/2+1
  300 continue
c
  400 stlat  = 9999.0
      stlong = 9999.0
      istnum = 9999.0
      return
      end
