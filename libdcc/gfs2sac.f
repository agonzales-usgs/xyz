c
c--------------------------------------------------------------
c
	subroutine gfs2sac
C
C  This code will convert Harvard gfs files to sac files.
C
	parameter( maxscan=100000, lun=3)
        common/myhed/nscan,nsta,nchn,ntyp,iy,id,ih,im,ss,dt,
     &            slat,slon,sdep,iys,ids,ihs,ims,sss

	real*4	  	ss,dt,dat(maxscan),head(30)
	integer*4  	nscan,iy, id, ih, im, ilen
	character*4	nsta, nchn, ntyp
	character*80	sacfile
        equivalence (head,nscan)
        data pi/3.14159265358979/
        rad = 180.0 / pi

c
C   Ask the user for input file name
C
        itype = 3
        call gfs_opena(1,'enter gfs filename',itype,nentry)
        write(*,*) 'itype, nentry = ',itype,nentry
C
C   Read the data file
c
        call gfs_phdef(1)
        call gfs_sdef(1)
        indx = 0
 100    call gfs_search(1,head,indx,0)
        if (indx.lt.0) goto 990
	if (nscan .le. 0 .or. nscan .gt. maxscan) go to 900
	call gfs_rwdata(1,dat,indx,1,nscan,'r',ierr)
C
C
C   Create individual sacfile names by year,time,station,component
C   by first initializing sacfile to blanks, then fill in the blanks,
C   then, determine its total length, then relpace any blanks inside
C   its total length with zeros with internal write statements.
C
        do 120 j=1,80
          sacfile(j:j)=' '
 120    continue
         if (lnblnk(nsta) .eq. 4) then
          write(sacfile,200) iy,id,ih,im,ss,nsta,nchn
         else
          write(sacfile,201) iy,id,ih,im,ss,nsta,nchn
         endif
        ilen=lnblnk(sacfile)
C
C now strip out blankspace and replace with zeros
C
         do 130 j=1,ilen
          if (sacfile(j:j) .eq. ' ') sacfile(j:j)='0'
 130     continue
c
c prepare additional info needed for sac file
c
        call allsta(nsta,t1,p1)
        rla = 90.0 - t1 * rad
        rlo = p1 * rad
        if (rlo.gt.180.0) rlo = rlo - 360.0
c
c write the sac file
c
	call wsach (sacfile(1:ilen), dat, nscan, dat, nerr,
     +		nsta, nchn, ntyp, iy, id, ih, im, ss, dt,
     +          rla, rlo, slat, slon, sdep)
	if (nerr .eq. 0) go to 100
C
 900	write (6,*) ' *****error: file too big******'

  200   format(i4,'.',i3,'.',i2,'.',i2,'.',f4.1,'.',a4,'.',a4)
  201   format(i4,'.',i3,'.',i2,'.',i2,'.',f4.1,'.',a3,'.',a4)
c
 990    continue
        call gfs_close(1)
 1000	stop
	end
