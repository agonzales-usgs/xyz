c
c---------------------------------------------------------
c
      subroutine siftcmt(ibeg,iend,numeq,neqlist)
c
c Sifts through a common block containing Harvard CMT's.
c Extracts the indices of the EQ's which fall within the 
c specified time window.
c
c You must first load the common block by calling readcmt
c
c input:
c  ibeg, iend = epochal start and end time of time window
c 
c output:
c  numeq = number of EQ's selected
c  neqlist() = array containing indices of selected EQ's
c
      dimension neqlist(1)
      parameter (maxevts=20000)
      common/eqinfo/inum,jy(maxevts),jd(maxevts),
     & jh(maxevts),jm(maxevts),ess(maxevts),eqlat(maxevts),
     & eqlon(maxevts),eqdep(maxevts),eqmb(maxevts),eqms(maxevts),
     & eqmw(maxevts)
      data iflag/0/
     
      numeq = 0
      do 30 i=1,inum
        ievt = ipoch(jy(i),jd(i),jh(i),jm(i),int(ess(i)))
        if (ievt.ge.ibeg .and. ievt.le.iend) then
          numeq = numeq + 1
          neqlist(numeq) = i
        endif
 30     continue
c      
cc       write(*,*) '  # of events selected: ',numeq
cc      do 60 i=1,numeq
cc        j = neqlist(i)
cc        write(*,*) jy(j),jd(j),jh(j),jm(j),ess(j),
cc     &    eqlat(j),eqlon(j),eqdep(j)
cc 60   continue
      return
      end
