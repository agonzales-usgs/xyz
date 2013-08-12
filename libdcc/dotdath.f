c
c---------------------------------------------------------
c
      subroutine dotdath(filename,slist,numsta)
      character fileid*80, nchn*3, type*3, string*256, filename*256,
     & slist*4, nsta*4, nloc*2,chans*4
      parameter(nmax=20000)
      dimension slist(nmax)
      common/locid/nloc(nmax),chans(nmax)
      common/files/nfiles,nnet(nmax),nsta(nmax),nchn(nmax),
     &   iy(nmax),id(nmax),ih(nmax),im(nmax),ss(nmax),
     &    nscan(nmax),fileid(nmax)
      
      io=8
      open(io,file=filename)
      numsta = 0
      n = 0
 5    read(io,'(a)',end=100) string
      read(string,'(a3)') type
      
      if (type.eq.'DIR') then
      endif
      
      if (type.eq.'SAC') then
        if(n.eq.nmax) then
          write(*,*) 'max files reached'
          goto 100
        endif
        n = n + 1
        read(string,10) nnet(n),nsta(n),nloc(n),nchn(n),iy(n),id(n),
     &                  ih(n),im(n),ss(n),nscan(n),fileid(n)
 10     format(5x,a2,1x,a4,2x,a2,1x,a3,1x,i4,1x,i3,
     &         1x,i2,1x,i2,1x,f6.3,i9,1x,a)
        chans(n) = nchn(n)
c
c build list of unique station names
c
        if (numsta.eq.0) then
          numsta = 1
          slist(numsta) = nsta(n)
          goto 5
        else
          do 15 i=1,numsta
 15         if (nsta(n).eq.slist(i)) goto 5
          numsta = numsta + 1
ccc          write(*,*) 'nsta(n) = ',nsta(n)
          slist(numsta) = nsta(n)
        endif
      endif
      goto 5
      
 100  continue
      nfiles = n
      write(*,*) '# of data files available = ',n
      write(*,*) '# of unique stations      = ',numsta
      close(io)
      return
      end
