c-----------------------------------------------------------
c
      subroutine mk_sec_lh(io,treename,depth,title)
      character*(*) treename, title
      integer system
c      
      open(io,file='sec_lh.cmd')
c
c commands for running ttcrv - to make 'ptable'
c      
      write(8,*) '#'
      write(8,*) '#commands for running ttcrv'
      write(8,*) '#'
      write(8,*) 'rm -f ptable'
      write(8,*) 'ttcrv<<!'
      write(8,*) '/u04/users/woodward/dbs/prem.ttcrv'
      write(8,*) 'ptable'
      write(8,*)  depth,  1.0
      write(8,*) '!'
c
c commands for running ttcrv - to make 'stable'
c      
      write(8,*) '#'
      write(8,*) '#commands for running ttcrv'
      write(8,*) '#'
      write(8,*) 'rm -f stable'
      write(8,*) 'ttcrv<<!'
      write(8,*) '/u04/users/woodward/dbs/prem.ttcrv.s'
      write(8,*) 'stable'
      write(8,*)  depth,  1.0
      write(8,*) '!'
c
c commands for making plot of traces side-by-side 
c
      write(8,*) '#'
      write(8,*) '#commands for making plot of traces side-by-side'
      write(8,*) '#'
      write(8,*) 'sec<<!'
      write(8,'(a)') treename(1:lnblnk(treename))
      write(8,*) 3
      write(8,*) 2
      write(8,*) 0.1, 1.0, 40.0
      write(8,*) 2
      write(8,*) 0
      write(8,*) 0.0, 180.0
      write(8,*) .5
      write(8,'(a)') title(1:lnblnk(title)) 
      write(8,*) 0
      write(8,*) 'lhz'
      write(8,*) '!'
      write(8,*) 'mv mypost sec1_lh.ps'
c
c commands for making standard record section
c
      write(8,*) '#'
      write(8,*) '#commands for making standard record section'
      write(8,*) '#'
      write(8,*) 'sec<<!'
      write(8,'(a)') treename(1:lnblnk(treename))
      write(8,*) 3
      write(8,*) 1
      write(8,*) 0.1, 1.0, 40.0
      write(8,*) 2
      write(8,*) 1
      write(8,'(a)') 'stable'
      write(8,*) 0
      write(8,*) 0.0, 180.0
      write(8,*) 0.5
      write(8,'(a)') title(1:lnblnk(title)) 
      write(8,*) 0
      write(8,*) 'lhz'
      write(8,*) '!'
      write(8,*) 'mv mypost sec2_lh.ps'
c
c finish up
c
      close(8)
      ierr = system('chmod +x sec_lh.cmd')
      return
      end
