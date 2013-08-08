      character*80 function gname(prompt)
      character*(*) prompt
c
  10  write(*,'(a,$)') prompt
      read(*,'(a)',err=10) gname
      return
      end
