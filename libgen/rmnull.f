c
c----------------------------------------------
c      
      subroutine rmnull(string)
c
c Removes nulls from a string. If the new string packs down
c shorter, the extra space at the end is filled with blanks.
c
      character*(*) string
      
      len = lnblnk(string)
      j = 0
      do 20 i=1,len
        if (string(i:i) .ne. char(0)) then
          j = j + 1
          string(j:j) = string(i:i)
        endif
 20   continue
      do 30 i=j+1,len
30      string(i:i) = ' '
      return
      end
