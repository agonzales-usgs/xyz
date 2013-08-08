      subroutine upcase(a)
c Converts lower case characters in "a" to upper case
c all other characters are left untouched.
c
      character*(*) a
      
      nchar = lnblnk(a)
      do 10 i=1,nchar
        idum=ichar(a(i:i))
        if(idum.ge.97.and.idum.le.122) a(i:i)=char(idum-32)
   10 continue       
      return
      end
