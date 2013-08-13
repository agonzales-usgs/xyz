c
c------------------
c
      logical function strcmp(str1,str2)
c
c Compares string1 to string2. Strings can be the same length or
c different lengths. If the two strings are the same length and are
c equal, or if the shorter string is embedded in the longer string,
c then strcmp is returned true.
c
      character*(*) str1,str2

      strcmp = .true.
      len1 = lnblnk(str1)
      len2 = lnblnk(str2)

      if (len1.eq.0 .or. len2.eq.0) then
        strcmp = .false.
        return
      endif
      
      if (len1.eq.len2) then
        if (str1(1:len1).eq.str2(1:len2)) return       
      elseif (len1.gt.len2) then
        do 10 i=1,len1-len2+1
 10       if (str1(i:i+len2-1).eq.str2(1:len2)) return
      elseif (len2.gt.len1) then
        do 20 i=1,len2-len1+1
 20       if (str1(1:len1) .eq. str2(i:i+len1-1)) return
      endif
      strcmp = .false.
      return
      end
