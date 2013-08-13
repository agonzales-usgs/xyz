c
c------------------
c
      logical function strloc(str1,str2,iloc)
c
c Compares string1 to string2. Strings can be the same length or
c different lengths. If the two strings are the same length and are
c equal, or if the shorter string is embedded in the longer string,
c then strloc is returned true.
c
c output:
c  iloc = location in longer string where the shorter string
c         matches, for example:
c
c         this is the long string    (long string)
c                      ong           (short string)
c         iloc = 14
c
c   iloc = 1 if two strings are same length and match
c   iloc = 0 if strloc = false
c
      character*(*) str1,str2

      strloc = .true.
      iloc   = 1
      len1 = lnblnk(str1)
      len2 = lnblnk(str2)

      if (len1.eq.0 .or. len2.eq.0) then
        strloc = .false.
        iloc   = 0
        return
      endif
      
      if (len1.eq.len2) then
        if (str1(1:len1).eq.str2(1:len2)) return       
      elseif (len1.gt.len2) then
        do 10 i=1,len1-len2+1
          if (str1(i:i+len2-1).eq.str2(1:len2)) then
            iloc = i
            return
          endif
 10     continue
      elseif (len2.gt.len1) then
        do 20 i=1,len2-len1+1
          if (str1(1:len1) .eq. str2(i:i+len1-1)) then
            iloc = i
            return
          endif
 20     continue
      endif
      strloc = .false.
      iloc   = 0
      return
      end
