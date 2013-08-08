c
c----------------------------------------------------------------------
c
      subroutine calmo(imo,imo_name)
c
c input:
c  imo = month
c output:
c  imo_name = name of month (character*3)
c
      character*3 mo(12), imo_name
      DATA MO/'JAN','FEB','MAR','APR',
     &        'MAY','JUN','JUL','AUG',
     &        'SEP','OCT','NOV','DEC'/
      imo_name = mo(imo)
      return
      end
