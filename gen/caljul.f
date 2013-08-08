C
c--------------------------------------------------------------------
C
      subroutine caljul(iyr,imo,id,ldy)
c
c convert calendar date to julian date
c input:
c  iyr = year (e.g. 1984 or 84)
c  imo = month (1 to 12)
c  id  = day of month
c output:
c  ldy = julian day (1 to 366)
c
      goto (10,11,12,13,14,15,16,17,18,19,20,21), imo
   10 ldy=id
      return
   11 ldy=id+31
      return
   12 ldy=id+59
      goto 40
   13 ldy=id+90
      goto 40
   14 ldy=id+120
      goto 40
   15 ldy=id+151
      goto 40
   16 ldy=id+181
      goto 40
   17 ldy=id+212
      goto 40
   18 ldy=id+243
      goto 40
   19 ldy=id+273
      goto 40
   20 ldy=id+304
      goto 40
   21 ldy=id+334
      goto 40
   40 if(mod(iyr,4).eq.0) ldy = ldy + 1
      return
      end
