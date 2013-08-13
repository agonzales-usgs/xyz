c
c-----------------------------------------------------------
c
      subroutine cktime(ih,im,ss,ih2,im2,ss2,diftim)
      
      ihour = diftim / 3600  
      imin  = (diftim - ihour * 3600) / 60
      ssec  = diftim - ihour * 3600 - imin * 60
      
      ss2 = ss+ssec
      if (ss2.ge.60.0) then
        ss2 = ss2 - 60.0
        imin = imin + 1
      endif

      im2 = imin + im
      if (im2.ge.60) then
        im2 = im2 - 60
        ihour = ihour + 1
      endif
      
      ih2 = ihour + ih
      if (ih2.ge.24) then
        ih2 = ih2 - 24
      endif
      
      ierr = 0
      return 
      end
