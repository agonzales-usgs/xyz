c
c-------------------------------------------------------------------
c
      function atant(x,y)
      if(x.ne.0..or.y.ne.0.) then
        atant=atan2(x,y)
      else
         atant=0.
      endif
      return
      end
