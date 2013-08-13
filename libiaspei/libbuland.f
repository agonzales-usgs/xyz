c
      subroutine setupbuland(lu,depth)
      logical prnt(3)
      dimension usrs(2)
      character tablefile*80,dbsdir*80
      save depthbuland
      data depthbuland/-1./
      data prnt/.false.,.false.,.true./

      call getenv('HRVDBS',dbsdir)
      tablefile = dbsdir(1:lnblnk(dbsdir))//'/iasp91'
c
c      call assign(10,2,'ttim1.lis')
      if(depth.ne.depthbuland) then
        call tabin(lu,tablefile)
        call brnset(1,'ALL     ',prnt)
        call depset(depth,usrs)
        depthbuland=depth
      endif

      return
      end
c
      subroutine evalbuland(delta,n)
      parameter (max=60)
      character*8 phcd(max)
      common/bulandX/ tt(max),dtdd(max),dtdh(max),dddp(max),phcd
      call trtm(delta,max,n,tt,dtdd,dtdh,dddp,phcd)
      end
c
      subroutine getbuland(ip,ttime,pray,dtddep,ddeldp,phase)
      parameter (max=60)
      character*8 phcd(max),phase
      common/bulandX/ tt(max),dtdd(max),dtdh(max),dddp(max),phcd
      ttime=tt(ip)
      pray=dtdd(ip)
      dtddep=dtdh(ip)
      ddeldp=dddp(ip)
      phase=phcd(ip)
      return
      end
