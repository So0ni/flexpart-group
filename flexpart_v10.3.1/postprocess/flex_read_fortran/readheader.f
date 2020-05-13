      subroutine readheader(filename,nxmax,numxgrid,nymax,numygrid,
     +nzmax,numzgrid,outlon0,outlat0,dxout,dyout,outheight,ibdate,
     +ibtime,loutstep,maxspec,nspec,maxageclass,nageclass,lage,
     +ireleasestart,ireleaseend,maxpoint,numpoint,xpoint,ypoint,
     +zpoint1,zpoint2,heightnn,area)

      parameter(pi=3.14159265,r_earth=6.371e6,pih=pi/180.)

      character filename*150,compoint(maxpoint)*45,species(maxspec)*7
      real outheight(nzmax),heightnn(nxmax,nymax,0:nzmax)
      integer ireleasestart(maxpoint),ireleaseend(maxpoint)
      integer npart(maxpoint),kind(maxpoint),lage(0:maxageclass)
      real xpoint(maxpoint),ypoint(maxpoint),zpoint1(maxpoint)
      real zpoint2(maxpoint),xmass(maxpoint,maxspec)
      real oro(nxmax,nymax),area(nxmax,nymax)
      logical debug

      debug=.FALSE.

      open(10,file=filename,form='unformatted',status='old')
      read(10) ibdate,ibtime

      if (debug) then 
      write(*,*) ibdate,ibtime
      endif

      read(10) loutstep,loutaver,loutsample
      read(10) outlon0,outlat0,numxgrid,numygrid,dxout,dyout

      if (debug) then
      write(*,*) outlon0,outlat0,numxgrid,numygrid,dxout,dyout
      endif

      read(10) numzgrid,(outheight(i),i=1,numzgrid)
      read(10) jjjjmmdd,ihmmss

      if (debug) then
      write(*,*) jjjjmmdd,ihmss
      endif
      
      read(10) nspec
      nspec=nspec/3
      do 8 n=1,nspec
        read(10) numzgrid,species(n)
        read(10) numzgrid,species(n)
8       read(10) numzgrid,species(n)


      read(10) numpoint
      do 13 i=1,numpoint
        read(10) ireleasestart(i),ireleaseend(i)

        read(10) xpoint(i),ypoint(i),xp2,yp2,zpoint1(i),zpoint2(i)
        read(10) npart(i),kind(i)
        read(10) compoint(i)
        do 13 j=1,nspec
          read(10)
          read(10)
13        read(10) xmass(i,j)
      read(10) method
      read(10) nageclass,(lage(i),i=1,nageclass)
      lage(0)=0

      if (debug) then
      write(*,*) (lage(i),i=0,nageclass)
      endif

      do 130 ix=1,numxgrid
130     read(10) (oro(ix,jy),jy=1,numygrid)

      close(10)
      if (debug) then
      write(*,*) (outheight(i),i=1,numzgrid)
      endif
      
      if (loutstep.lt.0) nspec=numpoint


c Calculate height, which is outheight plus topography
******************************************************

      do 150 ix=1,numxgrid
        do 150 jy=1,numygrid
          if (ltopo.eq.1) then
            heightnn (ix,jy,0) = oro(ix,jy)
          else
            heightnn (ix,jy,0) = 0.
          endif
          do 150 i=1,numzgrid
            if (ltopo.eq.1) then
              heightnn (ix,jy,i) = outheight(i) + oro(ix,jy)
            else
              heightnn (ix,jy,i) = outheight(i)
            endif
150         continue


C Determine area of each output grid box
****************************************

      do 140 jy=1,numygrid
        ylata=outlat0+(float(jy-1)+0.5)*dyout
        ylatp=ylata+0.5*dyout
        ylatm=ylata-0.5*dyout
        if ((ylatm.lt.0).and.(ylatp.gt.0.)) then
          hzone=dyout*r_earth*pih
        else
          cosfact=cos(ylata*pih)*r_earth
          cosfactp=cos(ylatp*pih)*r_earth
          cosfactm=cos(ylatm*pih)*r_earth
          if (cosfactp.lt.cosfactm) then
            hzone=sqrt(r_earth**2-cosfactp**2)-
     +      sqrt(r_earth**2-cosfactm**2)
          else
            hzone=sqrt(r_earth**2-cosfactm**2)-
     +      sqrt(r_earth**2-cosfactp**2)
          endif
        endif
        gridarea=2.*pi*r_earth*hzone*dxout/360.
        do 140 ix=1,numxgrid
140       area(ix,jy)=gridarea


      end
