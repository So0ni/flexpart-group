      subroutine readgrid(filegrid,itime,nxmax,numxgrid,nymax,numygrid,
     +nzmax,numzgrid,maxspec,nspec,maxageclass,nageclass,grid,lage)

      real grid(0:nxmax-1,0:nymax-1,nzmax,maxspec,maxageclass)
      integer lage(0:maxageclass)

***************** new sparse output
      real smallnum
      integer fact,ii,ir
      real sparse_dump_r(nxmax*nymax*nzmax)
      integer sparse_dump_i(nxmax*nymax*nzmax)
      integer sp_count_i,sp_count_r
***************** new sparse output


      character*250 filegrid

      open(10,file=filegrid,form='unformatted',status='old')
      read(10,end=99) itime 
      !write(*,*) itime
      !print*, 'readgrid> itime=', itime

C Loop about all species
************************

      do 38 k=1,nspec

C Loop about all age classes
****************************

        do 38 nage=1,nageclass

C Initialize age class fields
*****************************

        do 63 ix=0,numxgrid-1
          do 63 jy=0,numygrid-1
            do 63 kz=1,numzgrid
63            grid(ix,jy,kz,k,nage)=0.


C Read wet deposition
*********************

        fact=1
        read(10) sp_count_i
        read(10) (sparse_dump_i(ix),ix=1,sp_count_i)
        read(10) sp_count_r
        read(10) (sparse_dump_r(ix),ix=1,sp_count_r)

c       ii=0
c       do 32 ir=1,sp_count_r
c         if ((sparse_dump_r(ir)*fact).gt.smallnum) then
c            ii=ii+1
c            n=sparse_dump_i(ii)
c            fact=fact*(-1.)
c         else
c            pos=pos+1
c         endif

c         jy=n/numxgrid
c         ix=n-numxgrid*jy
c         wetgrid(ix,jy,k,nage)=sparse_dump_r(ir)

c2      continue

C Read dry deposition
*********************

        fact=1
        read(10) sp_count_i
        read(10) (sparse_dump_i(ix),ix=1,sp_count_i)
        read(10) sp_count_r
        read(10) (sparse_dump_r(ix),ix=1,sp_count_r)

c       ii=0
c       do 36 ir=1,sp_count_r
c         if ((sparse_dump_r(ir)*fact).gt.smallnum) then
c            ii=ii+1
c            n=sparse_dump_i(ii)
c            fact=fact*(-1.)
c         else
c            pos=pos+1
c         endif

c         jy=n/numxgrid
c         ix=n-numxgrid*jy
c         drygrid(ix,jy,k,nage)=sparse_dump_r(ir)

c6      continue


C Read concentrations
*********************

        fact=1
        read(10) sp_count_i
        read(10) (sparse_dump_i(ix),ix=1,sp_count_i)
        read(10) sp_count_r
        read(10) (sparse_dump_r(ix),ix=1,sp_count_r)
c       write(*,*) sp_count_i,sp_count_r
c       write(*,*) (sparse_dump_i(ix),ix=1,sp_count_i)
c       write(*,*) (sparse_dump_r(ix),ix=1,sp_count_r)

        ii=0
        do 47 ir=1,sp_count_r
          if ((sparse_dump_r(ir)*fact).gt.smallnum) then
             ii=ii+1
             n=sparse_dump_i(ii)
             fact=fact*(-1.)
          else
             n=n+1
          endif

          kz=n/(numxgrid*numygrid)
          jy=(n-kz*numxgrid*numygrid)/numxgrid
          ix=n-numxgrid*numygrid*kz-numxgrid*jy
          grid(ix,jy,kz,k,nage)=abs(sparse_dump_r(ir))

47      continue

C End species loop, end age class loop
**************************************

138       continue
38      continue

      close(10)

99    continue


      end
