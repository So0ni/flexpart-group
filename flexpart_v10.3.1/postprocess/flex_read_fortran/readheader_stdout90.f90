    program flexpartread

    parameter(nxmax=720,nymax=360,nzmax=40,maxtime=10)
    parameter(maxpoint=1,maxspec=1,maxageclass=1)

    double precision :: jul,julstart,juldate
    real :: outheight(nzmax)
    integer :: ireleasestart(maxpoint),ireleaseend(maxpoint)
    integer :: lage(0:maxageclass)
    real :: xpoint(maxpoint),ypoint(maxpoint),zpoint1(maxpoint)
    real :: zpoint2(maxpoint)
    integer :: ireldate(maxspec),ireltime(maxspec)

    real :: grid(0:nxmax-1,0:nymax-1,nzmax,maxspec,maxageclass)
    real :: footprint(0:nxmax-1,0:nymax-1)
    real :: footprint_total(0:nxmax-1,0:nymax-1,maxspec)
    real :: area(0:nxmax-1,0:nymax-1)
    real :: heightnn(0:nxmax-1,0:nymax-1,0:nzmax)
    real :: smallnum,r_sparse_dump(5000000)
    integer :: i_sparse_dump(5000000)
    parameter(smallnum=1.e-38)

    character dirname*120
    logical ::  dirnameisarg
    character filename*150,fileout*150,aday*8,atime*6
    character(250) :: filegrid
          
    logical :: sp_zer

    real :: sizegrid_m
    real :: sum_grid, sum_sum_grid
    real :: max_grid, max_max_grid
    character(4) :: grid_magnitude

    grid_magnitude='time'
    grid_magnitude='conc'
    grid_magnitude='pptv'

    dirnameisarg= .FALSE. 
    print*, 'iargc=', iargc()
    select case (iargc())
    case (2)
    dirnameisarg= .TRUE. 
    call getarg(1,dirname)
    len=index(dirname,' ')-1
    call getarg(2,grid_magnitude)
    case (1)
    call getarg(1,dirname)
    len=index(dirname,' ')-1
    !call getarg(1,grid_magnitude)
    case (0)
    grid_magnitude='time'
    end select
    print*, 'grid_magnitude=', grid_magnitude

 !   if ( .NOT. dirnameisarg) then
 !   !******************
 !       print*, " Read CONTROL file "
    !******************
 !       open(15,file='dirlist')
 !       200 read(15,'(a)',end=199) dirname
 !       len=index(dirname,' ')-1
 !   endif

!      do 50 mother_or_nest=1,2
    do 50 mother_or_nest=1,1

    !*********************************
    ! Read FLEXPART header information
    !*********************************

        if (mother_or_nest == 1) then
            filename=dirname(1:len)//'header'
        else if (mother_or_nest == 2) then
            filename=dirname(1:len)//'header_nest'
        endif
        print*, 'call readheader10f for', filename
    !        call readheader(filename,nxmax,numxgrid,nymax,numygrid,nzmax,
        call readheader10f(filename,nxmax,numxgrid,nymax,numygrid,nzmax, &
        numzgrid,outlon0,outlat0,dxout,dyout,outheight,ibdate,ibtime, &
        loutstep,maxspec,nspec,maxageclass,nageclass,lage,ireleasestart, &
        ireleaseend,maxpoint,numpoint,xpoint,ypoint,zpoint1,zpoint2, &
        heightnn,area)
        print*, 'print readheader10f output'
        print*,'! Write the header information'
                
        print*,'! Write info on output interval, averaging time, samplin &
        g time'
        print*,'! Write information on output grid setup'
        print*,'! Write number of species, and name for each species '
        print*,'dimension of the fields (1 for depo, numzgrid for conc)'
        print*,'! Write information on release points:'
        print*,'! Write information on some model switches'
        print*,'! Write age class information'
        print*,'! Write topography to output file'

                
        print*,'filename'
        print*,

        print*,'nxmax,numxgrid,nymax,numygrid='
        print*,filename,nxmax,numxgrid,nymax,numygrid
        
        print*,'nzmax, numzgrid='
        print*,nzmax,numzgrid

        print*,'outlon0,outlat0,dxout,dyout,' 
        print*,outlon0,outlat0,dxout,dyout 
        
        print*,' outheight='
        do i=1,numzgrid
        print*, i, outheight(i)
        end do

        print*, 'ibdate,ibtime, loutstep'
        print*, ibdate,ibtime, loutstep

!  ,,,ireleasestart, &
!        ireleaseend,maxpoint,numpoint,xpoint,ypoint,zpoint1,zpoint2, &
!       heightnn,area)


        print*,'maxspec,nspec'
        print*,maxspec,nspec
    ! rint*,
        print*,'maxageclass,nageclass'
        print*, maxageclass,nageclass
    ! rint*,
        print*,'lage'
        print*, lage 

        print*,'ireleasestart,   ireleaseend,'
        print*, ireleasestart,   ireleaseend 

        print*,'maxpoint,numpoint'
        print*, maxpoint,numpoint 

        print*,'xpoint,ypoint'
        print*, xpoint,ypoint 

        print*,'zpoint1,zpoint2'
        print*, zpoint1,zpoint2  

        print*,'heightnn'
        print*, size(heightnn) 

!        print*,'xpoint,ypoint,zpoint1,zpoint2,'
!        print*, xpoint,ypoint,zpoint1,zpoint2 

    ! rint*,

        write(*,*) 'nspec',nspec
        julstart=juldate(ibdate,ibtime)
        do 30 k=1,nspec
            print*, 'k', k 
            jul=julstart+dble(float(ireleaseend(k))/86400.)
            call caldate(jul,ireldate(k),ireltime(k))
            print*,'ireldate',ireldate(k)
            print*,'ireltime',ireltime(k)
        30 END DO


         
    !      print*, 'about to exit'
    !      exit

    50 END DO

 !   if ( .NOT. dirnameisarg) then
 !       goto 200 !read(15, dirlist
 !       199 close(15) !dirlist
 !   endif

! ax_max_grid=max(max_max_grid,max_grid)
! um_sum_grid=sum_sum_grid+sum_grid

!         print*, 'for all times'
!         print*, 'sum sum(grid)=', sum_sum_grid
!         print*, 'max maxval(grid)=', max_max_grid

!         print*, 'sum_sum_grid_out=',sum_sum_grid
!         print*, 'max_max_grid_out=',max_max_grid
    END PROGRAM
