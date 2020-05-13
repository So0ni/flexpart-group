    program flex_read_compare

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
    real :: grid_reference(0:nxmax-1,0:nymax-1,nzmax,maxspec,maxageclass)
    real :: grid_diff(0:nxmax-1,0:nymax-1,nzmax,maxspec,maxageclass)
    real :: footprint(0:nxmax-1,0:nymax-1)
    real :: footprint_total(0:nxmax-1,0:nymax-1,maxspec)
    real :: area(0:nxmax-1,0:nymax-1)
    real :: heightnn(0:nxmax-1,0:nymax-1,0:nzmax)
    real :: smallnum,r_sparse_dump(5000000)
    integer :: i_sparse_dump(5000000)
    parameter(smallnum=1.e-38)

    character dirname*120, dirname_reference*120 
    logical ::  dirnameisarg
    character filename_reference*150,filename*150,fileout*150,aday*8,atime*6
    character(250) :: filegrid, filegrid_reference 
          
    logical :: sp_zer

    real :: sizegrid_m
    real :: sum_grid, sum_sum_grid
    real :: max_grid, max_max_grid
    real :: maxdiff, diff_tolerance 
    character(4) :: grid_magnitude

!    grid_magnitude='time'
    grid_magnitude='conc'
!    grid_magnitude='pptv'
    maxdiff=0.0  
    
    dirnameisarg= .FALSE. 
    ! print*, iargc()
    select case (iargc())
    case (3)
      call getarg(1,dirname)
      len=index(dirname,' ')-1
      call getarg(2,dirname_reference)
      len_reference=index(dirname_reference,' ')-1
      call getarg(3,grid_magnitude)
    case (2)
      !dirnameisarg= .TRUE. 
      call getarg(1,dirname)
      len=index(dirname,' ')-1
      call getarg(2,grid_magnitude)
    case (1)
      call getarg(1,grid_magnitude)
    case (0)
      grid_magnitude='time'
    print*,'Error: argument absent'
    stop 2
    end select
    print*, "grid_magnitude=", grid_magnitude

!    if ( .NOT. dirnameisarg) then
    !******************
!!        print*, " Read dirlist "
    !******************
!!        open(15,file='dirlist')
!!        200 read(15,'(a)',end=199) dirname_reference
!!       len_reference=index(dirname_reference,' ')-1
!    endif


     print*, 'dirname input: ', dirname
     print*, 'dirname reference: ', dirname_reference
     

!      do 50 mother_or_nest=1,2
    do 50 mother_or_nest=1,1

    !*********************************
    ! Read FLEXPART header information
    !*********************************

        if (mother_or_nest == 1) then
            filename=dirname_reference(1:len_reference)//'header'
        else if (mother_or_nest == 2) then
            filename=dirname_reference(1:len_reference)//'header_nest'
        endif
        call readheader(filename,nxmax,numxgrid,nymax,numygrid,nzmax, &
        numzgrid,outlon0,outlat0,dxout,dyout,outheight,ibdate,ibtime, &
        loutstep,maxspec,nspec,maxageclass,nageclass,lage,ireleasestart, &
        ireleaseend,maxpoint,numpoint,xpoint,ypoint,zpoint1,zpoint2, &
        heightnn,area)

        print*, 'ireleasestart, ireleaseend ',ireleasestart, ireleaseend


        if (mother_or_nest == 1) then
            filename=dirname(1:len)//'header'
        else if (mother_or_nest == 2) then
            filename=dirname(1:len)//'header_nest'
        endif
        call readheader(filename,nxmax,numxgrid,nymax,numygrid,nzmax, &
        numzgrid,outlon0,outlat0,dxout,dyout,outheight,ibdate,ibtime, &
        loutstep,maxspec,nspec,maxageclass,nageclass,lage,ireleasestart, &
        ireleaseend,maxpoint,numpoint,xpoint,ypoint,zpoint1,zpoint2, &
        heightnn,area)

        print*, 'ireleasestart, ireleaseend ',ireleasestart, ireleaseend
        !STOP


        write(*,*) 'nspec',nspec
        julstart=juldate(ibdate,ibtime)
        do 30 k=1,nspec
            jul=julstart+dble(float(ireleaseend(k))/86400.)
            call caldate(jul,ireldate(k),ireltime(k))
            print*,'ireldate',ireldate(k)
            print*,'ireltime',ireltime(k)
        30 END DO

        max_max_grid=0
        max_grid=0
        sum_grid=0
        sum_sum_grid=0
        sizegrid_m=0

    ! rint*, 'init sizegrid_m=', sizegrid_m !=0

        do 70 k=1,nspec
            do 70 ix=0,numxgrid-1
                do 70 jy=0,numygrid-1
                    footprint_total(ix,jy,k)=0.
        70 END DO

    !******************************************
    ! Loop about all times, given in file dates
    !******************************************
        !print*,'> open dates'
        
        open(20,file=dirname(1:len)//'dates',form='formatted', &
        status='old')
        it=0
        print*,'> loop on dates'
        100 read(20,'(i8,i6)',end=99) inday,intime
        write(aday,'(i8.8)') inday
        write(atime,'(i6.6)') intime
        it=it+1
                 
        if (mother_or_nest == 1) then
            !filegrid=dirname(1:len)//'/grid_conc_'//aday//atime//'_001'
            filegrid=dirname(1:len)//'/grid_'//grid_magnitude//'_' &
            //aday//atime//'_001'

            filegrid_reference=dirname_reference(1:len_reference)//'/grid_'//grid_magnitude//'_' &
            //aday//atime//'_001'

            fileout=dirname(1:len)//'footprint_'//aday//atime//'_001'
        else
            filegrid=dirname(1:len)//'grid_time_nest_'//aday//atime//'_001'
            fileout=dirname(1:len)//'footprint_nest_'//aday//atime//'_001'
        endif

    ! rite(*,*) filegrid
        print*, 'read filegrid input=', filegrid
        call readgrid(filegrid_reference,itime,nxmax,numxgrid,nymax,numygrid, &
        nzmax,numzgrid,maxspec,nspec,maxageclass,nageclass,grid_reference, &
        lage)

        print*, 'read filegrid reference=', filegrid_reference
        call readgrid(filegrid,itime,nxmax,numxgrid,nymax,numygrid, &
        nzmax,numzgrid,maxspec,nspec,maxageclass,nageclass,grid, &
        lage)




        grid_diff=grid_reference-grid

        print*, 'sum(reference - input)=', sum(grid_diff) 
        print*, 'sum(reference - input)=', sum(grid_reference-grid) 

        maxdiff = max(maxdiff,  sum(grid_reference-grid) )
        print*,'maxdiff =',maxdiff
        

        !diff_tolerance= max(sum(grid_reference)/sum(grid) , sum(grid)/sum(grid_reference)  )
        !if  (sum(abs(grid_diff)) > 1 ) then
        if  (sum(abs(grid_diff)) > sum(abs(grid))/100  ) then
           print*, 'fail! sum(abs(grid_diff)) > 1 '
           stop 1
        endif


        
        !print*, 'stop!'
        !stop


        print*, 'read grid:'
        print*, 'maxval(grid)=', maxval(grid)

        print*, 'sum(grid)=', sum(grid)
    !***************************************
    ! Display read values
    !***************************************
    ! izegrid_m=sizegrid_m+sum(grid)


        sum_grid=sum(grid)
        max_grid=maxval(grid)

        print*, 'max_max_grid,max_grid', max_max_grid, max_grid
        max_max_grid=max(max_max_grid,max_grid)
        sum_sum_grid=sum_sum_grid+sum_grid

        print*, 'sum_grid=',sum_grid !sum(grid)
        print*, 'max_grid=', max_grid !  maxval(grid)
        print*, 'part total', it, aday, atime
    ! rint*, 'sum sum(grid)=', sum_sum_grid
    ! rint*, 'max maxval(grid)=', max_max_grid
        print*, 'sum_sum_grid=', sum_sum_grid
        print*, 'max_max_grid=', max_max_grid
    ! rint*, 'sizegrid_m=', sizegrid_m


        goto 100 !read dates
        99 close(20)


    50 END DO

!    if ( .NOT. dirnameisarg) then
!!        goto 200 !read(15, dirlist
!!        199 close(15) !dirlist
!    endif


    print*, 'for all times'
    print*, 'sum sum(grid)=', sum_sum_grid
    print*, 'max maxval(grid)=', max_max_grid

!    print*, 'sum_sum_grid_out=',sum_sum_grid
!    print*, 'max_max_grid_out=',max_max_grid
    END PROGRAM
