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
    logical :: debug=.FALSE.
    
    real :: sizegrid_m
    real :: sum_grid, sum_sum_grid
    real :: max_grid, max_max_grid
    character(4) :: grid_magnitude

        max_max_grid=0
        max_grid=0
        sum_grid=0
        sum_sum_grid=0
        sizegrid_m=0


    grid_magnitude='time'
    grid_magnitude='conc'
    grid_magnitude='pptv'

    dirnameisarg= .FALSE. 

    if (debug) then
      print*, 'iargc', iargc()
    endif

    select case (iargc())
    case (3)
    debug=.TRUE.
    dirnameisarg= .TRUE. 
    call getarg(1,dirname)
    len=index(dirname,' ')-1
    call getarg(2,grid_magnitude)

    case (2)
    dirnameisarg= .TRUE. 
    call getarg(1,dirname)
    len=index(dirname,' ')-1
    call getarg(2,grid_magnitude)
    case (1)
    call getarg(1,grid_magnitude)
    case (0)
    grid_magnitude='time'
    end select
    if (debug) then
    print*, grid_magnitude
    endif
    
    if ( .NOT. dirnameisarg) then
    !******************
        print*, " Read CONTROL file "
    !******************
        open(15,file='dirlist')
        200 read(15,'(a)',end=199) dirname
        len=index(dirname,' ')-1
    endif

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
        call readheader(filename,nxmax,numxgrid,nymax,numygrid,nzmax, &
        numzgrid,outlon0,outlat0,dxout,dyout,outheight,ibdate,ibtime, &
        loutstep,maxspec,nspec,maxageclass,nageclass,lage,ireleasestart, &
        ireleaseend,maxpoint,numpoint,xpoint,ypoint,zpoint1,zpoint2, &
        heightnn,area)

        if (debug) then
          !write(*,*) 'nspec',nspec
          write(*,*) 'simulation starts ', ibdate,ibtime
          write(*,*) 'releases:'
        endif

        julstart=juldate(ibdate,ibtime)
        do 30 k=1,nspec
            jul=julstart+dble(float(ireleaseend(k))/86400.)
            call caldate(jul,ireldate(k),ireltime(k))
!            print*,'ireldate',ireldate(k)
!             print*,'ireltime',ireltime(k)
             if (debug) then 
             print*, k, 'ireldate, ireltime',ireldate(k), ireltime(k)
             endif
             call caldate(julstart+dble(float(ireleasestart(k))/86400.) &
             ,irelds,irelts)
             if (debug) then
             print*, k,irelds,irelts 
             print*, k,xpoint(k),ypoint(k),zpoint1(k),zpoint2(k) 
             !print*, k,irelds,irelts 
             endif 

        30 END DO
        

    ! rint*, 'init sizegrid_m=', sizegrid_m !=0

        do 70 k=1,nspec
            do 70 ix=0,numxgrid-1
                do 70 jy=0,numygrid-1
                    footprint_total(ix,jy,k)=0.
        70 END DO

    !******************************************
    ! Loop about all times, given in file dates
    !******************************************
        if (debug) then
          print*,'> open dates'
        endif

        open(20,file=dirname(1:len)//'dates',form='formatted', &
        status='old')
        it=0
        100 read(20,'(i8,i6)',end=99) inday,intime
        write(aday,'(i8.8)') inday
        write(atime,'(i6.6)') intime
        it=it+1

        if (debug) then
!         print*, 'it, aday, atime', it, aday, atime
         print*, 'i, day, time', it, inday, intime
         endif
                 
        if (mother_or_nest == 1) then
                        
        ! ilegrid=dirname(1:len)//'/grid_time_'//aday//atime//'_001'
            filegrid=dirname(1:len)//'/grid_conc_'//aday//atime//'_001'
        ! ilegrid=dirname(1:len)//'/grid_pptv_'//aday//atime//'_001'
                        
            filegrid=dirname(1:len)//'/grid_'//grid_magnitude//'_' &
            //aday//atime//'_001'
            fileout=dirname(1:len)//'footprint_'//aday//atime//'_001'
        else
            filegrid=dirname(1:len)//'grid_time_nest_'//aday//atime//'_001'
            fileout=dirname(1:len)//'footprint_nest_'//aday//atime//'_001'
        endif

    !    print*, 'filegrid=', filegrid

        call readgrid(filegrid,itime,nxmax,numxgrid,nymax,numygrid, &
        nzmax,numzgrid,maxspec,nspec,maxageclass,nageclass,grid, &
        lage)

        !print*, 'read grid:'
        !print*, 'maxval(grid)=', maxval(grid)
        !print*, 'sum(grid)=', sum(grid)
        !print*, 'max, sum                 ', maxval(grid), sum(grid)

    !***************************************
    ! Display read values
    !***************************************
    ! izegrid_m=sizegrid_m+sum(grid)


        sum_grid=sum(grid)
        max_grid=maxval(grid)

        if (debug) then
          print*, 'max, sum                 ', max_grid, sum_grid
        endif

        max_max_grid=max(max_max_grid,max_grid)
        sum_sum_grid=sum_sum_grid+sum_grid
!        print*, 'max_max_grid,max_grid', max_max_grid, max_grid
        if (debug) then
          print*, 'max_max_grid,sum_sum_grid', max_max_grid, sum_sum_grid
          print*,
        endif 
!        print*, 'sum_grid=',sum_grid !sum(grid)
!        print*, 'max_grid=', max_grid !  maxval(grid)
!        print*, 'part total', it, aday, atime
!        print*, 'sum_sum_grid=', sum_sum_grid
!        print*, 'max_max_grid=', max_max_grid

        if (0 == 1) then
        !****************************************
        ! Write out footprint files for each date
        !****************************************
                   
            open(97,file=fileout,form='unformatted')
            write(97) itime

        ! Add contributions from this time step to gridded field
        !*******************************************************

            do 80 k=1,nspec
                do 202 ix=0,numxgrid-1
                    do 202 jy=0,numygrid-1
                        footprint(ix,jy)=0.
                        do 202 nage=1,nageclass
                            footprint(ix,jy)=footprint(ix,jy)+ &
                            grid(ix,jy,1,k,nage)
                            footprint_total(ix,jy,k)=footprint_total(ix,jy,k)+ &
                            grid(ix,jy,1,k,nage)
                202 END DO


                n_sp_count_i=0
                n_sp_count_r=0
                sp_fact=-1.
                sp_zer= .TRUE. 
                do 135 jy=0,numygrid-1
                    do 135 ix=0,numxgrid-1
                        if (footprint(ix,jy) > smallnum) then
                            if (sp_zer) then ! first non zero value
                                n_sp_count_i=n_sp_count_i+1
                                i_sparse_dump(n_sp_count_i)= &
                                ix+jy*numxgrid+1*numxgrid*numygrid   ! vertical level is 1
                                sp_zer= .FALSE. 
                                sp_fact=sp_fact*(-1)
                            endif
                            n_sp_count_r=n_sp_count_r+1
                            r_sparse_dump(n_sp_count_r)=sp_fact*footprint(ix,jy)
                        else ! concentration is zero
                            sp_zer= .TRUE. 
                        endif
                135 END DO

            ! First, zero wet and dry depo
                write(97) 0
                write(97)
                write(97) 0
                write(97)
                write(97) 0
                write(97)
                write(97) 0
                write(97)

                write(97) n_sp_count_i
                write(97) (i_sparse_dump(i),i=1,n_sp_count_i)
                write(97) n_sp_count_r
                write(97) (r_sparse_dump(i),i=1,n_sp_count_r)

            80 END DO
            close(97)
        endif ! Write out footprint files for each date

        goto 100 !read dates
        99 close(20)


        if  (0 == 1) then
        ! relic
    ! Dump time-integrated footprint
    !*******************************
            if (mother_or_nest == 1) then
                fileout=dirname(1:len)//'footprint_total'//'_001'
            else
                fileout=dirname(1:len)//'footprint_total_nest'//'_001'
            endif

            open(97,file=fileout,form='unformatted')
            write(97) itime

            do 180 k=1,nspec
                n_sp_count_i=0
                n_sp_count_r=0
                sp_fact=-1.
                sp_zer= .TRUE. 
                do 145 jy=0,numygrid-1
                    do 145 ix=0,numxgrid-1
                        if (footprint_total(ix,jy,k) > smallnum) then
                            if (sp_zer) then ! first non zero value
                                n_sp_count_i=n_sp_count_i+1
                                i_sparse_dump(n_sp_count_i)= &
                                ix+jy*numxgrid+1*numxgrid*numygrid   ! vertical level is 1
                                sp_zer= .FALSE. 
                                sp_fact=sp_fact*(-1)
                            endif
                            n_sp_count_r=n_sp_count_r+1
                            r_sparse_dump(n_sp_count_r)= &
                            sp_fact*footprint_total(ix,jy,k)
                        else ! concentration is zero
                            sp_zer= .TRUE. 
                        endif
                145 END DO

            ! First, zero wet and dry depo
                write(97) 0
                write(97)
                write(97) 0
                write(97)
                write(97) 0
                write(97)
                write(97) 0
                write(97)

                write(97) n_sp_count_i
                write(97) (i_sparse_dump(i),i=1,n_sp_count_i)
                write(97) n_sp_count_r
                write(97) (r_sparse_dump(i),i=1,n_sp_count_r)

            180 END DO
            close(97)
        endif ! write footprint total

    50 END DO

    if ( .NOT. dirnameisarg) then
        goto 200 !read(15, dirlist
        199 close(15) !dirlist
    endif

! ax_max_grid=max(max_max_grid,max_grid)
! um_sum_grid=sum_sum_grid+sum_grid
    if (debug) then

    print*, 'for all times'
    print*, 'sum sum(grid)=', sum_sum_grid
    print*, 'max maxval(grid)=', max_max_grid
    endif 
    !if (debug) then

    print*, 'max:', max_max_grid, 'sum:', sum_sum_grid 

!    print*, 'sum_sum_grid_out=',sum_sum_grid
!    print*, 'max_max_grid_out=',max_max_grid
    END PROGRAM
