    subroutine readheader10f(filename, &
    nxmax,numxgrid,nymax,numygrid,nzmax,numzgrid, &
    outlon0,outlat0,dxout,dyout,outheight,ibdate, &
    ibtime,loutstep,maxspec,nspec,maxageclass,nageclass,lage, &
    ireleasestart,ireleaseend,maxpoint,numpoint,xpoint,ypoint, &
    zpoint1,zpoint2,heightnn,area)

    parameter(pi=3.14159265,r_earth=6.371e6,pih=pi/180.)

    character filename*150,compoint(maxpoint)*45,species(maxspec)*7
    real :: outheight(nzmax),heightnn(nxmax,nymax,0:nzmax)
! eal outheight(0:nzmax),heightnn(nxmax,nymax,0:nzmax) !v8.2
    integer :: ireleasestart(maxpoint),ireleaseend(maxpoint)
    integer :: npart(maxpoint),kind(maxpoint),lage(0:maxageclass)
    real :: xpoint(maxpoint),ypoint(maxpoint),zpoint1(maxpoint)
    real :: zpoint2(maxpoint),xmass(maxpoint,maxspec)
    real :: oro(nxmax,nymax),area(nxmax,nymax)
          
    character(len=256) :: pathfile, flexversion
    
    integer :: verbosity

    verbosity=1

    if (verbosity.gt.0) then
    print*,'subroutine readheader10f'
    endif 
    
    if (verbosity.gt.0) then
        print*,'intent in:'
        print*,'nxmax=', nxmax
        print*,'nymax=', nymax
        print*,'nzmax=', nzmax    
    endif
 



    open(10,file=filename,form='unformatted',status='old')

          
! 76
    if (verbosity.gt.0) then
    print*,'! Open header output file: '
    print*, '**********************************************'
    print*, filename    
    endif
    
    read(10) ibdate,ibtime, flexversion

    if (verbosity.gt.0) then
    write(*,*) '! Write the header information'
    print*, '**********************************************'
    print*,'(ibdate,ibtime if fwd. -- iedate,ietime if bwd.)'
    write(*,*) 'date,time=',ibdate,ibtime
    print*, 'flexversion=',flexversion
    endif
    
! 85
    read(10) loutstep,loutaver,loutsample
    if (verbosity.gt.0) then
    print*,' info on output interval, averaging time, sampling time' 
    print*, '**********************************************'
    print*,'loutstep, aver, sample=',loutstep,loutaver,loutsample
    endif

!87
    read(10) outlon0,outlat0,numxgrid,numygrid,dxout,dyout
    if (verbosity.gt.0) then
    print*, 'information on output grid setup'
    print*, '**********************************************'
    write(*,*) 'outlon0,outlat0,numxgrid,numygrid,dxout,dyout'
    write(*,*) outlon0,outlat0,numxgrid,numygrid,dxout,dyout
    endif

    read(10) numzgrid,(outheight(i),i=1,numzgrid)
    if (verbosity.gt.0) then  
    write(*,*) 'numzgrid=', numzgrid
!    write(*,*) 'outheight=', outheight
        print*,'outheight='
        do i=1,numzgrid
        print*, i, outheight(i)
        end do   
    endif 

    read(10) jjjjmmdd,ihmmss
    if (verbosity.gt.0) then
    write(*,*) 'jjjjmmdd,ihmmss=',jjjjmmdd,ihmss
    endif

  ! Write number of species, and name for each species (+extra name for depositions)
  ! Indicate the dimension of the fields (i.e., 1 for deposition fields, numzgrid for
  ! concentration fields
  !*****************************************************************************
    if (verbosity.gt.0) then
    print*, 'information on species'
    print*, '**********************************************'    
    endif 
! 102  3*nspec,maxpointspec_act implicit?
    read(10) nspec, maxpointspec_act
    if (verbosity.gt.0) then
    print*,'3*nspec,maxpointspec_act=',nspec,maxpointspec_act
    endif
    nspec=nspec/3
    do 8 n=1,nspec
        read(10) numzgrid,species(n)
        if (verbosity.gt.0) then
        print*,'numzgrid,species(n)',numzgrid,species(n)
        endif
        read(10) numzgrid,species(n)
        if (verbosity.gt.0) then
        print*,'numzgrid,species(n)',numzgrid,species(n)
        endif
        read(10) numzgrid,species(n)
        if (verbosity.gt.0) then
        print*,'numzgrid,species(n)',numzgrid,species(n)
        endif
    8 END DO

    if (verbosity.gt.0) then
    print*, 'information on release points:'
    print*, '**********************************************'    
    endif
    
    read(10) numpoint !l113
    if (verbosity.gt.0) then
    print*,'numpoint=',numpoint
    endif

    do 13 i=1,numpoint
        read(10) ireleasestart(i),ireleaseend(i)
        read(10) xpoint(i),ypoint(i),xp2,yp2,zpoint1(i),zpoint2(i)
        read(10) npart(i),kind(i)
        read(10) compoint(i)
        do 13 j=1,nspec
            read(10)
            read(10)
            read(10) xmass(i,j)
    13 END DO
    
    


    if (verbosity.gt.0) then
    print*,'! Write information on some model switches'
    print*, '**********************************************'
    
    endif
    read(10) method,lsubgrid,lconvection,ind_source,ind_receptor
    if (verbosity.gt.0) then
    print*,'method,lsubgrid,lconvection,ind_source,ind_receptor'
    print*,method,lsubgrid,lconvection,ind_source,ind_receptor
    endif


  ! Write age class information
  !****************************
    read(10) nageclass,(lage(i),i=1,nageclass)
    lage(0)=0
    if (verbosity.gt.0) then
    print*,'!Write age class information'
    print*, '**********************************************'
    endif
    if (verbosity.gt.0) then
    print*,'nageclass=',nageclass
    write(*,*) 'lage(0:nageclass)=', (lage(i),i=0,nageclass)
    endif
    

  ! Write topography to output file
  !********************************

    
    do 130 ix=1,numxgrid
        read(10) (oro(ix,jy),jy=1,numygrid)
    130 END DO
    
    if (verbosity.gt.0) then
    print*,'!Write topography to output file' 
    print*, '**********************************************'
    
    print*,'(oro(ix,jy)'
    print*,'numxgrid,numygrid=',numxgrid,numygrid
    print*,'nxmax,nymax=',nxmax,nymax
    endif      
    close(10)



! rite(*,*) (outheight(i),i=1,numzgrid)
! f (loutstep.lt.0) nspec=numpoint

    if (verbosity.gt.0) then
    print*,' !if (loutstep < 0) nspec=numpoint'
    print*,'outheight(0)=0. ???'
    endif
    
! Calculate height, which is outheight plus topography
!*****************************************************

    if (verbosity.gt.0) then     
    print*,'Calculate height, which is outheight plus topography'
    print*, '**********************************************'
    endif 

    do 150 ix=1,numxgrid
        do 150 jy=1,numygrid
            if (ltopo == 1) then
                heightnn (ix,jy,0) = oro(ix,jy)
            else
                heightnn (ix,jy,0) = 0.
            endif
            do 150 i=1,numzgrid
                if (ltopo == 1) then
                    heightnn (ix,jy,i) = outheight(i) + oro(ix,jy)
                else
                    heightnn (ix,jy,i) = outheight(i)
                endif
    150 END DO

    if (verbosity.gt.0) then
    print*,'C Determine area of each output grid box'
    endif

! Determine area of each output grid box
!***************************************

    do 140 jy=1,numygrid
        ylata=outlat0+(float(jy-1)+0.5)*dyout
        ylatp=ylata+0.5*dyout
        ylatm=ylata-0.5*dyout
        if ((ylatm < 0) .AND. (ylatp > 0.)) then
            hzone=dyout*r_earth*pih
        else
            cosfact=cos(ylata*pih)*r_earth
            cosfactp=cos(ylatp*pih)*r_earth
            cosfactm=cos(ylatm*pih)*r_earth
            if (cosfactp < cosfactm) then
                hzone=sqrt(r_earth**2-cosfactp**2)- &
                sqrt(r_earth**2-cosfactm**2)
            else
                hzone=sqrt(r_earth**2-cosfactm**2)- &
                sqrt(r_earth**2-cosfactp**2)
            endif
        endif
        gridarea=2.*pi*r_earth*hzone*dxout/360.
        do 140 ix=1,numxgrid
            area(ix,jy)=gridarea
    140 END DO
    if (verbosity.gt.0) then  
    print*,'end subroutine readheader10f'
    endif
    end subroutine readheader10f
