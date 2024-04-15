



   Module Plot
    use         :: regridpack_module              ! spline module for 1D, 2D, 3D, and 4D functions
    use         :: Kpath_module
    use         :: PDOS_i
    implicit none
   contains



   subroutine plot_bands
    integer     :: intpol(4)                      ! interpolation scheme
    real(8)     :: kx1(1),ky1(1),kz1(1),Ex1(1)    ! working arrays
    real(8)     :: Px(1,1,1,1)
    real(8)     :: Py(1,1,1)
    real(8)     :: P_orbx(5)                      ! 5 orbitals
    integer     :: kk,i,j,k,ia,iorb,ier
    real(8), allocatable     :: Pxx(:,:,:,:)
    real(8), allocatable     :: Pyy(:,:,:)
    
    allocate(Pxx(Nx,Ny,Nz,NE))
    allocate(Pyy(Nx,Ny,NE))      ! for 2D case Nz = 1

    intpol(1) = 1                   ! linear spline along kx
    intpol(2) = 1                   ! linear spline along ky
    intpol(3) = 1                   ! linear spline along kz
    intpol(4) = 1                   ! linear spline along E
    
    print *,'nk=',nk
    print *,'Nx=',Nx
    print *,'Ny=',Ny
    print *,'Nz=',Nz
    print *,'NE=',NE
    
    print *
    print *,'check k-path'
    do kk=1,nk
     kx1(1) = kpath(1,kk)
     ky1(1) = kpath(2,kk)
     kz1(1) = kpath(3,kk)
     print 3,kk,kp(kk),kx1(1),ky1(1),kz1(1)
    enddo  

    print *
    print *,'check k-path from PDOS (3D)'
        kk = 0
        do i=1,Nx
         do j=1,Ny
          do k=1,Nz
           kk = kk + 1
           print 3,kk,kx(i),ky(j),kz(k)
          enddo
         enddo
        enddo 




    do ia=1,2
     open(unit=2,file='bands_pl_fat_yambo_'//atom(ia)//'_test.dat')

     write(2,4) nk,NE
     do i=1,NE
       do k=1,nk

      kx1(1) = kpath(1,k)
      ky1(1) = kpath(2,k)
      kz1(1) = kpath(3,k)
      Ex1(1) = Ei(i)
      
      do iorb = 1,5                        ! s, pz, px, py, d
       if(Ex1(1) .lt. Ei(1) .or. Ex1(1) .gt. Ei(NE) .or. &
          kx1(1) .lt. kx(1) .or. kx1(1) .gt. kx(Nx)  .or. &
          ky1(1) .lt. ky(1) .or. ky1(1) .gt. ky(Ny)  .or. &
          kz1(1) .lt. kz(1) .or. kz1(1) .gt. kz(Nz)       ) then
        P_orbx(iorb) = 0.d0
       else
        if(Nz/=1) then
         Pxx(1:Nx,1:Ny,1:Nz,1:NE) = P_orb(1:Nx,1:Ny,1:Nz,1:NE,iorb,ia)
         call regrid(kx,ky,kz,Ei,Pxx,kx1,ky1,kz1,Ex1,Px,intpol,ier)
         P_orbx(iorb) = Px(1,1,1,1)
         if(ier/=0) print *,'2 regrid: ERROR ier=',ier
        else 
         Pyy(1:Nx,1:Ny,1:NE) = P_orb(1:Nx,1:Ny,1,1:NE,iorb,ia)
         call regrid(kx,ky,Ei,Pyy,kx1,ky1,Ex1,Py,intpol,ier)
         P_orbx(iorb) = Py(1,1,1)
         if(ier/=0) print *,'3 regrid: ERROR ier=',ier
        endif 
       endif    
      enddo 

       write(2,1) kp(k),Ei(i),P_orbx(1),P_orbx(2),P_orbx(3),P_orbx(4),P_orbx(5)        
      enddo
     enddo
     close(unit=2)
    enddo   ! ia

 1  format(2F14.6,5E17.4)
 2  format(' VARIABLES = "k", "E", "p1", "p2", "p3", "p4", "p5" '/ &
           ' ZONE T="',I2,'" I= ',I4,' F=POINT')
 3  format(I4,4F16.5)       
 4  format(' VARIABLES = "k", "E", "p1", "p2", "p3", "p4", "p5" '/ &
           ' ZONE   I= ',I5,'   J= ',I5,' F=POINT')
   end subroutine plot_bands
















   subroutine calc_Ex(E1,Ex)                           ! apply scissor correction
    real(8)     :: E1
    real(8)     :: Ex
    if(E1-EF < 1.d0) then
     Ex = E1 - EF 
    else
     Ex = E1 - EF + Eshift 
    endif
   end subroutine calc_Ex



   



   end module Plot











