





   Module Kpath_module                                           ! band structure
    implicit none
    integer               :: nk                           ! number of k-points     
    real(8), allocatable  :: kp(:)                        ! k-points module 
    real(8), allocatable  :: kpath(:,:)                   ! k-path (kx,ky,kz) 
    real(8)               :: EF                           ! Fermi level
    real(8)               :: Eshift                       ! scissor correction
   contains




    subroutine read_kpath
     integer      :: k
     EF = 0.d0
     Eshift = 0.d0

    print *,'read kpath.dat file'
    open(unit=2,file='kpath.dat')
     read(2,*) nk

     allocate(kp(nk))
     allocate(kpath(3,nk))
     do k = 1,nk
      read(2,*) kp(k),kpath(1:3,k)
     enddo 
    close(unit=2)
    
    do k=1,nk
     print 3,k,kp(k),kpath(1:3,k)
    enddo  
    
 3  format(I4,4F16.5)       
    end subroutine read_kpath













   end module Kpath_module







