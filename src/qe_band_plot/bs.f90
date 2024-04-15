





   Module BS                                              ! band structure
    integer               :: nk                           ! number of k-points     
    integer               :: nbnd                         ! number of bands
    integer, parameter    :: Nks = 2
    integer               :: ks(Nks)                      ! high-symmetry points
    real(8), allocatable  :: kp(:,:)                      ! k-points 
    real(8), allocatable  :: bn(:,:)                      ! bands 
    real(8)               :: EF                           ! Fermi level
    real(8)               :: Eshift                       ! scissor correction
   contains




    subroutine read_bands
     call getargi(1,ks(1))                      ! G
     call getargi(2,ks(2))                      ! M   K
     call getargR(3,EF)   
     call getargR(4,Eshift)   
     print *,'high-symmetry points:'
     print *,ks(1:Nks)                    
     open(unit=1,file='bands.dat')
     read(1,1) nbnd,nk
     print *,'nbnd=',nbnd
     print *,'nk=',nk
     allocate(kp(3,nk))
     allocate(bn(nbnd,nk))
     do k=1,nk
      read(1,*) kp(1:3,k)
      print *,'read k-point',k
      print *,kp(1:3,k)
      read(1,*) bn(1:nbnd,k)
     enddo
     
     ! substitute high-symmetry points by crystal coordinates
     kp(1:3,ks(1)) = (/0.d0, 0.d0, 0.d0/)
     kp(1:3,ks(2)) = (/0.5d0, 0.d0, 0.d0/)
     
     print *,'TEST'
     print *,kp(1:3,ks(1))
     print *,kp(1:3,ks(2))
     print *,'Fermi level=',EF
     print *,'Shift =',Eshift
 1   format(12x,I4,6x,I6)
    end subroutine read_bands




   subroutine getargi(i,N)                ! read integer from argument line
    integer       :: i,N
    character(3)  :: nc
    call getarg(i,nc)
    read(nc,*) N
   end subroutine getargi




   subroutine getargR(i,R)                ! read real from argument line
    integer       :: i
    real(8)       :: R
    character(10)  :: Rc
    call getarg(i,Rc)
    read(Rc,*) R
   end subroutine getargR





   end module BS







