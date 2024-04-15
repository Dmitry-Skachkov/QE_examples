



   Module Plot
    use BS
    real(8), allocatable :: kps(:)
   contains



   subroutine plot_bands
    real(8)        :: Ex
    call calc_kps
    open(unit=2,file='bands_plot.dat')
     do i=1,nbnd
      write(2,2) nk
      do k=1,nk
       call calc_Ex(bn(i,k),Ex)
       write(2,1) kps(k),Ex
      enddo
     enddo
     print *,'high symmetry points:   Nks=',Nks
     do k=1,Nks
      print 3,k,kps(ks(k))
     enddo
 1  format(2F14.6)
 2  format(' VARIABLES = "k", "E" '/ &
           ' ZONE I= ',I4,' F=POINT')
 3  format(I4,F14.6)
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



   subroutine calc_kps
    real(8)              :: kps0
    real(8)              :: dk
    real(8)              :: dk1(3)   !,kps1(3)
    integer              :: k0
    integer              :: kk
    real(8), allocatable :: kpath(:,:)
    print *,'allocate kps(',nk,')'
    allocate(kps(nk))
    allocate(kpath(3,nk))
    kps0 = 0.d0
    k0 = 1
    kpath(1:3,1) = kp(1:3,1)     ! Gamma point
    print *,'TEST:',kpath(1:3,1)

 !   kps1(1:3) = 0.d0
    do k=2,Nks                                         ! over high-symmetry points
     print *,'high-symmetry point',k
     kk = ks(k) - ks(k-1)
!     dk = dlength(kp(1:3,ks(k))-kp(1:3,ks(k)-1))
     dk = dlength(kp(1:3,ks(k))-kp(1:3,ks(k-1)))/dfloat(kk)   !*0.592202062d0/0.471404d0
     dk1(1:3) = (kp(1:3,ks(k))-kp(1:3,ks(k-1)))/dfloat(kk)
     if(k==2) then
      dk = 0.592202062d0/dfloat(kk)
     elseif(k==3) then
      dk = (0.88830309d0-0.592202062d0)/dfloat(kk)
     elseif(k==4) then
      dk = (1.40116512d0-0.88830309d0)/dfloat(kk)
     endif
     print *,'calculate next ',kk,' points'
     do k1=1,kk
!      print *,'k1=',k1
      k0 = k0 + 1 
      print *,'k0=',k0
      kps0 = kps0 + dk
      kps(k0) = kps0
      kpath(1:3,k0) = kp(1:3,ks(k-1)) + dk1(1:3)*k1 
      print *,'set kpath to',kpath(1:3,k0)
     enddo
    enddo 
    kps(nk) = kps0
    
    ! write k-path to file
    open(unit=2,file='kpath.dat')
     write(2,1) k0
     do k = 1,k0
      write(2,2) kps(k),kpath(1:3,k)
     enddo 
    close(unit=2)
 1  format(I5)
 2  format(4F15.5)
   end subroutine calc_kps




    real(8) function dlength(X)
     real(8)    :: X(3)
     dlength = dsqrt(X(1)**2+X(2)**2+X(3)**2)
    end function dlength







   subroutine plot_bands_2
    real(8), parameter      :: BtoA = 0.5291772109d0    ! Bohr in Angstrom
    real(8), parameter      :: RyeV = 13.605693123d0    ! Ry in eV
    real(8), parameter      :: HaeV = 2.d0*RyeV         ! Ha in eV
    integer, parameter      :: nspin = 0                ! spin components
    real(8), parameter      :: EF = 0.d0                ! Fermi level
    real(8)                 :: al(3,3)                  ! lattice vectors
    real(8)                 :: k1(3),k2(3),kx(3)
    integer                 :: k
    integer                 :: ksn
    integer                 :: ibndsteps = 20
    character(1)            :: akps(4)
    al(1:3,1) =  (/        1.0d0,         0.0d0,         0.0d0 /)     
    al(1:3,2) =  (/        0.0d0,         1.0d0,         0.0d0 /)
    al(1:3,3) =  (/        0.0d0,         0.0d0,         1.0d0 /)
    ksn = 4                                             ! high-symmetry points
    akps(1) = "G"
    akps(2) = "K"
    akps(3) = "M"
    akps(4) = "G"
    print *,'nbnd=',nbnd
    print *,'nk=',nk
    open(unit=2,file='bands_tbs.band')
     write(2,3) nbnd,nspin,EF
     write(2,4) ((al(i,j)/BtoA,i=1,3),j=1,3)
     write(2,5) ksn - 1                                       ! number of paths 
     do k=2,ksn
      write(2,6) ibndsteps+1,kp(1:3,ks(k-1)),kp(1:3,ks(k)),akps(k-1),akps(k)      ! steps, pathway k1-k2 
     enddo
     do k=1,ksn-1                                       ! ksn-1 pathways
      call calc_path(k,I1,I2)                           ! set indexes for the path
      call calc_set_k_path(k,k1,k2)                     ! set k1 and k2 points for the path
      do i=I1,I2                                        ! pathway
       call calc_kpoint(i,k1,k2,kx)
       write(2,7) nbnd,kx(1:3)
       write(2,8) bn(1:nbnd,i)/HaeV
      enddo
     enddo
 3  format(I4,I3,F19.8)
 4  format(9F16.9)
 5  format(I3)
 6  format(I3,6F16.10,1x,A1,1x,A1)
 7  format(I3,3F16.10)
 8  format(100F14.8)
   end subroutine plot_bands_2




   subroutine calc_path(k,I1,I2)                               ! set indexes for the path
    integer             :: k
    integer             :: I1                                  ! first k-point 
    integer             :: I2                                  ! last k-point
    print *,'k=',k
    I1 = 1 + (k-1)*ibndsteps
    I2 = I1 +ibndsteps
    print *,'the k-path set to I1=',I1,' I2=',I2
   end subroutine calc_path



   subroutine calc_set_k_path(k,k1,k2)                       ! set k1 and k2 points for the path
    integer             :: k
    real(8)             :: k1(3)
    real(8)             :: k2(3)
    k1(1:3) = kp(1:3,k)
    k2(1:3) = kp(1:3,k+1)
   end subroutine calc_set_k_path




   subroutine calc_kpoint(i,k1,k2,kx)
    integer           :: i
    real(8)           :: k1(3)           ! initial k-point
    real(8)           :: k2(3)           ! final k-point
    real(8)           :: kx(3)           ! current i k-point     
    real(8)           :: dk(3)
    dk(1:3) = (k2(1:3)-k1(1:3))/dfloat(ibndsteps)
    kx(1:3) = k1(1:3) + (i-1)*dk(1:3)   
   end subroutine calc_kpoint




   end module Plot











