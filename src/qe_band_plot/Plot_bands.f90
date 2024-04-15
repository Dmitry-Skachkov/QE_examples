



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







   
 

   end module Plot











