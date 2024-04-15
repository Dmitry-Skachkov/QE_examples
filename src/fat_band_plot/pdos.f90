


      Module PDOS_i
        implicit none
        integer, parameter    :: Nx = 21
        integer, parameter    :: Ny = 21
        integer, parameter    :: Nz = 1
        integer               :: Nkp                     ! number of k-points
        integer               :: NE                      ! number of E points
        real(8), allocatable  :: Ei(:)                   ! energy points
        real(8), allocatable  :: kpp(:,:)                ! k-mesh 
        integer, parameter    :: Na = 2                  ! number of types of atoms
        character(2)          :: atom(Na)
        integer               :: Nan(Na)                 ! number of atoms of each kind
        real(8), allocatable  :: P_orb(:,:,:,:,:,:)      ! PDOS 
        integer               :: nkx(3,Nx*Ny*Nz)            
        real(8)               :: kx(Nx),ky(Ny),kz(Nz)
        logical               :: lSO                     ! spin-orbit
        real(8)               :: EVBM
     Contains


       subroutine read_qe_kpdos
         call init
         call read_kpdos_files
         print *,'end of read_kpdos_files'
       end subroutine read_qe_kpdos



       subroutine init                            ! allocate arrays
        integer               :: kk     
        integer               :: i,j,k
        print *
        print *,'PDOS' 
        print *,'atom1=',atom(1)
        print *,'atom2=',atom(2)

        if(lSO) then
         call calc_param('1 ',atom(1),'1(s_j0.5)')          ! read NE and Nk from file
        elseif(.not.lSO) then
         call calc_param('1 ',atom(1), '1(s)     ')         ! read NE and Nk from file
        endif 
        
        if(Nkp /= Nx*Ny*Nz) then
         print*,'ERROR in Nkp'
         stop
        endif

        print *,'NE=',NE

        allocate(P_orb(Nx,Ny,Nz,NE,5,2))                    ! k-mesh, 5 orbitals for 2 atoms
        allocate(Ei(NE))
        allocate(kpp(3,Nkp))

        P_orb(:,:,:,:,:,:) = 0.d0
        Ei(:) = 0.d0

        kk = 0                           ! store indexes for d.k. files
        do i=1,Nx
         do j=1,Ny
          do k=1,Nz
           kk = kk + 1
           nkx(1,kk) = i
           nkx(2,kk) = j
           nkx(3,kk) = k
          enddo
         enddo
        enddo
        print *
        print *,'Check k points'
        do kk=1,Nkp
         print 1,kk,nkx(1,kk),nkx(2,kk),nkx(3,kk) 
        enddo

        call gen_kmesh                       ! generate k-mesh

 1      format(4I5)        
       end subroutine init




      subroutine gen_kmesh
       real(8)               :: dkx,dky,dkz
       integer               :: i,j,k

       dkx = 1.d0/dfloat(Nx-1)
       dky = 1.d0/dfloat(Ny-1)
       if(Nz == 1) then
        dkz = 0.d0
       else      
        dkz = 1.d0/dfloat(Nz-1)
       endif
        
       do i = 1,Nx 
        kx(i) = dfloat(i-11)*dkx
       enddo 
       do j = 1,Ny 
        ky(j) = dfloat(j-11)*dky
       enddo    
       do k = 1,Nz 
        kz(k) = dfloat(k-11)*dkz
       enddo
      end subroutine gen_kmesh





    subroutine read_kpdos_files                               ! read kpdos files from QE
       integer       :: ia,i

     if(.not.lSO) then

      if(trim(adjustl(atom(1)))=='Ga') then
       do ia = 1,Nan(1)     !4          ! 4 Ga atoms
         call read_kpdos(fstr(ia),atom(1),1,'1(s)     ',1)  
         call read_kpdos(fstr(ia),atom(1),1,'3(p)     ',2)     ! for 2,3,4 orbitals
         call read_kpdos(fstr(ia),atom(1),1,'5(d)     ',5)
       enddo
      endif 

      if(trim(adjustl(atom(2)))=='Se') then
       do ia = Nan(1)+1,Nan(1)+Nan(2)          ! 4 Se atoms
         call read_kpdos(fstr(ia),atom(2),2,'1(s)     ',1)  
         call read_kpdos(fstr(ia),atom(2),2,'3(p)     ',2)
         call read_kpdos(fstr(ia),atom(2),2,'5(d)     ',5)  !
       enddo
      elseif(trim(adjustl(atom(2)))=='S') then
       do ia = Nan(1)+1,Nan(1)+Nan(2)          ! 4 S atoms
         call read_kpdos(fstr(ia),atom(2),2,'1(s)     ',1)  
         call read_kpdos(fstr(ia),atom(2),2,'2(p)     ',2)      ! pz,px,py orbitals for 2,3,4
         call read_kpdos(fstr(ia),atom(2),2,'3(s)     ',1)
         call read_kpdos(fstr(ia),atom(2),2,'5(d)     ',5)  !
       enddo
      elseif(trim(adjustl(atom(2)))=='Te') then
       do ia = Nan(1)+1,Nan(1)+Nan(2)          ! 4 Te atoms
         call read_kpdos(fstr(ia),atom(2),2,'1(s)     ',1)  
         call read_kpdos(fstr(ia),atom(2),2,'2(p)     ',2)      ! pz,px,py orbitals for 2,3,4
       enddo
      endif 

     elseif(lSO) then
       do ia = 1,Nan(1)                                         ! 4 Ga atoms
         call read_kpdos(fstr(ia),atom(1),1,'1(s_j0.5)',1)  
         call read_kpdos(fstr(ia),atom(1),1,'2(p_j0.5)',2)
         call read_kpdos(fstr(ia),atom(1),1,'3(p_j1.5)',3)
         call read_kpdos(fstr(ia),atom(1),1,'4(d_j1.5)',4)
         call read_kpdos(fstr(ia),atom(1),1,'5(d_j2.5)',5)  !
       enddo

       do ia = Nan(1)+1,Nan(1)+Nan(2)                            ! 4 Se atoms
         call read_kpdos(fstr(ia),atom(2),2,'1(s_j0.5)',1)  
         call read_kpdos(fstr(ia),atom(2),2,'2(p_j0.5)',2)
         call read_kpdos(fstr(ia),atom(2),2,'3(p_j1.5)',3)
         call read_kpdos(fstr(ia),atom(2),2,'4(d_j1.5)',4)
         call read_kpdos(fstr(ia),atom(2),2,'5(d_j2.5)',5)  !
       enddo
     endif
       
       do i=1,NE
        Ei(i) = Ei(i) - EVBM
       enddo
    end subroutine read_kpdos_files










       subroutine calc_param(Na,At,Orb)   ! read NE and Nk numbers from file
        character(2)  :: Na
        character(2)  :: At
        character(9)  :: Orb
        integer       :: io
        integer       :: nlines
        character(6)  :: spin
        integer       :: i
        print *,'calc_param'
        call open_file(2,"d.k.pdos_atm#"//trim(adjustl(Na))//"("//trim(adjustl(At))//")_wfc#"//trim(adjustl(Orb)))
        nlines = 0
        do 
         read(2,*,iostat=io)
         if(io/=0) exit
         nlines = nlines + 1
        enddo
        print *,nlines,' lines in the file'
        rewind(unit=2)
        do i=1,nlines-2
         read(2,*) 
        enddo
        read(2,*) Nkp
        NE = (nlines - 1)/Nkp - 1
        print *,NE,' energy points and ',Nkp,' k-points'
        rewind(unit=2)
!        read(2,1) spin
!        if(spin == 'ldosup') then
!         ns = 2
!        elseif(spin == ' ldos(') then
!         ns = 1
!        else
!         print *,'ERROR:  spin=', spin
!         stop
!        endif
        close(unit=2)
 1      format(16x,A6)
       end subroutine calc_param
 













       subroutine read_kpdos(Na,At,Nat,Orb,korb)
        character(2)  :: Na           ! number of atom in the structure
        character(2)  :: At           ! atom name 
        integer       :: Nat          ! number of kind of atoms
        character(9)  :: Orb          ! orbital
        real(8)       :: PDOSx,Ex     !,PDOSx2
        integer       :: korb         ! number of orbital (1,2,3 for s,p,d)
        real(8)       :: pz,px,py                
        integer       :: k,i
        print *,'read_kpdos: Na=',Na
        print *,'read_kpdos: At=',At
        print *,'read_kpdos: Nat=',Nat
        print *,'read_kpdos: Orb=',Orb
        print *,'read_kpdos: korb=',korb
        print *,'read_kpdos: opening file ',"d.k.pdos_atm#"//trim(adjustl(Na))//"("//trim(adjustl(At))//")_wfc#"//trim(adjustl(Orb))
        call open_file(2,"d.k.pdos_atm#"//trim(adjustl(Na))//"("//trim(adjustl(At))//")_wfc#"//trim(adjustl(Orb)))
         read(2,*)
         do k=1,Nkp
          do i=1,NE
           if(korb == 2) then   ! pz, px, py orbitals
            read(2,1) Ex,PDOSx,pz,px,py
            P_orb(nkx(1,k),nkx(2,k),nkx(3,k),i,korb,Nat) = &
            P_orb(nkx(1,k),nkx(2,k),nkx(3,k),i,korb,Nat) +     pz
            P_orb(nkx(1,k),nkx(2,k),nkx(3,k),i,korb+1,Nat) = &
            P_orb(nkx(1,k),nkx(2,k),nkx(3,k),i,korb+1,Nat) +   px
            P_orb(nkx(1,k),nkx(2,k),nkx(3,k),i,korb+2,Nat) = &
            P_orb(nkx(1,k),nkx(2,k),nkx(3,k),i,korb+2,Nat) +   py
           else                 !   s and d orbitals
            read(2,1) Ex,PDOSx
            P_orb(nkx(1,k),nkx(2,k),nkx(3,k),i,korb,Nat) =    &
            P_orb(nkx(1,k),nkx(2,k),nkx(3,k),i,korb,Nat) + PDOSx
           endif    
            Ei(i) = Ex      
          enddo
          read(2,*)
         enddo
        close(unit=2)
        print *,'read ',Nkp,' k-points and ',NE,' energy points'
 1      format(5x,F9.3,10E11.3)
       end subroutine read_kpdos













     character(len=2) function fstr(k)                             !   Convert an integer to character*2
      integer, intent(in) :: k
      write (fstr,'(I2)') k
     end function fstr





      subroutine open_file(un,name)
       integer      :: un
       character(*) :: name
       logical      :: L_file
       if(L_check_file(trim(adjustl(name)))) then
        print *,' Open file ',name
        open(unit=un,file=trim(adjustl(name)))
        L_file = .true.
       else
        print *,'ERROR:  File ',trim(adjustl(name)),' does not exist'
        stop
       endif
      end subroutine open_file





    logical function L_check_file(name)
     character(*)   name
     inquire(file=trim(adjustl(name)),EXIST=L_check_file)
    end function L_check_file



   subroutine getargI(i,N)                ! read integer from argument line
    integer       :: i
    integer       :: N
    character(7)  :: Nc
    call getarg(i,Nc)
    read(Nc,*) N
   end subroutine getargI



   subroutine getargR(i,R)                ! read real from argument line
    integer       :: i
    real(8)       :: R
    character(10)  :: Rc
    call getarg(i,Rc)
    read(Rc,*) R
   end subroutine getargR






    end module pdos_i
 



