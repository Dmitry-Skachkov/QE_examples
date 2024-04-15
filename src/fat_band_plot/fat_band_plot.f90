





       Program fat_band_plot                             ! convert bands.dat to Tecplot format for plotting
        use Kpath_module
        use Plot
        use PDOS_i
        integer                   :: nSO

        call  getarg (1,atom(1))                         ! atom type 1
        call  getargi(2,Nan(1))                          ! number of atoms of type 1
        call  getarg (3,atom(2))                         ! atom type 2
        call  getargi(4,Nan(2))                          ! number of atoms of type 2
        call  getargi(5,nSO)                             ! number of atoms of type 2
        call  getargR(6,EVBM)                            ! E VBM to set to zero

        if(nSO == 1) then
         lSO = .false.
        elseif(nSO == 2) then
         lSO = .true.
        endif

        call read_kpath                                  ! the kpath.dat file is created in qe_band_plot
        call read_qe_kpdos
        print *,'call plot_bands'
        call plot_bands                                  ! TecPlot format
       end Program fat_band_plot





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



     character(len=2) function fstr(k)                             !   Convert an integer to character*2
      integer, intent(in) :: k
      write (fstr,'(I2)') k
     end function fstr











