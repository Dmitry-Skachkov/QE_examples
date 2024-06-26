!***************************************************************************************************
!>
!  A suite of Fortran routines for interpolating values between
!  one-, two-, three-, and four-dimensional arrays defined on uniform or nonuniform
!  orthogonal grids. This operation is commonly referred to as "regridding." Linear
!  or cubic interpolation can be selected independently in each dimension.
!  Extrapolation is not allowed. The subroutines in REGRIDPACK cannot be used to
!  transfer values on nonorthogonal (randomly scattered) data grids.
!
!### History
!  * John C. Adams (NCAR 1997) : original REGRIDPACK
!  * Jacob Williams, Oct 2019, modernized and refactored

    module regridpack_module

    use iso_fortran_env, only: wp => real64

    implicit none

    private

    interface regrid

        !low level routines:
        module procedure :: rgrd1, rgrd2, rgrd3, rgrd4
        module procedure :: rgrd1u, rgrd2u, rgrd3u, rgrd4u

        module procedure :: rgrd1_wrapper, rgrd2_wrapper, rgrd3_wrapper, rgrd4_wrapper

    end interface
    public :: regrid

    contains
!***************************************************************************************************

!**************************************************************************
!>
!  Wrapper to rgrd1.  Allocates the work arrays internally.

    subroutine rgrd1_wrapper(x,p,xx,q,intpol,ier)

    implicit none

    real(wp),dimension(:),intent(in)     :: x            !! original x
    real(wp),dimension(:),intent(in)     :: p            !! original p(x)
    real(wp),dimension(:),intent(in)     :: xx           !! regridded xx
    real(wp),dimension(:),intent(out)    :: q            !! regridded q(xx)
    integer,intent(in)                   :: intpol
    integer,intent(out)                  :: ier          !! status code:
                                                         !!
                                                         !! * 0    : no errors
                                                         !! * 1-6 : error [see original code]
                                                         !! * 10  : input vectors are the wrong size
                                                         !! * 100 : out of memory

    integer :: lw, liw
    integer :: nx, mx
    integer :: np, nq
    real(wp),dimension(:),allocatable :: w
    integer,dimension(:),allocatable :: iw
    integer :: ierr1, ierr2

    !get array sizes:

    nx = size(x)
    np = size(p)

    mx = size(xx)
    nq = size(q)

    if (nx/=np .or. mx/=nq) then
        !Error: vectors are the wrong size
        ier = 10
        return
    end if

    !allocate work matrices:

    select case(intpol)
    case(1)
        lw = mx
    case(3)
        lw = 4*mx
    case default
        ier = 6     !Error: invalid intpol value
        return
    end select

    liw = mx

    allocate(w(lw),   stat=ierr1)
    allocate(iw(liw), stat=ierr2)

    if (ierr1==0 .and. ierr2==0) then
        !call the main routine:
        call rgrd1(nx,x,p,mx,xx,q,intpol,w,lw,iw,liw,ier)
    else
        !error: out of memory
        ier = 100
    end if

    !clean up:
    if (allocated(w)) deallocate(w)
    if (allocated(iw)) deallocate(iw)

    end subroutine rgrd1_wrapper
!**************************************************************************

!**************************************************************************
!>
!  Wrapper to rgrd2.  Allocates the work arrays internally.

    subroutine rgrd2_wrapper(x,y,p,xx,yy,q,intpol,ier)

    implicit none

    real(wp),dimension(:),intent(in)     :: x              !! original x
    real(wp),dimension(:),intent(in)     :: y              !! original y
    real(wp),dimension(:,:),intent(in)   :: p              !! original p(x,y)
    real(wp),dimension(:),intent(in)     :: xx             !! regridded xx
    real(wp),dimension(:),intent(in)     :: yy             !! regridded yy
    real(wp),dimension(:,:),intent(out)  :: q              !! regridded q(xx,yy)
    integer,dimension(2),intent(in)      :: intpol
    integer,intent(out)                  :: ier            !! * 0    : no errors
                                                           !! * 1-6 : error [see original code]
                                                           !! * 10  : input vectors are the wrong size
                                                           !! * 100 : out of memory

    integer :: lw, liw
    integer :: nx, ny, mx, my
    integer,dimension(2) :: np, nq
    integer :: lwx, lwy
    real(wp),dimension(:),allocatable :: w
    integer,dimension(:),allocatable :: iw
    integer :: ierr1, ierr2

    !get array sizes:

    nx = size(x)
    ny = size(y)
    np(1) = size(p,1)
    np(2) = size(p,2)

    mx = size(xx)
    my = size(yy)
    nq(1) = size(q,1)
    nq(2) = size(q,2)

    if (nx/=np(1) .or. ny/=np(2) .or. mx/=nq(1) .or. my/=nq(2)) then

        !Error: vectors are the wrong size
        ier = 10
        return

    end if

    !allocate work matrices:

    select case(intpol(1))
    case(1)
        lwx = mx
    case(3)
        lwx = 4*mx
        case default
        ier = 6     !Error: invalid intpol value
        return
    end select

    select case(intpol(2))
    case(1)
        lwy = my+2*mx
    case(3)
        lwy = 4*(mx+my)
    end select

    lw  = lwx + lwy
    liw = mx + my

    allocate(w(lw),   stat=ierr1)
    allocate(iw(liw), stat=ierr2)

    if (ierr1==0 .and. ierr2==0) then

        !call the main routine:
        call rgrd2(nx,ny,x,y,p,mx,my,xx,yy,q,intpol,w,lw,iw,liw,ier)

    else

        !error: out of memory
        ier = 100

    end if

    !clean up:

    deallocate(w)
    deallocate(iw)

    end subroutine rgrd2_wrapper
!**************************************************************************

!**************************************************************************
!>
!  Wrapper to rgrd3.  Allocates the work arrays internally.

    subroutine rgrd3_wrapper(x,y,z,p,xx,yy,zz,q,intpol,ier)

    implicit none

    real(wp),dimension(:),intent(in)              :: x            !! original x
    real(wp),dimension(:),intent(in)              :: y            !! original y
    real(wp),dimension(:),intent(in)              :: z            !! original z
    real(wp),dimension(:,:,:),intent(in)          :: p            !! original p(x,y,z)
    real(wp),dimension(:),intent(in)              :: xx           !! regridded xx
    real(wp),dimension(:),intent(in)              :: yy           !! regridded yy
    real(wp),dimension(:),intent(in)              :: zz           !! regridded zz
    real(wp),dimension(:,:,:),intent(out)         :: q            !! regridded q(xx,yy,zz)
    integer,dimension(3),intent(in)               :: intpol
    integer,intent(out)                           :: ier          !! * 0   : no errors
                                                                  !! * 1-6 : error [see original code]
                                                                  !! * 10  : input vectors are the wrong size
                                                                  !! * 100 : out of memory

    integer :: nx, ny, nz, mx, my, mz
    integer,dimension(3) :: np, nq
    integer :: lw, liw
    integer :: lwx, lwy, lwz
    real(wp),dimension(:),allocatable :: w
    integer,dimension(:),allocatable :: iw
    integer :: ierr1, ierr2

    !get array sizes:

    nx = size(x)
    ny = size(y)
    nz = size(z)
    np(1) = size(p,1)
    np(2) = size(p,2)
    np(3) = size(p,3)

    mx = size(xx)
    my = size(yy)
    mz = size(zz)
    nq(1) = size(q,1)
    nq(2) = size(q,2)
    nq(3) = size(q,3)

    if (nx/=np(1) .or. ny/=np(2) .or. nz/=np(3) .or. mx/=nq(1) .or. my/=nq(2) .or. mz/=nq(3)) then

        !Error: vectors are the wrong size
        ier = 10
        return

    end if

    !allocate work matrices:

    select case(intpol(1))
    case(1)
        lwx = mx
    case(3)
        lwx = 4*mx
        case default
        ier = 6     !Error: invalid intpol value
        return
    end select

    select case(intpol(2))
    case(1)
        lwy = my+2*mx
    case(3)
        lwy = 4*(mx+my)
    end select
    select case(intpol(3))
    case(1)
        lwz = 2*mx*my+mz
    case(3)
        lwz = 4*(mx*my+mz)
    end select

    lw  = lwx + lwy + lwz
    liw = mx + my + mz

    allocate(w(lw),   stat=ierr1)
    allocate(iw(liw), stat=ierr2)

    if (ierr1==0 .and. ierr2==0) then

        !call the main routine:
        call rgrd3(nx,ny,nz,x,y,z,p,mx,my,mz,xx,yy,zz,q,intpol,w,lw,iw,liw,ier)

    else

        !error: out of memory
        ier = 100

    end if

    !clean up:

    deallocate(w)
    deallocate(iw)

    end subroutine rgrd3_wrapper
!**************************************************************************

!**************************************************************************
!>
!  Wrapper to rgrd4.  Allocates the work arrays internally.

    subroutine rgrd4_wrapper(x,y,z,t,p,xx,yy,zz,tt,q,intpol,ier)

    implicit none

    real(wp),dimension(:),intent(in)          :: x            !! original x
    real(wp),dimension(:),intent(in)          :: y            !! original y
    real(wp),dimension(:),intent(in)          :: z            !! original z
    real(wp),dimension(:),intent(in)          :: t            !! original t
    real(wp),dimension(:,:,:,:),intent(in)    :: p            !! original p(x,y,z,t)
    real(wp),dimension(:),intent(in)          :: xx           !! regridded xx
    real(wp),dimension(:),intent(in)          :: yy           !! regridded yy
    real(wp),dimension(:),intent(in)          :: zz           !! regridded zz
    real(wp),dimension(:),intent(in)          :: tt           !! regridded tt
    real(wp),dimension(:,:,:,:),intent(out)   :: q            !! regridded q(xx,yy,zz,tt)
    integer,dimension(4),intent(in)           :: intpol
    integer,intent(out)                       :: ier          !! * 0    : no errors
                                                              !! * 1-6 : error [see original code]
                                                              !! * 10  : input vectors are the wrong size
                                                              !! * 100 : out of memory

    integer :: nx, ny, nz, nt, mx, my, mz, mt
    integer,dimension(4) :: np, nq
    integer :: lw, liw
    integer :: lwx, lwy, lwz, lwt
    real(wp),dimension(:),allocatable :: w
    integer,dimension(:),allocatable :: iw
    integer :: ierr1, ierr2

    !get array sizes:

    nx = size(x)
    ny = size(y)
    nz = size(z)
    nt = size(t)

    np(1) = size(p,1)
    np(2) = size(p,2)
    np(3) = size(p,3)
    np(4) = size(p,4)

    mx = size(xx)
    my = size(yy)
    mz = size(zz)
    mt = size(tt)

    nq(1) = size(q,1)
    nq(2) = size(q,2)
    nq(3) = size(q,3)
    nq(4) = size(q,4)

    if (nx/=np(1) .or. ny/=np(2) .or. nz/=np(3) .or. nt/=np(4) .or. &
        mx/=nq(1).or. my/=nq(2) .or. mz/=nq(3) .or. mt/=nq(4)) then

        !Error: vectors are the wrong size
        ier = 10
        return

    end if

    !allocate work matrices:

    select case(intpol(1))
    case(1)
        lwx = mx
    case(3)
        lwx = 4*mx
        case default
        ier = 6     !Error: invalid intpol value
        return
    end select

    select case(intpol(2))
    case(1)
        lwy = my+2*mx
    case(3)
        lwy = 4*(mx+my)
    end select

    select case(intpol(3))
    case(1)
        lwz = 2*mx*my+mz
    case(3)
        lwz = 4*(mx*my+mz)
    end select

    select case(intpol(4))
    case(1)
        lwt = 2*mx*my*mz+mt
    case(3)
        lwt = 4*(mx*my*mz+mt)
    end select

    lw  = lwx + lwy + lwz + lwt
    liw = mx + my + mz + mt

    allocate(w(lw),   stat=ierr1)
    allocate(iw(liw), stat=ierr2)

    if (ierr1==0 .and. ierr2==0) then

        !call the main routine:
        call rgrd4(nx,ny,nz,nt,x,y,z,t,p,mx,my,mz,mt,xx,yy,zz,tt,q,intpol,w,lw,iw,liw,ier)

    else

        !error: out of memory
        ier = 100

    end if

    !clean up:

    deallocate(w)
    deallocate(iw)

    end subroutine rgrd4_wrapper
!**************************************************************************

!**************************************************************************
!>
!  subroutine rgrd1 interpolates the values p(i) on the grid x(i)
!  for i=1,...,nx onto q(ii) on the grid xx(ii),ii=1,...,mx.
!
!### requirements
!
!  x must be a strictly increasing grid and xx must be an increasing
!  grid (see ier = 4).  in addition the interval
!
!    [xx(1),xx(mx)]
!
!  must lie within the interval
!
!    [x(1),x(nx)].
!
!  extrapolation is not allowed (see ier=3).  if these intervals
!  are identical and the x and xx grids are UNIFORM then subroutine
!  rgrd1u should be used in place of rgrd1.

    subroutine rgrd1(nx,x,p,mx,xx,q,intpol,w,lw,iw,liw,ier)

    implicit none

    integer,intent(in) :: nx                       !! the integer dimension of the grid
                                                   !! vector x and the dimension of p.
                                                   !! nx > 1 if intpol = 1 or nx > 3 if
                                                   !! intpol = 3 is required.
    real(wp),dimension(nx),intent(in) :: x         !! a real(wp) nx vector of strictly
                                                   !! increasing values which defines the x
                                                   !! grid on which p is given.
    real(wp),dimension(nx),intent(in) :: p         !! a real(wp) nx vector of values given on the x grid
    integer,intent(in) :: mx                       !! the integer dimension of the grid vector
                                                   !! xx and the dimension of q.
                                                   !! mx > 0 is required.
    real(wp),dimension(mx),intent(in)  :: xx       !! a real(wp) mx vector of increasing values which defines the
                                                   !! grid on which q is defined.  xx(1) < x(1) or xx(mx) > x(nx)
                                                   !! is not allowed (see ier = 3)
    integer,intent(in) :: intpol                   !! an integer which sets linear or cubic
                                                   !! interpolation as follows:
                                                   !!
                                                   !! * intpol = 1 sets linear interpolation
                                                   !! * intpol = 3 sets cubic interpolation
                                                   !!
                                                   !! values other than 1 or 3 in intpol are not allowed (ier = 6).
    real(wp),intent(out) :: q(mx)                  !! a real(wp) mx vector of values on the xx grid which are
                                                   !! interpolated from p on the x grid
    integer,intent(in) :: lw                       !! the integer length of the real(wp) work space w.  let
                                                   !!
                                                   !!  * lwmin = mx     if intpol(1) = 1
                                                   !!  * lwmin = 4*mx   if intpol(1) = 3
                                                   !!
                                                   !! then lw must be greater than or equal to lwmin
    real(wp),dimension(lw),intent(inout) :: w      !! a real(wp) work space of length at least
                                                   !! lw which must be provided in the
                                                   !! routine calling rgrd1
    integer,intent(in) :: liw                      !! the length of the integer work space iw.
                                                   !! liw must be greater than or equal to mx.
    integer,dimension(*),intent(inout) :: iw       !! an integer work space of length at least
                                                   !! liw which must be provided in the
                                                   !! routine calling rgrd1
    integer,intent(out) :: ier                     !! an integer error flag set as follows:
                                                   !!
                                                   !! * ier = 0 if no errors in input arguments are detected
                                                   !! * ier = 1 if mx < 1
                                                   !! * ier = 2 if nx < 2 when intpol=1 or nx < 4 when intpol=3
                                                   !! * ier = 3 if xx(1) < x(1) or x(nx) < xx(mx)
                                                   !!    to avoid this flag when end points are intended to be the
                                                   !!    same but may differ slightly due to roundoff error, they
                                                   !!    should be set exactly in the calling routine (e.g., if both
                                                   !!    grids have the same x boundaries then xx(1)=x(1) and xx(mx)=x(nx)
                                                   !!    should be set before calling rgrd1)
                                                   !! * ier = 4 if the x grid is not strictly monotonically increasing
                                                   !!    or if the xx grid is not montonically increasing.  more
                                                   !!    precisely if:
                                                   !!    x(i+1) <= x(i) for some i such that 1 <= i < nx (or)
                                                   !!    xx(ii+1) < xx(ii) for some ii such that 1 <= ii < mx
                                                   !! * ier = 5 if lw or liw is too small (insufficient work space)
                                                   !! * ier = 6 if intpol is not equal to 1 or 3

    integer :: i,ii,i1,i2,i3,i4

    ! check arguments for errors

    ! check xx grid resolution
    ier = 1
    if (mx < 1) return

    ! check intpol
    ier = 6
    if (intpol/=1 .and. intpol/=3) return

    ! check x grid resolution
    ier = 2
    if (intpol==1 .and. nx<2) return
    if (intpol==3 .and. nx<4) return

    ! check xx grid contained in x grid
    ier = 3
    if (xx(1)<x(1) .or. xx(mx)>x(nx)) return

    ! check montonicity of grids
    do i=2,nx
        if (x(i-1)>=x(i)) then
            ier = 4
            return
        end if
    end do
    do ii=2,mx
        if (xx(ii-1)>xx(ii)) then
            ier = 4
            return
        end if
    end do

    ! check minimum work space lengths
    ier = 5
    if (intpol==1) then
        if (lw < mx) return
    else
        if (lw < 4*mx) return
    end if
    if (liw < mx) return

    ! arguments o.k.

    ier = 0

    if (intpol==1) then
        ! linear interpolation in x
        call linmx(nx,x,mx,xx,iw,w)
        call lint1(nx,p,mx,q,iw,w)
    else
        ! cubic interpolation in x
        i1 = 1
        i2 = i1+mx
        i3 = i2+mx
        i4 = i3+mx
        call cubnmx(nx,x,mx,xx,iw,w(i1),w(i2),w(i3),w(i4))
        call cubt1(nx,p,mx,q,iw,w(i1),w(i2),w(i3),w(i4))
    end if

    end subroutine rgrd1
!**************************************************************************

!**************************************************************************
!>
!  linearly interpolate p on x onto q on xx

    subroutine lint1(nx,p,mx,q,ix,dx)

    implicit none

    integer :: mx,ix(mx),nx,ii,i
    real(wp) :: p(nx),q(mx),dx(mx)

    do ii=1,mx
        i = ix(ii)
        q(ii) = p(i)+dx(ii)*(p(i+1)-p(i))
    end do

    end subroutine lint1
!**************************************************************************

!**************************************************************************
!>
! cubically interpolate p on x to q on xx

    subroutine cubt1(nx,p,mx,q,ix,dxm,dx,dxp,dxpp)

    implicit none

    integer :: mx,ix(mx),nx,i,ii
    real(wp) :: p(nx),q(mx),dxm(mx),dx(mx),dxp(mx),dxpp(mx)

    do ii=1,mx
        i = ix(ii)
        q(ii) = dxm(ii)*p(i-1)+dx(ii)*p(i)+dxp(ii)*p(i+1)+dxpp(ii)*p(i+2)
    end do

    end subroutine cubt1
!**************************************************************************

!**************************************************************************
!>
!  set x grid pointers for xx grid and interpolation scale terms

    subroutine linmx(nx,x,mx,xx,ix,dx)

    implicit none

    real(wp) :: x(*),xx(*),dx(*)
    integer :: ix(*),isrt,ii,i,nx,mx

    isrt = 1

    do ii=1,mx
        ! find x(i) s.t. x(i) < xx(ii) <= x(i+1)
        do i=isrt,nx-1
            if (x(i+1) >= xx(ii)) then
                isrt = i
                ix(ii) = i
                exit
            end if
        end do
    end do

    ! set linear scale term
    do ii=1,mx
        i = ix(ii)
        dx(ii) = (xx(ii)-x(i))/(x(i+1)-x(i))
    end do

    end subroutine linmx
!**************************************************************************

!**************************************************************************
!>
!
    subroutine cubnmx(nx,x,mx,xx,ix,dxm,dx,dxp,dxpp)

    implicit none

    real(wp) :: x(*),xx(*),dxm(*),dx(*),dxp(*),dxpp(*)
    integer :: ix(*),mx,nx,i,ii,isrt

    isrt = 1

    do ii=1,mx
        ! set i in [2,nx-2] closest s.t.
        ! x(i-1),x(i),x(i+1),x(i+2) can interpolate xx(ii)
        do i=isrt,nx-1
            if (x(i+1) >= xx(ii)) then
                ix(ii) = min(nx-2,max(2,i))
                isrt = ix(ii)
                exit
            end if
        end do
    end do

    ! set cubic scale terms

    do ii=1,mx
        i = ix(ii)
        dxm(ii) = (xx(ii)-x(i))*(xx(ii)-x(i+1))*(xx(ii)-x(i+2)) /((x(i-1)-x(i))*(x(i-1)-x(i+1))*(x(i-1)-x(i+2)))
        dx(ii) = (xx(ii)-x(i-1))*(xx(ii)-x(i+1))*(xx(ii)-x(i+2))/((x(i)-x(i-1))*(x(i)-x(i+1))*(x(i)-x(i+2)))
        dxp(ii) = (xx(ii)-x(i-1))*(xx(ii)-x(i))*(xx(ii)-x(i+2)) /((x(i+1)-x(i-1))*(x(i+1)-x(i))*(x(i+1)-x(i+2)))
        dxpp(ii) = (xx(ii)-x(i-1))*(xx(ii)-x(i))*(xx(ii)-x(i+1))/((x(i+2)-x(i-1))*(x(i+2)-x(i))*(x(i+2)-x(i+1)))
    end do

    end subroutine cubnmx
!**************************************************************************

!**************************************************************************
!>
!  subroutine rgrd1u interpolates the nx vector p onto
!  the mx vector q. it is assumed that p and q are
!  values on uniform nx and mx grids which subdivide
!  the same interval (INCLUDING END POINTS).  if p and
!  q are values on nonuniform grids and/or if q is defined
!  on a grid which lies within the p grid then subroutine
!  rgrd1 should be used.
!
!### method
!
!  linear or cubic interpolation (see intpol) is used when the
!  mx uniform grid is not a subgrid of the nx uniform grid (i.e.,
!  whenever mx-1 does not divide nx-1).  q is set directly from
!  p in the subgrid case.

    subroutine rgrd1u(nx,p,mx,q,intpol,w,lw,iw,liw,ier)

    implicit none

    integer,intent(in)  :: intpol       !! an integer which sets linear or cubic interpolation as follows:
                                        !!
                                        !! * intpol = 1 sets linear interpolation
                                        !! * intpol = 3 sets cubic interpolation
                                        !!
                                        !! values other than 1 or 3 in intpol are not allowed (ier = 4).
    integer,intent(in)  :: liw          !! the integer length of the integer work space iw in the routine calling rgrd1u.
                                        !! liw must be greater than or equal to mx.
    integer,intent(inout)  :: iw(liw)   !! an integer work space of length liw
    integer,intent(in)  :: lw           !! the integer length of the work space w in the routine calling rgrd1u.
                                        !! if mx-1 divides nx-1 then the mx uniform grid is a subgrid of
                                        !! the nx uniform grid.  in this case let lwmin = 1.  otherwise
                                        !! let lwmin = mx if intpol = 1 or lwmin = mx if intpol = 3.
                                        !! then lw must be greater than or equal to lwmin (see ier=4).
    integer,intent(in) :: nx            !! the integer dimension of p.  nx > 1 if intpol = 1 or
                                        !! nx > 3 if intpol = 3 is required (see ier = 2).
    integer,intent(in)  :: mx           !! the integer dimension of q.  mx > 1 is required (see ier = 1)
    real(wp),intent(in) :: p(nx)        !! a real(wp) nx dimensioned vector of given values
    real(wp),intent(inout) :: w(lw)     !! a real(wp) work space of length lw.
    real(wp),dimension(mx),intent(out) :: q  !! a real(wp) mx dimensioned vector of values which are interpolated from p.
    integer,intent(out) :: ier  !! an integer error flag set as follows:
                                !!
                                !! * ier = 0 if no errors in input arguments are detected
                                !! * ier = 1 if  mx < 2
                                !! * ier = 2 if nx < 2 when intpol=1 or nx < 4 when intpol=3.
                                !! * ier = 3 if intpol is not equal to 1 or 3
                                !! * ier = 4 if lw or liw is too small (insufficient work space)

    integer  :: inmx,isubx,i2,i3,i4,i5,lwmin

    ! check input arguments

    ! check mx
    ier = 1
    if (mx < 2) return

    ! check intpol
    ier = 3
    if (intpol/=1 .and. intpol/=3) return

    ! check nx
    ier = 2
    if (intpol==1 .and. nx<2) return
    if (intpol==3 .and. nx<4) return

    ! set subgrid integer indicator
    inmx = (nx-1)/(mx-1)
    isubx = nx - inmx*(mx-1)

    ! set minimum and check work space
    ier = 4
    if (isubx/=1) then
        if (intpol==1) lwmin = mx
        if (intpol==3) lwmin = 4*mx
    else
        lwmin = 1
    end if
    if (lw < lwmin) return
    if (liw < mx) return

    ! input arguments o.k.

    ier = 0

    ! preset pointers

    i2 = 1
    i3 = 1
    i4 = 1
    i5 = 1
    if (intpol == 1) then
        ! linear interpolation in x
        if (isubx /= 1) then
            call linmxu(nx,mx,iw,w)
        end if
        call lint1u(nx,p,mx,q,iw,w,inmx,isubx)
    else
        ! cubic interpolation in x
        if (isubx /= 1) then
            i2 = 1
            i3 = i2+mx
            i4 = i3+mx
            i5 = i4+mx
            call cubnmxu(nx,mx,iw,w(i2),w(i3),w(i4),w(i5))
        end if
        call cubt1u(nx,p,mx,q,iw,w(i2),w(i3),w(i4),w(i5),inmx,isubx)
    end if

    end subroutine rgrd1u
!**************************************************************************

!**************************************************************************
!>
    subroutine lint1u(nx,p,mx,q,ix,dx,inmx,isubx)

    implicit none

    integer :: nx,mx,ix(mx),inmx,isubx,i,ii
    real(wp) :: p(nx),q(mx),dx(mx)

    if (isubx == 1) then
        ! mx grid is subset of nx grid so q can be set directly
        do ii=1,mx
            i = inmx*(ii-1)+1
            q(ii) = p(i)
        end do
    else
        ! linearly interpolate
        do ii=1,mx
            i = ix(ii)
            q(ii) = p(i)+dx(ii)*(p(i+1)-p(i))
        end do
    end if

    end subroutine lint1u
!**************************************************************************

!**************************************************************************
!>
    subroutine cubt1u(nx,p,mx,q,ix,dxm,dx,dxp,dxpp,inmx,isubx)

    implicit none

    integer  :: nx,mx,ix(mx),inmx,isubx,i,ii
    real(wp) :: p(nx),q(mx)
    real(wp) :: dxm(mx),dx(mx),dxp(mx),dxpp(mx)

    if (isubx == 1) then
        ! mx grid is subset of nx grid so q can be set directly
        do ii=1,mx
            i = inmx*(ii-1)+1
            q(ii) = p(i)
        end do
    else
        ! cubically interpolate on uniform grid
        do ii=1,mx
            i = ix(ii)
            q(ii)=(dxm(ii)*p(i-1)+dx(ii)*p(i)+dxp(ii)*p(i+1)+dxpp(ii)*p(i+2))
        end do
    end if

    end subroutine cubt1u
!**************************************************************************

!**************************************************************************
!>
!  set linear interpolation terms

    subroutine linmxu(nx,mx,ix,dx)

    implicit none

    integer :: nx,mx,ix(mx),i,ii
    real(wp) :: dx(mx),dnx,dmx,x,xx

    ! set "virtual" uniform increments
    dnx = 1.0_wp/(nx-1)
    dmx = 1.0_wp/(mx-1)

    ! set ix(ii) = i  s.t. i,i+1 can interpolate for ii
    do ii=1,mx
        xx = (ii-1)*dmx
        ix(ii) = min(int(xx/dnx)+1,nx-1)
        ! set scale term for linear
        i = ix(ii)
        x = (i-1)*dnx
        dx(ii) = (xx-x)/dnx
    end do

    end subroutine linmxu
!**************************************************************************

!**************************************************************************
!>
! set cubic interpolation terms

    subroutine cubnmxu(nx,mx,ix,dxm,dx,dxp,dxpp)

    implicit none

    integer :: nx,mx,ix(mx),i,ii
    real(wp) :: dxm(mx),dx(mx),dxp(mx),dxpp(mx),dnx,dmx,odnx3
    real(wp) :: xx,xim,xi,xip,xipp

    ! set "virtual" uniform increments
    dnx = 1.0_wp/(nx-1)
    dmx = 1.0_wp/(mx-1)
    odnx3 = 1.0_wp/(6.0_wp*dnx*dnx*dnx)

    ! set i=ix(ii) in [2,nx-2] such that
    ! i-1,i,i+1,i+2 can be used to interpolate at ii
    do ii=1,mx
        xx = (ii-1)*dmx
        ix(ii) = min(max(int(xx/dnx)+1,2),nx-2)
        i = ix(ii)
        ! set scale terms for cubic
        xi = (i-1)*dnx
        xim = xi-dnx
        xip = xi+dnx
        xipp = xip+dnx
        dxm(ii) = -(xx-xi)*(xx-xip)*(xx-xipp)*odnx3
        dx(ii) = 3.0_wp*(xx-xim)*(xx-xip)*(xx-xipp)*odnx3
        dxp(ii) = -3.0_wp*(xx-xim)*(xx-xi)*(xx-xipp)*odnx3
        dxpp(ii) = (xx-xim)*(xx-xi)*(xx-xip)*odnx3
    end do

    end subroutine cubnmxu
!**************************************************************************

!**************************************************************************
!>
!  subroutine rgrd2 interpolates the values p(i,j) on the orthogonal
!  grid (x(i),y(j)) for i=1,...,nx and j=1,...,ny onto q(ii,jj) on the
!  orthogonal grid (xx(ii),yy(jj)) for ii=1,...,mx and jj=1,...,my.
!
!### method
!
!  linear or cubic interpolation is used (independently) in
!  each direction (see argument intpol).
!
!### requirements
!
!  each of the x,y grids must be strictly montonically increasing
!  and each of the xx,yy grids must be montonically increasing (see
!  ier = 4).  in addition the (X,Y) region
!
!    [xx(1),xx(mx)] X [yy(1),yy(my)]
!
!  must lie within the (X,Y) region
!
!    [x(1),x(nx)] X [y(1),y(ny)].
!
!  extrapolation is not allowed (see ier=3).  if these (X,Y)
!  regions are identical and the orthogonal grids are UNIFORM
!  in each direction then subroutine rgrd2u
!  should be used instead of rgrd2.

    subroutine rgrd2(nx,ny,x,y,p,mx,my,xx,yy,q,intpol,w,lw,iw,liw,ier)

    implicit none

    integer,intent(in)  :: nx      !! the integer dimension of the grid vector x and the first dimension
                                   !! of p.  nx > 1 if intpol(1) = 1 or nx > 3 if intpol(1) = 3 is required.
    integer,intent(in)  :: ny      !! the integer dimension of the grid vector y and the second dimension
                                   !! of p.  ny > 1 if intpol(2) = 1 or ny > 3 if intpol(2) = 3 is required.
    integer,intent(in)  :: mx      !! the integer dimension of the grid vector xx and the first dimension
                                   !! of q.  mx > 0 is required.
    integer,intent(in)  :: my      !! the integer dimension of the grid vector yy and the second dimension
                                   !! of q.  my > 0 is required.
    integer,intent(in)  :: lw      !! the integer length of the real(wp) work space w.  let
                                   !!
                                   !! * lwx = mx                if intpol(1) = 1
                                   !! * lwx = 4*mx              if intpol(1) = 3
                                   !! * lwy = my+2*mx           if intpol(2) = 1
                                   !! * lwy = 4*(mx+my)         if intpol(2) = 3
                                   !!
                                   !! then lw must be greater than or equal to lwx+lwy
    integer,intent(in)  :: liw  !! the integer length of the integer work space iw.  liw must be at least mx+my
    integer,intent(out)  :: ier  !! an integer error flag set as follows:
                                 !!
                                 !! * ier = 0 if no errors in input arguments are detected
                                 !! * ier = 1 if  min(mx,my) < 1
                                 !! * ier = 2 if nx < 2 when intpol(1)=1 or nx < 4 when intpol(1)=3 (or)
                                 !!   ny < 2 when intpol(2)=1 or ny < 4 when intpol(2)=3
                                 !! * ier = 3 if xx(1) < x(1) or x(nx) < xx(mx) (or)
                                 !!   yy(1) < y(1) or y(ny) < yy(my) (or)
                                 !!   to avoid this flag when end points are intended to be the
                                 !!   same but may differ slightly due to roundoff error, they
                                 !!   should be set exactly in the calling routine (e.g., if both
                                 !!   grids have the same y boundaries then yy(1)=y(1) and yy(my)=y(ny)
                                 !!   should be set before calling rgrd2)
                                 !! * ier = 4 if one of the grids x,y is not strictly monotonically
                                 !!   increasing or if one of the grids xx,yy is not
                                 !!   montonically increasing.  more precisely if:
                                 !!
                                 !!    * x(i+1) <= x(i) for some i such that 1 <= i < nx (or)
                                 !!    * y(j+1) <= y(j) for some j such that 1 <= j < ny (or)
                                 !!    * xx(ii+1) < xx(ii) for some ii such that 1 <= ii < mx (or)
                                 !!    * yy(jj+1) < yy(jj) for some jj such that 1 <= jj < my
                                 !! * ier = 5 if lw or liw is to small (insufficient work space)
                                 !! * ier = 6 if intpol(1) or intpol(2) is not equal to 1 or 3
    integer,intent(in)  :: intpol(2)    !! an integer vector of dimension 2 which sets linear or cubic
                                        !! interpolation in the x,y directions as follows:
                                        !!
                                        !! * intpol(1) = 1 sets linear interpolation in the x direction
                                        !! * intpol(1) = 3 sets cubic interpolation in the x direction.
                                        !! * intpol(2) = 1 sets linear interpolation in the y direction
                                        !! * intpol(2) = 3 sets cubic interpolation in the y direction.
                                        !!
                                        !! values other than 1 or 3 in intpol are not allowed (ier = 5).
    integer,intent(inout)  :: iw(liw)   !! an integer work space of length at least
                                        !! liw which must be provided in the
                                        !! routine calling rgrd2
    real(wp),intent(in) :: x(nx)    !! a real(wp) nx vector of strictly increasing values which defines the x
                                    !! portion of the orthogonal grid on which p is given
    real(wp),intent(in) :: y(ny)    !! a real(wp) ny vector of strictly increasing values which defines the y
                                    !! portion of the orthogonal grid on which p is given
    real(wp),intent(in) :: p(nx,ny) !! a real(wp) nx by ny array of values given on the orthogonal (x,y) grid
    real(wp),intent(in) :: xx(mx)   !! a real(wp) mx vector of increasing values which defines the x portion of the
                                    !! orthogonal grid on which q is defined.  xx(1) < x(1) or xx(mx) > x(nx)
                                    !! is not allowed (see ier = 3)
    real(wp),intent(in) :: yy(my)   !! a real(wp) my vector of increasing values which defines the y portion of the
                                    !! orthogonal grid on which q is defined.  yy(1) < y(1) or yy(my) > y(ny)
                                    !! is not allowed (see ier = 3)
    real(wp),intent(out) :: q(mx,my)    !! a real(wp) mx by my array of values on the (xx,yy) grid which are
                                        !! interpolated from p on the (x,y) grid
    real(wp),intent(inout) :: w(lw)     !! a real(wp) work space of length at least
                                        !! lw which must be provided in the
                                        !! routine calling rgrd2

    integer  :: i,ii,j,jj,j2,j3,j4,j5,j6,j7,j8,j9,i2,i3,i4,i5
    integer  :: jy,lwx,lwy

    ! check input arguments

    ! check (xx,yy) grid resolution
    ier = 1
    if (min(mx,my) < 1) return

    ! check intpol
    ier = 6
    if (intpol(1)/=1 .and. intpol(1)/=3) return
    if (intpol(2)/=1 .and. intpol(2)/=3) return

    ! check (x,y) grid resolution
    ier = 2
    if (intpol(1)==1 .and. nx<2) return
    if (intpol(1)==3 .and. nx<4) return
    if (intpol(2)==1 .and. ny<2) return
    if (intpol(2)==3 .and. ny<4) return

    ! check work space lengths
    ier = 5
    if (intpol(1)==1) then
        lwx = mx
    else
        lwx = 4*mx
    end if
    if (intpol(2)==1) then
        lwy = my+2*mx
    else
        lwy = 4*(mx+my)
    end if
    if (lw < lwx+lwy) return
    if (liw < mx+my) return

    ! check (xx,yy) grid contained in (x,y) grid
    ier = 3
    if (xx(1)<x(1) .or. xx(mx)>x(nx)) return
    if (yy(1)<y(1) .or. yy(my)>y(ny)) return

    ! check montonicity of grids
    ier = 4
    do i=2,nx
        if (x(i-1)>=x(i)) return
    end do
    do j=2,ny
        if (y(j-1)>=y(j)) return
    end do
    do ii=2,mx
        if (xx(ii-1)>xx(ii)) return
    end do
    do jj=2,my
        if (yy(jj-1)>yy(jj)) return
    end do

    ! arguments o.k.

    ier = 0

    ! set pointer in integer work space

    jy = mx+1
    if (intpol(2) ==1) then

        ! linearly interpolate in y

        j2 = 1
        j3 = j2
        j4 = j3+my
        j5 = j4
        j6 = j5
        j7 = j6
        j8 = j7+mx
        j9 = j8+mx

        ! set y interpolation indices and scales and linearly interpolate

        call linmx(ny,y,my,yy,iw(jy),w(j3))
        i2 = j9

        ! set work space portion and indices which depend on x interpolation

        if (intpol(1) == 1) then
            i3 = i2
            i4 = i3
            i5 = i4
            call linmx(nx,x,mx,xx,iw,w(i3))
        else
            i3 = i2+mx
            i4 = i3+mx
            i5 = i4+mx
            call cubnmx(nx,x,mx,xx,iw,w(i2),w(i3),w(i4),w(i5))
        end if
        call lint2(nx,ny,p,mx,my,q,intpol,iw(jy),w(j3),w(j7),w(j8),iw,w(i2),w(i3),w(i4),w(i5))

    else

        ! cubically interpolate in y, set indice pointers

        j2 = 1
        j3 = j2+my
        j4 = j3+my
        j5 = j4+my
        j6 = j5+my
        j7 = j6+mx
        j8 = j7+mx
        j9 = j8+mx
        call cubnmx(ny,y,my,yy,iw(jy),w(j2),w(j3),w(j4),w(j5))
        i2 =  j9+mx

        ! set work space portion and indices which depend on x interpolation

        if (intpol(1) == 1) then
            i3 = i2
            i4 = i3
            i5 = i4
            call linmx(nx,x,mx,xx,iw,w(i3))
        else
            i3 = i2+mx
            i4 = i3+mx
            i5 = i4+mx
            call cubnmx(nx,x,mx,xx,iw,w(i2),w(i3),w(i4),w(i5))
        end if
        call cubt2(nx,ny,p,mx,my,q,intpol,iw(jy),w(j2),w(j3),&
                   w(j4),w(j5),w(j6),w(j7),w(j8),w(j9),iw,w(i2),w(i3),w(i4),w(i5))

    end if

    end subroutine rgrd2
!**************************************************************************

!**************************************************************************
!>
!  linearly interpolate in y

    subroutine lint2(nx,ny,p,mx,my,q,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)

    implicit none

    integer  :: nx,ny,mx,my,intpol(2),jy(my),ix(mx)
    integer  :: jsave,j,jj,ii
    real(wp) :: p(nx,ny),q(mx,my)
    real(wp) :: pj(mx),pjp(mx),dy(my)
    real(wp) :: dxm(mx),dx(mx),dxp(mx),dxpp(mx)

    if (intpol(1)==1) then

        ! linear in x

        jsave = -1
        do jj=1,my
            j = jy(jj)
            if (j==jsave) then
                ! j pointer has not moved since last pass (no updates or interpolation)
            else if (j==jsave+1) then
                ! update j and interpolate j+1
                do ii=1,mx
                    pj(ii) = pjp(ii)
                end do
                call lint1(nx,p(1,j+1),mx,pjp,ix,dx)
            else
                ! interpolate j,j+1in pj,pjp on xx mesh
                call lint1(nx,p(1,j),mx,pj,ix,dx)
                call lint1(nx,p(1,j+1),mx,pjp,ix,dx)
            end if

            ! save j pointer for next pass

            jsave = j

            ! linearly interpolate q(ii,jj) from pjp,pj in y direction

            do ii=1,mx
                q(ii,jj) = pj(ii)+dy(jj)*(pjp(ii)-pj(ii))
            end do
        end do

    else

        ! cubic in x

        jsave = -1
        do jj=1,my
            j = jy(jj)
            if (j==jsave) then
                ! j pointer has not moved since last pass (no updates or interpolation)
            else if (j==jsave+1) then
                ! update j and interpolate j+1
                do ii=1,mx
                    pj(ii) = pjp(ii)
                end do
                call cubt1(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp)
            else
                ! interpolate j,j+1 in pj,pjp on xx mesh
                call cubt1(nx,p(1,j),mx,pj,ix,dxm,dx,dxp,dxpp)
                call cubt1(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp)
            end if

            ! save j pointer for next pass

            jsave = j

            ! linearly interpolate q(ii,jj) from pjp,pj in y direction

            do ii=1,mx
                q(ii,jj) = pj(ii)+dy(jj)*(pjp(ii)-pj(ii))
            end do
        end do

    end if

    end subroutine lint2
!**************************************************************************

!**************************************************************************
!>
    subroutine cubt2(nx,ny,p,mx,my,q,intpol,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)

    implicit none

    integer  :: nx,ny,mx,my,intpol(2),jy(my),ix(mx)
    integer  :: jsave,j,jj,ii
    real(wp) :: p(nx,ny),q(mx,my)
    real(wp) :: pjm(mx),pj(mx),pjp(mx),pjpp(mx)
    real(wp) :: dym(my),dy(my),dyp(my),dypp(my)
    real(wp) :: dxm(mx),dx(mx),dxp(mx),dxpp(mx)

    if (intpol(1)==1) then

        ! linear in x

        jsave = -3
        do jj=1,my

            ! load closest four j lines containing interpolate on xx mesh
            ! for j-1,j,j+1,j+2 in pjm,pj,pjp,pjpp

            j = jy(jj)
            if (j==jsave) then
                ! j pointer has not moved since last pass (no updates or interpolation)
            else if (j==jsave+1) then
                ! update j-1,j,j+1 and interpolate j+2
                do ii=1,mx
                    pjm(ii) = pj(ii)
                    pj(ii) = pjp(ii)
                    pjp(ii) = pjpp(ii)
                end do
                call lint1(nx,p(1,j+2),mx,pjpp,ix,dx)
            else if (j==jsave+2) then
                ! update j-1,j and interpolate j+1,j+2
                do ii=1,mx
                    pjm(ii) = pjp(ii)
                    pj(ii) = pjpp(ii)
                end do
                call lint1(nx,p(1,j+1),mx,pjp,ix,dx)
                call lint1(nx,p(1,j+2),mx,pjpp,ix,dx)
            else if (j==jsave+3) then
                ! update j-1 and interpolate j,j+1,j+2
                do ii=1,mx
                    pjm(ii) = pjpp(ii)
                end do
                call lint1(nx,p(1,j),mx,pj,ix,dx)
                call lint1(nx,p(1,j+1),mx,pjp,ix,dx)
                call lint1(nx,p(1,j+2),mx,pjpp,ix,dx)
            else
                ! interpolate all four j-1,j,j+1,j+2
                call lint1(nx,p(1,j-1),mx,pjm,ix,dx)
                call lint1(nx,p(1,j),mx,pj,ix,dx)
                call lint1(nx,p(1,j+1),mx,pjp,ix,dx)
                call lint1(nx,p(1,j+2),mx,pjpp,ix,dx)
            end if

            ! save j pointer for next pass

            jsave = j

            ! cubically interpolate q(ii,jj) from pjm,pj,pjp,pjpp in y direction

            do ii=1,mx
                q(ii,jj) = dym(jj)*pjm(ii)+dy(jj)*pj(ii)+dyp(jj)*pjp(ii)+dypp(jj)*pjpp(ii)
            end do
        end do
        return

    else

        ! cubic in x

        jsave = -3
        do jj=1,my

            ! load closest four j lines containing interpolate on xx mesh
            ! for j-1,j,j+1,j+2 in pjm,pj,pjp,pjpp

            j = jy(jj)
            if (j==jsave) then
                ! j pointer has not moved since last pass (no updates or interpolation)
            else if (j==jsave+1) then
                ! update j-1,j,j+1 and interpolate j+2
                do ii=1,mx
                    pjm(ii) = pj(ii)
                    pj(ii) = pjp(ii)
                    pjp(ii) = pjpp(ii)
                end do
                call cubt1(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp)
            else if (j==jsave+2) then
                ! update j-1,j and interpolate j+1,j+2
                do ii=1,mx
                    pjm(ii) = pjp(ii)
                    pj(ii) = pjpp(ii)
                end do
                call cubt1(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp)
                call cubt1(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp)
            else if (j==jsave+3) then
                ! update j-1 and interpolate j,j+1,j+2
                do ii=1,mx
                    pjm(ii) = pjpp(ii)
                end do
                call cubt1(nx,p(1,j),mx,pj,ix,dxm,dx,dxp,dxpp)
                call cubt1(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp)
                call cubt1(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp)
            else
                ! interpolate all four j-1,j,j+1,j+2
                call cubt1(nx,p(1,j-1),mx,pjm,ix,dxm,dx,dxp,dxpp)
                call cubt1(nx,p(1,j),mx,pj,ix,dxm,dx,dxp,dxpp)
                call cubt1(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp)
                call cubt1(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp)
            end if

            ! save j pointer for next pass

            jsave = j

            ! cubically interpolate q(ii,jj) from pjm,pj,pjp,pjpp in y direction

            do ii=1,mx
                q(ii,jj) = dym(jj)*pjm(ii)+dy(jj)*pj(ii)+dyp(jj)*pjp(ii)+dypp(jj)*pjpp(ii)
            end do
        end do

    end if

    end subroutine cubt2
!**************************************************************************

!**************************************************************************
!>
!  subroutine rgrd2u interpolates the nx by ny array p onto
!  the mx by my array q.  linear or cubic interpolation is
!  used in each direction (see argument intpol).  it is assumed
!  that p and q are values on uniform nx by ny and mx by my grids
!  superimposed on the same rectangle (INCLUDING BOUNDARIES).
!  if p and q are values on nonuniform orthogonal grids and/or
!  if the grid on which q is defined lies within the p grid
!  then subroutine rgrd2 should be used.
!
!### method
!
!  linear or cubic interpolation (see intpol) is used in each
!  direction for which the q grid is not a subgrid of the p grid.
!  [the mx (my) uniform grid is a subgrid of the nx (ny) uniform
!  grid if and only if mx-1 (my-1) divides nx-1 (ny-1)].
!  values are set directly without (the need for) interpolation
!  in subgrid directions.

    subroutine rgrd2u(nx,ny,p,mx,my,q,intpol,w,lw,iw,liw,ier)

    implicit none

    integer,intent(in)  :: nx   !! the integer first dimension of p.  nx > 1 if intpol(1) = 1 or
                                !! nx > 3 if intpol(1) = 3 is required (see ier = 2).
    integer,intent(in)  :: ny   !! the integer second dimension of p.  ny > 1 if intpol(2) = 1 or
                                !! ny > 3 if intpol(2) = 3 is required (see ier = 2).
    integer,intent(in)  :: mx   !! the integer first dimension of q.  mx > 1 is required (see ier = 1)
    integer,intent(in)  :: my   !! the integer second dimension of q. my > 1 is required (see ier = 1)
    integer,intent(in)  :: intpol(2)    !! an integer vector of dimension 2 which sets linear or cubic
                                        !! interpolation in each of the x,y directions as follows:
                                        !!
                                        !! * intpol(1) = 1 sets linear interpolation in the x direction
                                        !! * intpol(1) = 3 sets cubic interpolation in the x direction.
                                        !! * intpol(2) = 1 sets linear interpolation in the y direction
                                        !! * intpol(2) = 3 sets cubic interpolation in the y direction.
                                        !!
                                        !! values other than 1 or 3 in intpol are not allowed (ier = 3).
    integer,intent(in)  :: lw   !! the integer length of the work space w.
                                !!
                                !! * let lwx = 1 if mx-1 divides nx-1; otherwise
                                !!   let lwx = mx if intpol(1) = 1 or
                                !!   let lwx = 4*mx if intpol(1) = 3
                                !! * let lwy = 0 if my-1 divides ny-1; otherwise
                                !!   let lwy = 2*mx+my if intpol(2) = 1 or
                                !!   let lwy = 4*(mx+my)  if intpol(2) = 3
                                !!
                                !! then lw must be greater than or equal to lwx+lwy
    integer,intent(in)  :: liw  !! the integer length of the integer work space iw.
                                !! liw must be greater than or equal to mx+my.
    integer,intent(out)  :: ier !! an integer error flag set as follows:
                                !!
                                !! * ier = 0 if no errors in input arguments are detected
                                !! * ier = 1 if  min(mx,my) < 2
                                !! * ier = 2 if nx < 2 when intpol(1)=1 or nx < 4 when intpol(1)=3 (or)
                                !!   ny < 2 when intpol(2)=1 or ny < 4 when intpol(2)=3.
                                !! * ier = 3 if intpol(1) or intpol(2) is not equal to 1 or 3
                                !! * ier = 4 if lw or liw is to small (insufficient work space)
    real(wp),intent(in) :: p(nx,ny)   !! a real(wp) nx by ny array of given values
    real(wp),intent(out) :: q(mx,my)  !! a real(wp) mx by my array of values which are interpolated from p.
    real(wp),intent(inout) :: w(lw)     !! a real(wp) work space of length at
                                        !! least lw which must be provided in the
                                        !! routine calling rgrd2u
    integer,intent(inout)  :: iw(liw)   !! an integer work space of length at least
                                        !! liw which must be provided in the
                                        !! routine calling rgrd2u

    integer  :: inmx,jnmy,isubx,jsuby,lwx,lwy,jy
    integer  :: j2,j3,j4,j5,j6,j7,j8,j9,i2,i3,i4,i5

    ! check input aarguments

    ! check mx,my
    ier = 1
    if (min(mx,my) < 2) return

    ! check intpol
    ier = 3
    if (intpol(1)/=1 .and. intpol(1)/=3) return
    if (intpol(2)/=1 .and. intpol(2)/=3) return

    ! check nx,ny
    ier = 2
    if (intpol(1)==1 .and. nx<2) return
    if (intpol(1)==3 .and. nx<4) return
    if (intpol(2)==1 .and. ny<2) return
    if (intpol(2)==3 .and. ny<4) return

    ! set subgrid indicators
    inmx = (nx-1)/(mx-1)
    jnmy = (ny-1)/(my-1)
    isubx = nx - inmx*(mx-1)
    jsuby = ny - jnmy*(my-1)

    ! check work space length input
    ier = 4
    lwx = 1
    lwy = 0
    if (isubx/=1) then
        if (intpol(1)==1) then
            lwx = mx
        else
            lwx = mx
        end if
    end if
    if (jsuby/=1) then
        if (intpol(2)==1) then
            lwy = (my+2*mx)
        else
            lwy = 4*(mx+my)
        end if
    end if
    if (lw < lwx+lwy) return
    if (liw < mx+my) return

    ! input arguments o.k.

    ier = 0
    jy = mx+1

    ! preset work space pointers

    j2 = 1
    j3 = j2
    j4 = j3
    j5 = j4
    j6 = j5
    j7 = j6
    j8 = j7
    j9 = j8
    i2 = j9
    i3 = i2
    i4 = i3
    i5 = i4

    if (intpol(2) ==1) then

        ! linearly interpolate in y

        if (jsuby/=1) then
            j2 = 1
            j3 = j2
            j4 = j3+my
            j5 = j4
            j6 = j5
            j7 = j6
            j8 = j7+mx
            j9 = j8+mx
            ! set y interpolation indices and scales and linearly interpolate
            call linmxu(ny,my,iw(jy),w(j3))
            i2 = j9
        end if

        ! set work space portion and indices which depend on x interpolation

        if (isubx/=1) then
            if (intpol(1) == 1) then
                i3 = i2
                i4 = i3
                i5 = i4
                call linmxu(nx,mx,iw,w(i3))
            else
                i3 = i2+mx
                i4 = i3+mx
                i5 = i4+mx
                call cubnmxu(nx,mx,iw,w(i2),w(i3),w(i4),w(i5))
            end if
        end if
        call lint2u(nx,ny,p,mx,my,q,intpol,iw(jy),w(j3),w(j7),w(j8),iw,&
                    w(i2),w(i3),w(i4),w(i5),inmx,jnmy,isubx,jsuby)
        return

    else

        ! cubically interpolate in y, set indice pointers

        if (jsuby/=1) then
            j2 = 1
            j3 = j2+my
            j4 = j3+my
            j5 = j4+my
            j6 = j5+my
            j7 = j6+mx
            j8 = j7+mx
            j9 = j8+mx
            ! set y interpolation indices and scales and cubically interpolate in y
            call cubnmxu(ny,my,iw(jy),w(j2),w(j3),w(j4),w(j5))
            i2 =  j9+mx
        end if

        ! set work space portion and indices which depend on x interpolation

        if (isubx/=1) then
            if (intpol(1) == 1) then
                i3 = i2
                i4 = i3
                i5 = i4
                call linmxu(nx,mx,iw,w(i3))
            else
                i3 = i2+mx
                i4 = i3+mx
                i5 = i4+mx
                call cubnmxu(nx,mx,iw,w(i2),w(i3),w(i4),w(i5))
            end if
        end if
        call cubt2u(nx,ny,p,mx,my,q,intpol,iw(jy),w(j2),w(j3),w(j4),&
                    w(j5),w(j6),w(j7),w(j8),w(j9),iw,w(i2),w(i3),w(i4),w(i5),&
                    inmx,jnmy,isubx,jsuby)
        return
    end if

    end subroutine
!**************************************************************************

!**************************************************************************
!>
!  linearly interpolate p onto q in y

    subroutine lint2u(nx,ny,p,mx,my,q,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)

    implicit none

    integer :: nx,ny,mx,my,intpol(2),jy(my),ix(mx),inmx,jnmy,isubx,jsuby
    integer :: j,jj,ii,jsave
    real(wp) :: p(nx,ny),q(mx,my)
    real(wp) :: dy(my),pj(mx),pjp(mx)
    real(wp) :: dxm(mx),dx(mx),dxp(mx),dxpp(mx)

    if (intpol(1) == 1) then

        ! linear in x

        if (jsuby == 1) then
            ! my grid is subset of ny grid
            do jj=1,my
                j = jnmy*(jj-1)+1
                call lint1u(nx,p(1,j),mx,q(1,jj),ix,dx,inmx,isubx)
            end do
            return
        end if

        jsave = -1
        do jj=1,my
            j = jy(jj)
            if (j == jsave) then
                ! pointer has not moved, no interpolation in pj,pjp necessary
            else if (j == jsave+1) then
                do ii=1,mx
                    pj(ii) = pjp(ii)
                end do
                call lint1u(nx,p(1,j+1),mx,pjp,ix,dx,inmx,isubx)
            else
                call lint1u(nx,p(1,j),mx,pj,ix,dx,inmx,isubx)
                call lint1u(nx,p(1,j+1),mx,pjp,ix,dx,inmx,isubx)
            end if
            ! update pointer
            jsave = j
            do ii=1,mx
                q(ii,jj) = pj(ii)+dy(jj)*(pjp(ii)-pj(ii))
            end do
        end do

    else

        ! cubic in x

        if (jsuby == 1) then
            ! my grid is subset of ny grid
            do jj=1,my
                j = jnmy*(jj-1)+1
                call cubt1u(nx,p(1,j),mx,q(1,jj),ix,dxm,dx,dxp,dxpp,inmx,isubx)
            end do
            return
        end if

        jsave = -1
        do jj=1,my
            j = jy(jj)
            if (j == jsave) then
                ! no interpolation in pj,pjp necessary
            else if (j == jsave+1) then
                do ii=1,mx
                    pj(ii) = pjp(ii)
                end do
                call cubt1u(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp,inmx,isubx)
            else
                call cubt1u(nx,p(1,j),mx,pj,ix,dxm,dx,dxp,dxpp,inmx,isubx)
                call cubt1u(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp,inmx,isubx)
            end if
            ! update pointer
            jsave = j
            do ii=1,mx
                q(ii,jj) = pj(ii)+dy(jj)*(pjp(ii)-pj(ii))
            end do
        end do

    end if

    end subroutine lint2u
!**************************************************************************

!**************************************************************************
!>
!  cubically interpolate p onto q in y

    subroutine cubt2u(nx,ny,p,mx,my,q,intpol,jy,dym,dy,dyp,dypp,&
                      pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,&
                      isubx,jsuby)

    implicit none

    integer  :: nx,ny,mx,my,intpol(2),jy(my),ix(mx),inmx,jnmy,isubx,jsuby
    integer  :: j,jj,ii,jsave
    real(wp) :: p(nx,ny),q(mx,my)
    real(wp) :: dym(my),dy(my),dyp(my),dypp(my)
    real(wp) :: pjm(mx),pj(mx),pjp(mx),pjpp(mx)
    real(wp) :: dxm(mx),dx(mx),dxp(mx),dxpp(mx)

    if (intpol(1) == 1) then

        ! linear in x

        if (jsuby == 1) then
            ! my grid is subset of ny grid
            do jj=1,my
                j = jnmy*(jj-1)+1
                call lint1u(nx,p(1,j),mx,q(1,jj),ix,dx,inmx,isubx)
            end do
            return
        end if

        jsave = -3
        do jj=1,my
            j = jy(jj)

            ! load pjm,pj,pjp,pjpp

            if (j==jsave) then
                ! no updates or x interpolation necessary
            else if (j==jsave+1) then
                do ii=1,mx
                    pjm(ii) = pj(ii)
                    pj(ii) = pjp(ii)
                    pjp(ii) = pjpp(ii)
                end do
                call lint1u(nx,p(1,j+2),mx,pjpp,ix,dx,inmx,isubx)
            else if (j==jsave+2) then
                do ii=1,mx
                    pjm(ii) = pjp(ii)
                    pj(ii) = pjpp(ii)
                end do
                call lint1u(nx,p(1,j+1),mx,pjp,ix,dx,inmx,isubx)
                call lint1u(nx,p(1,j+2),mx,pjpp,ix,dx,inmx,isubx)
            else if (j==jsave+3) then
                do ii=1,mx
                    pjm(ii) = pjpp(ii)
                end do
                call lint1u(nx,p(1,j),mx,pj,ix,dx,inmx,isubx)
                call lint1u(nx,p(1,j+1),mx,pjp,ix,dx,inmx,isubx)
                call lint1u(nx,p(1,j+2),mx,pjpp,ix,dx,inmx,isubx)
            else
                ! load all four (no updates)
                call lint1u(nx,p(1,j-1),mx,pjm,ix,dx,inmx,isubx)
                call lint1u(nx,p(1,j),mx,pj,ix,dx,inmx,isubx)
                call lint1u(nx,p(1,j+1),mx,pjp,ix,dx,inmx,isubx)
                call lint1u(nx,p(1,j+2),mx,pjpp,ix,dx,inmx,isubx)
            end if
            ! update pointer
            jsave = j
            do ii=1,mx
                q(ii,jj) = dym(jj)*pjm(ii) + dy(jj)*pj(ii) + dyp(jj)*pjp(ii) + dypp(jj)*pjpp(ii)
            end do
        end do

    else

        ! cubic in x

        if (jsuby == 1) then
            ! my grid is subset of ny grid
            do jj=1,my
                j = jnmy*(jj-1)+1
                call cubt1u(nx,p(1,j),mx,q(1,jj),ix,dxm,dx,dxp,dxpp,inmx,isubx)
            end do
            return
        end if

        jsave = -3
        do jj=1,my
            j = jy(jj)

            ! load pjm,pj,pjp,pjpp

            if (j==jsave) then
                ! no updates or x interpolation necessary
            else if (j==jsave+1) then
                do ii=1,mx
                    pjm(ii) = pj(ii)
                    pj(ii) = pjp(ii)
                    pjp(ii) = pjpp(ii)
                end do
                call cubt1u(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp,inmx,isubx)
            else if (j==jsave+2) then
                do ii=1,mx
                    pjm(ii) = pjp(ii)
                    pj(ii) = pjpp(ii)
                end do
                call cubt1u(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp,inmx,isubx)
                call cubt1u(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp,inmx,isubx)
            else if (j==jsave+3) then
                do ii=1,mx
                    pjm(ii) = pjpp(ii)
                end do
                call cubt1u(nx,p(1,j),mx,pj,ix,dxm,dx,dxp,dxpp,inmx,isubx)
                call cubt1u(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp,inmx,isubx)
                call cubt1u(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp,inmx,isubx)
            else
                ! load all four (no updates)
                call cubt1u(nx,p(1,j-1),mx,pjm,ix,dxm,dx,dxp,dxpp,inmx,isubx)
                call cubt1u(nx,p(1,j),mx,pj,ix,dxm,dx,dxp,dxpp,inmx,isubx)
                call cubt1u(nx,p(1,j+1),mx,pjp,ix,dxm,dx,dxp,dxpp,inmx,isubx)
                call cubt1u(nx,p(1,j+2),mx,pjpp,ix,dxm,dx,dxp,dxpp,inmx,isubx)
            end if
            ! update pointer
            jsave = j
            do ii=1,mx
                q(ii,jj) = dym(jj)*pjm(ii) + dy(jj)*pj(ii) + dyp(jj)*pjp(ii) + dypp(jj)*pjpp(ii)
            end do
        end do
        return

    end if

    end subroutine cubt2u
!**************************************************************************

!**************************************************************************
!>
!  subroutine rgrd3 interpolates the values p(i,j,k) on the orthogonal
!  grid (x(i),y(j),z(k)) for i=1,...,nx; j=1,...,ny; k=1,...,nz
!  onto q(ii,jj,kk) on the orthogonal grid (xx(ii),yy(jj),zz(kk)) for
!  ii=1,...,mx; jj=1,...,my; kk=1,...,mz.
!
!### method
!
!  linear or cubic interpolation is used (independently) in
!  each direction (see argument intpol).
!
!### requirements
!
!  each of the x,y,z grids must be strictly montonically increasing
!  and each of the xx,yy,zz grids must be montonically increasing
!  (see ier = 4).  in addition the (X,Y,Z) region
!
!    [xx(1),xx(mx)] X [yy(1),yy(my)] X [zz(1),zz(mz)]
!
!  must lie within the (X,Y,Z) region
!
!    [x(1),x(nx)] X [y(1),y(ny)] X [z(1),z(nz)].
!
!  extrapolation is not allowed (see ier=3).  if these (X,Y,Z)
!  regions are identical and the orthogonal grids are UNIFORM
!  in each direction then subroutine rgrd3u
!  should be used instead of rgrd3.

    subroutine rgrd3(nx,ny,nz,x,y,z,p,mx,my,mz,xx,yy,zz,q,intpol,w,lw,iw,liw,ier)

    implicit none

    integer,intent(in)      :: nx   !! the integer dimension of the grid vector x and the first dimension of p.
                                    !! nx > 1 if intpol(1) = 1 or nx > 3 if intpol(1) = 3 is required.
    integer,intent(in)      :: ny   !! the integer dimension of the grid vector y and the second dimension of p.
                                    !! ny > 1 if intpol(2) = 1 or ny > 3 if intpol(2) = 3 is required.
    integer,intent(in)      :: nz   !! the integer dimension of the grid vector z and the third dimension of p.
                                    !! nz > 1 if intpol(3) = 1 or nz > 3 if intpol(3) = 3 is required.
    integer,intent(in)      :: mx   !! the integer dimension of the grid vector xx and the first dimension of q.
                                    !! mx > 0 is required.
    integer,intent(in)      :: my   !! the integer dimension of the grid vector yy and the second dimension of q.
                                    !! my > 0 is required.
    integer,intent(in)      :: mz   !! the integer dimension of the grid vector zz and the third dimension of q.
                                    !! mz > 0 is required.
    integer,intent(in)      :: lw   !! the integer length of the real(wp) work space w.  let
                                    !!
                                    !! * lwx = mx              if intpol(1) = 1
                                    !! * lwx = 4*mx            if intpol(1) = 3
                                    !! * lwy = my+2*mx         if intpol(2) = 1
                                    !! * lwy = 4*(mx+my)       if intpol(2) = 3
                                    !! * lwz = 2*mx*my+mz      if intpol(3) = 1
                                    !! * lwz = 4*(mx*my+mz)    if intpol(3) = 3
                                    !!
                                    !! then lw must be greater than or equal to lwx+lwy+lwz
    integer,intent(in)      :: liw  !! the integer length of the integer work space iw.  liw must be at least mx+my+mz
    integer,intent(out)     :: ier  !! an integer error flag set as follows:
                                    !!
                                    !! * ier = 0 if no errors in input arguments are detected
                                    !! * ier = 1 if  min(mx,my,mz) < 1
                                    !! * ier = 2 if nx < 2 when intpol(1)=1 or nx < 4 when intpol(1)=3 (or)
                                    !!                     ny < 2 when intpol(2)=1 or ny < 4 when intpol(2)=3 (or)
                                    !!                     nz < 2 when intpol(3)=1 or nz < 4 when intpol(3)=3.
                                    !! * ier = 3 if xx(1) < x(1) or x(nx) < xx(mx) (or)
                                    !!                     yy(1) < y(1) or y(ny) < yy(my) (or)
                                    !!                     zz(1) < z(1) or z(nz) < zz(mz)
                                    !!    to avoid this flag when end points are intended to be the
                                    !!    same but may differ slightly due to roundoff error, they
                                    !!    should be set exactly in the calling routine (e.g., if both
                                    !!    grids have the same y boundaries then yy(1)=y(1) and yy(my)=y(ny)
                                    !!    should be set before calling rgrd3)
                                    !! * ier = 4 if one of the grids x,y,z is not strictly monotonically
                                    !!   increasing or if one of the grids xx,yy,zz is not
                                    !!   montonically increasing.  more precisely if:
                                    !!
                                    !!    * x(i+1) <= x(i) for some i such that 1 <= i < nx (or)
                                    !!    * y(j+1) <= y(j) for some j such that 1 <= j < ny (or)
                                    !!    * z(k+1) <= z(k) for some k such that 1 <= k < nz (or)
                                    !!    * xx(ii+1) < xx(ii) for some ii such that 1 <= ii < mx (or)
                                    !!    * yy(jj+1) < yy(jj) for some jj such that 1 <= jj < my (or)
                                    !!    * zz(kk+1) < zz(k)  for some kk such that 1 <= kk < mz
                                    !! * ier = 5 if lw or liw is too small (insufficient work space)
                                    !! * ier = 6 if any of intpol(1),intpol(2),intpol(3) is not equal to 1 or 3
    real(wp),intent(in)     :: x(nx)  !! a real(wp) nx vector of strictly increasing values which defines the x
                                      !! portion of the orthogonal grid on which p is given
    real(wp),intent(in)     :: y(ny)  !! a real(wp) ny vector of strictly increasing values which defines the y
                                      !! portion of the orthogonal grid on which p is given
    real(wp),intent(in)     :: z(nz)  !! a real(wp) nz vector of strictly increasing values which defines the z
                                      !! portion of the orthogonal grid on which p is given
    real(wp),intent(in)     :: p(nx,ny,nz)  !! a real(wp) nx by ny by nz array of values given on the (x,y,z) grid
    real(wp),intent(in)     :: xx(mx)   !! a real(wp) mx vector of increasing values which defines the x portion of the
                                        !! orthogonal grid on which q is defined.  xx(1) < x(1) or xx(mx) > x(nx)
                                        !! is not allowed (see ier = 3)
    real(wp),intent(in)     :: yy(my)   !! a real(wp) my vector of increasing values which defines the y portion of the
                                        !! orthogonal grid on which q is defined.  yy(1) < y(1) or yy(my) > y(ny)
                                        !! is not allowed (see ier = 3)
    real(wp),intent(in)     :: zz(mz)   !! a real(wp) mz vector of increasing values which defines the z portion of the
                                        !! orthogonal grid on which q is defined.  zz(1) < z(1) or zz(mz) > z(nz)
                                        !! is not allowed (see ier = 3)
    real(wp),intent(out)    :: q(mx,my,mz)  !! a real(wp) mx by my by mz array of values
                                            !! on the (xx,yy,zz) grid which are
                                            !! interpolated from p on the (x,y,z) grid
    real(wp),intent(inout)  :: w(lw)    !! a real(wp) work space of length at least
                                        !! lw which must be provided in the
                                        !! routine calling rgrd3
    integer,intent(in)      :: intpol(3)    !! an integer vector of dimension 3 which sets linear or cubic
                                            !! interpolation in each of the x,y,z directions as follows:
                                            !!
                                            !! * intpol(1) = 1 sets linear interpolation in the x direction
                                            !! * intpol(1) = 3 sets cubic interpolation in the x direction.
                                            !! * intpol(2) = 1 sets linear interpolation in the y direction
                                            !! * intpol(2) = 3 sets cubic interpolation in the y direction.
                                            !! * intpol(3) = 1 sets linear interpolation in the z direction
                                            !! * intpol(3) = 3 sets cubic interpolation in the z direction.
                                            !!
                                            !! values other than 1 or 3 in intpol are not allowed (ier = 5).
    integer,intent(inout)   :: iw(liw)  !! an integer work space of length at least
                                        !! liw which must be provided in the
                                        !! routine calling rgrd3

    integer  :: i,ii,j,jj,k,kk
    integer  :: i2,i3,i4,i5
    integer  :: j2,j3,j4,j5,j6,j7,j8,j9
    integer  :: k2,k3,k4,k5,k6,k7,k8,k9
    integer  :: lwx,lwy,lwz,jy,kz,mxmy

    ! check input arguments

    ! check (xx,yy,zz) grid resolution
    ier = 1
    if (min(mx,my,mz) < 1) return

    ! check intpol
    ier = 6
    if (intpol(1)/=1 .and. intpol(1)/=3) return
    if (intpol(2)/=1 .and. intpol(2)/=3) return
    if (intpol(3)/=1 .and. intpol(3)/=3) return

    ! check (x,y,z) grid resolution
    ier = 2
    if (intpol(1)==1 .and. nx<2) return
    if (intpol(1)==3 .and. nx<4) return
    if (intpol(2)==1 .and. ny<2) return
    if (intpol(2)==3 .and. ny<4) return
    if (intpol(3)==1 .and. nz<2) return
    if (intpol(3)==3 .and. nz<4) return

    ! check work space length input and set minimum
    ier = 5
    mxmy = mx*my
    if (intpol(1)==1) then
        lwx = mx
    else
        lwx = 4*mx
    end if
    if (intpol(2)==1) then
        lwy = (my+2*mx)
    else
        lwy = 4*(my+mx)
    end if
    if (intpol(3)==1) then
        lwz = (2*mxmy+mz)
    else
        lwz = 4*(mxmy+mz)
    end if
    if (lw < lwx+lwy+lwz) return
    if (liw < mx+my+mz) return

    ! check (xx,yy,zz) grid contained in (x,y,z) grid
    ier = 3
    if (xx(1)<x(1) .or. xx(mx)>x(nx)) return
    if (yy(1)<y(1) .or. yy(my)>y(ny)) return
    if (zz(1)<z(1) .or. zz(mz)>z(nz)) return

    ! check montonicity of grids
    ier = 4
    do i=2,nx
        if (x(i-1)>=x(i)) return
    end do
    do j=2,ny
        if (y(j-1)>=y(j)) return
    end do
    do k=2,nz
        if (z(k-1)>=z(k)) return
    end do
    do ii=2,mx
        if (xx(ii-1)>xx(ii)) return
    end do
    do jj=2,my
        if (yy(jj-1)>yy(jj)) return
    end do
    do kk=2,mz
        if (zz(kk-1)>zz(kk)) return
    end do

    ! arguments o.k.

    ier = 0
    jy = mx+1
    kz = mx+my+1
    if (intpol(3)==1) then

        ! linearly interpolate in nz, set work space pointers and scales

        k2 = 1
        k3 = k2
        k4 = k3+mz
        k5 = k4
        k6 = k5
        k7 = k6
        k8 = k7+mxmy
        k9 = k8+mxmy
        call linmx(nz,z,mz,zz,iw(kz),w(k3))
        j2 = k9

        ! set indices and scales which depend on y interpolation

        if (intpol(2) == 1) then
            ! linear in y
            j3 = j2
            j4 = j3+my
            j5 = j4
            j6 = j5
            j7 = j6
            j8 = j7+mx
            j9 = j8+mx
            call linmx(ny,y,my,yy,iw(jy),w(j3))
            i2 = j9
        else
            ! cubic in y
            j3 = j2+my
            j4 = j3+my
            j5 = j4+my
            j6 = j5+my
            j7 = j6+mx
            j8 = j7+mx
            j9 = j8+mx
            call cubnmx(ny,y,my,yy,iw(jy),w(j2),w(j3),w(j4),w(j5))
            i2 = j9+mx
        end if

        ! set indices and scales which depend on x interpolation

        if (intpol(1) == 1) then
            ! linear in x
            i3 = i2
            i4 = i3
            i5 = i4
            call linmx(nx,x,mx,xx,iw,w(i3))
        else
            ! cubic in x
            i3 = i2+mx
            i4 = i3+mx
            i5 = i4+mx
            call cubnmx(nx,x,mx,xx,iw,w(i2),w(i3),w(i4),w(i5))
        end if
        call lint3(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,iw(kz),&
                    w(k3),w(k7),w(k8),iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),&
                    w(j7),w(j8),w(j9),iw,w(i2),w(i3),w(i4),w(i5))

    else

        ! cubically interpolate in z

        k2 = 1
        k3 = k2+mz
        k4 = k3+mz
        k5 = k4+mz
        k6 = k5+mz
        k7 = k6+mxmy
        k8 = k7+mxmy
        k9 = k8+mxmy
        call cubnmx(nz,z,mz,zz,iw(kz),w(k2),w(k3),w(k4),w(k5))
        j2 = k9+mxmy

        ! set indices which depend on y interpolation

        if (intpol(2) == 1) then
            j3 = j2
            j4 = j3+my
            j5 = j4
            j6 = j5
            j7 = j6
            j8 = j7+mx
            j9 = j8+mx
            call linmx(ny,y,my,yy,iw(jy),w(j3))
            i2 = j9
        else
            j3 = j2+my
            j4 = j3+my
            j5 = j4+my
            j6 = j5+my
            j7 = j6+mx
            j8 = j7+mx
            j9 = j8+mx
            call cubnmx(ny,y,my,yy,iw(jy),w(j2),w(j3),w(j4),w(j5))
            i2 = j9+mx
        end if

        ! set work space portion and indices which depend on x interpolation

        if (intpol(1) == 1) then
            i3 = i2
            i4 = i3
            i5 = i4
            call linmx(nx,x,mx,xx,iw,w(i3))
        else
            i3 = i2+mx
            i4 = i3+mx
            i5 = i4+mx
            call cubnmx(nx,x,mx,xx,iw,w(i2),w(i3),w(i4),w(i5))
        end if
        call cubt3(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,&
                    iw(kz),w(k2),w(k3),w(k4),w(k5),w(k6),w(k7),w(k8),w(k9),&
                    iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),w(j7),w(j8),w(j9),&
                    iw,w(i2),w(i3),w(i4),w(i5))

    end if

    end subroutine rgrd3
!**************************************************************************

!**************************************************************************
!>
!  linearly interpolate in z direction

    subroutine lint3(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,kz,&
                     dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,&
                     pjpp,ix,dxm,dx,dxp,dxpp)

    implicit none

    integer  :: nx,ny,nz,mx,my,mz,mxmy
    real(wp) :: p(nx,ny,nz),q(mxmy,mz)
    real(wp) :: dz(mz),pk(mxmy),pkp(mxmy)
    real(wp) :: dym(my),dy(my),dyp(my),dypp(my)
    real(wp) :: pjm(mx),pj(mx),pjp(mx),pjpp(mx)
    real(wp) :: dxm(mx),dx(mx),dxp(mx),dxpp(mx)
    integer  :: intpol(3),kz(mz),jy(my),ix(mx)
    integer  :: k,kk,iijj,ksave

    if (intpol(2) == 1) then

        ! linear in y

        ksave = -1
        do kk=1,mz
            k = kz(kk)
            if (k==ksave) then
                ! k pointer has not moved since last pass (no updates or interpolation)
            else if (k==ksave+1) then
                ! update k and interpolate k+1
                do iijj=1,mxmy
                    pk(iijj) = pkp(iijj)
                end do
                call lint2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
            else
                ! interpolate k,k+1 in pk,pkp on xx,yy mesh
                call lint2(nx,ny,p(1,1,k),mx,my,pk,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
                call lint2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
            end if

            ! save k pointer for next pass

            ksave = k

            ! linearly interpolate q(ii,jj,k) from pk,pkp in z direction

            do iijj=1,mxmy
                q(iijj,kk) = pk(iijj)+dz(kk)*(pkp(iijj)-pk(iijj))
            end do
        end do

    else

        ! cubic in y

        ksave = -1
        do kk=1,mz
            k = kz(kk)
            if (k==ksave) then
                ! k pointer has not moved since last pass (no updates or interpolation)
            else if (k==ksave+1) then
                ! update k and interpolate k+1
                do iijj=1,mxmy
                    pk(iijj) = pkp(iijj)
                end do
                call cubt2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
            else
                ! interpolate k,k+1 in pk,pkp on xx,yy mesh
                call cubt2(nx,ny,p(1,1,k),mx,my,pk,intpol,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
                call cubt2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
            end if

            ! save k pointer for next pass

            ksave = k

            ! linearly interpolate q(ii,jj,k) from pk,pkp in z direction

            do iijj=1,mxmy
                q(iijj,kk) = pk(iijj)+dz(kk)*(pkp(iijj)-pk(iijj))
            end do
        end do

    end if

    end subroutine lint3
!**************************************************************************

!**************************************************************************
!>
!  cubically interpolate in z

    subroutine cubt3(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,&
                     kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,&
                     jy,dym,dy,dyp,dypp,pjm,pj,&
                     pjp,pjpp,ix,dxm,dx,dxp,dxpp)

    implicit none

    integer  :: nx,ny,nz,mx,my,mxmy,mz,k,kk,ksave,iijj
    real(wp) :: p(nx,ny,nz),q(mxmy,mz)
    real(wp) :: pkm(mxmy),pk(mxmy),pkp(mxmy),pkpp(mxmy)
    real(wp) :: pjm(mx),pj(mx),pjp(mx),pjpp(mx)
    real(wp) :: dzm(mz),dz(mz),dzp(mz),dzpp(mz)
    real(wp) :: dym(my),dy(my),dyp(my),dypp(my)
    real(wp) :: dxm(mx),dx(mx),dxp(mx),dxpp(mx)
    integer  :: intpol(3),kz(mz),jy(my),ix(mx)

    if (intpol(2) == 1) then

        ! linear in y

        ksave = -3
        do kk=1,mz
            k = kz(kk)
            if (k==ksave) then
                ! k pointer has not moved since last pass (no updates or interpolation)
            else if (k==ksave+1) then
                ! update k-1,k,k+1 and interpolate k+2
                do iijj=1,mxmy
                    pkm(iijj) = pk(iijj)
                    pk(iijj) = pkp(iijj)
                    pkp(iijj) = pkpp(iijj)
                end do
                call lint2(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
            else if (k==ksave+2) then
                ! update k-1,k and interpolate k+1,k+2
                do iijj=1,mxmy
                    pkm(iijj) = pkp(iijj)
                    pk(iijj) = pkpp(iijj)
                end do
                call lint2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
                call lint2(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
            else if (k==ksave+3) then
                ! update k-1 and interpolate k,k+1,k+2
                do iijj=1,mxmy
                    pkm(iijj) = pkpp(iijj)
                end do
                call lint2(nx,ny,p(1,1,k),mx,my,pk,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
                call lint2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
                call lint2(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
            else
                ! interpolate all four k-1,k,k+1,k+2
                call lint2(nx,ny,p(1,1,k-1),mx,my,pkm,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
                call lint2(nx,ny,p(1,1,k),mx,my,pk,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
                call lint2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
                call lint2(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp)
            end if

            ! save k pointer for next pass

            ksave = k

            ! cubically interpolate q(ii,jj,kk) from pkm,pk,pkp,pkpp in z direction

            do iijj=1,mxmy
                q(iijj,kk) = dzm(kk)*pkm(iijj) + dz(kk)*pk(iijj) + dzp(kk)*pkp(iijj) + dzpp(kk)*pkpp(iijj)
            end do
        end do

    else

        ! cubic in y

        ksave = -3
        do kk=1,mz
            k = kz(kk)
            if (k==ksave) then
                ! k pointer has not moved since last pass (no updates or interpolation)
            else if (k==ksave+1) then
                ! update k-1,k,k+1 and interpolate k+2
                do iijj=1,mxmy
                    pkm(iijj) = pk(iijj)
                    pk(iijj) = pkp(iijj)
                    pkp(iijj) = pkpp(iijj)
                end do
                call cubt2(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
            else if (k==ksave+2) then
                ! update k-1,k and interpolate k+1,k+2
                do iijj=1,mxmy
                    pkm(iijj) = pkp(iijj)
                    pk(iijj) = pkpp(iijj)
                end do
                call cubt2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
                call cubt2(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
            else if (k==ksave+3) then
                ! update k-1 and interpolate k,k+1,k+2
                do iijj=1,mxmy
                    pkm(iijj) = pkpp(iijj)
                end do
                call cubt2(nx,ny,p(1,1,k),mx,my,pk,intpol,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
                call cubt2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
                call cubt2(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
            else
                ! interpolate all four k-1,k,k+1,k+2
                call cubt2(nx,ny,p(1,1,k-1),mx,my,pkm,intpol,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
                call cubt2(nx,ny,p(1,1,k),mx,my,pk,intpol,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
                call cubt2(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
                call cubt2(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
            end if

            ! save k pointer for next pass

            ksave = k

            ! cubically interpolate q(ii,jj,kk) from pkm,pk,pkp,pkpp in z direction

            do iijj=1,mxmy
                q(iijj,kk) = dzm(kk)*pkm(iijj) + dz(kk)*pk(iijj) + dzp(kk)*pkp(iijj) + dzpp(kk)*pkpp(iijj)
            end do
        end do

    end if

    end subroutine cubt3
!**************************************************************************

!**************************************************************************
!>
!  subroutine rgrd3u interpolates the nx by ny by nz array p onto
!  the mx by my by mz array q.  it is assumed that p and q are
!  values on uniform nx by ny by nz and mx by my by mz grids which
!  are superimposed on the same box region (INCLUDING BOUNDARIES).
!  if p and q are values on nonuniform orthogonal grids and/or
!  if the grid on which q is defined lies within the p grid then
!  subroutine rgrd3 should be used.
!
!### method
!
!  linear or cubic interpolation (see intpol) is used in each
!  direction for which the q grid is not a subgrid of the p grid.
!  [the mx (my,mz) uniform grid is a subgrid of the nx (ny,nz) uniform
!  grid if and only if mx-1 (my-1,nz-1) divides nx-1 (ny-1,nz-1)].
!  Values are set directly without (the need for) interpolation
!  in subgrid directions.

    subroutine rgrd3u(nx,ny,nz,p,mx,my,mz,q,intpol,w,lw,iw,liw,ier)

    implicit none

    integer,intent(in)     :: nx    !! the integer first dimension of p.  nx > 1 if intpol(1) = 1 or
                                    !! nx > 3 if intpol(1) = 3 is required (see ier = 2).
    integer,intent(in)     :: ny    !! the integer second dimension of p.  ny > 1 if intpol(2) = 1 or
                                    !! ny > 3 if intpol(2) = 3 is required (see ier = 2).
    integer,intent(in)     :: nz    !! the integer third dimension of p.  nz > 1 if intpol(3) = 1 or
                                    !! nz > 3 if intpol(3) = 3 is required (see ier = 2)
    integer,intent(in)     :: mx    !! the integer first dimension of q.  mx > 1 is required (see ier = 1)
    integer,intent(in)     :: my    !! the integer second dimension of q. my > 1 is required (see ier = 1)
    integer,intent(in)     :: mz    !! the integer third dimension of q. mz > 1 is required (see ier = 1)
    integer,intent(in)     :: intpol(3) !! an integer vector of dimension 3 which sets linear or cubic
                                        !! interpolation in each of the x,y,z directions as follows:
                                        !!
                                        !! * intpol(1) = 1 sets linear interpolation in the x direction
                                        !! * intpol(1) = 3 sets cubic interpolation in the x direction.
                                        !! * intpol(2) = 1 sets linear interpolation in the y direction
                                        !! * intpol(2) = 3 sets cubic interpolation in the y direction.
                                        !! * intpol(3) = 1 sets linear interpolation in the z direction
                                        !! * intpol(3) = 3 sets cubic interpolation in the z direction.
                                        !!
                                        !! values other than 1 or 3 in intpol are not allowed (ier = 3).
    integer,intent(in)     :: lw        !! the integer length of the real(wp) work space w.
                                        !!
                                        !! * let lwx = 1 if mx-1 divides nx-1; otherwise
                                        !! * let lwx = mx if intpol(1) = 1 or
                                        !! * let lwx = 4*mx if intpol(1) = 3
                                        !! * let lwy = 0 if my-1 divides ny-1; otherwise
                                        !! * let lwy = my+2*mx if intpol(2) = 1 or
                                        !! * let lwy = 4*(mx+my) if intpol(2) = 3
                                        !! * let lwz = 0 if mz-1 divides nz-1; otherwise
                                        !! * let lwz = 2*mx*my+mz if intpol(3) = 1 or
                                        !! * let lwz = 4*(mx*my+mz) if intpol(3) = 3
                                        !!
                                        !! then lw must be greater than or equal to lwx+lwy+lwz
    integer,intent(in)     :: liw       !! the integer length of the integer work space iw.
                                        !! liw must be greater than or equal to mx+my+mz
    integer,intent(inout)  :: iw(liw)   !! an integer work space of length at least liw
                                        !! which must be provided in the
                                        !! routine calling rgrd3u
    integer,intent(out)    :: ier       !! an integer error flag set as follows:
                                        !!
                                        !! * ier = 0 if no errors in input arguments are detected
                                        !! * ier = 1 if  min(mx,my,mz) < 2
                                        !! * ier = 2 if nx < 2 when intpol(1)=1 or nx < 4 when intpol(1)=3 (or)
                                        !!   ny < 2 when intpol(2)=1 or ny < 4 when intpol(2)=3 (or)
                                        !!   nz < 2 when intpol(3)=1 or nz < 4 when intpol(3)=3.
                                        !! * ier = 3 if any of intpol(1),intpol(2),intpol(3) is not equal to 1 or 3
                                        !! * ier = 4 if lw or liw is too small (insufficient work space)
    real(wp),intent(in)    :: p(nx,ny,nz)   !! a real(wp) nx by ny by nz array of given values
    real(wp),intent(out)   :: q(mx,my,mz)   !! a real(wp) mx by my by mz array of values which are interpolated from p.
    real(wp),intent(inout) :: w(lw) !! a real(wp) work space of length at least lw
                                    !! which must be provided in the
                                    !! routine calling rgrd3u

    integer  :: inmx,jnmy,knmz,isubx,jsuby,ksubz
    integer  :: lwx,lwy,lwz,mxmy,jy,kz
    integer  :: i2,i3,i4,i5
    integer  :: j2,j3,j4,j5,j6,j7,j8,j9
    integer  :: k2,k3,k4,k5,k6,k7,k8,k9

    ! check input arguments

    ! check mx,my,mz
    ier = 1
    if (min(mx,my,mz) < 1) return

    ! check intpol
    ier = 3
    if (intpol(1)/=1 .and. intpol(1)/=3) return
    if (intpol(2)/=1 .and. intpol(2)/=3) return
    if (intpol(3)/=1 .and. intpol(3)/=3) return

    ! check nx,ny,nz
    ier = 2
    if (intpol(1)==1 .and. nx<2) return
    if (intpol(1)==3 .and. nx<4) return
    if (intpol(2)==1 .and. ny<2) return
    if (intpol(2)==3 .and. ny<4) return
    if (intpol(3)==1 .and. nz<2) return
    if (intpol(3)==3 .and. nz<4) return

    ! set subgrid indicators
    inmx = (nx-1)/(mx-1)
    jnmy = (ny-1)/(my-1)
    knmz = (nz-1)/(mz-1)
    isubx = nx - inmx*(mx-1)
    jsuby = ny - jnmy*(my-1)
    ksubz = nz - knmz*(mz-1)

    ! check work space lengths
    ier = 4
    mxmy = mx*my
    lwx = 1
    if (isubx/=1) then
        if (intpol(1)==1) then
            lwx = mx
        else
            lwx = 4*mx
        end if
    end if
    lwy = 0
    if (jsuby/=1) then
        if (intpol(2)==1) then
            lwy = (2*mx+my)
        else
            lwy = 4*(mx+my)
        end if
    end if
    lwz = 0
    if (ksubz/=1) then
        if (intpol(3)==1) then
            lwz = (2*mxmy+mz)
        else
            lwz = 4*(mxmy+mz)
        end if
    end if
    if (lw < lwx+lwy+lwz) return
    if (liw < mx+my+mz) return

    ! arguments o.k.

    ier = 0
    jy = mx+1
    kz = mx+my+1

    ! preset work space pointers

    k2 = 1
    k3 = 1
    k4 = 1
    k5 = 1
    k6 = 1
    k7 = 1
    k8 = 1
    k9 = 1
    j2 = 1
    j3 = 1
    j4 = 1
    j5 = 1
    j6 = 1
    j7 = 1
    j8 = 1
    j9 = 1
    i2 = 1
    i3 = 1
    i4 = 1
    i5 = 1

    if (intpol(3)==1) then
        if (ksubz/=1) then
            ! linearly interpolate in nz, set work space pointers
            k2 = 1
            k3 = k2
            k4 = k3+mz
            k5 = k4
            k6 = k5
            k7 = k6
            k8 = k7+mxmy
            k9 = k8+mxmy
            ! set z interpolation indices and scales
            call linmxu(nz,mz,iw(kz),w(k3))
            j2 = k9
            i2 = k9
        end if

        if (jsuby/=1) then
            if (intpol(2) == 1) then
                ! linear in y
                j3 = j2
                j4 = j3+my
                j5 = j4
                j6 = j5
                j7 = j6
                j8 = j7+mx
                j9 = j8+mx
                call linmxu(ny,my,iw(jy),w(j3))
                i2 = j9
            else
                ! cubic in y
                j3 = j2+my
                j4 = j3+my
                j5 = j4+my
                j6 = j5+my
                j7 = j6+mx
                j8 = j7+mx
                j9 = j8+mx
                call cubnmxu(ny,my,iw(jy),w(j2),w(j3),w(j4),w(j5))
                i2 = j9+mx
            end if
        end if

        if (isubx/=1) then
            if (intpol(1) == 1) then
                ! linear in x
                i3 = i2
                i4 = i3
                i5 = i4
                call linmxu(nx,mx,iw,w(i3))
            else
                ! cubic in x
                i3 = i2+mx
                i4 = i3+mx
                i5 = i4+mx
                call cubnmxu(nx,mx,iw,w(i2),w(i3),w(i4),w(i5))
            end if
        end if

        ! linearly interpolate p onto q in z

        call lint3u(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,iw(kz),w(k3),&
                    w(k7),w(k8),iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),w(j7),&
                    w(j8),w(j9),iw,w(i2),w(i3),w(i4),w(i5),&
                    inmx,jnmy,knmz,isubx,jsuby,ksubz)

    else

        ! cubically interpolate in z

        if (ksubz/=1) then
            k2 = 1
            k3 = k2+mz
            k4 = k3+mz
            k5 = k4+mz
            k6 = k5+mz
            k7 = k6+mxmy
            k8 = k7+mxmy
            k9 = k8+mxmy
            call cubnmxu(nz,mz,iw(kz),w(k2),w(k3),w(k4),w(k5))
            j2 = k9+mxmy
            i2 = j2
        end if

        if (jsuby/=1) then
            if (intpol(2) == 1) then
                j3 = j2
                j4 = j3+my
                j5 = j4
                j6 = j5
                j7 = j6
                j8 = j7+mx
                j9 = j8+mx
                call linmxu(ny,my,iw(jy),w(j3))
                i2 = j9
            else
                j3 = j2+my
                j4 = j3+my
                j5 = j4+my
                j6 = j5+my
                j7 = j6+mx
                j8 = j7+mx
                j9 = j8+mx
                call cubnmxu(ny,my,iw(jy),w(j2),w(j3),w(j4),w(j5))
                i2 = j9+mx
            end if
        end if

        if (isubx/=1) then
            if (intpol(1) == 1) then
                i3 = i2
                i4 = i3
                i5 = i4
                call linmxu(nx,mx,iw,w(i3))
            else
                i3 = i2+mx
                i4 = i3+mx
                i5 = i4+mx
                call cubnmxu(nx,mx,iw,w(i2),w(i3),w(i4),w(i5))
            end if
        end if

        ! cubically interpolate p onto q in z

        call cubt3u(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,&
                    iw(kz),w(k2),w(k3),w(k4),w(k5),w(k6),w(k7),w(k8),w(k9),&
                    iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),w(j7),w(j8),w(j9),&
                    iw,w(i2),w(i3),w(i4),w(i5),&
                    inmx,jnmy,knmz,isubx,jsuby,ksubz)

    end if

    end subroutine rgrd3u
!**************************************************************************

!**************************************************************************
!>
!  linearly interpolate in z direction

    subroutine lint3u(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,kz,&
                      dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,&
                      pjpp,ix,dxm,dx,dxp,dxpp,&
                      inmx,jnmy,knmz,isubx,jsuby,ksubz)

    implicit none

    integer  :: nx,ny,nz,mx,my,mz,mxmy,intpol(3),kz(mz),jy(my),ix(mx)
    integer  :: inmx,jnmy,knmz,isubx,jsuby,ksubz
    integer  :: kk,k,iijj,ksave
    real(wp) :: p(nx,ny,nz),q(mxmy,mz)
    real(wp) :: dz(mz),pk(mxmy),pkp(mxmy)
    real(wp) :: dym(my),dy(my),dyp(my),dypp(my)
    real(wp) :: pjm(mx),pj(mx),pjp(mx),pjpp(mx)
    real(wp) :: dxm(mx),dx(mx),dxp(mx),dxpp(mx)

    if (intpol(2) == 1) then

        ! linear in y

        if (ksubz == 1) then
            ! mz grid is subset of nz grid
            do kk=1,mz
                k = knmz*(kk-1)+1
                call lint2u(nx,ny,p(1,1,k),mx,my,q(1,kk),intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
            end do
            return
        end if

        ksave = -1
        do kk=1,mz
            k = kz(kk)
            if (k==ksave) then
                ! k pointer has not moved since last pass (no updates or interpolation)
            else if (k==ksave+1) then
                ! update k and interpolate k+1
                do iijj=1,mxmy
                    pk(iijj) = pkp(iijj)
                end do
                call lint2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
            else
                ! interpolate k,k+1 in pk,pkp
                call lint2u(nx,ny,p(1,1,k),mx,my,pk,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
                call lint2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
            end if

            ! save k pointer for next pass

            ksave = k

            ! linearly interpolate q(ii,jj,k) from pk,pkp in z direction

            do iijj=1,mxmy
                q(iijj,kk) = pk(iijj)+dz(kk)*(pkp(iijj)-pk(iijj))
            end do
        end do

    else

        ! cubic in y

        if (ksubz == 1) then
            ! mz grid is subset of nz grid
            do kk=1,mz
                k = knmz*(kk-1)+1
                call cubt2u(nx,ny,p(1,1,k),mx,my,q(1,kk),intpol,&
                    jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                    inmx,jnmy,isubx,jsuby)
            end do
            return
        end if

        ksave = -1
        do kk=1,mz
            k = kz(kk)
            if (k==ksave) then
                ! k pointer has not moved since last pass (no updates or interpolation)
            else if (k==ksave+1) then
                ! update k and interpolate k+1
                do iijj=1,mxmy
                    pk(iijj) = pkp(iijj)
                end do
                call cubt2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,&
                    jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                    inmx,jnmy,isubx,jsuby)
            else
                ! interpolate k,k+1 in pk,pkp
                call cubt2u(nx,ny,p(1,1,k),mx,my,pk,intpol,&
                    jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                    inmx,jnmy,isubx,jsuby)
                call cubt2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,&
                    jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                    inmx,jnmy,isubx,jsuby)
            end if

            ! save k pointer for next pass

            ksave = k

            ! linearly interpolate q(ii,jj,k) from pk,pkp in z direction

            do iijj=1,mxmy
                q(iijj,kk) = pk(iijj)+dz(kk)*(pkp(iijj)-pk(iijj))
            end do
        end do

    end if

    end subroutine lint3u
!**************************************************************************

!**************************************************************************
!>
!  cubically interpolate in z

    subroutine cubt3u(nx,ny,nz,p,mx,my,mxmy,mz,q,intpol,&
                      kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,&
                      pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                      inmx,jnmy,knmz,isubx,jsuby,ksubz)

    implicit none

    integer  :: nx,ny,nz,mx,my,mz,mxmy,intpol(3),kz(mz),jy(my),ix(mx)
    integer  :: inmx,jnmy,knmz,isubx,jsuby,ksubz
    integer  :: kk,k,iijj,ksave
    real(wp) :: p(nx,ny,nz),q(mxmy,mz)
    real(wp) :: dzm(mz),dz(mz),dzp(mz),dzpp(mz)
    real(wp) :: pkm(mxmy),pk(mxmy),pkp(mxmy),pkpp(mxmy)
    real(wp) :: dym(my),dy(my),dyp(my),dypp(my)
    real(wp) :: pjm(mx),pj(mx),pjp(mx),pjpp(mx)
    real(wp) :: dxm(mx),dx(mx),dxp(mx),dxpp(mx)

    if (intpol(2) == 1) then

        ! linear in y
        if (ksubz == 1) then
            ! mz grid is subset of nz grid
            do kk=1,mz
                k = knmz*(kk-1)+1
                call lint2u(nx,ny,p(1,1,k),mx,my,q(1,kk),intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
            end do
            return
        end if

        ! mz not a subgrid of nz

        ksave = -3
        do kk=1,mz
            k = kz(kk)
            if (k==ksave) then
                ! k pointer has not moved since last pass (no updates or interpolation)
            else if (k==ksave+1) then
                ! update k-1,k,k+1 and interpolate k+2
                do iijj=1,mxmy
                    pkm(iijj) = pk(iijj)
                    pk(iijj) = pkp(iijj)
                    pkp(iijj) = pkpp(iijj)
                end do
                call lint2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
            else if (k==ksave+2) then
                ! update k-1,k and interpolate k+1,k+2
                do iijj=1,mxmy
                    pkm(iijj) = pkp(iijj)
                    pk(iijj) = pkpp(iijj)
                end do
                call lint2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
                call lint2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
            else if (k==ksave+3) then
                ! update k-1 and interpolate k,k+1,k+2
                do iijj=1,mxmy
                    pkm(iijj) = pkpp(iijj)
                end do
                call lint2u(nx,ny,p(1,1,k),mx,my,pk,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
                call lint2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
                call lint2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
            else
                ! interpolate all four k-1,k,k+1,k+2
                call lint2u(nx,ny,p(1,1,k-1),mx,my,pkm,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
                call lint2u(nx,ny,p(1,1,k),mx,my,pk,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
                call lint2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
                call lint2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,jy,dy,pj,pjp,ix,dxm,dx,dxp,dxpp,inmx,jnmy,isubx,jsuby)
            end if

            ! save k pointer for next pass

            ksave = k

            ! cubically interpolate q(ii,jj,kk) from pkm,pk,pkp,pkpp in z direction

            do iijj=1,mxmy
                q(iijj,kk) = dzm(kk)*pkm(iijj) + dz(kk)*pk(iijj) + dzp(kk)*pkp(iijj) + dzpp(kk)*pkpp(iijj)
            end do
        end do

    else

        ! cubic in y

        if (ksubz == 1) then
            ! mz grid is subset of nz grid
            do kk=1,mz
                k = knmz*(kk-1)+1
                call cubt2u(nx,ny,p(1,1,k),mx,my,q(1,kk),intpol,jy,dym,dy,&
                    dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                    inmx,jnmy,isubx,jsuby)

            end do
            return
        end if

        ksave = -3
        do kk=1,mz
            k = kz(kk)
            if (k==ksave) then
                ! k pointer has not moved since last pass (no updates or interpolation)
            else if (k==ksave+1) then
                ! update k-1,k,k+1 and interpolate k+2
                do iijj=1,mxmy
                    pkm(iijj) = pk(iijj)
                    pk(iijj) = pkp(iijj)
                    pkp(iijj) = pkpp(iijj)
                end do
                call cubt2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,&
                            jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,isubx,jsuby)
            else if (k==ksave+2) then
                ! update k-1,k and interpolate k+1,k+2
                do iijj=1,mxmy
                    pkm(iijj) = pkp(iijj)
                    pk(iijj) = pkpp(iijj)
                end do
                call cubt2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,&
                            jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,isubx,jsuby)
                call cubt2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,&
                            jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,isubx,jsuby)
            else if (k==ksave+3) then
                ! update k-1 and interpolate k,k+1,k+2
                do iijj=1,mxmy
                    pkm(iijj) = pkpp(iijj)
                end do
                call cubt2u(nx,ny,p(1,1,k),mx,my,pk,intpol,&
                            jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,isubx,jsuby)
                call cubt2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,&
                            jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,isubx,jsuby)
                call cubt2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,&
                            jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,isubx,jsuby)
            else
                ! interpolate all four k-1,k,k+1,k+2
                call cubt2u(nx,ny,p(1,1,k-1),mx,my,pkm,intpol,&
                            jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,isubx,jsuby)
                call cubt2u(nx,ny,p(1,1,k),mx,my,pk,intpol,&
                            jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,isubx,jsuby)
                call cubt2u(nx,ny,p(1,1,k+1),mx,my,pkp,intpol,&
                            jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,isubx,jsuby)
                call cubt2u(nx,ny,p(1,1,k+2),mx,my,pkpp,intpol,&
                            jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,isubx,jsuby)
            end if

            ! save k pointer for next pass

            ksave = k

            ! cubically interpolate q(ii,jj,kk) from pkm,pk,pkp,pkpp in z direction

            do iijj=1,mxmy
                q(iijj,kk) = dzm(kk)*pkm(iijj) + dz(kk)*pk(iijj) + dzp(kk)*pkp(iijj) + dzpp(kk)*pkpp(iijj)
            end do
        end do

    end if

    end subroutine cubt3u
!**************************************************************************

!**************************************************************************
!>
!  subroutine rgrd4 interpolates the values p(i,j,k,l) on the orthogonal
!  grid (x(i),y(j),z(k),t(l)) for i=1,...,nx;j=1,...,ny;k=1,...,nz;l=1,...,nt
!  onto q(ii,jj,kk,ll) on the orthogonal grid (xx(ii),yy(jj),zz(kk),tt(ll))
!  for ii=1,...,mx;jj=1,...,my;kk=1,...,mz;ll=1,...,mt
!
!### method
!
!  linear or cubic interpolation is used (independently) in
!  each direction (see argument intpol).
!
!### requirements
!
!  each of the x,y,z,t grids must be strictly montonically increasing
!  and each of the xx,yy,zz,tt grids must be montonically increasing
!  (see ier = 4).  in addition the (X,Y,Z,T) region of the q grid
!
!    [xx(1),xx(mx)] X [yy(1),yy(my)] X [zz(1),zz(mz)] X [tt(1),tt(my)]
!
!  must lie within the (X,Y,Z,T) region of the p grid
!
!    [x(1),x(nx)] X [y(1),y(ny)] X [z(1),z(nz)] X [t(1),t(nt)].
!
!  extrapolation is not allowed (see ier=3).  if these (X,Y,Z,T)
!  regions are identical and the orthogonal grids are UNIFORM
!  in each direction then subroutine rgrd4u
!  should be used instead of rgrd4.

    subroutine rgrd4(nx,ny,nz,nt,x,y,z,t,p,mx,my,mz,mt,xx,yy,zz,tt,q,intpol,w,lw,iw,liw,ier)

    implicit none

    integer,intent(in)  :: nx   !! the integer dimension of the grid vector x and the first dimension of p.
                                !! nx > 1 if intpol(1) = 1 or nx > 3 if intpol(1) = 3 is required.
    integer,intent(in)  :: ny   !! the integer dimension of the grid vector y and the second dimension of p.
                                !! ny > 1 if intpol(2) = 1 or ny > 3 if intpol(2) = 3 is required.
    integer,intent(in)  :: nz   !! the integer dimension of the grid vector z and the third dimension of p.
                                !! nz > 1 if intpol(3) = 1 or nz > 3 if intpol(3) = 3 is required.
    integer,intent(in)  :: nt   !! the integer dimension of the grid vector t and the fourth dimension of p.
                                !! nt > 1 if intpol(4) = 1 or nt > 3 if intpol(4) = 3 is required.
    integer,intent(in)  :: mx   !! the integer dimension of the grid vector xx and the first dimension
                                !! of q.  mx > 0 is required.
    integer,intent(in)  :: my   !! the integer dimension of the grid vector yy and the second dimension
                                !! of q.  my > 0 is required.
    integer,intent(in)  :: mz   !! the integer dimension of the grid vector zz and the third dimension
                                !! of q.  mz > 0 is required.
    integer,intent(in)  :: mt   !! the integer dimension of the grid vector tt and the fourth dimension
                                !! of q.  mt > 0 is required.
    integer,intent(in)  :: lw   !! the integer length of the real(wp) work space w.  let
                                !!
                                !! * lwx = mx                  if intpol(1) = 1
                                !! * lwx = 4*mx                if intpol(1) = 3
                                !! * lwy = my+2*mx             if intpol(2) = 1
                                !! * lwy = 4*(my+mx)           if intpol(2) = 3
                                !! * lwz = 2*mx*my+mz          if intpol(3) = 1
                                !! * lwz = 4*(mx*my+mz)        if intpol(3) = 3
                                !! * lwt = 2*mx*my*mz+mt       if intpol(4) = 1
                                !! * lwt = 4*(mx*my*mz+mt)     if intpol(4) = 3
                                !!
                                !! then lw must be greater than or equal to lwx+lwy+lwz+lwt
    integer,intent(in)  :: liw  !! the integer length of the integer work space iw.  liw must be at least mx+my+mz+mt
    integer,intent(out)  :: ier     !! an integer error flag set as follows:
                                    !!
                                    !! * ier = 0 if no errors in input arguments are detected
                                    !! * ier = 1 if  min(mx,my,mz,mt) < 1
                                    !! * ier = 2 if nx < 2 when intpol(1)=1 or nx < 4 when intpol(1)=3 (or)
                                    !!    * ny < 2 when intpol(2)=1 or ny < 4 when intpol(2)=3 (or)
                                    !!    * nz < 2 when intpol(3)=1 or nz < 4 when intpol(3)=3 (or)
                                    !!    * nt < 2 when intpol(4)=1 or nt < 4 when intpol(4)=3
                                    !! * ier = 3 if xx(1) < x(1) or x(nx) < xx(mx) (or)
                                    !!    * yy(1) < y(1) or y(ny) < yy(my) (or)
                                    !!    * zz(1) < z(1) or z(nz) < zz(mz) (or)
                                    !!    * tt(1) < t(1) or t(nt) < tt(mt)
                                    !!   to avoid this flag when end points are intended to be the
                                    !!   same but may differ slightly due to roundoff error, they
                                    !!   should be set exactly in the calling routine (e.g., if both
                                    !!   grids have the same y boundaries then yy(1)=y(1) and yy(my)=y(ny)
                                    !!   should be set before calling rgrd4)
                                    !!
                                    !! * ier = 4 if one of the grids x,y,z,t is not strictly monotonically
                                    !!   increasing or if one of the grids xx,yy,zz,tt is not
                                    !!   montonically increasing.  more precisely if:
                                    !!
                                    !!    * x(i+1) <= x(i) for some i such that 1 <= i < nx (or)
                                    !!    * y(j+1) <= y(j) for some j such that 1 <= j < ny (or)
                                    !!    * z(k+1) <= z(k) for some k such that 1 <= k < nz (or)
                                    !!    * t(l+1) <= t(l) for some l such that 1 <= l < nt (or)
                                    !!    * xx(ii+1) < xx(ii) for some ii such that 1 <= ii < mx (or)
                                    !!    * yy(jj+1) < yy(jj) for some jj such that 1 <= jj < my (or)
                                    !!    * zz(kk+1) < zz(k)  for some kk such that 1 <= kk < mz (or)
                                    !!    * tt(ll+1) < tt(l)  for some ll such that 1 <= ll < mt
                                    !! * ier = 5 if lw or liw is too small (insufficient work space)
                                    !! * ier = 6 if any of intpol(1),intpol(2),intpol(3),intpol(4)
                                    !!                 is not equal to 1 or 3

    integer,intent(inout)  :: iw(liw)   !! an integer work space of length at least liw
                                        !! which must be provided in the routine calling rgrd4
    integer,intent(in)  :: intpol(4)    !! an integer vector of dimension 4 which sets linear or cubic
                                        !! interpolation in each of the x,y,z,t directions as follows:
                                        !!
                                        !! * intpol(1) = 1 sets linear interpolation in the x direction
                                        !! * intpol(1) = 3 sets cubic interpolation in the x direction.
                                        !! * intpol(2) = 1 sets linear interpolation in the y direction
                                        !! * intpol(2) = 3 sets cubic interpolation in the y direction.
                                        !! * intpol(3) = 1 sets linear interpolation in the z direction
                                        !! * intpol(3) = 3 sets cubic interpolation in the z direction.
                                        !! * intpol(4) = 1 sets linear interpolation in the t direction
                                        !! * intpol(4) = 3 sets cubic interpolation in the t direction.
                                        !!
                                        !! values other than 1 or 3 in intpol are not allowed (ier = 6).
    real(wp),intent(in) :: x(nx)    !! a real(wp) nx vector of strictly increasing values which defines the x
                                    !! portion of the orthogonal grid on which p is given
    real(wp),intent(in) :: y(ny)    !! a real(wp) ny vector of strictly increasing values which defines the y
                                    !! portion of the orthogonal grid on which p is given
    real(wp),intent(in) :: z(nz)    !! a real(wp) nz vector of strictly increasing values which defines the z
                                    !! portion of the orthogonal grid on which p is given
    real(wp),intent(in) :: t(nt)    !! a real(wp) nt vector of strictly increasing values which defines the t
                                    !! portion of the orthogonal grid on which p is given
    real(wp),intent(in) :: p(nx,ny,nz,nt)   !! a real(wp) nx by ny by nz by nt array of values given on the (x,y,z,t) grid
    real(wp),intent(inout) :: w(lw) !! a real(wp) work space of length at least lw
                                    !! which must be provided in the routine calling rgrd4
    real(wp),intent(in) :: xx(mx)   !! a real(wp) mx vector of increasing values which defines the x portion of the
                                    !! orthogonal grid on which q is defined.  xx(1) < x(1) or xx(mx) > x(nx)
                                    !! is not allowed (see ier = 3)
    real(wp),intent(in) :: yy(my)   !! a real(wp) my vector of increasing values which defines the y portion of the
                                    !! orthogonal grid on which q is defined.  yy(1) < y(1) or yy(my) > y(ny)
                                    !! is not allowed (see ier = 3)
    real(wp),intent(in) :: zz(mz)   !! a real(wp) mz vector of increasing values which defines the z portion of the
                                    !! orthogonal grid on which q is defined.  zz(1) < z(1) or zz(mz) > z(nz)
                                    !! is not allowed (see ier = 3)
    real(wp),intent(in) :: tt(mt)   !! a real(wp) mt vector of increasing values which defines the t portion of the
                                    !! orthogonal grid on which q is defined.  tt(1) < t(1) or tt(mt) > t(nt)
                                    !! is not allowed (see ier = 3)
    real(wp),intent(out) :: q(mx,my,mz,mt)  !! a real(wp) mx by my by mz by mt array of values on the (xx,yy,zz,tt) grid
                                            !! which are interpolated from p on the (x,y,z,t) grid

    integer  :: l2,l3,l4,l5,l6,l7,l8,l9
    integer  :: k2,k3,k4,k5,k6,k7,k8,k9
    integer  :: j2,j3,j4,j5,j6,j7,j8,j9
    integer  :: i2,i3,i4,i5
    integer  :: lwx,lwy,lwz,lwt,mxmy,mxmymz
    integer  :: ii,jj,kk,ll,i,j,k,l
    integer  :: jy,kz,lt

    ! check input arguments

    ! check (xx,yy,zz,tt) grid resolution
    ier = 1
    if (min(mx,my,mz,mt) < 1) return

    ! check intpol
    ier = 6
    if (intpol(1)/=1 .and. intpol(1)/=3) return
    if (intpol(2)/=1 .and. intpol(2)/=3) return
    if (intpol(3)/=1 .and. intpol(3)/=3) return
    if (intpol(4)/=1 .and. intpol(4)/=3) return

    ! check (x,y,z,t) grid resolution
    ier = 2
    if (intpol(1)==1 .and. nx<2) return
    if (intpol(1)==3 .and. nx<4) return
    if (intpol(2)==1 .and. ny<2) return
    if (intpol(2)==3 .and. ny<4) return
    if (intpol(3)==1 .and. nz<2) return
    if (intpol(3)==3 .and. nz<4) return
    if (intpol(4)==1 .and. nt<2) return
    if (intpol(4)==3 .and. nt<4) return

    ! check work space length input and set minimum
    ier = 5
    mxmy = mx*my
    mxmymz = mxmy*mz
    if (intpol(1)==1) then
        lwx = mx
    else
        lwx = 4*mx
    end if
    if (intpol(2)==1) then
        lwy = (my+2*mx)
    else
        lwy = 4*(mx+my)
    end if
    if (intpol(3)==1) then
        lwz = (2*mxmy+mz)
    else
        lwz = 4*(mxmy+mz)
    end if
    if (intpol(4)==1) then
        lwt = (2*mxmymz+mt)
    else
        lwt = 4*(mxmymz+mt)
    end if
    if (lw < lwx+lwy+lwz+lwt) return
    if (liw < mx+my+mz+mt) return

    ! check (xx,yy,zz,tt) grid contained in (x,y,z,t) grid
    ier = 3
    if (xx(1)<x(1) .or. xx(mx)>x(nx)) return
    if (yy(1)<y(1) .or. yy(my)>y(ny)) return
    if (zz(1)<z(1) .or. zz(mz)>z(nz)) return
    if (tt(1)<t(1) .or. tt(mt)>t(nt)) return

    ! check montonicity of grids
    ier = 4
    do i=2,nx
        if (x(i-1)>=x(i)) return
    end do
    do j=2,ny
        if (y(j-1)>=y(j)) return
    end do
    do k=2,nz
        if (z(k-1)>=z(k)) return
    end do
    do l=2,nt
        if (t(l-1)>=t(l)) return
    end do
    do ii=2,mx
        if (xx(ii-1)>xx(ii)) return
    end do
    do jj=2,my
        if (yy(jj-1)>yy(jj)) return
    end do
    do kk=2,mz
        if (zz(kk-1)>zz(kk)) return
    end do
    do ll=2,mt
        if (tt(ll-1)>tt(ll)) return
    end do

    ! arguments o.k.

    ier = 0

    ! set pointers for integer work space iw

    jy = mx+1
    kz = mx+my+1
    lt = mx+my+mz+1

    if (intpol(4)==1) then

        ! linearly interpolate in nt, set work space pointers and scales

        l2 = 1
        l3 = l2
        l4 = l3+mt
        l5 = l4
        l6 = l5
        l7 = l6
        l8 = l7+mxmymz
        l9 = l8+mxmymz
        call linmx(nt,t,mt,tt,iw(lt),w(l3))
        k2 = l9

        if (intpol(3)==1) then
            ! linear in z
            k3 = k2
            k4 = k3+mz
            k5 = k4
            k6 = k5
            k7 = k6
            k8 = k7+mxmy
            k9 = k8+mxmy
            call linmx(nz,z,mz,zz,iw(kz),w(k3))
            j2 = k9
        else
            ! cubic in z
            k3 = k2+mz
            k4 = k3+mz
            k5 = k4+mz
            k6 = k5+mz
            k7 = k6+mxmy
            k8 = k7+mxmy
            k9 = k8+mxmy
            call cubnmx(nz,z,mz,zz,iw(kz),w(k2),w(k3),w(k4),w(k5))
            j2 = k9+mxmy
        end if

        if (intpol(2) == 1) then
            ! linear in y
            j3 = j2
            j4 = j3+my
            j5 = j4
            j6 = j5
            j7 = j6
            j8 = j7+mx
            j9 = j8+mx
            call linmx(ny,y,my,yy,iw(jy),w(j3))
            i2 = j9
        else
            ! cubic in y
            j3 = j2+my
            j4 = j3+my
            j5 = j4+my
            j6 = j5+my
            j7 = j6+mx
            j8 = j7+mx
            j9 = j8+mx
            call cubnmx(ny,y,my,yy,iw(jy),w(j2),w(j3),w(j4),w(j5))
            i2 = j9+mx
        end if

        if (intpol(1) == 1) then
            ! linear in x
            i3 = i2
            i4 = i3
            i5 = i4
            call linmx(nx,x,mx,xx,iw,w(i3))
        else
            ! cubic in x
            i3 = i2+mx
            i4 = i3+mx
            i5 = i4+mx
            call cubnmx(nx,x,mx,xx,iw,w(i2),w(i3),w(i4),w(i5))
        end if

        ! linearly interpolate in t

        call lint4(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,&
                    iw(lt),w(l3),w(l7),w(l8),&
                    iw(kz),w(k2),w(k3),w(k4),w(k5),w(k6),w(k7),w(k8),w(k9),&
                    iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),w(j7),w(j8),w(j9),&
                    iw,w(i2),w(i3),w(i4),w(i5))

    else

        ! cubically interpolate in t

        l2 = 1
        l3 = l2+mt
        l4 = l3+mt
        l5 = l4+mt
        l6 = l5+mt
        l7 = l6+mxmymz
        l8 = l7+mxmymz
        l9 = l8+mxmymz
        call cubnmx(nt,t,mt,tt,iw(lt),w(l2),w(l3),w(l4),w(l5))
        k2 = l9+mxmymz

        if (intpol(3)==1) then
            ! linear in z
            k3 = k2
            k4 = k3+mz
            k5 = k4
            k6 = k5
            k7 = k6
            k8 = k7+mxmy
            k9 = k8+mxmy
            call linmx(nz,z,mz,zz,iw(kz),w(k3))
            j2 = k9
        else
            ! cubic in z
            k3 = k2+mz
            k4 = k3+mz
            k5 = k4+mz
            k6 = k5+mz
            k7 = k6+mxmy
            k8 = k7+mxmy
            k9 = k8+mxmy
            call cubnmx(nz,z,mz,zz,iw(kz),w(k2),w(k3),w(k4),w(k5))
            j2 = k9+mxmy
        end if

        if (intpol(2) == 1) then
            j3 = j2
            j4 = j3+my
            j5 = j4
            j6 = j5
            j7 = j6
            j8 = j7+mx
            j9 = j8+mx
            call linmx(ny,y,my,yy,iw(jy),w(j3))
            i2 = j9
        else
            j3 = j2+my
            j4 = j3+my
            j5 = j4+my
            j6 = j5+my
            j7 = j6+mx
            j8 = j7+mx
            j9 = j8+mx
            call cubnmx(ny,y,my,yy,iw(jy),w(j2),w(j3),w(j4),w(j5))
            i2 = j9+mx
        end if

        ! set work space portion and indices which depend on x interpolation

        if (intpol(1) == 1) then
            i3 = i2
            i4 = i3
            i5 = i4
            call linmx(nx,x,mx,xx,iw,w(i3))
        else
            i3 = i2+mx
            i4 = i3+mx
            i5 = i4+mx
            call cubnmx(nx,x,mx,xx,iw,w(i2),w(i3),w(i4),w(i5))
        end if

        ! cubically interpolate in t

        call cubt4(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,&
                    iw(lt),w(l2),w(l3),w(l4),w(l5),w(l6),w(l7),w(l8),w(l9),&
                    iw(kz),w(k2),w(k3),w(k4),w(k5),w(k6),w(k7),w(k8),w(k9),&
                    iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),w(j7),w(j8),w(j9),&
                    iw,w(i2),w(i3),w(i4),w(i5))

    end if

    end subroutine rgrd4
!**************************************************************************

!**************************************************************************
!>
!  linearly interpolate in t direction

    subroutine lint4(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,&
                     lt,dt,pt,ptp,kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,&
                     jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)

    implicit none

    integer  :: nx,ny,nz,nt,mx,my,mz,mt,mxmy,mxmymz,lsave,ll,l,iijjkk
    integer  :: lt(mt),kz(mz),jy(my),ix(mx),intpol(4)
    real(wp) :: p(nx,ny,nz,nt),q(mxmymz,mt)
    real(wp) :: dt(mt),pt(mxmymz),ptp(mxmymz)
    real(wp) :: dzm(mz),dz(mz),dzp(mz),dzpp(mz)
    real(wp) :: pkm(mxmy),pk(mxmy),pkp(mxmy),pkpp(mxmy)
    real(wp) :: dym(my),dy(my),dyp(my),dypp(my)
    real(wp) :: pjm(mx),pj(mx),pjp(mx),pjpp(mx)
    real(wp) :: dxm(mx),dx(mx),dxp(mx),dxpp(mx)

    if (intpol(3) == 1) then

        ! linear in z

        lsave = -1
        do ll=1,mt
            l = lt(ll)
            if (l==lsave) then
                ! l pointer has not moved since last pass (no updates or interpolation)
            else if (l==lsave+1) then
                ! update l and interpolate l+1
                do iijjkk=1,mxmymz
                    pt(iijjkk) = ptp(iijjkk)
                end do
                call lint3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,&
                            kz,dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,&
                            dxm,dx,dxp,dxpp)
            else
                ! interpolate l,l+1 in pt,ptp on xx,yy,zz mesh
                call lint3(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,&
                            dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
                call lint3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,&
                            dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
            end if

            ! save l pointer for next pass

            lsave = l

            ! linearly interpolate q(ii,jj,,kk,ll) from pt,ptp in t direction

            do iijjkk=1,mxmymz
                q(iijjkk,ll) = pt(iijjkk)+dt(ll)*(ptp(iijjkk)-pt(iijjkk))
            end do
        end do

    else

        ! cubic in z

        lsave = -1
        do ll=1,mt
            l = lt(ll)
            if (l==lsave) then
                ! l pointer has not moved since last pass (no updates or interpolation)
            else if (l==lsave+1) then
                ! update l and interpolate l+1
                do iijjkk=1,mxmymz
                    pt(iijjkk) = ptp(iijjkk)
                end do
                call cubt3(nx,ny,nt,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,&
                            kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,&
                            jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
            else
                ! interpolate l,l+1 in pt,ptp on xx,yy,zz mesh
                call cubt3(nx,ny,nt,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,&
                            kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,&
                            jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
                call cubt3(nx,ny,nt,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,&
                            kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,&
                            jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
            end if

            ! save l pointer for next pass

            lsave = l

            ! linearly interpolate q(ii,jj,kk,ll) from pt,ptp in t direction

            do iijjkk=1,mxmymz
                q(iijjkk,ll) = pt(iijjkk)+dt(ll)*(ptp(iijjkk)-pt(iijjkk))
            end do

        end do

    end if

    end subroutine lint4
!**************************************************************************

!**************************************************************************
!>
!  cubically interpolate in t

    subroutine cubt4(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,&
                     lt,dtm,dt,dtp,dtpp,ptm,pt,ptp,ptpp,&
                     kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,&
                     jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,&
                     ix,dxm,dx,dxp,dxpp)

    implicit none

    integer  :: nx,ny,nz,nt,mx,my,mz,mt,mxmy,mxmymz,lsave,ll,l,iijjkk
    integer  :: lt(mt),kz(mz),jy(my),ix(mx),intpol(4)
    real(wp) :: p(nx,ny,nz,nt),q(mxmymz,mt)
    real(wp) :: dtm(mt),dt(mt),dtp(mt),dtpp(mt)
    real(wp) :: ptm(mxmymz),pt(mxmymz),ptp(mxmymz),ptpp(mxmymz)
    real(wp) :: dzm(mz),dz(mz),dzp(mz),dzpp(mz)
    real(wp) :: pkm(mxmy),pk(mxmy),pkp(mxmy),pkpp(mxmy)
    real(wp) :: dym(my),dy(my),dyp(my),dypp(my)
    real(wp) :: pjm(mx),pj(mx),pjp(mx),pjpp(mx)
    real(wp) :: dxm(mx),dx(mx),dxp(mx),dxpp(mx)

    if (intpol(3) == 1) then

        ! linear in z

        lsave = -3
        do ll=1,mt
            l = lt(ll)
            if (l==lsave) then
                ! l pointer has not moved since last pass (no updates or interpolation)
            else if (l==lsave+1) then
                ! update l-1,l,l+1 and interpolate l+2
                do iijjkk=1,mxmymz
                    ptm(iijjkk) = pt(iijjkk)
                    pt(iijjkk) = ptp(iijjkk)
                    ptp(iijjkk) = ptpp(iijjkk)
                end do
                call lint3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dz,&
                            pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
            else if (l==lsave+2) then
                ! update l-1,l and interpolate l+1,l+2
                do iijjkk=1,mxmymz
                    ptm(iijjkk) = ptp(iijjkk)
                    pt(iijjkk) = ptpp(iijjkk)
                end do
                call lint3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,dz,&
                            pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
                call lint3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dz,&
                            pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
            else if (l==lsave+3) then
                ! update l-1 and interpolate l,l+1,l+2

                do iijjkk=1,mxmymz
                    ptm(iijjkk) = ptpp(iijjkk)
                end do
                call lint3(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,dz,&
                            pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
                call lint3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,dz,&
                            pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
                call lint3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dz,&
                            pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
            else
                ! interpolate all four l-1,l,l+1,l+2
                call lint3(nx,ny,nz,p(1,1,1,l-1),mx,my,mxmy,mz,ptm,intpol,kz,dz,&
                            pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
                call lint3(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,dz,&
                            pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
                call lint3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,dz,&
                            pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
                call lint3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dz,&
                            pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp)
            end if

            ! save l pointer for next pass

            lsave = l

            ! cubically interpolate q(ii,jj,kk,ll) from ptm,pt,ptp,ptpp in t direction

            do iijjkk=1,mxmymz
                q(iijjkk,ll) = dtm(ll)*ptm(iijjkk) + dt(ll)*pt(iijjkk) + dtp(ll)*ptp(iijjkk) + dtpp(ll)*ptpp(iijjkk)
            end do
        end do

    else

        ! cubic in z

        lsave = -3
        do ll=1,mt
            l = lt(ll)
            if (l==lsave) then
                ! l pointer has not moved since last pass (no updates or interpolation)
            else if (l==lsave+1) then
                ! update l-1,l,l+1 and interpolate l+2
                do iijjkk=1,mxmymz
                    ptm(iijjkk) = pt(iijjkk)
                    pt(iijjkk) = ptp(iijjkk)
                    ptp(iijjkk) = ptpp(iijjkk)
                end do
                call cubt3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dzm,&
                            dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,&
                            ix,dxm,dx,dxp,dxpp)
            else if (l==lsave+2) then
                ! update l-1,l and interpolate l+1,l+2
                do iijjkk=1,mxmymz
                    ptm(iijjkk) = ptp(iijjkk)
                    pt(iijjkk) = ptpp(iijjkk)
                end do
                call cubt3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,dzm,&
                            dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,&
                            ix,dxm,dx,dxp,dxpp)
                call cubt3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dzm,&
                            dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,&
                            ix,dxm,dx,dxp,dxpp)
            else if (l==lsave+3) then
                ! update l-1 and interpolate l,l+1,l+2
                do iijjkk=1,mxmymz
                    ptm(iijjkk) = ptpp(iijjkk)
                end do
                call cubt3(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,dzm,&
                        dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,&
                        ix,dxm,dx,dxp,dxpp)
                call cubt3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,dzm,&
                        dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,&
                        ix,dxm,dx,dxp,dxpp)
                call cubt3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dzm,&
                        dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,&
                        ix,dxm,dx,dxp,dxpp)
            else
                ! interpolate all four l-1,l,l+1,l+2
                call cubt3(nx,ny,nz,p(1,1,1,l-1),mx,my,mxmy,mz,ptm,intpol,kz,dzm,&
                            dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,&
                            ix,dxm,dx,dxp,dxpp)
                call cubt3(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,dzm,&
                            dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,&
                            ix,dxm,dx,dxp,dxpp)
                call cubt3(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,dzm,&
                            dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,&
                            ix,dxm,dx,dxp,dxpp)
                call cubt3(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,dzm,&
                            dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,&
                            ix,dxm,dx,dxp,dxpp)
            end if

            ! save l pointer for next pass

            lsave = l

            ! cubically interpolate q(ii,jj,kk,ll) from ptm,pt,ptp,ptpp in t direction

            do iijjkk=1,mxmymz
                q(iijjkk,ll) = dtm(ll)*ptm(iijjkk) + dt(ll)*pt(iijjkk) + dtp(ll)*ptp(iijjkk) + dtpp(ll)*ptpp(iijjkk)
            end do
        end do

    end if

    end subroutine cubt4
!**************************************************************************

!**************************************************************************
!>
!  subroutine rgrd4u interpolates the nx by ny by nz by nt array p onto
!  the mx by my by mz by mt array q.  it is assumed that p and q are
!  values on uniform nx by ny by nz by nt and mx by my by mz by mt grids
!  which are superimposed on the same box region (INCLUDING BOUNDARIES).
!  if p and q are values on nonuniform orthogonal grids and/or if the grid
!  on which q is defined lies within the p grid then subroutine rgrd4
!  should be used.
!
!### method
!
!  linear or cubic interpolation (see intpol) is used in each
!  direction for which the q grid is not a subgrid of the p grid.
!  [the mx (my,mz,mt) uniform grid is a subgrid of the nx (ny,nz,nt)
!  uniform grid if and only if mx-1 (my-1,nz-1,nt-1) divides nx-1
!  (ny-1,nz-1,nt-1)].  Values are set directly without (the need for)
!  interpolation in subgrid directions.

    subroutine rgrd4u(nx,ny,nz,nt,p,mx,my,mz,mt,q,intpol,w,lw,iw,liw,ier)

    implicit none

    integer,intent(in)      :: nx   !! the integer first dimension of p.  nx > 1 if intpol(1) = 1 or
                                    !! nx > 3 if intpol(1) = 3 is required (see ier = 2).
    integer,intent(in)      :: ny   !! the integer second dimension of p.  ny > 1 if intpol(2) = 1 or
                                    !! ny > 3 if intpol(2) = 3 is required (see ier = 2).
    integer,intent(in)      :: nz   !! the integer third dimension of p.  nz > 1 if intpol(3) = 1 or
                                    !! nz > 3 if intpol(3) = 3 is required (see ier = 2)
    integer,intent(in)      :: nt   !! the integer fourth dimension of p.  nt > 1 if intpol(4) = 1 or
                                    !! nt > 3 if intpol(4) = 3 is required (see ier=2)
    integer,intent(in)      :: mx   !! the integer first dimension of q.  mx > 1 is required (see ier = 1)
    integer,intent(in)      :: my   !! the integer second dimension of q. my > 1 is required (see ier = 1)
    integer,intent(in)      :: mz   !! the integer third dimension of q. mz > 1 is required (see ier = 1)
    integer,intent(in)      :: mt   !! the integer fourth dimension of q. mt > 1 is required (see ier = 1)
    integer,intent(in)      :: intpol(4)    !! an integer vector of dimension 4 which sets linear or cubic
                                            !! interpolation in each of the x,y,z,t directions as follows:
                                            !!
                                            !! * intpol(1) = 1 sets linear interpolation in the x direction
                                            !! * intpol(1) = 3 sets cubic interpolation in the x direction.
                                            !! * intpol(2) = 1 sets linear interpolation in the y direction
                                            !! * intpol(2) = 3 sets cubic interpolation in the y direction.
                                            !! * intpol(3) = 1 sets linear interpolation in the z direction
                                            !! * intpol(3) = 3 sets cubic interpolation in the z direction.
                                            !! * intpol(4) = 1 sets linear interpolation in the t direction
                                            !! * intpol(4) = 3 sets cubic interpolation in the t direction.
                                            !!
                                            !! values other than 1 or 3 in intpol are not allowed (ier = 3).
    integer,intent(in)      :: liw          !! the integer length of the integer work space iw.
                                            !! liw must be at least mx+my+mz+mt
    integer,intent(inout)   :: iw(liw)      !! an integer work space of length at least liw
                                            !! which must be provided in the routine calling rgrd4u
    integer,intent(in)      :: lw           !! the integer length of the work space w.
                                            !!
                                            !! * let lwx = 1 if mx-1 divides nx-1; otherwise
                                            !!   let lwx = mx if intpol(1) = 1 or
                                            !!   let lwx = 4*mx if intpol(1) = 3
                                            !! * let lwy = 0 if my-1 divides ny-1; otherwise
                                            !!   let lwy = my+2*mx if intpol(2) = 1 or
                                            !!   let lwy = 4*(mx+my) if intpol(2) = 3
                                            !! * let lwz = 0 if mz-1 divides nz-1; otherwise
                                            !!   let lwz = 2*mx*my+mz if intpol(3) = 1 or
                                            !!   let lwz = 4*(mx*my+mz) if intpol(3) = 3
                                            !! * let lwt = 0 if mt-1 divides nt-1; otherwise
                                            !!   let lwt = 2*mx*my*mz+mt if intpol(4) = 1 or
                                            !!   let lwt = 4*(mx*my*mz+mt) if intpol(4) = 3
                                            !!
                                            !! then lw must be greater than or equal to lwx+lwy+lwz+lwt
    integer,intent(out)     :: ier          !! an integer error flag set as follows:
                                            !!
                                            !! * ier = 0 if no errors in input arguments are detected
                                            !! * ier = 1 if min(mx,my,mz,mt) < 2
                                            !! * ier = 2 if nx < 2 when intpol(1)=1 or nx < 4 when intpol(1)=3 (or)
                                            !!                     ny < 2 when intpol(2)=1 or ny < 4 when intpol(2)=3 (or)
                                            !!                     nz < 2 when intpol(3)=1 or nz < 4 when intpol(3)=3 (or)
                                            !!                     nt < 2 when intpol(4)=1 or nt < 4 when intpol(4)=3.
                                            !! * ier = 3 if any of intpol(1),intpol(2),intpol(3),intpol(4)  is not
                                            !!                 equal to 1 or 3.
                                            !! * ier = 4 if lw or liw is too small (insufficient work space)
    real(wp),intent(in)     :: p(nx,ny,nz,nt)   !! a real(wp) nx by ny by nz by nt array of given values
    real(wp),intent(out)    :: q(mx,my,mz,mt)   !! a real(wp) mx by my by mz by mt array of values which are interpolated from p.
    real(wp),intent(inout)  :: w(lw)    !! a real(wp) work space of length at least lw
                                        !! which must be provided in the routine calling rgrd4u


    integer :: inmx,jnmy,knmz,lnmt,isubx,jsuby,ksubz,lsubt
    integer :: mxmy,mxmymz,lwx,lwy,lwz,lwt,jy,kz,lt
    integer :: i2,i3,i4,i5
    integer :: j2,j3,j4,j5,j6,j7,j8,j9
    integer :: k2,k3,k4,k5,k6,k7,k8,k9
    integer :: l2,l3,l4,l5,l6,l7,l8,l9

    ! check input arguments

    ! check mx,my,mz,mt
    ier = 1
    if (min(mx,my,mz,mt) < 1) return

    ! check intpol
    ier = 3
    if (intpol(1)/=1 .and. intpol(1)/=3) return
    if (intpol(2)/=1 .and. intpol(2)/=3) return
    if (intpol(3)/=1 .and. intpol(3)/=3) return
    if (intpol(4)/=1 .and. intpol(4)/=3) return

    ! check nx,ny,nz,nt
    ier = 2
    if (intpol(1)==1 .and. nx<2) return
    if (intpol(1)==3 .and. nx<4) return
    if (intpol(2)==1 .and. ny<2) return
    if (intpol(2)==3 .and. ny<4) return
    if (intpol(3)==1 .and. nz<2) return
    if (intpol(3)==3 .and. nz<4) return
    if (intpol(4)==1 .and. nt<2) return
    if (intpol(4)==3 .and. nt<4) return

    ! set subgrid indicators
    inmx = (nx-1)/(mx-1)
    jnmy = (ny-1)/(my-1)
    knmz = (nz-1)/(mz-1)
    lnmt = (nt-1)/(mt-1)
    isubx = nx - inmx*(mx-1)
    jsuby = ny - jnmy*(my-1)
    ksubz = nz - knmz*(mz-1)
    lsubt = nt - lnmt*(mt-1)

    ! check work space length input
    ier = 4
    mxmy = mx*my
    mxmymz = mxmy*mz
    lwx = 1
    if (isubx/=1) then
        if (intpol(1)==1) then
            lwx = mx
        else
            lwx = 4*mx
        end if
    end if
    lwy = 0
    if (jsuby/=1) then
        if (intpol(2)==1) then
            lwy = (2*mx+my)
        else
            lwy = 4*my+4*mx
        end if
    end if
    lwz = 0
    if (ksubz/=1) then
        if (intpol(3)==1) then
            lwz = (2*mxmy+mz)
        else
            lwz = 4*mxmy+4*mz
        end if
    end if
    lwt = 0
    if (lsubt/=1) then
        if (intpol(4)==1) then
            lwt = (2*mxmymz+mt)
        else
            lwt = 4*mxmymz+4*mt
        end if
    end if

    if (lw < lwx+lwy+lwz+lwt) return
    if (liw < mx+my+mz+mt) return

    ! arguments o.k.

    ier = 0
    jy = mx+1
    kz = mx+my+1
    lt = mx+my+mz+1

    if (intpol(4)==1) then

        ! linearly interpolate in nt, set work space pointers and scales

        l2 = 1
        l3 = l2
        l4 = l3+mt
        l5 = l4
        l6 = l5
        l7 = l6
        l8 = l7+mxmymz
        l9 = l8+mxmymz
        call linmxu(nt,mt,iw(lt),w(l3))
        k2 = l9
        if (intpol(3)==1) then
            ! linear in z
            k3 = k2
            k4 = k3+mz
            k5 = k4
            k6 = k5
            k7 = k6
            k8 = k7+mxmy
            k9 = k8+mxmy
            call linmxu(nz,mz,iw(kz),w(k3))
            j2 = k9
        else
            ! cubic in z
            k3 = k2+mz
            k4 = k3+mz
            k5 = k4+mz
            k6 = k5+mz
            k7 = k6+mxmy
            k8 = k7+mxmy
            k9 = k8+mxmy
            call cubnmxu(nz,mz,iw(kz),w(k2),w(k3),w(k4),w(k5))
            j2 = k9+mxmy
        end if

        if (intpol(2) == 1) then
            ! linear in y
            j3 = j2
            j4 = j3+my
            j5 = j4
            j6 = j5
            j7 = j6
            j8 = j7+mx
            j9 = j8+mx
            call linmxu(ny,my,iw(jy),w(j3))
            i2 = j9
        else
            ! cubic in y
            j3 = j2+my
            j4 = j3+my
            j5 = j4+my
            j6 = j5+my
            j7 = j6+mx
            j8 = j7+mx
            j9 = j8+mx
            call cubnmxu(ny,my,iw(jy),w(j2),w(j3),w(j4),w(j5))
            i2 = j9+mx
        end if

        if (intpol(1) == 1) then
            ! linear in x
            i3 = i2
            i4 = i3
            i5 = i4
            call linmxu(nx,mx,iw,w(i3))
        else
            ! cubic in x
            i3 = i2+mx
            i4 = i3+mx
            i5 = i4+mx
            call cubnmxu(nx,mx,iw,w(i2),w(i3),w(i4),w(i5))
        end if

        ! linearly interpolate in t

        call lint4u(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,&
                    iw(lt),w(l3),w(l7),w(l8),&
                    iw(kz),w(k2),w(k3),w(k4),w(k5),w(k6),w(k7),w(k8),w(k9),&
                    iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),w(j7),w(j8),w(j9),&
                    iw,w(i2),w(i3),w(i4),w(i5),&
                    inmx,jnmy,knmz,lnmt,isubx,jsuby,ksubz,lsubt)

    else

        ! cubically interpolate in t

        l2 = 1
        l3 = l2+mt
        l4 = l3+mt
        l5 = l4+mt
        l6 = l5+mt
        l7 = l6+mxmymz
        l8 = l7+mxmymz
        l9 = l8+mxmymz
        call cubnmxu(nt,mt,iw(lt),w(l2),w(l3),w(l4),w(l5))
        k2 = l9+mxmymz

        if (intpol(3)==1) then
            ! linear in z
            k3 = k2
            k4 = k3+mz
            k5 = k4
            k6 = k5
            k7 = k6
            k8 = k7+mxmy
            k9 = k8+mxmy
            call linmxu(nz,mz,iw(kz),w(k3))
            j2 = k9
        else
            ! cubic in z
            k3 = k2+mz
            k4 = k3+mz
            k5 = k4+mz
            k6 = k5+mz
            k7 = k6+mxmy
            k8 = k7+mxmy
            k9 = k8+mxmy
            call cubnmxu(nz,mz,iw(kz),w(k2),w(k3),w(k4),w(k5))
            j2 = k9+mxmy
        end if

        if (intpol(2) == 1) then
            j3 = j2
            j4 = j3+my
            j5 = j4
            j6 = j5
            j7 = j6
            j8 = j7+mx
            j9 = j8+mx
            call linmxu(ny,my,iw(jy),w(j3))
            i2 = j9
        else
            j3 = j2+my
            j4 = j3+my
            j5 = j4+my
            j6 = j5+my
            j7 = j6+mx
            j8 = j7+mx
            j9 = j8+mx
            call cubnmxu(ny,my,iw(jy),w(j2),w(j3),w(j4),w(j5))
            i2 = j9+mx
        end if

        ! set work space portion and indices which depend on x interpolation

        if (intpol(1) == 1) then
            i3 = i2
            i4 = i3
            i5 = i4
            call linmxu(nx,mx,iw,w(i3))
        else
            i3 = i2+mx
            i4 = i3+mx
            i5 = i4+mx
            call cubnmxu(nx,mx,iw,w(i2),w(i3),w(i4),w(i5))
        end if

        ! cubically interpolate in t

        call cubt4u(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,&
                    iw(lt),w(l2),w(l3),w(l4),w(l5),w(l6),w(l7),w(l8),w(l9),&
                    iw(kz),w(k2),w(k3),w(k4),w(k5),w(k6),w(k7),w(k8),w(k9),&
                    iw(jy),w(j2),w(j3),w(j4),w(j5),w(j6),w(j7),w(j8),w(j9),&
                    iw,w(i2),w(i3),w(i4),w(i5),&
                    inmx,jnmy,knmz,lnmt,isubx,jsuby,ksubz,lsubt)

    end if

    end subroutine rgrd4u
!**************************************************************************

!**************************************************************************
!>
!  linearly interpolate in t direction

    subroutine lint4u(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,&
                      lt,dt,pt,ptp,kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,&
                      jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                      inmx,jnmy,knmz,lnmt,isubx,jsuby,ksubz,lsubt)

    implicit none

    integer  :: nx,ny,nz,nt,mx,my,mz,mt
    integer  :: mxmy,mxmymz
    real(wp) :: p(nx,ny,nz,nt),q(mxmymz,mt)
    integer  :: inmx,jnmy,knmz,lnmt,isubx,jsuby,ksubz,lsubt
    real(wp) :: dt(mt),pt(mxmymz),ptp(mxmymz)
    real(wp) :: dzm(mz),dz(mz),dzp(mz),dzpp(mz)
    real(wp) :: pkm(mxmy),pk(mxmy),pkp(mxmy),pkpp(mxmy)
    real(wp) :: dym(my),dy(my),dyp(my),dypp(my)
    real(wp) :: pjm(mx),pj(mx),pjp(mx),pjpp(mx)
    real(wp) :: dxm(mx),dx(mx),dxp(mx),dxpp(mx)
    integer  :: lt(mt),kz(mz),jy(my),ix(mx),intpol(4)
    integer  :: l,ll,lsave,iijjkk

    if (intpol(3) == 1) then
        ! linear in z
        if (lsubt == 1) then
            ! mt grid is subset of nt grid
            do ll=1,mt
                l = lnmt*(ll-1)+1
                call lint3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,q(1,ll),intpol,kz,&
                            dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,knmz,isubx,jsuby,ksubz)
            end do
            return
        end if

        lsave = -1
        do ll=1,mt
            l = lt(ll)
            if (l==lsave) then
                ! l pointer has not moved since last pass (no updates or interpolation)
            else if (l==lsave+1) then
                ! update l and interpolate l+1
                do iijjkk=1,mxmymz
                    pt(iijjkk) = ptp(iijjkk)
                end do
                call lint3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,&
                            dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,knmz,isubx,jsuby,ksubz)
            else
                ! interpolate l,l+1 in pt,ptp on xx,yy,zz mesh
                call lint3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,dz,&
                            pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,knmz,isubx,jsuby,ksubz)
                call lint3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,&
                            dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,knmz,isubx,jsuby,ksubz)
            end if

            ! save l pointer for next pass

            lsave = l

            ! linearly interpolate q(ii,jj,,kk,ll) from pt,ptp in t direction

            do iijjkk=1,mxmymz
                q(iijjkk,ll) = pt(iijjkk)+dt(ll)*(ptp(iijjkk)-pt(iijjkk))
            end do
        end do

    else

        ! cubic in z

        if (lsubt == 1) then
            ! mt grid is subset of nt grid
            do ll=1,mt
                l = lnmt*(ll-1)+1
                call cubt3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,q(1,ll),intpol,&
                    kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,&
                    pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                    inmx,jnmy,knmz,isubx,jsuby,ksubz)
            end do
            return
        end if

        lsave = -1
        do ll=1,mt
            l = lt(ll)
            if (l==lsave) then
                ! l pointer has not moved since last pass (no updates or interpolation)
            else if (l==lsave+1) then
                ! update l and interpolate l+1
                do iijjkk=1,mxmymz
                    pt(iijjkk) = ptp(iijjkk)
                end do
                call cubt3u(nx,ny,nt,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,&
                    kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,&
                    jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                    inmx,jnmy,knmz,isubx,jsuby,ksubz)
            else
                ! interpolate l,l+1 in pt,ptp on xx,yy,zz mesh
                call cubt3u(nx,ny,nt,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,&
                    kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,&
                    jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                    inmx,jnmy,knmz,isubx,jsuby,ksubz)
                call cubt3u(nx,ny,nt,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,&
                    kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,&
                    jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                    inmx,jnmy,knmz,isubx,jsuby,ksubz)
            end if

            ! save l pointer for next pass
            lsave = l

            ! linearly interpolate q(ii,jj,kk,ll) from pt,ptp in t direction
            do iijjkk=1,mxmymz
                q(iijjkk,ll) = pt(iijjkk)+dt(ll)*(ptp(iijjkk)-pt(iijjkk))
            end do

        end do

    end if

    end subroutine lint4u
!**************************************************************************

!**************************************************************************
!>
!  cubically interpolate in t

    subroutine cubt4u(nx,ny,nz,nt,p,mx,my,mz,mt,mxmy,mxmymz,q,intpol,&
                        lt,dtm,dt,dtp,dtpp,ptm,pt,ptp,ptpp,&
                        kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,&
                        jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,&
                        ix,dxm,dx,dxp,dxpp,&
                        inmx,jnmy,knmz,lnmt,isubx,jsuby,ksubz,lsubt)

    implicit none

    integer  :: nx,ny,nz,nt,mx,my,mz,mt
    integer  :: inmx,jnmy,knmz,lnmt,isubx,jsuby,ksubz,lsubt
    integer  :: mxmy,mxmymz
    real(wp) :: p(nx,ny,nz,nt),q(mxmymz,mt)
    real(wp) :: ptm(mxmymz),pt(mxmymz),ptp(mxmymz),ptpp(mxmymz)
    real(wp) :: dtm(mt),dt(mt),dtp(mt),dtpp(mt)
    real(wp) :: dzm(mz),dz(mz),dzp(mz),dzpp(mz)
    real(wp) :: pkm(mxmy),pk(mxmy),pkp(mxmy),pkpp(mxmy)
    real(wp) :: dym(my),dy(my),dyp(my),dypp(my)
    real(wp) :: pjm(mx),pj(mx),pjp(mx),pjpp(mx)
    real(wp) :: dxm(mx),dx(mx),dxp(mx),dxpp(mx)
    integer  :: lt(mt),kz(mz),jy(my),ix(mx),intpol(4)
    integer  :: l,ll,iijjkk,lsave

    if (intpol(3) == 1) then

        ! linear in z

        if (lsubt == 1) then
            ! mt grid is subset of nt grid
            do ll=1,mt
                l = lnmt*(ll-1)+1
                call lint3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,q(1,ll),intpol,kz,&
                            dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,knmz,isubx,jsuby,ksubz)
            end do
            return
        end if
        lsave = -3
        do ll=1,mt
            l = lt(ll)
            if (l==lsave) then
                ! l pointer has not moved since last pass (no updates or interpolation)
            else if (l==lsave+1) then
                ! update l-1,l,l+1 and interpolate l+2
                do iijjkk=1,mxmymz
                    ptm(iijjkk) = pt(iijjkk)
                    pt(iijjkk) = ptp(iijjkk)
                    ptp(iijjkk) = ptpp(iijjkk)
                end do
                call lint3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,&
                            dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,knmz,isubx,jsuby,ksubz)
            else if (l==lsave+2) then
                ! update l-1,l and interpolate l+1,l+2
                do iijjkk=1,mxmymz
                    ptm(iijjkk) = ptp(iijjkk)
                    pt(iijjkk) = ptpp(iijjkk)
                end do
                call lint3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,&
                            dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,knmz,isubx,jsuby,ksubz)
                call lint3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,&
                            dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,knmz,isubx,jsuby,ksubz)
            else if (l==lsave+3) then
                ! update l-1 and interpolate l,l+1,l+2
                do iijjkk=1,mxmymz
                    ptm(iijjkk) = ptpp(iijjkk)
                end do
                call lint3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,dz,&
                            pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,knmz,isubx,jsuby,ksubz)
                call lint3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,&
                            dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,knmz,isubx,jsuby,ksubz)
                call lint3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,&
                            dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,knmz,isubx,jsuby,ksubz)
            else
                ! interpolate all four l-1,l,l+1,l+2
                call lint3u(nx,ny,nz,p(1,1,1,l-1),mx,my,mxmy,mz,ptm,intpol,kz,&
                            dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,knmz,isubx,jsuby,ksubz)
                call lint3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,&
                            dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,knmz,isubx,jsuby,ksubz)
                call lint3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,&
                            dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,knmz,isubx,jsuby,ksubz)
                call lint3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,&
                            dz,pk,pkp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                            inmx,jnmy,knmz,isubx,jsuby,ksubz)
            end if

            ! save l pointer for next pass

            lsave = l

            ! cubically interpolate q(ii,jj,kk,ll) from ptm,pt,ptp,ptpp in t direction

            do iijjkk=1,mxmymz
                q(iijjkk,ll) = dtm(ll)*ptm(iijjkk) + dt(ll)*pt(iijjkk) + dtp(ll)*ptp(iijjkk) + dtpp(ll)*ptpp(iijjkk)
            end do
        end do

    else

        ! cubic in z

        if (lsubt == 1) then

            ! mt grid is subset of nt grid

            do ll=1,mt
                l = lnmt*(ll-1)+1
                call cubt3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,q(1,ll),intpol,&
                    kz,dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,&
                    pjm,pj,pjp,pjpp,ix,dxm,dx,dxp,dxpp,&
                    inmx,jnmy,knmz,isubx,jsuby,ksubz)
            end do
            return
        end if
        lsave = -3
        do ll=1,mt
            l = lt(ll)
            if (l==lsave) then

                ! l pointer has not moved since last pass (no updates or interpolation)

            else if (l==lsave+1) then

                ! update l-1,l,l+1 and interpolate l+2

                do iijjkk=1,mxmymz
                    ptm(iijjkk) = pt(iijjkk)
                    pt(iijjkk) = ptp(iijjkk)
                    ptp(iijjkk) = ptpp(iijjkk)
                end do
                call cubt3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,&
                    dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp&
                    ,ix,dxm,dx,dxp,dxpp,&
                    inmx,jnmy,knmz,isubx,jsuby,ksubz)
            else if (l==lsave+2) then

                ! update l-1,l and interpolate l+1,l+2

                do iijjkk=1,mxmymz
                    ptm(iijjkk) = ptp(iijjkk)
                    pt(iijjkk) = ptpp(iijjkk)
                end do
                call cubt3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,&
                    dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp&
                    ,ix,dxm,dx,dxp,dxpp,&
                    inmx,jnmy,knmz,isubx,jsuby,ksubz)
                call cubt3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,&
                    dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp&
                    ,ix,dxm,dx,dxp,dxpp,&
                    inmx,jnmy,knmz,isubx,jsuby,ksubz)
            else if (l==lsave+3) then

                ! update l-1 and interpolate l,l+1,l+2

                do iijjkk=1,mxmymz
                    ptm(iijjkk) = ptpp(iijjkk)
                end do
                call cubt3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,&
                    dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp&
                    ,ix,dxm,dx,dxp,dxpp,&
                    inmx,jnmy,knmz,isubx,jsuby,ksubz)
                call cubt3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,&
                    dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp&
                    ,ix,dxm,dx,dxp,dxpp,&
                    inmx,jnmy,knmz,isubx,jsuby,ksubz)
                call cubt3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,&
                    dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp&
                    ,ix,dxm,dx,dxp,dxpp,&
                    inmx,jnmy,knmz,isubx,jsuby,ksubz)
            else

                ! interpolate all four l-1,l,l+1,l+2

                call cubt3u(nx,ny,nz,p(1,1,1,l-1),mx,my,mxmy,mz,ptm,intpol,kz,&
                    dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp&
                    ,ix,dxm,dx,dxp,dxpp,&
                    inmx,jnmy,knmz,isubx,jsuby,ksubz)
                call cubt3u(nx,ny,nz,p(1,1,1,l),mx,my,mxmy,mz,pt,intpol,kz,&
                    dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp&
                    ,ix,dxm,dx,dxp,dxpp,&
                    inmx,jnmy,knmz,isubx,jsuby,ksubz)
                call cubt3u(nx,ny,nz,p(1,1,1,l+1),mx,my,mxmy,mz,ptp,intpol,kz,&
                    dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp&
                    ,ix,dxm,dx,dxp,dxpp,&
                    inmx,jnmy,knmz,isubx,jsuby,ksubz)
                call cubt3u(nx,ny,nz,p(1,1,1,l+2),mx,my,mxmy,mz,ptpp,intpol,kz,&
                    dzm,dz,dzp,dzpp,pkm,pk,pkp,pkpp,jy,dym,dy,dyp,dypp,pjm,pj,pjp,pjpp&
                    ,ix,dxm,dx,dxp,dxpp,&
                    inmx,jnmy,knmz,isubx,jsuby,ksubz)
            end if

            ! save l pointer for next pass
            lsave = l

            ! cubically interpolate q(ii,jj,kk,ll) from ptm,pt,ptp,ptpp in t direction
            do iijjkk=1,mxmymz
                q(iijjkk,ll) = dtm(ll)*ptm(iijjkk) + dt(ll)*pt(iijjkk) + dtp(ll)*ptp(iijjkk) + dtpp(ll)*ptpp(iijjkk)
            end do
        end do

    end if

    end subroutine cubt4u
!**************************************************************************

!***************************************************************************************************
    end module regridpack_module
!***************************************************************************************************