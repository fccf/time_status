module time_status
  use,intrinsic :: iso_fortran_env, only: output_unit
  implicit none


  public :: now, s2hms, timer_t


  private

  type step_t
    private
    character(:),allocatable :: name
    logical :: is_tracking = .FALSE.
    real    :: time    = 0.0
    real    :: persent = 0.0
    integer :: ncalls  = 0
    integer :: counter = 0

    type(step_t), pointer :: parent => null()
    type(step_t), pointer :: child  => null()
    ! type(step_t), pointer :: prev   => null()
    type(step_t), pointer :: next   => null()
  end type step_t


  type timer_t
    private
    type(step_t), pointer :: root => null()
    type(step_t), pointer :: current => null()
  contains
    procedure :: start  => timer_start
    procedure :: stop   => timer_stop
    procedure :: report => timer_report
  end type timer_t



contains



  !_____________________________________________________________________________
  subroutine timer_start (this, name)
    !< start timer
    !___________________________________________________________________________
    class(timer_t), intent(inout) :: this
    character(*), intent(in) :: name

    integer :: counter

    call system_clock (counter)

    if (.not.associated(this%root)) then
      allocate(this%root)
      this%current => this%root
    end if

    this%current => new_step(this%current%child)
    this%current%counter = counter
    this%current%is_tracking = .TRUE.

  contains
    recursive function new_step (current) result (new)
      type(step_t), pointer :: current, new
      if (.not.associated(current)) then
        allocate(current)
        current%name = name
        current%ncalls = 1
        current%parent => this%current
        new => current
      else if (current%name == name) then
        current%ncalls = current%ncalls + 1
        new => current
      else
        new => new_step(current%next)
      end if
    end function new_step

  end subroutine timer_start



  !_____________________________________________________________________________
  subroutine timer_stop (this, name)
    !< stop timer
    !___________________________________________________________________________
    class(timer_t), intent(inout) :: this
    character(*), intent(in) :: name

    integer :: counter, count_rate
    real    :: time

    if (name /= this%current%name) then
      error stop 'current timer is ' // this%current%name // '; cannot stop ' // name
    end if

    call system_clock (counter, count_rate)

    time = (counter-this%current%counter)/real(count_rate)
    this%current%time = this%current%time + time
    this%current%is_tracking = .FALSE.

    this%current => this%current%parent

    if(.not.associated(this%current%parent)) then
      this%current%time = this%current%time + time
    endif

  end subroutine timer_stop



  !_____________________________________________________________________________
  subroutine timer_report (this, unit)
    !< report time status information
    !___________________________________________________________________________
    class(timer_t), intent(in) :: this
    integer, intent(in), optional :: unit
    type(step_t), pointer :: child

    integer :: lunit

    lunit = output_unit
    if(present(unit)) lunit = unit

    write(lunit,fmt='(a)') 'step, time, ncalls, persent'
    write(lunit,fmt='(a,es10.3,a)') 'root,',this%root%time, ', 1, 100.0%'
    child => this%root%child
    do while (associated(child))
      call write_step (child, '')
      child => child%next
    end do

  contains
    recursive subroutine write_step (step, pre)
      type(step_t), intent(inout) :: step
      character(*), intent(in)    :: pre
      type(step_t), pointer :: child

      character(:),allocatable :: pref

      step%persent = step%time/step%parent%time*100
      write(lunit,fmt='(a,es10.3,a,i0,a,f5.1,a)') pre//step%name//',', step%time,', ',step%ncalls,',',step%persent,'%'

      child => step%child
      do while (associated(child))
        pref = child%parent%name//'-'//pre
        call write_step (child, pref)
        child => child%next
      end do

    end subroutine write_step

  end subroutine timer_report



  !_____________________________________________________________________________
  function s2hms(sec) result(hms)
    !< return seconds to h:m:s string
    !___________________________________________________________________________
    real, intent(in) :: sec
    character(:), allocatable :: hms
    integer :: hi,mi,si
    character(3) :: hs,ms,ss

    si = nint(sec)
    hi = int(si/3600)
    si = si - 3600*hi
    mi = int(si/60)
    si = si - 60*mi

    write(hs,'(i0)') hi
    write(ms,'(i0)') mi
    write(ss,'(i0)') si
    hms = trim(adjustl(hs))//'h:'//trim(adjustl(ms))//'m:'//trim(adjustl(ss))//'s'

  end function s2hms



  !_____________________________________________________________________________
  function now()
    !< return time now as a string
    !___________________________________________________________________________
    character(:), allocatable :: now

    integer :: y,m,d,h,n,s,values(8)
    character(len=10) :: date
    character(len=8)  :: time

    call date_and_time ( values = values )

    y = values(1)
    m = values(2)
    d = values(3)
    h = values(5)
    n = values(6)
    s = values(7)

    write(date,'(i0,a,i0,a,i0)') y,'-',m,'-',d
    write(time,'(i0,a,i0,a,i0)') h,':',n,':',s

    now = trim(adjustl(date))//' '//trim(adjustl(time))

  end function now


end module time_status
