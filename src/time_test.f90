program time_test
  use time_status
  implicit none

  type(timer_t) :: tt

  print*, now()

  print*,s2hms(36009.7)

  call tt%start('A')
  call spend_time()
  call tt%stop('A')

  call tt%start('A')
  call spend_time()
  call tt%stop('A')

  call tt%start('B')
  call spend_time()
  call tt%stop('B')

  call tt%start('C')
  call spend_time()
  call tt%start('D')
  call spend_time()
  call tt%stop('D')
  call tt%stop('C')

  call tt%report()

contains
  subroutine spend_time()

    integer, parameter :: n = 1000
    real :: a(n,n), b(n,n),c(n,n)

    a = 1.
    b = 2.

    c= matmul(a,b)

  end subroutine spend_time


end program time_test
