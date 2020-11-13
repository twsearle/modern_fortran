module mod_utils

use iso_fortran_env, only : real32

implicit none

private
public :: alloc_1d, &
          free,     &
          reverse,  &
          average,  &
          std

contains 

subroutine alloc_1d(arr, size)
    real(real32), allocatable, intent(inout) :: arr(:)
    integer, intent(in)                      :: size

    if (allocated(arr)) then
        deallocate(arr)
    end if
    allocate(arr(size))

end subroutine alloc_1d

subroutine free(arr)
    real(real32), allocatable, intent(inout) :: arr(:)

    if (allocated(arr)) then
        deallocate(arr)
    end if
end subroutine free

function reverse(arr) result(res)
    real(real32), intent(in) :: arr(:)
    real(real32) :: res(size(arr))
    res = arr(size(arr):1:-1)
end function reverse

pure real function average(arr)
    real(real32), intent(in) :: arr(:)
    average = sum(arr) / size(arr)
end function average

pure real function std(arr)
    real(real32), intent(in) :: arr(:)
    std = sqrt(average((average(arr) - arr)**2))
end function std

pure function moving_average(arr, window) result(mv_avg)
    real(real32), intent(in)  :: arr(:)
    integer, intent(in)       :: window
    real(real32)              :: mv_avg(size(arr))

    integer :: i

    do i=1,size(arr)
        mv_avg(i) = average(arr(max(i-window,1):i))
    end do

end function moving_average

pure function moving_std(arr, window) result(mv_std)
    real(real32), intent(in)  :: arr(:)
    integer, intent(in)       :: window
    real(real32)              :: mv_std(size(arr))

    integer :: i

    do i=1,size(arr)
        mv_std(i) = std(arr(max(i-window,1):i))
    end do

end function moving_std

end module mod_utils