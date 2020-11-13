module mod_utils

use iso_fortran_env, only : real32

implicit none

private
public :: alloc_1d, free

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

end module mod_utils