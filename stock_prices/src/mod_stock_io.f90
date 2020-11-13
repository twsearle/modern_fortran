module mod_stock_io

use iso_fortran_env, only: real32

use mod_utils, only : alloc_1d, free

implicit none

private
public :: read_stock

contains

integer function num_records(filename)
    ! Return the number of records (lines) of a text file.
    character(len=*), intent(in) :: filename
    integer :: fileunit
    open(newunit=fileunit, file=filename)
    num_records = 0
    do
      read(unit=fileunit, fmt=*, end=1)
      num_records = num_records + 1
    end do
    1 continue
    close(unit=fileunit)
end function num_records


subroutine read_stock(filename, time, open, high, low,&
                      close, adjclose, volume)
    character(*)                              :: filename
    character(:), allocatable, intent(in out) :: time(:)
    real(real32), allocatable, intent(in out) :: open(:), &
                                                 high(:), &
                                                 low(:), &
                                                 close(:), &
                                                 adjclose(:), &
                                                 volume(:)

    integer             :: fileunit
    integer             :: n, length

    length = num_records(filename) - 1

    if (allocated(time)) deallocate(time)
    allocate(character(10) :: time(length))
    call alloc_1d(open, length)
    call alloc_1d(high, length)
    call alloc_1d(low, length)
    call alloc_1d(close, length)
    call alloc_1d(adjclose, length)
    call alloc_1d(volume, length)

    open(newunit=fileunit, file=filename)
    read(fileunit, fmt=*, end=1)
    do n = 1, length
        read(fileunit, fmt=*, end=1) time(n), open(n), high(n), &
                                     low(n), close(n), adjclose(n), &
                                     volume(n)
    end do
    1 close(fileunit)
end subroutine read_stock

end module mod_stock_io