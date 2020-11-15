module mod_diff

    use iso_fortran_env, only: &
        int32, &
        real32 

    implicit none

    private
    public :: diff_centred

    contains

    pure function diff(x) result(dx)
        ! calculate differences

        real(real32), intent(in) :: x(:)
        real(real32)             :: dx(size(x))
        integer(int32)           :: n

        n = size(x)

        dx(1) = x(1) - x(n)
        dx(2:n) = x(2:n) - x(1:n-1)

    end function diff

    pure function diff_centred(x) result(dx)
	! warning only calculated for interior points,
	! use halo-swap for halo points
        real(real32), intent(in) :: x(:)
        real(real32)             :: dx(size(x))
        integer(int32)           :: n

        n = size(x)
	dx = 0
        dx(2:n-1) = x(3:n) - x(1:n-2)
        dx = 0.5 * dx
    end function diff_centred

    pure function diff_centred_periodic(x) result(dx)
        real(real32), intent(in) :: x(:)
        real(real32)             :: dx(size(x))
        integer(int32)           :: n

        n = size(x)
	dx(1) = x(2) - x(n)
	dx(n) = x(1) - x(n-1)
        dx(2:n-1) = x(3:n) - x(1:n-2)
        dx = 0.5 * dx
    end function diff_centred_periodic
end module mod_diff
