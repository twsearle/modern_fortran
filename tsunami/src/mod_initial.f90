module mod_initial

    use iso_fortran_env, only: &
        int32, &
        real32 

    implicit none

    contains

    subroutine set_gaussian(x, icenter, decay)
        ! create Gaussian distribution

        integer(int32), intent(in)     :: icenter
        real(real32), intent(in)        :: decay
        real(real32), intent(inout)     :: x(:)

        integer(int32)                 :: i

        do i = 1, size(x)
            x(i) = exp(-decay * (i - icenter)**2)
        end do

    end subroutine set_gaussian
    
end module mod_initial
