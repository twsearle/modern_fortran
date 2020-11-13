program tsunami

    use iso_fortran_env, only: &
        int32, &
        real32 

    use mod_initial, only : &
        set_gaussian 

    use mod_diff, only : &
        diff => diff_centred

    implicit none

    integer(int32), parameter :: grid_size = 100
    integer(int32), parameter :: num_time_steps = 5000
    real(real32), parameter   :: dt = .02
    real(real32), parameter   :: dx = 1.
    real(real32), parameter   :: g = 9.8
    real(real32), parameter   :: hmean = 10

    integer(int32), parameter :: icenter = 25
    real(real32), parameter   :: decay = 0.02

    integer(int32) :: i, n
    real(real32)   :: h(grid_size), u(grid_size)

    if (grid_size <= 0) stop 'grid_size must be > 0'
    if (dt <= 0) stop 'time step dt must be > 0'
    if (dx <= 0) stop 'grid spacing dx must be > 0'

    call set_gaussian(h, icenter, decay)
    u = 0

    print *, 0, h

    time_loop: do n = 1, num_time_steps

        ! Update velocity
        u = u - (u * diff(u) + g * diff(h)) / dx * dt
        ! update shallow water eqns height
        h = h - diff(u * (hmean + h)) / dx * dt
        
        if (this_image() == 1) then
            print *, n, h
        end if

    end do time_loop

    contains

    real elemental function cold_front_temperature( &
        temp1, temp2, c, dx, dt) result(ans)
        ! cold front temperature

        real(real32), intent(in) :: temp1, temp2, c, dx, dt
        ans = temp2 - c *(temp2 - temp1) / dx * dt

    end function cold_front_temperature

end program tsunami

