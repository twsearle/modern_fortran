program tsunami

    use iso_fortran_env, only: &
        int32, &
        real32 

    use mod_initial, only : &
        set_gaussian 

    use mod_diff, only : &
        diff => diff_centred

    use mod_tiles, only : &
        tile_images, &
        tile_neighbours

    implicit none

    integer(int32), parameter :: grid_size = 100
    integer(int32), parameter :: num_time_steps = 5000
    real(real32), parameter   :: dt = .02
    real(real32), parameter   :: dx = 1.
    real(real32), parameter   :: g = 9.8

    integer(int32), parameter :: icenter = 25
    real(real32), parameter   :: decay = 0.02

    integer(int32)              :: i, n
    integer(int32)              :: neighbours(2)
    integer(int32)              :: left, right
    integer(int32)              :: indices(2)
    integer(int32)              :: is, ie
    integer(int32)              :: ils, ile
    integer(int32)              :: ims, ime
    integer(int32)              :: tile_size
    real(real32), allocatable   :: h(:)[:], u(:)[:]
    real(real32), allocatable   :: gather(:)[:]
    real(real32), allocatable   :: hmean(:)

    if (grid_size <= 0) stop 'grid_size must be > 0'
    if (dt <= 0) stop 'time step dt must be > 0'
    if (dx <= 0) stop 'grid spacing dx must be > 0'

    neighbours = tile_neighbours()
    left = neighbours(1)
    right = neighbours(2)

    indices = tile_images(grid_size)
    is = indices(1)
    ie = indices(2)
    sync all

    tile_size = ie - is + 1
    ils = 1
    ile = tile_size
    ims = ils - 1
    ime = ile + 1

    allocate(hmean(ims:ime))
    hmean = 10
    allocate(u(ims:ime)[*])
    u = 0
    allocate(h(ims:ime)[*])
    h(ims:ime) = 0

    allocate(gather(grid_size)[*])
    if (this_image() == 1) then
        call set_gaussian(gather, icenter, decay)
    end if
    h(ils:ile) = gather(is:ie)[1]
    sync all
    h(ime)[left] = h(ils)
    h(ims)[right] = h(ile)

    sync all
    if (this_image() == 1) then
        print *, 0, gather
    end if

    time_loop: do n = 1, num_time_steps

        h(ime)[left] = h(ils)
        h(ims)[right] = h(ile)
        sync all

        ! Update velocity
        u = u - (u * diff(u) + g * diff(h)) / dx * dt

        sync all

        u(ime)[left] = u(ils)
        u(ims)[right] = u(ile)
        sync all

        ! update shallow water eqns height
        h = h - diff(u * (hmean + h)) / dx * dt
        
        gather(is:ie)[1] = h(ils:ile)
        sync all
        if (this_image() == 1) then
            print *, n, gather 
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

