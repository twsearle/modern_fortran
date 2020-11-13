program stock_gain

    use iso_fortran_env, only:&
        real32

    use mod_stock_io, only: &
        read_stock

    use mod_utils, only: &
        reverse

    implicit none

    character(len=4), allocatable :: symbols(:)
    character(len=:), allocatable :: time(:)
    real(real32), allocatable     :: open(:), &
                                     high(:), &
                                     low(:), &
                                     close(:), &
                                     adjclose(:), &
                                     volume(:)
    integer :: n
    real(real32) :: gain

    symbols = ['AAPL', 'AMZN', 'CRAY', 'CSCO', 'HPQ ', &
               'IBM ', 'INTC', 'MSFT', 'NVDA', 'ORCL']

do n =1, size(symbols)
    print *, "Working on ", symbols(n)

    call read_stock("/Users/toby/repos/mf_book_stock_prices/data/" &
                    //trim(symbols(n))//".csv", &
                    time, open, high, low, close, adjclose, volume)

    adjclose = reverse(adjclose)
    gain = (adjclose(size(adjclose))) - adjclose(1)

    print *, symbols(n), gain, nint (gain / adjclose(1) * 100)
end do

end program stock_gain