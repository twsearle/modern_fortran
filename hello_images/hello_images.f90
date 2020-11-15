program hello_images

!print *, "i am image ", this_image(), "of", num_images()
print *, " image ", this_image(), "has tile ", tile_images(9)
print *, " image ", this_image(), "has neighbours", tile_neighbours()

contains

pure function tile_images(num_els) result(limits)
    integer, intent(in)  :: num_els
    integer              :: limits(2) 

    integer              :: width, remainder, remaining
    
    width = num_els / num_images()
    remainder = mod(num_els, num_images())

    if (remainder > 0) then
        if (this_image() <= remainder) then
            limits(1) = (width+1)*(this_image() - 1) + 1
            limits(2) = (width+1)*this_image() 
        else 
            limits(1) = width*(this_image() - 1) + 1 + remainder
            limits(2) = width*this_image() + remainder
        end if
    else
        limits(1) = width*(this_image() - 1) + 1
        limits(2) = width*this_image() 
    end if

end function tile_images

pure function tile_neighbours() result(neighbours)
    integer              :: neighbours(2) 

    if (num_images() == 1) then
        neighbours(1) = 1
        neighbours(2) = 1
    else
        if (this_image() == 1) then
            neighbours(1) = num_images()
            neighbours(2) = this_image() + 1
        else if (this_image() == num_images()) then
            neighbours(1) = this_image() - 1
            neighbours(2) = 1
        else
            neighbours(1) = this_image() - 1
            neighbours(2) = this_image() + 1
        end if
    end if
end function tile_neighbours

end program hello_images
