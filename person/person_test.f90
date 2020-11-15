module mod_person

implicit none

type :: Person
    character(len=20), private :: name
    integer	      :: age
    character(len=20) :: occupation
    character(len=100):: greeting_message
    contains
    procedure, pass(self) :: get_name
    procedure, pass(self) :: set_name
    procedure, pass(self) :: greet
end type Person

interface Person
    module procedure :: person_constructor
end interface Person

contains 

pure type(Person) function person_constructor( &
    name, age, occupation) result(obj)
    character(len=*), intent(in) :: name
    integer, intent(in)          :: age
    character(len=*), intent(in) :: occupation

    obj % name = name
    obj % age = age
    obj % occupation = occupation
    if (occupation == "Pirate") then
	obj % greeting_message = "Ahoy, matey!"
    else
	obj % greeting_message = "Hi, there."
    end if

end function person_constructor

pure function get_name(self) result(name) 
    class(Person), intent(in) :: self
    character(len=20)	      :: name

    name = self % name
end function get_name

subroutine set_name(self, name)
    class(Person), intent(inout) :: self
    character(len=*), intent(in) :: name
    self % name = name
end subroutine set_name

impure elemental subroutine greet(self)
    class(Person), intent(in) :: self
    print *, "Hello my name is " // trim(self % name)
end subroutine greet

end module mod_person

program person_test

    use mod_person, only: Person

    implicit none

    type(Person) :: toby
    type(Person) :: Alfred
    type(Person) :: people(2)

    toby = Person("Toby", 31, "Pirate")
    Alfred = Person("Alfred", 68, "Butler")

    print '(a)', trim(toby % get_name()) // " says " // toby % greeting_message
    print '(a,I2,a)', trim(toby % get_name()) // " says he is a ", &
	  toby % age, " year old " // trim(toby % occupation)
    print '(a)', trim(Alfred % get_name()) // " says " // Alfred % greeting_message
    print '(a,I2,a)', trim(Alfred % get_name()) // " says he is a ", &
	  alfred % age, " year old " // trim(alfred % occupation)

    print '(a)', trim(toby % get_name()) // " changes his name "
    call toby % set_name("Oscar")
    print '(a)', "He is now called " // trim(toby % get_name())

    people = [toby, Alfred]
    call people % greet

end program person_test
