module metafunction
    use iso_fortran_env
         implicit none
         abstract interface
         pure function f(x)
            real, intent(in) :: x 
            real :: f
         end function f
        end interface
        
        public :: polinom, polinom_elemental, map
     contains
     
     pure function polinom(x)
        implicit none
        real,intent(in) :: x
        real :: polinom
        polinom = 2*x**3 + 3*x**2 - 4*10
     end function polinom
     
     elemental function polinom_elemental(x)
        implicit none
        real,intent(in) :: x
        real :: polinom_elemental
        polinom_elemental = 2*x**3 + 3*x**2 - 4*10
     end function polinom_elemental
     
    pure function map(pol, A)
        real,dimension(1:),intent(in) :: A
        procedure(f) :: pol
        real,dimension(1:size(A,1)) :: map
        integer(int64):: i
            do i=1,size(A,1),1 
            map(i) = pol(A(i))
            end do
    end function map
    end module metafunction
     