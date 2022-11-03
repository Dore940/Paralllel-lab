include "./map.f90"
program test_map
use iso_fortran_env
use metafunction
implicit none
real, dimension(1:7) :: A, B

A = [2.2,4.5,6.7,4.3,9.1,1.3,0.0]
B = map(polinom, A)
print *, "Result:"
print *, polinom_elemental([2.2,4.5,6.7,4.3,9.1,1.3,0.0])
print *, "Result elemental:"
print *, B

end program test_map