module ex3
    use omp_lib
    use iso_fortran_env
    implicit none

    public :: my_vector, my_scalar
    contains

    function my_vector(array) result(res)
        real(real32), dimension(1:), intent(in) :: array
        real(real32), dimension(1:size(array, 1)) :: res

        res = sin((log(array)**2 + tanh(array)**2) / (sin(array**2) + cos(array**2)))

    end function my_vector

    function my_scalar(scalar) result(res)
        real(real32), intent(in) :: scalar
        real(real32) :: res

        res = sin((log(scalar)**2 + tanh(scalar)**2) / (sin(scalar**2) + cos(scalar**2)))

    end function my_scalar
end module ex3