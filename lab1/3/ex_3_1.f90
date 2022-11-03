include "ex_3.f90"

program ex_3_1
use ex_3
real(kind=4), dimension(1:10**6) :: my_array, res_array
real(kind=8) :: time1, time2
integer :: i

call random_number(my_array)


time1 = omp_get_wtime()
res_array = my_vector(my_array)
time2 = omp_get_wtime()
print *, "Обработка целого вектора заняла", time2 - time1, " секунд"



time1 = omp_get_wtime()
do i = 1, size(my_array, 1), 1
    res_array(i) = my_function_scalar(my_array(i))
end do
time2 = omp_get_wtime()
print *, "Per element processing took ", time2 - time1, " seconds"


end program ex_3_1
