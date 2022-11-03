program ex_1
    implicit none
    include "omp_lib.h"
    
    double precision :: time1, time2, new_time
    integer :: n 

    open(5, file="out.txt", action="write")
    do n=1, 10**6, 1
        time1 = omp_get_wtime()
        call wait()
        time2 = omp_get_wtime()
        new_time = time2-time1
        write(5,*) new_time
    end do
    close(5, status = "keep")
end program ex_1

subroutine wait()
    implicit none
    integer :: i
    real(kind=8) :: temp, any1, any2, any3

    do i = 1, 10**6, 1
        temp = 2.3
        any1 = (cosh(temp)*sinh(temp*i)+ log(temp))/3
        any2 = sinh(any1*i)
        any3 = any1/any2
    end do
end subroutine wait 