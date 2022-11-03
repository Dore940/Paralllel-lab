program ex_1
    implicit none
    include "omp_lib.h"
    
    real(kind=8) :: time1, time2
    integer :: n 

    do n=1, 10, 1
        time1 = omp_get_wtime()
        call wait()
        time2 = omp_get_wtime()
        write(*, *) "Время", time2-time1, "сек. "
    end do
end program ex_1

subroutine wait()
    implicit none
    call sleep(1)
end subroutine wait 