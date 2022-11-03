program ex_2_1
    implicit none
    include "omp_lib.h"

!$omp parallel
    write(*, *) "Номер потока:", omp_get_thread_num()
    !$omp single
        write(*, *) "Всего потоков ", omp_get_num_threads()
    !$omp end single
!$omp end parallel

end program ex_2_1
