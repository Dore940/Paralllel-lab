include "../src/random.f90"

program birthday_paradox
  use random
  use iso_fortran_env
  use omp_lib
  implicit none

  integer(int64), parameter :: turns_num = 10**5
  integer(int64) :: turn
  integer :: N ! количество студентов в группе
  integer :: res
  integer :: num_ths ! количество потоков
  character(len=30) :: argv ! для принятия переменных
  double precision, dimension(2:100) :: final_res ! массив для хранения значений вероятности с соответствующим число студентов группы

  ! количество потоков получаем как аргумент командной строки
  if (command_argument_count() > 1) then
    call get_command_argument(1, argv)
    read(argv, *) num_ths
  else
  ! если ничего не передано, то 
    num_ths = omp_get_max_threads()
  end if
  
  !N = 23
  !$omp parallel num_threads(num_ths)
    !$omp do reduction(+ : res)
    do N = 2,100,1
        res = 0
        do turn = 1,turns_num,1
            res = res + birthday(N)
        end do
        !print "(I0, ',', G0)", N, dble(res) / dble(turns_num)
        final_res(N) = dble(res) / dble(turns_num)
    end do
    !$omp end do
  !$omp end parallel

    ! для рисования:
    do N = 2,100,1
        print "(I0, ',', G0)", N, final_res(N)
    end do
  !print "('# Вероятность одинаковых дней рождений: ', G0)", dble(res)/dble(turns_num)

end program birthday_paradox