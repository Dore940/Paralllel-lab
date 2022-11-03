include "../src/random.f90"

program monty_hall_paradox
  use random
  use iso_fortran_env
  use omp_lib
  implicit none

  integer(int64), parameter :: turns_num = 10**6
  integer(int64) :: turn
  integer :: N
  integer :: res
  integer :: num_ths ! количество потоков
  character(len=30) :: argv ! для принятия переменных
  logical :: does_he_change_his_strategy

  ! количество потоков получаем как аргумент командной строки
  if (command_argument_count() > 1) then
    call get_command_argument(1, argv)
    read(argv, *) num_ths
  else
  ! если ничего не передано, то 
    num_ths = omp_get_max_threads()
  end if
  
  N = 6

  ! Пункт 1. Первая стратегия заключается в том, что игрок никогда не меняет свое первое решение
  ! does_he_change_his_strategy = .false.

  ! Пункт 2. Вторая стратегия заключается в том, что игрок всегда меняет свое решение
   does_he_change_his_strategy = .true.

  res = 0
  !$omp parallel num_threads(num_ths)
    !$omp do reduction(+ : res)
    do turn = 1,turns_num,1
        res = res + monty(does_he_change_his_strategy)
    end do
    !$omp end do
  !$omp end parallel

   print "('Вероятность выигрыша: ', G0)", dble(res)/dble(turns_num)
end program monty_hall_paradox