include "../src/random.f90"
include "../src/stats.f90"

program player_ruin
  use random
  use iso_fortran_env
  use omp_lib
  use stats
  implicit none
  ! include "omp_lib.h"

  integer(int64), parameter :: turns_num = 10**6
  integer(int64) :: turn
  integer :: OR(1:2)
  integer :: S, N, count_total, tmp, count_positive
  real(real64) :: avg, var, m2
  integer :: num_ths ! количество потоков
  character(len=30) :: argv ! для принятия переменных

  ! количество потоков получаем как аргумент командной строки
  if (command_argument_count() > 1) then
    call get_command_argument(1, argv)
    read(argv, *) num_ths
  else
  ! если ничего не передано, то 
    num_ths = omp_get_max_threads()
  end if
  
  avg = 0
  count_total = 0
  count_positive = 0
  S = 6 ! начальное число денег у игрока
  N = 10 ! всего денег в системе
  OR = [-1, 1] ! первое число - проигрыш, второе число - выигрыш

do turn = 1,turns_num,1
    do while (S > 0 .and. S < N)
        tmp = OR(randomint(2))
        if (tmp == 1) then
            count_positive = count_positive + 1
        end if
        S = S + tmp
        count_total = count_total + 1
    end do
    call online_variance(turn, dble(count_total), avg, m2, var)
end do

  !print "('Частота побед игры: ', G0)", dble(count_positive) / dble(count_total)
  !print "('Частота проигрышей игры: ', G0)", 1.0 - dble(count_positive) / dble(count_total)
  print "('Среднее число раундов: ', G0)", avg

end program player_ruin