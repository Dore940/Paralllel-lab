include "../src/random.f90"

program rand_number
  use random
  use iso_fortran_env
  use omp_lib
  implicit none
  ! include "omp_lib.h"

  integer(int64), parameter :: turns_num = 10**6
  integer(int64) :: turn
  integer :: N, count1, count2, count3, count4, count5, count6
  integer :: res
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
  
  N = 6
  ! Пункт 1. Возвращение рандомного целого числа от 1 до N с помощью функции randomint()
  ! res = randomint(N)
  ! print "('Случайное целое число от 1 до ', I0, ' равно ', I0)", N, res

  ! Пункт 2 - 3. Подстчет вероятностей выпадения определнной стороны шестигранного кубика с помощью циклов
  count1 = 0
  count2 = 0
  count3 = 0
  count4 = 0
  count5 = 0
  count6 = 0

!   do turn = 1,turns_num,1
!     res = randomint(N)
!     if (res == 1) then
!         count1 = count1 + 1
!     else if (res == 2) then
!         count2 = count2 + 1
!     else if (res == 3) then
!         count3 = count3 + 1
!     else if (res == 4) then
!         count4 = count4 + 1
!     else if (res == 5) then
!         count5 = count5 + 1
!     else
!         count6 = count6 + 1
!     end if
!   end do

  ! Пункт 4. Подстчет вероятностей выпадения определнной стороны шестигранного кубика с помощью распараллеливония
  !$omp parallel num_threads(num_ths)
    !$omp do reduction(+ : count1, count2, count3, count4, count5, count6)
    do turn = 1,turns_num,1
        res = randomint(N)
        if (res == 1) then
            count1 = count1 + 1
        else if (res == 2) then
            count2 = count2 + 1
        else if (res == 3) then
            count3 = count3 + 1
        else if (res == 4) then
            count4 = count4 + 1
        else if (res == 5) then
            count5 = count5 + 1
        else
            count6 = count6 + 1
        end if
    end do
    !$omp end do
  !$omp end parallel

  print "('Вероятноятность выпадения 1 равна ', G0)", dble(count1)/dble(turns_num)
  print "('Вероятноятность выпадения 2 равна ', G0)", dble(count2)/dble(turns_num)
  print "('Вероятноятность выпадения 3 равна ', G0)", dble(count3)/dble(turns_num)
  print "('Вероятноятность выпадения 4 равна ', G0)", dble(count4)/dble(turns_num)
  print "('Вероятноятность выпадения 5 равна ', G0)", dble(count5)/dble(turns_num)
  print "('Вероятноятность выпадения 6 равна ', G0)", dble(count6)/dble(turns_num)
  
end program rand_number