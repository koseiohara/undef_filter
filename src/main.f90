program main

    use namelist, only : read_nml, minuteTaker
    use undef_filter, only : undef_filter_apply

    implicit none

    real(4) :: begin_time
    real(4) :: end_time


    call cpu_time(begin_time)

    call read_nml()

    call minuteTaker()

    call undef_filter_apply()

    call cpu_time(end_time)

    write(*,'(A,F0.3,A)') 'Elapsed Time : ', end_time-begin_time, 's'
   

end program main


