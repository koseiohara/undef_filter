module globals

    implicit none

    
    integer, parameter :: kp = 4

    integer, save :: nx
    integer, save :: ny
    integer, save :: nz

    integer, save :: irec_init
    integer, save :: varnum
    integer, save :: tnum

    real(kp), save :: input_undef
    real(kp), save :: output_undef

    character(128), save :: ifile
    character(128), save :: ofile


end module globals

