module namelist

    use globals, only : kp, nx, ny, nz, irec_init, varnum, tnum, &
                      & input_undef, output_undef, ifile, ofile

    implicit none

    private
    public :: read_nml, minuteTaker

    contains


    subroutine read_nml()
        integer, parameter :: nml_unit = 5

        namelist / grid   / nx, ny, nz
        namelist / undef  / input_undef, output_undef
        namelist / files  / ifile, ofile
        namelist / record / irec_init, varnum, tnum


        nx = 0
        ny = 0
        nz = 0

        input_undef  = 9.99E20_kp
        output_undef = 9.99E20_kp

        ifile = ''
        ofile = ''

        irec_init = 0
        varnum    = 0
        tnum      = 0

        read(nml_unit, nml=grid  )
        read(nml_unit, nml=undef )
        read(nml_unit, nml=files )
        read(nml_unit, nml=record)

        call checker()

    end subroutine read_nml


    subroutine checker()

        if (nx <= 0) then
            write(*,'(A)')    '---'
            write(*,'(A)')    'ERROR STOP'
            write(*,'(A,I0)') 'Invalid NX value. NX=', nx
            ERROR STOP
        endif

        if (ny <= 0) then
            write(*,'(A)')    '---'
            write(*,'(A)')    'ERROR STOP'
            write(*,'(A,I0)') 'Invalid NY value. NY=', ny
            ERROR STOP
        endif

        if (nz <= 0) then
            write(*,'(A)')    '---'
            write(*,'(A)')    'ERROR STOP'
            write(*,'(A,I0)') 'Invalid NZ value. NZ=', nz
            ERROR STOP
        endif

        if (input_undef == output_undef) then
            write(*,'(A)')        '---'
            write(*,'(A)')        'WARNING'
            write(*,'(A,ES0.5)') 'input_undef and output_undef have the same value. Both are ', input_undef
            write(*,'(A)')        '---'
        endif

        if (trim(ifile) == '') then
            write(*,'(A)') '---'
            write(*,'(A)') 'ERROR STOP'
            write(*,'(A)') 'Invalid input file name : ' // trim(ifile)
            ERROR STOP
        endif

        if (trim(ofile) == '') then
            write(*,'(A)') '---'
            write(*,'(A)') 'ERROR STOP'
            write(*,'(A)') 'Invalid output file name : ' // trim(ofile)
            ERROR STOP
        endif

        if (irec_init <= 0) then
            write(*,'(A)')    '---'
            write(*,'(A)')    'ERROR STOP'
            write(*,'(A,I0)') 'Invalid irec_init value. irec_init=', irec_init
            ERROR STOP
        endif

        if (varnum <= 0) then
            write(*,'(A)')    '---'
            write(*,'(A)')    'ERROR STOP'
            write(*,'(A,I0)') 'Invalid varnum value. varnum=', varnum
            ERROR STOP
        endif

        if (tnum <= 0) then
            write(*,'(A)')    '---'
            write(*,'(A)')    'ERROR STOP'
            write(*,'(A,I0)') 'Invalid tnum value. tnum=', tnum
            ERROR STOP
        endif

    end subroutine checker


    subroutine minuteTaker()
        
        write(*,'(A)')       '---'
        write(*,'(A)')       'FILE NAME'
        write(*,'(A)')       '  INPUT  : ' // trim(ifile)
        write(*,'(A)')       '  OUTPUT : ' // trim(ofile)
        write(*,*)
        write(*,'(A)')       'GRID SIZE'
        write(*,'(A,I0)')    '  NX : ', nx
        write(*,'(A,I0)')    '  NY : ', ny
        write(*,'(A,I0)')    '  NZ : ', nz
        write(*,*)
        write(*,'(A)')       'RECORD INFO'
        write(*,'(A,I0)')    '  INITIAL              : ', irec_init
        write(*,'(A,I0)')    '  NUMBER OF VARIABLES  : ', varnum
        write(*,'(A,I0)')    '  NUMBER OF TIME STEPS : ', tnum
        write(*,*)
        write(*,'(A)')       'UNDEF VALUE'
        write(*,'(A,ES0.5)') '  ORIGINAL : ', input_undef
        write(*,'(A,ES0.5)') '  NEW      : ', output_undef
        write(*,'(A)')       '---'

    end subroutine minuteTaker
    

end module namelist

