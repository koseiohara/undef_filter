module undef_filter

    use fileio , only : finfo, fopen, fclose, fwrite, fread
    use globals, only : kp, nx, ny, nz, irec_init, varnum, tnum, &
                      & input_undef, output_undef, ifile, ofile

    implicit none

    private
    public :: undef_filter_apply

    contains


    subroutine undef_filter_apply()
        real(kp) :: replacer(nx,ny,nz)

        type(finfo) :: input_file
        type(finfo) :: output_file

        integer :: t


        call fopen(ftype  =input_file , &  !! OUT
                 & fname  =ifile      , &  !! IN
                 & action ='READ'     , &  !! IN
                 & recl   =kp*nx*ny*nz, &  !! IN
                 & record =irec_init  , &  !! IN
                 & recstep=varnum       )  !! IN

        call fopen(ftype  =output_file, &  !! OUT
                 & fname  =ofile      , &  !! IN
                 & action ='WRITE'    , &  !! IN
                 & recl   =kp*nx*ny*nz, &  !! IN
                 & record =1          , &  !! IN
                 & recstep=1            )  !! IN

        do t = 1, tnum
            call fread(input_file                    , &  !! INOUT
                     & replacer(1:nx,1:ny,1:nz)        )  !! OUT

            call undef_replace(replacer(1:nx,1:ny,1:nz))  !! INOUT

            call fwrite(output_file                  , &  !! INOUT
                      & replacer(1:nx,1:ny,1:nz)       )  !! IN
        enddo

        call fclose(input_file)   !! INOUT
        call fclose(output_file)  !! INOUT

    end subroutine undef_filter_apply


    subroutine undef_replace(replacer)
        real(kp), intent(inout) :: replacer(nx,ny,nz)

        !Assign a new marker to grids with undef-value
        where (replacer == input_undef)
            replacer = output_undef
        endwhere

    end subroutine undef_replace


end module undef_filter

