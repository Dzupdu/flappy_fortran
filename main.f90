module c_interface
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int, c_int32_t
    implicit none

    interface
        integer(c_int) function c_get_key() bind(c)
            import :: c_int
        end function c_get_key
    end interface

    interface
    ! Source for this interface:
    ! https://cyber.dabamos.de/programming/modernfortran/sleep.html
        function c_usleep(useconds) bind(c, name='usleep')
            import :: c_int, c_int32_t
            implicit none
            integer(kind=c_int32_t), value :: useconds
            integer(kind=c_int)            :: c_usleep
        end function c_usleep
    end interface
end module c_interface


program flappy_fortran
    use mod_flappy
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int
    use c_interface
    implicit none

    type(Gamestate) :: game
    real, parameter :: target_fps = 10
    integer :: rc, fd = 5
    character :: c
    integer(c_int) :: key
    ! real :: x_pixels_per_s = 20

    game = init_gamestate()
    !print *, nint(1e6/target_fps)

    do while (game % is_alive())
        ! print * , "from: ", game % y_position, game % y_velocity
        call  game % progress()
        ! print * , "to ", game % y_position, game % y_velocity
        call game % draw()

        key = c_get_key()
        if (key /= 0) then
            ! print * , "addding speeed"
            ! print * , "from: ", game % y_velocity
            game % y_velocity = game % y_velocity - 10.0
            ! print * , "To ", game % y_velocity
        end if
         !call fgetc(fd, c, rc)
         !call fput(c)
        ! Call sleep(1/target_fps)
        ! print *, RESOLUTION(1), RESOLUTION(2)
        rc = c_usleep(nint(1e6/target_fps))
    end do

    ! READ *, 
end program flappy_fortran
