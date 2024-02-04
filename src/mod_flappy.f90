module mod_flappy
    implicit none

    integer, parameter :: OBSTACLE_WIDTH = 20
    integer, parameter :: OBSTACLE_GAP = 30
    integer, parameter :: OBSTACLE_Y_OFFSET = 15
    integer, parameter :: OBSTACLE_X_DIST = 50
    integer, parameter :: FLAPPY_WIDTH = 27
    integer, parameter :: FLAPPY_HEIGHT = 12
    integer, parameter, dimension(2) :: RESOLUTION = (/200,50/)
    integer, parameter :: OBSTACLE_SPEED = 2
    real, parameter :: GRAVITY = 0.4
    real, parameter :: JUMP_SPEED = 6.0
    character(len=5) :: RED = achar(27) // "[41m"
    character(len=5) :: GREEN = achar(27) // "[42m"
    character(len=5) :: YELLOW = achar(27) // "[43m"
    character(len=5) :: WHITE = achar(27) // "[47m"
    character(len=5) :: BLACK = achar(27) // "[40m"
    character(len=5) :: BASE_COLOR = achar(27) // "[46m" ! CYAN
    ! character(len=5) :: BACKGROUND_COLOR = achar(27) // "[46m" ! CYAN
    ! character(len=1) :: COLOR_ESC = achar(27)


    type :: Gamestate
        integer :: points
        ! real :: angle
        real :: y_position 
        real :: y_velocity
        integer, allocatable, dimension(:) :: obstacle_xs, obstacle_ys
        logical :: is_alive

    contains
        procedure, public, pass(self) :: draw
        procedure, public, pass(self) :: progress
    end type Gamestate


contains
    subroutine render_char_array(arr, use_colors)
        character(len=1), dimension(:,:), intent(in) :: arr
        logical, intent(in) :: use_colors
        integer :: i, j
        character :: c
        ! clear console, would be nice to find a better way
        call system("clear")
        ! print "pixel by pixel"
        do i=1, size(arr, 2)
            do j=1, size(arr, 1)
                if (use_colors) then
                    c = '.'! arr(j, i)
                    SELECT CASE (arr(j, i))
                        CASE ("@")
                            write(*, fmt="(A)", advance='no') BLACK // c
                        CASE ("/", "|", "-")
                            write(*, fmt="(A)", advance='no') GREEN // c
                        CASE (":")
                            write(*, fmt="(A)", advance='no') YELLOW // c
                        CASE ("_")
                            write(*, fmt="(A)", advance='no') WHITE // c
                        CASE ("=")
                            write(*, fmt="(A)", advance='no') RED // c
                        CASE ('.')
                            write(*, fmt="(A)", advance='no') BASE_COLOR // c
                        CASE DEFAULT
                            write(*, fmt="(A)", advance='no') BASE_COLOR // arr(j, i)
                    END SELECT
                else
                    write(*, fmt="(A)", advance='no') arr(j, i)
                end if
            end do
            write(*,*)
        end do
        write(*,*)

    end subroutine render_char_array


    subroutine draw(self)
        class(Gamestate), intent(in out) :: self
        character(len=1), dimension(FLAPPY_WIDTH, FLAPPY_HEIGHT) :: flappy_str
        character(len=1), dimension(RESOLUTION(1), RESOLUTION(2)) :: screen
        character(len=3) :: points_str
        integer :: self_pos, i, j, obs_i, obs_x_lim, flappy_render_height
        logical :: use_colors = .true.
        ! Had to break image on mutliple lines as Fortran linewidth would be exceeded otherwise
        flappy_str = reshape((/ '.','.','.','.','.','.','.','.','.','.', &
                                '@','@','@','@','@','@','@','@','@','@','.','.','.','.','.','.','.', &
                                '.','.','.','.','.','.','.','@','@',':', &
                                ':',':',':',':',':','@','_','_','_','_','@','.','.','.','.','.','.', &
                                '.','.','.','.','.','@',':',':',':',':', &
                                ':',':',':','@','_','_','_','_','_','_','_','_','@','.','.','.','.', &
                                '.','.','.','@','@',':',':',':',':',':', &
                                ':',':',':','@','_','_','_','_','_','_','@','_','_','@','.','.','.', &
                                '.','.','@',':',':',':',':',':',':',':', &
                                ':',':',':','@','_','_','_','_','_','_','@','_','_','@','.','.','.', &
                                '.','@','@','@','@','@','@','@','@','@', &
                                ':',':',':',':',':','@','_','_','_','_','_','_','_','@','.','.','.', &
                                '@',':',':',':',':',':',':',':',':',':', &
                                '@',':',':',':',':',':',':','@','@','@','@','@','@','@','@','@','.', &
                                '@',':',':',':',':',':',':',':',':',':', &
                                '@',':',':',':',':','@','=','=','=','=','=','=','=','=','=','=','@', &
                                '.','@','@','@','@','@','@','@','@','@', &
                                ':',':',':','@','=','=','=','@','@','@','@','@','@','@','@','@','.', &
                                '.','.','.','.','@',':',':',':',':',':', &
                                ':',':',':',':',':','@','=','=','=','=','=','=','=','=','=','@','.', &
                                '.','.','.','.','.','@','@','@',':',':', &
                                ':',':',':',':',':',':',':','@','@','@','@','@','@','@','@','.','.', &
                                '.','.','.','.','.','.','.','.','@','@', &
                                '@','@','@','@','@','@','@','.','.','.','.','.','.','.','.','.','.'/), &
                        shape(flappy_str), order=(/1,2/) )

        ! Initialize whole array to dots
        screen = '.' 
        ! Top and bottom borders
        screen(:, 1) = '/' 
        screen(:, RESOLUTION(2)) = '/' 

        ! bird
        self_pos = nint(self % y_position)
        if (self_pos < 0 .or. (self_pos + FLAPPY_HEIGHT) > RESOLUTION(2)) then
            self % is_alive = .false.
        end if

        flappy_render_height = FLAPPY_HEIGHT - max(self_pos + FLAPPY_HEIGHT - RESOLUTION(2), 0)
        screen(1:FLAPPY_WIDTH, self_pos: self_pos + flappy_render_height - 1) = flappy_str(:,1:flappy_render_height)
        
        ! pipes
        do obs_i = 1, size(self % obstacle_xs)
            if (self % obstacle_xs(obs_i) < RESOLUTION(1)) then
                obs_x_lim = min(self % obstacle_xs(obs_i) + OBSTACLE_WIDTH, RESOLUTION(1))
                ! Check if any of the pipe locations contain "@"
                if(any(screen(self % obstacle_xs(obs_i):obs_x_lim, &
                    self % obstacle_ys(obs_i) + OBSTACLE_GAP/2:RESOLUTION(2)) == '@') &
                   .or. any(screen(self % obstacle_xs(obs_i):obs_x_lim, &
                   1:self % obstacle_ys(obs_i) - OBSTACLE_GAP/2) == '@')) then
                    self % is_alive = .false.
                end if 
                screen(self % obstacle_xs(obs_i):obs_x_lim, self % obstacle_ys(obs_i) + OBSTACLE_GAP/2:RESOLUTION(2)) = '|'
                screen(self % obstacle_xs(obs_i):obs_x_lim, 1:self % obstacle_ys(obs_i) - OBSTACLE_GAP/2) = '|'
            end if
        end do

        ! points counter, TODO multipixel numbers
        screen(RESOLUTION(1)/2-2:RESOLUTION(1)/2+2, 5:7) = '='
        write (points_str , "(I3)") self % points
        screen(RESOLUTION(1)/2-1:RESOLUTION(1)/2+1, 6) = [points_str(1:1),points_str(2:2),points_str(3:3)]

        call render_char_array(screen, use_colors)

    end subroutine draw

    subroutine progress(self)
        class(Gamestate), intent(in out) :: self
        integer :: i
        self % y_velocity = self % y_velocity + GRAVITY
        self % y_position = self % y_position + self % y_velocity
        do i = 1, size(self % obstacle_xs)
            self % obstacle_xs(i) = self % obstacle_xs(i) - OBSTACLE_SPEED
        end do
        if (self % obstacle_xs(1) < 0) then
            self % obstacle_xs = self % obstacle_xs(2:)
            self % obstacle_ys = self % obstacle_ys(2:)
            self % points = self % points + 1
        end if

        if (self % obstacle_xs(size(self % obstacle_xs)) < RESOLUTION(1)) then
            self % obstacle_xs = [self % obstacle_xs , RESOLUTION(1) + OBSTACLE_X_DIST]
            self % obstacle_ys = [self % obstacle_ys , obstacle_y_location()]
        end if
    end subroutine progress

    type(Gamestate) function init_gamestate() result(new_state)
        new_state % is_alive = .true.
        new_state % points = 0
        new_state % y_velocity = 0.0
        new_state % y_position = 0.2*RESOLUTION(2)
        new_state % obstacle_xs = (/ RESOLUTION(1),  RESOLUTION(1) + OBSTACLE_X_DIST/)
        new_state % obstacle_ys = (/ obstacle_y_location() ,  obstacle_y_location() /)
    end function init_gamestate

    integer function obstacle_y_location()
        obstacle_y_location = randint(OBSTACLE_Y_OFFSET, RESOLUTION(2) - OBSTACLE_Y_OFFSET)
    end function obstacle_y_location

    integer function randint(n, m)
        integer, intent(in) :: n, m
        real :: u
        call random_number(u)
        randint = n + floor((m+1-n)*u)
    end function randint

end module mod_flappy