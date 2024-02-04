module mod_flappy
    implicit none

    integer, parameter :: OBSTACLE_WIDTH = 30
    integer, parameter :: OBSTACLE_GAP = 30
    integer, parameter :: OBSTACLE_Y_OFFSET = 10
    integer, parameter :: FLAPPY_WIDTH = 27
    integer, parameter :: FLAPPY_HEIGHT = 12
    integer, parameter, dimension(2) :: RESOLUTION = (/200,50/)
    real, parameter :: GRAVITY = 1.0
    character(len=5) :: RED = achar(27) // "[41m"
    character(len=5) :: GREEN = achar(27) // "[42m"
    character(len=5) :: YELLOW = achar(27) // "[43m"
    character(len=5) :: WHITE = achar(27) // "[47m"
    character(len=5) :: BLACK = achar(27) // "[40m"
    character(len=5) :: BASE_COLOR = achar(27) // "[46m" ! CYAN
    character(len=5) :: BACKGROUND_COLOR = achar(27) // "[46m" ! CYAN
    ! character(len=1) :: COLOR_ESC = achar(27)


    type :: Gamestate
        integer :: points
        ! real :: angle
        real :: y_position 
        real :: y_velocity
        integer, allocatable, dimension(:) :: obstacle_xs, obstacle_ys

    contains
        procedure, public, pass(self) :: draw
        procedure, public, pass(self) :: progress
        procedure, public, pass(self) :: is_alive
    end type Gamestate


contains
    subroutine render_char_array(arr)
        character(len=1), dimension(:,:), intent(in) :: arr
        integer :: i, j
        ! clear console, would be nice to find a better way
        call system("clear")
        write(*, fmt="(A)", advance='no') BACKGROUND_COLOR
        ! print "pixel by pixel"
        do i=1, size(arr, 2)
            do j=1, size(arr, 1)
                SELECT CASE (arr(j, i))
                CASE ("@")
                    write(*, fmt="(A)", advance='no') BLACK // arr(j, i)
                CASE ("/", "|", "_")
                    write(*, fmt="(A)", advance='no') GREEN // arr(j, i)
                CASE (":")
                    write(*, fmt="(A)", advance='no') YELLOW // arr(j, i)
                CASE ("*")
                    write(*, fmt="(A)", advance='no') WHITE // arr(j, i)
                CASE ("=")
                    write(*, fmt="(A)", advance='no') RED // arr(j, i)
                CASE DEFAULT
                    write(*, fmt="(A)", advance='no') BASE_COLOR // arr(j, i)
             END SELECT
            end do
            write(*,*)
        end do
        write(*,*)

    end subroutine render_char_array



    subroutine draw(self)
        class(Gamestate), intent(in) :: self
        character(len=1), dimension(FLAPPY_WIDTH, FLAPPY_HEIGHT) :: flappy_str
        character(len=1), dimension(RESOLUTION(1), RESOLUTION(2)) :: canvas
        integer :: self_pos, i, j
        logical :: use_colors = .true.
        ! Had to break image on mutliple lines as Fortran linewidth would be exceeded otherwise
        if (use_colors) then
        flappy_str = reshape((/ '.','.','.','.','.','.','.','.','.','.', &
                                '@','@','@','@','@','@','@','@','@','@','.','.','.','.','.','.','.', &
                                '.','.','.','.','.','.','.','@','@',':', &
                                ':',':',':',':',':','@','*','*','*','*','@','.','.','.','.','.','.', &
                                '.','.','.','.','.','@',':',':',':',':', &
                                ':',':',':','@','*','*','*','*','*','*','*','*','@','.','.','.','.', &
                                '.','.','.','@','@',':',':',':',':',':', &
                                ':',':',':','@','*','*','*','*','*','*','@','*','*','@','.','.','.', &
                                '.','.','@',':',':',':',':',':',':',':', &
                                ':',':',':','@','*','*','*','*','*','*','@','*','*','@','.','.','.', &
                                '.','@','@','@','@','@','@','@','@','@', &
                                ':',':',':',':',':','@','*','*','*','*','*','*','*','@','.','.','.', &
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
        else
            flappy_str = reshape((/ '.','.','.','.','.','.','.','.','.','.', &
            '@','@','@','@','@','@','@','@','@','@','.','.','.','.','.','.','.', &
            '.','.','.','.','.','.','.','@','@','.', &
            '.','.','.','.','.','@','.','.','.','.','@','.','.','.','.','.','.', &
            '.','.','.','.','.','@','.','.','.','.', &
            '.','.','.','@','.','.','.','.','.','.','.','.','@','.','.','.','.', &
            '.','.','.','@','@','.','.','.','.','.', &
            '.','.','.','@','.','.','.','.','.','.','@','.','.','@','.','.','.', &
            '.','.','@','.','.','.','.','.','.','.', &
            '.','.','.','@','.','.','.','.','.','.','@','.','.','@','.','.','.', &
            '.','@','@','@','@','@','@','@','@','@', &
            '.','.','.','.','.','@','.','.','.','.','.','.','.','@','.','.','.', &
            '@','.','.','.','.','.','.','.','.','.', &
            '@','.','.','.','.','.','.','@','@','@','@','@','@','@','@','@','.', &
            '@','.','.','.','.','.','.','.','.','.', &
            '@','.','.','.','.','@','=','=','=','=','=','=','=','=','=','=','@', &
            '.','@','@','@','@','@','@','@','@','@', &
            '.','.','.','@','=','=','=','@','@','@','@','@','@','@','@','@','.', &
            '.','.','.','.','@','.','.','.','.','.', &
            '.','.','.','.','.','@','=','=','=','=','=','=','=','=','=','@','.', &
            '.','.','.','.','.','@','@','@','.','.', &
            '.','.','.','.','.','.','.','@','@','@','@','@','@','@','@','.','.', &
            '.','.','.','.','.','.','.','.','@','@', &
            '@','@','@','@','@','@','@','.','.','.','.','.','.','.','.','.','.'/), &
            shape(flappy_str), order=(/1,2/) )
        end if
        ! Initialize whole array to dots
        canvas = '.' 
        ! Top and bottom borders
        canvas(:, 1) = '/' 
        canvas(:, RESOLUTION(2)) = '/' 

        self_pos = nint(self % y_position)
        canvas(1:FLAPPY_WIDTH, self_pos: self_pos + FLAPPY_HEIGHT - 1) = flappy_str
    
        call render_char_array(canvas)

    end subroutine draw

    subroutine progress(self)
        class(Gamestate), intent(in out) :: self
        integer :: i
        self % y_velocity = self % y_velocity + GRAVITY
        self % y_position = self % y_position + self % y_velocity
        do i = 1, size(self % obstacle_xs)
            self % obstacle_xs(i) = self % obstacle_xs(i) - 1
        end do
        if (self % obstacle_xs(1) < 0) then
            self % obstacle_xs = self % obstacle_xs(2:)
        end if
    end subroutine progress

    pure logical function is_alive(self) 
        class(Gamestate), intent(in) :: self
        is_alive = .True.
    end function is_alive

    type(Gamestate) function init_gamestate() result(new_state)
        new_state % points = 0
        new_state % y_velocity = 0.0
        new_state % y_position = 0.2*RESOLUTION(2)
        new_state % obstacle_xs = (/ RESOLUTION(1) + OBSTACLE_WIDTH / 2 ,  RESOLUTION(1) + 3 * OBSTACLE_WIDTH / 2/)
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