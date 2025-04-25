type(Circle) :: circ
type(Rectange) :: rect

circ%radius = 5.0
rect%width = 4.0
rect%height = 6.0

call print_area(circ) ! Calls Circle's compute_area
call print_area(rect) ! Calls Rectangle's compute_area

