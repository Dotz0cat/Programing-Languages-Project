type(Circle) :: circ

circ%radius = 5.0

call circ%compute_area()
print *, 'Area = ', circ%area ! Output: 78.53975

