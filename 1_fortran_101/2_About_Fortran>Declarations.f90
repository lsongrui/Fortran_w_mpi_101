! compile with
! gfortran 2_About_Fortran\>Declarations.f90 -o 2_About_Fortran\>Declarations
program Declarations
real             :: x, x2, y, ymax
integer          :: i, j, index
logical          :: b, b2
complex          :: z
character(len=8) :: name

real, parameter    :: clight = 3.e8
integer, parameter :: m      = 100

real, dimension(100)  :: xarray, yarray  ! the arrays start at index 1 and end at index 100.
real, dimension(m)    :: another_array
real, dimension(m,m)  :: array_in_2_dimensions  
real, dimension(-m:m,0:20) :: array_with_lower_bounds ! the arrays start at lower bound and end at higher bound.

real, dimension(3,2) :: a ! column-major order. 
! a(1,1), a(2,1), a(3,1), a(1,2), a(2,2), a(3,2)

x    = 5.0
y   =  1.0
x    = x + y
i = 5; j = 2; k = 3
b    = .true.
name = 'John Doe'
xarray(1) = x2
z = cmplx(y, 6.0) ! this is the most compatiable way to define a complex number in FORTRAN
end program Declarations