! compile with
! gfortran 3_InputOutput\>print.f90 -o 3_InputOutput\>print  
program PrintPractice
character(len=8) :: name = 'Karl'
integer          :: iage = 25
print *, 'My name is ', name            ! auto formaat
print '(a10, i2)', 'My age is ', iage    ! format, refer to 
! https://cvw.cac.cornell.edu/fortran-intro/input-output/print
end program PrintPractice