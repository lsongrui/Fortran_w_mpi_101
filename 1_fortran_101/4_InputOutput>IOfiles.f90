! compile with
! gfortran 4_InputOutput\>IOfiles.f90 -o 4_InputOutput\>IOfiles   
program PrintFiles
    character(len=8) :: name = 'Karl'
    integer :: iage = 25
    open (unit=10, file='output.txt')
    write (10,'(a,a8)') 'My name is ', name
    write (10,'(a,i4)') 'My age is ',  iage
    close (10)    
end program PrintFiles