! compile with
! gfortran module_print_real_array.f90 5_array_and_print.f90 -o 5_array_and_print

program generatic_print_array
    use print_real_array
    implicit none

    real :: array_1d(5) = [1.0, 2.0, 3.0, 4.0, 5.0]
    real :: array_2d(2, 3) = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], shape=[2, 3])
    real :: array_3d(2, 2, 2) = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0], shape=[2, 2, 2])
    real :: array_reshape(6,1) = reshape([1.0, 2.0, 3.0, 4.0, 5.0,6.0], shape=[6,1])

    call print_array(array_1d)
    write(*,*)
    call print_array(array_2d)
    write(*,*)
    call print_array(array_3d)
    write(*,*)
    call print_array(array_reshape)

end program generatic_print_array
