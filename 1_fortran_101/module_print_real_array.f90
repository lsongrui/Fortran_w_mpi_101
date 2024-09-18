module print_real_array
    implicit none

    interface print_array
        module procedure print_1d_array, print_2d_array, print_3d_array
    end interface

contains
    subroutine print_1d_array(arr)
        real, intent(in) :: arr(:) ! Assumed-shape 1D array
        integer :: i

        write(*, "(A)", advance='no') '['
        do i = 1, size(arr)
            if (i < size(arr)) then
                write(*, "(F6.2, A)", advance='no') arr(i), ', '
            else
                write(*, "(F6.2)", advance='no') arr(i)
            end if
        end do
        print *, ']'''
    end subroutine print_1d_array

    subroutine print_2d_array(arr)
        real, intent(in) :: arr(:, :)
        integer :: i, j

        write(*, "(A)") '[' 
        do i = 1, size(arr, 1)
            write(*, "(A)", advance='no') ' [' 
            do j = 1, size(arr, 2)  
                if (j < size(arr, 2)) then
                    write(*, "(F6.2, A)", advance='no') arr(i, j), ', ' 
                else
                    write(*, "(F6.2)", advance='no') arr(i, j) 
                end if
            end do
            if (i < size(arr, 1)) then
                print *, '],' 
            else
                print *, ']' 
            end if
        end do
        write(*, "(A)") ']' 
    end subroutine print_2d_array

    subroutine print_3d_array(arr)
        real, intent(in) :: arr(:, :, :) 
        integer :: i, j, k

        write(*, "(A)") '['  
        do i = 1, size(arr, 1)  
            write(*, "(A)") ' ['  
            do j = 1, size(arr, 2)  
                write(*, "(A)", advance='no') '  ['  
                do k = 1, size(arr, 3)  
                    if (k < size(arr, 3)) then
                        write(*, "(F6.2, A)", advance='no') arr(i, j, k), ', '
                    else
                        write(*, "(F6.2)", advance='no') arr(i, j, k)
                    end if
                end do
                if (j < size(arr, 2)) then
                    print *, '], '  
                else
                    print *, ']'  
                end if
            end do
            if (i < size(arr, 1)) then
                print *, '],' 
            else
                print *, ']' 
            end if
        end do
        write(*, "(A)") '[' 
    end subroutine print_3d_array

end module print_real_array
