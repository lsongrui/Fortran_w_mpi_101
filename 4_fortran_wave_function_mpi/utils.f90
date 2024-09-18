module utils
    implicit none
contains

subroutine prograss_bar(step, nSteps)
    implicit none
    integer, intent(in) :: step, nSteps
    character(len=100) :: command
    integer :: status
    write(command, '(" sh progress_bar.sh ", I5, " ", I5)') step, nSteps
    status = SYSTEM(command)
    call flush(unit=6)
end subroutine prograss_bar

subroutine write_data_to_file(x, y, u, nx, ny, step)
    implicit none
    integer, intent(in) :: nx, ny, step
    real(8), intent(in) :: x(nx+1, ny+1), y(nx+1, ny+1), u(nx+1, ny+1)
    integer :: i, j
    character(len=100) :: filename
    integer :: unit
    ! write to a file
    write(filename, '("results/data_step_", I4.4, ".dat")') step
    open(unit=unit, file=filename, status='replace', action='write')
    do j = 1, ny
        do i = 1, nx
            write(unit, '(F10.5, F10.5, F10.5)') x(i,j), y(i,j), u(i,j)
        end do
        write(unit, *) ! one extra line suitable for pm3d
    end do
    close(unit)
end subroutine write_data_to_file

end module utils