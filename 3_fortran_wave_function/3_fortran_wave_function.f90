! compile with
! gfortran 3_fortran_wave_function.f90 utils.f90 -o 3_fortran_wave_function
! run with
! ./3_fortran_wave_function  
! plot the result with
! gnuplot plot_script.gp
! result is in ./results/animation.gif

! It solves a 2d wave function
! equal-space orthogonal cartesian mesh
! central difference Finite Difference Method
! initial condition is picked from a textbook
! dirchlet boundary is adopted, all the u at the boundary is 0

program wave_simulation
    use utils
    ! use print_real_array
    implicit none

    ! define paras
    integer, parameter :: nx = 200, ny = 200, nSteps = 1000     ! grid size, time steps
    real(8), parameter :: Dt = 1.0e-3, C = 15.0, w = 20.0       ! dt, Stiffness constant, Initial wave width
    real(8), parameter :: xmax = 1.0, ymax = 1.0                ! Domain size (-xmax<x<xmax, -ymax<y<ymax)

    ! define
    real(8) :: Dx, Dy, r(nx+1, ny+1), x(nx+1, ny+1), y(nx+1, ny+1)
    real(8) :: u(nx+1, ny+1), v(nx+1, ny+1)
    integer :: i, j, step

    ! Calculate grid spacing
    Dx = 2.0 * xmax / nx
    Dy = 2.0 * ymax / ny

    ! Initialize x, y, r arrays
    do j = 1, ny + 1
        do i = 1, nx + 1
            x(i,j) = -xmax + (i-1) * Dx
            y(i,j) = -ymax + (j-1) * Dy
            r(i,j) = x(i,j)**2 + y(i,j)**2
        end do
    end do

    ! Initialize u and v arrays to zero
    u = 0.0
    v = 0.0

    ! Set initial condition for u
    do j = 1, ny + 1
        do i = 1, nx + 1
            if (r(i,j) < 1.0) then
                u(i,j) = exp(-4.0*w**2 * r(i,j)**2) - 0.75*exp(-w**2 * r(i,j)**2)
                u(i,j) = u(i,j) + 0.75*exp(max(-700.0, -w**2)) - exp(max(-700.0, -4.0*w**2))
                u(i,j) = u(i,j) * (r(i,j)**4 - 1.0)
            end if
        end do
    end do

    ! Loop over timesteps
    print *, 'Simulation has kicked off!'
    do step = 0, nSteps
        ! Update u array
        do j = 2, ny
            do i = 2, nx
                u(i,j) = u(i,j) + Dt * v(i,j)
            end do
        end do

        ! Update v array using finite differences
        do j = 2, ny
            do i = 2, nx
                v(i,j) = v(i,j) + (C * Dt / Dx**2) * (u(i-1,j) - 2.0*u(i,j) + u(i+1,j)) &
                               + (C * Dt / Dy**2) * (u(i,j-1) - 2.0*u(i,j) + u(i,j+1))
            end do
        end do

        ! output
        if (mod(step,5) == 0) then
            ! Call the plot_function with gnuplot
            call write_data_to_file(x, y, u, nx, ny, step)
        end if
        call prograss_bar(step, nSteps)

    end do
end program wave_simulation