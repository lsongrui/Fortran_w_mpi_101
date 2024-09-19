! compile with
! mpif90  module_print_real_array.f90 utils.f90 4_fortran_wave_function_w_mpi.f90 -O3 -o 4_fortran_wave_function_w_mpi
! run with
! mpirun -np 4 ./4_fortran_wave_function_w_mpi     
! plot the result with
! gnuplot plot_script.gp
! result is in ./results/animation.gif

! It solves a 2d wave function
! equal-space orthogonal cartesian mesh
! central difference Finite Difference Method
! initial condition is picked from a textbook
! dirchlet boundary is adopted, all the u at the boundary is 0

program wave_simulation_mpi
    use mpi
    use print_real_array
    use utils
    implicit none
    integer :: rank, size_Of_Cluster, ierror, tag

    ! define paras
    integer, parameter :: nx = 200, ny = 200, nSteps = 1000      ! grid size, time steps
    real(8), parameter :: Dt = 1.0e-3, C = 15.0, w = 20.0       ! dt, Stiffness constant, Initial wave width
    real(8), parameter :: xmax = 1.0, ymax = 1.0                ! Domain size (-xmax<x<xmax, -ymax<y<ymax)

    ! define whole computational domain
    real(8) :: Dx, Dy, x(nx+1, ny+1), y(nx+1, ny+1), u(nx+1, ny+1), v(nx+1, ny+1)
    integer :: i, j, step

    ! define local grid vars
    integer :: n_y, start_y_index, end_y_index, start_gather_y_index, end_gather_y_index          ! n_y stands for local grid size, except for the last rank 
    real(8), allocatable :: local_u(:,:), local_v(:,:), local_x(:,:), local_y(:,:), local_r(:,:)
    integer, allocatable :: r_counts(:), displ(:)

    ! init
    Dx = 2.0 * xmax / nx        ! grid spacing
    Dy = 2.0 * ymax / ny

    ! simply split in x, maintain the full rank in y
    ! whole computational domain:                           
    !       rank 0                  rank 1                  rank 2                    rank {size}
    ! | +--+--+--+--+-    |    -+--+--+--+--+-    |    -+--+--+--+--+-     | ... |    -+--+--+--+-.-+ |      ! "+" stands for nodes
    !   1..2......n_y+1   rank*(n_y+1)+1..(rank+1)*(n_y+1)                      rank*(n_y+1)+1......ny+1     ! node index: i ∈ [start_y_index, end_y_index] [1, ny+1]
    ! -xmax..(i-1)*Dx-xmax (i-1)*Dx-xmax..(i-1)*Dx-xmax   ...                ... size*n_y*Dx-xmax..xmax      ! coordinate: local_x ∈ [-xmax, xmax]
    
    ! add halo nodes:                                       
    !       rank 0                  rank 1                  rank 2                    rank {size}
    ! | +--+--+--+--+--*- | -*--+--+--+--+--+--*- | -*--+--+--+--+--+--*-  | ... | -*--+--+--+--+-.-+ |      ! "*" stands for extra halo nodes
    !   1..2........n_y+2  rank*(n_y+1)...(rank+1)*(n_y+1)+1                    rank*(n_y+1)........ny+1       ! node index: i ∈ [start_y_index, end_y_index] [1, ny+1]
    
    ! exchange step 1:   left  -> right                    
    !       rank 0                  rank 1                  rank 2                    rank {size}
    ! | +--+--+--+--S--*- | -R--+--+--+--+--S--*- | -R--+--+--+--+--S--*-  | ... | -R--+--+--+--+-.-+ |      ! S stands for send, R stands for Recieve
    
    ! exchange step 2:   left <- right
    !       rank 0                  rank 1                  rank 2                    rank {size}
    ! | +--+--+--+--+--R- | -*--S--+--+--+--+--R- | -*--S--+--+--+--+--R-  | ... | -*--S--+--+--+-.-+ |

    !  local grid
    call MPI_INIT(ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size_Of_Cluster, ierror)
    allocate(r_counts(size_Of_Cluster), displ(size_Of_Cluster))

    n_y = ny/size_Of_Cluster-1
    
    if (rank==0) then
        start_y_index = rank*(n_y+1)+1
        end_y_index = (rank+1)*(n_y+1)+1
        start_gather_y_index = start_y_index    ! index for data gathering
        end_gather_y_index = end_y_index        ! index for data gathering
    else if (rank==(size_Of_Cluster-1)) then
        start_y_index = rank*(n_y+1)
        end_y_index = ny+1
        start_gather_y_index = start_y_index+2
        end_gather_y_index = end_y_index
    else
        start_y_index = rank*(n_y+1)
        end_y_index = (rank+1)*(n_y+1)+1
        start_gather_y_index = start_y_index+2
        end_gather_y_index = end_y_index
    end if

    ! if (rank==0) then; print *, 'n_y: ', n_y; end if
    ! call MPI_BARRIER( MPI_COMM_WORLD, ierror) ! Synchronization
    ! print *, 'Index: [', start_y_index, ',', end_y_index,']', end_y_index-start_y_index+1, 'nodes. cluster:', rank, 'of', size_Of_Cluster 

    allocate(local_u(nx+1, start_y_index:end_y_index), local_v(nx+1, start_y_index:end_y_index))
    allocate(local_x(nx+1, start_y_index:end_y_index), local_y(nx+1, start_y_index:end_y_index))
    allocate(local_r(nx+1, start_y_index:end_y_index))

    ! Initialize local_x, local_y, local_r arrays
    do j = start_y_index, end_y_index
        do i = 1, nx + 1
            local_x(i,j) = (i-1)*Dx-xmax
            local_y(i,j) = (j-1)*Dy-ymax
            local_r(i,j) = local_x(i,j)**2+local_y(i,j)**2
        end do
    end do

    ! Initialize local_u and local_v arrays to zero
    local_u = 0.0
    local_v = 0.0

    ! Set initial condition for local_u
    do j = start_y_index, end_y_index
        do i = 1, nx + 1
            if (local_r(i,j)<1.0) then
                local_u(i,j) = exp(-4.0*w**2 * local_r(i,j)**2) - 0.75*exp(-w**2 * local_r(i,j)**2)
                local_u(i,j) = local_u(i,j) + 0.75*exp(-w**2) - exp(max(-700.0, -4.0*w**2))
                local_u(i,j) = local_u(i,j) * (local_r(i,j)**4 - 1.0)
            end if
        end do
    end do

    ! call MPI_BARRIER( MPI_COMM_WORLD, ierror) ! Synchronization

    ! Collect data from all processes
    call MPI_ALLGATHER(size(local_u(:, start_gather_y_index: end_gather_y_index)),1,MPI_INT,r_counts,1,MPI_INT,MPI_COMM_WORLD,ierror)
    displ(1)=0; 
    do i = 2, size_Of_Cluster; displ(i) = displ(i-1)+r_counts(i-1); end do
    ! if (rank==3) then; print *, r_counts; end if
    ! if (rank==3) then; print *, displ; end if
    call MPI_GATHERV(local_x(:,start_gather_y_index:end_gather_y_index),size(local_x(:,start_gather_y_index:end_gather_y_index)),MPI_REAL8,x,r_counts,displ,MPI_REAL8,0,MPI_COMM_WORLD,ierror)
    call MPI_GATHERV(local_y(:,start_gather_y_index:end_gather_y_index),size(local_y(:,start_gather_y_index:end_gather_y_index)),MPI_REAL8,y,r_counts,displ,MPI_REAL8,0,MPI_COMM_WORLD,ierror)
    call MPI_GATHERV(local_u(:,start_gather_y_index:end_gather_y_index),size(local_u(:,start_gather_y_index:end_gather_y_index)),MPI_REAL8,u,r_counts,displ,MPI_REAL8,0,MPI_COMM_WORLD,ierror)
    ! if (rank==0) then; print *, "Gathered: "; call print_array(x); write(*,*); end if ! print array to test

    ! Loop over timesteps
    if (rank==0) then; print *, 'Simulation has kicked off!'; end if
    do step = 0, nSteps
        ! Update u array
        do j = start_y_index, end_y_index
            do i = 2, nx
                local_u(i,j) = local_u(i,j) + Dt * local_v(i,j)
            end do
        end do

        ! halo exchange u
        if (rank<size_Of_Cluster-1) then
            call MPI_SEND(local_u(:, end_y_index-1), nx+1, MPI_REAL8, rank+1,  nx+1, MPI_COMM_WORLD, ierror)
            call MPI_RECV(local_u(:, end_y_index), nx+1, MPI_REAL8, rank+1,  nx+1, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
        end if
        if (rank>0) then
            call MPI_RECV(local_u(:, start_y_index), nx+1, MPI_REAL8, rank-1,  nx+1, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
            call MPI_SEND(local_u(:, start_y_index+1), nx+1, MPI_REAL8, rank-1,  nx+1, MPI_COMM_WORLD, ierror)
        end if

        ! Update v array using finite differences
        do j = start_y_index, end_y_index
            do i = 2, nx
                local_v(i,j) = local_v(i,j) + (C * Dt / Dx**2) * (local_u(i-1,j) - 2.0*local_u(i,j) + local_u(i+1,j)) + (C * Dt / Dy**2) * (local_u(i,j-1) - 2.0*local_u(i,j) + local_u(i,j+1))
            end do
        end do
        ! Halo exchange v
        if (rank<size_Of_Cluster-1) then
            call MPI_SEND(local_v(:, end_y_index-1), nx+1, MPI_REAL8, rank+1,  nx+1, MPI_COMM_WORLD, ierror)
            call MPI_RECV(local_v(:, end_y_index), nx+1, MPI_REAL8, rank+1,  nx+1, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
        end if
        if (rank>0) then
            call MPI_RECV(local_v(:, start_y_index), nx+1, MPI_REAL8, rank-1,  nx+1, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
            call MPI_SEND(local_v(:, start_y_index+1), nx+1, MPI_REAL8, rank-1,  nx+1, MPI_COMM_WORLD, ierror)
        end if

        ! output
        if (mod(step,5) == 0) then
            call MPI_GATHERV(local_u(:,start_gather_y_index:end_gather_y_index),size(local_u(:,start_gather_y_index:end_gather_y_index)),MPI_REAL8,u,r_counts,displ,MPI_REAL8,0,MPI_COMM_WORLD,ierror)
            ! Call the plot_function with gnuplot
            if (rank==0) then; call write_data_to_file(x, y, u, nx, ny, step); end if
        end if

        if (rank==0) then; print '(a4, i5, a2,i5)', 'Step', step, '/', nSteps; end if
        ! if (rank==0) then; call prograss_bar(step, nSteps); end if        ! somehow not working properly...

    end do
    
    call MPI_FINALIZE(ierror)
end program wave_simulation_mpi