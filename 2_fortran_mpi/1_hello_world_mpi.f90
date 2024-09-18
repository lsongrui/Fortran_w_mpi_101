! mpif90 1_hello_world_mpi.f90 -o 1_hello_world_mpi
! mpirun -np 4 ./1_hello_world_mpi

PROGRAM hello_world_mpi
    use mpi
    
    integer process_Rank, size_Of_Cluster, ierror, tag
    
    call MPI_INIT(ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size_Of_Cluster, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, process_Rank, ierror)
    
    print *, 'Hello World from process: ', process_Rank, 'of ', size_Of_Cluster
    
    call MPI_FINALIZE(ierror)
END PROGRAM