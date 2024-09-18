! mpif90 3_mpi_send_recieve.f90 -o 3_mpi_send_recieve
! mpirun -np 4 ./3_mpi_send_recieve

! process 1 to send out a message containing the integer 42 to process 2.

PROGRAM send_recv_mpi
    include 'mpif.h'
    
    integer process_Rank, size_Of_Cluster, ierror, message_Item
    
    call MPI_INIT(ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size_Of_Cluster, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, process_Rank, ierror)
    
    IF(process_Rank == 0) THEN
        message_Item = 42
        call MPI_SEND(message_Item, 1, MPI_INT, 1, 1, MPI_COMM_WORLD, ierror) ! built-in synchronization
        print *, "Sending message containing: ", message_Item
    ELSE IF(process_Rank == 1) THEN
        call MPI_RECV(message_Item, 1, MPI_INT, 0, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror) ! built-in synchronization
        print *, "Received message containing: ", message_Item
    END IF
    
    call MPI_FINALIZE(ierror)
END PROGRAM