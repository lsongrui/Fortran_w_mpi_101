! mpif90 module_print_real_array.f90 4_mpi_scatter_gather.f90 -o  4_mpi_scatter_gather
! mpirun -np 4 ./ 4_mpi_scatter_gather

! PROGRAM scatter_mpi
!     include 'mpif.h'
    
!     integer rank, size_Of_Cluster, ierror, message_Item
!     integer scattered_Data
!     integer, dimension(4) :: distro_Array
!     distro_Array = (/39, 72, 129, 42/)
    
!     call MPI_INIT(ierror)
!     call MPI_COMM_SIZE(MPI_COMM_WORLD, size_Of_Cluster, ierror)
!     call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
!     call MPI_Scatter(distro_Array, 1, MPI_REAL16, scattered_Data, 1, MPI_REAL16, 0, MPI_COMM_WORLD, ierror);
    
!     print *, "Process ", rank, "received: ", scattered_Data
!     call MPI_FINALIZE(ierror)
    
! END PROGRAM

! subroutine gatherr1d(r,rgather)
! real(kind=r8) :: r(:),rgather(:)
! integer :: ierror
! call mpi_gather(r,size(r),mpir8,rgather,size(r),mpir8,0, &
!     mpi_comm_world,ierror)
! return
! end subroutine gatherr1d 

PROGRAM gather_mpi
    use print_real_array
    use mpi
    
    integer rank, size_Of_Cluster, ierror, message_Item
    real(8), dimension(4,6) :: data
    real(8), dimension(4,17) :: gather_array
    integer :: i
    
    call MPI_INIT(ierror)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, size_Of_Cluster, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)

    do j=1,6
        do i = 1,4
            data(i,j) = rank*16+(j-1)*4+i
        end do
    end do

    print *, "Process ", rank, "send: "
    call print_array(data)

    call MPI_Barrier(MPI_COMM_WORLD,ierror)

    if (rank == 0) then
        call mpi_gather(data(:,1:5),size(data(:,1:5)),MPI_REAL8,gather_array,size(data(:,1:5)),MPI_REAL8,0, MPI_COMM_WORLD,ierror)
    else
        call mpi_gather(data(:,1:4),size(data(:,1:4)),MPI_REAL8,gather_array,size(data(:,1:4)),MPI_REAL8,0, MPI_COMM_WORLD,ierror)
    end if

    ! call MPI_Scatter(distro_Array, 1, MPI_REAL16, scattered_Data, 1, MPI_REAL16, 0, MPI_COMM_WORLD, ierror);
    
    ! print *, "Process ", rank, "send: ", gather_array
    call MPI_FINALIZE(ierror)

    if (rank==0) then
        print *, "Gathered: "
        call print_array(gather_array)
        write(*,*)
    end if
    
END PROGRAM