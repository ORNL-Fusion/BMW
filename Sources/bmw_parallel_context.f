!*******************************************************************************
!>  @file bmw_parallel_context.f
!>  @brief Contains module @ref bmw_parallel_context.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref bmw_parallel_context_class. This
!>  contains the state variables needed by BMW for parallel computation.
!*******************************************************************************
      MODULE bmw_parallel_context
      USE mpi_inc
      USE profiler

      IMPLICIT NONE

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) bmw parallel context base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a bmw parallel context. This contains all memory
!>  needed parameters needed to parallel processing.
!-------------------------------------------------------------------------------
      TYPE :: bmw_parallel_context_class
!>  Problem space offset.
         INTEGER :: offset
!>  Problem space stride.
         INTEGER :: stride

!>  Number of threads.
         INTEGER :: num_threads

#if defined (MPI_OPT)
!>  MPI communicator reference for computation. BMW can be used as an embeded
!>  library. Multiple instances can be running in parallel on separate problems
!>  so do not assume the world comm.
         INTEGER :: comm = MPI_COMM_NULL
!>  Flag to mark if bmw intialized the MPI context.
         LOGICAL :: initialized_mpi
#endif
      CONTAINS
         PROCEDURE, PASS ::                                                    &
     &      set_threads => bmw_parallel_context_set_threads
         PROCEDURE, PASS :: report => bmw_parallel_context_report
         PROCEDURE, PASS :: reduce1 => bmw_parallel_context_reduce1
         PROCEDURE, PASS :: reduce2 => bmw_parallel_context_reduce2
         PROCEDURE, PASS :: reduce3 => bmw_parallel_context_reduce3
         PROCEDURE, PASS :: reduce4 => bmw_parallel_context_reduce4
         GENERIC         :: reduce => reduce1, reduce2, reduce3, reduce4
         PROCEDURE, PASS :: start => bmw_parallel_context_start
         PROCEDURE, PASS :: end => bmw_parallel_context_end
         FINAL           :: bmw_parallel_context_destruct
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for the bmw_parallel_context constructor.
!-------------------------------------------------------------------------------
      INTERFACE bmw_parallel_context_class
         MODULE PROCEDURE bmw_parallel_context_construct
      END INTERFACE

      CONTAINS

!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref bmw_context_class object.
!>
!>  Allocates memory and initializes a @ref bmw_parallel_context_class object.
!>
!>  @param[in] comm        MPI communicator. Only used if MPI support has been
!>                         compiled in.
!>  @returns A pointer to a constructed @ref bmw_parallel_context_class object.
!-------------------------------------------------------------------------------
      FUNCTION bmw_parallel_context_construct(                                 &
#if defined (MPI_OPT)
     &                                        comm                             &
#endif
     &                                       )

      IMPLICIT NONE

!  Declare Arguments
      CLASS (bmw_parallel_context_class), POINTER ::                           &
     &   bmw_parallel_context_construct
#if defined (MPI_OPT)
      INTEGER, INTENT(in)                        :: comm
#endif

!  local variables
#if defined (MPI_OPT)
      LOGICAL                                    :: isinit
      INTEGER                                    :: status
#endif
      REAL (rprec)                               :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(bmw_parallel_context_construct)

#if defined (MPI_OPT)
      CALL MPI_INITIALIZED(isinit, status)

      bmw_parallel_context_construct%initialized_mpi = .false.
      IF (.not.isinit) THEN
         bmw_parallel_context_construct%initialized_mpi = .true.
         CALL MPI_INIT(status)
      END IF

      bmw_parallel_context_construct%comm = comm
      CALL MPI_COMM_RANK(bmw_parallel_context_construct%comm,                  &
     &                   bmw_parallel_context_construct%offset,                &
     &                   status)
      CALL MPI_COMM_SIZE(bmw_parallel_context_construct%comm,                  &
     &                   bmw_parallel_context_construct%stride,                &
     &                   status)

#else
      bmw_parallel_context_construct%offset = 0
      bmw_parallel_context_construct%stride = 1
#endif

!  Configure the number of threads to use.
      bmw_parallel_context_construct%num_threads = 1

      CALL profiler_set_stop_time('bmw_parallel_context_construct',            &
     &                            start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref bmw_parallel_context_class object.
!>
!>  Deallocates memory and uninitializes a @ref bmw_parallel_context_class
!>  object.
!>
!>  @param[inout] this     A @ref bmw_parallel_context_class instance.
!>  @param[in]    finalize Flag to call MPI_FINALIZE. Only used if MPI support
!>                         has been compiled in.
!-------------------------------------------------------------------------------
      SUBROUTINE bmw_parallel_context_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (bmw_parallel_context_class), INTENT(inout) :: this

!  local variables
#if defined (MPI_OPT)
      INTEGER                                          :: status

!  Start of executable code
      IF (this%initialized_mpi) THEN
         CALL MPI_FINALIZE(status)
      END IF
#endif

      END SUBROUTINE

!*******************************************************************************
!  SETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Set the number of threads.
!>
!>  Sets the number of OpenMP threads to use.
!>
!>  @param[in] this        A @ref bmw_parallel_context_class instance.
!>  @param[in] num_threads Number of threads to use.
!-------------------------------------------------------------------------------
      SUBROUTINE bmw_parallel_context_set_threads(this, num_threads)
!$    USE omp_lib

      IMPLICIT NONE

!  Declare Arguments
      CLASS (bmw_parallel_context_class), INTENT(inout) :: this
      INTEGER, INTENT(in)                               :: num_threads

!  local variables
      REAL (rprec)                                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Configure the number of threads to use.
      this%num_threads = num_threads

!$    IF (this%num_threads .gt. 0) THEN
!$       CALL OMP_SET_NUM_THREADS(this%num_threads)
!$    END IF
!$OMP PARALLEL
!$    this%num_threads = OMP_GET_MAX_THREADS()
!$OMP END PARALLEL

      CALL profiler_set_stop_time('bmw_parallel_context_set_threads',          &
     &                            start_time)

      END SUBROUTINE

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Abort the entire program.
!>
!>  Aborts everything.
!>
!>  @param[in] status Error number to abort with.
!-------------------------------------------------------------------------------
      SUBROUTINE bmw_parallel_context_abort(status)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER, INTENT(in) :: status

!  local variables
      INTEGER             :: error

!  Start of executable code
#if defined (MPI_OPT)
      CALL MPI_ABORT(MPI_COMM_WORLD, status, error)
#else
      CALL EXIT(status)
#endif

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Report the number of parallel processes and threads.
!>
!>  Provides a summary report if the number of threads and processes currently
!>  configured.
!>
!>  @param[in] this    A @ref bmw_parallel_context_class instance.
!>  @param[in] io_unit Unit number to write messages to.
!-------------------------------------------------------------------------------
      SUBROUTINE bmw_parallel_context_report(this, io_unit)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (bmw_parallel_context_class), INTENT(inout) :: this
      INTEGER, INTENT(in)                               :: io_unit

!  local variables
      REAL (rprec)                                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (this%offset .eq. 0) THEN
#if defined(MPI_OPT)
         WRITE (io_unit,1000) this%stride
#endif
         WRITE (io_unit,1001) this%num_threads
      END IF

      CALL profiler_set_stop_time('bmw_parallel_context_report',               &
     &                            start_time)

1000  FORMAT('Using ',i4,' processes.')
#if defined (MPI_OPT)
1001  FORMAT('Using ',i4,' threads per process.')
#else
1001  FORMAT('Using ',i4,' threads.')
#endif

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Reduce parallel buffers.
!>
!>  Reduce a buffer from all parallel processes. In the single processes case,
!>  this reduces to nothing. This reduces a 1D buffer.
!>
!>  @param[in]    this   A @ref bmw_parallel_context_class instance.
!>  @param[inout] buffer Buffer to reduce.
!-------------------------------------------------------------------------------
      SUBROUTINE bmw_parallel_context_reduce1(this, buffer)
      USE stel_kinds, ONLY: rprec

      IMPLICIT NONE

!  Declare Arguments
      CLASS (bmw_parallel_context_class), INTENT(in) :: this
      REAL (rprec), DIMENSION(:), INTENT(inout)      :: buffer

!  local variables
      REAL (rprec)                                   :: start_time
      INTEGER                                        :: status

!  Start of executable code
      start_time = profiler_get_start_time()

#if defined (MPI_OPT)
      CALL MPI_ALLREDUCE(MPI_IN_PLACE, buffer, SIZE(buffer), MPI_REAL8,        &
     &                   MPI_SUM, this%comm, status)
#endif

      CALL profiler_set_stop_time('bmw_parallel_context_reduce1',              &
     &                            start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Reduce parallel buffers.
!>
!>  Reduce a buffer from all parallel processes. In the single processes case,
!>  this reduces to nothing. This reduces a 2D buffer.
!>
!>  @param[in]    this    A @ref bmw_parallel_context_class instance.
!>  @param[inout] buffer Buffer to reduce.
!-------------------------------------------------------------------------------
      SUBROUTINE bmw_parallel_context_reduce2(this, buffer)
      USE stel_kinds, ONLY: rprec

      IMPLICIT NONE

!  Declare Arguments
      CLASS (bmw_parallel_context_class), INTENT(in) :: this
      REAL (rprec), DIMENSION(:,:), INTENT(inout)    :: buffer

!  local variables
      REAL (rprec)                                   :: start_time
      INTEGER                                        :: status

!  Start of executable code
      start_time = profiler_get_start_time()

#if defined (MPI_OPT)
      CALL MPI_ALLREDUCE(MPI_IN_PLACE, buffer, SIZE(buffer), MPI_REAL8,        &
     &                   MPI_SUM, this%comm, status)
#endif

      CALL profiler_set_stop_time('bmw_parallel_context_reduce2',              &
     &                            start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Reduce parallel buffers.
!>
!>  Reduce a buffer from all parallel processes. In the single processes case,
!>  this reduces to nothing. This reduces a 3D buffer.
!>
!>  @param[in]    this   A @ref bmw_parallel_context_class instance.
!>  @param[inout] buffer Buffer to reduce.
!-------------------------------------------------------------------------------
      SUBROUTINE bmw_parallel_context_reduce3(this, buffer)
      USE stel_kinds, ONLY: rprec

      IMPLICIT NONE

!  Declare Arguments
      CLASS (bmw_parallel_context_class), INTENT(in) :: this
      REAL (rprec), DIMENSION(:,:,:), INTENT(inout)  :: buffer

!  local variables
      REAL (rprec)                                   :: start_time
      INTEGER                                        :: status

!  Start of executable code
      start_time = profiler_get_start_time()

#if defined (MPI_OPT)
      CALL MPI_ALLREDUCE(MPI_IN_PLACE, buffer, SIZE(buffer), MPI_REAL8,        &
     &                   MPI_SUM, this%comm, status)
#endif

      CALL profiler_set_stop_time('bmw_parallel_context_reduce3',              &
     &                            start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Reduce parallel buffers.
!>
!>  Reduce a buffer from all parallel processes. In the single processes case,
!>  this reduces to nothing. This reduces a 4D buffer.
!>
!>  @param[in]    this   A @ref bmw_parallel_context_class instance.
!>  @param[inout] buffer Buffer to reduce.
!-------------------------------------------------------------------------------
      SUBROUTINE bmw_parallel_context_reduce4(this, buffer)
      USE stel_kinds, ONLY: rprec

      IMPLICIT NONE

!  Declare Arguments
      CLASS (bmw_parallel_context_class), INTENT(in)  :: this
      REAL (rprec), DIMENSION(:,:,:,:), INTENT(inout) :: buffer

!  local variables
      REAL (rprec)                                    :: start_time
      INTEGER                                         :: status

!  Start of executable code
      start_time = profiler_get_start_time()

#if defined (MPI_OPT)
      CALL MPI_ALLREDUCE(MPI_IN_PLACE, buffer, SIZE(buffer), MPI_REAL8,        &
     &                   MPI_SUM, this%comm, status)
#endif

      CALL profiler_set_stop_time('bmw_parallel_context_reduce4',              &
     &                            start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Compute the start index of a flat array.
!>
!>  The total problem size is divided by the number of processes. Any remaining
!>  elements are added to the first few processes.
!>
!>  @param[in] this  A @ref bmw_parallel_context_class instance.
!>  @param[in] total Total number of elements in the loop.
!>  @returns The Starting index for parallel multiprocess computation.
!-------------------------------------------------------------------------------
      PURE FUNCTION bmw_parallel_context_start(this, total)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: bmw_parallel_context_start
      CLASS (bmw_parallel_context_class), INTENT(in) :: this
      INTEGER, INTENT(in)                            :: total

!  local variables
      INTEGER                                        :: n_per_process
      INTEGER                                        :: n_left

!  Start of executable code
!  Need to divide the problem space evenly by the number of processes.
      n_per_process = total/this%stride

!  The number of items may not evenly divide by the number of processes.
      n_left = MOD(total, this%stride)

      bmw_parallel_context_start = 1 + this%offset*n_per_process

!  Acound for the remaining elements in the first n_left processes.
      IF (this%offset .lt. n_left) THEN
         bmw_parallel_context_start = bmw_parallel_context_start               &
     &                              + this%offset
      ELSE
         bmw_parallel_context_start = bmw_parallel_context_start               &
     &                              + n_left
      END IF

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Compute the end index of a flat array.
!>
!>  The total problem size is divided by the number of processes. Any remaining
!>  elements are added to the first few processes.
!>
!>  @param[in] this  A @ref bmw_parallel_context_class instance.
!>  @param[in] total Total number of elements in the loop.
!>  @returns The Ending index for parallel multiprocess computation.
!-------------------------------------------------------------------------------
      PURE FUNCTION bmw_parallel_context_end(this, total)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: bmw_parallel_context_end
      CLASS (bmw_parallel_context_class), INTENT(in) :: this
      INTEGER, INTENT(in)                            :: total

!  local variables
      INTEGER                                        :: n_per_process
      INTEGER                                        :: n_left

!  Start of executable code
!  Need to divide the problem space evenly by the number of processes.
      n_per_process = total/this%stride

!  The number of items may not evenly divide by the number of processes.
      n_left = MOD(total, this%stride)

      bmw_parallel_context_end = (this%offset + 1)*n_per_process

!  Acound for the remaining elements in the first n_left processes.
      IF (this%offset .lt. n_left) THEN
         bmw_parallel_context_end = bmw_parallel_context_end               &
     &                            + this%offset + 1
      ELSE
         bmw_parallel_context_end = bmw_parallel_context_end               &
     &                            + n_left
      END IF

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Compute the i index of a flat array.
!>
!>  The total problem size represents a three dimensional space. From the flat
!>  index, compute the i index.
!>
!>  @param[in] index Flat index.
!>  @param[in] num_i Size of the ith dimension.
!>  @returns The index of the i dimension.
!-------------------------------------------------------------------------------
      PURE FUNCTION bmw_parallel_context_i(index, num_i)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: bmw_parallel_context_i
      INTEGER, INTENT(in) :: index
      INTEGER, INTENT(in) :: num_i

!  Start of executable code
      bmw_parallel_context_i = MOD(index - 1, num_i) + 1

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Compute the j index of a flat array.
!>
!>  The total problem size represents a three dimensional space. From the flat
!>  index, compute the j index.
!>
!>  @param[in] index Flat index.
!>  @param[in] num_i Size of the ith dimension.
!>  @param[in] num_j Size of the jth dimension.
!>  @returns The index of the j dimension.
!-------------------------------------------------------------------------------
      PURE FUNCTION bmw_parallel_context_j(index, num_i, num_j)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: bmw_parallel_context_j
      INTEGER, INTENT(in) :: index
      INTEGER, INTENT(in) :: num_i
      INTEGER, INTENT(in) :: num_j

!  Start of executable code
      bmw_parallel_context_j = MOD((index - 1)/num_i, num_j) + 1

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Compute the k index of a flat array.
!>
!>  The total problem size represents a three dimensional space. From the flat
!>  index, compute the k index.
!>
!>  @param[in] index Flat index.
!>  @param[in] num_i Size of the ith dimension.
!>  @param[in] num_j Size of the jth dimension.
!>  @returns The index of the k dimension.
!-------------------------------------------------------------------------------
      PURE FUNCTION bmw_parallel_context_k(index, num_i, num_j)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: bmw_parallel_context_k
      INTEGER, INTENT(in) :: index
      INTEGER, INTENT(in) :: num_i
      INTEGER, INTENT(in) :: num_j

!  Start of executable code
      bmw_parallel_context_k = (index - 1)/(num_j*num_i) + 1

      END FUNCTION

      END MODULE
