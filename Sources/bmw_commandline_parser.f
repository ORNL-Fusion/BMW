!-------------------------------------------------------------------------------
!  The @header2, @begin_table, @item3 and @end_table commands are custom defined
!  commands in Doxygen.in. They are defined under ALIASES. For the page created
!  here, the 80 column limit is exceeded. Arguments of aliases are separated by
!  ','. If you intended ',' to be a string you must use an escaped comma '\,'.
!
!>  @page bmw_cl_parsing_sec Command Line Arguments
!>
!>  @tableofcontents
!>
!>  @section bmw_cl_parsing_intro Introduction
!>  This contains a description of the command line arguments. All arguments
!>  take the form of
!>
!>  @fixed_width{-arg=value}
!>
!>  @section bmw_cl_parsing_arg_sec Command Line Arguments
!>  @header2{Argument, Takes Value, Discription}
!>  @begin_table
!>     @item3{@fixed_width{-h},       N, Displays the help text and exits the program.}
!>     @item3{@fixed_width{-mgridf},  Y, Specify the mgrid input file name.}
!>     @item3{@fixed_width{-woutf},   Y, Specify the wout input file name.}
!>     @item3{@fixed_width{-wvacf},   Y, Specify the vacuum wout input file name.}
!>     @item3{@fixed_width{-siestaf}, Y, Specify the siesta input file name.}
!>     @item3{@fixed_width{-outf},    Y, Specify the output input file name.}
!>     @item3{@fixed_width{-logf},    Y, Write screen output to a log file.}
!>     @item3{@fixed_wdith{-jv},      N, Force balance j^v from curl derived j^u. Overrdes the -ju flag.}
!>     @item3{@fixed_wdith{-ju},      N, Force balance j^u from curl derived j^v.}
!>     @item3{@fixed_wdith{-p_start}, Y, Starting phi index to compute fields. If this flag is not set, default
!>                                       to index 1.}
!>     @item3{@fixed_wdith{-p_end},   Y, Ending phi index to compute fields. If this flag is not set, default to
!>                                       the last index.}
!>     @item3{@fixed_width{-para},    Y, Configures openmp parallelism. The value of this flag sets the maximum
!>                                       number of threads to use. A value of @fixed_width{-1} uses the default.}
!>     @item3{@fixed_width{-force},   N, Force override of error conditions. To prevent loss of valid data, BMW will
!>                                       terminate if an error condition is triggerd. This flag overrides that error
!>                                       potential overwritting valid data.}
!>     @item3{@fixed_width{-num_r},   Y, Number radial points. Note not used when mgridf is specified.}
!>     @item3{@fixed_width{-num_p},   Y, Number toroidal points. Note not used when mgridf is specified.}
!>     @item3{@fixed_width{-num_z},   Y, Number vertial points. Note not used when mgridf is specified.}
!>     @item3{@fixed_width{-rmax},    Y, Maximum radial grid. Note not used when mgridf is specified.}
!>     @item3{@fixed_width{-rmin},    Y, Minimum radial grid. Note not used when mgridf is specified.}
!>     @item3{@fixed_width{-zmax},    Y, Maximum vertical grid. Note not used when mgridf is specified.}
!>     @item3{@fixed_width{-zmin},    Y, Minimum vertical grid. Note not used when mgridf is specified.}
!>  @end_table
!>
!>  @note
!>  OpenMP and MPI support must be configured at compile time to use. A simple way to check if support has been
!>  included is to check the command line help @fixed_width{-h}.
!>
!>  @section bmw_cl_pasring_prog_ref_sec Programmers Reference
!>  Reference material for the coding to implement command line parsing is found
!>  in the @ref bmw_commandline_parser module.
!-------------------------------------------------------------------------------
!*******************************************************************************
!>  @file bmw_commandline_parser.f
!>  @brief Contains module @ref bmw_commandline_parser
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref bmw_commandline_parser_class.
!*******************************************************************************

      MODULE bmw_commandline_parser
      USE stel_kinds
      USE file_opts, only: path_length
      USE profiler
      USE bmw_parallel_context

      IMPLICIT NONE

!*******************************************************************************
!  bmw_commandline parser module parameters
!*******************************************************************************
!>  Maximum length of the argument including the '-' character.
      INTEGER, PARAMETER, PRIVATE :: max_arg_length = 8
!>  Maximum length of the complete flag. All command line flags take the form of
!>  '-flag=value'.
      INTEGER, PARAMETER, PRIVATE :: max_length = path_length                  &
     &                                          + max_arg_length + 1

!  Commandline parser error codes.
!>  Commandline argument not found.
      INTEGER, PARAMETER :: bmw_commandline_parser_no_error      = 0
!>  Commandline argument not found.
      INTEGER, PARAMETER :: bmw_commandline_parser_arg_not_found = -1

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) bmw_commandline parser base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class containing a parsed bmw_commandline.
!-------------------------------------------------------------------------------
      TYPE :: bmw_commandline_parser_class
!>  Command name of current process.
         CHARACTER (len=max_length) :: command
!>  Array of arguments. An argument is the form of -flag
         CHARACTER (len=max_arg_length), DIMENSION(:), POINTER ::              &
     &      arg => null()
!>  Array of value corresponding to the flag. A value is everything after the
!>  '=' character. The value maybe blank indicating there was no value provided.
         CHARACTER (len=path_length), DIMENSION(:), POINTER ::                 &
     &      value => null()
      CONTAINS
         PROCEDURE, PASS ::                                                    &
     &      get_string => bmw_commandline_parser_get_string
         PROCEDURE, PASS ::                                                    &
     &      get_integer => bmw_commandline_parser_get_integer
         PROCEDURE, PASS ::                                                    &
     &      get_real => bmw_commandline_parser_get_real
         GENERIC         :: get => get_string, get_integer, get_real
         PROCEDURE, PASS ::                                                    &
     &      is_flag_set => bmw_commandline_parser_is_flag_set
         PROCEDURE, PASS :: flag_requires_value =>                             &
     &      bmw_commandline_parser_flag_requires_value
         PROCEDURE, PASS ::                                                    &
     &      check_arg => bmw_commandline_parser_check_arg
         FINAL           :: bmw_commandline_parser_destruct
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for the bmw_commandline_parser constructor.
!-------------------------------------------------------------------------------
      INTERFACE bmw_commandline_parser_class
         MODULE PROCEDURE bmw_commandline_parser_construct
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref bmw_commandline_parser_class object.
!>
!>  Allocates memory and initializes a @ref bmw_commandline_parser_class object.
!>  The command line is parsed assuming the the flags take the form of
!>  -flag=value. If the '=' is missing, it is assumed there is no value. If -h
!>  is found, the help text is printed out and the program terminated.
!>
!>  @param[in] parallel @ref bmw_parallel_context_class object instance.
!>  @returns A pointer to a constructed @ref bmw_commandline_parser_class
!>           object.
!-------------------------------------------------------------------------------
      FUNCTION bmw_commandline_parser_construct(parallel)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (bmw_commandline_parser_class), POINTER  ::                        &
     &   bmw_commandline_parser_construct
      CLASS (bmw_parallel_context_class), INTENT(in) :: parallel

!  local variables
      CHARACTER (len=max_length)                     :: temp
      INTEGER                                        :: num_args
      INTEGER                                        :: i
      INTEGER                                        :: value_index
      REAL (rprec)                                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      temp = ''
      num_args = 0
      value_index = 0
      ALLOCATE(bmw_commandline_parser_construct)

!  Read the zeroith arg to get the number of arguments. This should also be the
!  command name.
      CALL getcarg(0, bmw_commandline_parser_construct%command,                &
     &             num_args)

      IF (num_args .le. 0 .and. parallel%offset .eq. 0) THEN
         CALL bmw_commandline_parser_print_help
      END IF

!  Allocate the arrays and
      ALLOCATE(bmw_commandline_parser_construct%arg(num_args))
      ALLOCATE(bmw_commandline_parser_construct%value(num_args))

!  Loop through the command line arguments, and setup the argument and value
!  arrays
      DO i = 1, num_args
         CALL getcarg(i, temp, num_args)

!  Check for a - as the first character.
         IF (temp(1:1) .eq. '-') THEN
            value_index = INDEX(temp, '=')
            IF (value_index .eq. 0) THEN
!  Check for help command.
               IF (TRIM(temp) .eq. '-h' .and.                                  &
     &             parallel%offset .eq. 0) THEN
                  CALL bmw_commandline_parser_print_help
               END IF

               bmw_commandline_parser_construct%arg(i) =                       &
     &            TRIM(bmw_commandline_parser_construct%check_arg(temp))
               bmw_commandline_parser_construct%value(i) = ''
            ELSE
               bmw_commandline_parser_construct%arg(i) =                       &
     &            TRIM(bmw_commandline_parser_construct%check_arg(             &
     &                    temp(1:value_index - 1)))
               bmw_commandline_parser_construct%value(i) =                     &
     &            temp(value_index + 1:LEN_TRIM(temp))
            END IF
         ELSE
            WRITE (*,1000) temp
         END IF

         CALL bmw_commandline_parser_flag_requires_value(                     &
     &            bmw_commandline_parser_construct, i)
      END DO

      CALL profiler_set_stop_time('bmw_commandline_parser_construct',          &
     &                            start_time)

1000  FORMAT('Invalid commandline argument: ',a)
      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref bmw_commandline_parser_class object.
!>
!>  Deallocates memory and uninitializes a @ref bmw_commandline_parser_class
!>  object.
!>
!>  @param[inout] this A @ref bmw_commandline_parser_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE bmw_commandline_parser_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (bmw_commandline_parser_class), INTENT(inout) :: this

!  Start of executable code
      IF (ASSOCIATED(this%arg)) THEN
         DEALLOCATE(this%arg)
         this%arg => null()
      END IF

      IF (ASSOCIATED(this%value)) THEN
         DEALLOCATE(this%value)
         this%value => null()
      END IF

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Get the value of an argument as a string.
!>
!>  Searches the parsed command line flags for a matching command like argument.
!>  If no matching argument is found or no value was set, a blank string is
!>  returned. command line arguments take the form of -flag.
!>
!>  @param[in] this A @ref bmw_commandline_parser_class instance.
!>  @param[in] arg  A argument string to search for.
!>  @returns The argument value.
!-------------------------------------------------------------------------------
      FUNCTION bmw_commandline_parser_get_string(this, arg)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=path_length) :: bmw_commandline_parser_get_string
      CLASS (bmw_commandline_parser_class), INTENT(in) :: this
      CHARACTER (len=*), INTENT(in)                    :: arg

!  Local arguments
      INTEGER                                          :: i
      REAL (rprec)                                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Loop through the arguments until the correct arg is found.
      bmw_commandline_parser_get_string = ''

      IF (ASSOCIATED(this%arg)) THEN
         DO i = 1, SIZE(this%arg)
            IF (TRIM(this%arg(i)) .eq. TRIM(arg)) THEN
               bmw_commandline_parser_get_string = this%value(i)

               CALL profiler_set_stop_time(                                    &
     &                 'bmw_commandline_parser_get_string', start_time)

               RETURN
            END IF
         END DO
      END IF

      CALL profiler_set_stop_time('bmw_commandline_parser_get_string',         &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the value of an argument as an integer.
!>
!>  Searches the parsed command line flags for a matching command like argument.
!>  If no matching argument is found, no value was set or the value cannot be
!>  converted to an integer, the default value is returned. Command line
!>  arguments take the form of -flag.
!>
!>  @param[in] this          A @ref bmw_commandline_parser_class instance.
!>  @param[in] arg           A argument string to search for.
!>  @param[in] default_value Default value in case flag is not set.
!>  @returns The argument value.
!-------------------------------------------------------------------------------
      FUNCTION bmw_commandline_parser_get_integer(this, arg,                   &
     &                                            default_value)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: bmw_commandline_parser_get_integer
      CLASS (bmw_commandline_parser_class), INTENT(in) :: this
      CHARACTER (len=*), INTENT(in)                    :: arg
      INTEGER, INTENT(in)                              :: default_value

!  Local arguments
      CHARACTER (len=path_length)                      :: value
      INTEGER                                          :: status
      REAL (rprec)                                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      value = this%get(arg)

      IF (TRIM(value) .eq. '') THEN
         bmw_commandline_parser_get_integer = default_value

         CALL profiler_set_stop_time(                                          &
     &           'bmw_commandline_parser_get_integer', start_time)

         RETURN
      END IF

      READ (value,1000,iostat=status) bmw_commandline_parser_get_integer

      IF (status .ne. 0) THEN
         bmw_commandline_parser_get_integer = default_value
      END IF

      CALL profiler_set_stop_time('bmw_commandline_parser_get_integer',        &
     &                            start_time)

1000  FORMAT(i20)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the value of an argument as a Real.
!>
!>  Searches the parsed command line flags for a matching command like argument.
!>  If no matching argument is found, no value was set or the value cannot be
!>  converted to an integer, the default value is returned. Command line
!>  arguments take the form of -flag.
!>
!>  @param[in] this          A @ref bmw_commandline_parser_class instance.
!>  @param[in] arg           A argument string to search for.
!>  @param[in] default_value Default value in case flag is not set.
!>  @returns The argument value.
!-------------------------------------------------------------------------------
      FUNCTION bmw_commandline_parser_get_real(this, arg, default_value)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: bmw_commandline_parser_get_real
      CLASS (bmw_commandline_parser_class), INTENT(in) :: this
      CHARACTER (len=*), INTENT(in)                    :: arg
      REAL (rprec), INTENT(in)                         :: default_value

!  Local arguments
      CHARACTER (len=path_length)                      :: value
      INTEGER                                          :: status
      REAL (rprec)                                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      value = this%get(arg)

      IF (TRIM(value) .eq. '') THEN
         bmw_commandline_parser_get_real = default_value

         CALL profiler_set_stop_time('bmw_commandline_parser_get_real',        &
     &                               start_time)

         RETURN
      END IF

      READ (value,*,iostat=status) bmw_commandline_parser_get_real

      IF (status .ne. 0) THEN
         bmw_commandline_parser_get_real = default_value
      END IF

      CALL profiler_set_stop_time('bmw_commandline_parser_get_real',           &
     &                            start_time)

      END FUNCTION

!*******************************************************************************
!  QUERY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Check if a command line argument was set.
!>
!>  Searches the parsed command line flags for a matching command line argument.
!>  If no matching argument is found the flag was not set.
!>
!>  @param[in] this A @ref bmw_commandline_parser_class instance.
!>  @param[in] arg  A argument string to search for.
!>  @returns True if the argument was found, false if it was not found.
!-------------------------------------------------------------------------------
      FUNCTION bmw_commandline_parser_is_flag_set(this, arg)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL :: bmw_commandline_parser_is_flag_set
      CLASS (bmw_commandline_parser_class), INTENT(in) :: this
      CHARACTER (len=*), INTENT(in)                    :: arg

!  Local arguments
      INTEGER                                          :: i
      REAL (rprec)                                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Loop through the arguments until the correct arg is found.
      bmw_commandline_parser_is_flag_set = .false.

      IF (ASSOCIATED(this%arg)) THEN
         DO i = 1, SIZE(this%arg)
            IF (TRIM(this%arg(i)) .eq. TRIM(arg)) THEN
               bmw_commandline_parser_is_flag_set = .true.

               CALL profiler_set_stop_time(                                    &
     &                 'bmw_commandline_parser_is_flag_set', start_time)

               RETURN
            END IF
         END DO
      END IF

      CALL profiler_set_stop_time('bmw_commandline_parser_is_flag_set',        &
     &                            start_time)

      END FUNCTION

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Check if a command line argument requires a value.
!>
!>  Command line arguments for
!>
!>    * -mgridf
!>    * -woutf
!>    * -wvacf
!>    * -siestaf
!>    * -outf
!>    * -logf
!>
!>  require a value to be set.
!>
!>  @param[in] this A @ref bmw_commandline_parser_class instance.
!>  @param[in] index  A argument string to search for.
!>  @returns True if the argument has a value.
!-------------------------------------------------------------------------------
      SUBROUTINE bmw_commandline_parser_flag_requires_value(this, index)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (bmw_commandline_parser_class), INTENT(in) :: this
      INTEGER, INTENT(in)                              :: index

!  Local arguments
      REAL (rprec)                                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (TRIM(this%arg(index)))

         CASE ('-mgridf','-woutf','-wvacf','-siestaf','-outf','-logf')
            IF (TRIM(this%value(index)) .eq. '') THEN
               WRITE (*,1000) TRIM(this%arg(index)),                           &
     &                        TRIM(this%arg(index))

               CALL bmw_commandline_parser_print_help
            END IF

      END SELECT

      CALL profiler_set_stop_time(                                             &
     &        'bmw_commandline_parser_flag_requires_value', start_time)

1000  FORMAT(a,' flag requires value. Usage: ',a,'=value')

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Check if a command line argument is valid.
!>
!>  Valid commands are
!>
!>    * -h
!>    * -mgridf
!>    * -woutf
!>    * -wvacf
!>    * -siestaf
!>    * -outf
!>    * -logf
!>    * -jv
!>    * -ju
!>    * -p_start
!>    * -p_end
!>    * -para
!>    * -force
!>    * -num_r
!>    * -num_p
!>    * -num_z
!>    * -rmax
!>    * -rmin
!>    * -zmax
!>    * -zmin
!>
!>  @param[in] this A @ref bmw_commandline_parser_class instance.
!>  @param[in] arg  A command line argument to check.
!>  @returns The argument if it's valid.
!-------------------------------------------------------------------------------
      FUNCTION bmw_commandline_parser_check_arg(this, arg)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=path_length) :: bmw_commandline_parser_check_arg
      CLASS (bmw_commandline_parser_class), INTENT(in) :: this
      CHARACTER (len=*), INTENT(in)                    :: arg

!  Local arguments
      INTEGER                                          :: i
      REAL (rprec)                                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (TRIM(arg))

         CASE ('-h','-mgridf','-woutf','-wvacf','-siestaf','-outf',            &
     &         '-logf','-jv','-ju','-p_start','-p_end','-para',                &
     &         '-force','-num_r','-num_p','-num_z','-rmax','-rmin',            &
     &         '-zmax','-zmin')
            bmw_commandline_parser_check_arg = arg

         CASE DEFAULT
            WRITE (*,1000) TRIM(arg)
            CALL bmw_commandline_parser_print_help

      END SELECT

      CALL profiler_set_stop_time('bmw_commandline_parser_check_arg',          &
     &                            start_time)

1000  FORMAT('Invalid commandline argument: ',a)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Print out help text.
!>
!>  Command line help text should take the form of '-flag' that is maximum
!>  length of @ref max_arg_length followed by a space. A Y or N indicating the
!>  flag takes a value or not followed by a space. A short message describing
!>  the flag. A reference for all the command line arguments can be found at
!>  @ref cl_parsing_sec.
!-------------------------------------------------------------------------------
      SUBROUTINE bmw_commandline_parser_print_help

      IMPLICIT NONE

!  Start of executable code
!  All command line messages need to fit within this width.
!                '           s              c                          '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '                         BMW                         '
      WRITE(*,*) '                                                     '
      WRITE(*,*) 'Usage: xbmw [-arg][=option] ...                      '
      WRITE(*,*) '                                                     '
      WRITE(*,*) 'Options:                                             '
      WRITE(*,*) 'All options are displayed as [arg][takesoption][text]'
      WRITE(*,*) '  -h       N Display this information                '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -mgridf  Y Specify the mgrid file name.            '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -woutf   Y Specify the wout file name.             '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -wvacf   Y Specify the vacume wout file name.      '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -siestaf Y Specify the siesta restart file name.   '
      WRITE(*,*) '             When this flag is used, plasma currents '
      WRITE(*,*) '             are computed from siesta fields instead '
      WRITE(*,*) '             instead of vmec fields. Overrides the   '
      WRITE(*,*) '             -ju and -jv flags.                      '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -outf    Y Specify the output file name.           '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -logf    Y Write screen output to a log file.      '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -jv      N Force balance j^v from curl derived j^u.'
      WRITE(*,*) '             Overrdes the -ju flag.                  '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -ju      N Force balance j^u from curl derived j^v.'
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -p_start Y Starting phi index to compute fields. If'
      WRITE(*,*) '             this flag is not set, default to index  '
      WRITE(*,*) '             1.                                      '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -p_end   Y Ending phi index to compute fields. If  '
      WRITE(*,*) '             this flag is not set, default to the    '
      WRITE(*,*) '             last index.                             '
      WRITE(*,*) '                                                     '
!$    WRITE(*,*) '  -para    Y Determines number of threads threads to '
!$    WRITE(*,*) '             run with. A value of -1 means use the   '
!$    WRITE(*,*) '             default number of threads.              '
!$    WRITE(*,*) '                                                     '
      WRITE(*,*) '  -force   N Force override of error conditions. To  '
      WRITE(*,*) '             prevent loss of valid data, BMW will    '
      WRITE(*,*) '             terminate if an error condition is      '
      WRITE(*,*) '             triggerd. This flag overrides that error'
      WRITE(*,*) '             potential overwritting valid data.      '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -num_r   Y Number radial points. Note not used when'
      WRITE(*,*) '             mgridf is specified.                    '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -num_p   Y Number toroidal points. Note not used   '
      WRITE(*,*) '             when mgridf is specified.               '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -num_z   Y Number vertial points. Note not used    '
      WRITE(*,*) '             when mgridf is specified.               '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -rmax    Y Maximum radial grid. Note not used when '
      WRITE(*,*) '             mgridf is specified.                    '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -rmin    Y Minimum radial grid. Note not used when '
      WRITE(*,*) '             mgridf is specified.                    '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -zmax    Y Maximum vertical grid. Note not used    '
      WRITE(*,*) '             when mgridf is specified.               '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -zmin    Y Minimum vertical grid. Note not used    '
      WRITE(*,*) '             when mgridf is specified.               '
      WRITE(*,*) '                                                     '
      CALL bmw_parallel_context_abort(0)

      END SUBROUTINE

      END MODULE
