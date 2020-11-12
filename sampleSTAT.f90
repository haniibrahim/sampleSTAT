! sampleSTAT - Statistics for Sampling Distributions
! =============================================================================
!
! Purpose:
!    Performs statistical tests for sampling distributions:
!      - Arithmetic Mean
!      - Standard Deviation
!      - Range of Dispersion of Values
!      - Range of Dispersion of Mean
!      - Minimum Value
!      - Maximum Value
!
!
! History:
!    Version   Programmer         Date       Description
!    -------   ----------         ---------- -----------
!    1.0       Ibrahim, Hani      2006-05-29 Original code
!    1.1       Ibrahim, Hani      2016-01-19 Changed -s/--sens to -l/--level
!                                            and some little cosmetic corrections
!    1.2       Ibrahim, Hani      2020-11-07 Switched to linked list for reading data
!
! Processing:
!   Read 1-dimensional numerical data from stdin (pipe/redirection) and calculate
!   elementary tests for the evaluation of univariate measurement data that are
!   typically recorded by scientists and engineers. These data have to be normally
!   distributed for sampleSTAT's routines:
!

MODULE PrgMod
    USE SysConst
    USE SampleStatistics

    IMPLICIT NONE
	
	INTEGER :: stderr ! stderr unit

CONTAINS

!----------------------------------------------------------------------
   SUBROUTINE Foo()
      IMPLICIT NONE
      WRITE(StdErr,*) 'Option not implemented yet'
   END SUBROUTINE Foo
!----------------------------------------------------------------------
   SUBROUTINE Version()
   ! Write version information to stdout
      IMPLICIT NONE
      WRITE(*,'(/,5(A/))') &
      'sampleSTAT - Version 1.2 - 2020-11-07',&
      'For information, please contact: Hani Andreas Ibrahim (hani.ibrahim@gmx.de)',&
      'sampleSTAT comes with NO WARRANTY, to the extent permitted by law. You may ',&
      'redistribute copies of sampleSTAT under the terms of the GNU General Public ',&
      'License, refer <www.gnu.org/licenses/gpl.html> for details.'
   END SUBROUTINE Version
!----------------------------------------------------------------------
   SUBROUTINE Help()
   ! Write help page to stdout
      WRITE(*,'(/,3(A/),/,7(A/),/,(4(A/)),/,(5(A/)),/)')&
      'sampleSTAT performs tests for statistical samples:',&
      '   Aritmetic Mean, Range of Dispersion of values and mean based on t-factor,', &
      '   Standard Deviation, Minimum, Maximum.', &
      'Usage: sampleSTAT [-hv] -l X [<inputfile] [>outputfile]',&
      '  -h    --help /?   Print this help screen',&
      '  -v    --version   Print version information',&
      '  -l P  --level=P   Set confidence level:',&
      '                    P=95   => conf. level: 95%',&
      '                    P=99   => conf. level: 99%',&
      '                    P=99.9 => conf. level: 99.9%',&
      'Examples:',&
      '  sampleSTAT -l 95 <mydata.dat',&
      '  sampleSTAT --level=99 <mydata.dat >results.txt',&
      '  sampleSTAT -l 99.9 <mydata.dat',&
      'Input data:',&
      '  Data has to be committed in a one column form, like:',&
      '     22.43',&
      '     22.45',&
      '     22.50'
   END SUBROUTINE Help

END MODULE PrgMod

!======================================================================

PROGRAM sampleSTAT

   USE SysConst
   USE SampleStatistics
   USE PrgMod     ! ReadInVec
   USE Sng        ! [libsng] Command-line parsing
   USE ReadData   ! Read data from stdin

   IMPLICIT NONE

   INTEGER                                  :: I                 ! Loop index
   INTEGER                                  :: ReadErr           ! error variables
   REAL(KIND=DP), DIMENSION(:), POINTER     :: Values_ptr        ! Data values
   INTEGER                                  :: N                 ! Numbers of values
   REAL(KIND=SP)                            :: Level = -1.       ! confidence level ...
                                                                 ! ... for "OutlierOut" routine
   CHARACTER(LEN=6)                         :: LevelMsg          ! confidence level string
   REAL(KIND=DP)                            :: StrayAreaResult   ! Stray Area of single values depending on conf. level
  ! Command-line variables
   character(16)                            ::arg_val            ! [sng] Command line argument value
   character(16)                            ::opt_sng            ! [sng] Option string
   character(2)                             ::dsh_key1, dsh_key2 ! [sng] Command line dash and switch
   integer                                  ::arg_idx            ! [idx] Counting index
   integer                                  ::arg_nbr            ! [nbr] Number of command line arguments
   integer                                  ::opt_lng            ! [nbr] Length of option

   ! Command-line option switches
   LOGICAL                                  :: LogLevel =.FALSE. ! Value "Level" commited: true/false
   LOGICAL                                  :: LogHlp   =.FALSE. ! Switch "help"
   LOGICAL                                  :: LogVer   =.FALSE. ! Switch "version"


   ! Error messages
   CHARACTER(Len=*), PARAMETER              :: DeallocError = 'Deallocation error!'
   CHARACTER(Len=*), PARAMETER              :: AllocError   = 'Allocation error!'
   CHARACTER(Len=*), PARAMETER              :: LngOptErr    = 'Long option has no name!'
   CHARACTER(Len=*), PARAMETER              :: LevelError   = 'Wrong confidence level committed! Refer "samplestat --help"!'
   CHARACTER(Len=*), PARAMETER              :: OptError     = 'No meaningful options committed! Refer "samplestat --help"!'
   CHARACTER(Len=*), PARAMETER              :: R_OpnError   = 'Data file open error!'
   CHARACTER(Len=*), PARAMETER              :: R_ReadError  = 'Data file read error!'
   CHARACTER(Len=*), PARAMETER              :: R_AllocError = 'Data file allocation error!'

   !Report strings
   CHARACTER(Len=*), PARAMETER              :: Rpt_00       = '' ! left margin
   CHARACTER(Len=*), PARAMETER              :: Rpt_01       = 'sampleSTAT - Statistics for Sampling Distributions'
   CHARACTER(Len=*), PARAMETER              :: Rpt_02       = '=================================================='
   CHARACTER(Len=*), PARAMETER              :: Rpt_03       = 'Number of Values            : '
   CHARACTER(Len=*), PARAMETER              :: Rpt_04       = 'Arithmetic Mean             : '
   CHARACTER(Len=*), PARAMETER              :: Rpt_05       = 'Confidence Level            : '
   CHARACTER(Len=*), PARAMETER              :: Rpt_06       = 'Range of Dispersion (values): '
   CHARACTER(Len=*), PARAMETER              :: Rpt_07       = 'Range of Dispersion (mean)  : '
   CHARACTER(Len=*), PARAMETER              :: Rpt_08       = 'Standard Deviation          : '
   CHARACTER(Len=*), PARAMETER              :: Rpt_09       = 'Minimum                     : '
   CHARACTER(Len=*), PARAMETER              :: Rpt_10       = 'Maximum                     : '
   CHARACTER(Len=*), PARAMETER              :: Level0Msg    = '95%'
   CHARACTER(Len=*), PARAMETER              :: Level1Msg    = '99%'
   CHARACTER(Len=*), PARAMETER              :: Level2Msg    = '99.9%'

   ! Nullify pointer(s)
   NULLIFY(Values_ptr)

! --- Command-line parsing
   arg_nbr=command_argument_count() ! [nbr] Number of command line arguments
   arg_idx=1 ! [idx] Counting index

   cmd_ln: do while (arg_idx <= arg_nbr)

      call ftn_getarg_wrp(arg_idx,arg_val) ! [sbr] Call getarg, increment arg_idx

      dsh_key1=arg_val(1:1) ! First character of option (e.g. -)
      dsh_key2=arg_val(1:2) ! First two characters of option (e.g. --)
      opt_lng=ftn_opt_lng_get(arg_val) ! [nbr] Length of option

      if (opt_lng <= 0) stop "Long option has no name"

      ! Handle long options
      lng_cmd: IF (dsh_key2 == '--') THEN
         opt_sng=arg_val(3:2+opt_lng) ! Option string without --

         opt: IF (opt_sng == 'level' ) THEN         ! confidence level (/ 0, 1, 2 /)
            CALL ftn_arg_get(arg_idx,arg_val,Level)
            LogLevel = .TRUE.
         ELSE IF (opt_sng == 'version' ) THEN      ! Print version
            LogVer = .TRUE.
         ELSE IF (opt_sng == 'help' ) THEN         ! Print help page
            LogHlp = .TRUE.
         ELSE opt ! Option not recognized
            arg_idx=arg_idx-1 ! [idx] Counting index
            CALL ftn_getarg_err(arg_idx,arg_val) ! [sbr] Error handler for getarg()
         END IF opt ! endif option is recognized

         ! Jump to top of while loop
         CYCLE cmd_ln! C, F77, and F90 use 'continue', 'goto', and 'cycle'

      END IF lng_cmd ! endif long option

      ! Short option with parameters (e.g. -l 2)
      s_par: if (dsh_key2 == '-l') then
         call ftn_arg_get(arg_idx, arg_val, Level) ! Read parameter of '-l'
         LogLevel = .TRUE.
         cycle cmd_ln ! Next option
      ELSE IF (dsh_key2 == '/?') THEN
         LogHlp = .TRUE.
      end if s_par


      ! Handle short options
      short_cmd: IF ((dsh_key1 == "-") .AND. (dsh_key2 /= '--')) THEN
         DO i=2, opt_lng+1
            IF ((arg_val(i:i) == 'h') .OR. (arg_val(i:i) == 'H')) THEN
               LogHlp = .TRUE.
            ELSE IF ((arg_val(i:i) == 'v') .OR. (arg_val(i:i) == 'V')) THEN
               LogVer = .TRUE.
           ELSE
               arg_idx = arg_idx - 1
               call ftn_getarg_err(arg_idx, arg_val) ! Error handler
            end if
         end do
         cycle cmd_ln ! Next option
      END IF short_cmd

   END DO cmd_ln             ! end while (arg_idx <= arg_nbr)

! --- Examine committed options, read in and proceed data
   IF (arg_nbr == 0) THEN ! If no arguments are committed display version and help info
      CALL Help()
      STOP
   END IF

   ! Quit, if senseless options are committed ...


   ! Print help- or version-screen if right option was committed
   IF (LogHlp) CALL Help()
   IF (LogVer) CALL Version()
   IF (LogHlp .OR. LogVer) STOP ! Avoid further processing after print help or version

   ! Check committed options for "l" or "level"
   IF (( .NOT.(Level >= 0 .AND. Level <= 2)) .AND. ((Level .NE. 95) .AND. (Level .NE. 99) .AND. (Level .NE. 99.9))) THEN
       STOP LevelError
   END IF

   IF (LogLevel) THEN  !-------------------------------------------------------- Report
      ! Read in data
      CALL ReadInData(Values_ptr, N, ReadErr)
      ! Data file error handling
      IF (ReadErr == 1) STOP R_OpnError
      IF (ReadErr == 2) STOP R_ReadError
      IF (ReadErr == 3) STOP R_AllocError
      ! confidence level messages for report
      IF ((Level == 0) .OR. (Level == 95)) THEN ! Strayarea of the single values, stat. sec. 95%
         LevelMsg = Level0Msg
         StrayAreaResult = StrayArea(Values_ptr, N, 'lo')
      ELSE IF ((Level == 1) .OR. (Level == 99)) THEN ! Strayarea of the single values, stat. sec. 99%
         LevelMsg = Level1Msg
         StrayAreaResult = StrayArea(Values_ptr, N, 'md')
      ELSE IF ((Level == 2) .OR. (Level == 99.9)) THEN ! Strayarea of the single values, stat. sec. 99,9%
         LevelMsg = Level2Msg
         StrayAreaResult = StrayArea(Values_ptr, N, 'hi')
      END IF
      ! Report message
      WRITE(*,*)
      WRITE(*,*) Rpt_00, Rpt_01                             ! Title
      WRITE(*,*) Rpt_00, Rpt_02                             ! Underline
      WRITE(*,'(4A,i0)') ' ', Rpt_00, Rpt_03, '   ', N      ! Numbers of values
      WRITE(*,*) Rpt_00, Rpt_04, ArithMean(Values_ptr, N)   ! Mean
      WRITE(*,'(5A)') ' ', Rpt_00, Rpt_05, '   ', TRIM(LevelMsg)   ! confidence level
      WRITE(*,*) Rpt_00, Rpt_06, StrayAreaResult            ! Strayarea of the single values
      WRITE(*,*) Rpt_00, Rpt_07, StrayAreaResult/SQRT(REAL(N,KIND=DP))    ! TrustArea, Strayarea of mean
      WRITE(*,*) Rpt_00, Rpt_08, StdDev(Values_ptr, N)      ! Standard deviation
      WRITE(*,*) Rpt_00, Rpt_09, MinVar(Values_ptr, N)      ! Minimum
      WRITE(*,*) Rpt_00, Rpt_10, MaxVar(Values_ptr, N)      ! Maximum
      WRITE(*,*)                                            ! Blank line after report
   ELSE
      STOP OptError
   END IF

END PROGRAM sampleSTAT
