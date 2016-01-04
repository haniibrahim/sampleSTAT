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
!    1.0       Ibrahim, Hani      05/29/2006 Original code
!
! Processing:
!    Data has to be in a column and is read from Stdin. 
!
!
!    

MODULE PrgMod
    USE SysConst
    USE SampleStatistics
    
    IMPLICIT NONE
    
    INTEGER, PARAMETER :: StdErr = 0 ! Standard Error Unit (here 0)
   
CONTAINS
!----------------------------------------------------------------------   
   SUBROUTINE ReadInVec(Dat_ptr, N, Error)
   ! Read in data from pipe, keyboard or command-line redirection and store the data
   ! in a real pointer vector.
      IMPLICIT NONE
      REAL(KIND=DP), DIMENSION(:), POINTER :: Dat_ptr    ! OUT: Data values
      INTEGER, INTENT(OUT)                 :: N          ! OUT: Number of values
      INTEGER, INTENT(OUT)                 :: Error      ! OUT: Error flag
                                                         !      0 -> no error
                                                         !      1 -> open file error
                                                         !      2 -> read data error
                                                         !      3 -> alloc error
        
      INTEGER                         :: AllocStat       ! Allocate status
      INTEGER                         :: IOStatus        ! I/O status
      REAL(KIND=DP)                   :: Temp            ! Temp. storage of data
      INTEGER                         :: I               ! Loop index
      INTEGER                         :: Scratch_ID = 15 ! Unit ID for scratchfile
      
      Error = 0                                          ! Set error flag -> no error
      N = 0                                              ! Set Counter = 0
      NULLIFY(Dat_ptr)
      
      OPEN(Scratch_ID, STATUS='SCRATCH', IOSTAT=IOStatus)
  
      file_io: IF (IOStatus == 0) THEN      ! successfully opened 
         handle_data: DO 
            READ(*,*, IOSTAT=IOStatus) Temp ! read from stdin/pipe ...
            IF (IOStatus < 0) EXIT          ! EOF reached
            count_n: IF (IOStatus > 0) THEN ! read error
               Error = 2
            ELSE count_n
               N = N + 1                    ! ... count the numbers of values ...
               WRITE(Scratch_ID,*) Temp     ! ... and write values to scratch file
            END IF count_n
         END DO handle_data
      ELSE file_io
         Error   = 1                        ! open error
      END IF file_io

      err_chk: IF (Error == 0) THEN   
         ALLOCATE(Dat_ptr(N), STAT=AllocStat)! alloc output pointer
         alloc: IF (AllocStat /= 0) THEN     ! alloc error
            Error = 3
         ELSE alloc
            REWIND(Scratch_ID)               ! Rewind scratch file
            readin: Do I=1, N                ! Read values from scratch file and store them in Dat_ptr
               READ(Scratch_ID,*) Dat_ptr(I)
            END DO readin
         END IF alloc
      END IF err_chk
      
   END SUBROUTINE ReadInVec
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
      'sampleSTAT - Version 1.0 - 06/28/2007',&
      'For information, please contact: Hani Andreas Ibrahim (hani.ibrahim@gmx.de)',&
      'sampleSTAT comes with NO WARRANTY, to the extent permitted by law. You may ',&
      'redistribute copies of sampleSTAT under the terms of the GNU General Public ',&
      'License, refer <www.gnu.org/licenses/gpl.html> for details.'
   END SUBROUTINE Version
!----------------------------------------------------------------------   
   SUBROUTINE Help()
   ! Write help page to stdout
      WRITE(*,'(/,3(A/),/,7(A/),/,(3(A/)),/,(5(A/)),/)')&
      'sampleSTAT performs tests for statistical samples:',& 
      '   Aritmetic Mean, Range of Dispersion of values and mean based on t-factor,', &
      '   Standard Deviation, Minimum, Maximum.', &
      'Usage: sampeSTAT [-hv] -s X [<inputfile] [>outputfile]',&
      '  -h    --help /?   Print this help screen',&
      '  -v    --version   Print version information',&
      '  -s X  --sens=X    Set confidence level:',&
      '                    X=0 conf. level: 95%',&
      '                    X=1 conf. level: 99%',&
      '                    X=2 conf. level: 99.9%',&
      'Examples:',&
      '  sampleSTAT -s 0 <mydata.dat',&
      '  sampleSTAT --sens=1 <mydata.dat >results.txt',&
      'Input data:',&
      '  Data has to be committed in a one column form, like:',&
      '     22.43',&
      '     22.45',&
      '     22.50'
   END SUBROUTINE Help

END MODULE PrgMod
!======================================================================
PROGRAM sample_stat

   USE SysConst
   USE SampleStatistics
   USE PrgMod    ! ReadInVec
   USE Sng        ! [libsng] Command-line parsing

   IMPLICIT NONE
   
   INTEGER                                  :: I                         ! Loop index
   INTEGER                                  :: ReadErr, AllocErr         ! error variables
   REAL(KIND=DP), DIMENSION(:), POINTER     :: Values_ptr                ! Data values
   INTEGER                                  :: N                         ! Numbers of values
   INTEGER                                  :: Sens = -1                 ! Statistical senitivity ...
                                                                         ! ... for "OutlierOut" routine
   CHARACTER(LEN=6)                         :: SensMsg                   ! statistival sensitivity string
   INTEGER                                  :: MaxNum                    ! Max. numbers of lines for table of report
   REAL(KIND=DP)                            :: StrayAreaResult           ! Stray Area of single values depending on stat. sens.
   
  ! Command-line variables
   character(16)                            ::arg_val            ! [sng] Command line argument value
   character(16)                            ::opt_sng            ! [sng] Option string
   character(2)                             ::dsh_key1, dsh_key2 ! [sng] Command line dash and switch
   integer                                  ::arg_idx            ! [idx] Counting index
   integer                                  ::arg_nbr            ! [nbr] Number of command line arguments
   integer                                  ::opt_lng            ! [nbr] Length of option
   
   ! Command-line option switches
   LOGICAL                                  :: LogSens =.FALSE.  ! Value "Sens" commited: true/false
   LOGICAL                                  :: LogHlp  =.FALSE.  ! Switch "help"
   LOGICAL                                  :: LogVer  =.FALSE.  ! Switch "version"
  

   ! Error messages
   CHARACTER(Len=*), PARAMETER              :: DeallocError = 'Deallocation error!'
   CHARACTER(Len=*), PARAMETER              :: AllocError   = 'Allocation error!'
   CHARACTER(Len=*), PARAMETER              :: LngOptErr    = 'Long option has no name!'
   CHARACTER(Len=*), PARAMETER              :: SensError    = 'Wrong statistical sensitivity committed! Refer "samplestat --help"!'
   CHARACTER(Len=*), PARAMETER              :: OptError     = 'No meaningful options committed! Refer "samplestat --help"!'
   CHARACTER(Len=*), PARAMETER              :: R_OpnError   = 'Data file open error!'
   CHARACTER(Len=*), PARAMETER              :: R_ReadError  = 'Data file read error!'
   CHARACTER(Len=*), PARAMETER              :: R_AllocError = 'Data file allocation error!'
   
   !Report strings
   CHARACTER(Len=*), PARAMETER              :: Rpt_00       = '    ' ! left margin
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
   CHARACTER(Len=*), PARAMETER              :: Sens0Msg     = ' 95%'
   CHARACTER(Len=*), PARAMETER              :: Sens1Msg     = ' 99%'
   CHARACTER(Len=*), PARAMETER              :: Sens2Msg     = ' 99,9%'
   
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
         
         opt: IF (opt_sng == 'sens' ) THEN         ! Statistical sensitivity (/ 0, 1, 2 /)
            CALL ftn_arg_get(arg_idx,arg_val,Sens)
            LogSens = .TRUE.
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
      
      ! Short option with parameters (e.g. -s 2)
      s_par: if (dsh_key2 == '-s') then
         call ftn_arg_get(arg_idx, arg_val, Sens) ! Read parameter of '-s'
         LogSens = .TRUE.
         cycle cmd_ln ! Next option
      ELSE IF (dsh_key2 == '/?') THEN
         LogHlp = .TRUE.
      end if s_par
      
      
      ! Handle short options
      !~ short_cmd: IF (((dsh_key1 == "-") .OR. (dsh_key1 == '/')) .AND. (dsh_key2 /= '--')) THEN
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
   IF (arg_nbr == 0) THEN                                       ! If no arguments are committed display version and help info
      CALL Help()
      STOP
   END IF
   
   ! Quit, if sensless options are committed ...
                   
   
   ! Print help- or version-screen if right option was committed
   IF (LogHlp) CALL Help()
   IF (LogVer) CALL Version()
   IF (LogHlp .OR. LogVer) STOP ! Avoid further processing after print help or version
   
   IF (Sens > 2 .OR. Sens < 0) STOP SensError                   ! Sens has to be 0, 1, 2
   IF (LogSens) THEN  !-------------------------------------------------------- Report
      ! Read in data
      CALL ReadInVec(Values_ptr, N, ReadErr)
      ! Data file error handling 
      IF (ReadErr == 1) STOP R_OpnError
      IF (ReadErr == 2) STOP R_ReadError
      IF (ReadErr == 3) STOP R_AllocError
      ! Stat. sensitivity messages for report
      IF (Sens == 0) THEN ! Strayarea of the single values, stat. sec. 95%
         SensMsg = Sens0Msg
         StrayAreaResult = StrayArea(Values_ptr, N, 'lo')
      ELSE IF (Sens == 1) THEN ! Strayarea of the single values, stat. sec. 99%
         SensMsg = Sens1Msg
         StrayAreaResult = StrayArea(Values_ptr, N, 'md')
      ELSE IF (Sens == 2) THEN ! Strayarea of the single values, stat. sec. 99,9%
         SensMsg = Sens2Msg
         StrayAreaResult = StrayArea(Values_ptr, N, 'hi')
      END IF
      ! Report message
      WRITE(*,*)
      WRITE(*,*) Rpt_00, Rpt_01                             ! Titel
      WRITE(*,*) Rpt_00, Rpt_02                             ! Underline
      WRITE(*,*) Rpt_00, Rpt_03, N                          ! Numbers of values
      WRITE(*,*) Rpt_00, Rpt_04, ArithMean(Values_ptr, N)   ! Mean
      WRITE(*,*) Rpt_00, Rpt_05, TRIM(SensMsg)              ! Stat. sensitivity
      WRITE(*,*) Rpt_00, Rpt_06, StrayAreaResult            ! Strayarea of the single values
      WRITE(*,*) Rpt_00, Rpt_07, StrayAreaResult/SQRT(REAL(N,KIND=DP))    ! TrustArea, Strayarea of mean
      WRITE(*,*) Rpt_00, Rpt_08, StdDev(Values_ptr, N)      ! Standard deviation
      WRITE(*,*) Rpt_00, Rpt_09, MinVar(Values_ptr, N)      ! Minimum
      WRITE(*,*) Rpt_00, Rpt_10, MaxVar(Values_ptr, N)      ! Maximum
      WRITE(*,*)                                    ! Blank line after report
   ELSE
      STOP OptError
   END IF
   
! --- Deallocate all dynamic arrays and pointers
   IF (ASSOCIATED(Values_ptr)) DEALLOCATE(Values_ptr, STAT=AllocErr)
   IF (AllocErr /= 0) STOP DeallocError

END PROGRAM sample_stat
