MODULE SampleStatistics
!
! ================== Prologue ==========================================
!
! Purpose:
!    Offers routines for sample (series) statistics, like mean, variance,
!    standard deviation and student factor as well as stray area of the 
!    values and trust area of the arith. mean.
!
!    Calculations use double precision (on IEEE 754) reals.
! 
! History:
!    Version   Programmer         Date       Description
!    -------   ----------         ---------- -----------
!    1.0       Ibrahim, Hani      2006/04/01 Orginal code
!    1.0.1     Ibrahim, Hani      2006/05/23 - Pointer bug removed from OutlierOut[Nalimov]
!                                            - Add Error code "OError" to OutlierOut[Nalimov]
!    1.0.2     Ibrahim, Hani      2006/06/15 Add OPTIONAL attributes to some args of OutlierOut [Nalimov]
!
! User routines:
!     ArithMean = Aritmetic mean
!     MinVar    = Min. value
!     MaxVar    = Max. value
!     Sums      = Sums of x(i) and x^2(i)                                                                        
!     Variance  = Statistical variance
!     StdDev    = Standard deviation
!     StrayArea = Range of dispersion
!     TrustArea = Stray area of the arithmetical mean
!
! Special requirements:
!     MODULE SysConst
!
! ------------------ Use Module / Include files ------------------------
   USE SysConst
! ------------------ Implicit ------------------------------------------
   IMPLICIT NONE
! ------------------ Local declarations --------------------------------
   PUBLIC  :: ArithMean, MaxVar, MinVar, Variance, StdDev, StrayArea, & 
              TrustArea, Sums
   PRIVATE :: StudentFactor
! ------------------ Constant declarations -----------------------------

   ! Contains the Student factor t-Table
   ! ===================================
   ! Column 1 : Degree of freedom (f)
   ! Column 2 : Student factor, 95% security
   ! Column 3 : Student factor, 99% security
   ! Column 4 : Student factor, 99.9% security
   !
   ! 999 := infinite
   
   REAL(KIND=DP), DIMENSION(35,4), Parameter, PRIVATE :: TTable = RESHAPE((/ &
     1.0_DP, 12.71_DP,  63.66_DP, 636.62_DP, &
     2.0_DP,  4.30_DP,   9.92_DP,  31.60_DP, &
     3.0_DP,  3.18_DP,   5.84_DP,  12.92_DP, &
     4.0_DP,  2.78_DP,   4.60_DP,   8.61_DP, &
     5.0_DP,  2.57_DP,   4.03_DP,   6.86_DP, &
     6.0_DP,  2.45_DP,   3.71_DP,   5.96_DP, &
     7.0_DP,  2.37_DP,   3.50_DP,   5.41_DP, &
     8.0_DP,  2.31_DP,   3.36_DP,   5.04_DP, &
     9.0_DP,  2.26_DP,   3.25_DP,   4.78_DP, &
    10.0_DP,  2.23_DP,   3.17_DP,   4.59_DP, &
    11.0_DP,  2.20_DP,   3.11_DP,   4.44_DP, &
    12.0_DP,  2.18_DP,   3.06_DP,   4.32_DP, &
    13.0_DP,  2.16_DP,   3.01_DP,   4.22_DP, &
    14.0_DP,  2.15_DP,   2.98_DP,   4.14_DP, &
    15.0_DP,  2.13_DP,   2.95_DP,   4.07_DP, &
    16.0_DP,  2.12_DP,   2.92_DP,   4.02_DP, &
    17.0_DP,  2.11_DP,   2.90_DP,   3.96_DP, &
    18.0_DP,  2.10_DP,   2.88_DP,   3.92_DP, &
    19.0_DP,  2.09_DP,   2.86_DP,   3.88_DP, &
    20.0_DP,  2.08_DP,   2.85_DP,   3.85_DP, &
    25.0_DP,  2.060_DP,  2.787_DP,  3.725_DP,&
    30.0_DP,  2.042_DP,  2.750_DP,  3.646_DP,&
    35.0_DP,  2.030_DP,  2.724_DP,  3.592_DP,&
    40.0_DP,  2.021_DP,  2.704_DP,  3.551_DP,&
    45.0_DP,  2.014_DP,  2.689_DP,  3.521_DP,&
    50.0_DP,  2.009_DP,  2.678_DP,  3.496_DP,&
   100.0_DP,  1.984_DP,  2.626_DP,  3.390_DP,&
   200.0_DP,  1.972_DP,  2.601_DP,  3.340_DP,&
   300.0_DP,  1.969_DP,  2.595_DP,  3.328_DP,&
   400.0_DP,  1.967_DP,  2.590_DP,  3.318_DP,&
   500.0_DP,  1.965_DP,  2.586_DP,  3.310_DP,&
   600.0_DP,  1.964_DP,  2.585_DP,  3.307_DP,&
   700.0_DP,  1.963_DP,  2.584_DP,  3.304_DP,&
   800.0_DP,  1.963_DP,  2.583_DP,  3.302_DP,&
   999.0_DP,  1.960_DP,  2.576_DP,  3.291_DP &
    /),(/ 35, 4 /), ORDER=(/ 2, 1/))

! ------------------ Code ----------------------------------------------

CONTAINS



   SUBROUTINE Sums(X, N, SumX, SumSqX)
   ! Calculate sums of x(i) and x^2(i)
      IMPLICIT NONE
      
      REAL(KIND=DP), DIMENSION(:), INTENT(IN) :: X             ! IN: array of value (vector)
      INTEGER, INTENT(IN)                     :: N             ! IN: numbers of values
      REAL(KIND=DP), INTENT(OUT)              :: SumX, SumSqX  ! OUT: SumX = sum of x(i)
                                                               ! OUT: SumSqX = sum of x^2(i)
      SumX = 0.0_DP; SumSqX = 0.0_DP
      
      SumX   = SUM( X(1:N) )
      SumSqX = SUM( X(1:N) * X(1:N) )

   END SUBROUTINE Sums



   FUNCTION ArithMean(X, N)
   ! Calculate the arithmetic mean.
      IMPLICIT NONE
      
      REAL(KIND=DP)                           :: ArithMean     ! OUT: arithmetical mean
      REAL(KIND=DP), DIMENSION(:), INTENT(IN) :: X             ! IN: array of value (vector)
      INTEGER, INTENT(IN)                     :: N             ! IN: numbers of values
      
      REAL(KIND=DP)                           :: SumX, SumSqX  ! SumX = sum of x(i)
                                                               ! SumSqX = sum of x^2(i)
      CALL Sums(X, N, SumX, SumSqX)
      ArithMean = SumX / REAL(N,KIND=DP)
      
   END FUNCTION ArithMean
   
   
   
   !~ FUNCTION GeoMean(X, N)
   !~ ! Calculate the geometric mean
      !~ IMPLICIT NONE
      
      !~ REAL(KIND=DP)                           :: GeoMean       ! geometric mean
      !~ REAL(KIND=DP), DIMENSION(:), INTENT(IN) :: X             ! array of value (vector)
      !~ INTEGER, INTENT(IN)                     :: N             ! numbers of values
      
      !~ INTEGER                                 :: I             ! loop variable
      !~ REAL(KIND=DP)                           :: Produkt       ! product of x(i)

      !~ Produkt = 1.0_DP
      
      !~ DO I=1, N
         !~ Produkt = Produkt * X(I)
      !~ END DO
      
      !~ GeoMean = Produkt**(1.0_DP/REAL(N, KIND=DP))
   !~ END FUNCTION GeoMean
   
   
   
   !~ FUNCTION HarMean(X, N)
   !~ ! Calculate the harmonic mean
      !~ IMPLICIT NONE
      
      !~ REAL(KIND=DP)                           :: HarMean       ! harmonic mean
      !~ REAL(KIND=DP), DIMENSION(:), INTENT(IN) :: X             ! array of value (vector)
      !~ INTEGER, INTENT(IN)                     :: N             ! numbers of values
      
      !~ INTEGER                                 :: I             ! loop variable
      !~ REAL(KIND=DP)                           :: InvSum        ! inverse sum of x(i)

      !~ InvSum = 0.0_DP
      
      !~ DO I=1, N
         !~ InvSum = InvSum + 1._DP/X(I)
      !~ END DO
      
      !~ HarMean = REAL(N, KIND=DP)/InvSum
   !~ END FUNCTION HarMean



   Function Variance(X, N)
   ! Calculate the statistical variance
      IMPLICIT NONE
      
      REAL(KIND=DP)                           :: Variance      ! OUT: statistical variance
      REAL(KIND=DP), DIMENSION(:), INTENT(IN) :: X             ! IN: array of value (vector)
      INTEGER, INTENT(IN)                     :: N             ! IN: numbers of values
      
      REAL(KIND=DP)                           :: SumX, SumSqX  ! SumX = sum of x(i)
                                                               ! SumSqX = sum of x^2(i)
      
      CALL Sums(X, N, SumX, SumSqX)
      Variance = (SumSqX - SumX*SumX/REAL(N,KIND=DP))/REAL((N-1),KIND=DP)
   END FUNCTION Variance



   FUNCTION StdDev(X, N)
   ! Calculate the standard deviation
      IMPLICIT NONE
      
      REAL(KIND=DP)                           :: StdDev        ! OUT: standard deviation
      REAL(KIND=DP), DIMENSION(:), INTENT(IN) :: X             ! IN: array of values (vector)
      INTEGER, INTENT(IN)                     :: N             ! IN: numbers of values
 
      StdDev = SQRT(Variance(X, N))
   END FUNCTION StdDev


   FUNCTION MinVar(X, N)
   ! Returns the minimum value of array X
    
    IMPLICIT NONE
    
    REAL(KIND=DP)                             :: MinVar        ! OUT: Minimum value
    INTEGER, INTENT(IN)                       :: N             ! IN: numbers of values
    REAL(KIND=DP), DIMENSION(:), INTENT(IN)   :: X             ! IN: array of values (vector)
    
    INTEGER                                   :: I             ! Loop index
    INTEGER 				      :: MinIdx = 1    ! Index of the min. value
    
    DO I=2, N
        IF (X(I) < X(MinIdx)) THEN
            MinIdx = I
        END IF
    END DO
    
    MinVar = X(MinIdx)
   
   END FUNCTION MinVar
   
   
   
   FUNCTION MaxVar(X, N)
   ! Returns the maximum value of array X
    
    IMPLICIT NONE
    
    REAL(KIND=DP)                             :: MaxVar        ! OUT: Maximum value
    INTEGER, INTENT(IN)                       :: N             ! IN: numbers of values
    REAL(KIND=DP), DIMENSION(:), INTENT(IN)   :: X             ! IN: array of values (vector)
    
    INTEGER                                   :: I             ! Loop index
    INTEGER                                   :: MaxIdx = 1    ! Index of the max. value
    
    MaxIdx = 1
    
    DO I=2, N
        IF (X(I) > X(MaxIdx)) THEN
            MaxIdx = I
        END IF
    END DO
    
    MaxVar = X(MaxIdx)
   
   END FUNCTION MaxVar
   
   
   
   FUNCTION StudentFactor(N, P)
   ! Pick the correct Student-factor out of the t-table, depentent of 
   ! the number of values (N) and the statistical security (P)
   
   ! Errors:
   ! StudentFactor = -1.0 -> Wrong statistical security, choose "lo" for 95%, "md" for 99%, "hi" for 99.9%
   ! StudentFactor = -2.0 -> Incorrect degree of freedom, has to be > 0 at least
   
      IMPLICIT NONE
      
      REAL(KIND=DP)               :: StudentFactor             ! OUT: student factor
      INTEGER, INTENT(IN)         :: N                         ! IN: numbers of values
      CHARACTER(LEN=2),INTENT(IN) :: P                         ! IN: statistical security 
                                                               !     (lo=95%, md=99%, hi=99.5%)
							       
      INTEGER                     :: F, I, J, K                ! F = stat. degree of freedom,
                                                               ! I = row of t-table
                                                               ! J = column of t-table
						                                       ! K = interpolation step
      
      F     = N - 1 ! Calculate degree of freedom
      
      ! Set the proper column of the t-table, depeWrong statistical security, choose "lo" for 95%, "md" for 99%, "hi" for 99.9%ndent of the stat. security
      SELECT CASE(P)
      CASE('lo')
        J = 2
	  CASE('md')
	    J = 3
	  CASE('hi')
	    J = 4
	  CASE DEFAULT
	    ! Wrong statistical security, choose "lo" for 95%, "md" for 99%, "hi" for 99.9%
	    StudentFactor = -1.0_DP
            RETURN
      END SELECT
      
      ! Pick the correct Student-factor out of the t-table and interpolate if necessary
      SELECT CASE(F)
      CASE(1:20)
	    StudentFactor = TTable(F,J)
	  CASE(21:50)
	    K = INT(F/5)*5
	    I = 20 + INT((F-20)/5)
	    StudentFactor = TTable(I,J)-((TTable(I,J)-TTable(I+1,J))/5.0_DP*(F-K))
	  CASE(51:100)
	    K = F - 50
	    I = 26
	    StudentFactor = TTable(I,J)-((TTable(I,J)-TTable(I+1,J))/50.0_DP*K)
	  CASE(101:800)
	    K = INT(F/100)*100
	    I = 26 + INT(F/100)
	    StudentFactor = TTable(I,J)-((TTable(I,J)-TTable(I+1,J))/100.0_DP*(F-K))	    
	  CASE(801:) ! infinite
	    StudentFactor = TTable(35,J)
	  CASE DEFAULT
            ! Incorrect degree of freedom
            StudentFactor = -2.0_DP
            RETURN
      END SELECT
      
   END FUNCTION StudentFactor
   
   
   
   FUNCTION StrayArea(X, N, P)
   ! Calculates the range of dispersion (stray area) of the values
   ! StrayArea = StdDev * StudentFactor
   
   ! Error:
   ! StrayArea < 0 -> StudentFactor is not valid
   
      IMPLICIT NONE
      
      REAL(KIND=DP)                           :: StrayArea     ! OUT: range of dispersion
      REAL(KIND=DP), DIMENSION(:), INTENT(IN) :: X             ! IN: array of value (vector)
      INTEGER, INTENT(IN)                     :: N             ! IN: number of values
      CHARACTER(LEN=2), INTENT(IN)            :: P             ! IN: statistical security
                                                               !     (lo=95%, md=99%, hi=99.5%)
                                                               
      ! Check whether StudentFactor is valid
      IF (StudentFactor(N,P) < 0._DP) THEN
         StrayArea = -1._DP
         RETURN
      END IF
      
      StrayArea = StdDev(X, N) * StudentFactor(N, P)
      
   END FUNCTION StrayArea
   
   
   FUNCTION TrustArea(X, N, P)
   ! Calculates the stray area of the arithmetical mean.
   ! TrustArea = StrayArea/SQRT(number of values)
   
   ! Error:
   ! TrustArea < 0 -> StudentFactor is not valid
   
      IMPLICIT NONE
      
      REAL(KIND=DP)              :: TrustArea                  ! OUT: stray area of the arithmetic mean
      REAL(KIND=DP), DIMENSION(:), INTENT(IN) :: X             ! IN: array of value (vector)
      INTEGER, INTENT(IN)                     :: N             ! IN: number of values
      CHARACTER(LEN=2), INTENT(IN)            :: P             ! IN: statistical security
                                                               !     (lo=95%, md=99%, hi=99.5%)

      REAL(KIND=DP)              :: StrayArea_internal         ! Internal value of StrayArea to avoid multiple invocation

      StrayArea_internal = StrayArea(X, N, P)
      
      ! Check whether StrayArea is valid
      IF (StrayArea_internal < 0._DP) THEN
         TrustArea = -1._DP
         RETURN
      END IF
      
      TrustArea = StrayArea_internal/SQRT(REAL(N,KIND=DP))   
   
   END FUNCTION TrustArea
   
END MODULE SampleStatistics
