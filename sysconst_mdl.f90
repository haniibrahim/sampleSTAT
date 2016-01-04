MODULE SysConst
!
! ================== Prologue ==========================================
!
! Purpose:
!    Provide literal constants for single and double precision reals and 
!    integers (on IEEE 754) as well as the unit number for error message 
!    output to STDERR (here = 0).
!
!
! History:
!    Version   Programmer         Date       Description
!    -------   ----------         ---------- -----------
!    1.0       Ibrahim, Hani      2006/04/01 Orginal code
!    1.0.1     Ibrahim, Hani      2006/05/23 - Pointer bug removed from OutlierOut[Nalimov]
!                                            - Add Error code "OError" to OutlierOut[Nalimov]
!    1.0.2     Ibrahim, Hani      2006/06/15 Add OPTIONAL attributes to some args of OutlierOut [Nalimov]
!
! Processing:
!     
!
!
! Special requirements:
!     
!
! ------------------ Use Module / Include files ------------------------
! ------------------ Implicit ------------------------------------------
   IMPLICIT NONE
! ------------------ Local declarations --------------------------------
! ------------------ Constant declarations -----------------------------
   
   ! Set single (SP) and double (DP) precision reals and single (SI) and
   ! double (LI) precision integers based on IEEE 754 machines.
   
   INTEGER, PARAMETER :: SP = SELECTED_REAL_KIND(6,37)
   INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(15,307)

END MODULE SysConst