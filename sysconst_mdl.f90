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
!    1.0       Ibrahim, Hani      2006-04-01 Originalcode
!    1.1       Ibrahim, Hani      2020-11-07 Portable declaration of stdin/out/err
!
! Processing:
!
!
!
! Special requirements:
!
!
! ------------------ Use Module / Include files ------------------------
! Portable declaration of stderr, stdin, stdout
#ifdef f2003
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, &
                                          stdout=>output_unit, &
                                          stderr=>error_unit
#else
#define stdin  5
#define stdout 6
#define stderr 0
#endif
! ------------------ Implicit ------------------------------------------
   IMPLICIT NONE
! ------------------ Local declarations --------------------------------
! ------------------ Constant declarations -----------------------------

   ! Set single (SP) and double (DP) precision reals and single (SI) and
   ! double (LI) precision integers based on IEEE 754 machines.

   INTEGER, PARAMETER :: SP = SELECTED_REAL_KIND(6,37)
   INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(15,307)

END MODULE SysConst
