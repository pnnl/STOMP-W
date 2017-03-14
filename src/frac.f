!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE FRAC( NCMP,XMSS,XMOL,INDX )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Converts between mole and mass fractions.
!
!     INDX - 0, convert mass fraction to mole fraction
!     INDX - 1, convert mole fraction to mass fraction
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle's Pacific Northwest Division, 1996.
!     Last Modified by MD White on August 1, 1996.
!     $Id: frac.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE CCP
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Common Blocks-----------------------------------!
!



      REAL*8 XMSS(LCMP),XMOL(LCMP)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/FRAC'
      IF( INDEX(SVN_ID(63)(1:1),'$').EQ.0 ) SVN_ID(63) =
     & '$Id: frac.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      DENOM = 0.D+0
      IF( INDX.EQ.0 ) THEN
        DO 100 I = 1,NCMP
          DENOM = DENOM + XMSS(I)/WTM(I)
  100   CONTINUE
        DO 110 I = 1,NCMP
          XMOL(I) = XMSS(I)/(WTM(I)*DENOM)
  110   CONTINUE
      ELSE
        DO 120 I = 1,NCMP
          DENOM = DENOM + XMOL(I)*WTM(I)
  120   CONTINUE
        DO 130 I = 1,NCMP
          XMSS(I) = XMOL(I)*WTM(I)/DENOM
  130   CONTINUE
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of FRAC group  ---
!
      RETURN
      END

