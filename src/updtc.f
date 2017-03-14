!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE UPDTC(NSL)
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
!     Updates concentrations.
!
!----------------------Authors-----------------------------------------!
!
!     Written by ML Rockhold, Battelle, PNL, May 1993.
!     Last Modified by ML Rockhold, Battelle, PNL, April 14, 1994.
!     $Id: updtc.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE JACOB
      USE GRID
      USE CONST
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



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/UPDTC'
      IF( INDEX(SVN_ID(247)(1:1),'$').EQ.0 ) SVN_ID(247) =
     & '$Id: updtc.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Put the B array into the A array, skipping masked nodes  ---
!
!$OMP PARALLEL DO
!$OMP&  DEFAULT(NONE)
!$OMP&  SHARED(C,NFBN,IXP,IBR,BLU,EPSL)
!$OMP&  PRIVATE(N,NMD)
!$OMP&  FIRSTPRIVATE(NSL)
      DO 900 N = 1,NFBN
        IF( IXP(N).EQ.0 .OR. IBR(4,N).NE.N ) GOTO 900
        NMD = IXP(N)
        C(N,NSL) = BLU(NMD)
        IF( ABS(C(N,NSL)).LT.EPSL ) C(N,NSL) = 0.D+0
  900 CONTINUE
!$OMP END PARALLEL DO
!
!---  End of UPDTC group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE UPDTCO(NSL)
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
!     Updates concentrations.
!
!----------------------Authors-----------------------------------------!
!
!     Written by ML Rockhold, Battelle, PNL, May 1993.
!     Last Modified by ML Rockhold, Battelle, PNL, April 14, 1994.
!     $Id: updtc.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE GRID
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



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/UPDTCO'
      IF( INDEX(SVN_ID(247)(1:1),'$').EQ.0 ) SVN_ID(247) =
     & '$Id: updtc.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!$OMP PARALLEL DO
!$OMP&  DEFAULT(NONE)
!$OMP&  SHARED(C,CO,IXP,NFLD)
!$OMP&  PRIVATE(N)
!$OMP&  FIRSTPRIVATE(NSL)
      DO 900 N = 1,NFLD
        IF( IXP(N).LE.0 ) GOTO 900
        CO(N,NSL) = C(N,NSL)
  900 CONTINUE
!$OMP END PARALLEL DO
!
!---  End of UPDTCO group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

