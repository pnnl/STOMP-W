!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE DBGBSL( N,MU,ML )
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
!     Double precision, gaussian elimination, banded matrix
!            solver.
!
!     LINPACK User's Guide. J.J. Dongarra, C.B. Moler, J.R. Bunch, and
!     G.W. Stewart. SIAM, Philadelphia, 1979.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, March, 1993.
!     Last Modified by MD White, Battelle, PNL, April 14, 1994.
!     $Id: dbgbsl.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE JACOB
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
      SUB_LOG(ISUB_LOG) = '/DBGBSL'
      IF( INDEX(SVN_ID(29)(1:1),'$').EQ.0 ) SVN_ID(29) =
     & '$Id: dbgbsl.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      M = ML + MU + 1
      NM1 = N - 1
      ZERO = 0.D+0
!
!---  Solve A * X = B
!     First solve L * Y = B  ---
!
      IF( ML.EQ.0 .OR. NM1.LT.1 ) GOTO 30
      DO 20 K = 1,NM1
        LM = MIN0( ML,N-K )
        L = ILU(K)
        TT = BLU(L)
        IF( L.EQ.K ) GOTO 10
        BLU(L) = BLU(K)
        BLU(K) = TT
   10   CONTINUE
!
!---  Function SAXPY  ---
!
        IF( LM.EQ.0 .OR. ABS(TT)/EPSL.LE.EPSL ) GOTO 18
        DO 15 I = 1,LM
          BLU(K+I) = TT*ALU(M+I,K) + BLU(K+I)
   15   CONTINUE
   18   CONTINUE
   20 CONTINUE
   30 CONTINUE
!
!---  Now solve U * X = Y  ---
!
      DO 40 KB = 1,N
        K = N + 1 - KB
        BLU(K) = BLU(K)/ALU(M,K)
        LM = MIN0( K,M ) - 1
        LA = M - LM
        LB = K - LM
        TT = -BLU(K)
!
!---  Function SAXPY  ---
!
        IF( LM.EQ.0 .OR. ABS(TT)/EPSL.LE.EPSL ) GOTO 38
        DO 35 I = 1,LM
          BLU(LB+I-1) = TT*ALU(LA+I-1,K) + BLU(LB+I-1)
   35   CONTINUE
   38   CONTINUE
   40 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of DBGBSL group  ---
!
      RETURN
      END


