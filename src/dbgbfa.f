!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE DBGBFA( N,MU,ML,INFO )
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
!            factorization.
!
!     LINPACK User's Guide. J.J. Dongarra, C.B. Moler, J.R. Bunch, and
!     G.W. Stewart. SIAM, Philadelphia, 1979.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, March, 1993.
!     Last Modified by MD White, Battelle, PNL, April 14, 1994.
!     $Id: dbgbfa.F 1080 2017-03-14 16:22:02Z d3c002 $
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
      SUB_LOG(ISUB_LOG) = '/DBGBFA'
      IF( INDEX(SVN_ID(28)(1:1),'$').EQ.0 ) SVN_ID(28) =
     & '$Id: dbgbfa.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      M = ML + MU + 1
      INFO = 0
      ZERO = 0.D+0
!
!---  Zero initial fill-in columns  ---
!
      J0 = MU + 2
      J1 = MIN0(N,M) - 1
      IF (J1 .LT. J0) GO TO 30
      DO 20 JZ = J0, J1
         I0 = M + 1 - JZ
         DO 10 I = I0, ML
            ALU(I,JZ) = 0.D+0
   10    CONTINUE
   20 CONTINUE
   30 CONTINUE
      JZ = J1
      JU = 0
!
!---  Gaussian elimination with partial pivoting  ---
!
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 130
      DO 120 K = 1, NM1
         KP1 = K + 1
!
!---  Zero next fill-in column  ---
!
         JZ = JZ + 1
         IF (JZ .GT. N) GO TO 50
         IF (ML .LT. 1) GO TO 50
            DO 40 I = 1, ML
               ALU(I,JZ) = 0.D+0
   40       CONTINUE
   50    CONTINUE
!
!---  Find L = Pivot index  ---
!
         LM = MIN0(ML,N-K)
!
!---  Function ISAMAX  ---
!
         ISAMAX = 0
         IF( LM.LT.0 ) GOTO 58
         ISAMAX = 1
         IF( LM.EQ.0 ) GOTO 58
         SMAX = ABS(ALU(M,K))
         DO 55 I = 2,LM+1
           IF( ABS(ALU(M+I-1,K)) .LE. SMAX ) GOTO 55
           ISAMAX = I
           SMAX = ABS(ALU(M+I-1,K))
   55    CONTINUE
   58    CONTINUE
         L = ISAMAX + M - 1
         ILU(K) = L + K - M
!
!---  Zero pivot implies this column already triangularized  ---
!
         IF( ABS(ALU(L,K))/EPSL.LE.EPSL ) GOTO 100
!
!---  Interchange if necessary  ---
!
            IF( L.EQ.M ) GOTO 60
               TT = ALU(L,K)
               ALU(L,K) = ALU(M,K)
               ALU(M,K) = TT
   60       CONTINUE
!
!---  Compute multipliers  ---
!
            TT = -1.D+0/ALU(M,K)
!
!---  Function SSCAL  ---
!
            IF( LM.EQ.0 ) GOTO 68
            DO 65 I = 1,LM
              ALU(M+I,K) = TT*ALU(M+I,K)
   65       CONTINUE
   68       CONTINUE
!
!---  Row elimination with column indexing  ---
!
            JU = MIN0(MAX0(JU,MU+ILU(K)),N)
            MM = M
            IF (JU.LT.KP1) GO TO 90
            DO 80 J = KP1, JU
               L = L - 1
               MM = MM - 1
               TT = ALU(L,J)
               IF( L.EQ.MM ) GO TO 70
                  ALU(L,J) = ALU(MM,J)
                  ALU(MM,J) = TT
   70          CONTINUE
!
!---  Function SAXPY  ---
!
               IF( LM .EQ. 0 .OR. TT .EQ. ZERO ) GOTO 78
               DO 75 I = 1,LM
                 ALU(MM+I,J) = TT*ALU(M+I,K) + ALU(MM+I,J)
   75          CONTINUE
   78          CONTINUE
   80       CONTINUE
   90       CONTINUE
         GO TO 110
  100    CONTINUE
            INFO = K
  110    CONTINUE
  120 CONTINUE
  130 CONTINUE
      ILU(N) = N
      IF( ABS(ALU(M,N))/EPSL.LE.EPSL ) INFO = N
      ISUB_LOG = ISUB_LOG-1
!
!---  End of DBGBFA group
!
      RETURN
      END


