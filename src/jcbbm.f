!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCBBM( RSS,RSP,RSA,N,I,J,K,MEQ )
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
!     Load the Jacobian matrix (banded matrix solver).
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle's Pacific Northwest Division, 1996.
!     Last Modified by MD White on September 5, 1996.




!     $Id: jcbbm.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE WELL_CL
      USE SOLTN
      USE JACOB
      USE GRID
      USE FDVP
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!

!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RSP(LUK),RSA(LUK,6)




!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/JCBBM'
      IF( INDEX(SVN_ID(79)(1:1),'$').EQ.0 ) SVN_ID(79) =
     & '$Id: jcbbm.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Banded solver  ---
!
      IF( ILES.EQ.1 ) THEN
!
!---    Node  ---
!
        NMD = IXP(N)
        MP = IM(MEQ,NMD)
        DO 100 M = 1,ISVC
          MCOL = IM(M,NMD)
          MROW = MP-MCOL+MDC
          ALU(MROW,MCOL) = ALU(MROW,MCOL) + (RSP(M)-RSS)/DNR(M,N)
  100   CONTINUE
        BLU(MP) = BLU(MP) - RSS
        RSDL(MEQ,N) = BLU(MP)
!
!---    Bottom ---
!
        IF( K.NE.1 ) THEN
          NB = N-IJFLD
          IF( IXP(NB).EQ.0 .OR. INBS(1,N).NE.0 ) GOTO 210
          NMD = IXP(NB)
          DO 200 M = 1,ISVC
            MCOL = IM(M,NMD)
            MROW = MP-MCOL+MDC
            ALU(MROW,MCOL) = ALU(MROW,MCOL) + (RSA(M,1)-RSS)/DNR(M,NB)
  200     CONTINUE
        ENDIF
  210 CONTINUE
!
!---    South ---
!
        IF( J.NE.1 ) THEN
          NS = N-IFLD
          IF( IXP(NS).EQ.0 .OR. INBS(2,N).NE.0 ) GOTO 310
          NMD = IXP(NS)
          DO 300 M = 1,ISVC
            MCOL = IM(M,NMD)
            MROW = MP-MCOL+MDC
            ALU(MROW,MCOL) = ALU(MROW,MCOL) + (RSA(M,2)-RSS)/DNR(M,NS)
  300     CONTINUE
        ENDIF
  310   CONTINUE
!
!---    West ---
!
        IF( I.NE.1 ) THEN
          NW = N-1
          IF( IXP(NW).EQ.0 .OR. INBS(3,N).NE.0 ) GOTO 410
          NMD = IXP(NW)
          DO 400 M = 1,ISVC
            MCOL = IM(M,NMD)
            MROW = MP-MCOL+MDC
            ALU(MROW,MCOL) = ALU(MROW,MCOL) + (RSA(M,3)-RSS)/DNR(M,NW)
  400     CONTINUE
        ENDIF
  410   CONTINUE
!
!---    East ---
!
        IF( I.NE.IFLD ) THEN
          NE = N+1
          IF( IXP(NE).EQ.0 .OR. INBS(4,N).NE.0 ) GOTO 510
          NMD = IXP(NE)
          DO 500 M = 1,ISVC
            MCOL = IM(M,NMD)
            MROW = MP-MCOL+MDC
            ALU(MROW,MCOL) = ALU(MROW,MCOL) + (RSA(M,4)-RSS)/DNR(M,NE)
  500     CONTINUE
        ENDIF
  510   CONTINUE
!
!---    North ---
!
        IF( J.NE.JFLD ) THEN
          NN = N+IFLD
          IF( IXP(NN).EQ.0 .OR. INBS(5,N).NE.0 ) GOTO 610
          NMD = IXP(NN)
          DO 600 M = 1,ISVC
            MCOL = IM(M,NMD)
            MROW = MP-MCOL+MDC
            ALU(MROW,MCOL) = ALU(MROW,MCOL) + (RSA(M,5)-RSS)/DNR(M,NN)
  600     CONTINUE
        ENDIF
  610   CONTINUE
!
!---    Top ---
!
        IF( K.NE.KFLD ) THEN
          NT = N+IJFLD
          IF( IXP(NT).EQ.0 .OR. INBS(6,N).NE.0 ) GOTO 710
          NMD = IXP(NT)
          DO 700 M = 1,ISVC
            MCOL = IM(M,NMD)
            MROW = MP-MCOL+MDC
            ALU(MROW,MCOL) = ALU(MROW,MCOL) + (RSA(M,6)-RSS)/DNR(M,NT)
  700     CONTINUE
        ENDIF
  710   CONTINUE
!
!---  SPLib or Lis solver  ---
!
      ELSEIF( ILES.EQ.3 .OR. ILES.EQ.4 ) THEN
!
!---    Node  ---
!
        NMD = IXP(N)
        MP = IM(MEQ,NMD)
        MA = 0
!
!---    Coupled well  ---
!
        IF( IXW(N).NE.0 ) MA = MA + ISVC
        DO 2100 M = 1,ISVC
          MCOL = KLU(MP,M+MA)
          DLU(MCOL) = DLU(MCOL) + (RSP(M)-RSS)/DNR(M,N)
 2100   CONTINUE
        BLU(MP) = BLU(MP) - RSS
        RSDL(MEQ,N) = BLU(MP)
        MA = MA + ISVC
!
!---    Bottom ---
!
        IF( K.NE.1 ) THEN
          NB = N-IJFLD
          IF( IXP(NB).EQ.0 .OR. INBS(1,N).NE.0 ) GOTO 2210
          NMD = IXP(NB)
          DO 2200 M = 1,ISVC
            MCOL = KLU(MP,M+MA)
            DLU(MCOL) = DLU(MCOL) + (RSA(M,1)-RSS)/DNR(M,NB)
 2200     CONTINUE
          MA = MA + ISVC
        ENDIF
 2210   CONTINUE
!
!---    South ---
!
        IF( J.NE.1 ) THEN
          NS = N-IFLD
          IF( IXP(NS).EQ.0 .OR. INBS(2,N).NE.0 ) GOTO 2310
          NMD = IXP(NS)
!
!---      Coupled well  ---
!
          IF( IXW(N).NE.0 ) MA = MA + ISVC
          DO 2300 M = 1,ISVC
            MCOL = KLU(MP,M+MA)
            DLU(MCOL) = DLU(MCOL) + (RSA(M,2)-RSS)/DNR(M,NS)
 2300     CONTINUE
          MA = MA + ISVC
        ENDIF
 2310   CONTINUE
!
!---    West ---
!
        IF( I.NE.1 ) THEN
          NW = N-1
          IF( IXP(NW).EQ.0 .OR. INBS(3,N).NE.0 ) GOTO 2410
          NMD = IXP(NW)
!
!---      Coupled well  ---
!
          IF( IXW(N).NE.0 ) MA = MA + ISVC
          DO 2400 M = 1,ISVC
            MCOL = KLU(MP,M+MA)
            DLU(MCOL) = DLU(MCOL) + (RSA(M,3)-RSS)/DNR(M,NW)
 2400     CONTINUE
          MA = MA + ISVC
        ENDIF
 2410   CONTINUE
!
!---    East ---
!
        IF( I.NE.IFLD ) THEN
          NE = N+1
          IF( IXP(NE).EQ.0 .OR. INBS(4,N).NE.0 ) GOTO 2510
          NMD = IXP(NE)
!
!---      Coupled well  ---
!
          IF( IXW(N).NE.0 ) MA = MA + 2*ISVC
          DO 2500 M = 1,ISVC
            MCOL = KLU(MP,M+MA)
            DLU(MCOL) = DLU(MCOL) + (RSA(M,4)-RSS)/DNR(M,NE)
 2500     CONTINUE
          MA = MA + ISVC
        ENDIF
 2510   CONTINUE
!
!---    North ---
!
        IF( J.NE.JFLD ) THEN
          NN = N+IFLD
          IF( IXP(NN).EQ.0 .OR. INBS(5,N).NE.0 ) GOTO 2610
          NMD = IXP(NN)
!
!---      Coupled well  ---
!
          IF( IXW(N).NE.0 ) MA = MA + 2*ISVC
          DO 2600 M = 1,ISVC
            MCOL = KLU(MP,M+MA)
            DLU(MCOL) = DLU(MCOL) + (RSA(M,5)-RSS)/DNR(M,NN)
 2600     CONTINUE
          MA = MA + ISVC
        ENDIF
 2610   CONTINUE
!
!---    Top ---
!
        IF( K.NE.KFLD ) THEN
          NT = N+IJFLD
          IF( IXP(NT).EQ.0 .OR. INBS(6,N).NE.0 ) GOTO 2710
          NMD = IXP(NT)
!
!---      Coupled well  ---
!
          IF( IXW(N).NE.0 ) MA = MA + 2*ISVC
          DO 2700 M = 1,ISVC
            MCOL = KLU(MP,M+MA)
            DLU(MCOL) = DLU(MCOL) + (RSA(M,6)-RSS)/DNR(M,NT)
 2700     CONTINUE
          MA = MA + ISVC
        ENDIF
 2710   CONTINUE

      ELSE






        INDX = 3
        CHMSG = 'Unknown Linear Equation Solver'
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCBBM group
!
      RETURN
      END
      
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCBBM_BR( RSS,RSP,RSA,N,MEQ )
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
!     Load the Jacobian matrix for the block refinement configuration.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 April 2016.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE WELL_CL
      USE SOLTN
      USE JACOB
      USE GRID
      USE FDVP
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!

!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RSP(LUK),RSA(LUK,LSTC-1)




!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/JCBBM_BR'
      IF( INDEX(SVN_ID(79)(1:1),'$').EQ.0 ) SVN_ID(79) =
     & '$Id: jcbbm.F 1080 2017-03-14 16:22:02Z d3c002 $' 

!
!---  Banded solver  ---
!
      IF( ILES.EQ.1 ) THEN
!
!---    Node  ---
!
        NMD = IXP(N)
        MP = IM(MEQ,NMD)
        DO 100 M = 1,ISVC
          MCOL = IM(M,NMD)
          MROW = MP-MCOL+MDC
          ALU(MROW,MCOL) = ALU(MROW,MCOL) + (RSP(M)-RSS)/DNR(M,N)
  100   CONTINUE
        BLU(MP) = BLU(MP) - RSS
        RSDL(MEQ,N) = BLU(MP)
!
!---    Loop over surface connections  ---
!
        MA = 1
        DO 106 ISX = 1,6
          DO 104 NCX = 1,4
            NA = ICM(NCX,ISX,N)
            IF( NA.EQ.0 ) EXIT
             NMD = IXP(NA)
             DO 102 M = 1,ISVC
              DNRX = DNR(M,NA)
              MCOL = IM(M,NMD)
              MROW = MP-MCOL+MDC
              ALU(MROW,MCOL) = ALU(MROW,MCOL) + (RSA(M,MA)-RSS)/DNRX
  102       CONTINUE
            MA = MA + 1
  104     CONTINUE
  106   CONTINUE
      ELSE
        INDX = 3
        CHMSG = 'Unknown Linear Equation Solver'
        CALL WRMSGS( INDX )
      ENDIF

      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCBBM_BR group
!
      RETURN
      END


