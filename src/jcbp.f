!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCBP
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
!     Configure the Jacobian matrix pointer arrays.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1996.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PLT_ATM
      USE GRID
      USE COUP_WELL
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
      SUB_LOG(ISUB_LOG) = '/JCBP'
      IF( INDEX(SVN_ID(95)(1:1),'$').EQ.0 ) SVN_ID(95) =
     & '$Id: jcbp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Jacobian matrix space  ---
!
      IF( (NFLD-NXP).GT.LAN ) THEN
        INDX = 12
        CHMSG = 'Number of Active Nodes > Parameter LAN: '
        IMSG = NFLD-NXP
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Coupled well model for STOMP-WA and STOMP-WO  ---
!
      IF( LWELL.EQ.1 ) THEN
        CALL JCBP_WELL
!
!---  Coupled-well model for STOMP-CO2 or STOMP-EOR  ---
!
      ELSEIF( N_CW.GT.0  ) THEN
!
!---    Dual-porosity model for STOMP-EOR  ---
!
        IF( ISLC(11).EQ.1 ) THEN
          CALL JCBP_CW_DP
!
!---    Block refinement scheme  ---
!
        ELSEIF( ISLC(33).EQ.1 ) THEN
          CALL JCBP_CW_BR
        ELSE
          CALL JCBP_CW
        ENDIF
!
!---  Surface cover model for STOMP-GT  ---
!
      ELSEIF( NSFCN.GT.0 ) THEN
        CALL JCBP_SFC
!
!---  No coupled well model  ---
!
      ELSE
!
!---    Dual-porosity modeld for STOMP-EOR  ---
!
        IF( ISLC(11).EQ.1 ) THEN
          CALL JCBP_NCW_DP
!
!---    Block refinement scheme  ---
!
        ELSEIF( ISLC(33).EQ.1 ) THEN
          CALL JCBP_NCW_BR
        ELSE
          CALL JCBP_NCW
        ENDIF
      ENDIF
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCBP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCBP_CW
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
!     Configure the Jacobian matrix pointer arrays for coupled wells
!     for STOMP-CO2.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 April 2011.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PTZR
      USE JACOB
      USE GRID
      USE COUP_WELL
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
!----------------------Type Declarations-------------------------------!
!




      INTEGER, DIMENSION(:,:), ALLOCATABLE :: M_IJK,M_JKI,M_KIJ

!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/JCBP_CW'
      IF( INDEX(SVN_ID(95)(1:1),'$').EQ.0 ) SVN_ID(95) =
     & '$Id: jcbp.F 1080 2017-03-14 16:22:02Z d3c002 $'
      ILIMIT = 2**30

!
!---  Dynamic memory allocation  ---
!
      IF( .NOT.ALLOCATED(M_IJK) ) THEN
        ALLOCATE( M_IJK(1:2,1:LN_CW),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: M_IJK'
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
      IF( .NOT.ALLOCATED(M_JKI) ) THEN
        ALLOCATE( M_JKI(1:2,1:LN_CW),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: M_JKI'
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
      IF( .NOT.ALLOCATED(M_KIJ) ) THEN
        ALLOCATE( M_KIJ(1:2,1:LN_CW),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: M_KIJ'
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF

!
!---  Coupled equations half band width using I,J,K ordering  ---
!
      NC = 0
      DO 5 K = 1,KFLD
      DO 5 J = 1,JFLD
      DO 5 I = 1,IFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 5
        NC = NC + 1
        IXP(N) = NC
    5 CONTINUE
      DO 60 NCW = 1,N_CW
        M_IJK(1,NCW) = ILIMIT
!
!---    Locate coupled well equation at the mid-point of the string
!       of field nodes that contain the well  ---
!
        IDCW = (ID_CW(5,NCW)+ID_CW(6,NCW))/2
        NC = 0
        NWF = IWF_CW(IDCW)
        MHBC_IJK = 0
        DO 20 K = 1,KFLD
        DO 20 J = 1,JFLD
        DO 20 I = 1,IFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 20
          DO 10 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
   10     CONTINUE
          IF( NWF.EQ.N ) THEN
            NC = NC+1
            JM_CWX = NC
          ENDIF
   20   CONTINUE
!
!---    Determine matrix half-band widths, using I,J,K ordering  ---
!
        DO 40 K = 1,KFLD
        DO 40 J = 1,JFLD
        DO 40 I = 1,IFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 40
!
!---      Node  ---
!
          MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              NB = ABS(IXP(N-IJFLD))
              MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NB)),
     &          ABS(IM(ISVC,NP)-IM(1,NB)))
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              NS = ABS(IXP(N-IFLD))
              MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NS)),
     &          ABS(IM(ISVC,NP)-IM(1,NS)))
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              NW = ABS(IXP(N-1))
              MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NW)),
     &          ABS(IM(ISVC,NP)-IM(1,NW)))
            ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              NE = ABS(IXP(N+1))
              MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NE)),
     &          ABS(IM(ISVC,NP)-IM(1,NE)))
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
            IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              NN = ABS(IXP(N+IFLD))
              MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NN)),
     &           ABS(IM(ISVC,NP)-IM(1,NN)))
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
            IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              NT = ABS(IXP(N+IJFLD))
              MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NT)),
     &          ABS(IM(ISVC,NP)-IM(1,NT)))
            ENDIF
          ENDIF
!
!---      Coupled well nodes  ---
!
          DO 30 JDCW = ID_CW(5,NCW),ID_CW(6,NCW)
            IF( IWF_CW(JDCW).EQ.N ) THEN
              MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-JM_CWX),
     &          ABS(IM(ISVC,NP)-JM_CWX))
            ENDIF
   30     CONTINUE
   40   CONTINUE
        IF( MHBC_IJK.LT.M_IJK(1,NCW) ) THEN
          M_IJK(1,NCW) = MHBC_IJK
          M_IJK(2,NCW) = IDCW
        ENDIF
   60 CONTINUE
!
!---  Coupled equations half band width using J,K,I ordering  ---
!
      NC = 0
      DO 65 I = 1,IFLD
      DO 65 K = 1,KFLD
      DO 65 J = 1,JFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 65
        NC = NC + 1
        IXP(N) = NC
   65 CONTINUE
      DO 120 NCW = 1,N_CW
        M_JKI(1,NCW) = ILIMIT
!
!---    Locate coupled well equation at the mid-point of the string
!       of field nodes that contain the well  ---
!
        IDCW = (ID_CW(5,NCW)+ID_CW(6,NCW))/2
        NC = 0
        NWF = IWF_CW(IDCW)
        MHBC_JKI = 0
        DO 80 I = 1,IFLD
        DO 80 K = 1,KFLD
        DO 80 J = 1,JFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 80
          DO 70 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
   70     CONTINUE
          IF( NWF.EQ.N ) THEN
            NC = NC+1
            JM_CWX = NC
          ENDIF
   80   CONTINUE
!
!---    Determine matrix half-band widths, using J,K,I ordering  ---
!
        DO 100 I = 1,IFLD
        DO 100 K = 1,KFLD
        DO 100 J = 1,JFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 100
!
!---      Node  ---
!
          MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              NB = ABS(IXP(N-IJFLD))
              MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NB)),
     &          ABS(IM(ISVC,NP)-IM(1,NB)))
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              NS = ABS(IXP(N-IFLD))
              MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NS)),
     &          ABS(IM(ISVC,NP)-IM(1,NS)))
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              NW = ABS(IXP(N-1))
              MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NW)),
     &          ABS(IM(ISVC,NP)-IM(1,NW)))
            ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              NE = ABS(IXP(N+1))
              MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NE)),
     &          ABS(IM(ISVC,NP)-IM(1,NE)))
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
            IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              NN = ABS(IXP(N+IFLD))
              MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NN)),
     &           ABS(IM(ISVC,NP)-IM(1,NN)))
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
            IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              NT = ABS(IXP(N+IJFLD))
              MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NT)),
     &          ABS(IM(ISVC,NP)-IM(1,NT)))
            ENDIF
          ENDIF
!
!---      Coupled well nodes  ---
!
          DO 90 JDCW = ID_CW(5,NCW),ID_CW(6,NCW)
            IF( IWF_CW(JDCW).EQ.N ) THEN
              MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-JM_CWX),
     &          ABS(IM(ISVC,NP)-JM_CWX))
            ENDIF
   90     CONTINUE
  100   CONTINUE
        IF( MHBC_JKI.LT.M_JKI(1,NCW) ) THEN
          M_JKI(1,NCW) = MHBC_JKI
          M_JKI(2,NCW) = IDCW
        ENDIF
  120 CONTINUE
!
!---  Coupled equations half band width using K,I,J ordering  ---
!
      NC = 0
      DO 125 J = 1,JFLD
      DO 125 I = 1,IFLD
      DO 125 K = 1,KFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 125
        NC = NC + 1
        IXP(N) = NC
  125 CONTINUE
      DO 180 NCW = 1,N_CW
        M_KIJ(1,NCW) = ILIMIT
!
!---    Locate coupled well equation at the mid-point of the string
!       of field nodes that contain the well  ---
!
        IDCW = (ID_CW(5,NCW)+ID_CW(6,NCW))/2
        NC = 0
        NWF = IWF_CW(IDCW)
        MHBC_KIJ = 0
        DO 140 J = 1,JFLD
        DO 140 I = 1,IFLD
        DO 140 K = 1,KFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 140
          DO 130 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
  130     CONTINUE
          IF( NWF.EQ.N ) THEN
            NC = NC+1
            JM_CWX = NC
          ENDIF
  140   CONTINUE
!
!---    Determine matrix half-band widths using K,I,J ordering  ---
!
        DO 160 J = 1,JFLD
        DO 160 I = 1,IFLD
        DO 160 K = 1,KFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 160
!
!---      Node  ---
!
          MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              NB = ABS(IXP(N-IJFLD))
              MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NB)),
     &          ABS(IM(ISVC,NP)-IM(1,NB)))
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              NS = ABS(IXP(N-IFLD))
              MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NS)),
     &          ABS(IM(ISVC,NP)-IM(1,NS)))
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              NW = ABS(IXP(N-1))
              MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NW)),
     &          ABS(IM(ISVC,NP)-IM(1,NW)))
            ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              NE = ABS(IXP(N+1))
              MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NE)),
     &          ABS(IM(ISVC,NP)-IM(1,NE)))
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
            IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              NN = ABS(IXP(N+IFLD))
              MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NN)),
     &           ABS(IM(ISVC,NP)-IM(1,NN)))
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
            IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              NT = ABS(IXP(N+IJFLD))
              MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NT)),
     &          ABS(IM(ISVC,NP)-IM(1,NT)))
            ENDIF
          ENDIF
!
!---      Coupled well nodes  ---
!
          DO 150 JDCW = ID_CW(5,NCW),ID_CW(6,NCW)
            IF( IWF_CW(JDCW).EQ.N ) THEN
              MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-JM_CWX),
     &          ABS(IM(ISVC,NP)-JM_CWX))
            ENDIF
  150     CONTINUE
  160   CONTINUE
        IF( MHBC_KIJ.LT.M_KIJ(1,NCW) ) THEN
          M_KIJ(1,NCW) = MHBC_KIJ
          M_KIJ(2,NCW) = IDCW
        ENDIF
  180 CONTINUE
!
!---  Loop over wells searching for the maximum half band width   ---
!
      MHBC_IJK = 0
      MHBC_JKI = 0
      MHBC_KIJ = 0
      DO 190 NCW = 1,N_CW
        IF( M_IJK(1,NCW).GT.MHBC_IJK ) MHBC_IJK = M_IJK(1,NCW)
        IF( M_JKI(1,NCW).GT.MHBC_JKI ) MHBC_JKI = M_JKI(1,NCW)
        IF( M_KIJ(1,NCW).GT.MHBC_KIJ ) MHBC_KIJ = M_KIJ(1,NCW)
  190 CONTINUE
!
!---  I,J,K ordering yields the lowest half band width   ---
!
      IF( MHBC_IJK.LE.MHBC_JKI .AND. MHBC_IJK.LE.MHBC_KIJ ) THEN
        NJD = LBD*(3*MHBC_IJK+1) + LSP
        IF( NJD.GT.LJD ) THEN
          INDX = 5
          CHMSG = 'Number of Banded Matrix Rows ' //
     &      '> Parameter LJD'
          CALL WRMSGS( INDX )
        ENDIF
        DO 200 NCW = 1,LN_CW
          ID_CW(7,NCW) = M_IJK(2,NCW)
  200   CONTINUE
!
!---    Equation ordering using I,J,K ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 205 K = 1,KFLD
        DO 205 J = 1,JFLD
        DO 205 I = 1,IFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 205
          NC = NC + 1
          IXP(N) = NC
  205   CONTINUE
!
!---    Initialize counter  ---
!
        NC = 0
        MHBC = 0
        MHBT = 0
!
!---    Coupled equations half band width using I,J,K ordering  ---
!
        DO 230 K = 1,KFLD
        DO 230 J = 1,JFLD
        DO 230 I = 1,IFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 230
          DO 210 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
  210     CONTINUE
!
!---      Loop over coupled wells, placing well equation
!         in the principal node of the well  ---
!
          DO 220 NCW = 1,N_CW
            IF( IWF_CW(ID_CW(7,NCW)).EQ.N ) THEN
              NC = NC+1
              JM_CW(NCW) = NC
            ENDIF
  220     CONTINUE
  230   CONTINUE
!
!---    Determine matrix half-band widths, using I,J,K ordering  ---
!
        DO 260 K = 1,KFLD
        DO 260 J = 1,JFLD
        DO 260 I = 1,IFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 260
!
!---      Node  ---
!
          MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              NB = IXP(N-IJFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &          ABS(IM(ISVC,NP)-IM(1,NB)))
              MHBT = MAX( MHBT,ABS(NP-NB) )
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              NS = IXP(N-IFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &          ABS(IM(ISVC,NP)-IM(1,NS)))
              MHBT = MAX( MHBT,ABS(NP-NS) )
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              NW = IXP(N-1)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &          ABS(IM(ISVC,NP)-IM(1,NW)))
              MHBT = MAX( MHBT,ABS(NP-NW) )
            ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              NE = IXP(N+1)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &          ABS(IM(ISVC,NP)-IM(1,NE)))
              MHBT = MAX( MHBT,ABS(NP-NE) )
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
            IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              NN = IXP(N+IFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &           ABS(IM(ISVC,NP)-IM(1,NN)))
              MHBT = MAX( MHBT,ABS(NP-NN) )
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
            IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              NT = IXP(N+IJFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &          ABS(IM(ISVC,NP)-IM(1,NT)))
              MHBT = MAX( MHBT,ABS(NP-NT) )
            ENDIF
          ENDIF
!
!---      Coupled well nodes  ---
!
          DO 250 NCW = 1,N_CW
            DO 240 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
              IF( IWF_CW(NWF).EQ.N ) THEN
                MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_CW(NCW)),
     &            ABS(IM(ISVC,NP)-JM_CW(NCW)))
              ENDIF
  240       CONTINUE
  250     CONTINUE
  260   CONTINUE
!
!---  J,K,I ordering yields the lowest half band width   ---
!
      ELSEIF( MHBC_JKI.LE.MHBC_KIJ ) THEN
        NJD = LBD*(3*MHBC_JKI+1) + LSP
        IF( NJD.GT.LJD ) THEN
          INDX = 5
          CHMSG = 'Number of Banded Matrix Rows ' //
     &      '> Parameter LJD'
          CALL WRMSGS( INDX )
        ENDIF
        DO 300 NCW = 1,LN_CW
          ID_CW(7,NCW) = M_JKI(2,NCW)
  300   CONTINUE
!
!---    Equation ordering using J,K,I ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 305 I = 1,IFLD
        DO 305 K = 1,KFLD
        DO 305 J = 1,JFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 305
          NC = NC + 1
          IXP(N) = NC
  305   CONTINUE
!
!---    Initialize counter  ---
!
        NC = 0
        MHBC = 0
        MHBT = 0
!
!---    Coupled equations half band width using J,K,I ordering  ---
!
        DO 330 I = 1,IFLD
        DO 330 K = 1,KFLD
        DO 330 J = 1,JFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 330
          DO 310 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
  310     CONTINUE
!
!---      Loop over coupled wells, placing well equation
!         in the principal node of the well  ---
!
          DO 320 NCW = 1,N_CW
            IF( IWF_CW(ID_CW(7,NCW)).EQ.N ) THEN
              NC = NC+1
              JM_CW(NCW) = NC
            ENDIF
  320     CONTINUE
  330   CONTINUE
!
!---    Determine matrix half-band widths, using J,K,I ordering  ---
!
        DO 360 I = 1,IFLD
        DO 360 K = 1,KFLD
        DO 360 J = 1,JFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 360
!
!---      Node  ---
!
          MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              NB = IXP(N-IJFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &          ABS(IM(ISVC,NP)-IM(1,NB)))
              MHBT = MAX( MHBT,ABS(NP-NB) )
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              NS = IXP(N-IFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &          ABS(IM(ISVC,NP)-IM(1,NS)))
              MHBT = MAX( MHBT,ABS(NP-NS) )
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              NW = IXP(N-1)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &          ABS(IM(ISVC,NP)-IM(1,NW)))
              MHBT = MAX( MHBT,ABS(NP-NW) )
            ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              NE = IXP(N+1)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &          ABS(IM(ISVC,NP)-IM(1,NE)))
              MHBT = MAX( MHBT,ABS(NP-NE) )
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
            IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              NN = IXP(N+IFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &           ABS(IM(ISVC,NP)-IM(1,NN)))
              MHBT = MAX( MHBT,ABS(NP-NN) )
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
            IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              NT = IXP(N+IJFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &          ABS(IM(ISVC,NP)-IM(1,NT)))
              MHBT = MAX( MHBT,ABS(NP-NT) )
            ENDIF
          ENDIF
!
!---      Coupled well nodes  ---
!
          DO 350 NCW = 1,N_CW
            DO 340 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
              IF( IWF_CW(NWF).EQ.N ) THEN
                MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_CW(NCW)),
     &            ABS(IM(ISVC,NP)-JM_CW(NCW)))
              ENDIF
  340       CONTINUE
  350     CONTINUE
  360   CONTINUE
!
!---  K,I,J ordering yields the lowest half band width   ---
!
      ELSE
        NJD = LBD*(3*MHBC_KIJ+1) + LSP
        IF( NJD.GT.LJD ) THEN
          INDX = 5
          CHMSG = 'Number of Banded Matrix Rows ' //
     &      '> Parameter LJD'
          CALL WRMSGS( INDX )
        ENDIF
        DO 400 NCW = 1,LN_CW
          ID_CW(7,NCW) = M_KIJ(2,NCW)
  400   CONTINUE
!
!---    Equation ordering using K,I,J ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 405 J = 1,JFLD
        DO 405 I = 1,IFLD
        DO 405 K = 1,KFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 405
          NC = NC + 1
          IXP(N) = NC
  405   CONTINUE
!
!---    Initialize counter  ---
!
        NC = 0
        MHBC = 0
        MHBT = 0
!
!---    Coupled equations half band width using K,I,J ordering  ---
!
        DO 430 J = 1,JFLD
        DO 430 I = 1,IFLD
        DO 430 K = 1,KFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 430
          DO 410 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
  410     CONTINUE
!
!---      Loop over coupled wells, placing well equation
!         in the principal node of the well  ---
!
          DO 420 NCW = 1,N_CW
            IF( IWF_CW(ID_CW(7,NCW)).EQ.N ) THEN
              NC = NC+1
              JM_CW(NCW) = NC
            ENDIF
  420     CONTINUE
  430   CONTINUE
!
!---    Determine matrix half-band widths, using K,I,J ordering  ---
!
        DO 460 J = 1,JFLD
        DO 460 I = 1,IFLD
        DO 460 K = 1,KFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 460
!
!---      Node  ---
!
          MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              NB = IXP(N-IJFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &          ABS(IM(ISVC,NP)-IM(1,NB)))
              MHBT = MAX( MHBT,ABS(NP-NB) )
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              NS = IXP(N-IFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &          ABS(IM(ISVC,NP)-IM(1,NS)))
              MHBT = MAX( MHBT,ABS(NP-NS) )
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              NW = IXP(N-1)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &          ABS(IM(ISVC,NP)-IM(1,NW)))
              MHBT = MAX( MHBT,ABS(NP-NW) )
            ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              NE = IXP(N+1)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &          ABS(IM(ISVC,NP)-IM(1,NE)))
              MHBT = MAX( MHBT,ABS(NP-NE) )
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
            IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              NN = IXP(N+IFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &           ABS(IM(ISVC,NP)-IM(1,NN)))
              MHBT = MAX( MHBT,ABS(NP-NN) )
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
            IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              NT = IXP(N+IJFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &          ABS(IM(ISVC,NP)-IM(1,NT)))
              MHBT = MAX( MHBT,ABS(NP-NT) )
            ENDIF
          ENDIF
!
!---      Coupled well nodes  ---
!
          DO 450 NCW = 1,N_CW
            DO 440 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
              IF( IWF_CW(NWF).EQ.N ) THEN
                MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_CW(NCW)),
     &            ABS(IM(ISVC,NP)-JM_CW(NCW)))
              ENDIF
  440       CONTINUE
  450     CONTINUE
  460   CONTINUE
      ENDIF
      MLC = MHBC
      MLT = ISVT*MHBT + ISVT - 1
      MUC = MHBC
      MUT = ISVT*MHBT + ISVT - 1
      MDC = MLC + MUC + 1
      MDT = MLT + MUT + 1
!
!---  SPLIB .or. Lis .or. PETSc Solver  ---
!
      IF( ILES.EQ.3 .OR. ILES.EQ.4 .OR. ILES.EQ.5 ) THEN
!
!---    X-Y Plane yields the lowest band width.
!       Load Jacobian matrix in the increment order I,J,K  ---
!
        IF( MHBC_IJK.LE.MHBC_JKI .AND. MHBC_IJK.LE.MHBC_KIJ ) THEN
          NC = 0
          NCC = 0




          NLU(1) = 1
          NLUC(1) = 1

          DO 800 K = 1,KFLD
          DO 800 J = 1,JFLD
          DO 800 I = 1,IFLD
            N = ND(I,J,K)
            IF( IXP(N).EQ.0 ) CYCLE
            NP = IXP(N)
            DO 660 L = 1,ISVC
              NMD = IM(L,NP)
              MA = 0
!
!---          Node  ---
!
              DO 500 M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM(M,NP)

                KLU(NMD,M+MA) = NC

  500         CONTINUE
              MA = MA + ISVC
!
!---          Bottom node neighbors  ---
!
              DO 512 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                DO 510 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NB)

                  KLU(NMD,M+MA) = NC

  510           CONTINUE
                MA = MA + ISVC
  512         CONTINUE
!
!---          South node neighbors  ---
!
              DO 522 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                DO 520 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NS)

                  KLU(NMD,M+MA) = NC

  520           CONTINUE
                MA = MA + ISVC
  522         CONTINUE
!
!---          West node neighbors  ---
!
              DO 532 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                DO 530 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NW)

                  KLU(NMD,M+MA) = NC

  530           CONTINUE
                MA = MA + ISVC
  532         CONTINUE
!
!---          East node neighbors  ---
!
              DO 542 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                DO 540 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NE)

                  KLU(NMD,M+MA) = NC

  540           CONTINUE
                MA = MA + ISVC
  542         CONTINUE
!
!---          North node neighbors  ---
!
              DO 552 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                DO 550 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NN)

                  KLU(NMD,M+MA) = NC

  550           CONTINUE
                MA = MA + ISVC
  552         CONTINUE
!
!---          Top node neighbors  ---
!
              DO 562 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                DO 560 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NT)

                  KLU(NMD,M+MA) = NC

  560           CONTINUE
                MA = MA + ISVC
  562         CONTINUE
!
!---          Node contains a coupled well node,
!             include couple well equations in nodal equations  ---
!
              DO 650 NCW = 1,N_CW
                NWFX = ID_CW(6,NCW)-ID_CW(5,NCW)+1
                DO 640 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                  IF( IWF_CW(NWF).EQ.N ) THEN
                    MA = (NWFX*ISVC) + (NWF-ID_CW(5,NCW))*ISVC + 1
                    NC = NC+1




                    MLU(NC) = JM_CW(NCW)

                    KLU_CW(L+MA,NCW) = NC

                  ENDIF
  640           CONTINUE
  650         CONTINUE



              NLU(NMD+1) = NC+1

  660       CONTINUE
!
!---        Loop over coupled wells  ---
!
            DO 668 NCW = 1,N_CW
!
!---          Node contains principal coupled well node,
!             include coupled well equation  ---
!
              IF( IWF_CW(ID_CW(7,NCW)).EQ.N ) THEN
                NC = NC+1
                NMD = JM_CW(NCW)




                MLU(NC) = NMD

                KLU_CW(1,NCW) = NC

!
!---            Loop over nodes that contain coupled well nodes
!               include nodal equations in coupled well equation  ---
!
                DO 664 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                  NPX = IXP(IWF_CW(NWF))
                  MA = (NWF-ID_CW(5,NCW))*ISVC + 1
                  DO 662 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NPX)

                    KLU_CW(M+MA,NCW) = NC

  662             CONTINUE
  664           CONTINUE



                NLU(NMD+1) = NC+1

              ENDIF
  668       CONTINUE
!
!---        Solute transport equations  ---
!
            MA = 1
!
!---        Node  ---
!
            NCC = NCC+1




            MLUC(NCC) = NP

            KLUC(NP,MA) = NCC

            MA = MA + 1
!
!---        Bottom node neighbors  ---
!
            DO 702 MX = 1,4
              NB = ICM(MX,1,N)
              IF( NB.EQ.0 ) EXIT
              NB = IXP(NB)
              NCC = NCC+1




              MLUC(NCC) = NB

              KLUC(NP,MA) = NCC

              MA = MA + 1
  702       CONTINUE
!
!---        South node neighbors  ---
!
            DO 712 MX = 1,4
              NS = ICM(MX,2,N)
              IF( NS.EQ.0 ) EXIT
              NS = IXP(NS)
              NCC = NCC+1




              MLUC(NCC) = NS

              KLUC(NP,MA) = NCC

              MA = MA + 1
  712       CONTINUE
!
!---        West node neighbors  ---
!
            DO 722 MX = 1,4
              NW = ICM(MX,3,N)
              IF( NW.EQ.0 ) EXIT
              NW = IXP(NW)
              NCC = NCC+1




              MLUC(NCC) = NW

              KLUC(NP,MA) = NCC

              MA = MA + 1
  722       CONTINUE
!
!---        East node neighbors  ---
!
            DO 732 MX = 1,4
              NE = ICM(MX,4,N)
              IF( NE.EQ.0 ) EXIT
              NE = IXP(NE)
              NCC = NCC+1




              MLUC(NCC) = NE

              KLUC(NP,MA) = NCC

              MA = MA + 1
  732       CONTINUE
!
!---        North node neighbors  ---
!
            DO 742 MX = 1,4
              NN = ICM(MX,5,N)
              IF( NN.EQ.0 ) EXIT
              NN = IXP(NN)
              NCC = NCC+1




              MLUC(NCC) = NN

              KLUC(NP,MA) = NCC

              MA = MA + 1
  742       CONTINUE
!
!---        Top node neighbors  ---
!
            DO 752 MX = 1,4
              NT = ICM(MX,6,N)
              IF( NT.EQ.0 ) EXIT
              NT = IXP(NT)
              NCC = NCC+1




              MLUC(NCC) = NT

              KLUC(NP,MA) = NCC

              MA = MA + 1
  752       CONTINUE




            NLUC(NP+1) = NCC+1


  800     CONTINUE
          MKC = NC
          MKT = NCC
!
!---    Y-Z Plane yields the lowest band width.
!       Load Jacobian matrix in the increment order J,K,I  ---
!
        ELSEIF( MHBC_JKI.LE.MHBC_KIJ ) THEN
          NC = 0
          NCC = 0




          NLU(1) = 1
          NLUC(1) = 1

          DO 1800 I = 1,IFLD
          DO 1800 K = 1,KFLD
          DO 1800 J = 1,JFLD
            N = ND(I,J,K)
            IF( IXP(N).EQ.0 ) CYCLE
            NP = IXP(N)
            DO 1660 L = 1,ISVC
              NMD = IM(L,NP)
              MA = 0
!
!---          Node  ---
!
              DO 1500 M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM(M,NP)

                KLU(NMD,M+MA) = NC

 1500         CONTINUE
              MA = MA + ISVC
!
!---          Bottom node neighbors  ---
!
              DO 1512 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                DO 1510 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NB)

                  KLU(NMD,M+MA) = NC

 1510           CONTINUE
                MA = MA + ISVC
 1512         CONTINUE
!
!---          South node neighbors  ---
!
              DO 1522 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                DO 1520 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NS)

                  KLU(NMD,M+MA) = NC

 1520           CONTINUE
                MA = MA + ISVC
 1522         CONTINUE
!
!---          West node neighbors  ---
!
              DO 1532 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                DO 1530 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NW)

                  KLU(NMD,M+MA) = NC

 1530           CONTINUE
                MA = MA + ISVC
 1532           CONTINUE
!
!---          East node neighbors  ---
!
              DO 1542 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                DO 1540 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NE)

                  KLU(NMD,M+MA) = NC

 1540           CONTINUE
                MA = MA + ISVC
 1542         CONTINUE
!
!---          North node neighbors  ---
!
              DO 1552 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                DO 1550 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NN)

                  KLU(NMD,M+MA) = NC

 1550           CONTINUE
                MA = MA + ISVC
 1552         CONTINUE
!
!---          Top node neighbors  ---
!
              DO 1562 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                DO 1560 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NT)

                  KLU(NMD,M+MA) = NC

 1560           CONTINUE
                MA = MA + ISVC
 1562         CONTINUE
!
!---          Node contains a coupled well node,
!             include couple well equations in nodal equations  ---
!
              DO 1650 NCW = 1,N_CW
                NWFX = ID_CW(6,NCW)-ID_CW(5,NCW)+1
                DO 1640 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                  IF( IWF_CW(NWF).EQ.N ) THEN
                    MA = (NWFX*ISVC) + (NWF-ID_CW(5,NCW))*ISVC + 1
                    NC = NC+1




                    MLU(NC) = JM_CW(NCW)

                    KLU_CW(L+MA,NCW) = NC

                  ENDIF
 1640           CONTINUE
 1650         CONTINUE



              NLU(NMD+1) = NC+1

 1660       CONTINUE
!
!---        Loop over coupled wells  ---
!
            DO 1668 NCW = 1,N_CW
!
!---          Node contains principal coupled well node,
!             include coupled well equation  ---
!
              IF( IWF_CW(ID_CW(7,NCW)).EQ.N ) THEN
                NC = NC+1
                NMD = JM_CW(NCW)




                MLU(NC) = NMD

                KLU_CW(1,NCW) = NC

!
!---            Loop over nodes that contain coupled well nodes
!               include nodal equations in coupled well equation  ---
!
                DO 1664 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                  NPX = IXP(IWF_CW(NWF))
                  MA = (NWF-ID_CW(5,NCW))*ISVC + 1
                  DO 1662 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NPX)

                    KLU_CW(M+MA,NCW) = NC

 1662             CONTINUE
 1664           CONTINUE



                NLU(NMD+1) = NC+1

              ENDIF
 1668       CONTINUE
!
!---        Solute transport equations  ---
!
            MA = 1
!
!---        Node  ---
!
            NCC = NCC+1




            MLUC(NCC) = NP

            KLUC(NP,MA) = NCC

            MA = MA + 1
!
!---        Bottom node neighbors  ---
!
            DO 1702 MX = 1,4
              NB = ICM(MX,1,N)
              IF( NB.EQ.0 ) EXIT
              NB = IXP(NB)
              NCC = NCC+1




              MLUC(NCC) = NB

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1702       CONTINUE
!
!---        South node neighbors  ---
!
            DO 1712 MX = 1,4
              NS = ICM(MX,2,N)
              IF( NS.EQ.0 ) EXIT
              NS = IXP(NS)
              NCC = NCC+1




              MLUC(NCC) = NS

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1712       CONTINUE
!
!---        West node neighbors  ---
!
            DO 1722 MX = 1,4
              NW = ICM(MX,3,N)
              IF( NW.EQ.0 ) EXIT
              NW = IXP(NW)
              NCC = NCC+1




              MLUC(NCC) = NW

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1722       CONTINUE
!
!---        East node neighbors  ---
!
            DO 1732 MX = 1,4
              NE = ICM(MX,4,N)
              IF( NE.EQ.0 ) EXIT
              NE = IXP(NE)
              NCC = NCC+1




              MLUC(NCC) = NE

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1732       CONTINUE
!
!---        North node neighbors  ---
!
            DO 1742 MX = 1,4
              NN = ICM(MX,5,N)
              IF( NN.EQ.0 ) EXIT
              NN = IXP(NN)
              NCC = NCC+1




              MLUC(NCC) = NN

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1742       CONTINUE
!
!---        Top node neighbors  ---
!
            DO 1752 MX = 1,4
              NT = ICM(MX,6,N)
              IF( NT.EQ.0 ) EXIT
              NT = IXP(NT)
              NCC = NCC+1




              MLUC(NCC) = NT

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1752       CONTINUE




            NLUC(NP+1) = NCC+1


 1800     CONTINUE
          MKC = NC
          MKT = NCC
!
!---    Z-X Plane yields the lowest band width.
!       Load Jacobian matrix in the increment order K,I,J  ---
!
        ELSE
          NC = 0
          NCC = 0




          NLU(1) = 1
          NLUC(1) = 1

          DO 2800 J = 1,JFLD
          DO 2800 I = 1,IFLD
          DO 2800 K = 1,KFLD
            N = ND(I,J,K)
            IF( IXP(N).EQ.0 ) CYCLE
            NP = IXP(N)
            DO 2660 L = 1,ISVC
              NMD = IM(L,NP)
              MA = 0
!
!---          Node  ---
!
              DO 2500 M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM(M,NP)

                KLU(NMD,M+MA) = NC

 2500         CONTINUE
              MA = MA + ISVC
!
!---          Bottom node neighbors  ---
!
              DO 2512 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                DO 2510 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NB)

                  KLU(NMD,M+MA) = NC

 2510           CONTINUE
                MA = MA + ISVC
 2512         CONTINUE
!
!---          South node  ---
!
              DO 2522 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                DO 2520 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NS)

                  KLU(NMD,M+MA) = NC

 2520           CONTINUE
                MA = MA + ISVC
 2522         CONTINUE
!
!---          West node  ---
!
              DO 2532 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                DO 2530 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NW)

                  KLU(NMD,M+MA) = NC

 2530           CONTINUE
                MA = MA + ISVC
 2532         CONTINUE
!
!---          East node  ---
!
              DO 2542 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                DO 2540 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NE)

                  KLU(NMD,M+MA) = NC

 2540           CONTINUE
                MA = MA + ISVC
 2542         CONTINUE
!
!---          North node  ---
!
              DO 2552 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                DO 2550 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NN)

                  KLU(NMD,M+MA) = NC

 2550           CONTINUE
                MA = MA + ISVC
 2552         CONTINUE
!
!---          Top node  ---
!
              DO 2562 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                DO 2560 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NT)

                  KLU(NMD,M+MA) = NC

 2560           CONTINUE
                MA = MA + ISVC
 2562         CONTINUE
!
!---          Node contains a coupled well node,
!             include couple well equations in nodal equations  ---
!
              DO 2650 NCW = 1,N_CW
                NWFX = ID_CW(6,NCW)-ID_CW(5,NCW)+1
                DO 2640 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                  IF( IWF_CW(NWF).EQ.N ) THEN
                    MA = (NWFX*ISVC) + (NWF-ID_CW(5,NCW))*ISVC + 1
                    NC = NC+1




                    MLU(NC) = JM_CW(NCW)

                    KLU_CW(L+MA,NCW) = NC

                  ENDIF
 2640           CONTINUE
 2650         CONTINUE



              NLU(NMD+1) = NC+1

 2660       CONTINUE
!
!---        Loop over coupled wells  ---
!
            DO 2668 NCW = 1,N_CW
!
!---          Node contains principal coupled well node,
!             include coupled well equation  ---
!
              IF( IWF_CW(ID_CW(7,NCW)).EQ.N ) THEN
                NC = NC+1
                NMD = JM_CW(NCW)




                MLU(NC) = NMD

                KLU_CW(1,NCW) = NC

!
!---            Loop over nodes that contain coupled well nodes
!               include nodal equations in coupled well equation  ---
!
                DO 2664 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                  NPX = IXP(IWF_CW(NWF))
                  MA = (NWF-ID_CW(5,NCW))*ISVC + 1
                  DO 2662 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NPX)

                    KLU_CW(M+MA,NCW) = NC

 2662             CONTINUE
 2664           CONTINUE



                NLU(NMD+1) = NC+1

              ENDIF
 2668       CONTINUE
!
!---        Solute transport equations  ---
!
            MA = 1
!
!---        Node  ---
!
            NCC = NCC+1




            MLUC(NCC) = NP

            KLUC(NP,MA) = NCC

            MA = MA + 1
!
!---        Bottom node neighbors  ---
!
            DO 2702 MX = 1,4
              NB = ICM(MX,1,N)
              IF( NB.EQ.0 ) EXIT
              NB = IXP(NB)
              NCC = NCC+1




              MLUC(NCC) = NB

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2702       CONTINUE
!
!---        South node neighbors  ---
!
            DO 2712 MX = 1,4
              NS = ICM(MX,2,N)
              IF( NS.EQ.0 ) EXIT
              NS = IXP(NS)
              NCC = NCC+1




              MLUC(NCC) = NS

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2712       CONTINUE
!
!---        West node neighbors  ---
!
            DO 2722 MX = 1,4
              NW = ICM(MX,3,N)
              IF( NW.EQ.0 ) EXIT
              NW = IXP(NW)
              NCC = NCC+1




              MLUC(NCC) = NW

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2722       CONTINUE
!
!---        East node neighbors  ---
!
            DO 2732 MX = 1,4
              NE = ICM(MX,4,N)
              IF( NE.EQ.0 ) EXIT
              NE = IXP(NE)
              NCC = NCC+1




              MLUC(NCC) = NE

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2732       CONTINUE
!
!---        North node neighbors  ---
!
            DO 2742 MX = 1,4
              NN = ICM(MX,5,N)
              IF( NN.EQ.0 ) EXIT
              NN = IXP(NN)
              NCC = NCC+1




              MLUC(NCC) = NN

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2742       CONTINUE
!
!---        Top node neighbors  ---
!
            DO 2752 MX = 1,4
              NT = ICM(MX,6,N)
              IF( NT.EQ.0 ) EXIT
              NT = IXP(NT)
              NCC = NCC+1




              MLUC(NCC) = NT

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2752       CONTINUE




            NLUC(NP+1) = NCC+1


 2800     CONTINUE
          MKC = NC
          MKT = NCC
        ENDIF
      ENDIF
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCBP_CW group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCBP_CW_BR
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
!     Configure the Jacobian matrix pointer arrays for coupled wells
!     with block refinement.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 April 2016.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PTZR
      USE JACOB
      USE GRID
      USE COUP_WELL
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
!----------------------Type Declarations-------------------------------!
!




      INTEGER, DIMENSION(:,:), ALLOCATABLE :: M_IJK,M_JKI,M_KIJ

!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/JCBP_CW_BR'
      IF( INDEX(SVN_ID(95)(1:1),'$').EQ.0 ) SVN_ID(95) =
     & '$Id: jcbp.F 1080 2017-03-14 16:22:02Z d3c002 $'
      ILIMIT = 2**30

!
!---  Dynamic memory allocation  ---
!
      IF( .NOT.ALLOCATED(M_IJK) ) THEN
        ALLOCATE( M_IJK(1:2,1:LN_CW),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: M_IJK'
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
      IF( .NOT.ALLOCATED(M_JKI) ) THEN
        ALLOCATE( M_JKI(1:2,1:LN_CW),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: M_JKI'
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
      IF( .NOT.ALLOCATED(M_KIJ) ) THEN
        ALLOCATE( M_KIJ(1:2,1:LN_CW),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: M_KIJ'
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF

!
!---  Coupled equations half band width using I,J,K ordering  ---
!
      NC = 0
      DO 10 K = 1,KFLD
      DO 10 J = 1,JFLD
      DO 10 I = 1,IFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 10
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          IXP(N) = -IRX*JRX*KRX
          DO 2 KX = 1,KRX
          DO 2 JX = 1,JRX
          DO 2 IX = 1,IRX
            N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
            NC = NC + 1
            IXP(N) = NC
    2     CONTINUE
        ELSE
          NC = NC + 1
          IXP(N) = NC
        ENDIF
   10 CONTINUE
!
!---  Loop over coupled wells  ---
!
      DO 60 NCW = 1,N_CW
        M_IJK(1,NCW) = ILIMIT
!
!---    Locate coupled well equation at the mid-point of the string
!       of field nodes that contain the well  ---
!
        IDCW = (ID_CW(5,NCW)+ID_CW(6,NCW))/2
        NC = 0
        NWF = IWF_CW(IDCW)
        MHBC_IJK = 0
        DO 20 K = 1,KFLD
        DO 20 J = 1,JFLD
        DO 20 I = 1,IFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
          IF( IXP(N).EQ.0 ) GOTO 20
!
!---       Loop over block refined nodes  ---
!
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 14 KX = 1,KRX
            DO 14 JX = 1,JRX
            DO 14 IX = 1,IRX
              N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
              NMD = ABS(IXP(N))
              DO 12 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
   12         CONTINUE
              IF( NWF.EQ.N ) THEN
                NC = NC+1
                JM_CWX = NC
              ENDIF
   14       CONTINUE
          ELSE
            DO 16 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
   16       CONTINUE
            IF( NWF.EQ.N ) THEN
              NC = NC+1
              JM_CWX = NC
            ENDIF
          ENDIF
   20   CONTINUE
!
!---    Determine matrix half-band widths, using I,J,K ordering  ---
!
        DO 40 K = 1,KFLD
        DO 40 J = 1,JFLD
        DO 40 I = 1,IFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
          IF( IXP(N).EQ.0 ) GOTO 40
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 32 KX = 1,KRX
          DO 32 JX = 1,JRX
          DO 32 IX = 1,IRX
            N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
            NP = ABS(IXP(N))
!
!---        Node  ---
!
            MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
                DO 21 MX = 1,4
                  NB = ICM(MX,1,N)
                  IF( NB.EQ.0 ) EXIT
                  NB = ABS(IXP(NB))
                  MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
   21           CONTINUE
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
                DO 22 MX = 1,4
                  NS = ICM(MX,2,N)
                  IF( NS.EQ.0 ) EXIT
                  NS = ABS(IXP(NS))
                  MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
   22           CONTINUE
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
                DO 23 MX = 1,4
                  NW = ICM(MX,3,N)
                  IF( NW.EQ.0 ) EXIT
                  NW = ABS(IXP(NW))
                  MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
   23           CONTINUE
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.IFLD ) THEN
                DO 24 MX = 1,4
                  NE = ICM(MX,4,N)
                  IF( NE.EQ.0 ) EXIT
                  NE = ABS(IXP(NE))
                  MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
   24           CONTINUE
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.JFLD ) THEN
                DO 25 MX = 1,4
                  NN = ICM(MX,5,N)
                  IF( NN.EQ.0 ) EXIT
                  NN = ABS(IXP(NN))
                  MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NN)),
     &              ABS(IM(ISVC,NP)-IM(1,NN)))
   25           CONTINUE
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.KFLD ) THEN
                DO 26 MX = 1,4
                  NT = ICM(MX,6,N)
                  IF( NT.EQ.0 ) EXIT
                  NT = ABS(IXP(NT))
                  MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
   26           CONTINUE
            ENDIF
!
!---        Coupled well nodes  ---
!
            DO 30 JDCW = ID_CW(5,NCW),ID_CW(6,NCW)
              IF( IWF_CW(JDCW).EQ.N ) THEN
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-JM_CWX),
     &            ABS(IM(ISVC,NP)-JM_CWX))
              ENDIF
   30       CONTINUE
   32     CONTINUE
   40   CONTINUE
        IF( MHBC_IJK.LT.M_IJK(1,NCW) ) THEN
          M_IJK(1,NCW) = MHBC_IJK
          M_IJK(2,NCW) = IDCW
        ENDIF
   60 CONTINUE
!
!---  Coupled equations half band width using J,K,I ordering  ---
!
      NC = 0
      DO 70 I = 1,IFLD
      DO 70 K = 1,KFLD
      DO 70 J = 1,JFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 70
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          IXP(N) = -IRX*JRX*KRX
          DO 62 IX = 1,IRX
          DO 62 KX = 1,KRX
          DO 62 JX = 1,JRX
            N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
            NC = NC + 1
            IXP(N) = NC
   62     CONTINUE
        ELSE
          NC = NC + 1
          IXP(N) = NC
        ENDIF
   70 CONTINUE
!
!---  Loop over coupled wells  ---
!
      DO 120 NCW = 1,N_CW
        M_JKI(1,NCW) = ILIMIT
!
!---    Locate coupled well equation at the mid-point of the string
!       of field nodes that contain the well  ---
!
        IDCW = (ID_CW(5,NCW)+ID_CW(6,NCW))/2
        NC = 0
        NWF = IWF_CW(IDCW)
        MHBC_JKI = 0
        DO 80 I = 1,IFLD
        DO 80 K = 1,KFLD
        DO 80 J = 1,JFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
          IF( IXP(N).EQ.0 ) GOTO 80
!
!---       Loop over block refined nodes  ---
!
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 74 IX = 1,IRX
            DO 74 KX = 1,KRX
            DO 74 JX = 1,JRX
              N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
              NMD = ABS(IXP(N))
              DO 72 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
   72         CONTINUE
              IF( NWF.EQ.N ) THEN
                NC = NC+1
                JM_CWX = NC
              ENDIF
   74       CONTINUE
          ELSE
            DO 76 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
   76       CONTINUE
            IF( NWF.EQ.N ) THEN
              NC = NC+1
              JM_CWX = NC
            ENDIF
          ENDIF
   80   CONTINUE
!
!---    Determine matrix half-band widths, using J,K,I ordering  ---
!
        DO 100 I = 1,IFLD
        DO 100 K = 1,KFLD
        DO 100 J = 1,JFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
          IF( IXP(N).EQ.0 ) GOTO 100
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 92 IX = 1,IRX
          DO 92 KX = 1,KRX
          DO 92 JX = 1,JRX
            N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
            NP = ABS(IXP(N))
!
!---        Node  ---
!
            MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
                DO 81 MX = 1,4
                  NB = ICM(MX,1,N)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
   81           CONTINUE
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
                DO 82 MX = 1,4
                  NS = ICM(MX,2,N)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
   82           CONTINUE
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
                DO 83 MX = 1,4
                  NW = ICM(MX,3,N)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
   83           CONTINUE
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.IFLD ) THEN
                DO 84 MX = 1,4
                  NE = ICM(MX,4,N)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
   84           CONTINUE
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.JFLD ) THEN
                DO 85 MX = 1,4
                  NN = ICM(MX,5,N)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NN)),
     &              ABS(IM(ISVC,NP)-IM(1,NN)))
   85           CONTINUE
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.KFLD ) THEN
                DO 86 MX = 1,4
                  NT = ICM(MX,6,N)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
   86           CONTINUE
            ENDIF
!
!---        Coupled well nodes  ---
!
            DO 90 JDCW = ID_CW(5,NCW),ID_CW(6,NCW)
              IF( IWF_CW(JDCW).EQ.N ) THEN
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-JM_CWX),
     &            ABS(IM(ISVC,NP)-JM_CWX))
              ENDIF
   90       CONTINUE
   92     CONTINUE
  100   CONTINUE
        IF( MHBC_JKI.LT.M_JKI(1,NCW) ) THEN
          M_JKI(1,NCW) = MHBC_JKI
          M_JKI(2,NCW) = IDCW
        ENDIF
  120 CONTINUE
!
!---  Coupled equations half band width using K,I,J ordering  ---
!
      NC = 0
      DO 130 J = 1,JFLD
      DO 130 I = 1,IFLD
      DO 130 K = 1,KFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 130
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          IXP(N) = -IRX*JRX*KRX
          DO 122 JX = 1,JRX
          DO 122 IX = 1,IRX
          DO 122 KX = 1,KRX
            N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
            NC = NC + 1
            IXP(N) = NC
  122     CONTINUE
        ELSE
          NC = NC + 1
          IXP(N) = NC
        ENDIF
  130 CONTINUE
!
!---  Loop over coupled wells  ---
!
      DO 180 NCW = 1,N_CW
        M_KIJ(1,NCW) = ILIMIT
!
!---    Locate coupled well equation at the mid-point of the string
!       of field nodes that contain the well  ---
!
        IDCW = (ID_CW(5,NCW)+ID_CW(6,NCW))/2
        NC = 0
        NWF = IWF_CW(IDCW)
        MHBC_KIJ = 0
        DO 140 J = 1,JFLD
        DO 140 I = 1,IFLD
        DO 140 K = 1,KFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
          IF( IXP(N).EQ.0 ) GOTO 140
!
!---       Loop over block refined nodes  ---
!
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 134 JX = 1,JRX
            DO 134 IX = 1,IRX
            DO 134 KX = 1,KRX
              N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
              DO 132 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  132         CONTINUE
              IF( NWF.EQ.N ) THEN
                NC = NC+1
                JM_CWX = NC
              ENDIF
  134       CONTINUE
          ELSE
            DO 136 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  136       CONTINUE
            IF( NWF.EQ.N ) THEN
              NC = NC+1
              JM_CWX = NC
            ENDIF
          ENDIF
  140   CONTINUE
!
!---    Determine matrix half-band widths, using K,I,J ordering  ---
!
        DO 160 J = 1,JFLD
        DO 160 I = 1,IFLD
        DO 160 K = 1,KFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
          IF( IXP(N).EQ.0 ) GOTO 160
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 152 IX = 1,IRX
          DO 152 KX = 1,KRX
          DO 152 JX = 1,JRX
            N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
            NP = ABS(IXP(N))
!
!---        Node  ---
!
            MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
                DO 141 MX = 1,4
                  NB = ICM(MX,1,N)
                  IF( NB.EQ.0 ) EXIT
                  NB = ABS(IXP(NB))
                  MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
  141           CONTINUE
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
                DO 142 MX = 1,4
                  NS = ICM(MX,2,N)
                  IF( NS.EQ.0 ) EXIT
                  NS = ABS(IXP(NS))
                  MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
  142           CONTINUE
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
                DO 143 MX = 1,4
                  NW = ICM(MX,3,N)
                  IF( NW.EQ.0 ) EXIT
                  NW = ABS(IXP(NW))
                  MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
  143           CONTINUE
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.IFLD ) THEN
                DO 144 MX = 1,4
                  NE = ICM(MX,4,N)
                  IF( NE.EQ.0 ) EXIT
                  NE = ABS(IXP(NE))
                  MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
  144           CONTINUE
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.JFLD ) THEN
                DO 145 MX = 1,4
                  NN = ICM(MX,5,N)
                  IF( NN.EQ.0 ) EXIT
                  NN = ABS(IXP(NN))
                  MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NN)),
     &              ABS(IM(ISVC,NP)-IM(1,NN)))
  145           CONTINUE
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.KFLD ) THEN
                DO 146 MX = 1,4
                  NT = ICM(MX,6,N)
                  IF( NT.EQ.0 ) EXIT
                  NT = ABS(IXP(NT))
                  MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
  146           CONTINUE
            ENDIF
            DO 150 JDCW = ID_CW(5,NCW),ID_CW(6,NCW)
              IF( IWF_CW(JDCW).EQ.N ) THEN
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-JM_CWX),
     &            ABS(IM(ISVC,NP)-JM_CWX))
              ENDIF
  150       CONTINUE
  152     CONTINUE
  160   CONTINUE
        IF( MHBC_KIJ.LT.M_KIJ(1,NCW) ) THEN
          M_KIJ(1,NCW) = MHBC_KIJ
          M_KIJ(2,NCW) = IDCW
        ENDIF
  180 CONTINUE
!
!---  Loop over wells searching for the maximum half band width   ---
!
      MHBC_IJK = 0
      MHBC_JKI = 0
      MHBC_KIJ = 0
      DO 190 NCW = 1,N_CW
        IF( M_IJK(1,NCW).GT.MHBC_IJK ) MHBC_IJK = M_IJK(1,NCW)
        IF( M_JKI(1,NCW).GT.MHBC_JKI ) MHBC_JKI = M_JKI(1,NCW)
        IF( M_KIJ(1,NCW).GT.MHBC_KIJ ) MHBC_KIJ = M_KIJ(1,NCW)
  190 CONTINUE








!
!---  I,J,K ordering yields the lowest half band width or PETSc 
!     solver   ---
!
      IF( MHBC_IJK.LE.MHBC_JKI .AND. MHBC_IJK.LE.MHBC_KIJ ) THEN
        NJD = LBD*(3*MHBC_IJK+1) + LSP
        IF( NJD.GT.LJD ) THEN
          INDX = 5
          CHMSG = 'Number of Banded Matrix Rows ' //
     &      '> Parameter LJD'
          CALL WRMSGS( INDX )
        ENDIF
        DO 200 NCW = 1,LN_CW
          ID_CW(7,NCW) = M_IJK(2,NCW)
  200   CONTINUE
!
!---    Equation ordering using I,J,K ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 210 K = 1,KFLD
        DO 210 J = 1,JFLD
        DO 210 I = 1,IFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 210
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            IXP(N) = -IRX*JRX*KRX
            DO 202 KX = 1,KRX
            DO 202 JX = 1,JRX
            DO 202 IX = 1,IRX
              N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
              NC = NC + 1
              IXP(N) = NC
  202       CONTINUE
          ELSE
            NC = NC + 1
            IXP(N) = NC
          ENDIF
  210   CONTINUE
!
!---    Initialize counter  ---
!
        NC = 0
        MHBC = 0
        MHBT = 0
!
!---    Coupled equations half band width using I,J,K ordering  ---
!
        DO 230 K = 1,KFLD
        DO 230 J = 1,JFLD
        DO 230 I = 1,IFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
          IF( IXP(N).EQ.0 ) GOTO 230
!
!---      Loop over block refined nodes  ---
!
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 216 KX = 1,KRX
            DO 216 JX = 1,JRX
            DO 216 IX = 1,IRX
              N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
              NMD = ABS(IXP(N))
              DO 212 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  212         CONTINUE
!
!---          Loop over coupled wells, placing well equation
!             in the principal node of the well  ---
!
              DO 214 NCW = 1,N_CW
                IF( IWF_CW(ID_CW(7,NCW)).EQ.N ) THEN
                  NC = NC+1
                  JM_CW(NCW) = NC
                ENDIF
  214         CONTINUE
  216       CONTINUE
          ELSE
            DO 218 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  218       CONTINUE
!
!---        Loop over coupled wells, placing well equation
!           in the principal node of the well  ---
!
            DO 220 NCW = 1,N_CW
              IF( IWF_CW(ID_CW(7,NCW)).EQ.N ) THEN
                NC = NC+1
                JM_CW(NCW) = NC
              ENDIF
  220       CONTINUE
          ENDIF
  230   CONTINUE
!
!---    Determine matrix half-band widths, using I,J,K ordering  ---
!
        DO 260 K = 1,KFLD
        DO 260 J = 1,JFLD
        DO 260 I = 1,IFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
          IF( IXP(N).EQ.0 ) GOTO 260
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 252 KX = 1,KRX
          DO 252 JX = 1,JRX
          DO 252 IX = 1,IRX
            N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
            NP = ABS(IXP(N))
!
!---        Node  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
                DO 241 MX = 1,4
                  NB = ICM(MX,1,N)
                  IF( NB.EQ.0 ) EXIT
                  NB = ABS(IXP(NB))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NB)) )
  241           CONTINUE
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
                DO 242 MX = 1,4
                  NS = ICM(MX,2,N)
                  IF( NS.EQ.0 ) EXIT
                  NS = ABS(IXP(NS))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NS)) )
  242           CONTINUE
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
                DO 243 MX = 1,4
                  NW = ICM(MX,3,N)
                  IF( NW.EQ.0 ) EXIT
                  NW = ABS(IXP(NW))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NW)) )
  243           CONTINUE
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.IFLD ) THEN
                DO 244 MX = 1,4
                  NE = ICM(MX,4,N)
                  IF( NE.EQ.0 ) EXIT
                  NE = ABS(IXP(NE))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NE)) )
  244           CONTINUE
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.JFLD ) THEN
                DO 245 MX = 1,4
                  NN = ICM(MX,5,N)
                  IF( NN.EQ.0 ) EXIT
                  NN = ABS(IXP(NN))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &               ABS(IM(ISVC,NP)-IM(1,NN)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NN)) )
  245           CONTINUE
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.KFLD ) THEN
                DO 246 MX = 1,4
                  NT = ICM(MX,6,N)
                  IF( NT.EQ.0 ) EXIT
                  NT = ABS(IXP(NT))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NT)) )
  246           CONTINUE
            ENDIF
!
!---        Loop over coupled well nodes  ---
!
            DO 250 NCW = 1,N_CW
              DO 248 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                IF( IWF_CW(NWF).EQ.N ) THEN
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_CW(NCW)),
     &              ABS(IM(ISVC,NP)-JM_CW(NCW)))
                ENDIF
  248         CONTINUE
  250       CONTINUE
  252     CONTINUE
  260   CONTINUE
!
!---  J,K,I ordering yields the lowest half band width   ---
!
      ELSEIF( MHBC_JKI.LE.MHBC_KIJ ) THEN
        NJD = LBD*(3*MHBC_JKI+1) + LSP
        IF( NJD.GT.LJD ) THEN
          INDX = 5
          CHMSG = 'Number of Banded Matrix Rows ' //
     &      '> Parameter LJD'
          CALL WRMSGS( INDX )
        ENDIF
        DO 300 NCW = 1,LN_CW
          ID_CW(7,NCW) = M_JKI(2,NCW)
  300   CONTINUE
!
!---    Equation ordering using J,K,I ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 310 I = 1,IFLD
        DO 310 K = 1,KFLD
        DO 310 J = 1,JFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 310
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            IXP(N) = -IRX*JRX*KRX
            DO 302 IX = 1,IRX
            DO 302 KX = 1,KRX
            DO 302 JX = 1,JRX
              N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
              NC = NC + 1
              IXP(N) = NC
  302       CONTINUE
          ELSE
            NC = NC + 1
            IXP(N) = NC
          ENDIF
  310   CONTINUE
!
!---    Initialize counter  ---
!
        NC = 0
        MHBC = 0
        MHBT = 0
!
!---    Coupled equations half band width using J,K,I ordering  ---
!
        DO 330 I = 1,IFLD
        DO 330 K = 1,KFLD
        DO 330 J = 1,JFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
          IF( IXP(N).EQ.0 ) GOTO 330
!
!---      Loop over block refined nodes  ---
!
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 316 IX = 1,IRX
            DO 316 KX = 1,KRX
            DO 316 JX = 1,JRX
              N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
              NMD = ABS(IXP(N))
              DO 312 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  312         CONTINUE
!
!---          Loop over coupled wells, placing well equation
!             in the principal node of the well  ---
!
              DO 314 NCW = 1,N_CW
                IF( IWF_CW(ID_CW(7,NCW)).EQ.N ) THEN
                  NC = NC+1
                  JM_CW(NCW) = NC
                ENDIF
  314         CONTINUE
  316       CONTINUE
          ELSE
            DO 318 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  318       CONTINUE
!
!---        Loop over coupled wells, placing well equation
!           in the principal node of the well  ---
!
            DO 320 NCW = 1,N_CW
              IF( IWF_CW(ID_CW(7,NCW)).EQ.N ) THEN
                NC = NC+1
                JM_CW(NCW) = NC
              ENDIF
  320       CONTINUE
          ENDIF
  330   CONTINUE
!
!---    Determine matrix half-band widths using J,K,I ordering  ---
!
        DO 360 I = 1,IFLD
        DO 360 K = 1,KFLD
        DO 360 J = 1,JFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
          IF( IXP(N).EQ.0 ) GOTO 360
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 352 KX = 1,KRX
          DO 352 JX = 1,JRX
          DO 352 IX = 1,IRX
            N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
            NP = ABS(IXP(N))
!
!---        Node  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
                DO 341 MX = 1,4
                  NB = ICM(MX,1,N)
                  IF( NB.EQ.0 ) EXIT
                  NB = ABS(IXP(NB))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NB)) )
  341           CONTINUE
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
                DO 342 MX = 1,4
                  NS = ICM(MX,2,N)
                  IF( NS.EQ.0 ) EXIT
                  NS = ABS(IXP(NS))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NS)) )
  342           CONTINUE
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
                DO 343 MX = 1,4
                  NW = ICM(MX,3,N)
                  IF( NW.EQ.0 ) EXIT
                  NW = ABS(IXP(NW))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NW)) )
  343           CONTINUE
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.IFLD ) THEN
                DO 344 MX = 1,4
                  NE = ICM(MX,4,N)
                  IF( NE.EQ.0 ) EXIT
                  NE = ABS(IXP(NE))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NE)) )
  344           CONTINUE
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.JFLD ) THEN
                DO 345 MX = 1,4
                  NN = ICM(MX,5,N)
                  IF( NN.EQ.0 ) EXIT
                  NN = ABS(IXP(NN))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &               ABS(IM(ISVC,NP)-IM(1,NN)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NN)) )
  345           CONTINUE
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.KFLD ) THEN
                DO 346 MX = 1,4
                  NT = ICM(MX,6,N)
                  IF( NT.EQ.0 ) EXIT
                  NT = ABS(IXP(NT))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NT)) )
  346           CONTINUE
            ENDIF
!
!---        Loop over coupled well nodes  ---
!
            DO 350 NCW = 1,N_CW
              DO 348 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                IF( IWF_CW(NWF).EQ.N ) THEN
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_CW(NCW)),
     &              ABS(IM(ISVC,NP)-JM_CW(NCW)))
                ENDIF
  348         CONTINUE
  350       CONTINUE
  352     CONTINUE
  360   CONTINUE
!
!---  K,I,J ordering yields the lowest half band width   ---
!
      ELSE
        NJD = LBD*(3*MHBC_KIJ+1) + LSP
        IF( NJD.GT.LJD ) THEN
          INDX = 5
          CHMSG = 'Number of Banded Matrix Rows ' //
     &      '> Parameter LJD'
          CALL WRMSGS( INDX )
        ENDIF
        DO 400 NCW = 1,LN_CW
          ID_CW(7,NCW) = M_KIJ(2,NCW)
  400   CONTINUE
!
!---    Equation ordering using K,I,J ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 410 J = 1,JFLD
        DO 410 I = 1,IFLD
        DO 410 K = 1,KFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 410
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            IXP(N) = -IRX*JRX*KRX
            DO 402 JX = 1,JRX
            DO 402 IX = 1,IRX
            DO 402 KX = 1,KRX
              N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
              NC = NC + 1
              IXP(N) = NC
  402       CONTINUE
          ELSE
            NC = NC + 1
            IXP(N) = NC
          ENDIF
  410   CONTINUE
!
!---    Initialize counter  ---
!
        NC = 0
        MHBC = 0
        MHBT = 0
!
!---    Coupled equations half band width using K,I,J ordering  ---
!
        DO 430 J = 1,JFLD
        DO 430 I = 1,IFLD
        DO 430 K = 1,KFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
          IF( IXP(N).EQ.0 ) GOTO 430
!
!---      Loop over block refined nodes  ---
!
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 416 IX = 1,IRX
            DO 416 KX = 1,KRX
            DO 416 JX = 1,JRX
              N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
              NMD = ABS(IXP(N))
              DO 412 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  412         CONTINUE
!
!---          Loop over coupled wells, placing well equation
!             in the principal node of the well  ---
!
              DO 414 NCW = 1,N_CW
                IF( IWF_CW(ID_CW(7,NCW)).EQ.N ) THEN
                  NC = NC+1
                  JM_CW(NCW) = NC
                ENDIF
  414         CONTINUE
  416       CONTINUE
          ELSE
            DO 418 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  418       CONTINUE
!
!---        Loop over coupled wells, placing well equation
!           in the principal node of the well  ---
!
            DO 420 NCW = 1,N_CW
              IF( IWF_CW(ID_CW(7,NCW)).EQ.N ) THEN
                NC = NC+1
                JM_CW(NCW) = NC
              ENDIF
  420       CONTINUE
          ENDIF
  430   CONTINUE
!
!---    Determine matrix half-band widths using K,I,J ordering  ---
!
        DO 460 J = 1,JFLD
        DO 460 I = 1,IFLD
        DO 460 K = 1,KFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
          IF( IXP(N).EQ.0 ) GOTO 460
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 452 KX = 1,KRX
          DO 452 JX = 1,JRX
          DO 452 IX = 1,IRX
            N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
            NP = ABS(IXP(N))
!
!---        Node  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
                DO 441 MX = 1,4
                  NB = ICM(MX,1,N)
                  IF( NB.EQ.0 ) EXIT
                  NB = ABS(IXP(NB))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NB)) )
  441           CONTINUE
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
                DO 442 MX = 1,4
                  NS = ICM(MX,2,N)
                  IF( NS.EQ.0 ) EXIT
                  NS = ABS(IXP(NS))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NS)) )
  442           CONTINUE
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
                DO 443 MX = 1,4
                  NW = ICM(MX,3,N)
                  IF( NW.EQ.0 ) EXIT
                  NW = ABS(IXP(NW))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NW)) )
  443           CONTINUE
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.IFLD ) THEN
                DO 444 MX = 1,4
                  NE = ICM(MX,4,N)
                  IF( NE.EQ.0 ) EXIT
                  NE = ABS(IXP(NE))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NE)) )
  444           CONTINUE
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.JFLD ) THEN
                DO 445 MX = 1,4
                  NN = ICM(MX,5,N)
                  IF( NN.EQ.0 ) EXIT
                  NN = ABS(IXP(NN))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &               ABS(IM(ISVC,NP)-IM(1,NN)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NN)) )
  445           CONTINUE
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.KFLD ) THEN
                DO 446 MX = 1,4
                  NT = ICM(MX,6,N)
                  IF( NT.EQ.0 ) EXIT
                  NT = ABS(IXP(NT))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NT)) )
  446           CONTINUE
            ENDIF
!
!---        Loop over coupled well nodes  ---
!
            DO 450 NCW = 1,N_CW
              DO 448 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                IF( IWF_CW(NWF).EQ.N ) THEN
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_CW(NCW)),
     &              ABS(IM(ISVC,NP)-JM_CW(NCW)))
                ENDIF
  448         CONTINUE
  450       CONTINUE
  452     CONTINUE
  460   CONTINUE
      ENDIF
      MLC = MHBC
      MLT = ISVT*MHBT + ISVT - 1
      MUC = MHBC
      MUT = ISVT*MHBT + ISVT - 1
      MDC = MLC + MUC + 1
      MDT = MLT + MUT + 1
!
!---  Check Parameter LJD  ---
!
      NJD = LBD*(3*MHBC+1) + LSP
      IF( NJD.GT.LJD ) THEN
        INDX = 5
        CHMSG = 'Number of Banded Matrix Rows ' //
     &    '> Parameter LJD'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  SPLIB .or. Lis .or. PETSc Solver  ---
!
      IF( ILES.EQ.3 .OR. ILES.EQ.4 .OR. ILES.EQ.5 ) THEN
!
!---    X-Y Plane yields the lowest band width.
!       Load Jacobian matrix in the increment order I,J,K  ---
!
        IF( MHBC_IJK.LE.MHBC_JKI .AND. MHBC_IJK.LE.MHBC_KIJ ) THEN
          NC = 0
          NCC = 0




          NLU(1) = 1
          NLUC(1) = 1

          DO 800 K = 1,KFLD
          DO 800 J = 1,JFLD
          DO 800 I = 1,IFLD
            N = ND(I,J,K)
            IF( IXP(N).EQ.0 ) CYCLE
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 790 KX = 1,KRX
            DO 790 JX = 1,JRX
            DO 790 IX = 1,IRX
              N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
              NP = IXP(N)
              DO 660 L = 1,ISVC
                NMD = IM(L,NP)
                MA = 0
!
!---            Node  ---
!
                DO 500 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NP)

                  KLU(NMD,M+MA) = NC

  500           CONTINUE
                MA = MA + ISVC
!
!---            Bottom node neighbors  ---
!
                DO 512 MX = 1,4
                  NB = ICM(MX,1,N)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  DO 510 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NB)

                    KLU(NMD,M+MA) = NC

  510             CONTINUE
                  MA = MA + ISVC
  512           CONTINUE
!
!---            South node neighbors  ---
!
                DO 522 MX = 1,4
                  NS = ICM(MX,2,N)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  DO 520 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NS)

                    KLU(NMD,M+MA) = NC

  520             CONTINUE
                  MA = MA + ISVC
  522           CONTINUE
!
!---            West node neighbors  ---
!
                DO 532 MX = 1,4
                  NW = ICM(MX,3,N)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  DO 530 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NW)

                    KLU(NMD,M+MA) = NC

  530             CONTINUE
                  MA = MA + ISVC
  532           CONTINUE
!
!---            East node neighbors  ---
!
                DO 542 MX = 1,4
                  NE = ICM(MX,4,N)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  DO 540 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NE)

                    KLU(NMD,M+MA) = NC

  540             CONTINUE
                  MA = MA + ISVC
  542           CONTINUE
!
!---            North node neighbors  ---
!
                DO 552 MX = 1,4
                  NN = ICM(MX,5,N)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  DO 550 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NN)

                    KLU(NMD,M+MA) = NC

  550             CONTINUE
                  MA = MA + ISVC
  552           CONTINUE
!
!---            Top node neighbors  ---
!
                DO 562 MX = 1,4
                  NT = ICM(MX,6,N)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  DO 560 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NT)

                    KLU(NMD,M+MA) = NC

  560             CONTINUE
                  MA = MA + ISVC
  562           CONTINUE
!
!---            Node contains a coupled well node,
!               include couple well equations in nodal equations  ---
!
                DO 650 NCW = 1,N_CW
                  NWFX = ID_CW(6,NCW)-ID_CW(5,NCW)+1
                  DO 640 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                    IF( IWF_CW(NWF).EQ.N ) THEN
                      MA = (NWFX*ISVC) + (NWF-ID_CW(5,NCW))*ISVC + 1
                      NC = NC+1




                      MLU(NC) = JM_CW(NCW)

                      KLU_CW(L+MA,NCW) = NC

                    ENDIF
  640             CONTINUE
  650           CONTINUE



                NLU(NMD+1) = NC+1

  660         CONTINUE
!
!---          Loop over coupled wells  ---
!
              DO 668 NCW = 1,N_CW
!
!---            Node contains principal coupled well node,
!               include coupled well equation  ---
!
                IF( IWF_CW(ID_CW(7,NCW)).EQ.N ) THEN
                  NC = NC+1
                  NMD = JM_CW(NCW)




                  MLU(NC) = NMD

                  KLU_CW(1,NCW) = NC

!
!---              Loop over nodes that contain coupled well nodes
!                 include nodal equations in coupled well equation  ---
!
                  DO 664 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                    NPX = IXP(IWF_CW(NWF))
                    MA = (NWF-ID_CW(5,NCW))*ISVC + 1
                    DO 662 M = 1,ISVC
                      NC = NC+1




                      MLU(NC) = IM(M,NPX)

                      KLU_CW(M+MA,NCW) = NC

  662               CONTINUE
  664             CONTINUE



                  NLU(NMD+1) = NC+1

                ENDIF
  668         CONTINUE
!
!---          Solute transport equations  ---
!
              MA = 1
!
!---          Node  ---
!
              NCC = NCC+1




              MLUC(NCC) = NP

              KLUC(NP,MA) = NCC

              MA = MA + 1
!
!---          Bottom node neighbors  ---
!
              DO 702 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                NCC = NCC+1




                MLUC(NCC) = NB

                KLUC(NP,MA) = NCC

                MA = MA + 1
  702         CONTINUE
!
!---          South node neighbors  ---
!
              DO 712 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                NCC = NCC+1




                MLUC(NCC) = NS

                KLUC(NP,MA) = NCC

                MA = MA + 1
  712         CONTINUE
!
!---          West node neighbors  ---
!
              DO 722 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                NCC = NCC+1




                MLUC(NCC) = NW

                KLUC(NP,MA) = NCC

                MA = MA + 1
  722         CONTINUE
!
!---          East node neighbors  ---
!
              DO 732 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                NCC = NCC+1




                MLUC(NCC) = NE

                KLUC(NP,MA) = NCC

                MA = MA + 1
  732         CONTINUE
!
!---          North node neighbors  ---
!
              DO 742 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                NCC = NCC+1




                MLUC(NCC) = NN

                KLUC(NP,MA) = NCC

                MA = MA + 1
  742         CONTINUE
!
!---          Top node neighbors  ---
!
              DO 752 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                NCC = NCC+1




                MLUC(NCC) = NT

                KLUC(NP,MA) = NCC

                MA = MA + 1
  752         CONTINUE




              NLUC(NP+1) = NCC+1


  790       CONTINUE
  800     CONTINUE
          MKC = NC
          MKT = NCC
!
!---    Y-Z Plane yields the lowest band width.
!       Load Jacobian matrix in the increment order J,K,I  ---
!
        ELSEIF( MHBC_JKI.LE.MHBC_KIJ ) THEN
          NC = 0
          NCC = 0




          NLU(1) = 1
          NLUC(1) = 1

          DO 1800 I = 1,IFLD
          DO 1800 K = 1,KFLD
          DO 1800 J = 1,JFLD
            N = ND(I,J,K)
            IF( IXP(N).EQ.0 ) CYCLE
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 1790 IX = 1,IRX
            DO 1790 KX = 1,KRX
            DO 1790 JX = 1,JRX
              N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
              NP = IXP(N)
              DO 1660 L = 1,ISVC
                NMD = IM(L,NP)
                MA = 0
!
!---            Node  ---
!
                DO 1500 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NP)

                  KLU(NMD,M+MA) = NC

 1500           CONTINUE
                MA = MA + ISVC
!
!---            Bottom node neighbors  ---
!
                DO 1512 MX = 1,4
                  NB = ICM(MX,1,N)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  DO 1510 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NB)

                    KLU(NMD,M+MA) = NC

 1510             CONTINUE
                  MA = MA + ISVC
 1512           CONTINUE
!
!---            South node neighbors  ---
!
                DO 1522 MX = 1,4
                  NS = ICM(MX,2,N)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  DO 1520 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NS)

                    KLU(NMD,M+MA) = NC

 1520             CONTINUE
                  MA = MA + ISVC
 1522           CONTINUE
!
!---            West node neighbors  ---
!
                DO 1532 MX = 1,4
                  NW = ICM(MX,3,N)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  DO 1530 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NW)

                    KLU(NMD,M+MA) = NC

 1530             CONTINUE
                  MA = MA + ISVC
 1532           CONTINUE
!
!---            East node neighbors  ---
!
                DO 1542 MX = 1,4
                  NE = ICM(MX,4,N)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  DO 1540 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NE)

                    KLU(NMD,M+MA) = NC

 1540             CONTINUE
                  MA = MA + ISVC
 1542           CONTINUE
!
!---            North node neighbors  ---
!
                DO 1552 MX = 1,4
                  NN = ICM(MX,5,N)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  DO 1550 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NN)

                    KLU(NMD,M+MA) = NC

 1550             CONTINUE
                  MA = MA + ISVC
 1552           CONTINUE
!
!---            Top node neighbors  ---
!
                DO 1562 MX = 1,4
                  NT = ICM(MX,6,N)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  DO 1560 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NT)

                    KLU(NMD,M+MA) = NC

 1560             CONTINUE
                  MA = MA + ISVC
 1562           CONTINUE
!
!---            Node contains a coupled well node,
!               include couple well equations in nodal equations  ---
!
                DO 1650 NCW = 1,N_CW
                  NWFX = ID_CW(6,NCW)-ID_CW(5,NCW)+1
                  DO 1640 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                    IF( IWF_CW(NWF).EQ.N ) THEN
                      MA = (NWFX*ISVC) + (NWF-ID_CW(5,NCW))*ISVC + 1
                      NC = NC+1




                      MLU(NC) = JM_CW(NCW)

                      KLU_CW(L+MA,NCW) = NC

                    ENDIF
 1640             CONTINUE
 1650           CONTINUE



                NLU(NMD+1) = NC+1

 1660         CONTINUE
!
!---          Loop over coupled wells  ---
!
              DO 1668 NCW = 1,N_CW
!
!---            Node contains principal coupled well node,
!               include coupled well equation  ---
!
                IF( IWF_CW(ID_CW(7,NCW)).EQ.N ) THEN
                  NC = NC+1
                  NMD = JM_CW(NCW)




                  MLU(NC) = NMD

                  KLU_CW(1,NCW) = NC

!
!---              Loop over nodes that contain coupled well nodes
!                 include nodal equations in coupled well equation  ---
!
                  DO 1664 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                    NPX = IXP(IWF_CW(NWF))
                    MA = (NWF-ID_CW(5,NCW))*ISVC + 1
                    DO 1662 M = 1,ISVC
                      NC = NC+1




                      MLU(NC) = IM(M,NPX)

                      KLU_CW(M+MA,NCW) = NC

 1662               CONTINUE
 1664             CONTINUE



                  NLU(NMD+1) = NC+1

                ENDIF
 1668         CONTINUE
!
!---          Solute transport equations  ---
!
              MA = 1
!
!---          Node  ---
!
              NCC = NCC+1




              MLUC(NCC) = NP

              KLUC(NP,MA) = NCC

              MA = MA + 1
!
!---          Bottom node neighbors  ---
!
              DO 1702 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                NCC = NCC+1




                MLUC(NCC) = NB

                KLUC(NP,MA) = NCC

                MA = MA + 1
 1702         CONTINUE
!
!---          South node neighbors  ---
!
              DO 1712 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                NCC = NCC+1




                MLUC(NCC) = NS

                KLUC(NP,MA) = NCC

                MA = MA + 1
 1712         CONTINUE
!
!---          West node neighbors  ---
!
              DO 1722 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                NCC = NCC+1




                MLUC(NCC) = NW

                KLUC(NP,MA) = NCC

                MA = MA + 1
 1722         CONTINUE
!
!---          East node neighbors  ---
!
              DO 1732 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                NCC = NCC+1




                MLUC(NCC) = NE

                KLUC(NP,MA) = NCC

                MA = MA + 1
 1732         CONTINUE
!
!---          North node neighbors  ---
!
              DO 1742 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                NCC = NCC+1




                MLUC(NCC) = NN

                KLUC(NP,MA) = NCC

                MA = MA + 1
 1742         CONTINUE
!
!---          Top node neighbors  ---
!
              DO 1752 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                NCC = NCC+1




                MLUC(NCC) = NT

                KLUC(NP,MA) = NCC

                MA = MA + 1
 1752         CONTINUE




              NLUC(NP+1) = NCC+1


 1790       CONTINUE
 1800     CONTINUE
          MKC = NC
          MKT = NCC
!
!---    Z-X Plane yields the lowest band width.
!       Load Jacobian matrix in the increment order K,I,J  ---
!
        ELSE
          NC = 0
          NCC = 0




          NLU(1) = 1
          NLUC(1) = 1

          DO 2800 J = 1,JFLD
          DO 2800 I = 1,IFLD
          DO 2800 K = 1,KFLD
            N = ND(I,J,K)
            IF( IXP(N).EQ.0 ) CYCLE
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 2790 JX = 1,JRX
            DO 2790 IX = 1,IRX
            DO 2790 KX = 1,KRX
              N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
              NP = IXP(N)
              DO 2660 L = 1,ISVC
                NMD = IM(L,NP)
                MA = 0
!
!---            Node  ---
!
                DO 2500 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NP)

                  KLU(NMD,M+MA) = NC

 2500           CONTINUE
                MA = MA + ISVC
!
!---            Bottom node neighbors  ---
!
                DO 2512 MX = 1,4
                  NB = ICM(MX,1,N)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  DO 2510 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NB)

                    KLU(NMD,M+MA) = NC

 2510             CONTINUE
                  MA = MA + ISVC
 2512           CONTINUE
!
!---            South node neighbors  ---
!
                DO 2522 MX = 1,4
                  NS = ICM(MX,2,N)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  DO 2520 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NS)

                    KLU(NMD,M+MA) = NC

 2520             CONTINUE
                  MA = MA + ISVC
 2522           CONTINUE
!
!---            West node neighbors  ---
!
                DO 2532 MX = 1,4
                  NW = ICM(MX,3,N)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  DO 2530 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NW)

                    KLU(NMD,M+MA) = NC

 2530             CONTINUE
                  MA = MA + ISVC
 2532           CONTINUE
!
!---            East node neighbors  ---
!
                DO 2542 MX = 1,4
                  NE = ICM(MX,4,N)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  DO 2540 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NE)

                    KLU(NMD,M+MA) = NC

 2540             CONTINUE
                  MA = MA + ISVC
 2542           CONTINUE
!
!---            North node neighbors  ---
!
                DO 2552 MX = 1,4
                  NN = ICM(MX,5,N)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  DO 2550 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NN)

                    KLU(NMD,M+MA) = NC

 2550             CONTINUE
                  MA = MA + ISVC
 2552           CONTINUE
!
!---            Top node neighbors  ---
!
                DO 2562 MX = 1,4
                  NT = ICM(MX,6,N)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  DO 2560 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NT)

                    KLU(NMD,M+MA) = NC

 2560             CONTINUE
                  MA = MA + ISVC
 2562           CONTINUE
!
!---            Node contains a coupled well node,
!               include couple well equations in nodal equations  ---
!
                DO 2650 NCW = 1,N_CW
                  NWFX = ID_CW(6,NCW)-ID_CW(5,NCW)+1
                  DO 2640 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                    IF( IWF_CW(NWF).EQ.N ) THEN
                      MA = (NWFX*ISVC) + (NWF-ID_CW(5,NCW))*ISVC + 1
                      NC = NC+1




                      MLU(NC) = JM_CW(NCW)

                      KLU_CW(L+MA,NCW) = NC

                    ENDIF
 2640             CONTINUE
 2650           CONTINUE



                NLU(NMD+1) = NC+1

 2660         CONTINUE
!
!---          Loop over coupled wells  ---
!
              DO 2668 NCW = 1,N_CW
!
!---            Node contains principal coupled well node,
!               include coupled well equation  ---
!
                IF( IWF_CW(ID_CW(7,NCW)).EQ.N ) THEN
                  NC = NC+1
                  NMD = JM_CW(NCW)




                  MLU(NC) = NMD

                  KLU_CW(1,NCW) = NC

!
!---              Loop over nodes that contain coupled well nodes
!                 include nodal equations in coupled well equation  ---
!
                  DO 2664 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                    NPX = IXP(IWF_CW(NWF))
                    MA = (NWF-ID_CW(5,NCW))*ISVC + 1
                    DO 2662 M = 1,ISVC
                      NC = NC+1




                      MLU(NC) = IM(M,NPX)

                      KLU_CW(M+MA,NCW) = NC

 2662               CONTINUE
 2664             CONTINUE



                  NLU(NMD+1) = NC+1

                ENDIF
 2668         CONTINUE
!
!---          Solute transport equations  ---
!
              MA = 1
!
!---          Node  ---
!
              NCC = NCC+1




              MLUC(NCC) = NP

              KLUC(NP,MA) = NCC

              MA = MA + 1
!
!---          Bottom node neighbors  ---
!
              DO 2702 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                NCC = NCC+1




                MLUC(NCC) = NB

                KLUC(NP,MA) = NCC

                MA = MA + 1
 2702         CONTINUE
!
!---          South node neighbors  ---
!
              DO 2712 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                NCC = NCC+1




                MLUC(NCC) = NS

                KLUC(NP,MA) = NCC

                MA = MA + 1
 2712         CONTINUE
!
!---          West node neighbors  ---
!
              DO 2722 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                NCC = NCC+1




                MLUC(NCC) = NW

                KLUC(NP,MA) = NCC

                MA = MA + 1
 2722         CONTINUE
!
!---          East node neighbors  ---
!
              DO 2732 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                NCC = NCC+1




                MLUC(NCC) = NE

                KLUC(NP,MA) = NCC

                MA = MA + 1
 2732         CONTINUE
!
!---          North node neighbors  ---
!
              DO 2742 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                NCC = NCC+1




                MLUC(NCC) = NN

                KLUC(NP,MA) = NCC

                MA = MA + 1
 2742         CONTINUE
!
!---          Top node neighbors  ---
!
              DO 2752 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                NCC = NCC+1




                MLUC(NCC) = NT

                KLUC(NP,MA) = NCC

                MA = MA + 1
 2752         CONTINUE




              NLUC(NP+1) = NCC+1


 2790       CONTINUE
 2800     CONTINUE
          MKC = NC
          MKT = NCC
        ENDIF
      ENDIF
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCBP_CW_BR group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCBP_CW_DP
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
!     Configure the Jacobian matrix pointer arrays for coupled wells
!     and dual-porosity model for STOMP-EOR.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 November 2015.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PTZR
      USE JACOB
      USE GRID
      USE DUAL_POR
      USE COUP_WELL
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
!----------------------Type Declarations-------------------------------!
!




      INTEGER, DIMENSION(:,:), ALLOCATABLE :: M_IJK,M_JKI,M_KIJ

!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/JCBP_CW_DP'
      IF( INDEX(SVN_ID(95)(1:1),'$').EQ.0 ) SVN_ID(95) =
     & '$Id: jcbp.F 1080 2017-03-14 16:22:02Z d3c002 $'
      ILIMIT = 2**30

!
!---  Dynamic memory allocation  ---
!
      IF( .NOT.ALLOCATED(M_IJK) ) THEN
        ALLOCATE( M_IJK(1:2,1:LN_CW),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: M_IJK'
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
      IF( .NOT.ALLOCATED(M_JKI) ) THEN
        ALLOCATE( M_JKI(1:2,1:LN_CW),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: M_JKI'
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
      IF( .NOT.ALLOCATED(M_KIJ) ) THEN
        ALLOCATE( M_KIJ(1:2,1:LN_CW),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: M_KIJ'
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF

!
!---  Coupled equations half band width using I,J,K ordering  ---
!
      NC = 0
      DO 5 K = 1,KFLD
      DO 5 J = 1,JFLD
      DO 5 I = 1,IFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 5
        NC = NC + 1
        IXP(N) = NC
    5 CONTINUE
      DO 60 NCW = 1,N_CW
        M_IJK(1,NCW) = ILIMIT
!
!---    Locate coupled well equation at the mid-point of the string
!       of field nodes that contain the well  ---
!
        IDCW = (ID_CW(5,NCW)+ID_CW(6,NCW))/2
        NC = 0
        NWF = IWF_CW(IDCW)
        MHBC_IJK = 0
        DO 20 K = 1,KFLD
        DO 20 J = 1,JFLD
        DO 20 I = 1,IFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 20
          DO 10 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
   10     CONTINUE
          DO 11 M = 1,ISVC
            NC = NC+1
            IM_M(M,NMD) = NC
   11     CONTINUE
          IF( NWF.EQ.N ) THEN
            NC = NC+1
            JM_CWX = NC
          ENDIF
   20   CONTINUE
!
!---    Determine matrix half-band widths, using I,J,K ordering  ---
!
        DO 40 K = 1,KFLD
        DO 40 J = 1,JFLD
        DO 40 I = 1,IFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 40
!
!---      Node  ---
!
          MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Fracture to matrix connections  ---
!
          MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM_M(ISVC,NP)),
     &      ABS(IM(ISVC,NP)-IM_M(1,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              NB = ABS(IXP(N-IJFLD))
              MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NB)),
     &          ABS(IM(ISVC,NP)-IM(1,NB)))
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              NS = ABS(IXP(N-IFLD))
              MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NS)),
     &          ABS(IM(ISVC,NP)-IM(1,NS)))
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              NW = ABS(IXP(N-1))
              MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NW)),
     &          ABS(IM(ISVC,NP)-IM(1,NW)))
            ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              NE = ABS(IXP(N+1))
              MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NE)),
     &          ABS(IM(ISVC,NP)-IM(1,NE)))
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
            IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              NN = ABS(IXP(N+IFLD))
              MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NN)),
     &           ABS(IM(ISVC,NP)-IM(1,NN)))
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
            IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              NT = ABS(IXP(N+IJFLD))
              MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NT)),
     &          ABS(IM(ISVC,NP)-IM(1,NT)))
            ENDIF
          ENDIF
!
!---      Coupled well nodes  ---
!
          DO 30 JDCW = ID_CW(5,NCW),ID_CW(6,NCW)
            IF( IWF_CW(JDCW).EQ.N ) THEN
              MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-JM_CWX),
     &          ABS(IM(ISVC,NP)-JM_CWX))
            ENDIF
   30     CONTINUE
   40   CONTINUE
        IF( MHBC_IJK.LT.M_IJK(1,NCW) ) THEN
          M_IJK(1,NCW) = MHBC_IJK
          M_IJK(2,NCW) = IDCW
        ENDIF
   60 CONTINUE
!
!---  Coupled equations half band width using J,K,I ordering  ---
!
      NC = 0
      DO 65 I = 1,IFLD
      DO 65 K = 1,KFLD
      DO 65 J = 1,JFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 65
        NC = NC + 1
        IXP(N) = NC
   65 CONTINUE
      DO 120 NCW = 1,N_CW
        M_JKI(1,NCW) = ILIMIT
!
!---    Locate coupled well equation at the mid-point of the string
!       of field nodes that contain the well  ---
!
        IDCW = (ID_CW(5,NCW)+ID_CW(6,NCW))/2
        NC = 0
        NWF = IWF_CW(IDCW)
        MHBC_JKI = 0
        DO 80 I = 1,IFLD
        DO 80 K = 1,KFLD
        DO 80 J = 1,JFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 80
          DO 70 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
   70     CONTINUE
          DO 71 M = 1,ISVC
            NC = NC+1
            IM_M(M,NMD) = NC
   71     CONTINUE
          IF( NWF.EQ.N ) THEN
            NC = NC+1
            JM_CWX = NC
          ENDIF
   80   CONTINUE
!
!---    Determine matrix half-band widths, using J,K,I ordering  ---
!
        DO 100 I = 1,IFLD
        DO 100 K = 1,KFLD
        DO 100 J = 1,JFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 100
!
!---      Node  ---
!
          MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Fracture to matrix connections  ---
!
          MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM_M(ISVC,NP)),
     &      ABS(IM(ISVC,NP)-IM_M(1,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              NB = ABS(IXP(N-IJFLD))
              MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NB)),
     &          ABS(IM(ISVC,NP)-IM(1,NB)))
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              NS = ABS(IXP(N-IFLD))
              MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NS)),
     &          ABS(IM(ISVC,NP)-IM(1,NS)))
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              NW = ABS(IXP(N-1))
              MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NW)),
     &          ABS(IM(ISVC,NP)-IM(1,NW)))
            ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              NE = ABS(IXP(N+1))
              MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NE)),
     &          ABS(IM(ISVC,NP)-IM(1,NE)))
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
            IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              NN = ABS(IXP(N+IFLD))
              MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NN)),
     &           ABS(IM(ISVC,NP)-IM(1,NN)))
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
            IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              NT = ABS(IXP(N+IJFLD))
              MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NT)),
     &          ABS(IM(ISVC,NP)-IM(1,NT)))
            ENDIF
          ENDIF
!
!---      Coupled well nodes  ---
!
          DO 90 JDCW = ID_CW(5,NCW),ID_CW(6,NCW)
            IF( IWF_CW(JDCW).EQ.N ) THEN
              MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-JM_CWX),
     &          ABS(IM(ISVC,NP)-JM_CWX))
            ENDIF
   90     CONTINUE
  100   CONTINUE
        IF( MHBC_JKI.LT.M_JKI(1,NCW) ) THEN
          M_JKI(1,NCW) = MHBC_JKI
          M_JKI(2,NCW) = IDCW
        ENDIF
  120 CONTINUE
!
!---  Coupled equations half band width using K,I,J ordering  ---
!
      NC = 0
      DO 125 J = 1,JFLD
      DO 125 I = 1,IFLD
      DO 125 K = 1,KFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 125
        NC = NC + 1
        IXP(N) = NC
  125 CONTINUE
      DO 180 NCW = 1,N_CW
        M_KIJ(1,NCW) = ILIMIT
!
!---    Locate coupled well equation at the mid-point of the string
!       of field nodes that contain the well  ---
!
        IDCW = (ID_CW(5,NCW)+ID_CW(6,NCW))/2
        NC = 0
        NWF = IWF_CW(IDCW)
        MHBC_KIJ = 0
        DO 140 J = 1,JFLD
        DO 140 I = 1,IFLD
        DO 140 K = 1,KFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 140
          DO 130 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
  130     CONTINUE
          DO 131 M = 1,ISVC
            NC = NC+1
            IM_M(M,NMD) = NC
  131     CONTINUE
          IF( NWF.EQ.N ) THEN
            NC = NC+1
            JM_CWX = NC
          ENDIF
  140   CONTINUE
!
!---    Determine matrix half-band widths using K,I,J ordering  ---
!
        DO 160 J = 1,JFLD
        DO 160 I = 1,IFLD
        DO 160 K = 1,KFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 160
!
!---      Node  ---
!
          MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Fracture to matrix connections  ---
!
          MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM_M(ISVC,NP)),
     &      ABS(IM(ISVC,NP)-IM_M(1,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              NB = ABS(IXP(N-IJFLD))
              MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NB)),
     &          ABS(IM(ISVC,NP)-IM(1,NB)))
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              NS = ABS(IXP(N-IFLD))
              MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NS)),
     &          ABS(IM(ISVC,NP)-IM(1,NS)))
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              NW = ABS(IXP(N-1))
              MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NW)),
     &          ABS(IM(ISVC,NP)-IM(1,NW)))
            ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              NE = ABS(IXP(N+1))
              MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NE)),
     &          ABS(IM(ISVC,NP)-IM(1,NE)))
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
            IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              NN = ABS(IXP(N+IFLD))
              MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NN)),
     &           ABS(IM(ISVC,NP)-IM(1,NN)))
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
            IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              NT = ABS(IXP(N+IJFLD))
              MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NT)),
     &          ABS(IM(ISVC,NP)-IM(1,NT)))
            ENDIF
          ENDIF
!
!---      Coupled well nodes  ---
!
          DO 150 JDCW = ID_CW(5,NCW),ID_CW(6,NCW)
            IF( IWF_CW(JDCW).EQ.N ) THEN
              MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-JM_CWX),
     &          ABS(IM(ISVC,NP)-JM_CWX))
            ENDIF
  150     CONTINUE
  160   CONTINUE
        IF( MHBC_KIJ.LT.M_KIJ(1,NCW) ) THEN
          M_KIJ(1,NCW) = MHBC_KIJ
          M_KIJ(2,NCW) = IDCW
        ENDIF
  180 CONTINUE
!
!---  Loop over wells searching for the maximum half band width   ---
!
      MHBC_IJK = 0
      MHBC_JKI = 0
      MHBC_KIJ = 0
      DO 190 NCW = 1,N_CW
        IF( M_IJK(1,NCW).GT.MHBC_IJK ) MHBC_IJK = M_IJK(1,NCW)
        IF( M_JKI(1,NCW).GT.MHBC_JKI ) MHBC_JKI = M_JKI(1,NCW)
        IF( M_KIJ(1,NCW).GT.MHBC_KIJ ) MHBC_KIJ = M_KIJ(1,NCW)
  190 CONTINUE
!
!---  I,J,K ordering yields the lowest half band width   ---
!
      IF( MHBC_IJK.LE.MHBC_JKI .AND. MHBC_IJK.LE.MHBC_KIJ ) THEN
        NJD = LBD*(3*MHBC_IJK+1) + LSP
        IF( NJD.GT.LJD ) THEN
          INDX = 5
          CHMSG = 'Number of Banded Matrix Rows ' //
     &      '> Parameter LJD'
          CALL WRMSGS( INDX )
        ENDIF
        DO 200 NCW = 1,LN_CW
          ID_CW(7,NCW) = M_IJK(2,NCW)
  200   CONTINUE
!
!---    Equation ordering using I,J,K ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 205 K = 1,KFLD
        DO 205 J = 1,JFLD
        DO 205 I = 1,IFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 205
          NC = NC + 1
          IXP(N) = NC
  205   CONTINUE
!
!---    Initialize counter  ---
!
        NC = 0
        MHBC = 0
        MHBT = 0
!
!---    Coupled equations half band width using I,J,K ordering  ---
!
        DO 230 K = 1,KFLD
        DO 230 J = 1,JFLD
        DO 230 I = 1,IFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 230
          DO 210 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
  210     CONTINUE
          DO 211 M = 1,ISVC
            NC = NC+1
            IM_M(M,NMD) = NC
  211     CONTINUE
!
!---      Loop over coupled wells, placing well equation
!         in the principal node of the well  ---
!
          DO 220 NCW = 1,N_CW
            IF( IWF_CW(ID_CW(7,NCW)).EQ.N ) THEN
              NC = NC+1
              JM_CW(NCW) = NC
            ENDIF
  220     CONTINUE
  230   CONTINUE
!
!---    Determine matrix half-band widths, using I,J,K ordering  ---
!
        DO 260 K = 1,KFLD
        DO 260 J = 1,JFLD
        DO 260 I = 1,IFLD
            N = ND(I,J,K)
            NP = ABS(IXP(N))
!
!---        Skip to next active node  ---
!
            IF( IXP(N).EQ.0 ) GOTO 260
!
!---      Node  ---
!
          MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Fracture to matrix connections  ---
!
          MHBC = MAX(MHBC,ABS(IM(1,NP)-IM_M(ISVC,NP)),
     &      ABS(IM(ISVC,NP)-IM_M(1,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              NB = IXP(N-IJFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &          ABS(IM(ISVC,NP)-IM(1,NB)))
              MHBT = MAX( MHBT,ABS(NP-NB) )
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              NS = IXP(N-IFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &          ABS(IM(ISVC,NP)-IM(1,NS)))
              MHBT = MAX( MHBT,ABS(NP-NS) )
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              NW = IXP(N-1)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &          ABS(IM(ISVC,NP)-IM(1,NW)))
              MHBT = MAX( MHBT,ABS(NP-NW) )
            ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              NE = IXP(N+1)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &          ABS(IM(ISVC,NP)-IM(1,NE)))
              MHBT = MAX( MHBT,ABS(NP-NE) )
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
            IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              NN = IXP(N+IFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &           ABS(IM(ISVC,NP)-IM(1,NN)))
              MHBT = MAX( MHBT,ABS(NP-NN) )
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
            IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              NT = IXP(N+IJFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &          ABS(IM(ISVC,NP)-IM(1,NT)))
              MHBT = MAX( MHBT,ABS(NP-NT) )
            ENDIF
          ENDIF
!
!---      Coupled well nodes  ---
!
          DO 250 NCW = 1,N_CW
            DO 240 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
              IF( IWF_CW(NWF).EQ.N ) THEN
                MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_CW(NCW)),
     &            ABS(IM(ISVC,NP)-JM_CW(NCW)))
              ENDIF
  240       CONTINUE
  250     CONTINUE
  260   CONTINUE
!
!---  J,K,I ordering yields the lowest half band width   ---
!
      ELSEIF( MHBC_JKI.LE.MHBC_KIJ ) THEN
        NJD = LBD*(3*MHBC_JKI+1) + LSP
        IF( NJD.GT.LJD ) THEN
          INDX = 5
          CHMSG = 'Number of Banded Matrix Rows ' //
     &      '> Parameter LJD'
          CALL WRMSGS( INDX )
        ENDIF
        DO 300 NCW = 1,LN_CW
          ID_CW(7,NCW) = M_JKI(2,NCW)
  300   CONTINUE
!
!---    Equation ordering using J,K,I ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 305 I = 1,IFLD
        DO 305 K = 1,KFLD
        DO 305 J = 1,JFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 305
          NC = NC + 1
          IXP(N) = NC
  305   CONTINUE
!
!---    Initialize counter  ---
!
        NC = 0
        MHBC = 0
        MHBT = 0
!
!---    Coupled equations half band width using J,K,I ordering  ---
!
        DO 330 I = 1,IFLD
        DO 330 K = 1,KFLD
        DO 330 J = 1,JFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 330
          DO 310 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
  310     CONTINUE
          DO 311 M = 1,ISVC
            NC = NC + 1
            IM_M(M,NMD) = NC
  311     CONTINUE
!
!---      Loop over coupled wells, placing well equation
!         in the principal node of the well  ---
!
          DO 320 NCW = 1,N_CW
            IF( IWF_CW(ID_CW(7,NCW)).EQ.N ) THEN
              NC = NC+1
              JM_CW(NCW) = NC
            ENDIF
  320     CONTINUE
  330   CONTINUE
!
!---    Determine matrix half-band widths, using J,K,I ordering  ---
!
        DO 360 I = 1,IFLD
        DO 360 K = 1,KFLD
        DO 360 J = 1,JFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 360
!
!---      Node  ---
!
          MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Fracture to matrix connections  ---
!
          MHBC = MAX(MHBC,ABS(IM(1,NP)-IM_M(ISVC,NP)),
     &      ABS(IM(ISVC,NP)-IM_M(1,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              NB = IXP(N-IJFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &          ABS(IM(ISVC,NP)-IM(1,NB)))
              MHBT = MAX( MHBT,ABS(NP-NB) )
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              NS = IXP(N-IFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &          ABS(IM(ISVC,NP)-IM(1,NS)))
              MHBT = MAX( MHBT,ABS(NP-NS) )
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              NW = IXP(N-1)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &          ABS(IM(ISVC,NP)-IM(1,NW)))
              MHBT = MAX( MHBT,ABS(NP-NW) )
            ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              NE = IXP(N+1)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &          ABS(IM(ISVC,NP)-IM(1,NE)))
              MHBT = MAX( MHBT,ABS(NP-NE) )
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
            IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              NN = IXP(N+IFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &           ABS(IM(ISVC,NP)-IM(1,NN)))
              MHBT = MAX( MHBT,ABS(NP-NN) )
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
            IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              NT = IXP(N+IJFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &          ABS(IM(ISVC,NP)-IM(1,NT)))
              MHBT = MAX( MHBT,ABS(NP-NT) )
            ENDIF
          ENDIF
!
!---      Coupled well nodes  ---
!
          DO 350 NCW = 1,N_CW
            DO 340 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
              IF( IWF_CW(NWF).EQ.N ) THEN
                MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_CW(NCW)),
     &            ABS(IM(ISVC,NP)-JM_CW(NCW)))
              ENDIF
  340       CONTINUE
  350     CONTINUE
  360   CONTINUE
!
!---  K,I,J ordering yields the lowest half band width   ---
!
      ELSE
        NJD = LBD*(3*MHBC_KIJ+1) + LSP
        IF( NJD.GT.LJD ) THEN
          INDX = 5
          CHMSG = 'Number of Banded Matrix Rows ' //
     &      '> Parameter LJD'
          CALL WRMSGS( INDX )
        ENDIF
        DO 400 NCW = 1,LN_CW
          ID_CW(7,NCW) = M_KIJ(2,NCW)
  400   CONTINUE
!
!---    Equation ordering using K,I,J ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 405 J = 1,JFLD
        DO 405 I = 1,IFLD
        DO 405 K = 1,KFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 405
          NC = NC + 1
          IXP(N) = NC
  405   CONTINUE
!
!---    Initialize counter  ---
!
        NC = 0
        MHBC = 0
        MHBT = 0
!
!---    Coupled equations half band width using K,I,J ordering  ---
!
        DO 430 J = 1,JFLD
        DO 430 I = 1,IFLD
        DO 430 K = 1,KFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 430
          DO 410 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
  410     CONTINUE
          DO 411 M = 1,ISVC
            NC = NC+1
            IM_M(M,NMD) = NC
  411     CONTINUE
!
!---      Loop over coupled wells, placing well equation
!         in the principal node of the well  ---
!
          DO 420 NCW = 1,N_CW
            IF( IWF_CW(ID_CW(7,NCW)).EQ.N ) THEN
              NC = NC+1
              JM_CW(NCW) = NC
            ENDIF
  420     CONTINUE
  430   CONTINUE
!
!---    Determine matrix half-band widths, using K,I,J ordering  ---
!
        DO 460 J = 1,JFLD
        DO 460 I = 1,IFLD
        DO 460 K = 1,KFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 460
!
!---      Node  ---
!
          MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Fracture to matrix connections  ---
!
          MHBC = MAX(MHBC,ABS(IM(1,NP)-IM_M(ISVC,NP)),
     &      ABS(IM(ISVC,NP)-IM_M(1,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              NB = IXP(N-IJFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &          ABS(IM(ISVC,NP)-IM(1,NB)))
              MHBT = MAX( MHBT,ABS(NP-NB) )
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              NS = IXP(N-IFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &          ABS(IM(ISVC,NP)-IM(1,NS)))
              MHBT = MAX( MHBT,ABS(NP-NS) )
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              NW = IXP(N-1)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &          ABS(IM(ISVC,NP)-IM(1,NW)))
              MHBT = MAX( MHBT,ABS(NP-NW) )
            ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              NE = IXP(N+1)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &          ABS(IM(ISVC,NP)-IM(1,NE)))
              MHBT = MAX( MHBT,ABS(NP-NE) )
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
            IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              NN = IXP(N+IFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &           ABS(IM(ISVC,NP)-IM(1,NN)))
              MHBT = MAX( MHBT,ABS(NP-NN) )
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
            IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              NT = IXP(N+IJFLD)
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &          ABS(IM(ISVC,NP)-IM(1,NT)))
              MHBT = MAX( MHBT,ABS(NP-NT) )
            ENDIF
          ENDIF
!
!---      Coupled well nodes  ---
!
          DO 450 NCW = 1,N_CW
            DO 440 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
              IF( IWF_CW(NWF).EQ.N ) THEN
                MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_CW(NCW)),
     &            ABS(IM(ISVC,NP)-JM_CW(NCW)))
              ENDIF
  440       CONTINUE
  450     CONTINUE
  460   CONTINUE
      ENDIF
      MLC = MHBC
      MLT = ISVT*MHBT + ISVT - 1
      MUC = MHBC
      MUT = ISVT*MHBT + ISVT - 1
      MDC = MLC + MUC + 1
      MDT = MLT + MUT + 1
!
!---  SPLIB .or. Lis .or. PETSc Solver  ---
!
      IF( ILES.EQ.3 .OR. ILES.EQ.4 .OR. ILES.EQ.5 ) THEN
!
!---    X-Y Plane yields the lowest band width.
!       Load Jacobian matrix in the increment order I,J,K  ---
!
        IF( MHBC_IJK.LE.MHBC_JKI .AND. MHBC_IJK.LE.MHBC_KIJ ) THEN
          NC = 0
          NCC = 0




          NLU(1) = 1
          NLUC(1) = 1

          DO 800 K = 1,KFLD
          DO 800 J = 1,JFLD
          DO 800 I = 1,IFLD
            N = ND(I,J,K)
            IF( IXP(N).LE.0 ) CYCLE
            NP = IXP(N)
            DO 660 L = 1,ISVC
              NMD = IM(L,NP)
              MA = 0
!
!---          Node  ---
!
              DO 500 M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM(M,NP)

                KLU(NMD,M+MA) = NC

  500         CONTINUE
              MA = MA + ISVC
!
!---          Bottom node neighbors  ---
!
              DO 512 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                DO 510 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NB)

                  KLU(NMD,M+MA) = NC

  510           CONTINUE
                MA = MA + ISVC
  512         CONTINUE
!
!---          South node neighbors  ---
!
              DO 522 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                DO 520 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NS)

                  KLU(NMD,M+MA) = NC

  520           CONTINUE
                MA = MA + ISVC
  522         CONTINUE
!
!---          West node neighbors  ---
!
              DO 532 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                DO 530 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NW)

                  KLU(NMD,M+MA) = NC

  530           CONTINUE
                MA = MA + ISVC
  532         CONTINUE
!
!---          East node neighbors  ---
!
              DO 542 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                DO 540 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NE)

                  KLU(NMD,M+MA) = NC

  540           CONTINUE
                MA = MA + ISVC
  542         CONTINUE
!
!---          North node neighbors  ---
!
              DO 552 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                DO 550 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NN)

                  KLU(NMD,M+MA) = NC

  550           CONTINUE
                MA = MA + ISVC
  552         CONTINUE
!
!---          Top node neighbors  ---
!
              DO 562 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                DO 560 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NT)

                  KLU(NMD,M+MA) = NC

  560           CONTINUE
                MA = MA + ISVC
  562         CONTINUE
!
!---          Node contains a coupled well node,
!             include couple well equations in nodal equations  ---
!
              DO 650 NCW = 1,N_CW
                NWFX = ID_CW(6,NCW)-ID_CW(5,NCW)+1
                DO 640 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                  IF( IWF_CW(NWF).EQ.N ) THEN
                    MA = (NWFX*ISVC) + (NWF-ID_CW(5,NCW))*ISVC + 1
                    NC = NC+1




                    MLU(NC) = JM_CW(NCW)

                    KLU_CW(L+MA,NCW) = NC

                  ENDIF
  640           CONTINUE
  650         CONTINUE



              NLU(NMD+1) = NC+1

  660       CONTINUE
!
!---        Loop over matrix node equations  ---
!
            L1: DO L = 1,ISVC
              NMD = IM_M(L,NP)
!
!---          Matrix node equations ---
!
              DO M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM_M(M,NP)

                KLU(NMD,M) = NC

              ENDDO
!
!---          Fracture node equations  ---
!
              MA = ISVC
              DO M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM(M,NP)

                KLU(NMD,M+MA) = NC

              ENDDO



              NLU(NMD+1) = NC+1

            ENDDO L1
!
!---        Loop over coupled wells  ---
!
            DO 690 NCW = 1,N_CW
!
!---          Node contains principal coupled well node,
!             include coupled well equation  ---
!
              IF( IWF_CW(ID_CW(7,NCW)).EQ.N ) THEN
                NC = NC+1
                NMD = JM_CW(NCW)




                MLU(NC) = NMD

                KLU_CW(1,NCW) = NC

!
!---            Loop over nodes that contain coupled well nodes
!               include nodal equations in coupled well equation  ---
!
                DO 680 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                  NPX = IXP(IWF_CW(NWF))
                  MA = (NWF-ID_CW(5,NCW))*ISVC + 1
                  DO 670 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NPX)

                    KLU_CW(M+MA,NCW) = NC

  670             CONTINUE
  680           CONTINUE



                NLU(NMD+1) = NC+1

              ENDIF
  690       CONTINUE
!
!---        Solute transport equations  ---
!
            MA = 1
!
!---        Node  ---
!
            NCC = NCC+1




            MLUC(NCC) = NP

            KLUC(NP,MA) = NCC

            MA = MA + 1
!
!---        Bottom node neighbors  ---
!
            DO 702 MX = 1,4
              NB = ICM(MX,1,N)
              IF( NB.EQ.0 ) EXIT
              NB = IXP(NB)
              NCC = NCC+1




              MLUC(NCC) = NB

              KLUC(NP,MA) = NCC

              MA = MA + 1
  702       CONTINUE
!
!---        South node neighbors  ---
!
            DO 712 MX = 1,4
              NS = ICM(MX,2,N)
              IF( NS.EQ.0 ) EXIT
              NS = IXP(NS)
              NCC = NCC+1




              MLUC(NCC) = NS

              KLUC(NP,MA) = NCC

              MA = MA + 1
  712       CONTINUE
!
!---        West node neighbors  ---
!
            DO 722 MX = 1,4
              NW = ICM(MX,3,N)
              IF( NW.EQ.0 ) EXIT
              NW = IXP(NW)
              NCC = NCC+1




              MLUC(NCC) = NW

              KLUC(NP,MA) = NCC

              MA = MA + 1
  722       CONTINUE
!
!---        East node neighbors  ---
!
            DO 732 MX = 1,4
              NE = ICM(MX,4,N)
              IF( NE.EQ.0 ) EXIT
              NE = IXP(NE)
              NCC = NCC+1




              MLUC(NCC) = NE

              KLUC(NP,MA) = NCC

              MA = MA + 1
  732       CONTINUE
!
!---        North node neighbors  ---
!
            DO 742 MX = 1,4
              NN = ICM(MX,5,N)
              IF( NN.EQ.0 ) EXIT
              NN = IXP(NN)
              NCC = NCC+1




              MLUC(NCC) = NN

              KLUC(NP,MA) = NCC

              MA = MA + 1
  742       CONTINUE
!
!---        Top node neighbors  ---
!
            DO 752 MX = 1,4
              NT = ICM(MX,6,N)
              IF( NT.EQ.0 ) EXIT
              NT = IXP(NT)
              NCC = NCC+1




              MLUC(NCC) = NT

              KLUC(NP,MA) = NCC

              MA = MA + 1
  752       CONTINUE




            NLUC(NP+1) = NCC+1


  800     CONTINUE
          MKC = NC
          MKT = NCC
!
!---    Y-Z Plane yields the lowest band width.
!       Load Jacobian matrix in the increment order J,K,I  ---
!
        ELSEIF( MHBC_JKI.LE.MHBC_KIJ ) THEN
          NC = 0
          NCC = 0




          NLU(1) = 1
          NLUC(1) = 1

          DO 1800 I = 1,IFLD
          DO 1800 K = 1,KFLD
          DO 1800 J = 1,JFLD
            N = ND(I,J,K)
            IF(  IXP(N).LE.0 ) CYCLE
            NP = IXP(N)
            DO 1660 L = 1,ISVC
              NMD = IM(L,NP)
              MA = 0
!
!---          Node  ---
!
              DO 1500 M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM(M,NP)

                KLU(NMD,M+MA) = NC

 1500         CONTINUE
              MA = MA + ISVC
!
!---          Bottom node neighbors  ---
!
              DO 1512 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                DO 1510 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NB)

                  KLU(NMD,M+MA) = NC

 1510           CONTINUE
                MA = MA + ISVC
 1512         CONTINUE
!
!---          South node neighbors  ---
!
              DO 1522 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                DO 1520 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NS)

                  KLU(NMD,M+MA) = NC

 1520           CONTINUE
                MA = MA + ISVC
 1522         CONTINUE
!
!---          West node neighbors  ---
!
              DO 1532 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                DO 1530 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NW)

                  KLU(NMD,M+MA) = NC

 1530           CONTINUE
                MA = MA + ISVC
 1532         CONTINUE
!
!---          East node neighbors  ---
!
              DO 1542 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                DO 1540 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NE)

                  KLU(NMD,M+MA) = NC

 1540           CONTINUE
                MA = MA + ISVC
 1542         CONTINUE
!
!---          North node neighbors  ---
!
              DO 1552 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                DO 1550 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NN)

                  KLU(NMD,M+MA) = NC

 1550           CONTINUE
                MA = MA + ISVC
 1552         CONTINUE
!
!---          Top node neighbors  ---
!
              DO 1562 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                DO 1560 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NT)

                  KLU(NMD,M+MA) = NC

 1560           CONTINUE
                MA = MA + ISVC
 1562         CONTINUE
!
!---          Node contains a coupled well node,
!             include couple well equations in nodal equations  ---
!
              DO 1650 NCW = 1,N_CW
                NWFX = ID_CW(6,NCW)-ID_CW(5,NCW)+1
                DO 1640 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                  IF( IWF_CW(NWF).EQ.N ) THEN
                    MA = (NWFX*ISVC) + (NWF-ID_CW(5,NCW))*ISVC + 1
                    NC = NC+1




                    MLU(NC) = JM_CW(NCW)

                    KLU_CW(L+MA,NCW) = NC

                  ENDIF
 1640           CONTINUE
 1650         CONTINUE



              NLU(NMD+1) = NC+1

 1660       CONTINUE
!
!---        Loop over matrix node equations  ---
!
            L2: DO L = 1,ISVC
              NMD = IM_M(L,NP)
!
!---          Matrix node equations ---
!
              DO M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM_M(M,NP)

                KLU(NMD,M) = NC

              ENDDO
!
!---          Fracture node equations  ---
!
              MA = ISVC
              DO M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM(M,NP)

                KLU(NMD,M+MA) = NC

              ENDDO



              NLU(NMD+1) = NC+1

            ENDDO L2
!
!---        Loop over coupled wells  ---
!
            DO 1690 NCW = 1,N_CW
!
!---          Node contains principal coupled well node,
!             include coupled well equation  ---
!
              IF( IWF_CW(ID_CW(7,NCW)).EQ.N ) THEN
                NC = NC+1
                NMD = JM_CW(NCW)




                MLU(NC) = NMD

                KLU_CW(1,NCW) = NC

!
!---            Loop over nodes that contain coupled well nodes
!               include nodal equations in coupled well equation  ---
!
                DO 1680 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                  NPX = IXP(IWF_CW(NWF))
                  MA = (NWF-ID_CW(5,NCW))*ISVC + 1
                  DO 1670 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NPX)

                    KLU_CW(M+MA,NCW) = NC

 1670             CONTINUE
 1680           CONTINUE



                NLU(NMD+1) = NC+1

              ENDIF
 1690       CONTINUE
!
!---        Solute transport equations  ---
!
            MA = 1
!
!---        Node  ---
!
            NCC = NCC+1




            MLUC(NCC) = NP

            KLUC(NP,MA) = NCC

            MA = MA + 1
!
!---        Bottom node neighbors  ---
!
            DO 1702 MX = 1,4
              NB = ICM(MX,1,N)
              IF( NB.EQ.0 ) EXIT
              NB = IXP(NB)
              NCC = NCC+1




              MLUC(NCC) = NB

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1702       CONTINUE
!
!---        South node neighbors  ---
!
            DO 1712 MX = 1,4
              NS = ICM(MX,2,N)
              IF( NS.EQ.0 ) EXIT
              NS = IXP(NS)
              NCC = NCC+1




              MLUC(NCC) = NS

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1712       CONTINUE
!
!---        West node neighbors  ---
!
            DO 1722 MX = 1,4
              NW = ICM(MX,3,N)
              IF( NW.EQ.0 ) EXIT
              NW = IXP(NW)
              NCC = NCC+1




              MLUC(NCC) = NW

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1722       CONTINUE
!
!---        East node neighbors  ---
!
            DO 1732 MX = 1,4
              NE = ICM(MX,4,N)
              IF( NE.EQ.0 ) EXIT
              NE = IXP(NE)
              NCC = NCC+1




              MLUC(NCC) = NE

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1732       CONTINUE
!
!---        North node neighbors  ---
!
            DO 1742 MX = 1,4
              NN = ICM(MX,5,N)
              IF( NN.EQ.0 ) EXIT
              NN = IXP(NN)
              NCC = NCC+1




              MLUC(NCC) = NN

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1742       CONTINUE
!
!---        Top node neighbors  ---
!
            DO 1752 MX = 1,4
              NT = ICM(MX,6,N)
              IF( NT.EQ.0 ) EXIT
              NT = IXP(NT)
              NCC = NCC+1




              MLUC(NCC) = NT

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1752       CONTINUE




            NLUC(NP+1) = NCC+1


 1800     CONTINUE
          MKC = NC
          MKT = NCC
!
!---    Z-X Plane yields the lowest band width.
!       Load Jacobian matrix in the increment order K,I,J  ---
!
        ELSE
          NC = 0
          NCC = 0




          NLU(1) = 1
          NLUC(1) = 1

          DO 2800 J = 1,JFLD
          DO 2800 I = 1,IFLD
          DO 2800 K = 1,KFLD
            N = ND(I,J,K)
            IF( IXP(N).LE.0 ) CYCLE
            NP = IXP(N)
            DO 2660 L = 1,ISVC
              NMD = IM(L,NP)
              MA = 0
!
!---          Node  ---
!
              DO 2500 M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM(M,NP)

                KLU(NMD,M+MA) = NC

 2500         CONTINUE
              MA = MA + ISVC
!
!---          Bottom node neighbors  ---
!
              DO 2512 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                DO 2510 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NB)

                  KLU(NMD,M+MA) = NC

 2510           CONTINUE
                MA = MA + ISVC
 2512         CONTINUE
!
!---          South node neighbors  ---
!
              DO 2522 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                DO 2520 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NS)

                  KLU(NMD,M+MA) = NC

 2520           CONTINUE
                MA = MA + ISVC
 2522         CONTINUE
!
!---          West node neighbors  ---
!
              DO 2532 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                DO 2530 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NW)

                  KLU(NMD,M+MA) = NC

 2530           CONTINUE
                MA = MA + ISVC
 2532         CONTINUE
!
!---          East node neighbors  ---
!
              DO 2542 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                DO 2540 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NE)

                  KLU(NMD,M+MA) = NC

 2540           CONTINUE
                MA = MA + ISVC
 2542         CONTINUE
!
!---          North node neighbors  ---
!
              DO 2552 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                DO 2550 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NN)

                  KLU(NMD,M+MA) = NC

 2550           CONTINUE
                MA = MA + ISVC
 2552         CONTINUE
!
!---          Top node  ---
!
              DO 2562 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                DO 2560 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NT)

                  KLU(NMD,M+MA) = NC

 2560           CONTINUE
                MA = MA + ISVC
 2562         CONTINUE
!
!---          Node contains a coupled well node,
!             include couple well equations in nodal equations  ---
!
              DO 2650 NCW = 1,N_CW
                NWFX = ID_CW(6,NCW)-ID_CW(5,NCW)+1
                DO 2640 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                  IF( IWF_CW(NWF).EQ.N ) THEN
                    MA = (NWFX*ISVC) + (NWF-ID_CW(5,NCW))*ISVC + 1
                    NC = NC+1




                    MLU(NC) = JM_CW(NCW)

                    KLU_CW(L+MA,NCW) = NC

                  ENDIF
 2640           CONTINUE
 2650         CONTINUE



              NLU(NMD+1) = NC+1

 2660       CONTINUE
!
!---        Loop over matrix node equations  ---
!
            L3: DO L = 1,ISVC
              NMD = IM_M(L,NP)
!
!---          Matrix node equations ---
!
              DO M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM_M(M,NP)

                KLU(NMD,M) = NC

              ENDDO
!
!---          Fracture node equations  ---
!
              MA = ISVC
              DO M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM(M,NP)

                KLU(NMD,M+MA) = NC

              ENDDO



              NLU(NMD+1) = NC+1

            ENDDO L3
!
!---        Loop over coupled wells  ---
!
            DO 2690 NCW = 1,N_CW
!
!---          Node contains principal coupled well node,
!             include coupled well equation  ---
!
              IF( IWF_CW(ID_CW(7,NCW)).EQ.N ) THEN
                NC = NC+1
                NMD = JM_CW(NCW)




                MLU(NC) = NMD

                KLU_CW(1,NCW) = NC

!
!---            Loop over nodes that contain coupled well nodes
!               include nodal equations in coupled well equation  ---
!
                DO 2680 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                  NPX = IXP(IWF_CW(NWF))
                  MA = (NWF-ID_CW(5,NCW))*ISVC + 1
                  DO 2670 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NPX)

                    KLU_CW(M+MA,NCW) = NC

 2670             CONTINUE
 2680           CONTINUE



                NLU(NMD+1) = NC+1

              ENDIF
 2690       CONTINUE
!
!---        Solute transport equations  ---
!
            MA = 1
!
!---        Node  ---
!
            NCC = NCC+1




            MLUC(NCC) = NP

            KLUC(NP,MA) = NCC

            MA = MA + 1
!
!---        Bottom node neighbors  ---
!
            DO 2702 MX = 1,4
              NB = ICM(MX,1,N)
              IF( NB.EQ.0 ) EXIT
              NB = IXP(NB)
              NCC = NCC+1




              MLUC(NCC) = NB

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2702       CONTINUE
!
!---        South node neighbors  ---
!
            DO 2712 MX = 1,4
              NS = ICM(MX,2,N)
              IF( NS.EQ.0 ) EXIT
              NS = IXP(NS)
              NCC = NCC+1




              MLUC(NCC) = NS

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2712       CONTINUE
!
!---        West node neighbors  ---
!
            DO 2722 MX = 1,4
              NW = ICM(MX,3,N)
              IF( NW.EQ.0 ) EXIT
              NW = IXP(NW)
              NCC = NCC+1




              MLUC(NCC) = NW

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2722       CONTINUE
!
!---        East node neighbors  ---
!
            DO 2732 MX = 1,4
              NE = ICM(MX,4,N)
              IF( NE.EQ.0 ) EXIT
              NE = IXP(NE)
              NCC = NCC+1




              MLUC(NCC) = NE

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2732       CONTINUE
!
!---        North node neighbors  ---
!
            DO 2742 MX = 1,4
              NN = ICM(MX,5,N)
              IF( NN.EQ.0 ) EXIT
              NN = IXP(NN)
              NCC = NCC+1




              MLUC(NCC) = NN

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2742       CONTINUE
!
!---        Top node neighbors  ---
!
            DO 2752 MX = 1,4
              NT = ICM(MX,6,N)
              IF( NT.EQ.0 ) EXIT
              NT = IXP(NT)
              NCC = NCC+1




              MLUC(NCC) = NT

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2752       CONTINUE




          NLUC(NP+1) = NCC+1


 2800     CONTINUE
          MKC = NC
          MKT = NCC
        ENDIF
      ENDIF
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCBP_CW_DP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCBP_NCW
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
!     Configure the Jacobian matrix pointer arrays.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 April 2011.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PTZR
      USE JACOB
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
      SUB_LOG(ISUB_LOG) = '/JCBP_NCW'
      IF( INDEX(SVN_ID(95)(1:1),'$').EQ.0 ) SVN_ID(95) =
     & '$Id: jcbp.F 1080 2017-03-14 16:22:02Z d3c002 $'
      ILIMIT = 2**30
!
!---  Coupled equations half band width using I,J,K ordering  ---
!
      NC = 0
      DO 5 K = 1,KFLD
      DO 5 J = 1,JFLD
      DO 5 I = 1,IFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 5
        NC = NC + 1
        IXP(N) = NC
    5 CONTINUE
      M_IJK = ILIMIT
      NC = 0
      MHBC_IJK = 0
      DO 20 K = 1,KFLD
      DO 20 J = 1,JFLD
      DO 20 I = 1,IFLD
        N = ND(I,J,K)
        NMD = ABS(IXP(N))
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 20
        DO 10 M = 1,ISVC
          NC = NC+1
          IM(M,NMD) = NC
   10   CONTINUE
   20 CONTINUE
!
!---  Determine matrix half-band widths, using I,J,K ordering  ---
!
      DO 40 K = 1,KFLD
      DO 40 J = 1,JFLD
      DO 40 I = 1,IFLD
        N = ND(I,J,K)
        NP = ABS(IXP(N))
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 40
!
!---    Node  ---
!
        MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---    Bottom node neighbors  ---
!
        IF( K.GT.1 ) THEN
          IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
            NB = ABS(IXP(N-IJFLD))
            MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NB)),
     &        ABS(IM(ISVC,NP)-IM(1,NB)))
          ENDIF
        ENDIF
!
!---    South node neighbors  ---
!
        IF( J.GT.1 ) THEN
          IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
            NS = ABS(IXP(N-IFLD))
            MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NS)),
     &        ABS(IM(ISVC,NP)-IM(1,NS)))
          ENDIF
        ENDIF
!
!---    West node neighbors  ---
!
        IF( I.GT.1 ) THEN
          IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
            NW = ABS(IXP(N-1))
            MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NW)),
     &        ABS(IM(ISVC,NP)-IM(1,NW)))
          ENDIF
        ENDIF
!
!---    East node neighbors  ---
!
        IF( I.LT.IFLD ) THEN
          IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
            NE = ABS(IXP(N+1))
            MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NE)),
     &        ABS(IM(ISVC,NP)-IM(1,NE)))
          ENDIF
        ENDIF
!
!---    North node neighbors  ---
!
        IF( J.LT.JFLD ) THEN
          IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
            NN = ABS(IXP(N+IFLD))
            MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NN)),
     &         ABS(IM(ISVC,NP)-IM(1,NN)))
          ENDIF
        ENDIF
!
!---    Top node neighbors  ---
!
        IF( K.LT.KFLD ) THEN
          IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
            NT = ABS(IXP(N+IJFLD))
            MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NT)),
     &        ABS(IM(ISVC,NP)-IM(1,NT)))
          ENDIF
        ENDIF
   40 CONTINUE
      IF( MHBC_IJK.LT.M_IJK ) THEN
        M_IJK = MHBC_IJK
      ENDIF
!
!---  Coupled equations half band width using J,K,I ordering  ---
!
      NC = 0
      DO 65 I = 1,IFLD
      DO 65 K = 1,KFLD
      DO 65 J = 1,JFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 65
        NC = NC + 1
        IXP(N) = NC
   65 CONTINUE
      M_JKI = ILIMIT
      NC = 0
      MHBC_JKI = 0
      DO 80 I = 1,IFLD
      DO 80 K = 1,KFLD
      DO 80 J = 1,JFLD
        N = ND(I,J,K)
        NMD = ABS(IXP(N))
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 80
        DO 70 M = 1,ISVC
          NC = NC+1
          IM(M,NMD) = NC
   70   CONTINUE
   80 CONTINUE
!
!---  Determine matrix half-band widths, using J,K,I ordering  ---
!
      DO 100 I = 1,IFLD
      DO 100 K = 1,KFLD
      DO 100 J = 1,JFLD
        N = ND(I,J,K)
        NP = ABS(IXP(N))
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 100
!
!---    Node  ---
!
        MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---    Bottom node neighbors  ---
!
        IF( K.GT.1 ) THEN
          IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
            NB = ABS(IXP(N-IJFLD))
            MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NB)),
     &        ABS(IM(ISVC,NP)-IM(1,NB)))
          ENDIF
        ENDIF
!
!---    South node neighbors  ---
!
        IF( J.GT.1 ) THEN
          IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
            NS = ABS(IXP(N-IFLD))
            MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NS)),
     &        ABS(IM(ISVC,NP)-IM(1,NS)))
          ENDIF
        ENDIF
!
!---    West node neighbors  ---
!
        IF( I.GT.1 ) THEN
          IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
            NW = ABS(IXP(N-1))
            MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NW)),
     &        ABS(IM(ISVC,NP)-IM(1,NW)))
          ENDIF
        ENDIF
!
!---    East node neighbors  ---
!
        IF( I.LT.IFLD ) THEN
          IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
            NE = ABS(IXP(N+1))
            MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NE)),
     &        ABS(IM(ISVC,NP)-IM(1,NE)))
          ENDIF
        ENDIF
!
!---    North node neighbors  ---
!
        IF( J.LT.JFLD ) THEN
          IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
            NN = ABS(IXP(N+IFLD))
            MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NN)),
     &         ABS(IM(ISVC,NP)-IM(1,NN)))
          ENDIF
        ENDIF
!
!---    Top node neighbors  ---
!
        IF( K.LT.KFLD ) THEN
          IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
            NT = ABS(IXP(N+IJFLD))
            MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NT)),
     &        ABS(IM(ISVC,NP)-IM(1,NT)))
          ENDIF
        ENDIF
  100 CONTINUE
      IF( MHBC_JKI.LT.M_JKI ) THEN
        M_JKI = MHBC_JKI
      ENDIF
!
!---  Coupled equations half band width using K,I,J ordering  ---
!
      NC = 0
      DO 125 J = 1,JFLD
      DO 125 I = 1,IFLD
      DO 125 K = 1,KFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 125
        NC = NC + 1
        IXP(N) = NC
  125 CONTINUE
      M_KIJ = ILIMIT
      NC = 0
      MHBC_KIJ = 0
      DO 140 J = 1,JFLD
      DO 140 I = 1,IFLD
      DO 140 K = 1,KFLD
        N = ND(I,J,K)
        NMD = ABS(IXP(N))
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 140
        DO 130 M = 1,ISVC
          NC = NC+1
          IM(M,NMD) = NC
  130   CONTINUE
  140 CONTINUE
!
!---  Determine matrix half-band widths, using K,I,J ordering  ---
!
      DO 160 J = 1,JFLD
      DO 160 I = 1,IFLD
      DO 160 K = 1,KFLD
        N = ND(I,J,K)
        NP = ABS(IXP(N))
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 160
!
!---    Node  ---
!
        MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---    Bottom node neighbors  ---
!
        IF( K.GT.1 ) THEN
          IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
            NB = ABS(IXP(N-IJFLD))
            MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NB)),
     &        ABS(IM(ISVC,NP)-IM(1,NB)))
          ENDIF
        ENDIF
!
!---    South node neighbors  ---
!
        IF( J.GT.1 ) THEN
          IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
            NS = ABS(IXP(N-IFLD))
            MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NS)),
     &        ABS(IM(ISVC,NP)-IM(1,NS)))
          ENDIF
        ENDIF
!
!---    West node neighbors  ---
!
        IF( I.GT.1 ) THEN
          IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
            NW = ABS(IXP(N-1))
            MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NW)),
     &        ABS(IM(ISVC,NP)-IM(1,NW)))
          ENDIF
        ENDIF
!
!---    East node neighbors  ---
!
        IF( I.LT.IFLD ) THEN
          IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
            NE = ABS(IXP(N+1))
            MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NE)),
     &        ABS(IM(ISVC,NP)-IM(1,NE)))
          ENDIF
        ENDIF
!
!---    North node neighbors  ---
!
        IF( J.LT.JFLD ) THEN
          IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
            NN = ABS(IXP(N+IFLD))
            MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NN)),
     &         ABS(IM(ISVC,NP)-IM(1,NN)))
          ENDIF
        ENDIF
!
!---    Top node neighbors  ---
!
        IF( K.LT.KFLD ) THEN
          IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
            NT = ABS(IXP(N+IJFLD))
            MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NT)),
     &        ABS(IM(ISVC,NP)-IM(1,NT)))
          ENDIF
        ENDIF
  160 CONTINUE
      IF( MHBC_KIJ.LT.M_KIJ ) THEN
        M_KIJ = MHBC_KIJ
      ENDIF
!
!---  Find the maximum half band width   ---
!
      MHBC_IJK = 0
      MHBC_JKI = 0
      MHBC_KIJ = 0
      IF( M_IJK.GT.MHBC_IJK ) MHBC_IJK = M_IJK
      IF( M_JKI.GT.MHBC_JKI ) MHBC_JKI = M_JKI
      IF( M_KIJ.GT.MHBC_KIJ ) MHBC_KIJ = M_KIJ








!
!---  I,J,K ordering yields the lowest half band width   ---
!
      IF( MHBC_IJK.LE.MHBC_JKI .AND. MHBC_IJK.LE.MHBC_KIJ ) THEN
        NJD = LBD*(3*MHBC_IJK+1) + LSP
        IF( NJD.GT.LJD ) THEN
          INDX = 5
          CHMSG = 'Number of Banded Matrix Rows ' //
     &      '> Parameter LJD'
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Equation ordering using I,J,K ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 205 K = 1,KFLD
        DO 205 J = 1,JFLD
        DO 205 I = 1,IFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 205
          NC = NC + 1
          IXP(N) = NC
  205   CONTINUE
!
!---    Initialize counter  ---
!
        NC = 0
        MHBC = 0
        MHBT = 0
!
!---    Coupled equations half band width using I,J,K ordering  ---
!
        DO 230 K = 1,KFLD
        DO 230 J = 1,JFLD
        DO 230 I = 1,IFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 230
          DO 210 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
  210     CONTINUE
  230   CONTINUE
!
!---    Determine matrix half-band widths, using I,J,K ordering  ---
!
        DO 260 K = 1,KFLD
        DO 260 J = 1,JFLD
        DO 260 I = 1,IFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 260
!
!---      Node  ---
!
          MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              NB = ABS(IXP(N-IJFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &          ABS(IM(ISVC,NP)-IM(1,NB)))
              MHBT = MAX( MHBT,ABS(NP-NB) )
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              NS = ABS(IXP(N-IFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &          ABS(IM(ISVC,NP)-IM(1,NS)))
              MHBT = MAX( MHBT,ABS(NP-NS) )
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              NW = ABS(IXP(N-1))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &          ABS(IM(ISVC,NP)-IM(1,NW)))
              MHBT = MAX( MHBT,ABS(NP-NW) )
            ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              NE = ABS(IXP(N+1))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &          ABS(IM(ISVC,NP)-IM(1,NE)))
              MHBT = MAX( MHBT,ABS(NP-NE) )
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
            IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              NN = ABS(IXP(N+IFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &           ABS(IM(ISVC,NP)-IM(1,NN)))
              MHBT = MAX( MHBT,ABS(NP-NN) )
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
            IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              NT = ABS(IXP(N+IJFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &          ABS(IM(ISVC,NP)-IM(1,NT)))
              MHBT = MAX( MHBT,ABS(NP-NT) )
            ENDIF
          ENDIF
  260   CONTINUE
!
!---  J,K,I ordering yields the lowest half band width   ---
!
      ELSEIF( MHBC_JKI.LE.MHBC_KIJ ) THEN
        NJD = LBD*(3*MHBC_JKI+1) + LSP
        IF( NJD.GT.LJD ) THEN
          INDX = 5
          CHMSG = 'Number of Banded Matrix Rows ' //
     &      '> Parameter LJD'
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Equation ordering using J,K,I ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 305 I = 1,IFLD
        DO 305 K = 1,KFLD
        DO 305 J = 1,JFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 305
          NC = NC + 1
          IXP(N) = NC
  305   CONTINUE
!
!---    Initialize counter  ---
!
        NC = 0
        MHBC = 0
        MHBT = 0
!
!---    Coupled equations half band width using J,K,I ordering  ---
!
        DO 330 I = 1,IFLD
        DO 330 K = 1,KFLD
        DO 330 J = 1,JFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 330
          DO 310 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
  310     CONTINUE
  330   CONTINUE
!
!---    Determine matrix half-band widths using J,K,I ordering  ---
!
        DO 360 I = 1,IFLD
        DO 360 K = 1,KFLD
        DO 360 J = 1,JFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 360
!
!---      Node  ---
!
          MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              NB = ABS(IXP(N-IJFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &          ABS(IM(ISVC,NP)-IM(1,NB)))
              MHBT = MAX( MHBT,ABS(NP-NB) )
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              NS = ABS(IXP(N-IFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &          ABS(IM(ISVC,NP)-IM(1,NS)))
              MHBT = MAX( MHBT,ABS(NP-NS) )
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              NW = ABS(IXP(N-1))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &          ABS(IM(ISVC,NP)-IM(1,NW)))
              MHBT = MAX( MHBT,ABS(NP-NW) )
            ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              NE = ABS(IXP(N+1))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &          ABS(IM(ISVC,NP)-IM(1,NE)))
              MHBT = MAX( MHBT,ABS(NP-NE) )
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
            IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              NN = ABS(IXP(N+IFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &           ABS(IM(ISVC,NP)-IM(1,NN)))
              MHBT = MAX( MHBT,ABS(NP-NN) )
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
            IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              NT = ABS(IXP(N+IJFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &          ABS(IM(ISVC,NP)-IM(1,NT)))
              MHBT = MAX( MHBT,ABS(NP-NT) )
            ENDIF
          ENDIF
  360   CONTINUE
!
!---  K,I,J ordering yields the lowest half band width   ---
!
      ELSE
        NJD = LBD*(3*MHBC_KIJ+1) + LSP
        IF( NJD.GT.LJD ) THEN
          INDX = 5
          CHMSG = 'Number of Banded Matrix Rows ' //
     &      '> Parameter LJD'
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Equation ordering using K,I,J ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 405 J = 1,JFLD
        DO 405 I = 1,IFLD
        DO 405 K = 1,KFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 405
          NC = NC + 1
          IXP(N) = NC
  405   CONTINUE
!
!---    Initialize counter  ---
!
        NC = 0
        MHBC = 0
        MHBT = 0
!
!---    Coupled equations half band width using K,I,J ordering  ---
!
        DO 430 J = 1,JFLD
        DO 430 I = 1,IFLD
        DO 430 K = 1,KFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 430
          DO 410 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
  410     CONTINUE
  430   CONTINUE
!
!---    Determine matrix half-band widths using K,I,J ordering  ---
!
        DO 460 J = 1,JFLD
        DO 460 I = 1,IFLD
        DO 460 K = 1,KFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 460
!
!---      Node  ---
!
          MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              NB = ABS(IXP(N-IJFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &          ABS(IM(ISVC,NP)-IM(1,NB)))
              MHBT = MAX( MHBT,ABS(NP-NB) )
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              NS = ABS(IXP(N-IFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &          ABS(IM(ISVC,NP)-IM(1,NS)))
              MHBT = MAX( MHBT,ABS(NP-NS) )
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              NW = ABS(IXP(N-1))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &          ABS(IM(ISVC,NP)-IM(1,NW)))
              MHBT = MAX( MHBT,ABS(NP-NW) )
            ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              NE = ABS(IXP(N+1))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &          ABS(IM(ISVC,NP)-IM(1,NE)))
              MHBT = MAX( MHBT,ABS(NP-NE) )
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
            IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              NN = ABS(IXP(N+IFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &           ABS(IM(ISVC,NP)-IM(1,NN)))
              MHBT = MAX( MHBT,ABS(NP-NN) )
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
            IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              NT = ABS(IXP(N+IJFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &          ABS(IM(ISVC,NP)-IM(1,NT)))
              MHBT = MAX( MHBT,ABS(NP-NT) )
            ENDIF
          ENDIF
  460   CONTINUE
      ENDIF
      MLC = MHBC
      MLT = ISVT*MHBT + ISVT - 1
      MUC = MHBC
      MUT = ISVT*MHBT + ISVT - 1
      MDC = MLC + MUC + 1
      MDT = MLT + MUT + 1
!
!---  SPLIB .or. Lis .or. PETSc Solver  ---
!
      IF( ILES.EQ.3 .OR. ILES.EQ.4 .OR. ILES.EQ.5 ) THEN
!
!---    X-Y Plane yields the lowest band width.
!       Load Jacobian matrix in the increment order I,J,K  ---
!
        IF( MHBC_IJK.LE.MHBC_JKI .AND. MHBC_IJK.LE.MHBC_KIJ ) THEN
          NC = 0
          NCC = 0




          NLU(1) = 1
          NLUC(1) = 1

          DO 800 K = 1,KFLD
          DO 800 J = 1,JFLD
          DO 800 I = 1,IFLD
            N = ND(I,J,K)
            IF( IXP(N).EQ.0 ) CYCLE
            NP = IXP(N)
            DO 660 L = 1,ISVC
              NMD = IM(L,NP)
              MA = 0
!
!---          Node  ---
!
              DO 500 M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM(M,NP)

                KLU(NMD,M+MA) = NC

  500         CONTINUE
              MA = MA + ISVC
!
!---          Bottom node neighbors  ---
!
              DO 512 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                DO 510 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NB)

                  KLU(NMD,M+MA) = NC

  510           CONTINUE
                MA = MA + ISVC
  512         CONTINUE
!
!---          South node neighbors  ---
!
              DO 522 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                DO 520 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NS)

                  KLU(NMD,M+MA) = NC

  520           CONTINUE
                MA = MA + ISVC
  522         CONTINUE
!
!---          West node neighbors  ---
!
              DO 532 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                DO 530 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NW)

                  KLU(NMD,M+MA) = NC

  530           CONTINUE
                MA = MA + ISVC
  532         CONTINUE
!
!---          East node neighbors  ---
!
              DO 542 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                DO 540 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NE)

                  KLU(NMD,M+MA) = NC

  540           CONTINUE
                MA = MA + ISVC
  542         CONTINUE
!
!---          North node neighbors  ---
!
              DO 552 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                DO 550 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NN)

                  KLU(NMD,M+MA) = NC

  550           CONTINUE
                MA = MA + ISVC
  552         CONTINUE
!
!---          Top node neighbors  ---
!
              DO 562 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                DO 560 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NT)

                  KLU(NMD,M+MA) = NC

  560           CONTINUE
                MA = MA + ISVC
  562         CONTINUE



              NLU(NMD+1) = NC+1

  660       CONTINUE
!
!---        Solute transport equations  ---
!
            MA = 1
!
!---        Node  ---
!
            NCC = NCC+1




            MLUC(NCC) = NP

            KLUC(NP,MA) = NCC

            MA = MA + 1
!
!---        Bottom node neighbors  ---
!
            DO 702 MX = 1,4
              NB = ICM(MX,1,N)
              IF( NB.EQ.0 ) EXIT
              NB = IXP(NB)
              NCC = NCC+1




              MLUC(NCC) = NB

              KLUC(NP,MA) = NCC

              MA = MA + 1
  702       CONTINUE
!
!---        South node neighbors  ---
!
            DO 712 MX = 1,4
              NS = ICM(MX,2,N)
              IF( NS.EQ.0 ) EXIT
              NS = IXP(NS)
              NCC = NCC+1




              MLUC(NCC) = NS

              KLUC(NP,MA) = NCC

              MA = MA + 1
  712       CONTINUE
!
!---        West node neighbors  ---
!
            DO 722 MX = 1,4
              NW = ICM(MX,3,N)
              IF( NW.EQ.0 ) EXIT
              NW = IXP(NW)
              NCC = NCC+1




              MLUC(NCC) = NW

              KLUC(NP,MA) = NCC

              MA = MA + 1
  722       CONTINUE
!
!---        East node neighbors  ---
!
            DO 732 MX = 1,4
              NE = ICM(MX,4,N)
              IF( NE.EQ.0 ) EXIT
              NE = IXP(NE)
              NCC = NCC+1




              MLUC(NCC) = NE

              KLUC(NP,MA) = NCC

              MA = MA + 1
  732       CONTINUE
!
!---        North node neighbors  ---
!
            DO 742 MX = 1,4
              NN = ICM(MX,5,N)
              IF( NN.EQ.0 ) EXIT
              NN = IXP(NN)
              NCC = NCC+1




              MLUC(NCC) = NN

              KLUC(NP,MA) = NCC

              MA = MA + 1
  742       CONTINUE
!
!---        Top node neighbors  ---
!
            DO 752 MX = 1,4
              NT = ICM(MX,6,N)
              IF( NT.EQ.0 ) EXIT
              NT = IXP(NT)
              NCC = NCC+1




              MLUC(NCC) = NT

              KLUC(NP,MA) = NCC

              MA = MA + 1
  752       CONTINUE




            NLUC(NP+1) = NCC+1


  800     CONTINUE
          MKC = NC
          MKT = NCC
!
!---    Y-Z Plane yields the lowest band width.
!       Load Jacobian matrix in the increment order J,K,I  ---
!
        ELSEIF( MHBC_JKI.LE.MHBC_KIJ ) THEN
          NC = 0
          NCC = 0




          NLU(1) = 1
          NLUC(1) = 1

          DO 1800 I = 1,IFLD
          DO 1800 K = 1,KFLD
          DO 1800 J = 1,JFLD
            N = ND(I,J,K)
            IF( IXP(N).EQ.0 ) CYCLE
            NP = IXP(N)
            DO 1660 L = 1,ISVC
              NMD = IM(L,NP)
              MA = 0
!
!---          Node  ---
!
              DO 1500 M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM(M,NP)

                KLU(NMD,M+MA) = NC

 1500         CONTINUE
              MA = MA + ISVC
!
!---          Bottom node neighbors  ---
!
              DO 1512 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                DO 1510 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NB)

                  KLU(NMD,M+MA) = NC

 1510           CONTINUE
                MA = MA + ISVC
 1512         CONTINUE
!
!---          South node neighbors  ---
!
              DO 1522 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                DO 1520 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NS)

                  KLU(NMD,M+MA) = NC

 1520           CONTINUE
                MA = MA + ISVC
 1522         CONTINUE
!
!---          West node neighbors  ---
!
              DO 1532 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                DO 1530 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NW)

                  KLU(NMD,M+MA) = NC

 1530           CONTINUE
                MA = MA + ISVC
 1532         CONTINUE
!
!---          East node neighbors  ---
!
              DO 1542 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                DO 1540 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NE)

                  KLU(NMD,M+MA) = NC

 1540           CONTINUE
                MA = MA + ISVC
 1542         CONTINUE
!
!---          North node neighbors  ---
!
              DO 1552 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                DO 1550 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NN)

                  KLU(NMD,M+MA) = NC

 1550           CONTINUE
                MA = MA + ISVC
 1552         CONTINUE
!
!---          Top node neighbors  ---
!
              DO 1562 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                DO 1560 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NT)

                  KLU(NMD,M+MA) = NC

 1560           CONTINUE
                MA = MA + ISVC
 1562         CONTINUE



              NLU(NMD+1) = NC+1

 1660       CONTINUE
!
!---        Solute transport equations  ---
!
            MA = 1
!
!---        Node  ---
!
            NCC = NCC+1




            MLUC(NCC) = NP

            KLUC(NP,MA) = NCC

            MA = MA + 1
!
!---        Bottom node neighbors  ---
!
            DO 1702 MX = 1,4
              NB = ICM(MX,1,N)
              IF( NB.EQ.0 ) EXIT
              NB = IXP(NB)
              NCC = NCC+1




              MLUC(NCC) = NB

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1702       CONTINUE
!
!---        South node neighbors  ---
!
            DO 1712 MX = 1,4
              NS = ICM(MX,2,N)
              IF( NS.EQ.0 ) EXIT
              NS = IXP(NS)
              NCC = NCC+1




              MLUC(NCC) = NS

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1712       CONTINUE
!
!---        West node neighbors  ---
!
            DO 1722 MX = 1,4
              NW = ICM(MX,3,N)
              IF( NW.EQ.0 ) EXIT
              NW = IXP(NW)
              NCC = NCC+1




              MLUC(NCC) = NW

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1722       CONTINUE
!
!---        East node neighbors  ---
!
            DO 1732 MX = 1,4
              NE = ICM(MX,4,N)
              IF( NE.EQ.0 ) EXIT
              NE = IXP(NE)
              NCC = NCC+1




              MLUC(NCC) = NE

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1732       CONTINUE
!
!---        North node neighbors  ---
!
            DO 1742 MX = 1,4
              NN = ICM(MX,5,N)
              IF( NN.EQ.0 ) EXIT
              NN = IXP(NN)
              NCC = NCC+1




              MLUC(NCC) = NN

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1742       CONTINUE
!
!---        Top node neighbors  ---
!
            DO 1752 MX = 1,4
              NT = ICM(MX,6,N)
              IF( NT.EQ.0 ) EXIT
              NT = IXP(NT)
              NCC = NCC+1




              MLUC(NCC) = NT

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1752       CONTINUE




            NLUC(NP+1) = NCC+1


 1800     CONTINUE
          MKC = NC
          MKT = NCC
!
!---    Z-X Plane yields the lowest band width.
!       Load Jacobian matrix in the increment order K,I,J  ---
!
        ELSE
          NC = 0
          NCC = 0




          NLU(1) = 1
          NLUC(1) = 1

          DO 2800 J = 1,JFLD
          DO 2800 I = 1,IFLD
          DO 2800 K = 1,KFLD
            N = ND(I,J,K)
            IF( IXP(N).EQ.0 ) CYCLE
            NP = IXP(N)
            DO 2660 L = 1,ISVC
              NMD = IM(L,NP)
              MA = 0
!
!---          Node  ---
!
              DO 2500 M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM(M,NP)

                KLU(NMD,M+MA) = NC

 2500         CONTINUE
              MA = MA + ISVC
!
!---          Bottom node neighbors  ---
!
              DO 2512 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                DO 2510 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NB)

                  KLU(NMD,M+MA) = NC

 2510           CONTINUE
                MA = MA + ISVC
 2512         CONTINUE
!
!---          South node neighbors  ---
!
              DO 2522 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                DO 2520 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NS)

                  KLU(NMD,M+MA) = NC

 2520           CONTINUE
                MA = MA + ISVC
 2522         CONTINUE
!
!---          West node neighbors  ---
!
              DO 2532 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                DO 2530 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NW)

                  KLU(NMD,M+MA) = NC

 2530           CONTINUE
                MA = MA + ISVC
 2532         CONTINUE
!
!---          East node neighbors  ---
!
              DO 2542 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                DO 2540 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NE)

                  KLU(NMD,M+MA) = NC

 2540           CONTINUE
                MA = MA + ISVC
 2542         CONTINUE
!
!---          North node neighbors  ---
!
              DO 2552 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                DO 2550 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NN)

                  KLU(NMD,M+MA) = NC

 2550           CONTINUE
                MA = MA + ISVC
 2552         CONTINUE
!
!---          Top node neighbors  ---
!
              DO 2562 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                DO 2560 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NT)

                  KLU(NMD,M+MA) = NC

 2560           CONTINUE
                MA = MA + ISVC
 2562         CONTINUE



              NLU(NMD+1) = NC+1

 2660       CONTINUE
!
!---        Solute transport equations  ---
!
            MA = 1
!
!---        Node  ---
!
            NCC = NCC+1




            MLUC(NCC) = NP

            KLUC(NP,MA) = NCC

            MA = MA + 1
!
!---        Bottom node neighbors  ---
!
            DO 2702 MX = 1,4
              NB = ICM(MX,1,N)
              IF( NB.EQ.0 ) EXIT
              NB = IXP(NB)
              NCC = NCC+1




              MLUC(NCC) = NB

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2702       CONTINUE
!
!---        South node neighbors  ---
!
            DO 2712 MX = 1,4
              NS = ICM(MX,2,N)
              IF( NS.EQ.0 ) EXIT
              NS = IXP(NS)
              NCC = NCC+1




              MLUC(NCC) = NS

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2712       CONTINUE
!
!---        West node neighbors  ---
!
            DO 2722 MX = 1,4
              NW = ICM(MX,3,N)
              IF( NW.EQ.0 ) EXIT
              NW = IXP(NW)
              NCC = NCC+1




              MLUC(NCC) = NW

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2722       CONTINUE
!
!---        East node neighbors  ---
!
            DO 2732 MX = 1,4
              NE = ICM(MX,4,N)
              IF( NE.EQ.0 ) EXIT
              NE = IXP(NE)
              NCC = NCC+1




              MLUC(NCC) = NE

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2732       CONTINUE
!
!---        North node neighbors  ---
!
            DO 2742 MX = 1,4
              NN = ICM(MX,5,N)
              IF( NN.EQ.0 ) EXIT
              NN = IXP(NN)
              NCC = NCC+1




              MLUC(NCC) = NN

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2742       CONTINUE
!
!---        Top node neighbors  ---
!
            DO 2752 MX = 1,4
              NT = ICM(MX,6,N)
              IF( NT.EQ.0 ) EXIT
              NT = IXP(NT)
              NCC = NCC+1




              MLUC(NCC) = NT

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2752       CONTINUE




            NLUC(NP+1) = NCC+1


 2800     CONTINUE
          MKC = NC
          MKT = NCC
        ENDIF
      ENDIF
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCBP_NCW group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCBP_NCW_BR
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
!     Configure the Jacobian matrix pointer arrays for coupled wells
!     with block refinement.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 April 2016.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PTZR
      USE JACOB
      USE GRID
      USE COUP_WELL
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
      SUB_LOG(ISUB_LOG) = '/JCBP_NCW_BR'
      IF( INDEX(SVN_ID(95)(1:1),'$').EQ.0 ) SVN_ID(95) =
     & '$Id: jcbp.F 1080 2017-03-14 16:22:02Z d3c002 $'
      ILIMIT = 2**30
!
!---  Coupled equations half band width using I,J,K ordering  ---
!
      NC = 0
      DO 10 K = 1,KFLD
      DO 10 J = 1,JFLD
      DO 10 I = 1,IFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 10
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          IXP(N) = -IRX*JRX*KRX
          DO 2 KX = 1,KRX
          DO 2 JX = 1,JRX
          DO 2 IX = 1,IRX
            N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
            NC = NC + 1
            IXP(N) = NC
    2     CONTINUE
        ELSE
          NC = NC + 1
          IXP(N) = NC
        ENDIF
   10 CONTINUE
      M_IJK = ILIMIT
      NC = 0
      MHBC_IJK = 0
      DO 20 K = 1,KFLD
      DO 20 J = 1,JFLD
      DO 20 I = 1,IFLD
        N = ND(I,J,K)
        NMD = ABS(IXP(N))
        IF( IXP(N).EQ.0 ) GOTO 20
!
!---     Loop over block refined nodes  ---
!
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 14 KX = 1,KRX
          DO 14 JX = 1,JRX
          DO 14 IX = 1,IRX
            N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
            NMD = ABS(IXP(N))
            DO 12 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
   12       CONTINUE
   14     CONTINUE
        ELSE
          DO 16 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
   16     CONTINUE
        ENDIF
   20 CONTINUE
!
!---  Determine matrix half-band widths, using I,J,K ordering  ---
!
      DO 40 K = 1,KFLD
      DO 40 J = 1,JFLD
      DO 40 I = 1,IFLD
        N = ND(I,J,K)
        NMD = ABS(IXP(N))
        IF( IXP(N).EQ.0 ) GOTO 40
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 32 KX = 1,KRX
        DO 32 JX = 1,JRX
        DO 32 IX = 1,IRX
          N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
          NP = ABS(IXP(N))
!
!---      Node  ---
!
          MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
              DO 21 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = ABS(IXP(NB))
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NB)),
     &            ABS(IM(ISVC,NP)-IM(1,NB)))
   21         CONTINUE
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
              DO 22 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = ABS(IXP(NS))
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NS)),
     &            ABS(IM(ISVC,NP)-IM(1,NS)))
   22         CONTINUE
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
              DO 23 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = ABS(IXP(NW))
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NW)),
     &            ABS(IM(ISVC,NP)-IM(1,NW)))
   23         CONTINUE
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
              DO 24 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = ABS(IXP(NE))
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NE)),
     &            ABS(IM(ISVC,NP)-IM(1,NE)))
   24         CONTINUE
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
              DO 25 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = ABS(IXP(NN))
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NN)),
     &            ABS(IM(ISVC,NP)-IM(1,NN)))
   25         CONTINUE
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
              DO 26 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = ABS(IXP(NT))
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NT)),
     &            ABS(IM(ISVC,NP)-IM(1,NT)))
   26         CONTINUE
          ENDIF
   32   CONTINUE
   40 CONTINUE
      IF( MHBC_IJK.LT.M_IJK ) THEN
        M_IJK = MHBC_IJK
      ENDIF
!
!---  Coupled equations half band width using J,K,I ordering  ---
!
      NC = 0
      DO 70 I = 1,IFLD
      DO 70 K = 1,KFLD
      DO 70 J = 1,JFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 70
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          IXP(N) = -IRX*JRX*KRX
          DO 62 IX = 1,IRX
          DO 62 KX = 1,KRX
          DO 62 JX = 1,JRX
            N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
            NC = NC + 1
            IXP(N) = NC
   62     CONTINUE
        ELSE
          NC = NC + 1
          IXP(N) = NC
        ENDIF
   70 CONTINUE
      M_JKI = ILIMIT
      NC = 0
      MHBC_JKI = 0
      DO 80 I = 1,IFLD
      DO 80 K = 1,KFLD
      DO 80 J = 1,JFLD
        N = ND(I,J,K)
        NMD = ABS(IXP(N))
        IF( IXP(N).EQ.0 ) GOTO 80
!
!---    Loop over block refined nodes  ---
!
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 74 IX = 1,IRX
          DO 74 KX = 1,KRX
          DO 74 JX = 1,JRX
            N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
            NMD = ABS(IXP(N))
            DO 72 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
   72       CONTINUE
   74     CONTINUE
        ELSE
          DO 76 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
   76     CONTINUE
        ENDIF
   80 CONTINUE
!
!---  Determine matrix half-band widths, using J,K,I ordering  ---
!
      DO 100 I = 1,IFLD
      DO 100 K = 1,KFLD
      DO 100 J = 1,JFLD
        N = ND(I,J,K)
        NP = ABS(IXP(N))
        IF( IXP(N).EQ.0 ) GOTO 100
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 92 IX = 1,IRX
        DO 92 KX = 1,KRX
        DO 92 JX = 1,JRX
          N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
          NP = ABS(IXP(N))
!
!---      Node  ---
!
          MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
              DO 81 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NB)),
     &            ABS(IM(ISVC,NP)-IM(1,NB)))
   81         CONTINUE
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
              DO 82 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NS)),
     &            ABS(IM(ISVC,NP)-IM(1,NS)))
   82         CONTINUE
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
              DO 83 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NW)),
     &            ABS(IM(ISVC,NP)-IM(1,NW)))
   83         CONTINUE
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
              DO 84 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NE)),
     &            ABS(IM(ISVC,NP)-IM(1,NE)))
   84         CONTINUE
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
              DO 85 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NN)),
     &            ABS(IM(ISVC,NP)-IM(1,NN)))
   85         CONTINUE
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
              DO 86 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NT)),
     &            ABS(IM(ISVC,NP)-IM(1,NT)))
   86         CONTINUE
          ENDIF
   92   CONTINUE
  100 CONTINUE
      IF( MHBC_JKI.LT.M_JKI ) THEN
        M_JKI = MHBC_JKI
      ENDIF
!
!---  Coupled equations half band width using K,I,J ordering  ---
!
      NC = 0
      DO 130 J = 1,JFLD
      DO 130 I = 1,IFLD
      DO 130 K = 1,KFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 130
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          IXP(N) = -IRX*JRX*KRX
          DO 122 JX = 1,JRX
          DO 122 IX = 1,IRX
          DO 122 KX = 1,KRX
            N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
            NC = NC + 1
            IXP(N) = NC
  122     CONTINUE
        ELSE
          NC = NC + 1
          IXP(N) = NC
        ENDIF
  130 CONTINUE
      M_KIJ = ILIMIT
      NC = 0
      MHBC_KIJ = 0
      DO 140 J = 1,JFLD
      DO 140 I = 1,IFLD
      DO 140 K = 1,KFLD
        N = ND(I,J,K)
        NMD = ABS(IXP(N))
        IF( IXP(N).EQ.0 ) GOTO 140
!
!---     Loop over block refined nodes  ---
!
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 134 JX = 1,JRX
          DO 134 IX = 1,IRX
          DO 134 KX = 1,KRX
            N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
            NMD = ABS(IXP(N))
            DO 132 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  132       CONTINUE
  134     CONTINUE
        ELSE
          DO 136 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
  136     CONTINUE
        ENDIF
  140 CONTINUE
!
!---  Determine matrix half-band widths, using K,I,J ordering  ---
!
      DO 160 J = 1,JFLD
      DO 160 I = 1,IFLD
      DO 160 K = 1,KFLD
        N = ND(I,J,K)
        NP = ABS(IXP(N))
        IF( IXP(N).EQ.0 ) GOTO 160
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 152 IX = 1,IRX
        DO 152 KX = 1,KRX
        DO 152 JX = 1,JRX
          N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
          NP = ABS(IXP(N))
!
!---      Node  ---
!
          MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
              DO 141 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = ABS(IXP(NB))
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NB)),
     &            ABS(IM(ISVC,NP)-IM(1,NB)))
  141         CONTINUE
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
              DO 142 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = ABS(IXP(NS))
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NS)),
     &            ABS(IM(ISVC,NP)-IM(1,NS)))
  142         CONTINUE
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
              DO 143 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = ABS(IXP(NW))
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NW)),
     &            ABS(IM(ISVC,NP)-IM(1,NW)))
  143         CONTINUE
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
              DO 144 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = ABS(IXP(NE))
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NE)),
     &            ABS(IM(ISVC,NP)-IM(1,NE)))
  144         CONTINUE
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
              DO 145 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = ABS(IXP(NN))
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NN)),
     &            ABS(IM(ISVC,NP)-IM(1,NN)))
  145         CONTINUE
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
              DO 146 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = ABS(IXP(NT))
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NT)),
     &            ABS(IM(ISVC,NP)-IM(1,NT)))
  146         CONTINUE
          ENDIF
  152   CONTINUE
  160 CONTINUE
      IF( MHBC_KIJ.LT.M_KIJ ) THEN
        M_KIJ = MHBC_KIJ
      ENDIF
!
!---  Find the maximum half band width   ---
!
      MHBC_IJK = 0
      MHBC_JKI = 0
      MHBC_KIJ = 0
      IF( M_IJK.GT.MHBC_IJK ) MHBC_IJK = M_IJK
      IF( M_JKI.GT.MHBC_JKI ) MHBC_JKI = M_JKI
      IF( M_KIJ.GT.MHBC_KIJ ) MHBC_KIJ = M_KIJ








!
!---  I,J,K ordering yields the lowest half band width or PETSc 
!     solver   ---
!
      IF( MHBC_IJK.LE.MHBC_JKI .AND. MHBC_IJK.LE.MHBC_KIJ ) THEN
        NJD = LBD*(3*MHBC_IJK+1) + LSP
        IF( NJD.GT.LJD ) THEN
          INDX = 5
          CHMSG = 'Number of Banded Matrix Rows ' //
     &      '> Parameter LJD'
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Equation ordering using I,J,K ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 210 K = 1,KFLD
        DO 210 J = 1,JFLD
        DO 210 I = 1,IFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 210
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            IXP(N) = -IRX*JRX*KRX
            DO 202 KX = 1,KRX
            DO 202 JX = 1,JRX
            DO 202 IX = 1,IRX
              N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
              NC = NC + 1
              IXP(N) = NC
  202       CONTINUE
          ELSE
            NC = NC + 1
            IXP(N) = NC
          ENDIF
  210   CONTINUE
!
!---    Initialize counter  ---
!
        NC = 0
        MHBC = 0
        MHBT = 0
!
!---    Coupled equations half band width using I,J,K ordering  ---
!
        DO 230 K = 1,KFLD
        DO 230 J = 1,JFLD
        DO 230 I = 1,IFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
          IF( IXP(N).EQ.0 ) GOTO 230
!
!---      Loop over block refined nodes  ---
!
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 216 KX = 1,KRX
            DO 216 JX = 1,JRX
            DO 216 IX = 1,IRX
              N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
              NMD = ABS(IXP(N))
              DO 212 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  212         CONTINUE
  216       CONTINUE
          ELSE
            DO 218 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  218       CONTINUE
          ENDIF
  230   CONTINUE
!
!---    Determine matrix half-band widths, using I,J,K ordering  ---
!
        DO 260 K = 1,KFLD
        DO 260 J = 1,JFLD
        DO 260 I = 1,IFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
          IF( IXP(N).EQ.0 ) GOTO 260
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 252 KX = 1,KRX
          DO 252 JX = 1,JRX
          DO 252 IX = 1,IRX
            N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
            NP = ABS(IXP(N))
!
!---        Node  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
                DO 241 MX = 1,4
                  NB = ICM(MX,1,N)
                  IF( NB.EQ.0 ) EXIT
                  NB = ABS(IXP(NB))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NB)) )
  241           CONTINUE
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
                DO 242 MX = 1,4
                  NS = ICM(MX,2,N)
                  IF( NS.EQ.0 ) EXIT
                  NS = ABS(IXP(NS))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NS)) )
  242           CONTINUE
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
                DO 243 MX = 1,4
                  NW = ICM(MX,3,N)
                  IF( NW.EQ.0 ) EXIT
                  NW = ABS(IXP(NW))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NW)) )
  243           CONTINUE
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.IFLD ) THEN
                DO 244 MX = 1,4
                  NE = ICM(MX,4,N)
                  IF( NE.EQ.0 ) EXIT
                  NE = ABS(IXP(NE))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NE)) )
  244           CONTINUE
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.JFLD ) THEN
                DO 245 MX = 1,4
                  NN = ICM(MX,5,N)
                  IF( NN.EQ.0 ) EXIT
                  NN = ABS(IXP(NN))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &               ABS(IM(ISVC,NP)-IM(1,NN)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NN)) )
  245           CONTINUE
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.KFLD ) THEN
                DO 246 MX = 1,4
                  NT = ICM(MX,6,N)
                  IF( NT.EQ.0 ) EXIT
                  NT = ABS(IXP(NT))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NT)) )
  246           CONTINUE
            ENDIF
  252     CONTINUE
  260   CONTINUE
!
!---  J,K,I ordering yields the lowest half band width   ---
!
      ELSEIF( MHBC_JKI.LE.MHBC_KIJ ) THEN
        NJD = LBD*(3*MHBC_JKI+1) + LSP
        IF( NJD.GT.LJD ) THEN
          INDX = 5
          CHMSG = 'Number of Banded Matrix Rows ' //
     &      '> Parameter LJD'
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Equation ordering using J,K,I ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 310 I = 1,IFLD
        DO 310 K = 1,KFLD
        DO 310 J = 1,JFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 310
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            IXP(N) = -IRX*JRX*KRX
            DO 302 IX = 1,IRX
            DO 302 KX = 1,KRX
            DO 302 JX = 1,JRX
              N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
              NC = NC + 1
              IXP(N) = NC
  302       CONTINUE
          ELSE
            NC = NC + 1
            IXP(N) = NC
          ENDIF
  310   CONTINUE
!
!---    Initialize counter  ---
!
        NC = 0
        MHBC = 0
        MHBT = 0
!
!---    Coupled equations half band width using J,K,I ordering  ---
!
        DO 330 I = 1,IFLD
        DO 330 K = 1,KFLD
        DO 330 J = 1,JFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
          IF( IXP(N).EQ.0 ) GOTO 330
!
!---      Loop over block refined nodes  ---
!
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 316 IX = 1,IRX
            DO 316 KX = 1,KRX
            DO 316 JX = 1,JRX
              N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
              NMD = ABS(IXP(N))
              DO 312 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  312         CONTINUE
  316       CONTINUE
          ELSE
            DO 318 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  318       CONTINUE
          ENDIF
  330   CONTINUE
!
!---    Determine matrix half-band widths using J,K,I ordering  ---
!
        DO 360 I = 1,IFLD
        DO 360 K = 1,KFLD
        DO 360 J = 1,JFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
          IF( IXP(N).EQ.0 ) GOTO 360
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 352 KX = 1,KRX
          DO 352 JX = 1,JRX
          DO 352 IX = 1,IRX
            N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
            NP = ABS(IXP(N))
!
!---        Node  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
                DO 341 MX = 1,4
                  NB = ICM(MX,1,N)
                  IF( NB.EQ.0 ) EXIT
                  NB = ABS(IXP(NB))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NB)) )
  341           CONTINUE
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
                DO 342 MX = 1,4
                  NS = ICM(MX,2,N)
                  IF( NS.EQ.0 ) EXIT
                  NS = ABS(IXP(NS))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NS)) )
  342           CONTINUE
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
                DO 343 MX = 1,4
                  NW = ICM(MX,3,N)
                  IF( NW.EQ.0 ) EXIT
                  NW = ABS(IXP(NW))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NW)) )
  343           CONTINUE
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.IFLD ) THEN
                DO 344 MX = 1,4
                  NE = ICM(MX,4,N)
                  IF( NE.EQ.0 ) EXIT
                  NE = ABS(IXP(NE))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NE)) )
  344           CONTINUE
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.JFLD ) THEN
                DO 345 MX = 1,4
                  NN = ICM(MX,5,N)
                  IF( NN.EQ.0 ) EXIT
                  NN = ABS(IXP(NN))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &               ABS(IM(ISVC,NP)-IM(1,NN)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NN)) )
  345           CONTINUE
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.KFLD ) THEN
                DO 346 MX = 1,4
                  NT = ICM(MX,6,N)
                  IF( NT.EQ.0 ) EXIT
                  NT = ABS(IXP(NT))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NT)) )
  346           CONTINUE
            ENDIF
  352     CONTINUE
  360   CONTINUE
!
!---  K,I,J ordering yields the lowest half band width   ---
!
      ELSE
        NJD = LBD*(3*MHBC_KIJ+1) + LSP
        IF( NJD.GT.LJD ) THEN
          INDX = 5
          CHMSG = 'Number of Banded Matrix Rows ' //
     &      '> Parameter LJD'
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Equation ordering using K,I,J ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 410 J = 1,JFLD
        DO 410 I = 1,IFLD
        DO 410 K = 1,KFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 410
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            IXP(N) = -IRX*JRX*KRX
            DO 402 JX = 1,JRX
            DO 402 IX = 1,IRX
            DO 402 KX = 1,KRX
              N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
              NC = NC + 1
              IXP(N) = NC
  402       CONTINUE
          ELSE
            NC = NC + 1
            IXP(N) = NC
          ENDIF
  410   CONTINUE
!
!---    Initialize counter  ---
!
        NC = 0
        MHBC = 0
        MHBT = 0
!
!---    Coupled equations half band width using K,I,J ordering  ---
!
        DO 430 J = 1,JFLD
        DO 430 I = 1,IFLD
        DO 430 K = 1,KFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
          IF( IXP(N).EQ.0 ) GOTO 430
!
!---      Loop over block refined nodes  ---
!
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 416 IX = 1,IRX
            DO 416 KX = 1,KRX
            DO 416 JX = 1,JRX
              N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
              NMD = ABS(IXP(N))
              DO 412 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  412         CONTINUE
  416       CONTINUE
          ELSE
            DO 418 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  418       CONTINUE
          ENDIF
  430   CONTINUE
!
!---    Determine matrix half-band widths using K,I,J ordering  ---
!
        DO 460 J = 1,JFLD
        DO 460 I = 1,IFLD
        DO 460 K = 1,KFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
          IF( IXP(N).EQ.0 ) GOTO 460
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 452 KX = 1,KRX
          DO 452 JX = 1,JRX
          DO 452 IX = 1,IRX
            N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
            NP = ABS(IXP(N))
!
!---        Node  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
                DO 441 MX = 1,4
                  NB = ICM(MX,1,N)
                  IF( NB.EQ.0 ) EXIT
                  NB = ABS(IXP(NB))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NB)) )
  441           CONTINUE
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
                DO 442 MX = 1,4
                  NS = ICM(MX,2,N)
                  IF( NS.EQ.0 ) EXIT
                  NS = ABS(IXP(NS))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NS)) )
  442           CONTINUE
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
                DO 443 MX = 1,4
                  NW = ICM(MX,3,N)
                  IF( NW.EQ.0 ) EXIT
                  NW = ABS(IXP(NW))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NW)) )
  443           CONTINUE
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.IFLD ) THEN
                DO 444 MX = 1,4
                  NE = ICM(MX,4,N)
                  IF( NE.EQ.0 ) EXIT
                  NE = ABS(IXP(NE))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NE)) )
  444           CONTINUE
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.JFLD ) THEN
                DO 445 MX = 1,4
                  NN = ICM(MX,5,N)
                  IF( NN.EQ.0 ) EXIT
                  NN = ABS(IXP(NN))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &               ABS(IM(ISVC,NP)-IM(1,NN)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NN)) )
  445           CONTINUE
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.KFLD ) THEN
                DO 446 MX = 1,4
                  NT = ICM(MX,6,N)
                  IF( NT.EQ.0 ) EXIT
                  NT = ABS(IXP(NT))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
                  MHBT = MAX( MHBT,ABS(IXP(NP)-IXP(NT)) )
  446           CONTINUE
            ENDIF
  452     CONTINUE
  460   CONTINUE
      ENDIF
      MLC = MHBC
      MLT = ISVT*MHBT + ISVT - 1
      MUC = MHBC
      MUT = ISVT*MHBT + ISVT - 1
      MDC = MLC + MUC + 1
      MDT = MLT + MUT + 1
!
!---  Check Parameter LJD  ---
!
      NJD = LBD*(3*MHBC+1) + LSP
      IF( NJD.GT.LJD ) THEN
        INDX = 5
        CHMSG = 'Number of Banded Matrix Rows ' //
     &    '> Parameter LJD'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  SPLIB .or. Lis .or. PETSc Solver  ---
!
      IF( ILES.EQ.3 .OR. ILES.EQ.4 .OR. ILES.EQ.5 ) THEN
!
!---    X-Y Plane yields the lowest band width.
!       Load Jacobian matrix in the increment order I,J,K  ---
!
        IF( MHBC_IJK.LE.MHBC_JKI .AND. MHBC_IJK.LE.MHBC_KIJ ) THEN
          NC = 0
          NCC = 0




          NLU(1) = 1
          NLUC(1) = 1

          DO 800 K = 1,KFLD
          DO 800 J = 1,JFLD
          DO 800 I = 1,IFLD
            N = ND(I,J,K)
            IF( IXP(N).EQ.0 ) CYCLE
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 790 KX = 1,KRX
            DO 790 JX = 1,JRX
            DO 790 IX = 1,IRX
              N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
              NP = IXP(N)
              DO 660 L = 1,ISVC
                NMD = IM(L,NP)
                MA = 0
!
!---            Node  ---
!
                DO 500 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NP)

                  KLU(NMD,M+MA) = NC

  500           CONTINUE
                MA = MA + ISVC
!
!---            Bottom node neighbors  ---
!
                DO 512 MX = 1,4
                  NB = ICM(MX,1,N)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  DO 510 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NB)

                    KLU(NMD,M+MA) = NC

  510             CONTINUE
                  MA = MA + ISVC
  512           CONTINUE
!
!---            South node neighbors  ---
!
                DO 522 MX = 1,4
                  NS = ICM(MX,2,N)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  DO 520 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NS)

                    KLU(NMD,M+MA) = NC

  520             CONTINUE
                  MA = MA + ISVC
  522           CONTINUE
!
!---            West node neighbors  ---
!
                DO 532 MX = 1,4
                  NW = ICM(MX,3,N)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  DO 530 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NW)

                    KLU(NMD,M+MA) = NC

  530             CONTINUE
                  MA = MA + ISVC
  532           CONTINUE
!
!---            East node neighbors  ---
!
                DO 542 MX = 1,4
                  NE = ICM(MX,4,N)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  DO 540 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NE)

                    KLU(NMD,M+MA) = NC

  540             CONTINUE
                  MA = MA + ISVC
  542           CONTINUE
!
!---            North node neighbors  ---
!
                DO 552 MX = 1,4
                  NN = ICM(MX,5,N)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  DO 550 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NN)

                    KLU(NMD,M+MA) = NC

  550             CONTINUE
                  MA = MA + ISVC
  552           CONTINUE
!
!---            Top node neighbors  ---
!
                DO 562 MX = 1,4
                  NT = ICM(MX,6,N)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  DO 560 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NT)

                    KLU(NMD,M+MA) = NC

  560             CONTINUE
                  MA = MA + ISVC
  562           CONTINUE



                NLU(NMD+1) = NC+1

  660         CONTINUE
!
!---          Solute transport equations  ---
!
              MA = 1
!
!---          Node  ---
!
              NCC = NCC+1




              MLUC(NCC) = NP

              KLUC(NP,MA) = NCC

              MA = MA + 1
!
!---          Bottom node neighbors  ---
!
              DO 702 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                NCC = NCC+1




                MLUC(NCC) = NB

                KLUC(NP,MA) = NCC

                MA = MA + 1
  702         CONTINUE
!
!---          South node neighbors  ---
!
              DO 712 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                NCC = NCC+1




                MLUC(NCC) = NS

                KLUC(NP,MA) = NCC

                MA = MA + 1
  712         CONTINUE
!
!---          West node neighbors  ---
!
              DO 722 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                NCC = NCC+1




                MLUC(NCC) = NW

                KLUC(NP,MA) = NCC

                MA = MA + 1
  722         CONTINUE
!
!---          East node neighbors  ---
!
              DO 732 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                NCC = NCC+1




                MLUC(NCC) = NE

                KLUC(NP,MA) = NCC

                MA = MA + 1
  732         CONTINUE
!
!---          North node neighbors  ---
!
              DO 742 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                NCC = NCC+1




                MLUC(NCC) = NN

                KLUC(NP,MA) = NCC

                MA = MA + 1
  742         CONTINUE
!
!---          Top node neighbors  ---
!
              DO 752 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                NCC = NCC+1




                MLUC(NCC) = NT

                KLUC(NP,MA) = NCC

                MA = MA + 1
  752         CONTINUE




              NLUC(NP+1) = NCC+1


  790       CONTINUE
  800     CONTINUE
          MKC = NC
          MKT = NCC
!
!---    Y-Z Plane yields the lowest band width.
!       Load Jacobian matrix in the increment order J,K,I  ---
!
        ELSEIF( MHBC_JKI.LE.MHBC_KIJ ) THEN
          NC = 0
          NCC = 0




          NLU(1) = 1
          NLUC(1) = 1

          DO 1800 I = 1,IFLD
          DO 1800 K = 1,KFLD
          DO 1800 J = 1,JFLD
            N = ND(I,J,K)
            IF( IXP(N).EQ.0 ) CYCLE
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 1790 IX = 1,IRX
            DO 1790 KX = 1,KRX
            DO 1790 JX = 1,JRX
              N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
              NP = IXP(N)
              DO 1660 L = 1,ISVC
                NMD = IM(L,NP)
                MA = 0
!
!---            Node  ---
!
                DO 1500 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NP)

                  KLU(NMD,M+MA) = NC

 1500           CONTINUE
                MA = MA + ISVC
!
!---            Bottom node neighbors  ---
!
                DO 1512 MX = 1,4
                  NB = ICM(MX,1,N)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  DO 1510 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NB)

                    KLU(NMD,M+MA) = NC

 1510             CONTINUE
                  MA = MA + ISVC
 1512           CONTINUE
!
!---            South node neighbors  ---
!
                DO 1522 MX = 1,4
                  NS = ICM(MX,2,N)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  DO 1520 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NS)

                    KLU(NMD,M+MA) = NC

 1520             CONTINUE
                  MA = MA + ISVC
 1522           CONTINUE
!
!---            West node neighbors  ---
!
                DO 1532 MX = 1,4
                  NW = ICM(MX,3,N)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  DO 1530 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NW)

                    KLU(NMD,M+MA) = NC

 1530             CONTINUE
                  MA = MA + ISVC
 1532           CONTINUE
!
!---            East node neighbors  ---
!
                DO 1542 MX = 1,4
                  NE = ICM(MX,4,N)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  DO 1540 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NE)

                    KLU(NMD,M+MA) = NC

 1540             CONTINUE
                  MA = MA + ISVC
 1542           CONTINUE
!
!---            North node neighbors  ---
!
                DO 1552 MX = 1,4
                  NN = ICM(MX,5,N)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  DO 1550 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NN)

                    KLU(NMD,M+MA) = NC

 1550             CONTINUE
                  MA = MA + ISVC
 1552           CONTINUE
!
!---            Top node neighbors  ---
!
                DO 1562 MX = 1,4
                  NT = ICM(MX,6,N)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  DO 1560 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NT)

                    KLU(NMD,M+MA) = NC

 1560             CONTINUE
                  MA = MA + ISVC
 1562           CONTINUE



                NLU(NMD+1) = NC+1

 1660         CONTINUE
!
!---          Solute transport equations  ---
!
              MA = 1
!
!---          Node  ---
!
              NCC = NCC+1




              MLUC(NCC) = NP

              KLUC(NP,MA) = NCC

              MA = MA + 1
!
!---          Bottom node neighbors  ---
!
              DO 1702 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                NCC = NCC+1




                MLUC(NCC) = NB

                KLUC(NP,MA) = NCC

                MA = MA + 1
 1702         CONTINUE
!
!---          South node neighbors  ---
!
              DO 1712 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                NCC = NCC+1




                MLUC(NCC) = NS

                KLUC(NP,MA) = NCC

                MA = MA + 1
 1712         CONTINUE
!
!---          West node neighbors  ---
!
              DO 1722 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                NCC = NCC+1




                MLUC(NCC) = NW

                KLUC(NP,MA) = NCC

                MA = MA + 1
 1722         CONTINUE
!
!---          East node neighbors  ---
!
              DO 1732 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                NCC = NCC+1




                MLUC(NCC) = NE

                KLUC(NP,MA) = NCC

                MA = MA + 1
 1732         CONTINUE
!
!---          North node neighbors  ---
!
              DO 1742 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                NCC = NCC+1




                MLUC(NCC) = NN

                KLUC(NP,MA) = NCC

                MA = MA + 1
 1742         CONTINUE
!
!---          Top node neighbors  ---
!
              DO 1752 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                NCC = NCC+1




                MLUC(NCC) = NT

                KLUC(NP,MA) = NCC

                MA = MA + 1
 1752         CONTINUE




              NLUC(NP+1) = NCC+1


 1790       CONTINUE
 1800     CONTINUE
          MKC = NC
          MKT = NCC
!
!---    Z-X Plane yields the lowest band width.
!       Load Jacobian matrix in the increment order K,I,J  ---
!
        ELSE
          NC = 0
          NCC = 0




          NLU(1) = 1
          NLUC(1) = 1

          DO 2800 J = 1,JFLD
          DO 2800 I = 1,IFLD
          DO 2800 K = 1,KFLD
            N = ND(I,J,K)
            IF( IXP(N).EQ.0 ) CYCLE
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 2790 JX = 1,JRX
            DO 2790 IX = 1,IRX
            DO 2790 KX = 1,KRX
              N = IBR(4,ND(I,J,K)) + NDBR(IX,IRX,JX,JRX,KX)
              NP = IXP(N)
              DO 2660 L = 1,ISVC
                NMD = IM(L,NP)
                MA = 0
!
!---            Node  ---
!
                DO 2500 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NP)

                  KLU(NMD,M+MA) = NC

 2500           CONTINUE
                MA = MA + ISVC
!
!---            Bottom node neighbors  ---
!
                DO 2512 MX = 1,4
                  NB = ICM(MX,1,N)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  DO 2510 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NB)

                    KLU(NMD,M+MA) = NC

 2510             CONTINUE
                  MA = MA + ISVC
 2512           CONTINUE
!
!---            South node neighbors  ---
!
                DO 2522 MX = 1,4
                  NS = ICM(MX,2,N)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  DO 2520 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NS)

                    KLU(NMD,M+MA) = NC

 2520             CONTINUE
                  MA = MA + ISVC
 2522           CONTINUE
!
!---            West node neighbors  ---
!
                DO 2532 MX = 1,4
                  NW = ICM(MX,3,N)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  DO 2530 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NW)

                    KLU(NMD,M+MA) = NC

 2530             CONTINUE
                  MA = MA + ISVC
 2532           CONTINUE
!
!---            East node neighbors  ---
!
                DO 2542 MX = 1,4
                  NE = ICM(MX,4,N)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  DO 2540 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NE)

                    KLU(NMD,M+MA) = NC

 2540             CONTINUE
                  MA = MA + ISVC
 2542           CONTINUE
!
!---            North node neighbors  ---
!
                DO 2552 MX = 1,4
                  NN = ICM(MX,5,N)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  DO 2550 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NN)

                    KLU(NMD,M+MA) = NC

 2550             CONTINUE
                  MA = MA + ISVC
 2552           CONTINUE
!
!---            Top node neighbors  ---
!
                DO 2562 MX = 1,4
                  NT = ICM(MX,6,N)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  DO 2560 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NT)

                    KLU(NMD,M+MA) = NC

 2560             CONTINUE
                  MA = MA + ISVC
 2562           CONTINUE



                NLU(NMD+1) = NC+1

 2660         CONTINUE
!
!---          Solute transport equations  ---
!
              MA = 1
!
!---          Node  ---
!
              NCC = NCC+1




              MLUC(NCC) = NP

              KLUC(NP,MA) = NCC

              MA = MA + 1
!
!---          Bottom node neighbors  ---
!
              DO 2702 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                NCC = NCC+1




                MLUC(NCC) = NB

                KLUC(NP,MA) = NCC

                MA = MA + 1
 2702         CONTINUE
!
!---          South node neighbors  ---
!
              DO 2712 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                NCC = NCC+1




                MLUC(NCC) = NS

                KLUC(NP,MA) = NCC

                MA = MA + 1
 2712         CONTINUE
!
!---          West node neighbors  ---
!
              DO 2722 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                NCC = NCC+1




                MLUC(NCC) = NW

                KLUC(NP,MA) = NCC

                MA = MA + 1
 2722         CONTINUE
!
!---          East node neighbors  ---
!
              DO 2732 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                NCC = NCC+1




                MLUC(NCC) = NE

                KLUC(NP,MA) = NCC

                MA = MA + 1
 2732         CONTINUE
!
!---          North node neighbors  ---
!
              DO 2742 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                NCC = NCC+1




                MLUC(NCC) = NN

                KLUC(NP,MA) = NCC

                MA = MA + 1
 2742         CONTINUE
!
!---          Top node neighbors  ---
!
              DO 2752 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                NCC = NCC+1




                MLUC(NCC) = NT

                KLUC(NP,MA) = NCC

                MA = MA + 1
 2752         CONTINUE




              NLUC(NP+1) = NCC+1


 2790       CONTINUE
 2800     CONTINUE
          MKC = NC
          MKT = NCC
        ENDIF
      ENDIF
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCBP_NCW_BR group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCBP_NCW_DP
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
!     Configure the Jacobian matrix pointer arrays for dual-porosity
!     model for STOMP-EOR.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 November 2015.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PTZR
      USE JACOB
      USE GRID
      USE DUAL_POR
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
      SUB_LOG(ISUB_LOG) = '/JCBP_NCW_DP'
      IF( INDEX(SVN_ID(95)(1:1),'$').EQ.0 ) SVN_ID(95) =
     & '$Id: jcbp.F 1080 2017-03-14 16:22:02Z d3c002 $'
      ILIMIT = 2**30
!
!---  Coupled equations half band width using I,J,K ordering  ---
!
      NC = 0
      DO 5 K = 1,KFLD
      DO 5 J = 1,JFLD
      DO 5 I = 1,IFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 5
        NC = NC + 1
        IXP(N) = NC
    5 CONTINUE
      M_IJK = ILIMIT
      NC = 0
      MHBC_IJK = 0
      DO 20 K = 1,KFLD
      DO 20 J = 1,JFLD
      DO 20 I = 1,IFLD
        N = ND(I,J,K)
        NMD = ABS(IXP(N))
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 20
        DO 10 M = 1,ISVC
          NC = NC+1
          IM(M,NMD) = NC
   10   CONTINUE
        DO 11 M = 1,ISVC
          NC = NC+1
          IM_M(M,NMD) = NC
   11   CONTINUE
   20 CONTINUE
!
!---  Determine matrix half-band widths, using I,J,K ordering  ---
!
      DO 40 K = 1,KFLD
      DO 40 J = 1,JFLD
      DO 40 I = 1,IFLD
        N = ND(I,J,K)
        NP = ABS(IXP(N))
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 40
!
!---    Node  ---
!
        MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---    Fracture to matrix connections  ---
!
        MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM_M(ISVC,NP)),
     &    ABS(IM(ISVC,NP)-IM_M(1,NP)))
!
!---    Bottom node neighbors  ---
!
        IF( K.GT.1 ) THEN
          IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
            NB = ABS(IXP(N-IJFLD))
            MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NB)),
     &        ABS(IM(ISVC,NP)-IM(1,NB)))
          ENDIF
        ENDIF
!
!---    South node neighbors  ---
!
        IF( J.GT.1 ) THEN
          IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
            NS = ABS(IXP(N-IFLD))
            MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NS)),
     &        ABS(IM(ISVC,NP)-IM(1,NS)))
          ENDIF
        ENDIF
!
!---    West node neighbors  ---
!
        IF( I.GT.1 ) THEN
          IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
            NW = ABS(IXP(N-1))
            MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NW)),
     &        ABS(IM(ISVC,NP)-IM(1,NW)))
          ENDIF
        ENDIF
!
!---    East node neighbors  ---
!
        IF( I.LT.IFLD ) THEN
          IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
            NE = ABS(IXP(N+1))
            MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NE)),
     &        ABS(IM(ISVC,NP)-IM(1,NE)))
          ENDIF
        ENDIF
!
!---    North node neighbors  ---
!
        IF( J.LT.JFLD ) THEN
          IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
            NN = ABS(IXP(N+IFLD))
            MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NN)),
     &         ABS(IM(ISVC,NP)-IM(1,NN)))
          ENDIF
        ENDIF
!
!---    Top node neighbors  ---
!
        IF( K.LT.KFLD ) THEN
          IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
            NT = ABS(IXP(N+IJFLD))
            MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NT)),
     &        ABS(IM(ISVC,NP)-IM(1,NT)))
          ENDIF
        ENDIF
   40 CONTINUE
      IF( MHBC_IJK.LT.M_IJK ) THEN
        M_IJK = MHBC_IJK
      ENDIF
!
!---  Coupled equations half band width using J,K,I ordering  ---
!
      NC = 0
      DO 65 I = 1,IFLD
      DO 65 K = 1,KFLD
      DO 65 J = 1,JFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 65
        NC = NC + 1
        IXP(N) = NC
   65 CONTINUE
      M_JKI = ILIMIT
      NC = 0
      MHBC_JKI = 0
      DO 80 I = 1,IFLD
      DO 80 K = 1,KFLD
      DO 80 J = 1,JFLD
        N = ND(I,J,K)
        NMD = ABS(IXP(N))
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 80
        DO 70 M = 1,ISVC
          NC = NC+1
          IM(M,NMD) = NC
   70   CONTINUE
        DO 71 M = 1,ISVC
          NC = NC+1
          IM_M(M,NMD) = NC
   71   CONTINUE
   80 CONTINUE
!
!---  Determine matrix half-band widths, using J,K,I ordering  ---
!
      DO 100 I = 1,IFLD
      DO 100 K = 1,KFLD
      DO 100 J = 1,JFLD
        N = ND(I,J,K)
        NP = ABS(IXP(N))
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 100
!
!---    Node  ---
!
        MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---    Fracture to matrix connections  ---
!
        MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM_M(ISVC,NP)),
     &    ABS(IM(ISVC,NP)-IM_M(1,NP)))
!
!---    Bottom node neighbors  ---
!
        IF( K.GT.1 ) THEN
          IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
            NB = ABS(IXP(N-IJFLD))
            MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NB)),
     &        ABS(IM(ISVC,NP)-IM(1,NB)))
          ENDIF
        ENDIF
!
!---    South node neighbors  ---
!
        IF( J.GT.1 ) THEN
          IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
            NS = ABS(IXP(N-IFLD))
            MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NS)),
     &        ABS(IM(ISVC,NP)-IM(1,NS)))
          ENDIF
        ENDIF
!
!---    West node neighbors  ---
!
        IF( I.GT.1 ) THEN
          IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
            NW = ABS(IXP(N-1))
            MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NW)),
     &        ABS(IM(ISVC,NP)-IM(1,NW)))
          ENDIF
        ENDIF
!
!---    East node neighbors  ---
!
        IF( I.LT.IFLD ) THEN
          IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
            NE = ABS(IXP(N+1))
            MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NE)),
     &        ABS(IM(ISVC,NP)-IM(1,NE)))
          ENDIF
        ENDIF
!
!---    North node neighbors  ---
!
        IF( J.LT.JFLD ) THEN
          IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
            NN = ABS(IXP(N+IFLD))
            MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NN)),
     &         ABS(IM(ISVC,NP)-IM(1,NN)))
          ENDIF
        ENDIF
!
!---    Top node neighbors  ---
!
        IF( K.LT.KFLD ) THEN
          IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
            NT = ABS(IXP(N+IJFLD))
            MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NT)),
     &        ABS(IM(ISVC,NP)-IM(1,NT)))
          ENDIF
        ENDIF
  100 CONTINUE
      IF( MHBC_JKI.LT.M_JKI ) THEN
        M_JKI = MHBC_JKI
      ENDIF
!
!---  Coupled equations half band width using K,I,J ordering  ---
!
      NC = 0
      DO 125 J = 1,JFLD
      DO 125 I = 1,IFLD
      DO 125 K = 1,KFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 125
        NC = NC + 1
        IXP(N) = NC
  125 CONTINUE
      M_KIJ = ILIMIT
      NC = 0
      MHBC_KIJ = 0
      DO 140 J = 1,JFLD
      DO 140 I = 1,IFLD
      DO 140 K = 1,KFLD
        N = ND(I,J,K)
        NMD = ABS(IXP(N))
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 140
        DO 130 M = 1,ISVC
          NC = NC+1
          IM(M,NMD) = NC
  130   CONTINUE
        DO 131 M = 1,ISVC
          NC = NC+1
          IM_M(M,NMD) = NC
  131   CONTINUE
  140 CONTINUE
!
!---  Determine matrix half-band widths, using K,I,J ordering  ---
!
      DO 160 J = 1,JFLD
      DO 160 I = 1,IFLD
      DO 160 K = 1,KFLD
        N = ND(I,J,K)
        NP = ABS(IXP(N))
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 160
!
!---    Node  ---
!
        MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---    Fracture to matrix connections  ---
!
        MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM_M(ISVC,NP)),
     &    ABS(IM(ISVC,NP)-IM_M(1,NP)))
!
!---    Bottom node neighbors  ---
!
        IF( K.GT.1 ) THEN
          IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
            NB = ABS(IXP(N-IJFLD))
            MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NB)),
     &        ABS(IM(ISVC,NP)-IM(1,NB)))
          ENDIF
        ENDIF
!
!---    South node neighbors  ---
!
        IF( J.GT.1 ) THEN
          IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
            NS = ABS(IXP(N-IFLD))
            MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NS)),
     &        ABS(IM(ISVC,NP)-IM(1,NS)))
          ENDIF
        ENDIF
!
!---    West node neighbors  ---
!
        IF( I.GT.1 ) THEN
          IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
            NW = ABS(IXP(N-1))
            MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NW)),
     &        ABS(IM(ISVC,NP)-IM(1,NW)))
          ENDIF
        ENDIF
!
!---    East node neighbors  ---
!
        IF( I.LT.IFLD ) THEN
          IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
            NE = ABS(IXP(N+1))
            MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NE)),
     &        ABS(IM(ISVC,NP)-IM(1,NE)))
          ENDIF
        ENDIF
!
!---    North node neighbors  ---
!
        IF( J.LT.JFLD ) THEN
          IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
            NN = ABS(IXP(N+IFLD))
            MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NN)),
     &         ABS(IM(ISVC,NP)-IM(1,NN)))
          ENDIF
        ENDIF
!
!---    Top node neighbors  ---
!
        IF( K.LT.KFLD ) THEN
          IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
            NT = ABS(IXP(N+IJFLD))
            MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NT)),
     &        ABS(IM(ISVC,NP)-IM(1,NT)))
          ENDIF
        ENDIF
  160 CONTINUE
      IF( MHBC_KIJ.LT.M_KIJ ) THEN
        M_KIJ = MHBC_KIJ
      ENDIF
!
!---  Loop over wells searching for the maximum half band width   ---
!
      MHBC_IJK = 0
      MHBC_JKI = 0
      MHBC_KIJ = 0
      IF( M_IJK.GT.MHBC_IJK ) MHBC_IJK = M_IJK
      IF( M_JKI.GT.MHBC_JKI ) MHBC_JKI = M_JKI
      IF( M_KIJ.GT.MHBC_KIJ ) MHBC_KIJ = M_KIJ
!
!---  I,J,K ordering yields the lowest half band width   ---
!
      IF( MHBC_IJK.LE.MHBC_JKI .AND. MHBC_IJK.LE.MHBC_KIJ ) THEN
        NJD = LBD*(3*MHBC_IJK+1) + LSP
        IF( NJD.GT.LJD ) THEN
          INDX = 5
          CHMSG = 'Number of Banded Matrix Rows ' //
     &      '> Parameter LJD'
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Equation ordering using I,J,K ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 205 K = 1,KFLD
        DO 205 J = 1,JFLD
        DO 205 I = 1,IFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 205
          NC = NC + 1
          IXP(N) = NC
  205   CONTINUE
!
!---    Initialize counter  ---
!
        NC = 0
        MHBC = 0
        MHBT = 0
!
!---    Coupled equations half band width using I,J,K ordering  ---
!
        DO 230 K = 1,KFLD
        DO 230 J = 1,JFLD
        DO 230 I = 1,IFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 230
          DO 210 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
  210     CONTINUE
          DO 211 M = 1,ISVC
            NC = NC+1
            IM_M(M,NMD) = NC
  211     CONTINUE
  230   CONTINUE
!
!---    Determine matrix half-band widths, using I,J,K ordering  ---
!
        DO 260 K = 1,KFLD
        DO 260 J = 1,JFLD
        DO 260 I = 1,IFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 260
!
!---      Node  ---
!
          MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Fracture to matrix connections  ---
!
          MHBC = MAX(MHBC,ABS(IM(1,NP)-IM_M(ISVC,NP)),
     &      ABS(IM(ISVC,NP)-IM_M(1,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              NB = ABS(IXP(N-IJFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &          ABS(IM(ISVC,NP)-IM(1,NB)))
              MHBT = MAX( MHBT,ABS(NP-NB) )
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              NS = ABS(IXP(N-IFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &          ABS(IM(ISVC,NP)-IM(1,NS)))
              MHBT = MAX( MHBT,ABS(NP-NS) )
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              NW = ABS(IXP(N-1))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &          ABS(IM(ISVC,NP)-IM(1,NW)))
              MHBT = MAX( MHBT,ABS(NP-NW) )
            ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              NE = ABS(IXP(N+1))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &          ABS(IM(ISVC,NP)-IM(1,NE)))
              MHBT = MAX( MHBT,ABS(NP-NE) )
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
            IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              NN = ABS(IXP(N+IFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &           ABS(IM(ISVC,NP)-IM(1,NN)))
              MHBT = MAX( MHBT,ABS(NP-NN) )
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
            IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              NT = ABS(IXP(N+IJFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &          ABS(IM(ISVC,NP)-IM(1,NT)))
              MHBT = MAX( MHBT,ABS(NP-NT) )
            ENDIF
          ENDIF
  260   CONTINUE
!
!---  J,K,I ordering yields the lowest half band width   ---
!
      ELSEIF( MHBC_JKI.LE.MHBC_KIJ ) THEN
        NJD = LBD*(3*MHBC_JKI+1) + LSP
        IF( NJD.GT.LJD ) THEN
          INDX = 5
          CHMSG = 'Number of Banded Matrix Rows ' //
     &      '> Parameter LJD'
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Equation ordering using J,K,I ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 305 I = 1,IFLD
        DO 305 K = 1,KFLD
        DO 305 J = 1,JFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 305
          NC = NC + 1
          IXP(N) = NC
  305   CONTINUE
!
!---    Initialize counter  ---
!
        NC = 0
        MHBC = 0
        MHBT = 0
!
!---    Coupled equations half band width using J,K,I ordering  ---
!
        DO 330 I = 1,IFLD
        DO 330 K = 1,KFLD
        DO 330 J = 1,JFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 330
          DO 310 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
  310     CONTINUE
          DO 311 M = 1,ISVC
            NC = NC + 1
            IM_M(M,NMD) = NC
  311     CONTINUE
  330   CONTINUE
!
!---    Determine matrix half-band widths using J,K,I ordering  ---
!
        DO 360 I = 1,IFLD
        DO 360 K = 1,KFLD
        DO 360 J = 1,JFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 360
!
!---      Node  ---
!
          MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Fracture to matrix connections  ---
!
          MHBC = MAX(MHBC,ABS(IM(1,NP)-IM_M(ISVC,NP)),
     &      ABS(IM(ISVC,NP)-IM_M(1,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              NB = ABS(IXP(N-IJFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &          ABS(IM(ISVC,NP)-IM(1,NB)))
              MHBT = MAX( MHBT,ABS(NP-NB) )
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              NS = ABS(IXP(N-IFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &          ABS(IM(ISVC,NP)-IM(1,NS)))
              MHBT = MAX( MHBT,ABS(NP-NS) )
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              NW = ABS(IXP(N-1))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &          ABS(IM(ISVC,NP)-IM(1,NW)))
              MHBT = MAX( MHBT,ABS(NP-NW) )
            ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              NE = ABS(IXP(N+1))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &          ABS(IM(ISVC,NP)-IM(1,NE)))
              MHBT = MAX( MHBT,ABS(NP-NE) )
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
            IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              NN = ABS(IXP(N+IFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &           ABS(IM(ISVC,NP)-IM(1,NN)))
              MHBT = MAX( MHBT,ABS(NP-NN) )
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
            IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              NT = ABS(IXP(N+IJFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &          ABS(IM(ISVC,NP)-IM(1,NT)))
              MHBT = MAX( MHBT,ABS(NP-NT) )
            ENDIF
          ENDIF
  360   CONTINUE
!
!---  K,I,J ordering yields the lowest half band width   ---
!
      ELSE
        NJD = LBD*(3*MHBC_KIJ+1) + LSP
        IF( NJD.GT.LJD ) THEN
          INDX = 5
          CHMSG = 'Number of Banded Matrix Rows ' //
     &      '> Parameter LJD'
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Equation ordering using K,I,J ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 405 J = 1,JFLD
        DO 405 I = 1,IFLD
        DO 405 K = 1,KFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 405
          NC = NC + 1
          IXP(N) = NC
  405   CONTINUE
!
!---    Initialize counter  ---
!
        NC = 0
        MHBC = 0
        MHBT = 0
!
!---    Coupled equations half band width using K,I,J ordering  ---
!
        DO 430 J = 1,JFLD
        DO 430 I = 1,IFLD
        DO 430 K = 1,KFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 430
          DO 410 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
  410     CONTINUE
          DO 411 M = 1,ISVC
            NC = NC+1
            IM_M(M,NMD) = NC
  411     CONTINUE
  430   CONTINUE
!
!---    Determine matrix half-band widths using K,I,J ordering  ---
!
        DO 460 J = 1,JFLD
        DO 460 I = 1,IFLD
        DO 460 K = 1,KFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 460
!
!---      Node  ---
!
          MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Fracture to matrix connections  ---
!
          MHBC = MAX(MHBC,ABS(IM(1,NP)-IM_M(ISVC,NP)),
     &      ABS(IM(ISVC,NP)-IM_M(1,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              NB = ABS(IXP(N-IJFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &          ABS(IM(ISVC,NP)-IM(1,NB)))
              MHBT = MAX( MHBT,ABS(NP-NB) )
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              NS = ABS(IXP(N-IFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &          ABS(IM(ISVC,NP)-IM(1,NS)))
              MHBT = MAX( MHBT,ABS(NP-NS) )
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              NW = ABS(IXP(N-1))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &          ABS(IM(ISVC,NP)-IM(1,NW)))
              MHBT = MAX( MHBT,ABS(NP-NW) )
            ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              NE = ABS(IXP(N+1))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &          ABS(IM(ISVC,NP)-IM(1,NE)))
              MHBT = MAX( MHBT,ABS(NP-NE) )
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
            IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              NN = ABS(IXP(N+IFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &           ABS(IM(ISVC,NP)-IM(1,NN)))
              MHBT = MAX( MHBT,ABS(NP-NN) )
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
            IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              NT = ABS(IXP(N+IJFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &          ABS(IM(ISVC,NP)-IM(1,NT)))
              MHBT = MAX( MHBT,ABS(NP-NT) )
            ENDIF
          ENDIF
  460   CONTINUE
      ENDIF
      MLC = MHBC
      MLT = ISVT*MHBT + ISVT - 1
      MUC = MHBC
      MUT = ISVT*MHBT + ISVT - 1
      MDC = MLC + MUC + 1
      MDT = MLT + MUT + 1
!
!---  SPLIB .or. Lis .or. PETSc Solver  ---
!
      IF( ILES.EQ.3 .OR. ILES.EQ.4 .OR. ILES.EQ.5 ) THEN
!
!---    X-Y Plane yields the lowest band width.
!       Load Jacobian matrix in the increment order I,J,K  ---
!
        IF( MHBC_IJK.LE.MHBC_JKI .AND. MHBC_IJK.LE.MHBC_KIJ ) THEN
          NC = 0
          NCC = 0




          NLU(1) = 1
          NLUC(1) = 1

          DO 800 K = 1,KFLD
          DO 800 J = 1,JFLD
          DO 800 I = 1,IFLD
            N = ND(I,J,K)
            IF( IXP(N).LE.0 ) CYCLE
            NP = IXP(N)
            DO 660 L = 1,ISVC
              NMD = IM(L,NP)
              MA = 0
!
!---          Node  ---
!
              DO 500 M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM(M,NP)

                KLU(NMD,M+MA) = NC

  500         CONTINUE
              MA = MA + ISVC
!
!---          Bottom node neighbors  ---
!
              DO 512 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                DO 510 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NB)

                  KLU(NMD,M+MA) = NC

  510           CONTINUE
                MA = MA + ISVC
  512         CONTINUE
!
!---          South node neighbors  ---
!
              DO 522 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                DO 520 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NS)

                  KLU(NMD,M+MA) = NC

  520           CONTINUE
                MA = MA + ISVC
  522         CONTINUE
!
!---          West node neighbors  ---
!
              DO 532 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                DO 530 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NW)

                  KLU(NMD,M+MA) = NC

  530           CONTINUE
                MA = MA + ISVC
  532         CONTINUE
!
!---          East node neighbors  ---
!
              DO 542 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                DO 540 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NE)

                  KLU(NMD,M+MA) = NC

  540           CONTINUE
                MA = MA + ISVC
  542         CONTINUE
!
!---          North node neighbors  ---
!
              DO 552 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                DO 550 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NN)

                  KLU(NMD,M+MA) = NC

  550           CONTINUE
                MA = MA + ISVC
  552         CONTINUE
!
!---          Top node neighbors  ---
!
              DO 562 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                DO 560 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NT)

                  KLU(NMD,M+MA) = NC

  560           CONTINUE
                MA = MA + ISVC
  562         CONTINUE



              NLU(NMD+1) = NC+1

  660       CONTINUE
!
!---        Loop over matrix node equations  ---
!
            L1: DO L = 1,ISVC
              NMD = IM_M(L,NP)
!
!---          Matrix node equations ---
!
              DO M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM_M(M,NP)

                KLU(NMD,M) = NC

              ENDDO
!
!---          Fracture node equations  ---
!
              MA = ISVC
              DO M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM(M,NP)

                KLU(NMD,M+MA) = NC

              ENDDO



              NLU(NMD+1) = NC+1

            ENDDO L1
!
!---        Solute transport equations  ---
!
            MA = 1
!
!---        Node  ---
!
            NCC = NCC+1




            MLUC(NCC) = NP

            KLUC(NP,MA) = NCC

            MA = MA + 1
!
!---        Bottom node neighbors  ---
!
            DO 702 MX = 1,4
              NB = ICM(MX,1,N)
              IF( NB.EQ.0 ) EXIT
              NB = IXP(NB)
              NCC = NCC+1




              MLUC(NCC) = NB

              KLUC(NP,MA) = NCC

              MA = MA + 1
  702       CONTINUE
!
!---        South node neighbors  ---
!
            DO 712 MX = 1,4
              NS = ICM(MX,2,N)
              IF( NS.EQ.0 ) EXIT
              NS = IXP(NS)
              NCC = NCC+1




              MLUC(NCC) = NS

              KLUC(NP,MA) = NCC

              MA = MA + 1
  712       CONTINUE
!
!---        West node neighbors  ---
!
            DO 722 MX = 1,4
              NW = ICM(MX,3,N)
              IF( NW.EQ.0 ) EXIT
              NW = IXP(NW)
              NCC = NCC+1




              MLUC(NCC) = NW

              KLUC(NP,MA) = NCC

              MA = MA + 1
  722       CONTINUE
!
!---        East node neighbors  ---
!
            DO 732 MX = 1,4
              NE = ICM(MX,4,N)
              IF( NE.EQ.0 ) EXIT
              NE = IXP(NE)
              NCC = NCC+1




              MLUC(NCC) = NE

              KLUC(NP,MA) = NCC

              MA = MA + 1
  732       CONTINUE
!
!---        North node neighbors  ---
!
            DO 742 MX = 1,4
              NN = ICM(MX,5,N)
              IF( NN.EQ.0 ) EXIT
              NN = IXP(NN)
              NCC = NCC+1




              MLUC(NCC) = NN

              KLUC(NP,MA) = NCC

              MA = MA + 1
  742       CONTINUE
!
!---        Top node neighbors  ---
!
            DO 752 MX = 1,4
              NT = ICM(MX,6,N)
              IF( NT.EQ.0 ) EXIT
              NT = IXP(NT)
              NCC = NCC+1




              MLUC(NCC) = NT

              KLUC(NP,MA) = NCC

              MA = MA + 1
  752       CONTINUE




            NLUC(NP+1) = NCC+1


  800     CONTINUE
          MKC = NC
          MKT = NCC
!
!---    Y-Z Plane yields the lowest band width.
!       Load Jacobian matrix in the increment order J,K,I  ---
!
        ELSEIF( MHBC_JKI.LE.MHBC_KIJ ) THEN
          NC = 0
          NCC = 0




          NLU(1) = 1
          NLUC(1) = 1

          DO 1800 I = 1,IFLD
          DO 1800 K = 1,KFLD
          DO 1800 J = 1,JFLD
            N = ND(I,J,K)
            IF(  IXP(N).LE.0 ) CYCLE
            NP = IXP(N)
            DO 1660 L = 1,ISVC
              NMD = IM(L,NP)
              MA = 0
!
!---          Node  ---
!
              DO 1500 M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM(M,NP)

                KLU(NMD,M+MA) = NC

 1500         CONTINUE
              MA = MA + ISVC
!
!---          Bottom node neighbors  ---
!
              DO 1512 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                DO 1510 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NB)

                  KLU(NMD,M+MA) = NC

 1510           CONTINUE
                MA = MA + ISVC
 1512         CONTINUE
!
!---          South node neighbors  ---
!
              DO 1522 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                DO 1520 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NS)

                  KLU(NMD,M+MA) = NC

 1520           CONTINUE
                MA = MA + ISVC
 1522         CONTINUE
!
!---          West node neighbors  ---
!
              DO 1532 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                DO 1530 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NW)

                  KLU(NMD,M+MA) = NC

 1530           CONTINUE
                MA = MA + ISVC
 1532         CONTINUE
!
!---          East node neighbors  ---
!
              DO 1542 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                DO 1540 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NE)

                  KLU(NMD,M+MA) = NC

 1540           CONTINUE
                MA = MA + ISVC
 1542         CONTINUE
!
!---          North node neighbors  ---
!
              DO 1552 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                DO 1550 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NN)

                  KLU(NMD,M+MA) = NC

 1550           CONTINUE
                MA = MA + ISVC
 1552         CONTINUE
!
!---          Top node neighbors  ---
!
              DO 1562 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                DO 1560 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NT)

                  KLU(NMD,M+MA) = NC

 1560           CONTINUE
                MA = MA + ISVC
 1562         CONTINUE



              NLU(NMD+1) = NC+1

 1660       CONTINUE
!
!---        Loop over matrix node equations  ---
!
            L2: DO L = 1,ISVC
              NMD = IM_M(L,NP)
!
!---          Matrix node equations ---
!
              DO M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM_M(M,NP)

                KLU(NMD,M) = NC

              ENDDO
!
!---          Fracture node equations  ---
!
              MA = ISVC
              DO M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM(M,NP)

                KLU(NMD,M+MA) = NC

              ENDDO



              NLU(NMD+1) = NC+1

            ENDDO L2
!
!---        Solute transport equations  ---
!
            MA = 1
!
!---        Node  ---
!
            NCC = NCC+1




            MLUC(NCC) = NP

            KLUC(NP,MA) = NCC

            MA = MA + 1
!
!---        Bottom node neighbors  ---
!
            DO 1702 MX = 1,4
              NB = ICM(MX,1,N)
              IF( NB.EQ.0 ) EXIT
              NB = IXP(NB)
              NCC = NCC+1




              MLUC(NCC) = NB

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1702       CONTINUE
!
!---        South node neighbors  ---
!
            DO 1712 MX = 1,4
              NS = ICM(MX,2,N)
              IF( NS.EQ.0 ) EXIT
              NS = IXP(NS)
              NCC = NCC+1




              MLUC(NCC) = NS

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1712       CONTINUE
!
!---        West node neighbors  ---
!
            DO 1722 MX = 1,4
              NW = ICM(MX,3,N)
              IF( NW.EQ.0 ) EXIT
              NW = IXP(NW)
              NCC = NCC+1




              MLUC(NCC) = NW

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1722       CONTINUE
!
!---        East node neighbors  ---
!
            DO 1732 MX = 1,4
              NE = ICM(MX,4,N)
              IF( NE.EQ.0 ) EXIT
              NE = IXP(NE)
              NCC = NCC+1




              MLUC(NCC) = NE

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1732       CONTINUE
!
!---        North node neighbors  ---
!
            DO 1742 MX = 1,4
              NN = ICM(MX,5,N)
              IF( NN.EQ.0 ) EXIT
              NN = IXP(NN)
              NCC = NCC+1




              MLUC(NCC) = NN

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1742       CONTINUE
!
!---        Top node neighbors  ---
!
            DO 1752 MX = 1,4
              NT = ICM(MX,6,N)
              IF( NT.EQ.0 ) EXIT
              NT = IXP(NT)
              NCC = NCC+1




              MLUC(NCC) = NT

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1752       CONTINUE




            NLUC(NP+1) = NCC+1


 1800     CONTINUE
          MKC = NC
          MKT = NCC
!
!---    Z-X Plane yields the lowest band width.
!       Load Jacobian matrix in the increment order K,I,J  ---
!
        ELSE
          NC = 0
          NCC = 0




          NLU(1) = 1
          NLUC(1) = 1

          DO 2800 J = 1,JFLD
          DO 2800 I = 1,IFLD
          DO 2800 K = 1,KFLD
            N = ND(I,J,K)
            IF( IXP(N).LE.0 ) CYCLE
            NP = IXP(N)
            DO 2660 L = 1,ISVC
              NMD = IM(L,NP)
              MA = 0
!
!---          Node  ---
!
              DO 2500 M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM(M,NP)

                KLU(NMD,M+MA) = NC

 2500         CONTINUE
              MA = MA + ISVC
!
!---          Bottom node neighbors  ---
!
              DO 2512 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                DO 2510 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NB)

                  KLU(NMD,M+MA) = NC

 2510           CONTINUE
                MA = MA + ISVC
 2512         CONTINUE
!
!---          South node neighbors  ---
!
              DO 2522 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                DO 2520 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NS)

                  KLU(NMD,M+MA) = NC

 2520           CONTINUE
                MA = MA + ISVC
 2522         CONTINUE
!
!---          West node neighbors  ---
!
              DO 2532 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                DO 2530 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NW)

                  KLU(NMD,M+MA) = NC

 2530           CONTINUE
                MA = MA + ISVC
 2532         CONTINUE
!
!---          East node neighbors  ---
!
              DO 2542 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                DO 2540 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NE)

                  KLU(NMD,M+MA) = NC

 2540           CONTINUE
                MA = MA + ISVC
 2542         CONTINUE
!
!---          North node neighbors  ---
!
              DO 2552 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                DO 2550 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NN)

                  KLU(NMD,M+MA) = NC

 2550           CONTINUE
                MA = MA + ISVC
 2552         CONTINUE
!
!---          Top node neighbors  ---
!
              DO 2562 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                DO 2560 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NT)

                  KLU(NMD,M+MA) = NC

 2560           CONTINUE
                MA = MA + ISVC
 2562         CONTINUE



              NLU(NMD+1) = NC+1

 2660       CONTINUE
!
!---        Loop over matrix node equations  ---
!
            L3: DO L = 1,ISVC
              NMD = IM_M(L,NP)
!
!---          Matrix node equations ---
!
              DO M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM_M(M,NP)

                KLU(NMD,M) = NC

              ENDDO
!
!---          Fracture node equations  ---
!
              MA = ISVC
              DO M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM(M,NP)

                KLU(NMD,M+MA) = NC

              ENDDO



              NLU(NMD+1) = NC+1

            ENDDO L3
!
!---        Solute transport equations  ---
!
            MA = 1
!
!---        Node  ---
!
            NCC = NCC+1




            MLUC(NCC) = NP

            KLUC(NP,MA) = NCC

            MA = MA + 1
!
!---        Bottom node neighbors  ---
!
            DO 2702 MX = 1,4
              NB = ICM(MX,1,N)
              IF( NB.EQ.0 ) EXIT
              NB = IXP(NB)
              NCC = NCC+1




              MLUC(NCC) = NB

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2702       CONTINUE
!
!---        South node neighbors  ---
!
            DO 2712 MX = 1,4
              NS = ICM(MX,2,N)
              IF( NS.EQ.0 ) EXIT
              NS = IXP(NS)
              NCC = NCC+1




              MLUC(NCC) = NS

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2712       CONTINUE
!
!---        West node neighbors  ---
!
            DO 2722 MX = 1,4
              NW = ICM(MX,3,N)
              IF( NW.EQ.0 ) EXIT
              NW = IXP(NW)
              NCC = NCC+1




              MLUC(NCC) = NW

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2722       CONTINUE
!
!---        East node neighbors  ---
!
            DO 2732 MX = 1,4
              NE = ICM(MX,4,N)
              IF( NE.EQ.0 ) EXIT
              NE = IXP(NE)
              NCC = NCC+1




              MLUC(NCC) = NE

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2732       CONTINUE
!
!---        North node neighbors  ---
!
            DO 2742 MX = 1,4
              NN = ICM(MX,5,N)
              IF( NN.EQ.0 ) EXIT
              NN = IXP(NN)
              NCC = NCC+1




              MLUC(NCC) = NN

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2742       CONTINUE
!
!---        Top node neighbors  ---
!
            DO 2752 MX = 1,4
              NT = ICM(MX,6,N)
              IF( NT.EQ.0 ) EXIT
              NT = IXP(NT)
              NCC = NCC+1




              MLUC(NCC) = NT

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2752       CONTINUE




            NLUC(NP+1) = NCC+1


 2800     CONTINUE
          MKC = NC
          MKT = NCC
        ENDIF
      ENDIF
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCBP_NCW_DP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCBP_SFC
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
!     Configure the Jacobian matrix pointer arrays for surface covers
!     for STOMP-GT.
!
!     Nodes adjacent to surface cover nodes include 2 bare surface
!     equations + 3 vegetated surface equations.
!     Nodes connected to surface cover nodes include 3 vegetated
!     surface equations.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 14 November 2015.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PTZR
      USE PLT_ATM
      USE JACOB
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
      SUB_LOG(ISUB_LOG) = '/JCBP_SFC'
      IF( INDEX(SVN_ID(95)(1:1),'$').EQ.0 ) SVN_ID(95) =
     & '$Id: jcbp.F 1080 2017-03-14 16:22:02Z d3c002 $'
      ILIMIT = 2**30
!
!---  Coupled equations half band width using I,J,K ordering  ---
!
      NC = 0
      DO 5 K = 1,KFLD
      DO 5 J = 1,JFLD
      DO 5 I = 1,IFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 5
        NC = NC + 1
        IXP(N) = NC
    5 CONTINUE
      M_IJK = ILIMIT
      NC = 0
      MHBC_IJK = 0
      DO 20 K = 1,KFLD
      DO 20 J = 1,JFLD
      DO 20 I = 1,IFLD
        N = ND(I,J,K)
        NMD = ABS(IXP(N))
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 20
        DO 10 M = 1,ISVC
          NC = NC+1
          IM(M,NMD) = NC
   10   CONTINUE
!
!---    Loop over surface cover nodes  ---
!
        DO NSCN = 1,NSFCN
!
!---      Node adjacent to surface cover node  ---
!
          IF( ICM_SFC(1,NSCN).EQ.N ) THEN
            DO M = 1,5
              NC = NC + 1
              JM_SFC(M,NSCN) = NC
            ENDDO
          ENDIF
        ENDDO
   20 CONTINUE
!
!---  Determine matrix half-band widths, using I,J,K ordering  ---
!
      DO 40 K = 1,KFLD
      DO 40 J = 1,JFLD
      DO 40 I = 1,IFLD
        N = ND(I,J,K)
        NP = ABS(IXP(N))
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 40
!
!---    Node  ---
!
        MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---    Bottom node neighbors  ---
!
        IF( K.GT.1 ) THEN
          IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
            NB = ABS(IXP(N-IJFLD))
            MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NB)),
     &        ABS(IM(ISVC,NP)-IM(1,NB)))
          ENDIF
        ENDIF
!
!---    South node neighbors  ---
!
        IF( J.GT.1 ) THEN
          IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
            NS = ABS(IXP(N-IFLD))
            MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NS)),
     &        ABS(IM(ISVC,NP)-IM(1,NS)))
          ENDIF
        ENDIF
!
!---    West node neighbors  ---
!
        IF( I.GT.1 ) THEN
          IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
            NW = ABS(IXP(N-1))
            MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NW)),
     &        ABS(IM(ISVC,NP)-IM(1,NW)))
          ENDIF
        ENDIF
!
!---    East node neighbors  ---
!
        IF( I.LT.IFLD ) THEN
          IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
            NE = ABS(IXP(N+1))
            MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NE)),
     &        ABS(IM(ISVC,NP)-IM(1,NE)))
          ENDIF
        ENDIF
!
!---    North node neighbors  ---
!
        IF( J.LT.JFLD ) THEN
          IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
            NN = ABS(IXP(N+IFLD))
            MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NN)),
     &         ABS(IM(ISVC,NP)-IM(1,NN)))
          ENDIF
        ENDIF
!
!---    Top node neighbors  ---
!
        IF( K.LT.KFLD ) THEN
          IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
            NT = ABS(IXP(N+IJFLD))
            MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NT)),
     &        ABS(IM(ISVC,NP)-IM(1,NT)))
          ENDIF
        ENDIF
!
!---    Loop over surface cover nodes  ---
!
        DO NSCN = 1,NSFCN
!
!---      Node adjacent to surface cover node, consider ground surface
!         canopy, and plant surface cover equations  ---
!
          IF( ICM_SFC(1,NSCN).EQ.N ) THEN
!
!---        Consider surface cover equation limits  ---
!
            MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-JM_SFC(1,NSCN)),
     &        ABS(IM(ISVC,NP)-JM_SFC(1,NSCN)))
            MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &        ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
!
!---      Check for a nonadjacent node connected to 
!         surface cover node  ---
!
          ELSE
!
!---        Loop over surface cover connection map  ---
!
            DO NSCC = 2,NSFCC
              IF( ICM_SFC(NSCC,NSCN).EQ.0 ) EXIT
!
!---          Node connected to surface cover, only consider
!             canopy, and plant surface cover equations  ---
!
              IF( ICM_SFC(NSCC,NSCN).EQ.N ) THEN
!
!---            Consider surface cover equation limits  ---
!
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-JM_SFC(3,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(3,NSCN)))
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
              ENDIF
            ENDDO
          ENDIF
        ENDDO
   40 CONTINUE
      IF( MHBC_IJK.LT.M_IJK ) THEN
        M_IJK = MHBC_IJK
      ENDIF
!
!---  Coupled equations half band width using J,K,I ordering  ---
!
      NC = 0
      DO 65 I = 1,IFLD
      DO 65 K = 1,KFLD
      DO 65 J = 1,JFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 65
        NC = NC + 1
        IXP(N) = NC
   65 CONTINUE
      M_JKI = ILIMIT
      NC = 0
      MHBC_JKI = 0
      DO 80 I = 1,IFLD
      DO 80 K = 1,KFLD
      DO 80 J = 1,JFLD
        N = ND(I,J,K)
        NMD = ABS(IXP(N))
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 80
        DO 70 M = 1,ISVC
          NC = NC+1
          IM(M,NMD) = NC
   70   CONTINUE
!
!---    Loop over surface cover nodes  ---
!
        DO NSCN = 1,NSFCN
!
!---      Node adjacent to surface cover node  ---
!
          IF( ICM_SFC(1,NSCN).EQ.N ) THEN
            DO M = 1,5
              NC = NC + 1
              JM_SFC(M,NSCN) = NC
            ENDDO
          ENDIF
        ENDDO
   80 CONTINUE
!
!---  Determine matrix half-band widths, using J,K,I ordering  ---
!
      DO 100 I = 1,IFLD
      DO 100 K = 1,KFLD
      DO 100 J = 1,JFLD
        N = ND(I,J,K)
        NP = ABS(IXP(N))
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 100
!
!---    Node  ---
!
        MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---    Bottom node neighbors  ---
!
        IF( K.GT.1 ) THEN
          IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
            NB = ABS(IXP(N-IJFLD))
            MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NB)),
     &        ABS(IM(ISVC,NP)-IM(1,NB)))
          ENDIF
        ENDIF
!
!---    South node neighbors  ---
!
        IF( J.GT.1 ) THEN
          IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
            NS = ABS(IXP(N-IFLD))
            MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NS)),
     &        ABS(IM(ISVC,NP)-IM(1,NS)))
          ENDIF
        ENDIF
!
!---    West node neighbors  ---
!
        IF( I.GT.1 ) THEN
          IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
            NW = ABS(IXP(N-1))
            MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NW)),
     &        ABS(IM(ISVC,NP)-IM(1,NW)))
          ENDIF
        ENDIF
!
!---    East node neighbors  ---
!
        IF( I.LT.IFLD ) THEN
          IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
            NE = ABS(IXP(N+1))
            MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NE)),
     &        ABS(IM(ISVC,NP)-IM(1,NE)))
          ENDIF
        ENDIF
!
!---    North node neighbors  ---
!
        IF( J.LT.JFLD ) THEN
          IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
            NN = ABS(IXP(N+IFLD))
            MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NN)),
     &         ABS(IM(ISVC,NP)-IM(1,NN)))
          ENDIF
        ENDIF
!
!---    Top node neighbors  ---
!
        IF( K.LT.KFLD ) THEN
          IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
            NT = ABS(IXP(N+IJFLD))
            MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NT)),
     &        ABS(IM(ISVC,NP)-IM(1,NT)))
          ENDIF
        ENDIF
!
!---    Loop over surface cover nodes  ---
!
        DO NSCN = 1,NSFCN
!
!---      Node adjacent to surface cover node, consider ground surface
!         canopy, and plant surface cover equations  ---
!
          IF( ICM_SFC(1,NSCN).EQ.N ) THEN
!
!---        Consider surface cover equation limits  ---
!
            MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-JM_SFC(1,NSCN)),
     &        ABS(IM(ISVC,NP)-JM_SFC(1,NSCN)))
            MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &        ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
!
!---      Check for a nonadjacent node connected to 
!         surface cover node  ---
!
          ELSE
!
!---        Loop over surface cover connection map  ---
!
            DO NSCC = 2,NSFCC
              IF( ICM_SFC(NSCC,NSCN).EQ.0 ) EXIT
!
!---          Node connected to surface cover, only consider
!             canopy, and plant surface cover equations  ---
!
              IF( ICM_SFC(NSCC,NSCN).EQ.N ) THEN
!
!---            Consider surface cover equation limits  ---
!
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-JM_SFC(3,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(3,NSCN)))
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
              ENDIF
            ENDDO
          ENDIF
        ENDDO
  100 CONTINUE
      IF( MHBC_JKI.LT.M_JKI ) THEN
        M_JKI = MHBC_JKI
      ENDIF
!
!---  Coupled equations half band width using K,I,J ordering  ---
!
      NC = 0
      DO 125 J = 1,JFLD
      DO 125 I = 1,IFLD
      DO 125 K = 1,KFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 125
        NC = NC + 1
        IXP(N) = NC
  125 CONTINUE
      M_KIJ = ILIMIT
      NC = 0
      MHBC_KIJ = 0
      DO 140 J = 1,JFLD
      DO 140 I = 1,IFLD
      DO 140 K = 1,KFLD
        N = ND(I,J,K)
        NMD = ABS(IXP(N))
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 140
        DO 130 M = 1,ISVC
          NC = NC+1
          IM(M,NMD) = NC
  130   CONTINUE
!
!---    Loop over surface cover nodes  ---
!
        DO NSCN = 1,NSFCN
!
!---      Node adjacent to surface cover node  ---
!
          IF( ICM_SFC(1,NSCN).EQ.N ) THEN
            DO M = 1,5
              NC = NC + 1
              JM_SFC(M,NSCN) = NC
            ENDDO
          ENDIF
        ENDDO
  140 CONTINUE
!
!---  Determine matrix half-band widths, using K,I,J ordering  ---
!
      DO 160 J = 1,JFLD
      DO 160 I = 1,IFLD
      DO 160 K = 1,KFLD
        N = ND(I,J,K)
        NP = ABS(IXP(N))
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 160
!
!---    Node  ---
!
        MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---    Bottom node neighbors  ---
!
        IF( K.GT.1 ) THEN
          IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
            NB = ABS(IXP(N-IJFLD))
            MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NB)),
     &        ABS(IM(ISVC,NP)-IM(1,NB)))
          ENDIF
        ENDIF
!
!---    South node neighbors  ---
!
        IF( J.GT.1 ) THEN
          IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
            NS = ABS(IXP(N-IFLD))
            MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NS)),
     &        ABS(IM(ISVC,NP)-IM(1,NS)))
          ENDIF
        ENDIF
!
!---    West node neighbors  ---
!
        IF( I.GT.1 ) THEN
          IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
            NW = ABS(IXP(N-1))
            MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NW)),
     &        ABS(IM(ISVC,NP)-IM(1,NW)))
          ENDIF
        ENDIF
!
!---    East node neighbors  ---
!
        IF( I.LT.IFLD ) THEN
          IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
            NE = ABS(IXP(N+1))
            MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NE)),
     &        ABS(IM(ISVC,NP)-IM(1,NE)))
          ENDIF
        ENDIF
!
!---    North node neighbors  ---
!
        IF( J.LT.JFLD ) THEN
          IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
            NN = ABS(IXP(N+IFLD))
            MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NN)),
     &         ABS(IM(ISVC,NP)-IM(1,NN)))
          ENDIF
        ENDIF
!
!---    Top node neighbors  ---
!
        IF( K.LT.KFLD ) THEN
          IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
            NT = ABS(IXP(N+IJFLD))
            MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NT)),
     &        ABS(IM(ISVC,NP)-IM(1,NT)))
          ENDIF
        ENDIF
!
!---    Loop over surface cover nodes  ---
!
        DO NSCN = 1,NSFCN
!
!---      Node adjacent to surface cover node, consider ground surface
!         canopy, and plant surface cover equations  ---
!
          IF( ICM_SFC(1,NSCN).EQ.N ) THEN
!
!---        Consider surface cover equation limits  ---
!
            MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-JM_SFC(1,NSCN)),
     &        ABS(IM(ISVC,NP)-JM_SFC(1,NSCN)))
            MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &        ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
!
!---      Check for a nonadjacent node connected to 
!         surface cover node  ---
!
          ELSE
!
!---        Loop over surface cover connection map  ---
!
            DO NSCC = 2,NSFCC
              IF( ICM_SFC(NSCC,NSCN).EQ.0 ) EXIT
!
!---          Node connected to surface cover, only consider
!             canopy, and plant surface cover equations  ---
!
              IF( ICM_SFC(NSCC,NSCN).EQ.N ) THEN
!
!---            Consider surface cover equation limits  ---
!
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-JM_SFC(3,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(3,NSCN)))
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
              ENDIF
            ENDDO
          ENDIF
        ENDDO
  160 CONTINUE
      IF( MHBC_KIJ.LT.M_KIJ ) THEN
        M_KIJ = MHBC_KIJ
      ENDIF
!
!---  Loop over wells searching for the maximum half band width   ---
!
      MHBC_IJK = 0
      MHBC_JKI = 0
      MHBC_KIJ = 0
      IF( M_IJK.GT.MHBC_IJK ) MHBC_IJK = M_IJK
      IF( M_JKI.GT.MHBC_JKI ) MHBC_JKI = M_JKI
      IF( M_KIJ.GT.MHBC_KIJ ) MHBC_KIJ = M_KIJ
!
!---  I,J,K ordering yields the lowest half band width   ---
!
      IF( MHBC_IJK.LE.MHBC_JKI .AND. MHBC_IJK.LE.MHBC_KIJ ) THEN
        NJD = LBD*(3*MHBC_IJK+1) + LSP
        IF( NJD.GT.LJD ) THEN
          INDX = 5
          CHMSG = 'Number of Banded Matrix Rows ' //
     &      '> Parameter LJD'
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Equation ordering using I,J,K ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 205 K = 1,KFLD
        DO 205 J = 1,JFLD
        DO 205 I = 1,IFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 205
          NC = NC + 1
          IXP(N) = NC
  205   CONTINUE
!
!---    Initialize counter  ---
!
        NC = 0
        MHBC = 0
        MHBT = 0
!
!---    Coupled equations half band width using I,J,K ordering  ---
!
        DO 230 K = 1,KFLD
        DO 230 J = 1,JFLD
        DO 230 I = 1,IFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 230
          DO 210 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
  210     CONTINUE
!
!---      Loop over surface cover nodes  ---
!
          DO NSCN = 1,NSFCN
!
!---        Node adjacent to surface cover node  ---
!
            IF( ICM_SFC(1,NSCN).EQ.N ) THEN
              DO M = 1,5
                NC = NC + 1
                JM_SFC(M,NSCN) = NC
              ENDDO
            ENDIF
          ENDDO
  230   CONTINUE
!
!---    Determine matrix half-band widths, using I,J,K ordering  ---
!
        DO 260 K = 1,KFLD
        DO 260 J = 1,JFLD
        DO 260 I = 1,IFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 260
!
!---      Node  ---
!
          MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              NB = ABS(IXP(N-IJFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &          ABS(IM(ISVC,NP)-IM(1,NB)))
              MHBT = MAX( MHBT,ABS(NP-NB) )
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              NS = ABS(IXP(N-IFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &          ABS(IM(ISVC,NP)-IM(1,NS)))
              MHBT = MAX( MHBT,ABS(NP-NS) )
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              NW = ABS(IXP(N-1))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &          ABS(IM(ISVC,NP)-IM(1,NW)))
              MHBT = MAX( MHBT,ABS(NP-NW) )
            ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              NE = ABS(IXP(N+1))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &          ABS(IM(ISVC,NP)-IM(1,NE)))
              MHBT = MAX( MHBT,ABS(NP-NE) )
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
            IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              NN = ABS(IXP(N+IFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &           ABS(IM(ISVC,NP)-IM(1,NN)))
              MHBT = MAX( MHBT,ABS(NP-NN) )
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
            IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              NT = ABS(IXP(N+IJFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &          ABS(IM(ISVC,NP)-IM(1,NT)))
              MHBT = MAX( MHBT,ABS(NP-NT) )
            ENDIF
          ENDIF
!
!---    Loop over surface cover nodes  ---
!
        DO NSCN = 1,NSFCN
!
!---      Node adjacent to surface cover node, consider ground surface
!         canopy, and plant surface cover equations  ---
!
          IF( ICM_SFC(1,NSCN).EQ.N ) THEN
!
!---        Consider surface cover equation limits  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(1,NSCN)),
     &        ABS(IM(ISVC,NP)-JM_SFC(1,NSCN)))
            MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &        ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
!
!---      Check for a nonadjacent node connected to 
!         surface cover node  ---
!
          ELSE
!
!---        Loop over surface cover connection map  ---
!
            DO NSCC = 2,NSFCC
              IF( ICM_SFC(NSCC,NSCN).EQ.0 ) EXIT
!
!---          Node connected to surface cover, only consider
!             canopy, and plant surface cover equations  ---
!
              IF( ICM_SFC(NSCC,NSCN).EQ.N ) THEN
!
!---            Consider surface cover equation limits  ---
!
                MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(3,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(3,NSCN)))
                MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
              ENDIF
            ENDDO
          ENDIF
        ENDDO
  260   CONTINUE
!
!---  J,K,I ordering yields the lowest half band width   ---
!
      ELSEIF( MHBC_JKI.LE.MHBC_KIJ ) THEN
        NJD = LBD*(3*MHBC_JKI+1) + LSP
        IF( NJD.GT.LJD ) THEN
          INDX = 5
          CHMSG = 'Number of Banded Matrix Rows ' //
     &      '> Parameter LJD'
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Equation ordering using J,K,I ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 305 I = 1,IFLD
        DO 305 K = 1,KFLD
        DO 305 J = 1,JFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 305
          NC = NC + 1
          IXP(N) = NC
  305   CONTINUE
!
!---    Initialize counter  ---
!
        NC = 0
        MHBC = 0
        MHBT = 0
!
!---    Coupled equations half band width using J,K,I ordering  ---
!
        DO 330 I = 1,IFLD
        DO 330 K = 1,KFLD
        DO 330 J = 1,JFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 330
          DO 310 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
  310     CONTINUE
!
!---      Loop over surface cover nodes  ---
!
          DO NSCN = 1,NSFCN
!
!---        Node adjacent to surface cover node  ---
!
            IF( ICM_SFC(1,NSCN).EQ.N ) THEN
              DO M = 1,5
                NC = NC + 1
                JM_SFC(M,NSCN) = NC
              ENDDO
            ENDIF
          ENDDO
  330   CONTINUE
!
!---    Determine matrix half-band widths using J,K,I ordering  ---
!
        DO 360 I = 1,IFLD
        DO 360 K = 1,KFLD
        DO 360 J = 1,JFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 360
!
!---      Node  ---
!
          MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              NB = ABS(IXP(N-IJFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &          ABS(IM(ISVC,NP)-IM(1,NB)))
              MHBT = MAX( MHBT,ABS(NP-NB) )
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              NS = ABS(IXP(N-IFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &          ABS(IM(ISVC,NP)-IM(1,NS)))
              MHBT = MAX( MHBT,ABS(NP-NS) )
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              NW = ABS(IXP(N-1))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &          ABS(IM(ISVC,NP)-IM(1,NW)))
              MHBT = MAX( MHBT,ABS(NP-NW) )
            ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              NE = ABS(IXP(N+1))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &          ABS(IM(ISVC,NP)-IM(1,NE)))
              MHBT = MAX( MHBT,ABS(NP-NE) )
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
            IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              NN = ABS(IXP(N+IFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &           ABS(IM(ISVC,NP)-IM(1,NN)))
              MHBT = MAX( MHBT,ABS(NP-NN) )
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
            IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              NT = ABS(IXP(N+IJFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &          ABS(IM(ISVC,NP)-IM(1,NT)))
              MHBT = MAX( MHBT,ABS(NP-NT) )
            ENDIF
          ENDIF
!
!---    Loop over surface cover nodes  ---
!
        DO NSCN = 1,NSFCN
!
!---      Node adjacent to surface cover node, consider ground surface
!         canopy, and plant surface cover equations  ---
!
          IF( ICM_SFC(1,NSCN).EQ.N ) THEN
!
!---        Consider surface cover equation limits  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(1,NSCN)),
     &        ABS(IM(ISVC,NP)-JM_SFC(1,NSCN)))
            MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &        ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
!
!---      Check for a nonadjacent node connected to 
!         surface cover node  ---
!
          ELSE
!
!---        Loop over surface cover connection map  ---
!
            DO NSCC = 2,NSFCC
              IF( ICM_SFC(NSCC,NSCN).EQ.0 ) EXIT
!
!---          Node connected to surface cover, only consider
!             canopy, and plant surface cover equations  ---
!
              IF( ICM_SFC(NSCC,NSCN).EQ.N ) THEN
!
!---            Consider surface cover equation limits  ---
!
                MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(3,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(3,NSCN)))
                MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
              ENDIF
            ENDDO
          ENDIF
        ENDDO
  360   CONTINUE
!
!---  K,I,J ordering yields the lowest half band width   ---
!
      ELSE
        NJD = LBD*(3*MHBC_KIJ+1) + LSP
        IF( NJD.GT.LJD ) THEN
          INDX = 5
          CHMSG = 'Number of Banded Matrix Rows ' //
     &      '> Parameter LJD'
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Equation ordering using K,I,J ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 405 J = 1,JFLD
        DO 405 I = 1,IFLD
        DO 405 K = 1,KFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 405
          NC = NC + 1
          IXP(N) = NC
  405   CONTINUE
!
!---    Initialize counter  ---
!
        NC = 0
        MHBC = 0
        MHBT = 0
!
!---    Coupled equations half band width using K,I,J ordering  ---
!
        DO 430 J = 1,JFLD
        DO 430 I = 1,IFLD
        DO 430 K = 1,KFLD
          N = ND(I,J,K)
          NMD = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 430
          DO 410 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
  410     CONTINUE
!
!---      Loop over surface cover nodes  ---
!
          DO NSCN = 1,NSFCN
!
!---        Node adjacent to surface cover node  ---
!
            IF( ICM_SFC(1,NSCN).EQ.N ) THEN
              DO M = 1,5
                NC = NC + 1
                JM_SFC(M,NSCN) = NC
              ENDDO
            ENDIF
          ENDDO
  430   CONTINUE
!
!---    Determine matrix half-band widths using K,I,J ordering  ---
!
        DO 460 J = 1,JFLD
        DO 460 I = 1,IFLD
        DO 460 K = 1,KFLD
          N = ND(I,J,K)
          NP = ABS(IXP(N))
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 460
!
!---      Node  ---
!
          MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              NB = ABS(IXP(N-IJFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &          ABS(IM(ISVC,NP)-IM(1,NB)))
              MHBT = MAX( MHBT,ABS(NP-NB) )
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              NS = ABS(IXP(N-IFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &          ABS(IM(ISVC,NP)-IM(1,NS)))
              MHBT = MAX( MHBT,ABS(NP-NS) )
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              NW = ABS(IXP(N-1))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &          ABS(IM(ISVC,NP)-IM(1,NW)))
              MHBT = MAX( MHBT,ABS(NP-NW) )
            ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.IFLD ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              NE = ABS(IXP(N+1))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &          ABS(IM(ISVC,NP)-IM(1,NE)))
              MHBT = MAX( MHBT,ABS(NP-NE) )
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.JFLD ) THEN
            IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              NN = ABS(IXP(N+IFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &           ABS(IM(ISVC,NP)-IM(1,NN)))
              MHBT = MAX( MHBT,ABS(NP-NN) )
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.KFLD ) THEN
            IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              NT = ABS(IXP(N+IJFLD))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &          ABS(IM(ISVC,NP)-IM(1,NT)))
              MHBT = MAX( MHBT,ABS(NP-NT) )
            ENDIF
          ENDIF
!
!---      Loop over surface cover nodes  ---
!
          DO NSCN = 1,NSFCN
!
!---        Node adjacent to surface cover node, consider ground surface
!           canopy, and plant surface cover equations  ---
!
            IF( ICM_SFC(1,NSCN).EQ.N ) THEN
!
!---          Consider surface cover equation limits  ---
!
              MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(1,NSCN)),
     &          ABS(IM(ISVC,NP)-JM_SFC(1,NSCN)))
              MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &          ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
!
!---        Check for a nonadjacent node connected to 
!           surface cover node  ---
!
            ELSE
!
!---          Loop over surface cover connection map  ---
!
              DO NSCC = 2,NSFCC
                IF( ICM_SFC(NSCC,NSCN).EQ.0 ) EXIT
!
!---            Node connected to surface cover, only consider
!               canopy, and plant surface cover equations  ---
!
                IF( ICM_SFC(NSCC,NSCN).EQ.N ) THEN
!
!---              Consider surface cover equation limits  ---
!
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(3,NSCN)),
     &              ABS(IM(ISVC,NP)-JM_SFC(3,NSCN)))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &              ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
                ENDIF
              ENDDO
            ENDIF
          ENDDO
  460   CONTINUE
      ENDIF
      MLC = MHBC
      MLT = ISVT*MHBT + ISVT - 1
      MUC = MHBC
      MUT = ISVT*MHBT + ISVT - 1
      MDC = MLC + MUC + 1
      MDT = MLT + MUT + 1
!
!---  SPLIB .or. Lis .or. PETSc Solver  ---
!
      IF( ILES.EQ.3 .OR. ILES.EQ.4 .OR. ILES.EQ.5 ) THEN
!
!---    X-Y Plane yields the lowest band width.
!       Load Jacobian matrix in the increment order I,J,K  ---
!
        IF( MHBC_IJK.LE.MHBC_JKI .AND. MHBC_IJK.LE.MHBC_KIJ ) THEN
          NC = 0
          NCC = 0




          NLU(1) = 1
          NLUC(1) = 1

          DO 800 K = 1,KFLD
          DO 800 J = 1,JFLD
          DO 800 I = 1,IFLD
            N = ND(I,J,K)
            IF( IXP(N).EQ.0 ) CYCLE
            NP = IXP(N)
            DO 660 L = 1,ISVC
              NMD = IM(L,NP)
              MA = 0
!
!---          Node  ---
!
              DO 500 M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM(M,NP)

                KLU(NMD,M+MA) = NC

  500         CONTINUE
              MA = MA + ISVC
!
!---          Bottom node neighbors  ---
!
              DO 512 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                DO 510 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NB)

                  KLU(NMD,M+MA) = NC

  510           CONTINUE
                MA = MA + ISVC
  512         CONTINUE
!
!---          South node neighbors  ---
!
              DO 522 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                DO 520 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NS)

                  KLU(NMD,M+MA) = NC

  520           CONTINUE
                MA = MA + ISVC
  522         CONTINUE
!
!---          West node neighbors  ---
!
              DO 532 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                DO 530 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NW)

                  KLU(NMD,M+MA) = NC

  530           CONTINUE
                MA = MA + ISVC
  532         CONTINUE
!
!---          East node neighbors  ---
!
              DO 542 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                DO 540 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NE)

                  KLU(NMD,M+MA) = NC

  540           CONTINUE
                MA = MA + ISVC
  542         CONTINUE
!
!---          North node neighbors  ---
!
              DO 552 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                DO 550 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NN)

                  KLU(NMD,M+MA) = NC

  550           CONTINUE
                MA = MA + ISVC
  552         CONTINUE
!
!---          Top node neighbors  ---
!
              DO 562 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                DO 560 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NT)

                  KLU(NMD,M+MA) = NC

  560           CONTINUE
                MA = MA + ISVC
  562         CONTINUE
!
!---          Loop over surface cover nodes  ---
!
              DO 650 NSCN = 1,NSFCN
!
!---            Node is adjacent to a surface cover node, include 
!               bare and vegetaged surface cover equations in 
!               nodal equations  ---
!
                IF( ICM_SFC(1,NSCN).EQ.N ) THEN
                  MA = 13 + 2*ISVC + 3*ISVC*NSFCC + 5*(L-1)
                  DO M = 1,5
                    NC = NC + 1




                    MLU(NC) = JM_SFC(M,NSCN)

                    KLU_SFC(M+MA,NSCN) = NC

                  ENDDO
                ENDIF
  650         CONTINUE



              NLU(NMD+1) = NC+1

  660       CONTINUE
!
!---        Bare and vegetated surface cover equations, 
!           loop over surface cover nodes  ---
!
            DO 690 NSCN = 1,NSFCN
!
!---          Node is adjacent to surface cover node,
!             include bare and vegetated surface cover equations  ---
!
              IF( ICM_SFC(1,NSCN).EQ.N ) THEN
!
!---            Bare surface cover equations  ---
!
                DO L = 1,2
                  MA = (L-1)*2
                  DO M = 1,2
                    NC = NC+1




                    MLU(NC) = JM_SFC(M,NSCN)

                    KLU_SFC(M+MA,NSCN) = NC

                  ENDDO
                ENDDO
!
!---            Vegetated surface cover equations  ---
!
                DO L = 1,3
                  MA = 4 + (L-1)*3
                  DO M = 1,3
                    NC = NC+1




                    MLU(NC) = JM_SFC(M+2,NSCN)

                    KLU_SFC(M+MA,NSCN) = NC

                  ENDDO
                ENDDO
!
!---            Include nodal equations for nodes adjacent to 
!               the surface cover node in surface cover equations  ---
!
                NPX = IXP(ICM_SFC(1,NSCN))
                MA = 13
!
!---            Loop over number of field node unknowns  ---
!
                DO 670 L = 1,ISVC
                  MA = MA + (L-1)*ISVC
!
!---              Loop over number of bare and vegetated 
!                 surface cover node unknowns  ---
!
                  DO M = 1,5
                    NC = NC+1




                    MLU(NC) = IM(M,NPX)

                    KLU_SFC(M+MA,NSCN) = NC

                  ENDDO
  670           CONTINUE



                NLU(NMD+1) = NC+1

              ENDIF
  690       CONTINUE
!
!---        Solute transport equations  ---
!
            MA = 1
!
!---        Node  ---
!
            NCC = NCC+1




            MLUC(NCC) = NP

            KLUC(NP,MA) = NCC

            MA = MA + 1
!
!---        Bottom node neighbors  ---
!
            DO 702 MX = 1,4
              NB = ICM(MX,1,N)
              IF( NB.EQ.0 ) EXIT
              NB = IXP(NB)
              NCC = NCC+1




              MLUC(NCC) = NB

              KLUC(NP,MA) = NCC

              MA = MA + 1
  702       CONTINUE
!
!---        South node neighbors  ---
!
            DO 712 MX = 1,4
              NS = ICM(MX,2,N)
              IF( NS.EQ.0 ) EXIT
              NS = IXP(NS)
              NCC = NCC+1




              MLUC(NCC) = NS

              KLUC(NP,MA) = NCC

              MA = MA + 1
  712       CONTINUE
!
!---        West node neighbors  ---
!
            DO 722 MX = 1,4
              NW = ICM(MX,3,N)
              IF( NW.EQ.0 ) EXIT
              NW = IXP(NW)
              NCC = NCC+1




              MLUC(NCC) = NW

              KLUC(NP,MA) = NCC

              MA = MA + 1
  722       CONTINUE
!
!---        East node neighbors  ---
!
            DO 732 MX = 1,4
              NE = ICM(MX,4,N)
              IF( NE.EQ.0 ) EXIT
              NE = IXP(NE)
              NCC = NCC+1




              MLUC(NCC) = NE

              KLUC(NP,MA) = NCC

              MA = MA + 1
  732       CONTINUE
!
!---        North node neighbors  ---
!
            DO 742 MX = 1,4
              NN = ICM(MX,5,N)
              IF( NN.EQ.0 ) EXIT
              NN = IXP(NN)
              NCC = NCC+1




              MLUC(NCC) = NN

              KLUC(NP,MA) = NCC

              MA = MA + 1
  742       CONTINUE
!
!---        Top node neighbors  ---
!
            DO 752 MX = 1,4
              NT = ICM(MX,6,N)
              IF( NT.EQ.0 ) EXIT
              NT = IXP(NT)
              NCC = NCC+1




              MLUC(NCC) = NT

              KLUC(NP,MA) = NCC

              MA = MA + 1
  752       CONTINUE




            NLUC(NP+1) = NCC+1


  800     CONTINUE
          MKC = NC
          MKT = NCC
!
!---    Y-Z Plane yields the lowest band width.
!       Load Jacobian matrix in the increment order J,K,I  ---
!
        ELSEIF( MHBC_JKI.LE.MHBC_KIJ ) THEN
          NC = 0
          NCC = 0




          NLU(1) = 1
          NLUC(1) = 1

          DO 1800 I = 1,IFLD
          DO 1800 K = 1,KFLD
          DO 1800 J = 1,JFLD
            N = ND(I,J,K)
            IF( IXP(N).EQ.0 ) CYCLE
            NP = IXP(N)
            DO 1660 L = 1,ISVC
              NMD = IM(L,NP)
              MA = 0
!
!---          Node  ---
!
              DO 1500 M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM(M,NP)

                KLU(NMD,M+MA) = NC

 1500         CONTINUE
              MA = MA + ISVC
!
!---          Bottom node neighbors  ---
!
              DO 1512 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                DO 1510 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NB)

                  KLU(NMD,M+MA) = NC

 1510           CONTINUE
                MA = MA + ISVC
 1512         CONTINUE
!
!---          South node neighbors  ---
!
              DO 1522 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                DO 1520 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NS)

                  KLU(NMD,M+MA) = NC

 1520           CONTINUE
                MA = MA + ISVC
 1522         CONTINUE
!
!---          West node neighbors  ---
!
              DO 1532 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                DO 1530 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NW)

                  KLU(NMD,M+MA) = NC

 1530           CONTINUE
                MA = MA + ISVC
 1532         CONTINUE
!
!---          East node neighbors  ---
!
              DO 1542 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                DO 1540 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NE)

                  KLU(NMD,M+MA) = NC

 1540           CONTINUE
                MA = MA + ISVC
 1542         CONTINUE
!
!---          North node neighbors  ---
!
              DO 1552 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                DO 1550 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NN)

                  KLU(NMD,M+MA) = NC

 1550           CONTINUE
                MA = MA + ISVC
 1552         CONTINUE
!
!---          Top node neighbors  ---
!
              DO 1562 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                DO 1560 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NT)

                  KLU(NMD,M+MA) = NC

 1560           CONTINUE
                MA = MA + ISVC
 1562         CONTINUE
!
!---          Loop over surface cover nodes  ---
!
              DO 1650 NSCN = 1,NSFCN
!
!---            Node is adjacent to a surface cover node, include 
!               bare and vegetaged surface cover equations in 
!               nodal equations  ---
!
                IF( ICM_SFC(1,NSCN).EQ.N ) THEN
                  MA = 13 + 2*ISVC + 3*ISVC*NSFCC + 5*(L-1)
                  DO M = 1,5
                    NC = NC + 1




                    MLU(NC) = JM_SFC(M,NSCN)

                    KLU_SFC(M+MA,NSCN) = NC

                  ENDDO
              ENDIF
 1650         CONTINUE



              NLU(NMD+1) = NC+1

 1660       CONTINUE
!
!---        Bare and vegetated surface cover equations, 
!           loop over surface cover nodes  ---
!
            DO 1690 NSCN = 1,NSFCN
!
!---          Node is adjacent to surface cover node,
!             include bare and vegetated surface cover equations  ---
!
              IF( ICM_SFC(1,NSCN).EQ.N ) THEN
!
!---            Bare surface cover equations  ---
!
                DO L = 1,2
                  MA = (L-1)*2
                  DO M = 1,2
                    NC = NC+1




                    MLU(NC) = JM_SFC(M,NSCN)

                    KLU_SFC(M+MA,NSCN) = NC

                  ENDDO
                ENDDO
!
!---            Vegetated surface cover equations  ---
!
                DO L = 1,3
                  MA = 4 + (L-1)*3
                  DO M = 1,3
                    NC = NC+1




                    MLU(NC) = JM_SFC(M+2,NSCN)

                    KLU_SFC(M+MA,NSCN) = NC

                  ENDDO
                ENDDO
!
!---            Include nodal equations for nodes adjacent to 
!               the surface cover node in surface cover equations  ---
!
                NPX = IXP(ICM_SFC(1,NSCN))
                MA = 13
!
!---            Loop over number of field node unknowns  ---
!
                DO 1670 L = 1,ISVC
                  MA = MA + (L-1)*ISVC
!
!---              Loop over number of bare and vegetated 
!                 surface cover node unknowns  ---
!
                  DO M = 1,5
                    NC = NC+1




                    MLU(NC) = IM(M,NPX)

                    KLU_SFC(M+MA,NSCN) = NC

                  ENDDO
 1670           CONTINUE



                NLU(NMD+1) = NC+1

              ENDIF
 1690       CONTINUE
!
!---        Solute transport equations  ---
!
            MA = 1
!
!---        Node  ---
!
            NCC = NCC+1




            MLUC(NCC) = NP

            KLUC(NP,MA) = NCC

            MA = MA + 1
!
!---        Bottom node neighbors  ---
!
            DO 1702 MX = 1,4
              NB = ICM(MX,1,N)
              IF( NB.EQ.0 ) EXIT
              NB = IXP(NB)
              NCC = NCC+1




              MLUC(NCC) = NB

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1702       CONTINUE
!
!---        South node neighbors  ---
!
            DO 1712 MX = 1,4
              NS = ICM(MX,2,N)
              IF( NS.EQ.0 ) EXIT
              NS = IXP(NS)
              NCC = NCC+1




              MLUC(NCC) = NS

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1712       CONTINUE
!
!---        West node neighbors  ---
!
            DO 1722 MX = 1,4
              NW = ICM(MX,3,N)
              IF( NW.EQ.0 ) EXIT
              NW = IXP(NW)
              NCC = NCC+1




              MLUC(NCC) = NW

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1722       CONTINUE
!
!---        East node neighbors  ---
!
            DO 1732 MX = 1,4
              NE = ICM(MX,4,N)
              IF( NE.EQ.0 ) EXIT
              NE = IXP(NE)
              NCC = NCC+1




              MLUC(NCC) = NE

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1732       CONTINUE
!
!---        North node neighbors  ---
!
            DO 1742 MX = 1,4
              NN = ICM(MX,5,N)
              IF( NN.EQ.0 ) EXIT
              NN = IXP(NN)
              NCC = NCC+1




              MLUC(NCC) = NN

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1742       CONTINUE
!
!---        Top node neighbors  ---
!
            DO 1752 MX = 1,4
              NT = ICM(MX,6,N)
              IF( NT.EQ.0 ) EXIT
              NT = IXP(NT)
              NCC = NCC+1




              MLUC(NCC) = NT

              KLUC(NP,MA) = NCC

              MA = MA + 1
 1752       CONTINUE




            NLUC(NP+1) = NCC+1


 1800     CONTINUE
          MKC = NC
          MKT = NCC
!
!---    Z-X Plane yields the lowest band width.
!       Load Jacobian matrix in the increment order K,I,J  ---
!
        ELSE
          NC = 0
          NCC = 0




          NLU(1) = 1
          NLUC(1) = 1

          DO 2800 J = 1,JFLD
          DO 2800 I = 1,IFLD
          DO 2800 K = 1,KFLD
            N = ND(I,J,K)
            IF( IXP(N).EQ.0 ) CYCLE
            NP = IXP(N)
            DO 2660 L = 1,ISVC
              NMD = IM(L,NP)
              MA = 0
!
!---          Node  ---
!
              DO 2500 M = 1,ISVC
                NC = NC+1




                MLU(NC) = IM(M,NP)

                KLU(NMD,M+MA) = NC

 2500         CONTINUE
              MA = MA + ISVC
!
!---          Bottom node neighbors  ---
!
              DO 2512 MX = 1,4
                NB = ICM(MX,1,N)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                DO 2510 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NB)

                  KLU(NMD,M+MA) = NC

 2510           CONTINUE
                MA = MA + ISVC
 2512         CONTINUE
!
!---          South node neighbors  ---
!
              DO 2522 MX = 1,4
                NS = ICM(MX,2,N)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                DO 2520 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NS)

                  KLU(NMD,M+MA) = NC

 2520           CONTINUE
                MA = MA + ISVC
 2522         CONTINUE
!
!---          West node neighbors  ---
!
              DO 2532 MX = 1,4
                NW = ICM(MX,3,N)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                DO 2530 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NW)

                  KLU(NMD,M+MA) = NC

 2530           CONTINUE
                MA = MA + ISVC
 2532         CONTINUE
!
!---          East node neighbors  ---
!
              DO 2542 MX = 1,4
                NE = ICM(MX,4,N)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                DO 2540 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NE)

                  KLU(NMD,M+MA) = NC

 2540           CONTINUE
                MA = MA + ISVC
 2542         CONTINUE
!
!---          North node neighbors  ---
!
              DO 2552 MX = 1,4
                NN = ICM(MX,5,N)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                DO 2550 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NN)

                  KLU(NMD,M+MA) = NC

 2550           CONTINUE
                MA = MA + ISVC
 2552         CONTINUE
!
!---          Top node neighbors  ---
!
              DO 2562 MX = 1,4
                NT = ICM(MX,6,N)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                DO 2560 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NT)

                  KLU(NMD,M+MA) = NC

 2560           CONTINUE
                MA = MA + ISVC
 2562         CONTINUE
!
!---          Loop over surface cover nodes  ---
!
              DO 2650 NSCN = 1,NSFCN
!
!---            Node is adjacent to a surface cover node, include 
!               bare and vegetaged surface cover equations in 
!               nodal equations  ---
!
                IF( ICM_SFC(1,NSCN).EQ.N ) THEN
                  MA = 13 + 2*ISVC + 3*ISVC*NSFCC + 5*(L-1)
                  DO M = 1,5
                    NC = NC + 1




                    MLU(NC) = JM_SFC(M,NSCN)

                    KLU_SFC(M+MA,NSCN) = NC

                  ENDDO
                ENDIF
 2650         CONTINUE



              NLU(NMD+1) = NC+1

 2660       CONTINUE
!
!---        Bare and vegetated surface cover equations, 
!           loop over surface cover nodes  ---
!
            DO 2690 NSCN = 1,NSFCN
!
!---          Node is adjacent to surface cover node,
!             include bare and vegetated surface cover equations  ---
!
              IF( ICM_SFC(1,NSCN).EQ.N ) THEN
!
!---            Bare surface cover equations  ---
!
                DO L = 1,2
                  MA = (L-1)*2
                  DO M = 1,2
                    NC = NC+1




                    MLU(NC) = JM_SFC(M,NSCN)

                    KLU_SFC(M+MA,NSCN) = NC

                  ENDDO
                ENDDO
!
!---            Vegetated surface cover equations  ---
!
                DO L = 1,3
                  MA = 4 + (L-1)*3
                  DO M = 1,3
                    NC = NC+1




                    MLU(NC) = JM_SFC(M+2,NSCN)

                    KLU_SFC(M+MA,NSCN) = NC

                  ENDDO
                ENDDO
!
!---            Include nodal equations for nodes adjacent to 
!               the surface cover node in surface cover equations  ---
!
                NPX = IXP(ICM_SFC(1,NSCN))
                MA = 13
!
!---            Loop over number of field node unknowns  ---
!
                DO 2670 L = 1,ISVC
                  MA = MA + (L-1)*ISVC
!
!---              Loop over number of bare and vegetated 
!                 surface cover node unknowns  ---
!
                  DO M = 1,5
                    NC = NC+1




                    MLU(NC) = IM(M,NPX)

                    KLU_SFC(M+MA,NSCN) = NC

                  ENDDO
 2670           CONTINUE



                NLU(NMD+1) = NC+1

              ENDIF
 2690       CONTINUE
!
!---        Solute transport equations  ---
!
            MA = 1
!
!---        Node  ---
!
            NCC = NCC+1




            MLUC(NCC) = NP

            KLUC(NP,MA) = NCC

            MA = MA + 1
!
!---        Bottom node neighbors  ---
!
            DO 2702 MX = 1,4
              NB = ICM(MX,1,N)
              IF( NB.EQ.0 ) EXIT
              NB = IXP(NB)
              NCC = NCC+1




              MLUC(NCC) = NB

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2702       CONTINUE
!
!---        South node neighbors  ---
!
            DO 2712 MX = 1,4
              NS = ICM(MX,2,N)
              IF( NS.EQ.0 ) EXIT
              NS = IXP(NS)
              NCC = NCC+1




              MLUC(NCC) = NS

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2712       CONTINUE
!
!---        West node neighbors  ---
!
            DO 2722 MX = 1,4
              NW = ICM(MX,3,N)
              IF( NW.EQ.0 ) EXIT
              NW = IXP(NW)
              NCC = NCC+1




              MLUC(NCC) = NW

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2722       CONTINUE
!
!---        East node neighbors  ---
!
            DO 2732 MX = 1,4
              NE = ICM(MX,4,N)
              IF( NE.EQ.0 ) EXIT
              NE = IXP(NE)
              NCC = NCC+1




              MLUC(NCC) = NE

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2732       CONTINUE
!
!---        North node neighbors  ---
!
            DO 2742 MX = 1,4
              NN = ICM(MX,5,N)
              IF( NN.EQ.0 ) EXIT
              NN = IXP(NN)
              NCC = NCC+1




              MLUC(NCC) = NN

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2742       CONTINUE
!
!---        Top node neighbors  ---
!
            DO 2752 MX = 1,4
              NT = ICM(MX,6,N)
              IF( NT.EQ.0 ) EXIT
              NT = IXP(NT)
              NCC = NCC+1




              MLUC(NCC) = NT

              KLUC(NP,MA) = NCC

              MA = MA + 1
 2752       CONTINUE




            NLUC(NP+1) = NCC+1


 2800     CONTINUE
          MKC = NC
          MKT = NCC
        ENDIF
      ENDIF
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCBP_SFC group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCBP_WELL
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
!     Configure the Jacobian matrix pointer arrays for coupled wells
!     for STOMP-WA and STOMP-WO.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 April 2011.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE WELL_CL
      USE SOLTN
      USE PTZR
      USE JACOB
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
      SUB_LOG(ISUB_LOG) = '/JCBP_WELL'
      IF( INDEX(SVN_ID(95)(1:1),'$').EQ.0 ) SVN_ID(95) =
     & '$Id: jcbp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Determine maximum well length  ---
!
      KWX = 0
      DO 1010 NWL = 1,NWLS
        KWX = MAX( KWX,(IWLDM(4,NWL)-IWLDM(3,NWL)+1))
 1010 CONTINUE
      IJFLDX = IFLD*JFLD
      JKFLDX = JFLD*(KFLD+KWX)
      KIFLDX = IFLD*(KFLD+KWX)
      NC = 0
!
!---  X-Y Plane yields the lowest band width;
!     Load Jacobian matrix in the increment order I,J,K  ---
!
      IF( IJFLDX.LE.JKFLDX .AND. IJFLDX.LE.KIFLDX ) THEN
        DO 1030 K = 1,KFLD
        DO 1030 J = 1,JFLD
        DO 1030 I = 1,IFLD
          N = ND(I,J,K)
          IF( IXP(N).LE.0 ) GOTO 1030
          NMD = IXP(N)
          ISVCX = ISVC
          IF( IXW(N).NE.0 ) ISVCX = 2*ISVC
          DO 1020 M = 1,ISVCX
            NC = NC+1
            IM(M,NMD) = NC
 1020     CONTINUE
 1030   CONTINUE
!
!---  Y-Z Plane yields the lowest band width;
!     load Jacobian matrix in the increment order J,K,I  ---
!
      ELSEIF( JKFLDX.LE.IJFLDX .AND. JKFLDX.LE.KIFLDX ) THEN
        DO 1050 I = 1,IFLD
        DO 1050 K = 1,KFLD
        DO 1050 J = 1,JFLD
          N = ND(I,J,K)
          IF( IXP(N).LE.0 ) GOTO 1050
          NMD = IXP(N)
          ISVCX = ISVC
          IF( IXW(N).NE.0 ) ISVCX = 2*ISVC
          DO 1040 M = 1,ISVCX
            NC = NC+1
            IM(M,NMD) = NC
 1040     CONTINUE
 1050   CONTINUE
!
!---  Z-X Plane yields the lowest band width;
!     load Jacobian matrix in the increment order K,I,J  ---
!
      ELSEIF( KIFLDX.LE.IJFLDX .AND. KIFLDX.LE.JKFLDX ) THEN
        DO 1070 J = 1,JFLD
        DO 1070 I = 1,IFLD
        DO 1070 K = 1,KFLD
          N = ND(I,J,K)
          IF( IXP(N).LE.0 ) GOTO 1070
          NMD = IXP(N)
          ISVCX = ISVC
          IF( IXW(N).NE.0 ) ISVCX = 2*ISVC
          DO 1060 M = 1,ISVCX
            NC = NC+1
            IM(M,NMD) = NC
 1060     CONTINUE
 1070   CONTINUE
      ENDIF
!
!---  Determine the matrix half-band widths  ---
!
      MHBC = 0
      MHBT = 0
      DO 1100 K = 1,KFLD
      DO 1100 J = 1,JFLD
      DO 1100 I = 1,IFLD
        N = ND(I,J,K)
        IF( IXP(N).LE.0 ) GOTO 1100
        NP = IXP(N)
        IF( K.GT.1 ) THEN
          IF( IXP(N-IJFLD).GT.0 ) THEN
            NB = IXP(N-IJFLD)
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &        ABS(IM(ISVC,NP)-IM(1,NB)))
            MHBT = MAX(MHBT,ABS(NP-NB))
          ENDIF
        ENDIF
        IF( J.GT.1 ) THEN
          IF( IXP(N-IFLD).GT.0 ) THEN
            NS = IXP(N-IFLD)
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &        ABS(IM(ISVC,NP)-IM(1,NS)))
            MHBT = MAX(MHBT,ABS(NP-NS))
          ENDIF
        ENDIF
        IF( I.GT.1 ) THEN
          IF( IXP(N-1).GT.0 ) THEN
            NW = IXP(N-1)
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &        ABS(IM(ISVC,NP)-IM(1,NW)))
            MHBT = MAX(MHBT,ABS(NP-NW))
          ENDIF
        ENDIF
        IF( I.LT.IFLD ) THEN
          IF( IXP(N+1).GT.0 ) THEN
            NE = IXP(N+1)
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &        ABS(IM(ISVC,NP)-IM(1,NE)))
            MHBT = MAX(MHBT,ABS(NP-NE))
          ENDIF
        ENDIF
        IF( J.LT.JFLD ) THEN
          IF( IXP(N+IFLD).GT.0 ) THEN
            NN = IXP(N+IFLD)
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &         ABS(IM(ISVC,NP)-IM(1,NN)))
            MHBT = MAX(MHBT,ABS(NP-NN))
          ENDIF
        ENDIF
        IF( K.LT.KFLD ) THEN
          IF( IXP(N+IJFLD).GT.0 ) THEN
            NT = IXP(N+IJFLD)
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &        ABS(IM(ISVC,NP)-IM(1,NT)))
            MHBT = MAX(MHBT,ABS(NP-NT))
          ENDIF
        ENDIF
 1100 CONTINUE
      MLC = MHBC
      MLT = ISVT*MHBT + ISVT - 1
      MUC = MHBC
      MUT = ISVT*MHBT + ISVT - 1
      MDC = MLC + MUC + 1
      MDT = MLT + MUT + 1
!
!---  SPLIB .or. Lis .or. PETSc Solver  ---
!
      IF( ILES.EQ.3 .OR. ILES.EQ.4 .OR. ILES.EQ.5 ) THEN
!
!---  X-Y Plane yields the lowest band width.
!     Load Jacobian matrix in the increment order I,J,K  ---
!
        IF( IJFLDX.LE.JKFLDX .AND. IJFLDX.LE.KIFLDX ) THEN
          NC = 0
          NCC = 0




          NLU(1) = NC+1
          NLUC(1) = NCC+1

          DO 2000 K = 1,KFLD
          DO 2000 J = 1,JFLD
          DO 2000 I = 1,IFLD
            N = ND(I,J,K)
            NP = IXP(N)
            IF( NP.LE.0 ) GOTO 2000
            ISVCX = ISVC
            IF( IXW(N).NE.0 ) ISVCX = 2*ISVC
            DO 1900 L = 1,ISVCX
              NMD = IM(L,NP)
!
!---          Bottom node  ---
!
              IF( K.GT.1 ) THEN
                NB = N-IJFLD
                NB = IXP(NB)
                IF( NB.LE.0 ) GOTO 1240
!
!---            Well node  ---
!
                IF( IXW(N).NE.0 ) THEN
!
!---              Field equations  ---
!
                  IF( L.LE.ISVC ) THEN
                    DO 1200 M = 1,ISVC
                      NC = NC+1




                      MLU(NC) = IM(M,NB)

                      KLU(NMD,M) = NC

 1200               CONTINUE
                  ENDIF
!
!---              Well equations  ---
!
                  IF( IXW(N-IJFLD).NE.0 ) THEN
                    IF( L.GT.ISVC ) THEN
                      DO 1210 M = ISVC+1,2*ISVC
                        NC = NC+1




                        MLU(NC) = IM(M,NB)

                        KLU(NMD,M) = NC

 1210                 CONTINUE
                    ENDIF
                  ENDIF
!
!---            Non-well node  ---
!
                ELSE
!
!---              Field equations  ---
!
                  DO 1220 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NB)

                    KLU(NMD,M) = NC

 1220             CONTINUE
                ENDIF
              ENDIF
 1240       CONTINUE
!
!---        South node  ---
!
              IF( J.GT.1 ) THEN
                NS = N-IFLD
                NS = IXP(NS)
                IF( NS.LE.0 ) GOTO 1260
                MA = ISVC
                IF( IXW(N).NE.0 ) MA = 2*ISVC
!
!---            Field equations  ---
!
                IF( L.LE.ISVC ) THEN
                  DO 1250 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NS)

                    KLU(NMD,M+MA) = NC

 1250             CONTINUE
                ENDIF
              ENDIF
 1260         CONTINUE
!
!---          West node  ---
!
              IF( I.GT.1 ) THEN
                NW = N-1
                NW = IXP(NW)
                IF( NW.LE.0 ) GOTO 1280
                MA = 2*ISVC
                IF( IXW(N).NE.0 ) MA = 3*ISVC
!
!---            Field equations  ---
!
                IF( L.LE.ISVC ) THEN
                  DO 1270 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NW)

                    KLU(NMD,M+MA) = NC

 1270             CONTINUE
                ENDIF
              ENDIF
 1280         CONTINUE
!
!---          Node  ---
!
              MA = 3*ISVC
              IF( IXW(N).NE.0 ) MA = 4*ISVC
!
!---          Well screen node  ---
!
              IF( IXW(N).LT.0 ) THEN
!
!---            Field equations  ---
!
                DO 1290 M = 1,2*ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NP)

                  KLU(NMD,M+MA) = NC

 1290           CONTINUE
!
!---          Well casing node  ---
!
              ELSEIF( IXW(N).GT.0 ) THEN
!
!---            Field equations  ---
!
                IF( L.LE.ISVC ) THEN
                  DO 1300 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NP)

                    KLU(NMD,M+MA) = NC

 1300             CONTINUE
                ENDIF
!
!---            Well equations  ---
!
                IF( L.GT.ISVC ) THEN
                  DO 1310 M = ISVC+1,2*ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NP)

                    KLU(NMD,M+MA) = NC

 1310             CONTINUE
                ENDIF
!
!---          Non-well node  ---
!
              ELSE
!
!---            Field equations  ---
!
                DO 1320 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NP)

                  KLU(NMD,M+MA) = NC

 1320           CONTINUE
              ENDIF
!
!---          East node  ---
!
              IF( I.LT.IFLD ) THEN
                NE = N+1
                NE = IXP(NE)
                IF( NE.LE.0 ) GOTO 1340
                MA = 4*ISVC
                IF( IXW(N).NE.0 ) MA = 6*ISVC
!
!---            Field equations  ---
!
                IF( L.LE.ISVC ) THEN
                  DO 1330 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NE)

                    KLU(NMD,M+MA) = NC

 1330             CONTINUE
                ENDIF
              ENDIF
 1340         CONTINUE
!
!---          North node  ---
!
              IF( J.LT.JFLD ) THEN
                NN = N+IFLD
                NN = IXP(NN)
                IF( NN.LE.0 ) GOTO 1360
                MA = 5*ISVC
                IF( IXW(N).NE.0 ) MA = 7*ISVC
!
!---            Field equations  ---
!
                IF( L.LE.ISVC ) THEN
                  DO 1350 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NN)

                    KLU(NMD,M+MA) = NC

 1350             CONTINUE
                ENDIF
              ENDIF
 1360         CONTINUE
!
!---          Top node  ---
!
              IF( K.LT.KFLD ) THEN
                NT = N+IJFLD
                NT = IXP(NT)
                IF( NT.LE.0 ) GOTO 1400
                MA = 6*ISVC
                IF( IXW(N).NE.0 ) MA = 8*ISVC
!
!---            Well screen node  ---
!
                IF( IXW(N).NE.0 ) THEN
!
!---              Field equations  ---
!
                  IF( L.LE.ISVC ) THEN
                    DO 1370 M = 1,ISVC
                      NC = NC+1




                      MLU(NC) = IM(M,NT)

                      KLU(NMD,M+MA) = NC

 1370               CONTINUE
                  ENDIF
!
!---              Well equations  ---
!
                  IF( IXW(N+IJFLD).NE.0 ) THEN
                    IF( L.GT.ISVC ) THEN
                      DO 1380 M = ISVC+1,2*ISVC
                        NC = NC+1




                        MLU(NC) = IM(M,NT)

                        KLU(NMD,M+MA) = NC

 1380                 CONTINUE
                    ENDIF
                  ENDIF
!
!---            Non-well node  ---
!
                ELSE
!
!---              Field equations  ---
!
                  IF( L.LE.ISVC ) THEN
                    DO 1390 M = 1,ISVC
                      NC = NC+1




                      MLU(NC) = IM(M,NT)

                      KLU(NMD,M+MA) = NC

 1390               CONTINUE
                  ENDIF
                ENDIF
              ENDIF
 1400         CONTINUE



              NLU(NMD+1) = NC+1

 1900       CONTINUE
!
!---        Solute transport equations  ---
!
            IF( K.GT.1 ) THEN
              NB = N - IJFLD
              NB = IXP(NB)
              IF( NB.LE.0 ) GOTO 1910
              NCC = NCC+1




              MLUC(NCC) = NB

              KLUC(NP,1) = NCC

            ENDIF
 1910       CONTINUE
            IF( J.GT.1 ) THEN
              NS = N - IFLD
              NS = IXP(NS)
              IF( NS.LE.0 ) GOTO 1920
              NCC = NCC+1




              MLUC(NCC) = NS

              KLUC(NP,2) = NCC

            ENDIF
 1920       CONTINUE
            IF( I.GT.1 ) THEN
              NW = N - 1
              NW = IXP(NW)
              IF( NW.LE.0 ) GOTO 1930
              NCC = NCC+1




              MLUC(NCC) = NW

              KLUC(NP,3) = NCC

            ENDIF
 1930       CONTINUE
            NCC = NCC+1




            MLUC(NCC) = NP

            KLUC(NP,4) = NCC

            IF( I.LT.IFLD ) THEN
              NE = N + 1
              NE = IXP(NE)
              IF( NE.LE.0 ) GOTO 1940
              NCC = NCC+1




              MLUC(NCC) = NE

              KLUC(NP,5) = NCC

            ENDIF
 1940       CONTINUE
            IF( J.LT.JFLD ) THEN
              NN = N + IFLD
              NN = IXP(NN)
              IF( NN.LE.0 ) GOTO 1950
              NCC = NCC+1




              MLUC(NCC) = NN

              KLUC(NP,6) = NCC

            ENDIF
 1950       CONTINUE
            IF( K.LT.KFLD ) THEN
              NT = N + IJFLD
              NT = IXP(NT)
              IF( NT.LE.0 ) GOTO 1960
              NCC = NCC+1




              MLUC(NCC) = NT

              KLUC(NP,7) = NCC

            ENDIF
 1960       CONTINUE




            NLUC(NP+1) = NCC+1


 2000     CONTINUE
          MKC = NC
          MKT = NCC
!
!---  Y-Z Plane yields the lowest band width.
!     Load Jacobian matrix in the increment order J,K,I  ---
!
        ELSEIF( JKFLDX.LE.IJFLDX .AND. JKFLDX.LE.KIFLDX ) THEN
          NC = 0
          NCC = 0




          NLU(1) = NC+1
          NLUC(1) = NCC+1

          DO 4000 I = 1,IFLD
          DO 4000 K = 1,KFLD
          DO 4000 J = 1,JFLD
            N = ND(I,J,K)
            NP = IXP(N)
            IF( NP.LE.0 ) GOTO 4000
            ISVCX = ISVC
            IF( IXW(N).NE.0 ) ISVCX = 2*ISVC
            DO 3900 L = 1,ISVCX
              NMD = IM(L,NP)
!
!---          Bottom node  ---
!
              IF( K.GT.1 ) THEN
                NB = N-IJFLD
                NB = IXP(NB)
                IF( NB.LE.0 ) GOTO 3240
!
!---            Well node  ---
!
                IF( IXW(N).NE.0 ) THEN
!
!---              Field equations  ---
!
                  IF( L.LE.ISVC ) THEN
                    DO 3200 M = 1,ISVC
                      NC = NC+1




                      MLU(NC) = IM(M,NB)

                      KLU(NMD,M) = NC

 3200               CONTINUE
                  ENDIF
!
!---              Well equations  ---
!
                  IF( IXW(N-IJFLD).NE.0 ) THEN
                    IF( L.GT.ISVC ) THEN
                      DO 3210 M = ISVC+1,2*ISVC
                        NC = NC+1




                        MLU(NC) = IM(M,NB)

                        KLU(NMD,M) = NC

 3210                 CONTINUE
                    ENDIF
                  ENDIF
!
!---            Non-well node  ---
!
                ELSE
!
!---              Field equations  ---
!
                  DO 3220 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NB)

                    KLU(NMD,M) = NC

 3220             CONTINUE
                ENDIF
              ENDIF
 3240       CONTINUE
!
!---        South node  ---
!
              IF( J.GT.1 ) THEN
                NS = N-IFLD
                NS = IXP(NS)
                IF( NS.LE.0 ) GOTO 3260
                MA = ISVC
                IF( IXW(N).NE.0 ) MA = 2*ISVC
!
!---            Field equations  ---
!
                IF( L.LE.ISVC ) THEN
                  DO 3250 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NS)

                    KLU(NMD,M+MA) = NC

 3250             CONTINUE
                ENDIF
              ENDIF
 3260         CONTINUE
!
!---          West node  ---
!
              IF( I.GT.1 ) THEN
                NW = N-1
                NW = IXP(NW)
                IF( NW.LE.0 ) GOTO 3280
                MA = 2*ISVC
                IF( IXW(N).NE.0 ) MA = 3*ISVC
!
!---            Field equations  ---
!
                IF( L.LE.ISVC ) THEN
                  DO 3270 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NW)

                    KLU(NMD,M+MA) = NC

 3270             CONTINUE
                ENDIF
              ENDIF
 3280         CONTINUE
!
!---          Node  ---
!
              MA = 3*ISVC
              IF( IXW(N).NE.0 ) MA = 4*ISVC
!
!---          Well screen node  ---
!
              IF( IXW(N).LT.0 ) THEN
!
!---            Field equations  ---
!
                DO 3290 M = 1,2*ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NP)

                  KLU(NMD,M+MA) = NC

 3290           CONTINUE
!
!---          Well casing node  ---
!
              ELSEIF( IXW(N).GT.0 ) THEN
!
!---            Field equations  ---
!
                IF( L.LE.ISVC ) THEN
                  DO 3300 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NP)

                    KLU(NMD,M+MA) = NC

 3300             CONTINUE
                ENDIF
!
!---            Well equations  ---
!
                IF( L.GT.ISVC ) THEN
                  DO 3310 M = ISVC+1,2*ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NP)

                    KLU(NMD,M+MA) = NC

 3310             CONTINUE
                ENDIF
!
!---          Non-well node  ---
!
              ELSE
!
!---            Field equations  ---
!
                DO 3320 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NP)

                  KLU(NMD,M+MA) = NC

 3320           CONTINUE
              ENDIF
!
!---          East node  ---
!
              IF( I.LT.IFLD ) THEN
                NE = N+1
                NE = IXP(NE)
                IF( NE.LE.0 ) GOTO 3340
                MA = 4*ISVC
                IF( IXW(N).NE.0 ) MA = 6*ISVC
!
!---            Field equations  ---
!
                IF( L.LE.ISVC ) THEN
                  DO 3330 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NE)

                    KLU(NMD,M+MA) = NC

 3330             CONTINUE
                ENDIF
              ENDIF
 3340         CONTINUE
!
!---          North node  ---
!
              IF( J.LT.JFLD ) THEN
                NN = N+IFLD
                NN = IXP(NN)
                IF( NN.LE.0 ) GOTO 3360
                MA = 5*ISVC
                IF( IXW(N).NE.0 ) MA = 7*ISVC
!
!---            Field equations  ---
!
                IF( L.LE.ISVC ) THEN
                  DO 3350 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NN)

                    KLU(NMD,M+MA) = NC

 3350             CONTINUE
                ENDIF
              ENDIF
 3360         CONTINUE
!
!---          Top node  ---
!
              IF( K.LT.KFLD ) THEN
                NT = N+IJFLD
                NT = IXP(NT)
                IF( NT.LE.0 ) GOTO 3400
                MA = 6*ISVC
                IF( IXW(N).NE.0 ) MA = 8*ISVC
!
!---            Well screen node  ---
!
                IF( IXW(N).NE.0 ) THEN
!
!---              Field equations  ---
!
                  IF( L.LE.ISVC ) THEN
                    DO 3370 M = 1,ISVC
                      NC = NC+1




                      MLU(NC) = IM(M,NT)

                      KLU(NMD,M+MA) = NC

 3370               CONTINUE
                  ENDIF
!
!---              Well equations  ---
!
                  IF( IXW(N+IJFLD).NE.0 ) THEN
                    IF( L.GT.ISVC ) THEN
                      DO 3380 M = ISVC+1,2*ISVC
                        NC = NC+1




                        MLU(NC) = IM(M,NT)

                        KLU(NMD,M+MA) = NC

 3380                 CONTINUE
                    ENDIF
                  ENDIF
!
!---            Non-well node  ---
!
                ELSE
!
!---              Field equations  ---
!
                  IF( L.LE.ISVC ) THEN
                    DO 3390 M = 1,ISVC
                      NC = NC+1




                      MLU(NC) = IM(M,NT)

                      KLU(NMD,M+MA) = NC

 3390               CONTINUE
                  ENDIF
                ENDIF
              ENDIF
 3400         CONTINUE



              NLU(NMD+1) = NC+1

 3900       CONTINUE
!
!---        Solute transport equations  ---
!
            IF( K.GT.1 ) THEN
              NB = N - IJFLD
              NB = IXP(NB)
              IF( NB.LE.0 ) GOTO 3910
              NCC = NCC+1




              MLUC(NCC) = NB

              KLUC(NP,1) = NCC

            ENDIF
 3910       CONTINUE
            IF( J.GT.1 ) THEN
              NS = N - IFLD
              NS = IXP(NS)
              IF( NS.LE.0 ) GOTO 3920
              NCC = NCC+1




              MLUC(NCC) = NS

              KLUC(NP,2) = NCC

            ENDIF
 3920       CONTINUE
            IF( I.GT.1 ) THEN
              NW = N - 1
              NW = IXP(NW)
              IF( NW.LE.0 ) GOTO 3930
              NCC = NCC+1




              MLUC(NCC) = NW

              KLUC(NP,3) = NCC

            ENDIF
 3930       CONTINUE
            NCC = NCC+1




            MLUC(NCC) = NP

            KLUC(NP,4) = NCC

            IF( I.LT.IFLD ) THEN
              NE = N + 1
              NE = IXP(NE)
              IF( NE.LE.0 ) GOTO 3940
              NCC = NCC+1




              MLUC(NCC) = NE

              KLUC(NP,5) = NCC

            ENDIF
 3940       CONTINUE
            IF( J.LT.JFLD ) THEN
              NN = N + IFLD
              NN = IXP(NN)
              IF( NN.LE.0 ) GOTO 3950
              NCC = NCC+1




              MLUC(NCC) = NN

              KLUC(NP,6) = NCC

            ENDIF
 3950       CONTINUE
            IF( K.LT.KFLD ) THEN
              NT = N + IJFLD
              NT = IXP(NT)
              IF( NT.LE.0 ) GOTO 3960
              NCC = NCC+1




              MLUC(NCC) = NT

              KLUC(NP,7) = NCC

            ENDIF
 3960       CONTINUE




            NLUC(NP+1) = NCC+1


 4000     CONTINUE
          MKC = NC
          MKT = NCC
!
!---  Z-X Plane yields the lowest band width.
!     Load Jacobian matrix in the increment order K,I,J  ---
!
        ELSEIF( KIFLDX.LE.IJFLDX .AND. KIFLDX.LE.JKFLDX ) THEN
          NC = 0
          NCC = 0




          NLU(1) = NC+1
          NLUC(1) = NCC+1

          DO 6000 J = 1,JFLD
          DO 6000 I = 1,IFLD
          DO 6000 K = 1,KFLD
            N = ND(I,J,K)
            NP = IXP(N)
            IF( NP.LE.0 ) GOTO 6000
            ISVCX = ISVC
            IF( IXW(N).NE.0 ) ISVCX = 2*ISVC
            DO 5900 L = 1,ISVCX
              NMD = IM(L,NP)
!
!---          Bottom node  ---
!
              IF( K.GT.1 ) THEN
                NB = N-IJFLD
                NB = IXP(NB)
                IF( NB.LE.0 ) GOTO 5240
!
!---            Well node  ---
!
                IF( IXW(N).NE.0 ) THEN
!
!---              Field equations  ---
!
                  IF( L.LE.ISVC ) THEN
                    DO 5200 M = 1,ISVC
                      NC = NC+1




                      MLU(NC) = IM(M,NB)

                      KLU(NMD,M) = NC

 5200               CONTINUE
                  ENDIF
!
!---              Well equations  ---
!
                  IF( IXW(N-IJFLD).NE.0 ) THEN
                    IF( L.GT.ISVC ) THEN
                      DO 5210 M = ISVC+1,2*ISVC
                        NC = NC+1




                        MLU(NC) = IM(M,NB)

                        KLU(NMD,M) = NC

 5210                 CONTINUE
                    ENDIF
                  ENDIF
!
!---            Non-well node  ---
!
                ELSE
!
!---              Field equations  ---
!
                  DO 5220 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NB)

                    KLU(NMD,M) = NC

 5220             CONTINUE
                ENDIF
              ENDIF
 5240       CONTINUE
!
!---        South node  ---
!
              IF( J.GT.1 ) THEN
                NS = N-IFLD
                NS = IXP(NS)
                IF( NS.LE.0 ) GOTO 5260
                MA = ISVC
                IF( IXW(N).NE.0 ) MA = 2*ISVC
!
!---            Field equations  ---
!
                IF( L.LE.ISVC ) THEN
                  DO 5250 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NS)

                    KLU(NMD,M+MA) = NC

 5250             CONTINUE
                ENDIF
              ENDIF
 5260         CONTINUE
!
!---          West node  ---
!
              IF( I.GT.1 ) THEN
                NW = N-1
                NW = IXP(NW)
                IF( NW.LE.0 ) GOTO 5280
                MA = 2*ISVC
                IF( IXW(N).NE.0 ) MA = 3*ISVC
!
!---            Field equations  ---
!
                IF( L.LE.ISVC ) THEN
                  DO 5270 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NW)

                    KLU(NMD,M+MA) = NC

 5270             CONTINUE
                ENDIF
              ENDIF
 5280         CONTINUE
!
!---          Node  ---
!
              MA = 3*ISVC
              IF( IXW(N).NE.0 ) MA = 4*ISVC
!
!---          Well screen node  ---
!
              IF( IXW(N).LT.0 ) THEN
!
!---            Field equations  ---
!
                DO 5290 M = 1,2*ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NP)

                  KLU(NMD,M+MA) = NC

 5290           CONTINUE
!
!---          Well casing node  ---
!
              ELSEIF( IXW(N).GT.0 ) THEN
!
!---            Field equations  ---
!
                IF( L.LE.ISVC ) THEN
                  DO 5300 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NP)

                    KLU(NMD,M+MA) = NC

 5300             CONTINUE
                ENDIF
!
!---            Well equations  ---
!
                IF( L.GT.ISVC ) THEN
                  DO 5310 M = ISVC+1,2*ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NP)

                    KLU(NMD,M+MA) = NC

 5310             CONTINUE
                ENDIF
!
!---          Non-well node  ---
!
              ELSE
!
!---            Field equations  ---
!
                DO 5320 M = 1,ISVC
                  NC = NC+1




                  MLU(NC) = IM(M,NP)

                  KLU(NMD,M+MA) = NC

 5320           CONTINUE
              ENDIF
!
!---          East node  ---
!
              IF( I.LT.IFLD ) THEN
                NE = N+1
                NE = IXP(NE)
                IF( NE.LE.0 ) GOTO 5340
                MA = 4*ISVC
                IF( IXW(N).NE.0 ) MA = 6*ISVC
!
!---            Field equations  ---
!
                IF( L.LE.ISVC ) THEN
                  DO 5330 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NE)

                    KLU(NMD,M+MA) = NC

 5330             CONTINUE
                ENDIF
              ENDIF
 5340         CONTINUE
!
!---          North node  ---
!
              IF( J.LT.JFLD ) THEN
                NN = N+IFLD
                NN = IXP(NN)
                IF( NN.LE.0 ) GOTO 5360
                MA = 5*ISVC
                IF( IXW(N).NE.0 ) MA = 7*ISVC
!
!---            Field equations  ---
!
                IF( L.LE.ISVC ) THEN
                  DO 5350 M = 1,ISVC
                    NC = NC+1




                    MLU(NC) = IM(M,NN)

                    KLU(NMD,M+MA) = NC

 5350             CONTINUE
                ENDIF
              ENDIF
 5360         CONTINUE
!
!---          Top node  ---
!
              IF( K.LT.KFLD ) THEN
                NT = N+IJFLD
                NT = IXP(NT)
                IF( NT.LE.0 ) GOTO 5400
                MA = 6*ISVC
                IF( IXW(N).NE.0 ) MA = 8*ISVC
!
!---            Well screen node  ---
!
                IF( IXW(N).NE.0 ) THEN
!
!---              Field equations  ---
!
                  IF( L.LE.ISVC ) THEN
                    DO 5370 M = 1,ISVC
                      NC = NC+1




                      MLU(NC) = IM(M,NT)

                      KLU(NMD,M+MA) = NC

 5370               CONTINUE
                  ENDIF
!
!---              Well equations  ---
!
                  IF( IXW(N+IJFLD).NE.0 ) THEN
                    IF( L.GT.ISVC ) THEN
                      DO 5380 M = ISVC+1,2*ISVC
                        NC = NC+1




                        MLU(NC) = IM(M,NT)

                        KLU(NMD,M+MA) = NC

 5380                 CONTINUE
                    ENDIF
                  ENDIF
!
!---            Non-well node  ---
!
                ELSE
!
!---              Field equations  ---
!
                  IF( L.LE.ISVC ) THEN
                    DO 5390 M = 1,ISVC
                      NC = NC+1




                      MLU(NC) = IM(M,NT)

                      KLU(NMD,M+MA) = NC

 5390               CONTINUE
                  ENDIF
                ENDIF
              ENDIF
 5400         CONTINUE



              NLU(NMD+1) = NC+1

 5900       CONTINUE
!
!---        Solute transport equations  ---
!
            IF( K.GT.1 ) THEN
              NB = N - IJFLD
              NB = IXP(NB)
              IF( NB.LE.0 ) GOTO 5910
              NCC = NCC+1




              MLUC(NCC) = NB

              KLUC(NP,1) = NCC

            ENDIF
 5910       CONTINUE
            IF( J.GT.1 ) THEN
              NS = N - IFLD
              NS = IXP(NS)
              IF( NS.LE.0 ) GOTO 5920
              NCC = NCC+1




              MLUC(NCC) = NS

              KLUC(NP,2) = NCC

            ENDIF
 5920       CONTINUE
            IF( I.GT.1 ) THEN
              NW = N - 1
              NW = IXP(NW)
              IF( NW.LE.0 ) GOTO 5930
              NCC = NCC+1




              MLUC(NCC) = NW

              KLUC(NP,3) = NCC

            ENDIF
 5930       CONTINUE
            NCC = NCC+1




            MLUC(NCC) = NP

            KLUC(NP,4) = NCC

            IF( I.LT.IFLD ) THEN
              NE = N + 1
              NE = IXP(NE)
              IF( NE.LE.0 ) GOTO 5940
              NCC = NCC+1




              MLUC(NCC) = NE

              KLUC(NP,5) = NCC

            ENDIF
 5940       CONTINUE
            IF( J.LT.JFLD ) THEN
              NN = N + IFLD
              NN = IXP(NN)
              IF( NN.LE.0 ) GOTO 5950
              NCC = NCC+1




              MLUC(NCC) = NN

              KLUC(NP,6) = NCC

            ENDIF
 5950       CONTINUE
            IF( K.LT.KFLD ) THEN
              NT = N + IJFLD
              NT = IXP(NT)
              IF( NT.LE.0 ) GOTO 5960
              NCC = NCC+1




              MLUC(NCC) = NT

              KLUC(NP,7) = NCC

            ENDIF
 5960       CONTINUE




            NLUC(NP+1) = NCC+1


 6000     CONTINUE
          MKC = NC
          MKT = NCC
        ENDIF
      ENDIF
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCBP_WELL group  ---
!
      RETURN
      END

