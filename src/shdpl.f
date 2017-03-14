!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SHDPL( N,DPB,DPS,DPW,DPE,DPN,DPT,NSL )
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
!     Calculates hydrodynamic dispersion coefficients for the aqueous
!     phase from phase velocities and user-specified dispersivities.
!
!----------------------Authors-----------------------------------------!
!
!     Written by ML Rockhold, Battelle, PNL, March 1993.
!     Last Modified by MD White, Battelle, PNL, August 23, 1993.
!     $Id: shdpl.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE GRID
      USE FLUXP
      USE FDVP
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
      SUB_LOG(ISUB_LOG) = '/SHDPL'
      IF( INDEX(SVN_ID(193)(1:1),'$').EQ.0 ) SVN_ID(193) =
     & '$Id: shdpl.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      M = 1
      DPB = 0.D+0
      DPS = 0.D+0
      DPW = 0.D+0
      DPE = 0.D+0
      DPN = 0.D+0
      DPT = 0.D+0
      IF( IDISP .EQ. 0 ) GOTO 170

!
!---  Define indexing for grid block surfaces  ---
!
      NW = N-1
      NE = N+1
      NS = N-IFLD
      NN = N+IFLD
      NB = N-IJFLD
      NT = N+IJFLD
      I = ID(N)
      J = JD(N)
      K = KD(N)
!
!---  Bottom face  ---
!
      IF( K .NE. 1 ) THEN
        IF( IXP(NB) .LE. 0 ) GOTO 110
        CALL ADVB( PORD,SL,UL,VL,WL,ULBX,VLBX,WLBX,N,M )
        ULBSQ = ULBX*ULBX
        VLBSQ = VLBX*VLBX
        WLBSQ = WLBX*WLBX
        ZVB = SQRT(ULBSQ+VLBSQ+WLBSQ)
        INDX = 17
        DISPLB = DISPL(IZ(NB))*SMDEF(IZ(NB),NSL)
        DISPLP = DISPL(IZ(N))*SMDEF(IZ(N),NSL)
        DISPTB = DISPT(IZ(NB))*SMDEF(IZ(NB),NSL)
        DISPTP = DISPT(IZ(N))*SMDEF(IZ(N),NSL)
        DPLB = DIFMN(DISPLB,DISPLP,DZGF(NB),DZGF(N),WLBX,INDX)
        DPTB = DIFMN(DISPTB,DISPTP,DZGF(NB),DZGF(N),WLBX,INDX)
        DPB = (DPLB*WLBSQ + DPTB*(VLBSQ+ULBSQ))/(ZVB+SMALL)
      ENDIF
  110 CONTINUE
!
!---  South face  ---
!
      IF( J .NE. 1 ) THEN
        IF( IXP(NS) .LE. 0 ) GOTO 120
        CALL ADVS( PORD,SL,UL,VL,WL,ULSX,VLSX,WLSX,N,M )
        ULSSQ = ULSX*ULSX
        VLSSQ = VLSX*VLSX
        WLSSQ = WLSX*WLSX
        ZVS = SQRT(ULSSQ+VLSSQ+WLSSQ)
        INDX = 17
        DISPLS = DISPL(IZ(NS))*SMDEF(IZ(NS),NSL)
        DISPLP = DISPL(IZ(N))*SMDEF(IZ(N),NSL)
        DISPTS = DISPT(IZ(NS))*SMDEF(IZ(NS),NSL)
        DISPTP = DISPT(IZ(N))*SMDEF(IZ(N),NSL)
        DPLS = DIFMN(DISPLS,DISPLP,DYGF(NS),DYGF(N),VLSX,INDX)
        DPTS = DIFMN(DISPTS,DISPTP,DYGF(NS),DYGF(N),VLSX,INDX)
        DPS = (DPLS*VLSSQ + DPTS*(ULSSQ+WLSSQ))/(ZVS+SMALL)
      ENDIF
  120 CONTINUE
!
!---  West face  ---
!
      IF( I .NE. 1 ) THEN
        IF( IXP(NW) .LE. 0 ) GOTO 130
        CALL ADVW( PORD,SL,UL,VL,WL,ULX,VLX,WLX,N,M )
        ULWSQ = ULX*ULX
        VLWSQ = VLX*VLX
        WLWSQ = WLX*WLX
        ZVW = SQRT(ULWSQ+VLWSQ+WLWSQ)
        INDX = 17
        DISPLW = DISPL(IZ(NW))*SMDEF(IZ(NW),NSL)
        DISPLP = DISPL(IZ(N))*SMDEF(IZ(N),NSL)
        DISPTW = DISPT(IZ(NW))*SMDEF(IZ(NW),NSL)
        DISPTP = DISPT(IZ(N))*SMDEF(IZ(N),NSL)
        DPLW = DIFMN(DISPLW,DISPLP,DXGF(NW),DXGF(N),ULX,INDX)
        DPTW = DIFMN(DISPTW,DISPTP,DXGF(NW),DXGF(N),ULX,INDX)
        DPW = (DPLW*ULWSQ + DPTW*(VLWSQ+WLWSQ))/(ZVW+SMALL)
      ENDIF
  130 CONTINUE

!
!---  East face  ---
!
      IF( I .NE. IFLD ) THEN
        IF( IXP(NE) .LE. 0 ) GOTO 140
        CALL ADVE( PORD,SL,UL,VL,WL,ULEX,VLEX,WLEX,N,M )
        ULESQ = ULEX*ULEX
        VLESQ = VLEX*VLEX
        WLESQ = WLEX*WLEX
        ZVE = SQRT(ULESQ+VLESQ+WLESQ)
        INDX = 17
        DISPLE = DISPL(IZ(NE))*SMDEF(IZ(NE),NSL)
        DISPLP = DISPL(IZ(N))*SMDEF(IZ(N),NSL)
        DISPTE = DISPT(IZ(NE))*SMDEF(IZ(NE),NSL)
        DISPTP = DISPT(IZ(N))*SMDEF(IZ(N),NSL)
        DPLE = DIFMN(DISPLP,DISPLE,DXGF(N),DXGF(NE),ULEX,INDX)
        DPTE = DIFMN(DISPTP,DISPTE,DXGF(N),DXGF(NE),ULEX,INDX)
        DPE = (DPLE*ULESQ + DPTE*(VLESQ+WLESQ))/(ZVE+SMALL)
      ENDIF
  140 CONTINUE
!
!---  North face  ---
!
      IF( J .NE. JFLD ) THEN
        IF( IXP(NN) .LE. 0 ) GOTO 150
        CALL ADVN( PORD,SL,UL,VL,WL,ULNX,VLNX,WLNX,N,M )
        ULNSQ = ULNX*ULNX
        VLNSQ = VLNX*VLNX
        WLNSQ = WLNX*WLNX
        ZVN = SQRT(ULNSQ+VLNSQ+WLNSQ)
        INDX = 17
        DISPLN = DISPL(IZ(NN))*SMDEF(IZ(NN),NSL)
        DISPLP = DISPL(IZ(N))*SMDEF(IZ(N),NSL)
        DISPTN = DISPT(IZ(NN))*SMDEF(IZ(NN),NSL)
        DISPTP = DISPT(IZ(N))*SMDEF(IZ(N),NSL)
        DPLN = DIFMN(DISPLP,DISPLN,DYGF(N),DYGF(NN),VLNX,INDX)
        DPTN = DIFMN(DISPTP,DISPTN,DYGF(N),DYGF(NN),VLNX,INDX)
        DPN = (DPLN*VLNSQ + DPTN*(ULNSQ+WLNSQ))/(ZVN+SMALL)
      ENDIF
  150 CONTINUE
!
!---  Top face  ---
!
      IF( K .NE. KFLD ) THEN
        IF( IXP(NT) .LE. 0 ) GOTO 160
        CALL ADVT( PORD,SL,UL,VL,WL,ULTX,VLTX,WLTX,N,M )
        ULTSQ = ULTX*ULTX
        VLTSQ = VLTX*VLTX
        WLTSQ = WLTX*WLTX
        ZVT = SQRT(ULTSQ+VLTSQ+WLTSQ)
        INDX = 17
        DISPLT = DISPL(IZ(NT))*SMDEF(IZ(NT),NSL)
        DISPLP = DISPL(IZ(N))*SMDEF(IZ(N),NSL)
        DISPTT = DISPT(IZ(NT))*SMDEF(IZ(NT),NSL)
        DISPTP = DISPT(IZ(N))*SMDEF(IZ(N),NSL)
        DPLT = DIFMN(DISPLP,DISPLT,DZGF(N),DZGF(NT),WLTX,INDX)
        DPTT = DIFMN(DISPTP,DISPTT,DZGF(N),DZGF(NT),WLTX,INDX)
        DPT = (DPLT*WLTSQ + DPTT*(VLTSQ+ULTSQ))/(ZVT+SMALL)
      ENDIF
  160 CONTINUE
  170 CONTINUE
!
!---  End of SHDPL group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END




