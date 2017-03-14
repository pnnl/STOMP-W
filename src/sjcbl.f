!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SJCBL( NSL )
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
!     Loads the matrix elements and solution vector for the
!     aqueous-phase convective-dispersive mass transport equation.
!
!     The Jacobian matrix is initially configured assuming zero-flux
!     boundary conditions.  The matrix is then updated for other
!     user-specified boundary conditions.
!
!     Matrix elements are stored in the array ALU.
!     Elements for the right-hand-side are stored in the array BLU.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle's Pacific Northwest Division, 1996.
!     Last Modified by MD White on September 5, 1996.
!     $Id: sjcbl.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE JACOB
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
!----------------------Type Declarations-------------------------------!
!
      INTEGER :: IROW,ICOL
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/SJCBL'
      IF( INDEX(SVN_ID(196)(1:1),'$').EQ.0 ) SVN_ID(196) =
     & '$Id: sjcbl.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Fill matrix elements  ---
!
!$OMP PARALLEL DO
!$OMP&  DEFAULT(NONE)
!$OMP&  SHARED(AFX,AFY,AFZ,ALU,BLU,CHDF,CO,DLU,DT,DTI,DXGF,DXGP,DYGF,
!$OMP&    DYGP,DZGF,DZGP,HLF,ID,IEDL,IFLD,IJFLD,ILES,INBS,ISLC,IXP,IZ,
!$OMP&    JD,JFLD,KD,KFLD,KLUC,MDT,NFLD,NSOLU,NSX,NSY,NSZ,ONE,
!$OMP&    PETSC_OFFSET,PORD,
!$OMP&    RP,SDCL,SL,SMALL,SMDL,T,TABS,TENTH,TMAT,TORL,TSPRF,UC,UL,VC,
!$OMP&    VISL,VISRL,VL,VOL,WC,WL,YL,ZERO)
!$OMP&  PRIVATE(AB,AE,ALB,ALE,ALN,ALS,ALT,ALW,AN,AP,AS,AT,AW,CC,
!$OMP&    CRLB,CRLE,CRLN,CRLS,CRLT,CRLW,DLB,DLE,DLN,DLP,DLS,
!$OMP&    DLT,DLW,DLX,DLY,DLZ,DPLB,DPLE,DPLN,DPLS,DPLT,DPLW,DXF,
!$OMP&    DYF,DYNR,DYR,DYSR,DZF,FCLB,FCLBB,FCLE,FCLEE,FCLN,FCLNN,
!$OMP&    FCLP,FCLS,FCLSS,FCLT,FCLTT,FCLW,FCLWW,FLB,FLE,FLN,FLS,
!$OMP&    FLT,FLW,I,ICOL,IERR,INDX,IROW,IZN,J,K,MA,MCB,MCD,MCE,MCN,
!$OMP&    MCOL,MCP,MCS,MCT,MCW,MRD,MROW,N,NB,NE,NN,NPSL,NPX,NPY,NPZ,
!$OMP&    NQX,NQY,NQZ,NS,NT,NW,R,SC,SDFLB,SDFLE,SDFLN,SDFLP,
!$OMP&    SDFLS,SDFLT,SDFLW,TCOR,THETA,UCX,V1X,V2X,V3X,V4X,VCY,VMCB,
!$OMP&    VMCBX,VMCE,VMCEX,VMCN,VMCNX,VMCP,VMCS,VMCSX,VMCT,VMCTX,
!$OMP&    VMCW,VMCWX,WCZ)
!$OMP&  FIRSTPRIVATE(NSL)
      DO 900 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 900
        I = ID(N)
        J = JD(N)
        K = KD(N)
        NW = N-1
        NE = N+1
        NS = N-IFLD
        NN = N+IFLD
        NB = N-IJFLD
        NT = N+IJFLD
        IZN = IZ(N)
        NPX = NSX(N)
        NPY = NSY(N)
        NPZ = NSZ(N)
        NQX = NSX(N)+1
        IF( INBS(4,N).NE.0 ) NQX = INBS(4,N)
        NQY = NSY(N)+IFLD
        IF( INBS(5,N).NE.0 ) NQY = INBS(5,N)
        NQZ = NSZ(N)+IJFLD
        IF( INBS(6,N).NE.0 ) NQZ = INBS(6,N)
        DYR = RP(I)*DYGF(N)
!
!---    Storage terms  ---
!
        SC = VOL(N)*DTI
        CC = 0.D+0
        MCP = IXP(N)
        MA = 1
!
!---    Banded solver  ---
!
        IF( ILES.EQ.1 ) THEN
          MCD = MCP
          MRD = MDT
          ALU(MRD,MCD) = ALU(MRD,MCD) + SC
!
!---    SPLib solver  ---
!
        ELSEIF( ILES.EQ.3 .OR. ILES.EQ.4 ) THEN
          MCD = KLUC(MCP,MA)
          MA = MA + 1
          DLU(MCD) = DLU(MCD) + SC

        ENDIF
!
!---    Molecular diffusion coefficients at the nodes  ---
!
        TCOR = (T(2,N)+TABS)/TSPRF
        SDFLP = SMDL(NSL)*TCOR*(VISRL/VISL(2,N))
        VMCP = SL(2,N)*PORD(2,N)
        FCLP = YL(N,NSL)/(VMCP+SMALL)
        IF( IEDL(NSL).EQ.2 ) THEN
          DLP = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &      EXP(VMCP*SDCL(3,IZN,NSL))
        ELSEIF( IEDL(NSL).EQ.3 ) THEN
          DLP = TORL(2,N)*VMCP*SMDL(NSL)
        ELSEIF( IEDL(NSL).EQ.4 ) THEN
          DLP = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &      VMCP**SDCL(3,IZN,NSL)
        ELSE
          DLP = TORL(2,N)*VMCP*SDFLP
        ENDIF
!
!---    Hydrodynamic dispersion coefficients at cell faces  ---
!
        CALL SHDPL( N,DPLB,DPLS,DPLW,DPLE,DPLN,DPLT,NSL )
!
!---    Bottom face diffusion and advection terms  ---
!
        IF( K.NE.1 ) THEN
          IF( IXP(NB).EQ.0 .OR. INBS(1,N).NE.0 ) GOTO 110
          TCOR = (T(2,NB)+TABS)/TSPRF
          SDFLB = SMDL(NSL)*TCOR*(VISRL/VISL(2,NB))
          VMCB = SL(2,NB)*PORD(2,NB)
          FCLB = YL(NB,NSL)/(VMCB+SMALL)
          IF( IEDL(NSL).EQ.2 ) THEN
            DLB = SDCL(1,IZ(NB),NSL)*SDCL(2,IZ(NB),NSL)*
     &        EXP(VMCB*SDCL(3,IZ(NB),NSL))
          ELSEIF( IEDL(NSL).EQ.3 ) THEN
            DLB = TORL(2,NB)*VMCB*SMDL(NSL)
          ELSEIF( IEDL(NSL).EQ.4 ) THEN
            DLB = SDCL(1,IZ(NB),NSL)*SDCL(2,IZ(NB),NSL)*
     &        VMCB**SDCL(3,IZ(NB),NSL)
          ELSE
            DLB = TORL(2,NB)*VMCB*SDFLB
          ENDIF
          INDX = 16
          DLZ = DIFMN(DLB,DLP,DZGF(NB),DZGF(N),WL(1,NPZ),INDX)
          DLZ = AFZ(NPZ)*(DLZ+DPLB)/DZGP(NPZ)
          FLB = AFZ(NPZ)*WL(1,NPZ)
          IF( MOD(ISLC(23),10).EQ.1 ) FLB = 0.D+0
          VMCBX = DIFMN(VMCB,VMCP,DZGF(NB),DZGF(N),WL(1,NPZ),INDX)
          CRLB = ABS(WL(1,NPZ))*DT/(DZGP(NPZ)*VMCBX+SMALL)
!
!---      TVD solute transport  ---
!
          IF( ISLC(1).GE.1 ) THEN
            IF( FLB.GE.ZERO .AND. K.GT.2 ) THEN
             IF( IXP(NB-IJFLD).GT.0 ) THEN
              FCLBB = YL(NB-IJFLD,NSL)
     &          /(SL(2,NB-IJFLD)*PORD(2,NB-IJFLD)+SMALL)
              V1X = CO(NB,NSL)*FCLB-CO(NB-IJFLD,NSL)*FCLBB
              V2X = CO(N,NSL)*FCLP-CO(NB,NSL)*FCLB
              V3X = DZGF(N)+DZGF(NB)
              V4X = DZGF(NB)+DZGF(NB-IJFLD)
              R = RLIMIT( V1X,V2X,V3X,V4X )
              THETA = FLIMIT( R,CRLB,ISLC(1) )
              DZF = DZGF(NB)/(DZGF(N)+DZGF(NB))
              WCZ = CO(NB,NSL)*FLB*(1.D+0-THETA*DZF)*FCLB
     &          + CO(N,NSL)*FLB*THETA*DZF*FCLP
              WC(NPZ,NSL) = WC(NPZ,NSL) + WCZ/AFZ(NPZ)
              CC = CC + WCZ
             ELSE
              WCZ = CO(NB,NSL)*FLB*FCLB
              WC(NPZ,NSL) = WC(NPZ,NSL) + WCZ/AFZ(NPZ)
              CC = CC + WCZ
             ENDIF
            ELSEIF( FLB.GE.ZERO .AND. K.EQ.2 ) THEN
              WCZ = CO(NB,NSL)*FLB*FCLB
              WC(NPZ,NSL) = WC(NPZ,NSL) + WCZ/AFZ(NPZ)
              CC = CC + WCZ
            ELSEIF( FLB.LT.ZERO .AND. K.LT.KFLD ) THEN
             IF( IXP(NT).GT.0 ) THEN
              FCLT = YL(NT,NSL)/(SL(2,NT)*PORD(2,NT)+SMALL)
              V1X = CO(N,NSL)*FCLP-CO(NT,NSL)*FCLT
              V2X = CO(NB,NSL)*FCLB-CO(N,NSL)*FCLP
              V3X = DZGF(NB)+DZGF(N)
              V4X = DZGF(N)+DZGF(NT)
              R = RLIMIT( V1X,V2X,V3X,V4X )
              THETA = FLIMIT( R,CRLB,ISLC(1) )
              DZF = DZGF(N)/(DZGF(N)+DZGF(NB))
              WCZ = CO(NB,NSL)*FLB*THETA*DZF*FCLB
     &          + CO(N,NSL)*FLB*(1.D+0-THETA*DZF)*FCLP
              WC(NPZ,NSL) = WC(NPZ,NSL) + WCZ/AFZ(NPZ)
              CC = CC + WCZ
             ELSE
              WCZ = CO(N,NSL)*FLB*FCLP
              WC(NPZ,NSL) = WC(NPZ,NSL) + WCZ/AFZ(NPZ)
              CC = CC + WCZ
             ENDIF
            ELSEIF( FLB.LT.ZERO .AND. K.EQ.KFLD ) THEN
              WCZ = CO(N,NSL)*FLB*FCLP
              WC(NPZ,NSL) = WC(NPZ,NSL) + WCZ/AFZ(NPZ)
              CC = CC + WCZ
            ENDIF
            AB = DLZ*FCLB
            AP = DLZ*FCLP
!
!---      Patankar solute transport  ---
!
          ELSE
            ALB = MAX(FLB,ZERO)
     &        + DLZ*MAX((ONE-(TENTH*ABS(FLB)/(DLZ+SMALL)))**5,ZERO)
            AP = (ALB-FLB)*FCLP
            AB = ALB*FCLB
          ENDIF
          MCB = IXP(NB)
!
!---      Banded solver  ---
!
          IF( ILES.EQ.1 ) THEN
            MCOL = MCB
            MROW = MCP-MCB+MDT
            ALU(MRD,MCD) = ALU(MRD,MCD) + AP
            ALU(MROW,MCOL) = ALU(MROW,MCOL) - AB
!
!---      SPLib solver  ---
!
          ELSEIF( ILES.EQ.3 .OR. ILES.EQ.4 ) THEN
            DLU(MCD) = DLU(MCD) + AP
            MROW = KLUC(MCP,MA)
            MA = MA + 1
            DLU(MROW) = DLU(MROW) - AB

          ENDIF
        ENDIF
  110   CONTINUE
!
!---    South face diffusion and advection terms  ---
!
        IF( J.NE.1 ) THEN
          IF( IXP(NS).EQ.0 .OR. INBS(2,N).NE.0 ) GOTO 120
          DYSR = RP(I)*DYGF(NS)
          TCOR = (T(2,NS)+TABS)/TSPRF
          SDFLS = SMDL(NSL)*TCOR*(VISRL/VISL(2,NS))
          VMCS = SL(2,NS)*PORD(2,NS)
          FCLS = YL(NS,NSL)/(VMCS+SMALL)
          IF( IEDL(NSL).EQ.2 ) THEN
            DLS = SDCL(1,IZ(NS),NSL)*SDCL(2,IZ(NS),NSL)*
     &        EXP(VMCS*SDCL(3,IZ(NS),NSL))
          ELSEIF( IEDL(NSL).EQ.3 ) THEN
            DLS = TORL(2,NS)*VMCS*SMDL(NSL)
          ELSEIF( IEDL(NSL).EQ.4 ) THEN
            DLS = SDCL(1,IZ(NS),NSL)*SDCL(2,IZ(NS),NSL)*
     &        VMCS**SDCL(3,IZ(NS),NSL)
          ELSE
            DLS = TORL(2,NS)*VMCS*SDFLS
          ENDIF
          INDX = 16
          DLY = DIFMN(DLS,DLP,DYSR,DYR,VL(1,NPY),INDX)
          DLY = AFY(NPY)*(DLY+DPLS)/(DYGP(NPY)*RP(I))
          FLS = AFY(NPY)*VL(1,NPY)
          IF( MOD(ISLC(23),10).EQ.1 ) FLS = 0.D+0
          VMCSX = DIFMN(VMCS,VMCP,DYSR,DYR,VL(1,NPY),INDX)
          CRLS = ABS(VL(1,NPY))*DT/(DYGP(NPY)*VMCSX+SMALL)/RP(I)
!
!---      TVD solute transport  ---
!
          IF( ISLC(1).GE.1 ) THEN
            IF( FLS.GE.ZERO .AND. J.GT.2 ) THEN
             IF( IXP(NS-IFLD).GT.0 ) THEN
              FCLSS = YL(NS-IFLD,NSL)
     &          /(SL(2,NS-IFLD)*PORD(2,NS-IFLD)+SMALL)
              V1X = CO(NS,NSL)*FCLS-CO(NS-IFLD,NSL)*FCLSS
              V2X = CO(N,NSL)*FCLP-CO(NS,NSL)*FCLS
              V3X = DYGF(N)+DYGF(NS)
              V4X = DYGF(NS)+DYGF(NS-IFLD)
              R = RLIMIT( V1X,V2X,V3X,V4X )
!              R = ((CO(NS,NSL)*FCLS-CO(NS-IFLD,NSL)*FCLSS)
!     &          /(CO(N,NSL)*FCLP-CO(NS,NSL)*FCLS+EPSL))
!     &          *((DYGF(N)+DYGF(NS))/(DYGF(NS)+DYGF(NS-IFLD)))
              THETA = FLIMIT( R,CRLS,ISLC(1) )
              DYF = DYGF(NS)/(DYGF(N)+DYGF(NS))
              VCY = CO(NS,NSL)*FLS*(1.D+0-THETA*DYF)*FCLS
     &          + CO(N,NSL)*FLS*THETA*DYF*FCLP
              VC(NPY,NSL) = VC(NPY,NSL) + VCY/AFY(NPY)
              CC = CC + VCY
             ELSE
              VCY = CO(NS,NSL)*FLS*FCLS
              VC(NPY,NSL) = VC(NPY,NSL) + VCY/AFY(NPY)
              CC = CC + VCY
             ENDIF
            ELSEIF( FLS.GE.ZERO .AND. J.EQ.2 ) THEN
              VCY = CO(NS,NSL)*FLS*FCLS
              VC(NPY,NSL) = VC(NPY,NSL) + VCY/AFY(NPY)
              CC = CC + VCY
            ELSEIF( FLS.LT.ZERO .AND. J.LT.JFLD ) THEN
             IF( IXP(NN).GT.0 ) THEN
              FCLN = YL(NN,NSL)/(SL(2,NN)*PORD(2,NN)+SMALL)
              V1X = CO(N,NSL)*FCLP-CO(NN,NSL)*FCLN
              V2X = CO(NS,NSL)*FCLS-CO(N,NSL)*FCLP
              V3X = DYGF(NS)+DYGF(N)
              V4X = DYGF(N)+DYGF(NN)
              R = RLIMIT( V1X,V2X,V3X,V4X )
!              R = ((CO(N,NSL)*FCLP-CO(NN,NSL)*FCLN)
!     &          /(CO(NS,NSL)*FCLS-CO(N,NSL)*FCLP+EPSL))
!     &          *((DYGF(NS)+DYGF(N))/(DYGF(N)+DYGF(NN)))
              THETA = FLIMIT( R,CRLS,ISLC(1) )
              DYF = DYGF(N)/(DYGF(N)+DYGF(NS))
              VCY = CO(NS,NSL)*FLS*THETA*DYF*FCLS
     &          + CO(N,NSL)*FLS*(1.D+0-THETA*DYF)*FCLP
              VC(NPY,NSL) = VC(NPY,NSL) + VCY/AFY(NPY)
              CC = CC + VCY
             ELSE
              VCY = CO(N,NSL)*FLS*FCLP
              VC(NPY,NSL) = VC(NPY,NSL) + VCY/AFY(NPY)
              CC = CC + VCY
             ENDIF
            ELSEIF( FLS.LT.ZERO .AND. J.EQ.JFLD ) THEN
              VCY = CO(N,NSL)*FLS*FCLP
              VC(NPY,NSL) = VC(NPY,NSL) + VCY/AFY(NPY)
              CC = CC + VCY
            ENDIF
            AS = DLY*FCLS
            AP = DLY*FCLP
!
!---      Patankar solute transport  ---
!
          ELSE
            ALS = MAX(FLS,ZERO)
     &        + DLY*MAX((ONE-(TENTH*ABS(FLS)/(DLY+SMALL)))**5,ZERO)
            AP = (ALS-FLS)*FCLP
            AS = ALS*FCLS
          ENDIF
          MCS = IXP(NS)
!
!---      Banded solver  ---
!
          IF( ILES.EQ.1 ) THEN
            MCOL = MCS
            MROW = MCP-MCS+MDT
            ALU(MRD,MCD) = ALU(MRD,MCD) + AP
            ALU(MROW,MCOL) = ALU(MROW,MCOL) - AS
!
!---      SPLib solver  ---
!
          ELSEIF( ILES.EQ.3 .OR. ILES.EQ.4 ) THEN
            DLU(MCD) = DLU(MCD) + AP
            MROW = KLUC(MCP,MA)
            MA = MA + 1
            DLU(MROW) = DLU(MROW) - AS

          ENDIF
        ENDIF
  120   CONTINUE
!
!---    West face diffusion and advection terms  ---
!
        IF( I.NE.1 ) THEN
          IF( IXP(NW).EQ.0 .OR. INBS(3,N).NE.0 ) GOTO 130
          TCOR = (T(2,NW)+TABS)/TSPRF
          SDFLW = SMDL(NSL)*TCOR*(VISRL/VISL(2,NW))
          VMCW = SL(2,NW)*PORD(2,NW)
          FCLW = YL(NW,NSL)/(VMCW+SMALL)
          IF( IEDL(NSL).EQ.2 ) THEN
            DLW = SDCL(1,IZ(NW),NSL)*SDCL(2,IZ(NW),NSL)*
     &        EXP(VMCW*SDCL(3,IZ(NW),NSL))
          ELSEIF( IEDL(NSL).EQ.3 ) THEN
            DLW = TORL(2,NW)*VMCW*SMDL(NSL)
          ELSEIF( IEDL(NSL).EQ.4 ) THEN
            DLW = SDCL(1,IZ(NW),NSL)*SDCL(2,IZ(NW),NSL)*
     &        VMCW**SDCL(3,IZ(NW),NSL)
          ELSE
            DLW = TORL(2,NW)*VMCW*SDFLW
          ENDIF
          INDX = 16
          DLX = DIFMN(DLW,DLP,DXGF(NW),DXGF(N),UL(1,NPX),INDX)
          DLX = AFX(NPX)*(DLX+DPLW)/DXGP(NPX)
          FLW = AFX(NPX)*UL(1,NPX)
          IF( MOD(ISLC(23),10).EQ.1 ) FLW = 0.D+0
          VMCWX = DIFMN(VMCW,VMCP,DXGF(NW),DXGF(N),UL(1,NPX),INDX)
          CRLW = ABS(UL(1,NPX))*DT/(DXGP(NPX)*VMCWX+SMALL)
!
!---      TVD solute transport  ---
!
          IF( ISLC(1).GE.1 ) THEN
            IF( FLW.GE.ZERO .AND. I.GT.2 ) THEN
             IF( IXP(NW-1).GT.0 ) THEN
              FCLWW = YL(NW-1,NSL)/(SL(2,NW-1)*PORD(2,NW-1)+SMALL)
              V1X = CO(NW,NSL)*FCLW-CO(NW-1,NSL)*FCLWW
              V2X = CO(N,NSL)*FCLP-CO(NW,NSL)*FCLW
              V3X = DXGF(N)+DXGF(NW)
              V4X = DXGF(NW)+DXGF(NW-1)
              R = RLIMIT( V1X,V2X,V3X,V4X )
!              R = ((CO(NW,NSL)*FCLW-CO(NW-1,NSL)*FCLWW)
!     &          /(CO(N,NSL)*FCLP-CO(NW,NSL)*FCLW+EPSL))
!     &          *((DXGF(N)+DXGF(NW))/(DXGF(NW)+DXGF(NW-1)))
              THETA = FLIMIT( R,CRLW,ISLC(1) )
              DXF = DXGF(NW)/(DXGF(N)+DXGF(NW))
              UCX = CO(NW,NSL)*FLW*(1.D+0-THETA*DXF)*FCLW
     &          + CO(N,NSL)*FLW*THETA*DXF*FCLP
              UC(NPX,NSL) = UC(NPX,NSL) + UCX/AFX(NPX)
              CC = CC + UCX
             ELSE
              UCX = CO(NW,NSL)*FLW*FCLW
              UC(NPX,NSL) = UC(NPX,NSL) + UCX/AFX(NPX)
              CC = CC + UCX
             ENDIF
            ELSEIF( FLW.GE.ZERO .AND. I.EQ.2 ) THEN
              UCX = CO(NW,NSL)*FLW*FCLW
              UC(NPX,NSL) = UC(NPX,NSL) + UCX/AFX(NPX)
              CC = CC + UCX
            ELSEIF( FLW.LT.ZERO .AND. I.LT.IFLD ) THEN
             IF( IXP(NE).GT.0 ) THEN
              FCLE = YL(NE,NSL)/(SL(2,NE)*PORD(2,NE)+SMALL)
              V1X = CO(N,NSL)*FCLP-CO(NE,NSL)*FCLE
              V2X = CO(NW,NSL)*FCLW-CO(N,NSL)*FCLP
              V3X = DXGF(NW)+DXGF(N)
              V4X = DXGF(N)+DXGF(NE)
              R = RLIMIT( V1X,V2X,V3X,V4X )
!              R = ((CO(N,NSL)*FCLP-CO(NE,NSL)*FCLE)
!     &          /(CO(NW,NSL)*FCLW-CO(N,NSL)*FCLP+EPSL))
!     &          *((DXGF(NW)+DXGF(N))/(DXGF(N)+DXGF(NE)))
              THETA = FLIMIT( R,CRLW,ISLC(1) )
              DXF = DXGF(N)/(DXGF(N)+DXGF(NW))
              UCX = CO(NW,NSL)*FLW*THETA*DXF*FCLW
     &          + CO(N,NSL)*FLW*(1.D+0-THETA*DXF)*FCLP
              UC(NPX,NSL) = UC(NPX,NSL) + UCX/AFX(NPX)
              CC = CC + UCX
             ELSE
              UCX = CO(N,NSL)*FLW*FCLP
              UC(NPX,NSL) = UC(NPX,NSL) + UCX/AFX(NPX)
              CC = CC + UCX
             ENDIF
            ELSEIF( FLW.LT.ZERO .AND. I.EQ.IFLD ) THEN
              UCX = CO(N,NSL)*FLW*FCLP
              UC(NPX,NSL) = UC(NPX,NSL) + UCX/AFX(NPX)
              CC = CC + UCX
            ENDIF
            AW = DLX*FCLW
            AP = DLX*FCLP
!
!---      Patankar solute transport  ---
!
          ELSE
            ALW = MAX(FLW,ZERO)
     &        + DLX*MAX((ONE-(TENTH*ABS(FLW)/(DLX+SMALL)))**5,ZERO)
            AP = (ALW-FLW)*FCLP
            AW = ALW*FCLW
          ENDIF
          MCW = IXP(NW)
!
!---      Banded solver  ---
!
          IF( ILES.EQ.1 ) THEN
            MCOL = MCW
            MROW = MCP-MCW+MDT
            ALU(MRD,MCD) = ALU(MRD,MCD) + AP
            ALU(MROW,MCOL) = ALU(MROW,MCOL) - AW
!
!---      SPLib solver  ---
!
          ELSEIF( ILES.EQ.3 .OR. ILES.EQ.4 ) THEN
            DLU(MCD) = DLU(MCD) + AP
            MROW = KLUC(MCP,MA)
            MA = MA + 1
            DLU(MROW) = DLU(MROW) - AW

          ENDIF
        ENDIF
  130   CONTINUE
!
!---    East face diffusion and advection terms  ---
!
        IF( I.NE.IFLD ) THEN
          IF( IXP(NE).EQ.0 .OR. INBS(4,N).NE.0 ) GOTO 140
          TCOR = (T(2,NE)+TABS)/TSPRF
          SDFLE = SMDL(NSL)*TCOR*(VISRL/VISL(2,NE))
          VMCE = SL(2,NE)*PORD(2,NE)
          FCLE = YL(NE,NSL)/(VMCE+SMALL)
          IF( IEDL(NSL).EQ.2 ) THEN
            DLE = SDCL(1,IZ(NE),NSL)*SDCL(2,IZ(NE),NSL)*
     &        EXP(VMCE*SDCL(3,IZ(NE),NSL))
          ELSEIF( IEDL(NSL).EQ.3 ) THEN
            DLE = TORL(2,NE)*VMCE*SMDL(NSL)
          ELSEIF( IEDL(NSL).EQ.4 ) THEN
            DLE = SDCL(1,IZ(NE),NSL)*SDCL(2,IZ(NE),NSL)*
     &        VMCE**SDCL(3,IZ(NE),NSL)
          ELSE
            DLE = TORL(2,NE)*VMCE*SDFLE
          ENDIF
          INDX = 16
          DLX = DIFMN(DLP,DLE,DXGF(N),DXGF(NE),UL(1,NQX),INDX)
          DLX = AFX(NQX)*(DLX+DPLE)/DXGP(NQX)
          FLE = AFX(NQX)*UL(1,NQX)
          IF( MOD(ISLC(23),10).EQ.1 ) FLE = 0.D+0
          VMCEX = DIFMN(VMCP,VMCE,DXGF(N),DXGF(NE),UL(1,NQX),INDX)
          CRLE = ABS(UL(1,NQX))*DT/(DXGP(NQX)*VMCEX+SMALL)
!
!---      TVD solute transport  ---
!
          IF( ISLC(1).GE.1 ) THEN
            IF( FLE.GE.ZERO .AND. I.GT.1 ) THEN
             IF( IXP(NW).GT.0 ) THEN
              FCLW = YL(NW,NSL)/(SL(2,NW)*PORD(2,NW)+SMALL)
              V1X = CO(N,NSL)*FCLP-CO(NW,NSL)*FCLW
              V2X = CO(NE,NSL)*FCLE-CO(N,NSL)*FCLP
              V3X = DXGF(NE)+DXGF(N)
              V4X = DXGF(N)+DXGF(NW)
              R = RLIMIT( V1X,V2X,V3X,V4X )
!              R = ((CO(N,NSL)*FCLP-CO(NW,NSL)*FCLW)
!     &          /(CO(NE,NSL)*FCLE-CO(N,NSL)*FCLP+EPSL))
!     &          *((DXGF(NE)+DXGF(N))/(DXGF(N)+DXGF(NW)))
              THETA = FLIMIT( R,CRLE,ISLC(1) )
              DXF = DXGF(N)/(DXGF(N)+DXGF(NE))
              UCX =  CO(NE,NSL)*FLE*THETA*DXF*FCLE
     &          + CO(N,NSL)*FLE*(1.D+0-THETA*DXF)*FCLP
              CC = CC - UCX
             ELSE
              UCX =  CO(N,NSL)*FLE*FCLP
              CC = CC - UCX
             ENDIF
            ELSEIF( FLE.GE.ZERO .AND. I.EQ.1 ) THEN
              UCX =  CO(N,NSL)*FLE*FCLP
              CC = CC - UCX
            ELSEIF( FLE.LT.ZERO .AND. I.LT.IFLD-1 ) THEN
             IF( IXP(NE+1).GT.0 ) THEN
              FCLEE = YL(NE+1,NSL)/(SL(2,NE+1)*PORD(2,NE+1)+SMALL)
              V1X = CO(NE,NSL)*FCLE-CO(NE+1,NSL)*FCLEE
              V2X = CO(N,NSL)*FCLP-CO(NE,NSL)*FCLE
              V3X = DXGF(N)+DXGF(NE)
              V4X = DXGF(NE)+DXGF(NE+1)
              R = RLIMIT( V1X,V2X,V3X,V4X )
!              R = ((CO(NE,NSL)*FCLE-CO(NE+1,NSL)*FCLEE)
!     &          /(CO(N,NSL)*FCLP-CO(NE,NSL)*FCLE+EPSL))
!     &          *((DXGF(N)+DXGF(NE))/(DXGF(NE)+DXGF(NE+1)))
              THETA = FLIMIT( R,CRLE,ISLC(1) )
              DXF = DXGF(NE)/(DXGF(N)+DXGF(NE))
              UCX = CO(NE,NSL)*FLE*(1.D+0-THETA*DXF)*FCLE
     &          + CO(N,NSL)*FLE*THETA*DXF*FCLP
              CC = CC - UCX
             ELSE
              UCX = CO(NE,NSL)*FLE*FCLE
              CC = CC - UCX
             ENDIF
            ELSEIF( FLE.LT.ZERO .AND. I.EQ.IFLD-1 ) THEN
              UCX = CO(NE,NSL)*FLE*FCLE
              CC = CC - UCX
            ENDIF
            AE = DLX*FCLE
            AP = DLX*FCLP
!
!---      Patankar solute transport  ---
!
          ELSE
            ALE = MAX(-FLE,ZERO)
     &        + DLX*MAX((ONE-(TENTH*ABS(FLE)/(DLX+SMALL)))**5,ZERO)
            AP = (ALE+FLE)*FCLP
            AE = ALE*FCLE
          ENDIF
          MCE = IXP(NE)
!
!---      Banded solver  ---
!
          IF( ILES.EQ.1 ) THEN
            MCOL = MCE
            MROW = MCP-MCE+MDT
            ALU(MRD,MCD) = ALU(MRD,MCD) + AP
            ALU(MROW,MCOL) = ALU(MROW,MCOL) - AE
!
!---      SPLib solver  ---
!
          ELSEIF( ILES.EQ.3 .OR. ILES.EQ.4 ) THEN
            DLU(MCD) = DLU(MCD) + AP
            MROW = KLUC(MCP,MA)
            MA = MA + 1
            DLU(MROW) = DLU(MROW) - AE

          ENDIF
        ENDIF
  140   CONTINUE
!
!---    North face diffusion and advection terms  ---
!
        IF( J.NE.JFLD ) THEN
          IF( IXP(NN).EQ.0 .OR. INBS(5,N).NE.0 ) GOTO 150
          DYNR = RP(I)*DYGF(NN)
          TCOR = (T(2,NN)+TABS)/TSPRF
          SDFLN = SMDL(NSL)*TCOR*(VISRL/VISL(2,NN))
          VMCN = SL(2,NN)*PORD(2,NN)
          FCLN = YL(NN,NSL)/(VMCN+SMALL)
          IF( IEDL(NSL).EQ.2 ) THEN
            DLN = SDCL(1,IZ(NN),NSL)*SDCL(2,IZ(NN),NSL)*
     &        EXP(VMCN*SDCL(3,IZ(NN),NSL))
          ELSEIF( IEDL(NSL).EQ.3 ) THEN
            DLN = TORL(2,NN)*VMCN*SMDL(NSL)
          ELSEIF( IEDL(NSL).EQ.4 ) THEN
            DLN = SDCL(1,IZ(NN),NSL)*SDCL(2,IZ(NN),NSL)*
     &        VMCN**SDCL(3,IZ(NN),NSL)
          ELSE
            DLN = TORL(2,NN)*VMCN*SDFLN
          ENDIF
          INDX = 16
          DLY = DIFMN(DLP,DLN,DYNR,DYR,VL(1,NQY),INDX)
          DLY = AFY(NQY)*(DLY+DPLN)/(DYGP(NQY)*RP(I))
          FLN = AFY(NQY)*VL(1,NQY)
          IF( MOD(ISLC(23),10).EQ.1 ) FLS = 0.D+0
          VMCNX = DIFMN(VMCP,VMCN,DYNR,DYR,VL(1,NQY),INDX)
          CRLN = ABS(VL(1,NQY))*DT/(DYGP(NQY)*VMCNX+SMALL)/RP(I)
!
!---      TVD solute transport  ---
!
          IF( ISLC(1).GE.1 ) THEN
            IF( FLN.GE.ZERO .AND. J.GT.1 ) THEN
             IF( IXP(NS).GT.0 ) THEN
              FCLS = YL(NS,NSL)/(SL(2,NS)*PORD(2,NS)+SMALL)
              V1X = CO(N,NSL)*FCLP-CO(NS,NSL)*FCLS
              V2X = CO(NN,NSL)*FCLN-CO(N,NSL)*FCLP
              V3X = DYGF(NN)+DYGF(N)
              V4X = DYGF(N)+DYGF(NS)
              R = RLIMIT( V1X,V2X,V3X,V4X )
!              R = ((CO(N,NSL)*FCLP-CO(NS,NSL)*FCLS)
!     &          /(CO(NN,NSL)*FCLN-CO(N,NSL)*FCLP+EPSL))
!     &          *((DYGF(NN)+DYGF(N))/(DYGF(N)+DYGF(NS)))
              THETA = FLIMIT( R,CRLN,ISLC(1) )
              DYF = DYGF(N)/(DYGF(N)+DYGF(NN))
              VCY = CO(NN,NSL)*FLN*THETA*DYF*FCLN
     &          + CO(N,NSL)*FLN*(1.D+0-THETA*DYF)*FCLP
              CC = CC - VCY
             ELSE
              VCY = CO(N,NSL)*FLN*FCLP
              CC = CC - VCY
             ENDIF
            ELSEIF( FLN.GE.ZERO .AND. J.EQ.1 ) THEN
              VCY = CO(N,NSL)*FLN*FCLP
              CC = CC - VCY
            ELSEIF( FLN.LT.ZERO .AND. J.LT.JFLD-1 ) THEN
             IF( IXP(NN+IFLD).GT.0 ) THEN
              FCLNN = YL(NN+IFLD,NSL)
     &          /(SL(2,NN+IFLD)*PORD(2,NN+IFLD)+SMALL)
              V1X = CO(NN,NSL)*FCLN-CO(NN+IFLD,NSL)*FCLNN
              V2X = CO(N,NSL)*FCLP-CO(NN,NSL)*FCLN
              V3X = DYGF(N)+DYGF(NN)
              V4X = DYGF(NN)+DYGF(NN+IFLD)
              R = RLIMIT( V1X,V2X,V3X,V4X )
!              R = ((CO(NN,NSL)*FCLN-CO(NN+IFLD,NSL)*FCLNN)
!     &          /(CO(N,NSL)*FCLP-CO(NN,NSL)*FCLN+EPSL))
!     &          *((DYGF(N)+DYGF(NN))/(DYGF(NN)+DYGF(NN+IFLD)))
              THETA = FLIMIT( R,CRLN,ISLC(1) )
              DYF = DYGF(NN)/(DYGF(N)+DYGF(NN))
              VCY = CO(NN,NSL)*FLN*(1.D+0-THETA*DYF)*FCLN
     &          + CO(N,NSL)*FLN*THETA*DYF*FCLP
              CC = CC - VCY
             ELSE
              VCY = CO(NN,NSL)*FLN*FCLN
              CC = CC - VCY
             ENDIF
            ELSEIF( FLN.LT.ZERO .AND. J.EQ.JFLD-1 ) THEN
              VCY = CO(NN,NSL)*FLN*FCLN
              CC = CC - VCY
            ENDIF
            AN = DLY*FCLN
            AP = DLY*FCLP
!
!---      Patankar solute transport  ---
!
          ELSE
            ALN = MAX(-FLN,ZERO)
     &        + DLY*MAX((ONE-(TENTH*ABS(FLN)/(DLY+SMALL)))**5,ZERO)
            AP = (ALN+FLN)*FCLP
            AN = ALN*FCLN
          ENDIF
          MCN = IXP(NN)
!
!---      Banded solver  ---
!
          IF( ILES.EQ.1 ) THEN
            MCOL = MCN
            MROW = MCP-MCN+MDT
            ALU(MRD,MCD) = ALU(MRD,MCD) + AP
            ALU(MROW,MCOL) = ALU(MROW,MCOL) - AN
!
!---      SPLib solver  ---
!
          ELSEIF( ILES.EQ.3 .OR. ILES.EQ.4 ) THEN
            DLU(MCD) = DLU(MCD) + AP
            MROW = KLUC(MCP,MA)
            MA = MA + 1
            DLU(MROW) = DLU(MROW) - AN

          ENDIF
        ENDIF
  150   CONTINUE
!
!---    Top face diffusion and advection terms  ---
!
        IF( K.NE.KFLD ) THEN
          IF( IXP(NT).EQ.0 .OR. INBS(6,N).NE.0 ) GOTO 160
          TCOR = (T(2,NT)+TABS)/TSPRF
          SDFLT = SMDL(NSL)*TCOR*(VISRL/VISL(2,NT))
          VMCT = SL(2,NT)*PORD(2,NT)
          FCLT = YL(NT,NSL)/(VMCT+SMALL)
          IF( IEDL(NSL).EQ.2 ) THEN
            DLT = SDCL(1,IZ(NT),NSL)*SDCL(2,IZ(NT),NSL)*
     &        EXP(VMCT*SDCL(3,IZ(NT),NSL))
          ELSEIF( IEDL(NSL).EQ.3 ) THEN
            DLT = TORL(2,NT)*VMCT*SMDL(NSL)
          ELSEIF( IEDL(NSL).EQ.4 ) THEN
            DLT = SDCL(1,IZ(NT),NSL)*SDCL(2,IZ(NT),NSL)*
     &        VMCT**SDCL(3,IZ(NT),NSL)
          ELSE
            DLT = TORL(2,NT)*VMCT*SDFLT
          ENDIF
          INDX = 16
          DLZ = DIFMN(DLP,DLT,DZGF(N),DZGF(NT),WL(1,NQZ),INDX)
          DLZ = AFZ(NQZ)*(DLZ+DPLT)/DZGP(NQZ)
          FLT = AFZ(NQZ)*WL(1,NQZ)
          IF( MOD(ISLC(23),10).EQ.1 ) FLT = 0.D+0
          VMCTX = DIFMN(VMCP,VMCT,DZGF(N),DZGF(NT),WL(1,NQZ),INDX)
          CRLT = ABS(WL(1,NQZ))*DT/(DZGP(NQZ)*VMCTX+SMALL)
!
!---      TVD solute transport  ---
!
          IF( ISLC(1).GE.1 ) THEN
            IF( FLT.GE.ZERO .AND. K.GT.1 ) THEN
             IF( IXP(NB).GT.0 ) THEN
              FCLB = YL(NB,NSL)/(SL(2,NB)*PORD(2,NB)+SMALL)
              V1X = CO(N,NSL)*FCLP-CO(NB,NSL)*FCLB
              V2X = CO(NT,NSL)*FCLT-CO(N,NSL)*FCLP
              V3X = DZGF(NT)+DZGF(N)
              V4X = DZGF(N)+DZGF(NB)
              R = RLIMIT( V1X,V2X,V3X,V4X )
!              R = ((CO(N,NSL)*FCLP-CO(NB,NSL)*FCLB)
!     &          /(CO(NT,NSL)*FCLT-CO(N,NSL)*FCLP+EPSL))
!     &          *((DZGF(NT)+DZGF(N))/(DZGF(N)+DZGF(NB)))
              THETA = FLIMIT( R,CRLT,ISLC(1) )
              DZF = DZGF(N)/(DZGF(N)+DZGF(NT))
              WCZ = CO(NT,NSL)*FLT*THETA*DZF*FCLT
     &          + CO(N,NSL)*FLT*(1.D+0-THETA*DZF)*FCLP
              CC = CC - WCZ
             ELSE
              WCZ = CO(N,NSL)*FLT*FCLP
              CC = CC - WCZ
             ENDIF
            ELSEIF( FLT.GE.ZERO .AND. K.EQ.1 ) THEN
              WCZ = CO(N,NSL)*FLT*FCLP
              CC = CC - WCZ
            ELSEIF( FLT.LT.ZERO .AND. K.LT.KFLD-1 ) THEN
             IF( IXP(NT+IJFLD).GT.0 ) THEN
              FCLTT = YL(NT+IJFLD,NSL)
     &          /(SL(2,NT+IJFLD)*PORD(2,NT+IJFLD)+SMALL)
              V1X = CO(NT,NSL)*FCLT-CO(NT+IJFLD,NSL)*FCLTT
              V2X = CO(N,NSL)*FCLP-CO(NT,NSL)*FCLT
              V3X = DZGF(N)+DZGF(NT)
              V4X = DZGF(NT)+DZGF(NT+IJFLD)
              R = RLIMIT( V1X,V2X,V3X,V4X )
!              R = ((CO(NT,NSL)*FCLT-CO(NT+IJFLD,NSL)*FCLTT)
!     &          /(CO(N,NSL)*FCLP-CO(NT,NSL)*FCLT+EPSL))
!     &          *((DZGF(N)+DZGF(NT))/(DZGF(NT)+DZGF(NT+IJFLD)))
              THETA = FLIMIT( R,CRLT,ISLC(1) )
              DZF = DZGF(NT)/(DZGF(N)+DZGF(NT))
              WCZ = CO(NT,NSL)*FLT*(1.D+0-THETA*DZF)*FCLT
     &          + CO(N,NSL)*FLT*THETA*DZF*FCLP
              CC = CC - WCZ
             ELSE
              WCZ = CO(NT,NSL)*FLT*FCLT
              CC = CC - WCZ
             ENDIF
            ELSEIF( FLT.LT.ZERO .AND. K.EQ.KFLD-1 ) THEN
              WCZ = CO(NT,NSL)*FLT*FCLT
              CC = CC - WCZ
            ENDIF
            AT = DLZ*FCLT
            AP = DLZ*FCLP
!
!---      Patankar solute transport  ---
!
          ELSE
            ALT = MAX(-FLT,ZERO)
     &        + DLZ*MAX((ONE-(TENTH*ABS(FLT)/(DLZ+SMALL)))**5,ZERO)
            AP = (ALT+FLT)*FCLP
            AT = ALT*FCLT
          ENDIF
          MCT = IXP(NT)
!
!---      Banded solver  ---
!
          IF( ILES.EQ.1 ) THEN
            MCOL = MCT
            MROW = MCP-MCT+MDT
            ALU(MRD,MCD) = ALU(MRD,MCD) + AP
            ALU(MROW,MCOL) = ALU(MROW,MCOL) - AT
!
!---      SPLib solver  ---
!
          ELSEIF( ILES.EQ.3 .OR. ILES.EQ.4 ) THEN
            DLU(MCD) = DLU(MCD) + AP
            MROW = KLUC(MCP,MA)
            MA = MA + 1
            DLU(MROW) = DLU(MROW) - AT

          ENDIF
        ENDIF
  160   CONTINUE
!
!---    Solution vector and parent 1 decay  ---
!
        BLU(MCP) = BLU(MCP) + CO(N,NSL)*SC
     &    - 6.931D-1*CO(N,NSL)*VOL(N)/HLF(NSL)
!        BLU(MCP) = BLU(MCP) + CO(N,NSL)*(2.D+0**(-DT/HLF(NSL)))*SC
!
!---    Daughter 1 chain decay  ---
!
        IF( NSL.LE.NSOLU ) THEN
          DO 170 NPSL = 1,NSOLU
            IF( NPSL.EQ.NSL ) GOTO 170
            BLU(MCP) = BLU(MCP) +
     &        CHDF(NPSL,NSL)*6.931D-1*CO(N,NPSL)*VOL(N)/HLF(NPSL)
!            BLU(MCP) = BLU(MCP) +
!     &        CHDF(NPSL,NSL)*CO(N,NPSL)*(2.D+0**(-DT/HLF(NPSL)))*SC
  170     CONTINUE
        ENDIF
!
!---    Leonard-TVD, Roe's Superbee, or 1st-order upwind  ---
!
        IF( ISLC(1).GE.1 ) BLU(MCP) = BLU(MCP) + CC
  900 CONTINUE
!$OMP END PARALLEL DO
!
!---  End of SJCBL group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END


