!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SFXL( NSL )
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
!     Compute solute transport flux aqueous-phase, excluding boundaries,
!     using either a Patankar scheme or a TVD scheme with  third-order
!     Leonard limiting for the  advective transport component.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, October.
!     Last Modified by MD White, Battelle, PNL, October 13, 1995.
!     $Id: sfxl.F 1080 2017-03-14 16:22:02Z d3c002 $
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
      SUB_LOG(ISUB_LOG) = '/SFXL'
      IF( INDEX(SVN_ID(186)(1:1),'$').EQ.0 ) SVN_ID(186) =
     & '$Id: sfxl.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      M = 1
!
!---  X-direction solute flux aqueous-phase, excluding boundaries
!
!$OMP PARALLEL DO
!$OMP&  DEFAULT(NONE)
!$OMP&  SHARED(C,DISPL,DISPT,DXGF,DXGP,ID,IDISP,IEDL,INBS,ISLC,IXP,IZ,
!$OMP&    JD,KD,NFLD,NSX,ONE,PORD,SDCL,SL,SMALL,SMDEF,SMDL,T,TABS,
!$OMP&    TENTH,TORL,TSPRF,UC,UL,VISL,VISRL,VL,WL,YL,ZERO)
!$OMP&  PRIVATE(AL,ALP,DDW,DFP,DFW,DISPLP,DISPLW,DISPTP,DISPTW,DPLW,
!$OMP&    DPTW,DPW,FCLP,FCLW,I,INDX,J,K,N,NPX,NW,SDFLP,SDFLW,
!$OMP&    TCOR,ULWSQ,ULX,VLWSQ,VLX,VMCP,VMCW,WLWSQ,WLX,ZVW)
!$OMP&  FIRSTPRIVATE(M,NSL)
      DO 100 N = 1,NFLD
        I = ID(N)
        IF( I.EQ.1 ) CYCLE
        J = JD(N)
        K = KD(N)
        NW = N-1
        IF( IXP(N).EQ.0 .OR. IXP(NW).EQ.0 .OR.
     &    INBS(3,N).NE.0 .OR. INBS(4,NW).NE.0 ) GOTO 100
        NPX = NSX(N)
        TCOR = (T(2,N)+TABS)/TSPRF
        SDFLP = SMDL(NSL)*TCOR*(VISRL/VISL(2,N))
        VMCP = SL(2,N)*PORD(2,N)
        FCLP = YL(N,NSL)/(VMCP+SMALL)
        IF( IEDL(NSL).EQ.1 ) THEN
          DFP = TORL(2,N)*VMCP*SDFLP
        ELSEIF( IEDL(NSL).EQ.2 ) THEN
          DFP = SDCL(1,IZ(N),NSL)*SDCL(2,IZ(N),NSL)*
     &      EXP(VMCP*SDCL(3,IZ(N),NSL))
        ELSEIF( IEDL(NSL).EQ.3 ) THEN
          DFP = TORL(2,N)*VMCP*SMDL(NSL)
        ELSEIF( IEDL(NSL).EQ.4 ) THEN
          DFP = SDCL(1,IZ(N),NSL)*SDCL(2,IZ(N),NSL)*
     &      VMCP**SDCL(3,IZ(N),NSL)
        ENDIF
        TCOR = (T(2,NW)+TABS)/TSPRF
        SDFLW = SMDL(NSL)*TCOR*(VISRL/VISL(2,NW))
        VMCW = SL(2,NW)*PORD(2,NW)
        FCLW = YL(NW,NSL)/(VMCW+SMALL)
        IF( IEDL(NSL).EQ.1 ) THEN
          DFW = TORL(2,NW)*VMCW*SDFLW
        ELSEIF( IEDL(NSL).EQ.2 ) THEN
          DFW = SDCL(1,IZ(NW),NSL)*SDCL(2,IZ(NW),NSL)*
     &      EXP(VMCW*SDCL(3,IZ(NW),NSL))
        ELSEIF( IEDL(NSL).EQ.3 ) THEN
          DFW = TORL(2,NW)*VMCW*SMDL(NSL)
        ELSEIF( IEDL(NSL).EQ.4 ) THEN
          DFW = SDCL(1,IZ(NW),NSL)*SDCL(2,IZ(NW),NSL)*
     &      VMCW**SDCL(3,IZ(NW),NSL)
        ENDIF
        IF( IDISP .EQ. 1 ) THEN
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
        ELSE
          DPW = 0.D+0
        ENDIF
        INDX = 16
        DFW = DIFMN( DFW,DFP,DXGF(NW),DXGF(N),UL(1,NPX),INDX)
        DDW = (DFW+DPW)/DXGP(NPX)
        IF( ISLC(1).GE.1 )  THEN
          UC(NPX,NSL) = UC(NPX,NSL) + DDW*(C(NW,NSL)*FCLW-C(N,NSL)*FCLP)
        ELSE
         AL = MAX( UL(1,NPX),ZERO ) +
     &     DDW*MAX( (ONE-(TENTH*ABS(UL(1,NPX))/(DDW+SMALL)))**5,ZERO )
         ALP = MAX( -UL(1,NPX),ZERO ) +
     &     DDW*MAX( (ONE-(TENTH*ABS(UL(1,NPX))/(DDW+SMALL)))**5,ZERO )
         UC(NPX,NSL) = UC(NPX,NSL)+(C(NW,NSL)*AL*FCLW-C(N,NSL)*ALP*FCLP)
        ENDIF
  100 CONTINUE
!$OMP END PARALLEL DO
!
!---  Y-direction solute flux aqueous-phase, excluding boundaries
!
!$OMP PARALLEL DO
!$OMP&  DEFAULT(NONE)
!$OMP&  SHARED(C,DISPL,DISPT,DYGF,DYGP,ID,IDISP,IEDL,IFLD,INBS,ISLC,
!$OMP&    IXP,IZ,JD,KD,NFLD,NSY,ONE,PORD,RP,SDCL,SL,SMALL,SMDEF,SMDL,
!$OMP&    T,TABS,TENTH,TORL,TSPRF,UL,VC,VISL,VISRL,VL,WL,YL,ZERO)
!$OMP&  PRIVATE(AL,ALP,DDS,DFP,DFS,DISPLP,DISPLS,DISPTP,DISPTS,DPLS,
!$OMP&    DPS,DPTS,FCLP,FCLS,I,INDX,J,K,N,NPY,NS,SDFLP,SDFLS,
!$OMP&    TCOR,ULSSQ,ULSX,VLSSQ,VLSX,VMCP,VMCS,WLSSQ,WLSX,ZVS)
!$OMP&  FIRSTPRIVATE(M,NSL)
      DO 200 N = 1,NFLD
        J = JD(N)
        IF( J.EQ.1 ) CYCLE
        I = ID(N)
        K = KD(N)
        NS = N-IFLD
        IF( IXP(N).EQ.0 .OR. IXP(NS).EQ.0 .OR.
     &    INBS(2,N).NE.0 .OR. INBS(5,NS).NE.0 ) GOTO 200
        NPY = NSY(N)
        TCOR = (T(2,N)+TABS)/TSPRF
        SDFLP = SMDL(NSL)*TCOR*(VISRL/VISL(2,N))
        VMCP = SL(2,N)*PORD(2,N)
        FCLP = YL(N,NSL)/(VMCP+SMALL)
        IF( IEDL(NSL).EQ.1 ) THEN
          DFP = TORL(2,N)*VMCP*SDFLP
        ELSEIF( IEDL(NSL).EQ.2 ) THEN
          DFP = SDCL(1,IZ(N),NSL)*SDCL(2,IZ(N),NSL)*
     &      EXP(VMCP*SDCL(3,IZ(N),NSL))
        ELSEIF( IEDL(NSL).EQ.3 ) THEN
          DFP = TORL(2,N)*VMCP*SMDL(NSL)
        ELSEIF( IEDL(NSL).EQ.4 ) THEN
          DFP = SDCL(1,IZ(N),NSL)*SDCL(2,IZ(N),NSL)*
     &      VMCP**SDCL(3,IZ(N),NSL)
        ENDIF
        TCOR = (T(2,NS)+TABS)/TSPRF
        SDFLS = SMDL(NSL)*TCOR*(VISRL/VISL(2,NS))
        VMCS = SL(2,NS)*PORD(2,NS)
        FCLS = YL(NS,NSL)/(VMCS+SMALL)
        IF( IEDL(NSL).EQ.1 ) THEN
          DFS = TORL(2,NS)*VMCS*SDFLS
        ELSEIF( IEDL(NSL).EQ.2 ) THEN
          DFS = SDCL(1,IZ(NS),NSL)*SDCL(2,IZ(NS),NSL)*
     &      EXP(VMCS*SDCL(3,IZ(NS),NSL))
        ELSEIF( IEDL(NSL).EQ.3 ) THEN
          DFS = TORL(2,NS)*VMCS*SMDL(NSL)
        ELSEIF( IEDL(NSL).EQ.4 ) THEN
          DFS = SDCL(1,IZ(NS),NSL)*SDCL(2,IZ(NS),NSL)*
     &      VMCS**SDCL(3,IZ(NS),NSL)
        ENDIF
        IF( IDISP .EQ. 1 ) THEN
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
        ELSE
          DPS = 0.D+0
        ENDIF
        INDX = 16
        DFS = DIFMN( DFS,DFP,DYGF(NS),DYGF(N),VL(1,NPY),INDX)
        DDS = (DFS+DPS)/(DYGP(NPY)*RP(I))
        IF( ISLC(1).GE.1 )  THEN
          VC(NPY,NSL) = VC(NPY,NSL) + DDS*(C(NS,NSL)*FCLS-C(N,NSL)*FCLP)
        ELSE
         AL = MAX( VL(1,NPY),ZERO ) +
     &     DDS*MAX( (ONE-(TENTH*ABS(VL(1,NPY))/(DDS+SMALL)))**5,ZERO )
         ALP = MAX( -VL(1,NPY),ZERO ) +
     &     DDS*MAX( (ONE-(TENTH*ABS(VL(1,NPY))/(DDS+SMALL)))**5,ZERO )
         VC(NPY,NSL) = VC(NPY,NSL)+(C(NS,NSL)*AL*FCLS-C(N,NSL)*ALP*FCLP)
        ENDIF
  200 CONTINUE
!$OMP END PARALLEL DO
!
!---  Z-direction solute flux aqueous-phase, excluding boundaries
!
!$OMP PARALLEL DO
!$OMP&  DEFAULT(NONE)
!$OMP&  SHARED(C,DISPL,DISPT,DZGF,DZGP,ID,IDISP,IEDL,IJFLD,INBS,
!$OMP&    ISLC,IXP,IZ,JD,KD,NFLD,NSZ,ONE,PORD,SDCL,SL,SMALL,SMDEF,
!$OMP&    SMDL,T,TABS,TENTH,TORL,TSPRF,UL,VISL,VISRL,VL,WC,WL,YL,ZERO)
!$OMP&  PRIVATE(AL,ALP,DDB,DFB,DFP,DISPLB,DISPLP,DISPTB,DISPTP,DPB,
!$OMP&    DPLB,DPTB,FCLB,FCLP,I,INDX,J,K,N,NB,NPZ,SDFLB,SDFLP,
!$OMP&    TCOR,ULBSQ,ULBX,VLBSQ,VLBX,VMCB,VMCP,WLBSQ,WLBX,ZVB)
!$OMP&  FIRSTPRIVATE(M,NSL)
      DO 300 N = 1,NFLD
        K = KD(N)
        IF( K.EQ.1 ) CYCLE
        J = JD(N)
        I = ID(N)
        NB = N-IJFLD
        IF( IXP(N).EQ.0 .OR. IXP(NB).EQ.0 .OR.
     &    INBS(1,N).NE.0 .OR. INBS(6,NB).NE.0 ) GOTO 300
        NPZ = NSZ(N)
        TCOR = (T(2,N)+TABS)/TSPRF
        SDFLP = SMDL(NSL)*TCOR*(VISRL/VISL(2,N))
        VMCP = SL(2,N)*PORD(2,N)
        FCLP = YL(N,NSL)/(VMCP+SMALL)
        IF( IEDL(NSL).EQ.1 ) THEN
          DFP = TORL(2,N)*VMCP*SDFLP
        ELSEIF( IEDL(NSL).EQ.2 ) THEN
          DFP = SDCL(1,IZ(N),NSL)*SDCL(2,IZ(N),NSL)*
     &      EXP(VMCP*SDCL(3,IZ(N),NSL))
        ELSEIF( IEDL(NSL).EQ.3 ) THEN
          DFP = TORL(2,N)*VMCP*SMDL(NSL)
        ELSEIF( IEDL(NSL).EQ.4 ) THEN
          DFP = SDCL(1,IZ(N),NSL)*SDCL(2,IZ(N),NSL)*
     &      VMCP**SDCL(3,IZ(N),NSL)
        ENDIF
        TCOR = (T(2,NB)+TABS)/TSPRF
        SDFLB = SMDL(NSL)*TCOR*(VISRL/VISL(2,NB))
        VMCB = SL(2,NB)*PORD(2,NB)
        FCLB = YL(NB,NSL)/(VMCB+SMALL)
        IF( IEDL(NSL).EQ.1 ) THEN
          DFB = TORL(2,NB)*VMCB*SDFLB
        ELSEIF( IEDL(NSL).EQ.2 ) THEN
          DFB = SDCL(1,IZ(NB),NSL)*SDCL(2,IZ(NB),NSL)*
     &      EXP(VMCB*SDCL(3,IZ(NB),NSL))
        ELSEIF( IEDL(NSL).EQ.3 ) THEN
          DFB = TORL(2,NB)*VMCB*SMDL(NSL)
        ELSEIF( IEDL(NSL).EQ.4 ) THEN
          DFB = SDCL(1,IZ(NB),NSL)*SDCL(2,IZ(NB),NSL)*
     &      VMCB**SDCL(3,IZ(NB),NSL)
        ENDIF
        IF( IDISP .EQ. 1 ) THEN
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
        ELSE
          DPB = 0.D+0
        ENDIF
        INDX = 16
        DFB = DIFMN( DFB,DFP,DZGF(NB),DZGF(N),WL(1,NPZ),INDX)
        DDB = (DFB+DPB)/DZGP(NPZ)
        IF( ISLC(1).GE.1 ) THEN
          WC(NPZ,NSL) = WC(NPZ,NSL) + DDB*(C(NB,NSL)*FCLB-C(N,NSL)*FCLP)
        ELSE
         AL = MAX( WL(1,NPZ),ZERO ) +
     &     DDB*MAX( (ONE-(TENTH*ABS(WL(1,NPZ))/(DDB+SMALL)))**5,ZERO )
         ALP = MAX( -WL(1,NPZ),ZERO ) +
     &     DDB*MAX( (ONE-(TENTH*ABS(WL(1,NPZ))/(DDB+SMALL)))**5,ZERO )
         WC(NPZ,NSL) = WC(NPZ,NSL)+(C(NB,NSL)*AL*FCLB-C(N,NSL)*ALP*FCLP)
        ENDIF
  300 CONTINUE
!$OMP END PARALLEL DO
!
!---  End of SFXL group
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END


