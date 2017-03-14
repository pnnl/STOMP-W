!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SFXLB( NSL )
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
!     Compute solute aqueous-phase fluxes on boundary surfaces.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, January, 1995.
!     Last Modified by MD White, Battelle, PNL, February 11, 1999.
!     $Id: sfxlb.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE PORMED
      USE GRID
      USE FLUXP
      USE FDVP
      USE CONST
      USE BCVP
      USE BCV
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
      REAL*8 BCX(LSPBC+1)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/SFXLB'
      IF( INDEX(SVN_ID(187)(1:1),'$').EQ.0 ) SVN_ID(187) =
     & '$Id: sfxlb.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Loop over number of specified boundary conditions  ---
!
      NBCT = MIN( NSL+LUK,NSOLU+LUK+1 )
      DO 200 NB = 1,NBC
        TMZ = TM
        MB = IBCIN(NB)
        IF( IBCC(NB).EQ.1 ) TMZ = MOD( TM,BC(1,IBCM(NB),MB) )
        IF( TMZ.LE.BC(1,1,MB) ) GOTO 200
        IF( IBCM(NB).GT.1 .AND. TMZ.GT.BC(1,IBCM(NB),MB) ) GOTO 200
        IF( IBCM(NB).EQ.1 ) THEN
!
!---      Solute transport  ---
!
          IF( NSL.LE.NSOLU ) THEN
            BCX(1) = BC(NSL+LBCU,1,MB)
!
!---      Reactive species transport  ---
!
          ELSE
            BCX(1) = 0.D+0
            DO 10 NSPX = 1,IBCSP(1,NB)
              MX = NSOLU+LBCU+NSPX
              BCX(NSPX+1) = BC(MX,1,MB)
   10       CONTINUE
          ENDIF
        ELSE
          DO 100 M = 2,IBCM(NB)
            IF( TMZ.LE.BC(1,M,MB) ) THEN
              DTBC = MIN( BC(1,M,MB)-TM,DT )
              TFBC = (TM-5.D-1*DTBC-BC(1,M-1,MB))/
     &          (BC(1,M,MB)-BC(1,M-1,MB))
!
!---          Solute transport  ---
!
              IF( NSL.LE.NSOLU ) THEN
                BCX(1) = BC(NSL+LBCU,M-1,MB) +
     &            TFBC*(BC(NSL+LBCU,M,MB)-BC(NSL+LBCU,M-1,MB))
                IF( IBCT(NBCT,NB).EQ.12 ) BCX(1) = CBO(NB,NSL)
!
!---          Reactive species transport  ---
!
              ELSE
                BCX(1) = 0.D+0
                DO 20 NSPX = 1,IBCSP(1,NB)
                  MX = NSOLU+LBCU+NSPX
                  BCX(NSPX+1) = BC(MX,M-1,MB) +
     &              TFBC*(BC(MX,M,MB)-BC(MX,M-1,MB))
                  IF( IBCT(NBCT,NB).EQ.12 ) BCX(NSPX) = CBO(NB,NSL)
   20           CONTINUE
              ENDIF
              GOTO 110
            ENDIF
  100     CONTINUE
          GOTO 200
        ENDIF
  110   CONTINUE
        N = IBCN(NB)
        MF = 1
        IZN = IZ(N)
        MP = IXP(N)
        I = ID(N)
        J = JD(N)
        K = KD(N)
!
!---  Compute adjacent node phase fractions  ---
!
        SVLP = SL(2,N)*PORD(2,N)
        FCLP = 0.D+0
        IF( SVLP.GT.SMALL ) FCLP = YL(N,NSL)/SVLP
!
!---    Solute transport only, skip calculations for reactive
!       species transport  ---
!
        IF( NSL.LE.NSOLU ) THEN
!
!---      Compute boundary phase fractions  ---
!
          IF( IPCL(NSL).EQ.2 ) THEN
            SVSB = RHOS(IZN)*PCSL(1,IZN,NSL)*(1.D+0-PORTB(2,NB))*
     &        SLB(2,NB)
          ELSE
            SVSB = RHOS(IZN)*PCSL(1,IZN,NSL)*(1.D+0-PORTB(2,NB))
          ENDIF
          SVLB = SLB(2,NB)*PORDB(2,NB)
          SVGB = SGB(2,NB)*PORDB(2,NB)
          SVNB = SNB(2,NB)*PORDB(2,NB)
!
!---      Constant gas-aqueous partition coefficient  ---
!
          IF( IPCGL(NSL).EQ.0 ) THEN
            PCGLX = PCGL(1,NSL)
!
!---      Temperature dependent gas-aqueous partition coefficient  ---
!
          ELSEIF( IPCGL(NSL).EQ.1 ) THEN
            TK = TB(2,NB)+TABS
            PCGLX = EXP( PCGL(1,NSL) + PCGL(2,NSL)/TK
     &        + PCGL(3,NSL)*LOG(TK) + PCGL(4,NSL)*TK
     &        + PCGL(5,NSL)*TK**2 )
!
!---      Water-vapor equilibrium gas-aqueous partition coefficient  ---
!
          ELSEIF( IPCGL(NSL).EQ.2 ) THEN
            PCGLX = RHOG(2,N)*XGW(2,N)/(RHOL(2,N)*XLW(2,N))
          ENDIF
          PCGLX = MAX( PCGLX,1.D-20 )
          PCGLX = MIN( PCGLX,1.D+20 )
!
!---    Phase-volumetric concentration ratios  ---
!
          FCLB = 1.D+0/(SVSB + SVLB + SVNB/PCLN(1,NSL) 
     &      + SVGB*PCGLX)
          FCGB = 1.D+0/((SVSB + SVLB + SVNB)/PCGLX + SVGB)
          FCNB = 1.D+0/((SVSB + SVLB + SVGB*PCGLX)*PCLN(1,NSL) + SVNB)
!
!---    Phase mole fractions  ---
!
          YLB(NB,NSL) = SVLB*FCLB
          YGB(NB,NSL) = SVGB*FCGB
          YNB(NB,NSL) = SVNB*FCNB
!
!---    Convert boundary concentrations  ---
!
          IF( IBCT(NBCT,NB).EQ.8 .OR. IBCT(NBCT,NB).EQ.14
     &      .OR. IBCT(NBCT,NB).EQ.23 ) THEN
            BCX(1) = BCX(1)/(FCLB+SMALL)
          ELSEIF( IBCT(NBCT,NB).EQ.9 .OR. IBCT(NBCT,NB).EQ.15 ) THEN
            BCX(1) = BCX(1)/(FCGB+SMALL)
          ELSEIF( IBCT(NBCT,NB).EQ.10 .OR. IBCT(NBCT,NB).EQ.16 ) THEN
            BCX(1) = BCX(1)/(FCNB+SMALL)
          ENDIF
        ELSE
!
!---      Skip for initial condition type boundary condition  ---
!
          IF( IBCT(NBCT,NB).NE.12 ) THEN
!
!---        Convert species concentrations to total-component
!           concentrations  ---
!
            IF( NSL.LE.NSOLU+NEQC ) THEN
              NEQ = NSL-NSOLU
              DO 130 NSP = 1,IEQ_C(1,NEQ)
                DO 120 NSPX = 1,IBCSP(1,NB)
                  IF( IBCSP(NSPX+1,NB).EQ.IEQ_C(NSP+1,NEQ) ) THEN
                    BCX(1) = BCX(1) + EQ_C(NSP,NEQ)*BCX(NSPX+1)
                  ENDIF
  120           CONTINUE
  130         CONTINUE
!
!---        Convert species concentrations to total-kinetic
!           concentrations  ---
!
            ELSEIF( NSL.LE.NSOLU+NEQC+NEQK ) THEN
              NEQ = NSL-NSOLU-NEQC
              DO 150 NSP = 1,IEQ_K(1,NEQ)
                DO 140 NSPX = 1,IBCSP(1,NB)
                  IF( IBCSP(NSPX+1,NB).EQ.IEQ_K(NSP+1,NEQ) ) THEN
                    BCX(1) = BCX(1) + EQ_K(NSP,NEQ)*BCX(NSPX+1)
                  ENDIF
  140           CONTINUE
  150         CONTINUE
            ENDIF
          ENDIF
          SVLB = SLB(2,NB)*PORDB(2,NB)
          YLB(NB,NSL) = 1.D+0
          FCLB = 0.D+0
          IF( SVLB.GT.SMALL ) FCLB = YLB(NB,NSL)/SVLB
!
!---      Convert boundary phase concentrations to
!         volumetric concentrations  ---
!
          IF( IBCT(NBCT,NB).EQ.8 .OR. IBCT(NBCT,NB).EQ.14
     &      .OR. IBCT(NBCT,NB).EQ.23 ) THEN
            BCX(1) = BCX(1)*SVLB
          ENDIF
        ENDIF
!
!---  Diffusion coefficients at nodes adjacent to boundaries  ---
!
        IF( IEDL(NSL).EQ.1 ) THEN
          TCOR = (T(2,N)+TABS)/TSPRF
          SDFLP = SMDL(NSL)*TCOR*(VISRL/VISL(2,N))
          DLP = TORL(2,N)*SL(2,N)*PORD(2,N)*SDFLP
        ELSEIF( IEDL(NSL).EQ.2 ) THEN
          DLP = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &      EXP(SL(2,N)*PORD(2,N)*SDCL(3,IZN,NSL))
        ELSEIF( IEDL(NSL).EQ.3 ) THEN
          DLP = TORL(2,N)*SL(2,N)*PORD(2,N)*SMDL(NSL)
        ELSEIF( IEDL(NSL).EQ.4 ) THEN
          DLP = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &      (SL(2,N)*PORD(2,N))**SDCL(3,IZN,NSL)
        ENDIF
!
!---  Bottom boundary  ---
!
        IF( IBCD(NB).EQ.-3 ) THEN
          NPZ = NSZ(N)
!
!---  Hydraulic dispersion
!
          IF( IDISP.EQ.1 ) THEN
            CALL ADVBB( PORD(2,N),PORDB(2,NB),SL(2,N),SLB(2,NB),
     &        UL,VL,WL,ULBX,VLBX,WLBX,N,MF )
            CALL SHDP( WLBX,ULBX,VLBX,DISPL(IZN),DISPT(IZN),DPLB )
!            ULBX = (0.5D+0*(UL(1,NSX(N))+UL(1,NSX(N)+1)))**2
!            VLBX = (0.5D+0*(VL(1,NSY(N))+VL(1,NSY(N)+IFLD)))**2
!            WLBX = (WL(1,NPZ))**2
!            ZLB = SQRT(ULBX + VLBX + WLBX)
!            DPLB = (DISPL(IZN)*WLBX + DISPT(IZN)*(ULBX+VLBX))/
!     &        (ZLB+SMALL)
            DPLB = DPLB*SMDEF(IZN,NSL)
          ELSE
            DPLB = 0.D+0
          ENDIF
!
!---  Dirichlet ---
!
          IF( IBCT(NBCT,NB).EQ.1 .OR. IBCT(NBCT,NB).EQ.8 .OR.
     &      IBCT(NBCT,NB).EQ.9 .OR. IBCT(NBCT,NB).EQ.10 .OR.
     &      IBCT(NBCT,NB).EQ.12 ) THEN
            IF( IEDL(NSL).EQ.1 ) THEN
              TCOR = (TB(2,NB)+TABS)/TSPRF
              SDFLB = SMDL(NSL)*TCOR*(VISRL/VISLB(2,NB))
              DLB = TORLB(2,NB)*SVLB*SDFLB
            ELSEIF( IEDL(NSL).EQ.2 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          EXP(SVLB*SDCL(3,IZN,NSL))
            ELSEIF( IEDL(NSL).EQ.3 ) THEN
              DLB = TORLB(2,NB)*SVLB*SMDL(NSL)
            ELSEIF( IEDL(NSL).EQ.4 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          SVLB**SDCL(3,IZN,NSL)
            ENDIF
            INDX = 16
            DLZ = DIFMN(DLB,DLP,DZGF(N),DZGF(N),WL(1,NPZ),INDX)
            DLZ = (DLZ+DPLB)/(5.D-1*DZGF(N))
            IF( ISLC(1).GE.1 .AND. KFLD.GT.1 )  THEN
              AB = DLZ
              AP = DLZ
            ELSE
              AB = MAX( WL(1,NPZ),ZERO ) +
     &         DLZ*MAX((ONE-(TENTH*ABS(WL(1,NPZ))/(DLZ+SMALL)))**5,ZERO)
              AP = MAX( -WL(1,NPZ),ZERO ) +
     &         DLZ*MAX((ONE-(TENTH*ABS(WL(1,NPZ))/(DLZ+SMALL)))**5,ZERO)
            ENDIF
            WC(NPZ,NSL) = WC(NPZ,NSL)+(BCX(1)*AB*FCLB-C(N,NSL)*AP*FCLP)
!
!---  Outflow ---
!
          ELSEIF( IBCT(NBCT,NB).EQ.7 .OR.
     &      ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (WL(1,NPZ)/EPSL.LT.-EPSL)) ) THEN
            IF( ISLC(1).GE.1 .AND. KFLD.GT.1 )  THEN
              AP = 0.D+0
            ELSE
              AP = MAX( -WL(1,NPZ),ZERO )
            ENDIF
            WC(NPZ,NSL) = WC(NPZ,NSL) -  C(N,NSL)*AP*FCLP
!
!---  Inflow ---
!
          ELSEIF( IBCT(NBCT,NB).GE.13 .AND. IBCT(NBCT,NB).LE.16
     &       .OR. ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (WL(1,NPZ)/EPSL.GT.EPSL)) ) THEN
            IF( ISLC(1).GE.1 .AND. KFLD.GT.1 )  THEN
              AB = 0.D+0
            ELSE
              AB = MAX( WL(1,NPZ),ZERO )
            ENDIF
            WC(NPZ,NSL) = WC(NPZ,NSL) + BCX(1)*AB*FCLB
          ENDIF
!
!---  South boundary  ---
!
        ELSEIF( IBCD(NB).EQ.-2 ) THEN
          NPY = NSY(N)
!
!---  Hydraulic dispersion
!
          IF( IDISP.EQ.1 ) THEN
            CALL ADVSB( PORD(2,N),PORDB(2,NB),SL(2,N),SLB(2,NB),
     &        UL,VL,WL,ULSX,VLSX,WLSX,N,MF )
            CALL SHDP( VLSX,WLSX,ULSX,DISPL(IZN),DISPT(IZN),DPLS )
!            ULSX = (0.5D+0*(UL(1,NSX(N))+UL(1,NSX(N)+1)))**2
!            VLSX = VL(1,NPY)**2
!            WLSX = (0.5D+0*(WL(1,NSZ(N))+WL(1,NSZ(N)+IJFLD)))**2
!            ZLS = SQRT(ULSX + VLSX + WLSX)
!            DPLS = (DISPL(IZN)*VLSX + DISPT(IZN)*(ULSX+WLSX))/
!     &        (ZLS+SMALL)
            DPLS = DPLS*SMDEF(IZN,NSL)
          ELSE
            DPLS = 0.D+0
          ENDIF
!
!---  Dirichlet ---
!
          IF( IBCT(NBCT,NB).EQ.1 .OR. IBCT(NBCT,NB).EQ.8 .OR.
     &      IBCT(NBCT,NB).EQ.9 .OR. IBCT(NBCT,NB).EQ.10 .OR. 
     &      IBCT(NBCT,NB).EQ.12 ) THEN
            IF( IEDL(NSL).EQ.1 ) THEN
              TCOR = (TB(2,NB)+TABS)/TSPRF
              SDFLB = SMDL(NSL)*TCOR*(VISRL/VISLB(2,NB))
              DLB = TORLB(2,NB)*SVLB*SDFLB
            ELSEIF( IEDL(NSL).EQ.2 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          EXP(SVLB*SDCL(3,IZN,NSL))
            ELSEIF( IEDL(NSL).EQ.3 ) THEN
              DLB = TORLB(2,NB)*SVLB*SMDL(NSL)
            ELSEIF( IEDL(NSL).EQ.4 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          SVLB**SDCL(3,IZN,NSL)
            ENDIF
            INDX = 16
            DLY = DIFMN(DLB,DLP,DYGF(N),DYGF(N),VL(1,NPY),INDX)
            DLY = (DLY+DPLS)/RP(I)/(5.D-1*DYGF(N))
            IF( ISLC(1).GE.1 .AND. JFLD.GT.1 )  THEN
              AS = DLY
              AP = DLY
            ELSE
              AS = MAX( VL(1,NPY),ZERO ) +
     &         DLY*MAX((ONE-(TENTH*ABS(VL(1,NPY))/(DLY+SMALL)))**5,ZERO)
              AP = MAX( -VL(1,NPY),ZERO ) +
     &         DLY*MAX((ONE-(TENTH*ABS(VL(1,NPY))/(DLY+SMALL)))**5,ZERO)
            ENDIF
            VC(NPY,NSL) = VC(NPY,NSL)+(BCX(1)*AS*FCLB-C(N,NSL)*AP*FCLP)
!
!---  Outflow ---
!
          ELSEIF( IBCT(NBCT,NB).EQ.7 .OR.
     &      ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (VL(1,NPY)/EPSL.LT.-EPSL)) ) THEN
            IF( ISLC(1).GE.1 .AND. JFLD.GT.1 )  THEN
              AP = 0.D+0
            ELSE
              AP = MAX( -VL(1,NPY),ZERO )
            ENDIF
            VC(NPY,NSL) = VC(NPY,NSL) - C(N,NSL)*AP*FCLP
!
!---  Inflow ---
!
          ELSEIF( IBCT(NBCT,NB).GE.13 .AND. IBCT(NBCT,NB).LE.16
     &       .OR. ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (VL(1,NPY)/EPSL.GT.EPSL)) ) THEN
            IF( ISLC(1).GE.1 .AND. JFLD.GT.1 )  THEN
              AS = 0.D+0
            ELSE
              AS = MAX( VL(1,NPY),ZERO )
            ENDIF
            VC(NPY,NSL) = VC(NPY,NSL) - BCX(1)*AS*FCLB
          ENDIF
!
!---  West boundary  ---
!
        ELSEIF( IBCD(NB).EQ.-1 ) THEN
          NPX = NSX(N)
!
!---  Hydraulic dispersion
!
          IF( IDISP.EQ.1 ) THEN
            CALL ADVWB( PORD(2,N),PORDB(2,NB),SL(2,N),SLB(2,NB),
     &        UL,VL,WL,ULX,VLX,WLX,N,MF )
            CALL SHDP( ULX,VLX,WLX,DISPL(IZN),DISPT(IZN),DPLW )
!            ULX = UL(1,NPX)**2
!            VLX = (0.5D+0*(VL(1,NSY(N))+VL(1,NSY(N)+IFLD)))**2
!            WLX = (0.5D+0*(WL(1,NSZ(N))+WL(1,NSZ(N)+IJFLD)))**2
!            ZLW = SQRT(ULX + VLX + WLX)
!            DPLW = (DISPL(IZN)*ULX + DISPT(IZN)*(WLX+VLX))/
!     &          (ZLW+SMALL)
            DPLW = DPLW*SMDEF(IZN,NSL)
            ELSE
              DPLW = 0.D+0
            ENDIF
!
!---  Dirichlet ---
!
          IF( IBCT(NBCT,NB).EQ.1 .OR. IBCT(NBCT,NB).EQ.8 .OR.
     &      IBCT(NBCT,NB).EQ.9 .OR. IBCT(NBCT,NB).EQ.10 .OR.
     &      IBCT(NBCT,NB).EQ.12 ) THEN
            IF( IEDL(NSL).EQ.1 ) THEN
              TCOR = (TB(2,NB)+TABS)/TSPRF
              SDFLB = SMDL(NSL)*TCOR*(VISRL/VISLB(2,NB))
              DLB = TORLB(2,NB)*SVLB*SDFLB
            ELSEIF( IEDL(NSL).EQ.2 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          EXP(SVLB*SDCL(3,IZN,NSL))
            ELSEIF( IEDL(NSL).EQ.3 ) THEN
              DLB = TORLB(2,NB)*SVLB*SMDL(NSL)
            ELSEIF( IEDL(NSL).EQ.4 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          SVLB**SDCL(3,IZN,NSL)
            ENDIF
            INDX = 16
            DLX = DIFMN(DLB,DLP,DXGF(N),DXGF(N),UL(1,NPX),INDX)
            DLX = (DLX+DPLW)/(5.D-1*DXGF(N))
            IF( ISLC(1).GE.1 .AND. IFLD.GT.1 )  THEN
              AW = DLX
              AP = DLX
            ELSE
              AW = MAX( UL(1,NPX),ZERO ) +
     &         DLX*MAX((ONE-(TENTH*ABS(UL(1,NPX))/(DLX+SMALL)))**5,ZERO)
              AP = MAX( -UL(1,NPX),ZERO ) +
     &         DLX*MAX((ONE-(TENTH*ABS(UL(1,NPX))/(DLX+SMALL)))**5,ZERO)
            ENDIF
            UC(NPX,NSL) = UC(NPX,NSL)+(BCX(1)*AW*FCLB-C(N,NSL)*AP*FCLP)
!
!---  Outflow ---
!
          ELSEIF( IBCT(NBCT,NB).EQ.7 .OR.
     &      ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (UL(1,NPX)/EPSL.LT.-EPSL)) ) THEN
            IF( ISLC(1).GE.1 .AND. IFLD.GT.1 )  THEN
              AP = 0.D+0
            ELSE
              AP = MAX( -UL(1,NPX),ZERO )
            ENDIF
            UC(NPX,NSL) = UC(NPX,NSL) -  C(N,NSL)*AP*FCLP
!
!---  Inflow ---
!
          ELSEIF( IBCT(NBCT,NB).GE.13 .AND. IBCT(NBCT,NB).LE.16
     &       .OR. ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (UL(1,NPX)/EPSL.GT.EPSL)) ) THEN
            IF( ISLC(1).GE.1 .AND. IFLD.GT.1 )  THEN
              AW = 0.D+0
            ELSE
              AW = MAX( UL(1,NPX),ZERO )
            ENDIF
            UC(NPX,NSL) = UC(NPX,NSL) + BCX(1)*AW*FCLB
          ENDIF
!
!---  East boundary  ---
!
        ELSEIF( IBCD(NB).EQ.1 ) THEN
          NQX = NSX(N)+1
!
!---  Hydraulic dispersion
!
          IF( IDISP.EQ.1 ) THEN
            CALL ADVEB( PORD(2,N),PORDB(2,NB),SL(2,N),SLB(2,NB),
     &        UL,VL,WL,ULEX,VLEX,WLEX,N,MF )
            CALL SHDP( ULEX,VLEX,WLEX,DISPL(IZN),DISPT(IZN),DPLE )
!            ULEX = UL(1,NQX)**2
!            VLEX = (0.5D+0*(VL(1,NSY(N))+VL(1,NSY(N)+IFLD)))**2
!            WLEX = (0.5D+0*(WL(1,NSZ(N))+WL(1,NSZ(N)+IJFLD)))**2
!            ZLE = SQRT(ULEX + VLEX + WLEX)
!            DPLE = (DISPL(IZN)*ULEX + DISPT(IZN)*(WLEX+VLEX))/
!     &        (ZLE+SMALL)
            DPLE = DPLE*SMDEF(IZN,NSL)
          ELSE
            DPLE = 0.D+0
          ENDIF
!
!---  Dirichlet ---
!
          IF( IBCT(NBCT,NB).EQ.1 .OR. IBCT(NBCT,NB).EQ.8 .OR.
     &      IBCT(NBCT,NB).EQ.9 .OR. IBCT(NBCT,NB).EQ.10 .OR.
     &      IBCT(NBCT,NB).EQ.12 ) THEN
            TCOR = (TB(2,NB)+TABS)/TSPRF
            SDFLB = SMDL(NSL)*TCOR*(VISRL/VISLB(2,NB))
            IF( IEDL(NSL).EQ.1 ) THEN
              TCOR = (TB(2,NB)+TABS)/TSPRF
              SDFLB = SMDL(NSL)*TCOR*(VISRL/VISLB(2,NB))
              DLB = TORLB(2,NB)*SVLB*SDFLB
            ELSEIF( IEDL(NSL).EQ.2 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          EXP(SVLB*SDCL(3,IZN,NSL))
            ELSEIF( IEDL(NSL).EQ.3 ) THEN
              DLB = TORLB(2,NB)*SVLB*SMDL(NSL)
            ELSEIF( IEDL(NSL).EQ.4 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          SVLB**SDCL(3,IZN,NSL)
            ENDIF
            INDX = 16
            DLX = DIFMN(DLP,DLB,DXGF(N),DXGF(N),UL(1,NQX),INDX)
            DLX = (DLX+DPLE)/(5.D-1*DXGF(N))
            IF( ISLC(1).GE.1 .AND. IFLD.GT.1 )  THEN
              AE = DLX
              AP = DLX
            ELSE
              AE = MAX( -UL(1,NQX),ZERO ) +
     &         DLX*MAX((ONE-(TENTH*ABS(UL(1,NQX))/(DLX+SMALL)))**5,ZERO)
              AP = MAX( UL(1,NQX),ZERO ) +
     &         DLX*MAX((ONE-(TENTH*ABS(UL(1,NQX))/(DLX+SMALL)))**5,ZERO)
            ENDIF
            UC(NQX,NSL) = UC(NQX,NSL)+(C(N,NSL)*AP*FCLP-BCX(1)*AE*FCLB)
!
!---  Outflow ---
!
          ELSEIF( IBCT(NBCT,NB).EQ.7 .OR.
     &      ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (UL(1,NQX)/EPSL.GT.EPSL)) ) THEN
            IF( ISLC(1).GE.1 .AND. IFLD.GT.1 )  THEN
              AP = 0.D+0
            ELSE
              AP = MAX( UL(1,NQX),ZERO )
            ENDIF
            UC(NQX,NSL) = UC(NQX,NSL) + C(N,NSL)*AP*FCLP
!
!---  Inflow ---
!
          ELSEIF( IBCT(NBCT,NB).GE.13 .AND. IBCT(NBCT,NB).LE.16
     &       .OR. ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (UL(1,NQX)/EPSL.LT.-EPSL)) ) THEN
            IF( ISLC(1).GE.1 .AND. IFLD.GT.1 )  THEN
              AE = 0.D+0
            ELSE
              AE = MAX( -UL(1,NQX),ZERO )
            ENDIF
            UC(NQX,NSL) = UC(NQX,NSL) - BCX(1)*AE*FCLB
          ENDIF
!
!---  North boundary  ---
!
        ELSEIF( IBCD(NB).EQ.2 ) THEN
          NQY = NSY(N)+IFLD
!
!---  Hydraulic dispersion
!
          IF( IDISP.EQ.1 ) THEN
            CALL ADVNB( PORD(2,N),PORDB(2,NB),SL(2,N),SLB(2,NB),
     &        UL,VL,WL,ULNX,VLNX,WLNX,N,MF )
            CALL SHDP( VLNX,WLNX,ULNX,DISPL(IZN),DISPT(IZN),DPLN )
!            ULNX = (0.5D+0*(UL(1,NSX(N))+UL(1,NSX(N)+1)))**2
!            VLNX = VL(1,NQY)**2
!            WLNX = (0.5D+0*(WL(1,NSZ(N))+WL(1,NSZ(N)+IJFLD)))**2
!            ZLN = SQRT(ULNX + VLNX + WLNX)
!            DPLN = (DISPL(IZN)*VLNX + DISPT(IZN)*(ULNX+WLNX))/
!     &        (ZLN+SMALL)
            DPLN = DPLN*SMDEF(IZN,NSL)
          ELSE
            DPLN = 0.D+0
          ENDIF
!
!---  Dirichlet ---
!
          IF( IBCT(NBCT,NB).EQ.1 .OR. IBCT(NBCT,NB).EQ.8 .OR.
     &      IBCT(NBCT,NB).EQ.9 .OR. IBCT(NBCT,NB).EQ.10 .OR.
     &      IBCT(NBCT,NB).EQ.12 ) THEN
            TCOR = (TB(2,NB)+TABS)/TSPRF
            SDFLB = SMDL(NSL)*TCOR*(VISRL/VISLB(2,NB))
            IF( IEDL(NSL).EQ.1 ) THEN
              TCOR = (TB(2,NB)+TABS)/TSPRF
              SDFLB = SMDL(NSL)*TCOR*(VISRL/VISLB(2,NB))
              DLB = TORLB(2,NB)*SVLB*SDFLB
            ELSEIF( IEDL(NSL).EQ.2 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          EXP(SVLB*SDCL(3,IZN,NSL))
            ELSEIF( IEDL(NSL).EQ.3 ) THEN
              DLB = TORLB(2,NB)*SVLB*SMDL(NSL)
            ELSEIF( IEDL(NSL).EQ.4 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          SVLB**SDCL(3,IZN,NSL)
            ENDIF
            INDX = 16
            DLY = DIFMN(DLP,DLB,DYGF(N),DYGF(N),VL(1,NQY),INDX)
            DLY = (DLY+DPLN)/RP(I)/(5.D-1*DYGF(N))
            IF( ISLC(1).GE.1 .AND. JFLD.GT.1 )  THEN
              AN = DLY
              AP = DLY
            ELSE
              AN = MAX( -VL(1,NQY),ZERO ) +
     &         DLY*MAX((ONE-(TENTH*ABS(VL(1,NQY))/(DLY+SMALL)))**5,ZERO)
              AP = MAX( VL(1,NQY),ZERO ) +
     &         DLY*MAX((ONE-(TENTH*ABS(VL(1,NQY))/(DLY+SMALL)))**5,ZERO)
            ENDIF
            VC(NQY,NSL) = VC(NQY,NSL)+(C(N,NSL)*AP*FCLP-BCX(1)*AN*FCLB)
!
!---  Outflow ---
!
          ELSEIF( IBCT(NBCT,NB).EQ.7 .OR.
     &      ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (VL(1,NQY)/EPSL.GT.EPSL)) ) THEN
            IF( ISLC(1).GE.1 .AND. JFLD.GT.1 )  THEN
              AP = 0.D+0
            ELSE
              AP = MAX( VL(1,NQY),ZERO )
            ENDIF
            VC(NQY,NSL) = VC(NQY,NSL) + C(N,NSL)*AP*FCLP
!
!---  Inflow ---
!
          ELSEIF( IBCT(NBCT,NB).GE.13 .AND. IBCT(NBCT,NB).LE.16
     &       .OR. ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (VL(1,NQY)/EPSL.LT.-EPSL)) ) THEN
            IF( ISLC(1).GE.1 .AND. JFLD.GT.1 )  THEN
              AN = 0.D+0
            ELSE
              AN = MAX( -VL(1,NQY),ZERO )
            ENDIF
            VC(NQY,NSL) = VC(NQY,NSL) - BCX(1)*AN*FCLB
          ENDIF
!
!---  Top boundary
!
        ELSEIF( IBCD(NB).EQ.3 ) THEN
          NQZ = NSZ(N)+IJFLD
!
!---  Hydraulic dispersion
!
          IF( IDISP.EQ.1 ) THEN
            CALL ADVTB( PORD(2,N),PORDB(2,NB),SL(2,N),SLB(2,NB),
     &        UL,VL,WL,ULTX,VLTX,WLTX,N,MF )
            CALL SHDP( WLTX,ULTX,VLTX,DISPL(IZN),DISPT(IZN),DPLT )
!            ULTX = (0.5D+0*(UL(1,NSX(N))+UL(1,NSX(N)+1)))**2
!            VLTX = (0.5D+0*(VL(1,NSY(N))+VL(1,NSY(N)+IFLD)))**2
!            WLTX = (WL(1,NQZ))**2
!            ZLT = SQRT(ULTX + VLTX + WLTX)
!            DPLT = (DISPL(IZN)*WLTX + DISPT(IZN)*(ULTX+VLTX))/
!     &        (ZLT+SMALL)
            DPLT = DPLT*SMDEF(IZN,NSL)
          ELSE
            DPLT = 0.D+0
          ENDIF
!
!---  Dirichlet ---
!
          IF( IBCT(NBCT,NB).EQ.1 .OR. IBCT(NBCT,NB).EQ.8 .OR.
     &      IBCT(NBCT,NB).EQ.9 .OR. IBCT(NBCT,NB).EQ.10 .OR.
     &      IBCT(NBCT,NB).EQ.12 ) THEN
            TCOR = (TB(2,NB)+TABS)/TSPRF
            SDFLB = SMDL(NSL)*TCOR*(VISRL/VISLB(2,NB))
            IF( IEDL(NSL).EQ.1 ) THEN
              TCOR = (TB(2,NB)+TABS)/TSPRF
              SDFLB = SMDL(NSL)*TCOR*(VISRL/VISLB(2,NB))
              DLB = TORLB(2,NB)*SVLB*SDFLB
            ELSEIF( IEDL(NSL).EQ.2 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          EXP(SVLB*SDCL(3,IZN,NSL))
            ELSEIF( IEDL(NSL).EQ.3 ) THEN
              DLB = TORLB(2,NB)*SVLB*SMDL(NSL)
            ELSEIF( IEDL(NSL).EQ.4 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          SVLB**SDCL(3,IZN,NSL)
            ENDIF
            INDX = 16
            DLZ = DIFMN(DLP,DLB,DZGF(N),DZGF(N),WL(1,NQZ),INDX)
            DLZ = (DLZ+DPLT)/(5.D-1*DZGF(N))
            IF( ISLC(1).GE.1 .AND. KFLD.GT.1 )  THEN
              AT = DLZ
              AP = DLZ
            ELSE
              AT = MAX( -WL(1,NQZ),ZERO ) +
     &         DLZ*MAX((ONE-(TENTH*ABS(WL(1,NQZ))/(DLZ+SMALL)))**5,ZERO)
              AP = MAX( WL(1,NQZ),ZERO ) +
     &         DLZ*MAX((ONE-(TENTH*ABS(WL(1,NQZ))/(DLZ+SMALL)))**5,ZERO)
            ENDIF
            WC(NQZ,NSL) = WC(NQZ,NSL)+(C(N,NSL)*AP*FCLP-BCX(1)*AT*FCLB)
!
!---  Outflow ---
!
          ELSEIF( IBCT(NBCT,NB).EQ.7 .OR.
     &      ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (WL(1,NQZ)/EPSL.GT.EPSL)) ) THEN
            IF( ISLC(1).GE.1 .AND. KFLD.GT.1 )  THEN
              AP = 0.D+0
            ELSE
              AP = MAX( WL(1,NQZ),ZERO )
            ENDIF
            WC(NQZ,NSL) = WC(NQZ,NSL) + C(N,NSL)*AP*FCLP
!
!---  Inflow ---
!
          ELSEIF( IBCT(NBCT,NB).GE.13 .AND. IBCT(NBCT,NB).LE.16
     &       .OR. ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (WL(1,NQZ)/EPSL.LT.-EPSL)) ) THEN
            IF( ISLC(1).GE.1 .AND. KFLD.GT.1 )  THEN
              AT = 0.D+0
            ELSE
              AT = MAX( -WL(1,NQZ),ZERO )
            ENDIF
            WC(NQZ,NSL) = WC(NQZ,NSL) - BCX(1)*AT*FCLB
          ENDIF
        ENDIF
  200 CONTINUE
!
!---  End of SFXLB group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END


