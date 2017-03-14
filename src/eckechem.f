!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE BCK_STP
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
!     Back step.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 6 June 2006.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE HYST
      USE GRID
      USE FDVS
      USE FDVP
      USE FDVGC
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
      SUB_LOG(ISUB_LOG) = '/BCK_STP'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
      NSTEP = NSTEP-1
!
!---  Water Operational Mode  ---
!
      IF( IOM.EQ.1 ) THEN
        DO 10 N = 1,NFLD
          T(2,N) = T(1,N)
          PL(2,N) = PL(1,N)
          PG(2,N) = PG(1,N)
          SL(2,N) = SL(1,N)
          SGT(2,N) = SGT(1,N)
          ASLMIN(2,N) = ASLMIN(1,N)
          NPHAZ(2,N) = NPHAZ(1,N)
          IPH(2,N) = IPH(1,N)
  10    CONTINUE
!
!---  Water-Air Operational Mode  ---
!
      ELSEIF( IOM.EQ.2 ) THEN
        DO 20 N = 1,NFLD
          T(2,N) = T(1,N)
          PL(2,N) = PL(1,N)
          PG(2,N) = PG(1,N)
          SL(2,N) = SL(1,N)
          SGT(2,N) = SGT(1,N)
          ASLMIN(2,N) = ASLMIN(1,N)
          SG(2,N) = SG(1,N)
          XMLA(2,N) = XMLA(2,N)
          SN(2,N) = SN(1,N)
          XGO(2,N) = XGO(1,N)
          NPHAZ(2,N) = NPHAZ(1,N)
  20    CONTINUE
!
!---  WNE Operational Mode  ---
!
      ELSEIF( IOM.EQ.30 ) THEN
        DO 302 N = 1,NFLD
          T(2,N) = T(1,N)
          PL(2,N) = PL(1,N)
          PG(2,N) = PG(1,N)
          SL(2,N) = SL(1,N)
          SG(2,N) = SG(1,N)
          PVW(2,N) = PVW(1,N)
          PVA(2,N) = PVA(1,N)
          XGA(2,N) = XGA(1,N)
          XGW(2,N) = XGW(1,N)
          XMGA(2,N) = XMGA(1,N)
          XMGW(2,N) = XMGW(1,N)
          XLA(2,N) = XLA(1,N)
          XLW(2,N) = XLW(1,N)
          XMLA(2,N) = XMLA(1,N)
          XMLW(2,N) = XMLW(1,N)
          NPHAZ(2,N) = NPHAZ(1,N)
          DO 300 IGC = 1,NGC
            PVC(IGC,2,N) = PVC(IGC,1,N)
            XGC(IGC,2,N) = XGC(IGC,1,N)
            XMGC(IGC,2,N) = XMGC(IGC,1,N)
            XLC(IGC,2,N) = XLC(IGC,1,N)
            XMLC(IGC,2,N) = XMLC(IGC,1,N)
            DFLC(IGC,2,N) = DFLC(IGC,1,N)
            DFGC(IGC,2,N) = DFGC(IGC,1,N)
            HGC(IGC,2,N) = HGC(IGC,1,N)
 300      CONTINUE
 302    CONTINUE
!
!---  H2O-NaCl-CO2 Operational Mode  ---
!
      ELSEIF( IOM.EQ.32 ) THEN
        DO 320 N = 1,NFLD
          T(2,N) = T(1,N)
          PL(2,N) = PL(1,N)
          PG(2,N) = PG(1,N)
          SL(2,N) = SL(1,N)
          SGT(2,N) = SGT(1,N)
          ASLMIN(2,N) = ASLMIN(1,N)
          SG(2,N) = SG(1,N)
          XLA(2,N) = XLA(1,N)
          YLS(2,N) = YLS(1,N)
          NPHAZ(2,N) = NPHAZ(1,N)
 320    CONTINUE
!
!---  WNSE Operational Mode  ---
!
      ELSEIF( IOM.EQ.40 ) THEN
        DO 402 N = 1,NFLD
          T(2,N) = T(1,N)
          PL(2,N) = PL(1,N)
          PG(2,N) = PG(1,N)
          SL(2,N) = SL(1,N)
          SG(2,N) = SG(1,N)
          PVW(2,N) = PVW(1,N)
          PVA(2,N) = PVA(1,N)
          XGA(2,N) = XGA(1,N)
          XGW(2,N) = XGW(1,N)
          XMGA(2,N) = XMGA(1,N)
          XMGW(2,N) = XMGW(1,N)
          XLA(2,N) = XLA(1,N)
          XLW(2,N) = XLW(1,N)
          XMLA(2,N) = XMLA(1,N)
          XMLW(2,N) = XMLW(1,N)
          YLS(2,N) = YLS(1,N)
          NPHAZ(2,N) = NPHAZ(1,N)
          DO 400 IGC = 1,NGC
            PVC(IGC,2,N) = PVC(IGC,1,N)
            XGC(IGC,2,N) = XGC(IGC,1,N)
            XMGC(IGC,2,N) = XMGC(IGC,1,N)
            XLC(IGC,2,N) = XLC(IGC,1,N)
            XMLC(IGC,2,N) = XMLC(IGC,1,N)
            DFLC(IGC,2,N) = DFLC(IGC,1,N)
            DFGC(IGC,2,N) = DFGC(IGC,1,N)
            HGC(IGC,2,N) = HGC(IGC,1,N)
 400      CONTINUE
 402    CONTINUE
!
!---  EOR Operational Mode  ---
!
      ELSEIF( IOM.EQ.43 ) THEN
        DO 432 N = 1,NFLD
          T(2,N) = T(1,N)
          PL(2,N) = PL(1,N)
          PG(2,N) = PG(1,N)
          PN(2,N) = PN(1,N)
          SL(2,N) = SL(1,N)
          SG(2,N) = SG(1,N)
          SN(2,N) = SN(1,N)
          PVW(2,N) = PVW(1,N)
          PVA(2,N) = PVA(1,N)
          XGA(2,N) = XGA(1,N)
          XGW(2,N) = XGW(1,N)
          XMGA(2,N) = XMGA(1,N)
          XMGW(2,N) = XMGW(1,N)
          XLA(2,N) = XLA(1,N)
          XLW(2,N) = XLW(1,N)
          XMLA(2,N) = XMLA(1,N)
          XMLW(2,N) = XMLW(1,N)
          YLS(2,N) = YLS(1,N)
          NPHAZ(2,N) = NPHAZ(1,N)
          DO 430 IGC = 1,NGC+2
            PVC(IGC,2,N) = PVC(IGC,1,N)
            XGC(IGC,2,N) = XGC(IGC,1,N)
            XMGC(IGC,2,N) = XMGC(IGC,1,N)
            XLC(IGC,2,N) = XLC(IGC,1,N)
            XMLC(IGC,2,N) = XMLC(IGC,1,N)
            TMC(IGC,2,N) = TMC(IGC,1,N)
            XMNC(IGC,2,N) = XMNC(IGC,1,N)
            XNC(IGC,2,N) = XNC(IGC,1,N)
            ZMC(IGC,2,N) = ZMC(IGC,1,N)            
            DFLC(IGC,2,N) = DFLC(IGC,1,N)
            DFGC(IGC,2,N) = DFGC(IGC,1,N)
            DFNC(IGC,2,N) = DFNC(IGC,1,N)
 430      CONTINUE
 432    CONTINUE
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of BCK_STP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE BDOT( ACTVX,SP_CX,DSP_CX,SLX,PORDX,RHOLX,TX,XLWX )
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
!     B-Dot or extended Debye-Huckel model for activity coefficient.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 20 December 2004.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 ACTVX(LSPL+1,LSPL),SP_CX(LSPL),DSP_CX(LSPL)
      REAL*8 CPIX(LSPL+1)
      REAL*8 ACOF(5),BCOF(5),BDCOF(5)
      REAL*8 SACOF(4),SBCOF(4),SCCOF(4)
!
!----------------------Data Statements---------------------------------!
!
!      SAVE ACOF,BCOF,BDCOF,SACOF,SBCOF,SCCOF
!      SAVE ASMX,BSMX,CSMX,TSMI
!      SAVE ACPX,BCPX,BDCPX
      DATA ACOF / 0.49463D+0,0.000398438D+0,9.9092D-6,-4.36244D-8,
     &  1.09811D-10 /
      DATA BCOF / 0.325325D+0,0.000122253D+0,6.68944D-7,-2.57037D-9,
     &  4.89847D-12 /
      DATA BDCOF / 0.0373904D+0,0.000191209D+0,-2.05222D-6,
     &  1.31092D-8,-3.26031D-11 /
      DATA SACOF / 0.131678D+0,-0.000836829D+0,3.07179D-06,
     &  1.46701D-09 /
      DATA SBCOF / -0.0186731D+0,0.00039022D+0,-2.62611D-06,
     &  4.40918D-09 /
      DATA SCCOF / 0.00288841D+0,-6.70405D-05,5.65666D-07,
     &  -1.34012D-09 /
!      DATA TSMI / -1.D+3 /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/BDOT'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!!
!!---  Coefficients as a function of temperature, where
!!     calculations are skipped for sequential calls to
!!     subroutine with the same temperature  ---
!!
!      IF( ABS(TX-TSMI).GT.EPSL ) THEN
        ACPX = ACOF(5)
        BCPX = BCOF(5)
        BDCPX = BDCOF(5)
        DO 10 I = 4,1,-1
          ACPX = ACPX*TX + ACOF(I)
          BCPX = BCPX*TX + BCOF(I)
          BDCPX = BDCPX*TX + BDCOF(I)
   10   CONTINUE
        ASMX = SACOF(4)
        BSMX = SBCOF(4)
        CSMX = SCCOF(4)
        DO 20 I = 3,1,-1
          ASMX = ASMX*TX + SACOF(I)
          BSMX = BSMX*TX + SBCOF(I)
          CSMX = CSMX*TX + SCCOF(I)
   20   CONTINUE
!      ENDIF
!      TSMI = TX
!
!---  Ionic strength of the aqueous solution  ---
!
      DO 40 M = 1,NSPL+1
        CPIX(M) = 0.D+0
        SUM_MZ = 0.D+0
        SUM_M = 0.D+0
        DO 30 NSP = 1,NSPL
!
!---      Skip neutral species  ---
!
          IF( ABS(SP_L(1,NSP)).LT.EPSL ) GOTO 30
          IF( NSP.EQ.(M-1) ) THEN
            CLX = SP_CX(NSP) + DSP_CX(NSP)
          ELSE
            CLX = SP_CX(NSP)
          ENDIF
!
!---      Molarity in mol solute/m^3 aqueous
!         or mol solute/l aqueous  ---
!
          CMX = CLX/(SLX*PORDX)
!
!---      Molality in mol solute/kg water  ---
!
          CMX = CMX/(RHOLX*XLWX)
          SUM_M = SUM_M + CMX
          SUM_MZ = SUM_MZ + CMX*SP_L(1,NSP)
          CPIX(M) = CPIX(M) + CMX*(SP_L(1,NSP)**2)
   30   CONTINUE
!
!---    Correct for electrical neutrality  ---
!
        IF( ABS(SUM_MZ).GT.EPSL )
     &    CPIX(M) = CPIX(M) + (SUM_MZ**2)/SUM_M
        CPIX(M) = CPIX(M)*5.D-1
   40 CONTINUE
!
!---  Activity coefficients for aqueous species  ---
!
      DO 60 M = 1,NSPL+1
        DO 50 NSP = 1,NSPL
          IF( ABS(SP_L(1,NSP)).GT.EPSL ) THEN
!
!---        Activity coefficients for charged species  ---
!
            ACTVX(M,NSP) = -ACPX*(SP_L(1,NSP)**2)*SQRT(CPIX(M))
     &      /(1.D+0 + SP_L(2,NSP)*1.D+10*BCPX*SQRT(CPIX(M)))
     &      + BDCPX*CPIX(M)
            ACTVX(M,NSP) = EXP(TOLN*ACTVX(M,NSP))
!
!---        Activity coefficients for electrically neutral species  ---
!
          ELSE
            ACTVX(M,NSP) = EXP(TOLN*(ASMX*CPIX(M) + BSMX*(CPIX(M)**2) +
     &        CSMX*(CPIX(M)**3)))
          ENDIF
   50   CONTINUE
   60 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of BDOT group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CECHEM( ACTVX,AJM,BJM,CX,SP_CX,N,NEQ,INDX )
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
!     Conservation Equation CHEMistry
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 January 2005.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 AJM(LSPR,LSPR),BJM(LSPR)
      REAL*8 SP_CX(LSPR)
      REAL*8 CX(LEQC+LEQK)
      REAL*8 ACTVX(LSPL+1,LSPL)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/CECHEM'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Volumetric concentration to aqueous concentration,
!     mol/m^3 -> mol/m^3 aqu  ---
!
      VTOLX = 1.D+0/(SL(2,N)*PORD(2,N))
!
!---  Loop over conservation equation species  ---
!
      NEQX = NEQ - NEQE
      BJM(NEQ) = CX(NEQX)*VTOLX
!
!---  Skip for initial pH  ---
!
      IF( ISPLK(1).GT.1000 ) THEN
        IF( NEQ.EQ.IEQ_S(MOD(ISPLK(1),1000)) .AND.
     &    (NSTEP-NRST).EQ.0 ) THEN
          AJM(NEQ,NEQ) = 1.D+0
          BJM(NEQ) = 0.D+0
          GOTO 1000
        ENDIF
      ENDIF
!
!---  Fixed species concentration or activity  ---
!
      DO 100 NSPKX = 1,NSPLK
        IF( ISPLK(14+NSPKX).LT.0 ) THEN
          NSPX = ABS(ISPLK(14+NSPKX))
          IF( NSPX.GT.1000 ) NSPX = NSPX - 1000
          IF( NEQ.EQ.IEQ_S(NSPX) ) THEN
            AJM(NEQ,NEQ) = 1.D+0
            BJM(NEQ) = 0.D+0
            GOTO 1000
          ENDIF
        ENDIF
  100 CONTINUE
      DO 110 M = 1,IEQ_C(1,NEQX)
        NSP = IEQ_C(M+1,NEQX)
        BJM(NEQ) = BJM(NEQ) - EQ_C(M,NEQX)*SP_CX(NSP)*VTOLX
!
!---    Skip for initial pH  ---
!
        IF( ISPLK(1).GT.1000 ) THEN
          IF( NSP.EQ.MOD(ISPLK(1),1000) .AND.
     &      (NSTEP-NRST).EQ.0 ) GOTO 110
        ENDIF
!
!---    Skip fixed species concentration ---
!
        DO 102 NSPKX = 1,NSPLK
          IF( ISPLK(14+NSPKX).LT.0 ) THEN
            NSPX = ABS(ISPLK(14+NSPKX))
            IF( NSP.EQ.NSPX) GOTO 110
          ENDIF
  102   CONTINUE
        AJM(NEQ,IEQ_S(NSP)) = - EQ_C(M,NEQX)*VTOLX
        DO 104 NSPKX = 1,NSPLK
          IF( ISPLK(14+NSPKX).LT.0 ) THEN
            NSPX = ABS(ISPLK(14+NSPKX))
            IF( NSPX .GT. 1000 ) NSPX = NSPX - 1000
            IF( NSP.EQ.NSPX ) AJM(NEQ,IEQ_S(NSP)) =
     &        -1.D+0/ACTVX(1,NSP)*EQ_C(M,NEQX)*VTOLX
          ENDIF
  104   CONTINUE
  110 CONTINUE
!
!---  Return residual vector and Jacobian matrix  ---
!
      IF( ABS(INDX).EQ.1 ) GOTO 1000
      BJM(NEQ) = -BJM(NEQ)
 1000 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of CECHEM group  ---
!
      RETURN
      END
      
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CECHEM_R( ACTVX,AJM,BJM,CX,SP_CX,N,NEQ,INDX )
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
!     Conservation Equation CHEMistry
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 January 2005.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 AJM(LSPR,LSPR),BJM(LSPR)
      REAL*8 SP_CX(LSPR)
      REAL*8 CX(LEQC+LEQK)
      REAL*8 ACTVX(LSPL+1,LSPL)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/CECHEM_R'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Volumetric concentration to aqueous concentration,
!     mol/m^3 -> mol/m^3 aqu  ---
!
      VTOLX = 1.D+0/(SL(2,N)*PORD(2,N))
!
!---  Loop over conservation equation species  ---
!
      NEQX = NEQ - NEQE
      NROW = NEQX
      BJM(NROW) = CX(NEQX)*VTOLX
!
!---  Skip for initial pH  ---
!
      IF( ISPLK(1).GT.1000 ) THEN
        IF( NEQ.EQ.IEQ_S(MOD(ISPLK(1),1000)) .AND.
     &    (NSTEP-NRST).EQ.0 ) THEN
          AJM(NROW,NROW) = 1.D+0
          BJM(NROW) = 0.D+0
          GOTO 1000
        ENDIF
      ENDIF
!
!--- fixed species concentration or activity
!
      DO NSPKX = 1,NSPLK
       IF( ISPLK(14+NSPKX).LT.0 ) THEN
        NSPX = ABS(ISPLK(14+NSPKX))
        IF( NSPX.GT.1000 ) NSPX = NSPX - 1000
        IF( NEQ.EQ.IEQ_S(NSPX) ) THEN
          AJM(NROW,NROW) = 1.D+0
          BJM(NROW) = 0.D+0
          GOTO 1000
        ENDIF
       ENDIF
      ENDDO
!      
      DO 110 M = 1,IEQ_C(1,NEQX)
        NSP = IEQ_C(M+1,NEQX)
        NEQXX = IEQ_S(NSP)
        BJM(NROW) = BJM(NROW) - EQ_C(M,NEQX)*SP_CX(NSP)*VTOLX
!        DO NSPKX = 1,NSPLK
!          IF(ISPLK(14+NSPKX).LT.-1000) THEN
!            NSPXX = ABS(ISPLK(14+NSPKX))-1000
!            IF(NSP.EQ.NSPXX) 
!     &      BJM(NEQX) = BJM(NEQ)+EQ_C(M,NEQX)*SP_CX(NSP)*VTOLX
!     &              - EQ_C(M,NEQX)*SP_CX(NSP)/ACTVX(1,NSP)*VTOLX
!          ENDIF
!        ENDDO
        IF( NEQXX.GT.NEQE ) THEN
!
!---    Skip for initial pH  ---
!
         IF( ISPLK(1).GT.1000 ) THEN
          IF( NSP.EQ.MOD(ISPLK(1),1000) .AND.
     &      (NSTEP-NRST).EQ.0 ) GOTO 110
         ENDIF
!
!---   Skip fixed species concentration
!
         DO NSPKX = 1,NSPLK
          IF( ISPLK(14+NSPKX).LT.0 ) THEN
           NSPX = ABS(ISPLK(14+NSPKX))
           IF( NSP.EQ.NSPX) GOTO 110
          ENDIF
         ENDDO
         NCOL = NEQXX-NEQE
         AJM(NROW,NCOL) = AJM(NROW,NCOL)- EQ_C(M,NEQX)*VTOLX
         DO NSPKX = 1,NSPLK
          IF( ISPLK(14+NSPKX).LT.0 ) THEN
           NSPX = ABS(ISPLK(14+NSPKX))
           IF( NSPX .GT. 1000 ) NSPX = NSPX - 1000
           IF( NSP.EQ.NSPX ) AJM(NROW,NCOL) = AJM(NROW,NCOL)-
     &       1.D0/ACTVX(1,NSP)*EQ_C(M,NEQX)*VTOLX
          ENDIF
         ENDDO
        ELSE
!
!---   Equilibrium mass action
!
          NSE = IEQ_E(1,NEQXX)
!
!---  Loop over equilibrium species  ---
!
          DO MSX = 2,NSE
            NCM_SP = IEQ_E(MSX+1,NEQXX)
            NCOL = IEQ_S(NCM_SP)-NEQE
            AJM(NROW,NCOL) = AJM(NROW,NCOL)-EQ_C(M,NEQX)*VTOLX*
     &        EQ_E(MSX-1,NEQXX)*SP_CX(NSP)/SP_CX(NCM_SP)
          ENDDO
        ENDIF

  110 CONTINUE
!
!---  Return residual vector and Jacobian matrix  ---
!
      IF( ABS(INDX).EQ.1 ) GOTO 1000
      BJM(NROW) = -BJM(NROW)
 1000 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of CECHEM_R group  ---
!
      RETURN
      END
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE DAVIES( ACTVX,SP_CX,DSP_CX,SLX,PORDX,RHOLX,TX,XLWX )
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
!     B-Dot or extended Debye-Huckel model for activity coefficient.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 20 December 2004.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 ACTVX(LSPL+1,LSPL),SP_CX(LSPL),DSP_CX(LSPL)
      REAL*8 CPIX(LSPL+1)
      REAL*8 ACOF(5),BCOF(5),BDCOF(5)
      REAL*8 SACOF(4),SBCOF(4),SCCOF(4)
!
!----------------------Data Statements---------------------------------!
!
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/DAVIES'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Ionic strength of the aqueous solution  ---
!
      DO 40 M = 1,NSPL+1
        CPIX(M) = 0.D+0
        SUM_MZ = 0.D+0
        SUM_M = 0.D+0
        DO 30 NSP = 1,NSPL
!
!---      Skip neutral species  ---
!
          IF( ABS(SP_L(1,NSP)).LT.EPSL ) GOTO 30
          IF( NSP.EQ.(M-1) ) THEN
            CLX = SP_CX(NSP) + DSP_CX(NSP)
          ELSE
            CLX = SP_CX(NSP)
          ENDIF
!
!---      Molarity in mol solute/m^3 aqueous
!         or mol solute/l aqueous  ---
!
          CMX = CLX/(SLX*PORDX)
!
!---      Molality in mol solute/kg water  ---
!
          CMX = CMX/(RHOLX*XLWX)
          SUM_M = SUM_M + CMX
          SUM_MZ = SUM_MZ + CMX*SP_L(1,NSP)
          CPIX(M) = CPIX(M) + CMX*(SP_L(1,NSP)**2)
   30   CONTINUE
!
!---    Correct for electrical neutrality  ---
!
        CPIX(M) = CPIX(M)+DABS(SUM_MZ)
        CPIX(M) = CPIX(M)*5.D-1
   40 CONTINUE
!
!---  Activity coefficients for aqueous species  ---
!
      DO 60 M = 1,NSPL+1
        DO 50 NSP = 1,NSPL
!
!---      Activity coefficients for charged species  ---
!
          IF( ABS(SP_L(1,NSP)).GT.EPSL ) THEN
            ACTVX(M,NSP) = 5.D-1*(SP_L(1,NSP)**2)*(SQRT(CPIX(M))
     &        /(1.D+0 + SQRT(CPIX(M)))- 2.4D-1*CPIX(M))
            IF(ACTVX(M,NSP) > 1.D+1) ACTVX(M,NSP) = 0.D+0
            ACTVX(M,NSP) = 1.D+1**(-ACTVX(M,NSP))
!
!---      Activity coefficients for electrically neutral species  ---
!
          ELSE
            ACTVX(M,NSP) = 1.D+0
          ENDIF
   50   CONTINUE
   60 CONTINUE
!      DO 70 IC = 1,6      
!        ACTV16(IC) = 5.D-1*(REAL(IC)**2)*(SQRT(CPIX(1))
!     &    /(1.D+0 + SQRT(CPIX(1)))- 2.4D-1*CPIX(1))
!        IF(ACTV16(IC) > 1.D+1) ACTV16(IC) = 0.D+0
!        ACTV16(IC) = 1.D+1**(-ACTV16(IC))
!   70 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of DAVIES group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE DGELG(R,A,MXDOF,M,EPS,IER)
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Full pivoting from BIOGEOCHEM
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, December 16, 2005.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
      DIMENSION A(MXDOF,MXDOF),R(MXDOF),IPIV(MXDOF)
!
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
      SUB_LOG(ISUB_LOG) = '/DGELG'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
      IF(M) 23,23,1
!
!     SEARCH FOR THE GREATEST ELEMENT IN MATRIX A
!
    1 IER=0
      PIV=0.0D0
      DO 3 LI=1,M
      DO 3 LJ=1,M
      TBX=DABS(A(LI,LJ))
      IF(TBX-PIV) 3,3,2
    2 PIV=TBX
      I=LI
      J=LJ
    3 CONTINUE
      TOL=EPS*PIV
!     A(I,J) IS PIVOT ELEMENT. PIV CONTAINS THE ABSOLUTE VALUE OF A(I,J)
!     START ELIMINATION LOOP
      LST=1
      DO 17 K=1,M
!
!     TEST ON SINGULARITY
!
      IF(PIV) 23,23,4
    4 IF(IER) 7,5,7
    5 IF(PIV-TOL) 6,6,7
    6 IER=K-1
    7 PIVI=1.0D0/A(I,J)
      I=I-K
      J=J-K
!     I+K IS ROW-INDEX, J+K COLUMN-INDEX OF PIVOT ELEMENT
!
!     PIVOT ROW REDUCTION AND ROW INTERCHANGE IN RIGHT HAND SIDE R
      KK=K+I
      TBX=PIVI*R(KK)
      R(KK)=R(K)
      R(K)=TBX
!
!     IS ELIMINATION TERMINATED?
      IF(K-M) 9,18,18
!     COLUMN INTERCHANGE IN MATRIX A
    9 CONTINUE
      IF(J) 12,12,10
   10 KK=J+K
      DO 11 L=K,M
      TBX=A(L,K)
      A(L,K)=A(L,KK)
   11 A(L,KK)=TBX
!     ROW INTERCHANGE AND PIVOT ROW REDUCTION IN MATRIX A
   12 KK=K+I
      DO 13 L=K,M
      TBX=PIVI*A(KK,L)
      IF(L.EQ.K) TBX=A(KK,L)
      A(KK,L)=A(K,L)
   13 A(K,L)=TBX
!     SAVE COLUMN INTERCHANGE INFORMATION
      IPIV(K)=J
!     ELEMENT REDUCTION AND NEXT PIVOT SEARCH
      PIV=0.0D0
      LST=K+1
      DO 16 LI=LST,M
      PIVI=-A(LI,K)
      DO 15 LJ=LST,M
      A(LI,LJ)=A(LI,LJ)+PIVI*A(K,LJ)
      TBX=DABS(A(LI,LJ))
      IF(TBX-PIV) 15,15,14
   14 PIV=TBX
      I=LI
      J=LJ
   15 CONTINUE
      R(LI)=R(LI)+PIVI*R(K)
   16 CONTINUE
   17 CONTINUE
!     END OF ELIMINATION
!     BACK SUBSTITUTION AND BACK INTERCHANGE
   18 IF(M-1) 23,22,19
   19 LST=M+1
      DO 21 I=2,M
      II=LST-I
      L=IPIV(II)
      III=II+1
      TBX=R(II)
      DO 20 K=III,M
      TBX=TBX-A(II,K)*R(K)
   20 CONTINUE
      K=II+L
      R(II)=R(K)
      R(K)=TBX
   21 CONTINUE
   22 CONTINUE
      ISUB_LOG = ISUB_LOG-1
      RETURN
!     ERROR RETURN
   23 IER=-1
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ECKECHEM
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
!     Equilibrium, Conservation, and Kinetic Equation CHEMistry.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 20 December 2004.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE REACT
      USE PORMED
      USE GRID
      USE FILES
      USE FDVP
      USE CONST
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



!
!----------------------Type Declarations-------------------------------!
!
!     REAL*8 AJM(LSPR,LSPR),BJM(LSPR)
      REAL*8 SP_CX(LSPR),SP_CXX(LSPR)
      REAL*8 CX(LEQC+LEQK),COX(LEQC+LEQK)
!     REAL*8 DCLX(LSPL)
      REAL*8 CLX(LSPL),ACTVX(LSPL+1,LSPL)
!     REAL*8 VFX(LSPS+1)
!     INTEGER IJM(LSPR)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ECKECHEM'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
      ECKE_ER = .FALSE.
!
!---  Total number of equations  ---
!
      NEQR = NEQE + NEQK + NEQC
!
!---  Total number species  ---
!
      NSPR = NSPG + NSPL + NSPN + NSPS + NSPE
!
!---  Loop over all nodes, skipping inactive nodes  ---
!
!$OMP PARALLEL DO
!$OMP&  DEFAULT(NONE)
!$OMP&  COPYIN(ISUB_LOG)
!$OMP&  SHARED(C,CMIN,CO,DTI,ECKE_ER,EPSL,EQ_C,EQ_K,GCPP,ID,IEQ_C,
!$OMP&    IEQ_K,IOM,ISLC,ISP_MN,ISPLK,IXP,IZ,JD,KD,NEQC,NEQE,NEQK,
!$OMP&    NFLD,NGC,NRST,NSD,NSOLU,NSPE,NSPG,NSPL,NSPLK,NSPN,NSPR,NSPS,
!$OMP&    NSTEP,POR,POR0,RS_S,SP_C,SP_CMN,SP_CO,SP_S,SRCA,SRCGC,VOL,
!$OMP&    WTMA)
!$OMP&  FIRSTPRIVATE(NEQR)
!$OMP&  PRIVATE(ACTVX,CMBX,COX,CX,IGC,ISHIFTX,M,N,NEQ,NSL,
!$OMP&    NSP,NSP_M,NSPKX,NSPX,SP_COX,SP_CX,SP_CXX,SPI_COX,SPI_CX,
!$OMP&    SRCAX,SRCGCX)
      DO 300 N = 1,NFLD
        IF( ECKE_ER ) GOTO 300
        IF( IXP(N).EQ.0 ) GOTO 300
!        IGUSPCN = 0
  100   CONTINUE
!
!---    Assign local values of old-time-step species concentration  ---
!
        DO 110 NSP = 1,NSPR
          IF ( ISLC(57).EQ.0) THEN
            SP_CX(NSP) = SP_C(N,NSP)
          ELSE
            SP_CX(NSP) = MAX(SP_C(N,NSP),CMIN)
          ENDIF
  110   CONTINUE
!
!---    Assign local values of component species concentration  ---
!
        DO 120 NSP = 1,NEQC
          NSL = NSP + NSOLU
          CX(NSP) = C(N,NSL)
          COX(NSP) = C(N,NSL)
  120   CONTINUE
!
!---    Assign local values of kinetic species concentration  ---
!
        DO 130 NSP = 1,NEQK
          NSPX = NSP + NEQC
          NSL = NSPX + NSOLU
          CX(NSPX) = C(N,NSL)
          COX(NSPX) = C(N,NSL)
  130   CONTINUE
!
!---    Guess specie concentrations, assuming activity
!       coefficients of 1.0  ---
!
        IF( ISLC(42).EQ.1 .AND. (NSTEP-NRST).EQ.0 ) THEN
!
!---      Check if neighboring cell has identical
!         conservation component species concentrations  ---
!
          IF( N.GT.1 .AND. NFLD.GT.1 ) THEN
            DO 140 NSL = NSOLU+1,NSOLU+NEQC
              IF( ABS(CO(N-1,NSL)-C(N,NSL))/EPSL.GT.EPSL ) GOTO 170
  140       CONTINUE
!
!---        Skip guess calculation, use neighbor-cell species
!           concentrations  ---
!
            DO 150 NSP = 1,NSPR
              SP_CX(NSP) = SP_C(N-1,NSP)
  150       CONTINUE
            GOTO 180
          ENDIF
  170     CONTINUE
          CALL GUSPCN( CX,SP_CX,N )
        ENDIF
  180   CONTINUE
!
!---    Conventional Newton method, Global solve  ---
!
        IF (ISLC(57).EQ.0) THEN
          CALL ECKECN( CX,COX,SP_CX,N )
!
!---    Reduced Equations  ---
!
        ELSE
          CALL ECKECN_R( CX,COX,SP_CX,N )
        ENDIF
!
!---    Convergence failure, store failed node number
!---    but complete loop  ---
!
        IF( ECKE_ER ) THEN
          NSD(1) = N
          GOTO 300
        ENDIF
!
!---   Fixed species activity
!
        DO 195 NSP = 1,NSPR
          SP_CXX(NSP) = SP_CX(NSP)
          DO 190 NSPKX = 1,NSPLK
           IF( ISPLK(14+NSPKX).LT.-1000 ) THEN
            NSPX = ABS(ISPLK(14+NSPKX))-1000
            IF(NSP.EQ.NSPX) SP_CX(NSP) = SP_CX(NSP)/ACTVX(1,NSP)
           ENDIF
  190     CONTINUE
  195   CONTINUE
!
!---    Conservation-component species, mobile conservation-component
!       species, and old time step mobile conservation-component
!       species  ---
!
        DO 210 NEQ = 1,NEQC
          NSL = NSOLU + NEQ
          C(N,NSL) = 0.D+0
          CMBX = 0.D+0
!
!---      Loop over conservation-component species  ---
!
          DO 200 M = 1,IEQ_C(1,NEQ)
            NSP = IEQ_C(M+1,NEQ)
            IF( ABS(SP_CX(NSP)).LT.CMIN ) THEN
              SP_CX(NSP) = 0.D+0
            ENDIF
            C(N,NSL) = C(N,NSL) + EQ_C(M,NEQ)*SP_CX(NSP)
!
!---        Mobile species ---
!
            IF( NSP.LE.NSPL .OR. NSP.GT.(NSPL+NSPS+NSPE) )
     &        CMBX = CMBX + EQ_C(M,NEQ)*SP_CX(NSP)
!
!---        Immobile species ---
!
            IF( NSP.GT.NSPL .AND. NSP.LE.(NSPL+NSPS+NSPE) ) THEN
              IF( ABS(SP_CO(N,NSP)).LT.CMIN ) THEN
                SP_COX = 0.D+0
              ELSE
                SP_COX = SP_CO(N,NSP)
              ENDIF
              SPI_CX = EQ_C(M,NEQ)*SP_CX(NSP)
              SPI_COX = EQ_C(M,NEQ)*SP_COX
!
!---          Load CO2 sources for linked aqueous CO2,
!             skip for initial conditions  ---
!
              IF( ISPLK(6).EQ.NSL .AND. (NSTEP-NRST).GT.0 ) THEN
                SRCAX = -1.D-3*(SPI_CX-SPI_COX)*VOL(N)*WTMA
                IF( ABS(SRCAX).LT.1.D-16 ) SRCAX = 0.D+0
                SRCA(1,N) = SRCA(1,N) + SRCAX
              ENDIF
!
!---          Load component sources for linked aqueous component,
!             skip for initial conditions  ---
!
              ISHIFTX=0
              IF( IOM.EQ.43 ) ISHIFTX=2
                DO IGC = 1,NGC+ISHIFTX
                  IF( ISPLK(14+NSPLK+IGC).EQ.NSL .AND. 
     &              (NSTEP-NRST).GT.0 ) THEN
                    SRCGCX = -1.D-3*(SPI_CX-SPI_COX)*VOL(N)*GCPP(1,IGC)
                    IF( ABS(SRCGCX).LT.1.D-16 ) SRCGCX = 0.D+0
                    SRCGC(IGC,1,N) = SRCGC(IGC,1,N) + SRCGCX
                  ENDIF
                END DO
!
!---          Load component sources for linked gas component,
!             skip for initial conditions  ---
!
              ISHIFTX=0
              IF( IOM.EQ.43 ) ISHIFTX=2
                DO IGC = 1,NGC+ISHIFTX
                IF( ISPLK(14+NSPLK+NGC+ISHIFTX+IGC).EQ.NSL .AND. 
     &            (NSTEP-NRST).GT.0 ) THEN
                  SRCGCX = -1.D-3*(SPI_CX-SPI_COX)*VOL(N)*GCPP(1,IGC)
                  IF( ABS(SRCGCX).LT.1.D-16 ) SRCGCX = 0.D+0
                  SRCGC(IGC,1,N) = SRCGC(IGC,1,N) + SRCGCX
                ENDIF
                END DO
!
!---          Load air sources for linked aqueous air,
!             skip for initial conditions  ---
!
              IF( ISPLK(4).EQ.NSL .AND. (NSTEP-NRST).GT.0 ) THEN
                SRCAX = -1.D-3*(SPI_CX-SPI_COX)*VOL(N)*WTMA
                IF( ABS(SRCAX).LT.1.D-16 ) SRCAX = 0.D+0
                SRCA(1,N) = SRCA(1,N) + SRCAX
              ENDIF
            ENDIF
  200     CONTINUE
  210   CONTINUE
!
!---    Kinetic-component species, mobile kinetic-component species
!       and old time step mobile kinetic-component species  ---
!
        DO 230 NEQ = 1,NEQK
          NSL = NSOLU + NEQC + NEQ
          C(N,NSL) = 0.D+0
          CMBX = 0.D+0
!
!---      Loop over kinetic-component species  ---
!
          DO 220 M = 1,IEQ_K(1,NEQ)
            NSP = IEQ_K(M+1,NEQ)
            C(N,NSL) = C(N,NSL) + EQ_K(M,NEQ)*SP_CX(NSP)
!
!---        Mobile species ---
!
            IF( NSP.LE.NSPL .OR. NSP.GT.(NSPL+NSPS+NSPE) )
     &        CMBX = CMBX + EQ_K(M,NEQ)*SP_CX(NSP)
!
!---        Immobile species ---
!
            IF( NSP.GT.NSPL .AND. NSP.LE.(NSPL+NSPS+NSPE) ) THEN
              IF( ABS(SP_CO(N,NSP)).LT.CMIN ) THEN
                SP_COX = 0.D+0
              ELSE
                SP_COX = SP_CO(N,NSP)
              ENDIF
              SPI_CX = EQ_K(M,NEQ)*SP_CX(NSP)
              SPI_COX = EQ_K(M,NEQ)*SP_COX
!
!---          Load CO2 sources for linked aqueous CO2,
!             skip for initial conditions  ---
!
              IF( ISPLK(6).EQ.NSL .AND. (NSTEP-NRST).GT.0 ) THEN
                SRCAX = -1.D-3*(SPI_CX-SPI_COX)*VOL(N)*WTMA*DTI
                IF( ABS(SRCAX).LT.1.D-16 ) SRCAX = 0.D+0
                SRCA(1,N) = SRCA(1,N) + SRCAX
              ENDIF
!
!---          Load gas component sources for linked aqueous component,
!             skip for initial conditions  ---
!
              ISHIFTX=0
              IF( IOM.EQ.43 ) ISHIFTX=2
              DO IGC = 1,NGC+ISHIFTX
                IF( ISPLK(14+NSPLK+IGC).EQ.NSL .AND. 
     &            (NSTEP-NRST).GT.0 ) THEN
                  SRCGCX = -1.D-3*(SPI_CX-SPI_COX)*VOL(N)*GCPP(1,IGC)
     &              *DTI
                  IF( ABS(SRCGCX).LT.1.D-16 ) SRCGCX = 0.D+0
                  SRCGC(IGC,1,N) = SRCGC(IGC,1,N) + SRCGCX
                ENDIF
              END DO
!
!---          Load gas component sources for linked gaseous component,
!             skip for initial conditions  ---
!
              ISHIFTX=0
              IF( IOM.EQ.43 ) ISHIFTX=2
              DO IGC = 1,NGC+ISHIFTX
                IF( ISPLK(14+NSPLK+NGC+ISHIFTX+IGC).EQ.NSL .AND. 
     &            (NSTEP-NRST).GT.0 ) THEN
                  SRCGCX = -1.D-3*(SPI_CX-SPI_COX)*VOL(N)*GCPP(1,IGC)
     &              *DTI
                  IF( ABS(SRCGCX).LT.1.D-16 ) SRCGCX = 0.D+0
                  SRCGC(IGC,1,N) = SRCGC(IGC,1,N) + SRCGCX
                ENDIF
              END DO
!
!---          Load air sources for linked aqueous air,
!             skip for initial conditions  ---
!
              IF( ISPLK(4).EQ.NSL .AND. (NSTEP-NRST).GT.0 ) THEN
                SRCAX = -1.D-3*(SPI_CX-SPI_COX)*VOL(N)*WTMA*DTI
                IF( ABS(SRCAX).LT.1.D-16 ) SRCAX = 0.D+0
                SRCA(1,N) = SRCA(1,N) + SRCAX
              ENDIF
            ENDIF
  220     CONTINUE
  230   CONTINUE
!
!---    Assign global species concentrations  ---
!
        DO 240 NSP = 1,NSPR
          IF( ABS(SP_CX(NSP)).LT.CMIN ) THEN
            SP_C(N,NSP) = 0.D+0
          ELSE
            SP_C(N,NSP) = SP_CX(NSP)
          ENDIF
!
!---   Fixed species activity
!
          DO 235 NSPKX = 1,NSPLK
           IF( ISPLK(14+NSPKX).LT.-1000 ) THEN
            NSPX = ABS(ISPLK(14+NSPKX))-1000
            SP_C(N,NSP) = SP_CXX(NSP)
           ENDIF
  235     CONTINUE
  240   CONTINUE
!
!---    Loop over solid species to determine current mineral
!       volume fractions and porosity
!       POR0(1,N) total porosity
!       POR0(2,N) diffusive porosity
!       POR0(3,N) total unreactive mineral volume   ---
!
        POR0(1,N) = 1.D+0 - POR0(3,N)
        POR0(2,N) = 1.D+0 - POR0(3,N) - POR(2,IZ(N)) + POR(1,IZ(N))
        DO 250 NSP = NSPL+1,NSPL+NSPS
          NSP_M = NSP-NSPL
!
!---      Minerals ---
!
          IF( ISP_MN(NSP).EQ.1 ) THEN
            IF( SP_CX(NSP)+SP_CMN(N,NSP_M).LT.CMIN ) THEN
              RS_S(3,NSP_M,N) = 0.D+0
            ELSE
              RS_S(3,NSP_M,N) = 1.D-3*(SP_CX(NSP)+SP_CMN(N,NSP_M))*
     &          SP_S(2,NSP_M)/SP_S(1,NSP_M)
            ENDIF
!
!---      Non minerals ---
!
          ELSE
            IF( SP_CX(NSP).GT.CMIN ) THEN
              RS_S(3,NSP_M,N) = 1.D-3*SP_CX(NSP)*SP_S(2,NSP_M)/
     &          SP_S(1,NSP_M)
            ELSE
              RS_S(3,NSP_M,N) = 0.D+0
            ENDIF
          ENDIF
          RS_S(3,NSP_M,N) = MIN(RS_S(3,NSP_M,N),1.D+0)
          POR0(1,N) = POR0(1,N) - RS_S(3,NSP_M,N)
          POR0(2,N) = POR0(2,N) - RS_S(3,NSP_M,N)
          POR0(1,N) = MAX(POR0(1,N),1.D-12)
          POR0(2,N) = MAX(POR0(2,N),1.D-12)
  250   CONTINUE
  300 CONTINUE
!$OMP END PARALLEL DO
!
!---  Convergence failure, double the number of
!     sub-time steps  ---
!
      IF( ECKE_ER ) THEN
        N_RST = 2*N_RST
        REALX = REAL(N_RST)
        DT = DT_RST/REALX
        DTI = 1.D+0/(DT+EPSL)
        TM = TM_RST - DT_RST
        WRITE(ISC,'(A,I6,A)')
     &    '          ---  ECKEChem Convergence Failure, Node = '
     &    ,NSD(1),'  ---'
        WRITE(ISC,'(A,I6,A)')
     &    '          ---  ECKEChem Sub-Time Stepping Factor = '
     &    ,N_RST,'  ---'
        WRITE(IWR,'(A,I6,A)')
     &    '          ---  ECKEChem Convergence Failure, Node = '
     &    ,NSD(1),'  ---'
        WRITE(IWR,'(A,I6,A)')
     &    '          ---  ECKEChem Sub-Time Stepping Factor = '
     &    ,N_RST,'  ---'
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ECKECHEM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ECKECN( CX,COX,SP_CX,N )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Conventional Newton scheme for solving the nonlinear
!     equilibrium, conservation, and kinetic chemistry equations.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, January 26, 2006.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 AJM(LSPR,LSPR),BJM(LSPR)
      REAL*8 AJMC(LSPR,LSPR),BJMC(LSPR)
      REAL*8 SP_CX(*)
      REAL*8 CX(*),COX(*)
      REAL*8 DCLX(LSPL),CLX(LSPL),ACTVX(LSPL+1,LSPL)
      REAL*8 VFX(LSPS+1)
      INTEGER IJM(LSPR)
      CHARACTER*64 GETSPNM
      EXTERNAL GETSPNM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ECKECN'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Total number of equations  ---
!
      NEQR = NSPR
      RLXF = 1.D+0
!
!--- Fixed species activity ---
!
      IF( (NSTEP-NRST).EQ.0 ) THEN
        DO 10 NSPKX = 1,NSPLK
          IF( ISPLK(14+NSPKX).LT.0 ) THEN
            NSPX = ABS(ISPLK(14+NSPKX))
            IF( NSPX.GT.1000 ) THEN
              NSPX = NSPX - 1000
              FACTV(NSPX) = SP_CX(NSPX)
            ENDIF
          ENDIF
   10   CONTINUE
      ENDIF
!
!---  For initial conditions, skip kinetic equations  ---
!
      IF( (NSTEP-NRST).EQ.0 ) NEQR = NEQE+NEQC
  100 CONTINUE
!!
!!---  Guess specie concentrations, assuming activity
!!     coefficients of 1.0  ---
!!
!      IF( IGUSPCN.EQ.1 ) THEN
!!
!!---    Assign local values of old-time-step species concentration  ---
!!
!        DO 110 NSP = 1,NSPR
!          IF( ABS(SP_C(N,NSP)).LT.CMIN ) THEN
!            SP_CX(NSP) = 0.D+0
!          ELSE
!            SP_CX(NSP) = SP_C(N,NSP)
!          ENDIF
!  110   CONTINUE
!!
!!---    Assign local values of component species concentration  ---
!!
!        DO 120 NSP = 1,NEQC
!          NSL = NSP + NSOLU
!          CX(NSP) = C(N,NSL)
!          COX(NSP) = C(N,NSL)
!  120   CONTINUE
!!
!!---    Assign local values of kinetic species concentration  ---
!!
!        DO 140 NSP = 1,NEQK
!          NSPX = NSP + NEQC
!          NSL = NSPX + NSOLU
!          CX(NSPX) = C(N,NSL)
!          COX(NSPX) = C(N,NSL)
!  140   CONTINUE
!        CALL GUSPCN( CX,SP_CX,N )
!      ENDIF
!
!---  Top of Newton-Raphson loop  ---
!
      NC = 0
  220 CONTINUE
      NC = NC + 1
!
!---  Set index to compute both the Jacobian matrix
!     and residual vector  ---
!
      INDX = 0
      CALL ECKEJCB( ACTVX,AJM,BJM,CX,COX,SP_CX,N,INDX,IER )
!!
!!---  Matrix-problem vector copy  ---
!!
!      DO 320 L = 1,NEQR
!        DO 310 M = 1,NEQR
!          AJMC(L,M) = AJM(L,M)
!  310   CONTINUE
!        BJMC(L) = BJM(L)
!  320 CONTINUE
!
!---  Solve linear system using LU Decomposition  ---
!
!      CALL LUDCMP( AJM,NEQR,LSPR,IJM,DJM )
!      CALL LUBKSB( AJM,NEQR,LSPR,IJM,BJM )
!      CALL MPROVE( AJMC,AJM,NEQR,LSPR,IJM,BJMC,BJM )
!      CALL DGELG(BJMC,AJMC,LSPR,NEQR,EPSL,IER)
      CALL GAUSSJ( AJM,BJM,NEQR,LSPR )
      IF( IER.EQ.-1 ) GOTO 350
!
!---  Maximum residual  ---
!
      RSDMX = 1.D-20
      DO 330 NSP = 1,NSPR
        IF( ABS(SP_CX(NSP))/EPSL.GT.EPSL .OR.
     &    ABS(BJM(IEQ_S(NSP)))/EPSL.GT.EPSL ) THEN
          SP_CMX = MAX( ABS(SP_CX(NSP)),ABS(SP_C(N,NSP)),CMIN )
          IF( ABS(SP_CMX).GT.CMIN ) THEN
            RSDX = ABS(BJM(IEQ_S(NSP)))/SP_CMX
          ELSE
            RSDX = 0.D+0
          ENDIF
          IF( INDEX(GETSPNM(NSP),'fix').NE.0 ) THEN
            RSDX = 0.D+0
          ENDIF
          IF( RSDX.GT.RSDMX ) THEN
            IF( ABS(SP_CX(NSP)).GT.1.D-16 ) THEN
              RSDMX = RSDX
              NSPMX = NSP
            ENDIF
          ENDIF
        ENDIF
  330 CONTINUE
!
!---  Relaxation for high maximum residuals  ---
!
      IF( RSDMX.GT.1.D+4 ) THEN
        RLXF = 1.D-1
      ELSEIF( RSDMX.GT.1.D+2 ) THEN
        RLXF = 6.D-1
      ELSE
        RLXF = 1.D+0
      ENDIF
      IF( NC.GT.16 ) RLXF = MIN( 6.D-1,RLXF )
!
!---  Update the species concentrations, mol/m^3  ---
!
      DO 332 NSP = 1,NSPR
!
!---    Relaxation/Check for NaNs  ---
!
        CCX = RLXF*BJM(IEQ_S(NSP))
        IF( CCX.NE.CCX )THEN
          IER = -1
          GOTO 350
        ENDIF
        IF( CCX.LT.0.D+0 ) THEN
!
!---      Mineral species  ---
!
          IF( ISP_MN(NSP).EQ.1 ) THEN
            NSP_MN = NSP - NSPL
            SP_CCX = SP_CMN(N,NSP_MN)+SP_CX(NSP)
            CCX = SIGN( MIN( SP_CCX,ABS(CCX) ),CCX )
!
!---      Immobile species  ---
!
          ELSEIF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS+NSPE ) THEN
            SP_CCX = SP_CX(NSP)
            CCX = SIGN( MIN( SP_CCX,ABS(CCX) ),CCX )
!
!---      Mobile species  ---
!
          ELSE
            SP_CCX = 0.9999D+0*SP_CX(NSP)
            CCX = SIGN( MIN( SP_CCX,ABS(CCX) ),CCX )
          ENDIF
        ENDIF
!
!---    Update mineral species concentration  ---
!
        IF( ISP_MN(NSP).EQ.1 ) THEN
          SP_CX(NSP) = SP_CX(NSP)+CCX
          IF( ABS(SP_CX(NSP)).LT.CMIN ) SP_CX(NSP) = 0.D+0
!
!---    Update non-mineral species concentration  ---
!
        ELSE
!
!---    Fix species concentration
!
          IF( INDEX(GETSPNM(NSP),'fix').NE.0 ) THEN
            CCX = 0.D+0
            IF ( NSTEP-NRST.GT.0 ) THEN
!
!---          Convert reactive species from node volumetric, kmol/m^3
!             to node volumetric, mol/m^3  ---
!
              IF( MOD(IC_SP(N,NSP),10).EQ.1 ) THEN
                SP_CX(NSP) = 1.D+3*SP_CI(N,NSP)
!
!---          Convert reactive species from aqueous volumetric, kmol/m^3
!             to node volumetric, mol/m^3  ---
!
              ELSEIF( MOD(IC_SP(N,NSP),10).EQ.2 ) THEN
                SP_CX(NSP) = 1.D+3*SP_CI(N,NSP)*SL(2,N)*PORD(2,N)
!
!---          Convert reactive species from aqueous molal, kmol/kg water
!             to node volumetric, mol/m^3  ---
!
              ELSEIF( MOD(IC_SP(N,NSP),10).EQ.3 ) THEN
                SP_CX(NSP) = 1.D+3*SP_CI(N,NSP)*SL(2,N)*PORD(2,N)*
     &            RHOL(2,N)*XLW(2,N)
!
!---          Convert reactive species from gas volumetric, kmol/m^3
!             to node volumetric, mol/m^3  ---
!
              ELSEIF( MOD(IC_SP(N,NSP),10).EQ.4 ) THEN
                SP_CX(NSP) = 1.D+3*SP_CI(N,NSP)*SG(2,N)*PORD(2,N)
              ENDIF
            ENDIF
          ENDIF
          SP_CX(NSP) = SP_CX(NSP)+CCX
          IF( ABS(SP_CX(NSP)).LT.CMIN ) SP_CX(NSP) = 0.D+0
        ENDIF
  332 CONTINUE
!
!--- Fixed species activity  ---
!
      DO 340 NSPKX = 1,NSPLK
        IF( ISPLK(14+NSPKX).LT.0 ) THEN
          NSPX = ABS(ISPLK(14+NSPKX))
          IF( NSPX.GT.1000 ) THEN
            NSPX = NSPX - 1000
            IF( IACTV.EQ.3 ) THEN
             ACTVX(1,NSP) = ACTVC
            ELSEIF( IACTV.EQ.1 ) THEN
             CALL DAVIES( ACTVX,CLX,DCLX,SL(2,N),PORD(2,N),
     &        RHOL(2,N),T(2,N),XLW(2,N) )
            ELSE
             CALL BDOT( ACTVX,CLX,DCLX,SL(2,N),PORD(2,N),
     &         RHOL(2,N),T(2,N),XLW(2,N) )
            ENDIF
            SP_CX(NSPX) = FACTV(NSPX)/ACTVX(1,NSPX)
          ENDIF
        ENDIF
  340 CONTINUE
!
!---  Unconverged species concentration exit or repeat
!     Newton-Raphson loop  ---
!
!      if( n.eq.42 ) then
!        do nsp = 1,nspr
!          print '(a,i2,a,1pe12.5)','sp_cx(',nsp,') = ',sp_cx(nsp)
!        enddo
!        print '(a,1pe12.5,a,1pe12.5,a,i6)','rsdmx = ',rsdmx,
!     &    ' rlxf = ',rlxf,' nc = ',nc 
!      endif
      IF( RSDMX.GT.1.D-6 ) THEN
        IF( NC.EQ.64 ) THEN
          ECKE_ER = .TRUE.
        ELSE
          GOTO 220
        ENDIF
      ENDIF
  350 CONTINUE
      IF(IER.EQ.-1) ECKE_ER = .TRUE.
!
!---  End of ECKECN group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ECKECN_R( CX,COX,SP_CX,N )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Conventional Newton scheme for solving the nonlinear
!     equilibrium, conservation, and kinetic chemistry equations.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, January 26, 2006.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 AJM(LSPR,LSPR),BJM(LSPR),AJMC(LSPR,LSPR),BJMC(LSPR)
      REAL*8 SP_CX(*),EQKX(LEQE)
      REAL*8 CX(*),COX(*)
      REAL*8 DCLX(LSPL),CLX(LSPL),ACTVX(LSPL+1,LSPL)
      REAL*8 VFX(LSPS+1)
      INTEGER IJM(LSPR)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ECKECN_R'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Total number of equations  ---
!
      NEQR = NEQC+NEQK
      RLXF = 1.D+0
!
!---  Volumetric concentration to molality  ---
!
      VTOMX = 1.D+0/(SL(2,N)*PORD(2,N)*RHOL(2,N)*XLW(2,N))
!
!---  Equilibrium constant as a function of temperature  ---
!
      DO NEQ = 1,NEQE
        NS = IEQ_E(1,NEQ)
        IRCE = IEQ_E((NS+2),NEQ)
        CALL EQCN( EQKX(NEQ),T(2,N),IRCE,N )
      ENDDO
!
!---  For initial conditions, skip kinetic equations  ---
!YFang -- Keep kinetic variables
!      IF( (NSTEP-NRST).EQ.0 ) NEQR = NEQE+NEQC
  100 CONTINUE
!
!---  Guess specie concentrations, assuming activity
!     coefficients of 1.0  ---
!
      IF( NSTEP.EQ.NRST ) THEN
!
!---    Assign local values of old-time-step species concentration  ---
!
        DO 110 NSP = 1,NSPR
          IF( ABS(SP_C(N,NSP)).LT.CMIN ) THEN
!            SP_CX(NSP) = 0.D+0
            SP_CX(NSP) = MAX(SP_CX(NSP),CMIN)
          ELSE
            SP_CX(NSP) = SP_C(N,NSP)
          ENDIF
  110   CONTINUE
!
!---    Assign local values of component species concentration  ---
!
        DO 120 NSP = 1,NEQC
          NSL = NSP + NSOLU
          CX(NSP) = C(N,NSL)
          COX(NSP) = C(N,NSL)
  120   CONTINUE
!
!---    Assign local values of kinetic species concentration  ---
!
        DO 140 NSP = 1,NEQK
          NSPX = NSP + NEQC
          NSL = NSPX + NSOLU
          CX(NSPX) = C(N,NSL)
          COX(NSPX) = C(N,NSL)
  140   CONTINUE
        NSPH = MOD(ISPLK(1),1000)
        DO NEQ = 1,NEQC
          NSP = IEQ_C(2,NEQ)
          IF( NSP.EQ.NSPH ) CYCLE
          SP_CX(NSP) = MAX( CX(NEQ)*5.D-2,CMIN )
        ENDDO
!
!---  If uninitialized set pH to 1.D-7 molality, mol/kg water  ---
!
        IF( NSPH.GE.1 .AND. NSPH.LE.LSPR ) THEN
          IF( SP_CX(NSPH)/EPSL.LT.EPSL ) SP_CX(NSPH) = 1.D-7/VTOMX
        ENDIF
!
!---  Recalculate equilibrium species concentrations 
!
        DO NEQ = 1,NEQE
         NEQ_SP = IEQ_E(2,NEQ)
         NS = IEQ_E(1,NEQ)
!
!---    Equilibrium constant and exponent  ---
!
!xyl
        IF( ISLC(60).EQ.0 ) THEN
         C_NEG = EQKX(NEQ)**EQ_E(NS,NEQ)
        ELSE
         C_NEG = EQ_E(NS,NEQ)*LOG(EQKX(NEQ))
        ENDIF
!xyl
!
!---    Loop over species in equilibrium equation
!       skipping the equilibrium species  ---
!
         DO K = 1,NS
          NCM_SP = IEQ_E(K+1,NEQ)
!
!---      Skip equilibrium species  ---
!
          IF( NCM_SP.EQ.NEQ_SP ) CYCLE
!
!---      Convert conservation species concentration to
!         molality, mol/kg H2O  ---
!
          CMX = SP_CX(NCM_SP)*VTOMX
!xyl
          IF( ISLC(60).EQ.0 ) THEN
          C_NEG = C_NEG*(CMX**EQ_E(K-1,NEQ))
          ELSE
           C_NEG = C_NEG+EQ_E(K-1,NEQ)*LOG(CMX)
          ENDIF
!xyl
         ENDDO
!xyl
         IF( ISLC(60).EQ.1 ) THEN
           C_NEG = EXP(C_NEG)
         ENDIF
!
!---    Convert equilibrium species concentration to
!       node volumetric, mol/m^3  ---
!
         SP_CX(NEQ_SP) = C_NEG/VTOMX
         SPCXX = MIN( 1.0D+30,SP_CX(NEQ_SP) )
         DO NCX = 1,NEQC
           LIN: DO IX=1,IEQ_C(1,NCX)
             NSP=IEQ_C(IX+1,NCX)
             IF(NEQ_SP.EQ.NSP) THEN
               SPCXM=ABS(CX(NCX)/EQ_C(IX,NCX))
               SPCXX=MIN(SPCXX,SPCXM)
               EXIT LIN
             ENDIF
           ENDDO LIN
         ENDDO
         SP_CX(NEQ_SP) = MAX(SPCXX,CMIN)
        ENDDO
      ENDIF
!
!---  Top of Newton-Raphson loop  ---
!
      NC = 0
  220 CONTINUE
      NC = NC + 1
      INDX = 0
!
!---  Set index to compute both the Jacobian matrix
!     and residual vector  ---
!
      CALL ECKEJCB( ACTVX,AJM,BJM,CX,COX,SP_CX,N,INDX,IER )
!!
!!---  Matrix-problem vector copy  ---
!!
!      DO 320 L = 1,NEQR
!        DO 310 M = 1,NEQR
!          AJMC(L,M) = AJM(L,M)
!  310   CONTINUE
!        BJMC(L) = BJM(L)
!  320 CONTINUE
!
!---  Solve linear system using LU Decomposition  ---
!
!      CALL LUDCMP( AJM,NEQR,LSPR,IJM,DJM )
!      CALL LUBKSB( AJM,NEQR,LSPR,IJM,BJM )
!      CALL MPROVE( AJMC,AJM,NEQR,LSPR,IJM,BJMC,BJM )
!      CALL DGELG(BJMC,AJMC,LSPR,NEQR,EPSL,IER)
      CALL GAUSSJ( AJM,BJM,NEQR,LSPR )

      IF( IER.EQ.-1 ) goto 350
!
!---  Maximum residual  ---
!
      RSDMX = 1.D-20
      DO 330 NSP = 1,NSPR
        NEQX = IEQ_S(NSP)
        IF( NEQX.GT.NEQE) THEN
          NROWX = NEQX-NEQE
          IF( SP_CX(NSP)/EPSL.GT.EPSL .OR.
     &      BJM(NROWX)/EPSL.GT.EPSL ) THEN
            SP_CMX = MAX( ABS(SP_CX(NSP)),ABS(SP_C(N,NSP)),CMIN )
            IF( ABS(SP_CMX).GT.CMIN ) THEN
              RSDX = ABS(BJM(NROWX))/SP_CMX
            ELSE
              RSDX = 1.D+0
            ENDIF
            IF( RSDX.GT.RSDMX ) THEN
              IF( ABS(SP_CX(NSP)).GT.1.D-16 ) THEN
                RSDMX = RSDX
                NSPMX = NSP
              ENDIF
            ENDIF
          ENDIF
        ENDIF
  330 CONTINUE
!
!---  Update the species concentrations, mol/m^3  ---
!
      DO 332 NSP = 1,NSPR
!
!---    Relaxation  ---
!
       NEQX = IEQ_S(NSP)
       IF( NEQX.GT.NEQE ) THEN
        NROWX = NEQX-NEQE
        CCX = RLXF*BJM(NROWX)
        IF( CCX.LT.0.D+0 ) THEN
!
!---      Mineral species  ---
!
          IF( ISP_MN(NSP).EQ.1 ) THEN
            NSP_MN = NSP - NSPL
            SP_CCX = SP_CMN(N,NSP_MN)+SP_CX(NSP)
            CCX = SIGN( MIN( SP_CCX,ABS(CCX) ),CCX )
!
!---      Immobile species  ---
!
          ELSEIF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
            SP_CCX = SP_CX(NSP)
            CCX = SIGN( MIN( SP_CCX,ABS(CCX) ),CCX )
!
!---      Mobile species  ---
!
          ELSE
            SP_CCX = 9.5D-1*SP_CX(NSP)
            CCX = SIGN( MIN( SP_CCX,ABS(CCX) ),CCX )
          ENDIF
        ENDIF
!
!---    Update mineral species concentration  ---
!
        IF( ISP_MN(NSP).EQ.1 ) THEN
          SP_CX(NSP) = SP_CX(NSP)+CCX
          IF( ABS(SP_CX(NSP)).LT.CMIN ) SP_CX(NSP) = 0.D+0
!
!---    Update non-mineral species concentration  ---
!
        ELSE
          SP_CX(NSP) = MAX( SP_CX(NSP)+CCX,0.D+0 )
        ENDIF
       ENDIF
  332 CONTINUE
!
!---  Update equilium species concentration
!
!
!---  Calculate species activity coefficient
!
!      CLX(1:NSPL) = SP_CX(1:NSPL)
!      DCLX = 1.D-6
!      IF( IACTV.EQ.3 ) THEN
!        ACTVX(1,NSP) = ACTVC
!      ELSEIF( IACTV.EQ.1 ) THEN
!        CALL DAVIES( ACTVX,CLX,DCLX,SL(2,N),PORD(2,N),
!     &    RHOL(2,N),T(2,N),XLW(2,N) )
!      ELSE
!        CALL BDOT( ACTVX,CLX,DCLX,SL(2,N),PORD(2,N),
!     &    RHOL(2,N),T(2,N),XLW(2,N) )
!      ENDIF
!
!---  Recalculate equilibrium species concentrations 
!
      DO NEQ = 1,NEQE
        NEQ_SP = IEQ_E(2,NEQ)
        NS = IEQ_E(1,NEQ)
!
!---    Equilibrium constant and exponent  ---
!
!xyl
        IF( ISLC(60).EQ.0 ) THEN
        C_NEG = EQKX(NEQ)**EQ_E(NS,NEQ)
        ELSE
          C_NEG = EQ_E(NS,NEQ)*LOG(EQKX(NEQ))
        ENDIF
!xyl
!
!---    Loop over species in equilibrium equation
!       skipping the equilibrium species  ---
!
        DO K = 1,NS
          NCM_SP = IEQ_E(K+1,NEQ)
!
!---      Skip equilibrium species  ---
!
          IF( NCM_SP.EQ.NEQ_SP ) CYCLE
!
!---      Convert conservation species concentration to
!         molality, mol/kg H2O  ---
!
          IF(NCM_SP.LE.NSPL) THEN
            ACX = ACTVX(1,NCM_SP)
          ELSE
            ACX = 1.D+0
          ENDIF
          CMX = SP_CX(NCM_SP)*VTOMX*ACX
!xyl
          IF( ISLC(60).EQ.0 ) THEN
          C_NEG = C_NEG*(CMX**EQ_E(K-1,NEQ))
          ELSE
           C_NEG = C_NEG+EQ_E(K-1,NEQ)*LOG(CMX)
          ENDIF
!xyl
        ENDDO
!xyl
         IF( ISLC(60).EQ.1 ) THEN
           C_NEG = EXP(C_NEG)
         ENDIF
!
!---    Convert equilibrium species concentration to
!       node volumetric, mol/m^3  ---
!
        IF(NEQ_SP.LE.NSPL) THEN
          ACX = ACTVX(1,NEQ_SP)
        ELSE
          ACX = 1.D+0
        ENDIF
        SP_CX(NEQ_SP) = C_NEG/VTOMX/ACX
      ENDDO
!
!---  Unconverged species concentration exit or repeat
!     Newton-Raphson loop  ---
!
      IF( RSDMX.GT.1.D-6 ) THEN
        IF( NC.EQ.32 ) THEN
          RLXF = 6.D-1
          GOTO 220
        ELSEIF( NC.EQ.200 ) THEN
          ECKE_ER = .TRUE.
        ELSE
          GOTO 220
        ENDIF
      ENDIF
!
!---  Check mass balance for components
!
      DO NEQX=1,NEQC
        NSP =  ISP_S(NEQX+NEQE)
        IF( MOD(ISPLK(1),1000).NE.0 ) CYCLE
        DIFF = CX(NEQX)
        DO M=1,IEQ_C(1,NEQX)
          NSP=IEQ_C(M+1,NEQX)
          DIFF=DIFF-EQ_C(M,NEQX)*SP_CX(NSP)          
        ENDDO
        IF( CX(NEQX).EQ.0.D+0 .AND. DIFF.LE.1.D-8 ) CYCLE
        IF( DIFF .LE. 0.04*ABS(CX(NEQX)) ) CYCLE
        ECKE_ER = .TRUE.
      ENDDO
  350 continue
      if(ier.eq.-1) ecke_er = .true.
!
!---  End of ECKECN_R group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ECKEJCB( ACTVX,AJM,BJM,CX,COX,SP_CX,N,INDX,IER )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Numerical Recipes in Fortran 77, The Art of Scientific Computing
!     W.H. Press, B.P. Flannery, Saul A. Teukolsky, and W.T. Vetterling
!
!     INDX =  1 - This subroutine replaces the original funcv
!     INDX = -1 - This subroutine replaces the original fdjac
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, January 26, 2006.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 AJM(LSPR,LSPR),BJM(LSPR)
      REAL*8 SP_CX(LSPR),DSP_CX(LSPR)
      REAL*8 CX(LEQC+LEQK),COX(LEQC+LEQK)
      REAL*8 DCLX(LSPL),CLX(LSPL),ACTVX(LSPL+1,LSPL)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ECKEJCB'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Number of equations equals number of species  ---
!
      NEQR = NSPR
!
!---  For initial conditions, skip kinetic equations  ---
!
!      IF( (NSTEP-NRST).EQ.0 ) NEQR = NEQE+NEQC
!
!---  Specie increments  ---
!
      DO 230 NSP = 1,NSPR
        DSP_CX(NSP) = 1.D-5*SP_CX(NSP)
        IF( ABS(SP_CX(NSP)).LT.CMIN ) THEN
          IF( ISP_MN(NSP).EQ.1 ) THEN
            DSP_CX(NSP) = 1.D-6
          ELSE
            SP_CX(NSP) = CMIN
            DSP_CX(NSP) = 1.D-16
          ENDIF
        ENDIF
  230 CONTINUE
!
!---  Load parameters for activity coefficient
!     calculations for aqueous species  ---
!
      DO 240 NSP = 1,NSPL
         CLX(NSP) = SP_CX(NSP)
         DCLX(NSP) = DSP_CX(NSP)
  240 CONTINUE
!
!---  Activity coefficients for aqueous species  ---
!
      IF( IACTV.EQ.3 ) THEN
        DO 260 NSP = 1,NSPL
          DO 250 M = 1,NSPL+1
            ACTVX(M,NSP) = ACTVC
  250     CONTINUE
  260   CONTINUE
      ELSEIF( IACTV.EQ.1 ) THEN
        CALL DAVIES( ACTVX,CLX,DCLX,SL(2,N),PORD(2,N),
     &    RHOL(2,N),T(2,N),XLW(2,N) )
      ELSEIF( IACTV.EQ.2 ) THEN
        CALL PITZER( ACTVX,CLX,DCLX,SL(2,N),PORD(2,N),
     &    RHOL(2,N),T(2,N),XLW(2,N) )
      ELSE
        CALL BDOT( ACTVX,CLX,DCLX,SL(2,N),PORD(2,N),
     &    RHOL(2,N),T(2,N),XLW(2,N) )
      ENDIF
!
!---  Initialize residual and residual partial
!     derivatives  ---
!
      DO 280 NEQ = 1,NEQR
        BJM(NEQ) = 0.D+0
        DO 270 M = 1,NEQR
          AJM(NEQ,M) = 0.D+0
  270   CONTINUE
  280 CONTINUE
!
!---  Loop over equations  ---
!
      IF( ISLC(57).EQ.0 ) THEN
      DO 300 NEQ = 1,NEQR
!
!---    Equilibrium equation  ---
!
        IF( NEQ.LE.NEQE ) THEN
          CALL EECHEM( ACTVX,AJM,BJM,SP_CX,DSP_CX,N,NEQ,INDX )
!
!---    Conservation equation  ---
!
        ELSEIF( NEQ.LE.NEQE+NEQC ) THEN
          CALL CECHEM( ACTVX,AJM,BJM,CX,SP_CX,N,NEQ,INDX )
!
!---    Kinetic equation  ---
!
        ELSEIF( NEQ.LE.NEQE+NEQC+NEQK ) THEN
          CALL KECHEM( ACTVX,AJM,BJM,COX,SP_CX,DSP_CX,N,NEQ,INDX,IER )
!
!---    Unknown equation  ---
!
        ELSE
          CHMSG = 'Unrecognized Equation Index: '
          IMSG = NEQ
          INDX = 12
          CALL WRMSGS( INDX )
        ENDIF
  300 CONTINUE
      ELSE
        DO 400 NEQ = 1,NEQR
!
!---      Equilibrium equation  ---
!         
          IF( NEQ.LE.NEQE ) THEN
            GOTO 400
!         
!---      Conservation equation  ---
!         
          ELSEIF( NEQ.LE.NEQE+NEQC ) THEN
            CALL CECHEM_R( ACTVX,AJM,BJM,CX,SP_CX,N,NEQ,INDX )
!         
!---      Kinetic equation  ---
!         
          ELSEIF( NEQ.LE.NEQE+NEQC+NEQK ) THEN
            CALL KECHEM_R( ACTVX,AJM,BJM,COX,SP_CX,DSP_CX,N,NEQ,INDX )
!         
!---      Unknown equation  ---
!         
          ELSE
            CHMSG = 'Unrecognized Equation Index: '
            IMSG = NEQ
            INDX = 12
            CALL WRMSGS( INDX )
          ENDIF
  400   CONTINUE
      ENDIF
!
!---  End of ECKEJCB group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE EECHEM( ACTVX,AJM,BJM,SP_CX,DSP_CX,N,NEQ,INDX )
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
!     Equilibrium Equation CHEMistry
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 January 2005.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 ACTVX(LSPL+1,LSPL)
      REAL*8 BJM(LSPR),AJM(LSPR,LSPR),SP_CX(LSPR),DSP_CX(LSPR)
      REAL*8 ACTEX(LESITE)
      LOGICAL FCHK
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/EECHEM'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Skip for initial pH  ---
!
      IF( ISPLK(1).GT.1000 ) THEN
        IF( NEQ.EQ.IEQ_S(MOD(ISPLK(1),1000)) .AND.
     &    (NSTEP-NRST).EQ.0 ) THEN
          AJM(NEQ,NEQ) = 1.D+0
          BJM(NEQ) = 0.D+0
          GOTO 1000
        ENDIF
      ENDIF
!
!--- Fixed species concentration or activity  ---
!
      DO 10 NSLKX = 1,NSPLK
        IF( ISPLK(14+NSLKX).LT.0 ) THEN
          NSPX = ABS(ISPLK(14+NSLKX))
          IF( NSPX.GT.1000 ) NSPX = NSPX - 1000
          IF( NEQ.EQ.IEQ_S(NSPX) ) THEN
            AJM(NEQ,NEQ) = 1.D+0
            BJM(NEQ) = 0.D+0
            GOTO 1000
          ENDIF
        ENDIF
   10 CONTINUE
!
!---  Total number of species  ---
!
      NSPR = NSPG + NSPL + NSPN + NSPS + NSPE
!
!---  Equilibrium constant as a function of temperature  ---
!
      NS = IEQ_E(1,NEQ)
      IRCE = IEQ_E((NS+2),NEQ)
      CALL EQCN( EQKX,T(2,N),IRCE,N )
!
!---  Volumetric concentration to molality  ---
!
      VTOMX = 1.D+0/(SL(2,N)*PORD(2,N)*RHOL(2,N)*XLW(2,N))
      VTOGX = 1.D+0/(MAX( 1.D-20,SG(2,N)*PORD(2,N)) )
      VTOLX = 1.D+0/(SL(2,N)*PORD(2,N))
      RHOWX = RHOL(2,N)*XLW(2,N)
!
!--- Prepare for exchanged species activity calculation
!
      IF( NSPE.NE.0 ) THEN
        DO 20 ISITE = 1,NESITE
          ACTEX(ISITE) = 0.0D+0
   20   CONTINUE
!
!--- Gaines-Thomas convention
!
        IF( IACTEX.EQ.1 ) THEN
          DO 30 NSP = 1,NSPE
            NCM_SP = NSPL + NSPS + NSP
            SP_CX(NCM_SP) = MAX(CMIN,SP_CX(NCM_SP))
            ISITE = ISP_E(NSP)
            ICAT = IEL_LK(NSP)
            ACTEX(ISITE) = ACTEX(ISITE)+SP_CX(NCM_SP)*SP_L(1,ICAT)
   30     CONTINUE
        ENDIF
      ENDIF
!
!---  Base residual  ---
!
      IF( ISLC(60).EQ.0 ) THEN
      C_NEG = EQKX**EQ_E(NS,NEQ)
      ELSE
        C_NEG = EQ_E(NS,NEQ)*LOG(EQKX)
      ENDIF
!
!---  Loop over equilibrium species  ---
!
      DO 100 M = 1,NS
        NCM_SP = IEQ_E(M+1,NEQ)
!
!---    Aqueous species,
!       concentration in molality, mol/kg H2O  ---
!
        IF( NCM_SP.LE.NSPL ) THEN
          CMX = SP_CX(NCM_SP)*VTOMX
          ACX = ACTVX(1,NCM_SP)
!
!---    Non-aqueous species,
!       concentration in kmolal, mol/kg H2O  ---
!
        ELSEIF(NCM_SP.LE.NSPL+NSPS) THEN
          CMX = SP_CX(NCM_SP)*VTOMX
          ACX = 1.D+0
!
!---    Exchanged species,
!       concentration in kmolal, mol/kg H2O  ---
!
        ELSEIF(NCM_SP.LE.NSPL+NSPS+NSPE) THEN
          ISITE = ISP_E(NCM_SP-NSPL-NSPS)
          CMX = SP_CX(NCM_SP)
          ICAT = IEL_LK(NCM_SP-NSPL-NSPS)
          ACX = CMX*SP_L(1,ICAT)/ACTEX(ISITE)
          CMX = 1.D+0
!
!---    Gas species,
!       convert from mol/node volume to mol/volume of gas,
!       use RHOWX for Henry's law  ---
!
        ELSEIF(NCM_SP.LE.NSPL+NSPS+NSPE+NSPG) THEN
          CMX = SP_CX(NCM_SP)*VTOGX/RHOWX
          ACX = 1.D+0
        ENDIF
!
!---    Equilibrium species  ---
!
        IF( M.EQ.1 ) THEN
          C_POS = ACX*CMX
!
!---    Conservation species  ---
!
        ELSE
!xyl 
          IF( ISLC(60).EQ.0 ) THEN
           C_NEG = C_NEG*((ACX*CMX)**EQ_E(M-1,NEQ))
          ELSE
           C_NEG = C_NEG+EQ_E(M-1,NEQ)*LOG(ACX*CMX)
          ENDIF
!xyl
        ENDIF
  100 CONTINUE
      IF( ISLC(60).EQ.1 ) THEN
        C_NEG = EXP(C_NEG)
      ENDIF
      BJM(NEQ) = C_POS - C_NEG
!
!---  Return residual vector  ---
!
      IF( INDX.EQ.1 ) GOTO 1000
!
!---  Incremented residuals, computed numerically  ---
!
      DO 400 NSP = 1,NSPR
!
!---    Check whether specie is a equilibrium equation specie,
!         which affects the residual via the equilibrium equation
!       or an aqueous specie,
!         which affects the residual via the equilibrium equation,
!         via the equilibrium equation reactions
!         via the equilibrium constant,
!         via the activity coefficient,
!         via the ionic strength  ---
!
        FCHK = .FALSE.
        IF( NSP.LE.NSPL ) FCHK = .TRUE.
!
!---    Loop over equilibrium equation species  ---
!
        IF( .NOT.FCHK ) THEN
          DO 310 M = 1,NS
            NSP_E = IEQ_E(M+1,NEQ)
            IF( NSP.EQ.NSP_E ) THEN
              FCHK = .TRUE.
              GOTO 320
            ENDIF
  310     CONTINUE
  320     CONTINUE
        ENDIF
!
!---    Skip for initial pH  ---
!
        IF( ISPLK(1).GT.1000 ) THEN
          IF( NSP.EQ.MOD(ISPLK(1),1000) .AND.
     &      (NSTEP-NRST).EQ.0 ) FCHK = .FALSE.
        ENDIF
!
!---    Specie is either an equilibrium equation specie
!       or an aqueous specie  ---
!
        IF( FCHK ) THEN
!
!---      Base residual  ---
!
          IF( ISLC(60).EQ.0 ) THEN
          C_NEG = EQKX**EQ_E(NS,NEQ)
          ELSE
            C_NEG = EQ_E(NS,NEQ)*LOG(EQKX)
          ENDIF
!
!---      Loop over equilibrium species  ---
!
          DO 330 M = 1,NS
            NCM_SP = IEQ_E(M+1,NEQ)
!
!---        Aqueous species,
!           concentration in molality, mol/kg H2O  ---
!
            IF( NCM_SP.LE.NSPL ) THEN
!
!---          Incremented species  ---
!
              IF( NCM_SP.EQ.NSP )  THEN
                CMX = (SP_CX(NCM_SP)+DSP_CX(NCM_SP))*VTOMX
                ACX = ACTVX(NSP+1,NCM_SP)
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NCM_SP)*VTOMX
                ACX = ACTVX(1,NCM_SP)
              ENDIF
!
!---        Non-aqueous species,
!           concentration in kmolal, mol/kg H2O  ---
!
            ELSEIF(NCM_SP.LE.NSPL+NSPS) THEN
!
!---          Incremented species  ---
!
              IF( NCM_SP.EQ.NSP )  THEN
                CMX = (SP_CX(NCM_SP)+DSP_CX(NCM_SP))*VTOMX
                ACX = 1.D+0
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NCM_SP)*VTOMX
                ACX = 1.D+0
              ENDIF
!
!---        Exchanged species  ---
!
            ELSEIF(NCM_SP.LE.NSPL+NSPS+NSPE) THEN
!
!---          Incremented species  ---
!
              ISITE = ISP_E(NCM_SP-NSPL-NSPS)
              ICAT = IEL_LK(NCM_SP-NSPL-NSPS)
              CHARGX = SP_L(1,ICAT)
              IF( NCM_SP.EQ.NSP )  THEN
                CMX = SP_CX(NCM_SP)+DSP_CX(NCM_SP)
                ACX = CMX*CHARGX/(ACTEX(ISITE)+DSP_CX(NCM_SP)*CHARGX)
                CMX = 1.D+0
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NCM_SP)
                ACX = CMX*CHARGX/ACTEX(ISITE)
                CMX = 1.D+0
              ENDIF
!
!---        Gas species
!
            ELSEIF(NCM_SP.LE.NSPL+NSPS+NSPE+NSPG) THEN
!
!---          Incremented species  ---
!
              IF( NCM_SP.EQ.NSP )  THEN
                CMX = (SP_CX(NCM_SP)+DSP_CX(NCM_SP))*VTOGX/RHOWX
                ACX = 1.D+0
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NCM_SP)*VTOGX/RHOWX
                ACX = 1.D+0
              ENDIF
            ENDIF
!
!---        Equilibrium species  ---
!
            IF( M.EQ.1 ) THEN
              C_POS = ACX*CMX
!
!---        Conservation species  ---
!
            ELSE
!xyl 
             IF( ISLC(60).EQ.0 ) THEN
              C_NEG = C_NEG*((ACX*CMX)**EQ_E(M-1,NEQ))
            ELSE
              C_NEG = C_NEG+EQ_E(M-1,NEQ)*LOG(ACX*CMX)
            ENDIF
!xyl
            ENDIF
  330     CONTINUE
          IF( ISLC(60).EQ.1 ) THEN
            C_NEG = EXP(C_NEG)
          ENDIF
          AVX = C_NEG + BJM(NEQ)
          BVX = ABS(C_NEG) + ABS(BJM(NEQ))
          CVX = C_POS
          IF( BVX/EPSL.GT.EPSL ) THEN
            IF( ABS(AVX)/BVX.GT.EPSL ) THEN
              CVX = C_POS - AVX
            ENDIF
          ENDIF
!          AJM(NEQ,IEQ_S(NSP)) = (C_POS - C_NEG - BJM(NEQ))/DSP_CX(NSP)
          AJM(NEQ,IEQ_S(NSP)) = CVX/DSP_CX(NSP)
        ENDIF
  400 CONTINUE
!
!---  Return Jacobian matrix and residual vector  ---
!
      IF( ABS(INDX).EQ.1 ) GOTO 1000
      BJM(NEQ) = -BJM(NEQ)
 1000 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of EECHEM group  ---
!
      RETURN
      END
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ELECTS(ZI,ZJ,IT,CPIX,APHI)
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
!     This subroutine calculates higher order electrostatic functions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by A Felmy, from GMIN
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PTZRCOEF
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
      REAL*8 A1(21),A2(21),A3(7),A4(9),A5(9),A6(9),P(10)
      REAL*8 J01,J02,J03,J11,J12,J13,J0,J1,J2,J21,J22,J23

      DATA a1/-.000000000010991D0,-.000000000002563D0
     & ,0.000000000001943D0,0.000000000046333D0,-.000000000050847D0 
     & ,-.000000000821969D0,0.000000001229405D0,0.000000013522610D0 
     & ,-.000000025267769D0,-.000000202099617D0,0.000000396566462D0
     & ,0.000002937706971D0,-.000004537895710D0,-.000045036975204D0
     & ,0.000036583601823D0,0.000636874599598D0,0.000388260636404D0
     & ,-.007299499690937D0,-.029779077456514D0,-.060076477753119D0
     & ,1.925154014814667D0/

      DATA a2/0.000000000237816D0,-.000000002849257D0
     & ,-.000000006944757D0,0.000000004558555D0,0.000000080779570D0
     & ,0.000000216991779D0,-.000000250453880D0,-.000003548684306D0
     & ,-.000004583768938D0,0.000034682122751D0,0.000087294451594D0
     & ,-.000242107641309D0,-.000887171310131D0,0.001130378079086D0
     & ,0.006519840398744D0,-.001668087945272D0,-.036552745910311D0
     & ,-.028796057604906D0,0.150044637187895D0,0.462762985338493D0
     & ,0.628023320520852D0/

      DATA a3/.000029308779366,.000029869648486,.000009838580455
     & ,.000000827954226,-.000000098522914,.000000013943401
     & ,-.000000005963131/

      DATA a4/.018407992691,.023626104695,.005004621881
     & ,-.000300844194,-.000001533185,.000009318246,-.000004258797
     & ,.000001509090,-.000000766492/

      DATA a5/3.9986000731,3.7950588585,-.3325673918
     & ,-.0899335274,.1338914658,.1948882815,.2368262404
     & ,.1379406836,.0478072558/

      DATA a6/37.837805702,24.470110234,-3.719597578
     & ,0.991895847,-.327141729,.121485594,-.051539972
     & ,.017779940,-.011800766/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ELECTS'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
      DSQI=DSQRT(CPIX)
      X1 = 6.0d0*APHI*DSQI
      P(1)=1.0D0
!
!--- Calculate integrals(see Harvie 1981)
!
      DO I = 1,3
        IF( I.EQ.1 ) XX = X1*ZI*ZI
        IF( I.EQ.2 ) XX = X1*ZJ*ZJ
        IF( I.EQ.3 ) XX = X1*ZI*ZJ
        BK = 0.0D0
        DK = 0.0D0
        BK1 = 0.0D0
        BK2 = 0.0D0
        DK1 = 0.0D0
        DK2 = 0.0D0
        IF( XX.LE.1.0D0 )THEN
          TT = 4.0d0*(XX**0.2D0)
          DZ = TT/(5.0d0*XX)
          ZZ = TT-2.0D0
          IF( XX.LE.0.05D0 )THEN
            Z2 = (XX/0.0245D0)-1.040816D0
            P(2) = Z2
            DO K = 3,7
              P(K)=2.0d0*Z2*P(K-1)-P(K-2)
            END DO
            J2 = 0.0D0
            DO K = 1,7
              J2 = J2+A3(K)*P(K)
            END DO
          ELSE
            Z2 = (XX/0.475D0)-1.105263D0
            P(2)=Z2
            DO K = 3,9
              P(K) = 2.0d0*Z2*P(K-1)-P(K-2)
            END DO
            J2 = 0.0D0
            DO K = 1,9
              J2 = J2+A4(K)*P(K)
            END DO
          END IF
          DO K = 1,21
            BK2 = BK1
            BK1 = BK
            BK = ZZ*BK1-BK2+A1(K)
            DK2 = DK1
            DK1 = DK
            DK = BK1+ZZ*DK1-DK2
          END DO
        ELSE
          TT = 4.444444444D0*(XX**(-.10D0))
          DZ = -TT/(10.0D0*XX)
          ZZ = TT-2.44444444D0
          IF( XX.LE.50.0D0 )THEN
            Z2 = (XX/24.5D0)-1.040816
            P(2) = Z2
            DO K = 3,9
              P(K) = 2.0D0*Z2*P(K-1)-P(K-2)
            END DO
            J2 = 0.0D0
            DO K = 1,9
              J2 = J2+A5(K)*P(K)
            END DO
        ELSE
            Z2 = (XX/425.0D0)-1.117647
            P(2) = Z2
            DO K = 3,9
              P(K) = 2.0D0*Z2*P(K-1)-P(k-2)
            END DO
            J2 = 0.0D0
            DO K = 1,9
              J2 = J2+A6(K)*P(K)
            END DO
          END IF
          DO K = 1,21
            BK2 = BK1
            BK1 = BK
            BK = ZZ*BK1-BK2+A2(K)
            DK2 = DK1
            DK1 = DK
            DK = BK1+ZZ*DK1-DK2
          END DO
        END IF
!
!--- Now calculate electrostatic functions
!
        J0 =0.25D0*xx+0.5D0*(BK-BK2)-1.0D0
        J1 = XX*(0.25D0+0.5D0*DZ*(DK-DK2))
        J2 = J2/XX
        J03 = J0
        J13 = J1
        J23 = J2
        IF( I.EQ.1 )THEN
          J01 = J0
          J11 = J1
          J21 = J2
        END IF
        IF( I.eq.2 )THEN
          J02 = J0
          J12 = J1
          J22 = J2
        END IF
      END DO
!
!--- Now calculate eth and ethp
!
      TMP = (ZI*ZJ)/(4.0D0*CPIX)
      ETH(IT) = TMP*(J03-0.5D0*(J01+J02))
      ETHP(IT) = (TMP/(2.0D0*CPIX))*(J13-0.5D0*(J11+J12))
     &          -ETH(IT)/CPIX
      ETHP2(IT) = -(1.0D0/(2.0D0*CPIX))*(ETH(IT)/CPIX+5.0D0*ETHP(IT))
     & +(TMP/(4.0D0*CPIX*CPIX))*(J23-0.5D0*(J21+J22))
!
!---  End of ELECTS ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END
!
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE EQCN( EQKX,TX,INDX,N )
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
!     Equilibrium reaction constant as a function of temperature.
!
!     INDX > 0 : equilibrium reaction index  RC_E
!     INDX < 0 : kinetic reaction index RC_K
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 20 December 2004.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
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
!----------------------Type Declarations-------------------------------!
!
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/EQCN'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Absolute temperature  ---
!
      TKX = TX+TABS
!
!---  Equilibrium reaction  ---
!
      IF( INDX.GT.0 ) THEN
        EQKX = RC_E(1,INDX)*LOG(TKX) + RC_E(2,INDX) +
     &    RC_E(3,INDX)*TKX + RC_E(4,INDX)/TKX +
     &    RC_E(5,INDX)/(TKX**2)
        EQKX = EXP(TOLN*EQKX)
!
!---  Kinetic reaction  ---
!
      ELSEIF( INDX.LT.0 ) THEN
        JNDX = -INDX
        NSPRX = IRC_K(1,JNDX)
        NSPPX = IRC_K(2,JNDX)
        NSPKX = NSPRX+NSPPX
        N4 = MAX(1,N*IRCKN(NSPKX+4))
        N5 = MAX(1,N*IRCKN(NSPKX+5))
        N6 = MAX(1,N*IRCKN(NSPKX+6))
        N7 = MAX(1,N*IRCKN(NSPKX+7))
        N8 = MAX(1,N*IRCKN(NSPKX+8))
        EQKX = RC_K(NSPKX+4,N4,JNDX)*LOG(TKX) + RC_K(NSPKX+5,N5,JNDX) +
     &    RC_K(NSPKX+6,N6,JNDX)*TKX + RC_K(NSPKX+7,N7,JNDX)/TKX +
     &    RC_K(NSPKX+8,N8,JNDX)/(TKX**2)
        EQKX = EXP(TOLN*EQKX)
      ELSE
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of EQCN group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE EQEQ( EQKX,SP_CX,VTOMX )
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
!     Calculate equilibrium species concentrations according to
!     the conservation species concentrations, assuming
!     an activity of 1.0
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 August 2005.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
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
      REAL*8 SP_CX(LSPR)
      REAL*8 EQKX(LEQE)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/EQEQ'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Set equilibrium species concentrations according to
!     the conservation species concentrations, assuming
!     an activity of 1.0  ---
!
      DO 40 NEQ = 1,NEQE
        NEQ_SP = IEQ_E(2,NEQ)
        NS = IEQ_E(1,NEQ)
!
!---    Equilibrium constant and exponent  ---
!
!xyl
        IF( ISLC(60).EQ.0 ) THEN
        C_NEG = EQKX(NEQ)**EQ_E(NS,NEQ)
        ELSE
         C_NEG = EQ_E(NS,NEQ)*LOG(EQKX(NEQ))
        ENDIF
!xyl
!
!---    Loop over species in equilibrium equation
!       skipping the equilibrium species  ---
!
        DO 30 K = 1,NS
          NCM_SP = IEQ_E(K+1,NEQ)
!
!---      Skip equilibrium species  ---
!
          IF( NCM_SP.EQ.NEQ_SP ) GOTO 30
!
!---      Convert conservation species concentration to
!         molality, mol/kg H2O  ---
!
          SP_CX(NCM_SP) = MAX(CMIN,SP_CX(NCM_SP))
          CMX = SP_CX(NCM_SP)*VTOMX
!xyl
        IF( ISLC(60).EQ.0 ) THEN
          C_NEG = C_NEG*(CMX**EQ_E(K-1,NEQ))
        ELSE
          C_NEG = C_NEG+EQ_E(K-1,NEQ)*LOG(CMX)
        ENDIF
   30   CONTINUE
!xyl
        IF( ISLC(60).EQ.1 ) THEN
          C_NEG = EXP(C_NEG)
        ENDIF
!
!---    Convert equilibrium species concentration to
!       node volumetric, mol/m^3  ---
!
        SP_CX(NEQ_SP) = C_NEG/VTOMX
   40 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of EQEQ group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE FLHSP
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
!     Convert reactive species initial condition concentrations into
!     mol/m^3 node volume
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 18 August 2005.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE GRID
      USE FDVP
      USE CONST
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
      CHARACTER*64 GETSPNM
      EXTERNAL GETSPNM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/FLHSP'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
      NSPR = NSPG + NSPL + NSPN + NSPS + NSPE
!
!---  Define completely immobile conservation component species  ---
!
      DO 2 NEQ = 1,NEQC
        IMMB(NEQ) = 1
!
!---    Loop over conservation-component species  ---
!
        DO 4 M = 1,IEQ_C(1,NEQ)
          NSP = IEQ_C(M+1,NEQ)
!
!---      Mobile species ---
!
          IF( NSP.LE.NSPL .OR. NSP.GT.(NSPL+NSPS+NSPE) ) IMMB(NEQ) = 0
    4   CONTINUE
    2 CONTINUE
!
!---  Define completely immobile kinetic component species  ---
!
      DO 8 NEQ = 1,NEQK
        IMMB(NEQ+NEQC) = 1
!
!---    Loop over kinetic-component species  ---
!
        DO 6 M = 1,IEQ_K(1,NEQ)
          NSP = IEQ_K(M+1,NEQ)
!
!---      Mobile species ---
!
          IF( NSP.LE.NSPL .OR. NSP.GT.(NSPL+NSPS+NSPE) )
     &      IMMB(NEQ+NEQC) = 0
    6   CONTINUE
    8 CONTINUE
!
!---  Loop over all nodes, skipping inactive nodes  ---
!
      DO 400 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 400
        IZN = IZ(N)
!
!---    Loop over reactive species  ---
!
        DO 100 NSP = 1,NSPR
          IF( INDEX(GETSPNM(NSP),'fix').NE.0 ) THEN
            SP_CI(N,NSP) = SP_C(N,NSP)
          ENDIF
!
!---      Restart simulation  ---
!
          IF( IEO.EQ.2 ) THEN
!
!---        Convert reactive species from node volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            IF( IC_SP(N,NSP).EQ.11 ) THEN
              SP_C(N,NSP) = 1.D+3*SP_C(N,NSP)
!
!---        Convert reactive species from aqueous volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( IC_SP(N,NSP).EQ.12 ) THEN
              SP_C(N,NSP) = 1.D+3*SP_C(N,NSP)*SL(2,N)*PORD(2,N)
!
!---        Convert reactive species from aqueous molal, kmol/kg water
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( IC_SP(N,NSP).EQ.13 ) THEN
              SP_C(N,NSP) = 1.D+3*SP_C(N,NSP)*SL(2,N)*PORD(2,N)*
     &          RHOL(2,N)*XLW(2,N)
!
!---        Convert reactive species from gas volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( IC_SP(N,NSP).EQ.14 ) THEN
              SP_C(N,NSP) = 1.D+3*SP_C(N,NSP)*SG(2,N)*PORD(2,N)
!
!---        Convert solid-species mineral volumetric fraction
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
              DO 10 NRC = 1,NRCK
                IF( IRCKT(NRC).EQ.20 ) THEN
                  NSPX = IRC_K(1,NRC)
                ELSE
                  NSPRX = IRC_K(1,NRC)
                  NSPPX = IRC_K(2,NRC)
                  NSPX = IRC_K(3+NSPRX+NSPPX,NRC)
                ENDIF
                IF( NSP.EQ.NSPX ) THEN
                  NSPX = NSP-NSPL
!
!---              Restart simulation w/ lithology overwrite  ---
!
                  IF( SP_S(2,NSPX)/EPSL.GT.EPSL .AND.
     &              ISP_OW(NSPX,N).EQ.1 ) THEN
                    SP_CMN(N,NSPX) = 1.D+3*RS_S(2,NSPX,N)*
     &                SP_S(1,NSPX)/SP_S(2,NSPX)
                  ENDIF
                  GOTO 20
                ENDIF
  10          CONTINUE
  20          CONTINUE
            ENDIF
!
!---      Normal simulation  ---
!
          ELSE
!
!---        Convert reactive species from node volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            IF( IC_SP(N,NSP).EQ.1 ) THEN
              SP_C(N,NSP) = 1.D+3*SP_C(N,NSP)
!
!---        Convert reactive species from aqueous volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( IC_SP(N,NSP).EQ.2 ) THEN
              SP_C(N,NSP) = 1.D+3*SP_C(N,NSP)*SL(2,N)*PORD(2,N)
!
!---        Convert reactive species from aqueous molal, kmol/kg water
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( IC_SP(N,NSP).EQ.3 ) THEN
              SP_C(N,NSP) = 1.D+3*SP_C(N,NSP)*SL(2,N)*PORD(2,N)*
     &          RHOL(2,N)*XLW(2,N)
!
!---        Convert reactive species from gas volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( IC_SP(N,NSP).EQ.4 ) THEN
              SP_C(N,NSP) = 1.D+3*SP_C(N,NSP)*SG(2,N)*PORD(2,N)
!
!---        Convert solid-species mineral volumetric fraction
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
              DO 30 NRC = 1,NRCK
                IF( IRCKT(NRC).EQ.20 ) THEN
                  NSPX = IRC_K(1,NRC)
                ELSE
                  NSPRX = IRC_K(1,NRC)
                  NSPPX = IRC_K(2,NRC)
                  NSPX = IRC_K(3+NSPRX+NSPPX,NRC)
                ENDIF
                IF( NSP.EQ.NSPX ) THEN
                  NSPX = NSP-NSPL
                  IF( SP_S(2,NSPX)/EPSL.GT.EPSL ) THEN
                    SP_CMN(N,NSPX) = 1.D+3*RS_S(2,NSPX,N)*
     &                SP_S(1,NSPX)/SP_S(2,NSPX)
                  ENDIF
                  GOTO 40
                ENDIF
  30          CONTINUE
  40          CONTINUE
            ENDIF
          ENDIF
 100    CONTINUE
!
!---    Component species concentration  ---
!
        DO 200 NEQ = 1,NEQC
          NSL = NEQ + NSOLU
!
!---      Restart simulation  ---
!
          IF( IEO.EQ.2 ) THEN
!
!---        Convert reactive species from node volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            IF( ICT(N,NSL).EQ.11 ) THEN
              C(N,NSL) = 1.D+3*C(N,NSL)
!
!---        Convert reactive species from aqueous volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(N,NSL).EQ.12 ) THEN
              C(N,NSL) = 1.D+3*C(N,NSL)*SL(2,N)*PORD(2,N)
!
!---        Convert reactive species from aqueous molal, kmol/kg water
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(N,NSL).EQ.13 ) THEN
              C(N,NSL) = 1.D+3*C(N,NSL)*SL(2,N)*PORD(2,N)*
     &          RHOL(2,N)*XLW(2,N)
!
!---        Convert reactive species from gas volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(N,NSL).EQ.14 ) THEN
              C(N,NSL) = 1.D+3*C(N,NSL)*SG(2,N)*PORD(2,N)
            ELSE
              IEDL(NSL) = ISP_IEDL
              IF( IEQW.GT.0 ) THEN
                SMDL(NSL) = SP_MDL
                IF( ISP_IEDL.EQ.2 .OR. ISP_IEDL.EQ.4 ) THEN
                  SDCL(1,IZN,NSL) = SP_SDCL(1)
                  SDCL(2,IZN,NSL) = SP_SDCL(2)
                  SDCL(3,IZN,NSL) = SP_SDCL(3)
                ENDIF
              ENDIF
              IF( IEQA.GT.0 .OR. IOM.EQ.30 .OR. IOM.EQ.40 )
     &          SMDG(NSL) = SP_MDG
              IF( IEQO.GT.0 ) SMDN(NSL) = SP_MDN
              C(N,NSL) = 0.D+0
              DO 110 NSP = 1,IEQ_C(1,NEQ)
                C(N,NSL) = C(N,NSL)+EQ_C(NSP,NEQ)*
     &            SP_C(N,IEQ_C(NSP+1,NEQ))
 110          CONTINUE
            ENDIF
            CO(N,NSL) = C(N,NSL)
!
!---      Normal simulation  ---
!
          ELSE
!
!---        Convert reactive species from node volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            IF( ICT(N,NSL).EQ.1 ) THEN
              C(N,NSL) = 1.D+3*C(N,NSL)
!
!---        Convert reactive species from aqueous volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(N,NSL).EQ.2 ) THEN
              C(N,NSL) = 1.D+3*C(N,NSL)*SL(2,N)*PORD(2,N)
!
!---        Convert reactive species from aqueous molal, kmol/kg water
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(N,NSL).EQ.3 ) THEN
              C(N,NSL) = 1.D+3*C(N,NSL)*SL(2,N)*PORD(2,N)*
     &          RHOL(2,N)*XLW(2,N)
!
!---        Convert reactive species from gas volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(N,NSL).EQ.4 ) THEN
              C(N,NSL) = 1.D+3*C(N,NSL)*SG(2,N)*PORD(2,N)
            ELSE
              IEDL(NSL) = ISP_IEDL
              IF( IEQW.GT.0 ) THEN
                SMDL(NSL) = SP_MDL
                IF( ISP_IEDL.EQ.2 .OR. ISP_IEDL.EQ.4 ) THEN
                  SDCL(1,IZN,NSL) = SP_SDCL(1)
                  SDCL(2,IZN,NSL) = SP_SDCL(2)
                  SDCL(3,IZN,NSL) = SP_SDCL(3)
                ENDIF
              ENDIF
              IF( IEQA.GT.0 .OR. IOM.EQ.30 .OR. IOM.EQ.40 ) 
     &          SMDG(NSL) = SP_MDG
              IF( IEQO.GT.0 ) SMDN(NSL) = SP_MDN
              C(N,NSL) = 0.D+0
              DO 120 NSP = 1,IEQ_C(1,NEQ)
                C(N,NSL) = C(N,NSL)+EQ_C(NSP,NEQ)*
     &            SP_C(N,IEQ_C(NSP+1,NEQ))
 120          CONTINUE
            ENDIF
            CO(N,NSL) = C(N,NSL)
          ENDIF
 200    CONTINUE
!
!---    Kinetic species concentration  ---
!
        DO 300 NEQ = 1,NEQK
          NSL = NEQ + NEQC + NSOLU
!
!---      Restart simulation  ---
!
          IF( IEO.EQ.2 ) THEN
!
!---        Convert reactive species from node volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            IF( ICT(N,NSL).EQ.11 ) THEN
              C(N,NSL) = 1.D+3*C(N,NSL)
!
!---        Convert reactive species from aqueous volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(N,NSL).EQ.12 ) THEN
              C(N,NSL) = 1.D+3*C(N,NSL)*SL(2,N)*PORD(2,N)
!
!---        Convert reactive species from aqueous molal, kmol/kg water
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(N,NSL).EQ.13 ) THEN
              C(N,NSL) = 1.D+3*C(N,NSL)*SL(2,N)*PORD(2,N)*
     &          RHOL(2,N)*XLW(2,N)
!
!---        Convert reactive species from gas volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(N,NSL).EQ.14 ) THEN
              C(N,NSL) = 1.D+3*C(N,NSL)*SG(2,N)*PORD(2,N)
            ELSE
              IEDL(NSL) = ISP_IEDL
              IF( IEQW.GT.0 ) THEN
                SMDL(NSL) = SP_MDL
                IF( ISP_IEDL.EQ.2 .OR. ISP_IEDL.EQ.4 ) THEN
                  SDCL(1,IZN,NSL) = SP_SDCL(1)
                  SDCL(2,IZN,NSL) = SP_SDCL(2)
                  SDCL(3,IZN,NSL) = SP_SDCL(3)
                ENDIF
              ENDIF
              IF( IEQA.GT.0 .OR. IOM.EQ.30 .OR. IOM.EQ.40 ) 
     &          SMDG(NSL) = SP_MDG
              IF( IEQO.GT.0 ) SMDN(NSL) = SP_MDN
              C(N,NSL) = 0.D+0
              DO 220 NSP = 1,IEQ_K(1,NEQ)
                C(N,NSL) = C(N,NSL)+EQ_K(NSP,NEQ)*
     &            SP_C(N,IEQ_K(NSP+1,NEQ))
 220          CONTINUE
              CO(N,NSL) = C(N,NSL)
            ENDIF
!
!---      Normal simulation  ---
!
          ELSE
!
!---        Convert reactive species from node volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            IF( ICT(N,NSL).EQ.1 ) THEN
              C(N,NSL) = 1.D+3*C(N,NSL)
!
!---        Convert reactive species from aqueous volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(N,NSL).EQ.2 ) THEN
              C(N,NSL) = 1.D+3*C(N,NSL)*SL(2,N)*PORD(2,N)
!
!---        Convert reactive species from aqueous molal, kmol/kg water
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(N,NSL).EQ.3 ) THEN
              C(N,NSL) = 1.D+3*C(N,NSL)*SL(2,N)*PORD(2,N)*
     &          RHOL(2,N)*XLW(2,N)
!
!---        Convert reactive species from gas volumetric, kmol/m^3
!           to node volumetric, mol/m^3  ---
!
            ELSEIF( ICT(N,NSL).EQ.4 ) THEN
              C(N,NSL) = 1.D+3*C(N,NSL)*SG(2,N)*PORD(2,N)
            ELSE
              IEDL(NSL) = ISP_IEDL
              IF( IEQW.GT.0 ) THEN
                SMDL(NSL) = SP_MDL
                IF( ISP_IEDL.EQ.2 .OR. ISP_IEDL.EQ.4 ) THEN
                  SDCL(1,IZN,NSL) = SP_SDCL(1)
                  SDCL(2,IZN,NSL) = SP_SDCL(2)
                  SDCL(3,IZN,NSL) = SP_SDCL(3)
                ENDIF
              ENDIF
              IF( IEQA.GT.0 .OR. IOM.EQ.30 .OR. IOM.EQ.40 ) 
     &          SMDG(NSL) = SP_MDG
              IF( IEQO.GT.0 ) SMDN(NSL) = SP_MDN
              C(N,NSL) = 0.D+0
              DO 230 NSP = 1,IEQ_K(1,NEQ)
                C(N,NSL) = C(N,NSL)+EQ_K(NSP,NEQ)*
     &            SP_C(N,IEQ_K(NSP+1,NEQ))
 230          CONTINUE
              CO(N,NSL) = C(N,NSL)
            ENDIF
          ENDIF
 300    CONTINUE
 400  CONTINUE
!
!---  Assign old species concentrations  ---
!
      DO 510 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 510
        IZN = IZ(N)
!
!---    Loop over reactive species  ---
!
        DO 500 NSP = 1,NSPR
          SP_CO(N,NSP) = SP_C(N,NSP)
  500   CONTINUE
  510 CONTINUE
!
!---  Assign boundary solute concentrations for initial condition
!     type boundary conditions  ---
!
      DO 530 NSP = 1,NSPR
        DO 520 NB = 1,NBC
          N = IBCN(NB)
          SP_CBO(NB,NSP) = SP_C(N,NSP)
  520   CONTINUE
  530 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of FLHSP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE GAUSSJ( AX,BX,NX,NPX )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Numerical Recipes in Fortran 77, The Art of Scientific Computing
!     W.H. Press, B.P. Flannery, Saul A. Teukolsky, and W.T. Vetterling
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 29 September 2016.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 AX(NPX,NPX),BX(NPX)
      INTEGER IPIV(NPX),INDXR(NPX),INDXC(NPX)
!
!----------------------Executable Lines--------------------------------!
!
      EPSL = 1.D-14
      DO J = 1,NX
        IPIV(J) = 0
      ENDDO
      DO I = 1,NX
        BIG = 0.D+0
        DO J = 1,NX
          IF( IPIV(J).NE.1 ) THEN
            DO K = 1,NX
              IF( IPIV(K).EQ.0 ) THEN
                IF( ABS(AX(J,K)).GE.BIG ) THEN
                  BIG = ABS(AX(J,K))
                  IROW = J
                  ICOL = K
                ENDIF
              ELSEIF( IPIV(K).GT.1 ) THEN
                PRINT '(A)','Execution Error: GAUSSJ: Singular Matrix'
                STOP
              ENDIF
            ENDDO
          ENDIF
        ENDDO
        IPIV(ICOL) = IPIV(ICOL) + 1
        IF( IROW.NE.ICOL ) THEN
          DO L = 1,NX
            DUMX = AX(IROW,L)
            AX(IROW,L) = AX(ICOL,L)
            AX(ICOL,L) = DUMX
          ENDDO
          DUMX = BX(IROW)
          BX(IROW) = BX(ICOL)
          BX(ICOL) = DUMX
        ENDIF
        INDXR(I) = IROW
        INDXC(I) = ICOL
        IF( ABS(AX(ICOL,ICOL))/EPSL.LT.EPSL ) THEN
          PRINT '(A)','Execution Error: GAUSSJ: Singular Matrix'
          STOP
        ENDIF
        PIVINV = 1.D+0/AX(ICOL,ICOL)
        AX(ICOL,ICOL) = 1.D+0
        DO L = 1,NX
          AX(ICOL,L) = AX(ICOL,L)*PIVINV
        ENDDO
        BX(ICOL) = BX(ICOL)*PIVINV
        DO LL = 1,NX
          IF( LL.NE.ICOL ) THEN
            DUMX = AX(LL,ICOL)
            AX(LL,ICOL) = 0.D+0
            DO L = 1,NX
              AX(LL,L) = AX(LL,L) - AX(ICOL,L)*DUMX
            ENDDO
            BX(LL) = BX(LL) - BX(ICOL)*DUMX
          ENDIF
        ENDDO
      ENDDO
!      DO L = NX,1,-1
!        IF( INDXR(L).NE.INDXC(L) ) THEN
!          DO K = 1,NX
!            DUMX = AX(K,INDXR(L))
!            AX(K,INDXR(L)) = AX(K,INDXC(L))
!            AX(K,INDXC(L)) = DUMX
!          ENDDO
!        ENDIF
!      ENDDO
!
!---  End of GAUSSJ group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      FUNCTION GETSPNM( INDX )
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
!     Get species name from global species index.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 16 December 2004.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
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
      CHARACTER*64 GETSPNM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/GETSPNM'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Aqueous species  ---
!
      IF( INDX.LE.NSPL ) THEN
        GETSPNM = SPNML(INDX)
!
!---  Solid species  ---
!
      ELSEIF( INDX.LE.(NSPL+NSPS) ) THEN
        GETSPNM = SPNMS(INDX-NSPL)
!
!---  Exchange species  ---
!
      ELSEIF( INDX.LE.(NSPL+NSPS+NSPE) ) THEN
        GETSPNM = SPNME(INDX-NSPL-NSPS)
!
!---  Gas species  ---
!
      ELSEIF( INDX.LE.(NSPL+NSPS+NSPE+NSPG) ) THEN
        GETSPNM = SPNMG(INDX-NSPL-NSPS-NSPE)
!
!---  NAPL species  ---
!
      ELSEIF( INDX.LE.(NSPL+NSPS+NSPE+NSPG+NSPN) ) THEN
        GETSPNM = SPNMN(INDX-NSPL-NSPS-NSPE-NSPG)
      ELSE
        CHMSG = 'Unrecognized Global Species Index: '
        IMSG = INDX
        INDX = 12
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of GETSPNM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE GUSPCN( CX,SP_CX,N )
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
!     Guess specie concentrations using component rule of thumb,
!     equilibrium equations, and pH.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 26 January 2005.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 AJM(LEQC,LEQC),BJM(LEQC)
      REAL*8 CX(LEQC+LEQK),CIX(LEQC+LEQK)
      REAL*8 SP_CX(LSPR),SP_CIX(LSPR)
      REAL*8 ERRX(2),DERRX(2,LEQC),EQKX(LEQE)
!      INTEGER ICIX(LSPR)
      INTEGER INEG(LEQC),IPOS(LEQC)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/GUSPCN'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Total number of equations and species  ---
!
      NSPR = NSPG + NSPL + NSPN + NSPS + NSPE
!
!---  Volumetric concentration to molality  ---
!
      VTOMX = 1.D+0/(SL(2,N)*PORD(2,N)*RHOL(2,N)*XLW(2,N))
!
!---  Set initial concentrations  ---
!
      DO 10 NSP = 1,NSPR
        SP_CIX(NSP) = SP_CX(NSP)
   10 CONTINUE
!
!---  Set conservation species concentrations to 1/50 of
!     the total component concentration  ---
!
      NSPH = MOD(ISPLK(1),1000)
      DO 20 NEQ = 1,NEQC
        NSP = IEQ_C(2,NEQ)
        IF( NSP.EQ.NSPH ) GOTO 20
        SP_CIX(NSP) = MAX( CX(NEQ)*5.D-2,0.D+0 )
!        SP_CIX(NSP) = MAX( CX(NEQ)*5.D-2,CMIN )
   20 CONTINUE
!
!---  If uninitialized set pH to 1.D-7 molality, mol/kg water  ---
!
      IF( NSPH.GE.1 .AND. NSPH.LE.LSPR ) THEN
        IF( SP_CX(NSPH)/EPSL.LT.EPSL ) SP_CIX(NSPH) = 1.D-7/VTOMX
      ENDIF
!
!---  Equilibrium constant as a function of temperature  ---
!
      DO 40 NEQ = 1,NEQE
        NS = IEQ_E(1,NEQ)
        IRCE = IEQ_E((NS+2),NEQ)
        CALL EQCN( EQKX(NEQ),T(2,N),IRCE,N )
   40 CONTINUE
!
!---  Recalculate equilibrium species concentrations according to
!     the conservation species concentrations, assuming
!     an activity of 1.0  ---
!
      CALL EQEQ( EQKX,SP_CIX,VTOMX )
!
!---  Total error in conservation species concentration  ---
!
      DO 50 NEQ = 1,NEQC
        DERRX(1,NEQ) = 0.D+0
        CIX(NEQ) = 0.D+0
        DO 45 M = 1,IEQ_C(1,NEQ)
          NSP = IEQ_C(M+1,NEQ)
          CIX(NEQ) = CIX(NEQ)+EQ_C(M,NEQ)*SP_CIX(NSP)
   45   CONTINUE
        DERRX(1,NEQ) = ABS(CIX(NEQ)-CX(NEQ))/(CX(NEQ)+SMALL)
   50 CONTINUE
!
!---  Adjust conservation species concentrations by 1.667 and search
!     for improvements in total error in component species
!     concentrations  ---
!
      NC = 0
   60 CONTINUE
      NC = NC + 1
!
!---  Loop over conservation species  ---
!
      DO 200 NEQX = 1,NEQC
        NSPX = IEQ_C(2,NEQX)
!
!---    Initialize conservation species direction flags  ---
!
        IPOS(NEQX) = 0
        INEG(NEQX) = 0
!
!---  Skip for initial pH  ---
!
        IF( NSPX.EQ.NSPH .AND. ISPLK(1).GT.1000 ) GOTO 200
!
!---    Decrease conservation species concentration by 1.667  ---
!
!        CALL EQEQ( EQKX,SP_CIX,VTOMX )
        SP_CIX(NSPX) = SP_CIX(NSPX)/1.667D+0
!
!---    Recalculate equilibrium species concentrations according to
!       the conservation species concentrations, assuming
!       an activity of 1.0  ---
!
        CALL EQEQ( EQKX,SP_CIX,VTOMX )
!
!---    Total error in conservation species concentration  ---
!
        DO 110 NEQ = 1,NEQC
          DERRX(2,NEQ) = 0.D+0
          CIX(NEQ) = 0.D+0
          DO 100 M = 1,IEQ_C(1,NEQ)
            NSP = IEQ_C(M+1,NEQ)
            CIX(NEQ) = CIX(NEQ)+EQ_C(M,NEQ)*SP_CIX(NSP)
  100     CONTINUE
          DERRX(2,NEQ) = ABS(CIX(NEQ)-CX(NEQ))/(CX(NEQ)+SMALL)
  110   CONTINUE
!
!---    Check for improvement in total error in component species
!       concentration  ---
!
!        CERRX = (ERRX(1)-ERRX(2))/(ERRX(1)+SMALL)
        CERRX = (DERRX(1,NEQX)-DERRX(2,NEQX))/(DERRX(1,NEQX)+SMALL)
!
!---    Improvement found  ---
!
        IF( CERRX.GT.1.D-2 ) THEN
!          ERRX(1) = ERRX(2)
          DO 112 NEQ = 1,NEQC
            DERRX(1,NEQ) = DERRX(2,NEQ)
  112     CONTINUE
          INEG(NEQX) = 1
!
!---    No improvement found, increase conservation species
!       concentration ---
!
        ELSE
!          CALL EQEQ( EQKX,SP_CIX,VTOMX )
          SP_CIX(NSPX) = SP_CIX(NSPX)*(1.667D+0**2)
!
!---      Recalculate equilibrium species concentrations according to
!         the conservation species concentrations, assuming
!         an activity of 1.0  ---
!
          CALL EQEQ( EQKX,SP_CIX,VTOMX )
!
!---      Total error in component species concentration  ---
!
          DO 130 NEQ = 1,NEQC
            DERRX(2,NEQ) = 0.D+0
            CIX(NEQ) = 0.D+0
            DO 120 M = 1,IEQ_C(1,NEQ)
              NSP = IEQ_C(M+1,NEQ)
              CIX(NEQ) = CIX(NEQ)+EQ_C(M,NEQ)*SP_CIX(NSP)
  120       CONTINUE
            DERRX(2,NEQ) = ABS(CIX(NEQ)-CX(NEQ))/(CX(NEQ)+SMALL)
  130     CONTINUE
!
!---      Check for improvement in total error in component species
!         concentration  ---
!
          CERRX = (DERRX(1,NEQX)-DERRX(2,NEQX))/(DERRX(1,NEQX)+SMALL)
!
!---      Improvement found  ---
!
          IF( CERRX.GT.1.D-2 ) THEN
!            ERRX(1) = ERRX(2)
            DO 132 NEQ = 1,NEQC
              DERRX(1,NEQ) = DERRX(2,NEQ)
  132       CONTINUE
            IPOS(NEQX) = 1
!
!---      No improvement found, reset conservation species
!         concentration ---
!
          ELSE
            SP_CIX(NSPX) = SP_CIX(NSPX)/1.667D+0
          ENDIF
        ENDIF
  200 CONTINUE
!
!---  Loop over conservation species, checking for no
!     further improvements  ---
!
      DO 300 NEQ = 1,NEQC
        IF( IPOS(NEQ).EQ.1 .OR. INEG(NEQ).EQ.1 ) GOTO 60
  300 CONTINUE
!
!---  Set species concentration guesses  ---
!
      DO 400 NSP = 1,NSPR
        SP_CX(NSP) = MAX( SP_CIX(NSP),0.D+0 )
  400 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of GUSPCN group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE HOMIX(CPIX,APHI)
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
!     This subroutine calculates higher order mixing terms for
!     unsymmetrical electrolyte mixings.
!
!----------------------Authors-----------------------------------------!
!
!     Written by A. Felmy, from GMIN
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE PTZRCOEF
      USE PTZR
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
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/HOMIX'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
      MAXETH = LMCG**2
      DO I = 1,MAXETH
        ETH(I)=0.0D0
        ETHP(I)=0.0D0
        ETHP2(I)=0.0D0
      END DO
      IF( NCC_PZ.GE.2 )THEN
        NT = 1
        DO I = 1,NCC_PZ
          DO J = I+1,NCC_PZ
            IT = 1
            ZI = SP_L(1,JPC(I))
            ZJ = SP_L(1,JPC(J))
            IF( ZI.NE.ZJ ) THEN
              IT = INT(ZI*ZJ)
              IF( ETH(IT).EQ.0.0D0 ) CALL ELECTS(ZI,ZJ,IT,CPIX,APHI)
            END IF
            CTCPH(NT) = TCC(NT)+ETH(IT)+CPIX*ETHP(IT)
            CTC(NT) = TCC(NT)+ETH(IT)
            CTCPR(NT) = ETHP(IT)
            CTCPPR(NT) = ETHP2(IT)
            NT = NT+1
          END DO
        END DO
      END IF
      IF( NA_PZ.GE.2 )THEN
        NT = 1
        DO I = 1,NA_PZ
          DO J = I+1,NA_PZ
            IT = 1
            ZI = SP_L(1,JPA(I))
            ZJ = SP_L(1,JPA(J))
            IF( ZI.NE.ZJ )THEN
              IT = INT(ZI*ZJ)
              IF( ETH(IT).EQ.0.0D0) CALL ELECTS(ZI,ZJ,IT,CPIX,APHI)
            END IF
            CTAPH(NT) = TAA(NT)+ETH(IT)+CPIX*ETHP(IT)
            CTA(NT) = TAA(NT)+ETH(IT)
            CTAPR(NT) = ETHP(IT)
            CTAPPR(NT) = ETHP2(IT)
            NT = NT+1
          END DO
        END DO
      END IF
!
!---  End of HOMIX ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE IMOBCF( NEQ )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Add immobile species concentrations into component species
!     concentrations.
!
!     C(N,NSL) component species concentration (kmol/m^3 node)
!     SP_C(N,NSP) species concentration (kmol/m^3 node)
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, August 15, 2005.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE GRID
      USE FDVP
      USE FDVGC
      USE CONST
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



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/IMOBCF'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Loop over active nodes  ---
!
      DO 200 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 200
        NSL = NSOLU + NEQ
!
!---    Linked aqueous air   ---
!
        IF( ISPLK(4).EQ.NSL ) THEN
          COX = 1.D+3*XLA(1,N)*RHOL(1,N)*SL(1,N)*PORD(1,N)/WTMA
          CX = 1.D+3*XLA(2,N)*RHOL(2,N)*SL(2,N)*PORD(2,N)/WTMA
          C(N,NSL) = COX + (CX-COX)*DT/DT_RST
        ENDIF
!
!---    Linked aqueous CO2   ---
!
        IF( ISPLK(6).EQ.NSL ) THEN
          COX = 1.D+3*XLA(1,N)*RHOL(1,N)*SL(1,N)*PORD(1,N)/WTMA
          CX = 1.D+3*XLA(2,N)*RHOL(2,N)*SL(2,N)*PORD(2,N)/WTMA
          C(N,NSL) = COX + (CX-COX)*DT/DT_RST
        ENDIF
!
!---    Linked aqueous component   ---
!
        ISHIFTX=0
        IF( IOM.EQ.43 ) ISHIFTX=2
        DO IGC = 1,NGC+ISHIFTX
        IF( ISPLK(14+NSPLK+IGC).EQ.NSL ) THEN
          COX = 1.D+3*XLC(IGC,1,N)*RHOL(1,N)*SL(1,N)*PORD(1,N)/
     &      GCPP(1,IGC)
          CX = 1.D+3*XLC(IGC,2,N)*RHOL(2,N)*SL(2,N)*PORD(2,N)/
     &      GCPP(1,IGC)
          C(N,NSL) = COX + (CX-COX)*DT/DT_RST
        ENDIF
        END DO
!
!---    Linked gas component   ---
!
        ISHIFTX=0
        IF( IOM.EQ.43 ) ISHIFTX=2
        DO IGC = 1,NGC+ISHIFTX
        IF( ISPLK(14+NSPLK+NGC+ISHIFTX+IGC).EQ.NSL ) THEN
          COX = 1.D+3*XGC(IGC,1,N)*RHOG(1,N)*SG(1,N)*PORD(1,N)/
     &      GCPP(1,IGC)
          CX = 1.D+3*XGC(IGC,2,N)*RHOG(2,N)*SG(2,N)*PORD(2,N)/
     &      GCPP(1,IGC)
          C(N,NSL) = COX + (CX-COX)*DT/DT_RST
        ENDIF
        END DO
!
!---    Loop over conservation-component species  ---
!
        DO 100 M = 1,IEQ_C(1,NEQ)
          NSP = IEQ_C(M+1,NEQ)
!
!---      Solid and exchange species ---
!
          IF( NSP.GT.NSPL .AND. NSP.LE.(NSPL+NSPS+NSPE) ) THEN
            IF( ABS(SP_C(N,NSP)).LT.CMIN ) THEN
              SP_CX = 0.D+0
            ELSE
              SP_CX = SP_C(N,NSP)
            ENDIF
            C(N,NSL) = C(N,NSL) + EQ_C(M,NEQ)*SP_CX
          ENDIF
  100   CONTINUE
!
!---    Update old time step conservation-component species ---
!
        CO(N,NSL) = C(N,NSL)
  200 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of IMOBCF group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE IMOBKF( NEQ )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Mobile kinetic component fractions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, August 15, 2005.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE GRID
      USE FDVP
      USE FDVGC
      USE CONST
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



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/IMOBKF'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Convert to global component indices  ---
!
      NEQX = NEQ + NEQC
!
!---  Loop over active nodes  ---
!
      DO 200 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 200
        NSL = NSOLU + NEQX
!
!---    Linked aqueous air   ---
!
        IF( ISPLK(4).EQ.NSL ) THEN
          COX = 1.D+3*XLA(1,N)*RHOL(1,N)*SL(1,N)*PORD(1,N)/WTMA
          CX = 1.D+3*XLA(2,N)*RHOL(2,N)*SL(2,N)*PORD(2,N)/WTMA
          C(N,NSL) = COX + (CX-COX)*DT/DT_RST
        ENDIF
!
!---    Linked aqueous CO2   ---
!
        IF( ISPLK(6).EQ.NSL ) THEN
          COX = 1.D+3*XLA(1,N)*RHOL(1,N)*SL(1,N)*PORD(1,N)/WTMA
          CX = 1.D+3*XLA(2,N)*RHOL(2,N)*SL(2,N)*PORD(2,N)/WTMA
          C(N,NSL) = COX + (CX-COX)*DT/DT_RST
        ENDIF
!
!---    Linked aqueous component   ---
!
        ISHIFTX=0
        IF( IOM.EQ.43 ) ISHIFTX=2
        DO IGC = 1,NGC+ISHIFTX
        IF( ISPLK(14+NSPLK+IGC).EQ.NSL ) THEN
          COX = 1.D+3*XLC(IGC,1,N)*RHOL(1,N)*SL(1,N)*PORD(1,N)/
     &      GCPP(1,IGC)
          CX = 1.D+3*XLC(IGC,2,N)*RHOL(2,N)*SL(2,N)*PORD(2,N)/
     &      GCPP(1,IGC)
          C(N,NSL) = COX + (CX-COX)*DT/DT_RST
        ENDIF
        END DO
!
!---    Linked gas component   ---
!
        ISHIFTX=0
        IF( IOM.EQ.43 ) ISHIFTX=2
        DO IGC = 1,NGC+ISHIFTX
        IF( ISPLK(14+NSPLK+NGC+ISHIFTX+IGC).EQ.NSL ) THEN
          COX = 1.D+3*XGC(IGC,1,N)*RHOG(1,N)*SG(1,N)*PORD(1,N)/
     &      GCPP(1,IGC)
          CX = 1.D+3*XGC(IGC,2,N)*RHOG(2,N)*SG(2,N)*PORD(2,N)/
     &      GCPP(1,IGC)
          C(N,NSL) = COX + (CX-COX)*DT/DT_RST
        ENDIF
        END DO
!
!---    Loop over kinetic-component species  ---
!
        DO 100 M = 1,IEQ_K(1,NEQ)
          NSP = IEQ_K(M+1,NEQ)
!
!---      Solid and exchange species ---
!
          IF( NSP.GT.NSPL .AND. NSP.LE.(NSPL+NSPS+NSPE) ) THEN
            IF( ABS(SP_C(N,NSP)).LT.CMIN ) THEN
              SP_CX = 0.D+0
            ELSE
              SP_CX = SP_C(N,NSP)
            ENDIF
            C(N,NSL) = C(N,NSL) + EQ_K(M,NEQ)*SP_CX
          ENDIF
  100   CONTINUE
!
!---    Update old time step kinetic-component species ---
!
        CO(N,NSL) = C(N,NSL)
  200 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of IMOBKF group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE KECHEM( ACTVX,AJM,BJM,COX,SP_CX,DSP_CX,N,NEQ,INDX,IER )
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
!     Kinetic Equation CHEMistry
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 January 2005.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE PORMED
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 ACTVX(LSPL+1,LSPL)
      REAL*8 BJM(LSPR),AJM(LSPR,LSPR)
      REAL*8 SP_CX(LSPR),DSP_CX(LSPR)
      REAL*8 COX(LEQC+LEQK)
      REAL*8 EQKX(LREK),RRBX(LREK),RRCX(LREK)
      LOGICAL FCHK
      CHARACTER*64 GETSPNM
      EXTERNAL GETSPNM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/KECHEM'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Skip for initial pH  ---
!
      IF( ISPLK(1).GT.1000 ) THEN
        IF( NEQ.EQ.IEQ_S(MOD(ISPLK(1),1000)) .AND.
     &    (NSTEP-NRST).EQ.0 ) THEN
          AJM(NEQ,NEQ) = 1.D+0
          BJM(NEQ) = 0.D+0
          GOTO 1000
        ENDIF
      ENDIF
!
!---  Fixed species concentration or activity  ---
!
      DO 10 NSLKX = 1,NSPLK
        IF( ISPLK(14+NSLKX).LT.0 ) THEN
          NSPX = ABS(ISPLK(14+NSLKX))
          IF( NSPX.GT.1000 ) NSPX = NSPX - 1000
          IF( NEQ.EQ.IEQ_S(NSPX) ) THEN
            AJM(NEQ,NEQ) = 1.D+0
            BJM(NEQ) = 0.D+0
            GOTO 1000
          ENDIF
        ENDIF
   10 CONTINUE
!
!---  Volumetric concentration to molality, mol/m^3 -> mol/kg aqu  ---
!
      VTOMX = 1.D+0/(SL(2,N)*PORD(2,N)*RHOL(2,N)*XLW(2,N))
!
!---  Volumetric concentration to aqueous concentration,
!     mol/m^3 -> mol/m^3 aqu  ---
!
      VTOLX = 1.D+0/(SL(2,N)*PORD(2,N))
!
!---  Volumetric concentration to sorbed concentration,
!     mol/m^3 -> mol/kg sol  ---
!
      VTOSX = 1.D+0/((1.D+0-PORT(2,N))*RHOS(IZ(N)))
!
!---  Total number of species  ---
!
      NEQX = NEQ - NEQE - NEQC
      NSPR = NSPG + NSPL + NSPN + NSPS + NSPE
!
!---  Number of species and reactions in kinetic equation  ---
!
      NS = IEQ_K(1,NEQX)
      NR = IEQ_K(NS+2,NEQX)
      IF( (NSTEP-NRST).EQ.0 ) THEN
        NR = 0
        NSPRX = 0
        NSPPX = 0
        NSPKX = 0
      ENDIF
!
!---  Find aqueous silica
!
      DO M = NSPG+1,NSPG+NSPL
        IF (GETSPNM(M) == 'sio2(aq)') NSPSI = M
      END DO
!
!---  Loop over kinetic reactions in kinetic equation to
!     determine rate constants  ---
!
      DO 100 M = 1,NR
!
!---    Reaction index, number of reactants, number of products  ---
!
        IRCX = IEQ_K(NS+2+M,NEQX)
        NSPRX = IRC_K(1,IRCX)
        NSPPX = IRC_K(2,IRCX)
        NSPKX = NSPRX+NSPPX
!
!---    Dissolution-precipitation kinetic reaction  ---
!
        IF( (IRCKT(IRCX).GE.10 .AND. IRCKT(IRCX).LE.12) .OR.
     &    (IRCKT(IRCX).EQ.14) .OR.
     &    (IRCKT(IRCX).GE.5 .AND. IRCKT(IRCX).LE.9) ) THEN
!
!---      Equilibrium constants as a function of temperature  ---
!
          IRCX = -IRCX
          CALL EQCN( EQKX(M),T(2,N),IRCX,N )
          IRCX = -IRCX
!
!---      Reaction rate constants as a function of temperature
!         mol/m^2 s  ---
!
          TKX = T(2,N)+TABS
          N1 = MAX(1,N*IRCKN(NSPKX+1))
          N2 = MAX(1,N*IRCKN(NSPKX+2))
          N3 = MAX(1,N*IRCKN(NSPKX+3))
          TKRX = RC_K(NSPKX+3,N3,IRCX)+TABS
          RRCX(M) = RC_K(NSPKX+1,N1,IRCX)*EXP( -RC_K(NSPKX+2,N2,IRCX)*
     &      ((1.D+0/TKX)-(1.D+0/TKRX))/(1.D-3*RCU) )
        ENDIF
  100 CONTINUE
!
!---  Base residual  ---
!
      BJM(NEQ) = 0.D+0
      RSBX = 0.D+0
!
!---  Loop over kinetic reactions  ---
!
      DO 290 M = 1,NR
!
!---    Reaction index, number of reactants, number of products  ---
!
        IRCX = IEQ_K(NS+2+M,NEQX)
        NSPRX = IRC_K(1,IRCX)
        NSPPX = IRC_K(2,IRCX)
        NSPKX = NSPRX+NSPPX
!
!---    TST kinetic reaction  ---
!
        IF( (IRCKT(IRCX).GE.10 .AND. IRCKT(IRCX).LE.12) .OR.
     &    (IRCKT(IRCX).EQ.14) .OR.
     &    (IRCKT(IRCX).GE.5 .AND. IRCKT(IRCX).LE.9) ) THEN
!
!---      Ion activity product mol/kg water, loop over species in
!         kinetic reaction  ---
!
          QX = 1.D+0
!
!---      Glass equilibrium dependent on aqueous silica only
!
          IF (IRCKT(IRCX).EQ.14) THEN
            CMX = SP_CX(NSPSI)*VTOMX
            ACX = ACTVX(1,NSPSI)
            QX = QX*(CMX*ACX)
          ELSE
!
!---      Loop over species in kinetic reaction  ---
!
          DO 120 L = 1,NSPKX
            NSPX = IRC_K(L+2,IRCX)
!
!---        Aqueous species,
!           concentration in molality, mol/kg H2O  ---
!
            IF( NSPX.LE.NSPL ) THEN
              CMX = SP_CX(NSPX)*VTOMX
              ACX = ACTVX(1,NSPX)
!
!---          Reactants  ---
!
              N1 = MAX(1,N*IRCKN(L))
              IF( L.LE.NSPRX ) THEN
                QX = QX*((CMX*ACX)**RC_K(L,N1,IRCX))
!
!---          Products  ---
!
              ELSE
                QX = QX/((CMX*ACX)**RC_K(L,N1,IRCX))
              ENDIF
!
!---      CFMX is scaling factor to translate between pore-scale
!---      and macro-scale simulations.  Default = 1
!
              IF (ISLC(58).EQ.1) THEN
                QX = CFMX(N)*QX
              ENDIF
!
!---        Solid species, skip  ---
!
            ELSEIF( NSPX.LE.NSPL+NSPS ) THEN
              GOTO 120
            ENDIF
  120     CONTINUE
          ENDIF
!
!---      Initial reactive surface area, initial mineral volume 
!         fraction, current mineral volume fraction, minimum current 
!         mineral volume fraction allows re-precipitation of dissolved 
!         primary minerals NSP_M - mineral species number  ---
!
          NSPX = IRC_K(3+NSPKX,IRCX)
          NSP_M =  NSPX - NSPL
!
!---      Primary mineral  ---
!
          IF( RS_S(2,NSP_M,N).GT.EPSL ) THEN
            AOX = RS_S(1,NSP_M,N)*VOL(N)*RS_S(2,NSP_M,N)*SP_S(1,NSP_M)
            VFMOX = RS_S(2,NSP_M,N)
            IF( ISP_MN(NSPX).EQ.1 ) THEN
              VFMX = 1.D-3*(SP_CMN(N,NSP_M)+SP_CX(NSPX))
     &          *SP_S(2,NSP_M)/SP_S(1,NSP_M)
            ELSE
              VFMX = 1.D-3*SP_CX(NSPX)*SP_S(2,NSP_M)/SP_S(1,NSP_M)
            ENDIF
            IF( (IRCKT(IRCX).NE.7) .AND. (IRCKT(IRCX).NE.9) .AND.
     &          (IRCKT(IRCX).NE.12) .AND. (IRCKT(IRCX).NE.14) )
     &      VFMX = MAX( VFMX,1.D-5 )
     
!
!---      Secondary mineral, initial reactive surface area
!         for seconary minerals is set to 0.25 m^2/dm^3  ---
!
          ELSE
            IF( RS_S(1,NSP_M,N).GT.EPSL ) THEN
              VFMOX = 1.D-5
              AOX = RS_S(1,NSP_M,N)*VOL(N)*VFMOX*SP_S(1,NSP_M)
            ELSE
              AOX = 0.25D+3*VOL(N)
              VFMOX = 1.D-2
            ENDIF
            VFMX = 1.D-3*SP_CX(NSPX)*SP_S(2,NSP_M)/SP_S(1,NSP_M)
            IF( (IRCKT(IRCX).NE.7) .AND. (IRCKT(IRCX).NE.9) .AND.
     &          (IRCKT(IRCX).NE.12) .AND. (IRCKT(IRCX).NE.14) )
     &      VFMX = MAX( VFMX,1.D-5 )
          ENDIF
          VFMX = MAX( VFMX,0.D+0 )
!
!---      Reactive surface area  ---
!
          AX = AOX*(((POR0(2,N)*VFMX)/
     &      (POR(2,IZ(N))*VFMOX))**(2.D+0/3.D+0))
          IF( ISLC(56).EQ.1 ) AX = AX * SL(2,N)
          IF( ISLC(56).EQ.2 ) AX = AOX
          IF( ISLC(58).EQ.1 ) AX = 1.0D+0
!
!---      Reaction rate, mol/s  ---
!
          RRBX(M) = -AX*RRCX(M)*(1.D+0-(QX/EQKX(M)))
          IF( RRBX(M).NE.RRBX(M) ) THEN
            IER = -1
            RETURN
          ENDIF
!
!---      pH dependence  ---
!
          IF( ((IRCKT(IRCX).GE.5 .AND. IRCKT(IRCX).LE.9)
     &    .OR. (IRCKT(IRCX).EQ.14))
     &      .AND. ISPLK(1).NE.0 ) THEN
            NSP_PHX = MOD(ISPLK(1),1000)
            PHX = -LOG10(1.D-3*SP_CX(NSP_PHX)*VTOLX)
            IF( IRCKT(IRCX).GE.8 .AND. IRCKT(IRCX).LE.9 ) THEN
              RRBX(M) = RRBX(M)*MAX( 0.D+0,
     &          (7.9201D-1 - 1.3479D-1*PHX + 5.2D-3*(PHX**2)))
            ELSE
              N9 = MAX(1,N*IRCKN(NSPKX+9))
              RRBX(M) = RRBX(M)*(1.D+1**(-RC_K(NSPKX+9,N9,IRCX)*PHX))
            ENDIF
          ENDIF
!
!---      Reaction rate, mol/m^3 aqu s  ---
!
          RRBX(M) = RRBX(M)*VTOLX/VOL(N)
          SP_AREA(N,NSP_M) = AX
          NSP_MIN = IEQ_K(2,NEQX)-NSPL
          IF (M.EQ.1) THEN
            SP_RATE(N,NSP_MIN) = RRBX(M)/VTOLX*VOL(N) * EQ_K(M+1,NEQX)
          ELSE
            SP_RATE(N,NSP_MIN) = SP_RATE(N,NSP_MIN) 
     &                         + RRBX(M)/VTOLX*VOL(N) * EQ_K(M+1,NEQX)
          ENDIF
!
!---      Direction limited  ---
!
          IF( IRCKT(IRCX).EQ.6 .OR. IRCKT(IRCX).EQ.8
     &      .OR. IRCKT(IRCX).EQ.11 ) THEN
            RRBX(M) = MAX( RRBX(M),0.D+0 )
          ELSEIF( IRCKT(IRCX).EQ.7 .OR. IRCKT(IRCX).EQ.9
     &      .OR. IRCKT(IRCX).EQ.12 .OR. IRCKT(IRCX).EQ.14 ) THEN
            RRBX(M) = MIN( RRBX(M),0.D+0 )
          ENDIF
!
!---    Forward-backward kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.1 ) THEN
          N1 = MAX(1,N*IRCKN(NSPKX+1))
          N2 = MAX(1,N*IRCKN(NSPKX+2))
          FRRX = RC_K(NSPKX+1,N1,IRCX)
          BRRX = RC_K(NSPKX+2,N2,IRCX)
!
!---      Loop over reactants  ---
!
          DO 140 L = 1,NSPRX
!
!---        Global species index  ---
!
            NSPX = IRC_K(L+2,IRCX)
            CMX = SP_CX(NSPX)*VTOLX
            N1 = MAX(1,N*IRCKN(L))
            FRRX = FRRX*(CMX**RC_K(L,N1,IRCX))
  140     CONTINUE
!
!---      Loop over products  ---
!
          DO 160 L = 1,NSPPX
!
!---        Global species index  ---
!
            NSPX = IRC_K(L+2+NSPRX,IRCX)
            CMX = SP_CX(NSPX)*VTOLX
            N1 = MAX(1,N*IRCKN(L+NSPRX))
            BRRX = BRRX*(CMX**RC_K(L+NSPRX,N1,IRCX))
  160     CONTINUE
          RRBX(M) = FRRX - BRRX
!
!---    Valocchi-Monod kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.2 ) THEN
!
!---      Concentration of biomass in mol/m^3 aqu  ---
!
          NSPX = IRC_K(5,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
!
!---      Partial rate of donor degredation  ---
!
          N6 = MAX(1,N*IRCKN(6))
          RRBX(M) = RC_K(6,N6,IRCX)*CMX
!
!---      Concentration of donor in mol/kg water  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Partial rate of donor degredation  ---
!
          N4 = MAX(1,N*IRCKN(4))
          RRBX(M) = RRBX(M)*(CMX/(RC_K(4,N4,IRCX)+CMX))
!
!---      Concentration of acceptor in mol/kg water  ---
!
          NSPX = IRC_K(4,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Rate of donor degredation  ---
!
          N5 = MAX(1,N*IRCKN(5))
          RRBX(M) = RRBX(M)*(CMX/(RC_K(5,N5,IRCX)+CMX))
!
!---    Valocchi-Sorption kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.3 ) THEN
!
!---      Concentration of sorbed species in mol/kg soil  ---
!
          NSPX = IRC_K(4,IRCX)
!         CMX = SP_CX(NSPX)*VTOSX*1.D-3
          CMX = SP_CX(NSPX)*VTOSX
          N4 = MAX(1,N*IRCKN(4))
          RRBX(M) = CMX/RC_K(4,N4,IRCX)
!
!---      Concentration of aqueous species in mol/m^3 water  ---
!
          NSPX = IRC_K(3,IRCX)
!         CMX = SP_CX(NSPX)*VTOMX
          CMX = SP_CX(NSPX)*VTOLX
!
!---      Rate of sorption in mol/m^3 aqu s  ---
!
!         RRBX(M) = RC_K(3,IRCX)*(CMX-RRBX(M))*1.D+3
          N3 = MAX(1,N*IRCKN(3))
          RRBX(M) = RC_K(3,N3,IRCX)*(CMX-RRBX(M))
!
!---    Langmuir-Sorption kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.13 ) THEN
!
!---      Concentration of sorbed species in mol/kg soil  ---
!
          NSPX = IRC_K(4,IRCX)
          CSX = SP_CX(NSPX)*VTOSX
!
!---      Concentration of aqueous species in mol/m^3 water  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
!
!---      Rate of sorption in mol/m^3 aqu s  ---
!
          N3 = MAX(1,N*IRCKN(3))
          N4 = MAX(1,N*IRCKN(4))
          N5 = MAX(1,N*IRCKN(5))
          RRBX(M) = RC_K(3,N3,IRCX)*CMX*(RC_K(5,N5,IRCX)-CSX)-
     &      RC_K(4,N4,IRCX)*CSX
!
!---    Valocchi-Biomass kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.4 ) THEN
!
!---      Concentration of biomass in mol/m^3 aqu  ---
!
          NSPX = IRC_K(5,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
!
!---      Partial rate of donor degredation  ---
!
          N6 = MAX(1,N*IRCKN(6))
          RRBX(M) = RC_K(6,N6,IRCX)*CMX
!
!---      Concentration of donor in mol/m^3 aqu  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Partial rate of donor degredation  ---
!
          N4 = MAX(1,N*IRCKN(4))
          RRBX(M) = RRBX(M)*(CMX/(RC_K(4,N4,IRCX)+CMX))
!
!---      Concentration of acceptor in mol/m^3 aqu  ---
!
          NSPX = IRC_K(4,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Rate of donor degredation  ---
!
          N5 = MAX(1,N*IRCKN(5))
          RRBX(M) = RRBX(M)*(CMX/(RC_K(5,N5,IRCX)+CMX))
!
!---      Concentration of biomass in mol/m^3 aqu  ---
!
          NSPX = IRC_K(5,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
!
!---      Rate of biomass production in mol/m^3 aqu s  ---
!
          N7 = MAX(1,N*IRCKN(7))
          N8 = MAX(1,N*IRCKN(8))
          RRBX(M) = RC_K(7,N7,IRCX)*RRBX(M) - RC_K(8,N8,IRCX)*CMX
!
!---    Emulsion- or oil-sorption kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.15 ) THEN
!
!---      Neighborhood-particle dimensionless number  ---
!
          GAMMAX = (MAX( 1.D+0-PORD(2,N),0.D+0 ))**(THIRD)
          ASX = 2.D+0*(1.D+0-(GAMMAX**5))/(2.D+0 - 3.D+0*GAMMAX
     &      + 3.D+0*(GAMMAX**5) - 2.D+0*(GAMMAX**6))
!
!---      Hamaker constant (J)  ---
!
          CHX = 1.D-20
!
!---      Bolztmann constant (kg m^2/s^2 K)  ---
!
          CBX = 1.38D-23
!
!---      Average aqueous velocity (m/s)  ---
!
          ULAVX = 0.D+0
          NC = 0
          NPX = NSX(N)
          IF( ABS(UL(1,NPX)).GT.EPSL ) THEN
            NC = NC+1
            ULAVX = ULAVX + UL(1,NPX)
          ENDIF
          NQX = NSX(N)+1
          IF( ABS(UL(1,NQX)).GT.EPSL ) THEN
            NC = NC+1
            ULAVX = ULAVX + UL(1,NQX)
          ENDIF
          NPY = NSY(N)
          IF( ABS(VL(1,NPY)).GT.EPSL ) THEN
            NC = NC+1
            ULAVX = ULAVX + VL(1,NPY)
          ENDIF
          NQY = NSY(N)+IFLD
          IF( ABS(VL(1,NQY)).GT.EPSL ) THEN
            NC = NC+1
            ULAVX = ULAVX + VL(1,NQY)
          ENDIF
          NPZ = NSZ(N)
          IF( ABS(WL(1,NPZ)).GT.EPSL ) THEN
            NC = NC+1
            ULAVX = ULAVX + WL(1,NPZ)
          ENDIF
          NQZ = NSZ(N)+IJFLD
          IF( ABS(WL(1,NQZ)).GT.EPSL ) THEN
            NC = NC+1
            ULAVX = ULAVX + WL(1,NQZ)
          ENDIF
          ULAVX = ULAVX/REAL(NC)
!
!---      London - van der Waals dimensionless number  ---
!
          N6 = MAX(1,N*IRCKN(6))
          DNLOX = 4.D+0*CHX/
     &      (9.D+0*GPI*VISL(2,N)*(RC_K(6,N6,IRCX)**2)*ULAVX)
!
!---      Inception dimensionless number  ---
!
          N3 = MAX(1,N*IRCKN(3))
          DNRX = RC_K(6,N6,IRCX)/RC_K(3,N3,IRCX)
!
!---      Sedimentation dimensionless number  ---
!
          N7 = MAX(1,N*IRCKN(7))
          DNGX = GRAV*(RHOL(2,N)-RC_K(7,N7,IRCX))*(RC_K(6,N6,IRCX)**2)
     &      /(1.8D+1*VISL(2,N)*ULAVX)
!
!---      Diffusion dimensionless number  ---
!
          DNPEX = 3.D+0*GPI*VISL(2,N)*ULAVX*RC_K(3,N3,IRCX)
     &      *RC_K(6,N6,IRCX)/(CBX*(T(2,N)+TABS))
!
!---      Single collector efficiency  ---
!
          ETAX = ASX*((DNLOX**(1.D+0/8.D+0))*(DNRX**(1.5D+1/8.D+0)) +
     &      3.38D-3*(DNGX**1.2D+0)*(DNRX**(-4.D-1)) +
     &      4.D+0*(DNPEX**(-2.D+0/3.D+0)))
!
!---      Concentration of immobile oil (kg oil/kg soil)  ---
!
          NSPX = IRC_K(3,IRCX)
          WTMX = SP_S(2,(NSPX-NSPL))
          CIMX = 1.D-3*SP_CX(NSPX)*VTOSX*WTMX
!
!---      Concentration of mobile oil (kg oil/m^3 aqu)  ---
!
          NSPX = IRC_K(4,IRCX)
          CMX = 1.D-3*SP_CX(NSPX)*VTOLX*WTMX
!
!---      Rate of immobile-oil production in kg oil/kg soil s  ---
!
          N4 = MAX(1,N*IRCKN(4))
          N5 = MAX(1,N*IRCKN(5))
          RRBX(M) = 3.D+0*ULAVX*ETAX*MAX(1.D+0-PORD(2,N),0.D+0)*
     &      RC_K(4,N4,IRCX)*MAX(RC_K(5,N5,IRCX)-CIMX,0.D+0)*CMX/
     &      (2.D+0*RC_K(3,N3,IRCX)*RC_K(5,N5,IRCX))
!
!---      Rate of immobile-oil production in mol/m^3 aqu s  ---
!
          RRBX(M) = 1.D+3*RRBX(M)*VTOLX/(VTOSX*WTMX)
!
!---    Multirate  ---
!
        ELSEIF( IRCKT(IRCX).EQ.20 ) THEN
!
!---      Neutral reaction rate, mol/m^2 s  ---
!
          TKX = T(2,N)+TABS
          N1 = MAX(1,N*IRCKN(1))
          N2 = MAX(1,N*IRCKN(2))
          N3 = MAX(1,N*IRCKN(3))
          TKRX = RC_K(3,N3,IRCX)+TABS
          RRCX(M) = RC_K(1,N1,IRCX)*EXP( -RC_K(2,N2,IRCX)*
     &      ((1.D+0/TKX)-(1.D+0/TKRX))/(1.D-3*RCU) )
!
!---      Loop over mechanisms  ---
!
          DO 190 NKRMX = 1,IRC_K(2,IRCX)
            IX = 3+((NKRMX-1)*6)
!
!---        Ion activity product mol/kg water, loop over species  ---
!
            QX = 1.D+0
            DO 180 L = 1,IRC_K(IX,IRCX)
              IX = 3+((NKRMX-1)*6)+L
              NX = MAX(1,N*IRCKN(IX))
              NSPX = IRC_K(IX,IRCX)
!
!---          Aqueous species,
!             concentration in molality, mol/kg H2O  ---
!
              IF( NSPX.LE.NSPL ) THEN
                CMX = SP_CX(NSPX)*VTOMX
                ACX = ACTVX(1,NSPX)
                IX = 6+((NKRMX-1)*8)+L
                QX = QX*((CMX*ACX)**RC_K(IX,NX,IRCX))
              ENDIF
  180       CONTINUE
            IX = 4+((NKRMX-1)*8)
            NX = MAX(1,N*IRCKN(IX))
            N1 = MAX(1,N*IRCKN(IX+1))
            N2 = MAX(1,N*IRCKN(IX+2))
            TKRX = RC_K(IX+2,N2,IRCX)+TABS
            RRCX(M) = RRCX(M) + RC_K(IX,NX,IRCX)
     &        *EXP( -RC_K(IX+1,N1,IRCX)*
     &      ((1.D+0/TKX)-(1.D+0/TKRX))/(1.D-3*RCU) )*QX
  190     CONTINUE
!
!---      Initial reactive surface area, initial mineral volume 
!         fraction, current mineral volume fraction, minimum current
!         mineral volume fraction allows re-precipitation of dissolved
!         primary minerals NSP_M - mineral species number  ---
!
          NSPX = IRC_K(1,IRCX)
          NSP_M =  NSPX - NSPL
!
!---      Primary mineral  ---
!
          IF( RS_S(2,NSP_M,N).GT.EPSL ) THEN
            AOX = RS_S(1,NSP_M,N)*VOL(N)*RS_S(2,NSP_M,N)*SP_S(1,NSP_M)
            VFMOX = RS_S(2,NSP_M,N)
            IF( ISP_MN(NSPX).EQ.1 ) THEN
              VFMX = 1.D-3*(SP_CMN(N,NSP_M)+SP_CX(NSPX))
     &          *SP_S(2,NSP_M)/SP_S(1,NSP_M)
            ELSE
              VFMX = 1.D-3*SP_CX(NSPX)*SP_S(2,NSP_M)/SP_S(1,NSP_M)
            ENDIF
            IF( (IRCKT(IRCX).NE.7) .AND. (IRCKT(IRCX).NE.9) .AND.
     &          (IRCKT(IRCX).NE.12) .AND. (IRCKT(IRCX).NE.14) )
     &      VFMX = MAX( VFMX,1.D-5 )
!
!---      Secondary mineral, initial reactive surface area
!         for seconary minerals is set to 0.25 m^2/dm^3  ---
!
          ELSE
            IF( RS_S(1,NSP_M,N).GT.EPSL ) THEN
              VFMOX = 1.D-5
              AOX = RS_S(1,NSP_M,N)*VOL(N)*VFMOX*SP_S(1,NSP_M)
            ELSE
              AOX = 0.25D+3*VOL(N)
              VFMOX = 1.D-2
            ENDIF
            VFMX = 1.D-3*SP_CX(NSPX)*SP_S(2,NSP_M)/SP_S(1,NSP_M)
            IF( (IRCKT(IRCX).NE.7) .AND. (IRCKT(IRCX).NE.9) .AND.
     &          (IRCKT(IRCX).NE.12) .AND. (IRCKT(IRCX).NE.14) )
     &      VFMX = MAX( VFMX,1.D-5 )
          ENDIF
          VFMX = MAX( VFMX,0.D+0 )
!
!---      Reactive surface area  ---
!
          AX = AOX*(((POR0(2,N)*VFMX)/
     &      (POR(2,IZ(N))*VFMOX))**(2.D+0/3.D+0))
          IF( ISLC(56).EQ.1 ) AX = AX * SL(2,N)
          IF( ISLC(56).EQ.2 ) AX = AOX
!
!---      Reaction rate, mol/s  ---
!
          RRBX(M) = -AX*RRCX(M)
!
!---      Reaction rate, mol/m^3 aqu s  ---
!
          RRBX(M) = RRBX(M)*VTOLX/VOL(N)
          SP_AREA(N,NSP_M) = AX
          NSP_MIN = IEQ_K(2,NEQX)-NSPL
          IF (M.EQ.1) THEN
            SP_RATE(N,NSP_MIN) = RRBX(M)/VTOLX*VOL(N) * EQ_K(M+1,NEQX)
          ELSE
            SP_RATE(N,NSP_MIN) = SP_RATE(N,NSP_MIN) 
     &                         + RRBX(M)/VTOLX*VOL(N) * EQ_K(M+1,NEQX)
          ENDIF
!
!---    Monod kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.22 ) THEN
          JCX = 0
          RRBX(M) = 1.D+0
!
!---      Loop over the number of reactants, less one  ---
!
          DO 200 NSP = 1,NSPRX-1
!
!---        Concentration of reactant in mol/m^3 aqu  ---
!
            NSPX = IRC_K(NSP+3,IRCX)
            CMX = SP_CX(NSPX)*VTOLX
!
!---        Partial rate of donor degredation  ---
!
            JCX = JCX+1
            N1 = MAX(1,N*IRCKN(JCX))
            RRBX(M) = RRBX(M)*(CMX/(RC_K(JCX,N1,IRCX)+CMX))
  200     CONTINUE
!
!---      Concentration of biomass in mol/m^3 aqu  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
!
!---      Rate of reactant degredation in mol/m^3 aqu s  ---
!
          JCX = JCX+1
          N1 = MAX(1,N*IRCKN(JCX))
          RRBX(M) = -RRBX(M)*RC_K(JCX,N1,IRCX)*CMX
!
!---    Biomass kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.24 ) THEN
          JCX = 0
          RRBX(M) = 0.D+0
!
!---      Loop over the number of reactants, less one  ---
!
          DO 210 NSP = 1,NSPRX-1
!
!---        Concentration of reactant in mol/m^3 water  ---
!
            NSPX = IRC_K(NSP+3,IRCX)
            CMX = SP_CX(NSPX)*VTOLX
!
!---        Partial rate of reactant degredation  ---
!
            JCX = JCX+1
            N1 = MAX(1,N*IRCKN(JCX))
            RRBXX = (CMX/(RC_K(JCX,N1,IRCX)+CMX))
!
!---        Concentration of biomass in mol/m^3 aqu  ---
!
            NSPX = IRC_K(3,IRCX)
            CMX = SP_CX(NSPX)*VTOLX
!
!---        Partial rate of biomass degredation  ---
!
            JCX = JCX+1
            N1 = MAX(1,N*IRCKN(JCX))
            RRBXX = RRBXX*RC_K(JCX,N1,IRCX)*CMX
            RRBX(M) = RRBX(M) + RRBXX
  210     CONTINUE
!
!---      Microbial specific yield coefficient  ---
!
          JCX = JCX+1
          N1 = MAX(1,N*IRCKN(JCX))
          RRBX(M) = RRBX(M)*RC_K(JCX,N1,IRCX)
!
!---      Concentration of reactant in mol/kg water  ---
!
          NSPX = IRC_K(NSP+3,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Partial rate of microbial degredation  ---
!
          JCX = JCX+1
          N1 = MAX(1,N*IRCKN(JCX))
          RRBX(M) = RRBX(M) - RC_K(JCX,N1,IRCX)*CMX
!
!---    Dual Monod kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.35 ) THEN
!
!---      Partial rate of donor degredation  ---
!
          N5 = MAX(1,N*IRCKN(5))
          RRBX(M) = RC_K(5,N5,IRCX)
!
!---      Concentration of donor in mol/kg water  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Partial rate of donor degredation  ---
!
          N3 = MAX(1,N*IRCKN(3))
          RRBX(M) = RRBX(M)*(CMX/(RC_K(3,N3,IRCX)+CMX))
!
!---      Concentration of acceptor in mol/kg water  ---
!
          NSPX = IRC_K(4,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Rate of donor degredation  ---
!
          N4 = MAX(1,N*IRCKN(4))
          RRBX(M) = RRBX(M)*(CMX/(RC_K(4,N4,IRCX)+CMX))
!
!---    Single Monod kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.36 ) THEN
!
!---      Partial rate of donor degredation  ---
!
          N4 = MAX(1,N*IRCKN(4))
          RRBX(M) = RC_K(4,N4,IRCX)
!
!---      Concentration of donor in mol/kg water  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Partial rate of donor degredation  ---
!
          N3 = MAX(1,N*IRCKN(3))
          RRBX(M) = RRBX(M)*(CMX/(RC_K(3,N3,IRCX)+CMX))
!
!---      Concentration of acceptor in mol/kg water  ---
!
          NSPX = IRC_K(4,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Rate of donor degredation  ---
!
          RRBX(M) = RRBX(M)*CMX
!
!---    Liu's multi-rate kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.41 ) THEN
          N1 = MAX(1,N*IRCKN(NSPKX+1))
          N2 = MAX(1,N*IRCKN(NSPKX+2))
          N3 = MAX(1,N*IRCKN(NSPKX+3))
          N4 = MAX(1,N*IRCKN(NSPKX+4))
          N5 = MAX(1,N*IRCKN(NSPKX+5))
          RMX = RC_K(NSPKX+1,N1,IRCX)
          SDENX = RC_K(NSPKX+2,N2,IRCX)*VTOLX/VTOSX
          PFRCX = RC_K(NSPKX+3,N3,IRCX)
          XLGK1 = RC_K(NSPKX+4,N4,IRCX)
          XLGK2 = RC_K(NSPKX+5,N5,IRCX)
          FRRX = 1.D+0
          BRRX = 1.D+0
!
!---      Loop over reactants  ---
!
          DO 220 L = 1,NSPRX
!
!---        Global species index  ---
!
            NSPX = IRC_K(L+2,IRCX)
            CMX = SP_CX(NSPX)*VTOMX
            IF( NSPX.LE.NSPL ) THEN
              ACX = ACTVX(1,NSPX)
            ELSE
              ACX = 1.D+0
            ENDIF
            N1 = MAX(1,N*IRCKN(L))
            FRRX = FRRX*((CMX*ACX)**RC_K(L,N1,IRCX))
  220     CONTINUE
!
!---      Loop over products  ---
!
          DO 230 L = 1,NSPPX-1
!
!---        Global species index  ---
!
            NSPX = IRC_K(L+2+NSPRX,IRCX)
            CMX = SP_CX(NSPX)*VTOMX
            IF( NSPX.LE.NSPL ) THEN
              ACX = ACTVX(1,NSPX)
            ELSE
              ACX = 1.D+0
            ENDIF
            N1 = MAX(1,N*IRCKN(L+NSPRX))
            BRRX = BRRX*((CMX*ACX)**RC_K(L+NSPRX,N1,IRCX))
  230     CONTINUE
          L = NSPPX
          NSPX = IRC_K(L+2+NSPRX,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
          FRRX = FRRX*(1.D+1**XLGK1)
          BRRX = BRRX*(1.D+1**XLGK2)
          RRBX(M) = RMX*(SDENX*PFRCX*FRRX/(1.D+0+FRRX+BRRX)-CMX)
!
!---    Liu's dual domain kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.42 ) THEN
          N1 = MAX(1,N*IRCKN(NSPKX+1))
          RMX = RC_K(NSPKX+1,N1,IRCX)
!
!---      Loop over reactants  ---
!
          N3 = MAX(1,N*IRCKN(NSPKX+3))
          PFRCX = RC_K(NSPKX+3,N3,IRCX)
          FRRX = 0.D+0
!
!---      Global species index  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
          FRRX = FRRX + (CMX**VTOLX)
          NSPX = IRC_K(4,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
          FRRX = FRRX - (CMX**VTOLX)
          RRBX(M) = RMX*FRRX
        ENDIF
!
!---    Residual contribution  ---
!
!        BJM(NEQ) = BJM(NEQ) - (RRBX(M)*EQ_K(NS+M,NEQX))
        RSBX = RSBX + RRBX(M)*EQ_K(NS+M,NEQX)
  290 CONTINUE
!
!---  Loop over kinetic species in kinetic equation  ---
!
      CX = 0.D+0
      CTOX = 0.D+0
      DO 310 M = 1,NS
        NSPX = IEQ_K(M+1,NEQX)
        CX = CX + SP_CX(NSPX)*EQ_K(M,NEQX)
        IF( ISP_MN(NSPX).EQ.1 ) THEN
          NSP_M =  NSPX - NSPL
          CTOX = CTOX + (SP_CO(N,NSPX)+SP_CMN(N,NSP_M))*EQ_K(M,NEQX)
        ELSE
          CTOX = CTOX + SP_CO(N,NSPX)*EQ_K(M,NEQX)
        ENDIF
        DO 300 NSPKX = 1,NSPLK
          IF(ISPLK(14+NSPKX).LT.-1000) THEN
            NSPXX=ABS(ISPLK(14+NSPKX))-1000
            IF(NSPX.EQ.NSPXX) THEN
             IF( NSPX.LE.NSPL ) THEN
               ACX = ACTVX(1,NSPX)
             ELSE
               ACX = 1.D0
             ENDIF
             CTOX = CTOX-SP_CO(N,NSPX)*EQ_K(M,NEQX)
     &            + SP_CO(N,NSPX)/ACX*EQ_K(M,NEQX)
            ENDIF
          ENDIF
  300 CONTINUE
  310 CONTINUE
      CMX = CX*VTOLX
      CMOX = COX(NEQ-NEQE)*VTOLX
      CMTOX = CTOX*VTOLX
!
!---  Check for complete consumption  ---
!
      IF( RSBX.LT.0.D+0 ) THEN
        RSBX = MAX( RSBX,(-CMTOX*DTI) )
      ENDIF
      BJM(NEQ) = (CMX-CMOX)*DTI - RSBX
!
!---  Return residual vector  ---
!
      IF( INDX.EQ.1 ) GOTO 1000
!
!---  Incremented residuals  ---
!
      DO 900 NSP = 1,NSPR
!
!---  Check whether specie is a kinetic equation specie,
!     which affects the residual via the kinetic equation
!     or a kinetic reaction specie,
!     which affects the residual via the kinetic equation,
!     via the kinetic equation reactions  ---
!
        FCHK = .FALSE.
!
!---    Fixed species concentration  ---
!
        DO 400 NSPKX = 1,NSPLK
          IF( ISPLK(14+NSPKX).LT.0 ) THEN
            NSPXX = ABS(ISPLK(14+NSPKX))
            IF( NSP.EQ.NSPXX ) THEN
              FCHK = .FALSE.
              GOTO 450
            ENDIF
          ENDIF
  400   CONTINUE
!
!---    Loop over species in kinetic equation  ---
!
        DO 410 M = 1,NS
          NSPX = IEQ_K(M+1,NEQX)
!
!---      Specie is a kinetic reaction specie  ---
!
          IF( NSP.EQ.NSPX ) THEN
            FCHK = .TRUE.
            GOTO 450
          ENDIF
  410   CONTINUE
!
!---    Loop over kinetic reaction reactants  ---
!
        DO 420 L = 1,NSPRX
!
!---      Global species index  ---
!
          NSPX = IRC_K(L+2,IRCX)
!
!---      Specie is a kinetic reaction specie  ---
!
          IF( NSP.EQ.NSPX ) THEN
            FCHK = .TRUE.
            GOTO 450
          ENDIF
  420   CONTINUE
!
!---    Skip for initial pH  ---
!
        IF( ISPLK(1).GT.1000 ) THEN
          IF( NSP.EQ.MOD(ISPLK(1),1000) .AND.
     &      (NSTEP-NRST).EQ.0 ) FCHK = .FALSE.
        ENDIF
!
!---    Loop over kinetic reaction products  ---
!
        DO 430 L = 1,NSPPX
!
!---      Global species index  ---
!
          NSPX = IRC_K(L+2+NSPRX,IRCX)
!
!---      Specie is a kinetic reaction specie  ---
!
          IF( NSP.EQ.NSPX ) THEN
            FCHK = .TRUE.
            GOTO 450
          ENDIF
  430   CONTINUE
  450   CONTINUE
!
!---    Specie is a kinetic equation specie,
!       or a kinetic equation reaction specie  ---
!
        IF( FCHK ) THEN
          RSX = 0.D+0
!
!---      Loop over kinetic reactions  ---
!
          DO 600 M = 1,NR
!
!---        Reaction index, number of reactants, number of products  ---
!
            IRCX = IEQ_K(NS+2+M,NEQX)
            NSPRX = IRC_K(1,IRCX)
            NSPPX = IRC_K(2,IRCX)
            NSPKX = NSPRX+NSPPX
!
!---        Dissolution-precipitation kinetic reaction  ---
!
            IF( (IRCKT(IRCX).GE.10 .AND. IRCKT(IRCX).LE.12) .OR.
     &        (IRCKT(IRCX).EQ.14) .OR.
     &        (IRCKT(IRCX).GE.5 .AND. IRCKT(IRCX).LE.9) ) THEN
!
!---          Ion activity product mol/kg water, loop over species in
!             kinetic reaction  ---
!
              QX = 1.D+0
!
!---          Glass equilibrium dependent on aqueous silica only
!
              IF (IRCKT(IRCX).EQ.14) THEN
!
!---            Incremented species  ---
!
                IF( NSPSI.EQ.NSP ) THEN
                  SP_CXX = SP_CX(NSPSI)+DSP_CX(NSPSI)
!
!---            Unincremented species  ---
!
                ELSE
                  SP_CXX = SP_CX(NSPSI)
                ENDIF
                  CMX = SP_CXX*VTOMX
                  ACX = ACTVX(1,NSPSI)
                  QX = QX*(CMX*ACX)
              ELSE
!
!---          Loop over species in kinetic reaction  ---
!
              DO 510 L = 1,NSPKX
                NSPX = IRC_K(L+2,IRCX)
!
!---            Incremented species  ---
!
                IF( NSPX.EQ.NSP ) THEN
                  SP_CXX = SP_CX(NSPX)+DSP_CX(NSPX)
!
!---            Unincremented species  ---
!
                ELSE
                  SP_CXX = SP_CX(NSPX)
                ENDIF
!
!---            Aqueous species,
!               concentration in molality, mol/kg H2O  ---
!
                IF( NSPX.LE.NSPL ) THEN
                  CMX = SP_CXX*VTOMX
                  ACX = ACTVX(1,NSPX)
!
!---              Reactants  ---
!
                  N1 = MAX(1,N*IRCKN(L))
                  IF( L.LE.NSPRX ) THEN
                    QX = QX*((CMX*ACX)**RC_K(L,N1,IRCX))
!
!---              Products  ---
!
                  ELSE
                    QX = QX/((CMX*ACX)**RC_K(L,N1,IRCX))
                  ENDIF
!
!---              CFMX is scaling factor to translate between pore-scale
!---              and macro-scale simulations.  Default = 1
!
                  IF (ISLC(58).EQ.1) THEN
                    QX = CFMX(N)*QX
                  ENDIF
!
!---            Solid species, skip  ---
!
                ELSEIF( NSPX.LE.NSPL+NSPS ) THEN
                  GOTO 510
                ENDIF
  510         CONTINUE
              ENDIF
!
!---          Reactive surface area
!             NSP_M - mineral species number  ---
!
              NSPX = IRC_K(3+NSPKX,IRCX)
              NSP_M = NSPX - NSPL
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                SP_CXX = SP_CX(NSPX)+DSP_CX(NSPX)
!
!---          Unincremented species  ---
!
              ELSE
                SP_CXX = SP_CX(NSPX)
              ENDIF
!
!---          Primary mineral  ---
!
              IF( RS_S(2,NSP_M,N).GT.EPSL ) THEN
                AOX = RS_S(1,NSP_M,N)*VOL(N)*
     &            RS_S(2,NSP_M,N)*SP_S(1,NSP_M)
                VFMOX = RS_S(2,NSP_M,N)
                IF( ISP_MN(NSPX).EQ.1 ) THEN
                  VFMX = 1.D-3*(SP_CMN(N,NSP_M)+SP_CXX)
     &              *SP_S(2,NSP_M)/SP_S(1,NSP_M)
                ELSE
                  VFMX = 1.D-3*SP_CXX*SP_S(2,NSP_M)/SP_S(1,NSP_M)
                ENDIF
                IF( (IRCKT(IRCX).NE.7) .AND. (IRCKT(IRCX).NE.9) .AND.
     &              (IRCKT(IRCX).NE.12) .AND. (IRCKT(IRCX).NE.14) )
     &          VFMX = MAX( VFMX,1.D-5 )
!
!---          Secondary mineral, initial reactive surface area
!             for seconary minerals is set to 0.25 m^2/dm^3  ---
!
              ELSE
                IF( RS_S(1,NSP_M,N).GT.EPSL ) THEN
                  VFMOX = 1.D-5
                  AOX = RS_S(1,NSP_M,N)*VOL(N)*VFMOX*SP_S(1,NSP_M)
                ELSE
                  AOX = 0.25D+3*VOL(N)
                  VFMOX = 1.D-2
                ENDIF
                VFMX = 1.D-3*SP_CXX*SP_S(2,NSP_M)/SP_S(1,NSP_M)
                IF( (IRCKT(IRCX).NE.7) .AND. (IRCKT(IRCX).NE.9) .AND.
     &              (IRCKT(IRCX).NE.12) .AND. (IRCKT(IRCX).NE.14) )
     &          VFMX = MAX( VFMX,1.D-5 )
              ENDIF
              VFMX = MAX( VFMX,0.D+0 )
!
!---          Reactive surface area  ---
!
              AX = AOX*(((POR0(2,N)*VFMX)/
     &          (POR(2,IZ(N))*VFMOX))**(2.D+0/3.D+0))
              IF( ISLC(56).EQ.1 ) AX = AX * SL(2,N)
              IF( ISLC(56).EQ.2 ) AX = AOX
              IF( ISLC(58).EQ.1 ) AX = 1.0D+0
!
!---          Reaction rate, mol/s  ---
!
              RRX = -AX*RRCX(M)*(1.D+0-(QX/EQKX(M)))
!
!---          pH dependence  ---
!
              IF( ((IRCKT(IRCX).GE.5 .AND. IRCKT(IRCX).LE.9)
     &          .OR. (IRCKT(IRCX).EQ.14))
     &          .AND. ISPLK(1).NE.0 ) THEN
                NSP_PHX = MOD(ISPLK(1),1000)
!
!---            Incremented species  ---
!
                IF( NSP_PHX.EQ.NSP ) THEN
                  SP_CXX = SP_CX(NSP_PHX)+DSP_CX(NSP_PHX)
!
!---            Unincremented species  ---
!
                ELSE
                  SP_CXX = SP_CX(NSP_PHX)
                ENDIF
                PHX = -LOG10(1.D-3*SP_CXX*VTOLX)
                IF( IRCKT(IRCX).GE.8 .AND. IRCKT(IRCX).LE.9 ) THEN
                  RRX = RRX*MAX( 0.D+0,
     &              (7.9201D-1 - 1.3479D-1*PHX + 5.2D-3*(PHX**2)))
                ELSE
                  N9 = MAX(1,N*IRCKN(NSPKX+9))
                  RRX = RRX*(1.D+1**(-RC_K(NSPKX+9,N9,IRCX)*PHX))
                ENDIF
              ENDIF
!
!---          Reaction rate, mol/m^3 aqu s  ---
!
              RRX = RRX*VTOLX/VOL(N)
!
!---          Direction limited  ---
!
              IF( IRCKT(IRCX).EQ.6 .OR. IRCKT(IRCX).EQ.8
     &          .OR. IRCKT(IRCX).EQ.11 ) THEN
                RRX = MAX( RRX,0.D+0 )
              ELSEIF( IRCKT(IRCX).EQ.7 .OR. IRCKT(IRCX).EQ.9
     &          .OR. IRCKT(IRCX).EQ.12 .OR. IRCKT(IRCX).EQ.14 ) THEN
                RRX = MIN( RRX,0.D+0 )
              ENDIF
!
!---        Forward-backward kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.1 ) THEN
              N1 = MAX(1,N*IRCKN(NSPKX+1))
              N2 = MAX(1,N*IRCKN(NSPKX+2))
              FRRX = RC_K(NSPKX+1,N1,IRCX)
              BRRX = RC_K(NSPKX+2,N2,IRCX)
!
!---          Loop over reactants  ---
!
              DO 520 L = 1,NSPRX
!
!---            Global species index  ---
!
                NSPX = IRC_K(L+2,IRCX)
!
!---            Incremented species  ---
!
                IF( NSPX.EQ.NSP ) THEN
                  CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---            Unincremented species  ---
!
                ELSE
                  CMX = SP_CX(NSPX)*VTOLX
                ENDIF
                N1 = MAX(1,N*IRCKN(L))
                FRRX = FRRX*(CMX**RC_K(L,N1,IRCX))
!
  520         CONTINUE
!
!---          Loop over products  ---
!
              DO 530 L = 1,NSPPX
!
!---            Global species index  ---
!
                NSPX = IRC_K(L+2+NSPRX,IRCX)
!
!---            Incremented species  ---
!
                IF( NSPX.EQ.NSP ) THEN
                  CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---            Unincremented species  ---
!
                ELSE
                  CMX = SP_CX(NSPX)*VTOLX
                ENDIF
                N1 = MAX(1,N*IRCKN(L+NSPRX))
                BRRX = BRRX*(CMX**RC_K(L+NSPRX,N1,IRCX))
  530         CONTINUE
              RRX = FRRX - BRRX
!
!---        Valocchi-Monod kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.2 ) THEN
!
!---          Concentration of biomass in mol/m^3 aqu  ---
!
              NSPX = IRC_K(5,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOLX
              ENDIF
!
!---          Partial rate of donor degredation  ---
!
              N6 = MAX(1,N*IRCKN(6))
              RRX = RC_K(6,N6,IRCX)*CMX
!
!---          Concentration of donor in mol/m^3 aqu  ---
!
              NSPX = IRC_K(3,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOMX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOMX
              ENDIF
!
!---          Partial rate of donor degredation  ---
!
              N4 = MAX(1,N*IRCKN(4))
              RRX = RRX*(CMX/(RC_K(4,N4,IRCX)+CMX))
!
!---          Concentration of acceptor in mol/m^3 aqu  ---
!
              NSPX = IRC_K(4,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOMX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOMX
              ENDIF
!
!---          Rate of donor degredation, mol/m^3 aqu s  ---
!
              N5 = MAX(1,N*IRCKN(5))
              RRX = RRX*(CMX/(RC_K(5,N5,IRCX)+CMX))
!
!---        Valocchi-Sorption kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.3 ) THEN
!
!---          Concentration of sorbed species in mol/gm soil  ---
!
              NSPX = IRC_K(4,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
!               CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOSX*1.D-3
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOSX
!
!---          Unincremented species  ---
!
              ELSE
!               CMX = SP_CX(NSPX)*VTOSX*1.D-3
                CMX = SP_CX(NSPX)*VTOSX
              ENDIF
              N4 = MAX(1,N*IRCKN(4))
              RRX = CMX/RC_K(4,N4,IRCX)
!
!---          Concentration of aqueous species in mol/m^3 aqu  ---
!
              NSPX = IRC_K(3,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
!               CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOMX
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---          Unincremented species  ---
!
              ELSE
!               CMX = SP_CX(NSPX)*VTOMX
                CMX = SP_CX(NSPX)*VTOLX
              ENDIF
!
!---          Rate of sorption in mol/m^3 aqu s  ---
!
!             RRX = RC_K(3,IRCX)*(CMX-RRX)*1.D+3
              N3 = MAX(1,N*IRCKN(3))
              RRX = RC_K(3,N3,IRCX)*(CMX-RRX)
!
!---        Langmuir-Sorption kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.13 ) THEN
!
!---          Concentration of sorbed species in mol/gm soil  ---
!
              NSPX = IRC_K(4,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CSX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOSX
!
!---          Unincremented species  ---
!
              ELSE
                CSX = SP_CX(NSPX)*VTOSX
              ENDIF
!
!---          Concentration of aqueous species in mol/m^3 aqu  ---
!
              NSPX = IRC_K(3,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOLX
              ENDIF
!
!---          Rate of sorption in mol/m^3 aqu s  ---
!
              N3 = MAX(1,N*IRCKN(3))
              N4 = MAX(1,N*IRCKN(4))
              N5 = MAX(1,N*IRCKN(5))
              RRX = RC_K(3,N3,IRCX)*CMX*(RC_K(5,N5,IRCX)-CSX)
     &          -RC_K(4,N4,IRCX)*CSX
!
!---        Valocchi-Biomass kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.4 ) THEN
!
!---          Concentration of biomass in mol/m^3 aqu  ---
!
              NSPX = IRC_K(5,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOLX
              ENDIF
!
!---          Partial rate of donor degredation  ---
!
              N6 = MAX(1,N*IRCKN(6))
              RRX = RC_K(6,N6,IRCX)*CMX
!
!---          Concentration of donor in mol/m^3 aqu  ---
!
              NSPX = IRC_K(3,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOMX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOMX
              ENDIF
!
!---          Partial rate of donor degredation  ---
!
              N4 = MAX(1,N*IRCKN(4))
              RRX = RRX*(CMX/(RC_K(4,N4,IRCX)+CMX))
!
!---          Concentration of acceptor in mol/m^3 aqu  ---
!
              NSPX = IRC_K(4,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOMX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOMX
              ENDIF
!
!---          Rate of donor degredation, mol/m^3 aqu s  ---
!
              N5 = MAX(1,N*IRCKN(5))
              RRX = RRX*(CMX/(RC_K(5,N5,IRCX)+CMX))
!
!---          Concentration of biomass in mol/m^3 aqu  ---
!
              NSPX = IRC_K(5,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOLX
              ENDIF
!
!---          Rate of biomass production in mol/m^3 aqu s  ---
!
              N7 = MAX(1,N*IRCKN(7))
              N8 = MAX(1,N*IRCKN(8))
              RRX = RC_K(7,N7,IRCX)*RRX - RC_K(8,N8,IRCX)*CMX
!
!---        Emulsion- or oil-sorption kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.15 ) THEN
!
!---          Concentration of immobile oil (kg oil/kg soil)  ---
!
              NSPX = IRC_K(3,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CIMX = 1.D-3*(SP_CX(NSPX)+DSP_CX(NSPX))*VTOSX*WTMX
!
!---          Unincremented species  ---
!
              ELSE
                CIMX = 1.D-3*SP_CX(NSPX)*VTOSX*WTMX
              ENDIF
!
!---          Concentration of mobile oil (kg oil/m^3 aqu)  ---
!
              NSPX = IRC_K(4,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = 1.D-3*(SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX*WTMX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = 1.D-3*SP_CX(NSPX)*VTOLX*WTMX
              ENDIF
!
!---          Rate of immobile-oil production in kg oil/kg soil s  ---
!
              N3 = MAX(1,N*IRCKN(3))
              N4 = MAX(1,N*IRCKN(4))
              N5 = MAX(1,N*IRCKN(5))
              RRX = 3.D+0*ULAVX*ETAX*MAX(1.D+0-PORD(2,N),0.D+0)*
     &          RC_K(4,N4,IRCX)*MAX(RC_K(5,N5,IRCX)-CIMX,0.D+0)*CMX/
     &          (2.D+0*RC_K(3,N3,IRCX)*RC_K(5,N5,IRCX))
!
!---          Rate of immobile-oil production in mol/m^3 aqu s  ---
!
              RRX = 1.D+3*RRX*VTOLX/(VTOSX*WTMX)
!
!---        Monod kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.22 ) THEN
              JCX = 0
              RRX = 1.D+0
!
!---          Loop over the number of reactants, less one  ---
!
              DO 540 L = 1,NSPRX-1
!
!---            Concentration of reactant in mol/m^3 water  ---
!
                NSPX = IRC_K(L+3,IRCX)
!
!---            Incremented species  ---
!
                IF( NSPX.EQ.NSP ) THEN
                  CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---            Unincremented species  ---
!
                ELSE
                  CMX = SP_CX(NSPX)*VTOLX
                ENDIF
!
!---            Partial rate of reactant degredation  ---
!
                JCX = JCX+1
!               RRX = RRX*(CMX/(RC_K(JCX,IRCX)+CMX))
                N1 = MAX(1,N*IRCKN(JCX))
                RRX = RRX*(CMX**RC_K(JCX,N1,IRCX))*VTOLX/VTOSX*1.D-3
  540         CONTINUE
!
!---          Concentration of biomass in mol/m^3 aqu  ---
!
              NSPX = IRC_K(3,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOLX
              ENDIF
!
!---          Partial rate of biomass degredation  ---
!
              JCX = JCX+1
              N1 = MAX(1,N*IRCKN(JCX))
              RRX = -RRX*RC_K(JCX,N1,IRCX)*CMX
!
!---        Biomass kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.24 ) THEN
              JCX = 0
              RRX = 0.D+0
!
!---          Loop over the number of reactants, less one  ---
!
              DO 560 L = 1,NSPRX-1
!
!---            Concentration of reactant in mol/m^3 water  ---
!
                NSPX = IRC_K(L+3,IRCX)
!
!---            Incremented species  ---
!
                IF( NSPX.EQ.NSP ) THEN
                  CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---            Unincremented species  ---
!
                ELSE
                  CMX = SP_CX(NSPX)*VTOLX
                ENDIF
!
!---            Partial rate of reactant degredation  ---
!
                JCX = JCX+1
                N1 = MAX(1,N*IRCKN(JCX))
                RRBXX = (CMX/(RC_K(JCX,N1,IRCX)+CMX))
!
!---            Concentration of biomass in mol/m^3 aqu  ---
!
                NSPX = IRC_K(3,IRCX)
                CMX = SP_CX(NSPX)*VTOLX
!
!---            Partial rate of biomass degredation  ---
!
                JCX = JCX+1
                N1 = MAX(1,N*IRCKN(JCX))
                RRBXX = RRBXX*RC_K(JCX,N1,IRCX)*CMX
                RRX = RRX + RRBXX
  560         CONTINUE
!
!---          Microbial specific yield coefficient  ---
!
              JCX = JCX+1
              N1 = MAX(1,N*IRCKN(JCX))
              RRX = RRX*RC_K(JCX,N1,IRCX)
!
!---          Concentration of biomass in mol/m^3 aqu  ---
!
              NSPX = IRC_K(3,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOLX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOLX
              ENDIF
!
!---          Partial rate of microbial degredation  ---
!
              JCX = JCX+1
              N1 = MAX(1,N*IRCKN(JCX))
              RRX = RRX - RC_K(JCX,N1,IRCX)*CMX
!
!---        Dual-Monod kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.35 ) THEN
!
!---          Partial rate of donor degredation  ---
!
              N5 = MAX(1,N*IRCKN(5))
              RRX = RC_K(5,N5,IRCX)
!
!---          Concentration of donor in mol/m^3 aqu  ---
!
              NSPX = IRC_K(3,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOMX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOMX
              ENDIF
!
!---          Partial rate of donor degredation  ---
!
              N3 = MAX(1,N*IRCKN(3))
              RRX = RRX*(CMX/(RC_K(3,N3,IRCX)+CMX))
!
!---          Concentration of acceptor in mol/m^3 aqu  ---
!
              NSPX = IRC_K(4,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOMX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOMX
              ENDIF
!
!---          Rate of donor degredation, mol/m^3 aqu s  ---
!
              N4 = MAX(1,N*IRCKN(4))
              RRX = RRX*(CMX/(RC_K(4,N4,IRCX)+CMX))
!
!---        Single-Monod kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.36 ) THEN
!
!---          Partial rate of donor degredation  ---
!
              N4 = MAX(1,N*IRCKN(4))
              RRX = RC_K(4,N4,IRCX)
!
!---          Concentration of donor in mol/m^3 aqu  ---
!
              NSPX = IRC_K(3,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOMX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOMX
              ENDIF
!
!---          Partial rate of donor degredation  ---
!
              N3 = MAX(1,N*IRCKN(3))
              RRX = RRX*(CMX/(RC_K(3,N3,IRCX)+CMX))
!
!---          Concentration of acceptor in mol/m^3 aqu  ---
!
              NSPX = IRC_K(4,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOMX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOMX
              ENDIF
!
!---          Rate of donor degredation, mol/m^3 aqu s  ---
!
              RRX = RRX*CMX
!
!---        Liu's multi-rate kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.41 ) THEN
              N1 = MAX(1,N*IRCKN(NSPKX+1))
              N2 = MAX(1,N*IRCKN(NSPKX+2))
              N3 = MAX(1,N*IRCKN(NSPKX+3))
              N4 = MAX(1,N*IRCKN(NSPKX+4))
              N5 = MAX(1,N*IRCKN(NSPKX+5))
              RMX = RC_K(NSPKX+1,N1,IRCX)
              SDENX = RC_K(NSPKX+2,N2,IRCX)*VTOLX/VTOSX
              PFRCX = RC_K(NSPKX+3,N3,IRCX)
              XLGK1 = RC_K(NSPKX+4,N4,IRCX)
              XLGK2 = RC_K(NSPKX+5,N5,IRCX)
              FRRX = 1.D+0
              BRRX = 1.D+0
!
!---          Loop over reactants  ---
!
              DO 570 L = 1,NSPRX
!
!---            Global species index  ---
!
                NSPX = IRC_K(L+2,IRCX)
                IF( NSPX.EQ.NSP ) THEN
                  CMX = (SP_CX(NSPX)+DSP_CX(NSPX))*VTOMX
                  IF(NSPX.LE.NSPL) THEN
                    ACX = ACTVX(NSP+1,NSPX)
                  ELSE
                    ACX = 1.D+0
                  ENDIF
!
!---            Unincremented species  ---
!
                ELSE
                  CMX = SP_CX(NSPX)*VTOMX
                  IF( NSPX.LE.NSPL ) THEN
                    ACX = ACTVX(1,NSPX)
                  ELSE
                    ACX = 1.D+0
                  ENDIF
                ENDIF
                N1 = MAX(1,N*IRCKN(L))
                FRRX = FRRX*((CMX*ACX)**RC_K(L,N1,IRCX))
  570         CONTINUE
!
!---          Loop over products  ---
!
              DO 580 L = 1,NSPPX-1
!
!---            Global species index  ---
!
                NSPX = IRC_K(L+2+NSPRX,IRCX)
!
!---            Incremented species  ---
!
                IF( NSPX.EQ.NSP ) THEN
                  CMX = (SP_CX(NSPX) + DSP_CX(NSPX))*VTOMX
                  IF( NSPX.LE.NSPL ) THEN
                    ACX = ACTVX(NSP+1,NSPX)
                  ELSE
                    ACX = 1.D+0
                  ENDIF
!
!---            Unincremented species  ---
!
                ELSE
                  CMX = SP_CX(NSPX)*VTOMX
                  IF( NSPX.LE.NSPL ) THEN
                    ACX = ACTVX(1,NSPX)
                  ELSE
                    ACX = 1.D+0
                  ENDIF
               ENDIF
                N1 = MAX(1,N*IRCKN(L+NSPRX))
                BRRX = BRRX*((CMX*ACX)**RC_K(L+NSPRX,N1,IRCX))
  580         CONTINUE
              L = NSPPX
              NSPX = IRC_K(L+2+NSPRX,IRCX)
!
!---          Incremented species  ---
!
              IF( NSPX.EQ.NSP ) THEN
                CMX = (SP_CX(NSPX) + DSP_CX(NSPX))*VTOLX
!
!---          Unincremented species  ---
!
              ELSE
                CMX = SP_CX(NSPX)*VTOLX
              ENDIF
              FRRX = FRRX*(1.D+1**XLGK1)
              BRRX = BRRX*(1.D+1**XLGK2)
              RRX = RMX*(SDENX*PFRCX*FRRX/(1.D+0+FRRX+BRRX)-CMX)
!
!---        Liu's dual domain kinetic reaction  ---
!
            ELSEIF( IRCKT(IRCX).EQ.42 ) THEN
              N1 = MAX(1,N*IRCKN(NSPKX+1))
              N3 = MAX(1,N*IRCKN(NSPKX+3))
              RMX = RC_K(NSPKX+1,N1,IRCX)
              PFRCX = RC_K(NSPKX+3,N3,IRCX)
              FRRX = 0.D+0
!
!---        Global species index  ---
!
              NSPX = IRC_K(3,IRCX)
              IF( NSPX.EQ.NSP ) THEN
                SP_CXX = SP_CX(NSPX) + DSP_CX(NSPX)
              ELSE
                SP_CXX = SP_CX(NSPX)
              ENDIF               
              CMX = SP_CXX*VTOLX
              FRRX = FRRX + (CMX**VTOLX)
              NSPX = IRC_K(4,IRCX)
              IF( NSPX.EQ.NSP ) THEN
                SP_CXX = SP_CX(NSPX) + DSP_CX(NSPX)
              ELSE
                SP_CXX = SP_CX(NSPX)
              ENDIF               
              CMX = SP_CXX*VTOLX
              FRRX = FRRX - (CMX**VTOLX)
              RRX = RMX*FRRX
            ENDIF
!
!---        Liu's dual domain kinetic reaction  ---
!
            IF( IRCKT(IRCX).EQ.42 ) THEN
              EQ_KX = EQ_K(NS+M,NEQX)
              IF( IMMB(NEQC+NEQX).EQ.1 ) THEN
                N2 = MAX(1,N*IRCKN(NSPKX+2))
                EQ_KX = EQ_K(NS+M,NEQX)*RC_K(NSPKX+2,N2,IRCX)
              ENDIF
!
!---        Residual contribution  ---
!
            ELSE
              RSX = RSX + RRX*EQ_K(NS+M,NEQX)
            ENDIF
!            RSX = RSX - ((RRX-RRBX(M))*EQ_K(NS+M,NEQX))
  600     CONTINUE
!
!---      Loop over kinetic species in kinetic equation  ---
!
          DO 700 M = 1,NS
            NSPX = IEQ_K(M+1,NEQX)
!
!---        Incremented species  ---
!
            IF( NSPX.EQ.NSP ) THEN
              AJM(NEQ,IEQ_S(NSP)) = EQ_K(M,NEQX)*VTOLX*DTI
!
!---          Fixed species activity  ---
!
              DO 610 NSPKX = 1,NSPLK
                IF( ISPLK(14+NSPKX).LT.-1000 ) THEN
                  NSPXX = ABS(ISPLK(14+NSPKX))-1000
                  IF( NSP.EQ.NSPXX ) THEN
                    IF( NSP.LE.NSPL ) THEN
                      ACX = ACTVX(1,NSP)
                    ELSE
                      ACX = 1.D+0
                    ENDIF
                    AJM(NEQ,IEQ_S(NSP))=EQ_K(M,NEQX)/ACX*VTOLX*DTI
                  ENDIF
                ENDIF
  610         CONTINUE
              GOTO 710
            ENDIF
  700     CONTINUE
  710     CONTINUE
!
!---      Check for complete consumption  ---
!
          IF( RSX.LT.0.D+0 ) THEN
            RSX = MAX( RSX,(-CMTOX*DTI) )
          ENDIF
!          AJM(NEQ,IEQ_S(NSP)) = AJM(NEQ,IEQ_S(NSP)) + RSX/DSP_CX(NSP)
          AJM(NEQ,IEQ_S(NSP)) = AJM(NEQ,IEQ_S(NSP)) -
     &      (RSX-RSBX)/DSP_CX(NSP)
        ENDIF
  900 CONTINUE
!
!---  Return residual vector and Jacobian matrix  ---
!
      IF( ABS(INDX).EQ.1 ) GOTO 1000
      BJM(NEQ) = -BJM(NEQ)
 1000 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of KECHEM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE KECHEM_R( ACTVX,AJM,BJM,COX,SP_CX,DSP_CX,N,NEQ,INDX )
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
!     Kinetic Equation CHEMistry
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 January 2005.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE PORMED
      USE GRID
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 ACTVX(LSPL+1,LSPL)
      REAL*8 BJM(LSPR),AJM(LSPR,LSPR)
      REAL*8 SP_CX(LSPR),DSP_CX(LSPR)
      REAL*8 COX(LEQC+LEQK)
      REAL*8 EQKX(LREK),RRBX(LREK),RRCX(LREK),RSBX(LREK)
      LOGICAL FCHK
      SAVE NR
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/KECHEM_R'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
      NROW = NEQ-NEQE
!
!---  Skip for initial pH  ---
!
      IF( ISPLK(1).GT.1000 ) THEN
        IF( NEQ.EQ.IEQ_S(MOD(ISPLK(1),1000)) .AND.
     &    (NSTEP-NRST).EQ.0 ) THEN
          AJM(NROW,NROW) = 1.D+0
          BJM(NROW) = 0.D+0
          GOTO 1000
        ENDIF
      ENDIF
!
!--- fixed species concentration or activity
!
      DO NSLKX = 1,NSPLK
        IF( ISPLK(14+NSLKX).LT.0 ) THEN
          NSPX = ABS(ISPLK(14+NSLKX))
          IF( NSPX.GT.1000 ) NSPX = NSPX - 1000
          IF( NEQ.EQ.IEQ_S(NSPX) ) THEN
            AJM(NROW,NROW) = 1.D+0
            BJM(NROW) = 0.D+0
            GOTO 1000
          ENDIF
        ENDIF
      ENDDO
!
!---  Volumetric concentration to molality, mol/m^3 -> mol/kg aqu  ---
!
      VTOMX = 1.D+0/(SL(2,N)*PORD(2,N)*RHOL(2,N)*XLW(2,N))
!
!---  Volumetric concentration to aqueous concentration,
!     mol/m^3 -> mol/m^3 aqu  ---
!
      VTOLX = 1.D+0/(SL(2,N)*PORD(2,N))
!
!---  Volumetric concentration to sorbed concentration,
!     mol/m^3 -> mol/kg sol  ---
!
      VTOSX = 1.D+0/((1.D+0-PORT(2,N))*RHOS(IZ(N)))
!
!---  Total number of species  ---
!
      NEQX = NEQ - NEQE - NEQC
      NSPR = NSPG + NSPL + NSPN + NSPS
!
!---  Number of species and reactions in kinetic equation  ---
!
      NS = IEQ_K(1,NEQX)
      NR = IEQ_K(NS+2,NEQX)
      IF( (NSTEP-NRST).EQ.0 ) THEN
        NR = 0
        NSPRX = 0
        NSPPX = 0
        NSPKX = 0
      ENDIF
!
!---  Loop over kinetic reactions in kinetic equation to
!     determine rate constants  ---
!
      DO 100 M = 1,NR
!
!---    Reaction index, number of reactants, number of products  ---
!
        IRCX = IEQ_K(NS+2+M,NEQX)
        NSPRX = IRC_K(1,IRCX)
        NSPPX = IRC_K(2,IRCX)
        NSPKX = NSPRX+NSPPX
!
!---    Dissolution-precipitation kinetic reaction  ---
!
        IF( (IRCKT(IRCX).GE.10 .AND. IRCKT(IRCX).LE.12) .OR.
     &    (IRCKT(IRCX).GE.5 .AND. IRCKT(IRCX).LE.9) ) THEN
!
!---      Equilibrium constants as a function of temperature  ---
!
          IRCX = -IRCX
          CALL EQCN( EQKX(M),T(2,N),IRCX,N )
          IRCX = -IRCX
!
!---      Reaction rate constants as a function of temperature
!         mol/m^2 s  ---
!
          TKX = T(2,N)+TABS
          N1 = MAX(1,N*IRCKN(NSPKX+1))
          N2 = MAX(1,N*IRCKN(NSPKX+2))
          N3 = MAX(1,N*IRCKN(NSPKX+3))
          TKRX = RC_K(NSPKX+3,N3,IRCX)+TABS
          RRCX(M) = RC_K(NSPKX+1,N1,IRCX)*EXP( -RC_K(NSPKX+2,N2,IRCX)*
     &      ((1.D+0/TKX)-(1.D+0/TKRX))/(1.D-3*RCU) )
        ENDIF
  100 CONTINUE
!
!---  Base residual  ---
!
      BJM(NROW) = 0.D+0
      RSBXX = 0.D0
!
!---  Loop over kinetic reactions  ---
!
      DO 200 M = 1,NR
        RSBX(M) = 0.D+0
!
!---    Reaction index, number of reactants, number of products  ---
!
        IRCX = IEQ_K(NS+2+M,NEQX)
        NSPRX = IRC_K(1,IRCX)
        NSPPX = IRC_K(2,IRCX)
        NSPKX = NSPRX+NSPPX
        CALL RATER( NEQX,M,NS,N,IRCX,NSPKX,NSPRX,NSPPX,SP_CX,ACTVX,
     &      VTOMX,VTOLX,VTOSX,EQKX(M),RRCX(M),RSBX(M) )
        RSBXX = RSBXX + RSBX(M)
  200 CONTINUE
!
!---  Loop over kinetic species in kinetic equation  ---
!
      CX = 0.D+0
      CTOX = 0.D+0
      DO 300 M = 1,NS
        NSPX = IEQ_K(M+1,NEQX)
        CX = CX + SP_CX(NSPX)*EQ_K(M,NEQX)
        IF( ISP_MN(NSPX).EQ.1 ) THEN
          NSP_M =  NSPX - NSPL
          CTOX = CTOX + (SP_CO(N,NSPX)+SP_CMN(N,NSP_M))*EQ_K(M,NEQX)
        ELSE
          CTOX = CTOX + SP_CO(N,NSPX)*EQ_K(M,NEQX)
        ENDIF
        DO NSPKX = 1,NSPLK
          IF(ISPLK(14+NSPKX).LT.-1000) THEN
            NSPXX=ABS(ISPLK(14+NSPKX))-1000
            IF(NSPX.EQ.NSPXX) THEN
             IF( NSPX.LE.NSPL ) THEN
               ACX = ACTVX(1,NSPX)
             ELSE
               ACX = 1.D0
             ENDIF             
             CTOX = CTOX-SP_CO(N,NSPX)*EQ_K(M,NEQX)
     &            + SP_CO(N,NSPX)/ACX*EQ_K(M,NEQX)
            ENDIF
          ENDIF
        ENDDO
  300 CONTINUE
      CMX = CX*VTOLX
      CMOX = COX(NEQ-NEQE)*VTOLX
      CMTOX = CTOX*VTOLX
!
!---  Check for complete consumption  ---
!
!      IF( RSBXX.LT.0.D+0 ) THEN
!        RSBXX = MAX( RSBXX,(-CMTOX*DTI) )
!      ENDIF
      BJM(NROW) = (CMX-CMOX)*DTI - RSBXX
!
!---  Return residual vector  ---
!
      IF( INDX.EQ.1 ) GOTO 1000
!
!---      Loop over kinetic reactions  ---
!
      DO 600 M = 1,NR
!
!---        Reaction index, number of reactants, number of products  ---
!
        IRCX = IEQ_K(NS+2+M,NEQX)
        NSPRX = IRC_K(1,IRCX)
        NSPPX = IRC_K(2,IRCX)
        NSPKX = NSPRX+NSPPX
        DO L = 1,NSPKX
          RSX = 0.D0
          NSPX = IRC_K(L+2,IRCX)
          CMMX = SP_CX(NSPX)
          IF( NSPX.LE.NSPL ) ACX = ACTVX(1,NSPX)
!          DSP_CX(NSPX) = MAX(1.D-10,DSP_CX(NSPX))
          SP_CX(NSPX) = CMMX+DSP_CX(NSPX)
          IF( NSPX.LE.NSPL ) ACTVX(1,NSPX) = ACTVX(NSPX+1,NSPX)
          CALL RATER( NEQX,M,NS,N,IRCX,NSPKX,NSPRX,NSPPX,SP_CX,ACTVX,
     &       VTOMX,VTOLX,VTOSX,EQKX(M),RRCX(M),RSX )
          SP_CX(NSPX) = CMMX
          IF( NSPX.LE.NSPL ) ACTVX(1,NSPX) = ACX
          NEQXX=IEQ_S(NSPX)
          IF( NEQXX.GT.NEQE) THEN
            NCOL = NEQXX-NEQE
            AJM(NROW,NCOL) = AJM(NROW,NCOL) - (RSX-RSBX(M))/DSP_CX(NSPX)
          ELSE
!
!---   Equilibrium mass action
!
            NSE = IEQ_E(1,NEQXX)
!
!---  Loop over equilibrium species  ---
!
            DO MSX = 2,NSE
              NCM_SP = IEQ_E(MSX+1,NEQXX)
              NCOL = IEQ_S(NCM_SP)-NEQE
              AJM(NROW,NCOL) = AJM(NROW,NCOL)-(RSX-RSBX(M))/
     &          DSP_CX(NSPX)*EQ_E(MSX-1,NEQXX)*SP_CX(NSPX)/SP_CX(NCM_SP)
            ENDDO
          ENDIF
        ENDDO
  600 CONTINUE
!
!---      Loop over kinetic species in kinetic equation  ---
!
      DO 700 M = 1,NS
        NSPX = IEQ_K(M+1,NEQX)
!
!---        Incremented species  ---
!
        NEQXX = IEQ_S(NSPX)
        IF( NEQXX.GT.NEQE ) THEN
         NCOL = IEQ_S(NSPX)-NEQE
         AJM(NROW,NCOL) = AJM(NROW,NCOL)+EQ_K(M,NEQX)*VTOLX*DTI
!
!---  fixed species activity
!
         DO NSPKX = 1,NSPLK
          IF( ISPLK(14+NSPKX).LT.-1000 ) THEN
            NSPXX = ABS(ISPLK(14+NSPKX))-1000
            IF( NSPX.EQ.NSPXX ) THEN
              IF( NSPX.LE.NSPL ) THEN
                ACX = ACTVX(1,NSPX)
              ELSE
                ACX = 1.D0
              ENDIF
              AJM(NROW,NCOL)=AJM(NROW,NCOL)+EQ_K(M,NEQX)/ACX*VTOLX*DTI
            ENDIF
          ENDIF
         ENDDO
        ELSE
!
!---   Equilibrium mass action
!
          NSE = IEQ_E(1,NEQXX)
!
!---  Loop over equilibrium species  ---
!
          DO MSX = 2,NSE
            NCM_SP = IEQ_E(MSX+1,NEQXX)
            NCOL = IEQ_S(NCM_SP)-NEQE
            AJM(NROW,NCOL) = AJM(NROW,NCOL)+EQ_K(M,NEQX)*VTOLX*DTI*
     &        EQ_E(MSX-1,NEQXX)*SP_CX(NSPX)/SP_CX(NCM_SP)
          ENDDO
        ENDIF
  700 CONTINUE
!
!---  Return residual vector and Jacobian matrix  ---
!
      IF( ABS(INDX).EQ.1 ) GOTO 1000
      BJM(NROW) = -BJM(NROW)
 1000 CONTINUE
!
!---  End of KECHEM_R group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END


!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MOBCF( NEQ )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Mobile conservation component fractions.
!
!     YSPLX aqueous fraction of component species NEQ at node N
!     YSPGX gas fraction of component species NEQ at node N
!     YSPNX NAPL fraction of component species NEQ at node N
!     C(N,NSL) component species concentration (kmol/m^3 node)
!     SP_C(N,NSP) species concentration (kmol/m^3 node)
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, August 15, 2005.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
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
      SUB_LOG(ISUB_LOG) = '/MOBCF'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Loop over active nodes  ---
!
      DO 200 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 200
        YSPLX = 0.D+0
        YSPGX = 0.D+0
        YSPNX = 0.D+0
        YSPLZ = 0.D+0
        YSPGZ = 0.D+0
        YSPNZ = 0.D+0
        NSL = NSOLU + NEQ
        IF( ICT(N,NSL).NE.0 .AND. (NSTEP-NRST).EQ.0 ) GOTO 200
        C(N,NSL) = 0.D+0
!
!---    Loop over conservation-component species  ---
!
        DO 100 M = 1,IEQ_C(1,NEQ)
          NSP = IEQ_C(M+1,NEQ)
          IF( ABS(SP_C(N,NSP)).LT.CMIN ) THEN
            SP_CX = 0.D+0
          ELSE
            SP_CX = SP_C(N,NSP)
          ENDIF
!
!---      Aqueous species ---
!
          IF( NSP.LE.NSPL .AND. IEQW.GT.0 ) THEN
            YSPLX = YSPLX + EQ_C(M,NEQ)*SP_CX
            YSPLZ = YSPLZ + EQ_C(M,NEQ)
            C(N,NSL) = C(N,NSL) + EQ_C(M,NEQ)*SP_CX
!
!---      Gas species ---
!
          ELSEIF( NSP.GT.(NSPL+NSPS+NSPE) .AND.
     &      NSP.LE.(NSPL+NSPS+NSPE+NSPG) .AND. 
     &      (IEQA.GT.0 .OR. IOM.EQ.30 .OR. IOM.EQ.40) ) THEN
            YSPGX = YSPGX + EQ_C(M,NEQ)*SP_CX
            YSPGZ = YSPGZ + EQ_C(M,NEQ)
            C(N,NSL) = C(N,NSL) + EQ_C(M,NEQ)*SP_CX
!
!---      NAPL species ---
!
          ELSEIF( NSP.GT.(NSPL+NSPS+NSPE+NSPG) .AND.
     &      NSP.LE.(NSPL+NSPS+NSPE+NSPG+NSPN) .AND. IEQO.GT.0 ) THEN
            YSPNX = YSPNX + EQ_C(M,NEQ)*SP_CX
            YSPNZ = YSPNZ + EQ_C(M,NEQ)
            C(N,NSL) = C(N,NSL) + EQ_C(M,NEQ)*SP_CX
          ENDIF
  100   CONTINUE
!
!---    Update old time step conservation-component species ---
!
        CO(N,NSL) = C(N,NSL)
        YSPZ = YSPLZ+YSPGZ+YSPNZ
!
!---    Aqueous species ---
!
        IF( IEQW.GT.0 ) THEN
!
!---      Zero mobile species  ---
!
          IF( ABS(YSPZ)/EPSL.LT.EPSL ) THEN
            YSPLX = 0.D+0
!
!---      Zero species concentration  ---
!
          ELSEIF( ABS(C(N,NSL))/EPSL.LT.EPSL ) THEN
            YSPLX = YSPLZ/YSPZ
!            YSPLX = 0.D+0
!
!---      Non-zero species concentration  ---
!
          ELSE
            YSPLX = YSPLX/C(N,NSL)
          ENDIF
          YL(N,NSL) = YSPLX
!
!---      pH link ---
!
          IF( ISPLK(1).EQ.NSL ) THEN
            YL(N,NSL) = 1.D+0
!
!---      Air link ---
!
          ELSEIF( ISPLK(4).EQ.NSL ) THEN
            YL(N,NSL) = 1.D+0
!
!---      CO2 link ---
!
          ELSEIF( ISPLK(6).EQ.NSL ) THEN
            YL(N,NSL) = 1.D+0
          ENDIF
!
!---      Component link ---
!
          ISHIFTX=0
          IF( IOM.EQ.43 ) ISHIFTX=2
          DO IGC = 1,NGC+ISHIFTX
          IF( ISPLK(14+NSPLK+IGC).EQ.NSL ) THEN
            YL(N,NSL) = 1.D+0
          ENDIF
          END DO
        ENDIF
!
!---    Gas species ---
!
        IF( IEQA.GT.0 .OR. IOM.EQ.30 .OR. IOM.EQ.40 ) THEN
!
!---      Zero mobile species  ---
!
          IF( ABS(YSPZ)/EPSL.LT.EPSL ) THEN
            YSPGX = 0.D+0
!
!---      Zero species concentration  ---
!
          ELSEIF( ABS(C(N,NSL))/EPSL.LT.EPSL ) THEN
!            YSPGX = 0.D+0
            YSPGX = YSPGZ/(YSPZ)
!
!---      Non-zero species concentration  ---
!
          ELSE
            YSPGX = YSPGX/C(N,NSL)
          ENDIF
          YG(N,NSL) = YSPGX
!
!---      Component link ---
!
          ISHIFTX=0
          IF( IOM.EQ.43 ) ISHIFTX=2
          DO IGC = 1,NGC+ISHIFTX
          IF( ISPLK(14+NSPLK+NGC+ISHIFTX+IGC).EQ.NSL ) THEN
            YG(N,NSL) = 1.D+0
          ENDIF
          END DO
        ENDIF
!
!---    NAPL species ---
!
        IF( IEQO.GT.0 .OR. IOM.EQ.43 ) THEN
!
!---      Zero mobile species  ---
!
          IF( ABS(YSPZ)/EPSL.LT.EPSL ) THEN
            YSPNX = 0.D+0
!
!---      Zero species concentration  ---
!
          ELSEIF( ABS(C(N,NSL))/EPSL.LT.EPSL ) THEN
!            YSPNX = 0.D+0
            YSPNX = YSPNZ/(YSPZ)
!
!---      Non-zero species concentration  ---
!
          ELSE
            YSPNX = YSPNX/C(N,NSL)
          ENDIF
          YN(N,NSL) = YSPNX
        ENDIF
  200 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of MOBCF group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MOBKF( NEQ )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Mobile kinetic component fractions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, August 15, 2005.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
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
      SUB_LOG(ISUB_LOG) = '/MOBKF'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Convert to global component indices  ---
!
      NEQX = NEQ + NEQC
!
!---  Loop over active nodes  ---
!
      DO 200 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 200
        YSPLX = 0.D+0
        YSPGX = 0.D+0
        YSPNX = 0.D+0
        YSPLZ = 0.D+0
        YSPGZ = 0.D+0
        YSPNZ = 0.D+0
        NSL = NSOLU + NEQX
        IF( ICT(N,NSL).NE.0 .AND. (NSTEP-NRST).EQ.0 ) GOTO 200
        C(N,NSL) = 0.D+0
!
!---    Loop over kinetic-component species  ---
!
        DO 100 M = 1,IEQ_K(1,NEQ)
          NSP = IEQ_K(M+1,NEQ)
          IF( ABS(SP_C(N,NSP)).LT.CMIN ) THEN
            SP_CX = 0.D+0
          ELSE
            SP_CX = SP_C(N,NSP)
          ENDIF
!
!---      Aqueous species ---
!
          IF( NSP.LE.NSPL ) THEN
            YSPLX = YSPLX + EQ_K(M,NEQ)*SP_CX
            YSPLZ = YSPLZ + EQ_K(M,NEQ)
            C(N,NSL) = C(N,NSL) + EQ_K(M,NEQ)*SP_CX
!
!---      Gas species ---
!
          ELSEIF( NSP.GT.(NSPL+NSPS+NSPE) .AND.
     &      NSP.LE.(NSPL+NSPS+NSPE+NSPG) ) THEN
            YSPGX = YSPGX + EQ_K(M,NEQ)*SP_CX
            YSPGZ = YSPGZ + EQ_K(M,NEQ)
            C(N,NSL) = C(N,NSL) + EQ_K(M,NEQ)*SP_CX
!
!---      NAPL species ---
!
          ELSEIF( NSP.GT.(NSPL+NSPS+NSPE+NSPG) .AND.
     &      NSP.LE.(NSPL+NSPS+NSPE+NSPG+NSPN) ) THEN
            YSPNX = YSPNX + EQ_K(M,NEQ)*SP_CX
            YSPNZ = YSPNZ + EQ_K(M,NEQ)
            C(N,NSL) = C(N,NSL) + EQ_K(M,NEQ)*SP_CX
          ENDIF
  100   CONTINUE
!
!---    Update old time step kinetic-component species ---
!
        CO(N,NSL) = C(N,NSL)
        YSPZ = YSPLZ+YSPGZ+YSPNZ
!
!---    Aqueous species ---
!
        IF( IEQW.GT.0 ) THEN
!
!---      Zero mobile species ---
!
          IF( ABS(YSPZ)/EPSL.LT.EPSL ) THEN
            YSPLX = 0.D+0
!
!---      Zero species concentration  ---
!
          ELSEIF( ABS(C(N,NSL))/EPSL.LT.EPSL ) THEN
!            YSPLX = 0.D+0
            YSPLX = YSPLZ/YSPZ
!
!---      Non-zero species concentration  ---
!
          ELSE
            YSPLX = YSPLX/C(N,NSL)
          ENDIF
          YL(N,NSL) = YSPLX
        ENDIF
!
!---    Gas species ---
!
        IF( IEQA.GT.0 .OR. IOM.EQ.30 .OR. IOM.EQ.40 ) THEN
!
!---      Zero mobile species ---
!
          IF( ABS(YSPZ)/EPSL.LT.EPSL ) THEN
            YSPGX = 0.D+0
!
!---      Zero species concentration  ---
!
          ELSEIF( ABS(C(N,NSL))/EPSL.LT.EPSL ) THEN
!            YSPGX = 0.D+0
            YSPGX = YSPGZ/YSPZ
!
!---      Non-zero species concentration  ---
!
          ELSE
            YSPGX = YSPGX/C(N,NSL)
          ENDIF
          YG(N,NSL) = YSPGX
        ENDIF
!
!---    NAPL species ---
!
        IF( IEQO.GT.0 ) THEN
!
!---      Zero mobile species ---
!
          IF( ABS(YSPZ)/EPSL.LT.EPSL ) THEN
            YSPNX = 0.D+0
!
!---      Zero species concentration  ---
!
          ELSEIF( ABS(C(N,NSL))/EPSL.LT.EPSL ) THEN
!            YSPNX = 0.D+0
            YSPNX = YSPNZ/YSPZ
!
!---      Non-zero species concentration  ---
!
          ELSE
            YSPNX = YSPNX/C(N,NSL)
          ENDIF
          YN(N,NSL) = YSPNX
        ENDIF
  200 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of MOBKF group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MPROVE( A,AX,N,NP,IX,B,BX )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Numerical Recipes, The Art of Scientific Computing
!     W.H. Press, B.P. Flannery, Saul A. Teukolsky, and W.T. Vetterling
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, January 25, 2005.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
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
      REAL*8 A(NP,NP),B(NP),AX(NP,NP),BX(NP),RX(NP)
      INTEGER IX(NP)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/MPROVE'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
      DO 12 I = 1,N
        SDPX = -B(I)
        DO 11 J = 1,N
          SDPX = SDPX + A(I,J)*BX(J)
   11   CONTINUE
        RX(I) = SDPX
   12 CONTINUE
      CALL LUBKSB( AX,N,NP,IX,RX )
      DO 13 I = 1,N
        BX(I) = BX(I) - RX(I)
   13 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of MPROVE group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE NMNSP
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Normalize mineral species.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 2 May 2006.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
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
      SUB_LOG(ISUB_LOG) = '/NMNSP'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Loop over active nodes  ---
!
      DO 100 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 100
!
!---    Loop over solid species  ---
!
        DO 10 NSPX = 1,NSPS
          NSP = NSPL + NSPX
!
!---      Mineral species  ---
!
          IF( ISP_MN(NSP).EQ.1 ) THEN
            SP_CMN(N,NSPX) = SP_C(N,NSP)
            SP_C(N,NSP) = 0.D+0
          ENDIF
   10   CONTINUE
  100 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of NMNSP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE PITZER( ACTVX,SP_CX,DSP_CX,SLX,PORDX,RHOLX,TX,XLWX )
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
!     This subroutine computes activity and osmotic coefficients for
!     electrolyte solutions: Pitzer's equations for mixed 
!     electrolyte solutions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by A. Felmy, from GMIN.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE PTZRCOEF
      USE PTZR
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
!----------------------Type Declarations-------------------------------!
!

      REAL*8 ACTVX(LSPL+1,LSPL),SP_CX(LSPL),DSP_CX(LSPL)
      REAL*8 CPIX(LSPL+1),CAPZ(LSPL+1),LNG(LSPL+1,LSPL),TFUNC
      REAL*8 FF(LSPL+1),G4M(LSPL+1)
      EXTERNAL TFUNC
      REAL*8 LNA, G(4),GP(4),GPP(4),ALPHA(4),ATTMP(8)
!
!----------------------Data Statements---------------------------------!
!
!      SAVE TSMI
!      DATA TSMI / -1.D+3 /
      DATA ALPHA / 2.0D+00,1.40D+00,1.20D+01,5.0D+01 /
      DATA BB  /1.2D+00 /

!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/PITZER'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!--- Initialize Variables
!
      DO I = 1,4
        G(I) = 0.0
        GP(I) = 0.0
        GPP(I) = 0.0
      ENDDO
      
      DO I = 1,8
        ATTMP(I) = 0.0
      ENDDO

      DO M = 1,NSPL+1
        CAPZ(M) = 0.0
        CPIX(M) = 0.0
          FF(M) = 0.0
          G4M(M) = 0.0
        DO NSP = 1,NSPL
          LNG(M,NSP) = 0.0
        ENDDO
      ENDDO
      
      IKH2O = 0

!
!---  Recalculate Pitzer parameters for non-isothermal solution.
!
      TK = TX + 273.15d0

      APHI = 0.336901532d0-6.3210043D-04*TK+9.14252359d0/
     &       TK-1.35143986D-02*DLOG(TK)+2.26089488D-03/(TK-263.d0)+
     &       1.92118597D-06*TK*TK+4.52586464D+01/(680.d0-TK)

!      IF( ABS(TX-TSMI).GT.EPSL ) THEN
!  
!---  Re-calculate binary parameters
!
        DO I = 1,NCC_PZ
          DO J = 1,NA_PZ
            DO k = 1,8
              ATTMP(K) = ATB0(I,J,K)
            END DO
            B0(I,J) = TFUNC(ATTMP,TK)
          END DO
        END DO
        
        DO I = 1,NCC_PZ
          DO J = 1,NA_PZ
            DO K = 1,8
              ATTMP(K) = ATB1(I,J,K)
            END DO
            B1(I,J) = TFUNC(ATTMP,TK)
          END DO
        END DO
 
        DO I = 1,NCC_PZ
          DO J = 1,NA_PZ
            DO K = 1,8
             ATTMP(K) = ATB2(I,J,K)
            END DO
            B2(I,J) = TFUNC(ATTMP,TK)
          END DO
        END DO
  
        DO I = 1,NCC_PZ
          DO J = 1,NA_PZ
            DO K = 1,8
              ATTMP(K) = ATCMX(I,J,K)
            END DO
            CMXX(I,J) = TFUNC(ATTMP,TK)
            CMXX(I,J)=CMXX(I,J)/(2.*SQRT(ABS(SP_L(1,JPC(I))*
     &                SP_L(1,JPA(J)))))
          END DO
        END DO
!  
!---  Recalculate theta and psi.
!  
        II = 0
        IF( NCC_PZ.GE.2 )THEN
          DO I = 1,NCC_PZ
            DO J = I+1,NCC_PZ
              II = II+1
              DO K = 1,6
                ATTMP(K) = ATTC(II,K)
              END DO
              TCC(II)=TFUNC(ATTMP,TK)
              DO KK = 1,NA_PZ
                DO K = 1,6
                  ATTMP(K) = ATPC(II,KK,K)
                END DO
                PSIC(II,KK)=TFUNC(ATTMP,TK)
              END DO
            END DO
          END DO
        END IF
 
        II = 0
        IF( NA_PZ.GE.2 )THEN
          DO I = 1,NA_PZ
            DO J = I+1,NA_PZ
              II = II+1
              DO K = 1,6
                ATTMP(K) = ATTA(II,K)
              END DO
              TAA(II)=TFUNC(ATTMP,TK)
              DO KK = 1,NCC_PZ
                DO K = 1,6
                  ATTMP(K) = ATPA(II,KK,K)
                END DO
                PSIA(II,KK)=TFUNC(ATTMP,TK)
              END DO
            END DO
          END DO
        END IF
!  
!--- Recalculate ternary parameters.
!  
        DO I = 1,NNN_PZ
          DO J = 1,NNN_PZ
            DO K = 1,6
              ATTMP(K) = ATNLAM(I,J,K)
            END DO
            ELAMB(I,J) = TFUNC(ATTMP,TK)
          END DO
          
          DO J = 1,NCC_PZ
            DO K = 1,6
              ATTMP(K) = ATCLAM(I,J,K)
            END DO
            CLAMB(I,J) = TFUNC(ATTMP,TK)
          END DO
          
          DO J = 1,NA_PZ
            DO K = 1,6
              ATTMP(K) = ATALAM(I,J,K)
            END DO
            ALAMB(I,J) = TFUNC(ATTMP,TK)
          END DO
        END DO
        
        II = 0
        DO I = 1,NNN_PZ
          DO J = 1,NCC_PZ
            II = II+1
            DO KK = 1,NA_PZ
              DO K = 1,6
               ATTMP(K) = ATHLAM(II,KK,K)
              END DO
              HOLAMB(II,KK) = TFUNC(ATTMP,TK)
            END DO
          END DO
        END DO
!      END IF
!      TSMI = TX
!
!--- End temperature-dependent calculation of Pitzer parameters
!
!
!---  Ionic strength of the aqueous solution  ---
!
      DO 40 M = 1,NSPL+1
        CPIX(M) = 0.D+0
        SUM_M = 0.D+0
        DO 30 NSP = 1,NSPL
          IF(IDD_PZ(NSP).GE.300000) GO TO 30
          IF(SPNML(NSP).EQ.'h2o') THEN
            IKH2O = NSP
            GO TO 30
          ENDIF
          IF( NSP.EQ.(M-1) ) THEN
            CLX = SP_CX(NSP) + DSP_CX(NSP)
          ELSE
            CLX = SP_CX(NSP)
          ENDIF
!       
!---    Molarity in mol solute/m^3 aqueous
!       or mol solute/l aqueous  ---
!       
          CMX = CLX/(SLX*PORDX)
!       
!---    Molality in mol solute/kg water  ---
!       
          CMX = CMX/(RHOLX*XLWX)
          SUM_M = SUM_M + CMX
          CPIX(M) = CPIX(M) + CMX*(SP_L(1,NSP)**2)
          CAPZ(M) = CAPZ(M) + CMX*DABS(SP_L(1,NSP))
 30     CONTINUE
        CPIX(M) = CPIX(M)*5.D-1
 40   CONTINUE

!
!--- Calculate Pitzer Activities
!
      DO 60 M = 1,NSPL+1

        PHI1 = 0.0
        PHI2 = 0.0
        PHI3 = 0.0
        PHI4 = 0.0
        PHI5 = 0.0
        PHI6 = 0.0
        PHI7 = 0.0
        PHI8 = 0.0
        
        F1   = 0.0
        F2   = 0.0
        F3   = 0.0
        F4   = 0.0
        F5   = 0.0
        TMA  = 0.0
        TMCX = 0.0
        TMN  = 0.0
        FPR  = 0.0
!
!--- Calculate g functions
!
        DO I = 1,4
          X1 = ALPHA(I)*DSQRT(CPIX(M))
          X2 = X1*X1
          DEX = DEXP(-X1)
          G(I) = 2.0D0*(1.0D0-(1.0D0+X1)*DEX)/X2
          GP(I) = -2.0D0*(1.0D0-(1.0D0+X1+X2/2.0D0)*DEX)/X2
          GPP(I) = -(2.0d0*GP(I)+(X1/2.0d0)*DEX)/(CPIX(M)*CPIX(M))
        END DO
!
!--- Calculate b functions
!
        DO I = 1,NCC_PZ
          DO J = 1,NA_PZ
            K1 = 1
            K2 = 3
            IF( SP_L(1,JPC(I)).GE.2.0D0.AND.
     &        DABS(SP_L(1,(JPA(J)))).GE.2.0D0 )THEN
              K1 = 2
              K2 = 4
            END IF
            IF(SP_L(1,JPC(I)).EQ.2.0D0.AND.
     &        DABS(SP_L(1,JPA(J))).EQ.2.0D0 )THEN
              K1 = 2
              K2 = 3
            END IF
            X1 = -ALPHA(K1)*DSQRT(CPIX(M))
            X2 = -ALPHA(K2)*DSQRT(CPIX(M))
            BMMX(I,J) = B0(I,J)+B1(I,J)*G(K1)+B2(I,J)*G(K2)
            BPHI(I,J) = B0(I,J)+B1(I,J)*DEXP(X1)+B2(I,J)*DEXP(X2)
            BPR(I,J) = B1(I,J)*GP(K1)/CPIX(M)+B2(I,J)*GP(K2)/CPIX(M)
            BPPR(I,J) = B1(I,J)*GPP(K1)+B2(I,J)*GPP(K2)
          END DO
        END DO
!
!--- Calculate higher order mixing functions
!
        CALL HOMIX(CPIX(M),APHI)
!
!--- Start calculations for activity and osmotic coefficients
!
        TMP = 1.0d0+BB*DSQRT(CPIX(M))
        F1 = -APHI*DSQRT(CPIX(M))/TMP
        F2 = -APHI*(2.0d0/BB)*DLOG(TMP)
        PHI1 = F1*CPIX(M)
        FPR = -APHI*(TMP+0.5d0)/(DSQRT(CPIX(M))*TMP*TMP)
!
!--- Start first major loop
!--- The terms are labeled in roughly the order they appear
!--- in the sums given by Felmy and Weare

        F3 = 0.0D0
        PHI2 = 0.0D0
        HBPP = 0.0D0

        DO I = 1,NCC_PZ
          DO J = 1,NA_PZ
            IF( JPC(I).EQ.(M-1) ) THEN
              CLXC = SP_CX(JPC(I)) + DSP_CX(JPC(I))
            ELSE
              CLXC = SP_CX(JPC(I))
            ENDIF
            IF( JPA(J).EQ.(M-1) ) THEN
              CLXA = SP_CX(JPA(J)) + DSP_CX(JPA(J))
            ELSE
              CLXA = SP_CX(JPA(J))
            ENDIF
            CMXC = CLXC/(SLX*PORDX)
            CMXC = CMXC/(RHOLX*XLWX)
            CMXA = CLXA/(SLX*PORDX)
            CMXA = CMXA/(RHOLX*XLWX)
            TMP = CMXC*CMXA
            F3 = F3+TMP*BPR(I,J)
            G4M(M) = G4M(M)+TMP*CMXX(I,J)
            PHI2 = PHI2+TMP*BPHI(I,J)
            HBPP = HBPP+TMP*BPPR(I,J)
            TMP1 = 2.0d0*BMMX(I,J)+CAPZ(M)*CMXX(I,J)
            LNG(M,JPC(I)) = LNG(M,JPC(I))+CMXA*TMP1
            LNG(M,JPA(J)) = LNG(M,JPA(J))+CMXC*TMP1
          END DO
        END DO

        
        PHI2 = PHI2+CAPZ(M)*G4M(M)

!
!--- Ternary electrolyte terms
!
        F4 = 0.0D0
        PHI3 = 0.0D0
        HTC = 0.0D0
        
        IF( NCC_PZ.GE.2 )THEN
          NT = 1
          DO I = 1,NCC_PZ-1
            DO J = I+1,NCC_PZ
              IF( JPC(I).EQ.(M-1) ) THEN
                CLXC = SP_CX(JPC(I)) + DSP_CX(JPC(I))
              ELSE
                CLXC = SP_CX(JPC(I))
              ENDIF
              IF( JPC(J).EQ.(M-1) ) THEN
                CLXC2 = SP_CX(JPC(J)) + DSP_CX(JPC(J))
              ELSE
                CLXC2 = SP_CX(JPC(J))
              ENDIF
              CMXC = CLXC/(SLX*PORDX)
              CMXC = CMXC/(RHOLX*XLWX)
              CMXC2 = CLXC2/(SLX*PORDX)
              CMXC2 = CMXC2/(RHOLX*XLWX)
              TMP = CMXC*CMXC2
              F4 = F4+TMP*CTCPR(NT)
              PHI3 = PHI3+TMP*CTCPH(NT)
              HTC = HTC+TMP*CTCPPR(NT)
!      
!--- Now g2m
!      
              DO K = 1,NCC_PZ
                N1 = 0
                IF( JPC(I).EQ.JPC(K) ) N1=J
                IF( JPC(J).EQ.JPC(K) ) N1=I
                IF( N1.NE.0 )THEN
                  IF( JPC(N1).EQ.(M-1) ) THEN
                    CLXC = SP_CX(JPC(N1)) + DSP_CX(JPC(N1))
                  ELSE
                    CLXC = SP_CX(JPC(N1))
                  ENDIF
                  CMXC = CLXC/(SLX*PORDX)
                  CMXC = CMXC/(RHOLX*XLWX)
                  TMP1 = CMXC
                  LNG(M,JPC(K)) = LNG(M,JPC(K))+2.0d0*TMP1*CTC(NT)
                  DO N = 1,NA_PZ
                    IF( JPA(N).EQ.(M-1) ) THEN
                      CLXA = SP_CX(JPA(N)) + DSP_CX(JPA(N))
                    ELSE
                      CLXA = SP_CX(JPA(N))
                    ENDIF
                    CMXA = CLXA/(SLX*PORDX)
                    CMXA = CMXA/(RHOLX*XLWX)
                    LNG(M,JPC(K)) = LNG(M,JPC(K))+TMP1*CMXA*PSIC(NT,N)
                  END DO
                END IF
              END DO
             
              DO K = 1,NA_PZ
                TMP1 = TMP*PSIC(NT,K)
                LNG(M,JPA(K)) = LNG(M,JPA(K))+TMP1
                IF( JPA(K).EQ.(M-1) ) THEN
                  CLXA = SP_CX(JPA(K)) + DSP_CX(JPA(K))
                ELSE
                  CLXA = SP_CX(JPA(K))
                ENDIF
                CMXA = CLXA/(SLX*PORDX)
                CMXA = CMXA/(RHOLX*XLWX)
                PHI3 = PHI3+TMP1*CMXA
              END DO
              NT=NT+1
            END DO
          END DO
        END IF

        F5 = 0.0D0
        PHI4 = 0.0D0
        HTA = 0.0D0

        IF( NA_PZ.GE.2 )THEN
          NT = 1
          DO I = 1,NA_PZ-1
            DO J = I+1,NA_PZ
              IF( JPA(I).EQ.(M-1) ) THEN
                CLXA = SP_CX(JPA(I)) + DSP_CX(JPA(I))
              ELSE
                CLXA = SP_CX(JPA(I))
              ENDIF
              IF( JPA(J).EQ.(M-1) ) THEN
                CLXA2 = SP_CX(JPA(J)) + DSP_CX(JPA(J))
              ELSE
                CLXA2 = SP_CX(JPA(J))
              ENDIF
              CMXA = CLXA/(SLX*PORDX)
              CMXA = CMXA/(RHOLX*XLWX)
              CMXA2 = CLXA2/(SLX*PORDX)
              CMXA2 = CMXA2/(RHOLX*XLWX)
              TMP = CMXA*CMXA2
              F5 = F5+TMP*CTAPR(NT)
              PHI4 = PHI4+TMP*CTAPH(NT)
              HTA = HTA+TMP*CTAPPR(NT)
!      
!--- Now g2x
!      
              DO K = 1,NA_PZ
                N1 = 0
                IF( JPA(I).EQ.JPA(K) )N1=J
                IF( JPA(J).EQ.JPA(K) )N1=I
                IF( N1.NE.0 )THEN
                  IF( JPA(N1).EQ.(M-1) ) THEN
                    CLXA = SP_CX(JPA(N1)) + DSP_CX(JPA(N1))
                  ELSE
                    CLXA = SP_CX(JPA(N1))
                  ENDIF
                  CMXA = CLXA/(SLX*PORDX)
                  CMXA = CMXA/(RHOLX*XLWX)
                  TMP1 = CMXA
                  LNG(M,JPA(K)) = LNG(M,JPA(K))+2.0d0*TMP1*CTA(NT)
                  DO N = 1,NCC_PZ
                    IF( JPC(N).EQ.(M-1) ) THEN
                      CLXC = SP_CX(JPC(N)) + DSP_CX(JPC(N))
                    ELSE
                      CLXC = SP_CX(JPC(N))
                    ENDIF
                    CMXC = CLXC/(SLX*PORDX)
                    CMXC = CMXC/(RHOLX*XLWX)
                    LNG(M,JPA(K)) = LNG(M,JPA(K))+TMP1*CMXC*PSIA(NT,N)
                  END DO
                END IF
              END DO
          
              DO K = 1,NCC_PZ
                TMP1 = TMP*PSIA(NT,K)
                LNG(M,JPC(K)) = LNG(M,JPC(K))+TMP1
                IF( JPC(K).EQ.(M-1) ) THEN
                  CLXC = SP_CX(JPC(K)) + DSP_CX(JPC(K))
                ELSE
                  CLXC = SP_CX(JPC(K))
                ENDIF
                CMXC = CLXC/(SLX*PORDX)
                CMXC = CMXC/(RHOLX*XLWX)
                PHI4 = PHI4+TMP1*CMXC
              END DO
              NT = NT+1
            END DO
          END DO
        END IF


        PHI5 = 0.0D0
        PHI6 = 0.0D0
        PHI7 = 0.0D0
        PHI8 = 0.0D0
        
        IF( NNN_PZ.GT.0 )THEN
          NT = 1
          DO I = 1,NNN_PZ
            IF( JPN(I).EQ.(M-1) ) THEN
              CLXN = SP_CX(JPN(I)) + DSP_CX(JPN(I))
            ELSE
              CLXN = SP_CX(JPN(I))
            ENDIF
            CMXN = CLXN/(SLX*PORDX)
            CMXN = CMXN/(RHOLX*XLWX)
            TMN = CMXN
            DO J = 1,NCC_PZ
              IF( JPC(J).EQ.(M-1) ) THEN
                CLXC = SP_CX(JPC(J)) + DSP_CX(JPC(J))
              ELSE
                CLXC = SP_CX(JPC(J))
              ENDIF
              CMXC = CLXC/(SLX*PORDX)
              CMXC = CMXC/(RHOLX*XLWX)
              TMCX = CMXC
              PHI5 = PHI5+TMN*TMCX*CLAMB(I,J)
              LNG(M,JPC(J)) = LNG(M,JPC(J))+2.0d0*TMN*CLAMB(I,J)
              LNG(M,JPN(I)) = LNG(M,JPN(I))+2.0d0*TMCX*CLAMB(I,J)
              DO K = 1,NA_PZ
                IF( JPA(K).EQ.(M-1) ) THEN
                  CLXA = SP_CX(JPA(K)) + DSP_CX(JPA(K))
                ELSE
                  CLXA = SP_CX(JPA(K))
                ENDIF
                CMXA = CLXA/(SLX*PORDX)
                CMXA = CMXA/(RHOLX*XLWX)
                TMA = CMXA
                PHI7 = PHI7+TMN*TMCX*TMA*HOLAMB(NT,K)
                LNG(M,JPN(I)) = LNG(M,JPN(I))+TMCX*TMA*HOLAMB(NT,K)
                LNG(M,JPC(J)) = LNG(M,JPC(J))+TMN*TMA*HOLAMB(NT,K)
                LNG(M,JPA(K)) = LNG(M,JPA(K))+TMN*TMCX*HOLAMB(NT,K)
              END DO
              NT = NT+1
            END DO
            DO K = 1,NA_PZ
              LNG(M,JPA(K)) = LNG(M,JPA(K))+2.0d0*TMN*ALAMB(I,K)
              IF( JPA(K).EQ.(M-1) ) THEN
                CLXA = SP_CX(JPA(K)) + DSP_CX(JPA(K))
              ELSE
                CLXA = SP_CX(JPA(K))
              ENDIF
              CMXA = CLXA/(SLX*PORDX)
              CMXA = CMXA/(RHOLX*XLWX)
              LNG(M,JPN(I)) = LNG(M,JPN(I))+2.0D0*CMXA*ALAMB(I,K)
              PHI6 = PHI6+TMN*CMXA*ALAMB(I,K)
            END DO
            DO K = 1,NNN_PZ
              IF( JPN(K).EQ.(M-1) ) THEN
                CLXN = SP_CX(JPN(K)) + DSP_CX(JPN(K))
              ELSE
                CLXN = SP_CX(JPN(K))
              ENDIF
              CMXN = CLXN/(SLX*PORDX)
              CMXN = CMXN/(RHOLX*XLWX)
              PHI8 = PHI8+CMXN*TMN*ELAMB(I,K)
              LNG(M,JPN(I)) = LNG(M,JPN(I))+2.0d0*CMXN*ELAMB(I,K)
            END DO
          END DO
        END IF
!
!--- Sum up the f function and carry along the proper charges
!
        FF(M) = F1+F2+F3+F4+F5
!
!--- Calculate osmotic coefficient
!
        IF(SUM_M.NE.0)PHI = (2.0d0/SUM_M)*
     &                      (PHI1+PHI2+PHI3+PHI4+PHI5+PHI6+PHI7+PHI8)
        PHI = PHI+1.0d0
        
        DO 50 NSP = 1,NSPL
          IF( NSP.EQ.IKH2O ) THEN
            IF( IKH2O.EQ.(M-1) ) THEN
              CLX = SP_CX(IKH2O) + DSP_CX(IKH2O)
            ELSE
              CLX = SP_CX(IKH2O)
            ENDIF
            CMX = CLX/(SLX*PORDX)
            CMX = CMX/(RHOLX*XLWX)
            LNA = -.0180153*SUM_M*PHI
            H2OACT = DEXP(LNA)
            LNG(M,IKH2O) = LNA - DLOG(CMX)
            ACTVX(M,IKH2O) = DEXP(LNG(M,IKH2O))
          ELSE
            LNG(M,NSP) = LNG(M,NSP)+SP_L(1,NSP)*SP_L(1,NSP)*FF(M)+
     &                 DABS(SP_L(1,NSP))*G4M(M)
            ACTVX(M,NSP) = DEXP(LNG(M,NSP))
          ENDIF
  50    CONTINUE
  60  CONTINUE

!
!---  End of PITZER ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END 


!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE PTZRP
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
!     This subroutine reads in the necessary pitzer parameters for
!     a non-ideal solution model. The parameters are stored in three
!     separate files.  In this version the parameters are parameterized
!     as a function of temperature.
!
!     file lun1 contains the single electrolyte parameters
!     file lun2 contains the theta and psi parameters
!     file lun3 contains the lambdas and higher order lambdas
!
!----------------------Authors-----------------------------------------!
!
!     Written by A. Felmy, from GMIN.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE PTZRCOEF
      USE PTZR
      USE FILES
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

      INTEGER idtmp(3,2),idtmp2(3)
      CHARACTER*64 binfile,ternfile,lamfile
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/PTZRP'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Open Files
!
      BINFILE = 'binaryt.dat'
      TERNFILE = 'ternaryt.dat'
      LAMFILE = 'lambdat.dat'
      LUN1=9
      LUN2=10
      LUN3=11
      OPEN(UNIT=LUN1,FILE=binfile,STATUS='OLD')
      OPEN(UNIT=LUN2,FILE=ternfile,STATUS='OLD')
      OPEN(UNIT=LUN3,FILE=lamfile,STATUS='OLD')
!
!---  Read Id tag lines ---
!
      READ(LUN1,'(A)') CHDUM
      READ(LUN2,'(A)') CHDUM
      READ(LUN3,'(A)') CHDUM
!
!---  Initialize variables
!
      AT1 = 0.0
      AT2 = 0.0
      AT3 = 0.0
      AT4 = 0.0
      AT5 = 0.0
      AT6 = 0.0
      AT7 = 0.0
      AT8 = 0.0
!
!---  Temperature is in T(2,N)
!
      TK = 2.7315D+02 + 2.5D+01
      TEMPC = TK-2.7315D+02
!     TEMPC = 25.
!
!---  Now read in the single electrolyte parameters
!
  100 READ(LUN1,5000) ID1,ID2,INDX,INDXT,AT1,AT2,AT3, 
     &                AT4,AT5,AT6,AT7,AT8,TTMIN,TTMAX
5000  FORMAT(I6,1X,I6,I2,I2,8F15.7,F7.3,1X,F7.3)

      IF( ID1.EQ.0 ) GO TO 200
      IF( INDXT.EQ.1.AND.TEMPC.GT.TTMAX) GO TO 100
      IF( INDXT.EQ.2.AND.TEMPC.LT.TTMIN) GO TO 100
      TBB = AT1+AT2*TK+AT3/TK+AT4*DLOG(TK)+AT5/(TK-263.0)+AT6*TK*TK
     &   +AT7/(680.0-TK)+AT8/(TK-227.0)
      DO I=1,NCC_PZ
        IF( IDD_PZ(JPC(I)).EQ.ID1 )THEN
          DO J=1,NA_PZ
            IF(IDD_PZ(JPA(J)).EQ.ID2) THEN
              IF( INDX.EQ.1 ) THEN
                B0(I,J)=TBB
                ATB0(I,J,1) = AT1
                ATB0(I,J,2) = AT2
                ATB0(I,J,3) = AT3
                ATB0(I,J,4) = AT4
                ATB0(I,J,5) = AT5
                ATB0(I,J,6) = AT6
                ATB0(I,J,7) = AT7
                ATB0(I,J,8) = AT8
                IF( TEMPC.GT.TTMAX )THEN
                  WRITE(IWR,5017)
                  WRITE(IWR,5018)TEMPC,TTMAX,SPNML(JPC(I)),SPNML(JPA(J))
                ENDIF
                IF( TEMPC.LT.TTMIN )THEN
                  WRITE(IWR,5017)
                  WRITE(IWR,5019)TEMPC,TTMIN,SPNML(JPC(I)),SPNML(JPA(J))
                ENDIF
5017            FORMAT(/'*** WARNING ****')
5018            FORMAT('Input temperature',1x,f7.3,1x,'exceeds max 
     & database temp (',F7.3,' ) for the single electrolyte parameters 
     & (b0) of', 1X,A10,'and',1X,A10)
5019            FORMAT('Input temperature',1x,f7.3,1x,'is less than 
     & min databasetemp (',F7.3,' ) for the single electrolyte 
     & parameters (b0) of',  1X,A10,'and',1X,A10)
              END IF

              IF( INDX.EQ.2 )THEN
                B1(I,J)=TBB
                ATB1(I,J,1) = AT1
                ATB1(I,J,2) = AT2
                ATB1(I,J,3) = AT3
                ATB1(I,J,4) = AT4
                ATB1(I,J,5) = AT5
                ATB1(I,J,6) = AT6
                ATB1(I,J,7) = AT7
                ATB1(I,J,8) = AT8
                IF( TEMPC.GT.TTMAX )THEN
                  WRITE(IWR,5017)
                  WRITE(IWR,5021)TEMPC,TTMAX,SPNML(JPC(I)),SPNML(JPA(J))
                ENDIF
                IF( TEMPC.LT.TTMIN )THEN
                  WRITE(IWR,5017)
                  WRITE(IWR,5022)TEMPC,TTMIN,SPNML(JPC(I)),SPNML(JPA(J))
                ENDIF
5021            FORMAT('Input temperature',1X,F7.3,1X,'exceeds max 
     & database temp (',f7.3,' ) for the single electrolyte 
     & parameters (b1) of', 1X,A10,'and',1X,A10)
5022            FORMAT('Input temperature',1X,F7.3,1X,'is less than min
     &  database temp (',F7.3,' ) for the single electrolyte parameters
     &  (b1) of', 1X,A10,'and',1X,A10)
              END IF
              
              IF( INDX.EQ.3 )THEN
                B2(I,J)=TBB
                ATB2(I,J,1) = AT1
                ATB2(I,J,2) = AT2
                ATB2(I,J,3) = AT3
                ATB2(I,J,4) = AT4
                ATB2(I,J,5) = AT5
                ATB2(I,J,6) = AT6
                ATB2(I,J,7) = AT7
                ATB2(I,J,8) = AT8
                IF( TEMPC.GT.TTMAX )THEN
                  WRITE(IWR,5017)
                  WRITE(IWR,5023)TEMPC,TTMAX,SPNML(JPC(I)),SPNML(JPA(J))
                ENDIF
                IF( TEMPC.LT.TTMIN )THEN
                  WRITE(IWR,5017)
                  WRITE(IWR,5024)TEMPC,TTMIN,SPNML(JPC(I)),SPNML(JPA(J))
                ENDIF
5023            FORMAT('Input temperature',1X,F7.3,1X,'exceeds max 
     & database temp (',F7.3,' ) for the single electrolyte parameters
     &  (b2) of',  1X,a10,'and',1X,A10)
5024            FORMAT('Input temperature',1X,F7.3,1x,'is less than min
     &  database temp (',F7.3,' ) for the single electrolyte parameters
     &  (b2) of', 1X,A10,'and',1X,A10)
              END IF
        
              IF( INDX.EQ.4 )THEN
                CMXX(I,J)=TBB
                ATCMX(I,J,1) = AT1
                ATCMX(I,J,2) = AT2
                ATCMX(I,J,3) = AT3
                ATCMX(I,J,4) = AT4
                ATCMX(I,J,5) = AT5
                ATCMX(I,J,6) = AT6
                ATCMX(I,J,7) = AT7
                ATCMX(I,J,8) = AT8
                IF( TEMPC.GT.TTMAX )THEN
                  WRITE(IWR,5017)
                  WRITE(IWR,5025)TEMPC,TTMAX,SPNML(JPC(I)),SPNML(JPA(J))
                ENDIF
                IF( TEMPC.LT.TTMIN )THEN
                  WRITE(IWR,5017)
                  WRITE(IWR,5026)TEMPC,TTMIN,SPNML(JPC(I)),SPNML(JPA(J))
                ENDIF
5025            FORMAT('Input temperature',1X,F7.3,1X,'exceeds max 
     & database temp (',f7.3,' ) for the single electrolyte parameters
     &  (cmxx) of',  1X,A10,'and',1X,A10)
5026            FORMAT('Input temperature',1X,F7.3,1X,'is less than min
     & database temp (',F7.3,' ) for the single electrolyte parameters
     & (cmxx) of', 1X,A10,'and',1X,A10)
              END IF
              GO TO 100
            END IF
          ENDDO
          GOTO 100
        END IF
      ENDDO
      GOTO 100

200   CONTINUE
210   CONTINUE

      DO i=1,3
        IDTMP2(I)=0
        DO J=1,2
          IDTMP(I,J)=0
        END DO
      END DO
      READ(LUN3,5011) ID1,ID2,ID3A,A1T,A2T,A3T,A4T,A5T,A6T,TTMIN,TTMAX
5011  FORMAT(I6,1X,I6,1X,I6,1X,6F15.7,F7.3,1X,F7.3)

      IF( ID1.NE.0 )THEN
        TLAMBDA = A1T+A2T*TK+A3T/TK+A4T*dlog(TK)+A5T/
     &            (TK-263.0)+A6T*TK*TK
        IDTMP2(1)=ID1
        IDTMP2(2)=ID2
        IDTMP2(3)=ID3A
        IF(ID3A.NE.0)THEN
          NJ=3
        ELSE
          NJ=2
        END IF
        DO J=1,NJ
          DO I=1,NNN_PZ
            IF( IDD_PZ(JPN(I)).EQ.IDTMP2(J) )THEN
              IDTMP(J,1)=1
              IDTMP(J,2)=i
              J1 = JPN(I)
            END IF
          END DO
          DO I=1,NCC_PZ
            IF( IDD_PZ(JPC(I)).EQ.IDTMP2(J) )THEN
              IDTMP(J,1)=2
              IDTMP(J,2)=i
              J2 = JPC(I)
            END IF
          END DO
          DO I=1,NA_PZ
            IF( IDD_PZ(JPA(I)).EQ.IDTMP2(J) )THEN
              IDTMP(J,1)=3
              IDTMP(J,2)=I
              J3 = JPA(I)
            END IF
          END DO
        END DO
        
        IF(( IDTMP(1,1).EQ.0).OR.(IDTMP(2,1).EQ.0) )THEN
          GO TO 210
        ELSE IF( IDTMP(3,1).EQ.0.and.NJ.EQ.3 )THEN
          GO TO 210
        END IF
        
  
        IF( TEMPC.GT.TTMAX.OR.TEMPC.LT.TTMIN )THEN
          WRITE(IWR,5017)
          WRITE(IWR,*)'Temperature out of range in lambdat.dat neutral
     &ion parameters for:'
          IF( NJ.EQ.2 )THEN
            WRITE(IWR,5029)SPNML(J1),SPNML(J2)
          ENDIF
          IF( NJ.EQ.3 )THEN
            WRITE(IWR,5029)SPNML(J1),SPNML(J2),SPNML(J3)
          ENDIF
5029      FORMAT(3(1x,a10))
        END IF
  
        IF( NJ.EQ.2 )THEN
          IF(IDTMP(1,1).EQ.1.AND.IDTMP(2,1).EQ.1)THEN
            ELAMB(IDTMP(1,2),IDTMP(2,2)) = TLAMBDA
            ATNLAM(IDTMP(1,2),IDTMP(2,2),1) = A1T
            ATNLAM(IDTMP(1,2),IDTMP(2,2),2) = A2T
            ATNLAM(IDTMP(1,2),IDTMP(2,2),3) = A3T
            ATNLAM(IDTMP(1,2),IDTMP(2,2),4) = A4T
            ATNLAM(IDTMP(1,2),IDTMP(2,2),5) = A5T
            ATNLAM(IDTMP(1,2),IDTMP(2,2),6) = A6T
          ELSEIF( IDTMP(1,1).EQ.1.AND.IDTMP(2,1).EQ.2 )THEN
            CLAMB(IDTMP(1,2),IDTMP(2,2)) = TLAMBDA
            ATCLAM(IDTMP(1,2),IDTMP(2,2),1) = A1T
            ATCLAM(IDTMP(1,2),IDTMP(2,2),2) = A2T
            ATCLAM(IDTMP(1,2),IDTMP(2,2),3) = A3T
            ATCLAM(IDTMP(1,2),IDTMP(2,2),4) = A4T
            ATCLAM(IDTMP(1,2),IDTMP(2,2),5) = A5T
            ATCLAM(IDTMP(1,2),IDTMP(2,2),6) = A6T
          ELSEIF( IDTMP(1,1).EQ.1.AND.idtmp(2,1).EQ.3 )THEN
            ALAMB(IDTMP(1,2),IDTMP(2,2)) = TLAMBDA
            ATALAM(IDTMP(1,2),IDTMP(2,2),1) = A1T
            ATALAM(IDTMP(1,2),IDTMP(2,2),2) = A2T
            ATALAM(IDTMP(1,2),IDTMP(2,2),3) = A3T
            ATALAM(IDTMP(1,2),IDTMP(2,2),4) = A4T
            ATALAM(IDTMP(1,2),IDTMP(2,2),5) = A5T
            ATALAM(IDTMP(1,2),IDTMP(2,2),6) = A6T
          END IF
        ELSE
          NT=(IDTMP(1,2)-1)*NCC_PZ+IDTMP(2,2)
          HOLAMB(NT,IDTMP(3,2)) = TLAMBDA
          ATHLAM(NT,IDTMP(3,2),1) = A1T
          ATHLAM(NT,IDTMP(3,2),2) = A2T
          ATHLAM(NT,IDTMP(3,2),3) = A3T
          ATHLAM(NT,IDTMP(3,2),4) = A4T
          ATHLAM(NT,IDTMP(3,2),5) = A5T
          ATHLAM(NT,IDTMP(3,2),6) = A6T
        END IF
        GO TO 210
      END IF

!
!--- Now read in theta's and psi's
!--- First cation-cation terms
!

      IF( NCC_PZ.LT.2 ) GOTO 360
      NT=0
      DO I=1,NCC_PZ
        DO J=I+1,NCC_PZ
          NT=NT+1
 310      CONTINUE
          READ(LUN2,5010) ID1,ID2,ID3A,INDXT,A1T,A2T,A3T,A4T,A5T,A6T,
     &                   TTMIN,TTMAX
5010      FORMAT(I6,1x,I6,1X,I6,1X,I2,6f15.7,F7.3,1X,F7.3)
          IF( ID1.EQ.0 ) GOTO 330
          IF( IDD_PZ(JPC(I)).EQ.ID1.OR.IDD_PZ(JPC(J)).EQ.ID1 )THEN
            IF( IDD_PZ(JPC(I)).EQ.ID2.OR.IDD_PZ(JPC(J)).EQ.ID2 )THEN
              IF( ID3A.EQ.0 )THEN
                IF( INDXT.EQ.1.AND.TEMPC.GT.TTMAX ) GOTO 310
                IF( INDXT.EQ.2.AND.TEMPC.LT.TTMIN ) GOTO 310
                TCC(NT)=A1T+A2T*TK+A3T/TK+A4T*DLOG(TK)+A5T/(TK-263.0)
     &                  +A6T*TK*TK
                ATTC(NT,1) = A1T
                ATTC(NT,2) = A2T
                ATTC(NT,3) = A3T
                ATTC(NT,4) = A4T
                ATTC(NT,5) = A5T
                ATTC(NT,6) = A6T
                IF( TEMPC.GT.TTMAX )THEN
                  WRITE(IWR,5017)
                  WRITE(IWR,5027)TEMPC,TTMAX,SPNML(JPC(I)),SPNML(JPC(J))
                ENDIF
                IF( TEMPC.LT.TTMIN )THEN
                  WRITE(IWR,5017)
                  WRITE(IWR,5028)TEMPC,TTMIN,SPNML(JPC(I)),SPNML(JPC(J))
                ENDIF
5027            FORMAT('Input temperature',1X,F7.3,1X,'exceeds max 
     & database temp (',F7.3,' ) for the ternary electrolyte parameters
     & (theta-c) of', 1X,A10,'and',1X,A10)
5028            FORMAT('Input temperature',1X,F7.3,1X,'is less than min
     & database temp (',F7.3,' ) for the ternary electrolyte parameters
     & (theta-c) of',  1X,A10,'and',1X,A10)
              ELSE
                DO N=1,NA_PZ
                  IF( IDD_PZ(JPA(N)).EQ.ID3A )THEN
                    IF(INDXT.EQ.1.AND.TEMPC.GT.TTMAX ) GO TO 310
                    IF(INDXT.EQ.2.AND.TEMPC.LT.TTMIN ) GO TO 310
                    PSIC(NT,N)=A1T+A2T*TK+A3T/TK+A4T*DLOG(TK)+
     &                A5T/(TK-263.0)+A6T*TK*TK
                    ATPC(NT,N,1) = A1T
                    ATPC(NT,N,2) = A2T
                    ATPC(NT,N,3) = A3T
                    ATPC(NT,N,4) = A4T
                    ATPC(NT,N,5) = A5T
                    ATPC(NT,N,6) = A6T
                    IF( TEMPC.GT.TTMAX )THEN
                      WRITE(IWR,5017)
                      WRITE(IWR,5031)TEMPC,TTMAX,SPNML(JPC(I)),
     &                             SPNML(JPC(J)),SPNML(JPA(N))
                    ENDIF
                    IF( TEMPC.LT.TTMIN )THEN
                      WRITE(IWR,5017)
                      WRITE(IWR,5032)TEMPC,TTMIN,SPNML(JPC(I)),
     &                               SPNML(JPC(J)),SPNML(JPA(N))
                    ENDIF
5031            FORMAT('Input temperature',1X,F7.3,1X,'exceeds max
     & database temp (',F7.3,' ) for the ternary electrolyte parameters
     & (psi-c) of', 1X,A10,'and',1X,A10,1X, 'and',1X,A10)
5032                FORMAT('Input temperature',1X,F7.3,1X,'is less than
     & min database temp (',F7.3,' ) for the ternary electrolyte 
     & parameters (psi-c) of',  1X,A10,'and',1X,A10,1X,'and',1X,A10)
                    GO TO 310
                  ENDIF
                ENDDO
              ENDIF
              GO TO 310
            ENDIF
          ENDIF
          GO TO 310
330       REWIND(LUN2)
        ENDDO
      ENDDO

360   CONTINUE


!
!--- Now anion-anion terms
!

      IF( NA_PZ.LT. 2 )GO TO 460
      REWIND(lun2)
      NT = 0

      DO I=1,NA_PZ
        DO J=I+1,NA_PZ
          NT = NT+1
410       read(LUN2,5010) ID1,ID2,ID3A,INDXT,A1T,A2T,A3T,A4T,A5T,A6T,
     &                     TTMIN,TTMAX
          IF( ID1.EQ.0 )GO TO 430
          IF(IDD_PZ(JPA(I)).EQ.ID1.OR.IDD_PZ(JPA(J)).EQ.ID1 )THEN
            IF( IDD_PZ(JPA(I)).EQ.ID2.OR.IDD_PZ(JPA(J)).EQ.ID2 )THEN
              IF( ID3A.EQ. 0 )THEN
                IF( INDXT.EQ.1.AND.TEMPC.GT.TTMAX ) GO TO 410
                IF( INDXT.EQ.2.AND.TEMPC.LT.TTMIN ) GO TO 410
                TAA(NT)= A1T+A2T*TK+A3T/TK+A4T*DLOG(TK)+A5T/(TK-263.0)
     &                  +A6T*TK*TK
                ATTA(NT,1) = A1T
                ATTA(NT,2) = A2T
                ATTA(NT,3) = A3T
                ATTA(NT,4) = A4T
                ATTA(NT,5) = A5T
                ATTA(NT,6) = A6T
                IF( TEMPC.GT.TTMAX )THEN
                  WRITE(IWR,5017)
                  WRITE(IWR,5033)TEMPC,TTMAX,SPNML(JPA(I)),SPNML(JPA(J))
                ENDIF
                IF( TEMPC.LT.TTMIN )THEN
                  WRITE(IWR,5017)
                  WRITE(IWR,5034)TEMPC,TTMIN,SPNML(JPA(I)),SPNML(JPA(J))
                ENDIF
5033            FORMAT('Input temperature',1X,F7.3,1X,'exceeds max 
     & database temp (',F7.3,' ) for the ternary electrolyte parameters
     & (theta-a) of', 1X,A10,'and',1X,A10)
5034            FORMAT('Input temperature',1X,F7.3,1X,'is less than min
     & database temp (',F7.3,' ) for the ternary electrolyte parameters
     & (theta-a) of',  1X,A10,'and',1X,A10)
              ELSE
                DO N=1,NCC_PZ
                  IF( IDD_PZ(JPC(N)).EQ.ID3A )THEN
                    IF( INDXT.EQ.1.AND.TEMPC.GT.TTMAX )GO TO 410
                    IF( INDXT.EQ.2.AND.TEMPC.LT.TTMIN )GO TO 410
                    PSIA(NT,N) = A1T+A2T*TK+A3T/TK+A4T*DLOG(TK)+A5T/
     &                           (TK-263.0)+A6T*TK*TK
                    ATPA(NT,N,1) = A1T
                    ATPA(NT,N,2) = A2T
                    ATPA(NT,N,3) = A3T
                    ATPA(NT,N,4) = A4T
                    ATPA(NT,N,5) = A5T
                    ATPA(NT,N,6) = A6T
                    IF( TEMPC.GT.TTMAX )THEN
                      WRITE(IWR,5017)
                      WRITE(IWR,5035)TEMPC,TTMAX,SPNML(JPA(I)),
     &                             SPNML(JPA(J)),SPNML(JPC(N))
                    ENDIF
                    IF( TEMPC.LT.TTMIN )THEN
                      WRITE(IWR,5017)
                      WRITE(IWR,5036)TEMPC,TTMIN,SPNML(JPA(I)),
     &                             SPNML(JPA(J)),SPNML(JPC(N))
                    ENDIF
5035                FORMAT('Input temperature',1X,F7.3,1X,'exceeds max
     & database temp (',F7.3,' ) for the ternary electrolyte parameters
     & (psi-a) of',  1X,A10,'and',1X,A10,1X,'and',1X,A10)
5036                FORMAT('Input temperature',1X,F7.3,1x,'is less than 
     & min database temp (',F7.3,' ) for the ternary electrolyte 
     & parameters (psi-a) of',  1X,A10,'and',1X,A10,1X,'and',1X,A10)
                    GO TO 410
                  END IF
                END DO
              END IF
              GO TO 410
            END IF
          END IF
          GO TO 410
 430      REWIND(lun2)
        END DO
      END DO

460   CONTINUE

!
!-- Now output the pitzer parameters
!
      WRITE(IWR,5020)
5020  FORMAT(/'       non-ideal electrolyte parameters')
      WRITE(IWR,5030)
5030  FORMAT(/'       single electrolyte parameters'/)

      DO I=1,NCC_PZ
        DO J=1,NA_PZ
          WRITE(IWR,5050) SPNML(JPC(I)),SPNML(JPA(J)),B0(I,J),B1(I,J),
     &                  B2(I,J),CMXX(I,J)
          TMP=SP_L(1,JPC(I))*SP_L(1,JPA(J))
          TMP=DABS(TMP)
          CMXX(I,J)=CMXX(I,J)/(2.0D0*DSQRT(TMP))
        END DO
      END DO
5050  FORMAT(2A20,4(1X,F10.5))

!
!--- Now write out the theta's and psi's
!
      IPRTI=8
      WRITE(IWR,5070)
5070  FORMAT(/'       ternary electrolyte parameters')
!
!--- First cation-cation-anion
!
      IF(NCC_PZ.LT.2) GO TO 620
      M = 1
      IPRT1 = 1
615   IPRT2 = M*IPRTI
      IF( NA_PZ.LE.IPRT2 )IPRT2 = NA_PZ
      WRITE(IWR,5080) (SPNML(JPA(K)),K=IPRT1,IPRT2)
5080  FORMAT(/64X,20A20)
      
      NT = 0
      DO i=1,ncc
        DO j=i+1,ncc
         nt=nt+1
         WRITE(IWR,5060) SPNML(JPC(I)),SPNML(JPC(J)),TCC(NT),
     &     (PSIC(NT,K),K=IPRT1,IPRT2)
        END DO
      END DO
5060  FORMAT(2A20,1X,F10.5,10X,20(F10.5,10X))

      M = M + 1
      IPRT1 = IPRT2 + 1
      IF( IPRT2.LT.NA_PZ )GO TO 615

620   CONTINUE

!
!--- Now anion-anion-cation
!
      IF( NA_PZ.LT.2 )GO TO 720

      M = 1
      IPRT1=1
715   IPRT2 = M*IPRTI

      IF( NCC_PZ.LE.IPRT2 )IPRT2 = NCC_PZ
      WRITE(IWR,5081) (SPNML(JPC(K)),K=IPRT1,IPRT2)
5081  FORMAT(/64X,20A20)
      NT = 0
      DO I=1,NA_PZ
        DO J=I+1,NA_PZ
          NT = NT+1
          WRITE(IWR,5060) SPNML(jpa(i)),SPNML(jpa(j)),TAA(NT),
     &                   (PSIA(NT,K),K=IPRT1,IPRT2)
        END DO
      END DO

      M = M+1
      IPRT1 = IPRT2+1
      IF( IPRT2.lt.NCC_PZ ) GO TO 715

720   CONTINUE

      IF( NNN_PZ.GT.0 )THEN
  
        WRITE(IWR,5090)
5090    FORMAT(/'       neutral ion parameters')
        WRITE(IWR,5095) (SPNML(JPN(K)),K=1,NNN_PZ)
5095    FORMAT(24X,10A20)
        DO I=1,NCC_PZ
          WRITE(IWR,5100) SPNML(JPC(I)),(CLAMB(J,I),J=1,NNN_PZ)
        END DO
  
        DO I=1,NA_PZ
          WRITE(IWR,5100) SPNML(JPA(I)),(ALAMB(J,I),J=1,NNN_PZ)
        END DO
  
        DO I=1,NNN_PZ
          WRITE(IWR,5100) SPNML(JPN(I)),(ELAMB(J,I),J=1,NNN_PZ)
        END DO
5100    FORMAT(A20,1X,10(F10.5,10X))
  
!
!--- Now higher order lambdas
!
        WRITE(IWR,5110)
5110    FORMAT(/'        higher order lambdas')
        M = 1
        IPRT1 = 1
  815   IPRT2 = M*IPRTI
  
        IF( NA_PZ.LE.IPRT2) IPRT2 = NA_PZ
        WRITE(IWR,5096) (SPNML(JPA(K)),K=IPRT1,IPRT2)
5096    FORMAT(/44X,10A20)
        NT = 0
  
        DO I=1,NNN_PZ
          DO J=1,NCC_PZ
            NT=NT+1
            WRITE(IWR,5065) SPNML(JPN(I)),SPNML(JPC(J)),
     &                    (HOLAMB(NT,K),K=IPRT1,IPRT2)
5065  FORMAT(2A20,1X,F10.5,20(10X,F10.5))
          END DO
        END DO
  
        M = M+1
        IPRT1 = IPRT2+1
  
        IF(IPRT2.LT.NA_PZ) go to 815
  
      END IF


      CLOSE(LUN1)
      CLOSE(LUN2)
      CLOSE(LUN3)

!
!---  End of PTZRP group ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RATER( NEQX,M,NS,N,IRCX,NSPKX,NSPRX,NSPPX,SP_CX,ACTVX,
     &      VTOMX,VTOLX,VTOSX,EQKX,RRCX,RSBX )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     REACTION RATE.
!
!     YSPLX aqueous fraction of component species NEQ at node N
!     YSPGX gas fraction of component species NEQ at node N
!     YSPNX NAPL fraction of component species NEQ at node N
!     C(N,NSL) component species concentration (kmol/m^3 node)
!     SP_C(N,NSP) species concentration (kmol/m^3 node)
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, August 15, 2005.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE PORMED
      USE GRID
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 ACTVX(LSPL+1,LSPL), SP_CX(LSPR)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RATER'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---    TST kinetic reaction  ---
!
        IF( (IRCKT(IRCX).GE.10 .AND. IRCKT(IRCX).LE.12) .OR.
     &    (IRCKT(IRCX).GE.5 .AND. IRCKT(IRCX).LE.9) ) THEN
!
!---      Ion activity product mol/kg water, loop over species in
!         kinetic reaction  ---
!
          QX = 1.D+0
          DO 120 L = 1,NSPKX
            NSPX = IRC_K(L+2,IRCX)
!
!---        Aqueous species,
!           concentration in molality, mol/kg H2O  ---
!
            IF( NSPX.LE.NSPL ) THEN
              CMX = SP_CX(NSPX)*VTOMX
              ACX = ACTVX(1,NSPX)
!
!---          Reactants  ---
!
              N1 = MAX(1,N*IRCKN(L))
              IF( L.LE.NSPRX ) THEN
                QX = QX*((CMX*ACX)**RC_K(L,N1,IRCX))
!
!---          Products  ---
!
              ELSE
                QX = QX/((CMX*ACX)**RC_K(L,N1,IRCX))
              ENDIF
!
!---        Solid species, skip  ---
!
            ELSEIF( NSPX.LE.NSPL+NSPS ) THEN
              GOTO 120
            ENDIF
  120     CONTINUE
!
!---      Initial reactive surface area, initial mineral volume 
!         fraction, current mineral volume fraction, minimum current 
!         mineral volumefraction allows re-precipitation of dissolved 
!         primary minerals NSP_M - mineral species number  ---
!
          NSPX = IRC_K(3+NSPKX,IRCX)
          NSP_M =  NSPX - NSPL
!
!---      Primary mineral  ---
!
          IF( RS_S(2,NSP_M,N).GT.EPSL ) THEN
            AOX = RS_S(1,NSP_M,N)*VOL(N)*RS_S(2,NSP_M,N)*SP_S(1,NSP_M)
            VFMOX = RS_S(2,NSP_M,N)
            IF( ISP_MN(NSPX).EQ.1 ) THEN
              VFMX = 1.D-3*(SP_CMN(N,NSP_M)+SP_CX(NSPX))
     &          *SP_S(2,NSP_M)/SP_S(1,NSP_M)
            ELSE
              VFMX = 1.D-3*SP_CX(NSPX)*SP_S(2,NSP_M)/SP_S(1,NSP_M)
            ENDIF
            VFMX = MAX( VFMX,1.D-2 )
!
!---      Secondary mineral, initial reactive surface area
!         for seconary minerals is set to 0.25 m^2/dm^3  ---
!
          ELSE
            AOX = 0.25D+3*VOL(N)
            VFMOX = 1.D-2
            VFMX = 1.D-3*SP_CX(NSPX)*SP_S(2,NSP_M)/SP_S(1,NSP_M)
            VFMX = MAX( VFMX,1.D-2 )
          ENDIF
          VFMX = MAX( VFMX,0.D+0 )
!
!---      Reactive surface area  ---
!
          AX = AOX*(((POR0(2,N)*VFMX)/
     &      (POR(2,IZ(N))*VFMOX))**(2.D+0/3.D+0))
          IF( ISLC(56).EQ.2 ) AX = AOX
!
!---      Reaction rate, mol/s  ---
!
          RRBX = -AX*RRCX*(1.D+0-(QX/EQKX))
!
!---      pH dependence  ---
!
          IF( IRCKT(IRCX).GE.5 .AND. IRCKT(IRCX).LE.9
     &      .AND. ISPLK(1).NE.0 ) THEN
            NSP_PHX = MOD(ISPLK(1),1000)
            PHX = -LOG10(1.D-3*SP_CX(NSP_PHX)*VTOLX)
            IF( IRCKT(IRCX).GE.8 .AND. IRCKT(IRCX).LE.9 ) THEN
              RRBX = RRBX*MAX( 0.D+0,
     &          (7.9201D-1 - 1.3479D-1*PHX + 5.2D-3*(PHX**2)))
            ELSE
              N9 = MAX(1,N*IRCKN(NSPKX+9))
              RRBX = RRBX*(1.D+1**(-RC_K(NSPKX+9,N9,IRCX)*PHX))
            ENDIF
          ENDIF
!
!---      Reaction rate, mol/m^3 aqu s  ---
!
          RRBX = RRBX*VTOLX/VOL(N)
!
!---      Direction limited  ---
!
          IF( IRCKT(IRCX).EQ.6 .OR. IRCKT(IRCX).EQ.8
     &      .OR. IRCKT(IRCX).EQ.11 ) THEN
            RRBX = MAX( RRBX,0.D+0 )
          ELSEIF( IRCKT(IRCX).EQ.7 .OR. IRCKT(IRCX).EQ.9
     &      .OR. IRCKT(IRCX).EQ.12 ) THEN
            RRBX = MIN( RRBX,0.D+0 )
          ENDIF
!
!---    Forward-backward kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.1 ) THEN
          N1 = MAX(1,N*IRCKN(NSPKX+1))
          N2 = MAX(1,N*IRCKN(NSPKX+2))
          FRRX = RC_K(NSPKX+1,N1,IRCX)
          BRRX = RC_K(NSPKX+2,N2,IRCX)
!
!---      Loop over reactants  ---
!
          DO 140 L = 1,NSPRX
!
!---        Global species index  ---
!
            NSPX = IRC_K(L+2,IRCX)
            CMX = SP_CX(NSPX)*VTOLX
            N1 = MAX(1,N*IRCKN(L))
            FRRX = FRRX*(CMX**RC_K(L,N1,IRCX))
  140     CONTINUE
!
!---      Loop over products  ---
!
          DO 160 L = 1,NSPPX
!
!---        Global species index  ---
!
            NSPX = IRC_K(L+2+NSPRX,IRCX)
            CMX = SP_CX(NSPX)*VTOLX
            N1 = MAX(1,N*IRCKN(L+NSPRX))
            BRRX = BRRX*(CMX**RC_K(L+NSPRX,N1,IRCX))
  160     CONTINUE
          RRBX = FRRX - BRRX
!
!---    Valocchi-Monod kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.2 ) THEN
!
!---      Concentration of biomass in mol/m^3 aqu  ---
!
          NSPX = IRC_K(5,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
!
!---      Partial rate of donor degredation  ---
!
          N6 = MAX(1,N*IRCKN(6))
          RRBX = RC_K(6,N6,IRCX)*CMX
!
!---      Concentration of donor in mol/kg water  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Partial rate of donor degredation  ---
!
          N4 = MAX(1,N*IRCKN(4))
          RRBX = RRBX*(CMX/(RC_K(4,N4,IRCX)+CMX))
!
!---      Concentration of acceptor in mol/kg water  ---
!
          NSPX = IRC_K(4,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Rate of donor degredation  ---
!
          N5 = MAX(1,N*IRCKN(5))
          RRBX = RRBX*(CMX/(RC_K(5,N5,IRCX)+CMX))
!
!---    Valocchi-Sorption kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.3 ) THEN
!
!---      Concentration of sorbed species in mol/gm soil  ---
!
          NSPX = IRC_K(4,IRCX)
          CMX = SP_CX(NSPX)*VTOSX*1.D-3
          N4 = MAX(1,N*IRCKN(4))
          RRBX = CMX/RC_K(4,N4,IRCX)
!
!---      Concentration of aqueous species in mol/kg water  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Rate of sorption in mol/m^3 aqu s  ---
!
          N3 = MAX(1,N*IRCKN(3))
          RRBX = RC_K(3,N3,IRCX)*(CMX-RRBX)*1.D+3
!
!---    Valocchi-Biomass kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.4 ) THEN
!
!---      Concentration of biomass in mol/m^3 aqu  ---
!
          NSPX = IRC_K(5,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
!
!---      Partial rate of donor degredation  ---
!
          N6 = MAX(1,N*IRCKN(6))
          RRBX = RC_K(6,N6,IRCX)*CMX
!
!---      Concentration of donor in mol/m^3 aqu  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Partial rate of donor degredation  ---
!
          N4 = MAX(1,N*IRCKN(4))
          RRBX = RRBX*(CMX/(RC_K(4,N4,IRCX)+CMX))
!
!---      Concentration of acceptor in mol/m^3 aqu  ---
!
          NSPX = IRC_K(4,IRCX)
          CMX = SP_CX(NSPX)*VTOMX
!
!---      Rate of donor degredation  ---
!
          N5 = MAX(1,N*IRCKN(5))
          RRBX = RRBX*(CMX/(RC_K(5,N5,IRCX)+CMX))
!
!---      Concentration of biomass in mol/m^3 aqu  ---
!
          NSPX = IRC_K(5,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
!
!---      Rate of biomass production in mol/m^3 aqu s  ---
!
          N7 = MAX(1,N*IRCKN(7))
          N8 = MAX(1,N*IRCKN(8))
          RRBX = RC_K(7,N7,IRCX)*RRBX - RC_K(8,N8,IRCX)*CMX
!
!---    Liu's multi-rate kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.41 ) THEN
          N1 = MAX(1,N*IRCKN(NSPKX+1))
          RMX = RC_K(NSPKX+1,N1,IRCX)
!
!---      Loop over reactants  ---
!
          N2 = MAX(1,N*IRCKN(NSPKX+2))
          N3 = MAX(1,N*IRCKN(NSPKX+3))
          N4 = MAX(1,N*IRCKN(NSPKX+4))
          N5 = MAX(1,N*IRCKN(NSPKX+5))
          SDENX = RC_K(NSPKX+2,N2,IRCX)*VTOLX/VTOSX
          PFRCX = RC_K(NSPKX+3,N3,IRCX)
          XLGK1 = RC_K(NSPKX+4,N4,IRCX)
          XLGK2 = RC_K(NSPKX+5,N5,IRCX)
          FRRX = 1.D0
          BRRX = 1.D0
          DO L = 1,NSPRX
!
!---        Global species index  ---
!
            NSPX = IRC_K(L+2,IRCX)
            CMX = SP_CX(NSPX)*VTOMX
            IF(NSPX.LE.NSPL) THEN
              ACX = ACTVX(1,NSPX)
            ELSE
              ACX =1.D0
            ENDIF
            N1 = MAX(1,N*IRCKN(L))
            FRRX = FRRX*((CMX*ACX)**RC_K(L,N1,IRCX))
          ENDDO
!
!---      Loop over products  ---
!
          DO L = 1,NSPPX-1
!
!---        Global species index  ---
!
            NSPX = IRC_K(L+2+NSPRX,IRCX)
            CMX = SP_CX(NSPX)*VTOMX
            IF(NSPX.LE.NSPL) THEN
              ACX = ACTVX(1,NSPX)
            ELSE
              ACX = 1.D0
            ENDIF
            N1 = MAX(1,N*IRCKN(L+NSPRX))
            BRRX = BRRX*((CMX*ACX)**RC_K(L+NSPRX,N1,IRCX))
          ENDDO
          L=NSPPX
          NSPX = IRC_K(L+2+NSPRX,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
          FRRX = FRRX*10**XLGK1
          BRRX = BRRX*10**XLGK2
          RRBX = RMX*(SDENX*PFRCX*FRRX/(1.D0+FRRX+BRRX)-CMX)
!
!---    Liu's dual domain kinetic reaction  ---
!
        ELSEIF( IRCKT(IRCX).EQ.42 ) THEN
          N1 = MAX(1,N*IRCKN(NSPKX+1))
          RMX = RC_K(NSPKX+1,N1,IRCX)
!
!---      Loop over reactants  ---
!
          N3 = MAX(1,N*IRCKN(NSPKX+3))
          PFRCX = RC_K(NSPKX+3,N3,IRCX)
          FRRX = 0.D0
!
!---        Global species index  ---
!
          NSPX = IRC_K(3,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
          FRRX = FRRX+CMX**VTOLX
          NSPX = IRC_K(4,IRCX)
          CMX = SP_CX(NSPX)*VTOLX
          FRRX = FRRX-CMX**VTOLX
          RRBX = RMX*FRRX
!
!---    Multirate  ---
!
        ELSEIF( IRCKT(IRCX).EQ.20 ) THEN
!
!---      Neutral reaction rate, mol/m^2 s  ---
!
          TKX = T(2,N)+TABS
          N1 = MAX(1,N*IRCKN(1))
          N2 = MAX(1,N*IRCKN(2))
          N3 = MAX(1,N*IRCKN(3))
          TKRX = RC_K(3,N3,IRCX)+TABS
          RRCX = RC_K(1,N1,IRCX)*EXP( -RC_K(2,N2,IRCX)*
     &      ((1.D+0/TKX)-(1.D+0/TKRX))/(1.D-3*RCU) )
!
!---      Loop over mechanisms  ---
!
          DO 190 NKRMX = 1,IRC_K(2,IRCX)
            IX = 3+((NKRMX-1)*6)
!
!---        Ion activity product mol/kg water, loop over species  ---
!
            QX = 1.D+0
            DO 180 L = 1,IRC_K(IX,IRCX)
              IX = 3+((NKRMX-1)*6)+L
              NSPX = IRC_K(IX,IRCX)
!
!---          Aqueous species,
!             concentration in molality, mol/kg H2O  ---
!
              IF( NSPX.LE.NSPL ) THEN
                CMX = SP_CX(NSPX)*VTOMX
                ACX = ACTVX(1,NSPX)
                IX = 6+((NKRMX-1)*8)+L
                NX = MAX(1,N*IRCKN(IX))
                QX = QX*((CMX*ACX)**RC_K(IX,NX,IRCX))
              ENDIF
  180       CONTINUE
            IX = 4+((NKRMX-1)*8)
            NX = MAX(1,N*IRCKN(IX))
            N1 = MAX(1,N*IRCKN(IX+1))
            N2 = MAX(1,N*IRCKN(IX+2))
            TKRX = RC_K(IX+2,N2,IRCX)+TABS
            RRCX = RRCX + RC_K(IX,NX,IRCX)*EXP( -RC_K(IX+1,N1,IRCX)*
     &      ((1.D+0/TKX)-(1.D+0/TKRX))/(1.D-3*RCU) )*QX
  190     CONTINUE
!
!---      Initial reactive surface area, initial mineral volume 
!         fraction,current mineral volume fraction, minimum current 
!         mineral volumefraction allows re-precipitation of dissolved 
!         primary minerals NSP_M - mineral species number  ---
!
          NSPX = IRC_K(1,IRCX)
          NSP_M =  NSPX - NSPL
!
!---      Primary mineral  ---
!
          IF( RS_S(2,NSP_M,N).GT.EPSL ) THEN
            AOX = RS_S(1,NSP_M,N)*VOL(N)*RS_S(2,NSP_M,N)*SP_S(1,NSP_M)
            VFMOX = RS_S(2,NSP_M,N)
            IF( ISP_MN(NSPX).EQ.1 ) THEN
              VFMX = 1.D-3*(SP_CMN(N,NSP_M)+SP_CX(NSPX))
     &          *SP_S(2,NSP_M)/SP_S(1,NSP_M)
            ELSE
              VFMX = 1.D-3*SP_CX(NSPX)*SP_S(2,NSP_M)/SP_S(1,NSP_M)
            ENDIF
            VFMX = MAX( VFMX,1.D-2 )
!
!---      Secondary mineral, initial reactive surface area
!         for seconary minerals is set to 0.25 m^2/dm^3  ---
!
          ELSE
            AOX = 0.25D+3*VOL(N)
            VFMOX = 1.D-2
            VFMX = 1.D-3*SP_CX(NSPX)*SP_S(2,NSP_M)/SP_S(1,NSP_M)
            VFMX = MAX( VFMX,1.D-2 )
          ENDIF
          VFMX = MAX( VFMX,0.D+0 )
!
!---      Reactive surface area  ---
!
          AX = AOX*(((POR0(2,N)*VFMX)/
     &      (POR(2,IZ(N))*VFMOX))**(2.D+0/3.D+0))
          IF( ISLC(56).EQ.2 ) AX = AOX
!
!---      Reaction rate, mol/s  ---
!
          RRBX = -AX*RRCX
!
!---      Reaction rate, mol/m^3 aqu s  ---
!
          RRBX = RRBX*VTOLX/VOL(N)
        ENDIF
!
!Liu's dual domain model
        IF( IRCKT(IRCX).EQ.42 ) THEN
          EQ_KX = EQ_K(NS+M,NEQX)
          IF( IMMB(NEQC+NEQX) == 1 ) THEN
! RC_K(2,IRCX) is pore fraction of immobile domain
            N2 = MAX(1,N*IRCKN(NSPKX+2))
            EQ_KX = EQ_K(NS+M,NEQX)*RC_K(NSPKX+2,N2,IRCX)
          ENDIF
          RSBX = RSBX + RRBX*EQ_KX
        ELSE
          RSBX = RSBX + RRBX*EQ_K(NS+M,NEQX)
        ENDIF
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RATER group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDAQSP
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
!     Read aqueous species.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 December 2004.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FILES
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
      CHARACTER*64 ADUM,RDUM,UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDAQSP'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Write card information to ouput file  ---
!
      CARD = 'Aqueous Species Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of aqueous species  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Aqueous Species'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NSPL )
      IF( NSPL.GT.LSPL ) THEN
        INDX = 5
        CHMSG = 'Number of Aqueous Species > Parameter LSPL'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Check for molecular diffusion option  ---
!
      VARB = 'Aqueous Species Molecular Diffusion Option'
      IVR = INDEX( VARB,'  ')-1
      CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
      IF( INDX.EQ.0 ) THEN
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'conventional').NE.0 ) THEN
          ISP_IEDL = 1
          WRITE(IWR,'(2X,3A)') VARB(1:IVR),': ','Conventional'
        ELSEIF( INDEX(ADUM(1:),'empirical').NE.0 ) THEN
          ISP_IEDL = 2
          WRITE(IWR,'(2X,3A)') VARB(1:IVR),': ','Power Function'
        ELSEIF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
          ISP_IEDL = 3
          WRITE(IWR,'(2X,3A)') VARB(1:IVR),': ','Constant'
        ELSEIF( INDEX(ADUM(1:),'power').NE.0 ) THEN
          ISP_IEDL = 4
          WRITE(IWR,'(2X,3A)') VARB(1:IVR),': ','Power Function'
        ENDIF
      ELSE
        ISP_IEDL = 1
        WRITE(IWR,'(2X,3A)') VARB(1:IVR),': ','Conventional'
      ENDIF
!
!---  Read aqueous molecular diffusion coefficient  ---
!
      VARB = 'Aqueous Species Molecular Diffusion Coefficient'
      IVR = INDEX( VARB,'  ')-1
      CALL RDDPR(ISTART,ICOMMA,CHDUM,SP_MDL)
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      WRITE(IWR,'(2X,4A,1PE11.4)') VARB(1:IVR),', ',
     &  UNTS(1:NCH),': ',SP_MDL
      INDX = 0
      IUNM = 2
      IUNS = -1
      CALL RDUNIT(UNTS,SP_MDL,INDX)
!
!---  Power Function or van Schaik and Kemper Empirical 
!     Aqueous Diffusion Models  ---
!
      IF( ISP_IEDL.EQ.2 .OR. ISP_IEDL.EQ.4 ) THEN
        SP_SDCL(1) = SP_MDL
        VARB = 'a Constant'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SP_SDCL(2))
        WRITE(IWR,'(4x,2A,1PE11.4)') VARB(1:IVR),': ',SP_SDCL(2)
        VARB = 'b Constant'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SP_SDCL(3))
        WRITE(IWR,'(4x,2A,1PE11.4)') VARB(1:IVR),': ',SP_SDCL(3)
      ENDIF
!
!---  Check for activity coefficient option  ---
!
      CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
      IF( INDX.EQ.1 ) THEN
        VARB = 'Activity Coefficient Option: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'davies').NE.0 ) THEN
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),'Davies Equation'
          IACTV = 1
        ELSEIF( INDEX(ADUM(1:),'pitzer').NE.0 ) THEN
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),'Pitzer Equation'
          IACTV = 2
        ELSEIF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),'Constant'
          IACTV = 3
          VARB = 'Constant Activity Coefficient'
          IDFLT = 1
          ACTVC = 1.D+0
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ACTVC)
          WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &        ': ',ACTVC
        ELSE
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),'B-Dot Equation'
        ENDIF
      ELSE
        WRITE(IWR,'(2X,2A)') VARB(1:IVR),'B-Dot Equation'
      ENDIF
!
!---  Loop over the aqueous species  ---
!
      DO 500 NSP = 1,NSPL
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Aqueous Species Name: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,SPNML(NSP))
        DO 100 M = 1,NSP-1
          IF( SPNML(M).EQ.SPNML(NSP) ) THEN
            INDX = 4
            CHMSG = 'Duplicate Aqueous Species Name: ' // SPNML(NSP)
            CALL WRMSGS( INDX )
          ENDIF
  100   CONTINUE
        WRITE (IWR,'(/,2A)') ' Aqueous Species Name: ',SPNML(NSP)
!
!---    Skip for constant activity coefficent  ---
!
        IF( IACTV.EQ.3 ) GOTO 500
!
!---    Read aqueous species charge  ---
!
        VARB = 'Charge'
        IUNM = 1
        IDFLT = 1
        SP_L(1,NSP) = 0.D+0
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SP_L(1,NSP))
        WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &      ': ',SP_L(1,NSP)
!
!---    Read aqueous species diameter  ---
!
        VARB = 'Diameter'
        SP_L(2,NSP) = 3.D-10
        IDFLT = 1
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SP_L(2,NSP))
        UNTS = 'm'
        IUNM = 1
        IDFLT = 1
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',SP_L(2,NSP)
        INDX = 0
        CALL RDUNIT(UNTS,SP_L(2,NSP),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',SP_L(2,NSP),', m)'
!
!---    Read aqueous species molecular weight  ---
!
        VARB = 'Molecular Weight'
        UNTS = 'g/mol'
        IUNMOL = -1
        IUNKG = 1
        IDFLT = 1
        SP_L(3,NSP) = 0.D+0
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SP_L(3,NSP))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',SP_L(3,NSP)
        INDX = 0
        CALL RDUNIT(UNTS,SP_L(3,NSP),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',SP_L(3,NSP),', g/mol)'
!
!---  Read next aqueous species  ---
!
  500 CONTINUE
!
!---  Read Pitzer parameters  ---
!
      IF( IACTV.EQ.2 )THEN
        CALL RDPTZR
      ENDIF
      WRITE(IWR,'(A)') ' '
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDAQSP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDCNEQ
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
!     Read conservation equations for reactions.
!     Standard equation form:
!
!     d( Cs(1)*[Sp(1)] + Cs(2)*[Sp(2)] + .... )/dt = 0
!
!     Cs(1) = EQ_C(1,NEQC)
!     Cs(ns) = EQ_C(ns,NEQC)
!     ns = IEQ_C(1)
!     Sp(1) = IEQ_C(2)
!     Sp(ns) = IEQ_C(ns+1)
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 10 December 2004.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE FILES
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
      CHARACTER*64 ADUM,BDUM
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDCNEQ'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Write card information to ouput file  ---
!
      CARD = 'Conservation Equations Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of conservation equations  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Conservation Equations'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NEQC )
      IF( NEQC.GT.LEQC ) THEN
        INDX = 5
        CHMSG = 'Number of Conservation Equations > Parameter LEQC'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Loop over the conservation equations  ---
!
      NSPC = NEQC
      DO 500 NEQ = 1,NEQC
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Conservation Component Species Name'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,SPNMC(NEQ))
!
!---    Check for duplicate conservation component species name  ---
!
        DO 100 M = 1,NEQ-1
          IF( SPNMC(NEQ).EQ.SPNMC(M) ) THEN
            INDX = 4
            CHMSG = 'Duplicate Conservation Component Species Name: '
     &        // SPNMC(NEQ)(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
  100   CONTINUE
!
!---    Load conservation component name into solute name
!       for output  ---
!
        SOLUT(NSOLU+NEQ) = SPNMC(NEQ)(1:NCH)
!
!---    Extract species name from component species name  ---
!
        IF( INDEX(SPNMC(NEQ)(1:),'total').NE.0 ) THEN
          ICH = INDEX(SPNMC(NEQ)(1:),'total') + 6
          BDUM = SPNMC(NEQ)(ICH:)
        ELSE
          BDUM = SPNMC(NEQ)
        ENDIF
        NCHB = INDEX(BDUM(1:),'  ')-1
!
!---    Aqueous species, assign species index  ---
!
        DO 110 M = 1,NSPL
          IF( SPNML(M).EQ.BDUM ) THEN
            IEQ_C(2,NEQ) = M
            GOTO 150
          ENDIF
  110   CONTINUE
!
!---    Solid species, assign species index  ---
!
        DO 120 M = 1,NSPS
          IF( SPNMS(M).EQ.BDUM ) THEN
            IEQ_C(2,NEQ) = NSPL + M
            GOTO 150
          ENDIF
  120   CONTINUE
!
!---    Exchanged species, assign species index  ---
!
        DO 125 M = 1,NSPE
          IF( SPNME(M).EQ.BDUM ) THEN
            IEQ_C(2,NEQ) = NSPL + NSPS + M
            GOTO 150
          ENDIF
  125   CONTINUE
!
!---    Gas species, assign species index  ---
!
        DO 130 M = 1,NSPG
          IF( SPNMG(M).EQ.BDUM ) THEN
            IEQ_C(2,NEQ) = NSPL + NSPS + NSPE + M
            GOTO 150
          ENDIF
  130   CONTINUE
!
!---    NAPL species, assign species index  ---
!
        DO 140 M = 1,NSPN
          IF( SPNMN(M).EQ.BDUM ) THEN
            IEQ_C(2,NEQ) = NSPL + NSPS + NSPE + NSPG + M
            GOTO 150
          ENDIF
  140   CONTINUE
        INDX = 4
        CHMSG = 'Unrecognized Component Species Name: ' //
     &    'Total-' // BDUM(1:NCHB)
        CALL WRMSGS( INDX )
  150   CONTINUE
        WRITE (IWR,'(/,2A)')
     &      'Component Species Name: ','Total-' // BDUM(1:NCHB)
!
!---    Loop over component species  ---
!
        VARB = 'Number of Species in Conservation Equation'
        CALL RDINT( ISTART,ICOMMA,CHDUM,IEQ_C(1,NEQ) )
        IF( IEQ_C(1,NEQ).GT.LSEC ) THEN
          INDX = 5
          CHMSG = 'Number of Species in Conservation Equation > ' //
     &      'Parameter LSEC'
          CALL WRMSGS( INDX )
        ENDIF
        ICSPX = 0
!
!---    Loop over the conservation-equation species  ---
!
        DO 300 NSP = 1,IEQ_C(1,NEQ)
          VARB = 'Conservation-Equation Species Name'
!
!---      Allow for returns in input lines  ---
!
          CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RDINPL( CHDUM )
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          ICX = NSP + 2 - ICSPX
!
!---      Aqueous species, assign species index  ---
!
          DO 210 M = 1,NSPL
            IF( SPNML(M).EQ.ADUM ) THEN
              IF( M.EQ.IEQ_C(2,NEQ) ) THEN
                ICSPX = 1
                ICX = 2
              ELSE
                IEQ_C(ICX,NEQ) = M
              ENDIF
              GOTO 250
            ENDIF
  210     CONTINUE
!
!---      Solid species, assign species index  ---
!
          DO 220 M = 1,NSPS
            IF( SPNMS(M).EQ.ADUM ) THEN
              IF( (M+NSPL).EQ.IEQ_C(2,NEQ) ) THEN
                ICSPX = 1
                ICX = 2
              ELSE
                IEQ_C(ICX,NEQ) = NSPL + M
              ENDIF
              GOTO 250
            ENDIF
  220     CONTINUE
!
!---      Exchanged species, assign species index  ---
!
          DO 225 M = 1,NSPE
            IF( SPNME(M).EQ.ADUM ) THEN
              IF( (M+NSPL+NSPS).EQ.IEQ_C(2,NEQ) ) THEN
                ICSPX = 1
                ICX = 2
              ELSE
                IEQ_C(ICX,NEQ) = NSPL + NSPS + M
              ENDIF
              GOTO 250
            ENDIF
  225     CONTINUE
!
!---      Gas species, assign species index  ---
!
          DO 230 M = 1,NSPG
            IF( SPNMG(M).EQ.ADUM ) THEN
              IF( (M+NSPL+NSPS+NSPE).EQ.IEQ_C(2,NEQ) ) THEN
                ICSPX = 1
                ICX = 2
              ELSE
                IEQ_C(ICX,NEQ) = NSPL + NSPS + NSPE + M
              ENDIF
              GOTO 250
            ENDIF
  230     CONTINUE
!
!---      NAPL species, assign species index  ---
!
          DO 240 M = 1,NSPN
            IF( SPNMN(M).EQ.ADUM ) THEN
              IF( (M+NSPL+NSPS+NSPE+NSPG).EQ.IEQ_C(2,NEQ) ) THEN
                ICSPX = 1
                ICX = 2
              ELSE
                IEQ_C(ICX,NEQ) = NSPL + NSPS + NSPE + NSPG + M
              ENDIF
              GOTO 250
            ENDIF
  240     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Conservation-Equation Species Name: ' //
     &      ADUM(1:NCH)
          CALL WRMSGS( INDX )
  250     CONTINUE
          WRITE (IWR,'(/,2A)')
     &        'Conservation-Equation Species Name: ',ADUM(1:NCH)
!
!---      Read conservation-equation species coefficient  ---
!
          VARB = 'Conservation-Equation Species Coefficient: '
          IDFLT = 1
          EQ_C(ICX-1,NEQ) = 0.D+0
!
!---      Allow for returns in input lines  ---
!
          CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RDINPL( CHDUM )
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RDDPR(ISTART,ICOMMA,CHDUM,EQ_C(ICX-1,NEQ))
          WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &        ': ',EQ_C(ICX-1,NEQ)
  300   CONTINUE
        IF( ICSPX.EQ.0 ) THEN
          INDX = 4
          CHMSG = 'Component Species not a Constituent: ' //
     &      BDUM(1:NCHB)
          CALL WRMSGS( INDX )
        ENDIF
!
!---  Read next equilibrium equation  ---
!
        IF( NEQ.LT.NEQE ) WRITE(IWR,'(/)')
  500 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDCNEQ group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDEQEQ
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
!     Read equilibrium equations for reactions.
!     Standard equation form:
!
!     (Sp(1)) = (Keq^Ek)*((Sp(2))^Es(2))*((Sp(3))^Es(3))*...
!
!     Es(2) = EQ_E(1,NEQE)
!     Es(ns) = EQ_E(ns-1,NEQE)
!     Ek = EQ_E(ns,NEQE)
!     ns = IEQ_E(1,NEQE)
!     Sp(1) = IEQ_E(2,NEQE)
!     Sp(ns) = IEQ_E(ns+1,NEQE)
!     Keq = IEQ_E(ns+2,NEQE)
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 10 December 2004.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FILES
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
      CHARACTER*64 ADUM
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDEQEQ'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Write card information to ouput file  ---
!
      CARD = 'Equilibrium Equations Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of equilibrium equations  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Equilibrium Equations'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NEQE )
      IF( NEQE.GT.LEQE ) THEN
        INDX = 5
        CHMSG = 'Number of Equilibrium Equations > Parameter LEQE'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Loop over the equilibrium equations  ---
!
      DO 500 NEQ = 1,NEQE
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Number of Species in Equilibrium Equation'
        ICX = 1
        CALL RDINT( ISTART,ICOMMA,CHDUM,IEQ_E(ICX,NEQ) )
        IF( IEQ_E(ICX,NEQ).GT.LSEE ) THEN
          INDX = 5
          CHMSG = 'Number of Species in Equilibrium Equation > ' //
     &      'Parameter LSEE'
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Loop over the equilibrium-equation species  ---
!
        DO 200 NSP = 1,IEQ_E(ICX,NEQ)
          VARB = 'Equilibrium-Equation Species Name: '
!
!---      Allow for returns in input lines  ---
!
          CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RDINPL( CHDUM )
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          ICX = ICX + 1
!
!---      Aqueous species, assign species index  ---
!
          DO 100 M = 1,NSPL
            IF( SPNML(M).EQ.ADUM ) THEN
              IEQ_E(ICX,NEQ) = M
              GOTO 130
            ENDIF
  100     CONTINUE
!
!---      Solid species, assign species index  ---
!
          DO 110 M = 1,NSPS
            IF( SPNMS(M).EQ.ADUM ) THEN
              IEQ_E(ICX,NEQ) = NSPL + M
              GOTO 130
            ENDIF
  110     CONTINUE
!
!---      Exchanged species, assign species index  ---
!
          DO 115 M = 1,NSPE
            IF( SPNME(M).EQ.ADUM ) THEN
              IEQ_E(ICX,NEQ) = NSPL + NSPS + M
              GOTO 130
            ENDIF
  115     CONTINUE
!
!---      Gas species, assign species index  ---
!
          DO 120 M = 1,NSPG
            IF( SPNMG(M).EQ.ADUM ) THEN
              IEQ_E(ICX,NEQ) = NSPL + NSPS + NSPE + M
              GOTO 130
            ENDIF
  120     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Equilibrium-Equation Species Name: ' //
     &      ADUM(1:NCH)
          CALL WRMSGS( INDX )
  130     CONTINUE
          WRITE (IWR,'(/,2A)') ' Equilibrium-Equation Species Name: ',
     &      ADUM
!
!---      Read equilibrium-equation species exponent, skipping
!         the exponent for the equilibrium species  ---
!
          IF( NSP.GT.1 ) THEN
            VARB = 'Equilibrium-Equation Species Exponent: '
            IDFLT = 1
            EQ_E(ICX-2,NEQ) = 1.D+0
!
!---        Allow for returns in input lines  ---
!
            CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              CALL RDINPL( CHDUM )
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
            CALL RDDPR(ISTART,ICOMMA,CHDUM,EQ_E(ICX-2,NEQ))
            WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &          ': ',EQ_E(ICX-2,NEQ)
          ENDIF
  200   CONTINUE
!
!---    Equilibrium-equations equilibrium reaction  ---
!
        VARB = 'Equilibrium-Equation Equilibrium-Reaction Name: '
!
!---    Allow for returns in input lines  ---
!
        CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
        IF( INDX.EQ.0 ) THEN
          CALL RDINPL( CHDUM )
          CALL LCASE( CHDUM )
          ISTART = 1
        ENDIF
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        ICX = ICX+1
!
!---    Assign equilibrium-reaction index  ---
!
        DO 300 M = 1,NRCE
          IF( RCNME(M).EQ.ADUM ) THEN
            IEQ_E(ICX,NEQ) = M
            GOTO 320
          ENDIF
  300   CONTINUE
        INDX = 4
        CHMSG = 'Unrecognized Equilibrium-Equation ' //
     &    'Equilibrium-Reaction Name: ' // ADUM(1:NCH)
        CALL WRMSGS( INDX )
  320   CONTINUE
        WRITE (IWR,'(/,2A)') ' Equilibrium-Equation ' //
     &    'Equilibrium-Reaction Name: ', ADUM
!
!---    Read equilibrium-equation equilibrium-reaction exponent  ---
!
        VARB = 'Equilibrium-Equation Equilibrium-Reaction Exponent: '
        IDFLT = 1
        EQ_E(ICX-2,NEQ) = 1.D+0
!
!---    Allow for returns in input lines  ---
!
        CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
        IF( INDX.EQ.0 ) THEN
          CALL RDINPL( CHDUM )
          CALL LCASE( CHDUM )
          ISTART = 1
        ENDIF
        CALL RDDPR(ISTART,ICOMMA,CHDUM,EQ_E(ICX-2,NEQ))
        WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &      ': ',EQ_E(ICX-2,NEQ)
!
!---  Read next equilibrium equation  ---
!
        IF( NEQ.LT.NEQE ) WRITE(IWR,'(/)')
  500 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDEQEQ group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDEQRC
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
!     Read equilibrium reactions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 December 2004.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FILES
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
      CHARACTER*64 ADUM,UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDEQRC'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Write card information to ouput file  ---
!
      CARD = 'Equilibrium Reactions Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of equilibrium reactions  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Equilibrium Reactions'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NRCE )
      IF( NRCE.GT.LRCE ) THEN
        INDX = 5
        CHMSG = 'Number of Equilibrium Reactions > Parameter LRCE'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Loop over the equilibrium reactions  ---
!
      DO 500 NRC = 1,NRCE
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Equilibrium Reaction Name'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,RCNME(NRC))
        DO 10 M = 1,NRC-1
          IF( RCNME(M).EQ.RCNME(NRC) ) THEN
            INDX = 4
            CHMSG = 'Duplicate Equilibrium Reaction Name: ' //
     &        RCNME(NRC)
            CALL WRMSGS( INDX )
          ENDIF
   10   CONTINUE
        WRITE (IWR,'(/,2A)') '  Equilibrium Reaction Name: ',RCNME(NRC)
!
!---    Read equilibrium constant function coefficients
!       where, log(K) = b1*ln(T) + b2 + b3*T + b4/T + b5/(T^2)
!       and the equilibrium constant relates the aqueous
!       activity-molality product, gas fugacity, and
!       mineral activity  ---
!
        DO 300 M = 1,5
!
!---      Read equilibrium constant function coefficients  ---
!
          VARB = 'Equilibrium-Reaction Coefficient'
          IDFLT = 1
          RC_E(M,NRC) = 0.D+0
          CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_E(M,NRC))
          WRITE(IWR,'(A,1PE11.4)')
     &      '    Reaction Coefficient: ',RC_E(M,NRC)
  300   CONTINUE
!
!---    Read next equilibrium reaction  ---
!
        IF( NRC.LT.NRCE ) WRITE(IWR,'(/)')
  500 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDEQRC group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDEXSP
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
!     Read exchange species.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 December 2004.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FILES
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
      CHARACTER*64 ADUM,UNTS,SPNMX
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDEXSP'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Write card information to ouput file  ---
!
      CARD = 'Exchanged Species Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of Exchanged species  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Exchanged Species'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NSPE )
      IF( NSPE.GT.LSPE ) THEN
        INDX = 5
        CHMSG = 'Number of Exchanged Species > Parameter LSPE'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Read number of Exchange sites  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Exchange Sites'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NESITE )
      IF( NESITE.GT.LESITE ) THEN
        INDX = 5
        CHMSG = 'Number of Exchange Sites > Parameter LESITE'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Check for activity coefficient option  ---
!
      CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
      IF( INDX.EQ.1 ) THEN
        VARB = 'Activity Coefficient Option: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'gaines-thomas').NE.0 ) THEN
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),'Gaines-Thomas convention'
          IACTEX = 1
        ELSEIF( INDEX(ADUM(1:),'vanselow').NE.0 ) THEN
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),'Vanselow convention'
          IACTEX = 2
        ELSEIF( INDEX(ADUM(1:),'Gapon').NE.0 ) THEN
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),'Gapon convention'
          IACTEX = 3
        ELSE
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),'Gaines-Thomas convention'
        ENDIF
      ELSE
        WRITE(IWR,'(2X,2A)') VARB(1:IVR),'Gaines-Thomas convention'
      ENDIF
!
!---  Loop over the exchanged species  ---
!
      DO 500 NSP = 1,NSPE
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Exchanged Species Name: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,SPNME(NSP))
        DO 100 M = 1,NSP-1
          IF( SPNME(M).EQ.SPNME(NSP) ) THEN
            INDX = 4
            CHMSG = 'Duplicate Exchanged Species Name: ' // SPNME(NSP)
            CALL WRMSGS( INDX )
          ENDIF
  100   CONTINUE
        DO 110 M = 1,NSPL
          IF( SPNME(NSP).EQ.SPNML(M) )
     &      THEN
            INDX = 4
            CHMSG = 'Exchanged Species Name = Aqueous Species Name: ' //
     &      SPNME(NSP)
            CALL WRMSGS( INDX )
          ENDIF
  110   CONTINUE
        DO 120 M = 1,NSPS
          IF( SPNME(NSP).EQ.SPNMS(M) )
     &      THEN
            INDX = 4
            CHMSG = 'Exchanged Species Name = Solid Species ' //
     &       'Name: ' // SPNME(NSP)
            CALL WRMSGS( INDX )
          ENDIF
  120   CONTINUE
        WRITE (IWR,'(/,2A)') ' Exchanged Species Name: ',SPNME(NSP)
!
!---    Read adsorped cation  ---
!
        VARB = 'Reactive Species Name: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,SPNMX)
        NSPX = 0
        NSLX = 0
!
!---    Aqueous species, assign species index  ---
!
        DO 125 M = 1,NSPL
          IF( SPNML(M).EQ.SPNMX ) THEN
            NSPX = M
            IEL_LK(NSP) = NSPX
            NCH1 = INDEX(SPNME(NSP)(1:),' ') - 1
            NCH2 = INDEX(SPNMX(1:),' ') - 1
            WRITE (IWR,'(2A)') ' Exchanged/Cation Species Link: ' //
     &      SPNME(NSP)(1:NCH1)// ' <=> '//SPNMX(1:NCH2)
            GOTO 200
          ENDIF
  125   CONTINUE
        INDX = 4
        CHMSG = 'Unrecognized Reactive Species Name: ' //
     &    SPNMX(1:NCH)
        CALL WRMSGS( INDX )
  200   CONTINUE
!
!---    Read exchange site  ---
!
        VARB = 'Exchange site'
!        IUNM = 1
!        IDFLT = 1
        ISP_E(NSP) = 0
        CALL RDINT(ISTART,ICOMMA,CHDUM,ISP_E(NSP))
        WRITE(IWR,'(2X,2A,I4)') VARB(1:IVR),
     &      ': ',ISP_E(NSP)
!
!---  Read next exchanged species  ---
!
  500 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDEXSP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDGSSP
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
!     Read gas species.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 11 August 2005.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FILES
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
      CHARACTER*64 ADUM,UNTS,SPNMLX
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDGSSP'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Write card information to ouput file  ---
!
      CARD = 'Gas Species Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of gas species  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Gas Species'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NSPG )
      IF( NSPG.GT.LSPG ) THEN
        INDX = 5
        CHMSG = 'Number of Gas Species > Parameter LSPG'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Read gas molecular diffusion coefficient  ---
!
      VARB = 'Gas Species Molecular Diffusion Coefficient'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,SP_MDG)
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      WRITE(IWR,'(4A,1PE11.4)') VARB(1:IVR),', ',
     &  UNTS(1:NCH),': ',SP_MDG
      INDX = 0
      IUNM = 2
      IUNS = -1
      CALL RDUNIT(UNTS,SP_MDG,INDX)
!
!---  Loop over the gas species  ---
!
      DO 500 NSP = 1,NSPG
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Gas Species Name: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,SPNMG(NSP))
        DO 100 M = 1,NSP-1
          IF( SPNMG(M).EQ.SPNMG(NSP) ) THEN
            INDX = 4
            CHMSG = 'Duplicate Gas Species Name: ' // SPNMG(NSP)
            CALL WRMSGS( INDX )
          ENDIF
  100   CONTINUE
        DO 110 M = 1,NSPL
          IF( SPNMG(NSP).EQ.SPNML(M) ) THEN
            INDX = 4
            CHMSG = 'Gas Species Name = Aqueous Species Name: ' //
     &        SPNMG(NSP)
            CALL WRMSGS( INDX )
          ENDIF
  110   CONTINUE
        DO 120 M = 1,NSPS
          IF( SPNMG(NSP).EQ.SPNMS(M) ) THEN
            INDX = 4
            CHMSG = 'Gas Species Name = Solid Species Name: ' //
     &        SPNMG(NSP)
            CALL WRMSGS( INDX )
          ENDIF
  120   CONTINUE
        WRITE (IWR,'(/,2A)') ' Gas Species Name: ',SPNMG(NSP)
!
!---  Read next gas species  ---
!
  500 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDGSSP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDKNEQ
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
!     Read kinetic equations for reactions.
!     Standard equation form:
!
!     d( Cs(1)*[Sp(1)] + Cs(2)*[Sp(2)] + ... )/dt =
!       Cr(1)*R(1) + Cr(2)*R(2) + ...
!
!     Cs(1) = EQ_K(1,NEQK)
!     Cs(ns) = EQ_K(ns,NEQK)
!     Cr(1) = EQ_K(ns+1,NEQK)
!     Cr(nr) = EQ_K(ns+nr,NEQK)
!     ns = IEQ_K(1,NEQK)
!     Sp(1) = IEQ_K(2,NEQK)
!     Sp(ns) = IEQ_K(ns+1,NEQK)
!     nr = IEQ_K(ns+2,NEQK)
!     R(1) = IEQ_K(ns+3,NEQK)
!     R(nr) = IEQ_K(ns+nr+2,NEQK)
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 10 December 2004.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE FILES
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
      CHARACTER*64 ADUM
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDKNEQ'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Write card information to ouput file  ---
!
      CARD = 'Kinetic Equations Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of kinetic equations  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Kinetic Equations'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NEQK )
      IF( NEQK.GT.LEQK ) THEN
        INDX = 5
        CHMSG = 'Number of Kinetic Equations > Parameter LEQK'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Loop over the kinetic equations  ---
!
      DO 500 NEQ = 1,NEQK
!
!---    Kinetic equation species ---
!
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Kinetic Component Species Name'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,SPNMK(NEQ))
!
!---    Check for duplicate conservation component species name  ---
!
        DO 10 M = 1,NEQ-1
          IF( SPNMK(NEQ).EQ.SPNMK(M) ) THEN
            INDX = 4
            CHMSG = 'Duplicate Kinetic Component Species Name: '
     &        // SPNMK(NEQ)(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
   10   CONTINUE
!
!---    Load kinetic component species name into solute name
!       for output  ---
!
        SOLUT(NSOLU+NEQC+NEQ) = SPNMK(NEQ)(1:NCH)
!
!---    Number of species in kinetic equation  ---
!
        VARB = 'Number of Species in Kinetic Equation'
        ICX = 1
        JCX = 0
        CALL RDINT( ISTART,ICOMMA,CHDUM,IEQ_K(ICX,NEQ) )
        IF( IEQ_K(ICX,NEQ).GT.LSEK ) THEN
          INDX = 5
          CHMSG = 'Number of Species in Kinetic Equation > ' //
     &      'Parameter LSEK'
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Loop over the kinetic equation species  ---
!
        DO 200 NSP = 1,IEQ_K(ICX,NEQ)
          VARB = 'Kinetic-Equation Species Name: '
!
!---      Allow for returns in input lines  ---
!
          CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RDINPL( CHDUM )
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          ICX = ICX+1
!
!---      Aqueous species, assign species index  ---
!
          DO 100 M = 1,NSPL
            IF( SPNML(M).EQ.ADUM ) THEN
              IEQ_K(ICX,NEQ) = M
              GOTO 120
            ENDIF
  100     CONTINUE
!
!---      Solid species, assign species index  ---
!
          DO 110 M = 1,NSPS
            IF( SPNMS(M).EQ.ADUM ) THEN
              IEQ_K(ICX,NEQ) = NSPL + M
              GOTO 120
            ENDIF
  110     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Kinetic-Equation Species Name: ' //
     &      ADUM(1:NCH)
          CALL WRMSGS( INDX )
  120     CONTINUE
          WRITE (IWR,'(/,2A)') ' Kinetic-Equation Species Name: ',
     &      ADUM
!
!---      Read kinetic equation species coefficient  ---
!
          VARB = 'Kinetic-Equation Species Coefficient: '
          IDFLT = 1
          JCX = JCX+1
          EQ_K(JCX,NEQ) = 0.D+0
!
!---      Allow for returns in input lines  ---
!
          CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RDINPL( CHDUM )
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RDDPR(ISTART,ICOMMA,CHDUM,EQ_K(JCX,NEQ))
          WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &        ': ',EQ_K(JCX,NEQ)
  200   CONTINUE
!
!---    Kinetic-equation kinetic reactions  ---
!
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Number of Kinetic Reactions in Kinetic Equation'
        ICX = ICX+1
        CALL RDINT( ISTART,ICOMMA,CHDUM,IEQ_K(ICX,NEQ) )
        IF( IEQ_K(ICX,NEQ).GT.LREK ) THEN
          INDX = 5
          CHMSG = 'Number of Kinetic-Reactions in Kinetic ' //
     &      'Equation > Parameter LREK'
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Loop over the kinetic-equation kinetic reactions  ---
!
        DO 400 NRC = 1,IEQ_K(ICX,NEQ)
          VARB = 'Kinetic-Equation Kinetic-Reaction Name: '
!
!---      Allow for returns in input lines  ---
!
          CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RDINPL( CHDUM )
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          ICX = ICX+1
!
!---      Assign kinetic-reaction index  ---
!
          DO 300 M = 1,NRCK
            IF( RCNMK(M).EQ.ADUM ) THEN
              IEQ_K(ICX,NEQ) = M
              GOTO 320
            ENDIF
  300     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Kinetic-Equation ' //
     &      'Kinetic-Reaction Name: ' // ADUM(1:NCH)
          CALL WRMSGS( INDX )
  320     CONTINUE
          WRITE (IWR,'(/,2A)') ' Kinetic-Equation ' //
     &      'Kinetic-Reaction Name: ',ADUM
!
!---      Read kinetic-equation kinetic-reaction coefficient  ---
!
          VARB = 'Kinetic-Equation Kinetic-Reaction Coefficient: '
          IDFLT = 1
          JCX = JCX+1
          EQ_K(JCX,NEQ) = 0.D+0
!
!---      Allow for returns in input lines  ---
!
          CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RDINPL( CHDUM )
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RDDPR(ISTART,ICOMMA,CHDUM,EQ_K(JCX,NEQ))
          WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &        ': ',EQ_K(JCX,NEQ)
  400   CONTINUE
!
!---  Read next kinetic equation  ---
!
        IF( NEQ.LT.NEQK ) WRITE(IWR,'(/)')
  500 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDKNEQ group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDKNRC
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
!     Read kinetic reactions.
!
!     IRC_K(1,NRC)           number of kinetic reac. reactants (NSPR)
!     IRC_K(2,NRC)           number of kinetic reac. products (NSPP)
!                            NSPS = NSPR + NSPP
!     IRC_K(3:NSPR+2,NRC)    global species num. of reactants
!     IRC_K(NSPR+3:NSPS+2,NRC) global species num. of products
!     IRC_K(NSPS+3,NRC)      global species num. of mineral
!
!     RC_K(1:NSPR,NRC)       kinetic reac. reactant stochiometric coeff.
!     RC_K(NSPR+1:NSPS,NRC)  kinetic reac. product stochiometric coeff.
!     RC_K(NSPS+1,NRC)       forward kinetic reac. ref. rate, mol/m^2/s
!     RC_K(NSPS+2,NRC)       backward kinetic reac. ref. rate, mol/m^2/s
!     RC_K(NSPS+3,NRC)       kinetic reac. activation energy, J/mol
!     RC_K(NSPS+4,NRC)       kinetic reac. reference temperature, C
!     RC_K(NSPS+5,NRC)       equilibrium constant function coefficient
!     RC_K(NSPS+6,NRC)       equilibrium constant function coefficient
!     RC_K(NSPS+7,NRC)       equilibrium constant function coefficient
!     RC_K(NSPS+8,NRC)       equilibrium constant function coefficient
!     RC_K(NSPS+9,NRC)       equilibrium constant function coefficient
!     RC_K(NSPS+10,NRC)      equilibrium constant function coefficient
!
!     Multi-rate variables
!
!     IRC_K(NSPS+4,NRC)          number of rate mechanisms <= 5
!     IRC_K(NSPS+5,NRC)       number of species in rate mechanism #1 <= 5
!     IRC_K(NSPS+6:NSPS+10,NRC) global species num. in rate mechanism #1
!     IRC_K(NSPS+11,NRC)      number of species in rate mechanism #2 <= 5
!     IRC_K(NSPS+12:NSPS+16,NRC)global species num. in rate mechanism #2
!     IRC_K(NSPS+17,NRC)      number of species in rate mechanism #3 <= 5
!     IRC_K(NSPS+18:NSPS+22,NRC)global species num. in rate mechanism #3
!     IRC_K(NSPS+23,NRC)      number of species in rate mechanism #4 <= 5
!     IRC_K(NSPS+24:NSPS+28,NRC)global species num. in rate mechanism #4
!     IRC_K(NSPS+29,NRC)      number of species in rate mechanism #5 <= 5
!     IRC_K(NSPS+30:NSPS+34,NRC)global species num. in rate mechanism #5
!     RC_K(NSPS+11,NRC)       neutral reference reaction rate, mol/m^2/s
!     RC_K(NSPS+12,NRC)       neutral activation energy, J/mol
!     RC_K(NSPS+13,NRC)       neutral reference temperature, C
!     RC_K(NSPS+14,NRC)      mech. #1 reference reaction rate, mol/m^2/s
!     RC_K(NSPS+15,NRC)       mech. #1 activation energy, J/mol
!     RC_K(NSPS+16,NRC)       mech. #1 reference temperature, C
!     RC_K(NSPS+17:NSPS+21,NRC)mech. #1 species #1-5 stochiom. coeff.
!     RC_K(NSPS+22,NRC)      mech. #1 species #5 stochiometric coeff.
!     RC_K(NSPS+23,NRC)      mech. #2 reference reaction rate, mol/m^2/s
!     RC_K(13,NRC)           mech. #2 activation energy, J/mol
!     RC_K(14,NRC)           mech. #2 reference temperature, C
!     RC_K(15:19,NRC)        mech. #2 species #1-5 stochiometric coeff.
!     RC_K(20,NRC)           mech. #3 reference reaction rate, mol/m^2/s
!     RC_K(21,NRC)           mech. #3 activation energy, J/mol
!     RC_K(22,NRC)           mech. #3 reference temperature, C
!     RC_K(23:27,NRC)        mech. #3 species #1-5 stochiometric coeff.
!     RC_K(28,NRC)           mech. #4 reference reaction rate, mol/m^2/s
!     RC_K(29,NRC)           mech. #4 activation energy, J/mol
!     RC_K(30,NRC)           mech. #4 reference temperature, C
!     RC_K(31:35,NRC)        mech. #4 species #1-5 stochiometric coeff.
!     RC_K(36,NRC)           mech. #5 reference reaction rate, mol/m^2/s
!     RC_K(37,NRC)           mech. #5 activation energy, J/mol
!     RC_K(38,NRC)           mech. #5 reference temperature, C
!     RC_K(39:43,NRC)        mech. #5 species #1-5 stochiometric coeff.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 8 December 2004.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE FILES
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
      CHARACTER*64 ADUM,BDUM,UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDKNRC'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Write card information to ouput file  ---
!
      CARD = 'Kinetic Reactions Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of kinetic reactions  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Kinetic Reactions'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NRCK )
      IF( NRCK.GT.LRCK ) THEN
        INDX = 5
        CHMSG = 'Number of Kinetic Reactions > Parameter LRCK'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Loop over the kinetic reactions  ---
!
      RC_K = 0.D+0
      DO 500 NRC = 1,NRCK
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Kinetic Reaction Name: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,RCNMK(NRC))
        DO 10 M = 1,NRC-1
          IF( RCNMK(M).EQ.RCNMK(NRC) ) THEN
            INDX = 4
            CHMSG = 'Duplicate Kinetic-Reaction Name: ' // RCNMK(NRC)
            CALL WRMSGS( INDX )
          ENDIF
   10   CONTINUE
        WRITE (IWR,'(/,2A)') ' Kinetic-Reaction Name: ',RCNMK(NRC)
!
!---    Kinetic reaction type
!
        VARB = 'Kinetic Reaction Type: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'dissolu').NE.0 .OR.
     &    INDEX(ADUM(1:),'precip').NE.0 .OR.
     &    INDEX(ADUM(1:),'tst').NE.0 ) THEN
          IF ( INDEX(ADUM(1:),'tst').NE.0 ) WRITE(IWR,*) 'TST'
          IF( INDEX(ADUM(1:),'ph').NE.0 ) THEN
            IRCKT(NRC) = 5
            IF( INDEX(ADUM(1:),'schaef').NE.0 ) THEN
              IF( INDEX(ADUM(1:),'toward products').NE.0 ) THEN
                IRCKT(NRC) = 8
              ELSEIF( INDEX(ADUM(1:),'toward reactants').NE.0 ) THEN
                IRCKT(NRC) = 9
              ENDIF
            ELSE IF( INDEX(ADUM(1:),'glass').NE.0 ) THEN
              IF( INDEX(ADUM(1:),'toward reactants').NE.0 ) THEN
                IRCKT(NRC) = 14
                WRITE(IWR,*) 'Glass'
              ENDIF
            ELSE
              IF( INDEX(ADUM(1:),'toward products').NE.0 ) THEN
                IRCKT(NRC) = 6
              ELSEIF( INDEX(ADUM(1:),'toward reactants').NE.0 ) THEN
                IRCKT(NRC) = 7
              ENDIF
            ENDIF
          ELSE IF( INDEX(ADUM(1:),'constant').NE.0 .OR.
     &      INDEX(ADUM(1:),'constant rate').NE.0 ) THEN
            IRCKT(NRC) = 16
          ELSE
            IRCKT(NRC) = 10
            IF( INDEX(ADUM(1:),'toward products').NE.0 ) THEN
              IRCKT(NRC) = 11
            ELSEIF( INDEX(ADUM(1:),'toward reactants').NE.0 ) THEN
              IRCKT(NRC) = 12
            ENDIF
          ENDIF
!
!---    Read coefficient matrix to translate rxn rate from 
!       pore- to macro-scale
!
          IF ( INDEX(ADUM(1:),'w/coef').NE.0 ) THEN
             WRITE(IWR,*) 'w/coeff'
             ISLC(58) = 1
             UNTS = 'null'
             CALL RDCM( ISTART,CHDUM,UNTS,CFMX )
          ENDIF
        ELSEIF( ( INDEX(ADUM(1:),'emulsion').NE.0 .OR.
     &    INDEX(ADUM(1:),'oil').NE.0 ) .AND.
     &    INDEX(ADUM(1:),'sorption').NE.0 ) THEN
          IRCKT(NRC) = 15
        ELSEIF( INDEX(ADUM(1:),'multi').NE.0 .AND.
     &    INDEX(ADUM(1:),'rate').NE.0 ) THEN
          IRCKT(NRC) = 20
        ELSEIF( INDEX(ADUM(1:),'forward').NE.0 .OR.
     &    INDEX(ADUM(1:),'backward').NE.0 ) THEN
          IRCKT(NRC) = 1
        ELSEIF( INDEX(ADUM(1:),'monod').NE.0 .AND.
     &    INDEX(ADUM(1:),'valocchi').NE.0 ) THEN
          IRCKT(NRC) = 2
        ELSEIF( INDEX(ADUM(1:),'sorption').NE.0 .AND.
     &    INDEX(ADUM(1:),'valocchi').NE.0 ) THEN
          IRCKT(NRC) = 3
        ELSEIF( INDEX(ADUM(1:),'sorption').NE.0 .AND.
     &    INDEX(ADUM(1:),'langmuir').NE.0 ) THEN
          IRCKT(NRC) = 13
        ELSEIF( INDEX(ADUM(1:),'biomass').NE.0 .AND.
     &    INDEX(ADUM(1:),'valocchi').NE.0 ) THEN
          IRCKT(NRC) = 4
        ELSEIF( INDEX(ADUM(1:),'monod').NE.0 .AND.
     &    INDEX(ADUM(1:),'dual').NE.0 ) THEN
          IRCKT(NRC) = 35
        ELSEIF( INDEX(ADUM(1:),'monod').NE.0 .AND.
     &    INDEX(ADUM(1:),'single').NE.0 ) THEN
          IRCKT(NRC) = 36
        ELSEIF( INDEX(ADUM(1:),'monod').NE.0 ) THEN
          IRCKT(NRC) = 22
        ELSEIF( INDEX(ADUM(1:),'biomass').NE.0 ) THEN
          IRCKT(NRC) = 24
        ELSEIF( INDEX(ADUM(1:),'lognormal').NE.0 .AND.
     &    INDEX(ADUM(1:),'liu').NE.0 ) THEN
          IRCKT(NRC) = 41
        ELSEIF( INDEX(ADUM(1:),'dualdomain').NE.0 .AND.
     &    INDEX(ADUM(1:),'liu').NE.0 ) THEN
          IRCKT(NRC) = 42
        ENDIF
!
!---    Mineral  ---
!
        IF( (IRCKT(NRC).GE.10 .AND. IRCKT(NRC).LE.12) .OR.
     &    (IRCKT(NRC).GE.5 .AND. IRCKT(NRC).LE.9) .OR.
     &    IRCKT(NRC).EQ.14 .OR. IRCKT(NRC).EQ.16 .OR.
     &    IRCKT(NRC).EQ.20 ) THEN
!
!---      Allow for returns in input lines  ---
!
          CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RDINPL( CHDUM )
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          VARB = 'Mineral Name'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
!
!---      Solid species, assign species index  ---
!
          DO 20 M = 1,NSPS
            IF( SPNMS(M).EQ.BDUM ) THEN
              IRC_KX = NSPL + M
              GOTO 30
            ENDIF
   20     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Kinetic Mineral Name: ' //
     &      BDUM(1:NCH)
          CALL WRMSGS( INDX )
   30     CONTINUE
        ENDIF
!
!---    Multi-rate mineral  ---
!
        IF( IRCKT(NRC).EQ.20 ) GOTO 400
!
!---    Number of reactants in kinetic reaction  ---
!
        VARB = 'Number of Reactants in Kinetic Reaction'
        CALL RDINT( ISTART,ICOMMA,CHDUM,IRC_K(1,NRC) )
        NSPRX = IRC_K(1,NRC)
        IF( NSPRX.GT.LSPK ) THEN
          INDX = 4
          CHMSG = 'Number of Reactants in Kinetic Reaction > ' //
     &      'Parameter LSPK'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IRCKT(NRC).EQ.2 .AND. NSPRX.NE.3 ) THEN
          INDX = 4
          CHMSG = 'Valocchi-Monod Kinetics: Number of Reactants != 3'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IRCKT(NRC).EQ.3 .AND. NSPRX.NE.1 ) THEN
          INDX = 4
          CHMSG = 'Valocchi-Sorption Kinetics: Number of Reactants != 1'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IRCKT(NRC).EQ.13 .AND. NSPRX.NE.1 ) THEN
          INDX = 4
          CHMSG = 'Langmuir-Sorption Kinetics: Number of Reactants != 1'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IRCKT(NRC).EQ.4 .AND. NSPRX.NE.3 ) THEN
          INDX = 4
          CHMSG = 'Valocchi-Biomass Kinetics:  Number of Reactants != 3'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IRCKT(NRC).EQ.22 .AND. NSPRX.LE.1 ) THEN
          INDX = 4
          CHMSG = 'Monod Kinetics:  Number of Reactants <= 1'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IRCKT(NRC).EQ.24 .AND. NSPRX.LE.1 ) THEN
          INDX = 4
          CHMSG = 'Biomass Kinetics:  Number of Reactants <= 1'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IRCKT(NRC).EQ.35 .AND. NSPRX.NE.2 ) THEN
          INDX = 4
          CHMSG = 'Dual-Monod Kinetics:  Number of Reactants != 2'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IRCKT(NRC).EQ.36 .AND. NSPRX.NE.2 ) THEN
          INDX = 4
          CHMSG = 'Single-Monod Kinetics:  Number of Reactants != 2'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IRCKT(NRC).EQ.15 .AND. NSPRX.NE.1 ) THEN
          INDX = 4
          CHMSG = 'Oil-Sorption Kinetics:  Number of Reactants != 1'
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Loop over the kinetic reaction reactants  ---
!
        ICX = 2
        JCX = 0
        DO 130 NSP = 1,NSPRX
          VARB = 'Kinetic-Reaction Reactant Name: '
          ICX = ICX+1
          JCX = JCX+1
!
!---      Allow for returns in input lines  ---
!
          CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RDINPL( CHDUM )
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---      Aqueous species, assign species index  ---
!
          DO 100 M = 1,NSPL
            IF( SPNML(M).EQ.ADUM ) THEN
              IRC_K(ICX,NRC) = M
              GOTO 120
            ENDIF
  100     CONTINUE
!
!---      Solid species, assign species index  ---
!
          DO 110 M = 1,NSPS
            IF( SPNMS(M).EQ.ADUM ) THEN
              IRC_K(ICX,NRC) = NSPL + M
              GOTO 120
            ENDIF
  110     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Kinetic Species Name: ' //
     &      ADUM(1:NCH)
          CALL WRMSGS( INDX )
  120     CONTINUE
          WRITE (IWR,'(/,2A)') ' Kinetic Reaction Reactant Name: ',
     &      ADUM
!
!---      Set default values
!
          IJK = 1
          LNDX = LSPK+11
!
!---      Skip kinetic reaction reactant stoichiometric coefficient
!         for Monod and Biomass kinetics  ---
!
          IF( IRCKT(NRC).NE.22 .AND. IRCKT(NRC).NE.24 ) THEN
!
!---        Read kinetic reaction reactant stoichiometric 
!           coefficient  ---
!
            VARB = 'Kinetic-Reaction Reactant Stoichiometric ' //
     &        'Coefficient: '
            IDFLT = 1
!           RC_K(JCX,NRC) = 0.D+0
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              INDX = JCX
              UNTS = 'null'
              CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
              IRCKN(JCX)=1
            ELSE
!
!---        Allow for returns in input lines  ---
!
            CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              CALL RDINPL( CHDUM )
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
            WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &            ': ',RC_K(JCX,1,NRC)
            ENDIF
          ELSE
            JCX = JCX-1
          ENDIF
  130   CONTINUE
!
!---    Number of products in kinetic reaction  ---
!
        ICX = 2+NSPRX
!
!---    Skip kinetic reaction products
!       for Monod and Biomass kinetics  ---
!
        IF( IRCKT(NRC).NE.22 .AND. IRCKT(NRC).NE.24 ) THEN
          VARB = 'Number of Products in Kinetic Reaction'
          CALL RDINT( ISTART,ICOMMA,CHDUM,IRC_K(2,NRC) )
          NSPPX = IRC_K(2,NRC)
          IF( NSPPX+NSPRX.GT.LSPK ) THEN
            INDX = 4
            CHMSG = 'Number of Reactants + Products in ' //
     &        'Kinetic Reaction > Parameter LSPK'
            CALL WRMSGS( INDX )
          ENDIF
          IF( IRCKT(NRC).EQ.2 .AND. NSPPX.NE.0 ) THEN
            INDX = 4
            CHMSG = 'Valocchi-Monod Kinetics: Number of Products != 0'
            CALL WRMSGS( INDX )
          ENDIF
          IF( IRCKT(NRC).EQ.3 .AND. NSPPX.NE.1 ) THEN
            INDX = 4
            CHMSG = 'Valocchi-Sorption Kinetics:  ' //
     &        'Number of Products != 1'
            CALL WRMSGS( INDX )
          ENDIF
          IF( IRCKT(NRC).EQ.13 .AND. NSPPX.NE.1 ) THEN
            INDX = 4
            CHMSG = 'Langmuir-Sorption Kinetics: ' //
     &        'Number of Products != 1'
            CALL WRMSGS( INDX )
          ENDIF
          IF( IRCKT(NRC).EQ.4 .AND. NSPPX.NE.0 ) THEN
            INDX = 4
            CHMSG = 'Valocchi-Biomass Kinetics: Number of Products != 0'
            CALL WRMSGS( INDX )
          ENDIF
          IF( IRCKT(NRC).EQ.35 .AND. NSPPX.NE.0 ) THEN
            INDX = 4
            CHMSG = 'Dual-Monod Kinetics: Number of Products != 0'
            CALL WRMSGS( INDX )
          ENDIF
          IF( IRCKT(NRC).EQ.36 .AND. NSPPX.NE.0 ) THEN
            INDX = 4
            CHMSG = 'Single-Monod Kinetics: Number of Products != 0'
            CALL WRMSGS( INDX )
          ENDIF
          IF( IRCKT(NRC).EQ.15 .AND. NSPPX.NE.1 ) THEN
            INDX = 4
            CHMSG = 'Oil-Sorption Kinetics: Number of Products != 1'
            CALL WRMSGS( INDX )
          ENDIF
!
!---      Loop over the kinetic reaction products  ---
!
          DO 230 NSP = 1,NSPPX
            VARB = 'Kinetic-Reaction Product Name: '
            ICX = ICX+1
!
!---        Allow for returns in input lines  ---
!
            CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              CALL RDINPL( CHDUM )
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---        Aqueous species, assign species index  ---
!
            DO 200 M = 1,NSPL
              IF( SPNML(M).EQ.ADUM ) THEN
                IRC_K(ICX,NRC) = M
                GOTO 220
              ENDIF
  200       CONTINUE
!
!---        Solid species, assign species index  ---
!
            DO 210 M = 1,NSPS
              IF( SPNMS(M).EQ.ADUM ) THEN
                IRC_K(ICX,NRC) = NSPL + M
                GOTO 220
              ENDIF
  210       CONTINUE
            INDX = 4
            CHMSG = 'Unrecognized Kinetic Species Name: ' //
     &        ADUM(1:NCH)
            CALL WRMSGS( INDX )
  220       CONTINUE
            WRITE (IWR,'(/,2A)') ' Kinetic Reaction Reactant Name: ',
     &        ADUM
!
!---        Read kinetic reaction reactant
!           stoichiometric coefficient  ---
!
            VARB = 'Kinetic-Reaction Reactant Stoichiometric ' //
     &        'Coefficient: '
            IDFLT = 1
            JCX = JCX+1
!           RC_K(JCX,NRC) = 0.D+0
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              INDX = JCX
              UNTS = 'null'
              CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
              IRCKN(JCX)=1
            ELSE
!
!---        Allow for returns in input lines  ---
!
            CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              CALL RDINPL( CHDUM )
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
            WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &            ': ',RC_K(JCX,1,NRC)
            ENDIF
  230     CONTINUE
        ELSE
          IRC_K(2,NRC) = 0
        ENDIF
!
!---    Read kinetic reaction parameters  ---
!
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
!
!---    Dissolution-precipitation type reactions  ---
!
        IF( (IRCKT(NRC).GE.10 .AND. IRCKT(NRC).LE.12) .OR.
     &    IRCKT(NRC).EQ.14 .OR. IRCKT(NRC).EQ.16 .OR.
     &    (IRCKT(NRC).GE.5 .AND. IRCKT(NRC).LE.9) ) THEN
!
!---      Identify the mineral species  ---
!
          IRC_K(3+NSPRX+NSPPX,NRC) = IRC_KX
          ISP_MN(IRC_KX) = 1
!
!---      Read forward dissolution-precipitation
!         reference reaction rate  ---
!
          VARB = 'Kinetic Reaction Reference Rate'
          UNTS = 'mol/m^2 s'
          IUNMOL = 1
          IUNM = -2
          IUNS = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
            DO 232 J = 1,LRC
!
!---        Convert from kmol/m^2 s to mol/m^2 s  ---
!           
              RC_K(JCX,J,NRC) = 1.D+3*RC_K(JCX,J,NRC)

  232       CONTINUE
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---      Convert from kmol/m^2 s to mol/m^2 s  ---
!
            RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &      ', mol/m^2 s)'
          ENDIF
!
!---      Read activation energy  ---
!
          VARB = 'Kinetic Reaction Activation Energy'
          UNTS = 'j/mol'
          IUNMOL = -1
          IUNM = 2
          IUNKG = 1
          IUNS = -2
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
            DO 234 J = 1,LRC
!
!---        Convert from J/kmol to J/mol ---
!
              RC_K(JCX,J,NRC) = 1.D-3*RC_K(JCX,J,NRC)
  234       CONTINUE
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---      Convert from J/kmol to J/mol  ---
!
            RC_K(JCX,1,NRC) = 1.D-3*RC_K(JCX,1,NRC)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &      ', J/mol)'
          ENDIF
!
!---      Read forward dissolution-precipitation
!         reference reaction temperature  ---
!
          VARB = 'Kinetic Reaction Reference Temperature'
          UNTS = 'c'
          IUNK = 1
          IDFLT = 1
          JCX = JCX + 1
!xl          RC_K(JCX,1:LRCK,NRC) = 1.D+20
          RC_K(JCX,1:LCKN,NRC) = 1.D+20
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &      ', C)'
          ENDIF
!
!---      Read dissolution-precipitation
!         pH exponent  ---
!
          IF(( IRCKT(NRC).GE.5 .AND. IRCKT(NRC).LE.9 ) .OR.
     &    (IRCKT(NRC).EQ.14 ) ) THEN
            VARB = 'Kinetic Reaction pH Exponent'
            IDFLT = 1
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              INDX = JCX+6
              CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
              IRCKN(JCX)=1
            ELSE
              RC_K(JCX+6,1,NRC) = 0.D+0
              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX+6,1,NRC))
            WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &          ': ',RC_K(JCX+6,1,NRC)
            ENDIF
          ENDIF
!
!---      Read equilibrium constant function coefficients
!         where, log(K) = b1*ln(T) + b2 + b3*T + b4/T + b5/(T^2)
!         and the equilibrium constant relates the aqueous
!         activity-molality product, gas fugacity, and
!         mineral activity  ---
!
          CALL RDINPL( CHDUM )
          CALL LCASE( CHDUM )
          ISTART = 1
          DO 300 M = 1,5
!
!---        Read equilibrium constant function coefficients  ---
!
            VARB = 'Equilibrium Reaction Constant Coefficient'
            IDFLT = 1
            JCX = JCX + 1
!           RC_K(JCX,NRC) = 0.D+0
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              INDX = JCX
              CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
              IRCKN(JCX)=1
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
            WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &          ': ',RC_K(JCX,1,NRC)
            ENDIF
  300     CONTINUE
!
!---    Forward-backward type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.1 ) THEN
!
!---      Read forward reaction rate  ---
!
          VARB = 'Forward Kinetic Reaction Rate'
          UNTS = '1/s'
          IUNS = -1
          IDFLT = 1
          JCX = JCX + 1
          VARX = 1.D+0
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
          ENDIF
!          CALL RDUNIT(UNTS,VARX,INDX)
!!
!!---      Convert from exponent to rate, 1/s  ---
!!
!          RC_K(JCX,NRC) = VARX*EXP(TOLN*RC_K(JCX,NRC))
          WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &      ', 1/s)'
!
!---      Read backward reaction rate  ---
!
          VARB = 'Backward Kinetic Reaction Rate'
          UNTS = 'mol/s'
          IUNS = -1
          IDFLT = 1
          JCX = JCX + 1
          VARX = 1.D+0
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
          ENDIF
!          CALL RDUNIT(UNTS,VARX,INDX)
!!
!!---      Convert from exponent to rate, 1/s  ---
!!
!          RC_K(JCX,NRC) = VARX*EXP(TOLN*RC_K(JCX,NRC))
          WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &      ', 1/s)'
!
!---    Valocchi-Monod type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.2 ) THEN
!
!---      Read half-saturation constant for donor  ---
!
          VARB = 'Valocchi-Monod Kin. Reac.: ' //
     &      'Half-Sat. Const. for Donor'
          UNTS = 'mol/kg'
          IUNMOL = 1
          IUNKG = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
            DO 236 J = 1,LRC
!
!---        Convert from kmol/kg to mol/kg ---
!
              RC_K(JCX,J,NRC) = 1.D+3*RC_K(JCX,J,NRC)
  236       CONTINUE
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---      Convert from kmol/kg to mol/kg  ---
!
            RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &      ', mol/kg)'
          ENDIF
!
!---      Read half-saturation constant for acceptor  ---
!
          VARB = 'Valocchi-Monod Kin. Reac.: ' //
     &      'Half-Sat. Const. for Acceptor'
          UNTS = 'mol/kg'
          IUNMOL = 1
          IUNKG = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
            DO 238 J = 1,LRC
!
!---        Convert from kmol/kg to mol/kg ---
!
              RC_K(JCX,J,NRC) = 1.D+3*RC_K(JCX,J,NRC)
  238       CONTINUE
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---      Convert from kmol/kg to mol/kg  ---
!
            RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &      ', mol/kg)'
          ENDIF
!
!---      Read maximum specific rate of substrate utilization  ---
!
          VARB = 'Valocchi-Monod Kin. Reac.: ' //
     &      'Max. Spec. Rate of Substrate Util.'
          UNTS = '1/s'
          IUNS = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &        ', 1/s)'
          ENDIF
!
!---    Valocchi-Sorption type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.3 ) THEN
!
!---      Read mass transfer coefficient  ---
!
          VARB = 'Valocchi-Sorption Kin. Reac.: ' //
     &      'Mass Transfer Coeff.'
          UNTS = '1/s'
          IUNS = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &      ', 1/s)'
          ENDIF
!
!---      Read distribution coefficient  ---
!
          VARB = 'Valocchi-Sorption Kin. Reac.: ' //
     &      'Distribution Coeff.'
          UNTS = 'm^3/kg'
          IUNM = 3
          IUNKG = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &      ', m^3/kg)'
          ENDIF
!
!---    Langmuir-Sorption type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.13 ) THEN
!
!---      Read forward mass transfer coefficient  ---
!
          VARB = 'Langmuir-Sorption Kin. Reac.: ' //
     &      'Forward Mass Transfer Coeff.'
          UNTS = '1/s'
          IUNS = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &      ', 1/s)'
          ENDIF
!
!---      Read backward mass transfer coefficient  ---
!
          VARB = 'Langmuir-Sorption Kin. Reac.: ' //
     &      'Backward Mass Transfer Coeff.'
          UNTS = '1/s'
          IUNS = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &      ', 1/s)'
          ENDIF
!
!---      Read maximum sorbed concentration ---
!
          VARB = 'Langmuir-Sorption Kin. Reac.: ' //
     &      'Maximum Sorbed Concentration'
          UNTS = 'mol/kg'
          IUNMOL = 1
          IUNKG = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
            DO 242 J = 1,LRC
!
!---        Convert from kmol/kg to mol/kg ---
!
              RC_K(JCX,J,NRC) = 1.D+3*RC_K(JCX,J,NRC)
  242       CONTINUE
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---      Convert from kmol/kg to mol/kg  ---
!
            RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &      ', mol/kg)'
          ENDIF
!
!---    Valocchi-Biomass type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.4 ) THEN
!
!---      Read half-saturation constant for donor  ---
!
          VARB = 'Valocchi-Monod Kin. Reac.: ' //
     &      'Half-Sat. Const. for Donor'
          UNTS = 'mol/kg'
          IUNMOL = 1
          IUNKG = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
            DO 244 J = 1,LRC
!
!---        Convert from kmol/kg to mol/kg ---
!
              RC_K(JCX,J,NRC) = 1.D+3*RC_K(JCX,J,NRC)
  244       CONTINUE
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---      Convert from kmol/kg to mol/kg  ---
!
            RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &      ', mol/kg)'
          ENDIF
!
!---      Read half-saturation constant for acceptor  ---
!
          VARB = 'Valocchi-Monod Kin. Reac.: ' //
     &      'Half-Sat. Const. for Acceptor'
          UNTS = 'mol/kg'
          IUNMOL = 1
          IUNKG = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
            DO 246 J = 1,LRC
!
!---        Convert from kmol/kg to mol/kg ---
!
              RC_K(JCX,J,NRC) = 1.D+3*RC_K(JCX,J,NRC)
  246       CONTINUE
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---       Convert from kmol/kg to mol/kg  ---
!
             RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
             WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &       ', mol/m^3)'
          ENDIF
!
!---       Read maximum specific rate of substrate utilization  ---
!
           VARB = 'Valocchi-Monod Kin. Reac.: ' //
     &       'Max. Spec. Rate of Substrate Util.'
           UNTS = '1/s'
           IUNS = -1
           IDFLT = 1
           JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
           CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
           WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
           INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &       ', 1/s)'
          ENDIF
!
!---       Read microbial yield coefficient  ---
!
           VARB = 'Valocchi-Monod Kin. Reac.: ' //
     &       'Microbial Yield Coeff.'
           IDFLT = 1
           JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
           WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &        ': ',RC_K(JCX,1,NRC)
          ENDIF
!
!---       Read first-order micobial decay coefficient  ---
!
           VARB = 'Valocchi-Monod Kin. Reac.: ' //
     &       'First-Order Microbial Decay Coeff.'
           UNTS = '1/s'
           IUNS = -1
           IDFLT = 1
           JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
           CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
           WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
           INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &       ', 1/s)'
          ENDIF
!
!---    Micro-emulsion type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.15 ) THEN
!
!---      Read equivalent collector diameter  ---
!
          VARB = 'Micro-Emulsion Kin. Reac.: ' //
     &      'Equivalent Collector Diameter'
          UNTS = 'm'
          IUNM = 1
          IDFLT = 1
          JCX = JCX + 1
          RC_K(JCX,1:LRC,NRC) = 1.D-4
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
           CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
           CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
           WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &       UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
           INDX = 0
           CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
           WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &       ', m)'
          ENDIF
!
!---      Read empty bed collision efficiency  ---
!
          VARB = 'Micro-Emulsion Kin. Reac.: ' //
     &      'Empty Bed Collision Efficiency'
          IDFLT = 1
          JCX = JCX + 1
          RC_K(JCX,1:LRC,NRC) = 2.5D-5
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
           CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
           WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &       ': ',RC_K(JCX,1,NRC)
          ENDIF
!
!---      Read maximum oil retention by sediment  ---
!
          VARB = 'Micro-Emulsion Kin. Reac.: ' //
     &      'Maximum Oil Retention by Sediment'
          IDFLT = 1
          JCX = JCX + 1
          RC_K(JCX,1:LRC,NRC) = 3.7D-3
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
           WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &        ': ',RC_K(JCX,1,NRC)
          ENDIF
!
!---       Read equivalent oil-droplet diameter  ---
!
           VARB = 'Micro-Emulsion Kin. Reac.: ' //
     &       'Equivalent Oil-Droplet Diameter'
           UNTS = 'm'
           IUNM = 1
           IDFLT = 1
           JCX = JCX + 1
          RC_K(JCX,1,NRC) = 1.25D-6
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
           CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
           WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
           INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &       ', m)'
          ENDIF
!
!---       Read oil-droplet density  ---
!
           VARB = 'Micro-Emulsion Kin. Reac.: ' //
     &       'Oil-Droplet Density'
           UNTS = 'kg/m^3'
           IUNKG = 1
           IUNM = -3
           IDFLT = 1
           JCX = JCX + 1
          RC_K(JCX,1:LRC,NRC) = 9.5D+2
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
           CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
           WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
           INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &       ', kg/m^3)'
          ENDIF
!
!---    Monod type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.22 ) THEN
!
!---      Loop over reactants, less one  ---
!
          DO 320 NSP = 1,NSPRX-1
!
!---        Allow for returns in input lines  ---
!
            CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              CALL RDINPL( CHDUM )
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
!
!---        Read half-saturation constant  ---
!
            VARB = 'Monod Kinetic Reaction: ' //
     &        'Half-Saturation Constant'
            UNTS = 'mol/liter'
            IUNMOL = 1
            IUNM = -3
            IDFLT = 1
            JCX = JCX + 1
!           RC_K(JCX,NRC) = 0.D+0
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              INDX = JCX
              CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
              IRCKN(JCX)=1
              DO 248 J = 1,LRC
!            
!---          Convert from kmol/m^3 to kmol/m^3 ---
!            
                RC_K(JCX,J,NRC) = 1.D+3*RC_K(JCX,J,NRC)
  248         CONTINUE
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
            INDX = 0
              CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---        Convert from kmol/m^3 to mol/m^3  ---
!
              RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &        ', mol/m^3)'
            ENDIF
  320     CONTINUE
!
!---      Allow for returns in input lines  ---
!
          CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RDINPL( CHDUM )
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
!
!---      Read maximum specific rate of substrate utilization  ---
!
          VARB = 'Monod Kinetic Reaction: ' //
     &      'Maximum Specific Rate of Reactant Utilization'
          UNTS = '1/s'
          IUNS = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &      ', 1/s)'
          ENDIF
!
!---    Biomass type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.24 ) THEN
!
!---      Loop over reactants, less one  ---
!
          DO 330 NSP = 1,NSPRX-1
!
!---        Allow for returns in input lines  ---
!
            CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              CALL RDINPL( CHDUM )
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
!
!---        Read half-saturation constant  ---
!
            VARB = 'Biomass Kinetic Reaction: ' //
     &        'Half-Saturation Constant'
            UNTS = 'mol/liter'
            IUNMOL = 1
            IUNM = -3
            IDFLT = 1
            JCX = JCX + 1
!           RC_K(JCX,NRC) = 0.D+0
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              INDX = JCX
              CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
              IRCKN(JCX)=1
              DO 252 J = 1,LRC
!            
!---          Convert from kmol/m^3 to kmol/m^3 ---
!            
                RC_K(JCX,J,NRC) = 1.D+3*RC_K(JCX,J,NRC)
  252         CONTINUE
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
            INDX = 0
              CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---        Convert from kmol/m^3 to mol/m^3  ---
!
              RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &        ', mol/m^3)'
!
!---        Allow for returns in input lines  ---
!
            CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              CALL RDINPL( CHDUM )
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
            ENDIF
!
!---        Read maximum specific rate of substrate utilization  ---
!
            VARB = 'Biomass Kinetic Reaction: ' //
     &        'Maximum Specific Rate of Reactant Utilization'
            UNTS = '1/s'
            IUNS = -1
            IDFLT = 1
            JCX = JCX + 1
!           RC_K(JCX,NRC) = 0.D+0
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              INDX = JCX
              CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
              IRCKN(JCX)=1
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
            INDX = 0
              CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &        ', 1/s)'
            ENDIF
  330     CONTINUE
!
!---       Read microbial yield coefficient  ---
!
           VARB = 'Biomass Kinetic Reaction: ' //
     &       'Microbial Yield Coefficient'
           IDFLT = 1
           JCX = JCX + 1
!          RC_K(JCX,NRC) = 0.D+0
           IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
             INDX = JCX
             CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
             IRCKN(JCX)=1
           ELSE
             CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
           WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &         ': ',RC_K(JCX,1,NRC)
           ENDIF
!
!---      Read first-order micobial decay coefficient  ---
!
          VARB = 'Biomass Kinetic Reaction: ' //
     &      'Microbial Decay Coefficient'
          UNTS = '1/s'
          IUNS = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &      ', 1/s)'
          ENDIF
!
!---    Dual-Monod type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.35 ) THEN
!
!---      Read half-saturation constant for donor  ---
!
          VARB = 'Dual-Monod Kin. Reac.: ' //
     &      'Half-Sat. Const. for Donor'
          UNTS = 'mol/kg'
          IUNMOL = 1
          IUNKG = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
            DO 254 J = 1,LRC
!            
!---        Convert from kmol/kg to kmol/kg ---
!          
              RC_K(JCX,J,NRC) = 1.D+3*RC_K(JCX,J,NRC)
  254       CONTINUE
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
          ENDIF
!
!---      Convert from kmol/kg to mol/kg  ---
!
          RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &      ', mol/kg)'
!
!---      Read half-saturation constant for acceptor  ---
!
          VARB = 'Dual-Monod Kin. Reac.: ' //
     &      'Half-Sat. Const. for Acceptor'
          UNTS = 'mol/kg'
          IUNMOL = 1
          IUNKG = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
            DO 256 J = 1,LRC
!            
!---        Convert from kmol/kg to kmol/kg ---
!          
              RC_K(JCX,J,NRC) = 1.D+3*RC_K(JCX,J,NRC)
  256       CONTINUE
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---      Convert from kmol/kg to mol/kg  ---
!
            RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &      ', mol/kg)'
          ENDIF
!
!---      Read maximum specific rate of substrate utilization  ---
!
          VARB = 'Dual-Monod Kin. Reac.: ' //
     &      'Max. Spec. Rate of Substrate Util.'
          UNTS = '1/s'
          IUNS = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &        ', 1/s)'
          ENDIF
!
!---    Single-Monod type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.36 ) THEN
!
!---      Read half-saturation constant for donor  ---
!
          VARB = 'Single-Monod Kin. Reac.: ' //
     &      'Half-Sat. Const. for Donor'
          UNTS = 'mol/kg'
          IUNMOL = 1
          IUNKG = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
            DO 258 J = 1,LRC
!            
!---        Convert from kmol/kg to kmol/kg ---
!          
              RC_K(JCX,J,NRC) = 1.D+3*RC_K(JCX,J,NRC)
  258       CONTINUE
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---      Convert from kmol/kg to mol/kg  ---
!
            RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &      ', mol/kg)'
          ENDIF
!
!---      Read maximum specific rate of substrate utilization  ---
!
          VARB = 'Single-Monod Kin. Reac.: ' //
     &      'Max. Spec. Rate of Substrate Util.'
          UNTS = '1/s'
          IUNS = -1
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &        ', 1/s)'
          ENDIF
!
!---    Liu's lognormal multi rate type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.41 ) THEN
!
!---      Read rate from lognormal distribution
!
          VARB = 'Liu Multi-Rate Kin. Reac.: ' //
     &      'Rate Constant'
          UNTS = '1/s'
          IUNMOL = 0
          IUNKG = 0
          IUNS = -1
          IUNM = 0
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &      ', 1/s)'
          ENDIF
!
!---      Read kinetic site density  ---
!
          VARB = 'Liu Multi-Rate Kin. Reac.: ' //
     &      'Kinetic Site Density'
          UNTS = 'mol/g'
          IUNMOL = 1
          IUNKG = -1
          IUNS = 0
          IUNM = 0
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
            DO 262 J = 1,LRC
!            
!---        Convert from kmol/m^3 to kmol/m^3 ---
!          
              RC_K(JCX,J,NRC) = 1.D+3*RC_K(JCX,J,NRC)
  262       CONTINUE
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
!
!---      Convert from kmol/m^3 to mol/m^3  ---
!
            RC_K(JCX,1,NRC) = 1.D+3*RC_K(JCX,1,NRC)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &      ', mol/m^3)'
          ENDIF
!
!---      Read pore ratio  ---
!
          VARB = 'Liu Multi-Rate Kin. Reac.: ' //
     &      'Pore Ratio.'
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &        ': ',RC_K(JCX,1,NRC)
          ENDIF
!
!---      Read logK1 for the first sorped species on the site  ---
!
          VARB = 'Liu Multi-Rate Kin. Reac.: ' //
     &      'Log K1.'
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &       ': ',RC_K(JCX,1,NRC)
          ENDIF
!
!---      Read logK2 for the second sorped species on the site ---
!
          VARB = 'Liu Multi-Rate Kin. Reac.: ' //
     &      'Log K2.'
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &       ': ',RC_K(JCX,1,NRC)
          ENDIF
!
!---    Liu's dual domain rate type reactions  ---
!
        ELSEIF( IRCKT(NRC).EQ.42 ) THEN
!
!---      Mass transfer rate
!
          VARB = 'Liu Dual-Domain Kin. Reac.: ' //
     &      'Rate Constant'
          UNTS = '1/s'
          IUNMOL = 0
          IUNKG = 0
          IUNS = -1
          IUNM = 0
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(JCX,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(JCX,1,NRC),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(JCX,1,NRC),
     &      ', 1/s)'
          ENDIF
!
!---      Read pore ratio (immobile pore/mobile pore) ---
!
          VARB = 'Liu Dual-Domain Kin. Reac.: ' //
     &      'Pore Ratio.'
          IDFLT = 1
          JCX = JCX + 1
!         RC_K(JCX,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = JCX
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(JCX,1,NRC))
          WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &        ': ',RC_K(JCX,1,NRC)
          ENDIF
        ENDIF
!
!---    Multi-rate mineral  ---
!
  400   CONTINUE
        IF( IRCKT(NRC).EQ.20 ) THEN
!
!---      Identify the mineral species  ---
!
          IRC_K(1,NRC) = IRC_KX
          ISP_MN(IRC_KX) = 1
!
!---      Number of mechanisms in kinetic reaction  ---
!
          VARB = 'Number of Mechanisms in Kinetic Reaction'
          CALL RDINT( ISTART,ICOMMA,CHDUM,IRC_K(2,NRC) )
          IF( IRC_K(2,NRC).GT.5 ) THEN
            INDX = 4
            CHMSG = 'Multirate Kinetics:  Number of Mechanisms > 5'
            CALL WRMSGS( INDX )
          ENDIF
          CALL RDINPL( CHDUM )
          CALL LCASE( CHDUM )
          ISTART = 1
!
!---      Neutral reference reaction rate  ---
!
          VARB = 'Neutral Reaction Reference Rate'
          UNTS = 'mol/m^2 s'
          IUNMOL = 1
          IUNM = -2
          IUNS = -1
          IDFLT = 1
!         RC_K(1,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = 1
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
            DO 264 J = 1,LRC
!            
!---        Convert from kmol/m^2 s to mol/m^2 s ---
!          
              RC_K(JCX,J,NRC) = 1.D+3*RC_K(1,J,NRC)
  264       CONTINUE
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(1,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(1,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(1,1,NRC),INDX)
!
!---      Convert from kmol/m^2 s to mol/m^2 s  ---
!
            RC_K(1,1,NRC) = 1.D+3*RC_K(1,1,NRC)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(1,1,NRC),
     &      ', mol/m^2 s)'
          ENDIF
!
!---      Neutral activation energy  ---
!
          VARB = 'Neutral Reaction Activation Energy'
          UNTS = 'j/mol'
          IUNMOL = -1
          IUNM = 2
          IUNKG = 1
          IUNS = -2
          IDFLT = 1
!         RC_K(2,NRC) = 0.D+0
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = 2
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
            DO 266 J = 1,LRC
!            
!---        Convert from J/kmol to J/mol ---
!          
              RC_K(JCX,J,NRC) = 1.D-3*RC_K(2,J,NRC)
  266       CONTINUE
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(2,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(2,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(2,1,NRC),INDX)
!
!---      Convert from J/kmol to J/mol  ---
!
            RC_K(2,1,NRC) = 1.D-3*RC_K(2,1,NRC)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(2,1,NRC),
     &      ', J/mol)'
          ENDIF
!
!---      Neutral reference reaction temperature  ---
!
          VARB = 'Neutral Reaction Reference Temperature'
          UNTS = 'c'
          IUNK = 1
          IDFLT = 1
          RC_K(3,1:LRC,NRC) = 1.D+20
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            INDX = 3
            CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
            IRCKN(JCX)=1
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(3,1,NRC))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',RC_K(3,1,NRC)
          INDX = 0
            CALL RDUNIT(UNTS,RC_K(3,1,NRC),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(3,1,NRC),
     &      ', C)'
          ENDIF
!
!---      Loop over mechanisms  ---
!
          DO 490 NKRM = 1,IRC_K(2,NRC)
            CALL RDINPL( CHDUM )
            CALL LCASE( CHDUM )
            ISTART = 1
!
!---        Mechanism reference reaction rate  ---
!
            VARB = 'Mechanism Kinetic Reaction Reference Rate'
            UNTS = 'mol/m^2 s'
            IUNMOL = 1
            IUNM = -2
            IUNS = -1
            IDFLT = 1
            IX = 4+((NKRM-1)*8)
!           RC_K(IX,NRC) = 0.D+0
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              INDX = IX
              CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
              IRCKN(JCX)=1
              DO 268 J = 1,LRC
!              
!---          Convert from kmol/m^2 s to mol/m^2 s ---
!            
                RC_K(JCX,J,NRC) = 1.D+3*RC_K(IX,J,NRC)
  268         CONTINUE
            ELSE

              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(IX,1,NRC))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',RC_K(IX,1,NRC)
            INDX = 0
              CALL RDUNIT(UNTS,RC_K(IX,1,NRC),INDX)
!
!---        Convert from kmol/m^2 s to mol/m^2 s  ---
!
              RC_K(IX,1,NRC) = 1.D+3*RC_K(IX,1,NRC)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(IX,1,NRC),
     &        ', mol/m^2 s)'
             ENDIF
!
!---        Mechanism activation energy  ---
!
            VARB = 'Mechanism Kinetic Reaction Activation Energy'
            UNTS = 'j/mol'
            IUNMOL = -1
            IUNM = 2
            IUNKG = 1
            IUNS = -2
            IDFLT = 1
            IX = 5+((NKRM-1)*8)
!            RC_K(IX,NRC) = 0.D+0
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              INDX = IX
              CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
              IRCKN(JCX)=1
              DO 272 J = 1,LRC
!              
!---          Convert from J/kmol to J/mol ---
!            
                RC_K(JCX,J,NRC) = 1.D-3*RC_K(IX,J,NRC)
  272         CONTINUE
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(IX,1,NRC))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',RC_K(IX,1,NRC)
            INDX = 0
              CALL RDUNIT(UNTS,RC_K(IX,1,NRC),INDX)
!
!---        Convert from J/kmol to J/mol  ---
!
              RC_K(IX,1,NRC) = 1.D-3*RC_K(IX,1,NRC)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(IX,1,NRC),
     &        ', J/mol)'
            ENDIF
!
!---        Mechanism reference reaction temperature  ---
!
            VARB = 'Mechanism Kinetic Reaction Reference Temperature'
            UNTS = 'c'
            IUNK = 1
            IDFLT = 1
            IX = 6+((NKRM-1)*8)
            RC_K(IX,1:LRC,NRC) = 1.D+20
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              INDX = IX
              CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
              IRCKN(JCX)=1
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(IX,1,NRC))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',RC_K(IX,1,NRC)
            INDX = 0
              CALL RDUNIT(UNTS,RC_K(IX,1,NRC),INDX)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',RC_K(IX,1,NRC),
     &        ', C)'
            ENDIF
!
!---        Number of species in mechanism  ---
!
            CALL RDINPL( CHDUM )
            CALL LCASE( CHDUM )
            ISTART = 1
            VARB = 'Number of Species in Mechanism'
            IX = 3+((NKRM-1)*6)
            CALL RDINT( ISTART,ICOMMA,CHDUM,IRC_K(IX,NRC) )
            IF( IRC_K(IX,NRC).GT.5 ) THEN
              INDX = 4
              CHMSG = 'Multirate Kinetics:  Number of Species > 5'
              CALL WRMSGS( INDX )
            ENDIF
!
!---        Loop over species  ---
!
            DO 450 NSP = 1,IRC_K(IX,NRC)
              IX = 3+((NKRM-1)*6)+NSP
              VARB = 'Mechanism Species Name: '
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---          Aqueous species, assign species index  ---
!
              DO 420 M = 1,NSPL
                IF( SPNML(M).EQ.ADUM ) THEN
                  IRC_K(IX,NRC) = M
                  GOTO 440
                ENDIF
  420         CONTINUE
!
!---          Solid species, assign species index  ---
!
              DO 430 M = 1,NSPS
                IF( SPNMS(M).EQ.ADUM ) THEN
                  IRC_K(IX,NRC) = NSPL + M
                  GOTO 440
                ENDIF
  430         CONTINUE
!
!---          Exchanged species, assign species index  ---
!
              DO 435 M = 1,NSPE
                IF( SPNME(M).EQ.ADUM ) THEN
                  IRC_K(IX,NRC) = NSPL + NSPS + M
                  GOTO 440
                ENDIF
  435         CONTINUE
              INDX = 4
              CHMSG = 'Unrecognized Mechanism Species Name: ' //
     &          ADUM(1:NCH)
              CALL WRMSGS( INDX )
  440         CONTINUE
              WRITE (IWR,'(/,2A)') ' Mechanism Species Name: ',
     &          ADUM
!
!---          Read kinetic reaction reactant stoichiometric 
!             coefficient  ---
!
              VARB = 'Mechanism Species Stoichiometric Coefficient: '
              IDFLT = 1
              IX = 6+((NKRM-1)*8)+NSP
              RC_K(IX,1:LRC,NRC) = 1.D+0
              IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
                INDX = IX
                CALL RDIJKC( ISTART,IJK,CHDUM,UNTS,RC_K,INDX,LNDX )
                IRCKN(JCX)=1
              ELSE
                CALL RDDPR(ISTART,ICOMMA,CHDUM,RC_K(IX,1,NRC))
              WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &              ': ',RC_K(IX,1,NRC)
              ENDIF
  450       CONTINUE
  490     CONTINUE
        ENDIF
!
!---    Read next kinetic reaction  ---
!
        IF( NRC.LT.NRCK ) WRITE(IWR,'(/)')
  500 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDKNRC group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDLITH
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
!     Read solid species lithology of rock/soil types.
!
!     RS_S(1,LSPS,LFDR) - initial mineral (solid species) area
!     RS_S(2,LSPS,LFDR) - initial mineral (solid species) vol. fraction
!     RS_S(3,LSPS,LFDR) - current mineral (solid species) vol. fraction
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 January 2005.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE PORMED
      USE GRID
      USE FILES
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
!----------------------Type Declarations-------------------------------!
!

      REAL*8, DIMENSION(:,:), ALLOCATABLE :: RS_SX



      CHARACTER*64 ADUM,UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDLITH'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'

      ALLOCATE( RS_SX(1:3,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RS_SX'
        CALL WRMSGS( INDX )
      ENDIF

!
!---  Write card information to ouput file  ---
!
      CARD = 'Lithology Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Loop over the rock/soil hydraulic information lines  ---
!
      N = 0
      IJK = 0
   10   CONTINUE
        IF( N.GE.NROCK .OR. IJK.GT.0 ) GOTO 500
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      VARB = 'Rock/Soil Name'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---  IJK, KIJ, or JKI indexing  ---
!
        IF( INDEX(ADUM(1:),'indexing').NE.0 ) THEN
          IF( INDEX(ROCK(1)(1:),'indexing').EQ.0 ) THEN
            INDX = 4
            CHMSG = 'Indexing Option Not Declared ' //
     &        'in Rock/Soil Zonation Card'
            CALL WRMSGS( INDX )
          ENDIF
          IF( INDEX(ADUM,'ijk').NE.0 ) THEN
            IJK = 1
          ELSEIF( INDEX(ADUM,'jki').NE.0 ) THEN
            IJK = 2
          ELSEIF( INDEX(ADUM,'kij').NE.0 ) THEN
            IJK = 3
          ELSE
            INDX = 4
            CHMSG = 'Unrecognized Indexing Option' // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          GOTO 220
        ENDIF
!
!---  Search known rock types for a matching type ---
!
        DO 100 M = 1,NROCK
           IF( ADUM.EQ.ROCK(M) ) THEN
          IROCK = M
              ISGRP = 0
          GOTO 200
        ENDIF
  100 CONTINUE
!
!---  Search known scaling groups for a matching type ---
!
      IF( ISLC(19).EQ.1 ) THEN
        DO 110 M = 1,NSCALE
           IF( ADUM.EQ.SCALNM(M) ) THEN
              ISGRP = M
              IROCK = 1
              GOTO 200
           ENDIF
  110   CONTINUE
        INDX = 2
        CHMSG = 'Unrecognized Rock/Soil Type or Scaling Group: '
     &    // ADUM(1:NCH)
        CALL WRMSGS( INDX )
          GOTO 10
      ENDIF
      INDX = 2
      CHMSG = 'Unrecognized Rock/Soil Type: ' // ADUM(1:NCH)
      CALL WRMSGS( INDX )
        GOTO 10
  200 CONTINUE
!
!---  Loop over rock/soils within scaling group  ---
!
      IF( ISLC(19).EQ.1 .AND. ISGRP.NE.0 ) THEN
          DO 202 M = IROCK,NROCK
          IF( ISCALE(M).EQ.ISGRP ) THEN
            IROCK = M
            GOTO 204
          ENDIF
  202   CONTINUE
      ENDIF
  204 CONTINUE
!
!---  Write rock/soil name  ---
!
      WRITE (IWR,'(/,2A)') 'Rock/Soil Name: ',ROCK(IROCK)
      N = N + 1
  220 CONTINUE
!
!---  Write rock/soil name  ---
!
      VARB = 'Number of Solid-Species Minerals'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NSPSX )
      IF( NSPSX.GT.NSPS ) THEN
        INDX = 5
        CHMSG = 'Number of Solid Species > Solid Species Count'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Loop over the solid species  ---
!
      DO 300 M = 1,NSPSX
        ISTART = 1
        CALL RDINPL( CHDUM )
         CALL LCASE( CHDUM )
        VARB = 'Lithology Species Name: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        DO 250 NSP = 1,NSPS
          IF( SPNMS(NSP).EQ.ADUM ) GOTO 260
  250   CONTINUE
        INDX = 4
        CHMSG = 'Unrecognized Solid-Species Mineral Name: ' //
     &    ADUM(1:NCH)
        CALL WRMSGS( INDX )
  260   CONTINUE
        WRITE (IWR,'(/,2A)') ' Solid-Species Mineral Name: ',
     &    ADUM(1:NCH)
!
!---    Read initial mineral (solid species) area  ---
!
        VARB = 'Initial Solid-Species Mineral Specific Area'
        UNTS = 'm^2/kg'
        IUNM = 2
        IUNKG = -1
        IF( IJK.GT.0 ) THEN
          INDX = 1
          LNDX = 3
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RS_SX,INDX,LNDX )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,RS_SX(1,IROCK))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',RS_SX(1,IROCK)
        INDX = 0
          CALL RDUNIT(UNTS,RS_SX(1,IROCK),INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',RS_SX(1,IROCK),', m^2/kg)'
        ENDIF
!
!---    Read initial mineral volume fraction ---
!
        VARB = 'Initial Solid-Species Mineral Volume Fraction'
        UNTS = 'null'
        INDX = NSP
        ISTX = ISTART
        IF( IJK.GT.0 ) THEN
          INDX = 2
          LNDX = 3
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RS_SX,INDX,LNDX )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,RS_SX(2,IROCK))
        WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &      RS_SX(2,IROCK)
        ENDIF
!
!---    Load current mineral volume fraction ---
!
        VARB = 'Current Solid-Species Mineral Volume Fraction'
        UNTS = 'null'
        ISTART = ISTX
        IF( IJK.GT.0 ) THEN
          INDX = 3
          LNDX = 3
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RS_SX,INDX,LNDX )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,RS_SX(3,IROCK))
        WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &      RS_SX(3,IROCK)
        ENDIF
!
!---    Restart overwrite option ---
!
        ISP_OWX = 0
        CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
        IF( INDX.EQ.1 ) THEN
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'overwrite').NE.0 ) ISP_OWX = 1
        ENDIF
!
!---    Map rock/soil types to nodes  ---
!
        DO 270 NX = 1,NFLD
          IF( IJK.GT.0 ) THEN
              RS_S(1,NSP,NX) = RS_SX(1,NX)
              RS_S(2,NSP,NX) = RS_SX(2,NX)
              RS_S(3,NSP,NX) = RS_SX(3,NX)
              ISP_OW(NSP,NX) = ISP_OWX
              POR0(3,NX) = POR0(3,NX) + RS_SX(2,NX)
          ELSE
            IF( IROCK.EQ.IZ(NX) ) THEN
              RS_S(1,NSP,NX) = RS_SX(1,IROCK)
              RS_S(2,NSP,NX) = RS_SX(2,IROCK)
              RS_S(3,NSP,NX) = RS_SX(3,IROCK)
              ISP_OW(NSP,NX) = ISP_OWX
              POR0(3,NX) = POR0(3,NX) + RS_SX(2,IROCK)
            ENDIF
          ENDIF
  270   CONTINUE
  300 CONTINUE
!
!---  Unreactive mineral volume fraction  ---
!
      DO 320 NX = 1,NFLD
        IF( IJK.GT.0 ) THEN
            POR0(3,NX) = 1.D+0 - POR(2,NX) - POR0(3,NX)
        ELSE
          IF( IROCK.EQ.IZ(NX) ) THEN
            POR0(3,NX) = 1.D+0 - POR(2,IROCK) - POR0(3,NX)
          ENDIF
        ENDIF
        IF( POR0(3,NX).GT.1.D+0 .OR. 
     &      POR0(3,NX).LT.-1.D-6 ) THEN
          INDX = 16
          CHMSG = 'Out of Range Unreactive Mineral Volume ' // 
     &      'Fraction at Node'
          IMSG = NX
          RLMSG = POR0(3,NX)
          CALL WRMSGS( INDX )
        ENDIF
  320 CONTINUE
!
!---  Read next rock/soil type or scaling group  ---
!
      IF( N.LT.NROCK ) WRITE(IWR,'(/)')
      GOTO 10
 500  CONTINUE

      DEALLOCATE( RS_SX,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RS_SX'
        CALL WRMSGS( INDX )
      ENDIF

      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDLITH group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDPTZR

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
!     This routine reads in the necessary data for the chemical  
!     equilibrium model. Two input files are required.  
!     Modified 03/13/07 by VL Freedman
!     to change inputread based on id. no. to species name.The comp.dat 
!     database contains the species charge atomic no. etc
!
!----------------------Authors-----------------------------------------!
!
!     Written by A. Felmy, from GMIN.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE PTZRCOEF
      USE PTZR
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64  INTMP

      REAL*8 TMIN(LSPL),TMAX(LSPL)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDPTZR'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
      LUN2 = 8
      OPEN(UNIT=LUN2,FILE='comp.dat',STATUS='OLD')
!
      DO  I = 1,NSPL
        IDD_PZ(I) = 0
        TMIN(I) = 0.0
        TMAX(I) = 0.0
      ENDDO
!       
!--- Assume temperature at first node in domain
!
      IKH2O = 0
      TX = T(2,1) + 273.15
!
!--- First aphi
!
      APHI=.336901532-6.3210043D-04*TX+9.14252359/TX-1.35143986D-02*  
     &      DLOG(TX)+2.26089488D-03/(TX-263.0)+1.92118597D-06*TX*TX
     &      +4.52586464D+01/(680.0-TX)

      IFOUND = 0
      NOTFOUND = 0
      I = 1

110   READ(LUN2,5009) IDTMP,INTMP,IZTMP

      CALL LCASE(INTMP)
!xyl
!
!--- Match in comp.dat is made based on name, not id no.
!--- Remove leading spaces from intmp in comp.dat.For minerals, 
!    switch to lower case.
!
!---  Eliminate trailing blank spaces  ---
!
       K = 64
 116   CONTINUE
       IF( ICHAR(SPNML(I)(K:K)).EQ.32 ) THEN
         K = K-1
         GOTO 116
       ENDIF
!
!--- Check to see if at end of comp.dat file.
!--- Check to see species which are not found in the comp.dat.
!--- ID number of notfound species are set to 299999<300000
      IF( IDTMP.LT.0 ) THEN
        WRITE(*,5020) SPNML(I)
        IDD_PZ(I) = 299999
        NOTFOUND = NOTFOUND+1
        IF( IFOUND+NOTFOUND.EQ.NSPL ) GO TO 120
        REWIND(LUN2)
        I=I+1
        GO TO 110
      END IF
!
!---  Eliminate leading blank spaces  ---
!
       J = 1
 115  CONTINUE
      IF( ICHAR(INTMP(J:J)).EQ.32 )THEN
        J = J+1
        GO TO 115
      END IF
!xyl
      IF( INTMP(J:20).EQ.SPNML(I)(1:20) ) THEN
         IFOUND=IFOUND+1
         IDD_PZ(I)=IDTMP
         IF(IDD_PZ(I).EQ.1080) IKH2O = I
      END IF
!  
!--- Now count cations anions and neutrals
!--- and set up pointers
!  
      IF( INTMP(J:20).EQ.SPNML(I)(1:20) ) THEN
        IF( I.GE.1 ) THEN
          IF( SP_L(1,I).GT.0 .AND. IDD_PZ(I).NE.101000 .AND.
     &       IDD_PZ(I).LT.300000 ) THEN
            NCC_PZ=NCC_PZ+1
            JPC(NCC_PZ)=I
            GO TO 155
          END IF
          IF( SP_L(1,I).LT.0.AND.IDD_PZ(I).LT.300000 ) THEN
            NA_PZ=NA_PZ+1
            JPA(NA_PZ)=I
            GO TO 155
          END IF
          IF( SP_L(1,I).EQ.0.0D0 .AND. I.LE.NSPL .AND. 
     &      IDD_PZ(I).LT.300000 ) THEN
            NNN_PZ = NNN_PZ+1
            JPN(NNN_PZ)=I
          END IF
        END IF
  
  155   CONTINUE
  
        IF( IFOUND+NOTFOUND.EQ.NSPL ) GO TO 120
        REWIND(LUN2)
        I=I+1
        GO TO 110
      END IF

      GO TO 110

120   CONTINUE

      IF( IKH2O.EQ.0 ) THEN
        CHMSG = 'H2O Must Be Included As Species in Pitzer Formulation '
        INDX = 1
        IMSG = INDX
        CALL WRMSGS( INDX )
      ENDIF

      CALL PTZRP

      CLOSE(LUN2)

5001  FORMAT(4X,'ID',6X,'NAME',17X,'Z',3X,'tmin',4X,'tmax')
5003  FORMAT(10X,'AQUEOUS SPECIES'/)
5004  FORMAT(/,10X,'SOLID PHASES'/)
5009  FORMAT(I6,A20,1X,I2)
5011  FORMAT(1X,I6,5X,A20,F3.0,F7.3,1X,F7.3,1x,F7.3)
5020  FORMAT(1X,'Species not found in data (comp.dat) file:  ',A20)
5031  FORMAT(/12X,'t = ',f7.2,1X,'aphi = ',F7.4/)
6040  FORMAT(/' ** TOO MANY SPECIES **')
6045  FORMAT(/' ** TOO MANY CATIONS **')
6050  FORMAT(/' ** TOO MANY ANIONS  **')
6055  FORMAT(/' ** TOO MANY NEUTRALS *')

!
!---  End of RDPTZR  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END 

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDSDSP
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
!     Read solid species.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 December 2004.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FILES
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
      CHARACTER*64 ADUM,UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDSDSP'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Write card information to ouput file  ---
!
      CARD = 'Solid Species Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of solid species  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Solid Species'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NSPS )
      IF( NSPS.GT.LSPS ) THEN
        INDX = 5
        CHMSG = 'Number of Solid Species > Parameter LSPS'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Loop over the solid species  ---
!
      DO 500 NSP = 1,NSPS
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Solid Species Name: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,SPNMS(NSP))
        DO 100 M = 1,NSP-1
          IF( SPNMS(M).EQ.SPNMS(NSP) ) THEN
            INDX = 4
            CHMSG = 'Duplicate Solid Species Name: ' // SPNMS(NSP)
            CALL WRMSGS( INDX )
          ENDIF
  100   CONTINUE
        DO 110 M = 1,NSPL
          IF( SPNMS(NSP).EQ.SPNML(M) ) THEN
            INDX = 4
            CHMSG = 'Solid Species Name = Aqueous Species Name: ' //
     &        SPNMS(NSP)
            CALL WRMSGS( INDX )
          ENDIF
  110   CONTINUE
        WRITE (IWR,'(/,2A)') ' Solid Species Name: ',SPNMS(NSP)
!
!---    Read solid species mass density  ---
!
        VARB = 'Solid Species Mass Density'
        UNTS = 'kg/m^3'
        IUNM = -3
        IUNKG = 1
        IDFLT = 1
        SP_S(1,NSP) = 2.65D+3
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SP_S(1,NSP))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',SP_S(1,NSP)
        INDX = 0
        CALL RDUNIT(UNTS,SP_S(1,NSP),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',SP_S(1,NSP),', kg/m^3)'
!
!---    Read solid species molecular weight  ---
!
        VARB = 'Solid Species Molecular Weight'
        UNTS = 'g/mol'
        IUNMOL = -1
        IUNKG = 1
        IDFLT = 1
        SP_S(2,NSP) = 0.D+0
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SP_S(2,NSP))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',SP_S(2,NSP)
        INDX = 0
        CALL RDUNIT(UNTS,SP_S(2,NSP),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',SP_S(2,NSP),', g/mol)'
!
!---  Read next solid species  ---
!
        IF( NSP.LT.NSPS ) WRITE(IWR,'(/)')
  500 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDSDSP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDSPLK
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
!     Link reactive species with pH and multifluid flow components.
!
!     ISPLK(1) <=> Aqueous pH
!     ISPLK(2) <=> Aqueous H2O
!     ISPLK(3) <=> Gas H2O
!     ISPLK(4) <=> Aqueous Air
!     ISPLK(5) <=> Gas Air
!     ISPLK(6) <=> Aqueous CO2
!     ISPLK(7) <=> Gas CO2
!     ISPLK(8) <=> Aqueous CH4
!     ISPLK(9) <=> Gas CH4
!     ISPLK(10) <=> Aqueous NaCl
!     ISPLK(11) <=> Solid NaCl
!     ISPLK(12) <=> Aqueous Oil
!     ISPLK(13) <=> NAPL Oil
!     ISPLK(14) <=> Gas Oil
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 21 December 2004.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE FILES
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
      CHARACTER*64 ADUM,UNTS,SPNMX
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDSPLK'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Write card information to ouput file  ---
!
      CARD = 'Reactive Species Link Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of reactive species links  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Reactive Species Links'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NSPLK )
!
!---  Loop over the number of reactive species links  ---
!
      DO 500 NSP = 1,NSPLK
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Reactive Species Name: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,SPNMX)
        NSPX = 0
        NSLX = 0
!
!---    Aqueous species, assign species index  ---
!
        DO 100 M = 1,NSPL
          IF( SPNML(M).EQ.SPNMX ) THEN
            NSPX = M
            GOTO 200
          ENDIF
  100   CONTINUE
!
!---    Solid species, assign species index  ---
!
        DO 110 M = 1,NSPS
          IF( SPNMS(M).EQ.SPNMX ) THEN
            NSPX = NSPL + M
            GOTO 200
          ENDIF
  110   CONTINUE
!
!---    Gas species, assign species index  ---
!
        DO 120 M = 1,NSPG
          IF( SPNMG(M).EQ.SPNMX ) THEN
            NSPX = NSPS + NSPL + M
            GOTO 200
          ENDIF
  120   CONTINUE
!
!---    Conservation component species,
!       assign negative solute index  ---
!
        DO 130 M = 1,NEQC
          IF( SPNMC(M).EQ.SPNMX ) THEN
            NSLX = NSOLU + M
            GOTO 200
          ENDIF
  130   CONTINUE
!
!---    Kinetic component species,
!       assign negative solute index  ---
!
        DO 140 M = 1,NEQK
          IF( SPNMK(M).EQ.SPNMX ) THEN
            NSLX = NSOLU + NEQC + M
            GOTO 200
          ENDIF
  140   CONTINUE
        INDX = 4
        CHMSG = 'Unrecognized Reactive Species Name: ' //
     &    SPNMX(1:NCH)
        CALL WRMSGS( INDX )
  200   CONTINUE
!
!---    Read reactive species link  ---
!
        VARB = 'Reactive Species Link: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:2),'ph').NE.0 ) THEN
          IF( NSPX.EQ.0 .AND. NSLX.GT.0 ) THEN
            INDX = 4
            CHMSG = 'pH Linked to Component Species: ' // SPNMX(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          ISPLK(1) = NSPX
          WRITE (IWR,'(2A)') ' Reactive Species Link: ' //
     &      'pH <=>',SPNMX
        ELSEIF( (IOM.EQ.30 .OR. IOM.EQ.40 .OR. IOM.EQ.43) .AND.
     &    (INDEX(ADUM(1:),'aqu').NE.0) ) THEN
          ISHIFTX=0
          IF( IOM.EQ.43 ) ISHIFTX=2
          DO IGC = 1,NGC+ISHIFTX
            NCHX = INDEX( GCNM(IGC)(1:),'  ' ) - 1
            IF( INDEX(ADUM(1:),GCNM(IGC)(1:NCHX)).NE.0 ) THEN
              IF( NSPX.GT.0 .AND. NSLX.EQ.0 ) THEN
                INDX = 4
                CHMSG = 'Aqueous ' // GCNM(IGC)(1:NCHX) // 
     &            ' Linked to Non-Conservation-Component Species: ' 
     &             // SPNMX(1:NCH)
                CALL WRMSGS( INDX )
              ENDIF
              ISPLK(14+NSPLK+IGC) = NSLX
              WRITE (IWR,'(2A)') ' Reactive Species Link: ' //
     &          'Aqueous ' // GCNM(IGC)(1:NCHX) // 
     &          ' <=>',SPNMX
            ENDIF
          END DO
        ELSEIF( (IOM.EQ.30 .OR. IOM.EQ.40 .OR. IOM.EQ.43) .AND.
     &    (INDEX(ADUM(1:),'gas').NE.0) ) THEN
          ISHIFTX=0
          IF( IOM.EQ.43 ) ISHIFTX=2
          DO IGC = 1,NGC+ISHIFTX
            NCHX = INDEX( GCNM(IGC)(1:),'  ' ) - 1
            IF( INDEX(ADUM(1:),GCNM(IGC)(1:NCHX)).NE.0 ) THEN
              IF( NSPX.GT.0 .AND. NSLX.EQ.0 ) THEN
              INDX = 4
              CHMSG = 'Gaseous ' // GCNM(IGC)(1:NCHX) //
     &          ' Linked to Non-Conservation-Component Species: ' 
     &          // SPNMX(1:NCH)
              CALL WRMSGS( INDX )
            ENDIF
            ISPLK(14+NSPLK+NGC+ISHIFTX+IGC) = NSLX
            WRITE (IWR,'(2A)') ' Reactive Species Link: ' //
     &        'Gaseous ' // GCNM(IGC)(1:NCHX) // 
     &        ' <=>',SPNMX
              ENDIF
          END DO
        ELSEIF( (INDEX(ADUM(1:),'h2o').NE.0 .OR.
     &    INDEX(ADUM(1:),'water').NE.0) .AND.
     &    INDEX(ADUM(1:),'aqu').NE.0 ) THEN
          ISPLK(2) = NSPX
          WRITE (IWR,'(2A)') ' Reactive Species Link: ' //
     &      'Aqueous H2O <=>',SPNMX
        ELSEIF( (INDEX(ADUM(1:),'h2o').NE.0 .OR.
     &    INDEX(ADUM(1:),'water').NE.0) .AND.
     &    INDEX(ADUM(1:),'gas').NE.0 ) THEN
          ISPLK(3) = NSPX
          WRITE (IWR,'(2A)') ' Reactive Species Link: ' //
     &      'Gas H2O <=>',SPNMX
        ELSEIF( INDEX(ADUM(1:),'air').NE.0 .AND.
     &    INDEX(ADUM(1:),'aqu').NE.0 ) THEN
          IF( NSPX.GT.0 .AND. NSLX.EQ.0 ) THEN
            INDX = 4
            CHMSG = 'Aqueous Air Linked to Non-Component Species: ' //
     &        SPNMX(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          ISPLK(4) = NSLX
          WRITE (IWR,'(2A)') ' Reactive Species Link: ' //
     &      'Aqueous Air <=>',SPNMX
        ELSEIF( INDEX(ADUM(1:),'air').NE.0 .AND.
     &    INDEX(ADUM(1:),'gas').NE.0 ) THEN
          ISPLK(5) = NSPX
          WRITE (IWR,'(2A)') ' Reactive Species Link: ' //
     &      'Gas Air <=>',SPNMX
        ELSEIF( (INDEX(ADUM(1:),'co2').NE.0 .OR.
     &    (INDEX(ADUM(1:),'carbon').NE.0 .AND.
     &     INDEX(ADUM(1:),'dioxide').NE.0)) .AND.
     &    INDEX(ADUM(1:),'aqu').NE.0 ) THEN
          IF( NSPX.GT.0 .AND. NSLX.EQ.0 ) THEN
            INDX = 4
            CHMSG = 'Aqueous CO2 Linked to Non-Component Species: ' //
     &        SPNMX(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          ISPLK(6) = NSLX
          WRITE (IWR,'(2A)') ' Reactive Species Link: ' //
     &      'Aqueous CO2 <=>',SPNMX
        ELSEIF( (INDEX(ADUM(1:),'co2').NE.0 .OR.
     &    (INDEX(ADUM(1:),'carbon').NE.0 .AND.
     &     INDEX(ADUM(1:),'dioxide').NE.0)) .AND.
     &    INDEX(ADUM(1:),'gas').NE.0 ) THEN
          ISPLK(7) = NSPX
          WRITE (IWR,'(2A)') ' Reactive Species Link: ' //
     &      'Gas CO2 <=>',SPNMX
        ELSEIF( (INDEX(ADUM(1:),'ch4').NE.0 .OR.
     &    INDEX(ADUM(1:),'methane').NE.0) .AND.
     &    INDEX(ADUM(1:),'aqu').NE.0 ) THEN
          ISPLK(8) = NSPX
          WRITE (IWR,'(2A)') ' Reactive Species Link: ' //
     &      'Aqueous CH4 <=>',SPNMX
        ELSEIF( (INDEX(ADUM(1:),'ch4').NE.0 .OR.
     &    INDEX(ADUM(1:),'methane').NE.0) .AND.
     &    INDEX(ADUM(1:),'gas').NE.0 ) THEN
          ISPLK(9) = NSPX
          WRITE (IWR,'(2A)') ' Reactive Species Link: ' //
     &      'Gas CH4 <=>',SPNMX
        ELSEIF( (INDEX(ADUM(1:),'nacl').NE.0 .OR.
     &    INDEX(ADUM(1:),'salt').NE.0) .AND.
     &    INDEX(ADUM(1:),'aqu').NE.0 ) THEN
          ISPLK(10) = NSPX
          WRITE (IWR,'(2A)') ' Reactive Species Link: ' //
     &      'Aqueous NaCl <=>',SPNMX
        ELSEIF( INDEX(ADUM(1:),'concentration').NE.0 ) THEN 
          ISPLK(14+NSP) = -NSPX
          WRITE (IWR,'(2A)') ' Reactive Species Link: ' //
     &      SPNMX, ' Concentration fixed '
        ELSEIF( INDEX(ADUM(1:),'activity').NE.0 ) THEN
          ISPLK(14+NSP) = -1000-NSPX
          WRITE (IWR,'(2A)') ' Reactive Species Link: ' //
     &      SPNMX, ' Activity fixed '
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Reactive Species Link: ' //
     &      ADUM(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
!
!---  Read next reactive species link  ---
!
  500 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDSPLK group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RESET_SP
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Reset reactive-species concentrations with old time-step
!     component-species concentrations.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 08 January 2013.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
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
      SUB_LOG(ISUB_LOG) = '/RESET_SP'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Loop over active nodes  ---
!
      DO 100 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 100
        DO 20 NSP = 1,NSPR
          SP_C(N,NSP) = SP_CO(N,NSP)
   20   CONTINUE
  100 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RESET_SP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RMNSP
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Reconstitute mineral species concentrations.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 2 May 2006.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
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
      SUB_LOG(ISUB_LOG) = '/RMNSP'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Loop over active nodes  ---
!
      DO 100 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 100
!
!---    Loop over solid species  ---
!
        DO 10 NSPX = 1,NSPS
          NSP = NSPL + NSPX
!
!---      Mineral species  ---
!
          IF( ISP_MN(NSP).EQ.1 ) THEN
            SP_C(N,NSP) = SP_C(N,NSP) + SP_CMN(N,NSPX)
          ENDIF
   10   CONTINUE
  100 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RMNSP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SEQEQ
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
!     Sequence reaction equations to assure non-zero diagonals.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 15 December 2004.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE FILES
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
      INTEGER ISPX(LSPR)
      CHARACTER*64 GETSPNM
      EXTERNAL GETSPNM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/SEQEQ'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Define a pseudo card  ---
!
      CARD = 'Equilibrium-Kinetic-Conservation Equation Cards'
      ICD = INDEX( CARD,'  ' )-1
!
!---  Check that the number of reaction equations (i.e., equilibrium,
!     kinetic, and conservation equations equals the number of reactive
!     species  ---
!
      NEQR = NEQE + NEQK + NEQC
      NSPR = NSPG + NSPL + NSPN + NSPS + NSPE
      IF( NEQR.NE.NSPR ) THEN
        INDX = 4
        CHMSG = 'Number of Equations != Number of Species'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Initialize species and equation counters  ---
!
      DO 10 NSP = 1,NSPR
        ISPX(NSP) = 0
   10 CONTINUE
!
!---  Count number of times species appear in equations  ---
!
      DO 30 NEQ = 1,NEQE
        DO 20 NSP = 2,IEQ_E(1,NEQ)+1
          I = IEQ_E(NSP,NEQ)
          ISPX(I) = ISPX(I) + 1
   20   CONTINUE
   30 CONTINUE
      DO 50 NEQ = 1,NEQC
        DO 40 NSP = 2,IEQ_C(1,NEQ)+1
          I = IEQ_C(NSP,NEQ)
          ISPX(I) = ISPX(I) + 1
   40   CONTINUE
   50 CONTINUE
      DO 70 NEQ = 1,NEQK
        DO 60 NSP = 2,IEQ_K(1,NEQ)+1
          I = IEQ_K(NSP,NEQ)
          ISPX(I) = ISPX(I) + 1
   60   CONTINUE
   70 CONTINUE
!
!---  Check for species that never appear in a
!     reactive equation  ---
!
      IMNX = NSPR
      DO 100 NSP = 1,NSPR
        IMNX = MIN( IMNX,ISPX(NSP) )
        IF( ISPX(NSP).EQ.0 ) THEN
          INDX = 4
          CHMSG = 'Species Not Found in Reactive Equations: ' //
     &      GETSPNM(NSP)
          CALL WRMSGS( INDX )
        ENDIF
  100 CONTINUE
!
!---  Associate conservation equations with their component species  ---
!
      DO 110 NEQ = 1,NEQC
        NSP = IEQ_C(2,NEQ)
        NC = NEQ + NEQE
        IEQ_S(NSP) = NC
        ISP_S(NC) = NSP
  110 CONTINUE
      GOTO 300
!
!---  Correlate species and equations for species that appear
!     in a single reactive equation  ---
!
  200 CONTINUE
!
!---  Loop over all species  ---
!
      DO 290 NSP = 1,NSPR
        IF( ISPX(NSP).EQ.IMNX ) THEN
          NSPMN = NSPR
!
!---      Loop over equilibrium equations  ---
!
          DO 230 NEQ = 1,NEQE
            NC = NEQ
            DO 220 N = 2,IEQ_E(1,NEQ)+1
!
!---          If the equilibrium equation contains the specie,
!             is unassigned, and contains the fewest number of species,
!             then assign equilibrium equation to species, using
!             global equation indexing
!             (i.e., equilibrium-conservation-kinetic)  ---
!
              IF( NSP.EQ.IEQ_E(N,NEQ) .AND. ISP_S(NC).EQ.0 ) THEN
                IF( IEQ_E(1,NEQ).LE.NSPMN ) THEN
                  IEQ_S(NSP) = NC
                  NSPMN = IEQ_E(1,NEQ)
                  GOTO 230
                ENDIF
              ENDIF
  220       CONTINUE
  230     CONTINUE
!          DO 250 NEQ = 1,NEQC
!            NC = NC + 1
!            DO 240 N = 2,IEQ_C(1,NEQ)+1
!!
!!---          If the conservation equation contains the specie,
!!             is unassigned, and contains the fewest number of species,
!!             then assign conservation equation to species, using
!!             global equation indexing
!!             (i.e., equilibrium-conservation-kinetic)  ---
!!
!              IF( NSP.EQ.IEQ_C(N,NEQ) .AND. ISP_S(NC).EQ.0 ) THEN
!                IF( IEQ_C(1,NEQ).LT.NSPMN ) THEN
!                  IEQ_S(NSP) = NC
!                  NSPMN = IEQ_C(1,NEQ)
!                  GOTO 250
!                ENDIF
!              ENDIF
!  240       CONTINUE
!  250     CONTINUE
!
!---      Loop over kinetic equations  ---
!
          DO 270 NEQ = 1,NEQK
            NC = NEQ + NEQC + NEQE
            DO 260 N = 2,IEQ_K(1,NEQ)+1
!
!---          If the kinetic equation contains the specie,
!             is unassigned, and contains the fewest number of species,
!             then assign kinetic equation to species, using
!             global equation indexing
!             (i.e., equilibrium-conservation-kinetic)  ---
!
              IF( NSP.EQ.IEQ_K(N,NEQ) .AND. ISP_S(NC).EQ.0 ) THEN
                IF( IEQ_K(1,NEQ).LT.NSPMN ) THEN
                  IEQ_S(NSP) = NC
                  NSPMN = IEQ_K(1,NEQ)
                  GOTO 270
                ENDIF
              ENDIF
  260       CONTINUE
  270     CONTINUE
!
!---      Assign specie to equation with the fewest number
!         of species, regardless of equation type  ---
!
          IF( IEQ_S(NSP).GT.0 ) ISP_S(IEQ_S(NSP)) = NSP
        ENDIF
  290 CONTINUE
  300 CONTINUE
!
!---  Reinitialize species counter  ---
!
      DO 310 NSP = 1,NSPR
        ISPX(NSP) = 0
  310 CONTINUE
!
!---  Loop over equilibrium equations  ---
!
      DO 330 NEQ = 1,NEQE
        NC = NEQ
!
!---    Skip assigned equations  ---
!
        IF( ISP_S(NC).EQ.0 ) THEN
          DO 320 NSP = 2,IEQ_E(1,NEQ)+1
            I = IEQ_E(NSP,NEQ)
!
!---        Skip assigned species  ---
!
            IF( IEQ_S(I).EQ.0 ) THEN
              ISPX(I) = ISPX(I) + 1
            ENDIF
  320     CONTINUE
        ENDIF
  330 CONTINUE
!      DO 350 NEQ = 1,NEQC
!        NC = NC + 1
!!
!!---    Skip assigned equations  ---
!!
!        IF( ISP_S(NC).EQ.0 ) THEN
!          DO 340 NSP = 2,IEQ_C(1,NEQ)+1
!            I = IEQ_C(NSP,NEQ)
!!
!!---        Skip assigned species  ---
!!
!            IF( IEQ_S(I).EQ.0 ) THEN
!              ISPX(I) = ISPX(I) + 1
!            ENDIF
!  340     CONTINUE
!        ENDIF
!  350 CONTINUE
!
!---  Loop over kinetic equations  ---
!
      DO 370 NEQ = 1,NEQK
        NC = NEQ + NEQC + NEQE
!
!---    Skip assigned equations  ---
!
        IF( ISP_S(NC).EQ.0 ) THEN
          DO 360 NSP = 2,IEQ_K(1,NEQ)+1
            I = IEQ_K(NSP,NEQ)
!
!---        Skip assigned species  ---
!
            IF( IEQ_S(I).EQ.0 ) THEN
              ISPX(I) = ISPX(I) + 1
            ENDIF
  360     CONTINUE
        ENDIF
  370 CONTINUE
!
!---  Minimum species count in unassigned equations  ---
!
      IMNX = NSPR
      DO 380 NSP = 1,NSPR
        IF( ISPX(NSP).GT.0 ) IMNX = MIN( IMNX,ISPX(NSP) )
  380 CONTINUE
!
!---  Equation assignment not complete, continue search  ---
!
      IF( IMNX.LT.NSPR ) GOTO 200
!
!---  Check for unassigned species  ---
!
      DO 400 NSP = 1,NSPR
        IF( IEQ_S(NSP).EQ.0 ) THEN
          INDX = 4
          CHMSG = 'Unassigned Species: ' // GETSPNM(NSP)
          CALL WRMSGS( INDX )
        ENDIF
  400 CONTINUE
!
!---  Print species and equations  ---
!
      WRITE (IWR,'(//,3A,/)') ' ~ ',CARD(1:ICD),': '
      DO 410 NSP = 1,NSPR
        IF( IEQ_S(NSP).GT.0 .AND. IEQ_S(NSP).LE.NEQE ) THEN
          WRITE (IWR,'(2A,I3)') GETSPNM(NSP),
     &      ' => Equilibrium Equation # ',IEQ_S(NSP)
        ELSEIF( IEQ_S(NSP).LE.(NEQE+NEQC) ) THEN
          WRITE (IWR,'(2A,I3)') GETSPNM(NSP),
     &      ' => Conservation Equation # ',IEQ_S(NSP)-NEQE
        ELSEIF( IEQ_S(NSP).LE.(NEQE+NEQC+NEQK) ) THEN
          WRITE (IWR,'(2A,I3)') GETSPNM(NSP),
     &      ' => Kinetic Equation # ',IEQ_S(NSP)-NEQE-NEQC
        ELSE
          INDX = 4
          CHMSG = 'Species Not Assigned to a '
     &      // 'Conservation Equation: ' // GETSPNM(NSP)
          CALL WRMSGS( INDX )
        ENDIF
  410 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of SEQEQ group  ---
!
      RETURN
      END
      
!----------------------Function--------------------------------------!
!
      FUNCTION TFUNC(A,TX)
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
!     This subroutine calculates higher order electrostatic functions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by S. Yabusaki.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
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
      REAL*8 A(8)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/TFUNC'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
      TFUNC = A(1)+A(2)*TX+A(3)/TX+A(4)*LOG(TX)+A(5)/(TX-263)+A(6)*TX**2
     &         +A(7)/(680-TX)+A(8)/(TX-227)
!
!---  End of TFUNC ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END 

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE UPDTCHEM
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Load old reactive-species concentrations and
!     component-species concentrations.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, December 16, 2005.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
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
      SUB_LOG(ISUB_LOG) = '/UPDTCHEM'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Loop over active nodes  ---
!
      DO 100 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 100
        DO 10 NEQ = 1,NEQC+NEQK
          NSL = NEQ + NSOLU
          CO(N,NSL) = C(N,NSL)
   10   CONTINUE
        DO 20 NSP = 1,NSPR
          SP_CO(N,NSP) = SP_C(N,NSP)
   20   CONTINUE
  100 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of UPDTCHEM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ZLKSRC
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
!     Zero linked sources
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 6 June 2006.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOURC
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
      SUB_LOG(ISUB_LOG) = '/ZLKSRC'
      IF( INDEX(SVN_ID(285)(1:1),'$').EQ.0 ) SVN_ID(285) =
     & '$Id: eckechem.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Loop over all nodes, skipping inactive nodes  ---
!
      DO 100 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 100
!
!---    Zero linked CO2 source  ---
!
        SRCA(1,N) = 0.D+0
!
!---    Zero linked component source  ---
!
        ISHIFTX=0
        IF( IOM.EQ.43 ) ISHIFTX=2
        DO IGC=1,NGC+ISHIFTX
          SRCGC(IGC,1,N) = 0.D+0
        END DO
 100  CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ZLKSRC group  ---
!
      RETURN
      END

