!----------------------Program-----------------------------------------!
!
      PROGRAM STOMP
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
!     STOMP: Subsurface Transport Over Multiple Phases
!
!     Water Mode
!
!     This engineering program numerically simulates thermal
!     and hydrologic transport phenomena in variably saturated
!     subsurface environments, contaminated with a water immiscible
!     volatile organic compound.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.





!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE UCODE
      USE TRNSPT
      USE SOLTN



      USE REACT
      USE OUTPU
      USE JACOB
      USE GEOMECH
      USE FILES
      USE CONST
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!

!#ifdef omp
!#include "omp_lib.h"
!#endif
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      LOGICAL HALT,PLOT,RESTART

!
!----------------------Executable Lines--------------------------------!
!

!
!---  Read input file to determine memory requirements  ---
!
      CALL STEP
!
!---  Allocate memory  ---
!
      CALL ALLOC

      ISUB_LOG = 1
      SUB_LOG(1) = 'STOMP1'
      ICODE = 1
      IF( INDEX(SVN_ID(204)(1:1),'$').EQ.0 ) SVN_ID(204) =
     & '$Id: stomp_w.F 1081 2017-03-14 17:09:57Z d3c002 $' 

!
!---  Intialize variables in common blocks and open files  ---
!
      CALL INTLZ
!
!---  Print banner on screen and output file  ---
!
      CALL BANNER
!
!---  Read user input and restart files  ---
!
      CALL RDINPT1
!
!---  Create a node connection map  ---
!
      CALL CONNMAP
!
!---  Check for internal boundary surfaces and write connectivity
!     list file  --
!
      CALL CONNLST
!
!---  For geomechanics simulations create a finite-element node map  --
!
      IF( ISLC(50).NE.0 ) CALL CONNFEN
!
!---  For geomechanics simulations check and preprocess boundary
!     conditions, and set the reference volumetric stress from
!     the initial displacements stored in the restart file  ---
!
      IF( ISLC(50).NE.0 ) CALL CHK_GM







!
!---  Check thermodynamic and hydrologic initial states  ---
!
      CALL CHK1

!
!---  Sequence reaction equations  ---
!
      IF( ISLC(40).EQ.1 ) CALL SEQEQ

!
!---  Compute Jacobian matrix pointers  ---
!
      CALL JCBP
!
!---  For geomechanics simulations compute Jacobian matrix pointers  --
!
      IF( ISLC(50).NE.0 ) CALL JCBP_GM
!
!---  Compute primary variable increments  ---
!
      CALL INCRM1
!
!---  Saturation, relative permeability, porosity, and tortuosity  ---
!
      CALL SMC1
!
!---  Thermodynamic properties and equations of state  ---
!
      CALL TMPR1
!
!---  Initial hydrologic and thermodynamic properties on
!     boundary surfaces  ---
!
       CALL BCP1
!
!---  Compute initial solute concentrations  ---
!
      CALL CISC1

!
!---  Reactive transport  ---
!
      IF( ISLC(40).EQ.1 ) THEN
!
!---    Normalize mineral species for restart  ---
!
        IF( IEO.EQ.2 ) CALL NMNSP
!
!---    Convert initial reactive species concentrations to
!       node volume basis, mol/m^3  ---
!
        CALL FLHSP
!
!---    Temporarily store time stepping  ---
!
        DT_RST = DT
        DTI_RST = DTI
        TM_RST = TM
!
!---    Loop over number of conservation component species  ---
!
        DO 20 NEQ = 1,NEQC
          NSL = NEQ + NSOLU
!
!---      Mobile conservation component fractions   ---
!
          CALL MOBCF( NEQ )
!
!---      Add immobile conservation component fractions   ---
!
          CALL IMOBCF( NEQ )
!
!---    End of conservation component species transport  ---
!
   20   CONTINUE
!
!---    Loop over number of kinetic component species  ---
!
        DO 40 NEQ = 1,NEQK
          NSL = NEQ + NEQC + NSOLU
! 
!---      Mobile kinetic component fractions   ---
!
          CALL MOBKF( NEQ )
! 
!---      Add immobile kinetic component fractions   ---
!
          CALL IMOBKF( NEQ )
!
!---    End of conservation component species transport  ---
!
   40   CONTINUE
!
!---    Equilibrium-conservation-kinetic reaction chemistry   ---
!
        CALL ECKECHEM
!
!---    Reconstitute mineral species concentrations for initial
!       output  ---
!
        CALL RMNSP
      ENDIF
!
!---  Reset time stepping  ---
!
      DT = DT_RST
      DTI = DTI_RST
      TM = TM_RST

!
!---  Initialize SPLIB routines  ---
!
      IF( ILES.EQ.3 ) THEN
        INDX = -1
        CALL PSPLIB( 0,INDX )
      ENDIF

!
!---  Initialize geomechanics  ---
!
      IF( ISLC(50).NE.0 ) THEN
!
!---    Reference state porothermoelastic geomechanics; first call
!       to STATIC_GM eliminates reference boundary conditions  ---
!
        CALL STATIC_GM
!
!---    Load reference displacements at finite elment nodes  ---
!
        CALL LDDISP_GM
!
!---    Reference volumetric stresses at finite element centroids  ---
!
        IF( ISLC(50).LT.0 ) THEN
          INDX = 0
          CALL STRESSV_GM( INDX )
!
!---      Remove restart check for geomechanics options  ---
!
          ISLC(50) = ABS(ISLC(50))
        ENDIF
!
!---    Static porothermoelastic geomechanics  ---
!
        CALL STATIC_GM
!
!---    Set k iterate value of pore pressure and volumetric stress
!
        INDX = 2
        CALL PRESS_GM( INDX )
        CALL STRESSV_GM( INDX )
      ENDIF
!
!---  Compute initial fluxes on interior and boundary surfaces  ---
!
      ISVF = 1
      CALL DRCVL
      CALL BCF1
      ISVF = 2*ISVC+1
!
!---  Surface flux integrator for zero time step  ---
!
      DTX = DT
      DT = 0.D+0
      CALL SFIN
      DT = DTX
!
!---  New Time Step ---
!
  100 CONTINUE
!
!---  Load old time step arrays  ---
!
      CALL LDO1
!
!---  Load old time step arrays for the volumetric stress
!     and pore pressure  ---
!
      IF( ISLC(50).NE.0 ) THEN
        INDX = 1
        CALL LD_GM( INDX )
      ENDIF
!
!---  Update porosity in response to initial stress  ---
!
      IF( (NSTEP-NRST).EQ.0 .AND. ISLC(50).NE.0 ) CALL PORSTY_GM
!
!---  Compute trapping number  ---
!
      CALL TRPGL1
!
!---  Reference node(s) output  ---
!
      IF( MOD( (NSTEP-NRST),IFQS ).EQ.0 .OR.
     &  MOD( (NSTEP-NRST),IFQO ).EQ.0 ) CALL REFNOD

!
!---  Normalize mineral species concentrations after initial
!     output  ---
!
      IF( (NSTEP-NRST).EQ.0 ) CALL NMNSP

!
!---  End of initial conditions simulations  ---
!
      IF( IEO.EQ.3 ) THEN
        INDX = 1
        CHMSG = 'Simulation Stopped:  Initial Condition'
        CALL WRMSGS( INDX )
        GOTO 900
      ENDIF
!
!---  Stop simulation if simulation time exceeds limit  ---
!
      IF( ABS(TMMX-TM)/EPSL.LT.EPSL ) THEN
        INDX = 1
        CHMSG = 'Simulation Stopped:  Simulation Time Limit'
        CALL WRMSGS( INDX )
        GOTO 900
      ENDIF
!
!---  Stop simulation if file "stop_stomp" exists  ---
!
      INQUIRE( FILE="stop_stomp", EXIST=HALT )
      IF( HALT ) THEN
        OPEN( UNIT=19, FILE="stop_stomp" )
        CLOSE( UNIT=19, STATUS='DELETE' )
        INDX = 1
        CHMSG = 'Simulation Stopped:  User Interrupt'
        CALL WRMSGS( INDX )
        ISLC(18) = 0
        GOTO 900
      ENDIF

!
!---  Generate plot file if file "plot_stomp" exists  ---
!
      INQUIRE( FILE="plot_stomp", EXIST=PLOT )
      IF( PLOT ) THEN
        OPEN( UNIT=19, FILE="plot_stomp" )
        CLOSE( UNIT=19, STATUS='DELETE' )
        CALL WRPLOT
        IF( ISLC(18).LT.1 ) CALL WRRST        
      ENDIF
!
!---  Generate restart file if file "restart_stomp" exists  ---
!
      INQUIRE( FILE="restart_stomp", EXIST=RESTART )
      IF( RESTART ) THEN
        OPEN( UNIT=19, FILE="restart_stomp" )
        CLOSE( UNIT=19, STATUS='DELETE' )
        CALL WRRST        
      ENDIF

!
!---  Restart and plot file outputs  ---
!
      IF( ABS(TMPR-TM)/EPSL.LE.EPSL ) THEN
        CALL WRPLOT
        IF( ISLC(18).LT.1 ) CALL WRRST
      ENDIF






!
!---  Inverse output  ---
!
      IF( ISLC(20).EQ.1 .AND. ABS(TMOB-TM).LT.(1.D+1*EPSL) ) CALL WROBDA
!
!---  Compute the next time step and increment time step counter  ---
!
      DTSO = DT
      CALL TMSTEP
      IF( NSTEP.EQ.0 ) DTSO = DT
      NSTEP = NSTEP + 1
      IF( NSTEP-NRST.GT.MXSTEP ) THEN
        INDX = 1
        CHMSG = 'Simulation Stopped:  Time Step Limit'
        CALL WRMSGS( INDX )
        NSTEP = NSTEP - 1





        GOTO 900
      ENDIF
!
!---  No flow solution  ---
!
      IF( ISLC(47).EQ.1 ) THEN
        CALL BCP1
        GOTO 600
      ENDIF
      NTSR = 0
!
!---  Top of sequential flow and transport and geomechanics  ---
!
      K_GM(1) = 0
      K_GM(2) = 0
  190 CONTINUE
      K_GM(1) = K_GM(1) + 1
!
!---  Newton-Raphson iteration restart  ---
!
  200 CONTINUE
      NITER = 0
!
!---  Newton-Raphson iteration start  ---
!
  300 CONTINUE
      NITER = NITER + 1
      K_GM(2) = K_GM(2) + 1
!
!---  Compute boundary saturation, relative permeability, and
!     thermodynamic properties  ---
!
      CALL BCP1
!
!---  Compute source contributions  ---
!
      CALL SORC1
!
!---  Compute matrix source contributions for dual porosity soils  ---
!
      CALL DPOR1
!
!---  Compute aqueous-phase volumetric flux (interior surfaces)  ---
!
      CALL DRCVL
!
!---  Compute aqueous-phase volumetric flux (boundary surfaces)  ---
!
      CALL BCF1
!
!---  Zero Jacobian matrix  ---
!



      CALL JCBZ( ISVC,MUC,MLC,MKC )
!
!---  Load Jacobian matrix for the water equation
!     (zero flux boundary)  ---
!
      CALL JCBWL
!
!---  Modify the Jacobian matrix for boundary conditions  ---
!
      CALL BCJ1
!
!---  Linear equation solver  ---
!
      IF( ILES.EQ.1 ) THEN
        INDX = 0
        CALL BAND( ISVC,MUC,MLC,INDX )
      ELSEIF( ILES.EQ.3 ) THEN
        INDX = 0
        CALL PSPLIB( ISVC,INDX )

      ENDIF
!
!---  Update primary variables  ---
!
      CALL UPDT1
!
!---  Compute convergence from maximum relative residuals  ---
!
      CALL RSDL1
!
!---  Compute primary variable increments, saturation,
!     relative permeability, porosity, tortuosity,
!     thermodynamic properties for interior nodes,
!     except immediately after a new time step  ---
!
      CALL INCRM1
      CALL SMC1
      CALL TMPR1
      CALL DPOR1
      GOTO( 200,300,600,900 ) ICNV
  600 CONTINUE
!
!---  Solve geomechanics  ---
!
      IF( ISLC(50).NE.0 ) THEN
!
!---    Set k+1 iterate value of pore pressure  ---
!
        INDX = 3
        CALL PRESS_GM( INDX )
!
!---    Static porothermoelastic geomechanics  ---
!
        CALL STATIC_GM
!
!---    Convergence check for sequential coupled flow and transport
!       and geomechanics  ---
!
        CALL RSDL_GM
        IF( RSD_GM.GT.RSDM_GM(IEPD) ) THEN
!
!---      Load k level arrays for the volumetric stress
!         and pore pressure  ---
!
          INDX = 2
          CALL LD_GM( INDX )
!
!---      Update porosity for geomechical stress  ---
!
          CALL PORSTY_GM
          GOTO 190
        ENDIF
      ENDIF
!
!---  Compute aqueous-phase volumetric flux (interior surfaces)  ---
!
      ISVF = 1
      CALL DRCVL
!
!---  Compute aqueous-phase volumetric flux (boundary surfaces)  ---
!
      CALL BCF1
!
!---  Compute Local Courant Numbers  ---
!
      IF( ICRNT.EQ.1 ) CALL CRNTNB
      ISVF = 2*ISVC+1
!
!---  Beginning of transport equation solution  ---
!
      IF( IEQC.EQ.0 .AND. ISLC(40).EQ.0 ) GOTO 800
!
!---  Loop over number of solutes  ---
!
      DO 700 NSL = 1,NSOLU
!
!---  Courant number limiting  ---
!
        N_CRN(NSL) = 1
        IF( ISLC(17).NE.0 ) CALL CRN_LIM( NSL )
!
!---    Sub-time step loop  ---
!
        DO 690 NC = 1,N_CRN(NSL)
          IF( ISLC(17).NE.0 ) TM = MIN( TM+DT,TM_CRN )
!
!---      Compute solute mole fractions ---
!
          CALL SPRP1( NSL )
!
!---      Solute transport ---
!
          CALL TPORT1( NSL )
!
!---      Load old sub-time-step solute concentrations  ---
!
          IF( ISLC(17).NE.0 ) CALL UPDTCO( NSL )
!
!---    Bottom of sub-time step loop  ---
!
  690   CONTINUE
!
!---  Courant number limiting, reset time stepping  ---
!
        IF( ISLC(17).NE.0 ) THEN
          DT = DT_CRN
          DTI = DTI_CRN
          TM = TM_CRN
        ENDIF
!
!---  End of transport equation solution  ---
!
  700 CONTINUE

!
!---  Reactive transport  ---
!
      IF( ISLC(40).EQ.1 ) THEN
        N_CRN(NSOLU+1) = 1
        IF( ISLC(17).NE.0 ) CALL CRN_LIM( NSOLU+1 )
!
!---    Courant-limiting sub-time step loop  ---
!
        DO 792 NCR = 1,N_CRN(NSOLU+1)
          IF( ISLC(17).NE.0 ) TM = MIN( TM+DT,TM_CRN )
          DT_RST = DT
          DTI_RST = DTI
          TM_RST = TM
          TM = TM - DT
          N_RST = 1
  710     CONTINUE
          IF( N_RST.GT.16 ) THEN
            WRITE(ISC,'(A)') '          ---  ECKEChem ' // 
     &        'Sub-Time Step Reduction Limit Exceeded  ---'
            WRITE(IWR,'(A)') '          ---  ECKEChem ' // 
     &        'Sub-Time Step Reduction Limit Exceeded  ---'
            DT = DT_RST
            DTI = DTI_RST
            TM = TM_RST
            NSTEP = NSTEP-1
            TM = TM-DT
            DT = DTO
            CALL BCK_STP
            GOTO 900
          ENDIF
!
!---      ECKEChem sub-time step loop  ---
!
          DO 790 NC = 1,N_RST
            TM = TM + DT
!
!---        Loop over number of conservation component species  ---
!
            DO 730 NEQ = 1,NEQC
              NSL = NEQ + NSOLU
!
!---          Mobile conservation component fractions   ---
!
              CALL MOBCF( NEQ )
!
!---          Skip transport for immobile conservation component 
!             species   ---
!
              IF( IMMB(NEQ).EQ.1 ) GOTO 720
!
!---          Solute transport ---
!
              CALL TPORT1( NSL )
!
!---          Add immobile conservation component fractions   ---
!
  720         CONTINUE
              CALL IMOBCF( NEQ )
!
!---        End of conservation component species transport  ---
!
  730       CONTINUE
!
!---        Loop over number of kinetic component species  ---
!
            DO 750 NEQ = 1,NEQK
              NSL = NEQ + NEQC + NSOLU
! 
!---          Mobile kinetic component fractions   ---
!
              CALL MOBKF( NEQ )
!
!---          Skip transport for immobile conservation component 
!             species   ---
!
              IF( IMMB(NEQ+NEQC).EQ.1 ) GOTO 740
!
!---          Solute transport ---
!
              CALL TPORT1( NSL )
! 
!---          Add immobile kinetic component fractions   ---
!
  740         CONTINUE
              CALL IMOBKF( NEQ )
!
!---        End of conservation component species transport  ---
!
  750       CONTINUE
!
!---        Equilibrium-conservation-kinetic reaction chemistry   ---
!
            CALL ECKECHEM
            IF( ECKE_ER ) THEN
              CALL RESET_SP
              GOTO 710
            ENDIF
!
!---        Load old sub-time-step reactive species
!           concentrations and component species concentrations  ---
!
            IF( ISLC(17).NE.0 ) CALL UPDTCHEM
!
!---      Bottom of sub-time step loop  ---
!
  790     CONTINUE
!
!---      Reset time stepping  ---
!
          IF( N_RST.GT.1 ) THEN
            DT = DT_RST
            DTI = DTI_RST
            TM = TM_RST
          ENDIF
  792   CONTINUE
!
!---    Courant number limiting, reset time stepping  ---
!
        IF( ISLC(17).NE.0 ) THEN
          DT = DT_CRN
          DTI = DTI_CRN
          TM = TM_CRN
        ENDIF
      ENDIF

!
!---  Correct aqueous liquid density and viscosity for electrolyte
!     solute concentration  ---
!
      IF( ISLC(16).EQ.1 ) CALL ELC1

  800 CONTINUE






!
!---  Surface flux integrator  ---
!
      CALL SFIN
!
!---  Proceed to new time step  ---
!
      GOTO 100
!
!---  Write plot file, restart file, close files, and
!     terminate simulation  ---
!
  900 CONTINUE
      CALL WRPLOT
      IF( ISLC(18).LT.2 ) CALL WRRST
!
!---  Inverse output  ---
!
      IF( ISLC(20).EQ.1 ) THEN
        IF( ABS(TMOB-TM).LT.(1.D+1*EPSL) ) CALL WROBDA
        IF( FLG_EXT ) WRITE(IOBDEF,'(A,/)') 'END'
        IF( FLG_UNI ) WRITE(IOBDUF,'(A,/)') 'END'
      ENDIF
      WRITE(IWR,'(/,A)') '---  End of STOMP Simulation  ---'
      WRITE(ISC,'(/,A)') '---  End of STOMP Simulation  ---'
      CALL WRCVS

      STOP
!
!---  End of STOMP program  ---
!
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE BCF1
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
!     Water Mode
!
!     Compute boundary surface fluxes.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE POINTE
      USE JACOB
      USE GRID
      USE FLUXP
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
      REAL*8 BCX(LBCV)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/BCF1'
!
!---  Zero boundary fluxes  ---
!
!$OMP PARALLEL DO
!$OMP&  COPYIN(ISUB_LOG)
!$OMP&  DEFAULT(NONE)
!$OMP&  SHARED(NBC,IBCN,IBCD,WL,VL,UL,ISVF,NSZ,NSY,NSX,IFLD,IJFLD)
!$OMP&  PRIVATE(N,NPX,NPY,NPZ,NQX,NQY,NQZ)
      DO 70 NB = 1,NBC
        N = IBCN(NB)
        IF( IBCD(NB).EQ.-3 ) THEN
          DO 10 M = 1,ISVF
            WL(M,NSZ(N)) = 0.D+0
   10     CONTINUE
        ELSEIF( IBCD(NB).EQ.-2 ) THEN
          DO 20 M = 1,ISVF
            VL(M,NSY(N)) = 0.D+0
   20     CONTINUE
        ELSEIF( IBCD(NB).EQ.-1 ) THEN
          DO 30 M = 1,ISVF
            UL(M,NSX(N)) = 0.D+0
   30     CONTINUE
        ELSEIF( IBCD(NB).EQ.1 ) THEN
          DO 40 M = 1,ISVF
            UL(M,NSX(N)+1) = 0.D+0
   40     CONTINUE
        ELSEIF( IBCD(NB).EQ.2 ) THEN
          DO 50 M = 1,ISVF
            VL(M,NSY(N)+IFLD) = 0.D+0
   50     CONTINUE
        ELSEIF( IBCD(NB).EQ.3 ) THEN
          DO 60 M = 1,ISVF
            WL(M,NSZ(N)+IJFLD) = 0.D+0
   60     CONTINUE
        ENDIF
   70 CONTINUE
!$OMP END PARALLEL DO
!
!---  Loop over boundary conditions  ---
!
!$OMP PARALLEL DO
!$OMP&  COPYIN(ISUB_LOG)
!$OMP&  DEFAULT(NONE)
!$OMP&  SHARED(NBC,TM,NSTEP,NRST,EPSL,IBCIN,IBCC,BC,IBCM,LBCV,DT,
!$OMP&    IBCN,ID,JD,KD,NSX,NSY,NSZ,IBCD,IBCT,IEQW,ISVF,WL,VL,MPOS,
!$OMP&    PG,PL,RHORL,GRAV,UL,MNEG,IFLD,IJFLD)
!$OMP&  PRIVATE(TMZ,MB,BCX,TDBC,DTBC,TFBC,I,J,K,NPX,NPY,NPZ,NQX,
!$OMP&    NQY,NQZ,MP,HDGL,PEFX,MN)
      DO 200 NB = 1,NBC
        TMZ = TM
        IF( NSTEP-NRST.EQ.0 ) TMZ = TMZ*(1.D+0+EPSL)+EPSL
        MB = IBCIN(NB)
        IF( IBCC(NB).EQ.1 ) TMZ = MOD( TM,BC(1,IBCM(NB),MB) )
        IF( TMZ.LE.BC(1,1,MB) ) GOTO 200
        IF( IBCM(NB).EQ.1 ) THEN
          DO 80 N = 1,LBCV
            BCX(N) = BC(N,1,MB)
   80     CONTINUE
        ELSE
          DO 100 M = 2,IBCM(NB)
            IF( TMZ.LE.BC(1,M,MB) ) THEN
             TDBC = (BC(1,M,MB)-BC(1,M-1,MB))
             DTBC = MIN( BC(1,M,MB)-TMZ,DT )
             TFBC = (TMZ-5.D-1*DTBC-BC(1,M-1,MB))/TDBC
             DO 90 N = 1,LBCV
               BCX(N) = BC(N,M-1,MB) + TFBC*(BC(N,M,MB)-BC(N,M-1,MB))
   90        CONTINUE
             GOTO 105
            ENDIF
  100     CONTINUE
          GOTO 200
        ENDIF
  105   CONTINUE
        N = IBCN(NB)
        I = ID(N)
        J = JD(N)
        K = KD(N)
        NPZ = NSZ(N)
        NPY = NSY(N)
        NPX = NSX(N)
        NQX = NPX+1
        NQY = NPY+IFLD
        NQZ = NPZ+IJFLD
!
!---  Bottom boundary
!
        IF( IBCD(NB).EQ.-3 ) THEN
!
!---  Aqueous Neumann else Dirichlet, Saturated, Unit Gradient
!
          IF( IBCT(IEQW,NB).EQ.2 ) THEN
            DO 110 M = 1,ISVF
              WL(M,NPZ) = BCX(2)
  110       CONTINUE
          ELSEIF( IBCT(IEQW,NB).EQ.24 ) THEN
            DO 112 M = 1,ISVF
!              MP = MPOS(M)
!              HDGL = MAX( (PG(MP,N)-PL(MP,N))/RHORL/GRAV,1.D-14 )
!              PEFX = 1.D+0
!              IF( HDGL.GT.BCX(3) ) THEN
!                PEFX = (BCX(3)/HDGL)**4
!              ENDIF
!              WL(M,NPZ) = -ABS(BCX(2))*PEFX
              WL(M,NPZ) = BCX(2)
  112       CONTINUE
          ELSEIF( IBCT(IEQW,NB).NE.3 ) THEN
            CALL DRCVLB( N,NB )
          ENDIF
!
!---  South boundary
!
        ELSEIF( IBCD(NB).EQ.-2 ) THEN
!
!---  Aqueous Neumann else Dirichlet, Saturated, Unit Gradient
!
          IF( IBCT(IEQW,NB).EQ.2 ) THEN
            DO 120 M = 1,ISVF
              VL(M,NPY) = BCX(2)
  120       CONTINUE
          ELSEIF( IBCT(IEQW,NB).EQ.24 ) THEN
            DO 122 M = 1,ISVF
              MP = MPOS(M)
              HDGL = MAX( (PG(MP,N)-PL(MP,N))/RHORL/GRAV,1.D-14 )
              PEFX = 1.D+0
              IF( HDGL.GT.BCX(3) ) THEN
                PEFX = (BCX(3)/HDGL)**4
              ENDIF
              VL(M,NPY) = -ABS(BCX(2))*PEFX
  122       CONTINUE
          ELSEIF( IBCT(IEQW,NB).NE.3 ) THEN
            CALL DRCVLS( N,NB )
          ENDIF
!
!---  West boundary
!
        ELSEIF( IBCD(NB).EQ.-1 ) THEN
!
!---  Aqueous Neumann else Dirichlet, Saturated, Unit Gradient
!
          IF( IBCT(IEQW,NB).EQ.2 ) THEN
            DO 130 M = 1,ISVF
              UL(M,NPX) = BCX(2)
  130       CONTINUE
          ELSEIF( IBCT(IEQW,NB).EQ.24 ) THEN
            DO 132 M = 1,ISVF
              MP = MPOS(M)
              HDGL = MAX( (PG(MP,N)-PL(MP,N))/RHORL/GRAV,1.D-14 )
              PEFX = 1.D+0
              IF( HDGL.GT.BCX(3) ) THEN
                PEFX = (BCX(3)/HDGL)**4
              ENDIF
              UL(M,NPX) = -ABS(BCX(2))*PEFX
  132       CONTINUE
          ELSEIF( IBCT(IEQW,NB).NE.3 ) THEN
            CALL DRCVLW( N,NB )
          ENDIF
!
!---  East boundary
!
        ELSEIF( IBCD(NB).EQ.1 ) THEN
!
!---  Aqueous Neumann else Dirichlet, Saturated, Unit Gradient
!
          IF( IBCT(IEQW,NB).EQ.2 ) THEN
            DO 140 M = 1,ISVF
              UL(M,NQX) = BCX(2)
  140       CONTINUE
          ELSEIF( IBCT(IEQW,NB).EQ.24 ) THEN
            DO 142 M = 1,ISVF
              MN = MNEG(M)
              HDGL = MAX( (PG(MN,N)-PL(MN,N))/RHORL/GRAV,1.D-14 )
              PEFX = 1.D+0
              IF( HDGL.GT.BCX(3) ) THEN
                PEFX = (BCX(3)/HDGL)**4
              ENDIF
              UL(M,NQX) = ABS(BCX(2))*PEFX
  142       CONTINUE
          ELSEIF( IBCT(IEQW,NB).NE.3 ) THEN
            CALL DRCVLE( N,NB )
          ENDIF
!
!---  North boundary
!
        ELSEIF( IBCD(NB).EQ.2 ) THEN
!
!---  Aqueous Neumann else Dirichlet, Saturated, Unit Gradient
!
          IF( IBCT(IEQW,NB).EQ.2 ) THEN
            DO 150 M = 1,ISVF
              VL(M,NQY) = BCX(2)
  150       CONTINUE
          ELSEIF( IBCT(IEQW,NB).EQ.24 ) THEN
            DO 152 M = 1,ISVF
              MN = MNEG(M)
              HDGL = MAX( (PG(MN,N)-PL(MN,N))/RHORL/GRAV,1.D-14 )
              PEFX = 1.D+0
              IF( HDGL.GT.BCX(3) ) THEN
                PEFX = (BCX(3)/HDGL)**4
              ENDIF
              VL(M,NQY) = ABS(BCX(2))*PEFX
  152       CONTINUE
          ELSEIF( IBCT(IEQW,NB).NE.3 ) THEN
            CALL DRCVLN( N,NB )
          ENDIF
!
!---  Top boundary
!
        ELSEIF( IBCD(NB).EQ.3 ) THEN
!
!---  Aqueous Neumann else Dirichlet, Saturated, Unit Gradient
!
          IF( IBCT(IEQW,NB).EQ.2 ) THEN
            DO 160 M = 1,ISVF
              WL(M,NQZ) = BCX(2)
  160       CONTINUE
          ELSEIF( IBCT(IEQW,NB).EQ.24 ) THEN
            DO 162 M = 1,ISVF
!              MN = MNEG(M)
!              HDGL = MAX( (PG(MN,N)-PL(MN,N))/RHORL/GRAV,1.D-14 )
!              PEFX = 1.D+0
!              IF( HDGL.GT.BCX(3) ) THEN
!                PEFX = (BCX(3)/HDGL)**4
!              ENDIF
!              WL(M,NQZ) = ABS(BCX(2))*PEFX
              WL(M,NQZ) = BCX(2)
  162       CONTINUE
          ELSEIF( IBCT(IEQW,NB).NE.3 ) THEN
            CALL DRCVLT( N,NB )
          ENDIF
        ENDIF
  200 CONTINUE
!$OMP END PARALLEL DO
      ISUB_LOG = ISUB_LOG-1
!
!---  End of BCF1 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE BCJ1
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
!     Water Mode
!
!     Modify the Jacobian matrix for boundary conditions
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GRID
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
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/BCJ1'
!
!---  Loop over boundary conditions  ---
!
!$OMP PARALLEL DO
!$OMP&  COPYIN(ISUB_LOG)
!$OMP&  DEFAULT(NONE)
!$OMP&  SHARED(NBC,NSTEP,NRST,EPSL,IBCC,BC,IBCM,IBCN,IXP,NSX,NSY,NSZ,
!$OMP&    IBCD,IBCT,IEQW,TM,IBCIN,IFLD,IJFLD)
!$OMP&  PRIVATE(TMZ,MB,N,NPX,NPY,NPZ,NQX,NQY,NQZ)
      DO 100 NB = 1,NBC
        TMZ = TM
        IF( NSTEP-NRST.EQ.0 ) TMZ = TMZ*(1.D+0+EPSL)+EPSL
        MB = IBCIN(NB)
        IF( IBCC(NB).EQ.1 ) TMZ = MOD( TM,BC(1,IBCM(NB),MB) )
        IF( TMZ.LE.BC(1,1,MB) ) GOTO 100
        IF( IBCM(NB).GT.1 .AND. TMZ.GT.BC(1,IBCM(NB),MB) ) GOTO 100
        N = IBCN(NB)
        IF( IXP(N).EQ.0 ) GOTO 100
        NPX = NSX(N)
        NPY = NSY(N)
        NPZ = NSZ(N)
        NQX = NPX + 1
        NQY = NPY + IFLD
        NQZ = NPZ + IJFLD
!
!---  Bottom boundary  ---
!
        IF( IBCD(NB).EQ.-3 ) THEN
!
!---  Aqueous  ---
!
          IF( IBCT(IEQW,NB).NE.3 ) THEN
            CALL JCBWLB( N,NB,NPZ )
          ENDIF

!
!---  South boundary  ---
!
        ELSEIF( IBCD(NB).EQ.-2 ) THEN
!
!---  Aqueous  ---
!
          IF( IBCT(IEQW,NB).NE.3 ) THEN
            CALL JCBWLS( N,NB,NPY )
          ENDIF
!
!---  West boundary  ---
!
        ELSEIF( IBCD(NB).EQ.-1 ) THEN
!
!---  Aqueous  ---
!
          IF( IBCT(IEQW,NB).NE.3 ) THEN
            CALL JCBWLW( N,NB,NPX )
          ENDIF
!
!---  East boundary
!
        ELSEIF( IBCD(NB).EQ.1 ) THEN
!
!---  Aqueous  ---
!
          IF( IBCT(IEQW,NB).NE.3 ) THEN
            CALL JCBWLE( N,NB,NQX )
          ENDIF
!
!---  North boundary
!
        ELSEIF( IBCD(NB).EQ.2 ) THEN
!
!---  Aqueous  ---
!
          IF( IBCT(IEQW,NB).NE.3 ) THEN
            CALL JCBWLN( N,NB,NQY )
          ENDIF
!
!---  Top boundary
!
        ELSEIF( IBCD(NB).EQ.3 ) THEN
!
!---  Aqueous  ---
!
          IF( IBCT(IEQW,NB).NE.3 ) THEN
            CALL JCBWLT( N,NB,NQZ )
          ENDIF
        ENDIF
  100 CONTINUE
!$OMP END PARALLEL DO
      ISUB_LOG = ISUB_LOG-1
!
!---  End of BCJ1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE BCP1
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
!     Water Mode
!
!     Compute saturation, relative permeability and thermodynamic
!     properties for boundary surfaces.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE PTZR
      USE PORMED
      USE JACOB
      USE HYST
      USE GRID
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
      REAL*8 BCX(LBCV)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/BCP1'
!
!---  Assign values for initial condition type boundary conditions  ---
!
      IF( NSTEP-NRST.LE.1 .AND. NITER.LE.1 ) THEN
      DO 50 NB = 1,NBC
        MB = IBCIN(NB)
        IF( IBCT(IEQW,NB).EQ.12 ) THEN
          N = IBCN(NB)
          IF( IBCD(NB).EQ.-3 ) THEN
             DB = 0.5D+0*DZGF(N)
             NPZ = NSZ(N)
             GB = GRVZ(NPZ)*DB
          ELSEIF( IBCD(NB).EQ.-2 ) THEN
             DB = 0.5D+0*DYGF(N)*RP(ID(N))
             NPY = NSY(N)
             GB = GRVY(NPY)*DB
          ELSEIF( IBCD(NB).EQ.-1 ) THEN
             DB = 0.5D+0*DXGF(N)
             NPX = NSX(N)
             GB = GRVX(NPX)*DB
          ELSEIF( IBCD(NB).EQ.1 ) THEN
             DB = -0.5D+0*DXGF(N)
             NQX = NSX(N)+1
             GB = GRVX(NQX)*DB
          ELSEIF( IBCD(NB).EQ.2 ) THEN
             DB = -0.5D+0*DYGF(N)*RP(ID(N))
             NQY = NSY(N)+IFLD
             GB = GRVY(NQY)*DB
          ELSEIF( IBCD(NB).EQ.3 ) THEN
             DB = -0.5D+0*DZGF(N)
             NQZ = NSZ(N)+IJFLD
             GB = GRVZ(NQZ)*DB
          ENDIF
          IF( IBCT(IEQW,NB).EQ.12 ) PLB(1,NB) = PL(2,N) + RHOL(2,N)*GB
        ENDIF
   50 CONTINUE
      ENDIF
!
!---  Loop over boundary conditions  ---
!
!$OMP PARALLEL DO
!$OMP&  COPYIN(ISUB_LOG)
!$OMP&  DEFAULT(NONE)
!$OMP&  SHARED(NBC,TM,NSTEP,NRST,EPSL,IBCIN,IBCC,BC,IBCM,LBCV,DT,
!$OMP&    IBCT,IEQW,PLB,IBCN,IZ,IBCD,ID,JD,KD,NSX,NSY,NSZ,IFLD,IJFLD,
!$OMP&    GRVX,GRVY,GRVZ,DXGF,DYGF,DZGF,RP,IBCLL,JBCLL,KBCLL,ND,XP,
!$OMP&    YP,ZP,MBCLL,XE,YE,ZE,ISVC,PL,PG,T,VISL,PERM,RHOL,IXP,DZGP,
!$OMP&    DYGP,DXGP,RKL,RHORL,GRAV,SLB,RKLB,IPH,PGB,SGB,ZERO,PATM,
!$OMP&    PCMP,PORDB,PORTB,ISLC,TORLB,PVWB,RHOLB,VISLB,XLWB,RHOS,PCSL,
!$OMP&    CB,TB,NSL_ELC,ELC_DCF,ELC_VCF,POR,POR0,NBHG)
!$OMP&  PRIVATE(TMZ,MB,BCX,TDBC,DTBC,TFBC,IZN,IBD,I,J,K,NPX,NPY,NPZ,
!$OMP&    NQX,NQY,NQZ,DB,GB,I1X,J1X,K1X,NBS,XBS,YBS,ZBS,XBE,YBE,ZBE,
!$OMP&    DXX,DYX,DZX,PLX,PGX,TX,PCT,PCP,PCN,PCE,PCW,PCS,PCB,NA,
!$OMP&    HDGL,SGRMX,MX,INDX,ASLX,ASLMINX,ASGTX,PX,TORGX,TORNX,XVLB,
!$OMP&    XVSB,CLX)
      DO 400 NB = 1,NBC
        TMZ = TM
        IF( NSTEP-NRST.EQ.0 ) TMZ = TMZ*(1.D+0+EPSL)+EPSL
        MB = IBCIN(NB)
        IF( IBCC(NB).EQ.1 ) TMZ = MOD( TM,BC(1,IBCM(NB),MB) )
        IF( TMZ.LE.BC(1,1,MB) ) GOTO 400
!
!---  Assign local boundary condition variables  ---
!
        IF( IBCM(NB).EQ.1 ) THEN
          DO 80 N = 1,LBCV
            BCX(N) = BC(N,1,MB)
   80     CONTINUE
        ELSE
          DO 100 M = 2,IBCM(NB)
            IF( TMZ.LE.BC(1,M,MB) ) THEN
             TDBC = (BC(1,M,MB)-BC(1,M-1,MB))
             DTBC = MIN( BC(1,M,MB)-TMZ,DT )
             TFBC = (TMZ-BC(1,M-1,MB))/TDBC
             DO 90 N = 1,LBCV
               BCX(N) = BC(N,M-1,MB) + TFBC*(BC(N,M,MB)-BC(N,M-1,MB))
   90        CONTINUE
             IF( IBCT(IEQW,NB).EQ.2 ) THEN
               BCX(2) = BCX(2)-5.D-1*DTBC*(BC(2,M,MB)-BC(2,M-1,MB))/TDBC
             ENDIF
             GOTO 110
            ENDIF
  100     CONTINUE
          GOTO 400
        ENDIF
  110   CONTINUE
!
!---    Initial condition boundary condition  ---
!
        IF( IBCT(IEQW,NB).EQ.12 ) BCX(2) = PLB(1,NB)
        N = IBCN(NB)
        IZN = IZ(N)
        IBD = ABS(IBCD(NB))

        POR0(1,N) = POR0(1,N)
        POR0(2,N) = POR0(2,N)

!!
!!---  Check for constant boundary conditions (Dirichlet,
!!     hydraulic gradient, seepage face)  ---
!!
!        IF( IBCC(NB).EQ.1 ) THEN
!          IF( IBCT(IEQW,NB).EQ.2 ) GOTO 400
!          IF( IBCT(IEQW,NB).EQ.11 ) GOTO 400
!          IF( IBCT(IEQW,NB).EQ.17 ) GOTO 400
!        ENDIF
!
!---  Boundary Direction  ---
!
        I = ID(N)
        J = JD(N)
        K = KD(N)
        NPX = NSX(N)
        NPY = NSY(N)
        NPZ = NSZ(N)
        NQX = NPX+1
        NQY = NPY+IFLD
        NQZ = NPZ+IJFLD
        IF( IBCD(NB).EQ.-3 ) THEN
           DB = 0.5D+0*DZGF(N)
           GB = GRVZ(NPZ)*DB
        ELSEIF( IBCD(NB).EQ.-2 ) THEN
           DB = 0.5D+0*DYGF(N)*RP(ID(N))
           GB = GRVY(NPY)*DB
        ELSEIF( IBCD(NB).EQ.-1 ) THEN
           DB = 0.5D+0*DXGF(N)
           GB = GRVX(NPX)*DB
        ELSEIF( IBCD(NB).EQ.1 ) THEN
           DB = -0.5D+0*DXGF(N)
           GB = GRVX(NQX)*DB
        ELSEIF( IBCD(NB).EQ.2 ) THEN
           DB = -0.5D+0*DYGF(N)*RP(ID(N))
           GB = GRVY(NQY)*DB
        ELSEIF( IBCD(NB).EQ.3 ) THEN
           DB = -0.5D+0*DZGF(N)
           GB = GRVZ(NQZ)*DB
        ENDIF
!
!---    Update boundary pressure for x-y-z hydraulic gradient
!       or seepage face boundaries.
!
        IF( ABS(IBCT(IEQW,NB)).EQ.44 .OR. ABS(IBCT(IEQW,NB)).EQ.45) THEN
          I1X = IBCLL(MB)
          J1X = JBCLL(MB)
          K1X = KBCLL(MB)
          NBS = ND(I1X,J1X,K1X)
          XBS = XP(NBS)
          YBS = YP(NBS)
          ZBS = ZP(NBS)
          IF( MBCLL(MB).EQ.-1 ) THEN
            XBS = XE(1,NBS)
          ELSEIF( MBCLL(MB).EQ.1 ) THEN
            XBS = XE(2,NBS)
          ELSEIF( MBCLL(MB).EQ.-2 ) THEN
            YBS = YE(1,NBS)
          ELSEIF( MBCLL(MB).EQ.2 ) THEN
            YBS = YE(3,NBS)
          ELSEIF( MBCLL(MB).EQ.-3 ) THEN
            ZBS = ZE(1,NBS)
          ELSEIF( MBCLL(MB).EQ.3 ) THEN
            ZBS = ZE(5,NBS)
          ENDIF
          XBE = XP(N)
          YBE = YP(N)
          ZBE = ZP(N)
          IF( IBCD(NB).EQ.-1 ) THEN
            XBE = XE(1,N)
          ELSEIF( IBCD(NB).EQ.1 ) THEN
            XBE = XE(2,N)
          ELSEIF( IBCD(NB).EQ.-2 ) THEN
            YBE = YE(1,N)
          ELSEIF( IBCD(NB).EQ.2 ) THEN
            YBE = YE(3,N)
          ELSEIF( IBCD(NB).EQ.-3 ) THEN
            ZBE = ZE(1,N)
          ELSEIF( IBCD(NB).EQ.3 ) THEN
            ZBE = ZE(5,N)
          ENDIF
          DXX = XBE - XBS
          DYX = YBE - YBS
          DZX = ZBE - ZBS
        ENDIF
!
!---  Loop over secondary variable indices  ---
!
        DO 300 M = 2,ISVC+2
          PLX = PL(M,N)
          PGX = PG(2,N)
          TX = T(2,N)
!
!---  Aqueous Dirichlet  ---
!
          IF( IBCT(IEQW,NB).EQ.1 .OR. IBCT(IEQW,NB).EQ.12 ) THEN
            PLX = BCX(2)
!
!---  Aqueous Neumann  ---
!
          ELSEIF( IBCT(IEQW,NB).EQ.2 ) THEN
            PLX = PLX + BCX(2)*DB*VISL(M,N)/PERM(IBD,IZN)
     &        + RHOL(M,N)*GB
!
!---  Aqueous Zero Flux  ---
!
          ELSEIF( IBCT(IEQW,NB).EQ.3 ) THEN
            PLX = PLX + RHOL(M,N)*GB
!
!---  Aqueous Saturated  ---
!
          ELSEIF( IBCT(IEQW,NB).EQ.4 ) THEN
            PLX = PGX
!
!---  Aqueous Unit Gradient  ---
!
          ELSEIF( IBCT(IEQW,NB).EQ.5 ) THEN
            PLX = PLX
!
!---  Aqueous Free Gradient  ---
!
          ELSEIF( IBCT(IEQW,NB).EQ.6 ) THEN
            IF( IBCD(NB).EQ.-3 ) THEN
              IF( IXP(N+IJFLD).NE.0 ) THEN
                PCT = PL(1,N+IJFLD)
                PCP = PL(1,N)
                PLX = PCP + 0.5D+0*(PCP-PCT)*DZGF(N)/DZGP(NQZ)
              ELSE
                PLX = PLX + RHOL(M,N)*GB
              ENDIF
            ELSEIF( IBCD(NB).EQ.-2 ) THEN
              IF( IXP(N+IFLD).NE.0 ) THEN
                PCN = PL(1,N+IFLD)
                PCP = PL(1,N)
                PLX = PCP + 0.5D+0*(PCP-PCN)*DYGF(N)/DYGP(NQY)
              ELSE
                PLX = PLX + RHOL(M,N)*GB
              ENDIF
            ELSEIF( IBCD(NB).EQ.-1 ) THEN
              IF( IXP(N+1).NE.0 ) THEN
                PCE = PL(1,N+1)
                PCP = PL(1,N)
                PLX = PCP + 0.5D+0*(PCP-PCE)*DXGF(N)/DXGP(NQX)
              ELSE
                PLX = PLX + RHOL(M,N)*GB
              ENDIF
            ELSEIF( IBCD(NB).EQ.1 ) THEN
              IF( IXP(N-1).NE.0 ) THEN
                PCW = PL(1,N-1)
                PCP = PL(1,N)
                PLX = PCP + 0.5D+0*(PCP-PCW)*DXGF(N)/DXGP(NQX)
              ELSE
                PLX = PLX + RHOL(M,N)*GB
              ENDIF
            ELSEIF( IBCD(NB).EQ.2 ) THEN
              IF( IXP(N-IFLD).NE.0 ) THEN
                PCS = PL(1,N-IFLD)
                PCP = PL(1,N)
                PLX = PCP + 0.5D+0*(PCP-PCS)*DYGF(N)/DYGP(NQY)
              ELSE
                PLX = PLX + RHOL(M,N)*GB
              ENDIF
            ELSEIF( IBCD(NB).EQ.3 ) THEN
              IF( IXP(N-IJFLD).NE.0 ) THEN
                PCB = PL(1,N-IJFLD)
                PCP = PL(1,N)
                PLX = PCP + 0.5D+0*(PCP-PCB)*DZGF(N)/DZGP(NQZ)
              ELSE
                PLX = PLX + RHOL(M,N)*GB
              ENDIF
            ENDIF
!
!---  Aqueous Outflow  ---
!
          ELSEIF( IBCT(IEQW,NB).EQ.7 ) THEN
            PLX = BCX(2)
!
!---  Aqueous Free Boundary  ---
!
          ELSEIF( IBCT(IEQW,NB).EQ.22 ) THEN
            IF( IBCD(NB).EQ.-3 .AND. IXP(N+IJFLD).NE.0 ) THEN
              K = KD(N)
              NA = N+IJFLD
              PLX = PL(2,N) + 0.5D+0*(PL(2,N)-PL(2,NA))*DZGP(NPZ)/
     &          DZGP(NQZ)
            ELSEIF( IBCD(NB).EQ.-2 .AND. IXP(N+IFLD).NE.0 ) THEN
              J = JD(N)
              NA = N+IFLD
              PLX = PL(2,N) + 0.5D+0*(PL(2,N)-PL(2,NA))*DYGP(NPY)/
     &          DYGP(NQY)
            ELSEIF( IBCD(NB).EQ.-1 .AND. IXP(N+1).NE.0 ) THEN
              I = ID(N)
              NA = N+1
              PLX = PL(2,N) + 0.5D+0*(PL(2,N)-PL(2,NA))*DXGP(NPX)/
     &          DXGP(NQX)
            ELSEIF( IBCD(NB).EQ.1 .AND. IXP(N-1).NE.0 ) THEN
              I = ID(N)
              NA = N-1
              PLX = PL(2,N) + 0.5D+0*(PL(2,N)-PL(2,NA))*DXGP(NQX)/
     &          DXGP(NPX)
            ELSEIF( IBCD(NB).EQ.2 .AND. IXP(N-IFLD).NE.0 ) THEN
              J = JD(N)
              NA = N-IFLD
              PLX = PL(2,N) + 0.5D+0*(PL(2,N)-PL(2,NA))*DYGP(NQY)/
     &          DYGP(NPY)
            ELSEIF( IBCD(NB).EQ.3 .AND. IXP(N-IJFLD).NE.0 ) THEN
              K = KD(N)
              NA = N-IJFLD
              PLX = PL(2,N) + 0.5D+0*(PL(2,N)-PL(2,NA))*DZGP(NQZ)/
     &          DZGP(NPZ)
            ENDIF
            PLX = PGX
!
!---  Aqueous Hydraulic Gradient  ---
!
          ELSEIF( ABS(IBCT(IEQW,NB)).EQ.11 ) THEN
            IF( IBCT(IEQW,NB).EQ.-11 ) THEN
              PLX = BCX(2)
            ELSEIF( IBCT(IEQW,NB).EQ.11 ) THEN
              PLX = FNHGBL( NBHG(1,NB),NB,M )
            ENDIF
!
!---  Aqueous Seepage Face  ---
!
          ELSEIF( ABS(IBCT(IEQW,NB)).EQ.17 ) THEN
            IF( IBCT(IEQW,NB).EQ.-17 ) THEN
              PLX = MAX( PGX,BCX(2) )
            ELSEIF( IBCT(IEQW,NB).EQ.17 ) THEN
              PLX = MAX( PGX,FNHGBL( NBHG(1,NB),NB,M ) )
            ENDIF
!
!---  Aqueous Potential Evaporation  ---
!
          ELSEIF( ABS(IBCT(IEQW,NB)).EQ.24 ) THEN
!            HDGL = MAX( (PG(M,N)-PL(M,N))/RHORL/GRAV,1.D-14 )
!            PEFX = 1.D+0
!            IF( HDGL.GT.BCX(3) ) THEN
!              PEFX = (BCX(3)/HDGL)**4
!            ENDIF
!            IF( IBCD(NB).LT.0 ) THEN
!              PLX = PLX - ABS(BCX(2))*PEFX*DB*VISL(M,N)/PERM(IBD,IZN)
!     &          + RHOL(M,N)*GB
!            ELSE
!              PLX = PLX + ABS(BCX(2))*PEFX*DB*VISL(M,N)/PERM(IBD,IZN)
!     &          + RHOL(M,N)*GB
!            ENDIF
            IF( IBCD(NB).LT.0 ) THEN
              PLX = PLX - ABS(BCX(2))*DB*VISL(M,N)/
     &          (PERM(IBD,IZN)*RKL(IBD,M,N)) + RHOL(M,N)*GB
            ELSE
              PLX = PLX + ABS(BCX(2))*DB*VISL(M,N)/
     &          (PERM(IBD,IZN)*RKL(IBD,M,N)) + RHOL(M,N)*GB
            ENDIF
            HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
            IF( HDGL.GT.BCX(3) ) THEN
              PLX = PGX - BCX(3)*RHORL*GRAV
              IBCT(IEQW,NB) = -24
            ELSE
              IBCT(IEQW,NB) = 24
            ENDIF
!
!---      X-Y-Z Aqueous Hydraulic Gradient
!
          ELSEIF( ABS(IBCT(IEQW,NB)).EQ.44 ) THEN
            IF( IBCT(IEQW,NB).EQ.-44 ) THEN
              PLX = BCX(2)
            ELSEIF( IBCT(IEQW,NB).EQ.44 ) THEN
              PLX = FNHGBL( NBHG(1,NB),NB,M )
              PLX = PLX + DXX*BCX(LBCV-2) + DYX*BCX(LBCV-1) 
     &                  + DZX*BCX(LBCV)
            ENDIF
!
!---      X-Y-Z Aqueous Seepage Face
!
          ELSEIF( ABS(IBCT(IEQW,NB)).EQ.45 ) THEN
            IF( IBCT(IEQW,NB).EQ.-45 ) THEN
              PLX = MAX( PGX,BCX(2) )
            ELSEIF( IBCT(IEQW,NB).EQ.45 ) THEN
              PLX = FNHGBL( NBHG(1,NB),NB,M )
              PLX = PLX + DXX*BCX(LBCV-2) + DYX*BCX(LBCV-1) 
     &                  + DZX*BCX(LBCV)
              PLX = MAX( PGX,PLX )
            ENDIF
          ENDIF
!
!---  Secondary and primary boundary variables  ---
!
          SGRMX = 0.D+0
          MX = 2
          INDX = 1
          CALL KSP1( N,IZN,MX,PGX,PLX,SLB(M,NB),RKLB(1,M,NB),
     &      ASLX,ASLMINX,ASGTX,SGRMX,INDX,IPH(2,N),PGB(1,NB),
     &      PLB(1,NB),SLB(1,NB) )
          SGB(M,NB) = MAX( 1.D+0-SLB(M,NB),ZERO )
          PX = MAX( PGX,PLX )+PATM
          CALL PORSTY(N,PX,PCMP(N),PORDB(M,NB),PORTB(M,NB))
          IF( ISLC(3).EQ.1 ) CALL TORTU( IZN,SLB(M,NB),SGB(M,NB),ZERO,
     &      PORDB(M,NB),TORLB(M,NB),TORGX,TORNX )
!
!---  Convert pressure to absolute prior to computing physical
!     properties  ---
!
          PLX = PLX + PATM
          PGX = PGX + PATM
!
!---  Compute thermodynamic properties  ---
!
          CALL WATSP( T(2,N),PVWB(M,NB) )
          PX = MAX( PLX,PGX,PVWB(M,NB) )
          CALL WATLQD( T(2,N),PX,RHOLB(M,NB) )
          CALL WATLQV( T(2,N),PX,PVWB(2,NB),VISLB(M,NB) )
!
!---  Correct aqueous liquid density and viscosity for electrolyte
!     solute concentration  ---
!
          XLWB(M,NB) = 1.D+0
          IF( ISLC(16).EQ.1 ) THEN
            XVLB = SLB(2,NB)*PORDB(2,NB)
            XVSB = SLB(2,NB)*RHOS(IZN)*PCSL(1,IZN,NSL_ELC)*
     &        (1.D+0-PORTB(2,NB))
            CLX = CB(NB,NSL_ELC)/(XVSB+XVLB)
            XLWB(M,NB) = RHOLB(M,NB)
            CALL ELC_DEN( RHOLB(M,NB),CLX,ELC_DCF )
            XLWB(M,NB) = XLWB(M,NB)/RHOLB(M,NB)
            CALL ELC_VIS( VISLB(M,NB),CLX,ELC_VCF )
          ENDIF
          PLB(M,NB) = PLX - PATM
          PGB(M,NB) = PGX - PATM
          TB(M,NB) = TX
  300   CONTINUE
  400 CONTINUE
!$OMP END PARALLEL DO
      ISUB_LOG = ISUB_LOG-1
!
!---  End of BCP1 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CAP1( IZN,SLX,SGTX,CPGL,SLOX,CPGLO,IPHX )
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
!     Water Mode
!
!     Compute the gas/aqueous capillary pressure from the aqueous
!     saturation.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 19, 1997.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
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
      SUB_LOG(ISUB_LOG) = '/CAP1'
!
!---  van Genuchten saturation function
!
      IF( ISCHR(IZN).EQ.1 ) THEN
        CN = MAX( SCHR(3,IZN),SMALL )
        IF( SCHR(14,IZN).LE.ZERO ) THEN
          IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
            CM = 1.D+0 - 2.D+0/CN
          ELSE
            CM = 1.D+0 - 1.D+0/CN
          ENDIF
        ELSE
          CM = SCHR(14,IZN)
        ENDIF
        IF( SLX.GT.SCHR(4,IZN ) ) THEN
          SLP = (SLX-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
          HDGL = (((1.D+0/SLP)**(1.D+0/CM)-1.D+0)**
     &      (1.D+0/CN))/SCHR(1,IZN)
        ELSEIF( ISM(IZN).EQ.1 ) THEN
          HDGL = EXP((1.D+0-SLX/SCHR(4,IZN))*LOG(HDOD))
        ELSE
          HDGL = HDOD
          SLX = SCHR(4,IZN) + 1.D-6
        ENDIF
        HDGL = MAX( HDGL,1.D-14 )
  100   CONTINUE
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMP = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        DSMP = -SCHR(4,IZN)/(HDGL*LOG(HDOD))*REALX
        SLP = 1.D+0/((1.D+0 + (SCHR(1,IZN)*HDGL)**CN)**CM)
        DSLP = -CM*SCHR(1,IZN)*CN*((SCHR(1,IZN)*HDGL)**(CN-1.D+0))
     &  /((1.D+0 + (SCHR(1,IZN)*HDGL)**CN)**(CM+1.D+0))
        SLZ = SLP*(1.D+0-SMP) + SMP
        DSLZ = DSLP*(1.D+0-SMP) + DSMP*(1.D+0-SLP)
        F = SLX - SLZ
        DF = -DSLZ
        DH = -F/(DF+SMALL)
        IF( HDGL+DH.LT.0.D+0 ) DH = 6.D-1*DH
        HDGL = HDGL + DH
        IF( ABS(DH).GT.1.D-8 ) GOTO 100
        CPGL = HDGL*RHORL*GRAV
!
!---  Brooks and Corey saturation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.2 ) THEN
        CL = MAX( SCHR(3,IZN),SMALL )
        IF( SLX.GT.SCHR(4,IZN ) ) THEN
          SLP = (SLX-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
          HDGL = SCHR(1,IZN)*(1.D+0/SLP)**(1.D+0/CL)
        ELSEIF( ISM(IZN).EQ.1 ) THEN
          HDGL = EXP((1.D+0-SLX/SCHR(4,IZN))*LOG(HDOD))
        ELSE
          HDGL = HDOD
          SLX = SCHR(4,IZN) + 1.D-9
        ENDIF
        HDGL = MAX( HDGL,1.D-14 )
  200   CONTINUE
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMP = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        DSMP = -SCHR(4,IZN)*REALX/(HDGL*LOG(HDOD))
        SLP = (SCHR(1,IZN)/HDGL)**CL
        DSLP = -CL*(SCHR(1,IZN)/(HDGL**2))
     &    *(SCHR(1,IZN)/HDGL)**(CL-1.D+0)
        SLZ = SLP*(1.D+0-SMP) + SMP
        DSLZ = DSLP*(1.D+0-SMP) + DSMP*(1.D+0-SLP)
        F = SLX - SLZ
        DF = -DSLZ
        DH = -F/(DF+SMALL)
        IF( HDGL+DH.LT.0.D+0 ) DH = 6.D-1*DH
        HDGL = HDGL + DH
        IF( ABS(DH).GT.1.D-8 ) GOTO 200
        CPGL = HDGL*RHORL*GRAV
!
!---  Dual porosity van Genuchten saturation function
!
      ELSEIF( ISCHR(IZN).EQ.3 ) THEN
        CNM = MAX( SCHR(3,IZN),SMALL )
        IF( SCHR(14,IZN).LE.ZERO ) THEN
          IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
            CMM = 1.D+0 - 2.D+0/CNM
          ELSE
            CMM = 1.D+0 - 1.D+0/CNM
          ENDIF
        ELSE
          CMM = SCHR(14,IZN)
        ENDIF
        CNF = MAX( SCHR(6,IZN),SMALL )
        IF( SCHR(15,IZN).LE.ZERO ) THEN
          IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
            CMF = 1.D+0 - 2.D+0/CNF
          ELSE
            CMF = 1.D+0 - 1.D+0/CNF
          ENDIF
        ELSE
          CMF = SCHR(15,IZN)
        ENDIF
        PORDM = (1.D+0-POR(4,IZN))*POR(2,IZN)/
     &    ( POR(4,IZN) + (1.D+0-POR(4,IZN))*POR(2,IZN) + SMALL )
        PORDF = POR(4,IZN)/
     &    ( POR(4,IZN) + (1.D+0-POR(4,IZN))*POR(2,IZN) + SMALL )
        IF( SLX.GT.PORDM ) THEN
          IF( SLX.GT.SCHR(7,IZN ) ) THEN
            SLP = (SLX-SCHR(7,IZN))/(1.D+0-SCHR(7,IZN))
            HDGL = (((1.D+0/SLP)**(1.D+0/CMF)-1.D+0)**
     &        (1.D+0/CNF))/SCHR(5,IZN)
          ELSEIF( ISM(IZN).EQ.1 ) THEN
            HDGL = EXP((1.D+0-SLX/SCHR(7,IZN))*LOG(HDOD))
          ELSE
            HDGL = HDOD
            SLX = SCHR(7,IZN) + 1.D-6
          ENDIF
        ELSE
          IF( SLX.GT.SCHR(4,IZN ) ) THEN
            SLP = (SLX-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
            HDGL = (((1.D+0/SLP)**(1.D+0/CMM)-1.D+0)**
     &        (1.D+0/CNM))/SCHR(1,IZN)
          ELSEIF( ISM(IZN).EQ.1 ) THEN
            HDGL = EXP((1.D+0-SLX/SCHR(4,IZN))*LOG(HDOD))
          ELSE
            HDGL = HDOD
            SLX = SCHR(4,IZN) + 1.D-6
          ENDIF
        ENDIF
        HDGL = MAX( HDGL,1.D-14 )
  300   CONTINUE
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMPM = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        DSMPM = -SCHR(4,IZN)/(HDGL*LOG(HDOD))*REALX
        SLPM = 1.D+0/((1.D+0 + (SCHR(1,IZN)*HDGL)**CNM)**CMM)
        DSLPM = -CMM*SCHR(1,IZN)*CNM*((SCHR(1,IZN)*HDGL)**(CNM-1.D+0))
     &  /((1.D+0 + (SCHR(1,IZN)*HDGL)**CNM)**(CMM+1.D+0))
        SLZM = SLPM*(1.D+0-SMPM) + SMPM
        DSLZM = DSLPM*(1.D+0-SMPM) + DSMPM*(1.D+0-SLPM)
        SMPF = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        DSMPF = -SCHR(7,IZN)/(HDGL*LOG(HDOD))*REALX
        SLPF = 1.D+0/((1.D+0 + (SCHR(5,IZN)*HDGL)**CNF)**CMF)
        DSLPF = -CMF*SCHR(5,IZN)*CNF*((SCHR(5,IZN)*HDGL)**(CNF-1.D+0))
     &  /((1.D+0 + (SCHR(5,IZN)*HDGL)**CNF)**(CMF+1.D+0))
        SLZF = SLPF*(1.D+0-SMPF) + SMPF
        DSLZF = DSLPF*(1.D+0-SMPF) + DSMPF*(1.D+0-SLPF)
        F = SLX - SLZM*PORDM - SLZF*PORDF
        DF = -DSLZM*PORDM - DSLZF*PORDF
        DH = -F/(DF+SMALL)
        IF( HDGL+DH.LT.0.D+0 ) DH = 6.D-1*DH
        HDGL = MAX( HDGL+DH,1.D-14 )
        IF( ABS(DH).GT.1.D-8 ) GOTO 300
        CPGL = HDGL*RHORL*GRAV
!
!---  Dual Porosity Brooks and Corey saturation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.4 ) THEN
        CLM = MAX( SCHR(3,IZN),SMALL )
        CLF = MAX( SCHR(6,IZN),SMALL )
        PORDM = (1.D+0-POR(4,IZN))*POR(2,IZN)/
     &    ( POR(4,IZN) + (1.D+0-POR(4,IZN))*POR(2,IZN) + SMALL )
        PORDF = POR(4,IZN)/
     &    ( POR(4,IZN) + (1.D+0-POR(4,IZN))*POR(2,IZN) + SMALL )
        IF( SLX.GT.PORDM ) THEN
          IF( SLX.GT.SCHR(4,IZN ) ) THEN
            SLP = (SLX-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
            HDGL = SCHR(1,IZN)*(1.D+0/SLP)**(1.D+0/CLM)
          ELSEIF( ISM(IZN).EQ.1 ) THEN
            HDGL = EXP((1.D+0-SLX/SCHR(4,IZN))*LOG(HDOD))
          ELSE
            HDGL = HDOD
            SLX = SCHR(4,IZN) + 1.D-9
          ENDIF
        ELSE
          IF( SLX.GT.SCHR(7,IZN ) ) THEN
            SLP = (SLX-SCHR(7,IZN))/(1.D+0-SCHR(7,IZN))
            HDGL = SCHR(5,IZN)*(1.D+0/SLP)**(1.D+0/CLF)
          ELSEIF( ISM(IZN).EQ.1 ) THEN
            HDGL = EXP((1.D+0-SLX/SCHR(7,IZN))*LOG(HDOD))
          ELSE
            HDGL = HDOD
            SLX = SCHR(7,IZN) + 1.D-9
          ENDIF
        ENDIF
        HDGL = MAX( HDGL,1.D-14 )
  400   CONTINUE
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMPM = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        DSMPM = -SCHR(4,IZN)*REALX/(HDGL*LOG(HDOD))
        SLPM = (SCHR(1,IZN)/HDGL)**CLM
        DSLPM = -CLM*(SCHR(1,IZN)/(HDGL**2))
     &    *(SCHR(1,IZN)/HDGL)**(CLM-1.D+0)
        SLZM = SLPM*(1.D+0-SMPM) + SMPM
        DSLZM = DSLPM*(1.D+0-SMPM) + DSMPM*(1.D+0-SLPM)
        SMPF = MAX( (1.D+0-HSCL)*SCHR(7,IZN),ZERO )
        DSMPF = -SCHR(7,IZN)*REALX/(HDGL*LOG(HDOD))
        SLPF = (SCHR(5,IZN)/HDGL)**CLF
        DSLPF = -CLF*(SCHR(5,IZN)/(HDGL**2))
     &    *(SCHR(5,IZN)/HDGL)**(CLF-1.D+0)
        SLZF = SLPF*(1.D+0-SMPF) + SMPF
        DSLZF = DSLPF*(1.D+0-SMPF) + DSMPF*(1.D+0-SLPF)
        F = SLX - SLZM*PORDM - SLZF*PORDF
        DF = -DSLZM*PORDM - DSLZF*PORDF
        DH = -F/(DF+SMALL)
        IF( HDGL+DH.LT.0.D+0 ) DH = 6.D-1*DH
        HDGL = HDGL + DH
        IF( ABS(DH).GT.1.D-8 ) GOTO 400
        CPGL = HDGL*RHORL*GRAV
!
!---  Haverkamp saturation function  ---
!
      ELSEIF( ABS(ISCHR(IZN)).EQ.5 ) THEN
        IF( SLX.GT.SCHR(4,IZN ) ) THEN
          SLP = (SLX-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
          ALPHAX = SCHR(2,IZN)/SCHR(5,IZN)
          IF( ISCHR(IZN).EQ.-5 ) THEN
            HDGL = EXP((-(SLP*ALPHAX-ALPHAX)/SLP)**(1.D+0/SCHR(3,IZN)))*
     &        SCHR(5,IZN)
          ELSE
            HDGL = ((-(SLP*ALPHAX-ALPHAX)/SLP)**(1.D+0/SCHR(3,IZN)))*
     &        SCHR(5,IZN)
          ENDIF
        ELSEIF( ISM(IZN).EQ.1 ) THEN
          HDGL = EXP((1.D+0-SLX/SCHR(4,IZN))*LOG(HDOD))
        ELSE
          HDGL = HDOD
          SLX = SCHR(4,IZN) + 1.D-9
        ENDIF
        HDGL = MAX( HDGL,1.D-14 )
  500   CONTINUE
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMP = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        DSMP = -SCHR(4,IZN)*REALX/(HDGL*LOG(HDOD))
        ALPHAX = SCHR(2,IZN)/SCHR(5,IZN)
        IF( ISCHR(IZN).EQ.-5 ) THEN
          HDGLX = LOG(HDGL/SCHR(5,IZN))
          SLP = ALPHAX/(ALPHAX + (HDGLX**SCHR(3,IZN)))
          DSLP = -(ALPHAX*(HDGLX**SCHR(3,IZN))*
     &      (SCHR(3,IZN)/(HDGLX*HDGL/SCHR(5,IZN))))/
     &      ((ALPHAX+(HDGLX**SCHR(3,IZN)))**2)
        ELSE
          HDGLX = HDGL/SCHR(5,IZN)
          SLP = ALPHAX/(ALPHAX + (HDGLX**SCHR(3,IZN)))
          DSLP = -(ALPHAX*(HDGLX**SCHR(3,IZN))*(SCHR(3,IZN)/HDGLX))/
     &      ((ALPHAX+(HDGLX**SCHR(3,IZN)))**2)
        ENDIF
        SLZ = SLP*(1.D+0-SMP) + SMP
        DSLZ = DSLP*(1.D+0-SMP) + DSMP*(1.D+0-SLP)
        F = SLX - SLZ
        DF = -DSLZ
        DH = -F/(DF+SMALL)
        IF( HDGL+DH.LT.0.D+0 ) DH = 6.D-1*DH
        HDGL = HDGL + DH
        IF( ABS(DH).GT.1.D-8 ) GOTO 500
        CPGL = HDGL*RHORL*GRAV
!
!---  Russo saturation function
!
      ELSEIF( ISCHR(IZN).EQ.9 ) THEN
        IF( SLX.GT.SCHR(4,IZN ) ) THEN
          SLP = (SLX-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
          HDGL = (2.D+0/SCHR(1,IZN))*
     &      SQRT(1.D+0 - (SLP**(5.D-1*SCHR(3,IZN)+1.D+0)))
        ELSEIF( ISM(IZN).EQ.1 ) THEN
          HDGL = EXP((1.D+0-SLX/SCHR(4,IZN))*LOG(HDOD))
        ELSE
          HDGL = HDOD
          SLX = SCHR(4,IZN) + 1.D-6
        ENDIF
        HDGL = MAX( HDGL,1.D-14 )
  900   CONTINUE
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMP = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        DSMP = -SCHR(4,IZN)/(HDGL*LOG(HDOD))*REALX
        SLP = (EXP(-5.D-1*SCHR(1,IZN)*HDGL)*
     &    (1.D+0 + 5.D-1*SCHR(1,IZN)*HDGL))**(2.D+0/(SCHR(3,IZN)+2.D+0))
        DSLP = -((2.5D-1*EXP(-SCHR(1,IZN)*HDGL)*
     &   ((2.D+0+(SCHR(1,IZN)*HDGL))**2))**(1.D+0/(SCHR(3,IZN)+2.D+0)))*
     &   (SCHR(1,IZN)**2)*(HDGL/((SCHR(3,IZN)+2.D+0)*
     &   (2.D+0*SCHR(1,IZN)*HDGL)))
        SLZ = SLP*(1.D+0-SMP) + SMP
        DSLZ = DSLP*(1.D+0-SMP) + DSMP*(1.D+0-SLP)
        F = SLX - SLZ
        DF = -DSLZ
        DH = -F/(DF+SMALL)
        IF( HDGL+DH.LT.0.D+0 ) DH = 6.D-1*DH
        HDGL = HDGL + DH
        IF( ABS(DH).GT.1.D-8 ) GOTO 900
        CPGL = HDGL*RHORL*GRAV
!
!---  Linear or linear-log interpolation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.10 .OR. ISCHR(IZN).EQ.12 ) THEN
        ITBX = 0
        HDGL = FNTBLX( SLX,ISLTBL(1,IZN),ISLTBL(2,IZN),ITBX )
        IF( ISCHR(IZN).EQ.12 ) HDGL = EXP(HDGL)
        CPGL = HDGL*RHORL*GRAV
!
!---  Cubic-spline .or. cubic-spline-log interpolation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.11 .OR. ISCHR(IZN).EQ.13 ) THEN
        HDGL = FSPLNX( SLX,ISLTBL(1,IZN),ISLTBL(2,IZN) )
        IF( ISCHR(IZN).EQ.13 ) HDGL = EXP(HDGL)
        CPGL = HDGL*RHORL*GRAV
!
!---  Cambridge saturation function
!
      ELSEIF( ISCHR(IZN).EQ.41 ) THEN
        CN = MAX( SCHR(3,IZN),SMALL )
        SLP = (SLX-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
        SLP = MAX( SLP,0.D+0 )
        HDGL = SCHR(1,IZN)*(1.D+0-(SLP**CN))
        CPGL = HDGL*RHORL*GRAV
!
!---  van Genuchten saturation function w/ gas entrapment  ---
!
      ELSEIF( ISCHR(IZN).EQ.101 ) THEN
        CN = MAX( SCHR(3,IZN),SMALL )
        IF( SCHR(14,IZN).LE.ZERO ) THEN
          IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
            CM = 1.D+0 - 2.D+0/CN
          ELSE
            CM = 1.D+0 - 1.D+0/CN
          ENDIF
        ELSE
          CM = SCHR(14,IZN)
        ENDIF
        ASGTX = SGTX/(1.D+0-SCHR(4,IZN))
        ESL = (SLX-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
        ASLX = ESL + ASGTX
        HDGL = (((1.D+0/ASLX)**(1.D+0/CM)-1.D+0)**
     &    (1.D+0/CN))/SCHR(1,IZN)
        CPGL = HDGL*RHORL*GRAV
!
!---  Brooks and Corey saturation function w/ gas entrapment  ---
!
      ELSEIF( ISCHR(IZN).EQ.102 ) THEN
        ASGTX = SGTX/(1.D+0-SCHR(4,IZN))
        ESL = (SLX-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
        ASLX = ESL + ASGTX
        IF( (1.D+0-ASLX)/EPSL.LT.EPSL ) THEN
          HDGL = 0.D+0
        ELSE
          HDGL = SCHR(1,IZN)/(ASLX**(1.D+0/SCHR(3,IZN)))
        END IF
        CPGL = HDGL*RHORL*GRAV
!
!---  van Genuchten triple curve saturation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.301 ) THEN
!
!---  Drainage scanning (including main drainage)  ---
!
        IF( IPHX.EQ.-1 ) THEN
          SMP = SCHR(4,IZN)
          CN = MAX( SCHR(3,IZN),SMALL )
          IF( SCHR(14,IZN).LE.ZERO ) THEN
            IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
              CM = 1.D+0 - 2.D+0/CN
            ELSE
              CM = 1.D+0 - 1.D+0/CN
            ENDIF
          ELSE
            CM = SCHR(14,IZN)
          ENDIF
          IF( SLOX.GT.EPSL ) THEN
            HDGL = CPGLO/RHORL/GRAV
            SLPHO = (1.D+0/(1.D+0 + (SCHR(1,IZN)*HDGL)**CN))**CM
            SLPO = (SLOX-SMP)/(1.D+0-SMP)
          ELSE
            SLPO = 0.D+0
            SLPHO = 0.D+0
          ENDIF
          IF( SLX.GT.SMP ) THEN
            SLP = (SLX-SMP)/(1.D+0-SMP)
          ELSE
            CPGL = HDOD*RHORL*GRAV
            ISUB_LOG = ISUB_LOG-1
            RETURN
          ENDIF
          SLP = SLP*SLPHO/SLPO
          IF( 1.D+0-SLP.LT.EPSL ) THEN
            HDGL = 0.D+0
          ELSE
            HDGL = (((1.D+0/SLP)**(1.D+0/CM)-1.D+0)**
     &        (1.D+0/CN))/SCHR(1,IZN)
          ENDIF
          CPGL = HDGL*RHORL*GRAV
!
!---  Main wetting  ---
!
        ELSEIF( IPHX.EQ.2 ) THEN
          SMP = SCHR(6,IZN)
          CN = MAX( SCHR(5,IZN),SMALL )
          IF( SCHR(7,IZN).LE.ZERO ) THEN
            IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
              CM = 1.D+0 - 2.D+0/CN
            ELSE
              CM = 1.D+0 - 1.D+0/CN
            ENDIF
          ELSE
            CM = SCHR(7,IZN)
          ENDIF
          IF( SLX.GT.SMP ) THEN
            SLP = (SLX-SMP)/(1.D+0-SMP)
          ELSE
            CPGL = HDOD*RHORL*GRAV
            ISUB_LOG = ISUB_LOG-1
            RETURN
          ENDIF
          IF( 1.D+0-SLP.LT.EPSL ) THEN
            HDGL = 0.D+0
          ELSE
            HDGL = (((1.D+0/SLP)**(1.D+0/CM)-1.D+0)**
     &        (1.D+0/CN))/SCHR(2,IZN)
          ENDIF
          CPGL = HDGL*RHORL*GRAV
!
!---  Wetting scanning (including boundary wetting scanning)  ---
!
        ELSE
          SMP = SCHR(4,IZN)
          CN = MAX( SCHR(3,IZN),SMALL )
          IF( SCHR(14,IZN).LE.ZERO ) THEN
            IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
              CM = 1.D+0 - 2.D+0/CN
            ELSE
              CM = 1.D+0 - 1.D+0/CN
            ENDIF
          ELSE
            CM = SCHR(14,IZN)
          ENDIF
          IF( SLOX.GT.EPSL ) THEN
            HDGL = CPGLO/RHORL/GRAV
            SLPHO = (1.D+0/(1.D+0 + (SCHR(12,IZN)*HDGL)**CN))**CM
            SLPO = (SLOX-SMP)/(1.D+0-SMP)
          ELSE
            SLPO = 0.D+0
            SLPHO = 0.D+0
          ENDIF
          IF( SLX.GT.SMP ) THEN
            SLP = (SLX-SMP)/(1.D+0-SMP)
          ELSE
            CPGL = HDOD*RHORL*GRAV
            ISUB_LOG = ISUB_LOG-1
            RETURN
          ENDIF
          SLPM = (SCHR(10,IZN)-SMP)/(1.D+0-SMP)
          SLP = (SLP*(SLPM-SLPHO) + SLPM*(SLPHO-SLPO))/(SLPM-SLPO)
          IF( 1.D+0-SLP.LT.EPSL ) THEN
            HDGL = 0.D+0
          ELSE
            HDGL = (((1.D+0/SLP)**(1.D+0/CM)-1.D+0)**
     &        (1.D+0/CN))/SCHR(12,IZN)
          ENDIF
          CPGL = HDGL*RHORL*GRAV
        ENDIF
!
!---  Brooks and Corey  ---
!
      ELSEIF( ISCHR(IZN).EQ.302 ) THEN
!
!---  Drainage scanning (including main drainage)  ---
!
        IF( IPHX.EQ.-1 ) THEN
          SMP = SCHR(4,IZN)
          CL = MAX( SCHR(3,IZN),SMALL )
          IF( SLOX.GT.EPSL ) THEN
            HDGL = CPGLO/RHORL/GRAV
            IF( HDGL.LE.SCHR(1,IZN) ) THEN
              SLP = 1.D+0
            ELSE
              SLP = (SCHR(1,IZN)/HDGL)**CL
            ENDIF
            SLPO = (SLOX-SMP)/(1.D+0-SMP)
          ELSE
            SLPO = 0.D+0
            SLPHO = 0.D+0
          ENDIF
          IF( SLX.GT.SMP ) THEN
            SLP = (SLX-SMP)/(1.D+0-SMP)
          ELSE
            CPGL = HDOD*RHORL*GRAV
            ISUB_LOG = ISUB_LOG-1
            RETURN
          ENDIF
          SLP = SLP*SLPHO/SLPO
          IF( 1.D+0-SLP.LT.EPSL ) THEN
            HDGL = 0.D+0
          ELSE
            HDGL = SCHR(1,IZN)*(1.D+0/SLP)**(1.D+0/CL)
          ENDIF
          CPGL = HDGL*RHORL*GRAV
!
!---  Main wetting  ---
!
        ELSEIF( IPHX.EQ.2 ) THEN
          SMP = SCHR(6,IZN)
          CL = MAX( SCHR(5,IZN),SMALL )
          IF( SLX.GT.SMP ) THEN
            SLP = (SLX-SMP)/(1.D+0-SMP)
          ELSE
            CPGL = HDOD*RHORL*GRAV
            ISUB_LOG = ISUB_LOG-1
            RETURN
          ENDIF
          IF( 1.D+0-SLP.LT.EPSL ) THEN
            HDGL = 0.D+0
          ELSE
            HDGL = SCHR(2,IZN)*(1.D+0/SLP)**(1.D+0/CL)
          ENDIF
          CPGL = HDGL*RHORL*GRAV
!
!---  Wetting scanning (including boundary wetting scanning)  ---
!
        ELSE
          SMP = SCHR(4,IZN)
          CL = MAX( SCHR(3,IZN),SMALL )
          IF( SLOX.GT.EPSL ) THEN
            HDGL = CPGLO/RHORL/GRAV
            IF( HDGL.LE.SCHR(12,IZN) ) THEN
              SLP = 1.D+0
            ELSE
              SLP = (SCHR(12,IZN)/HDGL)**CL
            ENDIF
            SLPO = (SLOX-SMP)/(1.D+0-SMP)
          ELSE
            SLPO = 0.D+0
            SLPHO = 0.D+0
          ENDIF
          IF( SLX.GT.SMP ) THEN
            SLP = (SLX-SMP)/(1.D+0-SMP)
          ELSE
            CPGL = HDGL*RHORL*GRAV
            ISUB_LOG = ISUB_LOG-1
            RETURN
          ENDIF
          SLPM = (SCHR(10,IZN)-SMP)/(1.D+0-SMP)
          SLP = (SLP*(SLPM-SLPHO) + SLPM*(SLPHO-SLPO))/(SLPM-SLPO)
          IF( 1.D+0-SLP.LT.EPSL ) THEN
            HDGL = 0.D+0
          ELSE
            HDGL = SCHR(12,IZN)*(1.D+0/SLP)**(1.D+0/CL)
          ENDIF
          CPGL = HDGL*RHORL*GRAV
        ENDIF
!
!---  Polynomial function  ---
!
      ELSEIF( ISCHR(IZN).EQ.19 ) THEN
!
!---    Check for saturation above first polynomial  ---
!
        IF( SLX.GT.CPLY_SL(3,1,IZN) ) THEN
          HDGL = CPLY_SL(1,1,IZN)*SCHR(1,IZN)
          CPGL = HDGL*RHORL*GRAV
!
!---    Check for saturation below last polynomial  ---
!
        ELSEIF( SLX.LT.CPLY_SL(4,NPLY_SL(IZN),IZN) ) THEN
          HDGL = CPLY_SL(2,NPLY_SL(IZN),IZN)*SCHR(1,IZN)
          CPGL = HDGL*RHORL*GRAV
!
!---    Find polynomial and set initial guess of head  ---
!
        ELSE
          DSLX = 1.D-6
          DO 1900 NP = 1,NPLY_SL(IZN)
            IF( (SLX-DSLX).LE.CPLY_SL(3,NP,IZN) .AND.
     &        (SLX+DSLX).GE.CPLY_SL(4,NP,IZN) ) THEN
              HDGLU = 1.D+1**((SLX-CPLY_SL(4,NP,IZN))*
     &         (LOG10(CPLY_SL(1,NP,IZN))-LOG10(CPLY_SL(2,NP,IZN)))/
     &         (CPLY_SL(3,NP,IZN)-CPLY_SL(4,NP,IZN)) + 
     &         LOG10(CPLY_SL(2,NP,IZN)))
              GOTO 1910
            ENDIF
 1900     CONTINUE
          INDX = 15
          CHMSG = 'Saturation Polynomial Not Found for Rock/Soil #'
          IMSG = IZN
          RLMSG = SLX
          CALL WRMSGS( INDX )
 1910     CONTINUE
          SLZ = 0.D+0
          DSLZ = 0.D+0
          NPOLYC = LPOLYC
          DO 1920 NC = 5,NPOLYC
            SLZ = SLZ + CPLY_SL(NC,NP,IZN)*(LOG10(HDGLU)**(NC-5))
 1920     CONTINUE
          DO 1930 NC = 6,NPOLYC
            REALX = REAL(NC-5)
            DSLZ = DSLZ + REALX*CPLY_SL(NC,NP,IZN)*
     &        (LOG10(HDGLU)**(NC-6))/(HDGLU*2.302585093D+0)
 1930     CONTINUE
          F = SLX - SLZ
          DF = -DSLZ
          DH = -F/(DF+SMALL)
          IF( HDGLU+DH.LT.0.D+0 ) DH = 6.D-1*DH
          HDGLU = HDGLU + DH
          HDGLU = MAX( HDGLU,0.95D+0*CPLY_SL(1,NP,IZN) )
          HDGLU = MIN( HDGLU,1.05D+0*CPLY_SL(2,NP,IZN) )
          IF( ABS(DH).GT.1.D-8 .AND. ABS(F).GT.1.D-8 ) GOTO 1910
          HDGL = HDGLU*SCHR(1,IZN)
          CPGL = HDGL*RHORL*GRAV
        ENDIF
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of CAP1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CHK1
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








!
!----------------------Description-------------------------------------!
!
!     Water Mode
!
!     Check the thermodynamic and hydrologic states declared through
!     user inputs.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 22, 1997.




!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE REACT
      USE PORMED
      USE JACOB
      USE HYST
      USE GRID
      USE FDVP
      USE FDVD
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
      EXTERNAL SCALING
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/CHK1'
!
!---  Dual porosity reassignment of van Genuchten or Brooks Corey
!     characteristic functions  ---
!
      DO 10 IZN = 1,NROCK
        IF( ISCHR(IZN).EQ.3 .AND. ABS(IDP(IZN)).EQ.1 ) ISCHR(IZN) = 1
        IF( ISCHR(IZN).EQ.4 .AND. ABS(IDP(IZN)).EQ.1 ) ISCHR(IZN) = 2
   10 CONTINUE
!
!---  Scaling factors  ---
!
      IF( ISLC(19).EQ.1 ) THEN
!
!---    Simple scaling  ---
!
        DO 20 IZN = 1,NROCK
          PERM(1,IZN) = SCALING( GAMMA(1,IZN),PERM(1,IZN),IGAMMA(1) )
          PERM(2,IZN) = SCALING( GAMMA(1,IZN),PERM(2,IZN),IGAMMA(1) )
          PERM(3,IZN) = SCALING( GAMMA(1,IZN),PERM(3,IZN),IGAMMA(1) )
          PERM(6,IZN) = SCALING( GAMMA(1,IZN),PERM(6,IZN),IGAMMA(1) )
          PERM(7,IZN) = SCALING( GAMMA(6,IZN),PERM(7,IZN),IGAMMA(1) )
          PERM(8,IZN) = SCALING( GAMMA(6,IZN),PERM(8,IZN),IGAMMA(1) )
          PERM(9,IZN) = SCALING( GAMMA(6,IZN),PERM(9,IZN),IGAMMA(1) )
          POR(1,IZN) = SCALING( GAMMA(2,IZN),POR(1,IZN),IGAMMA(2) )
          POR(2,IZN) = SCALING( GAMMA(2,IZN),POR(2,IZN),IGAMMA(2) )
          POR(3,IZN) = SCALING( GAMMA(7,IZN),POR(3,IZN),IGAMMA(2) )
          POR(4,IZN) = SCALING( GAMMA(7,IZN),POR(4,IZN),IGAMMA(2) )
          SCHR(1,IZN) = SCALING( GAMMA(3,IZN),SCHR(1,IZN),IGAMMA(3) )
          SCHR(3,IZN) = SCALING( GAMMA(4,IZN),SCHR(3,IZN),IGAMMA(4) )
!
!---      Scaling for defaulted Mualem/Burdine parameters  ---
!
          IF( ISCHR(IZN).EQ.1 .OR. ISCHR(IZN).EQ.101 ) THEN
            IF( SCHR(14,IZN)/EPSL.LE.EPSL ) THEN
              IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
                SCHR(11,IZN) = 1.D+0 - 2.D+0/(SCHR(3,IZN)+SMALL)
              ELSE
                SCHR(11,IZN) = 1.D+0 - 1.D+0/(SCHR(3,IZN)+SMALL)
              ENDIF
            ENDIF
          ELSEIF( ISCHR(IZN).EQ.2 .OR. ISCHR(IZN).EQ.102 ) THEN
            IF( SCHR(14,IZN)/EPSL.LE.EPSL ) THEN
              IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
                SCHR(11,IZN) = SCHR(3,IZN)
              ELSE
                SCHR(11,IZN) = SCHR(3,IZN)
              ENDIF
            ENDIF
          ENDIF
!
!---      Scaling for Mualem-Anisotropy parameters  ---
!
          IF( ISCHR(IZN).EQ.1 .OR. ISCHR(IZN).EQ.101 ) THEN
            IF( IRPL(IZN).EQ.301 ) THEN
              SCHR(11,IZN) = SCALING( GAMMA(11,IZN),SCHR(11,IZN),
     &          IGAMMA(4) )
              SCHR(12,IZN) = SCALING( GAMMA(12,IZN),SCHR(12,IZN),
     &          IGAMMA(4) )
              SCHR(13,IZN) = SCALING( GAMMA(13,IZN),SCHR(13,IZN),
     &          IGAMMA(4) )
            ENDIF
          ELSEIF( ISCHR(IZN).EQ.2 .OR. ISCHR(IZN).EQ.102 ) THEN
            IF( IRPL(IZN).EQ.301 ) THEN
              SCHR(11,IZN) = SCALING( GAMMA(11,IZN),SCHR(11,IZN),
     &          IGAMMA(4) )
              SCHR(12,IZN) = SCALING( GAMMA(12,IZN),SCHR(12,IZN),
     &          IGAMMA(4) )
              SCHR(13,IZN) = SCALING( GAMMA(13,IZN),SCHR(13,IZN),
     &          IGAMMA(4) )
            ENDIF
          ENDIF
          SCHR(4,IZN) = SCALING( GAMMA(5,IZN),SCHR(4,IZN),IGAMMA(5) )
          SCHR(5,IZN) = SCALING( GAMMA(8,IZN),SCHR(5,IZN),IGAMMA(3) )
          SCHR(6,IZN) = SCALING( GAMMA(9,IZN),SCHR(6,IZN),IGAMMA(4) )
          SCHR(7,IZN) = SCALING( GAMMA(10,IZN),SCHR(7,IZN),IGAMMA(5) )
          PERM(4,IZN) = PERM(1,IZN)
          PERM(1,IZN) = PERM(4,IZN)*(1.D+0-POR(4,IZN)) +
     &      PERM(7,IZN)*POR(4,IZN)
          PERM(5,IZN) = PERM(2,IZN)
          PERM(2,IZN) = PERM(5,IZN)*(1.D+0-POR(4,IZN)) +
     &      PERM(8,IZN)*POR(4,IZN)
          PERM(6,IZN) = PERM(3,IZN)
          PERM(3,IZN) = PERM(6,IZN)*(1.D+0-POR(4,IZN)) +
     &      PERM(9,IZN)*POR(4,IZN)
   20   CONTINUE
      ENDIF
!
!---  Compute at reference aqueous density  ---
!
      TX = 2.D+1
      PX = PATM
      CALL WATLQD( TX,PX,RHORL )
!
!---  Check initial pressures and temperature  ---
!
      INDX = 0
      DO 100 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 100
        DO 90 M = 1,ISVC+2
          T(M,N) = T(2,N)
          PG(M,N) = PG(2,N)
          PL(M,N) = PL(2,N)
          PN(M,N) = PN(2,N)
          SG(M,N) = SG(2,N)
          SL(M,N) = SL(2,N)
          SN(M,N) = SN(2,N)
   90   CONTINUE
        IZN = IZ(N)
        IF( T(2,N).GT.TMX .OR. T(2,N).LT.TMN ) THEN
          INDX = 16
          IMSG = N
          RLMSG = T(2,N)
          CHMSG = 'Initial Temperature(C) @ Node'
          CALL WRMSGS( INDX )
        ENDIF
        IF( PL(2,N).GT.PMX-PATM ) THEN
          INDX = 16
          IMSG = N
          CHMSG = 'Initial Aqueous Pressure(Pa) @ Node'
          RLMSG = PL(2,N)+PATM
          CALL WRMSGS( INDX )
        ENDIF
        IF( SL(2,N).GT.1.D+0 .OR. SL(2,N).LT.0.D+0 ) THEN
          INDX = 16
          IMSG = N
          CHMSG = 'Initial Aqueous Saturation @ Node'
          RLMSG = SL(2,N)
          CALL WRMSGS( INDX )
        ENDIF
        IF( PG(2,N).GT.PMX-PATM .OR. PG(2,N).LT.PMN-PATM ) THEN
          INDX = 16
          IMSG = N
          CHMSG = 'Initial Gas Pressure(Pa) @ Node'
          RLMSG = PG(2,N)+PATM
          CALL WRMSGS( INDX )
        ENDIF
        SGT(2,N) = SG(2,N)

!
!---    Load reactive transport total and diffusive porosity  ---
!
        POR0(1,N) = POR(1,IZ(N))
        POR0(2,N) = POR(2,IZ(N))

  100 CONTINUE

      IF( INDX.GT.0 ) STOP

!
!---  Compute capillary pressure head for water entry on the
!     main wetting path for rocks using the van Genuchten or
!     Brooks and Corey triple curve saturation functions  ---
!
      DO 120 IZN = 1,NROCK
        IF( ISCHR(IZN).EQ.301 .OR. ISCHR(IZN).EQ.302 ) THEN
          SLX = 9.9D-1*(1.D+0-SCHR(6,IZN)) + SCHR(6,IZN)
          SGTX = 0.D+0
          SLOX = 0.D+0
          CPGLOX = 0.D+0
          IPHX = 2
          CALL CAP1( IZN,SLX,SGTX,CPGL,SLOX,CPGLOX,IPHX )
          HCMWE(IZN) = CPGL/RHORL/GRAV
        ENDIF
  120 CONTINUE
!
!---  Compute saturated vapor pressures and aqueous density  ---
!
      DO 160 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 160
        CALL WATSP( T(2,N),PSW(2,N) )
        PX = MAX( PL(2,N)+PATM,PG(2,N)+PATM,PSW(2,N) )
        CALL WATLQD( T(2,N),PX,RHOL(2,N) )
  160 CONTINUE
!
!---  Compute the total trapping number  ---
!
      CALL TRPGL1
!
!---  Return for restart simulations  ---
!
      IF( IEO.EQ.2 ) THEN
!
!---    Establish reference pressure for soil compressibility  ---
!
        DO 170 N = 1,NFLD
          IF( IXP(N).EQ.0 ) GOTO 170
          IZN = IZ(N)
          IF( CMP(3,IZN).GT.PATM ) THEN
            PCMP(N) = CMP(3,IZN)
          ELSEIF( ISLC(61).EQ.0 ) THEN
            PCMP(N) = MAX( PL(2,N),PG(2,N) )+PATM
          ENDIF
  170   CONTINUE
        GOTO 248
      ENDIF
!
!---  Compute initial saturation conditions  ---
!
      IF( ISIC.EQ.1 .OR. ISIC.EQ.11 ) THEN
        DO 210 N = 1,NFLD
          IF( IXP(N).EQ.0 ) GOTO 210
          IZN = IZ(N)
!
!---      Moisture content initial condition  ---
!
          IF( ISIC.EQ.11 ) THEN
            SL(2,N) = 1.D+0
            IF( POR(2,IZN)/EPSL.GT.EPSL ) SL(2,N) = RHOG(2,N)/POR(2,IZN)
            SL(2,N) = MIN( SL(2,N),1.D+0 )
          ENDIF
          IF( SL(2,N).LT.SCHR(4,IZN) ) THEN
            IF( ISCHR(IZN).EQ.301 .OR. ISCHR(IZN).EQ.302 ) THEN
             IF( SL(2,N).LT.SCHR(6,IZN) ) THEN
               INDX = 16
               IMSG = N
               RLMSG = SL(2,N)
               CHMSG = 'Initial Saturation < Residual Saturation @ Node'
               CALL WRMSGS( INDX )
             ELSE
               IPH(2,N) = 2
             ENDIF
            ELSE
              INDX = 16
              IMSG = N
              RLMSG = SL(2,N)
              CHMSG = 'Initial Saturation < Residual Saturation @ Node'
              CALL WRMSGS( INDX )
            ENDIF
          ENDIF
          IF( ISCHR(IZN).EQ.101 .OR. ISCHR(IZN).EQ.102 ) THEN
            SGRMX = SCHR(15,IZN)/(1.D+0+TRPGL(2,N)/(SCHR(9,IZN)+SMALL))
            IF( SGRMX.GT.EPSL ) THEN
              IF( SGT(2,N).GT.1.D+2 ) THEN
                SGT(2,N) = SGT(2,N)-1.D+2
                IF( SGT(2,N).GT.1.D+0 .OR. SGT(2,N).LT.0.D+0 ) THEN
                  INDX = 17
                  N_DB = N
                  CHMSG = 'Initial Relative Trapped Gas Saturation: '
                  CALL WRMSGS( INDX )
                ENDIF
                R = 1.D+0/SGRMX - 1.D+0
                ESLX = (SL(2,N)-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
                ASGTX = (-((-1.D+0 - SGRMX + ESLX)*R) -
     &            SQRT(((-1.D+0 - SGRMX + ESLX)**2)*(R**2) -
     &            4.D+0*R*(-1.D+0 + SGRMX + ESLX + SGRMX*R -
     &            SGRMX*ESLX*R)))/(2.D+0*R)
                ASGTX = SGT(2,N)*ASGTX
                SGT(2,N) = ASGTX*(1.D+0-SCHR(4,IZN))
              ELSE
                R = 1.D+0/SGRMX - 1.D+0
                ASGTX = SGT(2,N)/(1.D+0-SCHR(4,IZN))
              ENDIF
            ELSE
              ASGTX = 0.D+0
            ENDIF
            ASLX = (SL(2,N)-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN)) + ASGTX
            ASLMIN(2,N) = (ASLX - ASGTX - 2.D+0*R*ASGTX + ASLX*R*ASGTX
     &        - (R**2)*ASGTX + ASLX*(R**2)*ASGTX)/(1.D+0 - R*ASGTX
     &        - (R**2)*ASGTX + ASLX*(R**2)*ASGTX)
            IF( ASLMIN(2,N).LT.ZERO ) THEN
              INDX = 16
              IMSG = N
              RLMSG = SGT(2,N)
              CHMSG = 'Initial Aqueous Trapped Gas Saturation @ Node'
              CALL WRMSGS( INDX )
            ENDIF
          ENDIF
          CPGLO = PG(1,N)-PL(1,N)
          CALL CAP1( IZN,SL(2,N),SGT(2,N),CPGL,SL(1,N),CPGLO,IPH(2,N) )
          PL(2,N) = PG(2,N) - CPGL
          PX = MAX( PG(2,N),PL(2,N) )

          POR0(1,N) = POR0(1,N)
          POR0(2,N) = POR0(2,N)

          CALL PORSTY(N,PX,PX,PORD(2,N),PORT(2,N))
          DO 209 M = 1,ISVC+2
            T(M,N) = T(2,N)
            PL(M,N) = PL(2,N)
            PG(M,N) = PG(2,N)
  209     CONTINUE
          ASLMIN(1,N) = ASLMIN(2,N)
          SL(1,N) = SL(2,N)
  210   CONTINUE
      ELSEIF( ISIC.EQ.2 .OR. ISIC.EQ.12 ) THEN
        DO 220 N = 1,NFLD
          IF( IXP(N).EQ.0 ) GOTO 220
          IZN = IZ(N)
!
!---      Moisture content initial condition  ---
!
          IF( ISIC.EQ.12 ) THEN
            SL(2,N) = 1.D+0
            IF( POR(2,IZN)/EPSL.GT.EPSL ) SL(2,N) = RHOG(2,N)/POR(2,IZN)
            SL(2,N) = MIN( SL(2,N),1.D+0 )
          ENDIF
          IF( SL(2,N).LT.SCHR(4,IZN) ) THEN
            IF( ISCHR(IZN).EQ.301 .OR. ISCHR(IZN).EQ.302 ) THEN
             IF( SL(2,N).LT.SCHR(6,IZN) ) THEN
               INDX = 16
               IMSG = N
               RLMSG = SL(2,N)
               CHMSG = 'Initial Saturation < Residual Saturation @ Node'
               CALL WRMSGS( INDX )
             ELSE
               IPH(2,N) = 2
             ENDIF
            ELSE
              INDX = 16
              IMSG = N
              RLMSG = SL(2,N)
              CHMSG = 'Initial Saturation < Residual Saturation @ Node'
              CALL WRMSGS( INDX )
            ENDIF
          ENDIF
          IF( ISCHR(IZN).EQ.101 .OR. ISCHR(IZN).EQ.102 ) THEN
            SGRMX = SCHR(15,IZN)/(1.D+0+TRPGL(2,N)/(SCHR(9,IZN)+SMALL))
            IF( SGRMX.GT.EPSL ) THEN
              IF( SGT(2,N).GT.1.D+2 ) THEN
                SGT(2,N) = SGT(2,N)-1.D+2
                IF( SGT(2,N).GT.1.D+0 .OR. SGT(2,N).LT.0.D+0 ) THEN
                  INDX = 17
                  N_DB = N
                  CHMSG = 'Initial Relative Trapped Gas Saturation: '
                  CALL WRMSGS( INDX )
                ENDIF
                R = 1.D+0/SGRMX - 1.D+0
                ESLX = (SL(2,N)-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
                ASGTX = (-((-1.D+0 - SGRMX + ESLX)*R) -
     &            SQRT(((-1.D+0 - SGRMX + ESLX)**2)*(R**2) -
     &            4.D+0*R*(-1.D+0 + SGRMX + ESLX + SGRMX*R -
     &            SGRMX*ESLX*R)))/(2.D+0*R)
                ASGTX = SGT(2,N)*ASGTX
                SGT(2,N) = ASGTX*(1.D+0-SCHR(4,IZN))
              ELSE
                R = 1.D+0/SGRMX - 1.D+0
                ASGTX = SGT(2,N)/(1.D+0-SCHR(4,IZN))
              ENDIF
            ELSE
              ASGTX = 0.D+0
            ENDIF
            ASLX = (SL(2,N)-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN)) + ASGTX
            ASLMIN(2,N) = (ASLX - ASGTX - 2.D+0*R*ASGTX + ASLX*R*ASGTX
     &        - (R**2)*ASGTX + ASLX*(R**2)*ASGTX)/(1.D+0 - R*ASGTX
     &        - (R**2)*ASGTX + ASLX*(R**2)*ASGTX)
            IF( ASLMIN(2,N).LT.ZERO ) THEN
              INDX = 16
              IMSG = N
              RLMSG = SGT(2,N)
              CHMSG = 'Initial Aqueous Trapped Gas Saturation @ Node'
              CALL WRMSGS( INDX )
            ENDIF
          ENDIF
          CPGLO = PG(1,N)-PL(1,N)
          CALL CAP1( IZN,SL(2,N),SGT(2,N),CPGL,SL(1,N),CPGLO,IPH(2,N) )
          PG(2,N) = PL(2,N) + CPGL
          PX = MAX( PG(2,N),PL(2,N) )

          POR0(1,N) = POR0(1,N)
          POR0(2,N) = POR0(2,N)

          CALL PORSTY(N,PX,PX,PORD(2,N),PORT(2,N))
          DO 219 M = 1,ISVC+2
            T(M,N) = T(2,N)
            PL(M,N) = PL(2,N)
            PG(M,N) = PG(2,N)
  219     CONTINUE
          ASLMIN(1,N) = ASLMIN(2,N)
          SL(1,N) = SL(2,N)
  220   CONTINUE
      ELSEIF( ISIC.EQ.3 ) THEN
        INDX = 0
        DO 230 N = 1,NFLD
          IF( IXP(N).EQ.0 ) GOTO 230
          IZN = IZ(N)
          MX = 2
          INDX = 2
          CALL KSP1( N,IZN,MX,PG(2,N),PL(2,N),SL(2,N),RKL(1,2,N),
     &      ASLX,ASLMIN(2,N),ASGTX,SGRMX,INDX,IPH(2,N),
     &      PG(1,N),PL(1,N),SL(1,N) )
          IF( ISCHR(IZN).EQ.101 .OR. ISCHR(IZN).EQ.102 ) THEN
            SGRMX = SCHR(15,IZN)/(1.D+0+TRPGL(2,N)/(SCHR(9,IZN)+SMALL))
            IF( SGRMX.GT.EPSL ) THEN
              IF( SGT(2,N).GT.1.D+2 ) THEN
                SGT(2,N) = SGT(2,N)-1.D+2
                IF( SGT(2,N).GT.1.D+0 .OR. SGT(2,N).LT.0.D+0 ) THEN
                  INDX = 17
                  N_DB = N
                  CHMSG = 'Initial Relative Trapped Gas Saturation: '
                  CALL WRMSGS( INDX )
                ENDIF
                R = 1.D+0/SGRMX - 1.D+0
                ASGTX = SGRMX - (1.D+0-ASLX)/(1.D+0 + R*(1.D+0-ASLX))
                ASGTX = SGT(2,N)*ASGTX
                SGT(2,N) = ASGTX*(1.D+0-SCHR(4,IZN))
              ELSE
                R = 1.D+0/SGRMX - 1.D+0
                ASGTX = SGT(2,N)/(1.D+0-SCHR(4,IZN))
              ENDIF
            ELSE
              ASGTX = 0.D+0
            ENDIF
            IF( ASGTX-SGRMX.GT.EPSL ) THEN
              INDX = 16
              IMSG = N
              RLMSG = SGT(2,N)
              CHMSG = 'Initial Aqueous Trapped Gas Saturation @ Node'
              CALL WRMSGS( INDX )
            ENDIF
          ENDIF
          IF( ISCHR(IZN).EQ.101 .OR. ISCHR(IZN).EQ.102 ) THEN
            ASLMIN(2,N) = (ASLX - ASGTX - 2.D+0*R*ASGTX + ASLX*R*ASGTX
     &        - (R**2)*ASGTX + ASLX*(R**2)*ASGTX)/(1.D+0 - R*ASGTX
     &        - (R**2)*ASGTX + ASLX*(R**2)*ASGTX)
            IF( ASLMIN(2,N).LT.ZERO ) THEN
              INDX = 16
              IMSG = N
              RLMSG = SGT(2,N)
              CHMSG = 'Initial Aqueous Trapped Gas Saturation @ Node'
              CALL WRMSGS( INDX )
            ENDIF
          ENDIF
          PX = MAX( PG(2,N),PL(2,N) )

          POR0(1,N) = POR0(1,N)
          POR0(2,N) = POR0(2,N)

          CALL PORSTY(N,PX,PX,PORD(2,N),PORT(2,N))
          DO 229 M = 1,ISVC+2
            T(M,N) = T(2,N)
            PL(M,N) = PL(2,N)
            PG(M,N) = PG(2,N)
  229     CONTINUE
          ASLMIN(1,N) = ASLMIN(2,N)
          SL(1,N) = SL(2,N)
  230   CONTINUE
      ENDIF
!
!---    Establish reference pressure for soil compressibility  ---
!
      DO 240 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 240
        IZN = IZ(N)
        IF( CMP(3,IZN).GT.PATM ) THEN
          PCMP(N) = CMP(3,IZN)
        ELSEIF( ISLC(61).EQ.0 ) THEN
          PCMP(N) = MAX( PL(2,N),PG(2,N) )+PATM
        ENDIF
  240 CONTINUE
!
!---  Normal and restart simulations  ---
!
  248 CONTINUE

      ISUB_LOG = ISUB_LOG-1
!
!---  End of CHK1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CISC1
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
!     Water Mode
!
!     Compute initial solute concentrations.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE PORMED
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
!----------------------Executable Lines--------------------------------!
!
      IF( IEQC.EQ.0 .AND. ISLC(40).EQ.0 ) RETURN
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/CISC1'
      DO 140 NSL = 1,NSOLU
        DO 110 N = 1,NFLD
          IF( IXP(N).EQ.0 ) GOTO 110
          IZN = IZ(N)
          XVL = SL(2,N)*PORD(2,N)
          IF( IPCL(NSL).EQ.4 ) THEN
            NS = IPCSL(IZN,NSL)
            XVS = SL(2,N)*RHOS(IZN)*PCSL(1,IZN,NS)*(1.D+0-PORT(2,N))
            CLX = C(N,NS)/(XVS+XVL)
            IF( CLX.LT.SMALL ) THEN
              PCSLX = PCSL(1,IZN,NSL)
            ELSE
             PCSLX = 1.D+1**(PCSL(2,IZN,NSL)+PCSL(3,IZN,NSL)*LOG10(CLX))
            ENDIF
            XVS = SL(2,N)*RHOS(IZN)*PCSLX*(1.D+0-PORT(2,N))
          ELSEIF( IPCL(NSL).EQ.3 ) THEN
            NS = IPCSL(IZN,NSL)
            XVS = SL(2,N)*RHOS(IZN)*PCSL(1,IZN,NS)*(1.D+0-PORT(2,N))
            CLX = C(N,NS)/(XVS+XVL)
            IF( CLX.LT.SMALL ) THEN
              PCSLX = PCSL(1,IZN,NSL)
            ELSE
             PCSLX = 1.D+1**(PCSL(2,IZN,NSL)+PCSL(3,IZN,NSL)*LOG10(CLX))
            ENDIF
            XVS = RHOS(IZN)*PCSLX*(1.D+0-PORT(2,N))
          ELSEIF( IPCL(NSL).EQ.2 ) THEN
            XVS = SL(2,N)*RHOS(IZN)*PCSL(1,IZN,NSL)*(1.D+0-PORT(2,N))
          ELSE
            XVS = RHOS(IZN)*PCSL(1,IZN,NSL)*(1.D+0-PORT(2,N))
          ENDIF
          IF( (XVL+XVS)/EPSL.LT.EPSL ) THEN
            YL(N,NSL) = 0.D+0
          ELSE
            YL(N,NSL) = XVL/(XVL+XVS)
          ENDIF
!
!---  Phase-volumetric concentration ratios  ---
!
          IF( ICT(N,NSL).EQ.2 ) THEN
            C(N,NSL) = C(N,NSL)*(XVS + XVL)
          ELSEIF( ICT(N,NSL).EQ.3 ) THEN
            C(N,NSL) = C(N,NSL)*XVL/YL(N,NSL)
          ELSEIF( ICT(N,NSL).EQ.-1 ) THEN
            C(N,NSL) = C(N,NSL)*RHOS(IZN)*(1.D+0-PORT(2,N))
          ELSEIF( ICT(N,NSL).EQ.-2 ) THEN
            C(N,NSL) = C(N,NSL)*(XVS+XVL)*RHOS(IZN)*(1.D+0-PORT(2,N))
          ENDIF
  110   CONTINUE
!
!---  Assign boundary solute concentrations for initial condition
!     type boundary conditions  ---
!
        DO 130 NB = 1,NBC
          N = IBCN(NB)
          CBO(NB,NSL) = C(N,NSL)
  130   CONTINUE
  140 CONTINUE
!
!---  Correct aqueous liquid density and viscosity for electrolyte
!     solute concentration  ---
!
      IF( ISLC(16).EQ.1 ) THEN
        DO 200 N = 1,NFLD
          CLX = C(N,NSL_ELC)*YL(N,NSL_ELC)/(SL(2,N)*PORD(2,N)+EPSL)
          XLW(2,N) = RHOL(2,N)
          CALL ELC_DEN( RHOL(2,N),CLX,ELC_DCF )
          XLW(2,N) = XLW(2,N)/RHOL(2,N)
          CALL ELC_VIS( VISL(2,N),CLX,ELC_VCF )
  200   CONTINUE
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of CISC1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE DFINA1
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
!     Water Mode
!
!     Define temporarily inactive nodes.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE JACOB
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
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/DFINA1'
      IF( ICNV.EQ.3 ) THEN
        DO 100 N = 1,NFLD
          IXP(N) = ABS(IXP(N))
  100   CONTINUE
      ELSEIF( NITER.EQ.1 ) THEN
        DO 200 N = 1,NFLD
          IF( IXP(N).EQ.0 ) GOTO 200
          NMD = IXP(N)
          MPL = IM(IEQW,NMD)
          ACP = PORD(2,N)*RHOL(2,N)*SL(2,N)*DTI*VOL(N)
          RSDX = MIN( ABS(BLU(MPL))/(ABS(PG(2,N)-PL(2,N))+PATM),
     &      ABS(RSDL(IEQW,N)/(ACP+SMALL)) )
          IF( RSDX.LT.RSDMX ) IXP(N) = -IXP(N)
  200   CONTINUE
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of DFINA1 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE DPOR1
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
!     Water Mode
!
!     Dual porosity model.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, February, 1997.
!     Last Modified by MD White, Battelle, PNL, February 9, 1997.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOURC
      USE SOLTN
      USE REACT
      USE PORMED
      USE JACOB
      USE HYST
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 PLX(2),SLX(2),RKLX(3,2),PORDX(2),PORTX(2),
     &  RHOLX(2),VISLX(2)
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/DPOR1'
      ASLX = 0.D+0
      ASLMINX = 0.D+0
      ASGTX = 0.D+0
      SGRMX = 0.D+0
      INDX = 1
!$OMP PARALLEL DO
!$OMP&  COPYIN(ISUB_LOG)
!$OMP&  DEFAULT(NONE)
!$OMP&  SHARED(NFLD,IXP,IZ,IDP,PN,DNR,IEQW,PG,IPH,PL,SL,PATM,PCMP,
!$OMP&    PVW,T,PSW,PERM,DT,CHML,DTI,SMALL,ISVC,GPI,EPSL,SRCW,PORD,
!$OMP&    VOL,POR,POR0)
!$OMP&  PRIVATE(IZN,PLX,SLX,RKLX,PX,PORDX,PORTX,RHOLX,VISLX,PERMX,
!$OMP&    STOR,RKLMX,ALPHA,THETA,PHI,YX,REALX,BETAX)
!$OMP&  FIRSTPRIVATE(ASLX,ASLMINX,ASGTX,SGRMX,INDX)
      DO 1000 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 1000
        IZN = IZ(N)
        IF( IDP(IZN).EQ.0 ) GOTO 1000
        IDP(IZN) = 2*IDP(IZN)
        PLX(1) = PN(1,N)
        PLX(2) = PN(1,N) + DNR(IEQW,N)

        POR0(1,N) = POR0(1,N)
        POR0(2,N) = POR0(2,N)

!
!---  Compute matrix properties  ----
!
        DO 100 M = 1,2
          CALL KSP1( N,IZN,M,PG(2,N),PLX(M),SLX(M),RKLX(1,M),
     &      ASLX,ASLMINX,ASGTX,SGRMX,INDX,IPH(2,N),PG(1,N),
     &      PL(1,N),SL(1,N) )
          PX = MAX( PG(2,N),PLX(M) )+PATM
          CALL PORSTY(N,PX,PCMP(N),PORDX(M),PORTX(M))
          PX = MAX( PLX(M)+PATM,PG(2,N)+PATM,PVW(2,N) )
          CALL WATLQD( T(2,N),PX,RHOLX(M) )
          CALL WATLQV( T(2,N),PX,PSW(2,N),VISLX(M) )
          PERMX = (PERM(4,IZN)*PERM(5,IZN)*PERM(6,IZN))**(1./3.)
  100   CONTINUE
        STOR = (PORDX(2)*SLX(2)*RHOLX(2)-PORDX(1)*SLX(1)*RHOLX(1))/
     &    DNR(IEQW,N)
        RKLMX = (RKLX(1,1)*RKLX(2,1)*RKLX(3,1))**(1./3.)
        IF( IDP(IZN).EQ.2 ) THEN
          ALPHA = RHOLX(1)*RKLMX*PERMX/STOR/VISLX(1)
          THETA = ALPHA*DT/(4.D+0*CHML(IZN)**2)
        ELSE
          ALPHA = 4.D+0/(CHML(IZN)**2)
          PHI = STOR*DTI*VISLX(1)/(ALPHA*RHOLX(1)*RKLMX*PERMX+SMALL)
        ENDIF
        DO 300 M = 2,ISVC+2
          IF( IDP(IZN).EQ.2 ) THEN
            YX = 0.D+0
            DO 200 I = 1,499,2
              REALX = REAL(I)
              BETAX = GPI*REALX
              YX = YX + (8.D+0/(BETAX**2))*EXP(-(BETAX**2)*THETA)
  200       CONTINUE
            YX = MIN( YX,1.D+0 )
            PN(M,N) = (PN(1,N)-PL(M,N))*YX + PL(M,N)
          ELSE
            PN(M,N) = PN(1,N)
            IF( ABS(PHI).GT.EPSL ) PN(M,N) = (2.D+0*PL(M,N) -
     &        PN(1,N)*(1.D+0-2.D+0*PHI))/(1.D+0 + 2.D+0*PHI)
c            IF( PN(1,N).GE.PL(M,N) ) PN(M,N) = MAX( PL(M,N),PN(M,N) )
c            IF( PN(1,N).LE.PL(M,N) ) PN(M,N) = MIN( PL(M,N),PN(M,N) )
          ENDIF
          SRCW(M,N) = SRCW(M,N) + (PN(1,N)-PN(M,N))*DTI*STOR*
     &     (1.D+0-PORD(M,N))*VOL(N)
  300   CONTINUE
        IDP(IZN) = IDP(IZN)/2
 1000 CONTINUE
!$OMP END PARALLEL DO
      ISUB_LOG = ISUB_LOG-1
!
!---  End of DPOR1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ELC1
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
!     Water Mode
!
!     Correct aqueous liquid density and viscosity for electrolyte
!     solute concentration
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, October 2000.
!     Last Modified by MD White, PNNL, October 11, 2000.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE JACOB
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
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ELC1'
      DO 400 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 400
        PGX = PG(2,N) + PATM
        DO 300 M = 2,ISVC+2
          PLX = PL(M,N) + PATM
          CALL WATSP( T(2,N),PVW(M,N) )
          PX = MAX( PLX,PGX,PVW(M,N) )
          CALL WATLQD( T(2,N),PX,RHOL(M,N) )
          CALL WATLQV( T(2,N),PX,PSW(2,N),VISL(M,N) )
          CLX = C(N,NSL_ELC)*YL(N,NSL_ELC)/(SL(M,N)*PORD(M,N)+SMALL)
          XLW(M,N) = RHOL(M,N)
          CALL ELC_DEN( RHOL(M,N),CLX,ELC_DCF )
          XLW(M,N) = XLW(M,N)/RHOL(M,N)
          CALL ELC_VIS( VISL(M,N),CLX,ELC_VCF )
  300   CONTINUE
  400 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ELC1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE INCRM1
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
!     Water Mode
!
!     Compute primary variable increments
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
      USE JACOB
      USE HYST
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
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/INCRM1'
!
!--- Determine wetting/drying direction on the first
!    iteration  ---
!
      IF( NITER.EQ.1 ) THEN
!$OMP PARALLEL DO
!$OMP&  COPYIN(ISUB_LOG)
!$OMP&  DEFAULT(NONE)
!$OMP&  SHARED(NFLD,IXP,PG,PL,IPH,ASLMIN,SL,ASTMIN,BTGL,RHORL,GRAV)
        DO 10 N = 1,NFLD
          IF( IXP(N).EQ.0 ) GOTO 10
!
!---      Draining  ---
!
          IF( (PG(2,N)-PL(2,N)).GT.(PG(1,N)-PL(1,N)) ) THEN
              IPH(2,N) = -1
!
!---        Reset reversal point  ---
!
            IF( IPH(1,N).EQ.1 ) THEN
              ASLMIN(2,N) = SL(1,N)
              ASTMIN(2,N) = BTGL(1,N)*(PG(1,N)-PL(1,N))/RHORL/GRAV
            ENDIF
!
!---      Wetting  ---
!
          ELSE
            IPH(2,N) = 1
!
!---        Reset reversal point  ---
!
            IF( IPH(1,N).EQ.-1 ) THEN
              ASLMIN(2,N) = SL(1,N)
              ASTMIN(2,N) = BTGL(1,N)*(PG(1,N)-PL(1,N))/RHORL/GRAV
            ENDIF
          ENDIF
   10   CONTINUE
!$OMP END PARALLEL DO
      ENDIF
!
!--- Compute increments  ---
!
!$OMP PARALLEL DO
!$OMP&  COPYIN(ISUB_LOG)
!$OMP&  DEFAULT(NONE)
!$OMP&  SHARED(NFLD,IXP,IZ,NITER,IPH,PG,PL,RHORL,GRAV,HCMWE,ISCHR,DNR,
!$OMP&    IEQW,ISVC)
!$OMP&  PRIVATE(IZN)
      DO 200 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 200
        IZN = IZ(N)
        IF( NITER.EQ.1 ) THEN
          IF( IPH(2,N).EQ.2 ) THEN
            IF( (PG(2,N)-PL(2,N))/RHORL/GRAV.LE.HCMWE(IZN) ) THEN
              IF( (PG(2,N)-PL(2,N)).GT.(PG(1,N)-PL(1,N)) ) THEN
                IPH(2,N) = -1
              ELSE
                IPH(2,N) = 1
              ENDIF
            ENDIF
          ELSE
            IF( (PG(2,N)-PL(2,N)).GT.(PG(1,N)-PL(1,N)) ) THEN
              IPH(2,N) = -1
            ELSE
              IPH(2,N) = 1
            ENDIF
          ENDIF
        ENDIF
        IF( ISCHR(IZN).EQ.1 .OR. ISCHR(IZN).EQ.2 ) THEN
!          IF( PG(2,N).GT.PL(2,N) ) THEN
!            CALL QSAT1( PL(2,N),PG(2,N),DPGLX,N )
!            DNR(IEQW,N) = -DPGLX
!          ELSE
            DNR(IEQW,N) = -MAX( 1.D-1,1.D-6*ABS(PG(2,N)-PL(2,N)) )
!          ENDIF
        ELSE
          DNR(IEQW,N) = -MAX( 1.D-1,1.D-6*ABS(PG(2,N)-PL(2,N)) )
        ENDIF
!
!--- Increment the primary variables  ---
!
        DO 100 M = 3,ISVC+2
          PL(M,N) = PL(2,N)
          IF( M.EQ.IEQW+2 ) THEN
            PL(M,N) = PL(M,N) + DNR(IEQW,N)
          ENDIF
  100   CONTINUE
  200 CONTINUE
!$OMP END PARALLEL DO
      ISUB_LOG = ISUB_LOG-1
!
!---  End of INCRM1 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE KSP1( N,IZN,M,PGX,PLX,SLX,RKLX,ASLX,ASLMINX,
     &  ASGTX,SGRMX,INDX,IPHX,PGOX,PLOX,SLOX )
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
!     Water Mode
!
!     Compute the aqueous saturation from the gas/aqueous capillary
!     pressure, and compute the aqueous relative permeability from the
!     aqueous saturation.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
      USE HYST
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
!      REAL*8 SCHRX(LSCHR)
      REAL*8 RKLX(3)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/KSP1'
!      DO 10 I = 1,LSCHR
!        SCHRX(I) = SCHR(I,IZN)
!   10 CONTINUE
!      IF( ABS(IDP(IZN)).EQ.1 ) THEN
!        SCHRX(1) = SCHR(5,IZN)
!        SCHRX(3) = SCHR(6,IZN)
!        SCHRX(4) = SCHR(7,IZN)
!        SCHRX(11) = SCHR(8,IZN)
!        SCHRX(14) = SCHR(15,IZN)
!      ENDIF
!
!---  van Genuchten saturation function  ---
!
      IF( ISCHR(IZN).EQ.1 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
!
!---    Two-pressure dual-porosity model  ---
!
        IF( ABS(IDP(IZN)).EQ.1 ) THEN
          CN = MAX( SCHR(6,IZN),SMALL )
          IF( SCHR(15,IZN).LE.ZERO ) THEN
            IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
              CM = 1.D+0 - 2.D+0/CN
            ELSE
              CM = 1.D+0 - 1.D+0/CN
            ENDIF
          ELSE
            CM = SCHR(15,IZN)
          ENDIF
          SLP = (1.D+0/(1.D+0 + (SCHR(5,IZN)*HDGL)**CN))**CM
          REALX = REAL(ISM(IZN))
          HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
          SMP = MAX( (1.D+0-HSCL)*SCHR(7,IZN),ZERO )
        ELSE
          CN = MAX( SCHR(3,IZN),SMALL )
          IF( SCHR(14,IZN).LE.ZERO ) THEN
            IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
              CM = 1.D+0 - 2.D+0/CN
            ELSE
              CM = 1.D+0 - 1.D+0/CN
            ENDIF
          ELSE
            CM = SCHR(14,IZN)
          ENDIF
          SLP = (1.D+0/(1.D+0 + (SCHR(1,IZN)*HDGL)**CN))**CM
          REALX = REAL(ISM(IZN))
          HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
          SMP = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        ENDIF
        SLX = SLP*(1.D+0-SMP) + SMP
        ASLX = SLP
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
!
!---  Brooks and Corey saturation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.2 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
!
!---    Two-pressure dual-porosity model  ---
!
        IF( ABS(IDP(IZN)).EQ.1 ) THEN
          CL = MAX( SCHR(6,IZN),SMALL )
          IF( HDGL.LE.SCHR(5,IZN) ) THEN
            SLP = 1.D+0
          ELSE
            SLP = (SCHR(5,IZN)/HDGL)**CL
          ENDIF
          REALX = REAL(ISM(IZN))
          HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
          SMP = MAX( (1.D+0-HSCL)*SCHR(7,IZN),ZERO )
        ELSE
          CL = MAX( SCHR(3,IZN),SMALL )
          IF( HDGL.LE.SCHR(1,IZN) ) THEN
            SLP = 1.D+0
          ELSE
            SLP = (SCHR(1,IZN)/HDGL)**CL
          ENDIF
          REALX = REAL(ISM(IZN))
          HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
          SMP = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        ENDIF
        SLX = SLP*(1.D+0-SMP) + SMP
        ASLX = SLP
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
!
!---  Single-pressure dual-porosity
!     van Genuchten saturation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.3 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
        CN = MAX( SCHR(3,IZN),SMALL )
        IF( SCHR(14,IZN).LE.ZERO ) THEN
          IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
            CM = 1.D+0 - 2.D+0/CN
          ELSE
            CM = 1.D+0 - 1.D+0/CN
          ENDIF
        ELSE
          CM = SCHR(14,IZN)
        ENDIF
        SLPM = (1.D+0/(1.D+0 + (SCHR(1,IZN)*HDGL)**CN))**CM
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMPM = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        SDPM(N) = SLPM*(1.D+0-SMPM) + SMPM
        CN = MAX( SCHR(6,IZN),SMALL )
        IF( SCHR(15,IZN).LE.ZERO ) THEN
          IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
            CM = 1.D+0 - 2.D+0/CN
          ELSE
            CM = 1.D+0 - 1.D+0/CN
          ENDIF
        ELSE
          CM = SCHR(15,IZN)
        ENDIF
        SLPF = (1.D+0/(1.D+0 + (SCHR(5,IZN)*HDGL)**CN))**CM
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMPF = MAX( (1.D+0-HSCL)*SCHR(7,IZN),ZERO )
        SDPF(N) = SLPF*(1.D+0-SMPF) + SMPF
        PORDM = (1.D+0-POR(4,IZN))*POR(2,IZN)/
     &    ( POR(4,IZN) + (1.D+0-POR(4,IZN))*POR(2,IZN) + SMALL )
        PORDF = POR(4,IZN)/
     &    ( POR(4,IZN) + (1.D+0-POR(4,IZN))*POR(2,IZN) + SMALL )
        SLX = SDPF(N)*PORDF + SDPM(N)*PORDM
        ASLX = SLPF*PORDF + SLPM*PORDM
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
!
!---  Single-pressure dual-porosity
!     Brooks and Corey saturation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.4 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
        CL = MAX( SCHR(3,IZN),SMALL )
        IF( HDGL.LE.SCHR(1,IZN) ) THEN
          SLPM = 1.D+0
        ELSE
          SLPM = (SCHR(1,IZN)/HDGL)**CL
        ENDIF
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMPM = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        SDPM(N) = SLPM*(1.D+0-SMPM) + SMPM
        CL = MAX( SCHR(6,IZN),SMALL )
        IF( HDGL.LE.SCHR(5,IZN) ) THEN
          SLPF = 1.D+0
        ELSE
          SLPF = (SCHR(5,IZN)/HDGL)**CL
        ENDIF
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMPF = MAX( (1.D+0-HSCL)*SCHR(7,IZN),ZERO )
        SDPF(N) = SLPF*(1.D+0-SMPF) + SMPF
        PORDM = (1.D+0-POR(4,IZN))*POR(2,IZN)/
     &    ( POR(4,IZN) + (1.D+0-POR(4,IZN))*POR(2,IZN) + SMALL )
        PORDF = POR(4,IZN)/
     &    ( POR(4,IZN) + (1.D+0-POR(4,IZN))*POR(2,IZN) + SMALL )
        SLX = SDPF(N)*PORDF + SDPM(N)*PORDM
        ASLX = SLPF*PORDF + SLPM*PORDM
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
!
!---  Haverkamp saturation function  ---
!
      ELSEIF( ABS(ISCHR(IZN)).EQ.5 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
        IF( HDGL.LE.SCHR(1,IZN) ) THEN
          SLP = 1.D+0
        ELSE
          ALPHAX = SCHR(2,IZN)/SCHR(5,IZN)
          IF( ISCHR(IZN).EQ.-5 ) THEN
            HDGLX = LOG(HDGL/SCHR(5,IZN))
          ELSE
            HDGLX = HDGL/SCHR(5,IZN)
          ENDIF
          SLP = ALPHAX/(ALPHAX
     &      + (HDGLX**SCHR(3,IZN)))
        ENDIF
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMP = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        SLX = SLP*(1.D+0-SMP) + SMP
        ASLX = SLP
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
!
!---  Russo saturation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.9 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
        SLP = (EXP(-5.D-1*SCHR(1,IZN)*HDGL)*
     &    (1.D+0 + 5.D-1*SCHR(1,IZN)*HDGL))**(2.D+0/(SCHR(3,IZN)+2.D+0))
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMP = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        SLX = SLP*(1.D+0-SMP) + SMP
        ASLX = SLP
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
!
!---  Linear or linear-log interpolation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.10 .OR. ISCHR(IZN).EQ.12 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
        IF( ISCHR(IZN).EQ.12 ) HDGL = LOG(HDGL)
        ITBX = 0
        IF( M.NE.2 ) ITBX = 1
        SLP = FNTBLY( HDGL,ISLTBL(1,IZN),ISLTBL(2,IZN),ITBX )
        SLX = SLP
        SGX = MAX( 1.D+0-SLX,ZERO )
        ASLX = SLP
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
!
!---  Cubic-spline or cubic-spline-log interpolation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.11 .OR. ISCHR(IZN).EQ.13 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
        IF( ISCHR(IZN).EQ.13 ) HDGL = LOG(HDGL)
        SLP = FSPLNY( HDGL,ISLTBL(1,IZN),ISLTBL(2,IZN) )
        SLX = SLP
        SGX = MAX( 1.D+0-SLX,ZERO )
        ASLX = SLP
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
!
!---  Polynomial function  ---
!
      ELSEIF( ISCHR(IZN).EQ.19 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
!
!---    Convert head units for polynomial function basis  ---
!
        HDGLU = HDGL/SCHR(1,IZN)
        IF( HDGLU.LT.CPLY_SL(1,1,IZN) ) THEN
          SLX = 1.D+0
        ELSEIF( HDGLU.GE.CPLY_SL(2,NPLY_SL(IZN),IZN) ) THEN
          SLX = 0.D+0
        ELSE
          DO 192 NP = 1,NPLY_SL(IZN)
            IF( HDGLU.GE.CPLY_SL(1,NP,IZN) .AND.
     &        HDGLU.LT.CPLY_SL(2,NP,IZN) ) THEN
                SLX = 0.D+0
                NPOLYC = LPOLYC
                DO 190 NC = 5,NPOLYC
                  SLX = SLX + CPLY_SL(NC,NP,IZN)*(LOG10(HDGLU)**(NC-5))
  190           CONTINUE
                GOTO 194
            ENDIF
  192     CONTINUE
  194     CONTINUE
        ENDIF
        SLP = SLX
        SGX = MAX( 1.D+0-SLX,0.D+0 )
        ASLX = SLP
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
!
!---  Cambridge function
!
      ELSEIF( ISCHR(IZN).EQ.41 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
        CN = MAX( SCHR(3,IZN),SMALL )
        SLP = ((SCHR(1,IZN)-HDGL)/(SCHR(1,IZN)+SMALL))**(1.D+0/CN)
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMP = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        SLX = SLP*(1.D+0-SMP) + SMP
        ASLX = SLP
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
!
!---  van Genuchten saturation function w/ gas entrapment  ---
!
      ELSEIF( ISCHR(IZN).EQ.101 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
        CN = MAX( SCHR(3,IZN),SMALL )
        IF( SCHR(14,IZN).LE.ZERO ) THEN
          IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
            CM = 1.D+0 - 2.D+0/CN
          ELSE
            CM = 1.D+0 - 1.D+0/CN
          ENDIF
        ELSE
          CM = SCHR(14,IZN)
        ENDIF
        ASLX = (1.D+0/(1.D+0 + (SCHR(1,IZN)*HDGL)**CN))**CM
        IF( INDX.EQ.2 ) GOTO 200
        ASLM = MIN( ASLX,ASLMINX )
        IF( SGRMX.GT.EPSL ) THEN
          R = 1.D+0/SGRMX - 1.D+0
          ASGTX = (1.D+0-ASLM)/(1.D+0 + R*(1.D+0-ASLM)) -
     &      (1.D+0-ASLX)/(1.D+0 + R*(1.D+0-ASLX))
        ELSE
          ASGTX = 0.D+0
        ENDIF
        SLP = ASLX - ASGTX
        SMP = MAX( SCHR(4,IZN),ZERO )
        SLX = SLP*(1.D+0-SMP) + SMP
!
!---  Brooks and Corey saturation function w/ gas entrapment  ---
!
      ELSEIF( ISCHR(IZN).EQ.102 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
        CL = MAX( SCHR(3,IZN),SMALL )
        IF( HDGL.LE.SCHR(1,IZN) ) THEN
          ASLX = 1.D+0
        ELSE
          ASLX = (SCHR(1,IZN)/HDGL)**CL
        ENDIF
        IF( INDX.EQ.2 ) GOTO 200
        ASLM = MIN( ASLX,ASLMINX )
        IF( SGRMX.GT.EPSL ) THEN
          R = 1.D+0/SGRMX - 1.D+0
          ASGTX = (1.D+0-ASLM)/(1.D+0 + R*(1.D+0-ASLM)) -
     &      (1.D+0-ASLX)/(1.D+0 + R*(1.D+0-ASLX))
        ELSE
          ASGTX = 0.D+0
        ENDIF
        SLP = ASLX - ASGTX
        SMP = MAX( SCHR(4,IZN),ZERO )
        SLX = SLP*(1.D+0-SMP) + SMP
!
!---  van Genuchten triple curve saturation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.301 ) THEN
!
!---  Drainage scanning (including main drainage)  ---
!
        IF( IPHX.EQ.-1 ) THEN
          HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
          CN = MAX( SCHR(3,IZN),SMALL )
          IF( SCHR(14,IZN).LE.ZERO ) THEN
            IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
              CM = 1.D+0 - 2.D+0/CN
            ELSE
              CM = 1.D+0 - 1.D+0/CN
            ENDIF
          ELSE
            CM = SCHR(14,IZN)
          ENDIF
          SLP = (1.D+0/(1.D+0 + (SCHR(1,IZN)*HDGL)**CN))**CM
          SMP = SCHR(4,IZN)
          IF( SLOX.GT.EPSL ) THEN
            SLPO = (SLOX-SMP)/(1.D+0-SMP)
            HDGL = MAX( (PGOX-PLOX)/RHORL/GRAV,1.D-14 )
            CN = MAX( SCHR(3,IZN),SMALL )
            IF( SCHR(14,IZN).LE.ZERO ) THEN
              IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
                CM = 1.D+0 - 2.D+0/CN
              ELSE
                CM = 1.D+0 - 1.D+0/CN
              ENDIF
            ELSE
              CM = SCHR(14,IZN)
            ENDIF
            SLPHO = (1.D+0/(1.D+0 + (SCHR(1,IZN)*HDGL)**CN))**CM
            SLP = SLP*SLPO/(SLPHO+SMALL)
          ENDIF
          SLX = SLP*(1.D+0-SMP) + SMP
!
!---  Main wetting  ---
!
        ELSEIF( IPHX.EQ.2 ) THEN
          HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
          CN = MAX( SCHR(5,IZN),SMALL )
          IF( SCHR(7,IZN).LE.ZERO ) THEN
            IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
              CM = 1.D+0 - 2.D+0/CN
            ELSE
              CM = 1.D+0 - 1.D+0/CN
            ENDIF
          ELSE
            CM = SCHR(7,IZN)
          ENDIF
          SLP = (1.D+0/(1.D+0 + (SCHR(2,IZN)*HDGL)**CN))**CM
          SMP = SCHR(6,IZN)
          SLX = SLP*(1.D+0-SMP) + SMP
!
!---  Wetting scanning (including boundary wetting scanning)  ---
!
        ELSE
          HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
          CN = MAX( SCHR(3,IZN),SMALL )
          IF( SCHR(14,IZN).LE.ZERO ) THEN
            IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
              CM = 1.D+0 - 2.D+0/CN
            ELSE
              CM = 1.D+0 - 1.D+0/CN
            ENDIF
          ELSE
            CM = SCHR(14,IZN)
          ENDIF
          SLP = (1.D+0/(1.D+0 + (SCHR(12,IZN)*HDGL)**CN))**CM
          SMP = SCHR(4,IZN)
          IF( SLOX.GT.EPSL ) THEN
            SLPO = (SLOX-SMP)/(1.D+0-SMP)
            SLPM = (SCHR(10,IZN)-SMP)/(1.D+0-SMP)
            HDGL = MAX( (PGOX-PLOX)/RHORL/GRAV,1.D-14 )
            CN = MAX( SCHR(3,IZN),SMALL )
            IF( SCHR(14,IZN).LE.ZERO ) THEN
              IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
                CM = 1.D+0 - 2.D+0/CN
              ELSE
                CM = 1.D+0 - 1.D+0/CN
              ENDIF
            ELSE
              CM = SCHR(14,IZN)
            ENDIF
            SLPHO = (1.D+0/(1.D+0 + (SCHR(12,IZN)*HDGL)**CN))**CM
            SLP = (SLP-SLPM)*(SLPO-SLPM)/(SLPHO-SLPM+SMALL) + SLPM
            SLP = MIN( SLP,SLPM )
          ENDIF
          SLX = SLP*(1.D+0-SMP) + SMP
        ENDIF
        ASLX = SLP
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
!
!---  Brooks and Corey triple curve saturation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.302 ) THEN
!
!---  Drainage scanning (including main drainage)  ---
!
        IF( IPHX.EQ.-1 ) THEN
          HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
          CL = MAX( SCHR(3,IZN),SMALL )
          IF( HDGL.LE.SCHR(1,IZN) ) THEN
            SLP = 1.D+0
          ELSE
            SLP = (SCHR(1,IZN)/HDGL)**CL
          ENDIF
          SMP = SCHR(4,IZN)
          IF( SLOX.GT.EPSL ) THEN
            SLPO = (SLOX-SMP)/(1.D+0-SMP)
            HDGL = MAX( (PGOX-PLOX)/RHORL/GRAV,1.D-14 )
            CL = MAX( SCHR(3,IZN),SMALL )
            IF( HDGL.LE.SCHR(1,IZN) ) THEN
              SLPHO = 1.D+0
            ELSE
              SLPHO = (SCHR(1,IZN)/HDGL)**CL
            ENDIF
            SLP = SLP*SLPO/(SLPHO+SMALL)
          ENDIF
          SLX = SLP*(1.D+0-SMP) + SMP
!
!---  Main wetting  ---
!
        ELSEIF( IPHX.EQ.2 ) THEN
          HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
          CL = MAX( SCHR(5,IZN),SMALL )
          IF( HDGL.LE.SCHR(2,IZN) ) THEN
            SLP = 1.D+0
          ELSE
            SLP = (SCHR(2,IZN)/HDGL)**CL
          ENDIF
          SMP = SCHR(6,IZN)
          SLX = SLP*(1.D+0-SMP) + SMP
!
!---  Wetting scanning (including boundary wetting scanning)  ---
!
        ELSE
          HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
          CL = MAX( SCHR(3,IZN),SMALL )
          IF( HDGL.LE.SCHR(12,IZN) ) THEN
            SLP = 1.D+0
          ELSE
            SLP = (SCHR(12,IZN)/HDGL)**CL
          ENDIF
          SMP = SCHR(4,IZN)
          IF( SLOX.GT.EPSL ) THEN
            SLPM = (SCHR(10,IZN)-SMP)/(1.D+0-SMP)
            SLPO = (SLOX-SMP)/(1.D+0-SMP)
            HDGL = MAX( (PGOX-PLOX)/RHORL/GRAV,1.D-14 )
            CL = MAX( SCHR(3,IZN),SMALL )
            IF( HDGL.LE.SCHR(12,IZN) ) THEN
              SLPHO = 1.D+0
            ELSE
              SLPHO = (SCHR(12,IZN)/HDGL)**CL
            ENDIF
            SLP = (SLP-SLPM)*(SLPO-SLPM)/(SLPHO-SLPM+SMALL) + SLPM
            SLP = MIN( SLP,SLPM )
          ENDIF
          SLX = SLP*(1.D+0-SMP) + SMP
        ENDIF
        ASLX = SLP
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
      ENDIF
!
!---  Skip relative permeability functions  ---
!
      IF( INDX.EQ.0 ) GOTO 200
!
!---  Relative permeability  ---
!
      CALL RKL1( HDGL,RKLX,SLP,SLPF,SLPM,SLX,IZN,IPH(2,N),M )
!
!---  Relative permeability tensor  ---
!
      DO 180 ITX = 1,3
        IF( IRPLT(ITX,IZN).NE.0 )
     &    CALL RKLT1( HDGL,RKLX(ITX),SLP,SLPF,SLPM,SLX,IZN,
     &    ITX,IPH(2,N),M )
  180 CONTINUE
  200 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of KSP1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE LDO1
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
!     Water Mode
!
!     Load the current time step values into the old time step
!     variables.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE REACT
      USE HYST
      USE GRID
      USE FDVP
      USE FDVD
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
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/LDO1'
!
!---  Assign old time step values  ---
!
      DO 100 N = 1,NFLD
        PG(1,N) = PG(2,N)
        T(1,N) = T(2,N)
        PL(1,N) = PL(2,N)
        PN(1,N) = PN(2,N)
        PORD(1,N) = PORD(2,N)
        PORT(1,N) = PORT(2,N)
        SL(1,N) = SL(2,N)
        SG(1,N) = SG(2,N)
        PVA(1,N) = PVA(2,N)
        PVW(1,N) = PVW(2,N)
        PVO(1,N) = PVO(2,N)
        XLW(1,N) = XLW(2,N)
        RHOL(1,N) = RHOL(2,N)
        VISL(1,N) = VISL(2,N)
        TORL(1,N) = TORL(2,N)
        RKL(1,1,N) = RKL(1,2,N)
        RKL(2,1,N) = RKL(2,2,N)
        RKL(3,1,N) = RKL(3,2,N)
        TRPGL(1,N) = TRPGL(2,N)
        ASLMIN(1,N) = MIN( ASL(N),ASLMIN(2,N) )
        ASLMIN(2,N) = ASLMIN(1,N)
        NPHAZ(1,N) = NPHAZ(2,N)
        IPH(1,N) = IPH(2,N)
        DO 90 NSL = 1,NSOLU
          CO(N,NSL) = C(N,NSL)
   90   CONTINUE

        DO 92 NEQ = 1,NEQC+NEQK
          NSL = NEQ + NSOLU 
          CO(N,NSL) = C(N,NSL)
   92   CONTINUE
        DO 94 NSP = 1,NSPR
          SP_CO(N,NSP) = SP_C(N,NSP)
   94   CONTINUE

  100 CONTINUE
      DO 105 NS = 1,NSR
        QLW(1,NS) = QLW(1,NS) + QLW(3,NS)
  105 CONTINUE
      DO 107 NB = 1,NBC
        PLB(1,NB) = PLB(2,NB)
        PGB(1,NB) = PGB(2,NB)
        SLB(1,NB) = SLB(2,NB)
  107 CONTINUE








      ISUB_LOG = ISUB_LOG-1
!
!---  End of LDO1 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE QSAT1( PLX,PGX,DPGLX,N )
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
!     Water Mode
!
!     Calculate pressure differential required to yield a differential
!     change in saturation.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, February, 2001.
!     Last Modified by MD White, Battelle, February 9, 2001.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
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
      SUB_LOG(ISUB_LOG) = '/QSAT1'
!
!---  Zonation index   ---
!
      IZN = IZ(N)
!
!---  van Genuchten saturation functions  ---
!
      IF( ISCHR(IZN).EQ.1 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
        CN = MAX( SCHR(3,IZN),SMALL )
        IF( SCHR(14,IZN).LE.ZERO ) THEN
          IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
            CM = 1.D+0 - 2.D+0/CN
          ELSE
            CM = 1.D+0 - 1.D+0/CN
          ENDIF
        ELSE
          CM = SCHR(14,IZN)
        ENDIF
        SLP = (1.D+0/(1.D+0 + (SCHR(1,IZN)*HDGL)**CN))**CM
        IF( SLP.LT.5.D-1 ) THEN
          SLP = SLP + 1.D-7
        ELSE
          SLP = SLP - 1.D-7
        ENDIF
        HAW = ((-1.D+0 + (1.D+0/SLP)**(1.D+0/CM))**(1.D+0/CN))/
     &    SCHR(1,IZN)
        DPGLX = HAW*RHORL*GRAV + PLX - PGX
!
!---  Brooks and Corey saturation functions  ---
!
      ELSEIF( ISCHR(IZN).EQ.2 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
        CL = MAX( SCHR(3,IZN),SMALL )
        IF( HDGL.LE.SCHR(1,IZN) ) THEN
          SLP = 1.D+0
        ELSE
          SLP = (SCHR(1,IZN)/HDGL)**CL
        ENDIF
        IF( SLP.LT.5.D-1 ) THEN
          SLP = SLP + 1.D-6
        ELSE
          SLP = SLP - 1.D-6
        ENDIF
        HAW = ((1.D+0/SLP)**(1.D+0/CL))*SCHR(1,IZN)
        DPGLX = HAW*RHORL*GRAV + PLX - PGX
      ENDIF
!
!---  End of QSAT1 group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDBC1
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
!     Water Mode
!
!     Read input file for boundary condition information.
!
!     1 - Dirichlet
!     2 - Neumann
!     3 - Zero Flux
!     4 - Saturated
!     5 - Unit Gradient
!     6 - Free Gradient
!     7 - Outflow
!     8 - Aqueous Concentration
!     9 - Gas Concentration
!     10 - NAPL Concentration
!     11 - Hydraulic Gradient
!     12 - Initial Condition
!     13 - Inflow
!     14 - Inflow Aqueous-Phase
!     15 - Inflow Gas-Phase
!     16 - Inflow NAPL
!     17 - Seepage Face
!     18 - Convective
!     19 - Inflow-Outflow Volumetric
!     20 - Falling Head
!     21 - Falling Pond
!     22 - Free Boundary
!     23 - Inflow-Outflow Aqueous
!     24 - Potential Evaporation
!     25 - Fluctuating Water Table
!     26 - Dirichlet-Outflow
!     27 - Diode
!     28 - Convective-Radiative
!     29 - Convective Ground Surface
!     30 - Shuttleworth-Wallace
!     31 - Bare Shuttleworth-Wallace
!     32 - Relative Saturation
!     33 - Inflow Relative Saturation
!     34 - Aqu. Rel. Sat.
!     35 - Inflow Aqu. Rel. Sat.
!     36 - Aqu. Mass Frac.
!     37 - Inflow Aqu. Mass Frac.
!     38 - Vol. Conc.
!     39 - Inflow Vol. Conc.
!     40 - Aqu. Conc.
!     41 - Inflow Aqu. Conc.
!     42 - Dirichlet-Inflow
!     43 - Inflow-Outflow Gas
!     44 - X-Y-Z Hydraulic Gradient
!     45 - X-Y-Z Seepage Face
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE GRID
      USE FILES
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
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,BDUM(LUK+LSOLU+1),FDUM,SDUM
      CHARACTER*64 UNTS
      CHARACTER*24 CHLB(3)
      CHARACTER*32 CHTYP(45)
      CHARACTER*512 CHDUM
      REAL*8 VAR(LBTM,LBCV)
      INTEGER ITYP(LUK+LSOLU+1)
      INTEGER IRDBCF(LBTM,LBCV)
      INTEGER IBCSPX(LSPBC+1)
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Data Statements---------------------------------!
!
      SAVE CHLB
      DATA CHLB /'X-Direction Gradient, ','Y-Direction Gradient, ',
     &           'Z-Direction Gradient, '/
      SAVE CHTYP
      DATA CHTYP /'Dirichlet','Neumann','Zero Flux','Saturated',
     &  'Unit Gradient','Free Gradient','Outflow',
     &  'Aqueous Concentration','Gas Concentration',
     &  'NAPL Concentration','Hydraulic Gradient',
     &  'Initial Condition','Inflow','Inflow Aqueous-Phase',
     &  'Inflow Gas-Phase','Inflow NAPL','Seepage Face','Convective',
     &  'Inflow-Outflow Volumetric','Falling Head','Falling Pond',
     &  'Free Boundary','Inflow-Outflow Aqueous',
     &  'Potential Evaporation','Fluctuating Water Table',
     &  'Dirichlet-Outflow','Diode',
     &  'Convective-Radiative','Convective Ground Surface',
     &  'Shuttleworth-Wallace','Bare Shuttleworth-Wallace',
     &  'Relative Saturation','Inflow Relative Saturation',
     &  'Aqu. Rel. Sat.','Inflow Aqu. Rel. Sat.','Aqu. Mass Frac.',
     &  'Inflow Aqu. Mass Frac.','Vol. Conc.','Inflow Vol. Conc.',
     &  'Aqu. Conc.','Inflow Aqu. Conc.','Dirichlet-Inflow',
     &  'Inflow-Outflow Gas',
     &  'X-Y-Z Hydraulic Gradient','X-Y-Z Seepage Face'/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDBC1'
!
!---  Write card information to ouput file  ---
!
      CARD = 'Boundary Conditions Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE(IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
      NBC = 0
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Boundary Condition Cards: '
      CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
      DO 400 NB = 1, NLIN
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        IF( NB.NE.1 ) WRITE(IWR, '(/)')
!
!---    Read boundary orientation  ---
!
        ISTART = 1
        VARB = 'Boundary Condition Orientation: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        WRITE(IWR,'(/,A,$)') VARB(1:IVR)
        IF( INDEX(ADUM(1:),'west').NE.0 ) THEN
          IBCDX = -1
          WRITE(IWR,'(A)') 'X-Direction: West Surface'
        ELSEIF( INDEX(ADUM(1:),'east').NE.0 ) THEN
          IBCDX = 1
          WRITE(IWR,'(A)') 'X-Direction: East Surface'
        ELSEIF( INDEX(ADUM(1:),'south').NE.0 ) THEN
          IBCDX = -2
          WRITE(IWR,'(A)') 'Y-Direction: South Surface'
        ELSEIF( INDEX(ADUM(1:),'north').NE.0 ) THEN
          IBCDX = 2
          WRITE(IWR,'(A)') 'Y-Direction: North Surface'
        ELSEIF( INDEX(ADUM(1:),'bottom').NE.0 ) THEN
          IBCDX = -3
          WRITE(IWR,'(A)') 'Z-Direction: Bottom Surface'
        ELSEIF( INDEX(ADUM(1:),'top').NE.0 ) THEN
          IBCDX = 3
          WRITE(IWR,'(A)') 'Z-Direction: Top Surface'
        ELSEIF( INDEX(ADUM(1:),'file').NE.0 ) THEN
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
          NCH = INDEX(FDUM,'  ')-1
          OPEN(UNIT=26,FILE=FDUM(1:NCH),STATUS='OLD',FORM='FORMATTED')
          WRITE(IWR,'(/,2A)') 'Boundary Condition Domain File: ',
     &      FDUM(1:NCH)
          I1X = 1
          I2X = 1
          J1X = 1
          J2X = 1
          K1X = 1
          K2X = 0
    5     CONTINUE
          READ(26,*,END=10) IX,JX,KX,IBCDX
          K2X = K2X+1
          GOTO 5
   10     CONTINUE
          REWIND(26)
        ENDIF
!
!---    Read boundary types  ---
!
        VARB = 'Boundary Condition Type: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM(1))
        IF( IEQC.GT.0 ) THEN
          DO 15 NSL = 1,NSOLU
!
!---        Allow for returns in input lines  ---
!
            CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              CALL RDINPL( CHDUM )
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
            BDUM(NSL+LUK) = 'zero flux'
            IDFLT = 1
            VARB = 'Solute Boundary Condition Type: '
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM(NSL+LUK))
   15     CONTINUE
        ENDIF

!
!---    Reactive species boundary condition type  ---
!
        IF( ISLC(40).EQ.1 ) THEN
!
!---      Allow for returns in input lines  ---
!
          CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RDINPL( CHDUM )
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          BDUM(NSOLU+LUK+1) = 'zero flux'
          IDFLT = 1
          VARB = 'Reactive Species Boundary Condition Type: '
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM(NSOLU+LUK+1))
!
!---      Number of reactive species  ---
!
          CALL RDINPL( CHDUM )
          CALL LCASE( CHDUM )
          ISTART = 1
          VARB = 'Number of Reactive Species'
          CALL RDINT(ISTART,ICOMMA,CHDUM,IBCSPX(1))
          DO 16 NSPX = 2,IBCSPX(1)+1
            IBCSPX(NSPX) = 0
   16     CONTINUE
!
!---      Loop over number of reactive species  ---
!
          DO 20 NSPX = 1,IBCSPX(1)
!
!---        Allow for returns in input lines  ---
!
            CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              CALL RDINPL( CHDUM )
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
            VARB = 'Species Name'
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,SDUM)
!
!---        Aqueous species  ---
!
            DO 17 M = 1,NSPL
              IF( SPNML(M).EQ.SDUM ) THEN
                IBCSPX(NSPX+1) = M
                GOTO 18
              ENDIF
   17       CONTINUE
            INDX = 4
            CHMSG = 'Unrecognized Aqueous Species Name: '
     &         // SDUM(1:NCH)
            CALL WRMSGS( INDX )
   18       CONTINUE
   20     CONTINUE
        ENDIF

!
!---    Assign aqueous boundary condition type ---
!
        IF( INDEX(BDUM(2)(1:),'dirichlet-outflow').NE.0 ) THEN
           ITYP(IEQW) = 26
        ELSEIF( INDEX(BDUM(1)(1:),'dirichlet').NE.0 ) THEN
           ITYP(IEQW) = 1
        ELSEIF( INDEX(BDUM(1)(1:),'neumann').NE.0 ) THEN
           ITYP(IEQW) = 2
        ELSEIF( INDEX(BDUM(1)(1:),'zero flux').NE.0 ) THEN
           ITYP(IEQW) = 3
        ELSEIF( INDEX(BDUM(1)(1:),'saturated').NE.0 ) THEN
           ITYP(IEQW) = 4
        ELSEIF( INDEX(BDUM(1)(1:),'unit gradient').NE.0 ) THEN
           ITYP(IEQW) = 5
        ELSEIF( INDEX(BDUM(1)(1:),'free gradient').NE.0 ) THEN
           ITYP(IEQW) = 6
        ELSEIF( INDEX(BDUM(1)(1:),'outflow').NE.0 ) THEN
           ITYP(IEQW) = 7
        ELSEIF( INDEX(BDUM(1)(1:),'hydraulic gradient').NE.0 ) THEN
           ITYP(IEQW) = 11
           IF( INDEX(BDUM(1)(1:),'x-y-z').NE.0 ) ITYP(IEQW) = 44
        ELSEIF( INDEX(BDUM(1)(1:),'initial cond').NE.0 ) THEN
           ITYP(IEQW) = 12
        ELSEIF( INDEX(BDUM(1)(1:),'seepage face').NE.0 ) THEN
           ITYP(IEQW) = 17
           IF( INDEX(BDUM(1)(1:),'x-y-z').NE.0 ) ITYP(IEQW) = 45
        ELSEIF( INDEX(BDUM(1)(1:),'falling head').NE.0 ) THEN
           ITYP(IEQW) = 20
        ELSEIF( INDEX(BDUM(1)(1:),'falling pond').NE.0 ) THEN
           ITYP(IEQW) = 21
        ELSEIF( INDEX(BDUM(1)(1:),'free boundary').NE.0 ) THEN
           ITYP(IEQW) = 22
        ELSEIF( INDEX(BDUM(1)(1:),'poten').NE.0 .AND.
     &    INDEX(BDUM(1)(1:),'evap').NE.0 ) THEN
           ITYP(IEQW) = 24
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Boundary Condition: '//BDUM(1)
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Assign solute boundary condition type ---
!
        IF( IEQC.GT.0 ) THEN
          DO 25 NSL = 1,NSOLU
            IF( INDEX(BDUM(NSL+LUK)(1:),
     &        'inflow-outflow vol').NE.0 ) THEN
              ITYP(NSL+LUK) = 19
            ELSEIF( INDEX(BDUM(NSL+LUK)(1:),
     &        'inflow-outflow aqu').NE.0 ) THEN
              ITYP(NSL+LUK) = 23
            ELSEIF( INDEX(BDUM(NSL+LUK)(1:),'outflow').NE.0 ) THEN
              ITYP(NSL+LUK) = 7
            ELSEIF( INDEX(BDUM(NSL+LUK)(1:),'initial cond').NE.0 ) THEN
              ITYP(NSL+LUK) = 12
            ELSEIF( INDEX(BDUM(NSL+LUK)(1:),'inflow vol').NE.0 ) THEN
              ITYP(NSL+LUK) = 13
            ELSEIF( INDEX(BDUM(NSL+LUK)(1:),'inflow aqu').NE.0 ) THEN
              ITYP(NSL+LUK) = 14
            ELSEIF(INDEX(BDUM(NSL+LUK)(1:),
     &        'volumetric conc').NE.0 ) THEN
              ITYP(NSL+LUK) = 1
            ELSEIF( INDEX(BDUM(NSL+LUK)(1:),'aqueous conc').NE.0 )THEN
              ITYP(NSL+LUK) = 8
            ELSEIF( INDEX(BDUM(NSL+LUK)(1:),'zero flux').NE.0 ) THEN
              ITYP(NSL+LUK) = 3
            ELSE
              INDX = 4
              CHMSG = 'Unrecognized Solute Boundary Condition: '
     &          //BDUM(NSL+LUK)
              CALL WRMSGS( INDX )
            ENDIF
   25     CONTINUE
        ENDIF

!
!---    Assign reactive species boundary condition type ---
!
        IF( ISLC(40).EQ.1 ) THEN
          IF( INDEX(BDUM(NSOLU+LUK+1)(1:),
     &      'inflow-outflow aqu').NE.0 ) THEN
            ITYP(NSOLU+LUK+1) = 23
          ELSEIF( INDEX(BDUM(NSOLU+LUK+1)(1:),'outflow').NE.0 ) THEN
            ITYP(NSOLU+LUK+1) = 7
          ELSEIF( INDEX(BDUM(NSOLU+LUK+1)(1:),'initial co').NE.0 ) THEN
            ITYP(NSOLU+LUK+1) = 12
          ELSEIF( INDEX(BDUM(NSOLU+LUK+1)(1:),'inflow aqu').NE.0 ) THEN
            ITYP(NSOLU+LUK+1) = 14
          ELSEIF( INDEX(BDUM(NSOLU+LUK+1)(1:),'aqueous conc').NE.0 )THEN
            ITYP(NSOLU+LUK+1) = 8
          ELSEIF( INDEX(BDUM(NSOLU+LUK+1)(1:),'zero flux').NE.0 ) THEN
            ITYP(NSOLU+LUK+1) = 3
          ELSE
            INDX = 4
            CHMSG = 'Unrecognized Reactive Species Boundary Condition: '
     &        //BDUM(NSOLU+LUK)
            CALL WRMSGS( INDX )
          ENDIF
        ENDIF

!
!---    Write boundary condition type(s) ---
!
        WRITE(IWR,'(A,$)') 'Boundary Condition Type: '
        WRITE(IWR,'(2X,2A)') CHTYP(ITYP(IEQW)),' Aqueous'
!
!---    Write solute boundary condition type(s) ---
!
        IF( IEQC.GT.0 ) THEN
          DO 30 NSL = 1,NSOLU
            IDB = INDEX( SOLUT(NSL)(1:),'  ') - 1
            WRITE(IWR,'(2X,2A)') CHTYP(ITYP(NSL+LUK)),SOLUT(NSL)(1:IDB)
   30     CONTINUE
        ENDIF

!
!---    Write species boundary condition type(s) ---
!
        IF( ISLC(40).EQ.1 ) THEN
          WRITE(IWR,'(2X,2A)') CHTYP(ITYP(NSOLU+LUK+1)),
     &      ' Reactive Species'
        ENDIF

!
!---  Read and write boundary domain indices  ---
!
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        IF( INDEX(ADUM(1:),'file').EQ.0 ) THEN
          VARB = 'Boundary Condition Domain: '
          CALL RDINT(ISTART,ICOMMA,CHDUM,I1X)
          CALL RDINT(ISTART,ICOMMA,CHDUM,I2X)
          CALL RDINT(ISTART,ICOMMA,CHDUM,J1X)
          CALL RDINT(ISTART,ICOMMA,CHDUM,J2X)
          CALL RDINT(ISTART,ICOMMA,CHDUM,K1X)
          CALL RDINT(ISTART,ICOMMA,CHDUM,K2X)
          WRITE(IWR,'(A)') VARB(1:IVR)
          WRITE(IWR, '(2X,A,I6,A,I6)') 'I = ',I1X,' to ',I2X
          WRITE(IWR, '(2X,A,I6,A,I6)') 'J = ',J1X,' to ',J2X
          WRITE(IWR, '(2X,A,I6,A,I6)') 'K = ',K1X,' to ',K2X
!
!---  Check boundary domain  ---
!
          IF( I1X.GT.I2X .OR. J1X.GT.J2X .OR. K1X.GT.K2X ) THEN
            INDX = 4
            CHMSG = 'Nonascending Boundary Condition Domain Indices'
            CALL WRMSGS( INDX )
          ENDIF
          IF( I1X.LT.1 .OR. I2X.GT.IFLD. OR. J1X.LT.1 .OR.
     &      J2X.GT.JFLD .OR. K1X.LT.1 .OR. K2X.GT.KFLD ) THEN
            INDX = 4
            CHMSG = 'Illegal Boundary Condition Domain'
            CALL WRMSGS( INDX )
          ENDIF
        ENDIF
!
!---  Read number of boundary times  ---
!
        VARB = 'Number of Boundary Condition Times: '
        CALL RDINT(ISTART,ICOMMA,CHDUM,IBCMX)
        IF( IBCMX.LE.-3 ) THEN
          IBCCX = 1
          IBCMX = -IBCMX
          WRITE(IWR,'(A)') 'Cyclic Boundary Conditions'
        ELSEIF( IBCMX.GE.1 ) THEN
          IBCCX = 0
          WRITE(IWR,'(A)') 'Noncyclic Boundary Conditions'
        ELSEIF( IBCMX.EQ.0 ) THEN
          INDX = 4
          CHMSG = 'No Boundary Condition Times'
          CALL WRMSGS( INDX )
        ELSE
          INDX = 4
          CHMSG = 'Number of Cyclic Boundary Conditions Times < 3'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IBCMX.GT.LBTM ) THEN
          INDX = 5
          CHMSG = 'Number of Boundary Condition Times > LBTM'
          CALL WRMSGS( INDX )
        ENDIF
        BCTMO = -SMALL
        WRITE(IWR,'(A)') 'Boundary Condition Times and Variables:'
        DO 100 NTM = 1,IBCMX
          DO 40 M = 1,LBCV
            VAR(NTM,M) = 0.D+0
   40     CONTINUE
!
!---  Read, write, and convert boundary condition time, variables,
!     and units  ---
!
          CALL RDINPL( CHDUM )
          CALL LCASE( CHDUM )
          ISTART = 1
          VARB = 'Boundary Time'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,1))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',VAR(NTM,1)
          INDX = 0
          IUNS = 1
          CALL RDUNIT(UNTS,VAR(NTM,1),INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,1),', s)'
          IF( ITYP(IEQW).EQ.1 ) THEN
            VARB = 'Aqueous Pressure, '
            WRITE(IWR,'(2X,A,$)') VARB(1:IVR)
            ISX = ISTART
            ICX = ICOMMA
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
            ISTART = ISX
            ICOMMA = ICX
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
            INDX = 0
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', Pa)'
            VAR(NTM,2) = VAR(NTM,2) - PATM
          ELSEIF( ITYP(IEQW).EQ.2 ) THEN
            VARB = 'Volumetric Aqueous Flux, '
            WRITE(IWR,'(2X,A,$)') VARB(1:IVR)
            ISX = ISTART
            ICX = ICOMMA
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
            ISTART = ISX
            ICOMMA = ICX
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
            INDX = 0
            IUNM = 1
            IUNS = -1
            CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', m^3/s)'
          ELSEIF( ITYP(IEQW).EQ.3 ) THEN
            VARB = 'Aqueous Pressure'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            ISX = ISTART
            ICX = ICOMMA
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
            ISTART = ISX
            ICOMMA = ICX
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
            INDX = 0
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', Pa)'
            VAR(NTM,2) = VAR(NTM,2) - PATM
          ELSEIF( ITYP(IEQW).EQ.7 ) THEN
            VARB = 'Aqueous Pressure'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            ISX = ISTART
            ICX = ICOMMA
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
            ISTART = ISX
            ICOMMA = ICX
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
            INDX = 0
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', Pa)'
            VAR(NTM,2) = VAR(NTM,2) - PATM
          ELSEIF( ITYP(IEQW).EQ.11 ) THEN
            VARB = 'Base Aqueous Pressure, '
            WRITE(IWR,'(2X,A,$)') VARB(1:IVR)
            ISX = ISTART
            ICX = ICOMMA
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
            ISTART = ISX
            ICOMMA = ICX
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
            INDX = 0
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', Pa)'
            VAR(NTM,2) = VAR(NTM,2) - PATM
          ELSEIF( ITYP(IEQW).EQ.12 ) THEN
            VARB = 'Dummy Variable, '
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          ELSEIF( ITYP(IEQW).EQ.17 ) THEN
            VARB = 'Base Aqueous Pressure, '
            WRITE(IWR,'(2X,A,$)') VARB(1:IVR)
            ISX = ISTART
            ICX = ICOMMA
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
            ISTART = ISX
            ICOMMA = ICX
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
            INDX = 0
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', Pa)'
            VAR(NTM,2) = VAR(NTM,2) - PATM
!
!---      Aqueous potential evaporation  ---
!
          ELSEIF( ITYP(IEQW).EQ.24 ) THEN
            VARB = 'Potential Evaporation Rate'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            ISX = ISTART
            ICX = ICOMMA
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
            ISTART = ISX
            ICOMMA = ICX
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
            INDX = 0
            IUNM = 1
            IUNS = -1
            CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', m/s)'
            VARB = 'Maximum Capillary Head'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            ISX = ISTART
            ICX = ICOMMA
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
            ISTART = ISX
            ICOMMA = ICX
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,3))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,3)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,3),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,3),', m)'
!
!---      X-Y-Z Hydraulic gradient 
!
          ELSEIF( ITYP(IEQW).EQ.44 ) THEN
            VARB = 'Base Aqueous Pressure, '
            WRITE(IWR,'(2X,A,$)') VARB(1:IVR)
            ISX = ISTART
            ICX = ICOMMA
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
            ISTART = ISX
            ICOMMA = ICX
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
            INDX = 0
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', Pa)'
            VAR(NTM,2) = VAR(NTM,2) - PATM
            DO 44 I = 1,3
              CALL RDDPR(ISTART,ICOMMA,CHDUM,BCXYZG(NTM,I))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(2X,4A,2PE11.4,$)') CHLB(I),', ',
     &          UNTS(1:NCH),': ',BCXYZG(NTM,I)
              INDX = 0
              IUNM = -1
              CALL RDUNIT( UNTS,BCXYZG(NTM,I),INDX )
              WRITE(IWR,'(A,1PE11.4,A)') ' (',BCXYZG(NTM,I),', 1/m)'
   44       CONTINUE
!
!---      X-Y-Z Seepage face
!
          ELSEIF( ITYP(IEQW).EQ.45 ) THEN
            VARB = 'Base Aqueous Pressure, '
            WRITE(IWR,'(2X,A,$)') VARB(1:IVR)
            ISX = ISTART
            ICX = ICOMMA
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
            ISTART = ISX
            ICOMMA = ICX
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
            INDX = 0
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', Pa)'
            VAR(NTM,2) = VAR(NTM,2) - PATM
            DO 45 I = 1,3
              CALL RDDPR(ISTART,ICOMMA,CHDUM,BCXYZG(NTM,I))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(2X,4A,2PE11.4,$)') CHLB(I),', ',
     &          UNTS(1:NCH),': ',BCXYZG(NTM,I)
              INDX = 0
              IUNM = -1
              CALL RDUNIT( UNTS,BCXYZG(NTM,I),INDX )
              WRITE(IWR,'(A,1PE11.4,A)') ' (',BCXYZG(NTM,I),', 1/m)'
   45       CONTINUE
          ELSE
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          ENDIF
!
!---      Solute transport  ---
!
          IF( IEQC.GT.0 ) THEN
            DO 50 NSL = 1,NSOLU
              IF( ITYP(NSL+LUK).EQ.1 .OR. ITYP(NSL+LUK).EQ.19 ) THEN
                VARB = 'Volumetric Concentration, '
                ISX = ISTART
                ICX = ICOMMA
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
                ISTART = ISX
                ICOMMA = ICX
                CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,NSL+LBCU))
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                IDB = INDEX( SOLUT(NSL)(1:),'  ') - 1
                WRITE(IWR,'(2X,A,2X,3A,1PE11.4,$)') SOLUT(NSL)(1:IDB),
     &            VARB(1:IVR),UNTS(1:NCH),': ',VAR(NTM,NSL+LBCU)
                INDX = 0
                IUNM = -3
                CALL RDUNIT(UNTS,VAR(NTM,NSL+LBCU),INDX)
                WRITE(IWR,'(A,1PE11.4,A)') ' (',
     &            VAR(NTM,NSL+LBCU),', 1/m^3)'
              ELSEIF( ITYP(NSL+LUK).EQ.8 .OR. ITYP(NSL+LUK).EQ.23 ) THEN
                VARB = 'Aqueous-Phase Volumetric Concentration, '
                ISX = ISTART
                ICX = ICOMMA
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
                ISTART = ISX
                ICOMMA = ICX
                CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,NSL+LBCU))
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                IDB = INDEX( SOLUT(NSL)(1:),'  ') - 1
                WRITE(IWR,'(2X,A,2X,3A,1PE11.4,$)') SOLUT(NSL)(1:IDB),
     &            VARB(1:IVR),UNTS(1:NCH),': ',VAR(NTM,NSL+LBCU)
                INDX = 0
                IUNM = -3
                CALL RDUNIT(UNTS,VAR(NTM,NSL+LBCU),INDX)
                WRITE(IWR,'(A,1PE11.4,A)') ' (',
     &            VAR(NTM,NSL+LBCU),', 1/m^3)'
              ELSEIF( ITYP(NSL+LUK).EQ.12 ) THEN
                VARB = 'Solute Transport Input'
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              ELSEIF( ITYP(NSL+LUK).EQ.13 ) THEN
                VARB = 'Volumetric Concentration, '
                ISX = ISTART
                ICX = ICOMMA
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
                ISTART = ISX
                ICOMMA = ICX
                CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,NSL+LBCU))
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                IDB = INDEX( SOLUT(NSL)(1:),'  ') - 1
                WRITE(IWR,'(2X,A,2X,3A,1PE11.4,$)') SOLUT(NSL)(1:IDB),
     &            VARB(1:IVR),UNTS(1:NCH),': ',VAR(NTM,NSL+LBCU)
                INDX = 0
                IUNM = -3
                CALL RDUNIT(UNTS,VAR(NTM,NSL+LBCU),INDX)
                WRITE(IWR,'(A,1PE11.4,A)') ' (',
     &            VAR(NTM,NSL+LBCU),', 1/m^3)'
              ELSEIF( ITYP(NSL+LUK).EQ.14 ) THEN
                VARB = 'Aqueous-Phase Volumetric Concentration, '
                ISX = ISTART
                ICX = ICOMMA
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
                ISTART = ISX
                ICOMMA = ICX
                CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,NSL+LBCU))
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                IDB = INDEX( SOLUT(NSL)(1:),'  ') - 1
                WRITE(IWR,'(2X,A,2X,3A,1PE11.4,$)') SOLUT(NSL)(1:IDB),
     &            VARB(1:IVR),UNTS(1:NCH),': ',VAR(NTM,NSL+LBCU)
                INDX = 0
                IUNM = -3
                CALL RDUNIT(UNTS,VAR(NTM,NSL+LBCU),INDX)
                WRITE(IWR,'(A,1PE11.4,A)') ' (',
     &            VAR(NTM,NSL+LBCU),', 1/m^3)'
              ELSE
                VARB = 'Solute Transport Input'
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              ENDIF
   50       CONTINUE
          ENDIF

!
!---      Reactive species concentrations  ---
!
          IF( ISLC(40).EQ.1 ) THEN
!
!---        Loop over reactive species inputs  ---
!
            DO 62 NSPX = 1,IBCSPX(1)
              NSP = IBCSPX(NSPX+1)
              M = NSOLU+LBCU+NSPX
!
!---          Initial input line  ---
!
              IF( NSPX.EQ.1 ) THEN
                CALL RDINPL( CHDUM )
                CALL LCASE( CHDUM )
                ISTART = 1
              ENDIF
!
!---          Allow for returns in input lines  ---
!
              CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
              IF( INDX.EQ.0 ) THEN
                CALL RDINPL( CHDUM )
                CALL LCASE( CHDUM )
                ISTART = 1
              ENDIF
              IF( ITYP(NSOLU+LUK+1).EQ.8 
     &          .OR. ITYP(NSOLU+LUK+1).EQ.14
     &          .OR. ITYP(NSOLU+LUK+1).EQ.23 ) THEN
                VARB = 'Aqueous-Phase Concentration, '
                CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,M))
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                IDB = INDEX( SPNML(NSP)(1:),'  ') - 1
                WRITE(IWR,'(2X,A,2X,3A,1PE11.4,$)') SPNML(NSP)(1:IDB),
     &            VARB(1:IVR),UNTS(1:NCH),': ',VAR(NTM,M)
                INDX = 0
                IUNM = -3
                IUNMOL = 1
                CALL RDUNIT(UNTS,VAR(NTM,M),INDX)
!
!---            Convert aqueous concentration from kmol/m^3 to
!               mol/m^3  ---
!
                VAR(NTM,M) = VAR(NTM,M)*1.D+3
                WRITE(IWR,'(A,1PE11.4,A)') ' (',
     &            VAR(NTM,M),', mol/m^3)'
              ELSEIF( ITYP(NSOLU+LUK+1).EQ.12 ) THEN
                VARB = 'Dummy Variable, '
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              ELSE
                VARB = 'Dummy Variable, '
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              ENDIF
   62       CONTINUE
          ENDIF

!
!---  Check for nonascending boundary condition times  ---
!
          IF( VAR(NTM,1).LT.BCTMO ) THEN
            INDX = 4
            CHMSG = 'Boundary Condition Time Sequencing'
            CALL WRMSGS( INDX )
          ENDIF
          BCTMO = VAR(NTM,1)
  100   CONTINUE
!
!---    Assign values to boundary variables  ---
!
        DO 108 NTM = 1,IBCMX
          DO 102 M = 1,LBCU
            BC(M,NTM,NB) = VAR(NTM,M)
  102     CONTINUE
          DO 104 NSL = 1,NSOLU
            M = NSL+LBCU
            BC(M,NTM,NB) = VAR(NTM,M)
  104     CONTINUE

          IF( ISLC(40).EQ.1 ) THEN
            DO 106 NSPX = 1,IBCSPX(1)
              M = NSOLU+LBCU+NSPX
              BC(M,NTM,NB) = VAR(NTM,M)
  106       CONTINUE
          ENDIF

  108   CONTINUE
!
!---  Assign values to boundary variables  ---
!
        NBCL = 0
        DO 320 K = K1X,K2X
          DO 310 J = J1X,J2X
            DO 300 I = I1X,I2X
              IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
                READ(26,*,END=320) IX,JX,KX,IBCDX
                N = ND(IX,JX,KX)
                IF( NBCL.EQ.1 ) THEN
                  IBCLL(NB) = IX
                  JBCLL(NB) = JX
                  KBCLL(NB) = KX
                  MBCLL(NB) = IBCDX
                ENDIF
              ELSE
                N = ND(I,J,K)
                IX = I
                JX = J
                KX = K
              ENDIF
!
!---         Check for boundary applied to inactive nodes  ---
!
              IF( IXP(N).EQ.0 ) THEN
                WRITE(IWR,'(A,I9)') 'Boundary Condition Applied ' //
     &            'to an Inactive Node: ',N
                GOTO 300
              ENDIF
!
!---          Check for boundary applied to interior surfaces  ---
!
              IERR = 0
              IF( IBCDX.EQ.-3 .AND. KX.NE.1) THEN
                IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                  IERR = 1
                  WRITE(ISC,'(A)') 'Bottom Boundary'
                  WRITE(IWR,'(A)') 'Bottom Boundary'
                ENDIF
              ELSEIF( IBCDX.EQ.-2 .AND. JX.NE.1) THEN
                IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                  IERR = 1
                  WRITE(ISC,'(A)') 'South Boundary'
                  WRITE(IWR,'(A)') 'South Boundary'
                ENDIF
              ELSEIF( IBCDX.EQ.-1 .AND. IX.NE.1) THEN
                IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                  IERR = 1
                  WRITE(ISC,'(A)') 'West Boundary'
                  WRITE(IWR,'(A)') 'West Boundary'
                ENDIF
              ELSEIF( IBCDX.EQ.1 .AND. IX.NE.IFLD) THEN
                IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                  IERR = 1
                  WRITE(ISC,'(A)') 'East Boundary'
                  WRITE(IWR,'(A)') 'East Boundary'
                ENDIF
              ELSEIF( IBCDX.EQ.2 .AND. JX.NE.JFLD) THEN
                IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                  IERR = 1
                  WRITE(ISC,'(A)') 'North Boundary'
                  WRITE(IWR,'(A)') 'North Boundary'
                ENDIF
              ELSEIF( IBCDX.EQ.3 .AND. KX.NE.KFLD) THEN
                IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                  IERR = 1
                  WRITE(ISC,'(A)') 'Top Boundary'
                  WRITE(IWR,'(A)') 'Top Boundary'
                ENDIF
              ENDIF
!
!---          Report boundary error  ---
!
              IF( IERR.EQ.1 ) THEN
                WRITE(ISC,'(A,I9)') 'Node = ',N
                WRITE(IWR,'(A,I9)') 'Node = ',N
                WRITE(ISC,'(3(A,I9))') 'I = ',I,' J = ',J,' K = ',K
                WRITE(IWR,'(3(A,I9))') 'I = ',I,' J = ',J,' K = ',K
                INDX = 7
                IMSG = NBC
                CHMSG = 'Boundary Cond. Applied to an Interior Surface'
     &            //': Boundary Number'
                CALL WRMSGS( INDX )
              ENDIF
              NBCL = NBCL + 1
              NBC = NBC + 1
              IF( NBC.GT.LBC ) THEN
                INDX = 5
                CHMSG = 'Number of Boundary Condition Surfaces > '
     &            //'Parameter LBC'
                CALL WRMSGS( INDX )
              ENDIF
              IBCN(NBC) = N
              IBCC(NBC) = IBCCX
              IBCD(NBC) = IBCDX
              IBCT(IEQW,NBC) = ITYP(IEQW)
              IF( IEQC.GT.0 ) THEN
                DO 110 NSL = 1,NSOLU
                  IBCT(NSL+LUK,NBC) = ITYP(NSL+LUK)
  110           CONTINUE
              ENDIF

              IF( ISLC(40).EQ.1 ) THEN
                IBCT(NSOLU+LUK+1,NBC) = ITYP(NSOLU+LUK+1)
                DO 120 NSP = 1,LSPBC+1
                  IBCSP(NSP,NBC) = IBCSPX(NSP)
  120           CONTINUE
              ENDIF

              IBCM(NBC) = IBCMX
              IBCIN(NBC) = NB
!
!---  Assign hydraulic gradient type boundary condition
!     for the base surface  ---
!
              IF( ITYP(IEQW).EQ.11 ) THEN
                IF( NBCL.EQ.1 ) THEN
                  IBCT(IEQW,NBC) = -11
                  NBHG(1,NBC) = NBC
                ELSE
                  NBHG(1,NBC) = NBHG(1,NBC-1)
                ENDIF
              ENDIF
!
!---  Assign seepage face boundary condition
!     for the base surface  ---
!
              IF( ITYP(IEQW).EQ.17 ) THEN
                IF( NBCL.EQ.1 ) THEN
                  IBCT(IEQW,NBC) = -17
                  NBHG(1,NBC) = NBC
                ELSE
                  NBHG(1,NBC) = NBHG(1,NBC-1)
                ENDIF
              ENDIF
!
!---  Assign x-y-z hydraulic gradient type boundary condition
!     for the base surface  ---
!
              IF( ITYP(IEQW).EQ.44 ) THEN
                IF( NBCL.EQ.1 ) THEN
                  IBCT(IEQW,NBC) = -44
                  NBHG(1,NBC) = NBC
                ELSE
                  NBHG(1,NBC) = NBHG(1,NBC-1)
                ENDIF
              ENDIF
!
!---  Assign x-y-z seepage face boundary condition
!     for the base surface  ---
!
              IF( ITYP(IEQW).EQ.45 ) THEN
                IF( NBCL.EQ.1 ) THEN
                  IBCT(IEQW,NBC) = -45
                  NBHG(1,NBC) = NBC
                ELSE
                  NBHG(1,NBC) = NBHG(1,NBC-1)
                ENDIF
              ENDIF
!
!---  Check for double boundary conditions  ---
!
              DO 220 M = 1,NBC-1
                MB = IBCIN(M)
                IF( IBCN(M).EQ.N .AND. IBCD(M).EQ.IBCDX ) THEN
                  IF( (VAR(1,1).GT.BC(1,1,MB) .AND.
     &              VAR(1,1).LT.BC(1,IBCM(M),MB)) .OR.
     &              (VAR(IBCMX,1).GT.BC(1,1,MB) .AND.
     &              VAR(IBCMX,1).LT.BC(1,IBCM(M),MB)) ) THEN
                      INDX = 4
                      CHMSG = 'Multiple Boundary Conditions'
                      CALL WRMSGS( INDX )
                  ENDIF
                ENDIF
  220         CONTINUE
  300       CONTINUE
  310     CONTINUE
  320   CONTINUE
!
!---    Reset boundary surface types for x-y-z hydraulic gradient
!       and x-y-z seepage face boundaries  ---
!
        IF( ABS(ITYP(IEQW)).EQ.44 .OR. ABS(ITYP(IEQW)).EQ.45 ) THEN
          DO 370 NTM = 1,IBCMX 
            DO 360 ICNT = 1,3 
              JCNT = LBCV - 3 + ICNT
              BC(JCNT,NTM,MB) = BCXYZG(NTM,ICNT)
  360       CONTINUE
  370     CONTINUE
        ENDIF
        IF( INDEX(ADUM(1:),'file').NE.0 ) CLOSE(UNIT=26)
  400 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDBC1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDIC1
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
!     Water Mode
!
!     Read input file for initial conditions information.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 2, 1997.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE HYST
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
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,BDUM,FDUM,FMDUM,UNTS
      CHARACTER*24 CHLB(3)
      CHARACTER*512 CHDUM
      INTEGER IDOM(6)
      REAL*8 VAR(5)
      LOGICAL FCHK
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Data Statements---------------------------------!
!
      SAVE CHLB
      DATA CHLB /'X-Direction Gradient, ','Y-Direction Gradient, ',
     &           'Z-Direction Gradient, '/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDIC1'
      IAPE = 0
      IASE = 0
!
!---  Write card information to ouput file  ---
!
      CARD = 'Initial Conditions Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE(IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Restart file will be read for initial conditions  ---
!
      IF( IEO.EQ.2 ) THEN
        INDX = 2
        CALL RDRST(INDX)
        ISIC = 3
      ENDIF
!
!---  Read saturation initial condition option  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Initial Saturation Option: '
      CALL RDCHR(ISTART,ICOMMA,NCHA,CHDUM,ADUM)
      CALL RDCHR(ISTART,ICOMMA,NCHB,CHDUM,BDUM)
      IF( IEO.EQ.2 ) GOTO 10
      WRITE(IWR,'(/,A)') VARB(1:IVR)
      WRITE(IWR,'(2X,A)') ADUM
      WRITE(IWR,'(2X,A)') BDUM
!
!---  Reject cases when both aqueous saturation and moisture
!     content are specified  ---
!
      IF( INDEX(ADUM(1:),'aqueous saturation').NE.0 .AND.
     &  INDEX(ADUM(1:),'moisture content').NE.0 ) THEN
          INDX = 4
          CHMSG = 'Unrecognized Initial Saturation Option: '
     &      //ADUM(1:NCHA)//','//BDUM(1:NCHB)
          CALL WRMSGS( INDX )
      ENDIF
      IF( INDEX(BDUM(1:),'aqueous saturation').NE.0 .AND.
     &  INDEX(BDUM(1:),'moisture content').NE.0 ) THEN
          INDX = 4
          CHMSG = 'Unrecognized Initial Saturation Option: '
     &      //ADUM(1:NCHA)//','//BDUM(1:NCHB)
          CALL WRMSGS( INDX )
      ENDIF
      IF( INDEX(ADUM(1:),'aqueous saturation').NE.0 .OR.
     &  INDEX(ADUM(1:),'moisture content').NE.0 ) THEN
        IASE = 1
        IF( INDEX(BDUM(1:),'gas pressure').NE.0 ) THEN
          ISIC = 1
          IF( INDEX(BDUM(1:),'moisture content').NE.0 ) ISIC = 11
        ELSEIF( INDEX(BDUM(1:),'aqueous pressure').NE.0 ) THEN
          ISIC = 2
          IAPE = 1
          IF( INDEX(BDUM(1:),'moisture content').NE.0 ) ISIC = 12
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Initial Saturation Option: '
     &      //ADUM(1:NCHA)//','//BDUM(1:NCHB)
          CALL WRMSGS( INDX )
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'gas pressure').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'aqueous saturation').NE.0 .OR.
     &  INDEX(BDUM(1:),'moisture content').NE.0 ) THEN
          ISIC = 1
          IASE = 1
          IF( INDEX(BDUM(1:),'moisture content').NE.0 ) ISIC = 11
        ELSEIF( INDEX(BDUM(1:),'aqueous pressure').NE.0 ) THEN
          ISIC = 3
          IAPE = 1
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Initial Saturation Option: '
     &      //ADUM(1:NCHA)//','//BDUM(1:NCHB)
          CALL WRMSGS( INDX )
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'aqueous pressure').NE.0 ) THEN
        IAPE = 1
        IF( INDEX(BDUM(1:),'aqueous saturation').NE.0 .OR.
     &  INDEX(BDUM(1:),'moisture content').NE.0 ) THEN
          ISIC = 2
          IASE = 1
          IF( INDEX(BDUM(1:),'moisture content').NE.0 ) ISIC = 12
        ELSEIF( INDEX(BDUM(1:),'gas pressure').NE.0 ) THEN
          ISIC = 3
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Initial Saturation Option: '
     &      //ADUM(1:NCHA)//','//BDUM(1:NCHB)
          CALL WRMSGS( INDX )
        ENDIF
      ELSE
        INDX = 4
        CHMSG = 'Unrecognized Initial Saturation Option: '
     &    //ADUM(1:NCHA)//','//BDUM(1:NCHB)
        CALL WRMSGS( INDX )
      ENDIF
   10 CONTINUE
!
!---  Read initial conditions  ---
!
      WRITE(IWR,'(/,A)') 'Initial Condition Variable(s) and Domain(s)'
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Initial Condition Cards: '
      CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
      DO 1000 NL = 1, NLIN
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Initial Condition Variable: '
        CALL RDCHR(ISTART,ICOMMA,NCHA,CHDUM,ADUM)
        IF( IEO.NE.2 .AND. IEO.NE.4 ) THEN
          IF( INDEX( ADUM(1:),'aqueous pres' ).NE.0 ) THEN
            IF( ISIC.EQ.2 .OR. ISIC.EQ.3 .OR.
     &        ISIC.EQ.12 ) IAPE = 0
          ELSEIF( INDEX( ADUM(1:),'aqueous sat' ).NE.0 .OR.
     &      INDEX(ADUM(1:),'moisture content').NE.0 ) THEN
            IF( ISIC.EQ.1 .OR. ISIC.EQ.2 .OR.
     &        ISIC.EQ.11 .OR. ISIC.EQ.12 ) IASE = 0
          ENDIF
        ENDIF
        IF( INDEX( ADUM(1:),'overwrite').EQ.0 .AND.
     &    ( IEO.EQ.2 ) ) GOTO 1000
        IF( INDEX( ADUM(1:),'aqueous pres' ).NE.0 ) THEN
          VARB = 'Initial Aqueous Pressure'
          IUNM = -1
          IUNKG = 1
          IUNS = -2
        ELSEIF( INDEX( ADUM(1:),'gas pres' ).NE.0 ) THEN
          VARB = 'Initial Gas Pressure'
          IUNM = -1
          IUNKG = 1
          IUNS = -2
        ELSEIF( INDEX( ADUM(1:),'matrix pres' ).NE.0 ) THEN
          VARB = 'Initial Matrix Pressure'
          IUNM = -1
          IUNKG = 1
          IUNS = -2
        ELSEIF( INDEX( ADUM(1:),'imbibition' ).NE.0 .OR.
     &    INDEX( ADUM(1:),'wetting' ).NE.0 ) THEN
          IF( INDEX( ADUM(1:),'primary' ).NE.0 ) THEN
            VARB = 'Initial Primary Imbibition Path'
          ELSE
            VARB = 'Initial Imbibition Path'
          ENDIF
        ELSEIF( INDEX( ADUM(1:),'drainage' ).NE.0 .OR.
     &    INDEX( ADUM(1:),'drying' ).NE.0 ) THEN
          VARB = 'Initial Drainage Path'
        ELSEIF( INDEX( ADUM(1:),'temperature' ).NE.0 ) THEN
          VARB = 'Initial Temperature'
          IUNK = 1
        ELSEIF( INDEX( ADUM(1:),'aqueous sat' ).NE.0 ) THEN
          VARB = 'Initial Aqueous Saturation'
        ELSEIF( INDEX( ADUM(1:),'moisture cont' ).NE.0 ) THEN
          VARB = 'Initial Moisture Content'
        ELSEIF( INDEX( ADUM(1:),'relative' ).NE.0 .AND.
     &    INDEX( ADUM(1:),'trapped gas' ).NE.0 ) THEN
          VARB = 'Initial Relative Trapped Gas Saturation'
        ELSEIF( INDEX( ADUM(1:),'trapped gas' ).NE.0 ) THEN
          VARB = 'Initial Trapped Gas Saturation'
        ELSEIF( INDEX( ADUM(1:),'solute' ).NE.0 ) THEN
          VARB = 'Solute Name: '
          CALL RDCHR(ISTART,ICOMMA,NCHB,CHDUM,BDUM)
          VARB = 'Initial Solute Concentration'
          IUNM = -3

        ELSEIF( INDEX( ADUM(1:),'species' ).NE.0 ) THEN
          VARB = 'Species Name: '
          CALL RDCHR(ISTART,ICOMMA,NCHB,CHDUM,BDUM)
          VARB = 'Initial Species Concentration'
!
!---      Set species units  ---
!
          IUNMOL = 1
          IF( INDEX(ADUM(1:),'aqueous').NE.0 ) THEN
            IF( INDEX(ADUM(1:),'molal').NE.0 ) THEN
              IVAR = 3
              IUNKG = -1
            ELSE
              IVAR = 2
              IUNM = -3
            ENDIF
          ELSE
            IVAR = 1
            IUNM = -3
          ENDIF
          IF( IEO.EQ.2 ) IVAR = IVAR+10

        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Initial Condition Variable: '//
     &      ADUM(1:NCHA)
          CALL WRMSGS( INDX )
        ENDIF
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(1))
        CALL RDCHR(ISTART,ICOMMA,NCHU,CHDUM,UNTS)
!
!---  Read initial conditions input from an external file  ---
!
        IF( INDEX( ADUM(1:),'file' ).NE.0 ) THEN
          IF( INDEX( ADUM(1:),'binary' ).NE.0 ) THEN
            WRITE(IWR,'(2X,3A)') ADUM(1:NCHA),',',UNTS(1:NCHU)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
            NCH = INDEX(FDUM,'  ')-1
!
!---        Check for external file  ---
!
            INQUIRE( FILE=FDUM(1:NCH), FORM=FMDUM, EXIST=FCHK )
            IF( .NOT.FCHK ) THEN
              INDX = 4
              CHMSG = 'Missing Initial Conditions File: ' // FDUM(1:NCH)
              CALL WRMSGS( INDX )
            ELSEIF( FDUM.EQ.'formatted' ) THEN
              INDX = 4
              CHMSG = 'Initial Conditions File Format: ' // FDUM(1:NCH)
              CALL WRMSGS( INDX )
            ENDIF
            OPEN(UNIT=26,FILE=FDUM(1:NCH),STATUS='OLD',
     &        FORM='UNFORMATTED')
            WRITE(IWR,'(/,2A)') 'Initial Conditions File: ',FDUM(1:NCH)
          ELSEIF( INDEX( ADUM(1:),'ascii' ).NE.0 ) THEN
            WRITE(IWR,'(2X,3A)') ADUM(1:NCHA),',',UNTS(1:NCHU)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
            NCH = INDEX(FDUM,'  ')-1
!
!---        Check for external file  ---
!
            INQUIRE( FILE=FDUM(1:NCH), FORM=FMDUM, EXIST=FCHK )
            IF( .NOT.FCHK ) THEN
              INDX = 4
              CHMSG = 'Missing Initial Conditions File: ' // FDUM(1:NCH)
              CALL WRMSGS( INDX )
            ELSEIF( FDUM.EQ.'unformatted' ) THEN
              INDX = 4
              CHMSG = 'Initial Conditions File Format: ' // FDUM(1:NCH)
              CALL WRMSGS( INDX )
            ENDIF
            OPEN(UNIT=26,FILE=FDUM(1:NCH),STATUS='OLD',
     &        FORM='FORMATTED')
            WRITE(IWR,'(/,2A)') 'Initial Conditions File: ',FDUM(1:NCH)
          ELSE
            WRITE(IWR,'(2X,4A,1PE11.4)') ADUM(1:NCHA),
     &        ' (Default Value), ',UNTS(1:NCHU),': ',VAR(1)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
            NCH = INDEX(FDUM,'  ')-1
!
!---        Check for external file  ---
!
            INQUIRE( FILE=FDUM(1:NCH), FORM=FMDUM, EXIST=FCHK )
            IF( .NOT.FCHK ) THEN
              INDX = 4
              CHMSG = 'Missing Initial Conditions File: ' // FDUM(1:NCH)
              CALL WRMSGS( INDX )
            ELSEIF( FDUM.EQ.'unformatted' ) THEN
              INDX = 4
              CHMSG = 'Initial Conditions File Format: ' // FDUM(1:NCH)
              CALL WRMSGS( INDX )
            ENDIF
            OPEN(UNIT=26,FILE=FDUM(1:NCH),STATUS='OLD',FORM='FORMATTED')
            WRITE(IWR,'(/,2A)') 'Initial Conditions File: ',FDUM(1:NCH)
            INDX = 0
            CALL RDUNIT( UNTS,VAR(1),INDX )
          ENDIF
!
!---  Read initial conditions according to rock/soil zonations  ---
!
        ELSEIF( INDEX( ADUM(1:),'rock' ).NE.0 .OR.
     &    INDEX( ADUM(1:),'zonation' ).NE.0 ) THEN
          VARB = 'Rock/Soil Name'
          CALL RDCHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
!
!---  Search known rock types for a matching type ---
!
          DO 20 M = 1, NROCK
            IF( FDUM .EQ. ROCK(M)) THEN
            IROCK = M
            GOTO 30
          ENDIF
   20     CONTINUE
          INDX = 2
          CHMSG = 'Unrecognized Rock/Soil Type: '//FDUM
          CALL WRMSGS( INDX )
          GOTO 1000
   30     CONTINUE
          WRITE(IWR,'(2X,3A,1PE11.4,2A)') ADUM(1:NCHA),UNTS(1:NCHU),
     &      ': ',VAR(1),' Rock/Soil Type: ',FDUM(1:NCHF)
          INDX = 0
          CALL RDUNIT( UNTS,VAR(1),INDX )
!
!---  Read initial condtions input from the input file  ---
!
        ELSE
          WRITE(IWR,'(2X,4A,1PE11.4)') ADUM(1:NCHA),', ',
     &      UNTS(1:NCHU),': ',VAR(1)
          INDX = 0
          CALL RDUNIT( UNTS,VAR(1),INDX )
          INDX = 2
          VAR(5) = 1.D+0
          NCH = INDEX( UNTS,'  ' ) - 1
          IF( UNTS(1:NCH).EQ.'f' .OR. UNTS(1:NCH).EQ.'r' ) THEN
            VAR(5) = VAR(5)/1.8D+0
          ELSEIF( UNTS(1:NCH).EQ.'c' .OR. UNTS(1:NCH).EQ.'k' ) THEN
            VAR(5) = 1.D+0
          ELSE
            CALL RDUNIT( UNTS,VAR(5),INDX )
          ENDIF
          VARB = 'Initial Condition Variable Gradient: '
          DO 100 I = 2,4
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(I))
            VAR(I) = VAR(I)*VAR(5)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,4A,1PE11.4,$)') CHLB(I-1),', ',UNTS(1:NCH),
     &        ': ',VAR(I)
            INDX = 0
            IUNM = -1
            CALL RDUNIT( UNTS,VAR(I),INDX )
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(I),', 1/m)'
  100     CONTINUE
!
!---  Read domain indices  ---
!
          VARB = 'Initial Condition Domain Index: '
          DO 200 I = 1, 6
            CALL RDINT(ISTART,ICOMMA,CHDUM,IDOM(I))
  200     CONTINUE
        ENDIF
!
!---  Read variables  ---
!
        IF( INDEX(ADUM(1:),'aqueous pres').NE.0 ) THEN
          ADDER = -PATM
          INDX = 2
          IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
              CALL RDINBS( PL,ADDER,UNTS,INDX )
            ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
              CALL RDINAS( PL,ADDER,UNTS,INDX )
            ELSE
              CALL RDINFS( PL,VAR,ADDER,UNTS,INDX )
            ENDIF
            CLOSE(UNIT=26)
          ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR.
     &      INDEX(ADUM(1:),'zonation').NE.0 )  THEN
            CALL RDINZS( PL,VAR(1),ADDER,IROCK,INDX )
          ELSE
            CALL RDINIS( PL,VAR,ADDER,IDOM,INDX )
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'gas pres').NE.0 ) THEN
          ADDER = -PATM
          INDX = 2
          IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
              CALL RDINBS( PG,ADDER,UNTS,INDX )
            ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
              CALL RDINAS( PG,ADDER,UNTS,INDX )
            ELSE
              CALL RDINFS( PG,VAR,ADDER,UNTS,INDX )
            ENDIF
            CLOSE(UNIT=26)
          ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR.
     &      INDEX(ADUM(1:),'zonation').NE.0 )  THEN
            CALL RDINZS( PG,VAR(1),ADDER,IROCK,INDX )
          ELSE
            CALL RDINIS( PG,VAR,ADDER,IDOM,INDX )
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'matrix pres').NE.0 ) THEN
          ADDER = -PATM
          INDX = 2
          IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
              CALL RDINBS( PN,ADDER,UNTS,INDX )
            ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
              CALL RDINAS( PN,ADDER,UNTS,INDX )
            ELSE
              CALL RDINFS( PN,VAR,ADDER,UNTS,INDX )
            ENDIF
            CLOSE(UNIT=26)
          ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR.
     &      INDEX(ADUM(1:),'zonation').NE.0 )  THEN
            CALL RDINZS( PN,VAR(1),ADDER,IROCK,INDX )
          ELSE
            CALL RDINIS( PN,VAR,ADDER,IDOM,INDX )
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'aqueous sat').NE.0 ) THEN
          ADDER = 0.D+0
          INDX = 2
          IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
            IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
              CALL RDINBS( SL,ADDER,UNTS,INDX )
            ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
              CALL RDINAS( SL,ADDER,UNTS,INDX )
            ELSE
              CALL RDINFS( SL,VAR,ADDER,UNTS,INDX )
            ENDIF
            CLOSE(UNIT=26)
          ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR.
     &      INDEX(ADUM(1:),'zonation').NE.0 )  THEN
            CALL RDINZS( SL,VAR(1),ADDER,IROCK,INDX )
          ELSE
            CALL RDINIS( SL,VAR,ADDER,IDOM,INDX )
          ENDIF
!
!---    Use the variable RHOG to temporarily hold the initial
!       moisture content  ---
!
        ELSEIF( INDEX(ADUM(1:),'moisture cont').NE.0 ) THEN
          ADDER = 0.D+0
          INDX = 2
          IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
            IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
              CALL RDINBS( RHOG,ADDER,UNTS,INDX )
            ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
              CALL RDINAS( RHOG,ADDER,UNTS,INDX )
            ELSE
              CALL RDINFS( RHOG,VAR,ADDER,UNTS,INDX )
            ENDIF
            CLOSE(UNIT=26)
          ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR.
     &      INDEX(ADUM(1:),'zonation').NE.0 )  THEN
            CALL RDINZS( RHOG,VAR(1),ADDER,IROCK,INDX )
          ELSE
            CALL RDINIS( RHOG,VAR,ADDER,IDOM,INDX )
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'imbibition').NE.0 .OR.
     &    INDEX(ADUM(1:),'wetting').NE.0 ) THEN
          ADDER = 0.D+0
          IF( INDEX(ADUM(1:),'primary').NE.0 .OR.
     &      INDEX(ADUM(1:),'main').NE.0 ) THEN
            VAR(1) = 2.D+0
          ELSE
            VAR(1) = 1.D+0
          ENDIF
          VAR(2) = 0.D+0
          VAR(3) = 0.D+0
          VAR(4) = 0.D+0
          INDX = 1
          IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
            IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
              CALL RDINBS( SG,ADDER,UNTS,INDX )
            ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
              CALL RDINAS( SG,ADDER,UNTS,INDX )
            ELSE
              CALL RDINFS( SG,VAR,ADDER,UNTS,INDX )
            ENDIF
            CLOSE(UNIT=26)
          ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR.
     &      INDEX(ADUM(1:),'zonation').NE.0 )  THEN
            CALL RDINZS( SG,VAR(1),ADDER,IROCK,INDX )
          ELSE
            CALL RDINIS( SG,VAR,ADDER,IDOM,INDX )
          ENDIF
          DO 202 N = 1,NFLD
            IF( IXP(N).EQ.0 ) GOTO 202
            IPH(2,N) = INT( SG(INDX,N) )
  202     CONTINUE
        ELSEIF( INDEX(ADUM(1:),'drainage').NE.0 .OR.
     &    INDEX(ADUM(1:),'drying').NE.0 ) THEN
          ADDER = 0.D+0
          VAR(1) = -1.D+0
          VAR(2) = 0.D+0
          VAR(3) = 0.D+0
          VAR(4) = 0.D+0
          INDX = 1
          IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
            IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
              CALL RDINBS( SG,ADDER,UNTS,INDX )
            ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
              CALL RDINAS( SG,ADDER,UNTS,INDX )
            ELSE
              CALL RDINFS( SG,VAR,ADDER,UNTS,INDX )
            ENDIF
            CLOSE(UNIT=26)
          ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR.
     &      INDEX(ADUM(1:),'zonation').NE.0 )  THEN
            CALL RDINZS( SG,VAR(1),ADDER,IROCK,INDX )
          ELSE
            CALL RDINIS( SG,VAR,ADDER,IDOM,INDX )
          ENDIF
          DO 204 N = 1,NFLD
            IF( IXP(N).EQ.0 ) GOTO 204
            IPH(2,N) = INT( SG(INDX,N) )
  204     CONTINUE
        ELSEIF( INDEX(ADUM(1:),'temperature').NE.0 ) THEN
          ADDER = 0.D+0
          INDX = 2
          IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
            IUNK = 1
            IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
              CALL RDINBS( T,ADDER,UNTS,INDX )
            ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
              CALL RDINAS( T,ADDER,UNTS,INDX )
            ELSE
              CALL RDINFS( T,VAR,ADDER,UNTS,INDX )
            ENDIF
            CLOSE(UNIT=26)
          ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR.
     &      INDEX(ADUM(1:),'zonation').NE.0 )  THEN
            CALL RDINZS( T,VAR(1),ADDER,IROCK,INDX )
          ELSE
            CALL RDINIS( T,VAR,ADDER,IDOM,INDX )
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'relative trapped gas').NE.0 .AND.
     &    INDEX(ADUM(1:),'trapped gas').NE.0 ) THEN
          ADDER = 1.D+2
          INDX = 2
          IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
            IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
              CALL RDINBS( SGT,ADDER,UNTS,INDX )
            ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
              CALL RDINAS( SGT,ADDER,UNTS,INDX )
            ELSE
              CALL RDINFS( SGT,VAR,ADDER,UNTS,INDX )
            ENDIF
            CLOSE(UNIT=26)
          ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR.
     &      INDEX(ADUM(1:),'zonation').NE.0 )  THEN
            CALL RDINZS( SGT,VAR(1),ADDER,IROCK,INDX )
          ELSE
            CALL RDINIS( SGT,VAR,ADDER,IDOM,INDX )
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'trapped gas').NE.0 ) THEN
          ADDER = 0.D+0
          INDX = 2
          IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
            IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
              CALL RDINBS( SG,ADDER,UNTS,INDX )
            ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
              CALL RDINAS( SG,ADDER,UNTS,INDX )
            ELSE
              CALL RDINFS( SG,VAR,ADDER,UNTS,INDX )
            ENDIF
            CLOSE(UNIT=26)
          ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR.
     &      INDEX(ADUM(1:),'zonation').NE.0 )  THEN
            CALL RDINZS( SG,VAR(1),ADDER,IROCK,INDX )
          ELSE
            CALL RDINIS( SG,VAR,ADDER,IDOM,INDX )
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'solute').NE.0 ) THEN
          IF( INDEX(ADUM(1:),'aqueous equ').NE.0 ) THEN
            IVAR = 3
          ELSEIF( INDEX(ADUM(1:),'aqueous').NE.0 ) THEN
            IVAR = 2
          ELSE
            IVAR = 1
          ENDIF
          IF( INDEX( UNTS(1:),'bd' ).NE.0 ) IVAR = -IVAR
          DO 220 NSL = 1,NSOLU
            IDB = INDEX(SOLUT(NSL)(1:),'  ') - 1
            IF( BDUM(1:NCHB).EQ.SOLUT(NSL)(1:IDB) ) THEN
              ADDER = 0.D+0
              IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
                IUNM = -3
                IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
                  CALL RDINBP( C(1,NSL),ADDER,ICT(1,NSL),IVAR,UNTS )
                ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
                  CALL RDINAP( C(1,NSL),ADDER,ICT(1,NSL),IVAR,UNTS )
                ELSE
                  CALL RDINFP( C(1,NSL),VAR,ADDER,ICT(1,NSL),IVAR,UNTS )
                ENDIF
                CLOSE(UNIT=26)
              ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR.
     &          INDEX(ADUM(1:),'zonation').NE.0 )  THEN
                CALL RDINZP( C(1,NSL),VAR(1),ADDER,ICT(1,NSL),
     &            IVAR,IROCK )
              ELSE
                CALL RDINIP( C(1,NSL),VAR,ADDER,ICT(1,NSL),IVAR,IDOM )
              ENDIF
              GOTO 230
            ENDIF
  220     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Solute Name: '//BDUM(1:NCHB)
          CALL WRMSGS( INDX )
  230     CONTINUE

        ELSEIF( INDEX(ADUM(1:),'specie').NE.0 ) THEN
          ADDER = 0.D+0
!
!---      Conservation- or kinetic-component species  ---
!
          IF( INDEX( BDUM(1:),'total_' ).NE.0 ) THEN
            DO 300 NSLX = NSOLU+1,NSOLU+NEQC+NEQK
              IDB = INDEX(SOLUT(NSLX)(1:),'  ') - 1
              IF( BDUM(1:NCHB).EQ.SOLUT(NSLX) ) THEN
                NSL = NSLX
                GOTO 340
              ENDIF
  300       CONTINUE
          ENDIF
!
!---      Aqueous reactive species  ---
!
          DO 310 NSPX = 1,NSPL
            IDB = INDEX(SPNML(NSPX)(1:),'  ') - 1
            IF( BDUM(1:NCHB).EQ.SPNML(NSPX)(1:IDB) ) THEN
              NSP = NSPX
              GOTO 340
            ENDIF
  310     CONTINUE
!
!---      Solid reactive species  ---
!
          DO 320 NSPX = 1,NSPS
            IDB = INDEX(SPNMS(NSPX)(1:),'  ') - 1
            IF( BDUM(1:NCHB).EQ.SPNMS(NSPX)(1:IDB) ) THEN
              NSP = NSPX + NSPL
!
!---          Verify that solid-species is not a mineral  ---
!
              IF( ISP_MN(NSP).EQ.1 ) THEN
                INDX = 4
                CHMSG = 'Solid-Species Mineral ' // 
     &            '(see Lithology Card): ' // BDUM(1:NCHB)
                CALL WRMSGS( INDX )
              ENDIF
              GOTO 340
            ENDIF
  320     CONTINUE
!
!---      Exchange reactive species  ---
!
          DO 325 NSPX = 1,NSPE
            IDB = INDEX(SPNME(NSPX)(1:),'  ') - 1
            IF( BDUM(1:NCHB).EQ.SPNME(NSPX)(1:IDB) ) THEN
              NSP = NSPX + NSPL + NSPS
              GOTO 340
            ENDIF
  325     CONTINUE
!
!---      pH  ---
!
          IF( BDUM(1:NCHB).EQ.'ph' .AND. ISPLK(1).NE.0 ) THEN
            NSP = MOD(ISPLK(1),1000)
            ISPLK(1) = ISPLK(1) + 1000
            IVAR = 2
            IF( IEO.EQ.2 ) IVAR = IVAR+10
            ADDER = 7.D+0
!
!---        Verify that species linked to pH is a conservation
!           component species  ---
!
            DO 330 NEQ = 1,NEQC
              IF( NSP.EQ.IEQ_C(2,NEQ) ) GOTO 340
  330       CONTINUE
            INDX = 4
            CHMSG = 'pH Species not a Conservation ' //
     &        'Component Species: ' // BDUM(1:NCHB)
            CALL WRMSGS( INDX )
          ENDIF
          INDX = 4
          CHMSG = 'Unrecognized Reactive Species: ' // BDUM(1:NCHB)
          CALL WRMSGS( INDX )
  340     CONTINUE
!
!---      Conservation- or kinetic-component species  ---
!
          IF( INDEX( BDUM(1:),'total_' ).NE.0 ) THEN
            IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
              IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
                CALL RDINBP( C(1,NSL),ADDER,ICT(1,NSL),IVAR,UNTS )
              ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
                CALL RDINAP( C(1,NSL),ADDER,ICT(1,NSL),IVAR,UNTS )
              ELSE
                CALL RDINFP( C(1,NSL),VAR,ADDER,ICT(1,NSL),
     &            IVAR,UNTS )
              ENDIF
              CLOSE(UNIT=26)
            ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR.
     &        INDEX(ADUM(1:),'zonation').NE.0 )  THEN
              CALL RDINZP( C(1,NSL),VAR(1),ADDER,ICT(1,NSL),
     &          IVAR,IROCK )
            ELSE
              CALL RDINIP( C(1,NSL),VAR,ADDER,ICT(1,NSL),
     &          IVAR,IDOM )
            ENDIF
          ELSE
            IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
              IF( INDEX(ADUM(1:),'binary').NE.0 ) THEN
                CALL RDINBP( SP_C(1,NSP),ADDER,IC_SP(1,NSP),IVAR,UNTS )
              ELSEIF( INDEX(ADUM(1:),'ascii').NE.0 ) THEN
                CALL RDINAP( SP_C(1,NSP),ADDER,IC_SP(1,NSP),IVAR,UNTS )
              ELSE
                CALL RDINFP( SP_C(1,NSP),VAR,ADDER,IC_SP(1,NSP),
     &            IVAR,UNTS )
              ENDIF
              CLOSE(UNIT=26)
            ELSEIF( INDEX(ADUM(1:),'rock').NE.0 .OR.
     &        INDEX(ADUM(1:),'zonation').NE.0 )  THEN
              CALL RDINZP( SP_C(1,NSP),VAR(1),ADDER,IC_SP(1,NSP),
     &          IVAR,IROCK )
            ELSE
              CALL RDINIP( SP_C(1,NSP),VAR,ADDER,IC_SP(1,NSP),
     &          IVAR,IDOM )
            ENDIF
          ENDIF

        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Initial Condition Variable: '//
     &      ADUM(1:NCHA)
          CALL WRMSGS( INDX )
        ENDIF
 1000 CONTINUE
      IF( IAPE.NE.0 ) THEN
        INDX = 4
        CHMSG = 'Undeclared Initial Aqueous Pressure'
        CALL WRMSGS( INDX )
      ELSEIF( IASE.NE.0 ) THEN
        INDX = 4
        CHMSG = 'Undeclared Initial Aqueous Saturation'
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDIC1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINPT1
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
!     Water Mode
!
!     Read input file cards.
!     Direct control to card reader subroutines.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.




!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GRID
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
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDINPT1'
!
!---  Write header line to output file  ---
!
      WRITE(IWR,'(/,A)') ' --- Input File Record ---'
!
!---  Initial aqueous relative permeability tensor indices  ---
!
      IRPLX = 0
      IRPLY = 0
      IRPLZ = 0
!
!---  Search input file for simulation title card  ---
!
  100 CONTINUE
  109 READ(IRD,'(A)', END=110) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 109
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'simulation').NE.0 ) THEN
        CALL RDSIMU
        REWIND(IRD)
        GOTO 200
      ELSE
        GOTO 100
      ENDIF
  110 CONTINUE
      INDX = 18
      CHMSG = 'Missing Simulation Title Card'
      CALL WRMSGS( INDX )
!
!---  Search input file for solution control card  ---
!
  200 CONTINUE
  209 READ(IRD,'(A)', END=210) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 209
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'solution').NE.0 ) THEN
        CALL RDSOLU
        IF( IOM.NE.1 ) THEN
          INDX = 18
          CHMSG = 'Incompatible Operational Mode'
          CALL WRMSGS( INDX )
        ENDIF
        REWIND(IRD)
        GOTO 300
      ELSE
        GOTO 200
      ENDIF
  210 CONTINUE
      INDX = 18
      CHMSG = 'Missing Solution Control Card'
      CALL WRMSGS( INDX )
!
!---  Search input file for grid card  ---
!
  300 CONTINUE
  309 READ(IRD,'(A)', END=310) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 309
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'grid').NE.0 ) THEN
        CALL RDGRID
        REWIND(IRD)
        GOTO 400
      ELSE
        GOTO 300
      ENDIF
  310 CONTINUE
      INDX = 18
      CHMSG = 'Missing Grid Card'
      CALL WRMSGS( INDX )
!
!---  Search input file for rock/soil zonation card  ---
!
  400 CONTINUE
  409 READ(IRD,'(A)', END=410) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 409
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'rock/soil').NE.0 ) THEN
        CALL RDROCK
        REWIND(IRD)
        GOTO 500
      ELSE
        GOTO 400
      ENDIF
  410 CONTINUE
      INDX = 4
      CHMSG = 'Missing Rock/Soil Zonation Card'
      CALL WRMSGS( INDX )
!
!---  Search input file for inactive nodes card  ---
!
  500 CONTINUE
  509 READ(IRD,'(A)', END=510) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 509
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'inactive').NE.0 ) THEN
        CALL RDINAC
        REWIND(IRD)
        GOTO 600
      ELSE
        GOTO 500
      ENDIF
  510 CONTINUE
      INDX = 1
      CHMSG = 'Missing Inactive Nodes Card'
      CALL WRMSGS( INDX )
      REWIND(IRD)
!
!---  Search input file for mechanical properties card  ---
!
  600 CONTINUE
  609 READ(IRD,'(A)', END=610) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 609
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'mechanical').NE.0 ) THEN
        CALL RDMECH
        REWIND(IRD)
        GOTO 700
      ELSE
        GOTO 600
      ENDIF
  610 CONTINUE
      INDX = 18
      CHMSG = 'Missing Mechanical Properties Card'
      CALL WRMSGS( INDX )
!
!---  Search input file for hydraulic properties card  ---
!
  700 CONTINUE
  709 READ(IRD,'(A)', END=710) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 709
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'hydraulic').NE.0 ) THEN
        CALL RDHYDR
        REWIND(IRD)
        GOTO 800
      ELSE
        GOTO 700
      ENDIF
  710 CONTINUE
      INDX = 18
      CHMSG = 'Missing Hydraulic Properties Card'
      CALL WRMSGS( INDX )
!
!---  Search input file for saturation function card  ---
!
  800 CONTINUE
  809 READ(IRD,'(A)', END=810) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 809
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'saturation').NE.0 ) THEN
        CALL RDSP1
        REWIND(IRD)
        GOTO 900
      ELSE
        GOTO 800
      ENDIF
  810 CONTINUE
      INDX = 18
      CHMSG = 'Missing Saturation Function Card'
      CALL WRMSGS( INDX )
!
!---  Search input file for aqueous relative permeability cards  ---
!
  900 CONTINUE
  909 READ(IRD,'(A)', END=910) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 909
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'aqueous rel').NE.0 .AND.
     &    INDEX(CHDUM(2:),'x-aqueous rel').EQ.0 .AND.
     &    INDEX(CHDUM(2:),'y-aqueous rel').EQ.0 .AND.
     &    INDEX(CHDUM(2:),'z-aqueous rel').EQ.0 ) THEN
        CALL RDLRP
        IRPLX = 1
        IRPLY = 1
        IRPLZ = 1
        REWIND(IRD)
        GOTO 920
      ELSE
        GOTO 900
      ENDIF
  910 CONTINUE
      REWIND( IRD )
  920 CONTINUE
  929 READ(IRD,'(A)', END=930) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 929
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'x-aqueous rel').NE.0 ) THEN
        ITX = 1
        CALL RDLRPT( ITX )
        IRPLX = 1
        REWIND(IRD)
        GOTO 940
      ELSE
        GOTO 920
      ENDIF
  930 CONTINUE
      REWIND( IRD )
  940 CONTINUE
  949 READ(IRD,'(A)', END=950) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 949
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'y-aqueous rel').NE.0 ) THEN
        ITX = 2
        CALL RDLRPT( ITX )
        IRPLY = 1
        REWIND(IRD)
        GOTO 960
      ELSE
        GOTO 940
      ENDIF
  950 CONTINUE
      REWIND( IRD )
  960 CONTINUE
  969 READ(IRD,'(A)', END=970) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 969
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'z-aqueous rel').NE.0 ) THEN
        ITX = 3
        CALL RDLRPT( ITX )
        IRPLZ = 1
        REWIND(IRD)
        GOTO 970
      ELSE
        GOTO 960
      ENDIF
  970 CONTINUE
      REWIND( IRD )
      IF( IRPLX.EQ.0 .AND. IRPLY.EQ.0 .AND. IRPLZ.EQ.0 ) THEN
        INDX = 18
        CHMSG = 'Missing Aqueous Relative Permeability Card'
        CALL WRMSGS( INDX )
      ELSE
        IF( IRPLX.EQ.0 .AND. IFLD.GT.1 ) THEN
          INDX = 18
          CHMSG = 'Missing X-Aqueous Relative Permeability Card'
          CALL WRMSGS( INDX )
        ELSEIF( IRPLY.EQ.0 .AND. JFLD.GT.1 ) THEN
          INDX = 18
          CHMSG = 'Missing Y-Aqueous Relative Permeability Card'
          CALL WRMSGS( INDX )
        ELSEIF( IRPLZ.EQ.0 .AND. KFLD.GT.1 ) THEN
          INDX = 18
          CHMSG = 'Missing Z-Aqueous Relative Permeability Card'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IRPLX.EQ.0 .AND. IFLD.EQ.1 ) THEN
          INDX = 1
          CHMSG = 'Missing X-Aqueous Relative Permeability Card'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IRPLY.EQ.0 .AND. JFLD.EQ.1 ) THEN
          INDX = 1
          CHMSG = 'Missing Y-Aqueous Relative Permeability Card'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IRPLZ.EQ.0 .AND. KFLD.EQ.1 ) THEN
          INDX = 1
          CHMSG = 'Missing Z-Aqueous Relative Permeability Card'
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!!
!!---  Search input file for aqueous relative permeability card  ---
!!
!  900 CONTINUE
!  909 READ(IRD,'(A)', END=910) CHDUM
!      IF( CHDUM(1:1).EQ.'#' ) GOTO 909
!      CALL LCASE( CHDUM )
!      IF( CHDUM(1:1).EQ.'~' .AND.
!     &    INDEX(CHDUM(2:),'aqueous rel').NE.0 ) THEN
!        CALL RDLRP
!        REWIND(IRD)
!        GOTO 1000
!      ELSE
!        GOTO 900
!      ENDIF
!  910 CONTINUE
!      INDX = 18
!      CHMSG = 'Missing Aqueous Relative Permeability Card'
!      CALL WRMSGS( INDX )
!
!---  Search input file for scaling card  ---
!
 1000 CONTINUE
      IF( ISLC(19).EQ.1 ) THEN
 1009   READ(IRD,'(A)', END=1010) CHDUM
        IF( CHDUM(1:1).EQ.'#' ) GOTO 1009
        CALL LCASE( CHDUM )
        IF( CHDUM(1:1).EQ.'~' .AND.
     &      INDEX(CHDUM(2:),'scaling').NE.0 ) THEN
          CALL RDSCLF1
          REWIND(IRD)
          GOTO 2000
        ELSE
          GOTO 1000
        ENDIF
 1010   CONTINUE
        INDX = 18
        CHMSG = 'Missing Scaling Card'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Search input file for solute/fluid interaction card --
!
 2000 CONTINUE
 2009 READ(IRD,'(A)', END=2010) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 2009
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'solute/fluid').NE.0 ) THEN
        CALL RDTF1
        REWIND(IRD)
        GOTO 2100
      ELSE
        GOTO 2000
      ENDIF
 2010 CONTINUE
      IF( IEQC.EQ.0 ) THEN
        REWIND(IRD)
      ELSE
        INDX = 18
        CHMSG = 'Missing Solute/Fluid Interactions Card'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Search input file for solute/porous media interaction card --
!
 2100 CONTINUE
 2109 READ(IRD,'(A)', END=2110) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 2109
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'solute/porous').NE.0 ) THEN
        CALL RDTP1
        REWIND(IRD)
        GOTO 2200
      ELSE
        GOTO 2100
      ENDIF
 2110 CONTINUE
      IF( IEQC.EQ.0 .AND. ISLC(40).EQ.0 ) THEN
        REWIND(IRD)
      ELSE
        INDX = 18
        CHMSG = 'Missing Solute/Porous Media Card'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Skip reaction and equation cards, no
!     reactive transport  ---
!
 2200 CONTINUE

!      IF( ISLC(40).EQ.0 ) GOTO 3000
!
!---  Search input file for aqueous species card  ---
!
 2209 READ(IRD,'(A)', END=2210) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 2209
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &  INDEX(CHDUM(2:),'aqueous').NE.0 .AND.
     &  INDEX(CHDUM(2:),'specie').NE.0) THEN
        CALL RDAQSP
        REWIND(IRD)
        GOTO 2300
      ELSE
        GOTO 2209
      ENDIF
 2210 CONTINUE
      REWIND(IRD)
      IF( ISLC(40).GT.0 ) THEN
        INDX = 1
        CHMSG = 'Missing Aqueous Species Card'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Search input file for solid species card  ---
!
 2300 CONTINUE
 2309 READ(IRD,'(A)', END=2310) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 2309
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &  INDEX(CHDUM(2:),'solid').NE.0 .AND.
     &  INDEX(CHDUM(2:),'specie').NE.0) THEN
        CALL RDSDSP
        REWIND(IRD)
        GOTO 2400
      ELSE
        GOTO 2300
      ENDIF
 2310 CONTINUE
      REWIND(IRD)
      IF( ISLC(40).GT.0 ) THEN
        INDX = 1
        CHMSG = 'Missing Solid Species Card'
        CALL WRMSGS( INDX )
      ENDIF
      REWIND(IRD)
 2400 CONTINUE
!
!---  Search input file for exchanged species card  ---
!
 2409 READ(IRD,'(A)', END=2410) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 2409
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &  INDEX(CHDUM(2:),'excha').NE.0 .AND.
     &  INDEX(CHDUM(2:),'specie').NE.0) THEN
        CALL RDEXSP
        REWIND(IRD)
        GOTO 2420
      ELSE
        GOTO 2400
      ENDIF
 2410 CONTINUE
      REWIND(IRD)
      IF( ISLC(40).GT.0 ) THEN
        INDX = 1
        CHMSG = 'Missing Exchanged Species Card'
        CALL WRMSGS( INDX )
      ENDIF
      REWIND(IRD)
 2420 CONTINUE
!
!---  Search input file for equilibrium reactions card  ---
!
 2429 READ(IRD,'(A)', END=2430) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 2429
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &  INDEX(CHDUM(2:),'equil').NE.0 .AND.
     &  INDEX(CHDUM(2:),'react').NE.0) THEN
        CALL RDEQRC
        REWIND(IRD)
        GOTO 2500
      ELSE
        GOTO 2420
      ENDIF
 2430 CONTINUE
      REWIND(IRD)
      IF( ISLC(40).GT.0 ) THEN
        INDX = 1
        CHMSG = 'Missing Equilibrium Reactions Card'
        CALL WRMSGS( INDX )
      ENDIF
      REWIND(IRD)
!
!---  Search input file for kinetic reactions card  ---
!
 2500 CONTINUE
 2509 READ(IRD,'(A)', END=2510) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 2509
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &  INDEX(CHDUM(2:),'kinetic').NE.0 .AND.
     &  INDEX(CHDUM(2:),'react').NE.0) THEN
        CALL RDKNRC
        REWIND(IRD)
        GOTO 2700
      ELSE
        GOTO 2500
      ENDIF
 2510 CONTINUE
      REWIND(IRD)
      IF( ISLC(40).GT.0 ) THEN
        INDX = 1
        CHMSG = 'Missing Kinetic Reactions Card'
        CALL WRMSGS( INDX )
      ENDIF
      REWIND(IRD)
!
!---  Search input file for equilibrium equation card  ---
!
 2700 CONTINUE
 2709 READ(IRD,'(A)', END=2710) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 2709
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &  INDEX(CHDUM(2:),'equil').NE.0 .AND.
     &  INDEX(CHDUM(2:),'equat').NE.0) THEN
        CALL RDEQEQ
        REWIND(IRD)
        GOTO 2800
      ELSE
        GOTO 2700
      ENDIF
 2710 CONTINUE
      REWIND(IRD)
      IF( ISLC(40).GT.0 ) THEN
        INDX = 1
        CHMSG = 'Missing Equilibrium Equations Card'
        CALL WRMSGS( INDX )
      ENDIF
      REWIND(IRD)
!
!---  Search input file for conservation equations card  ---
!
 2800 CONTINUE
 2809 READ(IRD,'(A)', END=2810) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 2809
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &  INDEX(CHDUM(2:),'conservation').NE.0 .AND.
     &  INDEX(CHDUM(2:),'equat').NE.0) THEN
        CALL RDCNEQ
        REWIND(IRD)
        GOTO 2900
      ELSE
        GOTO 2800
      ENDIF
 2810 CONTINUE
      REWIND(IRD)
      IF( ISLC(40).GT.0 ) THEN
        INDX = 1
        CHMSG = 'Missing Conservation Equations Card'
        CALL WRMSGS( INDX )
      ENDIF
      REWIND(IRD)
!
!---  Search input file for kinetic equations card  ---
!
 2900 CONTINUE
 2909 READ(IRD,'(A)', END=2910) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 2909
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &  INDEX(CHDUM(2:),'kinetic').NE.0 .AND.
     &  INDEX(CHDUM(2:),'equat').NE.0) THEN
        CALL RDKNEQ
        REWIND(IRD)
        GOTO 3000
      ELSE
        GOTO 2900
      ENDIF
 2910 CONTINUE
      REWIND(IRD)
      IF( ISLC(40).GT.0 ) THEN
        INDX = 1
        CHMSG = 'Missing Kinetic Equations Card'
        CALL WRMSGS( INDX )
      ENDIF
      REWIND(IRD)
!
!---  Search input file for lithology card  ---
!
 3000 CONTINUE
 3009 READ(IRD,'(A)', END=3010) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 3009
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &  INDEX(CHDUM(2:),'lithol').NE.0) THEN
        CALL RDLITH
        REWIND(IRD)
        GOTO 3100
      ELSE
        GOTO 3000
      ENDIF
 3010 CONTINUE
      REWIND(IRD)
      IF( ISLC(40).GT.0 ) THEN
        INDX = 1
        CHMSG = 'Missing Lithology Card'
        CALL WRMSGS( INDX )
      ENDIF
      REWIND(IRD)
!
!---  Search input file for reactive species link card  ---
!
 3100 CONTINUE
 3109 READ(IRD,'(A)', END=3110) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 3109
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &  INDEX(CHDUM(2:),'link').NE.0 .AND.
     &  INDEX(CHDUM(2:),'specie').NE.0) THEN
        CALL RDSPLK
        REWIND(IRD)
        GOTO 3200
      ELSE
        GOTO 3100
      ENDIF
 3110 CONTINUE
      REWIND(IRD)
      IF( ISLC(40).GT.0 ) THEN
        INDX = 1
        CHMSG = 'Missing Reactive Species Link Card'
        CALL WRMSGS( INDX )
      ENDIF
      REWIND(IRD)
 3200 CONTINUE

!
!---  Search input file for initial conditions card --
!
 4000 CONTINUE
 4009 READ(IRD,'(A)', END=4010) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 4009
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'initial').NE.0 ) THEN
        CALL RDIC1
        REWIND(IRD)
        GOTO 4100
      ELSE
        GOTO 4000
      ENDIF
 4010 CONTINUE
      IF( IEO.EQ.2 ) THEN
        INDX = 1
        CHMSG = 'Missing Initial Conditions Card'
        CALL WRMSGS( INDX )
        INDX = 2
        CALL RDRST(INDX)
        ISIC = 3
        REWIND(IRD)
      ELSE
        INDX = 18
        CHMSG = 'Missing Initial Conditions Card'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Search input file for boundary conditions card --
!
 4100 CONTINUE
 4109 READ(IRD,'(A)', END=4110) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 4109
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'boundary').NE.0 ) THEN
        CALL RDBC1
        REWIND(IRD)
        GOTO 4200
      ELSE
        GOTO 4100
      ENDIF
 4110 CONTINUE
      INDX = 1
      CHMSG = 'Missing Boundary Conditions Card'
      CALL WRMSGS( INDX )
      REWIND(IRD)
!
!---  Search input file for source card --
!
 4200 CONTINUE
 4209 READ(IRD,'(A)', END= 4210) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 4209
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'source').NE.0 ) THEN
        CALL RDSR1
        REWIND(IRD)
        GOTO 4300
      ELSE
        GOTO 4200
      ENDIF
 4210 CONTINUE
      INDX = 1
      CHMSG = 'Missing Source Card'
      CALL WRMSGS( INDX )
      REWIND(IRD)
!
!---  Search input file for output control card --
!
 4300 CONTINUE
 4309 READ(IRD,'(A)', END=4310) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 4309
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'output').NE.0 ) THEN
        CALL RDOU1
        REWIND(IRD)
        GOTO 4400
      ELSE
        GOTO 4300
      ENDIF
 4310 CONTINUE
      INDX = 1
      CHMSG = 'Missing Output Control Card'
      CALL WRMSGS( INDX )
      REWIND(IRD)
!
!---  Search input file for surface flux card --
!
 4400 CONTINUE
 4409 READ(IRD,'(A)', END=4410) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 4409
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'surface').NE.0 ) THEN
        CALL RDSF1
        REWIND(IRD)
        GOTO 4500
      ELSE
        GOTO 4400
      ENDIF
 4410 CONTINUE
      INDX = 1
      CHMSG = 'Missing Surface Flux Card'
      CALL WRMSGS( INDX )
      REWIND(IRD)
!
!---  Search input file for observed data card --
!
 4500 CONTINUE
      IF( ISLC(20).EQ.1 ) THEN
 4509   READ(IRD,'(A)', END=4510) CHDUM
        IF( CHDUM(1:1).EQ.'#' ) GOTO 4509
        CALL LCASE( CHDUM )
        IF( CHDUM(1:1).EQ.'~' .AND.
     &      INDEX(CHDUM(2:),'observed').NE.0 ) THEN
          CALL RDOBDA
          REWIND(IRD)
          GOTO 4600
        ELSE
          GOTO 4500
        ENDIF
 4510   CONTINUE
        IF( ISLC(20).EQ.0 ) THEN
          REWIND(IRD)
        ELSE
          INDX = 18
          CHMSG = 'Missing Observed Data Card'
          CALL WRMSGS( INDX )
        ENDIF
!
!---  Search input file for UCode control Card --
!
 4600   CONTINUE
 4609   READ(IRD,'(A)', END=4610) CHDUM
        IF( CHDUM(1:1).EQ.'#' ) GOTO 4609
        CALL LCASE( CHDUM )
        IF( CHDUM(1:1).EQ.'~' .AND.
     &      INDEX(CHDUM(2:),'ucode').NE.0 ) THEN
          CALL RDUCODE
          REWIND(IRD)
          GOTO 4700
        ELSE
          GOTO 4600
        ENDIF
 4610   CONTINUE
        IF( ISLC(20).EQ.0 ) THEN
          REWIND(IRD)
        ELSE
          INDX = 18
          CHMSG = 'Missing UCode Control Card'
          CALL WRMSGS( INDX )
        ENDIF
 4700   CONTINUE
      ENDIF

!
!---  Geomechanics  ---
!
      IF( ISLC(50).NE.0 ) THEN
 5100   CONTINUE
!
!---    Search input file for geomechanics property card --
!
 5109   READ(IRD,'(A)', END=5110) CHDUM
        IF( CHDUM(1:1).EQ.'#' ) GOTO 5109
        CALL LCASE( CHDUM )
          IF( CHDUM(1:1).EQ.'~' .AND.
     &      INDEX(CHDUM(2:),'geomech').NE.0 .AND.
     &      INDEX(CHDUM(2:),'prop').NE.0 ) THEN
          CALL RDGMP
          REWIND(IRD)
          GOTO 5200
        ELSE
          GOTO 5100
        ENDIF
 5110   CONTINUE
!
!---    Geomechanical simulations  ---
!
        IF( ISLC(50).NE.0 ) THEN
          INDX = 4
          CHMSG = 'Missing Geomechanical Properties Card'
          CALL WRMSGS( INDX )
        ELSE
          REWIND(IRD)
        ENDIF
 5200   CONTINUE
!!
!!---    Search input file for geomechanics link card --
!!
! 5209   READ(IRD,'(A)', END=5210) CHDUM
!        IF( CHDUM(1:1).EQ.'#' ) GOTO 5209
!        CALL LCASE( CHDUM )
!          IF( CHDUM(1:1).EQ.'~' .AND.
!     &      INDEX(CHDUM(2:),'geomech').NE.0 .AND.
!     &      INDEX(CHDUM(2:),'link').NE.0 ) THEN
!          CALL RDGMLK
!          REWIND(IRD)
!          GOTO 5300
!        ELSE
!          GOTO 5200
!        ENDIF
! 5210   CONTINUE
!!
!!---    Geomechanical simulations  ---
!!
!        IF( ISLC(50).NE.0 ) THEN
!          INDX = 4
!          CHMSG = 'Missing Geomechanics Link Card'
!          CALL WRMSGS( INDX )
!        ELSE
!          REWIND(IRD)
!        ENDIF
 5300   CONTINUE
!
!---    Search input file for geomechanics boundary condition card --
!
 5309   READ(IRD,'(A)', END=5310) CHDUM
        IF( CHDUM(1:1).EQ.'#' ) GOTO 5309
        CALL LCASE( CHDUM )
          IF( CHDUM(1:1).EQ.'~' .AND.
     &      INDEX(CHDUM(2:),'geomech').NE.0 .AND.
     &      INDEX(CHDUM(2:),'bound').NE.0 ) THEN
          CALL RDGMBC
          REWIND(IRD)
          GOTO 5400
        ELSE
          GOTO 5300
        ENDIF
 5310   CONTINUE
!
!---    Geomechanical simulations  ---
!
        IF( ISLC(50).NE.0 ) THEN
          INDX = 1
          CHMSG = 'Missing Geomechanics Boundary Condition Card'
          CALL WRMSGS( INDX )
        ELSE
          REWIND(IRD)
        ENDIF
 5400   CONTINUE
      ENDIF
!
!---  End of input record --
!
      CARD = 'End of Input Record'
      ICD = INDEX( CARD,'  ' )-1
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDINPT1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDOU1
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
!     Water Mode
!
!     Read input file for output information.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE OUTPU
      USE GRID
      USE FILES
      USE CONST
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
      EXTERNAL ICOUNT
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,UNTS,SOLNM,SPNM
      CHARACTER*512 CHDUM
      CHARACTER*6 FORM
      CHARACTER*4 FORM1
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM,FORM1
      DATA FORM / '(I6,$)' /
      DATA FORM1 / '(I )' /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDOU1'
!
!---  Write card information to ouput file  ---
!
      CARD = 'Output Control Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE(IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read reference node information  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Reference Nodes: '
      CALL RDINT(ISTART,ICOMMA,CHDUM,NREF)
      IF( NREF.GT.LREF ) THEN
        INDX = 5
        CHMSG = 'Number of Reference Nodes > LREF'
        CALL WRMSGS( INDX )
      ENDIF
      WRITE(IWR,'(/,A)') 'Reference Node No. and Indices'
      DO 100 N = 1,NREF
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Reference Node Index'
        CALL RDINT(ISTART,ICOMMA,CHDUM,IRF)
        CALL RDINT(ISTART,ICOMMA,CHDUM,JRF)
        CALL RDINT(ISTART,ICOMMA,CHDUM,KRF)
        IF( IRF.LT.1 .OR. IRF.GT.IFLD .OR. JRF.LT.1 .OR.
     &    JRF.GT.JFLD. OR. KRF.LT.1 .OR. KRF.GT.KFLD) THEN
          INDX = 4
          CHMSG = 'Out-of-Range Reference Node Index'
          CALL WRMSGS( INDX )
        ENDIF
        NDREF(N) = ND(IRF,JRF,KRF)
        WRITE(FORM(3:3),'(I1)') ICOUNT(NDREF(N))
        WRITE(IWR,'(2X,A,$)') 'Reference Node No. '
        WRITE(IWR,FORM) NDREF(N)
        WRITE(FORM(3:3),'(I1)') ICOUNT(IRF)
        WRITE(IWR,'(2X,A,$)') 'I = '
        WRITE(IWR,FORM) IRF
        WRITE(FORM(3:3),'(I1)') ICOUNT(JRF)
        WRITE(IWR,'(2X,A,$)') 'J = '
        WRITE(IWR,FORM) JRF
        WRITE(FORM(3:3),'(I1)') ICOUNT(KRF)
        WRITE(IWR,'(2X,A,$)') 'K = '
        WRITE(IWR,FORM) KRF
        WRITE(IWR,'(2X,A)' ) 'Indices'
  100 CONTINUE
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      IDFLT = 1
      IFQS = IBIG
      VARB = 'Reference Node Screen Output Frequency'
      CALL RDINT(ISTART,ICOMMA,CHDUM,IFQS)
      WRITE(IWR,'(/,2A,I6,A)') VARB(1:IVR),': Every ',IFQS,
     &' Time Step(s)'
      IF( IFQS.LE.0 ) IFQS = IBIG
      IDFLT = 1
      IFQO = IBIG
      VARB = 'Reference Node Output File Frequency'
      CALL RDINT(ISTART,ICOMMA,CHDUM,IFQO)
      WRITE(IWR,'(2A,I6,A)') VARB(1:IVR),': Every ',IFQO,' Time Step(s)'
      IF( IFQO.LE.0 ) IFQO = IBIG
      IDFLT = 1
      VARB = 'Time Output Units'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTM)
      WRITE(IWR,'(3A)') VARB(1:IVR),': ',UNTM(1:NCH)
      IDFLT = 1
      VARB = 'Length Output Units'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNLN)
      WRITE(IWR,'(3A)') VARB(1:IVR),': ',UNLN(1:NCH)
      IF( ICS.EQ.2 .OR. ICS.EQ.6 ) THEN
        IDFLT = 1
        VARB = 'Arc Output Units'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNAR)
        WRITE(IWR,'(3A)') VARB(1:IVR),': ',UNAR(1:NCH)
      ENDIF
      IDFLT = 1
      VARB = 'Screen Significant Digits'
      CALL RDINT(ISTART,ICOMMA,CHDUM,ISGNS)
      WRITE(IWR,'(2A,I2)') VARB(1:IVR),': ',ISGNS
      IDFLT = 1
      VARB = 'Output File Significant Digits'
      CALL RDINT(ISTART,ICOMMA,CHDUM,ISGNO)
      WRITE(IWR,'(2A,I2)') VARB(1:IVR),': ',ISGNO
      IDFLT = 1
      VARB = 'Plot File Significant Digits'
      CALL RDINT(ISTART,ICOMMA,CHDUM,ISGNP)
      WRITE(IWR,'(2A,I2)') VARB(1:IVR),': ',ISGNP
!
!---  Read reference node variables  ---
!
      WRITE( IWR,'(/,A)') 'Reference Node Variables'
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Reference Node Variables: '
      CALL RDINT(ISTART,ICOMMA,CHDUM,NVREF)
      NVC = 0
      DO 200 NV = 1,NVREF
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART=1
        VARB = 'Reference Node Variable: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX( ADUM(1:),'solute' ).NE.0 ) THEN
          VARB = 'Reference Node Variable: Solute Name: '
          CALL RDCHR(ISTART,ICOMMA,NCS,CHDUM,SOLNM)
          DO 110 NSL = 1,NSOLU
            IF( SOLNM.EQ.SOLUT(NSL) ) GOTO 120
  110     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Reference-Node Solute Name: '//SOLNM
          CALL WRMSGS( INDX )
          NVC = NVC -1
          GOTO 200
  120     CONTINUE
        ENDIF

        IF( INDEX( ADUM(1:),'species' ).NE.0 ) THEN
          IF( ISLC(40).EQ.0 ) THEN
            NVC = NVC -1
            GOTO 200
          ENDIF
          VARB = 'Reference Node Variable: Reactive Species Name: '
          CALL RDCHR(ISTART,ICOMMA,NCS,CHDUM,SPNM)
!
!---      Aqueous species  ---
!
          DO 122 M = 1,NSPL
            NSP = M
            IF( SPNM.EQ.SPNML(M) ) GOTO 128
  122     CONTINUE
!
!---      Solid species  ---
!
          DO 124 M = 1,NSPS
            NSP = M+NSPL
            IF( SPNM.EQ.SPNMS(M) ) GOTO 128
  124     CONTINUE
!
!---      Exchanged species  ---
!
          DO 125 M = 1,NSPE
            NSP = M+NSPL+NSPS
            IF( SPNM.EQ.SPNME(M) ) GOTO 128
  125     CONTINUE
!
!---      Conservation- or kinetic-component species  ---
!
          IF( INDEX( SPNM(1:),'total_' ).NE.0 ) THEN
            DO 126 NSL = NSOLU+1,NSOLU+NEQC+NEQK
              IF( SPNM.EQ.SOLUT(NSL) ) GOTO 128
  126       CONTINUE
          ENDIF
!
!---      Unrecognized species name  ---
!
          INDX = 4
          CHMSG = 'Unrecognized Reference-Node Reactive Species Name: '
     &      // SPNM
          CALL WRMSGS( INDX )
          NVC = NVC -1
          GOTO 200
  128     CONTINUE
        ENDIF

        IF( INDEX(ADUM(1:),'aqueous pressure').NE.0 ) THEN
          IREF(NV) = 1
        ELSEIF( INDEX(ADUM(1:),'gas pressure').NE.0 ) THEN
          IREF(NV) = 2
        ELSEIF( INDEX(ADUM(1:),'matrix pressure').NE.0 ) THEN
          CHREF(3) = 'PM'
          IREF(NV) = 3
        ELSEIF( INDEX(ADUM(1:),'temperature').NE.0 ) THEN
          IREF(NV) = 4
        ELSEIF( INDEX(ADUM(1:),'phase condition').NE.0 ) THEN
          IREF(NV) = 5
        ELSEIF( INDEX(ADUM(1:),'aqueous gauge pressure').NE.0 ) THEN
          IREF(NV) = 6
        ELSEIF( INDEX(ADUM(1:),'gas gauge pressure').NE.0 ) THEN
          IREF(NV) = 7
        ELSEIF( INDEX(ADUM(1:),'apparent aqueous sat').NE.0 ) THEN
          IREF(NV) = 9
        ELSEIF( INDEX(ADUM(1:),'effective trapped gas').NE.0 ) THEN
          IREF(NV) = 19
        ELSEIF( INDEX(ADUM(1:),'trapped gas sat').NE.0 ) THEN
          IREF(NV) = 105
        ELSEIF( INDEX(ADUM(1:),'aqueous saturation').NE.0 ) THEN
          IREF(NV) = 11
        ELSEIF( INDEX(ADUM(1:),'gas saturation').NE.0 ) THEN
          IREF(NV) = 12
        ELSEIF( INDEX(ADUM(1:),'aqueous moisture cont').NE.0 ) THEN
          IREF(NV) = 15
        ELSEIF( INDEX(ADUM(1:),'diffusive porosity').NE.0 ) THEN
          IREF(NV) = 20
        ELSEIF( INDEX(ADUM(1:),'water aqueous mass frac').NE.0 ) THEN
          IREF(NV) = 24
        ELSEIF( INDEX(ADUM(1:),'aqueous hydraulic head').NE.0 ) THEN
          IREF(NV) = 27
        ELSEIF( INDEX(ADUM(1:),'rock/soil type').NE.0 ) THEN
          IREF(NV) = 30
        ELSEIF( INDEX(ADUM(1:),'aqueous density').NE.0 ) THEN
          IREF(NV) = 34
        ELSEIF( INDEX(ADUM(1:),'total water mass').NE.0 ) THEN
          IREF(NV) = 37
        ELSEIF( INDEX(ADUM(1:),'water mass source int').NE.0 ) THEN
          IREF(NV) = 40
        ELSEIF( INDEX(ADUM(1:),'aqueous courant').NE.0 ) THEN
          ICRNT = 1
          IREF(NV) = 49
        ELSEIF( INDEX(ADUM(1:),'x aqueous vol').NE.0 ) THEN
          IREF(NV) = 51
        ELSEIF( INDEX(ADUM(1:),'y aqueous vol').NE.0 ) THEN
          IREF(NV) = 52
        ELSEIF( INDEX(ADUM(1:),'z aqueous vol').NE.0 ) THEN
          IREF(NV) = 53
        ELSEIF( INDEX(ADUM(1:),'aqueous matrix').NE.0 ) THEN
          IREF(NV) = 83
        ELSEIF( INDEX(ADUM(1:),'aqueous fracture').NE.0 ) THEN
          IREF(NV) = 84
        ELSEIF( INDEX(ADUM(1:),'xnc aqueous vol').NE.0 ) THEN
          IREF(NV) = 87
        ELSEIF( INDEX(ADUM(1:),'ync aqueous vol').NE.0 ) THEN
          IREF(NV) = 88
        ELSEIF( INDEX(ADUM(1:),'znc aqueous vol').NE.0 ) THEN
          IREF(NV) = 89
        ELSEIF( INDEX(ADUM(1:),'space monitor').NE.0 ) THEN
          IREF(NV) = 100
        ELSEIF( INDEX(ADUM(1:),'well pressure').NE.0 ) THEN
          IREF(NV) = 138
        ELSEIF( INDEX(ADUM(1:),'water mass source rate').NE.0 ) THEN
          IREF(NV) = 140
        ELSEIF( INDEX(ADUM(1:),'aqueous well depth').NE.0 ) THEN
          IREF(NV) = 144
        ELSEIF( INDEX(ADUM(1:),'well flow rate').NE.0 ) THEN
          IREF(NV) = 145
        ELSEIF( INDEX(ADUM(1:),'well flow integral').NE.0 ) THEN
          IREF(NV) = 146
        ELSEIF( INDEX(ADUM(1:),'scanning path').NE.0 ) THEN
          IREF(NV) = 149
        ELSEIF( INDEX(ADUM(1:),'aqueous viscosity').NE.0 ) THEN
          IREF(NV) = 176
        ELSEIF( INDEX(ADUM(1:),'matric pot').NE.0 ) THEN
          IREF(NV) = 63
        ELSEIF( INDEX(ADUM(1:),'integrated water mass').NE.0 ) THEN
          IREF(NV) = 191
        ELSEIF( INDEX(ADUM(1:),'x aqueous relative perm').NE.0 ) THEN
          IREF(NV) = 201
        ELSEIF( INDEX(ADUM(1:),'y aqueous relative perm').NE.0 ) THEN
          IREF(NV) = 202
        ELSEIF( INDEX(ADUM(1:),'z aqueous relative perm').NE.0 ) THEN
          IREF(NV) = 203
        ELSEIF( INDEX(ADUM(1:),'x intrinsic perm').NE.0 ) THEN
          IREF(NV) = 247
        ELSEIF( INDEX(ADUM(1:),'y intrinsic perm').NE.0 ) THEN
          IREF(NV) = 248
        ELSEIF( INDEX(ADUM(1:),'z intrinsic perm').NE.0 ) THEN
          IREF(NV) = 249
        ELSEIF( INDEX(ADUM(1:),'aqueous relative perm').NE.0 ) THEN
          IREF(NV) = 31
        ELSEIF( INDEX(ADUM(1:),'ponding height').NE.0 ) THEN
          IREF(NV) = 277
        ELSEIF( ( INDEX(ADUM(1:),'similarity').NE.0 .OR.
     &    INDEX(ADUM(1:),'similitude').NE.0 ) .AND.
     &    INDEX(ADUM(1:),'variable').NE.0) THEN
          IREF(NV) = 299

        ELSEIF( ( INDEX(ADUM(1:),'solute volumetric conc').NE.0 .OR.
     &    INDEX(ADUM(1:),'species volumetric conc').NE.0 )  .AND.
     &    INDEX( SPNM(1:),'total_' ).NE.0 .AND.
     &    INDEX( SPNM(1:),'exchange' ).NE.0 ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 12
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 12
            CHREF(INDX) = 'SPX '
            UNREF(INDX) = 'mol/m^3'
          ENDIF
        ELSEIF( ( INDEX(ADUM(1:),'solute volumetric conc').NE.0 .OR.
     &    INDEX(ADUM(1:),'species volumetric conc').NE.0 )  .AND.
     &    INDEX( SPNM(1:),'total_' ).NE.0 .AND.
     &    INDEX( SPNM(1:),'solid' ).NE.0 ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 26
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 26
            CHREF(INDX) = 'SPS '
            UNREF(INDX) = 'mol/m^3'
          ENDIF
        ELSEIF( ( INDEX(ADUM(1:),'solute volumetric conc').NE.0 .OR.
     &    INDEX(ADUM(1:),'species volumetric conc').NE.0 )  .AND.
     &    INDEX( SPNM(1:),'total_' ).NE.0 ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 1
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 1
            CHREF(INDX) = ' SP '
            UNREF(INDX) = 'mol/m^3'
          ENDIF

        ELSEIF( INDEX(ADUM(1:),'solute volumetric conc').NE.0 ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 1

        ELSEIF( ( INDEX(ADUM(1:),'solute aqueous conc').NE.0 .OR.
     &    INDEX(ADUM(1:),'species aqueous conc').NE.0 ) .AND.
     &    INDEX( SPNM(1:),'total_' ).NE.0 .AND.
     &    INDEX( SPNM(1:),'exchange' ).NE.0 ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 13
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 13
            CHREF(INDX) = 'SPLX'
            UNREF(INDX) = 'mol/m^3'
          ENDIF
        ELSEIF( ( INDEX(ADUM(1:),'solute aqueous conc').NE.0 .OR.
     &    INDEX(ADUM(1:),'species aqueous conc').NE.0 ) .AND.
     &    INDEX( SPNM(1:),'total_' ).NE.0 .AND.
     &    INDEX( SPNM(1:),'solid' ).NE.0 ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 27
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 27
            CHREF(INDX) = 'SPLS'
            UNREF(INDX) = 'mol/m^3'
          ENDIF
        ELSEIF( ( INDEX(ADUM(1:),'solute aqueous conc').NE.0 .OR.
     &    INDEX(ADUM(1:),'species aqueous conc').NE.0 ) .AND.
     &    INDEX( SPNM(1:),'total_' ).NE.0 ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 2
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 2
            CHREF(INDX) = 'SPL '
            UNREF(INDX) = 'mol/m^3'
          ENDIF

        ELSEIF( INDEX(ADUM(1:),'solute aqueous conc').NE.0 ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 2
        ELSEIF( INDEX(ADUM(1:),'solute aqueous mol').NE.0 ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 5
        ELSEIF( INDEX(ADUM(1:),'solute integrated aqueous').NE.0 ) THEN
          WRITE(FORM1(3:3),'(I1)') ICOUNT(NSL)
          INDX = 400+(NSL-1)*33 + 6
          IREF(NV) = INDX
          CHREF(INDX)(1:3) = 'ICL'
          WRITE( CHREF(INDX)(4:),FORM1) NSL
          UNREF(INDX) = 'null'
        ELSEIF( INDEX(ADUM(1:),'solute inventory').NE.0 ) THEN
          INDX = 400+(NSL-1)*33 + 7
          IREF(NV) = INDX
          CHREF(INDX)(1:2) = 'CI'
        ELSEIF( INDEX(ADUM(1:),'x solute flux').NE.0 ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 8
        ELSEIF( INDEX(ADUM(1:),'y solute flux').NE.0 ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 9
        ELSEIF( INDEX(ADUM(1:),'z solute flux').NE.0 ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 10

        ELSEIF( (INDEX(ADUM(1:),'solute source').NE.0) .OR.
     &    ((INDEX(ADUM(1:),'species source').NE.0) .AND.
     &    (INDEX( SPNM(1:),'total_' ).NE.0)) ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 11
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 11
            CHREF(INDX) = 'SPSR'
            UNREF(INDX) = 'mol/s'
          ENDIF

        ELSEIF( INDEX(ADUM(1:),'solute source').NE.0 ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 11
        ELSEIF( INDEX(ADUM(1:),'solute mass conc').NE.0 ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 30

        ELSEIF( (INDEX(ADUM(1:),'solute integrated mass').NE.0) .OR.
     &    ((INDEX(ADUM(1:),'species integrated mass').NE.0) .AND.
     &    (INDEX( SPNM(1:),'total_' ).NE.0)) ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 23
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 23
            CHREF(INDX) = 'SPIM'
            UNREF(INDX) = 'mol'
          ENDIF

        ELSEIF( INDEX(ADUM(1:),'solute integrated mass').NE.0 ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 23

        ELSEIF( INDEX(ADUM(1:),'species volumetric conc').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 1
          IREF(NV) = INDX
          CHREF(INDX) = ' SP '
          UNREF(INDX) = 'mol/m^3'
        ELSEIF( INDEX(ADUM(1:),'species aqueous conc').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 2
          IREF(NV) = INDX
          CHREF(INDX) = 'SPL '
          UNREF(INDX) = 'mol/m^3'
        ELSEIF( INDEX(ADUM(1:),'species source').NE.0 ) THEN
          IREF(NV) = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 11
        ELSEIF( INDEX(ADUM(1:),'species integrated mass').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 23
          IREF(NV) = INDX
          CHREF(INDX) = 'SPIM'
          UNREF(INDX) = 'mol'
        ELSEIF( INDEX(ADUM(1:),'mineral area').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 24
          IREF(NV) = INDX
          CHREF(INDX) = 'SPMA'
          UNREF(INDX) = 'm^2'
        ELSEIF( INDEX(ADUM(1:),'mineral rate').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 25
          IREF(NV) = INDX
          CHREF(INDX) = 'SPMR'
          UNREF(INDX) = 'mol/s'
        ELSEIF( INDEX(ADUM(1:),'volume fraction').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 26
          IREF(NV) = INDX
          CHREF(INDX) = 'SPVF'
        ELSEIF( INDEX(ADUM(1:),'ph').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 27
          IREF(NV) = INDX
          CHREF(INDX) = 'pH'

        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Reference Node Variable: '//ADUM
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Check for duplicate reference node variables  ---
!
        DO 190 NX = 1,NV-1
          IF( IREF(NV).EQ.IREF(NX) ) THEN
            INDX = 4
            CHMSG = 'Duplicate Reference Node Variable: '//ADUM
            CALL WRMSGS( INDX )
          ENDIF
  190   CONTINUE
!
!---    Reference node variable units  ---
!
        IDFLT = 1
        VARB = 'Reference Node Variable Unit: '
        CALL RDCHR(ISTART,ICOMMA,NCU,CHDUM,UNREF(IREF(NV)))
        IF( INDEX( ADUM(1:),'solute' ).NE.0 ) THEN
          WRITE( IWR,'(2X,3A,2X,2A,I2,A)' ) ADUM(1:NCH),', ',
     &      UNREF(IREF(NV))(1:NCU),SOLNM(1:NCS),' Solute(',NSL,')'

        ELSEIF( INDEX( ADUM(1:),'species' ).NE.0 ) THEN
          WRITE( IWR,'(2X,3A,2X,2A,I2,A)' ) ADUM(1:NCH),', ',
     &      UNREF(IREF(NV))(1:NCU),SPNM(1:NCS),' Species(',NSP,')'

        ELSE
          WRITE( IWR,'(2X,3A)' ) ADUM(1:NCH),', ',UNREF(IREF(NV))(1:NCU)
        ENDIF
        CALL RDOUUN( IREF(NV) )
        VAR = 0.D+0
        INDX = 0
        CALL RDUNIT( UNREF(IREF(NV)),VAR,INDX )
  200 CONTINUE
      NVREF = NVREF + NVC
!
!---  Plot file output times  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Plot File Output Times'
      CALL RDINT(ISTART,ICOMMA,CHDUM,NPRTM)
      WRITE(IWR,'(/,A)') ' Plot File Output Times:'
      PRTMX = 0.D+0
      IC = 0
      DO 300 N = 1,NPRTM
        IF( IC.GT.1 ) PRTMX = PRTM(IC-1)
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        ICMX = INDEX( CHDUM(ISTART:), ',' )
        IATX = INDEX( CHDUM(ISTART:), '@' )
!
!---    Sequence of plot file output times  ---
!
        IF( IATX.GT.1 .AND. IATX.LT.ICMX ) THEN
          CHDUM(IATX:IATX) = ','
          VARB = 'Count Integer'
          CALL RDINT(ISTART,ICOMMA,CHDUM,IATX )
          VARB = 'Delta Plot File Output Time'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,DTX )
          VARB = 'Plot File Output Time Units'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNS = 1
          CALL RDUNIT(UNTS,DTX,INDX)
          DO 210 II = 1,IATX
            IC = IC + 1
            IF( IC.GT.LPTM ) THEN
              INDX = 5
              CHMSG = 'Number of Output Times > Parameter LPTM'
              CALL WRMSGS( INDX )
            ENDIF
            IF( IC.EQ.1 ) THEN
              PRTM(IC) = DTX
            ELSE
              PRTM(IC) = PRTM(IC-1) + DTX
            ENDIF
            PRTMX = PRTM(IC)
            INDX = 1
            IUNS = 1
            CALL RDUNIT(UNTS,PRTMX,INDX)
            WRITE(IWR,'(2X,1PE11.4,1X,A)') PRTMX,UNTS(1:NCH)
            TMPR = MIN( TMPR,PRTM(IC) )
  210     CONTINUE
!
!---    Single plot file output time  ---
!
        ELSE
          IC = IC + 1
          IF( IC.GT.LPTM ) THEN
            INDX = 5
            CHMSG = 'Number of Output Times > Parameter LPTM'
            CALL WRMSGS( INDX )
          ENDIF
          VARB = 'Plot File Output Time'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,PRTM(IC))
          VARB = 'Plot File Output Time Units'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,1PE11.4,1X,A)') PRTM(IC),UNTS(1:NCH)
          INDX = 0
          IUNS = 1
          CALL RDUNIT(UNTS,PRTM(IC),INDX)
          TMPR = MIN( TMPR,PRTM(IC) )
        ENDIF
 300  CONTINUE
      NPRTM = IC
      WRITE(IWR,'(2X,A)') 'After the Final Time Step'
!
!---  Read Plot File Variables  ---
!
      WRITE( IWR,'(/,A)') 'Plot File Variables:'
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Plot File Variables: '
      CALL RDINT(ISTART,ICOMMA,CHDUM,NVPLOT)
      NVC = 0
      DO 400 NV = 1,NVPLOT
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART=1
        VARB = 'Plot File Variable: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX( ADUM(1:),'solute' ).NE.0 ) THEN
          VARB = 'Plot File Variable: Solute Name: '
          CALL RDCHR(ISTART,ICOMMA,NCS,CHDUM,SOLNM)
          DO 310 NSL = 1,NSOLU
            IF( SOLNM.EQ.SOLUT(NSL) ) GOTO 320
  310     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Plot File Solute Name: '//SOLNM
          CALL WRMSGS( INDX )
          NVC = NVC -1
          GOTO 400
  320     CONTINUE
        ENDIF

        IF( INDEX( ADUM(1:),'species' ).NE.0 ) THEN
          IF( ISLC(40).EQ.0 ) THEN
            NVC = NVC -1
            GOTO 400
          ENDIF
          VARB = 'Plot File Variable: Reactive Species Name: '
          CALL RDCHR(ISTART,ICOMMA,NCS,CHDUM,SPNM)
!
!---      Conservation- or kinetic-component species  ---
!
          IF( INDEX( SPNM(1:),'total_' ).NE.0 ) THEN
            DO 330 NSL = NSOLU+1,NSOLU+NEQC+NEQK
              IF( SPNM.EQ.SOLUT(NSL) ) GOTO 350
  330       CONTINUE
          ENDIF
!
!---      Aqueous species  ---
!
          DO 332 M = 1,NSPL
            NSP = M
            IF( SPNM.EQ.SPNML(M) ) GOTO 350
  332     CONTINUE
!
!---      Solid species  ---
!
          DO 334 M = 1,NSPS
            NSP = M+NSPL
            IF( SPNM.EQ.SPNMS(M) ) GOTO 350
  334     CONTINUE
!
!---      Exchanged species  ---
!
          DO 336 M = 1,NSPE
            NSP = M + NSPL + NSPS
            IF( SPNM.EQ.SPNME(M) ) GOTO 350
  336     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Plot File Reactive Species Name: '
     &      // SPNM
          CALL WRMSGS( INDX )
          NVC = NVC -1
          GOTO 400
  350     CONTINUE
        ENDIF

        IF( INDEX(ADUM(1:),'final restart').NE.0 ) THEN
          ISLC(18) = 1
          IPLOT(NV) = 200
        ELSEIF( INDEX(ADUM(1:),'no restart').NE.0 ) THEN
          ISLC(18) = 2
          IPLOT(NV) = 200
        ELSEIF( INDEX(ADUM(1:),'3d grid').NE.0 ) THEN
          ISLC(63) = 1
          IPLOT(NV) = 200
        ELSEIF( INDEX(ADUM(1:),'aqueous pressure').NE.0 ) THEN
          IPLOT(NV) = 1
        ELSEIF( INDEX(ADUM(1:),'gas pressure').NE.0 ) THEN
          IPLOT(NV) = 2
        ELSEIF( INDEX(ADUM(1:),'matrix pressure').NE.0 ) THEN
          IPLOT(NV) = 3
        ELSEIF( INDEX(ADUM(1:),'temperature').NE.0 ) THEN
          IPLOT(NV) = 4
        ELSEIF( INDEX(ADUM(1:),'phase condition').NE.0 ) THEN
          IPLOT(NV) = 5
        ELSEIF( INDEX(ADUM(1:),'aqueous gauge pressure').NE.0 ) THEN
          IPLOT(NV) = 6
        ELSEIF( INDEX(ADUM(1:),'gas gauge pressure').NE.0 ) THEN
          IPLOT(NV) = 7
        ELSEIF( INDEX(ADUM(1:),'apparent aqueous sat').NE.0 ) THEN
          IPLOT(NV) = 9
        ELSEIF( INDEX(ADUM(1:),'effective trapped gas').NE.0 ) THEN
          IPLOT(NV) = 19
        ELSEIF( INDEX(ADUM(1:),'trapped gas sat').NE.0 ) THEN
          IPLOT(NV) = 105
        ELSEIF( INDEX(ADUM(1:),'aqueous saturation').NE.0 ) THEN
          IPLOT(NV) = 11
        ELSEIF( INDEX(ADUM(1:),'gas saturation').NE.0 ) THEN
          IPLOT(NV) = 12
        ELSEIF( INDEX(ADUM(1:),'aqueous moisture cont').NE.0 ) THEN
          IPLOT(NV) = 15
        ELSEIF( INDEX(ADUM(1:),'diffusive porosity').NE.0 ) THEN
          IPLOT(NV) = 20
        ELSEIF( INDEX(ADUM(1:),'water aqueous mass frac').NE.0 ) THEN
          IPLOT(NV) = 24
        ELSEIF( INDEX(ADUM(1:),'aqueous hydraulic head').NE.0 ) THEN
          IPLOT(NV) = 27
        ELSEIF( INDEX(ADUM(1:),'rock/soil type').NE.0 ) THEN
          IPLOT(NV) = 30
        ELSEIF( INDEX(ADUM(1:),'aqueous density').NE.0 ) THEN
          IPLOT(NV) = 34
        ELSEIF( INDEX(ADUM(1:),'total water mass').NE.0 ) THEN
          IPLOT(NV) = 37
        ELSEIF( INDEX(ADUM(1:),'water mass source int').NE.0 ) THEN
          IPLOT(NV) = 40
        ELSEIF( INDEX(ADUM(1:),'aqueous courant').NE.0 ) THEN
          ICRNT = 1
          IPLOT(NV) = 49
        ELSEIF( INDEX(ADUM(1:),'x aqueous vol').NE.0 ) THEN
          IPLOT(NV) = 51
        ELSEIF( INDEX(ADUM(1:),'y aqueous vol').NE.0 ) THEN
          IPLOT(NV) = 52
        ELSEIF( INDEX(ADUM(1:),'z aqueous vol').NE.0 ) THEN
          IPLOT(NV) = 53
        ELSEIF( INDEX(ADUM(1:),'matric pot').NE.0 ) THEN
          IPLOT(NV) = 63
        ELSEIF( INDEX(ADUM(1:),'aqueous matrix').NE.0 ) THEN
          IPLOT(NV) = 83
        ELSEIF( INDEX(ADUM(1:),'aqueous fracture').NE.0 ) THEN
          IPLOT(NV) = 84
        ELSEIF( INDEX(ADUM(1:),'xnc aqueous vol').NE.0 ) THEN
          IPLOT(NV) = 87
        ELSEIF( INDEX(ADUM(1:),'ync aqueous vol').NE.0 ) THEN
          IPLOT(NV) = 88
        ELSEIF( INDEX(ADUM(1:),'znc aqueous vol').NE.0 ) THEN
          IPLOT(NV) = 89
        ELSEIF( INDEX(ADUM(1:),'space monitor').NE.0 ) THEN
          IPLOT(NV) = 100
        ELSEIF( INDEX(ADUM(1:),'water mass source rate').NE.0 ) THEN
          IPLOT(NV) = 140
        ELSEIF( INDEX(ADUM(1:),'scanning path').NE.0 ) THEN
          IPLOT(NV) = 149
        ELSEIF( INDEX(ADUM(1:),'aqueous viscosity').NE.0 ) THEN
          IPLOT(NV) = 176
        ELSEIF( INDEX(ADUM(1:),'x aqueous relative perm').NE.0 ) THEN
          IPLOT(NV) = 201
        ELSEIF( INDEX(ADUM(1:),'y aqueous relative perm').NE.0 ) THEN
          IPLOT(NV) = 202
        ELSEIF( INDEX(ADUM(1:),'z aqueous relative perm').NE.0 ) THEN
          IPLOT(NV) = 203
        ELSEIF( INDEX(ADUM(1:),'x intrinsic perm').NE.0 ) THEN
          IPLOT(NV) = 247
        ELSEIF( INDEX(ADUM(1:),'y intrinsic perm').NE.0 ) THEN
          IPLOT(NV) = 248
        ELSEIF( INDEX(ADUM(1:),'z intrinsic perm').NE.0 ) THEN
          IPLOT(NV) = 249
        ELSEIF( INDEX(ADUM(1:),'x').NE.0 .AND.
     &    INDEX(ADUM(1:),'node').NE.0 .AND.
     &    INDEX(ADUM(1:),'centroid').NE.0 ) THEN
          IPLOT(NV) = 291
        ELSEIF( INDEX(ADUM(1:),'y').NE.0 .AND.
     &    INDEX(ADUM(1:),'node').NE.0 .AND.
     &    INDEX(ADUM(1:),'centroid').NE.0 ) THEN
          IPLOT(NV) = 292
        ELSEIF( INDEX(ADUM(1:),'z').NE.0 .AND.
     &    INDEX(ADUM(1:),'node').NE.0 .AND.
     &    INDEX(ADUM(1:),'centroid').NE.0 ) THEN
          IPLOT(NV) = 293
        ELSEIF( INDEX(ADUM(1:),'aqueous relative perm').NE.0 ) THEN
          IPLOT(NV) = 31
        ELSEIF( ( INDEX(ADUM(1:),'similarity').NE.0 .OR.
     &    INDEX(ADUM(1:),'similitude').NE.0 ) .AND.
     &    INDEX(ADUM(1:),'variable').NE.0) THEN
          IPLOT(NV) = 299

        ELSEIF( ( INDEX(ADUM(1:),'solute volumetric conc').NE.0 .OR.
     &    INDEX(ADUM(1:),'species volumetric conc').NE.0 ) .AND.
     &    INDEX( SPNM(1:),'total_' ).NE.0 .AND.
     &    INDEX( SPNM(1:),'exchange' ).NE.0 ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 12
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 12
            UNPLOT(INDX) = 'mol/m^3'
          ENDIF
        ELSEIF( ( INDEX(ADUM(1:),'solute volumetric conc').NE.0 .OR.
     &    INDEX(ADUM(1:),'species volumetric conc').NE.0 ) .AND.
     &    INDEX( SPNM(1:),'total_' ).NE.0 .AND.
     &    INDEX( SPNM(1:),'solid' ).NE.0 ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 26
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 26
            UNPLOT(INDX) = 'mol/m^3'
          ENDIF
        ELSEIF( ( INDEX(ADUM(1:),'solute volumetric conc').NE.0 .OR.
     &    INDEX(ADUM(1:),'species volumetric conc').NE.0 ) .AND.
     &    INDEX( SPNM(1:),'total_' ).NE.0 ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 1
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 1
            UNPLOT(INDX) = 'mol/m^3'
          ENDIF

        ELSEIF( INDEX(ADUM(1:),'solute volumetric conc').NE.0 ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 1

        ELSEIF( ( INDEX(ADUM(1:),'solute aqueous conc').NE.0 .OR.
     &    INDEX(ADUM(1:),'species aqueous conc').NE.0 ) .AND.
     &    INDEX( SPNM(1:),'total_' ).NE.0 .AND.
     &    INDEX( SPNM(1:),'exchange' ).NE.0 ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 13
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 13
            UNPLOT(INDX) = 'mol/m^3'
          ENDIF
        ELSEIF( ( INDEX(ADUM(1:),'solute aqueous conc').NE.0 .OR.
     &    INDEX(ADUM(1:),'species aqueous conc').NE.0 ) .AND.
     &    INDEX( SPNM(1:),'total_' ).NE.0 .AND.
     &    INDEX( SPNM(1:),'solid' ).NE.0 ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 27
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 27
            UNPLOT(INDX) = 'mol/m^3'
          ENDIF
        ELSEIF( ( INDEX(ADUM(1:),'solute aqueous conc').NE.0 .OR.
     &    INDEX(ADUM(1:),'species aqueous conc').NE.0 ) .AND.
     &    INDEX( SPNM(1:),'total_' ).NE.0 ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 2
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 2
            UNPLOT(INDX) = 'mol/m^3'
          ENDIF

        ELSEIF( INDEX(ADUM(1:),'solute aqueous conc').NE.0 ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 2
        ELSEIF( INDEX(ADUM(1:),'solute aqueous mol').NE.0 ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 5
        ELSEIF( INDEX(ADUM(1:),'solute inventory').NE.0 ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 7
        ELSEIF( INDEX(ADUM(1:),'x solute flux').NE.0 ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 8
        ELSEIF( INDEX(ADUM(1:),'y solute flux').NE.0 ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 9
        ELSEIF( INDEX(ADUM(1:),'z solute flux').NE.0 ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 10

        ELSEIF( (INDEX(ADUM(1:),'solute source').NE.0) .OR.
     &    ((INDEX(ADUM(1:),'species source').NE.0) .AND.
     &    (INDEX( SPNM(1:),'total_' ).NE.0)) ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 11
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 11
            UNPLOT(INDX) = 'mol/s'
          ENDIF

        ELSEIF( INDEX(ADUM(1:),'solute source').NE.0 ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 11
        ELSEIF( INDEX(ADUM(1:),'solute mass conc').NE.0 ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 30

        ELSEIF( INDEX(ADUM(1:),'species volumetric conc').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 1
          IPLOT(NV) = INDX
          UNPLOT(INDX) = 'mol/m^3'
        ELSEIF( INDEX(ADUM(1:),'species aqueous conc').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 2
          IPLOT(NV) = INDX
          UNPLOT(INDX) = 'mol/m^3'
        ELSEIF( INDEX(ADUM(1:),'species source').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 11
          IPLOT(NV) = INDX
          UNPLOT(INDX) = 'mol'
        ELSEIF( INDEX(ADUM(1:),'mineral area').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 24
          IPLOT(NV) = INDX
          UNPLOT(INDX) = 'm^2'
        ELSEIF( INDEX(ADUM(1:),'mineral rate').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 25
          IPLOT(NV) = INDX
          UNPLOT(INDX) = 'mol/s'
        ELSEIF( INDEX(ADUM(1:),'volume fraction').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 26
          IPLOT(NV) = INDX
        ELSEIF( INDEX(ADUM(1:),'ph').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 27
          IPLOT(NV) = INDX

        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Plot File Variable: '//ADUM
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Check for duplicate plot file variables  ---
!
        DO 390 NX = 1,NV-1
          IF( IPLOT(NV).EQ.IPLOT(NX) .AND. IPLOT(NV).NE.200 ) THEN
            INDX = 4
            CHMSG = 'Duplicate Plot File Variable: '//ADUM
            CALL WRMSGS( INDX )
          ENDIF
  390   CONTINUE
!
!---    Reference node variable units  ---
!
        IDFLT = 1
        VARB = 'Plot File Variable Units: '
        CALL RDCHR(ISTART,ICOMMA,NCU,CHDUM,UNPLOT(IPLOT(NV)))
        IF( INDEX( ADUM(1:),'solute' ).NE.0 ) THEN
          WRITE( IWR,'(2X,3A,2X,2A,I2,A)' ) ADUM(1:NCH),', ',
     &      UNPLOT(IPLOT(NV))(1:NCU),SOLNM(1:NCS),' Solute(',NSL,')'

        ELSEIF( INDEX( ADUM(1:),'species' ).NE.0 ) THEN
          WRITE( IWR,'(2X,3A,2X,2A,I2,A)' ) ADUM(1:NCH),', ',
     &      UNPLOT(IPLOT(NV))(1:NCU),SPNM(1:NCS),' Species(',NSP,')'

        ELSE
          WRITE( IWR,'(2X,3A)' ) ADUM(1:NCH),', ',
     &      UNPLOT(IPLOT(NV))(1:NCU)
        ENDIF
        CALL RDOUUN( IPLOT(NV) )
        VAR = 0.D+0
        INDX = 0
        CALL RDUNIT( UNPLOT(IPLOT(NV)),VAR,INDX )
  400 CONTINUE
      NVPLOT = NVPLOT + NVC
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDOU1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDSCLF1
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
!     Water Mode
!
!     Read input file for rock/soil scaling factor information.
!
!     1 -- Saturated Hydraulic Conductivity
!     2 -- Diffusive Porosity
!     3 -- van Genuchten "alpha" parameter
!     3 -- Brooks and Corey "psi" parameter
!     4 -- van Genuchten "n" parameter
!     4 -- Brooks and Corey "lambda" parameter
!     5 -- Residual saturation
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 24 April 2001.
!     Last Modified by MD White, PNNL, 24 April 2001.
!     $Id: stomp_w.F 1081 2017-03-14 17:09:57Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
      USE GRID
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
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM
      CHARACTER*512 CHDUM
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDSCLF1'
!
!---  Write card information to output file  ---
!
      CARD = 'Scaling Factor Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read scaling factor equation types  ---
!
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      VARB = 'Saturated Hydraulic Conductivity Scaling Function'
      CALL RDCHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
      IF( INDEX(ADUM(1:),'log').NE.0 ) THEN
        IGAMMA(1) = 2
        WRITE(IWR,'(2A)') VARB(1:NCH),': Logarithmic Scaling'
      ELSE
        IGAMMA(1) = 1
        WRITE(IWR,'(2A)') VARB(1:NCH),': Linear Scaling'
      ENDIF
      VARB = 'Diffusive Porosity Scaling Function'
      CALL RDCHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
      IF( INDEX(ADUM(1:),'log').NE.0 ) THEN
        IGAMMA(2) = 2
        WRITE(IWR,'(2A)') VARB(1:NCH),': Logarithmic Scaling'
      ELSE
        IGAMMA(2) = 1
        WRITE(IWR,'(2A)') VARB(1:NCH),': Linear Scaling'
      ENDIF
      VARB = 'van Genuchten "alpha" or Brooks/Corey "psi" ' //
     &  'Scaling Function'
      CALL RDCHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
      IF( INDEX(ADUM(1:),'log').NE.0 ) THEN
        IGAMMA(3) = 2
        WRITE(IWR,'(2A)') VARB(1:NCH),': Logarithmic Scaling'
      ELSE
        IGAMMA(3) = 1
        WRITE(IWR,'(2A)') VARB(1:NCH),': Linear Scaling'
      ENDIF
      VARB = 'van Genuchten "n" or Brooks/Corey "lambda" ' //
     &  'Scaling Function'
      CALL RDCHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
      IF( INDEX(ADUM(1:),'log').NE.0 ) THEN
        IGAMMA(4) = 2
        WRITE(IWR,'(2A)') VARB(1:NCH),': Logarithmic Scaling'
      ELSE
        IGAMMA(4) = 1
        WRITE(IWR,'(2A)') VARB(1:NCH),': Linear Scaling'
      ENDIF
      VARB = 'Residual Saturation Scaling Function'
      CALL RDCHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
      IF( INDEX(ADUM(1:),'log').NE.0 ) THEN
        IGAMMA(5) = 2
        WRITE(IWR,'(2A)') VARB(1:NCH),': Logarithmic Scaling'
      ELSE
        IGAMMA(5) = 1
        WRITE(IWR,'(2A)') VARB(1:NCH),': Linear Scaling'
      ENDIF
!
!---  Read input lines until all rock/soil types are found  ---
!
      N = 0
      IJK = 0
   10 CONTINUE
      IF( N.GE.NROCK .OR. IJK.GT.0 ) GOTO 500
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      VARB = 'Rock/Soil Name'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---  Search known rock types for a matching type ---
!
      DO 100 M = 1, NROCK
        IF( ADUM .EQ. ROCK(M)) THEN
           IROCK = M
           GOTO 200
        ENDIF
  100 CONTINUE
      INDX = 2
      CHMSG = 'Unrecognized Rock/Soil Type: '//ADUM(1:NCH)
      CALL WRMSGS( INDX )
      GOTO 10
  200 CONTINUE
      WRITE (IWR,'(/,2A)') 'Rock/Soil Name: ',ROCK(IROCK)
      N = N+1
!
!---  Read saturated hydraulic conductivity (intrinsic permeability)
!     scaling factor ---
!
      IDFLT = 1
      VARB = 'Saturated Hydraulic Conductivity Scaling Factor'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(1,IROCK))
!
!---  Read fracture saturated hydraulic conductivity
!     (intrinsic permeability) scaling factor ---
!
      IF( INDEX(ADUM(1:),'fractured').NE.0 .OR.
     &  INDEX(ADUM(1:),'dp').NE.0 .OR.
     &  INDEX(ADUM(1:),'dual').NE.0 ) THEN
        IDFLT = 1
        VARB = 'Fracture Saturated Hydraulic Conductivity' //
     &    ' Scaling Factor'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(6,IROCK))
      ENDIF
!
!---  Read diffusive porosity scaling factor  ---
!
      IDFLT = 1
      VARB = 'Diffusive Porosity Scaling Factor'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(2,IROCK))
!
!---  Read fracture diffusive porosity scaling factor  ---
!
      IF( INDEX(ADUM(1:),'fractured').NE.0 .OR.
     &  INDEX(ADUM(1:),'dp').NE.0 .OR.
     &  INDEX(ADUM(1:),'dual').NE.0 ) THEN
        IDFLT = 1
        VARB = 'Fracture Diffusive Porosity' //
     &    ' Scaling Factor'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(7,IROCK))
      ENDIF
!
!---  Read van Genuchten "alpha" or Brooks/Corey "psi"
!     scaling factor ---
!
      IDFLT = 1
      VARB = 'van Genuchten "alpha" Brooks/Corey "psi" Scaling Factor'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(3,IROCK))
!
!---  Read fracture van Genuchten "alpha" or Brooks/Corey "psi"
!     scaling factor ---
!
      IF( INDEX(ADUM(1:),'fractured').NE.0 .OR.
     &  INDEX(ADUM(1:),'dp').NE.0 .OR.
     &  INDEX(ADUM(1:),'dual').NE.0 ) THEN
        IDFLT = 1
        VARB = 'Fracture van Genuchten "alpha" Brooks/Corey "psi"' //
     &    ' Scaling Factor'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(8,IROCK))
      ENDIF
!
!---  Read van Genuchten "n" or Brooks/Corey "lambda"
!     scaling factor ---
!
      IDFLT = 1
      VARB = 'van Genuchten "n" Brooks/Corey "lambda" Scaling Factor'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(4,IROCK))
!
!---  Read fracture van Genuchten "n" or Brooks/Corey "lambda"
!     scaling factor ---
!
      IF( INDEX(ADUM(1:),'fractured').NE.0 .OR.
     &  INDEX(ADUM(1:),'dp').NE.0 .OR.
     &  INDEX(ADUM(1:),'dual').NE.0 ) THEN
        IDFLT = 1
        VARB = 'Fracture van Genuchten "n" Brooks/Corey "lambda"' //
     &    ' Scaling Factor'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(9,IROCK))
      ENDIF
!
!---  Read residual saturation scaling factor ---
!
      IDFLT = 1
      VARB = 'Residual Saturation Scaling Factor'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(5,IROCK))
!
!---  Read fracture residual saturation scaling factor ---
!
      IF( INDEX(ADUM(1:),'fractured').NE.0 .OR.
     &  INDEX(ADUM(1:),'dp').NE.0 .OR.
     &  INDEX(ADUM(1:),'dual').NE.0 ) THEN
        IDFLT = 1
        VARB = 'Fracture Residual Saturation' //
     &    ' Scaling Factor'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(10,IROCK))
      ENDIF
!
!---  Read scaling parameters for Mualem-Anisotropy
!     Relative Permeability ---
!
      IF( IRPL(IROCK).EQ.301 ) THEN
        IDFLT = 1
        VARB = 'van Genuchten "m" or Brooks/Corey "lambda"' //
     &    ' Scaling Factor'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(11,IROCK))
        IDFLT = 1
        VARB = 'Horizontal Pore-Scale Scaling Factor'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(12,IROCK))
        IDFLT = 1
        VARB = 'Vertical Pore-Scale Scaling Factor'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,GAMMA(13,IROCK))
      ENDIF
      IF( N .LT. NROCK ) WRITE(IWR,'(/)')
      GOTO 10
 500  CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDSCLF1 group ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDSF1
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
!     Water Mode
!
!     Read input file for surface flux information.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!     Last Modified by CV Freedman, PNNL, 7 January 2003.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE OUTPU
      USE GRID
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
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,BDUM,FDUM,GDUM
      CHARACTER*512 CHDUM,CHDUMX
      LOGICAL FLG_CHK
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDSF1'
!
!---  Write card information to ouput file  ---
!
      CARD = 'Surface Flux Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE(IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read surface flux card information  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Surface Flux Inputs'
      CALL RDINT(ISTART,ICOMMA,CHDUM,NSF)
      IF( NSF.GT.LSF ) THEN
        INDX = 5
        CHMSG = 'Number of Surface Flux Domains > Parameter LSF'
        CALL WRMSGS( INDX )
      ENDIF
      NSFF = 0
      DO 100 NS = 1, NSF
        IF( NS.NE.1 ) WRITE(IWR, '(/)')
        CALL RDINPL( CHDUM )
        CHDUMX = CHDUM
        CALL LCASE( CHDUM )
        ISTART = 1
!
!---  Check for specified surface flux filename  ---
!
        CALL CHKINT(ISTART,ICOMMA,CHDUM,INDX)
        IF( INDX .EQ. 1 ) THEN
          VARB = 'Number of Surface Flux Inputs for the Specified File'
          CALL RDINT(ISTART,ICOMMA,CHDUMX,NSFF)
          IF( NSFF.LT.1 ) THEN
            INDX = 4
            CHMSG = 'Number of Surface Flux Inputs < 1'
            CALL WRMSGS( INDX )
          ENDIF
          VARB = 'Surface Output Filename: '
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUMX,ADUM)
          NSFGP = NSFGP + 1
          ISFGP(NSFGP) = NSFF
          IF( NSFGP.GT.LSF ) THEN
            INDX = 4
            CHMSG = 'Number of Surface Flux Files > LSF'
            CALL WRMSGS( INDX )
          ENDIF
          FNSF(NSFGP) = ADUM
          CALL RDINPL( CHDUM )
          CALL LCASE( CHDUM )
          ISTART = 1
        ENDIF
        IF( NSFF.GT.0 ) THEN
          ISFF(NS) = NSFGP
          NSFF = NSFF-1
        ELSE
          NSFF = 0
          ISFF(NS) = 1
          ISFGP(1) = ISFGP(1) + 1
        ENDIF
!
!---  Read surface flux type  ---
!
        VARB = 'Surface Flux Type: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        WRITE(IWR,'(/,A,$)') VARB(1:IVR)
        IF( INDEX(ADUM(1:),'aqueous').NE.0) THEN
          IF( INDEX(ADUM(1:),'volum').NE.0) THEN
            ISFT(NS) = 2
            WRITE(IWR,'(A)') 'Aqueous-Phase Volumetric Flux Surface'
            UNSF(1,NS) = 'm^3/s'
            UNSF(2,NS) = 'm^3'
          ELSE
            ISFT(NS) = 5
            WRITE(IWR,'(A)') 'Aqueous-Phase Mass Flux Surface'
            UNSF(1,NS) = 'kg/s'
            UNSF(2,NS) = 'kg'
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'solute').NE.0 ) THEN
          VARB = 'Solute Name: '
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
          DO 10 NSL = 1,NSOLU
            IDB = INDEX(SOLUT(NSL)(1:),'  ')
            IF( BDUM(1:IDB).EQ.SOLUT(NSL)(1:IDB) ) THEN
              ISFT(NS) = NSL+100
              WRITE(IWR,'(2X,2A)') SOLUT(NSL),' Flux Surface'
              UNSF(1,NS) = 'sol/s'
              UNSF(2,NS) = 'sol'
              GOTO 12
            ENDIF
   10     CONTINUE
            INDX = 4
            CHMSG = 'Unrecognized Surface Flux Solute Name: '//BDUM
            CALL WRMSGS( INDX )
   12     CONTINUE

!
!---    Conservation-component species surface flux input  ---
!
        ELSEIF( INDEX(ADUM(1:),'conservation').NE.0 .AND.
     &    INDEX(ADUM(1:),'component').NE.0 ) THEN
          VARB = 'Conservation-Component Species Name: '
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
          DO 14 NSL = NSOLU+1,NSOLU+NEQC
            IDB = INDEX(SOLUT(NSL)(1:),'  ')
            IF( BDUM(1:IDB).EQ.SOLUT(NSL)(1:IDB) ) THEN
              ISFT(NS) = NSL+100
              WRITE(IWR,'(2X,2A)') SOLUT(NSL),' Flux Surface'
              UNSF(1,NS) = 'mol/s'
              UNSF(2,NS) = 'mol'
              GOTO 16
            ENDIF
   14     CONTINUE
            INDX = 4
            CHMSG = 'Unrecognized Conservation-Component ' // 
     &       'Species Name: '//BDUM
            CALL WRMSGS( INDX )
   16     CONTINUE
!
!---    Kinetic-component species surface flux input  ---
!
        ELSEIF( INDEX(ADUM(1:),'kinetic').NE.0 .AND.
     &    INDEX(ADUM(1:),'component').NE.0 ) THEN
          VARB = 'Kinetic-Component Species Name: '
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
          DO 18 NSL = NSOLU+NEQC+1,NSOLU+NEQC+NEQK
            IDB = INDEX(SOLUT(NSL)(1:),'  ')
            IF( BDUM(1:IDB).EQ.SOLUT(NSL)(1:IDB) ) THEN
              ISFT(NS) = NSL+100
              WRITE(IWR,'(2X,2A)') SOLUT(NSL),' Flux Surface'
              UNSF(1,NS) = 'mol/s'
              UNSF(2,NS) = 'mol'
              GOTO 20
            ENDIF
   18     CONTINUE
            INDX = 4
            CHMSG = 'Unrecognized Kinetic-Component ' // 
     &       'Species Name: '//BDUM
            CALL WRMSGS( INDX )
   20     CONTINUE

        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Surface Flux Type: '//ADUM
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Read surface flux variable units  ---
!
        IDFLT = 1
        VARB = 'Surface Flux Rate Variable Unit'
        CALL RDCHR(ISTART,ICOMMA,NCU,CHDUM,UNSF(1,NS))
        CALL RDSFUN( ISFT(NS) )
        VAR = 0.D+0
        INDX = 0
        CALL RDUNIT(UNSF(1,NS),VAR,INDX)
        IDFLT = 1
        VARB = 'Surface Flux Integral Variable Unit'
        CALL RDCHR(ISTART,ICOMMA,NCU,CHDUM,UNSF(2,NS))
        INDX = -ISFT(NS)
        CALL RDSFUN( INDX )
        VAR = 0.D+0
        INDX = 0
        CALL RDUNIT(UNSF(2,NS),VAR,INDX)
!
!---    Read surface flux orientation  ---
!
        VARB = 'Surface Flux Orientation: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        WRITE(IWR,'(A,$)') VARB(1:IVR)
        ISFSN(NS) = 0
        IF( INDEX(ADUM(1:),'surface normal').NE.0 )  ISFSN(NS) = 1
        IF( INDEX(ADUM(1:),'west').NE.0) THEN
          ISFD(NS) = -1
          WRITE(IWR,'(A)') 'X-Direction: West Surface'
        ELSEIF( INDEX(ADUM(1:),'east').NE.0) THEN
          ISFD(NS) = 1
          WRITE(IWR,'(A)') 'X-Direction: East Surface'
        ELSEIF( INDEX(ADUM(1:),'south').NE.0) THEN
          ISFD(NS) = -2
          WRITE(IWR,'(A)') 'Y-Direction: South Surface'
        ELSEIF( INDEX(ADUM(1:),'north').NE.0) THEN
          ISFD(NS) = 2
          WRITE(IWR,'(A)') 'Y-Direction: North Surface'
        ELSEIF( INDEX(ADUM(1:),'bottom').NE.0) THEN
          ISFD(NS) = -3
          WRITE(IWR,'(A)') 'Z-Direction: Bottom Surface'
        ELSEIF( INDEX(ADUM(1:),'top').NE.0) THEN
          ISFD(NS) = 3
          WRITE(IWR,'(A)') 'Z-Direction: Top Surface'
        ELSEIF( INDEX(ADUM(1:),'file').NE.0 ) THEN
          CALL RDCHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
          NCHF = INDEX(FDUM,'  ')-1
          INQUIRE( FILE=FDUM(1:NCHF), FORM=GDUM, EXIST=FLG_CHK )
          IF( .NOT.FLG_CHK ) THEN
            INDX = 4
            CHMSG = 'Surface-Flux-Domain File: '
     &        // FDUM(1:NCHF)
            CALL WRMSGS( INDX )
          ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
            INDX = 4
            CHMSG = 'Unformatted Surface-Flux-Domain File: '
     &        // FDUM(1:NCHF)
            CALL WRMSGS( INDX )
          ENDIF
          OPEN(UNIT=26,FILE=FDUM(1:NCHF),STATUS='OLD',FORM='FORMATTED')
          WRITE(IWR,'(/,2A)') 'Surface-Flux-Domain File: ',
     &      FDUM(1:NCHF)
          ISFD(NS) = 4
          ISFC(1,NS) = 1
          ISFC(2,NS) = 1
          ISFC(3,NS) = 1
          ISFC(4,NS) = 1
          ISFC(5,NS) = 1
          ISFC(6,NS) = 1
          NC = 0
   30     CONTINUE
          READ(26,*,END=40) IX,JX,KX,ISFDX
          NC = NC + 1
          IF( NC.GT.LSFDOM ) THEN
            INDX = 5
            CHMSG = 'Number of Surface-Flux-Domain Surfaces ' //
     &        '> Parameter LSFDOM'
            CALL WRMSGS( INDX )
          ENDIF
          ISFDOM(1,NC,NS) = IX
          ISFDOM(2,NC,NS) = JX
          ISFDOM(3,NC,NS) = KX
          ISFDOM(4,NC,NS) = ISFDX
          GOTO 30
   40     CONTINUE
          NSFDOM(NS) = NC
          CLOSE(26)
        ENDIF
!
!---    Check surface flux domain  ---
!
        IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
          DO 50 NC = 1,NSFDOM(NS)
            IX = ISFDOM(1,NC,NS)
            JX = ISFDOM(2,NC,NS)
            KX = ISFDOM(3,NC,NS)
            ISFDX = ISFDOM(4,NC,NS)
            IF( IX.LT.1 .OR. IX.GT.IFLD ) THEN
              INDX = 4
              CHMSG = 'Illegal Surface Flux Domain: I Index'
              CALL WRMSGS( INDX )
            ENDIF
            IF( JX.LT.1 .OR. JX.GT.JFLD ) THEN
              INDX = 4
              CHMSG = 'Illegal Surface Flux Domain: J Index'
              CALL WRMSGS( INDX )
            ENDIF
            IF( KX.LT.1 .OR. KX.GT.KFLD ) THEN
              INDX = 4
              CHMSG = 'Illegal Surface Flux Domain: K Index'
              CALL WRMSGS( INDX )
            ENDIF
   50     CONTINUE  

        ELSE
!
!---  Read and check surface flux domain  ---
!
          VARB = 'Surface Flux Domain: '
          CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC(1,NS))
          CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC(2,NS))
          CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC(3,NS))
          CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC(4,NS))
          CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC(5,NS))
          CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC(6,NS))
          IF( ISFC(1,NS).LT.1 .OR. ISFC(1,NS).GT.IFLD .OR.
     &      ISFC(2,NS).LT.1 .OR. ISFC(2,NS).GT.IFLD .OR.
     &      ISFC(1,NS).GT.ISFC(2,NS) ) THEN
            INDX = 4
            CHMSG = 'Illegal Surface Flux Domain: I Indices'
            CALL WRMSGS( INDX )
          ENDIF
          IF( ISFC(3,NS).LT.1 .OR. ISFC(3,NS).GT.JFLD .OR.
     &      ISFC(4,NS).LT.1 .OR. ISFC(4,NS).GT.JFLD .OR.
     &      ISFC(3,NS).GT.ISFC(4,NS) ) THEN
            INDX = 4
            CHMSG = 'Illegal Surface Flux Domain: J Indices'
            CALL WRMSGS( INDX )
          ENDIF
          IF( ISFC(5,NS).LT.1 .OR. ISFC(5,NS).GT.KFLD .OR.
     &      ISFC(6,NS).LT.1 .OR. ISFC(6,NS).GT.KFLD .OR.
     &      ISFC(5,NS).GT.ISFC(6,NS) ) THEN
            INDX = 4
            CHMSG = 'Illegal Surface Flux Domain: K Indices'
            CALL WRMSGS( INDX )
          ENDIF
          ISFC(1,NS) = MAX( 1,ISFC(1,NS) )
          ISFC(1,NS) = MIN( IFLD,ISFC(1,NS),ISFC(2,NS) )
          ISFC(2,NS) = MAX( 1,ISFC(1,NS),ISFC(2,NS) )
          ISFC(2,NS) = MIN( IFLD,ISFC(2,NS) )
          ISFC(3,NS) = MAX( 1,ISFC(3,NS) )
          ISFC(3,NS) = MIN( JFLD,ISFC(3,NS),ISFC(4,NS) )
          ISFC(4,NS) = MAX( 1,ISFC(3,NS),ISFC(4,NS) )
          ISFC(4,NS) = MIN( JFLD,ISFC(4,NS) )
          ISFC(5,NS) = MAX( 1,ISFC(5,NS) )
          ISFC(5,NS) = MIN( KFLD,ISFC(5,NS),ISFC(6,NS) )
          ISFC(6,NS) = MAX( 1,ISFC(5,NS),ISFC(6,NS) )
          ISFC(6,NS) = MIN( KFLD,ISFC(6,NS) )
          WRITE(IWR,'(/,A)') VARB(1:IVR)
          WRITE(IWR, '(2X,2(A,I6))') 'I = ',ISFC(1,NS),' to ',ISFC(2,NS)
          WRITE(IWR, '(2X,2(A,I6))') 'J = ',ISFC(3,NS),' to ',ISFC(4,NS)
          WRITE(IWR, '(2X,2(A,I6))') 'K = ',ISFC(5,NS),' to ',ISFC(6,NS)
        ENDIF
  100 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDSF1 group.
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDSP1
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
!     Water Mode
!
!     Read input file for rock/soil saturation function information.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 19, 1997.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TABL
      USE SOLTN
      USE PORMED
      USE GRID
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
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,RDUM,UNTS
      CHARACTER*512 CHDUM
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDSP1'
!
!---  Write card information to ouput file  ---
!
      CARD = 'Rock/Soil Saturation Function Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE(IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Loop over the rock/soil saturation information lines  ---
!
      N = 0
      IJK = 0
      ISGRP = 0
   10 CONTINUE
      IF( N.GE.NROCK .OR. IJK.GT.0 ) GOTO 500
      ISTART = 1
      VARB = 'Saturation Function: Rock Name: '
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,RDUM)
!
!---  IJK, KIJ, or JKI indexing  ---
!
      IF( INDEX(RDUM(1:),'indexing').NE.0 ) THEN
        IF( INDEX(ROCK(1)(1:),'indexing').EQ.0 ) THEN
          INDX = 4
          CHMSG = 'Indexing Option Not Declared ' // 
     &      'in Rock/Soil Zonation Card'
          CALL WRMSGS( INDX )
        ENDIF
        IF( INDEX(RDUM,'ijk').NE.0 ) THEN
          IJK = 1
        ELSEIF( INDEX(RDUM,'jki').NE.0 ) THEN
          IJK = 2
        ELSEIF( INDEX(RDUM,'kij').NE.0 ) THEN
          IJK = 3
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Indexing Option' // RDUM(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
        GOTO 220
      ENDIF
!
!---  Search known rock types for a matching type ---
!
      DO 100 M = 1, NROCK
        IF( RDUM.EQ.ROCK(M)) THEN
          IROCK = M
          GOTO 200
        ENDIF
  100 CONTINUE
!
!---  Search known scaling groups for a matching type ---
!
      IF( ISLC(19).EQ.1 ) THEN
        DO 110 M = 1,NSCALE
           IF( RDUM.EQ.SCALNM(M) ) THEN
              ISGRP = M
              IROCK = 1
              GOTO 200
           ENDIF
  110   CONTINUE
        INDX = 2
        CHMSG = 'Unrecognized Rock/Soil Type or Scaling Group: '
     &    // RDUM(1:NCH)
        CALL WRMSGS( INDX )
        GOTO 10
      ENDIF
      INDX = 2
      CHMSG = 'Unrecognized Rock/Soil Type: ' // RDUM(1:NCH)
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
!---  Read saturation/capillary pressure function  ---
!
      VARB = 'Saturation Function Type: '
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      ISMX = 0
      IF( INDEX(ADUM(1:),'extended').NE.0 ) ISMX = 1
      IF( INDEX(ADUM(1:),'entrap').NE.0 ) THEN
        IF( INDEX(ADUM(1:),'van genuchten').NE.0 ) THEN
          ISCHRX = 101
        ELSEIF( INDEX(ADUM(1:),'brooks').NE.0 .AND.
     &    INDEX(ADUM(1:),'corey').NE.0 ) THEN
          ISCHRX = 102
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Saturation Function: '//ADUM
          CALL WRMSGS( INDX )
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'finger').NE.0 .OR.
     &  INDEX(ADUM(1:),'triple').NE.0 ) THEN
        IF( INDEX(ADUM(1:),'van genuchten').NE.0 ) THEN
          ISCHRX = 301
        ELSEIF( INDEX(ADUM(1:),'brooks').NE.0 .AND.
     &    INDEX(ADUM(1:),'corey').NE.0 ) THEN
          ISCHRX = 302
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Saturation Function: '//ADUM
          CALL WRMSGS( INDX )
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'van genuchten').NE.0 ) THEN
        ISCHRX = 1
        IF( INDEX(RDUM(1:),'fractured').NE.0 .OR.
     &    INDEX(RDUM(1:),'dp').NE.0 .OR.
     &    INDEX(RDUM(1:),'dual').NE.0 ) ISCHRX = 3
      ELSEIF( INDEX(ADUM(1:),'brooks').NE.0 .AND.
     &    INDEX(ADUM(1:),'corey').NE.0 ) THEN
        ISCHRX = 2
        IF( INDEX(RDUM(1:),'fractured').NE.0 .OR.
     &    INDEX(RDUM(1:),'dp').NE.0 .OR.
     &    INDEX(RDUM(1:),'dual').NE.0 ) ISCHRX = 4
      ELSEIF( INDEX(ADUM(1:),'haverkamp').NE.0 ) THEN
        ISCHRX = 5
        IF( INDEX(ADUM(1:),'log').NE.0 ) ISCHRX = -5
      ELSEIF( INDEX(ADUM(1:),'well').NE.0 ) THEN
        ISCHRX = 7
        WRITE(IWR,'(A)') 'Well Function'
      ELSEIF( INDEX(ADUM(1:),'luckner').NE.0 ) THEN
        ISCHRX = 8
      ELSEIF( INDEX(ADUM(1:),'russo').NE.0 ) THEN
        ISCHRX = 9
      ELSEIF( INDEX(ADUM(1:),'tabular').NE.0 ) THEN
        IF( INDEX( ADUM(1:),'spline' ).NE.0 ) THEN
          IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
            ISCHRX = 13
            WRITE(IWR,'(A)') 'Cubic-Spline-Log Interpolation'
          ELSE
            ISCHRX = 11
            WRITE(IWR,'(A)') 'Cubic-Spline Interpolation'
          ENDIF
        ELSE
          IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
            ISCHRX = 12
            WRITE(IWR,'(A)') 'Linear-Log Interpolation'
          ELSE
            ISCHRX = 10
            WRITE(IWR,'(A)') 'Linear Interpolation'
          ENDIF
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'polynomial').NE.0 ) THEN
        ISCHRX = 19
      ELSEIF( INDEX(ADUM(1:),'cambridge').NE.0 ) THEN
        ISCHRX = 41
      ELSE
        INDX = 4
        CHMSG = 'Unrecognized Saturation Function: '//ADUM
        CALL WRMSGS( INDX )
      ENDIF
      IF( IJK.GT.0 ) THEN
        DO 230 N = 1,NFLD
          ISCHR(IZ(N)) = ISCHRX
          IF( ISCHRX.EQ.7 ) IRPL(IZ(N)) = 7
          ISM(IZ(N)) = ISMX
  230   CONTINUE
      ELSE
        ISCHR(IROCK) = ISCHRX
        IF( ISCHRX.EQ.7 ) IRPL(IROCK) = 7
        ISM(IROCK) = ISMX
      ENDIF
!
!---  van Genuchten Function  ---
!
      IF( ISCHRX.EQ.1 .OR. ISCHRX.EQ.101 ) THEN
        IF( ISCHRX.EQ.1 ) THEN
          WRITE(IWR,'(A)') 'van Genuchten Function'
        ELSEIF( ISCHRX.EQ.101 ) THEN
          WRITE(IWR,'(A)') 'van Genuchten Function w/ Gas Entrapment'
        ENDIF
        VARB = 'van Genuchten (alpha)'
        IF( IJK.GT.0 ) THEN
          INDX = 1
          LNDX = LSCHR
          UNTS = '1/m'
          IUNM = -1
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,SCHR,INDX,LNDX )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(1,IROCK))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',SCHR(1,IROCK)
          INDX = 0
          IUNM = -1
          CALL RDUNIT(UNTS,SCHR(1,IROCK),INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHR(1,IROCK),', 1/m)'
        ENDIF
        VARB = 'van Genuchten (n): '
        IF( IJK.GT.0 ) THEN
          INDX = 3
          LNDX = LSCHR
          UNTS = 'null'
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,SCHR,INDX,LNDX )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(3,IROCK))
          WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(3,IROCK)
        ENDIF
        VARB = 'van Genuchten (residual saturation): '
        IF( IJK.GT.0 ) THEN
          INDX = 4
          LNDX = LSCHR
          UNTS = 'null'
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,SCHR,INDX,LNDX )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(4,IROCK))
          WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(4,IROCK)
        ENDIF
        IF( IJK.GT.0 ) THEN
          DO 240 N = 1,NFLD
            SCHR(9,IZ(N)) = 1.D+20
  240     CONTINUE
        ELSE
          SCHR(9,IROCK) = 1.D+20
        ENDIF
        VARB = 'van Genuchten (m)'
        IF( IJK.GT.0 ) THEN
          INDX = 14
          LNDX = LSCHR
          UNTS = 'null'
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,SCHR,INDX,LNDX )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(14,IROCK))
          WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',SCHR(14,IROCK)
        ENDIF
        IF( ISCHRX.EQ.101 ) THEN
          VARB = 'van Genuchten (Effective Gas Residual Sat.: ' //
     &      'Gas-Aqueous System)'
          IF( IJK.GT.0 ) THEN
            INDX = 15
            LNDX = LSCHR
            UNTS = 'null'
            CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,SCHR,INDX,LNDX )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(15,IROCK))
            WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &        SCHR(15,IROCK)
          ENDIF
          VARB = 'van Genuchten (Critical Trapping Number)'
          IDFLT = 1
          IF( IJK.GT.0 ) THEN
            INDX = 9
            LNDX = LSCHR
            UNTS = 'null'
            CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,SCHR,INDX,LNDX )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(9,IROCK))
            WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &        SCHR(9,IROCK)
          ENDIF
        ENDIF
!
!---  van Genuchten triple curve function  ---
!
      ELSEIF( ISCHRX.EQ.301 ) THEN
        WRITE(IWR,'(A)') 'van Genuchten Triple Curve Function'
        VARB = 'van Genuchten (main drainage alpha)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(1,IROCK))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',SCHR(1,IROCK)
        INDX = 0
        IUNM = -1
        CALL RDUNIT(UNTS,SCHR(1,IROCK),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHR(1,IROCK),', 1/m)'
        VARB = 'van Genuchten (main drainage n): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(3,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(3,IROCK)
        VARB = 'van Genuchten (main drainage residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(4,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(4,IROCK)
        SCHR(9,IROCK) = 1.D+20
        VARB = 'van Genuchten (main drainage m)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(14,IROCK))
        WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',SCHR(14,IROCK)
        VARB = 'van Genuchten (boundary wetting scanning alpha)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(12,IROCK))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',SCHR(12,IROCK)
        INDX = 0
        IUNM = -1
        CALL RDUNIT(UNTS,SCHR(12,IROCK),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHR(12,IROCK),', 1/m)'
        VARB = 'van Genuchten (main wetting alpha)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(2,IROCK))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',SCHR(2,IROCK)
        INDX = 0
        IUNM = -1
        CALL RDUNIT(UNTS,SCHR(2,IROCK),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHR(2,IROCK),', 1/m)'
        VARB = 'van Genuchten (main wetting n): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(5,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(5,IROCK)
        VARB = 'van Genuchten (main wetting residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(6,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(6,IROCK)
        VARB = 'van Genuchten (main wetting m)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(7,IROCK))
        WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',SCHR(7,IROCK)
        VARB = 'van Genuchten (wetting maximum saturation)'
        SCHR(10,IROCK) = 1.D+0
        IDFLT = 1
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(10,IROCK))
        WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',SCHR(10,IROCK)
        IF( SCHR(4,IROCK).LT.SCHR(6,IROCK) ) THEN
          INDX = 4
          NCH = INDEX( ROCK(IROCK),'  ' ) - 1
          CHMSG = 'Drainage SM < Imbibition SM @ Rock '//
     &      ROCK(IROCK)(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
!
!---  Brooks and Corey Function  ---
!
      ELSEIF( ISCHRX.EQ.2 .OR. ISCHRX.EQ.102 ) THEN
        IF( ISCHRX.EQ.2 ) THEN
          WRITE(IWR,'(A)') 'Brooks and Corey Function'
        ELSEIF( ISCHRX.EQ.102 ) THEN
         WRITE(IWR,'(A)')'Brooks and Corey Function w/ Gas Entrapment'
        ENDIF
        VARB = 'Brooks and Corey (psi): '
        IF( IJK.GT.0 ) THEN
          INDX = 1
          LNDX = LSCHR
          UNTS = 'm'
          IUNM = 1
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,SCHR,INDX,LNDX )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(1,IROCK))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',SCHR(1,IROCK)
          INDX = 0
          IUNM = 1
          CALL RDUNIT(UNTS,SCHR(1,IROCK),INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHR(1,IROCK),', m)'
        ENDIF
        VARB = 'Brooks and Corey (lambda): '
        IF( IJK.GT.0 ) THEN
          INDX = 3
          LNDX = LSCHR
          UNTS = 'null'
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,SCHR,INDX,LNDX )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(3,IROCK))
          WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(3,IROCK)
        ENDIF
        VARB = 'Brooks and Corey (residual saturation): '
        IF( IJK.GT.0 ) THEN
          INDX = 4
          LNDX = LSCHR
          UNTS = 'null'
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,SCHR,INDX,LNDX )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(4,IROCK))
          WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(4,IROCK)
        ENDIF
        IF( IJK.GT.0 ) THEN
          DO 250 N = 1,NFLD
            SCHR(9,IZ(N)) = 1.D+20
  250     CONTINUE
        ELSE
          SCHR(9,IROCK) = 1.D+20
        ENDIF
        IF( ISCHRX.EQ.102 ) THEN
          VARB = 'Brooks and Corey (Eff. Gas Residual ' //
     &      'Sat.: Gas-Aqueous System)'
          IF( IJK.GT.0 ) THEN
            INDX = 15
            LNDX = LSCHR
            UNTS = 'null'
            CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,SCHR,INDX,LNDX )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(15,IROCK))
            WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &        SCHR(15,IROCK)
          ENDIF
          VARB = 'Brooks and Corey (Critical Trapping Number)'
          IF( IJK.GT.0 ) THEN
            INDX = 9
            LNDX = LSCHR
            UNTS = 'null'
            CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,SCHR,INDX,LNDX )
          ELSE
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(9,IROCK))
            WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &        SCHR(9,IROCK)
          ENDIF
        ENDIF
!
!---  Brooks and Corey Triple Curve Function  ---
!
      ELSEIF( ISCHRX.EQ.302 ) THEN
       WRITE(IWR,'(A)')'Brooks and Corey Triple Curve Function'
        VARB = 'Brooks and Corey (main drainage psi): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(1,IROCK))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',SCHR(1,IROCK)
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,SCHR(1,IROCK),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHR(1,IROCK),', m)'
        VARB = 'Brooks and Corey (main drainage lambda): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(3,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(3,IROCK)
        VARB = 'Brooks and Corey(main drainage residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(4,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(4,IROCK)
        VARB = 'Brooks and Corey (boundary wetting scanning psi): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(12,IROCK))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',SCHR(12,IROCK)
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,SCHR(1,IROCK),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHR(12,IROCK),', m)'
        VARB = 'Brooks and Corey (main wetting psi): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(2,IROCK))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',SCHR(2,IROCK)
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,SCHR(1,IROCK),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHR(2,IROCK),', m)'
        VARB = 'Brooks and Corey (main wetting lambda): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(5,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(5,IROCK)
        VARB = 'Brooks and Corey (main wetting residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(6,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(6,IROCK)
        VARB = 'Brooks and Corey (wetting maximum saturation)'
        SCHR(10,IROCK) = 1.D+0
        IDFLT = 1
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(10,IROCK))
        WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',SCHR(10,IROCK)
        IF( SCHR(4,IROCK).LT.SCHR(6,IROCK) ) THEN
          INDX = 4
          NCH = INDEX( ROCK(IROCK),'  ' ) - 1
          CHMSG = 'Drainage SM < Imbibition SM @ Rock '//
     &      ROCK(IROCK)(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
!
!---  Dual Porosity van Genuchten Function  ---
!
      ELSEIF( ISCHRX.EQ.3 ) THEN
        WRITE(IWR,'(A)') 'Dual Porosity van Genuchten Function'
        VARB = 'Matrix van Genuchten (alpha)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(1,IROCK))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',SCHR(1,IROCK)
        INDX = 0
        IUNM = -1
        CALL RDUNIT(UNTS,SCHR(1,IROCK),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHR(1,IROCK),', 1/m)'
        VARB = 'Matrix van Genuchten (n): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(3,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(3,IROCK)
        VARB = 'Matrix van Genuchten (residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(4,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(4,IROCK)
        VARB = 'Fracture van Genuchten (alpha), '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(5,IROCK))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',SCHR(5,IROCK)
        INDX = 0
        IUNM = -1
        CALL RDUNIT(UNTS,SCHR(5,IROCK),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHR(5,IROCK),', 1/m)'
        VARB = 'Fracture van Genuchten (n): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(6,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(6,IROCK)
        VARB = 'Fracture van Genuchten (residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(7,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(7,IROCK)
        VARB = 'Matrix van Genuchten (m)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(14,IROCK))
        WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',SCHR(14,IROCK)
        VARB = 'Fracture van Genuchten (m)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(15,IROCK))
        WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',SCHR(15,IROCK)
!
!---  Dual Porosity Brooks and Corey Function  ---
!
      ELSEIF( ISCHRX.EQ.4 ) THEN
        WRITE(IWR,'(A)') 'Dual Porosity Brooks and Corey Function'
        VARB = 'Matrix Brooks and Corey (psi)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(1,IROCK))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',SCHR(1,IROCK)
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,SCHR(1,IROCK),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHR(1,IROCK),', m)'
        VARB = 'Matrix Brooks and Corey (lambda): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(3,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(3,IROCK)
        VARB = 'Matrix Brooks and Corey (residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(4,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(4,IROCK)
        VARB = 'Fracture Brooks and Corey (psi)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(5,IROCK))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',SCHR(5,IROCK)
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,SCHR(5,IROCK),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHR(5,IROCK),', m)'
        VARB = 'Fracture Brooks and Corey (lambda): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(6,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(6,IROCK)
        VARB = 'Fracture Brooks and Corey (residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(7,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(7,IROCK)
!
!---  Haverkamp Function  ---
!
      ELSEIF( ABS(ISCHRX).EQ.5 ) THEN
        IF( ISCHRX.EQ.-5 ) THEN
          WRITE(IWR,'(A)') 'Haverkamp w/ Log Function'
        ELSE
          WRITE(IWR,'(A)') 'Haverkamp Function'
        ENDIF
        VARB = 'Haverkamp Air Entry Head (psi): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(1,IROCK))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',SCHR(1,IROCK)
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,SCHR(1,IROCK),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHR(1,IROCK),', m)'
        VARB = 'Haverkamp (alpha): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(2,IROCK))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',SCHR(2,IROCK)
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,SCHR(2,IROCK),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHR(2,IROCK),', m)'
        INDX = 0
        IUNM = 1
        SCHR(5,IROCK) = 1.D+0
        CALL RDUNIT(UNTS,SCHR(5,IROCK),INDX)
        VARB = 'Haverkamp (beta): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(3,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(3,IROCK)
        VARB = 'Haverkamp (residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(4,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(4,IROCK)
!
!---  Luckner-van Genuchten-Nielsen Function  ---
!
      ELSEIF( ISCHRX.EQ.8 ) THEN
        WRITE(IWR,'(A)') 'Luckner-van Genuchten-Nielsen Function'
        VARB = 'Luckner et al. (alpha drainage)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(1,IROCK))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',SCHR(1,IROCK)
        INDX = 0
        IUNM = -1
        CALL RDUNIT(UNTS,SCHR(1,IROCK),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHR(1,IROCK),', 1/m)'
        VARB = 'Luckner et al. (alpha wetting)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(2,IROCK))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',SCHR(2,IROCK)
        INDX = 0
        IUNM = -1
        CALL RDUNIT(UNTS,SCHR(2,IROCK),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHR(2,IROCK),', 1/m)'
        VARB = 'Luckner et al. (n): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(3,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(3,IROCK)
        VARB = 'Luckner et al. (aqueous residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(4,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(4,IROCK)
        VARB = 'Luckner et al. (gas residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(5,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(5,IROCK)
        VARB = 'Luckner et al. (m)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(14,IROCK))
        WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',SCHR(14,IROCK)
!
!---  Russo Function  ---
!
      ELSEIF( ISCHRX.EQ.9 ) THEN
        WRITE(IWR,'(A)') 'Russo Function'
        VARB = 'Russo (alpha)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(1,IROCK))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',SCHR(1,IROCK)
        INDX = 0
        IUNM = -1
        CALL RDUNIT(UNTS,SCHR(1,IROCK),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHR(1,IROCK),', 1/m)'
        VARB = 'Russo (m): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(3,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(3,IROCK)
        VARB = 'Russo (residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(4,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(4,IROCK)
!
!---  Polynomial Function  ---
!
        ELSEIF( ISCHR(IROCK).EQ.19 ) THEN
          WRITE(IWR,'(A)') 'Polynomial Function'
          VARB = 'Number of Polynomial Function Pieces'
          CALL RDINT(ISTART,ICOMMA,CHDUM,NPLY_SL(IROCK))
          WRITE(IWR,'(2X,2A,I1)') VARB(1:IVR),': ',NPLY_SL(IROCK)
          IF( NPLY_SL(IROCK).GT.LPOLYN ) THEN
            INDX = 5
            CHMSG = 'Number of Saturation Polynomial Function ' //
     &        'Pieces > Parameter LPOLYN'
            CALL WRMSGS( INDX )
          ENDIF
          DO 360 NPX = 1,NPLY_SL(IROCK)
            VARB = 'Polynomial Piece #  : '
            WRITE(VARB(14:14),'(I1)') NPX
            ISTART = 1
            CALL RDINPL( CHDUM )
            CALL LCASE( CHDUM )
            VARB = 'Number of Polynomial Coefficients'
            CALL RDINT(ISTART,ICOMMA,CHDUM,NCOEF)
            IF( (NCOEF+4).GT.LPOLYC ) THEN
              INDX = 5
              CHMSG = 'Number of Saturation Polynomial Coefficients ' //
     &          ' > Parameter LPOLYC'
              CALL WRMSGS( INDX )
            ENDIF
            VARB = 'Minimum Head for Polynomial Piece'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,CPLY_SL(1,NPX,IROCK))
            VARB = 'Maximum Head for Polynomial Piece'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,CPLY_SL(2,NPX,IROCK))
            CPLY_SL(3,NPX,IROCK) = 0.D+0
            CPLY_SL(4,NPX,IROCK) = 0.D+0
            DO 340 NCX = 5,NCOEF+4
              VARB = 'Coefficient for Polynomial Piece'
              CALL RDDPR(ISTART,ICOMMA,CHDUM,CPLY_SL(NCX,NPX,IROCK))
!
!---          Maximum saturation for polynomial piece ---
!
              CPLY_SL(3,NPX,IROCK) = CPLY_SL(3,NPX,IROCK) + 
     &          CPLY_SL(NCX,NPX,IROCK)*(LOG10(CPLY_SL(1,NPX,IROCK))
     &          **(NCX-5))
!
!---          Mininum saturation for polynomial piece ---
!
              CPLY_SL(4,NPX,IROCK) = CPLY_SL(4,NPX,IROCK) + 
     &          CPLY_SL(NCX,NPX,IROCK)*(LOG10(CPLY_SL(2,NPX,IROCK))
     &          **(NCX-5))
  340       CONTINUE
            VARB = 'Head Units for Polynomial Piece'
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,3A)') VARB(1:IVR),': ',UNTS
            SCHR(1,IROCK) = 1.D+0
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,SCHR(1,IROCK),INDX)
            WRITE(IWR,'(2X,3A,1PE11.4,$)')
     &        'Minimum Head for Polynomial Piece, ',UNTS(1:NCH),
     &        ': ',CPLY_SL(1,NPX,IROCK)
            VAR = CPLY_SL(1,NPX,IROCK)*SCHR(1,IROCK)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR,', m)'
            WRITE(IWR,'(2X,3A,1PE11.4,$)')
     &        'Maximum Head for Polynomial Piece, ',UNTS(1:NCH),
     &        ': ',CPLY_SL(2,NPX,IROCK)
            VAR = CPLY_SL(2,NPX,IROCK)*SCHR(1,IROCK)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR,', m)'
            DO 350 NCX = 5,NCOEF+4
              VARB = 'Coefficient #  : '
              WRITE(VARB(14:14),'(I1)') NCX-4
              WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:17),
     &          CPLY_SL(NCX,NPX,IROCK)
  350       CONTINUE
  360     CONTINUE
!
!---  Cambridge Function  ---
!
      ELSEIF( ISCHRX.EQ.41 ) THEN
        WRITE(IWR,'(A)') 'Cambridge Function'
        VARB = 'Cambridge (alpha)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(1,IROCK))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',SCHR(1,IROCK)
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,SCHR(1,IROCK),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',SCHR(1,IROCK),', m)'
        VARB = 'Cambridge (m): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(3,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(3,IROCK)
        VARB = 'Cambridge (residual saturation): '
        CALL RDDPR(ISTART,ICOMMA,CHDUM,SCHR(4,IROCK))
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),SCHR(4,IROCK)
!
!---  Tabular  ---
!
      ELSEIF( ISCHRX.GE.10 .AND. ISCHRX.LE.13 ) THEN
        IF( INDEX( ADUM(1:),'content' ).NE.0 ) THEN
          WRITE(IWR,'(A)') 'Tabular Water Content versus ' //
     &      'Capillary Head'
        ELSE
          IF( ISCHRX.GE.12 .AND. ISCHRX.LE.13 ) THEN
            WRITE(IWR,'(A)') 'Tabular Aqueous Saturation versus ' //
     &        'Log Capillary Head'
          ELSE
            WRITE(IWR,'(A)') 'Tabular Aqueous Saturation versus ' //
     &        'Capillary Head'
          ENDIF
        ENDIF
        VARB = 'Number of Table Entries'
        CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
        WRITE(IWR,'(2A,I6)') VARB(1:IVR),': ',NLIN
        IF( NLIN.LT.2 ) THEN
          INDX = 4
          CHMSG = 'Saturation Invalid Table'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IJK.GT.0 ) THEN
          VARB = 'Capillary Head'
          UNTS = 'm'
          IUNM = 1
          NTBLX = NTBL
          ILOG = 0
          IF( INDEX( ADUM(1:),'log' ).NE.0 ) ILOG = 1
          CALL RDIJKT( ISTART,IJK,CHDUM,UNTS,TBLX,ISLTBL,
     &      NLIN,NTBLX,ILOG )
          IF( INDEX( ADUM(1:),'content' ).NE. 0 ) THEN
            VARB = 'Water Content'
          ELSE
            VARB = 'Saturation'
          ENDIF
          UNTS = 'null'
          ILOG = 0
          CALL RDIJKT( ISTART,IJK,CHDUM,UNTS,TBLY,ISLTBL,
     &      NLIN,NTBL,ILOG )
          IF( INDEX( ADUM(1:),'content' ).NE. 0 ) THEN
            DO 380 N = 1,NFLD
              DO 380 M = ISLTBL(1,N),ISLTBL(2,N)
                TBLY(M) = TBLY(M)/POR(2,N)
  380       CONTINUE
          ENDIF
        ELSE
          ISLTBL(1,IROCK) = NTBL + 1
          DO 400 NL = 1,NLIN
            NTBL = NTBL + 1
            IF( NTBL.GT.LTBL ) THEN
              INDX = 5
              CHMSG = 'Number of Table Values > Parameter LTBL'
              CALL WRMSGS( INDX )
            ENDIF
            ISTART = 1
            CALL RDINPL( CHDUM )
            CALL LCASE( CHDUM )
            VARB = 'Capillary Head'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLX(NTBL))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',TBLX(NTBL)
            INDX = 0
            IUNM = 1
            VARX = 1.D+0
            CALL RDUNIT(UNTS,VARX,INDX)
            IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
              TBLX(NTBL) = LOG( EXP(TBLX(NTBL)*VARX) )
            ELSE
              TBLX(NTBL) = VARX*TBLX(NTBL)
            ENDIF
            WRITE(IWR,'(A,1PE11.4,A)') ' (',TBLX(NTBL),', m)'
            IF( INDEX( ADUM(1:),'content' ).NE. 0 ) THEN
              VARB = 'Water Content'
            ELSE
              VARB = 'Saturation'
            ENDIF
            CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLY(NTBL))
            WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',TBLY(NTBL)
            IF( INDEX( ADUM(1:),'content' ).NE. 0 ) THEN
              TBLY(NTBL) = TBLY(NTBL)/POR(2,IROCK)
            ENDIF
            IF( NL.EQ.2 ) THEN
              IF( TBLX(NTBL-1).LT.TBLX(NTBL) ) THEN
                ITDX = 1
              ELSEIF( TBLX(NTBL-1).GT.TBLX(NTBL) ) THEN
                ITDX = -1
              ELSE
                INDX = 4
                CHMSG = 'Invalid Saturation Table'
                CALL WRMSGS( INDX )
              ENDIF
              IF( TBLY(NTBL-1).LT.TBLY(NTBL) ) THEN
                ITDY = 1
              ELSEIF( TBLY(NTBL-1).GT.TBLY(NTBL) ) THEN
                ITDY = -1
              ELSE
                INDX = 4
                CHMSG = 'Invalid Saturation Table'
                CALL WRMSGS( INDX )
              ENDIF
            ELSEIF( NL.GT.2 ) THEN
              IF( (ITDX.EQ.1 .AND. TBLX(NTBL).LE.TBLX(NTBL-1)) .OR.
     &          (ITDX.EQ.-1 .AND. TBLX(NTBL).GE.TBLX(NTBL-1)) )THEN
                INDX = 4
                CHMSG = 'Invalid Saturation Table'
                CALL WRMSGS( INDX )
              ENDIF
              IF( (ITDY.EQ.1 .AND. TBLY(NTBL).LE.TBLY(NTBL-1)) .OR.
     &          (ITDY.EQ.-1 .AND. TBLY(NTBL).GE.TBLY(NTBL-1)) )THEN
                INDX = 4
                CHMSG = 'Invalid Saturation Table'
                CALL WRMSGS( INDX )
              ENDIF
            ENDIF
  400     CONTINUE
          ISLTBL(2,IROCK) = NTBL
          IF( ISCHRX.EQ.11 ) THEN
            CALL SPLINY( ISLTBL(1,IROCK),ISLTBL(2,IROCK) )
            CALL SPLINX( ISLTBL(1,IROCK),ISLTBL(2,IROCK) )
          ENDIF
        ENDIF
      ENDIF
!
!---  Loop over remaining rock/soils within scaling group  ---
!
      IF( ISLC(19).EQ.1 .AND. IROCK.LT.NROCK ) THEN
        DO 490 M = IROCK+1,NROCK
          IF( ISCALE(M).EQ.ISGRP ) THEN
            N = N+1
            ISM(M) = ISM(IROCK)
            ISCHR(M) = ISCHR(IROCK)
            IF( ISCHR(M).EQ.7 ) IRPL(M) = 7
            DO 480 L = 1,LSCHR
              SCHR(L,M) = SCHR(L,IROCK)
  480       CONTINUE
            DO 482 L = 1,2
              ISLTBL(L,M) = ISLTBL(L,IROCK)
  482       CONTINUE
          ENDIF
  490   CONTINUE
      ENDIF
!
!---  Read next rock/soil type or scaling group  ---
!
      IF( N.LT.NROCK ) WRITE(IWR,'(/)')
      GOTO 10
 500  CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDSP1 group ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDSR1
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
!     Water Mode
!
!     Read input file for source information.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE REACT
      USE GRID
      USE FILES
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
      CHARACTER*64 ADUM,BDUM,UNTS
      CHARACTER*512 CHDUM
      REAL*8 VAR(LSTM,8+LSOLU)
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDSR1'
!
!---  Write card information to ouput file  ---
!
      CARD = 'Source Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE(IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
      NSR = 0
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Sources: '
      CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
      DO 140 NS = 1, NLIN
        NSL = 0
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
!
!---  Read source type  ---
!
        VARB = 'Source Type: '
        WRITE(IWR,'(/,A,$)') VARB(1:IVR)
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'aqueous').NE.0 ) THEN
          IF( INDEX(ADUM(1:),'mass').NE.0 ) THEN
            IF( INDEX(ADUM(1:),'density').NE.0 ) THEN
              WRITE(IWR,'(2X,A)') 'Aqueous Mass Density Source'
              ISRTX = 5
            ELSE
              WRITE(IWR,'(2X,A)') 'Aqueous Mass Source'
              ISRTX = 4
            ENDIF
          ELSEIF( INDEX(ADUM(1:),'volumetric').NE.0 ) THEN
            IF( INDEX(ADUM(1:),'density').NE.0 ) THEN
              WRITE(IWR,'(2X,A)') 'Aqueous Volumetric Density Source'
              ISRTX = 3
            ELSE
              WRITE(IWR,'(2X,A)') 'Aqueous Volumetric Source'
              ISRTX = 2
            ENDIF
          ENDIF

        ELSEIF( INDEX(ADUM(1:),'z-dir').NE.0 .AND.
     &    INDEX(ADUM(1:),'injec').NE.0 .AND.
     &    INDEX(ADUM(1:),'well').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') 'Z-Direction Vertical ' //
     &      'Injection Well Source'
            ISRTX = 31
        ELSEIF( INDEX(ADUM(1:),'x-dir').NE.0 .AND.
     &    INDEX(ADUM(1:),'injec').NE.0 .AND.
     &    INDEX(ADUM(1:),'well').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') 'X-Direction Horizontal ' //
     &      'Injection Well Source'
            ISRTX = 32
        ELSEIF( INDEX(ADUM(1:),'y-dir').NE.0 .AND.
     &    INDEX(ADUM(1:),'injec').NE.0 .AND.
     &    INDEX(ADUM(1:),'well').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') 'Y-Direction Horizontal ' //
     &      'Injection Well Source'
            ISRTX = 33
        ELSEIF( IEQC.NE.0 .AND. INDEX(ADUM(1:),'solute').NE.0 ) THEN
          VARB = 'Solute Name: '
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
          DO 30 NSL = 1,NSOLU
            IDB = INDEX(SOLUT(NSL)(1:),'  ')
            IF( BDUM(1:IDB).EQ.SOLUT(NSL)(1:IDB) ) THEN
              IF( INDEX(ADUM(1:),'iwvs').NE.0 ) THEN
                ISRTX = -(NSL+2*NSOLU)
                WRITE(IWR,'(2X,2A)')
     &            'In-Well Vapor-Stripping Solute Source: ',SOLUT(NSL)
              ELSEIF( INDEX(ADUM(1:),'density').NE.0 ) THEN
                ISRTX = -(NSL+NSOLU)
                WRITE(IWR,'(2X,2A)')'Solute Source Density: ',SOLUT(NSL)
              ELSEIF( INDEX(ADUM(1:),'inventory').NE.0 ) THEN
                ISRTX = -(NSL+3*NSOLU)
                WRITE(IWR,'(2X,2A)')'Solute Inventory Source: ',
     &            SOLUT(NSL)
              ELSEIF( INDEX(ADUM(1:),'advection').NE.0 ) THEN
                ISRTX = -(NSL+4*NSOLU)
                WRITE(IWR,'(2X,2A)')'Advection-Dominated Solute ' //
     &            'Release Model: ',SOLUT(NSL)
              ELSEIF( INDEX(ADUM(1:),'diffusion').NE.0 ) THEN
                IF( INDEX(ADUM(1:),'variable').NE.0 ) THEN
                  ISRTX = -(NSL+8*NSOLU)
                  WRITE(IWR,'(2X,2A)')'Variable Diffusion-Dominated ' //
     &              'Solute Release Model: ',SOLUT(NSL)
                ELSE
                  ISRTX = -(NSL+5*NSOLU)
                  WRITE(IWR,'(2X,2A)')'Diffusion-Dominated Solute ' //
     &              'Release Model: ',SOLUT(NSL)
                ENDIF
              ELSEIF( INDEX(ADUM(1:),'solubility').NE.0 ) THEN
                IF( INDEX(ADUM(1:),'salt').NE.0 .OR.
     &            INDEX(ADUM(1:),'cake').NE.0 ) THEN
                  ISRTX = -(NSL+7*NSOLU)
                  WRITE(IWR,'(2X,2A)')'Solubility-Controlled Salt ' //
     &              'Cake Release Model: ',SOLUT(NSL)
                ELSE
                  ISRTX = -(NSL+6*NSOLU)
                  WRITE(IWR,'(2X,2A)')'Solubility-Controlled Solute ' //
     &              'Release Model: ',SOLUT(NSL)
                ENDIF
              ELSE
                ISRTX = -NSL
                WRITE(IWR,'(2X,2A)')'Solute Source: ',SOLUT(NSL)
              ENDIF
              GOTO 40
            ENDIF
   30     CONTINUE
            INDX = 4
            CHMSG = 'Unrecognized Source Solute Name: '//BDUM
            CALL WRMSGS( INDX )
   40     CONTINUE

        ELSEIF( INDEX(ADUM(1:),'specie').NE.0 ) THEN
          VARB = 'Species Name: '
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
!
!---      Aqueous species  ---
!          
          DO 42 NSP = 1,NSPL
            IDB = INDEX(SPNML(NSP)(1:),'  ')
            IF( BDUM(1:IDB).EQ.SPNML(NSP)(1:IDB) ) THEN
              IF( INDEX(ADUM(1:),'density').NE.0 ) THEN
                ISRTX = 100+NSPL+NSPS+NSP
                WRITE(IWR,'(2X,2A)')'Solute Source Density: ',
     &            SPNML(NSP)(1:IDB)
              ELSE
                ISRTX = 100+NSP
                WRITE(IWR,'(2X,2A)')'Species Source: ',
     &            SPNML(NSP)(1:IDB)
              ENDIF
              GOTO 44
            ENDIF
   42     CONTINUE
            INDX = 4
            CHMSG = 'Unrecognized Source Species Name: '//BDUM
            CALL WRMSGS( INDX )
   44     CONTINUE

        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Source Type: '//ADUM
          CALL WRMSGS( INDX )
        ENDIF
!
!---  Read source domain indices  ---
!
        VARB = 'Source Domain Index'
        I1X = ISTART
        CALL RDINT(ISTART,ICOMMA,CHDUM,I1X)
        CALL RDINT(ISTART,ICOMMA,CHDUM,I2X)
        CALL RDINT(ISTART,ICOMMA,CHDUM,J1X)
        CALL RDINT(ISTART,ICOMMA,CHDUM,J2X)
        CALL RDINT(ISTART,ICOMMA,CHDUM,K1X)
        CALL RDINT(ISTART,ICOMMA,CHDUM,K2X)
        ICX = ISTART
        WRITE(IWR,'(/,2X,A)') 'Source Domain:'
        WRITE(IWR,'(4X,A,I6,A,I6)') 'I = ',I1X,' to ',I2X
        WRITE(IWR,'(4X,A,I6,A,I6)') 'J = ',J1X,' to ',J2X
        WRITE(IWR,'(4X,A,I6,A,I6)') 'K = ',K1X,' to ',K2X
!
!---  Check for ill-defined source domains  ---
!
        IF( I1X.LT.1 .OR. I1X.GT.IFLD .OR. I2X.LT.1 .OR.
     &    I2X.GT.IFLD .OR. I2X.LT.I1X ) THEN
          INDX = 4
          CHMSG = 'Invalid Source Domain: ' // CHDUM(ISX:ICX)
          CALL WRMSGS( INDX )
        ENDIF
        IF( J1X.LT.1 .OR. J1X.GT.JFLD .OR. J2X.LT.1 .OR.
     &    J2X.GT.JFLD .OR. J2X.LT.J1X ) THEN
          INDX = 4
          CHMSG = 'Invalid Source Domain: ' // CHDUM(ISX:ICX)
          CALL WRMSGS( INDX )
        ENDIF
        IF( K1X.LT.1 .OR. K1X.GT.KFLD .OR. K2X.LT.1 .OR.
     &    K2X.GT.KFLD .OR. K2X.LT.K1X ) THEN
          INDX = 4
          CHMSG = 'Invalid Source Domain: ' // CHDUM(ISX:ICX)
          CALL WRMSGS( INDX )
        ENDIF

!
!---  Read surface direction and surface domain indices for
!     in-well vapor-stripping type sources  ---
!
        VOLX = 0.D+0
        IF( ISRTX.LT.-2*NSOLU .AND. ISRTX.GE.-3*NSOLU ) THEN
          DO 48 I = I1X,I2X
            DO 48 J = J1X,J2X
              DO 48 K = K1X,K2X
                N = ND(I,J,K)
                VOLX = VOLX + VOL(N)
   48     CONTINUE
          VARB = 'In-Well Vapor-Stripping Orientation: '
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          WRITE(IWR,'(/,A,$)') VARB(1:IVR)
          IF( INDEX(ADUM(1:),'west').NE.0 ) THEN
            IVSDX = -1
            WRITE(IWR,'(A)') 'X-Direction: West Surface'
          ELSEIF( INDEX(ADUM(1:),'east').NE.0 ) THEN
            IVSDX = 1
            WRITE(IWR,'(A)') 'X-Direction: East Surface'
          ELSEIF( INDEX(ADUM(1:),'south').NE.0 ) THEN
            IVSDX = -2
            WRITE(IWR,'(A)') 'Y-Direction: South Surface'
          ELSEIF( INDEX(ADUM(1:),'north').NE.0 ) THEN
            IVSDX = 2
            WRITE(IWR,'(A)') 'Y-Direction: North Surface'
          ELSEIF( INDEX(ADUM(1:),'bottom').NE.0 ) THEN
            IVSDX = -3
            WRITE(IWR,'(A)') 'Z-Direction: Bottom Surface'
          ELSEIF( INDEX(ADUM(1:),'top').NE.0 ) THEN
            IVSDX = 3
            WRITE(IWR,'(A)') 'Z-Direction: Top Surface'
          ENDIF
          VARB = 'In-Well Vapor-Stripping Domain Index: '
          CALL RDINT(ISTART,ICOMMA,CHDUM,ISVS)
          CALL RDINT(ISTART,ICOMMA,CHDUM,IEVS)
          CALL RDINT(ISTART,ICOMMA,CHDUM,JSVS)
          CALL RDINT(ISTART,ICOMMA,CHDUM,JEVS)
          CALL RDINT(ISTART,ICOMMA,CHDUM,KSVS)
          CALL RDINT(ISTART,ICOMMA,CHDUM,KEVS)
          IF( ABS(IVSDX).EQ.1 .AND. ABS(ISVS-IEVS).GT.0 ) THEN
            INDX = 4
            CHMSG = 'Invalid East/West Surface Flux Domain'
            CALL WRMSGS( INDX )
          ELSEIF( ABS(IVSDX).EQ.2 .AND. ABS(JSVS-JEVS).GT.0 ) THEN
            INDX = 4
            CHMSG = 'Invalid North/South Surface Flux Domain'
            CALL WRMSGS( INDX )
          ELSEIF( ABS(IVSDX).EQ.3 .AND. ABS(KSVS-KEVS).GT.0 ) THEN
            INDX = 4
            CHMSG = 'Invalid Top/Bottom Surface Flux Domain'
            CALL WRMSGS( INDX )
          ENDIF
          IF( ISVS.LT.1 .OR. ISVS.GT.IFLD .OR. IEVS.LT.1 .OR.
     &      IEVS.GT.IFLD .OR. IEVS.LT.ISVS ) THEN
            INDX = 4
            CHMSG = 'Invalid Surface Flux Domain'
            CALL WRMSGS( INDX )
          ENDIF
          IF( JSVS.LT.1 .OR. JSVS.GT.JFLD .OR. JEVS.LT.1 .OR.
     &      JEVS.GT.JFLD .OR. JEVS.LT.JSVS ) THEN
            INDX = 4
            CHMSG = 'Invalid Surface Flux Domain'
            CALL WRMSGS( INDX )
          ENDIF
          IF( KSVS.LT.1 .OR. KSVS.GT.KFLD .OR. KEVS.LT.1 .OR.
     &      KEVS.GT.KFLD .OR. KEVS.LT.KSVS ) THEN
            INDX = 4
            CHMSG = 'Invalid Surface Flux Domain'
            CALL WRMSGS( INDX )
          ENDIF
          WRITE(IWR,'(/,A)') 'In-Well Vapor-Stripping Domain:'
          WRITE(IWR,'(2X,A,I6,A,I6)') 'I = ',ISVS,' to ',IEVS
          WRITE(IWR,'(2X,A,I6,A,I6)') 'J = ',JSVS,' to ',JEVS
          WRITE(IWR,'(2X,A,I6,A,I6)') 'K = ',KSVS,' to ',KEVS
        ENDIF
!
!---  Check for sources applied to inactive nodes  ---
!
        DO 50 K = K1X,K2X
          DO 50 J = J1X,J2X
            DO 50 I = I1X,I2X
              IF( IXP(ND(I,J,K)).EQ.0 ) THEN
                INDX = 4
                CHMSG = 'Source Applied to an Inactive Node'
                CALL WRMSGS( INDX )
              ENDIF
   50   CONTINUE
!
!---  Read number of source times  ---
!
        VARB = 'Number of Source Times: '
        CALL RDINT(ISTART,ICOMMA,CHDUM,ISRM(NS))
        IF( ISRM(NS).GT.LSTM ) THEN
          INDX = 5
          CHMSG = 'Number of Source Times > Parameter LSTM'
          CALL WRMSGS( INDX )
        ENDIF
        IF( ISRTX.EQ.(-(NSL+3*NSOLU)) .AND. ISRM(NS).GT.1 ) THEN
          INDX = 4
          CHMSG = 'Multiple Times with Solute Inventory Type Source'
          CALL WRMSGS( INDX )
        ENDIF
        SRTMO = -SMALL
        DO 100 NTM = 1,ISRM(NS)
          DO 60 M = 1,6
            VAR(NTM,M) = 0.D+0
   60     CONTINUE
!
!---  Read start time, source values, and units  ---
!
          CALL RDINPL( CHDUM )
          CALL LCASE( CHDUM )
          ISTART = 1
          VARB = 'Source Time'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,1))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(/,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',VAR(NTM,1)
          INDX = 0
          IUNS = 1
          CALL RDUNIT(UNTS,VAR(NTM,1),INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,1),', s)'
          IF( ISRTX.EQ.3 ) THEN
            VARB = 'Source Aqueous Volumetric Density Rate'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',VAR(NTM,4)
            INDX = 0
            IUNS = -1
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', 1/s)'
          ELSEIF( ISRTX.EQ.2 ) THEN
            VARB = 'Source Aqueous Volumetric Rate'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',VAR(NTM,4)
            INDX = 0
            IUNM = 3
            IUNS = -1
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', m^3/s)'
          ELSEIF( ISRTX.EQ.4 ) THEN
            VARB = 'Source Aqueous Mass Rate'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',VAR(NTM,4)
            INDX = 0
            IUNKG = 1
            IUNS = -1
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', kg/s)'
          ELSEIF( ISRTX.EQ.5 ) THEN
            VARB = 'Source Aqueous Mass Density Rate'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',VAR(NTM,4)
            INDX = 0
            IUNKG = 1
            IUNM = -3
            IUNS = -1
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', kg/m^3 s)'

!
!---      Injection Well Source  ---
!
          ELSEIF( ISRTX.GE.31 .AND. ISRTX.LE.33 ) THEN
            VARB = 'Well Pressure'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',VAR(NTM,2)
            INDX = 0
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', Pa)'
            VARB = 'Well Diameter'
            IDFLT = 1
            VAR(NTM,3) = 1.7D-1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,3))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',VAR(NTM,3)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,3),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,3),', m)'
            VARB = 'Symmetry Factor'
            IDFLT = 1
            VAR(NTM,4) = 1.D+0
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',VAR(NTM,4)
!
!---        Convert minimum well pressure to guage and well diameter
!           to well radius  ---
!
            VAR(NTM,2) = VAR(NTM,2)-PATM
            VAR(NTM,3) = 5.D-1*VAR(NTM,3)
          ELSEIF( ISRTX.LT.0 .AND. ISRTX.GE.-NSOLU ) THEN
            VARB = 'Source Solute Rate: '
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',VAR(NTM,4)
            INDX = 0
            IUNS = -1
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', 1/s)'
          ELSEIF( ISRTX.LT.-NSOLU .AND. ISRTX.GE.-2*NSOLU ) THEN
            VARB = 'Solute Density Rate'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',VAR(NTM,4)
            INDX = 0
            IUNS = -1
            IUNM = -3
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', 1/m^3 s)'
          ELSEIF( ISRTX.LT.-2*NSOLU .AND. ISRTX.GE.-3*NSOLU ) THEN
            VARB = 'Source Exhaust Gas Temperature'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,6))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(4A,1PE11.4)') VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',VAR(NTM,6)
            INDX = 0
            IUNK = 1
            CALL RDUNIT(UNTS,VAR(NTM,6),INDX)
            VARB = 'Source Exhaust Gas Pressure'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',VAR(NTM,2)
            INDX = 0
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', Pa)'
            CALL WATSP( VAR(NTM,6),PSWX )
            IF( VAR(NTM,2).LT.PSWX ) THEN
              INDX = 4
              CHMSG = 'Exhaust Gas Pressure < Sat. Water Vapor Pres.'
              CALL WRMSGS( INDX )
            ENDIF
            VARB = 'Source Air/Water Volumetric Flow Ratio'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,3))
            WRITE(IWR,'(2A,1PE11.4)') VARB(1:IVR),': ',VAR(NTM,3)
            VARB = 'Air/Water Partition Coefficient (Henry''s Const.)'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',VAR(NTM,4)
            INDX = 0
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', Pa)'
            VAR(NTM,5) = VOLX
            IDFLT = 1
            VAR(NTM,7) = 1.D+0
            VARB = 'Source Vapor Stripping Efficiency'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,7))
            WRITE(IWR,'(2A,1PE11.4)') VARB(1:IVR),': ',VAR(NTM,7)
            IF( VAR(NTM,7).GT.1.D+0 )
     &        VAR(NTM,7)=MAX(VAR(NTM,7)/1.D+2,1.D+0)
          ELSEIF( ISRTX.LT.-3*NSOLU .AND. ISRTX.GE.-4*NSOLU ) THEN
            VARB = 'Domain Solute Inventory'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,3))
            WRITE(IWR,'(2A,1PE11.4,$)') VARB(1:IVR),': ',VAR(NTM,3)
            VARB = 'Solute Aqueous Concentration'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',VAR(NTM,4)
            INDX = 0
            IUNM = -3
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', 1/m^3)'
            VAR(NTM,5) = -1.D+0
!
!---  Spread inventory uniformily over domain according to node volume
!     and define a unique source input for each node in the domain  ---
!
            VOLX = 0.D+0
            DO 90 K = K1X,K2X
            DO 90 J = J1X,J2X
            DO 90 I = I1X,I2X
              N = ND(I,J,K)
              VOLX = VOLX + VOL(N)
   90       CONTINUE
            DO 92 K = K1X,K2X
            DO 92 J = J1X,J2X
            DO 92 I = I1X,I2X
              N = ND(I,J,K)
              NSR = NSR + 1
              IF( NSR.GT.LSR ) THEN
                INDX = 5
                CHMSG = 'Number of Sources > Parameter LSR'
                CALL WRMSGS( INDX )
              ENDIF
              ISRDM(1,NSR) = I
              ISRDM(2,NSR) = I
              ISRDM(3,NSR) = J
              ISRDM(4,NSR) = J
              ISRDM(5,NSR) = K
              ISRDM(6,NSR) = K
              ISRT(NSR) = ISRTX
              ISRM(NSR) = 1
              SRC(1,NTM,NSR) = VAR(NTM,1)
              SRC(3,NTM,NSR) = VAR(NTM,3)*VOL(N)/VOLX
              YN(N,NSL) = SRC(3,NTM,NSR)
              SRC(4,NTM,NSR) = VAR(NTM,4)
              SRC(5,NTM,NSR) = VAR(NTM,5)
   92       CONTINUE
            GOTO 140
!
!---      Advection-dominated solute release model  ---
!
          ELSEIF( ISRTX.LT.-4*NSOLU .AND. ISRTX.GE.-5*NSOLU ) THEN
            VARB = 'Nodal Solute Inventory'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,3))
            WRITE(IWR,'(2A,1PE11.4,$)') VARB(1:IVR),': ',VAR(NTM,3)
            VARB = 'Vertical Depth of Residual Waste'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',VAR(NTM,4)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', m)'
            VARB = 'Number of Mixing Cells'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,5))
            WRITE(IWR,'(2A,1PE11.4,$)') VARB(1:IVR),': ',VAR(NTM,5)
!
!---      Diffusion-dominated solute release model  ---
!
          ELSEIF( ISRTX.LT.-5*NSOLU .AND. ISRTX.GE.-6*NSOLU ) THEN
            VARB = 'Nodal Solute Inventory'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,3))
            WRITE(IWR,'(2A,1PE11.4,$)') VARB(1:IVR),': ',VAR(NTM,3)
            VARB = 'Vertical Depth of Residual Waste'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',VAR(NTM,4)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', m)'
            VARB = 'Diffusion Coefficient'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,5))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',VAR(NTM,5)
            INDX = 0
            IUNM = 2
            IUNS = -1
            CALL RDUNIT(UNTS,VAR(NTM,5),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,5),', m^2/s)'
!
!---      Solubility-controlled solute release model  ---
!
          ELSEIF( ISRTX.LT.-6*NSOLU .AND. ISRTX.GE.-7*NSOLU ) THEN
!
!---        SRX(2): nodal solute inventory
!           SRX(3): aqueous solubility  ---
!
            VARB = 'Nodal Solute Inventory'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            WRITE(IWR,'(2A,1PE11.4,$)') VARB(1:IVR),': ',VAR(NTM,2)
            VARB = 'Aqueous Solubility of Solute'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,3))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',VAR(NTM,3)
            INDX = 0
            IUNM = -3
            CALL RDUNIT(UNTS,VAR(NTM,3),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,3),', 1/m^3)'
!
!---      Solubility-controlled salt cake release model  ---
!
          ELSEIF( ISRTX.LT.-7*NSOLU .AND. ISRTX.GE.-8*NSOLU ) THEN
!
!---        SRX(2): nodal solute inventory
!           SRX(3): nodal salt cake inventory
!           SRX(4): salt cake solubility  ---
!
            VARB = 'Nodal Solute Inventory'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            WRITE(IWR,'(2A,1PE11.4,$)') VARB(1:IVR),': ',VAR(NTM,2)
            VARB = 'Nodal Salt Cake Inventory'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,3))
            WRITE(IWR,'(2A,1PE11.4,$)') VARB(1:IVR),': ',VAR(NTM,3)
            VARB = 'Aqueous Solubility of Salt Cake'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',VAR(NTM,4)
            INDX = 0
            IUNM = -3
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', 1/m^3)'
!
!---      Diffusion-dominated solute release model 
!         (w/ variable diffusion) ---
!
          ELSEIF( ISRTX.LT.-8*NSOLU .AND. ISRTX.GE.-9*NSOLU ) THEN
            VARB = 'Nodal Solute Inventory'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,3))
            WRITE(IWR,'(2A,1PE11.4,$)') VARB(1:IVR),': ',VAR(NTM,3)
            VARB = 'Vertical Depth of Residual Waste'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',VAR(NTM,4)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', m)'
            VARB = 'Diffusion Coefficient'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,5))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',VAR(NTM,5)
            INDX = 0
            IUNM = 2
            IUNS = -1
            CALL RDUNIT(UNTS,VAR(NTM,5),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,5),', m^2/s)'
            VARB = 'Constrictivity'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,6))
            WRITE(IWR,'(2A,1PE11.4,$)') VARB(1:IVR),': ',VAR(NTM,6)
          ENDIF
!
!---  Check for nonascending source times  ---
!
          IF( VAR(NTM,1).LT.SRTMO ) THEN
            INDX = 4
            CHMSG = 'Invalid Source Time Sequencing'
            CALL WRMSGS( INDX )
          ENDIF
          SRTMO = VAR(NTM,1)
  100   CONTINUE
!
!---  Assign values to source variables  ---
!
        NSR = NSR + 1
        IF( NSR.GT.LSR ) THEN
          INDX = 5
          CHMSG = 'Number of Sources > Parameter LSR'
          CALL WRMSGS( INDX )
        ENDIF
        ISRDM(1,NSR) = I1X
        ISRDM(2,NSR) = I2X
        ISRDM(3,NSR) = J1X
        ISRDM(4,NSR) = J2X
        ISRDM(5,NSR) = K1X
        ISRDM(6,NSR) = K2X
        IF( ISRTX.LT.-2*NSOLU .AND. ISRTX.GE.-3*NSOLU ) THEN
          ISRDM(7,NSR) = IVSDX
          ISRDM(8,NSR) = ISVS
          ISRDM(9,NSR) = IEVS
          ISRDM(10,NSR) = JSVS
          ISRDM(11,NSR) = JEVS
          ISRDM(12,NSR) = KSVS
          ISRDM(13,NSR) = KEVS
        ENDIF
        ISRT(NSR) = ISRTX
        DO 130 NTM = 1,ISRM(NS)
          DO 120 M = 1,8+NSOLU
            SRC(M,NTM,NSR) = VAR(NTM,M)
  120     CONTINUE
  130   CONTINUE
  140 CONTINUE
!
!---  Check that solute inventory source domains are unique   ---
!
      IF( IEQC.EQ.0 ) GOTO 310
      DO 300 NS = 1,NSR
        IF( ISRT(NS).LT.(-3*NSOLU) .AND. ISRT(NS).GE.(-4*NSOLU) ) THEN
          I = ISRDM(1,NS)
          J = ISRDM(3,NS)
          K = ISRDM(5,NS)
          DO 290 MS = 1,NSR
            IF( MS.EQ.NS .OR. ( ISRT(MS).GT.0 .AND. ISRT(MS).LT.20 ) )
     &        GOTO 290
            IF( ISRT(MS).NE.ISRT(NS) ) GOTO 290
            IF( I.GE.ISRDM(1,MS) .AND. I.LE.ISRDM(2,MS) .AND.
     &        J.GE.ISRDM(3,MS) .AND. J.LE.ISRDM(4,MS) .AND.
     &        K.GE.ISRDM(5,MS) .AND. K.LE.ISRDM(6,MS) ) THEN
              INDX = 7
              CHMSG = 'Multiple Solute Sources for a ' //
     &          'Node with Solute Inventory Source Type, Node: '
              IMSG = ND(I,J,K)
            ENDIF
  290     CONTINUE
        ENDIF
  300 CONTINUE
  310 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDSR1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDTF1
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
!     Water Mode
!
!     Reads solute/fluid interaction card for diffusion and partition
!     coefficients, and internodal diffusion term averaging scheme for
!     single phase (aqueous) solute transport equation.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE FILES
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
      CHARACTER*64 ADUM,BDUM,UNTS
      CHARACTER*512 CHDUM
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDTF1'
!
!---  Write card information to output file  ---
!
      CARD = 'Solute/Fluid Interaction Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE(IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of different solutes  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Solutes'
      CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
      NSOLU = 0
      DO 200 NL = 1, NLIN
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        ADUM(1:) = ' '
        VARB = 'Solute Name'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        DO 100 NSL = 1,NSOLU
          IF( SOLUT(NSL).EQ.ADUM ) GOTO 110
  100   CONTINUE
        NSOLU = NSOLU + 1
        IF( NSOLU.GT.LSOLU ) THEN
          INDX = 5
          CHMSG = 'Number of Solutes > Parameter LSOLU'
          CALL WRMSGS( INDX )
        ENDIF
        SOLUT(NSOLU) = ADUM
        NSL = NSOLU
  110   CONTINUE
        WRITE(IWR,'(/,3A)') VARB(1:IVR),': ',ADUM
!
!---  Aqueous effective diffusion option  ---
!
        VARB = 'Aqueous Effective Diffusion Option: '
        ADUM = 'conventional'
        IDFLT = 1
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        WRITE( IWR,'(/,A,$)' ) VARB(1:IVR)
        IF( INDEX(ADUM(1:),'empirical').NE.0 )  THEN
          IEDL(NSL) = 2
          WRITE( IWR,'(A)' ) 'Kemper and van Schaik Empirical Diffusion
     &Model'
          WRITE( IWR,'(A)' ) '  Model Parameters Entered on the Solute/P
     &orous Media Interaction Card'
        ELSEIF( INDEX(ADUM(1:),'power').NE.0 )  THEN
          IEDL(NSL) = 4
          WRITE( IWR,'(A)' ) 'Power Function Empirical Diffusion Model'
          WRITE( IWR,'(A)' ) '  Model Parameters Entered on the Solute/P
     &orous Media Interaction Card'
        ELSEIF( INDEX(ADUM(1:),'conventional').NE.0 )  THEN
          IEDL(NSL) = 1
          WRITE( IWR,'(A)' ) 'Conventional Diffusion Model'
          VARB = 'Aqueous Molecular Diffusion Coefficient @ 20 C'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SMDL(NSL))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',SMDL(NSL)
          INDX = 0
          IUNM = 2
          IUNS = -1
          CALL RDUNIT(UNTS,SMDL(NSL),INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',SMDL(NSL),', m^2/s)'
        ELSEIF( INDEX(ADUM(1:),'constant').NE.0 )  THEN
          IEDL(NSL) = 3
          WRITE( IWR,'(A)' ) 'Constant Diffusion Model'
          VARB = 'Aqueous Molecular Diffusion Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SMDL(NSL))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',SMDL(NSL)
          INDX = 0
          IUNM = 2
          IUNS = -1
          CALL RDUNIT(UNTS,SMDL(NSL),INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',SMDL(NSL),', m^2/s)'
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Aqueous Diffusion Option: '//ADUM
          CALL WRMSGS( INDX )
        ENDIF
!
!---  Solid-Aqueous Partition option  ---
!
        VARB = 'Solid-Aqueous Partition Option: '
        ADUM = 'continuous'
        IDFLT = 1
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        WRITE( IWR,'(/,A,$)' ) VARB(1:IVR)
        IF( INDEX(ADUM(1:),'noncontinuous').NE.0 )  THEN
          IF( INDEX(ADUM(1:),'concentration dependent').NE.0 ) THEN
            IPCL(NSL) = 4
            WRITE( IWR,'(A)' ) 'Noncontinuous Solid Wetting / ' //
     &        'Concentration Dependent'
          ELSE
            IPCL(NSL) = 2
            WRITE( IWR,'(A)' ) 'Noncontinuous Solid Wetting'
          ENDIF
        ELSE
          IF( INDEX(ADUM(1:),'concentration dependent').NE.0 ) THEN
            IPCL(NSL) = 3
            WRITE( IWR,'(A)' ) 'Continuous Solid Wetting / Concentration
     &Dependent'
          ELSE
            IPCL(NSL) = 1
            WRITE( IWR,'(A)' ) 'Continuous Solid Wetting'
          ENDIF
        ENDIF

!
!---  Half-life  ---
!
          IDFLT = 1
          VARB = 'Radioactive Half-Life'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,HLF(NSL))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',HLF(NSL)
          INDX = 0
          IUNS = 1
          CALL RDUNIT(UNTS,HLF(NSL),INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',HLF(NSL),', s)'
          HLF(NSL) = MAX( HLF(NSL),SMALL )
!
!---  Cut-off concentration for the Courant number limiter  ---
!
          IF( ISLC(17).EQ.2 ) THEN
            IDFLT = 1
            VARB = 'Aqueous-Phase Cut-0ff Concentration'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,CCL_CRN(NSL))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',CCL_CRN(NSL)
            INDX = 0
            IUNM = -3
            CALL RDUNIT(UNTS,CCL_CRN(NSL),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',CCL_CRN(NSL),', 1/m^3)'
          ENDIF
  200 CONTINUE
!
!---  Electrolyte density option  ---
!
      IF( ISLC(16).EQ.1 ) THEN
        NSL_ELC = 0
        DO 210 MSL = 1,NSOLU
          IF( SOLUT(MSL).EQ.ELC_SOL ) NSL_ELC = MSL
  210   CONTINUE
        IF( NSL_ELC.EQ.0 ) THEN
          NCH = INDEX( ELC_SOL(1:),'  ' )-1
          INDX = 4
          CHMSG = 'Electrolyte Solute Not Listed' // ELC_SOL(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!
!---  Read number of lines of chain decay information  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Chain Decay Lines'
      CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
      IF( NLIN.GT.0 ) THEN
        WRITE(IWR,'(/,A)') 'Chain Decay Fractions:'
      ENDIF
      DO 400 NL = 1, NLIN
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        ADUM(1:) = ' '
        VARB = 'Parent Solute Name'
        NPSL = 0
        CALL RDCHR(ISTART,ICOMMA,NCHA,CHDUM,ADUM)
        DO 300 NSL = 1,NSOLU
          IF( SOLUT(NSL).EQ.ADUM ) NPSL = NSL
  300   CONTINUE
        BDUM(1:) = ' '
        VARB = 'Daughter Solute Name'
        NDSL = 0
        CALL RDCHR(ISTART,ICOMMA,NCHB,CHDUM,BDUM)
        DO 310 NSL = 1,NSOLU
          IF( SOLUT(NSL).EQ.BDUM ) NDSL = NSL
  310   CONTINUE
        IF( NPSL.EQ.0 .OR. NDSL.EQ.0 ) THEN
          INDX = 4
          CHMSG = 'Invalid Chain Decay: '//
     &      ADUM(1:NCHA)//': '//BDUM(1:NCHB)
          CALL WRMSGS( INDX )
        ELSEIF( NPSL.EQ.NDSL ) THEN
          INDX = 4
          CHMSG = 'Invalid Chain Decay (Parent = Progeny): '//
     &      ADUM(1:NCHA)//': '//BDUM(1:NCHB)
          CALL WRMSGS( INDX )
        ENDIF
        VARB = 'Chain Decay Fraction'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,CHDF(NPSL,NDSL))
        WRITE(IWR,'(2X,5A,1PE11.4)') 'From ',
     &    ADUM(1:NCHA),' to ',BDUM(1:NCHB),': ',CHDF(NPSL,NDSL)
  400 CONTINUE
      DO 420 NDSL = 1,NSOLU
        CHDFX = 0.D+0
        DO 410 NPSL = NDSL+1,NSOLU
          CHDFX = CHDFX + CHDF(NPSL,NDSL)
  410   CONTINUE
        IF( CHDFX.GE.1.D+0 ) THEN
          INDX = 4
          CHMSG = 'Chain Decay Fraction Summation'
          CALL WRMSGS( INDX )
        ENDIF
  420 CONTINUE

      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDTF1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDTP1
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
!     Water Mode
!
!     Reads the solute/porous media interaction card for the
!     dispersivities, half-lives, and partition coefficients.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
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
      CHARACTER*64 ADUM,BDUM,RDUM,UNTS
      CHARACTER*512 CHDUM
      INTEGER NCH
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDTP1'
!
!---  Write card information to output file  ---
!
      CARD = 'Solute/Porous Media Interaction Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE(IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
      IDISP = 0
!
!---  Loop over the rock/soil saturation information lines  ---
!
      N = 0
      IJK = 0
      ISGRP = 0
   10 CONTINUE
      IF( N.GE.NROCK .OR. IJK.GT.0 ) GOTO 600
      ISTART = 1
      VARB = 'Rock Name: '
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,RDUM)
!
!---  IJK, KIJ, or JKI indexing  ---
!
      IF( INDEX(RDUM(1:),'indexing').NE.0 ) THEN
        IF( INDEX(ROCK(1)(1:),'indexing').EQ.0 ) THEN
          INDX = 4
          CHMSG = 'Indexing Option Not Declared ' // 
     &      'in Rock/Soil Zonation Card'
          CALL WRMSGS( INDX )
        ENDIF
        IF( INDEX(RDUM,'ijk').NE.0 ) THEN
          IJK = 1
        ELSEIF( INDEX(RDUM,'jki').NE.0 ) THEN
          IJK = 2
        ELSEIF( INDEX(RDUM,'kij').NE.0 ) THEN
          IJK = 3
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Indexing Option' // RDUM(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
        GOTO 220
      ENDIF
!
!---  Search known rock types for a matching type ---
!
      DO 100 M = 1, NROCK
        IF( RDUM.EQ.ROCK(M)) THEN
          IROCK = M
          GOTO 200
        ENDIF
  100 CONTINUE
!
!---  Search known scaling groups for a matching type ---
!
      IF( ISLC(19).EQ.1 ) THEN
        DO 110 M = 1,NSCALE
           IF( RDUM.EQ.SCALNM(M) ) THEN
              ISGRP = M
              IROCK = 1
              GOTO 200
           ENDIF
  110   CONTINUE
        INDX = 2
        CHMSG = 'Unrecognized Rock/Soil Type or Scaling Group: '
     &    // RDUM(1:NCH)
        CALL WRMSGS( INDX )
        GOTO 10
      ENDIF
      INDX = 2
      CHMSG = 'Unrecognized Rock/Soil Type: ' // RDUM(1:NCH)
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
!---  Longitudinal dispersivity  ---
!
      VARB = 'Longitudinal Dispersivity: '
      IF( IJK.GT.0 ) THEN
        UNTS = 'm'
        IUNM = 1
        CALL RDIJK( ISTART,IJK,CHDUM,UNTS,DISPL )
        IDISP = 1
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,DISPL(IROCK))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',DISPL(IROCK)
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,DISPL(IROCK),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',DISPL(IROCK),', m)'
        IF( DISPL(IROCK).GE.SMALL ) IDISP = 1
      ENDIF
!
!---  Transverse dispersivity  ---
!
      VARB = 'Transverse Dispersivity: '
      IF( IJK.GT.0 ) THEN
        UNTS = 'm'
        IUNM = 1
        CALL RDIJK( ISTART,IJK,CHDUM,UNTS,DISPT )
        IDISP = 1
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,DISPT(IROCK))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',DISPT(IROCK)
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,DISPT(IROCK),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',DISPT(IROCK),', m)'
        IF( DISPT(IROCK).GE.SMALL ) IDISP = 1
      ENDIF
!
!---  Loop over number of solutes or radionuclides  ---
!
      DO 500 NS = 1,NSOLU
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Solute Name: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---    Search known solutes for matching name  ---
!
        DO 300 NSL = 1,NSOLU
          IF( ADUM.EQ.SOLUT(NSL)) GOTO 400
  300   CONTINUE
        INDX = 4
        CHMSG = 'Unrecognized Solute Name: '//ADUM
        CALL WRMSGS( INDX )
  400   CONTINUE
        WRITE(IWR,'(/,2A)') 'Solute Name:',SOLUT(NSL)
!
!---    Solid-aqueous partition coefficient  ---
!
        IDFLT = 1
        VARB = 'Solid-Aqueous Partition Coefficient: '
        IF( IJK.GT.0 ) THEN
          INDX = 1
          LNDX = 5
          UNTS = 'm^3/kg'
          IUNKG = -1
          IUNM = 3
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PCSL(1,1,NSL),INDX,LNDX )
          DO 402 IROCK = 1,NFLD
            PCSL(1,IROCK,NSL) = MAX( PCSL(1,IROCK,NSL),1.D-20 )
  402     CONTINUE
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,PCSL(1,IROCK,NSL))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4)') VARB(1:IVR),', ',UNTS (1:NCH),
     &      ': ',PCSL(1,IROCK,NSL)
          INDX = 0
          IUNKG = -1
          IUNM = 3
          CALL RDUNIT(UNTS,PCSL(1,IROCK,NSL),INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',
     &      PCSL(1,IROCK,NSL),', m^3/kg)'
          PCSL(1,IROCK,NSL) = MAX( PCSL(1,IROCK,NSL),1.D-20 )
        ENDIF
        IF( IPCL(NSL).EQ.3 .OR. IPCL(NSL).EQ.4 ) THEN
          VARB = 'Solute Name: '
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
          DO 410 NSS = 1,NSOLU
            IF( BDUM.EQ.SOLUT(NSS)) GOTO 420
  410     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Solute Name: '//ADUM
          CALL WRMSGS( INDX )
  420     CONTINUE
          WRITE(IWR,'(/,2A)') 'Dependent Solute Name:',SOLUT(NSS)
          IPCSL(IROCK,NSL) = NSS
          CALL RDDPR(ISTART,ICOMMA,CHDUM,PCSL(2,IROCK,NSL))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          VAR = 1.D+0
          INDX = 0
          IUNKG = -1
          IUNM = 3
          CALL RDUNIT(UNTS,VAR,INDX)
          PCSL(2,IROCK,NSL) = PCSL(2,IROCK,NSL) + LOG10(VAR)
          CALL RDDPR(ISTART,ICOMMA,CHDUM,PCSL(3,IROCK,NSL))
        ENDIF
!
!---    van Schaik and Kemper Empirical Aqueous Diffusion Model  ---
!
        IF( IEDL(NSL).EQ.2 ) THEN
          VARB = 'Aqueous Molecular Diffusion Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SDCL(1,IROCK,NSL))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',SDCL(1,IROCK,NSL)
          INDX = 0
          IUNM = 2
          IUNS = -1
          CALL RDUNIT(UNTS,SDCL(1,IROCK,NSL),INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',
     &      SDCL(1,IROCK,NSL),', m^2/s)'
          VARB = 'a Constant'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SDCL(2,IROCK,NSL))
          WRITE(IWR,'(2A,1PE11.4)') VARB(1:IVR),': ',SDCL(2,IROCK,NSL)
          VARB = 'b Constant'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SDCL(3,IROCK,NSL))
          WRITE(IWR,'(2A,1PE11.4)') VARB(1:IVR),': ',SDCL(3,IROCK,NSL)
        ENDIF
C
C---  Power Function Empirical Aqueous Diffusion Model  ---
C
          IF( IEDL(NSL).EQ.4 ) THEN
            VARB = 'Aqueous Molecular Diffusion Coefficient'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,SDCL(1,IROCK,NSL))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',SDCL(1,IROCK,NSL)
            INDX = 0
            IUNM = 2
            IUNS = -1
            CALL RDUNIT(UNTS,SDCL(1,IROCK,NSL),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',
     &        SDCL(1,IROCK,NSL),', m^2/s)'
            VARB = 'a Constant'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,SDCL(2,IROCK,NSL))
            WRITE(IWR,'(2A,1PE11.4)') VARB(1:IVR),': ',SDCL(2,IROCK,NSL)
            VARB = 'b Constant'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,SDCL(3,IROCK,NSL))
            WRITE(IWR,'(2A,1PE11.4)') VARB(1:IVR),': ',SDCL(3,IROCK,NSL)
          ENDIF
!
!---    Macrodispersivity enhancement factor  ---
!
        CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
        IF( INDX.EQ.1 ) THEN
          VARB = 'Macrodispersivity Enhancement Factor'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,SMDEF(IROCK,NSL))
          WRITE(IWR,'(2A,1PE11.4)') VARB(1:IVR),': ',SMDEF(IROCK,NSL)
        ENDIF
  500 CONTINUE
!
!---  Read next rock/soil type or scaling group  ---
!
      IF( N.LT.NROCK ) WRITE(IWR,'(/)')
      GOTO 10
  600 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDTP1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RSDL1
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
!     Water Mode
!
!     Compute the maximum relative residuals
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.




!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOURC
      USE SOLTN
      USE PORMED
      USE OUTPU
      USE JACOB
      USE HYST
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
!----------------------Executable Lines--------------------------------!
!
      IF( ICNV.EQ.1 .OR. ICNV.EQ.4 ) RETURN
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RSDL1'
!$OMP PARALLEL DO
!$OMP& DEFAULT(NONE)
!$OMP& SHARED(ISVC,RSD,NSD)
      DO 100 M = 1,ISVC
        RSD(M) = 0.D+0
        NSD(M) = 0
  100 CONTINUE
!$OMP END PARALLEL DO
!
!---  Loop over all nodes
!
!$OMP PARALLEL DO
!$OMP&  COPYIN(ISUB_LOG)
!$OMP&  DEFAULT(NONE)
!$OMP&  SHARED(NFLD,NMD,IXP,IZ,IM,IEQW,ISKP,NPHAZ,PORD,RHOL,SL,DTI,
!$OMP&    VOL,BLU,PL,PATM,RSDL,RSD,NSD)
!$OMP&  PRIVATE(IZN,MPL,ACP,RSDX)
      DO 200 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 200
        IZN = IZ(N)
        NMD = IXP(N)
        MPL = IM(IEQW,NMD)
        IF( ISKP(IZN).EQ.1 ) GOTO 200
!
!---  Water saturated system prior to iteration  ---
!
        IF( NPHAZ(2,N).EQ.1 ) THEN
          ACP = PORD(2,N)*RHOL(2,N)*SL(2,N)*DTI*VOL(N)
          RSDX = MIN( ABS(BLU(MPL))/(ABS(PL(2,N))+PATM),
     &      ABS(RSDL(IEQW,N)/ACP) )
          IF( RSDX.GT.RSD(IEQW) ) THEN
            RSD(IEQW) = RSDX
            NSD(IEQW) = N
          ENDIF
!
!---  Water-gas system prior to iteration  ---
!
        ELSEIF( NPHAZ(2,N).EQ.2 ) THEN
          ACP = PORD(2,N)*RHOL(2,N)*SL(2,N)*DTI*VOL(N)
          RSDX = MIN( ABS(BLU(MPL))/(ABS(PL(2,N))+PATM),
     &      ABS(RSDL(IEQW,N)/ACP) )
          IF( RSDX.GT.RSD(IEQW) ) THEN
            RSD(IEQW) = RSDX
            NSD(IEQW) = N
          ENDIF
        ENDIF
  200 CONTINUE
!$OMP END PARALLEL DO
!
!---  Assign a convergence index  ---
!
      DO 300 M = 1,ISVC
        IF( RSD(M).GT.RSDMX ) ICNV = 2
  300 CONTINUE
      IF( ICNV.EQ.2 .AND. NITER.GE.NRIMX ) ICNV = 1
!
!---  Unconverged solution Newton-Raphson iteration limit exceeded  ---
!
      IF( ICNV.EQ.1 ) THEN
        WRITE(ISC,'(10X,A)') '---  Convergence Failure  ---'
        WRITE(IWR,'(10X,A)') '---  Convergence Failure  ---'
        WRITE(ISC,'(4X,A,1PE11.4,A,I6)') 'Water Equation Maximum Residua
     &l = ',RSD(IEQW),' Node = ',NSD(IEQW)
        WRITE(IWR,'(4X,A,1PE11.4,A,I6)') 'Water Equation Maximum Residua
     &l = ',RSD(IEQW),' Node = ',NSD(IEQW)
!
!---  Reduce time step  ---
!



        IF( NTSR.LT.4 .OR. (DTCF*DT).GT.DTMN ) THEN

          NTSR = NTSR + 1
          DTX = DT
          TM = TM - (1.D+0-DTCF)*DT
          DT = DTCF*DT
          DTO = DT
          DTI = 1.D+0/DT
          VAR = DT
          VARX = DTX
          IF( UNTM.NE.'null' ) THEN
            INDX = 1
            IUNS = 1
            CALL RDUNIT(UNTM,VAR,INDX)
            INDX = 1
            IUNS = 1
            CALL RDUNIT(UNTM,VARX,INDX)
            NCH = INDEX( UNTM,'  ')-1
          ENDIF
          WRITE(ISC,'(4X,A,1PE11.4,1X,2A,1PE11.4,1X,A)')
     &      'Time Step Reduced From ',VARX,UNTM(1:NCH),' to ',
     &      VAR,UNTM(1:NCH)
          WRITE(IWR,'(4X,A,1PE11.4,1X,2A,1PE11.4,1X,A)')
     &      'Time Step Reduced From ',VARX,UNTM(1:NCH),' to ',
     &      VAR,UNTM(1:NCH)
          DO 400 N = 1,NFLD
            PL(2,N) = PL(1,N)
            IXP(N) = ABS(IXP(N))
            NPHAZ(2,N) = NPHAZ(1,N)
            IPH(2,N) = IPH(1,N)
  400     CONTINUE





!
!---  Number of time step reductions failure: stop simulation  ---
!
        ELSE
          WRITE(ISC,'(10X,A)') '---  Time Step Reduction Limit Exceeded
     & ---'
          WRITE(IWR,'(10X,A)') '---  Time Step Reduction Limit Exceeded
     & ---'
          ICNV = 4






        ENDIF
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RSDL1 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RKL1( HDGL,RKLX,SLP,SLPF,SLPM,SLX,IZN,IPHX,M )
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
!     Water Mode
!
!     Compute the aqueous relative permeability from the
!     aqueous saturation.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!     Last Modified by MD White, PNNL, 16 December 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
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
      REAL*8 RKLX(3)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RKL1'
!
!---  Constant relative permeability function  ---
!
      IF( MOD( IRPL(IZN),100 ).EQ.0 ) THEN
!
!---    Single-pressure dual-porosity saturation function  ---
!
        IF( ISCHR(IZN).EQ.3 .OR. ISCHR(IZN).EQ.4 ) THEN
          RKLM = RPLC(2,IZN)
          RKLF = RPLC(1,IZN)
          RKLX(1) = ( PERM(4,IZN)*RKLM*(1.D+0-POR(4,IZN)) +
     &      PERM(7,IZN)*RKLF*POR(4,IZN) )/
     &      ( PERM(4,IZN)*(1.D+0-POR(4,IZN)) + PERM(7,IZN)*POR(4,IZN)
     &      + SMALL )
          RKLX(2) = ( PERM(5,IZN)*RKLM*(1.D+0-POR(4,IZN)) +
     &      PERM(8,IZN)*RKLF*POR(4,IZN) )/
     &      ( PERM(5,IZN)*(1.D+0-POR(4,IZN)) + PERM(8,IZN)*POR(4,IZN)
     &      + SMALL )
          RKLX(3) = ( PERM(6,IZN)*RKLM*(1.D+0-POR(4,IZN)) +
     &      PERM(9,IZN)*RKLF*POR(4,IZN) )/
     &      ( PERM(6,IZN)*(1.D+0-POR(4,IZN)) + PERM(9,IZN)*POR(4,IZN)
     &      + SMALL )
!
!---    Triple-curve saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.301 .OR. ISCHR(IZN).EQ.302 ) THEN
          IF( IPHX.EQ.2 ) THEN
            RKLX(1) = 0.D+0
            RKLX(2) = 0.D+0
            RKLX(3) = 0.D+0
            IF( SLX.GE.SCHR(4,IZN) ) RKLX(1) = RPLC(1,IZN)
            IF( SLX.GE.SCHR(4,IZN) ) RKLX(2) = RPLC(1,IZN)
            IF( SLX.GE.SCHR(4,IZN) ) RKLX(3) = RPLC(1,IZN)
          ELSE
            RKLX(1) = RPLC(2,IZN)
            RKLX(2) = RPLC(2,IZN)
            RKLX(3) = RPLC(2,IZN)
          ENDIF
!
!---    Other saturation functions  ---
!
        ELSE
!
!---      Two-pressure dual-porosity  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            RKLX(1) = RPLC(1,IZN)
            RKLX(2) = RPLC(1,IZN)
            RKLX(3) = RPLC(1,IZN)
          ELSE
            RKLX(1) = RPLC(2,IZN)
            RKLX(2) = RPLC(2,IZN)
            RKLX(3) = RPLC(2,IZN)
          ENDIF
        ENDIF
!
!---  Mualem-irreducible porosity distribution function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.21 ) THEN
        SLPX = (SLP-SLP*SCHR(4,IZN)+SCHR(4,IZN)-RPLC(1,IZN))/
     &    (1.D+0-RPLC(1,IZN))
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHR(IZN).EQ.1  ) THEN
          RKLX(1) = SQRT(SLPX)*((1.D+0-(1.D+0-SLPX**(1.D+0/RPLC(2,IZN)))
     &      **RPLC(2,IZN))**2)
          RKLX(2) = RKLX(1)
          RKLX(3) = RKLX(1)
!
!---    Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.2 ) THEN
          RKLX(1) = SLPX**(2.5D+0 + 2.0D+0/RPLC(2,IZN))
          RKLX(2) = RKLX(1)
          RKLX(3) = RKLX(1)
        ENDIF
!
!---  Mualem-Anisotropy reference porosity distribution function  ---
!
      ELSEIF( IRPL(IZN).EQ.301 ) THEN
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHR(IZN).EQ.1 .OR. ISCHR(IZN).EQ.101 ) THEN
          RKLX(1) = (SLP**RPLC(3,IZN))*((1.D+0-(1.D+0-SLP**
     &      (1.D+0/RPLC(2,IZN)))**RPLC(2,IZN))**2)
          RKLX(2) = RKLX(1)
          RKLX(3) = (SLP**RPLC(4,IZN))*((1.D+0-(1.D+0-SLP**
     &      (1.D+0/RPLC(2,IZN)))**RPLC(2,IZN))**2)
!
!---    Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.2 .OR. ISCHR(IZN).EQ.102 ) THEN
          RKLX(1) = (SLP**RPLC(3,IZN))*SLP**(2.D+0 + 2.0D+0/RPLC(2,IZN))
          RKLX(2) = RKLX(1)
          RKLX(3) = (SLP**RPLC(4,IZN))*SLP**(2.D+0 + 2.0D+0/RPLC(2,IZN))
        ENDIF
!
!---  Mualem porosity distribution function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.1 ) THEN
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHR(IZN).EQ.1 .OR. ISCHR(IZN).EQ.101 ) THEN
!
!---    Two-pressure dual-porosity  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            RKLX(1) = SQRT(SLP)*((1.D+0-(1.D+0-SLP**(1.D+0/RPLC(1,IZN)))
     &        **RPLC(1,IZN))**2)
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ELSE
            RKLX(1) = SQRT(SLP)*((1.D+0-(1.D+0-SLP**(1.D+0/RPLC(2,IZN)))
     &        **RPLC(2,IZN))**2)
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ENDIF
!
!---    Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.2 .OR. ISCHR(IZN).EQ.102 ) THEN
!
!---    Two-pressure dual-porosity  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            RKLX(1) = SLP**(2.5D+0 + 2.0D+0/RPLC(1,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ELSE
            RKLX(1) = SLP**(2.5D+0 + 2.0D+0/RPLC(2,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ENDIF
!
!---    van Genuchten triple-curve saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.301) THEN
          IF( IPHX.EQ.2 ) THEN
            RKLX(1) = 0.D+0
            IF( SLX.GT.SCHR(4,IZN) ) RKLX(1) = SQRT(SLP)*
     &        ((1.D+0-(1.D+0-SLP**(1.D+0/RPLC(1,IZN)))**RPLC(1,IZN))**2)
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ELSE
            RKLX(1) = SQRT(SLP)*((1.D+0-(1.D+0-SLP**(1.D+0/RPLC(2,IZN)))
     &        **RPLC(2,IZN))**2)
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ENDIF
!
!---    Brooks-Corey triple-curve saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.302 ) THEN
          IF( IPHX.EQ.2 ) THEN
            RKLX(1) = 0.D+0
            IF( SLX.GT.SCHR(4,IZN) )
     &        RKLX(1) = SLP**(2.5D+0 + 2.0D+0/RPLC(1,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ELSE
            RKLX(1) = SLP**(2.5D+0 + 2.0D+0/RPLC(2,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ENDIF
!
!---    Single-pressure dual-porosity van Genuchten  ---
!
        ELSEIF( ISCHR(IZN).EQ.3  ) THEN
          RKLM = 1.D+0 - (SLPM**(1.D+0/RPLC(2,IZN)))
          RKLM = SQRT(SLPM)*((1.D+0-(RKLM**RPLC(2,IZN)))**2)
          RKLF = 1.D+0 - (SLPF**(1.D+0/RPLC(1,IZN)))
          RKLF = SQRT(SLPF)*((1.D+0-(RKLF**RPLC(1,IZN)))**2)
          RKLX(1) = ( PERM(4,IZN)*RKLM*(1.D+0-POR(4,IZN)) +
     &      PERM(7,IZN)*RKLF*POR(4,IZN) )/
     &      ( PERM(4,IZN)*(1.D+0-POR(4,IZN)) + PERM(7,IZN)*POR(4,IZN)
     &      + SMALL )
          RKLX(2) = ( PERM(5,IZN)*RKLM*(1.D+0-POR(4,IZN)) +
     &      PERM(8,IZN)*RKLF*POR(4,IZN) )/
     &      ( PERM(5,IZN)*(1.D+0-POR(4,IZN)) + PERM(8,IZN)*POR(4,IZN)
     &      + SMALL )
          RKLX(3) = ( PERM(6,IZN)*RKLM*(1.D+0-POR(4,IZN)) +
     &      PERM(9,IZN)*RKLF*POR(4,IZN) )/
     &      ( PERM(6,IZN)*(1.D+0-POR(4,IZN)) + PERM(9,IZN)*POR(4,IZN)
     &      + SMALL )
!
!---    Single-pressure dual-porosity Brooks and Corey  ---
!
        ELSEIF( ISCHR(IZN).EQ.4  ) THEN
          RKLM = SLPM**(2.5D+0 + 2.0D+0/RPLC(2,IZN))
          RKLF = SLPF**(2.5D+0 + 2.0D+0/RPLC(1,IZN))
          RKLX(1) = ( PERM(4,IZN)*RKLM*(1.D+0-POR(4,IZN)) +
     &      PERM(7,IZN)*RKLF*POR(4,IZN) )/
     &      ( PERM(4,IZN)*(1.D+0-POR(4,IZN)) + PERM(7,IZN)*POR(4,IZN)
     &      + SMALL )
          RKLX(2) = ( PERM(5,IZN)*RKLM*(1.D+0-POR(4,IZN)) +
     &      PERM(8,IZN)*RKLF*POR(4,IZN) )/
     &      ( PERM(5,IZN)*(1.D+0-POR(4,IZN)) + PERM(8,IZN)*POR(4,IZN)
     &      + SMALL )
          RKLX(3) = ( PERM(6,IZN)*RKLM*(1.D+0-POR(4,IZN)) +
     &      PERM(9,IZN)*RKLF*POR(4,IZN) )/
     &      ( PERM(6,IZN)*(1.D+0-POR(4,IZN)) + PERM(9,IZN)*POR(4,IZN)
     &      + SMALL )
        ENDIF
!
!---  Burdine porosity distribution function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHR(IZN).EQ.1 .OR. ISCHR(IZN).EQ.101 ) THEN
!
!---      Two-pressure dual-porosity  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            RKLX(1) = (SLP**2)*(1.D+0-(1.D+0-SLP**(1.D+0/RPLC(1,IZN)))
     &        **RPLC(1,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ELSE
            RKLX(1) = (SLP**2)*(1.D+0-(1.D+0-SLP**(1.D+0/RPLC(2,IZN)))
     &        **RPLC(2,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ENDIF
!
!---    Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.2 .OR. ISCHR(IZN).EQ.102 ) THEN
!
!---      Two-pressure dual-porosity  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            RKLX(1) = SLP**(3.0D+0 + 2.0D+0/RPLC(1,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ELSE
            RKLX(1) = SLP**(3.0D+0 + 2.0D+0/RPLC(2,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ENDIF
!
!---    Triple-curve van Genuchten saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.301 ) THEN
          IF( IPHX.EQ.2 ) THEN
            RKLX(1) = 0.D+0
            IF( SLX.GE.SCHR(4,IZN) ) RKLX(1) = (SLP**2)*(1.D+0-
     &        (1.D+0-SLP**(1.D+0/RPLC(1,IZN)))**RPLC(1,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ELSE
            RKLX(1) = (SLP**2)*(1.D+0-(1.D+0-SLP**(1.D+0/RPLC(2,IZN)))
     &        **RPLC(2,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ENDIF
!
!---    Triple-curve Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.302 ) THEN
          IF( IPHX.EQ.2 ) THEN
            RKLX(1) = 0.D+0
            IF( SLX.GE.SCHR(4,IZN) )
     &        RKLX(1) = SLP**(3.0D+0 + 2.0D+0/RPLC(1,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ELSE
            RKLX(1) = SLP**(3.0D+0 + 2.0D+0/RPLC(2,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ENDIF
!
!---    Single-pressure dual-porosity van Genuchten  ---
!
        ELSEIF( ISCHR(IZN).EQ.3 ) THEN
          RKLM = (SLPM**2)*(1.D+0-(1.D+0-SLPM**(1.D+0/RPLC(2,IZN)))
     &      **RPLC(2,IZN))
          RKLF = (SLPF**2)*(1.D+0-(1.D+0-SLPF**(1.D+0/RPLC(1,IZN)))
     &      **RPLC(1,IZN))
          RKLX(1) = ( PERM(4,IZN)*RKLM*(1.D+0-POR(4,IZN)) +
     &      PERM(7,IZN)*RKLF*POR(4,IZN) )/
     &      ( PERM(4,IZN)*(1.D+0-POR(4,IZN)) + PERM(7,IZN)*POR(4,IZN)
     &      + SMALL )
          RKLX(2) = ( PERM(5,IZN)*RKLM*(1.D+0-POR(4,IZN)) +
     &      PERM(8,IZN)*RKLF*POR(4,IZN) )/
     &      ( PERM(5,IZN)*(1.D+0-POR(4,IZN)) + PERM(8,IZN)*POR(4,IZN)
     &      + SMALL )
          RKLX(3) = ( PERM(6,IZN)*RKLM*(1.D+0-POR(4,IZN)) +
     &      PERM(9,IZN)*RKLF*POR(4,IZN) )/
     &      ( PERM(6,IZN)*(1.D+0-POR(4,IZN)) + PERM(9,IZN)*POR(4,IZN)
     &      + SMALL )
!
!---    Single-pressure dual-porosity Brooks and Corey  ---
!
        ELSEIF( ISCHR(IZN).EQ.4 ) THEN
          RKLM = SLPM**(3.0D+0 + 2.0D+0/RPLC(2,IZN))
          RKLF = SLPF**(3.0D+0 + 2.0D+0/RPLC(1,IZN))
          RKLX(1) = ( PERM(4,IZN)*RKLM*(1.D+0-POR(4,IZN)) +
     &      PERM(7,IZN)*RKLF*POR(4,IZN) )/
     &      ( PERM(4,IZN)*(1.D+0-POR(4,IZN)) + PERM(7,IZN)*POR(4,IZN)
     &      + SMALL )
          RKLX(2) = ( PERM(5,IZN)*RKLM*(1.D+0-POR(4,IZN)) +
     &      PERM(8,IZN)*RKLF*POR(4,IZN) )/
     &      ( PERM(5,IZN)*(1.D+0-POR(4,IZN)) + PERM(8,IZN)*POR(4,IZN)
     &      + SMALL )
          RKLX(3) = ( PERM(6,IZN)*RKLM*(1.D+0-POR(4,IZN)) +
     &      PERM(9,IZN)*RKLF*POR(4,IZN) )/
     &      ( PERM(6,IZN)*(1.D+0-POR(4,IZN)) + PERM(9,IZN)*POR(4,IZN)
     &      + SMALL )
        ENDIF
!
!---  Corey relative permeability function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.3 ) THEN
        RKLX(1) = SLP**4
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Fatt and Klikoff relative permeability function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.4 ) THEN
        RKLX(1) = SLP**3
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Haverkamp relative permeability function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.5 ) THEN
        IF( HDGL.LE.RPLC(3,IZN) ) THEN
          RKLX(1) = 1.D+0
        ELSE
          ALPHAX = RPLC(1,IZN)/RPLC(4,IZN)
          HDGLX = HDGL/RPLC(4,IZN)
          RKLX(1) = ALPHAX/(ALPHAX +
     &      (HDGLX**RPLC(2,IZN)))
        ENDIF
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Touma and Vauclin relative permeability function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.6 ) THEN
        RKLX(1) = RPLC(1,IZN)*(SLP**RPLC(2,IZN))
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Free Corey relative permeability function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.7 ) THEN
        SLRX = RPLC(3,IZN)
        SGRX = RPLC(4,IZN)
        SLPX = MIN( MAX( (SLX-SLRX)/(1.D+0-SLRX-SGRX),0.D+0 ),1.D+0 )
        RKLX(1) = RPLC(1,IZN)*(SLPX**(RPLC(2,IZN)))
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Rijtema-Gardner modified exponential function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.9 ) THEN
        RKLX(1) = EXP( RPLC(1,IZN)*HDGL + RPLC(2,IZN) )
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Tabular function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.10 ) THEN
        ITBX = 0
        IF( M.NE.2 ) ITBX = 1
        RKLX(1) = FNTBLY( SLX,IRLTBL(1,IZN),IRLTBL(2,IZN),ITBX )
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Cubic-spline tabular function versus saturation  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.11 ) THEN
        ITX = 1
        ITBX = 0
        RKLX(1) = FSPLNY( SLX,IRLTBLT(1,IZN,ITX),IRLTBLT(2,IZN,ITX) )
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Linear tabular function versus capillary head  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.12 ) THEN
        ITX = 1
        ITBX = 0
        RKLX(1) = FNTBLY( HDGL,IRLTBLT(1,IZN,ITX),IRLTBLT(2,IZN,ITX),
     &     ITBX )
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Cubic-spline tabular function versus capillary head  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.13 ) THEN
        ITX = 1
        ITBX = 0
        RKLX(1) = FSPLNY( HDGL,IRLTBLT(1,IZN,ITX),IRLTBLT(2,IZN,ITX) )
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Linear tabular function versus log capillary head  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.14 ) THEN
        ITX = 1
        ITBX = 0
        HDGLX = LOG(HDGL)
        RKLX(1) = FNTBLY( HDGLX,IRLTBLT(1,IZN,ITX),IRLTBLT(2,IZN,ITX),
     &    ITBX )
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Cubic-spline tabular function versus log capillary head  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.15 ) THEN
        ITX = 1
        ITBX = 0
        HDGLX = LOG(HDGL)
        RKLX(1) = FSPLNY( HDGLX,IRLTBLT(1,IZN,ITX),IRLTBLT(2,IZN,ITX) )
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Polynomial function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.19 ) THEN
!
!---    Convert head units for polynomial function basis  ---
!
        HDGLU = HDGL/RPLC(1,IZN)
        IF( HDGLU.LT.CPLY_RL(1,1,IZN) ) THEN
          RKLX(1) = 1.D+0
        ELSEIF( HDGLU.GE.CPLY_RL(2,NPLY_RL(IZN),IZN) ) THEN
          RKLX(1) = 0.D+0
        ELSE
          DO 1192 NP = 1,NPLY_RL(IZN)
            IF( HDGLU.GE.CPLY_RL(1,NP,IZN) .AND.
     &        HDGLU.LT.CPLY_RL(2,NP,IZN) ) THEN
                RKLX(1) = 0.D+0
                NPOLYC = LPOLYC
                DO 1190 NC = 5,NPOLYC
                  RKLX(1) = RKLX(1) + CPLY_RL(NC,NP,IZN)*
     &              (LOG10(HDGLU)**(NC-5))
 1190           CONTINUE
                GOTO 1194
            ENDIF
 1192     CONTINUE
 1194     CONTINUE
!
!---  Normalize absolute conductivity with saturated
!     conductivity  ---
!
          RKLX(1) = (1.D+1**RKLX(1))/RPLC(2,IZN)
        ENDIF
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Modified-Mualem porosity distribution function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.22 ) THEN
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHR(IZN).EQ.1 .OR. ISCHR(IZN).EQ.101 ) THEN
          RKLX(1) = (SLP**RPLC(1,IZN))*((1.D+0-(1.D+0-SLP**
     &      (1.D+0/RPLC(2,IZN)))**RPLC(2,IZN))**2)
          RKLX(2) = RKLX(1)
          RKLX(3) = RKLX(1)
!
!---    Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.2 .OR. ISCHR(IZN).EQ.102 ) THEN
          RKLX(1) = (SLP**RPLC(1,IZN))*SLP**(2.D+0 + 2.0D+0/RPLC(2,IZN))
          RKLX(2) = RKLX(1)
          RKLX(3) = RKLX(1)
        ENDIF
      ENDIF
!
!---  Polmann anisotropy permeability function  ---
!
      IF( IRPL(IZN).GE.100 .AND. IRPL(IZN).LT.200 ) THEN
        PSI = HDGL*1.D+2
        SKLX = RPLC(5,IZN) - RPLC(10,IZN)*PSI -
     &    RPLC(6,IZN)*RPLC(9,IZN)*( RPLC(7,IZN) -
     &    (RPLC(7,IZN)**2)*PSI - (RPLC(8,IZN)**2)*PSI)/
     &    (1.D+0 + RPLC(10,IZN)*RPLC(9,IZN))
        SIGMA = RPLC(6,IZN)*(((1.D+0 - RPLC(7,IZN)*PSI)**2) +
     &    (RPLC(8,IZN)**2)*(PSI**2))/
     &    (1.D+0 + RPLC(10,IZN)*RPLC(9,IZN))
        SKHX = EXP( SKLX + 5.D-1*SIGMA )
        SKVX = EXP( SKLX - 5.D-1*SIGMA )
        ANISOX = MIN( MAX( SKHX/SKVX,0.D+0 ),RPLC(11,IZN) )
        ANISOX = MAX( ANISOX,RPLC(12,IZN) )
        RKLX(1) = RKLX(3)*ANISOX
        RKLX(2) = RKLX(3)*ANISOX
      ENDIF
      RKLX(1) = MAX( RKLX(1),ZERO )
      RKLX(2) = MAX( RKLX(2),ZERO )
      RKLX(3) = MAX( RKLX(3),ZERO )
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RKL1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RKLT1( HDGL,RKLX,SLP,SLPF,SLPM,SLX,IZN,ITX,IPHX,M )
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
!     Water Mode
!
!     Compute the aqueous relative permeability tensor components
!     from the aqueous saturation.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!     Last Modified by MD White, PNNL, 16 December 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
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
      SUB_LOG(ISUB_LOG) = '/RKLT1'
!
!---  Constant relative permeability function  ---
!
      IF( MOD( IRPLT(ITX,IZN),100 ).EQ.0 ) THEN
!
!---    Single-pressure dual-porosity saturation function  ---
!
        IF( ISCHR(IZN).EQ.3 .OR. ISCHR(IZN).EQ.4 ) THEN
          RKLM = RPLT(ITX,2,IZN)
          RKLF = RPLT(ITX,1,IZN)
          IMX = ITX + 3
          IFX = ITX + 6
          RKLX = ( PERM(IMX,IZN)*RKLM*(1.D+0-POR(4,IZN)) +
     &      PERM(IFX,IZN)*RKLF*POR(4,IZN) )/
     &      ( PERM(IMX,IZN)*(1.D+0-POR(4,IZN)) +
     &      PERM(IFX,IZN)*POR(4,IZN) + SMALL )
!
!---    Triple-curve saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.301 .OR. ISCHR(IZN).EQ.302 ) THEN
          IF( IPHX.EQ.2 ) THEN
            RKLX = 0.D+0
            IF( SLX.GE.SCHR(4,IZN) ) RKLX = RPLT(ITX,1,IZN)
          ELSE
            RKLX = RPLT(ITX,2,IZN)
          ENDIF
!
!---    Other saturation functions  ---
!
        ELSE
!
!---      Two-pressure dual-porosity  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            RKLX = RPLT(ITX,1,IZN)
          ELSE
            RKLX = RPLT(ITX,2,IZN)
          ENDIF
        ENDIF
!
!---  Mualem-irreducible porosity distribution function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.21 ) THEN
        SLPX = (SLP-SLP*SCHR(4,IZN)+SCHR(4,IZN)-RPLT(ITX,1,IZN))/
     &    (1.D+0-RPLT(ITX,1,IZN))
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHR(IZN).EQ.1  ) THEN
          RKLX = SQRT(SLPX)*((1.D+0-(1.D+0-SLPX**
     &      (1.D+0/RPLT(ITX,2,IZN)))**RPLT(ITX,2,IZN))**2)
!
!---    Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.2 ) THEN
          RKLX = SLPX**(2.5D+0 + 2.0D+0/RPLT(ITX,2,IZN))
        ENDIF
!
!---  Mualem porosity distribution function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.1 ) THEN
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHR(IZN).EQ.1 .OR. ISCHR(IZN).EQ.101 ) THEN
!
!---    Two-pressure dual-porosity  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            RKLX = SQRT(SLP)*((1.D+0-(1.D+0-SLP**
     &        (1.D+0/RPLT(ITX,1,IZN)))**RPLT(ITX,1,IZN))**2)
          ELSE
            RKLX = SQRT(SLP)*((1.D+0-(1.D+0-SLP**
     &        (1.D+0/RPLT(ITX,2,IZN)))**RPLT(ITX,2,IZN))**2)
          ENDIF
!
!---    Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.2 .OR. ISCHR(IZN).EQ.102 ) THEN
!
!---    Two-pressure dual-porosity  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            RKLX = SLP**(2.5D+0 + 2.0D+0/RPLT(ITX,1,IZN))
          ELSE
            RKLX = SLP**(2.5D+0 + 2.0D+0/RPLT(ITX,2,IZN))
          ENDIF
!
!---    van Genuchten triple-curve saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.301) THEN
          IF( IPHX.EQ.2 ) THEN
            RKLX = 0.D+0
            IF( SLX.GT.SCHR(4,IZN) ) RKLX = SQRT(SLP)*
     &        ((1.D+0-(1.D+0-SLP**(1.D+0/RPLT(ITX,1,IZN)))**
     &        RPLT(ITX,1,IZN))**2)
          ELSE
            RKLX = SQRT(SLP)*((1.D+0-(1.D+0-SLP**
     &        (1.D+0/RPLT(ITX,2,IZN)))**RPLT(ITX,2,IZN))**2)
          ENDIF
!
!---    Brooks-Corey triple-curve saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.302 ) THEN
          IF( IPHX.EQ.2 ) THEN
            RKLX = 0.D+0
            IF( SLX.GT.SCHR(4,IZN) )
     &        RKLX = SLP**(2.5D+0 + 2.0D+0/RPLT(ITX,1,IZN))
          ELSE
            RKLX = SLP**(2.5D+0 + 2.0D+0/RPLT(ITX,2,IZN))
          ENDIF
!
!---    Single-pressure dual-porosity van Genuchten  ---
!
        ELSEIF( ISCHR(IZN).EQ.3  ) THEN
          RKLM = 1.D+0 - (SLPM**(1.D+0/RPLT(ITX,2,IZN)))
          RKLM = SQRT(SLPM)*((1.D+0-(RKLM**RPLT(ITX,2,IZN)))**2)
          RKLF = 1.D+0 - (SLPF**(1.D+0/RPLT(ITX,1,IZN)))
          RKLF = SQRT(SLPF)*((1.D+0-(RKLF**RPLT(ITX,1,IZN)))**2)
          IMX = ITX + 3
          IFX = ITX + 6
          RKLX = ( PERM(ITX,IZN)*RKLM*(1.D+0-POR(4,IZN)) +
     &      PERM(IFX,IZN)*RKLF*POR(4,IZN) )/
     &      ( PERM(ITX,IZN)*(1.D+0-POR(4,IZN))
     &      + PERM(IFX,IZN)*POR(4,IZN)+ SMALL )
!
!---    Single-pressure dual-porosity Brooks and Corey  ---
!
        ELSEIF( ISCHR(IZN).EQ.4  ) THEN
          RKLM = SLPM**(2.5D+0 + 2.0D+0/RPLT(ITX,2,IZN))
          RKLF = SLPF**(2.5D+0 + 2.0D+0/RPLT(ITX,1,IZN))
          IMX = ITX + 3
          IFX = ITX + 6
          RKLX = ( PERM(IMX,IZN)*RKLM*(1.D+0-POR(4,IZN)) +
     &      PERM(ITX,IZN)*RKLF*POR(4,IZN) )/
     &      ( PERM(IMX,IZN)*(1.D+0-POR(4,IZN))
     &      + PERM(ITX,IZN)*POR(4,IZN) + SMALL )
        ENDIF
!
!---  Burdine porosity distribution function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.2 ) THEN
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHR(IZN).EQ.1 .OR. ISCHR(IZN).EQ.101 ) THEN
!
!---      Two-pressure dual-porosity  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            RKLX = (SLP**2)*(1.D+0-(1.D+0-SLP**(1.D+0/RPLT(ITX,1,IZN)))
     &        **RPLT(ITX,1,IZN))
          ELSE
            RKLX = (SLP**2)*(1.D+0-(1.D+0-SLP**(1.D+0/RPLT(ITX,2,IZN)))
     &        **RPLT(ITX,2,IZN))
          ENDIF
!
!---    Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.2 .OR. ISCHR(IZN).EQ.102 ) THEN
!
!---      Two-pressure dual-porosity  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            RKLX = SLP**(3.0D+0 + 2.0D+0/RPLT(ITX,1,IZN))
          ELSE
            RKLX = SLP**(3.0D+0 + 2.0D+0/RPLT(ITX,2,IZN))
          ENDIF
!
!---    Triple-curve van Genuchten saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.301 ) THEN
          IF( IPHX.EQ.2 ) THEN
            RKLX = 0.D+0
            IF( SLX.GE.SCHR(4,IZN) ) RKLX = (SLP**2)*(1.D+0-
     &        (1.D+0-SLP**(1.D+0/RPLT(ITX,1,IZN)))**RPLT(ITX,1,IZN))
          ELSE
            RKLX = (SLP**2)*(1.D+0-(1.D+0-SLP**(1.D+0/RPLT(ITX,2,IZN)))
     &        **RPLT(ITX,2,IZN))
          ENDIF
!
!---    Triple-curve Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.302 ) THEN
          IF( IPHX.EQ.2 ) THEN
            RKLX = 0.D+0
            IF( SLX.GE.SCHR(4,IZN) )
     &        RKLX = SLP**(3.0D+0 + 2.0D+0/RPLT(ITX,1,IZN))
          ELSE
            RKLX = SLP**(3.0D+0 + 2.0D+0/RPLT(ITX,2,IZN))
          ENDIF
!
!---    Single-pressure dual-porosity van Genuchten  ---
!
        ELSEIF( ISCHR(IZN).EQ.3 ) THEN
          RKLM = (SLPM**2)*(1.D+0-(1.D+0-SLPM**(1.D+0/RPLT(ITX,2,IZN)))
     &      **RPLT(ITX,2,IZN))
          RKLF = (SLPF**2)*(1.D+0-(1.D+0-SLPF**(1.D+0/RPLT(ITX,1,IZN)))
     &      **RPLT(ITX,1,IZN))
          IMX = ITX + 3
          IFX = ITX + 6
          RKLX = ( PERM(IMX,IZN)*RKLM*(1.D+0-POR(4,IZN)) +
     &      PERM(IFX,IZN)*RKLF*POR(4,IZN) )/
     &      ( PERM(IMX,IZN)*(1.D+0-POR(4,IZN)) +
     &      PERM(IFX,IZN)*POR(4,IZN)
     &      + PERM(IFX,IZN)*POR(4,IZN) + SMALL )
!
!---    Single-pressure dual-porosity Brooks and Corey  ---
!
        ELSEIF( ISCHR(IZN).EQ.4 ) THEN
          RKLM = SLPM**(3.0D+0 + 2.0D+0/RPLT(ITX,2,IZN))
          RKLF = SLPF**(3.0D+0 + 2.0D+0/RPLT(ITX,1,IZN))
	      IMX = ITX + 3
          IFX = ITX + 6
          RKLX = ( PERM(IMX,IZN)*RKLM*(1.D+0-POR(4,IZN)) +
     &      PERM(IFX,IZN)*RKLF*POR(4,IZN) )/
     &      ( PERM(IMX,IZN)*(1.D+0-POR(4,IZN))
     &      + PERM(IFX,IZN)*POR(4,IZN)+ SMALL )
        ENDIF
!
!---  Corey relative permeability function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.3 ) THEN
        RKLX = SLP**4
!
!---  Fatt and Klikoff relative permeability function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.4 ) THEN
        RKLX = SLP**3
!
!---  Haverkamp relative permeability function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.5 ) THEN
        IF( HDGL.LE.RPLC(3,IZN) ) THEN
          RKLX = 1.D+0
        ELSE
          ALPHAX = RPLC(1,IZN)/RPLC(4,IZN)
          HDGLX = HDGL/RPLC(4,IZN)
          RKLX = ALPHAX/(ALPHAX +
     &      (HDGLX**RPLC(2,IZN)))
        ENDIF
!
!---  Touma and Vauclin relative permeability function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.6 ) THEN
        RKLX = RPLT(ITX,1,IZN)*(SLP**RPLT(ITX,2,IZN))
!
!---  Free Corey relative permeability function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.7 ) THEN
        SLRX = RPLC(3,IZN)
        SGRX = RPLC(4,IZN)
        SLPX = MIN( MAX( (SLX-SLRX)/(1.D+0-SLRX-SGRX),0.D+0 ),1.D+0 )
        RKLX = RPLC(1,IZN)*(SLPX**(RPLC(2,IZN)))
!
!---  Rijtema-Gardner modified exponential function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.9 ) THEN
        RKLX = EXP( RPLT(ITX,1,IZN)*HDGL + RPLT(ITX,2,IZN) )
!
!---  Modified-Mualem porosity distribution function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.22 ) THEN
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHR(IZN).EQ.1 .OR. ISCHR(IZN).EQ.101 ) THEN
          RKLX = (SLP**RPLT(ITX,2,IZN))*((1.D+0-(1.D+0-SLP**
     &      (1.D+0/RPLT(ITX,1,IZN)))**RPLT(ITX,1,IZN))**2)
!
!---    Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.2 .OR. ISCHR(IZN).EQ.102 ) THEN
          RKLX = (SLP**RPLT(ITX,2,IZN))*
     &      (SLP**(2.D+0 + 2.0D+0/RPLT(ITX,1,IZN)))
        ENDIF
!
!---  Tabular function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.10 ) THEN
        ITBX = 0
        IF( M.NE.2 ) ITBX = 1
        RKLX = FNTBLY( SLX,IRLTBL(1,IZN),IRLTBL(2,IZN),ITBX )
!
!---  Cubic-spline tabular function versus saturation  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.11 ) THEN
        ITBX = 0
        RKLX = FSPLNY( SLX,IRLTBLT(1,IZN,ITX),IRLTBLT(2,IZN,ITX) )
!
!---  Linear tabular function versus capillary head  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.12 ) THEN
        ITBX = 0
        RKLX = FNTBLY( HDGL,IRLTBLT(1,IZN,ITX),IRLTBLT(2,IZN,ITX),ITBX )
!
!---  Cubic-spline tabular function versus capillary head  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.13 ) THEN
        ITBX = 0
        RKLX = FSPLNY( HDGL,IRLTBLT(1,IZN,ITX),IRLTBLT(2,IZN,ITX) )
!
!---  Linear tabular function versus log capillary head  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.14 ) THEN
        ITBX = 0
        HDGLX = LOG(HDGL)
        RKLX = FNTBLY( HDGLX,IRLTBLT(1,IZN,ITX),IRLTBLT(2,IZN,ITX),
     &    ITBX )
!
!---  Cubic-spline tabular function versus log capillary head  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.15 ) THEN
        ITBX = 0
        HDGLX = LOG(HDGL)
        RKLX = FSPLNY( HDGLX,IRLTBLT(1,IZN,ITX),IRLTBLT(2,IZN,ITX) )
      ENDIF
      RKLX = MAX( RKLX,ZERO )
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RKLT1 group  ---
!
      RETURN
      END



!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SBND1( NSL )
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
!     Water Mode
!
!     Modify the Jacobian matrix for the solute transport equation
!     to incorporate boundary conditions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE PORMED
      USE JACOB
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

      REAL*8 BCX(LSPBC+1)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/SBND1'
!
!---  Loop over number of specified boundary conditions  ---
!
      NBCT = MIN( NSL+LUK,NSOLU+LUK+1 )
      DO 200 NB = 1,NBC
!
!---    Zero flux boundary condition  ---
!
        IF( IBCT(NBCT,NB).EQ.3 ) GOTO 200
        TMZ = TM
        IF( NSTEP-NRST.EQ.0 ) TMZ = TMZ*(1.D+0+EPSL)+EPSL
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
            IF( IBCT(NBCT,NB).EQ.12 ) BCX(1) = CBO(NB,NSL)
!
!---      Reactive species transport  ---
!
          ELSE
            BCX(1) = 0.D+0
            DO 10 NSPX = 1,IBCSP(1,NB)
              MX = NSOLU+LBCU+NSPX
              BCX(NSPX+1) = BC(MX,1,MB)
              IF( IBCT(NBCT,NB).EQ.12 ) BCX(NSPX+1) = SP_CBO(NB,NSP)
   10       CONTINUE
          ENDIF
        ELSE
          DO 100 M = 2,IBCM(NB)
            IF( TMZ.LE.BC(1,M,MB) ) THEN
              TDBC = (BC(1,M,MB)-BC(1,M-1,MB))
              DTBC = MIN( BC(1,M,MB)-TMZ,DT )
              TFBC = (TMZ-5.D-1*DTBC-BC(1,M-1,MB))/TDBC
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
                  NSP = IBCSP(NSPX+1,NB)
                  MX = NSOLU+LBCU+NSPX
                  BCX(NSPX+1) = BC(MX,M-1,MB) +
     &              TFBC*(BC(MX,M,MB)-BC(MX,M-1,MB))
                  IF( IBCT(NBCT,NB).EQ.12 ) BCX(NSPX+1) = SP_CBO(NB,NSP)
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
        IF( ILES.EQ.1 ) THEN
          MCOL = MP
          MROW = MDT
        ELSEIF( ILES.EQ.3 .OR. ILES.EQ.4 ) THEN
          MA = 1
          MCOL = KLUC(MP,MA)
          MA = MA + 1




        ENDIF
!
!---  Phase fraction factors at node adjacent to boundary  ---
!
        FCLP = YL(N,NSL)/(SL(2,N)*PORD(2,N)+SMALL)
!
!---    Diffusion coefficients at node adjacent to boundary  ---
!
        TCOR = (T(2,N)+TABS)/TSPRF
        SMDLP = SMDL(NSL)*TCOR*(VISRL/VISL(2,N))
!
!---    Kemper and van Schaik empirical model  ---
!
        IF( IEDL(NSL).EQ.2 ) THEN
          DLP = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &      EXP(SL(2,N)*PORD(2,N)*SDCL(3,IZN,NSL))
!
!---    Temperature dependent molecular diffusion coefficient  ---
!
        ELSEIF( IEDL(NSL).EQ.3 ) THEN
          DLP = TORL(2,N)*SL(2,N)*PORD(2,N)*SMDL(NSL)
!
!---    Power function molecular diffusion coefficient  ---
!
        ELSEIF( IEDL(NSL).EQ.4 ) THEN
          DLP = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &      (SL(2,N)*PORD(2,N))**SDCL(3,IZN,NSL)
!
!---    Constant molecular diffusion coefficient  ---
!
        ELSE
          DLP = TORL(2,N)*SL(2,N)*PORD(2,N)*SMDLP
        ENDIF
!
!---    Solute transport only, skip calculations for reactive
!       species transport  ---
!
        XVLB = SLB(2,NB)*PORDB(2,NB)
        IF( NSL.LE.NSOLU ) THEN
!
!---      Phase fraction factors at boundary  ---
!
          IF( IPCL(NSL).EQ.4 ) THEN
            NS = IPCSL(IZN,NSL)
            XVSB = SLB(2,NB)*RHOS(IZN)*PCSL(1,IZN,NS)*
     &        (1.D+0-PORTB(2,NB))
            CLX = CB(NB,NS)/(XVSB+XVLB)
            IF( CLX.LT.SMALL ) THEN
              PCSLX = PCSL(1,IZN,NSL)
            ELSE
              PCSLX = 1.D+1**(PCSL(2,IZN,NSL)+PCSL(3,IZN,NSL)*
     &          LOG10(CLX))
            ENDIF
            XVSB = RHOS(IZN)*PCSLX*(1.D+0-PORTB(2,NB))*SLB(2,NB)
          ELSEIF( IPCL(NSL).EQ.3 ) THEN
            NS = IPCSL(IZN,NSL)
            XVSB = SLB(2,NB)*RHOS(IZN)*PCSL(1,IZN,NS)*
     &        (1.D+0-PORTB(2,NB))
            CLX = CB(NB,NS)/(XVSB+XVLB)
            IF( CLX.LT.SMALL ) THEN
              PCSLX = PCSL(1,IZN,NSL)
            ELSE
              PCSLX = 1.D+1**(PCSL(2,IZN,NSL)+PCSL(3,IZN,NSL)*
     &          LOG10(CLX))
           ENDIF
            XVSB = RHOS(IZN)*PCSLX*(1.D+0-PORTB(2,NB))
          ELSEIF( IPCL(NSL).EQ.2 ) THEN
            XVSB = RHOS(IZN)*PCSL(1,IZN,NSL)*(1.D+0-PORTB(2,NB))*
     &        SLB(2,NB)
          ELSE
            XVSB = RHOS(IZN)*PCSL(1,IZN,NSL)*(1.D+0-PORTB(2,NB))
          ENDIF
!
!---      Phase-volumetric concentration ratios  ---
!
          FCL = 0.D+0
          IF( (XVSB+XVLB)/EPSL.GT.EPSL ) FCL = 1.D+0/(XVSB+XVLB)
!
!---      Phase mole fractions  ---
!
          YLB(NB,NSL) = XVLB*FCL
!
!---      Convert boundary phase concentrations to
!         volumetric concentrations  ---
!
          IF( IBCT(NBCT,NB).EQ.8 .OR. IBCT(NBCT,NB).EQ.14
     &      .OR. IBCT(NBCT,NB).EQ.23 ) THEN
            BCX(1) = BCX(1)/FCL
          ENDIF
!
!---      Load boundary concentration  ---
!
          CB(NB,NSL) = BCX(1)

        ELSE
!
!---      Convert species concentrations to total-component
!         concentrations  ---
!
          IF( NSL.LE.NSOLU+NEQC ) THEN
            NEQ = NSL-NSOLU
            DO 130 NSP = 1,IEQ_C(1,NEQ)
              DO 120 NSPX = 1,IBCSP(1,NB)
                IF( IBCSP(NSPX+1,NB).EQ.IEQ_C(NSP+1,NEQ) ) THEN
                  BCX(1) = BCX(1) + EQ_C(NSP,NEQ)*BCX(NSPX+1)
                ENDIF
  120         CONTINUE
  130       CONTINUE
!
!---      Convert species concentrations to total-kinetic
!         concentrations  ---
!
          ELSEIF( NSL.LE.NSOLU+NEQC+NEQK ) THEN
            NEQ = NSL-NSOLU-NEQC
            DO 150 NSP = 1,IEQ_K(1,NEQ)
              DO 140 NSPX = 1,IBCSP(1,NB)
                IF( IBCSP(NSPX+1,NB).EQ.IEQ_K(NSP+1,NEQ) ) THEN
                  BCX(1) = BCX(1) + EQ_K(NSP,NEQ)*BCX(NSPX+1)
                ENDIF
  140         CONTINUE
  150       CONTINUE
          ENDIF
!
!---      Phase-volumetric concentration ratios  ---
!
          FCL = 0.D+0
          IF( XVLB/EPSL.GT.EPSL ) FCL = 1.D+0/XVLB
!
!---      Phase mole fractions  ---
!
          YLB(NB,NSL) = 1.D+0
!
!---      Convert boundary phase concentrations to
!         volumetric concentrations  ---
!
          IF( IBCT(NBCT,NB).EQ.8 .OR. IBCT(NBCT,NB).EQ.14
     &      .OR. IBCT(NBCT,NB).EQ.23 ) THEN
            BCX(1) = BCX(1)/FCL
          ENDIF
!
!---      Load boundary concentration  ---
!
          CB(NB,NSL) = BCX(1)

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
            DPLB = DPLB*SMDEF(IZN,NSL)
          ELSE
            DPLB = 0.D+0
          ENDIF
          FLB = AFZ(NPZ)*WL(1,NPZ)
          CRLB = ABS( WL(1,NPZ) )*DT/(DZGF(N)*XVLB+SMALL)
!
!---  Dirichlet ---
!
          IF( IBCT(NBCT,NB).EQ.1 .OR. IBCT(NBCT,NB).EQ.8
     &      .OR. IBCT(NBCT,NB).EQ.12 ) THEN
            TCOR = (TB(2,NB)+TABS)/TSPRF
            SMDLB = SMDL(NSL)*TCOR*(VISRL/VISLB(2,NB))
            IF( IEDL(NSL).EQ.2 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          EXP(XVLB*SDCL(3,IZN,NSL))
            ELSEIF( IEDL(NSL).EQ.3 ) THEN
              DLB = TORLB(2,NB)*XVLB*SMDL(NSL)
            ELSEIF( IEDL(NSL).EQ.4 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          XVLB**SDCL(3,IZN,NSL)
            ELSE
              DLB = TORLB(2,NB)*XVLB*SMDLB
            ENDIF
            INDX = 16
            DLZ = DIFMN(DLB,DLP,DZGF(N),DZGF(N),WL(1,NPZ),INDX)
            DLZ = AFZ(NPZ)*(DLZ+DPLB)/(5.D-1*DZGF(N))
!
!---  TVD Transport for the boundary surface  ---
!
            IF( ISLC(1).GE.1 .AND. KFLD.GT.1 )  THEN
              IF( FLB.GE.ZERO ) THEN
                WCZ = BCX(1)*FCL*FLB
              ELSEIF( FLB.LT.ZERO .AND. K.LT.KFLD ) THEN
                NBT = N+IJFLD
                FCLT = YL(NBT,NSL)/(SL(2,NBT)*PORD(2,NBT)+SMALL)
                R = ((C(N,NSL)*FCLP-C(NBT,NSL)*FCLT)
     &            /(BCX(1)*FCL-C(N,NSL)*FCLP+SMALL))
     &            *(DZGF(N)/(DZGF(N)+DZGF(NBT)))
                THETA = FLIMIT( R,CRLB,ISLC(1) )
                WCZ = BCX(1)*FLB*THETA*FCL
     &            + C(N,NSL)*FLB*(1.D+0-THETA)*FCLP
              ELSEIF( FLB.LT.ZERO .AND. K.EQ.KFLD ) THEN
                WCZ = C(N,NSL)*FLB*FCLP
              ENDIF
              AB = DLZ*FCL
              AP = DLZ*FCLP
              WC(NPZ,NSL) = WC(NPZ,NSL) + WCZ/AFZ(NPZ)
              BLU(MP) = BLU(MP) + WCZ
!
!---  TVD Transport for interior surface adjacent to boundary  ---
!
              NQZ = NPZ+IJFLD
              FLT = AFZ(NQZ)*WL(1,NQZ)
              IF( FLT.GE.ZERO ) THEN
                NBT = N+IJFLD
                XVLX = SL(2,NBT)*PORD(2,NBT)
                FCLT = YL(NBT,NSL)/(XVLX+SMALL)
                CRLT = ABS( WL(1,NQZ) )*DT/DZGP(NQZ)/(XVLX+SMALL)
                R = ((C(N,NSL)*FCLP-BCX(1)*FCL)
     &            /(C(NBT,NSL)*FCLT-C(N,NSL)*FCLP+SMALL))
     &            *((DZGF(NBT)+DZGF(N))/DZGF(N))
                THETA = FLIMIT( R,CRLT,ISLC(1) )
                DZF = DZGF(N)/(DZGF(N)+DZGF(NBT))
                WCZ = C(N,NSL)*FLT*(1.D+0-THETA*DZF)*FCLP
     &            + C(NBT,NSL)*FLT*THETA*DZF*FCLT
                WCZF = CO(N,NSL)*FLT*FCLP
                WC(NQZ,NSL) = WC(NQZ,NSL) + WCZ/AFZ(NQZ) - WCZF/AFZ(NQZ)
                BLU(MP) = BLU(MP) - WCZ + WCZF
                BLU(IXP(NBT)) = BLU(IXP(NBT)) + WCZ - WCZF
              ENDIF
            ELSE
              ALB = MAX(FLB,ZERO)
     &          + DLZ*MAX((ONE-(TENTH*ABS(FLB)/(DLZ+SMALL)))**5,ZERO)
              AP = (ALB-FLB)*FCLP
              AB = ALB*FCL
            ENDIF
            BLU(MP) = BLU(MP) + AB*BCX(1)
!
!---  Outflow ---
!
          ELSEIF( IBCT(NBCT,NB).EQ.7 .OR.
     &      ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (FLB.LT.0.D+0)) ) THEN
            FLB = MIN( FLB,0.D+0 )
!
!---  TVD Transport for the boundary surface  ---
!
            IF( ISLC(1).GE.1 .AND. KFLD.GT.1 )  THEN
              WCZ = 0.D+0
              IF( FLB.LT.ZERO .AND. K.LT.KFLD ) THEN
                NBT = N+IJFLD
                FCLT = YL(NBT,NSL)/(SL(2,NBT)*PORD(2,NBT)+SMALL)
                R = ((C(N,NSL)*FCLP-C(NBT,NSL)*FCLT)
     &            /(BCX(1)*FCL-C(N,NSL)*FCLP+SMALL))
     &            *(DZGF(N)/(DZGF(N)+DZGF(NBT)))
                THETA = FLIMIT( R,CRLB,ISLC(1) )
                WCZ = BCX(1)*FLB*THETA*FCL
     &            + C(N,NSL)*FLB*(1.D+0-THETA)*FCLP
              ELSEIF( FLB.LT.ZERO .AND. K.EQ.KFLD ) THEN
                WCZ = C(N,NSL)*FLB*FCLP
              ENDIF
              AB = 0.D+0
              AP = 0.D+0
              WC(NPZ,NSL) = WC(NPZ,NSL) + WCZ/AFZ(NPZ)
              BLU(MP) = BLU(MP) + WCZ
!
!---  TVD Transport for interior surface adjacent to boundary  ---
!
              NQZ = NPZ+IJFLD
              FLT = AFZ(NQZ)*WL(1,NQZ)
              IF( FLT.GE.ZERO ) THEN
                NBT = N+IJFLD
                XVLX = SL(2,NBT)*PORD(2,NBT)
                FCLT = YL(NBT,NSL)/(XVLX+SMALL)
                CRLT = ABS( WL(1,NQZ) )*DT/DZGP(NQZ)/(XVLX+SMALL)
                R = ((C(N,NSL)*FCLP-BCX(1)*FCL)
     &            /(C(NBT,NSL)*FCLT-C(N,NSL)*FCLP+SMALL))
     &            *((DZGF(NBT)+DZGF(N))/DZGF(N))
                THETA = FLIMIT( R,CRLT,ISLC(1) )
                DZF = DZGF(N)/(DZGF(N)+DZGF(NBT))
                WCZ = C(N,NSL)*FLT*(1.D+0-THETA*DZF)*FCLP
     &            + C(NBT,NSL)*FLT*THETA*DZF*FCLT
                WCZF = CO(N,NSL)*FLT*FCLP
                WC(NQZ,NSL) = WC(NQZ,NSL) + WCZ/AFZ(NQZ) - WCZF/AFZ(NQZ)
                BLU(MP) = BLU(MP) - WCZ + WCZF
                BLU(IXP(NBT)) = BLU(IXP(NBT)) + WCZ - WCZF
              ENDIF
            ELSE
              ALB = MAX(FLB,ZERO)
              AP = (ALB-FLB)*FCLP
              AB = ALB*FCL
            ENDIF
            BLU(MP) = BLU(MP) + AB*BCX(1)
!
!--- Inflow ---
!
          ELSEIF( IBCT(NBCT,NB).EQ.13 .OR. IBCT(NBCT,NB).EQ.14
     &       .OR. ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (FLB.GE.0.D+0)) ) THEN
            FLB = MAX( FLB,0.D+0 )
!
!---  TVD Transport for the boundary surface  ---
!
            IF( ISLC(1).GE.1 .AND. KFLD.GT.1 )  THEN
              WCZ = 0.D+0
              IF( FLB.GE.ZERO ) WCZ = BCX(1)*FCL*FLB
              AB = 0.D+0
              AP = 0.D+0
              WC(NPZ,NSL) = WC(NPZ,NSL) + WCZ/AFZ(NPZ)
              BLU(MP) = BLU(MP) + WCZ
!
!---  TVD Transport for interior surface adjacent to boundary  ---
!
              NQZ = NPZ+IJFLD
              FLT = AFZ(NQZ)*WL(1,NQZ)
              IF( FLT.GE.ZERO ) THEN
                NBT = N+IJFLD
                XVLX = SL(2,NBT)*PORD(2,NBT)
                FCLT = YL(NBT,NSL)/(XVLX+SMALL)
                CRLT = ABS( WL(1,NQZ) )*DT/DZGP(NQZ)/(XVLX+SMALL)
                R = ((C(N,NSL)*FCLP-BCX(1)*FCL)
     &            /(C(NBT,NSL)*FCLT-C(N,NSL)*FCLP+SMALL))
     &            *((DZGF(NBT)+DZGF(N))/DZGF(N))
                THETA = FLIMIT( R,CRLT,ISLC(1) )
                DZF = DZGF(N)/(DZGF(N)+DZGF(NBT))
                WCZ = C(N,NSL)*FLT*(1.D+0-THETA*DZF)*FCLP
     &            + C(NBT,NSL)*FLT*THETA*DZF*FCLT
                WCZF = CO(N,NSL)*FLT*FCLP
                WC(NQZ,NSL) = WC(NQZ,NSL) + WCZ/AFZ(NQZ) - WCZF/AFZ(NQZ)
                BLU(MP) = BLU(MP) - WCZ + WCZF
                BLU(IXP(NBT)) = BLU(IXP(NBT)) + WCZ - WCZF
              ENDIF
            ELSE
              ALB = MAX(FLB,ZERO)
              AP = (ALB-FLB)*FCLP
              AB = ALB*FCL
            ENDIF
            BLU(MP) = BLU(MP) + AB*BCX(1)
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
            DPLS = DPLS*SMDEF(IZN,NSL)
          ELSE
            DPLS = 0.D+0
          ENDIF
          FLS = AFY(NPY)*VL(1,NPY)
          CRLS = ABS( VL(1,NPY) )*DT/(RP(I)*DYGF(N)*XVLB+SMALL)
!
!---  Dirichlet ---
!
          IF( IBCT(NBCT,NB).EQ.1 .OR. IBCT(NBCT,NB).EQ.8
     &      .OR. IBCT(NBCT,NB).EQ.12 ) THEN
            TCOR = (TB(2,NB)+TABS)/TSPRF
            SMDLB = SMDL(NSL)*TCOR*(VISRL/VISLB(2,NB))
            IF( IEDL(NSL).EQ.2 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          EXP(XVLB*SDCL(3,IZN,NSL))
            ELSEIF( IEDL(NSL).EQ.3 ) THEN
              DLB = TORLB(2,NB)*XVLB*SMDL(NSL)
            ELSEIF( IEDL(NSL).EQ.4 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          XVLB**SDCL(3,IZN,NSL)
            ELSE
              DLB = TORLB(2,NB)*XVLB*SMDLB
            ENDIF
            INDX = 16
            DLY = DIFMN(DLB,DLP,DYGF(N),DYGF(N),VL(1,NPY),INDX)
            DLY = AFY(NPY)*(DLY+DPLS)/(5.D-1*DYGF(N))/RP(I)
!
!---  TVD Transport for the boundary surface  ---
!
            IF( ISLC(1).GE.1 .AND. JFLD.GT.1 )  THEN
              IF( FLS.GE.ZERO ) THEN
                VCY = BCX(1)*FCL*FLS
              ELSEIF( FLS.LT.ZERO .AND. J.LT.JFLD ) THEN
                NBN = N+IFLD
                FCLN = YL(NBN,NSL)/(SL(2,NBN)*PORD(2,NBN)+SMALL)
                R = ((C(N,NSL)*FCLP-C(NBN,NSL)*FCLN)
     &            /(BCX(1)*FCL-C(N,NSL)*FCLP+SMALL))
     &            *(DYGF(N)/(DYGF(N)+DYGF(NBN)))
                THETA = FLIMIT( R,CRLS,ISLC(1) )
                VCY = BCX(1)*FLS*THETA*FCL
     &            + C(N,NSL)*FLS*(1.D+0-THETA)*FCLP
              ELSEIF( FLS.LT.ZERO .AND. J.EQ.JFLD ) THEN
                VCY = C(N,NSL)*FLS*FCLP
              ENDIF
              AS = DLY*FCL
              AP = DLY*FCLP
              VC(NPY,NSL) = VC(NPY,NSL) + VCY/AFY(NPY)
              BLU(MP) = BLU(MP) + VCY
!
!---  TVD Transport for interior surface adjacent to boundary  ---
!
              NQY = NPY+IFLD
              FLN = AFY(NQY)*VL(1,NQY)
              IF( FLN.GE.ZERO ) THEN
                NBN = N+IFLD
                XVLX = SL(2,NBN)*PORD(2,NBN)
                FCLN = YL(NBN,NSL)/(XVLX+SMALL)
                CRLN = ABS( VL(1,NQY) )*DT/DYGP(NQY)/(RP(I)*XVLX+SMALL)
                R = ((C(N,NSL)*FCLP-BCX(1)*FCL)
     &            /(C(NBN,NSL)*FCLN-C(N,NSL)*FCLP+SMALL))
     &            *((DYGF(NBN)+DYGF(N))/DYGF(N))
                THETA = FLIMIT( R,CRLN,ISLC(1) )
                DYF = DYGF(N)/(DYGF(N)+DYGF(NBN))
                VCY = C(N,NSL)*FLN*(1.D+0-THETA*DYF)*FCLP
     &            + C(NBN,NSL)*FLN*THETA*DYF*FCLN
                VCYF = CO(N,NSL)*FLN*FCLP
                VC(NQY,NSL) = VC(NQY,NSL) + VCY/AFY(NQY) - VCYF/AFY(NQY)
                BLU(MP) = BLU(MP) - VCY + VCYF
                BLU(IXP(NBN)) = BLU(IXP(NBN)) + VCY - VCYF
              ENDIF
            ELSE
              ALS = MAX(FLS,ZERO)
     &          + DLY*MAX((ONE-(TENTH*ABS(FLS)/(DLY+SMALL)))**5,ZERO)
              AP = (ALS-FLS)*FCLP
              AS = ALS*FCL
            ENDIF
            BLU(MP) = BLU(MP) + AS*BCX(1)
!
!---  Outflow ---
!
          ELSEIF( IBCT(NBCT,NB).EQ.7 .OR.
     &      ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (FLS.LT.0.D+0)) ) THEN
            FLS = MIN( FLS,0.D+0 )
!
!---  TVD Transport for the boundary surface  ---
!
            IF( ISLC(1).GE.1 .AND. JFLD.GT.1 )  THEN
              VCY = 0.D+0
              IF( FLS.LT.ZERO .AND. J.LT.JFLD ) THEN
                NBN = N+IFLD
                FCLN = YL(NBN,NSL)/(SL(2,NBN)*PORD(2,NBN)+SMALL)
                R = ((C(N,NSL)*FCLP-C(NBN,NSL)*FCLN)
     &            /(BCX(1)*FCL-C(N,NSL)*FCLP+SMALL))
     &            *(DYGF(N)/(DYGF(N)+DYGF(NBN)))
                THETA = FLIMIT( R,CRLS,ISLC(1) )
                VCY = BCX(1)*FLS*THETA*FCL
     &            + C(N,NSL)*FLS*(1.D+0-THETA)*FCLP
              ELSEIF( FLS.LT.ZERO .AND. J.EQ.JFLD ) THEN
                VCY = C(N,NSL)*FLS*FCLP
              ENDIF
              AS = 0.D+0
              AP = 0.D+0
              VC(NPY,NSL) = VC(NPY,NSL) + VCY/AFY(NPY)
              BLU(MP) = BLU(MP) + VCY
!
!---  TVD Transport for interior surface adjacent to boundary  ---
!
              NQY = NPY+IFLD
              FLN = AFY(NQY)*VL(1,NQY)
              IF( FLN.GE.ZERO ) THEN
                NBN = N+IFLD
                XVLX = SL(2,NBN)*PORD(2,NBN)
                FCLN = YL(NBN,NSL)/(XVLX+SMALL)
                CRLN = ABS( VL(1,NQY) )*DT/DYGP(NQY)/(RP(I)*XVLX+SMALL)
                R = ((C(N,NSL)*FCLP-BCX(1)*FCL)
     &            /(C(NBN,NSL)*FCLN-C(N,NSL)*FCLP+SMALL))
     &            *((DYGF(NBN)+DYGF(N))/DYGF(N))
                THETA = FLIMIT( R,CRLN,ISLC(1) )
                DYF = DYGF(N)/(DYGF(N)+DYGF(NBN))
                VCY = C(N,NSL)*FLN*(1.D+0-THETA*DYF)*FCLP
     &            + C(NBN,NSL)*FLN*THETA*DYF*FCLN
                VCYF = CO(N,NSL)*FLN*FCLP
                VC(NQY,NSL) = VC(NQY,NSL) + VCY/AFY(NQY) - VCYF/AFY(NQY)
                BLU(MP) = BLU(MP) - VCY + VCYF
                BLU(IXP(NBN)) = BLU(IXP(NBN)) + VCY - VCYF
              ENDIF
            ELSE
              ALS = MAX(FLS,ZERO)
              AP = (ALS-FLS)*FCLP
              AS = ALS*FCL
            ENDIF
            BLU(MP) = BLU(MP) + AS*BCX(1)
!
!--- Inflow ---
!
          ELSEIF( IBCT(NBCT,NB).EQ.13 .OR. IBCT(NBCT,NB).EQ.14
     &       .OR. ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (FLS.GE.0.D+0)) ) THEN
            FLS = MAX( FLS,0.D+0 )
!
!---  TVD Transport for the boundary surface  ---
!
            IF( ISLC(1).GE.1 .AND. JFLD.GT.1 )  THEN
              VCY = 0.D+0
              IF( FLS.GE.ZERO ) VCY = BCX(1)*FCL*FLS
              AS = 0.D+0
              AP = 0.D+0
              VC(NPY,NSL) = VC(NPY,NSL) + VCY/AFY(NPY)
              BLU(MP) = BLU(MP) + VCY
!
!---  TVD Transport for interior surface adjacent to boundary  ---
!
              NQY = NPY+IFLD
              FLN = AFY(NQY)*VL(1,NQY)
              IF( FLN.GE.ZERO ) THEN
                NBN = N+IFLD
                XVLX = SL(2,NBN)*PORD(2,NBN)
                FCLN = YL(NBN,NSL)/(XVLX+SMALL)
                CRLN = ABS( VL(1,NQY) )*DT/DYGP(NQY)/(RP(I)*XVLX+SMALL)
                R = ((C(N,NSL)*FCLP-BCX(1)*FCL)
     &            /(C(NBN,NSL)*FCLN-C(N,NSL)*FCLP+SMALL))
     &            *((DYGF(NBN)+DYGF(N))/DYGF(N))
                THETA = FLIMIT( R,CRLN,ISLC(1) )
                DYF = DYGF(N)/(DYGF(N)+DYGF(NBN))
                VCY = C(N,NSL)*FLN*(1.D+0-THETA*DYF)*FCLP
     &            + C(NBN,NSL)*FLN*THETA*DYF*FCLN
                VCYF = CO(N,NSL)*FLN*FCLP
                VC(NQY,NSL) = VC(NQY,NSL) + VCY/AFY(NQY) - VCYF/AFY(NQY)
                BLU(MP) = BLU(MP) - VCY + VCYF
                BLU(IXP(NBN)) = BLU(IXP(NBN)) + VCY - VCYF
              ENDIF
            ELSE
              ALS = MAX(FLS,ZERO)
              AP = (ALS-FLS)*FCLP
              AS = ALS*FCL
            ENDIF
            BLU(MP) = BLU(MP) + AS*BCX(1)
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
            DPLW = DPLW*SMDEF(IZN,NSL)
          ELSE
            DPLW = 0.D+0
          ENDIF
          FLW = AFX(NPX)*UL(1,NPX)
          CRLW = ABS( UL(1,NPX) )*DT/(DXGF(N)*XVLB+SMALL)
!
!---  Dirichlet ---
!
          IF( IBCT(NBCT,NB).EQ.1 .OR. IBCT(NBCT,NB).EQ.8
     &      .OR. IBCT(NBCT,NB).EQ.12 ) THEN
            TCOR = (TB(2,NB)+TABS)/TSPRF
            SMDLB = SMDL(NSL)*TCOR*(VISRL/VISLB(2,NB))
            IF( IEDL(NSL).EQ.2 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          EXP(XVLB*SDCL(3,IZN,NSL))
            ELSEIF( IEDL(NSL).EQ.3 ) THEN
              DLB = TORLB(2,NB)*XVLB*SMDL(NSL)
            ELSEIF( IEDL(NSL).EQ.4 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          XVLB**SDCL(3,IZN,NSL)
            ELSE
              DLB = TORLB(2,NB)*XVLB*SMDLB
            ENDIF
            INDX = 16
            DLX = DIFMN(DLB,DLP,DXGF(N),DXGF(N),UL(1,NPX),INDX)
            DLX = AFX(NPX)*(DLX+DPLW)/(5.D-1*DXGF(N))
!
!---  TVD Transport for the boundary surface  ---
!
            IF( ISLC(1).GE.1 .AND. IFLD.GT.1 )  THEN
              IF( FLW.GE.ZERO ) THEN
                UCX = BCX(1)*FCL*FLW
              ELSEIF( FLW.LT.ZERO .AND. I.LT.IFLD ) THEN
                NBE = N+1
                FCLE = YL(NBE,NSL)/(SL(2,NBE)*PORD(2,NBE)+SMALL)
                R = ((C(N,NSL)*FCLP-C(NBE,NSL)*FCLE)
     &            /(BCX(1)*FCL-C(N,NSL)*FCLP+SMALL))
     &            *(DXGF(N)/(DXGF(N)+DXGF(NBE)))
                THETA = FLIMIT( R,CRLW,ISLC(1) )
                UCX = C(N,NSL)*FLW*(1.D+0-THETA)*FCLP
     &            + BCX(1)*FLW*THETA*FCL
              ELSEIF( FLW.LT.ZERO .AND. I.EQ.IFLD ) THEN
                UCX = C(N,NSL)*FLW*FCLP
              ENDIF
              AW = DLX*FCL
              AP = DLX*FCLP
              UC(NPX,NSL) = UC(NPX,NSL) + UCX/AFX(NPX)
              BLU(MP) = BLU(MP) + UCX
!
!---  TVD Transport for interior surface adjacent to boundary  ---
!
              NQX = NPX+1
              FLE = AFX(NQX)*UL(1,NQX)
              IF( FLE.GE.ZERO ) THEN
                NBE = N+1
                XVLX = SL(2,NBE)*PORD(2,NBE)
                FCLE = YL(NBE,NSL)/(XVLX+SMALL)
                CRLE = ABS( UL(1,NQX) )*DT/DXGP(NQX)/(XVLX+SMALL)
                R = ((C(N,NSL)*FCLP-BCX(1)*FCL)
     &            /(C(NBE,NSL)*FCLE-C(N,NSL)*FCLP+SMALL))
     &            *((DXGF(NBE)+DXGF(N))/DXGF(N))
                THETA = FLIMIT( R,CRLE,ISLC(1) )
                DXF = DXGF(N)/(DXGF(N)+DXGF(NBE))
                UCX = C(N,NSL)*FLE*(1.D+0-THETA*DXF)*FCLP
     &            + C(NBE,NSL)*FLE*THETA*DXF*FCLE
                UCXF = CO(N,NSL)*FLE*FCLP
                UC(NQX,NSL) = UC(NQX,NSL) + UCX/AFX(NQX) - UCXF/AFX(NQX)
                BLU(MP) = BLU(MP) - UCX + UCXF
                BLU(IXP(NBE)) = BLU(IXP(NBE)) + UCX - UCXF
              ENDIF
            ELSE
              ALW = MAX(FLW,ZERO)
     &          + DLX*MAX((ONE-(TENTH*ABS(FLW)/(DLX+SMALL)))**5,ZERO)
              AP = (ALW-FLW)*FCLP
              AW = ALW*FCL
            ENDIF
            BLU(MP) = BLU(MP) + AW*BCX(1)
!
!---  Outflow ---
!
          ELSEIF( IBCT(NBCT,NB).EQ.7 .OR.
     &      ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (FLW.LT.0.D+0)) ) THEN
            FLW = MIN( FLW,0.D+0 )
!
!---  TVD Transport for the boundary surface  ---
!
            IF( ISLC(1).GE.1 .AND. IFLD.GT.1 )  THEN
              UCX = 0.D+0
              IF( FLW.LT.ZERO .AND. I.LT.IFLD ) THEN
                NBE = N+1
                FCLE = YL(NBE,NSL)/(SL(2,NBE)*PORD(2,NBE)+SMALL)
                R = ((C(N,NSL)*FCLP-C(NBE,NSL)*FCLE)
     &            /(BCX(1)*FCL-C(N,NSL)*FCLP+SMALL))
     &            *(DXGF(N)/(DXGF(N)+DXGF(NBE)))
                THETA = FLIMIT( R,CRLW,ISLC(1) )
                UCX = C(N,NSL)*FLW*(1.D+0-THETA)*FCLP
     &            + BCX(1)*FLW*THETA*FCL
              ELSEIF( FLW.LT.ZERO .AND. I.EQ.IFLD ) THEN
                UCX = C(N,NSL)*FLW*FCLP
              ENDIF
              AW = 0.D+0
              AP = 0.D+0
              UC(NPX,NSL) = UC(NPX,NSL) + UCX/AFX(NPX)
              BLU(MP) = BLU(MP) + UCX
!
!---  TVD Transport for interior surface adjacent to boundary  ---
!
              NQX = NPX+1
              FLE = AFX(NQX)*UL(1,NQX)
              IF( FLE.GE.ZERO ) THEN
                NBE = N+1
                XVLX = SL(2,NBE)*PORD(2,NBE)
                FCLE = YL(NBE,NSL)/(XVLX+SMALL)
                CRLE = ABS( UL(1,NQX) )*DT/DXGP(NQX)/(XVLX+SMALL)
                R = ((C(N,NSL)*FCLP-BCX(1)*FCL)
     &            /(C(NBE,NSL)*FCLE-C(N,NSL)*FCLP+SMALL))
     &            *((DXGF(NBE)+DXGF(N))/DXGF(N))
                THETA = FLIMIT( R,CRLE,ISLC(1) )
                DXF = DXGF(N)/(DXGF(N)+DXGF(NBE))
                UCX = C(N,NSL)*FLE*(1.D+0-THETA*DXF)*FCLP
     &            + C(NBE,NSL)*FLE*THETA*DXF*FCLE
                UCXF = CO(N,NSL)*FLE*FCLP
                UC(NQX,NSL) = UC(NQX,NSL) + UCX/AFX(NQX) - UCXF/AFX(NQX)
                BLU(MP) = BLU(MP) - UCX + UCXF
                BLU(IXP(NBE)) = BLU(IXP(NBE)) + UCX - UCXF
              ENDIF
            ELSE
              ALW = MAX(FLW,ZERO)
              AP = (ALW-FLW)*FCLP
              AW = ALW*FCL
            ENDIF
            BLU(MP) = BLU(MP) + AW*BCX(1)
!
!--- Inflow ---
!
          ELSEIF( IBCT(NBCT,NB).EQ.13 .OR. IBCT(NBCT,NB).EQ.14
     &       .OR. ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (FLW.GE.0.D+0)) ) THEN
            FLW = MAX( FLW,0.D+0 )
!
!---  TVD Transport for the boundary surface  ---
!
            IF( ISLC(1).GE.1 .AND. IFLD.GT.1 )  THEN
              UCX = 0.D+0
              IF( FLW.GE.ZERO ) UCX = BCX(1)*FCL*FLW
              AW = 0.D+0
              AP = 0.D+0
              UC(NPX,NSL) = UC(NPX,NSL) + UCX/AFX(NPX)
              BLU(MP) = BLU(MP) + UCX
!
!---  TVD Transport for interior surface adjacent to boundary  ---
!
              NQX = NPX+1
              FLE = AFX(NQX)*UL(1,NQX)
              IF( FLE.GE.ZERO ) THEN
                NBE = N+1
                XVLX = SL(2,NBE)*PORD(2,NBE)
                FCLE = YL(NBE,NSL)/(XVLX+SMALL)
                CRLE = ABS( UL(1,NQX) )*DT/DXGP(NQX)/(XVLX+SMALL)
                R = ((C(N,NSL)*FCLP-BCX(1)*FCL)
     &            /(C(NBE,NSL)*FCLE-C(N,NSL)*FCLP+SMALL))
     &            *((DXGF(NBE)+DXGF(N))/DXGF(N))
                THETA = FLIMIT( R,CRLE,ISLC(1) )
                DXF = DXGF(N)/(DXGF(N)+DXGF(NBE))
                UCX = C(N,NSL)*FLE*(1.D+0-THETA*DXF)*FCLP
     &            + C(NBE,NSL)*FLE*THETA*DXF*FCLE
                UCXF = CO(N,NSL)*FLE*FCLP
                UC(NQX,NSL) = UC(NQX,NSL) + UCX/AFX(NQX) - UCXF/AFX(NQX)
                BLU(MP) = BLU(MP) - UCX + UCXF
                BLU(IXP(NBE)) = BLU(IXP(NBE)) + UCX - UCXF
              ENDIF
            ELSE
              ALW = MAX(FLW,ZERO)
              AP = (ALW-FLW)*FCLP
              AW = ALW*FCL
            ENDIF
            BLU(MP) = BLU(MP) + AW*BCX(1)
          ENDIF
!
!---  East boundary
!
        ELSEIF( IBCD(NB).EQ.1 ) THEN
          NQX = NSX(N) + 1
!
!---  Hydraulic dispersion
!
          IF( IDISP.EQ.1 ) THEN
            CALL ADVEB( PORD(2,N),PORDB(2,NB),SL(2,N),SLB(2,NB),
     &        UL,VL,WL,ULEX,VLEX,WLEX,N,MF )
            CALL SHDP( ULEX,VLEX,WLEX,DISPL(IZN),DISPT(IZN),DPLE )
            DPLE = DPLE*SMDEF(IZN,NSL)
          ELSE
            DPLE = 0.D+0
          ENDIF
          FLE = AFX(NQX)*UL(1,NQX)
          CRLE = ABS( UL(1,NQX) )*DT/(DXGF(N)*XVLB+SMALL)
!
!---  Dirichlet ---
!
          IF( IBCT(NBCT,NB).EQ.1 .OR. IBCT(NBCT,NB).EQ.8
     &      .OR. IBCT(NBCT,NB).EQ.12 ) THEN
            TCOR = (TB(2,NB)+TABS)/TSPRF
            SMDLB = SMDL(NSL)*TCOR*(VISRL/VISLB(2,NB))
            IF( IEDL(NSL).EQ.2 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          EXP(XVLB*SDCL(3,IZN,NSL))
            ELSEIF( IEDL(NSL).EQ.3 ) THEN
              DLB = TORLB(2,NB)*XVLB*SMDL(NSL)
            ELSEIF( IEDL(NSL).EQ.4 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          XVLB**SDCL(3,IZN,NSL)
            ELSE
              DLB = TORLB(2,NB)*XVLB*SMDLB
            ENDIF
            INDX = 16
            DLX = DIFMN(DLP,DLB,DXGF(N),DXGF(N),UL(1,NQX),INDX)
            DLX = AFX(NQX)*(DLX+DPLE)/(5.D-1*DXGF(N))
!
!--- TVD Transport for the boundary surface  ---
!
            IF( ISLC(1).GE.1 .AND. IFLD.GT.1 ) THEN
              IF( FLE.LT.ZERO ) THEN
                UCX = BCX(1)*FCL*FLE
              ELSEIF( FLE.GE.ZERO .AND. I.GT.1 ) THEN
                NBW = N-1
                FCLW = YL(NBW,NSL)/(SL(2,NBW)*PORD(2,NBW)+SMALL)
                R = ((C(N,NSL)*FCLP-C(NBW,NSL)*FCLW)
     &            /(BCX(1)*FCL-C(N,NSL)*FCLP+SMALL))
     &            *(DXGF(N)/(DXGF(N)+DXGF(NBW)))
                THETA = FLIMIT( R,CRLE,ISLC(1) )
                UCX =  C(N,NSL)*FLE*(1.D+0-THETA)*FCLP
     &          +  BCX(1)*FLE*THETA*FCL
              ELSEIF( FLE.GE.ZERO .AND. I.EQ.1 ) THEN
                UCX =  C(N,NSL)*FLE*FCLP
              ENDIF
              AE = DLX*FCL
              AP = DLX*FCLP
              UC(NQX,NSL) = UC(NQX,NSL) + UCX/AFX(NQX)
              BLU(MP) = BLU(MP) - UCX
!
!--- TVD Transport for interior surface adjacent to boundary  ---
!
              NPX = NSX(N)
              FLW = AFX(NPX)*UL(1,NPX)
              IF( FLW.LT.ZERO ) THEN
                NBW = N-1
                XVLX = SL(2,NBW)*PORD(2,NBW)
                CRLW = ABS( UL(1,NPX) )*DT/DXGP(NPX)/(XVLX+SMALL)
                FCLW = YL(NBW,NSL)/(XVLX+SMALL)
                R = ((C(N,NSL)*FCLP-BCX(1)*FCL)
     &            /(C(NBW,NSL)*FCLW-C(N,NSL)*FCLP+SMALL))
     &            *((DXGF(NBW)+DXGF(N))/DXGF(N))
                THETA = FLIMIT( R,CRLW,ISLC(1) )
                DXF = DXGF(N)/(DXGF(N)+DXGF(NBW))
                UCX = C(N,NSL)*FLW*(1.D+0-THETA*DXF)*FCLP
     &            + C(NBW,NSL)*FLW*THETA*DXF*FCLW
                UCXF = CO(N,NSL)*FLW*FCLP
                UC(NPX,NSL) = UC(NPX,NSL) + UCX/AFX(NPX) - UCXF/AFX(NPX)
                BLU(MP) = BLU(MP) + UCX - UCXF
                BLU(IXP(NBW)) = BLU(IXP(NBW)) - UCX + UCXF
              ENDIF
            ELSE
              ALE = MAX( -FLE,ZERO ) +
     &          DLX*MAX((ONE-(TENTH*ABS(FLE)/(DLX+SMALL)))**5,ZERO)
              AP = (ALE+FLE)*FCLP
              AE = ALE*FCL
            ENDIF
            BLU(MP) = BLU(MP) + AE*BCX(1)
!
!---  Outflow ---
!
          ELSEIF( IBCT(NBCT,NB).EQ.7 .OR.
     &      ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (FLE.GE.0.D+0)) ) THEN
            FLE = MAX( FLE,0.D+0 )
!
!--- TVD Transport for the boundary surface  ---
!
            IF( ISLC(1).GE.1 .AND. IFLD.GT.1 ) THEN
              UCX = 0.D+0
              IF( FLE.GE.ZERO .AND. I.GT.1 ) THEN
                NBW = N-1
                FCLW = YL(NBW,NSL)/(SL(2,NBW)*PORD(2,NBW)+SMALL)
                R = ((C(N,NSL)*FCLP-C(NBW,NSL)*FCLW)
     &            /(BCX(1)*FCL-C(N,NSL)*FCLP+SMALL))
     &            *(DXGF(N)/(DXGF(N)+DXGF(NBW)))
                THETA = FLIMIT( R,CRLE,ISLC(1) )
                UCX =  C(N,NSL)*FLE*(1.D+0-THETA)*FCLP
     &          +  BCX(1)*FLE*THETA*FCL
              ELSEIF( FLE.GE.ZERO .AND. I.EQ.1 ) THEN
                UCX =  C(N,NSL)*FLE*FCLP
              ENDIF
              AE = 0.D+0
              AP = 0.D+0
              UC(NQX,NSL) = UC(NQX,NSL) + UCX/AFX(NQX)
              BLU(MP) = BLU(MP) - UCX
!
!--- TVD Transport for interior surface adjacent to boundary  ---
!
              NPX = NSX(N)
              FLW = AFX(NPX)*UL(1,NPX)
              IF( FLW.LT.ZERO ) THEN
                NBW = N-1
                XVLX = SL(2,NBW)*PORD(2,NBW)
                CRLW = ABS( UL(1,NPX) )*DT/DXGP(NPX)/(XVLX+SMALL)
                FCLW = YL(NBW,NSL)/(XVLX+SMALL)
                R = ((C(N,NSL)*FCLP-BCX(1)*FCL)
     &            /(C(NBW,NSL)*FCLW-C(N,NSL)*FCLP+SMALL))
     &            *((DXGF(NBW)+DXGF(N))/DXGF(N))
                THETA = FLIMIT( R,CRLW,ISLC(1) )
                DXF = DXGF(N)/(DXGF(N)+DXGF(NBW))
                UCX = C(N,NSL)*FLW*(1.D+0-THETA*DXF)*FCLP
     &            + C(NBW,NSL)*FLW*THETA*DXF*FCLW
                UCXF = CO(N,NSL)*FLW*FCLP
                UC(NPX,NSL) = UC(NPX,NSL) + UCX/AFX(NPX) - UCXF/AFX(NPX)
                BLU(MP) = BLU(MP) + UCX - UCXF
                BLU(IXP(NBW)) = BLU(IXP(NBW)) - UCX + UCXF
              ENDIF
            ELSE
              ALE = MAX( -FLE,ZERO )
              AP = (ALE+FLE)*FCLP
              AE = ALE*FCL
            ENDIF
            BLU(MP) = BLU(MP) + AE*BCX(1)
!
!--- Inflow ---
!
          ELSEIF( IBCT(NBCT,NB).EQ.13 .OR. IBCT(NBCT,NB).EQ.14
     &       .OR. ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (FLE.LT.0.D+0)) ) THEN
            FLE = MIN( FLE,0.D+0 )
!
!--- TVD Transport for the boundary surface  ---
!
            IF( ISLC(1).GE.1 .AND. IFLD.GT.1 ) THEN
              UCX = 0.D+0
              IF( FLE.LT.ZERO ) UCX = BCX(1)*FCL*FLE
              AE = 0.D+0
              AP = 0.D+0
              UC(NQX,NSL) = UC(NQX,NSL) + UCX/AFX(NQX)
              BLU(MP) = BLU(MP) - UCX
!
!--- TVD Transport for interior surface adjacent to boundary  ---
!
              NPX = NSX(N)
              FLW = AFX(NPX)*UL(1,NPX)
              IF( FLW.LT.ZERO ) THEN
                NBW = N-1
                XVLX = SL(2,NBW)*PORD(2,NBW)
                CRLW = ABS( UL(1,NPX) )*DT/DXGP(NPX)/(XVLX+SMALL)
                FCLW = YL(NBW,NSL)/(XVLX+SMALL)
                R = ((C(N,NSL)*FCLP-BCX(1)*FCL)
     &            /(C(NBW,NSL)*FCLW-C(N,NSL)*FCLP+SMALL))
     &            *((DXGF(NBW)+DXGF(N))/DXGF(N))
                THETA = FLIMIT( R,CRLW,ISLC(1) )
                DXF = DXGF(N)/(DXGF(N)+DXGF(NBW))
                UCX = C(N,NSL)*FLW*(1.D+0-THETA*DXF)*FCLP
     &            + C(NBW,NSL)*FLW*THETA*DXF*FCLW
                UCXF = CO(N,NSL)*FLW*FCLP
                UC(NPX,NSL) = UC(NPX,NSL) + UCX/AFX(NPX) - UCXF/AFX(NPX)
                BLU(MP) = BLU(MP) + UCX - UCXF
                BLU(IXP(NBW)) = BLU(IXP(NBW)) - UCX + UCXF
              ENDIF
            ELSE
              ALE = MAX( -FLE,ZERO )
              AP = (ALE+FLE)*FCLP
              AE = ALE*FCL
            ENDIF
            BLU(MP) = BLU(MP) + AE*BCX(1)
          ENDIF
!
!---  North boundary  ---
!
        ELSEIF( IBCD(NB).EQ.2 ) THEN
          NQY = NSY(N) + IFLD
!
!---  Hydraulic dispersion  ---
!
          IF( IDISP.EQ.1 ) THEN
            CALL ADVNB( PORD(2,N),PORDB(2,NB),SL(2,N),SLB(2,NB),
     &        UL,VL,WL,ULNX,VLNX,WLNX,N,MF )
            CALL SHDP( VLNX,WLNX,ULNX,DISPL(IZN),DISPT(IZN),DPLN )
            DPLN = DPLN*SMDEF(IZN,NSL)
          ELSE
            DPLN = 0.D+0
          ENDIF
          FLN = AFY(NQY)*VL(1,NQY)
          CRLN = ABS( VL(1,NQY) )*DT/(RP(I)*DYGF(N)*XVLB+SMALL)
!
!---  Dirichlet ---
!
          IF( IBCT(NBCT,NB).EQ.1 .OR. IBCT(NBCT,NB).EQ.8
     &      .OR. IBCT(NBCT,NB).EQ.12 ) THEN
            TCOR = (TB(2,NB)+TABS)/TSPRF
            SMDLB = SMDL(NSL)*TCOR*(VISRL/VISLB(2,NB))
            IF( IEDL(NSL).EQ.2 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          EXP(XVLB*SDCL(3,IZN,NSL))
            ELSEIF( IEDL(NSL).EQ.3 ) THEN
              DLB = TORLB(2,NB)*XVLB*SMDL(NSL)
            ELSEIF( IEDL(NSL).EQ.4 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          XVLB**SDCL(3,IZN,NSL)
            ELSE
              DLB = TORLB(2,NB)*XVLB*SMDLB
            ENDIF
            INDX = 16
            DLY = DIFMN(DLP,DLB,DYGF(N),DYGF(N),VL(1,NQY),INDX)
            DLY = AFY(NQY)*(DLY+DPLN)/(5.D-1*DYGF(N))/RP(I)
!
!--- TVD Transport for the boundary surface  ---
!
            IF( ISLC(1).GE.1 .AND. JFLD.GT.1 ) THEN
              IF( FLN.LT.ZERO ) THEN
                VCY = BCX(1)*FCL*FLN
              ELSEIF( FLN.GE.ZERO .AND. J.GT.1 ) THEN
                NBS = N-IFLD
                FCLS = YL(NBS,NSL)/(SL(2,NBS)*PORD(2,NBS)+SMALL)
                R = ((C(N,NSL)*FCLP-C(NBS,NSL)*FCLS)
     &            /(BCX(1)*FCL-C(N,NSL)*FCLP+SMALL))
     &            *(DYGF(N)/(DYGF(N)+DYGF(NBS)))
                THETA = FLIMIT( R,CRLN,ISLC(1) )
                VCY =  BCX(1)*FLN*THETA*FCL
     &            + C(N,NSL)*FLN*(1.D+0-THETA)*FCLP
              ELSEIF( FLN.GE.ZERO .AND. J.EQ.1 ) THEN
                VCY =  C(N,NSL)*FLN*FCLP
              ENDIF
              AN = DLY*FCL
              AP = DLY*FCLP
              VC(NQY,NSL) = VC(NQY,NSL) + VCY/AFY(NQY)
              BLU(MP) = BLU(MP) - VCY
!
!--- TVD Transport for interior surface adjacent to boundary  ---
!
              NPY = NSY(N)
              FLS = AFY(NPY)*VL(1,NPY)
              IF( FLS.LT.ZERO ) THEN
                NBS = N-IFLD
                XVLX = SL(2,NBS)*PORD(2,NBS)
                CRLS = ABS( VL(1,NPY) )*DT/DYGP(NPY)/(XVLX*RP(I)+SMALL)
                FCLS = YL(NBS,NSL)/(XVLX+SMALL)
                R = ((C(N,NSL)*FCLP-BCX(1)*FCL)
     &            /(C(NBS,NSL)*FCLS-C(N,NSL)*FCLP+SMALL))
     &            *((DYGF(NBS)-DYGF(N))/DYGF(N))
                THETA = FLIMIT( R,CRLS,ISLC(1) )
                DYF = DYGF(N)/(DYGF(N)+DYGF(NBS))
                VCY = C(N,NSL)*FLS*(1.D+0-THETA*DYF)*FCLP
     &            + C(NBS,NSL)*FLS*THETA*DYF*FCLS
                VCYF = CO(N,NSL)*FLS*FCLP
                VC(NPY,NSL) = VC(NPY,NSL) + VCY/AFY(NPY) - VCYF/AFY(NPY)
                BLU(MP) = BLU(MP) + VCY - VCYF
                BLU(IXP(NBS)) = BLU(IXP(NBS)) - VCY + VCYF
              ENDIF
            ELSE
              ALN = MAX( -FLN,ZERO ) +
     &          DLY*MAX((ONE-(TENTH*ABS(FLN)/(DLY+SMALL)))**5,ZERO)
              AP = (ALN+FLN)*FCLP
              AN = ALN*FCL
            ENDIF
            BLU(MP) = BLU(MP) + AN*BCX(1)
!
!---  Outflow ---
!
          ELSEIF( IBCT(NBCT,NB).EQ.7 .OR.
     &      ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (FLN.GE.0.D+0)) ) THEN
            FLN = MAX( FLN,0.D+0 )
!
!--- TVD Transport for the boundary surface  ---
!
            IF( ISLC(1).GE.1 .AND. JFLD.GT.1 ) THEN
              VCY = 0.D+0
              IF( FLN.GE.ZERO .AND. J.GT.1 ) THEN
                NBS = N-IFLD
                FCLS = YL(NBS,NSL)/(SL(2,NBS)*PORD(2,NBS)+SMALL)
                R = ((C(N,NSL)*FCLP-C(NBS,NSL)*FCLS)
     &            /(BCX(1)*FCL-C(N,NSL)*FCLP+SMALL))
     &            *(DYGF(N)/(DYGF(N)+DYGF(NBS)))
                THETA = FLIMIT( R,CRLN,ISLC(1) )
                VCY =  BCX(1)*FLN*THETA*FCL
     &            + C(N,NSL)*FLN*(1.D+0-THETA)*FCLP
              ELSEIF( FLN.GE.ZERO .AND. J.EQ.1 ) THEN
                VCY =  C(N,NSL)*FLN*FCLP
              ENDIF
              AN = 0.D+0
              AP = 0.D+0
              VC(NQY,NSL) = VC(NQY,NSL) + VCY/AFY(NQY)
              BLU(MP) = BLU(MP) - VCY
!
!--- TVD Transport for interior surface adjacent to boundary  ---
!
              NPY = NSY(N)
              FLS = AFY(NPY)*VL(1,NPY)
              IF( FLS.LT.ZERO ) THEN
                NBS = N-IFLD
                XVLX = SL(2,NBS)*PORD(2,NBS)
                CRLS = ABS( VL(1,NPY) )*DT/DYGP(NPY)/(XVLX*RP(I)+SMALL)
                FCLS = YL(NBS,NSL)/(XVLX+SMALL)
                R = ((C(N,NSL)*FCLP-BCX(1)*FCL)
     &            /(C(NBS,NSL)*FCLS-C(N,NSL)*FCLP+SMALL))
     &            *((DYGF(NBS)-DYGF(N))/DYGF(N))
                THETA = FLIMIT( R,CRLS,ISLC(1) )
                DYF = DYGF(N)/(DYGF(N)+DYGF(NBS))
                VCY = C(N,NSL)*FLS*(1.D+0-THETA*DYF)*FCLP
     &            + C(NBS,NSL)*FLS*THETA*DYF*FCLS
                VCYF = CO(N,NSL)*FLS*FCLP
                VC(NPY,NSL) = VC(NPY,NSL) + VCY/AFY(NPY) - VCYF/AFY(NPY)
                BLU(MP) = BLU(MP) + VCY - VCYF
                BLU(IXP(NBS)) = BLU(IXP(NBS)) - VCY + VCYF
              ENDIF
            ELSE
              ALN = MAX( -FLN,ZERO )
              AP = (ALN+FLN)*FCLP
              AN = ALN*FCL
            ENDIF
            BLU(MP) = BLU(MP) + AN*BCX(1)
!
!--- Inflow ---
!
          ELSEIF( IBCT(NBCT,NB).EQ.13 .OR. IBCT(NBCT,NB).EQ.14
     &       .OR. ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (FLN.LT.0.D+0)) ) THEN
            FLN = MIN( FLN,0.D+0 )
!
!--- TVD Transport for the boundary surface  ---
!
            IF( ISLC(1).GE.1 .AND. JFLD.GT.1 ) THEN
              VCY = 0.D+0
              IF( FLN.LT.ZERO ) VCY = BCX(1)*FCL*FLN
              AN = 0.D+0
              AP = 0.D+0
              VC(NQY,NSL) = VC(NQY,NSL) + VCY/AFY(NQY)
              BLU(MP) = BLU(MP) - VCY
!
!--- TVD Transport for interior surface adjacent to boundary  ---
!
              NPY = NSY(N)
              FLS = AFY(NPY)*VL(1,NPY)
              IF( FLS.LT.ZERO ) THEN
                NBS = N-IFLD
                XVLX = SL(2,NBS)*PORD(2,NBS)
                CRLS = ABS( VL(1,NPY) )*DT/DYGP(NPY)/(XVLX*RP(I)+SMALL)
                FCLS = YL(NBS,NSL)/(XVLX+SMALL)
                R = ((C(N,NSL)*FCLP-BCX(1)*FCL)
     &            /(C(NBS,NSL)*FCLS-C(N,NSL)*FCLP+SMALL))
     &            *((DYGF(NBS)-DYGF(N))/DYGF(N))
                THETA = FLIMIT( R,CRLS,ISLC(1) )
                DYF = DYGF(N)/(DYGF(N)+DYGF(NBS))
                VCY = C(N,NSL)*FLS*(1.D+0-THETA*DYF)*FCLP
     &            + C(NBS,NSL)*FLS*THETA*DYF*FCLS
                VCYF = CO(N,NSL)*FLS*FCLP
                VC(NPY,NSL) = VC(NPY,NSL) + VCY/AFY(NPY) - VCYF/AFY(NPY)
                BLU(MP) = BLU(MP) + VCY - VCYF
                BLU(IXP(NBS)) = BLU(IXP(NBS)) - VCY + VCYF
              ENDIF
            ELSE
              ALN = MAX( -FLN,ZERO )
              AP = (ALN+FLN)*FCLP
              AN = ALN*FCL
            ENDIF
            BLU(MP) = BLU(MP) + AN*BCX(1)
          ENDIF
!
!---  Top boundary
!
        ELSEIF( IBCD(NB).EQ.3 ) THEN
          NQZ = NSZ(N) + IJFLD
!
!---  Hydraulic dispersion
!
          IF( IDISP.EQ.1 ) THEN
            CALL ADVTB( PORD(2,N),PORDB(2,NB),SL(2,N),SLB(2,NB),
     &        UL,VL,WL,ULTX,VLTX,WLTX,N,MF )
            CALL SHDP( WLTX,ULTX,VLTX,DISPL(IZN),DISPT(IZN),DPLT )
            DPLT = DPLT*SMDEF(IZN,NSL)
          ELSE
            DPLT = 0.D+0
          ENDIF
          FLT = AFZ(NQZ)*WL(1,NQZ)
          CRLT = ABS( WL(1,NQZ) )*DT/(DZGF(N)*XVLB+SMALL)
!
!---  Dirichlet ---
!
          IF( IBCT(NBCT,NB).EQ.1 .OR. IBCT(NBCT,NB).EQ.8
     &      .OR. IBCT(NBCT,NB).EQ.12 ) THEN
            TCOR = (TB(2,NB)+TABS)/TSPRF
            SMDLB = SMDL(NSL)*TCOR*(VISRL/VISLB(2,NB))
            IF( IEDL(NSL).EQ.2 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          EXP(XVLB*SDCL(3,IZN,NSL))
            ELSEIF( IEDL(NSL).EQ.3 ) THEN
              DLB = TORLB(2,NB)*XVLB*SMDL(NSL)
            ELSEIF( IEDL(NSL).EQ.4 ) THEN
              DLB = SDCL(1,IZN,NSL)*SDCL(2,IZN,NSL)*
     &          XVLB**SDCL(3,IZN,NSL)
            ELSE
              DLB = TORLB(2,NB)*XVLB*SMDLB
            ENDIF
            INDX = 16
            DLZ = DIFMN(DLP,DLB,DZGF(N),DZGF(N),WL(1,NQZ),INDX)
            DLZ = AFZ(NQZ)*(DLZ+DPLT)/(5.D-1*DZGF(N))
!
!--- TVD Transport for the boundary surface  ---
!
            IF( ISLC(1).GE.1 .AND. KFLD.GT.1 ) THEN
              IF( FLT.LT.ZERO ) THEN
                WCZ = BCX(1)*FCL*FLT
              ELSEIF( FLT.GE.ZERO .AND. K.GT.1 ) THEN
                NBB = N-IJFLD
                FCLB = YL(NBB,NSL)/(SL(2,NBB)*PORD(2,NBB)+SMALL)
                R = ((C(N,NSL)*FCLP-C(NBB,NSL)*FCLB)
     &            /(BCX(1)*FCL-C(N,NSL)*FCLP+SMALL))
     &            *(DZGF(N)/(DZGF(N)+DZGF(NBB)))
                THETA = FLIMIT( R,CRLT,ISLC(1) )
                WCZ =  C(N,NSL)*FLT*(1.D+0-THETA)*FCLP
     &            + BCX(1)*FLT*THETA*FCL
              ELSEIF( FLT.GE.ZERO .AND. K.EQ.1 ) THEN
                WCZ =  C(N,NSL)*FLT*FCLP
              ENDIF
              AT = DLZ*FCL
              AP = DLZ*FCLP
              WC(NQZ,NSL) = WC(NQZ,NSL) + WCZ/AFZ(NQZ)
              BLU(MP) = BLU(MP) - WCZ
!
!--- TVD Transport for interior surface adjacent to boundary  ---
!
              NPZ = NSZ(N)
              FLB = AFZ(NPZ)*WL(1,NPZ)
              IF( FLB.LT.ZERO ) THEN
                NBB = N-IJFLD
                XVLX = SL(2,NBB)*PORD(2,NBB)
                CRLB = ABS( WL(1,NPZ) )*DT/DZGP(NPZ)/(XVLX+SMALL)
                FCLB = YL(NBB,NSL)/(XVLX+SMALL)
                R = ((C(N,NSL)*FCLP-BCX(1)*FCL)
     &            /(C(NBB,NSL)*FCLB-C(N,NSL)*FCLP+SMALL))
     &            *((DZGF(NBB)+DZGF(N))/DZGF(N))
                THETA = FLIMIT( R,CRLB,ISLC(1) )
                DZF = DZGF(N)/(DZGF(N)+DZGF(NBB))
                WCZ = C(N,NSL)*FLB*(1.D+0-THETA*DZF)*FCLP
     &            + C(NBB,NSL)*FLB*THETA*DZF*FCLB
                WCZF = CO(N,NSL)*FLB*FCLP
                WC(NPZ,NSL) = WC(NPZ,NSL) + WCZ/AFZ(NPZ) - WCZF/AFZ(NPZ)
                BLU(MP) = BLU(MP) + WCZ - WCZF
                BLU(IXP(NBB)) = BLU(IXP(NBB)) - WCZ + WCZF
              ENDIF
            ELSE
              ALT = MAX( -FLT,ZERO ) +
     &          DLZ*MAX((ONE-(TENTH*ABS(FLT)/(DLZ+SMALL)))**5,ZERO)
              AP = (ALT+FLT)*FCLP
              AT = ALT*FCL
            ENDIF
            BLU(MP) = BLU(MP) + AT*BCX(1)
!
!---  Outflow ---
!
          ELSEIF( IBCT(NBCT,NB).EQ.7 .OR.
     &      ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (FLT.GE.0.D+0)) ) THEN
            FLT = MAX( FLT,0.D+0 )
!
!--- TVD Transport for the boundary surface  ---
!
            IF( ISLC(1).GE.1 .AND. KFLD.GT.1 ) THEN
              WCZ = 0.D+0
              IF( FLT.GE.ZERO .AND. K.GT.1 ) THEN
                NBB = N-IJFLD
                FCLB = YL(NBB,NSL)/(SL(2,NBB)*PORD(2,NBB)+SMALL)
                R = ((C(N,NSL)*FCLP-C(NBB,NSL)*FCLB)
     &            /(BCX(1)*FCL-C(N,NSL)*FCLP+SMALL))
     &            *(DZGF(N)/(DZGF(N)+DZGF(NBB)))
                THETA = FLIMIT( R,CRLT,ISLC(1) )
                WCZ =  C(N,NSL)*FLT*(1.D+0-THETA)*FCLP
     &            + BCX(1)*FLT*THETA*FCL
              ELSEIF( FLT.GE.ZERO .AND. K.EQ.1 ) THEN
                WCZ =  C(N,NSL)*FLT*FCLP
              ENDIF
              AT = 0.D+0
              AP = 0.D+0
              WC(NQZ,NSL) = WC(NQZ,NSL) + WCZ/AFZ(NQZ)
              BLU(MP) = BLU(MP) - WCZ
!
!--- TVD Transport for interior surface adjacent to boundary  ---
!
              NPZ = NSZ(N)
              FLB = AFZ(NPZ)*WL(1,NPZ)
              IF( FLB.LT.ZERO ) THEN
                NBB = N-IJFLD
                XVLX = SL(2,NBB)*PORD(2,NBB)
                CRLB = ABS( WL(1,NPZ) )*DT/DZGP(NPZ)/(XVLX+SMALL)
                FCLB = YL(NBB,NSL)/(XVLX+SMALL)
                R = ((C(N,NSL)*FCLP-BCX(1)*FCL)
     &            /(C(NBB,NSL)*FCLB-C(N,NSL)*FCLP+SMALL))
     &            *((DZGF(NBB)+DZGF(N))/DZGF(N))
                THETA = FLIMIT( R,CRLB,ISLC(1) )
                DZF = DZGF(N)/(DZGF(N)+DZGF(NBB))
                WCZ = C(N,NSL)*FLB*(1.D+0-THETA*DZF)*FCLP
     &            + C(NBB,NSL)*FLB*THETA*DZF*FCLB
                WCZF = CO(N,NSL)*FLB*FCLP
                WC(NPZ,NSL) = WC(NPZ,NSL) + WCZ/AFZ(NPZ) - WCZF/AFZ(NPZ)
                BLU(MP) = BLU(MP) + WCZ - WCZF
                BLU(IXP(NBB)) = BLU(IXP(NBB)) - WCZ + WCZF
              ENDIF
            ELSE
              ALT = MAX( -FLT,ZERO )
              AP = (ALT+FLT)*FCLP
              AT = ALT*FCL
            ENDIF
            BLU(MP) = BLU(MP) + AT*BCX(1)
!
!--- Inflow ---
!
          ELSEIF( IBCT(NBCT,NB).EQ.13 .OR. IBCT(NBCT,NB).EQ.14
     &       .OR. ((IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23)
     &      .AND. (FLT.LT.0.D+0)) ) THEN
            FLT = MIN( FLT,0.D+0 )
!
!--- TVD Transport for the boundary surface  ---
!
            IF( ISLC(1).GE.1 .AND. KFLD.GT.1 ) THEN
              WCZ = 0.D+0
              IF( FLT.LT.ZERO ) WCZ = BCX(1)*FCL*FLT
              AT = 0.D+0
              AP = 0.D+0
              WC(NQZ,NSL) = WC(NQZ,NSL) + WCZ/AFZ(NQZ)
              BLU(MP) = BLU(MP) - WCZ
!
!--- TVD Transport for interior surface adjacent to boundary  ---
!
              NPZ = NSZ(N)
              FLB = AFZ(NPZ)*WL(1,NPZ)
              IF( FLB.LT.ZERO ) THEN
                NBB = N-IJFLD
                XVLX = SL(2,NBB)*PORD(2,NBB)
                CRLB = ABS( WL(1,NPZ) )*DT/DZGP(NPZ)/(XVLX+SMALL)
                FCLB = YL(NBB,NSL)/(XVLX+SMALL)
                R = ((C(N,NSL)*FCLP-BCX(1)*FCL)
     &            /(C(NBB,NSL)*FCLB-C(N,NSL)*FCLP+SMALL))
     &            *((DZGF(NBB)+DZGF(N))/DZGF(N))
                THETA = FLIMIT( R,CRLB,ISLC(1) )
                DZF = DZGF(N)/(DZGF(N)+DZGF(NBB))
                WCZ = C(N,NSL)*FLB*(1.D+0-THETA*DZF)*FCLP
     &            + C(NBB,NSL)*FLB*THETA*DZF*FCLB
                WCZF = CO(N,NSL)*FLB*FCLP
                WC(NPZ,NSL) = WC(NPZ,NSL) + WCZ/AFZ(NPZ) - WCZF/AFZ(NPZ)
                BLU(MP) = BLU(MP) + WCZ - WCZF
                BLU(IXP(NBB)) = BLU(IXP(NBB)) - WCZ + WCZF
              ENDIF
            ELSE
              ALT = MAX( -FLT,ZERO )
              AP = (ALT+FLT)*FCLP
              AT = ALT*FCL
            ENDIF
            BLU(MP) = BLU(MP) + AT*BCX(1)
          ENDIF
        ENDIF
        IF( ILES.EQ.1 ) THEN
          ALU(MROW,MCOL) = ALU(MROW,MCOL) + AP
        ELSEIF( ILES.EQ.3 .OR. ILES.EQ.4 ) THEN
          DLU(MCOL) = DLU(MCOL) + AP




        ENDIF
  200 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of SBND1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SMC1
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
!     Water Mode
!
!     Control saturation, relative permeability, porosity, and
!     tortuosity calculations
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE REACT
      USE PORMED
      USE JACOB
      USE HYST
      USE GRID
      USE FDVP
      USE FDVD
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
      SUB_LOG(ISUB_LOG) = '/SMC1'
      INDX = 1
!$OMP PARALLEL DO
!$OMP&  COPYIN(ISUB_LOG)
!$OMP&  DEFAULT(NONE)
!$OMP&  SHARED(NFLD,IXP,IZ,ISVC,TRPGL,SCHR,ASLMIN,PG,PL,SL,RKL,
!$OMP&    IPH,ASL,ASGT,SGT,SG,ZERO,PATM,PCMP,PORD,PORT,ISLC,TORL,
!$OMP&    IPRF,PERMRF,POR,POR0,PERM)
!$OMP&  PRIVATE(IZN,SGRMX,ASLMINX,ASLX,ASGTX,PX,TORGX,TORNX,DPX)
!$OMP&  FIRSTPRIVATE(INDX)
      DO 200 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 200
        IZN = IZ(N)

        POR0(1,N) = POR0(1,N)
        POR0(2,N) = POR0(2,N)

        DO 100 M = 2,ISVC+2
          SGRMX = SCHR(15,IZN)/(1.D+0+TRPGL(2,N)/SCHR(9,IZN))
          ASLMINX = ASLMIN(1,N)
          CALL KSP1( N,IZN,M,PG(M,N),PL(M,N),SL(M,N),RKL(1,M,N),
     &      ASLX,ASLMINX,ASGTX,SGRMX,INDX,IPH(2,N),
     &      PG(1,N),PL(1,N),SL(1,N) )
          IF( M.EQ.2 ) THEN
            ASL(N) = ASLX
            ASGT(N) = ASGTX
            ASLMIN(2,N) = ASLMINX
          ENDIF
          SGT(M,N) = ASGTX*(1.D+0-SCHR(4,IZN))
          SG(M,N) = MAX( 1.D+0-SL(M,N),ZERO )
          PX = MAX( PG(M,N),PL(M,N) )+PATM
          CALL PORSTY(N,PX,PCMP(N),PORD(M,N),PORT(M,N))
          IF( ISLC(3).EQ.1 ) CALL TORTU( IZN,SL(M,N),SG(M,N),ZERO,
     &      PORD(M,N),TORL(M,N),TORGX,TORNX )
!
!---      Kozeny-Carman Permeability Model ---
!
          PERMRF(M,N) = 1.D+0
          IF( IPRF(IZN).EQ.2 ) CALL PERM_I( PERMRF(M,N),PORD(M,N) )
!
!---      Poroelastic Permeability Model ---
!
          IF( IPRF(IZN)/10.EQ.1 ) THEN
            DPX = PX-PCMP(N)
            PERMRF(M,N) = PERMRF(M,N)*EXP( PERM(4,IZN)*DPX/PERM(5,IZN) )
          ENDIF
  100   CONTINUE
  200 CONTINUE
!$OMP END PARALLEL DO
      ISUB_LOG = ISUB_LOG-1
!
!---  End of SMC1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SORC1
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
!     Water Mode
!
!     Compute source terms.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE PORMED
      USE JACOB
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



      REAL*8 SRX(8+LSOLU)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/SORC1'
!
!---  Zero source terms  ---
!
      IF( NSR.GT.0 ) THEN
!$OMP PARALLEL DO
!$OMP&  DEFAULT(NONE)
!$OMP&  SHARED(NFLD,ISVC,SRCW)
        DO 50 N = 1,NFLD
          DO 40 M = 2,ISVC+2
              SRCW(M,N) = 0.D+0
   40     CONTINUE
   50   CONTINUE
!$OMP END PARALLEL DO
!
!---  Loop over sources  ---
!
!$OMP PARALLEL DO
!$OMP&  COPYIN(ISUB_LOG)
!$OMP&  DEFAULT(NONE)
!$OMP&  SHARED(NSR,TM,SRC,ISRM,NSOLU,DT,ISRT,ISRDM,ND,IZ,IXP,ISVC,
!$OMP&    SRCW,RHOL,VOL,AFZ,NSZ,GPI,DZGF,PATM,T,ZP,GRAVZ,PERM,EPSL,
!$OMP&    VISL,PL,RKL,NSX,DXGF,XP,GRAVX,AFY,NSY,DYGF,YP,RP,GRAVY)
!$OMP&  PRIVATE(SRX,DTSR,TFSR,IZN,RDW,RDE,ACWX,DRD2,PLWX,PX,RHOLX,
!$OMP&    NX,GB,PERMX,HCLX,QLX,RKLX,AFX)
      DO 700 NS = 1,NSR
!
!---  Check source time  ---
!
        IF( TM.LE.SRC(1,1,NS) ) GOTO 700
        SRX(1) = TM
        IF( ISRM(NS).EQ.1 ) THEN
          DO 70 N = 2,8+NSOLU
            SRX(N) = SRC(N,1,NS)
   70     CONTINUE

        ELSE
          DO 100 M = 2,ISRM(NS)
            IF( TM.LE.SRC(1,M,NS) ) THEN
              DTSR = MIN( SRC(1,M,NS)-TM,DT )
              TFSR = (TM-0.5D+0*DTSR-SRC(1,M-1,NS))/
     &          (SRC(1,M,NS)-SRC(1,M-1,NS))
              DO 80 N = 2,8+NSOLU
                SRX(N) = SRC(N,M-1,NS)+TFSR*(SRC(N,M,NS)-SRC(N,M-1,NS))
   80         CONTINUE
!
!---  Stair step the slug and pulse well sources  ---
!
              IF( ISRT(NS).GE.24 .AND. ISRT(NS).LE.27 ) THEN
                DO 82 N = 2,8+NSOLU
                  SRX(N) = SRC(N,M-1,NS)
   82           CONTINUE
              ENDIF
              GOTO 110
            ENDIF

  100     CONTINUE
          GOTO 700
        ENDIF
  110   CONTINUE



!
!---  Loop source domain  ---
!
!$OMP CRITICAL
          DO 500 I = ISRDM(1,NS),ISRDM(2,NS)
          DO 500 J = ISRDM(3,NS),ISRDM(4,NS)
          DO 500 K = ISRDM(5,NS),ISRDM(6,NS)
            N = ND(I,J,K)
            IZN = IZ(N)
            IF( IXP(N).EQ.0 ) GOTO 500
            DO 400 M = 2,ISVC+2
!
!---          Aqueous volumetric source  ---
!
              IF( ISRT(NS).EQ.2 ) THEN
                SRCW(M,N) = SRCW(M,N) + SRX(4)*RHOL(M,N)
!
!---          Aqueous volumetric density source  ---
!
              ELSEIF( ISRT(NS).EQ.3 ) THEN
                SRCW(M,N) = SRCW(M,N) + SRX(4)*RHOL(M,N)*VOL(N)
!
!---          Aqueous mass source  ---
!
              ELSEIF( ISRT(NS).EQ.4 ) THEN
                SRCW(M,N) = SRCW(M,N) + SRX(4)
!
!---          Aqueous mass density source  ---
!
              ELSEIF( ISRT(NS).EQ.5 ) THEN
                SRCW(M,N) = SRCW(M,N) + SRX(4)*VOL(N)
!
!---          Z-direction vertical injection well  ---
!
              ELSEIF( ISRT(NS).EQ.31 ) THEN
!
!---            Geometric factors  ---
!
                RDW = SRX(3)
                RDE = SQRT( AFZ(NSZ(N))/GPI/SRX(4) )
                ACWX = 2.D+0*GPI*RDW*DZGF(N)
                DRD2 = (RDE**2-RDW**2)
!
!---            Well pressure  ---
!
                IF( M.EQ.2 ) THEN
                  IF( K.EQ.ISRDM(5,NS) ) THEN
                    PLWX = SRX(2)
                    PX = PLWX+PATM
                    CALL WATLQD(T(2,N),PX,RHOLX)
                  ELSE
                    PX = PLWX+PATM
                    CALL WATLQD(T(2,N),PX,RHOLX)
                    NX = ND(I,J,K-1)
                    GB = (ZP(N)-ZP(NX))*GRAVZ
                    PLWX = PLWX - RHOLX*GB
                  ENDIF
                ENDIF
                IF( (PERM(1,IZN)/EPSL).GT.EPSL ) THEN
                  IF( (PERM(2,IZN)/EPSL).GT.EPSL ) THEN
                    PERMX = SQRT( PERM(1,IZN)*PERM(2,IZN) )
                  ELSE
                    PERMX = PERM(1,IZN)
                  ENDIF
                ELSE
                  PERMX = PERM(2,IZN)
                ENDIF
                HCLX = 2.D+0*GPI*PERMX*DRD2*DZGF(N)/
     &            (VISL(M,N)*((RDE**2)*LOG(RDE/RDW)-5.D-1*DRD2))
!
!---            Injection  ---
!
                IF( PLWX-PL(M,N).GT.EPSL ) THEN
                  QLX = SRX(4)*(PLWX-PL(M,N))*HCLX
                  SRCW(M,N) = SRCW(M,N) + QLX*RHOLX
!
!---              Withdrawl  ---
!
                ELSE
                  RKLX = SQRT(RKL(1,M,N)*RKL(2,M,N))
                  QLX = SRX(4)*(PLWX-PL(M,N))*RKLX*HCLX
                  SRCW(M,N) = SRCW(M,N) + QLX*RHOL(M,N)
                ENDIF
!
!---          X-direction horizontal injection well  ---
!
              ELSEIF( ISRT(NS).EQ.32 ) THEN
!
!---            Geometric factors  ---
!
                RDW = SRX(3)
                RDE = SQRT( AFX(NSX(N))/GPI/SRX(4) )
                ACWX = 2.D+0*GPI*RDW*DXGF(N)
                DRD2 = (RDE**2-RDW**2)
!
!---            Well pressure  ---
!
                IF( M.EQ.2 ) THEN
                  IF( I.EQ.ISRDM(1,NS) ) THEN
                    PLWX = SRX(2)
                    PX = PLWX+PATM
                    CALL WATLQD(T(2,N),PX,RHOLX)
                  ELSE
                    PX = PLWX+PATM
                    CALL WATLQD(T(2,N),PX,RHOLX)
                    NX = ND(I-1,J,K)
                    GB = (XP(N)-XP(NX))*GRAVX
                    PLWX = PLWX - RHOLX*GB
                  ENDIF
                ENDIF
                IF( (PERM(2,IZN)/EPSL).GT.EPSL ) THEN
                  IF( (PERM(3,IZN)/EPSL).GT.EPSL ) THEN
                    PERMX = SQRT( PERM(2,IZN)*PERM(3,IZN) )
                  ELSE
                    PERMX = PERM(2,IZN)
                  ENDIF
                ELSE
                  PERMX = PERM(3,IZN)
                ENDIF
                HCLX = 2.D+0*GPI*PERMX*DRD2*DXGF(N)/
     &            (VISL(M,N)*((RDE**2)*LOG(RDE/RDW)-5.D-1*DRD2))
!
!---            Injection  ---
!
                IF( PLWX-PL(M,N).GT.EPSL ) THEN
                  QLX = SRX(4)*(PLWX-PL(M,N))*HCLX
                  SRCW(M,N) = SRCW(M,N) + QLX*RHOLX
!
!---              Withdrawl  ---
!
                ELSE
                  RKLX = SQRT(RKL(2,M,N)*RKL(3,M,N))
                  QLX = SRX(4)*(PLWX-PL(M,N))*RKLX*HCLX
                  SRCW(M,N) = SRCW(M,N) + QLX*RHOL(M,N)
                ENDIF
!
!---          Y-direction horizontal injection well  ---
!
              ELSEIF( ISRT(NS).EQ.33 ) THEN
!
!---            Geometric factors  ---
!
                RDW = SRX(3)
                RDE = SQRT( AFY(NSY(N))/GPI/SRX(4) )
                ACWX = 2.D+0*GPI*RDW*DYGF(N)
                DRD2 = (RDE**2-RDW**2)
!
!---            Well pressure  ---
!
                IF( M.EQ.2 ) THEN
                  IF( J.EQ.ISRDM(3,NS) ) THEN
                    PLWX = SRX(2)
                    PX = PLWX+PATM
                    CALL WATLQD(T(2,N),PX,RHOLX)
                  ELSE
                    PX = PLWX+PATM
                    CALL WATLQD(T(2,N),PX,RHOLX)
                    NX = ND(I,J-1,K)
                    GB = (YP(N)-YP(NX))*RP(I)*GRAVY
                    PLWX = PLWX - RHOLX*GB
                  ENDIF
                ENDIF
                IF( (PERM(1,IZN)/EPSL).GT.EPSL ) THEN
                  IF( (PERM(3,IZN)/EPSL).GT.EPSL ) THEN
                    PERMX = SQRT( PERM(1,IZN)*PERM(3,IZN) )
                  ELSE
                    PERMX = PERM(1,IZN)
                  ENDIF
                ELSE
                  PERMX = PERM(3,IZN)
                ENDIF
                HCLX = 2.D+0*GPI*PERMX*DRD2*DYGF(N)/
     &            (VISL(M,N)*((RDE**2)*LOG(RDE/RDW)-5.D-1*DRD2))
!
!---            Injection  ---
!
                IF( PLWX-PL(M,N).GT.EPSL ) THEN
                  QLX = SRX(4)*(PLWX-PL(M,N))*HCLX
                  SRCW(M,N) = SRCW(M,N) + QLX*RHOLX
!
!---            Withdrawl  ---
!
                ELSE
                  RKLX = SQRT(RKL(1,M,N)*RKL(3,M,N))
                  QLX = SRX(4)*(PLWX-PL(M,N))*RKLX*HCLX
                  SRCW(M,N) = SRCW(M,N) + QLX*RHOL(M,N)
                ENDIF
              ENDIF
  400       CONTINUE
  500     CONTINUE
!$OMP END CRITICAL

  700 CONTINUE
!$OMP END PARALLEL DO
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of SORC1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SORIT1( NSL )
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

!----------------------Description-------------------------------------!
!
!     Water Mode
!
!     Compute solute transport source integrals.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOURC
      USE SOLTN
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
      REAL*8 SRX(8+LSOLU)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/SORIT1'
!
!---  Loop over sources  ---
!
      DO 600 NS = 1,NSR
        IF( TM.LE.SRC(1,1,NS) ) GOTO 600
        SRX(1) = TM
        IF( ISRM(NS).EQ.1 ) THEN
          DO 70 N = 2,8+NSOLU
            SRX(N) = SRC(N,1,NS)
   70     CONTINUE
        ELSE
          DO 100 M = 2,ISRM(NS)
            IF( TM.LE.SRC(1,M,NS) ) THEN
              DTSR = MIN( SRC(1,M,NS)-TM,DT )
              TFSR = (TM-0.5D+0*DTSR-SRC(1,M-1,NS))/
     &          (SRC(1,M,NS)-SRC(1,M-1,NS))
              DO 80 N = 2,8+NSOLU
                SRX(N) = SRC(N,M-1,NS)+TFSR*(SRC(N,M,NS)-SRC(N,M-1,NS))
   80         CONTINUE
             GOTO 110
            ENDIF
  100     CONTINUE
          GOTO 600
        ENDIF
  110   CONTINUE



!
!---  Loop over source domain  ---
!
          DO 500 I = ISRDM(1,NS),ISRDM(2,NS)
          DO 500 J = ISRDM(3,NS),ISRDM(4,NS)
          DO 500 K = ISRDM(5,NS),ISRDM(6,NS)
            N = ND(I,J,K)
            IF( IXP(N).EQ.0 ) GOTO 500
!
!---  Aqueous Volumetric Sink  ---
!
            IF( ISRT(NS).EQ.2 .AND. SRX(4).LT.0.D+0 ) THEN
              SRCIC(N,NSL) = SRCIC(N,NSL) - C(N,NSL)*SRX(4)*
     &          YL(N,NSL)*DT/(SL(2,N)*PORD(2,N))
!
!---  Aqueous Volumetric Sink  ---
!
            ELSEIF( ISRT(NS).EQ.3 .AND. SRX(4).LT.0.D+0 ) THEN
              SRCIC(N,NSL) = SRCIC(N,NSL) - C(N,NSL)*SRX(4)*VOL(N)*
     &          YL(N,NSL)*DT/(SL(2,N)*PORD(2,N))
!
!---  Aqueous Mass Sink  ---
!
            ELSEIF( ISRT(NS).EQ.4 .AND. SRX(4).LT.0.D+0 ) THEN
              SRCIC(N,NSL) = SRCIC(N,NSL) - C(N,NSL)*SRX(4)*
     &          YL(N,NSL)*DT/(SL(2,N)*PORD(2,N)*RHOL(2,N))
!
!---  Aqueous Mass Density Sink  ---
!
            ELSEIF( ISRT(NS).EQ.5 .AND. SRX(4).LT.0.D+0 ) THEN
              SRCIC(N,NSL) = SRCIC(N,NSL) - C(N,NSL)*SRX(4)*VOL(N)*
     &          YL(N,NSL)*DT/(SL(2,N)*PORD(2,N)*RHOL(2,N))
!
!---  Solute source  ---
!
            ELSEIF( ISRT(NS).EQ.-NSL .AND. NSL.LE.NSOLU ) THEN
              SRCIC(N,NSL) = SRCIC(N,NSL) + SRX(4)*DT
!
!---  Solute Density Source  ---
!
            ELSEIF( ISRT(NS).EQ.-(NSL+NSOLU) .AND. NSL.LE.NSOLU ) THEN
              SRCIC(N,NSL) = SRCIC(N,NSL) + SRX(4)*DT*VOL(N)
!
!---  Solute Inventory Source  ---
!
            ELSEIF( ISRT(NS).EQ.-(NSL+3*NSOLU) .AND. NSL.LE.NSOLU ) THEN
!
!---  Solute inventory lost through 1 decay
!     Solute inventory transported from the node  ---
!
              CIRDX = 6.931D-1*SRX(3)*DT/HLF(NSL)
              CITOX = -( WC(NSZ(N),NSL)*AFZ(NSZ(N)) +
     &          VC(NSY(N),NSL)*AFY(NSY(N)) +
     &          UC(NSX(N),NSL)*AFX(NSX(N)) -
     &          UC(NSX(N)+1,NSL)*AFX(NSX(N)+1) -
     &          VC(NSY(N)+IFLD,NSL)*AFY(NSY(N)+IFLD) -
     &          WC(NSZ(N)+IJFLD,NSL)*AFZ(NSZ(N)+IJFLD) )*DT
              CIRDX = MAX( CIRDX,0.D+0 )
              CITOX = MAX( CITOX,0.D+0 )
!
!---          If inventory remains, compute the inventory source
!             and inventory time derivative, reduce the inventory by the
!             amount of solute exiting the node and decaying, and
!             store the inventory in a field variable for writing  ---
!
              IF( SRC(3,1,NS).GT.EPSL ) THEN
!
!---            For the initial time step include the inventory needed
!               to charge the node with dissolved and sorbed solute  ---
!
                IF( SRX(5).LT.0.D+0 ) THEN
                  IF( SRX(3).LT.(SRX(4)*SL(2,N)*PORD(2,N)*VOL(N)/
     &              YL(N,NSL)) ) THEN
                    SRCIC(N,NSL) = SRCIC(N,NSL) + SRX(3)
                    SRC(3,1,NS) = 0.D+0
                  ELSE
                    SRC(5,1,NS) = 1.D+0
                    CIFSX = MAX( SRX(4)*SL(2,N)*PORD(2,N)/
     &                YL(N,NSL)-CO(N,NSL),0.D+0 )*VOL(N)
                    SRCIC(N,NSL) = SRCIC(N,NSL)+CIFSX+CITOX
                    SRC(3,1,NS) = SRC(3,1,NS)-CIFSX-CIRDX-CITOX
                  ENDIF
                ELSE
                  SRCIC(N,NSL) = SRCIC(N,NSL) + CITOX
                  SRC(3,1,NS) = SRC(3,1,NS)-CIRDX-CITOX
!
!---              For exhausted inventory reduce the dissolved and
!                 sorbed solute  ---
!
                  IF( SRC(3,1,NS).LT.0.D+0 ) THEN
                    C(N,NSL) = MAX((C(N,NSL)+SRC(3,1,NS)/VOL(N)),0.D+0)
                    SRC(3,1,NS) = 0.D+0
                  ENDIF
                ENDIF
                YN(N,NSL) = SRC(3,1,NS)
              ENDIF
!
!---        Advection-dominated solute release model  ---
!
            ELSEIF( ISRT(NS).EQ.-(NSL+4*NSOLU) .AND. NSL.LE.NSOLU ) THEN
              SRCIC(N,NSL) = SRCIC(N,NSL) + CNL(N,NSL)*DT
!
!---        Diffusion-dominated solute release model  ---
!
            ELSEIF( (ISRT(NS).EQ.-(NSL+5*NSOLU) .OR.
     &        ISRT(NS).EQ.-(NSL+8*NSOLU)) .AND. NSL.LE.NSOLU ) THEN
              SRCIC(N,NSL) = SRCIC(N,NSL) + CNL(N,NSL)*DT
!
!---        Solubility-controlled solute release model or
!           solubility-controlled salt-cake release model  ---
!
            ELSEIF( (ISRT(NS).EQ.-(NSL+6*NSOLU) .OR.
     &        ISRT(NS).EQ.-(NSL+7*NSOLU)) .AND. NSL.LE.NSOLU ) THEN
!
!---          SRX(2): nodal solute inventory  ---
!
              IF ( CNL(N,NSL).GT.ZERO ) THEN
!
!---            Integrated solute mass has exceeded solute inventory
!               and is being truncated to solute inventory  ---
!
                SRCIC(N,NSL) = SRCIC(N,NSL) + CNL(N,NSL)*DT
              ELSEIF ( SRCIC(N,NSL).LT.SRX(2) ) THEN
!
!---            Integrated solute mass still below solute inventory  ---
!
                NPX = NSX(N)
                NPY = NSY(N)
                NPZ = NSZ(N)
                NQX = NPX + 1
                NQY = NPY + IFLD
                NQZ = NPZ + IJFLD
                DCDT =  (C(N,NSL)-CO(N,NSL))*VOL(N)*DTI
                CLX = ZERO
                IF ( I.NE.1 ) THEN
                  CLX =  CLX + UC(NPX,NSL)*AFX(NPX)
                ENDIF
                IF ( I.NE.IFLD ) THEN
                  CLX =  CLX - UC(NQX,NSL)*AFX(NQX)
                ENDIF
                IF ( J.NE.1 ) THEN
                  CLX =  CLX + VC(NPY,NSL)*AFY(NPY)
                ENDIF
                IF ( J.NE.JFLD ) THEN
                  CLX =  CLX - VC(NQY,NSL)*AFY(NQY)
                ENDIF
                IF ( K.NE.1 ) THEN
                  CLX =  CLX + WC(NPZ,NSL)*AFZ(NPZ)
                ENDIF
                IF ( K.NE.KFLD ) THEN
                  CLX =  CLX - WC(NQZ,NSL)*AFZ(NQZ)
                ENDIF
                SRCIC(N,NSL) = SRCIC(N,NSL) + (DCDT-CLX)*DT
              ENDIF
            ENDIF
  500     CONTINUE

  600 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of SORIT1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SORT1( NSL )
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

!----------------------Description-------------------------------------!
!
!     Water Mode
!
!     Compute solute transport source terms.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE PORMED
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!





      REAL*8 SRX(8+LSOLU),VX(10)
      INTEGER IVX(10)
      EXTERNAL ADRM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/SORT1'
!
!---  Loop over sources  ---
!
      DO 600 NS = 1,NSR
!
!---  Check source time  ---
!
        IF( TM.LE.SRC(1,1,NS) ) GOTO 600
        SRX(1) = TM
        IF( ISRM(NS).EQ.1 ) THEN
          DO 70 N = 2,8+NSOLU
            SRX(N) = SRC(N,1,NS)
   70     CONTINUE
          DTMAX = TM-SRC(1,1,NS)
        ELSE
          DO 100 M = 2,ISRM(NS)
            IF( TM.LE.SRC(1,M,NS) ) THEN
              DTSR = MIN( SRC(1,M,NS)-TM,DT )
              TFSR = (TM-0.5D+0*DTSR-SRC(1,M-1,NS))/
     &          (SRC(1,M,NS)-SRC(1,M-1,NS))
              DO 80 N = 2,8+NSOLU
                SRX(N) = SRC(N,M-1,NS)+TFSR*(SRC(N,M,NS)-SRC(N,M-1,NS))
   80         CONTINUE
              DTMAX = TM-SRC(1,M-1,NS)
              GOTO 110
            ENDIF
  100     CONTINUE
          GOTO 600
        ENDIF
  110   CONTINUE

!
!---    Loop over source domain  ---
!
        DO 500 I = ISRDM(1,NS),ISRDM(2,NS)
        DO 500 J = ISRDM(3,NS),ISRDM(4,NS)
        DO 500 K = ISRDM(5,NS),ISRDM(6,NS)
          N = ND(I,J,K)
          MP = IXP(N)
          IF( ILES.EQ.1 ) THEN
            MCOL = MP
            MROW = MDT
          ELSEIF( ILES.EQ.3 .OR. ILES.EQ.4 ) THEN
            MA = 1
            MCOL = KLUC(MP,MA)
            MA = MA + 1




          ENDIF
          IADDVAL = 1
          SORTX = 0.D+0
!
!---      Aqueous Volumetric Sink  ---
!
          IF( ISRT(NS).EQ.2 .AND. SRX(4).LT.0.D+0 ) THEN
            SORTX = -SRX(4)*YL(N,NSL)/(PORD(2,N)*SL(2,N))
!
!---      Aqueous Volumetric Density Sink  ---
!
          ELSEIF( ISRT(NS).EQ.3 .AND. SRX(4).LT.0.D+0 ) THEN
            SORTX = -SRX(4)*YL(N,NSL)*VOL(N)/(SL(2,N)*PORD(2,N))
!
!---      Aqueous Mass Sink  ---
!
          ELSEIF( ISRT(NS).EQ.4 .AND. SRX(4).LT.0.D+0 ) THEN
            SORTX = -SRX(4)*YL(N,NSL)/(SL(2,N)*PORD(2,N)*RHOL(2,N))
!
!---      Aqueous Mass Density Sink  ---
!
          ELSEIF( ISRT(NS).EQ.5 .AND. SRX(4).LT.0.D+0 ) THEN
            SORTX = -SRX(4)*YL(N,NSL)*VOL(N)/
     &        (SL(2,N)*PORD(2,N)*RHOL(2,N))
!
!---      Solute Source  ---
!
          ELSEIF( ISRT(NS).EQ.-NSL .AND. NSL.LE.NSOLU ) THEN
            BLU(MP) = BLU(MP) + SRX(4)
!
!---      Solute Density Source  ---
!
          ELSEIF( ISRT(NS).EQ.-(NSL+NSOLU) .AND. NSL.LE.NSOLU ) THEN
            BLU(MP) = BLU(MP) + SRX(4)*VOL(N)
!
!---      Solute Inventory Source  ---
!
          ELSEIF( ISRT(NS).EQ.-(NSL+3*NSOLU) .AND. NSL.LE.NSOLU ) THEN
            IF( SRX(3).GT.EPSL ) THEN
              IF( SRX(3).LT.(SRX(4)*SL(2,N)*PORD(2,N)*VOL(N)/
     &          YL(N,NSL)) .AND. SRX(5).LT.0.D+0 ) THEN
                BLU(MP) = BLU(MP) + SRX(3)*DTI
              ELSE
                IF( ILES.EQ.1 ) THEN
                  ALU(MROW,MCOL) = 1.D+9*DTI*VOL(N)
                ELSEIF( ILES.EQ.3 .OR. ILES.EQ.4 ) THEN
                  DLU(MCOL) = 1.D+9*DTI*VOL(N)






                ENDIF
                BLU(MP) = SRX(4)*SL(2,N)*PORD(2,N)*1.D+9*DTI*VOL(N)
     &            /YL(N,NSL)
              ENDIF
            ENDIF
            IADDVAL = 0
!
!---      Advection-dominated solute release model  ---
!
          ELSEIF( ISRT(NS).EQ.-(NSL+4*NSOLU) .AND. NSL.LE.NSOLU ) THEN
!            NPX = NSX(N)
!            NPY = NSY(N)
            NPZ = NSZ(N)
!            NQX = NPX + 1
!            NQY = NPY + IFLD
            NQZ = NPZ + IJFLD
!            ULX = ABS(5.D-1*(UL(1,NQX)+UL(1,NPX)))
!            VLX = ABS(5.D-1*(VL(1,NQY)+VL(1,NPY)))
            WLX = ABS(5.D-1*(WL(1,NQZ)+WL(1,NPZ)))
            WLX = MAX( -WL(1,NPZ),ZERO )
!            FLX = SQRT((ULX**2)+(VLX**2)+(WLX**2))
            ALPHAX = WLX/(PORD(2,N)*SL(2,N)*SRX(4))
            VX(1) = ALPHAX
            VX(2) = SRX(3)
            IVX(1) = INT(SRX(5))
            CALL QROMB( ADRM,VX,ZERO,DTMAX,QSRIX,IERR,IVX )
            IF( IERR.NE.0 ) THEN
              INDX = 3
              CHMSG = 'Failed Romberg Integration: ' //
     &          'Advection-Dominated Release Model'
              CALL WRMSGS( INDX )
            ENDIF
            QSRIX = MIN( SRX(3),QSRIX )
!            QSRIX = SRX(3)*(1.D+0 - (EXP(-1.D+1*ALPHAX*DTMAX)/5.67D+2)*
!     &        ((1.56250D+5*((ALPHAX*DTMAX)**9)) +
!     &         (2.81250D+5*((ALPHAX*DTMAX)**8)) +
!     &         (3.37500D+5*((ALPHAX*DTMAX)**7)) +
!     &         (3.15000D+5*((ALPHAX*DTMAX)**6)) +
!     &         (2.36250D+5*((ALPHAX*DTMAX)**5)) +
!     &         (1.41750D+5*((ALPHAX*DTMAX)**4)) +
!     &         (6.6150D+4*((ALPHAX*DTMAX)**3)) +
!     &         (2.2680D+4*((ALPHAX*DTMAX)**2)) +
!     &         (5.103D+3*ALPHAX*DTMAX) + 5.67D+2))
            QSRRX = MAX( (QSRIX-SRCIC(N,NSL))*DTI,0.D+0 )
!
!---        Hold solute release rate in variable CNL  ---
!
            CNL(N,NSL) = QSRRX
            BLU(MP) = BLU(MP) + QSRRX
!
!---      Diffusion-dominated solute release model  ---
!
          ELSEIF( ISRT(NS).EQ.-(NSL+5*NSOLU) .AND. NSL.LE.NSOLU ) THEN
            QSRIX = 2.D+0*SRX(3)*SQRT(SRX(5)*DTMAX/GPI)/SRX(4)
            QSRIX = MIN( SRX(3),QSRIX )
            QSRRX = MAX( (QSRIX-SRCIC(N,NSL))*DTI,0.D+0 )
!
!---        Hold solute release rate in variable CNL  ---
!
            CNL(N,NSL) = QSRRX
            BLU(MP) = BLU(MP) + QSRRX
!
!---      Solubility-controlled solute release models or
!         solubility-controlled salt cake release models  ---
!
          ELSEIF( ISRT(NS).EQ.-(NSL+6*NSOLU) .OR.
     &            ISRT(NS).EQ.-(NSL+7*NSOLU) .AND. NSL.LE.NSOLU ) THEN
!
!---        SRX(2): nodal solute inventory  ---
!
            IF ( SRCIC(N,NSL).LT.SRX(2) ) THEN
!
!---          Forecast whether the cummulative mass dissolved will 
!             exceed the nodal inventory within the current time step.  
!             This is an approximation since the fluxes were computed  
!             at the previous time step.  However, the calculation 
!             does provide an estimate for minimizing the error 
!             due to overshooting the mass dissolved.  ---
!
              NPX = NSX(N)
              NPY = NSY(N)
              NPZ = NSZ(N)
              NQX = NPX + 1
              NQY = NPY + IFLD
              NQZ = NPZ + IJFLD
              DCDT =  (C(N,NSL)-CO(N,NSL))*VOL(N)*DTI
              CLX = ZERO
              IF ( I.NE.1 ) THEN
                CLX =  CLX + UC(NPX,NSL)*AFX(NPX)
              ENDIF
              IF ( I.NE.IFLD ) THEN
                CLX =  CLX - UC(NQX,NSL)*AFX(NQX)
              ENDIF
              IF ( J.NE.1 ) THEN
                CLX =  CLX + VC(NPY,NSL)*AFY(NPY)
              ENDIF
              IF ( J.NE.JFLD ) THEN
                CLX =  CLX - VC(NQY,NSL)*AFY(NQY)
              ENDIF
              IF ( K.NE.1 ) THEN
                CLX =  CLX + WC(NPZ,NSL)*AFZ(NPZ)
              ENDIF
              IF ( K.NE.KFLD ) THEN
                CLX =  CLX - WC(NQZ,NSL)*AFZ(NQZ)
              ENDIF
              CNL(N,NSL) = ZERO
              IF ( SRCIC(N,NSL)+(DCDT-CLX)*DT.GT.SRX(2) ) THEN
!
!---            If the forecast predicts overshoot, set the source mass
!               as the difference between the current cummulative mass
!               and the nodal inventory.  ---
!
                QSRRX = (SRX(2)-SRCIC(N,NSL))*DTI
                CNL(N,NSL) = QSRRX
                BLU(MP) = BLU(MP) + QSRRX
              ELSE
!
!---            Otherwise, set diagonal matrix entry to 1 and all 
!               off-diagonal to 0, to force the concentration to be the 
!               solubility limit.  ---
!
                IF( ILES.EQ.1 ) THEN
                  ALU(MROW,MCOL) = 1.D+0
                ELSEIF( ILES.EQ.3 .OR. ILES.EQ.4 ) THEN
                  DLU(MCOL) = 1.D+0






                ENDIF
                IF ( K.NE.1 ) THEN
                  IF( ILES.EQ.1 ) THEN
                    MCB = IXP(N-IJFLD)
                    MCOL = MCB
                    MROW = MP-MCB+MDT
                    ALU(MROW,MCOL) = 0.D+0
                  ELSEIF( ILES.EQ.3 .OR. ILES.EQ.4 ) THEN
                    MROW = KLUC(MP,MA)
                    DLU(MROW) = 0.D+0
                    MA = MA + 1







                  ENDIF
                ENDIF
                IF ( J.NE.1 ) THEN
                  IF( ILES.EQ.1 ) THEN
                    MCS = IXP(N-IFLD)
                    MCOL = MCS
                    MROW = MP-MCS+MDT
                    ALU(MROW,MCOL) = 0.D+0
                  ELSEIF( ILES.EQ.3 .OR. ILES.EQ.4 ) THEN
                    MROW = KLUC(MP,MA)
                    DLU(MROW) = 0.D+0
                    MA = MA + 1







                  ENDIF
                ENDIF
                IF ( I.NE.1 ) THEN
                  IF( ILES.EQ.1 ) THEN
                    MCW = IXP(N-1)
                    MCOL = MCW
                    MROW = MP-MCW+MDT
                    ALU(MROW,MCOL) = 0.D+0
                  ELSEIF( ILES.EQ.3 .OR. ILES.EQ.4 ) THEN
                    MROW = KLUC(MP,MA)
                    DLU(MROW) = 0.D+0
                    MA = MA + 1







                  ENDIF
                ENDIF
                IF ( I.NE.IFLD ) THEN
                  IF( ILES.EQ.1 ) THEN
                    MCE = IXP(N+1)
                    MCOL = MCE
                    MROW = MP-MCE+MDT
                    ALU(MROW,MCOL) = ZERO
                  ELSEIF( ILES.EQ.3 .OR. ILES.EQ.4 ) THEN
                    MROW = KLUC(MP,MA)
                    DLU(MROW) = 0.D+0
                    MA = MA + 1







                  ENDIF
                ENDIF
                IF ( J.NE.JFLD ) THEN
                  IF( ILES.EQ.1 ) THEN
                    MCN = IXP(N+IFLD)
                    MCOL = MCN
                    MROW = MP-MCN+MDT
                    ALU(MROW,MCOL) = 0.D+0
                  ELSEIF( ILES.EQ.3 .OR. ILES.EQ.4 ) THEN
                    MROW = KLUC(MP,MA)
                    DLU(MROW) = 0.D+0
                    MA = MA + 1







                  ENDIF
                ENDIF
                IF ( K.NE.KFLD ) THEN
                  IF( ILES.EQ.1 ) THEN
                    MCT = IXP(N+IJFLD)
                    MCOL = MCT
                    MROW = MP-MCT+MDT
                    ALU(MROW,MCOL) = 0.D+0
                  ELSEIF( ILES.EQ.3 .OR. ILES.EQ.4 ) THEN
                    MROW = KLUC(MP,MA)
                    DLU(MROW) = 0.D+0
                    MA = MA + 1







                  ENDIF
                ENDIF
!
!---            Solubility-controlled solute release model  ---
!
                IF( ISRT(NS).EQ.-(NSL+6*NSOLU)
     &            .AND. NSL.LE.NSOLU ) THEN
!
!---              SRX(2): nodal solute inventory
!                 SRX(3): aqueous solubility  ---
!
                  BLU(MP) = SRX(3)*SL(2,N)*PORD(2,N)/YL(N,NSL)
!
!---              Solubility-controlled salt cake release model  ---
!
                ELSEIF ( ISRT(NS).EQ.-(NSL+7*NSOLU) 
     &            .AND. NSL.LE.NSOLU ) THEN
!
!---              SRX(2): nodal solute inventory
!                 SRX(3): nodal salt cake inventory
!                 SRX(4): salt cake solubility  ---
!
                  BLU(MP) = SRX(4)*SRX(2)/SRX(3)*SL(2,N)*PORD(2,N)/
     &                    YL(N,NSL)
                ENDIF
              ENDIF
            ENDIF
            IADDVAL = 0
!
!---      Diffusion-dominated solute release model 
!         (w/variable diffusion)  ---
!
          ELSEIF( ISRT(NS).EQ.-(NSL+8*NSOLU) .AND. NSL.LE.NSOLU ) THEN
!
!---        SRX(3): nodal solute inventory
!           SRX(4): vertical depth of residual waste
!           SRX(5): diffusion coeficient
!           SRX(6): constrictivity  ---
!
            TORLX = TORL(2,N)
            DIFFIX = SRX(5)*PORD(2,N)*SRX(6)/(TORLX*TORLX)
            IZN = IZ(N)
            ALPHAX = PORD(2,N) + RHOS(IZN)*PCSL(1,IZN,NSL)*
     &                           (1.d0-PORT(2,N))
            DIFFAX = DIFFIX/ALPHAX
            QSRIX = 2.D+0*SRX(3)*SQRT(DIFFAX*DTMAX/GPI)/SRX(4)
            QSRIX = MIN( SRX(3),QSRIX )
            QSRRX = MAX( (QSRIX-SRCIC(N,NSL))*DTI,0.D+0 )
!
!---        Hold solute release rate in variable CNL  ---
!
            CNL(N,NSL) = QSRRX
            BLU(MP) = BLU(MP) + QSRRX
!
!---      In-Well Vapor-Stripping Solute Source  ---
!
          ELSEIF( ISRT(NS).EQ.-(NSL+2*NSOLU) .AND. NSL.LE.NSOLU ) THEN
!
!---        Loop over inlet well screen surface domain and compute
!           volumetric aqueous and solute fluxes, using outflow
!           logic for computing surface fluxes  ---
!
            QLVS = 0.D+0
            QCVS = 0.D+0
            DO 200 IVS = ISRDM(8,NS),ISRDM(9,NS)
            DO 200 JVS = ISRDM(10,NS),ISRDM(11,NS)
            DO 200 KVS = ISRDM(12,NS),ISRDM(13,NS)
              NVS = ND(IVS,JVS,KVS)
!
!---          West surface  ---
!
              IF( ISRDM(7,NS).EQ.-1 ) THEN
                NPX = NSX(NVS)
                QLX = MAX( -UL(1,NPX)*AFX(NPX),0.D+0 )
                QLVS = QLVS + QLX
                FCL = YL(NVS,NSL)/(SL(2,NVS)*PORD(2,NVS)+SMALL)
                QCVS = QCVS + QLX*FCL*C(NVS,NSL)
!
!---          East surface  ---
!
              ELSEIF( ISRDM(7,NS).EQ.1 ) THEN
                NQX = NSX(NVS)+1
                QLX = MAX( UL(1,NQX)*AFX(NQX),0.D+0 )
                QLVS = QLVS + QLX
                FCL = YL(NVS,NSL)/(SL(2,NVS)*PORD(2,NVS)+SMALL)
                QCVS = QCVS + QLX*FCL*C(NVS,NSL)
!
!---          South surface  ---
!
              ELSEIF( ISRDM(7,NS).EQ.-2 ) THEN
                NPY = NSY(NVS)
                QLY = MAX( -VL(1,NPY)*AFY(NPY),0.D+0 )
                QLVS = QLVS + QLY
                FCL = YL(NVS,NSL)/(SL(2,NVS)*PORD(2,NVS)+SMALL)
                QCVS = QCVS + QLY*FCL*C(NVS,NSL)
!
!---          North surface  ---
!
              ELSEIF( ISRDM(7,NS).EQ.2 ) THEN
                NQY = NSY(NVS)+IFLD
                QLY = MAX( VL(1,NQY)*AFY(NQY),0.D+0 )
                QLVS = QLVS + QLY
                FCL = YL(NVS,NSL)/(SL(2,NVS)*PORD(2,NVS)+SMALL)
                QCVS = QCVS + QLY*FCL*C(NVS,NSL)
!
!---          Bottom surface  ---
!
              ELSEIF( ISRDM(7,NS).EQ.-3 ) THEN
                NPZ = NSZ(NVS)
                QLZ = MAX( -WL(1,NPZ)*AFZ(NPZ),0.D+0 )
                QLVS = QLVS + QLZ
                FCL = YL(NVS,NSL)/(SL(2,NVS)*PORD(2,NVS)+SMALL)
                QCVS = QCVS + QLZ*FCL*C(NVS,NSL)
!
!---          Top surface  ---
!
              ELSEIF( ISRDM(7,NS).EQ.3 ) THEN
                NQZ = NSZ(NVS)+IJFLD
                QLZ = MAX( WL(1,NQZ)*AFZ(NQZ),0.D+0 )
                QLVS = QLVS + QLZ
                FCL = YL(NVS,NSL)/(SL(2,NVS)*PORD(2,NVS)+SMALL)
                QCVS = QCVS + QLZ*FCL*C(NVS,NSL)
              ENDIF
  200       CONTINUE
            CALL WATSP( SRX(6),PSWX )
            CALL WATLQD( SRX(6),SRX(2),RHOLX )
            PVAX = MAX( SRX(2)-PSWX,0.D+0 )
            INDX = 0
            CALL WATGSD( SRX(6),PSWX,RHOW,INDX )
            CALL AIRGSD( SRX(6),PVAX,RHOA )
            RHOGX = RHOW+RHOA
            WTMG = PVAX*WTMA/SRX(2) + PSWX*WTMW/SRX(2)
            IF( SRX(3).GT.1.D-6 ) THEN
              XLOX = QCVS/(QLVS*(SRX(7)*SRX(4)*RHOGX*SRX(3)/
     &          (SRX(2)*WTMG)+RHOLX/WTMW)+SMALL)
            ELSE
              XLOX = QCVS/(QLVS*SRX(7)*RHOLX/WTMW+SMALL)
            ENDIF
            BLU(MP) = BLU(MP)+XLOX*RHOLX*QLVS*VOL(N)/(SRX(5)*WTMW)
          ENDIF
!
!---      Load Jacobian  ---
!
          IF( IADDVAL.EQ.1 ) THEN
            IF( ILES.EQ.1 ) THEN
              ALU(MROW,MCOL) = ALU(MROW,MCOL) + SORTX
            ELSEIF( ILES.EQ.3 .OR. ILES.EQ.4 ) THEN
              DLU(MCOL) = DLU(MCOL) + SORTX





            ENDIF
          ENDIF
  500   CONTINUE

  600 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of SORT1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SPRP1( NSL )
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
!     Water Mode
!
!     Calculates the aqueous-phase solute
!     mole fractions from user-specified partition coefficients.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
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
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/SPRP1'
!
!---  Loop over all nodes  ---
!
      DO 100 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 100
        IZN = IZ(N)
        XVL = SL(2,N)*PORD(2,N)
        IF( IPCL(NSL).EQ.4 ) THEN
          NS = IPCSL(IZN,NSL)
          XVS = SL(2,N)*RHOS(IZN)*PCSL(1,IZN,NS)*(1.D+0-PORT(2,N))
          CLX = C(N,NS)/(XVS+XVL)
          IF( CLX.LT.SMALL ) THEN
            PCSLX = PCSL(1,IZN,NSL)
          ELSE
            PCSLX = 1.D+1**(PCSL(2,IZN,NSL)+PCSL(3,IZN,NSL)*LOG10(CLX))
          ENDIF
          XVS = RHOS(IZN)*PCSLX*(1.D+0-PORT(2,N))*SL(2,N)
        ELSEIF( IPCL(NSL).EQ.3 ) THEN
          NS = IPCSL(IZN,NSL)
          XVS = SL(2,N)*RHOS(IZN)*PCSL(1,IZN,NS)*(1.D+0-PORT(2,N))
          CLX = C(N,NS)/(XVS+XVL)
          IF( CLX.LT.SMALL ) THEN
            PCSLX = PCSL(1,IZN,NSL)
          ELSE
            PCSLX = 1.D+1**(PCSL(2,IZN,NSL)+PCSL(3,IZN,NSL)*LOG10(CLX))
          ENDIF
          XVS = RHOS(IZN)*PCSLX*(1.D+0-PORT(2,N))
        ELSEIF( IPCL(NSL).EQ.2 ) THEN
          XVS = RHOS(IZN)*PCSL(1,IZN,NSL)*(1.D+0-PORT(2,N))*SL(2,N)
        ELSE
          XVS = RHOS(IZN)*PCSL(1,IZN,NSL)*(1.D+0-PORT(2,N))
        ENDIF
!
!---  Phase-volumetric concentration ratios  ---
!
        YVL = 1.D+0/(XVS + XVL)
!
!---  Phase mole fractions  ---
!
        YL(N,NSL) = XVL*YVL
!
  100 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of SPRP1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE TMPR1
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
!     Water Mode
!
!     Control thermodynamic property calculations for the following:
!     Aqueous-Phase Properties
!       liquid density, liquid viscosity
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE JACOB
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
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/TMPR1'
!$OMP PARALLEL DO
!$OMP&  COPYIN(ISUB_LOG)
!$OMP&  DEFAULT(NONE)
!$OMP&  SHARED(NFLD,IXP,PG,PATM,ISVC,PL,T,PVW,RHOL,PSW,VISL,XLW,ISLC,
!$OMP&    C,NSL_ELC,YL,SL,PORD,SMALL,ELC_DCF,ELC_VCF) 
!$OMP&  PRIVATE(PGX,PLX,PX,CLX)
      DO 400 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 400
        PGX = PG(2,N) + PATM
        DO 300 M = 2,ISVC+2
          PLX = PL(M,N) + PATM
          CALL WATSP( T(2,N),PVW(M,N) )
          PX = MAX( PLX,PGX,PVW(M,N) )
          CALL WATLQD( T(2,N),PX,RHOL(M,N) )
          CALL WATLQV( T(2,N),PX,PSW(2,N),VISL(M,N) )
          XLW(M,N) = 1.D+0
!
!---  Correct aqueous liquid density and viscosity for electrolyte
!     solute concentration  ---
!
          IF( ISLC(16).EQ.1 ) THEN
            CLX = C(N,NSL_ELC)*YL(N,NSL_ELC)/(SL(M,N)*PORD(M,N)+SMALL)
            XLW(M,N) = RHOL(M,N)
            CALL ELC_DEN( RHOL(M,N),CLX,ELC_DCF )
            XLW(M,N) = XLW(M,N)/RHOL(M,N)
            CALL ELC_VIS( VISL(M,N),CLX,ELC_VCF )
          ENDIF
  300   CONTINUE
  400 CONTINUE
!$OMP END PARALLEL DO
      ISUB_LOG = ISUB_LOG-1
!
!---  End of TMPR1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE TPORT1( NSL )
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
!     Water Mode
!
!     Solute/Reactive Species Transport Shell.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 13 September 2005.



!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE JACOB



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
      SUB_LOG(ISUB_LOG) = '/TPORT1'
!
!---  Zero Jacobian matrix  ---
!



      CALL JCBZ( ISVT,MUT,MLT,MKT )
!
!---  Compute solute sources ---
!
      CALL SORT1( NSL )
!
!---  Zero solute transport fluxes  ---
!
      CALL SFXZ( NSL )
!
!---  Load Jacobian matrix ---
!
      CALL SJCBL( NSL )
!
!---  Modify Jacobian matrix for boundary conditions ---
!
      CALL SBND1( NSL )
!
!---  Linear equation solver  ---
!
      IF( ILES.EQ.1 ) THEN
        INDX = 1
        CALL BAND( 0,MUT,MLT,INDX )
      ELSEIF( ILES.EQ.3 ) THEN
        INDX = 1
        CALL PSPLIB( 0,INDX )

      ENDIF
!
!---  Update solute concentrations ---
!
      CALL UPDTC( NSL )
!
!---  Compute solute aqueous-phase fluxes (interior nodes)  ---
!
      CALL SFXL( NSL )
!
!---  Compute solute aqueous-phase fluxes (boundary surfaces)  ---
!
      CALL SFXLB( NSL )
!
!---  Integrate solute sources  ---
!
      CALL SORIT1( NSL )






      ISUB_LOG = ISUB_LOG-1
!
!---  End of TPORT1 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE TRPGL1
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
!     Water Mode
!
!     Compute the total trapping number for gas entrapment in the
!     aqueous phase.
!
!     Pennell, K.D., G.A. Pope, L.M. Abriola.  1996.
!     "Influence of Viscous and Buoyancy Forces on the Mobilization
!     of Residual Tetrachloroethylene during Surfactant Flushing."
!     Environ. Sci. Technol.  30(4):1328-1335.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle's Pacific Northwest Division, 1997.
!     Last Modified by MD White on December 18, 1997.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
      USE GRID
      USE FLUXP
      USE FDVP
      USE FDVD
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
      SUB_LOG(ISUB_LOG) = '/TRPGL1'
      DO 100 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 100
        NPX = NSX(N)
        NPY = NSY(N)
        NPZ = NSZ(N)
        NQX = NSX(N)+1
        NQY = NSY(N)+IFLD
        NQZ = NSZ(N)+IJFLD
        IZN = IZ(N)
        DFMLX = SQRT( ABS(UL(1,NPX))*ABS(UL(1,NQX)) +
     &    ABS(VL(1,NPY))*ABS(VL(1,NQY)) +
     &    ABS(WL(1,NPZ))*ABS(WL(1,NQZ)))
        ULX = 5.D-1*(UL(1,NPX)+UL(1,NQX))
        VLX = 5.D-1*(VL(1,NPY)+VL(1,NQY))
        WLX = 5.D-1*(WL(1,NPZ)+WL(1,NQZ))
        ULGX = 5.D-1*(UL(1,NPX)*GRVX(NPX)+UL(1,NQX)*GRVX(NQX))
        VLGX = 5.D-1*(VL(1,NPY)*GRVY(NPY)+VL(1,NQY)*GRVY(NQY))
        WLGX = 5.D-1*(WL(1,NPZ)*GRVZ(NPZ)+WL(1,NQZ)*GRVZ(NQZ))
        DFALX = ((ULGX + VLGX + WLGX)/GRAV)/
     &    ( SQRT( ULX**2 + VLX**2 + WLX**2 ) + SMALL )
        SKL = SQRT((PERM(1,IZN)*(5.D-1*(GRVX(NPX)+GRVX(NQX))))**2 + 
     &    (PERM(2,IZN)*5.D-1*(GRVY(NPY)+GRVY(NQY)))**2 +
     &    (PERM(3,IZN)*5.D-1*(GRVZ(NPZ)+GRVZ(NQZ)))**2)/GRAV
        RKLMX = (RKL(1,2,N)*RKL(2,2,N)*RKL(3,2,N))**(1./3.)
        BNDX = (RHOL(2,N)-RHORG)*GRAV*SKL*RKLMX
        CAPX = DFMLX*VISL(2,N)
        TRPGL(2,N) = SQRT(CAPX**2 + 2.D+0*CAPX*BNDX*DFALX + BNDX**2)
  100 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of TRPGL1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE UPDT1
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








!
!----------------------Description-------------------------------------!
!
!     Water Mode
!
!     Update the primary variables.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.




!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOURC
      USE SOLTN
      USE PORMED
      USE OUTPU
      USE JACOB
      USE HYST
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
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/UPDT1'
      IF( ICNV.EQ.1 ) GOTO 300
      IERR = 0
!
!---  Compute maximum residuals and update primary variables
!
!$OMP PARALLEL DO
!$OMP&  COPYIN(ISUB_LOG)
!$OMP&  DEFAULT(NONE)
!$OMP&  SHARED(NFLD,IXP,IZ,IM,IEQW,ISCHR,SCHR,RHORL,GRAV,SL,PG,PL,SGT,
!$OMP&    IPH,BLU,RLXF,ZERO,PMX,PATM,IERR,NSD)
!$OMP&  PRIVATE(IZN,NMD,MPL,PAE,SLX,CPGLO,CPGL,DPL)
      DO 200 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 200
        IZN = IZ(N)
        NMD = IXP(N)
        MPL = IM(IEQW,NMD)
        PAE = 0.D+0
        IF( MOD(ISCHR(IZN),10).EQ.2 ) PAE = SCHR(1,IZN)*RHORL*GRAV
!
!---  Limit aqueous pressure updates to changes in aqueous
!     saturation of 0.125 for unsaturated conditions   ---
!
        SLX = SL(2,N)-SIGN(0.125D+0,(SL(2,N)-0.5D+0))
        IF( (ISCHR(IZN).NE.301) .AND. (ISCHR(IZN).NE.302) .AND.
     &    (ISCHR(IZN).NE.3) .AND. (ISCHR(IZN).NE.4) .AND.
     &    (PG(2,N)-PL(2,N)-PAE.GT.0.D+0) ) THEN
          CPGLO = PG(1,N)-PL(1,N)
          CALL CAP1( IZN,SLX,SGT(2,N),CPGL,SL(1,N),CPGLO,IPH(2,N) )
          DPL = ABS( CPGL-PG(2,N)+PL(2,N) )
          DPL = MIN( DPL,ABS(BLU(MPL)) )
          DPL = SIGN( DPL,BLU(MPL) )*RLXF
        ELSE
          DPL = BLU(MPL)*RLXF
        ENDIF
!
!---  Relax aqueous pressure updates when transitioning from
!     unsaturated to saturated states   ---
!
        IF( PG(2,N)-PL(2,N)-PAE.GT.ZERO .AND.
     &      PG(2,N)-PL(2,N)-PAE-DPL.LT.ZERO ) DPL = 6.D-1*DPL
        PL(2,N) = PL(2,N) + DPL
!
!---  Check for excessive aqueous pressure  ---
!
        IF( PL(2,N).GT.PMX-PATM ) THEN
          IERR = 1
          NSD(1) = N
!         GOTO 300
        ENDIF
  200 CONTINUE
!$OMP END PARALLEL DO
      IF( IERR.EQ.1 ) THEN
          ICNV = 1
          N = NSD(1)
          WRITE(ISC,'(10X,A)')'---  Primary Variable(s) Error  ---'
          WRITE(IWR,'(10X,A)')'---  Primary Variable(s) Error  ---'
          WRITE(ISC,'(4X,A,1PE12.5,A,I6)')
     &      'Water Pressure = ',PL(2,N)+PATM,' Node = ',N
          WRITE(IWR,'(4X,A,1PE12.5,A,I6)')
     &      'Water Pressure = ',PL(2,N)+PATM,' Node = ',N
      ENDIF
!
!---  Reduce time step  ---
!
  300 CONTINUE
      IF( ICNV.EQ.1 ) THEN



        IF( NTSR.LT.4 .OR. (DTCF*DT).GT.DTMN ) THEN

          NTSR = NTSR + 1
          DTX = DT
          TM = TM - (1.D+0-DTCF)*DT
          DT = DTCF*DT
          DTO = DT
          DTI = 1.D+0/DT
          VAR = DT
          VARX = DTX
          IF( UNTM.NE.'null' ) THEN
            INDX = 1
            IUNS = 1
            CALL RDUNIT(UNTM,VAR,INDX)
            INDX = 1
            IUNS = 1
            CALL RDUNIT(UNTM,VARX,INDX)
            NCH = INDEX( UNTM,'  ')-1
          ENDIF
          WRITE(ISC,'(4X,A,1PE11.4,1X,2A,1PE11.4,1X,A)')
     &      'Time Step Reduced From ',VARX,UNTM(1:NCH),' to ',
     &      VAR,UNTM(1:NCH)
          WRITE(IWR,'(4X,A,1PE11.4,1X,2A,1PE11.4,1X,A)')
     &      'Time Step Reduced From ',VARX,UNTM(1:NCH),' to ',
     &      VAR,UNTM(1:NCH)
          DO 400 N = 1,NFLD
            PL(2,N) = PL(1,N)
            IXP(N) = ABS(IXP(N))
            NPHAZ(2,N) = NPHAZ(1,N)
            IPH(2,N) = IPH(1,N)
  400     CONTINUE





!
!---  Number of time step reductions failure: stop simulation  ---
!
        ELSE
          DO 410 N = 1,NFLD
            PL(2,N) = PL(1,N)
            IXP(N) = ABS(IXP(N))
            NPHAZ(2,N) = NPHAZ(1,N)
            IPH(2,N) = IPH(1,N)
  410     CONTINUE





          WRITE(ISC,'(10X,A)') '---  Time Step Reduction Limit Exceeded
     & ---'
          WRITE(IWR,'(10X,A)') '---  Time Step Reduction Limit Exceeded
     & ---'
          ICNV = 4






        ENDIF
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of UPDT1 group
!
      RETURN
      END


