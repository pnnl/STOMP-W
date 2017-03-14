!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDSOLU
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
!     Read input file for solution control information.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL November 1992.
!
!     Solution Control Variable Definitions
!
!     ISLC(1):   0    Patankar Solute Transport
!                1    Leonard-TVD Solute Transport
!                2    Roe's Superbee Solute Transport
!                3    Upwind Solute Transport
!     ISLC(2):   0    Zero Vapor Diffusion
!                1    Constant Vapor Diffusion Coefficient
!                2    Variable Vapor Diffusion Coefficient
!                3    Enhanced Vapor Diffusion Coefficient
!                4    Enhanced Vapor Diffusion Coefficient (Thermal)
!     ISLC(3):   0    Zero Tortuosity
!                1    Variable Tortuosity
!     ISLC(4):   0    Zero Aqueous-Phase Diffusion
!                1    Constant Aqueous-Phase Diffusion Coefficient
!                2    Variable Aqueous-Phase Diffusion Coefficient
!     ISLC(5):   0    No Soil Freezing
!                1    Soil Freezing Conditions
!     ISLC(6):   0    Patankar Salt Transport
!                1    Leonard Flux Limiter (TVD) Salt Transport
!     ISLC(7):   0    Without Osmotic Pressure Effects
!                1    With Osmotic Pressure Effects
!                2    With Osmotic Pressure and Surface Tension Effects
!                3    With Surface Tension Effects
!     ISLC(8):   0    Patankar Oil Transport
!                1    Leonard-TVD Oil Transport
!                2    Roe's Superbee Oil Transport
!                3    Upwind Oil Transport
!     ISLC(9):   0    Variable Fluid Density and Viscosity
!                1    Fixed Fluid Density and Viscosity
!     ISLC(10):  0    Patankar Dissolved Air Transport
!                1    Leonard Flux Limiter (TVD) Dissolved Air Transport
!     ISLC(11):  0    Single Porosity Model
!                1    Dual Porosity Model
!                2    Equivalent Continuum Model
!     ISLC(12):  0    Diffusive Dissolved-Oil Transport
!                1    Diffusive-Dispersive Dissolved-Oil Transport
!     ISLC(13):  0    No Particle-Displacing Bubbles
!                1    Particle-Displacing Bubbles
!     ISLC(14):  0    No SPLIB summary output
!                1    SPLIB summary output
!     ISLC(15):  0    Bulk compressibility w/ fixed bulk volume model
!                1    Pore compressibility w/ fixed bulk volume model
!                10   Bulk compressibility w/ variable bulk volume model
!                11   Pore compressibility w/ variable bulk volume model
!     ISLC(16):  0    No Density Dependent Solute Transport
!                1    Density Dependent Solute Transport
!     ISLC(17):  0    No Courant Number Control
!                1    Courant Number Control
!                2    Special Vadose Zone Courant Number Control
!                3    Aqueous-Only Courant Number Control
!     ISLC(18):  0    Intermediate Restart Files with Plot Files
!                1    No Intermediate Restart Files
!                2    No Restart Files
!     ISLC(19):  0    No Scaling Factors
!                1    Scaling Factors
!     ISLC(20):  0    No Inverse (UCode)
!                1    Inverse (UCode)
!     ISLC(21):  0    Restart: Current Operational Mode
!                1    Restart: Water Operational Mode
!                2    Restart: Water-Air Operational Mode
!                3    Restart: Water-Air-Energy Operational Mode
!                4    Restart: Water-Oil Operational Mode
!                5    Restart: Water-Oil-Air Operational Mode
!                6    Restart: Water-Oil-Air-Energy Operational Mode
!                7    Restart: Water-Oil-Alcohol Operational Mode
!               11    Restart: Water-Salt Operational Mode
!               12    Restart: Water-Air-Salt Operational Mode
!               13    Restart: Water-Air-Salt-Energy Operational Mode
!     ISLC(22):  0    Default Salt Functions
!                1    Seawater density function
!     ISLC(23):  0    Advective Solute Transport
!                1    No Aqueous Advective Solute Transport
!               10    No Gaseous Advective Solute Transport
!              100    No NAPL Advective Solute Transport
!               11    No Aqueous-Gaseous Solute Transport
!              101    No Aqueous-NAPL Advective Solute Transport
!              110    No Gaseous-NAPL Advective Solute Transport
!              111    No Advective Solute Transport
!     ISLC(24):  0    No Plants
!                1    Single Temperature Plants
!                2    Multiple Temperature Plants
!               10    No Plants (w/ Time-Lag Scheme)
!               11    Single Temperature Plants (w/ Time-Lag Scheme)
!               12    Multiple Temperature Plants (w/ Time-Lag Scheme)
!     ISLC(25):  0    No Poynting Effect (CO2 Solubility)
!                1    Poynting Effect (CO2 Solubility)
!                2    Fractional CO2 Solubility in Aqueous
!     ISLC(26):  0    No Rainfall Interception by Plants
!                1    Rainfall Interception by Plants
!     ISLC(27):  0    Aqueous Molar Density Gradient Diffusion
!                1    Aqueous Mole Fraction Gradient Diffusion
!     ISLC(28):  0    Gas Molar Density Gradient Diffusion
!                1    Gas Mole Fraction Gradient Diffusion
!     ISLC(29):  0    NAPL Molar Density Gradient Diffusion
!                1    NAPL Mole Fraction Gradient Diffusion
!     ISLC(30):  0    Nonisothermal
!                1    Isothermal
!     ISLC(31):  0    Equilibrium Hydrate Saturation
!                1    First-Order Kinetic Hydrate Saturation
!     ISLC(32):  0    Nonisobrine
!                1    Isobrine
!     ISLC(33)   0    Conventional Node Connections
!                1    Block Refinement Node Connections
!     ISLC(34)   0    No Vector or Matrix Output
!                1    Coupled-Flow Problem Vector
!                2    Coupled-Flow Matrix
!                3    Coupled-Flow Solution Vector
!               11    Transport Problem Vector
!               12    Transport Matrix
!               13    Transport Solution Vector
!               21    Geomechanics Problem Vector
!               22    Geomechanics Matrix
!               23    Geomechanics Solution Vector
!     ISLC(40):  0    No Reactive Transport
!                1    ECKEChem Reactive Transport
!     ISLC(41):  0    No hydrate inhibitor effects
!                1    Hydrate inhibitor effects
!     ISLC(42):  0    No initial species concentration guessing
!                1    Initial species concentration guessing
!     ISLC(43):  0    No porosity alteration with precipitation
!                1    Porosity alteration with precipitation
!     ISLC(44):  0    Invoke vapor pressure lowering
!                1    No vapor pressure lowering
!     ISLC(45):  0    CO2 Solution 
!                1    No CO2 Solution (Iso-CO2)
!     ISLC(46):  0    No Kinetic Volatilization
!                1    Kinetic Volatilization w/ Wilkins Model
!                2    Kinetic Volatilization w/ Van der Ham Model
!                3    Kinetic Volatilization w/ Yoon Model
!     ISLC(47):  0    Flow Calculation 
!                1    No Flow Calculation
!     ISLC(48):  0
!                1
!     ISLC(49):  0    No NAPL Surface Spill 
!                1    NAPL Surface Spill
!     ISLC(50):  0    No Geomechanics 
!                1    Geomechanics
!                2    Geomechanics w/o Poroelasticity
!                3    Geomechanics w/o Thermoelasticity
!                4    Geomechanics w/o Poro or Thermoelasticity
!                5    Geomechanics w/o Coupled Flow Solution 
!     ISLC(51):  0    No Fuel-Cell Well 
!                1    Fuel-Cell Well
!     ISLC(52):  0    Hydraulic dispersion coefficient w/ flow velocity
!                1    Hydraulic dispersion coefficient w/ flux velocity
!     ISLC(53):  0    Variable aqueous density
!                1    Constant aqueous density
!     ISLC(54):  0    Default supersaturation factor
!                1    Specified supersaturation factor
!     ISLC(55):  0    No electrolyte surface tension effects
!                1    Electrolyte surface tension effects
!     ISLC(56):  0    No effective mineral reaction area
!                1    Effective mineral reaction area scaled by 
!                     water saturation
!                2    No effective mineral reaction area,
!                     surface area held constant at initial value
!     ISLC(57):  0    No reduced equilibrium equations (eckechem)
!                1    Reduced equilibrium equations (eckechem)
!     ISLC(58):  0    No mixing coefficient provided in file read 
!                     for TST formulation (eckechem)
!                1    Mixing coefficient provided in file read 
!                     for TST formulation (eckechem)
!     ISLC(59):  0    Use default value for minimum concentration 
!                     (1.D-30) in eckechem
!                1    Provide minimum concentration value for eckechem
!     ISLC(60):  0    Use default non-log formulation for solving 
!                     chemistry in eckechem
!                1    Use log formulation for solving chemistry 
!                     in eckechem
!     ISLC(61):  0    Use system pressure for the reference 
!                     pressure for compressibility
!                1    Use reference pressure from restart file for 
!                     the reference pressure for compressibility
!     ISLC(62):  0    Standard convergence-failure output
!                1    Extended convergence-failure output
!     ISLC(63):  0    Standard plot grid
!                1    3D plot grid
!     ISLC(64):  0    CH4 Solution 
!                1    No CH4 Solution (Iso-CH4)
!     ISLC(65):  0    N2 Solution 
!                1    No N2 Solution (Iso-N2)
!     ISLC(66):  0    Zero Nonaqueous-Liquid Diffusion
!                1    Constant Nonaqueous-Liquid Diffusion Coefficient
!                2    Variable Nonaqueous-Liquid Diffusion Coefficient
!     ISLC(67):  0    H2O Solution 
!                1    No H2O Solution (Iso-H2O)
!     ISLC(68):  1    Peng-Robinson Equation of State
!                2    Soave-Redlich-Kwong Equation of State
!     ISLC(69):  0    Default binary interaction parameters 
!                1    Zero binary interaction parameters 
!     ISLC(70):  0    Standard Well Model
!                1    Enhanced Well Model 
!     ISLC(71):  0    No Lis time reporting
!                1    Lis time reporting
!     ISLC(72):  2    Two Gauss Sample Points
!                3    Three Gauss Sample Points
!                4    Four Gauss Sample Points
!                5    Five Gauss Sample Points
!     ISLC(73):  0    Vapor Pressure Differencing for Hydrate Exchange
!                1    Mole Fraction Differencing for Hydrate Exchange
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE NCG_PT
      USE NAPL
      USE JACOB
      USE GEOMECH
      USE FLUXS
      USE FILES
      USE FDVH
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



      PARAMETER( LCV=20,LCS=8 )



!
!----------------------Type Declarations-------------------------------!
!
      INTEGER NCV(LCV),NCS(LCS)
      CHARACTER*64 UNTS,CHIFV(LCV),CHIFS(LCS)
      CHARACTER*128 BDUM,FDUM
      CHARACTER*384 ADUM
      CHARACTER*512 CHDUM
      CHARACTER*6 FORM
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Data Statements---------------------------------!
!
      SAVE CHIFS,CHIFV,NCV,NCS,FORM
      DATA CHIFS /'harmonic','geometric','arithmetic','upwind',
     &            'downstream','moderated upwind',
     &            'neiber downstream','null'/
      DATA CHIFV /'thermal conductivity','aqueous density',
     &            'gas density','napl density','aqueous viscosity',
     &            'gas viscosity','napl viscosity',
     &            'aqueous relative permeability',
     &            'gas relative permeability',
     &            'napl relative permeability',
     &            'intrinsic permeability','gas diffusion',
     &            'gas diffusion','aqueous diffusion',
     &            'aqueous diffusion','solute diffusion',
     &            'hydraulic dispersion','salt aqueous diffusion',
     &            'napl diffusion','napl diffusion'/
      DATA NCV /20,15,11,12,17,13,14,29,25,26,22,13,13,17,17,
     &          16,20,22,14,14/
      DATA NCS /8,9,10,6,10,4,17,4/
      DATA FORM / '(I6,$)' /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDSOLU'
      IF( INDEX(SVN_ID(161)(1:1),'$').EQ.0 ) SVN_ID(161) =
     & '$Id: rdsolu.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Write card information to ouput file  ---
!
      CARD = 'Solution Control Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read Execution Option  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      WRITE(IWR,'(/,A,$)') 'Execution Option: '
      VARB = 'Execution Option'
      ISTART = 1
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      IF( INDEX(ADUM(1:),'normal').NE.0 ) THEN
        IEO = 1
        IF( INDEX(ADUM(1:),'no flow').NE.0 ) THEN
          ISLC(47) = 1
          WRITE(IWR,'(A)') 'Normal'
          WRITE(IWR,'(A)') '  w/ No Flow'
          WRITE(IWR,'(A)') '  w/ Static Domain'
        ELSE
          WRITE(IWR,'(A)') '  Normal'
          WRITE(IWR,'(A)') '  w/ Static Domain'
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'restart').NE.0 ) THEN
        IEO = 2
        ISLC(61) = 1
        IF( INDEX(ADUM(1:),'no flow').NE.0 ) THEN
          ISLC(47) = 1
          WRITE(IWR,'(A)') 'Restart'
          WRITE(IWR,'(A)') '  w/ No Flow'
          WRITE(IWR,'(A)') '  w/ Static Domain'
        ELSE
          WRITE(IWR,'(A)') 'Restart'
          WRITE(IWR,'(A)') '  w/ Static Domain'
        ENDIF
        IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
          IDFLT = 1
          FDUM = FNRS
          VARB = 'Restart File Name'
          CALL RDCHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
          WRITE(IWR,'(2X,3A)') VARB(1:IVR),': ',FDUM(1:NCHF)
          FNRS = FDUM(1:NCHF)
        ENDIF
        IF( INDEX(ADUM(1:),'mode').NE.0 ) THEN
          VARB = 'Restart File Operational Mode'
          CALL RDINT(ISTART,ICOMMA,CHDUM,ISLC(21))
          WRITE(IWR,'(2X,2A,I3)') VARB(1:IVR),': ',ISLC(21)
        ENDIF
        IF( INDEX(ADUM(1:),'reset compress').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') 'Reset Reference Pressure for ' //
     &      'Compressibility'
          ISLC(61) = 0
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'initial').NE.0 ) THEN
        IEO = 3
        WRITE(IWR,'(A)') 'Initial Conditions'
      ELSEIF( INDEX(ADUM(1:),'mmp').NE.0 ) THEN
        IEO = 4
        WRITE(IWR,'(A)') 'Minimum Miscibility Pressure'
      ELSE
        INDX = 4
        NCH = INDEX( ADUM(1:),'  ' )-1
        CHMSG = 'Unrecognized Execution Option: ' // ADUM(1:NCH)
        CALL WRMSGS( INDX )
      ENDIF
      WRITE(IWR,'(A)') '  w/ First-Order Time Differencing'
      ISLC(11) = 0
!
!---  Output Coupled-Flow Problem Vector  ---
!
      IF( INDEX(ADUM(1:),'coupled-flow-problem-vector').NE.0 ) THEN
        WRITE(IWR,'(A)') '  w/ Output Coupled-Flow Problem Vector'
        ISLC(34) = 1
      ENDIF
!
!---  Output Coupled-Flow Matrix  ---
!
      IF( INDEX(ADUM(1:),'coupled-flow-matrix').NE.0 ) THEN
        WRITE(IWR,'(A)') '  w/ Output Coupled-Flow Matrix'
        ISLC(34) = 2
      ENDIF
!
!---  Output Coupled-Flow Solution Vector  ---
!
      IF( INDEX(ADUM(1:),'coupled-flow-solution-vector').NE.0 ) THEN
        WRITE(IWR,'(A)') '  w/ Output Coupled-Flow Solution Vector'
        ISLC(34) = 3
      ENDIF
!
!---  Output Solute/Species Transport Problem Vector  ---
!
      IF( INDEX(ADUM(1:),'transport-problem-vector').NE.0 ) THEN
        WRITE(IWR,'(A)') '  w/ Output Transport Problem Vector'
        ISLC(34) = 11
      ENDIF
!
!---  Output Solute/Species Transport Matrix  ---
!
      IF( INDEX(ADUM(1:),'transport-matrix').NE.0 ) THEN
        WRITE(IWR,'(A)') '  w/ Output Transport Matrix'
        ISLC(34) = 12
      ENDIF
!
!---  Output Solute/Species Transport Solution Vector  ---
!
      IF( INDEX(ADUM(1:),'transport-solution-vector').NE.0 ) THEN
        WRITE(IWR,'(A)') '  w/ Output Transport Solution Vector'
        ISLC(34) = 13
      ENDIF
!
!---  Output Geomechanics Problem Vector  ---
!
      IF( INDEX(ADUM(1:),'geomechanics-problem-vector').NE.0 ) THEN
        WRITE(IWR,'(A)') '  w/ Output Geomechanics Problem Vector'
        ISLC(34) = 21
      ENDIF
!
!---  Output Geomechanics Matrix  ---
!
      IF( INDEX(ADUM(1:),'geomechanics-matrix').NE.0 ) THEN
        WRITE(IWR,'(A)') '  w/ Output Geomechanics Matrix'
        ISLC(34) = 22
      ENDIF
!
!---  Output Geomechanics Solution Vector  ---
!
      IF( INDEX(ADUM(1:),'geomechanics-solution-vector').NE.0 ) THEN
        WRITE(IWR,'(A)') '  w/ Output Geomechanics Solution Vector'
        ISLC(34) = 23
      ENDIF
!
!---  Scaling Factor Option  ---
!
      IF( INDEX(ADUM(1:),'scaling').NE.0 ) THEN
        WRITE(IWR,'(A)') '  w/ Scaling Factors'
        ISLC(19) = 1
      ENDIF
!
!---  Inverse (UCode) Option  ---
!
      IF( INDEX(ADUM(1:),'inverse').NE.0 .OR.
     &  INDEX(ADUM(1:),'ucode').NE.0 ) THEN
        WRITE(IWR,'(A)') '  w/ Inverse (UCode)'
        ISLC(20) = 1
      ENDIF
!
!---  Extended Output on Convergence-Failure Option  ---
!
      IF( INDEX(ADUM(1:),'extended').NE.0 .AND.
     &  INDEX(ADUM(1:),'output').NE.0 ) THEN
        WRITE(IWR,'(A)') '  w/ Extended Output on Convergence Failure'
        ISLC(62) = 1
      ELSE
        WRITE(IWR,'(A)') '  w/ Standard Output on Convergence Failure'
      ENDIF
!
!---  Linear System Solver Option  ---
!
      NLBD = LBD
      NLSP = LSP
      NLPT = LPT
      NLIS = LIS
      IF( INDEX(CHDUM(1:),'summary').NE.0 ) ISLC(14) = 1
      IF( NLBD.EQ.1 .AND. NLSP.EQ.0 .AND. 
     &  NLPT.EQ.0 .AND. NLIS.EQ.0 ) THEN
        WRITE(IWR,'(A)') 'Linear System Solver: Direct Banded'
        ILES = 1
      ELSEIF( NLBD.EQ.0 .AND. NLSP.EQ.1 .AND. 
     &  NLPT.EQ.0 .AND. NLIS.EQ.0 ) THEN
          WRITE(IWR,'(A)') 'Linear System Solver: SPLIB'
          WRITE(IWR,'(A)') '  Preconditioner: ILU(k)'
          WRITE(IWR,'(A)') '  Solver: BiCGStab'
          ILES = 3
      ELSEIF( NLBD.EQ.0 .AND. NLSP.EQ.0 .AND. 
     &  NLPT.EQ.0 .AND. NLIS.EQ.1 ) THEN
          WRITE(IWR,'(A)') 'Linear System Solver: Lis'
          WRITE(IWR,'(A)') '  Preconditioner: ILU(k)'
          WRITE(IWR,'(A)') '  Solver: BiCGStab'
          ILES = 4








      ELSEIF( NLBD.EQ.0 .AND. NLSP.EQ.0 .AND. 
     &  NLPT.EQ.1 .AND. NLIS.EQ.0 ) THEN
        WRITE(IWR,'(A)') 'Linear System Solver: PETSc'
        WRITE(IWR,'(A)') '  Preconditioner: block Jacobi w/ILU(0)'
        WRITE(IWR,'(A)') '  Solver: BiCGStab'
        ILES = 5
      ELSE
        INDX = 4
        CHMSG = 'Unrecognized Linear System Solver Option'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Read Operational Mode  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Operational Mode'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      NEQ = 0
!
!---  Hydrate inhibitors  ---
!
      I_INH = 0
      IF( INDEX(ADUM(1:),'nacl').NE.0 ) I_INH = 1
      IF( INDEX(ADUM(1:),'cacl2').NE.0 ) I_INH = 2
      IF( INDEX(ADUM(1:),'kcl').NE.0 ) I_INH = 3
      IF( INDEX(ADUM(1:),'nabr').NE.0 ) I_INH = 4
      IF( INDEX(ADUM(1:),'kbr').NE.0 ) I_INH = 5
      IF( INDEX(ADUM(1:),'hcoona').NE.0 ) I_INH = 6
      IF( INDEX(ADUM(1:),'hcook').NE.0 ) I_INH = 7
      IF( INDEX(ADUM(1:),'hcoocs').NE.0 ) I_INH = 8
      IF( INDEX(ADUM(1:),'k2co3').NE.0 ) I_INH = 9
      IF( INDEX(ADUM(1:),'methanol').NE.0 ) I_INH = 10
      IF( INDEX(ADUM(1:),'ethanol').NE.0 ) I_INH = 11
      IF( INDEX(ADUM(1:),'glycerol').NE.0 ) I_INH = 12
      IF( INDEX(ADUM(1:),'meg').NE.0 ) I_INH = 13
      IF( INDEX(ADUM(1:),'deg').NE.0 ) I_INH = 14
      IF( INDEX(ADUM(1:),'teg').NE.0 ) I_INH = 15
      IGAS = 0
      IAQU = 0
      INAPL = 0
!
!---  STOMP-EOR Operational Mode  ---
!
      IF( INDEX(ADUM(1:),'eor').NE.0 ) THEN
!
!---    Block refinement node connections  ---
!
        ISLC(33) = 1
        VARB = 'Cubic Equation of State'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
        IF( INDEX(BDUM(1:),'black').NE.0 .AND.
     &    INDEX(BDUM(1:),'oil').NE.0 ) THEN
          ISLC(68) = 0
          WRITE(IWR,'(2X,A)') 'Black-Oil Equation of State'
        ELSEIF( INDEX(BDUM(1:),'peng').NE.0 .AND.
     &    INDEX(BDUM(1:),'robinson').NE.0 ) THEN
          ISLC(68) = 1
          WRITE(IWR,'(2X,A)') 'Peng-Robinson Equation of State'
        ELSEIF( INDEX(BDUM(1:),'soave').NE.0 .AND.
     &    INDEX(BDUM(1:),'redlich').NE.0 .AND.
     &    INDEX(BDUM(1:),'kwong').NE.0 ) THEN
          ISLC(68) = 2
          WRITE(IWR,'(2X,A)') 'Soave-Redlich-Kwong Equation of State'
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Equation of State ' // BDUM(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
        IF( ISLC(68).NE.0 ) THEN
          VARB = 'Number of Petroleum Components'
          CALL RDINT( ISTART,ICOMMA,CHDUM,NGC )
          IF( NGC+2.GT.LNGC ) THEN
            INDX = 5
            CHMSG = 'Num. of Petroleum Components + 2 > Parameter LNGC'
            CALL WRMSGS( INDX )
          ENDIF
        ELSE
          NGC = 0
        ENDIF
        IF( INDEX(BDUM(1:),'zero').NE.0 .AND.
     &    INDEX(BDUM(1:),'binary').NE.0 ) THEN
          ISLC(69) = 1
          WRITE(IWR,'(2X,A)') 'Zero Binary Interaction Parameters'
        ELSE
          ISLC(69) = 0
          WRITE(IWR,'(2X,A)') 'Default Binary Interaction Parameters'
        ENDIF
        IEQT = 1
        IEQW = 2
        IEQX = MAX( IEQW,IEQT )
!
!---    Black-oil option  ---
!
        IF( ISLC(68).EQ.0 ) THEN
          IEQA = IEQX+1
          IEQO = IEQX+2
          IEQS = IEQX+3
          NEQ = IEQX+3
!
!---    Compositional option  ---
!
        ELSE
          IEQA = IEQX+1
          IEQO = IEQX+2
          IEQGC(1) = IEQX+1
          IEQGC(2) = IEQX+2
          DO 16 M = 3,NGC+2
            IEQGC(M) = IEQX+M
   16     CONTINUE
          IEQS = NGC+5
          NEQ = NGC+5
        ENDIF
        NPH = 3
        IGAS = 1
        IAQU = 1
        INAPL = 1
        IOM = 43
        WRITE(IWR,'(A)') 'Operational Mode: STOMP-EOR'
        IF( INDEX(ADUM(1:),'isotherm').NE.0 ) THEN
          ISLC(30) = 1
          IEQT = 0
          IEQW = 1
          IEQX = MAX( IEQW,IEQT )
          IEQA = IEQX+1
          IEQO = IEQX+2
          IEQGC(1) = IEQX+1
          IEQGC(2) = IEQX+2
          DO 18 M = 3,NGC+2
            IEQGC(M) = IEQX+M
   18     CONTINUE
          IEQS = IEQX+NGC+3
          NEQ = NEQ-1
          WRITE(IWR,'(A)') '  w/ Isothermal Option'
        ELSEIF( ISLC(68).EQ.0 ) THEN
          INDX = 4
          CHMSG = 'Option Conflict: Black-Oil without Isothermal'
          CALL WRMSGS( INDX )        
        ENDIF
        IF( INDEX(ADUM(1:),'iso-h2o').NE.0 .AND. ISLC(68).NE.0  ) THEN
          ISLC(67) = 1
          IEQW = 0
          IEQX = MAX( IEQW,IEQT )
          IEQA = IEQX+1
          IEQO = IEQX+2
          IEQGC(1) = IEQX+1
          IEQGC(2) = IEQX+2
          DO 20 M = 3,NGC+2
            IEQGC(M) = IEQX+M
   20     CONTINUE
          IEQS = IEQX+NGC+3
          NEQ = NEQ-1
          WRITE(IWR,'(A)') '  w/ Iso-H2O Option'
        ENDIF
        IF( INDEX(ADUM(1:),'iso-co2').NE.0 .AND.
     &    INDEX(ADUM(1:),'iso-ch4').NE.0 .AND. ISLC(68).NE.0 ) THEN
          IEQX = MAX( IEQW,IEQT )
          IEQA = 0
          IEQO = 0
          IEQGC(1) = 0
          IEQGC(2) = 0
          DO 22 M = 3,NGC+2
            IEQGC(M) = IEQX+M-2
   22     CONTINUE
          IEQS = IEQX+NGC+1
          NEQ = NEQ-2
          ISLC(45) = 1
          ISLC(64) = 1
          WRITE(IWR,'(A)') '  w/ Iso-CO2 Option'
          WRITE(IWR,'(A)') '  w/ Iso-CH4 Option'
        ELSEIF( INDEX(ADUM(1:),'iso-co2').NE.0 .AND. 
     &    ISLC(68).NE.0 ) THEN
          IEQX = MAX( IEQW,IEQT )
          IEQA = 0
          IEQO = IEQX+1
          IEQGC(1) = 0
          IEQGC(2) = IEQX+1
          DO 24 M = 3,NGC+2
            IEQGC(M) = IEQX+M-1
   24     CONTINUE
          IEQS = IEQX+NGC+2
          NEQ = NEQ-1
          ISLC(45) = 1
          WRITE(IWR,'(A)') '  w/ Iso-CO2 Option'
        ELSEIF( INDEX(ADUM(1:),'iso-ch4').NE.0 .AND. 
     &    ISLC(68).NE.0 ) THEN
          IEQX = MAX( IEQW,IEQT )
          IEQA = IEQX+1
          IEQO = 0
          IEQGC(1) = IEQX+1
          IEQGC(2) = 0
          DO 26 M = 3,NGC+2
            IEQGC(M) = IEQX+M-1
   26     CONTINUE
          IEQS = IEQX+NGC+2
          NEQ = NEQ-1
          ISLC(64) = 1
          WRITE(IWR,'(A)') '  w/ Iso-CH4 Option'
        ENDIF
        IF( INDEX(ADUM(1:),'isobrine').NE.0 ) THEN
          ISLC(32) = 1
          IEQS = 0
          NEQ = NEQ-1
          WRITE(IWR,'(A)') '  w/ Isobrine Option'
        ENDIF
!
!---    Dual Porosity Model Option  ---
!
        IF( INDEX(ADUM(1:),'dual porosity').NE.0 .OR.
     &    INDEX(ADUM(1:),'dual-porosity').NE.0 ) THEN
          ISLC(11) = 1
        ENDIF
!
!---    Equivalent Continuum Model Option  ---
!
        IF( INDEX(ADUM(1:),'equivalent continuum').NE.0 .OR.
     &    INDEX(ADUM(1:),'equivalent-continuum').NE.0 ) THEN
          ISLC(11) = 2
        ENDIF
        ISVC = NEQ
        ISVF = 2*NEQ + 1
!
!---  STOMP-HYDT-KE Operational Mode  ---
!
      ELSEIF( INDEX(ADUM(1:),'hydt-ke').NE.0 .AND.
     &  I_INH.NE.0 ) THEN
        IEQT = 1
        IEQW = 2
        IEQA = 3
        IEQO = 4
        IEQN = 5
        IEQHA = 6
        IEQHO = 7
        IEQHN = 8
        IEQS = 9
        NEQ = 9
        NPH = 3
        IGAS = 1
        IAQU = 1
        WRITE(IWR,'(A)') 'Operational Mode: STOMP-HYDT-KE'
        IF( INDEX(ADUM(1:),'iso-co2').NE.0 .AND.
     &    INDEX(ADUM(1:),'iso-ch4').NE.0 .AND. 
     &    INDEX(ADUM(1:),'iso-n2').NE.0 ) THEN
          INDX = 4
          NCH = INDEX( ADUM(1:),'  ' )-1
          CHMSG = 'No Guest Molecule: ' // ADUM(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
        IF( INDEX(ADUM(1:),'iso-co2').NE.0 .AND.
     &    INDEX(ADUM(1:),'iso-ch4').NE.0 ) THEN
          IEQT = 1
          IEQW = 2
          IEQA = 0
          IEQO = 0
          IEQN = 3
          IEQHA = 0
          IEQHO = 0
          IEQHN = 4
          IEQS = 5
          NEQ = 5
          ISLC(45) = 1
          ISLC(64) = 1
          WRITE(IWR,'(A)') '  w/ Iso-CO2 Option'
          WRITE(IWR,'(A)') '  w/ Iso-CH4 Option'
        ELSEIF( INDEX(ADUM(1:),'iso-co2').NE.0 .AND.
     &    INDEX(ADUM(1:),'iso-n2').NE.0 ) THEN
          IEQT = 1
          IEQW = 2
          IEQA = 0
          IEQO = 3
          IEQN = 0
          IEQHA = 0
          IEQHO = 4
          IEQHN = 0
          IEQS = 5
          NEQ = 5
          ISLC(45) = 1
          ISLC(65) = 1
          WRITE(IWR,'(A)') '  w/ Iso-CO2 Option'
          WRITE(IWR,'(A)') '  w/ Iso-N2 Option'
        ELSEIF( INDEX(ADUM(1:),'iso-ch4').NE.0 .AND.
     &    INDEX(ADUM(1:),'iso-n2').NE.0 ) THEN
          IEQT = 1
          IEQW = 2
          IEQA = 3
          IEQO = 0
          IEQN = 0
          IEQHA = 4
          IEQHO = 0
          IEQHN = 0
          IEQS = 5
          NEQ = 5
          ISLC(64) = 1
          ISLC(65) = 1
          WRITE(IWR,'(A)') '  w/ Iso-CH4 Option'
          WRITE(IWR,'(A)') '  w/ Iso-N2 Option'
        ELSEIF( INDEX(ADUM(1:),'iso-co2').NE.0 ) THEN
          IEQT = 1
          IEQW = 2
          IEQA = 0
          IEQO = 3
          IEQN = 4
          IEQHA = 0
          IEQHO = 5
          IEQHN = 6
          IEQS = 7
          NEQ = 7
          ISLC(45) = 1
          WRITE(IWR,'(A)') '  w/ Iso-CO2 Option'
        ELSEIF( INDEX(ADUM(1:),'iso-ch4').NE.0 ) THEN
          IEQT = 1
          IEQW = 2
          IEQA = 3
          IEQO = 0
          IEQN = 4
          IEQHA = 5
          IEQHO = 0
          IEQHN = 6
          IEQS = 7
          NEQ = 7
          ISLC(64) = 1
          WRITE(IWR,'(A)') '  w/ Iso-CH4 Option'
        ELSEIF( INDEX(ADUM(1:),'iso-n2').NE.0 ) THEN
          IEQT = 1
          IEQW = 2
          IEQA = 3
          IEQO = 4
          IEQN = 0
          IEQHA = 5
          IEQHO = 6
          IEQHN = 0
          IEQS = 7
          NEQ = 7
          ISLC(65) = 1
          WRITE(IWR,'(A)') '  w/ Iso-N2 Option'
        ENDIF
        IF( INDEX(ADUM(1:),'isobrine').NE.0 ) THEN
          ISLC(32) = 1
          IEQS = 0
          IF( ISLC(30).EQ.1 ) IEQT = 6
          NEQ = NEQ-1
          WRITE(IWR,'(A)') '  w/ Isobrine Option'
        ENDIF
        IF( INDEX(ADUM(1:),'inhibitor').NE.0 ) THEN
          ISLC(41) = 1
          WRITE(IWR,'(A)') '  w/ Inhibitor Effects'
        ELSE
          WRITE(IWR,'(A)') '  w/o Inhibitor Effects'
        ENDIF
        IOM = 39
        ISVC = NEQ
        ISVF = 2*NEQ + 1
        IF( INDEX(ADUM(1:),'poynting' ).NE.0 ) THEN
          ISLC(25) = 1
          WRITE(IWR,'(A)') '  w/ Poynting Effect for ' //
     &      'CO2 Solubility'
        ELSE
          WRITE(IWR,'(A)') '  w/o Poynting Effect for ' //
     &      'CO2 Solubility'
        ENDIF
!
!---  STOMP-HYDT-KE Operational Mode  ---
!
      ELSEIF( INDEX(ADUM(1:),'hydt-ke').NE.0 .AND.
     &  I_INH.NE.0 ) THEN
        IEQT = 1
        IEQW = 2
        IEQA = 3
        IEQO = 4
        IEQN = 5
        IEQHA = 6
        IEQHO = 7
        IEQHN = 8
        IEQS = 9
        NEQ = 9
        NPH = 3
        IGAS = 1
        IAQU = 1
        WRITE(IWR,'(A)') 'Operational Mode: STOMP-HYDT-KE'
        IF( INDEX(ADUM(1:),'iso-co2').NE.0 .AND.
     &    INDEX(ADUM(1:),'iso-ch4').NE.0 .AND. 
     &    INDEX(ADUM(1:),'iso-n2').NE.0 ) THEN
          INDX = 4
          NCH = INDEX( ADUM(1:),'  ' )-1
          CHMSG = 'No Guest Molecule: ' // ADUM(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
        IF( INDEX(ADUM(1:),'iso-co2').NE.0 .AND.
     &    INDEX(ADUM(1:),'iso-ch4').NE.0 ) THEN
          IEQT = 1
          IEQW = 2
          IEQA = 0
          IEQO = 0
          IEQN = 3
          IEQHA = 0
          IEQHO = 0
          IEQHN = 4
          IEQS = 5
          NEQ = 5
          ISLC(45) = 1
          ISLC(64) = 1
          WRITE(IWR,'(A)') '  w/ Iso-CO2 Option'
          WRITE(IWR,'(A)') '  w/ Iso-CH4 Option'
        ELSEIF( INDEX(ADUM(1:),'iso-co2').NE.0 .AND.
     &    INDEX(ADUM(1:),'iso-n2').NE.0 ) THEN
          IEQT = 1
          IEQW = 2
          IEQA = 0
          IEQO = 3
          IEQN = 0
          IEQHA = 0
          IEQHO = 4
          IEQHN = 0
          IEQS = 5
          NEQ = 5
          ISLC(45) = 1
          ISLC(65) = 1
          WRITE(IWR,'(A)') '  w/ Iso-CO2 Option'
          WRITE(IWR,'(A)') '  w/ Iso-N2 Option'
        ELSEIF( INDEX(ADUM(1:),'iso-ch4').NE.0 .AND.
     &    INDEX(ADUM(1:),'iso-n2').NE.0 ) THEN
        IEQT = 1
        IEQW = 2
        IEQA = 3
          IEQO = 0
          IEQN = 0
          IEQHA = 4
          IEQHO = 0
          IEQHN = 0
        IEQS = 5
        NEQ = 5
          ISLC(64) = 1
          ISLC(65) = 1
          WRITE(IWR,'(A)') '  w/ Iso-CH4 Option'
          WRITE(IWR,'(A)') '  w/ Iso-N2 Option'
        ELSEIF( INDEX(ADUM(1:),'iso-co2').NE.0 ) THEN
          IEQT = 1
          IEQW = 2
          IEQA = 0
          IEQO = 3
          IEQN = 4
          IEQHA = 0
          IEQHO = 5
          IEQHN = 6
          IEQS = 7
          NEQ = 7
          ISLC(45) = 1
          WRITE(IWR,'(A)') '  w/ Iso-CO2 Option'
        ELSEIF( INDEX(ADUM(1:),'iso-ch4').NE.0 ) THEN
          IEQT = 1
          IEQW = 2
          IEQA = 3
          IEQO = 0
          IEQN = 4
          IEQHA = 5
          IEQHO = 0
          IEQHN = 6
          IEQS = 7
          NEQ = 7
          ISLC(64) = 1
          WRITE(IWR,'(A)') '  w/ Iso-CH4 Option'
        ELSEIF( INDEX(ADUM(1:),'iso-n2').NE.0 ) THEN
        IEQT = 1
        IEQW = 2
        IEQA = 3
          IEQO = 4
          IEQN = 0
          IEQHA = 5
          IEQHO = 6
          IEQHN = 0
          IEQS = 7
          NEQ = 7
          ISLC(65) = 1
          WRITE(IWR,'(A)') '  w/ Iso-N2 Option'
        ENDIF
        IF( INDEX(ADUM(1:),'isobrine').NE.0 ) THEN
          ISLC(32) = 1
          IEQS = 0
          IF( ISLC(30).EQ.1 ) IEQT = 6
          NEQ = NEQ-1
          WRITE(IWR,'(A)') '  w/ Isobrine Option'
        ENDIF
        IF( INDEX(ADUM(1:),'inhibitor').NE.0 ) THEN
          ISLC(41) = 1
          WRITE(IWR,'(A)') '  w/ Inhibitor Effects'
        ELSE
          WRITE(IWR,'(A)') '  w/o Inhibitor Effects'
        ENDIF
        IOM = 39
        ISVC = NEQ
        ISVF = 2*NEQ + 1
        IF( INDEX(ADUM(1:),'poynting' ).NE.0 ) THEN
          ISLC(25) = 1
          WRITE(IWR,'(A)') '  w/ Poynting Effect for ' //
     &      'CO2 Solubility'
        ELSE
          WRITE(IWR,'(A)') '  w/o Poynting Effect for ' //
     &      'CO2 Solubility'
        ENDIF
!
!---  STOMP-HYD Operational Mode  ---
!
      ELSEIF( INDEX(ADUM(1:),'hyd').NE.0 .AND.
     &  I_INH.NE.0 ) THEN
        IEQT = 1
        IEQW = 2
        IEQA = 3
        IEQDO = 4
        IEQS = 5
        NEQ = 5
        NPH = 3
        IGAS = 1
        IAQU = 1
        WRITE(IWR,'(A)') 'Operational Mode: STOMP-HYD'
        IF( INDEX(ADUM(1:),'isobrine').NE.0 ) THEN
          ISLC(32) = 1
          IEQS = 0
          NEQ = NEQ-1
          WRITE(IWR,'(A)') '  w/ Isobrine Option'
        ENDIF
        IF( INDEX(ADUM(1:),'iso-co2').NE.0 ) THEN
          ISLC(45) = 1
          IEQA = 0
          IEQDO = IEQDO-1
          IF( ISLC(32).EQ.0 ) IEQS = IEQS-1
          NEQ = NEQ-1
          WRITE(IWR,'(A)') '  w/ Isocarbon Option'
        ENDIF
        IF( INDEX(ADUM(1:),'inhibitor').NE.0 ) THEN
          ISLC(41) = 1
          WRITE(IWR,'(A)') '  w/ Inhibitor Effects'
        ELSE
          WRITE(IWR,'(A)') '  w/o Inhibitor Effects'
        ENDIF
        IOM = 37
        ISVC = NEQ
        ISVF = 2*NEQ + 1
        IF( INDEX(ADUM(1:),'poynting' ).NE.0 ) THEN
          ISLC(25) = 1
          WRITE(IWR,'(A)') '  w/ Poynting Effect for ' //
     &      'CO2 Solubility'
        ELSE
          WRITE(IWR,'(A)') '  w/o Poynting Effect for ' //
     &      'CO2 Solubility'
        ENDIF
!
!---  STOMP-COMP Operational Mode  ---
!
      ELSEIF( INDEX(ADUM(1:),'stomp-comp').NE.0 ) THEN
        IEQT = 1
        IEQW = 2
        IEQS = 3
        DO M = 1,LNGC
          IEQGC(M) = M+3
        END DO
        NEQ = 3+LNGC
        NPH = 2
        IGAS = 1
        IAQU = 1
        IOM = 40
        ISVC = NEQ
        ISVF = 2*NEQ + 1
        WRITE(IWR,'(A)') 'Operational Mode: STOMP-COMP'
        IF( INDEX(ADUM(1:),'isotherm').NE.0 ) THEN
          ISLC(30) = 1
          IEQT = 0
          IEQW = 1
          IEQS = 2
          DO M = 1,LNGC
           IEQGC(M) = IEQGC(M) - 1
          ENDDO
          NEQ = NEQ-1
          WRITE(IWR,'(A)') '  w/ Isothermal Option'
        ENDIF
        IF( INDEX(ADUM(1:),'isobrine').NE.0 ) THEN
          ISLC(32) = 1
          IEQS = 0
          DO M = 1,LNGC
           IEQGC(M) = IEQGC(M) - 1
          ENDDO
          NEQ = NEQ-1
          WRITE(IWR,'(A)') '  w/ Isobrine Option'
        ENDIF
        ISVC = NEQ
        ISVF = 2*NEQ + 1
!
!---  STOMP-CO2e Operational Mode  ---
!
      ELSEIF( INDEX(ADUM(1:),'stomp-co2e').NE.0 ) THEN
        IEQT = 1
        IEQW = 2
        IEQA = 3
        IEQS = 4
        NEQ = 4
        NPH = 2
        IGAS = 1
        IAQU = 1
        IOM = 33
        WRITE(IWR,'(A)') 'Operational Mode: STOMP-CO2e'
        IF( INDEX(ADUM(1:),'isotherm').NE.0 ) THEN
          ISLC(30) = 1
          IEQT = 0
          IEQW = 1
          IEQA = 2
          IEQS = 3
          NEQ = NEQ-1
          WRITE(IWR,'(A)') '  w/ Isothermal Option'
        ENDIF
        IF( INDEX(ADUM(1:),'isobrine').NE.0 ) THEN
          ISLC(32) = 1
          IEQS = 0
          NEQ = NEQ-1
          WRITE(IWR,'(A)') '  w/ Isobrine Option'
        ENDIF
        ISVC = NEQ
        ISVF = 2*NEQ + 1
        IF( INDEX(ADUM(1:),'invariant' ).NE.0 ) THEN
          ISLC(9) = 1
          WRITE(IWR,'(A)') '  w/ Invariant Fluid Density and Viscosity'
        ENDIF
        IF( INDEX(ADUM(1:),'fractional co2 solu' ).NE.0 ) THEN
          ISLC(25) = 2
          WRITE(IWR,'(A)') '  w/ Fractional CO2 Solubility'
        ENDIF
        IF( INDEX(ADUM(1:),'enhanced well' ).NE.0 ) THEN
          ISLC(70) = 1
          WRITE(IWR,'(A)') '  w/ Enhanced Coupled Well Model'
        ENDIF
!
!---  STOMP-CO2 Operational Mode  ---
!
      ELSEIF( INDEX(ADUM(1:),'stomp-co2').NE.0 ) THEN
        IEQW = 1
        IEQA = 2
        IEQS = 3
        NEQ = 3
        NPH = 2
        IGAS = 1
        IAQU = 1
        IOM = 32
        WRITE(IWR,'(A)') 'Operational Mode: STOMP-CO2'
        IF( INDEX(ADUM(1:),'isobrine').NE.0 ) THEN
          ISLC(32) = 1
          IEQS = 0
          NEQ = NEQ-1
          WRITE(IWR,'(A)') '  w/ Isobrine Option'
        ENDIF
        ISVC = NEQ
        ISVF = 2*NEQ + 1
        IF( INDEX(ADUM(1:),'invariant' ).NE.0 ) THEN
          ISLC(9) = 1
          WRITE(IWR,'(A)') '  w/ Invariant Fluid Density and Viscosity'
        ENDIF
        IF( INDEX(ADUM(1:),'fractional co2 solu' ).NE.0 ) THEN
          ISLC(25) = 2
          WRITE(IWR,'(A)') '  w/ Fractional CO2 Solubility'
        ENDIF
        IF( INDEX(ADUM(1:),'enhanced well' ).NE.0 ) THEN
          ISLC(70) = 1
          WRITE(IWR,'(A)') '  w/ Enhanced Coupled Well Model'
        ENDIF
!
!---  Water-Air-Oil Operational Mode  ---
!
      ELSEIF( INDEX(ADUM(1:),'stomp-woa').NE.0 ) THEN
        IEQW = 1
        IEQA = 2
        IEQO = 3
        NEQ = 3
        NPH = 3
        IGAS = 1
        IAQU = 1
        INAPL = 1
        IOM = 5
        ISVC = NEQ
        ISVF = 2*NEQ + 1
        WRITE(IWR,'(A)') 'Operational Mode: STOMP-WOA'
        IF( INDEX(ADUM(1:),'kinetic').NE.0 .AND.
     &    INDEX(ADUM(1:),'volatil').NE.0 ) THEN
          ISLC(46) = 1
          IEQDO = 4
          NEQ = 4
          ISVC = NEQ
          ISVF = 2*NEQ + 1
          WRITE(IWR,'(A)') '  w/ Kinetic Volatilization Option'
        ENDIF
!vlf
        IF( INDEX( ADUM(1:),'electrolyte' ).NE.0 ) THEN
          ISLC(16) = 1
          WRITE(IWR,'(A)') '  w/ Electrolyte Solute Transport'
        ENDIF
!vlf
!
!---  Water-Oil Operational Mode  ---
!
      ELSEIF( INDEX(ADUM(1:),'stomp-wo').NE.0 ) THEN
        IEQW = 1
        IEQO = 2
        NEQ = 2
        NPH = 2
        IAQU = 1
        INAPL = 1
        IOM = 4
        ISVC = NEQ
        ISVF = 2*NEQ + 1
        WRITE(IWR,'(A)') 'Operational Mode: STOMP-WO'
        IF( INDEX(ADUM(1:),'partition').NE.0 .OR.
     &    INDEX(ADUM(1:),'kinetic').NE.0 ) THEN
          IOM = 24
          WRITE(IWR,'(A)') '  w/ Kinetic Solute Partitioning Mode'
        ENDIF
        IF( INDEX(ADUM(1:),'constant-aqueous-density').NE.0 ) THEN
          ISLC(53) = 1
          WRITE(IWR,'(A)') '  w/ Constant Aqueous Density'
        ENDIF
        IF( INDEX(ADUM(1:),'supersaturation').NE.0 ) THEN
          ISLC(54) = 1
          WRITE(IWR,'(A)') '  w/ Supersaturation Factor'
        ENDIF
!
!---  Geothermal (GT) Operational Mode  ---
!
      ELSEIF( INDEX(ADUM(1:),'stomp-gt').NE.0 ) THEN
        IEQT = 1
        IEQW = 2
        IEQA = 3
        IEQS = 4
        NEQ = 4
        NPH = 2
        IGAS = 1
        IAQU = 1
        IOM = 3
        ISVC = NEQ
        ISVF = 2*NEQ + 1
        WRITE(IWR,'(A)') 'Operational Mode: STOMP-GT'
        IF( INDEX(ADUM(1:),'time-lag' ).NE.0 )  THEN
          WRITE(IWR,'(A)') '  w/ Time-Lag for Shuttleworth-Wallace' //
     &      'Boundary Condition Scheme'
          ISLC(24) = 10
        ENDIF
        IF( INDEX(ADUM(1:),'seawater' ).NE.0 )  THEN
          WRITE(IWR,'(A)') '  w/ Seawater Density Function'
          ISLC(22) = 1
        ENDIF
        IF( INDEX(ADUM(1:),'isobrine').NE.0 ) THEN
          ISLC(32) = 1
          IEQS = 0
          NEQ = NEQ-1
          WRITE(IWR,'(A)') '  w/ Isobrine Option'
        ENDIF
        ISVC = NEQ
        ISVF = 2*NEQ + 1
!
!---  Water Operational Mode  ---
!
      ELSEIF( INDEX(ADUM(1:),'stomp-w').NE.0 ) THEN
        IEQW = 1
        NEQ = 1
        NPH = 1
        IAQU = 1
        IOM = 1
        ISVC = NEQ
        ISVF = 2*NEQ + 1
        WRITE(IWR,'(A)') 'Operational Mode: STOMP-W'
        IF( INDEX(ADUM(1:),'5512').NE.0 ) THEN
          IOM = 5512
          WRITE(IWR,'(A)') '  w/ NUREG-5512 Transport'
        ENDIF
        IF( INDEX( ADUM(1:),'electrolyte' ).NE.0 ) THEN
          ISLC(16) = 1
          WRITE(IWR,'(A)') '  w/ Electrolyte Solute Transport'
          IF( INDEX( ADUM(1:),'tension' ).NE.0 ) THEN
            ISLC(55) = 1
            WRITE(IWR,'(A)') '  w/ Surface Tension Effects'
          ENDIF
        ENDIF
!
!---  Fluid Operational Mode  ---
!
      ELSEIF( INDEX(ADUM(1:),'fluid').NE.0 ) THEN
        IEQW = 1
        NEQ = 1
        NPH = 1
        IAQU = 1
        IOM = 1
        ISVC = NEQ
        ISVF = 2*NEQ + 1
        ISLC(9) = 1
        WRITE(IWR,'(A)') 'Operational Mode: STOMP-W'
        WRITE(IWR,'(A)') '  w/ Constant Fluid Properties'
        IF( INDEX( ADUM(1:),'electrolyte' ).NE.0 ) THEN
          ISLC(16) = 1
          WRITE(IWR,'(A)') '  w/ Electrolyte Solute Transport'
          IF( INDEX( ADUM(1:),'tension' ).NE.0 ) THEN
            ISLC(55) = 1
            WRITE(IWR,'(A)') '  w/ Surface Tension Effects'
          ENDIF
        ENDIF
!
!---  Unrecognized Operational Mode  ---
!
      ELSE
        INDX = 4
        CHMSG = 'Unrecognized Operational Mode ' //
     &    ADUM(1:NCH)
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Minimum miscibility pressure simulation specified for 
!     non-STOMP-EOR operational mode  ---
!
      IF( IEO.EQ.4 .AND. IOM.NE.43 ) THEN
        INDX = 4
        CHMSG = 'Minimum Miscibility Pressure Simulation Specified' //
     &    'for non-STOMP-EOR Operational Mode.'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Transport Options  ---
!
      IF( INDEX(ADUM(1:),'transport').NE.0 ) THEN
        IF( INDEX(ADUM(1:),'courant').NE.0 ) THEN
          IF( INDEX(ADUM(1:),'vadose').NE.0 ) THEN
            WRITE(IWR,'(A)') '  w/ Vadose-Zone, Courant-Number ' //
     &        'Limited Transport'
            ISLC(17) = 2
            ICRNT = 1
          ELSEIF( INDEX(ADUM(1:),'aqueous-only').NE.0 ) THEN
            WRITE(IWR,'(A)') '  w/ Aqueous-Phase Courant-Number ' //
     &        'Limited Transport'
            ISLC(17) = 3
            ICRNT = 1
          ELSE
            WRITE(IWR,'(A)') '  w/ Courant-Number Limited Transport'
            ISLC(17) = 1
            ICRNT = 1
          ENDIF
        ENDIF
        IF( INDEX(ADUM(1:),'no aqu').NE.0 ) THEN
          WRITE(IWR,'(A)') '  w/o Aqueous Advective Solute Transport'
          ISLC(23) = ISLC(23)+1
        ENDIF
        IF( INDEX(ADUM(1:),'no gas').NE.0 ) THEN
          WRITE(IWR,'(A)') '  w/o Gaseous Advective Solute Transport'
          ISLC(23) = ISLC(23)+10
        ENDIF
        IF( INDEX(ADUM(1:),'no napl').NE.0 ) THEN
          WRITE(IWR,'(A)') '  w/o NAPL Advective Solute Transport'
          ISLC(23) = ISLC(23)+100
        ENDIF
      ENDIF
!
!---  Geomechanics Options  ---
!
      IF( INDEX(ADUM(1:),'geomechanics').NE.0 ) THEN
        WRITE(IWR,'(A)') '  w/ Geomechanics'
        ISLC(50) = 1
        IF( (INDEX(ADUM(1:),'no poro').NE.0 .OR. 
     &    INDEX(ADUM(1:),'no-poro').NE.0) .AND.
     &    (INDEX(ADUM(1:),'no thermo').NE.0 .OR. 
     &    INDEX(ADUM(1:),'no-thermo').NE.0) ) THEN
          ISLC(50) = 4
          WRITE(IWR,'(A)') '  w/o Poroelasticity'
          WRITE(IWR,'(A)') '  w/o Thermoelasticity'
        ELSEIF( INDEX(ADUM(1:),'no poro').NE.0 .OR. 
     &    INDEX(ADUM(1:),'no-poro').NE.0 ) THEN
          ISLC(50) = 2
          WRITE(IWR,'(A)') '  w/o Poroelasticity'
        ELSEIF( INDEX(ADUM(1:),'no thermo').NE.0 .OR. 
     &    INDEX(ADUM(1:),'no-thermo').NE.0 ) THEN
          ISLC(50) = 3
          WRITE(IWR,'(A)') '  w/o Thermoelasticity'
        ELSEIF( INDEX(ADUM(1:),'no coupled').NE.0 .OR. 
     &    INDEX(ADUM(1:),'no-coupled').NE.0 ) THEN
          ISLC(50) = 5
          WRITE(IWR,'(A)') '  w/o Coupled Flow and Transport '
        ENDIF
        IF( INDEX(ADUM(1:),'no bodyforce').NE.0 .OR. 
     &    INDEX(ADUM(1:),'no-bodyforce').NE.0 .OR. 
     &    INDEX(ADUM(1:),'no gbf').NE.0 .OR. 
     &    INDEX(ADUM(1:),'no-gbf').NE.0 ) THEN
          ISLC(50) = ISLC(50) + 10
          WRITE(IWR,'(A)') '  w/o Gravitational Body Force '
        ENDIF
!
!---    Use negative values to set reference volumetric stresses
!       at finite elment centroids  ---
!
        IF( IEO.NE.2 ) ISLC(50) = -ISLC(50)
!
!---    Gauss Integration Options  ---
!
        IF( INDEX(ADUM(1:),'gauss 3-point').NE.0 ) THEN
          ISLC(72) = 3
          WRITE(IWR,'(A)') '  w/ 3 Gauss Integration Points '
        ELSEIF( INDEX(ADUM(1:),'gauss 4-point').NE.0 ) THEN
          ISLC(72) = 4
          WRITE(IWR,'(A)') '  w/ 4 Gauss Integration Points '
        ELSEIF( INDEX(ADUM(1:),'gauss 5-point').NE.0 ) THEN
          ISLC(72) = 5
          WRITE(IWR,'(A)') '  w/ 5 Gauss Integration Points '
        ELSE
          ISLC(72) = 2
          WRITE(IWR,'(A)') '  w/ 2 Gauss Integration Points '
        ENDIF
      ENDIF
!
!---  NAPL Surface Spill Options  ---
!
      IF( INDEX(ADUM(1:),'surface spill').NE.0 ) THEN
        WRITE(IWR,'(A)') '  w/ Surface Spill'
        ISLC(49) = 1
      ENDIF
!
!---  Vapor-Pressure-Lowering Options  ---
!
      IF( INDEX(ADUM(1:),'no vapor').NE.0 ) THEN
        WRITE(IWR,'(A)') '  w/o Vapor Pressure Lowering'
        ISLC(44) = 1
      ENDIF
!
!---  Reactive Transport Options  ---
!
      IF( INDEX(ADUM(1:),'eckechem').NE.0 ) THEN
        WRITE(IWR,'(A)') '  w/ ECKEChem'
        ISLC(40) = 1
         
        IF( INDEX(ADUM(1:),'equilibrium reduced').NE.0 ) THEN
          WRITE(IWR,'(A)') '  w/ Equilibrium Reduced'
          ISLC(57) = 1
        ENDIF
        IF( INDEX(ADUM(1:),'mixing coefficient').NE.0 ) THEN
          WRITE(IWR,'(A)') '  w/ Mixing Coefficient'
          ISLC(58) = 1
        ENDIF
        IF( INDEX(ADUM(1:),'minimum concentration').NE.0 ) THEN
          WRITE(IWR,'(A)') '  w/ Minimum Concentration'
          ISLC(59) = 1
        ENDIF
        IF( INDEX(ADUM(1:),'log').NE.0 ) THEN
          WRITE(IWR,'(A)') '  w/ Log'
          ISLC(60) = 1
        ENDIF
!
!---    Courant number control  ---
!
        IF( INDEX(ADUM(1:),'courant').NE.0 ) THEN
          IF( INDEX(ADUM(1:),'vadose').NE.0 ) THEN
            WRITE(IWR,'(A)') '  w/ Vadose-Zone, Courant-Number ' //
     &        'Limited Transport'
            ISLC(17) = 2
            ICRNT = 1
          ELSE
            WRITE(IWR,'(A)') '  w/ Courant-Number Limited Transport'
            ISLC(17) = 1
            ICRNT = 1
          ENDIF
        ENDIF
!
!---    Advection-diffusion transport scheme  ---
!
        IF( INDEX(ADUM(1:),'tvd').NE.0 .OR.
     &    INDEX(ADUM(1:),'leonard').NE.0 ) THEN
          ISLC(1) = 1
          WRITE(IWR,'(A)') '  w/ Leonard-TVD Solute Transport'
        ELSEIF( INDEX(ADUM(1:),'roe').NE.0 .OR.
     &    INDEX(ADUM(1:),'superbee').NE.0 ) THEN
          ISLC(1) = 2
          WRITE(IWR,'(A)') '  w/ Roe''s Superbee Solute Transport'
        ELSEIF( INDEX(ADUM(1:),'first-order').NE.0 .OR.
     &    INDEX(ADUM(1:),'upwind').NE.0 ) THEN
          ISLC(1) = 3
          WRITE(IWR,'(A)') '  w/ First-Order Upwind' //
     &      ' Solute Transport'
        ELSE
          ISLC(1) = 0
          WRITE(IWR,'(A)') '  w/ Patankar Solute Transport'
        ENDIF
!
!---    Flow or flux hydraulic dispersion coefficient  ---
!
        IF( INDEX(ADUM(1:),'flow-hyd').NE.0 ) THEN
          ISLC(52) = 0
          WRITE(IWR,'(A)') '  w/ Flow Velocity for Hydraulic Dispersion'
        ELSEIF( INDEX(ADUM(1:),'flux-hyd').NE.0 ) THEN
          ISLC(52) = 1
          WRITE(IWR,'(A)') '  w/ Flux Velocity for Hydraulic Dispersion'
        ENDIF
!
!---    Guess species concentrations on first call  ---
!
        IF( INDEX(ADUM(1:),'guess').NE.0 ) THEN
          ISLC(42) = 1
          WRITE(IWR,'(A)') '  w/ Initial Species Concentration Guessing'
        ENDIF
!
!---    Porosity alteration  ---
!
        IF( INDEX(ADUM(1:),'porosity').NE.0 ) THEN
          ISLC(43) = 1
          WRITE(IWR,'(A)') '  w/ Porosity Alteration with Precipitation'
        ENDIF
!
!---    Effective reaction area ---
!
        IF( INDEX(ADUM(1:),'area').NE.0 ) THEN
          IF( INDEX(ADUM(1:),'constant surface').NE.0 ) THEN
          ISLC(56) = 2
            WRITE(IWR,'(2A)') '  w/ Mineral surface area held ' //
     &        'constant at initial value'
          ELSE
          ISLC(56) = 1
            WRITE(IWR,'(A)') '  w/ Mineral effective surface area ' //
     &        'scales with water saturation'
          ENDIF
        ENDIF
      ENDIF
!
!---  Solute Transport  ---
!
      IF( INDEX(ADUM(1:),'transport').NE.0 ) THEN
        NEQ = NEQ + 1
        IEQC = NEQ
        ISVT = 1
        IF( INDEX(ADUM(1:),'tvd').NE.0 .OR.
     &    INDEX(ADUM(1:),'leonard').NE.0 ) THEN
          ISLC(1) = 1
          WRITE(IWR,'(A)') '  w/ Leonard-TVD Solute Transport'
        ELSEIF( INDEX(ADUM(1:),'roe').NE.0 .OR.
     &    INDEX(ADUM(1:),'superbee').NE.0 ) THEN
          ISLC(1) = 2
          WRITE(IWR,'(A)') '  w/ Roe''s Superbee Solute Transport'
        ELSEIF( INDEX(ADUM(1:),'first-order').NE.0 .OR.
     &    INDEX(ADUM(1:),'upwind').NE.0 ) THEN
          ISLC(1) = 3
          WRITE(IWR,'(A)') '  w/ First-Order Upwind' //
     &      ' Solute Transport'
        ELSE
          ISLC(1) = 0
          WRITE(IWR,'(A)') '  w/ Patankar Solute Transport'
        ENDIF
      ENDIF
!
!---  Maximum Courant number  ---
!
      CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
      IF( INDX.EQ.1 ) THEN
        VARB = 'Maximum Courant Number'
        IDFLT = 1
        CALL RDDPR(ISTART,ICOMMA,CHDUM,CRNTMXT)
        WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:IVR),CRNTMXT
      ENDIF
!
!---  Parameter size checks  ---
!
      NLT = LT
      NLL = LL
      NLG = LG
      NLN = LN
      NLC = LC
      NLD = LD
      NLS = LS
      NALC = LALC
      NLFC = LFW
      IF( IEQT.GT.0 .AND. NLT.EQ.0 ) THEN
        INDX = 5
        CHMSG = 'Energy Equation Solved w/ Parameter LT = 0'
        CALL WRMSGS( INDX )
      ELSEIF( IAQU.GT.0 .AND. NLL.EQ.0 ) THEN
        INDX = 5
        CHMSG = 'Aqueous Phase w/ Parameter LL = 0'
        CALL WRMSGS( INDX )
      ELSEIF( IGAS.GT.0 .AND. NLG.EQ.0 ) THEN
        INDX = 5
        CHMSG = 'Gas Phase w/ Parameter LG = 0'
        CALL WRMSGS( INDX )
      ELSEIF( INAPL.GT.0 .AND. NLN.EQ.0 ) THEN
        INDX = 5
        CHMSG = 'NAPL w/ Parameter LN = 0'
        CALL WRMSGS( INDX )
      ELSEIF( IEQC.GT.0 .AND. NLC.EQ.0 ) THEN
        INDX = 5
        CHMSG = 'Solute Equation Solved w/ Parameter LC = 0'
        CALL WRMSGS( INDX )
      ELSEIF( IEQD.GT.0 .AND. NLD.EQ.0 ) THEN
        INDX = 5
        CHMSG = 'Dissolved-Oil Equation w/ Parameter LD = 0'
        CALL WRMSGS( INDX )
      ELSEIF( IEQALC.GT.0 .AND. NALC.EQ.0 ) THEN
        INDX = 5
        CHMSG = 'Alcohol Equation w/ Parameter LALC = 0'
        CALL WRMSGS( INDX )
      ELSEIF( IEQS.GT.0 .AND. NLS.EQ.0 ) THEN
        INDX = 5
        IF( IOM.EQ.11 .OR. IOM.EQ.12 .OR. IOM.EQ.13 ) THEN
          CHMSG = 'Salt Equation Solved w/ Parameter LS = 0'
        ELSEIF( IOM.EQ.8 .OR. IOM.EQ.9 ) THEN
          CHMSG = 'Surfactant Equation Solved w/ Parameter LS = 0'
        ELSE
          CHMSG = 'Parameter LS = 0'
        ENDIF
        CALL WRMSGS( INDX )
      ELSEIF( ISLC(5).GT.0 .AND. NLFC.EQ.0 ) THEN
        INDX = 5
        CHMSG = 'Freezing Conditions w/ Parameter LFW = 0'
        CALL WRMSGS( INDX )
      ENDIF
      IF( NPH.GT.LPH ) THEN
        INDX = 5
        CHMSG = 'Number of Phases > Parameter LPH'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Skip time limit reads for initial conditions
!     or minimum miscibility simulations  ---
!
      IF( IEO.EQ.3 .OR. IEO.EQ.4 ) THEN
        ISUB_LOG = ISUB_LOG-1
        RETURN
      ELSEIF( IEO.EQ.2 ) THEN
        INDX = 1
        CALL RDRST( INDX )
      ENDIF
!
!---  Read Execution Periods  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Execution Periods'
      CALL RDINT(ISTART,ICOMMA,CHDUM,NEPD)
      IF( NEPD.LE.-3 ) THEN
        MEPD = 1
        NEPD = ABS(NEPD)
        WRITE(IWR,'(A)') 'Cyclic Execution Periods'
!
!---    If no initial time record is read for a restart simulation,
!       then obtain initial time record from the restart file  ---
!
        VARB = 'Simulation Start Time'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
        IF( IEO.EQ.2 .AND. ICOMMA.EQ.ISTART ) THEN
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(/,2X,2A,1PE11.4)') VARB(1:IVR),', s: ',TMPSX
        ELSE
          TMPSX = VAR
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(/,2X,4A,1PE11.4)') VARB(1:IVR),', ',
     &      UNTS(1:NCH),': ',TMPSX
          INDX = 0
          IUNS = 1
          CALL RDUNIT(UNTS,TMPSX,INDX)
        ENDIF
!
!---    Simulation stop time  ---
!
        VARB = 'Simulation Stop Time'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,TMPEX)
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE (IWR,'(2X,4A,1PE11.4)') VARB(1:IVR),', ',UNTS(1:NCH),': ',
     &    TMPEX
        INDX = 0
        IUNS = 1
        CALL RDUNIT(UNTS,TMPEX,INDX)
      ELSEIF( NEPD.GE.1 ) THEN
        MEPD = 0
        WRITE(IWR,'(A)') 'Noncyclic Execution Periods'
      ELSEIF( NEPD.EQ.0 ) THEN
        INDX = 4
        CHMSG = 'No Execution Periods'
        CALL WRMSGS( INDX )
      ELSE
        INDX = 4
        CHMSG = 'Number of Cyclic Execution Periods < 4'
        CALL WRMSGS( INDX )
      ENDIF
      WRITE(IWR,'(2A,I6)') VARB(1:IVR),': ',NEPD
      IF( NEPD.GT.LEPD ) THEN
        INDX = 5
        CHMSG = 'Number of Execution Periods > Parameter LEPD'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Loop over the number of execution periods
!
      DO 100 N = 1,NEPD
        WRITE(IWR,'(/,A,I6)') '  Execution Period No. ',N
!
!---    Read Solution Time Limits  ---
!
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
!
!---    If no initial time record is read for a restart simulation,
!       then obtain initial time record from the restart file  ---
!
        VARB = 'Execution Period Start Time'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
        IF( IEO.EQ.2 .AND. N.EQ.1 .AND. ICOMMA.EQ.ISTART ) THEN
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),', s: ',TMPS(N)
        ELSE
          TMPS(N) = VAR
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4)') VARB(1:IVR),', ',
     &      UNTS(1:NCH),': ',TMPS(N)
          INDX = 0
          IUNS = 1
          CALL RDUNIT(UNTS,TMPS(N),INDX)
        ENDIF
!
!---    Maximum time  ---
!
        VARB = 'Execution Period Stop Time'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,TMPE(N))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE (IWR,'(2X,4A,1PE11.4)') VARB(1:IVR),', ',UNTS(1:NCH),': ',
     &    TMPE(N)
        INDX = 0
        IUNS = 1
        CALL RDUNIT(UNTS,TMPE(N),INDX)
!
!---    If no time step record is read for a restart simulation,
!       then obtain the initial time step from the restart file  ---
!
        VARB = 'Initial Time Step'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
        IF( IEO.EQ.2 .AND. N.EQ.1 .AND. ICOMMA.EQ.ISTART ) THEN
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),', s: ',TMPD(N)
        ELSEIF( ICOMMA.EQ.ISTART ) THEN
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': Unspecified'
          TMPD(N) = 0.D+0
        ELSE
          TMPD(N) = VAR
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE (IWR,'(2X,4A,1PE11.4)')VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',TMPD(N)
          INDX = 0
          IUNS = 1
          CALL RDUNIT(UNTS,TMPD(N),INDX)
        ENDIF
!
!---    If no maximum time step record is read for a restart simulation,
!       then obtain the maximum time step from the restart file  ---
!
        VARB = 'Maximum Time Step'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
        IF( IEO.EQ.2 .AND. N.EQ.1 .AND. ICOMMA.EQ.ISTART ) THEN
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),', s: ',TMPX(N)
        ELSE
          TMPX(N) = VAR
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE (IWR,'(2X,4A,1PE11.4)')VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',TMPX(N)
          INDX = 0
          IUNS = 1
          CALL RDUNIT(UNTS,TMPX(N),INDX)
        ENDIF
!
!---    If no time acceleration record is read for a restart simulation,
!       then obtain the time step acceleration from the restart file  ---
!
        VARB = 'Time Step Accleration Factor'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
        IF( IEO.EQ.2 .AND. N.EQ.1 .AND. ICOMMA.EQ.ISTART ) THEN
          WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',TMPA(N)
        ELSE
          TMPA(N) = VAR
          WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',TMPA(N)
        ENDIF
!
!---    If no maximum number of Newton-Raphson iterations per time step
!       is read for a restart simulation, then obtain the information
!       from the restart file ---
!
        VARB = 'Maximum Newton Iterations per Step'
        CALL RDINT(ISTART,ICOMMA,CHDUM,IVAR)
        IF( IEO.EQ.2 .AND. N.EQ.1 .AND. ICOMMA.EQ.ISTART ) THEN
          WRITE (IWR,'(2X,2A,I4)') VARB(1:IVR),': ',NRIM(N)
        ELSE
          NRIM(N) = IVAR
          WRITE (IWR,'(2X,2A,I4)') VARB(1:IVR),': ',NRIM(N)
        ENDIF
!
!---    If no maximum convergence residual is read for a restart
!       simulation, then obtain the information from the restart file ---
!
        VARB = 'Maximum Convergence Residual'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
        IF( IEO.EQ.2 .AND. N.EQ.1 .AND. ICOMMA.EQ.ISTART ) THEN
          WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',RSDM(N)
        ELSE
          RSDM(N) = VAR
          WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',RSDM(N)
        ENDIF
!
!---    Tolerance criteria for sequential flow and transport
!       and geomechanics  ---
!
        IF( ISLC(50).NE.0 ) THEN
          VARB = 'Tolerance Criteria for Sequential Geomechanics'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,RSDM_GM(N))
          WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',RSDM_GM(N)
        ENDIF
!
!---    Check for additional input of a minimum time step 
!       and a time-step cut factor  ---
!
        VARB = 'Minimum Time Step & Cut Factor'
        CALL CHKDPR(ISTART,ICOMMA,CHDUM,INDX)
        IF( INDX.EQ.1 ) THEN
!
!---      If no minimum is read for a restart simulation,
!         then obtain the minimum time step from the restart file  ---
!
          VARB = 'Minimum Time Step'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
          IF( IEO.EQ.2 .AND. N.EQ.1 .AND. ICOMMA.EQ.ISTART ) THEN
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),', s: ',TMPN(N)
          ELSE
            TMPN(N) = VAR
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE (IWR,'(2X,4A,1PE11.4)')VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',TMPN(N)
            INDX = 0
            IUNS = 1
            CALL RDUNIT(UNTS,TMPN(N),INDX)
          ENDIF
!
!---      If no time step cut factor is read for a restart simulation,
!         then obtain the time step cut factor from the restart file  ---
!
          VARB = 'Time Step Cut Factor'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
          IF( IEO.EQ.2.AND.N.EQ.1.AND.ICOMMA.EQ.ISTART ) THEN
            WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',TMPC(N)
          ELSE
            TMPC(N) = VAR
            WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',TMPC(N)
          ENDIF
        ENDIF
  100 CONTINUE
!
!---  Assign initial time values  ---
!
      IF( MEPD.EQ.1 ) THEN
        TM = TMPSX
        TMMX = TMPEX
        TMZ = MOD(TM,TMPS(NEPD))
        DO 102 N = 1,NEPD
          IF( TMPS(N).LE.TMZ .AND. TMPE(N).GT.TMZ ) THEN
            DT = TMPD(N)
            DTMX = TMPX(N)
            DTMN = TMPN(N)
            DTAF = TMPA(N)
            DTCF = TMPC(N)
            NRIMX = NRIM(N)
            RSDMX = RSDM(N)
            IEPD = N
          ENDIF
  102   CONTINUE
      ELSE
        DO 104 N = 1,NEPD
          IF( TMPS(N).LT.TM ) THEN
            TM = TMPS(N)
            DT = TMPD(N)
            DTMX = TMPX(N)
            DTMN = TMPN(N)
            DTAF = TMPA(N)
            DTCF = TMPC(N)
            NRIMX = NRIM(N)
            RSDMX = RSDM(N)
            IEPD = N
          ENDIF
          IF( TMPE(N).GT.TMMX ) THEN
            TMMX = TMPE(N)
          ENDIF
  104   CONTINUE
      ENDIF
!
!---  Check for interlocking time periods  ---
!
      DO 120 N = 1,NEPD
        DO 110 M = 1,NEPD
          IF( M.NE.N ) THEN
            IF( (TMPS(N).LT.TMPS(M) .AND. TMPE(N).LT.TMPE(N)) .OR.
     &        (TMPS(N).GT.TMPS(M) .AND. TMPE(N).GT.TMPE(N)) ) THEN
              INDX = 4
              CHMSG = 'Interlocking Execution Periods'
              CALL WRMSGS( INDX )
            ENDIF
          ENDIF
  110   CONTINUE
  120 CONTINUE
!
!---  Assign initial time values  ---
!
      IF( DTAF.NE.ZERO ) DT = DT/DTAF
      DTO = DT
!
!---  Read maximum execution time, maximum clock time, and
!     maximum number of time steps  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
!
!---  Count the number of commas in this line  ---
!
      NC = 0
      ISX = 1
  130 CONTINUE
      ICX = INDEX( CHDUM(ISX:),',' )
      IF( ICX.GT.0 ) THEN
        ISX = ISX+ICX
        NC = NC+1
        GOTO 130
      ENDIF
!
!---  Skip maximum cpu and maximum clock time input  ---
!
      IF( NC.LE.2 ) GOTO 132
      VARB = 'Maximum CPU Time'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,CPUMX)
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      WRITE (IWR,'(/,4A,1PE11.4)') VARB(1:IVR),', ',
     &  UNTS(1:NCH),': ',CPUMX
      INDX = 0
      IUNS = 1
      CALL RDUNIT(UNTS,CPUMX,INDX)
!
!---  Skip maximum clock time input  ---
!
      VARB = 'Maximum Clock Time, '
      CALL RDDPR(ISTART,ICOMMA,CHDUM,CLKMX)
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      WRITE (IWR,'(4A,1PE11.4)') VARB(1:IVR),', ',UNTS(1:NCH),': ',CLKMX
      INDX = 0
      IUNS = 1
      CALL RDUNIT(UNTS,CLKMX,INDX)
  132 CONTINUE
      VARB = 'Maximum Number of Time Steps'
      CALL RDINT(ISTART,ICOMMA,CHDUM,MXSTEP)
      WRITE(FORM(3:3),'(I1)') ICOUNT(MXSTEP)
      WRITE (IWR,'(/,2A,$)') VARB(1:IVR),': '
      WRITE(IWR,FORM) MXSTEP
!
!---  Reference NAPL kinematic viscosity for STOMP-OS  ---
!
      IF( IOM.EQ.50 .OR. IOM.EQ.51 .OR. IOM.EQ.52 ) THEN
!        CALL RDINPL( CHDUM )
!        CALL LCASE( CHDUM )
!        ISTART = 1
!        VARB = 'NAPL Kinematic Viscosity at 50 C'
!        CALL RDDPR(ISTART,ICOMMA,CHDUM,VISCO(4))
!        UNTS = 'm^2/s'
!        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
!        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
!     &    ': ',VISCO(4)
!        INDX = 0
!        IUNM = 2
!        IUNS = -1
!        CALL RDUNIT(UNTS,VISCO(4),INDX)
!        WRITE(IWR,'(A,1PE11.4,A)') ' (',VISCO(4),', m^2/s)'
!        VISCO(4) = VISCO(4)*1.D+6
!        TKX = 5.D+1 + TABS
        VISCO(2) = -3.5D+0
        VISCO(3) = 7.D-1
!        VISCO(1) = LOG10(LOG10(VISCO(4)+VISCO(3))) 
!     &    - VISCO(2)*LOG10(TKX)
!        WRITE(IWR,'(4X,A,1PE11.4)') 
!     &    'ASTM Kinematic Viscosity a Parameter: ',VISCO(1)
        WRITE(IWR,'(4X,A,1PE11.4)') 
     &    'ASTM Kinematic Viscosity b Parameter: ',VISCO(2)
        WRITE(IWR,'(4X,A,1PE11.4)') 
     &    'ASTM Kinematic Viscosity f Parameter: ',VISCO(3)
      ENDIF
!
!---  Reactive transport sequence iterations  ---
!
      IF( ISLC(40).EQ.1 ) THEN
        ISVT = 1
        NRTSI = 1
        CALL CHKINT( ISTART,ICOMMA,CHDUM,INDX )
        IF( INDX.EQ.1 ) THEN
          VARB = 'Number of Reactive Transport Sequence Iterations'
           IDFLT = 1
           CALL RDINT(ISTART,ICOMMA,CHDUM,NRTSI)
          WRITE (IWR,'(2A,I6)') VARB(1:IVR),': ',NRTSI
        ENDIF
      ENDIF
!
!---  Solution Control Options  ---
!
      WRITE(IWR,'(/,A)') 'Solution Control Options'
      IF( IOM.EQ.1 .OR. IOM.EQ.11 .OR. IOM.EQ.5512 ) GOTO 180
!
!---  Read Aqueous Diffusion Option ---
!
      IF( IAQU.EQ.1 ) THEN
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Aqueous Phase Diffusion Option'
        ADUM = 'zero'
        IDFLT = 1
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'zero').NE.0 ) THEN
          ISLC(4) = 0
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),': Zero Diffusion'
        ELSEIF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
          ISLC(4) = 1
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),': Constant Coefficients'
!
!---      STOMP-EOR  ---
!
          IF( IOM.EQ.43 ) THEN
              VARB = 'CO2 Diffusion Coefficient'
              CALL RDDPR(ISTART,ICOMMA,CHDUM,DFLAC)
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(4X,4A,1PE11.4)') VARB(1:IVR),': ',
     &          UNTS(1:NCH),': ',DFLAC
              INDX = 0
              IUNM = 2
              IUNS = -1
              CALL RDUNIT(UNTS,DFLAC,INDX)
              VARB = 'Salt Diffusion Coefficient'
              CALL RDDPR(ISTART,ICOMMA,CHDUM,DFLSC)
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(4X,4A,1PE11.4)') VARB(1:IVR),': ',
     &          UNTS(1:NCH),': ',DFLSC
              INDX = 0
              IUNM = 2
              IUNS = -1
              CALL RDUNIT(UNTS,DFLSC,INDX)
          ELSE      
            IF( IEQA.GT.0 .OR. IEQGC(1).GT.0 ) THEN
              IF( IOM.EQ.32 .OR. IOM.EQ.33 ) THEN
                VARB = 'CO2 Diffusion Coefficient'
              ELSEIF( IOM.EQ.40 ) THEN
                VARB = 'Gas Diffusion Coefficient'
              ELSE
                VARB = 'Air Diffusion Coefficient'
              ENDIF
              CALL RDDPR(ISTART,ICOMMA,CHDUM,DFLAC)
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(4X,4A,1PE11.4)') VARB(1:IVR),': ',
     &          UNTS(1:NCH),': ',DFLAC
              INDX = 0
              IUNM = 2
              IUNS = -1
              CALL RDUNIT(UNTS,DFLAC,INDX)
            ENDIF
            IF( IEQO.GT.0 ) THEN
              IF( IOM.GE.37 .AND. IOM.LE.39 ) THEN
                VARB = 'CH4 Diffusion Coefficient'
              ELSE
              VARB = 'Oil Diffusion Coefficient'
              ENDIF         
              CALL RDDPR(ISTART,ICOMMA,CHDUM,DFLOC)
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(4X,4A,1PE11.4)') VARB(1:IVR),': ',
     &          UNTS(1:NCH),': ',DFLOC
              INDX = 0
              IUNM = 2
              IUNS = -1
              CALL RDUNIT(UNTS,DFLOC,INDX)
            ENDIF
            IF( IEQN.GT.0 ) THEN
              VARB = 'N2 Diffusion Coefficient'
              CALL RDDPR(ISTART,ICOMMA,CHDUM,DFLNC)
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(4X,4A,1PE11.4)') VARB(1:IVR),': ',
     &          UNTS(1:NCH),': ',DFLNC
              INDX = 0
              IUNM = 2
              IUNS = -1
              CALL RDUNIT(UNTS,DFLNC,INDX)
            ENDIF
            IF( IEQS.GT.0 ) THEN
              VARB = 'Salt Diffusion Coefficient'
              CALL RDDPR(ISTART,ICOMMA,CHDUM,DFLSC)
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(4X,4A,1PE11.4)') VARB(1:IVR),': ',
     &          UNTS(1:NCH),': ',DFLSC
              INDX = 0
              IUNM = 2
              IUNS = -1
              CALL RDUNIT(UNTS,DFLSC,INDX)
            ENDIF
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'variable').NE.0 ) THEN
          ISLC(4) = 2
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),': Variable Coefficients'
        ENDIF
!
!---    Aqueous diffusion gradient option  ---
!
        VARB = 'Aqueous Diffusion Gradient Option'
        CALL CHKCHR(ISTART,ICOMMA,CHDUM,INDX)
        IF( INDX.EQ.1 ) THEN
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          IF( INDEX(ADUM(1:),'mol').NE.0 .AND.
     &      INDEX(ADUM(1:),'frac').NE.0 ) THEN
            ISLC(27) = 1
            WRITE(IWR,'(/,A)') VARB(1:IVR),': Mole Fraction Gradient'
          ENDIF
        ENDIF
        IF( IOM.EQ.3 ) ISLC(27) = 1
      ENDIF
!
!---  Read Gas Diffusion Option ---
!
      IF( IGAS.EQ.1 ) THEN
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Vapor Diffusion Option'
        ADUM = 'zero'
        IDFLT = 1
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'zero').NE.0 ) THEN
          ISLC(2) = 0
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),' : Zero Diffusion'
        ELSEIF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
          ISLC(2) = 1
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),' : Constant Coefficients'
          IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
            IF( IEQA.GT.0 ) THEN
              VARB = 'CO2 Gas Diffusion Coefficient'
              CALL RDDPR(ISTART,ICOMMA,CHDUM,DFGAC)
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(4X,4A,1PE11.4)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',DFGAC
              INDX = 0
              IUNM = 2
              IUNS = -1
              CALL RDUNIT(UNTS,DFGAC,INDX)
            ENDIF
            IF( IEQO.GT.0 ) THEN
              VARB = 'CH4 Gas Diffusion Coefficient'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,DFGOC)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(4X,4A,1PE11.4)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',DFGOC
            INDX = 0
            IUNM = 2
            IUNS = -1
            CALL RDUNIT(UNTS,DFGOC,INDX)
            ENDIF
            IF( IEQN.GT.0 ) THEN
              VARB = 'N2 Gas Diffusion Coefficient'
              CALL RDDPR(ISTART,ICOMMA,CHDUM,DFGNC)
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(4X,4A,1PE11.4)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',DFGNC
              INDX = 0
              IUNM = 2
              IUNS = -1
              CALL RDUNIT(UNTS,DFGNC,INDX)
            ENDIF
            VARB = 'Water Gas Diffusion Coefficient'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,DFGWC)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(4X,4A,1PE11.4)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',DFGWC
            INDX = 0
            IUNM = 2
            IUNS = -1
            CALL RDUNIT(UNTS,DFGWC,INDX)
!
!---      STOMP-EOR  ---
!
          ELSEIF( IOM.EQ.43 ) THEN
            VARB = 'Gas Diffusion Coefficient'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,DFGAC)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(4X,4A,1PE11.4)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',DFGAC
            INDX = 0
            IUNM = 2
            IUNS = -1
            CALL RDUNIT(UNTS,DFGAC,INDX)
          ELSE
            IF( IEQW.GT.0 ) THEN
              VARB = 'Water Vapor Diffusion Coefficient'
              CALL RDDPR(ISTART,ICOMMA,CHDUM,DFGWC)
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(4X,4A,1PE11.4)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',DFGWC
              INDX = 0
              IUNM = 2
              IUNS = -1
              CALL RDUNIT(UNTS,DFGWC,INDX)
            ENDIF
            IF( IEQO.GT.0 ) THEN
              VARB = 'Oil Vapor Diffusion Coefficient'
              CALL RDDPR(ISTART,ICOMMA,CHDUM,DFGOC)
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(4X,4A,1PE11.4)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',DFGOC
              INDX = 0
              IUNM = 2
              IUNS = -1
              CALL RDUNIT(UNTS,DFGOC,INDX)
            ENDIF
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'variable').NE.0 ) THEN
          ISLC(2) = 2
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),': Variable Coefficients'
        ELSEIF( INDEX(ADUM(1:),'enhanced').NE.0 ) THEN
          ISLC(2) = 3
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),': Enhanced Vapor ' //
     &      'Diffusion Coefficients'
          IF( IEQW.GT.0 ) THEN
            VARB = 'Clay Mass Fraction'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,DFGWC)
            WRITE(IWR,'(4X,4A,1PE11.4)') VARB(1:IVR),': ',DFGWC
          ENDIF
        ENDIF
!
!---  Particle-displacing bubble to gas-phase effective air mass
!     transfer coefficient for air  ---
!
        IF( ISLC(13).EQ.1 ) THEN
          VARB = 'Bubble to Gas Effective Air Mass ' // 
     &      'Transfer Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,DFGOC)
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(4X,4A,1PE11.4)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',DFGOC
          INDX = 0
          IUNM = 2
          IUNS = -1
          CALL RDUNIT(UNTS,DFGOC,INDX)
        ENDIF
!
!---    Gas diffusion gradient option  ---
!
        VARB = 'Gas Diffusion Gradient Option'
        CALL CHKCHR(ISTART,ICOMMA,CHDUM,INDX)
        IF( INDX.EQ.1 ) THEN
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          IF( INDEX(ADUM(1:),'mol').NE.0 .AND.
     &      INDEX(ADUM(1:),'frac').NE.0 ) THEN
            ISLC(28) = 1
            WRITE(IWR,'(/,A)') VARB(1:IVR),': Mole Fraction Gradient'
          ENDIF
        ENDIF
        IF( IOM.EQ.3 ) ISLC(28) = 1
      ENDIF
!
!---  Read Nonaqueous-Liquid Diffusion Option ---
!
      IF( IOM.EQ.38 .OR. IOM.EQ.39 ) THEN
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Nonaqueous-Liquid Diffusion Option'
        ADUM = 'zero'
        IDFLT = 1
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'zero').NE.0 ) THEN
          ISLC(66) = 0
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),': Zero Diffusion'
        ELSEIF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
          ISLC(66) = 1
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),': Constant Coefficients'
          IF( IEQA.GT.0 ) THEN
            VARB = 'CO2 Nonaqueous-Liquid Diffusion Coefficient'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,DFNAC)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(4X,4A,1PE11.4)') VARB(1:IVR),': ',
     &        UNTS(1:NCH),': ',DFNAC
            INDX = 0
            IUNM = 2
            IUNS = -1
            CALL RDUNIT(UNTS,DFNAC,INDX)
          ENDIF
          IF( IEQO.GT.0 ) THEN
            VARB = 'CH4 Nonaqueous-Liquid Diffusion Coefficient'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,DFNOC)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(4X,4A,1PE11.4)') VARB(1:IVR),': ',
     &        UNTS(1:NCH),': ',DFNOC
            INDX = 0
            IUNM = 2
            IUNS = -1
            CALL RDUNIT(UNTS,DFNOC,INDX)
          ENDIF
          IF( IEQN.GT.0 ) THEN
            VARB = 'N2 Nonaqueous-Liquid Diffusion Coefficient'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,DFNNC)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(4X,4A,1PE11.4)') VARB(1:IVR),': ',
     &        UNTS(1:NCH),': ',DFNNC
            INDX = 0
            IUNM = 2
            IUNS = -1
            CALL RDUNIT(UNTS,DFNNC,INDX)
          ENDIF
          VARB = 'Water Nonaqueous-Liquid Diffusion Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,DFNWC)
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(4X,4A,1PE11.4)') VARB(1:IVR),': ',
     &      UNTS(1:NCH),': ',DFNWC
          INDX = 0
          IUNM = 2
          IUNS = -1
          CALL RDUNIT(UNTS,DFNWC,INDX)
        ELSEIF( INDEX(ADUM(1:),'variable').NE.0 ) THEN
          ISLC(66) = 2
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),': Variable Coefficients'
        ENDIF
!
!---  STOMP-EOR  ---
!
      ELSEIF( IOM.EQ.43 ) THEN
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Nonaqueous-Liquid Diffusion Option'
        ADUM = 'zero'
        IDFLT = 1
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'zero').NE.0 ) THEN
          ISLC(66) = 0
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),': Zero Diffusion'
        ELSEIF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
          ISLC(66) = 1
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),': Constant Coefficients'
          VARB = 'Nonaqueous-Liquid Diffusion Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,DFNAC)
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(4X,4A,1PE11.4)') VARB(1:IVR),': ',
     &      UNTS(1:NCH),': ',DFNAC
          INDX = 0
          IUNM = 2
          IUNS = -1
          CALL RDUNIT(UNTS,DFNAC,INDX)
        ELSEIF( INDEX(ADUM(1:),'variable').NE.0 ) THEN
          ISLC(66) = 2
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),': Variable Coefficients'
        ENDIF
      ENDIF
!
!---  Read Hydrate Dissociation Option  ---
!
      IF( IOM.EQ.36 .OR. IOM.EQ.37 ) THEN
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Hydrate Dissociation Option'
        ADUM = 'equilibrium'
        IDFLT = 1
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'kinetic').NE.0 ) THEN
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),': Kinetic'
          ISLC(31) = 1
          VARB = 'Hydrate Formation-Dissociation Rate Constant'
          IDFLT = 1
          CHKN(2) = 1.925D-5
          CALL RDDPR(ISTART,ICOMMA,CHDUM,CHKN(2))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',CHKN(2)
          INDX = 0
          IUNS = -1
          CALL RDUNIT(UNTS,CHKN(2),INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',CHKN(2),', 1/s)'
          CHKN(2) = MAX( CHKN(2),EPSL )
        ELSE
          WRITE(IWR,'(2X,A,1X,A)') VARB(1:IVR),': Equilibrium '
        ENDIF

        WRITE(IWR,'(2X,A)')
        VARB = 'Hydrate Guest-Molecule-Exchange Option: Equilibrium'

      ENDIF  
!
!---  Read hydrate guest-molecule exchange rate constant,
!     hydrate formation rate constant, and 
!     hydrate dissociation rate constant
!     hydrate formation/dissociation rate saturation exponent ---
!
      IF( IOM.EQ.38 .OR. IOM.EQ.39 ) THEN
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Hydrate Guest-Molecule-Exchange Rate Constant'
        IDFLT = 1
!        CHKN(4) = 1.D-4
        CALL RDDPR(ISTART,ICOMMA,CHDUM,CHKN(4))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',CHKN(4)
        INDX = 0
        IUNS = 1
        IUNMOL = 1
        IUNM = -1
        IUNKG = -1
        CALL RDUNIT(UNTS,CHKN(4),INDX)
!   --- kinetic unit update -- Ruprecht 3/20/215
!   --- unit are defined b/c pa = kg/m s2
        WRITE(IWR,'(A,1PE11.4,A)') ' (',CHKN(4),', kmol/m^2 Pa s)'
        VARB = 'Hydrate Formation Rate Constant'
        IDFLT = 1
!        CHKN(2) = 1.D-2
        CALL RDDPR(ISTART,ICOMMA,CHDUM,CHKN(2))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',CHKN(2)
        INDX = 0
        IUNS = 1
        IUNMOL = 1
        IUNM = -1
        IUNKG = -1
        CALL RDUNIT(UNTS,CHKN(2),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',CHKN(2),', kmol/m^2 Pa s)'
        VARB = 'Hydrate Intrinsic Kinetic Constant for Dissociation'
        IDFLT = 1
!        CHKN(3) = 1.D-2
        CALL RDDPR(ISTART,ICOMMA,CHDUM,CHKN(3))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',CHKN(3)
        INDX = 0
        IUNS = 1
        IUNMOL = 1
        IUNM = -1
        IUNKG = -1
        CALL RDUNIT(UNTS,CHKN(3),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',CHKN(3),', kmol/m^2 Pa s)'
        VARB = 'Hydrate CO2/CH4 Exchange Factor'
        IDFLT = 1
        CHKN(6) = 1.D+0
        CALL RDDPR(ISTART,ICOMMA,CHDUM,CHKN(6))
        WRITE(IWR,'(2A,1PE11.4)') VARB(1:IVR),': ',CHKN(6)
        VARB = 'Hydrate N2/CH4 Exchange Factor'
        IDFLT = 1
        CHKN(7) = 1.D+0
        CALL RDDPR(ISTART,ICOMMA,CHDUM,CHKN(7))
        WRITE(IWR,'(2A,1PE11.4)') VARB(1:IVR),': ',CHKN(7)
!        VARB = 'Hydrate CH4 Exchange Factor'
!        IDFLT = 1
!        CHKN(8) = 1.D+0
!        CALL RDDPR(ISTART,ICOMMA,CHDUM,CHKN(8))
!        WRITE(IWR,'(2A,1PE11.4)') VARB(1:IVR),': ',CHKN(8)
        VARB = 'Hydrate Exchange Model Option'
        ADUM = 'mole fraction'
        IDFLT = 1
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'vapor pressure').NE.0 ) THEN
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),': Vapor Pressure Difference'
          ISLC(73) = 1
        ELSEIF( INDEX(ADUM(1:),'mole fraction').NE.0 ) THEN
          WRITE(IWR,'(2X,2A)') VARB(1:IVR),': Mole Fraction Difference'
          ISLC(73) = 0
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Hydrate Exchange Model Option: ' // 
     &      ADUM(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
       ENDIF
!
!---  Read Henry's coefficient factor  ---
!
      IF( IOM.EQ.38 ) THEN
        VARB = 'Henry''s Coefficient Factor'
        IDFLT = 1
        CHKN(1) = 1.D+1
        CALL RDDPR(ISTART,ICOMMA,CHDUM,CHKN(1))
        WRITE(IWR,'(2A,1PE11.4)') VARB(1:IVR),': ',CHKN(1)
      ENDIF
  180 CONTINUE
!
!---  Read constant aqueous density  ---
!
      IF( ISLC(53).EQ.1 ) THEN
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        IDFLT = 1
        VARB = 'Aqueous Density'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RHOLI)
        IDFLT = 1
        UNTS = 'kg/m^3'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &    ': ',RHOLI
        INDX = 0
        IUNM = -3
        IUNKG = 1
        CALL RDUNIT(UNTS,RHOLI,INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',RHOLI,', kg/m^3)'
      ENDIF
!
!---  Read constant supersaturation factor  ---
!
      IF( ISLC(54).EQ.1 ) THEN
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        IDFLT = 1
        VARB = 'Supersaturation Factor'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VSLC(1))
        WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &    ': ',VSLC(1)
      ENDIF
!
!---  CO2 and CO2e options  ---
!
      IF( IOM.EQ.32 .OR. IOM.EQ.33 ) THEN
!
!---    Read fluid density and viscosity  ---
!
        IF( ISLC(9).EQ.1 ) THEN
          CALL RDINPL( CHDUM )
          CALL LCASE( CHDUM )
          ISTART = 1
          VARB = 'Aqueous Density'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,RHOLI)
          IDFLT = 1
          UNTS = 'kg/m^3'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',RHOLI
          INDX = 0
          IUNM = -3
          IUNKG = 1
          CALL RDUNIT(UNTS,RHOLI,INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',RHOLI,', kg/m^3)'
          VARB = 'Aqueous Viscosity'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,VISLI)
          UNTS = 'pa s'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',VISLI
          INDX = 0
          IUNM = -1
          IUNKG = 1
          IUNS = -1
          CALL RDUNIT(UNTS,VISLI,INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',VISLI,', Pa s)'
          VARB = 'Gas Density'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,RHOGI)
          UNTS = 'kg/m^3'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',RHOGI
          INDX = 0
          IUNM = -3
          IUNKG = 1
          CALL RDUNIT(UNTS,RHOLG,INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',RHOGI,', kg/m^3)'
          VARB = 'Gas Viscosity'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,VISGI)
          UNTS = 'pa s'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',VISGI
          INDX = 0
          IUNM = -1
          IUNKG = 1
          IUNS = -1
          CALL RDUNIT(UNTS,VISGI,INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',VISGI,', Pa s)'
        ENDIF
!
!---    Fractional CO2 solubility in aqueous  ---
!
        IF( ISLC(25).EQ.2 ) THEN
          IF( ISLC(9).EQ.0 ) THEN
            CALL RDINPL( CHDUM )
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          VARB = 'Fractional CO2 Solubility Factor'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,FSFLA)
          WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',FSFLA
        ENDIF
      ENDIF
!
!---  W and WA options  ---
!
      IF( IOM.EQ.1 .OR. IOM.EQ.2 ) THEN
!
!---    Read fluid density and viscosity  ---
!
        IF( ISLC(9).EQ.1 ) THEN
          CALL RDINPL( CHDUM )
          CALL LCASE( CHDUM )
          ISTART = 1
          IDFLT = 1
          VARB = 'Fluid Density'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,RHOLI)
          IDFLT = 1
          UNTS = 'kg/m^3'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',RHOLI
          INDX = 0
          IUNM = -3
          IUNKG = 1
          CALL RDUNIT(UNTS,RHOLI,INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',RHOLI,', kg/m^3)'
          VARB = 'Fluid Viscosity'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,VISLI)
          IDFLT = 1
          UNTS = 'pa s'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',VISLI
          INDX = 0
          IUNM = -1
          IUNKG = 1
          IUNS = -1
          CALL RDUNIT(UNTS,VISLI,INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',VISLI,', Pa s)'
        ENDIF
      ENDIF
      IF( ISLC(2).GE.1 .OR. ISLC(4).GE.1
     &  .OR. IEQC.GT.0 .OR. IEQS.GT.0 .OR. 
     &  IEQD.GT.0 .OR. ISLC(40).GE.1 ) THEN
        ISLC(3) = 1
      ELSE
        ISLC(3) = 0
      ENDIF
!
!---  Read electrolyte functions  ---
!
      IF( ISLC(16).EQ.1 ) THEN
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        IDFLT = 1
        VARB = 'Electrolyte Name'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ELC_SOL)
        WRITE(IWR,'(/,2X,3A)') VARB(1:IVR),': ',ELC_SOL(1:NCH)
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Electrolyte Density Function Option'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX( ADUM(1:),'leijnse' ).NE.0 ) THEN
          WRITE(IWR,'(A)') 'Leijnse Mass-Fraction Exponential Function'
          VARB = 'Leijnse Exponential Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_DCF(1))
          IDF_ELC = 1
        ELSEIF( INDEX( ADUM(1:),'fourth' ).NE.0 ) THEN
          VARB = 'Electrolyte Density Function Units'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNM = -3
          ELC_DUN = 1.D+0
          CALL RDUNIT(UNTS,ELC_DUN,INDX)
          WRITE(IWR,'(A)') 'Fourth-Order Polynomial'
          VARB = 'Polynomial "a" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_DCF(1))
          VARB = 'Polynomial "b" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_DCF(2))
          VARB = 'Polynomial "c" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_DCF(3))
          VARB = 'Polynomial "d" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_DCF(4))
          IDF_ELC = 2
        ELSE
          INDX = 4
          NCH = INDEX( ADUM(1:),'  ' )-1
          CHMSG = 'Unrecognized Electrolyte Density Option: ' //
     &      ADUM(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Electrolyte Viscosity Function Option'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX( ADUM(1:),'leijnse' ).NE.0 ) THEN
          WRITE(IWR,'(A)') 'Leijnse Mass-Fraction Empirical Function'
          VARB = 'Electrolyte Viscosity Function Units'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNM = -3
          ELC_VUN = 1.D+0
          CALL RDUNIT(UNTS,ELC_VUN,INDX)
          VARB = 'Leijnse "a" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_VCF(1))
          VARB = 'Leijnse "b" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_VCF(2))
          VARB = 'Leijnse "c" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_VCF(3))
          VARB = 'Leijnse "d" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_VCF(4))
          IVF_ELC = 1
        ELSEIF( INDEX( ADUM(1:),'fourth' ).NE.0 ) THEN
          VARB = 'Electrolyte Viscosity Function Units'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNM = -3
          ELC_VUN = 1.D+0
          CALL RDUNIT(UNTS,ELC_VUN,INDX)
          WRITE(IWR,'(A)') 'Fourth-Order Polynomial'
          VARB = 'Polynomial "a" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_VCF(1))
          VARB = 'Polynomial "b" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_VCF(2))
          VARB = 'Polynomial "c" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_VCF(3))
          VARB = 'Polynomial "d" Coefficient'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_VCF(4))
          IVF_ELC = 2
        ELSE
          INDX = 4
          NCH = INDEX( ADUM(1:),'  ' )-1
          CHMSG = 'Unrecognized Electrolyte Viscosity Option: ' //
     &      ADUM(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Read electrolyte surface tension parameters ---
!
        IF( ISLC(55).EQ.1 ) THEN
          CALL RDINPL( CHDUM )
          CALL LCASE( CHDUM )
          ISTART = 1
          WRITE(IWR,'(A)') 'Electrolyte Surface Tension Parameters'
          VARB = 'Surface Tension without Surfactant'
          ELC_SCF(1) = 72.D-3
          IDFLT = 1
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_SCF(1))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',ELC_SCF(1)
          INDX = 0
          IUNKG = 1
          IUNS = -2
          CALL RDUNIT(UNTS,ELC_SCF(1),INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',ELC_SCF(1) ,', N/m)'
          VARB = 'Critical Micelle Concentration'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_SCF(2))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &      UNTS(1:NCH),': ',ELC_SCF(2)
          INDX = 0
          IUNMOL = 1
          IUNM = -3
          CALL RDUNIT(UNTS,ELC_SCF(2),INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',ELC_SCF(2),', kmol/m^3)'
          VARB = 'Maximum Surface Excess Concentration'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_SCF(3))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &      UNTS(1:NCH),': ',ELC_SCF(3)
          INDX = 0
          IUNMOL = 1
          IUNM = -2
          CALL RDUNIT(UNTS,ELC_SCF(3),INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',ELC_SCF(3),', kmol/m^2)'
          VARB = 'Langmuir Constant'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,ELC_SCF(4))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &      UNTS(1:NCH),': ',ELC_SCF(4)
          INDX = 0
          IUNMOL = 1
          IUNM = -3
          CALL RDUNIT(UNTS,ELC_SCF(4),INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',ELC_SCF(4),', kmol/m^3)'
        ENDIF
      ENDIF
!
!---  Read Interfacial Average Options ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      WRITE(IWR,'(/,A)') 'Interfacial Averaging Schemes:'
      VARB = 'Number of Interfacial Average Lines'
      CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
      DO 300 N = 1,NLIN
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Interfacial Average Variable Name'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        DO 210 M = 1,LCV
          IF( INDEX(ADUM(1:),CHIFV(M)(1:10)).NE.0 ) GOTO 220
  210   CONTINUE
        INDX = 4
        NCH = INDEX( ADUM(1:),'  ' )-1
        CHMSG = 'Unrecognized Interfacial Avg. Variable: '//ADUM(1:NCH)
        CALL WRMSGS( INDX )
  220   CONTINUE
        VARB = 'Interfacial Averaging Scheme'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        DO 230 MM = LCS,1,-1
          IF( INDEX(ADUM(1:),CHIFS(MM)(1:NCS(MM))).NE.0 ) GOTO 240
  230   CONTINUE
        INDX = 4
        NCH = INDEX( ADUM(1:),'  ' )-1
        CHMSG = 'Unrecognized Interfacial Avg. Scheme: '//ADUM(1:NCH)
        CALL WRMSGS( INDX )
  240   CONTINUE
        IDMN(M) = MM
        IF( MM.EQ.5 .OR. MM.EQ.7 ) THEN
          VARB = 'Weighting Factor'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,WFMN(M))
          WFMN(M) = MAX( -1.D+0,WFMN(M) )
          WFMN(M) = MIN( 1.D+0,WFMN(M) )
        ELSEIF( MM.EQ.6 ) THEN
          VARB = 'Moderation Asymptote'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,WFMN(M))
          WFMN(M) = ABS(WFMN(M))
        ENDIF
  300 CONTINUE
      DO 310 N = 1,LCV
        IF( IDMN(N).EQ.5 .OR. IDMN(N).EQ.7 ) THEN
          WRITE(IWR,'(2X,4A,1PE11.4)') CHIFV(N)(1:NCV(N)),': ',
     &    CHIFS(IDMN(N))(1:NCS(IDMN(N))),
     &    ', Weighting Factor: ',WFMN(N)
        ELSEIF( IDMN(N).EQ.6 ) THEN
          WRITE(IWR,'(2X,4A,1PE11.4)') CHIFV(N)(1:NCV(N)),': ',
     &    CHIFS(IDMN(N))(1:NCS(IDMN(N))),
     &    ', Moderation Asymptote: ',WFMN(N)
        ELSE
          WRITE(IWR,'(2X,3A)') CHIFV(N)(1:NCV(N)),': ',
     &      CHIFS(IDMN(N))(1:NCS(IDMN(N)))
        ENDIF
  310 CONTINUE
!
! --- Read minimum concentration for reactive chemistry in eckechem
!
      CMIN = 1.D-30
      IF( ISLC(59).EQ.1 )THEN
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Minimum Aqueous Concentration in Eckechem'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,CMIN)
        CMIN = MAX(CMIN,1.D-110)
        WRITE(IWR,'(A,A,1PE11.4,A)') VARB(1:IVR),': ',CMIN
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDSOLU group  ---
!
      RETURN
      END

