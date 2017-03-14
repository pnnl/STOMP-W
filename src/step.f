!----------------------Program-----------------------------------------!
!

      SUBROUTINE STEP



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
!----------------------Description-------------------------------------!
!
!     STOMP: Subsurface Transport Over Multiple Phases
!
!     This utility program reads STOMP input files and writes a
!     STOMP parameter file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 29 September 2002.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE PORMED
      USE PLT_ATM
      USE JACOB
      USE GRID
      USE GEOMECH
      USE FILES
      USE DUAL_POR
      USE COUP_WELL
      USE CONST
      USE BCV
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!



      CHARACTER*512 CHDUM
      LOGICAL FCHK
      EXTERNAL I_COUNT
!
!----------------------Executable Lines--------------------------------!
!
      ALLOCATE( SUB_LOG(1:32),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SUB_LOG'
        CALL WRMSGP( INDX )
      ENDIF
      ISUB_LOG = 1
      SUB_LOG(1) = 'STEP'
!
!---  Initialize variables  ---
!
      IDMN(2) = 4
      IDMN(3) = 4
      IDMN(4) = 4
      IDMN(8) = 4
      IDMN(9) = 4
      IDMN(10) = 4
      SMALL = 1.D-20
      BIG = 1.D+20
      ZERO = 0.D+0
      THIRD = 1.D+0/3.D+0
      ONE = 1.D+0
      EPSL = 1.D-14
      GRAV = 9.81D+0
      GPI = 3.1415926536D+0
      TENTH = 1.D-1
      TOLN = LOG(1.D+1)
      ISMALL = -32000
      IBIG = 32000
      IEVPTRNS = 0
      NSFCA = 0
      NSFCN = 0

!
!---  No input file, program exited  ---
!
      INQUIRE( FILE='input', EXIST=FCHK )
      IF( .NOT.FCHK ) THEN
        WRITE(6,'(A)') 'ERROR: Nonexistent "input" file.'
        STOP
      ELSE
        IRD = 25
        OPEN(UNIT=IRD, FILE='input', STATUS='OLD', FORM='FORMATTED')
      ENDIF
!
!---  Write banner to screen  ---
!
      WRITE(ISC,'(A,//)')' Welcome to ...'
      WRITE(ISC,'(A,/)') '                       STEP'
      WRITE(ISC,'(A)')   ' A memory allocator for the STOMP ' //
     &  'simulator.  This'
      WRITE(ISC,'(A)')   ' routine, developed by the Pacific ' //
     &  'Northwest National'
      WRITE(ISC,'(A)')   ' Laboratory, reads a STOMP input ' //
     &  'file and determines memory'
      WRITE(ISC,'(A)')   ' requirements for an execution.  ' //
     &  'These requirements'
      WRITE(ISC,'(A)')   ' are either recorded to a file or ' //
     &  'used to dynamically'
      WRITE(ISC,'(A)')   ' allocate memory.'
      WRITE(ISC,'(A)')   ' For support:  Tel: 509.372.6070'
      WRITE(ISC,'(A,/)') '               E-mail:  mark.white@pnnl.gov'
      WRITE(ISC,'(A,/)') '                     ---  SCREEN ECHO  ---'
!
!---  Simulation Title Card Parameters  ---
!
  100 CONTINUE
  110 READ(IRD,'(A)', END=190) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 110
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'simulation').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Simulation Title Card'
        CALL WRMSGP( INDX )
        CALL RD_SIMU
        REWIND(IRD)
        GOTO 200
      ELSE
        GOTO 100
      ENDIF
  190 CONTINUE
      INDX = 18
      CHMSG = 'Missing Simulation Title Card'
      CALL WRMSGP( INDX )
!
!---  Solution Control Card Parameters  ---
!
  200 CONTINUE
  210 READ(IRD,'(A)', END=290) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 210
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'solution').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Solution Control Card'
        CALL WRMSGP( INDX )
        CALL RD_SOLU
        REWIND(IRD)
        GOTO 300
      ELSE
        GOTO 200
      ENDIF
  290 CONTINUE
      INDX = 18
      CHMSG = 'Missing Solution Control Card'
      CALL WRMSGP( INDX )
!
!---  Grid Card Parameters  ---
!
  300 CONTINUE
  310 READ(IRD,'(A)', END=390) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 310
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &  INDEX(CHDUM(2:),'grid').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Grid Card'
        CALL WRMSGP( INDX )
        CALL RD_GRID
        REWIND(IRD)
        GOTO 400
      ELSE
        GOTO 300
      ENDIF
  390 CONTINUE
      INDX = 18
      CHMSG = 'Missing Grid Card'
      CALL WRMSGP( INDX )
!
!---  Rock/Soil Zonation Card Parameters  ---
!
  400 CONTINUE
  410 READ(IRD,'(A)', END=490) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 410
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &  INDEX(CHDUM(2:),'rock/soil').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Rock/Soil Zonation Card'
        CALL WRMSGP( INDX )
        CALL RD_ROCK
        REWIND(IRD)
        GOTO 500
      ELSE
        GOTO 400
      ENDIF
  490 CONTINUE
      INDX = 18
      CHMSG = 'Missing Rock/Soil Zonation Card'
      CALL WRMSGP( INDX )
!
!---  Inactive Nodes Card Parameters  ---
!
  500 CONTINUE
  510 READ(IRD,'(A)', END=590) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 510
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &  INDEX(CHDUM(2:),'inactive').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Inactive Nodes Card'
        CALL WRMSGP( INDX )
        CALL RD_INAC
        REWIND(IRD)
        GOTO 600
      ELSE
        GOTO 500
      ENDIF
  590 CONTINUE
      INDX = 1
      CHMSG = 'Missing Inactive Nodes Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
!
!---  Block Refinement Card Parameters  ---
!
  600 CONTINUE
  609 READ(IRD,'(A)', END=610) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 609
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &  INDEX(CHDUM(2:),'block').NE.0 .AND.
     &  INDEX(CHDUM(2:),'refine').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Block Refinement Card'
        CALL WRMSGP( INDX )
        CALL RD_BR
        REWIND(IRD)
        GOTO 650
      ELSE
        GOTO 600
      ENDIF
  610 CONTINUE
      INDX = 1
      CHMSG = 'Missing Block Refinement Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
!
!---  Search input file for mechanical properties card  ---
!
  650 CONTINUE
  659 READ(IRD,'(A)', END=660) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 659
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'mechanical').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Mechanical Properties Card'
        CALL WRMSGP( INDX )
        REWIND(IRD)
        GOTO 700
      ELSE
        GOTO 650
      ENDIF
  660 CONTINUE
      INDX = 18
      CHMSG = 'Missing Mechanical Properties Card'
      CALL WRMSGP( INDX )
!
!---  Search input file for hydraulic properties card  ---
!
  700 CONTINUE
  709 READ(IRD,'(A)', END=710) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 709
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'hydraulic').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Hydraulic Properties Card'
        CALL WRMSGP( INDX )
        REWIND(IRD)
        GOTO 800
      ELSE
        GOTO 700
      ENDIF
  710 CONTINUE
      INDX = 18
      CHMSG = 'Missing Hydraulic Properties Card'
      CALL WRMSGP( INDX )
!
!---  Search input file for saturation function card  ---
!
  800 CONTINUE
  809 READ(IRD,'(A)', END=810) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 809
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'saturation').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Saturation Function Card'
        CALL WRMSGP( INDX )
        CALL RD_SP
        REWIND(IRD)
        GOTO 900
      ELSE
        GOTO 800
      ENDIF
  810 CONTINUE
      INDX = 18
      CHMSG = 'Missing Saturation Function Card'
      CALL WRMSGP( INDX )
!
!---  Search input file for aqueous relative permeability card  ---
!
  900 CONTINUE
      IF( LL.NE.0 ) THEN
  909   READ(IRD,'(A)', END=910) CHDUM
        IF( CHDUM(1:1).EQ.'#' ) GOTO 909
        CALL L_CASE( CHDUM )
        IF( CHDUM(1:1).EQ.'~' .AND.
     &    ( INDEX(CHDUM(2:),'aqueous').NE.0 .AND.
     &      INDEX(CHDUM(2:),'rel').NE.0 ) ) THEN
          INDX = 1
          CHMSG = 'Reading Aqueous Relative Permeability Card'
          CALL WRMSGP( INDX )
          CALL RD_LRP
          REWIND(IRD)
          GOTO 1100
        ELSEIF( CHDUM(1:1).EQ.'~' .AND.
     &    (INDEX(CHDUM(2:),'3').NE.0 .OR.
     &    INDEX(CHDUM(2:),'three').NE.0) .AND.
     &    INDEX(CHDUM(2:),'phase').NE.0 .AND.
     &    INDEX(CHDUM(2:),'relative').NE.0 .AND.
     &    INDEX(CHDUM(2:),'perm').NE.0 ) THEN
          INDX = 1
          CHMSG = 'Reading 3-Phase Relative Permeability Card for ' //
     &      'Aqueous Relative Permeability'
          CALL WRMSGP( INDX )
          REWIND(IRD)
          GOTO 1100
        ELSE
          GOTO 900
        ENDIF
  910   CONTINUE
        INDX = 18
        CHMSG = 'Missing Aqueous Relative Permeability Card'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Search input file for gas relative permeability card  ---
!
 1100 CONTINUE
      IF( LG.NE.0 ) THEN
 1109   READ(IRD,'(A)', END=1110) CHDUM
        IF( CHDUM(1:1).EQ.'#' ) GOTO 1109
        CALL L_CASE( CHDUM )
        IF( CHDUM(1:1).EQ.'~' .AND.
     &    ( INDEX(CHDUM(2:),'gas').NE.0 .AND.
     &      INDEX(CHDUM(2:),'rel').NE.0  ) ) THEN
          INDX = 1
          CHMSG = 'Reading Gas Relative Permeability Card'
          CALL WRMSGP( INDX )
          CALL RD_GRP
          REWIND(IRD)
          GOTO 1200
        ELSEIF( CHDUM(1:1).EQ.'~' .AND.
     &    (INDEX(CHDUM(2:),'3').NE.0 .OR.
     &    INDEX(CHDUM(2:),'three').NE.0) .AND.
     &    INDEX(CHDUM(2:),'phase').NE.0 .AND.
     &    INDEX(CHDUM(2:),'relative').NE.0 .AND.
     &    INDEX(CHDUM(2:),'perm').NE.0 ) THEN
          INDX = 1
          CHMSG = 'Reading 3-Phase Relative Permeability Card for ' //
     &      'Gas Relative Permeability'
          CALL WRMSGP( INDX )
          REWIND(IRD)
          GOTO 1200
        ELSE
          GOTO 1109
        ENDIF
 1110   CONTINUE
        INDX = 18
        CHMSG = 'Missing Gas Relative Permeability Card'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Search input file for a napl relative permeability card  ---
!
 1200 CONTINUE
      IF( LN.NE.0 ) THEN
 1209   READ(IRD,'(A)', END=1210) CHDUM
        IF( CHDUM(1:1).EQ.'#' ) GOTO 1209
        CALL L_CASE( CHDUM )
        IF( CHDUM(1:1).EQ.'~' .AND.
     &    (( INDEX(CHDUM(2:),'nonaqueous').NE.0 .AND.
     &      INDEX(CHDUM(2:),'rel').NE.0 ) .OR.
     &      ( INDEX(CHDUM(2:),'napl').NE.0 .AND.
     &      INDEX(CHDUM(2:),'rel').NE.0 ) .OR.
     &      ( INDEX(CHDUM(2:),'liquid').NE.0 .AND.
     &      INDEX(CHDUM(2:),'co2').NE.0 .AND.
     &      INDEX(CHDUM(2:),'rel').NE.0 ) ) ) THEN
          INDX = 1
          CHMSG = 'Reading NAPL Relative Permeability Card'
          CALL WRMSGP( INDX )
          CALL RD_NRP
          REWIND(IRD)
          GOTO 1300
        ELSEIF( CHDUM(1:1).EQ.'~' .AND.
     &    (INDEX(CHDUM(2:),'3').NE.0 .OR.
     &    INDEX(CHDUM(2:),'three').NE.0) .AND.
     &    INDEX(CHDUM(2:),'phase').NE.0 .AND.
     &    INDEX(CHDUM(2:),'relative').NE.0 .AND.
     &    INDEX(CHDUM(2:),'perm').NE.0 ) THEN
          INDX = 1
          CHMSG = 'Reading 3-Phase Relative Permeability Card for ' //
     &      'NAPL Relative Permeability'
          CALL WRMSGP( INDX )
          REWIND(IRD)
          GOTO 1300
        ELSE
          GOTO 1209
        ENDIF
 1210   CONTINUE
        INDX = 18
        CHMSG = 'Missing NAPL Relative Permeability Card'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Search input file for NAPL components properties card  ---
!
 1300 CONTINUE
 1309 READ(IRD,'(A)', END=1310) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 1309
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'napl').NE.0 .AND.
     &    INDEX(CHDUM(2:),'comp').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading NAPL Components Properties Card'
        CALL WRMSGP( INDX )
        CALL RD_NCP
        REWIND(IRD)
        GOTO 1400
      ELSE
        GOTO 1309
      ENDIF
 1310 CONTINUE
      INDX = 1
      CHMSG = 'Missing NAPL Components Properties Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
!
!---  Search input file for gas component properties card  ---
!
 1400 CONTINUE
 1409 READ(IRD,'(A)', END=1410) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 1409
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'gas').NE.0 .AND.
     &    INDEX(CHDUM(2:),'component').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Gas Components Properties Card'
        CALL WRMSGP( INDX )
        CALL RD_GCP
        REWIND(IRD)
        GOTO 1500
      ELSE
        GOTO 1409
      ENDIF
 1410 CONTINUE
      INDX = 1
      CHMSG = 'Missing Gas Components Properties Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
!
!---  Search input file for geomechanics boundary condition card  ---
!
 1500 CONTINUE
 1509 READ(IRD,'(A)', END= 1510) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 1509
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'geomech').NE.0 .AND.
     &    INDEX(CHDUM(2:),'bound').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Geomechanics Boundary Conditions Card'
        CALL WRMSGP( INDX )
        CALL RD_GMBC
        REWIND(IRD)
        GOTO 1600
      ELSE
        GOTO 1509
      ENDIF
 1510 CONTINUE
      IF( LM.EQ.0 ) THEN
        REWIND(IRD)
      ELSE
        INDX = 18
        CHMSG = 'Missing Geomechanics Boundary Conditions Card'
        CALL WRMSGP( INDX )
        REWIND(IRD)
      ENDIF
!
!---  Search input file for petroleum components properties card  ---
!
 1600 CONTINUE
 1609 READ(IRD,'(A)', END=1610) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 1609
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'petrol').NE.0 .AND.
     &    INDEX(CHDUM(2:),'component').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Petroleum Components Properties Card'
        CALL WRMSGP( INDX )
        CALL RD_PCP
        REWIND(IRD)
        GOTO 1900
      ELSE
        GOTO 1609
      ENDIF
 1610 CONTINUE
      INDX = 1
      CHMSG = 'Missing Petroleum Components Properties Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
!
!---  Search input file for scaling card  ---
!
 1900 CONTINUE
      IF( ISLC(19).EQ.1 ) THEN
 1909   READ(IRD,'(A)', END=1910) CHDUM
        IF( CHDUM(1:1).EQ.'#' ) GOTO 1909
        CALL L_CASE( CHDUM )
        IF( CHDUM(1:1).EQ.'~' .AND.
     &      INDEX(CHDUM(2:),'scaling').NE.0 ) THEN
          INDX = 1
          CHMSG = 'Reading Scaling Factors Card'
          CALL WRMSGP( INDX )
          REWIND(IRD)
          GOTO 2000
        ELSE
          GOTO 1909
        ENDIF
 1910   CONTINUE
        INDX = 18
        CHMSG = 'Missing Scaling Factors Card'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Search input file for solute/fluid interaction card --
!
 2000 CONTINUE
 2009 READ(IRD,'(A)', END=2010) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 2009
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'solute/fluid').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Solute/Fluid Interactions Card'
        CALL WRMSGP( INDX )
        CALL RD_TF
        REWIND(IRD)
        GOTO 2100
      ELSE
        GOTO 2000
      ENDIF
 2010 CONTINUE
      IF( LC.EQ.0 ) THEN
        REWIND(IRD)
      ELSE
        INDX = 18
        CHMSG = 'Missing Solute/Fluid Interactions Card'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Search input file for solute/porous media interaction card --
!
 2100 CONTINUE
 2109 READ(IRD,'(A)', END=2110) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 2109
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'solute/porous').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Solute/Porous Media Interactions Card'
        CALL WRMSGP( INDX )
        REWIND(IRD)
        GOTO 3000
      ELSE
        GOTO 2100
      ENDIF
 2110 CONTINUE
      IF( LC.EQ.0 .AND. LR.EQ.0 ) THEN
        REWIND(IRD)
      ELSE
        INDX = 18
        CHMSG = 'Missing Solute/Porous Media Interactions Card'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Search input file for surface cover card --
!
 3000 CONTINUE
 3010 READ(IRD,'(A)', END= 3020) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 3010
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &  INDEX(CHDUM(2:),'cover').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Surface Cover Card'
        CALL WRMSGP( INDX )
        CALL RD_SFCOV
        IEVPTRNS = 1
        REWIND(IRD)
        GOTO 3030
      ELSE
        GOTO 3000
      ENDIF
 3020 CONTINUE
      INDX = 1
      CHMSG = 'Missing Surface Cover Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
 3030 CONTINUE
!
!---  Search input file for plant card --
!
 3050 CONTINUE
 3060 READ(IRD,'(A)', END= 3070) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 3060
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'plant').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Plant Card'
        CALL WRMSGP( INDX )
        CALL RD_PLANT
        REWIND(IRD)
        GOTO 3080
      ELSE
        GOTO 3050
      ENDIF
 3070 CONTINUE
      IF( IEVPTRNS.EQ.0 ) THEN
        INDX = 1
        CHMSG = 'Missing Plant Card'
        CALL WRMSGP( INDX )
      ELSE
        INDX = 18
        CHMSG = 'Missing Plant Card'
        CALL WRMSGP( INDX )
      ENDIF
      REWIND(IRD)
 3080 CONTINUE
!
!---  Search input file for atmospheric conditions card --
!
 3100 CONTINUE
 3110 READ(IRD,'(A)', END= 3120) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 3110
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'atmospheric').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Atmospheric Conditions Card'
        CALL WRMSGP( INDX )
        CALL RD_ATMOS
        REWIND(IRD)
        GOTO 3130
      ELSE
        GOTO 3100
      ENDIF
 3120 CONTINUE
      IF( IEVPTRNS.EQ.0 ) THEN
        INDX = 1
        CHMSG = 'Missing Atmospheric Conditions Card'
        CALL WRMSGP( INDX )
      ELSE
        INDX = 18
        CHMSG = 'Missing Atmospheric Conditions Card'
        CALL WRMSGP( INDX )
      ENDIF
      REWIND(IRD)
 3130 CONTINUE
!
!---  Search input file for initial conditions card --
!
 4000 CONTINUE
 4009 READ(IRD,'(A)', END=4010) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 4009
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'initial').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Initial Conditions Card'
        CALL WRMSGP( INDX )
        REWIND(IRD)
        GOTO 4100
      ELSE
        GOTO 4000
      ENDIF
 4010 CONTINUE
      IF( IEO.EQ.2 ) THEN
        INDX = 1
        CHMSG = 'Missing Initial Conditions Card'
        CALL WRMSGP( INDX )
        REWIND(IRD)
      ELSE
        INDX = 18
        CHMSG = 'Missing Initial Conditions Card'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Search input file for boundary conditions card --
!
 4100 CONTINUE
 4109 READ(IRD,'(A)', END=4110) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 4109
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &  INDEX(CHDUM(2:),'boundary').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Boundary Conditions Card'
        CALL WRMSGP( INDX )
        CALL RD_BC
        REWIND(IRD)
        GOTO 4200
      ELSE
        GOTO 4100
      ENDIF
 4110 CONTINUE
      INDX = 1
      CHMSG = 'Missing Boundary Conditions Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
!
!---  Search input file for source card --
!
 4200 CONTINUE
 4209 READ(IRD,'(A)', END= 4210) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 4209
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'source').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Source Card'
        CALL WRMSGP( INDX )
        CALL RD_SR
        REWIND(IRD)
        GOTO 4300
      ELSE
        GOTO 4200
      ENDIF
 4210 CONTINUE
      INDX = 1
      CHMSG = 'Missing Source Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
!
!---  Search input file for output control card --
!
 4300 CONTINUE
 4309 READ(IRD,'(A)', END=4310) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 4309
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'output').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Output Control Card'
        CALL WRMSGP( INDX )
        CALL RD_OU
        REWIND(IRD)
        GOTO 4400
      ELSE
        GOTO 4300
      ENDIF
 4310 CONTINUE
      INDX = 1
      CHMSG = 'Missing Output Control Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
!
!---  Search input file for surface flux card --
!
 4400 CONTINUE
 4409 READ(IRD,'(A)', END=4410) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 4409
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'surface').NE.0 .AND.
     &    INDEX(CHDUM(2:),'cover').EQ.0 ) THEN
        CHMSG = 'Reading Surface Flux Card'
        CALL WRMSGP( INDX )
        CALL RD_SF
        REWIND(IRD)
        GOTO 4500
      ELSE
        GOTO 4400
      ENDIF
 4410 CONTINUE
      INDX = 1
      CHMSG = 'Missing Surface Flux Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
!
!---  Search input file for observed data card --
!
 4500 CONTINUE
      IF( ISLC(20).EQ.1 ) THEN
 4509   READ(IRD,'(A)', END=4510) CHDUM
        IF( CHDUM(1:1).EQ.'#' ) GOTO 4509
        CALL L_CASE( CHDUM )
        IF( CHDUM(1:1).EQ.'~' .AND.
     &      INDEX(CHDUM(2:),'observed').NE.0 ) THEN
          INDX = 1
          CHMSG = 'Reading Observed Data Card'
          CALL WRMSGP( INDX )
          CALL RD_OBDA
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
          CALL WRMSGP( INDX )
        ENDIF
!
!---  Search input file for UCode control Card --
!
 4600   CONTINUE
 4609   READ(IRD,'(A)', END=4610) CHDUM
        IF( CHDUM(1:1).EQ.'#' ) GOTO 4609
        CALL L_CASE( CHDUM )
        IF( CHDUM(1:1).EQ.'~' .AND.
     &      INDEX(CHDUM(2:),'ucode').NE.0 ) THEN
          INDX = 1
          CHMSG = 'Reading UCode Control Card'
          CALL WRMSGP( INDX )
          REWIND(IRD)
          GOTO 4620
        ELSE
          GOTO 4600
        ENDIF
 4610   CONTINUE
        IF( ISLC(20).EQ.0 ) THEN
          REWIND(IRD)
        ELSE
          INDX = 18
          CHMSG = 'Missing UCode Control Card'
          CALL WRMSGP( INDX )
        ENDIF
 4620   CONTINUE
      ENDIF
!
!---  Balance Card Parameters  ---
!
 4700 CONTINUE
 4710 READ(IRD,'(A)', END=4720) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 4710
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'balance').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Balance Card'
        CALL WRMSGP( INDX )
        CALL RD_BALA
        REWIND(IRD)
        GOTO 4720
      ELSE
        GOTO 4700
      ENDIF
 4720 CONTINUE
      REWIND(IRD)
!
!---  SAC Remediate Card Parameters  ---
!
 4800 CONTINUE
 4810 READ(IRD,'(A)', END=4820) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 4810
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'sac remediation').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading SAC Remediation Card'
        CALL WRMSGP( INDX )
        CALL RD_SREM
        REWIND(IRD)
        GOTO 4820
      ELSE
        GOTO 4800
      ENDIF
 4820 CONTINUE
      REWIND(IRD)
!
!---  SAC Release Card Parameters  ---
!
 4900 CONTINUE
 4910 READ(IRD,'(A)', END=4920) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 4910
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'sac release').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading SAC Release Card'
        CALL WRMSGP( INDX )
        CALL RD_SREL
        REWIND(IRD)
        GOTO 4920
      ELSE
        GOTO 4900
      ENDIF
 4920 CONTINUE
      REWIND(IRD)
!
!---  Search input file for reactive species link card --
!
 5200 CONTINUE
 5210 READ(IRD,'(A)', END= 5220) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 5210
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &  INDEX(CHDUM(2:),'link').NE.0 .AND.
     &  INDEX(CHDUM(2:),'specie').NE.0) THEN
        INDX = 1
        CHMSG = 'Reading Reactive Species Link Card'
        CALL WRMSGP( INDX )
        CALL RD_SPLK
        REWIND(IRD)
        GOTO 5230
      ELSE
        GOTO 5200
      ENDIF
 5220 CONTINUE
      INDX = 1
      CHMSG = 'Missing Reactive Species Link Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
 5230 CONTINUE
!
!---  Search input file for aqueous species card --
!
 5300 CONTINUE
 5310 READ(IRD,'(A)', END= 5320) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 5310
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'aqueous').NE.0 .AND.
     &    INDEX(CHDUM(2:),'species').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Aqueous Species Card'
        CALL WRMSGP( INDX )
        CALL RD_AQSP
        REWIND(IRD)
        GOTO 5330
      ELSE
        GOTO 5300
      ENDIF
 5320 CONTINUE
      INDX = 1
      CHMSG = 'Missing Aqueous Species Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
 5330 CONTINUE
!
!---  Search input file for solid species card --
!
 5400 CONTINUE
 5410 READ(IRD,'(A)', END= 5420) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 5410
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'solid').NE.0 .AND.
     &    INDEX(CHDUM(2:),'species').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Solid Species Card'
        CALL WRMSGP( INDX )
        CALL RD_SDSP
        REWIND(IRD)
        GOTO 5430
      ELSE
        GOTO 5400
      ENDIF
 5420 CONTINUE
      INDX = 1
      CHMSG = 'Missing Solid Species Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
 5430 CONTINUE
!
!---  Search input file for exchanged species card --
!
 5432 CONTINUE
 5434 READ(IRD,'(A)', END= 5436) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 5434
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'exchange').NE.0 .AND.
     &    INDEX(CHDUM(2:),'species').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Exchanged Species Card'
        CALL WRMSGP( INDX )
        CALL RD_EXSP
        REWIND(IRD)
        GOTO 5438
      ELSE
        GOTO 5432
      ENDIF
 5436 CONTINUE
      INDX = 1
      CHMSG = 'Missing Exchanged Species Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
 5438 CONTINUE
!
!---  Search input file for gas species card --
!
 5450 CONTINUE
 5460 READ(IRD,'(A)', END= 5470) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 5460
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'gas').NE.0 .AND.
     &    INDEX(CHDUM(2:),'species').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Gas Species Card'
        CALL WRMSGP( INDX )
        CALL RD_GSSP
        REWIND(IRD)
        GOTO 5480
      ELSE
        GOTO 5450
      ENDIF
 5470 CONTINUE
      INDX = 1
      CHMSG = 'Missing Gas Species Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
 5480 CONTINUE
!
!---  Search input file for equilibrium reactions card --
!
 5500 CONTINUE
 5510 READ(IRD,'(A)', END= 5520) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 5510
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'equil').NE.0 .AND.
     &    INDEX(CHDUM(2:),'react').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Equilibrium Reactions Card'
        CALL WRMSGP( INDX )
        CALL RD_EQRC
        REWIND(IRD)
        GOTO 5530
      ELSE
        GOTO 5500
      ENDIF
 5520 CONTINUE
      INDX = 1
      CHMSG = 'Missing Equilibrium Reactions Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
 5530 CONTINUE
!
!---  Search input file for kinetic reactions card --
!
 5600 CONTINUE
 5610 READ(IRD,'(A)', END= 5620) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 5610
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'kinetic').NE.0 .AND.
     &    INDEX(CHDUM(2:),'react').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Kinetic Reactions Card'
        CALL WRMSGP( INDX )
        CALL RD_KNRC
        REWIND(IRD)
        GOTO 5630
      ELSE
        GOTO 5600
      ENDIF
 5620 CONTINUE
      INDX = 1
      CHMSG = 'Missing Kinetic Reactions Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
 5630 CONTINUE
!
!---  Search input file for equilibrium equations card --
!
 5700 CONTINUE
 5710 READ(IRD,'(A)', END= 5720) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 5710
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'equil').NE.0 .AND.
     &    INDEX(CHDUM(2:),'equat').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Equilibrium Equations Card'
        CALL WRMSGP( INDX )
        CALL RD_EQEQ
        REWIND(IRD)
        GOTO 5730
      ELSE
        GOTO 5700
      ENDIF
 5720 CONTINUE
      INDX = 1
      CHMSG = 'Missing Equilibrium Equations Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
 5730 CONTINUE
!
!---  Search input file for kinetic equations card --
!
 5800 CONTINUE
 5810 READ(IRD,'(A)', END= 5820) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 5810
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'kinetic').NE.0 .AND.
     &    INDEX(CHDUM(2:),'equat').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Kinetic Equations Card'
        CALL WRMSGP( INDX )
        CALL RD_KNEQ
        REWIND(IRD)
        GOTO 5830
      ELSE
        GOTO 5800
      ENDIF
 5820 CONTINUE
      INDX = 1
      CHMSG = 'Missing Kinetic Equations Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
 5830 CONTINUE
!
!---  Search input file for conservation equations card --
!
 5900 CONTINUE
 5910 READ(IRD,'(A)', END= 5920) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 5910
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'conser').NE.0 .AND.
     &    INDEX(CHDUM(2:),'equat').NE.0 ) THEN
        INDX = 1
        CHMSG = 'Reading Conservation Equations Card'
        CALL WRMSGP( INDX )
        CALL RD_CNEQ
        REWIND(IRD)
        GOTO 5930
      ELSE
        GOTO 5900
      ENDIF
 5920 CONTINUE
      INDX = 1
      CHMSG = 'Missing Conservation Equations Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
 5930 CONTINUE
!
!---  Search input file for STOMP-WO well card --
!
 6000 CONTINUE
 6010 READ(IRD,'(A)', END= 6020) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 6010
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    INDEX(CHDUM(2:),'well').NE.0
     &    .AND. IOM.EQ.4 ) THEN
        INDX = 1
        CHMSG = 'Reading Well Card for STOMP-WO'
        CALL WRMSGP( INDX )
        CALL RD_WELL4
        REWIND(IRD)
        GOTO 6030
      ELSE
        GOTO 6000
      ENDIF
 6020 CONTINUE
      INDX = 1
      CHMSG = 'Missing STOMP-WO Well Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
 6030 CONTINUE
!
!---  Search input file for coupled well or ground-loop-well card --
!
 6040 CONTINUE
 6050 READ(IRD,'(A)', END= 6060) CHDUM
      IF( CHDUM(1:1).EQ.'#' ) GOTO 6050
      CALL L_CASE( CHDUM )
      IF( CHDUM(1:1).EQ.'~' .AND.
     &    (INDEX(CHDUM(2:),'coupled').NE.0 .OR.
     &    (INDEX(CHDUM(2:),'ground').NE.0 .AND.
     &    INDEX(CHDUM(2:),'loop').NE.0)) .AND.
     &    INDEX(CHDUM(2:),'well').NE.0 ) THEN
        INDX = 1
        IF( INDEX(CHDUM(2:),'coupled').NE.0 ) THEN
          CHMSG = 'Reading Coupled Well Card'
        ELSE
          CHMSG = 'Reading Ground-Loop Well Card'
        ENDIF
        CALL WRMSGP( INDX )
        CALL RD_COUP_WELL
        REWIND(IRD)
        GOTO 6070
      ELSE
        GOTO 6040
      ENDIF
 6060 CONTINUE
      INDX = 1
      CHMSG = 'Missing Coupled Well Card'
      CALL WRMSGP( INDX )
      REWIND(IRD)
 6070 CONTINUE
!
!---  End of cards  ---
!
      CALL CONN_LST
      CALL CONN_MAP
      IF( NSFCN.GT.0 ) CALL CONN_MAP_SFC
!
!---  For geomechanics simulations create a finite-element node map  --
!
      IF( LM.EQ.1 ) CALL CONN_FEN
!
!---  Jacobian matrix half-band width with coupled well  ---
!
      IF( L_CW.GT.0 ) THEN
!
!---    Dual-porosity modeld for STOMP-EOR  ---
!
        IF( ISLC(11).EQ.1 ) THEN
          CALL JCB_CW_DP
        ELSE
          CALL JCB_CW
        ENDIF
!
!---  Jacobian matrix half-band width with surface cover ---
!
      ELSEIF( NSFCN.GT.0 ) THEN
        CALL JCB_SURF_COV
!
!---  Jacobian matrix half-band width without coupled well  ---
!
      ELSE
!
!---    Dual-porosity modeld for STOMP-EOR  ---
!
        IF( ISLC(11).EQ.1 ) THEN
          CALL JCB_NCW_DP
        ELSE
          CALL JCB_NCW
        ENDIF
      ENDIF
!
!---  For geomechanics simulations compute Jacobian matrix half-band
!     width  --
!
      IF( LM.EQ.1 ) CALL JCB_GM
      LWN_CW = MAX( LWN_CW,1 )
      LWF_CW = MAX( LWF_CW,1 )
      LUK_CW = MAX( LUK_CW,1 )
!
!---  Deallocate memory for the scaling group index array  ---
!
      IF( ALLOCATED(ISCALE) ) THEN
        DEALLOCATE( ISCALE,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: ISCALE'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the rock/soil zonation index array  ---
!
      IF( ALLOCATED(IZ) ) THEN
        DEALLOCATE( IZ,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: IZ'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the Jacobian matrix pointer array  ---
!
      IF( ALLOCATED(IM) ) THEN
        DEALLOCATE( IM,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: IM'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the geomechanics Jacobian matrix pointer 
!     array  ---
!
      IF( ALLOCATED(IM_GM) ) THEN
        DEALLOCATE( IM_GM,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: IM_GM'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the matrix dual-porosity Jacobian matrix 
!     pointer array  ---
!
      IF( ALLOCATED(IM_M) ) THEN
        DEALLOCATE( IM_M,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: IM_M'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the inactive nodes array  ---
!
      IF( ALLOCATED(IXP) ) THEN
        DEALLOCATE( IXP,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: IXP'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the node connection map  ---
!
      IF( ALLOCATED(ICM) ) THEN
        DEALLOCATE( ICM,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: ICM'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the i-index node array  ---
!
      IF( ALLOCATED(ID) ) THEN
        DEALLOCATE( ID,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: ID'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the j-index node array  ---
!
      IF( ALLOCATED(JD) ) THEN
        DEALLOCATE( JD,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: JD'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the k-index node array  ---
!
      IF( ALLOCATED(KD) ) THEN
        DEALLOCATE( KD,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: KD'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the node index array  ---
!
      IF( ALLOCATED(ND) ) THEN
        DEALLOCATE( ND,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: ND'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the x-dimension array  ---
!
      IF( ALLOCATED(XE) ) THEN
        DEALLOCATE( XE,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: XE'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the y-dimension array  ---
!
      IF( ALLOCATED(YE) ) THEN
        DEALLOCATE( YE,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: YE'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the z-dimension array  ---
!
      IF( ALLOCATED(ZE) ) THEN
        DEALLOCATE( ZE,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: ZE'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the coupled-well well-interval
!     index array  ---
!
      IF( ALLOCATED(ID_CW) ) THEN
        DEALLOCATE( ID_CW,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: ID_CW'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the coupled-well well-node index array  ---
!
      IF( ALLOCATED(IWN_CW) ) THEN
        DEALLOCATE( IWN_CW,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: IWN_CW'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the index array for field nodes that
!     contain coupled-well nodes ---
!
      IF( ALLOCATED(IWF_CW) ) THEN
        DEALLOCATE( IWF_CW,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: IWF_CW'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the coupled-well Jacobian matrix array  ---
!
      IF( ALLOCATED(JM_CW) ) THEN
        DEALLOCATE( JM_CW,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: JM_CW'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the coupled-well x-transition 
!     point array  ---
!
      IF( ALLOCATED(XTP_CW) ) THEN
        DEALLOCATE( XTP_CW,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: XTP_CW'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the coupled-well y-transition 
!     point array  ---
!
      IF( ALLOCATED(YTP_CW) ) THEN
        DEALLOCATE( YTP_CW,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: YTP_CW'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the coupled-well z-transition 
!     point array  ---
!
      IF( ALLOCATED(ZTP_CW) ) THEN
        DEALLOCATE( ZTP_CW,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: ZTP_CW'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the coupled-well x-projection array  ---
!
      IF( ALLOCATED(XP_CW) ) THEN
        DEALLOCATE( XP_CW,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: XP_CW'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the coupled-well y-projection array  ---
!
      IF( ALLOCATED(YP_CW) ) THEN
        DEALLOCATE( YP_CW,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: YP_CW'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the coupled-well z-projection array  ---
!
      IF( ALLOCATED(ZP_CW) ) THEN
        DEALLOCATE( ZP_CW,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: ZP_CW'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the internal boundary surface array  ---
!
      IF( ALLOCATED(INBS) ) THEN
        DEALLOCATE( INBS,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: INBS'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the x vertices array  ---
!
      IF( ALLOCATED(XE) ) THEN
        DEALLOCATE( XE,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: XE'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the y vertices array  ---
!
      IF( ALLOCATED(YE) ) THEN
        DEALLOCATE( YE,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: YE'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the z vertices array  ---
!
      IF( ALLOCATED(ZE) ) THEN
        DEALLOCATE( ZE,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: ZE'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the grid refinement pointer array  ---
!
      IF( ALLOCATED(IBR) ) THEN
        DEALLOCATE( IBR,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: IBR'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the plant parameters array  ---
!
      IF( ALLOCATED(PARMS_P) ) THEN
        DEALLOCATE( PARMS_P,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: PARMS_P'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for surface cover connection map array  ---
!
      IF( ALLOCATED(ICM_SFC) ) THEN
        DEALLOCATE( ICM_SFC,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: ICM_SFC'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the finite-element node map  ---
!
      IF( ALLOCATED(ND_GM) ) THEN
        DEALLOCATE( ND_GM,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: ND_GM'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the inverse finite-element node map  ---
!
      IF( ALLOCATED(NE_GM) ) THEN
        DEALLOCATE( NE_GM,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: NE_GM'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Write "parameters" file  ---
!

      CALL CGLBP



!
!---  Close input and allocation file  ---
!
      CLOSE( UNIT=IRD )



!
!---  End of STEP program  ---
!
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CGLBP
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
!     Computed global parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNNL, 22 May 2003.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/CGLBP'
!
!---  Computed Parameters  ---
!
      LANW = LAN+(LWELL*LFZ*LNW)+(LSPILL*LFXY)
      LPH = LL+LG+LN+LHYD
      LMPH = LL+LG+LN
      LCMP = LD+LL+LN+LS
!
!---  Banded solver  ---
!
      IF( LBD.EQ.1 ) THEN
!
!---    Half band with not computed via Jacobian routine  ---
!
        IF( LHBW.EQ.1 ) THEN
          LHBW = LUK*LMNP + LUK - 1
        ENDIF
!
!---  Non-banded solver  ---
!
      ELSE
        LHBW = 1
      ENDIF      
!     
!---  Linear system solver variable parameters  ---
!
      LSTCX = MAX( LSTC,7 )
      LJA = MAX( LANW*LUK + L_CW*LN_CW + L_SFC*5*LSFCN + L_DP*LANW*LUK,
     &  LM*LFEN*3 )
      LJA = (LSP+LIS)*LJA + LBD + LPT
      LJB = MAX( LSTC*LANW*LUK*LUK + L_DP*LANW*LUK*LUK*3
     &   + L_CW*(LN_CW + 2*LWF_CW*LUK),LM*LFEN*3 )
      LJB = (LSP+LIS)*LJB
     &   + L_SFC*(LUK_SFC*LSFCN) + LBD + LPT
      LJO = LJB
      LJO_GM = (LSP+LIS)*LJO_GM + LBD + LPT
      LJB = MAX( LJB,LJO_GM)
      LJC = (LSP+LIS+LPT)*(LANW*LUK + L_CW*LN_CW + 
     &  L_SFC*5*LSFCN + L_DP*LANW*LUK + 1) + LBD
      LJC_GM = (LSP+LIS+LPT)*((LFEN*3) + 1) + LBD
      LJD = LBD*(3*LHBW+1) + LSP + LPT + LIS
      LJE = LBD*(LANW*LUK + L_CW*LN_CW + L_SFC*5*LSFCN 
     &  + L_DP*LANW*LUK) + LSP + LPT + LIS
      LJE = MAX( LBD*LFEN*3,LJE )
      LJF = LANW*LUK + L_CW*LN_CW + L_SFC*5*LSFCN
     &  + L_DP*LANW*LUK
      LJF = MAX( LFEN*3,LJF )
      LJG = (LSP+LIS)*(LANW*LUK + L_CW*LN_CW + 
     &  L_DP*LANW*LUK) + LBD + LPT
      LJG_GM = (LSP+LIS)*(LFEN) + LBD
      LJH = (LSP+LIS)*(LSTCX*LUK + 3*LUK*LWELL + L_DP*LUK)
     &  + LBD + LPT
      LJH_GM = (LSP+LIS)*(27)
      LJI = LBD*(LANW*LUK + L_CW*LN_CW + L_SFC*5*LSFCN 
     &  + L_DP*LANW*LUK) + LSP + LPT + LIS*LJF
      LJI = MAX( LBD*LFEN*3,LJI) 
      LJJ = LBD*(LANW*LUK + L_CW*LN_CW + L_SFC*5*LSFCN
     &  + L_DP*LANW*LUK) + LSP + LPT + LIS
      LJJ = MAX( LBD*LFEN*3,LJJ)
      LJK = (LSP+LIS)*LANW + LBD + LPT
      LJL = LSTCX
      LJM = (LSP+LIS)*LSTC*LANW + LBD + LPT
      LJN = (LSP+LIS+LPT)*(LANW+1) + LBD
      LSU = 2
      LSV = (LUK+2)
      LSFV = (2*LUK+1)
!
!---  Field Variable Parameters  ---
!
      IF( LT.EQ.0 ) THEN
        LFDT = 1
        LBRT = 0
      ELSE
        LFDT = LFD
        LBRT = LBR
      ENDIF
      IF( LL.EQ.0 ) THEN
        LFDL = 1
        LBRL = 0
      ELSE
        LFDL = LFD
        LBRL = LBR
      ENDIF
      IF( LG.EQ.0 ) THEN
        LFDG = 1
        LBRG = 0
      ELSE
        LFDG = LFD
        LBRG = LBR
      ENDIF
      IF( LN.EQ.0 ) THEN
        LFDN = 1
        LBRN = 0
      ELSE
        LFDN = LFD
        LBRN = LBR
      ENDIF
      IF( LN2.EQ.0 ) THEN
        LFDN2 = 1
        LBRN2 = 0
      ELSE
        LFDN2 = LFD
        LBRN2 = LBR
      ENDIF
      LFDNH = LFD**((LN+LHYD)-(LN*LHYD))
      LFDC = LFD**LC
      LFDM = LFD**LM
      LFDR = LFD**LR
      LBRR = LBR**LR
      LFDCR = LFD**((LC+LR)-(LC*LR))
      LFDRL = LFD**(LR*LL)
      LFDRG = LFD**(LR*LG)
      LFDRN = LFD**(LR*LN)
      LFDI = LFD**((LFW+LHYD)-(LFW*LHYD))
      IF( LS.EQ.1 .OR. LALC.EQ.1 ) THEN
        LFDS = LFD
        LBRS = LBR
      ELSE
        LFDS = 1
        LBRS = 0
      ENDIF
      LFDD = LFD**((LD+LDCO2)-(LD*LDCO2))
      LFDA = LFD**LN
      LFDH = LFD**LHYD
      LFDGC = LFD**LGC
!
!---  Surface Variable Parameters  ---
!
      LSX = LSX + (LFX+1)*LFYZ + LSRX
      LSY = LSY + LFX*(LFY+1)*LFZ + LSRY
      LSZ = LSZ + LFXY*(LFZ+1) + LSRZ
      LSXT = LSX**LT
      LSXL = LSX**LL
      LSXG = LSX**LG
      LSXN = LSX**((LN+LHYD)-(LN*LHYD))
      LSXN2 = LSX**LN2
      LSXC = LSX**((LC+LR)-(LC*LR))
      LSXS = LSX**((LS+LALC)-(LS*LALC))
      LSXGC = LSX**(LGC*LG)
      LSXLC = LSX**(LGC*LL)
      LSXNC = LSX**(LGC*LN)
      LSYT = LSY**LT
      LSYL = LSY**LL
      LSYG = LSY**LG
      LSYN = LSY**((LN+LHYD)-(LN*LHYD))
      LSYN2 = LSY**LN2
      LSYC = LSY**((LC+LR)-(LC*LR))
      LSYS = LSY**((LS+LALC)-(LS*LALC))
      LSYLC = LSY**(LGC*LG)
      LSYGC = LSY**(LGC*LL)
      LSYNC = LSY**(LGC*LN)
      LSZT = LSZ**LT
      LSZL = LSZ**LL
      LSZG = LSZ**LG
      LSZN = LSZ**((LN+LHYD)-(LN*LHYD))
      LSZN2 = LSZ**LN2
      LSZC = LSZ**((LC+LR)-(LC*LR))
      LSZS = LSZ**((LS+LALC)-(LS*LALC))
      LSZGC = LSZ**(LGC*LG)
      LSZLC = LSZ**(LGC*LL)
      LSZNC = LSZ**(LGC*LN)
!
!---  Rock/Soil Variable Parameters  ---
!
      LRCT = LRC**LT
      LRCL = LRC**LL
      LRCG = LRC**LG
      LRCN = LRC**LN
      LRCS = LRC**((LS+LALC)-(LS*LALC))
!
!---  Boundary Condition Parameters  ---
!
      LBCT = LBC**LT
      LBCL = LBC**LL
      LBCG = LBC**LG
      LBCN = LBC**LN
      LBCN2 = LBC**LN2
      LBCC = LBC**((LC+LR)-(LC*LR))
      LBCI = LBC**((LFW+LHYD)-(LFW*LHYD))
      LBCS = LBC**((LS+LALC)-(LS*LALC))
      LBCA = LBC**LN
      IF( IOM.EQ.32 ) THEN
        LBCU = 9
        LBCV = LBCU + (LSOLU*LC) + (LSPBC*LR)
      ELSEIF( IOM.EQ.36 .OR. IOM.EQ.37 .OR. IOM.EQ.38 ) THEN
        LBCU = 13
        LBCV = LBCU + (LSOLU*LC) + (LSPBC*LR)
      ELSEIF( IOM.EQ.43 ) THEN
        LBCU = 10 + 2*LNGC
        LBCV = LBCU + (LSOLU*LC) + (LSPBC*LR)
      ELSEIF( IOM.EQ.50 ) THEN
        LBCU = 11
        LBCV = LBCU+((LPH**LPC)*(LSOLU*LC))+(LSPBC*LR)
      ELSEIF( IOM.EQ.51 ) THEN
        LBCU = 13
        LBCV = LBCU+((LPH**LPC)*(LSOLU*LC))+(LSPBC*LR)
      ELSEIF( IOM.EQ.52 ) THEN
        LBCU = 11
        LBCV = LBCU+((LPH**LPC)*(LSOLU*LC))+(LSPBC*LR)
      ELSE
        LBCU = LUK+(LPH*LCN)+LT+((LPLANT-1)*LSW*2)+3+(LNGC-1)
        LBCV = LBCU+((LPH**LPC)*(LSOLU*LC))+(LSPBC*LR)
      ENDIF
      IF( LXYZG.EQ.1 ) LBCV = LBCV + 3
      LBCH = LBC**LHYD
      LBCGC = LBC**LGC
!
!---  Exchange species parameters  ---
!
      LSPE = MAX( LSPE,1 )
      LESITE = MAX( LESITE,1 )
!
!---  ECKEChem parameters  ---
!
      LSPR = MAX( LSPG+LSPL+LSPN+LSPS+LSPE,1 )
      LSPG = MAX( LSPG,1 )
      LSPL = MAX( LSPL,1 )
      LSPN = MAX( LSPN,1 )
      LSPS = MAX( LSPS,1 )
      LSPT = MAX( LSPT,1 )
!
!---  Output Variable Parameter
!     Number of Source Code Files Parameter  ---
!
      LOUPV = 400+33*((LSOLU**LC)+((LSPR+LSPT)**LR)+2-LC-LR)
      LFILES = 400
!
!---  Soil Moisture-Retention Characteristic Parameters  ---
!     Aqueous Relative Permeability Parameters
!     Gas Relative Permeability Parameters
!     NAPL Relative Permeability Parameters
!     Relative Permeability Tensor Parameters  ---
!
      LSCHR = 18
      LRPLC = 12
      LRPGC = 6
      LRPNC = 6
      LRPL = 4
!
!---  Coupled Multifluid Well Model Parameters  ---
!
      LNWN = LNW*LFZ
      LSZW = LNW*(LFZ+1)
      LNWV = 6
      LUKW = LUK+(LUK*LWELL)
!
!---  Noncondensible Gas Property Table Parameters  ---
!
      LP_TA = 72**LPTA
      LT_TA = 70**LPTA
      LT_TH = 100**LPTA
      LO_TH = 11**LPTA
      LT_PH = 155**LPTA
      LO_PH = 11**LPTA
      L_LV = 115**LPTA
      LINH = 15
!
!---  Ternary hydrate parameters  ---
!
      LHF_HT = 3**(LHYD*LN2)
      LCN_HT = 11**(LHYD*LN2)
      LCP_HT = 45**(LHYD*LN2)
      LCH_HT = 66**(LHYD*LN2)
      LPE_HT = 52**(LHYD*LN2)
      LHE_HT = 115**(LHYD*LN2)
      LTP_HT = 27**(LHYD*LN2)
      LPP_HT = 54**(LHYD*LN2)
!
!---  Dual Porosity and Equivalent Continuum Model parameters  ---
!
      LFD_DP = LFD**L_DP
      LFD_EC = LFD**(L_EC+L_DP)
      LBC_EC = LBC**L_EC
!
!---  End of CGLBP group
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
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
!     Check to see if a character-string input exists.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNNL, November 2000.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*(*) CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/CHK_CHR'
      ICD = INDEX( CARD,'  ')-1
      IVR = INDEX( VARB,'  ')-1
      ISX = ISTART
      ICX = ICOMMA
      INDX = 0
!
!---  End of card record error ---
!
      IF( CHDUM(1:1) .EQ. '~' ) THEN
        INDX = 4
        CHMSG = 'End of Card Record: ' // VARB(1:IVR)
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Find next comma  ---
!
      ICOMMA = INDEX (CHDUM(ISTART:), ',') + ISTART - 1
      ISTOP = ICOMMA
  100 CONTINUE
!
!---  Comma not found, missing real data  ---
!
      IF( ISTOP.LT.ISTART ) THEN
        GOTO 200
!
!---  Null entry  ---
!
      ELSEIF( ISTOP.EQ.ISTART ) THEN
        INDX = 1
        GOTO 200
!
!---  Characters between commas  ---
!
      ELSE
!
!---  Eliminate leading blank spaces  ---
!
        IF( ICHAR(CHDUM(ISTART:ISTART)).EQ.32 ) THEN
          ISTART = ISTART+1
          GOTO 100
        ENDIF
!
!---  Eliminate trailing blank spaces  ---
!
        ISTOP = ISTOP-1
  110   CONTINUE
        IF( ICHAR(CHDUM(ISTOP:ISTOP)).EQ.32 ) THEN
          ISTOP = ISTOP-1
          GOTO 110
        ENDIF
!
!---  Character string recognized as a character string  ---
!
        INDX = 1
      ENDIF
  200 CONTINUE
      ISTART = ISX
      ICOMMA = ICX
!
!---  End of CHK_CHR group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CHK_CW
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
!
!     STOMP-CO2
!
!     Define well nodes, determine trajectory points, and 
!     check for well trajectories within node surface planes
!
!----------------------Authors-----------------------------------------!
!
!     Written by M.D. White, PNNL, 31 March 2011.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GRID
      USE COUP_WELL
      USE CONST
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 XPX(5),YPX(5),ZPX(5)
      REAL*8 XIX(2),YIX(2),ZIX(2)
      REAL*8 PAX(3),PBX(3),PCX(3),PBCX(3)
      REAL*8 AJ(3,3),BJ(3)
      INTEGER IJ(3)
      INTEGER MSX(4,6)
      INTEGER N1X(4),N2X(4)
      TYPE(LIST_WELL_NODE), POINTER :: WN_CURR_PTR,WN_TMP_PTR
!
!----------------------Data Statements---------------------------------!
!
      SAVE MSX,N1X,N2X
      DATA MSX / 1,2,4,3,1,5,6,2,1,3,7,5,2,6,8,4,3,4,8,7,5,7,8,6 /
      DATA N1X / 2,3,4,1 /
      DATA N2X / 1,2,3,4 /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/CHK_CW'
      NULLIFY( WN_CW_PTR )
      EPSLX = 1.D-12
!
!---  Loop over coupled wells ---
!
      DO 600 NCW = 1,LN_CW
        ID_CW(3,NCW) = LWN_CW + 1
        ID_CW(5,NCW) = LWF_CW + 1
        NWN_CW = LWN_CW
        NWF_CW = LWF_CW
!
!---    Loop over number of well intervals  ---
!
        DO 490 NICW = ID_CW(1,NCW),ID_CW(2,NCW)
          IFNDX = 0
!
!---      Loop over active nodes to find well nodes and well
!         projections ---
!
          DO 480 N = 1,LFD-LBR
            IF( IXP(N).EQ.0 ) GOTO 480
            I = ID(N)
            J = JD(N)
            K = KD(N)
            NC = 0
            I = ID(N)
            J = JD(N)
            K = KD(N)
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 470 KX = 1,KRX
            DO 460 JX = 1,JRX
            DO 450 IX = 1,IRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NC = 0
!
!---          Determine whether the transition points are within
!             the hexahedron node volume or on the hexahedron
!             surfaces  ---
!
              DO 190 NPT = 1,2
!
!---            Cylindrical coordinates with azimuthal symmetry,
!               centrally located wells  ---
!
                IF( (ICS.EQ.2 .OR. ICS.EQ.6) .AND. LFY.EQ.1
     &            .AND. I.EQ.1 ) THEN
                  XPX(1) = 0.D+0
                  XPX(2) = 0.D+0
                  YPX(1) = 0.D+0
                  YPX(2) = 0.D+0
                  ZPX(1) = ZE(1,NX)
                  ZPX(2) = ZE(5,NX)
!
!---              Node height greater than EPSLX  ---
!
                  IF( ABS(ZPX(1)-ZPX(2)).GT.EPSLX ) THEN
                    DZPX1 = ZTP_CW(NPT,NICW)-ZPX(1)
                    DZPX2 = ZPX(2)-ZTP_CW(NPT,NICW)
                    IF( ABS(DZPX1).LT.EPSLX ) DZPX1 = 0.D+0
                    IF( ABS(DZPX2).LT.EPSLX ) DZPX2 = 0.D+0
!
!---                Transition point within vertical limits of node  ---
!
                    IF( DZPX1.GE.0.D+0 .AND. DZPX2.GE.0.D+0 ) THEN
                      NC = NC+1
                      XIX(NC) = 0.D+0
                      YIX(NC) = 0.D+0
                      ZIX(NC) = ZTP_CW(NPT,NICW)
                    ENDIF
                  ENDIF
                  GOTO 190
                ENDIF
!
!---            Loop over node surfaces,
!               (bottom,south,west,east,north,top)  ---
!
                ICWX = 0
                DO 180 NS = 1,6
!
!---              Define the five surface points, four corners
!                 and one centroid---
!
                  DO 110 NP = 1,4
                    MX = MSX(NP,NS)
!
!---                Cylindrical coordinates  ---
!
                    IF( ICS.EQ.2 .OR. ICS.EQ.6 ) THEN
                      XPX(NP) = XE(MX,NX)*COS(YE(MX,NX))
                      YPX(NP) = XE(MX,NX)*SIN(YE(MX,NX))
                      ZPX(NP) = ZE(MX,NX)
                    ELSE
                      XPX(NP) = XE(MX,NX)
                      YPX(NP) = YE(MX,NX)
                      ZPX(NP) = ZE(MX,NX)
                    ENDIF
  110             CONTINUE
                  NP = 4
                  CALL PG_CNTRD( NP,XPX(1),YPX(1),ZPX(1),
     &              XPX(5),YPX(5),ZPX(5) )
!
!---              Loop over the four triangular planes on the 
!                 surface face  ---
!
                  DO 130 NT = 1,4
!
!---                Built vectors between transition point
!                   and triangular plane points  ---
!
                    PAX(1) = XPX(5)-XTP_CW(NPT,NICW)
                    PAX(2) = YPX(5)-YTP_CW(NPT,NICW)
                    PAX(3) = ZPX(5)-ZTP_CW(NPT,NICW)
                    PBX(1) = XPX(N1X(NT))-XTP_CW(NPT,NICW)
                    PBX(2) = YPX(N1X(NT))-YTP_CW(NPT,NICW)
                    PBX(3) = ZPX(N1X(NT))-ZTP_CW(NPT,NICW)
                    PCX(1) = XPX(N2X(NT))-XTP_CW(NPT,NICW)
                    PCX(2) = YPX(N2X(NT))-YTP_CW(NPT,NICW)
                    PCX(3) = ZPX(N2X(NT))-ZTP_CW(NPT,NICW)
                    CALL V_CROSSP( PBX,PCX,PBCX )
                    SX = V_DOTP( PAX,PBCX )
!
!---                Clockwise rotation  ---
!
                    IF( SX.GT.EPSL ) THEN
!
!---                  Opposing rotations found, point outside 
!                     hexaheron  ---
!
                      IF( ICWX.EQ.-1 ) THEN
                        GOTO 190
!
!---                  Similar rotations found, continue searching  ---
!
                      ELSE
                        ICWX = 1
                      ENDIF
!
!---                Counterclockwise rotation  ---
!
                    ELSEIF( SX.LT.-EPSL ) THEN
!
!---                  Opposing rotations found, point outside 
!                     hexaheron  ---
!
                      IF( ICWX.EQ.1 ) THEN
                        GOTO 190
!
!---                  Similar rotations found, continue searching  ---
!
                      ELSE
                        ICWX = -1
                      ENDIF
                    ENDIF
  130             CONTINUE
  180           CONTINUE            
!
!---            No opposing rotations found, point inside 
!               hexahedron  ---
!
                NC = NC+1
                XIX(NC) = XTP_CW(NPT,NICW)
                YIX(NC) = YTP_CW(NPT,NICW)
                ZIX(NC) = ZTP_CW(NPT,NICW)
  190         CONTINUE
!
!---          Both transition points inside hexahedron, skip
!             search for well path crossing hexahedron surfaces  ---
!
              IF( NC.EQ.2 ) GOTO 232
!
!---          Cylindrical coordinates with azimuthal symmetry,
!             centrally located wells  ---
!
              IF( (ICS.EQ.2 .OR. ICS.EQ.6) .AND. LFY.EQ.1
     &          .AND. I.EQ.1 ) THEN
!
!---            Interval crosses lower node surface  ---
!
                DZPX1 = ZPX(1)-ZTP_CW(1,NICW)
                IF( ABS(DZPX1).LT.EPSLX ) DZPX1 = 0.D+0
                DZPX2 = ZPX(1)-ZTP_CW(2,NICW)
                IF( ABS(DZPX2).LT.EPSLX ) DZPX2 = 0.D+0
                IF( (DZPX1*DZPX2).LT.-EPSLX ) THEN
                  NC = NC+1
                  XIX(NC) = 0.D+0
                  YIX(NC) = 0.D+0
                  ZIX(NC) = ZPX(1)
                ENDIF
!
!---            Interval crosses upper node surface  ---
!
                DZPX1 = ZPX(2)-ZTP_CW(1,NICW)
                IF( ABS(DZPX1).LT.EPSLX ) DZPX1 = 0.D+0
                DZPX2 = ZPX(2)-ZTP_CW(2,NICW)
                IF( ABS(DZPX2).LT.EPSLX ) DZPX2 = 0.D+0
                IF( (DZPX1*DZPX2).LT.-EPSLX ) THEN
                  NC = NC+1
                  XIX(NC) = 0.D+0
                  YIX(NC) = 0.D+0
                  ZIX(NC) = ZPX(2)
                ENDIF
                GOTO 232
              ENDIF
!
!---          Loop over node surfaces,
!             (bottom,south,west,east,north,top)  ---
!
              DO 230 NS = 1,6
!
!---            Define the five surface points, four corners
!               and one centroid---
!
                DO 200 NP = 1,4
                  MX = MSX(NP,NS)
!
!---              Cylindrical coordinates---
!
                  IF( ICS.EQ.2 .OR. ICS.EQ.6 ) THEN
                    XPX(NP) = XE(MX,NX)*COS(YE(MX,NX))
                    YPX(NP) = XE(MX,NX)*SIN(YE(MX,NX))
                    ZPX(NP) = ZE(MX,NX)
                  ELSE
                    XPX(NP) = XE(MX,NX)
                    YPX(NP) = YE(MX,NX)
                    ZPX(NP) = ZE(MX,NX)
                  ENDIF
  200           CONTINUE
                NP = 4
                CALL PG_CNTRD( NP,XPX(1),YPX(1),ZPX(1),
     &            XPX(5),YPX(5),ZPX(5) )
!
!
!---            Loop over the four triangular planes on the 
!               surface face  ---
!
                DO 220 NT = 1,4
!
!---              Load plane-line intersection matrix and 
!                 problem vector  ---
!
                  AJ(1,1) = XTP_CW(1,NICW)-XTP_CW(2,NICW)
                  AJ(2,1) = YTP_CW(1,NICW)-YTP_CW(2,NICW)
                  AJ(3,1) = ZTP_CW(1,NICW)-ZTP_CW(2,NICW)
                  AJ(1,2) = XPX(N1X(NT))-XPX(5)
                  AJ(2,2) = YPX(N1X(NT))-YPX(5)
                  AJ(3,2) = ZPX(N1X(NT))-ZPX(5)
                  AJ(1,3) = XPX(N2X(NT))-XPX(5)
                  AJ(2,3) = YPX(N2X(NT))-YPX(5)
                  AJ(3,3) = ZPX(N2X(NT))-ZPX(5)
                  BJ(1) = XTP_CW(1,NICW)-XPX(5)
                  BJ(2) = YTP_CW(1,NICW)-YPX(5)
                  BJ(3) = ZTP_CW(1,NICW)-ZPX(5)
!
!---              Check for no intersection  ---
!
                  DO 210 IP = 1,3
                    AAMAX = 0.D+0
                    DO 202 JP = 1,3
                      IF( ABS(AJ(IP,JP)).GT.AAMAX ) 
     &                  AAMAX = ABS(AJ(IP,JP))
  202               CONTINUE
!
!---                No intersection go to next triangle on the 
!                   surface  ---
!
                    IF( ABS(AAMAX)/EPSL.LT.EPSL ) GOTO 220
  210             CONTINUE
!
!---              Find plane-line intersection matrix inverse  ---
!
                  JP = 3
                  KP = 3
                  CALL LU_DCMP( AJ,JP,KP,IJ,DJ )
                  CALL LU_BKSB( AJ,JP,KP,IJ,BJ )
!
!---              Find plane-line intersection point  ---
!
                  TX = BJ(1)
                  UX = BJ(2)
                  VX = BJ(3)
                  IF( ABS(TX).LT.EPSL ) TX = 0.D+0
                  IF( ABS(UX).LT.EPSL ) UX = 0.D+0
                  IF( ABS(VX).LT.EPSL ) VX = 0.D+0
!
!---              Line crosses surface, within the triangle  ---
!
                  IF( TX.GE.0.D+0 .AND. TX.LE.1.D+0 .AND.
     &              UX.GE.0.D+0 .AND. UX.LE.1.D+0 .AND. 
     &              VX.GE.0.D+0 .AND. VX.LE.1.D+0 .AND.
     &              (UX+VX).LE.1.D+0 ) THEN
                    XTX = XTP_CW(1,NICW)
     &                + (XTP_CW(2,NICW)-XTP_CW(1,NICW))*TX
                    YTX = YTP_CW(1,NICW)
     &                + (YTP_CW(2,NICW)-YTP_CW(1,NICW))*TX
                    ZTX = ZTP_CW(1,NICW)
     &                + (ZTP_CW(2,NICW)-ZTP_CW(1,NICW))*TX
!
!---                Check for non-distinct points  ---
!
                    IF( NC.GE.1 ) THEN
                      DO 212 NDP = 1,NC
                        DPX = SQRT( ((XTX-XIX(NDP))**2) 
     &                    + ((YTX-YIX(NDP))**2) + ((ZTX-ZIX(NDP))**2) )
!
!---                    Duplicate point found  ---
!
                        IF( DPX.LT.EPSLX ) GOTO 214
  212                 CONTINUE
                    ENDIF
                    IF( NC.EQ.2 ) THEN
                      INDX = 7
                      CHMSG = 'Three Distinct Coupled Well Points'
     &                  //  ' at Node'
                      IMSG = NX
                      CALL WRMSGP( INDX )
                    ENDIF
                    NC = NC + 1
                    XIX(NC) = XTX
                    YIX(NC) = YTX
                    ZIX(NC) = ZTX
                  ENDIF
  214             CONTINUE
  220           CONTINUE
  230         CONTINUE
  232         CONTINUE
!
!---          Check that two well points are distinct  ---
!
              DPX = 0.D+0
              IF( NC.EQ.2 ) THEN
                DPX = SQRT( ((XIX(2)-XIX(1))**2) + ((YIX(2)-YIX(1))**2)
     &            + ((ZIX(2)-ZIX(1))**2) )
              ENDIF
!
!---          Two distinct well points  ---
!
              IF( NC.EQ.2 .AND. DPX.GT.EPSLX ) THEN
!
!---            Cylindrical coordinates with azimuthal symmetry  ---
!
                IF( (ICS.EQ.2 .OR. ICS.EQ.6) .AND. LFY.EQ.1 ) GOTO 362
!
!---            Check if line between well points is contained in
!               a node surface plane, loop over node surfaces,
!               (bottom,south,west,east,north,top)  ---
!
                DO 360 NS = 1,6
!
!---              Define the five surface points, four corners
!                 and one centroid---
!
                  DO 352 NP = 1,4
                    MX = MSX(NP,NS)
!
!---                Cylindrical coordinates---
!
                    IF( ICS.EQ.2 .OR. ICS.EQ.6 ) THEN
                      XPX(NP) = XE(MX,NX)*COS(YE(MX,NX))
                      YPX(NP) = XE(MX,NX)*SIN(YE(MX,NX))
                      ZPX(NP) = ZE(MX,NX)
                    ELSE
                      XPX(NP) = XE(MX,NX)
                      YPX(NP) = YE(MX,NX)
                      ZPX(NP) = ZE(MX,NX)
                    ENDIF
  352             CONTINUE
                  NP = 4
                  CALL PG_CNTRD( NP,XPX(1),YPX(1),ZPX(1),
     &              XPX(5),YPX(5),ZPX(5) )
!
!---              Loop over the four triangular planes on the 
!                 surface face  ---
!
                  DO 358 NT = 1,4
!
!---                Loop over trajectory points  ---
!
                    DO 356 NPT = 1,2
!
!---                  Load point-plane matrix  ---
!
                      AJ(1,1) = XIX(NPT)-XPX(5)
                      AJ(2,1) = XIX(NPT)-XPX(N1X(NT))
                      AJ(3,1) = XIX(NPT)-XPX(N2X(NT))
                      AJ(1,2) = YIX(NPT)-YPX(5)
                      AJ(2,2) = YIX(NPT)-YPX(N1X(NT))
                      AJ(3,2) = YIX(NPT)-YPX(N2X(NT))
                      AJ(1,3) = ZIX(NPT)-ZPX(5)
                      AJ(2,3) = ZIX(NPT)-ZPX(N1X(NT))
                      AJ(3,3) = ZIX(NPT)-ZPX(N2X(NT))
!
!---                  Check for singular matrix  ---
!
                      DO 310 IP = 1,3
                        AAMAX = 0.D+0
                        DO 302 JP = 1,3
                          IF( ABS(AJ(IP,JP)).GT.AAMAX ) 
     &                    AAMAX = ABS(AJ(IP,JP))
  302                   CONTINUE
!
!---                    Singular matrix, trajectory point is within
!                       the surface plane  ---
!
                        IF( ABS(AAMAX)/EPSL.LT.EPSL ) GOTO 356
  310                 CONTINUE
!
!---                  Find matrix determinant  ---
!
                      JP = 3
                      KP = 3
                      CALL LU_DCMP( AJ,JP,KP,IJ,DJ )
                      DO 354 M = 1,JP
                        DJ = DJ*AJ(M,M)
  354                 CONTINUE
!
!---                  If determinant equals zero trajectory point
!                     is within the surface plane  ---
!
                      IF( ABS(DJ).GT.EPSL ) GOTO 358
  356               CONTINUE
                    INDX = 7
                    CHMSG = 'Both Well Trajectory Points within '
     &                //  'Surface Plane for Well Number'
                    IMSG = NCW
                    CALL WRMSGP( INDX )
  358             CONTINUE
  360           CONTINUE
  362           CONTINUE
!
!---            Loop over current well nodes, where the most recent
!               well node is first in the list order  ---
!
                WN_CURR_PTR => WN_CW_PTR
                DO 370 NWN = 1,LWN_CW-NWN_CW
!                DO 370 NWN = 1,LWN_CW
!
!---              Well node previously counted  ---
!
                  IF( WN_CURR_PTR%IWN_CW.EQ.NX ) THEN
                    GOTO 372
                  ENDIF
                  WN_CURR_PTR => WN_CURR_PTR%NEXT
  370           CONTINUE
!
!---            Increment the number of field nodes with
!               coupled-well nodes  ---
!
                LWF_CW = LWF_CW + 1
  372           CONTINUE
!
!---            Increment the number of well nodes  ---
!
                LWN_CW = LWN_CW + 1
                IFNDX = 1

!
!---            Allocate memory for new well node element  ---
!
                ALLOCATE( WN_TMP_PTR,STAT=ISTAT )
                IF( ISTAT.NE.0 ) THEN
                  INDX = 3
                  CHMSG = 'Allocation Error: WN_TMP_PTR'
                  CALL WRMSGP( INDX )
                ENDIF
                WN_TMP_PTR%IWN_CW = NX
                WN_TMP_PTR%XP1_CW = XIX(1)
                WN_TMP_PTR%YP1_CW = YIX(1)
                WN_TMP_PTR%ZP1_CW = ZIX(1)
                WN_TMP_PTR%XP2_CW = XIX(2)
                WN_TMP_PTR%YP2_CW = YIX(2)
                WN_TMP_PTR%ZP2_CW = ZIX(2)
                WN_TMP_PTR%NEXT => WN_CW_PTR
                WN_CW_PTR => WN_TMP_PTR
              ENDIF
  450       CONTINUE
  460       CONTINUE
  470       CONTINUE
  480     CONTINUE
          IF( IFNDX.EQ.0 ) THEN
            INDX = 7
            CHMSG = 'Well Interval not Found within Domain: ' //
     &        'Well Interval = '
            IMSG = NICW
            CALL WRMSGP( INDX )
          ENDIF
  490   CONTINUE
        ID_CW(4,NCW) = LWN_CW
        ID_CW(6,NCW) = LWF_CW
  600 CONTINUE
!
!---  Allocate memory for coupled-well well-node index array  ---
!
      ALLOCATE( IWN_CW(1:LWN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IWN_CW'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Allocate memory for field-node with coupled-well nodes
!     index array  ---
!
      ALLOCATE( IWF_CW(1:LWF_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IWF_CW'
        CALL WRMSGP( INDX )
      ENDIF
      DO 610 NWF = 1,LWF_CW
        IWF_CW(NWF) = 0
  610 CONTINUE
!
!---  Allocate memory for the coupled-well x-projection array  ---
!
      ALLOCATE( XP_CW(1:2,1:LWN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XP_CW'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Allocate memory for the coupled-well y-projection array  ---
!
      ALLOCATE( YP_CW(1:2,1:LWN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YP_CW'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Allocate memory for the coupled-well z-projection array  ---
!
      ALLOCATE( ZP_CW(1:2,1:LWN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZP_CW'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Load coupled-well well-node index array  ---
!
      WN_CURR_PTR => WN_CW_PTR
      DO 620 NWN = 1,LWN_CW
        M = LWN_CW-NWN+1
        IWN_CW(M) = WN_CURR_PTR%IWN_CW
        XP_CW(1,M) = WN_CURR_PTR%XP1_CW
        XP_CW(2,M) = WN_CURR_PTR%XP2_CW
        YP_CW(1,M) = WN_CURR_PTR%YP1_CW
        YP_CW(2,M) = WN_CURR_PTR%YP2_CW
        ZP_CW(1,M) = WN_CURR_PTR%ZP1_CW
        ZP_CW(2,M) = WN_CURR_PTR%ZP2_CW
        WN_CURR_PTR => WN_CURR_PTR%NEXT
  620 CONTINUE
!
!---  Loop over coupled wells ---
!
      NC = 0
      DO 700 NCW = 1,LN_CW
!
!---    Sequence well nodes according to their distance from
!       the starting point of the well  ---
!
        I1X = ID_CW(1,NCW)
        I3X = ID_CW(3,NCW)
        I4X = ID_CW(4,NCW)
  630   CONTINUE
        DMNX = 1.D+20
        DO 640 KCW = I3X,I4X
          XWPX = 5.D-1*(XP_CW(1,KCW)+XP_CW(2,KCW))
          YWPX = 5.D-1*(YP_CW(1,KCW)+YP_CW(2,KCW))
          ZWPX = 5.D-1*(ZP_CW(1,KCW)+ZP_CW(2,KCW))
          DISTX = SQRT( (XTP_CW(1,I1X)-XWPX)**2 +
     &      (YTP_CW(1,I1X)-YWPX)**2 + (ZTP_CW(1,I1X)-ZWPX)**2 )
          IF( DISTX.LT.DMNX ) THEN
            DMNX = DISTX
            IMNX = KCW
            IWNX = IWN_CW(KCW)
            XIX(1) = XP_CW(1,KCW)
            XIX(2) = XP_CW(2,KCW)
            YIX(1) = YP_CW(1,KCW)
            YIX(2) = YP_CW(2,KCW)
            ZIX(1) = ZP_CW(1,KCW)
            ZIX(2) = ZP_CW(2,KCW)
          ENDIF
  640   CONTINUE
        DO 650 JCW = I4X,I3X,-1
          IF( JCW.LT.IMNX ) THEN
            IWN_CW(JCW+1) = IWN_CW(JCW)
            XP_CW(1,JCW+1) = XP_CW(1,JCW)
            XP_CW(2,JCW+1) = XP_CW(2,JCW)
            YP_CW(1,JCW+1) = YP_CW(1,JCW)
            YP_CW(2,JCW+1) = YP_CW(2,JCW)
            ZP_CW(1,JCW+1) = ZP_CW(1,JCW)
            ZP_CW(2,JCW+1) = ZP_CW(2,JCW)
          ENDIF
  650   CONTINUE
        IWN_CW(I3X) = IWNX
        XP_CW(1,I3X) = XIX(1)
        XP_CW(2,I3X) = XIX(2)
        YP_CW(1,I3X) = YIX(1)
        YP_CW(2,I3X) = YIX(2)
        ZP_CW(1,I3X) = ZIX(1)
        ZP_CW(2,I3X) = ZIX(2)
        I3X = I3X+1
        IF( I3X.LT.I4X ) GOTO 630
!
!---    Sequence well node points according to their distance from
!       the starting point of the well  ---
!
        I1X = ID_CW(1,NCW)
        I3X = ID_CW(3,NCW)
        I4X = ID_CW(4,NCW)
        DO 652 KCW = I3X,I4X
          DIST1X = SQRT( (XTP_CW(1,I1X)-XP_CW(1,KCW))**2 +
     &      (YTP_CW(1,I1X)-YP_CW(1,KCW))**2 + 
     &      (ZTP_CW(1,I1X)-ZP_CW(1,KCW))**2 )
          DIST2X = SQRT( (XTP_CW(1,I1X)-XP_CW(2,KCW))**2 +
     &      (YTP_CW(1,I1X)-YP_CW(2,KCW))**2 + 
     &      (ZTP_CW(1,I1X)-ZP_CW(2,KCW))**2 )
          IF( DIST1X.GT.DIST2X ) THEN
            XIX(2) = XP_CW(1,KCW)
            YIX(2) = YP_CW(1,KCW)
            ZIX(2) = ZP_CW(1,KCW)
            XP_CW(1,KCW) = XP_CW(2,KCW)
            YP_CW(1,KCW) = YP_CW(2,KCW)
            ZP_CW(1,KCW) = ZP_CW(2,KCW)
            XP_CW(2,KCW) = XIX(2)
            YP_CW(2,KCW) = YIX(2)
            ZP_CW(2,KCW) = ZIX(2)
          ENDIF
  652   CONTINUE
!
!---    Load array for field nodes that contain coupled-well nodes  ---
!
        MC = 0
        DO 670 KCW = ID_CW(3,NCW),ID_CW(4,NCW)
          N = IWN_CW(KCW)
          DO 660 JCW = ID_CW(5,NCW),ID_CW(5,NCW)-1+MC
            IF( IWF_CW(JCW).EQ.N ) GOTO 662
  660     CONTINUE
          MC = MC + 1
          NC = NC + 1
          IWF_CW(NC) = N
  662     CONTINUE
  670   CONTINUE
  700 CONTINUE
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of CHK_CW group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
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
!     Fill double precision variable VAR with data between commas.
!     Return default value or zero for null entries.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, November 1992.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*(*) CHDUM
      CHARACTER*6 FORM1
      CHARACTER*7 FORM2
!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM1,FORM2
      DATA FORM1 /'(D .0)'/
      DATA FORM2 /'(D  .0)'/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/CHK_DPR'
      IDFLTD = 0
      ICD = INDEX( CARD,'  ')-1
      IVR = INDEX( VARB,'  ')-1
      ISX = ISTART
      ICX = ICOMMA
      INDX = 0
!
!---  End of card record error ---
!
      IF( CHDUM(1:1) .EQ. '~' ) THEN
        INDX = 4
        CHMSG = 'End of Card Record: ' // VARB(1:IVR)
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Find next comma  ---
!
      ICOMMA = INDEX (CHDUM(ISTART:), ',') + ISTART - 1
      ISTOP = ICOMMA
  100 CONTINUE
!
!---  Comma not found, missing real data  ---
!
      IF( ISTOP.LT.ISTART ) THEN
        GOTO 200
!
!---  Null entry  ---
!
      ELSEIF( ISTOP.EQ.ISTART ) THEN
        INDX = 1
        GOTO 200
!
!---  Characters between commas  ---
!
      ELSE
!
!---  Eliminate leading blank spaces  ---
!
        IF( ICHAR(CHDUM(ISTART:ISTART)).EQ.32 ) THEN
          ISTART = ISTART+1
          GOTO 100
        ENDIF
!
!---  Eliminate trailing blank spaces  ---
!
        ISTOP = ISTOP-1
  110   CONTINUE
        IF( ICHAR(CHDUM(ISTOP:ISTOP)).EQ.32 ) THEN
          ISTOP = ISTOP-1
          GOTO 110
        ENDIF
!
!---  Check for scientific notation  ---
!
        IEXP = ISTART-1
        IF( INDEX( CHDUM(ISTART:ISTOP),'e' ).NE.0 ) THEN
          IEXP = INDEX( CHDUM(ISTART:ISTOP),'e' )+ISTART-1
        ELSEIF( INDEX( CHDUM(ISTART:ISTOP),'d' ).NE.0 ) THEN
          IEXP = INDEX( CHDUM(ISTART:ISTOP),'d' )+ISTART-1
        ENDIF
        IPER = INDEX( CHDUM(ISTART:ISTOP),'.' )+ISTART-1
!
!---  Check for non-numerical characters  ---
!
        DO 120 N = ISTART,ISTOP
          IF( N.EQ.IEXP .OR. N.EQ.IPER ) GOTO 120
          NC = ICHAR(CHDUM(N:N))
          IF( ( N.EQ.ISTART .OR. N.EQ.IEXP+1 ) .AND.
     &      ( NC.EQ.43 .OR. NC.EQ.45 ) ) GOTO 120
          IF( NC.LT.48 .OR. NC.GT.57 ) GOTO 200
  120   CONTINUE
!
!---  Character string recognized as a double precision real  ---
!
        INDX = 1
      ENDIF
  200 CONTINUE
      ISTART = ISX
      ICOMMA = ICX
!
!---  End of CHK_DPR group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CHK_INT( ISTART,ICOMMA,CHDUM,INDX )
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
!     Fill integer variable IVAR with data between commas.
!     Return default value or zero for null entries.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNNL, November 2000.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*(*) CHDUM
      CHARACTER*4 FORM1
!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM1
      DATA FORM1 /'(I )'/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/CHK_INT'
      ICD = INDEX( CARD,'  ')-1
      IVR = INDEX( VARB,'  ')-1
      ISX = ISTART
      ICX = ICOMMA
      INDX = 0
!
!---  End of card record error ---
!
      IF( CHDUM(1:1) .EQ. '~' ) THEN
        INDX = 4
        CHMSG = 'End of Card Record: ' // VARB(1:IVR)
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Read numbers between commas  ---
!
      ICOMMA = INDEX (CHDUM(ISTART:), ',') + ISTART - 1
      ISTOP = ICOMMA
  100 CONTINUE
!
!---  Comma not found, missing integer data  ---
!
      IF( ISTOP.LT.ISTART ) THEN
        GOTO 200
!
!---  Null entry  ---
!
      ELSEIF( ISTOP.EQ.ISTART ) THEN
        INDX = 1
        GOTO 200
!
!---  Characters between commas  ---
!
      ELSE
!
!---  Eliminate leading blank spaces  ---
!
        IF( ICHAR(CHDUM(ISTART:ISTART)).EQ.32 ) THEN
          ISTART = ISTART+1
          GOTO 100
        ENDIF
!
!---  Eliminate trailing blank spaces  ---
!
        ISTOP = ISTOP-1
  110   CONTINUE
        IF( ICHAR(CHDUM(ISTOP:ISTOP)).EQ.32 ) THEN
          ISTOP = ISTOP-1
          GOTO 110
        ENDIF
!
!---  Check for non-numerical characters  ---
!
        DO 120 N = ISTART,ISTOP
          NC = ICHAR(CHDUM(N:N))
          IF( N.EQ.ISTART .AND. ( NC.EQ.43 .OR. NC.EQ.45 ) ) GOTO 120
          IF( NC.LT.48 .OR. NC.GT.57 ) GOTO 200
  120   CONTINUE
!
!---  Character string recognized as an integer  ---
!
        INDX = 1
      ENDIF
  200 CONTINUE
      ISTART = ISX
      ICOMMA = ICX
!
!---  End of CHK_INT group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CONN_LST
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
!     Search grid for internal boundary surfaces
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNNL, February 2012.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE CONST
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/CONN_LST'
!
!---  Search grid for internal boundary surfaces  ---
!
      DO 100 N = 1,LFD-LBR
        I = ID(N)
        J = JD(N)
        K = KD(N)
        NPX = (K-1)*LFY*(LFX+1) + (J-1)*(LFX+1) + I
        NPY = (K-1)*(LFY+1)*LFX + (J-1)*LFX + I
        NPZ = N
        NQX = NPX + 1
        NQY = NPY + LFX
        NQZ = NPZ + LFXY
!
!---    Bottom surface  ---
!
        IF( K.NE.1 ) THEN
          NB = N-LFXY
          IF( ABS(XE(1,N)-XE(5,NB)).GT.EPSL .OR. 
     &        ABS(YE(1,N)-YE(5,NB)).GT.EPSL .OR.
     &        ABS(ZE(1,N)-ZE(5,NB)).GT.EPSL .OR.
     &        ABS(XE(2,N)-XE(6,NB)).GT.EPSL .OR. 
     &        ABS(YE(2,N)-YE(6,NB)).GT.EPSL .OR.
     &        ABS(ZE(2,N)-ZE(6,NB)).GT.EPSL .OR.
     &        ABS(XE(3,N)-XE(7,NB)).GT.EPSL .OR. 
     &        ABS(YE(3,N)-YE(7,NB)).GT.EPSL .OR.
     &        ABS(ZE(3,N)-ZE(7,NB)).GT.EPSL .OR.
     &        ABS(XE(4,N)-XE(8,NB)).GT.EPSL .OR. 
     &        ABS(YE(4,N)-YE(8,NB)).GT.EPSL .OR.
     &        ABS(ZE(4,N)-ZE(8,NB)).GT.EPSL ) THEN
            INBS(1,N) = NPZ
            INBS(6,NB) = NQZ
          ENDIF
        ENDIF
!
!---    South surface  ---
!
        IF( J.NE.1 ) THEN
          NS = N-LFX
          IF( ABS(XE(1,N)-XE(3,NS)).GT.EPSL .OR. 
     &        ABS(YE(1,N)-YE(3,NS)).GT.EPSL .OR.
     &        ABS(ZE(1,N)-ZE(3,NS)).GT.EPSL .OR.
     &        ABS(XE(2,N)-XE(4,NS)).GT.EPSL .OR. 
     &        ABS(YE(2,N)-YE(4,NS)).GT.EPSL .OR.
     &        ABS(ZE(2,N)-ZE(4,NS)).GT.EPSL .OR.
     &        ABS(XE(5,N)-XE(7,NS)).GT.EPSL .OR. 
     &        ABS(YE(5,N)-YE(7,NS)).GT.EPSL .OR.
     &        ABS(ZE(5,N)-ZE(7,NS)).GT.EPSL .OR.
     &        ABS(XE(6,N)-XE(8,NS)).GT.EPSL .OR. 
     &        ABS(YE(6,N)-YE(8,NS)).GT.EPSL .OR.
     &        ABS(ZE(6,N)-ZE(8,NS)).GT.EPSL ) THEN
            INBS(2,N) = NPY
            INBS(5,NS) = NQY
          ENDIF
        ENDIF
!
!---    West surface  ---
!
        IF( I.NE.1 ) THEN
          NW = N-1
          IF( ABS(XE(1,N)-XE(2,NW)).GT.EPSL .OR. 
     &        ABS(YE(1,N)-YE(2,NW)).GT.EPSL .OR.
     &        ABS(ZE(1,N)-ZE(2,NW)).GT.EPSL .OR.
     &        ABS(XE(3,N)-XE(4,NW)).GT.EPSL .OR. 
     &        ABS(YE(3,N)-YE(4,NW)).GT.EPSL .OR.
     &        ABS(ZE(3,N)-ZE(4,NW)).GT.EPSL .OR.
     &        ABS(XE(5,N)-XE(6,NW)).GT.EPSL .OR. 
     &        ABS(YE(5,N)-YE(6,NW)).GT.EPSL .OR.
     &        ABS(ZE(5,N)-ZE(6,NW)).GT.EPSL .OR.
     &        ABS(XE(7,N)-XE(8,NW)).GT.EPSL .OR. 
     &        ABS(YE(7,N)-YE(8,NW)).GT.EPSL .OR.
     &        ABS(ZE(7,N)-ZE(8,NW)).GT.EPSL ) THEN
            INBS(3,N) = NPX
            INBS(4,NW) = NQX
          ENDIF
        ENDIF
!
!---    East surface  ---
!
        IF( I.NE.LFX ) THEN
          NE = N+1
          IF( ABS(XE(1,NE)-XE(2,N)).GT.EPSL .OR. 
     &        ABS(YE(1,NE)-YE(2,N)).GT.EPSL .OR.
     &        ABS(ZE(1,NE)-ZE(2,N)).GT.EPSL .OR.
     &        ABS(XE(3,NE)-XE(4,N)).GT.EPSL .OR. 
     &        ABS(YE(3,NE)-YE(4,N)).GT.EPSL .OR.
     &        ABS(ZE(3,NE)-ZE(4,N)).GT.EPSL .OR.
     &        ABS(XE(5,NE)-XE(6,N)).GT.EPSL .OR. 
     &        ABS(YE(5,NE)-YE(6,N)).GT.EPSL .OR.
     &        ABS(ZE(5,NE)-ZE(6,N)).GT.EPSL .OR.
     &        ABS(XE(7,NE)-XE(8,N)).GT.EPSL .OR. 
     &        ABS(YE(7,NE)-YE(8,N)).GT.EPSL .OR.
     &        ABS(ZE(7,NE)-ZE(8,N)).GT.EPSL ) THEN
            INBS(3,NE) = NPX
            LSX = LSX + 1
            INBS(4,N) = LSX
          ENDIF
        ENDIF
!
!---    North surface  ---
!
        IF( J.NE.LFY ) THEN
          NN = N+LFX
          IF( ABS(XE(1,NN)-XE(3,N)).GT.EPSL .OR. 
     &        ABS(YE(1,NN)-YE(3,N)).GT.EPSL .OR.
     &        ABS(ZE(1,NN)-ZE(3,N)).GT.EPSL .OR.
     &        ABS(XE(2,NN)-XE(4,N)).GT.EPSL .OR. 
     &        ABS(YE(2,NN)-YE(4,N)).GT.EPSL .OR.
     &        ABS(ZE(2,NN)-ZE(4,N)).GT.EPSL .OR.
     &        ABS(XE(5,NN)-XE(7,N)).GT.EPSL .OR. 
     &        ABS(YE(5,NN)-YE(7,N)).GT.EPSL .OR.
     &        ABS(ZE(5,NN)-ZE(7,N)).GT.EPSL .OR.
     &        ABS(XE(6,NN)-XE(8,N)).GT.EPSL .OR. 
     &        ABS(YE(6,NN)-YE(8,N)).GT.EPSL .OR.
     &        ABS(ZE(6,NN)-ZE(8,N)).GT.EPSL ) THEN
            INBS(2,NN) = NPY
            LSY = LSY + 1
            INBS(5,N) = LSY
          ENDIF
        ENDIF
!
!---    Top surface  ---
!
        IF( K.NE.LFZ ) THEN
          NT = N+LFXY
          IF( ABS(XE(1,NT)-XE(5,N)).GT.EPSL .OR. 
     &        ABS(YE(1,NT)-YE(5,N)).GT.EPSL .OR.
     &        ABS(ZE(1,NT)-ZE(5,N)).GT.EPSL .OR.
     &        ABS(XE(2,NT)-XE(6,N)).GT.EPSL .OR. 
     &        ABS(YE(2,NT)-YE(6,N)).GT.EPSL .OR.
     &        ABS(ZE(2,NT)-ZE(6,N)).GT.EPSL .OR.
     &        ABS(XE(3,NT)-XE(7,N)).GT.EPSL .OR. 
     &        ABS(YE(3,NT)-YE(7,N)).GT.EPSL .OR.
     &        ABS(ZE(3,NT)-ZE(7,N)).GT.EPSL .OR.
     &        ABS(XE(4,NT)-XE(8,N)).GT.EPSL .OR. 
     &        ABS(YE(4,NT)-YE(8,N)).GT.EPSL .OR.
     &        ABS(ZE(4,NT)-ZE(8,N)).GT.EPSL ) THEN
            INBS(1,NT) = NPZ
            LSZ = LSZ + 1
            INBS(6,N) = LSZ
          ENDIF
        ENDIF
  100 CONTINUE
!
!---  End of CONN_LST group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CONN_MAP
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
!     Create a node connection map and determine the maximum
!     number of connections
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNNL, January 2014.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE CONST
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/CONN_MAP'
!
!---  Allocate memory for coupled-well Jacobian index array  ---
!
      IF( .NOT.ALLOCATED(ICM) ) THEN
        ALLOCATE( ICM(1:5,1:6,1:LFD),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: ICM'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Initialize memory for coupled-well Jacobian index array  ---
!
      DO 10 K = 1,LFD
      DO 10 J = 1,6
      DO 10 I = 1,5
        ICM(I,J,K) = 0
   10 CONTINUE
!
!---  Bottom connections  ---
!
      DO 100 N = 1,LFD-LBR
        I = ID(N)
        J = JD(N)
        K = KD(N)
        NB = N-LFXY
!
!---    Bottom surface is an external boundary surface  ---
!
        IBX = 0
        IF( K.EQ.1 ) THEN
          IBX = 1
        ELSE
!
!---      Bottom surface is an internal boundary surface  ---
!
          IF( IXP(N).EQ.0 .OR. IXP(NB).EQ.0 .OR.
     &      INBS(1,N).NE.0 .OR. INBS(6,NB).NE.0 ) IBX = 1
          IF( IBX.EQ.0 ) ICM(5,1,N) = NB
        ENDIF
!
!---    Node refinement  ---
!
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 90 KX = 1,KRX
        IF( KX.EQ.1 .AND. IBX.EQ.1 ) CYCLE
        DO 80 JX = 1,JRX
        DO 70 IX = 1,IRX
          NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
          NC = 0
          IF( KX.EQ.1 ) THEN
!
!---        Bottom node refinement, in the x- and y-directions  ---
!
            IRBX = 2**IBR(1,NB)
            JRBX = 2**IBR(2,NB)
            KRBX = 2**IBR(3,NB)
            KBX = KRBX
            IF( JRBX.GT.JRX ) THEN
              J1X = 2*JX - 1
              J2X = 2*JX
            ELSEIF( JRBX.LT.JRX ) THEN
              J1X = (JX+1)/2
              J2X = (JX+1)/2
            ELSE
              J1X = JX
              J2X = JX
            ENDIF
            IF( IRBX.GT.IRX ) THEN
              I1X = 2*IX - 1
              I2X = 2*IX
            ELSEIF( IRBX.LT.IRX ) THEN
              I1X = (IX+1)/2
              I2X = (IX+1)/2
            ELSE
              I1X = IX
              I2X = IX
            ENDIF
            DO 60 JBX = J1X,J2X
            DO 50 IBX = I1X,I2X
              NRBX = IBR(4,NB) + ND_BR(IBX,IRBX,JBX,JRBX,KBX)
              NC = NC+1
              ICM(NC,1,NX) = NRBX
   50       CONTINUE
   60       CONTINUE
          ELSE
            KBX = KX-1
            NRBX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KBX)
            NC = NC+1
            ICM(NC,1,NX) = NRBX
          ENDIF
   70   CONTINUE
   80   CONTINUE
   90   CONTINUE
  100 CONTINUE
!
!---  South connections  ---
!
      DO 200 N = 1,LFD-LBR
        I = ID(N)
        J = JD(N)
        K = KD(N)
        NS = N-LFX
!
!---    South surface is an external boundary surface  ---
!
        IBX = 0
        IF( J.EQ.1 ) THEN
          IBX = 1
        ELSE
!
!---      South surface is an internal boundary surface  ---
!
          IF( IXP(N).EQ.0 .OR. IXP(NS).EQ.0 .OR.
     &      INBS(2,N).NE.0 .OR. INBS(5,NS).NE.0 ) IBX = 1
        ENDIF
        IF( IBX.EQ.0 ) ICM(5,2,N) = NS
!
!---    Node refinement  ---
!
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 190 KX = 1,KRX
        DO 180 JX = 1,JRX
        IF( JX.EQ.1 .AND. IBX.EQ.1 ) CYCLE
        DO 170 IX = 1,IRX
          NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
          NC = 0
          IF( JX.EQ.1 ) THEN
!
!---        South node refinement, in the x- and y-directions  ---
!
            IRSX = 2**IBR(1,NS)
            JRSX = 2**IBR(2,NS)
            KRSX = 2**IBR(3,NS)
            JSX = JRSX
            IF( KRSX.GT.KRX ) THEN
              K1X = 2*KX - 1
              K2X = 2*KX
            ELSEIF( KRSX.LT.KRX ) THEN
              K1X = (KX+1)/2
              K2X = (KX+1)/2
            ELSE
              K1X = KX
              K2X = KX
            ENDIF
            IF( IRSX.GT.IRX ) THEN
              I1X = 2*IX - 1
              I2X = 2*IX
            ELSEIF( IRSX.LT.IRX ) THEN
              I1X = (IX+1)/2
              I2X = (IX+1)/2
            ELSE
              I1X = IX
              I2X = IX
            ENDIF
            DO 160 KSX = K1X,K2X
            DO 150 ISX = I1X,I2X
              NRSX = IBR(4,NS) + ND_BR(ISX,IRSX,JSX,JRSX,KSX)
              NC = NC+1
              ICM(NC,2,NX) = NRSX
  150       CONTINUE
  160       CONTINUE
          ELSE
            JSX = JX-1
            NRSX = IBR(4,N) + ND_BR(IX,IRX,JSX,JRX,KX)
            NC = NC+1
            ICM(NC,2,NX) = NRSX
          ENDIF
  170   CONTINUE
  180   CONTINUE
  190   CONTINUE
  200 CONTINUE
!
!---  West connections  ---
!
      DO 300 N = 1,LFD-LBR
        I = ID(N)
        J = JD(N)
        K = KD(N)
        NW = N-1
!
!---    West surface is an external boundary surface  ---
!
        IBX = 0
        IF( I.EQ.1 ) THEN
          IBX = 1
        ELSE
!
!---      West surface is an internal boundary surface  ---
!
          IF( IXP(N).EQ.0 .OR. IXP(NW).EQ.0 .OR.
     &      INBS(3,N).NE.0 .OR. INBS(4,NW).NE.0 ) IBX = 1
        ENDIF
        IF( IBX.EQ.0 ) ICM(5,3,N) = NW
!
!---    Node refinement  ---
!
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 290 KX = 1,KRX
        DO 280 JX = 1,JRX
        DO 270 IX = 1,IRX
        IF( IX.EQ.1 .AND. IBX.EQ.1 ) CYCLE
          NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
          NC = 0
          IF( IX.EQ.1 ) THEN
!
!---        West node refinement, in the y- and z-directions  ---
!
            IRWX = 2**IBR(1,NW)
            JRWX = 2**IBR(2,NW)
            KRWX = 2**IBR(3,NW)
            IWX = IRWX
            IF( KRWX.GT.KRX ) THEN
              K1X = 2*KX - 1
              K2X = 2*KX
            ELSEIF( KRWX.LT.KRX ) THEN
              K1X = (KX+1)/2
              K2X = (KX+1)/2
            ELSE
              K1X = KX
              K2X = KX
            ENDIF
            IF( JRWX.GT.JRX ) THEN
              J1X = 2*JX - 1
              J2X = 2*JX
            ELSEIF( JRWX.LT.JRX ) THEN
              J1X = (JX+1)/2
              J2X = (JX+1)/2
            ELSE
              J1X = JX
              J2X = JX
            ENDIF
            DO 260 KWX = K1X,K2X
            DO 250 JWX = J1X,J2X
              NRWX = IBR(4,NW) + ND_BR(IWX,IRWX,JWX,JRWX,KWX)
              NC = NC+1
              ICM(NC,3,NX) = NRWX
  250       CONTINUE
  260       CONTINUE
          ELSE
            IWX = IX-1
            NRWX = IBR(4,N) + ND_BR(IWX,IRX,JX,JRX,KX)
            NC = NC+1
            ICM(NC,3,NX) = NRWX
          ENDIF
  270   CONTINUE
  280   CONTINUE
  290   CONTINUE
  300 CONTINUE
!
!---  East connections  ---
!
      DO 400 N = 1,LFD-LBR
        I = ID(N)
        J = JD(N)
        K = KD(N)
        NE = N+1
!
!---    East surface is an external boundary surface  ---
!
        IBX = 0
        IF( I.EQ.LFX ) THEN
          IBX = 1
        ELSE
!
!---      East surface is an internal boundary surface  ---
!
          IF( IXP(N).EQ.0 .OR. IXP(NE).EQ.0 .OR.
     &      INBS(4,N).NE.0 .OR. INBS(3,NE).NE.0 ) IBX = 1
        ENDIF
        IF( IBX.EQ.0 ) ICM(5,4,N) = NE
!
!---    Node refinement  ---
!
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 390 KX = 1,KRX
        DO 380 JX = 1,JRX
        DO 370 IX = 1,IRX
        IF( IX.EQ.IRX .AND. IBX.EQ.1 ) CYCLE
          NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
          NC = 0
          IF( IX.EQ.IRX ) THEN
!
!---        East node refinement, in the y- and z-directions  ---
!
            IREX = 2**IBR(1,NE)
            JREX = 2**IBR(2,NE)
            KREX = 2**IBR(3,NE)
            IEX = 1
            IF( KREX.GT.KRX ) THEN
              K1X = 2*KX - 1
              K2X = 2*KX
            ELSEIF( KREX.LT.KRX ) THEN
              K1X = (KX+1)/2
              K2X = (KX+1)/2
            ELSE
              K1X = KX
              K2X = KX
            ENDIF
            IF( JREX.GT.JRX ) THEN
              J1X = 2*JX - 1
              J2X = 2*JX
            ELSEIF( JREX.LT.JRX ) THEN
              J1X = (JX+1)/2
              J2X = (JX+1)/2
            ELSE
              J1X = JX
              J2X = JX
            ENDIF
            DO 360 KEX = K1X,K2X
            DO 350 JEX = J1X,J2X
              NREX = IBR(4,NE) + ND_BR(IEX,IREX,JEX,JREX,KEX)
              NC = NC+1
              ICM(NC,4,NX) = NREX
  350       CONTINUE
  360       CONTINUE
          ELSE
            IEX = IX+1
            NREX = IBR(4,N) + ND_BR(IEX,IRX,JX,JRX,KX)
            NC = NC+1
            ICM(NC,4,NX) = NREX
          ENDIF
  370   CONTINUE
  380   CONTINUE
  390   CONTINUE
  400 CONTINUE
!
!---  North connections  ---
!
      DO 500 N = 1,LFD-LBR
        I = ID(N)
        J = JD(N)
        K = KD(N)
        NN = N+LFX
!
!---    North surface is an external boundary surface  ---
!
        IBX = 0
        IF( J.EQ.LFY ) THEN
          IBX = 1
        ELSE
!
!---      North surface is an internal boundary surface  ---
!
          IF( IXP(N).EQ.0 .OR. IXP(NN).EQ.0 .OR.
     &      INBS(5,N).NE.0 .OR. INBS(2,NN).NE.0 ) IBX = 1
        ENDIF
        IF( IBX.EQ.0 ) ICM(5,5,N) = NN
!
!---    Node refinement  ---
!
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 490 KX = 1,KRX
        DO 480 JX = 1,JRX
        IF( JX.EQ.JRX .AND. IBX.EQ.1 ) CYCLE
        DO 470 IX = 1,IRX
          NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
          NC = 0
          IF( JX.EQ.JRX ) THEN
!
!---        North node refinement, in the x- and y-directions  ---
!
            IRNX = 2**IBR(1,NN)
            JRNX = 2**IBR(2,NN)
            KRNX = 2**IBR(3,NN)
            JNX = 1
            IF( KRNX.GT.KRX ) THEN
              K1X = 2*KX - 1
              K2X = 2*KX
            ELSEIF( KRNX.LT.KRX ) THEN
              K1X = (KX+1)/2
              K2X = (KX+1)/2
            ELSE
              K1X = KX
              K2X = KX
            ENDIF
            IF( IRNX.GT.IRX ) THEN
              I1X = 2*IX - 1
              I2X = 2*IX
            ELSEIF( IRNX.LT.IRX ) THEN
              I1X = (IX+1)/2
              I2X = (IX+1)/2
            ELSE
              I1X = IX
              I2X = IX
            ENDIF
            DO 460 KNX = K1X,K2X
            DO 450 INX = I1X,I2X
              NRNX = IBR(4,NN) + ND_BR(INX,IRNX,JNX,JRNX,KNX)
              NC = NC+1
              ICM(NC,5,NX) = NRNX
  450       CONTINUE
  460       CONTINUE
          ELSE
            JNX = JX+1
            NRNX = IBR(4,N) + ND_BR(IX,IRX,JNX,JRX,KX)
            NC = NC+1
            ICM(NC,5,NX) = NRNX
          ENDIF
  470   CONTINUE
  480   CONTINUE
  490   CONTINUE
  500 CONTINUE
!
!---  Top connections  ---
!
      DO 600 N = 1,LFD-LBR
        I = ID(N)
        J = JD(N)
        K = KD(N)
        NT = N+LFXY
!
!---    Top surface is an external boundary surface  ---
!
        IBX = 0
        IF( K.EQ.LFZ ) THEN
          IBX = 1
        ELSE
!
!---      Top surface is an internal boundary surface  ---
!
          IF( IXP(N).EQ.0 .OR. IXP(NT).EQ.0 .OR.
     &      INBS(6,N).NE.0 .OR. INBS(1,NT).NE.0 ) IBX = 1
        ENDIF
        IF( IBX.EQ.0 ) ICM(5,6,N) = NT
!
!---    Node refinement  ---
!
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 590 KX = 1,KRX
        IF( KX.EQ.KRX .AND. IBX.EQ.1 ) CYCLE
        DO 580 JX = 1,JRX
        DO 570 IX = 1,IRX
          NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
          NC = 0
          IF( KX.EQ.KRX ) THEN
!
!---        Top node refinement, in the x- and y-directions  ---
!
            IRTX = 2**IBR(1,NT)
            JRTX = 2**IBR(2,NT)
            KRTX = 2**IBR(3,NT)
            KTX = 1
            NC = 0
            IF( JRTX.GT.JRX ) THEN
              J1X = 2*JX - 1
              J2X = 2*JX
            ELSEIF( JRTX.LT.JRX ) THEN
              J1X = (JX+1)/2
              J2X = (JX+1)/2
            ELSE
              J1X = JX
              J2X = JX
            ENDIF
            IF( IRTX.GT.IRX ) THEN
              I1X = 2*IX - 1
              I2X = 2*IX
            ELSEIF( IRTX.LT.IRX ) THEN
              I1X = (IX+1)/2
              I2X = (IX+1)/2
            ELSE
              I1X = IX
              I2X = IX
            ENDIF
            DO 560 JTX = J1X,J2X
            DO 550 ITX = I1X,I2X
              NRTX = IBR(4,NT) + ND_BR(ITX,IRTX,JTX,JRTX,KTX)
              NC = NC+1
              ICM(NC,6,NX) = NRTX
  550       CONTINUE
  560       CONTINUE
          ELSE
            KTX = KX+1
            NRTX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KTX)
            NC = NC+1
            ICM(NC,6,NX) = NRTX
          ENDIF
  570   CONTINUE
  580   CONTINUE
  590   CONTINUE
  600 CONTINUE
!
!---  Maximum number of connections  ---
!
      NCMX = 0
      DO 800 N = 1,LFD-LBR
        I = ID(N)
        J = JD(N)
        K = KD(N)
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 790 KX = 1,KRX
        DO 780 JX = 1,JRX
        DO 770 IX = 1,IRX
          NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
          NCX = 0
          DO 760 NR = 1,4
            DO 750 NS = 1,6
              IF( ICM(NR,NS,NX).NE.0 ) NCX = NCX + 1
  750       CONTINUE
  760     CONTINUE
          NCMX = MAX( NCMX,NCX )
  770   CONTINUE
  780   CONTINUE
  790   CONTINUE
  800 CONTINUE
      LSTC = MAX( LSTC,NCMX+1 )
!
!---  End of CONN_MAP group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CONN_MAP_SFC
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
!     Determine the maximum number of field nodes connected
!     to a surface cover node via plant roots
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 12 November 2015.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE CONST
      USE PLT_ATM
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 XPX(4),YPX(4),ZPX(4)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/CONN_MAP_SFC'
!
!---  Find maximum plant root depth  ---
!
      PRDMX = 0.D+0
      DO IP = 1,NPLANT
        PRDMX = MAX( PRDMX,PARMS_P(1,IP) )
      ENDDO
!
!---  Loop over number of surface cover areas  ---
!
      DO NSCA = 1,NSFCA
!
!---    Loop over surface cover nodes  ---
!
        DO NSCN = 1,NSFCN
          NC = 1
          PRDX = PRDMX
          N = ICM_SFC(1,NSCN)
!
!---      Continuous loop from surface cover node down field nodes  ---
!
          DO
!
!---        Inactive node, exit  ---
!
            IF( IXP(N).EQ.0 ) EXIT
!
!---        Top surface centroid  ---
!
            XPX(1) = XE(5,N)
            XPX(2) = XE(6,N)
            XPX(3) = XE(8,N)
            XPX(4) = XE(7,N)
            YPX(1) = YE(5,N)
            YPX(2) = YE(6,N)
            YPX(3) = YE(8,N)
            YPX(4) = YE(7,N)
            ZPX(1) = ZE(5,N)
            ZPX(2) = ZE(6,N)
            ZPX(3) = ZE(8,N)
            ZPX(4) = ZE(7,N)
            NP = 4
            CALL PG_CNTRD( NP,XPX,YPX,ZPX,XFTX,YFTX,ZFTX )
!
!---        Bottom surface vertices  ---
!
            XPX(1) = XE(1,N)
            XPX(2) = XE(2,N)
            XPX(3) = XE(4,N)
            XPX(4) = XE(3,N)
            YPX(1) = YE(1,N)
            YPX(2) = YE(2,N)
            YPX(3) = YE(4,N)
            YPX(4) = YE(3,N)
            ZPX(1) = ZE(1,N)
            ZPX(2) = ZE(2,N)
            ZPX(3) = ZE(4,N)
            ZPX(4) = ZE(3,N)
            NP = 4
            CALL PG_CNTRD( NP,XPX,YPX,ZPX,XFBX,YFBX,ZFBX )
            DZGFX = SQRT((XFTX-XFBX)**2 + (YFTX-YFBX)**2 + 
     &        (ZFTX-ZFBX)**2)
!
!---        Roots continue through to next deeper field node  ---
!
            IF( PRDX.GT.DZGFX ) THEN
              NC = NC + 1
              PRDX = PRDX - DZGFX
              N = N - LFX*LFY
!
!---          Bottom of domain, exit  ---
!
              IF( N.LE.0 ) EXIT
!
!---          Inactive node, exit  ---
!
              IF( IXP(N).EQ.0 ) EXIT
            ELSE
!
!---          Maximum root depth ends within node  ---
!
              EXIT
            ENDIF
          ENDDO
          LSFCC = MAX( LSFCC,NC )
        ENDDO
      ENDDO
!
!---  Maximum number of surface cover equation unknowns  ---
!
      LUK_SFC = 2*(2*LUK + 3*LUK*LSFCC) + 13
!
!---  End of CONN_MAP_SFC group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CONN_FEN
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
!     Create a map of finite-element nodes for geomechanics.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 28 October 2016.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GEOMECH
      USE CONST
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER JNDX(4,15,8)
!
!----------------------Data Statements---------------------------------!
!
      DATA JNDX / 1,0,0,5,1,2,0,7,1,2,3,8,1,3,0,6,
     &            1,3,2,8,2,0,0,3,2,3,0,4,2,3,1,8,
     &            2,1,0,7,2,1,3,8,3,0,0,2,3,1,0,6,
     &            3,1,2,8,3,2,0,4,3,2,1,8,1,0,0,6,
     &            1,2,0,8,1,2,4,7,1,4,0,5,1,4,2,7,
     &            2,0,0,4,2,4,0,3,2,4,1,7,2,1,0,8,
     &            2,1,4,7,4,0,0,1,4,1,0,5,4,1,2,7,
     &            4,2,0,3,4,2,1,7,1,0,0,7,1,5,0,5,
     &            1,5,3,6,1,3,0,8,1,3,5,6,5,0,0,1,
     &            5,3,0,2,5,3,1,6,5,1,0,5,5,1,3,6,
     &            3,0,0,4,3,1,0,8,3,1,5,6,3,5,0,2,
     &            3,5,1,6,1,0,0,8,1,5,0,6,1,5,4,5,
     &            1,4,0,7,1,4,5,5,5,0,0,2,5,4,0,1,
     &            5,4,1,5,5,1,0,6,5,1,4,5,4,0,0,3,
     &            4,1,0,7,4,1,5,5,4,5,0,1,4,5,1,5,
     &            6,0,0,1,6,2,0,3,6,2,3,4,6,3,0,2,
     &            6,3,2,4,2,0,0,7,2,3,0,8,2,3,6,4,
     &            2,6,0,3,2,6,3,4,3,0,0,6,3,6,0,2,
     &            3,6,2,4,3,2,0,8,3,2,6,4,6,0,0,2,
     &            6,2,0,4,6,2,4,3,6,4,0,1,6,4,2,3,
     &            2,0,0,8,2,4,0,7,2,4,6,3,2,6,0,4,
     &            2,6,4,3,4,0,0,5,4,6,0,1,4,6,2,3,
     &            4,2,0,7,4,2,6,3,6,0,0,3,6,5,0,1,
     &            6,5,3,2,6,3,0,4,6,3,5,2,5,0,0,5,
     &            5,3,0,6,5,3,6,2,5,6,0,1,5,6,3,2,
     &            3,0,0,8,3,6,0,4,3,6,5,2,3,5,0,6,
     &            3,5,6,2,6,0,0,4,6,5,0,2,6,5,4,1,
     &            6,4,0,3,6,4,5,1,5,0,0,6,5,4,0,5,
     &            5,4,6,1,5,6,0,2,5,6,4,1,4,0,0,7,
     &            4,6,0,3,4,6,5,1,4,5,0,5,4,5,6,1 /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/CONN_FEN'
!
!---  Allocate memory for the finite-element node map  ---
!
      IF( .NOT.ALLOCATED(ND_GM) ) THEN
        ALLOCATE( ND_GM(1:8,1:LFD),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: ND_GM'
          CALL WRMSGP( INDX )
        ENDIF
        DO M1 = 1,8
          DO M2 = 1,LFD
            ND_GM(M1,M2) = 0
          ENDDO
        ENDDO
      ENDIF
!
!---  Loop over all nodes create a map of finite element nodes,
!     where nodes are numbered in the order around the hexahedron
!     grid cells as
!
!     1 i,j,k
!     2 i+1,j,k
!     3 i,j+1,k
!     4 i+1,j+1,k
!     5 i,j,k+1
!     6 i+1,j,k+1
!     7 i,j+1,k+1
!     8 i+1,j+1,k+1  ---
!
      NC = 0
      DO N = 1,LFD
        IF( IXP(N).EQ.0 ) CYCLE
!
!---    Loop over FE Nodes  ---
!
        DO NFE = 1,8
          IF( ND_GM(NFE,N).EQ.0 ) THEN
            NC = NC + 1
            ND_GM(NFE,N) = NC
!
!---        Loop over number of connection paths  ---
!
            DO NCP = 1,15
!
!---          Loop over number of connection tiers  ---
!
              NX = N
              DO NCT = 1,3
                IX = JNDX(NCT,NCP,NFE)
                IF( JNDX(NCT,NCP,NFE).EQ.0 ) EXIT
!
!---            Connection to neighboring element found  ---
!
                IF( ICM(5,IX,NX).NE.0 ) THEN
                  NX = ICM(5,IX,NX)
!
!---            Connection string broken  ---
!
                ELSE
                  NX = 0
                  EXIT
                ENDIF
              ENDDO
!
!---         Connection string completed  ---
!
              IF( NX.NE.0 ) THEN
                JX = JNDX(4,NCP,NFE)
                ND_GM(JX,NX) = NC
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO
!
!---  Number of finite element nodes  ---
!
      LFEN = MAX( NC,1 )
!
!---  Allocate memory for the inverse FE node map  ---
!
      IF( .NOT.ALLOCATED(NE_GM) ) THEN
        ALLOCATE( NE_GM(1:8,1:LFEN),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: NE_GM'
          CALL WRMSGP( INDX )
        ENDIF
        DO M1 = 1,8
          DO M2 = 1,LFEN
            NE_GM(M1,M2) = 0
          ENDDO
        ENDDO
      ENDIF
!
!---  Loop over all nodes, creating an inverse map of FE nodes  ---
!
      DO N = 1,LFD
        IF( IXP(N).EQ.0 ) CYCLE
!
!---    Loop over FE Nodes  ---
!
        DO NFE = 1,8
          IF( ND_GM(NFE,N).EQ.0 ) CYCLE
          NC = ND_GM(NFE,N)
          NE_GM(NFE,NC) = N
        ENDDO
      ENDDO
!
!---  End of CONN_FEN group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Function----------------------------------------!
!
      FUNCTION I_COUNT( I )
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
!     Count the number of digits in an integer variable.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, March, 1993.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/I_COUNT'
      IC = I
      I_COUNT = 0
   10 CONTINUE
      I_COUNT = I_COUNT + 1
      IC = IC/10
      IF( IC.GT.0 ) GOTO 10
!
!---  End of I_COUNT group
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCB_CW
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
!     Determine the matrix half band width for the coupled
!     well model
!
!----------------------Authors-----------------------------------------!
!
!     Written by M.D. White, PNNL, 03 February 2014.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE JACOB
      USE GRID
      USE COUP_WELL
      USE CONST
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: M_IJK,M_JKI,M_KIJ
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/JCB_CW'

!
!---  Allocate memory for Jacobian matrix equation index  ---
!
      IF( .NOT.ALLOCATED(IM) ) THEN
        ALLOCATE( IM(1:LUK,1:LFD+LBR),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: IM'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Allocate memory for coupled-well Jacobian index array  ---
!
      IF( .NOT.ALLOCATED(JM_CW) ) THEN
        ALLOCATE( JM_CW(1:LN_CW),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: JM_CW'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Allocate memory for half-band width count array 
!     for IJK index ordering  ---
!
      IF( .NOT.ALLOCATED(M_IJK) ) THEN
        ALLOCATE( M_IJK(1:2,1:LN_CW),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: M_IJK'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Allocate memory for half-band width count array 
!     for JKI index ordering  ---
!
      IF( .NOT.ALLOCATED(M_JKI) ) THEN
        ALLOCATE( M_JKI(1:2,1:LN_CW),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: M_JKI'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Allocate memory for half-band width count array 
!     for KIJ index ordering  ---
!
      IF( .NOT.ALLOCATED(M_KIJ) ) THEN
        ALLOCATE( M_KIJ(1:2,1:LN_CW),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: M_KIJ'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Initialize number of unknowns  ---
!
      ISVC = LUK
      ILIMIT = 2**30
!
!---  Coupled equations half band width using I,J,K ordering  ---
!
      NC = 0
      DO 10 K = 1,LFZ
      DO 10 J = 1,LFY
      DO 10 I = 1,LFX
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 10
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          IXP(N) = -(IRX*JRX*KRX)
          DO 2 KX = 1,KRX
          DO 2 JX = 1,JRX
          DO 2 IX = 1,IRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NC = NC + 1
            IXP(NX) = NC
    2     CONTINUE
        ELSE
          NC = NC + 1
          IXP(N) = NC
        ENDIF
   10 CONTINUE
      DO 60 NCW = 1,LN_CW
        M_IJK(1,NCW) = ILIMIT
        DO 50 IDCW = ID_CW(5,NCW),ID_CW(6,NCW)
          NC = 0
          NWF = IWF_CW(IDCW)
          MHBC_IJK = 0
          DO 20 K = 1,LFZ
          DO 20 J = 1,LFY
          DO 20 I = 1,LFX
            N = ND(I,J,K)
!
!---         Loop over block refined nodes  ---
!
            IF( IBR(4,N).GT.N ) THEN
              IRX = 2**IBR(1,N)
              JRX = 2**IBR(2,N)
              KRX = 2**IBR(3,N)
              DO 14 KX = 1,KRX
              DO 14 JX = 1,JRX
              DO 14 IX = 1,IRX
                NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
                NMD = IXP(NX)
                DO 12 M = 1,ISVC
                  NC = NC+1
                  IM(M,NMD) = NC
   12           CONTINUE
                IF( NWF.EQ.NX ) THEN
                  NC = NC+1
                  JM_CWX = NC
                ENDIF
   14         CONTINUE
            ELSE
              NMD = IXP(N)
!
!---          Skip to next active node  ---
!
              IF( IXP(N).EQ.0 ) GOTO 20
              DO 16 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
   16         CONTINUE
              IF( NWF.EQ.N ) THEN
                NC = NC+1
                JM_CWX = NC
              ENDIF
            ENDIF
   20     CONTINUE
!
!---      Determine matrix half-band widths, using I,J,K ordering  ---
!
          DO 40 K = 1,LFZ
          DO 40 J = 1,LFY
          DO 40 I = 1,LFX
            N = ND(I,J,K)
!
!---        Skip to next active node  ---
!
            IF( IXP(N).EQ.0 ) GOTO 40
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 32 KX = 1,KRX
            DO 32 JX = 1,JRX
            DO 32 IX = 1,IRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NP = IXP(NX)
!
!---          Node  ---
!
              MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---          Bottom node neighbors  ---
!
              IF( K.GT.1 ) THEN
                IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                  DO 21 MX = 1,4
                    NB = ICM(MX,1,NX)
                    IF( NB.EQ.0 ) EXIT
                    NB = IXP(NB)
                    MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NB)),
     &                ABS(IM(ISVC,NP)-IM(1,NB)))
   21             CONTINUE
                ENDIF
              ENDIF
!
!---          South node neighbors  ---
!
              IF( J.GT.1 ) THEN
                IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                  DO 22 MX = 1,4
                    NS = ICM(MX,2,NX)
                    IF( NS.EQ.0 ) EXIT
                    NS = IXP(NS)
                    MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NS)),
     &                ABS(IM(ISVC,NP)-IM(1,NS)))
   22             CONTINUE
                ENDIF
              ENDIF
!
!---          West node neighbors  ---
!
              IF( I.GT.1 ) THEN
                IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                  DO 23 MX = 1,4
                    NW = ICM(MX,3,NX)
                    IF( NW.EQ.0 ) EXIT
                    NW = IXP(NW)
                    MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NW)),
     &                ABS(IM(ISVC,NP)-IM(1,NW)))
   23             CONTINUE
               ENDIF
              ENDIF
!
!---          East node neighbors  ---
!
              IF( I.LT.LFX ) THEN
                IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                  DO 24 MX = 1,4
                    NE = ICM(MX,4,NX)
                    IF( NE.EQ.0 ) EXIT
                    NE = IXP(NE)
                    MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NE)),
     &                ABS(IM(ISVC,NP)-IM(1,NE)))
   24             CONTINUE
                ENDIF
              ENDIF
!
!---          North node neighbors  ---
!
              IF( J.LT.LFY ) THEN
                IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                  DO 25 MX = 1,4
                    NN = ICM(MX,5,NX)
                    IF( NN.EQ.0 ) EXIT
                    NN = IXP(NN)
                    MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NN)),
     &                ABS(IM(ISVC,NP)-IM(1,NN)))
   25             CONTINUE
                ENDIF
              ENDIF
!
!---          Top node neighbors  ---
!
              IF( K.LT.LFZ ) THEN
                IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                  DO 26 MX = 1,4
                    NT = ICM(MX,6,NX)
                    IF( NT.EQ.0 ) EXIT
                    NT = IXP(NT)
                    MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NT)),
     &                ABS(IM(ISVC,NP)-IM(1,NT)))
   26             CONTINUE
                ENDIF
              ENDIF
!
!---          Coupled well nodes  ---
!
              DO 30 JDCW = ID_CW(5,NCW),ID_CW(6,NCW)
                IF( IWF_CW(JDCW).EQ.NX ) THEN
                  MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-JM_CWX),
     &              ABS(IM(ISVC,NP)-JM_CWX))
                ENDIF
   30         CONTINUE
   32       CONTINUE
   40     CONTINUE
          IF( MHBC_IJK.LT.M_IJK(1,NCW) ) THEN
            M_IJK(1,NCW) = MHBC_IJK
            M_IJK(2,NCW) = IDCW
          ENDIF
   50   CONTINUE
   60 CONTINUE
!
!---  Coupled equations half band width using J,K,I ordering  ---
!
      NC = 0
      DO 70 I = 1,LFX
      DO 70 K = 1,LFZ
      DO 70 J = 1,LFY
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 70
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          IXP(N) = -(IRX*JRX*KRX)
          DO 62 IX = 1,IRX
          DO 62 KX = 1,KRX
          DO 62 JX = 1,JRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NC = NC + 1
            IXP(NX) = NC
   62     CONTINUE
        ELSE
          NC = NC + 1
          IXP(N) = NC
        ENDIF
   70 CONTINUE
      DO 120 NCW = 1,LN_CW
        M_JKI(1,NCW) = ILIMIT
        DO 110 IDCW = ID_CW(5,NCW),ID_CW(6,NCW)
          NC = 0
          NWF = IWF_CW(IDCW)
          MHBC_JKI = 0
          DO 80 I = 1,LFX
          DO 80 K = 1,LFZ
          DO 80 J = 1,LFY
            N = ND(I,J,K)
!
!---         Loop over block refined nodes  ---
!
            IF( IBR(4,N).GT.N ) THEN
              IRX = 2**IBR(1,N)
              JRX = 2**IBR(2,N)
              KRX = 2**IBR(3,N)
              DO 74 IX = 1,IRX
              DO 74 KX = 1,KRX
              DO 74 JX = 1,JRX
                NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
                NMD = IXP(NX)
                DO 72 M = 1,ISVC
                  NC = NC+1
                  IM(M,NMD) = NC
   72           CONTINUE
                IF( NWF.EQ.NX ) THEN
                  NC = NC+1
                  JM_CWX = NC
                ENDIF
   74         CONTINUE
            ELSE
              NMD = IXP(N)
!
!---          Skip to next active node  ---
!
              IF( IXP(N).EQ.0 ) GOTO 80
              DO 76 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
   76         CONTINUE
              IF( NWF.EQ.N ) THEN
                NC = NC+1
                JM_CWX = NC
              ENDIF
            ENDIF
   80     CONTINUE
!
!---      Determine matrix half-band widths, using J,K,I ordering  ---
!
          DO 100 I = 1,LFX
          DO 100 K = 1,LFZ
          DO 100 J = 1,LFY
            N = ND(I,J,K)
!
!---        Skip to next active node  ---
!
            IF( IXP(N).EQ.0 ) GOTO 100
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 92 IX = 1,IRX
            DO 92 KX = 1,KRX
            DO 92 JX = 1,JRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NP = IXP(NX)
!
!---          Node  ---
!
              MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---          Bottom node neighbors  ---
!
              IF( K.GT.1 ) THEN
                IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                  DO 81 MX = 1,4
                    NB = ICM(MX,1,NX)
                    IF( NB.EQ.0 ) EXIT
                    NB = IXP(NB)
                    MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NB)),
     &                ABS(IM(ISVC,NP)-IM(1,NB)))
   81             CONTINUE
                ENDIF
              ENDIF
!
!---          South node neighbors  ---
!
              IF( J.GT.1 ) THEN
                IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                  DO 82 MX = 1,4
                    NS = ICM(MX,2,NX)
                    IF( NS.EQ.0 ) EXIT
                    NS = IXP(NS)
                    MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NS)),
     &                ABS(IM(ISVC,NP)-IM(1,NS)))
   82             CONTINUE
                ENDIF
              ENDIF
!
!---          West node neighbors  ---
!
              IF( I.GT.1 ) THEN
                IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                  DO 83 MX = 1,4
                    NW = ICM(MX,3,NX)
                    IF( NW.EQ.0 ) EXIT
                    NW = IXP(NW)
                    MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NW)),
     &                ABS(IM(ISVC,NP)-IM(1,NW)))
   83             CONTINUE
               ENDIF
              ENDIF
!
!---          East node neighbors  ---
!
              IF( I.LT.LFX ) THEN
                IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                  DO 84 MX = 1,4
                    NE = ICM(MX,4,NX)
                    IF( NE.EQ.0 ) EXIT
                    NE = IXP(NE)
                    MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NE)),
     &                ABS(IM(ISVC,NP)-IM(1,NE)))
   84             CONTINUE
                ENDIF
              ENDIF
!
!---          North node neighbors  ---
!
              IF( J.LT.LFY ) THEN
                IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                  DO 85 MX = 1,4
                    NN = ICM(MX,5,NX)
                    IF( NN.EQ.0 ) EXIT
                    NN = IXP(NN)
                    MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NN)),
     &                ABS(IM(ISVC,NP)-IM(1,NN)))
   85             CONTINUE
                ENDIF
              ENDIF
!
!---          Top node neighbors  ---
!
              IF( K.LT.LFZ ) THEN
                IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                  DO 86 MX = 1,4
                    NT = ICM(MX,6,NX)
                    IF( NT.EQ.0 ) EXIT
                    NT = IXP(NT)
                    MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NT)),
     &                ABS(IM(ISVC,NP)-IM(1,NT)))
   86             CONTINUE
                ENDIF
              ENDIF
!
!---          Coupled well nodes  ---
!
              DO 90 JDCW = ID_CW(5,NCW),ID_CW(6,NCW)
                IF( IWF_CW(JDCW).EQ.NX ) THEN
                  MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-JM_CWX),
     &              ABS(IM(ISVC,NP)-JM_CWX))
                ENDIF
   90         CONTINUE
   92       CONTINUE
  100     CONTINUE
          IF( MHBC_JKI.LT.M_JKI(1,NCW) ) THEN
            M_JKI(1,NCW) = MHBC_JKI
            M_JKI(2,NCW) = IDCW
          ENDIF
  110   CONTINUE
  120 CONTINUE
!
!---  Coupled equations half band width using K,I,J ordering  ---
!
      NC = 0
      DO 130 J = 1,LFY
      DO 130 I = 1,LFX
      DO 130 K = 1,LFZ
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 130
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          IXP(N) = -(IRX*JRX*KRX)
          DO 122 JX = 1,JRX
          DO 122 IX = 1,IRX
          DO 122 KX = 1,KRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NC = NC + 1
            IXP(NX) = NC
  122     CONTINUE
        ELSE
          NC = NC + 1
          IXP(N) = NC
        ENDIF
  130 CONTINUE
!
!---  Coupled equations half band width using K,I,J ordering  ---
!
      DO 180 NCW = 1,LN_CW
        M_KIJ(1,NCW) = ILIMIT
        DO 170 IDCW = ID_CW(5,NCW),ID_CW(6,NCW)
          NC = 0
          NWF = IWF_CW(IDCW)
          MHBC_KIJ = 0
          DO 140 J = 1,LFY
          DO 140 I = 1,LFX
          DO 140 K = 1,LFZ
            N = ND(I,J,K)
!
!---         Loop over block refined nodes  ---
!
            IF( IBR(4,N).GT.N ) THEN
              IRX = 2**IBR(1,N)
              JRX = 2**IBR(2,N)
              KRX = 2**IBR(3,N)
              DO 134 JX = 1,JRX
              DO 134 IX = 1,IRX
              DO 134 KX = 1,KRX
                NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
                NMD = IXP(NX)
                DO 132 M = 1,ISVC
                  NC = NC+1
                  IM(M,NMD) = NC
  132           CONTINUE
                IF( NWF.EQ.NX ) THEN
                  NC = NC+1
                  JM_CWX = NC
                ENDIF
  134         CONTINUE
            ELSE
              NMD = IXP(N)
!
!---          Skip to next active node  ---
!
              IF( IXP(N).EQ.0 ) GOTO 140
              DO 136 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  136         CONTINUE
              IF( NWF.EQ.N ) THEN
                NC = NC+1
                JM_CWX = NC
              ENDIF
            ENDIF
  140     CONTINUE
!
!---      Determine matrix half-band widths, using K,I,J ordering  ---
!
          DO 160 J = 1,LFY
          DO 160 I = 1,LFX
          DO 160 K = 1,LFZ
            N = ND(I,J,K)
!
!---        Skip to next active node  ---
!
            IF( IXP(N).EQ.0 ) GOTO 160
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 152 IX = 1,IRX
            DO 152 KX = 1,KRX
            DO 152 JX = 1,JRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NP = IXP(NX)
!
!---          Node  ---
!
              MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---          Bottom node neighbors  ---
!
              IF( K.GT.1 ) THEN
                IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                  DO 141 MX = 1,4
                    NB = ICM(MX,1,NX)
                    IF( NB.EQ.0 ) EXIT
                    NB = IXP(NB)
                    MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NB)),
     &                ABS(IM(ISVC,NP)-IM(1,NB)))
  141             CONTINUE
                ENDIF
              ENDIF
!
!---          South node neighbors  ---
!
              IF( J.GT.1 ) THEN
                IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                  DO 142 MX = 1,4
                    NS = ICM(MX,2,NX)
                    IF( NS.EQ.0 ) EXIT
                    NS = IXP(NS)
                    MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NS)),
     &                ABS(IM(ISVC,NP)-IM(1,NS)))
  142             CONTINUE
                ENDIF
              ENDIF
!
!---          West node neighbors  ---
!
              IF( I.GT.1 ) THEN
                IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                  DO 143 MX = 1,4
                    NW = ICM(MX,3,NX)
                    IF( NW.EQ.0 ) EXIT
                    NW = IXP(NW)
                    MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NW)),
     &                ABS(IM(ISVC,NP)-IM(1,NW)))
  143             CONTINUE
               ENDIF
              ENDIF
!
!---          East node neighbors  ---
!
              IF( I.LT.LFX ) THEN
                IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                  DO 144 MX = 1,4
                    NE = ICM(MX,4,NX)
                    IF( NE.EQ.0 ) EXIT
                    NE = IXP(NE)
                    MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NE)),
     &                ABS(IM(ISVC,NP)-IM(1,NE)))
  144             CONTINUE
                ENDIF
              ENDIF
!
!---          North node neighbors  ---
!
              IF( J.LT.LFY ) THEN
                IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                  DO 145 MX = 1,4
                    NN = ICM(MX,5,NX)
                    IF( NN.EQ.0 ) EXIT
                    NN = IXP(NN)
                    MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NN)),
     &                ABS(IM(ISVC,NP)-IM(1,NN)))
  145             CONTINUE
                ENDIF
              ENDIF
!
!---          Top node neighbors  ---
!
              IF( K.LT.LFZ ) THEN
                IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                  DO 146 MX = 1,4
                    NT = ICM(MX,6,NX)
                    IF( NT.EQ.0 ) EXIT
                    NT = IXP(NT)
                    MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NT)),
     &                ABS(IM(ISVC,NP)-IM(1,NT)))
  146             CONTINUE
                ENDIF
              ENDIF
!
!---          Coupled well nodes  ---
!
              DO 150 JDCW = ID_CW(5,NCW),ID_CW(6,NCW)
                IF( IWF_CW(JDCW).EQ.NX ) THEN
                  MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-JM_CWX),
     &              ABS(IM(ISVC,NP)-JM_CWX))
                ENDIF
  150         CONTINUE
  152       CONTINUE
  160     CONTINUE
          IF( MHBC_KIJ.LT.M_KIJ(1,NCW) ) THEN
            M_KIJ(1,NCW) = MHBC_KIJ
            M_KIJ(2,NCW) = IDCW
          ENDIF
  170   CONTINUE
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
!---  IJK ordering yields the lowest half band width   ---
!
      IF( MHBC_IJK.LE.MHBC_JKI .AND. MHBC_IJK.LE.MHBC_KIJ ) THEN
        DO 200 NCW = 1,LN_CW
          ID_CW(7,NCW) = M_IJK(2,NCW)
  200   CONTINUE
!
!---    Equation ordering using I,J,K ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 210 K = 1,LFZ
        DO 210 J = 1,LFY
        DO 210 I = 1,LFX
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 210
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            IXP(N) = -(IRX*JRX*KRX)
            DO 202 KX = 1,KRX
            DO 202 JX = 1,JRX
            DO 202 IX = 1,IRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NC = NC + 1
              IXP(NX) = NC
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
!
!---    Coupled equations half band width using I,J,K ordering  ---
!
        DO 230 K = 1,LFZ
        DO 230 J = 1,LFY
        DO 230 I = 1,LFX
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
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
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NMD = IXP(NX)
              DO 212 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  212         CONTINUE
!
!---          Loop over coupled wells, placing well equation
!             in the principal node of the well  ---
!
              DO 214 NCW = 1,N_CW
                IF( IWF_CW(ID_CW(7,NCW)).EQ.NX ) THEN
                  NC = NC+1
                  JM_CW(NCW) = NC
                ENDIF
  214         CONTINUE
  216       CONTINUE
          ELSE
            NMD = IXP(N)
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
        DO 260 K = 1,LFZ
        DO 260 J = 1,LFY
        DO 260 I = 1,LFX
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 260
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 252 KX = 1,KRX
          DO 252 JX = 1,JRX
          DO 252 IX = 1,IRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NP = IXP(NX)
!
!---        Node  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
              IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                DO 241 MX = 1,4
                  NB = ICM(MX,1,NX)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
  241           CONTINUE
              ENDIF
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
              IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                DO 242 MX = 1,4
                  NS = ICM(MX,2,NX)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
  242           CONTINUE
              ENDIF
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
              IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                DO 243 MX = 1,4
                  NW = ICM(MX,3,NX)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
  243           CONTINUE
              ENDIF
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.LFX ) THEN
              IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                DO 244 MX = 1,4
                  NE = ICM(MX,4,NX)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
  244           CONTINUE
              ENDIF
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.LFY ) THEN
              IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                DO 245 MX = 1,4
                  NN = ICM(MX,5,NX)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &               ABS(IM(ISVC,NP)-IM(1,NN)))
  245           CONTINUE
              ENDIF
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.LFZ ) THEN
              IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                DO 246 MX = 1,4
                  NT = ICM(MX,6,NX)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
  246           CONTINUE
              ENDIF
            ENDIF
!
!---        Coupled well nodes  ---
!
            DO 250 NCW = 1,N_CW
              DO 248 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                IF( IWF_CW(NWF).EQ.NX ) THEN
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_CW(NCW)),
     &              ABS(IM(ISVC,NP)-JM_CW(NCW)))
                ENDIF
  248         CONTINUE
  250       CONTINUE
  252     CONTINUE
  260   CONTINUE
        LHBW = MHBC
!
!---  JKI ordering yields the lowest half band width   ---
!
      ELSEIF( MHBC_JKI.LE.MHBC_KIJ ) THEN
        DO 300 NCW = 1,LN_CW
          ID_CW(7,NCW) = M_JKI(2,NCW)
  300   CONTINUE
!
!---    Equation ordering using J,K,I ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 310 I = 1,LFX
        DO 310 K = 1,LFZ
        DO 310 J = 1,LFY
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 310
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            IXP(N) = -(IRX*JRX*KRX)
            DO 302 IX = 1,IRX
            DO 302 KX = 1,KRX
            DO 302 JX = 1,JRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NC = NC + 1
              IXP(NX) = NC
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
!
!---    Coupled equations half band width using J,K,I ordering  ---
!
        DO 330 I = 1,LFX
        DO 330 K = 1,LFZ
        DO 330 J = 1,LFY
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
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
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NMD = IXP(NX)
              DO 312 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  312         CONTINUE
!
!---          Loop over coupled wells, placing well equation
!             in the principal node of the well  ---
!
              DO 314 NCW = 1,N_CW
                IF( IWF_CW(ID_CW(7,NCW)).EQ.NX ) THEN
                  NC = NC+1
                  JM_CW(NCW) = NC
                ENDIF
  314         CONTINUE
  316       CONTINUE
          ELSE
            NMD = IXP(N)
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
        DO 360 I = 1,LFX
        DO 360 K = 1,LFZ
        DO 360 J = 1,LFY
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 360
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 352 KX = 1,KRX
          DO 352 JX = 1,JRX
          DO 352 IX = 1,IRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NP = IXP(NX)
!
!---        Node  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
              IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                DO 341 MX = 1,4
                  NB = ICM(MX,1,NX)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
  341           CONTINUE
              ENDIF
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
              IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                DO 342 MX = 1,4
                  NS = ICM(MX,2,NX)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
  342           CONTINUE
              ENDIF
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
              IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                DO 343 MX = 1,4
                  NW = ICM(MX,3,NX)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
  343           CONTINUE
              ENDIF
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.LFX ) THEN
              IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                DO 344 MX = 1,4
                  NE = ICM(MX,4,NX)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
  344           CONTINUE
              ENDIF
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.LFY ) THEN
              IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                DO 345 MX = 1,4
                  NN = ICM(MX,5,NX)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &               ABS(IM(ISVC,NP)-IM(1,NN)))
  345           CONTINUE
              ENDIF
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.LFZ ) THEN
              IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                DO 346 MX = 1,4
                  NT = ICM(MX,6,NX)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
  346           CONTINUE
              ENDIF
            ENDIF
!
!---        Coupled well nodes  ---
!
            DO 350 NCW = 1,N_CW
              DO 348 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                IF( IWF_CW(NWF).EQ.NX ) THEN
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_CW(NCW)),
     &              ABS(IM(ISVC,NP)-JM_CW(NCW)))
                ENDIF
  348         CONTINUE
  350       CONTINUE
  352     CONTINUE
  360   CONTINUE
        LHBW = MHBC
!
!---  KIJ ordering yields the lowest half band width   ---
!
      ELSE
        DO 400 NCW = 1,LN_CW
          ID_CW(7,NCW) = M_KIJ(2,NCW)
  400   CONTINUE
!
!---    Equation ordering using K,I,J ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 410 J = 1,LFY
        DO 410 I = 1,LFX
        DO 410 K = 1,LFZ
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 410
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            IXP(N) = -(IRX*JRX*KRX)
            DO 402 JX = 1,JRX
            DO 402 IX = 1,IRX
            DO 402 KX = 1,KRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NC = NC + 1
              IXP(NX) = NC
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
!
!---    Coupled equations half band width using K,I,J ordering  ---
!
        DO 430 J = 1,LFY
        DO 430 I = 1,LFX
        DO 430 K = 1,LFZ
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
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
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NMD = IXP(NX)
              DO 412 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  412         CONTINUE
!
!---          Loop over coupled wells, placing well equation
!             in the principal node of the well  ---
!
              DO 414 NCW = 1,N_CW
                IF( IWF_CW(ID_CW(7,NCW)).EQ.NX ) THEN
                  NC = NC+1
                  JM_CW(NCW) = NC
                ENDIF
  414         CONTINUE
  416       CONTINUE
          ELSE
            NMD = IXP(N)
            DO 418 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  418       CONTINUE
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
        DO 460 J = 1,LFY
        DO 460 I = 1,LFX
        DO 460 K = 1,LFZ
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 460
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 452 KX = 1,KRX
          DO 452 JX = 1,JRX
          DO 452 IX = 1,IRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NP = IXP(NX)
!
!---        Node  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
              IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                DO 441 MX = 1,4
                  NB = ICM(MX,1,NX)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
  441           CONTINUE
              ENDIF
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
              IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                DO 442 MX = 1,4
                  NS = ICM(MX,2,NX)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
  442           CONTINUE
              ENDIF
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
              IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                DO 443 MX = 1,4
                  NW = ICM(MX,3,NX)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
  443           CONTINUE
              ENDIF
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.LFX ) THEN
              IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                DO 444 MX = 1,4
                  NE = ICM(MX,4,NX)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
  444           CONTINUE
              ENDIF
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.LFY ) THEN
              IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                DO 445 MX = 1,4
                  NN = ICM(MX,5,NX)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &               ABS(IM(ISVC,NP)-IM(1,NN)))
  445           CONTINUE
              ENDIF
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.LFZ ) THEN
              IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                DO 446 MX = 1,4
                  NT = ICM(MX,6,NX)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
  446           CONTINUE
              ENDIF
            ENDIF
!
!---        Coupled well nodes  ---
!
            DO 450 NCW = 1,N_CW
              DO 440 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                IF( IWF_CW(NWF).EQ.NX ) THEN
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_CW(NCW)),
     &              ABS(IM(ISVC,NP)-JM_CW(NCW)))
                ENDIF
  440         CONTINUE
  450       CONTINUE
  452     CONTINUE
  460   CONTINUE
      ENDIF
      LHBW = MHBC

!
!---  Deallocate memory for the IJK half-band width array  ---
!
      IF( ALLOCATED(M_IJK) ) THEN
        DEALLOCATE( M_IJK,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: M_IJK'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the JKI half-band width array  ---
!
      IF( ALLOCATED(M_JKI) ) THEN
        DEALLOCATE( M_JKI,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: M_JKI'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the KIJ half-band width array  ---
!
      IF( ALLOCATED(M_KIJ) ) THEN
        DEALLOCATE( M_KIJ,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: M_KIJ'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCB_CW group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCB_CW_DP
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
!     Determine the matrix half band width for the coupled
!     well model and dual porosity
!
!----------------------Authors-----------------------------------------!
!
!     Written by M.D. White, PNNL, 24 November 2015.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE JACOB
      USE GRID
      USE DUAL_POR
      USE COUP_WELL
      USE CONST
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: M_IJK,M_JKI,M_KIJ
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/JCB_CW_DP'

!
!---  Allocate memory for Jacobian matrix equation index  ---
!
      IF( .NOT.ALLOCATED(IM) ) THEN
        ALLOCATE( IM(1:LUK,1:LFD+LBR),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: IM'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Allocate memory for the matrix dual-porosity Jacobian matrix 
!     pointer array  ---
!
      IF( .NOT.ALLOCATED(IM_M) ) THEN
        LFD_DP = LFD**L_DP
        ALLOCATE( IM_M(1:LUK,1:LFD_DP),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: IM_M'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Allocate memory for coupled-well Jacobian index array  ---
!
      IF( .NOT.ALLOCATED(JM_CW) ) THEN
        ALLOCATE( JM_CW(1:LN_CW),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: JM_CW'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Allocate memory for half-band width count array 
!     for IJK index ordering  ---
!
      IF( .NOT.ALLOCATED(M_IJK) ) THEN
        ALLOCATE( M_IJK(1:2,1:LN_CW),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: M_IJK'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Allocate memory for half-band width count array 
!     for JKI index ordering  ---
!
      IF( .NOT.ALLOCATED(M_JKI) ) THEN
        ALLOCATE( M_JKI(1:2,1:LN_CW),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: M_JKI'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Allocate memory for half-band width count array 
!     for KIJ index ordering  ---
!
      IF( .NOT.ALLOCATED(M_KIJ) ) THEN
        ALLOCATE( M_KIJ(1:2,1:LN_CW),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: M_KIJ'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Initialize number of unknowns  ---
!
      ISVC = LUK
      ILIMIT = 2**30
!
!---  Coupled equations half band width using I,J,K ordering  ---
!
      NC = 0
      DO 10 K = 1,LFZ
      DO 10 J = 1,LFY
      DO 10 I = 1,LFX
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 10
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          IXP(N) = -(IRX*JRX*KRX)
          DO 2 KX = 1,KRX
          DO 2 JX = 1,JRX
          DO 2 IX = 1,IRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NC = NC + 1
            IXP(NX) = NC
    2     CONTINUE
        ELSE
          NC = NC + 1
          IXP(N) = NC
        ENDIF
   10 CONTINUE
      DO 60 NCW = 1,LN_CW
        M_IJK(1,NCW) = ILIMIT
        DO 50 IDCW = ID_CW(5,NCW),ID_CW(6,NCW)
          NC = 0
          NWF = IWF_CW(IDCW)
          MHBC_IJK = 0
          DO 20 K = 1,LFZ
          DO 20 J = 1,LFY
          DO 20 I = 1,LFX
            N = ND(I,J,K)
!
!---         Loop over block refined nodes  ---
!
            IF( IBR(4,N).GT.N ) THEN
              IRX = 2**IBR(1,N)
              JRX = 2**IBR(2,N)
              KRX = 2**IBR(3,N)
              DO 14 KX = 1,KRX
              DO 14 JX = 1,JRX
              DO 14 IX = 1,IRX
                NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
                NMD = IXP(NX)
                DO 12 M = 1,ISVC
                  NC = NC+1
                  IM(M,NMD) = NC
   12           CONTINUE
                DO 13 M = 1,ISVC
                  NC = NC+1
                  IM_M(M,NMD) = NC
   13           CONTINUE
                IF( NWF.EQ.NX ) THEN
                  NC = NC+1
                  JM_CWX = NC
                ENDIF
   14         CONTINUE
            ELSE
              NMD = IXP(N)
!
!---          Skip to next active node  ---
!
              IF( IXP(N).EQ.0 ) GOTO 20
              DO 16 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
   16         CONTINUE
              DO 17 M = 1,ISVC
                NC = NC+1
                IM_M(M,NMD) = NC
   17         CONTINUE
              IF( NWF.EQ.N ) THEN
                NC = NC+1
                JM_CWX = NC
              ENDIF
            ENDIF
   20     CONTINUE
!
!---      Determine matrix half-band widths, using I,J,K ordering  ---
!
          DO 40 K = 1,LFZ
          DO 40 J = 1,LFY
          DO 40 I = 1,LFX
            N = ND(I,J,K)
!
!---        Skip to next active node  ---
!
            IF( IXP(N).EQ.0 ) GOTO 40
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 32 KX = 1,KRX
            DO 32 JX = 1,JRX
            DO 32 IX = 1,IRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NP = IXP(NX)
!
!---          Node  ---
!
              MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---          Fracture to matrix connections  ---
!
              MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM_M(ISVC,NP)),
     &          ABS(IM(ISVC,NP)-IM_M(1,NP)))
!
!---          Bottom node neighbors  ---
!
              IF( K.GT.1 ) THEN
                IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                  DO 21 MX = 1,4
                    NB = ICM(MX,1,NX)
                    IF( NB.EQ.0 ) EXIT
                    NB = IXP(NB)
                    MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NB)),
     &                ABS(IM(ISVC,NP)-IM(1,NB)))
   21             CONTINUE
                ENDIF
              ENDIF
!
!---          South node neighbors  ---
!
              IF( J.GT.1 ) THEN
                IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                  DO 22 MX = 1,4
                    NS = ICM(MX,2,NX)
                    IF( NS.EQ.0 ) EXIT
                    NS = IXP(NS)
                    MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NS)),
     &                ABS(IM(ISVC,NP)-IM(1,NS)))
   22             CONTINUE
                ENDIF
              ENDIF
!
!---          West node neighbors  ---
!
              IF( I.GT.1 ) THEN
                IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                  DO 23 MX = 1,4
                    NW = ICM(MX,3,NX)
                    IF( NW.EQ.0 ) EXIT
                    NW = IXP(NW)
                    MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NW)),
     &                ABS(IM(ISVC,NP)-IM(1,NW)))
   23             CONTINUE
               ENDIF
              ENDIF
!
!---          East node neighbors  ---
!
              IF( I.LT.LFX ) THEN
                IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                  DO 24 MX = 1,4
                    NE = ICM(MX,4,NX)
                    IF( NE.EQ.0 ) EXIT
                    NE = IXP(NE)
                    MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NE)),
     &                ABS(IM(ISVC,NP)-IM(1,NE)))
   24             CONTINUE
                ENDIF
              ENDIF
!
!---          North node neighbors  ---
!
              IF( J.LT.LFY ) THEN
                IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                  DO 25 MX = 1,4
                    NN = ICM(MX,5,NX)
                    IF( NN.EQ.0 ) EXIT
                    NN = IXP(NN)
                    MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NN)),
     &                ABS(IM(ISVC,NP)-IM(1,NN)))
   25             CONTINUE
                ENDIF
              ENDIF
!
!---          Top node neighbors  ---
!
              IF( K.LT.LFZ ) THEN
                IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                  DO 26 MX = 1,4
                    NT = ICM(MX,6,NX)
                    IF( NT.EQ.0 ) EXIT
                    NT = IXP(NT)
                    MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NT)),
     &                ABS(IM(ISVC,NP)-IM(1,NT)))
   26             CONTINUE
                ENDIF
              ENDIF
!
!---          Coupled well nodes  ---
!
              DO 30 JDCW = ID_CW(5,NCW),ID_CW(6,NCW)
                IF( IWF_CW(JDCW).EQ.NX ) THEN
                  MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-JM_CWX),
     &              ABS(IM(ISVC,NP)-JM_CWX))
                ENDIF
   30         CONTINUE
   32       CONTINUE
   40     CONTINUE
          IF( MHBC_IJK.LT.M_IJK(1,NCW) ) THEN
            M_IJK(1,NCW) = MHBC_IJK
            M_IJK(2,NCW) = IDCW
          ENDIF
   50   CONTINUE
   60 CONTINUE
!
!---  Coupled equations half band width using J,K,I ordering  ---
!
      NC = 0
      DO 70 I = 1,LFX
      DO 70 K = 1,LFZ
      DO 70 J = 1,LFY
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 70
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          IXP(N) = -(IRX*JRX*KRX)
          DO 62 IX = 1,IRX
          DO 62 KX = 1,KRX
          DO 62 JX = 1,JRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NC = NC + 1
            IXP(NX) = NC
   62     CONTINUE
        ELSE
          NC = NC + 1
          IXP(N) = NC
        ENDIF
   70 CONTINUE
      DO 120 NCW = 1,LN_CW
        M_JKI(1,NCW) = ILIMIT
        DO 110 IDCW = ID_CW(5,NCW),ID_CW(6,NCW)
          NC = 0
          NWF = IWF_CW(IDCW)
          MHBC_JKI = 0
          DO 80 I = 1,LFX
          DO 80 K = 1,LFZ
          DO 80 J = 1,LFY
            N = ND(I,J,K)
!
!---         Loop over block refined nodes  ---
!
            IF( IBR(4,N).GT.N ) THEN
              IRX = 2**IBR(1,N)
              JRX = 2**IBR(2,N)
              KRX = 2**IBR(3,N)
              DO 74 IX = 1,IRX
              DO 74 KX = 1,KRX
              DO 74 JX = 1,JRX
                NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
                NMD = IXP(NX)
                DO 72 M = 1,ISVC
                  NC = NC+1
                  IM(M,NMD) = NC
   72           CONTINUE
                DO 73 M = 1,ISVC
                  NC = NC+1
                  IM_M(M,NMD) = NC
   73           CONTINUE
                IF( NWF.EQ.NX ) THEN
                  NC = NC+1
                  JM_CWX = NC
                ENDIF
   74         CONTINUE
            ELSE
              NMD = IXP(N)
!
!---          Skip to next active node  ---
!
              IF( IXP(N).EQ.0 ) GOTO 80
              DO 76 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
   76         CONTINUE
              DO 77 M = 1,ISVC
                NC = NC+1
                IM_M(M,NMD) = NC
   77         CONTINUE
              IF( NWF.EQ.N ) THEN
                NC = NC+1
                JM_CWX = NC
              ENDIF
            ENDIF
   80     CONTINUE
!
!---      Determine matrix half-band widths, using J,K,I ordering  ---
!
          DO 100 I = 1,LFX
          DO 100 K = 1,LFZ
          DO 100 J = 1,LFY
            N = ND(I,J,K)
!
!---        Skip to next active node  ---
!
            IF( IXP(N).EQ.0 ) GOTO 100
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 92 IX = 1,IRX
            DO 92 KX = 1,KRX
            DO 92 JX = 1,JRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NP = IXP(NX)
!
!---          Node  ---
!
              MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---          Fracture to matrix connections  ---
!
              MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM_M(ISVC,NP)),
     &          ABS(IM(ISVC,NP)-IM_M(1,NP)))
!
!---          Bottom node neighbors  ---
!
              IF( K.GT.1 ) THEN
                IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                  DO 81 MX = 1,4
                    NB = ICM(MX,1,NX)
                    IF( NB.EQ.0 ) EXIT
                    NB = IXP(NB)
                    MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NB)),
     &                ABS(IM(ISVC,NP)-IM(1,NB)))
   81             CONTINUE
                ENDIF
              ENDIF
!
!---          South node neighbors  ---
!
              IF( J.GT.1 ) THEN
                IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                  DO 82 MX = 1,4
                    NS = ICM(MX,2,NX)
                    IF( NS.EQ.0 ) EXIT
                    NS = IXP(NS)
                    MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NS)),
     &                ABS(IM(ISVC,NP)-IM(1,NS)))
   82             CONTINUE
                ENDIF
              ENDIF
!
!---          West node neighbors  ---
!
              IF( I.GT.1 ) THEN
                IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                  DO 83 MX = 1,4
                    NW = ICM(MX,3,NX)
                    IF( NW.EQ.0 ) EXIT
                    NW = IXP(NW)
                    MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NW)),
     &                ABS(IM(ISVC,NP)-IM(1,NW)))
   83             CONTINUE
               ENDIF
              ENDIF
!
!---          East node neighbors  ---
!
              IF( I.LT.LFX ) THEN
                IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                  DO 84 MX = 1,4
                    NE = ICM(MX,4,NX)
                    IF( NE.EQ.0 ) EXIT
                    NE = IXP(NE)
                    MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NE)),
     &                ABS(IM(ISVC,NP)-IM(1,NE)))
   84             CONTINUE
                ENDIF
              ENDIF
!
!---          North node neighbors  ---
!
              IF( J.LT.LFY ) THEN
                IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                  DO 85 MX = 1,4
                    NN = ICM(MX,5,NX)
                    IF( NN.EQ.0 ) EXIT
                    NN = IXP(NN)
                    MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NN)),
     &                ABS(IM(ISVC,NP)-IM(1,NN)))
   85             CONTINUE
                ENDIF
              ENDIF
!
!---          Top node neighbors  ---
!
              IF( K.LT.LFZ ) THEN
                IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                  DO 86 MX = 1,4
                    NT = ICM(MX,6,NX)
                    IF( NT.EQ.0 ) EXIT
                    NT = IXP(NT)
                    MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NT)),
     &                ABS(IM(ISVC,NP)-IM(1,NT)))
   86             CONTINUE
                ENDIF
              ENDIF
!
!---          Coupled well nodes  ---
!
              DO 90 JDCW = ID_CW(5,NCW),ID_CW(6,NCW)
                IF( IWF_CW(JDCW).EQ.NX ) THEN
                  MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-JM_CWX),
     &              ABS(IM(ISVC,NP)-JM_CWX))
                ENDIF
   90         CONTINUE
   92       CONTINUE
  100     CONTINUE
          IF( MHBC_JKI.LT.M_JKI(1,NCW) ) THEN
            M_JKI(1,NCW) = MHBC_JKI
            M_JKI(2,NCW) = IDCW
          ENDIF
  110   CONTINUE
  120 CONTINUE
!
!---  Coupled equations half band width using K,I,J ordering  ---
!
      NC = 0
      DO 130 J = 1,LFY
      DO 130 I = 1,LFX
      DO 130 K = 1,LFZ
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 130
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          IXP(N) = -(IRX*JRX*KRX)
          DO 122 JX = 1,JRX
          DO 122 IX = 1,IRX
          DO 122 KX = 1,KRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NC = NC + 1
            IXP(NX) = NC
  122     CONTINUE
        ELSE
          NC = NC + 1
          IXP(N) = NC
        ENDIF
  130 CONTINUE
!
!---  Coupled equations half band width using K,I,J ordering  ---
!
      DO 180 NCW = 1,LN_CW
        M_KIJ(1,NCW) = ILIMIT
        DO 170 IDCW = ID_CW(5,NCW),ID_CW(6,NCW)
          NC = 0
          NWF = IWF_CW(IDCW)
          MHBC_KIJ = 0
          DO 140 J = 1,LFY
          DO 140 I = 1,LFX
          DO 140 K = 1,LFZ
            N = ND(I,J,K)
!
!---         Loop over block refined nodes  ---
!
            IF( IBR(4,N).GT.N ) THEN
              IRX = 2**IBR(1,N)
              JRX = 2**IBR(2,N)
              KRX = 2**IBR(3,N)
              DO 134 JX = 1,JRX
              DO 134 IX = 1,IRX
              DO 134 KX = 1,KRX
                NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
                NMD = IXP(NX)
                DO 132 M = 1,ISVC
                  NC = NC+1
                  IM(M,NMD) = NC
  132           CONTINUE
                DO 133 M = 1,ISVC
                  NC = NC+1
                  IM_M(M,NMD) = NC
  133           CONTINUE
                IF( NWF.EQ.NX ) THEN
                  NC = NC+1
                  JM_CWX = NC
                ENDIF
  134         CONTINUE
            ELSE
              NMD = IXP(N)
!
!---          Skip to next active node  ---
!
              IF( IXP(N).EQ.0 ) GOTO 140
              DO 136 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  136         CONTINUE
              DO 137 M = 1,ISVC
                NC = NC+1
                IM_M(M,NMD) = NC
  137         CONTINUE
              IF( NWF.EQ.N ) THEN
                NC = NC+1
                JM_CWX = NC
              ENDIF
            ENDIF
  140     CONTINUE
!
!---      Determine matrix half-band widths, using K,I,J ordering  ---
!
          DO 160 J = 1,LFY
          DO 160 I = 1,LFX
          DO 160 K = 1,LFZ
            N = ND(I,J,K)
!
!---        Skip to next active node  ---
!
            IF( IXP(N).EQ.0 ) GOTO 160
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 152 IX = 1,IRX
            DO 152 KX = 1,KRX
            DO 152 JX = 1,JRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NP = IXP(NX)
!
!---          Node  ---
!
              MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---          Node  ---
!
              MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---          Bottom node neighbors  ---
!
              IF( K.GT.1 ) THEN
                IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                  DO 141 MX = 1,4
                    NB = ICM(MX,1,NX)
                    IF( NB.EQ.0 ) EXIT
                    NB = IXP(NB)
                    MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NB)),
     &                ABS(IM(ISVC,NP)-IM(1,NB)))
  141             CONTINUE
                ENDIF
              ENDIF
!
!---          South node neighbors  ---
!
              IF( J.GT.1 ) THEN
                IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                  DO 142 MX = 1,4
                    NS = ICM(MX,2,NX)
                    IF( NS.EQ.0 ) EXIT
                    NS = IXP(NS)
                    MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NS)),
     &                ABS(IM(ISVC,NP)-IM(1,NS)))
  142             CONTINUE
                ENDIF
              ENDIF
!
!---          West node neighbors  ---
!
              IF( I.GT.1 ) THEN
                IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                  DO 143 MX = 1,4
                    NW = ICM(MX,3,NX)
                    IF( NW.EQ.0 ) EXIT
                    NW = IXP(NW)
                    MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NW)),
     &                ABS(IM(ISVC,NP)-IM(1,NW)))
  143             CONTINUE
               ENDIF
              ENDIF
!
!---          East node neighbors  ---
!
              IF( I.LT.LFX ) THEN
                IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                  DO 144 MX = 1,4
                    NE = ICM(MX,4,NX)
                    IF( NE.EQ.0 ) EXIT
                    NE = IXP(NE)
                    MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NE)),
     &                ABS(IM(ISVC,NP)-IM(1,NE)))
  144             CONTINUE
                ENDIF
              ENDIF
!
!---          North node neighbors  ---
!
              IF( J.LT.LFY ) THEN
                IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                  DO 145 MX = 1,4
                    NN = ICM(MX,5,NX)
                    IF( NN.EQ.0 ) EXIT
                    NN = IXP(NN)
                    MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NN)),
     &                ABS(IM(ISVC,NP)-IM(1,NN)))
  145             CONTINUE
                ENDIF
              ENDIF
!
!---          Top node neighbors  ---
!
              IF( K.LT.LFZ ) THEN
                IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                  DO 146 MX = 1,4
                    NT = ICM(MX,6,NX)
                    IF( NT.EQ.0 ) EXIT
                    NT = IXP(NT)
                    MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NT)),
     &                ABS(IM(ISVC,NP)-IM(1,NT)))
  146             CONTINUE
                ENDIF
              ENDIF
!
!---          Coupled well nodes  ---
!
              DO 150 JDCW = ID_CW(5,NCW),ID_CW(6,NCW)
                IF( IWF_CW(JDCW).EQ.NX ) THEN
                  MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-JM_CWX),
     &              ABS(IM(ISVC,NP)-JM_CWX))
                ENDIF
  150         CONTINUE
  152       CONTINUE
  160     CONTINUE
          IF( MHBC_KIJ.LT.M_KIJ(1,NCW) ) THEN
            M_KIJ(1,NCW) = MHBC_KIJ
            M_KIJ(2,NCW) = IDCW
          ENDIF
  170   CONTINUE
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
!---  IJK ordering yields the lowest half band width   ---
!
      IF( MHBC_IJK.LE.MHBC_JKI .AND. MHBC_IJK.LE.MHBC_KIJ ) THEN
        DO 200 NCW = 1,LN_CW
          ID_CW(7,NCW) = M_IJK(2,NCW)
  200   CONTINUE
!
!---    Equation ordering using I,J,K ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 210 K = 1,LFZ
        DO 210 J = 1,LFY
        DO 210 I = 1,LFX
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 210
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            IXP(N) = -(IRX*JRX*KRX)
            DO 202 KX = 1,KRX
            DO 202 JX = 1,JRX
            DO 202 IX = 1,IRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NC = NC + 1
              IXP(NX) = NC
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
!
!---    Coupled equations half band width using I,J,K ordering  ---
!
        DO 230 K = 1,LFZ
        DO 230 J = 1,LFY
        DO 230 I = 1,LFX
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
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
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NMD = IXP(NX)
              DO 212 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  212         CONTINUE
              DO 213 M = 1,ISVC
                NC = NC+1
                IM_M(M,NMD) = NC
  213         CONTINUE
!
!---          Loop over coupled wells, placing well equation
!             in the principal node of the well  ---
!
              DO 214 NCW = 1,N_CW
                IF( IWF_CW(ID_CW(7,NCW)).EQ.NX ) THEN
                  NC = NC+1
                  JM_CW(NCW) = NC
                ENDIF
  214         CONTINUE
  216       CONTINUE
          ELSE
            NMD = IXP(N)
            DO 218 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  218       CONTINUE
            DO 219 M = 1,ISVC
              NC = NC+1
              IM_M(M,NMD) = NC
  219       CONTINUE
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
        DO 260 K = 1,LFZ
        DO 260 J = 1,LFY
        DO 260 I = 1,LFX
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 260
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 252 KX = 1,KRX
          DO 252 JX = 1,JRX
          DO 252 IX = 1,IRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NP = IXP(NX)
!
!---        Node  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Fracture to matrix connections  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM_M(ISVC,NP)),
     &        ABS(IM(ISVC,NP)-IM_M(1,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
              IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                DO 241 MX = 1,4
                  NB = ICM(MX,1,NX)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
  241           CONTINUE
              ENDIF
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
              IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                DO 242 MX = 1,4
                  NS = ICM(MX,2,NX)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
  242           CONTINUE
              ENDIF
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
              IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                DO 243 MX = 1,4
                  NW = ICM(MX,3,NX)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
  243           CONTINUE
              ENDIF
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.LFX ) THEN
              IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                DO 244 MX = 1,4
                  NE = ICM(MX,4,NX)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
  244           CONTINUE
              ENDIF
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.LFY ) THEN
              IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                DO 245 MX = 1,4
                  NN = ICM(MX,5,NX)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &               ABS(IM(ISVC,NP)-IM(1,NN)))
  245           CONTINUE
              ENDIF
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.LFZ ) THEN
              IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                DO 246 MX = 1,4
                  NT = ICM(MX,6,NX)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
  246           CONTINUE
              ENDIF
            ENDIF
!
!---        Coupled well nodes  ---
!
            DO 250 NCW = 1,N_CW
              DO 248 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                IF( IWF_CW(NWF).EQ.NX ) THEN
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_CW(NCW)),
     &              ABS(IM(ISVC,NP)-JM_CW(NCW)))
                ENDIF
  248         CONTINUE
  250       CONTINUE
  252     CONTINUE
  260   CONTINUE
        LHBW = MHBC
!
!---  JKI ordering yields the lowest half band width   ---
!
      ELSEIF( MHBC_JKI.LE.MHBC_KIJ ) THEN
        DO 300 NCW = 1,LN_CW
          ID_CW(7,NCW) = M_JKI(2,NCW)
  300   CONTINUE
!
!---    Equation ordering using J,K,I ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 310 I = 1,LFX
        DO 310 K = 1,LFZ
        DO 310 J = 1,LFY
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 310
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            IXP(N) = -(IRX*JRX*KRX)
            DO 302 IX = 1,IRX
            DO 302 KX = 1,KRX
            DO 302 JX = 1,JRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NC = NC + 1
              IXP(NX) = NC
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
!
!---    Coupled equations half band width using J,K,I ordering  ---
!
        DO 330 I = 1,LFX
        DO 330 K = 1,LFZ
        DO 330 J = 1,LFY
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
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
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NMD = IXP(NX)
              DO 312 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  312         CONTINUE
              DO 313 M = 1,ISVC
                NC = NC + 1
                IM_M(M,NMD) = NC
  313         CONTINUE
!
!---          Loop over coupled wells, placing well equation
!             in the principal node of the well  ---
!
              DO 314 NCW = 1,N_CW
                IF( IWF_CW(ID_CW(7,NCW)).EQ.NX ) THEN
                  NC = NC+1
                  JM_CW(NCW) = NC
                ENDIF
  314         CONTINUE
  316       CONTINUE
          ELSE
            NMD = IXP(N)
            DO 318 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  318       CONTINUE
            DO 319 M = 1,ISVC
              NC = NC + 1
              IM_M(M,NMD) = NC
  319       CONTINUE
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
        DO 360 I = 1,LFX
        DO 360 K = 1,LFZ
        DO 360 J = 1,LFY
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 360
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 352 KX = 1,KRX
          DO 352 JX = 1,JRX
          DO 352 IX = 1,IRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NP = IXP(NX)
!
!---        Node  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Fracture to matrix connections  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM_M(ISVC,NP)),
     &        ABS(IM(ISVC,NP)-IM_M(1,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
              IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                DO 341 MX = 1,4
                  NB = ICM(MX,1,NX)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
  341           CONTINUE
              ENDIF
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
              IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                DO 342 MX = 1,4
                  NS = ICM(MX,2,NX)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
  342           CONTINUE
              ENDIF
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
              IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                DO 343 MX = 1,4
                  NW = ICM(MX,3,NX)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
  343           CONTINUE
              ENDIF
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.LFX ) THEN
              IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                DO 344 MX = 1,4
                  NE = ICM(MX,4,NX)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
  344           CONTINUE
              ENDIF
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.LFY ) THEN
              IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                DO 345 MX = 1,4
                  NN = ICM(MX,5,NX)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &               ABS(IM(ISVC,NP)-IM(1,NN)))
  345           CONTINUE
              ENDIF
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.LFZ ) THEN
              IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                DO 346 MX = 1,4
                  NT = ICM(MX,6,NX)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
  346           CONTINUE
              ENDIF
            ENDIF
!
!---        Coupled well nodes  ---
!
            DO 350 NCW = 1,N_CW
              DO 348 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                IF( IWF_CW(NWF).EQ.NX ) THEN
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_CW(NCW)),
     &              ABS(IM(ISVC,NP)-JM_CW(NCW)))
                ENDIF
  348         CONTINUE
  350       CONTINUE
  352     CONTINUE
  360   CONTINUE
        LHBW = MHBC
!
!---  KIJ ordering yields the lowest half band width   ---
!
      ELSE
        DO 400 NCW = 1,LN_CW
          ID_CW(7,NCW) = M_KIJ(2,NCW)
  400   CONTINUE
!
!---    Equation ordering using K,I,J ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 410 J = 1,LFY
        DO 410 I = 1,LFX
        DO 410 K = 1,LFZ
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 410
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            IXP(N) = -(IRX*JRX*KRX)
            DO 402 JX = 1,JRX
            DO 402 IX = 1,IRX
            DO 402 KX = 1,KRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NC = NC + 1
              IXP(NX) = NC
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
!
!---    Coupled equations half band width using K,I,J ordering  ---
!
        DO 430 J = 1,LFY
        DO 430 I = 1,LFX
        DO 430 K = 1,LFZ
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
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
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NMD = IXP(NX)
              DO 412 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  412         CONTINUE
              DO 413 M = 1,ISVC
                NC = NC+1
                IM_M(M,NMD) = NC
  413         CONTINUE
!
!---          Loop over coupled wells, placing well equation
!             in the principal node of the well  ---
!
              DO 414 NCW = 1,N_CW
                IF( IWF_CW(ID_CW(7,NCW)).EQ.NX ) THEN
                  NC = NC+1
                  JM_CW(NCW) = NC
                ENDIF
  414         CONTINUE
  416       CONTINUE
          ELSE
            NMD = IXP(N)
            DO 418 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  418       CONTINUE
            DO 419 M = 1,ISVC
              NC = NC+1
              IM_M(M,NMD) = NC
  419       CONTINUE
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
        DO 460 J = 1,LFY
        DO 460 I = 1,LFX
        DO 460 K = 1,LFZ
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 460
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 452 KX = 1,KRX
          DO 452 JX = 1,JRX
          DO 452 IX = 1,IRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NP = IXP(NX)
!
!---        Node  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Fracture to matrix connections  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM_M(ISVC,NP)),
     &        ABS(IM(ISVC,NP)-IM_M(1,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
              IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                DO 441 MX = 1,4
                  NB = ICM(MX,1,NX)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
  441           CONTINUE
              ENDIF
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
              IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                DO 442 MX = 1,4
                  NS = ICM(MX,2,NX)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
  442           CONTINUE
              ENDIF
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
              IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                DO 443 MX = 1,4
                  NW = ICM(MX,3,NX)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
  443           CONTINUE
              ENDIF
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.LFX ) THEN
              IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                DO 444 MX = 1,4
                  NE = ICM(MX,4,NX)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
  444           CONTINUE
              ENDIF
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.LFY ) THEN
              IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                DO 445 MX = 1,4
                  NN = ICM(MX,5,NX)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &               ABS(IM(ISVC,NP)-IM(1,NN)))
  445           CONTINUE
              ENDIF
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.LFZ ) THEN
              IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                DO 446 MX = 1,4
                  NT = ICM(MX,6,NX)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
  446           CONTINUE
              ENDIF
            ENDIF
!
!---        Coupled well nodes  ---
!
            DO 450 NCW = 1,N_CW
              DO 440 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
                IF( IWF_CW(NWF).EQ.NX ) THEN
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_CW(NCW)),
     &              ABS(IM(ISVC,NP)-JM_CW(NCW)))
                ENDIF
  440         CONTINUE
  450       CONTINUE
  452     CONTINUE
  460   CONTINUE
      ENDIF
      LHBW = MHBC

!
!---  Deallocate memory for the IJK half-band width array  ---
!
      IF( ALLOCATED(M_IJK) ) THEN
        DEALLOCATE( M_IJK,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: M_IJK'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the JKI half-band width array  ---
!
      IF( ALLOCATED(M_JKI) ) THEN
        DEALLOCATE( M_JKI,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: M_JKI'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Deallocate memory for the KIJ half-band width array  ---
!
      IF( ALLOCATED(M_KIJ) ) THEN
        DEALLOCATE( M_KIJ,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: M_KIJ'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCB_CW_DP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCB_GM
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
!     Determine the matrix half band width for geomechanics
!
!----------------------Authors-----------------------------------------!
!
!     Written by M.D. White, PNNL, 22 November 2016.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PTZR
      USE JACOB
      USE GRID
      USE GEOMECH
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER ISTCX(27)
      INTEGER NKX(8,8)
!
!----------------------Data Statements---------------------------------!
!
      DATA NKX /14,13,11,10,5,4,2,1,
     &          15,14,12,11,6,5,3,2,
     &          17,16,14,13,8,7,5,4,
     &          18,17,15,14,9,8,6,5,
     &          23,22,20,19,14,13,11,10,
     &          24,23,21,20,15,14,12,11,
     &          26,25,23,22,17,16,14,13,
     &          27,26,24,23,18,17,15,14 /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/JCB_GM'
!
!---  Allocate memory for Jacobian matrix equation index  ---
!
      IF( .NOT.ALLOCATED(IM_GM) ) THEN
        ALLOCATE( IM_GM(1:LFEN),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: IM_GM'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Geomechanics equations half band width  ---
!
      MHB_GM = 0
      NC = 0
!
!---  Loop over number of finite-element nodes to load
!     the Jacobian matrix pointer array  ---
!
      DO NFEN = 1,LFEN
!
!---    Finite-element node is inactive, only if all elements
!       that contain the FE node are inactive grid cells  ---
!
        IFX = 0
        DO M = 1,8
          N = NE_GM(M,NFEN)
          IF( N.EQ.0 ) CYCLE
          IF( IXP(N).NE.0 ) THEN
            IFX = 1
            EXIT
          ENDIF
        ENDDO
        IF( IFX.EQ.1 ) THEN
          NC = NC + 1
          IM_GM(NFEN) = NC
        ENDIF
      ENDDO
!
!---  Loop over number of finite-element nodes to compute the minimum
!     half band width  ---
!
      DO NFEN = 1,LFEN
!
!---    Loop over the connections to adjacent finite elements  ---
!
        DO M1 = 1,8
          N = NE_GM(M1,NFEN)
          IF( N.EQ.0 ) CYCLE
!
!---      Loop over the FE nodes in the adjacent finite element  ---
!
          DO M2 = 1,8
            MFEN = ND_GM(M2,N)
            IF( MFEN.EQ.0 ) CYCLE
!
!---        Compute differences in displacement equations for FE nodes
!           connected through adjacent finite elements  ---
!
            IM1X = (IM_GM(NFEN)-1)*3 + 1
            IM2X = (IM_GM(MFEN)-1)*3 + 3
            IM3X = (IM_GM(NFEN)-1)*3 + 3
            IM4X = (IM_GM(MFEN)-1)*3 + 1
            MHB_GM = MAX( MHB_GM,ABS(IM1X-IM2X),ABS(IM3X-IM4X) )
          ENDDO
        ENDDO
      ENDDO

      LHBW = MAX( MHB_GM,LHBW )

!
!---  SPLIB .or. Lis .or. PETSc Solver  ---
!
      NC = 0
!
!---  Loop over number of finite-element nodes  ---
!
      DO NFEN = 1,LFEN
!
!---    Determine the connection stencil for the finite-element
!       node  ---
!
        DO M1 = 1,27
          ISTCX(M1) = 0
        ENDDO
!
!---    Loop over the connections to adjacent finite elements  ---
!
        DO M2 = 1,8
          N = NE_GM(M2,NFEN)
          IF( N.EQ.0 ) CYCLE
!
!---      Loop over the FE nodes in the adjacent finite element  ---
!
          DO M3 = 1,8
            MFEN = ND_GM(M3,N)
            IF( MFEN.EQ.0 ) CYCLE
            ISTCX(NKX(M2,M3)) = MFEN
          ENDDO
        ENDDO
!
!---    Loop over displacement equations  ---
!
        DO M4 = 1,3
          NMD = (IM_GM(NFEN)-1)*3 + M4
!
!---      Loop over the active connections  ---
!
          DO M5 = 1,27
            IF( ISTCX(M5).EQ.0 ) CYCLE
            MFEN = ISTCX(M5)
!
!---        Loop over displacement equations  ---
!
            DO M6 = 1,3
              NC = NC + 1
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      LJO_GM = MAX( NC,LJO_GM )
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCB_GM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCB_NCW
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
!     Determine the matrix half band width for no coupled
!     well model
!
!----------------------Authors-----------------------------------------!
!
!     Written by M.D. White, PNNL, 03 February 2014.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE JACOB
      USE GRID
      USE COUP_WELL
      USE CONST
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/JCB_NCW'

!
!---  Allocate memory for Jacobian matrix equation index  ---
!
      IF( .NOT.ALLOCATED(IM) ) THEN
        ALLOCATE( IM(1:LUK,1:LFD+LBR),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: IM'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Initialize number of unknowns  ---
!
      ISVC = LUK
      ILIMIT = 2**30
!
!---  Coupled equations half band width using I,J,K ordering  ---
!
      NC = 0
      DO 10 K = 1,LFZ
      DO 10 J = 1,LFY
      DO 10 I = 1,LFX
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 10
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          IXP(N) = -(IRX*JRX*KRX)
          DO 2 KX = 1,KRX
          DO 2 JX = 1,JRX
          DO 2 IX = 1,IRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NC = NC + 1
            IXP(NX) = NC
    2     CONTINUE
        ELSE
          NC = NC + 1
          IXP(N) = NC
        ENDIF
   10 CONTINUE
      NC = 0
      MHBC_IJK = 0
      DO 20 K = 1,LFZ
      DO 20 J = 1,LFY
      DO 20 I = 1,LFX
        N = ND(I,J,K)
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
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NMD = IXP(NX)
            DO 12 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
   12       CONTINUE
   14     CONTINUE
        ELSE
          NMD = IXP(N)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 20
          DO 16 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
   16     CONTINUE
        ENDIF
   20 CONTINUE
!
!---  Determine matrix half-band widths, using I,J,K ordering  ---
!
      DO 40 K = 1,LFZ
      DO 40 J = 1,LFY
      DO 40 I = 1,LFX
        N = ND(I,J,K)
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 40
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 30 KX = 1,KRX
        DO 30 JX = 1,JRX
        DO 30 IX = 1,IRX
          NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
          NP = IXP(NX)
!
!---      Node  ---
!
          MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              DO 21 MX = 1,4
                NB = ICM(MX,1,NX)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NB)),
     &            ABS(IM(ISVC,NP)-IM(1,NB)))
   21         CONTINUE
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              DO 22 MX = 1,4
                NS = ICM(MX,2,NX)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NS)),
     &            ABS(IM(ISVC,NP)-IM(1,NS)))
   22         CONTINUE
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              DO 23 MX = 1,4
                NW = ICM(MX,3,NX)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NW)),
     &            ABS(IM(ISVC,NP)-IM(1,NW)))
   23         CONTINUE
           ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.LFX ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DO 24 MX = 1,4
                NE = ICM(MX,4,NX)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NE)),
     &            ABS(IM(ISVC,NP)-IM(1,NE)))
   24         CONTINUE
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.LFY ) THEN
            IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              DO 25 MX = 1,4
                NN = ICM(MX,5,NX)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NN)),
     &            ABS(IM(ISVC,NP)-IM(1,NN)))
   25         CONTINUE
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.LFZ ) THEN
            IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              DO 26 MX = 1,4
                NT = ICM(MX,6,NX)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NT)),
     &            ABS(IM(ISVC,NP)-IM(1,NT)))
   26         CONTINUE
            ENDIF
          ENDIF
   30   CONTINUE
   40 CONTINUE
!
!---  Coupled equations half band width using J,K,I ordering  ---
!
      NC = 0
      DO 70 I = 1,LFX
      DO 70 K = 1,LFZ
      DO 70 J = 1,LFY
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 70
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          IXP(N) = -(IRX*JRX*KRX)
          DO 62 IX = 1,IRX
          DO 62 KX = 1,KRX
          DO 62 JX = 1,JRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NC = NC + 1
            IXP(NX) = NC
   62     CONTINUE
        ELSE
          NC = NC + 1
          IXP(N) = NC
        ENDIF
   70 CONTINUE
      NC = 0
      MHBC_JKI = 0
      DO 80 I = 1,LFX
      DO 80 K = 1,LFZ
      DO 80 J = 1,LFY
        N = ND(I,J,K)
!
!---     Loop over block refined nodes  ---
!
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 74 IX = 1,IRX
          DO 74 KX = 1,KRX
          DO 74 JX = 1,JRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NMD = IXP(NX)
            DO 72 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
   72       CONTINUE
   74     CONTINUE
        ELSE
          NMD = IXP(N)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 80
          DO 76 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
   76     CONTINUE
        ENDIF
   80 CONTINUE
!
!---  Determine matrix half-band widths, using J,K,I ordering  ---
!
      DO 100 I = 1,LFX
      DO 100 K = 1,LFZ
      DO 100 J = 1,LFY
        N = ND(I,J,K)
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 100
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 90 IX = 1,IRX
        DO 90 KX = 1,KRX
        DO 90 JX = 1,JRX
          NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
          NP = IXP(NX)
!
!---      Node  ---
!
          MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              DO 81 MX = 1,4
                NB = ICM(MX,1,NX)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NB)),
     &            ABS(IM(ISVC,NP)-IM(1,NB)))
   81         CONTINUE
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              DO 82 MX = 1,4
                NS = ICM(MX,2,NX)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NS)),
     &            ABS(IM(ISVC,NP)-IM(1,NS)))
   82         CONTINUE
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              DO 83 MX = 1,4
                NW = ICM(MX,3,NX)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NW)),
     &            ABS(IM(ISVC,NP)-IM(1,NW)))
   83         CONTINUE
           ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.LFX ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DO 84 MX = 1,4
                NE = ICM(MX,4,NX)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NE)),
     &            ABS(IM(ISVC,NP)-IM(1,NE)))
   84         CONTINUE
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.LFY ) THEN
            IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              DO 85 MX = 1,4
                NN = ICM(MX,5,NX)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NN)),
     &            ABS(IM(ISVC,NP)-IM(1,NN)))
   85         CONTINUE
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.LFZ ) THEN
            IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              DO 86 MX = 1,4
                NT = ICM(MX,6,NX)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NT)),
     &            ABS(IM(ISVC,NP)-IM(1,NT)))
   86         CONTINUE
            ENDIF
          ENDIF
   90   CONTINUE
  100 CONTINUE
!
!---  Coupled equations half band width using K,I,J ordering  ---
!
      NC = 0
      DO 130 J = 1,LFY
      DO 130 I = 1,LFX
      DO 130 K = 1,LFZ
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 130
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          IXP(N) = -(IRX*JRX*KRX)
          DO 122 JX = 1,JRX
          DO 122 IX = 1,IRX
          DO 122 KX = 1,KRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NC = NC + 1
            IXP(NX) = NC
  122     CONTINUE
        ELSE
          NC = NC + 1
          IXP(N) = NC
        ENDIF
  130 CONTINUE
      NC = 0
      MHBC_KIJ = 0
      DO 140 J = 1,LFY
      DO 140 I = 1,LFX
      DO 140 K = 1,LFZ
        N = ND(I,J,K)
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
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NMD = IXP(NX)
            DO 132 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  132       CONTINUE
  134     CONTINUE
        ELSE
          NMD = IXP(N)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 140
          DO 136 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
  136     CONTINUE
        ENDIF
  140 CONTINUE
!
!---  Determine matrix half-band widths, using K,I,J ordering  ---
!
      DO 160 J = 1,LFY
      DO 160 I = 1,LFX
      DO 160 K = 1,LFZ
        N = ND(I,J,K)
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 160
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 150 IX = 1,IRX
        DO 150 KX = 1,KRX
        DO 150 JX = 1,JRX
          NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
          NP = IXP(NX)
!
!---      Node  ---
!
          MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              DO 141 MX = 1,4
                NB = ICM(MX,1,NX)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NB)),
     &            ABS(IM(ISVC,NP)-IM(1,NB)))
  141         CONTINUE
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              DO 142 MX = 1,4
                NS = ICM(MX,2,NX)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NS)),
     &            ABS(IM(ISVC,NP)-IM(1,NS)))
  142         CONTINUE
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              DO 143 MX = 1,4
                NW = ICM(MX,3,NX)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NW)),
     &            ABS(IM(ISVC,NP)-IM(1,NW)))
  143         CONTINUE
           ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.LFX ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DO 144 MX = 1,4
                NE = ICM(MX,4,NX)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NE)),
     &            ABS(IM(ISVC,NP)-IM(1,NE)))
  144         CONTINUE
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.LFY ) THEN
            IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              DO 145 MX = 1,4
                NN = ICM(MX,5,NX)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NN)),
     &            ABS(IM(ISVC,NP)-IM(1,NN)))
  145         CONTINUE
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.LFZ ) THEN
            IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              DO 146 MX = 1,4
                NT = ICM(MX,6,NX)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NT)),
     &            ABS(IM(ISVC,NP)-IM(1,NT)))
  146         CONTINUE
            ENDIF
          ENDIF
  150   CONTINUE
  160 CONTINUE
!
!---  I,J,K ordering yields the lowest half band width   ---
!
      IF( MHBC_IJK.LE.MHBC_JKI .AND. MHBC_IJK.LE.MHBC_KIJ ) THEN
!
!---    Equation ordering using I,J,K ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 210 K = 1,LFZ
        DO 210 J = 1,LFY
        DO 210 I = 1,LFX
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 210
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            IXP(N) = -(IRX*JRX*KRX)
            DO 202 KX = 1,KRX
            DO 202 JX = 1,JRX
            DO 202 IX = 1,IRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NC = NC + 1
              IXP(NX) = NC
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
!
!---    Coupled equations half band width using I,J,K ordering  ---
!
        DO 230 K = 1,LFZ
        DO 230 J = 1,LFY
        DO 230 I = 1,LFX
          N = ND(I,J,K)
!
!---      Loop over block refined nodes  ---
!
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 214 KX = 1,KRX
            DO 214 JX = 1,JRX
            DO 214 IX = 1,IRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NMD = IXP(NX)
              DO 212 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  212         CONTINUE
  214       CONTINUE
          ELSE
            NMD = IXP(N)
!
!---        Skip to next active node  ---
!
            IF( IXP(N).EQ.0 ) GOTO 230
            DO 216 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  216       CONTINUE
          ENDIF
  230   CONTINUE
!
!---    Determine matrix half-band widths, using I,J,K ordering  ---
!
        DO 260 K = 1,LFZ
        DO 260 J = 1,LFY
        DO 260 I = 1,LFX
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 260
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 250 KX = 1,KRX
          DO 250 JX = 1,JRX
          DO 250 IX = 1,IRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NP = IXP(NX)
!
!---        Node  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
              IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                DO 241 MX = 1,4
                  NB = ICM(MX,1,NX)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
  241           CONTINUE
              ENDIF
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
              IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                DO 242 MX = 1,4
                  NS = ICM(MX,2,NX)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
  242           CONTINUE
              ENDIF
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
              IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                DO 243 MX = 1,4
                  NW = ICM(MX,3,NX)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
  243           CONTINUE
              ENDIF
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.LFX ) THEN
              IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                DO 244 MX = 1,4
                  NE = ICM(MX,4,NX)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
  244           CONTINUE
              ENDIF
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.LFY ) THEN
              IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                DO 245 MX = 1,4
                  NN = ICM(MX,5,NX)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &               ABS(IM(ISVC,NP)-IM(1,NN)))
  245           CONTINUE
              ENDIF
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.LFZ ) THEN
              IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                DO 246 MX = 1,4
                  NT = ICM(MX,6,NX)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
  246           CONTINUE
              ENDIF
            ENDIF
  250     CONTINUE
  260   CONTINUE
!
!---  J,K,I ordering yields the lowest half band width   ---
!
      ELSEIF( MHBC_JKI.LE.MHBC_KIJ ) THEN
!
!---    Equation ordering using J,K,I ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 310 I = 1,LFX
        DO 310 K = 1,LFZ
        DO 310 J = 1,LFY
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 310
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            IXP(N) = -(IRX*JRX*KRX)
            DO 302 IX = 1,IRX
            DO 302 KX = 1,KRX
            DO 302 JX = 1,JRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NC = NC + 1
              IXP(NX) = NC
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
!
!---    Coupled equations half band width using J,K,I ordering  ---
!
        DO 330 I = 1,LFX
        DO 330 K = 1,LFZ
        DO 330 J = 1,LFY
          N = ND(I,J,K)
!
!---      Loop over block refined nodes  ---
!
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 314 IX = 1,IRX
            DO 314 KX = 1,KRX
            DO 314 JX = 1,JRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NMD = IXP(NX)
              DO 312 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  312         CONTINUE
  314       CONTINUE
          ELSE
            NMD = IXP(N)
!
!---        Skip to next active node  ---
!
            IF( IXP(N).EQ.0 ) GOTO 330
            DO 316 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  316       CONTINUE
          ENDIF
  330   CONTINUE
!
!---    Determine matrix half-band widths using J,K,I ordering  ---
!
        DO 360 I = 1,LFX
        DO 360 K = 1,LFZ
        DO 360 J = 1,LFY
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 360
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 350 KX = 1,KRX
          DO 350 JX = 1,JRX
          DO 350 IX = 1,IRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NP = IXP(NX)
!
!---        Node  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
              IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                DO 341 MX = 1,4
                  NB = ICM(MX,1,NX)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
  341           CONTINUE
              ENDIF
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
              IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                DO 342 MX = 1,4
                  NS = ICM(MX,2,NX)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
  342           CONTINUE
              ENDIF
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
              IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                DO 343 MX = 1,4
                  NW = ICM(MX,3,NX)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
  343           CONTINUE
              ENDIF
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.LFX ) THEN
              IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                DO 344 MX = 1,4
                  NE = ICM(MX,4,NX)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
  344           CONTINUE
              ENDIF
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.LFY ) THEN
              IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                DO 345 MX = 1,4
                  NN = ICM(MX,5,NX)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &               ABS(IM(ISVC,NP)-IM(1,NN)))
  345           CONTINUE
              ENDIF
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.LFZ ) THEN
              IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                DO 346 MX = 1,4
                  NT = ICM(MX,6,NX)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
  346           CONTINUE
              ENDIF
            ENDIF
  350     CONTINUE
  360   CONTINUE
!
!---  K,I,J ordering yields the lowest half band width   ---
!
      ELSE
!
!---    Equation ordering using K,I,J ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 410 J = 1,LFY
        DO 410 I = 1,LFX
        DO 410 K = 1,LFZ
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 410
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            IXP(N) = -(IRX*JRX*KRX)
            DO 402 JX = 1,JRX
            DO 402 IX = 1,IRX
            DO 402 KX = 1,KRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NC = NC + 1
              IXP(NX) = NC
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
!
!---    Coupled equations half band width using K,I,J ordering  ---
!
        DO 430 J = 1,LFY
        DO 430 I = 1,LFX
        DO 430 K = 1,LFZ
          N = ND(I,J,K)
!
!---      Loop over block refined nodes  ---
!
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 414 IX = 1,IRX
            DO 414 KX = 1,KRX
            DO 414 JX = 1,JRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NMD = IXP(NX)
              DO 412 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  412         CONTINUE
  414       CONTINUE
          ELSE
            NMD = IXP(N)
!
!---        Skip to next active node  ---
!
            IF( IXP(N).EQ.0 ) GOTO 430
            DO 416 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  416       CONTINUE
          ENDIF
  430   CONTINUE
!
!---    Determine matrix half-band widths using K,I,J ordering  ---
!
        DO 460 J = 1,LFY
        DO 460 I = 1,LFX
        DO 460 K = 1,LFZ
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 460
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 450 KX = 1,KRX
          DO 450 JX = 1,JRX
          DO 450 IX = 1,IRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NP = IXP(NX)
!
!---        Node  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
              IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                DO 441 MX = 1,4
                  NB = ICM(MX,1,NX)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
  441           CONTINUE
              ENDIF
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
              IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                DO 442 MX = 1,4
                  NS = ICM(MX,2,NX)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
  442           CONTINUE
              ENDIF
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
              IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                DO 443 MX = 1,4
                  NW = ICM(MX,3,NX)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
  443           CONTINUE
              ENDIF
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.LFX ) THEN
              IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                DO 444 MX = 1,4
                  NE = ICM(MX,4,NX)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
  444           CONTINUE
              ENDIF
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.LFY ) THEN
              IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                DO 445 MX = 1,4
                  NN = ICM(MX,5,NX)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &               ABS(IM(ISVC,NP)-IM(1,NN)))
  445           CONTINUE
              ENDIF
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.LFZ ) THEN
              IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                DO 446 MX = 1,4
                  NT = ICM(MX,6,NX)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
  446           CONTINUE
              ENDIF
            ENDIF
  450     CONTINUE
  460   CONTINUE
      ENDIF
      LHBW = MHBC

!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCB_NCW group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCB_NCW_DP
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
!     Determine the matrix half band width for no coupled
!     well model
!
!----------------------Authors-----------------------------------------!
!
!     Written by M.D. White, PNNL, 03 February 2014.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE JACOB
      USE GRID
      USE DUAL_POR
      USE COUP_WELL
      USE CONST
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/JCB_NCW_DP'

!
!---  Allocate memory for Jacobian matrix equation index  ---
!
      IF( .NOT.ALLOCATED(IM) ) THEN
        ALLOCATE( IM(1:LUK,1:LFD+LBR),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: IM'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Allocate memory for the matrix dual-porosity Jacobian matrix 
!     pointer array  ---
!
      IF( .NOT.ALLOCATED(IM_M) ) THEN
        LFD_DP = LFD**L_DP
        ALLOCATE( IM_M(1:LUK,1:LFD_DP),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: IM_M'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Initialize number of unknowns  ---
!
      ISVC = LUK
      ILIMIT = 2**30
!
!---  Coupled equations half band width using I,J,K ordering  ---
!
      NC = 0
      DO 10 K = 1,LFZ
      DO 10 J = 1,LFY
      DO 10 I = 1,LFX
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 10
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          IXP(N) = -(IRX*JRX*KRX)
          DO 2 KX = 1,KRX
          DO 2 JX = 1,JRX
          DO 2 IX = 1,IRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NC = NC + 1
            IXP(NX) = NC
    2     CONTINUE
        ELSE
          NC = NC + 1
          IXP(N) = NC
        ENDIF
   10 CONTINUE
      NC = 0
      MHBC_IJK = 0
      DO 20 K = 1,LFZ
      DO 20 J = 1,LFY
      DO 20 I = 1,LFX
        N = ND(I,J,K)
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
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NMD = IXP(NX)
            DO 12 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
   12       CONTINUE
            DO 13 M = 1,ISVC
              NC = NC+1
              IM_M(M,NMD) = NC
   13       CONTINUE
   14     CONTINUE
        ELSE
          NMD = IXP(N)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 20
          DO 16 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
   16     CONTINUE
          DO 17 M = 1,ISVC
            NC = NC+1
            IM_M(M,NMD) = NC
   17     CONTINUE
        ENDIF
   20 CONTINUE
!
!---  Determine matrix half-band widths, using I,J,K ordering  ---
!
      DO 40 K = 1,LFZ
      DO 40 J = 1,LFY
      DO 40 I = 1,LFX
        N = ND(I,J,K)
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 40
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 30 KX = 1,KRX
        DO 30 JX = 1,JRX
        DO 30 IX = 1,IRX
          NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
          NP = IXP(NX)
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
            IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              DO 21 MX = 1,4
                NB = ICM(MX,1,NX)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NB)),
     &            ABS(IM(ISVC,NP)-IM(1,NB)))
   21         CONTINUE
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              DO 22 MX = 1,4
                NS = ICM(MX,2,NX)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NS)),
     &            ABS(IM(ISVC,NP)-IM(1,NS)))
   22         CONTINUE
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              DO 23 MX = 1,4
                NW = ICM(MX,3,NX)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NW)),
     &            ABS(IM(ISVC,NP)-IM(1,NW)))
   23         CONTINUE
           ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.LFX ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DO 24 MX = 1,4
                NE = ICM(MX,4,NX)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NE)),
     &            ABS(IM(ISVC,NP)-IM(1,NE)))
   24         CONTINUE
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.LFY ) THEN
            IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              DO 25 MX = 1,4
                NN = ICM(MX,5,NX)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NN)),
     &            ABS(IM(ISVC,NP)-IM(1,NN)))
   25         CONTINUE
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.LFZ ) THEN
            IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              DO 26 MX = 1,4
                NT = ICM(MX,6,NX)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NT)),
     &            ABS(IM(ISVC,NP)-IM(1,NT)))
   26         CONTINUE
            ENDIF
          ENDIF
   30   CONTINUE
   40 CONTINUE
!
!---  Coupled equations half band width using J,K,I ordering  ---
!
      NC = 0
      DO 70 I = 1,LFX
      DO 70 K = 1,LFZ
      DO 70 J = 1,LFY
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 70
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          IXP(N) = -(IRX*JRX*KRX)
          DO 62 IX = 1,IRX
          DO 62 KX = 1,KRX
          DO 62 JX = 1,JRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NC = NC + 1
            IXP(NX) = NC
   62     CONTINUE
        ELSE
          NC = NC + 1
          IXP(N) = NC
        ENDIF
   70 CONTINUE
      NC = 0
      MHBC_JKI = 0
      DO 80 I = 1,LFX
      DO 80 K = 1,LFZ
      DO 80 J = 1,LFY
        N = ND(I,J,K)
!
!---     Loop over block refined nodes  ---
!
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 74 IX = 1,IRX
          DO 74 KX = 1,KRX
          DO 74 JX = 1,JRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NMD = IXP(NX)
            DO 72 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
   72       CONTINUE
            DO 73 M = 1,ISVC
              NC = NC+1
              IM_M(M,NMD) = NC
   73       CONTINUE
   74     CONTINUE
        ELSE
          NMD = IXP(N)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 80
          DO 76 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
   76     CONTINUE
          DO 77 M = 1,ISVC
            NC = NC+1
            IM_M(M,NMD) = NC
   77     CONTINUE
        ENDIF
   80 CONTINUE
!
!---  Determine matrix half-band widths, using J,K,I ordering  ---
!
      DO 100 I = 1,LFX
      DO 100 K = 1,LFZ
      DO 100 J = 1,LFY
        N = ND(I,J,K)
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 100
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 90 IX = 1,IRX
        DO 90 KX = 1,KRX
        DO 90 JX = 1,JRX
          NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
          NP = IXP(NX)
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
            IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              DO 81 MX = 1,4
                NB = ICM(MX,1,NX)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NB)),
     &            ABS(IM(ISVC,NP)-IM(1,NB)))
   81         CONTINUE
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              DO 82 MX = 1,4
                NS = ICM(MX,2,NX)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NS)),
     &            ABS(IM(ISVC,NP)-IM(1,NS)))
   82         CONTINUE
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              DO 83 MX = 1,4
                NW = ICM(MX,3,NX)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NW)),
     &            ABS(IM(ISVC,NP)-IM(1,NW)))
   83         CONTINUE
           ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.LFX ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DO 84 MX = 1,4
                NE = ICM(MX,4,NX)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NE)),
     &            ABS(IM(ISVC,NP)-IM(1,NE)))
   84         CONTINUE
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.LFY ) THEN
            IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              DO 85 MX = 1,4
                NN = ICM(MX,5,NX)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NN)),
     &            ABS(IM(ISVC,NP)-IM(1,NN)))
   85         CONTINUE
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.LFZ ) THEN
            IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              DO 86 MX = 1,4
                NT = ICM(MX,6,NX)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NT)),
     &            ABS(IM(ISVC,NP)-IM(1,NT)))
   86         CONTINUE
            ENDIF
          ENDIF
   90   CONTINUE
  100 CONTINUE
!
!---  Coupled equations half band width using K,I,J ordering  ---
!
      NC = 0
      DO 130 J = 1,LFY
      DO 130 I = 1,LFX
      DO 130 K = 1,LFZ
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 130
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          IXP(N) = -(IRX*JRX*KRX)
          DO 122 JX = 1,JRX
          DO 122 IX = 1,IRX
          DO 122 KX = 1,KRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NC = NC + 1
            IXP(NX) = NC
  122     CONTINUE
        ELSE
          NC = NC + 1
          IXP(N) = NC
        ENDIF
  130 CONTINUE
      NC = 0
      MHBC_KIJ = 0
      DO 140 J = 1,LFY
      DO 140 I = 1,LFX
      DO 140 K = 1,LFZ
        N = ND(I,J,K)
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
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NMD = IXP(NX)
            DO 132 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  132       CONTINUE
            DO 133 M = 1,ISVC
              NC = NC+1
              IM_M(M,NMD) = NC
  133       CONTINUE
  134     CONTINUE
        ELSE
          NMD = IXP(N)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 140
          DO 136 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
  136     CONTINUE
          DO 137 M = 1,ISVC
            NC = NC+1
            IM_M(M,NMD) = NC
  137     CONTINUE
        ENDIF
  140 CONTINUE
!
!---  Determine matrix half-band widths, using K,I,J ordering  ---
!
      DO 160 J = 1,LFY
      DO 160 I = 1,LFX
      DO 160 K = 1,LFZ
        N = ND(I,J,K)
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 160
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 150 IX = 1,IRX
        DO 150 KX = 1,KRX
        DO 150 JX = 1,JRX
          NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
          NP = IXP(NX)
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
            IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              DO 141 MX = 1,4
                NB = ICM(MX,1,NX)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NB)),
     &            ABS(IM(ISVC,NP)-IM(1,NB)))
  141         CONTINUE
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              DO 142 MX = 1,4
                NS = ICM(MX,2,NX)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NS)),
     &            ABS(IM(ISVC,NP)-IM(1,NS)))
  142         CONTINUE
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              DO 143 MX = 1,4
                NW = ICM(MX,3,NX)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NW)),
     &            ABS(IM(ISVC,NP)-IM(1,NW)))
  143         CONTINUE
           ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.LFX ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DO 144 MX = 1,4
                NE = ICM(MX,4,NX)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NE)),
     &            ABS(IM(ISVC,NP)-IM(1,NE)))
  144         CONTINUE
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.LFY ) THEN
            IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              DO 145 MX = 1,4
                NN = ICM(MX,5,NX)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NN)),
     &            ABS(IM(ISVC,NP)-IM(1,NN)))
  145         CONTINUE
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.LFZ ) THEN
            IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              DO 146 MX = 1,4
                NT = ICM(MX,6,NX)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NT)),
     &            ABS(IM(ISVC,NP)-IM(1,NT)))
  146         CONTINUE
            ENDIF
          ENDIF
  150   CONTINUE
  160 CONTINUE
!
!---  I,J,K ordering yields the lowest half band width   ---
!
      IF( MHBC_IJK.LE.MHBC_JKI .AND. MHBC_IJK.LE.MHBC_KIJ ) THEN
!
!---    Equation ordering using I,J,K ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 210 K = 1,LFZ
        DO 210 J = 1,LFY
        DO 210 I = 1,LFX
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 210
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            IXP(N) = -(IRX*JRX*KRX)
            DO 202 KX = 1,KRX
            DO 202 JX = 1,JRX
            DO 202 IX = 1,IRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NC = NC + 1
              IXP(NX) = NC
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
!
!---    Coupled equations half band width using I,J,K ordering  ---
!
        DO 230 K = 1,LFZ
        DO 230 J = 1,LFY
        DO 230 I = 1,LFX
          N = ND(I,J,K)
!
!---      Loop over block refined nodes  ---
!
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 214 KX = 1,KRX
            DO 214 JX = 1,JRX
            DO 214 IX = 1,IRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NMD = IXP(NX)
              DO 212 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  212         CONTINUE
              DO 213 M = 1,ISVC
                NC = NC+1
                IM_M(M,NMD) = NC
  213         CONTINUE
  214       CONTINUE
          ELSE
            NMD = IXP(N)
!
!---        Skip to next active node  ---
!
            IF( IXP(N).EQ.0 ) GOTO 230
            DO 216 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  216       CONTINUE
            DO 217 M = 1,ISVC
              NC = NC+1
              IM_M(M,NMD) = NC
  217       CONTINUE
          ENDIF
  230   CONTINUE
!
!---    Determine matrix half-band widths, using I,J,K ordering  ---
!
        DO 260 K = 1,LFZ
        DO 260 J = 1,LFY
        DO 260 I = 1,LFX
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 260
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 250 KX = 1,KRX
          DO 250 JX = 1,JRX
          DO 250 IX = 1,IRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NP = IXP(NX)
!
!---        Node  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Fracture to matrix connections  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM_M(ISVC,NP)),
     &        ABS(IM(ISVC,NP)-IM_M(1,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
              IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                DO 241 MX = 1,4
                  NB = ICM(MX,1,NX)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
  241           CONTINUE
              ENDIF
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
              IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                DO 242 MX = 1,4
                  NS = ICM(MX,2,NX)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
  242           CONTINUE
              ENDIF
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
              IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                DO 243 MX = 1,4
                  NW = ICM(MX,3,NX)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
  243           CONTINUE
              ENDIF
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.LFX ) THEN
              IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                DO 244 MX = 1,4
                  NE = ICM(MX,4,NX)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
  244           CONTINUE
              ENDIF
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.LFY ) THEN
              IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                DO 245 MX = 1,4
                  NN = ICM(MX,5,NX)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &               ABS(IM(ISVC,NP)-IM(1,NN)))
  245           CONTINUE
              ENDIF
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.LFZ ) THEN
              IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                DO 246 MX = 1,4
                  NT = ICM(MX,6,NX)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
  246           CONTINUE
              ENDIF
            ENDIF
  250     CONTINUE
  260   CONTINUE
!
!---  J,K,I ordering yields the lowest half band width   ---
!
      ELSEIF( MHBC_JKI.LE.MHBC_KIJ ) THEN
!
!---    Equation ordering using J,K,I ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 310 I = 1,LFX
        DO 310 K = 1,LFZ
        DO 310 J = 1,LFY
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 310
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            IXP(N) = -(IRX*JRX*KRX)
            DO 302 IX = 1,IRX
            DO 302 KX = 1,KRX
            DO 302 JX = 1,JRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NC = NC + 1
              IXP(NX) = NC
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
!
!---    Coupled equations half band width using J,K,I ordering  ---
!
        DO 330 I = 1,LFX
        DO 330 K = 1,LFZ
        DO 330 J = 1,LFY
          N = ND(I,J,K)
!
!---      Loop over block refined nodes  ---
!
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 314 IX = 1,IRX
            DO 314 KX = 1,KRX
            DO 314 JX = 1,JRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NMD = IXP(NX)
              DO 312 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  312         CONTINUE
              DO 313 M = 1,ISVC
                NC = NC + 1
                IM_M(M,NMD) = NC
  313         CONTINUE
  314       CONTINUE
          ELSE
            NMD = IXP(N)
!
!---        Skip to next active node  ---
!
            IF( IXP(N).EQ.0 ) GOTO 330
            DO 316 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  316       CONTINUE
            DO 317 M = 1,ISVC
              NC = NC + 1
              IM_M(M,NMD) = NC
  317       CONTINUE
          ENDIF
  330   CONTINUE
!
!---    Determine matrix half-band widths using J,K,I ordering  ---
!
        DO 360 I = 1,LFX
        DO 360 K = 1,LFZ
        DO 360 J = 1,LFY
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 360
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 350 KX = 1,KRX
          DO 350 JX = 1,JRX
          DO 350 IX = 1,IRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NP = IXP(NX)
!
!---        Node  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Fracture to matrix connections  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM_M(ISVC,NP)),
     &        ABS(IM(ISVC,NP)-IM_M(1,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
              IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                DO 341 MX = 1,4
                  NB = ICM(MX,1,NX)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
  341           CONTINUE
              ENDIF
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
              IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                DO 342 MX = 1,4
                  NS = ICM(MX,2,NX)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
  342           CONTINUE
              ENDIF
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
              IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                DO 343 MX = 1,4
                  NW = ICM(MX,3,NX)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
  343           CONTINUE
              ENDIF
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.LFX ) THEN
              IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                DO 344 MX = 1,4
                  NE = ICM(MX,4,NX)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
  344           CONTINUE
              ENDIF
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.LFY ) THEN
              IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                DO 345 MX = 1,4
                  NN = ICM(MX,5,NX)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &               ABS(IM(ISVC,NP)-IM(1,NN)))
  345           CONTINUE
              ENDIF
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.LFZ ) THEN
              IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                DO 346 MX = 1,4
                  NT = ICM(MX,6,NX)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
  346           CONTINUE
              ENDIF
            ENDIF
  350     CONTINUE
  360   CONTINUE
!
!---  K,I,J ordering yields the lowest half band width   ---
!
      ELSE
!
!---    Equation ordering using K,I,J ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 410 J = 1,LFY
        DO 410 I = 1,LFX
        DO 410 K = 1,LFZ
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 410
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            IXP(N) = -(IRX*JRX*KRX)
            DO 402 JX = 1,JRX
            DO 402 IX = 1,IRX
            DO 402 KX = 1,KRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NC = NC + 1
              IXP(NX) = NC
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
!
!---    Coupled equations half band width using K,I,J ordering  ---
!
        DO 430 J = 1,LFY
        DO 430 I = 1,LFX
        DO 430 K = 1,LFZ
          N = ND(I,J,K)
!
!---      Loop over block refined nodes  ---
!
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 414 IX = 1,IRX
            DO 414 KX = 1,KRX
            DO 414 JX = 1,JRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NMD = IXP(NX)
              DO 412 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  412         CONTINUE
              DO 413 M = 1,ISVC
                NC = NC+1
                IM_M(M,NMD) = NC
  413         CONTINUE
  414       CONTINUE
          ELSE
            NMD = IXP(N)
!
!---        Skip to next active node  ---
!
            IF( IXP(N).EQ.0 ) GOTO 430
            DO 416 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  416       CONTINUE
            DO 417 M = 1,ISVC
              NC = NC+1
              IM_M(M,NMD) = NC
  417       CONTINUE
          ENDIF
  430   CONTINUE
!
!---    Determine matrix half-band widths using K,I,J ordering  ---
!
        DO 460 J = 1,LFY
        DO 460 I = 1,LFX
        DO 460 K = 1,LFZ
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 460
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 450 KX = 1,KRX
          DO 450 JX = 1,JRX
          DO 450 IX = 1,IRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NP = IXP(NX)
!
!---        Node  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Fracture to matrix connections  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM_M(ISVC,NP)),
     &        ABS(IM(ISVC,NP)-IM_M(1,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
              IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                DO 441 MX = 1,4
                  NB = ICM(MX,1,NX)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
  441           CONTINUE
              ENDIF
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
              IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                DO 442 MX = 1,4
                  NS = ICM(MX,2,NX)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
  442           CONTINUE
              ENDIF
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
              IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                DO 443 MX = 1,4
                  NW = ICM(MX,3,NX)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
  443           CONTINUE
              ENDIF
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.LFX ) THEN
              IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                DO 444 MX = 1,4
                  NE = ICM(MX,4,NX)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
  444           CONTINUE
              ENDIF
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.LFY ) THEN
              IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                DO 445 MX = 1,4
                  NN = ICM(MX,5,NX)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &               ABS(IM(ISVC,NP)-IM(1,NN)))
  445           CONTINUE
              ENDIF
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.LFZ ) THEN
              IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                DO 446 MX = 1,4
                  NT = ICM(MX,6,NX)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
  446           CONTINUE
              ENDIF
            ENDIF
  450     CONTINUE
  460   CONTINUE
      ENDIF
      LHBW = MHBC

!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCB_NCW_DP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCB_SURF_COV
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
!     Determine the matrix half band width for surface covers
!     for STOMP-GT.
!
!     Nodes adjacent to surface cover nodes include 2 bare surface
!     equations + 3 vegetated surface equations.
!     Nodes connected to surface cover nodes include 3 vegetated
!     surface equations.
!
!----------------------Authors-----------------------------------------!
!
!     Written by M.D. White, PNNL, 22 November 2015.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PLT_ATM
      USE JACOB
      USE GRID
      USE COUP_WELL
      USE CONST
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/JCB_SURF_COV'

!
!---  Allocate memory for Jacobian matrix equation index  ---
!
      IF( .NOT.ALLOCATED(IM) ) THEN
        ALLOCATE( IM(1:LUK,1:LFD+LBR),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: IM'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Allocate memory for Jacobian matrix equation index
!     for surface cover equations  ---
!
      IF( .NOT.ALLOCATED(JM_SFC) ) THEN
        ALLOCATE( JM_SFC(1:5,1:LSFCN),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: JM_SFC'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Initialize number of unknowns  ---
!
      ISVC = LUK
      ILIMIT = 2**30
!
!---  Coupled equations half band width using I,J,K ordering  ---
!
      NC = 0
      DO 10 K = 1,LFZ
      DO 10 J = 1,LFY
      DO 10 I = 1,LFX
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 10
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          IXP(N) = -(IRX*JRX*KRX)
          DO 2 KX = 1,KRX
          DO 2 JX = 1,JRX
          DO 2 IX = 1,IRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NC = NC + 1
            IXP(NX) = NC
    2     CONTINUE
        ELSE
          NC = NC + 1
          IXP(N) = NC
        ENDIF
   10 CONTINUE
      NC = 0
      MHBC_IJK = 0
      DO 20 K = 1,LFZ
      DO 20 J = 1,LFY
      DO 20 I = 1,LFX
        N = ND(I,J,K)
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
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NMD = IXP(NX)
            DO 12 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
   12       CONTINUE
!
!---        Loop over surface cover nodes  ---
!
            DO NSCN = 1,NSFCN
!
!---          Node adjacent to surface cover node  ---
!
              IF( ICM_SFC(1,NSCN).EQ.NX ) THEN
                DO M = 1,5
                  NC = NC + 1
                  JM_SFC(M,NSCN) = NC
                ENDDO
              ENDIF
            ENDDO
   14     CONTINUE
        ELSE
          NMD = IXP(N)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 20
          DO 16 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
   16     CONTINUE
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
        ENDIF
   20 CONTINUE
!
!---  Determine matrix half-band widths, using I,J,K ordering  ---
!
      DO 40 K = 1,LFZ
      DO 40 J = 1,LFY
      DO 40 I = 1,LFX
        N = ND(I,J,K)
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 40
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 30 KX = 1,KRX
        DO 30 JX = 1,JRX
        DO 30 IX = 1,IRX
          NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
          NP = IXP(NX)
!
!---      Node  ---
!
          MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              DO 21 MX = 1,4
                NB = ICM(MX,1,NX)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NB)),
     &            ABS(IM(ISVC,NP)-IM(1,NB)))
   21         CONTINUE
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              DO 22 MX = 1,4
                NS = ICM(MX,2,NX)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NS)),
     &            ABS(IM(ISVC,NP)-IM(1,NS)))
   22         CONTINUE
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              DO 23 MX = 1,4
                NW = ICM(MX,3,NX)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NW)),
     &            ABS(IM(ISVC,NP)-IM(1,NW)))
   23         CONTINUE
           ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.LFX ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DO 24 MX = 1,4
                NE = ICM(MX,4,NX)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NE)),
     &            ABS(IM(ISVC,NP)-IM(1,NE)))
   24         CONTINUE
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.LFY ) THEN
            IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              DO 25 MX = 1,4
                NN = ICM(MX,5,NX)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NN)),
     &            ABS(IM(ISVC,NP)-IM(1,NN)))
   25         CONTINUE
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.LFZ ) THEN
            IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              DO 26 MX = 1,4
                NT = ICM(MX,6,NX)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-IM(ISVC,NT)),
     &            ABS(IM(ISVC,NP)-IM(1,NT)))
   26         CONTINUE
            ENDIF
          ENDIF
!
!---      Loop over surface cover nodes  ---
!
          DO NSCN = 1,NSFCN
!
!---        Node adjacent to surface cover, consider bare surface
!           and vegetated surface equations  ---
!
            IF( ICM_SFC(1,NSCN).EQ.NX ) THEN
!
!---          Consider surface cover equation limits  ---
!
              MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-JM_SFC(1,NSCN)),
     &          ABS(IM(ISVC,NP)-JM_SFC(1,NSCN)))
              MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &          ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
            ENDIF
!
!---        Loop over surface cover connection map  ---
!
            DO NSCC = 2,NSFCC
              IF( ICM_SFC(NSCC,NSCN).EQ.0 ) EXIT
!
!---          Node connected to surface cover, consider vegetated
!             surface equations  ---
!
              IF( ICM_SFC(NSCC,NSCN).EQ.NX ) THEN
!
!---            Consider surface cover equation limits  ---
!
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-JM_SFC(3,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(3,NSCN)))
                MHBC_IJK = MAX(MHBC_IJK,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
              ENDIF
            ENDDO
          ENDDO
   30   CONTINUE
   40 CONTINUE
!
!---  Coupled equations half band width using J,K,I ordering  ---
!
      NC = 0
      DO 70 I = 1,LFX
      DO 70 K = 1,LFZ
      DO 70 J = 1,LFY
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 70
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          IXP(N) = -(IRX*JRX*KRX)
          DO 62 IX = 1,IRX
          DO 62 KX = 1,KRX
          DO 62 JX = 1,JRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NC = NC + 1
            IXP(NX) = NC
   62     CONTINUE
        ELSE
          NC = NC + 1
          IXP(N) = NC
        ENDIF
   70 CONTINUE
      NC = 0
      MHBC_JKI = 0
      DO 80 I = 1,LFX
      DO 80 K = 1,LFZ
      DO 80 J = 1,LFY
        N = ND(I,J,K)
!
!---     Loop over block refined nodes  ---
!
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 74 IX = 1,IRX
          DO 74 KX = 1,KRX
          DO 74 JX = 1,JRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NMD = IXP(NX)
            DO 72 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
   72       CONTINUE
!
!---        Loop over surface cover nodes  ---
!
            DO NSCN = 1,NSFCN
!
!---          Node adjacent to surface cover node  ---
!
              IF( ICM_SFC(1,NSCN).EQ.NX ) THEN
                DO M = 1,5
                  NC = NC + 1
                  JM_SFC(M,NSCN) = NC
                ENDDO
              ENDIF
            ENDDO
   74     CONTINUE
        ELSE
          NMD = IXP(N)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 80
          DO 76 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
   76     CONTINUE
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
        ENDIF
   80 CONTINUE
!
!---  Determine matrix half-band widths, using J,K,I ordering  ---
!
      DO 100 I = 1,LFX
      DO 100 K = 1,LFZ
      DO 100 J = 1,LFY
        N = ND(I,J,K)
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 100
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 90 IX = 1,IRX
        DO 90 KX = 1,KRX
        DO 90 JX = 1,JRX
          NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
          NP = IXP(NX)
!
!---      Node  ---
!
          MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              DO 81 MX = 1,4
                NB = ICM(MX,1,NX)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NB)),
     &            ABS(IM(ISVC,NP)-IM(1,NB)))
   81         CONTINUE
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              DO 82 MX = 1,4
                NS = ICM(MX,2,NX)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NS)),
     &            ABS(IM(ISVC,NP)-IM(1,NS)))
   82         CONTINUE
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              DO 83 MX = 1,4
                NW = ICM(MX,3,NX)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NW)),
     &            ABS(IM(ISVC,NP)-IM(1,NW)))
   83         CONTINUE
           ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.LFX ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DO 84 MX = 1,4
                NE = ICM(MX,4,NX)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NE)),
     &            ABS(IM(ISVC,NP)-IM(1,NE)))
   84         CONTINUE
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.LFY ) THEN
            IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              DO 85 MX = 1,4
                NN = ICM(MX,5,NX)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NN)),
     &            ABS(IM(ISVC,NP)-IM(1,NN)))
   85         CONTINUE
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.LFZ ) THEN
            IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              DO 86 MX = 1,4
                NT = ICM(MX,6,NX)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-IM(ISVC,NT)),
     &            ABS(IM(ISVC,NP)-IM(1,NT)))
   86         CONTINUE
            ENDIF
          ENDIF
!
!---      Loop over surface cover nodes  ---
!
          DO NSCN = 1,NSFCN
!
!---        Node adjacent to surface cover, consider bare surface
!           and vegetated surface equations  ---
!
            IF( ICM_SFC(1,NSCN).EQ.N ) THEN
!
!---          Consider surface cover equation limits  ---
!
              MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-JM_SFC(1,NSCN)),
     &          ABS(IM(ISVC,NP)-JM_SFC(1,NSCN)))
              MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &          ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
            ENDIF
!
!---        Loop over surface cover connection map  ---
!
            DO NSCC = 2,NSFCC
              IF( ICM_SFC(NSCC,NSCN).EQ.0 ) EXIT
!
!---          Node connected to surface cover, consider vegetated
!             surface equations  ---
!
              IF( ICM_SFC(NSCC,NSCN).EQ.NX ) THEN
!
!---            Consider surface cover equation limits  ---
!
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-JM_SFC(3,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(3,NSCN)))
                MHBC_JKI = MAX(MHBC_JKI,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
              ENDIF
            ENDDO
          ENDDO
   90   CONTINUE
  100 CONTINUE
!
!---  Coupled equations half band width using K,I,J ordering  ---
!
      NC = 0
      DO 130 J = 1,LFY
      DO 130 I = 1,LFX
      DO 130 K = 1,LFZ
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 130
        IF( IBR(4,N).GT.N ) THEN
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          IXP(N) = -(IRX*JRX*KRX)
          DO 122 JX = 1,JRX
          DO 122 IX = 1,IRX
          DO 122 KX = 1,KRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NC = NC + 1
            IXP(NX) = NC
  122     CONTINUE
        ELSE
          NC = NC + 1
          IXP(N) = NC
        ENDIF
  130 CONTINUE
      NC = 0
      MHBC_KIJ = 0
      DO 140 J = 1,LFY
      DO 140 I = 1,LFX
      DO 140 K = 1,LFZ
        N = ND(I,J,K)
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
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NMD = IXP(NX)
            DO 132 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  132       CONTINUE
!
!---        Loop over surface cover nodes  ---
!
            DO NSCN = 1,NSFCN
!
!---          Node adjacent to surface cover node  ---
!
              IF( ICM_SFC(1,NSCN).EQ.NX ) THEN
                DO M = 1,5
                  NC = NC + 1
                  JM_SFC(M,NSCN) = NC
                ENDDO
              ENDIF
            ENDDO
  134     CONTINUE
        ELSE
          NMD = IXP(N)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 140
          DO 136 M = 1,ISVC
            NC = NC+1
            IM(M,NMD) = NC
  136     CONTINUE
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
        ENDIF
  140 CONTINUE
!
!---  Determine matrix half-band widths, using K,I,J ordering  ---
!
      DO 160 J = 1,LFY
      DO 160 I = 1,LFX
      DO 160 K = 1,LFZ
        N = ND(I,J,K)
!
!---    Skip to next active node  ---
!
        IF( IXP(N).EQ.0 ) GOTO 160
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 150 IX = 1,IRX
        DO 150 KX = 1,KRX
        DO 150 JX = 1,JRX
          NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
          NP = IXP(NX)
!
!---      Node  ---
!
          MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---      Bottom node neighbors  ---
!
          IF( K.GT.1 ) THEN
            IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              DO 141 MX = 1,4
                NB = ICM(MX,1,NX)
                IF( NB.EQ.0 ) EXIT
                NB = IXP(NB)
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NB)),
     &            ABS(IM(ISVC,NP)-IM(1,NB)))
  141         CONTINUE
            ENDIF
          ENDIF
!
!---      South node neighbors  ---
!
          IF( J.GT.1 ) THEN
            IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              DO 142 MX = 1,4
                NS = ICM(MX,2,NX)
                IF( NS.EQ.0 ) EXIT
                NS = IXP(NS)
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NS)),
     &            ABS(IM(ISVC,NP)-IM(1,NS)))
  142         CONTINUE
            ENDIF
          ENDIF
!
!---      West node neighbors  ---
!
          IF( I.GT.1 ) THEN
            IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              DO 143 MX = 1,4
                NW = ICM(MX,3,NX)
                IF( NW.EQ.0 ) EXIT
                NW = IXP(NW)
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NW)),
     &            ABS(IM(ISVC,NP)-IM(1,NW)))
  143         CONTINUE
           ENDIF
          ENDIF
!
!---      East node neighbors  ---
!
          IF( I.LT.LFX ) THEN
            IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DO 144 MX = 1,4
                NE = ICM(MX,4,NX)
                IF( NE.EQ.0 ) EXIT
                NE = IXP(NE)
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NE)),
     &            ABS(IM(ISVC,NP)-IM(1,NE)))
  144         CONTINUE
            ENDIF
          ENDIF
!
!---      North node neighbors  ---
!
          IF( J.LT.LFY ) THEN
            IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              DO 145 MX = 1,4
                NN = ICM(MX,5,NX)
                IF( NN.EQ.0 ) EXIT
                NN = IXP(NN)
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NN)),
     &            ABS(IM(ISVC,NP)-IM(1,NN)))
  145         CONTINUE
            ENDIF
          ENDIF
!
!---      Top node neighbors  ---
!
          IF( K.LT.LFZ ) THEN
            IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              DO 146 MX = 1,4
                NT = ICM(MX,6,NX)
                IF( NT.EQ.0 ) EXIT
                NT = IXP(NT)
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-IM(ISVC,NT)),
     &            ABS(IM(ISVC,NP)-IM(1,NT)))
  146         CONTINUE
            ENDIF
          ENDIF
!
!---      Loop over surface cover nodes  ---
!
          DO NSCN = 1,NSFCN
!
!---        Node adjacent to surface cover, consider bare surface
!           and vegetated surface equations  ---
!
            IF( ICM_SFC(1,NSCN).EQ.N ) THEN
!
!---          Consider surface cover equation limits  ---
!
              MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-JM_SFC(1,NSCN)),
     &          ABS(IM(ISVC,NP)-JM_SFC(1,NSCN)))
              MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &          ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
            ENDIF
!
!---        Loop over surface cover connection map  ---
!
            DO NSCC = 2,NSFCC
              IF( ICM_SFC(NSCC,NSCN).EQ.0 ) EXIT
!
!---          Node connected to surface cover, consider vegetated
!             surface equations  ---
!
              IF( ICM_SFC(NSCC,NSCN).EQ.NX ) THEN
!
!---            Consider surface cover equation limits  ---
!
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-JM_SFC(3,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(3,NSCN)))
                MHBC_KIJ = MAX(MHBC_KIJ,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
              ENDIF
            ENDDO
          ENDDO
  150   CONTINUE
  160 CONTINUE
!
!---  I,J,K ordering yields the lowest half band width   ---
!
      IF( MHBC_IJK.LE.MHBC_JKI .AND. MHBC_IJK.LE.MHBC_KIJ ) THEN
!
!---    Equation ordering using I,J,K ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 210 K = 1,LFZ
        DO 210 J = 1,LFY
        DO 210 I = 1,LFX
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 210
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            IXP(N) = -(IRX*JRX*KRX)
            DO 202 KX = 1,KRX
            DO 202 JX = 1,JRX
            DO 202 IX = 1,IRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NC = NC + 1
              IXP(NX) = NC
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
!
!---    Coupled equations half band width using I,J,K ordering  ---
!
        DO 230 K = 1,LFZ
        DO 230 J = 1,LFY
        DO 230 I = 1,LFX
          N = ND(I,J,K)
!
!---      Loop over block refined nodes  ---
!
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 214 KX = 1,KRX
            DO 214 JX = 1,JRX
            DO 214 IX = 1,IRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NMD = IXP(NX)
              DO 212 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  212         CONTINUE
!
!---          Loop over surface cover nodes  ---
!
              DO NSCN = 1,NSFCN
!
!---            Node adjacent to surface cover node  ---
!
                IF( ICM_SFC(1,NSCN).EQ.NX ) THEN
                  DO M = 1,5
                    NC = NC + 1
                    JM_SFC(M,NSCN) = NC
                  ENDDO
                ENDIF
              ENDDO
  214       CONTINUE
          ELSE
            NMD = IXP(N)
!
!---        Skip to next active node  ---
!
            IF( IXP(N).EQ.0 ) GOTO 230
            DO 216 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  216       CONTINUE
!
!---        Loop over surface cover nodes  ---
!
            DO NSCN = 1,NSFCN
!
!---          Node adjacent to surface cover node  ---
!
              IF( ICM_SFC(1,NSCN).EQ.N ) THEN
                DO M = 1,5
                  NC = NC + 1
                  JM_SFC(M,NSCN) = NC
                ENDDO
              ENDIF
            ENDDO
          ENDIF
  230   CONTINUE
!
!---    Determine matrix half-band widths, using I,J,K ordering  ---
!
        DO 260 K = 1,LFZ
        DO 260 J = 1,LFY
        DO 260 I = 1,LFX
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 260
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 250 KX = 1,KRX
          DO 250 JX = 1,JRX
          DO 250 IX = 1,IRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NP = IXP(NX)
!
!---        Node  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
              IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                DO 241 MX = 1,4
                  NB = ICM(MX,1,NX)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
  241           CONTINUE
              ENDIF
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
              IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                DO 242 MX = 1,4
                  NS = ICM(MX,2,NX)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
  242           CONTINUE
              ENDIF
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
              IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                DO 243 MX = 1,4
                  NW = ICM(MX,3,NX)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
  243           CONTINUE
              ENDIF
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.LFX ) THEN
              IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                DO 244 MX = 1,4
                  NE = ICM(MX,4,NX)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
  244           CONTINUE
              ENDIF
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.LFY ) THEN
              IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                DO 245 MX = 1,4
                  NN = ICM(MX,5,NX)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &               ABS(IM(ISVC,NP)-IM(1,NN)))
  245           CONTINUE
              ENDIF
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.LFZ ) THEN
              IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                DO 246 MX = 1,4
                  NT = ICM(MX,6,NX)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
  246           CONTINUE
              ENDIF
            ENDIF
!
!---        Loop over surface cover nodes  ---
!
            DO NSCN = 1,NSFCN
!
!---          Node adjacent to surface cover, consider bare surface
!             and vegetated surface equations  ---
!
              IF( ICM_SFC(1,NSCN).EQ.NX ) THEN
!
!---            Consider surface cover equation limits  ---
!
                MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(1,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(1,NSCN)))
                MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
              ENDIF
!
!---          Loop over surface cover connection map  ---
!
              DO NSCC = 2,NSFCC
                IF( ICM_SFC(NSCC,NSCN).EQ.0 ) EXIT
!
!---            Node connected to surface cover, consider vegetated
!               surface equations  ---
!
                IF( ICM_SFC(NSCC,NSCN).EQ.NX ) THEN
!
!---              Consider surface cover equation limits  ---
!
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(3,NSCN)),
     &              ABS(IM(ISVC,NP)-JM_SFC(3,NSCN)))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &              ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
                ENDIF
              ENDDO
            ENDDO
  250     CONTINUE
  260   CONTINUE
!
!---  J,K,I ordering yields the lowest half band width   ---
!
      ELSEIF( MHBC_JKI.LE.MHBC_KIJ ) THEN
!
!---    Equation ordering using J,K,I ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 310 I = 1,LFX
        DO 310 K = 1,LFZ
        DO 310 J = 1,LFY
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 310
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            IXP(N) = -(IRX*JRX*KRX)
            DO 302 IX = 1,IRX
            DO 302 KX = 1,KRX
            DO 302 JX = 1,JRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NC = NC + 1
              IXP(NX) = NC
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
!
!---    Coupled equations half band width using J,K,I ordering  ---
!
        DO 330 I = 1,LFX
        DO 330 K = 1,LFZ
        DO 330 J = 1,LFY
          N = ND(I,J,K)
!
!---      Loop over block refined nodes  ---
!
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 314 IX = 1,IRX
            DO 314 KX = 1,KRX
            DO 314 JX = 1,JRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NMD = IXP(NX)
              DO 312 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  312         CONTINUE
!
!---          Loop over surface cover nodes  ---
!
              DO NSCN = 1,NSFCN
!
!---            Node adjacent to surface cover node  ---
!
                IF( ICM_SFC(1,NSCN).EQ.NX ) THEN
                  DO M = 1,5
                    NC = NC + 1
                    JM_SFC(M,NSCN) = NC
                  ENDDO
                ENDIF
              ENDDO
  314       CONTINUE
          ELSE
            NMD = IXP(N)
!
!---        Skip to next active node  ---
!
            IF( IXP(N).EQ.0 ) GOTO 330
            DO 316 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  316       CONTINUE
!
!---        Loop over surface cover nodes  ---
!
            DO NSCN = 1,NSFCN
!
!---          Node adjacent to surface cover node  ---
!
              IF( ICM_SFC(1,NSCN).EQ.N ) THEN
                DO M = 1,5
                  NC = NC + 1
                  JM_SFC(M,NSCN) = NC
                ENDDO
              ENDIF
            ENDDO
          ENDIF
  330   CONTINUE
!
!---    Determine matrix half-band widths using J,K,I ordering  ---
!
        DO 360 I = 1,LFX
        DO 360 K = 1,LFZ
        DO 360 J = 1,LFY
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 360
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 350 KX = 1,KRX
          DO 350 JX = 1,JRX
          DO 350 IX = 1,IRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NP = IXP(NX)
!
!---        Node  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
              IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                DO 341 MX = 1,4
                  NB = ICM(MX,1,NX)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
  341           CONTINUE
              ENDIF
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
              IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                DO 342 MX = 1,4
                  NS = ICM(MX,2,NX)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
  342           CONTINUE
              ENDIF
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
              IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                DO 343 MX = 1,4
                  NW = ICM(MX,3,NX)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
  343           CONTINUE
              ENDIF
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.LFX ) THEN
              IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                DO 344 MX = 1,4
                  NE = ICM(MX,4,NX)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
  344           CONTINUE
              ENDIF
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.LFY ) THEN
              IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                DO 345 MX = 1,4
                  NN = ICM(MX,5,NX)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &               ABS(IM(ISVC,NP)-IM(1,NN)))
  345           CONTINUE
              ENDIF
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.LFZ ) THEN
              IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                DO 346 MX = 1,4
                  NT = ICM(MX,6,NX)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
  346           CONTINUE
              ENDIF
            ENDIF
!
!---        Loop over surface cover nodes  ---
!
            DO NSCN = 1,NSFCN
!
!---          Node adjacent to surface cover, consider bare surface
!             and vegetated surface equations  ---
!
              IF( ICM_SFC(1,NSCN).EQ.NX ) THEN
!
!---            Consider surface cover equation limits  ---
!
                MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(1,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(1,NSCN)))
                MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
              ENDIF
!
!---          Loop over surface cover connection map  ---
!
              DO NSCC = 2,NSFCC
                IF( ICM_SFC(NSCC,NSCN).EQ.0 ) EXIT
!
!---            Node connected to surface cover, consider vegetated
!               surface equations  ---
!
                IF( ICM_SFC(NSCC,NSCN).EQ.NX ) THEN
!
!---              Consider surface cover equation limits  ---
!
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(3,NSCN)),
     &              ABS(IM(ISVC,NP)-JM_SFC(3,NSCN)))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &              ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
                ENDIF
              ENDDO
            ENDDO
  350     CONTINUE
  360   CONTINUE
!
!---  K,I,J ordering yields the lowest half band width   ---
!
      ELSE
!
!---    Equation ordering using K,I,J ordering, skipping inactive
!       nodes  ---
!
        NC = 0
        DO 410 J = 1,LFY
        DO 410 I = 1,LFX
        DO 410 K = 1,LFZ
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 410
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            IXP(N) = -(IRX*JRX*KRX)
            DO 402 JX = 1,JRX
            DO 402 IX = 1,IRX
            DO 402 KX = 1,KRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NC = NC + 1
              IXP(NX) = NC
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
!
!---    Coupled equations half band width using K,I,J ordering  ---
!
        DO 430 J = 1,LFY
        DO 430 I = 1,LFX
        DO 430 K = 1,LFZ
          N = ND(I,J,K)
!
!---      Loop over block refined nodes  ---
!
          IF( IBR(4,N).GT.N ) THEN
            IRX = 2**IBR(1,N)
            JRX = 2**IBR(2,N)
            KRX = 2**IBR(3,N)
            DO 414 IX = 1,IRX
            DO 414 KX = 1,KRX
            DO 414 JX = 1,JRX
              NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
              NMD = IXP(NX)
              DO 412 M = 1,ISVC
                NC = NC+1
                IM(M,NMD) = NC
  412         CONTINUE
!
!---          Loop over surface cover nodes  ---
!
              DO NSCN = 1,NSFCN
!
!---            Node adjacent to surface cover node  ---
!
                IF( ICM_SFC(1,NSCN).EQ.NX ) THEN
                  DO M = 1,5
                    NC = NC + 1
                    JM_SFC(M,NSCN) = NC
                  ENDDO
                ENDIF
              ENDDO
  414       CONTINUE
          ELSE
            NMD = IXP(N)
!
!---        Skip to next active node  ---
!
            IF( IXP(N).EQ.0 ) GOTO 430
            DO 416 M = 1,ISVC
              NC = NC+1
              IM(M,NMD) = NC
  416       CONTINUE
!
!---        Loop over surface cover nodes  ---
!
            DO NSCN = 1,NSFCN
!
!---          Node adjacent to surface cover node  ---
!
              IF( ICM_SFC(1,NSCN).EQ.N ) THEN
                DO M = 1,5
                  NC = NC + 1
                  JM_SFC(M,NSCN) = NC
                ENDDO
              ENDIF
            ENDDO
          ENDIF
  430   CONTINUE
!
!---    Determine matrix half-band widths using K,I,J ordering  ---
!
        DO 460 J = 1,LFY
        DO 460 I = 1,LFX
        DO 460 K = 1,LFZ
          N = ND(I,J,K)
!
!---      Skip to next active node  ---
!
          IF( IXP(N).EQ.0 ) GOTO 460
          IRX = 2**IBR(1,N)
          JRX = 2**IBR(2,N)
          KRX = 2**IBR(3,N)
          DO 450 KX = 1,KRX
          DO 450 JX = 1,JRX
          DO 450 IX = 1,IRX
            NX = IBR(4,N) + ND_BR(IX,IRX,JX,JRX,KX)
            NP = IXP(NX)
!
!---        Node  ---
!
            MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NP)))
!
!---        Bottom node neighbors  ---
!
            IF( K.GT.1 ) THEN
              IF( IXP(N-LFXY).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                DO 441 MX = 1,4
                  NB = ICM(MX,1,NX)
                  IF( NB.EQ.0 ) EXIT
                  NB = IXP(NB)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NB)),
     &              ABS(IM(ISVC,NP)-IM(1,NB)))
  441           CONTINUE
              ENDIF
            ENDIF
!
!---        South node neighbors  ---
!
            IF( J.GT.1 ) THEN
              IF( IXP(N-LFX).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                DO 442 MX = 1,4
                  NS = ICM(MX,2,NX)
                  IF( NS.EQ.0 ) EXIT
                  NS = IXP(NS)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NS)),
     &              ABS(IM(ISVC,NP)-IM(1,NS)))
  442           CONTINUE
              ENDIF
            ENDIF
!
!---        West node neighbors  ---
!
            IF( I.GT.1 ) THEN
              IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                DO 443 MX = 1,4
                  NW = ICM(MX,3,NX)
                  IF( NW.EQ.0 ) EXIT
                  NW = IXP(NW)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NW)),
     &              ABS(IM(ISVC,NP)-IM(1,NW)))
  443           CONTINUE
              ENDIF
            ENDIF
!
!---        East node neighbors  ---
!
            IF( I.LT.LFX ) THEN
              IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                DO 444 MX = 1,4
                  NE = ICM(MX,4,NX)
                  IF( NE.EQ.0 ) EXIT
                  NE = IXP(NE)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NE)),
     &              ABS(IM(ISVC,NP)-IM(1,NE)))
  444           CONTINUE
              ENDIF
            ENDIF
!
!---        North node neighbors  ---
!
            IF( J.LT.LFY ) THEN
              IF( IXP(N+LFX).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                DO 445 MX = 1,4
                  NN = ICM(MX,5,NX)
                  IF( NN.EQ.0 ) EXIT
                  NN = IXP(NN)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NN)),
     &               ABS(IM(ISVC,NP)-IM(1,NN)))
  445           CONTINUE
              ENDIF
            ENDIF
!
!---        Top node neighbors  ---
!
            IF( K.LT.LFZ ) THEN
              IF( IXP(N+LFXY).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                DO 446 MX = 1,4
                  NT = ICM(MX,6,NX)
                  IF( NT.EQ.0 ) EXIT
                  NT = IXP(NT)
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-IM(ISVC,NT)),
     &              ABS(IM(ISVC,NP)-IM(1,NT)))
  446           CONTINUE
              ENDIF
            ENDIF
!
!---        Loop over surface cover nodes  ---
!
            DO NSCN = 1,NSFCN
!
!---          Node adjacent to surface cover, consider bare surface
!             and vegetated surface equations  ---
!
              IF( ICM_SFC(1,NSCN).EQ.N ) THEN
!
!---            Consider surface cover equation limits  ---
!
                MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(1,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(1,NSCN)))
                MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &            ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
              ENDIF
!
!---          Loop over surface cover connection map  ---
!
              DO NSCC = 2,NSFCC
                IF( ICM_SFC(NSCC,NSCN).EQ.0 ) EXIT
!
!---            Node connected to surface cover, consider vegetated
!               surface equations  ---
!
                IF( ICM_SFC(NSCC,NSCN).EQ.NX ) THEN
!
!---              Consider surface cover equation limits  ---
!
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(3,NSCN)),
     &              ABS(IM(ISVC,NP)-JM_SFC(3,NSCN)))
                  MHBC = MAX(MHBC,ABS(IM(1,NP)-JM_SFC(5,NSCN)),
     &              ABS(IM(ISVC,NP)-JM_SFC(5,NSCN)))
                ENDIF
              ENDDO
            ENDDO
  450     CONTINUE
  460   CONTINUE
      ENDIF
      LHBW = MHBC
!
!---  Deallocate memory for the Jacobian matrix pointer array
!     for surface cover equations  ---
!
      IF( ALLOCATED(JM_SFC) ) THEN
        DEALLOCATE( JM_SFC,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: JM_SFC'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF

!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCB_SURF_COV group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE L_CASE( CHDUM )
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
!---------------------!opyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Convert all upper-case characters in a variable-length string
!     variable to lower case.  This subroutine does not disturb
!     non-alphabetic characters; only captial letters
!     (ASCII 65 through 90) are modified.
!
!----------------------Authors-----------------------------------------!
!
!     Written by WE Nichols, Battelle, March, 1991.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*(*) CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/L_CASE'
      DO 10 N = 1,LEN(CHDUM)
        M = ICHAR(CHDUM(N:N))
        IF( M .GE. 65 .AND. M .LE. 90 ) THEN
          M = M + 32
          CHDUM(N:N) = CHAR(M)
        ENDIF
   10 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of L_CASE group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE LOC_PT( X0,Y0,X,Y,N,IINOUT,IPATH )
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
!     LOC_PT locates whether a point is inside a polygon in a 2D plane
!
!
!     Method:
!
!     Given a polygonal line connecting the vertices (x(i),y(i)) (i = 1,...,n)
!     taken in this order.  It is assumed that the polygonal path is a loop,
!     where (x(n),y(n)) = (x(1),y(1)) or there is an arc from (x(n),y(n)) to
!     (x(1),y(1)). The polygon may cross itself any number of times.
!
!     Reference:
!
!     Fortran 66 version by A.H. Morris
!     Converted to ELF90 compatibility by Alan Miller, 15 February 1997
!     Available at http://jblevins.org/mirror/amiller/    
!
!     Author:
!
!     John Burkardt
!
!     Parameters:
!
!     Input, real X0, Y0, the coordinate of the point.
!     Input, real X(N), Y(N), the coordinates of the points defining the polygon.
!     Input, integer N, the number of the points defining the polygon.
! 
!     Output, integer IINOUT, assigned as follows:
!             L = -1   if (X0,Y0) is outside the polygonal path
!             L =  0   if (X0,Y0) lies on the polygonal path
!             L =  1   if (X0,Y0) is inside the polygonal path
!
!     Output, integer IPATH, where IPATH = 0 if (X0,Y0) is on or outside the path. 
!                        if (X0,Y0) is inside the path then m is the winding 
!                        number of the path around the point (X0,Y0).
!
!----------------------Authors-----------------------------------------!
!
!     Written by SK White, PNNL, 15 September 2015.
!

!----------------------Fortran 90 Modules------------------------------!
!
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
      REAL*8 X(N),Y(N)
!      
!----------------------Executable Lines--------------------------------!
!      
!
!-----------------------------------------------------------------------
      EPSL = 1.D-14
      N0 = N
      IF (X(1) == X(N) .AND. Y(1) == Y(N)) N0 = N - 1
      PIX = ATAN2(0.D+0,-1.D+0)
      PI2 = 2.D+0 * PIX
      TOL = 4.D+0*EPSL*PIX
      IINOUT = -1
      IPATH = 0
!      
      U = X(1) - X0
      V = Y(1) - Y0
      IF (U .EQ. 0.D+0 .AND. V .EQ. 0.D+0) GO TO 20
      IF (N0 .LT. 2) RETURN
      THETA1 = ATAN2(V, U)
!      
      SUM = 0.0
      THETA = THETA1
      DO I = 2, N0
        U = X(I) - X0
        V = Y(I) - Y0
        IF (U .EQ. 0.D+0 .AND. V .EQ. 0.D+0) GO TO 20
        THETAI = ATAN2(V, U)
!        
        ANGLE = ABS(THETAI - THETA)
        IF (ABS(ANGLE - PIX) .LT. TOL) GO TO 20
        IF (ANGLE .GT. PIX) ANGLE = ANGLE - PI2
        IF (THETA .GT. THETAI) ANGLE = -ANGLE
        SUM = SUM + ANGLE
        THETA = THETAI
      END DO
!      
      ANGLE = ABS(THETA1 - THETA)
      IF (ABS(ANGLE - PIX) .LT. TOL) GO TO 20
      IF (ANGLE .GT. PIX) ANGLE = ANGLE - PI2
      IF (THETA .GT. THETA1) ANGLE = -ANGLE
      SUM = SUM + ANGLE
!      
!     SUM = 2*PIX*IPATH WHERE IPATH IS THE WINDING NUMBER
!      
      IPATH = INT( ABS(SUM)/PI2 + 0.2D+0 )
      IF (IPATH .EQ. 0) RETURN
      IINOUT = 1
      IF (SUM .LT. 0.D+0) M = -1 * IPATH
      RETURN
!      
!     (X0, Y0) IS ON THE BOUNDARY OF THE PATH
!      
   20 IINOUT = 0
!
!---  End of LOC_PT group ---
!
      RETURN
      END
      
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE LU_DCMP( A,N,NP,IX,D )
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
!     Written by Mark White, PNNL, August 1, 2000.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE CONST
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 A(NP,NP),VV(NP)
      INTEGER IX(NP)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/LU_DCMP'
      D = 1.D+0
      DO 12 I = 1,N
        AAMAX = 0.D+0
        DO 11 J = 1,N
          IF( ABS(A(I,J)).GT.AAMAX ) AAMAX = ABS(A(I,J))
   11   CONTINUE
        IF( ABS(AAMAX)/EPSL.LT.EPSL ) THEN
          INDX = 20
          CHMSG = 'Singular Matrix: '
          IMSG = NP
          CALL WRMSGP( INDX )
          ICUTTS = 1
        ENDIF
        VV(I) = 1.D+0/AAMAX
   12 CONTINUE
      IMAX = 0
      DO 19 J = 1,N
        DO 14 I = 1,J-1
          SUM = A(I,J)
          DO 13 K = 1,I-1
            SUM = SUM - A(I,K)*A(K,J)
   13     CONTINUE
          A(I,J) = SUM
   14   CONTINUE
        AAMAX = 0.D+0
        DO 16 I = J,N
          SUM = A(I,J)
          DO 15 K = 1,J-1
            SUM = SUM - A(I,K)*A(K,J)
   15     CONTINUE
          A(I,J) = SUM
          DUM = VV(I)*ABS(SUM)
          IF( DUM.GE.AAMAX ) THEN
            IMAX = I
            AAMAX = DUM
          ENDIF
   16   CONTINUE
        IF( J.NE.IMAX ) THEN
          DO 17 K = 1,N
            DUM = A(IMAX,K)
            A(IMAX,K) = A(J,K)
            A(J,K) = DUM
   17     CONTINUE
          D = -D
          VV(IMAX) = VV(J)
        ENDIF
        IX(J) = IMAX
        IF( ABS(A(J,J))/EPSL.LT.EPSL ) A(J,J) = 1.D-30
        IF( J.NE.N ) THEN
          DUM = 1.D+0/A(J,J)
          DO 18 I = J+1,N
            A(I,J) = A(I,J)*DUM
   18     CONTINUE
        ENDIF
   19 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of LU_DCMP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE LU_BKSB( A,N,NP,IX,B )
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
!     Written by Mark White, PNNL, August 1, 2000.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE CONST
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 A(NP,NP),B(NP)
      INTEGER IX(NP)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/LU_BKSB'
      II = 0
      DO 12 I = 1,N
        IL = IX(I)
        SUM = B(IL)
        B(IL) = B(I)
        IF( II.NE.0 ) THEN
          DO 11 J = II,I-1
            SUM = SUM - A(I,J)*B(J)
   11     CONTINUE
        ELSEIF( ABS(SUM)/EPSL.GT.EPSL ) THEN
          II = I
        ENDIF
        B(I) = SUM
   12 CONTINUE
      DO 14 I = N,1,-1
        SUM = B(I)
        IF( I.LT.N ) THEN
          DO 13 J = I+1,N
            SUM = SUM - A(I,J)*B(J)
   13     CONTINUE
        ENDIF
        B(I) = SUM/A(I,I)
   14 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of LU_BKSB group  ---
!
      RETURN
      END

!------------------------Function--------------------------------------!
!
      FUNCTION ND_BR( IX,IRX,JX,JRX,KX )
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
!     Block refined node index
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 28 January.
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER ND_BR
!
!----------------------Executable Lines--------------------------------!
!
      ND_BR = (KX-1)*IRX*JRX + (JX-1)*IRX + IX - 1
!
!---  End of ND_BR group  ---
!
      RETURN
      END
      
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE PG_CNTRD ( N,PX,PY,PZ,CX,CY,CZ )
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
!     PG_CNTRD computes the centroid of a polygon in 3D.
!
!
!     Method:
!
!     The centroid is the area-weighted sum of the centroids of
!     disjoint triangles that make up the polygon.
!
!     Reference:
!
!     Adrian Bowyer and John Woodwark,
!     A Programmer's Geometry,
!     Butterworths, 1983.
!
!     Author:
!
!     John Burkardt
!
!     Parameters:
!
!     Input, integer N, the number of vertices of the polygon.
!
!     Input, real X(N), Y(N), Z(N), the coordinates of the vertices.
! 
!     Output, real CX, CY, CZ, the coordinates of the centroid.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 March 2006.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
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
      REAL*8 AREA,AREAT
      REAL*8 CX,CY,CZ
      REAL*8 PX(N),PY(N),PZ(N)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/PG_CNTRD'
      AREA = 0.D+0
      CX = 0.D+0
      CY = 0.D+0
      CZ = 0.D+0
      DO 100 I = 1,N-2
        CALL TRG_AREA ( PX(1),PY(1),PZ(1),PX(I+1),
     &    PY(I+1),PZ(I+1),PX(I+2),PY(I+2),PZ(I+2),AREAT )
        AREA = AREA + AREAT
        CX = CX + AREAT*( PX(1)+PX(I+1)+PX(I+2) )/3.D+0
        CY = CY + AREAT*( PY(1)+PY(I+1)+PY(I+2) )/3.D+0
        CZ = CZ + AREAT*( PZ(1)+PZ(I+1)+PZ(I+2) )/3.D+0
  100 CONTINUE
      CX = CX/AREA
      CY = CY/AREA
      CZ = CZ/AREA
      ISUB_LOG = ISUB_LOG-1
!
!---  End of PG_CNTRD group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_AQSP
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
!     Read aqueous reaction species.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 8 December 2004.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
      USE REACT
      USE PTZR
      USE PTZRCOEF
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM,ADUM,UNTS
      REAL(KIND=DP) VAR
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_AQSP'
!
!---  Assign card string  ---
!
      CARD = 'Aqueous Species Card'
!
!---  Read number of aqueous species  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Aqueous Species'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NSPL)
      LSPL = MAX( LSPL,NSPL )
!
!---  First check for Molecular diffusion option  ---
!
      VARB = 'Aqueous Species Molecular Diffusion Option'
      IVR = INDEX( VARB,'  ')-1
      CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
      IF( INDX.EQ.0 ) THEN
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      ENDIF
!
!---  Check for Pitzer activity coefficient option  ---
!
      CALL RD_DPR(ISTART,ICOMMA,CHDUM,SP_MDL)
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)

      CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
      IF( INDX.EQ.1 ) THEN
        VARB = 'Activity Coefficient Option: '
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'pitzer').NE.0 ) THEN
!
!---  Loop over the aqueous species  ---
!
        LMCG = 0
        LCAT = 0
        LANI = 0
        LNEU = 0
        DO 500 NSP = 1,NSPL
          CALL RD_INPL( CHDUM )
          CALL L_CASE( CHDUM )
          ISTART = 1
          VARB = 'Aqueous Species Name: '
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          VARB = 'Charge'
          VAR = 0.0
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          IF (VAR > 0) THEN
            LCAT = LCAT+1
          ELSEIF(VAR < 0) THEN
            LANI = LANI+1
          ELSE
            LNEU = LNEU+1
          ENDIF
          LMCG = MAX(LMCG,INT(ABS(VAR)))
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)

 500    CONTINUE
        ENDIF
      ENDIF

      LNAF = LANI*(LANI-1)+1
      LNCF = LCAT*(LCAT-1)+1
      LNNC = MAX(LCAT,LNEU)
      LNNA = MAX(LNEU,LANI)
      LNNF = MAX(LNNC,LNNA)
      LNNF = LNNF*LNEU
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_AQSP group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_ATMOS
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
!     Read Atmospheric Conditions Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 29 October 2003.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE TRNSPT
      USE SOLTN
      USE PLT_ATM
      USE GRID
      USE FILES
      USE CONST
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
      CHARACTER*128 ADUM,FDUM,CDUM
      LOGICAL FCHK
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_ATMOS'
!
!---  Assign card string  ---
!
      CARD = 'Atmospheric Conditions Card'
!
!---  Skip over the static atmospheric conditions card inputs  ---
!
      CALL RD_INPL( CHDUM )
!
!---  Read number of atmospheric condition times  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Input Option [File, Integer]'
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---  Read atmospheric conditions from an external file  ---
!
      IF( INDEX(ADUM,'file').NE.0 ) THEN
        VARB = 'External Atmospheric Conditions File Name'
        CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
        INQUIRE( FILE=FDUM(1:NCHF), FORM=CDUM, EXIST=FCHK )
        IF( .NOT.FCHK ) THEN
          INDX = 4
          CHMSG = 'Atmospheric conditions file does not exist: '
     &      // FDUM(1:NCHF)
          CALL WRMSGP( INDX )
        ELSEIF( CDUM.EQ.'UNFORMATTED' ) THEN
          INDX = 4
          CHMSG = 'Atmospheric conditions file is unformatted: '
     &      // FDUM(1:NCHF)
          CALL WRMSGP( INDX )
        END IF
        OPEN(UNIT=27, FILE=FDUM(1:NCHF), STATUS='OLD', FORM='FORMATTED')
        READ(27,*,END=10) NATM
        CLOSE(UNIT=27)
!
!---    Cyclic atmospheric conditions  ---
!
        IF( NATM.LE.-3 ) THEN
          NATM = ABS(NATM)
!
!---    Empty atmospheric conditions file  ---
!
        ELSEIF( NATM.EQ.0 ) THEN
          INDX = 2
          CHMSG = 'No Atmospheric Condition Times'
          CALL WRMSGP( INDX )
!
!---    Ill-defined cyclic atmospheric conditions  ---
!
        ELSEIF( NATM.GT.-3 .AND. NATM.LT.0 ) THEN
          INDX = 4
          CHMSG = 'Number of Cyclic Atmospheric Conditions Times < 3'
          CALL WRMSGP( INDX )
        ENDIF
        LATM = MAX( LATM,NATM )
        GOTO 20
   10   CONTINUE
        INDX = 4
        CHMSG = 'Empty Atmospheric Conditions File'
        CALL WRMSGP( INDX )
   20   CONTINUE
!
!---  Read atmospheric conditions from input file  ---
!
      ELSE
        ISTART = 1
        VARB = 'Number of Atmospheric Condition Times'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,NATM)
        LATM = MAX( LATM,NATM )
      ENDIF
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_ATMOS group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_BALA
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
!     Read Balance Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by WE Nichols, PNNL, 13 June 2003.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_BALA'
!
!---  Assign card string  ---
!
      CARD = 'Balance Card'
!
!---  Grid dimension parameters  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Mass Balance Report Times'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,LBAL)
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_BALA group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_BC
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
!     Read Boundary Conditions Card for number of boundary conditions
!     and number of boundary condition times.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 10 October 2002.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
      CHARACTER*128 ADUM,BDUM,CDUM,FDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_BC'
!
!---  Assign card string  ---
!
      CARD = 'Boundary Conditions Card'
!
!---  Read number of boundary condition inputs  ---
!
      LBC = 1
      LBCIN = 1
      LBTM = 1
      NBC = 0
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Boundary Condition Inputs: '
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
      LBCIN = MAX( LBCIN,NLIN )
      LXYZG = 0
      DO 400 NB = 1, NLIN
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
!
!---    Read boundary orientation  ---
!
        VARB = 'Boundary Condition Orientation: '
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'west').NE.0 ) THEN
          IBCDX = -1
        ELSEIF( INDEX(ADUM(1:),'east').NE.0 ) THEN
          IBCDX = 1
        ELSEIF( INDEX(ADUM(1:),'south').NE.0 ) THEN
          IBCDX = -2
        ELSEIF( INDEX(ADUM(1:),'north').NE.0 ) THEN
          IBCDX = 2
        ELSEIF( INDEX(ADUM(1:),'bottom').NE.0 ) THEN
          IBCDX = -3
        ELSEIF( INDEX(ADUM(1:),'top').NE.0 ) THEN
          IBCDX = 3
        ELSEIF( INDEX(ADUM(1:),'file').NE.0 ) THEN
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
          NCH = INDEX(FDUM,'  ')-1
          OPEN(UNIT=27,FILE=FDUM(1:NCH),STATUS='OLD',FORM='FORMATTED')
          I1X = 1
          I2X = 1
          J1X = 1
          J2X = 1
          K1X = 1
          K2X = 0
    5     CONTINUE
          READ(27,*,END=10) IX,JX,KX,IBCDX
          K2X = K2X+1
          GOTO 5
   10     CONTINUE
          REWIND(27)
        ENDIF
!
!---    Loop over coupled flow and transport boundary types  ---
!
        NBTX = LD+LG+LL+LN+LS+LT-ISLC(30)-ISLC(46)
        IF( IOM.EQ.50 .OR. IOM.EQ.51 .OR. IOM.EQ.52 ) NBTX = 3
!
!---    STOMP-EOR  ---
!
        IF( IOM.EQ.43 ) THEN
          NBTX = 2
!
!---      Isothermal option  ---
!
          IF( ISLC(30).EQ.0 ) NBTX = 3
        ENDIF
!
!---    STOMP-HYDT-KE  ---
!
        IF( IOM.EQ.37 .OR. IOM.EQ.39 ) THEN
          NBTX = 2
!
!---      Isobrine option  ---
!
          IF( ISLC(32).EQ.0 ) NBTX = 3
        ENDIF
!
!---    STOMP-CO2e  ---
!
        IF( IOM.EQ.33 ) THEN
!
!---      Isothermal option  ---
!
          IF( ISLC(30).EQ.1 ) NBTX = NBTX - 1
!
!---      Isobrine option  ---
!
          IF( ISLC(32).EQ.1 ) NBTX = NBTX - 1
        ENDIF
!
!---     STOMP-GT  ---
!
        IF( IOM.EQ.3 ) THEN
!
!---      Isobrine option  ---
!
          IF( ISLC(32).EQ.0 ) NBTX = NBTX - 1
        ENDIF
!

!
!---    Loop over the number of boundary condition types  ---
!
        DO 50 NBT = 1,NBTX
!
!---      Check for Shuttleworth-Wallace boundary conditions  ---
!
          VARB = 'Boundary Condition Type'
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
          IF( INDEX(BDUM(1:),'shuttleworth').NE.0 .OR.
     &      ((INDEX(BDUM(1:),'shuttle').NE.0 ) .AND.
     &      (INDEX(BDUM(1:),'worth').NE.0 )) .OR.
     &      INDEX(BDUM(1:),'wallace').NE.0 ) LSW = 1
          IF( INDEX(BDUM(1:),'x-y-z').NE.0 .AND.
     &      ( INDEX(BDUM(1:),'hydraulic gradient').NE.0 .OR.
     &        INDEX(BDUM(1:),'seepage face').NE.0 ) ) LXYZG = 1
!
!---      STOMP-EOR  ---
!
          IF( IOM.EQ.43 .AND. INDEX(BDUM(1:),'bc1').NE.0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
            CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
          ELSEIF( IOM.EQ.43 .AND. INDEX(BDUM(1:),'bc2').NE.0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
          ENDIF
!
!---      STOMP-CO2  ---
!
          IF( IOM.EQ.32 .AND. INDEX(BDUM(1:),'hydrostatic').NE.0 ) EXIT
!
!---      STOMP-CO2e  ---
!
          IF( IOM.EQ.33 .AND. INDEX(BDUM(1:),'hydrostatic').NE.0 ) EXIT
!
!---      STOMP-GT  ---
!
          IF( IOM.EQ.3 .AND. INDEX(BDUM(1:),'neumann').NE.0 ) EXIT
          IF( IOM.EQ.3 .AND. INDEX(BDUM(1:),'barometric').NE.0 ) EXIT
   50   CONTINUE
!
!---    Loop over solute and reactive species boundary types,
!       allowing for returns in input lines  ---
!
        NBTX = (LSOLU*LC)+((LL+LG+LN)*LR)
        DO 60 NBT = 1,NBTX
          VARB = 'Solute or Reactive Species Boundary Condition Type'
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RD_INPL( CHDUM )
            CALL L_CASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
   60   CONTINUE
!
!---    Read number of reactive species in boundary
!       condition  ---
!
        NBCSPX = 0
        IF( LR.EQ.1 ) THEN
          CALL RD_INPL( CHDUM )
          CALL L_CASE( CHDUM )
          ISTART = 1
          CALL RD_INT(ISTART,ICOMMA,CHDUM,NBCSPX)
          LSPBC = MAX( LSPBC,NBCSPX )
!
!---      Loop over reactive species, allowing for returns
!         in the input lines  ---
!
          DO 70 NSPX = 1,NBCSPX
            VARB = 'Boundary Condition Species Name'
!
!---        Allow for returns in input lines  ---
!
            CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              CALL RD_INPL( CHDUM )
              CALL L_CASE( CHDUM )
              ISTART = 1
            ENDIF
            CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
   70     CONTINUE
        ENDIF
!
!---    Read and write boundary domain indices  ---
!
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
        IF( INDEX(ADUM(1:),'file').EQ.0 ) THEN
          VARB = 'Boundary Condition Domain: '
          CALL RD_INT(ISTART,ICOMMA,CHDUM,I1X)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,I2X)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,J1X)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,J2X)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,K1X)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,K2X)
!
!---  Check boundary domain  ---
!
          IF( I1X.GT.I2X .OR. J1X.GT.J2X .OR. K1X.GT.K2X ) THEN
            INDX = 4
            CHMSG = 'Nonascending Boundary Condition Domain Indices'
            CALL WRMSGP( INDX )
          ENDIF
        ENDIF
!
!---  Read number of boundary times  ---
!
        VARB = 'Number of Boundary Condition Times: '
        CALL RD_INT(ISTART,ICOMMA,CHDUM,IBCMX)
        IF( IBCMX.LE.-3 ) THEN
          IBCCX = 1
          IBCMX = -IBCMX
        ELSEIF( IBCMX.GE.1 ) THEN
          IBCCX = 0
        ELSEIF( IBCMX.EQ.0 ) THEN
          INDX = 4
          CHMSG = 'No Boundary Condition Times'
          CALL WRMSGP( INDX )
        ELSE
          INDX = 4
          CHMSG = 'Number of Cyclic Boundary Conditions Times < 3'
          CALL WRMSGP( INDX )
        ENDIF
        LBTM = MAX( LBTM,IBCMX )
!
!---    Skip over the boundary condition variables and units  ---
!
        DO 100 NTM = 1,IBCMX
          CALL RD_INPL( CHDUM )
          CALL L_CASE( CHDUM )
          ISTART = 1
!
!---      Check for external boundary condition time file  ---
!
          IF( NTM.EQ.1 ) CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,CDUM)
          CALL L_CASE( CDUM )
          IF( INDEX(CDUM(1:),'file').NE.0 ) GOTO 110
!
!---      Read gas component boundary condition line  ---
!
          IF( IOM.EQ.30 .OR. IOM.EQ.40 .OR. IOM.EQ.50 .OR. 
     &      IOM.EQ.51 .OR. IOM.EQ.52 ) THEN
            CALL RD_INPL( CHDUM )
            CALL L_CASE( CHDUM )
            ISTART = 1
          ENDIF
!
!---      Loop over reactive species inputs, allowing for
!         returns in input lines  ---
!      
          DO 90 NSPX = 1,NBCSPX
            IF( NSPX.EQ.1 ) THEN
              CALL RD_INPL( CHDUM )
              CALL L_CASE( CHDUM )
              ISTART = 1
            ENDIF
!
!---        Allow for returns in input lines  ---
!
            CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              CALL RD_INPL( CHDUM )
              CALL L_CASE( CHDUM )
              ISTART = 1
            ENDIF
            CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
            CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
   90     CONTINUE
  100   CONTINUE
  110   CONTINUE
!
!---    Compute the number of boundary surfaces  ---
!
        NBC = NBC + (K2X-K1X+1)*(J2X-J1X+1)*(I2X-I1X+1)
        LBC = MAX( LBC,NBC )
        IF( INDEX(ADUM(1:),'file').NE.0 ) CLOSE(UNIT=27)
  400 CONTINUE
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_BC group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_BR
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
!     Read Block Refinement Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 18 January 2014.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE GRID
      USE GEOMECH
      USE FILES
      USE CONST
      USE BCV
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 FDUM,FMDUM,UNTS
      CHARACTER*128 ADUM
      CHARACTER*512 CHDUM
      LOGICAL FCHK
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER, DIMENSION(:), ALLOCATABLE :: IXP_TMP
      INTEGER, DIMENSION(:), ALLOCATABLE :: ID_TMP,JD_TMP,KD_TMP
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: XE_TMP,YE_TMP,ZE_TMP
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_BR'
!
!---  Assign card string  ---
!
      CARD = 'Block Refinement Card'
!
!---  Read number of grid refinement domains  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Grid Refinement Domains'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NDOMX)
!
!---  Loop over number of grid refinement domains  ---
!
      MXBRFX = 0
      MXBRFY = 0
      MXBRFZ = 0
      DO 100 NX = 1,NDOMX     
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
        VARB = 'Grid Refinement Domain I-Start Index'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,I1X)
        VARB = 'Grid Refinement Domain I-End Index'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,I2X)
        VARB = 'Grid Refinement Domain J-Start Index'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,J1X)
        VARB = 'Grid Refinement Domain J-End Index'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,J2X)
        VARB = 'Grid Refinement Domain K-Start Index'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,K1X)
        VARB = 'Grid Refinement Domain K-End Index'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,K2X)
        IBRFX = 0
        IBRFY = 0
        IBRFZ = 0
   10   CONTINUE
        VARB = 'Grid Refinement Level'
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'1x').NE.0 ) THEN
          IBRFX = 1
        ELSEIF( INDEX(ADUM(1:),'2x').NE.0 ) THEN
          IBRFX = 2
        ELSEIF( INDEX(ADUM(1:),'3x').NE.0 ) THEN
          IBRFX = 3
        ELSEIF( INDEX(ADUM(1:),'1y').NE.0 ) THEN
          IBRFY = 1
        ELSEIF( INDEX(ADUM(1:),'2y').NE.0 ) THEN
          IBRFY = 2
        ELSEIF( INDEX(ADUM(1:),'3y').NE.0 ) THEN
          IBRFY = 3
        ELSEIF( INDEX(ADUM(1:),'1z').NE.0 ) THEN
          IBRFZ = 1
        ELSEIF( INDEX(ADUM(1:),'2z').NE.0 ) THEN
          IBRFZ = 2
        ELSEIF( INDEX(ADUM(1:),'3z').NE.0 ) THEN
          IBRFZ = 3
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Grid Refinement Level: ' // ADUM
          CALL WRMSGP( INDX )
        ENDIF
        IF( LFX.EQ.1 .AND. IBRFX.GT.0 ) THEN
          INDX = 4
          CHMSG = 'X-Grid Refinement for 1D X-Grid Domain: ' // ADUM
          CALL WRMSGP( INDX )
        ENDIF
        IF( LFY.EQ.1 .AND. IBRFY.GT.0 ) THEN
          INDX = 4
          CHMSG = 'Y-Grid Refinement for 1D Y-Grid Domain: ' // ADUM
          CALL WRMSGP( INDX )
        ENDIF
        IF( LFZ.EQ.1 .AND. IBRFZ.GT.0 ) THEN
          INDX = 4
          CHMSG = 'Z-Grid Refinement for 1D Z-Grid Domain: ' // ADUM
          CALL WRMSGP( INDX )
        ENDIF
        CALL CHK_CHR(ISTART,ICOMMA,CHDUM,INDX)
        IF( INDX.EQ.1 ) GOTO 10 
        I1X = MAX( 1,I1X )
        I1X = MIN( I1X,I2X,LFX )
        I2X = MAX( 1,I1X,I2X )
        I2X = MIN( I2X,LFX )
        J1X = MAX( 1,J1X )
        J1X = MIN( J1X,J2X,LFY )
        J2X = MAX( 1,J1X,J2X )
        J2X = MIN( J2X,LFY )
        K1X = MAX( 1,K1X )
        K1X = MIN( K1X,K2X,LFZ )
        K2X = MAX( 1,K1X,K2X )
        K2X = MIN( K2X,LFZ )
!
!---    Assign grid refinement indexing  ---
!
        DO 20 K = K1X,K2X
        DO 20 J = J1X,J2X
        DO 20 I = I1X,I2X
          N = ND(I,J,K)
!
!---      Skip inactive nodes  ---
!
          IF( IXP(N).EQ.0 ) GOTO 20
          IBR(1,N) = IBRFX
          IBR(2,N) = IBRFY
          IBR(3,N) = IBRFZ
   20   CONTINUE
        MXBRFX = MAX( MXBRFX,IBRFX )
        MXBRFY = MAX( MXBRFY,IBRFY )
        MXBRFZ = MAX( MXBRFZ,IBRFZ )
  100 CONTINUE
!!
!!---  Re-assign grid refinement indexing at node, honoring
!!     boundaries (boundary downgrade sweep)  ---
!!
!      DO 200 K = 1,LFZ
!      DO 200 J = 1,LFY
!      DO 200 I = 1,LFX
!        N = ND(I,J,K)
!!
!!---    Skip inactive nodes  ---
!!
!        IF( IXP(N).EQ.0 ) GOTO 200
!!
!!---    Find nearest bottom boundary  ---
!!
!        IF( LFZ.GT.1 ) THEN
!          DO 110 KX = K,1,-1
!            IF( KX.EQ.1 ) THEN
!              KB = 1
!            ELSE
!              NB = ND(I,J,KX)
!              NBB = ND(I,J,KX)-LFXY
!              IF( IXP(NBB).EQ.0 .OR. INBS(1,NB).NE.0 ) THEN
!                KB = KX
!              ENDIF
!            ENDIF
!  110     CONTINUE
!          IBR(1,N) = MIN( IBR(1,N),(K-KB) )
!          IBR(2,N) = MIN( IBR(2,N),(K-KB) )
!          IBR(3,N) = MIN( IBR(3,N),(K-KB) )
!        ENDIF
!!
!!---    Find nearest south boundary  ---
!!
!        IF( LFY.GT.1 ) THEN
!          DO 120 JX = J,1,-1
!            IF( JX.EQ.1 ) THEN
!              JS = 1
!            ELSE
!              NS = ND(I,JX,K)
!              NSS = ND(I,JX,K)-LFX
!              IF( IXP(NSS).EQ.0 .OR. INBS(2,NS).NE.0 ) THEN
!                JS = JX
!              ENDIF
!            ENDIF
!  120     CONTINUE
!          IBR(1,N) = MIN( IBR(1,N),(J-JS) )
!          IBR(2,N) = MIN( IBR(2,N),(J-JS) )
!          IBR(3,N) = MIN( IBR(3,N),(J-JS) )
!        ENDIF
!!
!!---    Find nearest west boundary  ---
!!
!        IF( LFX.GT.1 ) THEN
!          DO 130 IX = I,1,-1
!            IF( IX.EQ.1 ) THEN
!              IW = 1
!            ELSE
!              NW = ND(IX,J,K)
!              NWW = ND(IX,J,K)-1
!              IF( IXP(NWW).EQ.0 .OR. INBS(3,NW).NE.0 ) THEN
!                IW = IX
!              ENDIF
!            ENDIF
!  130     CONTINUE
!          IBR(1,N) = MIN( IBR(1,N),(I-IW) )
!          IBR(2,N) = MIN( IBR(2,N),(I-IW) )
!          IBR(3,N) = MIN( IBR(3,N),(I-IW) )
!        ENDIF
!!
!!---    Find nearest east boundary  ---
!!
!        IF( LFX.GT.1 ) THEN
!          DO 140 IX = I,LFX
!            IF( IX.EQ.LFX ) THEN
!              IE = LFX
!            ELSE
!              NE = ND(IX,J,K)
!              NEE = ND(IX,J,K)+1
!              IF( IXP(NEE).EQ.0 .OR. INBS(4,NE).NE.0 ) THEN
!                IE = IX
!              ENDIF
!            ENDIF
!  140     CONTINUE
!          IBR(1,N) = MIN( IBR(1,N),(IE-I) )
!          IBR(2,N) = MIN( IBR(2,N),(IE-I) )
!          IBR(3,N) = MIN( IBR(3,N),(IE-I) )
!        ENDIF
!!
!!---    Find nearest north boundary  ---
!!
!        IF( LFY.GT.1 ) THEN
!          DO 150 JX = J,LFY
!            IF( JX.EQ.LFY ) THEN
!              JN = LFY
!            ELSE
!              NN = ND(I,JX,K)
!              NNN = ND(I,JX,K)+LFX
!              IF( IXP(NNN).EQ.0 .OR. INBS(5,NN).NE.0 ) THEN
!                JN = JX
!              ENDIF
!            ENDIF
!  150     CONTINUE
!          IBR(1,N) = MIN( IBR(1,N),(JN-J) )
!          IBR(2,N) = MIN( IBR(2,N),(JN-J) )
!          IBR(3,N) = MIN( IBR(3,N),(JN-J) )
!        ENDIF
!!
!!---    Find nearest top boundary  ---
!!
!        IF( LFZ.GT.1 ) THEN
!          DO 160 KX = K,LFZ
!            IF( KX.EQ.LFZ ) THEN
!              KT = LFZ
!            ELSE
!              NT = ND(I,J,KX)
!              NTT = ND(I,J,KX)+LFXY
!              IF( IXP(NTT).EQ.0 .OR. INBS(6,NT).NE.0 ) THEN
!                KT = KX
!              ENDIF
!            ENDIF
!  160     CONTINUE
!          IBR(1,N) = MIN( IBR(1,N),(KT-K) )
!          IBR(2,N) = MIN( IBR(2,N),(KT-K) )
!          IBR(3,N) = MIN( IBR(3,N),(KT-K) )
!        ENDIF
!  200 CONTINUE
!  
!---  Re-assign grid refinement indexing at node, honoring adjacent 
!     refinement levels (adjacent upgrade)  ---
!
      DO 310 M = 1,MAX(1,MXBRFX-1)
        DO 300 K = 1,LFZ
        DO 300 J = 1,LFY
        DO 300 I = 1,LFX
          N = ND(I,J,K)
!  
!---      Skip inactive nodes  ---
!  
          IF( IXP(N).EQ.0 ) GOTO 300
!  
!---      Bottom boundary  ---
!  
          IF( LFZ.GT.1 ) THEN
            IF( K.GT.1 ) THEN
              NB = N-LFXY
              IBR(1,N) = MAX( IBR(1,N),IBR(1,NB)-1 )
              IBR(2,N) = MAX( IBR(2,N),IBR(2,NB)-1 )
              IBR(3,N) = MAX( IBR(3,N),IBR(3,NB)-1 )
            ENDIF
          ENDIF
!  
!---      South boundary  ---
!  
          IF( LFY.GT.1 ) THEN
            IF( J.GT.1 ) THEN
              NS = N-LFX
              IBR(1,N) = MAX( IBR(1,N),IBR(1,NS)-1 )
              IBR(2,N) = MAX( IBR(2,N),IBR(2,NS)-1 )
              IBR(3,N) = MAX( IBR(3,N),IBR(3,NS)-1 )
            ENDIF
          ENDIF
!  
!---      West boundary  ---
!  
          IF( LFX.GT.1 ) THEN
            IF( I.GT.1 ) THEN
              NW = N-1
              IBR(1,N) = MAX( IBR(1,N),IBR(1,NW)-1 )
              IBR(2,N) = MAX( IBR(2,N),IBR(2,NW)-1 )
              IBR(3,N) = MAX( IBR(3,N),IBR(3,NW)-1 )
            ENDIF
          ENDIF
!  
!---      East boundary  ---
!  
          IF( LFX.GT.1 ) THEN
            IF( I.LT.LFX ) THEN
              NE = N+1
              IBR(1,N) = MAX( IBR(1,N),IBR(1,NE)-1 )
              IBR(2,N) = MAX( IBR(2,N),IBR(2,NE)-1 )
              IBR(3,N) = MAX( IBR(3,N),IBR(3,NE)-1 )
            ENDIF
          ENDIF
!  
!---      North boundary  ---
!  
          IF( LFY.GT.1 ) THEN
            IF( J.LT.LFY ) THEN
              NN = N+LFX
              IBR(1,N) = MAX( IBR(1,N),IBR(1,NN)-1 )
              IBR(2,N) = MAX( IBR(2,N),IBR(2,NN)-1 )
              IBR(3,N) = MAX( IBR(3,N),IBR(3,NN)-1 )
            ENDIF
          ENDIF
!  
!---      Top boundary  ---
!  
          IF( LFZ.GT.1 ) THEN
            IF( K.LT.LFZ ) THEN
              NT = N+LFXY
              IBR(1,N) = MAX( IBR(1,N),IBR(1,NT)-1 )
              IBR(2,N) = MAX( IBR(2,N),IBR(2,NT)-1 )
              IBR(3,N) = MAX( IBR(3,N),IBR(3,NT)-1 )
            ENDIF
          ENDIF
  300   CONTINUE
  310 CONTINUE
!  
!---  Refined node pointers  ---
!
      NC = LFD
      LRFN = 0
      DO 400 K = 1,LFZ
      DO 400 J = 1,LFY
      DO 400 I = 1,LFX
        N = ND(I,J,K)
        NX = 1
        IBRX = (2**IBR(1,N))
        JBRX = (2**IBR(2,N))
        KBRX = (2**IBR(3,N))
        IF( LFX.GT.1 ) NX = NX*IBRX
        IF( LFY.GT.1 ) NX = NX*JBRX
        IF( LFZ.GT.1 ) NX = NX*KBRX
        IF( NX.GT.1 ) THEN
          IBR(4,N) = NC+1
          IBR(5,N) = NC+NX
          NC = NC+NX
          LSRX = LSRX + (IBRX+1)*JBRX*KBRX
          LSRY = LSRY + IBRX*(JBRX+1)*KBRX
          LSRZ = LSRZ + IBRX*JBRX*(KBRX+1)
          LRFN = LRFN + 1
        ENDIF
  400 CONTINUE
      LBR = NC-LFD
      LAN = LAN + LBR - LRFN
!
!---  Allocate memory for temporary inactive nodes index  ---
!
      ALLOCATE( IXP_TMP(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IXP_TMP'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Allocate memory for temporary I-index nodes index  ---
!
      ALLOCATE( ID_TMP(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ID_TMP'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Allocate memory for temporary J-index nodes index  ---
!
      ALLOCATE( JD_TMP(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: JD_TMP'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Allocate memory for temporary K-index nodes index  ---
!
      ALLOCATE( KD_TMP(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: KD_TMP'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Allocate memory for temporary x-direction vertices  ---
!
      ALLOCATE( XE_TMP(1:8,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XE_TMP'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Allocate memory for temporary y-direction vertices  ---
!
      ALLOCATE( YE_TMP(1:8,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YE_TMP'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Allocate memory for temporary z-direction vertices  ---
!
      ALLOCATE( ZE_TMP(1:8,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZE_TMP'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Store inactive nodes array  ---
!
      DO 410 N = 1,LFD
        DO 402 M = 1,8
           XE_TMP(M,N) = XE(M,N)
           YE_TMP(M,N) = YE(M,N)
           ZE_TMP(M,N) = ZE(M,N)
  402   CONTINUE
        IXP_TMP(N) = IXP(N)
        ID_TMP(N) = ID(N)
        JD_TMP(N) = JD(N)
        KD_TMP(N) = KD(N)
  410 CONTINUE
!
!---  Deallocate memory for the inactive nodes index  ---
!
      DEALLOCATE( IXP,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: IXP'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Deallocate memory for the I-index nodes index  ---
!
      DEALLOCATE( ID,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: ID'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Deallocate memory for the J-index nodes index  ---
!
      DEALLOCATE( JD,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: JD'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Deallocate memory for the K-index nodes index  ---
!
      DEALLOCATE( KD,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: KD'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Deallocate memory for the x-direction vertices  ---
!
      DEALLOCATE( XE,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: XE'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Deallocate memory for the y-direction vertices  ---
!
      DEALLOCATE( YE,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: YE'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Deallocate memory for the z-direction vertices  ---
!
      DEALLOCATE( ZE,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: ZE'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Reallocate memory for the inactive nodes index to include
!     field nodes and block refinement nodes  ---
!
      ALLOCATE( IXP(1:LFD+LBR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IXP'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Reallocate memory for the I-index nodes index to include
!     field nodes and block refinement nodes  ---
!
      ALLOCATE( ID(1:LFD+LBR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ID'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Reallocate memory for the J-index nodes index to include
!     field nodes and block refinement nodes  ---
!
      ALLOCATE( JD(1:LFD+LBR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: JD'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Reallocate memory for the K-index nodes index to include
!     field nodes and block refinement nodes  ---
!
      ALLOCATE( KD(1:LFD+LBR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: KD'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Reallocate memory for the x-direction vertices to include
!     field nodes and block refinement nodes  ---
!
      ALLOCATE( XE(1:8,1:LFD+LBR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XE'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Reallocate memory for the y-direction vertices to include
!     field nodes and block refinement nodes  ---
!
      ALLOCATE( YE(1:8,1:LFD+LBR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YE'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Reallocate memory for the z-direction vertices to include
!     field nodes and block refinement nodes  ---
!
      ALLOCATE( ZE(1:8,1:LFD+LBR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZE'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Reassign inactive nodes array  ---
!
      DO 420 N = 1,LFD
        DO 412 M = 1,8
           XE(M,N) = XE_TMP(M,N)
           YE(M,N) = YE_TMP(M,N)
           ZE(M,N) = ZE_TMP(M,N)
  412   CONTINUE
        IXP(N) = IXP_TMP(N)
        ID(N) = ID_TMP(N)
        JD(N) = JD_TMP(N)
        KD(N) = KD_TMP(N)
  420 CONTINUE
!
!---  Deallocate temporary memory for the inactive nodes index  ---
!
      DEALLOCATE( IXP_TMP,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: IXP_TMP'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Deallocate temporary memory for the I-index nodes index  ---
!
      DEALLOCATE( ID_TMP,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: ID_TMP'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Deallocate temporary memory for the J-index nodes index  ---
!
      DEALLOCATE( JD_TMP,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: JD_TMP'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Deallocate temporary memory for the K-index nodes index  ---
!
      DEALLOCATE( KD_TMP,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: KD_TMP'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Deallocate temporary memory for the x-direction vertices  ---
!
      DEALLOCATE( XE_TMP,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: XE_TMP'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Deallocate temporary memory for the y-direction vertices  ---
!
      DEALLOCATE( YE_TMP,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: YE_TMP'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Deallocate temporary memory for the z-direction vertices  ---
!
      DEALLOCATE( ZE_TMP,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: ZE_TMP'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Define the grid pointers and refined grid vertices  ---
!
      DO 540 N = 1,LFD
        NBRX = IBR(4,N)
        IF( NBRX.GT.N ) THEN
          IBRX = 2**IBR(1,N)
          JBRX = 2**IBR(2,N)
          KBRX = 2**IBR(3,N)
          DO 530 K = 1,KBRX
          DO 520 J = 1,JBRX
          DO 510 I = 1,IBRX
            IXP(NBRX) = NBRX
            ID(NBRX) = I
            JD(NBRX) = J
            KD(NBRX) = K
!
!---        Vertices 1 and 2  ---
!
            X15X = XE(1,N) + (XE(5,N)-XE(1,N))*REAL(K-1)/REAL(KBRX)
            X37X = XE(3,N) + (XE(7,N)-XE(3,N))*REAL(K-1)/REAL(KBRX)
            X26X = XE(2,N) + (XE(6,N)-XE(2,N))*REAL(K-1)/REAL(KBRX)
            X48X = XE(4,N) + (XE(8,N)-XE(4,N))*REAL(K-1)/REAL(KBRX)
            XLX = X15X + (X37X-X15X)*REAL(J-1)/REAL(JBRX)
            XUX = X26X + (X48X-X26X)*REAL(J-1)/REAL(JBRX)
            XE(1,NBRX) = XLX + (XUX-XLX)*REAL(I-1)/REAL(IBRX)
            XE(2,NBRX) = XLX + (XUX-XLX)*REAL(I)/REAL(IBRX)
            Y15X = YE(1,N) + (YE(5,N)-YE(1,N))*REAL(K-1)/REAL(KBRX)
            Y37X = YE(3,N) + (YE(7,N)-YE(3,N))*REAL(K-1)/REAL(KBRX)
            Y26X = YE(2,N) + (YE(6,N)-YE(2,N))*REAL(K-1)/REAL(KBRX)
            Y48X = YE(4,N) + (YE(8,N)-YE(4,N))*REAL(K-1)/REAL(KBRX)
            YLX = Y15X + (Y37X-Y15X)*REAL(J-1)/REAL(JBRX)
            YUX = Y26X + (Y48X-Y26X)*REAL(J-1)/REAL(JBRX)
            YE(1,NBRX) = YLX + (YUX-YLX)*REAL(I-1)/REAL(IBRX)
            YE(2,NBRX) = YLX + (YUX-YLX)*REAL(I)/REAL(IBRX)
            Z15X = ZE(1,N) + (ZE(5,N)-ZE(1,N))*REAL(K-1)/REAL(KBRX)
            Z37X = ZE(3,N) + (ZE(7,N)-ZE(3,N))*REAL(K-1)/REAL(KBRX)
            Z26X = ZE(2,N) + (ZE(6,N)-ZE(2,N))*REAL(K-1)/REAL(KBRX)
            Z48X = ZE(4,N) + (ZE(8,N)-ZE(4,N))*REAL(K-1)/REAL(KBRX)
            ZLX = Z15X + (Z37X-Z15X)*REAL(J-1)/REAL(JBRX)
            ZUX = Z26X + (Z48X-Z26X)*REAL(J-1)/REAL(JBRX)
            ZE(1,NBRX) = ZLX + (ZUX-ZLX)*REAL(I-1)/REAL(IBRX)
            ZE(2,NBRX) = ZLX + (ZUX-ZLX)*REAL(I)/REAL(IBRX)
!
!---        Vertices 3 and 4  ---
!
            XLX = X15X + (X37X-X15X)*REAL(J)/REAL(JBRX)
            XUX = X26X + (X48X-X26X)*REAL(J)/REAL(JBRX)
            XE(3,NBRX) = XLX + (XUX-XLX)*REAL(I-1)/REAL(IBRX)
            XE(4,NBRX) = XLX + (XUX-XLX)*REAL(I)/REAL(IBRX)
            YLX = Y15X + (Y37X-Y15X)*REAL(J)/REAL(JBRX)
            YUX = Y26X + (Y48X-Y26X)*REAL(J)/REAL(JBRX)
            YE(3,NBRX) = YLX + (YUX-YLX)*REAL(I-1)/REAL(IBRX)
            YE(4,NBRX) = YLX + (YUX-YLX)*REAL(I)/REAL(IBRX)
            ZLX = Z15X + (Z37X-Z15X)*REAL(J)/REAL(JBRX)
            ZUX = Z26X + (Z48X-Z26X)*REAL(J)/REAL(JBRX)
            ZE(3,NBRX) = ZLX + (ZUX-ZLX)*REAL(I-1)/REAL(IBRX)
            ZE(4,NBRX) = ZLX + (ZUX-ZLX)*REAL(I)/REAL(IBRX)
!
!---        Vertices 5 and 6  ---
!
            X15X = XE(1,N) + (XE(5,N)-XE(1,N))*REAL(K)/REAL(KBRX)
            X37X = XE(3,N) + (XE(7,N)-XE(3,N))*REAL(K)/REAL(KBRX)
            X26X = XE(2,N) + (XE(6,N)-XE(2,N))*REAL(K)/REAL(KBRX)
            X48X = XE(4,N) + (XE(8,N)-XE(4,N))*REAL(K)/REAL(KBRX)
            XLX = X15X + (X37X-X15X)*REAL(J-1)/REAL(JBRX)
            XUX = X26X + (X48X-X26X)*REAL(J-1)/REAL(JBRX)
            XE(5,NBRX) = XLX + (XUX-XLX)*REAL(I-1)/REAL(IBRX)
            XE(6,NBRX) = XLX + (XUX-XLX)*REAL(I)/REAL(IBRX)
            Y15X = YE(1,N) + (YE(5,N)-YE(1,N))*REAL(K)/REAL(KBRX)
            Y37X = YE(3,N) + (YE(7,N)-YE(3,N))*REAL(K)/REAL(KBRX)
            Y26X = YE(2,N) + (YE(6,N)-YE(2,N))*REAL(K)/REAL(KBRX)
            Y48X = YE(4,N) + (YE(8,N)-YE(4,N))*REAL(K)/REAL(KBRX)
            YLX = Y15X + (Y37X-Y15X)*REAL(J-1)/REAL(JBRX)
            YUX = Y26X + (Y48X-Y26X)*REAL(J-1)/REAL(JBRX)
            YE(5,NBRX) = YLX + (YUX-YLX)*REAL(I-1)/REAL(IBRX)
            YE(6,NBRX) = YLX + (YUX-YLX)*REAL(I)/REAL(IBRX)
            Z15X = ZE(1,N) + (ZE(5,N)-ZE(1,N))*REAL(K)/REAL(KBRX)
            Z37X = ZE(3,N) + (ZE(7,N)-ZE(3,N))*REAL(K)/REAL(KBRX)
            Z26X = ZE(2,N) + (ZE(6,N)-ZE(2,N))*REAL(K)/REAL(KBRX)
            Z48X = ZE(4,N) + (ZE(8,N)-ZE(4,N))*REAL(K)/REAL(KBRX)
            ZLX = Z15X + (Z37X-Z15X)*REAL(J-1)/REAL(JBRX)
            ZUX = Z26X + (Z48X-Z26X)*REAL(J-1)/REAL(JBRX)
            ZE(5,NBRX) = ZLX + (ZUX-ZLX)*REAL(I-1)/REAL(IBRX)
            ZE(6,NBRX) = ZLX + (ZUX-ZLX)*REAL(I)/REAL(IBRX)
!
!---        Vertices 7 and 8  ---
!
            XLX = X15X + (X37X-X15X)*REAL(J)/REAL(JBRX)
            XUX = X26X + (X48X-X26X)*REAL(J)/REAL(JBRX)
            XE(7,NBRX) = XLX + (XUX-XLX)*REAL(I-1)/REAL(IBRX)
            XE(8,NBRX) = XLX + (XUX-XLX)*REAL(I)/REAL(IBRX)
            YLX = Y15X + (Y37X-Y15X)*REAL(J)/REAL(JBRX)
            YUX = Y26X + (Y48X-Y26X)*REAL(J)/REAL(JBRX)
            YE(7,NBRX) = YLX + (YUX-YLX)*REAL(I-1)/REAL(IBRX)
            YE(8,NBRX) = YLX + (YUX-YLX)*REAL(I)/REAL(IBRX)
            ZLX = Z15X + (Z37X-Z15X)*REAL(J)/REAL(JBRX)
            ZUX = Z26X + (Z48X-Z26X)*REAL(J)/REAL(JBRX)
            ZE(7,NBRX) = ZLX + (ZUX-ZLX)*REAL(I-1)/REAL(IBRX)
            ZE(8,NBRX) = ZLX + (ZUX-ZLX)*REAL(I)/REAL(IBRX)
            NBRX = NBRX+1
  510     CONTINUE
  520     CONTINUE
  530     CONTINUE
        ENDIF
  540 CONTINUE
!
!---  Include block refinement nodes in field node parameter  ---
!
      LFD = LFD + LBR
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_BR group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
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
!     Fill character string ADUM with characters between commas.
!     Return 'null' for null entries.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNNL, November 1992.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*(*) ADUM
      CHARACTER*(*) CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_CHR'
      IDFLTD = 0
      ICD = INDEX( CARD,'  ')-1
      IVR = INDEX( VARB,'  ')-1
      NCH = INDEX( ADUM,'  ')-1
!
!---  End of card record error ---
!
      IF( CHDUM(1:1) .EQ. '~' ) THEN
        INDX = 4
        CHMSG = 'End of Card Record: ' // VARB(1:IVR)
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Find next comma  ---
!
      ICOMMA = INDEX (CHDUM(ISTART:), ',') + ISTART - 1
      ISTOP = ICOMMA
  100 CONTINUE
!
!---  Comma not found, missing character string data  ---
!
      IF( ISTOP.LT.ISTART ) THEN
        INDX = 4
        CHMSG = 'Missing Character-String Record: ' // VARB(1:IVR)
        CALL WRMSGP( INDX )
!
!---  Null entry  ---
!
      ELSEIF( ISTOP.EQ.ISTART ) THEN
        IF( IDFLT .EQ. 0 ) THEN
          ADUM = 'null'
          NCH = 4
          IDFLTD = 1
        ENDIF
        ISTART = ICOMMA + 1
        ICOMMA = ISTART
!
!---  Characters between commas  ---
!
      ELSE
!
!---  Eliminate leading blank spaces  ---
!
        IF( ICHAR(CHDUM(ISTART:ISTART)).EQ.32 ) THEN
          ISTART = ISTART+1
          GOTO 100
        ENDIF
!
!---  Eliminate trailing blank spaces  ---
!
        ISTOP = ISTOP-1
  110   CONTINUE
        IF( ICHAR(CHDUM(ISTOP:ISTOP)).EQ.32 ) THEN
          ISTOP = ISTOP-1
          GOTO 110
        ENDIF
!
!---  Translate character string into a character string  ---
!
        ADUM = ' '
        NCH = ISTOP-ISTART+1
        READ (CHDUM(ISTART:ISTOP), '(A)') ADUM(1:NCH)
        ISTART = ICOMMA + 1
      ENDIF
      IDFLT = 0
!
!---  End of RD_CHR group  ---
!
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_CNEQ
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
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 9 December 2004.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
      USE REACT
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
      CHARACTER*128 ADUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_CNEQ'
!
!---  Assign card string  ---
!
      CARD = 'Conservation Equations Card'
!
!---  Read number of conservation equations  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Conservation Equations'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NEQC)
      LEQC = MAX( LEQC,NEQC )
      LSPT = LSPT + LEQC
!
!---  Loop over the conservation equations  ---
!
      DO 100 NEQ = 1,NEQC
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
        VARB = 'Component Species Name: '
        CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
        VARB = 'Number of Species in Conservation Equation'
        CALL RD_INT( ISTART,ICOMMA,CHDUM,NSEC )
        LSEC = MAX( LSEC,NSEC )
!
!---    Loop over the conservation species allowing for
!       returns in the input  ---
!
        DO 90 NSP = 1,NSEC
          VARB = 'Conservation-Equation Species Name: '
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RD_INPL( CHDUM )
            CALL L_CASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
          VARB = 'Conservation-Equation Species Coefficient: '
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RD_INPL( CHDUM )
            CALL L_CASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
   90   CONTINUE
  100 CONTINUE
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_CNEQ group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_COUP_WELL
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
!     Read Coupled Well Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 25 March 2011.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE JACOB
      USE GRID
      USE FILES
      USE COUP_WELL
      USE CONST
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
      CHARACTER*128 ADUM,BDUM
      CHARACTER*64 UNTS
      TYPE(LIST_INTERVAL), POINTER :: IN_TMP_PTR
      TYPE(LIST_TRANSITION), POINTER :: TP_TMP_PTR
      TYPE(LIST_NODE), POINTER :: LOC_PTR
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_COUP_WELL'
!
!---  Assign card string  ---
!
      CARD = 'Coupled Well Card'
!
!---  Read number of wells  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Coupled Wells'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,N_CW)
      LN_CW = MAX( LN_CW,N_CW )
      L_CW = 1
!
!---  Loop over number of coupled wells  ---
!
      NIT_CW = 0
      NULLIFY( IN_CW_PTR )
      NULLIFY( TP_CW_PTR )
      DO 500 NCW = 1,N_CW
!
!---    Read well type  ---
!
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
        VARB = 'Coupled Well Type'
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---    Check for injection wells with STOMP-EOR  ---
!
        IT_CWX = 0
        IF( IOM.EQ.43 ) THEN
          IF( INDEX(ADUM(1:),'injection').NE.0 ) IT_CWX = 1
          VARB = 'X-Well Fraction Factor'
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          VARB = 'Y-Well Fraction Factor'
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          VARB = 'Z-Well Fraction Factor'
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---      Check for solutes  ---
!
          IF( LC.NE.0 ) THEN
            NC = 0
            DO
!
!---          Check for solute names or well name  ---
!
              CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
              IF( INDX.EQ.0 ) EXIT
!
!---          Check for solute names  ---
!
              ADUM(1:) = ' '
              VARB = 'Solute Name'
              CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
              IFIND = 0
              LOC_PTR => SOLUT_PTR
              DO
                IF( .NOT.ASSOCIATED(LOC_PTR) ) EXIT
!
!---            Solute name found  ---
!
                IF( LOC_PTR%LIST_NAME == ADUM ) THEN
                  NC = NC + 1
                  IFIND = 1
                  EXIT
                ENDIF
                LOC_PTR => LOC_PTR%NEXT
              ENDDO
              IF( IFIND.EQ.0 ) EXIT
            ENDDO
            LSOLU_CW = MAX( LSOLU_CW,NC )
          ENDIF
        ENDIF
!
!---    Read number of well intervals  ---
!
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
        VARB = 'Number of Well Intervals'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,NI_CW)
        NIT_CW = NIT_CW + NI_CW
        LWI_CW = MAX( NIT_CW,LWI_CW )
!
!---    Allocate memory for new well interval element  ---
!
        ALLOCATE( IN_TMP_PTR,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: IN_TMP_PTR'
          CALL WRMSGP( INDX )
        ENDIF
!
!---    Assign well transition pointers  ---
!
        IF( NCW.EQ.1 ) THEN
          IN_TMP_PTR%ID1_CW = 1
        ELSE
          IN_TMP_PTR%ID1_CW = NIT_CW - NI_CW + 1
        ENDIF
        IN_TMP_PTR%ID2_CW = IN_TMP_PTR%ID1_CW + NI_CW - 1
        IN_TMP_PTR%NEXT => IN_CW_PTR
        IN_CW_PTR => IN_TMP_PTR
!
!---    Loop over number of well intervals  ---
!
        DO 100 NICW = 1,NI_CW
          CALL RD_INPL( CHDUM )
          CALL L_CASE( CHDUM )
          ISTART = 1
!
!---      Read first x-transition point  ---
!
          VARB = 'Interval First X-Transition Point'
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,XTP1_CWX)
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNM = 1
          CALL RD_UNIT(UNTS,XTP1_CWX,INDX)
!
!---      Read first y-transition point  ---
!
          VARB = 'Interval First Y-Transition Point'
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,YTP1_CWX)
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNM = 1
          CALL RD_UNIT(UNTS,YTP1_CWX,INDX)
!
!---      Cylindrical coordinates with azimuthal symmetry  ---
!
          IF( (ICS.EQ.2 .OR. ICS.EQ.6) .AND. LFY.EQ.1 ) THEN
            IF( ABS(XTP1_CWX)/EPSL.GT.EPSL ) THEN
              INDX = 9
              CHMSG = 'Non-Zero Interval First X-Transition Point ' // 
     &          'for Radially Symmetric Domain: XTP_CW ='
              RLMSG = XTP1_CWX
              CALL WRMSGP( INDX )
            ENDIF
            IF( ABS(YTP1_CWX)/EPSL.GT.EPSL ) THEN
              INDX = 9
              CHMSG = 'Non-Zero Interval First Y-Transition Point ' // 
     &          'for Radially Symmetric Domain: YTP_CW ='
              RLMSG = YTP1_CWX
              CALL WRMSGP( INDX )
            ENDIF
          ENDIF
!
!---      Read first z-transition point  ---
!
          VARB = 'Interval First Z-Transition Point'
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,ZTP1_CWX)
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNM = 1
          CALL RD_UNIT(UNTS,ZTP1_CWX,INDX)
!
!---      Read second x-transition point  ---
!
          VARB = 'Interval Second X-Transition Point'
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,XTP2_CWX)
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNM = 1
          CALL RD_UNIT(UNTS,XTP2_CWX,INDX)
!
!---      Read second y-transition point  ---
!
          VARB = 'Interval Second Y-Transition Point'
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,YTP2_CWX)
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNM = 1
          CALL RD_UNIT(UNTS,YTP2_CWX,INDX)
!
!---      Cylindrical coordinates with azimuthal symmetry  ---
!
          IF( (ICS.EQ.2 .OR. ICS.EQ.6) .AND. LFY.EQ.1 ) THEN
            IF( ABS(XTP2_CWX)/EPSL.GT.EPSL ) THEN
              INDX = 9
              CHMSG = 'Non-Zero Interval Second X-Transition Point ' // 
     &          'for Radially Symmetric Domain: XTP_CW ='
              RLMSG = XTP2_CWX
              CALL WRMSGP( INDX )
            ENDIF
            IF( ABS(YTP2_CWX)/EPSL.GT.EPSL ) THEN
              INDX = 9
              CHMSG = 'Non-Zero Interval Second Y-Transition Point ' // 
     &          'for Radially Symmetric Domain: YTP_CW ='
              RLMSG = YTP2_CWX
              CALL WRMSGP( INDX )
            ENDIF
          ENDIF
!
!---      Read second z-transition point  ---
!
          VARB = 'Interval Second Z-Transition Point'
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,ZTP2_CWX)
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNM = 1
          CALL RD_UNIT(UNTS,ZTP2_CWX,INDX)
!
!---      Allocate memory for new transition point element  ---
!
          ALLOCATE( TP_TMP_PTR,STAT=ISTAT )
          IF( ISTAT.NE.0 ) THEN
            INDX = 3
            CHMSG = 'Allocation Error: TP_TMP_PTR'
            CALL WRMSGP( INDX )
          ENDIF
          TP_TMP_PTR%XTP1_CW = XTP1_CWX
          TP_TMP_PTR%YTP1_CW = YTP1_CWX
          TP_TMP_PTR%ZTP1_CW = ZTP1_CWX
          TP_TMP_PTR%XTP2_CW = XTP2_CWX
          TP_TMP_PTR%YTP2_CW = YTP2_CWX
          TP_TMP_PTR%ZTP2_CW = ZTP2_CWX
          TP_TMP_PTR%NEXT => TP_CW_PTR
          TP_CW_PTR => TP_TMP_PTR
  100   CONTINUE
!
!---    Read number of well times or well time periods  ---
!
        IF( IT_CWX.EQ.1 ) THEN
          VARB = 'Number of Well Time Periods'
        ELSE
          VARB = 'Number of Well Times'
        ENDIF
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
        CALL RD_INT(ISTART,ICOMMA,CHDUM,IM_CWX)
!
!---    STOMP-EOR injection well, loop over the number of well
!       time periods, and well times  ---
!
        IF( IT_CWX.EQ.1 ) THEN
          LWTP_CW = MAX( LWTP_CW,IM_CWX )
!
!---      Loop over number of well time periods  ---
!
          NC = 0
          DO 210 M = 1,IM_CWX
            CALL RD_INPL( CHDUM )
            CALL L_CASE( CHDUM )
            ISTART = 1
            CALL RD_INT(ISTART,ICOMMA,CHDUM,IMP_CWX)
            NC = NC + IMP_CWX
!
!---        Loop over number of well times  ---
!
            DO 200 MX = 1,IMP_CWX
              CALL RD_INPL( CHDUM )
  200       CONTINUE
  210     CONTINUE
          LWT_CW = MAX( LWT_CW,NC )
        ELSE
          LWTP_CW = MAX( LWTP_CW,IM_CWX )
          LWT_CW = MAX( LWT_CW,IM_CWX )
!
!---      Loop over number of well times  ---
!
          DO 300 M = 1,IM_CWX
            CALL RD_INPL( CHDUM )
  300     CONTINUE
        ENDIF
  500 CONTINUE
!
!---  Allocate memory for coupled-well interval/node index array  ---
!
      ALLOCATE( ID_CW(1:7,1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ID_CW'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Load coupled-well interval/node index array  ---
!
      IN_TMP_PTR => IN_CW_PTR
      DO 510 NCW = 1,N_CW
        M = N_CW-NCW+1
        ID_CW(1,M) = IN_TMP_PTR%ID1_CW
        ID_CW(2,M) = IN_TMP_PTR%ID2_CW
        IN_TMP_PTR => IN_TMP_PTR%NEXT
  510 CONTINUE
!
!---  Allocate memory for x-transition points array  ---
!
      ALLOCATE( XTP_CW(1:2,1:LWI_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XTP_CW'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Allocate memory for y-transition points array  ---
!
      ALLOCATE( YTP_CW(1:2,1:LWI_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YTP_CW'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Allocate memory for z-transition points array  ---
!
      ALLOCATE( ZTP_CW(1:2,1:LWI_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZTP_CW'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Load x-, y-, and z-transition point arrays  ---
!
      TP_TMP_PTR => TP_CW_PTR
      DO 520 NICW = 1,LWI_CW
        M = LWI_CW-NICW+1
        XTP_CW(1,M) = TP_TMP_PTR%XTP1_CW
        XTP_CW(2,M) = TP_TMP_PTR%XTP2_CW
        YTP_CW(1,M) = TP_TMP_PTR%YTP1_CW
        YTP_CW(2,M) = TP_TMP_PTR%YTP2_CW
        ZTP_CW(1,M) = TP_TMP_PTR%ZTP1_CW
        ZTP_CW(2,M) = TP_TMP_PTR%ZTP2_CW
        TP_TMP_PTR => TP_TMP_PTR%NEXT
  520 CONTINUE
!
!---  Number and location of coupled well nodes  ---
!
      CALL CHK_CW
      LWN_CW = MAX( LWN_CW,1 )
      LWF_CW = MAX( LWF_CW,1 )
!
!---  Maximum number of well equation unknowns  ---
!
      LUKX = 2*LUK
      DO 530 NCW = 1,N_CW
        LUK_CW = MAX( LUK_CW,(LUKX*(ABS(ID_CW(6,NCW)-ID_CW(5,NCW))+1)) )
  530 CONTINUE
      LUK_CW = LUK_CW + 1
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_COUP_WELL group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_DPR( ISTART,ICOMMA,CHDUM,VAR )
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
!     Fill double precision variable VAR with data between commas.
!     Return default value or zero for null entries.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, November 1992.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*(*) CHDUM
      CHARACTER*6 FORM1
      CHARACTER*7 FORM2
!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM1,FORM2
      DATA FORM1 /'(D .0)'/
      DATA FORM2 /'(D  .0)'/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_DPR'
      IDFLTD = 0
      ICD = INDEX( CARD,'  ')-1
      IVR = INDEX( VARB,'  ')-1
!
!---  End of card record error ---
!
      IF( CHDUM(1:1) .EQ. '~' ) THEN
        INDX = 4
        CHMSG = 'End of Card Record: ' // VARB(1:IVR)
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Find next comma  ---
!
      ICOMMA = INDEX (CHDUM(ISTART:), ',') + ISTART - 1
      ISTOP = ICOMMA
  100 CONTINUE
!
!---  Comma not found, missing real data  ---
!
      IF( ISTOP.LT.ISTART ) THEN
        INDX = 4
        CHMSG = 'Missing Real Record: ' // VARB(1:IVR)
        CALL WRMSGP( INDX )
!
!---  Null entry  ---
!
      ELSEIF( ISTOP.EQ.ISTART ) THEN
        IF( IDFLT .EQ. 0 ) THEN
          VAR = 0.D+0
          IDFLTD = 1
        ENDIF
        ISTART = ICOMMA + 1
        ICOMMA = ISTART
!
!---  Characters between commas  ---
!
      ELSE
!
!---  Eliminate leading blank spaces  ---
!
        IF( ICHAR(CHDUM(ISTART:ISTART)).EQ.32 ) THEN
          ISTART = ISTART+1
          GOTO 100
        ENDIF
!
!---  Eliminate trailing blank spaces  ---
!
        ISTOP = ISTOP-1
  110   CONTINUE
        IF( ICHAR(CHDUM(ISTOP:ISTOP)).EQ.32 ) THEN
          ISTOP = ISTOP-1
          GOTO 110
        ENDIF
!
!---  Check for scientific notation  ---
!
        IEXP = ISTART-1
        IF( INDEX( CHDUM(ISTART:ISTOP),'e' ).NE.0 ) THEN
          IEXP = INDEX( CHDUM(ISTART:ISTOP),'e' )+ISTART-1
        ELSEIF( INDEX( CHDUM(ISTART:ISTOP),'d' ).NE.0 ) THEN
          IEXP = INDEX( CHDUM(ISTART:ISTOP),'d' )+ISTART-1
        ENDIF
        IPER = INDEX( CHDUM(ISTART:ISTOP),'.' )+ISTART-1
!
!---  Check for non-numerical characters  ---
!
        DO 120 N = ISTART,ISTOP
          IF( N.EQ.IEXP .OR. N.EQ.IPER ) GOTO 120
          NC = ICHAR(CHDUM(N:N))
          IF( ( N.EQ.ISTART .OR. N.EQ.IEXP+1 ) .AND.
     &      ( NC.EQ.43 .OR. NC.EQ.45 ) ) GOTO 120
          IF( NC.LT.48 .OR. NC.GT.57 ) THEN
            INDX = 4
            CHMSG = 'Real Format: Nonnumeric Character: ' //
     &        VARB(1:IVR) // ': ' // CHDUM(ISTART:ISTOP)
            CALL WRMSGP(INDX)
          ENDIF
  120   CONTINUE
!
!---  Translate character string into a double precision real  ---
!
        NCHR = ISTOP-ISTART+1
        IF( NCHR .LT. 10 ) THEN
          WRITE( FORM1(3:3), '(I1)' ) NCHR
          READ (CHDUM(ISTART:ISTOP), FORM1 ) VAR
        ELSEIF( NCHR .LT. 100 ) THEN
          WRITE( FORM2(3:4), '(I2)' ) NCHR
          READ (CHDUM(ISTART:ISTOP), FORM2 ) VAR
        ELSE
          INDX = 4
          CHMSG = 'Excessive Length Real Record: ' //
     &      VARB(1:IVR) // ': ' // CHDUM(ISTART:ISTOP)
          CALL WRMSGP( INDX )
        ENDIF
        ISTART = ICOMMA + 1
      ENDIF
      IDFLT = 0
!
!---  End of RD_DPR group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_ECLGRID
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
!     Read ECLIPSE formatted grids.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 30 November 2011.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GRID
      USE FILES
      USE CONST
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 ZCX(8)
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: XX,YX,ZX
      REAL*8, DIMENSION(:), ALLOCATABLE :: XPETX
      REAL*8, DIMENSION(:), ALLOCATABLE :: ZCORNX
      CHARACTER*64 ADUM,BDUM,BMDUM,FDUM,FMDUM,UNTS
      CHARACTER*512 BHDUM,CHDUM
      INTEGER ICX(2,2,2)
      LOGICAL BCHK,FCHK
!
!----------------------Data Statements---------------------------------!
!
      SAVE ICX
      DATA ICX / 8,7,6,5,4,3,2,1 /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_ECLGRID'
!
!---  Dynamic memory allocation  ---
!
      ALLOCATE( XX(1:8,1:LFY+1),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XX'
        CALL WRMSGP( INDX )
      ENDIF
      ALLOCATE( YX(1:8,1:LFY+1),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YX'
        CALL WRMSGP( INDX )
      ENDIF
      ALLOCATE( ZX(1:8,1:LFY+1),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZX'
        CALL WRMSGP( INDX )
      ENDIF
      ALLOCATE( XPETX(1:(LFX+1)*(LFY+1)*6),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XPETX'
        CALL WRMSGP( INDX )
      ENDIF
      ALLOCATE( ZCORNX(1:LFX*LFY*LFZ*8),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZCORNX'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Read Eclipse file name  ---
!
      IFILE = 9
      ISTART = 1
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      VARB = 'Generic Eclipse File Name'
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
!
!---  Check for external file  ---
!
      INQUIRE( FILE=FDUM(1:NCH), FORM=FMDUM, EXIST=FCHK )
      IF( .NOT.FCHK ) THEN
        INDX = 4
        CHMSG = 'Missing Generic Eclipse File: ' // FDUM(1:NCH)
        CALL WRMSGP( INDX )
      ELSEIF( FDUM.EQ.'unformatted' ) THEN
        INDX = 4
        CHMSG = 'Generic Eclipse File Format: ' // FDUM(1:NCH)
        CALL WRMSGP( INDX )
      ENDIF
      OPEN( UNIT=9,FILE=FDUM(1:NCH),STATUS='OLD',FORM='FORMATTED' )
      REWIND( UNIT=9 )
      INDX = 0
      DZMINX = 0.D+0
      VARB = 'Minimum Z Grid Spacing'
      CALL RD_DPR(ISTART,ICOMMA,CHDUM,DZMINX)
      VARB = 'Minimum Z Grid Spacing Units'
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      IUNM = 1
      CALL RD_UNIT( UNTS,DZMINX,INDX )
!
!---  Search file for Petrel version number---
!
      IPET = 0
  100 READ(9,'(A)',END=110) CHDUM   
      CALL L_CASE( CHDUM )
      IF( INDEX(CHDUM(1:),'-- exported').NE.0 ) THEN
        IF( INDEX(CHDUM(25:),'2013').NE.0 ) THEN
          IPET=2013
        ELSEIF( INDEX(CHDUM(25:),'2011').NE.0 ) THEN
          IPET=2011
        ELSE
          IPET=2009
        ENDIF
        GOTO 120
      ELSE
        GOTO 100
      ENDIF
  110 CONTINUE
!
!---  Petrel Version number not found  ---
!
      INDX = 18
      CHMSG = 'Missing Petrel Version Number'
      CALL WRMSGP( INDX )
  120 CONTINUE
!
!
!---  Search file for key word "GRIDUNIT"  ---
!
  130 READ(9,'(A)',END=140) CHDUM   
      CALL L_CASE( CHDUM )
      IF( INDEX(CHDUM(1:),'gridunit').NE.0 ) THEN
        GOTO 150
      ELSE
        GOTO 130
      ENDIF
  140 CONTINUE
!
!---  Key word "GRIDUNIT" not found  ---
!
      INDX = 18
      CHMSG = 'Missing Grid Units'
      CALL WRMSGP( INDX )
  150 CONTINUE
!
!---  Read number of grid units  ---
!
      READ(9,'(A)') CHDUM
      CALL L_CASE( CHDUM )
      VARX = 1.D+0
      IF( INDEX(CHDUM(1:),'metres').NE.0 ) THEN
        INDX = 0
        IUNM = 1
        UNTS = 'm'
        CALL RD_UNIT(UNTS,VARX,INDX)
      ELSEIF( INDEX(CHDUM(1:),'feet').NE.0 ) THEN
        INDX = 0
        IUNM = 1
        UNTS = 'ft'
        CALL RD_UNIT(UNTS,VARX,INDX)
      ELSEIF( INDEX(CHDUM(1:),'nm').NE.0 ) THEN
        INDX = 0
        IUNM = 1
        UNTS = 'nm'
        CALL RD_UNIT(UNTS,VARX,INDX)
      ENDIF
!
!---  Search file for key word "SPECGRID"  ---
!
  160 READ(9,'(A)',END=170) CHDUM   
      CALL L_CASE( CHDUM )
      IF( INDEX(CHDUM(1:),'specgrid').NE.0 ) THEN
        GOTO 180
      ELSE
        GOTO 160
      ENDIF
  170 CONTINUE
!
!---  Key word "SPECGRID" not found  ---
!
      INDX = 18
      CHMSG = 'Missing Grid Specification'
      CALL WRMSGP( INDX )
  180 CONTINUE
!
!---  Read number of grid cells in each direction  ---
!
      READ(9,*) IFLDX,JFLDX,KFLDX
!
!---  Check grid dimensions against STOMP input  ---
!
      IF( IFLDX.NE.LFX .OR. JFLDX.NE.LFY .OR. KFLDX.NE.LFZ ) THEN
        INDX = 4
        CHMSG = 'STOMP-ECLIPSE Grid Conflict'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Search file for key word "COORD"  ---
!
  190 READ(9,'(A)', END=200) CHDUM   
      CALL L_CASE( CHDUM )
!
!---  Key word "COORD" found in main file  ---
!
      IF( INDEX(CHDUM(1:),'coord').NE.0 .AND.
     &  INDEX(CHDUM(1:),'coordsys').EQ.0 ) THEN
        GOTO 210
!
!---  Search for key word "COORD" found in include file  ---
!
      ELSEIF( INDEX(CHDUM(1:),'include').NE.0 ) THEN
        READ(9,'(A)',END=200) CHDUM
        ISTX = INDEX( CHDUM(1:),'''' )
        ISTX = ISTX + 1
        ISPX = INDEX( CHDUM(ISTX:),'''' ) + ISTX - 1
        BDUM = ' '
        ISPX = ISPX - 1
        NCH = ISPX-ISTX+1
        IF( NCH.LT.1 ) THEN
          INDX = 4
          CHMSG = 'Unrecognized File Name after INCLUDE'
          CALL WRMSGP( INDX )
        ENDIF
        READ(CHDUM(ISTX:ISPX),'(A)') BDUM(1:NCH)
        INQUIRE( FILE=BDUM(1:NCH), FORM=BMDUM, EXIST=BCHK )
        IF( .NOT.BCHK ) THEN
          INDX = 4
          CHMSG = 'Missing Include File: ' // BDUM(1:NCH)
          CALL WRMSGP( INDX )
        ENDIF
        OPEN( UNIT=10,FILE=BDUM(1:NCH),STATUS='OLD',FORM='FORMATTED' )
        REWIND( UNIT=10 )
  192   READ(10,'(A)',END=200) BHDUM
        CALL L_CASE( BHDUM )
!
!---    Skip header information in include file  ---
!
        IF( INDEX(BHDUM(1:2),'--').NE.0 ) THEN
          GOTO 192
!
!---    Key word "COORD" found in include file  ---
!
        ELSEIF( INDEX(BHDUM(1:),'coord').NE.0 .AND.
     &    INDEX(BHDUM(1:),'coordsys').EQ.0 ) THEN
          IFILE = 10
          GOTO 210
        ELSEIF( SCAN(BHDUM(1:),'0123456789').NE.0 ) THEN 
          CLOSE( UNIT=10 )
          IFILE = 9
        ELSE
          GOTO 192
        ENDIF
      ELSE
        GOTO 190
      ENDIF
  200 CONTINUE
!
!---  Key word "COORD" not found  ---
!
      INDX = 18
      CHMSG = 'Missing Coordinate Data'
      CALL WRMSGP( INDX )
  210 CONTINUE
!
!---  For Petrel 2009/2011 read all coordinates into array and assign 
!     to X and Y and convert coordinates to meters
!
      NCORX = (IFLDX+1)*(JFLDX+1)*6
      READ(IFILE,*,END=212) (XPETX(L),L=1,NCORX)
!
!---  Close include file  ---
!
      IF( IFILE.EQ.10 ) THEN
        CLOSE( UNIT=10 )
        IFILE = 9
      ENDIF
      GOTO 214
!
!---  End-of-file error for Generic Eclipse File ---
!
  212 CONTINUE
      INDX = 18
      CHMSG = 'Generic Eclipse EOF: COORD'
      CALL WRMSGP( INDX )
!
!---  File read completed for Generic Eclipse File ---
!
  214 CONTINUE
      DO 224 K = 1,LFZ
        DO 222 J = 1,LFY
          DO 220 I = 1,LFX
            N = ND(I,J,K)
!
!---        Points 1 and 5  ---
!
            IX = I
            JX = J
            L = (JX-1)*(IFLDX+1)*6 + (IX-1)*6
            XE(1,N) = XPETX(L+1)*VARX
            YE(1,N) = XPETX(L+2)*VARX
            XE(5,N) = XPETX(L+1)*VARX
            YE(5,N) = XPETX(L+2)*VARX
            ZTX = -XPETX(L+3)*VARX
            ZBX = -XPETX(L+6)*VARX
            IF( ZBX.GT.ZTX ) THEN
              INDX = 7
              IMSG = (J-1)*(LFX+1) + I
              CHMSG = 'STOMP-ECLIPSE Grid Reversal: COORD Line'
              CALL WRMSGP( INDX )
            ENDIF                 
!
!---        Points 2 and 6  ---
!
            IX = I+1
            JX = J
            L = (JX-1)*(IFLDX+1)*6 + (IX-1)*6
            XE(2,N) = XPETX(L+1)*VARX
            YE(2,N) = XPETX(L+2)*VARX
            XE(6,N) = XPETX(L+1)*VARX
            YE(6,N) = XPETX(L+2)*VARX
            ZTX = -XPETX(L+3)*VARX
            ZBX = -XPETX(L+6)*VARX
            IF( ZBX.GT.ZTX ) THEN
              INDX = 7
              IMSG = (J-1)*(LFX+1) + I
              CHMSG = 'STOMP-ECLIPSE Grid Reversal: COORD Line'
              CALL WRMSGP( INDX )
            ENDIF                 
!
!---        Points 3 and 7  ---
!
            IX = I
            JX = J+1
            L = (JX-1)*(IFLDX+1)*6 + (IX-1)*6
            XE(3,N) = XPETX(L+1)*VARX
            YE(3,N) = XPETX(L+2)*VARX
            XE(7,N) = XPETX(L+1)*VARX
            YE(7,N) = XPETX(L+2)*VARX
            ZTX = -XPETX(L+3)*VARX
            ZBX = -XPETX(L+6)*VARX
            IF( ZBX.GT.ZTX ) THEN
              INDX = 7
              IMSG = (J-1)*(LFX+1) + I
              CHMSG = 'STOMP-ECLIPSE Grid Reversal: COORD Line'
              CALL WRMSGP( INDX )
            ENDIF                 
!
!---        Points 4 and 8  ---
!
            IX = I+1
            JX = J+1
            L = (JX-1)*(IFLDX+1)*6 + (IX-1)*6
            XE(4,N) = XPETX(L+1)*VARX
            YE(4,N) = XPETX(L+2)*VARX
            XE(8,N) = XPETX(L+1)*VARX
            YE(8,N) = XPETX(L+2)*VARX
            ZTX = -XPETX(L+3)*VARX
            ZBX = -XPETX(L+6)*VARX
            IF( ZBX.GT.ZTX ) THEN
              INDX = 7
              IMSG = (J-1)*(LFX+1) + I
              CHMSG = 'STOMP-ECLIPSE Grid Reversal: COORD Line'
              CALL WRMSGP( INDX )
            ENDIF                 
  220     CONTINUE
  222   CONTINUE
  224 CONTINUE
!
!---  Search file for key word "ZCORN"  ---
!
  290 READ(9,'(A)',END=300) CHDUM   
      CALL L_CASE( CHDUM )
!
!---  Key word "ZCORN" found in main file  ---
!
      IF( INDEX(CHDUM(1:),'zcorn').NE.0 ) THEN
        GOTO 310
!
!---  Search for key word "ZCORN" found in include file  ---
!
      ELSEIF( INDEX(CHDUM(1:),'include').NE.0 ) THEN
        READ(9,'(A)',END=200) CHDUM
        ISTX = INDEX( CHDUM(1:),'''' )
        ISTX = ISTX + 1
        ISPX = INDEX( CHDUM(ISTX:),'''' ) + ISTX - 1
        BDUM = ' '
        ISPX = ISPX - 1
        NCH = ISPX-ISTX+1
        IF( NCH.LT.1 ) THEN
          INDX = 4
          CHMSG = 'Unrecognized File Name after INCLUDE'
          CALL WRMSGP( INDX )
        ENDIF
        READ(CHDUM(ISTX:ISPX),'(A)') BDUM(1:NCH)
        INQUIRE( FILE=BDUM(1:NCH), FORM=BMDUM, EXIST=BCHK )
        IF( .NOT.BCHK ) THEN
          INDX = 4
          CHMSG = 'Missing Include File: ' // BDUM(1:NCH)
          CALL WRMSGP( INDX )
        ENDIF
        OPEN( UNIT=10,FILE=BDUM(1:NCH),STATUS='OLD',FORM='FORMATTED' )
        REWIND( UNIT=10 )
  292   READ(10,'(A)',END=200) BHDUM
        CALL L_CASE( BHDUM )
!
!---    Skip header information in include file  ---
!
        IF( INDEX(BHDUM(1:2),'--').NE.0 ) THEN
          GOTO 292
!
!---    Key word "ZCORN" found in include file  ---
!
        ELSEIF( INDEX(BHDUM(1:),'zcorn').NE.0 ) THEN
          IFILE = 10
          GOTO 310
        ELSEIF( SCAN(BHDUM(1:),'0123456789').NE.0 ) THEN 
          CLOSE( UNIT=10 )
          IFILE = 9
        ELSE
          GOTO 292
        ENDIF
      ELSE
        GOTO 290
      ENDIF
  300 CONTINUE
!
!---  Key word "ZCORN" not found  ---
!
      INDX = 18
      CHMSG = 'Missing Z-Corner Data'
      CALL WRMSGP( INDX )
  310 CONTINUE
!
!---  Loop over the z corner data for Petrel version 2011 ---
!
      NCORN = LFX*LFY*LFZ*8
      READ(IFILE,*,END=312) ZCORNX(1:NCORN)
!
!---  Close include file  ---
!
      IF( IFILE.EQ.10 ) THEN
        CLOSE( UNIT=10 )
        IFILE = 9
      ENDIF
      GOTO 314
!
!---  End-of-file error for Generic Eclipse File ---
!
  312 CONTINUE
      INDX = 18
      CHMSG = 'Generic Eclipse EOF: ZCORN'
      CALL WRMSGP( INDX )
  314 CONTINUE
      NX = 0
!
!---  Loop from the top tier down  ---
!
      DO 340 K = LFZ,1,-1
!
!---    Upper ZE values  ---
!
        DO 324 J = 1,LFY
          DO 320 I = 1,LFX
            N = ND(I,J,K)
            NX = NX + 1
            ZE(5,N) = -ZCORNX(NX)*VARX
            NX = NX + 1
            ZE(6,N) = -ZCORNX(NX)*VARX
  320     CONTINUE
          DO 322 I = 1,LFX
            N = ND(I,J,K)
            NX = NX + 1
            ZE(7,N) = -ZCORNX(NX)*VARX
            NX = NX + 1
            ZE(8,N) = -ZCORNX(NX)*VARX
  322     CONTINUE
  324   CONTINUE
!
!---    Lower ZE values  ---
!
        DO 334 J = 1,LFY
          DO 330 I = 1,LFX
            N = ND(I,J,K)
            NX = NX + 1
            ZE(1,N) = -ZCORNX(NX)*VARX
            NX = NX + 1
            ZE(2,N) = -ZCORNX(NX)*VARX
  330     CONTINUE
          DO 332 I = 1,LFX
            N = ND(I,J,K)
            NX = NX + 1
            ZE(3,N) = -ZCORNX(NX)*VARX
            NX = NX + 1
            ZE(4,N) = -ZCORNX(NX)*VARX
  332     CONTINUE
  334   CONTINUE
  340 CONTINUE
!
!---  Set minimum z grid spacing, honoring the top
!     surface  ---
!
      DO 430 J = 1,LFY
        DO 420 I = 1,LFX
!
!---      Loop over four vertical corners of the hexahedral node  ---
!
          DO 410 M = 1,4
!
!---        Loop over nodes vertically top to bottom  ---
!
            DO 402 K = LFZ,1,-1
              N = ND(I,J,K)
              DZX = ZE(M+4,N)-ZE(M,N)
!
!---          Spacing of vertical corner < than specified minimum  ---
!
              IF( DZX.LT.DZMINX ) THEN
!
!---            Push lower vertices downward  ---
!
                ZE(M,N) = ZE(M,N) - DZMINX
                IF( K.GT.1 ) THEN
                  DO 400 KX = K-1,1,-1
                    NX = ND(I,J,KX)
                    ZE(M+4,NX) = ZE(M+4,NX) - DZMINX
                    ZE(M,NX) = ZE(M,NX) - DZMINX
  400             CONTINUE
                ENDIF
              ENDIF
  402       CONTINUE
  410     CONTINUE
  420   CONTINUE
  430 CONTINUE
!
!---  Transpose y grid  ---
!
      DO 470 K = 1,LFZ
        DO 460 I = 1,LFX
          DO 440 J = 1,LFY
            N = ND(I,J,K)
            DO 432 M = 1,8
              XX(M,J) = XE(M,N)
              YX(M,J) = YE(M,N)
              ZX(M,J) = ZE(M,N)
  432       CONTINUE
  440     CONTINUE
          DO 450 J = 1,LFY
            JX = LFY-J+1
            N = ND(I,J,K)
            XE(1,N) = XX(3,JX)
            YE(1,N) = YX(3,JX)
            ZE(1,N) = ZX(3,JX) 
            XE(2,N) = XX(4,JX)
            YE(2,N) = YX(4,JX)
            ZE(2,N) = ZX(4,JX) 
            XE(3,N) = XX(1,JX)
            YE(3,N) = YX(1,JX)
            ZE(3,N) = ZX(1,JX) 
            XE(4,N) = XX(2,JX)
            YE(4,N) = YX(2,JX)
            ZE(4,N) = ZX(2,JX) 
            XE(5,N) = XX(7,JX)
            YE(5,N) = YX(7,JX)
            ZE(5,N) = ZX(7,JX) 
            XE(6,N) = XX(8,JX)
            YE(6,N) = YX(8,JX)
            ZE(6,N) = ZX(8,JX) 
            XE(7,N) = XX(5,JX)
            YE(7,N) = YX(5,JX)
            ZE(7,N) = ZX(5,JX) 
            XE(8,N) = XX(6,JX)
            YE(8,N) = YX(6,JX)
            ZE(8,N) = ZX(6,JX) 
  450     CONTINUE
  460   CONTINUE
  470 CONTINUE
!
!---  Dynamic memory deallocation  ---
!
      IF( ALLOCATED(XX) ) THEN
        DEALLOCATE( XX,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: XX'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
      IF( ALLOCATED(YX) ) THEN
        DEALLOCATE( YX,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: YX'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
      IF( ALLOCATED(ZX) ) THEN
        DEALLOCATE( ZX,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: ZX'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
      IF( ALLOCATED(XPETX) ) THEN
        DEALLOCATE( XPETX,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: XPETX'
          CALL WRMSGP( INDX )
        ENDIF
      ENDIF
!
!---  Close Generic Eclipse File   ---
!
      CLOSE( UNIT=9 )
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_ECLGRID group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_ELMGRID
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
!     Read grid elements and vertices.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 December 2011.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GRID
      USE FILES
      USE CONST
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 VDX(8),VARX(3)
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: VX
      INTEGER IVX(8),JVX(8)
      CHARACTER*64 FDUM,FMDUM,UNTS
      CHARACTER*512 ADUM,CHDUM
      LOGICAL FCHK
!
!----------------------Data Statements---------------------------------!
!
      SAVE JVX
      DATA JVX / 1,2,4,3,5,6,8,7 /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_ELMGRID'
!
!---  Read external vertices file name  ---
!
      ISTART = 1
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      VARB = 'External Vertices File Name'
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
!
!---  Check for external file  ---
!
      INQUIRE( FILE=FDUM(1:NCH), FORM=FMDUM, EXIST=FCHK )
      IF( .NOT.FCHK ) THEN
        INDX = 4
        CHMSG = 'Missing Vertices File: ' // FDUM(1:NCH)
        CALL WRMSGP( INDX )
      ELSEIF( FDUM.EQ.'unformatted' ) THEN
        INDX = 4
        CHMSG = 'Vertices File Format: ' // FDUM(1:NCH)
        CALL WRMSGP( INDX )
      ENDIF
      OPEN( UNIT=9,FILE=FDUM(1:NCH),STATUS='OLD',FORM='FORMATTED' )
      REWIND( UNIT=9 )
!
!---  Number of vertices  ---
!
      VARB = 'Number of Vertices'
      CALL RD_INT( ISTART,ICOMMA,CHDUM,NVX )
!
!---  Dynamic memory allocation  ---
!
      ALLOCATE( VX(1:3,1:NVX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VX'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Read vertice units  ---
!
      ISTART = 1
      READ(9,'(A)') CHDUM
      CALL L_CASE( CHDUM )
      ISTART = INDEX( CHDUM(ISTART:), 'x[' ) + ISTART + 1
      ICOMMA = INDEX( CHDUM(ISTART:), ']' ) + ISTART - 1
      UNTS = ' '
      NCH = ICOMMA-ISTART
      ISTOP = ICOMMA-1
      READ (CHDUM(ISTART:ISTOP), '(A)') UNTS(1:NCH)
      INDX = 0
      VARX(1) = 1.D+0
      IUNM = 1
      CALL RD_UNIT(UNTS,VARX(1),INDX)
      ISTART = ICOMMA + 1
      ISTART = INDEX( CHDUM(ISTART:), 'y[' ) + ISTART + 1
      ICOMMA = INDEX( CHDUM(ISTART:), ']' ) + ISTART - 1
      UNTS = ' '
      NCH = ICOMMA-ISTART
      ISTOP = ICOMMA-1
      READ (CHDUM(ISTART:ISTOP), '(A)') UNTS(1:NCH)
      INDX = 0
      VARX(2) = 1.D+0
      IUNM = 1
      CALL RD_UNIT(UNTS,VARX(2),INDX)
      ISTART = ICOMMA + 1
      ISTART = INDEX( CHDUM(ISTART:), 'z[' ) + ISTART + 1
      ICOMMA = INDEX( CHDUM(ISTART:), ']' ) + ISTART - 1
      UNTS = ' '
      NCH = ICOMMA-ISTART
      ISTOP = ICOMMA-1
      READ (CHDUM(ISTART:ISTOP), '(A)') UNTS(1:NCH)
      INDX = 0
      VARX(3) = 1.D+0
      IUNM = 1
      CALL RD_UNIT(UNTS,VARX(3),INDX)
!
!---  Read vertices  ---
!
      DO 100 NX = 1,NVX
        READ(9,*) N,(VDX(M),M=1,3)
        IF( N.GT.NVX ) THEN
          INDX = 4
          CHMSG = 'Mismatch between Input File and Vertices File: '
     &      // FDUM(1:NCH)
          CALL WRMSGP( INDX )
        ENDIF
        DO 10 M = 1,3
          VX(M,N) = VDX(M)
   10   CONTINUE
  100 CONTINUE
!
!---  Close vertices file   ---
!
      CLOSE( UNIT=9 )
!
!---  Read external elements file name  ---
!
      ISTART = 1
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      VARB = 'External Elements File Name'
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
!
!---  Check for external file  ---
!
      INQUIRE( FILE=FDUM(1:NCH), FORM=FMDUM, EXIST=FCHK )
      IF( .NOT.FCHK ) THEN
        INDX = 4
        CHMSG = 'Missing Elements File: ' // FDUM(1:NCH)
        CALL WRMSGP( INDX )
      ELSEIF( FDUM.EQ.'unformatted' ) THEN
        INDX = 4
        CHMSG = 'Elements File Format: ' // FDUM(1:NCH)
        CALL WRMSGP( INDX )
      ENDIF
      OPEN( UNIT=9,FILE=FDUM(1:NCH),STATUS='OLD',FORM='FORMATTED' )
      REWIND( UNIT=9 )
!
!---  Assign vertices  ---
!
      DO 200 NX = 1,LFD
        READ(9,*) N,(IVX(M),M=1,8)
        DO 110 M = 1,8
          XE(M,N) = VX(1,IVX(M))*VARX(1)
          YE(M,N) = VX(2,IVX(M))*VARX(2)
          ZE(M,N) = VX(3,IVX(M))*VARX(3)
  110   CONTINUE
  200 CONTINUE
!
!---  Close elements File   ---
!
      CLOSE( UNIT=9 )
!
!---  Dynamic memory deallocation  ---
!
      DEALLOCATE( VX,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: VX'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_ECLGRID group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_EQEQ
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
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 9 December 2004.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
      USE REACT
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
      CHARACTER*128 ADUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_EQEQ'
!
!---  Assign card string  ---
!
      CARD = 'Equilibrium Equations Card'
!
!---  Read number of equilibrium equations  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Equilibrium Equations'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NEQE)
      LEQE = MAX( LEQE,NEQE )
!
!---  Loop over the equilibrium equations  ---
!
      DO 100 NEQ = 1,NEQE
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
        VARB = 'Number of Species in Equilibrium Equation'
        CALL RD_INT( ISTART,ICOMMA,CHDUM,NSEE )
        LSEE = MAX( LSEE,NSEE )
!
!---    Loop over the equilibrium-equation species allowing for
!       returns in the input  ---
!
        DO 90 NSP = 1,NSEE
          VARB = 'Equilibrium-Equation Species Name: '
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RD_INPL( CHDUM )
            CALL L_CASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
          IF( NSP.GT.1 ) THEN
            VARB = 'Equilibrium-Equation Species Exponent: '
!
!---        Allow for returns in input lines  ---
!
            CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              CALL RD_INPL( CHDUM )
              CALL L_CASE( CHDUM )
              ISTART = 1
            ENDIF
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
   90   CONTINUE
  100 CONTINUE
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_EQEQ group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_EQRC
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
!     Written by MD White, PNNL, 9 December 2004.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
      USE REACT
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*128 ADUM
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_EQRC'
!
!---  Assign card string  ---
!
      CARD = 'Equilibrium Reactions Card'
!
!---  Read number of equilibrium reactions  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Equilibrium Reactions'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NRCE)
      LRCE = MAX( LRCE,NRCE )
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_EQRC group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_EXSP
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
!     Read exchanged species.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 8 December 2004.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
      USE REACT
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_EXSP'
!
!---  Assign card string  ---
!
      CARD = 'Exchanged Species Card'
!
!---  Read number of exchanged species  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Exchanged Species'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NSPE)
      LSPE = MAX( LSPE,NSPE )
!
!---  Read number of exchange sites  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Exchange Sites'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NESITE)
      LESITE = MAX( LESITE,NESITE )
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_EXSP group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_GMBC
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
!     Read Geomechanics Boundary Conditions Card for number of 
!     boundary conditions and number of boundary condition times.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 29 November 2016.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GEOMECH
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
      CHARACTER*128 ADUM,BDUM,CDUM,FDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_GMBC'
!
!---  Assign card string  ---
!
      CARD = 'Geomechanical Boundary Conditions Card'
!
!---  Read number of boundary condition inputs  ---
!
      LBC_GM = 1
      LBCIN_GM = 1
      LBTM_GM = 1
      NBC = 0
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Geomechanical Boundary Condition Inputs: '
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
      LBCIN_GM = MAX( LBCIN_GM,NLIN )
      DO 400 NB = 1, NLIN
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
!
!---    Read boundary orientation  ---
!
        VARB = 'Boundary Condition Orientation: '
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'west').NE.0 ) THEN
          IBCDX = -1
        ELSEIF( INDEX(ADUM(1:),'east').NE.0 ) THEN
          IBCDX = 1
        ELSEIF( INDEX(ADUM(1:),'south').NE.0 ) THEN
          IBCDX = -2
        ELSEIF( INDEX(ADUM(1:),'north').NE.0 ) THEN
          IBCDX = 2
        ELSEIF( INDEX(ADUM(1:),'bottom').NE.0 ) THEN
          IBCDX = -3
        ELSEIF( INDEX(ADUM(1:),'top').NE.0 ) THEN
          IBCDX = 3
        ELSEIF( INDEX(ADUM(1:),'file').NE.0 ) THEN
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
          NCH = INDEX(FDUM,'  ')-1
          OPEN(UNIT=27,FILE=FDUM(1:NCH),STATUS='OLD',FORM='FORMATTED')
          I1X = 1
          I2X = 1
          J1X = 1
          J2X = 1
          K1X = 1
          K2X = 0
    5     CONTINUE
          READ(27,*,END=10) IX,JX,KX,IBCDX
          K2X = K2X+1
          GOTO 5
   10     CONTINUE
          REWIND(27)
        ENDIF
!
!---    Read and write boundary domain indices  ---
!
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
        IF( INDEX(ADUM(1:),'file').EQ.0 ) THEN
          VARB = 'Boundary Condition Domain: '
          CALL RD_INT(ISTART,ICOMMA,CHDUM,I1X)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,I2X)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,J1X)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,J2X)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,K1X)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,K2X)
!
!---      Check geomechanical boundary domain  ---
!
          IF( I1X.GT.I2X .OR. J1X.GT.J2X .OR. K1X.GT.K2X ) THEN
            INDX = 4
            CHMSG = 'Nonascending Geomechanical Boundary Condition '
     &        // 'FE Nodal Domain Indices'
            CALL WRMSGP( INDX )
          ENDIF
          IF( I1X.LT.1 .OR. I2X.GT.LFX+1. OR. J1X.LT.1 .OR.
     &      J2X.GT.LFY+1 .OR. K1X.LT.1 .OR. K2X.GT.LFZ+1 ) THEN
            INDX = 4
            CHMSG = 'Illegal Geomechanical Boundary Condition ' // 
     &        'FE Nodal Domain'
            CALL WRMSGP( INDX )
          ENDIF
        ENDIF
!
!---  Read number of geomechanical boundary times  ---
!
        VARB = 'Number of Geomechanical Boundary Condition Times'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,IBCMX)
        IF( IBCMX.LE.-3 ) THEN
          IBCCX = 1
          IBCMX = -IBCMX
        ELSEIF( IBCMX.GE.1 ) THEN
          IBCCX = 0
        ELSEIF( IBCMX.EQ.0 ) THEN
          INDX = 4
          CHMSG = 'No Boundary Condition Times'
          CALL WRMSGP( INDX )
        ELSE
          INDX = 4
          CHMSG = 'Number of Cyclic Boundary Conditions Times < 3'
          CALL WRMSGP( INDX )
        ENDIF
        LBTM_GM = MAX( LBTM_GM,IBCMX )
!
!---    Skip over the geomechanical boundary condition variables 
!       and units  ---
!
        DO 100 NTM = 1,IBCMX
          CALL RD_INPL( CHDUM )
          CALL L_CASE( CHDUM )
          ISTART = 1
!
!---      Check for external geomechanical boundary condition 
!         time file  ---
!
          IF( NTM.EQ.1 ) CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,CDUM)
          CALL L_CASE( CDUM )
          IF( INDEX(CDUM(1:),'file').NE.0 ) GOTO 110
  100   CONTINUE
  110   CONTINUE
!
!---    Compute the number of geomechanical boundary FE nodes  ---
!
        NBC = NBC + (K2X-K1X+1)*(J2X-J1X+1)*(I2X-I1X+1)
        LBC_GM = MAX( LBC_GM,NBC )
        IF( INDEX(ADUM(1:),'file').NE.0 ) CLOSE(UNIT=27)
  400 CONTINUE
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_GMBC group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_INPL( CHDUM )
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
!     Read input line.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, November 10, 1999.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*(*) CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_INPL'
!
!---  Skip input lines that begin with '#' or '!'  ---
!
   10 READ(IRD,'(A)') CHDUM
      IF( CHDUM(1:1).EQ.'#' .OR. CHDUM(1:1).EQ.'!' ) GOTO 10
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_INPL group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_INT( ISTART,ICOMMA,CHDUM,IVAR )
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
!     Fill integer variable IVAR with data between commas.
!     Return default value or zero for null entries.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNNL, November 1992.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*(*) CHDUM
      CHARACTER*4 FORM1
!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM1
      DATA FORM1 /'(I )'/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_INT'
      IDFLTD = 0
      ICD = INDEX( CARD,'  ')-1
      IVR = INDEX( VARB,'  ')-1
!
!---  End of card record error ---
!
      IF( CHDUM(1:1) .EQ. '~' ) THEN
        INDX = 4
        CHMSG = 'End of Card Record: ' // VARB(1:IVR)
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Read numbers between commas  ---
!
      ICOMMA = INDEX (CHDUM(ISTART:), ',') + ISTART - 1
      ISTOP = ICOMMA
  100 CONTINUE
!
!---  Comma not found, missing integer data  ---
!
      IF( ISTOP.LT.ISTART ) THEN
        INDX = 4
        CHMSG = 'Missing Integer Record: ' // VARB(1:IVR)
        CALL WRMSGP( INDX )
!
!---  Null entry  ---
!
      ELSEIF( ISTOP.EQ.ISTART ) THEN
        IF( IDFLT .EQ. 0 ) THEN
          IVAR = 0
          IDFLTD = 1
        ENDIF
        ISTART = ICOMMA + 1
        ICOMMA = ISTART
!
!---  Characters between commas  ---
!
      ELSE
!
!---  Eliminate leading blank spaces  ---
!
        IF( ICHAR(CHDUM(ISTART:ISTART)).EQ.32 ) THEN
          ISTART = ISTART+1
          GOTO 100
        ENDIF
!
!---  Eliminate trailing blank spaces  ---
!
        ISTOP = ISTOP-1
  110   CONTINUE
        IF( ICHAR(CHDUM(ISTOP:ISTOP)).EQ.32 ) THEN
          ISTOP = ISTOP-1
          GOTO 110
        ENDIF
!
!---  Check for non-numerical characters  ---
!
        DO 120 N = ISTART,ISTOP
          NC = ICHAR(CHDUM(N:N))
          IF( N.EQ.ISTART .AND. ( NC.EQ.43 .OR. NC.EQ.45 ) ) GOTO 120
          IF( NC.LT.48 .OR. NC.GT.57 ) THEN
            INDX = 4
            CHMSG = 'Integer Format: Nonnumeric Character: ' //
     &        VARB(1:IVR) // ': ' // CHDUM(ISTART:ISTOP)
            CALL WRMSGP(INDX)
          ENDIF
  120   CONTINUE
!
!---  Translate character string into an integer  ---
!
        NCHR = ISTOP-ISTART+1
        IF( NCHR.LT.10 ) THEN
          WRITE( FORM1(3:3),'(I1)' ) NCHR
          READ( CHDUM(ISTART:ISTOP),FORM1 ) IVAR
        ELSE
          INDX = 4
          CHMSG = 'Excessive Length Integer Record: ' //
     &      VARB(1:IVR) // ': ' // CHDUM(ISTART:ISTOP)
          CALL WRMSGP( INDX )
        ENDIF
        ISTART = ICOMMA + 1
      ENDIF
      IDFLT = 0
!
!---  End of RD_INT group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_GCP
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
!     Read Gas Components Properties Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 27 November 2007.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_GCP'
!
!---  Assign card string  ---
!
      CARD = 'Gas Components Properties Card'
!
!---  Read number of gas components  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Gas Components'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NGC)
      LNGC = MAX( LNGC,NGC )
      LUK = LUK + LNGC
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_GCP group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_GRID
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
!     Read Grid Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 21 November 2002.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE GRID
      USE GEOMECH
      USE FILES
      USE CONST
      USE BCV
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 FDUM,FMDUM,UNTS
      CHARACTER*128 ADUM
      CHARACTER*512 CHDUM
      LOGICAL FCHK
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_GRID'
!
!---  Assign card string  ---
!
      CARD = 'Grid Card'
!
!---  Read coordinate system type  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Coordinate System'
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      IF( INDEX(ADUM(1:),'tilted').NE.0 ) THEN
        ICS = 1
        VARB = 'X-Z Plane Horizontal Tilt'
        CALL RD_DPR(ISTART,ICOMMA,CHDUM,THXZ)
        VARB = 'X-Z Plane Horizontal Tilt Units'
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        INDX = 0
        CALL RD_UNIT(UNTS,THXZ,INDX)
        VARB = 'Y-Z Plane Horizontal Tilt'
        CALL RD_DPR(ISTART,ICOMMA,CHDUM,THYZ)
        VARB = 'Y-Z Plane Horizontal Tilt Units'
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        INDX = 0
        CALL RD_UNIT(UNTS,THYZ,INDX)
      ELSEIF( INDEX(ADUM(1:),'cartesian').NE.0 ) THEN
        IF( INDEX(ADUM(1:),'uniform').NE.0 ) THEN
          ICS = 5
        ELSE
          ICS = 1
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'cylindrical').NE.0 ) THEN
        IF( INDEX(ADUM(1:),'uniform').NE.0 ) THEN
          ICS = 6
        ELSE
          ICS = 2
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'boundary').NE.0 .OR.
     &  INDEX(ADUM(1:),'fitted').NE.0  .OR.
     &  INDEX(ADUM(1:),'orthogonal').NE.0 ) THEN
        ICS = 3
      ELSEIF( INDEX(ADUM(1:),'eclipse').NE.0 .AND.
     &  INDEX(ADUM(1:),'generic').NE.0 ) THEN
        ICS = 7
      ELSEIF( INDEX(ADUM(1:),'earthvision').NE.0 .AND.
     &  INDEX(ADUM(1:),'sampled').NE.0 ) THEN
        ICS = 8
      ELSEIF( INDEX(ADUM(1:),'element').NE.0 .AND.
     &  INDEX(ADUM(1:),'vertices').NE.0 ) THEN
        ICS = 9
      ELSE
        INDX = 4
        CHMSG = 'Unrecognized Coordinate System Type: ' // ADUM
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Read coordinate system node dimensions  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of I-indexed Nodes'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,LFX)
      IF( LFX.LT.1 ) THEN
        INDX = 4
        CHMSG = 'LFX < 1'
        CALL WRMSGP( INDX )
      ENDIF
      VARB = 'Number of J-indexed Nodes'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,LFY)
      IF( LFY.LT.1 ) THEN
        INDX = 4
        CHMSG = 'LFY < 1'
        CALL WRMSGP( INDX )
      ENDIF
      VARB = 'Number of K-indexed Nodes'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,LFZ)
      IF( LFZ.LT.1 ) THEN
        INDX = 4
        CHMSG = 'LFZ < 1'
        CALL WRMSGP( INDX )
      ENDIF
      LAD = 0
      IF( LFX.GT.1 ) LAD = LAD+1
      IF( LFY.GT.1 ) LAD = LAD+1
      IF( LFZ.GT.1 ) LAD = LAD+1
      LAD = MAX( LAD,1 )
      LAN = LFX*LFY*LFZ
      LFXY = LFX*LFY
      LFYZ = LFY*LFZ
      LFZX = LFZ*LFX
      LFD = LFX*LFY*LFZ
      LSTC = 2*LAD+1
!
!---  Allocate memory for the grid geometry arrays  ---
!
      ALLOCATE( XE(1:8,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XE'
        CALL WRMSGP( INDX )
      ENDIF
      ALLOCATE( YE(1:8,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YE'
        CALL WRMSGP( INDX )
      ENDIF
      ALLOCATE( ZE(1:8,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZE'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Allocate memory for the internal boundary conditions  ---
!
      ALLOCATE( INBS(1:8,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: INBS'
        CALL WRMSGP( INDX )
      ENDIF
      DO M0 = 1,8
        DO M1 = 1,LFD
          INBS(M0,M1) = 0
        ENDDO
      ENDDO
!
!---  Allocate memory for the inactive nodes array  ---
!
      ALLOCATE( IXP(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IXP'
        CALL WRMSGP( INDX )
      ENDIF
      DO M0 = 1,LFD
        IXP(M0) = 0
      ENDDO
!
!---  Allocate memory for the i-index nodes array  ---
!
      ALLOCATE( ID(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ID'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Allocate memory for the j-index nodes array  ---
!
      ALLOCATE( JD(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: JD'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Allocate memory for the k-index nodes array  ---
!
      ALLOCATE( KD(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: KD'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Allocate memory for the node index array  ---
!
      ALLOCATE( ND(1:LFX,1:LFY,1:LFZ),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ND'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Allocate memory for the block refinement pointers  ---
!
      ALLOCATE( IBR(1:5,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IBR'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Define the grid pointers  ---
!
      DO 130 K = 1,LFZ
        DO 120 J = 1,LFY
          DO 110 I = 1,LFX
            N = (K-1)*LFXY + (J-1)*LFX + I
            ID(N) = I
            JD(N) = J
            KD(N) = K
            ND(I,J,K) = N
            IBR(1,N) = 0
            IBR(2,N) = 0
            IBR(3,N) = 0
            IBR(4,N) = N
            IBR(5,N) = N
  110     CONTINUE
  120   CONTINUE
  130 CONTINUE
!
!---  Minimum band width parameter according
!     to solver type; parameter LMNP will be reset
!     for coupled wells  ---
!

      IF( LSPILL.EQ.0 ) THEN
        LMNP = MIN( (LFXY),(LFX*LFZ),(LFYZ) )
      ELSE
        LMNP = LFXY
      ENDIF




!
!---  Define active nodes assuming no inactive nodes  ---
!
      NC = 1
      IF( LFXY.LE.LFYZ .AND. LFXY.LE.LFZX ) THEN
!
!---  X-Y Plane yields the lowest band width.
!---  Load Jacobian matrix in the increment order I,J,K
!
        DO 230 K = 1,LFZ
          DO 220 J = 1,LFY
            DO 210 I = 1,LFX
              N = (K-1)*LFXY + (J-1)*LFX + I
              IXP(N) = NC
              NC = NC+1
  210       CONTINUE
  220     CONTINUE
  230   CONTINUE
      ELSEIF( LFYZ.LE.LFXY .AND. LFYZ.LE.LFZX ) THEN
!
!---  Y-Z Plane yields the lowest band width.
!---  Load Jacobian matrix in the increment order J,K,I
!
        DO 330 I = 1,LFX
          DO 320 K = 1,LFZ
            DO 310 J = 1,LFY
              N = (K-1)*LFXY + (J-1)*LFX + I
              IXP(N) = NC
              NC = NC+1
  310       CONTINUE
  320     CONTINUE
  330   CONTINUE
      ELSEIF( LFZX.LE.LFXY .AND. LFZX.LE.LFYZ ) THEN
!
!---  Z-X Plane yields the lowest band width.
!---  Load Jacobian matrix in the increment order K,I,J
!
        DO 430 J = 1,LFY
          DO 420 I = 1,LFX
            DO 410 K = 1,LFZ
              N = (K-1)*LFXY + (J-1)*LFX + I
              IXP(N) = NC
              NC = NC+1
  410       CONTINUE
  420     CONTINUE
  430   CONTINUE
      ENDIF
!
!---  Variable grid spacing  ---
!
      IF( ICS.EQ.1 .OR. ICS.EQ.2 ) THEN
        CALL RD_VBLGRID
!
!---  Uniform grid spacing  ---
!
      ELSEIF( ICS.EQ.5 .OR. ICS.EQ.6 ) THEN
        CALL RD_UNFGRID
!
!---  ECLIPSE formatted grids  ---
!
      ELSEIF( ICS.EQ.7 ) THEN
        CALL RD_ECLGRID
!
!---  Grids defined through hexahedral vertices  ---
!
      ELSEIF( ICS.EQ.3 .OR. ICS.EQ.8 ) THEN
        CALL RD_HEXGRID

!---  Grids defined through hexahedral elements and vertices  ---
!
      ELSEIF( ICS.EQ.9 ) THEN
        CALL RD_ELMGRID
      ENDIF
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_GRID group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_GRP
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
!     Read Gas Relative Permeability Function Parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 11 December 2002.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE TRNSPT
      USE TABL
      USE SOURC
      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*128 ADUM,RDUM
      CHARACTER*512 CHDUM
      TYPE(LIST_NODE), POINTER :: LOC_PTR
      TYPE(LIST_SCALING), POINTER :: SC_LOC_PTR
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_GRP'
!
!---  Assign card string  ---
!
      CARD = 'Gas Relative Permeability Card'
!
!---  Loop over the rock/soil gas relative permeability
!     information lines  ---
!
      N = 0
      IJK = 0
   10 CONTINUE
      IF( N.GE.NROCK .OR. IJK.GT.0 ) GOTO 500
      ISTART = 1
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      VARB = 'Rock/Soil Name'
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,RDUM)
!
!---  Check for a pair of delimiting slashes in the rock/soil name,
!     indicating a pattern of rock/soil types  ---
!
      KBS = 0
      IBS = INDEX( RDUM(1:),'/' )
      IF( IBS.GT.0 ) THEN
        IBS = IBS + 1
        JBS = INDEX( RDUM(IBS:),'/')
        IF( JBS.GT.0 ) THEN
          JBS = IBS + JBS - 2
          KBS = 1
          ISBS = ISTART
        ENDIF
      ENDIF
      IROCK = 1
   20 CONTINUE
!
!---  IJK, KIJ, or JKI indexing  ---
!
      IF( INDEX(RDUM(1:),'indexing').NE.0 ) THEN
        IF( INDEX(RDUM,'ijk').NE.0 ) THEN
          IJK = 1
        ELSEIF( INDEX(RDUM,'jki').NE.0 ) THEN
          IJK = 2
        ELSEIF( INDEX(RDUM,'kij').NE.0 ) THEN
          IJK = 3
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Indexing Option' // RDUM(1:NCH)
          CALL WRMSGP( INDX )
        ENDIF
        IROCK = 1
        GOTO 220
      ENDIF
!
!---  Search known rock types for a matching type  ---
!
          LOC_PTR => ROCK_PTR
          IROCK = 0
          DO
            IF( .NOT.ASSOCIATED(LOC_PTR) ) EXIT
            IROCK = IROCK + 1
            IF( LOC_PTR%LIST_NAME == RDUM ) THEN
              GOTO 200
            ELSE
              LOC_PTR => LOC_PTR%NEXT
            ENDIF
          ENDDO
!
!---  Search known scaling groups for a matching type  ---
!
        IF( ISLC(19).EQ.1 ) THEN
          SC_LOC_PTR => SCALING_PTR
          ISGRP = 0
          DO
            IF( .NOT.ASSOCIATED(SC_LOC_PTR) ) EXIT
            IF( SC_LOC_PTR%SCALING_NAME == RDUM ) THEN
              ISGRP = SC_LOC_PTR%SCALING_NUM
              IROCK = 1
              GOTO 200
            ELSE
              SC_LOC_PTR => SC_LOC_PTR%NEXT
            ENDIF
          ENDDO
          INDX = 2
          CHMSG = 'Unrecognized Rock/Soil Type or Scaling Group: '
     &      // RDUM(1:NCH)
          CALL WRMSGP( INDX )
          GOTO 10
        ENDIF
        INDX = 2
        CHMSG = 'Unrecognized Rock/Soil Type: ' // RDUM(1:NCH)
        CALL WRMSGP( INDX )
        GOTO 10
  200   CONTINUE
!
!---  Loop over rock/soils within scaling group  ---
!
        IF( ISLC(19).EQ.1 .AND. ISGRP.NE.0 ) THEN
          DO 202 M = IROCK,NROCK
            IF( ISCALE(M).EQ.ISGRP ) THEN
              IROCK = M
              GOTO 204
            ENDIF
  202     CONTINUE
        ENDIF
  204   CONTINUE
!
!---  Read gas relative permeability pressure function  ---
!
  220 CONTINUE
      N = N + 1
!
!---  Read saturation/capillary pressure function for
!     tabular forms  ---
!
      VARB = 'Gas Relative Permeability Function'
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---    Tabular (relative permeability versus liquid saturation)  ---
!
      IF( INDEX(ADUM(1:),'tabular') .NE. 0 ) THEN
        IF( INDEX( ADUM(1:),'spline' ).NE.0 ) THEN
          IRPGX = 11
        ELSE
          IRPGX = 10
        ENDIF
!
!---    IJK Indexing  ---
!
        IF( IJK.GT.0 ) THEN
          VARB = 'Number of Tables'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,NTABX)
          NTBL = 0
          DO 240 NTX = 1,NTABX
            ISTART = 1
            CALL RD_INPL( CHDUM )
            CALL L_CASE( CHDUM )
            VARB = 'Number of Table Entries'
            CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
            IF( NLIN.LT.2 ) THEN
              INDX = 4
              CHMSG = 'Invalid Gas Relative Permeability Table'
              CALL WRMSGP( INDX )
            ENDIF
            NTBL = NTBL + NLIN
            DO 230 NLX = 1,NLIN
              CALL RD_INPL( CHDUM )
  230       CONTINUE
  240     CONTINUE
          LTBL = LTBL + NTBL
        ELSE
          VARB = 'Number of Tabular Entries'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
          IF( NLIN.LT.2 ) THEN
            INDX = 4
            CHMSG = 'Invalid Gas Relative Permeability Table'
            CALL WRMSGP( INDX )
          ENDIF
          DO 250 NL = 1,NLIN
            CALL RD_INPL( CHDUM )
 250      CONTINUE
          IF( IJK.GT.0 ) THEN
            NTBL = LFD*NLIN
          ELSE
            NTBL = NLIN
          ENDIF
          LTBL = LTBL + NTBL
        ENDIF
      ENDIF
!
!---  Continue reading rock/soil type names for a pattern match  ---
!
      IF( KBS.EQ.1 .AND. IROCK.LT.NROCK ) THEN
        IROCK = IROCK + 1
        ISTART = ISBS
        GOTO 20
      ENDIF
!
!---  Loop over remaining rock/soils within scaling group  ---
!
      IF( ISLC(19).EQ.1 .AND. IROCK.LT.NROCK ) THEN
        DO 490 M = IROCK+1,NROCK
          IF( ISCALE(M).EQ.ISGRP ) THEN
            N = N+1
          ENDIF
  490   CONTINUE
      ENDIF
!
!---  Read next rock/soil type or scaling group  ---
!
      GOTO 10
 500  CONTINUE
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_GRP group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_GSSP
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
!     Written by MD White, PNNL, 16 August 2005.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
      USE REACT
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*128 ADUM
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_GSSP'
!
!---  Assign card string  ---
!
      CARD = 'Gas Species Card'
!
!---  Read number of equilibrium reactions  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Gas Species'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NSPG)
      LSPG = MAX( LSPG,NSPG )
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_GSSP group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_HEXGRID
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
!     Read hexahedral grid.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 December 2011.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GRID
      USE FILES
      USE CONST
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      REAL*8, DIMENSION(:,:,:), ALLOCATABLE :: VX
      CHARACTER*64 ADUM,FDUM,FMDUM,UNTS
      CHARACTER*512 CHDUM
      LOGICAL FCHK
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_HEXGRID'
!
!---  Read vertices file name  ---
!
      ISTART = 1
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      VARB = 'Vertices File Name'
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
!
!---  Check for external file  ---
!
      INQUIRE( FILE=FDUM(1:NCH), FORM=FMDUM, EXIST=FCHK )
      IF( .NOT.FCHK ) THEN
        INDX = 4
        CHMSG = 'Missing Vertices File: ' // FDUM(1:NCH)
        CALL WRMSGP( INDX )
      ELSEIF( FDUM.EQ.'unformatted' ) THEN
        INDX = 4
        CHMSG = 'Vertices File Format: ' // FDUM(1:NCH)
        CALL WRMSGP( INDX )
      ENDIF
      OPEN( UNIT=9,FILE=FDUM(1:NCH),STATUS='OLD',FORM='FORMATTED' )
      REWIND( UNIT=9 )
      READ(9,*) IFLDX,JFLDX,KFLDX
      IFLDZ = IFLDX
      JFLDZ = JFLDX
      KFLDZ = KFLDX
      IF(IFLDX.GT.1) IFLDZ = IFLDX-1
      IF(JFLDX.GT.1) JFLDZ = JFLDX-1
      IF(KFLDX.GT.1) KFLDZ = KFLDX-1
      IF( IFLDZ.NE.LFX .OR. JFLDZ.NE.LFY .OR. KFLDZ.NE.LFZ ) THEN
        INDX = 4
        CHMSG = 'Orthogonal Grid Dimension Conflict'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Dynamic memory allocation  ---
!
      ALLOCATE( VX(1:IFLDX,1:JFLDX,1:KFLDX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VX'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Read length units  ---
!
      VARB = 'Grid File Length Units'
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      INDX = 0
      VARX = 1.D+0
      IUNM = 1
      CALL RD_UNIT(UNTS,VARX,INDX)
!
!---  Read x-vertices from file  ---
!
      IF( IFLDX.GT.1 ) THEN
        READ(9,*)(((VX(I,J,K),I=1,IFLDX),J=1,JFLDX),K=1,KFLDX)
!
!---    3D-xyz domain  ---
!
        IF( JFLDX.GT.1 .AND. KFLDX.GT.1 ) THEN
          DO 100 K = 1,LFZ
          DO 100 J = 1,LFY
          DO 100 I = 1,LFX
            N = ND(I,J,K)
            XE(1,N) = VX(I,J,K)*VARX
            XE(2,N) = VX(I+1,J,K)*VARX
            XE(3,N) = VX(I,J+1,K)*VARX
            XE(4,N) = VX(I+1,J+1,K)*VARX
            XE(5,N) = VX(I,J,K+1)*VARX
            XE(6,N) = VX(I+1,J,K+1)*VARX
            XE(7,N) = VX(I,J+1,K+1)*VARX
            XE(8,N) = VX(I+1,J+1,K+1)*VARX
  100     CONTINUE
!
!---    2D-xy domain, unit z dimension  ---
!
        ELSEIF( JFLDX.GT.1 ) THEN
          DO 110 J = 1,LFY
          DO 110 I = 1,LFX
            N = ND(I,J,1)
            XE(1,N) = VX(I,J,1)*VARX
            XE(2,N) = VX(I+1,J,1)*VARX
            XE(3,N) = VX(I,J+1,1)*VARX
            XE(4,N) = VX(I+1,J+1,1)*VARX
            XE(5,N) = VX(I,J,1)*VARX
            XE(6,N) = VX(I+1,J,1)*VARX
            XE(7,N) = VX(I,J+1,1)*VARX
            XE(8,N) = VX(I+1,J+1,1)*VARX
  110     CONTINUE
!
!---    2D-xz domain, unit y dimension  ---
!
        ELSEIF( KFLDX.GT.1 ) THEN
          DO 120 K = 1,LFZ
          DO 120 I = 1,LFX
            N = ND(I,1,K)
            XE(1,N) = VX(I,1,K)*VARX
            XE(2,N) = VX(I+1,1,K)*VARX
            XE(3,N) = VX(I,1,K)*VARX
            XE(4,N) = VX(I+1,1,K)*VARX
            XE(5,N) = VX(I,1,K+1)*VARX
            XE(6,N) = VX(I+1,1,K+1)*VARX
            XE(7,N) = VX(I,1,K+1)*VARX
            XE(8,N) = VX(I+1,1,K+1)*VARX
  120     CONTINUE
!
!---    1D-x domain  ---
!
        ELSE
          INDX = 4
          CHMSG = 'Single Dimensioned Orthogonal Grid'
          CALL WRMSGP( INDX )
        ENDIF
!
!---  2D-yz domain, unit x dimension  ---
!
      ELSE
        DO 130 N = 1,LFD
          XE(1,N) = 0.D+0
          XE(2,N) = 1.D+0
          XE(3,N) = 0.D+0
          XE(4,N) = 1.D+0
          XE(5,N) = 0.D+0
          XE(6,N) = 1.D+0
          XE(7,N) = 0.D+0
          XE(8,N) = 1.D+0
  130   CONTINUE
      ENDIF
!
!---  Read y-vertices from file  ---
!
      IF( JFLDX.GT.1 ) THEN
        READ(9,*)(((VX(I,J,K),I=1,IFLDX),J=1,JFLDX),K=1,KFLDX)
!
!---    3D-xyz domain  ---
!
        IF( IFLDX.GT.1 .AND. KFLDX.GT.1 ) THEN
          DO 200 K = 1,LFZ
          DO 200 J = 1,LFY
          DO 200 I = 1,LFX
            N = ND(I,J,K)
            YE(1,N) = VX(I,J,K)*VARX
            YE(2,N) = VX(I+1,J,K)*VARX
            YE(3,N) = VX(I,J+1,K)*VARX
            YE(4,N) = VX(I+1,J+1,K)*VARX
            YE(5,N) = VX(I,J,K+1)*VARX
            YE(6,N) = VX(I+1,J,K+1)*VARX
            YE(7,N) = VX(I,J+1,K+1)*VARX
            YE(8,N) = VX(I+1,J+1,K+1)*VARX
  200     CONTINUE
!
!---    2D-xy domain, unit z dimension  ---
!
        ELSEIF( IFLDX.GT.1 ) THEN
          DO 210 J = 1,LFY
          DO 210 I = 1,LFX
            N = ND(I,J,1)
            YE(1,N) = VX(I,J,1)*VARX
            YE(2,N) = VX(I+1,J,1)*VARX
            YE(3,N) = VX(I,J+1,1)*VARX
            YE(4,N) = VX(I+1,J+1,1)*VARX
            YE(5,N) = VX(I,J,1)*VARX
            YE(6,N) = VX(I+1,J,1)*VARX
            YE(7,N) = VX(I,J+1,1)*VARX
            YE(8,N) = VX(I+1,J+1,1)*VARX
  210     CONTINUE
!
!---    2D-yz domain, unit x dimension  ---
!
        ELSEIF( KFLDX.GT.1 ) THEN
          DO 220 K = 1,LFZ
          DO 220 J = 1,LFY
            N = ND(1,J,K)
            YE(1,N) = VX(1,J,K)*VARX
            YE(2,N) = VX(1,J,K)*VARX
            YE(3,N) = VX(1,J+1,K)*VARX
            YE(4,N) = VX(1,J+1,K)*VARX
            YE(5,N) = VX(1,J,K+1)*VARX
            YE(6,N) = VX(1,J,K+1)*VARX
            YE(7,N) = VX(1,J+1,K+1)*VARX
            YE(8,N) = VX(1,J+1,K+1)*VARX
  220     CONTINUE
!
!---    1D-y domain  ---
!
        ELSE
          INDX = 4
          CHMSG = 'Single Dimensioned Orthogonal Grid'
          CALL WRMSGP( INDX )
        ENDIF
!
!---  2D-xz domain, unit y dimension  ---
!
      ELSE
        DO 230 N = 1,LFD
          YE(1,N) = 0.D+0
          YE(2,N) = 0.D+0
          YE(3,N) = 1.D+0
          YE(4,N) = 1.D+0
          YE(5,N) = 0.D+0
          YE(6,N) = 0.D+0
          YE(7,N) = 1.D+0
          YE(8,N) = 1.D+0
  230   CONTINUE
      ENDIF
!
!---  Read z-vertices from file  ---
!
      IF( KFLDX.GT.1 ) THEN
        READ(9,*)(((VX(I,J,K),I=1,IFLDX),J=1,JFLDX),K=1,KFLDX)
!
!---    3D-xyz domain  ---
!
        IF( IFLDX.GT.1 .AND. JFLDX.GT.1 ) THEN
          DO 300 K = 1,LFZ
          DO 300 J = 1,LFY
          DO 300 I = 1,LFX
            N = ND(I,J,K)
            ZE(1,N) = VX(I,J,K)*VARX
            ZE(2,N) = VX(I+1,J,K)*VARX
            ZE(3,N) = VX(I,J+1,K)*VARX
            ZE(4,N) = VX(I+1,J+1,K)*VARX
            ZE(5,N) = VX(I,J,K+1)*VARX
            ZE(6,N) = VX(I+1,J,K+1)*VARX
            ZE(7,N) = VX(I,J+1,K+1)*VARX
            ZE(8,N) = VX(I+1,J+1,K+1)*VARX
  300     CONTINUE
!
!---    2D-yz domain, unit x dimension  ---
!
        ELSEIF( JFLDX.GT.1 ) THEN
          DO 310 K = 1,LFZ
          DO 310 J = 1,LFY
            N = ND(1,J,K)
            ZE(1,N) = VX(1,J,K)*VARX
            ZE(2,N) = VX(1,J,K)*VARX
            ZE(3,N) = VX(1,J+1,K)*VARX
            ZE(4,N) = VX(1,J+1,K)*VARX
            ZE(5,N) = VX(1,J,K+1)*VARX
            ZE(6,N) = VX(1,J,K+1)*VARX
            ZE(7,N) = VX(1,J+1,K+1)*VARX
            ZE(8,N) = VX(1,J+1,K+1)*VARX
  310     CONTINUE
!
!---    2D-xz domain, unit y dimension  ---
!
        ELSEIF( KFLDX.GT.1 ) THEN
          DO 320 K = 1,LFZ
          DO 320 I = 1,LFX
            N = ND(I,1,K)
            ZE(1,N) = VX(I,1,K)*VARX
            ZE(2,N) = VX(I+1,1,K)*VARX
            ZE(3,N) = VX(I,1,K)*VARX
            ZE(4,N) = VX(I+1,1,K)*VARX
            ZE(5,N) = VX(I,1,K+1)*VARX
            ZE(6,N) = VX(I+1,1,K+1)*VARX
            ZE(7,N) = VX(I,1,K+1)*VARX
            ZE(8,N) = VX(I+1,1,K+1)*VARX
  320     CONTINUE
!
!---    1D-z domain  ---
!
        ELSE
          INDX = 4
          CHMSG = 'Single Dimensioned Orthogonal Grid'
          CALL WRMSGP( INDX )
        ENDIF
!
!---  2D-xy domain, unit z dimension  ---
!
      ELSE
        DO 330 N = 1,LFD
          ZE(1,N) = 0.D+0
          ZE(2,N) = 0.D+0
          ZE(3,N) = 0.D+0
          ZE(4,N) = 0.D+0
          ZE(5,N) = 1.D+0
          ZE(6,N) = 1.D+0
          ZE(7,N) = 1.D+0
          ZE(8,N) = 1.D+0
  330   CONTINUE
      ENDIF
!
!---  Dynamic memory deallocation  ---
!
      DEALLOCATE( VX,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: VX'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Closes Vertices File  ---
!
      CLOSE(UNIT=9)
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_HEXGRID group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_INAC
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
!     Read Inactive Node Parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 November 2002.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*128 ADUM
      CHARACTER*512 CHDUM
      TYPE(LIST_NODE), POINTER :: LOC_PTR
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_INAC'
!
!---  Assign card string  ---
!
      CARD = 'Inactive Nodes Card'
!
!---  Read optional number of inactive node card entries, or
!     read first line of single entry  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Multiple Entries Switch'
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      NDOM = 1
!
!---  Read boundary surface printing  ---
!
      IF( INDEX(ADUM,'boundary').NE.0 .AND.
     &  INDEX(ADUM,'surface').NE.0 ) THEN
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
        VARB = 'Multiple Entries Switch'
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      ENDIF
!
!---  Read number of multiple entries and first line
!     of first entry  ---
!
      IF( INDEX(ADUM,'multiple').NE.0 ) THEN
        VARB = 'Number of Entries'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,NDOM)
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
      ENDIF
      DO 450 NDM = 1,NDOM
      ISTART = 1
!
!---  Read first line for more than one entry  ---
!
      IF( NDM.GT.1 ) THEN
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
      ENDIF
      VARB = 'Input Option [Rock/Soil, Zonation File, File, Integer]'
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---  Read inactive node information according to rock/soil type  ---
!
      IF( INDEX(ADUM,'rock').NE.0 .OR. INDEX(ADUM,'soil').NE.0 ) THEN
        VARB = 'Number of Rock/Soil Type Lines'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
        DO 60 L = 1,NLIN
          ISTART = 1
          CALL RD_INPL( CHDUM )
          CALL L_CASE( CHDUM )
          VARB = 'Rock/Soil Name'
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---  Search known rock types for a matching type ---
!
          LOC_PTR => ROCK_PTR
          IROCK = NROCK+1
          DO
            IF( .NOT.ASSOCIATED(LOC_PTR) ) EXIT
            IROCK = IROCK - 1
            IF( LOC_PTR%LIST_NAME == ADUM ) THEN
              GOTO 40
            ELSE
              LOC_PTR => LOC_PTR%NEXT
            ENDIF
          ENDDO
          INDX = 2
          CHMSG = 'Unrecognized Rock/Soil Type: '//ADUM(1:NCH)
          CALL WRMSGP( INDX )
          GOTO 60
   40     CONTINUE
          DO 50 N = 1,LFD
            IF( IZ(N).EQ.IROCK ) THEN
              NXP = NXP + 1
              IXP(N) = 0
            ENDIF
   50     CONTINUE
   60   CONTINUE
!
!---  Read inactive node information from an external zonation file  ---
!
      ELSEIF( INDEX(ADUM,'zonation').NE.0 .AND.
     &  INDEX(ADUM,'file').NE.0 ) THEN
        VARB = 'Rock/soil zonation external file name'
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(CHDUM,'formatted').NE.0 ) THEN
          OPEN( UNIT=27,FILE=ADUM(1:NCH),STATUS='OLD',FORM='FORMATTED' )
          READ(27,*)(IZ(N),N=1,LFD)
          CLOSE(UNIT=27)
        ELSE
          OPEN( UNIT=27,FILE=ADUM(1:NCH),STATUS='OLD',
     &      FORM='UNFORMATTED' )
          READ(27)(IZ(N),N=1,LFD)
          CLOSE(UNIT=27)
        ENDIF
        DO 70 N = 1,LFD
          IF( IZ(N).EQ.0 ) THEN
            NXP = NXP + 1
            IXP(N) = 0
          ENDIF
   70   CONTINUE
!
!---  Read inactive node information from an external file  ---
!
      ELSEIF( INDEX(ADUM,'file').NE.0 ) THEN
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        NCH = INDEX(ADUM,'  ')-1
        OPEN(UNIT=27, FILE=ADUM(1:NCH), STATUS='OLD', FORM='FORMATTED')
   80   CONTINUE
        READ(27,*,END=90) I,J,K
        IF( I.LT.1 .OR. I.GT.LFX .OR. J.LT.1 .OR. J.GT.LFY
     &    .OR. K.LT.1 .OR. K.GT.LFZ ) THEN
          INDX = 4
          CHMSG = 'Inactive Node Index Out of Range'
          CALL WRMSGP(INDX)
        ENDIF
        NXP = NXP + 1
        N = (K-1)*LFXY + (J-1)*LFX + I
        IXP(N) = 0
        GOTO 80
   90   CONTINUE
        CLOSE(UNIT=27)
      ELSE
!
!---  Read inactive node information from the input file  ---
!
        VARB = 'Number of Inactive Node Lines'
        ISTART = 1
        CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
        DO 400 NL = 1, NLIN
          ISTART = 1
          CALL RD_INPL( CHDUM )
          CALL L_CASE( CHDUM )
          VARB = 'Inactive Node Domain Index'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,I1)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,I2)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,J1)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,J2)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,K1)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,K2)
          I1 = MAX( 1,I1 )
          I1 = MIN( I1,I2,LFX )
          I2 = MAX( 1,I1,I2 )
          I2 = MIN( I2,LFX )
          J1 = MAX( 1,J1 )
          J1 = MIN( J1,J2,LFY )
          J2 = MAX( 1,J1,J2 )
          J2 = MIN( J2,LFY )
          K1 = MAX( 1,K1 )
          K1 = MIN( K1,K2,LFZ )
          K2 = MAX( 1,K1,K2 )
          K2 = MIN( K2,LFZ )
          DO 300 K = K1,K2
            DO 200 J = J1,J2
              DO 100 I = I1,I2
                N = (K-1)*LFXY + (J-1)*LFX + I
                IF( IXP(N).NE.0 ) THEN
                  NXP = NXP + 1
                  IXP(N) = 0
                ENDIF
  100         CONTINUE
  200       CONTINUE
  300     CONTINUE
  400   CONTINUE
      ENDIF
  450 CONTINUE
!
!---  Redefine active nodes parameter ---
!
      LAN = LFD - NXP
      NC = 1
!
!---  X-Y Plane yields the lowest band width or
!     active surface spill nodes,
!---  load Jacobian matrix in the increment order I,J,K  ---
!
      IF( (LFXY.LE.LFYZ .AND. LFXY.LE.LFZX) .OR. LSPILL.EQ.1 ) THEN
        DO 530 K = 1,LFZ
          DO 520 J = 1,LFY
            DO 510 I = 1,LFX
              N = (K-1)*LFXY + (J-1)*LFX + I
              IF( IXP(N).NE.0 ) THEN
                IXP(N) = NC
                NC = NC+1
              ENDIF
  510       CONTINUE
  520     CONTINUE
  530   CONTINUE
!
!---  Y-Z Plane yields the lowest band width,
!---  load Jacobian matrix in the increment order J,K,I  ---
!
      ELSEIF( LFYZ.LE.LFXY .AND. LFYZ.LE.LFZX ) THEN
        DO 630 I = 1,LFX
          DO 620 K = 1,LFZ
            DO 610 J = 1,LFY
              N = (K-1)*LFXY + (J-1)*LFX + I
              IF( IXP(N).NE.0 ) THEN
                IXP(N) = NC
                NC = NC+1
              ENDIF
  610       CONTINUE
  620     CONTINUE
  630   CONTINUE
!
!---  Z-X Plane yields the lowest band width,
!---  load Jacobian matrix in the increment order K,I,J  ---
!
      ELSEIF( LFZX.LE.LFXY .AND. LFZX.LE.LFYZ ) THEN
        DO 730 J = 1,LFY
          DO 720 I = 1,LFX
            DO 710 K = 1,LFZ
              N = (K-1)*LFXY + (J-1)*LFX + I
              IF( IXP(N).NE.0 ) THEN
                IXP(N) = NC
                NC = NC+1
              ENDIF
  710       CONTINUE
  720     CONTINUE
  730   CONTINUE
      ENDIF
  900 CONTINUE
!
!---  Define upper active node for surface spills  ---
!
      KSPSX = 0
      IF( LSPILL.EQ.1 ) THEN
        DO 930 J = 1,LFX
          DO 920 I = 1,LFY
            DO 910 K = LFZ,1,-1
              N = (K-1)*LFXY + (J-1)*LFX + I
              IF( IXP(N).NE.0 ) THEN
                KSPSX = MAX( KSPSX,LFZ-K )
                GOTO 920
              ENDIF
  910       CONTINUE
  920     CONTINUE
  930   CONTINUE
!
!---    Reset minimum band width for inactive nodes below
!       spill surfaces  --
!

        LMNP = (KSPSX+1)*LFXY

      ENDIF
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_INAC group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_KNEQ
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
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 9 December 2004.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
      USE REACT
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*128 ADUM
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_KNEQ'
!
!---  Assign card string  ---
!
      CARD = 'Kinetic Equations Card'
!
!---  Read number of kinetic equations  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Kinetic Equations'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NEQK)
      LEQK = MAX( LEQK,NEQK )
      LSPT = LSPT + LEQK
!
!---  Loop over the kinetic reactions  ---
!
      DO 100 NEQ = 1,NEQK
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
        VARB = 'Kinetic Component Species Name'
        CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
        VARB = 'Number of Species in Kinetic Equation'
        CALL RD_INT( ISTART,ICOMMA,CHDUM,NSEK )
        LSEK = MAX( LSEK,NSEK )
!
!---    Loop over the kinetic-equation species allowing for
!       returns in the input  ---
!
        DO 80 NSP = 1,NSEK
          VARB = 'Kinetic-Equation Species Name: '
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RD_INPL( CHDUM )
            CALL L_CASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
          VARB = 'Kinetic-Equation Species Coefficient: '
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RD_INPL( CHDUM )
            CALL L_CASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
   80   CONTINUE
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
        VARB = 'Number of Reactions in Kinetic Equation'
        CALL RD_INT( ISTART,ICOMMA,CHDUM,NREK )
        LREK = MAX( LREK,NREK )
!
!---    Loop over the kinetic-equation kinetic reactions
!        allowing for returns in the input  ---
!
        DO 90 NSP = 1,NREK
          VARB = 'Kinetic-Equation Kinetic-Reaction Name: '
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RD_INPL( CHDUM )
            CALL L_CASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
          VARB = 'Kinetic-Equation Kinetic-Reaction Coefficient: '
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RD_INPL( CHDUM )
            CALL L_CASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
   90   CONTINUE
  100 CONTINUE
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_KNEQ group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_KNRC
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
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 9 December 2004.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
      USE REACT
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 GDUM
      CHARACTER*128 ADUM,FDUM
      CHARACTER*512 CHDUM
      LOGICAL FLG_CHK
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_KNRC'
!
!---  Assign card string  ---
!
      CARD = 'Kinetic Reactions Card'
!
!---  Read number of equilibrium reactions  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Kinetic Reactions'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NRCK)
      LRCK = MAX( LRCK,NRCK )
      LCKN = 1
!
!---  Loop over the kinetic reactions  ---
!
      DO 500 NRC = 1,NRCK
        JCX = 0
        IRCKTX = 0
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
        VARB = 'Kinetic Reaction Name'
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        VARB = 'Kinetic Reaction Type'
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'dissolu').NE.0 .OR.
     &    INDEX(ADUM(1:),'precip').NE.0 .OR.
     &    INDEX(ADUM(1:),'tst').NE.0 ) THEN
          IF( INDEX(ADUM(1:),'ph').NE.0 ) THEN
            IRCKTX = 5
            IF( INDEX(ADUM(1:),'toward products').NE.0 ) THEN
              IRCKTX = 6
            ELSEIF( INDEX(ADUM(1:),'toward reactants').NE.0 ) THEN
              IRCKTX = 7
            ENDIF
          ELSE
            IRCKTX = 10
            IF( INDEX(ADUM(1:),'toward products').NE.0 ) THEN
              IRCKTX = 11
            ELSEIF( INDEX(ADUM(1:),'toward reactants').NE.0 ) THEN
              IRCKTX = 12
            ENDIF
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'multi').NE.0 .AND.
     &    INDEX(ADUM(1:),'rate').NE.0 ) THEN
          IRCKTX = 20
        ELSEIF( INDEX(ADUM(1:),'forward').NE.0 .OR. 
     &    INDEX(ADUM(1:),'backward').NE.0 ) THEN
          IRCKTX = 1
        ELSEIF( INDEX(ADUM(1:),'monod').NE.0 .AND.
     &    INDEX(ADUM(1:),'valocchi').NE.0 ) THEN
          IRCKTX = 2
        ELSEIF( INDEX(ADUM(1:),'sorption').NE.0 .AND.
     &    INDEX(ADUM(1:),'valocchi').NE.0 ) THEN
          IRCKTX = 3
        ELSEIF( INDEX(ADUM(1:),'sorption').NE.0 .AND.
     &    INDEX(ADUM(1:),'langmuir').NE.0 ) THEN
          IRCKTX = 13
        ELSEIF( INDEX(ADUM(1:),'biomass').NE.0 .AND.
     &    INDEX(ADUM(1:),'valocchi').NE.0 ) THEN
          IRCKTX = 4
        ELSEIF( INDEX(ADUM(1:),'monod').NE.0 ) THEN
          IRCKTX = 22
        ELSEIF( INDEX(ADUM(1:),'biomass').NE.0 ) THEN
          IRCKTX = 24
        ELSEIF( INDEX(ADUM(1:),'lognormal').NE.0 .AND.
     &    INDEX(ADUM(1:),'liu').NE.0 ) THEN
          IRCKTX = 41
        ELSEIF( INDEX(ADUM(1:),'dualdomain').NE.0 .AND.
     &    INDEX(ADUM(1:),'liu').NE.0 ) THEN
          IRCKTX = 42
        ENDIF
        IF ( INDEX(ADUM(1:),'w/coef').NE.0 ) THEN
          LMC = LFX*LFY*LFZ
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
        ELSE
          LMC = 1
        ENDIF
!
!---    Mineral  ---
!
        IF( (IRCKTX.GE.10 .AND. IRCKTX.LE.12) .OR.
     &    (IRCKTX.GE.5 .AND. IRCKTX.LE.9) .OR.
     &    IRCKTX.EQ.14 .OR. IRCKTX.EQ.16 .OR. IRCKTX.EQ.20 ) THEN
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RD_INPL( CHDUM )
            CALL L_CASE( CHDUM )
            ISTART = 1
          ENDIF
          VARB = 'Mineral Name'
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
        ENDIF
!
!---    Multi-rate mineral  ---
!
        IF( IRCKTX.EQ.20 ) GOTO 400
!
!---    Number of reactants in kinetic reaction  ---
!
        VARB = 'Number of Reactants in Kinetic Reaction'
        CALL RD_INT( ISTART,ICOMMA,CHDUM,NSPRX )
!
!---    Loop over the kinetic reaction reactants allowing for
!       returns in the input  ---
!
        DO 10 NSP = 1,NSPRX
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RD_INPL( CHDUM )
            CALL L_CASE( CHDUM )
            ISTART = 1
          ENDIF
          VARB = 'Kinetic-Reaction Reactants Name: '
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Skip kinetic reaction reactant stoichiometric coefficient
!         for Monod and Biomass kinetics  ---
!
          IF( IRCKTX.NE.22 .AND. IRCKTX.NE.24 ) THEN
!
!---        Allow for returns in input lines  ---
!
            CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              CALL RD_INPL( CHDUM )
              CALL L_CASE( CHDUM )
              ISTART = 1
            ENDIF
            JCX = JCX+1
            VARB = 'Kinetic-Reaction Reactant Stoichiometric ' // 
     &        'Coefficient: '
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
              NCH  = INDEX(FDUM,':')+1
              NCHF = INDEX(FDUM,'  ')-1
!             
!---          Check that the file exists  ---
!             
              INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
              IF( .NOT.FLG_CHK ) THEN
                INDX = 4
                CHMSG = 'Kinetic-Reaction Reactant Stoichiometric ' //
     &            'Coefficient: ' // FDUM(NCH:NCHF)
                CALL WRMSGP( INDX )
              ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
                INDX = 4
                CHMSG = 'Unformatted Kinetic-Reaction Reactant ' //
     &            'Stoichiometric Coefficient: ' // FDUM(NCH:NCHF)
                CALL WRMSGP( INDX )
              ENDIF
              NCKN = LFX*LFY*LFZ
            ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          ENDIF
   10   CONTINUE
!
!---    Skip kinetic reaction products
!       for Monod and Biomass kinetics  ---
!
        IF( IRCKTX.NE.22 .AND. IRCKTX.NE.24 ) THEN
          VARB = 'Number of Products in Kinetic Reaction'
          CALL RD_INT( ISTART,ICOMMA,CHDUM,NSPPX )
!
!---      Loop over the kinetic reaction products allowing for
!         returns in the input  ---
!
          DO 20 NSP = 1,NSPPX
!
!---        Allow for returns in input lines  ---
!
            CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              CALL RD_INPL( CHDUM )
              CALL L_CASE( CHDUM )
              ISTART = 1
            ENDIF
            VARB = 'Kinetic-Reaction Products Name: '
            CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---        Allow for returns in input lines  ---
!
            CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              CALL RD_INPL( CHDUM )
              CALL L_CASE( CHDUM )
              ISTART = 1
            ENDIF
            JCX = JCX+1
            VARB = 'Kinetic-Reaction Product Stoichiometric ' // 
     &        'Coefficient: '
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
              NCH  = INDEX(FDUM,':')+1
              NCHF = INDEX(FDUM,'  ')-1
!
!---          Check that the file exists  ---
!
              INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
              IF( .NOT.FLG_CHK ) THEN
                INDX = 4
                CHMSG = 'Kinetic-Reaction Reactant Stoichiometric ' //
     &            'Coefficient: ' // FDUM(NCH:NCHF)
                CALL WRMSGP( INDX )
              ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
                INDX = 4
                CHMSG = 'Unformatted Kinetic-Reaction Reactant ' //
     &            'Stoichiometric Coefficient: ' // FDUM(NCH:NCHF)
                CALL WRMSGP( INDX )
              ENDIF
              NCKN = LFX*LFY*LFZ
            ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
            ENDIF
   20     CONTINUE
        ENDIF
!
!---    Read kinetic reaction parameters  ---
!
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
!
!---    TST type reactions  ---
!
        IF( (IRCKTX.GE.10 .AND. IRCKTX.LE.12) .OR.
     &    (IRCKTX.GE.5 .AND. IRCKTX.LE.7) ) THEN
!
!---      Read forward dissolution-precipitation
!         reference reaction rate  ---
!
          JCX = JCX+1
          VARB = 'Kinetic Reaction Reference Rate'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
!
!---        Check that the file exists  ---
!
            INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
            IF( .NOT.FLG_CHK ) THEN
              INDX = 4
              CHMSG = 'Kinetic-Reaction Reference Rate: ' 
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
              INDX = 4
              CHMSG = 'Kinetic-Reaction Reference Rate: ' 
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ENDIF
            NCKN = LFX*LFY*LFZ
          ELSE
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read activation energy  ---
!
          JCX = JCX+1
          VARB = 'Kinetic Reaction Activation Energy'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
!
!---        Check that the file exists  ---
!
            INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
            IF( .NOT.FLG_CHK ) THEN
              INDX = 4
              CHMSG = 'Kinetic-Reaction Activation Energy: '
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
              INDX = 4
              CHMSG = 'Kinetic-Reaction Activation Energy: '
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ENDIF
            NCKN = LFX*LFY*LFZ
          ELSE
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read forward dissolution-precipitation
!         reference reaction temperature  ---
!
          JCX = JCX+1
          VARB = 'Kinetic Reaction Reference Temperature'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
!
!---        Check that the file exists  ---
!
            INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
            IF( .NOT.FLG_CHK ) THEN
              INDX = 4
              CHMSG = 'Kinetic-Reaction Reference Temperature: '
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
              INDX = 4
              CHMSG = 'Kinetic-Reaction Reference Temperature: '
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ENDIF
            NCKN = LFX*LFY*LFZ
          ELSE
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read dissolution-precipitation
!         pH exponent  ---
!
          IF( IRCKTX.GE.5 .AND. IRCKTX.LE.9 ) THEN
            JCX = JCX+1
            VARB = 'Kinetic Reaction pH Exponent'
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
              NCH  = INDEX(FDUM,':')+1
              NCHF = INDEX(FDUM,'  ')-1
!        
!---          Check that the file exists  ---
!        
              INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
              IF( .NOT.FLG_CHK ) THEN
                INDX = 4
                CHMSG = 'Kinetic-Reaction pH Exponent: '
     &           // FDUM(NCH:NCHF)
                CALL WRMSGP( INDX )
              ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
                INDX = 4
                CHMSG = 'Kinetic-Reaction pH Exponent: '
     &           // FDUM(NCH:NCHF)
                CALL WRMSGP( INDX )
              ENDIF
              NCKN = LFX*LFY*LFZ
            ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          ENDIF
!
!---      Read equilibrium constant function coefficients
!         where, log(K) = b1*ln(T) + b2 + b3*T + b4/T + b5/(T^2)
!         and the equilibrium constant relates the aqueous
!         activity-molality product, gas fugacity, and
!         mineral activity  ---
!
          CALL RD_INPL( CHDUM )
          CALL L_CASE( CHDUM )
          ISTART = 1
          DO 300 M = 1,5
!
!---        Read equilibrium constant function coefficients  ---
!
            JCX = JCX+1
            VARB = 'Equilibrium Reaction Constant Coefficient'
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
              NCH  = INDEX(FDUM,':')+1
              NCHF = INDEX(FDUM,'  ')-1
!           
!---          Check that the file exists  ---
!           
              INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
              IF( .NOT.FLG_CHK ) THEN
                INDX = 4
                CHMSG = 'Equilibrium Reaction Constant Coefficient: '
     &           // FDUM(NCH:NCHF)
                CALL WRMSGP( INDX )
              ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
                INDX = 4
                CHMSG = 'Equilibrium Reaction Constant Coefficient: '
     &           // FDUM(NCH:NCHF)
                CALL WRMSGP( INDX )
              ENDIF
              NCKN = LFX*LFY*LFZ
            ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
            ENDIF
  300     CONTINUE
!
!---    Forward-backward type reactions  ---
!
        ELSEIF( IRCKTX.EQ.1 ) THEN
!
!---      Read forward reaction rate  ---
!
          JCX = JCX+1
          VARB = 'Forward Kinetic Reaction Rate Exponent'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
!
!---        Check that the file exists  ---
!
            INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
            IF( .NOT.FLG_CHK ) THEN
              INDX = 4
              CHMSG = 'Forward Kinetic Reaction Rate Exponent: '
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
              INDX = 4
              CHMSG = 'Forward Kinetic Reaction Rate Exponent: '
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ENDIF
            NCKN = LFX*LFY*LFZ
          ELSE
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read backward reaction rate  ---
!
          JCX = JCX+1
          VARB = 'Backward Kinetic Reaction Rate Exponent'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
!
!---        Check that the file exists  ---
!
            INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
            IF( .NOT.FLG_CHK ) THEN
              INDX = 4
              CHMSG = 'Backward Kinetic Reaction Rate Exponent '
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
              INDX = 4
              CHMSG = 'Backward Kinetic Reaction Rate Exponent '
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ENDIF
            NCKN = LFX*LFY*LFZ
          ELSE
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---    Valocchi-Monod type reactions  ---
!
        ELSEIF( IRCKTX.EQ.2 ) THEN
!
!---      Read half-saturation constant for donor  ---
!
          JCX = JCX+1
          VARB = 'Valocchi-Monod Kinetic Reaction: ' //
     &      'Half-Saturation Constant for Donor'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
!
!---        Check that the file exists  ---
!
            INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
            IF( .NOT.FLG_CHK ) THEN
              INDX = 4
              CHMSG = 'Valochhi-Monod Kinetic Reaction: '
     &         // 'Half-Saturation Constant for Donor' 
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
              INDX = 4
              CHMSG = 'Valochhi-Monod Kinetic Reaction: '
     &         // 'Half-Saturation Constant for Donor' 
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ENDIF
            NCKN = LFX*LFY*LFZ
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read half-saturation constant for acceptor  ---
!
          JCX = JCX+1
          VARB = 'Valocchi-Monod Kinetic Reaction: ' //
     &      'Half-Saturation Constant for Acceptor'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
!
!---        Check that the file exists  ---
!
            INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
            IF( .NOT.FLG_CHK ) THEN
              INDX = 4
              CHMSG = 'Valocchi-Monod Kinetic Reaction '
     &         // 'Half-Saturation Constant for Acceptor' 
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
              INDX = 4
              CHMSG = 'Valocchi-Monod Kinetic Reaction '
     &         // 'Half-Saturation Constant for Acceptor' 
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ENDIF
            NCKN = LFX*LFY*LFZ
          ELSE
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read maximum specific rate of substrate utilization  ---
!
          JCX = JCX+1
          VARB = 'Valocchi-Monod Kinetic Reaction: ' //
     &        'Maximum Specific Rate of Substrate Utilization'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
!
!---        Check that the file exists  ---
!
            INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
            IF( .NOT.FLG_CHK ) THEN
              INDX = 4
              CHMSG = 'Valocchi-Monod Kinetic Reaction:  ' //
     &         'Maximum Specific Rate of Substrate Utilization' 
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
              INDX = 4
              CHMSG = 'Valocchi-Monod Kinetic Reaction:  ' //
     &        'Maximum Specific Rate of Substrate Utilization'
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ENDIF
            NCKN = LFX*LFY*LFZ
          ELSE
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---    Valocchi-Sorption type reactions  ---
!
        ELSEIF( IRCKTX.EQ.3 ) THEN
!
!---      Read mass transfer coefficient  ---
!
          JCX = JCX+1
          VARB = 'Valocchi-Sorption Kinetic Reaction: ' //
     &      'Mass Transfer Coefficient'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
!
!---        Check that the file exists  ---
!
            INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
            IF( .NOT.FLG_CHK ) THEN
              INDX = 4
              CHMSG = 'Valocchi-Sorption Kinetic Reaction:  ' //
     &         'Mass Transfer Coefficient' 
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
              INDX = 4
              CHMSG = 'Valocchi-Sorption Kinetic Reaction:  ' //
     &         'Mass Transfer Coefficient' 
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ENDIF
            NCKN = LFX*LFY*LFZ
          ELSE
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read distribution coefficient  ---
!
          JCX = JCX+1
          VARB = 'Valocchi-Sorption Kinetic Reaction: ' //
     &      'Distribution Coefficient'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
!
!---        Check that the file exists  ---
!
            INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
            IF( .NOT.FLG_CHK ) THEN
              INDX = 4
              CHMSG = 'Valocchi-Sorption Kinetic Reaction:  ' //
     &         'Distribution Coefficient' 
     &          // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
              INDX = 4
              CHMSG = 'Valocchi-Sorption Kinetic Reaction:  ' //
     &         'Distribution Coefficient' 
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ENDIF
            NCKN = LFX*LFY*LFZ
          ELSE
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---    Valocchi-Biomass type reactions  ---
!
        ELSEIF( IRCKTX.EQ.4 ) THEN
!
!---      Read half-saturation constant for donor  ---
!
          JCX = JCX+1
          VARB = 'Valocchi-Monod Kinetic Reaction: ' //
     &      'Half-Saturation Constant for Donor'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
!
!---        Check that the file exists  ---
!
            INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
            IF( .NOT.FLG_CHK ) THEN
              INDX = 4
              CHMSG = 'Valocchi-Monod Kinetic Reaction:  ' //
     &         'Half-Saturation Constant for Donor' 
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
              INDX = 4
              CHMSG = 'Valocchi-Monod Kinetic Reaction:  ' //
     &         'Half-Saturation Constant for Donor' 
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ENDIF
            NCKN = LFX*LFY*LFZ
          ELSE
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read half-saturation constant for acceptor  ---
!
          JCX = JCX+1
          VARB = 'Valocchi-Monod Kinetic Reaction: ' //
     &      'Half-Saturation Constant for Acceptor'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
!
!---        Check that the file exists  ---
!
            INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
            IF( .NOT.FLG_CHK ) THEN
              INDX = 4
              CHMSG = 'Valocchi-Monod Kinetic Reaction:  ' //
     &         'Half-Saturation Constant for Acceptor'
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
              INDX = 4
              CHMSG = 'Valocchi-Monod Kinetic Reaction:  ' //
     &         'Half-Saturation Constant for Acceptor'
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ENDIF
            NCKN = LFX*LFY*LFZ
          ELSE
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read maximum specific rate of substrate utilization  ---
!
          JCX = JCX+1
          VARB = 'Valocchi-Monod Kinetic Reaction: ' //
     &        'Maximum Specific Rate of Substrate Utilization'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
!
!---        Check that the file exists  ---
!
            INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
            IF( .NOT.FLG_CHK ) THEN
              INDX = 4
              CHMSG = 'Valocchi-Monod Kinetic Reaction:  ' //
     &         'Maximum Specific Rate of Substrate Utilization'
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
              INDX = 4
              CHMSG = 'Valocchi-Monod Kinetic Reaction:  ' //
     &         'Maximum Specific Rate of Substrate Utilization'
     &        //  FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ENDIF
            NCKN = LFX*LFY*LFZ
          ELSE
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read microbial yield coefficient  ---
!
          JCX = JCX+1
          VARB = 'Valocchi-Monod Kinetic Reaction: ' //
     &      'Microbial Yield Coefficient'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
!
!---        Check that the file exists  ---
!
            INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
            IF( .NOT.FLG_CHK ) THEN
              INDX = 4
              CHMSG = 'Valocchi-Monod Kinetic Reaction:  ' //
     &         'Microbial Yield Coefficient'
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
              INDX = 4
              CHMSG = 'Valocchi-Monod Kinetic Reaction:  ' //
     &         'Microbial Yield Coefficient'
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ENDIF
            NCKN = LFX*LFY*LFZ
          ELSE
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
!
!---      Read first-order micobial decay coefficient  ---
!
          JCX = JCX+1
          VARB = 'Valocchi-Monod Kinetic Reaction: ' //
     &      'First-Order Microbial Decay Coefficient'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
!
!---        Check that the file exists  ---
!
            INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
            IF( .NOT.FLG_CHK ) THEN
              INDX = 4
              CHMSG = 'Valocchi-Monod Kinetic Reaction:  ' //
     &         'First-Order Microbial Decay Coefficient'
     &        //  FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
              INDX = 4
              CHMSG = 'Valocchi-Monod Kinetic Reaction:  ' //
     &         'First-Order Microbial Decay Coefficient'
     &        // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ENDIF
            NCKN = LFX*LFY*LFZ
          ELSE
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---    Monod type reactions  ---
!
        ELSEIF( IRCKTX.EQ.22 ) THEN
!
!---      Loop over reactants, less one  ---
!
          DO 320 NSP = 1,NSPRX-1
!
!---        Allow for returns in input lines  ---
!
            CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              CALL RD_INPL( CHDUM )
              CALL L_CASE( CHDUM )
              ISTART = 1
            ENDIF
!
!---        Read half-saturation constant  ---
!
            VARB = 'Monod Kinetic Reaction: ' //
     &        'Half-Saturation Constant'
            JCX = JCX+1
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
              NCH  = INDEX(FDUM,':')+1
              NCHF = INDEX(FDUM,'  ')-1
!           
!---          Check that the file exists  ---
!           
              INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
              IF( .NOT.FLG_CHK ) THEN
                INDX = 4
                CHMSG = 'Monod Kinetic Reaction:  ' //
     &           'Half-Saturation Constant'
     &           // FDUM(NCH:NCHF)
                CALL WRMSGP( INDX )
              ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
                INDX = 4
                CHMSG = 'Monod Kinetic Reaction:  ' //
     &           'Half-Saturation Constant'
     &           // FDUM(NCH:NCHF)
                CALL WRMSGP( INDX )
              ENDIF
              NCKN = LFX*LFY*LFZ
            ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
            ENDIF
            CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
  320     CONTINUE
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RD_INPL( CHDUM )
            CALL L_CASE( CHDUM )
            ISTART = 1
          ENDIF
!
!---      Read maximum specific rate of substrate utilization  ---
!
          VARB = 'Monod Kinetic Reaction: ' //
     &      'Maximum Specific Rate of Reactant Utilization'
          JCX = JCX+1
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
!
!---        Check that the file exists  ---
!
            INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
            IF( .NOT.FLG_CHK ) THEN
              INDX = 4
              CHMSG = 'Monod Kinetic Reaction:  ' //
     &         'Maximum Specific Rate of Reactant Utilization'
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
              INDX = 4
              CHMSG = 'Monod Kinetic Reaction:  ' //
     &         'Maximum Specific Rate of Reactant Utilization'
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ENDIF
            NCKN = LFX*LFY*LFZ
          ELSE
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---    Biomass type reactions  ---
!
        ELSEIF( IRCKTX.EQ.24 ) THEN
!
!---      Loop over reactants, less one  ---
!
          DO 330 NSP = 1,NSPRX-1
!
!---        Allow for returns in input lines  ---
!
            CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              CALL RD_INPL( CHDUM )
              CALL L_CASE( CHDUM )
              ISTART = 1
            ENDIF
!
!---        Read half-saturation constant  ---
!
            VARB = 'Biomass Kinetic Reaction: ' //
     &        'Half-Saturation Constant'
            JCX = JCX+1
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
              NCH  = INDEX(FDUM,':')+1
              NCHF = INDEX(FDUM,'  ')-1
!           
!---          Check that the file exists  ---
!           
              INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
              IF( .NOT.FLG_CHK ) THEN
                INDX = 4
                CHMSG = 'Biomass Kinetic Reaction:  ' //
     &           'Half-Saturation Constant' 
     &           // FDUM(NCH:NCHF)
                CALL WRMSGP( INDX )
              ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
                INDX = 4
                CHMSG = 'Biomass Kinetic Reaction:  ' //
     &           'Half-Saturation Constant' 
     &           // FDUM(NCH:NCHF)
                CALL WRMSGP( INDX )
              ENDIF
              NCKN = LFX*LFY*LFZ
            ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
            ENDIF
            CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---        Allow for returns in input lines  ---
!
            CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              CALL RD_INPL( CHDUM )
              CALL L_CASE( CHDUM )
              ISTART = 1
            ENDIF
!
!---        Read maximum specific rate of substrate utilization  ---
!
            VARB = 'Biomass Kinetic Reaction: ' //
     &        'Maximum Specific Rate of Reactant Utilization'
            JCX = JCX+1
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
              NCH  = INDEX(FDUM,':')+1
              NCHF = INDEX(FDUM,'  ')-1
!           
!---          Check that the file exists  ---
!           
              INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
              IF( .NOT.FLG_CHK ) THEN
                INDX = 4
                CHMSG = 'Biomass Kinetic Reaction:  ' //
     &           'Maximum Specific Rate of Reactant Utilization'
     &           // FDUM(NCH:NCHF)
                CALL WRMSGP( INDX )
              ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
                INDX = 4
                CHMSG = 'Biomass Kinetic Reaction:  ' //
     &           'Maximum Specific Rate of Reactant Utilization'
     &           // FDUM(NCH:NCHF)
                CALL WRMSGP( INDX )
              ENDIF
              NCKN = LFX*LFY*LFZ
            ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
            ENDIF
            CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
  330     CONTINUE
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RD_INPL( CHDUM )
            CALL L_CASE( CHDUM )
            ISTART = 1
          ENDIF
!
!---      Read microbial yield coefficient  ---
!
          VARB = 'Biomass Kinetic Reaction: ' //
     &      'Microbial Yield Coefficient'
          JCX = JCX+1
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
!
!---        Check that the file exists  ---
!
            INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
            IF( .NOT.FLG_CHK ) THEN
              INDX = 4
              CHMSG = 'Biomass Kinetic Reaction:  ' //
     &         'Microbial Yield Coefficient' 
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
              INDX = 4
              CHMSG = 'Biomass Kinetic Reaction:  ' //
     &         'Microbial Yield Coefficient' 
     &         // FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ENDIF
            NCKN = LFX*LFY*LFZ
          ELSE
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            CALL RD_INPL( CHDUM )
            CALL L_CASE( CHDUM )
            ISTART = 1
          ENDIF
!
!---    Read first-order micobial decay coefficient  ---
!
        VARB = 'Biomass Kinetic Reaction: ' //
     &    'Microbial Decay Coefficient'
        JCX = JCX+1
        IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
          CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
          NCH  = INDEX(FDUM,':')+1
          NCHF = INDEX(FDUM,'  ')-1
!
!---      Check that the file exists  ---
!
          INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
          IF( .NOT.FLG_CHK ) THEN
            INDX = 4
            CHMSG = 'Biomass Kinetic Reaction:  ' //
     &       'Microbial Decay Coefficient' 
     &       // FDUM(NCH:NCHF)
            CALL WRMSGP( INDX )
          ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
            INDX = 4
            CHMSG = 'Biomass Kinetic Reaction:  ' //
     &       'Microbial Decay Coefficient' 
     &      // FDUM(NCH:NCHF)
            CALL WRMSGP( INDX )
          ENDIF
          NCKN = LFX*LFY*LFZ
        ELSE
        CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
        ENDIF
        CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---    Liu's lognormal multi rate type reactions  ---
!
        ELSEIF( IRCKTX.EQ.41 ) THEN
!
!---      Read rate from lognormal distribution
!
          VARB = 'Liu Multi-Rate Kin. Reac.: ' //
     &      'Rate Constant'
          JCX = JCX + 1
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
            CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read kinetic site density  ---
!
          VARB = 'Liu Multi-Rate Kin. Reac.: ' //
     &      'Kinetic Site Density'
          JCX = JCX + 1
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
            CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read pore ratio  ---
!
          VARB = 'Liu Multi-Rate Kin. Reac.: ' //
     &      'Pore Ratio.'
          JCX = JCX + 1
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
!
!---      Read logK1 for the first sorped species on the site  ---
!
          VARB = 'Liu Multi-Rate Kin. Reac.: ' //
     &      'Log K1.'
          JCX = JCX + 1
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
!
!---      Read logK2 for the second sorped species on the site ---
!
          VARB = 'Liu Multi-Rate Kin. Reac.: ' //
     &      'Log K2.'
          JCX = JCX + 1
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
!
!---    Liu's dual domain rate type reactions  ---
!
        ELSEIF( IRCKTX.EQ.42 ) THEN
!
!---      Mass transfer rate
!
          VARB = 'Liu Dual-Domain Kin. Reac.: ' //
     &      'Rate Constant'
          JCX = JCX + 1
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
!
!---      Read pore ratio (immobile pore/mobile pore) ---
!
          VARB = 'Liu Dual-Domain Kin. Reac.: ' //
     &      'Pore Ratio.'
          JCX = JCX + 1
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
      ENDIF
!
!---    Multi-rate mineral  ---
!
  400   CONTINUE
        IF( IRCKTX.EQ.20 ) THEN
          VARB = 'Number of Mechanisms in Kinetic Reaction'
          JCX = 3
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCH  = INDEX(FDUM,':')+1
            NCHF = INDEX(FDUM,'  ')-1
!
!---        Check that the file exists  ---
!
            INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
            IF( .NOT.FLG_CHK ) THEN
              INDX = 4
              CHMSG = 'Number of Mechanisms in Kinetic Reaction:  ' //
     &         FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
              INDX = 4
              CHMSG = 'Number of Mechanisms in Kinetic Reaction:  ' //
     &         FDUM(NCH:NCHF)
              CALL WRMSGP( INDX )
            ENDIF
            NCKN = LFX*LFY*LFZ
          ELSE
          CALL RD_INT( ISTART,ICOMMA,CHDUM,NKRMX )
          ENDIF
          CALL RD_INPL( CHDUM )
!
!---      Loop over mechanisms  ---
!
          DO 490 NKRM = 1,NKRMX
            CALL RD_INPL( CHDUM )
!
!---        Mechanism reference reaction rate  ---
!
            VARB = 'Mechanism Kinetic Reaction Reference Rate'
            JCX = JCX+1
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
              NCH  = INDEX(FDUM,':')+1
              NCHF = INDEX(FDUM,'  ')-1
!           
!---          Check that the file exists  ---
!           
              INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
              IF( .NOT.FLG_CHK ) THEN
                INDX = 4
                CHMSG = 'Mechanism Kinetic Reaction Reference Rate: '
     &            // FDUM(NCH:NCHF)
                CALL WRMSGP( INDX )
              ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
                INDX = 4
                CHMSG = 'Mechanism Kinetic Reaction Reference Rate: '
     &           // FDUM(NCH:NCHF)
                CALL WRMSGP( INDX )
              ENDIF
              NCKN = LFX*LFY*LFZ
            ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
            ENDIF
            CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---        Mechanism activation energy  ---
!
            VARB = 'Mechanism Kinetic Reaction Activation Energy'
            JCX = JCX+1
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
              NCH  = INDEX(FDUM,':')+1
              NCHF = INDEX(FDUM,'  ')-1
!           
!---          Check that the file exists  ---
!           
              INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
              IF( .NOT.FLG_CHK ) THEN
                INDX = 4
                CHMSG = 'Mechanism Kinetic Reaction Activation Energy:  
     &           ' // FDUM(NCH:NCHF)
                CALL WRMSGP( INDX )
              ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
                INDX = 4
                CHMSG = 'Mechanism Kinetic Reaction Activation Energy:  
     &           ' // FDUM(NCH:NCHF)
                CALL WRMSGP( INDX )
              ENDIF
              NCKN = LFX*LFY*LFZ
            ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
            ENDIF
            CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---        Mechanism reference reaction temperature  ---
!
            VARB = 'Mechanism Kinetic Reaction Reference Temperature'
            JCX = JCX+1
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
              NCH  = INDEX(FDUM,':')+1
              NCHF = INDEX(FDUM,'  ')-1
!           
!---          Check that the file exists  ---
!           
              INQUIRE( FILE=FDUM(NCH:NCHF), FORM=GDUM, EXIST=FLG_CHK )
              IF( .NOT.FLG_CHK ) THEN
                INDX = 4
                CHMSG = 'Mechanism Kinetic Reaction Reference
     &            Temperature:  ' // FDUM(NCH:NCHF)
                CALL WRMSGP( INDX )
              ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
                INDX = 4
                CHMSG = 'Mechanism Kinetic Reaction Reference
     &            Temperature:  ' // FDUM(NCH:NCHF)
                CALL WRMSGP( INDX )
              ENDIF
              NCKN = LFX*LFY*LFZ
            ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
            ENDIF
            CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---        Number of species in mechanism  ---
!
            CALL RD_INPL( CHDUM )
            VARB = 'Number of Species in Mechanism'
            CALL RD_INT( ISTART,ICOMMA,CHDUM,NKRSX )
            JCX = JCX+NKRSX
  490     CONTINUE
        ENDIF
        NCKN = LFX*LFY*LFZ  !xl
        LSPK = MAX( LSPK,JCX )
        LCKN = MAX( NCKN,LCKN )
  500 CONTINUE
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_KNRC group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_LRP
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
!     Read Aqueous Relative Permeability Function Parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 26 November 2002.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE TRNSPT
      USE TABL
      USE SOURC
      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*128 ADUM,RDUM
      CHARACTER*512 CHDUM
      TYPE(LIST_NODE), POINTER :: LOC_PTR
      TYPE(LIST_SCALING), POINTER :: SC_LOC_PTR
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_LRP'
!
!---  Assign card string  ---
!
      CARD = 'Aqueous Relative Permeability Card'
!
!---  Loop over the rock/soil aqueous relative permeability
!     information lines  ---
!
      N = 0
      IJK = 0
   10 CONTINUE
      IF( N.GE.NROCK .OR. IJK.GT.0 ) GOTO 500
      ISTART = 1
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      VARB = 'Rock/Soil Name'
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,RDUM)
!
!---  Check for a pair of delimiting slashes in the rock/soil name,
!     indicating a pattern of rock/soil types  ---
!
      KBS = 0
      IBS = INDEX( RDUM(1:),'/' )
      IF( IBS.GT.0 ) THEN
        IBS = IBS + 1
        JBS = INDEX( RDUM(IBS:),'/')
        IF( JBS.GT.0 ) THEN
          JBS = IBS + JBS - 2
          KBS = 1
          ISBS = ISTART
        ENDIF
      ENDIF
      IROCK = 1
   20 CONTINUE
!
!---  IJK, KIJ, or JKI indexing  ---
!
      IF( INDEX(RDUM(1:),'indexing').NE.0 ) THEN
        IF( INDEX(RDUM,'ijk').NE.0 ) THEN
          IJK = 1
        ELSEIF( INDEX(RDUM,'jki').NE.0 ) THEN
          IJK = 2
        ELSEIF( INDEX(RDUM,'kij').NE.0 ) THEN
          IJK = 3
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Indexing Option' // RDUM(1:NCH)
          CALL WRMSGP( INDX )
        ENDIF
        IROCK = 1
        GOTO 220
      ENDIF
!
!---  Search known rock types for a matching type  ---
!
          LOC_PTR => ROCK_PTR
          IROCK = 0
          DO
            IF( .NOT.ASSOCIATED(LOC_PTR) ) EXIT
            IROCK = IROCK + 1
            IF( LOC_PTR%LIST_NAME == RDUM ) THEN
              GOTO 200
            ELSE
              LOC_PTR => LOC_PTR%NEXT
            ENDIF
          ENDDO
!
!---  Search known scaling groups for a matching type  ---
!
        IF( ISLC(19).EQ.1 ) THEN
          SC_LOC_PTR => SCALING_PTR
          ISGRP = 0
          DO
            IF( .NOT.ASSOCIATED(SC_LOC_PTR) ) EXIT
            IF( SC_LOC_PTR%SCALING_NAME == RDUM ) THEN
              ISGRP = SC_LOC_PTR%SCALING_NUM
              IROCK = 1
              GOTO 200
            ELSE
              SC_LOC_PTR => SC_LOC_PTR%NEXT
            ENDIF
          ENDDO
          INDX = 2
          CHMSG = 'Unrecognized Rock/Soil Type or Scaling Group: '
     &      // RDUM(1:NCH)
          CALL WRMSGP( INDX )
          GOTO 10
        ENDIF
        INDX = 2
        CHMSG = 'Unrecognized Rock/Soil Type: ' // RDUM(1:NCH)
        CALL WRMSGP( INDX )
        GOTO 10
  200   CONTINUE
!
!---  Loop over rock/soils within scaling group  ---
!
        IF( ISLC(19).EQ.1 .AND. ISGRP.NE.0 ) THEN
          DO 202 M = IROCK,NROCK
            IF( ISCALE(M).EQ.ISGRP ) THEN
              IROCK = M
              GOTO 204
            ENDIF
  202     CONTINUE
        ENDIF
  204   CONTINUE
!
!---  Read aqueous relative permeability pressure function  ---
!
  220 CONTINUE
      N = N + 1
!
!---  Read saturation/capillary pressure function for
!     tabular forms  ---
!
      VARB = 'Aqueous Relative Permeability Function'
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---  Tabular (relative permeability versus liquid saturation)  ---
!
      IF( INDEX(ADUM(1:),'tabular').NE.0 ) THEN
        IF( INDEX( ADUM(1:),'content' ).NE.0 ) THEN
          IRPLX = 10
        ELSEIF( INDEX( ADUM(1:),'head' ).NE.0 ) THEN
          IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
            IRPLX = 14
          ELSE
            IRPLX = 12
          ENDIF
        ELSE
          IRPLX = 10
        ENDIF
!
!---    IJK Indexing  ---
!
        IF( IJK.GT.0 ) THEN
          VARB = 'Number of Tables'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,NTABX)
          NTBL = 0
          DO 240 NTX = 1,NTABX
            ISTART = 1
            CALL RD_INPL( CHDUM )
            CALL L_CASE( CHDUM )
            VARB = 'Number of Table Entries'
            CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
            IF( NLIN.LT.2 ) THEN
              INDX = 4
              CHMSG = 'Invalid Aqueous Relative Permeability Table'
              CALL WRMSGP( INDX )
            ENDIF
            NTBL = NTBL + NLIN
            DO 230 NLX = 1,NLIN
              CALL RD_INPL( CHDUM )
  230       CONTINUE
  240     CONTINUE
          LTBL = LTBL + NTBL
        ELSE
          VARB = 'Number of Tabular Entries'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
          IF( NLIN.LT.2 ) THEN
            INDX = 4
            CHMSG = 'Invalid Aqueous Relative Permeability Table'
            CALL WRMSGP( INDX )
          ENDIF
          DO 250 NL = 1,NLIN
            CALL RD_INPL( CHDUM )
 250      CONTINUE
          NTBL = NLIN
          LTBL = LTBL + NTBL
        ENDIF
!
!---  Polynomial function  ---
!
      ELSEIF( INDEX(ADUM(1:),'polynomial').NE.0 ) THEN
        IRPLX = 19
        VARB = 'Number of Polynomial Function Pieces'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,NPOLY)
        LPOLYN = MAX( LPOLYN,NPOLY )
        DO 290 NP = 1,NPOLY
          ISTART = 1
          CALL RD_INPL( CHDUM )
          CALL L_CASE( CHDUM )
          VARB = 'Number of Polynomial Coefficients'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,NCOEF)
          LPOLYC = MAX( LPOLYC,NCOEF+4 )
 290    CONTINUE
      ENDIF
!
!---  Relative permeability model coefficients  ---
!
      LRPLC = 12
!
!---  Continue reading rock/soil type names for a pattern match  ---
!
      IF( KBS.EQ.1 .AND. IROCK.LT.NROCK ) THEN
        IROCK = IROCK + 1
        ISTART = ISBS
        GOTO 20
      ENDIF
!
!---  Loop over remaining rock/soils within scaling group  ---
!
      IF( ISLC(19).EQ.1 .AND. IROCK.LT.NROCK ) THEN
        DO 490 M = IROCK+1,NROCK
          IF( ISCALE(M).EQ.ISGRP ) THEN
            N = N+1
          ENDIF
  490   CONTINUE
      ENDIF
!
!---  Read next rock/soil type or scaling group  ---
!
      GOTO 10
 500  CONTINUE
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_LRP group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_OBDA
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
!     Read Observed Data Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 27 November 2002.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE TRNSPT
      USE TABL
      USE SOURC
      USE SOLTN
      USE PORMED
      USE OUTPU
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*128 ADUM,CDUM,FDUM
      CHARACTER*512 CHDUM
      REAL*8 R_OBDSX(2)
      LOGICAL FLG_CHK
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_OBDA'
!
!---  Assign card string  ---
!
      CARD = 'Observed-Data Card'
!
!---  Read number of observed-data types  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Observed-Data Types'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NOBDTX)
      LOBDT = MAX( LOBDT,NOBDTX )
      NSF = LSF
!
!---  Loop over the number of observed-data types  ---
!
      DO 300 NT = 1,NOBDTX
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
        VARB = 'Observed-Data Type'
        CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---  Surface-rate-observation variable  ---
!
        IF( INDEX(ADUM(1:),'surface').NE.0 .AND.
     &    ( INDEX(ADUM(1:),'rate').NE.0 .OR.
     &    INDEX(ADUM(1:),'flux').NE.0 ) ) THEN
          NSF = NSF + 1
          LSF = MAX( LSF,NSF )
!
!---  Surface-integral-observation variable  ---
!
        ELSEIF( INDEX(ADUM(1:),'surface').NE.0 .AND.
     &    INDEX(ADUM(1:),'integral').NE.0 ) THEN
          NSF = NSF + 1
          LSF = MAX( LSF,NSF )
        ENDIF
!
!---    Read number of observed data samples
!       or an external file name  ---
!
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
!
!---    Read observed-data samples from an external file  ---
!
        IF( INDEX( CHDUM(1:),'file').NE.0 ) THEN
          VARB = 'Observed-Data External File Name'
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
!
!---      Check that external file exists  ---
!
          INQUIRE( FILE=FDUM(1:NCHF), FORM=CDUM, EXIST=FLG_CHK )
          IF( .NOT.FLG_CHK ) THEN
            INDX = 4
            CHMSG = 'Missing Observed-Data External File: '
     &        // FDUM(1:NCHF)
            CALL WRMSGP( INDX )
          ELSEIF( CDUM.EQ.'UNFORMATTED' ) THEN
            INDX = 4
            CHMSG = 'Unformatted Observed-Data External File: '
     &        // FDUM(1:NCHF)
            CALL WRMSGP( INDX )
          ENDIF
          OPEN(UNIT=27,FILE=FDUM(1:NCHF),STATUS='OLD',FORM='FORMATTED')
          NS = 0
  100     READ(27,'(A)',END=110) CHDUM
          IF( CHDUM(1:1).EQ.'#' .OR. CHDUM(1:1).EQ.'!' ) GOTO 100
          BACKSPACE(27)
          NS = NS + 1
          LOBDS = MAX( LOBDS,NS )
          READ(27,*,END=110) R_OBDSX(2),R_OBDSX(1)
          GOTO 100
  110     CONTINUE
          CLOSE(UNIT=27)
          GOTO 300
        ENDIF
!
!---    Read observed-data samples from input file  ---
!
        VARB = 'Number of Observed Data Samples'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,NOBDSX)
        LOBDS = MAX( LOBDS,NOBDSX )
!
!---    Loop over number of observed data samples  ---
!
        DO 200 NS = 1,NOBDSX
          ISTART = 1
          CALL RD_INPL( CHDUM )
          CALL L_CASE( CHDUM )
  200   CONTINUE
  300 CONTINUE
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_OBDA group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_NCP
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
!     Read Oil Component Properties Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 12 January 2004.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_NCP'
!
!---  Assign card string  ---
!
      CARD = 'NAPL Components Properties Card'
!
!---  Read number of oil components  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of NAPL Components'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NCN)
      LCN = MAX( LCN,NCN )
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_NCP group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_NRP
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
!     Read NAPL Relative Permeability Function Parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 18 December 2013.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE TRNSPT
      USE TABL
      USE SOURC
      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*128 ADUM,RDUM
      CHARACTER*512 CHDUM
      TYPE(LIST_NODE), POINTER :: LOC_PTR
      TYPE(LIST_SCALING), POINTER :: SC_LOC_PTR
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_NRP'
!
!---  Assign card string  ---
!
      CARD = 'NAPL Relative Permeability Card'
!
!---  Loop over the rock/soil NAPL relative permeability
!     information lines  ---
!
      N = 0
      IJK = 0
   10 CONTINUE
      IF( N.GE.NROCK .OR. IJK.GT.0 ) GOTO 500
      ISTART = 1
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      VARB = 'Rock/Soil Name'
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,RDUM)
!
!---  Check for a pair of delimiting slashes in the rock/soil name,
!     indicating a pattern of rock/soil types  ---
!
      KBS = 0
      IBS = INDEX( RDUM(1:),'/' )
      IF( IBS.GT.0 ) THEN
        IBS = IBS + 1
        JBS = INDEX( RDUM(IBS:),'/')
        IF( JBS.GT.0 ) THEN
          JBS = IBS + JBS - 2
          KBS = 1
          ISBS = ISTART
        ENDIF
      ENDIF
      IROCK = 1
   20 CONTINUE
!
!---  IJK, KIJ, or JKI indexing  ---
!
      IF( INDEX(RDUM(1:),'indexing').NE.0 ) THEN
        IF( INDEX(RDUM,'ijk').NE.0 ) THEN
          IJK = 1
        ELSEIF( INDEX(RDUM,'jki').NE.0 ) THEN
          IJK = 2
        ELSEIF( INDEX(RDUM,'kij').NE.0 ) THEN
          IJK = 3
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Indexing Option' // RDUM(1:NCH)
          CALL WRMSGP( INDX )
        ENDIF
        IROCK = 1
        GOTO 220
      ENDIF
!
!---  Search known rock types for a matching type  ---
!
          LOC_PTR => ROCK_PTR
          IROCK = 0
          DO
            IF( .NOT.ASSOCIATED(LOC_PTR) ) EXIT
            IROCK = IROCK + 1
            IF( LOC_PTR%LIST_NAME == RDUM ) THEN
              GOTO 200
            ELSE
              LOC_PTR => LOC_PTR%NEXT
            ENDIF
          ENDDO
!
!---  Search known scaling groups for a matching type  ---
!
        IF( ISLC(19).EQ.1 ) THEN
          SC_LOC_PTR => SCALING_PTR
          ISGRP = 0
          DO
            IF( .NOT.ASSOCIATED(SC_LOC_PTR) ) EXIT
            IF( SC_LOC_PTR%SCALING_NAME == RDUM ) THEN
              ISGRP = SC_LOC_PTR%SCALING_NUM
              IROCK = 1
              GOTO 200
            ELSE
              SC_LOC_PTR => SC_LOC_PTR%NEXT
            ENDIF
          ENDDO
          INDX = 2
          CHMSG = 'Unrecognized Rock/Soil Type or Scaling Group: '
     &      // RDUM(1:NCH)
          CALL WRMSGP( INDX )
          GOTO 10
        ENDIF
        INDX = 2
        CHMSG = 'Unrecognized Rock/Soil Type: ' // RDUM(1:NCH)
        CALL WRMSGP( INDX )
        GOTO 10
  200   CONTINUE
!
!---  Loop over rock/soils within scaling group  ---
!
        IF( ISLC(19).EQ.1 .AND. ISGRP.NE.0 ) THEN
          DO 202 M = IROCK,NROCK
            IF( ISCALE(M).EQ.ISGRP ) THEN
              IROCK = M
              GOTO 204
            ENDIF
  202     CONTINUE
        ENDIF
  204   CONTINUE
!
!---  Read gas relative permeability pressure function  ---
!
  220 CONTINUE
      N = N + 1
!
!---  Read saturation/capillary pressure function for
!     tabular forms  ---
!
      VARB = 'NAPL Relative Permeability Function'
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---    Tabular (relative permeability versus liquid saturation)  ---
!
      IF( INDEX(ADUM(1:),'tabular') .NE. 0 ) THEN
        IF( INDEX( ADUM(1:),'spline' ).NE.0 ) THEN
          IRPGX = 11
        ELSE
          IRPGX = 10
        ENDIF
!
!---    IJK Indexing  ---
!
        IF( IJK.GT.0 ) THEN
          VARB = 'Number of Tables'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,NTABX)
          NTBL = 0
          DO 240 NTX = 1,NTABX
            ISTART = 1
            CALL RD_INPL( CHDUM )
            CALL L_CASE( CHDUM )
            VARB = 'Number of Table Entries'
            CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
            IF( NLIN.LT.2 ) THEN
              INDX = 4
              CHMSG = 'Invalid NAPL Relative Permeability Table'
              CALL WRMSGP( INDX )
            ENDIF
            NTBL = NTBL + NLIN
            DO 230 NLX = 1,NLIN
              CALL RD_INPL( CHDUM )
  230       CONTINUE
  240     CONTINUE
          LTBL = LTBL + NTBL
        ELSE
          VARB = 'Number of Tabular Entries'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
          IF( NLIN.LT.2 ) THEN
            INDX = 4
            CHMSG = 'Invalid NAPL Relative Permeability Table'
            CALL WRMSGP( INDX )
          ENDIF
          DO 250 NL = 1,NLIN
            CALL RD_INPL( CHDUM )
 250      CONTINUE
          IF( IJK.GT.0 ) THEN
            NTBL = LFD*NLIN
          ELSE
            NTBL = NLIN
          ENDIF
          LTBL = LTBL + NTBL
        ENDIF
      ENDIF
!
!---  Continue reading rock/soil type names for a pattern match  ---
!
      IF( KBS.EQ.1 .AND. IROCK.LT.NROCK ) THEN
        IROCK = IROCK + 1
        ISTART = ISBS
        GOTO 20
      ENDIF
!
!---  Loop over remaining rock/soils within scaling group  ---
!
      IF( ISLC(19).EQ.1 .AND. IROCK.LT.NROCK ) THEN
        DO 490 M = IROCK+1,NROCK
          IF( ISCALE(M).EQ.ISGRP ) THEN
            N = N+1
          ENDIF
  490   CONTINUE
      ENDIF
!
!---  Read next rock/soil type or scaling group  ---
!
      GOTO 10
 500  CONTINUE
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_NRP group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_OU
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
!     Read Output Control Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 26 November 2002.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE TRNSPT
      USE TABL
      USE SOURC
      USE SOLTN
      USE PORMED
      USE OUTPU
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_OU'
!
!---  Assign card string  ---
!
      CARD = 'Output Control Card'
!
!---  Read reference node information  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Reference Nodes: '
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NREF)
      LREF = MAX( LREF,NREF )
!
!---  Skip over reference node domain input  ---
!
      DO 100 N = 1,NREF
        CALL RD_INPL( CHDUM )
  100 CONTINUE
!
!---  Skip over output frequency and significant digits  ---
!
      CALL RD_INPL( CHDUM )
!
!---  Number of reference node variables  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Reference Node Variables: '
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NVREF)
      DO 200 NV = 1,NVREF
        CALL RD_INPL( CHDUM )
  200 CONTINUE
      LVREF = MAX( LVREF,NVREF )
!
!---  Plot file output times  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Plot File Output Times: '
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NPRTM)
      IC = 0
      DO 300 N = 1,NPRTM
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
        ICMX = INDEX( CHDUM(ISTART:), ',' )
        IATX = INDEX( CHDUM(ISTART:), '@' )
!
!---    Sequence of plot file output times  ---
!
        IF( IATX.GT.1 .AND. IATX.LT.ICMX ) THEN
          CHDUM(IATX:IATX) = ','
          VARB = 'Count Integer'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,IATX )
          IC = IC + IATX
!
!---    Single plot file output time  ---
!
        ELSE
          IC = IC + 1
        ENDIF
 300  CONTINUE
      LPTM = MAX( LPTM,IC )
!
!---  Number of plot file variables  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Plot File Variables: '
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NVPLOT)
      LVPLOT = MAX( LVPLOT,NVPLOT )
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_OU group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_PCP
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
!     Read Petroleum Components Properties Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 25 March 2013.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
      CHARACTER*64 ADUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_PCP'
!
!---  Assign card string  ---
!
      CARD = 'Petroleum Components Properties Card'
!!
!!---  Read number of petroleum components  ---
!!
!      CALL RD_INPL( CHDUM )
!      CALL L_CASE( CHDUM )
!      ISTART = 1
!      VARB = 'Number of Petroleum Components'
!      CALL RD_INT(ISTART,ICOMMA,CHDUM,NGC)
!      LNGC = MAX( LNGC,NGC ) + 2
!      LUK = LUK + LNGC
!
!---  Loop over number of petroleum components  ---
!
      DO 500 IGC = 3,NGC+2
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
        VARB = 'Petroleum Component Name'
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        CALL CHK_INT(ISTART,ICOMMA,CHDUM,INDX)
!
!---    Petroleum component composition declared as a combination
!       of petroluem component fractions  ---
!
        IF( INDX.EQ.1 ) THEN
          CALL RD_INT(ISTART,ICOMMA,CHDUM,NPCF)
          LPCF = MAX( LPCF,NPCF )
!
!---      Loop over the number of petroleum component fractions ---
!
          DO 200 N = 1,NPCF
            CALL RD_INPL( CHDUM )
  200     CONTINUE
!
!---    Petroleum component critical properties entered directly  ---
!
        ELSE
          CALL RD_INPL( CHDUM )
        ENDIF
  500 CONTINUE
          
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_PCP group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_PLANT
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
!     Read Plant Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 29 October 2003.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE PLT_ATM
      USE GRID
      USE FILES
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
      CHARACTER*64 UNTS
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_PLANT'
!
!---  Assign card string  ---
!
      CARD = 'Plant Properties Card'
!
!---  Read number of plant varietals  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Plants'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NPLANT)
      LPLANT = MAX( LPLANT,NPLANT )
!
!---  Allocate memory for the plant parameters  ---
!
      ALLOCATE( PARMS_P(1:29,1:LPLANT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PARMS_P'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Allocate memory for the plant names  ---
!
      ALLOCATE( PLANT(1:LPLANT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PLANT'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Loop over the plants information lines  ---
!
      DO 500 IP = 1,NPLANT
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
        VARB = 'Plant Name: '
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,PLANT(IP))
        DO 100 M = 1,IP-1
          IF( PLANT(M).EQ.PLANT(IP) ) THEN
            INDX = 4
            CHMSG = 'Duplicate Plant Name: ' // PLANT(IP)
            CALL WRMSGP( INDX )
          ENDIF
  100   CONTINUE
!
!---    Read root (Z) depth characteristics  ---
!
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
        VARB = 'Max. Root Depth'
        UNTS = 'm'
        IUNM = 1
        IDFLT = 1
        PARMS_P(1,IP) = 1.D-3
        CALL RD_DPR(ISTART,ICOMMA,CHDUM,PARMS_P(1,IP))
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        CALL RD_UNIT(UNTS,PARMS_P(1,IP),INDX)
!
!---    Minimum maximum plant root depth of 1 mm  ---
!
        PARMS_P(1,IP) = MAX( 1.D-3,PARMS_P(1,IP) )
  500 CONTINUE
!
!---  Deallocate memory for the plant names  ---
!
      DEALLOCATE( PLANT,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: PLANT'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_PLANT group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_ROCK
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
!     Read Rock/Soil Zonation Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 21 November 2002.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*128 ADUM,BDUM,CDUM,FDUM
      CHARACTER*512 CHDUM
      LOGICAL FCHK
      TYPE(LIST_NODE), POINTER :: LOC_PTR,TMP_PTR
      TYPE(LIST_SCALING), POINTER :: SC_LOC_PTR,SC_TMP_PTR
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_ROCK'
!
!---  Assign card string  ---
!
      CARD = 'Rock/Soil Zonation Card'
!
!---  Allocate memory for the rock/soil zonation index array  ---
!
      ALLOCATE( IZ(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IZ'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Rock/soil zonation input option  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Input Option [File, Zonation_File, Integer, Indexing]'
      CALL RD_CHR(ISTART,ICOMMA,NCHA,CHDUM,ADUM)
      IF( INDEX(ADUM,'zonation').NE.0 .AND.
     &  INDEX(ADUM,'file').NE.0 ) THEN
        VARB = 'Rock/soil zonation external file name'
        CALL RD_CHR(ISTART,ICOMMA,NCHB,CHDUM,BDUM)
        IF( INDEX(ADUM,'file').NE.0 ) THEN
          FDUM = BDUM
          NCHF = NCHB
        ENDIF
        IF( INDEX(BDUM,'file').NE.0 ) THEN
          FDUM = ADUM
          NCHF = NCHA
        ENDIF
        IF( INDEX(CHDUM,'formatted').NE.0 ) THEN
          INQUIRE( FILE=FDUM(1:NCHF), FORM=CDUM, EXIST=FCHK )
          IF( .NOT.FCHK ) THEN
            INDX = 4
            CHMSG = 'Rock/soil zonational file does not exist: '
     &        // BDUM(1:NCHB)
            CALL WRMSGP( INDX )
          ELSEIF( CDUM.EQ.'UNFORMATTED' ) THEN
            INDX = 4
            CHMSG = 'Rock/soil zonational file is unformatted: '
     &        // BDUM(1:NCHB)
            CALL WRMSGP( INDX )
          END IF
          OPEN( UNIT=27,FILE=FDUM(1:NCHF),STATUS='OLD',
     &      FORM='FORMATTED' )
          NROCK = 1
          READ(27,*)(IZ(N),N=1,LFD)
          CLOSE( UNIT=27 )
          DO 10 N = 1,LFD
            NROCK = MAX( NROCK,IZ(N) )
   10     CONTINUE
          LRC = MAX( LRC,NROCK )
        ELSE
          INQUIRE( FILE=FDUM(1:NCHF), FORM=CDUM, EXIST=FCHK )
          IF( .NOT.FCHK ) THEN
            INDX = 4
            CHMSG = 'Rock/soil zonational file does not exist: '
     &        // BDUM(1:NCHB)
            CALL WRMSGP( INDX )
          ELSEIF( CDUM.EQ.'FORMATTED' ) THEN
            INDX = 4
            CHMSG = 'Rock/soil zonational file is formatted: '
     &        // BDUM(1:NCHB)
            CALL WRMSGP( INDX )
          END IF
          OPEN( UNIT=27,FILE=FDUM(1:NCHF),STATUS='OLD',
     &      FORM='UNFORMATTED' )
          NROCK = 1
          READ(27,*)(IZ(N),N=1,LFD)
          CLOSE( UNIT=27 )
          DO 20 N = 1,LFD
            NROCK = MAX( NROCK,IZ(N) )
   20     CONTINUE
          LRC = MAX( LRC,NROCK )
        ENDIF
        NULLIFY( ROCK_PTR )
        DO 50 NL = 1,NROCK
          CALL RD_INPL( CHDUM )
          CALL L_CASE( CHDUM )
          ISTART = 1
          VARB = 'Rock/Soil Name: '
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---      Empty rock list  ---
!
          IF( .NOT.ASSOCIATED(ROCK_PTR) ) THEN
            ALLOCATE( ROCK_PTR,STAT=ISTAT )
            IF( ISTAT.NE.0 ) THEN
              INDX = 3
              CHMSG = 'Allocation Error: ROCK_PTR'
              CALL WRMSGP( INDX )
            ENDIF
            ROCK_PTR%LIST_NAME = ADUM
            NULLIFY(ROCK_PTR%NEXT)
          ELSE
            LOC_PTR => ROCK_PTR
!
!---        Check for duplicate rock/soil name  ---
!
            DO
              IF( .NOT.ASSOCIATED(LOC_PTR) ) EXIT
              IF( LOC_PTR%LIST_NAME == ADUM ) THEN
                INDX = 4
                CHMSG = 'Duplicate Rock/Soil Name: ' // ADUM
                CALL WRMSGP( INDX )
              ELSE
                LOC_PTR => LOC_PTR%NEXT
              ENDIF
            ENDDO
!
!---        Add rock/soil name to rock list  ---
!
            ALLOCATE( TMP_PTR,STAT=ISTAT )
            IF( ISTAT.NE.0 ) THEN
              INDX = 3
              CHMSG = 'Allocation Error: ROCK_PTR'
              CALL WRMSGP( INDX )
            ENDIF
            TMP_PTR%LIST_NAME = ADUM
            TMP_PTR%NEXT => ROCK_PTR
            ROCK_PTR => TMP_PTR
          ENDIF
   50   CONTINUE
!
!---  Read rock/soil zonation information from an external file  ---
!
      ELSEIF( INDEX(ADUM,'file').NE.0 ) THEN
        VARB = 'External File Name'
        CALL RD_CHR(ISTART,ICOMMA,NCHB,CHDUM,BDUM)
        INQUIRE( FILE=BDUM(1:NCHB), FORM=CDUM, EXIST=FCHK )
        IF( .NOT.FCHK ) THEN
          INDX = 4
          CHMSG = 'Rock/soil zonational file does not exist: '
     &      // BDUM(1:NCHB)
          CALL WRMSGP( INDX )
        ELSEIF( CDUM.EQ.'UNFORMATTED' ) THEN
          INDX = 4
          CHMSG = 'Rock/soil zonational file is unformatted: '
     &      // BDUM(1:NCHB)
          CALL WRMSGP( INDX )
        END IF
        OPEN(UNIT=27, FILE=BDUM(1:NCHB), STATUS='OLD',
     &    FORM='FORMATTED')
        VARB = 'Number of Zonation Lines'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
        NROCK = 0
        NULLIFY( ROCK_PTR )
        DO 440 NL = 1,NLIN
          CALL RD_INPL( CHDUM )
          CALL L_CASE( CHDUM )
          ISTART = 1
          VARB = 'Rock/Soil Name: '
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---      Empty rock list  ---
!
          IF( .NOT.ASSOCIATED(ROCK_PTR) ) THEN
            ALLOCATE( ROCK_PTR,STAT=ISTAT )
            IF( ISTAT.NE.0 ) THEN
              INDX = 3
              CHMSG = 'Allocation Error: ROCK_PTR'
              CALL WRMSGP( INDX )
            ENDIF
            ROCK_PTR%LIST_NAME = ADUM
            NULLIFY(ROCK_PTR%NEXT)
            NROCK = NROCK + 1
          ELSE
            LOC_PTR => ROCK_PTR
!
!---        Check for duplicate rock/soil name  ---
!
            DO
              IF( .NOT.ASSOCIATED(LOC_PTR) ) EXIT
              IF( LOC_PTR%LIST_NAME == ADUM ) THEN
                INDX = 4
                CHMSG = 'Duplicate Rock/Soil Name: ' // ADUM
                CALL WRMSGP( INDX )
              ELSE
                LOC_PTR => LOC_PTR%NEXT
              ENDIF
            ENDDO
!
!---        Add rock/soil name to rock list  ---
!
            NROCK = NROCK + 1
            ALLOCATE( TMP_PTR,STAT=ISTAT )
            IF( ISTAT.NE.0 ) THEN
              INDX = 3
              CHMSG = 'Allocation Error: ROCK_PTR'
              CALL WRMSGP( INDX )
            ENDIF
            TMP_PTR%LIST_NAME = ADUM
            TMP_PTR%NEXT => ROCK_PTR
            ROCK_PTR => TMP_PTR
          ENDIF
          LRC = NROCK
  440   CONTINUE
        CLOSE(UNIT=27)
!
!---  Assign rock/soil zonation information by indexing order,
!       useful for stochastic realizations  ---
!
      ELSEIF( INDEX(ADUM,'indexing').NE.0 ) THEN
        LRC = LFD
        NROCK = 1
!
!---  Read rock/soil zonation information from the input file  ---
!
      ELSE
        ISTART = 1
        VARB = 'Number of Zonation Lines'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
        NROCK = 0
        NSCALE = 0
        NULLIFY( ROCK_PTR )
        NULLIFY( SCALING_PTR )
        DO 480 NL = 1, NLIN
          CALL RD_INPL( CHDUM )
          CALL L_CASE( CHDUM )
          ISTART = 1
          ADUM(1:) = ' '
          VARB = 'Rock/Soil Name: '
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---      Empty rock list  ---
!
          IF( .NOT.ASSOCIATED(ROCK_PTR) ) THEN
            ALLOCATE( ROCK_PTR,STAT=ISTAT )
            IF( ISTAT.NE.0 ) THEN
              INDX = 3
              CHMSG = 'Allocation Error: ROCK_PTR'
              CALL WRMSGP( INDX )
            ENDIF
            ROCK_PTR%LIST_NAME = ADUM
            NULLIFY(ROCK_PTR%NEXT)
            NROCK = NROCK + 1
!
!---        Initialize scaling pointer  ---
!
            IF( ISLC(19).EQ.1 ) THEN
              ALLOCATE( SCALING_PTR,STAT=ISTAT )
              IF( ISTAT.NE.0 ) THEN
                INDX = 3
                CHMSG = 'Allocation Error: SCALING_PTR'
                CALL WRMSGP( INDX )
              ENDIF
              SCALING_PTR%ROCK_NAME = ADUM
              SCALING_PTR%ROCK_NUM = NROCK
              NULLIFY(SCALING_PTR%NEXT)
              NSCALE = NSCALE + 1
            ENDIF
          ELSE
            LOC_PTR => ROCK_PTR
!
!---        Check for repeated rock/soil name  ---
!
            DO
              IF( .NOT.ASSOCIATED(LOC_PTR) ) EXIT
              IF( LOC_PTR%LIST_NAME == ADUM ) GOTO 460
              LOC_PTR => LOC_PTR%NEXT
            ENDDO
!
!---        Add rock/soil name to rock list  ---
!
            NROCK = NROCK + 1
            ALLOCATE( TMP_PTR,STAT=ISTAT )
            IF( ISTAT.NE.0 ) THEN
              INDX = 3
              CHMSG = 'Allocation Error: ROCK_PTR'
              CALL WRMSGP( INDX )
            ENDIF
            TMP_PTR%LIST_NAME = ADUM
            TMP_PTR%NEXT => ROCK_PTR
            ROCK_PTR => TMP_PTR
!
!---        Add to scaling pointer  ---
!
            IF( ISLC(19).EQ.1 ) THEN
              SC_LOC_PTR => SCALING_PTR
              ALLOCATE( SC_TMP_PTR,STAT=ISTAT )
              IF( ISTAT.NE.0 ) THEN
                INDX = 3
                CHMSG = 'Allocation Error: SCALING_PTR'
                CALL WRMSGP( INDX )
              ENDIF
              SC_TMP_PTR%ROCK_NAME = ADUM
              SC_TMP_PTR%ROCK_NUM = NROCK
              SC_TMP_PTR%NEXT => SCALING_PTR
              SCALING_PTR => SC_TMP_PTR
            ENDIF
          ENDIF
  460     CONTINUE
          LRC = NROCK
!
!---      Read rock/soil domain  ---
!
          VARB = 'Rock/Soil Domain Index: '
          CALL RD_INT(ISTART,ICOMMA,CHDUM,I1)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,I2)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,J1)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,J2)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,K1)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,K2)
!
!---      Read scaling group associations  ---
!
          IF( ISLC(19).EQ.1 ) THEN
            ADUM(1:) = ' '
            VARB = 'Scaling Group Name: '
            CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
            SCALING_PTR%SCALING_NAME = ADUM
!
!---        Check for repeated scaling group name  ---
!
            SC_LOC_PTR => SCALING_PTR
            DO
              IF( .NOT.ASSOCIATED(SC_LOC_PTR) ) EXIT
              IF( SC_LOC_PTR%SCALING_NAME == ADUM ) GOTO 470
              SC_LOC_PTR => SC_LOC_PTR%NEXT
            ENDDO
            NSCALE = NSCALE + 1
  470       CONTINUE
            SCALING_PTR%SCALING_NUM = NSCALE
          ENDIF
  480   CONTINUE
!
!---    Assign scaling index  ---
!
        IF( ISLC(19).EQ.1 ) THEN
!
!---      Allocate memory for the rock/soil zonation index array  ---
!
          ALLOCATE( ISCALE(1:LRC),STAT=ISTAT )
          IF( ISTAT.NE.0 ) THEN
            INDX = 3
            CHMSG = 'Allocation Error: ISCALE'
            CALL WRMSGP( INDX )
          ENDIF
          SC_LOC_PTR => SCALING_PTR
          DO
            IF( .NOT.ASSOCIATED(SC_LOC_PTR) ) EXIT
            IROCK = SC_LOC_PTR%ROCK_NUM
            ISCALE(IROCK) = SC_LOC_PTR%SCALING_NUM
            SC_LOC_PTR => SC_LOC_PTR%NEXT
          ENDDO
        ENDIF
      ENDIF
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_ROCK group
!
      RETURN
      END

!-------------------------Disclaimer-----------------------------------!
!
      SUBROUTINE RD_SFCOV
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
!     Read Surface Cover Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by SK White, PNNL, 10 September 2015.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE PLT_ATM
      USE GRID
      USE FILES
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*128 ADUM,BDUM,CDUM
      CHARACTER*512 CHDUM,UNTS
      REAL*8, DIMENSION(:), ALLOCATABLE :: XSPX,YSPX
      INTEGER ITMP(LFX,LFY),NTMP(LFX,LFY)
      TYPE(LIST_NODE), POINTER :: LOC_PTR,TMP_PTR
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_SFCOV'
!
!---  Assign card string  ---
!
      CARD = 'Surface Cover Card'
!
!---  Initialize arrays  ---
!
      DO J = 1,LFY
        DO I = 1,LFX
          ITMP(I,J) = 0
        ENDDO
      ENDDO
!
!---  Read number of surface cover areas  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Surface Cover Areas'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NSFCA)
      LSFCA = MAX( LSFCA,NSFCA )
      L_SFC = 1
!
!---  Loop over number of surface cover areas  ---
!
      NULLIFY( SFC_PTR )
      DO 480 NSC = 1, NSFCA
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
        ADUM(1:) = ' '
        VARB = 'Surface Cover Area Name'
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---    Empty surface cover area list  ---
!
        IF( .NOT.ASSOCIATED(SFC_PTR) ) THEN
          ALLOCATE( SFC_PTR,STAT=ISTAT )
          IF( ISTAT.NE.0 ) THEN
            INDX = 3
            CHMSG = 'Allocation Error: SFC_PTR'
            CALL WRMSGP( INDX )
          ENDIF
          SFC_PTR%LIST_NAME = ADUM
          NULLIFY(SFC_PTR%NEXT)
        ELSE
          LOC_PTR => SFC_PTR
!
!---     Check for repeated surface cover area name  ---
!
          DO
            IF( .NOT.ASSOCIATED(LOC_PTR) ) EXIT
            IF( LOC_PTR%LIST_NAME == ADUM ) THEN
              INDX = 4
              CHMSG = 'Duplicate Surface Area Name'
              CALL WRMSGP( INDX )
            ENDIF
            LOC_PTR => LOC_PTR%NEXT
          ENDDO
!
!---      Add surface cover name to surface cover list  ---
!
          ALLOCATE( TMP_PTR,STAT=ISTAT )
          IF( ISTAT.NE.0 ) THEN
            INDX = 3
            CHMSG = 'Allocation Error: SFC_PTR'
            CALL WRMSGP( INDX )
          ENDIF
          TMP_PTR%LIST_NAME = ADUM
          TMP_PTR%NEXT => SFC_PTR
          SFC_PTR => TMP_PTR
        ENDIF
!
!---    Read number of surface cover polygons corresponding to 
!       surface cover area  ---
!
        VARB = 'Number of Surface Cover Polygons'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,NPLYX)
        IF( NPLYX.EQ.0 ) NPLYX = 1
!
!---    Loop over number of surface cover polyons  ---
!
        DO 400 NP = 1, NPLYX
!
!---      Read number of surface cover polygon definition x,y pairs  ---
!
          CALL RD_INPL( CHDUM )
          CALL L_CASE( CHDUM )
          ISTART = 1
          VARB = 'Number of Surface Cover Polygon Definition Pairs'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,NSFCPX)
          LSFCP = MAX( LSFCP,(NSFCPX+1) )            
!
!---      Read units for surface cover polygon coordinate units  ---
!
          VARB = 'Surface Cover Polygon Coordinate Units'
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          VARX = 1.D+0
          INDX = 0
          IUNM = 1
          CALL RD_UNIT(UNTS,VARX,INDX)
!
!---      Allocate memory for polygon point arrays  ---
!
          IF( .NOT.ALLOCATED(XSPX) ) THEN
            ALLOCATE( XSPX(LSFCP),STAT=ISTAT )
            IF( ISTAT.NE.0 ) THEN
              INDX = 3
              CHMSG = 'Allocation Error: XSPX'
              CALL WRMSGP( INDX )
            ENDIF
          ENDIF
!
          IF( .NOT.ALLOCATED(YSPX) ) THEN
            ALLOCATE( YSPX(LSFCP),STAT=ISTAT )
            IF( ISTAT.NE.0 ) THEN
              INDX = 3
              CHMSG = 'Allocation Error: YSPX'
              CALL WRMSGP( INDX )
            ENDIF
          ENDIF
!
!---      Read surface cover polygon coordinates  ---
!
          ISTART = 1
          CALL RD_INPL( CHDUM )
          CALL L_CASE( CHDUM )
          VARB = 'Surface Cover Polygon Points'
          DO 300 I = 1,NSFCPX   
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,XSPX(I))
            XSPX(I) = XSPX(I)*VARX
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,YSPX(I))
            YSPX(I) = YSPX(I)*VARX
  300     CONTINUE
!
!---      Check to make sure polygon is closed. If not, close it.  ---
!         
          IF ( XSPX(1).NE.XSPX(NSFCPX) .OR. 
     &      YSPX(1).NE.YSPX(NSFCPX) ) THEN
            NSFCPX = NSFCPX + 1
            XSPX(NSFCPX) = XSPX(1)
            YSPX(NSFCPX) = YSPX(1)
          ENDIF          
!
!---      Loop over nodes from top down to determine how many nodes are 
!         active surface nodes. If the top node is inactive, find the  
!         top most active node. If node is an active surface node check  
!         to see if it is within the surface cover area polygon     
!   
          DO 350 J = 1,LFY
          DO 350 I = 1,LFX
            DO 310 K = LFZ,1,-1
              N = ND(I,J,K)           
              IF( IXP(N).NE.0 ) THEN
                XPT = 2.5D-1*(XE(5,N)+XE(6,N)+XE(7,N)+XE(8,N))
                YPT = 2.5D-1*(YE(5,N)+YE(6,N)+YE(7,N)+YE(8,N))             
                CALL LOC_PT( XPT,YPT,XSPX,YSPX,NSFCPX,IINOUT,IPATH )
                IF ( IINOUT.GE.0 ) THEN
                  ITMP(I,J) = NSC
                  NTMP(I,J) = N
                ENDIF     
                EXIT
              ENDIF
  310      CONTINUE
  350     CONTINUE
  400   CONTINUE
!
!---    Read number of surface cover times  ---
!
        ISTART = 1
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        VARB = 'Number of Surface Cover Times'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,ISFCTX)
        IF( ISFCTX.EQ.0 ) THEN
          INDX = 4
          CHMSG = 'No Surface Cover Times'
          CALL WRMSGP( INDX )
        ENDIF
        LSFCT = MAX( LSFCT,ISFCTX )
!
!---    Skip over the surface cover variables and units  ---
!
        DO 450 NTM = 1,ISFCTX
          CALL RD_INPL( CHDUM )
          CALL L_CASE( CHDUM )
          ISTART = 1
  450   CONTINUE
!
  480 CONTINUE
!
!---  Loop over nodes in xy plane and count number of nodes 
!     within surface cover areas.
!   
      NC = 0        
      DO 550 NSC = 1,NSFCA
        DO 525 J = 1,LFY
        DO 525 I = 1,LFX
          IF( ITMP(I,J).EQ.NSC ) NC = NC + 1
  525   CONTINUE
  550 CONTINUE
      IF( NC.EQ.0 ) THEN
        INDX = 4
        CHMSG = 'No Nodes Within Surface Cover Areas'
        CALL WRMSGP( INDX )
      ENDIF
      NSFCN = NC
      LSFCN = MAX( LSFCN,NSFCN )
!
!---  Allocate memory for surface cover connection map array  ---
!
      ALLOCATE( ICM_SFC(1:1,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ICM_SFC'
        CALL WRMSGP( INDX )
      ENDIF
!
!---  Loop over nodes in xy plane and fill top node for surface
!     cover connection map index
! 
      NC = 0        
      DO 650 NSC = 1,NSFCA
        DO 625 J = 1,LFY
        DO 625 I = 1,LFX
          IF( ITMP(I,J).EQ.NSC ) THEN
            NC = NC + 1
            ICM_SFC(1,NC) = NTMP(I,J)
          ENDIF
  625   CONTINUE
  650 CONTINUE
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_SFCOV group
!
      RETURN
      END
      
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_SDSP
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
!     Read aqueous reaction species.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 8 December 2004.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
      USE REACT
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_SDSP'
!
!---  Assign card string  ---
!
      CARD = 'Solid Species Card'
!
!---  Read number of aqueous species  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Solid Species'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NSPS)
      LSPS = MAX( LSPS,NSPS )
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_SDSP group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_SF
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
!     Read Surface Flux Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 27 November 2002.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE TRNSPT
      USE TABL
      USE SOURC
      USE SOLTN
      USE PORMED
      USE OUTPU
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM,ADUM,BDUM,FDUM,GDUM
      LOGICAL FLG_CHK
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_SF'
!
!---  Assign card string  ---
!
      CARD = 'Surface Flux Card'
!
!---  Read surface flux card information  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Surface Flux Inputs'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NSF)
      LSF = MAX( LSF,NSF )
      NS = 0
      NSFF = 0
      DO
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
        CALL CHK_INT(ISTART,ICOMMA,CHDUM,INDX)
        IF( INDX == 1 ) THEN
          NSFF = NSFF+1
          CYCLE
        ENDIF
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'solute') /= 0 ) THEN
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
        END IF
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'file') /= 0 ) THEN
          CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
          NCHF = INDEX(FDUM,'  ')-1
!
!---      Check that surface flux domain file exists  ---
!
          INQUIRE( FILE=FDUM(1:NCHF), FORM=GDUM, EXIST=FLG_CHK )
          IF( .NOT.FLG_CHK ) THEN
            INDX = 4
            CHMSG = 'Surface-Flux-Domain File: '
     &        // FDUM(1:NCHF)
            CALL WRMSGP( INDX )
          ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
            INDX = 4
            CHMSG = 'Unformatted Surface-Flux-Domain File: '
     &        // FDUM(1:NCHF)
            CALL WRMSGP( INDX )
          ENDIF
          OPEN(UNIT=27,FILE=FDUM(1:NCHF),STATUS='OLD',FORM='FORMATTED')
          NC = 0
   30     CONTINUE
          READ(27,*,END=40) IX,JX,KX,ISFDX
          NC = NC + 1
          GOTO 30
   40     CONTINUE
          CLOSE(27)
          LSFDOM = MAX( LSFDOM,NC )
        END IF
        NS = NS+1
        IF( NS >= NSF ) EXIT
      END DO
!
!---  All surface flux files are named  ---
!
      IF( NSFF.EQ.NSF ) LSF = LSF+1
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_SF group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_SIMU
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
!     Read Simulation Title Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 October 2002.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_SIMU'
!
!---  Assign card string  ---
!
      CARD = 'Simulation Title Card'
!
!---  Skip over six input lines  ---
!
      LSKIP = 6
      DO 100 L = 1,LSKIP
        CALL RD_INPL( CHDUM )
  100 CONTINUE
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Simulation Note Lines: '
      CALL RD_INT(ISTART,ICOMMA,CHDUM,LNOTES)
      LNOTES = MAX( 1,LNOTES )
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_SIMU group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_SOLU
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
!     Read Solution Control Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 21 November 2002.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*128 ADUM,BDUM
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_SOLU'
!
!---  Assign card string  ---
!
      CARD = 'Solution Control Card'
!
!---  Read Execution Option  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Execution Option'
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      IF( INDEX(ADUM(1:),'normal').NE.0 ) THEN
        IF( INDEX(ADUM(1:),'no flow').NE.0 ) THEN
          IEO = 21
        ELSEIF( INDEX(ADUM(1:),'dynamic').NE.0 ) THEN
          IEO = 11
        ELSE
          IEO = 1
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'restart').NE.0 ) THEN
        IF( INDEX(ADUM(1:),'no flow').NE.0 ) THEN
          IF( INDEX(ADUM(1:),'zero solutes').NE.0 ) THEN
            IEO = 24
          ELSE
            IEO = 22
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'dynamic').NE.0 ) THEN
          IF( INDEX(ADUM(1:),'zero solutes').NE.0 ) THEN
            IEO = 14
          ELSE
            IEO = 12
          ENDIF
        ELSE
          IF( INDEX(ADUM(1:),'zero solutes').NE.0 ) THEN
            IEO = 4
          ELSE
            IEO = 2
          ENDIF
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'initial').NE.0 ) THEN
        IEO = 3
      ENDIF
!
!---  Scaling Factor Option  ---
!
      IF( INDEX(ADUM(1:),'scaling').NE.0 ) ISLC(19) = 1
!
!---  Inverse (UCode) Option  ---
!
      IF( INDEX(ADUM(1:),'inverse').NE.0 .OR.
     &  INDEX(ADUM(1:),'ucode').NE.0 ) ISLC(20) = 1
!
!---  Set solver option  ---
!

      LSP = 0
      LBD = 1
      LPT = 0
      LIS = 0

!
!---  Equation switch parameters  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Operational Mode'
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      IF( INDEX(ADUM(1:),'transport').NE.0 ) LC = 1
      IF( INDEX(ADUM(1:),'geomechanics').NE.0 ) LM = 1
      IF( INDEX(ADUM(1:),'eckechem').NE.0 )  LR = 1
      IF( INDEX(ADUM(1:),'5512').NE.0 ) LC = 1
      IF( INDEX(ADUM(1:),'ice').NE.0 ) LFW = 1
      IF( INDEX(ADUM(1:),'surface spill').NE.0 )  LSPILL = 1
!
!---  Water-NComponent-NaCl-Energy (H2O-NComponent-Nacl-E) Operational Mode  ---
!
      IF( ((INDEX(ADUM(1:),'energy').NE.0 .OR.
     &  INDEX(ADUM(1:),'-e ').NE.0) .AND.
     &  (INDEX(ADUM(1:),'water').NE.0 .OR.
     &  INDEX(ADUM(1:),'h2o').NE.0) .AND.
     &  (INDEX(ADUM(1:),'salt').NE.0 .OR.
     &  INDEX(ADUM(1:),'nacl').NE.0) .AND.
     &  (INDEX(ADUM(1:),'ncomponent').NE.0 .OR.
     &  INDEX(ADUM(1:),'n-component').NE.0 .OR.
     &  INDEX(ADUM(1:),'n component').NE.0)) .OR.
     &  INDEX(ADUM(1:),'stomp-comp').NE.0 ) THEN
        IOM = 40
        LUK = 3
        LFW = 1
        LGC = 1
        LG = 1
        LT = 1
        LS = 1
        LPTA = 1 
!
!---  H2O-CO2-CH4-HCO2-HCH4-NaCl-E Operational Mode  ---
!
      ELSEIF( (INDEX(ADUM(1:),'energy').NE.0 .OR.
     &  INDEX(ADUM(1:),'-e').NE.0) .AND.
     &  (INDEX(ADUM(1:),'water').NE.0 .OR.
     &  INDEX(ADUM(1:),'h2o').NE.0) .AND.
     &  INDEX(ADUM(1:),'mco2').NE.0 .AND.
     &  INDEX(ADUM(1:),'mch4').NE.0 .AND.
     &  INDEX(ADUM(1:),'hco2').NE.0 .AND.
     &  INDEX(ADUM(1:),'hch4').NE.0 .AND.
     &  (INDEX(ADUM(1:),'salt').NE.0 .OR.
     &  INDEX(ADUM(1:),'nacl').NE.0) ) THEN
        IOM = 39
        LUK = 7
        LT = 1
        LS = 1
        LG = 1
        LN = 1
        LNNGC = 3
        LNGC = 3
        LHYD = 1
        LPTA = 1
!
!---  H2O-CO2-CH4-NaCl-E Operational Mode  ---
!
      ELSEIF( (INDEX(ADUM(1:),'energy').NE.0 .OR.
     &  INDEX(ADUM(1:),'-e').NE.0) .AND.
     &  (INDEX(ADUM(1:),'water').NE.0 .OR.
     &  INDEX(ADUM(1:),'h2o').NE.0) .AND.
     &  INDEX(ADUM(1:),'co2').NE.0 .AND.
     &  INDEX(ADUM(1:),'ch4').NE.0 .AND.
     &  (INDEX(ADUM(1:),'salt').NE.0 .OR.
     &  INDEX(ADUM(1:),'nacl').NE.0) .AND.
     &  INDEX(ADUM(1:),'hyd2').NE.0 ) THEN
        IOM = 37
        LUK = 5
        LT = 1
        LS = 1
        LG = 1
        LN = 1
        LNGC = 3
        LNNGC = 3
        LHYD = 1
        LPTA = 1
!
!---  H2O-CO2-CH4-NaCl-E Operational Mode  ---
!
      ELSEIF( INDEX(ADUM(1:),'hyd1').NE.0 ) THEN
        IOM = 36
        LUK = 5
        LT = 1
        LS = 1
        LG = 1
        LNGC = 3
        LNNGC = 3
        LHYD = 1
        LPTA = 1
!
!---  EOR Operational Mode  ---
!
      ELSEIF( INDEX(ADUM(1:),'eor').NE.0 ) THEN
        LPF_EOR = 73
        IOM = 43
        LUK = 3
        LT = 1
        LS = 1
        LN = 1
        LG = 1
        LPTA = 1
        LGC = 1
        LNNGC = 1
!
!---    Read equation of state  ---
!
        VARB = 'Cubic Equation of State'
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
        IF( INDEX(BDUM(1:),'black').NE.0 .AND.
     &    INDEX(BDUM(1:),'oil').NE.0 ) THEN
          ISLC(68) = 0
          NGC = 0
          LNGC = MAX( LNGC,NGC ) + 2
          LUK = 2
          LT = 0
          LUK = LUK + LNGC
        ELSE
          IF( INDEX(BDUM(1:),'peng').NE.0 .AND.
     &      INDEX(BDUM(1:),'robinson').NE.0 ) THEN
            ISLC(68) = 1
          ELSEIF( INDEX(BDUM(1:),'soave').NE.0 .AND.
     &      INDEX(BDUM(1:),'redlich').NE.0 .AND.
     &      INDEX(BDUM(1:),'kwong').NE.0 ) THEN
            ISLC(68) = 2
          ENDIF
!
!---      Read number of petroleum components  ---
!
          VARB = 'Number of Petroleum Components'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,NGC)
          IF( INDEX(ADUM(1:),'iso-co2').NE.0 ) THEN
            ISLC(45) = 1
          ENDIF
          IF( INDEX(ADUM(1:),'iso-ch4').NE.0 ) THEN
            ISLC(64) = 1
          ENDIF
          LNGC = MAX( LNGC,NGC ) + 2
          LUK = LUK + LNGC
        ENDIF
        IF( INDEX(ADUM(1:),'isotherm').NE.0 ) THEN
          ISLC(30) = 1
        ELSEIF( ISLC(68).EQ.0 ) THEN
          INDX = 4
          CHMSG = 'Option Conflict: Black-Oil without Isothermal'
          CALL WRMSGP( INDX )        
        ENDIF
        IF( INDEX(ADUM(1:),'iso-h2o').NE.0 ) THEN
          ISLC(67) = 1
        ENDIF
        IF( INDEX(ADUM(1:),'isobrine').NE.0 ) THEN
          ISLC(32) = 1
        ENDIF
!
!---    Dual Porosity Model Option  ---
!
        IF( INDEX(ADUM(1:),'dual porosity').NE.0 .OR.
     &    INDEX(ADUM(1:),'dual-porosity').NE.0 ) THEN
          ISLC(11) = 1
          L_DP = 1
        ENDIF
!
!---    Equivalent Continuum Model Option  ---
!
        IF( INDEX(ADUM(1:),'equivalent continuum').NE.0 .OR.
     &    INDEX(ADUM(1:),'equivalent-continuum').NE.0 ) THEN
          ISLC(11) = 2
          L_EC = 1
        ENDIF
!
!---  HYDT-KE Operational Mode  ---
!
      ELSEIF( INDEX(ADUM(1:),'hydt-ke').NE.0 ) THEN
        IOM = 39
        LUK = 9
        LT = 1
        LS = 1
        LN = 1
        LG = 1
        LHYD = 1
        LN2 = 1
        LNHC = 4
        LNNGC = 3
        LPTA = 1
        IF( INDEX(ADUM(1:),'iso-co2').NE.0 ) THEN
          ISLC(45) = 1
          LUK = LUK - 2
        ENDIF
        IF( INDEX(ADUM(1:),'iso-ch4').NE.0 ) THEN
          ISLC(64) = 1
          LUK = LUK - 2
        ENDIF
        IF( INDEX(ADUM(1:),'iso-n2').NE.0 ) THEN
          ISLC(65) = 1
          LUK = LUK - 2
        ENDIF
        IF( INDEX(ADUM(1:),'isobrine').NE.0 ) THEN
          ISLC(32) = 1
          LUK = LUK - 1
        ENDIF
!
!---  HYD-KE Operational Mode  ---
!
      ELSEIF( INDEX(ADUM(1:),'hyd-ke').NE.0 ) THEN
        IOM = 38
        LUK = 7
        LT = 1
        LS = 1
        LG = 1
        LNNGC = 3
        LNGC = 3
        LHYD = 1
        LPTA = 1
!
!---  HYD Operational Mode  ---
!
      ELSEIF( INDEX(ADUM(1:),'hyd').NE.0 ) THEN
        IOM = 37
        LUK = 5
        LT = 1
        LS = 1
        LG = 1
        LN = 1
        LNGC = 3
        LNNGC = 3
        LHYD = 1
        LPTA = 1
!
!---  STOMP-CO2e Operational Mode  ---
!
      ELSEIF( ((INDEX(ADUM(1:),'energy').NE.0 .OR.
     &  INDEX(ADUM(1:),'-e').NE.0) .AND.
     &  (INDEX(ADUM(1:),'water').NE.0 .OR.
     &  INDEX(ADUM(1:),'h2o').NE.0) .AND.
     &  INDEX(ADUM(1:),'co2').NE.0 .AND.
     &  (INDEX(ADUM(1:),'salt').NE.0 .OR.
     &  INDEX(ADUM(1:),'nacl').NE.0)) .OR.
     &  INDEX(ADUM(1:),'stomp-co2e').NE.0 ) THEN
        IOM = 33
        LUK = 4
        LT = 1
        LS = 1
        LG = 1
        LPTA = 1
        IF( INDEX(ADUM(1:),'isotherm').NE.0 ) ISLC(30) = 1
!
!---  STOMP-CO2 Operational Mode  ---
!
      ELSEIF( ((INDEX(ADUM(1:),'water').NE.0 .OR.
     &  INDEX(ADUM(1:),'h2o').NE.0) .AND.
     &  INDEX(ADUM(1:),'co2').NE.0 .AND.
     &  (INDEX(ADUM(1:),'salt').NE.0 .OR.
     &  INDEX(ADUM(1:),'nacl').NE.0)) .OR.
     &  INDEX(ADUM(1:),'stomp-co2').NE.0 ) THEN
        IOM = 32
        LUK = 3
        LS = 1
        LG = 1
        LPTA = 1
!
!---  Water-Air-Oil Operational Mode  ---
!
      ELSEIF( ((INDEX(ADUM(1:),'water').NE.0 .OR.
     &  INDEX(ADUM(1:),'h2o').NE.0) .AND.
     &  INDEX(ADUM(1:),'air').NE.0 .AND.
     &  INDEX(ADUM(1:),'oil').NE.0) .OR.
     &  INDEX(ADUM(1:),'stomp-woa').NE.0 ) THEN
        IOM = 5
        LUK = 3
        LG = 1
        LN = 1
        IF( INDEX(ADUM(1:),'kinetic').NE.0 .AND.
     &    INDEX(ADUM(1:),'volatil').NE.0 ) THEN
          LUK = 4
          LD = 1
          ISLC(46) = 1
        ENDIF
!
!---  Water-Oil Operational Mode  ---
!
      ELSEIF( ((INDEX(ADUM(1:),'water').NE.0 .OR.
     &  INDEX(ADUM(1:),'h2o').NE.0) .AND.
     &  INDEX(ADUM(1:),'oil').NE.0) .OR.
     &  INDEX(ADUM(1:),'stomp-wo').NE.0 ) THEN
        IOM = 4
        LUK = 2
        LN = 1
!
!---  Water-Air-Energy (H2O-Air-E) Operational Mode  ---
!
      ELSEIF( ((INDEX(ADUM(1:),'energy').NE.0 .OR. 
     &  INDEX(ADUM(1:),'-e ').NE.0) .AND.
     &  (INDEX(ADUM(1:),'water').NE.0 .OR.
     &  INDEX(ADUM(1:),'h2o').NE.0) .AND.
     &  INDEX(ADUM(1:),'air').NE.0) .OR.
     &  INDEX(ADUM(1:),'stomp-wae').NE.0 ) THEN
        IOM = 3
        LUK = 3
        LFW = 1
        LT = 1
        LG = 1
!
!---  Geothermal (GT) Operational Mode  ---
!
      ELSEIF( INDEX(ADUM(1:),'stomp-gt').NE.0 .OR.
     &  INDEX(ADUM(1:),'geothermal').NE.0 ) THEN
        IOM = 3
        LUK = 4
        LFW = 1
        LT = 1
        LG = 1
        LS = 1
        IF( INDEX(ADUM(1:),'isobrine').NE.0 ) THEN
          ISLC(32) = 1
        ENDIF
!
!---  Water-CO2 (H2O-CO2) Operational Mode  ---
!
      ELSEIF( (INDEX(ADUM(1:),'water').NE.0 .OR.
     &  INDEX(ADUM(1:),'h2o').NE.0) .AND.
     &  INDEX(ADUM(1:),'co2').NE.0 ) THEN
        IOM = 22
        LUK = 2
        LG = 1
        LPTA = 1
!
!---  Water (H2O) Operational Mode  ---
!
      ELSEIF( INDEX(ADUM(1:),'water').NE.0 .OR.
     &  INDEX(ADUM(1:),'h2o').NE.0 .OR. 
     &  INDEX(ADUM(1:),'stomp-w').NE.0 ) THEN
        IOM = 1
        LUK = 1
!
!---  Fluid Operational Mode  ---
!
      ELSEIF( INDEX(ADUM(1:),'fluid').NE.0 ) THEN
        IOM = 1
        LUK = 1
      ENDIF
!
!---  Number of execution periods parameter  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Execution Periods'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,LEPD)
      LEPD = MAX( 1,LEPD )
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_SOLU group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_SP
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
!     Read Saturation Function Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 25 November 2002.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GRID
      USE GLB_PAR
      USE PORMED
      USE SOLTN
      USE TABL
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*128 ADUM,BDUM,RDUM
      CHARACTER*512 CHDUM
      TYPE(LIST_NODE), POINTER :: LOC_PTR
      TYPE(LIST_SCALING), POINTER :: SC_LOC_PTR
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_SP'
!
!---  Assign card string  ---
!
      CARD = 'Saturation Function Card'
!
!---  Skip fluid-pair interfacial tension line  ---
!
      IF( (IOM.GE.4 .AND. IOM.LE.9) .OR. IOM.EQ.24 
     &  .OR. (IOM.GE.35 .AND. IOM.LE.39) 
     &  .OR. IOM.EQ.44 .OR. IOM.EQ.45 ) 
     &  CALL RD_INPL( CHDUM )
!
!---  Loop over the rock/soil saturation information lines  ---
!
      N = 0
      IJK = 0
   10 CONTINUE
      IF( N.GE.NROCK .OR. IJK.GT.0 ) GOTO 500
      ISTART = 1
      VARB = 'Saturation Function: Rock Name: '
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,RDUM)
!
!---  IJK, KIJ, or JKI indexing  ---
!
      IF( INDEX(RDUM(1:),'indexing').NE.0 ) THEN
        IF( INDEX(RDUM,'ijk').NE.0 ) THEN
          IJK = 1
        ELSEIF( INDEX(RDUM,'jki').NE.0 ) THEN
          IJK = 2
        ELSEIF( INDEX(RDUM,'kij').NE.0 ) THEN
          IJK = 3
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Indexing Option' // RDUM(1:NCH)
          CALL WRMSGP( INDX )
        ENDIF
        GOTO 220
      ENDIF
!
!---  Search known rock types for a matching type  ---
!
        LOC_PTR => ROCK_PTR
        IROCK = 0
        DO
          IF( .NOT.ASSOCIATED(LOC_PTR) ) EXIT
          IROCK = IROCK + 1
          IF( LOC_PTR%LIST_NAME == RDUM ) THEN
            GOTO 200
          ELSE
            LOC_PTR => LOC_PTR%NEXT
          ENDIF
        ENDDO
!
!---  Search known scaling groups for a matching type  ---
!
      IF( ISLC(19).EQ.1 ) THEN
        SC_LOC_PTR => SCALING_PTR
        ISGRP = 0
        DO
          IF( .NOT.ASSOCIATED(SC_LOC_PTR) ) EXIT
          IF( SC_LOC_PTR%SCALING_NAME == RDUM ) THEN
            ISGRP = SC_LOC_PTR%SCALING_NUM
            IROCK = 1
            GOTO 200
          ELSE
            SC_LOC_PTR => SC_LOC_PTR%NEXT
          ENDIF
        ENDDO
        INDX = 2
        CHMSG = 'Unrecognized Rock/Soil Type or Scaling Group: '
     &    // RDUM(1:NCH)
        CALL WRMSGP( INDX )
        GOTO 10
      ENDIF
      INDX = 2
      CHMSG = 'Unrecognized Rock/Soil Type: ' // RDUM(1:NCH)
      CALL WRMSGP( INDX )
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
!---  End rock/soil or scaling group input  ---
!
  220 CONTINUE
      N = N + 1
!
!---  Read saturation/capillary pressure function for
!     tabular forms  ---
!
      VARB = 'Saturation Function Type: '
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      ISCHRX = 0
      IF( INDEX(ADUM(1:),'tabular').NE.0 ) THEN
        IF( INDEX( ADUM(1:),'spline' ).NE.0 ) THEN
          IF( INDEX( ADUM(1:),'hysteretic' ).NE.0 ) THEN
            ISCHRX = 13
          ELSE
            ISCHRX = 11
          ENDIF
        ELSE
          IF( INDEX( ADUM(1:),'hysteretic' ).NE.0 ) THEN
            ISCHRX = 12
          ELSE
            ISCHRX = 10
          ENDIF
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'polynomial').NE.0 ) THEN
        ISCHRX = 19
      ENDIF
!
!---  Tabular non-hysteretic  ---
!
      IF( ISCHRX.EQ.10 .OR. ISCHRX.EQ.11 ) THEN
!
!---    IJK Indexing  ---
!
        IF( IJK.GT.0 ) THEN
          VARB = 'Number of Tables'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,NTABX)
          NTBL = 0
          DO 272 NTX = 1,NTABX
            ISTART = 1
            CALL RD_INPL( CHDUM )
            CALL L_CASE( CHDUM )
            VARB = 'Number of Table Entries'
            CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
            IF( NLIN.LT.2 ) THEN
              INDX = 4
              CHMSG = 'Invalid Saturation Function Table'
              CALL WRMSGP( INDX )
            ENDIF
            NTBL = NTBL + NLIN
            DO 270 NLX = 1,NLIN
              CALL RD_INPL( CHDUM )
  270       CONTINUE
  272     CONTINUE
          LTBL = LTBL + NTBL
        ELSE
          VARB = 'Number of Table Entries'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
          IF( NLIN.LT.2 ) THEN
            INDX = 4
            CHMSG = 'Invalid Saturation Function Table'
            CALL WRMSGP( INDX )
          ENDIF
          DO 274 NL = 1,NLIN
            CALL RD_INPL( CHDUM )
  274     CONTINUE
          NTBL = NLIN
          LTBL = LTBL + NTBL
        ENDIF
!
!---  Tabular hysteretic  ---
!
      ELSEIF( ISCHRX.EQ.12 .OR. ISCHRX.EQ.13 ) THEN
!
!---    IJK Indexing  ---
!
        IF( IJK.GT.0 ) THEN
          VARB = 'Number of Drainage Saturation Tables'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,NDTABLX)
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          VARB = 'Number of Imbibition Saturation Tables'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,NITABLX)
          NTBL = 0
          DO 282 NTX = 1,NDTABLX
            ISTART = 1
            CALL RD_INPL( CHDUM )
            CALL L_CASE( CHDUM )
            VARB = 'Number of Drainage Table Entries'
            CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
            IF( NLIN.LT.2 ) THEN
              INDX = 4
              CHMSG = 'Invalid Saturation Drainage Table'
              CALL WRMSGP( INDX )
            ENDIF
            NTBL = NTBL + NLIN
            DO 280 NLX = 1,NLIN
              CALL RD_INPL( CHDUM )
  280       CONTINUE
  282     CONTINUE
          DO 286 NTX = 1,NITABLX
            ISTART = 1
            CALL RD_INPL( CHDUM )
            CALL L_CASE( CHDUM )
            VARB = 'Number of Imbibition Table Entries'
            CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
            IF( NLIN.LT.2 ) THEN
              INDX = 4
              CHMSG = 'Invalid Saturation Imbibition Table'
              CALL WRMSGP( INDX )
            ENDIF
            NTBL = NTBL + NLIN
            DO 284 NLX = 1,NLIN
              CALL RD_INPL( CHDUM )
  284       CONTINUE
  286     CONTINUE
          LTBL = LTBL + NTBL
        ELSE
          ISTART = 1
          CALL RD_INPL( CHDUM )
          CALL L_CASE( CHDUM )
          VARB = 'Number of Drainage Table Entries'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
          IF( NLIN.LT.2 ) THEN
            INDX = 4
            CHMSG = 'Invalid Saturation Drainage Table'
            CALL WRMSGP( INDX )
          ENDIF
          DO 292 NL = 1,NLIN
            CALL RD_INPL( CHDUM )
  292     CONTINUE
          NTBL = NLIN
          LTBL = LTBL + NTBL
          ISTART = 1
          CALL RD_INPL( CHDUM )
          CALL L_CASE( CHDUM )
          VARB = 'Number of Imbibition Table Entries'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
          IF( NLIN.LT.2 ) THEN
            INDX = 4
            CHMSG = 'Invalid Saturation Imbibition Table'
            CALL WRMSGP( INDX )
          ENDIF
          DO 294 NL = 1,NLIN
            CALL RD_INPL( CHDUM )
  294     CONTINUE
          NTBL = NLIN
          LTBL = LTBL + NTBL
        ENDIF
!
!---  Polynomial  ---
!
      ELSEIF( ISCHRX.EQ.19 ) THEN
!
!---    IJK Indexing  ---
!
        IF( IJK.GT.0 ) THEN
          INDX = 4
          CHMSG = 'Polynomial Coefficients not Available with ' // 
     &      'IJK Indexing'
          CALL WRMSGP( INDX )
        ENDIF
        VARB = 'Number of Polynomial Function Pieces'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,NPOLY)
        LPOLYN = MAX( LPOLYN,NPOLY )
        DO 290 NP = 1,NPOLY
          ISTART = 1
          CALL RD_INPL( CHDUM )
          CALL L_CASE( CHDUM )
          VARB = 'Number of Polynomial Coefficients'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,NCOEF)
          LPOLYC = MAX( LPOLYC,NCOEF+4 )
  290   CONTINUE
      ENDIF
!
!---  Loop over remaining rock/soils within scaling group  ---
!
      IF( ISLC(19).EQ.1 .AND. IROCK.LT.NROCK ) THEN
        DO 490 M = IROCK+1,NROCK
          IF( ISCALE(M).EQ.ISGRP ) THEN
            N = N+1
          ENDIF
  490   CONTINUE
      ENDIF
!
!---  Read next rock/soil type or scaling group  ---
!
      GOTO 10
 500  CONTINUE
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_SP group
!
      RETURN
      END
      
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_SPLK
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
!     Read reactive species link.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 December 2004.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
      USE REACT
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_SPLK'
!
!---  Assign card string  ---
!
      CARD = 'Reactive Species Link Card'
!
!---  Read number of component species  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Reactive Species Links'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NSPX)
      LSPLK = MAX( LSPLK,NSPX )
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_SPLK group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_SR
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
!     Read Source Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 10 October 2002.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE GRID
      USE GLB_PAR
      USE FILES
      USE CONST
      USE BCV
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM,CHDUMX
      CHARACTER*128 ADUM,BDUM,FDUM,FMDUM
      CHARACTER*64 UNTS
      LOGICAL FCHK
      REAL*8 SRCPX(8)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_SR'
      EPSLX = 1.D-12
!
!---  Assign card string  ---
!
      CARD = 'Source Card'
!
!---  Read number of source inputs  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Sources: '
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NSR)
      NSRX = 0
      DO 200 NS = 1,NSR
!
!---    Read source type, domain, and number of times  ---
!
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
!
!---    Read source type  ---
!
        VARB = 'Source Type'
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        ISRTX = 0
        IF( INDEX(ADUM(1:),'bioslurping').NE.0 .AND.
     &    INDEX(ADUM(1:),'well').NE.0 ) ISRTX = 30      
        IF( INDEX(ADUM(1:),'solute').NE.0 .AND.
     &    INDEX(ADUM(1:),'inventory').NE.0 ) ISRTX = -(3*LSOLU)
        IF( INDEX(ADUM(1:),'spill').NE.0 ) ISRTX = 40      
!
!---    Bioslurping well  ---
!
        IF( ISRTX == 30 ) THEN
!
!---      Read x,y,z-coordinate location of the bioslurping tube  ---
!
          VARB = 'Bioslurping Tube X-Direction Coordinate Location'
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,SRCPX(1))
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNM = 1
          CALL RD_UNIT(UNTS,SRCPX(1),INDX)
          VARB = 'Bioslurping Tube Y-Direction Coordinate Location'
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,SRCPX(2))
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNM = 1
          CALL RD_UNIT(UNTS,SRCPX(2),INDX)
!
!---      Cylindrical coordinates with azimuthal symmetry  ---
!
          IF( (ICS.EQ.2 .OR. ICS.EQ.6) .AND. LFY.EQ.1 ) THEN
            IF( ABS(SRCPX(1))/EPSL.GT.EPSL ) THEN
              INDX = 4
              CHMSG = 'Non-Zero Bioslurping Tube X-Direction ' // 
     &          'Coordinate Location for Radially Symmetric Domain'
              RLMSG = SRCPX(1)
              CALL WRMSGP( INDX )
            ENDIF
            IF( ABS(SRCPX(2))/EPSL.GT.EPSL ) THEN
              INDX = 9
              CHMSG = 'Non-Zero Bioslurping Tube Y-Direction ' // 
     &          'Coordinate Location for Radially Symmetric Domain'
              RLMSG = SRCPX(2)
              CALL WRMSGP( INDX )
            ENDIF
          ENDIF
          VARB = 'Bioslurping Tube Z-Direction Coordinate Location'
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,SRCPX(3))
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNM = 1
          CALL RD_UNIT(UNTS,SRCPX(3),INDX)
          VARB = 'Bioslurping Well Screened Interval X-Component Length'
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,SRCPX(4))
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          VARB = 'Bioslurping Well Screened Interval Y-Component Length'
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,SRCPX(5))
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          VARB = 'Bioslurping Well Screened Interval Z-Component Length'
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,SRCPX(6))
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          VARB = 'Bioslurping Well Radius'
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,SRCPX(7))
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          VARB = 'Bioslurping Well Skin Factor'
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,SRCPX(8))
!
!---      Loop over active nodes to find node number for the 
!         bioslurping tube location ---
!
          DO 70 N = 1,LFD
            IF( IXP(N).EQ.0 ) GOTO 70
            I = ID(N)
            J = JD(N)
            K = KD(N)
!
!---        Cylindrical coordinates with azimuthal symmetry,
!           centrally located wells  ---
!
            IF( (ICS.EQ.2 .OR. ICS.EQ.6) .AND. LFY.EQ.1
     &        .AND. I.EQ.1 ) THEN
!
!---          Node height greater than EPSLX  ---
!
              IF( ABS(ZE(1,N)-ZE(5,N)).GT.EPSLX ) THEN
                DZPX1 = SRCPX(3)-ZE(1,N)
                DZPX2 = ZE(5,N)-SRCPX(3)
                IF( ABS(DZPX1).LT.EPSLX ) DZPX1 = 0.D+0
                IF( ABS(DZPX2).LT.EPSLX ) DZPX2 = 0.D+0
!
!---            Transition point within vertical limits of node  ---
!
                IF( DZPX1.GE.0.D+0 .AND. DZPX2.GE.0.D+0 ) GOTO 80
              ENDIF
            ELSE
!
!---          Check for point within hexahedron  ---
!
              CALL WITH_IN( SRCPX(1),SRCPX(2),SRCPX(3),ICWX,N )
!
!---          Opposing rotations found, point outside hexahedron  ---
!
              IF( ICWX.EQ.0 ) GOTO 70
!
!---          No opposing rotations found, point inside hexahedron
!             store and record source node  ---
!
              GOTO 80
            ENDIF
   70     CONTINUE
          INDX = 4
          CHMSG = 'Bioslurping Point Outside Active Domain'
          CALL WRMSGP( INDX )
   80     CONTINUE
          NSRX = NSRX + 1
        ELSE
!
!---      Skip over character string inputs for
!         source type options and solute sources  ---
!
          DO
            CALL CHK_INT(ISTART,ICOMMA,CHDUM,INDX)
            IF( INDX == 1 ) EXIT
            VARB = 'Source Type Option'
            CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
          END DO
!
!---      Read source domain indices  ---
!
          VARB = 'Source Domain Index: '
          ISX = ISTART
          CALL RD_INT(ISTART,ICOMMA,CHDUM,I1X)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,I2X)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,J1X)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,J2X)
          IF( ISRTX.NE.40 ) THEN
            CALL RD_INT(ISTART,ICOMMA,CHDUM,K1X)
            CALL RD_INT(ISTART,ICOMMA,CHDUM,K2X)
          ENDIF
          ICX = ISTART
!
!---      Check for ill-defined source domains  ---
!
          IF( I1X.LT.1 .OR. I1X.GT.LFX .OR. I2X.LT.1 .OR.
     &      I2X.GT.LFX .OR. I2X.LT.I1X ) THEN
            INDX = 4
            CHMSG = 'Invalid Source Domain: ' // CHDUM(ISX:ICX)
            CALL WRMSGP( INDX )
          ENDIF
          IF( J1X.LT.1 .OR. J1X.GT.LFY .OR. J2X.LT.1 .OR.
     &      J2X.GT.LFY .OR. J2X.LT.J1X ) THEN
            INDX = 4
            CHMSG = 'Invalid Source Domain: ' // CHDUM(ISX:ICX)
            CALL WRMSGP( INDX )
          ENDIF
          IF( ISRTX.NE.40 ) THEN
          IF( K1X.LT.1 .OR. K1X.GT.LFZ .OR. K2X.LT.1 .OR.
     &        K2X.GT.LFZ .OR. K2X.LT.K1X ) THEN
              INDX = 4
              CHMSG = 'Invalid Source Domain: ' // CHDUM(ISX:ICX)
              CALL WRMSGP( INDX )
            ENDIF
          ENDIF
!
!---      Define a unique source input for each node 
!         in the domain for the solute inventory source  ---
!
          IF( ISRTX.EQ.-(3*LSOLU) ) THEN
            NSRX = NSRX+(I2X-I1X+1)*(J2X-J1X+1)*(K2X-K1X+1)
          ELSE
            NSRX = NSRX+1
          ENDIF
        ENDIF
!
!---    Read number of source times  ---
!
        VARB = 'Number of Source Times: '
        CALL RD_INT(ISTART,ICOMMA,CHDUM,NSTM)
        LSTM = MAX( LSTM,NSTM )
!
!---    Solute injected with well gas  ---
!
        IF( ISRTX >= 22 .AND. ISRTX <= 27 .AND. LC.NE.0 ) THEN
          CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.NE.0 ) THEN
            VARB = 'Solute Source Type'
            CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
            IF( INDEX(ADUM(1:),'solute').NE.0 ) THEN          
              CALL RD_INPL( CHDUM )
              CALL L_CASE( CHDUM )
              ISTART = 1
              VARB = 'Number of Solutes'
              CALL RD_INT(ISTART,ICOMMA,CHDUM,NSOLSRX)
              LSOLSR = MAX( NSOLSRX,LSOLSR )
            ENDIF
          ENDIF
        ENDIF
!
!---    Skip over source variables  ---
!
        DO 100 NTM = 1,NSTM
          CALL RD_INPL( CHDUM )
  100   CONTINUE
  200 CONTINUE
      LSR = MAX( LSR,NSRX )
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_SR group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_SREL
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
!     Read SAC Release Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by WE Nichols, PNNL, 13 June 2003.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_SREL'
!
!---  Assign card string  ---
!
      CARD = 'SAC Release Card'
!
!---  Grid dimension parameters  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of SAC Release Plane Time Changes'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,LREL)
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_SREL group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_SREM
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
!     Read SAC Remediation Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by WE Nichols, PNNL, 13 June 2003.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_SREM'
!
!---  Assign card string  ---
!
      CARD = 'SAC Remediation Card'
!
!---  Grid dimension parameters  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of SAC Remediation Events'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,LREM)
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_SREM group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_TF
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
!     Read Solute/Fluid Interaction Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 26 November 2002.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE TRNSPT
      USE TABL
      USE SOURC
      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*128 ADUM
      CHARACTER*512 CHDUM
      TYPE(LIST_NODE), POINTER :: LOC_PTR,TMP_PTR
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_TF'
!
!---  Assign card string  ---
!
      CARD = 'Solute/Fluid Interactions Card'
!
!---  Read number of different solutes  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Solutes'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
      NSOLU = 0
      NULLIFY( SOLUT_PTR )
      DO 200 NL = 1, NLIN
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
        ADUM(1:) = ' '
        VARB = 'Solute Name'
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---    Empty solute name list  ---
!
        IF( .NOT.ASSOCIATED(SOLUT_PTR) ) THEN
          ALLOCATE( SOLUT_PTR,STAT=ISTAT )
          IF( ISTAT.NE.0 ) THEN
            INDX = 3
            CHMSG = 'Allocation Error: SOLUT_PTR'
            CALL WRMSGP( INDX )
          ENDIF
          SOLUT_PTR%LIST_NAME = ADUM
          NULLIFY(SOLUT_PTR%NEXT)
          NSOLU = NSOLU + 1
!
!---    Established solute name list  ---
!
        ELSE
          LOC_PTR => SOLUT_PTR
!
!---      Check for repeated solute name  ---
!
          DO
            IF( .NOT.ASSOCIATED(LOC_PTR) ) EXIT
            IF( LOC_PTR%LIST_NAME == ADUM ) GOTO 110
            LOC_PTR => LOC_PTR%NEXT
          ENDDO
!
!---      Add solute name to solute list  ---
!
          NSOLU = NSOLU + 1
          ALLOCATE( TMP_PTR,STAT=ISTAT )
          IF( ISTAT.NE.0 ) THEN
            INDX = 3
            CHMSG = 'Allocation Error: SOLUT_PTR'
            CALL WRMSGP( INDX )
          ENDIF
          TMP_PTR%LIST_NAME = ADUM
          TMP_PTR%NEXT => SOLUT_PTR
          SOLUT_PTR => TMP_PTR
        ENDIF
  110   CONTINUE
        LSOLU = MAX( LSOLU,NSOLU )
        IF( NSOLU.GT.0 ) LC = 1

  200 CONTINUE
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_TF group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_UNFGRID
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
!     Read grids with variable spacing.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 30 November 2011.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GRID
      USE FILES
      USE CONST
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 FDUM,FMDUM,UNTS
      CHARACTER*128 ADUM
      CHARACTER*512 CHDUM
      LOGICAL FCHK
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_UNFGRID'
!
!---  First coordinate direction ---
!
      ISTART = 1
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      IF( ICS.EQ.6 ) THEN
        VARB = 'Radial Node Dimension'
      ELSE
        VARB = 'X Node Dimension'
      ENDIF
      CALL RD_DPR(ISTART,ICOMMA,CHDUM,XSPC)
      IF( ICS.EQ.6 ) THEN
        VARB = 'Radial Node Dimension Units'
      ELSE
        VARB = 'X Node Dimension Units'
      ENDIF
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      I = 1
      N = ND(I,1,1)
      XE(1,N) = 0.D+0
      IF( ICS.EQ.6 ) XE(1,N) = SMALL
      XE(2,N) = XSPC
      DO 100 I = 2,LFX
        N = ND(I,1,1)
        NW = N-1
        XE(1,N) = XE(2,NW)
        XE(2,N) = XE(1,N) + XSPC
 100  CONTINUE
      IF( ABS(XE(2,N))/EPSL.LT.EPSL ) THEN
        INDX = 4
        CHMSG = 'Zero X-Direction Domain'
        CALL WRMSGP( INDX )
      ENDIF
      INDX = 0
      DO 110 I = 1,LFX
        N = ND(I,1,1)
        IUNM = 1
        CALL RD_UNIT(UNTS,XE(1,N),INDX)
        IUNM = 1
        CALL RD_UNIT(UNTS,XE(2,N),INDX)
 110  CONTINUE
      DO 140 K = 1,LFZ
      DO 140 J = 1,LFY
      DO 140 I = 1,LFX
        N = ND(I,J,K)
        NI = ND(I,1,1)
        XE(1,N) = XE(1,NI)
        XE(3,N) = XE(1,NI)
        XE(5,N) = XE(1,NI)
        XE(7,N) = XE(1,NI)
        XE(2,N) = XE(2,NI)
        XE(4,N) = XE(2,NI)
        XE(6,N) = XE(2,NI)
        XE(8,N) = XE(2,NI)
 140  CONTINUE
!
!---  Second coordinate direction  ---
!
      ISTART = 1
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      IF( ICS.EQ.6 ) THEN
        VARB = 'Azimuthal Node Dimension'
      ELSE
        VARB = 'Y Node Dimension'
      ENDIF
      CALL RD_DPR(ISTART,ICOMMA,CHDUM,YSPC)
      IF( ICS.EQ.6 ) THEN
        VARB = 'Azimuthal Node Dimension Units'
      ELSE
        VARB = 'Y Node Dimension Units'
      ENDIF
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      J = 1
      N = ND(1,J,1)
      YE(1,N) = 0.D+0
      YE(3,N) = YSPC
      DO 200 J = 2,LFY
        N = ND(1,J,1)
        NS = N-LFX
        YE(1,N) = YE(3,NS)
        YE(3,N) = YE(1,N) + YSPC
 200  CONTINUE
      IF( ABS(YE(3,N))/EPSL.LT.EPSL ) THEN
        INDX = 4
        CHMSG = 'Zero Y-Direction Domain'
        CALL WRMSGP( INDX )
      ENDIF
      INDX = 0
      DO 210 J = 1,LFY
        N = ND(1,J,1)
        IUNM = 1
        IF( ICS.EQ.6 ) IUNM = 0
        CALL RD_UNIT(UNTS,YE(1,N),INDX)
        IUNM = 1
        IF( ICS.EQ.6 ) IUNM = 0
        CALL RD_UNIT(UNTS,YE(3,N),INDX)
  210 CONTINUE
      DO 240 K = 1,LFZ
      DO 240 J = 1,LFY
      DO 240 I = 1,LFX
        N = ND(I,J,K)
        NJ = ND(1,J,1)
        YE(1,N) = YE(1,NJ)
        YE(2,N) = YE(1,NJ)
        YE(5,N) = YE(1,NJ)
        YE(6,N) = YE(1,NJ)
        YE(3,N) = YE(3,NJ)
        YE(4,N) = YE(3,NJ)
        YE(7,N) = YE(3,NJ)
        YE(8,N) = YE(3,NJ)
 240  CONTINUE
!
!---  Third coordinate direction  ---
!
      ISTART = 1
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      IF( ICS.EQ.6 ) THEN
        VARB = 'Vertical Node Dimension'
      ELSE
        VARB = 'Z Node Dimension'
      ENDIF
      CALL RD_DPR(ISTART,ICOMMA,CHDUM,ZSPC)
      IF( ICS.EQ.6 ) THEN
        VARB = 'Vertical Node Dimension Units'
      ELSE
        VARB = 'Z Node Dimension Units'
      ENDIF
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      K = 1
      N = ND(1,1,K)
      ZE(1,N) = 0.D+0
      ZE(5,N) = ZSPC
      DO 300 K = 2,LFZ
        N = ND(1,1,K)
        NB = N-LFXY
        ZE(1,N) = ZE(5,NB)
        ZE(5,N) = ZE(1,N) + ZSPC
  300 CONTINUE
      IF( ABS(ZE(5,N))/EPSL.LT.EPSL ) THEN
        INDX = 4
        CHMSG = 'Zero Z-Direction Domain'
        CALL WRMSGP( INDX )
      ENDIF
      INDX = 0
      DO 310 K = 1,LFZ
        N = ND(1,1,K)
        IUNM = 1
        CALL RD_UNIT(UNTS,ZE(1,N),INDX)
        IUNM = 1
        CALL RD_UNIT(UNTS,ZE(5,N),INDX)
  310 CONTINUE
      DO 340 K = 1,LFZ
      DO 340 J = 1,LFY
      DO 340 I = 1,LFX
        N = ND(I,J,K)
        NK = ND(1,1,K)
        ZE(1,N) = ZE(1,NK)
        ZE(2,N) = ZE(1,NK)
        ZE(3,N) = ZE(1,NK)
        ZE(4,N) = ZE(1,NK)
        ZE(5,N) = ZE(5,NK)
        ZE(6,N) = ZE(5,NK)
        ZE(7,N) = ZE(5,NK)
        ZE(8,N) = ZE(5,NK)
 340  CONTINUE
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_UNFGRID group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_UNIT( UNTS,VAR,INDX )
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
!     Unit conversions for variable VAR.
!     INDX = 0 : Convert to SI Units.
!     INDX = 1 : Convert from SI Units.
!     INDX = 2 : Convert to SI Units without a units check.
!     INDX = 3 : Convert to SI Units for output.
!     INDX = 4 : Convert from SI Units for output.
!
!     Rules for expressing units:
!       Units may only contain one divisor.
!       Components within a units character string must be separated
!       with a blank space, colon, or a divisor.
!       No spaces between component name and divisor.
!       Units raised to powers are indicated with a '^' symbol.
!       Examples:
!         'btu:in/hr ft^2 F' converts to SI units of 'w/m k'
!         'lb/hr ft' converts to SI units of 'Pa s'
!         'g/l' converts to SI units of 'kg/m^3'
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, November 1992.
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
      PARAMETER (LUNS=119)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*4 FORM1
      CHARACTER*8 CHS(LUNS),CHD
      CHARACTER*64 UNTS
      REAL*8 CF(LUNS)
      INTEGER IUM(LUNS),IUKG(LUNS),IUS(LUNS),IUK(LUNS),IUMOL(LUNS)
!
!----------------------Data Statements---------------------------------!
!
      SAVE CHS,CF,IUM,IUKG,IUS,IUK,IUMOL,FORM1
      DATA CHS /'m','kg','s','j','c','pa','w','kgmol','rad',
     &  'solid','water','napl','gas','aqueous','oil','voc','sol',
     &  'ci','pci','liq','aqu','mi','mile','nm',
     &  'ft','cm','mm','yd','in','l','gal','liter',
     &  'ml','g','lb','slug','lbm','gm','gram','mg',
     &  'min','hr','d','wk','yr',
     &  'hour','day','week','year','sec',
     &  'btu','cal','hp','dynes','dyn','darcy',
     &  'k','f','r',
     &  'psi','bar','atm','wh','psf','lbf',
     &  'deg','degree','furlong','rod','rad','radian',
     &  'cp','p','hc','1','mol','mole','lbmol',
     &  'debyes','bd','n','newton','plant','langley',
     &  'kpa','mpa','gpa','upa','kmol','kmole','joule','kj','kjoule',
     &  'a','ao','ang','angstrom',
     &  'tonne','mt','mmt','md','mdarcy','ton',
     &  'ffl','ffa','ffvh_m','ffvh_ft',
     &  'ffvh_in','ffvh_yd','ffvh_cm','st','cst',
     &  'bbl','stb','scf','mscf','mmscf','bscf','tscf'/
      DATA CF  /1.D+0,1.D+0,1.D+0,1.D+0,1.D+0,1.D+0,1.D+0,1.D+0,1.D+0,
     &  1.D+0,1.D+0,1.D+0,1.D+0,1.D+0,1.D+0,1.D+0,1.D+0,
     &  1.D+0,1.D+0,1.D+0,1.D+0,1.609344D+3,1.609344D+3,1.D-9,
     &  3.048D-1,1.D-2,1.D-3,9.144D-1,2.54D-2,1.D-3,3.7854D-3,1.D-3,
     &  1.D-6,1.D-3,4.5359D-1,1.4594D+1,4.5359D-1,1.D-3,1.D-3,1.D-6,
     &  6.D+1,3.6D+3,8.64D+4,6.048D+5,3.15576D+7,
     &  3.6D+3,8.64D+4,6.048D+5,3.15576D+7,1.D+0,
     &  1.0544D+3,4.184D+0,7.457D+2,1.D-5,1.D-5,0.9869D-12,
     &  1.D+0,5.555556D-1,5.555556D-1,
     &  6.8948D+3,1.D+5,1.01325D+5,9.7935332D+03,4.7880556D+1,4.4482D+0,
     &  1.745329252D-2,1.745329252D-2,2.01168D+2,5.0292D+0,1.D+0,1.D+0,
     &  1.D-3,1.D-1,1.03910295199455D-07,1.D+0,1.D-3,1.D-3,4.5359D-1,
     &  1.D+0,1.D+0,1.D+0,1.D+0,1.D+0,4.186D+4,
     &  1.D+3,1.D+6,1.D+9,1.D-6,1.D+0,1.D+0,1.D+0,1.D+3,1.D+3,
     &  1.D-10,1.D-10,1.D-10,1.D-10,
     &  1.D+3,1.D+3,1.D+9,0.9869D-15,0.9869D-15,9.0718494D+2,
     &  109.728D+0,5351.215104D+0,5351.215104D+0,1631.050363699D+0,
     &  135.920863642D+0,4893.151091098D+0,53.51215104D+0,1.D-4,1.D-6,
     &  0.158987295D+0,0.158987295D+0,2.83168D-2,2.83168D+1,2.83168D+4,
     &  2.83168D+7,2.83168D+10/
      DATA IUM /1,0,0,2,0,-1,2,0,0,
     &  0,0,0,0,0,0,0,0,
     &  0,0,0,0,1,1,1,
     &  1,1,1,1,1,3,3,3,
     &  3,0,0,0,0,0,0,0,
     &  0,0,0,0,0,
     &  0,0,0,0,0,
     &  2,2,2,1,1,2,
     &  0,0,0,
     &  -1,-1,-1,-2,-1,1,
     &  0,0,1,1,0,0,
     &  -1,-1,0,0,0,0,0,
     &  0,-3,1,1,0,0,
     &  -1,-1,-1,-1,0,0,2,2,2,
     &  1,1,1,1,
     &  0,0,0,2,2,0,
     &  1,2,3,3,
     &  3,3,3,2,2,
     &  3,3,3,3,3,3,3/
      DATA IUKG /0,1,0,1,0,1,1,0,0,
     &  0,0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,0,
     &  0,1,1,1,1,1,1,1,
     &  0,0,0,0,0,
     &  0,0,0,0,0,
     &  1,1,1,1,1,0,
     &  0,0,0,
     &  1,1,1,1,1,1,
     &  0,0,0,0,0,0,
     &  1,1,0,0,0,0,0,
     &  0,1,1,1,0,1,
     &  1,1,1,1,0,0,1,1,1,
     &  0,0,0,0,
     &  1,1,1,0,0,1,
     &  0,0,0,0,
     &  0,0,0,0,0,
     &  0,0,0,0,0,0,0/
      DATA IUS /0,0,1,-2,0,-2,-3,0,0,
     &  0,0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,0,
     &  1,1,1,1,1,
     &  1,1,1,1,1,
     &  -2,-2,-3,-2,-2,0,
     &  0,0,0,
     &  -2,-2,-2,-2,-2,-2,
     &  0,0,0,0,0,0,
     &  -1,-1,0,0,0,0,0,
     &  0,0,-2,-2,0,-2,
     &  -2,-2,-2,-2,0,0,-2,-2,-2,
     &  0,0,0,0,
     &  0,0,0,0,0,0,
     &  0,0,0,0,
     &  0,0,0,-1,-1,
     &  0,0,0,0,0,0,0/
      DATA IUK /0,0,0,0,1,0,0,0,0,
     &  0,0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,0,
     &  0,0,0,0,0,
     &  0,0,0,0,0,
     &  0,0,0,0,0,0,
     &  1,1,1,
     &  0,0,0,0,0,0,
     &  0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,0,0,
     &  0,0,0,0,
     &  0,0,0,0,0,0,
     &  0,0,0,0,
     &  0,0,0,0,0,
     &  0,0,0,0,0,0,0/
      DATA IUMOL /0,0,0,0,0,0,0,1,0,
     &  0,0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,0,
     &  0,0,0,0,0,
     &  0,0,0,0,0,
     &  0,0,0,0,0,0,
     &  0,0,0,
     &  0,0,0,0,0,0,
     &  0,0,0,0,0,0,
     &  0,0,0,0,1,1,1,
     &  0,0,0,0,0,0,
     &  0,0,0,0,1,1,0,0,0,
     &  0,0,0,0,
     &  0,0,0,0,0,0,
     &  0,0,0,0,
     &  0,0,0,0,0,
     &  0,0,0,0,0,0,0/     
      DATA FORM1 /'(I )'/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_UNIT'
      NCH = INDEX( UNTS,'  ' ) - 1
      IF( UNTS(1:NCH).EQ.'null' .OR. UNTS(1:NCH).EQ.'none' ) THEN
        IUNM = 0
        IUNKG = 0
        IUNS = 0
        IUNK = 0
        IUNMOL = 0
        ISUB_LOG = ISUB_LOG-1
        RETURN
      ENDIF
!
!---  Intialize primary unit indices  ---
!
      IUMX = 0
      IUKGX = 0
      IUSX = 0
      IUKX = 0
      IUMOLX = 0
!
!---  Temperature units  --
!
      IF( UNTS(1:NCH) .EQ. 'c' ) THEN
        IUKX = 1
        GOTO 400
      ELSEIF( UNTS(1:NCH) .EQ. 'k' ) THEN
        IF( INDX.EQ.0 .OR. INDX.EQ.2 .OR. INDX.EQ.3 ) THEN
          VAR = VAR - 2.7315D+2
        ELSE
          VAR = VAR + 2.7315D+2
        ENDIF
        IUKX = 1
        GOTO 400
      ELSEIF( UNTS(1:NCH) .EQ. 'f' ) THEN
        IF( INDX.EQ.0 .OR. INDX.EQ.2 .OR. INDX.EQ.3 ) THEN
          VAR = (VAR-3.2D+1)/1.8D+0
        ELSE
          VAR = VAR*1.8D+0 + 3.2D+1
        ENDIF
        IUKX = 1
        GOTO 400
      ELSEIF( UNTS(1:NCH) .EQ. 'r' ) THEN
        IF( INDX.EQ.0 .OR. INDX.EQ.2 .OR. INDX.EQ.3 ) THEN
          VAR = (VAR-4.92D+2)/1.8D+0
        ELSE
          VAR = VAR*1.8D+0 + 4.92D+2
        ENDIF
        IUKX = 1
        GOTO 400
      ENDIF
!
!---  Decompose the units into components and convert individual
!     components  ---
!
      ISX = 1
      IDV = INDEX( UNTS(1:),'/' )-1
      IEX = INDEX( UNTS(1:),'  ' )-1
!
!---  Units without a divisor  ---
!
      IF( IDV .EQ. -1 ) THEN
  100 CONTINUE
        ISP = INDEX( UNTS(ISX:),' ' )+ISX-2
        ICO = INDEX( UNTS(ISX:),':' )+ISX-2
        IF( ICO .LT. ISX ) ICO = IEX
        IB = MIN( IEX,ISP,ICO )
        CHD = UNTS(ISX:IB)
        IC = INDEX( CHD(1:),'^' )
        IF( IC .EQ. 0 ) THEN
          IP = 1
        ELSE
          I1 = IC+1
          I2 = IB-ISX+1
          I3 = I2-I1+1
          WRITE( FORM1(3:3),'(I1)' ) I3
          READ(CHD(I1:I2),FORM1) IP
          I2 = IC-1
          CHD = CHD(1:I2)
        ENDIF
        DO 110 N = 1,LUNS
          IF( CHS(N) .EQ. CHD ) THEN
            IUMX = IUMX + IUM(N)*IP
            IUKGX = IUKGX + IUKG(N)*IP
            IUSX = IUSX + IUS(N)*IP
            IUKX = IUKX + IUK(N)*IP
            IUMOLX = IUMOLX + IUMOL(N)*IP
            IF( INDX.EQ.0 .OR. INDX.EQ.2 .OR. INDX.EQ.3 ) THEN
              VAR = VAR*(CF(N)**IP)
            ELSE
              VAR = VAR/(CF(N)**IP)
            ENDIF
            GOTO 120
          ENDIF
  110   CONTINUE
        INDX = 4
        CHMSG = 'Unrecognized Units: '//VARB(1:IVR)//': '//UNTS
        CALL WRMSGP( INDX )
  120   CONTINUE
        IF( IB .LT. IEX ) THEN
          ISX = IB+2
          GOTO 100
        ENDIF
!
!---  Units with a divisor  ---
!
      ELSE
!
!---  Components before the divisor  ---
!
  200 CONTINUE
        ISP = INDEX( UNTS(ISX:),' ' )+ISX-2
        ICO = INDEX( UNTS(ISX:),':' )+ISX-2
        IF( ICO .LT. ISX ) ICO = IEX
        IB = MIN( IDV,ISP,ICO )
        CHD = UNTS(ISX:IB)
        IC = INDEX( CHD(1:),'^' )
        IF( IC .EQ. 0 ) THEN
          IP = 1
        ELSE
          I1 = IC+1
          I2 = IB-ISX+1
          I3 = I2-I1+1
          WRITE( FORM1(3:3),'(I1)' ) I3
          READ(CHD(I1:I2),FORM1) IP
          I2 = IC-1
          CHD = CHD(1:I2)
        ENDIF
        DO 210 N = 1,LUNS
          IF( CHS(N) .EQ. CHD ) THEN
            IUMX = IUMX + IUM(N)*IP
            IUKGX = IUKGX + IUKG(N)*IP
            IUSX = IUSX + IUS(N)*IP
            IUKX = IUKX + IUK(N)*IP
            IUMOLX = IUMOLX + IUMOL(N)*IP
            IF( INDX.EQ.0 .OR. INDX.EQ.2 .OR. INDX.EQ.3 ) THEN
              VAR = VAR*(CF(N)**IP)
            ELSE
              VAR = VAR/(CF(N)**IP)
            ENDIF
            GOTO 220
          ENDIF
  210   CONTINUE
        INDX = 4
        CHMSG = 'Unrecognized Units: '//VARB(1:IVR)//': '//UNTS
        CALL WRMSGP( INDX )
  220   CONTINUE
        IF( IB .LT. IDV ) THEN
          ISX = IB+2
          GOTO 200
        ELSE
          ISX = IB+2
          GOTO 300
        ENDIF
!
!---  Components after the divisor  ---
!
  300   CONTINUE
        ISP = INDEX( UNTS(ISX:),' ' )+ISX-2
        ICO = INDEX( UNTS(ISX:),':' )+ISX-2
        IF( ICO .LT. ISX ) ICO = IEX
        IB = MIN( IEX,ISP,ICO )
        CHD = UNTS(ISX:IB)
        IC = INDEX( CHD(1:),'^' )
        IF( IC .EQ. 0 ) THEN
          IP = 1
        ELSE
          I1 = IC+1
          I2 = IB-ISX+1
          I3 = I2-I1+1
          WRITE( FORM1(3:3),'(I1)' ) I3
          READ(CHD(I1:I2),FORM1) IP
          I2 = IC-1
          CHD = CHD(1:I2)
        ENDIF
        DO 310 N = 1,LUNS
          IF( CHS(N) .EQ. CHD ) THEN
            IUMX = IUMX - IUM(N)*IP
            IUKGX = IUKGX - IUKG(N)*IP
            IUSX = IUSX - IUS(N)*IP
            IUKX = IUKX - IUK(N)*IP
            IUMOLX = IUMOLX - IUMOL(N)*IP
            IF( INDX.EQ.0 .OR. INDX.EQ.2 .OR. INDX.EQ.3 ) THEN
              VAR = VAR/(CF(N)**IP)
            ELSE
              VAR = VAR*(CF(N)**IP)
            ENDIF
            GOTO 320
          ENDIF
  310   CONTINUE
        INDX = 4
        CHMSG = 'Unrecognized Units: '//VARB(1:IVR)//': '//UNTS
        CALL WRMSGP( INDX )
  320   CONTINUE
        IF( IB .LT. IEX ) THEN
          ISX = IB+2
          GOTO 300
        ENDIF
      ENDIF
!
!---  Units Conversion Check  ---
!
  400 CONTINUE
      IF( INDX.EQ.2 ) THEN
        IUNM = 0
        IUNKG = 0
        IUNS = 0
        IUNK = 0
        IUNMOL = 0
      ELSEIF( IUMX.NE.IUNM .OR. IUKGX.NE.IUNKG .OR. IUSX.NE.IUNS .OR.
     &    IUKX.NE.IUNK .OR. IUMOLX.NE.IUNMOL ) THEN
        INDX = 4
        CHMSG = 'Incompatible Units: '//VARB(1:IVR)//': '//UNTS
        CALL WRMSGP( INDX )
      ELSE
        IUNM = 0
        IUNKG = 0
        IUNS = 0
        IUNK = 0
        IUNMOL = 0
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_UNIT group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_VBLGRID
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
!     Read grids with variable spacing.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 30 November 2011.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GRID
      USE FILES
      USE CONST
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 FDUM,FMDUM,UNTS
      CHARACTER*128 ADUM
      CHARACTER*512 CHDUM
      LOGICAL FCHK
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_VBLGRID'
!
!---  First coordinate direction ---
!
      IC = 0
  100 CONTINUE
      ISTART = 1
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      IR = LFX+1-IC
      DO 120 I = 1,IR
        ICMX = INDEX( CHDUM(ISTART:), ',' ) + ISTART - 1
        IF( ICMX.EQ.ISTART-1 ) GOTO 100
        IATX = INDEX( CHDUM(ISTART:), '@' ) + ISTART - 1
        IF( IATX.LT.ISTART .OR. IATX.GT.ICMX ) THEN
          IC = IC + 1
          N = ND(MIN(IC,LFX),1,1)
          VARB = 'X Dimension'
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VARX)
          VARB = 'X Dimension Units'
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( IC.EQ.LFX+1 ) THEN
            XE(2,N) = VARX
            INDX = 0
            IUNM = 1
            CALL RD_UNIT(UNTS,XE(2,N),INDX)
            GOTO 130
          ELSE
            XE(1,N) = VARX
          ENDIF
          INDX = 0
          IUNM = 1
          CALL RD_UNIT(UNTS,XE(1,N),INDX)
          IF( IC.GT.1 ) THEN
            NW = N-1
            XE(2,NW) = XE(1,N)
          ENDIF
        ELSE
          CHDUM(IATX:IATX) = ','
          VARB = 'Count Integer'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,IATX )
          VARB = 'X Dimension'
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,DXVAR )
          VARB = 'X Dimension Units'
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNM = 1
          CALL RD_UNIT(UNTS,DXVAR,INDX)
          DO 110 II = 1,IATX
            IC = IC + 1
            N = ND(MIN(IC,LFX),1,1)
            IF( IC.EQ.1 ) THEN
              XE(1,N) = 0.D+0
              XVAR = XE(1,N)
            ELSEIF( IC.EQ.LFX+1 ) THEN
              XE(2,N) = XE(1,N) + DXVAR
              XVAR = XE(2,N)
            ELSE
              NW = N-1
              XE(1,N) = XE(1,NW) + DXVAR
              XVAR = XE(1,N)
            ENDIF
            INDX = 1
            IUNM = 1
            CALL RD_UNIT(UNTS,XVAR,INDX )
            IF( IC.EQ.LFX+1 ) GOTO 130
            IF( IC.GT.1 ) THEN
              NW = N-1
              XE(2,NW) = XE(1,N)
            ENDIF
  110     CONTINUE
        ENDIF
  120 CONTINUE
  130 CONTINUE
      DO 140 K = 1,LFZ
      DO 140 J = 1,LFY
      DO 140 I = 1,LFX
        N = ND(I,J,K)
        NI = ND(I,1,1)
        XE(1,N) = XE(1,NI)
        XE(3,N) = XE(1,NI)
        XE(5,N) = XE(1,NI)
        XE(7,N) = XE(1,NI)
        XE(2,N) = XE(2,NI)
        XE(4,N) = XE(2,NI)
        XE(6,N) = XE(2,NI)
        XE(8,N) = XE(2,NI)
 140  CONTINUE
!
!---  Second coordinate direction  ---
!
      JC = 0
      JW = 0
  200 CONTINUE
      ISTART = 1
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      JR = LFY+1-JC
      DO 220 J = 1,JR
        JCM = INDEX( CHDUM(ISTART:), ',' ) + ISTART - 1
        IF( JCM.EQ.ISTART-1 ) GOTO 200
        JAT = INDEX( CHDUM(ISTART:), '@' ) + ISTART - 1
        IF( JAT.LT.ISTART .OR. JAT.GT.JCM ) THEN
          JC = JC + 1
          N = ND(1,MIN(JC,LFY),1)
          VARB = 'Y Dimension'
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VARX)
          VARB = 'Y Dimension Units'
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( JC.EQ.LFY+1 ) THEN
            YE(3,N) = VARX
            INDX = 0
            IUNM = 1
            IF( ICS.EQ.2 ) IUNM = 0
            CALL RD_UNIT(UNTS,YE(3,N),INDX)
            GOTO 230
          ELSE
            YE(1,N) = VARX
          ENDIF
          INDX = 0
          IUNM = 1
          IF( ICS.EQ.2 ) IUNM = 0
          CALL RD_UNIT(UNTS,YE(1,N),INDX)
          IF( JC.GT.1 ) THEN
            NS = N-LFX
            YE(3,NS) = YE(1,N)
          ENDIF
        ELSE
          CHDUM(JAT:JAT) = ','
          VARB = 'Count Integer'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,JAT )
          VARB = 'Y Dimension'
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,DYVAR)
          VARB = 'Y Dimension Units'
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNM = 1
          IF( ICS.EQ.2 ) IUNM = 0
          CALL RD_UNIT(UNTS,DYVAR,INDX)
          DO 210 JJ = 1,JAT
            JC = JC + 1
            N = ND(1,MIN(JC,LFY),1)
            IF( JC.EQ.1 ) THEN
              YE(1,N) = 0.D+0
              YVAR = YE(1,N)
            ELSEIF( JC.EQ.LFY+1 ) THEN
              YE(3,N) = YE(1,N) + DYVAR
              YVAR = YE(3,N)
            ELSE
              NS = N-LFX
              YE(1,N) = YE(1,NS) + DYVAR
              YVAR = YE(1,N)
            ENDIF
            INDX = 1
            IUNM = 1
            IF( ICS.EQ.2 ) IUNM = 0
            CALL RD_UNIT(UNTS,YVAR,INDX )
            IF( JC.EQ.LFY+1 ) GOTO 230
            IF( JC.GT.1 ) THEN
              NS = N-LFX
              YE(3,NS) = YE(1,N)
            ENDIF
  210     CONTINUE
        ENDIF
  220 CONTINUE
  230 CONTINUE
      DO 240 K = 1,LFZ
      DO 240 J = 1,LFY
      DO 240 I = 1,LFX
        N = ND(I,J,K)
        NJ = ND(1,J,1)
        YE(1,N) = YE(1,NJ)
        YE(2,N) = YE(1,NJ)
        YE(5,N) = YE(1,NJ)
        YE(6,N) = YE(1,NJ)
        YE(3,N) = YE(3,NJ)
        YE(4,N) = YE(3,NJ)
        YE(7,N) = YE(3,NJ)
        YE(8,N) = YE(3,NJ)
 240  CONTINUE
!
!---  Third coordinate direction  ---
!
      KC = 0
  300 CONTINUE
      ISTART = 1
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      KR = LFZ+1-KC
      DO 320 K = 1,KR
        KCM = INDEX( CHDUM(ISTART:), ',' ) + ISTART - 1
        IF( KCM.EQ.ISTART-1 ) GOTO 300
        KAT = INDEX( CHDUM(ISTART:), '@' ) + ISTART - 1
        IF( KAT.LT.ISTART .OR. KAT.GT.KCM ) THEN
          KC = KC + 1
          N = ND(1,1,MIN(KC,LFZ))
          VARB = 'Z Dimension'
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VARX)
          VARB = 'Z Dimension Units'
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( KC.EQ.LFZ+1 ) THEN
            ZE(5,N) = VARX
            INDX = 0
            IUNM = 1
            CALL RD_UNIT(UNTS,ZE(5,N),INDX)
            GOTO 330
          ELSE
            ZE(1,N) = VARX
          ENDIF
          INDX = 0
          IUNM = 1
          CALL RD_UNIT(UNTS,ZE(1,N),INDX)
          IF( KC.GT.1 ) THEN
            NB = N-LFXY
            ZE(5,NB) = ZE(1,N)
          ENDIF
        ELSE
          CHDUM(KAT:KAT) = ','
          VARB = 'Count Integer'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,KAT )
          VARB = 'Z Dimension'
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,DZVAR)
          VARB = 'Z Dimension Units'
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNM = 1
          CALL RD_UNIT(UNTS,DZVAR,INDX)
          DO 310 KK = 1,KAT
            KC = KC + 1
            N = ND(1,1,MIN(KC,LFZ))
            IF( KC.EQ.1 ) THEN
              ZE(1,N) = 0.D+0
              ZVAR = ZE(1,N)
            ELSEIF( KC.EQ.LFZ+1 ) THEN
              ZE(5,N) = ZE(1,N) + DZVAR
              ZVAR = ZE(5,N)
            ELSE
              NB = N-LFXY
              ZE(1,N) = ZE(1,NB) + DZVAR
              ZVAR = ZE(1,N)
            ENDIF
            INDX = 1
            IUNM = 1
            CALL RD_UNIT(UNTS,ZVAR,INDX )
            IF( KC.EQ.LFZ+1 ) GOTO 330
            IF( KC.GT.1 ) THEN
              NB = N-LFXY
              ZE(5,NB) = ZE(1,N)
            ENDIF
  310     CONTINUE
        ENDIF
  320 CONTINUE
  330 CONTINUE
      DO 340 K = 1,LFZ
      DO 340 J = 1,LFY
      DO 340 I = 1,LFX
        N = ND(I,J,K)
        NK = ND(1,1,K)
        ZE(1,N) = ZE(1,NK)
        ZE(2,N) = ZE(1,NK)
        ZE(3,N) = ZE(1,NK)
        ZE(4,N) = ZE(1,NK)
        ZE(5,N) = ZE(5,NK)
        ZE(6,N) = ZE(5,NK)
        ZE(7,N) = ZE(5,NK)
        ZE(8,N) = ZE(5,NK)
 340  CONTINUE
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_VBLGRID group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_WELL4
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
!     Read SAC Remediation Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by WE Nichols, PNNL, 13 June 2003.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RD_WELL4'
!
!---  Assign card string  ---
!
      CARD = 'Well Card'
      LWELL = 1
!
!---  Minimum band width parameter according
!     to solver type; parameter LMNP will be reset
!     for coupled wells  ---
!

      LMNP = LMNP*(1+LWELL)




!
!---  Read number of wells  ---
!
      CALL RD_INPL( CHDUM )
      CALL L_CASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Wells'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NWLS)
      LNW = MAX( LNW,NWLS )
      DO 1000 NWL = 1,NWLS
!
!---    Read well type  ---
!
        CALL RD_INPL( CHDUM )
!
!---    Read well domain  ---
!
        CALL RD_INPL( CHDUM )
        CALL L_CASE( CHDUM )
        ISTART = 1
        VARB = 'Well I Index'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,I)
        VARB = 'Well J Index'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,J)
        VARB = 'Well Lower K Index'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,K1X)
        VARB = 'Well Upper K Index'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,K2X)
!
!---    Read number of well screen intervals  ---
!
        CALL RD_INT(ISTART,ICOMMA,CHDUM,NWSI)
        LNWS = MAX( LNWS,NWSI )
!
!---    Read well screen intervals  ---
!
        DO 110 NWS = 1,NWSI
          VARB = 'Screen Lower K Index'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,K1X)
          VARB = 'Screen Upper K Index'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,K2X)
  110   CONTINUE
!
!---    Read number of well times  ---
!
        VARB = 'Number of Well Times'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,IWMX)
        LNWT = MAX( LNWT,ABS(IWMX) )
 1000 CONTINUE
!
!---  Reset subroutine name  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RD_WELL4 group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE TRG_AREA( X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,AREA )
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
!     TRGAREA computes the area of a triangle in 3D.
!
!
!     Discussion:
!
!     This routine uses the fact that the norm of the cross product 
!     vector is the area of the parallelogram they form.  The triangle 
!     they form has half that area.
!
!     Reference:
!
!     Adrian Bowyer and John Woodwark,
!     A Programmer's Geometry,
!     Butterworths, 1983.
!
!     Author:
!
!     John Burkardt
!
!     Parameters:
!
!     Input, real X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, 
!     the triangle vertices.
!
!     Output, real AREA, the area of the triangle.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 March 2006.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
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
      REAL*8 AREA,ENORM_3D,NORM
      REAL*8 AX(3),BX(3),CX(3)
      REAL*8 X1,Y1,Z1
      REAL*8 X2,Y2,Z2
      REAL*8 X3,Y3,Z3
      REAL*8 X4,Y4,Z4
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/TRG_AREA'
      AX(1) = X2-X1
      AX(2) = Y2-Y1
      AX(3) = Z2-Z1
      BX(1) = X3-X1
      BX(2) = Y3-Y1
      BX(3) = Z3-Z1
      CALL V_CROSSP( AX,BX,CX )
      NORM = SQRT( CX(1)*CX(1) + CX(2)*CX(2) + CX(3)*CX(3) )
      AREA = 5.D-1*NORM
      ISUB_LOG = ISUB_LOG-1
!
!---  End of TRG_AREA group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE V_CROSSP( AX,BX,CX )
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
!     Vector cross product.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 30 March 2011.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 AX(3),BX(3),CX(3)
!
!----------------------Executable Lines--------------------------------!
!
      CX(1) = AX(2)*BX(3) - AX(3)*BX(2)
      CX(2) = AX(3)*BX(1) - AX(1)*BX(3)
      CX(3) = AX(1)*BX(2) - AX(2)*BX(1)
!
!---  End of V_CROSSP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      FUNCTION V_DOTP( AX,BX )
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
!     Vector dot product.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 30 March 2011.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 AX(3),BX(3)
!
!----------------------Executable Lines--------------------------------!
!
      V_DOTP = AX(1)*BX(1) + AX(2)*BX(2) + AX(3)*BX(3)
!
!---  End of V_DOTP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WITH_IN( XX,YX,ZX,ICWX,NX )
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
!     Determine whether an x,y,z coordinate location is within
!     the bounds of a node.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 28 March 2012.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GRID
      USE CONST
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 XPX(5),YPX(5),ZPX(5)
      REAL*8 PAX(3),PBX(3),PCX(3),PBCX(3)
      INTEGER MSX(4,6)
      INTEGER N1X(4),N2X(4)
!
!----------------------Data Statements---------------------------------!
!
      SAVE MSX,N1X,N2X
      DATA MSX / 1,2,4,3,1,5,6,2,1,3,7,5,2,6,8,4,3,4,8,7,5,7,8,6 /
      DATA N1X / 2,3,4,1 /
      DATA N2X / 1,2,3,4 /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WITH_IN'
!
!---  Loop over node surfaces,
!     (bottom,south,west,east,north,top)  ---
!
      ICWX = 0
      DO 180 NS = 1,6
!
!---    Define the five surface points, four corners
!       and one centroid---
!
        DO 110 NP = 1,4
          MX = MSX(NP,NS)
!
!---      Cylindrical coordinates  ---
!
          IF( ICS.EQ.2 .OR. ICS.EQ.6 ) THEN
            XPX(NP) = XE(MX,NX)*COS(YE(MX,NX))
            YPX(NP) = XE(MX,NX)*SIN(YE(MX,NX))
            ZPX(NP) = ZE(MX,NX)
          ELSE
            XPX(NP) = XE(MX,NX)
            YPX(NP) = YE(MX,NX)
            ZPX(NP) = ZE(MX,NX)
          ENDIF
  110   CONTINUE
        NP = 4
        CALL PG_CNTRD( NP,XPX(1),YPX(1),ZPX(1),
     &    XPX(5),YPX(5),ZPX(5) )
!
!---    Loop over the four triangular planes on the 
!       surface face  ---
!
        DO 130 NT = 1,4
!
!---      Built vectors between transition point
!         and triangular plane points  ---
!
          PAX(1) = XPX(5)-XX
          PAX(2) = YPX(5)-YX
          PAX(3) = ZPX(5)-ZX
          PBX(1) = XPX(N1X(NT))-XX
          PBX(2) = YPX(N1X(NT))-YX
          PBX(3) = ZPX(N1X(NT))-ZX
          PCX(1) = XPX(N2X(NT))-XX
          PCX(2) = YPX(N2X(NT))-YX
          PCX(3) = ZPX(N2X(NT))-ZX
          CALL V_CROSSP( PBX,PCX,PBCX )
          SX = V_DOTP( PAX,PBCX )
!
!---      Clockwise rotation  ---
!
          IF( SX.GT.EPSL ) THEN
!
!---        Opposing rotations found, point outside hexahedron  ---
!
            IF( ICWX.EQ.-1 ) THEN
              ICWX = 0
              GOTO 200
!
!---        Similar rotations found, continue searching  ---
!
            ELSE
              ICWX = 1
            ENDIF
!
!---      Counterclockwise rotation  ---
!
          ELSEIF( SX.LT.-EPSL ) THEN
!
!---        Opposing rotations found, point outside hexahedron  ---
!
            IF( ICWX.EQ.1 ) THEN
              ICWX = 0
              GOTO 200
!
!---        Similar rotations found, continue searching  ---
!
            ELSE
              ICWX = -1
            ENDIF
          ENDIF
  130   CONTINUE
  180 CONTINUE
      ICWX = ABS( ICWX )
  200 CONTINUE            
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WITH_IN group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WR_FPF
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
!     Write formatted "parameters" file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNNL, 29 September 2002.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
      USE REACT
      USE PTZR
      USE PTZRCOEF
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*3 FORM0
      CHARACTER*8 FORM1
      CHARACTER*13 FORM2
      CHARACTER*18 FORM3
      CHARACTER*23 FORM4
      CHARACTER*28 FORM5
      CHARACTER*33 FORM6
      CHARACTER*38 FORM7
      CHARACTER*43 FORM8
      CHARACTER*48 FORM9
!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM0,FORM1,FORM2,FORM3,FORM4,FORM5,FORM6,FORM7,FORM8,FORM9
      DATA FORM0 /'(A)'/
      DATA FORM1 /'(A,I9,A)'/
      DATA FORM2 /'(A,I9,A,I9,A)'/
      DATA FORM3 /'(A,I9,A,I9,A,I9,A)'/
      DATA FORM4 /'(A,I9,A,I9,A,I9,A,I9,A)'/
      DATA FORM5 /'(A,I9,A,I9,A,I9,A,I9,A,I9,A)'/
      DATA FORM6 /'(A,I9,A,I9,A,I9,A,I9,A,I9,A,I9,A)'/
      DATA FORM7 /'(A,I9,A,I9,A,I9,A,I9,A,I9,A,I9,A,I9,A)'/
      DATA FORM8 /'(A,I9,A,I9,A,I9,A,I9,A,I9,A,I9,A,I9,A,I9,A)'/
      DATA FORM9 /'(A,I9,A,I9,A,I9,A,I9,A,I9,A,I9,A,I9,A,I9,A,I9,A)'/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WR_FPF'
!
!---  Write banner comment lines  ---
!
      WRITE(IPF,'(A)') '!-------------------------Disclaimer--------' //
     &  '---------------------------!'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!     This material was prepared as an ' //
     &  'account of work sponsored by'
      WRITE(IPF,'(A)') '!     an agency of the United States ' //
     &  'Government.  Neither the'
      WRITE(IPF,'(A)') '!     United States Government nor the ' //
     &  'United States Department of'
      WRITE(IPF,'(A)') '!     Energy, nor Battelle, nor any of ' //
     &  'their employees, makes any'
      WRITE(IPF,'(A)') '!     warranty, express or implied, or ' //
     &  'assumes any legal liability or'
      WRITE(IPF,'(A)') '!     responsibility for the accuracy, ' //
     &  'completeness, or usefulness'
      WRITE(IPF,'(A)') '!     of any information, apparatus, ' //
     &  'product, software or process'
      WRITE(IPF,'(A)') '!     disclosed, or represents that its ' //
     &  'use would not infringe'
      WRITE(IPF,'(A)') '!     privately owned rights.'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!----------------------Acknowledgement-----' //
     &  '----------------------------!'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!     This software and its documentation ' //
     &  'were produced with Government'
      WRITE(IPF,'(A)') '!     support under Contract Number ' //
     &  'DE-AC06-76RLO-1830 awarded by the'
      WRITE(IPF,'(A)') '!     United Department of Energy. The ' //
     &  'Government retains a paid-up'
      WRITE(IPF,'(A)') '!     non-exclusive, irrevocable worldwide ' //
     &  'license to reproduce,'
      WRITE(IPF,'(A)') '!     prepare derivative works, perform ' //
     &  'publicly and display publicly'
      WRITE(IPF,'(A)') '!     by or for the Government, including ' //
     &  'the right to distribute to'
      WRITE(IPF,'(A)') '!     other Government contractors.'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---------------------Copyright Notices-----' //
     &  '---------------------------!'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!            Copyright Battelle Memorial ' //
     &  'Institute, 1996'
      WRITE(IPF,'(A)') '!                    All Rights Reserved.'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!----------------------Description----------' //
     &  '---------------------------!'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!     STOMP parameters file.'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!----------------------Authors--------------' //
     &  '---------------------------!'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!     Written by the program STEP.'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---------------Parameter Statements--------' //
     &  '---------------------------!'
      WRITE(IPF,'(A)') '!'
!
!---  Write parameter LNOTES,LEPD  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Number of lines of simulation notes, '
     &  // 'LNOTES'
      WRITE(IPF,'(A)') '!     Number of execution periods, LEPD  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM2(5:5),'(I1)') I_COUNT(LNOTES)
      WRITE(FORM2(10:10),'(I1)') I_COUNT(LEPD)
      WRITE(IPF,FORM2) '      PARAMETER(LNOTES=',LNOTES,', LEPD=',
     &  LEPD,')'
!
!---  Write parameters: LFX,LFY,LFZ,LAN,LAD,LMNP  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Number of cells in the x or r ' //
     &  'coordinate direction, LFX'
      WRITE(IPF,'(A)') '!     Number of cells in the y or ' //
     &  'theta coordinate direction, LFY'
      WRITE(IPF,'(A)') '!     Number of cells in the z coordinate ' //
     &  'direction, LFZ'
      WRITE(IPF,'(A)') '!     Number of active cells, LAN'
      WRITE(IPF,'(A)') '!     Number of active dimensions, LAD'
      WRITE(IPF,'(A)') '!     Minimum of (LFXY*(1+LWELL), '
      WRITE(IPF,'(A)') '!                 LFX*LFZ*(1+LWELL), '
      WRITE(IPF,'(A)') '!                 LFYZ*(1+LWELL)), ' //
     &  'LMNP  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM3(5:5),'(I1)') I_COUNT(LFX)
      WRITE(FORM3(10:10),'(I1)') I_COUNT(LFY)
      WRITE(FORM3(15:15),'(I1)') I_COUNT(LFZ)
      WRITE(IPF,FORM3) '      PARAMETER(LFX=',LFX,', LFY=',
     &  LFY,', LFZ=',LFZ,')'
      WRITE(FORM3(5:5),'(I1)') I_COUNT(LAN)
      WRITE(FORM3(10:10),'(I1)') I_COUNT(LAD)
      WRITE(FORM3(15:15),'(I1)') I_COUNT(LMNP)
      WRITE(IPF,FORM3) '      PARAMETER(LAN=',LAN,', LAD=',
     &  LAD,', LMNP=',LMNP,')'
!
!---  Write parameters: LT, LL, LG, LN, LC, LFW, LS, LD, LPC,
!       LALC, LWELL, LDCO2  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Energy equation switch (0 = off, ' //
     &  '1 = on), LT'
      WRITE(IPF,'(A)') '!     Water mass equation switch (0 = off, ' //
     &  '1 = on), LL'
      WRITE(IPF,'(A)') '!     Air mass equation switch (0 = off,' //
     &  ' 1 = on), LG'
      WRITE(IPF,'(A)') '!     Oil mass equation switch (0 = off, ' //
     &  '1 = on), LN'
      WRITE(IPF,'(A)') '!     Solute transport equation switch ' //
     &  '(0 = off, 1 = on), LC'
      WRITE(IPF,'(A)') '!     Freezing conditions switch (0 = off, ' //
     &  '1 = on), LFW'
      WRITE(IPF,'(A)') '!     Dissolved salt transport equation ' //
     &  'switch (0 = off, 1 = on), LS'
      WRITE(IPF,'(A)') '!     Dissolved oil transport equation ' //
     &  'switch (0 = off, 1 = on), LD  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM8(5:5),'(I1)') I_COUNT(LT)
      WRITE(FORM8(10:10),'(I1)') I_COUNT(LL)
      WRITE(FORM8(15:15),'(I1)') I_COUNT(LG)
      WRITE(FORM8(20:20),'(I1)') I_COUNT(LN)
      WRITE(FORM8(25:25),'(I1)') I_COUNT(LC)
      WRITE(FORM8(30:30),'(I1)') I_COUNT(LFW)
      WRITE(FORM8(35:35),'(I1)') I_COUNT(LS)
      WRITE(FORM8(40:40),'(I1)') I_COUNT(LD)
      WRITE(IPF,FORM8) '      PARAMETER(LT=',LT,', LL=',LL,
     &  ', LG=',LG,', LN=',LN,', LC=',LC,', LFW=',LFW,', LS=',LS,
     &  ', LD=',LD,')'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Partitioning solute transport switch ' //
     &  '(0 = off, 1 = on), LPC'
      WRITE(IPF,'(A)') '!     Alcohol mass equation switch ' //
     &  '(0 = off, 1 = on), LALC'
      WRITE(IPF,'(A)') '!     Well switch (0 = off, 1 = on), LWELL'
      WRITE(IPF,'(A)') '!     Dissolved CO2 equation switch (0 = off' //
     &  ', 1 = on), LDCO2'
      WRITE(IPF,'(A)') '!     Hydrate/CH4 switch ' //
     &  '(0 = off, 1 = on), LHYD'
      WRITE(IPF,'(A)') '!     Reactive transport switch ' //
     &  '(0 = off, 1 = on), LR'
      WRITE(IPF,'(A)') '!     Component gas switch ' //
     &  '(0 = off, 1 = on), LGC  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM7(5:5),'(I1)') I_COUNT(LPC)
      WRITE(FORM7(10:10),'(I1)') I_COUNT(LALC)
      WRITE(FORM7(15:15),'(I1)') I_COUNT(LWELL)
      WRITE(FORM7(20:20),'(I1)') I_COUNT(LDCO2)
      WRITE(FORM7(25:25),'(I1)') I_COUNT(LHYD)
      WRITE(FORM7(30:30),'(I1)') I_COUNT(LR)
      WRITE(FORM7(35:35),'(I1)') I_COUNT(LGC)
      WRITE(IPF,FORM7) '      PARAMETER(LPC=',LPC,', LALC=',LALC,
     &  ', LWELL=',LWELL,', LDCO2=',LDCO2,', LHYD=',LHYD,', LR=',LR,
     &  ', LGC=',LGC,')'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Nitrogen switch ' //
     &  '(0 = off, 1 = on), LN2  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LN2)
      WRITE(IPF,FORM1) '      PARAMETER(LN2=',LN2,')'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Surface spill switch ' //
     &  '(0 = off, 1 = on), LSPILL  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LSPILL)
      WRITE(IPF,FORM1) '      PARAMETER(LSPILL=',LSPILL,')'
!
!---  Write linear solver parameters: LBD, LSP, LIS, LPT  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Banded matrix linear equation solver ' //
     &  'switch (0 = off, 1 = on), LBD'
      WRITE(IPF,'(A)') '!     SPLIB gradient linear equation solver ' //
     &  'switch (0 = off, 1 = on), LSP'
      WRITE(IPF,'(A)') '!     SPLIB gradient linear equation solver ' //
     &  'switch (0 = off, 1 = on), LIS'
      WRITE(IPF,'(A)') '!     PETSc linear equation solver ' //
     &  'switch (0 = off, 1 = on), LPT  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM4(5:5),'(I1)') I_COUNT(LBD)
      WRITE(FORM4(10:10),'(I1)') I_COUNT(LSP)
      WRITE(FORM4(15:15),'(I1)') I_COUNT(LPT)
      WRITE(FORM4(20:20),'(I1)') I_COUNT(LIS)
      WRITE(IPF,FORM4) '      PARAMETER(LBD=',LBD,', LSP=',LSP,
     &  ', LIS=',LIS,', LPT=',LPT,')'
!
!---  Write boundary condition parameters: LBC, LBCIN, LBTM  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Number of boundary condition ' //
     &  'surfaces, LBC'
      WRITE(IPF,'(A)') '!     Number of boundary condition ' //
     &  'inputs, LBCIN'
      WRITE(IPF,'(A)') '!     Number of boundary condition ' //
     &  'times, LBTM'
      WRITE(IPF,'(A)') '!     XYZ Gradient Option (0 = off, ' //
     &  '1 = on), LXYZG  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM4(5:5),'(I1)') I_COUNT(LBC)
      WRITE(FORM4(10:10),'(I1)') I_COUNT(LBCIN)
      WRITE(FORM4(15:15),'(I1)') I_COUNT(LBTM)
      WRITE(FORM4(20:20),'(I1)') I_COUNT(LXYZG)
      WRITE(IPF,FORM4) '      PARAMETER(LBC=',LBC,', LBCIN=',LBCIN,
     &  ', LBTM=',LBTM,', LXYZG=',LXYZG,')'
!
!---  Write source parameters: LSR, LSTM, LSOLSR  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Number of sources, LSR'
      WRITE(IPF,'(A)') '!     Number of source times, LSTM'
      WRITE(IPF,'(A)') '!     Number of solute well sources, ' //
     &  ' LSOLSR  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM3(5:5),'(I1)') I_COUNT(LSR)
      WRITE(FORM3(10:10),'(I1)') I_COUNT(LSTM)
      WRITE(FORM3(15:15),'(I1)') I_COUNT(LSOLSR)
      WRITE(IPF,FORM3) '      PARAMETER(LSR=',LSR,', LSTM=',LSTM,
     &  ', LSOLSR=',LSOLSR,')'
!
!---  Write parameters: LNW, LNWT, LNWS  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Number of wells, LNW'
      WRITE(IPF,'(A)') '!     Number of well times, LNWT'
      WRITE(IPF,'(A)') '!     Number of well screened cells, LWSI'
      WRITE(IPF,'(A)') '!     Number of well total cells, LWTI'
      WRITE(IPF,'(A)') '!     Number of well screen ' //
     &  'intervals, LNWS  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM5(5:5),'(I1)') I_COUNT(LNW)
      WRITE(FORM5(10:10),'(I1)') I_COUNT(LNWT)
      WRITE(FORM5(15:15),'(I1)') I_COUNT(LWSI)
      WRITE(FORM5(20:20),'(I1)') I_COUNT(LWTI)
      WRITE(FORM5(25:25),'(I1)') I_COUNT(LNWS)
      WRITE(IPF,FORM5) '      PARAMETER(LNW=',LNW,', LNWT=',LNWT,
     &  ', LWSI=',LWSI,', LWTI=',LWTI,', LNWS=',LNWS,')'
!
!---  Write coupled-well parameters: L_CW, LN_CW, LWI_CW, LWT_CW, 
!       LWTP_CW  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Coupled-well model switch (0 = off' //
     &  ', 1 = on), L_CW'
      WRITE(IPF,'(A)') '!     Number of coupled wells, LN_CW'
      WRITE(IPF,'(A)') '!     Number of coupled-well intervals, LWI_CW'
      WRITE(IPF,'(A)') '!     Number of coupled-well times, LWT_CW'
      WRITE(IPF,'(A)') '!     Number of coupled-well time periods, ' // 
     &  'LWTP_CW  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM5(5:5),'(I1)') I_COUNT(L_CW)
      WRITE(FORM5(10:10),'(I1)') I_COUNT(LN_CW)
      WRITE(FORM5(15:15),'(I1)') I_COUNT(LWI_CW)
      WRITE(FORM5(20:20),'(I1)') I_COUNT(LWT_CW)
      WRITE(FORM5(25:25),'(I1)') I_COUNT(LWTP_CW)
      WRITE(IPF,FORM5) '      PARAMETER(L_CW=',L_CW,', LN_CW=',LN_CW,
     &  ', LWI_CW=',LWI_CW,', LWT_CW=',LWT_CW,', LWTP_CW=',LWTP_CW,')'
!
!---  Write coupled-well parameters: LWN_CW, LWF_WC, LUK_CW  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Number of coupled-well nodes, LWN_CW'
      WRITE(IPF,'(A)') '!     Number of field nodes with ' // 
     &  'coupled-well nodes, LWF_CW'
      WRITE(IPF,'(A)') '!     Number of coupled-well solutes, ' // 
     &  'LSOLU_CW'
      WRITE(IPF,'(A)') '!     Number of coupled-well unknowns, ' // 
     &  'LUK_CW  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM4(5:5),'(I1)') I_COUNT(LWN_CW)
      WRITE(FORM4(10:10),'(I1)') I_COUNT(LWF_CW)
      WRITE(FORM4(15:15),'(I1)') I_COUNT(LUK_CW)
      WRITE(FORM4(20:20),'(I1)') I_COUNT(LSOLU_CW)
      WRITE(IPF,FORM4) '      PARAMETER(LWN_CW=',LWN_CW,
     &  ', LWF_CW=',LWF_CW,', LUK_CW=',LUK_CW,', LSOLU_CW=',LSOLU_CW,')'
!
!---  Write parameters: LRC, LSOLU  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Number of rock/soil types, LRC'
      WRITE(IPF,'(A)') '!     Number of solutes, LSOLU'
      WRITE(IPF,'(A)') '!     Number of oil components, LCN'
      WRITE(IPF,'(A)') '!     Number of gas components, LNGC  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM4(5:5),'(I1)') I_COUNT(LRC)
      WRITE(FORM4(10:10),'(I1)') I_COUNT(LSOLU)
      WRITE(FORM4(15:15),'(I1)') I_COUNT(LCN)
      WRITE(FORM4(20:20),'(I1)') I_COUNT(LNGC)
      WRITE(IPF,FORM4) '      PARAMETER(LRC=',LRC,', LSOLU=',LSOLU,
     &  ', LCN=',LCN, ', LNGC=',LNGC,')'
!
!---  Write parameters: LREF, LPTM, LSF  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Number of reference cells, LREF'
      WRITE(IPF,'(A)') '!     Number of print times, LPTM'
      WRITE(IPF,'(A)') '!     Number of integration surfaces, LSF'
      WRITE(IPF,'(A)') '!     Number of surface-flux-domain ' // 
     &  'surfaces, LSFDOM  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM4(5:5),'(I1)') I_COUNT(LREF)
      WRITE(FORM4(10:10),'(I1)') I_COUNT(LPTM)
      WRITE(FORM4(15:15),'(I1)') I_COUNT(LSF)
      WRITE(FORM4(20:20),'(I1)') I_COUNT(LSFDOM)
      WRITE(IPF,FORM4) '      PARAMETER(LREF=',LREF,', LPTM=',LPTM,
     &  ', LSF=',LSF,', LSFDOM=',LSFDOM,')'
!
!---  Write parameters: LOBDT, LOBDS  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Number of observed data types, LOBDT'
      WRITE(IPF,'(A)') '!     Number of observed data samples, ' //
     &  'LOBDS  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM2(5:5),'(I1)') I_COUNT(LOBDT)
      WRITE(FORM2(10:10),'(I1)') I_COUNT(LOBDS)
      WRITE(IPF,FORM2) '      PARAMETER(LOBDT=',LOBDT,
     &  ', LOBDS=',LOBDS,')'
!
!---  Write parameters: LTBL, LCHEM, LPTA, LNNGC  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Number saturation and relative ' //
     &  'permeability table entries, LTBL'
      WRITE(IPF,'(A)') '!     Number of chemical reactions, LCHEM'
      WRITE(IPF,'(A)') '!     Noncondensible gas property table ' //
     &  'switch, (0 = off, 1 = on), LPTA'
      WRITE(IPF,'(A)') '!     Number of noncondensible Gas ' //
     &  'components, LNNGC'
      WRITE(IPF,'(A)') '!     Number of hydrate ' //
     &  'components, LNHC  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM5(5:5),'(I1)') I_COUNT(LTBL)
      WRITE(FORM5(10:10),'(I1)') I_COUNT(LCHEM)
      WRITE(FORM5(15:15),'(I1)') I_COUNT(LPTA)
      WRITE(FORM5(20:20),'(I1)') I_COUNT(LNNGC)
      WRITE(FORM5(25:25),'(I1)') I_COUNT(LNHC)
      WRITE(IPF,FORM5) '      PARAMETER(LTBL=',LTBL,', LCHEM=',LCHEM,
     &  ', LPTA=',LPTA,', LNNGC=',LNNGC,', LNHC=',LNHC,')'
!
!---  Write parameters: LPLANT, LSW, LATM  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!     Number of plant species, LPLANT'
      WRITE(IPF,'(A)') '!     Shuttleworth-Wallace switch (0 = off, ' //
     &  '1 = on), LSW'
      WRITE(IPF,'(A)') '!     Number of atmospheric condition ' //
     &  'times, LATM'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM3(5:5),'(I1)') I_COUNT(LPLANT)
      WRITE(FORM3(10:10),'(I1)') I_COUNT(LSW)
      WRITE(FORM3(15:15),'(I1)') I_COUNT(LATM)
      WRITE(IPF,FORM3) '      PARAMETER(LPLANT=',LPLANT,
     &  ', LSW=',LSW,', LATM=',LATM,')' 
!
!---  Write parameters: LSFCA, LSFCC, LSFCP, LSFCT, LSFCN  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Number of surface cover areas, LSFCA '
      WRITE(IPF,'(A)') '!     Number of surface cover connections, ' // 
     &  ' LSFCC'
      WRITE(IPF,'(A)') '!     Number of surface cover polygon ' // 
     &  'definition pairs, LSFCP'
      WRITE(IPF,'(A)') '!     Number of surface cover times, ' // 
     &  ' LSFCT'
      WRITE(IPF,'(A)') '!     Number of surface cover nodes, ' // 
     &  ' LSFCN  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM5(5:5),'(I1)') I_COUNT(LSFCA)
      WRITE(FORM5(10:10),'(I1)') I_COUNT(LSFCC)
      WRITE(FORM5(15:15),'(I1)') I_COUNT(LSFCP)
      WRITE(FORM5(20:20),'(I1)') I_COUNT(LSFCT)
      WRITE(FORM5(25:25),'(I1)') I_COUNT(LSFCN)
      WRITE(IPF,FORM5) '      PARAMETER(LSFCA=',LSFCA,', LSFCC=',LSFCC,
     &  ', LSFCP=',LSFCP,', LSFCT=',LSFCT,', LSFCN=',LSFCN,')'
!
!---  Write parameters: LUK_SFC ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Number of surface cover equation ' // 
     &  'unknowns, LUK_SFC  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LUK_SFC)
      WRITE(IPF,FORM1) '      PARAMETER(LUK_SFC=',LUK_SFC,')'
!
!---  Write parameters: LPOLYN, LPOLYC  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!     Number of polynomial pieces, LPOLYN '
      WRITE(IPF,'(A)') '!     Number of polynomial coefficients, ' //
     &  'LPOLYC  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM2(5:5),'(I1)') I_COUNT(LPOLYN)
      WRITE(FORM2(10:10),'(I1)') I_COUNT(LPOLYC)
      WRITE(IPF,FORM2) '      PARAMETER(LPOLYN=',LPOLYN,
     &  ', LPOLYC=',LPOLYC,')'
!
!---  Write parameters: LSPG,LSPL,LSPN,LSPS,LSPR  ---
!
      LSPR = MAX( LSPG+LSPL+LSPN+LSPS+LSPE,1 )
      LSPG = MAX( LSPG,1 )
      LSPL = MAX( LSPL,1 )
      LSPN = MAX( LSPN,1 )
      LSPS = MAX( LSPS,1 )
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Number of gas reactive species, LSPG'
      WRITE(IPF,'(A)') '!     Number of aqueous reactive species, LSPL'
      WRITE(IPF,'(A)') '!     Number of NAPL reactive species, LSPN'
      WRITE(IPF,'(A)') '!     Number of solid reactive species, LSPS'
      WRITE(IPF,'(A)') '!     Number of reactive species, LSPR  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM5(5:5),'(I1)') I_COUNT(LSPG)
      WRITE(FORM5(10:10),'(I1)') I_COUNT(LSPL)
      WRITE(FORM5(15:15),'(I1)') I_COUNT(LSPN)
      WRITE(FORM5(20:20),'(I1)') I_COUNT(LSPS)
      WRITE(FORM5(25:25),'(I1)') I_COUNT(LSPR)
      WRITE(IPF,FORM5) '      PARAMETER(LSPG=',LSPG,', LSPL=',LSPL,
     &  ', LSPN=',LSPN,', LSPS=',LSPS,', LSPR=',LSPR,')'
!
!---  Write parameters: LSPE,LESITE  ---
!
      LSPE = MAX( LSPE,1 )
      LESITE = MAX( LESITE,1 )
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Number of exchanged species, LSPE'
      WRITE(IPF,'(A)') '!     Number of exchange sites, LESITE ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM2(5:5),'(I1)') I_COUNT(LSPE)
      WRITE(FORM2(10:10),'(I1)') I_COUNT(LESITE)
      WRITE(IPF,FORM2) '      PARAMETER(LSPE=',LSPE,
     &  ', LESITE=',LESITE,')'
!
!---  Write parameters: LSPT,LSPK,LRCE,LRCK,LCKN  ---
!
      LSPT = MAX( LSPT,1 )
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Number of transported species, LSPT'
      WRITE(IPF,'(A)') '!     Number of kinetic reaction ' //
     &  'species, LSPK'
      WRITE(IPF,'(A)') '!     Number of equilibrium ' //
     &  'reactions, LRCE'
      WRITE(IPF,'(A)') '!     Number of kinetic ' //
     &  'reactions, LRCK'
      WRITE(IPF,'(A)') '!     Number of kinetic ' //
     &  'parameters, LCKN'
      WRITE(IPF,'(A)') '!     Number of mixing ' //
     &  'coefficients, LMC  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM6(5:5),'(I1)') I_COUNT(LSPT)
      WRITE(FORM6(10:10),'(I1)') I_COUNT(LSPK)
      WRITE(FORM6(15:15),'(I1)') I_COUNT(LRCE)
      WRITE(FORM6(20:20),'(I1)') I_COUNT(LRCK)
      WRITE(FORM6(25:25),'(I1)') I_COUNT(LCKN)
      WRITE(FORM6(30:30),'(I1)') I_COUNT(LMC)
      WRITE(IPF,FORM6) '      PARAMETER(LSPT=',LSPT,
     &  ', LSPK=',LSPK,', LRCE=',LRCE,', LRCK=',LRCK,
     &  ', LCKN=',LCKN,', LMC=',LMC,')'
!
!---  Write parameters: LEQC,LEQE,LEQK,LREK  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Number of conservation equations, LEQC'
      WRITE(IPF,'(A)') '!     Number of equilibrium equations, LEQE'
      WRITE(IPF,'(A)') '!     Number of keinetic equations, LEQK'
      WRITE(IPF,'(A)') '!     Number of kinetic equation ' //
     &  'reactions, LREK  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM4(5:5),'(I1)') I_COUNT(LEQC)
      WRITE(FORM4(10:10),'(I1)') I_COUNT(LEQE)
      WRITE(FORM4(15:15),'(I1)') I_COUNT(LEQK)
      WRITE(FORM4(20:20),'(I1)') I_COUNT(LREK)
      WRITE(IPF,FORM4) '      PARAMETER(LEQC=',LEQC,
     &  ', LEQE=',LEQE,', LEQK=',LEQK,', LREK=',LREK,')'
!
!---  Write parameters: LSEC,LSEE,LSEK,LSPBC  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Number of conservation equation ' //
     &  'species, LSEC'
      WRITE(IPF,'(A)') '!     Number of equilibrium equation ' //
     &  'species, LSEE'
      WRITE(IPF,'(A)') '!     Number of kinetic equation ' //
     &  'species, LSEK'
      WRITE(IPF,'(A)') '!     Number of boundary condition ' //
     &  'species, LSPBC  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM4(5:5),'(I1)') I_COUNT(LSEC)
      WRITE(FORM4(10:10),'(I1)') I_COUNT(LSEE)
      WRITE(FORM4(15:15),'(I1)') I_COUNT(LSEK)
      WRITE(FORM4(20:20),'(I1)') I_COUNT(LSPBC)
      WRITE(IPF,FORM4) '      PARAMETER(LSEC=',LSEC,
     &  ', LSEE=',LSEE,', LSEK=',LSEK,', LSPBC=',LSPBC,')'
!
!---  Write parameters: LANI,LCAT,LNEU,LNAF,LNCF,LNFN,LMCG
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Pitzer activity coefficients'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!     Number of anions, LANI'
      WRITE(IPF,'(A)') '!     Number of cations, LCAT' 
      WRITE(IPF,'(A)') '!     Number of neutral species, LNEU  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM3(5:5),'(I1)') I_COUNT(LANI)
      WRITE(FORM3(10:10),'(I1)') I_COUNT(LCAT)
      WRITE(FORM3(15:15),'(I1)') I_COUNT(LNEU)
      WRITE(IPF,FORM3) '      PARAMETER(LANI=',LANI,', LCAT=',LCAT,
     &  ', LNEU=',LNEU,')'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Number of neutral-anion ' //
     &  'interaction parameters, LNAF'
      WRITE(IPF,'(A)') '!     Number of neutral-cation ' //
     &  'interaction parameters, LNCF'
      WRITE(IPF,'(A)') '!     Number of neutral-neutral ' //
     &  'interaction parameters, LNNF'
      WRITE(IPF,'(A)') '!     Number of higher-order ' //
     &  'interaction parameters, LMCG  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM4(5:5),'(I1)') I_COUNT(LNAF)
      WRITE(FORM4(10:10),'(I1)') I_COUNT(LNCF)
      WRITE(FORM4(15:15),'(I1)') I_COUNT(LNNF)
      WRITE(FORM4(20:20),'(I1)') I_COUNT(LMCG)
      WRITE(IPF,FORM4) '      PARAMETER(LNAF=',LNAF,
     &  ', LNCF=',LNCF,', LNNF=',LNNF, 
     &  ', LMCG=',LMCG,')'
!
!---  Write SAC Parameters  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Number of global mass balance check times'
      WRITE(IPF,'(A)') '!     Number of SAC remediation events'
      WRITE(IPF,'(A)') '!     Number of SAC release plane time changes'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM3(5:5),'(I1)') I_COUNT(LBAL)
      WRITE(FORM3(10:10),'(I1)') I_COUNT(LREM)
      WRITE(FORM3(15:15),'(I1)') I_COUNT(LREL)
      WRITE(IPF,FORM3) '      PARAMETER(LBAL=',LBAL,', LREM=',LREM,
     &  ', LREL=',LREL,')'
!
!---  Computed Parameters  ---
!
      LANW = LAN+(LWELL*LFZ*LNW)+(LSPILL*LFXY)
      LPH = LL + LG + LN + LHYD
      LCMP=LD+LL+LN+LS
      LMPH=LL+LG+LN
!
!---  Banded solver  ---
!
      IF( LBD.EQ.1 ) THEN
!
!---    Half band with not computed via Jacobian routine  ---
!
        IF( LHBW.EQ.1 ) THEN
          LHBW = LUK*LMNP + LUK - 1
        ENDIF
!
!---  Non-banded solver  ---
!
      ELSE
        LHBW = 1
      ENDIF      
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Computed parameters  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Number of Active Nodes, LANW'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LANW)
      WRITE(IPF,FORM1) '      PARAMETER(LANW=',LANW,')'
      WRITE(IPF,'(A)') '!---  Number of Unknowns per Node, LUK'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LUK)
      WRITE(IPF,FORM1) '      PARAMETER(LUK=',LUK,')'
      WRITE(IPF,'(A)') '!---  Number of Phases, LPH'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LPH)
      WRITE(IPF,FORM1) '      PARAMETER(LPH=',LPH,')'
      WRITE(IPF,'(A)') '!---  Number of Components, LCMP'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LCMP)
      WRITE(IPF,FORM1) '      PARAMETER(LCMP=',LCMP,')'
      WRITE(IPF,'(A)') '!---  Number of Mobile Phases, LMPH'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LMPH)
      WRITE(IPF,FORM1) '      PARAMETER(LMPH=',LMPH,')'
      WRITE(IPF,'(A)') '!---  Number of Nodes in XY Plane, LFXY'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LFXY)
      WRITE(IPF,FORM1) '      PARAMETER(LFXY=',LFXY,')'
      WRITE(IPF,'(A)') '!---  Number of Nodes in YZ Plane, LFYZ'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LFYZ)
      WRITE(IPF,FORM1) '      PARAMETER(LFYZ=',LFYZ,')'
      WRITE(IPF,'(A)') '!---  Number of Nodes in ZX Plane, LFZX'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LFZX)
      WRITE(IPF,FORM1) '      PARAMETER(LFZX=',LFZX,')'
      WRITE(IPF,'(A)') '!---  Number of Total Nodes, LFD'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LFD)
      WRITE(IPF,FORM1) '      PARAMETER(LFD=',LFD,')'
      WRITE(IPF,'(A)') '!---  Number of Block Refinement Nodes, LBR'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LBR)
      WRITE(IPF,FORM1) '      PARAMETER(LBR=',LBR,')'
      WRITE(IPF,'(A)') '!---  Maximum Connection Stencil, LSTC'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LSTC)
      WRITE(IPF,FORM1) '      PARAMETER(LSTC=',LSTC,')'
      WRITE(IPF,'(A)') '!---  Minimum Half-Band Width, LHBW'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LHBW)
      WRITE(IPF,FORM1) '      PARAMETER(LHBW=',LHBW,')'
!     
!---  Linear system solver variable parameters  ---
!
      LSTCX = MAX( LSTC,7 )
      LJA = MAX( LANW*LUK + L_CW*LN_CW + L_SFC*5*LSFCN + L_DP*LANW*LUK,
     &  LM*LFEN*3 )
      LJA = (LSP+LIS)*LJA + LBD + LPT
      LJB = MAX( LSTC*LANW*LUK*LUK + L_DP*LANW*LUK*LUK*3
     &   + L_CW*(LN_CW + 2*LWF_CW*LUK),LM*LFEN*3 )
      LJB = (LSP+LIS)*LJB
     &   + L_SFC*(LUK_SFC*LSFCN) + LBD + LPT
      LJO = LJB
      LJO_GM = (LSP+LIS)*LJO_GM + LBD + LPT
      LJB = MAX( LJB,LJO_GM)
      LJC = (LSP+LIS+LPT)*(LANW*LUK + L_CW*LN_CW + 
     &  L_SFC*5*LSFCN + L_DP*LANW*LUK + 1) + LBD
      LJC_GM = (LSP+LIS+LPT)*((LFEN*3) + 1) + LBD
      LJD = LBD*(3*LHBW+1) + LSP + LPT + LIS
      LJE = LBD*(LANW*LUK + L_CW*LN_CW + L_SFC*5*LSFCN 
     &  + L_DP*LANW*LUK) + LSP + LPT + LIS
      LJE = MAX( LBD*LFEN*3,LJE )
      LJF = LANW*LUK + L_CW*LN_CW + L_SFC*5*LSFCN
     &  + L_DP*LANW*LUK
      LJF = MAX( LFEN*3,LJF )
      LJG = (LSP+LIS)*(LANW*LUK + L_CW*LN_CW + 
     &  L_DP*LANW*LUK) + LBD + LPT
      LJG_GM = (LSP+LIS)*(LFEN) + LBD
      LJH = (LSP+LIS)*(LSTCX*LUK + 3*LUK*LWELL + L_DP*LUK)
     &  + LBD + LPT
      LJH_GM = (LSP+LIS)*(27)
      LJI = LBD*(LANW*LUK + L_CW*LN_CW + L_SFC*5*LSFCN 
     &  + L_DP*LANW*LUK) + LSP + LPT + LIS*LJF
      LJI = MAX( LBD*LFEN*3,LJI) 
      LJJ = LBD*(LANW*LUK + L_CW*LN_CW + L_SFC*5*LSFCN
     &  + L_DP*LANW*LUK) + LSP + LPT + LIS
      LJJ = MAX( LBD*LFEN*3,LJJ)
      LJK = (LSP+LIS)*LANW + LBD + LPT
      LJL = LSTCX
      LJM = (LSP+LIS)*LSTC*LANW + LBD + LPT
      LJN = (LSP+LIS+LPT)*(LANW+1) + LBD
      LSU = 2
      LSV = (LUK+2)
      LSFV = (2*LUK+1)
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Linear system solver variable '
     &  // 'parameters  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LJA)
      WRITE(IPF,FORM1) '      PARAMETER(LJA=',LJA,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LJB)
      WRITE(IPF,FORM1) '      PARAMETER(LJB=',LJB,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LJO_GM)
      WRITE(IPF,FORM1) '      PARAMETER(LJO_GM=',LJO_GM,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LJC)
      WRITE(IPF,FORM1) '      PARAMETER(LJC=',LJC,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LJC_GM)
      WRITE(IPF,FORM1) '      PARAMETER(LJC_GM=',LJC_GM,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LJD)
      WRITE(IPF,FORM1) '      PARAMETER(LJD=',LJD,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LJE)
      WRITE(IPF,FORM1) '      PARAMETER(LJE=',LJE,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LJF)
      WRITE(IPF,FORM1) '      PARAMETER(LJF=',LJF,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LJG)
      WRITE(IPF,FORM1) '      PARAMETER(LJG=',LJG,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LJG_GM)
      WRITE(IPF,FORM1) '      PARAMETER(LJG_GM=',LJG_GM,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LJH)
      WRITE(IPF,FORM1) '      PARAMETER(LJH=',LJH,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LJH_GM)
      WRITE(IPF,FORM1) '      PARAMETER(LJH_GM=',LJH_GM,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LJI)
      WRITE(IPF,FORM1) '      PARAMETER(LJI=',LJI,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LJJ)
      WRITE(IPF,FORM1) '      PARAMETER(LJJ=',LJJ,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LJK)
      WRITE(IPF,FORM1) '      PARAMETER(LJK=',LJK,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LJL)
      WRITE(IPF,FORM1) '      PARAMETER(LJL=',LJL,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LJM)
      WRITE(IPF,FORM1) '      PARAMETER(LJM=',LJM,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LJN)
      WRITE(IPF,FORM1) '      PARAMETER(LJN=',LJN,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LJO)
      WRITE(IPF,FORM1) '      PARAMETER(LJO=',LJO,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LSU)
      WRITE(IPF,FORM1) '      PARAMETER(LSU=',LSU,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LSV)
      WRITE(IPF,FORM1) '      PARAMETER(LSV=',LSV,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LSFV)
      WRITE(IPF,FORM1) '      PARAMETER(LSFV=',LSFV,')'
!
!---  Field Variable Parameters  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Field variable parameters  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,FORM0) '      PARAMETER(LFDT=LFD**LT)'
      WRITE(IPF,FORM0) '      PARAMETER(LFDL=LFD**LL)'
      WRITE(IPF,FORM0) '      PARAMETER(LFDG=LFD**LG)'
      WRITE(IPF,FORM0) '      PARAMETER(LFDN=LFD**LN)'
      WRITE(IPF,FORM0) '      PARAMETER(LFDN2=LFD**LN2)'
      WRITE(IPF,FORM0) '      PARAMETER(LFDC=LFD**LC)'
      WRITE(IPF,FORM0) '      PARAMETER(LFDM=LFD**LM)'
      WRITE(IPF,FORM0) '      PARAMETER(LFDS='
     &  // 'LFD**((LS+LALC)-(LS*LALC)))'
      WRITE(IPF,FORM0) '      PARAMETER(LFDR=LFD**LR)'
      WRITE(IPF,FORM0) '      PARAMETER(LFDRL=LFD**(LR*LL))'
      WRITE(IPF,FORM0) '      PARAMETER(LFDCR=LFD**((LC+LR)-(LC*LR)))'
      WRITE(IPF,FORM0) '      PARAMETER(LFDRG=LFD**(LR*LG))'
      WRITE(IPF,FORM0) '      PARAMETER(LFDRN=LFD**(LR*LN))'
      WRITE(IPF,FORM0) '      PARAMETER(LFDI='
     &  // 'LFD**((LFW+LHYD)-(LFW*LHYD)))'
      WRITE(IPF,FORM0) '      PARAMETER(LFDD='
     &  // 'LFD**((LD+LDCO2)-(LD*LDCO2)))'
      WRITE(IPF,FORM0) '      PARAMETER(LFDA=LFD**LN)'
      WRITE(IPF,FORM0) '      PARAMETER(LFDH=LFD**LHYD)'
      WRITE(IPF,FORM0) '      PARAMETER(LFDNH='
     &  // 'LFD**((LN+LHYD)-(LN*LHYD)))'
      WRITE(IPF,FORM0) '      PARAMETER(LFDGC=LFD**LGC)'
!
!---  Surface Variable Parameters  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Surface variable parameters  ---'
      WRITE(IPF,'(A)') '!'
      LSX = LSX + (LFX+1)*LFYZ + LSRX
      LSY = LSY + LFX*(LFY+1)*LFZ + LSRY
      LSZ = LSZ + LFXY*(LFZ+1) + LSRZ
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LSX)
      WRITE(IPF,FORM1) '      PARAMETER(LSX=',LSX,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LSY)
      WRITE(IPF,FORM1) '      PARAMETER(LSY=',LSY,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LSZ)
      WRITE(IPF,FORM1) '      PARAMETER(LSZ=',LSZ,')'
      WRITE(IPF,FORM0) '      PARAMETER(LSXT=LSX**LT)'
      WRITE(IPF,FORM0) '      PARAMETER(LSXL=LSX**LL)'
      WRITE(IPF,FORM0) '      PARAMETER(LSXG=LSX**LG)'
      WRITE(IPF,FORM0) '      PARAMETER(LSXN='
     &  // 'LSX**((LN+LHYD)-(LN*LHYD)))'
      WRITE(IPF,FORM0) '      PARAMETER(LSXN2=LSX**LN2)'
      WRITE(IPF,FORM0) '      PARAMETER(LSXC=LSX**((LC+LR)-(LC*LR)))'
      WRITE(IPF,FORM0) '      PARAMETER(LSXS=' 
     &  // 'LSX**((LS+LALC)-(LS*LALC)))'
      WRITE(IPF,FORM0) '      PARAMETER(LSXGC=LSX**(LGC*LG))'
      WRITE(IPF,FORM0) '      PARAMETER(LSXLC=LSX**(LGC*LL))'
      WRITE(IPF,FORM0) '      PARAMETER(LSXNC=LSX**(LGC*LN))'
      WRITE(IPF,FORM0) '      PARAMETER(LSYT=LSY**LT)'
      WRITE(IPF,FORM0) '      PARAMETER(LSYL=LSY**LL)'
      WRITE(IPF,FORM0) '      PARAMETER(LSYG=LSY**LG)'
      WRITE(IPF,FORM0) '      PARAMETER(LSYN='
     &  // 'LSY**((LN+LHYD)-(LN*LHYD)))'
      WRITE(IPF,FORM0) '      PARAMETER(LSYN2=LSY**LN2)'
      WRITE(IPF,FORM0) '      PARAMETER(LSYC=LSY**((LC+LR)-(LC*LR)))'
      WRITE(IPF,FORM0) '      PARAMETER(LSYS='
     &  // 'LSY**((LS+LALC)-(LS*LALC)))'
      WRITE(IPF,FORM0) '      PARAMETER(LSYGC=LSY**(LGC*LG))'
      WRITE(IPF,FORM0) '      PARAMETER(LSYLC=LSY**(LGC*LL))'
      WRITE(IPF,FORM0) '      PARAMETER(LSYNC=LSY**(LGC*LN))'
      WRITE(IPF,FORM0) '      PARAMETER(LSZT=LSZ**LT)'
      WRITE(IPF,FORM0) '      PARAMETER(LSZL=LSZ**LL)'
      WRITE(IPF,FORM0) '      PARAMETER(LSZG=LSZ**LG)'
      WRITE(IPF,FORM0) '      PARAMETER(LSZN='
     &  // 'LSZ**((LN+LHYD)-(LN*LHYD)))'
      WRITE(IPF,FORM0) '      PARAMETER(LSZN2=LSZ**LN2)'
      WRITE(IPF,FORM0) '      PARAMETER(LSZC=LSZ**((LC+LR)-(LC*LR)))'
      WRITE(IPF,FORM0) '      PARAMETER(LSZS='
     &  // 'LSZ**((LS+LALC)-(LS*LALC)))'
      WRITE(IPF,FORM0) '      PARAMETER(LSZGC=LSZ**(LGC*LG))'
      WRITE(IPF,FORM0) '      PARAMETER(LSZLC=LSZ**(LGC*LL))'
      WRITE(IPF,FORM0) '      PARAMETER(LSZNC=LSZ**(LGC*LN))'
!
!---  Rock/Soil Variable Parameters  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Rock/soil variable parameters  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,FORM0) '      PARAMETER(LRCT=LRC**LT)'
      WRITE(IPF,FORM0) '      PARAMETER(LRCL=LRC**LL)'
      WRITE(IPF,FORM0) '      PARAMETER(LRCG=LRC**LG)'
      WRITE(IPF,FORM0) '      PARAMETER(LRCN=LRC**LN)'
      WRITE(IPF,FORM0) '      PARAMETER(LRCS='
     &  // 'LRC**((LS+LALC)-(LS*LALC)))'
!
!---  Boundary Condition Parameters  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Boundary condition parameters  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,FORM0) '      PARAMETER(LBCT=LBC**LT)'
      WRITE(IPF,FORM0) '      PARAMETER(LBCL=LBC**LL)'
      WRITE(IPF,FORM0) '      PARAMETER(LBCG=LBC**LG)'
      WRITE(IPF,FORM0) '      PARAMETER(LBCN=LBC**LN)'
      WRITE(IPF,FORM0) '      PARAMETER(LBCN2=LBC**LN2)'
      WRITE(IPF,FORM0) '      PARAMETER(LBCC=LBC**((LC+LR)-(LC*LR)))'
      WRITE(IPF,FORM0) '      PARAMETER(LBCS='
     &  // 'LBC**((LS+LALC)-(LS*LALC)))'
      WRITE(IPF,FORM0) '      PARAMETER(LBCI='
     &  // 'LBC**((LFW+LHYD)-(LFW*LHYD)))'
      WRITE(IPF,FORM0) '      PARAMETER(LBCA=LBC**LN)'
      WRITE(IPF,FORM0) '      PARAMETER(LBCH=LBC**LHYD)'
      WRITE(IPF,FORM0) '      PARAMETER(LBCGC=LBC**LGC)'
      IF( IOM.EQ.36 .OR. IOM.EQ.37 .OR. IOM.EQ.38 ) THEN
        LBCU = 13
        LBCV = LBCU + (LSOLU*LC) + (LSPBC*LR)
      ELSEIF( IOM.EQ.43 ) THEN
        LBCU = 10 + 2*LNGC
        LBCV = LBCU + (LSOLU*LC) + (LSPBC*LR)
      ELSEIF( IOM.EQ.50 ) THEN
        LBCU = 11
        LBCV = LBCU+((LPH**LPC)*(LSOLU*LC))+(LSPBC*LR)
      ELSEIF( IOM.EQ.51 ) THEN
        LBCU = 13
        LBCV = LBCU+((LPH**LPC)*(LSOLU*LC))+(LSPBC*LR)
      ELSEIF( IOM.EQ.52 ) THEN
        LBCU = 11
        LBCV = LBCU+((LPH**LPC)*(LSOLU*LC))+(LSPBC*LR)
      ELSE
        LBCU = LUK+(LPH*LCN)+LT+((LPLANT-1)*LSW*2)+3+(LNGC-1)
        LBCV = LBCU+((LPH**LPC)*(LSOLU*LC))+(LSPBC*LR)
      ENDIF
      IF( LXYZG.EQ.1 ) LBCV = LBCV + 3
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LBCU)
      WRITE(IPF,FORM1) '      PARAMETER(LBCU=',LBCU,')'
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LBCV)
      WRITE(IPF,FORM1) '      PARAMETER(LBCV=',LBCV,')'
!
!---  Output Variable Parameters  ---
!     Number of Source Code Files Parameter  ---
!
      LOUPV = 400+33*((LSOLU**LC)+((LSPR+LSPT)**LR)+2-LC-LR)
      LFILES = 400
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Number of Output Variable Options, LOUPV'
      WRITE(IPF,'(A)') '!     Number of Source Code Files, LFILES'
      WRITE(IPF,'(A)') '!     Number of Reference Node Variables, LVREF'
      WRITE(IPF,'(A)') '!     Number of Plot File Variables, LVPLOT'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM4(5:5),'(I1)') I_COUNT(LOUPV)
      WRITE(FORM4(10:10),'(I1)') I_COUNT(LFILES)
      WRITE(FORM4(15:15),'(I1)') I_COUNT(LVREF)
      WRITE(FORM4(20:20),'(I1)') I_COUNT(LVPLOT)
      WRITE(IPF,FORM4) '      PARAMETER(LOUPV=',LOUPV,
     &  ', LFILES=',LFILES,', LVREF=',LVREF,', LVPLOT=',LVPLOT,')'
!
!---  Soil Moisture-Retention Characteristic Parameters  ---
!     Relative Permeability Tensor Parameters  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Soil moisture-retention characteristic '
     &  // 'parameters'
      WRITE(IPF,'(A)') '!     Aqueous relative permeability parameters'
      WRITE(IPF,'(A)') '!     Gas relative permeability parameters'
      WRITE(IPF,'(A)') '!     NAPL relative permeability parameters'
      WRITE(IPF,'(A)') '!     Relative permeability tensor '
     &  // 'parameters  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM3(5:5),'(I1)') I_COUNT(LRPLC)
      WRITE(FORM3(10:10),'(I1)') I_COUNT(LRPGC)
      WRITE(FORM3(10:10),'(I1)') I_COUNT(LRPNC)
      WRITE(IPF,FORM3) '      PARAMETER(LSCHR=18, LRPLC=',LRPLC,
     &  ', LRPGC=',LRPGC,', LRPNC=',LRPNC,', LRPL=4)'
!
!---  Write ternary hydrate parameters  ---
!
      LHF_HT = 3**(LHYD*LN2)
      LCN_HT = 11**(LHYD*LN2)
      LCP_HT = 45**(LHYD*LN2)
      LCH_HT = 66**(LHYD*LN2)
      LPE_HT = 52**(LHYD*LN2)
      LHE_HT = 115**(LHYD*LN2)
      LTP_HT = 27**(LHYD*LN2)
      LPP_HT = 54**(LHYD*LN2)
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Ternary hydrate parameters'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!     Number of hydrate formers, LHF_HT'
      WRITE(IPF,'(A)') '!     Number of phase and hydrate ' //
     &  'equilibria table concentrations, LCN_HT'
      WRITE(IPF,'(A)') '!     Number of phase equilibria table ' //
     &  'compositions, LCP_HT'
      WRITE(IPF,'(A)') '!     Number of hydrate equilibria table ' //
     &  'compositions, LCH_HT'
      WRITE(IPF,'(A)') '!     Number of phase envelope table ' //
     &  'points, LPE_HT'
      WRITE(IPF,'(A)') '!     Number of hydrate equilibrium table ' //
     &  'points, LHE_HT'
      WRITE(IPF,'(A)') '!     Number of two-phase table temperature ' //
     &  'points, LTP_HT'
      WRITE(IPF,'(A)') '!     Number of two-phase table pressure ' //
     &  'points, LPP_HT  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM4(5:5),'(I1)') I_COUNT(LHF_HT)
      WRITE(FORM4(10:10),'(I1)') I_COUNT(LCN_HT)
      WRITE(FORM4(15:15),'(I1)') I_COUNT(LCP_HT)
      WRITE(FORM4(20:20),'(I1)') I_COUNT(LCH_HT)
      WRITE(IPF,FORM4) '      PARAMETER(LHF_HT=',LHF_HT,
     &  ', LCN_HT=',LCN_HT,', LCP_HT=',LCP_HT,', LCH_HT=',LCH_HT,')'
      WRITE(FORM4(5:5),'(I1)') I_COUNT(LPE_HT)
      WRITE(FORM4(10:10),'(I1)') I_COUNT(LHE_HT)
      WRITE(FORM4(15:15),'(I1)') I_COUNT(LTP_HT)
      WRITE(FORM4(20:20),'(I1)') I_COUNT(LPP_HT)
      WRITE(IPF,FORM4) '      PARAMETER(LPE_HT=',LPE_HT,
     &  ', LHE_HT=',LHE_HT,', LTP_HT=',LTP_HT,', LPP_HT=',LPP_HT,')'
!
!---  Write EOR parameters  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  EOR parameters'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!     Number of petroleum fractions, '  // 
     &  'LPF_EOR'
      WRITE(IPF,'(A)') '!     Number of petroleum component '  // 
     &  'fractions, LPCF  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM2(5:5),'(I1)') I_COUNT(LPF_EOR)
      WRITE(FORM2(10:10),'(I1)') I_COUNT(LPCF)
      WRITE(IPF,FORM2) '      PARAMETER(LPF_EOR=',LPF_EOR,
     &  ', LPCF=',LPCF,')'
!
!---  Noncondensible Gas Property Table Parameters  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Noncondensible gas property table '
     &  // 'parameters  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,FORM0) '      PARAMETER(LP_TA=72**LPTA)'
      WRITE(IPF,FORM0) '      PARAMETER(LT_TA=70**LPTA)'
      WRITE(IPF,FORM0) '      PARAMETER(L_LV=115**LPTA)'
      WRITE(IPF,FORM0) '      PARAMETER(LT_TH=100**LPTA)'
      WRITE(IPF,FORM0) '      PARAMETER(LO_TH=11**LPTA)'
      WRITE(IPF,FORM0) '      PARAMETER(LT_PH=155**LPTA)'
      WRITE(IPF,FORM0) '      PARAMETER(LO_PH=11**LPTA)'
      WRITE(IPF,FORM0) '      PARAMETER(LINH=15)'
!
!---  Coupled Multifluid Well Model Parameters  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Coupled multifluid well model '
     &  // 'parameters  ---'
      WRITE(IPF,'(A)') '!'
      LNWN = LNW*LFZ
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LNWN)
      WRITE(IPF,FORM1) '      PARAMETER(LNWN=',LNWN,')'
      LSZW = LNW*(LFZ+1)
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LSZW)
      WRITE(IPF,FORM1) '      PARAMETER(LSZW=',LSZW,')'
      LNWV = 6
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LNWV)
      WRITE(IPF,FORM1) '      PARAMETER(LNWV=',LNWV,')'
      LUKW = LUK+(LUK*LWELL)
      WRITE(FORM1(5:5),'(I1)') I_COUNT(LUKW)
      WRITE(IPF,FORM1) '      PARAMETER(LUKW=',LUKW,')'
!
!---  Equivalent Continuum and Dual Porosity Model Parameters  ---
!
      LFD_DP = LFD**L_DP
      LFD_EC = LFD**(L_EC+L_DP)
      LBC_EC = LBC**L_EC
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Equivalent continuum and dual ' //  
     &  'porosity model parameters  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM3(5:5),'(I1)') I_COUNT(LFD_DP)
      WRITE(FORM3(10:10),'(I1)') I_COUNT(LFD_EC)
      WRITE(FORM3(15:15),'(I1)') I_COUNT(LBC_EC)
      WRITE(IPF,FORM3) '      PARAMETER(LFD_DP=',LFD_DP,
     &  ', LFD_EC=',LFD_EC,', LBC_EC=',LBC_EC,')'
      WRITE(IPF,'(A)') ' '
!
!---  Geomechanical Parameters  ---
!
      WRITE(IPF,'(A)') '!'
      WRITE(IPF,'(A)') '!---  Geomechanical model parameters  ---'
      WRITE(IPF,'(A)') '!'
      WRITE(FORM4(5:5),'(I1)') I_COUNT(LBC_GM)
      WRITE(FORM4(10:10),'(I1)') I_COUNT(LBCIN_GM)
      WRITE(FORM4(15:15),'(I1)') I_COUNT(LBTM_GM)
      WRITE(FORM4(20:20),'(I1)') I_COUNT(LBCV_GM)
      WRITE(IPF,FORM4) '      PARAMETER(LBC_GM=',LBC_GM,
     &  ', LBCIN_GM=',LBCIN_GM,', LBTM_GM=',LBTM_GM,
     &  ', LBCV_GM=',LBCV_GM,')'
      WRITE(IPF,'(A)') ' '
!
!---  End of WR_FPF group
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRMSGP( INDX )
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
!     Write warnings and error messages to the screen and output file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, September, 1994.
!     $Id: step.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
      EXTERNAL I_COUNT
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*9 FORM1
      CHARACTER*9 FORM2
      CHARACTER*17 FORM3
      CHARACTER*19 FORM4
      CHARACTER*9 FORM5
      CHARACTER*9 FORM6
      CHARACTER*19 FORM14
      CHARACTER*19 FORM17
      CHARACTER*19 FORM21
!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM1,FORM2,FORM3,FORM4,FORM5,FORM6
      DATA FORM1 / '(/,3A,I4)' /
      DATA FORM2 / '(/,2A,I6)' /
      DATA FORM3 / '(/,2A,I6,1PE11.4)' /
      DATA FORM4 / '(/,3A,I6,A,1PE11.4)' /
      DATA FORM5 / '(/,3A,I6)' /
      DATA FORM6 / '(/,2A,I6)' /
      DATA FORM14 / '(/,A,I6,2A,1PE11.4)' /
      DATA FORM17 / '(/,A,I6,2A,1PE11.4)' /
      DATA FORM21 / '(/,A,I6,2A,1PE11.4)' /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRMSGP'
      NCH = INDEX( CHMSG(1:),'  ' )-1
      ICSN = INDEX( SUB_LOG(1)(1:),'  ' )-1
      SUBNM(1:ICSN) = SUB_LOG(1)(1:ICSN)
      DO 10 I = 2,ISUB_LOG
        ICSNX = INDEX( SUB_LOG(I)(1:),'  ' )-1
        SUBNM(ICSN+1:ICSN+ICSNX) = SUB_LOG(I)(1:ICSNX)
        ICSN = ICSN+ICSNX
   10 CONTINUE
      IF( INDX.EQ.0 ) THEN
        WRITE(6,'(/,A)') CHMSG(:NCH)
      ELSEIF( INDX.EQ.1 ) THEN
        WRITE(6,'(2A)') 'NOTE: ',CHMSG(:NCH)
      ELSEIF( INDX.EQ.2 ) THEN
        WRITE(6,'(/,2A)') 'INPUT WARNING: ',CHMSG(:NCH)
        WRITE(6,'(2A)') 'INPUT CARD: ',CARD(:ICD)
      ELSEIF( INDX.EQ.3 ) THEN
        WRITE(6,'(/,2A)') 'EXECUTION ERROR: ',CHMSG(:NCH)
        WRITE(6,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        STOP
      ELSEIF( INDX.EQ.4 ) THEN
        WRITE(6,'(/,2A)') 'INPUT ERROR: ',CHMSG(:NCH)
        WRITE(6,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        STOP
      ELSEIF( INDX.EQ.5 ) THEN
        WRITE(6,'(/,2A)') 'PARAMETER ERROR: ',CHMSG(:NCH)
        WRITE(6,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        STOP
      ELSEIF( INDX.EQ.6 ) THEN
        WRITE(6,'(/,2A)') 'PARAMETER ERROR: ',CHMSG(:NCH)
        WRITE(6,'(2A)') 'INPUT CARD: ',CARD(:ICD)
      ELSEIF( INDX.EQ.7 ) THEN
        WRITE(FORM1(8:8),'(I1)') I_COUNT( IMSG )
        WRITE(6,FORM1) 'INPUT ERROR: ',CHMSG(:NCH),': ',IMSG
        WRITE(6,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        STOP
      ELSEIF( INDX.EQ.8 ) THEN
        WRITE(6,'(/,2A)') 'PARAMETER ERROR: ',CHMSG(:NCH)
        WRITE(6,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        STOP
      ELSEIF( INDX.EQ.9 ) THEN
        WRITE(6,'(/,3A,1PE11.4)') 'INPUT ERROR: ',CHMSG(:NCH),': ',
     &    RLMSG
        WRITE(6,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        STOP
      ELSEIF( INDX.EQ.10 ) THEN
        WRITE(6,'(/,3A,1PE11.4)') 'STATE CONDITION ERROR: ',
     &    CHMSG(:NCH),': ',RLMSG
      ELSEIF( INDX.EQ.11 ) THEN
        WRITE(6,'(/,3A,1PE11.4)') 'STATE CONDITION ERROR: ',
     &    CHMSG(:NCH),': ',RLMSG
        STOP
      ELSEIF( INDX.EQ.12 ) THEN
        WRITE(FORM2(8:8),'(I1)') I_COUNT( IMSG )
        WRITE(6,FORM2) 'EXECUTION ERROR: ',CHMSG(:NCH),IMSG
        WRITE(6,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        STOP
      ELSEIF( INDX.EQ.13 ) THEN
        WRITE(FORM14(7:7),'(I1)') I_COUNT( N_DB )
        WRITE(6,FORM14) 'EXECUTION WARNING: NODE = ',
     &    N_DB,': ',CHMSG(:NCH),RLMSG
        WRITE(6,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
      ELSEIF( INDX.EQ.14 ) THEN
        WRITE(FORM14(7:7),'(I1)') I_COUNT( N_DB )
        WRITE(6,FORM14) 'EXECUTION ERROR: NODE = ',
     &    N_DB,': ',CHMSG(:NCH),RLMSG
        WRITE(6,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        STOP
      ELSEIF( INDX.EQ.15 ) THEN
        WRITE(FORM3(8:8),'(I1)') I_COUNT( IMSG )
        WRITE(6,FORM3) 'EXECUTION ERROR: ',CHMSG(:NCH),IMSG,RLMSG
        WRITE(6,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        STOP
      ELSEIF( INDX.EQ.16 ) THEN
        WRITE(FORM4(8:8),'(I1)') I_COUNT( IMSG )
        WRITE(6,FORM4) 'INPUT ERROR: ',CHMSG(:NCH),
     &    ': ',IMSG,': ',RLMSG
        WRITE(6,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        STOP
      ELSEIF( INDX.EQ.17 ) THEN
        WRITE(FORM17(7:7),'(I1)') I_COUNT( N_DB )
        WRITE(6,FORM17) 'STATE CONDIITON ERROR: NODE = ',
     &    N_DB,': ',CHMSG(:NCH),RLMSG
        WRITE(6,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        STOP
      ELSEIF( INDX.EQ.18 ) THEN
        WRITE(6,'(/,2A)') 'INPUT ERROR: ',CHMSG(:NCH)
        STOP
      ELSEIF( INDX.EQ.19 ) THEN
        WRITE(6,'(/,2A,1PE11.4)')'EXECUTION ERROR: ',CHMSG(:NCH),RLMSG
        WRITE(6,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        STOP
      ELSEIF( INDX.EQ.20 ) THEN
        WRITE(FORM6(8:8),'(I1)') I_COUNT( IMSG )
        WRITE(6,FORM6) 'EXECUTION ERROR: ',CHMSG(:NCH),IMSG
        WRITE(6,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
      ELSEIF( INDX.EQ.21 ) THEN
        WRITE(FORM21(7:7),'(I1)') I_COUNT( N_DB )
        WRITE(6,FORM21) 'STATE CONDIITON ERROR: NODE = ',
     &    N_DB,': ',CHMSG(:NCH),RLMSG
        WRITE(6,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRMSGP group
!
      RETURN
      END

