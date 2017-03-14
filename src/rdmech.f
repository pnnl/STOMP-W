!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDMECH
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
!     Read input file for rock/soil mechanical information.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, December 1992.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
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
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,BDUM,CDUM,UNTS
      CHARACTER*512 CHDUM
!      CHARACTER*64 RDUM(28)
!      CHARACTER*512 CHDUMX
!      INTEGER NRCH(28)
!
!----------------------Common Blocks-----------------------------------!
!





!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDMECH'
      IF( INDEX(SVN_ID(153)(1:1),'$').EQ.0 ) SVN_ID(153) =
     & '$Id: rdmech.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!!
!!---  Subroutine to create a IJK property table from data listed
!!     at x, y, z coordinate locations, using a nearest neighbor
!!     approach (configured for the Johansen formation)  ---
!!
!      CALL CONV_XYZ
!
!---  Check for undefined rock/soil types in active nodes  ---
!
      CARD = 'Rock/Soil Zonation Card'
      DO 1 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 1
        IF( IZ(N).EQ.0 ) THEN
          INDX = 7
          IMSG = N
          CHMSG = 'Undefined Rock/Soil Type @ Node'
          CALL WRMSGS( INDX )
        ENDIF
    1 CONTINUE
!
!---  Write card information to ouput file  ---
!
      CARD = 'Rock/Soil Mechanical Properties Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Loop over the rock/soil mechanical information lines  ---
!
      N = 0
      IJK = 0
      ISGRP = 0
!      IKW = 0
!      CHDUMX = ' '
   10 CONTINUE
        IF( N.GE.NROCK .OR. IJK.GT.0 ) GOTO 500
!        IF( INDEX(CHDUMX(1:),'<rock/soil>').NE.0 ) THEN
!          CHDUM = CHDUMX
!        ELSEIF( INDEX(CHDUMX(1:),'<endcard>').NE.0 ) THEN
!          INDX = 4
!          CHMSG = 'Missing Rock Soil Type(s)'
!          CALL WRMSGS( INDX )
!        ELSE
        ISTART = 1
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
!        ENDIF
!!
!!---    Keyword formatting ---
!!
!        IF( INDEX(CHDUM(1:),'<rock/soil>').NE.0 ) THEN
!          DO 20 NR = 1,28
!            NRCH(NR) = 0
!   20     CONTINUE
!          CALL RDKEYW(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!          CALL RDCHR(ISTART,ICOMMA,NRCH(1),CHDUM,RDUM(1))
!   30     CONTINUE
!          ISTART = 1
!          CALL RDINPL( CHDUM )
!          CALL LCASE( CHDUM )
!          CALL RDKEYW(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!          IF( INDEX(ADUM(1:),'<grain density>').NE.0 ) THEN
!            CALL RDCHR(ISTART,ICOMMA,NRCH(2),CHDUM,RDUM(2))
!            CALL RDCHR(ISTART,ICOMMA,NRCH(3),CHDUM,RDUM(3))
!            GOTO 30
!          ELSEIF( INDEX(ADUM(1:),'<diffusive porosity>').NE.0 ) THEN
!            CALL RDCHR(ISTART,ICOMMA,NRCH(4),CHDUM,RDUM(4))
!            GOTO 30
!          ELSEIF( INDEX(ADUM(1:),'<total porosity>').NE.0 ) THEN
!            CALL RDCHR(ISTART,ICOMMA,NRCH(5),CHDUM,RDUM(5))
!            GOTO 30
!          ELSEIF( INDEX(ADUM(1:),'<bulk compressibility>').NE.0 ) THEN
!            CALL RDCHR(ISTART,ICOMMA,NRCH(6),CHDUM,RDUM(6))
!            CALL RDCHR(ISTART,ICOMMA,NRCH(7),CHDUM,RDUM(7))
!            GOTO 30
!          ELSEIF( INDEX(ADUM(1:),'<pore compressibility>').NE.0 ) THEN
!            CALL RDCHR(ISTART,ICOMMA,NRCH(6),CHDUM,RDUM(6))
!            CALL RDCHR(ISTART,ICOMMA,NRCH(7),CHDUM,RDUM(7))
!            GOTO 30
!          ELSEIF( INDEX(ADUM(1:),'<compressibility>').NE.0 ) THEN
!            CALL RDCHR(ISTART,ICOMMA,NRCH(6),CHDUM,RDUM(6))
!            CALL RDCHR(ISTART,ICOMMA,NRCH(7),CHDUM,RDUM(7))
!            GOTO 30
!          ELSEIF( INDEX(ADUM(1:),'<tortuosity option>').NE.0 ) THEN
!            CALL RDCHR(ISTART,ICOMMA,NRCH(8),CHDUM,RDUM(8))
!            GOTO 30
!          ELSEIF( INDEX(ADUM(1:),'<aqueous tortuosity factor>')
!     &      .NE.0 ) THEN
!            CALL RDCHR(ISTART,ICOMMA,NRCH(9),CHDUM,RDUM(9))
!            GOTO 30
!          ELSEIF( INDEX(ADUM(1:),'<gas tortuosity factor>').NE.0 ) THEN
!            CALL RDCHR(ISTART,ICOMMA,NRCH(10),CHDUM,RDUM(10))
!            GOTO 30
!          ELSEIF( INDEX(ADUM(1:),'<rock/soil>').NE.0 .OR.
!     &      INDEX(ADUM(1:),'<endcard>').NE.0 ) THEN
!            CHDUMX = CHDUM
!            CHDUM = ' '
!          ENDIF
!          ISX = 1
!          DO 40 NR = 1,10
!            IF( NRCH(NR).GT.0 ) THEN
!              CHDUM(ISX:ISX+NRCH(NR)) = RDUM(NR)(1:NRCH(NR)) // ','
!              ISX = ISX+NRCH(NR)+1
!            ELSE
!              CHDUM(ISX:ISX) = ','
!              ISX = ISX+1
!            ENDIF
!   40     CONTINUE
!          ISTART = 1
!        ENDIF
!!
!!---    Conventional formatting ---
!!
        VARB = 'Rock/Soil Name'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---  IJK, KIJ, or JKI indexing ---
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
          IF( ADUM .EQ. ROCK(M)) THEN
            IROCK = M
            GOTO 200
          ENDIF
  100   CONTINUE
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
  110     CONTINUE
          INDX = 2
          CHMSG = 'Unrecognized Rock/Soil Type or Scaling Group: '
     &      // ADUM(1:NCH)
          CALL WRMSGS( INDX )
          GOTO 10
        ENDIF
        INDX = 2
        CHMSG = 'Unrecognized Rock/Soil Type: ' // ADUM(1:NCH)
        CALL WRMSGS( INDX )
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
!---    Write rock/soil name  ---
!
        WRITE (IWR,'(/,2A)') 'Rock/Soil Name: ',ROCK(IROCK)
        N = N + 1
  220   CONTINUE
!
!---    Dual porosity ---
!
        IDPX = 0
        IF( INDEX(ADUM(1:),'fractured').NE.0 .OR.
     &    INDEX(ADUM(1:),'dp').NE.0 .OR.
     &    INDEX(ADUM(1:),'dual').NE.0 ) THEN
          IDPX = 1
          IF( INDEX(ADUM(1:),'pseudo').NE.0 ) IDPX = -1
        ENDIF
        IF( IOM.EQ.50 .OR. IOM.EQ.51 .OR. IOM.EQ.52 ) IDPX = 50
        IF( IJK.GT.0 ) THEN
          DO 230 IROCK = 1,NFLD
            IDP(IROCK) = IDPX
  230     CONTINUE
        ELSE
          IDP(IROCK) = IDPX
        ENDIF
!
!---    Read particle density ---
!
        VARB = 'Particle Density'
        UNTS = 'kg/m^3'
        IUNM = -3
        IUNKG = 1
        IF( IJK.GT.0 ) THEN
          CALL RDIJK( ISTART,IJK,CHDUM,UNTS,RHOS )
        ELSE
          IDFLT = 1
          CALL RDDPR(ISTART,ICOMMA,CHDUM,RHOS(IROCK))
          IDFLT = 1
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &      ': ',RHOS(IROCK)
          INDX = 0
          CALL RDUNIT(UNTS,RHOS(IROCK),INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',RHOS(IROCK),', kg/m^3)'
        ENDIF
!
!---    Read total and diffusive porosities ---
!
        IF( IDPX.EQ.50 ) THEN
          VARB = 'Matrix Porosity'
        ELSE
          VARB = 'Total Porosity'
        ENDIF
        UNTS = 'null'
        INDX = 1
        LNDX = 6
        IF( IJK.GT.0 ) THEN
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,POR,INDX,LNDX )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,POR(1,IROCK))
          WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',POR(1,IROCK)
        ENDIF
        IF( IDPX.EQ.50 ) THEN
          VARB = 'Fracture Porosity'
        ELSE
          VARB = 'Diffusive Porosity'
        ENDIF
        UNTS = 'null'
        INDX = 2
        IF( IJK.GT.0 ) THEN
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,POR,INDX,LNDX )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,POR(2,IROCK))
          WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',POR(2,IROCK)
        ENDIF
!
!---    Read fracture total and diffusive porosities 
!       for dual porosity soils
!
        IF( IDPX.NE.0 ) THEN
          IF( IDPX.EQ.50 ) THEN
            VARB = 'Fracture Density'
          ELSE
            VARB = 'Fracture Total Porosity'
          ENDIF
          UNTS = 'null'
          INDX = 3
          LNDX = 6
          IF( IJK.GT.0 ) THEN
            CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,POR,INDX,LNDX )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,POR(3,IROCK))
            WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',POR(3,IROCK)
          ENDIF
          IF( IDPX.EQ.50 ) THEN
            VARB = 'Fracture Aperture'
            UNTS = 'm'
            IUNM = 1
          ELSE
            VARB = 'Fracture Diffusive Porosity'
            UNTS = 'null'
          ENDIF
          INDX = 4
          LNDX = 6
          IF( IJK.GT.0 ) THEN
            CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,POR,INDX,LNDX )
          ELSE
            IF( IDPX.EQ.50 ) THEN
              CALL RDDPR(ISTART,ICOMMA,CHDUM,POR(4,IROCK))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',POR(4,IROCK)
              INDX = 0
              CALL RDUNIT(UNTS,POR(4,IROCK),INDX)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',POR(4,IROCK),', m)'
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,POR(4,IROCK))
              WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',POR(4,IROCK)
            ENDIF
          ENDIF
          IF( IDPX.EQ.50 ) THEN
            VARB = 'Fracture Absolute Roughness'
            UNTS = 'm'
            IUNM = 1
            INDX = 5
            LNDX = 6
            IF( IJK.GT.0 ) THEN
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,POR,INDX,LNDX )
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,POR(5,IROCK))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',POR(5,IROCK)
              INDX = 0
              CALL RDUNIT(UNTS,POR(5,IROCK),INDX)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',POR(5,IROCK),', m)'
            ENDIF
          ENDIF
        ENDIF
!
!---    Skip reads for compressibility for simulations 
!       with geomechanics  ---
!
        IF( ISLC(50).NE.0 ) THEN
          IF( IJK.GT.0 ) THEN
            DO 240 IROCK = 1,NFLD
              CMP(1,IROCK) = 0.D+0
              CMP(2,IROCK) = 0.D+0
  240       CONTINUE
          ELSE
            CMP(1,IROCK) = 0.D+0
            CMP(2,IROCK) = 0.D+0
          ENDIF
!
!---    Read specific storativity and convert to bulk compressibility,
!       or read bulk compressibility and reference pressure,
!       or read pore compressibility and reference pressure  ---
!
        ELSE
!
!---      Read compressibility model option  ---
!
          ISTARTX = ISTART
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
          IF( INDEX(BDUM(1:),'stiff grain').NE.0 ) THEN
            ISLC(15) = 10
            WRITE(IWR,'(2X,A)') 'Stiff Grain Model'
          ELSE
            ISLC(15) = 0
            WRITE(IWR,'(2X,A)') 'Fixed Bulk Volume Model'
          ENDIF
!
!---      Read compressibility input as compressibility  ---
!
          IF( INDEX(BDUM(1:),'compressibility').NE.0 ) THEN
!
!---        Read compressibility input as pore compressibility or
!           matrix pore compressibility ---
!
            IF( INDEX(BDUM(1:),'pore').NE.0 ) THEN
              ISLC(15) = ISLC(15) + 1
              IF( IDPX.EQ.0 ) THEN
                VARB = 'Pore Compressibility'
              ELSE
                VARB = 'Matrix Pore Compressibility'
              ENDIF
!
!---        Read compressibility input as bulk compressibility or
!           matrix bulk compressibility (OS,EC,DP) ---
!
            ELSE
              IF( IDPX.EQ.0 ) THEN
                VARB = 'Bulk Compressibility'
              ELSE
                VARB = 'Matrix Bulk Compressibility'
              ENDIF
            ENDIF
            UNTS = '1/pa'
            IUNM = 1
            IUNKG = -1
            IUNS = 2
            INDX = 1
            LNDX = 4
            IF( IJK.GT.0 ) THEN
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,CMP,INDX,LNDX )
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,CMP(1,IROCK))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',CMP(1,IROCK)
              INDX = 0
              CALL RDUNIT(UNTS,CMP(1,IROCK),INDX)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',CMP(1,IROCK),', 1/Pa)'
            ENDIF
!
!---        Read fracture compressibility ---
!
            IF( IDPX.NE.0 ) THEN
              IF( MOD(ISLC(15),10).EQ.1 ) THEN
                VARB = 'Fracture Pore Compressibility'
              ELSE
                VARB = 'Fracture Bulk Compressibility'
              ENDIF
              UNTS = '1/pa'
              IUNM = 1
              IUNKG = -1
              IUNS = 2
              INDX = 2
              LNDX = 4
              IF( IJK.GT.0 ) THEN
                CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,CMP,INDX,LNDX )
              ELSE
                CALL RDDPR(ISTART,ICOMMA,CHDUM,CMP(2,IROCK))
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &            UNTS(1:NCH),': ',CMP(2,IROCK)
                INDX = 0
                CALL RDUNIT(UNTS,CMP(2,IROCK),INDX)
                WRITE(IWR,'(A,1PE11.4,A)') ' (',CMP(2,IROCK),', 1/Pa)'
              ENDIF
            ENDIF
!
!---        Read reference pressure for compressibility ---
!
            VARB = 'Compressibility Reference Pressure'
            UNTS = 'pa'
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            INDX = 3
            LNDX = 4
            IF( IJK.GT.0 ) THEN
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,CMP,INDX,LNDX )
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,CMP(3,IROCK))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',CMP(3,IROCK)
              INDX = 0
              CALL RDUNIT(UNTS,CMP(3,IROCK),INDX)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',CMP(3,IROCK),', Pa)'
            ENDIF
!
!---      Read compressibility input as specific storage  ---
!
          ELSE
!
!---        Equivalent-continuum or dual-porosity model incompatible
!           with specific storage option  ---
!
            IF( ISLC(11).NE.0 ) THEN
              INDX = 4
              CHMSG = 'Specific Storage Option Incompatible with ' //
     &         'Equivalent-Continuum or Dual-Porosity Option'
              CALL WRMSGS( INDX )
            ENDIF
!
!---        Read specific storage or matrix specific storage (OS,EC,DP)
!           and convert to bulk compressibility or matrix bulk
!           compressibility  ---
!
            ISTART = ISTARTX
            IF( IDPX.EQ.0 ) THEN
              VARB = 'Specific Storage'
            ELSE
              VARB = 'Matrix Specific Storage'
            ENDIF
            UNTS = '1/m'
            IUNM = -1
            INDX = 1
            LNDX = 4
            IF( IJK.GT.0 ) THEN
              DO 300 IROCK = 1,NFLD
                PORDX = (1.D+0-POR(4,IROCK))*POR(2,IROCK)
                CMP(1,IROCK) = (CMP(1,IROCK)+PORDX*4.591D-10)*RHORL*GRAV
                IF( ISLC(9).EQ.1 ) CMP(1,IROCK) = CMP(1,IROCK)*RHORL*
     &            GRAV
  300         CONTINUE
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,CMP,INDX,LNDX )
              DO 310 IROCK = 1,NFLD
                CMP(1,IROCK) = MAX( CMP(1,IROCK)/(RHORL*GRAV)
     &            -PORDX*4.591D-10,ZERO )
                IF( ISLC(9).EQ.1 ) CMP(1,IROCK) = MAX( CMP(1,IROCK)/
     &            (RHORL*GRAV),ZERO )
  310         CONTINUE
            ELSE
              PORDX = (1.D+0-POR(4,IROCK))*POR(2,IROCK)
              VAR = (CMP(1,IROCK)+PORDX*4.591D-10)*RHORL*GRAV
              IF( ISLC(9).EQ.1 ) VAR = CMP(1,IROCK)*RHORL*GRAV
              IDFLT = 1
              CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',VAR
              INDX = 0
              IUNM = -1
              CALL RDUNIT(UNTS,VAR,INDX)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR,', 1/m)'
              CMP(1,IROCK) = MAX( VAR/(RHORL*GRAV)
     &          -PORDX*4.591D-10,ZERO )
              IF( ISLC(9).EQ.1 ) CMP(1,IROCK) =
     &          MAX( VAR/(RHORL*GRAV),0.D+0 )
              IF( IDPX.EQ.0 ) THEN
                WRITE(IWR,'(2X,A,1PE11.4)') 'Bulk Compressibility' //
     &            ', 1/Pa: ',CMP(1,IROCK)
              ELSE
                WRITE(IWR,'(2X,A,1PE11.4)') 'Matrix Bulk ' // 
     &            'Compressibility, 1/Pa: ',CMP(1,IROCK)
              ENDIF
            ENDIF
!
!---        Read fracture specific storage  ---
!
            IF( IDPX.NE.0 ) THEN
              VARB = 'Fracture Specific Storage'
              UNTS = '1/m'
              IUNM = -1
              INDX = 1
              LNDX = 4
              IF( IJK.GT.0 ) THEN
                DO 320 IROCK = 1,NFLD
                  PORDX = POR(4,IROCK)
                  CMP(2,IROCK) = (CMP(2,IROCK)+PORDX*4.591D-10)*RHORL*
     &              GRAV
                  IF( ISLC(9).EQ.1 ) CMP(2,IROCK) = CMP(2,IROCK)*RHORL*
     &              GRAV
  320           CONTINUE
                CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,CMP,INDX,LNDX )
                DO 330 IROCK = 1,NFLD
                  CMP(2,IROCK) = MAX( CMP(2,IROCK)/(RHORL*GRAV)
     &              -PORDX*4.591D-10,ZERO )
                  IF( ISLC(9).EQ.1 ) CMP(2,IROCK) = MAX( CMP(2,IROCK)/
     &              (RHORL*GRAV),ZERO )
  330           CONTINUE
              ELSE
                PORDX = POR(4,IROCK)
                VAR = (CMP(2,IROCK)+PORDX*4.591D-10)*RHORL*GRAV
                IF( ISLC(9).EQ.1 ) VAR = CMP(2,IROCK)*RHORL*GRAV
                IDFLT = 1
                CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &            UNTS(1:NCH),': ',VAR
                INDX = 0
                IUNM = -1
                CALL RDUNIT(UNTS,VAR,INDX)
                WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR,', 1/m)'
                CMP(2,IROCK) = MAX( VAR/(RHORL*GRAV)
     &            -PORDX*4.591D-10,ZERO )
                IF( ISLC(9).EQ.1 ) CMP(2,IROCK) =
     &            MAX( VAR/(RHORL*GRAV),0.D+0 )
                WRITE(IWR,'(2X,A,1PE11.4)') 'Fracture Bulk' //
     &            'Compressibility, 1/Pa: ',CMP(2,IROCK)
              ENDIF
            ENDIF
!
!---        Read compressibility reference pressure ---
!
            ISTARTX = ISTART
            CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
            CDUM = 'null'
            IF( INDX.EQ.1 ) CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,CDUM)
            IF( INDEX(CDUM(1:),'reference').NE.0 ) THEN
              VARB = 'Compressibility Reference Pressure'
              UNTS = 'pa'
              IUNM = -1
              IUNKG = 1
              IUNS = -2
              INDX = 3
              LNDX = 4
              IF( IJK.GT.0 ) THEN
                CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,CMP,INDX,LNDX )
              ELSE
                CALL RDDPR(ISTART,ICOMMA,CHDUM,CMP(3,IROCK))
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &            UNTS(1:NCH),': ',CMP(3,IROCK)
                INDX = 0
                CALL RDUNIT(UNTS,CMP(3,IROCK),INDX)
                WRITE(IWR,'(A,1PE11.4,A)') ' (',CMP(3,IROCK),', Pa)'
              ENDIF
            ELSE
              ISTART = ISTARTX
            ENDIF
          ENDIF
!
!---      Read fracturing pressure gradient  ---
!
          IF( IDPX.EQ.50 ) THEN
            VARB = 'Fracturing Pressure Gradient'
            UNTS = 'pa/m'
            IUNM = -2
            IUNKG = 1
            IUNS = -2
            INDX = 4
            LNDX = 4
            IF( IJK.GT.0 ) THEN
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,CMP,INDX,LNDX )
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,CMP(4,IROCK))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',CMP(4,IROCK)
              INDX = 0
              CALL RDUNIT(UNTS,CMP(4,IROCK),INDX)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',CMP(4,IROCK),', Pa/m)'
            ENDIF
          ENDIF
!
!---      Read matrix characteristic length ---
!
          IF( IDPX.EQ.-1 ) THEN
            VARB = 'Matrix Characteristic Length'
            UNTS = 'm'
            IUNM = 1
            IF( IJK.GT.0 ) THEN
              CALL RDIJK( ISTART,IJK,CHDUM,UNTS,CHML )
            ELSE
              IDFLT = 1
              CALL RDDPR(ISTART,ICOMMA,CHDUM,CHML(IROCK))
              IDFLT = 1
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',CHML(IROCK)
              INDX = 0
              CALL RDUNIT(UNTS,CHML(IROCK),INDX)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',CHML(IROCK),', m)'
            ENDIF
          ENDIF
        ENDIF
!
!---    Read aqueous, gas, and napl tortuosities if vapor diffusion or
!       the transport algorithms are active ---
!
        IF( ISLC(3) .EQ. 1 ) THEN
          VARB = 'Tortuosity Function'
          IDFLT = 1
          ADUM = 'millington and quirk'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          IF( INDEX(ADUM(1:),'constant-aqueous').NE.0 .AND.
     &      INDEX(ADUM(1:),'millington-gas').NE.0 ) THEN
            IF( IJK.GT.0 ) THEN
              DO 340 IROCK = 1,NFLD
                ITOR(IROCK) = 5
  340         CONTINUE
            ELSE
              ITOR(IROCK) = 5
            ENDIF
            WRITE(IWR,'(2X,A)') 'Constant Aqueous Tortuosity'
            VARB = 'Aqueous-Phase Tortuosity'
            UNTS = 'null'
            INDX = 1
            LNDX = 6
            IF( IJK.GT.0 ) THEN
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,TOR,INDX,LNDX )
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,TOR(1,IROCK))
              WRITE(IWR,'(4X,2A,1PE11.4)') VARB(1:IVR),': ',TOR(1,IROCK)
            ENDIF
            WRITE(IWR,'(2X,2A)') 'Millington and Quirk Gas Tortuosity '
          ELSEIF( INDEX(ADUM(1:),'constant') .NE. 0 ) THEN
            IF( IJK.GT.0 ) THEN
              DO 350 IROCK = 1,NFLD
                ITOR(IROCK) = 1
  350         CONTINUE
            ELSE
              ITOR(IROCK) = 1
            ENDIF
            WRITE(IWR,'(2X,A)') 'Constant Tortuosity Function'
            IF( IAQU.EQ.1 ) THEN
              VARB = 'Aqueous-Phase Tortuosity'
              UNTS = 'null'
              INDX = 1
              LNDX = 6
              IF( IJK.GT.0 ) THEN
                CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,TOR,INDX,LNDX )
              ELSE
                CALL RDDPR(ISTART,ICOMMA,CHDUM,TOR(1,IROCK))
                WRITE(IWR,'(4X,2A,1PE11.4)') VARB(1:IVR),': ',
     &            TOR(1,IROCK)
              ENDIF
            ENDIF
            IF( IGAS.EQ.1 ) THEN
              VARB = 'Gas-Phase Tortuosity'
              UNTS = 'null'
              INDX = 2
              LNDX = 6
              IF( IJK.GT.0 ) THEN
                CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,TOR,INDX,LNDX )
              ELSE
                CALL RDDPR(ISTART,ICOMMA,CHDUM,TOR(2,IROCK))
                WRITE(IWR,'(4X,2A,1PE11.4)') VARB(1:IVR),': ',
     &            TOR(2,IROCK)
              ENDIF
            ENDIF
            IF( INAPL.EQ.1 ) THEN
              VARB = 'NAPL Tortuosity'
              UNTS = 'null'
              INDX = 3
              LNDX = 6
              IF( IJK.GT.0 ) THEN
                CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,TOR,INDX,LNDX )
              ELSE
                CALL RDDPR(ISTART,ICOMMA,CHDUM,TOR(3,IROCK))
                WRITE(IWR,'(4X,2A,1PE11.4)')VARB(1:IVR),': ',
     &            TOR(3,IROCK)
              ENDIF
            ENDIF
          ELSEIF( INDEX(ADUM(1:),'free gas').NE.0 ) THEN
            IF( IJK.GT.0 ) THEN
              DO 360 IROCK = 1,NFLD
                ITOR(IROCK) = 3
  360         CONTINUE
            ELSE
              ITOR(IROCK) = 3
            ENDIF
            WRITE(IWR,'(2X,2A)') 'Millington and Quirk Tortuosity ',
     &        '(Free Gas) Function'
          ELSEIF( INDEX(ADUM(1:),'millington').NE.0 .OR.
     &            INDEX(ADUM(1:),'mq').NE.0 ) THEN
            IF( IJK.GT.0 ) THEN
              DO 370 IROCK = 1,NFLD
                ITOR(IROCK) = 2
  370         CONTINUE
            ELSE
              ITOR(IROCK) = 2
            ENDIF
            WRITE(IWR,'(2X,2A)') 'Millington and Quirk Tortuosity ',
     &        'Function'
          ELSEIF( INDEX(ADUM(1:),'marshall').NE.0 ) THEN
            IF( IJK.GT.0 ) THEN
              DO 380 IROCK = 1,NFLD
                ITOR(IROCK) = 4
  380         CONTINUE
            ELSE
              ITOR(IROCK) = 4
            ENDIF
            WRITE(IWR,'(2X,2A)') 'Marshall Tortuosity ',
     &        'Function'
          ELSE
            INDX = 4
            CHMSG = 'Unrecognized Tortuosity Function: ' // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
        ENDIF
!
!---    Loop over remaining rock/soils within scaling group  ---
!
        IF( ISLC(19).EQ.1 .AND. IROCK.LT.NROCK ) THEN
          DO 490 M = IROCK+1,NROCK
            IF( ISCALE(M).EQ.ISGRP ) THEN
              N = N+1
              RHOS(M) = RHOS(IROCK)
              CHML(M) = CHML(IROCK)
              ITOR(M) = ITOR(IROCK)
              IDP(M) = IDP(IROCK)
              DO 480 L = 1,6
                POR(L,M) = POR(L,IROCK)
  480         CONTINUE
              DO 482 L = 1,6
                TOR(L,M) = TOR(L,IROCK)
  482         CONTINUE
              DO 484 L = 1,3
                CMP(L,M) = CMP(L,IROCK)
  484         CONTINUE
            ENDIF
  490     CONTINUE
        ENDIF
!
!---    Read next rock/soil type or scaling group  ---
!
        IF( N.LT.NROCK ) WRITE(IWR,'(/)')
        GOTO 10
 500  CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDMECH group ---
!
      RETURN
      END

