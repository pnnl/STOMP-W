!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDLRP
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
!     Read input file for rock/soil aqueous relative permeability
!     function information.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, Battelle, PNL, December 1992.
!     Last Modified by MD White, Battelle, PNL, June 23, 1994.
!     $Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLCX(LRPLC)
      CHARACTER*4 FORM
      CHARACTER*64 ADUM,RDUM,UNTS
      CHARACTER*512 CHDUM

      INTEGER, DIMENSION(:), ALLOCATABLE :: IVAR



!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM
      DATA FORM /'(I9)'/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDLRP'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 

      ALLOCATE( IVAR(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IVAR'
        CALL WRMSGS( INDX )
      ENDIF

!
!---  Write card information to ouput file  ---
!
      CARD = 'Rock/Soil Aqueous Relative Permeability Function Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Loop over the rock/soil aqueous relative permeability
!     information lines  ---
!
      NR = 0
      IJK = 0
      ISGRP = 0
   10 CONTINUE
      IF( NR.GE.NROCK .OR. IJK.GT.0 ) GOTO 500
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      VARB = 'Rock/Soil Name'
   12 CONTINUE
!
!---  Rock/Soil option for IJK Indexing  ---
!
      IF( IJK.LT.0 ) THEN
        RDUM = 'Rock/Soil #'
        NR = NR + 1
        ICX = ICOUNT(NR)
        WRITE( FORM(3:3),'(I1)' ) ICX
        NCH = 12 + ICX - 1
        WRITE( RDUM(12:NCH),FORM ) NR
        WRITE (IWR,'(A)') RDUM(1:NCH)
        GOTO 220
!
!---  Read rock/soil name  ---
!
      ELSE
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,RDUM)
      ENDIF
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
        IROCK = 1
        GOTO 220
      ENDIF
!
!---  Search known rock types for a matching type ---
!
      DO 100 M = IROCK,NROCK
        IF( KBS.EQ.1 ) THEN
          IF( INDEX( ROCK(M)(1:),RDUM(IBS:JBS) ).GT.0 ) THEN
            IROCK = M
            GOTO 200
          ENDIF
        ELSE
          IF( RDUM.EQ.ROCK(M) ) THEN
            IROCK = M
            GOTO 200
          ENDIF
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
!
!---  Read aqueous relative permeability pressure function  ---
!
      NR = NR + 1
  220 CONTINUE
!
!---  Rock/Soil option for IJK Indexing, dissociate from
!     saturation function type  ---
!
      IF( IJK.LT.0 ) THEN
        ISCHRX = 0
      ELSE
        ISCHRX = MOD( ISCHR(IROCK),1000 )
      ENDIF
!
!---  Nonhysteretic saturation functions  ---
!
      IERR = 0
      IF( ISCHRX.LT.20 .OR. ISCHRX.GT.30 ) THEN
        VARB = 'Aqueous Relative Permeability Function'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---    Rock/Soil Zonation Option for IJK Indexing  ---
!
        IF( IJK.GT.0 .AND. INDEX(ADUM(1:),'rock/soil').NE.0 ) THEN
          VARB = 'Number of Rock/Soil Entries'
          CALL RDINT(ISTART,ICOMMA,CHDUM,NROCK)
          CALL RDIJKI( ISTART,IJK,CHDUM,IVAR )
          IJK = -IJK
          ISTART = 1
          CALL RDINPL( CHDUM )
          CALL LCASE( CHDUM )
          GOTO 12
        ENDIF
!
!---    Tabular (relative permeability versus liquid saturation)  ---
!
        IF( INDEX(ADUM(1:),'tabular').NE.0 ) THEN
          IF( INDEX( ADUM(1:),'head' ).NE.0 ) THEN
            IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
              WRITE(IWR,'(A)') 'Tabular Aqueous Relative Permeability '
     &          // 'Versus Log Capillary Head Function'
              IRPLX = 14
            ELSE
              WRITE(IWR,'(A)') 'Tabular Aqueous Relative Permeability '
     &          // 'Versus Capillary Head Function'
              IRPLX = 12
            ENDIF
          ELSE
            WRITE(IWR,'(A)') 'Tabular Aqueous Relative Permeability '
     &        // 'Versus Aqueous Saturation Function'
            IRPLX = 10
          ENDIF
          IF( INDEX( ADUM(1:),'spline' ).NE.0 ) THEN
            IRPLX = IRPLX + 1
            WRITE(IWR,'(A)') 'Cubic Spline Interpolation'
          ELSE
            WRITE(IWR,'(A)') 'Linear Interpolation'
          ENDIF
!
!---      IJK Indexing  ---
!
          IF( IJK.GT.0 ) THEN
            VARB = 'Number of Tables'
            CALL RDINT(ISTART,ICOMMA,CHDUM,NTABLX)
            IF( NTABLX.LT.1 ) THEN
              INDX = 4
              CHMSG = 'Invalid Number of Aqueous Relative ' // 
     &          'Permeability Tables'
              CALL WRMSGS( INDX )
            ENDIF
            CALL RDIJKI( ISTART,IJK,CHDUM,IVAR )
!
!---        Loop over aqueous relative permeability function tables  ---
!
            DO 280 NTX = 1,NTABLX
              CALL RDINPL( CHDUM )
              CALL LCASE( CHDUM )
              ISTART = 1
              VARB = 'Number of Table Entries'
              CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
              WRITE(IWR,'(2A,I6)') VARB(1:IVR),': ',NLIN
!
!---          Loop over lines in aqueous relative permeability 
!             function tables  ---
!
              NTBLX = NTBL+1
              DO 270 NL = 1,NLIN
                NTBL = NTBL + 1
                IF( NTBL.GT.LTBL ) THEN
                  INDX = 5
                  CHMSG = 'Number of Table Values > Parameter LTBL'
                  CALL WRMSGS( INDX )
                ENDIF
                ISTART = 1
                CALL RDINPL( CHDUM )
                CALL LCASE( CHDUM )
                IF( INDEX( ADUM(1:),'head' ).NE.0 ) THEN
                  IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
                    VARB = 'Log Capillary Head'
                  ELSE
                    VARB = 'Capillary Head'
                  ENDIF
                ELSE
                  VARB = 'Aqueous Saturation'
                ENDIF
!
!---            Correct table values for capillary-head units  ---
!
                IF( IRPLX.GE.12 .AND. IRPLX.LE.15 ) THEN
                  CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLX(NTBL))
                  CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                  WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &              UNTS(1:NCH),': ',TBLX(NTBL)
                  INDX = 0
                  IUNM = 1
                  VARX = 1.D+0
                  CALL RDUNIT(UNTS,VARX,INDX)
                  IF( IRPLX.GE.14 .AND. IRPLX.LE.15 ) THEN
                    TBLX(NTBL) = LOG( TBLX(NTBL)*VARX )
                  ELSE
                    TBLX(NTBL) = TBLX(NTBL)*VARX
                  ENDIF
                  WRITE(IWR,'(A,1PE11.4,A)') ' (',TBLX(NTBL),', m)'
                ELSE
                  CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLX(NTBL))
                  WRITE(IWR,'(4X,A,1PE11.4)') VARB,TBLX(NTBL)
                ENDIF
                VARB = 'Aqueous Relative Permeability'
                CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLY(NTBL))
                WRITE(IWR,'(4X,A,1PE11.4)') VARB,TBLY(NTBL)
                IF( NL.EQ.2 ) THEN
                  IF( TBLX(NTBL-1).LT.TBLX(NTBL) ) THEN
                    ITDX = 1
                  ELSEIF( TBLX(NTBL-1).GT.TBLX(NTBL) ) THEN
                    ITDX = -1
                  ELSE
                    INDX = 4
                    CHMSG = 'Invalid Aqueous Relative ' // 
     &                'Permeability Table'
                    CALL WRMSGS( INDX )
                  ENDIF
                ELSEIF( NL.GT.2 ) THEN
                  IF( (ITDX.EQ.1 .AND. TBLX(NTBL).LE.TBLX(NTBL-1)) .OR.
     &              (ITDX.EQ.-1 .AND. TBLX(NTBL).GE.TBLX(NTBL-1)) ) THEN
                    INDX = 4
                    CHMSG = 'Invalid Aqueous Relative ' // 
     &                'Permeability Table'
                    CALL WRMSGS( INDX )
                  ENDIF
                ENDIF
  270         CONTINUE
!
!---          Build cubic splines  ---
!
              IF( IRPLX.EQ.11 .OR. IRPLX.EQ.13 .OR. IRPLX.EQ.15 ) THEN
                CALL SPLINY( NTBLX,NTBL )
              ENDIF
!
!---          Correlate table numbers with nodes  ---
!
              DO 272 N = 1,NFLD
                IF( IVAR(N).EQ.NTX ) THEN
                  IRLTBL(1,N) = NTBLX
                  IRLTBL(2,N) = NTBL
                ELSEIF( IVAR(N).LT.1 .OR. IVAR(N).GT.NTABLX ) THEN
                  INDX = 4
                  CHMSG = 'Invalid Aqueous Relative Permeability ' //
     &              'Table Number'
                  CALL WRMSGS( INDX )
                ENDIF
 272          CONTINUE
 280        CONTINUE
!
!---      Rock/soil zonation  ---
!
          ELSE
            VARB = 'Number of Tabular Entries'
            CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
            WRITE(IWR,'(2X,2A,I6)') VARB,': ',NLIN
            IF( NLIN.LT.2 ) THEN
              INDX = 4
              CHMSG = 'Invalid Aqueous Relative Permeability Table'
              CALL WRMSGS( INDX )
            ENDIF
            IRLTBL(1,IROCK) = NTBL + 1
            DO 300 NL = 1,NLIN
              NTBL = NTBL + 1
              IF( NTBL.GT.LTBL ) THEN
                INDX = 5
                CHMSG = 'Number of Tables Values > Parameter LTBL'
                CALL WRMSGS( INDX )
              ENDIF
              ISTART = 1
              CALL RDINPL( CHDUM )
              CALL LCASE( CHDUM )
              IF( INDEX( ADUM(1:),'head' ).NE.0 ) THEN
                IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
                  VARB = 'Log Capillary Head'
                ELSE
                  VARB = 'Capillary Head'
                ENDIF
              ELSE
                VARB = 'Aqueous Saturation'
              ENDIF
!
!---          Correct table values for capillary-head units  ---
!
              IF( IRPLX.GE.12 .AND. IRPLX.LE.15 ) THEN
                CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLX(NTBL))
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &            UNTS(1:NCH),': ',TBLX(NTBL)
                INDX = 0
                IUNM = 1
                VARX = 1.D+0
                CALL RDUNIT(UNTS,VARX,INDX)
                IF( IRPLX.GE.14 .AND. IRPLX.LE.15 ) THEN
                  TBLX(NTBL) = LOG( TBLX(NTBL)*VARX )
                ELSE
                  TBLX(NTBL) = TBLX(NTBL)*VARX
                ENDIF
                WRITE(IWR,'(A,1PE11.4,A)') ' (',TBLX(NTBL),', m)'
              ELSE
                CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLX(NTBL))
                WRITE(IWR,'(4X,A,1PE11.4)') VARB,TBLX(NTBL)
              ENDIF
              VARB = 'Aqueous Relative Permeability'
              CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLY(NTBL))
              WRITE(IWR,'(4X,A,1PE11.4)') VARB,TBLY(NTBL)
              IF( NL.EQ.2 ) THEN
                IF( TBLX(NTBL-1).LT.TBLX(NTBL) ) THEN
                  ITDX = 1
                ELSEIF( TBLX(NTBL-1).GT.TBLX(NTBL) ) THEN
                  ITDX = -1
                ELSE
                  INDX = 4
                  CHMSG = 'Invalid Aqueous Relative ' // 
     &              'Permeability Table'
                  CALL WRMSGS( INDX )
                ENDIF
              ELSEIF( NL.GT.2 ) THEN
                IF( (ITDX.EQ.1 .AND. TBLX(NTBL).LE.TBLX(NTBL-1)) .OR.
     &            (ITDX.EQ.-1 .AND. TBLX(NTBL).GE.TBLX(NTBL-1)) ) THEN
                  INDX = 4
                  CHMSG = 'Invalid Aqueous Relative ' // 
     &              'Permeability Table'
                  CALL WRMSGS( INDX )
                ENDIF
              ENDIF
  300       CONTINUE
            IRLTBL(2,IROCK) = NTBL
            IF( IRPLX.EQ.11 .OR. IRPLX.EQ.13 .OR. IRPLX.EQ.15 ) THEN
              CALL SPLINY( IRLTBL(1,IROCK),IRLTBL(2,IROCK) )
            ENDIF
          ENDIF
          GOTO 460
        ENDIF
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHRX.EQ.1 .OR. ISCHRX.EQ.13 .OR.
     &    ISCHRX.EQ.15 .OR. ISCHRX.EQ.17 .OR.
     &    ISCHRX.EQ.101 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            INDX = 2
            VARB = 'Aqueous Relative Permeability'
            CALL C_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX )
          ELSEIF( INDEX(ADUM(1:),'gardner').NE.0 .OR.
     &      INDEX(ADUM(1:),'rijtema').NE.0 ) THEN
            IRPLX = 9
            CALL RG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND.
     &      INDEX(ADUM(1:),'modified').NE.0 ) THEN
            IF( IOM.NE.1 .AND. IOM.NE.3 .AND. IOM.NE.36 ) IERR = 1
            IRPLX = 22
            VARB = 'Modified-Mualem Porosity Distribution Model'
            CALL MMVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND.
     &      INDEX(ADUM(1:),'irreducible').NE.0 ) THEN
            IRPLX = 21
            VARB = 'Mualem-Irreducible Porosity Distribution Model'
            CALL MIVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Mualem Porosity Distribution Model'
            CALL MVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution Model'
            CALL BVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'free corey').NE.0 ) THEN
            IRPLX = 7
            CALL FC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'corey').NE.0 ) THEN
            IRPLX = 3
            WRITE (IWR,'(2X,A)')'Corey Relative Permeability Model'
          ELSEIF( INDEX(ADUM(1:),'fatt and klikoff').NE.0 ) THEN
            IRPLX = 4
            WRITE (IWR,'(2X,A)') 'Fatt and Klikoff Relative ' //
     &        'Permeability Function'
          ELSEIF( INDEX(ADUM(1:),'haverkamp').NE.0 ) THEN
            IRPLX = 5
            CALL HK_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'touma and vauclin').NE.0 ) THEN
            IRPLX = 6
            CALL TV_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'stone').NE.0 ) THEN
            IRPLX = 8
            CALL STN_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'polynomial').NE.0 ) THEN
            IRPLX = 19
            CALL PLY_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: '
     &        // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          IF( IERR.EQ.1 ) THEN
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: '
     &        // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          IF( INDEX(ADUM(1:),'polmann').NE.0 ) THEN
            IRPLX = IRPLX + 100
            CALL PA_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'pruess').NE.0 ) THEN
            IRPLX = IRPLX + 200
            CALL GPA_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND.
     &      INDEX(ADUM(1:),'anisotropy').NE.0 ) THEN
            IRPLX = IRPLX + 300
            INDX = 2
            CALL MAVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX )
          ENDIF
!
!---    Brooks and Corey entrapment or Modified Lenhard
!       saturation functions  ---
!
        ELSEIF( ISCHRX.EQ.6 .OR.
     &    (ISCHRX.GE.35 .AND. ISCHRX.LE.38) ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            INDX = 2
            VARB = 'Aqueous Relative Permeability'
            CALL C_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Mualem Porosity Distribution Model'
            CALL MBC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution Model'
            CALL BBC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'free corey').NE.0 ) THEN
            IRPLX = 7
            CALL FC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'stone').NE.0 ) THEN
            IRPLX = 8
            CALL STN_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: '
     &        // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    van Genuchten entrapment or Modified Lenhard
!       saturation functions  ---
!
        ELSEIF( ISCHRX.EQ.8 .OR.
     &    (ISCHRX.GE.31 .AND. ISCHRX.LE.34) ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            INDX = 2
            VARB = 'Aqueous Relative Permeability'
            CALL C_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Mualem Porosity Distribution Model'
            CALL MVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution Model'
            CALL BVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'free corey').NE.0 ) THEN
            IRPLX = 7
            CALL FC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'stone').NE.0 ) THEN
            IRPLX = 8
            CALL STN_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: '
     &        // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    van Genuchten triple curve function  ---
!
        ELSEIF( ISCHRX.EQ.301 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Aqueous Relative Permeability (main drainage)'
            INDX = 2
            CALL C_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX )
            VARB = 'Aqueous Relative Permeability (main wetting)'
            INDX = 1
            CALL C_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Mualem Porosity Distribution (main drainage)'
            CALL MVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
            INDX = 1
            JNDX = 5
            VARB = 'Mualem Porosity Distribution (main wetting)'
            CALL MVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution (main drainage)'
            CALL BVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
            INDX = 1
            JNDX = 5
            VARB = 'Burdine Porosity Distribution (main wetting)'
            CALL BVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: '
     &        // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    Brooks and Corey saturation functions  ---
!
        ELSEIF( ISCHRX.EQ.2 .OR.
     &    ISCHRX.EQ.14 .OR. ISCHRX.EQ.16 .OR.
     &    ISCHRX.EQ.18 .OR. ISCHRX.EQ.102 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Aqueous Relative Permeability'
            INDX = 2
            CALL C_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX )
          ELSEIF( INDEX(ADUM(1:),'gardner').NE.0 .OR.
     &      INDEX(ADUM(1:),'rijtema').NE.0 ) THEN
            IRPLX = 9
            CALL RG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND.
     &      INDEX(ADUM(1:),'modified').NE.0 ) THEN
            IF( IOM.NE.1 .AND. IOM.NE.3 .AND. IOM.NE.36 ) IERR = 1
            IRPLX = 22
            VARB = 'Modified-Mualem Porosity Distribution Model'
            CALL MMBC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND.
     &      INDEX(ADUM(1:),'irreducible').NE.0 ) THEN
            IRPLX = 21
            VARB = 'Mualem-Irreducible Porosity Distribution Model'
            CALL MIBC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Mualem Porosity Distribution Model'
            CALL MBC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution Model'
            CALL BBC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'free corey').NE.0 ) THEN
            IRPLX = 7
            CALL FC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'corey').NE.0 ) THEN
            IRPLX = 3
            WRITE (IWR,'(2X,A)')'Corey Relative Permeability Model'
          ELSEIF( INDEX(ADUM(1:),'fatt and klikoff').NE.0 ) THEN
            IRPLX = 4
            WRITE (IWR,'(2X,A)') 'Fatt and Klikoff Relative ' //
     &        'Permeability Function'
          ELSEIF( INDEX(ADUM(1:),'haverkamp').NE.0 ) THEN
            IRPLX = 5
            CALL HK_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'touma and vauclin').NE.0 ) THEN
            IRPLX = 6
            CALL TV_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'stone').NE.0 ) THEN
            IRPLX = 8
            CALL STN_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'polynomial').NE.0 ) THEN
            IRPLX = 19
            CALL PLY_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: '
     &        // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          IF( IERR.EQ.1 ) THEN
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: '
     &        // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          IF( INDEX(ADUM(1:),'polmann').NE.0 ) THEN
            IRPLX = IRPLX + 100
            CALL PA_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'pruess').NE.0 ) THEN
            IRPLX = IRPLX + 200
            CALL GPA_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND.
     &      INDEX(ADUM(1:),'anisotropy').NE.0 ) THEN
            IRPLX = IRPLX + 300
            INDX = 2
            CALL MABC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX )
          ENDIF
!
!---    Brooks and Corey triple curve  ---
!
        ELSEIF( ISCHRX.EQ.302 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Aqueous Relative Permeability (main drainage)'
            INDX = 2
            CALL C_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX )
            VARB = 'Aqueous Relative Permeability (main wetting)'
            INDX = 1
            CALL C_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Mualem Porosity Distribution (main drainage)'
            CALL MBC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
            INDX = 1
            JNDX = 5
            VARB = 'Mualem Porosity Distribution (main wetting)'
            CALL MBC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution (main drainage)'
            CALL BBC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
            INDX = 1
            JNDX = 5
            VARB = 'Burdine Porosity Distribution (main wetting)'
            CALL BBC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: '
     &        // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    Dual Porosity van Genuchten function  ---
!
        ELSEIF( ISCHRX.EQ.3 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Matrix Aqueous Relative Permeability'
            INDX = 2
            CALL C_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX )
            VARB = 'Fracture Aqueous Relative Permeability'
            INDX = 1
            CALL C_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND.
     &      INDEX(ADUM(1:),'linear fracture').NE.0 ) THEN
            IRPLX = 31
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Mualem Porosity Distribution Model'
            CALL MVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 .AND.
     &      INDEX(ADUM(1:),'linear fracture').NE.0 ) THEN
            IRPLX = 32
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Burdine Porosity Distribution Model'
            CALL BVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Mualem Porosity Distribution Model'
            CALL MVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
            INDX = 1
            JNDX = 6
            VARB = 'Fracture Mualem Porosity Distribution Model'
            CALL MVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Burdine Porosity Distribution Model'
            CALL BVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
            INDX = 1
            JNDX = 6
            VARB = 'Fracture Burdine Porosity Distribution Model'
            CALL BVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: '
     &        // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    Dual Porosity Brooks and Corey function  ---
!
        ELSEIF( ISCHRX.EQ.4 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Matrix Aqueous Relative Permeability'
            INDX = 2
            CALL C_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX )
            VARB = 'Fracture Aqueous Relative Permeability'
            INDX = 1
            CALL C_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND.
     &      INDEX(ADUM(1:),'linear fracture').NE.0 ) THEN
            IRPLX = 31
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Mualem Porosity Distribution Model'
            CALL MBC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 .AND.
     &      INDEX(ADUM(1:),'linear fracture').NE.0 ) THEN
            IRPLX = 32
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Burdine Porosity Distribution Model'
            CALL BBC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Mualem Porosity Distribution Model'
            CALL MBC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
            INDX = 1
            JNDX = 6
            VARB = 'Fracture Mualem Porosity Distribution Model'
            CALL MBC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Burdine Porosity Distribution Model'
            CALL BBC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
            INDX = 1
            JNDX = 6
            VARB = 'Fracture Burdine Porosity Distribution Model'
            CALL BBC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: '
     &        // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    Unknown saturation function  ---
!
        ELSE
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Aqueous Relative Permeability'
            INDX = 2
            CALL C_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX )
          ELSEIF( INDEX(ADUM(1:),'gardner').NE.0 .OR.
     &      INDEX(ADUM(1:),'rijtema').NE.0 ) THEN
            IRPLX = 9
            CALL RG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'free corey').NE.0 ) THEN
            IRPLX = 7
            CALL FC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'corey').NE.0 ) THEN
            IRPLX = 3
            WRITE (IWR,'(2X,A)')'Corey Relative Permeability Function'
          ELSEIF( INDEX(ADUM(1:),'fatt and klikoff').NE.0 ) THEN
            IRPLX = 4
            WRITE (IWR,'(2X,A)') 'Fatt and Klikoff Relative ' //
     &        'Permeability Function'
          ELSEIF( INDEX(ADUM(1:),'haverkamp').NE.0 ) THEN
            IRPLX = 5
            CALL HK_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'touma and vauclin').NE.0 ) THEN
            IRPLX = 6
            CALL TV_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'stone').NE.0 ) THEN
            IRPLX = 8
            CALL STN_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'polynomial').NE.0 ) THEN
            IRPLX = 19
            CALL PLY_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: '
     &        // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          IF( INDEX(ADUM(1:),'polmann').NE.0 ) THEN
            IRPLX = IRPLX + 100
            CALL PA_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ELSEIF( INDEX(ADUM(1:),'pruess').NE.0 ) THEN
            IRPLX = IRPLX + 200
            CALL GPA_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
          ENDIF
        ENDIF
!
!---  Hysteretic saturation functions  ---
!
      ELSE
        VARB = 'Porosity Distribution Model'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:), 'mualem').NE.0 ) THEN
          IRPLX = 1
          WRITE(IWR,'(2X,A)') 'Mualem Porosity Distribution Model'
        ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
          IRPLX = 2
          WRITE(IWR,'(2X,A)') 'Burdine Porosity Distribution Model'
        ELSE
          INDX = 4
          NCH = INDEX( ADUM(1:),'  ' )-1
          CHMSG = 'Unrecognized Relative Perm. Function: '
     &      // ADUM(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!
!---  Translate aqueous relative permeability type for IJK indexing  ---
!
  460 CONTINUE
      IF( IJK.GT.0 ) THEN
        DO 462 N = 1,NFLD
          IRPL(IZ(N)) = IRPLX
  462   CONTINUE
!
!---  For IJK indexing with the rock/soil option, correlate rock/soil 
!     numbers with nodes for aqueous relative permeability type and
!     parameters  ---
!
      ELSEIF( IJK.LT.0 ) THEN
        DO 472 N = 1,NFLD
          IF( IVAR(N).EQ.NR ) THEN
            DO 470 L = 1,LRPLC
              RPLC(L,N) = RPLCX(L)
  470       CONTINUE
            IRPL(N) = IRPLX
          ENDIF
  472   CONTINUE
!
!---  For rock/soil zonation input translate aqueous relative 
!     permeability type and parameters  ---
!
      ELSE
        IRPL(IROCK) = IRPLX
        DO 480 L = 1,LRPLC
          RPLC(L,IROCK) = RPLCX(L)
  480   CONTINUE
      ENDIF
!
!---  Loop over remaining rock/soils within scaling group  ---
!
      IF( ISLC(19).EQ.1 .AND. IROCK.LT.NROCK ) THEN
        DO 494 M = IROCK+1,NROCK
          IF( ISCALE(M).EQ.ISGRP ) THEN
            NR = NR + 1
            IRPL(M) = IRPL(IROCK)
            DO 490 L = 1,LRPLC
              RPLC(L,M) = RPLC(L,IROCK)
  490       CONTINUE
            DO 492 L = 1,2
              IRLTBL(L,M) = IRLTBL(L,IROCK)
  492       CONTINUE
          ENDIF
  494   CONTINUE
      ENDIF
!
!---  Read next rock/soil type or scaling group  ---
!
      IF( NR.LT.NROCK ) WRITE(IWR,'(/)')
!
!---  Continue reading rock/soil type names for a pattern match  ---
!
      IF( KBS.EQ.1 .AND. IROCK.LT.NROCK ) THEN
        IROCK = IROCK + 1
        ISTART = ISBS
        GOTO 20
      ENDIF
      GOTO 10
 500  CONTINUE

      DEALLOCATE( IVAR,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: IVAR'
        CALL WRMSGS( INDX )
      ENDIF

!
!---  End of RDLRP group ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE C_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX )
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
!     Constant aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 March 2002.
!     Last Modified by MD White, PNNL, 22 March 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
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
      REAL*8 RPLCX(LRPLC)
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/C_LRP'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      IF( IJK.GT.0 ) THEN
        UNTS = 'null'
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(INDX))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',RPLCX(INDX)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF C_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE FC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
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
!     Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 4 November 2002.
!     Last Modified by MD White, PNNL, 4 November 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
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
      REAL*8 RPLCX(LRPLC)
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/FC_LRP'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      WRITE (IWR,'(2X,A)')'Free Corey Relative Permeability ' //
     &  ' Function'
      VARB = 'Endpoint Aqueous Relative Permeability'
      IF( IJK.GT.0 ) THEN
        UNTS = 'null'
        INDX = 1
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(1))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLCX(1)
      ENDIF
      VARB = 'Exponent Aqueous Relative Permeability'
      IF( IJK.GT.0 ) THEN
        UNTS = 'null'
        INDX = 2
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(2))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLCX(2)
      ENDIF
      VARB = 'Residual Aqueous Saturation'
      IF( IJK.GT.0 ) THEN
        UNTS = 'null'
        INDX = 3
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(3))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLCX(3)
      ENDIF
      VARB = 'Residual Gas Saturation'
      IF( IJK.GT.0 ) THEN
        UNTS = 'null'
        INDX = 4
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(4))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLCX(4)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF FC_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE HK_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
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
!     Haverkamp aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 March 2002.
!     Last Modified by MD White, PNNL, 22 March 2002.
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLCX(LRPLC)
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/HK_LRP'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      WRITE (IWR,'(2X,A)')'Haverkamp Relative Permeability ' //
     &  ' Function'
      VARB = 'Haverkamp (A)'
      IF( IJK.GT.0 ) THEN
        UNTS = 'm'
        IUNM = 1
        INDX = 1
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
        INDX = 0
        IUNM = 1
        VARX = 1.D+0
        CALL RDUNIT(UNTS,VARX,INDX)
!
!---    IJK indexing  ---
!
        IF( IJK.EQ.1 ) THEN
          DO 100 K = 1,KFLD
          DO 100 J = 1,JFLD
          DO 100 I = 1,IFLD
            IROCK = (K-1)*IJFLD + (J-1)*IFLD + I
            RPLC(4,IROCK) = VARX
  100     CONTINUE
!
!---    JKI indexing  ---
!
        ELSEIF( IJK.EQ.2 ) THEN
          DO 200 I = 1,IFLD
          DO 200 K = 1,KFLD
          DO 200 J = 1,JFLD
            IROCK = (K-1)*IJFLD + (J-1)*IFLD + I
            RPLC(4,IROCK) = VARX
  200     CONTINUE
!
!---    KIJ indexing  ---
!
        ELSEIF( IJK.EQ.3 ) THEN
          DO 300 J = 1,JFLD
          DO 300 I = 1,IFLD
          DO 300 K = 1,KFLD
            IROCK = (K-1)*IJFLD + (J-1)*IFLD + I
            RPLC(4,IROCK) = VARX
  300     CONTINUE
        ENDIF
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(1))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(4X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &    UNTS(1:NCH),': ',RPLCX(1)
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,RPLCX(1),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',RPLCX(3),', m)'
        INDX = 0
        IUNM = 1
        RPLCX(4) = 1.D+0
        CALL RDUNIT(UNTS,RPLCX(4),INDX)
      ENDIF
      VARB = 'Haverkamp (gamma)'
      IF( IJK.GT.0 ) THEN
        UNTS = 'null'
        INDX = 2
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(2))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLCX(2)
      ENDIF
      VARB = 'Haverkamp: Effective Air Entry Head'
      IF( IJK.GT.0 ) THEN
        UNTS = 'm'
        IUNM = 1
        INDX = 3
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(3))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(4X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &    UNTS(1:NCH),': ',RPLCX(3)
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,RPLCX(3),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',RPLCX(3),', m)'
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF HK_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
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
!     Rijtema-Gardner aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 March 2002.
!     Last Modified by MD White, PNNL, 22 March 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
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
      REAL*8 RPLCX(LRPLC)
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RG_LRP'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      IDFLT = 1
      VARB = 'Rijtema-Gardner Modified Exponential Model'
      WRITE(IWR,'(2X,A)') VARB
      VARB = 'Rijtema-Gardner: a Parameter'
      IF( IJK.GT.0 ) THEN
        UNTS = '1/m'
        IUNM = -1
        INDX = 2
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(2))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(4X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &    UNTS(1:NCH),': ',RPLCX(2)
        INDX = 0
        IUNM = -1
        CALL RDUNIT(UNTS,RPLCX(2),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',RPLCX(2),', 1/m)'
      ENDIF
      VARB = 'Rijtema-Gardner: Effective Air Entry Head'
      IF( IJK.GT.0 ) THEN
        UNTS = 'm'
        IUNM = 1
        INDX = 1
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(1))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(4X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &    UNTS(1:NCH),': ',RPLCX(1)
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,RPLCX(1),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',RPLCX(1),', m)'
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF RG_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &  INDX,JNDX )
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
!     Mualem-van Genuchten aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 March 2002.
!     Last Modified by MD White, PNNL, 22 March 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLCX(LRPLC)
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/MVG_LRP'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      IDFLT = 1
      IF( IJK.GT.0 ) THEN
        DO 10 N = 1,NFLD
          RPLC(INDX,IZ(N)) = 1.D+0-1.D+0/SCHR(JNDX,IZ(N))
          IF( RPLC(INDX,IZ(N)).LT.EPSL ) THEN
            INDX = 4
            CHMSG = 'Negative van Genuchten ''m'' Parameter'
            CALL WRMSGS( INDX )
          ENDIF
   10   CONTINUE
        UNTS = 'null'
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        RPLCX(INDX) = 1.D+0-1.D+0/SCHR(JNDX,IROCK)
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(INDX))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF( ICOMMA.EQ.ISTX ) THEN
          WRITE(IWR,'(4X,A)') 'Default Value: m = 1 - 1/n'
        ENDIF
        WRITE(IWR,'(4X,A,1PE11.4)') 'm Parameter: ',RPLCX(INDX)
        IF( RPLCX(INDX).LT.EPSL ) THEN
          INDX = 4
          CHMSG = 'Negative van Genuchten ''m'' Parameter'
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF MVG_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MBC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &  INDX,JNDX )
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
!     Mualem-Brooks/Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 March 2002.
!     Last Modified by MD White, PNNL, 22 March 2002.
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLCX(LRPLC)
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/MBC_LRP'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      IDFLT = 1
      IF( IJK.GT.0 ) THEN
        DO 10 N = 1,NFLD
          RPLC(INDX,IZ(N)) = SCHR(JNDX,IZ(N))
   10   CONTINUE
        UNTS = 'null'
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        RPLCX(INDX) = SCHR(JNDX,IROCK)
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(INDX))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        WRITE(IWR,'(4X,A,1PE11.4)') 'Lambda Parameter: ',
     &    RPLCX(INDX)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF MBC_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE BVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &  INDX,JNDX )
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
!     Burdine-van Genuchten aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 March 2002.
!     Last Modified by MD White, PNNL, 22 March 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLCX(LRPLC)
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/BVG_LRP'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      IDFLT = 1
      IF( IJK.GT.0 ) THEN
        DO 10 N = 1,NFLD
          RPLC(INDX,IZ(N)) = 1.D+0-2.D+0/SCHR(JNDX,IZ(N))
          IF( RPLC(INDX,IZ(N)).LT.EPSL ) THEN
            INDX = 4
            CHMSG = 'Negative van Genuchten ''m'' Parameter'
            CALL WRMSGS( INDX )
          ENDIF
   10   CONTINUE
        UNTS = 'null'
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        RPLCX(INDX) = 1.D+0-2.D+0/SCHR(JNDX,IROCK)
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(INDX))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF( ICOMMA.EQ.ISTX ) THEN
          WRITE(IWR,'(4X,A)') 'Default Value: m = 1 - 2/n'
        ENDIF
        WRITE(IWR,'(4X,A,1PE11.4)') 'm Parameter: ',RPLCX(INDX)
        IF( RPLCX(INDX).LT.EPSL ) THEN
          INDX = 4
          CHMSG = 'Negative van Genuchten ''m'' Parameter'
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF BVG_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE BBC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &  INDX,JNDX )
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
!     Burdine-Brooks/Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 March 2002.
!     Last Modified by MD White, PNNL, 22 March 2002.
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLCX(LRPLC)
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/BBC_LRP'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      IDFLT = 1
      IF( IJK.GT.0 ) THEN
        DO 10 N = 1,NFLD
          RPLC(INDX,IZ(N)) = SCHR(JNDX,IZ(N))
   10   CONTINUE
        UNTS = 'null'
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        RPLCX(INDX) = SCHR(JNDX,IROCK)
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(INDX))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        WRITE(IWR,'(4X,A,1PE11.4)') 'Lambda Parameter: ',
     &    RPLCX(INDX)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF BBC_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE STN_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
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
!     Stone aqueous relative permeability.
!
!     Stone, H.L.  1970.  "Probability Model for Estimating Three-Phase
!     Relative Permeability."  Trans. SPE of AIME, 249:214-218.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 30 January 2007.
!     Last Modified by MD White, PNNL, 30 January 2007.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
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
      REAL*8 RPLCX(LRPLC)
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/STN_LRP'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      WRITE (IWR,'(2X,A)')'Stone Aqueous Relative Permeability ' //
     &  ' Function'
      VARB = 'Stone (Slr)'
      IF( IJK.GT.0 ) THEN
        UNTS = 'null'
        INDX = 1
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(1))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLCX(1)
      ENDIF
      VARB = 'Stone (n)'
      IF( IJK.GT.0 ) THEN 
        UNTS = 'null'
        INDX = 2
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(2))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLCX(2)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF STN_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE TV_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
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
!     Touma and Vauclin aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 March 2002.
!     Last Modified by MD White, PNNL, 22 March 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
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
      REAL*8 RPLCX(LRPLC)
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/TV_LRP'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      WRITE (IWR,'(2X,A)')'Touma and Vauclin Relative ' //
     &  'Permeability Function'
      VARB = 'Touma and Vauclin (alpha)'
      IF( IJK.GT.0 ) THEN
        UNTS = 'null'
        INDX = 1
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(1))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLCX(1)
      ENDIF
      VARB = 'Touma and Vauclin (beta)'
      IF( IJK.GT.0 ) THEN
        UNTS = 'null'
        INDX = 2
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(2))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLCX(2)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF TV_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE PA_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
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
!     Polmann anisotropy
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 March 2002.
!     Last Modified by MD White, PNNL, 22 March 2002.
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLCX(LRPLC)
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/PA_LRP'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      WRITE (IWR,'(/,2X,A)') 'w/ Polmann Anisotropy '
      VARB = 'Mean of ln(Ks), with Ks in cm/s'
      IF( IJK.GT.0 ) THEN
        UNTS = 'null'
        INDX = 5
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(5))
        WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLCX(5)
      ENDIF
      VARB = 'Variance of ln(Ks), with Ks in cm/s'
      IF( IJK.GT.0 ) THEN
        UNTS = 'null'
        INDX = 6
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(6))
        WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLCX(6)
      ENDIF
      VARB = 'Slope of the Beta versus ln(Ks) regression'
     &  // ' with Ks in cm/s'
      IF( IJK.GT.0 ) THEN
        UNTS = 'null'
        INDX = 7
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(7))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),', (1/cm): ',
     &    RPLCX(7)
      ENDIF
      VARB = 'Zeta (Ratio of Standard Deviation/Variance)'
     &  // ' with Ks in cm/s'
      IF( IJK.GT.0 ) THEN
        UNTS = 'null'
        INDX = 8
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(8))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),', (1/cm): ',
     &    RPLCX(8)
      ENDIF
      VARB = 'Vertical correlation lengths for ln(Ks)'
     &  // ' with Ks in cm/s'
      IF( IJK.GT.0 ) THEN
        UNTS = 'null'
        INDX = 9
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(9))
        WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),', (cm): ',
     &    RPLCX(9)
      ENDIF
      VARB = 'Mean slope, Beta, for ln(Ks) vs. Psi'
     &  // ' with Ks in cm/s'
      IF( IJK.GT.0 ) THEN
        UNTS = 'null'
        INDX = 10
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(10))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),', (1/cm): ',
     &    RPLCX(10)
      ENDIF
      IF( IJK.GT.0 ) THEN
        DO 10 N = 1,NFLD
          RPLC(11,IZ(N)) = 1.D+0
   10   CONTINUE
      ELSE
        RPLCX(11) = 1.D+0
      ENDIF
      CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
      IF( INDX.EQ.1 ) THEN
        VARB = 'Upper Anisotropy Ratio Limit'
        IF( IJK.GT.0 ) THEN
          UNTS = 'null'
          INDX = 11
          LNDX = LRPLC
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(11))
          WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &      RPLCX(11)
        ENDIF
      ENDIF
      IF( IJK.GT.0 ) THEN
        DO 20 N = 1,NFLD
          RPLC(12,IZ(N)) = 1.D+0
   20   CONTINUE
      ELSE
        RPLCX(12) = 1.D+0
      ENDIF
      CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
      IF( INDX.EQ.1 ) THEN
        VARB = 'Lower Anisotropy Ratio Limit'
        IF( IJK.GT.0 ) THEN
          UNTS = 'null'
          INDX = 12
          LNDX = LRPLC
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(12))
          WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &      RPLCX(12)
        ENDIF
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF PA_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE PLY_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
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
!     Polynomial aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 18 August 2003.
!     Last Modified by MD White, PNNL, 18 August 2003.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
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
      REAL*8 RPLCX(LRPLC)
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/PLY_LRP'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      WRITE (IWR,'(2X,A)')'Polynomial Aqueous Relative Permeability '
     &  // ' Function'
      VARB = 'Number of Polynomial Function Pieces'
      IF( IJK.GT.0 ) THEN
        INDX = 4
        CHMSG = 'IJK Indexing Not Available for Polynomial Functions'
        CALL WRMSGS( INDX )
      ENDIF
      CALL RDINT(ISTART,ICOMMA,CHDUM,NPLY_RL(IROCK))
      WRITE(IWR,'(2X,2A,I1)') VARB(1:IVR),': ',NPLY_RL(IROCK)
      IF( NPLY_RL(IROCK).GT.LPOLYN ) THEN
        INDX = 5
        CHMSG = 'Number of Aqueous Relative Permeability ' //
     &    'Polynomial Function Pieces > Parameter LPOLYN'
        CALL WRMSGS( INDX )
      ENDIF
      VARB = 'Saturated Hydraulic Conductivity'
      RPLCX(2) = 1.D+0
      IDFLT = 1
      CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(2))
      DO 60 NPX = 1,NPLY_RL(IROCK)
        VARB = 'Polynomial Piece #  : '
        WRITE(VARB(14:14),'(I1)') NPX
        ISTART = 1
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        VARB = 'Number of Polynomial Coefficients'
        CALL RDINT(ISTART,ICOMMA,CHDUM,NCOEF)
        IF( (NCOEF+4).GT.LPOLYC ) THEN
          INDX = 5
          CHMSG = 'Number of Aqueous Relative Permeability ' //
     &      'Polynomial Coefficients > Parameter LPOLYC'
          CALL WRMSGS( INDX )
        ENDIF
        VARB = 'Minimum Head for Polynomial Piece'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,CPLY_RL(1,NPX,IROCK))
        VARB = 'Maximum Head for Polynomial Piece'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,CPLY_RL(2,NPX,IROCK))
        CPLY_RL(3,NPX,IROCK) = 0.D+0
        CPLY_RL(4,NPX,IROCK) = 0.D+0
        DO 40 NCX = 5,NCOEF+4
          VARB = 'Coefficient for Polynomial Piece'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,CPLY_RL(NCX,NPX,IROCK))
!
!---      Maximum aqueous relative permeability for polynomial piece ---
!
          CPLY_RL(3,NPX,IROCK) = CPLY_RL(3,NPX,IROCK) + 
     &      CPLY_RL(NCX,NPX,IROCK)*(LOG10(CPLY_RL(1,NPX,IROCK))
     &      **(NCX-5))
!
!---      Mininum aqueous relative permeability for polynomial piece ---
!
          CPLY_RL(4,NPX,IROCK) = CPLY_RL(4,NPX,IROCK) + 
     &      CPLY_RL(NCX,NPX,IROCK)*(LOG10(CPLY_RL(2,NPX,IROCK))
     &      **(NCX-5))
   40   CONTINUE
        CPLY_RL(3,NPX,IROCK) = (1.D+1**CPLY_RL(3,NPX,IROCK))
     &    /RPLCX(2)
        CPLY_RL(4,NPX,IROCK) = (1.D+1**CPLY_RL(4,NPX,IROCK))
     &    /RPLCX(2)
        VARB = 'Head Units for Polynomial Piece'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,3A)') VARB(1:IVR),': ',UNTS
        RPLCX(1) = 1.D+0
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,RPLCX(1),INDX)
        WRITE(IWR,'(2X,3A,1PE11.4,$)')
     &    'Minimum Head for Polynomial Piece, ',UNTS(1:NCH),
     &    ': ',CPLY_RL(1,NPX,IROCK)
        VAR = CPLY_RL(1,NPX,IROCK)*RPLCX(1)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR,', m)'
        WRITE(IWR,'(2X,3A,1PE11.4,$)')
     &    'Maximum Head for Polynomial Piece, ',UNTS(1:NCH),
     &    ': ',CPLY_RL(2,NPX,IROCK)
        VAR = CPLY_RL(2,NPX,IROCK)*RPLCX(1)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR,', m)'
        DO  50 NCX = 5,NCOEF+4
          VARB = 'Coefficient #  : '
          WRITE(VARB(14:14),'(I1)') NCX-4
          WRITE(IWR,'(2X,A,1PE11.4)') VARB(1:17),
     &      CPLY_RL(NCX,NPX,IROCK)
   50   CONTINUE
   60   CONTINUE
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF PLY_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE GPA_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
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
!     Gompertz-Pruess anisotropy
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 March 2002.
!     Last Modified by MD White, PNNL, 22 March 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
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
      REAL*8 RPLCX(LRPLC)
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/GPA_LRP'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      VARB = 'Gompertz Function a Parameter'
      IF( IJK.GT.0 ) THEN
        UNTS = 'null'
        INDX = 5
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(5))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLCX(5)
      ENDIF
      VARB = 'Gompertz Function b Parameter'
      IF( IJK.GT.0 ) THEN
        UNTS = '1/m'
        IUNM = -1
        INDX = 6
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(6))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLCX(6)
      ENDIF
      VARB = 'Gompertz Function c Parameter'
      IF( IJK.GT.0 ) THEN
        UNTS = '1/m'
        IUNM = -1
        INDX = 7
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(7))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLCX(7)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF GPA_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MAVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX )
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
!     Mualem Anisotropy-van Genuchten aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 9 April 2002.
!     Last Modified by MD White, PNNL, 9 April 2002.
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLCX(LRPLC)
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/MAVG_LRP'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Read horizontal pore-scale parameter   ---
!
      IDFLT = 1
      INDC = INDX+1
      IF( IJK.GT.0 ) THEN
        DO 20 N = 1,NFLD
          RPLC(INDC,IZ(N)) = 0.5D+0
   20   CONTINUE
        UNTS = 'null'
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDC,LNDX )
      ELSE
        RPLCX(INDC) = 0.5D+0
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(INDC))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF( ICOMMA.EQ.ISTX ) THEN
          WRITE(IWR,'(4X,A)') 'Default Value: 1/2 (Square Root)'
        ENDIF
        WRITE(IWR,'(4X,A,1PE11.4)') 'Horizontal Pore-Scale Parameter: ',
     &    RPLCX(INDC)
      ENDIF
!
!---  Read vertical pore-scale parameter   ---
!
      IDFLT = 1
      INDC = INDX+2
      IF( IJK.GT.0 ) THEN
        DO 30 N = 1,NFLD
          RPLC(INDC,IZ(N)) = 0.5D+0
   30   CONTINUE
        UNTS = 'null'
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDC,LNDX )
      ELSE
        RPLCX(INDC) = 0.5D+0
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(INDC))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF( ICOMMA.EQ.ISTX ) THEN
          WRITE(IWR,'(4X,A)') 'Default Value: 1/2 (Square Root)'
        ENDIF
        WRITE(IWR,'(4X,A,1PE11.4)') 'Vertical Pore-Scale Parameter: ',
     &    RPLCX(INDC)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF MAVG_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MABC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX )
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
!     Mualem-Anisotropy Brooks/Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 9 April 2002.
!     Last Modified by MD White, PNNL, 9 April 2002.
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLCX(LRPLC)
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/MABC_LRP'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Read horizontal pore-scale parameter  ---
!
      IDFLT = 1
      INDC = INDX+1
      IF( IJK.GT.0 ) THEN
        DO 20 N = 1,NFLD
          RPLC(INDC,IZ(N)) = 0.5D+0
   20   CONTINUE
        UNTS = 'null'
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDC,LNDX )
      ELSE
        RPLCX(INDC) = 0.5D+0
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(INDC))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        WRITE(IWR,'(4X,A,1PE11.4)') 'Horizontal Pore-Scale Parameter: ',
     &    RPLC(INDC,IROCK)
      ENDIF
!
!---  Read vertical pore-scale parameter  ---
!
      IDFLT = 1
      INDC = INDX+2
      IF( IJK.GT.0 ) THEN
        DO 30 N = 1,NFLD
          RPLC(INDC,IZ(N)) = 0.5D+0
   30   CONTINUE
        UNTS = 'null'
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDC,LNDX )
      ELSE
        RPLCX(INDC) = 0.5D+0
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(INDC))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        WRITE(IWR,'(4X,A,1PE11.4)') 'Vertical Pore-Scale Parameter: ',
     &    RPLCX(INDC)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF MABC_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MIVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
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
!     Mualem Irreducible-van Genuchten aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 21 May 2002.
!     Last Modified by MD White, PNNL, 21 May 2002.
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLCX(LRPLC)
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/MIVG_LRP'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Read 'm' parameter  ---
!
      IDFLT = 1
      IF( IJK.GT.0 ) THEN
        DO 10 N = 1,NFLD
          RPLC(2,IZ(N)) = 1.D+0-1.D+0/SCHR(3,IZ(N))
   10   CONTINUE
        UNTS = 'null'
        INDX = 2
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        RPLCX(2) = 1.D+0-1.D+0/SCHR(3,IROCK)
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(2))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF( ICOMMA.EQ.ISTX ) THEN
          WRITE(IWR,'(4X,A)') 'Default Value: m = 1 - 1/n'
        ENDIF
        WRITE(IWR,'(4X,A,1PE11.4)') 'm Parameter: ',RPLCX(2)
      ENDIF
!
!---  Read irreducible saturation  ---
!
      IDFLT = 1
      IF( IJK.GT.0 ) THEN
        DO 20 N = 1,NFLD
          RPLC(1,IZ(N)) = SCHR(4,IZ(N))
   20   CONTINUE
        UNTS = 'null'
        INDX = 1
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        RPLCX(1) = SCHR(4,IROCK)
        IDFLT = 1
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(1))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        WRITE(IWR,'(4X,A,1PE11.4)') 'slr Parameter: ',RPLCX(1)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF MIVG_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MIBC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
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
!     Mualem Irreducible-Brooks/Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 21 May 2002.
!     Last Modified by MD White, PNNL, 21 May 2002.
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLCX(LRPLC)
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/MIBC_LRP'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Read 'm' parameter  ---
!
      IDFLT = 1
      IF( IJK.GT.0 ) THEN
        DO 10 N = 1,NFLD
          RPLC(2,IZ(N)) = SCHR(3,IZ(N))
   10   CONTINUE
        UNTS = 'null'
        INDX = 2
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        RPLCX(2) = SCHR(3,IROCK)
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(2))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        WRITE(IWR,'(4X,A,1PE11.4)') 'Lambda Parameter: ',
     &    RPLCX(2)
      ENDIF
!
!---  Read irreducible saturation  ---
!
      IDFLT = 1
      IF( IJK.GT.0 ) THEN
        DO 20 N = 1,NFLD
          RPLC(1,IZ(N)) = SCHR(4,IZ(N))
   20   CONTINUE
        UNTS = 'null'
        INDX = 1
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        RPLCX(1) = SCHR(4,IROCK)
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(1))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        WRITE(IWR,'(4X,A,1PE11.4)') 'slr Parameter: ',RPLCX(1)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF MIBC_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MMVG_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
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
!     Mualem Irreducible-van Genuchten aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 17 December 2002.
!     Last Modified by MD White, PNNL, 17 December 2002.
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLCX(LRPLC)
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/MMVG_LRP'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Read 'm' parameter  ---
!
      IDFLT = 1
      IF( IJK.GT.0 ) THEN
        DO 10 N = 1,NFLD
          RPLC(2,IZ(N)) = 1.D+0-1.D+0/SCHR(3,IZ(N))
   10   CONTINUE
        UNTS = 'null'
        INDX = 2
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        RPLCX(2) = 1.D+0-1.D+0/SCHR(3,IROCK)
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(2))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF( ICOMMA.EQ.ISTX ) THEN
          WRITE(IWR,'(4X,A)') 'Default Value: m = 1 - 1/n'
        ENDIF
        WRITE(IWR,'(4X,A,1PE11.4)') 'm Parameter: ',RPLCX(2)
      ENDIF
!
!---  Read pore-scale parameter  ---
!
      IDFLT = 1
      IF( IJK.GT.0 ) THEN
        DO 20 N = 1,NFLD
          RPLC(1,IZ(N)) = 5.D-1
   20   CONTINUE
        UNTS = 'null'
        INDX = 1
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        RPLCX(1) = 5.D-1
        IDFLT = 1
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(1))
        IF( ICOMMA.EQ.ISTX ) THEN
          WRITE(IWR,'(4X,A)') 'Default Value: 1/2 (Square Root)'
        ENDIF
        WRITE(IWR,'(4X,A,1PE11.4)') 'Pore-Scale Parameter: ',
     &    RPLCX(1)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF MMVG_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MMBC_LRP( RPLCX,ISTART,ICOMMA,IROCK,IJK,CHDUM )
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
!     Mualem Irreducible-Brooks/Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 17 December 2002.
!     Last Modified by MD White, PNNL, 17 December 2002.
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLCX(LRPLC)
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/MMBC_LRP'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Read 'm' parameter  ---
!
      IDFLT = 1
      IF( IJK.GT.0 ) THEN
        DO 10 N = 1,NFLD
          RPLC(2,IZ(N)) = SCHR(3,IZ(N))
   10   CONTINUE
        UNTS = 'null'
        INDX = 2
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        RPLCX(2) = SCHR(3,IROCK)
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(2))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        WRITE(IWR,'(4X,A,1PE11.4)') 'Lambda Parameter: ',
     &    RPLCX(2)
      ENDIF
!
!---  Read pore-scale parameter  ---
!
      IDFLT = 1
      IF( IJK.GT.0 ) THEN
        DO 20 N = 1,NFLD
          RPLC(1,IZ(N)) = 5.D-1
   20   CONTINUE
        UNTS = 'null'
        INDX = 1
        LNDX = LRPLC
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPLC,INDX,LNDX )
      ELSE
        RPLCX(1) = 5.D-1
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLCX(1))
        IF( ICOMMA.EQ.ISTX ) THEN
          WRITE(IWR,'(4X,A)') 'Default Value: 1/2 (Square Root)'
        ENDIF
        WRITE(IWR,'(4X,A,1PE11.4)') 'Pore-Scale Parameter: ',
     &    RPLCX(1)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF MMBC_LRP GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDLRPT( ITX )
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
!     Read input file for rock/soil aqueous
!     relative permeability tensor function information.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 August 2002.
!     Last Modified by MD White, PNNL, 1 August 2002.
!     $Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLTX(4,LRPL)
      CHARACTER*4 FORM
      CHARACTER*7 PREFIX
      CHARACTER*64 ADUM,RDUM,UNTS
      CHARACTER*512 CHDUM

      INTEGER, DIMENSION(:), ALLOCATABLE :: IVAR



!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM
      DATA FORM /'(I9)'/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDLRPT'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 

      ALLOCATE( IVAR(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IVAR'
        CALL WRMSGS( INDX )
      ENDIF

      IF( ITX.EQ.1 ) PREFIX = 'X-Dir. '
      IF( ITX.EQ.2 ) PREFIX = 'Y-Dir. '
      IF( ITX.EQ.3 ) PREFIX = 'Z-Dir. '
!
!---  Write card information to ouput file  ---
!
      IF( ITX.EQ.0 ) THEN
        CARD = 'Aqueous Relative Permeability Function Card'
      ELSE
        CARD = PREFIX // 'Aqueous Relative Permeability Function Card'
      ENDIF
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Loop over the rock/soil aqueous relative permeability
!     information lines  ---
!
      NR = 0
      IJK = 0
   10 CONTINUE
      IF( NR.GE.NROCK .OR. IJK.GT.0 ) GOTO 500
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      VARB = 'Rock/Soil Name'
   12 CONTINUE
!
!---  Rock/Soil option for IJK Indexing  ---
!
      IF( IJK.LT.0 ) THEN
        RDUM = 'Rock/Soil #'
        NR = NR + 1
        ICX = ICOUNT(NR)
        WRITE( FORM(3:3),'(I1)' ) ICX
        NCH = 12 + ICX - 1
        WRITE( RDUM(12:NCH),FORM ) NR
        WRITE (IWR,'(A)') RDUM(1:NCH)
        GOTO 220
!
!---  Read rock/soil name  ---
!
      ELSE
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,RDUM)
      ENDIF
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
        IROCK = 1
        GOTO 220
      ENDIF
!
!---  Search known rock types for a matching type ---
!
      DO 100 M = IROCK,NROCK
        IF( KBS.EQ.1 ) THEN
          IF( INDEX( ROCK(M)(1:),RDUM(IBS:JBS) ).GT.0 ) THEN
            IROCK = M
            GOTO 200
          ENDIF
        ELSE
          IF( RDUM.EQ.ROCK(M) ) THEN
            IROCK = M
            GOTO 200
          ENDIF
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
!
!---  Read aqueous relative permeability pressure function  ---
!
      NR = NR + 1
  220 CONTINUE
!
!---  Rock/Soil option for IJK Indexing, dissociate from
!     saturation function type  ---
!
      IF( IJK.LT.0 ) THEN
        ISCHRX = 0
      ELSE
        ISCHRX = MOD( ISCHR(IROCK),1000 )
      ENDIF
!
!---  Nonhysteretic saturation functions  ---
!
      IERR = 0
      IF( ISCHRX.LT.20 .OR. ISCHRX.GT.30 ) THEN
        IF( ITX.EQ.0 ) THEN
          VARB = 'Aqueous Relative Permeability Function'
        ELSE
          VARB = PREFIX // 'Aqueous Relative Permeability Function'
        ENDIF
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---    Rock/Soil Zonation Option for IJK Indexing  ---
!
        IF( IJK.GT.0 .AND. INDEX(ADUM(1:),'rock/soil').NE.0 ) THEN
          VARB = 'Number of Rock/Soil Entries'
          CALL RDINT(ISTART,ICOMMA,CHDUM,NROCK)
          CALL RDIJKI( ISTART,IJK,CHDUM,IVAR )
          IJK = -IJK
          ISTART = 1
          CALL RDINPL( CHDUM )
          CALL LCASE( CHDUM )
          GOTO 12
        ENDIF
!
!---    Tabular (relative permeability versus liquid saturation)  ---
!
        IF( INDEX(ADUM(1:),'tabular').NE.0 ) THEN
          IF( INDEX( ADUM(1:),'head' ).NE.0 ) THEN
            IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
              WRITE(IWR,'(A)') 'Tabular Aqueous Relative Permeability '
     &        // 'Versus Log Capillary Head Function'
              IRPLX = 14
            ELSE
              WRITE(IWR,'(A)') 'Tabular Aqueous Relative Permeability '
     &        // 'Versus Capillary Head Function'
              IRPLX = 12
            ENDIF
          ELSE
            WRITE(IWR,'(A)') 'Tabular Aqueous Relative Permeability '
     &        // 'Versus Aqueous Saturation Function'
            IRPLX = 10
          ENDIF
          IF( INDEX( ADUM(1:),'spline' ).NE.0 ) THEN
            IRPLX = IRPLX+1
            WRITE(IWR,'(A)') 'Cubic Spline Interpolation'
          ELSE
            WRITE(IWR,'(A)') 'Linear Interpolation'
          ENDIF
!
!---      IJK Indexing  ---
!
          IF( IJK.GT.0 ) THEN
            VARB = 'Number of Tables'
            CALL RDINT(ISTART,ICOMMA,CHDUM,NTABLX)
            IF( NTABLX.LT.1 ) THEN
              INDX = 4
              CHMSG = 'Invalid Number of Aqueous Relative ' // 
     &          'Permeability Tables'
              CALL WRMSGS( INDX )
            ENDIF
            CALL RDIJKI( ISTART,IJK,CHDUM,IVAR )
!
!---        Loop over aqueous relative permeability function tables  ---
!
            DO 280 NTX = 1,NTABLX
              CALL RDINPL( CHDUM )
              CALL LCASE( CHDUM )
              ISTART = 1
              VARB = 'Number of Table Entries'
              CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
              WRITE(IWR,'(2A,I6)') VARB(1:IVR),': ',NLIN
!
!---          Loop over lines in aqueous relative permeability 
!             function tables  ---
!
              NTBLX = NTBL+1
              DO 270 NL = 1,NLIN
                NTBL = NTBL + 1
                IF( NTBL.GT.LTBL ) THEN
                  INDX = 5
                  CHMSG = 'Number of Table Values > Parameter LTBL'
                  CALL WRMSGS( INDX )
                ENDIF
                ISTART = 1
                CALL RDINPL( CHDUM )
                CALL LCASE( CHDUM )
                IF( INDEX( ADUM(1:),'head' ).NE.0 ) THEN
                  IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
                    VARB = 'Log Capillary Head'
                  ELSE
                    VARB = 'Capillary Head'
                  ENDIF
                ELSE
                  VARB = 'Aqueous Saturation'
                ENDIF
!
!---            Correct table values for capillary-head units  ---
!
                IF( IRPLX.GE.12 .AND. IRPLX.LE.15 ) THEN
                  CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLX(NTBL))
                  CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                  WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &              UNTS(1:NCH),': ',TBLX(NTBL)
                  INDX = 0
                  IUNM = 1
                  VARX = 1.D+0
                  CALL RDUNIT(UNTS,VARX,INDX)
                  IF( IRPLX.GE.14 .AND. IRPLX.LE.15 ) THEN
                    TBLX(NTBL) = LOG( TBLX(NTBL)*VARX )
                  ELSE
                    TBLX(NTBL) = TBLX(NTBL)*VARX
                  ENDIF
                  WRITE(IWR,'(A,1PE11.4,A)') ' (',TBLX(NTBL),', m)'
                ELSE
                  CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLX(NTBL))
                  WRITE(IWR,'(4X,A,1PE11.4)') VARB,TBLX(NTBL)
                ENDIF
                VARB = 'Aqueous Relative Permeability'
                CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLY(NTBL))
                WRITE(IWR,'(4X,A,1PE11.4)') VARB,TBLY(NTBL)
                IF( NL.EQ.2 ) THEN
                  IF( TBLX(NTBL-1).LT.TBLX(NTBL) ) THEN
                    ITDX = 1
                  ELSEIF( TBLX(NTBL-1).GT.TBLX(NTBL) ) THEN
                    ITDX = -1
                  ELSE
                    INDX = 4
                    CHMSG = 'Invalid Aqueous Relative ' // 
     &                'Permeability Table'
                    CALL WRMSGS( INDX )
                  ENDIF
                ELSEIF( NL.GT.2 ) THEN
                  IF( (ITDX.EQ.1 .AND. TBLX(NTBL).LE.TBLX(NTBL-1)) .OR.
     &              (ITDX.EQ.-1 .AND. TBLX(NTBL).GE.TBLX(NTBL-1)) ) THEN
                    INDX = 4
                    CHMSG = 'Invalid Aqueous Relative ' // 
     &                'Permeability Table'
                    CALL WRMSGS( INDX )
                  ENDIF
                ENDIF
  270         CONTINUE
!
!---          Build cubic splines  ---
!
              IF( IRPLX.EQ.11 .OR. IRPLX.EQ.13 .OR. IRPLX.EQ.15 ) THEN
                CALL SPLINY( NTBLX,NTBL )
              ENDIF
!
!---          Correlate table numbers with nodes  ---
!
              DO 272 N = 1,NFLD
                IF( IVAR(N).EQ.NTX ) THEN
                  IF( ITX.EQ.0 ) THEN
                    IRLTBLT(1,N,1) = NTBLX
                    IRLTBLT(2,N,1) = NTBL
                    IRLTBLT(1,N,2) = NTBLX
                    IRLTBLT(2,N,2) = NTBL
                    IRLTBLT(1,N,3) = NTBLX
                    IRLTBLT(2,N,3) = NTBL
                  ELSE
                    IRLTBLT(1,N,ITX) = NTBLX
                    IRLTBLT(2,N,ITX) = NTBL
                  ENDIF
                ELSEIF( IVAR(N).LT.1 .OR. IVAR(N).GT.NTABLX ) THEN
                  INDX = 4
                  CHMSG = 'Invalid Aqueous Relative Permeability ' //
     &              'Table Number'
                  CALL WRMSGS( INDX )
                ENDIF
 272          CONTINUE
 280        CONTINUE
!
!---      Rock/soil zonation  ---
!
          ELSE
            VARB = 'Number of Tabular Entries'
            CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
            WRITE(IWR,'(2X,2A,I6)') VARB,': ',NLIN
            IF( NLIN.LT.2 ) THEN
              INDX = 4
              CHMSG = 'Invalid Aqueous Relative Permeability Table'
              CALL WRMSGS( INDX )
            ENDIF
            IF( ITX.EQ.0 ) THEN
              IRLTBLT(1,IROCK,1) = NTBL + 1
              IRLTBLT(1,IROCK,2) = NTBL + 1
              IRLTBLT(1,IROCK,3) = NTBL + 1
            ELSE
              IRLTBLT(1,IROCK,ITX) = NTBL + 1
            ENDIF
            DO 300 NL = 1,NLIN
              NTBL = NTBL + 1
              IF( NTBL.GT.LTBL ) THEN
                INDX = 5
                CHMSG = 'Number of Tables Values > Parameter LTBL'
                CALL WRMSGS( INDX )
              ENDIF
              ISTART = 1
              CALL RDINPL( CHDUM )
              CALL LCASE( CHDUM )
              IF( INDEX( ADUM(1:),'head' ).NE.0 ) THEN
                IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
                  VARB = 'Log Capillary Head'
                ELSE
                  VARB = 'Capillary Head'
                ENDIF
              ELSE
                VARB = 'Aqueous Saturation'
              ENDIF
!
!---          Correct table values for capillary-head units  ---
!
              IF( IRPLX.GE.12 .AND. IRPLX.LE.15 ) THEN
                CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLX(NTBL))
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &            UNTS(1:NCH),': ',TBLX(NTBL)
                INDX = 0
                IUNM = 1
                VARX = 1.D+0
                CALL RDUNIT(UNTS,VARX,INDX)
                IF( IRPLX.GE.14 .AND. IRPLX.LE.15 ) THEN
                  TBLX(NTBL) = LOG( TBLX(NTBL)*VARX )
                ELSE
                  TBLX(NTBL) = TBLX(NTBL)*VARX
                ENDIF
                WRITE(IWR,'(A,1PE11.4,A)') ' (',TBLX(NTBL),', m)'
              ELSE
                CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLX(NTBL))
                WRITE(IWR,'(4X,A,1PE11.4)') VARB,TBLX(NTBL)
              ENDIF
              VARB = 'Aqueous Relative Permeability'
              CALL RDDPR(ISTART,ICOMMA,CHDUM,TBLY(NTBL))
              WRITE(IWR,'(4X,A,1PE11.4)') VARB,TBLY(NTBL)
              IF( NL.EQ.2 ) THEN
                IF( TBLX(NTBL-1).LT.TBLX(NTBL) ) THEN
                  ITDX = 1
                ELSEIF( TBLX(NTBL-1).GT.TBLX(NTBL) ) THEN
                  ITDX = -1
                ELSE
                  INDX = 4
                  CHMSG = 'Invalid Aqueous Relative Permeability Table'
                  CALL WRMSGS( INDX )
                ENDIF
              ELSEIF( NL.GT.2 ) THEN
                IF( (ITDX.EQ.1 .AND. TBLX(NTBL).LE.TBLX(NTBL-1)) .OR.
     &            (ITDX.EQ.-1 .AND. TBLX(NTBL).GE.TBLX(NTBL-1)) ) THEN
                  INDX = 4
                  CHMSG = 'Invalid Aqueous Relative Permeability Table'
                  CALL WRMSGS( INDX )
                ENDIF
              ENDIF
  300       CONTINUE
            IF( ITX.EQ.0 ) THEN
              IRLTBLT(2,IROCK,1) = NTBL
              IRLTBLT(2,IROCK,2) = NTBL
              IRLTBLT(2,IROCK,3) = NTBL
              IF( IRPLX.EQ.11 .OR. IRPLX.EQ.13 .OR. IRPLX.EQ.15 ) THEN
                CALL SPLINY( IRLTBLT(1,IROCK,1),IRLTBLT(2,IROCK,1) )
              ENDIF
            ELSE
              IRLTBLT(2,IROCK,ITX) = NTBL
              IF( IRPLX.EQ.11 .OR. IRPLX.EQ.13 .OR. IRPLX.EQ.15 ) THEN
                CALL SPLINY( IRLTBLT(1,IROCK,ITX),IRLTBLT(2,IROCK,ITX) )
              ENDIF
            ENDIF
          ENDIF
          GOTO 460
        ENDIF
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHRX.EQ.1 .OR. ISCHRX.EQ.13 .OR.
     &    ISCHRX.EQ.15 .OR. ISCHRX.EQ.17 .OR.
     &    ISCHRX.EQ.101 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            INDX = 1
            VARB = VARB(1:7) // 'Aqueous Relative Permeability'
            CALL C_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'gardner').NE.0 .OR.
     &      INDEX(ADUM(1:),'rijtema').NE.0 ) THEN
            IRPLX = 9
            CALL RG_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND.
     &      INDEX(ADUM(1:),'modified').NE.0 ) THEN
            IF( IOM.NE.1 .AND. IOM.NE.3 .AND. IOM.NE.36 ) IERR = 1
            IRPLX = 22
            VARB = 'Modified-Mualem Porosity Distribution Model'
            CALL MMVG_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND.
     &      INDEX(ADUM(1:),'irreducible').NE.0 ) THEN
            IRPLX = 21
            VARB = 'Mualem-Irreducible Porosity Distribution Model'
            CALL MIVG_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Mualem Porosity Distribution Model'
            CALL MVG_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution Model'
            CALL BVG_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'free corey').NE.0 ) THEN
            IRPLX = 7
            WRITE (IWR,'(2X,A)')'Free Corey Relative Permeability Model'
          ELSEIF( INDEX(ADUM(1:),'corey').NE.0 ) THEN
            IRPLX = 3
            CALL FC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'fatt and klikoff').NE.0 ) THEN
            IRPLX = 4
            WRITE (IWR,'(2X,A)') 'Fatt and Klikoff Relative ' //
     &        'Permeability Function'
          ELSEIF( INDEX(ADUM(1:),'haverkamp').NE.0 ) THEN
            IRPLX = 5
            CALL HK_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'touma and vauclin').NE.0 ) THEN
            IRPLX = 6
            CALL TV_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: '
     &        // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          IF( IERR.EQ.1 ) THEN
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: '
     &        // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    van Genuchten entrapment or Modified Lenhard
!       saturation functions  ---
!
        ELSEIF( ISCHRX.EQ.8 .OR.
     &    (ISCHRX.GE.31 .AND. ISCHRX.LE.34) ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            INDX = 2
            VARB = 'Aqueous Relative Permeability'
            CALL C_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Mualem Porosity Distribution Model'
            CALL MVG_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution Model'
            CALL BVG_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX,ITX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: '
     &        // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    Brooks and Corey saturation function  ---
!
        ELSEIF( ISCHRX.EQ.2 .OR. ISCHRX.EQ.6 .OR.
     &    ISCHRX.EQ.14 .OR. ISCHRX.EQ.16 .OR.
     &    ISCHRX.EQ.18 .OR. ISCHRX.EQ.102 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Aqueous Relative Permeability'
            INDX = 1
            CALL C_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'gardner').NE.0 .OR.
     &      INDEX(ADUM(1:),'rijtema').NE.0 ) THEN
            IRPLX = 9
            CALL RG_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND.
     &      INDEX(ADUM(1:),'modified').NE.0 ) THEN
            IF( IOM.NE.1 .AND. IOM.NE.3 .AND. IOM.NE.36 ) IERR = 1
            IRPLX = 22
            VARB = 'Modified-Mualem Porosity Distribution Model'
            CALL MMBC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 .AND.
     &      INDEX(ADUM(1:),'irreducible').NE.0 ) THEN
            IRPLX = 21
            VARB = 'Mualem-Irreducible Porosity Distribution Model'
            CALL MIBC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 1
            JNDX = 3
            VARB = 'Mualem Porosity Distribution Model'
            CALL MBC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution Model'
            CALL BBC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'free corey').NE.0 ) THEN
            IRPLX = 7
            CALL FC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'corey').NE.0 ) THEN
            IRPLX = 3
            WRITE (IWR,'(2X,A)')'Corey Relative Permeability Model'
          ELSEIF( INDEX(ADUM(1:),'fatt and klikoff').NE.0 ) THEN
            IRPLX = 4
            WRITE (IWR,'(2X,A)') 'Fatt and Klikoff Relative ' //
     &        'Permeability Function'
          ELSEIF( INDEX(ADUM(1:),'haverkamp').NE.0 ) THEN
            IRPLX = 5
            CALL HK_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'touma and vauclin').NE.0 ) THEN
            IRPLX = 6
            CALL TV_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: '
     &        // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          IF( IERR.EQ.1 ) THEN
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: '
     &        // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    Brooks and Corey entrapment or Modified Lenhard
!       saturation functions  ---
!
        ELSEIF( ISCHRX.EQ.6 .OR.
     &    (ISCHRX.GE.35 .AND. ISCHRX.LE.38) ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Aqueous Relative Permeability'
            INDX = 1
            CALL C_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 1
            JNDX = 3
            VARB = 'Mualem Porosity Distribution Model'
            CALL MBC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution Model'
            CALL BBC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX,ITX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: '
     &        // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    Brooks and Corey w/ hysteresis saturation function  ---
!
        ELSEIF( ISCHRX.EQ.302 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Aqueous Relative Permeability (main drainage)'
            INDX = 1
            CALL C_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX,ITX )
            VARB = 'Aqueous Relative Permeability (main wetting)'
            INDX = 2
            CALL C_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'gardner').NE.0 .OR.
     &      INDEX(ADUM(1:),'rijtema').NE.0 ) THEN
            IRPLX = 9
            CALL RG_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 1
            JNDX = 3
            VARB = 'Mualem Porosity Distribution (main drainage)'
            CALL MBC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX,ITX )
            INDX = 2
            JNDX = 5
            VARB = 'Mualem Porosity Distribution (main wetting)'
            CALL MBC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Burdine Porosity Distribution (main drainage)'
            CALL BBC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX,ITX )
            INDX = 1
            JNDX = 5
            VARB = 'Burdine Porosity Distribution (main wetting)'
            CALL BBC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX,ITX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: '
     &        // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    Dual Porosity van Genuchten function  ---
!
        ELSEIF( ISCHRX.EQ.3 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Matrix Aqueous Relative Permeability'
            INDX = 1
            CALL C_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX,ITX )
            VARB = 'Fracture Aqueous Relative Permeability'
            INDX = 2
            CALL C_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Mualem Porosity Distribution Model'
            CALL MVG_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX,ITX )
            INDX = 1
            JNDX = 6
            VARB = 'Fracture Mualem Porosity Distribution Model'
            CALL MVG_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Burdine Porosity Distribution Model'
            CALL BVG_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX,ITX )
            INDX = 1
            JNDX = 6
            VARB = 'Fracture Burdine Porosity Distribution Model'
            CALL BVG_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX,ITX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: '
     &        // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    Dual Porosity Brooks and Corey function  ---
!
        ELSEIF( ISCHRX.EQ.4 ) THEN
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Matrix Aqueous Relative Permeability'
            INDX = 1
            CALL C_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX,ITX )
            VARB = 'Fracture Aqueous Relative Permeability'
            INDX = 2
            CALL C_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'mualem').NE.0 ) THEN
            IRPLX = 1
            INDX = 1
            JNDX = 3
            VARB = 'Matrix Mualem Porosity Distribution Model'
            CALL MBC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX,ITX )
            INDX = 2
            JNDX = 6
            VARB = 'Fracture Mualem Porosity Distribution Model'
            CALL MBC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
            IRPLX = 2
            INDX = 2
            JNDX = 3
            VARB = 'Matrix Burdine Porosity Distribution Model'
            CALL BBC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX,ITX )
            INDX = 1
            JNDX = 6
            VARB = 'Fracture Burdine Porosity Distribution Model'
            CALL BBC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &        INDX,JNDX,ITX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: '
     &        // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
!
!---    Unknown saturation function  ---
!
        ELSE
          IF( INDEX(ADUM(1:),'constant').NE.0 ) THEN
            IRPLX = 0
            VARB = 'Aqueous Relative Permeability'
            INDX = 2
            CALL C_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX,ITX )
          ELSEIF( INDEX(ADUM(1:),'gardner').NE.0 .OR.
     &      INDEX(ADUM(1:),'rijtema').NE.0 ) THEN
            IRPLX = 9
            CALL RG_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'free corey').NE.0 ) THEN
            IRPLX = 7
            CALL FC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'corey').NE.0 ) THEN
            IRPLX = 3
            WRITE (IWR,'(2X,A)')'Corey Relative Permeability Function'
          ELSEIF( INDEX(ADUM(1:),'fatt and klikoff').NE.0 ) THEN
            IRPLX = 4
            WRITE (IWR,'(2X,A)') 'Fatt and Klikoff Relative ' //
     &        'Permeability Function'
          ELSEIF( INDEX(ADUM(1:),'haverkamp').NE.0 ) THEN
            IRPLX = 5
            CALL HK_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
          ELSEIF( INDEX(ADUM(1:),'touma and vauclin').NE.0 ) THEN
            IRPLX = 6
            CALL TV_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
          ELSE
            INDX = 4
            NCH = INDEX( ADUM(1:),'  ' )-1
            CHMSG = 'Unrecognized Relative Perm. Function: '
     &        // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
        ENDIF
!
!---  Hysteretic saturation functions  ---
!
      ELSE
        IF( ITX.EQ.0 ) THEN
          VARB = 'Porosity Distribution Model'
        ELSE
          VARB = PREFIX // 'Porosity Distribution Model'
        ENDIF
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:), 'mualem').NE.0 ) THEN
          IRPLX = 1
          WRITE(IWR,'(2X,A)') 'Mualem Porosity Distribution Model'
        ELSEIF( INDEX(ADUM(1:),'burdine').NE.0 ) THEN
          IRPLX = 2
          WRITE(IWR,'(2X,A)') 'Burdine Porosity Distribution Model'
        ELSE
          INDX = 4
          NCH = INDEX( ADUM(1:),'  ' )-1
          CHMSG = 'Unrecognized Relative Perm. Function: '
     &      // ADUM(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!
!---  Translate aqueous relative permeability type for IJK indexing  ---
!
  460 CONTINUE
      IF( IJK.GT.0 ) THEN
        IF( ITX.EQ.0 ) THEN
          DO 462 N = 1,NFLD
            IRPLT(1,IZ(N)) = IRPLX
            IRPLT(2,IZ(N)) = IRPLX
            IRPLT(3,IZ(N)) = IRPLX
  462     CONTINUE
        ELSE
          DO 464 N = 1,NFLD
            IRPLT(ITX,IZ(N)) = IRPLX
  464     CONTINUE
        ENDIF
!
!---  For IJK indexing with the rock/soil option, correlate rock/soil 
!     numbers with nodes for aqueous relative permeability type and
!     parameters  ---
!
      ELSEIF( IJK.LT.0 ) THEN
        IF( ITX.EQ.0 ) THEN
          DO 468 N = 1,NFLD
            IF( IVAR(N).EQ.NR ) THEN
              DO 466 L = 1,LRPL
                RPLT(1,L,N) = RPLTX(1,L)
                RPLT(2,L,N) = RPLTX(2,L)
                RPLT(3,L,N) = RPLTX(3,L)
  466         CONTINUE
              IRPLT(1,N) = IRPLX
              IRPLT(2,N) = IRPLX
              IRPLT(3,N) = IRPLX
            ENDIF
  468     CONTINUE
        ELSE
          DO 472 N = 1,NFLD
            IF( IVAR(N).EQ.N ) THEN
              DO 470 L = 1,LRPL
                RPLT(ITX,L,N) = RPLTX(ITX,L)
  470         CONTINUE
              IRPLT(ITX,N) = IRPLX
            ENDIF
  472     CONTINUE
        ENDIF
!
!---  For rock/soil zonation input translate aqueous relative 
!     permeability type and parameters  ---
!
      ELSE
        IF( ITX.EQ.0 ) THEN
          DO 474 L = 1,LRPL
            RPLT(1,L,N) = RPLTX(1,L)
            RPLT(2,L,N) = RPLTX(2,L)
            RPLT(3,L,N) = RPLTX(3,L)
  474     CONTINUE
          IRPLT(1,IROCK) = IRPLX
          IRPLT(2,IROCK) = IRPLX
          IRPLT(3,IROCK) = IRPLX
        ELSE
          DO 476 L = 1,LRPL
            RPLT(ITX,L,N) = RPLTX(ITX,L)
  476     CONTINUE
          IRPLT(ITX,IROCK) = IRPLX
        ENDIF
      ENDIF
!
!---  Loop over remaining rock/soils within scaling group  ---
!
      IF( ISLC(19).EQ.1 .AND. IROCK.LT.NROCK ) THEN
        DO 490 M = IROCK+1,NROCK
          IF( ISCALE(M).EQ.ISGRP ) THEN
            NR = NR + 1
            IF( ITX.EQ.0 ) THEN
              IRPLT(1,M) = IRPLT(1,IROCK)
              IRPLT(2,M) = IRPLT(2,IROCK)
              IRPLT(3,M) = IRPLT(3,IROCK)
              DO 480 L = 1,LRPL
                RPLT(1,L,M) = RPLT(1,L,IROCK)
                RPLT(2,L,M) = RPLT(2,L,IROCK)
                RPLT(3,L,M) = RPLT(3,L,IROCK)
  480         CONTINUE
              DO 482 L = 1,2
                IRLTBLT(L,M,1) = IRLTBLT(L,IROCK,1)
                IRLTBLT(L,M,2) = IRLTBLT(L,IROCK,2)
                IRLTBLT(L,M,3) = IRLTBLT(L,IROCK,3)
  482         CONTINUE
            ELSE
              IRPLT(ITX,M) = IRPLT(ITX,IROCK)
              DO 484 L = 1,LRPL
                RPLT(ITX,L,M) = RPLT(ITX,L,IROCK)
  484         CONTINUE
              DO 486 L = 1,2
                IRLTBLT(L,M,ITX) = IRLTBLT(L,IROCK,ITX)
  486         CONTINUE
            ENDIF
          ENDIF
  490   CONTINUE
      ENDIF
!
!---  Read next rock/soil type or scaling group  ---
!
      IF( NR.LT.NROCK ) WRITE(IWR,'(/)')
!
!---  Continue reading rock/soil type names for a pattern match  ---
!
      IF( KBS.EQ.1 .AND. IROCK.LT.NROCK ) THEN
        IROCK = IROCK + 1
        ISTART = ISBS
        GOTO 20
      ENDIF
      GOTO 10
 500  CONTINUE

      DEALLOCATE( IVAR,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: IVAR'
        CALL WRMSGS( INDX )
      ENDIF

!
!---  End of RDLRPT group ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE C_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,INDX,ITX )
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
!     Constant aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 August 2002.
!     Last Modified by MD White, PNNL, 1 August 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
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
      REAL*8 RPLTX(4,LRPL)
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: RPX
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/C_LRPT'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      IF( IJK.GT.0 ) THEN
        ALLOCATE( RPX(1:LRPL,1:LFD),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
        UNTS = 'null'
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
        DO N = 1,LFD
          RPLT(ITX,INDX,N) = RPX(INDX,N)
        ENDDO
        DEALLOCATE( RPX,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
      ELSE
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,INDX))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLTX(ITX,INDX)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF C_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE FC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
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
!     Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 4 November 2002.
!     Last Modified by MD White, PNNL, 4 November 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
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
      REAL*8 RPLTX(4,LRPL)
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: RPX
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/FC_LRPT'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      WRITE (IWR,'(2X,A)')'Free Corey Relative Permeability ' //
     &  ' Function'
      IF( IJK.GT.0 ) THEN
        ALLOCATE( RPX(1:LRPL,1:LFD),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
        VARB = 'Endpoint Aqueous Relative Permeability'
        UNTS = 'null'
        INDX = 1
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
        VARB = 'Exponent Aqueous Relative Permeability'
        UNTS = 'null'
        INDX = 2
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
        VARB = 'Residual Aqueous Saturation'
        UNTS = 'null'
        INDX = 3
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
        VARB = 'Residual Gas Saturation'
        UNTS = 'null'
        INDX = 4
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
        DO N = 1,LFD
          DO M = 1,4
            RPLT(ITX,M,N) = RPX(M,N)
          ENDDO
        ENDDO
        DEALLOCATE( RPX,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
      ELSE
        VARB = 'Endpoint Aqueous Relative Permeability'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,1))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLTX(ITX,1)
        VARB = 'Exponent Aqueous Relative Permeability'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,2))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLTX(ITX,2)
        VARB = 'Residual Aqueous Saturation'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,3))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLTX(ITX,3)
        VARB = 'Residual Gas Saturation'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,4))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLTX(ITX,4)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF FC_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE HK_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
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
!     Haverkamp aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 August 2002.
!     Last Modified by MD White, PNNL, 1 August 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
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
      REAL*8 RPLTX(4,LRPL)
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: RPX
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/HK_LRPT'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      WRITE (IWR,'(2X,A)')'Haverkamp Relative Permeability ' //
     &  ' Function'
      IF( IJK.GT.0 ) THEN
        ALLOCATE( RPX(1:LRPL,1:LFD),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
        VARB = 'Haverkamp (A)'
        UNTS = 'null'
        INDX = 1
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
        VARB = 'Haverkamp (gamma)'
        UNTS = 'null'
        INDX = 2
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
        VARB = 'Haverkamp: Effective Air Entry Head'
        UNTS = 'm'
        IUNM = 1
        INDX = 3
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
        DO N = 1,LFD
          DO M = 1,3
            RPLT(ITX,M,N) = RPX(M,N)
          ENDDO
        ENDDO
        DEALLOCATE( RPX,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
      ELSE
        VARB = 'Haverkamp (A)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,1))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLTX(ITX,1)
        VARB = 'Haverkamp (gamma)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,2))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLTX(ITX,2)
        VARB = 'Haverkamp: Effective Air Entry Head'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,3))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(4X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &    UNTS(1:NCH),': ',RPLTX(ITX,3)
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,RPLTX(ITX,3),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',RPLTX(ITX,3),', m)'
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF HK_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RG_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
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
!     Rijtema-Gardner aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 August 2002.
!     Last Modified by MD White, PNNL, 1 August 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
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
      REAL*8 RPLTX(4,LRPL)
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: RPX
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RG_LRPT'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      IDFLT = 1
      VARB = 'Rijtema-Gardner Modified Exponential Model'
      WRITE(IWR,'(2X,A)') VARB
      IF( IJK.GT.0 ) THEN
        ALLOCATE( RPX(1:LRPL,1:LFD),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
        VARB = 'Rijtema-Gardner: a Parameter'
        UNTS = '1/m'
        IUNM = -1
        INDX = 1
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
        VARB = 'Rijtema-Gardner: Effective Air Entry Head'
        UNTS = 'm'
        IUNM = 1
        INDX = 2
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
        DO N = 1,LFD
          DO M = 1,2
            RPLT(ITX,M,N) = RPX(M,N)
          ENDDO
        ENDDO
        DEALLOCATE( RPX,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
      ELSE
      VARB = 'Rijtema-Gardner: a Parameter'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,1))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(4X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &    UNTS(1:NCH),': ',RPLTX(ITX,1)
        INDX = 0
        IUNM = -1
        CALL RDUNIT(UNTS,RPLTX(ITX,1),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',RPLTX(ITX,1),', 1/m)'
      VARB = 'Rijtema-Gardner: Effective Air Entry Head'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,2))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(4X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &    UNTS(1:NCH),': ',RPLTX(ITX,2)
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,RPLTX(ITX,2),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',RPLTX(ITX,2),', m)'
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF RG_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MVG_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &  INDX,JNDX,ITX )
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
!     Mualem-van Genuchten aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 August 2002.
!     Last Modified by MD White, PNNL, 1 August 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLTX(4,LRPL)
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: RPX
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/MVG_LRPT'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      IDFLT = 1
      IF( IJK.GT.0 ) THEN
        ALLOCATE( RPX(1:LRPL,1:LFD),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
        DO 10 N = 1,NFLD
          RPX(INDX,IZ(N)) = 1.D+0-1.D+0/SCHR(JNDX,IZ(N))
          IF( RPX(INDX,IZ(N)).LT.EPSL ) THEN
            INDX = 4
            CHMSG = 'Negative van Genuchten ''m'' Parameter'
            CALL WRMSGS( INDX )
          ENDIF
   10   CONTINUE
        UNTS = 'null'
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
        DO N = 1,LFD
          RPLT(ITX,INDX,N) = RPX(INDX,N)
        ENDDO
        DEALLOCATE( RPX,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
      ELSE
        RPLTX(ITX,INDX) = 1.D+0-1.D+0/SCHR(JNDX,IROCK)
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,INDX))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF( ICOMMA.EQ.ISTX ) THEN
          WRITE(IWR,'(4X,A)') 'Default Value: m = 1 - 1/n'
        ENDIF
        WRITE(IWR,'(4X,A,1PE11.4)') 'm Parameter: ',RPLTX(ITX,INDX)
        IF( RPLTX(ITX,INDX).LT.EPSL ) THEN
          INDX = 4
          CHMSG = 'Negative van Genuchten ''m'' Parameter'
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF MVG_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MBC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &  INDX,JNDX,ITX )
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
!     Mualem-Brooks/Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 August 2002.
!     Last Modified by MD White, PNNL, 1 August 2002.
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLTX(4,LRPL)
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: RPX
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/MBC_LRPT'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      IDFLT = 1
      IF( IJK.GT.0 ) THEN
        ALLOCATE( RPX(1:LRPL,1:LFD),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
        DO 10 N = 1,NFLD
          RPX(INDX,IZ(N)) = SCHR(JNDX,IZ(N))
   10   CONTINUE
        UNTS = 'null'
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
        DO N = 1,LFD
          RPLT(ITX,INDX,N) = RPX(INDX,N)
        ENDDO
        DEALLOCATE( RPX,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
      ELSE
        RPLTX(ITX,INDX) = SCHR(JNDX,IROCK)
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,INDX))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        WRITE(IWR,'(4X,A,1PE11.4)') 'Lambda Parameter: ',
     &    RPLTX(ITX,INDX)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF MBC_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE BVG_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &  INDX,JNDX,ITX )
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
!     Burdine-van Genuchten aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 August 2002.
!     Last Modified by MD White, PNNL, 1 August 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLTX(4,LRPL)
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: RPX
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/BVG_LRPT'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      IDFLT = 1
      IF( IJK.GT.0 ) THEN
        ALLOCATE( RPX(1:LRPL,1:LFD),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
        DO 10 N = 1,NFLD
          RPX(INDX,IZ(N)) = 1.D+0-2.D+0/SCHR(JNDX,IZ(N))
          IF( RPX(INDX,IZ(N)).LT.EPSL ) THEN
            INDX = 4
            CHMSG = 'Negative van Genuchten ''m'' Parameter'
            CALL WRMSGS( INDX )
          ENDIF
   10   CONTINUE
        UNTS = 'null'
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
        DO N = 1,LFD
          RPLT(ITX,INDX,N) = RPX(INDX,N)
        ENDDO
        DEALLOCATE( RPX,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
      ELSE
        RPLTX(ITX,INDX) = 1.D+0-2.D+0/SCHR(JNDX,IROCK)
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,INDX))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF( ICOMMA.EQ.ISTX ) THEN
          WRITE(IWR,'(4X,A)') 'Default Value: m = 1 - 2/n'
        ENDIF
        WRITE(IWR,'(4X,A,1PE11.4)') 'm Parameter: ',RPLTX(ITX,INDX)
        IF( RPLTX(ITX,INDX).LT.EPSL ) THEN
          INDX = 4
          CHMSG = 'Negative van Genuchten ''m'' Parameter'
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF BVG_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE BBC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,
     &  INDX,JNDX,ITX )
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
!     Burdine-Brooks/Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 August 2002.
!     Last Modified by MD White, PNNL, 1 August 2002.
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLTX(4,LRPL)
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: RPX
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/BBC_LRPT'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      IDFLT = 1
      IF( IJK.GT.0 ) THEN
        ALLOCATE( RPX(1:LRPL,1:LFD),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
        DO 10 N = 1,NFLD
          RPX(INDX,IZ(N)) = SCHR(JNDX,IZ(N))
   10   CONTINUE
        UNTS = 'null'
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
        DO N = 1,LFD
          RPLT(ITX,INDX,N) = RPX(INDX,N)
        ENDDO
        DEALLOCATE( RPX,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
      ELSE
        RPLTX(ITX,INDX) = SCHR(JNDX,IROCK)
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,INDX))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        WRITE(IWR,'(4X,A,1PE11.4)') 'Lambda Parameter: ',
     &    RPLTX(ITX,INDX)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF BBC_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE TV_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
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
!     Touma and Vauclin aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 August 2002.
!     Last Modified by MD White, PNNL, 1 August 2002.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
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
      REAL*8 RPLTX(4,LRPL)
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: RPX
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/TV_LRPT'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      WRITE (IWR,'(2X,A)')'Touma and Vauclin Relative ' //
     &  'Permeability Function'
      IF( IJK.GT.0 ) THEN
        ALLOCATE( RPX(1:LRPL,1:LFD),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
        VARB = 'Touma and Vauclin (alpha)'
        UNTS = 'null'
        INDX = 1
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
        VARB = 'Touma and Vauclin (beta)'
        UNTS = 'null'
        INDX = 2
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
        DO N = 1,LFD
          DO M = 1,2
            RPLT(ITX,M,N) = RPX(M,N)
          ENDDO
        ENDDO
        DEALLOCATE( RPX,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
      ELSE
        VARB = 'Touma and Vauclin (alpha)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,1))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLTX(ITX,1)
        VARB = 'Touma and Vauclin (beta)'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,2))
        WRITE (IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &    RPLTX(ITX,2)
      ENDIF
      IF( IJK.GT.0 ) THEN
      ELSE
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF TV_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MIVG_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
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
!     Mualem Irreducible-van Genuchten aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 21 May 2002.
!     Last Modified by MD White, PNNL, 21 May 2002.
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLTX(4,LRPL)
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: RPX
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/MIVG_LRPT'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      IDFLT = 1
      IF( IJK.GT.0 ) THEN
!
!---  Read 'm' parameter  ---
!
        ALLOCATE( RPX(1:LRPL,1:LFD),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
        DO 10 N = 1,NFLD
          RPX(1,IZ(N)) = 1.D+0-1.D+0/SCHR(3,IZ(N))
   10   CONTINUE
        UNTS = 'null'
        INDX = 1
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
!
!---  Read irreducible saturation  ---
!
        DO 20 N = 1,NFLD
          RPX(2,IZ(N)) = SCHR(4,IZ(N))
   20   CONTINUE
        UNTS = 'null'
        INDX = 2
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
        DO N = 1,LFD
          DO M = 1,2
            RPLT(ITX,M,N) = RPX(M,N)
          ENDDO
        ENDDO
        DEALLOCATE( RPX,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
      ELSE
!
!---  Read 'm' parameter  ---
!
        RPLTX(ITX,1) = 1.D+0-1.D+0/SCHR(3,IROCK)
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,1))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF( ICOMMA.EQ.ISTX ) THEN
          WRITE(IWR,'(4X,A)') 'Default Value: m = 1 - 1/n'
        ENDIF
        WRITE(IWR,'(4X,A,1PE11.4)') 'm Parameter: ',RPLTX(ITX,1)
!
!---  Read irreducible saturation  ---
!
        RPLTX(ITX,2) = SCHR(4,IROCK)
        IDFLT = 1
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,2))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        WRITE(IWR,'(4X,A,1PE11.4)') 'slr Parameter: ',RPLTX(ITX,2)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF MIVG_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MIBC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
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
!     Mualem Irreducible-Brooks/Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 21 May 2002.
!     Last Modified by MD White, PNNL, 21 May 2002.
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLTX(4,LRPL)
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: RPX
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/MIBC_LRPT'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      IDFLT = 1
      IF( IJK.GT.0 ) THEN
        ALLOCATE( RPX(1:LRPL,1:LFD),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
!
!---  Read 'm' parameter  ---
!
        DO 10 N = 1,NFLD
          RPX(1,IZ(N)) = SCHR(3,IZ(N))
   10   CONTINUE
        UNTS = 'null'
        INDX = 1
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
!
!---  Read irreducible saturation  ---
!
        DO 20 N = 1,NFLD
          RPX(2,IZ(N)) = SCHR(4,IZ(N))
   20   CONTINUE
        UNTS = 'null'
        INDX = 2
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
        DO N = 1,LFD
          DO M = 1,2
            RPLT(ITX,M,N) = RPX(M,N)
          ENDDO
        ENDDO
        DEALLOCATE( RPX,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
      ELSE
!
!---  Read 'm' parameter  ---
!
        RPLTX(ITX,1) = SCHR(3,IROCK)
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,1))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        WRITE(IWR,'(4X,A,1PE11.4)') 'Lambda Parameter: ',
     &    RPLTX(ITX,1)
!
!---  Read irreducible saturation  ---
!
        RPLTX(ITX,2) = SCHR(4,IROCK)
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,2))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        WRITE(IWR,'(4X,A,1PE11.4)') 'slr Parameter: ',RPLTX(ITX,2)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF MIBC_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MMVG_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
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
!     Modified Mualem-van Genuchten aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 17 December 2002.
!     Last Modified by MD White, PNNL, 17 December 2002.
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLTX(4,LRPL)
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: RPX
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/MMVG_LRPT'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      IDFLT = 1
      IF( IJK.GT.0 ) THEN
        ALLOCATE( RPX(1:LRPL,1:LFD),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
!
!---  Read 'm' parameter  ---
!
        DO 10 N = 1,NFLD
          RPX(1,IZ(N)) = 1.D+0-1.D+0/SCHR(3,IZ(N))
   10   CONTINUE
        UNTS = 'null'
        INDX = 1
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
!
!---  Read pore-scale parameter  ---
!
        DO 20 N = 1,NFLD
          RPX(2,IZ(N)) = 5.D-1
   20   CONTINUE
        UNTS = 'null'
        INDX = 2
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
        DO N = 1,LFD
          DO M = 1,2
            RPLT(ITX,M,N) = RPX(M,N)
          ENDDO
        ENDDO
        DEALLOCATE( RPX,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
      ELSE
!
!---  Read 'm' parameter  ---
!
        RPLTX(ITX,1) = 1.D+0-1.D+0/SCHR(3,IROCK)
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,1))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        IF( ICOMMA.EQ.ISTX ) THEN
          WRITE(IWR,'(4X,A)') 'Default Value: m = 1 - 1/n'
        ENDIF
        WRITE(IWR,'(4X,A,1PE11.4)') 'm Parameter: ',RPLTX(ITX,1)
!
!---  Read pore-scale parameter  ---
!
        RPLTX(ITX,2) = 5.D-1
        IDFLT = 1
        ISTX = ISTART
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,2))
        IF( ICOMMA.EQ.ISTX ) THEN
          WRITE(IWR,'(4X,A)') 'Default Value: 1/2 (Square Root)'
        ENDIF
        WRITE(IWR,'(4X,A,1PE11.4)') 'Pore-Scale Parameter: ',
     &    RPLTX(ITX,2)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF MMVG_LRPT GROUP  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MMBC_LRPT( RPLTX,ISTART,ICOMMA,IROCK,IJK,CHDUM,ITX )
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
!     Modified Mualem-Brooks/Corey aqueous relative permeability
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 17 December 2002.
!     Last Modified by MD White, PNNL, 17 December 2002.
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RPLTX(4,LRPL)
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: RPX
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/MMBC_LRPT'
      IF( INDEX(SVN_ID(152)(1:1),'$').EQ.0 ) SVN_ID(152) =
     & '$Id: rdlrp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      IDFLT = 1
      IF( IJK.GT.0 ) THEN
        ALLOCATE( RPX(1:LRPL,1:LFD),STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Allocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
!
!---  Read 'm' parameter  ---
!
        DO 10 N = 1,NFLD
          RPX(1,IZ(N)) = SCHR(3,IZ(N))
   10   CONTINUE
        UNTS = 'null'
        INDX = 1
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
!
!---  Read irreducible saturation  ---
!
        DO 20 N = 1,NFLD
          RPX(2,IZ(N)) = 5.D-1
   20   CONTINUE
        UNTS = 'null'
        INDX = 2
        LNDX = LRPL
        CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,RPX,INDX,LNDX )
        DO N = 1,LFD
          DO M = 1,2
            RPLT(ITX,M,N) = RPX(M,N)
          ENDDO
        ENDDO
        DEALLOCATE( RPX,STAT=ISTAT )
        IF( ISTAT.NE.0 ) THEN
          INDX = 3
          CHMSG = 'Deallocation Error: RPX'
          CALL WRMSGS( INDX )
        ENDIF
      ELSE
!
!---  Read 'm' parameter  ---
!
        RPLTX(ITX,1) = SCHR(3,IROCK)
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,1))
        WRITE(IWR,'(2X,A)') VARB(1:IVR)
        WRITE(IWR,'(4X,A,1PE11.4)') 'Lambda Parameter: ',
     &    RPLTX(ITX,1)
!
!---  Read irreducible saturation  ---
!
        RPLTX(ITX,2) = 5.D-1
        CALL RDDPR(ISTART,ICOMMA,CHDUM,RPLTX(ITX,2))
        WRITE(IWR,'(4X,A,1PE11.4)') 'Pore-Scale Parameter: ',
     &    RPLTX(ITX,2)
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  END OF MMBC_LRPT GROUP  ---
!
      RETURN
      END


