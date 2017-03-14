!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDROCK
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
!     Read input file for rock/soil zonation information.
!     Overlapping rock/soil zone definitions are sequence dependent.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, November 1992.
!     Last Modified by MD White, Battelle, PNL, December 31, 1992.
!     Last Modified by MD White, PNNL, 5 October 2001.
!     $Id: rdrock.F 1080 2017-03-14 16:22:02Z d3c002 $
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
      CHARACTER*64 ADUM,BDUM,FDUM,CDUM
      CHARACTER*512 CHDUM
      CHARACTER*17 FORM2
      CHARACTER*4 FORM1
      LOGICAL FCHK
!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM1,FORM2
      DATA FORM2 /'Soil 000000000'/
      DATA FORM1 /'(I )'/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDROCK'
      IF( INDEX(SVN_ID(158)(1:1),'$').EQ.0 ) SVN_ID(158) =
     & '$Id: rdrock.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Write card information to ouput file  ---
!
      CARD = 'Rock/Soil Zonation Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read the number of rock/soil zonation information lines  ---
!
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      NROCK = 0
      NROCK2 = 0
      VARB = 'Input Option [File, Zonation_File, Integer, Indexing]'
      CALL RDCHR(ISTART,ICOMMA,NCHA,CHDUM,ADUM)
!
!---  Read rock/soil zonation information from an external file  ---
!
      IF( INDEX(ADUM,'zonation').NE.0 .AND.
     &  INDEX(ADUM,'file').NE.0 ) THEN
        VARB = 'Rock/soil zonation external file name'
        CALL RDCHR(ISTART,ICOMMA,NCHB,CHDUM,BDUM)
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
     &       // BDUM(1:NCHB)
           CALL WRMSGS( INDX )
         ELSEIF( CDUM.EQ.'UNFORMATTED' ) THEN
           INDX = 4
           CHMSG = 'Rock/soil zonational file is unformatted: '
     &       // BDUM(1:NCHB)
           CALL WRMSGS( INDX )
         END IF
         OPEN( UNIT=36,FILE=FDUM(1:NCHF),STATUS='OLD',FORM='FORMATTED' )
         WRITE(IWR,'(/,2A)') 'Formatted Rock/Soil Zonation File: ',
     &     FDUM(1:NCHF)
         READ(36,*)(IZ(N),N=1,NFLD)
         CLOSE(UNIT=36)
        ELSE
          INQUIRE( FILE=FDUM(1:NCHF), FORM=CDUM, EXIST=FCHK )
          IF( .NOT.FCHK ) THEN
            INDX = 4
            CHMSG = 'Rock/soil zonational file does not exist: '
     &        // BDUM(1:NCHB)
            CALL WRMSGS( INDX )
          ELSEIF( CDUM.EQ.'FORMATTED' ) THEN
            INDX = 4
            CHMSG = 'Rock/soil zonational file is formatted: '
     &        // BDUM(1:NCHB)
            CALL WRMSGS( INDX )
          END IF
          OPEN( UNIT=36,FILE=FDUM(1:NCHF),STATUS='OLD',
     &      FORM='UNFORMATTED' )
          WRITE(IWR,'(/,2A)') 'Unformatted Rock/Soil Zonation File: ',
     &      FDUM(1:NCHF)
          READ(36)(IZ(N),N=1,NFLD)
          CLOSE(UNIT=36)
        ENDIF
        DO 30 N = 1,NFLD
          NROCK = MAX( NROCK,IZ(N) )
   30   CONTINUE
        IF( NROCK.GT.LRC ) THEN
          INDX = 5
          CHMSG = 'Number of Rock/Soil Types > Parameter LRC'
          CALL WRMSGS( INDX )
        ENDIF
        DO 50 NL = 1,NROCK
          CALL RDINPL( CHDUM )
          ISTART = 1
          CALL LCASE( CHDUM )
          VARB = 'Rock/Soil Name: '
          CALL RDCHR(ISTART,ICOMMA,NCHR,CHDUM,ROCK(NL))
          DO 40 M = 1,NL-1
            IF( ROCK(M).EQ.ROCK(NL) ) THEN
              INDX = 4
              CHMSG = 'Duplicate Rock/Soil Name: ' // ROCK(NL)(1:NCHR)
              CALL WRMSGS( INDX )
            ENDIF
   40     CONTINUE
          IF( INDEX(CHDUM(1:),'skip conv').NE.0 ) ISKP(NL) = 1
          IF( INDEX(CHDUM(1:),'no hydrate').NE.0 ) INHYD(NL) = 1
          WRITE(IWR,'(3A,I6)') 'Rock/Soil Name: ',ROCK(NL),
     &      'Zonation Number: ',NL
   50   CONTINUE
!
!---  Read rock/soil zonation information from an external file  ---
!
      ELSEIF( INDEX(ADUM,'file').NE.0 ) THEN
        VARB = 'External File Name'
        CALL RDCHR(ISTART,ICOMMA,NCHB,CHDUM,BDUM)
        INQUIRE( FILE=BDUM(1:NCHB), FORM=CDUM, EXIST=FCHK )
        IF( .NOT.FCHK ) THEN
          INDX = 4
          CHMSG = 'Rock/soil zonational file does not exist: '
     &      // BDUM(1:NCHB)
          CALL WRMSGS( INDX )
        ELSEIF( CDUM.EQ.'UNFORMATTED' ) THEN
          INDX = 4
          CHMSG = 'Rock/soil zonational file is unformatted: '
     &      // BDUM(1:NCHB)
          CALL WRMSGS( INDX )
        END IF
        OPEN(UNIT=26, FILE=BDUM(1:NCHB), STATUS='OLD', FORM='FORMATTED')
        WRITE(IWR,'(/,2A)') 'Rock/Soil Zonation File: ',BDUM(1:NCHB)
        VARB = 'Number of Zonation Lines'
        CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
        DO 70 NL = 1,NLIN
          CALL RDINPL( CHDUM )
          ISTART = 1
          CALL LCASE( CHDUM )
          VARB = 'Rock/Soil Name: '
          CALL RDCHR(ISTART,ICOMMA,NCHR,CHDUM,ROCK(NL))
          DO 60 M = 1,NROCK
            IF( ROCK(M).EQ.ROCK(NL) ) THEN
              INDX = 4
              CHMSG = 'Duplicate Rock/Soil Name: ' // ROCK(NL)(1:NCHR)
              CALL WRMSGS( INDX )
            ENDIF
   60     CONTINUE
          NROCK = NROCK+1
          IF( NROCK.GT.LRC ) THEN
            INDX = 5
            CHMSG = 'Number of Rock/Soil Types > Parameter LRC'
            CALL WRMSGS( INDX )
          ENDIF
          WRITE(IWR,'(2A)') 'Rock/Soil Name: ',ROCK(NL)
          IF( INDEX(CHDUM(1:),'skip conv').NE.0 ) ISKP(NROCK) = 1
   70   CONTINUE
!
!---  Read rock/soil zonation indices  ---
!
   80   CONTINUE
        READ(26,*,END=90) I,J,K,IROCK
        IF( I.LT.1 .OR. I.GT.IFLD .OR. J.LT.1 .OR. J.GT.JFLD
     &    .OR. K.LT.1 .OR. K.GT.KFLD ) THEN
          INDX = 4
          CHMSG = 'Rock/Soil Zonation Index Out of Range'
          CALL WRMSGS(INDX)
        ENDIF
        IF( IROCK.LT.1 .OR. IROCK.GT.NROCK ) THEN
          INDX = 7
          CHMSG = 'Rock/Soil Number Out of Range'
          IMSG = IROCK
          CALL WRMSGS(INDX)
        ENDIF
        IZ(ND(I,J,K)) = IROCK
        GOTO 80
   90   CONTINUE
        CLOSE(UNIT=26)
!
!---  Assign rock/soil zonation information by indexing order,
!     useful for stochastic realizations  ---
!
      ELSEIF( INDEX(ADUM,'indexing').NE.0 ) THEN
        NCHR = 5 + ICOUNT(NFLD)
        IROCK = 0
!
!---    IJK indexing  ---
!
        IF( INDEX(ADUM,'ijk').NE.0 ) THEN
          ROCK(1) = 'ijk indexing'
          DO 100 K = 1,KFLD
          DO 100 J = 1,JFLD
          DO 100 I = 1,IFLD
            N = ND(I,J,K)
            IROCK = IROCK + 1
            IZ(N) = IROCK
  100     CONTINUE
!
!---    KIJ indexing  ---
!
        ELSEIF( INDEX(ADUM,'kij').NE.0 ) THEN
          ROCK(1) = 'kij indexing'
          DO 110 J = 1,JFLD
          DO 110 I = 1,IFLD
          DO 110 K = 1,KFLD
            N = ND(I,J,K)
            IROCK = IROCK + 1
            IZ(N) = IROCK
  110     CONTINUE
!
!---    JKI indexing  ---
!
        ELSEIF( INDEX(ADUM,'jki').NE.0 ) THEN
          ROCK(1) = 'jki indexing'
          DO 120 I = 1,IFLD
          DO 120 K = 1,KFLD
          DO 120 J = 1,JFLD
            N = ND(I,J,K)
            IROCK = IROCK + 1
            IZ(N) = IROCK
  120     CONTINUE
!
!---    Unrecognized indexing  ---
!
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Indexing Option' // ADUM(1:NCHA)
          CALL WRMSGS(INDX)
        ENDIF
        NROCK = IROCK
!
!---    Check for optional zonation file  ---
!
        CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
        IF( INDX.EQ.1 ) THEN
          VARB = 'Zonation File Formatting'
          CALL RDCHR(ISTART,ICOMMA,NCHA,CHDUM,ADUM)
!
!---      Read rock/soil zonation information from an external file  ---
!
          VARB = 'Rock/soil zonation external file name'
          CALL RDCHR(ISTART,ICOMMA,NCHB,CHDUM,BDUM)
!
!---      Formatted zonation file  ---
!
          IF( INDEX(ADUM,'formatted').NE.0 ) THEN
           INQUIRE( FILE=BDUM(1:NCHB), FORM=CDUM, EXIST=FCHK )
           IF( .NOT.FCHK ) THEN
             INDX = 4
             CHMSG = 'Rock/soil zonational file does not exist: '
     &         // BDUM(1:NCHB)
             CALL WRMSGS( INDX )
           ELSEIF( CDUM.EQ.'UNFORMATTED' ) THEN
             INDX = 4
             CHMSG = 'Rock/soil zonational file is unformatted: '
     &         // BDUM(1:NCHB)
             CALL WRMSGS( INDX )
           END IF
           OPEN( UNIT=36,FILE=BDUM(1:NCHB),STATUS='OLD',
     &       FORM='FORMATTED' )
           WRITE(IWR,'(/,2A)') 'Formatted Rock/Soil Zonation File: ',
     &       BDUM(1:NCHB)
           READ(36,*)(IZ2(N),N=1,NFLD)
           CLOSE(UNIT=36)
!
!---      Unformatted zonation file  ---
!
          ELSE
            INQUIRE( FILE=BDUM(1:NCHB), FORM=CDUM, EXIST=FCHK )
            IF( .NOT.FCHK ) THEN
              INDX = 4
              CHMSG = 'Rock/soil zonational file does not exist: '
     &          // BDUM(1:NCHB)
              CALL WRMSGS( INDX )
            ELSEIF( CDUM.EQ.'FORMATTED' ) THEN
              INDX = 4
              CHMSG = 'Rock/soil zonational file is formatted: '
     &          // BDUM(1:NCHB)
              CALL WRMSGS( INDX )
            END IF
            OPEN( UNIT=36,FILE=BDUM(1:NCHB),STATUS='OLD',
     &        FORM='UNFORMATTED' )
            WRITE(IWR,'(/,2A)') 'Unformatted Rock/Soil Zonation File: ',
     &        BDUM(1:NCHB)
            READ(36)(IZ2(N),N=1,NFLD)
            CLOSE(UNIT=36)
          ENDIF
!
!---      Number of rock/soil types in zonation file  ---
!
          DO 130 N = 1,NFLD
            NROCK2 = MAX( NROCK2,IZ2(N) )
  130     CONTINUE
          IF( NROCK2.GT.LRC ) THEN
            INDX = 5
            CHMSG = 'Number of Rock/Soil Types > Parameter LRC'
            CALL WRMSGS( INDX )
          ENDIF
!
!---      Load rock/soil type names  ---
!
          DO 150 NL = 1,NROCK2
            CALL RDINPL( CHDUM )
            ISTART = 1
            CALL LCASE( CHDUM )
            VARB = 'Rock/Soil Name: '
            CALL RDCHR(ISTART,ICOMMA,NCHR,CHDUM,ROCK2(NL))
            DO 140 M = 1,NL-1
              IF( ROCK2(M).EQ.ROCK2(NL) ) THEN
                INDX = 4
                CHMSG = 'Duplicate Rock/Soil Name: ' // 
     &            ROCK2(NL)(1:NCHR)
                CALL WRMSGS( INDX )
              ENDIF
  140       CONTINUE
            IF( INDEX(CHDUM(1:),'skip conv').NE.0 ) ISKP(NL) = 1
            IF( INDEX(CHDUM(1:),'no hydrate').NE.0 ) INHYD(NL) = 1
            WRITE(IWR,'(3A,I6)') 'Rock/Soil Name: ',ROCK2(NL),
     &        'Zonation Number: ',NL
  150     CONTINUE
        ENDIF
        GOTO 500
      ELSE
!
!---  Read rock/soil zonation information from the input file  ---
!
        ISTART = 1
        VARB = 'Number of Zonation Lines'
        CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
        DO 300 NL = 1, NLIN
          CALL RDINPL( CHDUM )
          CALL LCASE( CHDUM )
          ISTART = 1
          ADUM(1:) = ' '
          VARB = 'Rock/Soil Name: '
          CALL RDCHR(ISTART,ICOMMA,NCHA,CHDUM,ADUM)
          DO 210 M = 1,NROCK
            IF( ROCK(M).EQ.ADUM ) THEN
              IROCK = M
              GOTO 220
            ENDIF
  210     CONTINUE
          NROCK = NROCK+1
          IF( NROCK.GT.LRC ) THEN
            INDX = 5
            CHMSG = 'Number of Rock/Soil Types > Parameter LRC'
            CALL WRMSGS( INDX )
          ENDIF
          ROCK(NROCK) = ADUM
          IROCK = NROCK
  220     CONTINUE
!
!---  Read rock/soil domain  ---
!
          VARB = 'Rock/Soil Domain Index: '
          CALL RDINT(ISTART,ICOMMA,CHDUM,I1)
          CALL RDINT(ISTART,ICOMMA,CHDUM,I2)
          CALL RDINT(ISTART,ICOMMA,CHDUM,J1)
          CALL RDINT(ISTART,ICOMMA,CHDUM,J2)
          CALL RDINT(ISTART,ICOMMA,CHDUM,K1)
          CALL RDINT(ISTART,ICOMMA,CHDUM,K2)
          I1 = MAX( 1,I1 )
          I1 = MIN( IFLD,I1,I2 )
          I2 = MAX( 1,I1,I2 )
          I2 = MIN( IFLD,I2 )
          J1 = MAX( 1,J1 )
          J1 = MIN( JFLD,J1,J2 )
          J2 = MAX( 1,J1,J2 )
          J2 = MIN( JFLD,J2 )
          K1 = MAX( 1,K1 )
          K1 = MIN( KFLD,K1,K2 )
          K2 = MAX( 1,K1,K2 )
          K2 = MIN( KFLD,K2 )
          WRITE(IWR,'(/,2A)') 'Rock/Soil Name: ',ROCK(IROCK)
          WRITE(IWR,'(2X,A)') 'Rock/Soil Domain:'
          WRITE(IWR,'(4X,A,I6,A,I6)') 'I = ',I1,' to ',I2
          WRITE(IWR,'(4X,A,I6,A,I6)') 'J = ',J1,' to ',J2
          WRITE(IWR,'(4X,A,I6,A,I6)') 'K = ',K1,' to ',K2
!
!---  Read scaling group associations  ---
!
          IF( ISLC(19).EQ.1 ) THEN
            ADUM(1:) = ' '
            VARB = 'Scaling Group Name: '
            CALL RDCHR(ISTART,ICOMMA,NCHA,CHDUM,ADUM)
            DO 230 M = 1,NSCALE
              IF( SCALNM(M).EQ.ADUM ) THEN
                IF( ISCALE(IROCK).NE.0 .AND. ISCALE(IROCK).NE.M ) THEN
                  INDX = 4
                  CHMSG = 'Rock/Soil Name Associated with ' //
     &              'Multiple Scaling Groups: ' // ROCK(IROCK)
                  CALL WRMSGS( INDX )
                ENDIF
                ISCALE(IROCK) = M
                GOTO 240
              ENDIF
  230       CONTINUE
            NSCALE = NSCALE+1
            IF( NSCALE.GT.LRC ) THEN
              INDX = 5
              CHMSG = 'Number of Scaling Groups > Parameter LRC'
              CALL WRMSGS( INDX )
            ENDIF
            SCALNM(NSCALE) = ADUM
            ISCALE(IROCK) = NSCALE
  240       CONTINUE
          ENDIF
!
!---  Label nodes with rock/soil types  ---
!
          DO 270 K = K1,K2
            DO 260 J = J1,J2
              DO 250 I = I1,I2
                N = ND(I,J,K)
                IZ(N) = IROCK
  250         CONTINUE
  260       CONTINUE
  270     CONTINUE
          IF( INDEX(CHDUM(1:),'skip conv').NE.0 ) ISKP(IROCK) = 1
          IF( INDEX(CHDUM(1:),'no hydrate').NE.0 ) INHYD(NL) = 1
          IF( NL .LT. NLIN ) WRITE(IWR,'(/)')
  300   CONTINUE
      ENDIF
!
!---  Check for conflicts between scaling group and rock/soil names  ---
!
      DO 402 M = 1,NROCK
        DO 400 L = 1,NSCALE
          IF( SCALNM(L).EQ.ROCK(M) )THEN
            INDX = 4
            CHMSG = 'Scaling Group Name Equals Rock/Soil Name: ' //
     &        SCALNM(L)
            CALL WRMSGS( INDX )
          ENDIF
  400   CONTINUE
  402 CONTINUE
  500 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDROCK group  ---
!
      RETURN
      END

