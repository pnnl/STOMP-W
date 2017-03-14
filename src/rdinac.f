!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINAC
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
!     Read input file for inactive node information.
!     Label inactive and active nodes.
!     Determine node sequencing for the Jacobian matrix.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, November 1992.
!     Last Modified by SK Wurstner, PNNL, December 07, 2007.
!     $Id: rdinac.F 1080 2017-03-14 16:22:02Z d3c002 $
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
      CHARACTER*64 ADUM
      CHARACTER*512 CHDUM
      CHARACTER*64 BSFX
      INTEGER IPBSX(6)




      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  IZ_TMP

!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDINAC'
      IF( INDEX(SVN_ID(147)(1:1),'$').EQ.0 ) SVN_ID(147) =
     & '$Id: rdinac.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Write card information to ouput file  ---
!
      CARD = 'Inactive Nodes Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE(IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
      NXP = 0
      NDOM = 1
      DO NC = 1,6
        IPBSX(NC) = 0
      ENDDO
!
!---  Read optional number of inactive node card entries, or
!     read first line of single entry  ---
!
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      VARB = 'Multiple Entries Switch'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---  Read boundary surface printing  ---
!
      IF( INDEX(ADUM,'boundary').NE.0 .AND.
     &  INDEX(ADUM,'surface').NE.0 ) THEN
!
!---    Check for boundary surface print requests  ---
!
        DO NC = 1,6
          CALL CHKCHR(ISTART,ICOMMA,CHDUM,INDX)
          IF( INDX.EQ.1 ) THEN
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
            IF( INDEX(ADUM(1:),'bottom').NE.0 ) THEN
              IPBSX(1) = 1
              WRITE(IWR,'(/,A)') 'Print Bottom Boundary Surfaces'
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BSFX)
              WRITE(IWR,'(A)') 'Bottom Boundary Surface File: ' // 
     &          BSFX(1:NCH)
              OPEN(UNIT=101, FILE=BSFX(1:NCH), STATUS='UNKNOWN', 
     &          FORM='FORMATTED')
              CLOSE(UNIT=101, STATUS='DELETE')
              OPEN(UNIT=101, FILE=BSFX(1:NCH), STATUS='NEW', 
     &          FORM='FORMATTED')
            ELSEIF( INDEX(ADUM(1:),'south').NE.0 ) THEN
              IPBSX(2) = 1
              WRITE(IWR,'(/,A)') 'Print South Boundary Surfaces'
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BSFX)
              WRITE(IWR,'(A)') 'South Boundary Surface File: ' // 
     &          BSFX(1:NCH)
              OPEN(UNIT=102, FILE=BSFX(1:NCH), STATUS='UNKNOWN', 
     &          FORM='FORMATTED')
              CLOSE(UNIT=102, STATUS='DELETE')
              OPEN(UNIT=102, FILE=BSFX(1:NCH), STATUS='NEW', 
     &          FORM='FORMATTED')
            ELSEIF( INDEX(ADUM(1:),'west').NE.0 ) THEN
              IPBSX(3) = 1
              WRITE(IWR,'(/,A)') 'Print West Boundary Surfaces'
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BSFX)
              WRITE(IWR,'(A)') 'Bottom West Surface File: ' // 
     &          BSFX(1:NCH)
              OPEN(UNIT=103, FILE=BSFX(1:NCH), STATUS='UNKNOWN', 
     &          FORM='FORMATTED')
              CLOSE(UNIT=103, STATUS='DELETE')
              OPEN(UNIT=103, FILE=BSFX(1:NCH), STATUS='NEW', 
     &          FORM='FORMATTED')
            ELSEIF( INDEX(ADUM(1:),'east').NE.0 ) THEN
              IPBSX(4) = 1
              WRITE(IWR,'(/,A)') 'Print East Boundary Surfaces'
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BSFX)
              WRITE(IWR,'(A)') 'Bottom East Surface File: ' // 
     &          BSFX(1:NCH)
              OPEN(UNIT=104, FILE=BSFX(1:NCH), STATUS='UNKNOWN', 
     &          FORM='FORMATTED')
              CLOSE(UNIT=104, STATUS='DELETE')
              OPEN(UNIT=104, FILE=BSFX(1:NCH), STATUS='NEW', 
     &          FORM='FORMATTED')
            ELSEIF( INDEX(ADUM(1:),'north').NE.0 ) THEN
              IPBSX(5) = 1
              WRITE(IWR,'(/,A)') 'Print North Boundary Surfaces'
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BSFX)
              WRITE(IWR,'(A)') 'Bottom North Surface File: ' // 
     &          BSFX(1:NCH)
              OPEN(UNIT=105, FILE=BSFX(1:NCH), STATUS='UNKNOWN', 
     &          FORM='FORMATTED')
              CLOSE(UNIT=105, STATUS='DELETE')
              OPEN(UNIT=105, FILE=BSFX(1:NCH), STATUS='NEW', 
     &          FORM='FORMATTED')
            ELSEIF( INDEX(ADUM(1:),'top').NE.0 ) THEN
              IPBSX(6) = 1
              WRITE(IWR,'(/,A)') 'Print Top Boundary Surfaces'
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BSFX)
              WRITE(IWR,'(A)') 'Bottom Top Surface File: ' // 
     &          BSFX(1:NCH)
              OPEN(UNIT=106, FILE=BSFX(1:NCH), STATUS='UNKNOWN', 
     &          FORM='FORMATTED')
              CLOSE(UNIT=106, STATUS='DELETE')
              OPEN(UNIT=106, FILE=BSFX(1:NCH), STATUS='NEW', 
     &          FORM='FORMATTED')
            ELSE
              EXIT
            ENDIF
          ELSE
            EXIT
          ENDIF
        ENDDO
!
!---    Read optional number of inactive node card entries, or
!       read first line of single entry  ---
!
        ISTART = 1
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        VARB = 'Multiple Entries Switch'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      ENDIF
!
!---  Read number of multiple entries and first line
!     of first entry  ---
!
      IF( INDEX(ADUM,'multiple').NE.0 ) THEN
        VARB = 'Number of Entries'
        CALL RDINT(ISTART,ICOMMA,CHDUM,NDOM)
!
!---    Read first line for first entry of multiple entries  ---
!
        ISTART = 1
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
      ENDIF
!
!---  Loop over number of inactive node card entries  ---
!
      DO 500 NDM = 1,NDOM
      ISTART = 1
!
!---    Read first line for more than one entry  ---
!
        IF( NDM.GT.1 ) THEN
          CALL RDINPL( CHDUM )
          CALL LCASE( CHDUM )
        ENDIF
        VARB = 'Input Option [Rock/Soil, Zonation File, File, Integer]'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---    Read inactive node information according to rock/soil type  ---
!
        IF( INDEX(ADUM,'rock').NE.0 .OR. INDEX(ADUM,'soil').NE.0 ) THEN
          VARB = 'Number of Rock/Soil Type Lines'
          CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
          WRITE(IWR,'(/,A)') 'Inactive Rock/Soil Types'
          DO 60 L = 1,NLIN
            ISTART = 1
            CALL RDINPL( CHDUM )
            CALL LCASE( CHDUM )
            VARB = 'Rock/Soil Name'
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---        Search known rock types for a matching type ---
!
            DO 30 M = 1, NROCK
               IF( ADUM .EQ. ROCK(M)) THEN
                  IROCK = M
                  GOTO 40
               ENDIF
   30       CONTINUE
            INDX = 2
            CHMSG = 'Unrecognized Rock/Soil Type: '//ADUM(1:NCH)
            CALL WRMSGS( INDX )
            GOTO 60
   40       CONTINUE
            WRITE( IWR,'(2x,2A)') 'Rock/Soil Name: ',ROCK(IROCK)
            DO 50 N = 1,NFLD
              IF( IZ(N).EQ.IROCK ) THEN
                IF( IXP(N).EQ.0 ) THEN
                  INDX = 7
                  IMSG = N
                  CHMSG = 'Node Previously Defined as Inactive'
                  CALL WRMSGS( INDX )
                ENDIF
                IXP(N) = 0
                NXP = NXP + 1
              ENDIF
   50       CONTINUE
   60     CONTINUE
!
!---    Read inactive node information from an external zonation
!       file  ---
!
        ELSEIF( INDEX(ADUM,'zonation').NE.0 .AND.
     &    INDEX(ADUM,'file').NE.0 ) THEN

!
!---    Dynamic memory allocation  ---
!
          ALLOCATE( IZ_TMP(1:LFD),STAT=ISTAT )
          IF( ISTAT.NE.0 ) THEN
            INDX = 3
            CHMSG = 'Allocation Error: IZ_TMP'
            CALL WRMSGS( INDX )
          ENDIF

          VARB = 'Rock/soil zonation external file name'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          IF( INDEX(CHDUM,'formatted').NE.0 ) THEN
            OPEN( UNIT=27,FILE=ADUM(1:NCH),STATUS='OLD',
     &        FORM='FORMATTED' )
            WRITE(IWR,'(/,2A)') 'Formatted Rock/Soil Zonation File: ',
     &        ADUM(1:NCH)
            READ(27,*)(IZ_TMP(N),N=1,NFLD)
            CLOSE(UNIT=27)
          ELSE
            OPEN( UNIT=27,FILE=ADUM(1:NCH),STATUS='OLD',
     &        FORM='UNFORMATTED' )
            WRITE(IWR,'(/,2A)') 'Unformatted Rock/Soil Zonation File: ',
     &        ADUM(1:NCH)
            READ(27)(IZ_TMP(N),N=1,NFLD)
            CLOSE(UNIT=27)
          ENDIF
          DO 70 N = 1,NFLD
            IF( IZ_TMP(N).EQ.0 ) THEN
              IF( IXP(N).EQ.0 ) THEN
                INDX = 7
                IMSG = N
                CHMSG = 'Node Previously Defined as Inactive'
                CALL WRMSGS( INDX )
              ENDIF
              IXP(N) = 0
              NXP = NXP + 1
            ENDIF
   70     CONTINUE
!
!---    Read inactive node information from an external file  ---
!
        ELSEIF( INDEX(ADUM,'file').NE.0 ) THEN
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          NCH = INDEX(ADUM,'  ')-1
          OPEN(UNIT=27, FILE=ADUM(1:NCH),STATUS='OLD',FORM='FORMATTED')
          WRITE(IWR,'(/,2A)') 'Inactive Node File: ',ADUM(1:NCH)
   80     CONTINUE
          READ(27,*,END=90) I,J,K
          IF( I.LT.1 .OR. I.GT.IFLD .OR. J.LT.1 .OR. J.GT.JFLD
     &      .OR. K.LT.1 .OR. K.GT.KFLD ) THEN
            INDX = 4
            CHMSG = 'Inactive Node Index Out of Range'
            CALL WRMSGS(INDX)
          ENDIF
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) THEN
            INDX = 7
            IMSG = N
            CHMSG = 'Node Previously Defined as Inactive'
            CALL WRMSGS( INDX )
          ENDIF
          IXP(N) = 0
          NXP = NXP + 1
          GOTO 80
   90     CONTINUE
          CLOSE(UNIT=27)
        ELSE
!
!---    Read inactive node information from the input file  ---
!
          VARB = 'Number of Inactive Node Lines'
          ISTART = 1
          CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
          DO 400 NL = 1, NLIN
            ISTART = 1
            CALL RDINPL( CHDUM )
            CALL LCASE( CHDUM )
            VARB = 'Inactive Node Domain Index'
            CALL RDINT(ISTART,ICOMMA,CHDUM,I1X)
            CALL RDINT(ISTART,ICOMMA,CHDUM,I2X)
            CALL RDINT(ISTART,ICOMMA,CHDUM,J1X)
            CALL RDINT(ISTART,ICOMMA,CHDUM,J2X)
            CALL RDINT(ISTART,ICOMMA,CHDUM,K1X)
            CALL RDINT(ISTART,ICOMMA,CHDUM,K2X)
            I1X = MAX( 1,I1X )
            I1X = MIN( I1X,I2X,IFLD )
            I2X = MAX( 1,I1X,I2X )
            I2X = MIN( I2X,IFLD )
            J1X = MAX( 1,J1X )
            J1X = MIN( J1X,J2X,JFLD )
            J2X = MAX( 1,J1X,J2X )
            J2X = MIN( J2X,JFLD )
            K1X = MAX( 1,K1X )
            K1X = MIN( K1X,K2X,KFLD )
            K2X = MAX( 1,K1X,K2X )
            K2X = MIN( K2X,KFLD )
            WRITE(IWR,'(/,A)' ) 'Inactive Node Domain'
            WRITE(IWR,'(4X,A,I6,A,I6)') 'I = ',I1X,' to ',I2X
            WRITE(IWR,'(4X,A,I6,A,I6)') 'J = ',J1X,' to ',J2X
            WRITE(IWR,'(4X,A,I6,A,I6)') 'K = ',K1X,' to ',K2X
            DO 300 K = K1X,K2X
              DO 200 J = J1X,J2X
                DO 100 I = I1X,I2X
                  N = ND(I,J,K)
                  IF( IXP(N).NE.0 ) THEN
                    IXP(N) = 0
                    NXP = NXP + 1
                  ENDIF
  100           CONTINUE
  200         CONTINUE
  300       CONTINUE
  400     CONTINUE
        ENDIF
  500 CONTINUE
!
!---  Redefine active nodes ---
!
      IF( NFLD-NXP.GT.LAN ) THEN
        INDX = 5
        CHMSG = 'Number of Active Nodes > Parameter LAN'
        CALL WRMSGS( INDX )
      ENDIF
      WRITE(IWR,'(/,A)' ) 'Node Count'
      WRITE(IWR,'(2X,A,I9)') 'Number of Nodes: ',NFLD
      WRITE(IWR,'(2X,A,I9)') 'Number of Active Nodes: ',NFLD-NXP
      WRITE(IWR,'(2X,A,I9)') 'Number of Inactive Nodes: ',NXP
      NC = 1
!
!---  X-Y Plane yields the lowest band width or
!     active surface spill nodes,
!---  load Jacobian matrix in the increment order I,J,K  ---
!
      IF( (IJFLD.LE.JKFLD .AND. IJFLD.LE.KIFLD) .OR.
     &  ISLC(49).EQ.1 ) THEN
        DO 530 K = 1,KFLD
          DO 520 J = 1,JFLD
            DO 510 I = 1,IFLD
              N = ND(I,J,K)
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
      ELSEIF( JKFLD.LE.IJFLD .AND. JKFLD.LE.KIFLD ) THEN
        DO 630 I = 1,IFLD
          DO 620 K = 1,KFLD
            DO 610 J = 1,JFLD
              N = ND(I,J,K)
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
      ELSEIF( KIFLD.LE.IJFLD .AND. KIFLD.LE.JKFLD ) THEN
        DO 730 J = 1,JFLD
          DO 720 I = 1,IFLD
            DO 710 K = 1,KFLD
              N = ND(I,J,K)
              IF( IXP(N).NE.0 ) THEN
                IXP(N) = NC
                NC = NC+1
              ENDIF
  710       CONTINUE
  720     CONTINUE
  730   CONTINUE
      ENDIF
!
!---  Write boundary surface files  ---
!
      DO K = 1,KFLD
        DO J = 1,JFLD
          DO I = 1,IFLD
            N = ND(I,J,K)
            IF( IXP(N).EQ.0 ) CYCLE
!
!---        Bottom boundary surfaces  ---
!
            IF( IPBSX(1).EQ.1 ) THEN
              IF( K.EQ.1 ) THEN
                WRITE(101,'(3(I9,1X),A)') I,J,K,' -3'
              ELSEIF( K.GT.1 ) THEN
                NB = ND(I,J,K-1)
                IF( IXP(NB).EQ.0 ) THEN
                  WRITE(101,'(3(I9,1X),A)') I,J,K,' -3'
                ENDIF
              ENDIF
            ENDIF
!
!---        South boundary surfaces  ---
!
            IF( IPBSX(2).EQ.1 ) THEN
              IF( J.EQ.1 ) THEN
                WRITE(102,'(3(I9,1X),A)') I,J,K,' -2'
              ELSEIF( J.GT.1 ) THEN
                NS = ND(I,J-1,K)
                IF( IXP(NS).EQ.0 ) THEN
                  WRITE(102,'(3(I9,1X),A)') I,J,K,' -2'
                ENDIF
              ENDIF
            ENDIF
!
!---        West boundary surfaces  ---
!
            IF( IPBSX(3).EQ.1 ) THEN
              IF( I.EQ.1 ) THEN
                WRITE(103,'(3(I9,1X),A)') I,J,K,' -1'
              ELSEIF( I.GT.1 ) THEN
                NW = ND(I-1,J,K)
                IF( IXP(NW).EQ.0 ) THEN
                  WRITE(103,'(3(I9,1X),A)') I,J,K,' -1'
                ENDIF
              ENDIF
            ENDIF
!
!---        East boundary surfaces  ---
!
            IF( IPBSX(4).EQ.1 ) THEN
              IF( I.EQ.IFLD ) THEN
                WRITE(104,'(3(I9,1X),A)') I,J,K,' 1'
              ELSEIF( I.LT.IFLD ) THEN
                NE = ND(I+1,J,K)
                IF( IXP(NE).EQ.0 ) THEN
                  WRITE(104,'(3(I9,1X),A)') I,J,K,' 1'
                ENDIF
              ENDIF
            ENDIF
!
!---        North boundary surfaces  ---
!
            IF( IPBSX(5).EQ.1 ) THEN
              IF( J.EQ.JFLD ) THEN
                WRITE(105,'(3(I9,1X),A)') I,J,K,' 2'
              ELSEIF( J.LT.IFLD ) THEN
                NN = ND(I,J+1,K)
                IF( IXP(NN).EQ.0 ) THEN
                  WRITE(105,'(3(I9,1X),A)') I,J,K,' 2'
                ENDIF
              ENDIF
            ENDIF
!
!---        Top boundary surfaces  ---
!
            IF( IPBSX(6).EQ.1 ) THEN
              IF( K.EQ.KFLD ) THEN
                WRITE(106,'(3(I9,1X),A)') I,J,K,' 3'
              ELSEIF( K.LT.IFLD ) THEN
                NT = ND(I,J,K+1)
                IF( IXP(NT).EQ.0 ) THEN
                  WRITE(106,'(3(I9,1X),A)') I,J,K,' 3'
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO
!
!---  Close boundary surface files  ---
!
      DO NC = 1,6
        INDX = 100 + NC
        IF( IPBSX(NC).EQ.1 ) CLOSE( UNIT=INDX )
      ENDDO
!
!---  Search for additional definitions  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDINAC group  ---
!
      RETURN
      END

