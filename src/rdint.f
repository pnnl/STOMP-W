!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINT( ISTART,ICOMMA,CHDUM,IVAR )
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
!     Last Modified by MD White, Battelle, PNNL, November 8, 2000.
!     $Id: rdint.F 1080 2017-03-14 16:22:02Z d3c002 $
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
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*(*) CHDUM
      CHARACTER*4 FORM1
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM1
      DATA FORM1 /'(I )'/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDINT'
      IF( INDEX(SVN_ID(150)(1:1),'$').EQ.0 ) SVN_ID(150) =
     & '$Id: rdint.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      IDFLTD = 0
      ICD = INDEX( CARD,'  ')-1
      IVR = INDEX( VARB,'  ')-1
!
!---  End of card record error ---
!
      IF( CHDUM(1:1) .EQ. '~' ) THEN
        INDX = 4
        CHMSG = 'End of Card Record: ' // VARB(1:IVR)
        CALL WRMSGS( INDX )
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
        CALL WRMSGS( INDX )
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
            CALL WRMSGS(INDX)
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
          CALL WRMSGS( INDX )
        ENDIF
        ISTART = ICOMMA + 1
      ENDIF
      IDFLT = 0
!
!---  End of RDINT group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CHKINT( ISTART,ICOMMA,CHDUM,INDX )
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
!     Last Modified by MD White, Battelle, PNNL, November 8, 2000.
!     $Id: rdint.F 1080 2017-03-14 16:22:02Z d3c002 $
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
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*(*) CHDUM
      CHARACTER*4 FORM1
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM1
      DATA FORM1 /'(I )'/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/CHKINT'
      IF( INDEX(SVN_ID(150)(1:1),'$').EQ.0 ) SVN_ID(150) =
     & '$Id: rdint.F 1080 2017-03-14 16:22:02Z d3c002 $' 
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
        CALL WRMSGS( INDX )
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
!---  End of CHKINT group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

