!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDDPR( ISTART,ICOMMA,CHDUM,VAR )
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
!     Last Modified by MD White, Battelle, PNL, November 3, 1994.
!     $Id: rddpr.F 1080 2017-03-14 16:22:02Z d3c002 $
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
      CHARACTER*6 FORM1
      CHARACTER*7 FORM2
!
!----------------------Common Blocks-----------------------------------!
!



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
      SUB_LOG(ISUB_LOG) = '/RDDPR'
      IF( INDEX(SVN_ID(142)(1:1),'$').EQ.0 ) SVN_ID(142) =
     & '$Id: rddpr.F 1080 2017-03-14 16:22:02Z d3c002 $' 
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
        CALL WRMSGS( INDX )
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
            CALL WRMSGS(INDX)
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
          CALL WRMSGS( INDX )
        ENDIF
        ISTART = ICOMMA + 1
      ENDIF
      IDFLT = 0
!
!---  End of RDDPR group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
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
!     Check to see if a double-precision-real input exists.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNNL, November 2000.
!     Last Modified by MD White, Battelle, PNNL, November 7, 2000.
!     $Id: rddpr.F 1080 2017-03-14 16:22:02Z d3c002 $
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
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/CHKDPR'
      IF( INDEX(SVN_ID(142)(1:1),'$').EQ.0 ) SVN_ID(142) =
     & '$Id: rddpr.F 1080 2017-03-14 16:22:02Z d3c002 $' 
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
!---  End of CHKDPR group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

