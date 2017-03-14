!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDCHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
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
!     Last Modified by MD White, Battelle, PNNL, November 8, 2000.
!     $Id: rdchr.F 1080 2017-03-14 16:22:02Z d3c002 $
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
      CHARACTER*(*) ADUM
      CHARACTER*(*) CHDUM
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDCHR'
      IF( INDEX(SVN_ID(141)(1:1),'$').EQ.0 ) SVN_ID(141) =
     & '$Id: rdchr.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      IDFLTD = 0
      ICD = INDEX( CARD,'  ')-1
      IVR = INDEX( VARB,'  ')-1
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
!---  Comma not found, missing character string data  ---
!
      IF( ISTOP.LT.ISTART ) THEN
        INDX = 4
        CHMSG = 'Missing Character String: ' // VARB(1:IVR)
        CALL WRMSGS( INDX )
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
        NCH = INDEX( ADUM,'  ')-1
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
!---  End of RDCHR group  ---
!
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
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
!     Last Modified by MD White, Battelle, PNNL, November 8, 2000.
!     $Id: rdchr.F 1080 2017-03-14 16:22:02Z d3c002 $
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
      SUB_LOG(ISUB_LOG) = '/CHKCHR'
      IF( INDEX(SVN_ID(141)(1:1),'$').EQ.0 ) SVN_ID(141) =
     & '$Id: rdchr.F 1080 2017-03-14 16:22:02Z d3c002 $' 
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
!---  Character string recognized as a character string  ---
!
        INDX = 1
      ENDIF
  200 CONTINUE
      ISTART = ISX
      ICOMMA = ICX
!
!---  End of CHKCHR group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CNTRTXT( TXTX,STRX,ITX,ISX )
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
!     Center text of length ITX in a string of length ISX.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 30 January 2004.
!     Last Modified by MD White, Battelle, PNNL, 30 January 2004.
!     $Id: rdchr.F 1080 2017-03-14 16:22:02Z d3c002 $
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
      CHARACTER*(*) TXTX,STRX
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/CNTRTXT'
      IF( INDEX(SVN_ID(141)(1:1),'$').EQ.0 ) SVN_ID(141) =
     & '$Id: rdchr.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      IF( ITX.GT.ISX ) THEN
        INDX = 3
        CHMSG = 'Text Length > String Length'
        CALL WRMSGS( INDX )
      ENDIF
      ISRX = (ISX-ITX)/2
      ISLX = ISX-ITX-ISRX
      DO 10 I = 1,ISLX
        STRX(I:I) = ' '
   10 CONTINUE
      STRX(ISLX+1:ISLX+ITX) = TXTX(1:ITX)
      DO 20 I = ISLX+ITX+1,ISLX+ITX+ISRX
        STRX(I:I) = ' '
   20 CONTINUE
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of CNTRTXT group  ---
!
      RETURN
      END

!!----------------------Subroutine--------------------------------------!
!!
!      SUBROUTINE RDKEYW( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!!
!!-------------------------Disclaimer-----------------------------------!
!!
!!     This material was prepared as an account of work sponsored by
!!     an agency of the United States Government. Neither the
!!     United States Government nor the United States Department of
!!     Energy, nor Battelle, nor any of their employees, makes any
!!     warranty, express or implied, or assumes any legal liability or
!!     responsibility for the accuracy, completeness, or usefulness
!!     of any information, apparatus, product, software or process
!!     disclosed, or represents that its use would not infringe
!!     privately owned rights.
!!
!!----------------------Acknowledgement---------------------------------!
!!
!!     This software and its documentation were produced with Government
!!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!!     United Department of Energy. The Government retains a paid-up
!!     non-exclusive, irrevocable worldwide license to reproduce,
!!     prepare derivative works, perform publicly and display publicly
!!     by or for the Government, including the right to distribute to
!!     other Government contractors.
!!
!!---------------------Copyright Notices--------------------------------!
!!
!!            Copyright Battelle Memorial Institute, 1996
!!                    All Rights Reserved.
!!
!!----------------------Description-------------------------------------!
!!
!!     Fill character string ADUM with characters between < and >.
!!     Return 'null' for null entries.
!!
!!----------------------Authors-----------------------------------------!
!!
!!     Written by MD White, April 11, 2012
!!
!#ifdef 1
!!----------------------Fortran 90 Modules------------------------------!
!!
!      USE GLB_PAR
!      USE SOLTN
!!
!#endif
!!----------------------Implicit Double Precision-----------------------!
!!
!      IMPLICIT REAL*8 (A-H,O-Z)
!      IMPLICIT INTEGER (I-N)
!!
!!----------------------Parameter Statements----------------------------!
!!
!#ifndef 1
!      INCLUDE 'parameters'
!#endif
!!
!!----------------------Type Declarations-------------------------------!
!!
!      CHARACTER*(*) ADUM
!      CHARACTER*(*) CHDUM
!!
!!----------------------Common Blocks-----------------------------------!
!!
!#ifndef 1
!      INCLUDE 'commons'
!#endif
!!
!!----------------------Executable Lines--------------------------------!
!!
!      ISUB_LOG = ISUB_LOG+1
!      SUB_LOG(ISUB_LOG) = '/RDKEYW'
!      IF( INDEX(SVN_ID(141)(1:1),'$').EQ.0 ) SVN_ID(141) =
!     & '$Id: rdchr.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!      IDFLTD = 0
!      ICD = INDEX( CARD,'  ')-1
!      IVR = INDEX( VARB,'  ')-1
!      INDX = 0
!!
!!---  End of card record error ---
!!
!      IF( CHDUM(1:1) .EQ. '~' ) THEN
!        INDX = 4
!        CHMSG = 'End of Card Record: ' // VARB(1:IVR)
!        CALL WRMSGS( INDX )
!      ENDIF
!!
!!---  Find closing keyword character >  ---
!!
!      ICOMMA = INDEX (CHDUM(ISTART:), '>') + ISTART - 1
!      ISTOP = ICOMMA
!!
!!---  Keyword formatting error ---
!!
!      IF( CHDUM(1:1) .NE. '<' ) THEN
!        INDX = 4
!        CHMSG = 'Keyword Formatting Error: ' // CHDUM(1:ISTOP)
!        CALL WRMSGS( INDX )
!      ENDIF
!  100 CONTINUE
!!
!!---  Closing keyword character > not found, 
!!     missing character string data  ---
!!
!      IF( ISTOP.LE.ISTART ) THEN
!        INDX = 4
!        CHMSG = 'Missing Keyword String: ' // CHDUM
!        CALL WRMSGS( INDX )
!      ENDIF
!!
!!---  Translate character string into a character string  ---
!!
!      ADUM = ' '
!      NCH = ISTOP-ISTART+1
!      READ (CHDUM(ISTART:ISTOP), '(A)') ADUM(1:NCH)
!      ISTART = ICOMMA + 1
!      ISUB_LOG = ISUB_LOG-1
!!
!!---  End of RDKEYW group  ---
!!
!      RETURN
!      END


