!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDSIMU
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
!     Read input file for simulation title information.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, November 1992.
!     Last Modified by MD White, Battelle, PNL, December 31, 1992.
!     $Id: rdsimu.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
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
      SUB_LOG(ISUB_LOG) = '/RDSIMU'
      IF( INDEX(SVN_ID(160)(1:1),'$').EQ.0 ) SVN_ID(160) =
     & '$Id: rdsimu.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Write card information to ouput file  ---
!
      CARD = 'Simulation Title Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read version number  ---
!
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      WRITE (IWR,'(/,A)') 'Version Number: ' //
     &  'See Configuration Version Record Below'
      ISTART = 1
      CALL RDINPL( CHDUM )
      VARB = 'Simulation Title'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,TITLE)
      WRITE (IWR,'(3A)') VARB(1:IVR),': ',TITLE(1:NCH)
      ISTART = 1
      CALL RDINPL( CHDUM )
      VARB = 'User Name'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,USER)
      WRITE (IWR,'(3A)') VARB(1:IVR),': ',USER(1:NCH)
      ISTART = 1
      CALL RDINPL( CHDUM )
      VARB = 'Company Name'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,CMPNY)
      WRITE (IWR,'(3A)') VARB(1:IVR),': ',CMPNY(1:NCH)
      ISTART = 1
      CALL RDINPL( CHDUM )
      VARB = 'Input Creation Date'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,INPDAT)
      WRITE (IWR,'(3A)') VARB(1:IVR),': ',INPDAT(1:NCH)
      ISTART = 1
      CALL RDINPL( CHDUM )
      VARB = 'Input Creation Time'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,INPTIM)
      WRITE (IWR,'(3A)') VARB(1:IVR),': ',INPTIM(1:NCH)
      ISTART = 1
      CALL RDINPL( CHDUM )
      VARB = 'Number of Simulation Note Lines: '
      CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
      IF( NLIN.GT.LNOTES ) THEN
        INDX = 5
        CHMSG = 'Number of Note Lines > Parameter LNOTES'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Read simulation notes  ---
!
      VARB = 'Simulation Notes: '
      WRITE (IWR,'(/,A,/)') VARB
      DO 120 NL = 1,NLIN
   98   READ (IRD,'(A)') NOTES(NL)
        IF( NOTES(NL)(1:1) .EQ. '#' ) GOTO 98
        WRITE(IWR,'(A)') NOTES(NL)
  120 CONTINUE
!
!---  End of RDSIMU group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

