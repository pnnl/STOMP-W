!----------------------Function----------------------------------------!
!
      FUNCTION FNTBLX( FY,N1,N2,INDX )
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
!     Linearly interpolate from a table of values.
!
!     INDX = 0 : Table truncation beyond table limits.
!     INDX = 1 : Table extrapolation beyond table limits.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, March, 1993.
!     Last Modified by MD White, Battelle, PNL, April 14, 1994.
!     $Id: fntblx.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TABL
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
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/FNTBLX'
      IF( INDEX(SVN_ID(61)(1:1),'$').EQ.0 ) SVN_ID(61) =
     & '$Id: fntblx.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Ascending table order  ---
!
      IF( TBLY(N2) .GT. TBLY(N1) ) THEN
        IF( FY .LE. TBLY(N1) ) THEN
          IF( INDX.EQ.0 ) THEN
            FNTBLX = TBLX(N1)
          ELSE
            FNTBLX = (FY-TBLY(N1))*(TBLX(N1+1)-TBLX(N1))/
     &        (TBLY(N1+1)-TBLY(N1)) + TBLX(N1)
          ENDIF
        ELSEIF( FY .GE. TBLY(N2) ) THEN
          IF( INDX.EQ.0 ) THEN
            FNTBLX = TBLX(N2)
          ELSE
            FNTBLX = (FY-TBLY(N2))*(TBLX(N2)-TBLX(N2-1))/
     &        (TBLY(N2)-TBLY(N2-1)) + TBLX(N2)
          ENDIF
        ELSE
          DO 100 N = N1+1,N2
            IF( FY .LE. TBLY(N) ) THEN
              FNTBLX = (FY-TBLY(N-1))*(TBLX(N)-TBLX(N-1))/
     &          (TBLY(N)-TBLY(N-1)) + TBLX(N-1)
              GOTO 110
            ENDIF
  100     CONTINUE
  110   CONTINUE
        ENDIF
!
!---  Descending table order  ---
!
      ELSEIF( TBLY(N2) .LT. TBLY(N1) ) THEN
        IF( FY .LE. TBLY(N2) ) THEN
          IF( INDX.EQ.0 ) THEN
            FNTBLX = TBLX(N2)
          ELSE
            FNTBLX = (FY-TBLY(N2))*(TBLX(N2-1)-TBLX(N2))/
     &        (TBLY(N2-1)-TBLY(N2)) + TBLX(N2)
          ENDIF
        ELSEIF( FY .GE. TBLY(N1) ) THEN
          IF( INDX.EQ.0 ) THEN
            FNTBLX = TBLX(N1)
          ELSE
            FNTBLX = (FY-TBLY(N1))*(TBLX(N1)-TBLX(N1+1))/
     &        (TBLY(N1)-TBLY(N1+1)) + TBLX(N1)
          ENDIF
        ELSE
          DO 200 N = N1+1,N2
            IF( FY .GE. TBLY(N) ) THEN
              FNTBLX = (FY-TBLY(N))*(TBLX(N-1)-TBLX(N))/
     &          (TBLY(N-1)-TBLY(N)) + TBLX(N)
              GOTO 210
            ENDIF
  200     CONTINUE
  210   CONTINUE
        ENDIF
      ELSE
        INDX = 3
        CHMSG = 'Invalid Table'
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of FNTBLX group
!
      RETURN
      END


