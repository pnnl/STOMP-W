!----------------------Function----------------------------------------!
!
      SUBROUTINE SPLINY( ITS,ITE )
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
!     Given arrays TBLX and TBLY containing a tabulated function
!     (i.e., y(n) = f(x(n))), with x(1) < x(2) < ... < x(n), this
!     routine returns an array TBLDDY which contains the second
!     derivatives of the interpolating function at the tabulated
!     points x(i).  The routine is structured to set the boundary
!     conditions at function end points to a natural spline, with zero
!     second derivatives on that boundary.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, September, 1994.
!     Last Modified by MD White, Battelle, PNL, September 9, 1994.
!     $Id: spliny.F 1080 2017-03-14 16:22:02Z d3c002 $
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 TBLU(LTBL)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/SPLINY'
      IF( INDEX(SVN_ID(199)(1:1),'$').EQ.0 ) SVN_ID(199) =
     & '$Id: spliny.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Ascending table order  ---
!
      IF( TBLX(ITE).GT.TBLX(ITS) ) THEN
        TBLD = (TBLY(ITS+1)-TBLY(ITS))/(TBLX(ITS+1)-TBLX(ITS))
!
!---  The lower boundary condition is set either to be natural or
!     else to have a specified first derivative.  ---
!
!        IF( TBLD.GT.0.99D+20 ) THEN
          TBLDDY(ITS) = 0.D+0
          TBLU(ITS) = 0.D+0
!        ELSE
!          TBLDDY(ITS) = -0.5D+0
!          TBLU(ITS) = (3.D+0/(TBLX(ITS+1)-TBLX(ITS)))*
!     &      ((TBLY(ITS+1)-TBLY(ITS))/(TBLX(ITS+1)-TBLX(ITS))-TBLD)
!        ENDIF
!
!---  This is the decomposition loop of the tridiagonal algorithm.
!     TBLDDY and TBLU are used as temporary storage of the decomposed
!     factors.  ---
!
        DO 11 I = ITS+1,ITE-1
          SIG = (TBLX(I)-TBLX(I-1))/(TBLX(I+1)-TBLX(I-1))
          P = SIG*TBLDDY(I-1)+2.D+0
          TBLDDY(I) = (SIG-1.D+0)/P
          TBLU(I) = (6.D+0*((TBLY(I+1)-TBLY(I))/(TBLX(I+1)-TBLX(I)) -
     &      (TBLY(I)-TBLY(I-1))/(TBLX(I)-TBLX(I-1)))/
     &      (TBLX(I+1)-TBLX(I-1)) - SIG*TBLU(I-1))/P
   11   CONTINUE
        TBLD = (TBLY(ITE)-TBLY(ITE-1))/(TBLX(ITE)-TBLX(ITE-1))
!
!---  The upper boundary condition is set either to be natural or
!     else to have a specified first derivative.  ---
!
!        IF( TBLD.GT.0.99D+20 ) THEN
          TBLDDY(ITE) = 0.D+0
          TBLU(ITE) = 0.D+0
!        ELSE
!          TBLDDY(ITE) = 0.5D+0
!          TBLU(ITE) = (3.D+0/(TBLX(ITE)-TBLX(ITE-1)))*
!     &      (TBLD-(TBLY(ITE)-TBLY(ITE-1))/(TBLX(ITE)-TBLX(ITE-1)))
!        ENDIF
!
!---  This is the backsubstitution loop of the tridiagonal algorthim
!
        TBLDDY(ITE) = (TBLU(ITE)-TBLDDY(ITE)*TBLU(ITE-1))/
     &    (TBLDDY(ITE)*TBLDDY(ITE-1)+1.D+0)
        DO 12 K = ITE-1,ITS,-1
          TBLDDY(K) = TBLDDY(K)*TBLDDY(K+1)+TBLU(K)
   12   CONTINUE
!
!---  Descending table order  ---
!
      ELSEIF( TBLX(ITE).LT.TBLX(ITS) ) THEN
        TBLD = (TBLY(ITE-1)-TBLY(ITE))/(TBLX(ITE-1)-TBLX(ITE))
!
!---  The lower boundary condition is set either to be natural or
!     else to have a specified first derivative.  ---
!
!        IF( TBLD.GT.0.99D+20 ) THEN
          TBLDDY(ITE) = 0.D+0
          TBLU(ITE) = 0.D+0
!        ELSE
!          TBLDDY(ITE) = -0.5D+0
!          TBLU(ITE) = (3.D+0/(TBLX(ITE-1)-TBLX(ITE)))*
!     &      ((TBLY(ITE-1)-TBLY(ITE))/(TBLX(ITE-1)-TBLX(ITE))-TBLD)
!        ENDIF
!
!---  This is the decomposition loop of the tridiagonal algorithm.
!     TBLDDY and TBLU are used as temporary storage of the decomposed
!     factors.  ---
!
        DO 21 I = ITE-1,ITS+1,-1
          SIG = (TBLX(I)-TBLX(I+1))/(TBLX(I-1)-TBLX(I+1))
          P = SIG*TBLDDY(I+1)+2.D+0
          TBLDDY(I) = (SIG-1.D+0)/P
          TBLU(I) = (6.D+0*((TBLY(I-1)-TBLY(I))/(TBLX(I-1)-TBLX(I)) -
     &      (TBLY(I)-TBLY(I+1))/(TBLX(I)-TBLX(I+1)))/
     &      (TBLX(I-1)-TBLX(I+1)) - SIG*TBLU(I+1))/P
   21   CONTINUE
        TBLD = (TBLY(ITS)-TBLY(ITS+1))/(TBLX(ITS)-TBLX(ITS+1))
!
!---  The upper boundary condition is set either to be natural or
!     else to have a specified first derivative.  ---
!
!        IF( TBLD.GT.0.99D+20 ) THEN
          TBLDDY(ITS) = 0.D+0
          TBLU(ITS) = 0.D+0
!        ELSE
!          TBLDDY(ITS) = 0.5D+0
!          TBLU(ITS) = (3.D+0/(TBLX(ITS)-TBLX(ITS+1)))*
!     &      (TBLD-(TBLY(ITS)-TBLY(ITS+1))/(TBLX(ITS)-TBLX(ITS+1)))
!        ENDIF
!
!---  This is the backsubstitution loop of the tridiagonal algorthim
!
        TBLDDY(ITS) = (TBLU(ITS)-TBLDDY(ITS)*TBLU(ITS+1))/
     &    (TBLDDY(ITS)*TBLDDY(ITS+1)+1.D+0)
        DO 22 K = ITS+1,ITE
          TBLDDY(K) = TBLDDY(K)*TBLDDY(K-1)+TBLU(K)
   22   CONTINUE
      ELSE
        INDX = 3
        CHMSG = 'Invalid Table'
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of SPLINY group
!
      RETURN
      END


