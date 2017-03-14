!----------------------Function----------------------------------------!
!
      SUBROUTINE SPLINX( ITS,ITE )
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
!     Given arrays TBLY and TBLX containing a tabulated function
!     (i.e., x(n) = f(y(n))), with y(1) < y(2) < ... < y(n), this
!     routine returns an array TBLDDX which contains the second
!     derivatives of the interpolating function at the tabulated
!     points y(i).  The routine is structured to set the boundary
!     conditions at function end points to a natural spline, with zero
!     second derivatives on that boundary.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, September, 1994.
!     Last Modified by MD White, Battelle, PNL, September 9, 1994.
!     $Id: splinx.F 1080 2017-03-14 16:22:02Z d3c002 $
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
      SUB_LOG(ISUB_LOG) = '/SPLINX'
      IF( INDEX(SVN_ID(198)(1:1),'$').EQ.0 ) SVN_ID(198) =
     & '$Id: splinx.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Ascending table order  ---
!
      IF( TBLY(ITE).GT.TBLY(ITS) ) THEN
        TBLD = (TBLX(ITS+1)-TBLX(ITS))/(TBLY(ITS+1)-TBLY(ITS))
!
!---  The lower boundary condition is set either to be natural or
!     else to have a specified first derivative.  ---
!
!        IF( TBLD.GT.0.99D+20 ) THEN
          TBLDDX(ITS) = 0.D+0
          TBLU(ITS) = 0.D+0
!        ELSE
!          TBLDDX(ITS) = -0.5D+0
!          TBLU(ITS) = (3.D+0/(TBLY(ITS+1)-TBLY(ITS)))*
!     &      ((TBLX(ITS+1)-TBLX(ITS))/(TBLY(ITS+1)-TBLY(ITS))-TBLD)
!        ENDIF
!
!---  This is the decomposition loop of the tridiagonal algorithm.
!     TBLDDX and TBLU are used as temporary storage of the decomposed
!     factors.  ---
!
        DO 11 I = ITS+1,ITE-1
          SIG = (TBLY(I)-TBLY(I-1))/(TBLY(I+1)-TBLY(I-1))
          P = SIG*TBLDDX(I-1)+2.D+0
          TBLDDX(I) = (SIG-1.D+0)/P
          TBLU(I) = (6.D+0*((TBLX(I+1)-TBLX(I))/(TBLY(I+1)-TBLY(I)) -
     &      (TBLX(I)-TBLX(I-1))/(TBLY(I)-TBLY(I-1)))/
     &      (TBLY(I+1)-TBLY(I-1)) - SIG*TBLU(I-1))/P
   11   CONTINUE
        TBLD = (TBLX(ITE)-TBLX(ITE-1))/(TBLY(ITE)-TBLY(ITE-1))
!
!---  The upper boundary condition is set either to be natural or
!     else to have a specified first derivative.  ---
!
!        IF( TBLD.GT.0.99D+20 ) THEN
          TBLDDX(ITE) = 0.D+0
          TBLU(ITE) = 0.D+0
!        ELSE
!          TBLDDX(ITE) = 0.5D+0
!          TBLU(ITE) = (3.D+0/(TBLY(ITE)-TBLY(ITE-1)))*
!     &      (TBLD-(TBLX(ITE)-TBLX(ITE-1))/(TBLY(ITE)-TBLY(ITE-1)))
!        ENDIF
!
!---  This is the backsubstitution loop of the tridiagonal algorthim
!
        TBLDDX(ITE) = (TBLU(ITE)-TBLDDX(ITE)*TBLU(ITE-1))/
     &    (TBLDDX(ITE)*TBLDDX(ITE-1)+1.D+0)
        DO 12 K = ITE-1,ITS,-1
          TBLDDX(K) = TBLDDX(K)*TBLDDX(K+1)+TBLU(K)
   12   CONTINUE
!
!---  Descending table order  ---
!
      ELSEIF( TBLY(ITE).LT.TBLY(ITS) ) THEN
        TBLD = (TBLX(ITE-1)-TBLX(ITE))/(TBLY(ITE-1)-TBLY(ITE))
!
!---  The lower boundary condition is set either to be natural or
!     else to have a specified first derivative.  ---
!
!        IF( TBLD.GT.0.99D+20 ) THEN
          TBLDDX(ITE) = 0.D+0
          TBLU(ITE) = 0.D+0
!        ELSE
!          TBLDDX(ITE) = -0.5D+0
!          TBLU(ITE) = (3.D+0/(TBLY(ITE-1)-TBLY(ITE)))*
!     &      ((TBLX(ITE-1)-TBLX(ITE))/(TBLY(ITE-1)-TBLY(ITE))-TBLD)
!        ENDIF
!
!---  This is the decomposition loop of the tridiagonal algorithm.
!     TBLDDX and TBLU are used as temporary storage of the decomposed
!     factors.  ---
!
        DO 21 I = ITE-1,ITS+1,-1
          SIG = (TBLY(I)-TBLY(I+1))/(TBLY(I-1)-TBLY(I+1))
          P = SIG*TBLDDX(I+1)+2.D+0
          TBLDDX(I) = (SIG-1.D+0)/P
          TBLU(I) = (6.D+0*((TBLX(I-1)-TBLX(I))/(TBLY(I-1)-TBLY(I)) -
     &      (TBLX(I)-TBLX(I+1))/(TBLY(I)-TBLY(I+1)))/
     &      (TBLY(I-1)-TBLY(I+1)) - SIG*TBLU(I+1))/P
   21   CONTINUE
        TBLD = (TBLX(ITS)-TBLX(ITS+1))/(TBLY(ITS)-TBLY(ITS+1))
!
!---  The upper boundary condition is set either to be natural or
!     else to have a specified first derivative.  ---
!
!        IF( TBLD.GT.0.99D+20 ) THEN
          TBLDDX(ITS) = 0.D+0
          TBLU(ITS) = 0.D+0
!        ELSE
!          TBLDDX(ITS) = 0.5D+0
!          TBLU(ITS) = (3.D+0/(TBLY(ITS)-TBLY(ITS+1)))*
!     &      (TBLD-(TBLX(ITS)-TBLX(ITS+1))/(TBLY(ITS)-TBLY(ITS+1)))
!        ENDIF
!
!---  This is the backsubstitution loop of the tridiagonal algorthim
!
        TBLDDX(ITS) = (TBLU(ITS)-TBLDDX(ITS)*TBLU(ITS+1))/
     &    (TBLDDX(ITS)*TBLDDX(ITS+1)+1.D+0)
        DO 22 K = ITS+1,ITE
          TBLDDX(K) = TBLDDX(K)*TBLDDX(K-1)+TBLU(K)
   22   CONTINUE
      ELSE
        INDX = 3
        CHMSG = 'Invalid Table'
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of SPLINX group
!
      RETURN
      END


