!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE POLINT( XA,YA,N,X,Y,DY,IERR )
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
!     Given arrays XA and YA, each of length N, and given a value X,
!     this routine returns a value Y, and an error estimate DY.
!     If P(X) is the polynomial of degree N-1 such that 
!     P(XA(I)) = YA(I), I = 1,N, then the returned value Y = P(X).
!
!     Press, W.H., B.P. Flannery, S.A. Teukolsky, and W.T. Vetterling.
!     1986.  Numerical Recipes, The Art of Scientific Computing.
!     Cambridge University Press, Cambridge.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, February, 1999.
!     Last Modified by Mark White, Battelle, February 19, 1999.
!     $Id: rombint.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
      PARAMETER( NMAX=10, EPSL=1.D-14 )
      REAL*8 XA(N),YA(N),C(NMAX),D(NMAX)
!
!----------------------Executable Lines--------------------------------!
!
      IERR = 0
      NS = 1
      DIF = ABS(X-XA(1))
!
!---  Find the index NS of the closest table entry  ---
!
      DO 11 I = 1,N
        DIFT = ABS(X-XA(I))
        IF( DIFT.LT.DIF ) THEN
          NS = I
          DIF = DIFT
        ENDIF
!
!---  Initialize the tableau fo C's and D's  ---
!
        C(I) = YA(I)
        D(I) = YA(I)
   11 CONTINUE
!
!---  Initial approximation to Y  ---
!
      Y = YA(NS)
      NS = NS-1
!
!---  Loop over the columns in the tableau and 
!     update the C's and D's  ---
!
      DO 13 M = 1,N-1
        DO 12 I = 1,N-M
          HO = XA(I)-X
          HP = XA(I+M)-X
          W = C(I+1)-D(I)
          DEN = HO-HP
!
!---  Exit subroutine if two input XA's are identical, 
!     within roundoff  ---
!
          IF( ABS( DEN )/EPSL.LT.EPSL ) THEN
            IERR = 1
            RETURN
          ENDIF
          DEN = W/DEN
!
!---  Update C's and D's  ---
!
          D(I) = HP*DEN
          C(I) = HO*DEN
   12   CONTINUE
!
!---  After each column in the tableau is completed, 
!     decide which direction C or D to add the accumulating 
!     value of Y, (i.e., which path to take
!     through the tableau - forking up or down).  
!     Do this in such a way as to take the most "straight line" 
!     route through the tableau to its apex, updating NS accordingly 
!     to keep track.  This route keeps the partial approximations 
!     centered (insofar as possible) on the target X.  The
!     last DY added is thus the error indication.  ---
!
        IF( 2*NS.LT.N-M ) THEN
          DY = C(NS+1)
        ELSE
          DY = D(NS)
          NS = NS-1
        ENDIF
        Y = Y+DY
   13 CONTINUE
!
!---  End of POLINT group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE QROMB( FUNC,V,A,B,SSX,IERR,IV )
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
!     Returns as S the integral of the function FUNC from A to B.
!     Integration is performed by Romberg's method of order 2K, where
!     e.g., K=2 is Simpson's rule.
!
!     Press, W.H., B.P. Flannery, S.A. Teukolsky, and W.T. Vetterling.
!     1986.  Numerical Recipes, The Art of Scientific Computing.
!     Cambridge University Press, Cambridge.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, November 19, 1999.
!     Last Modified by Mark White, Battelle, November 19, 1999.
!     $Id: rombint.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
      PARAMETER( EPS=1.D-6, EPSL=1.D-14 )
      PARAMETER( JMAX=20, JMAXP=JMAX+1, K=5, KM=K-1 )
!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 S(JMAXP),H(JMAXP),V(10)
      INTEGER IV(10)
      EXTERNAL FUNC
!
!----------------------Executable Lines--------------------------------!
!
      IERR = 0
      ZERO = 0.D+0
!
!---  S and H store the successive trapezodial approximations and their
!     relative step-sizes.  ---
!
      H(1) = 1.D+0
      DO 10 J = 1,JMAX
        CALL TRAPZD( FUNC,V,A,B,S(J),J,IV )
        IF( J.GE.K ) THEN
          CALL POLINT( H(J-KM),S(J-KM),K,ZERO,SSX,DSS,IERR )
          IF( (ABS(DSS)-EPS*ABS(SSX))/EPSL.LT.EPSL ) RETURN
        ENDIF
        S(J+1) = S(J)
        H(J+1) = 2.5D-1*H(J)
   10 CONTINUE
      IERR = 1
!
!---  End of QROMB group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE QSIMP( FUNC,V,A,B,S,IERR )
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
!     Returns as S the integral of the function FUNC from A to B.
!     Integration is performed by Simpson's rule.
!
!     Press, W.H., B.P. Flannery, S.A. Teukolsky, and W.T. Vetterling.
!     1986.  Numerical Recipes, The Art of Scientific Computing.
!     Cambridge University Press, Cambridge.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, February, 1999.
!     Last Modified by Mark White, Battelle, February 19, 1999.
!     $Id: rombint.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
      PARAMETER( EPS=1.D-6, JMAX=20 )
!
!-------------------------Type Statements------------------------------!
!
      EXTERNAL FUNC
      REAL*8 V(10)
      INTEGER IV(10)
!
!----------------------Executable Lines--------------------------------!
!
      OST = 1.D+20
      OS = -1.D+20
      DO 100 J = 1,JMAX
        CALL TRAPZD( FUNC,V,A,B,ST,J,IV )
        S = (4.D+0*ST-OST)/3.D+0
        IF( ABS(S-OS).LT.EPS*ABS(OS)) GOTO 200
        OS = S
        OST = ST
  100 CONTINUE
      IERR = 1
  200 CONTINUE
!
!---  End of QSIMP group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE TRAPZD( FUNC,V,A,B,S,N,IV )
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
!     This routine computes the N'th stage of refinement of an
!     extended trapezoid rule.
!
!     Press, W.H., B.P. Flannery, S.A. Teukolsky, and W.T. Vetterling.
!     1986.  Numerical Recipes, The Art of Scientific Computing.
!     Cambridge University Press, Cambridge.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, February, 1999.
!     Last Modified by Mark White, Battelle, February 19, 1999.
!     $Id: rombint.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!-------------------------Type Statements------------------------------!
!
      EXTERNAL FUNC
      REAL*8 V(10)
      INTEGER IV(10)
      SAVE IT
!
!----------------------Executable Lines--------------------------------!
!
      IF( N.EQ.1 ) THEN
        S = 5.D-1*(B-A)*(FUNC(A,V,IV)+FUNC(B,V,IV))
!
!---  IT is the number of points to be added on the next call  ---
!
        IT = 1
      ELSE
        REALX = REAL(IT)
        TNM = REALX
!
!---  Spacing of the points to be added.  ---
!
        DEL = (B-A)/TNM
        X = A + 5.D-1*DEL
        SUM = 0.D+0
        DO 100 J = 1,IT
          SUM = SUM + FUNC(X,V,IV)
          X = X + DEL
  100   CONTINUE
!
!---  Replace S by its refined value  ---
!
        S = 5.D-1*(S+(B-A)*SUM/TNM)
        IT = 2*IT
      ENDIF
!
!---  End of TRAPZD group
!
      RETURN
      END

!----------------------Function----------------------------------------!
!
      FUNCTION ADRM( TMX,V,IV )
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
!     Inverse van Genuchten function.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, February, 1999.
!     Last Modified by Mark White, Battelle, February 19, 1999.
!     $Id: rombint.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
      PARAMETER( SMALL=1.D-20 )
!
!-------------------------Type Statements------------------------------!
!
      REAL*8 V(10)
      INTEGER IV(10)
!
!----------------------Executable Lines--------------------------------!
!
      ADRM = 0.D+0
      FCTX = 1.D+0
      DO 200 N = 1,IV(1)
        REALX = REAL(N-1)
        IF( N.GT.1 ) FCTX = FCTX*REALX
        REALX = REAL(IV(1))
        ADRM = ADRM + ((V(1)*REALX*TMX)**(N-1))/FCTX
  200 CONTINUE
      ADRM = ADRM*V(1)*V(2)*EXP(-V(1)*REALX*TMX)
!
!---  End of ADRM group
!
      RETURN
      END

!----------------------Function----------------------------------------!
!
      FUNCTION HKI( SLX,V,IV )
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
!     Inverse Haverkamp function.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 17 December 2002.
!     Last Modified by Mark White, PNNL,  17 December 2002.
!     $Id: rombint.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
      PARAMETER( EPSL=1.D-14, SMALL=1.D-20 )
!
!-------------------------Type Statements------------------------------!
!
      REAL*8 V(10)
      INTEGER IV(10)
!
!----------------------Executable Lines--------------------------------!
!
      IF( (1.D+0-SLX).LT.EPSL ) THEN
        HKI = V(1)
      ELSE
        HKI = V(1) + V(5)*((V(2)*(1.D+0-SLX)/(SLX+SMALL))**
     &    (1.D+0/V(3)))
      ENDIF
!
!---  End of HKI group
!
      RETURN
      END

!----------------------Function----------------------------------------!
!
      FUNCTION SPLNI( SLX,V,IV )
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
!     Inverse cubic-spline or log-cubic-spline tabular function.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 17 December 2002.
!     Last Modified by Mark White, PNNL,  17 December 2002.
!     $Id: rombint.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!-------------------------Type Statements------------------------------!
!
      REAL*8 V(10)
      INTEGER IV(10)
      EXTERNAL FSPLNX
!
!----------------------Executable Lines--------------------------------!
!
        SPLNI = FSPLNX( SLX,IV(1),IV(2) )
!
!---  End of SPLNI group
!
      RETURN
      END

!----------------------Function----------------------------------------!
!
      FUNCTION TBLI( SLX,V,IV )
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
!     Inverse linear or log-linear tabular function.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 17 December 2002.
!     Last Modified by Mark White, PNNL,  17 December 2002.
!     $Id: rombint.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!-------------------------Type Statements------------------------------!
!
      REAL*8 V(10)
      INTEGER IV(10)
      EXTERNAL FNTBLX
!
!----------------------Executable Lines--------------------------------!
!
        TBLI = FNTBLX( SLX,IV(1),IV(2),IV(3) )
!
!---  End of TBLI group
!
      RETURN
      END

!----------------------Function----------------------------------------!
!
      FUNCTION VGI( SLX,V,IV )
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
!     Inverse van Genuchten function.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, February, 1999.
!     Last Modified by Mark White, Battelle, February 19, 1999.
!     $Id: rombint.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
      PARAMETER( SMALL=1.D-20 )
!
!-------------------------Type Statements------------------------------!
!
      REAL*8 V(10)
      INTEGER IV(10)
!
!----------------------Executable Lines--------------------------------!
!
      CNX = V(1)
      CMX = V(2)
      VGI = (-1.D+0 + (1.D+0/(SLX+SMALL))**(1.D+0/CMX))**(1.D+0/CNX)
!
!---  End of VGI group
!
      RETURN
      END

!----------------------Function----------------------------------------!
!
      FUNCTION VGTI( SLX,V,IV )
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
!     Inverse van Genuchten function.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, February, 1999.
!     Last Modified by Mark White, Battelle, February 19, 1999.
!     $Id: rombint.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
      PARAMETER( SMALL=1.D-20 )
!
!-------------------------Type Statements------------------------------!
!
      REAL*8 V(10)
      INTEGER IV(10)
!
!----------------------Executable Lines--------------------------------!
!
      CNX = V(1)
      CMX = V(2)
      CRX = V(3)
      VGTI = (-1.D+0 + (1.D+0/(SLX+SMALL))**(1.D+0/CMX))**(1.D+0/CNX)
      VGTI = VGTI*SLX/((-1.D+0 + (-1.D+0 + SLX)*CRX)**2)
!
!---  End of VGTI group
!
      RETURN
      END

!----------------------Function----------------------------------------!
!
      FUNCTION BCI( SLX,V,IV )
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
!     Inverse Brooks-Corey function.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, February, 1999.
!     Last Modified by Mark White, Battelle, February 19, 1999.
!     $Id: rombint.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
      PARAMETER( EPSL=1.D-14 )
!
!-------------------------Type Statements------------------------------!
!
      REAL*8 V(10)
      INTEGER IV(10)
!
!----------------------Executable Lines--------------------------------!
!
      CPX = V(1)
      CLX = V(2)
      IF( (1.D+0-SLX).LT.EPSL ) THEN
        BCI = CPX
      ELSE
        BCI = CPX/(SLX**(1.D+0/CLX))
      ENDIF
!
!---  End of BCI group
!
      RETURN
      END

!----------------------Function----------------------------------------!
!
      FUNCTION BCTI( SLX,V,IV )
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
!     Inverse Brooks-Corey function.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, February, 1999.
!     Last Modified by Mark White, Battelle, February 19, 1999.
!     $Id: rombint.F 1080 2017-03-14 16:22:02Z d3c002 $
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
      PARAMETER( EPSL=1.D-14 )
!
!-------------------------Type Statements------------------------------!
!
      REAL*8 V(10)
      INTEGER IV(10)
!
!----------------------Executable Lines--------------------------------!
!
      CPX = V(1)
      CLX = V(2)
      CRX = V(3)
      IF( (1.D+0-SLX).LT.EPSL ) THEN
        BCTI = CPX*SLX/((-1.D+0 + (-1.D+0 + SLX)*CRX)**2)
      ELSE
        BCTI = CPX*SLX/(SLX**(1.D+0/CLX))/
     &    ((-1.D+0 + (-1.D+0 + SLX)*CRX)**2)
      ENDIF
!
!---  End of BCTI group
!
      RETURN
      END


