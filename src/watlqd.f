!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WATLQD( TX,PX,RHO )
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
!     Calculate the subcooled or saturated density, as a function of
!     temperature and pressure per the steam table equations
!     as given by the 1967 International Formulation Committee:
!     Formulation for Industrial Use.
!
!     Thermodynamic and Transport Properties of Steam.
!     1967. ASME Steam Tables.
!     The American Society of Mechanical Engineers.
!     United Engineering Center, 345 East 47th Street, New York, N.Y.
!
!     The temperature is limited in this subroutine to the following
!     values:  0.01 C < T > 364.0 C
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, January, 1992.
!     Last Modified by MD White, Battelle, PNL, January 14, 1992.
!     $Id: watlqd.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE CONST
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
      REAL*8 A(23),SA(12),B(10)
!
!----------------------Data Statements---------------------------------!
!
      SAVE A,SA,B
      DATA A /6.824687741D+3,-5.422063673D+2,-2.096666205D+4,
     &3.941286787D+4,-6.733277739D+4,9.902381028D+4,-1.093911774D+5,
     &8.590841667D+4,-4.511168742D+4,1.418138926D+4,-2.017271113D+3,
     &7.982692717D+0,-2.616571843D-2,1.522411790D-3,2.284279054D-2,
     &2.421647003D+2,1.269716088D-10,2.074838328D-7,2.174020350D-8,
     &1.105710498D-9,1.293441934D+1,1.308119072D-5,6.047626338D-14/
      DATA SA /8.438375405D-1,5.362162162D-4,1.720D+0,7.342278489D-2,
     &4.975858870D-2,6.537154300D-1,1.15D-6,1.5108D-5,1.4188D-1,
     &7.002753165D+0,2.995284926D-4,2.040D-1/
      DATA B/9.99667D+2,6.85021D-2,-7.0966D-3,2.76483D-5,-5.4108D-8,
     &5.20175D-7,-7.41396D-9,1.41879D-10,-8.82877D-13,1.92152D-15/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WATLQD'
      IF( INDEX(SVN_ID(269)(1:1),'$').EQ.0 ) SVN_ID(269) =
     & '$Id: watlqd.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Constant aqueous density  ---
!
      IF( ISLC(9).EQ.1 .OR. ISLC(53).EQ.1 ) THEN
        RHO = RHOLI
!
!---  Polynomial formulation  ---
!
      ELSEIF( IOM.NE.3 .AND. IOM.NE.13 ) THEN
        RHO = B(1) + PX*B(6)
        DO 10 M = 1,4
          RHO = RHO + B(M+1)*(TX**M) + PX*B(M+6)*(TX**M)
   10   CONTINUE
!
!---  ASME formulation  ---
!
      ELSE
        TR = MIN( (TX+TABS)/TCRW,1.D+0 )
        PR = PX/PCRW
        YC = 1.0D+0 - SA(1)*TR*TR-SA(2)/(TR**6)
        ZC = YC +
     &    SQRT(MAX( ZERO,(SA(3)*YC*YC-2.D+0*SA(4)*TR+2.D+0*SA(5)*PR)))
        CHI = A(12)*SA(5)/ZC**(2.9412D-1) +A(13) +A(14)*TR +A(15)*TR*TR
     &    +A(16)*(SA(6)-TR)**10 +A(17)/(SA(7)+TR**19)
     &    -(A(18)+2.D+0*A(19)*PR+3.D+0*A(20)*PR*PR)/(SA(8)+TR**11)
     &    -(A(21)*TR**18*(SA(9)+TR*TR)*(-3.D+0/((SA(10)+PR)**4)+SA(11)))
     &    +3.D+0*A(22)*(SA(12)-TR)*PR*PR +4.D+0*A(23)*PR*PR*PR/(TR**20)
        RHO = 1.D+0/(CHI*VCRW*1.D-3/WTMW)
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WATLQD group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ELC_DEN( RHOLX,CLX,DCFX )
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
!     Compute brine density from brine solution mass fraction and
!     liquid water density.
!
!     Leijnse, A.  1992.  Three-Dimensional Modeling of Coupled Flow
!     and Transport in Porous Media.  Ph.D. Dissertation, Department
!     of Civil Engineering and Geological Sciences, University of
!     Notre Dame, Notre Dame, Indiana.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNNL, October 2000.
!     Last Modified by MD White, Battelle, PNNL, October 9, 2000.
!     $Id: watlqd.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
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
      REAL*8 DCFX(4)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ELC_DEN'
      IF( INDEX(SVN_ID(269)(1:1),'$').EQ.0 ) SVN_ID(269) =
     & '$Id: watlqd.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Electrolyte aqeuous density function by A. Leijnse  ---
!
      IF( IDF_ELC.EQ.1 ) THEN
        NC = 0
        RHOLSX = RHOLX
   10   CONTINUE
        NC = NC+1
        F = RHOLSX - RHOLX*EXP(DCFX(1)*CLX/RHOLSX)
        DF = 1.D+0 + RHOLX*DCFX(1)*CLX*EXP(DCFX(1)*CLX/RHOLSX)/
     &    (RHOLSX**2)
        DRHOX = -F/DF
        RHOLSX = RHOLSX + DRHOX
        IF( NC.GT.32 ) THEN
          INDX = 3
          CHMSG = 'Convergence Failure on Electrolyte Density'
          CALL WRMSGS( INDX )
        ENDIF
        IF( ABS(DRHOX/RHOLSX).GT.1.D-9 ) GOTO 10
        RHOLX = RHOLSX
!
!---  Fourth-order electrolyte aqeuous density function  ---
!
      ELSEIF( IDF_ELC.EQ.2 ) THEN
        CLXX = CLX/ELC_DUN
        RHOLX = RHOLX*(DCFX(1) + DCFX(2)*CLXX + DCFX(3)*(CLXX**2) +
     &    DCFX(4)*(CLXX**3))
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ELC_DEN group  ---
!
      RETURN
      END

!----------------------Function----------------------------------------!
!
      FUNCTION WLQDF( TX )
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
!     Water liquid density function.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNNL, 16 May 2001.
!     Last Modified by MD White, Battelle, PNNL, 16 May 2001.
!     $Id: watlqd.F 1080 2017-03-14 16:22:02Z d3c002 $
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
      REAL*8 B(10)
!
!----------------------Data Statements---------------------------------!
!
      SAVE B
      DATA B/9.99667D+2,6.85021D-2,-7.0966D-3,2.76483D-5,-5.4108D-8,
     &5.20175D-7,-7.41396D-9,1.41879D-10,-8.82877D-13,1.92152D-15/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WLQDF'
      IF( INDEX(SVN_ID(269)(1:1),'$').EQ.0 ) SVN_ID(269) =
     & '$Id: watlqd.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Water liquid density function  ---
!
      WLQDF = B(1)+B(2)*TX+B(3)*(TX**2)+B(4)*(TX**3)+B(5)*(TX**4)
!
!---  End of WLQDF group
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Function----------------------------------------!
!
      FUNCTION WLQDG( TX )
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
!     Water liquid density function.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNNL, 16 May 2001.
!     Last Modified by MD White, Battelle, PNNL, 16 May 2001.
!     $Id: watlqd.F 1080 2017-03-14 16:22:02Z d3c002 $
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
      REAL*8 B(10)
!
!----------------------Data Statements---------------------------------!
!
      SAVE B
      DATA B/9.99667D+2,6.85021D-2,-7.0966D-3,2.76483D-5,-5.4108D-8,
     &5.20175D-7,-7.41396D-9,1.41879D-10,-8.82877D-13,1.92152D-15/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WLQDG'
      IF( INDEX(SVN_ID(269)(1:1),'$').EQ.0 ) SVN_ID(269) =
     & '$Id: watlqd.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Water liquid density function  ---
!
      WLQDG = B(6)+B(7)*TX+B(8)*(TX**2)+B(9)*(TX**3)+B(10)*(TX**4)
!
!---  End of WLQDG group
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

