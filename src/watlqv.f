!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WATLQV( TX,PX,PSWX,VIS )
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
!     Calculate the subcooled or saturated viscosity, as a function of
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
!     $Id: watlqv.F 1080 2017-03-14 16:22:02Z d3c002 $
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
      REAL*8 K(4)
!
!----------------------Data Statements---------------------------------!
!
      SAVE K
      DATA K/-2.471D+1, 4.209D+3, 4.527D-02, -3.376D-5/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WATLQV'
      IF( INDEX(SVN_ID(272)(1:1),'$').EQ.0 ) SVN_ID(272) =
     & '$Id: watlqv.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      TK = MIN( TX+TABS,TCRW )
      DPX = MAX( PX,PSWX ) - PSWX
      PHI = 1.D+0 + 1.0467D+0*(TK-3.05D+2)*DPX/1.D+11
      VIS = PHI*EXP( K(1) + K(2)/TK + K(3)*TK + K(4)*TK*TK )*1.D-3
      IF( ISLC(9).EQ.1 ) VIS = VISLI
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WATLQV group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ELC_VIS( VISLX,CLX,VCFX )
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
!     Compute brine viscosity from brine solution mass fraction and
!     liquid water viscosity.
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
!     $Id: watlqv.F 1080 2017-03-14 16:22:02Z d3c002 $
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
      REAL*8 VCFX(4)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ELC_VIS'
      IF( INDEX(SVN_ID(272)(1:1),'$').EQ.0 ) SVN_ID(272) =
     & '$Id: watlqv.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Electrolyte aqeuous viscosity function by A. Leijnse  ---
!
      IF( IVF_ELC.EQ.1 ) THEN
        VISLX = VISLX*(VCFX(1) + VCFX(2)*CLX + VCFX(3)*(CLX**2)
     &    + VCFX(4)*(CLX**3))
!
!---  Fourth-order electrolyte aqeuous density function  ---
!
      ELSEIF( IDF_ELC.EQ.2 ) THEN
        CLXX = CLX/ELC_VUN
        VISLX = VISLX*(VCFX(1) + VCFX(2)*CLXX + VCFX(3)*(CLXX**2)
     &    + VCFX(4)*(CLXX**3))
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ELC_VIS group  ---
!
      RETURN
      END


