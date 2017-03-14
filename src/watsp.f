!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WATSP( TX,PX )
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
!     Calculate the saturation pressure of water as a function of
!     temperature per the 1967 International Formulation Committee
!     Formulation for Industrial Use.
!
!     Thermodynamic and Transport Properties of Steam.
!     1967. ASME Steam Tables.
!     The American Society of Mechanical Engineers.
!     United Engineering Center, 345 East 47th Street, New York, N.Y.
!
!     The temperature is limited in this subroutine to the following
!     values:  0.01 C < T > 364.0 !
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, January, 1992.
!     Last Modified by MD White, Battelle, PNL, January 14, 1992.
!     $Id: watsp.F 1080 2017-03-14 16:22:02Z d3c002 $
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
      REAL*8 K(9)
!
!----------------------Data Statements---------------------------------!
!
      SAVE K
      DATA K/-7.691234564D+0,-2.608023696D+1,-1.681706546D+2,
     &  6.423285504D+1,-1.189646225D+2,4.167117320D+0,2.097506760D+1,
     &  1.D+9,6.D+0/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WATSP'
      IF( INDEX(SVN_ID(273)(1:1),'$').EQ.0 ) SVN_ID(273) =
     & '$Id: watsp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      TR = MIN( (TX+TABS)/TCRW,1.D+0 )
      TR1 = 1.D+0-TR
      SVM = K(1)*(TR1**1) + K(2)*(TR1**2) + K(3)*(TR1**3)
     &  + K(4)*(TR1**4) + K(5)*(TR1**5)
      PR = EXP((SVM/(TR*(1.D+0+K(6)*TR1+K(7)*TR1*TR1)))
     &  + (TR1/(K(8)*TR1*TR1+K(9))))
      PX = PR*PCRW
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WATSP group  ---
!
      RETURN
      END

