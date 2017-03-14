!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WATGSD( TX,PX,RHOX,INDX )
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
!     INDX = 0
!
!     Calculate the water vapor component density
!     with the ideal gas law equation of state.
!
!     INDX = 1
!
!     Calculate the water vapor component density, as a function of
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
!     $Id: watgsd.F 1080 2017-03-14 16:22:02Z d3c002 $
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
      REAL*8 BX(31),SBX(5),L(3)
!
!----------------------Data Statements---------------------------------!
!
      SAVE BX,SBX,L,SL1
      DATA BX /1.683599274D+1,2.856067796D+1,-5.438923329D+1,
     &4.330662834D-1,
     &-6.547711697D-1,8.565182058D-2,6.670375918D-2,1.388983801D+0,
     &8.390104328D-2,2.614670893D-2,-3.373439453D-2,4.520918904D-1,
     &1.069036614D-1,-5.975336707D-1,-8.847535804D-2,5.958051609D-1,
     &-5.159303373D-1,2.075021122D-1,1.190610271D-1,-9.867174132D-2,
     &1.683998803D-1,-5.809438001D-2,6.552390126D-3,5.710218649D-4,
     &1.936587558D+2,-1.388522425D+3,4.126607219D+3,-6.508211677D+3,
     &5.745984054D+3,-2.693088365D+3,5.235718623D+2/
      DATA SBX /7.633333333D-1,4.006073948D-1,8.636081627D-2,
     &-8.532322921D-1,3.460208861D-1/
      DATA L/1.574373327D+1,-3.417061978D+1,1.931380707D+1/
      DATA SL1/4.260321148D+0/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WATGSD'
      IF( INDEX(SVN_ID(263)(1:1),'$').EQ.0 ) SVN_ID(263) =
     & '$Id: watgsd.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      IF( INDX .EQ. 0 ) THEN
        RHOX = PX/((TX+TABS)*RCW)
      ELSEIF( INDX .EQ. 1 ) THEN
        IF( PX .LT. SMALL ) THEN
          RHOX = 0.D+0
          ISUB_LOG = ISUB_LOG-1
          RETURN
        ENDIF
        TR = (TX+TABS)/TCRW
        PR = PX/PCRW
        CX = EXP(SBX(1)*(1.D+0-TR))
        BL = L(1) + L(2)*TR + L(3)*TR*TR
!----------------------------------------------------------------------!
!     Compute the vapor density.
!----------------------------------------------------------------------!
        CHX = SL1*TR/PR
        CHX = CHX -(BX(7)*CX**13 + BX(8)*CX**3)
        CHX = CHX -2.D+0*PR*(BX(9)*CX**18 + BX(10)*CX**2 + BX(11)*CX)
        CHX = CHX -3.D+0*(PR**2)*(BX(12)*CX**18 + BX(13)*CX**10)
        CHX = CHX -4.D+0*(PR**3)*(BX(14)*CX**25 + BX(15)*CX**14)
        CHX = CHX -5.D+0*(PR**4)*
     &    (BX(16)*CX**32 + BX(17)*CX**28 + BX(18)*CX**24)
        CHX = CHX -4.D+0*(1.D+0/(PR**5))*(BX(19)*CX**12 +
     &    BX(20)*CX**11)/
     &    (((1.D+0/(PR**4))+(SBX(2)*CX**14))**2)
        CHX = CHX -5.D+0*(1.D+0/(PR**6))*(BX(21)*CX**24 +
     &    BX(22)*CX**18)/
     &    (((1.D+0/(PR**5))+(SBX(3)*CX**19))**2)
        CHX = CHX -6.D+0*(1.D+0/(PR**7))*(BX(23)*CX**24 +
     &    BX(24)*CX**14)/
     &    (((1.D+0/(PR**6))+(SBX(4)*CX**54 + SBX(5)*CX**27))**2)
        T1 = 1.1D+1*((PR/BL)**10)
        CHX = CHX + T1*BX(25)
        CHX = CHX + T1*BX(26)*CX
        CHX = CHX + T1*BX(27)*CX**2
        CHX = CHX + T1*BX(28)*CX**3
        CHX = CHX + T1*BX(29)*CX**4
        CHX = CHX + T1*BX(30)*CX**5
        CHX = CHX + T1*BX(31)*CX**6
        RHOX = 1.D+0/(CHX*VCRW*1.D-3/WTMW)
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WATGSD group  ---
!
      RETURN
      END

