!----------------------Subroutine--------------------------------------!
!
      FUNCTION THKE( IZN,SLX,SNX,SFWX,THKLX,THKGX,THKNX,THKFWX,
     &  PORDX,PORTX,INDX )
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
!     Calculates equivalent thermal conductivity.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, January, 1992.
!     Last Modified by WE Nichols, Battelle, PNL, February 13, 1995.
!     Last Modified by MD White, PNNL, 21 May, 2002.
!     $Id: thke.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
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
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/THKE'
      IF( INDEX(SVN_ID(234)(1:1),'$').EQ.0 ) SVN_ID(234) =
     & '$Id: thke.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Constant thermal conductivity  ---
!
      IF( ITHK(IZN).EQ.1 ) THEN
        THKE = THKS(INDX,IZN)
!
!---  Parallel thermal conductivity model  ---
!
      ELSEIF( ITHK(IZN).EQ.2 ) THEN
        THKE = (1.D+0-PORTX)*THKS(INDX,IZN) +
     &    (PORTX-PORDX)*THKLX +
     &    SLX*PORDX*THKLX + SNX*PORDX*THKNX + SFWX*PORDX*THKFWX
!
!---  Linear thermal conductivity model  ---
!
      ELSEIF( ITHK(IZN).EQ.3 ) THEN
        THKE = THKS(INDX,IZN) + SLX*(THKS(INDX+3,IZN)-THKS(1,IZN)) +
     &  SFWX*(THKS(INDX+3,IZN)*(THKFWX/THKLX)-THKS(1,IZN)) +
     &  SNX*(THKS(INDX+6,IZN)-THKS(INDX,IZN))
!
!---  Somerton thermal conductivity model  ---
!
      ELSEIF( ITHK(IZN).EQ.4 ) THEN
        STX = SLX+SNX+SFWX
        THKSX = THKS(INDX+3,IZN)*((1.D+0-PORDX) +
     &    (THKLX*SLX + THKNX*SNX + THKFWX*SFWX)*PORDX/THKRW)
        THKUX = THKS(INDX,IZN)*((1.D+0-PORDX) + THKGX*PORDX/THKRA)
        THKE = THKUX + SQRT(STX)*(THKSX-THKUX)
!
!---  Campbell thermal conductivity model (inappropriate for ice)  ---
!
      ELSEIF( ITHK(IZN).EQ.5 ) THEN
        WMCX = (PORTX-PORDX)+PORDX*SLX
        THKE = THKS(1,IZN)+THKS(2,IZN)*WMCX-(THKS(1,IZN)-THKS(4,IZN))*
     &    EXP(-((THKS(3,IZN)*WMCX)**THKS(5,IZN)))
!
!---  Jame and Norium thermal conductivity data  ---
!
      ELSEIF( ITHK(IZN).EQ.6 ) THEN
        THKE = 1.90551 - 1.57308D+0*EXP(-9.44058D+0*PORDX*SLX)
     &    + SFWX*PORDX*THKFWX
!
!---  Cass  ---
!
      ELSEIF( ITHK(IZN).EQ.7 ) THEN
        THKE = THKS(1,IZN)+THKS(2,IZN)*SLX-(THKS(1,IZN)-THKS(4,IZN))*
     &    EXP(-((THKS(3,IZN)*SLX)**THKS(5,IZN)))
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of THKE group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      FUNCTION THKE_FW( IZN,SLX,SFWX,THKLX,THKGX,THKFWX,
     &  PORDX,PORTX,INDX )
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
!     Calculates equivalent thermal conductivity.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, January, 1992.
!     Last Modified by WE Nichols, Battelle, PNL, February 13, 1995.
!     Last Modified by MD White, PNNL, 21 May, 2002.
!     $Id: thke.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
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
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/THKE_FW'
      IF( INDEX(SVN_ID(234)(1:1),'$').EQ.0 ) SVN_ID(234) =
     & '$Id: thke.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Constant thermal conductivity  ---
!
      IF( ITHK(IZN).EQ.1 ) THEN
        THKE_FW = THKS(INDX,IZN)
!
!---  Parallel thermal conductivity model  ---
!
      ELSEIF( ITHK(IZN).EQ.2 ) THEN
        THKE_FW = (1.D+0-PORTX)*THKS(INDX,IZN) +
     &    (PORTX-PORDX)*THKLX +
     &    SLX*PORDX*THKLX + SFWX*PORDX*THKFWX
!
!---  Linear thermal conductivity model  ---
!
      ELSEIF( ITHK(IZN).EQ.3 ) THEN
        THKE_FW = THKS(INDX,IZN) + SLX*(THKS(INDX+3,IZN)-THKS(1,IZN)) +
     &  SFWX*(THKS(INDX+3,IZN)*(THKFWX/THKLX)-THKS(1,IZN))
!
!---  Somerton thermal conductivity model  ---
!
      ELSEIF( ITHK(IZN).EQ.4 ) THEN
        STX = SLX+SFWX
        THKSX = THKS(INDX+3,IZN)*((1.D+0-PORDX) +
     &    (THKLX*SLX + THKFWX*SFWX)*PORDX/THKRW)
        THKUX = THKS(INDX,IZN)*((1.D+0-PORDX) + THKGX*PORDX/THKRA)
        THKE_FW = THKUX + SQRT(STX)*(THKSX-THKUX)
!
!---  Campbell thermal conductivity model (inappropriate for ice)  ---
!
      ELSEIF( ITHK(IZN).EQ.5 ) THEN
        WMCX = (PORTX-PORDX)+PORDX*SLX
        THKE_FW = THKS(1,IZN)+THKS(2,IZN)*WMCX-
     &    (THKS(1,IZN)-THKS(4,IZN))*
     &    EXP(-((THKS(3,IZN)*WMCX)**THKS(5,IZN)))
!
!---  Jame and Norium thermal conductivity data  ---
!
      ELSEIF( ITHK(IZN).EQ.6 ) THEN
        THKE_FW = 1.90551 - 1.57308D+0*EXP(-9.44058D+0*PORDX*SLX)
     &    + SFWX*PORDX*THKFWX
!
!---  Cass  ---
!
      ELSEIF( ITHK(IZN).EQ.7 ) THEN
        THKE_FW = THKS(1,IZN)+THKS(2,IZN)*SLX-(THKS(1,IZN)-THKS(4,IZN))*
     &    EXP(-((THKS(3,IZN)*SLX)**THKS(5,IZN)))
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of THKE_FW group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      FUNCTION THKE_L( IZN,SLX,THKLX,PORDX,PORTX,INDX )
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
!     Calculates equivalent thermal conductivity.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, January, 1992.
!     Last Modified by WE Nichols, Battelle, PNL, February 13, 1995.
!     Last Modified by MD White, PNNL, 21 May, 2002.
!     $Id: thke.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
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
      SUB_LOG(ISUB_LOG) = '/THKE_L'
      IF( INDEX(SVN_ID(234)(1:1),'$').EQ.0 ) SVN_ID(234) =
     & '$Id: thke.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Constant thermal conductivity  ---
!
      IF( ITHK(IZN).EQ.1 ) THEN
        THKE_L = THKS(INDX,IZN)
!
!---  Parallel thermal conductivity model  ---
!
      ELSEIF( ITHK(IZN).EQ.2 ) THEN
        THKE_L = (1.D+0-PORTX)*THKS(INDX,IZN) +
     &    (PORTX-PORDX)*THKLX +
     &    SLX*PORDX*THKLX
!
!---  Linear thermal conductivity model  ---
!
      ELSEIF( ITHK(IZN).EQ.3 ) THEN
        THKE_L = THKS(INDX,IZN) + SLX*(THKS(INDX+3,IZN)-THKS(1,IZN))
!
!---  Somerton thermal conductivity model  ---
!
      ELSEIF( ITHK(IZN).EQ.4 ) THEN
        THKSX = THKS(INDX+3,IZN)
        THKUX = THKS(INDX,IZN)
        THKE_L = THKUX + SQRT(SLX)*(THKSX-THKUX)
!
!---  Campbell thermal conductivity model (inappropriate for ice)  ---
!
      ELSEIF( ITHK(IZN).EQ.5 ) THEN
        WMCX = (PORTX-PORDX)+PORDX*SLX
        THKE_L = THKS(1,IZN)+THKS(2,IZN)*WMCX-(THKS(1,IZN)-THKS(4,IZN))*
     &    EXP(-((THKS(3,IZN)*WMCX)**THKS(5,IZN)))
!
!---  Jame and Norium thermal conductivity data  ---
!
      ELSEIF( ITHK(IZN).EQ.6 ) THEN
        THKE_L = 1.90551 - 1.57308D+0*EXP(-9.44058D+0*PORDX*SLX)
!
!---  Cass  ---
!
      ELSEIF( ITHK(IZN).EQ.7 ) THEN
        THKE_L = THKS(1,IZN)+THKS(2,IZN)*SLX-(THKS(1,IZN)-THKS(4,IZN))*
     &    EXP(-((THKS(3,IZN)*SLX)**THKS(5,IZN)))
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of THKE_L group  ---
!
      RETURN
      END

