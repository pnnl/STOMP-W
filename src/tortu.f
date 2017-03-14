!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE TORTU( IZN,SLX,SGX,SNX,PORDX,TORLX,TORGX,TORNX )
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
!     Compute phase tortuosity.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, December, 1992.
!     Last Modified by MD White, Battelle, PNL, December 31, 1992.
!     $Id: tortu.F 1080 2017-03-14 16:22:02Z d3c002 $
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
      SUB_LOG(ISUB_LOG) = '/TORTU'
      IF( INDEX(SVN_ID(244)(1:1),'$').EQ.0 ) SVN_ID(244) =
     & '$Id: tortu.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Constant phase tortuosity  ---
!
      IF( ITOR(IZN).EQ.1 ) THEN
        TORLX = TOR(1,IZN)
        TORGX = TOR(2,IZN)
        TORNX = TOR(3,IZN)
!
!---  Millington and Quirk tortuosity model  ---
!
      ELSEIF( ITOR(IZN).EQ.2 ) THEN
        IF( SLX*PORDX.LT.EPSL ) THEN
          TORLX = 0.D+0
        ELSE
          TORLX = (PORDX*(SLX**7))**(THIRD)
        ENDIF
        IF( SGX*PORDX.LT.EPSL ) THEN
          TORGX = 0.D+0
        ELSE
          TORGX = (PORDX*(SGX**7))**(THIRD)
        ENDIF
        IF( SNX*PORDX.LT.EPSL ) THEN
          TORNX = 0.D+0
        ELSE
          TORNX = (PORDX*(SNX**7))**(THIRD)
        ENDIF
!
!---  Millington and Quirk (free gas) tortuosity model  ---
!
      ELSEIF( ITOR(IZN).EQ.3 ) THEN
        IF( SLX*PORDX.LT.EPSL ) THEN
          TORLX = 0.D+0
        ELSE
          TORLX = (PORDX*(SLX**7))**(THIRD)
        ENDIF
        SGFX = MAX( SGX-SNX,0.D+0 )
        IF( SGFX*PORDX.LT.EPSL ) THEN
          TORGX = 0.D+0
        ELSE
          TORGX = (PORDX*(SGFX**7))**(THIRD)
        ENDIF
        IF( SNX*PORDX.LT.EPSL ) THEN
          TORNX = 0.D+0
        ELSE
          TORNX = (PORDX*(SNX**7))**(THIRD)
        ENDIF
!
!---  Marshal tortuosity model  ---
!
      ELSEIF( ITOR(IZN).EQ.4 ) THEN
        IF( SLX*PORDX.LT.EPSL ) THEN
          TORLX = 0.D+0
        ELSE
          TORLX = SQRT(PORDX*SLX)
        ENDIF
        IF( SGX*PORDX.LT.EPSL ) THEN
          TORGX = 0.D+0
        ELSE
          TORGX = SQRT(PORDX*SGX)
        ENDIF
        IF( SNX*PORDX.LT.EPSL ) THEN
          TORNX = 0.D+0
        ELSE
          TORNX = SQRT(PORDX*SNX)
        ENDIF
!
!---  Constant aqueous/Millington gas tortuosity model  ---
!
      ELSEIF( ITOR(IZN).EQ.5 ) THEN
        TORLX = TOR(1,IZN)
        IF( SGX*PORDX.LT.EPSL ) THEN
          TORGX = 0.D+0
        ELSE
          TORGX = (PORDX*(SGX**7))**(THIRD)
        ENDIF
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of TORTU group  ---
!
      RETURN
      END

