!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE PORSTY( N,PX,PREFX,PORDX,PORTX )
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
!     Compute diffusive and total porosities.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, December, 1992.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
      USE GRID
      USE FDVP
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
      SUB_LOG(ISUB_LOG) = '/PORSTY'
      IF( INDEX(SVN_ID(134)(1:1),'$').EQ.0 ) SVN_ID(134) =
     & '$Id: porsty.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      DPX = PX-PREFX
      IZN = IZ(N)
!
!---  Pore compressibility w/ fixed bulk volume  ---
!
      IF( ISLC(15).EQ.1 ) THEN
!
!---    Fracture properties (dual porosity model)  ---
!
        IF( ABS(IDP(IZN)).EQ.1 ) THEN
          PORTX = POR(3,IZN)*EXP(DPX*CMP(2,IZN))
          PORDX = POR(4,IZN)*EXP(DPX*CMP(2,IZN))
!
!---    Matrix properties (dual porosity model)  ---
!
        ELSEIF( ABS(IDP(IZN)).EQ.2 ) THEN
          PORTX = POR(1,IZN)*EXP(DPX*CMP(1,IZN))
          PORDX = POR(2,IZN)*EXP(DPX*CMP(1,IZN))
        ELSE
!
!---      Reactive transport porosity alteration  ---
!
          IF( ISLC(43).EQ.1 ) THEN
            PORTX = POR0(1,N)*EXP(DPX*CMP(1,IZN))
            PORDX = POR0(2,N)*EXP(DPX*CMP(1,IZN))
          ELSE
            PORTX = POR(1,IZN)*EXP(DPX*CMP(1,IZN))
            PORDX = POR(2,IZN)*EXP(DPX*CMP(1,IZN))
          ENDIF
        ENDIF
!
!---  Bulk compressibility w/ variable bulk volume  ---
!
      ELSEIF( ISLC(15).EQ.10 ) THEN
!
!---      Reactive transport porosity alteration  ---
!
          IF( ISLC(43).EQ.1 ) THEN
            PORTX = POR0(1,N)*EXP(DPX*CMP(1,IZN)*(1.D+0-POR0(1,N)/
     &        POR0(1,N)))
            PORDX = POR0(2,N)*EXP(DPX*CMP(1,IZN)*(1.D+0-POR0(2,N)/
     &        POR0(2,N)))
          ELSE
            PORTX = POR(1,IZN)*EXP(DPX*CMP(1,IZN)*(1.D+0-POR(1,IZN)/
     &        POR(1,IZN)))
            PORDX = POR(2,IZN)*EXP(DPX*CMP(1,IZN)*(1.D+0-POR(2,IZN)/
     &        POR(2,IZN)))
          ENDIF
!
!---  Pore compressibility w/ variable bulk volume  ---
!
      ELSEIF( ISLC(15).EQ.11 ) THEN
!
!---    Reactive transport porosity alteration  ---
!
        IF( ISLC(43).EQ.1 ) THEN
          PORTX = POR0(1,N)*EXP(DPX*CMP(1,IZN)*(1.D+0-POR0(1,N)))
          PORDX = POR0(2,N)*EXP(DPX*CMP(1,IZN)*(1.D+0-POR0(2,N)))
        ELSE
          PORTX = POR(1,IZN)*EXP(DPX*CMP(1,IZN)*(1.D+0-POR(1,IZN)))
          PORDX = POR(2,IZN)*EXP(DPX*CMP(1,IZN)*(1.D+0-POR(2,IZN)))
        ENDIF
!
!---  Bulk compressibility w/ fixed bulk volume  ---
!
      ELSE
!
!---    Fracture properties (dual porosity model)  ---
!
        IF( ABS(IDP(IZN)).EQ.1 ) THEN
          PORTX = POR(3,IZN) + DPX*CMP(2,IZN)
          PORDX = POR(4,IZN) + DPX*CMP(2,IZN)
!
!---    Matrix properties (dual porosity model)  ---
!
        ELSEIF( ABS(IDP(IZN)).EQ.2 ) THEN
          PORTX = POR(1,IZN) + DPX*CMP(1,IZN)
          PORDX = POR(2,IZN) + DPX*CMP(1,IZN)
        ELSE
!
!---      Reactive transport porosity alteration  ---
!
          IF( ISLC(43).EQ.1 ) THEN
            PORTX = POR0(1,N) + DPX*CMP(1,IZN)
            PORDX = POR0(2,N) + DPX*CMP(1,IZN)
          ELSE
            PORTX = POR(1,IZN) + DPX*CMP(1,IZN)
            PORDX = POR(2,IZN) + DPX*CMP(1,IZN)
          ENDIF
        ENDIF
      ENDIF
      PORTX = MAX( MIN( PORTX,1.D+0 ),1.D-6 )
      PORDX = MAX( MIN( PORDX,1.D+0 ),1.D-6 )
      ISUB_LOG = ISUB_LOG-1
!
!---  End of PORSTY group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE PORSTY_M( N,PX,PREFX,PORDX,PORTX )
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
!     Compute matrix diffusive and total porosities.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 19 December 2015.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
      USE GRID
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
      SUB_LOG(ISUB_LOG) = '/PORSTY_M'
      IF( INDEX(SVN_ID(134)(1:1),'$').EQ.0 ) SVN_ID(134) =
     & '$Id: porsty.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      DPX = PX-PREFX
      IZN = IZ(N)
!
!---  Pore compressibility w/ fixed bulk volume  ---
!
      IF( ISLC(15).EQ.1 ) THEN
        PORTX = POR(3,IZN)*EXP(DPX*CMP(1,IZN))
        PORDX = POR(4,IZN)*EXP(DPX*CMP(1,IZN))
!
!---  Bulk compressibility w/ variable bulk volume  ---
!
      ELSEIF( ISLC(15).EQ.10 ) THEN
        PORTX = POR(3,IZN)*EXP(DPX*CMP(1,IZN)*(1.D+0-POR(3,IZN)/
     &    POR(3,IZN)))
        PORDX = POR(4,IZN)*EXP(DPX*CMP(1,IZN)*(1.D+0-POR(4,IZN)/
     &    POR(4,IZN)))
!
!---  Pore compressibility w/ variable bulk volume  ---
!
      ELSEIF( ISLC(15).EQ.11 ) THEN
        PORTX = POR(3,IZN)*EXP(DPX*CMP(1,IZN)*(1.D+0-POR(3,IZN)))
        PORDX = POR(4,IZN)*EXP(DPX*CMP(1,IZN)*(1.D+0-POR(4,IZN)))
!
!---  Bulk compressibility w/ fixed bulk volume  ---
!
      ELSE
        PORTX = POR(3,IZN) + DPX*CMP(1,IZN)
        PORDX = POR(4,IZN) + DPX*CMP(1,IZN)
      ENDIF
      PORTX = MAX( MIN( PORTX,1.D+0 ),1.D-6 )
      PORDX = MAX( MIN( PORDX,1.D+0 ),1.D-6 )
      ISUB_LOG = ISUB_LOG-1
!
!---  End of PORSTY_M group  ---
!
      RETURN
      END


