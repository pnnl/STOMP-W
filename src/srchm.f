!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SRCHM
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
!     Solute reactive chemistry (first-order reaction kinetics).
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle's Pacific Northwest Division, 1996.
!     Last Modified by MD White on October 11, 1996.
!     $Id: srchm.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE GRID
      USE FDVP
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
!      REAL*8 RFX(LSOLU,LCHEM),RRX(LSOLU)
      REAL*8 RFX(LSOLU),RFNX(LSOLU),RFPX(LSOLU)
      REAL*8 RKX(LSOLU,LCHEM),RKEX(LSOLU,LCHEM)
      REAL*8 CX(LSOLU)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/SRCHM'
      IF( INDEX(SVN_ID(200)(1:1),'$').EQ.0 ) SVN_ID(200) =
     & '$Id: srchm.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Loop over all active nodes  ---
!
      DO 400 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 400
!
!---    Initialize reaction terms  ---
!
        DO 10 NSL = 1,NSOLU
          RFX(NSL) = 0.D+0
          RFNX(NSL) = 0.D+0
          RFPX(NSL) = 0.D+0
          CX(NSL) = MAX( (C(N,NSL)-SOLML(NSL)*SL(2,N)*PORD(2,N)/
     &      YL(N,NSL)),0.D+0 )
  10    CONTINUE
!
!---    Loop over parent reactants  ---
!
        DO 50 NPSL = 1,NSOLU
!
!---      Convert reaction half life into reaction constant,
!         ignoring concentration limits and assign effective
!         reaction constants to their maximum values  ---
!
          DO 20 NCHM = 1,NCHEM(NPSL)
            RKX(NPSL,NCHM) = 6.93147D-1/MAX(RHLF(NPSL,NCHM),EPSL)
            RKEX(NPSL,NCHM) = RKX(NPSL,NCHM)
   20     CONTINUE
!
!---      Compute reaction terms for each reactant  ---
!
          DO 40 NCHM = 1,NCHEM(NPSL)
            DO 30 NDSL = 1,NSOLU
              RFX(NDSL) = RFX(NDSL) + RCHDF(NPSL,NDSL,NCHM)*CX(NPSL)*
     &          (1.D+0-EXP(-RKX(NPSL,NCHM)*DT))
!
!---          Compute positive reaction terms for each reactant  ---
!
              IF( RCHDF(NPSL,NDSL,NCHM).GE.0.D+0 ) THEN
                RFPX(NDSL) = RFPX(NDSL) + RCHDF(NPSL,NDSL,NCHM)*
     &            CX(NPSL)*(1.D+0-EXP(-RKX(NPSL,NCHM)*DT))
!
!---          Compute negative reaction terms for each reactant  ---
!
              ELSE
                RFNX(NDSL) = RFNX(NDSL) + RCHDF(NPSL,NDSL,NCHM)*
     &            CX(NPSL)*(1.D+0-EXP(-RKX(NPSL,NCHM)*DT))
              ENDIF
   30       CONTINUE
   40     CONTINUE
   50   CONTINUE
!
!---    Check zero concentration limits for each reactant  ---
!
        DO 80 NSL = 1,NSOLU
!
!---      Reactant concentration will fall below zero,
!         limit reaction constants that reduce the
!         reactant concentration  ---
!
          IF( CX(NSL)+RFX(NSL).LT.0.D+0 ) THEN
            DO 70 NPSL = 1,NSOLU
              DO 60 NCHM = 1,NCHEM(NPSL)
                IF( RCHDF(NPSL,NSL,NCHM).LT.0.D+0 ) THEN
                  RKEX(NPSL,NCHM) = -LOG(1.D+0-((1.D+0-
     &              EXP(-RKX(NPSL,NCHM)*DT))*(CX(NSL)+RFPX(NSL))
     &              /RFNX(NSL)))/DT
                ENDIF
   60         CONTINUE
   70       CONTINUE
          ENDIF
   80   CONTINUE
!
!---    Initialize reaction terms  ---
!
        DO 90 NSL = 1,NSOLU
          RFX(NSL) = 0.D+0
          RFNX(NSL) = 0.D+0
          RFPX(NSL) = 0.D+0
  90    CONTINUE
!
!---    Loop over parent reactants and recompute reaction
!       terms using effective reaction constants  ---
!
        DO 130 NPSL = 1,NSOLU
!
!---      Reset reaction constants to the effective reaction
!         constants computed from zero concentration limits  ---
!
          DO 100 NCHM = 1,NCHEM(NPSL)
            RKX(NPSL,NCHM) = RKEX(NPSL,NCHM)
  100     CONTINUE
!
!---      Compute reaction terms for each reactant  ---
!
          DO 120 NCHM = 1,NCHEM(NPSL)
            DO 110 NDSL = 1,NSOLU
              RFX(NDSL) = RFX(NDSL) + RCHDF(NPSL,NDSL,NCHM)*CX(NPSL)*
     &          (1.D+0-EXP(-RKX(NPSL,NCHM)*DT))
!
!---          Compute positive reaction terms for each reactant  ---
!
              IF( RCHDF(NPSL,NDSL,NCHM).GE.0.D+0 ) THEN
                RFPX(NDSL) = RFPX(NDSL) + RCHDF(NPSL,NDSL,NCHM)*
     &            CX(NPSL)*(1.D+0-EXP(-RKX(NPSL,NCHM)*DT))
!
!---          Compute negative reaction terms for each reactant  ---
!
              ELSE
                RFNX(NDSL) = RFNX(NDSL) + RCHDF(NPSL,NDSL,NCHM)*
     &            CX(NPSL)*(1.D+0-EXP(-RKX(NPSL,NCHM)*DT))
              ENDIF
  110       CONTINUE
  120     CONTINUE
  130   CONTINUE
!
!---    Check solubility concentration limits for each reactant  ---
!
        DO 160 NSL = 1,NSOLU
!
!---      Reactant concentration will increase above solubility limit,
!         limit reaction constants that increase the
!         reactant concentration  ---
!
          IF( SOLML(NSL)/EPSL.GT.EPSL ) THEN
            CMX = SOLML(NSL)*SL(2,N)*PORD(2,N)/YL(N,NSL)
            IF( C(N,NSL)+RFX(NSL).GT.CMX ) THEN
              DO 150 NPSL = 1,NSOLU
                DO 140 NCHM = 1,NCHEM(NPSL)
                  IF( RCHDF(NPSL,NSL,NCHM).GT.0.D+0 ) THEN
                    RKEX(NPSL,NCHM) = -LOG(1.D+0-((1.D+0-
     &                EXP(-RKX(NPSL,NCHM)*DT))*(CMX-C(N,NSL)-RFNX(NSL))
     &                /RFPX(NSL)))/DT
                  ENDIF
  140           CONTINUE
  150         CONTINUE
            ENDIF
          ENDIF
  160   CONTINUE
!
!---    Initialize reaction terms  ---
!
        DO 170 NSL = 1,NSOLU
          RFX(NSL) = 0.D+0
          RFNX(NSL) = 0.D+0
          RFPX(NSL) = 0.D+0
 170    CONTINUE
!
!---    Loop over parent reactants and recompute reaction
!       terms using effective reaction constants  ---
!
        DO 200 NPSL = 1,NSOLU
!
!---      Compute reaction terms for each reactant  ---
!
          DO 190 NCHM = 1,NCHEM(NPSL)
            DO 180 NDSL = 1,NSOLU
              RFX(NDSL) = RFX(NDSL) + RCHDF(NPSL,NDSL,NCHM)*CX(NPSL)*
     &          (1.D+0-EXP(-RKEX(NPSL,NCHM)*DT))
!
!---          Compute positive reaction terms for each reactant  ---
!
              IF( RCHDF(NPSL,NDSL,NCHM).GE.0.D+0 ) THEN
                RFPX(NDSL) = RFPX(NDSL) + RCHDF(NPSL,NDSL,NCHM)*
     &            CX(NPSL)*(1.D+0-EXP(-RKEX(NPSL,NCHM)*DT))
!
!---          Compute negative reaction terms for each reactant  ---
!
              ELSE
                RFNX(NDSL) = RFNX(NDSL) + RCHDF(NPSL,NDSL,NCHM)*
     &            CX(NPSL)*(1.D+0-EXP(-RKEX(NPSL,NCHM)*DT))
              ENDIF
  180       CONTINUE
  190     CONTINUE
  200   CONTINUE
!
!---    Update solute concentrations using effective reaction
!       constants
!
        DO 210 NSL = 1,NSOLU
          C(N,NSL) = C(N,NSL) + RFX(NSL)
  210   CONTINUE
!
!---  Compute reaction factors for each reaction  ---
!
!        DO 100 NPSL = 1,NSOLU
!          DO 90 NCHM = 1,NCHEM(NPSL)
!            RFX(NPSL,NCHM) = 0.D+0
!            DO 80 NDSL = 1,NSOLU
!              IF( RCHDF(NPSL,NDSL,NCHM).LT.ZERO ) THEN
!                IF( C(N,NDSL)/EPSL.LT.EPSL ) THEN
!                  RFX(NPSL,NCHM) = 1.D+20
!                ELSE
!                  RFX(NPSL,NCHM) = MAX( RFX(NPSL,NCHM),
!     &              -6.93147D-1*C(N,NPSL)*RCHDF(NPSL,NDSL,NCHM)*DT/
!     &              (MAX(C(N,NDSL)*RHLF(NPSL,NCHM),SMALL)) )
!                ENDIF
!              ENDIF
!   80       CONTINUE
!            CN = 2.D+0
!            CM = 1.D+0 - 1.D+0/CN
!            RFX(NPSL,NCHM) = (1.D+0 + (RFX(NPSL,NCHM)**CN))**(-CM)
!   90     CONTINUE
!  100   CONTINUE
!
!---  Compute reaction rates  ---
!
!        DO 200 NSL = 1,NSOLU
!          RRX(NSL) = 0.D+0
!          DO 190 NPSL = 1,NSOLU
!            DO 180 NCHM = 1,NCHEM(NPSL)
!              RRX(NSL) = RRX(NSL) + DT*RCHDF(NPSL,NSL,NCHM)
!     &          *RFX(NPSL,NCHM)*6.93147D-1*C(N,NPSL)/RHLF(NPSL,NCHM)
!  180        CONTINUE
!  190     CONTINUE
!  200   CONTINUE
!
!---  Update solute concentrations
!
!        DO 300 NSL = 1,NSOLU
!          C(N,NSL) = MAX( C(N,NSL)+RRX(NSL),ZERO )
!  300   CONTINUE
  400 CONTINUE
!
!---  End of SRCHM group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END


