!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE TMSTEP
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
!     Compute the time step based on the previous time step,
!     the acceleration factor, the maximum time step, the time until
!     the next print, and the time until the next start of a
!     boundary condition.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, February, 1993.
!     Last Modified by MD White, Battelle, PNL, April 14, 1994.
!     Last Modified by MD White, PNNL, 13 August 2003.




!     $Id: tmstep.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE WELL_CL
      USE UCODE
      USE SOURC
      USE SOLTN

      USE PLT_ATM
      USE OUTPU
      USE COUP_WELL
      USE CONST
      USE BCV
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
      SUB_LOG(ISUB_LOG) = '/TMSTEP'
      IF( INDEX(SVN_ID(243)(1:1),'$').EQ.0 ) SVN_ID(243) =
     & '$Id: tmstep.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Loop over execution periods to determine the current execution
!     period  ---
!
      NP = 0
      TMPSX = -1.D+20
      TMTOL = 1.D-9
      TMZ = TM
      IF( MEPD.EQ.1 ) TMZ = MOD(TM,TMPE(NEPD))
      DO 10 N = 1,NEPD
        IF( (TMZ-TMPS(N))/(ABS(TMZ)+EPSL).GT.-EPSL .AND.
     &    (TMPE(N)-TMZ)/(ABS(TMZ)+EPSL).GT.-EPSL ) THEN
          IF( TMPS(N).GT.TMPSX ) THEN
            TMPSX = TMPS(N)
            NP = N
          ENDIF
        ENDIF
   10 CONTINUE
      IF( NP.NE.0 ) IEPD = NP
      DTAF = TMPA(IEPD)
      DTCF = TMPC(IEPD)
      DTMX = TMPX(IEPD)
      DTMN = TMPN(IEPD)
      NRIMX = NRIM(IEPD)
      RSDMX = RSDM(IEPD)
!
!---  Assign the initial time step  ---
!
      IF( ABS((TMZ-TMPS(IEPD))/(ABS(TMZ)+EPSL)).LE.EPSL ) THEN
        IF( TMPD(IEPD).GT.EPSL ) THEN
          DT = TMPD(IEPD)/DTAF
          DTO = DT
        ENDIF
      ENDIF
      DTNX = MAX( DTO,DT*DTAF )
      DTQU = TMMX - TM
!
!---  Loop over print times
!     compute time step to next print time  ---
!
      DTPR = BIG
      DO 110 N = 1,NPRTM
        IF( PRTM(N).GT.TM ) DTPR = MIN( DTPR,PRTM(N)-TM )
  110 CONTINUE
      TMPR = TM + DTPR
!
!---  Loop over boundary conditions
!     compute time step to next boundary condition transition  ---
!
      DTBC = BIG
      DO 130 N = 1,NBC
        TMZ = TM
        MB = IBCIN(N)
        IF( IBCC(N).EQ.1 ) TMZ = MOD(TM,BC(1,IBCM(N),MB))
        DO 120 M = 1,IBCM(N)
          DTBCX = DTBC
          IF( BC(1,M,MB)-TMZ.GT.TMTOL ) 
     &      DTBC = MIN( DTBC,BC(1,M,MB)-TMZ )
          IF( (TM+DTBC).EQ.TM ) DTBC = DTBCX
  120   CONTINUE
  130 CONTINUE
!
!---  Loop over atmospheric conditions times
!     compute time step to next boundary condition transition  ---
!
      DTATM = BIG
!
!---  Cyclic atmospheric conditions  ---
!
      IF( IATM_C.EQ.1 ) THEN
        TMZ = MOD( TM,ATMOS(NATM_T,1) )
      ELSE
        TMZ = TM
      ENDIF
!
!---  Single atmospheric conditions time  ---
!
      IF( NATM_T.EQ.1 ) THEN
        IF( TMZ.LT.ATMOS(1,1) ) DTATM = ATMOS(1,1)-TMZ
!
!---  Multiple atmospheric conditions times  ---
!
      ELSEIF( NATM_T.GT.1 ) THEN
        CALL LOCATE( ATMOS(1,1),NATM_T,TMZ,NT )
!
!---    Simulation time prior to first atmospheric conditions time  ---
!
        IF( NT.EQ.0 ) THEN
          DTATM = ATMOS(1,1)-TMZ
!
!---    Simulation time within atmospheric conditions time limits  ---
!
        ELSEIF( NT.LT.NATM_T ) THEN
          DTATM = ATMOS(NT+1,1)-TMZ
        ENDIF
      ENDIF
!
!---  Loop over sources,
!     compute time step to next source transition  ---
!
      DTSR = BIG
      DO 150 N = 1,NSR
        DO 140 M = 1,ISRM(N)
          IF( SRC(1,M,N).GT.TM ) DTSR = MIN( DTSR,SRC(1,M,N)-TM )
  140   CONTINUE
  150 CONTINUE
!
!---  Loop over observed data,
!     compute time step to next observed data time  ---
!
      DTOB = BIG
      IF( ISLC(20).EQ.1 ) THEN
        DO 160 NT = 1,NOBDT
        DO 160 NS = 1,NOBDS(NT)
          IF( R_OBDS(2,NS,NT).GT.TM )
     &      DTOB = MIN( DTOB,R_OBDS(2,NS,NT)-TM )
  160   CONTINUE
        TMOB = TM + DTOB
      ENDIF
!
!---  Loop over execution periods,
!     compute time step to next execution period start  ---
!
      DTEP = BIG
      TMZ = TM
      IF( MEPD.EQ.1 ) TMZ = MOD(TM,TMPS(NEPD))
      DO 170 N = 1,NEPD
        IF( TMPS(N).GT.TMZ ) DTEP = MIN( DTEP,TMPS(N)-TMZ )
  170 CONTINUE
!
!---  Loop over well times
!     compute time step to next well transition  ---
!
      DTWL = BIG
      IF( LWELL.EQ.1 ) THEN
        DO 190 NWL = 1,NWLS
          TMZ = TM
          IF( IWCC(NWL).EQ.1 ) TMZ = MOD(TM,WLVR(1,IWM(NWL),NWL))
          DO 180 M = 1,IWM(NWL)
            DTWLX = DTWL
            IF( WLVR(1,M,NWL).GT.TMZ )
     &        DTWL = MIN( DTWL,WLVR(1,M,NWL)-TMZ )
            IF( (TM+DTWL).EQ.TM ) DTWL = DTWLX
  180     CONTINUE
  190   CONTINUE
      ENDIF

!
!---  Loop over coupled-well or ground-loop-well times
!     compute time step to next well transition  ---
!
      DT_CW = BIG
      NCWX = MAX( N_CW,N_GLW )
      DO 260 NCW = 1,NCWX
        TMZ = TM
!
!---    Cyclic time periods  ---
!
        IF( ICC_CW(NCW).EQ.1 ) THEN
!
!---      For multiple times in coupled-well period, loop over the 
!         injection well time periods, to find the final well time  ---
!
          NTX = 0
          DO 230 NTP = 1,IM_CW(NCW)
            NTX = NTX + MAX( 1,IMP_CW(NTP,NCW) )
  230     CONTINUE
!
!---      Determine time with the cyclic time period  ---
!
          TMZ = MOD( TM,VAR_CW(1,NTX,NCW) )
        ENDIF
!
!---    Loop over coupled-well periods  ---
!
        NS = 1
        DO 250 M = 1,IM_CW(NCW)
!
!---      For multiple times in coupled-well period, loop over coupled-
!         well period times  ---
!
          IF( IMP_CW(M,NCW).GT.0 ) THEN
            DO 240 N = 1,IMP_CW(M,NCW)
              MX = NS + N - 1
              DT_CWX = DT_CW
              IF( VAR_CW(1,MX,NCW)-TMZ.GT.TMTOL ) 
     &          DT_CW = MIN( DT_CW,VAR_CW(1,MX,NCW)-TMZ )
              IF( (TM+DT_CW).EQ.TM ) DT_CW = DT_CWX
  240       CONTINUE
            NS = NS + IMP_CW(M,NCW)
!
!---      Single-time coupled-well period  ---
!
          ELSE
            DT_CWX = DT_CW
            IF( VAR_CW(1,M,NCW)-TMZ.GT.TMTOL ) 
     &        DT_CW = MIN( DT_CW,VAR_CW(1,M,NCW)-TMZ )
            IF( (TM+DT_CW).EQ.TM ) DT_CW = DT_CWX
          ENDIF
  250   CONTINUE
  260 CONTINUE
!
!---  Compute the next step based on the minimum of
!     the incremented time step, the time step to quit,
!     the time step to print, the time step to a boundary condition
!     transition, the time step to a source transitions,
!     the time step to an execution period start,
!     the time step to a well time,





!     or the maximum time step  ---
!




      DT = MIN( DTNX,DTQU,DTPR,DTBC,DTATM,DTSR,DTOB,DTEP,DTMX,DT_CW )

      IF( LWELL.EQ.1 ) DT = MIN( DT,DTWL )
      DTI = 1.D+0/(DT+SMALL)
      IF( ABS(DT-DTNX)/(ABS(DT)+EPSL).LE.EPSL .OR.
     &  ABS(DT-DTMX)/(ABS(DT)+EPSL).LE.EPSL ) DTO = DT
      TM = TM + DT
      ISUB_LOG = ISUB_LOG-1
!
!---  End of TMSTEP group
!
      RETURN
      END

