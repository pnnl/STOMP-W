!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE DRCVLB( N,NB )
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
!     Compute the aqueous-phase Darcy flux from pressure gradients
!     and gravitational body forces on a bottom boundary.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1993.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
      USE POINTE
      USE JACOB
      USE GRID
      USE FLUXP
      USE FDVS
      USE FDVP
      USE CONST
      USE BCVS
      USE BCVP
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
      SUB_LOG(ISUB_LOG) = '/DRCVLB'
      IF( INDEX(SVN_ID(52)(1:1),'$').EQ.0 ) SVN_ID(52) =
     & '$Id: drcvlb.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      I = ID(N)
      J = JD(N)
      K = KD(N)
      NPZ = NSZ(N)
      OECZ = 0.D+0
      DO 100 M = 1,ISVF
        MP = MPOS(M)
        INDX = 11
        IF( ISLC(7).EQ.1 .OR. ISLC(7).EQ.2 )
     &    OECZ = DIFMN(OECB(MP,NB),OEC(MP,N),
     &    DZGF(N),DZGF(N),0.D+0,INDX)
        DPOS = OECZ*(POSB(MP,NB)-POSM(MP,N))
        HZ = PLB(MP,NB)-PL(MP,N)+DPOS
     &    -5.D-1*GRVZ(NPZ)*DZGF(N)*RHOLB(MP,NB)
        IF( M.EQ.1 ) HD = HZ
        INDX = 8
        RKM = DIFMN(RKLB(3,MP,NB),RKL(3,MP,N),DZGF(N),DZGF(N),HD,INDX)
        INDX = 5
        VM = DIFMN(VISLB(MP,NB),VISL(MP,N),DZGF(N),DZGF(N),HD,INDX)
        PERM_PX = PERMRF(MP,N)*PERM(3,IZ(N))
        WL(M,NSZ(N)) = 2.D+0*PERM_PX*RKM*HZ/(VM*DZGF(N))
!
!---    Outflow, diode or potential-evaporation
!       boundary condition  ---
!
        IF( ABS(IBCT(IEQW,NB)).EQ.7 .OR.
     &    ABS(IBCT(IEQW,NB)).EQ.23 ) THEN
          WL(M,NSZ(N)) = MIN( 0.D+0,WL(M,NSZ(N)) )
!
!---    Seepage boundary condition  ---
!
        ELSEIF( ABS(IBCT(IEQW,NB)).EQ.17 ) THEN 
          PLBX = PL(MP,N) + RHOL(MP,N)*5.D-1*GRVZ(NPZ)*DZGF(N)
          IF( PLBX.GT.PLB(MP,NB) ) THEN
            WL(M,NSZ(N)) = MIN( 0.D+0,WL(M,NSZ(N)) )
          ELSE
            WL(M,NSZ(N)) = 0.D+0
          ENDIF
!
!---    x-y-z seepage boundary condition  ---
!
        ELSEIF( ABS(IBCT(IEQW,NB)).EQ.45 ) THEN 
           IF( PG(MP,N)-PL(MP,N).GT.EPSL ) THEN
!           IF( ABS(1.D+0-SL(MP,N)).GT.EPSL ) THEN
             IF( PLB(MP,NB)-PGB(MP,NB).GT.EPSL ) THEN
               WL(M,NPZ) = MAX( 0.D+0,WL(M,NPZ) )
             ELSE
               WL(M,NPZ) = MIN( 0.D+0,WL(M,NPZ)*(RKM**4) )
             ENDIF
           ELSE
             IF( PLB(MP,NB)-PGB(MP,NB).LT.EPSL ) THEN
               WL(M,NPZ) = MIN( 0.D+0,WL(M,NPZ) )
             ENDIF
           ENDIF
!
!---    Shuttleworth-Wallace boundary condition  ---
!
        ELSEIF( IBCT(IEQW,NB).EQ.22 ) THEN
          IF( PL(MP,N)-PG(2,N).GE.EPSL ) THEN
            WL(M,NSZ(N)) = MIN( 0.D+0,WL(M,NSZ(N)) )
          ELSE
            WL(M,NSZ(N)) = 0.D+0
          ENDIF
!
!---    Dirichlet-Outflow boundary condition  ---
!
        ELSEIF( IBCT(IEQW,NB).EQ.26 ) THEN
          IF( WL(1,NPZ).LT.-EPSL ) THEN
            WL(M,NPZ) = WL(M,NPZ)
          ELSE
            WL(M,NPZ) = 0.D+0
          ENDIF
        ENDIF
  100 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of DRCVLB group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE DRCVLS( N,NB )
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
!     Compute the aqueous-phase Darcy flux from pressure gradients
!     and gravitational body forces on a south boundary.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1993.
!     Last Modified by MD White, PNNL, December 13, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
      USE POINTE
      USE JACOB
      USE GRID
      USE FLUXP
      USE FDVS
      USE FDVP
      USE CONST
      USE BCVS
      USE BCVP
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
      SUB_LOG(ISUB_LOG) = '/DRCVLS'
      IF( INDEX(SVN_ID(52)(1:1),'$').EQ.0 ) SVN_ID(52) =
     & '$Id: drcvlb.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      I = ID(N)
      J = JD(N)
      K = KD(N)
      NPY = NSY(N)
      OECY = 0.D+0
      DO 100 M = 1,ISVF
        MP = MPOS(M)
        INDX = 11
        IF( ISLC(7).EQ.1 .OR. ISLC(7).EQ.2 )
     &    OECY = DIFMN(OECB(MP,NB),OEC(MP,N),
     &    DYGF(N),DYGF(N),0.D+0,INDX)
        DPOS = OECY*(POSB(MP,NB)-POSM(MP,N))
        HY = PLB(MP,NB)-PL(MP,N)+DPOS
     &    -5.D-1*GRVY(NPY)*DYGF(N)*RP(I)*RHOLB(MP,NB)
        IF( M.EQ.1 ) HD = HY
        INDX = 8
        RKM = DIFMN(RKLB(2,MP,NB),RKL(2,MP,N),DYGF(N),DYGF(N),HD,INDX)
        INDX = 5
        VM = DIFMN(VISLB(MP,NB),VISL(MP,N),DYGF(N),DYGF(N),HD,INDX)
        PERM_PX = PERMRF(MP,N)*PERM(2,IZ(N))
        VL(M,NPY) = 2.D+0*PERM_PX*RKM*HY/(VM*RP(I)*DYGF(N))
!
!---    Outflow, diode or potential-evaporation
!       boundary condition  ---
!
        IF( ABS(IBCT(IEQW,NB)).EQ.7 .OR.
     &    ABS(IBCT(IEQW,NB)).EQ.23 ) THEN
          VL(M,NPY) = MIN( 0.D+0,VL(M,NPY) )
!
!---    Seepage boundary condition  ---
!
        ELSEIF( ABS(IBCT(IEQW,NB)).EQ.17 ) THEN 
          PLBX = PL(MP,N) + RHOL(MP,N)*5.D-1*GRVY(NPY)*DYGF(N)*RP(I)
          IF( PLBX.GT.PLB(MP,NB) ) THEN
            VL(M,NPY) = MIN( 0.D+0,VL(M,NPY) )
          ELSE
            VL(M,NPY) = 0.D+0
          ENDIF
!
!---    x-y-z seepage boundary condition  ---
!
        ELSEIF( ABS(IBCT(IEQW,NB)).EQ.45) THEN 
           IF( PG(MP,N)-PL(MP,N).GT.EPSL ) THEN
!           IF( ABS(1.D+0-SL(MP,N)).GT.EPSL ) THEN
             IF( PLB(MP,NB)-PGB(MP,NB).GT.EPSL ) THEN
               VL(M,NPY) = MAX( 0.D+0,VL(M,NPY) )
             ELSE
               VL(M,NPY) = MIN( 0.D+0,VL(M,NPY)*(RKM**4) )
             ENDIF
           ELSE
             IF( PLB(MP,NB)-PGB(MP,NB).LT.EPSL ) THEN
               VL(M,NPY) = MIN( 0.D+0,VL(M,NPY) )
             ENDIF
           ENDIF
!
!---    Shuttleworth-Wallace boundary condition  ---
!
        ELSEIF( IBCT(IEQW,NB).EQ.22 ) THEN
          IF( PL(MP,N)-PG(2,N).GE.EPSL ) THEN
            VL(M,NPY) = MIN( 0.D+0,VL(M,NPY) )
          ELSE
            VL(M,NPY) = 0.D+0
          ENDIF
!
!---    Dirichlet-Outflow boundary condition  ---
!
        ELSEIF( IBCT(IEQW,NB).EQ.26 ) THEN
          IF( VL(1,NPY).LT.-EPSL ) THEN
            VL(M,NPY) = VL(M,NPY)
          ELSE
            VL(M,NPY) = 0.D+0
          ENDIF
        ENDIF
  100 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of DRCVLS group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE DRCVLW( N,NB )
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
!     Compute the aqueous-phase Darcy flux from pressure gradients
!     and gravitational body forces on a west boundary.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1993.
!     Last Modified by MD White, PNNL, December 13, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
      USE POINTE
      USE JACOB
      USE GRID
      USE FLUXP
      USE FDVS
      USE FDVP
      USE CONST
      USE BCVS
      USE BCVP
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
      SUB_LOG(ISUB_LOG) = '/DRCVLW'
      IF( INDEX(SVN_ID(52)(1:1),'$').EQ.0 ) SVN_ID(52) =
     & '$Id: drcvlb.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      I = ID(N)
      J = JD(N)
      K = KD(N)
      NPX = NSX(N)
      OECX = 0.D+0
      DO 100 M = 1,ISVF
        MP = MPOS(M)
        INDX = 11
        IF( ISLC(7).EQ.1 .OR. ISLC(7).EQ.2 )
     &    OECX = DIFMN(OECB(MP,NB),OEC(MP,N),
     &    DXGF(N),DXGF(N),0.D+0,INDX)
        DPOS = OECX*(POSB(MP,NB)-POSM(MP,N))
        HX = PLB(MP,NB)-PL(MP,N)+DPOS
     &    -5.D-1*GRVX(NPX)*DXGF(N)*RHOLB(MP,NB)
        IF( M.EQ.1 ) HD = HX
        INDX = 8
        RKM = DIFMN(RKLB(1,MP,NB),RKL(1,MP,N),DXGF(N),DXGF(N),HD,INDX)
        INDX = 5
        VM = DIFMN(VISLB(MP,NB),VISL(MP,N),DXGF(N),DXGF(N),HD,INDX)
        PERM_PX = PERMRF(MP,N)*PERM(1,IZ(N))
        UL(M,NPX) = 2.D+0*PERM_PX*RKM*HX/(VM*DXGF(N))
!
!---    Outflow, diode or potential-evaporation
!       boundary condition  ---
!
        IF( ABS(IBCT(IEQW,NB)).EQ.7 .OR.
     &    ABS(IBCT(IEQW,NB)).EQ.23 ) THEN
          UL(M,NPX) = MIN( 0.D+0,UL(M,NPX) )
!
!---    Seepage seepage boundary condition  ---
!
        ELSEIF( ABS(IBCT(IEQW,NB)).EQ.17 ) THEN 
          PLBX = PL(MP,N) + RHOL(MP,N)*5.D-1*GRVX(NPX)*DXGF(N)
          IF( PLBX.GT.PLB(MP,NB) ) THEN
            UL(M,NPX) = MIN( 0.D+0,UL(M,NPX) )
          ELSE
            UL(M,NPX) = 0.D+0
          ENDIF
!
!---    x-y-z seepage boundary condition  ---
!
        ELSEIF( ABS(IBCT(IEQW,NB)).EQ.45 ) THEN 
           IF( PG(MP,N)-PL(MP,N).GT.EPSL ) THEN
!           IF( ABS(1.D+0-SL(MP,N)).GT.EPSL ) THEN
             IF( PLB(MP,NB)-PGB(MP,NB).GT.EPSL ) THEN
               UL(M,NPX) = MAX( 0.D+0,UL(M,NPX) )
             ELSE
               UL(M,NPX) = MIN( 0.D+0,UL(M,NPX)*(RKM**4) )
             ENDIF
           ELSE
             IF( PLB(MP,NB)-PGB(MP,NB).LT.EPSL ) THEN
               UL(M,NPX) = MIN( 0.D+0,UL(M,NPX) )
             ENDIF
           ENDIF
!
!---    Shuttleworth-Wallace boundary condition  ---
!
        ELSEIF( IBCT(IEQW,NB).EQ.22 ) THEN
          IF( PL(MP,N)-PG(2,N).GE.EPSL ) THEN
            UL(M,NPX) = MIN( 0.D+0,UL(M,NPX) )
          ELSE
            UL(M,NPX) = 0.D+0
          ENDIF
!
!---    Dirichlet-Outflow boundary condition  ---
!
        ELSEIF( IBCT(IEQW,NB).EQ.26 ) THEN
          IF( UL(1,NPX).LT.-EPSL ) THEN
            UL(M,NPX) = UL(M,NPX)
          ELSE
            UL(M,NPX) = 0.D+0
          ENDIF
        ENDIF
  100 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of DRCVLW group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE DRCVLE( N,NB )
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
!     Compute the aqueous-phase Darcy flux from pressure gradients
!     and gravitational body forces on a east boundary.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1993.
!     Last Modified by MD White, PNNL, December 13, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
      USE POINTE
      USE JACOB
      USE GRID
      USE FLUXP
      USE FDVS
      USE FDVP
      USE CONST
      USE BCVS
      USE BCVP
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
      SUB_LOG(ISUB_LOG) = '/DRCVLE'
      IF( INDEX(SVN_ID(52)(1:1),'$').EQ.0 ) SVN_ID(52) =
     & '$Id: drcvlb.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      I = ID(N)
      J = JD(N)
      K = KD(N)
      NQX = NSX(N)+1
      OECX = 0.D+0
      DO 100 M = 1,ISVF
        MN = MNEG(M)
        INDX = 11
        IF( ISLC(7).EQ.1 .OR. ISLC(7).EQ.2 )
     &    OECX = DIFMN(OEC(MN,N),OECB(MN,NB),
     &    DXGF(N),DXGF(N),0.D+0,INDX)
        DPOS = OECX*(POSM(MN,N)-POSB(MN,NB))
        HX = PL(MN,N)-PLB(MN,NB)+DPOS
     &    -5.D-1*GRVX(NQX)*DXGF(N)*RHOLB(MN,NB)
        IF( M.EQ.1 ) HD = HX
        INDX = 8
        RKM = DIFMN(RKL(1,MN,N),RKLB(1,MN,NB),DXGF(N),DXGF(N),HD,INDX)
        INDX = 5
        VM = DIFMN(VISL(MN,N),VISLB(MN,NB),DXGF(N),DXGF(N),HD,INDX)
        PERM_PX = PERMRF(MN,N)*PERM(1,IZ(N))
        UL(M,NQX) = 2.D+0*PERM_PX*RKM*HX/(VM*DXGF(N))
!
!---    Outflow, diode or potential-evaporation
!       boundary condition  ---
!
        IF( ABS(IBCT(IEQW,NB)).EQ.7 .OR.
     &    ABS(IBCT(IEQW,NB)).EQ.23 ) THEN
          UL(M,NQX) = MAX( 0.D+0,UL(M,NQX) )
!
!---    Seepage boundary condition  ---
!
        ELSEIF( ABS(IBCT(IEQW,NB)).EQ.17 ) THEN 
          PLBX = PL(MN,N) - RHOL(MN,N)*5.D-1*GRVX(NQX)*DXGF(N)
          IF( PLBX.GT.PLB(MN,NB) ) THEN
            UL(M,NQX) = MAX( 0.D+0,UL(M,NQX) )
          ELSE
            UL(M,NQX) = 0.D+0
          ENDIF
!
!---    x-y-z seepage boundary condition  ---
!
        ELSEIF( ABS(IBCT(IEQW,NB)).EQ.45 ) THEN 
           IF( PG(MN,N)-PL(MN,N).GT.EPSL ) THEN
!           IF( ABS(1.D+0-SL(MN,N)).GT.EPSL ) THEN
             IF( PLB(MN,NB)-PGB(MN,NB).GT.EPSL ) THEN
               UL(M,NQX) = MIN( 0.D+0,UL(M,NQX) )
             ELSE
               UL(M,NQX) = MAX( 0.D+0,UL(M,NQX)*(RKM**4) )
             ENDIF
           ELSE
             IF( PLB(MN,NB)-PGB(MN,NB).LT.EPSL ) THEN
               UL(M,NQX) = MAX( 0.D+0,UL(M,NQX) )
             ENDIF
           ENDIF
!
!---    Shuttleworth-Wallace boundary condition  ---
!
        ELSEIF( IBCT(IEQW,NB).EQ.22 ) THEN
          IF( PL(MN,N)-PG(2,N).GE.EPSL ) THEN
            UL(M,NQX) = MAX( 0.D+0,UL(M,NQX) )
          ELSE
            UL(M,NQX) = 0.D+0
          ENDIF
!
!---    Dirichlet-Outflow boundary condition  ---
!
        ELSEIF( IBCT(IEQW,NB).EQ.26 ) THEN
          IF( UL(1,NQX).GT.EPSL ) THEN
            UL(M,NQX) = UL(M,NQX)
          ELSE
            UL(M,NQX) = 0.D+0
          ENDIF
        ENDIF
  100 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of DRCVLE group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE DRCVLN( N,NB )
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
!     Compute the aqueous-phase Darcy flux from pressure gradients
!     and gravitational body forces on a north boundary.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1993.
!     Last Modified by MD White, PNNL, December 13, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
      USE POINTE
      USE JACOB
      USE GRID
      USE FLUXP
      USE FDVS
      USE FDVP
      USE CONST
      USE BCVS
      USE BCVP
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
      SUB_LOG(ISUB_LOG) = '/DRCVLN'
      IF( INDEX(SVN_ID(52)(1:1),'$').EQ.0 ) SVN_ID(52) =
     & '$Id: drcvlb.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      I = ID(N)
      J = JD(N)
      K = KD(N)
      NQY = NSY(N)+IFLD
      OECY = 0.D+0
      DO 100 M = 1,ISVF
        MN = MNEG(M)
        INDX = 11
        IF( ISLC(7).EQ.1 .OR. ISLC(7).EQ.2 )
     &    OECY = DIFMN(OEC(MN,N),OECB(MN,NB),
     &    DYGF(N),DYGF(N),0.D+0,INDX)
        DPOS = OECY*(POSM(MN,N)-POSB(MN,NB))
        HY = PL(MN,N)-PLB(MN,NB)+DPOS
     &    -5.D-1*GRVY(NQY)*DYGF(N)*RP(I)*RHOLB(MN,NB)
        IF( M.EQ.1 ) HD = HY
        INDX = 8
        RKM = DIFMN(RKL(2,MN,N),RKLB(2,MN,NB),DYGF(N),DYGF(N),HD,INDX)
        INDX = 5
        VM = DIFMN(VISL(MN,N),VISLB(MN,NB),DYGF(N),DYGF(N),HD,INDX)
        PERM_PX = PERMRF(MN,N)*PERM(2,IZ(N))
        VL(M,NQY) = 2.D+0*PERM_PX*RKM*HY/(VM*RP(I)*DYGF(N))
!
!---    Outflow, diode or potential-evaporation
!       boundary condition  ---
!
        IF( ABS(IBCT(IEQW,NB)).EQ.7 .OR.
     &    ABS(IBCT(IEQW,NB)).EQ.23 ) THEN
          VL(M,NQY) = MAX( 0.D+0,VL(M,NQY))
!
!---    Seepage boundary condition  ---
!
        ELSEIF( ABS(IBCT(IEQW,NB)).EQ.17 ) THEN 
          PLBX = PL(MN,N) - RHOL(MN,N)*5.D-1*GRVY(NQY)*DYGF(N)*RP(I)
          IF( PLBX.GT.PLB(MN,NB) ) THEN
            VL(M,NQY) = MAX( 0.D+0,VL(M,NQY))
          ELSE
            VL(M,NQY) = 0.D+0
          ENDIF
!
!---    x-y-z seepage boundary condition  ---
!
        ELSEIF( ABS(IBCT(IEQW,NB)).EQ.45) THEN 
           IF( PG(MN,N)-PL(MN,N).GT.EPSL ) THEN
!           IF( ABS(1.D+0-SL(MN,N)).GT.EPSL ) THEN
             IF( PLB(MN,NB)-PGB(MN,NB).GT.EPSL ) THEN
               VL(M,NQY) = MIN( 0.D+0,VL(M,NQY))
             ELSE
               VL(M,NQY) = MAX( 0.D+0,VL(M,NQY)*(RKM**4) )
             ENDIF
           ELSE
             IF( PLB(MN,NB)-PGB(MN,NB).LT.EPSL ) THEN
               VL(M,NQY) = MAX( 0.D+0,VL(M,NQY))
             ENDIF
           ENDIF
!
!---    Shuttleworth-Wallace boundary condition  ---
!
        ELSEIF( IBCT(IEQW,NB).EQ.22 ) THEN
          IF( PL(MN,N)-PG(2,N).GE.EPSL ) THEN
            VL(M,NQY) = MAX( 0.D+0,VL(M,NQY))
          ELSE
            VL(M,NQY) = 0.D+0
          ENDIF
!
!---    Dirichlet-Outflow boundary condition  ---
!
        ELSEIF( IBCT(IEQW,NB).EQ.26 ) THEN
          IF( VL(1,NQY).GT.EPSL ) THEN
            VL(M,NQY) = VL(M,NQY)
          ELSE
            VL(M,NQY) = 0.D+0
          ENDIF
        ENDIF
  100 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of DRCVLN group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE DRCVLT( N,NB )
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
!     Compute the aqueous-phase Darcy flux from pressure gradients
!     and gravitational body forces on a top boundary.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1993.
!     Last Modified by MD White, PNNL, December 13, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
      USE POINTE
      USE JACOB
      USE GRID
      USE FLUXP
      USE FDVS
      USE FDVP
      USE CONST
      USE BCVS
      USE BCVP
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
      SUB_LOG(ISUB_LOG) = '/DRCVLT'
      IF( INDEX(SVN_ID(52)(1:1),'$').EQ.0 ) SVN_ID(52) =
     & '$Id: drcvlb.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      I = ID(N)
      J = JD(N)
      K = KD(N)
      NQZ = NSZ(N)+IJFLD
      OECZ = 0.D+0
      DO 100 M = 1,ISVF
        MN = MNEG(M)
        INDX = 11
        IF( ISLC(7).EQ.1 .OR. ISLC(7).EQ.2 )
     &    OECZ = DIFMN(OEC(MN,N),OECB(MN,NB),
     &    DZGF(N),DZGF(N),0.D+0,INDX)
        DPOS = OECZ*(POSM(MN,N)-POSB(MN,NB))
        HZ = PL(MN,N)-PLB(MN,NB)+DPOS
     &    -5.D-1*GRVZ(NQZ)*DZGF(N)*RHOLB(MN,NB)
        IF( M.EQ.1 ) HD = HZ
        INDX = 8
        RKM = DIFMN(RKL(3,MN,N),RKLB(3,MN,NB),DZGF(N),DZGF(N),HD,INDX)
        INDX = 5
        VM = DIFMN(VISL(MN,N),VISLB(MN,NB),DZGF(N),DZGF(N),HD,INDX)
        PERM_PX = PERMRF(MN,N)*PERM(3,IZ(N))
        WL(M,NQZ) = 2.D+0*PERM_PX*RKM*HZ/(VM*DZGF(N))
!
!---    Outflow, diode or potential-evaporation
!       boundary condition  ---
!
        IF( ABS(IBCT(IEQW,NB)).EQ.7 .OR.
     &    ABS(IBCT(IEQW,NB)).EQ.23 ) THEN
          WL(M,NQZ) = MAX( 0.D+0,WL(M,NQZ) )
!
!---    Seepage boundary condition  ---
!
        ELSEIF( ABS(IBCT(IEQW,NB)).EQ.17 ) THEN 
          PLBX = PL(MN,N) - RHOL(MN,N)*5.D-1*GRVZ(NQZ)*DZGF(N)
          IF( PLBX.GT.PLB(MN,NB) ) THEN
            WL(M,NQZ) = MAX( 0.D+0,WL(M,NQZ) )
          ELSE
            WL(M,NQZ) = 0.D+0
          ENDIF
!
!---    x-y-z seepage boundary condition  ---
!
        ELSEIF( ABS(IBCT(IEQW,NB)).EQ.45 ) THEN 
           IF( PG(MN,N)-PL(MN,N).GT.EPSL ) THEN
!           IF( ABS(1.D+0-SL(MN,N)).GT.EPSL ) THEN
             IF( PLB(MN,NB)-PGB(MN,NB).GT.EPSL ) THEN
               WL(M,NQZ) = MIN( 0.D+0,WL(M,NQZ) )
             ELSE
               WL(M,NQZ) = MAX( 0.D+0,WL(M,NQZ)*(RKM**4) )
             ENDIF
           ELSE
             IF( PLB(MN,NB)-PGB(MN,NB).LT.EPSL ) THEN
               WL(M,NQZ) = MAX( 0.D+0,WL(M,NQZ) )
             ENDIF
           ENDIF
!
!---    Shuttleworth-Wallace boundary condition  ---
!
        ELSEIF( IBCT(IEQW,NB).EQ.22 ) THEN
          IF( PL(MN,N)-PG(2,N).GE.EPSL ) THEN
            WL(M,NQZ) = MAX( 0.D+0,WL(M,NQZ) )
          ELSE
            WL(M,NQZ) = 0.D+0
          ENDIF
!
!---    Dirichlet-Outflow boundary condition  ---
!
        ELSEIF( IBCT(IEQW,NB).EQ.26 ) THEN
          IF( WL(1,NQZ).GT.EPSL ) THEN
            WL(M,NQZ) = WL(M,NQZ)
          ELSE
            WL(M,NQZ) = 0.D+0
          ENDIF
        ENDIF
  100 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of DRCVLT group  ---
!
      RETURN
      END

