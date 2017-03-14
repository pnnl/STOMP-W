!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE BCJ_GM
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
!     Modify the geomechanics Jacobian matrix and problem vector
!     for geomechanics boundary conditions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 30 November 2016 (Magnus Carlsen
!     birthdate, Norwegian chess grandmaster).
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE JACOB
      USE GRID
      USE GLB_PAR
      USE GEOMECH
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
      REAL*8 BCX(LBCV_GM)
      REAL*8 XEX(5),YEX(5),ZEX(5)
      INTEGER IBX(5),ISX(5),IWX(5),IEX(5),INX(5),ITX(5)
!
!----------------------Data Statements---------------------------------!
!
!      DATA IBX / 1,2,4,3,1 /
!      DATA ISX / 1,5,6,2,1 /
!      DATA IWX / 1,3,7,5,1 /
!      DATA IEX / 2,6,8,4,2 /
!      DATA INX / 3,4,8,7,3 /
!      DATA ITX / 5,7,8,6,5 /
      DATA IBX / 1,3,4,2,1 /
      DATA ISX / 1,2,6,5,1 /
      DATA IWX / 1,5,7,3,1 /
      DATA IEX / 2,4,8,6,2 /
      DATA INX / 3,7,8,4,3 /
      DATA ITX / 5,6,8,7,5 /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/BCJ_GM'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  For restart simulations, eliminate the reference 
!     boundary condition  ---
!
      IF( IEO.EQ.2 ) THEN
        DO NB = 1,NBC_GM
          IF( IBCR_GM(NB).EQ.1 ) THEN
            DO M = 2,IBCM_GM(NB)
              DO N = 1,LBCV_GM
                BC_GM(N,M-1,MB) = BC_GM(N,M,MB)
              ENDDO
            ENDDO
            IBCM_GM(NB) = IBCM_GM(NB)-1
            IBCR_GM(NB) = 0
          ENDIF
        ENDDO
      ENDIF
!
!---  Loop over the geomechanical boundary conditions  ---
!
      DO NB = 1,NBC_GM
        TMZ = TM
        IF( NSTEP-NRST.EQ.0 ) TMZ = TMZ*(1.D+0+EPSL)+EPSL
        MB = IBCIN_GM(NB)
        IF( IBCC_GM(NB).EQ.1 ) TMZ = MOD( TM,BC_GM(1,IBCM_GM(NB),MB) )
        IF( TMZ.LE.BC_GM(1,1,MB) ) CYCLE
!
!---    Single geomechanical boundary condition time or reference
!       boundary condition  ---
!
        IF( IBCM_GM(NB).EQ.1 .OR. IBCR_GM(NB).EQ.1 ) THEN
!
!---      Assign local boundary condition variables  ---
!
          DO N = 1,LBCV_GM
            BCX(N) = BC_GM(N,1,MB)
          ENDDO
!
!---      Eliminate the reference boundary condition  ---
!
          IF( IBCR_GM(NB).EQ.1 ) THEN
            DO M = 2,IBCM_GM(NB)
              DO N = 1,LBCV_GM
                BC_GM(N,M-1,MB) = BC_GM(N,M,MB)
              ENDDO
            ENDDO
            IBCM_GM(NB) = IBCM_GM(NB)-1
            IBCR_GM(NB) = 0
          ENDIF
!
!---    Mulitple geomechanical boundary condition times  ---
!
        ELSE
          DO M = 2,IBCM_GM(NB)
            IF( TMZ.LE.BC_GM(1,M,MB) ) THEN
              TDBC = (BC_GM(1,M,MB)-BC_GM(1,M-1,MB))
              DTBC = MIN( BC_GM(1,M,MB)-TMZ,DT )
              TFBC = (TMZ-BC_GM(1,M-1,MB))/TDBC
!
!---          Assign local boundary condition variables  ---
!
              DO N = 1,LBCV_GM
                BCX(N) = BC_GM(N,M-1,MB) + 
     &            TFBC*(BC_GM(N,M,MB)-BC_GM(N,M-1,MB))
              ENDDO
              GOTO 110
            ENDIF
          ENDDO
!
!---      Time not within geomechanical boundary condition limits
!         proceed to next geomechanical boundary condition  ---
!
          CYCLE
        ENDIF
  110   CONTINUE
!
!---    Loop over the x-, y-, and z-directions  ---
!
        DO M0 = 1,3
!
!---      Traction boundary type, compute surface normal and projection
!         of surface normal on the global x-, y-, or z-directions, 
!         reduce boundary surface area by projection and multiple
!         stress by projection surface area, force imposed on each of
!         the FE nodes in element surface is 1/4 of the stress times
!         the projected surface area  ---
!
          IF( IBCT_GM(M0,NB).EQ.1 ) THEN
!
!---        Node  ---
!
            N = IBCN_GM(NB)
!
!---        Bottom surface  ---
!
            IF( IBCD_GM(NB).EQ.-3 ) THEN
              DO I = 1,5
                XEX(I) = XE(IBX(I),N)
                YEX(I) = YE(IBX(I),N)
                ZEX(I) = ZE(IBX(I),N)
              ENDDO
              NPZ = NSZ(N)
              AX = AFZ(NPZ)
!
!---        South surface  ---
!
            ELSEIF( IBCD_GM(NB).EQ.-2 ) THEN
              DO I = 1,5
                XEX(I) = XE(ISX(I),N)
                YEX(I) = YE(ISX(I),N)
                ZEX(I) = ZE(ISX(I),N)
              ENDDO
              NPY = NSY(N)
              AX = AFY(NPY)
!
!---        West surface  ---
!
            ELSEIF( IBCD_GM(NB).EQ.-1 ) THEN
              DO I = 1,5
                XEX(I) = XE(IWX(I),N)
                YEX(I) = YE(IWX(I),N)
                ZEX(I) = ZE(IWX(I),N)
              ENDDO
              NPX = NSX(N)
              AX = AFX(NPX)
!
!---        East surface  ---
!
            ELSEIF( IBCD_GM(NB).EQ.1 ) THEN
              DO I = 1,5
                XEX(I) = XE(IEX(I),N)
                YEX(I) = YE(IEX(I),N)
                ZEX(I) = ZE(IEX(I),N)
              ENDDO
              NQX = NSX(N)+1
              IF( INBS(4,N).NE.0 ) NQX = INBS(4,N)
              AX = AFX(NQX)
!
!---        North surface  ---
!
            ELSEIF( IBCD_GM(NB).EQ.2 ) THEN
              DO I = 1,5
                XEX(I) = XE(INX(I),N)
                YEX(I) = YE(INX(I),N)
                ZEX(I) = ZE(INX(I),N)
              ENDDO
              NQY = NSY(N)+IFLD
              IF( INBS(5,N).NE.0 ) NQY = INBS(5,N)
              AX = AFY(NQY)
!
!---        Top surface  ---
!
            ELSEIF( IBCD_GM(NB).EQ.3 ) THEN
              DO I = 1,5
                XEX(I) = XE(ITX(I),N)
                YEX(I) = YE(ITX(I),N)
                ZEX(I) = ZE(ITX(I),N)
              ENDDO
              NQZ = NSZ(N)+IJFLD
              IF( INBS(6,N).NE.0 ) NQZ = INBS(6,N)
              AX = AFZ(NQZ)
            ENDIF
!
!---        Surface normal x-, y-, and z-components  ---
!
            XNX = 0.D+0
            YNX = 0.D+0
            ZNX = 0.D+0
            DO I = 1,4
              XNX = XNX + (YEX(I)-YEX(I+1))*(ZEX(I)+ZEX(I+1))
              YNX = YNX + (ZEX(I)-ZEX(I+1))*(XEX(I)+XEX(I+1))
              ZNX = ZNX + (XEX(I)-XEX(I+1))*(YEX(I)+YEX(I+1))
            ENDDO
!
!---        Surface x-, y-, and z-projections  ---
!
            XPX = XNX/SQRT((XNX**2) + (YNX**2) + (ZNX**2))
            YPX = YNX/SQRT((XNX**2) + (YNX**2) + (ZNX**2))
            ZPX = ZNX/SQRT((XNX**2) + (YNX**2) + (ZNX**2))
!
!---        FE nodal force, Pa  ---
!
            IF( M0.EQ.1 ) THEN
              FX = AX*XPX*BCX((M0-1)*3+2)
            ELSEIF( M0.EQ.2 ) THEN
              FX = AX*YPX*BCX((M0-1)*3+2)
            ELSEIF( M0.EQ.3 ) THEN
              FX = AX*ZPX*BCX((M0-1)*3+2)
            ENDIF

!
!---        Banded solver  ---
!
            IF( ILES.EQ.1 ) THEN
!
!---          Loop over the FE nodes on the element surface  ---
!
              DO I = 1,4
                IF( IBCD_GM(NB).EQ.-3 ) THEN
                  NFEN = ND_GM(IBX(I),N)
                ELSEIF( IBCD_GM(NB).EQ.-2 ) THEN
                  NFEN = ND_GM(ISX(I),N)
                ELSEIF( IBCD_GM(NB).EQ.-1 ) THEN
                  NFEN = ND_GM(IWX(I),N)
                ELSEIF( IBCD_GM(NB).EQ.1 ) THEN
                  NFEN = ND_GM(IEX(I),N)
                ELSEIF( IBCD_GM(NB).EQ.2 ) THEN
                  NFEN = ND_GM(INX(I),N)
                ELSEIF( IBCD_GM(NB).EQ.3 ) THEN
                  NFEN = ND_GM(ITX(I),N)
                ENDIF
!
!---            Equation number  ---
!
                NMD = (IM_GM(NFEN)-1)*3 + M0
                BLU(NMD) = BLU(NMD) + 2.5D-1*FX
              ENDDO
            ELSE
              INDX = 3
              CHMSG = 'Unknown Linear Equation Solver'
              CALL WRMSGS( INDX )
            ENDIF

!
!---      Displacement boundary type, assign boundary condition
!         value to problem vector, set diagonal term to 1.0, and set
!         off diagonal terms to 0.0  ---
!
          ELSEIF( IBCT_GM(M0,NB).EQ.3 ) THEN
!
!---        FE node  ---
!
            NFEN = IBCN_GM(NB)

!
!---        Banded solver  ---
!
            IF( ILES.EQ.1 ) THEN
!
!---          Equation number  ---
!
              NMD = (IM_GM(NFEN)-1)*3 + M0
!
!---          Loop over the elements connected to the FE node  ---
!
              DO M1 = 1,8
                NX = NE_GM(M1,NFEN)
                IF( NX.EQ.0 ) CYCLE
!
!---            Loop over the FE nodes of the element  ---
!
                DO M2 = 1,8
                  MFEN = ND_GM(M2,NX)
!
!---              Loop over the displacement equations  ---
!
                  DO M3 = 1,3
!
!---                Column number  ---
!
                    MCOL = (IM_GM(MFEN)-1)*3 + M3
!
!---                Row number  ---
!
                    MROW = NMD - MCOL + MD_GM
!
!---                Diagonal term  ---
!
                    IF( NMD.EQ.MCOL ) THEN
!                      ALU(MROW,MCOL) = 1.D+0
!
!---                Off diagonal terms  ---
!
                    ELSE
                      ALU(MROW,MCOL) = 0.D+0
                    ENDIF
                  ENDDO
                ENDDO
              ENDDO
              BLU(NMD) = BCX((M0-1)*3+2)
            ELSE
              INDX = 3
              CHMSG = 'Unknown Linear Equation Solver'
              CALL WRMSGS( INDX )
            ENDIF

!
!---      Force boundary type, assign boundary condition
!         value to problem vector  ---
!
          ELSEIF( IBCT_GM(M0,NB).EQ.5 ) THEN
!
!---        FE node  ---
!
            NFEN = IBCN_GM(NB)

!
!---        Banded solver  ---
!
            IF( ILES.EQ.1 ) THEN
!
!---          Equation number  ---
!
              NMD = (IM_GM(NFEN)-1)*3 + M0
              BLU(NMD) = BLU(NMD) + BCX((M0-1)*3+2)
            ELSE
              INDX = 3
              CHMSG = 'Unknown Linear Equation Solver'
              CALL WRMSGS( INDX )
            ENDIF

          ENDIF
        ENDDO
      ENDDO
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of BCJ_GM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CHK_GM
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
!     Preprocessor routine for geomechanical boundary conditions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 30 November 2016 (Magnus Carlsen
!     birthdate, Norwegian chess grandmaster).
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE CONST
      USE GEOMECH
      USE GLB_PAR
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
      SUB_LOG(ISUB_LOG) = '/CHK_GM'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Loop over geomechanical boundary conditions, converting the 
!     temporary value of IBCN_GM to the FE node number for 
!     displacment geomechanical boundary conditions  ---
!
      DO NB = 1,NBC_GM
        IF( IBCT_GM(1,NB).EQ.3 .OR. IBCT_GM(2,NB).EQ.3 .OR.
     &    IBCT_GM(3,NB).EQ.3 .OR. IBCT_GM(2,NB).EQ.4 .OR.
     &    IBCT_GM(3,NB).EQ.4 .OR. IBCT_GM(2,NB).EQ.4 ) THEN
          N = IBCN_GM(NB)
          M = IBCD_GM(NB)
          IBCN_GM(NB) = ND_GM(M,N)
        ENDIF
      ENDDO
!
!---  Loop over geomechanical boundary conditions, converting the 
!     temporary value of IBCN_GM to the FE node number for 
!     force geomechanical boundary conditions  ---
!
      DO NB = 1,NBC_GM
        IF( IBCT_GM(1,NB).EQ.5 .OR. IBCT_GM(2,NB).EQ.5 .OR.
     &    IBCT_GM(3,NB).EQ.5 .OR. IBCT_GM(2,NB).EQ.6 .OR.
     &    IBCT_GM(3,NB).EQ.6 .OR. IBCT_GM(2,NB).EQ.6 ) THEN
          N = IBCN_GM(NB)
          M = IBCD_GM(NB)
          IBCN_GM(NB) = ND_GM(M,N)
        ENDIF
      ENDDO
!
!---  Loop over geomechanical boundary conditions, checking for
!     multiple displacement boundary conditions applied to a single 
!     FE node and x-, y-, and z-direction combination over the same
!     time interval  ---
!
      DO NB = 1,NBC_GM
        IF( IBCT_GM(1,NB).GE.3 .OR. IBCT_GM(2,NB).GE.3 .OR.
     &    IBCT_GM(3,NB).GE.3 ) THEN
          MB = IBCIN_GM(NB)
          DO NBX = 1,NB-1
            IF( (IBCT_GM(1,NBX).GE.3 .AND. IBCT_GM(1,NB).GE.3) .OR. 
     &          (IBCT_GM(2,NBX).GE.3 .AND. IBCT_GM(2,NB).GE.3) .OR.
     &          (IBCT_GM(3,NBX).GE.3 .AND. IBCT_GM(3,NB).GE.3) ) THEN
              MBX = IBCIN_GM(NBX)
!
!---          Mulitple displacement boundary conditions applied
!             to a single FE node  ---
!
              IF( IBCN_GM(NBX).EQ.IBCN_GM(NB) ) THEN
!
!---            Mulitple boundary conditions applied to a single FE node
!               and x-, y-, and z-direction combination over the same
!               time interval  ---
!
                TM1X = BC_GM(1,1,MBX)
                TM2X = BC_GM(1,IBCM_GM(NBX),MBX)
                TM3X = BC_GM(1,1,MB)
                TM4X = BC_GM(1,IBCM_GM(NB),MB)
                IF( (TM1X.GE.TM3X .AND. TM1X.LE.TM4X) .OR.
     &            (TM2X.GE.TM3X .AND. TM2X.LE.TM4X) ) THEN
                    INDX = 7
                    IMSG = NB
                    CHMSG = 'Multiple Geomechanical Boundary ' // 
     &                'Conditions at Boundary Number'
                    CALL WRMSGS( INDX )
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
!
!---  For restart simulations, with geomechanics data in the restart
!     file, use the initial displacements in the restart file to 
!     compute initial strains and stresses, and set the reference
!     volumetric stress  ---
!
      IF( IEO.EQ.2 .AND. ISLC(50).GT.0 ) THEN
!
!---    Load reference displacements at finite elment nodes  ---
!
        CALL LDDISP_GM
!
!---    Strains at finite element centroids  ---
!
        CALL STRAIN_GM
!
!---    Stresses at finite element centroids  ---
!
        CALL STRESS_GM
!
!---    Reference volumetric stresses at finite element centroids  ---
!
        INDX = 0
        CALL STRESSV_GM( INDX )
      ENDIF
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of CHK_GM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CONNFEN
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
!     Create a map of finite-element nodes for geomechanics.
!
!     ISDX is an array of search directions and the location of the
!     node in the hexahedron element in the ending element.
!     There are 15 search patterns for each of eight element nodes
!
!     FE Node 1 : B:BS:BSW:BW:BWS:S:SW:SWB:SB:SBW:W:WB:WBS:WS:WSB
!     FE Node 2 : B:BS:BSE:BE:BES:S:SE:SEB:SB:SBE:E:EB:EBS:ES:ESB
!     FE Node 3 : B:BN:BNW:BW:BWN:N:NW:NWB:NB:NBW:W:WB:WBN:WN:WNB
!     FE Node 4 : B:BN:BNE:BE:BEN:N:NE:NEB:NB:NBE:E:EB:EBN:EN:ENB
!     FE Node 5 : T:TS:TSW:TW:TWS:S:SW:SWT:ST:STW:W:WT:WTS:WS:WST
!     FE Node 6 : T:TS:TSE:TE:TES:S:SE:SET:ST:STE:E:ET:ETS:ES:EST
!     FE Node 7 : T:TN:TNW:TW:TWN:N:NW:NWT:NT:NTW:W:WT:WTN:WN:WNT
!     FE Node 8 : T:TN:TNE:TE:TEN:N:NE:NET:NT:NTE:E:ET:ETN:EN:ENT
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 October 2016 (Halloween).
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GEOMECH
      USE CONST
      USE GLB_PAR
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
      INTEGER ISDX(4,15,8)
!
!----------------------Data Statements---------------------------------!
!
      DATA ISDX / 1,0,0,5,1,2,0,7,1,2,3,8,1,3,0,6,
     &            1,3,2,8,2,0,0,3,2,3,0,4,2,3,1,8,
     &            2,1,0,7,2,1,3,8,3,0,0,2,3,1,0,6,
     &            3,1,2,8,3,2,0,4,3,2,1,8,1,0,0,6,
     &            1,2,0,8,1,2,4,7,1,4,0,5,1,4,2,7,
     &            2,0,0,4,2,4,0,3,2,4,1,7,2,1,0,8,
     &            2,1,4,7,4,0,0,1,4,1,0,5,4,1,2,7,
     &            4,2,0,3,4,2,1,7,1,0,0,7,1,5,0,5,
     &            1,5,3,6,1,3,0,8,1,3,5,6,5,0,0,1,
     &            5,3,0,2,5,3,1,6,5,1,0,5,5,1,3,6,
     &            3,0,0,4,3,1,0,8,3,1,5,6,3,5,0,2,
     &            3,5,1,6,1,0,0,8,1,5,0,6,1,5,4,5,
     &            1,4,0,7,1,4,5,5,5,0,0,2,5,4,0,1,
     &            5,4,1,5,5,1,0,6,5,1,4,5,4,0,0,3,
     &            4,1,0,7,4,1,5,5,4,5,0,1,4,5,1,5,
     &            6,0,0,1,6,2,0,3,6,2,3,4,6,3,0,2,
     &            6,3,2,4,2,0,0,7,2,3,0,8,2,3,6,4,
     &            2,6,0,3,2,6,3,4,3,0,0,6,3,6,0,2,
     &            3,6,2,4,3,2,0,8,3,2,6,4,6,0,0,2,
     &            6,2,0,4,6,2,4,3,6,4,0,1,6,4,2,3,
     &            2,0,0,8,2,4,0,7,2,4,6,3,2,6,0,4,
     &            2,6,4,3,4,0,0,5,4,6,0,1,4,6,2,3,
     &            4,2,0,7,4,2,6,3,6,0,0,3,6,5,0,1,
     &            6,5,3,2,6,3,0,4,6,3,5,2,5,0,0,5,
     &            5,3,0,6,5,3,6,2,5,6,0,1,5,6,3,2,
     &            3,0,0,8,3,6,0,4,3,6,5,2,3,5,0,6,
     &            3,5,6,2,6,0,0,4,6,5,0,2,6,5,4,1,
     &            6,4,0,3,6,4,5,1,5,0,0,6,5,4,0,5,
     &            5,4,6,1,5,6,0,2,5,6,4,1,4,0,0,7,
     &            4,6,0,3,4,6,5,1,4,5,0,5,4,5,6,1 /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/CONNFEN'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Loop over all nodes create a map of finite element nodes,
!     where nodes are numbered in the order around the hexahedron
!     grid cells as
!
!     1 i,j,k
!     2 i+1,j,k
!     3 i,j+1,k
!     4 i+1,j+1,k
!     5 i,j,k+1
!     6 i+1,j,k+1
!     7 i,j+1,k+1
!     8 i+1,j+1,k+1  ---
!
      NC = 0
      DO N = 1,NFLD
        IF( IXP(N).EQ.0 ) CYCLE
!
!---    Loop over FE Nodes  ---
!
        DO NFE = 1,8
          IF( ND_GM(NFE,N).EQ.0 ) THEN
            NC = NC + 1
            IF( NC.GT.LFEN ) THEN
              INDX = 5
              CHMSG = 'Number of FE Nodes > Parameter LFEN'
              CALL WRMSGS( INDX )
            ENDIF
            ND_GM(NFE,N) = NC
            NE_GM(NFE,NC) = N
!
!---        Loop over number of connection paths  ---
!
            DO NCP = 1,15
!
!---          Loop over number of connection tiers  ---
!
              NX = N
              DO NCT = 1,3
                IX = ISDX(NCT,NCP,NFE)
                IF( ISDX(NCT,NCP,NFE).EQ.0 ) EXIT
!
!---            Connection to neighboring element found  ---
!
                IF( ICM(5,IX,NX).NE.0 ) THEN
                  NX = ICM(5,IX,NX)
!
!---            Connection string broken  ---
!
                ELSE
                  NX = 0
                  EXIT
                ENDIF
              ENDDO
!
!---         Connection string completed  ---
!
              IF( NX.NE.0 ) THEN
                JX = ISDX(4,NCP,NFE)
                ND_GM(JX,NX) = NC
                NE_GM(JX,NC) = NX
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO
      NFEN_GM = NC
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of CONNFEN group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE DISP_GM
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
!     Update displacements at finite element nodes.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 January 2017 (3M begins marketing
!     Scotch Tape in 1930).
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE JACOB
      USE GLB_PAR
      USE GEOMECH
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
      SUB_LOG(ISUB_LOG) = '/DISP_GM'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Loop over the finite-element nodes ---
!
      DO NFEN = 1,NFEN_GM
        M0 = (NFEN-1)*3
        U_GM(2,NFEN) = BLU(M0+1)
        V_GM(2,NFEN) = BLU(M0+2)
        W_GM(2,NFEN) = BLU(M0+3)
      ENDDO
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of DISP_GM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ESDM( PXIX,PETAX,PMUX,XEX,YEX,ZEX,BX,DAX )
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
!     Compute the 6 x 24 components of the strain displacement matrix
!     for a hexahedron finite element at the location in the natural
!     coordinate system Xi, Eta, and Mu
!
!     PXIX input value of natural coordinate Xi -1 to +1
!     PETAX input value of natural coordinate Eta -1 to +1
!     PMUX input value of natural coordinate Mu -1 to +1
!     XE(1:8) input value of x global Cartesian hexahedron nodes
!     YE(1:8) input value of y global Cartesian hexahedron nodes
!     ZE(1:8) input value of z global Cartesian hexahedron nodes
!
!     where nodes are numbered in the order around the hexahedron
!     grid cells as
!
!     1 i,j,k
!     2 i+1,j,k
!     3 i,j+1,k
!     4 i+1,j+1,k
!     5 i,j,k+1
!     6 i+1,j,k+1
!     7 i,j+1,k+1
!     8 i+1,j+1,k+1  ---
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 2 November 2016 (Maiden flight of
!     Howard Hughes Spruce Goose).
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GEOMECH
      USE CONST
      USE GLB_PAR
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
      REAL*8 XIX(8),ETAX(8),MUX(8)
      REAL*8 XEX(8),YEX(8),ZEX(8)
      REAL*8 SAX(3,3),AIX(3,3),BX(6,24)
!
!----------------------Data Statements---------------------------------!
!
      DATA XIX / -1.D+0,1.D+0,-1.D+0,1.D+0,-1.D+0,1.D+0,-1.D+0,1.D+0 /
      DATA ETAX / -1.D+0,-1.D+0,1.D+0,1.D+0,-1.D+0,-1.D+0,1.D+0,1.D+0 /
      DATA MUX / -1.D+0,-1.D+0,-1.D+0,-1.D+0,1.D+0,1.D+0,1.D+0,1.D+0 /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ESDM'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Initialize a  ---
!
      DO J = 1,3
        DO I = 1,3
          SAX(I,J) = 0.D+0
        ENDDO
      ENDDO
!
!---  Loop over hexahedron nodes  ---
!
      DO M = 1,8
!
!---    Xi multiplier  ---
!
        VXIX = XIX(M)*(1.D+0+PETAX*ETAX(M))*(1.D+0+PMUX*MUX(M))
!
!---    Eta multiplier  ---
!
        VETAX = ETAX(M)*(1.D+0+PXIX*XIX(M))*(1.D+0+PMUX*MUX(M))
!
!---    Mu multiplier  ---
!
        VMUX = MUX(M)*(1.D+0+PETAX*ETAX(M))*(1.D+0+PXIX*XIX(M))
!
!---    dx/dxi at Xi, Eta, and Mu  ---
!
        SAX(1,1) = SAX(1,1) + XEX(M)*VXIX
!
!---    dy/dxi at Xi, Eta, and Mu  ---
!
        SAX(1,2) = SAX(1,2) + YEX(M)*VXIX
!
!---    dz/dxi at Xi, Eta, and Mu  ---
!
        SAX(1,3) = SAX(1,3) + ZEX(M)*VXIX
!
!---    dx/deta at Xi, Eta, and Mu  ---
!
        SAX(2,1) = SAX(2,1) + XEX(M)*VETAX
!
!---    dy/deta at Xi, Eta, and Mu  ---
!
        SAX(2,2) = SAX(2,2) + YEX(M)*VETAX
!
!---    dz/deta at Xi, Eta, and Mu  ---
!
        SAX(2,3) = SAX(2,3) + ZEX(M)*VETAX
!
!---    dx/dmu at Xi, Eta, and Mu  ---
!
        SAX(3,1) = SAX(3,1) + XEX(M)*VMUX
!
!---    dy/dmu at Xi, Eta, and Mu  ---
!
        SAX(3,2) = SAX(3,2) + YEX(M)*VMUX
!
!---    dz/dmu at Xi, Eta, and Mu  ---
!
        SAX(3,3) = SAX(3,3) + ZEX(M)*VMUX
      ENDDO
      DO J = 1,3
        DO I = 1,3
          SAX(I,J) = 1.25D-1*SAX(I,J)
        ENDDO
      ENDDO
!
!---  Inverse Jacobian matrix  ---
!
      AIX(1,1) = SAX(2,2)*SAX(3,3) - SAX(2,3)*SAX(3,2)
      AIX(1,2) = SAX(1,3)*SAX(3,2) - SAX(1,2)*SAX(3,3)
      AIX(1,3) = SAX(1,2)*SAX(2,3) - SAX(1,3)*SAX(2,2)
      AIX(2,1) = SAX(3,1)*SAX(2,3) - SAX(2,1)*SAX(3,3)
      AIX(2,2) = SAX(1,1)*SAX(3,3) - SAX(1,3)*SAX(3,1)
      AIX(2,3) = SAX(1,3)*SAX(2,1) - SAX(1,1)*SAX(2,3)
      AIX(3,1) = SAX(2,1)*SAX(3,2) - SAX(3,1)*SAX(2,2)
      AIX(3,2) = SAX(3,1)*SAX(1,2) - SAX(1,1)*SAX(3,2)
      AIX(3,3) = SAX(1,1)*SAX(2,2) - SAX(1,2)*SAX(2,1)
!
!---  Determinant of the Jacobian matrix  ---
!
      DAX = SAX(1,1)*AIX(1,1) + SAX(2,1)*AIX(1,2) + SAX(3,1)*AIX(1,3)
!
!---  Divide by determinant to complete the inverse  ---
!
      DO J = 1,3
        DO I = 1,3
          AIX(I,J) = AIX(I,J)/DAX
        ENDDO
      ENDDO
!
!---  Zero element strain displacement matrix  ---
!
      DO J = 1,24
        DO I = 1,6
          BX(I,J) = 0.D+0
        ENDDO
      ENDDO
!
!---  Element strain displacement matrix at Xi, Eta, and Mu  ---
!
      DO M = 1,8
        MX = (M-1)*3 + 1
        MY = (M-1)*3 + 2
        MZ = (M-1)*3 + 3
!
!---    Partial of shape factor with respect to hexahedron 
!       coordinates (Xi, Eta, and Mu)  ---
!
        DXIX = 1.25D-1*XIX(M)*(1.D+0 + PETAX*ETAX(M))*
     &    (1.D+0 + PMUX*MUX(M))
        DETAX = 1.25D-1*ETAX(M)*(1.D+0 + PMUX*MUX(M))*
     &    (1.D+0 + PXIX*XIX(M))
        DMUX = 1.25D-1*MUX(M)*(1.D+0 + PXIX*XIX(M))*
     &    (1.D+0 + PETAX*ETAX(M))
!
!---    Partial of shape factor with respect to global coordinates
!       (x, y, and z)  ---
!
        DXX = (AIX(1,1)*DXIX + AIX(1,2)*DETAX + AIX(1,3)*DMUX)
        DYX = (AIX(2,1)*DXIX + AIX(2,2)*DETAX + AIX(2,3)*DMUX)
        DZX = (AIX(3,1)*DXIX + AIX(3,2)*DETAX + AIX(3,3)*DMUX)
!
!---    du/dx for strain epsilon xx  ---
!
        BX(1,MX) = BX(1,MX) + DXX
!
!---    dv/dy for strain epsilon yy  ---
!
        BX(2,MY) = BX(2,MY) + DYX
!
!---    dw/dz for strain epsilon zz  ---
!
        BX(3,MZ) = BX(3,MZ) + DZX
!
!---    (dv/dz + dw/dy) for strain 2*epsilon yz  ---
!
        BX(4,MY) = BX(4,MY) + DZX
        BX(4,MZ) = BX(4,MZ) + DYX
!
!---    (du/dz + dw/dx) for strain 2*epsilon xz  ---
!
        BX(5,MX) = BX(5,MX) + DZX
        BX(5,MZ) = BX(5,MZ) + DXX
!
!---    (du/dy + dv/dx) for strain 2*epsilon xy  ---
!
        BX(6,MX) = BX(6,MX) + DYX
        BX(6,MY) = BX(6,MY) + DXX
      ENDDO
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ESDM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE GIESM( EX,ESMX,PTOX,PTVX,XEX,YEX,ZEX )
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
!     Gauss integration of the element stiffness matrix.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 November 2016 (Ensisheim meteorite).
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GEOMECH
      USE CONST
      USE GLB_PAR
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
      REAL*8 EX(6,6),ESMX(24,24)
      REAL*8 XEX(8),YEX(8),ZEX(8)
      REAL*8 BTEBX(24,24),BTEX(24,6)
      REAL*8 BX(6,24),BTX(24,6)
      REAL*8 WX(5),PX(5)
      REAL*8 BTPTX(24),PTOX(6),PTVX(24)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/GIESM'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Initialization  ---
!
      DO M = 1,5
        WX(M) = 0.D+0
        PX(M) = 0.D+0
      ENDDO
      DO L = 1,6
        DO M = 1,24
          BX(L,M) = 0.D+0
        ENDDO
      ENDDO
      DO M = 1,24
        PTVX(M) = 0.D+0
      ENDDO
!
!---  Number of sample points  ---
!
      IF( ISLC(72).EQ.2 ) THEN
        WX(1) = 1.D+0
        WX(2) = 1.D+0
        PX(1) = -1.D+0/SQRT(3.D+0)
        PX(2) = 1.D+0/SQRT(3.D+0)
      ELSEIF( ISLC(72).EQ.3 ) THEN
        WX(1) = 5.D+0/9.D+0
        WX(2) = 8.D+0/9.D+0
        WX(3) = 5.D+0/9.D+0
        PX(1) = -SQRT(3.D+0/5.D+0)
        PX(2) = 0.D+0
        PX(3) = SQRT(3.D+0/5.D+0)
      ELSEIF( ISLC(72).EQ.4 ) THEN
        WX(1) = 5.D-1 - (1.D+0/6.D+0)*SQRT(5.D+0/6.D+0)
        WX(2) = 5.D-1 + (1.D+0/6.D+0)*SQRT(5.D+0/6.D+0)
        WX(3) = 5.D-1 + (1.D+0/6.D+0)*SQRT(5.D+0/6.D+0)
        WX(4) = 5.D-1 - (1.D+0/6.D+0)*SQRT(5.D+0/6.D+0)
        PX(1) = -SQRT((3.D+0 + 2.D+0*SQRT(6.D+0/5.D+0))/7.D+0)
        PX(2) = -SQRT((3.D+0 - 2.D+0*SQRT(6.D+0/5.D+0))/7.D+0)
        PX(3) = SQRT((3.D+0 - 2.D+0*SQRT(6.D+0/5.D+0))/7.D+0)
        PX(4) = SQRT((3.D+0 + 2.D+0*SQRT(6.D+0/5.D+0))/7.D+0)
      ELSEIF( ISLC(72).EQ.5 ) THEN
        WX(1) = (3.22D+2 - 13.D+0*SQRT(7.D+1))/9.D+2
        WX(2) = (3.22D+2 + 13.D+0*SQRT(7.D+1))/9.D+2
        WX(3) = 5.12D+2/9.D+2
        WX(4) = (3.22D+2 + 13.D+0*SQRT(7.D+1))/9.D+2
        WX(5) = (3.22D+2 - 13.D+0*SQRT(7.D+1))/9.D+2
        PX(1) = -(1.D+0/3.D+0)*SQRT((5.D+0 + 2.D+0*SQRT(1.D+1/7.D+0)))
        PX(2) = -(1.D+0/3.D+0)*SQRT((5.D+0 - 2.D+0*SQRT(1.D+1/7.D+0)))
        PX(3) = 0.D+0
        PX(4) = (1.D+0/3.D+0)*SQRT((5.D+0 - 2.D+0*SQRT(1.D+1/7.D+0)))
        PX(5) = (1.D+0/3.D+0)*SQRT((5.D+0 + 2.D+0*SQRT(1.D+1/7.D+0)))
      ENDIF
!
!---  Numerical integration over hexahedra, loop over the three 
!     conical coordinate directions  ---
!
      DO K = 1,ISLC(72)
        DO J = 1,ISLC(72)
          DO I = 1,ISLC(72)
            CALL ESDM( PX(I),PX(J),PX(K),XEX,YEX,ZEX,BX,DETJX )
            WTX = WX(K)*WX(J)*WX(I)*DETJX
            CALL MATTRP( BX,BTX,6,24 )
            CALL MATMUL( BTX,EX,BTEX,24,6,6 )
            CALL MATMUL( BTX,PTOX,BTPTX,24,6,1 )
            CALL MATMUL( BTEX,BX,BTEBX,24,6,24 )
            DO M = 1,24
              DO L = 1,24
                ESMX(L,M) = ESMX(L,M) + WTX*BTEBX(L,M)  
              ENDDO
              PTVX(M) = PTVX(M) + WTX*BTPTX(M)
            ENDDO
          ENDDO
        ENDDO
      ENDDO
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of GIESM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE GSM( ESMX,GBDFX,PTVX,N )
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
!     Global stiffness matrix
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 November 2016 (Ensisheim meteorite).
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE PORMED
      USE GRID
      USE GLB_PAR
      USE GEOMECH
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
      REAL*8 EX(6,6),ESMX(24,24)
      REAL*8 PTOX(6),PTVX(24)
 !
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/GSM'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Rock/soil type  ---
!
      IZN = IZ(N)
!
!---  Stress-strain matrix  ---
!
      CALL SSM( EX,PROP_GM(1,IZN) )
!
!---  Gauss integration of the element stiffness matrix  ---
!
      DO J = 1,24
        DO I = 1,24
          ESMX(I,J) = 0.D+0
        ENDDO
        PTVX(J) = 0.D+0
      ENDDO
!
!---  Combined poroelasticity + thermoelasticity vector  ---
!
      DO M = 1,6
        PTOX(M) = 0.D+0
      ENDDO
!
!---  Bypass restart check for geomechanics options  ---
!
      ISLC50X = ABS(ISLC(50))
!
!---  Poroelasticity vector  ---
!
      IF( MOD(ISLC50X,10).NE.2 .AND. MOD(ISLC50X,10).NE.4 ) THEN
        PX = MAX( PG(2,N),PL(2,N),PN(2,N) ) + PATM
        PTOX(1) = PTOX(1) - PROP_GM(3,IZN)*PX
        PTOX(2) = PTOX(2) - PROP_GM(3,IZN)*PX
        PTOX(3) = PTOX(3) - PROP_GM(3,IZN)*PX
      ENDIF
!
!---  Thermoelasticity vector  ---
!
      IF( MOD(ISLC50X,10).NE.3 .AND. MOD(ISLC50X,10).NE.4 ) THEN
        PTOX(1) = PTOX(1) - PROP_GM(4,IZN)*
     &    (1.D+0-2.D+0*PROP_GM(2,IZN))*(T(2,N)-TCMP(N))/PROP_GM(1,IZN)
        PTOX(2) = PTOX(2) - PROP_GM(4,IZN)*
     &    (1.D+0-2.D+0*PROP_GM(2,IZN))*(T(2,N)-TCMP(N))/PROP_GM(1,IZN)
        PTOX(3) = PTOX(3) - PROP_GM(4,IZN)*
     &    (1.D+0-2.D+0*PROP_GM(2,IZN))*(T(2,N)-TCMP(N))/PROP_GM(1,IZN)
      ENDIF
!
!---  Finite-element node gravity body force  ---
!
      IF( ISLC50X.LT.10 ) THEN
        GBDFX = 1.25D-1*VOL(N)*(1.D+0-POR(2,IZN))*RHOS(IZN)*GRAV_GM
      ELSE
        GBDFX = 0.D+0
      ENDIF
!
!---  Gauss integration of the element stiffness matrix.
!
      CALL GIESM( EX,ESMX,PTOX,PTVX,XE(1,N),YE(1,N),ZE(1,N) )
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of GSM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCBL_GM
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
!     Load the Jacobian matrix
!
!     Each finite-element node point has three unknowns, the global
!     cartesian component of x-, y-, and z-displacments.
!
!     Equations are ordered by their finite-element node numbers.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 November 2016.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PTZR
      USE JACOB
      USE GRID
      USE GEOMECH
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
      REAL*8 ESMX(24,24),PTVX(24)




!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/JCBL_GM'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Loop over finite elements ---
!
      DO N = 1,NFLD
        IF( IXP(N).EQ.0 ) CYCLE
!
!---    Compute element contribution to Jacobian matrix ---
!
        CALL GSM( ESMX,GBDFX,PTVX,N )

!
!---    Banded solver  ---
!
        IF( ILES.EQ.1 ) THEN
!
!---      Loop over the FE Nodes of the element  ---
!
          DO M1 = 1,8
!
!---        Global FE node number  ---
!
            NFEN = ND_GM( M1,N )
!
!---        Loop over the displacement equations  ---
!
            DO M2 = 1,3
!
!---          Equation number  ---
!
              KROW = (M1-1)*3 + M2
              NMD = (IM_GM(NFEN)-1)*3 + M2
!
!---          Loop over the FE Nodes of the element  ---
!
              DO M3 = 1,8
!
!---            Global FE node number of   ---
!
                MFEN = ND_GM( M3,N )
!
!---            Loop over the displacement equations  ---
!
                DO M4 = 1,3
!
!---              Column number  ---
!
                  KCOL = (M3-1)*3 + M4
                  MCOL = (IM_GM(MFEN)-1)*3 + M4
!
!---              Row number  ---
!
                  MROW = NMD - MCOL + MD_GM
                  ALU(MROW,MCOL) = ALU(MROW,MCOL) + ESMX(KROW,KCOL)
                ENDDO
              ENDDO
!
!---          Poroelasticity + thermoelasticity  ---
!
              BLU(NMD) = BLU(NMD) - PTVX(KROW)
!
!---          Gravitational body force  ---
!
              IF( M2.EQ.3 ) BLU(NMD) = BLU(NMD) - GBDFX
            ENDDO
          ENDDO
        ELSE
          INDX = 3
          CHMSG = 'Unknown Linear Equation Solver'
          CALL WRMSGS( INDX )
        ENDIF

      ENDDO
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCBL_GM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCBP_GM
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
!     Configure the Jacobian matrix pointer arrays for geomechanics.
!
!     Each finite-element node point has three unknowns, the global
!     cartesian component of x-, y-, and z-displacments.
!
!     Equations are ordered by their finite-element node numbers.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 21 November 2016.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PTZR
      USE JACOB
      USE GRID
      USE GEOMECH
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
      INTEGER ISTCX(27)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/JCBP_GM'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Geomechanics equations half band width  ---
!
      MHB_GM = 0
      NC = 0
!
!---  Loop over number of finite-element nodes to load
!     the Jacobian matrix pointer array  ---
!
      DO NFEN = 1,NFEN_GM
!
!---    Finite-element node is inactive, only if all elements
!       that contain the FE node are inactive grid cells  ---
!
        IFX = 0
        DO M = 1,8
          N = NE_GM(M,NFEN)
          IF( N.EQ.0 ) CYCLE
          IF( IXP(N).NE.0 ) THEN
            IFX = 1
            EXIT
          ENDIF
        ENDDO
        IF( IFX.EQ.1 ) THEN
          NC = NC + 1
          IM_GM(NFEN) = NC
        ENDIF
      ENDDO
!
!---  Loop over number of finite-element nodes to compute the minimum
!     half band width  ---
!
      DO NFEN = 1,NFEN_GM
!
!---    Loop over the connections to adjacent finite elements  ---
!
        DO M1 = 1,8
          N = NE_GM(M1,NFEN)
          IF( N.EQ.0 ) CYCLE
!
!---      Loop over the FE nodes in the adjacent finite element  ---
!
          DO M2 = 1,8
            MFEN = ND_GM(M2,N)
            IF( MFEN.EQ.0 ) CYCLE
!
!---        Compute differences in displacement equations for FE nodes
!           connected through adjacent finite elements  ---
!
            IM1X = (IM_GM(NFEN)-1)*3 + 1
            IM2X = (IM_GM(MFEN)-1)*3 + 3
            IM3X = (IM_GM(NFEN)-1)*3 + 3
            IM4X = (IM_GM(MFEN)-1)*3 + 1
            MHB_GM = MAX( MHB_GM,ABS(IM1X-IM2X),ABS(IM3X-IM4X) )
          ENDDO
        ENDDO
      ENDDO
!
!---  Check half band width  ---
!
      NJD = LBD*(3*MHB_GM+1) + LSP
      IF( NJD.GT.LJD ) THEN
         INDX = 5
         CHMSG = 'Number of Geomechanics Banded Matrix Rows ' //
     &     '> Parameter LJD'
         CALL WRMSGS( INDX )
      ENDIF
      ML_GM = MHB_GM
      MU_GM = MHB_GM
      MD_GM = ML_GM + MU_GM + 1
!
!---  SPLIB .or. Lis .or. PETSc Solver  ---
!
      IF( ILES.EQ.3 .OR. ILES.EQ.4 .OR. ILES.EQ.5 ) THEN
        NC = 0



        NLU_GM(1) = 1

!
!---    Loop over number of finite-element nodes  ---
!
        DO NFEN = 1,NFEN_GM
!
!---      Determine the connection stencil for the finite-element
!         node  ---
!
          DO M1 = 1,27
            ISTCX(M1) = 0
          ENDDO
!
!---      Loop over the connections to adjacent finite elements  ---
!
          DO M2 = 1,8
            N = NE_GM(M2,NFEN)
            IF( N.EQ.0 ) CYCLE
!
!---        Loop over the FE nodes in the adjacent finite element  ---
!
            DO M3 = 1,8
              MFEN = ND_GM(M3,N)
              IF( MFEN.EQ.0 ) CYCLE
              ISTCX(NK_GM(M2,M3)) = MFEN
            ENDDO
          ENDDO
!
!---      Loop over displacement equations  ---
!
          DO M4 = 1,3
            NMD = (IM_GM(NFEN)-1)*3 + M4
!
!---        Loop over the active connections  ---
!
            DO M5 = 1,27
              IF( ISTCX(M5).EQ.0 ) CYCLE
              MFEN = ISTCX(M5)
!
!---          Loop over displacement equations  ---
!
              DO M6 = 1,3
                NC = NC + 1

                  IF( (M4*M6).EQ.1 ) KLU_GM(NFEN,M5) = NC



                  MLU_GM(NC) = (IM_GM(MFEN)-1)*3 + M6


              ENDDO
            ENDDO



            NLU_GM(NMD+1) = NC+1

          ENDDO
        ENDDO
        MK_GM = NC
      ENDIF
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCBP_GM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCBZ_GM
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
!     Zero the Jacobian matrix elements and solution vector for
!     the geomechanics solve
!
!     Each finite-element node point has three unknowns, the global
!     cartesian component of x-, y-, and z-displacments.
!
!     Equations are ordered by their finite-element node numbers.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 2 March 2017 (Birthday of Daniel Craig, 
!     English actor and producer, and 007 in 1968).
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PTZR
      USE JACOB
      USE GRID
      USE GEOMECH
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
      REAL*8 ESMX(24,24),PTVX(24)




!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/JCBZ_GM'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Geomechanics - number of unknowns equals number of active
!     finite element nodes x 3
!
      NUKX = 3*NFEN_GM
!
!---  Banded solver  ---
!
      IF( ILES.EQ.1 ) THEN
        NCOL = NUKX
        NROW = 2*MU_GM + ML_GM + 1
        DO MCOL = 1,NCOL
          DO MROW = 1,NROW
            ALU(MROW,MCOL) = 0.D+0
          ENDDO
          BLU(MCOL) = 0.D+0
          ILU(MCOL) = MCOL
        ENDDO
!
!---  SPLib Solver  ---
!
      ELSEIF( ILES.EQ.3 ) THEN
        NROW = NUKX
!$OMP PARALLEL DO
!$OMP& DEFAULT(NONE)
!$OMP& SHARED(DLU)
!$OMP& FIRSTPRIVATE(MK_GM)
!$OMP& PRIVATE(MROW)
        DO MROW = 1,MK_GM
          DLU(MROW) = 0.D+0
        ENDDO
!$OMP END PARALLEL DO
!$OMP PARALLEL DO
!$OMP& DEFAULT(NONE)
!$OMP& SHARED(BLU)
!$OMP& FIRSTPRIVATE(NROW)
!$OMP& PRIVATE(MROW)
        DO MROW = 1,NROW
          BLU(MROW) = 0.D+0
        ENDDO
!$OMP END PARALLEL DO

!
!---  Unknown Solver  ---
!
      ELSE
        INDX = 3
        CHMSG = 'Unknown Linear Equation Solver'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCBZ_GM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE LDDISP_GM
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
!     Load reference displacements at finite element nodes.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 3 February 2017 (Wake Forest 
!     University is established in 1834).
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE JACOB
      USE GLB_PAR
      USE GEOMECH
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
      SUB_LOG(ISUB_LOG) = '/LDDISP_GM'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Loop over the finite-element nodes ---
!
      DO NFEN = 1,NFEN_GM
        U_GM(1,NFEN) = U_GM(2,NFEN)
        V_GM(1,NFEN) = V_GM(2,NFEN)
        W_GM(1,NFEN) = W_GM(2,NFEN)
      ENDDO
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of LDDISP_GM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE LD_GM( INDX )
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
!     INDX = 1
!     Load old time step values of volumetric stress and pore pressure.
!     INDX = 2
!     Load k values of volumetric stress and pore pressure.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 March 2017 (Yellowstone National 
!     Park is established as the world's first national park. in 1872).
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GLB_PAR
      USE GEOMECH
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
      SUB_LOG(ISUB_LOG) = '/LD_GM'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Loop over the finite elements ---
!
      DO N = 1,NFLD
        IF( IXP(N).EQ.0 ) CYCLE
        SIGV_GM(INDX,N) = SIGV_GM(INDX+1,N)
        P_GM(INDX,N) = P_GM(INDX+1,N)
      ENDDO
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of LD_GM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MATMUL( AX,BX,CX,IX,JX,KX )
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
!     Multiple the matrix AX (IX X JX) and BX (JX X KX), returning the
!     result in matrix CX (IX X KX).
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 November 2016 (Ensisheim meteorite).
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GEOMECH
      USE CONST
      USE GLB_PAR
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
      REAL*8 AX(IX,JX),BX(JX,KX),CX(IX,KX)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/MATMUL'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Initialize CX  ---
!
      DO K = 1,KX
        DO I = 1,IX
          CX(I,K) = 0.D+0
        ENDDO
      ENDDO
!
!---  Matrix multiply  ---
!
      DO I = 1,IX
        DO K = 1,KX
          DO J = 1,JX
            CX(I,K) = CX(I,K) + AX(I,J)*BX(J,K)
          ENDDO
        ENDDO
      ENDDO
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of MATMUL group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE MATTRP( AX,BX,IX,JX )
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
!     Return the transpose of matrix AX (IX x JX) in BX (JX x IX).
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 November 2016 (Ensisheim meteorite).
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GEOMECH
      USE CONST
      USE GLB_PAR
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
      REAL*8 AX(IX,JX),BX(JX,IX)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/MATTRP'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Matrix transpose  ---
!
      DO J = 1,JX
        DO I = 1,IX
          BX(J,I) = AX(I,J)
        ENDDO
      ENDDO
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of MATTRP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE PORSTY_GM
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
!     Update porosity.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 28 February 2017 (The erroneous word
!     "dord" is discovered in the Webster's New International 
!     Dictionary, Second Edition, prompting an investigation in 1939).
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE PORMED
      USE JACOB
      USE GRID
      USE GLB_PAR
      USE GEOMECH
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
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/PORSTY_GM'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Loop over all nodes  ---
!
      DO N = 1,NFLD
!
!---    Skip inactive nodes  ---
!
        IF( IXP(N).EQ.0 ) CYCLE
        N_DB = N
!
!---    Loop over increment indices  ---
!
        DO M = 2,ISVC+2
          PX = MAX( PG(M,N),PL(M,N),PN(M,N) ) + PATM
          CALL PORSTY( N,PX,PCMP(N),PORD(M,N),PORT(M,N) )
          PORD(M,N) = MAX( PORD(M,N),EPSL )
          PORT(M,N) = MAX( PORT(M,N),PORD(M,N) )
        ENDDO
      ENDDO
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of PORSTY_GM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE PRESS_GM( INDX )
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
!     Set k iterate value of pore pressure.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 21 February 2017 (Initial issue of 
!     the Cherokee Phoenix in 1828).
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GLB_PAR
      USE GEOMECH
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
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/PRESS_GM'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Loop over the finite elements ---
!
      DO N = 1,NFLD
        IF( IXP(N).EQ.0 ) CYCLE
        IZN = IZ(N)
        P_GM(INDX,N) = MAX( PG(2,N),PL(2,N),PN(2,N) ) + PATM
      ENDDO
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of PRESS_GM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDGMP
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
!     Read geomechanical properties card.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 20 June 2008.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PORMED
      USE GRID
      USE GEOMECH
      USE FILES
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
      CHARACTER*64 ADUM,RDUM,UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDGMP'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Write card information to ouput file  ---
!
      CARD = 'Geomechanical Properties Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Loop over the geomechanical property information lines  ---
!
      N = 0
      IJK = 0
   10 CONTINUE
        IF( N.GE.NROCK .OR. IJK.GT.0 ) GOTO 500
        ISTART = 1
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        VARB = 'Rock/Soil Name'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---    IJK, KIJ, or JKI indexing  ---
!
        IF( INDEX(ADUM(1:),'indexing').NE.0 ) THEN
          IF( INDEX(ROCK(1)(1:),'indexing').EQ.0 ) THEN
            INDX = 4
            CHMSG = 'Indexing Option Not Declared ' //
     &        'in Geomechanical Properties Card'
            CALL WRMSGS( INDX )
          ENDIF
          IF( INDEX(ADUM,'ijk').NE.0 ) THEN
            IJK = 1
          ELSEIF( INDEX(ADUM,'jki').NE.0 ) THEN
            IJK = 2
          ELSEIF( INDEX(ADUM,'kij').NE.0 ) THEN
            IJK = 3
          ELSE
            INDX = 4
            CHMSG = 'Unrecognized Indexing Option' // ADUM(1:NCH)
            CALL WRMSGS( INDX )
          ENDIF
          GOTO 220
        ENDIF
!
!---    Search known rock types for a matching type ---
!
        DO 100 M = 1, NROCK
          IF( ADUM .EQ. ROCK(M)) THEN
            IROCK = M
            GOTO 200
          ENDIF
  100   CONTINUE
!
!---    Search known scaling groups for a matching type ---
!
        IF( ISLC(19).EQ.1 ) THEN
          DO 110 M = 1,NSCALE
             IF( ADUM.EQ.SCALNM(M) ) THEN
                ISGRP = M
                IROCK = 1
                GOTO 200
             ENDIF
  110     CONTINUE
          INDX = 2
          CHMSG = 'Unrecognized Rock/Soil Type or Scaling Group: '
     &      // ADUM(1:NCH)
          CALL WRMSGS( INDX )
          GOTO 10
        ENDIF
        INDX = 2
        CHMSG = 'Unrecognized Rock/Soil Type: ' // ADUM(1:NCH)
        CALL WRMSGS( INDX )
        GOTO 10
  200   CONTINUE
!
!---    Loop over rock/soils within scaling group  ---
!
        IF( ISLC(19).EQ.1 .AND. ISGRP.NE.0 ) THEN
          DO 202 M = IROCK,NROCK
            IF( ISCALE(M).EQ.ISGRP ) THEN
              IROCK = M
              GOTO 204
            ENDIF
  202     CONTINUE
        ENDIF
  204   CONTINUE
!
!---    Write rock/soil name  ---
!
        WRITE (IWR,'(/,2A)') 'Rock/Soil Name: ',ROCK(IROCK)
        N = N + 1
  220   CONTINUE
!
!---    Young's modulus (Pa)  ---
!
        VARB = 'Young''s Modulus (Drained Bulk)'
        IDFLT = 1
        UNTS = 'pa'
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        INDX = 1
        LNDX = 9
        IF( IJK.GT.0 ) THEN
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PROP_GM,INDX,LNDX )
        ELSE
          IDFLT = 1
          CALL RDDPR(ISTART,ICOMMA,CHDUM,PROP_GM(1,IROCK))
          IDFLT = 1
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',
     &      UNTS(1:NCH),': ',PROP_GM(1,IROCK)
          INDX = 0
          CALL RDUNIT(UNTS,PROP_GM(1,IROCK),INDX)
           WRITE(IWR,'(A,1PE11.4,A)') ' (',PROP_GM(1,IROCK),', Pa)'
        ENDIF
!
!---    Poisson's ratio  ---
!
        VARB = 'Poisson''s Ratio (Drained Bulk)'
        IDFLT = 1
        UNTS = 'null'
        INDX = 2
        LNDX = 9
        IF( IJK.GT.0 ) THEN
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PROP_GM,INDX,LNDX )
          DO N = 1,NFLD
            IF( IXP(N).EQ.0 ) CYCLE
            IF( PROP_GM(2,N).GE.5.D-1 ) THEN
              INDX = 9
              CHMSG = 'Poisson''s Ratio >= 0.5 '
              RLMSG = PROP_GM(2,IROCK)
              CALL WRMSGS( INDX )
            ENDIF
          ENDDO
        ELSE
          IDFLT = 1
          CALL RDDPR(ISTART,ICOMMA,CHDUM,PROP_GM(2,IROCK))
          WRITE(IWR,'(2A,1PE11.4)') VARB(1:IVR),
     &      ': ',PROP_GM(2,IROCK)
          IF( PROP_GM(2,IROCK).GE.5.D-1 ) THEN
            INDX = 9
            CHMSG = 'Poisson''s Ratio >= 0.5 '
            RLMSG = PROP_GM(2,IROCK)
            CALL WRMSGS( INDX )
          ENDIF
        ENDIF
!
!---    Biot Coefficient  ---
!
        VARB = 'Biot Coefficient'
        IDFLT = 1
        UNTS = 'null'
        INDX = 3
        LNDX = 9
        IF( IJK.GT.0 ) THEN
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PROP_GM,INDX,LNDX )
        ELSE
          IDFLT = 1
          CALL RDDPR(ISTART,ICOMMA,CHDUM,PROP_GM(3,IROCK))
          WRITE(IWR,'(2A,1PE11.4,$)') VARB(1:IVR),
     &      ': ',PROP_GM(3,IROCK)
        ENDIF
!
!---    Thermal expansion coefficient (1/K)  ---
!
        VARB = 'Thermal Expansion Coefficient'
        IDFLT = 1
        UNTS = '1/k'
        IUNK = -1
        INDX = 4
        LNDX = 9
        IF( IJK.GT.0 ) THEN
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PROP_GM,INDX,LNDX )
        ELSE
          IDFLT = 1
          CALL RDDPR(ISTART,ICOMMA,CHDUM,PROP_GM(4,IROCK))
          IDFLT = 1
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          WRITE(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',
     &      UNTS(1:NCH),': ',PROP_GM(4,IROCK)
          INDX = 0
          CALL RDUNIT(UNTS,PROP_GM(4,IROCK),INDX)
           WRITE(IWR,'(A,1PE11.4,A)') ' (',PROP_GM(4,IROCK),', 1/K)'
        ENDIF
!
!---    Loop over remaining rock/soils within scaling group  ---
!
        IF( ISLC(19).EQ.1 .AND. IROCK.LT.NROCK ) THEN
          DO 490 M = IROCK+1,NROCK
            IF( ISCALE(M).EQ.ISGRP ) THEN
              N = N+1
              DO 480 L = 1,5
                PROP_GM(L,M) = PROP_GM(L,IROCK)
  480         CONTINUE
            ENDIF
  490     CONTINUE
        ENDIF
!
!---    Read next rock/soil type or scaling group  ---
!
        IF( N .LT. NROCK ) WRITE(IWR,'(/)')
        GOTO 10
  500 CONTINUE
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDGMP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDGMBC
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
!     Read geomechanical boundary conditions card.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 28 November 2016.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GRID
      USE GEOMECH
      USE FILES
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
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,BDUM,CDUM,FDUM
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
      INTEGER ITYP(3)
      REAL*8 VAR(LBTM_GM,LBCV_GM)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDGMBC'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Write card information to ouput file  ---
!
      CARD = 'Geomechanical Boundary Conditions Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
      NBC_GM = 0
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Geomechanical Boundary Conditions'
      CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
      DO 400 NB = 1,NLIN
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        ISTART = 1
        IF( NB.NE.1 ) WRITE(IWR, '(/)')
!
!---    Read boundary condition range of FE nodes  ---
!
        WRITE(IWR,'(/,A)') 'Geomechanical Boundary Condition'
        IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
          NCH = INDEX(FDUM,'  ')-1
          OPEN(UNIT=26,FILE=FDUM(1:NCH),STATUS='OLD',FORM='FORMATTED')
          WRITE(IWR,'(/,2A)') 'Boundary Condition Domain File: ',
     &      FDUM(1:NCH)
          I1X = 1
          I2X = 1
          J1X = 1
          J2X = 1
          K1X = 1
          K2X = 0
    5     CONTINUE
          READ(26,*,END=10) IX,JX,KX
          K2X = K2X+1
          GOTO 5
   10     CONTINUE
          REWIND(26)
        ENDIF
!
!---    Read x-direction and/or y-direction and/or z-direction boundary
!       condition types  ---
!
        DO NX = 1,3
          ITYP(NX) = 0
        ENDDO
        ITRACX = 0
        IDISPX = 0
        IFORCX = 0
        IBCDX = 0
        DO NX = 1,4
          CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            IF( NX.EQ.1 ) THEN
              INDX = 4
              CHMSG = 'No Geomechanical Boundary Condition Type' // 
     &          ' Specified'
              CALL WRMSGS( INDX )
            ENDIF
            EXIT
          ELSE
            VARB = 'Geomechanical Boundary Condition Type'
            ISTX = ISTART
            ICMX = ICOMMA
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
            IF( INDEX(BDUM(1:),'x-traction').NE.0 .OR. 
     &        INDEX(BDUM(1:),'x-stress').NE.0 ) THEN
              IF( IDISPX.EQ.1 ) THEN
                INDX = 4
                CHMSG = 'Mixed Displacement and Traction Type' // 
     &            'Geomechanical Boundary Conditions'
                CALL WRMSGS( INDX )
              ENDIF
              IF( IFORCX.EQ.1 ) THEN
                INDX = 4
                CHMSG = 'Mixed Force and Traction Type' // 
     &            'Geomechanical Boundary Conditions'
                CALL WRMSGS( INDX )
              ENDIF
              IF( INDEX(BDUM(1:),'gradient').NE.0 ) THEN
                ITYP(1) = 2
                WRITE(IWR,'(2X,A)') 'X-Direction Traction Boundary ' // 
     &            'w/ Z-Gradient'
              ELSE
                ITYP(1) = 1
                WRITE(IWR,'(A)') 'X-Direction Traction Boundary '
              ENDIF
              ITRACX = 1
            ELSEIF( INDEX(BDUM(1:),'x-displacement').NE.0 .OR. 
     &        INDEX(BDUM(1:),'x-strain').NE.0 ) THEN
              IF( ITRACX.EQ.1 ) THEN
                INDX = 4
                CHMSG = 'Mixed Displacement and Traction Type' // 
     &            'Geomechanical Boundary Conditions'
                CALL WRMSGS( INDX )
              ENDIF
              IF( IFORCX.EQ.1 ) THEN
                INDX = 4
                CHMSG = 'Mixed Displacement and Force Type' // 
     &            'Geomechanical Boundary Conditions'
                CALL WRMSGS( INDX )
              ENDIF
              IF( INDEX(BDUM(1:),'gradient').NE.0 ) THEN
                ITYP(1) = 4
                WRITE(IWR,'(2X,A)') 'X-Direction Displacement Boundary ' 
     &            // 'w/ Z-Gradient'
              ELSE
                ITYP(1) = 3
                WRITE(IWR,'(2X,A)') 'X-Direction Displacement Boundary '
              ENDIF
              IDISPX = 1
            ELSEIF( INDEX(BDUM(1:),'x-force').NE.0 .OR. 
     &        INDEX(BDUM(1:),'x-load').NE.0 ) THEN
              IF( ITRACX.EQ.1 ) THEN
                INDX = 4
                CHMSG = 'Mixed Traction and Force Type' // 
     &            'Geomechanical Boundary Conditions'
                CALL WRMSGS( INDX )
              ENDIF
              IF( IDISPX.EQ.1 ) THEN
                INDX = 4
                CHMSG = 'Mixed Displacement and Force Type' // 
     &            'Geomechanical Boundary Conditions'
                CALL WRMSGS( INDX )
              ENDIF
              IF( INDEX(BDUM(1:),'gradient').NE.0 ) THEN
                ITYP(1) = 6
                WRITE(IWR,'(2X,A)') 'X-Direction Force Boundary ' 
     &            // 'w/ Z-Gradient'
              ELSE
                ITYP(1) = 5
                WRITE(IWR,'(2X,A)') 'X-Direction Force Boundary '
              ENDIF
              IFORCX = 1
            ELSEIF( INDEX(BDUM(1:),'y-traction').NE.0 .OR. 
     &        INDEX(BDUM(1:),'y-stress').NE.0 ) THEN
              IF( IDISPX.EQ.1 ) THEN
                INDX = 4
                CHMSG = 'Mixed Displacement and Traction Type' // 
     &            'Geomechanical Boundary Conditions'
                CALL WRMSGS( INDX )
              ENDIF
              IF( IFORCX.EQ.1 ) THEN
                INDX = 4
                CHMSG = 'Mixed Force and Traction Type' // 
     &            'Geomechanical Boundary Conditions'
                CALL WRMSGS( INDX )
              ENDIF
              IF( INDEX(BDUM(1:),'gradient').NE.0 ) THEN
                ITYP(2) = 2
                WRITE(IWR,'(2X,A)') 'Y-Direction Traction Boundary ' // 
     &            'w/ Z-Gradient'
              ELSE
                ITYP(2) = 1
                WRITE(IWR,'(2X,A)') 'Y-Direction Traction Boundary '
              ENDIF
              ITRACX = 1
            ELSEIF( INDEX(BDUM(1:),'y-displacement').NE.0 .OR. 
     &        INDEX(BDUM(1:),'y-strain').NE.0 ) THEN
              IF( ITRACX.EQ.1 ) THEN
                INDX = 4
                CHMSG = 'Mixed Displacement and Traction Type' // 
     &            'Geomechanical Boundary Conditions'
                CALL WRMSGS( INDX )
              ENDIF
              IF( IFORCX.EQ.1 ) THEN
                INDX = 4
                CHMSG = 'Mixed Displacement and Force Type' // 
     &            'Geomechanical Boundary Conditions'
                CALL WRMSGS( INDX )
              ENDIF
              IF( INDEX(BDUM(1:),'gradient').NE.0 ) THEN
                ITYP(2) = 4
                WRITE(IWR,'(2X,A)') 'Y-Direction Displacement Boundary '
     &            // 'w/ Z-Gradient'
              ELSE
                ITYP(2) = 3
                WRITE(IWR,'(2X,A)') 'Y-Direction Displacement Boundary '
              ENDIF
              IDISPX = 1
            ELSEIF( INDEX(BDUM(1:),'y-force').NE.0 .OR. 
     &        INDEX(BDUM(1:),'y-load').NE.0 ) THEN
              IF( ITRACX.EQ.1 ) THEN
                INDX = 4
                CHMSG = 'Mixed Traction and Force Type' // 
     &            'Geomechanical Boundary Conditions'
                CALL WRMSGS( INDX )
              ENDIF
              IF( IDISPX.EQ.1 ) THEN
                INDX = 4
                CHMSG = 'Mixed Displacement and Force Type' // 
     &            'Geomechanical Boundary Conditions'
                CALL WRMSGS( INDX )
              ENDIF
              IF( INDEX(BDUM(1:),'gradient').NE.0 ) THEN
                ITYP(2) = 6
                WRITE(IWR,'(2X,A)') 'Y-Direction Force Boundary ' 
     &            // 'w/ Z-Gradient'
              ELSE
                ITYP(2) = 5
                WRITE(IWR,'(2X,A)') 'Y-Direction Force Boundary '
              ENDIF
              IFORCX = 1
            ELSEIF( INDEX(BDUM(1:),'z-traction').NE.0 .OR. 
     &        INDEX(BDUM(1:),'z-stress').NE.0 ) THEN
              IF( IDISPX.EQ.1 ) THEN
                INDX = 4
                CHMSG = 'Mixed Displacement and Traction Type' // 
     &            'Geomechanical Boundary Conditions'
                CALL WRMSGS( INDX )
              ENDIF
              IF( IFORCX.EQ.1 ) THEN
                INDX = 4
                CHMSG = 'Mixed Force and Traction Type' // 
     &            'Geomechanical Boundary Conditions'
                CALL WRMSGS( INDX )
              ENDIF
              IF( INDEX(BDUM(1:),'gradient').NE.0 ) THEN
                ITYP(3) = 2
                WRITE(IWR,'(2X,A)') 'Z-Direction Traction Boundary ' // 
     &            'w/ Z-Gradient'
              ELSE
                ITYP(3) = 1
                WRITE(IWR,'(A)') 'Z-Direction Traction Boundary '
              ENDIF
              ITRACX = 1
            ELSEIF( INDEX(BDUM(1:),'z-displacement').NE.0 .OR. 
     &        INDEX(BDUM(1:),'z-strain').NE.0 ) THEN
              IF( ITRACX.EQ.1 ) THEN
                INDX = 4
                CHMSG = 'Mixed Displacement and Traction Type' // 
     &            'Geomechanical Boundary Conditions'
                CALL WRMSGS( INDX )
              ENDIF
              IF( IFORCX.EQ.1 ) THEN
                INDX = 4
                CHMSG = 'Mixed Displacement and Force Type' // 
     &            'Geomechanical Boundary Conditions'
                CALL WRMSGS( INDX )
              ENDIF
              IF( INDEX(BDUM(1:),'gradient').NE.0 ) THEN
                ITYP(3) = 4
                WRITE(IWR,'(2X,A)') 'Z-Direction Displacement Boundary '
     &            // 'w/ Z-Gradient'
              ELSE
                ITYP(3) = 3
                WRITE(IWR,'(2X,A)') 'Z-Direction Displacement Boundary '
              ENDIF
              IDISPX = 1
            ELSEIF( INDEX(BDUM(1:),'z-force').NE.0 .OR. 
     &        INDEX(BDUM(1:),'z-load').NE.0 ) THEN
              IF( ITRACX.EQ.1 ) THEN
                INDX = 4
                CHMSG = 'Mixed Traction and Force Type' // 
     &            'Geomechanical Boundary Conditions'
                CALL WRMSGS( INDX )
              ENDIF
              IF( IDISPX.EQ.1 ) THEN
                INDX = 4
                CHMSG = 'Mixed Displacement and Force Type' // 
     &            'Geomechanical Boundary Conditions'
                CALL WRMSGS( INDX )
              ENDIF
              IF( INDEX(BDUM(1:),'gradient').NE.0 ) THEN
                ITYP(3) = 6
                WRITE(IWR,'(2X,A)') 'Z-Direction Force Boundary ' 
     &            // 'w/ Z-Gradient'
              ELSE
                ITYP(3) = 5
                WRITE(IWR,'(2X,A)') 'Z-Direction Force Boundary '
              ENDIF
              IFORCX = 1
            ELSEIF( ITRACX.EQ.1 .AND. 
     &        INDEX(BDUM(1:),'bottom').NE.0 ) THEN
              IBCDX = -3
              WRITE(IWR,'(A)') 'Bottom Surface'
            ELSEIF( ITRACX.EQ.1 .AND. 
     &        INDEX(BDUM(1:),'south').NE.0 ) THEN
              IBCDX = -2
              WRITE(IWR,'(A)') 'South Surface'
            ELSEIF( ITRACX.EQ.1 .AND. 
     &        INDEX(BDUM(1:),'west').NE.0 ) THEN
              IBCDX = -1
              WRITE(IWR,'(A)') 'West Surface'
            ELSEIF( ITRACX.EQ.1 .AND. 
     &        INDEX(BDUM(1:),'east').NE.0 ) THEN
              IBCDX = 1
              WRITE(IWR,'(A)') 'East Surface'
            ELSEIF( ITRACX.EQ.1 .AND. 
     &        INDEX(BDUM(1:),'north').NE.0 ) THEN
              IBCDX = 2
              WRITE(IWR,'(A)') 'North Surface'
            ELSEIF( ITRACX.EQ.1 .AND. 
     &        INDEX(BDUM(1:),'top').NE.0 ) THEN
              IBCDX = 3
              WRITE(IWR,'(A)') 'Top Surface'
            ELSE
              INDX = 4
              CHMSG = 'Unrecognized Geomechanical Boundary ' // 
     &          'Condition Type or Surface: ' // BDUM(1:NCH)
              CALL WRMSGS( INDX )
            ENDIF
          ENDIF
        ENDDO
!
!---  Read and write boundary domain indices  ---
!
        ISTART = 1
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        IF( INDEX(ADUM(1:),'file').EQ.0 ) THEN
          VARB = 'Geomechanical Boundary Condition FE Nodal Domain: '
          CALL RDINT(ISTART,ICOMMA,CHDUM,I1X)
          CALL RDINT(ISTART,ICOMMA,CHDUM,I2X)
          CALL RDINT(ISTART,ICOMMA,CHDUM,J1X)
          CALL RDINT(ISTART,ICOMMA,CHDUM,J2X)
          CALL RDINT(ISTART,ICOMMA,CHDUM,K1X)
          CALL RDINT(ISTART,ICOMMA,CHDUM,K2X)
          WRITE(IWR,'(2X,A)') VARB(1:IVR)
          WRITE(IWR, '(2X,A,I6,A,I6)') 'I = ',I1X,' to ',I2X
          WRITE(IWR, '(2X,A,I6,A,I6)') 'J = ',J1X,' to ',J2X
          WRITE(IWR, '(2X,A,I6,A,I6)') 'K = ',K1X,' to ',K2X
!
!---      Check geomechanical boundary domain  ---
!
          IF( I1X.GT.I2X .OR. J1X.GT.J2X .OR. K1X.GT.K2X ) THEN
            INDX = 4
            CHMSG = 'Nonascending Geomechanical Boundary Condition '
     &        // 'FE Nodal Domain Indices'
            CALL WRMSGS( INDX )
          ENDIF
!
!---      Check traction type geomechanical boundary domain,
!         traction boundary conditions are applied to FE
!         element surfaces  ---
!
          IF( ITRACX.EQ.1 ) THEN
            IF( I1X.LT.1 .OR. I2X.GT.IFLD. OR. J1X.LT.1 .OR.
     &        J2X.GT.JFLD .OR. K1X.LT.1 .OR. K2X.GT.KFLD ) THEN
              INDX = 4
              CHMSG = 'Illegal Traction Geomechanical Boundary ' // 
     &          'Condition FE Nodal Domain'
              CALL WRMSGS( INDX )
            ENDIF
          ENDIF
!
!---      Check displacement type geomechanical boundary domain,
!         displacement boundary conditions are applied to FE nodes  ---
!
          IF( IDISPX.EQ.1 ) THEN
            IF( I1X.LT.1 .OR. I2X.GT.IFLD+1. OR. J1X.LT.1 .OR.
     &      J2X.GT.JFLD+1 .OR. K1X.LT.1 .OR. K2X.GT.KFLD+1 ) THEN
              INDX = 4
              CHMSG = 'Illegal Displacment Geomechanical Boundary ' // 
     &          'Condition FE Nodal Domain'
              CALL WRMSGS( INDX )
            ENDIF
          ENDIF
!
!---      Check force type geomechanical boundary domain,
!         force boundary conditions are applied to FE nodes  ---
!
          IF( IFORCX.EQ.1 ) THEN
            IF( I1X.LT.1 .OR. I2X.GT.IFLD+1. OR. J1X.LT.1 .OR.
     &      J2X.GT.JFLD+1 .OR. K1X.LT.1 .OR. K2X.GT.KFLD+1 ) THEN
              INDX = 4
              CHMSG = 'Illegal Force Geomechanical Boundary ' // 
     &          'Condition FE Nodal Domain'
              CALL WRMSGS( INDX )
            ENDIF
          ENDIF
        ENDIF
!
!---    Read number of geomechanical boundary times  ---
!
        IBCRX = 0
        VARB = 'Number of Geomechanical Boundary Condition Times'
        CALL RDINT(ISTART,ICOMMA,CHDUM,IBCMX)
        IF( IBCMX.LE.-3 ) THEN
          IBCCX = 1
          IBCMX = -IBCMX
          WRITE(IWR,'(2X,A)') 'Cyclic Geomechanical Boundary Conditions'
        ELSEIF( IBCMX.GE.1 ) THEN
          IBCCX = 0
          WRITE(IWR,'(2X,A)') 'Noncyclic Geomechanical Boundary ' // 
     &      'Conditions'
        ELSEIF( IBCMX.EQ.0 ) THEN
          INDX = 4
          CHMSG = 'No Geomechanical Boundary Condition Times'
          CALL WRMSGS( INDX )
        ELSE
          INDX = 4
          CHMSG = 'Number of Cyclic Geomechanical Boundary '
     &      // 'Conditions Times < 3'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IBCMX.GT.LBTM_GM ) THEN
          INDX = 5
          CHMSG = 'Number of Geomechanical Boundary Condition Times'
     &      // '> LBTM_GM'
          CALL WRMSGS( INDX )
        ENDIF
        BCTMO = -SMALL
        WRITE(IWR,'(A)') 'Geomechanical Boundary Condition '
     &    // 'Times and Variables:'
        DO 100 NTM = 1,IBCMX
          DO 40 M = 1,LBCV_GM
            VAR(NTM,M) = 0.D+0
   40     CONTINUE
!
!---      Read, write, and convert geomechanical boundary condition
!         time, variables, and units  ---
!
          CALL RDINPL( CHDUM )
          CALL LCASE( CHDUM )
          ISTART = 1
          VARB = 'Time'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,CDUM)
          IF( INDEX(CDUM(1:),'reference').NE.0 ) THEN
            IBCRX = 1
          ELSE
            ISTART = 1
            VAR(NTM,1) = 0.D+0
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,1))
            UNTS = 's'
            IDFLT = 1
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,4A,1PE11.4)') VARB(1:IVR),', ',UNTS(1:NCH),
     &        ': ',VAR(NTM,1)
            INDX = 0
            IUNS = 1
            CALL RDUNIT(UNTS,VAR(NTM,1),INDX)
          ENDIF
!
!---      X-direction stress  ---
!
          IF( ITYP(1).EQ.1 ) THEN
            VARB = 'X-Direction Stress'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            VAR(NTM,2) = 0.D+0
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            UNTS = 'pa'
            IDFLT = 1
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
            INDX = 0
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', Pa)'
!
!---      X-direction stress w/ z-gradient  ---
!
          ELSEIF( ITYP(1).EQ.2 ) THEN
            VARB = 'Reference X-Direction Stress'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            VAR(NTM,2) = 0.D+0
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            UNTS = 'pa'
            IDFLT = 1
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
            INDX = 0
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', Pa)'
            VARB = 'Reference Stress Z Point'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,3))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            WRITE(IWR,'(2A,1PE11.4)') UNTS(1:NCH),': ',VAR(NTM,3)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,3),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,3),', m)'
            VARB = 'Stress Gradient'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            WRITE(IWR,'(2A,1PE11.4)') UNTS(1:NCH),': ',VAR(NTM,4)
            INDX = 0
            IUNM = -2
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', Pa/m)'
!
!---      X-direction displacement  ---
!
          ELSEIF( ITYP(1).EQ.3 ) THEN
            VARB = 'X-Direction Displacement'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            VAR(NTM,2) = 0.D+0
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            UNTS = 'm'
            IDFLT = 1
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', m)'
!
!---      X-direction displacement w/ z-gradient  ---
!
          ELSEIF( ITYP(1).EQ.4 ) THEN
            VARB = 'X-Direction Displacement w/ Z-Gradient'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            VAR(NTM,2) = 0.D+0
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            UNTS = 'm'
            IDFLT = 1
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', m)'
            VARB = 'Reference Displacement Z Point'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,3))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            WRITE(IWR,'(2A,1PE11.4)') UNTS(1:NCH),': ',VAR(NTM,3)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,3),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,3),', m)'
            VARB = 'Displacement Gradient'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            WRITE(IWR,'(2A,1PE11.4)') UNTS(1:NCH),': ',VAR(NTM,4)
            INDX = 0
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', m/m)'
!
!---      X-direction force  ---
!
          ELSEIF( ITYP(1).EQ.5 ) THEN
            VARB = 'X-Direction Force'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            VAR(NTM,2) = 0.D+0
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            UNTS = 'n'
            IDFLT = 1
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
            INDX = 0
            IUNKG = 1
            IUNM = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', N)'
!
!---      X-direction force w/ z-gradient  ---
!
          ELSEIF( ITYP(1).EQ.6 ) THEN
            VARB = 'X-Direction Force w/ Z-Gradient'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            VAR(NTM,2) = 0.D+0
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            UNTS = 'n'
            IDFLT = 1
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
            INDX = 0
            IUNKG = 1
            IUNM = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', N)'
            VARB = 'Reference Displacement Z Point'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,3))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            WRITE(IWR,'(2A,1PE11.4)') UNTS(1:NCH),': ',VAR(NTM,3)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,3),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,3),', m)'
            VARB = 'Displacement Gradient'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            WRITE(IWR,'(2A,1PE11.4)') UNTS(1:NCH),': ',VAR(NTM,4)
            INDX = 0
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', m/m)'
          ENDIF
!
!---      Y-direction stress  ---
!
          IF( ITYP(2).EQ.1 ) THEN
            VARB = 'Y-Direction Stress'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            VAR(NTM,5) = 0.D+0
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,5))
            UNTS = 'pa'
            IDFLT = 1
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,5)
            INDX = 0
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,5),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,5),', Pa)'
!
!---      Y-direction stress w/ z-gradient  ---
!
          ELSEIF( ITYP(2).EQ.2 ) THEN
            VARB = 'Reference Y-Direction Stress'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            VAR(NTM,5) = 0.D+0
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,5))
            UNTS = 'pa'
            IDFLT = 1
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,5)
            INDX = 0
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,5),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,5),', Pa)'
            VARB = 'Reference Stress Z Point'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,6))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            WRITE(IWR,'(2A,1PE11.4)') UNTS(1:NCH),': ',VAR(NTM,6)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,6),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,6),', m)'
            VARB = 'Stress Gradient'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,7))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            WRITE(IWR,'(2A,1PE11.4)') UNTS(1:NCH),': ',VAR(NTM,7)
            INDX = 0
            IUNM = -2
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,7),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,7),', Pa/m)'
!
!---      Y-direction displacement  ---
!
          ELSEIF( ITYP(2).EQ.3 ) THEN
            VARB = 'Y-Direction Displacement'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            VAR(NTM,5) = 0.D+0
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,5))
            UNTS = 'm'
            IDFLT = 1
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,5)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,5),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,5),', m)'
!
!---      Y-direction displacement w/ z-gradient  ---
!
          ELSEIF( ITYP(2).EQ.4 ) THEN
            VARB = 'Y-Direction Displacement w/ Z-Gradient'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            VAR(NTM,5) = 0.D+0
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,5))
            UNTS = 'm'
            IDFLT = 1
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,5)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,5),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,5),', m)'
            VARB = 'Reference Displacement Z Point'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,6))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            WRITE(IWR,'(2A,1PE11.4)') UNTS(1:NCH),': ',VAR(NTM,6)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,6),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,6),', m)'
            VARB = 'Displacement Gradient'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,7))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            WRITE(IWR,'(2A,1PE11.4)') UNTS(1:NCH),': ',VAR(NTM,7)
            INDX = 0
            CALL RDUNIT(UNTS,VAR(NTM,7),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,7),', m/m)'
!
!---      Y-direction force  ---
!
          ELSEIF( ITYP(2).EQ.5 ) THEN
            VARB = 'Y-Direction Force'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            VAR(NTM,5) = 0.D+0
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,5))
            UNTS = 'm'
            IDFLT = 1
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,5)
            INDX = 0
            IUNKG = 1
            IUNM = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,5),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,5),', N)'
!
!---      Y-direction force w/ z-gradient  ---
!
          ELSEIF( ITYP(2).EQ.6 ) THEN
            VARB = 'Y-Direction Force w/ Z-Gradient'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            VAR(NTM,5) = 0.D+0
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,5))
            UNTS = 'm'
            IDFLT = 1
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,5)
            INDX = 0
            IUNKG = 1
            IUNM = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,5),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,5),', N)'
            VARB = 'Reference Displacement Z Point'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,6))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            WRITE(IWR,'(2A,1PE11.4)') UNTS(1:NCH),': ',VAR(NTM,6)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,6),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,6),', m)'
            VARB = 'Displacement Gradient'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,7))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            WRITE(IWR,'(2A,1PE11.4)') UNTS(1:NCH),': ',VAR(NTM,7)
            INDX = 0
            CALL RDUNIT(UNTS,VAR(NTM,7),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,7),', m/m)'
          ENDIF
!
!---      Z-direction stress  ---
!
          IF( ITYP(3).EQ.1 ) THEN
            VARB = 'Z-Direction Stress'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            VAR(NTM,8) = 0.D+0
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,8))
            UNTS = 'pa'
            IDFLT = 1
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,8)
            INDX = 0
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,8),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,8),', Pa)'
!
!---      Z-direction stress w/ z-gradient  ---
!
          ELSEIF( ITYP(3).EQ.2 ) THEN
            VARB = 'Reference Z-Direction Stress'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            VAR(NTM,8) = 0.D+0
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,8))
            UNTS = 'pa'
            IDFLT = 1
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,8)
            INDX = 0
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,8),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,8),', Pa)'
            VARB = 'Reference Stress Z Point'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,9))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            WRITE(IWR,'(2A,1PE11.4)') UNTS(1:NCH),': ',VAR(NTM,9)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,9),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,9),', m)'
            VARB = 'Stress Gradient'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,10))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            WRITE(IWR,'(2A,1PE11.4)') UNTS(1:NCH),': ',VAR(NTM,10)
            INDX = 0
            IUNM = -2
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,10),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,10),', Pa/m)'
!
!---      Z-direction displacement  ---
!
          ELSEIF( ITYP(3).EQ.3 ) THEN
            VARB = 'Z-Direction Displacement'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            VAR(NTM,8) = 0.D+0
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,8))
            UNTS = 'm'
            IDFLT = 1
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,8)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,8),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,8),', m)'
!
!---      Z-direction displacement w/ z-gradient  ---
!
          ELSEIF( ITYP(3).EQ.4 ) THEN
            VARB = 'Z-Direction Displacement w/ Z-Gradient'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            VAR(NTM,8) = 0.D+0
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,8))
            UNTS = 'm'
            IDFLT = 1
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,8)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,8),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,8),', m)'
            VARB = 'Reference Displacement Z Point'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,9))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            WRITE(IWR,'(2A,1PE11.4)') UNTS(1:NCH),': ',VAR(NTM,9)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,9),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,9),', m)'
            VARB = 'Displacement Gradient'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,10))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            WRITE(IWR,'(2A,1PE11.4)') UNTS(1:NCH),': ',VAR(NTM,10)
            INDX = 0
            CALL RDUNIT(UNTS,VAR(NTM,10),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,10),', m/m)'
!
!---      Z-direction force  ---
!
          ELSEIF( ITYP(3).EQ.5 ) THEN
            VARB = 'Z-Direction Force'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            VAR(NTM,8) = 0.D+0
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,8))
            UNTS = 'm'
            IDFLT = 1
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,8)
            INDX = 0
            IUNKG = 1
            IUNM = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,8),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,8),', N)'
!
!---      Z-direction force w/ z-gradient  ---
!
          ELSEIF( ITYP(3).EQ.6 ) THEN
            VARB = 'Z-Direction Force w/ Z-Gradient'
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            VAR(NTM,8) = 0.D+0
            IDFLT = 1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,8))
            UNTS = 'm'
            IDFLT = 1
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,8)
            INDX = 0
            IUNKG = 1
            IUNM = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,8),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,8),', N)'
            VARB = 'Reference Displacement Z Point'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,9))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            WRITE(IWR,'(2A,1PE11.4)') UNTS(1:NCH),': ',VAR(NTM,9)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,9),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,9),', m)'
            VARB = 'Displacement Gradient'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,10))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
            WRITE(IWR,'(2A,1PE11.4)') UNTS(1:NCH),': ',VAR(NTM,10)
            INDX = 0
            CALL RDUNIT(UNTS,VAR(NTM,10),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,10),', m/m)'
          ENDIF
!
!---      Check for nonascending geomechanical boundary
!         condition times  ---
!
          IF( VAR(NTM,1).LT.BCTMO ) THEN
            INDX = 4
            CHMSG = 'Geomechanical Boundary Condition Time Sequencing'
            CALL WRMSGS( INDX )
          ENDIF
          BCTMO = VAR(NTM,1)
  100   CONTINUE
!
!---    Assign values to geomechanical boundary variables  ---
!
        DO 106 NTM = 1,IBCMX
          DO 102 M = 1,LBCV_GM
            BC_GM(M,NTM,NB) = VAR(NTM,M)
  102     CONTINUE
  106   CONTINUE
!
!---    Displacement or force type geomechanical boundary, loop over 
!       FE node domain, associating the FE node with an FE element and 
!       hexahedron index  ---
!
        IF( IDISPX.EQ.1 .OR. IFORCX.EQ.1 ) THEN
          DO 320 K = K1X,K2X
          DO 310 J = J1X,J2X
          DO 300 I = I1X,I2X
            IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
              READ(26,*,END=320) IX,JX,KX
            ELSE
              IX = I
              JX = J
              KX = K
            ENDIF
!
!---        IX < IFLD+1 (1, 3, 5, 7)  ---
!
            IF( IX.LT.IFLD+1 ) THEN
              IF( JX.LT.JFLD+1 ) THEN
                IF( KX.LT.KFLD+1 ) THEN
                  N = ND(IX,JX,KX)
                  IBCDX = 1
                ELSE
                  N = ND(IX,JX,KFLD)
                  IBCDX = 5
                ENDIF
              ELSE
                IF( KX.LT.KFLD+1 ) THEN
                  N = ND(IX,JFLD,KX)
                  IBCDX = 3
                ELSE
                  N = ND(IX,JFLD,KFLD)
                  IBCDX = 7
                ENDIF
              ENDIF
!
!---        IX = IFLD+1 (2, 4, 6, 8)  ---
!
            ELSE
              IF( JX.LT.JFLD+1 ) THEN
                IF( KX.LT.KFLD+1 ) THEN
                  N = ND(IFLD,JX,KX)
                  IBCDX = 2
                ELSE
                  N = ND(IFLD,JX,KFLD)
                  IBCDX = 6
                ENDIF
              ELSE
                IF( KX.LT.KFLD+1 ) THEN
                  N = ND(IFLD,JFLD,KX)
                  IBCDX = 4
                ELSE
                  N = ND(IFLD,JFLD,KFLD)
                  IBCDX = 8
                ENDIF
              ENDIF
            ENDIF
!
!---        Check for geomechanical boundary values applied
!           to interior FE nodes  ---
!
            IERR = 1
            NBC_GM = NBC_GM + 1
            LOOP: DO K3X = -1,0
              IF( KX+K3X.LT.1 .OR. KX+K3X.GT.KFLD ) THEN
                IERR = 0
                EXIT LOOP
              ENDIF
              DO J3X = -1,0
                IF( JX+J3X.LT.1 .OR. JX+J3X.GT.JFLD ) THEN
                  IERR = 0
                  EXIT LOOP
                ENDIF
                DO I3X = -1,0
                  IF( IX+I3X.LT.1 .OR. IX+I3X.GT.IFLD ) THEN
                    IERR = 0
                    EXIT LOOP
                  ENDIF
                  NX = ND(IX+I3X,JX+J3X,KX+K3X)
                  IF( IXP(NX).EQ.0 ) THEN
                    IERR = 0
                    EXIT LOOP
                  ENDIF
                  IF( K3X.EQ.-1 .AND. INBS(6,NX).NE.0 ) THEN
                    IERR = 0
                    EXIT LOOP
                  ENDIF
                  IF( K3X.EQ.0 .AND. INBS(1,NX).NE.0 ) THEN
                    IERR = 0
                    EXIT LOOP
                  ENDIF
                  IF( J3X.EQ.-1 .AND. INBS(5,NX).NE.0 ) THEN
                    IERR = 0
                    EXIT LOOP
                  ENDIF
                  IF( J3X.EQ.0 .AND. INBS(2,NX).NE.0 ) THEN
                    IERR = 0
                    EXIT LOOP
                  ENDIF
                  IF( I3X.EQ.-1 .AND. INBS(4,NX).NE.0 ) THEN
                    IERR = 0
                    EXIT LOOP
                  ENDIF
                  IF( I3X.EQ.0 .AND. INBS(3,NX).NE.0 ) THEN
                    IERR = 0
                    EXIT LOOP
                  ENDIF
                ENDDO
              ENDDO
            ENDDO LOOP
            IF( IERR.EQ.1 ) THEN
               WRITE(ISC,'(3(A,I9))') 'I FE = ',IX,' J FE = ',JX,
     &          ' K FE = ',KX
              WRITE(IWR,'(3(A,I9))') 'I FE = ',IX,' J FE = ',JX,
     &          ' K FE = ',KX
              INDX = 7
              IMSG = NBC_GM
              CHMSG = 'Geomechanical Boundary Cond. Applied to ' // 
     &          ' an Interior Surface: Boundary Number'
              CALL WRMSGS( INDX )
            ENDIF
            IF( NBC_GM.GT.LBC_GM ) THEN
              INDX = 5
              CHMSG = 'Number of Geomechanical Boundary Condition'
     &          //' Surfaces > Parameter LBC_GM'
              CALL WRMSGS( INDX )
            ENDIF
            IBCN_GM(NBC_GM) = N
            IBCR_GM(NBC_GM) = IBCRX
            IBCM_GM(NBC_GM) = IBCMX
            IBCIN_GM(NBC_GM) = NB
            IBCD_GM(NBC_GM) = IBCDX
            IBCT_GM(1,NBC_GM) = ITYP(1)
            IBCT_GM(2,NBC_GM) = ITYP(2)
            IBCT_GM(3,NBC_GM) = ITYP(3)
  300     CONTINUE
  310     CONTINUE
  320     CONTINUE
!
!---    Traction type geomechanical boundary, loop over FE element
!       domain  ---
!
        ELSEIF( ITRACX.EQ.1 ) THEN
          DO 350 K = K1X,K2X
          DO 340 J = J1X,J2X
          DO 330 I = I1X,I2X
            IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
              READ(26,*,END=350) IX,JX,KX
            ELSE
              IX = I
              JX = J
              KX = K
            ENDIF
!
!---        Node number  ---
!
            N = ND(IX,JX,KX)
!
!---        Check for geomechanical boundary applied to 
!           interior surfaces  ---
!
            IERR = 0
            IF( IBCDX.EQ.-3 .AND. KX.NE.1) THEN
              IF( IXP(N-IJFLD).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
                IERR = 1
                WRITE(ISC,'(A)') 'Bottom Boundary'
                WRITE(IWR,'(A)') 'Bottom Boundary'
              ENDIF
            ELSEIF( IBCDX.EQ.-2 .AND. JX.NE.1) THEN
              IF( IXP(N-IFLD).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
                IERR = 1
                WRITE(ISC,'(A)') 'South Boundary'
                WRITE(IWR,'(A)') 'South Boundary'
              ENDIF
            ELSEIF( IBCDX.EQ.-1 .AND. IX.NE.1) THEN
              IF( IXP(N-1).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
                IERR = 1
                WRITE(ISC,'(A)') 'West Boundary'
                WRITE(IWR,'(A)') 'West Boundary'
              ENDIF
            ELSEIF( IBCDX.EQ.1 .AND. IX.NE.IFLD) THEN
              IF( IXP(N+1).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
                IERR = 1
                WRITE(ISC,'(A)') 'East Boundary'
                WRITE(IWR,'(A)') 'East Boundary'
              ENDIF
            ELSEIF( IBCDX.EQ.2 .AND. JX.NE.JFLD) THEN
              IF( IXP(N+IFLD).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
                IERR = 1
                WRITE(ISC,'(A)') 'North Boundary'
                WRITE(IWR,'(A)') 'North Boundary'
              ENDIF
            ELSEIF( IBCDX.EQ.3 .AND. KX.NE.KFLD) THEN
              IF( IXP(N+IJFLD).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
                IERR = 1
                WRITE(ISC,'(A)') 'Top Boundary'
                WRITE(IWR,'(A)') 'Top Boundary'
              ENDIF
            ENDIF
!
!---        Report boundary error  ---
!
            IF( IERR.EQ.1 ) THEN
              WRITE(ISC,'(A,I9)') 'Node = ',N
              WRITE(IWR,'(A,I9)') 'Node = ',N
              WRITE(ISC,'(3(A,I9))') 'I = ',I,' J = ',J,' K = ',K
              WRITE(IWR,'(3(A,I9))') 'I = ',I,' J = ',J,' K = ',K
              INDX = 7
              IMSG = NBC
              CHMSG = 'Geomechanical Boundary Condition Applied to ' // 
     &          'an Interior Surface: Boundary Number'
              CALL WRMSGS( INDX )
            ENDIF
            NBC_GM = NBC_GM + 1
            IF( NBC_GM.GT.LBC_GM ) THEN
              INDX = 5
              CHMSG = 'Number of Geomechanical Boundary Condition'
     &          //' Surfaces > Parameter LBC_GM'
              CALL WRMSGS( INDX )
            ENDIF
            IBCN_GM(NBC_GM) = N
            IBCR_GM(NBC_GM) = IBCRX
            IBCM_GM(NBC_GM) = IBCMX
            IBCIN_GM(NBC_GM) = NB
            IBCD_GM(NBC_GM) = IBCDX
            IBCT_GM(1,NBC_GM) = ITYP(1)
            IBCT_GM(2,NBC_GM) = ITYP(2)
            IBCT_GM(3,NBC_GM) = ITYP(3)
  330     CONTINUE
  340     CONTINUE
  350     CONTINUE
        ELSE
          INDX = 4
          CHMSG = 'Neither Displacement or Traction Geomechanical' //
     &      ' Boundary Condition Specified'
          CALL WRMSGS( INDX )
        ENDIF
        IF( INDEX(ADUM(1:),'file').NE.0 ) CLOSE(UNIT=26)
  400 CONTINUE
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDGMBC group  ---
!
      RETURN
      END

!!----------------------Subroutine--------------------------------------!
!!
!      SUBROUTINE RDGMLK
!!
!!-------------------------Disclaimer-----------------------------------!
!!
!!     This material was prepared as an account of work sponsored by
!!     an agency of the United States Government. Neither the
!!     United States Government nor the United States Department of
!!     Energy, nor Battelle, nor any of their employees, makes any
!!     warranty, express or implied, or assumes any legal liability or
!!     responsibility for the accuracy, completeness, or usefulness
!!     of any information, apparatus, product, software or process
!!     disclosed, or represents that its use would not infringe
!!     privately owned rights.
!!
!!----------------------Acknowledgement---------------------------------!
!!
!!     This software and its documentation were produced with Government
!!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!!     United Department of Energy. The Government retains a paid-up
!!     non-exclusive, irrevocable worldwide license to reproduce,
!!     prepare derivative works, perform publicly and display publicly
!!     by or for the Government, including the right to distribute to
!!     other Government contractors.
!!
!!---------------------Copyright Notices--------------------------------!
!!
!!            Copyright Battelle Memorial Institute, 1996
!!                    All Rights Reserved.
!!
!!----------------------Description-------------------------------------!
!!
!!     Read geomechanical link card.
!!
!!----------------------Authors-----------------------------------------!
!!
!!     Written by MD White, PNNL, 12 September 2011.
!!
!#ifdef 1
!!----------------------Fortran 90 Modules------------------------------!
!!
!      USE GLB_PAR
!      USE SOLTN
!      USE PORMED
!      USE GRID
!      USE GEOMECH
!      USE FILES
!!
!#endif
!!----------------------Implicit Double Precision-----------------------!
!!
!      IMPLICIT REAL*8 (A-H,O-Z)
!      IMPLICIT INTEGER (I-N)
!!
!!----------------------Parameter Statements----------------------------!
!!
!#ifndef 1
!      INCLUDE 'parameters'
!#endif
!!
!!----------------------Common Blocks-----------------------------------!
!!
!#ifndef 1
!      INCLUDE 'commons'
!#endif
!!
!!----------------------Type Declarations-------------------------------!
!!
!      CHARACTER*64 ADUM,RDUM,UNTS
!      CHARACTER*512 CHDUM
!!
!!----------------------Executable Lines--------------------------------!
!!
!      ISUB_LOG = ISUB_LOG+1
!      SUB_LOG(ISUB_LOG) = '/RDGMLK'
!      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
!     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!!
!!---  Write card information to ouput file  ---
!!
!      CARD = 'Geomechanical Link Card'
!      ICD = INDEX( CARD,'  ' )-1
!      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!!
!!---  Loop over the geomechanical link information lines  ---
!!
!      N = 0
!      IJK = 0
!   10 CONTINUE
!        IF( N.GE.NROCK .OR. IJK.GT.0 ) GOTO 500
!        ISTART = 1
!        CALL RDINPL( CHDUM )
!        CALL LCASE( CHDUM )
!        VARB = 'Rock/Soil Name'
!        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!!
!!---    IJK, KIJ, or JKI indexing  ---
!!
!        IF( INDEX(ADUM(1:),'indexing').NE.0 ) THEN
!          IF( INDEX(ROCK(1)(1:),'indexing').EQ.0 ) THEN
!            INDX = 4
!            CHMSG = 'Indexing Option Not Declared ' //
!     &        'in Geomechanical Properties Card'
!            CALL WRMSGS( INDX )
!          ENDIF
!          IF( INDEX(ADUM,'ijk').NE.0 ) THEN
!            IJK = 1
!          ELSEIF( INDEX(ADUM,'jki').NE.0 ) THEN
!            IJK = 2
!          ELSEIF( INDEX(ADUM,'kij').NE.0 ) THEN
!            IJK = 3
!          ELSE
!            INDX = 4
!            CHMSG = 'Unrecognized Indexing Option' // ADUM(1:NCH)
!            CALL WRMSGS( INDX )
!          ENDIF
!          GOTO 220
!        ENDIF
!!
!!---    Search known rock types for a matching type ---
!!
!        DO 100 M = 1, NROCK
!          IF( ADUM .EQ. ROCK(M)) THEN
!            IROCK = M
!            GOTO 200
!          ENDIF
!  100   CONTINUE
!!
!!---    Search known scaling groups for a matching type ---
!!
!        IF( ISLC(19).EQ.1 ) THEN
!          DO 110 M = 1,NSCALE
!             IF( ADUM.EQ.SCALNM(M) ) THEN
!                ISGRP = M
!                IROCK = 1
!                GOTO 200
!             ENDIF
!  110     CONTINUE
!          INDX = 2
!          CHMSG = 'Unrecognized Rock/Soil Type or Scaling Group: '
!     &      // ADUM(1:NCH)
!          CALL WRMSGS( INDX )
!          GOTO 10
!        ENDIF
!        INDX = 2
!        CHMSG = 'Unrecognized Rock/Soil Type: ' // ADUM(1:NCH)
!        CALL WRMSGS( INDX )
!        GOTO 10
!  200   CONTINUE
!!
!!---    Loop over rock/soils within scaling group  ---
!!
!        IF( ISLC(19).EQ.1 .AND. ISGRP.NE.0 ) THEN
!          DO 202 M = IROCK,NROCK
!            IF( ISCALE(M).EQ.ISGRP ) THEN
!              IROCK = M
!              GOTO 204
!            ENDIF
!  202     CONTINUE
!        ENDIF
!  204   CONTINUE
!!
!!---    Write rock/soil name  ---
!!
!        WRITE (IWR,'(/,2A)') 'Rock/Soil Name: ',ROCK(IROCK)
!        N = N + 1
!  220   CONTINUE
!!
!!---    Read porosity-mean stress function  ---
!!
!        VARB = 'Porosity-Mean Stress Function Type: '
!        IDFLT = 1
!        ADUM = 'zero'
!        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!        IF( INDEX(ADUM(1:),'davis').NE.0 ) THEN
!          IF( IJK.GT.0 ) THEN
!            DO 230 N = 1,NFLD
!              IPROP_GM(1,IZ(N)) = 1
!  230       CONTINUE
!          ELSE
!            IPROP_GM(1,IROCK) = 1
!          ENDIF
!          IP1X = 1
!        ELSEIF( INDEX(ADUM(1:),'zero').NE.0 .OR.
!     &    INDEX(ADUM(1:),'none').NE.0 .OR.
!     &    INDEX(ADUM(1:),'n/a').NE.0 .OR.
!     &    INDEX(ADUM(1:),'null').NE.0 ) THEN
!          IF( IJK.GT.0 ) THEN
!            DO 232 N = 1,NFLD
!              IPROP_GM(1,IZ(N)) = 0
!  232       CONTINUE
!          ELSE
!            IPROP_GM(1,IROCK) = 0
!          ENDIF
!          IP1X = 0
!        ELSE
!          INDX = 4
!          CHMSG = 'Unrecognized Porosity-Mean Stress Function: '//ADUM
!          CALL WRMSGS( INDX )
!        ENDIF
!!
!!---    Read intrinsic permeability-porosity function  ---
!!
!        VARB = 'Intrinsic Permeability-Porosity Function Type: '
!        IDFLT = 1
!        ADUM = 'zero'
!        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!        IF( INDEX(ADUM(1:),'davis').NE.0 ) THEN
!          IF( IJK.GT.0 ) THEN
!            DO 234 N = 1,NFLD
!              IPROP_GM(2,IZ(N)) = 1
!  234       CONTINUE
!          ELSE
!            IPROP_GM(2,IROCK) = 1
!          ENDIF
!          IP2X = 1
!        ELSEIF( INDEX(ADUM(1:),'zero').NE.0 .OR.
!     &    INDEX(ADUM(1:),'none').NE.0 .OR.
!     &    INDEX(ADUM(1:),'n/a').NE.0 .OR.
!     &    INDEX(ADUM(1:),'null').NE.0 ) THEN
!          IF( IJK.GT.0 ) THEN
!            DO 236 N = 1,NFLD
!              IPROP_GM(2,IZ(N)) = 0
!  236       CONTINUE
!          ELSE
!            IPROP_GM(2,IROCK) = 0
!          ENDIF
!          IP2X = 0
!        ELSE
!          INDX = 4
!          CHMSG = 'Unrecognized Intrinsic Permeability-' //
!     &      'Porosity Function: '//ADUM
!          CALL WRMSGS( INDX )
!        ENDIF
!!
!!---    Read capillary pressure-permeability/porosity function  ---
!!
!        VARB = 'Capillary Presssure-Permeability/Porosity ' //
!     &    'Function Type: '
!        IDFLT = 1
!        ADUM = 'zero'
!        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!        IF( INDEX(ADUM(1:),'leverett').NE.0 ) THEN
!          IF( IJK.GT.0 ) THEN
!            DO 238 N = 1,NFLD
!              IPROP_GM(3,IZ(N)) = 1
!  238       CONTINUE
!          ELSE
!            IPROP_GM(3,IROCK) = 1
!          ENDIF
!          IP3X = 1
!        ELSEIF( INDEX(ADUM(1:),'zero').NE.0 .OR.
!     &    INDEX(ADUM(1:),'none').NE.0 .OR.
!     &    INDEX(ADUM(1:),'n/a').NE.0 .OR.
!     &    INDEX(ADUM(1:),'null').NE.0 ) THEN
!          IF( IJK.GT.0 ) THEN
!            DO 240 N = 1,NFLD
!              IPROP_GM(3,IZ(N)) = 0
!  240       CONTINUE
!          ELSE
!            IPROP_GM(3,IROCK) = 0
!          ENDIF
!          IP3X = 1
!        ELSE
!          INDX = 4
!          CHMSG = 'Unrecognized Capillary Presssure-' //
!     &      'Permeability/Porosity Function: '//ADUM
!          CALL WRMSGS( INDX )
!        ENDIF
!!
!!---    Davis-Davis porosity-mean stress function  ---
!!
!        IF( IP1X.EQ.1 ) THEN
!          WRITE(IWR,'(A)') 'Davis-Davis Porosity-Mean Stress Function'
!!
!!---      Davis-Davis porosity-mean stress function exponent  ---
!!
!          VARB = 'Davis-Davis Porosity-Mean Stress Function Exponent'
!          IDFLT = 1
!          UNTS = 'null'
!          INDX = 6
!          LNDX = 9
!          IF( IJK.GT.0 ) THEN
!            CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PROP_GM,INDX,LNDX )
!          ELSE
!            IDFLT = 1
!            CALL RDDPR(ISTART,ICOMMA,CHDUM,PROP_GM(6,IROCK))
!            WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
!     &        ': ',PROP_GM(6,IROCK)
!          ENDIF
!!
!!---      Residual porosity at high stress  ---
!!
!          VARB = 'Residual Porosity at High Stress'
!          IDFLT = 1
!          UNTS = 'null'
!          INDX = 5
!          LNDX = 9
!          IF( IJK.GT.0 ) THEN
!            CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PROP_GM,INDX,LNDX )
!          ELSE
!            IDFLT = 1
!            CALL RDDPR(ISTART,ICOMMA,CHDUM,PROP_GM(5,IROCK))
!            WRITE(IWR,'(2X,2A,1PE11.4,$)') VARB(1:IVR),
!     &        ': ',PROP_GM(5,IROCK)
!          ENDIF
!        ENDIF
!!
!!---    Davis-Davis intrinsic permeability-porosity function  ---
!!
!        IF( IP2X.EQ.1 ) THEN
!          WRITE(IWR,'(A)') 'Davis-Davis Intrinsic Permeability-' //
!     &      'Porosity Function'
!!
!!---      Davis-Davis intrinsic permeability-porosity
!!         function exponent  ---
!!
!          VARB = 'Davis-Davis Intrinsic Permeability-Porosity ' //
!     &      'Function Exponent'
!          IDFLT = 1
!          UNTS = 'null'
!          INDX = 7
!          LNDX = 9
!          IF( IJK.GT.0 ) THEN
!            CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PROP_GM,INDX,LNDX )
!          ELSE
!            IDFLT = 1
!            CALL RDDPR(ISTART,ICOMMA,CHDUM,PROP_GM(7,IROCK))
!            WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
!     &        ': ',PROP_GM(7,IROCK)
!          ENDIF
!        ENDIF
!!
!!---    Loop over remaining rock/soils within scaling group  ---
!!
!        IF( ISLC(19).EQ.1 .AND. IROCK.LT.NROCK ) THEN
!          DO 490 M = IROCK+1,NROCK
!            IF( ISCALE(M).EQ.ISGRP ) THEN
!              N = N+1
!              DO 470 L = 1,9
!                PROP_GM(L,M) = PROP_GM(L,IROCK)
!  470         CONTINUE
!              DO 480 L = 1,3
!                IPROP_GM(L,M) = IPROP_GM(L,IROCK)
!  480         CONTINUE
!            ENDIF
!  490     CONTINUE
!        ENDIF
!!
!!---    Read next rock/soil type or scaling group  ---
!!
!        IF( N .LT. NROCK ) WRITE(IWR,'(/)')
!        GOTO 10
!  500 CONTINUE
!
!---  Reset subroutine character string ---
!
!      ISUB_LOG = ISUB_LOG-1
!!
!!---  End of RDGMLK group  ---
!!
!      RETURN
!      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RSDL_GM
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
!     Convergence check for sequential coupled flow and transport and
!     geomechanics  
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 22 February 2017 (Robert II becomes 
!     King of Scotland, beginning the Stuart dynasty in 1371).
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE PORMED
      USE GRID
      USE GLB_PAR
      USE GEOMECH
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
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RSDL_GM'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Initialize residual  ---
!
      RSD_GM = 0.D+0
      NSD_GM = 0
!
!---  Loop over the finite elements ---
!
      DO N = 1,NFLD
        IF( IXP(N).EQ.0 ) CYCLE
        IZN = IZ(N)
!
!---    Flow porosity at iterate level k+1  --
!
        PORDFWK1X = PORD(2,N)
!
!---    Drained bulk modulus  --
!
        BLKDRNX = PROP_GM(1,IZN)/(3.0E+0*(1.0D+0-2.0D+0*PROP_GM(2,IZN)))
!
!---    1/N  ---
!
        OONMODX = (PROP_GM(3,IZN)-POR(1,IZN))*(1.0D+0-PROP_GM(3,IZN))
     &    /BLKDRNX
!
!---    Reference volumetric strain  ---
!
        EPSVREFX = (SIGV_CMP(N)-PROP_GM(3,IZN)*PCMP(N))/BLKDRNX
!
!---    Volumetric strain, at iterate level k  ---
!
        EPSVX = (SIGV_GM(2,N)-PROP_GM(3,IZN)*P_GM(2,N))/BLKDRNX
!
!---    Volumetric strain differential, at iterate level k  ---
!
        DEVKX = EPSVX-EPSVREFX
!
!---    Pressure differential, at iterate level k  ---
!
        DPKX = P_GM(2,N)-PCMP(N)
!
!---    Mechanical porosity at iterate level k  --
!
        PORDMCHKX = POR(1,IZN) - PROP_GM(3,IZN)*DEVKX + OONMODX*DPKX
!
!---    Volumetric stress at iterate level k+1  ---
!
        SIGV_GM(3,N) = (SIG_GM(1,N)+SIG_GM(2,N)+SIG_GM(3,N))/3.D+0
!
!---    Volumetric stress differential, at iterate level k+1  ---
!
        DSVK1X = SIGV_GM(3,N)-SIGV_GM(2,N)
!
!---    Volumetric stress differential, at iterate level k+1  ---
!
        DEVK1X = DSVK1X/BLKDRNX
!
!---    Pressure differential, at iterate level k+1  ---
!
        DPK1X = P_GM(3,N)-P_GM(2,N)
!
!---    Mechanical porosity at iterate level k+1  --
!
        PORDMCHK1X = PORDMCHKX + OONMODX*DPK1X + PROP_GM(3,IZN)*DEVK1X
!
!---    Find maximum residual  --
!
        RSDX = ABS((PORDFWK1X-PORDMCHK1X)/PORDMCHK1X)
        IF( RSDX.GT.RSD_GM ) THEN
          RSD_GM = RSDX
          NSD_GM = N
        ENDIF
      ENDDO
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RSDL_GM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SSM( EX,PX )
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
!     Compute the 6 x 6 components of the stress-strain matrix
!
!     EX(6,6) - stress-strain matrix
!     PX(1) - Young's modulus, Pa
!     PX(2) - Poisson's ratio
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 7 November 2016 (Ensisheim meteorite).
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE CONST
      USE GLB_PAR
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
      REAL*8 PX(9),EX(6,6)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/SSM'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
      VARX = PX(1)/((1.D+0+PX(2))*(1.D+0-2.D+0*PX(2)))
      EX(1,1) = VARX*(1.D+0-PX(2))
      EX(1,2) = VARX*PX(2)
      EX(1,3) = VARX*PX(2)
      EX(1,4) = 0.D+0
      EX(1,5) = 0.D+0
      EX(1,6) = 0.D+0
      EX(2,1) = VARX*PX(2)
      EX(2,2) = VARX*(1.D+0-PX(2))
      EX(2,3) = VARX*PX(2)
      EX(2,4) = 0.D+0
      EX(2,5) = 0.D+0
      EX(2,6) = 0.D+0
      EX(3,1) = VARX*PX(2)
      EX(3,2) = VARX*PX(2)
      EX(3,3) = VARX*(1.D+0-PX(2))
      EX(3,4) = 0.D+0
      EX(3,5) = 0.D+0
      EX(3,6) = 0.D+0
      EX(4,1) = 0.D+0
      EX(4,2) = 0.D+0
      EX(4,3) = 0.D+0
      EX(4,4) = VARX*(1.D+0-2.D+0*PX(2))/2.D+0
      EX(4,5) = 0.D+0
      EX(4,6) = 0.D+0
      EX(5,1) = 0.D+0
      EX(5,2) = 0.D+0
      EX(5,3) = 0.D+0
      EX(5,4) = 0.D+0
      EX(5,5) = VARX*(1.D+0-2.D+0*PX(2))/2.D+0
      EX(5,6) = 0.D+0
      EX(6,1) = 0.D+0
      EX(6,2) = 0.D+0
      EX(6,3) = 0.D+0
      EX(6,4) = 0.D+0
      EX(6,5) = 0.D+0
      EX(6,6) = VARX*(1.D+0-2.D+0*PX(2))/2.D+0
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of SSM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE STATIC_GM
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
!     Static porothermoelastic geomechanics.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 28 February 2017 (The erroneous word
!     "dord" is discovered in the Webster's New International 
!     Dictionary, Second Edition, prompting an investigation in 1939).
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE PORMED
      USE GRID
      USE GLB_PAR
      USE GEOMECH
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
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/STATIC_GM'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---    Zero geomechanical Jacobian matrix  ---
!
        CALL JCBZ_GM
!
!---    Load geomechanical Jacobian matrix  ---
!
        CALL JCBL_GM
!
!---    Modify the geomechanical Jacobian matrix and problem vector
!       for boundary conditions  ---
!
        CALL BCJ_GM
!
!---    Linear equation solver  ---
!
        IF( ILES.EQ.1 ) THEN
          INDX = 2
          CALL BAND( 0,MU_GM,ML_GM,INDX )
        ELSEIF( ILES.EQ.3 ) THEN
          INDX = 2
          CALL PSPLIB( 0,INDX )
        ENDIF
!
!---    Displacements at finite elment nodes  ---
!
        CALL DISP_GM
!
!---    Strains at finite element centroids  ---
!
        CALL STRAIN_GM
!
!---    Stresses at finite element centroids  ---
!
        CALL STRESS_GM
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of STATIC_GM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE STRAIN_GM
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
!     Strain tensor at finite-element centroid.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 January 2017 (3M begins marketing
!     Scotch Tape in 1930).
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GLB_PAR
      USE GEOMECH
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
      REAL*8 BX(6,24),UX(24)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/STRAIN_GM'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Finite-element centroid in the natural
!     coordinate system Xi, Eta, and Mu ---
!
      PXIX = 0.D+0
      PETAX = 0.D+0
      PMUX = 0.D+0
!
!---  Loop over the finite elements ---
!
      DO N = 1,NFLD
        IF( IXP(N).EQ.0 ) CYCLE
        DO M0 = 1,8
          M1 = (M0-1)*3
          NFEN = ND_GM(M0,N)
          UX(M1+1) = U_GM(2,NFEN)
          UX(M1+2) = V_GM(2,NFEN)
          UX(M1+3) = W_GM(2,NFEN)
        ENDDO
        CALL ESDM( PXIX,PETAX,PMUX,XE(1,N),YE(1,N),ZE(1,N),BX,DETJX )
        DO M2 = 1,6
          EPS_GM(M2,N) = 0.D+0
        ENDDO
        DO M3 = 1,24
          EPS_GM(1,N) = EPS_GM(1,N) + BX(1,M3)*UX(M3)
          EPS_GM(2,N) = EPS_GM(2,N) + BX(2,M3)*UX(M3)
          EPS_GM(3,N) = EPS_GM(3,N) + BX(3,M3)*UX(M3)
          EPS_GM(4,N) = EPS_GM(4,N) + BX(4,M3)*UX(M3)
          EPS_GM(5,N) = EPS_GM(5,N) + BX(5,M3)*UX(M3)
          EPS_GM(6,N) = EPS_GM(6,N) + BX(6,M3)*UX(M3)
        ENDDO
!
!---    Change sign to make stress/strain to be positive in
!       compression  ---
!
        EPS_GM(1,N) = -EPS_GM(1,N)
        EPS_GM(2,N) = -EPS_GM(2,N)
        EPS_GM(3,N) = -EPS_GM(3,N)
        EPS_GM(4,N) = -5.D-1*EPS_GM(4,N)
        EPS_GM(5,N) = -5.D-1*EPS_GM(5,N)
        EPS_GM(6,N) = -5.D-1*EPS_GM(6,N)
      ENDDO
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of STRAIN_GM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE STRESS_GM
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
!     Stress tensor at finite-element centroid.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 31 January 2017 (3M begins marketing
!     Scotch Tape in 1930).
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GLB_PAR
      USE GEOMECH
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
      REAL*8 EX(6,6),FX(6)
!
!----------------------Data Statements---------------------------------!
!
      DATA FX / 1.D+0,1.D+0,1.D+0,2.D+0,2.D+0,2.D+0 /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/STRESS_GM'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Loop over the finite elements ---
!
      DO N = 1,NFLD
        IF( IXP(N).EQ.0 ) CYCLE
        IZN = IZ(N)
!
!---    Stress-strain matrix  ---
!
        CALL SSM( EX,PROP_GM(1,IZN) )
        DO M0 = 1,6
          SIG_GM(M0,N) = 0.D+0
        ENDDO
        DO M1 = 1,6
          DO M2 = 1,6
            SIG_GM(M1,N) = SIG_GM(M1,N) + EX(M1,M2)*FX(M2)*EPS_GM(M2,N)
          ENDDO
        ENDDO
      ENDDO
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of STRESS_GM group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE STRESSV_GM( INDX )
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
!     Reference volumetric stress at finite-element centroid.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 21 February 2017 (Initial issue of 
!     the Cherokee Phoenix in 1828).
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GLB_PAR
      USE GEOMECH
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
      SUB_LOG(ISUB_LOG) = '/STRESSV_GM'
      IF( INDEX(SVN_ID(307)(1:1),'$').EQ.0 ) SVN_ID(307) =
     & '$Id: geomech.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Loop over the finite elements ---
!
      DO N = 1,NFLD
        IF( IXP(N).EQ.0 ) CYCLE
!
!---    Reference volumetric stress  ---
!
        IF( INDX.EQ.0 ) THEN
          SIGV_CMP(N) = (SIG_GM(1,N)+SIG_GM(2,N)+SIG_GM(3,N))/3.D+0
!
!---    k or k+1 level volumetric stress  ---
!
        ELSE
          SIGV_GM(INDX,N) = (SIG_GM(1,N)+SIG_GM(2,N)+SIG_GM(3,N))/3.D+0
        ENDIF
      ENDDO
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of STRESSV_GM group  ---
!
      RETURN
      END



