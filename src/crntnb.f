!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CRNTNB
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
!     Compute the local maximum Courant numbers.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, October, 1995.
!     Last Modified by MD White, Battelle, PNL, October 10, 1995.
!     $Id: crntnb.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE HYST
      USE GRID
      USE FLUXP
      USE FLUXN
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
      SUB_LOG(ISUB_LOG) = '/CRNTNB'
      IF( INDEX(SVN_ID(27)(1:1),'$').EQ.0 ) SVN_ID(27) =
     & '$Id: crntnb.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Aqueous Phase Courant Number  ---
!
      IF( IEQW.GT.0 ) THEN
        DO 100 N = 1,NFLD
          IF( IXP(N).EQ.0 ) GOTO 100
          NPX = NSX(N)
          NPY = NSY(N)
          NPZ = NSZ(N)
          NQX = NSX(N)+1
          IF( INBS(4,N).NE.0 ) NQX = INBS(4,N)
          NQY = NSY(N)+IFLD
          IF( INBS(5,N).NE.0 ) NQY = INBS(5,N)
          NQZ = NSZ(N)+IJFLD
          IF( INBS(6,N).NE.0 ) NQZ = INBS(6,N)
          I = ID(N)
          J = JD(N)
          K = KD(N)
          CRNTL(N) = MAX( ABS(UL(1,NPX)*DT/DXGP(NPX)),
     &      ABS(UL(1,NQX)*DT/DXGP(NQX)),
     &      ABS(VL(1,NPY)*DT/(DYGP(NPY)*RP(I))),
     &      ABS(VL(1,NQY)*DT/(DYGP(NQY)*RP(I))),
     &      ABS(WL(1,NPZ)*DT/DZGP(NPZ)),
     &      ABS(WL(1,NQZ)*DT/DZGP(NQZ)) )
          IF( SL(2,N)*PORD(2,N).GT.EPSL ) THEN
            CRNTL(N) = CRNTL(N)/(SL(2,N)*PORD(2,N))
          ELSE
            CRNTL(N) = 0.D+0
          ENDIF
  100   CONTINUE
      ENDIF
!
!---  Gas Phase Courant Number  ---
!
      IF( IEQA.GT.0 ) THEN
        DO 200 N = 1,NFLD
          IF( IXP(N).EQ.0 ) GOTO 200
          NPX = NSX(N)
          NPY = NSY(N)
          NPZ = NSZ(N)
          NQX = NSX(N)+1
          IF( INBS(4,N).NE.0 ) NQX = INBS(4,N)
          NQY = NSY(N)+IFLD
          IF( INBS(5,N).NE.0 ) NQY = INBS(5,N)
          NQZ = NSZ(N)+IJFLD
          IF( INBS(6,N).NE.0 ) NQZ = INBS(6,N)
          I = ID(N)
          J = JD(N)
          K = KD(N)
          CRNTG(N) = MAX( ABS(UG(1,NPX)*DT/DXGP(NPX)),
     &      ABS(UG(1,NQX)*DT/DXGP(NQX)),
     &      ABS(VG(1,NPY)*DT/(DYGP(NPY)*RP(I))),
     &      ABS(VG(1,NQY)*DT/(DYGP(NQY)*RP(I))),
     &      ABS(WG(1,NPZ)*DT/DZGP(NPZ)),
     &      ABS(WG(1,NQZ)*DT/DZGP(NQZ)) )
          IF( (SG(2,N)-SGT(2,N))*PORD(2,N).GT.EPSL ) THEN
            CRNTG(N) = CRNTG(N)/((SG(2,N)-SGT(2,N))*PORD(2,N))
          ELSE
            CRNTG(N) = 0.D+0
          ENDIF
  200   CONTINUE
      ENDIF
!
!---  NAPL Courant Number  ---
!
      IF( IEQO.GT.0 ) THEN
        DO 300 N = 1,NFLD
          IF( IXP(N).EQ.0 ) GOTO 300
          NPX = NSX(N)
          NPY = NSY(N)
          NPZ = NSZ(N)
          NQX = NSX(N)+1
          IF( INBS(4,N).NE.0 ) NQX = INBS(4,N)
          NQY = NSY(N)+IFLD
          IF( INBS(5,N).NE.0 ) NQY = INBS(5,N)
          NQZ = NSZ(N)+IJFLD
          IF( INBS(6,N).NE.0 ) NQZ = INBS(6,N)
          I = ID(N)
          J = JD(N)
          K = KD(N)
          CRNTN(N) = MAX( ABS(UN(1,NPX)*DT/DXGP(NPX)),
     &      ABS(UN(1,NQX)*DT/DXGP(NQX)),
     &      ABS(VN(1,NPY)*DT/(DYGP(NPY)*RP(I))),
     &      ABS(VN(1,NQY)*DT/(DYGP(NQY)*RP(I))),
     &      ABS(WN(1,NPZ)*DT/DZGP(NPZ)),
     &      ABS(WN(1,NQZ)*DT/DZGP(NQZ)) )
          IF( (SN(2,N)-SNT(2,N))*PORD(2,N).GT.EPSL ) THEN
            CRNTN(N) = CRNTN(N)/((SN(2,N)-SNT(2,N))*PORD(2,N))
          ELSE
            CRNTN(N) = 0.D+0
          ENDIF
  300   CONTINUE
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of CRNTNB group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CRN_LIM( NSL )
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
!     Compute a time step that globally satisfies the Courant
!     number limit.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNNL, October, 2000.
!     Last Modified by MD White, Battelle, PNNL, October 17, 2000.
!     $Id: crntnb.F 1080 2017-03-14 16:22:02Z d3c002 $




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
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/CRN_LIM'
      IF( INDEX(SVN_ID(27)(1:1),'$').EQ.0 ) SVN_ID(27) =
     & '$Id: crntnb.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Compute the maximum Courant number ---
!
      CRNLX = 0.D+0
      CRNNX = 0.D+0
      CRNGX = 0.D+0
      IF( ISLC(17).EQ.1 ) THEN
        IF( IEQW.GT.0 ) THEN
          DO 100 N = 1,NFLD
            IF( IXP(N).EQ.0 ) GOTO 100







            CRNLX = MAX( CRNTL(N),CRNLX )
  100     CONTINUE
        ENDIF
        IF( IEQA.GT.0 ) THEN
          DO 110 N = 1,NFLD
            IF( IXP(N).EQ.0 ) GOTO 110







            CRNGX = MAX( CRNTG(N),CRNGX )
  110     CONTINUE
        ENDIF
        IF( IEQO.GT.0 ) THEN
          DO 120 N = 1,NFLD
            IF( IXP(N).EQ.0 ) GOTO 120







            CRNNX = MAX( CRNTN(N),CRNNX )
  120     CONTINUE
        ENDIF
      ELSEIF( ISLC(17).EQ.2 ) THEN
        IF( IEQW.GT.0 ) THEN
          DO 210 N = 1,NFLD
            IF( IXP(N).EQ.0 ) GOTO 210
            IF( 1.D+0-SL(2,N).LT.EPSL ) GOTO 210
            CLX = C(N,NSL)*YL(N,NSL)/(SL(2,N)*PORD(2,N)+EPSL)
            IF( CLX.GT.CCL_CRN(NSL) ) CRNLX = MAX( CRNTL(N),CRNLX )
  210     CONTINUE
        ENDIF
      ELSEIF( ISLC(17).EQ.3 ) THEN
        IF( IEQW.GT.0 ) THEN
          DO 300 N = 1,NFLD
            IF( IXP(N).EQ.0 ) GOTO 300







            CRNLX = MAX( CRNTL(N),CRNLX )
  300     CONTINUE
        ENDIF
      ENDIF
      CRNMX = MAX( CRNLX,CRNGX,CRNNX )
      DT_CRN = DT
      DTI_CRN = DTI
      TM_CRN = TM
      TM = TM-DT
      IF( CRNMX.GT.CRNTMXT ) THEN
        N_CRN(NSL) = INT(CRNMX/CRNTMXT) + 1
        REALX = REAL(N_CRN(NSL))
        DT = DT/REALX
        DTI = 1.D+0/(DT+EPSL)
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of CRN_LIM group  ---
!
      RETURN
      END
      
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CRN_G
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
!     Compute the maximum gas Courant number.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 01 Feburary 2007
!     Last Modified by MD White, PNNL, 01 Feburary 2007
!     $Id: crntnb.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOURC
      USE SOLTN
      USE OUTPU
      USE HYST
      USE GRID
      USE FLUXP
      USE FILES
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
      SUB_LOG(ISUB_LOG) = '/CRN_G'
      IF( INDEX(SVN_ID(27)(1:1),'$').EQ.0 ) SVN_ID(27) =
     & '$Id: crntnb.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Gas Courant number  ---
!
      CRNGMX = 0.D+0
      DO 100 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 100
        NPX = NSX(N)
        NPY = NSY(N)
        NPZ = NSZ(N)
        NQX = NSX(N)+1
        IF( INBS(4,N).NE.0 ) NQX = INBS(4,N)
        NQY = NSY(N)+IFLD
        IF( INBS(5,N).NE.0 ) NQY = INBS(5,N)
        NQZ = NSZ(N)+IJFLD
        IF( INBS(6,N).NE.0 ) NQZ = INBS(6,N)
        I = ID(N)
        J = JD(N)
        K = KD(N)
        IF( (SG(2,N)-SGT(2,N))*PORD(2,N).GT.EPSL ) THEN
          CRNGX = MAX( ABS(UG(1,NPX)*DT/DXGP(NPX)),
     &    ABS(UG(1,NQX)*DT/DXGP(NQX)),
     &    ABS(VG(1,NPY)*DT/(DYGP(NPY)*RP(I))),
     &    ABS(VG(1,NQY)*DT/(DYGP(NQY)*RP(I))),
     &    ABS(WG(1,NPZ)*DT/DZGP(NPZ)),
     &    ABS(WG(1,NQZ)*DT/DZGP(NQZ)) )
     &    /((SG(2,N)-SGT(2,N))*PORD(2,N))
          IF( CRNGX.GT.CRNGMX ) THEN
            CRNGMX = CRNGX
            NX = N
          ENDIF
        ENDIF
  100 CONTINUE
!
!---  Reduce time step  ---
!
      IF( CRNGMX.GT.CRNTMXC ) THEN
        ICNV = 1
        DTX = DT
        DT = 9.D-1*DT*CRNTMXC/CRNGMX
        TM = TM - DTX + DT
        DTO = DT
        DTI = 1.D+0/DT
        VAR = DT
        VARX = DTX
        IF( UNTM.NE.'null' ) THEN
          INDX = 1
          IUNS = 1
          CALL RDUNIT(UNTM,VAR,INDX)
          IUNS = 1
          CALL RDUNIT(UNTM,VARX,INDX)
          NCH = INDEX( UNTM,'  ')-1
        ENDIF
        WRITE(ISC,'(A,I6)') 'Execution Note: Gas Courant ' //
     &    'Limit Exceeded: Node: ',NX
        WRITE(IWR,'(A,I6)') 'Execution Note: Gas Courant ' //
     &    'Limit Exceeded: Node: ',NX
        WRITE(ISC,'(A,1PE11.4,1X,2A,1PE11.4,1X,A)') 
     &    '  Time step reduced from ',VARX,
     &    UNTM(1:NCH),' to ',VAR,UNTM(1:NCH)
        WRITE(IWR,'(A,1PE11.4,1X,2A,1PE11.4,1X,A)') 
     &    '  Time step reduced from ',VARX,
     &    UNTM(1:NCH),' to ',VAR,UNTM(1:NCH)
        DO 200 N = 1,NFLD
          PL(2,N) = PL(1,N)
          PN(2,N) = PN(1,N)
          XMLO(2,N) = XMLO(1,N)
          SN(2,N) = SN(1,N)
          NPHAZ(2,N) = NPHAZ(1,N)
  200   CONTINUE






      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of CRN_G group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CRN_L
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
!     Compute the maximum aqueous Courant number.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 01 Feburary 2007
!     Last Modified by MD White, PNNL, 01 Feburary 2007
!     $Id: crntnb.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOURC
      USE SOLTN
      USE OUTPU
      USE HYST
      USE GRID
      USE FLUXP
      USE FILES
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
      SUB_LOG(ISUB_LOG) = '/CRN_L'
      IF( INDEX(SVN_ID(27)(1:1),'$').EQ.0 ) SVN_ID(27) =
     & '$Id: crntnb.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Aqueous Courant number  ---
!
      CRNLMX = 0.D+0
      DO 100 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 100
        NPX = NSX(N)
        NPY = NSY(N)
        NPZ = NSZ(N)
        NQX = NSX(N)+1
        IF( INBS(4,N).NE.0 ) NQX = INBS(4,N)
        NQY = NSY(N)+IFLD
        IF( INBS(5,N).NE.0 ) NQY = INBS(5,N)
        NQZ = NSZ(N)+IJFLD
        IF( INBS(6,N).NE.0 ) NQZ = INBS(6,N)
        I = ID(N)
        J = JD(N)
        K = KD(N)
        IF( SL(2,N)*PORD(2,N).GT.EPSL ) THEN
          CRNLX = MAX( ABS(UL(1,NPX)*DT/DXGP(NPX)),
     &    ABS(UL(1,NQX)*DT/DXGP(NQX)),
     &    ABS(VL(1,NPY)*DT/(DYGP(NPY)*RP(I))),
     &    ABS(VL(1,NQY)*DT/(DYGP(NQY)*RP(I))),
     &    ABS(WL(1,NPZ)*DT/DZGP(NPZ)),
     &    ABS(WL(1,NQZ)*DT/DZGP(NQZ)) )
     &    /(SL(2,N)*PORD(2,N))
          IF( CRNLX.GT.CRNLMX ) THEN
            CRNLMX = CRNLX
            NX = N
          ENDIF
        ENDIF
  100 CONTINUE
!
!---  Reduce time step  ---
!
      IF( CRNLMX.GT.CRNTMXC ) THEN
        ICNV = 1
        DTX = DT
        DT = 9.D-1*DT*CRNTMXC/CRNLMX
        TM = TM - DTX + DT
        DTO = DT
        DTI = 1.D+0/DT
        VAR = DT
        VARX = DTX
        IF( UNTM.NE.'null' ) THEN
          INDX = 1
          IUNS = 1
          CALL RDUNIT(UNTM,VAR,INDX)
          IUNS = 1
          CALL RDUNIT(UNTM,VARX,INDX)
          NCH = INDEX( UNTM,'  ')-1
        ENDIF
        WRITE(ISC,'(A,I6)') 'Execution Note: Aqueous Courant ' //
     &    'Limit Exceeded: Node: ',NX
        WRITE(IWR,'(A,I6)') 'Execution Note: Aqueous Courant ' //
     &    'Limit Exceeded: Node: ',NX
        WRITE(ISC,'(A,1PE11.4,1X,2A,1PE11.4,1X,A)') 
     &    '  Time step reduced from ',VARX,
     &    UNTM(1:NCH),' to ',VAR,UNTM(1:NCH)
        WRITE(IWR,'(A,1PE11.4,1X,2A,1PE11.4,1X,A)') 
     &    '  Time step reduced from ',VARX,
     &    UNTM(1:NCH),' to ',VAR,UNTM(1:NCH)
        DO 200 N = 1,NFLD
          PL(2,N) = PL(1,N)
          PN(2,N) = PN(1,N)
          XMLO(2,N) = XMLO(1,N)
          SN(2,N) = SN(1,N)
          NPHAZ(2,N) = NPHAZ(1,N)
  200   CONTINUE






      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of CRN_L group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CRN_N
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
!     Compute the maximum NAPL Courant number.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 01 Feburary 2007
!     Last Modified by MD White, PNNL, 01 Feburary 2007
!     $Id: crntnb.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOURC
      USE SOLTN
      USE OUTPU
      USE HYST
      USE GRID
      USE FLUXN
      USE FILES
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
      SUB_LOG(ISUB_LOG) = '/CRN_N'
      IF( INDEX(SVN_ID(27)(1:1),'$').EQ.0 ) SVN_ID(27) =
     & '$Id: crntnb.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  NAPL Courant number  ---
!
      CRNNMX = 0.D+0
      DO 100 N = 1,NFLD
        IF( IXP(N).EQ.0 ) GOTO 100
        NPX = NSX(N)
        NPY = NSY(N)
        NPZ = NSZ(N)
        NQX = NSX(N)+1
        IF( INBS(4,N).NE.0 ) NQX = INBS(4,N)
        NQY = NSY(N)+IFLD
        IF( INBS(5,N).NE.0 ) NQY = INBS(5,N)
        NQZ = NSZ(N)+IJFLD
        IF( INBS(6,N).NE.0 ) NQZ = INBS(6,N)
        I = ID(N)
        J = JD(N)
        K = KD(N)
        IF( (SN(2,N)-SNT(2,N))*PORD(2,N).GT.EPSL ) THEN
          CRNNX = MAX( ABS(UN(1,NPX)*DT/DXGP(NPX)),
     &    ABS(UN(1,NQX)*DT/DXGP(NQX)),
     &    ABS(VN(1,NPY)*DT/(DYGP(NPY)*RP(I))),
     &    ABS(VN(1,NQY)*DT/(DYGP(NQY)*RP(I))),
     &    ABS(WN(1,NPZ)*DT/DZGP(NPZ)),
     &    ABS(WN(1,NQZ)*DT/DZGP(NQZ)) )
     &    /((SN(2,N)-SNT(2,N))*PORD(2,N))
          IF( CRNNX.GT.CRNNMX ) THEN
            CRNNMX = CRNNX
            NX = N
          ENDIF
        ENDIF
  100 CONTINUE
!
!---  Reduce time step  ---
!
      IF( CRNNMX.GT.CRNTMXC ) THEN
        ICNV = 1
        DTX = DT
        DT = 9.D-1*DT*CRNTMXC/CRNNMX
        TM = TM - DTX + DT
        DTO = DT
        DTI = 1.D+0/DT
        VAR = DT
        VARX = DTX
        IF( UNTM.NE.'null' ) THEN
          INDX = 1
          IUNS = 1
          CALL RDUNIT(UNTM,VAR,INDX)
          IUNS = 1
          CALL RDUNIT(UNTM,VARX,INDX)
          NCH = INDEX( UNTM,'  ')-1
        ENDIF
        WRITE(ISC,'(A,I6)') 'Execution Note: NAPL Courant ' //
     &    'Limit Exceeded: Node: ',NX
        WRITE(IWR,'(A,I6)') 'Execution Note: NAPL Courant ' //
     &    'Limit Exceeded: Node: ',NX
        WRITE(ISC,'(A,1PE11.4,1X,2A,1PE11.4,1X,A)') 
     &    '  Time step reduced from ',VARX,
     &    UNTM(1:NCH),' to ',VAR,UNTM(1:NCH)
        WRITE(IWR,'(A,1PE11.4,1X,2A,1PE11.4,1X,A)') 
     &    '  Time step reduced from ',VARX,
     &    UNTM(1:NCH),' to ',VAR,UNTM(1:NCH)
        DO 200 N = 1,NFLD
          PL(2,N) = PL(1,N)
          PN(2,N) = PN(1,N)
          XMLO(2,N) = XMLO(1,N)
          SN(2,N) = SN(1,N)
          NPHAZ(2,N) = NPHAZ(1,N)
  200   CONTINUE






      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of CRN_N group  ---
!
      RETURN
      END





