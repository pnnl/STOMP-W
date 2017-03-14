!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ADVB( PORX,SX,UX,VX,WX,UBX,VBX,WBX,N,M )
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
!     Computes the average flow or flux velocity on the bottom surface.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, August 1993.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE POINTE
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 UX(LSFV,*),VX(LSFV,*),WX(LSFV,*)
      REAL*8 SX(LSV,*),PORX(LSV,*)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ADVB'
      IF( INDEX(SVN_ID(2)(1:1),'$').EQ.0 ) SVN_ID(2) =
     & '$Id: advb.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      K = KD(N)
      NB = N-IJFLD
      UBX = 0.25D+0*((UX(1,NSX(N))+UX(1,NSX(N)+1))*DZGF(NB) +
     &  (UX(1,NSX(NB))+UX(1,NSX(NB)+1))*DZGF(N))/DZGP(NSZ(N))
      VBX = 0.25D+0*((VX(1,NSY(N))+VX(1,NSY(N)+IFLD))*DZGF(NB)
     &  + (VX(1,NSY(NB))+VX(1,NSY(NB)+IFLD))*DZGF(N))/DZGP(NSZ(N))
      WBX = WX(M,NSZ(N))
      IF( ISLC(52).EQ.0 ) THEN
        MN = MNEG(M)
        MP = MPOS(M)
        VMCP = SX(MP,N)*PORX(MP,N)
        VMCB = SX(MN,NB)*PORX(MN,NB)
        INDX = 17
        VMCX = DIFMN(VMCB,VMCP,DZGF(NB),DZGF(N),WBX,INDX)
        UBX = UBX/VMCX
        VBX = VBX/VMCX
        WBX = WBX/VMCX
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ADVB group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ADVS( PORX,SX,UX,VX,WX,USX,VSX,WSX,N,M )
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
!     Computes the average flow or flux velocity on the south surface.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, August 1993.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE POINTE
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 UX(LSFV,*),VX(LSFV,*),WX(LSFV,*)
      REAL*8 SX(LSV,*),PORX(LSV,*)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ADVS'
      IF( INDEX(SVN_ID(2)(1:1),'$').EQ.0 ) SVN_ID(2) =
     & '$Id: advb.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      J = JD(N)
      NS = N-IFLD
      USX = 0.25D+0*((UX(1,NSX(N))+UX(1,NSX(N)+1))*DYGF(NS) +
     &  (UX(1,NSX(NS))+UX(1,NSX(NS)+1))*DYGF(N))/DYGP(NSY(N))
      VSX = VX(M,NSY(N))
      WSX = 0.25D+0*((WX(1,NSZ(N))+WX(1,NSZ(N)+IJFLD))
     &  *DYGF(NS) + (WX(1,NSZ(NS))+WX(1,NSZ(NS)+IJFLD))
     &  *DYGF(N))/DYGP(NSY(N))
      IF( ISLC(52).EQ.0 ) THEN
        MN = MNEG(M)
        MP = MPOS(M)
        VMCP = SX(MP,N)*PORX(MP,N)
        VMCS = SX(MN,NS)*PORX(MN,NS)
        INDX = 17
        VMCX = DIFMN(VMCS,VMCP,DYGF(NS),DYGF(N),VSX,INDX)
        USX = USX/VMCX
        VSX = VSX/VMCX
        WSX = WSX/VMCX
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ADVS group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ADVW( PORX,SX,UX,VX,WX,UWX,VWX,WWX,N,M )
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
!     Computes the average flow or flux velocity on the west surface.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, August 1993.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE POINTE
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 UX(LSFV,*),VX(LSFV,*),WX(LSFV,*)
      REAL*8 SX(LSV,*),PORX(LSV,*)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ADVW'
      IF( INDEX(SVN_ID(2)(1:1),'$').EQ.0 ) SVN_ID(2) =
     & '$Id: advb.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      I = ID(N)
      NW = N-1
      UWX = UX(M,NSX(N))
      VWX = 0.25D+0*((VX(1,NSY(N))+VX(1,NSY(N)+IFLD))
     &  *DXGF(NW) + (VX(1,NSY(NW))+VX(1,NSY(NW)+IFLD))*DXGF(N))
     &  /DXGP(NSX(N))
      WWX = 0.25D+0*((WX(1,NSZ(N))+WX(1,NSZ(N)+IJFLD))
     &  *DXGF(NW) + (WX(1,NSZ(NW))+WX(1,NSZ(NW)+IJFLD))
     &  *DXGF(N))/DXGP(NSX(N))
      IF( ISLC(52).EQ.0 ) THEN
        MN = MNEG(M)
        MP = MPOS(M)
        VMCP = SX(MP,N)*PORX(MP,N)
        VMCW = SX(MN,NW)*PORX(MN,NW)
        INDX = 17
        VMCX = DIFMN(VMCW,VMCP,DXGF(NW),DXGF(N),UWX,INDX)
        UWX = UWX/VMCX
        VWX = VWX/VMCX
        WWX = WWX/VMCX
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ADVW group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ADVE( PORX,SX,UX,VX,WX,UEX,VEX,WEX,N,M )
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
!     Computes the average flow or flux velocity on the east surface.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, August 1993.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE POINTE
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 UX(LSFV,*),VX(LSFV,*),WX(LSFV,*)
      REAL*8 SX(LSV,*),PORX(LSV,*)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ADVE'
      IF( INDEX(SVN_ID(2)(1:1),'$').EQ.0 ) SVN_ID(2) =
     & '$Id: advb.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      I = ID(N)
      NE = N+1
      UEX = UX(M,NSX(N)+1)
      VEX = 0.25D+0*((VX(1,NSY(N))+VX(1,NSY(N)+IFLD))
     &  *DXGF(NE) + (VX(1,NSY(NE))+VX(1,NSY(NE)+IFLD))
     &  *DXGF(N))/DXGP(NSX(N)+1)
      WEX = 0.25D+0*((WX(1,NSZ(N))+WX(1,NSZ(N)+IJFLD))
     &  *DXGF(NE) + (WX(1,NSZ(NE))+WX(1,NSZ(NE)+IJFLD))
     &  *DXGF(N))/DXGP(NSX(N)+1)
      IF( ISLC(52).EQ.0 ) THEN
        MN = MNEG(M)
        MP = MPOS(M)
        VMCP = SX(MN,N)*PORX(MN,N)
        VMCE = SX(MP,NE)*PORX(MP,NE)
        INDX = 17
        VMCX = DIFMN(VMCP,VMCE,DXGF(N),DXGF(NE),UEX,INDX)
        UEX = UEX/VMCX
        VEX = VEX/VMCX
        WEX = WEX/VMCX
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ADVE group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ADVN( PORX,SX,UX,VX,WX,UNX,VNX,WNX,N,M )
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
!     Computes the average flow or flux velocity on the north surface.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, August 1993.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE POINTE
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 UX(LSFV,*),VX(LSFV,*),WX(LSFV,*)
      REAL*8 SX(LSV,*),PORX(LSV,*)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ADVN'
      IF( INDEX(SVN_ID(2)(1:1),'$').EQ.0 ) SVN_ID(2) =
     & '$Id: advb.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      J = JD(N)
      NN = N+IFLD
      UNX = 0.25D+0*((UX(1,NSX(N))+UX(1,NSX(N)+1))*DYGF(NN) +
     &  (UX(1,NSX(NN))+UX(1,NSX(NN)+1))*DYGF(N))/DYGP(NSY(N)+IFLD)
      VNX = VX(M,NSY(N)+IFLD)
      WNX = 0.25D+0*((WX(1,NSZ(N))+WX(1,NSZ(N)+IJFLD))
     &  *DYGF(NN) + (WX(1,NSZ(NN))+WX(1,NSZ(NN)+IJFLD))
     &  *DYGF(N))/DYGP(NSY(N)+IFLD)
      IF( ISLC(52).EQ.0 ) THEN
        MN = MNEG(M)
        MP = MPOS(M)
        VMCP = SX(MN,N)*PORX(MN,N)
        VMCN = SX(MP,NN)*PORX(MP,NN)
        INDX = 17
        VMCX = DIFMN(VMCP,VMCN,DYGF(N),DYGF(NN),VNX,INDX)
        UNX = UNX/VMCX
        VNX = VNX/VMCX
        WNX = WNX/VMCX
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ADVN group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ADVT( PORX,SX,UX,VX,WX,UTX,VTX,WTX,N,M )
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
!     Computes the average flow or flux velocity on the top surface.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, August 1993.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE POINTE
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 UX(LSFV,*),VX(LSFV,*),WX(LSFV,*)
      REAL*8 SX(LSV,*),PORX(LSV,*)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ADVT'
      IF( INDEX(SVN_ID(2)(1:1),'$').EQ.0 ) SVN_ID(2) =
     & '$Id: advb.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      K = KD(N)
      NT = N+IJFLD
      UTX = 0.25D+0*((UX(1,NSX(N))+UX(1,NSX(N)+1))*DZGF(NT) +
     &  (UX(1,NSX(NT))+UX(1,NSX(NT)+1))*DZGF(N))/DZGP(NSZ(N)+IJFLD)
      VTX = 0.25D+0*((VX(1,NSY(N))+VX(1,NSY(N)+IFLD))*DZGF(NT)
     & + (VX(1,NSY(NT))+VX(1,NSY(NT)+IFLD))*DZGF(N))/DZGP(NSZ(N)+IJFLD)
      WTX = WX(M,NSZ(N)+IJFLD)
      IF( ISLC(52).EQ.0 ) THEN
        MN = MNEG(M)
        MP = MPOS(M)
        VMCP = SX(MN,N)*PORX(MN,N)
        VMCT = SX(MP,NT)*PORX(MP,NT)
        INDX = 17
        VMCX = DIFMN(VMCP,VMCT,DZGF(N),DZGF(NT),WTX,INDX)
        UTX = UTX/VMCX
        VTX = VTX/VMCX
        WTX = WTX/VMCX
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ADVT group  ---
!
      RETURN
      END

