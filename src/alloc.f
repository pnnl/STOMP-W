!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ALLOC
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
!     Allocate memory for all array variables contained in
!     common blocks.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 5 November 2002.




!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE BCVGC
      USE BCVI
      USE BCVS
      USE BCVA
      USE BCVH
      USE CONST
      USE COUP_WELL
      USE CCP
      USE DB_PR
      USE EOR
      USE FILES
      USE FLUXP
      USE FLUXT
      USE FLUXN
      USE FLUXS
      USE FLUXGC
      USE FLUXD
      USE GEOMECH
      USE GRID
      USE HYDT
      USE HYST
      USE JACOB
      USE NAPL
      USE OUTPU
      USE POINTE
      USE WELL_FD
      USE WELL_FX
      USE WELL_CL
      USE PORMED
      USE SOLTN
      USE SOURC
      USE SPILL
      USE TABL
      USE TRNSPT
      USE FDVP
      USE FDVT
      USE FDVG
      USE FDVGC
      USE FDVN
      USE FDVI
      USE FDVS
      USE FDVD
      USE FDVA
      USE FDVH
      USE UCODE
      USE NCG_PT
      USE PLT_ATM
      USE REACT
      USE PTZRCOEF
      USE PTZR





!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL(KIND=DP) (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ALLOC'
!
!---  Allocate memory for version control log  ---
!
      ALLOCATE( SVN_ID(1:LFILES),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SVN_ID'
        CALL WRMSGS( INDX )
      ENDIF
      SVN_ID(11) =
     & '$Id: alloc.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Allocate memory for module BCV  ---
!
      CALL ALLOC_BCV
!
!---  Allocate memory for module BCVG  ---
!
      CALL ALLOC_BCVG
!
!---  Allocate memory for module BCVN  ---
!
      CALL ALLOC_BCVN
!
!---  Allocate memory for module BCVP  ---
!
      CALL ALLOC_BCVP
!
!---  Allocate memory for module BCVT  ---
!
      CALL ALLOC_BCVT
!
!---  Allocate memory for module DUAL_POR  ---
!
      CALL ALLOC_DUAL_POR
!
!---  Allocate memory for module GEOMECH  ---
!
      CALL ALLOC_GEOMECH
!
!---  Allocate memory for module PLT_ATM  ---
!
      CALL ALLOC_PLT_ATM
!
!---  Allocate memory  ---
!
      ALLOCATE( RHOIB(1:LSV,1:LBCI),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOIB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( THKIB(1:LSV,1:LBCI),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: THKIB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HIB(1:LSV,1:LBCI),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HIB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLSB(1:LSV,1:LBCS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLSB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMLSB(1:LSV,1:LBCS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMLSB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( OECB(1:LSV,1:LBCS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: OECB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YLSB(1:LSV,1:LBCS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YLSB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SSB(1:LSV,1:LBCS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SSB'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Nonaqueous liquid boundary condition variables
!
      ALLOCATE( XNAB(1:LSV,1:LBCA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XNAB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XNNB(1:LSV,1:LBCN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XNNB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XNOB(1:LSV,1:LBCA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XNOB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XNWB(1:LSV,1:LBCA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XNWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMNAB(1:LSV,1:LBCA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMNAB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMNNB(1:LSV,1:LBCN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMNNB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMNOB(1:LSV,1:LBCA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMNOB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMNWB(1:LSV,1:LBCA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMNWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFNAB(1:LSV,1:LBCA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFNAB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFNNB(1:LSV,1:LBCN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFNNB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFNOB(1:LSV,1:LBCA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFNOB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFNWB(1:LSV,1:LBCA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFNWB'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Gas component field variables  ---
!
      ALLOCATE( PVCB(1:LNGC,1:LSV,1:LBCGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PVCB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XGCB(1:LNGC,1:LSV,1:LBCGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XGCB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMGCB(1:LNGC,1:LSV,1:LBCGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMGCB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XNCB(1:LNGC,1:LSV,1:LBCGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XNCB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMNCB(1:LNGC,1:LSV,1:LBCGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMNCB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLCB(1:LNGC,1:LSV,1:LBCGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLCB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMLCB(1:LNGC,1:LSV,1:LBCGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMLCB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFLCB(1:LNGC,1:LSV,1:LBCGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFLCB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFGCB(1:LNGC,1:LSV,1:LBCGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFGCB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFNCB(1:LNGC,1:LSV,1:LBCGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFNCB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HGCB(1:LNGC,1:LSV,1:LBCGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HGCB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZGB(1:LSV,1:LBCGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZGB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZMCB(1:LNGC,1:LSV,1:LBCGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZMCB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZNB(1:LSV,1:LBCGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZNB'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Hydrate boundary condition variables
!
      ALLOCATE( XHWB(1:LSV,1:LBCH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XHWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XHAB(1:LSV,1:LBCH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XHAB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XHOB(1:LSV,1:LBCH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XHOB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XHNB(1:LSV,1:LBCN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XHNB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PVHNB(1:LSV,1:LBCN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PVHNB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PVNB(1:LSV,1:LBCN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PVNB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XGNB(1:LSV,1:LBCN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XGNB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMGNB(1:LSV,1:LBCN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMGNB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFGNB(1:LSV,1:LBCN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFGNB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLNB(1:LSV,1:LBCN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLNB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMLNB(1:LSV,1:LBCN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMLNB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOHB(1:LSV,1:LBCH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOHB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HHB(1:LSV,1:LBCH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HHB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( THKHB(1:LSV,1:LBCH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: THKHB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SHB(1:LSV,1:LBCH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SHB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PHB(1:LSV,1:LBCH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PHB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZLAB(1:LSV,1:LBCH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZLAB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PVHAB(1:LSV,1:LBCH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PVHAB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PVHOB(1:LSV,1:LBCH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PVHOB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UEGAB(1:LSV,1:LBCH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UEGAB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PTPS(1:5),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PTPS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IPTPS(1:4),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IPTPS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TC(1:LCMP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PC(1:LCMP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZRA(1:LCMP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZRA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VMC(1:LCMP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VMC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PAF(1:LCMP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PAF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WTM(1:LCMP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WTM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( GCPP(1:70,1:LNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: GCPP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( GWPP(1:70),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: GWPP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( BIJTD(1:LNGC+1,1:LNGC+1),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: BIJTD'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( BIJNA(1:LNGC+1,1:LNGC+1),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: BIJNA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISF(1:LSF),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( FNSF(1:LSF),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: FNSF'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Primary flux variables  ---
!
      ALLOCATE( UDGA(1:LSFV,1:LSX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UDGA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UDGN(1:LSFV,1:LSXN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UDGN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UDGO(1:LSFV,1:LSX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UDGO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UDGW(1:LSFV,1:LSX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UDGW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UDLA(1:LSFV,1:LSX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UDLA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UDLN(1:LSFV,1:LSXN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UDLN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UDLO(1:LSFV,1:LSX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UDLO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UDLW(1:LSFV,1:LSX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UDLW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UDS(1:LSFV,1:LSX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UDS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UG(1:LSFV,1:LSX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UGA(1:LSFV,1:LSX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UGA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UGN(1:LSFV,1:LSX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UGN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UGW(1:LSFV,1:LSX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UGW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UL(1:LSFV,1:LSX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ULA(1:LSFV,1:LSX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ULA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ULN(1:LSFV,1:LSXN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ULN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ULW(1:LSFV,1:LSX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ULW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VDGA(1:LSFV,1:LSY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VDGA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VDGN(1:LSFV,1:LSYN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VDGN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VDGO(1:LSFV,1:LSY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VDGO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VDGW(1:LSFV,1:LSY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VDGW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VDLA(1:LSFV,1:LSY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VDLA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VDLN(1:LSFV,1:LSYN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VDLN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VDLO(1:LSFV,1:LSY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VDLO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VDLW(1:LSFV,1:LSY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VDLW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VDS(1:LSFV,1:LSY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VDS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VG(1:LSFV,1:LSY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VGA(1:LSFV,1:LSY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VGA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VGN(1:LSFV,1:LSYN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VGN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VGW(1:LSFV,1:LSY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VGW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VL(1:LSFV,1:LSY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VLA(1:LSFV,1:LSY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VLA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VLN(1:LSFV,1:LSYN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VLN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VLW(1:LSFV,1:LSY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VLW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WDGA(1:LSFV,1:LSZ),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WDGA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WDGN(1:LSFV,1:LSZN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WDGN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WDGO(1:LSFV,1:LSZ),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WDGO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WDGW(1:LSFV,1:LSZ),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WDGW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WDLA(1:LSFV,1:LSZ),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WDLA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WDLN(1:LSFV,1:LSZN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WDLN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WDLO(1:LSFV,1:LSZ),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WDLO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WDLW(1:LSFV,1:LSZ),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WDLW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WDS(1:LSFV,1:LSZ),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WDS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WG(1:LSFV,1:LSZ),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WGA(1:LSFV,1:LSZ),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WGA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WGN(1:LSFV,1:LSZN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WGN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WGW(1:LSFV,1:LSZ),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WGW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WL(1:LSFV,1:LSZ),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WLA(1:LSFV,1:LSZ),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WLA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WLN(1:LSFV,1:LSZN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WLN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WLW(1:LSFV,1:LSZ),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WLW'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Energy flux variables  ---
!
      ALLOCATE( UQ(1:LSFV,1:LSXT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UQ'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VQ(1:LSFV,1:LSYT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VQ'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WQ(1:LSFV,1:LSZT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WQ'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  NAPL flux variables  ---
!
      ALLOCATE( UN(1:LSFV,1:LSXN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VN(1:LSFV,1:LSYN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WN(1:LSFV,1:LSZN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UDNA(1:LSFV,1:LSXN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UDNA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VDNA(1:LSFV,1:LSYN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VDNA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WDNA(1:LSFV,1:LSZN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WDNA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UDNN(1:LSFV,1:LSXN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UDNN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VDNN(1:LSFV,1:LSYN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VDNN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WDNN(1:LSFV,1:LSZN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WDNN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UDNO(1:LSFV,1:LSXN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UDNO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VDNO(1:LSFV,1:LSYN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VDNO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WDNO(1:LSFV,1:LSZN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WDNO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UDNW(1:LSFV,1:LSXN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UDNW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VDNW(1:LSFV,1:LSYN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VDNW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WDNW(1:LSFV,1:LSZN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WDNW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UNA(1:LSFV,1:LSXN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UNA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VNA(1:LSFV,1:LSYN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VNA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WNA(1:LSFV,1:LSZN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WNA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UNN(1:LSFV,1:LSXN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UNN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VNN(1:LSFV,1:LSYN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VNN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WNN(1:LSFV,1:LSZN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WNN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UNW(1:LSFV,1:LSXN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UNW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VNW(1:LSFV,1:LSYN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VNW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WNW(1:LSFV,1:LSZN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WNW'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Salt/Surfactant flux variables  ---
!
      ALLOCATE( US(1:LSFV,1:LSXS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: US'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VS(1:LSFV,1:LSYS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WS(1:LSFV,1:LSZS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UGO(1:LSFV,1:LSXN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UGO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VGO(1:LSFV,1:LSYN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VGO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WGO(1:LSFV,1:LSZN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WGO'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Dissolved-aqueous variables  ---
!
      ALLOCATE( ULO(1:LSFV,1:LSXN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ULO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VLO(1:LSFV,1:LSYN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VLO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WLO(1:LSFV,1:LSZN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WLO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UNO(1:LSFV,1:LSXN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UNO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VNO(1:LSFV,1:LSYN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VNO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WNO(1:LSFV,1:LSZN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WNO'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Gas component flux variables  ---
!
      ALLOCATE( UGC(1:LNGC,1:LSFV,1:LSXGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UGC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UDGC(1:LNGC,1:LSFV,1:LSXGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UDGC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UNC(1:LNGC,1:LSFV,1:LSXNC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UNC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UDNC(1:LNGC,1:LSFV,1:LSXNC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UDNC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ULC(1:LNGC,1:LSFV,1:LSXLC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ULC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UDLC(1:LNGC,1:LSFV,1:LSXLC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UDLC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VGC(1:LNGC,1:LSFV,1:LSYGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VGC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VDGC(1:LNGC,1:LSFV,1:LSYGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VDGC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VNC(1:LNGC,1:LSFV,1:LSYNC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VNC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VDNC(1:LNGC,1:LSFV,1:LSYNC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VDNC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VLC(1:LNGC,1:LSFV,1:LSYLC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VLC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VDLC(1:LNGC,1:LSFV,1:LSYLC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VDLC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WGC(1:LNGC,1:LSFV,1:LSZGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WGC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WDGC(1:LNGC,1:LSFV,1:LSZGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WDGC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WLC(1:LNGC,1:LSFV,1:LSZLC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WLC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WDLC(1:LNGC,1:LSFV,1:LSZLC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WDLC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WNC(1:LNGC,1:LSFV,1:LSZNC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WNC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WDNC(1:LNGC,1:LSFV,1:LSZNC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WDNC'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Grid variables  ---
!
      ALLOCATE( XE(1:8,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XE'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YE(1:8,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YE'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZE(1:8,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZE'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XP(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YP(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XREF(1:3),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XREF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZP(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RP(1:LFX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DXGF(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DXGF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DYGF(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DYGF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DZGF(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DZGF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DXGP(1:LSX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DXGP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DYGP(1:LSY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DYGP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DZGP(1:LSZ),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DZGP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( AFX(1:LSX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: AFX'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( AFY(1:LSY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: AFY'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( AFZ(1:LSZ),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: AFZ'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VOL(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VOL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( GRVX(1:LSX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: GRVX'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( GRVY(1:LSY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: GRVY'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( GRVZ(1:LSZ),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: GRVZ'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( GRVPX(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: GRVPX'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( GRVPY(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: GRVPY'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( GRVPZ(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: GRVPZ'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ROCK(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ROCK'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ROCK2(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ROCK2'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XREFU(1:3),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XREFU'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IXP(1:LFD+((LFX*LFY)*LSPILL)),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IXP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( INBS(1:6,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: INBS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( KSPS(1:(LFX**LSPILL),1:(LFY**LSPILL)),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: KSPS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IZ(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IZ'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IZ2(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IZ2'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( INP(1:LBR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: INP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IBR(1:5,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IBR'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ICM(1:5,1:6,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ICM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IXF(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IXF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( MDIM(1:6),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: MDIM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ID(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ID'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( JD(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: JD'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( KD(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: KD'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ND(1:LFX,1:LFY,1:LFZ),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ND'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NSX(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NSX'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NSY(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NSY'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NSZ(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NSZ'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NSSX(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NSSX'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NSSY(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NSSY'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NSSZ(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NSSZ'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IXREF(1:3),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IXREF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ASL(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ASL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( AST(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: AST'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ASLMIN(1:LSU,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ASLMIN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ASTMIN(1:LSU,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ASTMIN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ASTMAX(1:LSU,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ASTMAX'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ASNR(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ASNR'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ASNT(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ASNT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ASGT(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ASGT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SGT(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SGT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SNR(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SNR'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SNT(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SNT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NPHAZ(1:LSU,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NPHAZ'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ASGTL(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ASGTL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ASGTN(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ASGTN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SGTL(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SGTL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SGTN(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SGTN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ASNMIN(1:LSU,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ASNMIN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ASLSC(1:LSU,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ASLSC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SLSC(1:LSU,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SLSC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HDSC(1:LSU,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HDSC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IPH(1:LSU,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IPH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ALU(1:LJD,1:LJE),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ALU'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( BLU(1:LJF),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: BLU'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CLU(1:LJA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CLU'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DLU(1:LJB),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DLU'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RSDL(1:LUKW,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RSDL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IM(1:LUKW,1:LFD+((LFX*LFY)*LSPILL)),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ILU(1:LJI),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ILU'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( JLU(1:LJJ),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: JLU'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( JM(1:LUK,1:LUK),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: JM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( KLU(1:LJG,1:LJH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: KLU'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( MLU(1:LJO),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: MLU'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NLU(1:LJC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NLU'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( KLUC(1:LJK,1:LJL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: KLUC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( MLUC(1:LJM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: MLUC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NLUC(1:LJN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NLUC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISCO(1:4),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISCO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SATOC(1:4),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SATOC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CPOC(1:4),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CPOC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CIMTC(1:4),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CIMTC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISCS(1:4),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISCS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TERDC(1:11),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TERDC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SFCSF(1:6),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SFCSF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PCSLS(1:2,1:LRCS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PCSLS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CPAC(1:4),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CPAC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SATAC(1:4),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SATAC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISCA(1:4),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISCA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IPCSLS(1:LRCS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IPCSLS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PRTM(1:LPTM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PRTM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SF(1:2,1:LSF),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UNPLOT(1:LOUPV),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UNPLOT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UNREF(1:LOUPV),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UNREF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UNSF(1:2,1:LSF),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UNSF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CHREF(1:LOUPV),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CHREF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CHSF(1:LOUPV,1:3),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CHSF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IPLOT(1:LVPLOT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IPLOT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IREF(1:LVREF),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IREF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IPLOTGC(1:LVPLOT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IPLOTGC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IREFGC(1:LVREF),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IREFGC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NDREF(1:LREF),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NDREF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISFT(1:LSF),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISFT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISFGC(1:LSF),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISFGC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISFF(1:LSF),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISFF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISFD(1:LSF),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISFD'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISFC(1:6,1:LSF),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISFC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISFGP(1:LSF),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISFGP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISFSN(1:LSF),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISFSN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NSFDOM(1:LSF),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NSFDOM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISFDOM(4,1:LSFDOM,1:LSF),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISFDOM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( MNOD(1:LSFV),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: MNOD'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( MADJ(1:LSFV),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: MADJ'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( MFLX(1:LSFV),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: MFLX'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( MPOS(1:LSFV),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: MPOS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( MNEG(1:LSFV),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: MNEG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( MPOSB(1:LSV),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: MPOSB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( MNEGB(1:LSV),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: MNEGB'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Well field variables  ---
!
      ALLOCATE( PW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOLW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOLW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOGW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOGW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHONW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHONW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLOW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLOW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMLOW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMLOW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SNW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SNW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLWW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLWW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XGWW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XGWW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLAW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLAW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XGAW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XGAW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SLW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SLW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMLAW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMLAW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMGWW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMGWW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( STW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: STW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISLW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISLW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISGW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISGW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISNW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISNW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YMLOW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YMLOW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFLAW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFLAW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFGWW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFGWW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFLOW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFLOW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PWLW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PWLW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PWGW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PWGW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PWNW(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PWNW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PWB(1:LSV,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOLWB(1:LSV,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOLWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOGWB(1:LSV,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOGWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHONWB(1:LSV,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHONWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLWWB(1:LSV,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLWWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XGWWB(1:LSV,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XGWWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLAWB(1:LSV,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLAWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XGAWB(1:LSV,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XGAWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMLAWB(1:LSV,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMLAWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMGWWB(1:LSV,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMGWWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLOWB(1:LSV,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLOWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMLOWB(1:LSV,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMLOWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISLWB(1:LSV,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISLWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISGWB(1:LSV,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISGWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISNWB(1:LSV,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISNWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFLAWB(1:LSV,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFLAWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFGWWB(1:LSV,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFGWWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFLOWB(1:LSV,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFLOWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRCW_W(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRCW_W'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRCO_W(1:LSV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRCO_W'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHONW_O(1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHONW_O'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SNW_O(1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SNW_O'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLOW_O(1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLOW_O'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( STW_O(1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: STW_O'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOLW_O(1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOLW_O'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOGW_O(1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOGW_O'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SLW_O(1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SLW_O'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLWW_O(1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLWW_O'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XGWW_O(1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XGWW_O'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XGAW_O(1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XGAW_O'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLAW_O(1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLAW_O'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Well flux variables  ---
!
      ALLOCATE( WL_W(1:LSFV,1:LSZW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WL_W'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WG_W(1:LSFV,1:LSZW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WG_W'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WN_W(1:LSFV,1:LSZW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WN_W'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UL_W(1:LSFV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UL_W'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UG_W(1:LSFV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UG_W'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UN_W(1:LSFV,1:LSZW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UN_W'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WDLA_W(1:LSFV,1:LSZW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WDLA_W'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WDGW_W(1:LSFV,1:LSZW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WDGW_W'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WDLO_W(1:LSFV,1:LSZW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WDLO_W'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UDLA_W(1:LSFV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UDLA_W'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UDGW_W(1:LSFV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UDGW_W'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UDLO_W(1:LSFV,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UDLO_W'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( QL_W(1:4,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: QL_W'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( QN_W(1:4,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: QN_W'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( QT_W(1:4,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: QT_W'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Well control variables  ---
!
      ALLOCATE( WBR(1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WBR'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WBRS(1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WBRS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WWD(1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WWD'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WHP(1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WHP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WRH(1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WRH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WIDA(1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WIDA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WLVR(1:LNWV,1:LNWT,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WLVR'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WWDL(1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WWDL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WWDN(1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WWDN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WIDO(1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WIDO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DNRW(1:LUK,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DNRW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IWM(1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IWM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IWCC(1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IWCC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IXW(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IXW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IWL(1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IWL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NPHAZW(1:LSU,1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NPHAZW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NSZW(1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NSZW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IWN(1:LNWN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IWN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IWT(1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IWT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IWLDM(1:2*LNWS+4,1:LNW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IWLDM'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Coupled well variables  ---
!
      ALLOCATE( DNR_CW(1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DNR_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( FF_CW(1:3,1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: FF_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( FX_CW(1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: FX_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( FXA_CW(1:(LUK+2),1:LWN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: FXA_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( FXC_CW(1:LNGC,1:(LUK+2),1:LWN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: FXC_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( FXE_CW(1:(LUK+2),1:LWN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: FXE_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( FXW_CW(1:(LUK+2),1:LWN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: FXW_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( P_CW(1:3,1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: P_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PF_CW(1:LWF_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PF_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PL_CW(1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PL_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOF_CW(1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOF_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( Q_CW(1:4,1:LWN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: Q_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( QM_CW(1:(2*(3+LNGC)),1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: QM_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RS_CW(1:(LUK_CW+1),1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RS_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PAR_CW(1:5,1:LWI_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PAR_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TML_CW(1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TML_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VAR_CW(1:6+LNGC,1:LWT_CW,1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VAR_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VARC_CW(1:LSOLU_CW,1:LWT_CW,1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VARC_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PLX_CW(1:LWN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PLX_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PLY_CW(1:LWN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PLY_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PLZ_CW(1:LWN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PLZ_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XP_CW(1:2,1:LWN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XP_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YP_CW(1:2,1:LWN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YP_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZP_CW(1:2,1:LWN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZP_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XTP_CW(1:2,1:LWI_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XTP_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YTP_CW(1:2,1:LWI_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YTP_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZTP_CW(1:2,1:LWI_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZTP_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WNM_CW(1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WNM_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( T_CW(1:2,1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: T_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ICC_CW(1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ICC_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ID_CW(1:8,1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ID_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IM_CW(1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ,IM_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IMP_CW(1:LWTP_CW,1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ,IMP_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( INV_CW(1:LWN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ,INV_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IREF_CW(1:LVREF),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ,IREF_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IS_CW(1:LWI_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ,IS_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISOLU_CW(1:LSOLU_CW,1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ,ISOLU_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ITS_CW(1:LWTP_CW,1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ,ITS_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IT_CW(1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ,IT_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IWF_CW(1:LWF_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ,IWF_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IWN_CW(1:LWN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ,IWN_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IWP_CW(1:LWN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ,IWP_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( JM_CW(1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ,JM_CW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( KLU_CW(1:LUK_CW,1:LN_CW),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ,KLU_CW'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Porous media properties  ---
!
      ALLOCATE( POR(1:6,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: POR'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TOR(1:6,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TOR'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CMP(1:4,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CMP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( THKS(1:9,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: THKS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOS(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CPS(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CPS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PERM(1:9,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PERM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SCHR(1:LSCHR,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SCHR'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RPGC(1:LRPGC,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RPGC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RPLC(1:LRPLC,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RPLC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RPNC(1:LRPNC,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RPNC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NPLY_SL(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NPLY_SL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CPLY_SL(1:LPOLYC,1:LPOLYN,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CPLY_SL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NPLY_RL(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NPLY_RL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CPLY_RL(1:LPOLYC,1:LPOLYN,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CPLY_RL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DPLGA(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DPLGA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DPLLA(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DPLLA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DPTGA(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DPTGA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DPTLA(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DPTLA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CHML(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CHML'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( GAMMA(1:13,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: GAMMA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SCALNM(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SCALNM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RPLT(1:4,1:LRPL,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RPLT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFEF(1:5,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFEF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HCMWE(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HCMWE'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ITOR(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ITOR'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IRPG(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IRPG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IGAMMA(1:8),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IGAMMA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISCALE(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISCALE'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IRPL(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IRPL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IRPN(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IRPN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISCHR(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISCHR'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISM(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ITHK(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ITHK'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IRPLT(1:3,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IRPLT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISLTBL(1:4,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISLTBL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IRLTBL(1:4,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IRLTBL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IRGTBL(1:4,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IRGTBL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IRLTBLT(1:2,1:LRC,1:3),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IRLTBLT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IRNTBL(1:4,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IRNTBL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IDP(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IDP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISKP(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISKP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IPRF(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IPRF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( INHYD(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: INHYD'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TMPS(1:LEPD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TMPS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TMPE(1:LEPD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TMPE'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TMPD(1:LEPD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TMPD'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TMPX(1:LEPD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TMPX'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TMPN(1:LEPD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TMPN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TMPA(1:LEPD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TMPA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TMPC(1:LEPD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TMPC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RSD(1:(LUK*(1+LWELL+LSPILL))),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RSD'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RSDM(1:LEPD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RSDM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WFMN(1:20),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WFMN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IEQGC(1:LNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IEQGC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( GCNM(1:LNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: GCNM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HCNM(1:LNHC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HCNM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NRIM(1:LEPD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NRIM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NSD(1:(LUK*(1+LWELL+LSPILL))),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NSD'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NOTES(1:LNOTES),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NOTES'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRC(1:8+LSOLU+LSPT+LNGC,1:LSTM,1:LSR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRCP(1:8,1:LSR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRCP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRCT(1:LSV,1:LFDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRCT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRCW(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRCW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRCA(1:LSV,1:LFDG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRCA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRCN(1:LSV,1:LFDN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRCN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRCO(1:LSV,1:LFDNH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRCO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRCS(1:LSV,1:LFDS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRCS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRCD(1:LSV,1:LFDD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRCD'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRCGC(1:LNGC,1:LSV,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRCGC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRCIGC(1:LNGC,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRCIGC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRCIT(1:LFDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRCIT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRCIW(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRCIW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRCIA(1:LFDG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRCIA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRCIO(1:LFDNH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRCIO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRCIS(1:LFDS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRCIS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRCID(1:LFDD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRCID'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PLWB(1:LSV,1:LSR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PLWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PGW(1:LSV,1:LSR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PGW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( QLW(1:3,1:LSR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: QLW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( QNW(1:3,1:LSR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: QNW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( QTW(1:3,1:LSR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: QTW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( GWSI(1:3,1:LWSI,1:LSR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: GWSI'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SWSI(1:4,1:LWSI,1:LSR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SWSI'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IWSI(1:LWSI,1:LSR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IWSI'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISRDM(1:13,1:LSR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISRDM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISRM(1:LSR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISRM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISRT(1:LSR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISRT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NSOLSR(1:LSR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NSOLSR'
        CALL WRMSGS( INDX )
      ENDIF
!
!--- Spill variable allocation  ---
!
      ALLOCATE( ULSP(1:LSFV,1:(LFX+1)*LFY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ULSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VLSP(1:LSFV,1:(LFY+1)*LFX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VLSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOLSP(1:LSV,1:LFX*LFY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOLSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISLSP(1:LSV,1:LFX*LFY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISLSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HLSP(1:LSV,1:LFX*LFY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HLSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DNRSP(1:LUK,1:LFX*LFY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DNRSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UNSP(1:LSFV,1:(LFX+1)*LFY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UNSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VNSP(1:LSFV,1:(LFY+1)*LFX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VNSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHONSP(1:LSV,1:LFX*LFY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHONSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISNSP(1:LSV,1:LFX*LFY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISNSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HNSP(1:LSV,1:LFX*LFY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HNSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRCOSP(1:LSV,1:LFX*LFY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRCOSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRCWSP(1:LSV,1:LFX*LFY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRCWSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SPNORM(1:LFX*LFY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SPNORM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SPHMIN(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SPHMIN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TBLX(1:LTBL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TBLX'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TBLY(1:LTBL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TBLY'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TBLDDX(1:LTBL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TBLDDX'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TBLDDY(1:LTBL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TBLDDY'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( C(1:LFDCR,1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: C'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CO(1:LFDCR,1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CNL(1:LFDC,1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CNL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CNLO(1:LFDC,1:LSOLU),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CNLO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SMDEF(1:LRC,1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SMDEF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UC(1:LSXC,1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VC(1:LSYC,1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WC(1:LSZC,1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UCN(1:LSXC,1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UCN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VCN(1:LSYC,1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VCN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( WCN(1:LSZC,1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WCN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HLF(1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HLF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHLF(1:LSOLU,1:LCHEM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHLF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SRCIC(1:LFDCR,1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SRCIC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SMDL(1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SMDL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SMDN(1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SMDN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SMDG(1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SMDG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SDCL(1:3,1:LRC,1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SDCL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SDCLS(1:3,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SDCLS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YL(1:LFDCR,1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YG(1:LFDCR,1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YN(1:LFDCR,1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PCSL(1:5,1:LRC,1:LSOLU),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PCSL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PCSN(1:5,1:LRC,1:LSOLU),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PCSN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PCSLD(1:2,1:LRCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PCSLD'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CMTLN(1:4,1:LSOLU),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CMTLN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PCLN(1:5,1:LSOLU),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PCLN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PCGL(1:5,1:LSOLU),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PCGL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PCGN(1:5,1:LSOLU),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PCGN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CB(1:LBCC,1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CBO(1:LBCC,1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CBO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YLB(1:LBCC,1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YLB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YGB(1:LBCC,1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YGB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YNB(1:LBCC,1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YNB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DISPL(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DISPL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DISPT(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DISPT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RCHDF(1:LSOLU,1:LSOLU,1:LCHEM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RCHDF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SOLML(1:LSOLU),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SOLML'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RCHDFL(1:LSOLU,1:LSOLU,1:LCHEM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RCHDFL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RCHDFN(1:LSOLU,1:LSOLU,1:LCHEM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RCHDFN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHLFL(1:LSOLU,1:LCHEM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHLFL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHLFN(1:LSOLU,1:LCHEM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHLFN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ELC_DCF(1:4),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ELC_DCF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ELC_VCF(1:4),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ELC_VCF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ELC_SCF(1:4),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ELC_SCF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CCL_CRN(1:LSOLU),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CCL_CRN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DPLGS(1:LRCS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DPLGS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DPTRS(1:LRCS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DPTRS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DPLD(1:LRCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DPLD'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( D50(1:LRCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: D50'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DPTD(1:LRCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DPTD'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PMDD(1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PMDD'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CHDF(1:LSOLU,1:LSOLU),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CHDF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SOLUT(1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SOLUT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CRNTL(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CRNTL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CRNTG(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CRNTG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CRNTN(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CRNTN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IEDL(1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IEDL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IPCL(1:LSOLU),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IPCL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IPCN(1:LSOLU),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IPCN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IPCSL(1:LRC,1:LSOLU),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IPCSL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IPCSLD(1:LRCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IPCSLD'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ICT(1:LFDCR,1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ICT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ICTN(1:LFDC,1:LSOLU),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ICTN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NCHEM(1:LSOLU),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NCHEM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IPCLN(1:LSOLU),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IPCLN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IPCGL(1:LSOLU),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IPCGL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IPCGN(1:LSOLU),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IPCGN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IMTLN(1:LSOLU),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IMTLN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( N_CRN(1:LSOLU+1),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: N_CRN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( T(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: T'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PL(1:LSV,1:LFDL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PG(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PN(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DNR(1:LUK,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DNR'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PSO(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PSO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SL(1:LSV,1:LFDL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SG(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SN(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PORD(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PORD'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PORT(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PORT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PSW(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PSW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PVA(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PVA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PVO(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PVO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PVW(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PVW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XGA(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XGA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XGO(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XGO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XGW(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XGW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMGA(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMGA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMGO(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMGO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMGW(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMGW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLA(1:LSV,1:LFDL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLO(1:LSV,1:LFDL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLW(1:LSV,1:LFDL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMLA(1:LSV,1:LFDL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMLA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMLO(1:LSV,1:LFDL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMLO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMLW(1:LSV,1:LFDL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMLW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOL(1:LSV,1:LFDL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOG(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHON(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHON'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISL(1:LSV,1:LFDL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TORL(1:LSV,1:LFDL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TORL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RKL(1:3,1:LSV,1:LFDL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RKL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFLO(1:LSV,1:LFDL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFLO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFLN(1:LSV,1:LFDN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFLN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFLA(1:LSV,1:LFDL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFLA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFLS(1:LSV,1:LFDL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFLS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOMG(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOMG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SI(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SI'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PI(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PI'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOML(1:LSV,1:LFDL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOML'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOMN(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOMN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SDPM(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SDPM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SDPF(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SDPF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TMS(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TMS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( POSM(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: POSM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( BTGL(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: BTGL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PERMRF(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PERMRF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PCMP(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PCMP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TCMP(1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TCMP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( POR0(1:3,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: POR0'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PERMV(1:3,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PERMV'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SCHRV(1:1,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SCHRV'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( THKL(1:LSV,1:LFDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: THKL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( THKN(1:LSV,1:LFDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: THKN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HG(1:LSV,1:LFDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HL(1:LSV,1:LFDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HN(1:LSV,1:LFDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HGA(1:LSV,1:LFDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HGA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HGD(1:LSV,1:LFDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HGD'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HGO(1:LSV,1:LFDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HGO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HGW(1:LSV,1:LFDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HGW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HLA(1:LSV,1:LFDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HLA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HLO(1:LSV,1:LFDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HLO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HLS(1:LSV,1:LFDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HLS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HLW(1:LSV,1:LFDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HLW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( THKG(1:LSV,1:LFDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: THKG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UEG(1:LSV,1:LFDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UEG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UEL(1:LSV,1:LFDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UEL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UEN(1:LSV,1:LFDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UEN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( GNIFT(1:LSV,1:LFDG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: GNIFT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISG(1:LSV,1:LFDG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISDG(1:LSV,1:LFDG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISDG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TORG(1:LSV,1:LFDG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TORG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RKG(1:LSV,1:LFDG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RKG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFGW(1:LSV,1:LFDG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFGW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFGA(1:LSV,1:LFDG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFGA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFGO(1:LSV,1:LFDG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFGO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ICAIR(1:LFDG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ICAIR'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ICCO2(1:LFDG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ICCO2'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TMBP_A(1:LSV,1:LFDG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TMBP_A'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TMBP_N(1:LSV,1:LFDN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TMBP_N'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TMBP_O(1:LSV,1:LFDG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TMBP_O'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISN(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TORN(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TORN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RKN(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RKN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XSO(1:LSV,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XSO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ANLT(1:LFDN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ANLT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ANLF(1:LFDN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ANLF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HKL(1:LFDN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HKL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HKNT(1:LFDN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HKNT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HKNF(1:LFDN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HKNF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOI(1:LSV,1:LFDI),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOI'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( THKI(1:LSV,1:LFDI),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: THKI'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HI(1:LSV,1:LFDI),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HI'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLS(1:LSV,1:LFDS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMLS(1:LSV,1:LFDS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMLS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( OEC(1:LSV,1:LFDS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: OEC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YLS(1:LSV,1:LFDS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YLS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SS(1:LSV,1:LFDS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOSP(1:LSV,1:LFDS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HSP(1:LSV,1:LFDS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ICBRN(1:LFDS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ICBRN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TRPNL(1:LSU,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TRPNL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TRPGL(1:LSU,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TRPGL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( AVPVG(1:LFDD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: AVPVG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( AVPVL(1:LFDD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: AVPVL'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Nonaqueous liquid field variables  ---
!
      ALLOCATE( XNA(1:LSV,1:LFDA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XNA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XNN(1:LSV,1:LFDA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XNN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XNO(1:LSV,1:LFDA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XNO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XNW(1:LSV,1:LFDA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XNW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMNA(1:LSV,1:LFDA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMNA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMNN(1:LSV,1:LFDA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMNN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMNO(1:LSV,1:LFDA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMNO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMNW(1:LSV,1:LFDA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMNW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SUGL(1:LSV,1:LFDA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SUGL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SUGN(1:LSV,1:LFDA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SUGN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SUNL(1:LSV,1:LFDA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SUNL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFNA(1:LSV,1:LFDA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFNA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFNN(1:LSV,1:LFDA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFNN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFNO(1:LSV,1:LFDA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFNO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFNW(1:LSV,1:LFDA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFNW'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Gas component field variables  ---
!
      ALLOCATE( BETA(1:6,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: BETA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( FK(1:LNGC,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: FK'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PVC(1:LNGC,1:LSV,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PVC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XGC(1:LNGC,1:LSV,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XGC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMGC(1:LNGC,1:LSV,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMGC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XNC(1:LNGC,1:LSV,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XNC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMNC(1:LNGC,1:LSV,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMNC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMVGC(1:LNGC,1:LSV,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMVGC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMVGW(1:LSV,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMVGW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMVLC(1:LNGC,1:LSV,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMVLC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMVLW(1:LSV,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMVLW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLC(1:LNGC,1:LSV,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMLC(1:LNGC,1:LSV,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMLC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFLC(1:LNGC,1:LSV,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFLC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFGC(1:LNGC,1:LSV,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFGC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFNC(1:LNGC,1:LSV,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFNC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HGC(1:LNGC,1:LSV,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HGC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ICCOMP(1:LNGC,1:LFDG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ICCOMP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TMC(1:LNGC,1:LSV,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TMC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZG(1:LSV,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZMC(1:LNGC,1:LSV,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZMC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZN(1:LSV,1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IZMC(1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IZMC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IBETA(1:LFDGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IBETA'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Ternary hydrate variables  ---
!
      ALLOCATE( ZPC_HT(1:LHF_HT,1:LCP_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZPC_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZHC_HT(1:LHF_HT,1:LCH_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZHC_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TCR_HT(1:LCP_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TCR_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PCR_HT(1:LCP_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PCR_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TCB_HT(1:LCP_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TCB_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PCB_HT(1:LCP_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PCB_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TCT_HT(1:LCP_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TCT_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PCT_HT(1:LCP_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PCT_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TLE_HT(1:LPE_HT,1:LCP_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TLE_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PLE_HT(1:LPE_HT,1:LCP_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PLE_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( D2PLE_HT(1:LPE_HT,1:LCP_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: D2PLE_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( FKLE_HT(1:LPE_HT,1:LCP_HT,1:LHF_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: FKLE_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( D2FKLE_HT(1:LPE_HT,1:LCP_HT,1:LHF_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: D2FKLE_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TUE_HT(1:LPE_HT,1:LCP_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TUE_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PUE_HT(1:LPE_HT,1:LCP_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PUE_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( D2PUE_HT(1:LPE_HT,1:LCP_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: D2PUE_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( FKUE_HT(1:LPE_HT,1:LCP_HT,1:LHF_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: FKUE_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( D2FKUE_HT(1:LPE_HT,1:LCP_HT,1:LHF_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: D2FKUE_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( T2P_HT(1:LTP_HT,1:LCP_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: T2P_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( P2P_HT(1:LPP_HT,1:LCP_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: P2P_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( B2P_HT(1:LTP_HT,1:LPP_HT,1:LCP_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: B2P_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( D2B2P_HT(1:LTP_HT,1:LPP_HT,1:LCP_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: D2B2P_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( FK2P_HT(1:LTP_HT,1:LPP_HT,1:LCP_HT,1:LHF_HT),
     &  STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: FK2P_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( D2FK2P_HT(1:LTP_HT,1:LPP_HT,1:LCP_HT,1:LHF_HT),
     &  STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: D2FK2P_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( THE_HT(1:LHE_HT,1:LCH_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: THE_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PHE_HT(1:LHE_HT,1:LCH_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PHE_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XSCA_HT(1:LHE_HT,1:LCH_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XSCA_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XSCO_HT(1:LHE_HT,1:LCH_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XSCO_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XSCN_HT(1:LHE_HT,1:LCH_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XSCN_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLCA_HT(1:LHE_HT,1:LCH_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLCA_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLCO_HT(1:LHE_HT,1:LCH_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLCO_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLCN_HT(1:LHE_HT,1:LCH_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLCN_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( THE2P_HT(1:LHE_HT,1:LCH_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: THE2P_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XSCA2T_HT(1:LHE_HT,1:LCH_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XSCA2T_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XSCO2T_HT(1:LHE_HT,1:LCH_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XSCO2T_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XSCN2T_HT(1:LHE_HT,1:LCH_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XSCN2T_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLCA2T_HT(1:LHE_HT,1:LCH_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLCA2T_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLCO2T_HT(1:LHE_HT,1:LCH_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLCO2T_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLCN2T_HT(1:LHE_HT,1:LCH_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLCN2T_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PHE2T_HT(1:LHE_HT,1:LCH_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PHE2T_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZMIH_HT(1:LHF_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZMIH_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NPEP_HT(1:LCP_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NPEP_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NHEP_HT(1:LCH_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NHEP_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NTP_HT(1:LCP_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NTP_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NPP_HT(1:LCP_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NPP_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IZP_HT(1:LCN_HT,1:LCN_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IZP_HT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IZH_HT(1:LCN_HT,1:LCN_HT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IZH_HT'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Hydrate field variables  ---
!
      ALLOCATE( HCPP(1:30,1:LNHC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HCPP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XHW(1:LSV,1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XHW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XHA(1:LSV,1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XHA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XHO(1:LSV,1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XHO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XHN(1:LSV,1:LFDN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XHN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PVHN(1:LSV,1:LFDN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PVHN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PVN(1:LSV,1:LFDN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PVN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XGN(1:LSV,1:LFDN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XGN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMGN(1:LSV,1:LFDN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMGN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFGN(1:LSV,1:LFDN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFGN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PPEL(1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PPEL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PPEU(1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PPEU'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TCR(1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TCR'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TCT(1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TCT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLN(1:LSV,1:LFDN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMLN(1:LSV,1:LFDN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMLN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOH(1:LSV,1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( THKH(1:LSV,1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: THKH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HH(1:LSV,1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SH(1:LSV,1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PH(1:LSV,1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UEGA(1:LSV,1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UEGA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YMGA(1:LSV,1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YMGA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YMHGA(1:LSV,1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YMHGA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YMGO(1:LSV,1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YMGO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YMHGO(1:LSV,1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YMHGO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YMGN(1:LSV,1:LFDN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YMGN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YMHGN(1:LSV,1:LFDN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YMHGN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PVHA(1:LSV,1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PVHA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PVHO(1:LSV,1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PVHO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TMHA(1:LSV,1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TMHA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TMHN(1:LSV,1:LFDN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TMHN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TMHO(1:LSV,1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TMHO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZMCA(1:LSV,1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZMCA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZMCN(1:LSV,1:LFDN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZMCN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZMCO(1:LSV,1:LFDH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZMCO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CHKN(1:10),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CHKN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IC_OPT(1:15,1:LFD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IC_OPT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( BIPC(1:LNGC,1:LNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: BIPC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( BIPF(1:LPF_EOR+2,1:LPF_EOR+2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: BIPF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PFPP(1:30,1:LPF_EOR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PFPP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( GC_MMP(1:LNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: GC_MMP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMPCF(1:LPCF,1:LNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMPCF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PFNM(1:LPF_EOR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PFNM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IPCF(1:LPCF,1:LNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IPCF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NPCF(1:LNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NPCF'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( R_OBDS(1:2,1:LOBDS,1:LOBDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: R_OBDS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( R_OBDT(1:6,1:LOBDS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: R_OBDT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( C_OBDT(1:LOBDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: C_OBDT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( I_OBDT(1:9,1:LOBDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: I_OBDT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NOBDS(1:LOBDT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NOBDS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( P_TA(1:LP_TA,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: P_TA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( T_TA(1:LT_TA,1:LP_TA,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: T_TA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHO_TA(1:LT_TA,1:LP_TA,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHO_TA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( H_TA(1:LT_TA,1:LP_TA,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: H_TA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( U_TA(1:LT_TA,1:LP_TA,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: U_TA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( FUG_TA(1:LT_TA,1:LP_TA,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: FUG_TA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( S_TA(1:LT_TA,1:LP_TA,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: S_TA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHO_ST(1:LT_TA,1:LP_TA,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHO_ST'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( H_ST(1:LT_TA,1:LP_TA,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: H_ST'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( U_ST(1:LT_TA,1:LP_TA,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: U_ST'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( FUG_ST(1:LT_TA,1:LP_TA,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: FUG_ST'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( S_ST(1:LT_TA,1:LP_TA,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: S_ST'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( T_TH(1:LT_TH,1:LO_TH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: T_TH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YMHO_TH(1:LT_TH,1:LO_TH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YMHO_TH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XSCA_TH(1:LT_TH,1:LO_TH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XSCA_TH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLCA_TH(1:LT_TH,1:LO_TH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLCA_TH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XSCO_TH(1:LT_TH,1:LO_TH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XSCO_TH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLCO_TH(1:LT_TH,1:LO_TH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLCO_TH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( P_TH(1:LT_TH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: P_TH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YMGO_TH(1:LO_TH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YMGO_TH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( P_PH(1:LT_PH,1:LO_PH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: P_PH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YMHO_PH(1:LT_PH,1:LO_PH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YMHO_PH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XSCA_PH(1:LT_PH,1:LO_PH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XSCA_PH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLCA_PH(1:LT_PH,1:LO_PH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLCA_PH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XSCO_PH(1:LT_PH,1:LO_PH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XSCO_PH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLCO_PH(1:LT_PH,1:LO_PH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLCO_PH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( T_PH(1:LT_PH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: T_PH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YMGO_PH(1:LO_PH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YMGO_PH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( T_LV(1:L_LV,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: T_LV'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( P_LV(1:L_LV,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: P_LV'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOL_LV(1:L_LV,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOL_LV'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HL_LV(1:L_LV,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HL_LV'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UL_LV(1:L_LV,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UL_LV'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOV_LV(1:L_LV,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOV_LV'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HV_LV(1:L_LV,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HV_LV'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UV_LV(1:L_LV,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UV_LV'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( FUG_LV(1:L_LV,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: FUG_LV'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SL_LV(1:L_LV,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SL_LV'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SV_LV(1:L_LV,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SV_LV'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CINH(1:6,1:LINH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CINH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLIMX(1:LINH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLIMX'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( INHNM(1:LINH),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: INHNM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IP_TA(1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IP_TA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IPTP(1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IPTP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IPCR(1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IPCR'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IT_TA(1:LP_TA,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IT_TA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IV_TA(1:LP_TA,1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IV_TA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IC_NCG(1:LNNGC,1:LFDG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IC_NCG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( I_LV(1:LNNGC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: I_LV'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  [REACT] module variables  ---
!
      ALLOCATE( ACTV16(1:6),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ACTV16'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( C_PH(1:LFDR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: C_PH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CHARG(1:LSPR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CHARG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( FACTV(1:LSPR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: FACTV'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ACTVS(1:LSPR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ACTVS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( EQ_C(1:LSEC,1:LEQC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: EQ_C'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( EQ_E(1:LSEE,1:LEQE),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: EQ_E'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( EQ_K(1:LSEK+LREK,1:LEQK),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: EQ_K'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RC_E(1:5,1:LRCE),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RC_E'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RC_K(1:LSPK+11,1:LCKN,1:LRCK),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RC_K'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SP_C(1:LFDR,1:LSPR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SP_C'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SP_CO(1:LFDR,1:LSPR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SP_CO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SP_CI(1:LFDR,1:LSPR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SP_CI'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SP_CMN(1:LFDR,1:LSPS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SP_CMN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SP_RATE(1:LFDR,1:LSPS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SP_RATE'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SP_AREA(1:LFDR,1:LSPS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SP_AREA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SP_CBO(1:LBCC,1:LSPR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SP_CBO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IC_SP(1:LFDR,1:LSPR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IC_SP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISP_MN(1:LSPR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISP_MN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SP_SDCL(1:3),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SP_SDCL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SP_L(1:3,1:LSPL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SP_L'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SP_S(1:2,1:LSPS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SP_S'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SPNME(1:LSPE),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SPNME'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISP_E(1:LSPE),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISP_E'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RS_S(1:3,1:LSPS,1:LFDR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RS_S'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YSPG(1:LFDRG,1:(LEQC+LEQK)),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YSPG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YSPL(1:LFDRL,1:(LEQC+LEQK)),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YSPL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YSPN(1:LFDRN,1:(LEQC+LEQK)),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YSPN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SPNMC(1:LEQC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SPNMC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SPNMK(1:LEQK),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SPNMK'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SPNMG(1:LSPG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SPNMG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SPNML(1:LSPL),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SPNML'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SPNMN(1:LSPN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SPNMN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SPNMS(1:LSPS),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SPNMS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RCNME(1:LRCE),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RCNME'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RCNMK(1:LRCK),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RCNMK'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IEQ_C(1:LSEC+1,1:LEQC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IEQ_C'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IEQ_E(1:LSEE+2,1:LEQE),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IEQ_E'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IEQ_K(1:LSEK+LREK+2,1:LEQK),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IEQ_K'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IEQ_S(1:LSPR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IEQ_S'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IEL_LK(1:LSPE),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IEL_LK'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISP_S(1:LEQE+LEQC+LEQK),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISP_S'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IRC_K(1:LSPK+3,1:LRCK),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IRC_K'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IRCKT(1:LRCK),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IRCKT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IRCKN(LSPK+11),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IRCKN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISPLK(1:2*LNGC+LSPR+14),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISPLK'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISP_OW(1:LSPS,1:LFDR),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISP_OW'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IMMB(1:LSOLU+LSPT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IMMB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( CFMX(1:LMC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CFMX'
        CALL WRMSGS( INDX )
      ENDIF
!
!--- Pitzer activity coefficients
!
      ALLOCATE(JPA(LANI))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: JPA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(JPC(LCAT))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: JPC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(JPN(LNEU))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: JPN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(B0(LCAT,LANI))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: B0'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(B1(LCAT,LANI))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: B1'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(B2(LCAT,LANI))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: B2'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(CMXX(LCAT,LANI))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CMXX'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(TCC(LNCF))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TCC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(TAA(LNAF))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TAA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(PSIC(LNCF,LANI))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PSIC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(PSIA(LNAF,LCAT))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PSIA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(ALAMB(LNEU,LANI))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ALAMB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(CLAMB(LNEU,LCAT))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CLAMB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(ELAMB(LNEU,LNEU))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ELAMB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(HOLAMB(LNNF,LANI))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HOLAMB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(BPPR(LCAT,LANI))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: BPPR'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(BPHI(LCAT,LANI))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: BPHI'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(BPR(LCAT,LANI))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: BPR'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(BMMX(LCAT,LANI))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: BMMX'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(ATB0(LCAT,LANI,8))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ATB0'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(ATB1(LCAT,LANI,8))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ATB1'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(ATB2(LCAT,LANI,8))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ATB2'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(ATCMX(LCAT,LANI,8))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ATCMX'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(ATNLAM(LNEU,LNEU,6))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ATNLAM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(ATCLAM(LNEU,LCAT,6))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ATCLAM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(ATALAM(LNEU,LANI,6))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ATALAM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(ATHLAM(LNNF,LANI,6))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ATHLAM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(ATTC(LNCF,6))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ATTC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(ATPC(LNCF,LANI,6))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ATPC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(ATTA(LNAF,6))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ATTA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(ATPA(LNAF,LCAT,6))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ATPA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(CTCPH(LNCF))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CTCPH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(CTC(LNCF))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CTC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(CTCPR(LNCF))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CTCPR'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(CTCPPR(LNCF))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CTCPPR'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(CTAPH(LNAF))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CTAPH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(CTA(LNAF))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CTA'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(CTAPR(LNAF))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CTAPR'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(CTAPPR(LNAF))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: CTAPPR'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(ETH(LMCG*LMCG))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ETH'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(ETHP(LMCG*LMCG))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ETHP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(ETHP2(LMCG*LMCG))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ETHP2'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE(IDD_PZ(LSPL))
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IDD_PZ'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Spill Grid Variables  ---
!                  
      ALLOCATE( XSP(1:LFX*LFY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YSP(1:LFX*LFY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZSP(1:LFX*LFY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( AFZSP(1:LFX*LFY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: AFZSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TXSP(1:((LFX+1)*LFY)),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TXSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TYSP(1:((LFY+1)*LFX)),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TYSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PDXSP(1:((LFX+1)*LFY)),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PDXSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PDYSP(1:((LFY+1)*LFX)),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PDYSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DXSP(1:((LFY+1)*LFX)),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DXSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DYSP(1:((LFX+1)*LFY)),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DYSP'
        CALL WRMSGS( INDX )
      ENDIF

      ISUB_LOG = ISUB_LOG-1
!
!---  End of ALLOC group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ALLOC_BCV
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
!     Allocate memory for the BCV module
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 19 December 2015.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE BCV
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL(KIND=DP) (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ALLOC_BCV'
!
!---  Allocate memory for the BCV module  ---
!
      ALLOCATE( BC(1:LBCV,1:LBTM,1:LBCIN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: BC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PHDL(1:2,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PHDL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PHDN(1:2,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PHDN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IBCD(1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IBCD'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IBCN(1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IBCN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IBCT(1:LUK+LPH*LSOLU*LC+LR+LL+LG+LN,1:LBC),
     &  STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IBCT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IBCM(1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IBCM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IBCC(1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IBCC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IBCIN(1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IBCIN'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IBCSP(1:LSPBC+1,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IBCSP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IBCLL(1:LBCIN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IBCLL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( JBCLL(1:LBCIN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: JBCLL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( KBCLL(1:LBCIN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: KBCLL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( MBCLL(1:LBCIN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: MBCLL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( BCXYZG(1:LBTM,1:3),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: BCXYZG'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XPBC(1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XPBC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YPBC(1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YPBC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZPBC(1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZPBC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NBHG(1:LPH,1:LBC),
     &  STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NBHG'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ALLOC_BCV group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ALLOC_BCVG
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
!     Allocate memory for the BCVG module
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 19 December 2015.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE BCVG
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL(KIND=DP) (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ALLOC_BCVG'
!
!---  Allocate memory for the BCVG module  ---
!
      ALLOCATE( TORGB(1:LSV,1:LBCG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TORGB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISGB(1:LSV,1:LBCG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISGB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RKGB(1:LSV,1:LBCG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RKGB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFGWB(1:LSV,1:LBCG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFGWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFGAB(1:LSV,1:LBCG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFGAB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFGOB(1:LSV,1:LBCG),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFGOB'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ALLOC_BCVG group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ALLOC_BCVN
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
!     Allocate memory for the BCVN module
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 19 December 2015.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE BCVN
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL(KIND=DP) (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ALLOC_BCVN'
!
!---  Allocate memory for the BCVN module  ---
!
      ALLOCATE( TORNB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TORNB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISNB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISNB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RKNB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RKNB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XSOB(1:LSV,1:LBCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XSOB'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ALLOC_BCVN group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ALLOC_BCVP
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
!     Allocate memory for the BCVP module
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 19 December 2015.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE BCVP
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL(KIND=DP) (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ALLOC_BCVP'
!
!---  Allocate memory for the BCVP module  ---
!
      ALLOCATE( TB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PLB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PLB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PGB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PGB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PNB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PNB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PSOB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PSOB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PSWB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PSWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SLB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SLB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SGB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SGB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SNB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SNB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PORDB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PORDB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PORTB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PORTB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOMGB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOMGB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PVAB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PVAB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PVOB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PVOB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PVWB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PVWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XGAB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XGAB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XGOB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XGOB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XGWB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XGWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMGAB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMGAB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMGOB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMGOB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMGWB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMGWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLAB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLAB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLOB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLOB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLWB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMLWB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMLWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMLAB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMLAB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMLOB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMLOB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOLB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOLB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOGB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOGB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHONB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHONB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TORLB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TORLB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISLB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISLB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RKLB(1:3,1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RKLB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFLOB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFLOB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFLNB(1:LSV,1:LBCN2),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFLNB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFLAB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFLAB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFLSB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFLSB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SIB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SIB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PIB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PIB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SDPMB(1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SDPMB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SDPFB(1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SDPFB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TMSB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TMSB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( POSB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: POSB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( BTGLB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: BTGLB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOMLB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOMLB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOMNB(1:LSV,1:LBC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOMNB'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ALLOC_BCVP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ALLOC_BCVT
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
!     Allocate memory for the BCVP module
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 19 December 2015.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE BCVT
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL(KIND=DP) (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ALLOC_BCVT'
!
!---  Allocate memory for the BCVT module  ---
!
      ALLOCATE( THKLB(1:LSV,1:LBCT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: THKLB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( THKNB(1:LSV,1:LBCT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: THKNB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HLB(1:LSV,1:LBCT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HLB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HGB(1:LSV,1:LBCT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HGB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HNB(1:LSV,1:LBCT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HNB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UEGB(1:LSV,1:LBCT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UEGB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HGAB(1:LSV,1:LBCT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HGAB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HGOB(1:LSV,1:LBCT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HGOB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HGWB(1:LSV,1:LBCT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HGWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HLAB(1:LSV,1:LBCT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HLAB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HLOB(1:LSV,1:LBCT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HLOB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HLSB(1:LSV,1:LBCT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HLSB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HLWB(1:LSV,1:LBCT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HLWB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( THKGB(1:LSV,1:LBCT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: THKGB'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ALLOC_BCVT group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ALLOC_DUAL_POR
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
!     Allocate memory for the DUAL_POR module
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 19 December 2015
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE DUAL_POR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL(KIND=DP) (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ALLOC_DUAL_POR'
!
!---  Allocate memory for the DUAL_POR module  ---
!
      ALLOCATE( FRAC_P(1:17,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: FRAC_P'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VOL_M(LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VOL_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DNR_M(1:LUK,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DNR_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFGC_M(1:LNGC,1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFGC_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFNC_M(1:LNGC,1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFNC_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TMC_M(1:LNGC,1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TMC_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XGC_M(1:LNGC,1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XGC_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMGC_M(1:LNGC,1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMGC_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XNC_M(1:LNGC,1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XNC_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMNC_M(1:LNGC,1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMNC_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZMC_M(1:LNGC,1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZMC_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFGW_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFGW_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFLA_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFLA_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( DFLS_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFLS_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HG_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HG_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HGA_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HGA_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HGW_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HGW_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HL_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HL_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HLW_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HLW_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HN_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HN_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HSP_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HSP_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UEG_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UEG_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UEL_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UEL_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UEN_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UEN_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( THKG_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: THKG_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( THKL_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: THKL_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( THKN_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: THKN_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PERMRF_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PERMRF_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PG_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PG_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PL_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PL_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PN_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PN_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( POSM_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: POSM_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PSO_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PSO_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PORD_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PORD_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PORT_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PORT_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PVA_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PVA_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PVW_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PVW_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOG_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOG_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOL_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOL_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOMG_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOMG_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOML_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOML_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOMN_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOMN_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHON_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHON_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOSP_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOSP_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RKG_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RKG_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RKL_M(1:3,1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RKL_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RKN_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RKN_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SG_F(1:LSV,1:LFD_EC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SG_F'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SL_F(1:LSV,1:LFD_EC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SL_F'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SN_F(1:LSV,1:LFD_EC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SN_F'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SG_M(1:LSV,1:LFD_EC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SG_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SL_M(1:LSV,1:LFD_EC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SL_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SN_M(1:LSV,1:LFD_EC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SN_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SS_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SS_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( T_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: T_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TMS_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TMS_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TORG_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TORG_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TORL_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TORL_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TORN_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TORN_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISG_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISG_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISL_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISL_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISN_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISN_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XGW_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XGW_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLA_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLA_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLS_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLS_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLW_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLW_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMGW_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMGW_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMLA_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMLA_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMLS_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMLS_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMLW_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMLW_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YLS_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YLS_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZG_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZG_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZN_M(1:LSV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZN_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( POR0_M(1:3,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: POR0_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NPHAZ_M(1:LSU,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NPHAZ_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IM_M(1:LUK,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IM_M'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( QLA_FM(1:LSFV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: QLA_FM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( QGW_FM(1:LSFV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: QGW_FM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( QLW_FM(1:LSFV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: QLW_FM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( QGC_FM(1:LNGC,1:LSFV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: QGC_FM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( QNC_FM(1:LNGC,1:LSFV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: QNC_FM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( QS_FM(1:LSFV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: QS_FM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( QQ_FM(1:LSFV,1:LFD_DP),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: QQ_FM'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ALLOC_DUAL_POR group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ALLOC_GEOMECH
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
!     Allocate memory for the GEOMECH module
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 2 November 2016
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GEOMECH
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL(KIND=DP) (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ALLOC_GEOMECH'
!
!---  Allocate memory for the GEOMECH module  ---
!
      ALLOCATE( BC_GM(1:LBCV_GM,1:LBTM_GM,1:LBCIN_GM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: BC_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PROP_GM(1:9,1:LRC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PROP_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( EPS_GM(1:6,1:LFDM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: EPS_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SIG_GM(1:6,1:LFDM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SIG_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( P_GM(1:3,1:LFDM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: P_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SIGV_GM(1:3,1:LFDM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SIGV_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SIGV_CMP(1:LFDM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SIGV_CMP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RSDM_GM(1:LEPD),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RSDM_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( U_GM(1:2,1:LFEN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: U_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( V_GM(1:2,1:LFEN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: V_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( W_GM(1:2,1:LFEN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: W_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IBCT_GM(1:3,1:LBC_GM),
     &  STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IBCT_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IBCM_GM(1:LBC_GM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IBCM_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IBCC_GM(1:LBC_GM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IBCC_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IBCN_GM(1:LBC_GM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IBCN_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IBCIN_GM(1:LBC_GM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IBCIN_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IBCD_GM(1:LBC_GM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IBCD_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IBCR_GM(1:LBC_GM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IBCR_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IM_GM(1:LFEN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IM_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ND_GM(1:8,1:LFDM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ND_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NE_GM(1:8,1:LFEN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NE_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NK_GM(1:8,1:8),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NK_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( KLU_GM(1:LJG_GM,1:LJH_GM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: KLU_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( MLU_GM(1:LJO_GM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: MLU_GM'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NLU_GM(1:LJC_GM),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NLU_GM'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ALLOC_GEOMECH group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ALLOC_PLT_ATM
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
!     Allocate memory for the DUAL_POR module
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 19 December 2015
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE PLT_ATM
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL(KIND=DP) (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ALLOC_PLT_ATM'
!
!---  Allocate memory for the PLT_ATM module  ---
!
      ALLOCATE( PARMS_P(1:29,1:LPLANT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PARMS_P'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Stored water mass on plant leaves, kg/m^2 ground  ---
!
      ALLOCATE( SWM_PL(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SWM_PL'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Root-water uptake  ---
!
      ALLOCATE( RWU_SFC(1:6+LUK,1:LSFCN*LSFCC),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RWU_SFC'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Transpiration  ---
!
      ALLOCATE( TP_SFC(1:6,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TP_SFC'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Atmospheric conditions  ---
!
      ALLOCATE( ATMOS(1:LATM,1:7),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ATMOS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ATMC(1:7),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ATMC'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Surface albedo parameters  ---
!
      ALLOCATE( ALBEDO(1:5,1:LSFCA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ALBEDO'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PARMS_SFC(1:(LPLANT*2+1),1:LSFCT,1:LSFCA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PARMS_SFC('
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Primary variable increments  ---
!
      ALLOCATE( DNR_SFC(1:5,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DNR_SFC'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Residual for canopy water mass balance  ---
!
      ALLOCATE( RW_CP(1:(6+LUK*LSFCC),1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RW_CP'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Residual for canopy energy balance  ---
!
      ALLOCATE( RE_CP(1:(6+LUK*LSFCC),1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RE_CP'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Residual for plant energy balance  ---
!
      ALLOCATE( RE_PL(1:(6+LUK*LSFCC),1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RE_PL'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Residual for ground surface water mass balance  ---
!
      ALLOCATE( RW_GS(1:LUK+6,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RW_GS'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Residual for ground surface energy balance  ---
!
      ALLOCATE( RE_GS(1:LUK+6,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RE_GS'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Gas water diffusion coefficient ground surface  ---
!
      ALLOCATE( DFGW_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFGW_GS'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Aqueous air diffusion coefficient ground surface  ---
!
      ALLOCATE( DFLA_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DFLA_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HG_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HG_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HGA_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HGA_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HGW_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HGW_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HL_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HL_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HLW_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HLW_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( HLW_PL(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: HLW_PL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOG_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOG_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOMG_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOMG_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISG_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISG_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOL_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOL_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RHOML_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RHOML_GS'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Accumulated water mass on all plant surfaces  ---
!
      ALLOCATE( ACW_PL(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ACW_PL'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Plant water mass for all plants  ---
!
      ALLOCATE( WM_PL(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: WM_PL'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Plant dry mass for all plants  ---
!
      ALLOCATE( DM_PL(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: DM_PL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( VISL_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VISL_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PHMX(1:LSFCA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PHMX'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PVA_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PVA_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PVW_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PVW_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RKL_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RKL_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( RKG_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: RKG_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SL_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SL_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( SG_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: SG_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( T_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: T_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TORG_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TORG_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( TORL_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: TORL_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( UEG_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: UEG_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( THKG_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: THKG_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( THKL_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: THKL_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PL_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PL_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( T_PL(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: T_PL'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XGA_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XGA_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XGW_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XGW_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLA_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLA_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XLW_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XLW_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMLW_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMLW_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMLA_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMLA_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMGA_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMGA_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XMGW_GS(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XMGW_GS'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PVW_CP(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PVW_CP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( T_CP(1:7,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: T_CP'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IRSM_P(1:LPLANT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IRSM_P'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( PLANT(1:LPLANT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: PLANT'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ITMP_P(1:LPLANT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ITMP_P'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IALB(1:LSFCA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IALB'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( IALB_P(1:LPLANT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: IALB_P'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ISRM_P(1:LPLANT),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ISRM_P'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NAME_SFC(1:LSFCA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NAME_SFC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ID_SFC(1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ID_SFC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ICM_SFC(1:LSFCC,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ICM_SFC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( JM_SFC(1:5,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: JM_SFC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( KLU_SFC(1:LUK_SFC,1:LSFCN),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: KLU_SFC'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( NSFCT(1:LSFCA),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: NSFCT'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ALLOC_PLT_ATM group  ---
!
      RETURN
      END

