!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SFIN
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
!     Surface flux and source term integrator
!
!   1 heat flux - UQV, VQV, WQV
!   2   aqueous volumetric flux - ULV, VLV, WLV
!   3   gas volumetric flux - UGV, VGV, WGV
!   4   nonaqueous-liquid volumetric flux - UNV, VNV, WNV
!   5   aqueous mass flux - ULM, VLM, WLM
!   6   gas mass flux - UGM, VGM, WGM
!   7   nonaqueous-liquid mass flux - UNM, VNM, WNM
!   8   salt mass flux - USM, VSM, WSM
!   9   dissolved-oil mass flux - ULO, VLO, WLO
!  10   condensate water mass flux - UWM, VWM, WWM
!  11   gas oil mass flux - UGOM, VGOM, WGOM
!  12   aqueous oil mass flux
!       or aqueous CH4 mass flux - ULOM, VLOM, WLOM
!  13   total oil mass flux
!       or total CH4 mass flux - UOM, VOM, WOM
!  20   gas-advective heat flux - UGAQ, VGAQ, WGAQ
!  21   gas-advective water-mass flux - UGAW, VGAW, WGAW
!  22   gas-advective air-mass flux - UGAA, VGAA, WGAA
!  25   gas-diffusive heat flux - UGDQ, VGDQ, WGDQ
!  26   gas-diffusive water-mass flux - UGDW, VGDW, WGDW
!  27   gas-diffusive air-mass flux - UGDA, VGDA, WGDA
!  28   gas CO2 mass flux - UGAM, VGAM, WGAM
!  29   aqueous CO2 mass flux - ULAM, VLAM, WLAM
!  30   total CO2 mass flux - UAM, VAM, WAM
!  31   gas-advective oil-mass flux - UAGO, VAGO, WAGO
!  32   gas-diffusive oil-mass flux - UDGO, VDGO, WDGO
!  33   gas oil mass flux
!       or gas CH4 mass flux - UGO, VGO, WGO
!  34   surface actual evaporation - AE
!  35   surface potential evaportion - PE
!  36   surface actual transpiration - AT
!  37   surface potential transpiration - PT
!  38   surface net total radiation - NTR
!  39   surface net short-wave radiation - NSWR
!  40   surface net long-wave radiation - NLWR
!  41   surface water-mass balance - WMB
!  42   surface rain-water runoff - RWRO
!  43   aqueous water mass flux - ULWM, VLWM, WLWM
!  44   gas water mass flux - UGWM, VGWM, WGWM
!  45   total water mass flux - UWM, VWM, WWM
!  46   aqueous-advective gas-component mass flux
!  47   aqueous-diffusive gas-component mass flux
!  48   gas-advective gas-component mass flux
!  49   gas-diffusive gas-component mass flux
!  50   total-advective-diffusive gas-component mass flux
!  51   total-diffusive gas-component mass flux
!  52   napl-advective gas-component mass flux
!  53   napl-diffusive gas-component mass flux
!  54   total-advective-diffusive gas-component mass flux
!  55   nonaqueous-liquid CO2 mass flux - UNAM, VNAM, WNAM
!  56   nonaqueous-liquid CH4 mass flux - UNOM, VNOM, WNOM
!  57   nonaqueous-liquid water mass flux - UNWM, VNWM, WNWM
!  58   total N2 mass flux - UN2M, VN2M, WN2M
!  59   aqueous N2 mass flux - ULNM, VLNM, WLNM
!  60   gas N2 mass flux - UGNM, VGNM, WGNM
!  61   nonaqueous-liquid N2 mass flux - UNNM, VNNM, WNNM
!  62   gas component mass flux - UGC, VGC, WGC
!  63   aqueous component mass flux - ULC, VLC, WLC
!  64   nonaqueous-liquid component mass flux - UNC, VNC, WNC
!  65   total component mass flux - UTC, VTC, WTC
! 101 to 100+NSOLU
!       solute flux - UC, VC, WC

! 101+NSOLU to 100+NSOLU+NEQC   
!       conservation-component species flux - UCC, VCC, WCC
! 101+NSOLU+NEQC to 100+NSOLU+NEQC+NEQK
!       kinetic-component species flux - UKC, VKC, WKC




!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle,  February, 1993.
!     Last Modified by MD White, Battelle, June 18, 1995.
!     Last Modified by MD White, PNNL, 30 May 2002.
!     Last Modified by CV Freedman, PNNL, 7 January 2003.
!     Last Modified by CV Freedman, PNNL, 16 January 2003.




!     $Id: sfin.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE REACT
      USE OUTPU
      USE NAPL
      USE GRID
      USE FLUXT
      USE FLUXS
      USE FLUXP
      USE FLUXN
      USE FLUXGC
      USE FLUXD
      USE FILES
      USE FDVT
      USE FDVP
      USE CONST
      USE CCP
      USE BCVT
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
!----------------------Type Declarations-------------------------------!
!
      INTEGER NCX(LSF),ISFCX(6)
      LOGICAL IFLAG
      CHARACTER*3 CHX
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/SFIN'
      IF( INDEX(SVN_ID(181)(1:1),'$').EQ.0 ) SVN_ID(181) =
     & '$Id: sfin.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Integrate water mass, air mass, VOC mass, and energy source terms
!
      IF( IOM.EQ.32 .OR. IOM.EQ.33 .OR. IOM.EQ.37 .OR. IOM.EQ.38
     &  .OR. IOM.EQ.39 ) THEN
        CHX = 'CO2'
      ELSE
        CHX = 'Air'
      ENDIF
      IF( IEQW.GT.0 ) THEN
        DO 10 N = 1,NFLD
          SRCIW(N) = SRCIW(N) + SRCW(2,N)*DT
   10   CONTINUE
      ENDIF
      IF( IEQA.GT.0 ) THEN
        DO 12 N = 1,NFLD
          SRCIA(N) = SRCIA(N) + SRCA(2,N)*DT
   12   CONTINUE
      ENDIF
      IF( IEQO.GT.0 .OR. IEQDO.GT.0 ) THEN
        DO 14 N = 1,NFLD
          SRCIO(N) = SRCIO(N) + SRCO(2,N)*DT
   14   CONTINUE
      ENDIF
      IF( IEQT.GT.0 ) THEN
        DO 16 N = 1,NFLD
          SRCIT(N) = SRCIT(N) + SRCT(2,N)*DT
   16   CONTINUE
      ENDIF
      IF( IEQS.GT.0 ) THEN
        DO 18 N = 1,NFLD
          SRCIS(N) = SRCIS(N) + SRCS(2,N)*DT
   18   CONTINUE
      ENDIF
      IF( IEQD.GT.0 ) THEN
        DO 20 N = 1,NFLD
          SRCID(N) = SRCID(N) + SRCD(2,N)*DT
   20   CONTINUE
      ENDIF
      LGCX = LGC
      IF( LGCX.GT.0 ) THEN
        DO 24 N = 1,NFLD
          DO 22 IGC = 1,NGC
            SRCIGC(IGC,N) = SRCIGC(IGC,N) + SRCGC(IGC,2,N)*DT
   22     CONTINUE
   24   CONTINUE
      ENDIF
      IF( NSF.EQ.0 ) THEN
        ISUB_LOG = ISUB_LOG-1
        RETURN
      ENDIF
!
!---  Initialize surface file counter  ---
!
      DO 65 NS = 1,NSF
        NCX(NS) = 0
   65 CONTINUE
!
!---  Surface file  ---
!
      IF( IHSF.EQ.0 ) THEN
!
!---    Skip the default surface file if its unused  ---
!
        NSTART = 1
        IF( ISFGP(1).EQ.0 ) NSTART = 2
!
!---    Loop over the number of surface-flux files  ---
!
        DO 75 NSG = NSTART,NSFGP
!
!---      Open surface files  ---
!
          OPEN(UNIT=ISF(NSG),FILE=FNSF(NSG),STATUS='UNKNOWN',
     &         FORM='FORMATTED')
          CLOSE(UNIT=ISF(NSG),STATUS='DELETE')
          OPEN(UNIT=ISF(NSG),FILE=FNSF(NSG),STATUS='NEW',
     &      FORM='FORMATTED')
!
!---      Write header  ---
!
          WRITE(ISF(NSG),'(A,//)')' Welcome to ...'
          WRITE(ISF(NSG),'(A)')   '                           STOMP'
          WRITE(ISF(NSG),'(A,//)')'        Subsurface Transport Over '
     &       // 'Multiple Phases'
          WRITE(ISF(NSG),'(A)')   ' This file was produced by STOMP, '
     &       // 'a numerical simulator'
          WRITE(ISF(NSG),'(A)')   ' developed by the Pacific Northwest '
     &       // 'Laboratory, with'
          WRITE(ISF(NSG),'(A)')   ' support from the VOC-Arid '
     &       // 'Integrated Demonstration Project,'
          WRITE(ISF(NSG),'(A)')   ' Office of Technology Development, '
     &       // 'U.S. Department of Energy.'
          WRITE(ISF(NSG),'(A)')   ' Results from this version of STOMP '
     &       // 'should not be used for'
          WRITE(ISF(NSG),'(A,/)') ' license related applications.'
          WRITE(ISF(NSG),'(A,/)') ' For inquiries or assistance:  '
     &       // 'Call (509) 372-6070'
          WRITE(ISF(NSG),'(A,//)')'                      ---  SURFACE '
     &      // ' ---'
          WRITE(ISF(NSG),'(2A)') 'Version: ',CH_VRSN




          WRITE(ISF(NSG),'(2A)') 'Date: ','Date system call inactive.'
          WRITE(ISF(NSG),'(2A)') 'Time: ','Time system call inactive.'

          WRITE(ISF(NSG),'(A,I4)') 'Number of Surfaces: ',ISFGP(NSG)
          WRITE(ISF(NSG),'(A,/)') 'Surface Variables: '
!
!---  Loop over the defined surfaces  ---
!
          NCH = INDEX(UNTM,'  ')-1
          WRITE(ISF(NSG),'(4A)') 'Time',',',UNTM(1:NCH),','
          DO 70 NS = 1,NSF
!
!---        Surface not associated with surface file,
!           skip output  ---
!
            IF( NSG.NE.ISFF(NS) ) GOTO 70
            NCH1 = INDEX(UNSF(1,NS),'  ')-1
            NCH2 = INDEX(UNSF(2,NS),'  ')-1
            IF( ISFT(NS).EQ.1 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Heat Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.2 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Aqueous Volumetric Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.3 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Gas Volumetric Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.4 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Nonaqueous-Liquid Volumetric Flux'
     &          ,',',UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.5 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Aqueous Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.6 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Gas Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.7 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Nonaqueous-Liquid Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.8 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Salt Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.9 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Dissolved-Oil Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.10 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Condensate Water Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.11 ) THEN
              IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                WRITE(ISF(NSG),'(6A)') 'Gas CH4 Mass Flux',',',
     &            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
              ELSEIF( IOM.EQ.35 ) THEN
                WRITE(ISF(NSG),'(6A)') 'Gas CO2 Mass Flux',',',
     &            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
              ELSE
                WRITE(ISF(NSG),'(6A)') 'Gas Oil Mass Flux',',',
     &            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
              ENDIF
            ELSEIF( ISFT(NS).EQ.12 ) THEN
              IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                WRITE(ISF(NSG),'(6A)') 'Aqueous CH4 Mass Flux',',',
     &            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
              ELSEIF( IOM.EQ.35 ) THEN
                WRITE(ISF(NSG),'(6A)') 'Aqueous CO2 Mass Flux',',',
     &            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
              ELSE
                WRITE(ISF(NSG),'(6A)') 'Aqueous Oil Mass Flux',',',
     &            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
              ENDIF
            ELSEIF( ISFT(NS).EQ.13 ) THEN
              IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                WRITE(ISF(NSG),'(6A)') 'Total CH4 Mass Flux',',',
     &            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
              ELSEIF( IOM.EQ.35 ) THEN
                WRITE(ISF(NSG),'(6A)') 'Total CO2 Mass Flux',',',
     &            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
              ELSE
                WRITE(ISF(NSG),'(6A)') 'Total Oil Mass Flux',',',
     &            UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
              ENDIF
            ELSEIF( ISFT(NS).EQ.20 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Gas-Advective Heat Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.21 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Gas-Advective Water-Mass Flux',
     &          ',',UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.22 ) THEN
              WRITE(ISF(NSG),'(7A)') 'Gas-Advective ',CHX,'-Mass Flux',
     &          ',',UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.25 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Gas-Diffusive Heat Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.26 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Gas-Diffusive Water-Mass Flux',
     &          ',',UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.27 ) THEN
              WRITE(ISF(NSG),'(7A)') 'Gas-Diffusive ',CHX,'-Mass Flux',
     &          ',',UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.28 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Gas CO2 Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.29 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Aqueous CO2 Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.30 ) THEN
              WRITE(ISF(NSG),'(8A)') 'Total ',CHX,'-Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.31 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Gas-Advective Oil-Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.32 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Gas-Diffusive Oil-Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.33 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Gas-Total Oil-Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.34 ) THEN
              WRITE(ISF(NSG),'(5A)') 'Surface Actual Evaporation,',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.35 ) THEN
              WRITE(ISF(NSG),'(5A)') 'Surface Potential Evaporation,',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.36 ) THEN
              WRITE(ISF(NSG),'(5A)') 'Surface Actual Transpiration,',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.37 ) THEN
              WRITE(ISF(NSG),'(5A)') 'Surface Potential Transpiration,',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.38 ) THEN
              WRITE(ISF(NSG),'(5A)') 'Surface Net Total Radiaion,',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.39 ) THEN
              WRITE(ISF(NSG),'(5A)') 'Surface Net Short-Wave Radiaion,',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.40 ) THEN
              WRITE(ISF(NSG),'(5A)') 'Surface Net Long-Wave Radiaion,',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.41 ) THEN
              WRITE(ISF(NSG),'(5A)') 'Surface Water-Mass Balance,',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.42 ) THEN
              WRITE(ISF(NSG),'(5A)') 'Surface Rain-Water Runoff,',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.43 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Aqueous Water Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.44 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Gas Water Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.45 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Total Water Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.46 ) THEN
              IGC = ISFGC(NS)
              NCH0 = INDEX(GCNM(IGC),'  ')-1
              WRITE(ISF(NSG),'(6A)') 'Aqueous-Advective ' // 
     &          GCNM(IGC)(1:NCH0) // '-Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.47 ) THEN
              IGC = ISFGC(NS)
              NCH0 = INDEX(GCNM(IGC),'  ')-1
              WRITE(ISF(NSG),'(6A)') 'Aqueous-Diffusive ' // 
     &          GCNM(IGC)(1:NCH0) // '-Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.48 ) THEN
              IGC = ISFGC(NS)
              NCH0 = INDEX(GCNM(IGC),'  ')-1
              WRITE(ISF(NSG),'(6A)') 'Gas-Advective ' // 
     &          GCNM(IGC)(1:NCH0) // '-Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.49 ) THEN
              IGC = ISFGC(NS)
              NCH0 = INDEX(GCNM(IGC),'  ')-1
              WRITE(ISF(NSG),'(6A)') 'Gas-Diffusive ' // 
     &          GCNM(IGC)(1:NCH0) // '-Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.50 ) THEN
              IGC = ISFGC(NS)
              NCH0 = INDEX(GCNM(IGC),'  ')-1
              WRITE(ISF(NSG),'(6A)') 'Total-Advective ' // 
     &          GCNM(IGC)(1:NCH0) // '-Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.51 ) THEN
              IGC = ISFGC(NS)
              NCH0 = INDEX(GCNM(IGC),'  ')-1
              WRITE(ISF(NSG),'(6A)') 'Total-Diffusive ' // 
     &          GCNM(IGC)(1:NCH0) // '-Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.52 ) THEN
              IGC = ISFGC(NS)
              NCH0 = INDEX(GCNM(IGC),'  ')-1
              WRITE(ISF(NSG),'(6A)') 'Nonaqueous-Liquid-Advective ' // 
     &          GCNM(IGC)(1:NCH0) // '-Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.53 ) THEN
              IGC = ISFGC(NS)
              NCH0 = INDEX(GCNM(IGC),'  ')-1
              WRITE(ISF(NSG),'(6A)') 'Nonaqueous-Liquid-Diffusive ' // 
     &          GCNM(IGC)(1:NCH0) // '-Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.54 ) THEN
              IGC = ISFGC(NS)
              NCH0 = INDEX(GCNM(IGC),'  ')-1
              WRITE(ISF(NSG),'(6A)') 'Total-Advective-Diffusive ' // 
     &          GCNM(IGC)(1:NCH0) // '-Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.55 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Liquid-CO2 CO2 Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.56 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Liquid-CO2 CH4 Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.57 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Liquid-CO2 H20 Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.58 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Total N2 Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.59 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Aqueous N2 Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.60 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Gas N2 Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.61 ) THEN
              WRITE(ISF(NSG),'(6A)') 'Nonaqueous-Liquid N2 Mass Flux',
     &          ',',UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.62 ) THEN
              IGC = ISFGC(NS)
              NCH0 = INDEX(GCNM(IGC),'  ')-1
              WRITE(ISF(NSG),'(6A)') 'Gas Component ' // 
     &          GCNM(IGC)(1:NCH0) // ' Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.63 ) THEN
              IGC = ISFGC(NS)
              NCH0 = INDEX(GCNM(IGC),'  ')-1
              WRITE(ISF(NSG),'(6A)') 'Aqueous Component ' // 
     &          GCNM(IGC)(1:NCH0) // ' Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.64 ) THEN
              IGC = ISFGC(NS)
              NCH0 = INDEX(GCNM(IGC),'  ')-1
              WRITE(ISF(NSG),'(6A)') 'Nonaqueous-Liquid Component ' // 
     &          GCNM(IGC)(1:NCH0) // ' Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).EQ.65 ) THEN
              IGC = ISFGC(NS)
              NCH0 = INDEX(GCNM(IGC),'  ')-1
              WRITE(ISF(NSG),'(6A)') 'Total Component ' // 
     &          GCNM(IGC)(1:NCH0) // ' Mass Flux',',',
     &          UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).GT.100 .AND. ISFT(NS).LE.(100+NSOLU) ) THEN
              NSL = ISFT(NS)-100
              NCH = INDEX(SOLUT(NSL),'  ')-1
              WRITE(ISF(NSG),'(7A)') 'Solute Flux (',SOLUT(NSL)(1:NCH),
     &          '),',UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','

            ELSEIF( ISFT(NS).GT.(100+NSOLU) .AND. 
     &        ISFT(NS).LE.(100+NSOLU+NEQC) ) THEN
              NSL = ISFT(NS)-100
              NCH = INDEX(SOLUT(NSL),'  ')-1
              WRITE(ISF(NSG),'(7A)') 'Conservation Component ' // 
     &          'Species Flux (',SOLUT(NSL)(1:NCH),
     &          '),',UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','
            ELSEIF( ISFT(NS).GT.(100+NSOLU+NEQC) .AND. 
     &        ISFT(NS).LE.(100+NSOLU+NEQC+NEQK) ) THEN
              NSL = ISFT(NS)-100
              NCH = INDEX(SOLUT(NSL),'  ')-1
              WRITE(ISF(NSG),'(7A)') 'Kinetic Component ' // 
     &          'Species Flux (',SOLUT(NSL)(1:NCH),
     &          '),',UNSF(1,NS)(1:NCH1),',',UNSF(2,NS)(1:NCH2),','

            ENDIF
   70     CONTINUE
          WRITE(ISF(NSG),'(/)')
   75   CONTINUE
      ENDIF
!
!---  Skip the default surface file if its unused  ---
!
      NSTART = 1
      IF( ISFGP(1).EQ.0 ) NSTART = 2
!
!---  Loop over the number of surface-flux files  ---
!
      DO  95 NSG = NSTART,NSFGP
        NCSX = 0
        NCUX = 0
!
!---    Write header every ten time steps  ---
!
        IF( IHSF.EQ.0 .OR. IHSF.EQ.10 ) THEN
          IHSF = 0
          WRITE(ISF(NSG),9001) '    Time      '
          DO 80 NS = 1,NSF
!
!---        Surface not associated with surface file,
!           skip output  ---
!
            IF( NSG.NE.ISFF(NS) ) GOTO 80
            NCSX = NCSX + 1
            I = ABS(ISFT(NS))
            IF( I.GT.100 .AND. I.LE.(100+NSOLU) ) THEN
              I = 100

            ELSEIF( I.GT.(100+NSOLU) .AND. I.LE.(100+NSOLU+NEQC) ) THEN
              I = 102
            ELSEIF( I.GT.(100+NSOLU+NEQC) .AND. 
     &        I.LE.(100+NSOLU+NEQC+NEQK) ) THEN
              I = 103




            ENDIF
            I = MIN( ABS(ISFT(NS)),100 )            
            J = ABS( ISFD(NS) )
            IF( ISFD(NS).EQ.4 ) J = 1
            ICH = INDEX(CHSF(I,J)(1:),' ') - 1
            IF( NS.EQ.NSF .OR. NCSX.EQ.ISFGP(NSG) ) THEN         
              WRITE(ISF(NSG),9002) CHSF(I,J)(1:ICH)//'R','(',NS,')  '
              WRITE(ISF(NSG),9012) CHSF(I,J)(1:ICH)//'I','(',NS,')  '
            ELSE
              WRITE(ISF(NSG),9002) CHSF(I,J)(1:ICH)//'R','(',NS,')  '
              WRITE(ISF(NSG),9002) CHSF(I,J)(1:ICH)//'I','(',NS,')  '
            ENDIF
   80     CONTINUE
          WRITE(ISF(NSG),9003) ' [',UNTM(1:8),'] '
          DO 90 NS = 1,NSF
!
!---        Surface not associated with surface file,
!           skip output  ---
!
            IF( NSG.NE.ISFF(NS) ) GOTO 90
            NCUX = NCUX + 1
            IF( NS.EQ.NSF .OR. NCUX.EQ.ISFGP(NSG) ) THEN
              WRITE(ISF(NSG),9003) '[',UNSF(1,NS)(1:8),']  '
              WRITE(ISF(NSG),9013) '[',UNSF(2,NS)(1:8),']  '
            ELSE
              WRITE(ISF(NSG),9003) '[',UNSF(1,NS)(1:8),']  '
              WRITE(ISF(NSG),9003) '[',UNSF(2,NS)(1:8),']  '
            ENDIF
   90     CONTINUE
        ENDIF
   95 CONTINUE
!
!---  Loop over the defined surfaces  ---
!
      DO 7000 NS = 1,NSF
        SF(1,NS) = 0.D+0
        IF( ISFD(NS).EQ.4 ) THEN
          NSFDOMX = NSFDOM(NS)
        ELSE
          NSFDOMX = 1
        ENDIF
        DO 6900 NC = 1,NSFDOMX
        IF( ISFD(NS).EQ.4 ) THEN
          ISFDX = ISFDOM(4,NC,NS)
          DO 200 I = 1,3
            ISFCX((I-1)*2+1) = ISFDOM(I,NC,NS)
            ISFCX((I-1)*2+2) = ISFDOM(I,NC,NS)
  200     CONTINUE
        ELSE
          ISFDX = ISFD(NS)
          DO 220 I = 1,6
            ISFCX(I) = ISFC(I,NS)
  220     CONTINUE
        ENDIF
!
!---  Bottom surface  ---
!
        IF( ISFDX.EQ.-3 ) THEN
          DO 1600 I = ISFCX(1),ISFCX(2)
            DO 1500 J = ISFCX(3),ISFCX(4)
              DO 1400 K = ISFCX(5),ISFCX(6)
                N = ND(I,J,K)
                NPZ = NSZ(N)
                IF( ISFT(NS).EQ.1 ) THEN
                  SFX = WQ(1,NPZ)*AFZ(NPZ)
                ELSEIF( ISFT(NS).EQ.2 ) THEN
                  SFX = WL(1,NPZ)*AFZ(NPZ)
                ELSEIF( ISFT(NS).EQ.3 ) THEN
                  SFX = WG(1,NPZ)*AFZ(NPZ)
                ELSEIF( ISFT(NS).EQ.4 ) THEN
                  SFX = WN(1,NPZ)*AFZ(NPZ)
                ELSEIF( ISFT(NS).EQ.5 ) THEN
                  IF( WL(1,NPZ).GT.ZERO ) THEN
                    IF( K.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHOLX = RHOL(2,N-IJFLD)
                    ELSE
                      DO 1050 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHOLX = RHOLB(2,NB)
                          GOTO 1052
                        ENDIF
 1050                 CONTINUE
 1052                 CONTINUE
                    ENDIF
                  ELSE
                    RHOLX = RHOL(2,N)
                  ENDIF
                  SFX = WL(1,NPZ)*AFZ(NPZ)*RHOLX
                ELSEIF( ISFT(NS).EQ.6 ) THEN
                  IF( WG(1,NPZ).GT.ZERO ) THEN
                    IF( K.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHOGX = RHOG(2,N-IJFLD)
                    ELSE
                      DO 1060 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHOGX = RHOGB(2,NB)
                          GOTO 1062
                        ENDIF
 1060                 CONTINUE
 1062                 CONTINUE
                    ENDIF
                  ELSE
                    RHOGX = RHOG(2,N)
                  ENDIF
                  SFX = WG(1,NPZ)*AFZ(NPZ)*RHOGX
                ELSEIF( ISFT(NS).EQ.7 ) THEN
                  IF( WN(1,NPZ).GT.ZERO ) THEN
                    IF( K.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHONX = RHON(2,N-IJFLD)
                    ELSE
                      DO 1070 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHONX = RHONB(2,NB)
                          GOTO 1072
                        ENDIF
 1070                 CONTINUE
 1072                 CONTINUE
                    ENDIF
                  ELSE
                    RHONX = RHON(2,N)
                  ENDIF
                  SFX = WN(1,NPZ)*AFZ(NPZ)*RHONX
                ELSEIF( ISFT(NS).EQ.8 ) THEN
                  SFX = WS(1,NPZ)*AFZ(NPZ)
                ELSEIF( ISFT(NS).EQ.9 ) THEN
                  SFX = WLO(1,NPZ)*AFZ(NPZ)
                ELSEIF( ISFT(NS).EQ.10 ) THEN
                  IF( WL(1,NPZ).GT.ZERO ) THEN
                    IF( K.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHOLX = RHOL(2,N-IJFLD)*XLW(2,N-IJFLD)
                    ELSE
                      DO 1100 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHOLX = RHOLB(2,NB)*XLWB(2,NB)
                          GOTO 1102
                        ENDIF
 1100                 CONTINUE
 1102                 CONTINUE
                    ENDIF
                  ELSE
                    RHOLX = RHOL(2,N)*XLW(2,N)
                  ENDIF
                  SFX = WL(1,NPZ)*AFZ(NPZ)*RHOLX +
     &              (WDGW(1,NPZ)-WDLO(1,NPZ)-WDLA(1,NPZ))*AFZ(NPZ)*WTMW
!
!---            Gas-CH4 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.11 ) THEN
                  SFX = AFZ(NPZ)*WGO(1,NPZ)
!
!---            Aqueous-CH4 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.12 ) THEN
                  SFX = AFZ(NPZ)*WLO(1,NPZ)
!
!---            Total-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.13 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFZ(NPZ)*(WGO(1,NPZ)+WLO(1,NPZ)+WNO(1,NPZ))
                  ELSE
                    SFX = AFZ(NPZ)*(WGO(1,NPZ)+WLO(1,NPZ))
                  ENDIF
                ELSEIF( ISFT(NS).EQ.20 ) THEN
                  IF( WG(1,NPZ).GT.ZERO ) THEN
                    IF( K.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-IJFLD
                      VAR = (HGW(2,NB)*XGW(2,NB) + HGO(2,NB)*XGO(2,NB)
     &                  + HGA(2,NB)*XGA(2,NB))*RHOG(2,NB)
                    ELSE
                      DO 1200 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = (HGWB(2,NB)*XGWB(2,NB)+HGOB(2,NB)*XGOB(2,NB)
     &                  + HGAB(2,NB)*XGAB(2,NB))*RHOGB(2,NB)
                          GOTO 1201
                        ENDIF
 1200                 CONTINUE
 1201                CONTINUE
                    ENDIF
                  ELSE
                    VAR = (HGW(2,N)*XGW(2,N) + HGO(2,N)*XGO(2,N)
     &                + HGA(2,N)*XGA(2,N))*RHOG(2,N)
                  ENDIF
                  SFX = VAR*WG(1,NPZ)*AFZ(NPZ)
                ELSEIF( ISFT(NS).EQ.21 ) THEN
                  IF( WG(1,NPZ).GT.ZERO ) THEN
                    IF( K.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-IJFLD
                      VAR = XGW(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 1210 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGWB(2,NB)*RHOGB(2,NB)
                          GOTO 1211
                        ENDIF
 1210                 CONTINUE
 1211                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGW(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*WG(1,NPZ)*AFZ(NPZ)
                ELSEIF( ISFT(NS).EQ.22 ) THEN
                  IF( WG(1,NPZ).GT.ZERO ) THEN
                    IF( K.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-IJFLD
                      VAR = XGA(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 1220 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGAB(2,NB)*RHOGB(2,NB)
                          GOTO 1221
                        ENDIF
 1220                 CONTINUE
 1221                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGA(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*WG(1,NPZ)*AFZ(NPZ)
                ELSEIF( ISFT(NS).EQ.25 ) THEN
                  IF( WDGW(1,NPZ).GT.ZERO ) THEN
                    VARA = HGA(2,N)*WTMA
                    IF( K.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-IJFLD
                      VARW = HGW(2,NB)*WTMW
                    ELSE
                      DO 1230 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          VARW = HGWB(2,NB)*WTMW
                          GOTO 1231
                        ENDIF
 1230                 CONTINUE
 1231                CONTINUE
                    ENDIF
                  ELSE
                    VARW = HGW(2,N)*WTMW
                    IF( K.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-IJFLD
                      VARA = HGA(2,NB)*WTMA
                    ELSE
                      DO 1232 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          VARA = HGAB(2,NB)*WTMA
                          GOTO 1233
                        ENDIF
 1232                 CONTINUE
 1233                CONTINUE
                    ENDIF
                  ENDIF
                  SFX = WDGW(1,NPZ)*(VARW-VARA)*AFZ(NPZ)
                ELSEIF( ISFT(NS).EQ.26 ) THEN
                  SFX = WTMW*WDGW(1,NPZ)*AFZ(NPZ)
                ELSEIF( ISFT(NS).EQ.27 ) THEN
                  SFX = - WTMA*WDGW(1,NPZ)*AFZ(NPZ)
!
!---            Gas-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.28 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFZ(NPZ)*WGA(1,NPZ)
                  ELSE
                    IF( K.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-IJFLD
                      FGAB = XGA(2,NB)*RHOG(2,NB)
                      DZB = DZGF(NB)
                    ELSE
                      DO 1280 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          FGAB = XGAB(2,NB)*RHOGB(2,NB)
                          DZB = DZGF(N)
                        GOTO 1281
                        ENDIF
 1280                 CONTINUE
 1281                CONTINUE
                    ENDIF
                    FGAP = XGA(2,N)*RHOG(2,N)
                    INDX = 3
                    FGA = DIFMN( FGAB,FGAP,DZB,DZGF(N),WG(1,NPZ),INDX )
                    SFX = AFZ(NPZ)*(WG(1,NPZ)*FGA -
     &                WTMA*WDGW(1,NPZ))
                  ENDIF
!
!---            Aqueous-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.29 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFZ(NPZ)*WLA(1,NPZ)
                  ELSE
                    IF( K.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-IJFLD
                      FLAB = XLA(2,NB)*RHOL(2,NB)
                      DZB = DZGF(NB)
                    ELSE
                      DO 1290 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          FLAB = XLAB(2,NB)*RHOLB(2,NB)
                          DZB = DZGF(N)
                          GOTO 1291
                        ENDIF
 1290                 CONTINUE
 1291                 CONTINUE
                    ENDIF
                    FLAP = XLA(2,N)*RHOL(2,N)
                    INDX = 2
                    FLA = DIFMN( FLAB,FLAP,DZB,DZGF(N),WL(1,NPZ),INDX )
                    SFX = AFZ(NPZ)*(WL(1,NPZ)*FLA +
     &                WTMA*WDLA(1,NPZ))
                  ENDIF
!
!---            Total-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.30 ) THEN
                  IF( IOM.EQ.36 .OR. IOM.EQ.37 ) THEN
                    SFX = AFZ(NPZ)*(WGA(1,NPZ)+WLA(1,NPZ))
                  ELSEIF( IOM.EQ.38 ) THEN
                    SFX = AFZ(NPZ)*(WGA(1,NPZ)+WLA(1,NPZ)+WNC(1,1,NPZ))
                  ELSEIF( IOM.EQ.39 ) THEN
                    SFX = AFZ(NPZ)*(WGA(1,NPZ)+WLA(1,NPZ)+WNA(1,NPZ))
                  ELSE
                    IF( K.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-IJFLD
                      FGAB = XGA(2,NB)*RHOG(2,NB)
                      FLAB = XLA(2,NB)*RHOL(2,NB)
                      DZB = DZGF(NB)
                    ELSE
                      DO 1300 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          FGAB = XGAB(2,NB)*RHOGB(2,NB)
                          FLAB = XLAB(2,NB)*RHOLB(2,NB)
                          DZB = DZGF(N)
                          GOTO 1301
                        ENDIF
 1300                 CONTINUE
 1301                 CONTINUE
                    ENDIF
                    FGAP = XGA(2,N)*RHOG(2,N)
                    FLAP = XLA(2,N)*RHOL(2,N)
                    INDX = 3
                    FGA = DIFMN( FGAB,FGAP,DZB,DZGF(N),WG(1,NPZ),INDX )
                    INDX = 2
                    FLA = DIFMN( FLAB,FLAP,DZB,DZGF(N),WL(1,NPZ),INDX )
                    SFX = AFZ(NPZ)*(WL(1,NPZ)*FLA + WG(1,NPZ)*FGA
     &                - WTMA*WDGW(1,NPZ) + WTMA*WDLA(1,NPZ))
                  ENDIF
!
!---            Gas-Advective Oil-Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.31 ) THEN
                  IF( WG(1,NPZ).GT.ZERO ) THEN
                    IF( K.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-IJFLD
                      VAR = XGO(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 1310 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGOB(2,NB)*RHOGB(2,NB)
                          GOTO 1311
                        ENDIF
 1310                 CONTINUE
 1311                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGO(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*WG(1,NPZ)*AFZ(NPZ)
!
!---            Gas-Diffusive Oil-Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.32 ) THEN
                  SFX = WTMO*WDGO(1,NPZ)*AFZ(NPZ)
!
!---            Gas-Total Oil-Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.33 ) THEN
                  IF( WG(1,NPZ).GT.ZERO ) THEN
                    IF( K.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-IJFLD
                      VAR = XGO(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 1330 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGOB(2,NB)*RHOGB(2,NB)
                          GOTO 1331
                        ENDIF
 1330                 CONTINUE
 1331                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGO(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*WG(1,NPZ)*AFZ(NPZ)
     &              + WTMO*WDGO(1,NPZ)*AFZ(NPZ)
!
!---            Aqueous-Water Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.43 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFZ(NPZ)*WLW(1,NPZ)
                  ENDIF
!
!---            Gas-Water Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.44 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFZ(NPZ)*WGW(1,NPZ)
                  ENDIF
!
!---            Total-Water Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.45 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.38 ) THEN
                    SFX = AFZ(NPZ)*(WLW(1,NPZ)+WGW(1,NPZ))
                  ELSEIF( IOM.EQ.39 ) THEN
                    SFX = AFZ(NPZ)*(WLW(1,NPZ)+WGW(1,NPZ)+WNW(1,NPZ) )
                  ENDIF
!
!---            Aqueous-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.46 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (WLC(IGC,1,NPZ) - WDLC(IGC,1,NPZ)*GCPP(1,IGC))
     &              *AFZ(NPZ)
!
!---            Aqueous-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.47 ) THEN
                  IGC = ISFGC(NS)
                  SFX = WDLC(IGC,1,NPZ)*GCPP(1,IGC)*AFZ(NPZ)
!
!---            Gas-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.48 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (WGC(IGC,1,NPZ) - WDGC(IGC,1,NPZ)*GCPP(1,IGC))
     &              *AFZ(NPZ)
!
!---            Gas-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.49 ) THEN
                  IGC = ISFGC(NS)
                  SFX = WDGC(IGC,1,NPZ)*GCPP(1,IGC)*AFZ(NPZ)
!
!---            Total-Advective-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.50 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (WGC(IGC,1,NPZ) + WLC(IGC,1,NPZ) 
     &              + WNC(IGC,1,NPZ))*AFZ(NPZ)
!
!---            Total-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.51 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (WDGC(IGC,1,NPZ) 
     &              + WDLC(IGC,1,NPZ)  + WDNC(IGC,1,NPZ))
     &              *GCPP(1,IGC)*AFZ(NPZ)
!
!---            Nonaqueous-Liquid-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.52 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (WNC(IGC,1,NPZ) - WDNC(IGC,1,NPZ)*GCPP(1,IGC))
     &              *AFZ(NPZ)
!
!---            Nonaqueous-Liquid-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.53 ) THEN
                  IGC = ISFGC(NS)
                  SFX = WDNC(IGC,1,NPZ)*GCPP(1,IGC)*AFZ(NPZ)
!
!---            Total-Advective-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.54 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (WGC(IGC,1,NPZ) + WNC(IGC,1,NPZ))*AFZ(NPZ)
!
!---            Liquid-CO2 CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.55 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFZ(NPZ)*WNA(1,NPZ)
                  ELSE
                    IGC = 1
                    SFX = AFZ(NPZ)*WNC(IGC,1,NPZ)
                  ENDIF
!
!---            Liquid-CO2 CH4 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.56 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFZ(NPZ)*WNO(1,NPZ)
                  ELSE
                    IGC = 2
                    SFX = AFZ(NPZ)*WNC(IGC,1,NPZ)
                  ENDIF
!
!---            Liquid-CO2 H2O Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.57 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFZ(NPZ)*WNW(1,NPZ)
                  ELSE
                    IGC = 3
                    SFX = AFZ(NPZ)*WNC(IGC,1,NPZ)
                  ENDIF
!
!---            Total N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.58 ) THEN
                  SFX = AFZ(NPZ)*(WLN(1,NPZ)+WGN(1,NPZ)+WNN(1,NPZ))
!
!---            Aqueous N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.59 ) THEN
                  SFX = AFZ(NPZ)*WLN(1,NPZ)
!
!---            Gas N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.60 ) THEN
                  SFX = AFZ(NPZ)*WGN(1,NPZ)
!
!---            Nonaqueous Liquid N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.61 ) THEN
                  SFX = AFZ(NPZ)*WNN(1,NPZ)
!
!---            Gas Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.62 ) THEN
                  IGC = ISFGC(NS)
                  SFX = WGC(IGC,1,NPZ)*AFZ(NPZ)
!
!---            Aqueous Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.63 ) THEN
                  SFX = WLA(1,NPZ)*AFZ(NPZ)
!
!---            Nonaqueous-Liquid Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.64 ) THEN
                  IGC = ISFGC(NS)
                  SFX = WNC(IGC,1,NPZ)*AFZ(NPZ)
!
!---            Total Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.65 ) THEN
                  IGC = ISFGC(NS)
                  IF( IGC.EQ.1 ) THEN
                    SFX = (WLA(1,NPZ) + WGC(IGC,1,NPZ) + WNC(IGC,1,NPZ))
     &                *AFZ(NPZ)
                  ELSE
                    SFX = (WGC(IGC,1,NPZ) + WNC(IGC,1,NPZ))*AFZ(NPZ)
                  ENDIF
                ELSEIF( ISFT(NS).GT.100 .AND. 
     &            ISFT(NS).LE.(100+NSOLU) ) THEN
                  NSL = ISFT(NS)-100
                  SFX = WC(NPZ,NSL)*AFZ(NPZ)

                ELSEIF( ISFT(NS).GT.(100+NSOLU) .AND. 
     &            ISFT(NS).LE.(100+NSOLU+NEQC+NEQK) ) THEN
                  NSL = ISFT(NS)-100
                  SFX = WC(NPZ,NSL)*AFZ(NPZ)*1.D-3






                ENDIF
                RSFDX = REAL(ISFDX)
                SF(1,NS) = SF(1,NS)+SFX*SIGN(1.D+0,RSFDX)
 1400         CONTINUE
 1500       CONTINUE
 1600     CONTINUE
!
!---  South surface  ---
!
        ELSEIF( ISFDX.EQ.-2 ) THEN
          DO 2600 I = ISFCX(1),ISFCX(2)
            DO 2500 J = ISFCX(3),ISFCX(4)
              DO 2400 K = ISFCX(5),ISFCX(6)
                N = ND(I,J,K)
                NPY = NSY(N)
                IF( ISFT(NS).EQ.1 ) THEN
                  SFX = VQ(1,NPY)*AFY(NPY)
                ELSEIF( ISFT(NS).EQ.2 ) THEN
                  SFX = VL(1,NPY)*AFY(NPY)
                ELSEIF( ISFT(NS).EQ.3 ) THEN
                  SFX = VG(1,NPY)*AFY(NPY)
                ELSEIF( ISFT(NS).EQ.4 ) THEN
                  SFX = VN(1,NPY)*AFY(NPY)
                ELSEIF( ISFT(NS).EQ.5 ) THEN
                  IF( VL(1,NPY).GT.ZERO ) THEN
                    IF( J.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHOLX = RHOL(2,N-IFLD)
                    ELSE
                      DO 2050 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHOLX = RHOLB(2,NB)
                          GOTO 2052
                        ENDIF
 2050                 CONTINUE
 2052                 CONTINUE
                    ENDIF
                  ELSE
                    RHOLX = RHOL(2,N)
                  ENDIF
                  SFX = VL(1,NPY)*AFY(NPY)*RHOLX
                ELSEIF( ISFT(NS).EQ.6 ) THEN
                  IF( VG(1,NPY).GT.ZERO ) THEN
                    IF( J.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHOGX = RHOG(2,N-IFLD)
                    ELSE
                      DO 2060 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHOGX = RHOGB(2,NB)
                          GOTO 2062
                        ENDIF
 2060                 CONTINUE
 2062                 CONTINUE
                    ENDIF
                  ELSE
                    RHOGX = RHOG(2,N)
                  ENDIF
                  SFX = VG(1,NPY)*AFY(NPY)*RHOGX
                ELSEIF( ISFT(NS).EQ.7 ) THEN
                  IF( VN(1,NPY).GT.ZERO ) THEN
                    IF( J.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHONX = RHON(2,N-IFLD)
                    ELSE
                      DO 2070 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHONX = RHONB(2,NB)
                          GOTO 2072
                        ENDIF
 2070                 CONTINUE
 2072                 CONTINUE
                    ENDIF
                  ELSE
                    RHONX = RHON(2,N)
                  ENDIF
                  SFX = VN(1,NPY)*AFY(NPY)*RHONX
                ELSEIF( ISFT(NS).EQ.8 ) THEN
                  SFX = VS(1,NPY)*AFY(NPY)
                ELSEIF( ISFT(NS).EQ.9 ) THEN
                  SFX = VLO(1,NPY)*AFY(NPY)
                ELSEIF( ISFT(NS).EQ.10 ) THEN
                  IF( VL(1,NPY).GT.ZERO ) THEN
                    IF( J.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHOLX = RHOL(2,N-IFLD)*XLW(2,N-IFLD)
                    ELSE
                      DO 2100 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHOLX = RHOLB(2,NB)*XLWB(2,NB)
                          GOTO 2102
                        ENDIF
 2100                 CONTINUE
 2102                 CONTINUE
                    ENDIF
                  ELSE
                    RHOLX = RHOL(2,N)*XLW(2,N)
                  ENDIF
                  SFX = VL(1,NPY)*AFY(NPY)*RHOLX +
     &              (VDGW(1,NPY)-VDLA(1,NPY)-VDLO(1,NPY))*WTMW*AFY(NPY)
!
!---            Gas-CH4 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.11 ) THEN
                  SFX = AFY(NPY)*VGO(1,NPY)
!
!---            Aqueous-CH4 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.12 ) THEN
                  SFX = AFY(NPY)*VLO(1,NPY)
!
!---            Total-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.13 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFY(NPY)*(VGO(1,NPY)+VLO(1,NPY)+VNO(1,NPY))
                  ELSE
                    SFX = AFY(NPY)*(VGO(1,NPY)+VLO(1,NPY))
                  ENDIF
                ELSEIF( ISFT(NS).EQ.20 ) THEN
                  IF( VG(1,NPY).GT.ZERO ) THEN
                    IF( J.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-IFLD
                      VAR = (HGW(2,NB)*XGW(2,NB) + HGO(2,NB)*XGO(2,NB)
     &                  + HGA(2,NB)*XGA(2,NB))*RHOG(2,NB)
                    ELSE
                      DO 2200 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = (HGWB(2,NB)*XGWB(2,NB)+HGOB(2,NB)*XGOB(2,NB)
     &                  + HGAB(2,NB)*XGAB(2,NB))*RHOGB(2,NB)
                          GOTO 2201
                        ENDIF
 2200                 CONTINUE
 2201                CONTINUE
                    ENDIF
                  ELSE
                    VAR = (HGW(2,N)*XGW(2,N) + HGO(2,N)*XGO(2,N)
     &                + HGA(2,N)*XGA(2,N))*RHOG(2,N)
                  ENDIF
                  SFX = VAR*VG(1,NPY)*AFY(NPY)
                ELSEIF( ISFT(NS).EQ.21 ) THEN
                  IF( VG(1,NPY).GT.ZERO ) THEN
                    IF( J.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-IFLD
                      VAR = XGW(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 2210 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGWB(2,NB)*RHOGB(2,NB)
                          GOTO 2211
                        ENDIF
 2210                 CONTINUE
 2211                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGW(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*VG(1,NPY)*AFY(NPY)
                ELSEIF( ISFT(NS).EQ.22 ) THEN
                  IF( VG(1,NPY).GT.ZERO ) THEN
                    IF( J.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-IFLD
                      VAR = XGA(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 2220 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGAB(2,NB)*RHOGB(2,NB)
                          GOTO 2221
                        ENDIF
 2220                 CONTINUE
 2221                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGA(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*VG(1,NPY)*AFY(NPY)
                ELSEIF( ISFT(NS).EQ.25 ) THEN
                  IF( VDGW(1,NPY).GT.ZERO ) THEN
                    VARA = HGA(2,N)*WTMA
                    IF( J.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-IFLD
                      VARW = HGW(2,NB)*WTMW
                    ELSE
                      DO 2230 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          VARW = HGWB(2,NB)*WTMW
                          GOTO 2231
                        ENDIF
 2230                 CONTINUE
 2231                CONTINUE
                    ENDIF
                  ELSE
                    VARW = HGW(2,N)*WTMW
                    IF( J.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-IJFLD
                      VARA = HGA(2,NB)*WTMA
                    ELSE
                      DO 2232 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          VARA = HGAB(2,NB)*WTMA
                          GOTO 2233
                        ENDIF
 2232                 CONTINUE
 2233                CONTINUE
                    ENDIF
                  ENDIF
                  SFX = VDGW(1,NPY)*(VARW-VARA)*AFY(NPY)
                ELSEIF( ISFT(NS).EQ.26 ) THEN
                  SFX = WTMW*VDGW(1,NPY)*AFY(NPY)
                ELSEIF( ISFT(NS).EQ.27 ) THEN
                  SFX = - WTMA*VDGW(1,NPY)*AFY(NPY)
!
!---            Gas-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.28 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFY(NPY)*VGA(1,NPY)
                  ELSE
                    IF( J.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-IFLD
                      FGAB = XGA(2,NB)*RHOG(2,NB)
                      DYB = DYGF(NB)
                    ELSE
                      DO 2280 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          FGAB = XGAB(2,NB)*RHOGB(2,NB)
                          DYB = DYGF(N)
                          GOTO 2281
                        ENDIF
 2280                 CONTINUE
 2281                 CONTINUE
                    ENDIF
                    FGAP = XGA(2,N)*RHOG(2,N)
                    INDX = 3
                    FGA = DIFMN( FGAB,FGAP,DYB,DYGF(N),VG(1,NPY),INDX )
                    SFX = AFY(NPY)*(VG(1,NPY)*FGA -
     &                WTMA*VDGW(1,NPY))
                  ENDIF
!
!---            Aqueous-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.29 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFY(NPY)*VLA(1,NPY)
                  ELSE
                    IF( J.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-IFLD
                      FLAB = XLA(2,NB)*RHOL(2,NB)
                      DYB = DYGF(NB)
                    ELSE
                      DO 2290 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          FLAB = XLAB(2,NB)*RHOLB(2,NB)
                          DYB = DYGF(N)
                          GOTO 2291
                        ENDIF
 2290                 CONTINUE
 2291                 CONTINUE
                    ENDIF
                    FLAP = XLA(2,N)*RHOL(2,N)
                    INDX = 2
                    FLA = DIFMN( FLAB,FLAP,DYB,DYGF(N),VL(1,NPY),INDX )
                    SFX = AFY(NPY)*(VL(1,NPY)*FLA +
     &                WTMA*VDLA(1,NPY))
                  ENDIF
!
!---            Total-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.30 ) THEN
                  IF( IOM.EQ.36 .OR. IOM.EQ.37 ) THEN
                    SFX = AFY(NPY)*(VGA(1,NPY)+VLA(1,NPY))
                  ELSEIF( IOM.EQ.38 ) THEN
                    SFX = AFY(NPY)*(VGA(1,NPY)+VLA(1,NPY)+VNC(1,1,NPY))
                  ELSEIF( IOM.EQ.39 ) THEN
                    SFX = AFY(NPY)*(VGA(1,NPY)+VLA(1,NPY)+VNA(1,NPY))
                  ELSE
                    IF( J.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-IFLD
                      FGAB = XGA(2,NB)*RHOG(2,NB)
                      FLAB = XLA(2,NB)*RHOL(2,NB)
                      DYB = DYGF(NB)
                    ELSE
                      DO 2300 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          FGAB = XGAB(2,NB)*RHOGB(2,NB)
                          FLAB = XLAB(2,NB)*RHOLB(2,NB)
                          DYB = DYGF(N)
                          GOTO 2301
                        ENDIF
 2300                 CONTINUE
 2301                 CONTINUE
                    ENDIF
                    FGAP = XGA(2,N)*RHOG(2,N)
                    FLAP = XLA(2,N)*RHOL(2,N)
                    INDX = 3
                    FGA = DIFMN( FGAB,FGAP,DYB,DYGF(N),VG(1,NPY),INDX )
                    INDX = 2
                    FLA = DIFMN( FLAB,FLAP,DYB,DYGF(N),VL(1,NPY),INDX )
                    SFX = AFY(NPY)*(VL(1,NPY)*FLA + VG(1,NPY)*FGA 
     &                - WTMA*VDGW(1,NPY) + WTMA*VDLA(1,NPY))
                  ENDIF
!
!---            Gas-Advective Oil-Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.31 ) THEN
                  IF( VG(1,NPY).GT.ZERO ) THEN
                    IF( J.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-IFLD
                      VAR = XGO(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 2310 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGOB(2,NB)*RHOGB(2,NB)
                          GOTO 2311
                        ENDIF
 2310                 CONTINUE
 2311                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGO(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*VG(1,NPY)*AFY(NPY)
!
!---            Gas-Diffusive Oil-Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.32 ) THEN
                  SFX = WTMO*VDGO(1,NPY)*AFY(NPY)
!
!---            Gas-Total Oil-Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.33 ) THEN
                  IF( VG(1,NPY).GT.ZERO ) THEN
                    IF( J.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-IFLD
                      VAR = XGO(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 2330 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGOB(2,NB)*RHOGB(2,NB)
                          GOTO 2331
                        ENDIF
 2330                 CONTINUE
 2331                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGO(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*VG(1,NPY)*AFY(NPY)
     &              + WTMO*VDGO(1,NPY)*AFY(NPY)
!
!---            Aqueous-Water Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.43 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFY(NPY)*VLW(1,NPY)
                  ENDIF
!
!---            Gas-Water Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.44 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFY(NPY)*VGW(1,NPY)
                  ENDIF
!
!---            Total-Water Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.45 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.38 ) THEN
                    SFX = AFY(NPY)*(VLW(1,NPY)+VGW(1,NPY))
                  ELSEIF( IOM.EQ.39 ) THEN
                    SFX = AFY(NPY)*(VLW(1,NPY)+VGW(1,NPY)+VNW(1,NPY) )
                  ENDIF
!
!---            Aqueous-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.46 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (VLC(IGC,1,NPY) - VDLC(IGC,1,NPY)*GCPP(1,IGC))
     &              *AFY(NPY)
!
!---            Aqueous-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.47 ) THEN
                  IGC = ISFGC(NS)
                  SFX = VDLC(IGC,1,NPY)*GCPP(1,IGC)*AFY(NPY)
!
!---            Gas-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.48 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (VGC(IGC,1,NPY) - VDGC(IGC,1,NPY)*GCPP(1,IGC))
     &              *AFY(NPY)
!
!---            Gas-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.49 ) THEN
                  IGC = ISFGC(NS)
                  SFX = VDGC(IGC,1,NPY)*GCPP(1,IGC)*AFY(NPY)
!
!---            Total-Advective-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.50 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (VGC(IGC,1,NPY) + VLC(IGC,1,NPY)
     &              + VNC(IGC,1,NPY))*AFY(NPY)
!
!---            Total-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.51 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (VDGC(IGC,1,NPY)
     &              + VDLC(IGC,1,NPY) + VDNC(IGC,1,NPY))
     &              *GCPP(1,IGC)*AFY(NPY)
!
!---            Nonaqueous-Liquid-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.52 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (VNC(IGC,1,NPY) - VDNC(IGC,1,NPY)*GCPP(1,IGC))
     &              *AFY(NPY)
!
!---            Nonaqueous-Liquid-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.53 ) THEN
                  IGC = ISFGC(NS)
                  SFX = VDNC(IGC,1,NPY)*GCPP(1,IGC)*AFY(NPY)
!
!---            Total-Advective-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.54 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (VGC(IGC,1,NPY) + VNC(IGC,1,NPY))*AFY(NPY)
!
!---            Liquid-CO2 CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.55 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFY(NPY)*VNA(1,NPY)
                  ELSE
                    IGC = 1
                    SFX = AFY(NPY)*VNC(IGC,1,NPY)
                  ENDIF
!
!---            Liquid-CO2 CH4 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.56 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFY(NPY)*VNO(1,NPY)
                  ELSE
                    IGC = 2
                    SFX = AFY(NPY)*VNC(IGC,1,NPY)
                  ENDIF
!
!---            Liquid-CO2 H2O Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.57 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFY(NPY)*VNW(1,NPY)
                  ELSE
                    IGC = 3
                    SFX = AFY(NPY)*VNC(IGC,1,NPY)
                  ENDIF
!
!---            Total N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.58 ) THEN
                  SFX = AFY(NPY)*(VLN(1,NPY)+VGN(1,NPY)+VNN(1,NPY))
!
!---            Aqueous N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.59 ) THEN
                  SFX = AFY(NPY)*VLN(1,NPY)
!
!---            Gas N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.60 ) THEN
                  SFX = AFY(NPY)*VGN(1,NPY)
!
!---            Nonaqueous Liquid N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.61 ) THEN
                  SFX = AFY(NPY)*VNN(1,NPY)
!
!---            Gas Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.62 ) THEN
                  IGC = ISFGC(NS)
                  SFX = VGC(IGC,1,NPY)*AFY(NPY)
!
!---            Aqueous Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.63 ) THEN
                  SFX = VLA(1,NPY)*AFY(NPY)
!
!---            Nonaqueous-Liquid Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.64 ) THEN
                  IGC = ISFGC(NS)
                  SFX = VNC(IGC,1,NPY)*AFY(NPY)
!
!---            Total Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.65 ) THEN
                  IGC = ISFGC(NS)
                  IF( IGC.EQ.1 ) THEN
                    SFX = (VLA(1,NPY) + VGC(IGC,1,NPY) + VNC(IGC,1,NPY))
     &                *AFY(NPY)
                  ELSE
                    SFY = (VGC(IGC,1,NPY) + VNC(IGC,1,NPY))*AFY(NPY)
                  ENDIF
                ELSEIF( ISFT(NS).GT.100 .AND. 
     &            ISFT(NS).LE.(100+NSOLU) ) THEN
                  NSL = ISFT(NS)-100
                  SFX = VC(NPY,NSL)*AFY(NPY)

                ELSEIF( ISFT(NS).GT.(100+NSOLU) .AND. 
     &            ISFT(NS).LE.(100+NSOLU+NEQC+NEQK) ) THEN
                  NSL = ISFT(NS)-100
                  SFX = VC(NPY,NSL)*AFY(NPY)*1.D-3






                ENDIF
                RSFDX = REAL(ISFDX)
                SF(1,NS) = SF(1,NS)+SFX*SIGN(1.D+0,RSFDX)
 2400         CONTINUE
 2500       CONTINUE
 2600     CONTINUE
!
!---  West surface  ---
!
        ELSEIF( ISFDX.EQ.-1 ) THEN
          DO 3600 I = ISFCX(1),ISFCX(2)
            DO 3500 J = ISFCX(3),ISFCX(4)
              DO 3400 K = ISFCX(5),ISFCX(6)
                N = ND(I,J,K)
                NPX = NSX(N)
                IF( ISFT(NS).EQ.1 ) THEN
                  SFX = UQ(1,NPX)*AFX(NPX)
                ELSEIF( ISFT(NS).EQ.2 ) THEN
                  SFX = UL(1,NPX)*AFX(NPX)
                ELSEIF( ISFT(NS).EQ.3 ) THEN
                  SFX = UG(1,NPX)*AFX(NPX)
                ELSEIF( ISFT(NS).EQ.4 ) THEN
                  SFX = UN(1,NPX)*AFX(NPX)
                ELSEIF( ISFT(NS).EQ.5 ) THEN
                  IF( UL(1,NPX).GT.ZERO ) THEN
                    IF( I.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHOLX = RHOL(2,N-1)
                    ELSE
                      DO 3050 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHOLX = RHOLB(2,NB)
                          GOTO 3052
                        ENDIF
 3050                 CONTINUE
 3052                 CONTINUE
                    ENDIF
                  ELSE
                    RHOLX = RHOL(2,N)
                  ENDIF
                  SFX = UL(1,NPX)*AFX(NPX)*RHOLX
                ELSEIF( ISFT(NS).EQ.6 ) THEN
                  IF( UG(1,NPX).GT.ZERO ) THEN
                    IF( I.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHOGX = RHOG(2,N-1)
                    ELSE
                      DO 3060 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHOGX = RHOGB(2,NB)
                          GOTO 3062
                        ENDIF
 3060                 CONTINUE
 3062                 CONTINUE
                    ENDIF
                  ELSE
                    RHOGX = RHOG(2,N)
                  ENDIF
                  SFX = UG(1,NPX)*AFX(NPX)*RHOGX
                ELSEIF( ISFT(NS).EQ.7 ) THEN
                  IF( UN(1,NPX).GT.ZERO ) THEN
                    IF( I.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHONX = RHON(2,N-1)
                    ELSE
                      DO 3070 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHONX = RHONB(2,NB)
                          GOTO 3072
                        ENDIF
 3070                 CONTINUE
 3072                CONTINUE
                    ENDIF
                  ELSE
                    RHONX = RHON(2,N)
                  ENDIF
                  SFX = UN(1,NPX)*AFZ(NPX)*RHONX
                ELSEIF( ISFT(NS).EQ.8 ) THEN
                  SFX = US(1,NPX)*AFX(NPX)
                ELSEIF( ISFT(NS).EQ.9 ) THEN
                  SFX = ULO(1,NPX)*AFX(NPX)
                ELSEIF( ISFT(NS).EQ.10 ) THEN
                  IF( UL(1,NPX).GT.ZERO ) THEN
                    IF( I.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHOLX = RHOL(2,N-1)*XLW(2,N-1)
                    ELSE
                      DO 3100 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHOLX = RHOLB(2,NB)*XLWB(2,NB)
                          GOTO 3102
                        ENDIF
 3100                 CONTINUE
 3102                 CONTINUE
                    ENDIF
                  ELSE
                    RHOLX = RHOL(2,N)*XLW(2,N)
                  ENDIF
                  SFX = UL(1,NPX)*AFX(NPX)*RHOLX +
     &              (UDGW(1,NPX)-UDLA(1,NPX)-UDLO(1,NPX))*WTMW*AFX(NPX)
!
!---            Gas-CH4 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.11 ) THEN
                  SFX = AFX(NPX)*UGO(1,NPX)
!
!---            Aqueous-CH4 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.12 ) THEN
                  SFX = AFX(NPX)*ULO(1,NPX)
!
!---            Total-CH4 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.13 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFX(NPX)*(UGO(1,NPX)+ULO(1,NPX)+UNO(1,NPX))
                  ELSE
                    SFX = AFX(NPX)*(UGO(1,NPX)+ULO(1,NPX))
                  ENDIF
                ELSEIF( ISFT(NS).EQ.20 ) THEN
                  IF( UG(1,NPX).GT.ZERO ) THEN
                    IF( I.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-1
                      VAR = (HGW(2,NB)*XGW(2,NB) + HGO(2,NB)*XGO(2,NB)
     &                  + HGA(2,NB)*XGA(2,NB))*RHOG(2,NB)
                    ELSE
                      DO 3200 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = (HGWB(2,NB)*XGWB(2,NB)+HGOB(2,NB)*XGOB(2,NB)
     &                  + HGAB(2,NB)*XGAB(2,NB))*RHOGB(2,NB)
                          GOTO 3201
                        ENDIF
 3200                 CONTINUE
 3201                CONTINUE
                    ENDIF
                  ELSE
                    VAR = (HGW(2,N)*XGW(2,N) + HGO(2,N)*XGO(2,N)
     &                + HGA(2,N)*XGA(2,N))*RHOG(2,N)
                  ENDIF
                  SFX = VAR*UG(1,NPX)*AFX(NPX)
                ELSEIF( ISFT(NS).EQ.21 ) THEN
                  IF( UG(1,NPX).GT.ZERO ) THEN
                    IF( I.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-1
                      VAR = XGW(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 3210 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGWB(2,NB)*RHOGB(2,NB)
                          GOTO 3211
                        ENDIF
 3210                 CONTINUE
 3211                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGW(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*UG(1,NPX)*AFX(NPX)
                ELSEIF( ISFT(NS).EQ.22 ) THEN
                  IF( UG(1,NPX).GT.ZERO ) THEN
                    IF( I.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-1
                      VAR = XGA(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 3220 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGAB(2,NB)*RHOGB(2,NB)
                          GOTO 3221
                        ENDIF
 3220                 CONTINUE
 3221                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGA(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*UG(1,NPX)*AFX(NPX)
                ELSEIF( ISFT(NS).EQ.25 ) THEN
                  IF( UDGW(1,NPX).GT.ZERO ) THEN
                    VARA = HGA(2,N)*WTMA
                    IF( I.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-1
                      VARW = HGW(2,NB)*WTMW
                    ELSE
                      DO 3230 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          VARW = HGWB(2,NB)*WTMW
                          GOTO 3231
                        ENDIF
 3230                 CONTINUE
 3231                CONTINUE
                    ENDIF
                  ELSE
                    VARW = HGW(2,N)*WTMW
                    IF( I.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-1
                      VARA = HGA(2,NB)*WTMA
                    ELSE
                      DO 3232 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          VARA = HGAB(2,NB)*WTMA
                          GOTO 3233
                        ENDIF
 3232                 CONTINUE
 3233                CONTINUE
                    ENDIF
                  ENDIF
                  SFX = UDGW(1,NPX)*(VARW-VARA)*AFX(NPX)
                ELSEIF( ISFT(NS).EQ.26 ) THEN
                  SFX = WTMW*UDGW(1,NPX)*AFX(NPX)
                ELSEIF( ISFT(NS).EQ.27 ) THEN
                  SFX = - WTMA*UDGW(1,NPX)*AFX(NPX)
!
!---            Gas-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.28 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFX(NPX)*UGA(1,NPX)
                  ELSE
                    IF( I.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-1
                      FGAB = XGA(2,NB)*RHOG(2,NB)
                      DXB = DXGF(NB)
                    ELSE
                      DO 3280 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          FGAB = XGAB(2,NB)*RHOGB(2,NB)
                          DXB = DXGF(N)
                          GOTO 3281
                        ENDIF
 3280                 CONTINUE
 3281                 CONTINUE
                    ENDIF
                    FGAP = XGA(2,N)*RHOG(2,N)
                    INDX = 3
                    FGA = DIFMN( FGAB,FGAP,DXB,DXGF(N),UG(1,NPX),INDX )
                    SFX = AFX(NPX)*(UG(1,NPX)*FGA -
     &                WTMA*UDGW(1,NPX))
                  ENDIF
!
!---            Aqueous-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.29 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFX(NPX)*ULA(1,NPX)
                  ELSE
                    IF( I.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-1
                      FLAB = XLA(2,NB)*RHOL(2,NB)
                      DXB = DXGF(NB)
                    ELSE
                      DO 3290 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          FLAB = XLAB(2,NB)*RHOLB(2,NB)
                          DXB = DXGF(N)
                          GOTO 3291
                        ENDIF
 3290                 CONTINUE
 3291                 CONTINUE
                    ENDIF
                    FLAP = XLA(2,N)*RHOL(2,N)
                    INDX = 2
                    FLA = DIFMN( FLAB,FLAP,DXB,DXGF(N),UL(1,NPX),INDX )
                    SFX = AFX(NPX)*(UL(1,NPX)*FLA +
     &                WTMA*UDLA(1,NPX))
                  ENDIF
!
!---            Total-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.30 ) THEN
                  IF( IOM.EQ.36 .OR. IOM.EQ.37 ) THEN
                    SFX = AFX(NPX)*(UGA(1,NPX)+ULA(1,NPX))
                  ELSEIF( IOM.EQ.38 ) THEN
                    SFX = AFX(NPX)*(UGA(1,NPX)+ULA(1,NPX)+UNC(1,1,NPX))
                  ELSEIF( IOM.EQ.39 ) THEN
                    SFX = AFX(NPX)*(UGA(1,NPX)+ULA(1,NPX)+UNA(1,NPX))
                  ELSE
                    IF( I.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-1
                      FGAB = XGA(2,NB)*RHOG(2,NB)
                      FLAB = XLA(2,NB)*RHOL(2,NB)
                      DXB = DXGF(NB)
                    ELSE
                      DO 3300 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          FGAB = XGAB(2,NB)*RHOGB(2,NB)
                          FLAB = XLAB(2,NB)*RHOLB(2,NB)
                          DXB = DXGF(N)
                          GOTO 3301
                        ENDIF
 3300                 CONTINUE
 3301                 CONTINUE
                    ENDIF
                    FGAP = XGA(2,N)*RHOG(2,N)
                    FLAP = XLA(2,N)*RHOL(2,N)
                    INDX = 3
                    FGA = DIFMN( FGAB,FGAP,DXB,DXGF(N),UG(1,NPX),INDX )
                    INDX = 2
                    FLA = DIFMN( FLAB,FLAP,DXB,DXGF(N),UL(1,NPX),INDX )
                    SFX = AFX(NPX)*(UL(1,NPX)*FLA + UG(1,NPX)*FGA 
     &                - WTMA*UDGW(1,NPX) + WTMA*UDLA(1,NPX))
                  ENDIF
!
!---            Gas-Advective Oil-Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.31 ) THEN
                  IF( UG(1,NPX).GT.ZERO ) THEN
                    IF( I.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-1
                      VAR = XGO(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 3310 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGOB(2,NB)*RHOGB(2,NB)
                          GOTO 3311
                        ENDIF
 3310                 CONTINUE
 3311                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGO(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*UG(1,NPX)*AFX(NPX)
!
!---            Gas-Diffusive Oil-Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.32 ) THEN
                  SFX = WTMO*UDGO(1,NPX)*AFX(NPX)
!
!---            Gas-Total Oil-Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.33 ) THEN
                  IF( UG(1,NPX).GT.ZERO ) THEN
                    IF( I.GT.1 ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N-1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N-1
                      VAR = XGO(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 3330 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGOB(2,NB)*RHOGB(2,NB)
                          GOTO 3331
                        ENDIF
 3330                 CONTINUE
 3331                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGO(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*UG(1,NPX)*AFX(NPX)
     &              + WTMO*UDGO(1,NPX)*AFX(NPX)
!
!---            Aqueous-Water Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.43 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFX(NPX)*ULW(1,NPX)
                  ENDIF
!
!---            Gas-Water Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.44 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFX(NPX)*UGW(1,NPX)
                  ENDIF
!
!---            Total-Water Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.45 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.38 ) THEN
                    SFX = AFX(NPX)*(ULW(1,NPX)+UGW(1,NPX))
                  ELSEIF( IOM.EQ.39 ) THEN
                    SFX = AFX(NPX)*(ULW(1,NPX)+UGW(1,NPX)+UNW(1,NPX) )
                  ENDIF
!
!---            Aqueous-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.46 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (ULC(IGC,1,NPX) - UDLC(IGC,1,NPX)*GCPP(1,IGC))
     &              *AFX(NPX)
!
!---            Aqueous-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.47 ) THEN
                  IGC = ISFGC(NS)
                  SFX = UDLC(IGC,1,NPX)*GCPP(1,IGC)*AFX(NPX)
!
!---            Gas-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.48 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (UGC(IGC,1,NPX) - UDGC(IGC,1,NPX)*GCPP(1,IGC))
     &              *AFX(NPX)
!
!---            Gas-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.49 ) THEN
                  IGC = ISFGC(NS)
                  SFX = UDGC(IGC,1,NPX)*GCPP(1,IGC)*AFX(NPX)
!
!---            Total-Advective-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.50 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (UGC(IGC,1,NPX) + ULC(IGC,1,NPX)
     &              + UNC(IGC,1,NPX))*AFX(NPX)
!
!---            Total-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.51 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (UDGC(IGC,1,NPX)
     &              + UDLC(IGC,1,NPX) + UDNC(IGC,1,NPX))
     &              *GCPP(1,IGC)*AFX(NPX)
!
!---            Nonaqueous-Liquid-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.52 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (UNC(IGC,1,NPX) - UDNC(IGC,1,NPX)*GCPP(1,IGC))
     &              *AFX(NPX)
!
!---            Nonaqueous-Liquid-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.53 ) THEN
                  IGC = ISFGC(NS)
                  SFX = UDNC(IGC,1,NPX)*GCPP(1,IGC)*AFX(NPX)
!
!---            Total-Advective-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.54 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (UGC(IGC,1,NPX) + UNC(IGC,1,NPX))*AFX(NPX)
!
!---            Liquid-CO2 CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.55 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFX(NPX)*UNA(1,NPX)
                  ELSE
                    IGC = 1
                    SFX = AFX(NPX)*UNC(IGC,1,NPX)
                  ENDIF
!
!---            Liquid-CO2 CH4 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.56 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFX(NPX)*UNO(1,NPX)
                  ELSE
                    IGC = 2
                    SFX = AFX(NPX)*UNC(IGC,1,NPX)
                  ENDIF
!
!---            Liquid-CO2 H2O Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.57 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFX(NPX)*UNW(1,NPX)
                  ELSE
                    IGC = 3
                    SFX = AFX(NPX)*UNC(IGC,1,NPX)
                  ENDIF
!
!---            Total N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.58 ) THEN
                  SFX = AFX(NPX)*(ULN(1,NPX)+UGN(1,NPX)+UNN(1,NPX))
!
!---            Aqueous N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.59 ) THEN
                  SFX = AFX(NPX)*ULN(1,NPX)
!
!---            Gas N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.60 ) THEN
                  SFX = AFX(NPX)*UGN(1,NPX)
!
!---            Nonaqueous Liquid N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.61 ) THEN
                  SFX = AFX(NPX)*UNN(1,NPX)
!
!---            Gas Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.62 ) THEN
                  IGC = ISFGC(NS)
                  SFX = UGC(IGC,1,NPX)*AFX(NPX)
!
!---            Aqueous Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.63 ) THEN
                  SFX = ULA(1,NPX)*AFX(NPX)
!
!---            Nonaqueous-Liquid Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.64 ) THEN
                  IGC = ISFGC(NS)
                  SFX = UNC(IGC,1,NPX)*AFX(NPX)
!
!---            Total Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.65 ) THEN
                  IGC = ISFGC(NS)
                  IF( IGC.EQ.1 ) THEN
                    SFX = (ULA(1,NPX) + UGC(IGC,1,NPX) + UNC(IGC,1,NPX))
     &                *AFX(NPX)
                  ELSE
                    SFX = (UGC(IGC,1,NPX) + UNC(IGC,1,NPX))*AFX(NPX)
                  ENDIF
                ELSEIF( ISFT(NS).GT.100 .AND. 
     &            ISFT(NS).LE.(100+NSOLU) ) THEN
                  NSL = ISFT(NS)-100
                  SFX = VC(NPY,NSL)*AFY(NPY)
                ELSEIF( ISFT(NS).GT.100 .AND. 
     &            ISFT(NS).LE.(100+NSOLU) ) THEN
                  NSL = ISFT(NS)-100
                  SFX = UC(NPX,NSL)*AFX(NPX)

                ELSEIF( ISFT(NS).GT.(100+NSOLU) .AND. 
     &            ISFT(NS).LE.(100+NSOLU+NEQC+NEQK) ) THEN
                  NSL = ISFT(NS)-100
                  SFX = UC(NPX,NSL)*AFX(NPX)*1.D-3






                ENDIF
                RSFDX = REAL(ISFDX)
                SF(1,NS) = SF(1,NS)+SFX*SIGN(1.D+0,RSFDX)
 3400         CONTINUE
 3500       CONTINUE
 3600     CONTINUE
!
!---  East surface  ---
!
        ELSEIF( ISFDX.EQ.1 ) THEN
          DO 4600 I = ISFCX(1),ISFCX(2)
            DO 4500 J = ISFCX(3),ISFCX(4)
              DO 4400 K = ISFCX(5),ISFCX(6)
                N = ND(I,J,K)
                NQX = NSX(N)+1
                IF( INBS(4,N).NE.0 ) NQX = INBS(4,N)
                IF( ISFT(NS).EQ.1 ) THEN
                  SFX = UQ(1,NQX)*AFX(NQX)
                ELSEIF( ISFT(NS).EQ.2 ) THEN
                  SFX = UL(1,NQX)*AFX(NQX)
                ELSEIF( ISFT(NS).EQ.3 ) THEN
                  SFX = UG(1,NQX)*AFX(NQX)
                ELSEIF( ISFT(NS).EQ.4 ) THEN
                  SFX = UN(1,NQX)*AFX(NQX)
                ELSEIF( ISFT(NS).EQ.5 ) THEN
                  IF( UL(1,NQX).LT.ZERO ) THEN
                    IF( I.LT.IFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHOLX = RHOL(2,N+1)
                    ELSE
                      DO 4050 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHOLX = RHOLB(2,NB)
                          GOTO 4052
                        ENDIF
 4050                 CONTINUE
 4052                 CONTINUE
                    ENDIF
                  ELSE
                    RHOLX = RHOL(2,N)
                  ENDIF
                  SFX = UL(1,NQX)*AFX(NQX)*RHOLX
                ELSEIF( ISFT(NS).EQ.6 ) THEN
                  IF( UG(1,NQX).LT.ZERO ) THEN
                    IF( I.LT.IFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHOGX = RHOG(2,N+1)
                    ELSE
                      DO 4060 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHOGX = RHOGB(2,NB)
                          GOTO 4062
                        ENDIF
 4060                 CONTINUE
 4062                 CONTINUE
                    ENDIF
                  ELSE
                    RHOGX = RHOG(2,N)
                  ENDIF
                  SFX = UG(1,NQX)*AFX(NQX)*RHOGX
                ELSEIF( ISFT(NS).EQ.7 ) THEN
                  IF( UN(1,NQX).LT.ZERO ) THEN
                    IF( I.LT.IFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHONX = RHON(2,N+1)
                    ELSE
                      DO 4070 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHONX = RHONB(2,NB)
                          GOTO 4072
                        ENDIF
 4070                 CONTINUE
 4072                 CONTINUE
                    ENDIF
                  ELSE
                    RHONX = RHON(2,N)
                  ENDIF
                  SFX = UN(1,NQX)*AFX(NQX)*RHONX
                ELSEIF( ISFT(NS).EQ.8 ) THEN
                  SFX = US(1,NQX)*AFX(NQX)
                ELSEIF( ISFT(NS).EQ.9 ) THEN
                  SFX = ULO(1,NQX)*AFX(NQX)
                ELSEIF( ISFT(NS).EQ.10 ) THEN
                  IF( UL(1,NQX).LT.ZERO ) THEN
                    IF( I.LT.IFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHOLX = RHOL(2,N+1)*XLW(2,N+1)
                    ELSE
                      DO 4100 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHOLX = RHOLB(2,NB)*XLWB(2,NB)
                          GOTO 4102
                        ENDIF
 4100                 CONTINUE
 4102                 CONTINUE
                    ENDIF
                  ELSE
                    RHOLX = RHOL(2,N)*XLW(2,N)
                  ENDIF
                  SFX = UL(1,NQX)*AFX(NQX)*RHOLX +
     &              (UDGW(1,NQX)-UDLA(1,NQX)-UDLO(1,NQX))*WTMW*AFX(NQX)
!
!---            Gas-CH4 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.11 ) THEN
                  SFX = AFX(NQX)*UGO(1,NQX)
!
!---            Aqueous-CH4 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.12 ) THEN
                  SFX = AFX(NQX)*ULO(1,NQX)
!
!---            Total-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.13 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFX(NQX)*(UGO(1,NQX)+ULO(1,NQX)+UNO(1,NQX))
                  ELSE
                    SFX = AFX(NQX)*(UGO(1,NQX)+ULO(1,NQX))
                  ENDIF
                ELSEIF( ISFT(NS).EQ.20 ) THEN
                  IF( UG(1,NQX).LT.ZERO ) THEN
                    IF( I.LT.IFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+1
                      VAR = (HGW(2,NB)*XGW(2,NB) + HGO(2,NB)*XGO(2,NB)
     &                  + HGA(2,NB)*XGA(2,NB))*RHOG(2,NB)
                    ELSE
                      DO 4200 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = (HGWB(2,NB)*XGWB(2,NB)+HGOB(2,NB)*XGOB(2,NB)
     &                  + HGAB(2,NB)*XGAB(2,NB))*RHOGB(2,NB)
                          GOTO 4201
                        ENDIF
 4200                 CONTINUE
 4201                CONTINUE
                    ENDIF
                  ELSE
                    VAR = (HGW(2,N)*XGW(2,N) + HGO(2,N)*XGO(2,N)
     &                + HGA(2,N)*XGA(2,N))*RHOG(2,N)
                  ENDIF
                  SFX = VAR*UG(1,NQX)*AFX(NQX)
                ELSEIF( ISFT(NS).EQ.21 ) THEN
                  IF( UG(1,NQX).LT.ZERO ) THEN
                    IF( I.LT.IFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+1
                      VAR = XGW(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 4210 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGWB(2,NB)*RHOGB(2,NB)
                          GOTO 4211
                        ENDIF
 4210                 CONTINUE
 4211                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGW(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*UG(1,NQX)*AFX(NQX)
                ELSEIF( ISFT(NS).EQ.22 ) THEN
                  IF( UG(1,NQX).LT.ZERO ) THEN
                    IF( I.LT.IFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+1
                      VAR = XGA(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 4220 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGAB(2,NB)*RHOGB(2,NB)
                          GOTO 4221
                        ENDIF
 4220                 CONTINUE
 4221                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGA(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*UG(1,NQX)*AFX(NQX)
                ELSEIF( ISFT(NS).EQ.25 ) THEN
                  IF( UDGW(1,NQX).LT.ZERO ) THEN
                    VARA = HGA(2,N)*WTMA
                    IF( I.LT.IFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+1
                      VARW = HGW(2,NB)*WTMW
                    ELSE
                      DO 4230 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          VARW = HGWB(2,NB)*WTMW
                          GOTO 4231
                        ENDIF
 4230                 CONTINUE
 4231                CONTINUE
                    ENDIF
                  ELSE
                    VARW = HGW(2,N)*WTMW
                    IF( I.LT.IFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+1
                      VARA = HGA(2,NB)*WTMA
                    ELSE
                      DO 4232 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          VARA = HGAB(2,NB)*WTMA
                          GOTO 4233
                        ENDIF
 4232                 CONTINUE
 4233                CONTINUE
                    ENDIF
                  ENDIF
                  SFX = UDGW(1,NQX)*(VARW-VARA)*AFX(NQX)
                ELSEIF( ISFT(NS).EQ.26 ) THEN
                  SFX = WTMW*UDGW(1,NQX)*AFX(NQX)
                ELSEIF( ISFT(NS).EQ.27 ) THEN
                  SFX = - WTMA*UDGW(1,NQX)*AFX(NQX)
!
!---            Gas-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.28 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFX(NQX)*UGA(1,NQX)
                  ELSE
                  IF( I.LT.IFLD ) THEN
                    IFLAG = .TRUE.
                    IF( IXP(N+1).EQ.0 ) IFLAG = .FALSE.
                  ELSE
                    IFLAG = .FALSE.
                  ENDIF
                  IF( IFLAG ) THEN
                    NB = N+1
                    FGAB = XGA(2,NB)*RHOG(2,NB)
                    DXB = DXGF(NB)
                  ELSE
                    DO 4280 NB = 1,NBC
                      IF( IBCN(NB).EQ.N ) THEN
                        FGAB = XGAB(2,NB)*RHOGB(2,NB)
                        DXB = DXGF(N)
                        GOTO 4281
                      ENDIF
 4280               CONTINUE
 4281              CONTINUE
                  ENDIF
                  FGAP = XGA(2,N)*RHOG(2,N)
                  INDX = 3
                  FGA = DIFMN( FGAP,FGAB,DXGF(N),DXB,UG(1,NQX),INDX )
                  SFX = AFX(NQX)*(UG(1,NQX)*FGA -
     &              WTMA*UDGW(1,NQX))
                  ENDIF
!
!---            Aqueous-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.29 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFX(NQX)*ULA(1,NQX)
                  ELSE
                  IF( I.LT.IFLD ) THEN
                    IFLAG = .TRUE.
                    IF( IXP(N+1).EQ.0 ) IFLAG = .FALSE.
                  ELSE
                    IFLAG = .FALSE.
                  ENDIF
                  IF( IFLAG ) THEN
                    NB = N+1
                    FLAB = XLA(2,NB)*RHOL(2,NB)
                    DXB = DXGF(NB)
                  ELSE
                    DO 4290 NB = 1,NBC
                      IF( IBCN(NB).EQ.N ) THEN
                        FLAB = XLAB(2,NB)*RHOLB(2,NB)
                        DXB = DXGF(N)
                        GOTO 4291
                      ENDIF
 4290               CONTINUE
 4291              CONTINUE
                  ENDIF
                  FLAP = XLA(2,N)*RHOL(2,N)
                  INDX = 2
                  FLA = DIFMN( FLAP,FLAB,DXGF(N),DXB,UL(1,NQX),INDX )
                  SFX = AFX(NQX)*(UL(1,NQX)*FLA +
     &              WTMA*UDLA(1,NQX))
                  ENDIF
!
!---            Total-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.30 ) THEN
                  IF( IOM.EQ.36 .OR. IOM.EQ.37 ) THEN
                    SFX = AFX(NQX)*(UGA(1,NQX)+ULA(1,NQX))
                  ELSEIF( IOM.EQ.38 ) THEN
                    SFX = AFX(NQX)*(UGA(1,NQX)+ULA(1,NQX)+UNC(1,1,NQX))
                  ELSEIF( IOM.EQ.39 ) THEN
                    SFX = AFX(NQX)*(UGA(1,NQX)+ULA(1,NQX)+UNA(1,NQX))
                  ELSE
                    IF( I.LT.IFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+1
                      FGAB = XGA(2,NB)*RHOG(2,NB)
                      FLAB = XLA(2,NB)*RHOL(2,NB)
                      DXB = DXGF(NB)
                    ELSE
                      DO 4300 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          FGAB = XGAB(2,NB)*RHOGB(2,NB)
                          FLAB = XLAB(2,NB)*RHOLB(2,NB)
                          DXB = DXGF(N)
                          GOTO 4301
                        ENDIF
 4300                 CONTINUE
 4301                 CONTINUE
                    ENDIF
                    FGAP = XGA(2,N)*RHOG(2,N)
                    FLAP = XLA(2,N)*RHOL(2,N)
                    INDX = 3
                    FGA = DIFMN( FGAP,FGAB,DXGF(N),DXB,UG(1,NQX),INDX )
                    INDX = 2
                    FLA = DIFMN( FLAP,FLAB,DXGF(N),DXB,UL(1,NQX),INDX )
                    SFX = AFX(NQX)*(UL(1,NQX)*FLA + UG(1,NQX)*FGA 
     &                - WTMA*UDGW(1,NQX) + WTMA*UDLA(1,NQX))
                  ENDIF
!
!---            Gas-Advective Oil-Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.31 ) THEN
                  IF( UG(1,NQX).LT.ZERO ) THEN
                    IF( I.LT.IFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+1
                      VAR = XGO(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 4310 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGOB(2,NB)*RHOGB(2,NB)
                          GOTO 4311
                        ENDIF
 4310                 CONTINUE
 4311                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGO(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*UG(1,NQX)*AFX(NQX)
!
!---            Gas-Diffusive Oil-Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.32 ) THEN
                  SFX = WTMO*UDGO(1,NQX)*AFX(NQX)
!
!---            Gas-Total Oil-Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.33 ) THEN
                  IF( UG(1,NQX).LT.ZERO ) THEN
                    IF( I.LT.IFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+1).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+1
                      VAR = XGO(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 4330 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGOB(2,NB)*RHOGB(2,NB)
                          GOTO 4331
                        ENDIF
 4330                 CONTINUE
 4331                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGO(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*UG(1,NQX)*AFX(NQX)
     &              + WTMO*UDGO(1,NQX)*AFX(NQX)
!
!---            Aqueous-Water Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.43 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFX(NQX)*ULW(1,NQX)
                  ENDIF
!
!---            Gas-Water Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.44 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFX(NQX)*UGW(1,NQX)
                  ENDIF
!
!---            Total-Water Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.45 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.38 ) THEN
                    SFX = AFX(NQX)*(ULW(1,NQX)+UGW(1,NQX))
                  ELSEIF( IOM.LE.39 ) THEN
                    SFX = AFX(NQX)*(ULW(1,NQX)+UGW(1,NQX)+UNW(1,NQX))
                  ENDIF
!
!---            Aqueous-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.46 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (ULC(IGC,1,NQX) - UDLC(IGC,1,NQX)*GCPP(1,IGC))
     &              *AFX(NQX)
!
!---            Aqueous-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.47 ) THEN
                  IGC = ISFGC(NS)
                  SFX = UDLC(IGC,1,NQX)*GCPP(1,IGC)*AFX(NQX)
!
!---            Gas-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.48 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (UGC(IGC,1,NQX) - UDGC(IGC,1,NQX)*GCPP(1,IGC))
     &              *AFX(NQX)
!
!---            Gas-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.49 ) THEN
                  IGC = ISFGC(NS)
                  SFX = UDGC(IGC,1,NQX)*GCPP(1,IGC)*AFX(NQX)
!
!---            Total-Advective-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.50 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (UGC(IGC,1,NQX) + ULC(IGC,1,NQX)
     &              + UNC(IGC,1,NQX))*AFX(NQX)
!
!---            Total-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.51 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (UDGC(IGC,1,NQX) 
     &              + UDLC(IGC,1,NQX) + UDNC(IGC,1,NQX))
     &              *GCPP(1,IGC)*AFX(NQX)
!
!---            Nonaqueous-Liquid-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.52 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (UNC(IGC,1,NQX) - UDNC(IGC,1,NQX)*GCPP(1,IGC))
     &              *AFX(NQX)
!
!---            Nonaqueous-Liquid-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.53 ) THEN
                  IGC = ISFGC(NS)
                  SFX = UDNC(IGC,1,NQX)*GCPP(1,IGC)*AFX(NQX)
!
!---            Total-Advective-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.54 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (UGC(IGC,1,NQX) + UNC(IGC,1,NQX))*AFX(NQX)
!
!---            Liquid-CO2 CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.55 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFX(NQX)*UNA(1,NQX)
                  ELSE
                    IGC = 1
                    SFX = AFX(NQX)*UNC(IGC,1,NQX)
                  ENDIF
!
!---            Liquid-CO2 CH4 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.56 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFX(NQX)*UNO(1,NQX)
                  ELSE
                    IGC = 2
                    SFX = AFX(NQX)*UNC(IGC,1,NQX)
                  ENDIF
!
!---            Liquid-CO2 H2O Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.57 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFX(NQX)*UNW(1,NQX)
                  ELSE
                    IGC = 3
                    SFX = AFX(NQX)*UNC(IGC,1,NQX)
                  ENDIF
!
!---            Total N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.58 ) THEN
                  SFX = AFX(NQX)*(ULN(1,NQX)+UGN(1,NQX)+UNN(1,NQX))
!
!---            Aqueous N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.59 ) THEN
                  SFX = AFX(NQX)*ULN(1,NQX)
!
!---            Gas N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.60 ) THEN
                  SFX = AFX(NQX)*UGN(1,NQX)
!
!---            Nonaqueous Liquid N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.61 ) THEN
                  SFX = AFX(NQX)*UNN(1,NQX)
!
!---            Gas Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.62 ) THEN
                  IGC = ISFGC(NS)
                  SFX = UGC(IGC,1,NQX)*AFX(NQX)
!
!---            Aqueous Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.63 ) THEN
                  SFX = ULA(1,NQX)*AFX(NQX)
!
!---            Nonaqueous-Liquid Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.64 ) THEN
                  IGC = ISFGC(NS)
                  SFX = UNC(IGC,1,NQX)*AFX(NQX)
!
!---            Total Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.65 ) THEN
                  IGC = ISFGC(NS)
                  IF( IGC.EQ.1 ) THEN
                    SFX = (ULA(1,NQX) + UGC(IGC,1,NQX) + UNC(IGC,1,NQX))
     &                *AFX(NQX)
                  ELSE
                    SFX = (UGC(IGC,1,NQX) + UNC(IGC,1,NQX))*AFX(NQX)
                  ENDIF
                ELSEIF( ISFT(NS).GT.100 .AND. 
     &            ISFT(NS).LE.(100+NSOLU) ) THEN
                  NSL = ISFT(NS)-100
                  SFX = UC(NQX,NSL)*AFX(NQX)
                ELSEIF( ISFT(NS).GT.100 .AND. 
     &            ISFT(NS).LE.(100+NSOLU) ) THEN
                  NSL = ISFT(NS)-100
                  SFX = UC(NQX,NSL)*AFX(NQX)

                ELSEIF( ISFT(NS).GT.(100+NSOLU) .AND. 
     &            ISFT(NS).LE.(100+NSOLU+NEQC+NEQK) ) THEN
                  NSL = ISFT(NS)-100
                  SFX = UC(NQX,NSL)*AFX(NQX)*1.D-3






                ENDIF
                RSFDX = REAL(ISFDX)
                SF(1,NS) = SF(1,NS)+SFX*SIGN(1.D+0,RSFDX)
 4400         CONTINUE
 4500       CONTINUE
 4600     CONTINUE
!
!---  North surface  ---
!
        ELSEIF( ISFDX.EQ.2 ) THEN
          DO 5600 I = ISFCX(1),ISFCX(2)
            DO 5500 J = ISFCX(3),ISFCX(4)
              DO 5400 K = ISFCX(5),ISFCX(6)
                N = ND(I,J,K)
                NQY = NSY(N)+IFLD
                IF( INBS(5,N).NE.0 ) NQY = INBS(5,N)
                IF( ISFT(NS).EQ.1 ) THEN
                  SFX = VQ(1,NQY)*AFY(NQY)
                ELSEIF( ISFT(NS).EQ.2 ) THEN
                  SFX = VL(1,NQY)*AFY(NQY)
                ELSEIF( ISFT(NS).EQ.3 ) THEN
                  SFX = VG(1,NQY)*AFY(NQY)
                ELSEIF( ISFT(NS).EQ.4 ) THEN
                  SFX = VN(1,NQY)*AFY(NQY)
                ELSEIF( ISFT(NS).EQ.5 ) THEN
                  IF( VL(1,NQY).LT.ZERO ) THEN
                    IF( J.LT.JFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHOLX = RHOL(2,N+IFLD)
                    ELSE
                      DO 5050 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHOLX = RHOLB(2,NB)
                          GOTO 5052
                        ENDIF
 5050                 CONTINUE
 5052                 CONTINUE
                    ENDIF
                  ELSE
                    RHOLX = RHOL(2,N)
                  ENDIF
                  SFX = VL(1,NQY)*AFY(NQY)*RHOLX
                ELSEIF( ISFT(NS).EQ.6 ) THEN
                  IF( VG(1,NQY).LT.ZERO ) THEN
                    IF( J.LT.JFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHOGX = RHOG(2,N+IFLD)
                    ELSE
                      DO 5060 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHOGX = RHOGB(2,NB)
                          GOTO 5062
                        ENDIF
 5060                 CONTINUE
 5062                 CONTINUE
                    ENDIF
                  ELSE
                    RHOGX = RHOG(2,N)
                  ENDIF
                  SFX = VG(1,NQY)*AFY(NQY)*RHOGX
                ELSEIF( ISFT(NS).EQ.-6 ) THEN
                  IF( VG(1,NQY).LT.ZERO ) THEN
                    IF( J.LT.JFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHOGX = RHOG(2,N+IFLD)
                      XGWX = XGW(2,N+IFLD)
                    ELSE
                      DO 5064 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHOGX = RHOGB(2,NB)
                          XGWX = XGWB(2,NB)
                          GOTO 5066
                        ENDIF
 5064                 CONTINUE
 5066                 CONTINUE
                    ENDIF
                  ELSE
                    RHOGX = RHOG(2,N)
                    XGWX = XGW(2,N)
                  ENDIF
                  SFX = VG(1,NQY)*AFY(NQY)*RHOGX*XGWX
     &              + VDGW(1,NQY)*WTMW*AFY(NQY)
                ELSEIF( ISFT(NS).EQ.7 ) THEN
                  IF( VN(1,NQY).LT.ZERO ) THEN
                    IF( J.LT.JFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHONX = RHON(2,N+IFLD)
                    ELSE
                      DO 5070 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHONX = RHONB(2,NB)
                          GOTO 5072
                        ENDIF
 5070                 CONTINUE
 5072                 CONTINUE
                    ENDIF
                  ELSE
                    RHONX = RHON(2,N)
                  ENDIF
                  SFX = VN(1,NQY)*AFY(NQY)*RHONX
                ELSEIF( ISFT(NS).EQ.8 ) THEN
                  SFX = VS(1,NQY)*AFY(NQY)
                ELSEIF( ISFT(NS).EQ.9 ) THEN
                  SFX = VLO(1,NQY)*AFY(NQY)
                ELSEIF( ISFT(NS).EQ.10 ) THEN
                  IF( VL(1,NQY).LT.ZERO ) THEN
                    IF( J.LT.JFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHOLX = RHOL(2,N+IFLD)*XLW(2,N+IFLD)
                    ELSE
                      DO 5100 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHOLX = RHOLB(2,NB)*XLWB(2,NB)
                          GOTO 5102
                        ENDIF
 5100                 CONTINUE
 5102                 CONTINUE
                    ENDIF
                  ELSE
                    RHOLX = RHOL(2,N)*XLW(2,N)
                  ENDIF
                  SFX = VL(1,NQY)*AFY(NQY)*RHOLX +
     &              (UDGW(1,NQY)-UDLA(1,NQY)-UDLO(1,NQY))*WTMW*AFY(NQY)
!
!---            Gas-CH4 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.11 ) THEN
                  SFX = AFY(NQY)*VGO(1,NQY)
!
!---            Aqueous-CH4 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.12 ) THEN
                  SFX = AFY(NQY)*VLO(1,NQY)
!
!---            Total-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.13 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFY(NQY)*(VGO(1,NQY)+VLO(1,NQY)+VNO(1,NQY))
                  ELSE
                    SFX = AFY(NQY)*(VGO(1,NQY)+VLO(1,NQY))
                  ENDIF
                ELSEIF( ISFT(NS).EQ.20 ) THEN
                  IF( VG(1,NQY).LT.ZERO ) THEN
                    IF( J.LT.JFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+IFLD
                      VAR = (HGW(2,NB)*XGW(2,NB) + HGO(2,NB)*XGO(2,NB)
     &                  + HGA(2,NB)*XGA(2,NB))*RHOG(2,NB)
                    ELSE
                      DO 5200 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = (HGWB(2,NB)*XGWB(2,NB)+HGOB(2,NB)*XGOB(2,NB)
     &                  + HGAB(2,NB)*XGAB(2,NB))*RHOGB(2,NB)
                          GOTO 5201
                        ENDIF
 5200                 CONTINUE
 5201                CONTINUE
                    ENDIF
                  ELSE
                    VAR = (HGW(2,N)*XGW(2,N) + HGO(2,N)*XGO(2,N)
     &                + HGA(2,N)*XGA(2,N))*RHOG(2,N)
                  ENDIF
                  SFX = VAR*VG(1,NQY)*AFY(NQY)
                ELSEIF( ISFT(NS).EQ.21 ) THEN
                  IF( VG(1,NQY).LT.ZERO ) THEN
                    IF( J.LT.JFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+IFLD
                      VAR = XGW(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 5210 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGWB(2,NB)*RHOGB(2,NB)
                          GOTO 5211
                        ENDIF
 5210                 CONTINUE
 5211                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGW(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*VG(1,NQY)*AFY(NQY)
                ELSEIF( ISFT(NS).EQ.22 ) THEN
                  IF( VG(1,NQY).LT.ZERO ) THEN
                    IF( J.LT.JFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+IFLD
                      VAR = XGA(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 5220 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGAB(2,NB)*RHOGB(2,NB)
                          GOTO 5221
                        ENDIF
 5220                 CONTINUE
 5221                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGA(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*VG(1,NQY)*AFY(NQY)
                ELSEIF( ISFT(NS).EQ.25 ) THEN
                  IF( VDGW(1,NQY).LT.ZERO ) THEN
                    VARA = HGA(2,N)*WTMA
                    IF( J.LT.JFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+IFLD
                      VARW = HGW(2,NB)*WTMW
                    ELSE
                      DO 5230 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          VARW = HGWB(2,NB)*WTMW
                          GOTO 5231
                        ENDIF
 5230                 CONTINUE
 5231                CONTINUE
                    ENDIF
                  ELSE
                    VARW = HGW(2,N)*WTMW
                    IF( J.LT.JFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+IFLD
                      VARA = HGA(2,NB)*WTMA
                    ELSE
                      DO 5232 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          VARA = HGAB(2,NB)*WTMA
                          GOTO 5233
                        ENDIF
 5232                 CONTINUE
 5233                CONTINUE
                    ENDIF
                  ENDIF
                  SFX = VDGW(1,NQY)*(VARW-VARA)*AFY(NQY)
                ELSEIF( ISFT(NS).EQ.26 ) THEN
                  SFX = WTMW*VDGW(1,NQY)*AFY(NQY)
                ELSEIF( ISFT(NS).EQ.27 ) THEN
                  SFX = - WTMA*VDGW(1,NQY)*AFY(NQY)
!
!---            Gas-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.28 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFY(NQY)*VGA(1,NQY)
                  ELSE
                    IF( J.LT.JFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+IFLD
                      FGAB = XGA(2,NB)*RHOG(2,NB)
                      DYB = DYGF(NB)
                    ELSE
                      DO 5280 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          FGAB = XGAB(2,NB)*RHOGB(2,NB)
                          DYB = DYGF(N)
                          GOTO 5281
                        ENDIF
 5280                 CONTINUE
 5281                 CONTINUE
                    ENDIF
                    FGAP = XGA(2,N)*RHOG(2,N)
                    INDX = 3
                    FGA = DIFMN( FGAP,FGAB,DYGF(N),DYB,VG(1,NQY),INDX )
                    SFX = AFY(NQY)*(VG(1,NQY)*FGA -
     &                WTMA*VDGW(1,NQY))
                  ENDIF
!
!---            Aqueous-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.29 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFY(NQY)*VLA(1,NQY)
                  ELSE
                    IF( J.LT.JFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+IFLD
                      FLAB = XLA(2,NB)*RHOL(2,NB)
                      DYB = DYGF(NB)
                    ELSE
                      DO 5290 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          FLAB = XLAB(2,NB)*RHOLB(2,NB)
                          DYB = DYGF(N)
                          GOTO 5291
                        ENDIF
 5290                 CONTINUE
 5291                 CONTINUE
                    ENDIF
                    FLAP = XLA(2,N)*RHOL(2,N)
                    INDX = 2
                    FLA = DIFMN( FLAP,FLAB,DYGF(N),DYB,VL(1,NQY),INDX )
                    SFX = AFY(NQY)*(VL(1,NQY)*FLA +
     &                WTMA*VDLA(1,NQY))
                  ENDIF
!
!---            Total-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.30 ) THEN
                  IF( IOM.EQ.36 .OR. IOM.EQ.37 ) THEN
                    SFX = AFY(NQY)*(VGA(1,NQY)+VLA(1,NQY))
                  ELSEIF( IOM.EQ.38 ) THEN
                    SFX = AFY(NQY)*(VGA(1,NQY)+VLA(1,NQY)+VNC(1,1,NQY))
                  ELSEIF( IOM.EQ.39 ) THEN
                    SFX = AFY(NQY)*(VGA(1,NQY)+VLA(1,NQY)+VNA(1,NQY))
                  ELSE
                    IF( J.LT.JFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+IFLD
                      FGAB = XGA(2,NB)*RHOG(2,NB)
                      FLAB = XLA(2,NB)*RHOL(2,NB)
                      DYB = DYGF(NB)
                    ELSE
                      DO 5300 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          FGAB = XGAB(2,NB)*RHOGB(2,NB)
                          FLAB = XLAB(2,NB)*RHOLB(2,NB)
                          DYB = DYGF(N)
                          GOTO 5301
                        ENDIF
 5300                 CONTINUE
 5301                 CONTINUE
                    ENDIF
                    FGAP = XGA(2,N)*RHOG(2,N)
                    FLAP = XLA(2,N)*RHOL(2,N)
                    INDX = 3
                    FGA = DIFMN( FGAP,FGAB,DYGF(N),DYB,VG(1,NQY),INDX )
                    INDX = 2
                    FLA = DIFMN( FLAP,FLAB,DYGF(N),DYB,VL(1,NQY),INDX )
                    SFX = AFY(NQY)*(VL(1,NQY)*FLA + VG(1,NQY)*FGA 
     &                - WTMA*VDGW(1,NQY) + WTMA*VDLA(1,NQY))
                  ENDIF
!
!---            Gas-Advective Oil-Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.31 ) THEN
                  IF( VG(1,NQY).LT.ZERO ) THEN
                    IF( J.LT.JFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+IFLD
                      VAR = XGO(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 5310 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGOB(2,NB)*RHOGB(2,NB)
                          GOTO 5311
                        ENDIF
 5310                 CONTINUE
 5311                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGO(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*VG(1,NQY)*AFY(NQY)
!
!---            Gas-Diffusive Oil-Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.32 ) THEN
                  SFX = WTMO*VDGO(1,NQY)*AFY(NQY)
!
!---            Gas-Total Oil-Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.33 ) THEN
                  IF( VG(1,NQY).LT.ZERO ) THEN
                    IF( J.LT.JFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+IFLD
                      VAR = XGO(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 5330 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGOB(2,NB)*RHOGB(2,NB)
                          GOTO 5331
                        ENDIF
 5330                 CONTINUE
 5331                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGO(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*VG(1,NQY)*AFY(NQY)
     &              + WTMO*VDGO(1,NQY)*AFY(NQY)
!
!---            Aqueous-Water Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.43 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFY(NQY)*VLW(1,NQY)
                  ENDIF
!
!---            Gas-Water Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.44 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFY(NQY)*VGW(1,NQY)
                  ENDIF
!
!---            Total-Water Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.45 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.38 ) THEN
                    SFX = AFY(NQY)*(VLW(1,NQY)+VGW(1,NQY))
                  ELSEIF( IOM.EQ.39 ) THEN
                    SFX = AFY(NQY)*(VLW(1,NQY)+VGW(1,NQY)+VNW(1,NQY))
                  ENDIF
!
!---            Aqueous-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.46 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (VLC(IGC,1,NQY) - VDLC(IGC,1,NQY)*GCPP(1,IGC))
     &              *AFY(NQY)
!
!---            Aqueous-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.47 ) THEN
                  IGC = ISFGC(NS)
                  SFX = VDLC(IGC,1,NQY)*GCPP(1,IGC)*AFY(NQY)
!
!---            Gas-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.48 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (VGC(IGC,1,NQY) - VDGC(IGC,1,NQY)*GCPP(1,IGC))
     &              *AFY(NQY)
!
!---            Gas-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.49 ) THEN
                  IGC = ISFGC(NS)
                  SFX = VDGC(IGC,1,NQY)*GCPP(1,IGC)*AFY(NQY)
!
!---            Total-Advective-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.50 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (VGC(IGC,1,NQY) + VLC(IGC,1,NQY)
     &              + VNC(IGC,1,NQY))*AFY(NQY)
!
!---            Total-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.51 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (VDGC(IGC,1,NQY) 
     &              + VDLC(IGC,1,NQY) + VDNC(IGC,1,NQY))
     &              *GCPP(1,IGC)*AFY(NQY)
!
!---            Nonaqueous-Liquid-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.52 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (VNC(IGC,1,NQY) - VDNC(IGC,1,NQY)*GCPP(1,IGC))
     &              *AFY(NQY)
!
!---            Nonaqueous-Liquid-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.53 ) THEN
                  IGC = ISFGC(NS)
                  SFX = VDNC(IGC,1,NQY)*GCPP(1,IGC)*AFY(NQY)
!
!---            Total-Advective-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.54 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (VGC(IGC,1,NQY) + VNC(IGC,1,NQY))*AFY(NQY)
!
!---            Liquid-CO2 CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.55 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFY(NQY)*VNA(1,NQY)
                  ELSE
                    IGC = 1
                    SFX = AFY(NQY)*VNC(IGC,1,NQY)
                  ENDIF
!
!---            Liquid-CO2 CH4 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.56 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFY(NQY)*VNO(1,NQY)
                  ELSE
                    IGC = 2
                    SFX = AFY(NQY)*VNC(IGC,1,NQY)
                  ENDIF
!
!---            Liquid-CO2 H2O Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.57 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFY(NQY)*VNW(1,NQY)
                  ELSE
                    IGC = 3
                    SFX = AFY(NQY)*VNC(IGC,1,NQY)
                  ENDIF
!
!---            Total N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.58 ) THEN
                  SFX = AFY(NQY)*(VLN(1,NQY)+VGN(1,NQY)+VNN(1,NQY))
!
!---            Aqueous N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.59 ) THEN
                  SFX = AFY(NQY)*VLN(1,NQY)
!
!---            Gas N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.60 ) THEN
                  SFX = AFY(NQY)*VGN(1,NQY)
!
!---            Nonaqueous Liquid N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.61 ) THEN
                  SFX = AFY(NQY)*VNN(1,NQY)
                ELSEIF( ISFT(NS).GT.100 .AND. 
     &            ISFT(NS).LE.(100+NSOLU) ) THEN
                  NSL = ISFT(NS)-100
                  SFX = VC(NQY,NSL)*AFY(NQY)
!
!---            Gas Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.62 ) THEN
                  IGC = ISFGC(NS)
                  SFX = VGC(IGC,1,NQY)*AFY(NQY)
!
!---            Aqueous Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.63 ) THEN
                  SFX = VLA(1,NQY)*AFY(NQY)
!
!---            Nonaqueous-Liquid Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.64 ) THEN
                  IGC = ISFGC(NS)
                  SFX = VNC(IGC,1,NQY)*AFY(NQY)
!
!---            Total Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.65 ) THEN
                  IGC = ISFGC(NS)
                  IF( IGC.EQ.1 ) THEN
                    SFX = (VLA(1,NQY) + VGC(IGC,1,NQY) + VNC(IGC,1,NQY))
     &                *AFY(NQY)
                  ELSE
                    SFY = (VGC(IGC,1,NQY) + VNC(IGC,1,NQY))*AFY(NQY)
                  ENDIF

                ELSEIF( ISFT(NS).GT.(100+NSOLU) .AND. 
     &            ISFT(NS).LE.(100+NSOLU+NEQC+NEQK) ) THEN
                  NSL = ISFT(NS)-100
                  SFX = VC(NQY,NSL)*AFY(NQY)*1.D-3






                ENDIF
                RSFDX = REAL(ISFDX)
                SF(1,NS) = SF(1,NS)+SFX*SIGN(1.D+0,RSFDX)
 5400         CONTINUE
 5500       CONTINUE
 5600     CONTINUE
!
!---  Top surface  ---
!
        ELSEIF( ISFDX.EQ.3 ) THEN
          DO 6600 I = ISFCX(1),ISFCX(2)
            DO 6500 J = ISFCX(3),ISFCX(4)
              DO 6400 K = ISFCX(5),ISFCX(6)
                N = ND(I,J,K)
                NQZ = NSZ(N)+IJFLD
                IF( INBS(6,N).NE.0 ) NQZ = INBS(6,N)
                IF( ISFT(NS).EQ.1 ) THEN
                  SFX = WQ(1,NQZ)*AFZ(NQZ)
                ELSEIF( ISFT(NS).EQ.2 ) THEN
                  SFX = WL(1,NQZ)*AFZ(NQZ)
                ELSEIF( ISFT(NS).EQ.3 ) THEN
                  SFX = WG(1,NQZ)*AFZ(NQZ)
                ELSEIF( ISFT(NS).EQ.4 ) THEN
                  SFX = WN(1,NQZ)*AFZ(NQZ)
                ELSEIF( ISFT(NS).EQ.5 ) THEN
                  IF( WL(1,NQZ).LT.ZERO ) THEN
                    IF( K.LT.KFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHOLX = RHOL(2,N+IJFLD)
                    ELSE
                      DO 6050 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHOLX = RHOLB(2,NB)
                          GOTO 6052
                        ENDIF
 6050                 CONTINUE
 6052                 CONTINUE
                    ENDIF
                  ELSE
                    RHOLX = RHOL(2,N)
                  ENDIF
                  SFX = WL(1,NQZ)*AFZ(NQZ)*RHOLX
                ELSEIF( ISFT(NS).EQ.6 ) THEN
                  IF( WG(1,NQZ).LT.ZERO ) THEN
                    IF( K.LT.KFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHOGX = RHOG(2,N+IJFLD)
                    ELSE
                      DO 6060 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHOGX = RHOGB(2,NB)
                          GOTO 6062
                        ENDIF
 6060                 CONTINUE
 6062                 CONTINUE
                    ENDIF
                  ELSE
                    RHOGX = RHOG(2,N)
                  ENDIF
                  SFX = WG(1,NQZ)*AFZ(NQZ)*RHOGX
                ELSEIF( ISFT(NS).EQ.-6 ) THEN
                  IF( WG(1,NQZ).LT.ZERO ) THEN
                    IF( K.LT.KFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHOGX = RHOG(2,N+IJFLD)
                      XGWX = XGW(2,N+IJFLD)
                    ELSE
                      DO 6064 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHOGX = RHOGB(2,NB)
                          XGWX = XGWB(2,NB)
                          GOTO 6066
                        ENDIF
 6064                 CONTINUE
 6066                 CONTINUE
                    ENDIF
                  ELSE
                    RHOGX = RHOG(2,N)
                    XGWX = XGW(2,N)
                  ENDIF
                  SFX = WG(1,NQZ)*AFZ(NQZ)*RHOGX*XGWX
     &              + WDGW(1,NQZ)*WTMW*AFZ(NQZ)
                ELSEIF( ISFT(NS).EQ.7 ) THEN
                  IF( WN(1,NQZ).LT.ZERO ) THEN
                    IF( K.LT.KFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHONX = RHON(2,N+IJFLD)
                    ELSE
                      DO 6070 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHONX = RHONB(2,NB)
                          GOTO 6072
                        ENDIF
 6070                 CONTINUE
 6072                 CONTINUE
                    ENDIF
                  ELSE
                    RHONX = RHON(2,N)
                  ENDIF
                  SFX = WN(1,NQZ)*AFZ(NQZ)*RHONX
                ELSEIF( ISFT(NS).EQ.8 ) THEN
                  SFX = WS(1,NQZ)*AFZ(NQZ)
                ELSEIF( ISFT(NS).EQ.9 ) THEN
                  SFX = WLO(1,NQZ)*AFZ(NQZ)
                ELSEIF( ISFT(NS).EQ.10 ) THEN
                  IF( WL(1,NQZ).LT.ZERO ) THEN
                    IF( K.LT.KFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      RHOLX = RHOL(2,N+IJFLD)*XLW(2,N+IJFLD)
                    ELSE
                      DO 6100 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          RHOLX = RHOLB(2,NB)*XLWB(2,NB)
                          GOTO 6102
                        ENDIF
 6100                 CONTINUE
 6102                 CONTINUE
                    ENDIF
                  ELSE
                    RHOLX = RHOL(2,N)*XLW(2,N)
                  ENDIF
                  SFX = WL(1,NQZ)*AFZ(NQZ)*RHOLX +
     &              (WDGW(1,NQZ)-WDLA(1,NQZ)-WDLO(1,NQZ))*WTMW*AFZ(NQZ)
!
!---            Gas-CH4 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.11 ) THEN
                  SFX = AFZ(NQZ)*WGO(1,NQZ)
!
!---            Aqueous-CH4 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.12 ) THEN
                  SFX = AFZ(NQZ)*WLO(1,NQZ)
!
!---            Total-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.13 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFZ(NQZ)*(WGO(1,NQZ)+WLO(1,NQZ)+WNO(1,NQZ))
                  ELSE
                    SFX = AFZ(NQZ)*(WGO(1,NQZ)+WLO(1,NQZ))
                  ENDIF
                ELSEIF( ISFT(NS).EQ.20 ) THEN
                  IF( WG(1,NQZ).LT.ZERO ) THEN
                    IF( K.LT.KFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+IJFLD
                      VAR = (HGW(2,NB)*XGW(2,NB) + HGO(2,NB)*XGO(2,NB)
     &                  + HGA(2,NB)*XGA(2,NB))*RHOG(2,NB)
                    ELSE
                      DO 6200 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = (HGWB(2,NB)*XGWB(2,NB)+HGOB(2,NB)*XGOB(2,NB)
     &                  + HGAB(2,NB)*XGAB(2,NB))*RHOGB(2,NB)
                          GOTO 6201
                        ENDIF
 6200                 CONTINUE
 6201                CONTINUE
                    ENDIF
                  ELSE
                    VAR = (HGW(2,N)*XGW(2,N) + HGO(2,N)*XGO(2,N)
     &                + HGA(2,N)*XGA(2,N))*RHOG(2,N)
                  ENDIF
                  SFX = VAR*WG(1,NQZ)*AFZ(NQZ)
                ELSEIF( ISFT(NS).EQ.21 ) THEN
                  IF( WG(1,NQZ).LT.ZERO ) THEN
                    IF( K.LT.KFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+IJFLD
                      VAR = XGW(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 6210 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGWB(2,NB)*RHOGB(2,NB)
                          GOTO 6211
                        ENDIF
 6210                 CONTINUE
 6211                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGW(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*WG(1,NQZ)*AFZ(NQZ)
                ELSEIF( ISFT(NS).EQ.22 ) THEN
                  IF( WG(1,NQZ).LT.ZERO ) THEN
                    IF( K.LT.KFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+IJFLD
                      VAR = XGA(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 6220 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGAB(2,NB)*RHOGB(2,NB)
                          GOTO 6221
                        ENDIF
 6220                 CONTINUE
 6221                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGA(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*WG(1,NQZ)*AFZ(NQZ)
                ELSEIF( ISFT(NS).EQ.25 ) THEN
                  IF( WDGW(1,NQZ).LT.ZERO ) THEN
                    VARA = HGA(2,N)*WTMA
                    IF( K.LT.KFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+IJFLD
                      VARW = HGW(2,NB)*WTMW
                    ELSE
                      DO 6230 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          VARW = HGWB(2,NB)*WTMW
                          GOTO 6231
                        ENDIF
 6230                 CONTINUE
 6231                CONTINUE
                    ENDIF
                  ELSE
                    VARW = HGW(2,N)*WTMW
                    IF( K.LT.KFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+IJFLD
                      VARA = HGA(2,NB)*WTMA
                    ELSE
                      DO 6232 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          VARA = HGAB(2,NB)*WTMA
                          GOTO 6233
                        ENDIF
 6232                 CONTINUE
 6233                CONTINUE
                    ENDIF
                  ENDIF
                  SFX = WDGW(1,NQZ)*(VARW-VARA)*AFZ(NQZ)
                ELSEIF( ISFT(NS).EQ.26 ) THEN
                  SFX = WTMW*WDGW(1,NQZ)*AFZ(NQZ)
                ELSEIF( ISFT(NS).EQ.27 ) THEN
                  SFX = - WTMA*WDGW(1,NQZ)*AFZ(NQZ)
!
!---            Gas-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.28 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFZ(NQZ)*WGA(1,NQZ)
                  ELSE
                    IF( K.LT.KFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+IJFLD
                      FGAB = XGA(2,NB)*RHOG(2,NB)
                      DZB = DZGF(NB)
                    ELSE
                      DO 6280 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          FGAB = XGAB(2,NB)*RHOGB(2,NB)
                          DZB = DZGF(N)
                          GOTO 6281
                        ENDIF
 6280                 CONTINUE
 6281                 CONTINUE
                    ENDIF
                    FGAP = XGA(2,N)*RHOG(2,N)
                    INDX = 3
                    FGA = DIFMN( FGAP,FGAB,DZGF(N),DZB,WG(1,NQZ),INDX )
                    SFX = AFZ(NQZ)*(WG(1,NQZ)*FGA -
     &                WTMA*WDGW(1,NQZ))
                  ENDIF
!
!---            Aqueous-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.29 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFZ(NQZ)*WLA(1,NQZ)
                  ELSE
                    IF( K.LT.KFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+IJFLD
                      FLAB = XLA(2,NB)*RHOL(2,NB)
                      DZB = DZGF(NB)
                    ELSE
                      DO 6290 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          FLAB = XLAB(2,NB)*RHOLB(2,NB)
                          DZB = DZGF(N)
                          GOTO 6291
                        ENDIF
 6290                 CONTINUE
 6291                 CONTINUE
                    ENDIF
                    FLAP = XLA(2,N)*RHOL(2,N)
                    INDX = 2
                    FLA = DIFMN( FLAP,FLAB,DZGF(N),DZB,WL(1,NQZ),INDX )
                    SFX = AFZ(NQZ)*(WL(1,NQZ)*FLA +
     &                WTMA*WDLA(1,NQZ))
                  ENDIF
!
!---            Total-CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.30 ) THEN
                  IF( IOM.EQ.36 .OR. IOM.EQ.37 ) THEN
                    SFX = AFZ(NQZ)*(WGA(1,NQZ)+WLA(1,NQZ))
                  ELSEIF( IOM.EQ.38 ) THEN
                    SFX = AFZ(NQZ)*(WGA(1,NQZ)+WLA(1,NQZ)+WNC(1,1,NQZ))
                  ELSEIF( IOM.EQ.39 ) THEN
                    SFX = AFZ(NQZ)*(WGA(1,NQZ)+WLA(1,NQZ)+WNA(1,NQZ))
                  ELSE
                    IF( K.LT.KFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+IJFLD
                      FGAB = XGA(2,NB)*RHOG(2,NB)
                      FLAB = XLA(2,NB)*RHOL(2,NB)
                      DZB = DZGF(NB)
                    ELSE
                      DO 6300 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                          FGAB = XGAB(2,NB)*RHOGB(2,NB)
                          FLAB = XLAB(2,NB)*RHOLB(2,NB)
                          DZB = DZGF(N)
                          GOTO 6301
                        ENDIF
 6300                 CONTINUE
 6301                CONTINUE
                    ENDIF
                    FGAP = XGA(2,N)*RHOG(2,N)
                    FLAP = XLA(2,N)*RHOL(2,N)
                    INDX = 3
                    FGA = DIFMN( FGAP,FGAB,DZGF(N),DZB,WG(1,NQZ),INDX )
                    INDX = 2
                    FLA = DIFMN( FLAP,FLAB,DZGF(N),DZB,WL(1,NQZ),INDX )
                    SFX = AFZ(NQZ)*(WL(1,NQZ)*FLA + WG(1,NQZ)*FGA 
     &                - WTMA*WDGW(1,NQZ) + WTMA*WDLA(1,NQZ))
                  ENDIF
!
!---            Gas-Advective Oil-Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.31 ) THEN
                  IF( WG(1,NQZ).LT.ZERO ) THEN
                    IF( K.LT.KFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+IJFLD
                      VAR = XGO(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 6310 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGOB(2,NB)*RHOGB(2,NB)
                          GOTO 6311
                        ENDIF
 6310                 CONTINUE
 6311                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGO(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*WG(1,NQZ)*AFZ(NQZ)
!
!---            Gas-Diffusive Oil-Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.32 ) THEN
                  SFX = WTMO*WDGO(1,NQZ)*AFZ(NQZ)
!
!---            Gas-Total Oil-Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.33 ) THEN
                  IF( WG(1,NQZ).LT.ZERO ) THEN
                    IF( K.LT.KFLD ) THEN
                      IFLAG = .TRUE.
                      IF( IXP(N+IJFLD).EQ.0 ) IFLAG = .FALSE.
                    ELSE
                      IFLAG = .FALSE.
                    ENDIF
                    IF( IFLAG ) THEN
                      NB = N+IJFLD
                      VAR = XGO(2,NB)*RHOG(2,NB)
                    ELSE
                      DO 6330 NB = 1,NBC
                        IF( IBCN(NB).EQ.N ) THEN
                      VAR = XGOB(2,NB)*RHOGB(2,NB)
                          GOTO 6331
                        ENDIF
 6330                 CONTINUE
 6331                CONTINUE
                    ENDIF
                  ELSE
                    VAR = XGO(2,N)*RHOG(2,N)
                  ENDIF
                  SFX = VAR*WG(1,NQZ)*AFZ(NQZ)
     &              + WTMO*WDGO(1,NQZ)*AFZ(NQZ)
!
!---            Actual Evaporation Flux  ---
!
                ELSEIF( ISFT(NS).EQ.34 ) THEN
                  M = 2
                  SFX = RHON(M,N)
!
!---            Potential Evaporation Flux  ---
!
                ELSEIF( ISFT(NS).EQ.35 ) THEN
                  M = 3
                  SFX = RHON(M,N)
!
!---            Actual Transpiration Flux  ---
!
                ELSEIF( ISFT(NS).EQ.36 ) THEN
                  M = 4
                  SFX = RHON(M,N)
!
!---            Potential Transpiration Flux  ---
!
                ELSEIF( ISFT(NS).EQ.37 ) THEN
                  M = 5
                  SFX = RHON(M,N)
!
!---            Net Total Radiation Flux  ---
!
                ELSEIF( ISFT(NS).EQ.38 ) THEN
                  M = 5
                  SFX = SN(M,N)*AFZ(NQZ)
!
!---            Net Short-Wave Radiation Flux  ---
!
                ELSEIF( ISFT(NS).EQ.39 ) THEN
                  M = 4
                  SFX = SN(M,N)*AFZ(NQZ)
!
!---            Net Long-Wave Radiation Flux  ---
!
                ELSEIF( ISFT(NS).EQ.40 ) THEN
                  M = 3
                  SFX = SN(M,N)*AFZ(NQZ)
!
!---            Surface Water-Mass Balance  ---
!
                ELSEIF( ISFT(NS).EQ.41 ) THEN
                  M = 1
                  SFX = RHON(M,N)
!
!---            Rain-Water Runoff  ---
!
                ELSEIF( ISFT(NS).EQ.42 ) THEN
                  M = 1
                  SFX = XGO(M,N)
!
!---            Aqueous-Water Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.43 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFZ(NQZ)*WLW(1,NQZ)
                  ENDIF
!
!---            Gas-Water Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.44 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.39 ) THEN
                    SFX = AFZ(NQZ)*WGW(1,NQZ)
                  ENDIF
!
!---            Total-Water Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.45 ) THEN
                  IF( IOM.GE.36 .AND. IOM.LE.38 ) THEN
                    SFX = AFZ(NQZ)*(WLW(1,NQZ)+WGW(1,NQZ))
                  ELSEIF( IOM.EQ.39 ) THEN
                    SFX = AFZ(NQZ)*(WLW(1,NQZ)+WGW(1,NQZ)+WNW(1,NQZ))
                  ENDIF
!
!---            Aqueous-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.46 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (WLC(IGC,1,NQZ) - WDLC(IGC,1,NQZ)*GCPP(1,IGC))
     &              *AFZ(NQZ)
!
!---            Aqueous-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.47 ) THEN
                  IGC = ISFGC(NS)
                  SFX = WDLC(IGC,1,NQZ)*GCPP(1,IGC)*AFZ(NQZ)
!
!---            Gas-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.48 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (WGC(IGC,1,NQZ) - WDGC(IGC,1,NQZ)*GCPP(1,IGC))
     &              *AFZ(NQZ)
!
!---            Gas-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.49 ) THEN
                  IGC = ISFGC(NS)
                  SFX = WDGC(IGC,1,NQZ)*GCPP(1,IGC)*AFZ(NQZ)
!
!---            Total-Advective-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.50 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (WGC(IGC,1,NQZ) + WLC(IGC,1,NQZ)
     &               + WNC(IGC,1,NQZ))*AFZ(NQZ)
!
!---            Total-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.51 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (WDGC(IGC,1,NQZ) 
     &               + WDLC(IGC,1,NQZ) + WDNC(IGC,1,NQZ))
     &              *GCPP(1,IGC)*AFZ(NQZ)
!
!---            Nonaqueous-Liquid-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.52 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (WNC(IGC,1,NQZ) - WDNC(IGC,1,NQZ)*GCPP(1,IGC))
     &              *AFZ(NQZ)
!
!---            Nonaqueous-Liquid-Advective Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.53 ) THEN
                  IGC = ISFGC(NS)
                  SFX = WDNC(IGC,1,NQZ)*GCPP(1,IGC)*AFZ(NQZ)
!
!---            Total-Advective-Diffusive Gas-Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.54 ) THEN
                  IGC = ISFGC(NS)
                  SFX = (WGC(IGC,1,NQZ) + WNC(IGC,1,NQZ))*AFZ(NQZ)
!
!---            Liquid-CO2 CO2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.55 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFZ(NQZ)*WNA(1,NQZ)
                  ELSE
                    IGC = 1
                    SFX = AFZ(NQZ)*WNC(IGC,1,NQZ)
                  ENDIF
!
!---            Liquid-CO2 CH4 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.56 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFZ(NQZ)*WNO(1,NQZ)
                  ELSE
                    IGC = 2
                    SFX = AFZ(NQZ)*WNC(IGC,1,NQZ)
                  ENDIF
!
!---            Liquid-CO2 H2O Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.57 ) THEN
                  IF( IOM.EQ.39 ) THEN
                    SFX = AFZ(NQZ)*WNW(1,NQZ)
                  ELSE
                    IGC = 3
                    SFX = AFZ(NQZ)*WNC(IGC,1,NQZ)
                  ENDIF
!
!---            Total N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.58 ) THEN
                  SFX = AFZ(NQZ)*(WLN(1,NQZ)+WGN(1,NQZ)+WNN(1,NQZ))
!
!---            Aqueous N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.59 ) THEN
                  SFX = AFZ(NQZ)*WLN(1,NQZ)
!
!---            Gas N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.60 ) THEN
                  SFX = AFZ(NQZ)*WGN(1,NQZ)
!
!---            Nonaqueous Liquid N2 Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.61 ) THEN
                  SFX = AFZ(NQZ)*WNN(1,NQZ)
!
!---            Gas Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.62 ) THEN
                  IGC = ISFGC(NS)
                  SFX = WGC(IGC,1,NQZ)*AFZ(NQZ)
!
!---            Aqueous Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.63 ) THEN
                  SFX = WLA(1,NQZ)*AFZ(NQZ)
!
!---            Nonaqueous-Liquid Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.64 ) THEN
                  IGC = ISFGC(NS)
                  SFX = WNC(IGC,1,NQZ)*AFZ(NQZ)
!
!---            Total Component Mass Flux  ---
!
                ELSEIF( ISFT(NS).EQ.65 ) THEN
                  IGC = ISFGC(NS)
                  IF( IGC.EQ.1 ) THEN
                    SFX = (WLA(1,NQZ) + WGC(IGC,1,NQZ) + WNC(IGC,1,NQZ))
     &                *AFZ(NQZ)
                  ELSE
                    SFX = (WGC(IGC,1,NQZ) + WNC(IGC,1,NQZ))*AFZ(NQZ)
                  ENDIF
                ELSEIF( ISFT(NS).GT.100 .AND. 
     &            ISFT(NS).LE.(100+NSOLU) ) THEN
                  NSL = ISFT(NS)-100
                  SFX = WC(NQZ,NSL)*AFZ(NQZ)

                ELSEIF( ISFT(NS).GT.(100+NSOLU) .AND. 
     &            ISFT(NS).LE.(100+NSOLU+NEQC+NEQK) ) THEN
                  NSL = ISFT(NS)-100
                  SFX = WC(NQZ,NSL)*AFZ(NQZ)*1.D-3






                ENDIF
                RSFDX = REAL(ISFDX)
                SF(1,NS) = SF(1,NS)+SFX*SIGN(1.D+0,RSFDX)
 6400         CONTINUE
 6500       CONTINUE
 6600     CONTINUE
        ENDIF
 6900 CONTINUE
      SF(2,NS) = SF(2,NS) + SF(1,NS)*DT
 7000 CONTINUE
      VAR = TM
      IF( UNTM.NE.'null' ) THEN
        INDX = 1
        IUNS = 1
        CALL RDUNIT(UNTM,VAR,INDX)
      ENDIF
      IF( ABS(VAR).LT.1.D-99 ) VAR = 0.D+0
!
!---    Skip the default surface file if its unused  ---
!
      NSTART = 1
      IF( ISFGP(1).EQ.0 ) NSTART = 2
!
!---    Loop over surface-flux files, writing surface-flux
!       time  ---
!
      DO 7500 NSG = NSTART,NSFGP
        WRITE(ISF(NSG),9005) VAR,' '
 7500 CONTINUE
      DO 8000 NS = 1,NSF
        VAR = SF(1,NS)
        IF( UNSF(1,NS).NE.'null' ) THEN
          INDX = 1
          IF( ISFT(NS).EQ.1 ) THEN
            IUNM = 2
            IUNKG = 1
            IUNS = -3
          ELSEIF( ISFT(NS).EQ.2 ) THEN
            IUNM = 3
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.3 ) THEN
            IUNM = 3
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.4 ) THEN
            IUNM = 3
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.5 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.6 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.-6 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.7 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.8 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.9 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.11 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.12 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.13 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.20 ) THEN
            IUNM = 2
            IUNKG = 1
            IUNS = -3
          ELSEIF( ISFT(NS).EQ.21 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.22 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.25 ) THEN
            IUNM = 2
            IUNKG = 1
            IUNS = -3
          ELSEIF( ISFT(NS).EQ.26 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.27 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.28 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.29 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.30 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.31 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.32 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.33 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.34 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.35 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.36 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.37 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.38 ) THEN
            IUNM = 2
            IUNKG = 1
            IUNS = -3
          ELSEIF( ISFT(NS).EQ.39 ) THEN
            IUNM = 2
            IUNKG = 1
            IUNS = -3
          ELSEIF( ISFT(NS).EQ.40 ) THEN
            IUNM = 2
            IUNKG = 1
            IUNS = -3
          ELSEIF( ISFT(NS).EQ.41 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.42 ) THEN
            IUNM = 3
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.43 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.44 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.45 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.46 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.47 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.48 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.49 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.50 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.51 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.52 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.53 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.54 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.55 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.56 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.57 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.58 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.59 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.60 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.61 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.62 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.63 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.64 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).EQ.65 ) THEN
            IUNKG = 1
            IUNS = -1
          ELSEIF( ISFT(NS).GT.100 .AND. ISFT(NS).LE.(100+NSOLU) ) THEN
            IUNS = -1

          ELSEIF( ISFT(NS).GT.(100+NSOLU) .AND. 
     &      ISFT(NS).LE.(100+NSOLU+NEQC+NEQK) ) THEN
            IUNMOL = 1
            IUNS = -1





          ENDIF
          CALL RDUNIT(UNSF(1,NS),VAR,INDX)
        ENDIF
        IF( ABS(VAR).LT.1.D-99 ) VAR = 0.D+0
!
!---    Skip the default surface file if its unused  ---
!
        NSTART = 1
        IF( ISFGP(1).EQ.0 ) NSTART = 2
!
!---    Loop over surface-flux files, writing surface-flux
!       rates  ---
!
        DO 7800 NSG = NSTART,NSFGP
!
!---      Surface not associated with surface file,
!         skip output  ---
!
          IF( NSG.EQ.ISFF(NS) ) WRITE(ISF(NSG),9005) VAR,' '
 7800   CONTINUE
        VAR = SF(2,NS)
        IF( UNSF(2,NS).NE.'null' ) THEN
          INDX = 1
          IF( ISFT(NS).EQ.1 ) THEN
            IUNM = 2
            IUNKG = 1
            IUNS = -2
          ELSEIF( ISFT(NS).EQ.2 ) THEN
            IUNM = 3
          ELSEIF( ISFT(NS).EQ.3 ) THEN
            IUNM = 3
          ELSEIF( ISFT(NS).EQ.4 ) THEN
            IUNM = 3
          ELSEIF( ISFT(NS).EQ.5 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.6 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.-6 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.7 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.8 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.9 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.11 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.12 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.13 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.20 ) THEN
            IUNM = 2
            IUNKG = 1
            IUNS = -2
          ELSEIF( ISFT(NS).EQ.21 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.22 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.25 ) THEN
            IUNM = 2
            IUNKG = 1
            IUNS = -2
          ELSEIF( ISFT(NS).EQ.26 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.27 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.28 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.29 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.30 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.31 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.32 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.33 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.34 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.35 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.36 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.37 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.38 ) THEN
            IUNM = 2
            IUNKG = 1
            IUNS = -2
          ELSEIF( ISFT(NS).EQ.39 ) THEN
            IUNM = 2
            IUNKG = 1
            IUNS = -2
          ELSEIF( ISFT(NS).EQ.40 ) THEN
            IUNM = 2
            IUNKG = 1
            IUNS = -2
          ELSEIF( ISFT(NS).EQ.41 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.42 ) THEN
            IUNM = 3
          ELSEIF( ISFT(NS).EQ.43 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.44 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.45 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.46 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.47 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.48 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.49 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.50 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.51 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.52 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.53 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.54 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.55 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.56 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.57 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.58 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.59 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.60 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.61 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.62 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.63 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.64 ) THEN
            IUNKG = 1
          ELSEIF( ISFT(NS).EQ.65 ) THEN
            IUNKG = 1

!
!---      Conservation component species and kinetic component
!         species molar flux  ---
!
          ELSEIF( ISFT(NS).GT.(100+NSOLU) .AND. 
     &      ISFT(NS).LE.(100+NSOLU+NEQC+NEQK) ) THEN
            IUNMOL = 1

          ENDIF
          CALL RDUNIT(UNSF(2,NS),VAR,INDX)
        ENDIF
        IF( ABS(VAR).LT.1.D-99 ) VAR = 0.D+0
!
!---    Skip the default surface file if its unused  ---
!
        NSTART = 1
        IF( ISFGP(1).EQ.0 ) NSTART = 2
!
!---    Loop over surface-flux files, writing surface-flux
!       integrals  ---
!
        DO 7900 NSG = NSTART,NSFGP
!
!---      Surface not associated with surface file,
!         skip output  ---
!
          IF( ISFF(NS).EQ.NSG ) THEN
            NCX(ISFF(NS)) = NCX(ISFF(NS)) + 1
            IF( NS.EQ.NSF .OR. NCX(ISFF(NS)).EQ.ISFGP(NSG) ) THEN
              WRITE(ISF(NSG),9015) VAR
            ELSE
              WRITE(ISF(NSG),9005) VAR,' '
            ENDIF
          ENDIF
 7900   CONTINUE
 8000 CONTINUE
      IHSF = IHSF + 1
!
!---  Format Statements  ---
!
 9001 FORMAT(A,$)
 9002 FORMAT(2X,A,A,I3,A,$)
 9003 FORMAT(2X,A,A,A,$)
 9005 FORMAT(1PE13.6,A,$)
 9012 FORMAT(2X,A,A,I3,A)
 9013 FORMAT(2X,A,A,A)
 9015 FORMAT(1PE13.6)
      ISUB_LOG = ISUB_LOG-1
!
!---  End of SFIN group
!
      RETURN
      END

