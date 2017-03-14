!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDHYDR
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
!     Read input file for rock/soil hydraulic information.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, December 1992.
!     Last Modified by MD White, PNNL, 5 October 2001.
!     $Id: rdhydr.F 1080 2017-03-14 16:22:02Z d3c002 $
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
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,UNTS
      CHARACTER*512 CHDUM
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 PERMRFX      
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDHYDR'
      IF( INDEX(SVN_ID(145)(1:1),'$').EQ.0 ) SVN_ID(145) =
     & '$Id: rdhydr.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Write card information to ouput file  ---
!
      CARD = 'Rock/Soil Hydraulic Properties Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Loop over the rock/soil hydraulic information lines  ---
!
      N = 0
      IJK = 0
      ISGRP = 0
   10 CONTINUE
        IF( N.GE.NROCK .OR. IJK.GT.0 ) GOTO 500
        ISTART = 1
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        VARB = 'Rock/Soil Name'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---  IJK, KIJ, or JKI indexing  ---
!
        IF( INDEX(ADUM(1:),'indexing').NE.0 ) THEN
          IF( INDEX(ROCK(1)(1:),'indexing').EQ.0 ) THEN
            INDX = 4
            CHMSG = 'Indexing Option Not Declared ' // 
     &        'in Rock/Soil Zonation Card'
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
!---  Search known rock types for a matching type ---
!
        DO 100 M = 1,NROCK
           IF( ADUM.EQ.ROCK(M) ) THEN
              IROCK = M
              ISGRP = 0
              GOTO 200
           ENDIF
  100   CONTINUE
!
!---  Search known scaling groups for a matching type ---
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
!---  Loop over rock/soils within scaling group  ---
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
!---    Dual porosity index  ---
!
        IDPX = 0
        IF( IJK.GT.0 ) THEN
          IDPX = IDP(1)
        ELSE
          IDPX = IDP(IROCK)
        ENDIF
!
!---    Read intrinsic permeabilities: three coordinate directions ---
!
        IF( ABS(IDPX).GT.0 ) THEN
          VARB = 'X-Direction Intrinsic Permeability'
        ELSE
          VARB = 'X-Direction Matrix Intrinsic Permeability'
        ENDIF
        IF( IJK.GT.0 ) THEN
          ISTX = ISTART
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IHC = INDEX( UNTS(1:),'hc' )
          IF( IHC.EQ.0 ) THEN
            UNTS = 'm^2'
            IUNM = 2
          ELSE
            UNTS = 'm/s'
            IUNM = 1
            IUNS = -1
          ENDIF
          ISTART = ISTX
          INDX = 1
          LNDX = 9
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(1,IROCK))
          IF( ICOMMA.EQ.ISTART .AND. IFLD.GT.1 ) THEN
            INDX = 4
            IF( ABS(IDPX).GT.0 ) THEN
              CHMSG = 'Zero X-Dir. Intrinsic Permeability'
            ELSE
              CHMSG = 'Zero X-Dir. Matrix Intrinsic Permeability'
            ENDIF
            CALL WRMSGS( INDX )
          ENDIF
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IHC = INDEX( UNTS(1:),'hc' )
          IF( IHC.EQ.0 ) THEN
            WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',PERM(1,IROCK)
            IUNM = 2
          ELSE
            WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &   UNTS(1:NCH),': ',PERM(1,IROCK)
            IUNM = 1
            IUNS = -1
          ENDIF
          INDX = 0
          CALL RDUNIT(UNTS,PERM(1,IROCK),INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',PERM(1,IROCK),', m^2)'
        ENDIF
        IF( ABS(IDPX).GT.0 ) THEN
          VARB = 'Y-Direction Intrinsic Permeability'
        ELSE
          VARB = 'Y-Direction Matrix Intrinsic Permeability'
        ENDIF
        IF( IJK.GT.0 ) THEN
          ISTX = ISTART
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IHC = INDEX( UNTS(1:),'hc' )
          IF( IHC.EQ.0 ) THEN
            UNTS = 'm^2'
            IUNM = 2
          ELSE
            UNTS = 'm/s'
            IUNM = 1
            IUNS = -1
          ENDIF
          ISTART = ISTX
          INDX = 2
          LNDX = 9
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(2,IROCK))
          IF( ICOMMA.EQ.ISTART .AND. JFLD.GT.1 ) THEN
            INDX = 4
            IF( ABS(IDPX).GT.0 ) THEN
              CHMSG = 'Zero Y-Dir. Intrinsic Permeability'
            ELSE
              CHMSG = 'Zero Y-Dir. Matrix Intrinsic Permeability'
            ENDIF
            CALL WRMSGS( INDX )
          ENDIF
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IHC = INDEX( UNTS(1:),'hc' )
          IF( IHC.EQ.0 ) THEN
            WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',PERM(2,IROCK)
            IUNM = 2
          ELSE
            WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',PERM(2,IROCK)
            IUNM = 1
            IUNS = -1
          ENDIF
          INDX = 0
          CALL RDUNIT(UNTS,PERM(2,IROCK),INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',PERM(2,IROCK),', m^2)'
        ENDIF
        IF( ABS(IDPX).GT.0 ) THEN
          VARB = 'Z-Direction Intrinsic Permeability'
        ELSE
          VARB = 'Z-Direction Matrix Intrinsic Permeability'
        ENDIF
        IF( IJK.GT.0 ) THEN
          ISTX = ISTART
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IHC = INDEX( UNTS(1:),'hc' )
          IF( IHC.EQ.0 ) THEN
            UNTS = 'm^2'
            IUNM = 2
          ELSE
            UNTS = 'm/s'
            IUNM = 1
            IUNS = -1
          ENDIF
          ISTART = ISTX
          INDX = 3
          LNDX = 9
          CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
        ELSE
          CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(3,IROCK))
          IF( ICOMMA.EQ.ISTART .AND. KFLD.GT.1 ) THEN
            INDX = 4
            IF( ABS(IDPX).GT.0 ) THEN
              CHMSG = 'Zero Z-Dir. Intrinsic Permeability'
            ELSE
              CHMSG = 'Zero Z-Dir. Matrix Intrinsic Permeability'
            ENDIF
            CALL WRMSGS( INDX )
          ENDIF
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IHC = INDEX( UNTS(1:),'hc' )
          IF( IHC.EQ.0 ) THEN
            WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',PERM(3,IROCK)
            IUNM = 2
          ELSE
            WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &        UNTS(1:NCH),': ',PERM(3,IROCK)
            IUNM = 1
            IUNS = -1
          ENDIF
          INDX = 0
          CALL RDUNIT(UNTS,PERM(3,IROCK),INDX)
          WRITE(IWR,'(A,1PE11.4,A)') ' (',PERM(3,IROCK),', m^2)'
        ENDIF
!
!---    Read fracture permeability for dual porosity rock/soil types,
!       fracture permeability is calculated from fracture properties
!       in the Oil Shale operational mode (STOMP-OS).
!
        IF( ABS(IDPX).EQ.1 ) THEN
          VARB = 'Fracture X-Direction Permeability'
          IF( IJK.GT.0 ) THEN
            ISTX = ISTART
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IHC = INDEX( UNTS(1:),'hc' )
            IF( IHC.EQ.0 ) THEN
              UNTS = 'm^2'
              IUNM = 2
            ELSE
              UNTS = 'm/s'
              IUNM = 1
              IUNS = -1
            ENDIF
            ISTART = ISTX
            INDX = 7
            LNDX = 9
            CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
            DO N = 1,NFLD
              PERM(4,IZ(N)) = PERM(1,IZ(N))
              PERM(1,IZ(N)) = PERM(4,IZ(N))*(1.D+0-POR(4,IZ(N))) +
     &          PERM(7,IZ(N))*POR(4,IZ(N))
            ENDDO
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(7,IROCK))
            IF( ICOMMA.EQ.ISTART .AND. IFLD.GT.1 ) THEN
              INDX = 4
              CHMSG = 'Zero X-Dir. Fracture Intrinsic Permeability'
              CALL WRMSGS( INDX )
            ENDIF
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IHC = INDEX( UNTS(1:),'hc' )
            IF( IHC.EQ.0 ) THEN
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',PERM(7,IROCK)
              IUNM = 2
            ELSE
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',PERM(7,IROCK)
              IUNM = 1
              IUNS = -1
            ENDIF
            INDX = 0
            CALL RDUNIT(UNTS,PERM(7,IROCK),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',PERM(7,IROCK),', m^2)'
            PERM(4,IROCK) = PERM(1,IROCK)
            PERM(1,IROCK) = PERM(4,IROCK)*(1.D+0-POR(4,IROCK)) +
     &        PERM(7,IROCK)*POR(4,IROCK)
            VARB = 'Effective X-Direction Permeability'
            IVR = INDEX( VARB,'  ')-1
            WRITE(IWR,'(2X,3A,1PE11.4)') VARB(1:IVR),', ',
     &        'm^2: ',PERM(1,IROCK)
!            IF( ABS(IDP(IROCK)).EQ.1 ) PERM(1,IROCK) = PERM(7,IROCK)
          ENDIF
          VARB = 'Fracture Y-Direction Permeability'
          IF( IJK.GT.0 ) THEN
            ISTX = ISTART
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IHC = INDEX( UNTS(1:),'hc' )
            IF( IHC.EQ.0 ) THEN
              UNTS = 'm^2'
              IUNM = 2
            ELSE
              UNTS = 'm/s'
              IUNM = 1
              IUNS = -1
            ENDIF
            ISTART = ISTX
            INDX = 8
            LNDX = 9
            CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
            DO N = 1,NFLD
              PERM(5,IZ(N)) = PERM(2,IZ(N))
              PERM(2,IZ(N)) = PERM(5,IZ(N))*(1.D+0-POR(4,IZ(N))) +
     &          PERM(8,IZ(N))*POR(4,IZ(N))
            ENDDO
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(8,IROCK))
            IF( ICOMMA.EQ.ISTART .AND. JFLD.GT.1 ) THEN
              INDX = 4
              CHMSG = 'Zero Y-Dir. Intrinsic Permeability'
              CALL WRMSGS( INDX )
            ENDIF
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IHC = INDEX( UNTS(1:),'hc' )
            IF( IHC.EQ.0 ) THEN
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',PERM(8,IROCK)
              IUNM = 2
            ELSE
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',PERM(8,IROCK)
              IUNM = 1
              IUNS = -1
            ENDIF
            INDX = 0
            CALL RDUNIT(UNTS,PERM(8,IROCK),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',PERM(8,IROCK),', m^2)'
            PERM(5,IROCK) = PERM(2,IROCK)
            PERM(2,IROCK) = PERM(5,IROCK)*(1.D+0-POR(4,IROCK)) +
     &        PERM(8,IROCK)*POR(4,IROCK)
            VARB = 'Effective Y-Direction Permeability'
            IVR = INDEX( VARB,'  ')-1
            WRITE(IWR,'(2X,3A,1PE11.4)') VARB(1:IVR),', ',
     &        'm^2: ',PERM(2,IROCK)
!            IF( ABS(IDP(IROCK)).EQ.1 ) PERM(2,IROCK) = PERM(8,IROCK)
          ENDIF
          VARB = 'Fracture Z-Direction Permeability'
          IF( IJK.GT.0 ) THEN
            ISTX = ISTART
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IHC = INDEX( UNTS(1:),'hc' )
            IF( IHC.EQ.0 ) THEN
              UNTS = 'm^2'
              IUNM = 2
            ELSE
              UNTS = 'm/s'
              IUNM = 1
              IUNS = -1
            ENDIF
            ISTART = ISTX
            INDX = 8
            LNDX = 9
            CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
            DO N = 1,NFLD
              PERM(6,IZ(N)) = PERM(3,IZ(N))
              PERM(3,IZ(N)) = PERM(6,IZ(N))*(1.D+0-POR(4,IZ(N))) +
     &          PERM(9,IZ(N))*POR(4,IZ(N))
            ENDDO
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(9,IROCK))
            IF( ICOMMA.EQ.ISTART .AND. KFLD.GT.1 ) THEN
              INDX = 4
              CHMSG = 'Zero Z-Dir. Intrinsic Permeability'
              CALL WRMSGS( INDX )
            ENDIF
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IHC = INDEX( UNTS(1:),'hc' )
            IF( IHC.EQ.0 ) THEN
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',PERM(9,IROCK)
              IUNM = 2
            ELSE
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',PERM(9,IROCK)
              IUNM = 1
              IUNS = -1
            ENDIF
            INDX = 0
            CALL RDUNIT(UNTS,PERM(9,IROCK),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',PERM(9,IROCK),', m^2)'
            PERM(6,IROCK) = PERM(3,IROCK)
            PERM(3,IROCK) = PERM(6,IROCK)*(1.D+0-POR(4,IROCK)) +
     &        PERM(9,IROCK)*POR(4,IROCK)
            VARB = 'Effective Z-Direction Permeability'
            IVR = INDEX( VARB,'  ')-1
            WRITE(IWR,'(2X,3A,1PE11.4)') VARB(1:IVR),', ',
     &        'm^2: ',PERM(3,IROCK)
          ENDIF
!
!---    Fracture permeability (EC) or dual porosity model  ---
!
        ELSEIF( IDPX.EQ.2 .OR. IDPX.EQ.3 ) THEN
!
!---      Read fracture intrinsic permeability ---
!
          VARB = 'Fracture Intrinsic Permeability'
          IF( IJK.GT.0 ) THEN
            ISTX = ISTART
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IHC = INDEX( UNTS(1:),'hc' )
            IF( IHC.EQ.0 ) THEN
              UNTS = 'm^2'
              IUNM = 2
            ELSE
              UNTS = 'm/s'
              IUNM = 1
              IUNS = -1
            ENDIF
            ISTART = ISTX
            INDX = 4
            LNDX = 9
            CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(4,IROCK))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IHC = INDEX( UNTS(1:),'hc' )
            IF( IHC.EQ.0 ) THEN
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',PERM(4,IROCK)
              IUNM = 2
            ELSE
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &     UNTS(1:NCH),': ',PERM(4,IROCK)
              IUNM = 1
              IUNS = -1
            ENDIF
            INDX = 0
            CALL RDUNIT(UNTS,PERM(4,IROCK),INDX)
            WRITE(IWR,'(A,1PE11.4,A)') ' (',PERM(4,IROCK),', m^2)'
          ENDIF
!
!---    Fracture permeability is calculated from fracture properties
!       in the Oil Shale operational mode (STOMP-OS)  ---
!
        ELSEIF( IDPX.EQ.50 ) THEN
          IF( IJK.GT.0 ) THEN
            DO N = 1,NFLD
              RRFX = MIN( POR(5,IZ(N))/(2.D+0*POR(4,IZ(N))),1.D+0 )
              PERM(4,IZ(N)) = POR(3,IZ(N))*(POR(4,IZ(N))**3)/
     &          (1.2D+1*(1.D+0 + 8.8D+0*(RRFX**1.5D+0)))
            ENDDO
          ELSE
            VARB = 'Reference Fracture Intrinsic Permeability'
            IVR = INDEX( VARB,'  ')-1
            RRFX = MIN( POR(5,IROCK)/(2.D+0*POR(4,IROCK)),1.D+0 )
            PERM(4,IROCK) = POR(3,IROCK)*(POR(4,IROCK)**3)/
     &        (1.2D+1*(1.D+0 + 8.8D+0*(RRFX**1.5D+0)))
            WRITE(IWR,'(2X,3A,1PE11.4)') VARB(1:IVR),', ',
     &        'm^2: ',PERM(4,IROCK)
          ENDIF
        ENDIF
!
!---    Civan model for permeability reduction factors, used
!       for STOMP-HYDT-KE  ---
!
        IF( IOM.EQ.39 ) THEN
          VARB = 'Civan Model Beta Parameter'
          UNTS = 'null'
          INDX = 6
          LNDX = 9
          IF( IJK.GT.0 ) THEN
            CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(6,IROCK))
            WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',PERM(6,IROCK)
          ENDIF
        ENDIF
!
!---    Check for permeability reduction factors  ---
!
        CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
        IF( INDX.EQ.1 ) THEN
          IF( IJK.GT.0 ) THEN
            DO 350 N = 1,NFLD
              IPRF(N) = 1
  350       CONTINUE
          ELSE
            IPRF(IROCK) = 1
          ENDIF
          VARB = 'Pore-Body Fractional Length'
          UNTS = 'null'
          INDX = 4
          LNDX = 9
          IF( IJK.GT.0 ) THEN
            CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
            DO 360 N = 1,NFLD
              IF( PERM(4,N).LE.0.D+0.OR.PERM(4,N).GE.1.D+0 ) THEN
                INDX = 9
                CHMSG = 'Out of Range Pore-Body Fractional Length: '
                RLMSG = PERM(4,N)
                CALL WRMSGS( INDX )
              ENDIF
  360       CONTINUE
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(4,IROCK))
            WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',PERM(4,IROCK)
            IF( PERM(4,IROCK).LE.0.D+0.OR.PERM(4,IROCK).GE.1.D+0 ) THEN
              INDX = 9
              CHMSG = 'Out of Range Pore-Body Fractional Length: '
              RLMSG = PERM(4,IROCK)
              CALL WRMSGS( INDX )
            ENDIF
          ENDIF
          VARB = 'Fractional Critical Porosity'
          UNTS = 'null'
          INDX = 5
          LNDX = 9
          IF( IJK.GT.0 ) THEN
            CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
            DO 380 N = 1,NFLD
              IF( PERM(5,N).LE.0.D+0.OR.PERM(5,N).GE.1.D+0 ) THEN
                INDX = 9
                CHMSG = 'Out of Range Fractional Critical Porosity: '
                RLMSG = PERM(5,N)
                CALL WRMSGS( INDX )
              ENDIF
  380       CONTINUE
          ELSE
            CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(5,IROCK))
            WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',PERM(5,IROCK)
            IF( PERM(5,IROCK).LE.0.D+0.OR.PERM(5,IROCK).GE.1.D+0 ) THEN
              INDX = 9
              CHMSG = 'Out of Range Fractional Critical Porosity: '
              RLMSG = PERM(5,IROCK)
              CALL WRMSGS( INDX )
            ENDIF
          ENDIF
        ENDIF
!
!---    Check for permeability models   ---
!
        CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
        IF( INDX.EQ.1 ) THEN
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---      Kozeny-Carmen intrinsic permeability model   ---
!
          IF( INDEX(ADUM(1:),'kozeny').NE.0 ) THEN
            WRITE(IWR,'(A)') 'Kozeny & Carmen Intrinsic Permeability'
            IF( IJK.GT.0 ) THEN
              DO 410 N = 1,NFLD
                IPRF(N) = 2
                CALL PERM_I( PERMRFX,POR(1,IZ(N)))
                PERM(1,IZ(N))=PERM(1,IZ(N))/PERMRFX
                PERM(2,IZ(N))=PERM(2,IZ(N))/PERMRFX
                PERM(3,IZ(N))=PERM(3,IZ(N))/PERMRFX
  410         CONTINUE
            ELSE
              IPRF(IROCK) = 2
              CALL PERM_I( PERMRFX,POR(1,IROCK))
              PERM(1,IROCK)=PERM(1,IROCK)/PERMRFX
              PERM(2,IROCK)=PERM(2,IROCK)/PERMRFX
              PERM(3,IROCK)=PERM(3,IROCK)/PERMRFX
            ENDIF
!
!---      Poroelastic permeability model   ---
!
          ELSEIF( INDEX(ADUM(1:),'poroelastic').NE.0 ) THEN
            WRITE(IWR,'(A)') 'Poroelastic Intrinsic Permeability'
            IF( IJK.GT.0 ) THEN
              DO 420 N = 1,NFLD
                IPRF(N) = IPRF(N) + 10
  420         CONTINUE
            ELSE
              IPRF(IROCK) = IPRF(IROCK) + 10
            ENDIF
!
!---        Poroelastic parameter   ---
!
            VARB = 'Poroelastic Parameter'
            IF( IJK.GT.0 ) THEN
              UNTS = 'null'
              INDX = 4
              LNDX = 9
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
            ELSE
              UNTS = 'null'
              CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(4,IROCK))
              WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &          PERM(4,IROCK)
            ENDIF
!
!---        Pressure parameter   ---
!
            VARB = 'Pressure Parameter'
            IF( IJK.GT.0 ) THEN
              INDX = 5
              LNDX = 9
              UNTS = 'pa'
              IUNM = -1
              IUNKG = 1
              IUNS = -2
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(5,IROCK))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',PERM(5,IROCK)
              INDX = 0
              IUNM = -1
              IUNKG = 1
              IUNS = -2
              CALL RDUNIT(UNTS,PERM(5,IROCK),INDX)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',PERM(5,IROCK),', Pa)'
            ENDIF
!
!---      Mohr-Coulomb stress permeability model   ---
!
          ELSEIF( INDEX(ADUM(1:),'mohr').NE.0 .AND.
     &      INDEX(ADUM(1:),'coulomb').NE.0 .AND.
     &      INDEX(ADUM(1:),'stress').NE.0 ) THEN
            WRITE(IWR,'(A)') 'Mohr-Coulomb Stress Permeability Model'
            IF( IJK.GT.0 ) THEN
              DO 430 N = 1,NFLD
                IPRF(N) = IPRF(N) + 20
  430         CONTINUE
            ELSE
              IPRF(IROCK) = IPRF(IROCK) + 20
            ENDIF
!
!---        Minimum principal stress (Pa)   ---
!
            VARB = 'Minimum Principal Stress'
            IF( IJK.GT.0 ) THEN
              INDX = 4
              LNDX = 9
              UNTS = 'pa'
              IUNM = -1
              IUNKG = 1
              IUNS = -2
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(4,IROCK))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',PERM(4,IROCK)
              INDX = 0
              IUNM = -1
              IUNKG = 1
              IUNS = -2
              CALL RDUNIT(UNTS,PERM(4,IROCK),INDX)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',PERM(4,IROCK),', Pa)'
            ENDIF
!
!---        Maximum principal stress (Pa)   ---
!
            VARB = 'Maximum Principal Stress'
            IF( IJK.GT.0 ) THEN
              INDX = 5
              LNDX = 9
              UNTS = 'pa'
              IUNM = -1
              IUNKG = 1
              IUNS = -2
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(5,IROCK))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',PERM(5,IROCK)
              INDX = 0
              IUNM = -1
              IUNKG = 1
              IUNS = -2
              CALL RDUNIT(UNTS,PERM(5,IROCK),INDX)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',PERM(5,IROCK),', Pa)'
            ENDIF
!
!---        Coefficient of friction parameter   ---
!
            VARB = 'Coefficient of Friction Parameter'
            IF( IJK.GT.0 ) THEN
              UNTS = 'null'
              INDX = 6
              LNDX = 9
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
            ELSE
              UNTS = 'null'
              CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(6,IROCK))
              WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &          PERM(6,IROCK)
            ENDIF
!
!---        Cohesion parameter   ---
!
            VARB = 'Cohesion Parameter'
            IF( IJK.GT.0 ) THEN
              INDX = 7
              LNDX = 9
              UNTS = 'pa'
              IUNM = -1
              IUNKG = 1
              IUNS = -2
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(7,IROCK))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',PERM(7,IROCK)
              INDX = 0
              IUNM = -1
              IUNKG = 1
              IUNS = -2
              CALL RDUNIT(UNTS,PERM(7,IROCK),INDX)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',PERM(7,IROCK),', Pa)'
            ENDIF
!
!---        Maximum permeability factor parameter   ---
!
            VARB = 'Maximum Permeability Factor Parameter'
            IF( IJK.GT.0 ) THEN
              UNTS = 'null'
              INDX = 8
              LNDX = 9
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
            ELSE
              UNTS = 'null'
              CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(8,IROCK))
              WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',
     &          PERM(8,IROCK)
            ENDIF
!
!---        Mohr-Coulomb Stress Ramp Parameter   ---
!
            VARB = 'Mohr-Coulomb Stress Ramp Parameter'
            IF( IJK.GT.0 ) THEN
              INDX = 9
              LNDX = 9
              UNTS = 'pa'
              IUNM = -1
              IUNKG = 1
              IUNS = -2
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(9,IROCK))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',PERM(9,IROCK)
              INDX = 0
              IUNM = -1
              IUNKG = 1
              IUNS = -2
              CALL RDUNIT(UNTS,PERM(9,IROCK),INDX)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',PERM(9,IROCK),', Pa)'
            ENDIF
!
!---        Young's modulus (Pa)  ---
!
            VARB = 'Young''s Modulus'
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
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',PROP_GM(1,IROCK)
              INDX = 0
              CALL RDUNIT(UNTS,PROP_GM(1,IROCK),INDX)
               WRITE(IWR,'(A,1PE11.4,A)') ' (',PROP_GM(1,IROCK),', Pa)'
            ENDIF
!
!---        Poisson's ratio  ---
!
            VARB = 'Poisson''s Ratio'
            IDFLT = 1
            UNTS = 'null'
            INDX = 2
            LNDX = 9
            IF( IJK.GT.0 ) THEN
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PROP_GM,INDX,LNDX )
            ELSE
              IDFLT = 1
              CALL RDDPR(ISTART,ICOMMA,CHDUM,PROP_GM(2,IROCK))
              WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &          ': ',PROP_GM(2,IROCK)
            ENDIF
!
!---        Biot Coefficient  ---
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
              WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &          ': ',PROP_GM(3,IROCK)
            ENDIF
!
!---        Volumetric thermal expansion coefficient (1/K)  ---
!
            VARB = 'Volumetric Thermal Expansion Coefficient'
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
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',PROP_GM(4,IROCK)
              INDX = 0
              CALL RDUNIT(UNTS,PROP_GM(4,IROCK),INDX)
               WRITE(IWR,'(A,1PE11.4,A)') ' (',PROP_GM(4,IROCK),', 1/K)'
            ENDIF
!
!---      Fenton-Hill fracture permeability model   ---
!
          ELSEIF( INDEX(ADUM(1:),'fenton').NE.0 .AND.
     &      INDEX(ADUM(1:),'hill').NE.0 ) THEN
            WRITE(IWR,'(A)') 'Fenton-Hill Fracture Aperture Model'
            IF( IJK.GT.0 ) THEN
              DO 440 N = 1,NFLD
                IPRF(N) = IPRF(N) + 30
  440         CONTINUE
            ELSE
              IPRF(IROCK) = IPRF(IROCK) + 30
            ENDIF
!
!---        Initial fracture aperture, m   ---
!
            VARB = 'Initial Fracture Aperture'
            IF( IJK.GT.0 ) THEN
              INDX = 4
              LNDX = 9
              UNTS = 'm'
              IUNM = 1
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(4,IROCK))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',PERM(4,IROCK)
              INDX = 0
              IUNM = 1
              CALL RDUNIT(UNTS,PERM(4,IROCK),INDX)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',PERM(4,IROCK),', m)'
            ENDIF
!
!---        Propped fracture aperture, m   ---
!
            VARB = 'Propped Fracture Aperture'
            IF( IJK.GT.0 ) THEN
              INDX = 5
              LNDX = 9
              UNTS = 'm'
              IUNM = 1
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(5,IROCK))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',PERM(5,IROCK)
              INDX = 0
              IUNM = 1
              CALL RDUNIT(UNTS,PERM(5,IROCK),INDX)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',PERM(5,IROCK),', m)'
            ENDIF
!
!---        Fracture radius, m   ---
!
            VARB = 'Fracture Radius'
            IF( IJK.GT.0 ) THEN
              INDX = 6
              LNDX = 9
              UNTS = 'm'
              IUNM = 1
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(6,IROCK))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',PERM(6,IROCK)
              INDX = 0
              IUNM = 1
              CALL RDUNIT(UNTS,PERM(6,IROCK),INDX)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',PERM(6,IROCK),', m)'
            ENDIF
!
!---        Fracture z-centroid, m   ---
!
            VARB = 'Fracture Z-Centroid'
            IF( IJK.GT.0 ) THEN
              INDX = 7
              LNDX = 9
              UNTS = 'm'
              IUNM = 1
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(7,IROCK))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',PERM(7,IROCK)
              INDX = 0
              IUNM = 1
              CALL RDUNIT(UNTS,PERM(7,IROCK),INDX)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',PERM(7,IROCK),', m)'
            ENDIF
!
!---        Fracture normal stress, Pa   ---
!
            VARB = 'Fracture Normal Stress'
            IF( IJK.GT.0 ) THEN
              INDX = 8
              LNDX = 9
              UNTS = 'pa'
              IUNM = -1
              IUNKG = 1
              IUNS = -2
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(8,IROCK))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',PERM(8,IROCK)
              INDX = 0
              IUNM = -1
              IUNKG = 1
              IUNS = -2
              CALL RDUNIT(UNTS,PERM(8,IROCK),INDX)
              WRITE(IWR,'(A,1PE11.4,A)') ' (',PERM(8,IROCK),', Pa)'
            ENDIF
!
!---        Young's modulus, Pa  ---
!
            VARB = 'Young''s Modulus'
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
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',PROP_GM(1,IROCK)
              INDX = 0
              CALL RDUNIT(UNTS,PROP_GM(1,IROCK),INDX)
               WRITE(IWR,'(A,1PE11.4,A)') ' (',PROP_GM(1,IROCK),', Pa)'
            ENDIF
!
!---        Poisson's ratio  ---
!
            VARB = 'Poisson''s Ratio'
            IDFLT = 1
            UNTS = 'null'
            INDX = 2
            LNDX = 9
            IF( IJK.GT.0 ) THEN
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PROP_GM,INDX,LNDX )
            ELSE
              IDFLT = 1
              CALL RDDPR(ISTART,ICOMMA,CHDUM,PROP_GM(2,IROCK))
              WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &          ': ',PROP_GM(2,IROCK)
            ENDIF
!
!---        Linear thermal coefficient of expansion, 1/C  ---
!
            VARB = 'Linear Thermal Coefficient of Expansion'
            IDFLT = 1
            UNTS = '1/C'
            IUNK = -1
            INDX = 3
            LNDX = 9
            IF( IJK.GT.0 ) THEN
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PROP_GM,INDX,LNDX )
            ELSE
              IDFLT = 1
              CALL RDDPR(ISTART,ICOMMA,CHDUM,PROP_GM(3,IROCK))
              IDFLT = 1
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &          UNTS(1:NCH),': ',PROP_GM(3,IROCK)
              INDX = 0
              CALL RDUNIT(UNTS,PROP_GM(3,IROCK),INDX)
               WRITE(IWR,'(A,1PE11.4,A)') ' (',PROP_GM(3,IROCK),', 1/C)'
            ENDIF
!
!---      Verma and Pruess porosity-permeability model   ---
!
          ELSEIF( INDEX(ADUM(1:),'verma').NE.0 .AND.
     &      INDEX(ADUM(1:),'pruess').NE.0 ) THEN
            WRITE(IWR,'(A)') 'Verma and Pruess ' //
     &        'Porosity-Permeability Model'
            IF( IJK.GT.0 ) THEN
              DO 450 N = 1,NFLD
                IPRF(N) = IPRF(N) + 40
  450         CONTINUE
            ELSE
              IPRF(IROCK) = IPRF(IROCK) + 40
            ENDIF
!
!---        Initial porosity   ---
!
            VARB = 'Initial Porosity'
            IF( IJK.GT.0 ) THEN
              INDX = 4
              LNDX = 9
              UNTS = 'null'
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(4,IROCK))
              WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &          ': ',PERM(4,IROCK)
            ENDIF
!
!---        Critical porosity   ---
!
            VARB = 'Critical Porosity'
            IF( IJK.GT.0 ) THEN
              INDX = 5
              LNDX = 9
              UNTS = 'null'
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(5,IROCK))
              WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &          ': ',PERM(5,IROCK)
            ENDIF
!
!---        Function Exponent   ---
!
            VARB = 'Function Exponent'
            IF( IJK.GT.0 ) THEN
              INDX = 6
              LNDX = 9
              UNTS = 'null'
              CALL RDIJKD( ISTART,IJK,CHDUM,UNTS,PERM,INDX,LNDX )
            ELSE
              CALL RDDPR(ISTART,ICOMMA,CHDUM,PERM(6,IROCK))
              WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),
     &          ': ',PERM(6,IROCK)
            ENDIF
          ENDIF
        ENDIF
!
!---    Loop over remaining rock/soils within scaling group  ---
!
        IF( ISLC(19).EQ.1 .AND. IROCK.LT.NROCK ) THEN
          DO 490 M = IROCK+1,NROCK
            IF( ISCALE(M).EQ.ISGRP ) THEN
              N = N+1
              IDP(M) = IDP(IROCK)
              DO 480 L = 1,9
                PERM(L,M) = PERM(L,IROCK)
  480         CONTINUE
            ENDIF
  490     CONTINUE
        ENDIF
!
!---  Read next rock/soil type or scaling group  ---
!
        IF( N.LT.NROCK ) WRITE(IWR,'(/)')
        GOTO 10
 500  CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDHYDR group ---
!
      RETURN
      END

