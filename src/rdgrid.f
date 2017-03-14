!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDGRID
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
!     Read input file for grid geometry information.
!     Compute geometric length, area, and volume parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, November 1992.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GRID
      USE FILES
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
      CHARACTER*64 ADUM,UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDGRID'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
      IWRGRD = 0
      THXZ = 0.D+0
      THYZ = 0.D+0
!
!---  Write card information to ouput file  ---
!
      CARD = 'Grid Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read coordinate system type  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Coordinate System'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      IF( INDEX(ADUM(1:),'tilted').NE.0 ) THEN
        ICS = 1
        WRITE(IWR,'(/,A)') 'Tilted Bed Coordinate System'
        VARB = 'X-Z Plane Horizontal Tilt'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,THXZ)
        VARB = 'X-Z Plane Horizontal Tilt Units'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(3A,1PE11.4)') 'X-Z Plane Horizontal Tilt,'
     &,UNTS(1:NCH),': ',THXZ
        INDX = 0
        CALL RDUNIT(UNTS,THXZ,INDX)
        VARB = 'Y-Z Plane Horizontal Tilt'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,THYZ)
        VARB = 'Y-Z Plane Horizontal Tilt Units'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(3A,1PE11.4)') 'Y-Z Plane Horizontal Tilt,'
     &,UNTS(1:NCH),': ',THYZ
        INDX = 0
        CALL RDUNIT(UNTS,THYZ,INDX)
        GRAVX = GRAV*SIN(THXZ)*COS(THYZ)
        GRAVY = GRAV*COS(THXZ)*SIN(THYZ)
        GRAVZ = GRAV*COS(THXZ)*COS(THYZ)
        WRITE(IWR,'(A,1PE11.4)') 'X-Direction Gravitational ' //
     &    'Accleration, m/s^2: ',GRAVX
        WRITE(IWR,'(A,1PE11.4)') 'Y-Direction Gravitational ' //
     &    'Accleration,  m/s^2: ',GRAVY
        WRITE(IWR,'(A,1PE11.4)') 'Z-Direction Gravitational ' //
     &    'Accleration, m/s^2: ',GRAVZ
      ELSEIF( INDEX(ADUM(1:),'cartesian').NE.0 ) THEN
        IF( INDEX(ADUM(1:),'uniform').NE.0 ) THEN
          ICS = 5
          WRITE(IWR,'(/,A)') 'Uniform Cartesian Coordinate System'
        ELSE
          ICS = 1
          WRITE(IWR,'(/,A)') 'Cartesian Coordinate System'
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'cylindrical').NE.0 ) THEN
        IF( INDEX(ADUM(1:),'uniform').NE.0 ) THEN
          ICS = 6
          WRITE(IWR,'(/,A)') 'Uniform Cylindrical Coordinate System'
        ELSE
          ICS = 2
          WRITE(IWR,'(/,A)') 'Cylindrical Coordinate System'
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'boundary').NE.0 .OR.
     &  INDEX(ADUM(1:),'fitted').NE.0  .OR.
     &  INDEX(ADUM(1:),'orthogonal').NE.0 ) THEN
        ICS = 3
        WRITE(IWR,'(/,A)') 'Orthogonal Boundary Fitted ' //
     &    'Coordinate System'
      ELSEIF( INDEX(ADUM(1:),'eclipse').NE.0 .AND.
     &  INDEX(ADUM(1:),'generic').NE.0 ) THEN
        ICS = 7
        WRITE(IWR,'(/,A)') 'Generic Eclipse Input'
        WRITE(IWR,'(/,A)') 'Orthogonal Boundary Fitted ' //
     &    'Coordinate System'
      ELSEIF( INDEX(ADUM(1:),'earthvision').NE.0 .AND.
     &  INDEX(ADUM(1:),'sampled').NE.0 ) THEN
        ICS = 8
        WRITE(IWR,'(/,A)') 'EarthVision Sampled Input'
        WRITE(IWR,'(/,A)') 'Orthogonal Boundary Fitted ' //
     &    'Coordinate System'
      ELSEIF( INDEX(ADUM(1:),'element').NE.0 .AND.
     &  INDEX(ADUM(1:),'vertices').NE.0 ) THEN
        ICS = 9
        WRITE(IWR,'(/,A)') 'Element and Vertices Grid Input'
        WRITE(IWR,'(/,A)') 'Orthogonal Boundary Fitted ' //
     &    'Coordinate System'
      ELSEIF( INDEX(ADUM(1:),'xyz').NE.0 .AND.
     &  INDEX(ADUM(1:),'vertice').NE.0 ) THEN
        ICS = 10
        WRITE(IWR,'(/,A)') 'XYZ Vertices Grid Input'
        WRITE(IWR,'(/,A)') 'Orthogonal Boundary Fitted ' //
     &    'Coordinate System'
      ELSE
        INDX = 4
        CHMSG = 'Unrecognized Coordinate System Type: ' // ADUM
        CALL WRMSGS( INDX )
      ENDIF
      IF( INDEX(ADUM(1:),'write').NE.0 ) IWRGRD = 1
!
!---  Read reference point for output  ---
!
      IF( INDEX(ADUM(1:),'reference').NE.0 .AND. 
     &  INDEX(ADUM(1:),'depth').NE.0 ) THEN
        VARB = 'Reference Grid Depth'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,XREF(3))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',
     &    UNTS(1:NCH),': ',XREF(3)
        INDX = 0
        IUNM = 1
        CALL RDUNIT(UNTS,XREF(3),INDX)
        WRITE(IWR,'(A,1PE11.4,A)') ' (',XREF(3),', m)'
        IXREF(3) = 1
      ELSEIF( INDEX(ADUM(1:),'reference').NE.0 ) THEN
        INDX = 0
        VARB = 'X Reference Point'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,XREF(1))
        VARB = 'X Reference Point Units'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,XREFU(1))
        VARB = 'X-Direction Distance from Reference Point'
        CALL RDINT(ISTART,ICOMMA,CHDUM,IXREF(1))
        WRITE(IWR,'(2X,3A,1PE11.4,$)') 'X Reference Point, ',XREFU(1),
     &    ': ',XREF(1)
        IUNM = 1
        CALL RDUNIT( XREFU(1),XREF(1),INDX )
        WRITE(IWR,'(A,1PE11.4,A,$)') ' (',XREF(1),', m)'
        IF( IXREF(1).LT.0 ) THEN
          WRITE(IWR,'(A)') ': Descending direction.'
        ELSE
          WRITE(IWR,'(A)') ': Ascending direction.'
        ENDIF
        VARB = 'Y Reference Point'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,XREF(2))
        VARB = 'Y Reference Point Units'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,XREFU(2))
        VARB = 'Y-Direction Distance from Reference Point'
        CALL RDINT(ISTART,ICOMMA,CHDUM,IXREF(2))
        WRITE(IWR,'(2X,3A,1PE11.4,$)') 'Y Reference Point, ',XREFU(2),
     &    ': ',XREF(2)
        IUNM = 1
        CALL RDUNIT( XREFU(2),XREF(2),INDX )
        WRITE(IWR,'(A,1PE11.4,A,$)') ' (',XREF(2),', m)'
        IF( IXREF(2).LT.0 ) THEN
          WRITE(IWR,'(A)') ': Descending direction.'
        ELSE
          WRITE(IWR,'(A)') ': Ascending direction.'
        ENDIF
        VARB = 'Z Reference Point'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,XREF(3))
        VARB = 'Z Reference Point Units'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,XREFU(3))
        VARB = 'Z-Direction Distance from Reference Point'
        CALL RDINT(ISTART,ICOMMA,CHDUM,IXREF(3))
        WRITE(IWR,'(2X,3A,1PE11.4,$)') 'Z Reference Point, ',XREFU(3),
     &    ': ',XREF(3)
        IUNM = 1
        CALL RDUNIT( XREFU(3),XREF(3),INDX )
        IF( IXREF(3).LT.0 ) THEN
          WRITE(IWR,'(A)') ': Descending direction.'
        ELSE
          WRITE(IWR,'(A)') ': Ascending direction.'
        ENDIF
      ENDIF
!
!---  Read coordinate system node dimensions  ---
!
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      VARB = 'Number of I-indexed Nodes'
      CALL RDINT(ISTART,ICOMMA,CHDUM,IFLD)
      VARB = 'Number of J-indexed Nodes'
      CALL RDINT(ISTART,ICOMMA,CHDUM,JFLD)
      VARB = 'Number of K-indexed Nodes'
      CALL RDINT(ISTART,ICOMMA,CHDUM,KFLD)
!
!---  Write coordinate information and compute dimensionality  ---
!
      WRITE (IWR,'(/,A)') 'Coordinate System Dimensions'
      WRITE (IWR,'(2X,A,I9)') 'Number of I-indexed Nodes:   ',IFLD
      WRITE (IWR,'(2X,A,I9)') 'Number of J-indexed Nodes:   ',JFLD
      WRITE (IWR,'(2X,A,I9)') 'Number of K-indexed Nodes:   ',KFLD
      NDIM = 0
      IF( IFLD.GT.1 ) NDIM = NDIM+1
      IF( JFLD.GT.1 ) NDIM = NDIM+1
      IF( KFLD.GT.1 ) NDIM = NDIM+1
      IF( NDIM.GT.LAD ) THEN
        INDX = 5
        CHMSG = 'Number of Active Dimensions > Parameter LAD'
        CALL WRMSGS( INDX )
      ENDIF
      IF( NDIM.LT.1 ) NDIM = 1
      MDX = 0
      IF( KFLD.GT.1 ) THEN
        MDX = MDX+1
        MDIM(1) = MDX
      ENDIF
      IF( JFLD.GT.1 ) THEN
        MDX = MDX+1
        MDIM(2) = MDX
      ENDIF
      IF( IFLD.GT.1 ) THEN
        MDX = MDX+1
        MDIM(3) = MDX
      ENDIF
      IF( IFLD.GT.1 ) THEN
        MDX = MDX+1
        MDIM(4) = MDX
      ENDIF
      IF( JFLD.GT.1 ) THEN
        MDX = MDX+1
        MDIM(5) = MDX
      ENDIF
      IF( KFLD.GT.1 ) THEN
        MDX = MDX+1
        MDIM(6) = MDX
      ENDIF
      IJFLD = IFLD*JFLD
      JKFLD = JFLD*KFLD
      KIFLD = KFLD*IFLD
!
!---  For restart simulations, check that the number of field nodes
!     agree in count  ---
!
      IF( IEO.EQ.2 ) THEN      
        IF( NFLD.NE.IJFLD*KFLD ) THEN
          INDX = 3
          CHMSG = 'Restart File Number of Nodes Conflict'
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
      NFLD = IJFLD*KFLD
      NFBN = NFLD
!
!---  Check coordinate system dimensions against parameter sizes ---
!
      IF( IFLD.LT.1  ) THEN
        INDX = 5
        CHMSG = '1 > Num. of X-Dir. Nodes'
        CALL WRMSGS( INDX )
      ELSEIF( IFLD.GT.LFX ) THEN
        INDX = 5
        CHMSG = 'Num. of X-Dir. Nodes > Parameter LFX'
        CALL WRMSGS( INDX )
      ELSEIF( JFLD.LT.1  ) THEN
        INDX = 5
        CHMSG = '1 > Num. of Y-Dir. Nodes'
        CALL WRMSGS( INDX )
      ELSEIF( JFLD.GT.LFY ) THEN
        INDX = 5
        CHMSG = 'Num. of Y-Dir. Nodes > Parameter LFY'
        CALL WRMSGS( INDX )
      ELSEIF( KFLD.LT.1  ) THEN
        INDX = 5
        CHMSG = '1 > Num. of Z-Dir. Nodes'
        CALL WRMSGS( INDX )
      ELSEIF( KFLD.GT.LFZ ) THEN
        INDX = 5
        CHMSG = 'Num. of Z-Dir. Nodes > Parameter LFZ'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Record coordinate system physical dimensions  ---
!
      IF( ICS.NE.3 ) THEN
        WRITE (IWR,'(/,A)') 'Coordinate System Physical Dimensions'
      ENDIF
!
!---  Define the grid pointers  ---
!
      DO 130 K = 1,KFLD
        DO 120 J = 1,JFLD
          DO 110 I = 1,IFLD
            N = (K-1)*IJFLD + (J-1)*IFLD + I
            ND(I,J,K) = N
            ID(N) = I
            JD(N) = J
            KD(N) = K
            NSX(N) = (K-1)*JFLD*(IFLD+1) + (J-1)*(IFLD+1) + I
            NSY(N) = (K-1)*(JFLD+1)*IFLD + (J-1)*IFLD + I
            NSZ(N) = N
            NSSX(N) = (K-1)*JFLD*(IFLD+1) + (J-1)*(IFLD+1) + I + 1
            NSSY(N) = (K-1)*(JFLD+1)*IFLD + (J-1)*IFLD + I + IFLD
            NSSZ(N) = N + IFLD*JFLD
            IBR(1,N) = 0
            IBR(2,N) = 0
            IBR(3,N) = 0
            IBR(4,N) = N
            IBR(5,N) = N
  110     CONTINUE
  120   CONTINUE
  130 CONTINUE
!
!---  Variable grid spacing  ---
!
      IF( ICS.EQ.1 .OR. ICS.EQ.2 ) THEN
        CALL RDVBLGRID
!
!---  Uniform grid spacing  ---
!
      ELSEIF( ICS.EQ.5 .OR. ICS.EQ.6 ) THEN
        CALL RDUNFGRID
!
!---  ECLIPSE formatted grids  ---
!
      ELSEIF( ICS.EQ.7 ) THEN
        CALL RDECLGRID
!
!---  Grids defined through hexahedral vertices  ---
!
      ELSEIF( ICS.EQ.3 .OR. ICS.EQ.8 ) THEN
        CALL RDHEXGRID
!
!---  Grids defined through hexahedral elements and vertices  ---
!
      ELSEIF( ICS.EQ.9 ) THEN
        CALL RDELMGRID
!
!---  Grids defined through a listing of x, y, and z vertices  ---
!
      ELSEIF( ICS.EQ.10 ) THEN
        CALL RDXYZVERT
      ENDIF
!
!---  Set indices for internal boundary surfaces for split grids  ---
!
      CALL SPLIT_GRID
!
!---  Reset grid type  ---
!
      IF( ICS.EQ.5 ) ICS = 1
      IF( ICS.EQ.6 ) ICS = 2
!
!---  Area and volume for Cartesian grids  ---
!
      IF( ICS.EQ.1 ) THEN
        CALL AVCART
!
!---  Area and volume for Cylindrical grids  ---
!
      ELSEIF( ICS.EQ.2 ) THEN
        CALL AVCYLN
!
!---  Area and volume for hexahedral, ECLIPSE, and 
!     Earthvision grids  ---
!
      ELSEIF( ICS.EQ.3 .OR. ICS.EQ.7 .OR. ICS.EQ.8 .OR. ICS.EQ.9
     &   .OR. ICS.EQ.10 ) THEN
        CALL AVHEX
        ICS = 3
      ENDIF
!
!---  Define active nodes assuming no inactive nodes  ---
!
      NC = 1
      IF( IJFLD.LE.JKFLD .AND. IJFLD.LE.KIFLD ) THEN
!
!---  X-Y Plane yields the lowest band width.
!---  Load Jacobian matrix in the increment order I,J,K
!
        DO 230 K = 1,KFLD
          DO 220 J = 1,JFLD
            DO 210 I = 1,IFLD
              N = ND(I,J,K)
!
!---          Fault inactive node  ---
!
              IF( IXP(N).EQ.-1 ) THEN
                IXP(N) = 0
              ELSE
                IXP(N) = NC
                NC = NC+1
              ENDIF
  210       CONTINUE
  220     CONTINUE
  230   CONTINUE
      ELSEIF( JKFLD.LE.IJFLD .AND. JKFLD.LE.KIFLD ) THEN
!
!---  Y-Z Plane yields the lowest band width.
!---  Load Jacobian matrix in the increment order J,K,I
!
        DO 330 I = 1,IFLD
          DO 320 K = 1,KFLD
            DO 310 J = 1,JFLD
              N = ND(I,J,K)
!
!---          Fault inactive node  ---
!
              IF( IXP(N).EQ.-1 ) THEN
                IXP(N) = 0
              ELSE
                IXP(N) = NC
                NC = NC+1
              ENDIF
  310       CONTINUE
  320     CONTINUE
  330   CONTINUE
      ELSEIF( KIFLD.LE.IJFLD .AND. KIFLD.LE.JKFLD ) THEN
!
!---  Z-X Plane yields the lowest band width.
!---  Load Jacobian matrix in the increment order K,I,J
!
        DO 430 J = 1,JFLD
          DO 420 I = 1,IFLD
            DO 410 K = 1,KFLD
              N = ND(I,J,K)
!
!---          Fault inactive node  ---
!
              IF( IXP(N).EQ.-1 ) THEN
                IXP(N) = 0
              ELSE
                IXP(N) = NC
                NC = NC+1
              ENDIF
  410       CONTINUE
  420     CONTINUE
  430   CONTINUE
      ENDIF
!
!---  Write grid to an external file --
!
      IF( IWRGRD.EQ.1 ) THEN
        OPEN( UNIT=26,FILE='grid',STATUS='UNKNOWN',FORM='FORMATTED' )
        REWIND( UNIT=26 )
        WRITE(26,*) IFLD,JFLD,KFLD
        WRITE(26,*)((XE(M,N),M=1,8),N=1,NFLD)
        WRITE(26,*)((YE(M,N),M=1,8),N=1,NFLD)
        WRITE(26,*)((ZE(M,N),M=1,8),N=1,NFLD)
        CLOSE(UNIT=26)
      ENDIF
!
!---  End of RDGRID group ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE AVBR1
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
!     Surface areas and volumes block refinement nodes.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 21 January 2014.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GRID
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
      REAL*8 XVX(8),YVX(8),ZVX(8)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/AVBR1'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Compute cell centroids, differential lengths,
!     and gravitational components  ---
!
      DO 200 N = 1,NFLD
        NRX = IBR(4,N)
        IF( NRX.LE.N ) CYCLE
        NB = N-IJFLD
        NS = N-IFLD
        NW = N-1
        NE = N+1
        NN = N+IFLD
        NT = N+IJFLD
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 190 K = 1,KRX
        DO 180 J = 1,JRX
        DO 170 I = 1,IRX
          VOL(NRX) = 0.D+0
!
!---      Cell centroid  ---
!
          PXMIN = 1.D+20
          PXMAX = -1.D+20
          PYMIN = 1.D+20
          PYMAX = -1.D+20
          PZMIN = 1.D+20
          PZMAX = -1.D+20
!
!---      West surface centroid  ---
!
          XVX(1) = XE(1,NRX)
          XVX(2) = XE(3,NRX)
          XVX(3) = XE(7,NRX)
          XVX(4) = XE(5,NRX)
          YVX(1) = YE(1,NRX)
          YVX(2) = YE(3,NRX)
          YVX(3) = YE(7,NRX)
          YVX(4) = YE(5,NRX)
          ZVX(1) = ZE(1,NRX)
          ZVX(2) = ZE(3,NRX)
          ZVX(3) = ZE(7,NRX)
          ZVX(4) = ZE(5,NRX)
          NP = 4
          DO 10 NX = 1,NP
            PXMIN = MIN( XVX(NX),PXMIN )
            PXMAX = MAX( XVX(NX),PXMAX )
            PYMIN = MIN( YVX(NX),PYMIN )
            PYMAX = MAX( YVX(NX),PYMAX )
            PZMIN = MIN( ZVX(NX),PZMIN )
            PZMAX = MAX( ZVX(NX),PZMAX )
   10     CONTINUE
          CALL PGCNTRD( NP,XVX,YVX,ZVX,XFW,YFW,ZFW )
!
!---      West surface area and volume contributions  ---
!
          NPX = NSX(NRX)
          AFX(NPX) = 0.D+0
          DO 12 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TRGAREA( XVX(ITX),YVX(ITX),ZVX(ITX),
     &        XVX(JTX),YVX(JTX),ZVX(JTX),XFW,YFW,ZFW,AFXX )
            AFX(NPX) = AFX(NPX)+AFXX
   12     CONTINUE
!
!---      East surface centroid  ---
!
          XVX(1) = XE(2,NRX)
          XVX(2) = XE(4,NRX)
          XVX(3) = XE(8,NRX)
          XVX(4) = XE(6,NRX)
          YVX(1) = YE(2,NRX)
          YVX(2) = YE(4,NRX)
          YVX(3) = YE(8,NRX)
          YVX(4) = YE(6,NRX)
          ZVX(1) = ZE(2,NRX)
          ZVX(2) = ZE(4,NRX)
          ZVX(3) = ZE(8,NRX)
          ZVX(4) = ZE(6,NRX)
          NP = 4
          DO 20 NX = 1,NP
            PXMIN = MIN( XVX(NX),PXMIN )
            PXMAX = MAX( XVX(NX),PXMAX )
            PYMIN = MIN( YVX(NX),PYMIN )
            PYMAX = MAX( YVX(NX),PYMAX )
            PZMIN = MIN( ZVX(NX),PZMIN )
            PZMAX = MAX( ZVX(NX),PZMAX )
   20     CONTINUE
          CALL PGCNTRD( NP,XVX,YVX,ZVX,XFE,YFE,ZFE )
!
!---      East surface area and volume contributions  ---
!
          NQX = NSX(NRX)+1
          AFX(NQX) = 0.D+0
          DO 22 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TRGAREA( XVX(ITX),YVX(ITX),ZVX(ITX),
     &        XVX(JTX),YVX(JTX),ZVX(JTX),XFE,YFE,ZFE,AFXX )
            AFX(NQX) = AFX(NQX)+AFXX
   22     CONTINUE
!
!---      South surface centroid  ---
!
          XVX(1) = XE(1,NRX)
          XVX(2) = XE(2,NRX)
          XVX(3) = XE(6,NRX)
          XVX(4) = XE(5,NRX)
          YVX(1) = YE(1,NRX)
          YVX(2) = YE(2,NRX)
          YVX(3) = YE(6,NRX)
          YVX(4) = YE(5,NRX)
          ZVX(1) = ZE(1,NRX)
          ZVX(2) = ZE(2,NRX)
          ZVX(3) = ZE(6,NRX)
          ZVX(4) = ZE(5,NRX)
          NP = 4
          DO 30 NX = 1,NP
            PXMIN = MIN( XVX(NX),PXMIN )
            PXMAX = MAX( XVX(NX),PXMAX )
            PYMIN = MIN( YVX(NX),PYMIN )
            PYMAX = MAX( YVX(NX),PYMAX )
            PZMIN = MIN( ZVX(NX),PZMIN )
            PZMAX = MAX( ZVX(NX),PZMAX )
   30     CONTINUE
          CALL PGCNTRD( NP,XVX,YVX,ZVX,XFS,YFS,ZFS )
!
!---      South surface area and volume contributions  ---
!
          NPY = NSY(NRX)
          AFY(NPY) = 0.D+0
          DO 32 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TRGAREA( XVX(ITX),YVX(ITX),ZVX(ITX),
     &        XVX(JTX),YVX(JTX),ZVX(JTX),XFS,YFS,ZFS,AFYX )
            AFY(NPY) = AFY(NPY)+AFYX
  32     CONTINUE
!
!---      North surface centroid  ---
!
          XVX(1) = XE(3,NRX)
          XVX(2) = XE(4,NRX)
          XVX(3) = XE(8,NRX)
          XVX(4) = XE(7,NRX)
          YVX(1) = YE(3,NRX)
          YVX(2) = YE(4,NRX)
          YVX(3) = YE(8,NRX)
          YVX(4) = YE(7,NRX)
          ZVX(1) = ZE(3,NRX)
          ZVX(2) = ZE(4,NRX)
          ZVX(3) = ZE(8,NRX)
          ZVX(4) = ZE(7,NRX)
          NP = 4
          DO 40 NX = 1,NP
            PXMIN = MIN( XVX(NX),PXMIN )
            PXMAX = MAX( XVX(NX),PXMAX )
            PYMIN = MIN( YVX(NX),PYMIN )
            PYMAX = MAX( YVX(NX),PYMAX )
            PZMIN = MIN( ZVX(NX),PZMIN )
            PZMAX = MAX( ZVX(NX),PZMAX )
   40     CONTINUE
          CALL PGCNTRD( NP,XVX,YVX,ZVX,XFN,YFN,ZFN )
!
!---      North surface area and volume contributions  ---
!
          NQY = NSY(NRX)+IRX
          AFY(NQY) = 0.D+0
          DO 42 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TRGAREA( XVX(ITX),YVX(ITX),ZVX(ITX),
     &        XVX(JTX),YVX(JTX),ZVX(JTX),XFN,YFN,ZFN,AFYX )
            AFY(NQY) = AFY(NQY)+AFYX
   42     CONTINUE
!
!---      Bottom surface centroid  ---
!
          XVX(1) = XE(1,NRX)
          XVX(2) = XE(2,NRX)
          XVX(3) = XE(4,NRX)
          XVX(4) = XE(3,NRX)
          YVX(1) = YE(1,NRX)
          YVX(2) = YE(2,NRX)
          YVX(3) = YE(4,NRX)
          YVX(4) = YE(3,NRX)
          ZVX(1) = ZE(1,NRX)
          ZVX(2) = ZE(2,NRX)
          ZVX(3) = ZE(4,NRX)
          ZVX(4) = ZE(3,NRX)
          NP = 4
          DO 50 NX = 1,NP
            PXMIN = MIN( XVX(NX),PXMIN )
            PXMAX = MAX( XVX(NX),PXMAX )
            PYMIN = MIN( YVX(NX),PYMIN )
            PYMAX = MAX( YVX(NX),PYMAX )
            PZMIN = MIN( ZVX(NX),PZMIN )
            PZMAX = MAX( ZVX(NX),PZMAX )
   50     CONTINUE
          CALL PGCNTRD( NP,XVX,YVX,ZVX,XFB,YFB,ZFB )
!
!---      Bottom surface area and volume contributions  ---
!
          NPZ = NSZ(NRX)
          AFZ(NPZ) = 0.D+0
          DO 52 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TRGAREA( XVX(ITX),YVX(ITX),ZVX(ITX),
     &        XVX(JTX),YVX(JTX),ZVX(JTX),XFB,YFB,ZFB,AFZX )
            AFZ(NPZ) = AFZ(NPZ)+AFZX
   52     CONTINUE
!
!---      Top surface centroid  ---
!
          XVX(1) = XE(5,NRX)
          XVX(2) = XE(6,NRX)
          XVX(3) = XE(8,NRX)
          XVX(4) = XE(7,NRX)
          YVX(1) = YE(5,NRX)
          YVX(2) = YE(6,NRX)
          YVX(3) = YE(8,NRX)
          YVX(4) = YE(7,NRX)
          ZVX(1) = ZE(5,NRX)
          ZVX(2) = ZE(6,NRX)
          ZVX(3) = ZE(8,NRX)
          ZVX(4) = ZE(7,NRX)
          NP = 4
          DO 60 NX = 1,NP
            PXMIN = MIN( XVX(NX),PXMIN )
            PXMAX = MAX( XVX(NX),PXMAX )
            PYMIN = MIN( YVX(NX),PYMIN )
            PYMAX = MAX( YVX(NX),PYMAX )
            PZMIN = MIN( ZVX(NX),PZMIN )
            PZMAX = MAX( ZVX(NX),PZMAX )
   60     CONTINUE
          CALL PGCNTRD( NP,XVX,YVX,ZVX,XFT,YFT,ZFT )
!
!---      Top surface area and volume contributions  ---
!
          NQZ = NSZ(NRX)+IRX*JRX
          AFZ(NQZ) = 0.D+0
          DO 62 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TRGAREA( XVX(ITX),YVX(ITX),ZVX(ITX),
     &        XVX(JTX),YVX(JTX),ZVX(JTX),XFT,YFT,ZFT,AFZX )
            AFZ(NQZ) = AFZ(NQZ)+AFZX
  62     CONTINUE
!
!---      Centroid  ---
!
          XP(NRX) = (XFW+XFE+XFS+XFN+XFB+XFT)/6.D+0
          YP(NRX) = (YFW+YFE+YFS+YFN+YFB+YFT)/6.D+0
          ZP(NRX) = (ZFW+ZFE+ZFS+ZFN+ZFB+ZFT)/6.D+0
          IF( XP(NRX).LT.PXMIN .OR. XP(NRX).GT.PXMAX .OR.
     &      YP(NRX).LT.PYMIN .OR. YP(NRX).GT.PYMAX .OR.
     &      ZP(NRX).LT.PZMIN .OR. ZP(NRX).GT.PZMAX ) THEN
            WRITE(6,'(A)')'Node Centroid Outside of Polygon Limits'
            WRITE(6,'(4(A,I6))')'NRX = ',NRX,' I = ',I,' J = ',J,
     &        ' K = ',K
            WRITE(6,'(3(A,1PE11.4))')'XP = ',XP(NRX),' XMIN = ',PXMIN,
     &        ' XMAX = ',PXMAX
            WRITE(6,'(3(A,1PE11.4))')'YP = ',YP(NRX),' YMIN = ',PYMIN,
     &        ' YMAX = ',PYMAX
            WRITE(6,'(3(A,1PE11.4))')'ZP = ',ZP(NRX),' ZMIN = ',PZMIN,
     &        ' ZMAX = ',PZMAX
          ENDIF
!
!---      West surface vertices  ---
!
          XVX(1) = XE(1,NRX)
          XVX(2) = XE(3,NRX)
          XVX(3) = XE(7,NRX)
          XVX(4) = XE(5,NRX)
          YVX(1) = YE(1,NRX)
          YVX(2) = YE(3,NRX)
          YVX(3) = YE(7,NRX)
          YVX(4) = YE(5,NRX)
          ZVX(1) = ZE(1,NRX)
          ZVX(2) = ZE(3,NRX)
          ZVX(3) = ZE(7,NRX)
          ZVX(4) = ZE(5,NRX)
!
!---      West surface volume contributions  ---
!
          DO 112 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TETRVOL( XVX(ITX),YVX(ITX),ZVX(ITX),XVX(JTX),
     &        YVX(JTX),ZVX(JTX),XFW,YFW,ZFW,XP(NRX),YP(NRX),
     &        ZP(NRX),VOLX )
            VOL(NRX) = VOL(NRX)+VOLX
  112     CONTINUE
!
!---      East surface vertices  ---
!
          XVX(1) = XE(2,NRX)
          XVX(2) = XE(4,NRX)
          XVX(3) = XE(8,NRX)
          XVX(4) = XE(6,NRX)
          YVX(1) = YE(2,NRX)
          YVX(2) = YE(4,NRX)
          YVX(3) = YE(8,NRX)
          YVX(4) = YE(6,NRX)
          ZVX(1) = ZE(2,NRX)
          ZVX(2) = ZE(4,NRX)
          ZVX(3) = ZE(8,NRX)
          ZVX(4) = ZE(6,NRX)
!
!---      East surface volume contributions  ---
!
          DO 122 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TETRVOL( XVX(ITX),YVX(ITX),ZVX(ITX),XVX(JTX),
     &        YVX(JTX),ZVX(JTX),XFE,YFE,ZFE,XP(NRX),YP(NRX),
     &        ZP(NRX),VOLX )
            VOL(NRX) = VOL(NRX)+VOLX
  122     CONTINUE
!
!---      South surface vertices  ---
!
          XVX(1) = XE(1,NRX)
          XVX(2) = XE(2,NRX)
          XVX(3) = XE(6,NRX)
          XVX(4) = XE(5,NRX)
          YVX(1) = YE(1,NRX)
          YVX(2) = YE(2,NRX)
          YVX(3) = YE(6,NRX)
          YVX(4) = YE(5,NRX)
          ZVX(1) = ZE(1,NRX)
          ZVX(2) = ZE(2,NRX)
          ZVX(3) = ZE(6,NRX)
          ZVX(4) = ZE(5,NRX)
!
!---      South surface volume contributions  ---
!
          DO 132 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TETRVOL( XVX(ITX),YVX(ITX),ZVX(ITX),XVX(JTX),
     &        YVX(JTX),ZVX(JTX),XFS,YFS,ZFS,XP(NRX),YP(NRX),
     &        ZP(NRX),VOLX )
            VOL(NRX) = VOL(NRX)+VOLX
 132     CONTINUE
!
!---      North surface vertices  ---
!
          XVX(1) = XE(3,NRX)
          XVX(2) = XE(4,NRX)
          XVX(3) = XE(8,NRX)
          XVX(4) = XE(7,NRX)
          YVX(1) = YE(3,NRX)
          YVX(2) = YE(4,NRX)
          YVX(3) = YE(8,NRX)
          YVX(4) = YE(7,NRX)
          ZVX(1) = ZE(3,NRX)
          ZVX(2) = ZE(4,NRX)
          ZVX(3) = ZE(8,NRX)
          ZVX(4) = ZE(7,NRX)
!
!---      North surface volume contributions  ---
!
          DO 142 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TETRVOL( XVX(ITX),YVX(ITX),ZVX(ITX),XVX(JTX),
     &        YVX(JTX),ZVX(JTX),XFN,YFN,ZFN,XP(NRX),YP(NRX),
     &        ZP(NRX),VOLX )
            VOL(NRX) = VOL(NRX)+VOLX
  142     CONTINUE
!
!---      Bottom surface vertices  ---
!
          XVX(1) = XE(1,NRX)
          XVX(2) = XE(2,NRX)
          XVX(3) = XE(4,NRX)
          XVX(4) = XE(3,NRX)
          YVX(1) = YE(1,NRX)
          YVX(2) = YE(2,NRX)
          YVX(3) = YE(4,NRX)
          YVX(4) = YE(3,NRX)
          ZVX(1) = ZE(1,NRX)
          ZVX(2) = ZE(2,NRX)
          ZVX(3) = ZE(4,NRX)
          ZVX(4) = ZE(3,NRX)
!
!---      Bottom surface volume contributions  ---
!
          DO 152 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TETRVOL( XVX(ITX),YVX(ITX),ZVX(ITX),XVX(JTX),
     &        YVX(JTX),ZVX(JTX),XFB,YFB,ZFB,XP(NRX),YP(NRX),
     &        ZP(NRX),VOLX )
            VOL(NRX) = VOL(NRX)+VOLX
  152     CONTINUE
!
!---      Top surface vertices  ---
!
          XVX(1) = XE(5,NRX)
          XVX(2) = XE(6,NRX)
          XVX(3) = XE(8,NRX)
          XVX(4) = XE(7,NRX)
          YVX(1) = YE(5,NRX)
          YVX(2) = YE(6,NRX)
          YVX(3) = YE(8,NRX)
          YVX(4) = YE(7,NRX)
          ZVX(1) = ZE(5,NRX)
          ZVX(2) = ZE(6,NRX)
          ZVX(3) = ZE(8,NRX)
          ZVX(4) = ZE(7,NRX)
!
!---      Top surface volume contributions  ---
!
          DO 162 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TETRVOL( XVX(ITX),YVX(ITX),ZVX(ITX),XVX(JTX),
     &        YVX(JTX),ZVX(JTX),XFT,YFT,ZFT,XP(NRX),YP(NRX),
     &        ZP(NRX),VOLX )
            VOL(NRX) = VOL(NRX)+VOLX
  162     CONTINUE
!
!---      Check for zero or negative volume  ---
!
          IF( VOL(NRX).LE.0.D+0 ) THEN
            INDX = 7
            IMSG = NRX
             CHMSG = 'Non-Positive Grid-Cell Volume at Node: '
            CALL WRMSGS( INDX )
          ENDIF
!
!---      West-east differentials  ---
!
          DXGF(NRX) = SQRT((XFE-XFW)**2 + (YFE-YFW)**2 + (ZFE-ZFW)**2)
!
!---      South-north differentials  ---
!
          DYGF(NRX) = SQRT((XFN-XFS)**2 + (YFN-YFS)**2 + (ZFN-ZFS)**2)
!
!---      Bottom-top differentials  ---
!
          DZGF(NRX) = SQRT((XFT-XFB)**2 + (YFT-YFB)**2 + (ZFT-ZFB)**2)
          NRX = NRX + 1
  170   CONTINUE
  180   CONTINUE
  190   CONTINUE
  200 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of AVBR1 group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE AVBR2
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
!     Differential lengths, gravitational components,
!     and adjust block refinement areas for surface normal
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 21 January 2014.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GRID
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
      REAL*8 XVX(8),YVX(8),ZVX(8)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/AVBR2'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Compute differential lengths, gravitational components,
!     and adjust block refinement areas for surface normal  ---
!
      DO 400 N = 1,NFLD
        IF( IBR(4,N).LE.N ) CYCLE
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
!
!---    Loop over block refined nodes  ---
!
        DO 390 NRX = IBR(4,N),IBR(5,N)
!
!---      Cell centroid  ---
!
          PXMIN = 1.D+20
          PXMAX = -1.D+20
          PYMIN = 1.D+20
          PYMAX = -1.D+20
          PZMIN = 1.D+20
          PZMAX = -1.D+20
!
!---      Loop over connections in the bottom direction  ---
!
          NPZ = NSZ(NRX)
          DO NC = 1,4
            NB = ICM(NC,1,NRX)
            IF( NB.EQ.0 ) EXIT
            DZGP(NPZ) = SQRT((XP(NRX)-XP(NB))**2 + 
     &        (YP(NRX)-YP(NB))**2 + (ZP(NRX)-ZP(NB))**2)
            VARX = SQRT((XP(NRX)-XP(NB))**2 + (YP(NRX)-YP(NB))**2)
            IF( VARX.LT.EPSL ) THEN
              GRVZ(NPZ) = GRAV
            ELSE
              GRVZ(NPZ) = GRAV*SIN(ATAN((ZP(NRX)-ZP(NB))/VARX))
            ENDIF
!
!---        Adjust surface area for angle between surface
!           normal and internodal connection  ---
!
            XVX(1) = XE(1,NRX)
            XVX(2) = XE(2,NRX)
            XVX(3) = XE(4,NRX)
            XVX(4) = XE(3,NRX)
            YVX(1) = YE(1,NRX)
            YVX(2) = YE(2,NRX)
            YVX(3) = YE(4,NRX)
            YVX(4) = YE(3,NRX)
            ZVX(1) = ZE(1,NRX)
            ZVX(2) = ZE(2,NRX)
            ZVX(3) = ZE(4,NRX)
            ZVX(4) = ZE(3,NRX)
            NP = 4
            DO NX = 1,NP
              PXMIN = MIN( XVX(NX),PXMIN )
              PXMAX = MAX( XVX(NX),PXMAX )
              PYMIN = MIN( YVX(NX),PYMIN )
              PYMAX = MAX( YVX(NX),PYMAX )
              PZMIN = MIN( ZVX(NX),PZMIN )
              PZMAX = MAX( ZVX(NX),PZMAX )
            ENDDO
!
!---        Bottom surface centroid  ---
!
            CALL PGCNTRD( NP,XVX,YVX,ZVX,XFB,YFB,ZFB )
!
!---        Magnitude of vector for surface normal  ---
!
            DSNZ = SQRT((XP(NRX)-XFB)**2 + 
     &        (YP(NRX)-YFB)**2 + (ZP(NRX)-ZFB)**2)
!
!---        Dot product of vector of surface normal and
!           internodal connection  ---
!
            DPZ = (XP(NRX)-XFB)*(XP(NRX)-XP(NB)) +
     &        (YP(NRX)-YFB)*(YP(NRX)-YP(NB)) +
     &        (ZP(NRX)-ZFB)*(ZP(NRX)-ZP(NB))
!
!---        Cosine of the angle between the surface normal  ---
!
            COSZ = DPZ/(DSNZ*DZGP(NPZ))
!
!---        Area correction  ---
!
            AFZ(NPZ) = AFZ(NPZ)/COSZ
          ENDDO
!
!---      Loop over connections in the south direction  ---
!
          NPY = NSY(NRX)
          DO NC = 1,4
            NS = ICM(NC,2,NRX)
            IF( NS.EQ.0 ) EXIT
            DYGP(NPY) = SQRT((XP(NRX)-XP(NS))**2 + 
     &        (YP(NRX)-YP(NS))**2 + (ZP(NRX)-ZP(NS))**2)
            VARX = SQRT((XP(NRX)-XP(NS))**2 + (YP(NRX)-YP(NS))**2)
            IF( VARX.LT.EPSL ) THEN
              GRVY(NPY) = GRAV
            ELSE
              GRVY(NPY) = GRAV*SIN(ATAN((ZP(NRX)-ZP(NS))/VARX))
            ENDIF
!
!---        Adjust surface area for angle between surface
!           normal and internodal connection  ---
!
            XVX(1) = XE(1,NRX)
            XVX(2) = XE(2,NRX)
            XVX(3) = XE(6,NRX)
            XVX(4) = XE(5,NRX)
            YVX(1) = YE(1,NRX)
            YVX(2) = YE(2,NRX)
            YVX(3) = YE(6,NRX)
            YVX(4) = YE(5,NRX)
            ZVX(1) = ZE(1,NRX)
            ZVX(2) = ZE(2,NRX)
            ZVX(3) = ZE(6,NRX)
            ZVX(4) = ZE(5,NRX)
            NP = 4
            DO NX = 1,NP
              PXMIN = MIN( XVX(NX),PXMIN )
              PXMAX = MAX( XVX(NX),PXMAX )
              PYMIN = MIN( YVX(NX),PYMIN )
              PYMAX = MAX( YVX(NX),PYMAX )
              PZMIN = MIN( ZVX(NX),PZMIN )
              PZMAX = MAX( ZVX(NX),PZMAX )
            ENDDO
!
!---        South surface centroid  ---
!
            CALL PGCNTRD( NP,XVX,YVX,ZVX,XFS,YFS,ZFS )
!
!---        Magnitude of vector for surface normal  ---
!
            DSNY = SQRT((XP(NRX)-XFS)**2 + 
     &        (YP(NRX)-YFS)**2 + (ZP(NRX)-ZFS)**2)
!
!---        Dot product of vector of surface normal and
!           internodal connection  ---
!
            DPY = (XP(NRX)-XFS)*(XP(NRX)-XP(NS)) +
     &        (YP(NRX)-YFS)*(YP(NRX)-YP(NS)) +
     &        (ZP(NRX)-ZFS)*(ZP(NRX)-ZP(NS))
!
!---        Cosine of the angle between the surface normal  ---
!
            NPY = NSY(NRX)
            COSY = DPY/(DSNY*DYGP(NPY))
!
!---        Area correction  ---
!
            AFY(NPY) = AFY(NPY)/COSY
          ENDDO
!
!---      Loop over connections in the west direction  ---
!
          NPX = NSX(NRX)
          DO NC = 1,4
            NW = ICM(NC,3,NRX)
            IF( NW.EQ.0 ) EXIT
            DXGP(NPX) = SQRT((XP(NRX)-XP(NW))**2 + 
     &        (YP(NRX)-YP(NW))**2 + (ZP(NRX)-ZP(NW))**2)
            VARX = SQRT((XP(NRX)-XP(NW))**2 + (YP(NRX)-YP(NW))**2)
            IF( VARX.LT.EPSL ) THEN
              GRVX(NPX) = GRAV
            ELSE
              GRVX(NPX) = GRAV*SIN(ATAN((ZP(NRX)-ZP(NW))/VARX))
            ENDIF
!
!---        Adjust surface area for angle between surface
!           normal and internodal connection  ---
!
            XVX(1) = XE(1,NRX)
            XVX(2) = XE(3,NRX)
            XVX(3) = XE(7,NRX)
            XVX(4) = XE(5,NRX)
            YVX(1) = YE(1,NRX)
            YVX(2) = YE(3,NRX)
            YVX(3) = YE(7,NRX)
            YVX(4) = YE(5,NRX)
            ZVX(1) = ZE(1,NRX)
            ZVX(2) = ZE(3,NRX)
            ZVX(3) = ZE(7,NRX)
            ZVX(4) = ZE(5,NRX)
            NP = 4
            DO NX = 1,NP
              PXMIN = MIN( XVX(NX),PXMIN )
              PXMAX = MAX( XVX(NX),PXMAX )
              PYMIN = MIN( YVX(NX),PYMIN )
              PYMAX = MAX( YVX(NX),PYMAX )
              PZMIN = MIN( ZVX(NX),PZMIN )
              PZMAX = MAX( ZVX(NX),PZMAX )
            ENDDO
!
!---        West surface centroid  ---
!
            CALL PGCNTRD( NP,XVX,YVX,ZVX,XFW,YFW,ZFW )
!
!---        Magnitude of vector for surface normal  ---
!
            DSNX = SQRT((XP(NRX)-XFW)**2 + 
     &        (YP(NRX)-YFW)**2 + (ZP(NRX)-ZFW)**2)
!
!---        Dot product of vector of surface normal and
!           internodal connection  ---
!
            DPX = (XP(NRX)-XFW)*(XP(NRX)-XP(NW)) +
     &        (YP(NRX)-YFW)*(YP(NRX)-YP(NW)) +
     &        (ZP(NRX)-ZFW)*(ZP(NRX)-ZP(NW))
!
!---        Cosine of the angle between the surface normal  ---
!
            COSX = DPX/(DSNX*DXGP(NPX))
!
!---        Area correction  ---
!
            AFX(NPX) = AFX(NPX)/COSX
          ENDDO
!
!---      Loop over connections in the east direction  ---
!
          NQX = NSX(NRX)+1
          DO NC = 1,4
            NE = ICM(NC,4,NRX)
            IF( NE.EQ.0 ) EXIT
            DXGP(NQX) = SQRT((XP(NRX)-XP(NE))**2 + 
     &        (YP(NRX)-YP(NE))**2 + (ZP(NRX)-ZP(NE))**2)
            VARX = SQRT((XP(NRX)-XP(NE))**2 + (YP(NRX)-YP(NE))**2)
            IF( VARX.LT.EPSL ) THEN
              GRVX(NQX) = GRAV
            ELSE
              GRVX(NQX) = GRAV*SIN(ATAN((ZP(NE)-ZP(NRX))/VARX))
            ENDIF
!
!---        Adjust surface area for angle between surface
!           normal and internodal connection  ---
!
            XVX(1) = XE(2,NRX)
            XVX(2) = XE(4,NRX)
            XVX(3) = XE(8,NRX)
            XVX(4) = XE(6,NRX)
            YVX(1) = YE(2,NRX)
            YVX(2) = YE(4,NRX)
            YVX(3) = YE(8,NRX)
            YVX(4) = YE(6,NRX)
            ZVX(1) = ZE(2,NRX)
            ZVX(2) = ZE(4,NRX)
            ZVX(3) = ZE(8,NRX)
            ZVX(4) = ZE(6,NRX)
            NP = 4
            DO NX = 1,NP
              PXMIN = MIN( XVX(NX),PXMIN )
              PXMAX = MAX( XVX(NX),PXMAX )
              PYMIN = MIN( YVX(NX),PYMIN )
              PYMAX = MAX( YVX(NX),PYMAX )
              PZMIN = MIN( ZVX(NX),PZMIN )
              PZMAX = MAX( ZVX(NX),PZMAX )
            ENDDO
!
!---        East surface centroid  ---
!
            CALL PGCNTRD( NP,XVX,YVX,ZVX,XFE,YFE,ZFE )
!
!---        Magnitude of vector for surface normal  ---
!
            DSNX = SQRT((XP(NRX)-XFE)**2 + 
     &        (YP(NRX)-YFE)**2 + (ZP(NRX)-ZFE)**2)
!
!---        Dot product of vector of surface normal and
!           internodal connection  ---
!
            DPX = (XP(NRX)-XFE)*(XP(NRX)-XP(NE)) +
     &        (YP(NRX)-YFE)*(YP(NRX)-YP(NE)) +
     &        (ZP(NRX)-ZFE)*(ZP(NRX)-ZP(NE))
!
!---        Cosine of the angle between the surface normal  ---
!
            COSX = DPX/(DSNX*DXGP(NQX))
!
!---        Area correction  ---
!
            AFX(NQX) = AFX(NQX)/COSX
          ENDDO
!
!---      Loop over connections in the north direction  ---
!
          NQY = NSY(NRX)+IRX
          DO NC = 1,4
            NN = ICM(NC,5,NRX)
            IF( NN.EQ.0 ) EXIT
            DYGP(NQY) = SQRT((XP(NRX)-XP(NN))**2 + 
     &        (YP(NRX)-YP(NN))**2 + (ZP(NRX)-ZP(NN))**2)
            VARX = SQRT((XP(NRX)-XP(NN))**2 + (YP(NRX)-YP(NN))**2)
            IF( VARX.LT.EPSL ) THEN
              GRVY(NQY) = GRAV
            ELSE
              GRVY(NQY) = GRAV*SIN(ATAN((ZP(NN)-ZP(NRX))/VARX))
            ENDIF
!
!---        Adjust surface area for angle between surface
!           normal and internodal connection  ---
!
            XVX(1) = XE(3,NRX)
            XVX(2) = XE(4,NRX)
            XVX(3) = XE(8,NRX)
            XVX(4) = XE(7,NRX)
            YVX(1) = YE(3,NRX)
            YVX(2) = YE(4,NRX)
            YVX(3) = YE(8,NRX)
            YVX(4) = YE(7,NRX)
            ZVX(1) = ZE(3,NRX)
            ZVX(2) = ZE(4,NRX)
            ZVX(3) = ZE(8,NRX)
            ZVX(4) = ZE(7,NRX)
            NP = 4
            DO NX = 1,NP
              PXMIN = MIN( XVX(NX),PXMIN )
              PXMAX = MAX( XVX(NX),PXMAX )
              PYMIN = MIN( YVX(NX),PYMIN )
              PYMAX = MAX( YVX(NX),PYMAX )
              PZMIN = MIN( ZVX(NX),PZMIN )
              PZMAX = MAX( ZVX(NX),PZMAX )
            ENDDO
!
!---        North surface centroid  ---
!
            CALL PGCNTRD( NP,XVX,YVX,ZVX,XFN,YFN,ZFN )
!
!---        Magnitude of vector for surface normal  ---
!
            DSNY = SQRT((XP(NRX)-XFN)**2 + 
     &        (YP(NRX)-YFN)**2 + (ZP(NRX)-ZFN)**2)
!
!---        Dot product of vector of surface normal and
!           internodal connection  ---
!
            DPY = (XP(NRX)-XFN)*(XP(NRX)-XP(NN)) +
     &        (YP(NRX)-YFN)*(YP(NRX)-YP(NN)) +
     &        (ZP(NRX)-ZFN)*(ZP(NRX)-ZP(NN))
!
!---        Cosine of the angle between the surface normal  ---
!
            COSY = DPY/(DSNY*DYGP(NQY))
!
!---        Area correction  ---
!
            AFY(NQY) = AFY(NQY)/COSY
          ENDDO
!
!---      Loop over connections in the top direction  ---
!
          NQZ = NSZ(NRX)+IRX*JRX
          DO NC = 1,4
            NT = ICM(NC,6,NRX)
            IF( NT.EQ.0 ) EXIT
            DZGP(NQZ) = SQRT((XP(NRX)-XP(NT))**2 + 
     &        (YP(NRX)-YP(NT))**2 + (ZP(NRX)-ZP(NT))**2)
            VARX = SQRT((XP(NRX)-XP(NT))**2 + (YP(NRX)-YP(NT))**2)
            IF( VARX.LT.EPSL ) THEN
              GRVZ(NQZ) = GRAV
            ELSE
              GRVZ(NQZ) = GRAV*SIN(ATAN((ZP(NT)-ZP(NRX))/VARX))
            ENDIF
!
!---        Adjust surface area for angle between surface
!           normal and internodal connection  ---
!
            XVX(1) = XE(5,NRX)
            XVX(2) = XE(6,NRX)
            XVX(3) = XE(8,NRX)
            XVX(4) = XE(7,NRX)
            YVX(1) = YE(5,NRX)
            YVX(2) = YE(6,NRX)
            YVX(3) = YE(8,NRX)
            YVX(4) = YE(7,NRX)
            ZVX(1) = ZE(5,NRX)
            ZVX(2) = ZE(6,NRX)
            ZVX(3) = ZE(8,NRX)
            ZVX(4) = ZE(7,NRX)
            NP = 4
            DO NX = 1,NP
              PXMIN = MIN( XVX(NX),PXMIN )
              PXMAX = MAX( XVX(NX),PXMAX )
              PYMIN = MIN( YVX(NX),PYMIN )
              PYMAX = MAX( YVX(NX),PYMAX )
              PZMIN = MIN( ZVX(NX),PZMIN )
              PZMAX = MAX( ZVX(NX),PZMAX )
            ENDDO
!
!---        Top surface centroid  ---
!
            CALL PGCNTRD( NP,XVX,YVX,ZVX,XFT,YFT,ZFT )
!
!---        Magnitude of vector for surface normal  ---
!
            DSNZ = SQRT((XP(NRX)-XFT)**2 + 
     &        (YP(NRX)-YFT)**2 + (ZP(NRX)-ZFT)**2)
!
!---        Dot product of vector of surface normal and
!           internodal connection  ---
!
            DPZ = (XP(NRX)-XFT)*(XP(NRX)-XP(NT)) +
     &        (YP(NRX)-YFT)*(YP(NRX)-YP(NT)) +
     &        (ZP(NRX)-ZFT)*(ZP(NRX)-ZP(NT))
!
!---        Cosine of the angle between the surface normal  ---
!
            COSZ = DPZ/(DSNZ*DZGP(NQZ))
!
!---        Area correction  ---
!
            AFZ(NQZ) = AFZ(NQZ)/COSZ
          ENDDO
  390   CONTINUE
  400 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of AVBR2 group  ---
!
      RETURN
      END
      
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE AVCART
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
!     Surface areas and volumes for Cartesian grids.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 29 November 2011.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
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
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/AVCART'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Define physical area data ---
!
      NPX = 0
      DO 130 K = 1,KFLD
        DO 120 J = 1,JFLD
          DO 110 I = 1,IFLD+1
            IX = MIN(I,IFLD)
            JX = MIN(J,JFLD)
            KX = MIN(K,KFLD)
            N = ND(IX,JX,KX)
            NPX = NPX + 1
            AFX(NPX) = DYGF(N)*DZGF(N)
  110     CONTINUE
  120   CONTINUE
  130 CONTINUE
      NPY = 0
      DO 230 K = 1,KFLD
        DO 220 J = 1,JFLD+1
          DO 210 I = 1,IFLD
            IX = MIN(I,IFLD)
            JX = MIN(J,JFLD)
            KX = MIN(K,KFLD)
            N = ND(IX,JX,KX)
            NPY = NPY+1
            AFY(NPY) = DZGF(N)*DXGF(N)
 210      CONTINUE
 220    CONTINUE
 230  CONTINUE
      NPZ = 0
      DO 330 K = 1,KFLD+1
        DO 320 J = 1,JFLD
          DO 310 I = 1,IFLD
            IX = MIN(I,IFLD)
            JX = MIN(J,JFLD)
            KX = MIN(K,KFLD)
            N = ND(IX,JX,KX)
            NPZ = NPZ + 1
            AFZ(NPZ) = DXGF(N)*DYGF(N)
 310      CONTINUE
 320    CONTINUE
 330  CONTINUE
!
!---  Define physical volume data ---
!
      DO 430 K = 1,KFLD
        DO 420 J = 1,JFLD
          DO 410 I = 1,IFLD
            IX = MIN(I,IFLD)
            JX = MIN(J,JFLD)
            KX = MIN(K,KFLD)
            N = ND(IX,JX,KX)
            NPZ = NSZ(N)
            NPY = NSY(N)
            NPX = NSX(N)
            NQX = NSX(N)+1
            IF( INBS(4,N).NE.0 ) NQX = INBS(4,N)
            NQY = NSY(N)+IFLD
            IF( INBS(5,N).NE.0 ) NQY = INBS(5,N)
            NQZ = NSZ(N)+IJFLD
            IF( INBS(6,N).NE.0 ) NQZ = INBS(6,N)
            GRVPX(N) = 5.D-1*(GRVX(NPX)+GRVX(NQX))
            GRVPY(N) = 5.D-1*(GRVY(NPY)+GRVY(NQY))
            GRVPZ(N) = 5.D-1*(GRVZ(NPZ)+GRVZ(NQZ))
            VOL(N) = DXGF(N)*DYGF(N)*DZGF(N)
!
!---        Check for zero or negative volume  ---
!
            IF( VOL(N).LE.0.D+0 ) THEN
              INDX = 7
              IMSG = N
              CHMSG = 'Non-Positive Grid-Cell Volume at Node: '
              CALL WRMSGS( INDX )
            ENDIF
 410      CONTINUE
 420    CONTINUE
 430  CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of AVCART group  ---
!
      RETURN
      END
      
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE AVCYLN
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
!     Surface areas and volumes for cylindrical grids.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 29 November 2011.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
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
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/AVCYLN'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Define physical area data ---
!
      NPX = 0
      DO 130 K = 1,KFLD
        DO 120 J = 1,JFLD
          DO 110 I = 1,IFLD+1
            IX = MIN(I,IFLD)
            JX = MIN(J,JFLD)
            KX = MIN(K,KFLD)
            N = ND(IX,JX,KX)
            NPX = NPX + 1
            IF( I.EQ.IFLD+1 ) THEN
              AFX(NPX) = XE(2,N)*DYGF(N)*DZGF(N)
            ELSE
              AFX(NPX) = XE(1,N)*DYGF(N)*DZGF(N)
            ENDIF
  110     CONTINUE
  120   CONTINUE
  130 CONTINUE
      NPY = 0
      DO 230 K = 1,KFLD
        DO 220 J = 1,JFLD+1
          DO 210 I = 1,IFLD
            IX = MIN(I,IFLD)
            JX = MIN(J,JFLD)
            KX = MIN(K,KFLD)
            N = ND(IX,JX,KX)
            NPY = NPY+1
            AFY(NPY) = DZGF(N)*DXGF(N)
 210      CONTINUE
 220    CONTINUE
 230  CONTINUE
      NPZ = 0
      DO 330 K = 1,KFLD+1
        DO 320 J = 1,JFLD
          DO 310 I = 1,IFLD
            IX = MIN(I,IFLD)
            JX = MIN(J,JFLD)
            KX = MIN(K,KFLD)
            N = ND(IX,JX,KX)
            NPZ = NPZ + 1
            IF( K.EQ.KFLD+1 ) THEN
              AFZ(NPZ) = DYGF(N)*(XE(6,N)**2-XE(5,N)**2)/2.D+0
            ELSE
              AFZ(NPZ) = DYGF(N)*(XE(2,N)**2-XE(1,N)**2)/2.D+0
            ENDIF
 310      CONTINUE
 320    CONTINUE
 330  CONTINUE
!
!---  Define physical volume data ---
!
      DO 430 K = 1,KFLD
        DO 420 J = 1,JFLD
          DO 410 I = 1,IFLD
            IX = MIN(I,IFLD)
            JX = MIN(J,JFLD)
            KX = MIN(K,KFLD)
            N = ND(IX,JX,KX)
            NPZ = NSZ(N)
            NPY = NSY(N)
            NPX = NSX(N)
            NQX = NSX(N)+1
            IF( INBS(4,N).NE.0 ) NQX = INBS(4,N)
            NQY = NSY(N)+IFLD
            IF( INBS(5,N).NE.0 ) NQY = INBS(5,N)
            NQZ = NSZ(N)+IJFLD
            IF( INBS(6,N).NE.0 ) NQZ = INBS(6,N)
            GRVPX(N) = 5.D-1*(GRVX(NPX)+GRVX(NQX))
            GRVPY(N) = 5.D-1*(GRVY(NPY)+GRVY(NQY))
            GRVPZ(N) = 5.D-1*(GRVZ(NPZ)+GRVZ(NQZ))
            VOL(N) = AFZ(NPZ)*DZGF(N)
!
!---        Check for zero or negative volume  ---
!
            IF( VOL(N).LE.0.D+0 ) THEN
              INDX = 7
              IMSG = N
              CHMSG = 'Non-Positive Grid-Cell Volume at Node: '
              CALL WRMSGS( INDX )
            ENDIF
 410      CONTINUE
 420    CONTINUE
 430  CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of AVCYLN group  ---
!
      RETURN
      END
      
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE AVHEX
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
!     Surface areas and volumes for ECLIPSE and Earthvision grids.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 30 November 2011.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GRID
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
      REAL*8 XVX(8),YVX(8),ZVX(8)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/AVHEX'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---    Compute cell centroids, differential lengths,
!       and gravitational components  ---
!
        DO 170 K = 1,KFLD
        DO 170 J = 1,JFLD
        DO 170 I = 1,IFLD
          N = ND(I,J,K)
          NB = N-IJFLD
          NS = N-IFLD
          NW = N-1
          NPX = NSX(N)
          NPY = NSY(N)
          NPZ = NSZ(N)
          NQX = NSX(N)+1
          IF( INBS(4,N).NE.0 ) NQX = INBS(4,N)
          NQY = NSY(N)+IFLD
          IF( INBS(5,N).NE.0 ) NQY = INBS(5,N)
          NQZ = NSZ(N)+IJFLD
          IF( INBS(6,N).NE.0 ) NQZ = INBS(6,N)
          I1 = I+1
          J1 = J+1
          K1 = K+1  
          VOL(N) = 0.D+0
          RP(I) = 1.D+0
!
!---      Cell centroid  ---
!
          PXMIN = 1.D+20
          PXMAX = -1.D+20
          PYMIN = 1.D+20
          PYMAX = -1.D+20
          PZMIN = 1.D+20
          PZMAX = -1.D+20
!
!---      West surface centroid  ---
!
          XVX(1) = XE(1,N)
          XVX(2) = XE(3,N)
          XVX(3) = XE(7,N)
          XVX(4) = XE(5,N)
          YVX(1) = YE(1,N)
          YVX(2) = YE(3,N)
          YVX(3) = YE(7,N)
          YVX(4) = YE(5,N)
          ZVX(1) = ZE(1,N)
          ZVX(2) = ZE(3,N)
          ZVX(3) = ZE(7,N)
          ZVX(4) = ZE(5,N)
          NP = 4
          DO 10 NX = 1,NP
            PXMIN = MIN( XVX(NX),PXMIN )
            PXMAX = MAX( XVX(NX),PXMAX )
            PYMIN = MIN( YVX(NX),PYMIN )
            PYMAX = MAX( YVX(NX),PYMAX )
            PZMIN = MIN( ZVX(NX),PZMIN )
            PZMAX = MAX( ZVX(NX),PZMAX )
   10     CONTINUE
          CALL PGCNTRD( NP,XVX,YVX,ZVX,XFW,YFW,ZFW )
!
!---      West surface area and volume contributions  ---
!
          AFX(NPX) = 0.D+0
          DO 12 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TRGAREA( XVX(ITX),YVX(ITX),ZVX(ITX),
     &        XVX(JTX),YVX(JTX),ZVX(JTX),XFW,YFW,ZFW,AFXX )
            AFX(NPX) = AFX(NPX)+AFXX
   12     CONTINUE
!
!---      East surface centroid  ---
!
          XVX(1) = XE(2,N)
          XVX(2) = XE(4,N)
          XVX(3) = XE(8,N)
          XVX(4) = XE(6,N)
          YVX(1) = YE(2,N)
          YVX(2) = YE(4,N)
          YVX(3) = YE(8,N)
          YVX(4) = YE(6,N)
          ZVX(1) = ZE(2,N)
          ZVX(2) = ZE(4,N)
          ZVX(3) = ZE(8,N)
          ZVX(4) = ZE(6,N)
          NP = 4
          DO 20 NX = 1,NP
            PXMIN = MIN( XVX(NX),PXMIN )
            PXMAX = MAX( XVX(NX),PXMAX )
            PYMIN = MIN( YVX(NX),PYMIN )
            PYMAX = MAX( YVX(NX),PYMAX )
            PZMIN = MIN( ZVX(NX),PZMIN )
            PZMAX = MAX( ZVX(NX),PZMAX )
   20     CONTINUE
          CALL PGCNTRD( NP,XVX,YVX,ZVX,XFE,YFE,ZFE )
!
!---      East surface area and volume contributions  ---
!
          AFX(NQX) = 0.D+0
          DO 22 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TRGAREA( XVX(ITX),YVX(ITX),ZVX(ITX),
     &        XVX(JTX),YVX(JTX),ZVX(JTX),XFE,YFE,ZFE,AFXX )
            AFX(NQX) = AFX(NQX)+AFXX
   22     CONTINUE
!
!---      South surface centroid  ---
!
          XVX(1) = XE(1,N)
          XVX(2) = XE(2,N)
          XVX(3) = XE(6,N)
          XVX(4) = XE(5,N)
          YVX(1) = YE(1,N)
          YVX(2) = YE(2,N)
          YVX(3) = YE(6,N)
          YVX(4) = YE(5,N)
          ZVX(1) = ZE(1,N)
          ZVX(2) = ZE(2,N)
          ZVX(3) = ZE(6,N)
          ZVX(4) = ZE(5,N)
          NP = 4
          DO 30 NX = 1,NP
            PXMIN = MIN( XVX(NX),PXMIN )
            PXMAX = MAX( XVX(NX),PXMAX )
            PYMIN = MIN( YVX(NX),PYMIN )
            PYMAX = MAX( YVX(NX),PYMAX )
            PZMIN = MIN( ZVX(NX),PZMIN )
            PZMAX = MAX( ZVX(NX),PZMAX )
   30     CONTINUE
          CALL PGCNTRD( NP,XVX,YVX,ZVX,XFS,YFS,ZFS )
!
!---      South surface area and volume contributions  ---
!
          AFY(NPY) = 0.D+0
          DO 32 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TRGAREA( XVX(ITX),YVX(ITX),ZVX(ITX),
     &        XVX(JTX),YVX(JTX),ZVX(JTX),XFS,YFS,ZFS,AFYX )
            AFY(NPY) = AFY(NPY)+AFYX
  32     CONTINUE
!
!---      North surface centroid  ---
!
          XVX(1) = XE(3,N)
          XVX(2) = XE(4,N)
          XVX(3) = XE(8,N)
          XVX(4) = XE(7,N)
          YVX(1) = YE(3,N)
          YVX(2) = YE(4,N)
          YVX(3) = YE(8,N)
          YVX(4) = YE(7,N)
          ZVX(1) = ZE(3,N)
          ZVX(2) = ZE(4,N)
          ZVX(3) = ZE(8,N)
          ZVX(4) = ZE(7,N)
          NP = 4
          DO 40 NX = 1,NP
            PXMIN = MIN( XVX(NX),PXMIN )
            PXMAX = MAX( XVX(NX),PXMAX )
            PYMIN = MIN( YVX(NX),PYMIN )
            PYMAX = MAX( YVX(NX),PYMAX )
            PZMIN = MIN( ZVX(NX),PZMIN )
            PZMAX = MAX( ZVX(NX),PZMAX )
   40     CONTINUE
          CALL PGCNTRD( NP,XVX,YVX,ZVX,XFN,YFN,ZFN )
!
!---      North surface area and volume contributions  ---
!
          AFY(NQY) = 0.D+0
          DO 42 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TRGAREA( XVX(ITX),YVX(ITX),ZVX(ITX),
     &        XVX(JTX),YVX(JTX),ZVX(JTX),XFN,YFN,ZFN,AFYX )
            AFY(NQY) = AFY(NQY)+AFYX
   42     CONTINUE
!
!---      Bottom surface centroid  ---
!
          XVX(1) = XE(1,N)
          XVX(2) = XE(2,N)
          XVX(3) = XE(4,N)
          XVX(4) = XE(3,N)
          YVX(1) = YE(1,N)
          YVX(2) = YE(2,N)
          YVX(3) = YE(4,N)
          YVX(4) = YE(3,N)
          ZVX(1) = ZE(1,N)
          ZVX(2) = ZE(2,N)
          ZVX(3) = ZE(4,N)
          ZVX(4) = ZE(3,N)
          NP = 4
          DO 50 NX = 1,NP
            PXMIN = MIN( XVX(NX),PXMIN )
            PXMAX = MAX( XVX(NX),PXMAX )
            PYMIN = MIN( YVX(NX),PYMIN )
            PYMAX = MAX( YVX(NX),PYMAX )
            PZMIN = MIN( ZVX(NX),PZMIN )
            PZMAX = MAX( ZVX(NX),PZMAX )
   50     CONTINUE
          CALL PGCNTRD( NP,XVX,YVX,ZVX,XFB,YFB,ZFB )
!
!---      Bottom surface area and volume contributions  ---
!
          AFZ(NPZ) = 0.D+0
          DO 52 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TRGAREA( XVX(ITX),YVX(ITX),ZVX(ITX),
     &        XVX(JTX),YVX(JTX),ZVX(JTX),XFB,YFB,ZFB,AFZX )
            AFZ(NPZ) = AFZ(NPZ)+AFZX
   52     CONTINUE
!
!---      Top surface centroid  ---
!
          XVX(1) = XE(5,N)
          XVX(2) = XE(6,N)
          XVX(3) = XE(8,N)
          XVX(4) = XE(7,N)
          YVX(1) = YE(5,N)
          YVX(2) = YE(6,N)
          YVX(3) = YE(8,N)
          YVX(4) = YE(7,N)
          ZVX(1) = ZE(5,N)
          ZVX(2) = ZE(6,N)
          ZVX(3) = ZE(8,N)
          ZVX(4) = ZE(7,N)
          NP = 4
          DO 60 NX = 1,NP
            PXMIN = MIN( XVX(NX),PXMIN )
            PXMAX = MAX( XVX(NX),PXMAX )
            PYMIN = MIN( YVX(NX),PYMIN )
            PYMAX = MAX( YVX(NX),PYMAX )
            PZMIN = MIN( ZVX(NX),PZMIN )
            PZMAX = MAX( ZVX(NX),PZMAX )
   60     CONTINUE
          CALL PGCNTRD( NP,XVX,YVX,ZVX,XFT,YFT,ZFT )
!
!---      Top surface area and volume contributions  ---
!
          AFZ(NQZ) = 0.D+0
          DO 62 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TRGAREA( XVX(ITX),YVX(ITX),ZVX(ITX),
     &        XVX(JTX),YVX(JTX),ZVX(JTX),XFT,YFT,ZFT,AFZX )
            AFZ(NQZ) = AFZ(NQZ)+AFZX
  62     CONTINUE
!
!---      Centroid  ---
!
          XP(N) = (XFW+XFE+XFS+XFN+XFB+XFT)/6.D+0
          YP(N) = (YFW+YFE+YFS+YFN+YFB+YFT)/6.D+0
          ZP(N) = (ZFW+ZFE+ZFS+ZFN+ZFB+ZFT)/6.D+0
          IF( XP(N).LT.PXMIN .OR. XP(N).GT.PXMAX .OR.
     &      YP(N).LT.PYMIN .OR. YP(N).GT.PYMAX .OR.
     &      ZP(N).LT.PZMIN .OR. ZP(N).GT.PZMAX ) THEN
            PRINT *,'Node Centroid Outside of Polygon Limits'
            PRINT *,'N = ',N,' I = ',ID(N),' J = ',JD(N),' K = ',KD(N)
            PRINT *,'XP = ',XP(N),' XMIN = ',PXMIN,' XMAX = ',PXMAX
            PRINT *,'YP = ',YP(N),' YMIN = ',PYMIN,' YMAX = ',PYMAX
            PRINT *,'ZP = ',ZP(N),' ZMIN = ',PZMIN,' ZMAX = ',PZMAX
          ENDIF
!
!---      West surface vertices  ---
!
          XVX(1) = XE(1,N)
          XVX(2) = XE(3,N)
          XVX(3) = XE(7,N)
          XVX(4) = XE(5,N)
          YVX(1) = YE(1,N)
          YVX(2) = YE(3,N)
          YVX(3) = YE(7,N)
          YVX(4) = YE(5,N)
          ZVX(1) = ZE(1,N)
          ZVX(2) = ZE(3,N)
          ZVX(3) = ZE(7,N)
          ZVX(4) = ZE(5,N)
!
!---      West surface volume contributions  ---
!
          DO 112 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TETRVOL( XVX(ITX),YVX(ITX),ZVX(ITX),XVX(JTX),YVX(JTX),
     &        ZVX(JTX),XFW,YFW,ZFW,XP(N),YP(N),ZP(N),VOLX )
            VOL(N) = VOL(N)+VOLX
  112     CONTINUE
!
!---      East surface vertices  ---
!
          XVX(1) = XE(2,N)
          XVX(2) = XE(4,N)
          XVX(3) = XE(8,N)
          XVX(4) = XE(6,N)
          YVX(1) = YE(2,N)
          YVX(2) = YE(4,N)
          YVX(3) = YE(8,N)
          YVX(4) = YE(6,N)
          ZVX(1) = ZE(2,N)
          ZVX(2) = ZE(4,N)
          ZVX(3) = ZE(8,N)
          ZVX(4) = ZE(6,N)
!
!---      East surface volume contributions  ---
!
          DO 122 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TETRVOL( XVX(ITX),YVX(ITX),ZVX(ITX),XVX(JTX),YVX(JTX),
     &        ZVX(JTX),XFE,YFE,ZFE,XP(N),YP(N),ZP(N),VOLX )
            VOL(N) = VOL(N)+VOLX
  122     CONTINUE
!
!---      South surface vertices  ---
!
          XVX(1) = XE(1,N)
          XVX(2) = XE(2,N)
          XVX(3) = XE(6,N)
          XVX(4) = XE(5,N)
          YVX(1) = YE(1,N)
          YVX(2) = YE(2,N)
          YVX(3) = YE(6,N)
          YVX(4) = YE(5,N)
          ZVX(1) = ZE(1,N)
          ZVX(2) = ZE(2,N)
          ZVX(3) = ZE(6,N)
          ZVX(4) = ZE(5,N)
!
!---      South surface volume contributions  ---
!
          DO 132 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TETRVOL( XVX(ITX),YVX(ITX),ZVX(ITX),XVX(JTX),YVX(JTX),
     &        ZVX(JTX),XFS,YFS,ZFS,XP(N),YP(N),ZP(N),VOLX )
            VOL(N) = VOL(N)+VOLX
 132     CONTINUE
!
!---      North surface vertices  ---
!
          XVX(1) = XE(3,N)
          XVX(2) = XE(4,N)
          XVX(3) = XE(8,N)
          XVX(4) = XE(7,N)
          YVX(1) = YE(3,N)
          YVX(2) = YE(4,N)
          YVX(3) = YE(8,N)
          YVX(4) = YE(7,N)
          ZVX(1) = ZE(3,N)
          ZVX(2) = ZE(4,N)
          ZVX(3) = ZE(8,N)
          ZVX(4) = ZE(7,N)
!
!---      North surface volume contributions  ---
!
          DO 142 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TETRVOL( XVX(ITX),YVX(ITX),ZVX(ITX),XVX(JTX),YVX(JTX),
     &        ZVX(JTX),XFN,YFN,ZFN,XP(N),YP(N),ZP(N),VOLX )
            VOL(N) = VOL(N)+VOLX
  142     CONTINUE
!
!---      Bottom surface vertices  ---
!
          XVX(1) = XE(1,N)
          XVX(2) = XE(2,N)
          XVX(3) = XE(4,N)
          XVX(4) = XE(3,N)
          YVX(1) = YE(1,N)
          YVX(2) = YE(2,N)
          YVX(3) = YE(4,N)
          YVX(4) = YE(3,N)
          ZVX(1) = ZE(1,N)
          ZVX(2) = ZE(2,N)
          ZVX(3) = ZE(4,N)
          ZVX(4) = ZE(3,N)
!
!---      Bottom surface volume contributions  ---
!
          DO 152 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TETRVOL( XVX(ITX),YVX(ITX),ZVX(ITX),XVX(JTX),YVX(JTX),
     &        ZVX(JTX),XFB,YFB,ZFB,XP(N),YP(N),ZP(N),VOLX )
            VOL(N) = VOL(N)+VOLX
  152     CONTINUE
!
!---      Top surface vertices  ---
!
          XVX(1) = XE(5,N)
          XVX(2) = XE(6,N)
          XVX(3) = XE(8,N)
          XVX(4) = XE(7,N)
          YVX(1) = YE(5,N)
          YVX(2) = YE(6,N)
          YVX(3) = YE(8,N)
          YVX(4) = YE(7,N)
          ZVX(1) = ZE(5,N)
          ZVX(2) = ZE(6,N)
          ZVX(3) = ZE(8,N)
          ZVX(4) = ZE(7,N)
!
!---      Top surface volume contributions  ---
!
          DO 162 ITX = 1,4
            JTX = ITX+1
            IF( JTX.EQ.5 ) JTX = 1
            CALL TETRVOL( XVX(ITX),YVX(ITX),ZVX(ITX),XVX(JTX),YVX(JTX),
     &        ZVX(JTX),XFT,YFT,ZFT,XP(N),YP(N),ZP(N),VOLX )
            VOL(N) = VOL(N)+VOLX
 162     CONTINUE
!
!---      Check for zero or negative volume  ---
!
          IF( VOL(N).LE.0.D+0 ) THEN
            INDX = 7
            IMSG = N
             CHMSG = 'Non-Positive Grid-Cell Volume at Node: '
            CALL WRMSGS( INDX )
          ENDIF
!
!---      West-east differentials  ---
!
          DXGF(N) = SQRT((XFE-XFW)**2 + (YFE-YFW)**2 + (ZFE-ZFW)**2)
          DXW = SQRT((XP(N)-XFW)**2 + (YP(N)-YFW)**2 + (ZP(N)-ZFW)**2)
          DXE = SQRT((XFE-XP(N))**2 + (YFE-YP(N))**2 + (ZFE-ZP(N))**2)
          IF( I.EQ.1 .OR. INBS(3,N).NE.0 ) THEN
            DXGP(NPX) = DXW
          ELSE
            DXGP(NPX) = DXGP(NPX) + DXW
          ENDIF
          IF( I.EQ.IFLD .OR. INBS(4,N).NE.0 ) THEN
            DXGP(NQX) = DXE
          ELSE
            DXGP(NQX) = DXGP(NQX) + DXE
          ENDIF
!
!---      South-north differentials  ---
!
          DYGF(N) = SQRT((XFN-XFS)**2 + (YFN-YFS)**2 + (ZFN-ZFS)**2)
          DYS = SQRT((XP(N)-XFS)**2 + (YP(N)-YFS)**2 + (ZP(N)-ZFS)**2)
          DYN = SQRT((XFN-XP(N))**2 +(YFN-YP(N))**2 +(ZFN-ZP(N))**2)
          IF( J.EQ.1 .OR. INBS(2,N).NE.0 ) THEN
            DYGP(NPY) = DYS
          ELSE
            DYGP(NPY) = DYGP(NPY) + DYS
          ENDIF
          IF( J.EQ.JFLD .OR. INBS(5,N).NE.0 ) THEN
            DYGP(NQY) = DYN
          ELSE
            DYGP(NQY) = DYGP(NQY) + DYN
          ENDIF
!
!---      Bottom-top differentials  ---
!
          DZGF(N) = SQRT((XFT-XFB)**2 + (YFT-YFB)**2 + (ZFT-ZFB)**2)
          DZB = SQRT((XP(N)-XFB)**2 + (YP(N)-YFB)**2 + (ZP(N)-ZFB)**2)
          DZT = SQRT((XFT-XP(N))**2 + (YFT-YP(N))**2 + (ZFT-ZP(N))**2)
          IF( K.EQ.1 .OR. INBS(1,N).NE.0 ) THEN
            DZGP(NPZ) = DZB
          ELSE
            DZGP(NPZ) = DZGP(NPZ) + DZB
          ENDIF
          IF( K.EQ.KFLD .OR. INBS(6,N).NE.0 ) THEN
            DZGP(NQZ) = DZT
          ELSE
            DZGP(NQZ) = DZGP(NQZ) + DZT
          ENDIF
!
!---      West-east gravity vectors and surface tilts  ---
!
          IF( I.EQ.1 .OR. INBS(3,N).NE.0 ) THEN
            VARX = SQRT((XP(N)-XFW)**2 + (YP(N)-YFW)**2)
            IF( VARX.LT.EPSL ) THEN
              GRVX(NPX) = GRAV
            ELSE
              GRVX(NPX) = GRAV*SIN(ATAN((ZP(N)-ZFW)/VARX))
            ENDIF
          ENDIF
          IF( I.EQ.IFLD .OR. INBS(4,N).NE.0 ) THEN
            VARX = SQRT((XFE-XP(N))**2 + (YFE-YP(N))**2)
            IF( VARX.LT.EPSL ) THEN
              GRVX(NQX) = GRAV
            ELSE
              GRVX(NQX) = GRAV*SIN(ATAN((ZFE-ZP(N))/VARX))
            ENDIF
          ENDIF
          IF( I.GT.1 .AND. IFLD.GT.1 .AND. INBS(3,N).EQ.0 ) THEN
            VARX = SQRT((XP(N)-XP(NW))**2 + (YP(N)-YP(NW))**2)
            IF( VARX.LT.EPSL ) THEN
              GRVX(NPX) = GRAV
            ELSE
              GRVX(NPX) = GRAV*SIN(ATAN((ZP(N)-ZP(NW))/VARX))
            ENDIF
          ENDIF
!
!---      South-north gravity vectors and surface tilts  ---
!
          IF( J.EQ.1 .OR. INBS(2,N).NE.0 ) THEN
            VARX = SQRT((XP(N)-XFS)**2 + (YP(N)-YFS)**2)
            IF( VARX.LT.EPSL ) THEN
              GRVY(NPY) = GRAV
            ELSE
              GRVY(NPY) = GRAV*SIN(ATAN((ZP(N)-ZFS)/VARX))
            ENDIF
          ENDIF
          IF( J.EQ.JFLD .OR. INBS(5,N).NE.0 ) THEN
            VARX = SQRT((XFN-XP(N))**2 + (YFN-YP(N))**2)
            IF( VARX.LT.EPSL ) THEN
              GRVY(NQY) = GRAV
            ELSE
              GRVY(NQY) = GRAV*SIN(ATAN((ZFN-ZP(N))/VARX))
            ENDIF
          ENDIF
          IF( J.GT.1 .AND. JFLD.GT.1 .AND. INBS(2,N).EQ.0 ) THEN
            VARX = SQRT((XP(N)-XP(NS))**2 + (YP(N)-YP(NS))**2)
            IF( VARX.LT.EPSL ) THEN
              GRVY(NPY) = GRAV
            ELSE
              GRVY(NPY) = GRAV*SIN(ATAN((ZP(N)-ZP(NS))/VARX))
            ENDIF
          ENDIF
!
!---      Bottom-top gravity vectors and surface tilts  ---
!
          IF( K.EQ.1 .OR. INBS(1,N).NE.0 ) THEN
            VARX = SQRT((XP(N)-XFB)**2 + (YP(N)-YFB)**2)
            IF( VARX.LT.EPSL ) THEN
              GRVZ(NPZ) = GRAV
            ELSE
              GRVZ(NPZ) = GRAV*SIN(ATAN((ZP(N)-ZFB)/VARX))
            ENDIF
          ENDIF
          IF( K.EQ.KFLD .OR. INBS(6,N).NE.0 ) THEN
            VARX = SQRT((XFT-XP(N))**2 + (YFT-YP(N))**2)
            IF( VARX.LT.EPSL ) THEN
              GRVZ(NQZ) = GRAV
            ELSE
              GRVZ(NQZ) = GRAV*SIN(ATAN((ZFT-ZP(N))/VARX))
            ENDIF
          ENDIF
          IF( K.GT.1 .AND. KFLD.GT.1 .AND. INBS(1,N).EQ.0 ) THEN
            VARX = SQRT((XP(N)-XP(NB))**2 + (YP(N)-YP(NB))**2)
            IF( VARX.LT.EPSL ) THEN
              GRVZ(NPZ) = GRAV
            ELSE
              GRVZ(NPZ) = GRAV*SIN(ATAN((ZP(N)-ZP(NB))/VARX))
            ENDIF
          ENDIF
          GRVPX(N) = 5.D-1*(GRVX(NPX)+GRVX(NQX))
          GRVPY(N) = 5.D-1*(GRVY(NPY)+GRVY(NQY))
          GRVPZ(N) = 5.D-1*(GRVZ(NPZ)+GRVZ(NQZ))
  170   CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of AVHEX group  ---
!
      RETURN
      END
      
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CONNLST
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
!     Write Tecplot connectivity list for finite element grid
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 26 January 2012.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
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
      INTEGER NC0X(2,2),NC1X(2,2),NC2X(2,2),NC3X(2,2)
!
!----------------------Data Statements---------------------------------!
!
      DATA NC0X / 1,1,1,1 /
      DATA NC1X / 1,2,2,4 /
      DATA NC2X / 1,1,2,3 /
      DATA NC3X / 1,2,1,2 /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/CONNLST'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      ISUB_LOG = ISUB_LOG-1
!
!---  Open 'connect' file for writing  ---
!
      OPEN(UNIT=26, FILE='connect', STATUS='UNKNOWN', FORM='FORMATTED')
      CLOSE(UNIT=26,STATUS='DELETE')
      OPEN(UNIT=26, FILE='connect', STATUS='NEW', FORM='FORMATTED')
!
!---  XYZ domain ( Points 1,2,3,4,5,6,7,8 )  ---
!
      IF( (IFLD.GT.1 .AND. JFLD.GT.1 .AND. KFLD.GT.1) .OR. 
     &  (ICS.EQ.3) .OR. (ISLC(63).EQ.1) ) THEN
        DO 200 N = 1,NFBN
          IF( IXP(N).EQ.0 .OR. IBR(4,N).NE.N ) GOTO 200
          IF( N.GT.NFLD ) THEN
            NF = INP(N-NFLD)
          ELSE
            NF = N
          ENDIF
!
!---      Point 1 (i,j,k)  ---
!
          IVX = 8*(N-1) + 1
!
!---      West node  ---
!
          NW = ICM(1,3,N)
          IF( NW.GT.0 ) THEN
            IF( NW.GT.NFLD ) THEN
              NFW = INP(NW-NFLD)
            ELSE
              NFW = NW
            ENDIF
            IRY = IBR(2,NFW)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFW)/(IBR(3,NF)+1) + 1
            NC = NC0X(IRY,IRZ)
            NW = ICM(NC,3,N)
            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              DVX = ABS(XE(1,N)-XE(2,NW)) +
     &           ABS(YE(1,N)-YE(2,NW)) + ABS(ZE(1,N)-ZE(2,NW))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NW-1) + 2 )
              ENDIF
            ENDIF
!
!---        West-south node  ---
!
            NWS = ICM(1,2,NW)
            IF( NWS.GT.0 ) THEN
              IF( NWS.GT.NFLD ) THEN
                NFWS = INP(NWS-NFLD)
              ELSE
                NFWS = NWS
              ENDIF
              IRX = IBR(1,NFWS)/(IBR(1,NFW)+1) + 1
              IRZ = IBR(3,NFWS)/(IBR(3,NFW)+1) + 1
              NC = NC3X(IRX,IRZ)
              NWS = ICM(NC,2,NW)
              IF( IXP(NWS).NE.0 .AND. INBS(2,NW).EQ.0 ) THEN
                DVX = ABS(XE(1,N)-XE(4,NWS)) +
     &             ABS(YE(1,N)-YE(4,NWS)) + ABS(ZE(1,N)-ZE(4,NWS))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NWS-1) + 4 )
                ENDIF
              ENDIF
!
!---          West-south-bottom node  ---
!
              NWSB = ICM(1,1,NWS)
              IF( NWSB.GT.0 ) THEN
                IF( NWSB.GT.NFLD ) THEN
                  NFWSB = INP(NWSB-NFLD)
                ELSE
                  NFWSB = NWSB
                ENDIF
                IRX = IBR(1,NFWSB)/(IBR(1,NFWS)+1) + 1
                IRY = IBR(2,NFWSB)/(IBR(2,NFWS)+1) + 1
                NC = NC1X(IRX,IRY)
                NWSB = ICM(NC,1,NWS)
                IF( IXP(NWSB).NE.0 .AND. INBS(1,NWS).EQ.0 ) THEN
                  DVX = ABS(XE(1,N)-XE(8,NWSB)) +
     &               ABS(YE(1,N)-YE(8,NWSB)) + ABS(ZE(1,N)-ZE(8,NWSB))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NWSB-1) + 8 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      South node  ---
!
          NS = ICM(1,2,N)
          IF( NS.GT.0 ) THEN
            IF( NS.GT.NFLD ) THEN
              NFS = INP(NS-NFLD)
            ELSE
              NFS = NS
            ENDIF
            IRX = IBR(1,NFS)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFS)/(IBR(3,NF)+1) + 1
            NC = NC0X(IRX,IRZ)
            NS = ICM(NC,2,N)
            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              DVX = ABS(XE(1,N)-XE(3,NS)) +
     &           ABS(YE(1,N)-YE(3,NS)) + ABS(ZE(1,N)-ZE(3,NS))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NS-1) + 3 )
              ENDIF
            ENDIF
!
!---        South-bottom node  ---
!
            NSB = ICM(1,1,NS)
            IF( NSB.GT.0 ) THEN
              IF( NSB.GT.NFLD ) THEN
                NFSB = INP(NSB-NFLD)
              ELSE
                NFSB = NSB
              ENDIF
              IRX = IBR(1,NFSB)/(IBR(1,NFS)+1) + 1
              IRY = IBR(2,NFSB)/(IBR(2,NFS)+1) + 1
              NC = NC2X(IRX,IRY)
              NSB = ICM(NC,1,NS)
              IF( IXP(NSB).NE.0 .AND. INBS(1,NS).EQ.0 ) THEN
                DVX = ABS(XE(1,N)-XE(7,NSB)) +
     &             ABS(YE(1,N)-YE(7,NSB)) + ABS(ZE(1,N)-ZE(7,NSB))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NSB-1) + 7 )
                ENDIF
              ENDIF
!
!---          South-bottom-west node  ---
!
              NSBW = ICM(1,3,NSB)
              IF( NSBW.GT.0 ) THEN
                IF( NSBW.GT.NFLD ) THEN
                  NFSBW = INP(NSBW-NFLD)
                ELSE
                  NFSBW = NSBW
                ENDIF
                IRY = IBR(2,NFSBW)/(IBR(2,NFSB)+1) + 1
                IRZ = IBR(3,NFSBW)/(IBR(3,NFSB)+1) + 1
                NC = NC1X(IRY,IRZ)
                NSBW = ICM(NC,3,NSB)
                IF( IXP(NSBW).NE.0 .AND. INBS(3,NSB).EQ.0 ) THEN
                  DVX = ABS(XE(1,N)-XE(8,NSBW)) +
     &               ABS(YE(1,N)-YE(8,NSBW)) + ABS(ZE(1,N)-ZE(8,NSBW))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NSBW-1) + 8 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      Bottom node  ---
!
          NB = ICM(1,1,N)
          IF( NB.GT.0 ) THEN
            IF( NB.GT.NFLD ) THEN
              NFB = INP(NB-NFLD)
            ELSE
              NFB = NB
            ENDIF
            IRX = IBR(1,NFB)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFB)/(IBR(2,NF)+1) + 1
            NC = NC0X(IRX,IRY)
            NB = ICM(NC,1,N)
            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              DVX = ABS(XE(1,N)-XE(5,NB)) +
     &           ABS(YE(1,N)-YE(5,NB)) + ABS(ZE(1,N)-ZE(5,NB))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NB-1) + 5 )
              ENDIF
            ENDIF
!
!---        Bottom-west node  ---
!
            NBW = ICM(1,3,NB)
            IF( NBW.GT.0 ) THEN
              IF( NBW.GT.NFLD ) THEN
                NFBW = INP(NBW-NFLD)
              ELSE
                NFBW = NBW
              ENDIF
              IRY = IBR(2,NFBW)/(IBR(2,NFB)+1) + 1
              IRZ = IBR(3,NFBW)/(IBR(3,NFB)+1) + 1
              NC = NC2X(IRY,IRZ)
              NBW = ICM(NC,3,NB)
              IF( IXP(NBW).NE.0 .AND. INBS(3,NB).EQ.0 ) THEN
                DVX = ABS(XE(1,N)-XE(6,NBW)) +
     &             ABS(YE(1,N)-YE(6,NBW)) + ABS(ZE(1,N)-ZE(6,NBW))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NBW-1) + 6 )
                ENDIF
              ENDIF
!
!---          Bottom-west-south node  ---
!
              NBWS = ICM(1,2,NBW)
              IF( NBWS.GT.0 ) THEN
                IF( NBWS.GT.NFLD ) THEN
                  NFBWS = INP(NBWS-NFLD)
                ELSE
                  NFBWS = NBWS
                ENDIF
                IRX = IBR(1,NFBWS)/(IBR(1,NFBW)+1) + 1
                IRZ = IBR(3,NFBWS)/(IBR(3,NFBW)+1) + 1
                NC = NC1X(IRX,IRZ)
                NBWS = ICM(NC,2,NBW)
                IF( IXP(NBWS).NE.0 .AND. INBS(2,NBW).EQ.0 ) THEN
                  DVX = ABS(XE(1,N)-XE(8,NBWS)) +
     &               ABS(YE(1,N)-YE(8,NBWS)) + ABS(ZE(1,N)-ZE(8,NBWS))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NBWS-1) + 8 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 2 (i+1,j,k)  ---
!
          IVX = 8*(N-1) + 2
!
!---      East node  ---
!
          NE = ICM(1,4,N)
          IF( NE.GT.0 ) THEN
            IF( NE.GT.NFLD ) THEN
              NFE = INP(NE-NFLD)
            ELSE
              NFE = NE
            ENDIF
            IRY = IBR(2,NFE)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFE)/(IBR(3,NF)+1) + 1
            NC = NC0X(IRY,IRZ)
            NE = ICM(NC,4,N)
            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DVX = ABS(XE(2,N)-XE(1,NE)) +
     &           ABS(YE(2,N)-YE(1,NE)) + ABS(ZE(2,N)-ZE(1,NE))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NE-1) + 1 )
              ENDIF
            ENDIF
!
!---        East-south node  ---
!
            NES = ICM(1,2,NE)
            IF( NES.GT.0 ) THEN
              IF( NES.GT.NFLD ) THEN
                NFES = INP(NES-NFLD)
              ELSE
                NFES = NES
              ENDIF
              IRX = IBR(1,NFES)/(IBR(1,NFE)+1) + 1
              IRZ = IBR(3,NFES)/(IBR(3,NFE)+1) + 1
              NC = NC0X(IRX,IRZ)
              NES = ICM(NC,2,NE)
              IF( IXP(NES).NE.0 .AND. INBS(2,NE).EQ.0 ) THEN
                DVX = ABS(XE(2,N)-XE(3,NES)) +
     &             ABS(YE(2,N)-YE(3,NES)) + ABS(ZE(2,N)-ZE(3,NES))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NES-1) + 3 )
                ENDIF
              ENDIF
!
!---          East-south-bottom node  ---
!
              NESB = ICM(1,1,NES)
              IF( NESB.GT.0 ) THEN
                IF( NESB.GT.NFLD ) THEN
                  NFESB = INP(NESB-NFLD)
                ELSE
                  NFESB = NESB
                ENDIF
                IRX = IBR(1,NFESB)/(IBR(1,NFES)+1) + 1
                IRY = IBR(2,NFESB)/(IBR(2,NFES)+1) + 1
                NC = NC2X(IRX,IRY)
                NESB = ICM(NC,1,NES)
                IF( IXP(NESB).NE.0 .AND. INBS(1,NES).EQ.0 ) THEN
                  DVX = ABS(XE(2,N)-XE(7,NESB)) +
     &               ABS(YE(2,N)-YE(7,NESB)) + ABS(ZE(2,N)-ZE(7,NESB))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NESB-1) + 7 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      South node  ---
!
          NS = ICM(1,2,N)
          IF( NS.GT.0 ) THEN
            IF( NS.GT.NFLD ) THEN
              NFS = INP(NS-NFLD)
            ELSE
              NFS = NS
            ENDIF
            IRX = IBR(1,NFS)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFS)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRX,IRZ)
            NS = ICM(NC,2,N)
            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              DVX = ABS(XE(2,N)-XE(4,NS)) +
     &           ABS(YE(2,N)-YE(4,NS)) + ABS(ZE(2,N)-ZE(4,NS))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NS-1) + 4 )
              ENDIF
            ENDIF
!
!---        South-bottom node  ---
!
            NSB = ICM(1,1,NS)
            IF( NSB.GT.0 ) THEN
              IF( NSB.GT.NFLD ) THEN
                NFSB = INP(NSB-NFLD)
              ELSE
                NFSB = NSB
              ENDIF
              IRX = IBR(1,NFSB)/(IBR(1,NFS)+1) + 1
              IRY = IBR(2,NFSB)/(IBR(2,NFS)+1) + 1
              NC = NC1X(IRX,IRY)
              NSB = ICM(NC,1,NS)
              IF( IXP(NSB).NE.0 .AND. INBS(1,NS).EQ.0 ) THEN
                DVX = ABS(XE(2,N)-XE(8,NSB)) +
     &             ABS(YE(2,N)-YE(8,NSB)) + ABS(ZE(2,N)-ZE(8,NSB))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NSB-1) + 8 )
                ENDIF
              ENDIF
!
!---          South-bottom-east node  ---
!
              NSBE = ICM(1,4,NSB)
              IF( NSBE.GT.0 ) THEN
                IF( NSBE.GT.NFLD ) THEN
                  NFSBE = INP(NSBE-NFLD)
                ELSE
                  NFSBE = NSBE
                ENDIF
                IRY = IBR(2,NFSBE)/(IBR(2,NFSB)+1) + 1
                IRZ = IBR(3,NFSBE)/(IBR(3,NFSB)+1) + 1
                NC = NC1X(IRY,IRZ)
                NSBE = ICM(NC,4,NSB)
                IF( IXP(NSBE).NE.0 .AND. INBS(4,NSB).EQ.0 ) THEN
                  DVX = ABS(XE(2,N)-XE(7,NSBE)) +
     &               ABS(YE(2,N)-YE(7,NSBE)) + ABS(ZE(2,N)-ZE(7,NSBE))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NSBE-1) + 7 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      Bottom node  ---
!
          NB = ICM(1,1,N)
          IF( NB.GT.0 ) THEN
            IF( NB.GT.NFLD ) THEN
              NFB = INP(NB-NFLD)
            ELSE
              NFB = NB
            ENDIF
            IRX = IBR(1,NFB)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFB)/(IBR(2,NF)+1) + 1
            NC = NC3X(IRX,IRY)
            NB = ICM(NC,1,N)
            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              DVX = ABS(XE(2,N)-XE(6,NB)) +
     &           ABS(YE(2,N)-YE(6,NB)) + ABS(ZE(2,N)-ZE(6,NB))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NB-1) + 6 )
              ENDIF
            ENDIF
!
!---        Bottom-east node  ---
!
            NBE = ICM(1,4,NB)
            IF( NBE.GT.0 ) THEN
              IF( NBE.GT.NFLD ) THEN
                NFBE = INP(NBE-NFLD)
              ELSE
                NFBE = NBE
              ENDIF
              IRY = IBR(2,NFBE)/(IBR(2,NFB)+1) + 1
              IRZ = IBR(3,NFBE)/(IBR(3,NFB)+1) + 1
              NC = NC2X(IRY,IRZ)
              NBE = ICM(NC,4,NB)
              IF( IXP(NBE).NE.0 .AND. INBS(4,NB).EQ.0 ) THEN
                DVX = ABS(XE(2,N)-XE(5,NBE)) +
     &             ABS(YE(2,N)-YE(5,NBE)) + ABS(ZE(2,N)-ZE(5,NBE))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NBE-1) + 5 )
                ENDIF
              ENDIF
!
!---          Bottom-east-south node  ---
!
              NBES = ICM(1,2,NBE)
              IF( NBES.GT.0 ) THEN
                IF( NBES.GT.NFLD ) THEN
                  NFBES = INP(NBES-NFLD)
                ELSE
                  NFBES = NBES
                ENDIF
                IRX = IBR(1,NFBES)/(IBR(1,NFBE)+1) + 1
                IRZ = IBR(3,NFBES)/(IBR(3,NFBE)+1) + 1
                NC = NC2X(IRX,IRZ)
                NBES = ICM(NC,2,NBE)
                IF( IXP(NBES).NE.0 .AND. INBS(2,NBE).EQ.0 ) THEN
                  DVX = ABS(XE(2,N)-XE(7,NBES)) +
     &               ABS(YE(2,N)-YE(7,NBES)) + ABS(ZE(2,N)-ZE(7,NBES))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NBES-1) + 7 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 3 (i+1,j+1,k)  ---
!
          IVX = 8*(N-1) + 4
!
!---      East node  ---
!
          NE = ICM(1,4,N)
          IF( NE.GT.0 ) THEN
            IF( NE.GT.NFLD ) THEN
              NFE = INP(NE-NFLD)
            ELSE
              NFE = NE
            ENDIF
            IRY = IBR(2,NFE)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFE)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRY,IRZ)
            NE = ICM(NC,4,N)
            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DVX = ABS(XE(4,N)-XE(3,NE)) +
     &           ABS(YE(4,N)-YE(3,NE)) + ABS(ZE(4,N)-ZE(3,NE))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NE-1) + 3 )
              ENDIF
            ENDIF
!
!---        East-north node  ---
!
            NEN = ICM(1,5,NE)
            IF( NEN.GT.0 ) THEN
              IF( NEN.GT.NFLD ) THEN
                NFEN = INP(NEN-NFLD)
              ELSE
                NFEN = NEN
              ENDIF
              IRX = IBR(1,NFEN)/(IBR(1,NFE)+1) + 1
              IRZ = IBR(3,NFEN)/(IBR(3,NFE)+1) + 1
              NC = NC0X(IRX,IRZ)
              NEN = ICM(NC,5,NE)
              IF( IXP(NEN).NE.0 .AND. INBS(5,NE).EQ.0 ) THEN
                DVX = ABS(XE(4,N)-XE(1,NEN)) +
     &             ABS(YE(4,N)-YE(1,NEN)) + ABS(ZE(4,N)-ZE(1,NEN))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NEN-1) + 1 )
                ENDIF
              ENDIF
!
!---          East-north-bottom node  ---
!
              NENB = ICM(1,1,NEN)
              IF( NENB.GT.0 ) THEN
                IF( NENB.GT.NFLD ) THEN
                  NFENB = INP(NENB-NFLD)
                ELSE
                  NFENB = NENB
                ENDIF
                IRX = IBR(1,NFENB)/(IBR(1,NFEN)+1) + 1
                IRY = IBR(2,NFENB)/(IBR(2,NFEN)+1) + 1
                NC = NC0X(IRX,IRY)
                NENB = ICM(NC,1,NEN)
                IF( IXP(NENB).NE.0 .AND. INBS(1,NEN).EQ.0 ) THEN
                  DVX = ABS(XE(4,N)-XE(5,NENB)) +
     &               ABS(YE(4,N)-YE(5,NENB)) + ABS(ZE(4,N)-ZE(5,NENB))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NENB-1) + 5 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      North node  ---
!
          NN = ICM(1,5,N)
          IF( NN.GT.0 ) THEN
            IF( NN.GT.NFLD ) THEN
              NFN = INP(NN-NFLD)
            ELSE
              NFN = NN
            ENDIF
            IRX = IBR(1,NFN)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFN)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRX,IRZ)
            NN = ICM(NC,5,N)
            IF( IXP(NN).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              DVX = ABS(XE(4,N)-XE(2,NN)) +
     &           ABS(YE(4,N)-YE(2,NN)) + ABS(ZE(4,N)-ZE(2,NN))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NN-1) + 2 )
              ENDIF
            ENDIF
!
!---        North-bottom node  ---
!
            NNB = ICM(1,1,NN)
            IF( NNB.GT.0 ) THEN
              IF( NNB.GT.NFLD ) THEN
                NFNB = INP(NNB-NFLD)
              ELSE
                NFNB = NNB
              ENDIF
              IRX = IBR(1,NFNB)/(IBR(1,NFN)+1) + 1
              IRY = IBR(2,NFNB)/(IBR(2,NFN)+1) + 1
              NC = NC3X(IRX,IRY)
              NNB = ICM(NC,1,NN)
              IF( IXP(NNB).NE.0 .AND. INBS(1,NN).EQ.0 ) THEN
                DVX = ABS(XE(4,N)-XE(6,NNB)) +
     &             ABS(YE(4,N)-YE(6,NNB)) + ABS(ZE(4,N)-ZE(6,NNB))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NNB-1) + 6 )
                ENDIF
              ENDIF
!
!---          North-bottom-east node  ---
!
              NNBE = ICM(1,4,NNB)
              IF( NNBE.GT.0 ) THEN
                IF( NNBE.GT.NFLD ) THEN
                  NFNBE = INP(NNBE-NFLD)
                ELSE
                  NFNBE = NNBE
                ENDIF
                IRY = IBR(2,NFNBE)/(IBR(2,NFNB)+1) + 1
                IRZ = IBR(3,NFNBE)/(IBR(3,NFNB)+1) + 1
                NC = NC2X(IRY,IRZ)
                NNBE = ICM(NC,4,NNB)
                IF( IXP(NNBE).NE.0 .AND. INBS(4,NNB).EQ.0 ) THEN
                  DVX = ABS(XE(4,N)-XE(5,NNBE)) +
     &               ABS(YE(4,N)-YE(5,NNBE)) + ABS(ZE(4,N)-ZE(5,NNBE))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NNBE-1) + 5 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      Bottom node  ---
!
          NB = ICM(1,1,N)
          IF( NB.GT.0 ) THEN
            IF( NB.GT.NFLD ) THEN
              NFB = INP(NB-NFLD)
            ELSE
              NFB = NB
            ENDIF
            IRX = IBR(1,NFB)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFB)/(IBR(2,NF)+1) + 1
            NC = NC1X(IRX,IRY)
            NB = ICM(NC,1,N)
            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              DVX = ABS(XE(4,N)-XE(8,NB)) +
     &           ABS(YE(4,N)-YE(8,NB)) + ABS(ZE(4,N)-ZE(8,NB))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NB-1) + 8 )
              ENDIF
            ENDIF
!
!---        Bottom-east node  ---
!
            NBE = ICM(1,4,NB)
            IF( NBE.GT.0 ) THEN
              IF( NBE.GT.NFLD ) THEN
                NFBE = INP(NBE-NFLD)
              ELSE
                NFBE = NBE
              ENDIF
              IRY = IBR(2,NFBE)/(IBR(2,NFB)+1) + 1
              IRZ = IBR(3,NFBE)/(IBR(3,NFB)+1) + 1
              NC = NC1X(IRY,IRZ)
              NBE = ICM(NC,4,NB)
              IF( IXP(NBE).NE.0 .AND. INBS(4,NB).EQ.0 ) THEN
                DVX = ABS(XE(4,N)-XE(7,NBE)) +
     &             ABS(YE(4,N)-YE(7,NBE)) + ABS(ZE(4,N)-ZE(7,NBE))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NBE-1) + 7 )
                ENDIF
              ENDIF
!
!---          Bottom-east-north node  ---
!
              NBEN = ICM(1,5,NBE)
              IF( NBEN.GT.0 ) THEN
                IF( NBEN.GT.NFLD ) THEN
                  NFBEN = INP(NBEN-NFLD)
                ELSE
                  NFBEN = NBEN
                ENDIF
                IRX = IBR(1,NFBEN)/(IBR(1,NFBE)+1) + 1
                IRZ = IBR(3,NFBEN)/(IBR(3,NFBE)+1) + 1
                NC = NC2X(IRX,IRZ)
                NBEN = ICM(NC,5,NBE)
                IF( IXP(NBEN).NE.0 .AND. INBS(2,NBE).EQ.0 ) THEN
                  DVX = ABS(XE(4,N)-XE(5,NBEN)) +
     &               ABS(YE(4,N)-YE(5,NBEN)) + ABS(ZE(4,N)-ZE(5,NBEN))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NBEN-1) + 5 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 4 (i,j+1,k)  ---
!
          IVX = 8*(N-1) + 3
!
!---      West node  ---
!
          NW = ICM(1,3,N)
          IF( NW.GT.0 ) THEN
            IF( NW.GT.NFLD ) THEN
              NFW = INP(NW-NFLD)
            ELSE
              NFW = NW
            ENDIF
            IRY = IBR(2,NFW)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFW)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRY,IRZ)
            NW = ICM(NC,3,N)
            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              DVX = ABS(XE(3,N)-XE(4,NW)) +
     &           ABS(YE(3,N)-YE(4,NW)) + ABS(ZE(3,N)-ZE(4,NW))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NW-1) + 4 )
              ENDIF
            ENDIF
!
!---        West-north node  ---
!
            NWN = ICM(1,5,NW)
            IF( NWN.GT.0 ) THEN
              IF( NWN.GT.NFLD ) THEN
                NFWN = INP(NWN-NFLD)
              ELSE
                NFWN = NWN
              ENDIF
              IRX = IBR(1,NFWN)/(IBR(1,NFW)+1) + 1
              IRZ = IBR(3,NFWN)/(IBR(3,NFW)+1) + 1
              NC = NC3X(IRX,IRZ)
              NWN = ICM(NC,5,NW)
              IF( IXP(NWN).NE.0 .AND. INBS(4,NW).EQ.0 ) THEN
                DVX = ABS(XE(3,N)-XE(2,NWN)) +
     &             ABS(YE(3,N)-YE(2,NWN)) + ABS(ZE(3,N)-ZE(2,NWN))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NWN-1) + 2 )
                ENDIF
              ENDIF
!
!---          West-north-bottom node  ---
!
              NWNB = ICM(1,1,NWN)
              IF( NWNB.GT.0 ) THEN
                IF( NWNB.GT.NFLD ) THEN
                  NFWNB = INP(NWNB-NFLD)
                ELSE
                  NFWNB = NWNB
                ENDIF
                IRX = IBR(1,NFWNB)/(IBR(1,NFWN)+1) + 1
                IRY = IBR(2,NFWNB)/(IBR(2,NFWN)+1) + 1
                NC = NC3X(IRX,IRY)
                NWNB = ICM(NC,1,NWN)
                IF( IXP(NWNB).NE.0 .AND. INBS(1,NWN).EQ.0 ) THEN
                  DVX = ABS(XE(3,N)-XE(6,NWNB)) +
     &               ABS(YE(3,N)-YE(6,NWNB)) + ABS(ZE(3,N)-ZE(6,NWNB))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NWNB-1) + 6 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      North node  ---
!
          NN = ICM(1,5,N)
          IF( NN.GT.0 ) THEN
            IF( NN.GT.NFLD ) THEN
              NFN = INP(NN-NFLD)
            ELSE
              NFN = NN
            ENDIF
            IRX = IBR(1,NFN)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFN)/(IBR(3,NF)+1) + 1
            NC = NC0X(IRX,IRZ)
            NN = ICM(NC,5,N)
            IF( IXP(NN).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DVX = ABS(XE(3,N)-XE(1,NN)) +
     &           ABS(YE(3,N)-YE(1,NN)) + ABS(ZE(3,N)-ZE(1,NN))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NN-1) + 1 )
              ENDIF
            ENDIF
!
!---        North-bottom node  ---
!
            NNB = ICM(1,1,NN)
            IF( NNB.GT.0 ) THEN
              IF( NNB.GT.NFLD ) THEN
                NFNB = INP(NNB-NFLD)
              ELSE
                NFNB = NNB
              ENDIF
              IRX = IBR(1,NFNB)/(IBR(1,NFN)+1) + 1
              IRY = IBR(2,NFNB)/(IBR(2,NFN)+1) + 1
              NC = NC0X(IRX,IRY)
              NNB = ICM(NC,1,NN)
              IF( IXP(NNB).NE.0 .AND. INBS(1,NN).EQ.0 ) THEN
                DVX = ABS(XE(3,N)-XE(5,NNB)) +
     &             ABS(YE(3,N)-YE(5,NNB)) + ABS(ZE(3,N)-ZE(5,NNB))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NNB-1) + 5 )
                ENDIF
              ENDIF
!
!---          North-bottom-west node  ---
!
              NNBW = ICM(1,3,NNB)
              IF( NNBW.GT.0 ) THEN
                IF( NNBW.GT.NFLD ) THEN
                  NFNBW = INP(NNBW-NFLD)
                ELSE
                  NFNBW = NNBW
                ENDIF
                IRY = IBR(2,NFNBW)/(IBR(2,NFNB)+1) + 1
                IRZ = IBR(3,NFNBW)/(IBR(3,NFNB)+1) + 1
                NC = NC2X(IRY,IRZ)
                NNBW = ICM(NC,3,NNB)
                IF( IXP(NNBW).NE.0 .AND. INBS(3,NNB).EQ.0 ) THEN
                  DVX = ABS(XE(3,N)-XE(6,NNBW)) +
     &               ABS(YE(3,N)-YE(6,NNBW)) + ABS(ZE(3,N)-ZE(6,NNBW))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NNBW-1) + 6 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      Bottom node  ---
!
          NB = ICM(1,1,N)
          IF( NB.GT.0 ) THEN
            IF( NB.GT.NFLD ) THEN
              NFB = INP(NB-NFLD)
            ELSE
              NFB = NB
            ENDIF
            IRX = IBR(1,NFB)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFB)/(IBR(2,NF)+1) + 1
            NC = NC2X(IRX,IRY)
            NB = ICM(NC,1,N)
            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              DVX = ABS(XE(3,N)-XE(7,NB)) +
     &           ABS(YE(3,N)-YE(7,NB)) + ABS(ZE(3,N)-ZE(7,NB))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NB-1) + 7 )
              ENDIF
            ENDIF
!
!---        Bottom-west node  ---
!
            NBW = ICM(1,3,NB)
            IF( NBW.GT.0 ) THEN
              IF( NBW.GT.NFLD ) THEN
                NFBW = INP(NBW-NFLD)
              ELSE
                NFBW = NBW
              ENDIF
              IRY = IBR(2,NFBW)/(IBR(2,NFB)+1) + 1
              IRZ = IBR(3,NFBW)/(IBR(3,NFB)+1) + 1
              NC = NC1X(IRY,IRZ)
              NBW = ICM(NC,3,NB)
              IF( IXP(NBW).NE.0 .AND. INBS(3,NB).EQ.0 ) THEN
                DVX = ABS(XE(3,N)-XE(8,NBW)) +
     &             ABS(YE(3,N)-YE(8,NBW)) + ABS(ZE(3,N)-ZE(8,NBW))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NBW-1) + 8 )
                ENDIF
              ENDIF
!
!---          Bottom-west-north node  ---
!
              NBWN = ICM(1,5,NBW)
              IF( NBWN.GT.0 ) THEN
                IF( NBWN.GT.NFLD ) THEN
                  NFBWN = INP(NBWN-NFLD)
                ELSE
                  NFBWN = NBWN
                ENDIF
                IRX = IBR(1,NFBWN)/(IBR(1,NFBW)+1) + 1
                IRZ = IBR(3,NFBWN)/(IBR(3,NFBW)+1) + 1
                NC = NC1X(IRX,IRZ)
                NBWN = ICM(NC,5,NBW)
                IF( IXP(NBWN).NE.0 .AND. INBS(4,NBW).EQ.0 ) THEN
                  DVX = ABS(XE(3,N)-XE(6,NBWN)) +
     &               ABS(YE(3,N)-YE(6,NBWN)) + ABS(ZE(3,N)-ZE(6,NBWN))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NBWN-1) + 6 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 5 (i,j,k+1)  ---
!
          IVX = 8*(N-1) + 5
!
!---      West node  ---
!
          NW = ICM(1,3,N)
          IF( NW.GT.0 ) THEN
            IF( NW.GT.NFLD ) THEN
              NFW = INP(NW-NFLD)
            ELSE
              NFW = NW
            ENDIF
            IRY = IBR(2,NFW)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFW)/(IBR(3,NF)+1) + 1
            NC = NC2X(IRY,IRZ)
            NW = ICM(NC,3,N)
            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              DVX = ABS(XE(5,N)-XE(6,NW)) +
     &           ABS(YE(5,N)-YE(6,NW)) + ABS(ZE(5,N)-ZE(6,NW))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NW-1) + 6 )
              ENDIF
            ENDIF
!
!---        West-south node  ---
!
            NWS = ICM(1,2,NW)
            IF( NWS.GT.0 ) THEN
              IF( NWS.GT.NFLD ) THEN
                NFWS = INP(NWS-NFLD)
              ELSE
                NFWS = NWS
              ENDIF
              IRX = IBR(1,NFWS)/(IBR(1,NFW)+1) + 1
              IRZ = IBR(3,NFWS)/(IBR(3,NFW)+1) + 1
              NC = NC1X(IRX,IRZ)
              NWS = ICM(NC,2,NW)
              IF( IXP(NWS).NE.0 .AND. INBS(2,NW).EQ.0 ) THEN
                DVX = ABS(XE(5,N)-XE(8,NWS)) +
     &             ABS(YE(5,N)-YE(8,NWS)) + ABS(ZE(5,N)-ZE(8,NWS))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NWS-1) + 8 )
                ENDIF
              ENDIF
!
!---          West-south-top node  ---
!
              NWST = ICM(1,6,NWS)
              IF( NWST.GT.0 ) THEN
                IF( NWST.GT.NFLD ) THEN
                  NFWST = INP(NWST-NFLD)
                ELSE
                  NFWST = NWST
                ENDIF
                IRX = IBR(1,NFWST)/(IBR(1,NFWS)+1) + 1
                IRY = IBR(2,NFWST)/(IBR(2,NFWS)+1) + 1
                NC = NC1X(IRX,IRY)
                NWST = ICM(NC,6,NWS)
                IF( IXP(NWST).NE.0 .AND. INBS(6,NWS).EQ.0 ) THEN
                  DVX = ABS(XE(5,N)-XE(4,NWST)) +
     &               ABS(YE(5,N)-YE(4,NWST)) + ABS(ZE(5,N)-ZE(4,NWST))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NWST-1) + 4 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      South node  ---
!
          NS = ICM(1,2,N)
          IF( NS.GT.0 ) THEN
            IF( NS.GT.NFLD ) THEN
              NFS = INP(NS-NFLD)
            ELSE
              NFS = NS
            ENDIF
            IRX = IBR(1,NFS)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFS)/(IBR(3,NF)+1) + 1
            NC = NC2X(IRX,IRZ)
            NS = ICM(NC,2,N)
            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              DVX = ABS(XE(5,N)-XE(7,NS)) +
     &           ABS(YE(5,N)-YE(7,NS)) + ABS(ZE(5,N)-ZE(7,NS))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NS-1) + 7 )
              ENDIF
            ENDIF
!
!---        South-top node  ---
!
            NST = ICM(1,6,NS)
            IF( NST.GT.0 ) THEN
              IF( NST.GT.NFLD ) THEN
                NFST = INP(NST-NFLD)
              ELSE
                NFST = NST
              ENDIF
              IRX = IBR(1,NFST)/(IBR(1,NFS)+1) + 1
              IRY = IBR(2,NFST)/(IBR(2,NFS)+1) + 1
              NC = NC3X(IRX,IRY)
              NST = ICM(NC,6,NS)
              IF( IXP(NST).NE.0 .AND. INBS(6,NS).EQ.0 ) THEN
                DVX = ABS(XE(5,N)-XE(3,NST)) +
     &             ABS(YE(5,N)-YE(3,NST)) + ABS(ZE(5,N)-ZE(3,NST))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NST-1) + 3 )
                ENDIF
              ENDIF
!
!---          South-top-west node  ---
!
              NSTW = ICM(1,3,NST)
              IF( NSTW.GT.0 ) THEN
                IF( NSTW.GT.NFLD ) THEN
                  NFSTW = INP(NSTW-NFLD)
                ELSE
                  NFSTW = NSTW
                ENDIF
                IRY = IBR(2,NFSTW)/(IBR(2,NFST)+1) + 1
                IRZ = IBR(3,NFSTW)/(IBR(3,NFST)+1) + 1
                NC = NC3X(IRY,IRZ)
                NSTW = ICM(NC,3,NST)
                IF( IXP(NSTW).NE.0 .AND. INBS(3,NST).EQ.0 ) THEN
                  DVX = ABS(XE(5,N)-XE(4,NSTW)) +
     &               ABS(YE(5,N)-YE(4,NSTW)) + ABS(ZE(5,N)-ZE(4,NSTW))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NSTW-1) + 4 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      Top node  ---
!
          NT = ICM(1,6,N)
          IF( NT.GT.0 ) THEN
            IF( NT.GT.NFLD ) THEN
              NFT = INP(NT-NFLD)
            ELSE
              NFT = NT
            ENDIF
            IRX = IBR(1,NFT)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFT)/(IBR(2,NF)+1) + 1
            NC = NC0X(IRX,IRY)
            NT = ICM(NC,6,N)
            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              DVX = ABS(XE(5,N)-XE(1,NT)) +
     &           ABS(YE(5,N)-YE(1,NT)) + ABS(ZE(5,N)-ZE(1,NT))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NT-1) + 1 )
              ENDIF
            ENDIF
!
!---        Top-west node  ---
!
            NTW = ICM(1,3,NT)
            IF( NTW.GT.0 ) THEN
              IF( NTW.GT.NFLD ) THEN
                NFTW = INP(NTW-NFLD)
              ELSE
                NFTW = NTW
              ENDIF
              IRY = IBR(2,NFTW)/(IBR(2,NFT)+1) + 1
              IRZ = IBR(3,NFTW)/(IBR(3,NFT)+1) + 1
              NC = NC0X(IRY,IRZ)
              NTW = ICM(NC,3,NT)
              IF( IXP(NTW).NE.0 .AND. INBS(3,NT).EQ.0 ) THEN
                DVX = ABS(XE(5,N)-XE(2,NTW)) +
     &             ABS(YE(5,N)-YE(2,NTW)) + ABS(ZE(5,N)-ZE(2,NTW))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NTW-1) + 2 )
                ENDIF
              ENDIF
!
!---          Top-west-south node  ---
!
              NTWS = ICM(1,2,NTW)
              IF( NTWS.GT.0 ) THEN
                IF( NTWS.GT.NFLD ) THEN
                  NFTWS = INP(NTWS-NFLD)
                ELSE
                  NFTWS = NTWS
                ENDIF
                IRX = IBR(1,NFTWS)/(IBR(1,NFTW)+1) + 1
                IRZ = IBR(3,NFTWS)/(IBR(3,NFTW)+1) + 1
                NC = NC3X(IRX,IRZ)
                NTWS = ICM(NC,2,NTW)
                IF( IXP(NTWS).NE.0 .AND. INBS(2,NTW).EQ.0 ) THEN
                  DVX = ABS(XE(5,N)-XE(4,NTWS)) +
     &               ABS(YE(5,N)-YE(4,NTWS)) + ABS(ZE(5,N)-ZE(4,NTWS))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NTWS-1) + 4 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 6 (i+1,j,k+1)  ---
!
          IVX = 8*(N-1) + 6
!
!---      East node  ---
!
          NE = ICM(1,4,N)
          IF( NE.GT.0 ) THEN
            IF( NE.GT.NFLD ) THEN
              NFE = INP(NE-NFLD)
            ELSE
              NFE = NE
            ENDIF
            IRY = IBR(2,NFE)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFE)/(IBR(3,NF)+1) + 1
            NC = NC2X(IRY,IRZ)
            NE = ICM(NC,4,N)
            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DVX = ABS(XE(6,N)-XE(5,NE)) +
     &           ABS(YE(6,N)-YE(5,NE)) + ABS(ZE(6,N)-ZE(5,NE))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NE-1) + 5 )
              ENDIF
            ENDIF
!
!---        East-south node  ---
!
            NES = ICM(1,2,NE)
            IF( NES.GT.0 ) THEN
              IF( NES.GT.NFLD ) THEN
                NFES = INP(NES-NFLD)
              ELSE
                NFES = NES
              ENDIF
              IRX = IBR(1,NFES)/(IBR(1,NFE)+1) + 1
              IRZ = IBR(3,NFES)/(IBR(3,NFE)+1) + 1
              NC = NC2X(IRX,IRZ)
              NES = ICM(NC,2,NE)
              IF( IXP(NES).NE.0 .AND. INBS(2,NE).EQ.0 ) THEN
                DVX = ABS(XE(6,N)-XE(7,NES)) +
     &             ABS(YE(6,N)-YE(7,NES)) + ABS(ZE(6,N)-ZE(7,NES))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NES-1) + 7 )
                ENDIF
              ENDIF
!
!---          East-south-top node  ---
!
              NEST = ICM(1,6,NES)
              IF( NEST.GT.0 ) THEN
                IF( NEST.GT.NFLD ) THEN
                  NFEST = INP(NEST-NFLD)
                ELSE
                  NFEST = NEST
                ENDIF
                IRX = IBR(1,NFEST)/(IBR(1,NFES)+1) + 1
                IRY = IBR(2,NFEST)/(IBR(2,NFES)+1) + 1
                NC = NC2X(IRX,IRY)
                NEST = ICM(NC,6,NES)
                IF( IXP(NEST).NE.0 .AND. INBS(6,NES).EQ.0 ) THEN
                  DVX = ABS(XE(6,N)-XE(3,NEST)) +
     &               ABS(YE(6,N)-YE(3,NEST)) + ABS(ZE(6,N)-ZE(3,NEST))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NEST-1) + 3 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      South node  ---
!
          NS = ICM(1,2,N)
          IF( NS.GT.0 ) THEN
            IF( NS.GT.NFLD ) THEN
              NFS = INP(NS-NFLD)
            ELSE
              NFS = NS
            ENDIF
            IRX = IBR(1,NFS)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFS)/(IBR(3,NF)+1) + 1
            NC = NC1X(IRX,IRZ)
            NS = ICM(NC,2,N)
            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              DVX = ABS(XE(6,N)-XE(8,NS)) +
     &           ABS(YE(6,N)-YE(8,NS)) + ABS(ZE(6,N)-ZE(8,NS))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NS-1) + 8 )
              ENDIF
            ENDIF
!
!---        South-top node  ---
!
            NST = ICM(1,6,NS)
            IF( NST.GT.0 ) THEN
              IF( NST.GT.NFLD ) THEN
                NFST = INP(NST-NFLD)
              ELSE
                NFST = NST
              ENDIF
              IRX = IBR(1,NFST)/(IBR(1,NFS)+1) + 1
              IRY = IBR(2,NFST)/(IBR(2,NFS)+1) + 1
              NC = NC1X(IRX,IRY)
              NST = ICM(NC,6,NS)
              IF( IXP(NST).NE.0 .AND. INBS(6,NS).EQ.0 ) THEN
                DVX = ABS(XE(6,N)-XE(4,NST)) +
     &             ABS(YE(6,N)-YE(4,NST)) + ABS(ZE(6,N)-ZE(4,NST))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NST-1) + 4 )
                ENDIF
              ENDIF
!
!---          South-top-east node  ---
!
              NSTE = ICM(1,4,NST)
              IF( NSTE.GT.0 ) THEN
                IF( NSTE.GT.NFLD ) THEN
                  NFSTE = INP(NSTE-NFLD)
                ELSE
                  NFSTE = NSTE
                ENDIF
                IRY = IBR(2,NFSTE)/(IBR(2,NFST)+1) + 1
                IRZ = IBR(3,NFSTE)/(IBR(3,NFST)+1) + 1
                NC = NC2X(IRY,IRZ)
                NSTE = ICM(NC,4,NST)
                IF( IXP(NSTE).NE.0 .AND. INBS(4,NST).EQ.0 ) THEN
                  DVX = ABS(XE(6,N)-XE(3,NSTE)) +
     &               ABS(YE(6,N)-YE(3,NSTE)) + ABS(ZE(6,N)-ZE(3,NSTE))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NSTE-1) + 3 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      Top node  ---
!
          NT = ICM(1,6,N)
          IF( NT.GT.0 ) THEN
            IF( NT.GT.NFLD ) THEN
              NFT = INP(NT-NFLD)
            ELSE
              NFT = NT
            ENDIF
            IRX = IBR(1,NFT)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFT)/(IBR(2,NF)+1) + 1
            NC = NC3X(IRX,IRY)
            NT = ICM(NC,6,N)
            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              DVX = ABS(XE(6,N)-XE(2,NT)) +
     &           ABS(YE(6,N)-YE(2,NT)) + ABS(ZE(6,N)-ZE(2,NT))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NT-1) + 2 )
              ENDIF
            ENDIF
!
!---        Top-east node  ---
!
            NTE = ICM(1,4,NT)
            IF( NTE.GT.0 ) THEN
              IF( NTE.GT.NFLD ) THEN
                NFTE = INP(NTE-NFLD)
              ELSE
                NFTE = NTE
              ENDIF
              IRY = IBR(2,NFTE)/(IBR(2,NFT)+1) + 1
              IRZ = IBR(3,NFTE)/(IBR(3,NFT)+1) + 1
              NC = NC0X(IRY,IRZ)
              NTE = ICM(NC,4,NT)
              IF( IXP(NTE).NE.0 .AND. INBS(4,NT).EQ.0 ) THEN
                DVX = ABS(XE(6,N)-XE(1,NTE)) +
     &             ABS(YE(6,N)-YE(1,NTE)) + ABS(ZE(6,N)-ZE(1,NTE))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NTE-1) + 1 )
                ENDIF
              ENDIF
!
!---          Top-east-south node  ---
!
              NTES = ICM(1,2,NTE)
              IF( NTES.GT.0 ) THEN
                IF( NTES.GT.NFLD ) THEN
                  NFTES = INP(NTES-NFLD)
                ELSE
                  NFTES = NTES
                ENDIF
                IRX = IBR(1,NFTES)/(IBR(1,NFTE)+1) + 1
                IRZ = IBR(3,NFTES)/(IBR(3,NFTE)+1) + 1
                NC = NC0X(IRX,IRZ)
                NTES = ICM(NC,2,NTE)
                IF( IXP(NTES).NE.0 .AND. INBS(2,NTE).EQ.0 ) THEN
                  DVX = ABS(XE(6,N)-XE(3,NTES)) +
     &               ABS(YE(6,N)-YE(3,NTES)) + ABS(ZE(6,N)-ZE(3,NTES))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NTES-1) + 3 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 7 (i+1,j+1,k+1)  ---
!
          IVX = 8*(N-1) + 8
!
!---      East node  ---
!
          NE = ICM(1,4,N)
          IF( NE.GT.0 ) THEN
            IF( NE.GT.NFLD ) THEN
              NFE = INP(NE-NFLD)
            ELSE
              NFE = NE
            ENDIF
            IRY = IBR(2,NFE)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFE)/(IBR(3,NF)+1) + 1
            NC = NC1X(IRY,IRZ)
            NE = ICM(NC,4,N)
            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DVX = ABS(XE(8,N)-XE(7,NE)) +
     &           ABS(YE(8,N)-YE(7,NE)) + ABS(ZE(8,N)-ZE(7,NE))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NE-1) + 7 )
              ENDIF
            ENDIF
!
!---        East-north node  ---
!
            NEN = ICM(1,5,NE)
            IF( NEN.GT.0 ) THEN
              IF( NEN.GT.NFLD ) THEN
                NFEN = INP(NEN-NFLD)
              ELSE
                NFEN = NEN
              ENDIF
              IRX = IBR(1,NFEN)/(IBR(1,NFE)+1) + 1
              IRZ = IBR(3,NFEN)/(IBR(3,NFE)+1) + 1
              NC = NC2X(IRX,IRZ)
              NEN = ICM(NC,5,NE)
              IF( IXP(NEN).NE.0 .AND. INBS(5,NE).EQ.0 ) THEN
                DVX = ABS(XE(8,N)-XE(5,NEN)) +
     &             ABS(YE(8,N)-YE(5,NEN)) + ABS(ZE(8,N)-ZE(5,NEN))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NEN-1) + 5 )
                ENDIF
              ENDIF
!
!---          East-north-top node  ---
!
              NENT = ICM(1,6,NEN)
              IF( NENT.GT.0 ) THEN
                IF( NENT.GT.NFLD ) THEN
                  NFENT = INP(NENT-NFLD)
                ELSE
                  NFENT = NENT
                ENDIF
                IRX = IBR(1,NFENT)/(IBR(1,NFEN)+1) + 1
                IRY = IBR(2,NFENT)/(IBR(2,NFEN)+1) + 1
                NC = NC0X(IRX,IRY)
                NENT = ICM(NC,6,NEN)
                IF( IXP(NENT).NE.0 .AND. INBS(6,NEN).EQ.0 ) THEN
                  DVX = ABS(XE(8,N)-XE(1,NENT)) +
     &               ABS(YE(8,N)-YE(1,NENT)) + ABS(ZE(8,N)-ZE(1,NENT))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NENT-1) + 1 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      North node  ---
!
          NN = ICM(1,5,N)
          IF( NN.GT.0 ) THEN
            IF( NN.GT.NFLD ) THEN
              NFN = INP(NN-NFLD)
            ELSE
              NFN = NN
            ENDIF
            IRX = IBR(1,NFN)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFN)/(IBR(3,NF)+1) + 1
            NC = NC1X(IRX,IRZ)
            NN = ICM(NC,5,N)
            IF( IXP(NN).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              DVX = ABS(XE(8,N)-XE(6,NN)) +
     &           ABS(YE(8,N)-YE(6,NN)) + ABS(ZE(8,N)-ZE(6,NN))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NN-1) + 6 )
              ENDIF
            ENDIF
!
!---        North-top node  ---
!
            NNT = ICM(1,6,NN)
            IF( NNT.GT.0 ) THEN
              IF( NNT.GT.NFLD ) THEN
                NFNT = INP(NNT-NFLD)
              ELSE
                NFNT = NNT
              ENDIF
              IRX = IBR(1,NFNT)/(IBR(1,NFN)+1) + 1
              IRY = IBR(2,NFNT)/(IBR(2,NFN)+1) + 1
              NC = NC3X(IRX,IRY)
              NNT = ICM(NC,6,NN)
              IF( IXP(NNT).NE.0 .AND. INBS(6,NN).EQ.0 ) THEN
                DVX = ABS(XE(8,N)-XE(2,NNT)) +
     &             ABS(YE(8,N)-YE(2,NNT)) + ABS(ZE(8,N)-ZE(2,NNT))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NNT-1) + 2 )
                ENDIF
              ENDIF
!
!---          North-top-east node  ---
!
              NNTE = ICM(1,4,NNT)
              IF( NNTE.GT.0 ) THEN
                IF( NNTE.GT.NFLD ) THEN
                  NFNTE = INP(NNTE-NFLD)
                ELSE
                  NFNTE = NNTE
                ENDIF
                IRY = IBR(2,NFNTE)/(IBR(2,NFNT)+1) + 1
                IRZ = IBR(3,NFNTE)/(IBR(3,NFNT)+1) + 1
                NC = NC0X(IRY,IRZ)
                NNTE = ICM(NC,4,NNT)
                IF( IXP(NNTE).NE.0 .AND. INBS(4,NNT).EQ.0 ) THEN
                  DVX = ABS(XE(8,N)-XE(1,NNTE)) +
     &               ABS(YE(8,N)-YE(1,NNTE)) + ABS(ZE(8,N)-ZE(1,NNTE))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NNTE-1) + 1 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      Top node  ---
!
          NT = ICM(1,6,N)
          IF( NT.GT.0 ) THEN
            IF( NT.GT.NFLD ) THEN
              NFT = INP(NT-NFLD)
            ELSE
              NFT = NT
            ENDIF
            IRX = IBR(1,NFT)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFT)/(IBR(2,NF)+1) + 1
            NC = NC1X(IRX,IRY)
            NT = ICM(NC,6,N)
            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              DVX = ABS(XE(8,N)-XE(4,NT)) +
     &           ABS(YE(8,N)-YE(4,NT)) + ABS(ZE(8,N)-ZE(4,NT))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NT-1) + 4 )
              ENDIF
            ENDIF
!
!---        Top-east node  ---
!
            NTE = ICM(1,4,NT)
            IF( NTE.GT.0 ) THEN
              IF( NTE.GT.NFLD ) THEN
                NFTE = INP(NTE-NFLD)
              ELSE
                NFTE = NTE
              ENDIF
              IRY = IBR(2,NFTE)/(IBR(2,NFT)+1) + 1
              IRZ = IBR(3,NFTE)/(IBR(3,NFT)+1) + 1
              NC = NC3X(IRY,IRZ)
              NTE = ICM(NC,4,NT)
              IF( IXP(NTE).NE.0 .AND. INBS(4,NT).EQ.0 ) THEN
                DVX = ABS(XE(8,N)-XE(3,NTE)) +
     &             ABS(YE(8,N)-YE(3,NTE)) + ABS(ZE(8,N)-ZE(3,NTE))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NTE-1) + 3 )
                ENDIF
              ENDIF
!
!---          Top-east-north node  ---
!
              NTEN = ICM(1,5,NTE)
              IF( NTEN.GT.0 ) THEN
                IF( NTEN.GT.NFLD ) THEN
                  NFTEN = INP(NTEN-NFLD)
                ELSE
                  NFTEN = NTEN
                ENDIF
                IRX = IBR(1,NFTEN)/(IBR(1,NFTE)+1) + 1
                IRZ = IBR(3,NFTEN)/(IBR(3,NFTE)+1) + 1
                NC = NC0X(IRX,IRZ)
                NTEN = ICM(NC,5,NTE)
                IF( IXP(NTEN).NE.0 .AND. INBS(2,NTE).EQ.0 ) THEN
                  DVX = ABS(XE(8,N)-XE(1,NTEN)) +
     &               ABS(YE(8,N)-YE(1,NTEN)) + ABS(ZE(8,N)-ZE(1,NTEN))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NTEN-1) + 1 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 8 (i,j+1,k+1)  ---
!
          IVX = 8*(N-1) + 7
!
!---      West node  ---
!
          NW = ICM(1,3,N)
          IF( NW.GT.0 ) THEN
            IF( NW.GT.NFLD ) THEN
              NFW = INP(NW-NFLD)
            ELSE
              NFW = NW
            ENDIF
            IRY = IBR(2,NFW)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFW)/(IBR(3,NF)+1) + 1
            NC = NC1X(IRY,IRZ)
            NW = ICM(NC,3,N)
            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              DVX = ABS(XE(7,N)-XE(8,NW)) +
     &           ABS(YE(7,N)-YE(8,NW)) + ABS(ZE(7,N)-ZE(8,NW))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NW-1) + 8 )
              ENDIF
            ENDIF
!
!---        West-north node  ---
!
            NWN = ICM(1,5,NW)
            IF( NWN.GT.0 ) THEN
              IF( NWN.GT.NFLD ) THEN
                NFWN = INP(NWN-NFLD)
              ELSE
                NFWN = NWN
              ENDIF
              IRX = IBR(1,NFWN)/(IBR(1,NFW)+1) + 1
              IRZ = IBR(3,NFWN)/(IBR(3,NFW)+1) + 1
              NC = NC1X(IRX,IRZ)
              NWN = ICM(NC,5,NW)
              IF( IXP(NWN).NE.0 .AND. INBS(4,NW).EQ.0 ) THEN
                DVX = ABS(XE(7,N)-XE(6,NWN)) +
     &             ABS(YE(7,N)-YE(6,NWN)) + ABS(ZE(7,N)-ZE(6,NWN))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NWN-1) + 6 )
                ENDIF
              ENDIF
!
!---          West-north-top node  ---
!
              NWNT = ICM(1,6,NWN)
              IF( NWNT.GT.0 ) THEN
                IF( NWNT.GT.NFLD ) THEN
                  NFWNT = INP(NWNT-NFLD)
                ELSE
                  NFWNT = NWNT
                ENDIF
                IRX = IBR(1,NFWNT)/(IBR(1,NFWN)+1) + 1
                IRY = IBR(2,NFWNT)/(IBR(2,NFWN)+1) + 1
                NC = NC3X(IRX,IRY)
                NWNT = ICM(NC,6,NWN)
                IF( IXP(NWNT).NE.0 .AND. INBS(6,NWN).EQ.0 ) THEN
                  DVX = ABS(XE(7,N)-XE(2,NWNT)) +
     &               ABS(YE(7,N)-YE(2,NWNT)) + ABS(ZE(7,N)-ZE(2,NWNT))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NWNT-1) + 2 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      North node  ---
!
          NN = ICM(1,5,N)
          IF( NN.GT.0 ) THEN
            IF( NN.GT.NFLD ) THEN
              NFN = INP(NN-NFLD)
            ELSE
              NFN = NN
            ENDIF
            IRX = IBR(1,NFN)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFN)/(IBR(3,NF)+1) + 1
            NC = NC2X(IRX,IRZ)
            NN = ICM(NC,5,N)
            IF( IXP(NN).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DVX = ABS(XE(7,N)-XE(5,NN)) +
     &           ABS(YE(7,N)-YE(5,NN)) + ABS(ZE(7,N)-ZE(5,NN))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NN-1) + 5 )
              ENDIF
            ENDIF
!
!---        North-top node  ---
!
            NNT = ICM(1,6,NN)
            IF( NNT.GT.0 ) THEN
              IF( NNT.GT.NFLD ) THEN
                NFNT = INP(NNT-NFLD)
              ELSE
                NFNT = NNT
              ENDIF
              IRX = IBR(1,NFNT)/(IBR(1,NFN)+1) + 1
              IRY = IBR(2,NFNT)/(IBR(2,NFN)+1) + 1
              NC = NC0X(IRX,IRY)
              NNT = ICM(NC,6,NN)
              IF( IXP(NNT).NE.0 .AND. INBS(6,NN).EQ.0 ) THEN
                DVX = ABS(XE(7,N)-XE(1,NNT)) +
     &             ABS(YE(7,N)-YE(1,NNT)) + ABS(ZE(7,N)-ZE(1,NNT))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NNT-1) + 1 )
                ENDIF
              ENDIF
!
!---          North-top-west node  ---
!
              NNTW = ICM(1,3,NNT)
              IF( NNTW.GT.0 ) THEN
                IF( NNTW.GT.NFLD ) THEN
                  NFNTW = INP(NNTW-NFLD)
                ELSE
                  NFNTW = NNTW
                ENDIF
                IRY = IBR(2,NFNTW)/(IBR(2,NFNT)+1) + 1
                IRZ = IBR(3,NFNTW)/(IBR(3,NFNT)+1) + 1
                NC = NC0X(IRY,IRZ)
                NNTW = ICM(NC,3,NNT)
                IF( IXP(NNTW).NE.0 .AND. INBS(3,NNT).EQ.0 ) THEN
                  DVX = ABS(XE(7,N)-XE(2,NNTW)) +
     &               ABS(YE(7,N)-YE(2,NNTW)) + ABS(ZE(7,N)-ZE(2,NNTW))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NNTW-1) + 2 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      Top node  ---
!
          NT = ICM(1,6,N)
          IF( NT.GT.0 ) THEN
            IF( NT.GT.NFLD ) THEN
              NFT = INP(NT-NFLD)
            ELSE
              NFT = NT
            ENDIF
            IRX = IBR(1,NFT)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFT)/(IBR(2,NF)+1) + 1
            NC = NC2X(IRX,IRY)
            NT = ICM(NC,6,N)
            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              DVX = ABS(XE(7,N)-XE(3,NT)) +
     &           ABS(YE(7,N)-YE(3,NT)) + ABS(ZE(7,N)-ZE(3,NT))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NT-1) + 3 )
              ENDIF
            ENDIF
!
!---        Top-west node  ---
!
            NTW = ICM(1,3,NT)
            IF( NTW.GT.0 ) THEN
              IF( NTW.GT.NFLD ) THEN
                NFTW = INP(NTW-NFLD)
              ELSE
                NFTW = NTW
              ENDIF
              IRY = IBR(2,NFTW)/(IBR(2,NFT)+1) + 1
              IRZ = IBR(3,NFTW)/(IBR(3,NFT)+1) + 1
              NC = NC3X(IRY,IRZ)
              NTW = ICM(NC,3,NT)
              IF( IXP(NTW).NE.0 .AND. INBS(3,NT).EQ.0 ) THEN
                DVX = ABS(XE(7,N)-XE(4,NTW)) +
     &             ABS(YE(7,N)-YE(4,NTW)) + ABS(ZE(7,N)-ZE(4,NTW))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NTW-1) + 4 )
                ENDIF
              ENDIF
!
!---          Top-west-north node  ---
!
              NTWN = ICM(1,5,NTW)
              IF( NTWN.GT.0 ) THEN
                IF( NTWN.GT.NFLD ) THEN
                  NFTWN = INP(NTWN-NFLD)
                ELSE
                  NFTWN = NTWN
                ENDIF
                IRX = IBR(1,NFTWN)/(IBR(1,NFTW)+1) + 1
                IRZ = IBR(3,NFTWN)/(IBR(3,NFTW)+1) + 1
                NC = NC3X(IRX,IRZ)
                NTWN = ICM(NC,5,NTW)
                IF( IXP(NTWN).NE.0 .AND. INBS(4,NTW).EQ.0 ) THEN
                  DVX = ABS(XE(7,N)-XE(2,NTWN)) +
     &               ABS(YE(7,N)-YE(2,NTWN)) + ABS(ZE(7,N)-ZE(2,NTWN))
                  IF( DVX.LT.1.D-9 ) THEN
                    IVX = MIN( IVX,8*(NTWN-1) + 2 )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9)') IVX
  200   CONTINUE
!
!---  XY domain ( Points 1,2,3,4 )
!
      ELSEIF( IFLD.GT.1 .AND. JFLD.GT.1 ) THEN
        DO 210 N = 1,NFLD
          IF( IXP(N).EQ.0 ) GOTO 210
          IF( N.GT.NFLD ) THEN
            NF = INP(N-NFLD)
          ELSE
            NF = N
          ENDIF
!
!---      Point 1 (i,j,k)  ---
!
          IVX = 4*(N-1) + 1
!
!---      West node  ---
!
          NW = ICM(1,3,N)
          IF( NW.GT.0 ) THEN
            IF( NW.GT.NFLD ) THEN
              NFW = INP(NW-NFLD)
            ELSE
              NFW = NW
            ENDIF
            IRY = IBR(2,NFW)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFW)/(IBR(3,NF)+1) + 1
            NC = NC0X(IRY,IRZ)
            NW = ICM(NC,3,N)
            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              DVX = ABS(XE(1,N)-XE(2,NW)) +
     &           ABS(YE(1,N)-YE(2,NW)) + ABS(ZE(1,N)-ZE(2,NW))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NW-1) + 2 )
              ENDIF
            ENDIF
!
!---        West-south node  ---
!
            NWS = ICM(1,2,NW)
            IF( NWS.GT.0 ) THEN
              IF( NWS.GT.NFLD ) THEN
                NFWS = INP(NWS-NFLD)
              ELSE
                NFWS = NWS
              ENDIF
              IRX = IBR(1,NFWS)/(IBR(1,NFW)+1) + 1
              IRZ = IBR(3,NFWS)/(IBR(3,NFW)+1) + 1
              NC = NC3X(IRX,IRZ)
              NWS = ICM(NC,2,NW)
              IF( IXP(NWS).NE.0 .AND. INBS(2,NW).EQ.0 ) THEN
                DVX = ABS(XE(1,N)-XE(4,NWS)) +
     &             ABS(YE(1,N)-YE(4,NWS)) + ABS(ZE(1,N)-ZE(4,NWS))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NWS-1) + 4 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      South node  ---
!
          NS = ICM(1,2,N)
          IF( NS.GT.0 ) THEN
            IF( NS.GT.NFLD ) THEN
              NFS = INP(NS-NFLD)
            ELSE
              NFS = NS
            ENDIF
            IRX = IBR(1,NFS)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFS)/(IBR(3,NF)+1) + 1
            NC = NC0X(IRX,IRZ)
            NS = ICM(NC,2,N)
            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              DVX = ABS(XE(1,N)-XE(3,NS)) +
     &           ABS(YE(1,N)-YE(3,NS)) + ABS(ZE(1,N)-ZE(3,NS))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NS-1) + 3 )
              ENDIF
            ENDIF
!
!---        South-west node  ---
!
            NSW = ICM(1,3,NS)
            IF( NSW.GT.0 ) THEN
              IF( NSW.GT.NFLD ) THEN
                NFSW = INP(NSW-NFLD)
              ELSE
                NFSW = NSW
              ENDIF
              IRY = IBR(2,NFSW)/(IBR(2,NFS)+1) + 1
              IRZ = IBR(3,NFSW)/(IBR(3,NFS)+1) + 1
              NC = NC1X(IRY,IRZ)
              NSW = ICM(NC,3,NS)
              IF( IXP(NSW).NE.0 .AND. INBS(3,NS).EQ.0 ) THEN
                DVX = ABS(XE(1,N)-XE(4,NSW)) +
     &             ABS(YE(1,N)-YE(4,NSW)) + ABS(ZE(1,N)-ZE(4,NSW))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NSW-1) + 4 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 2 (i+1,j,k)  ---
!
          IVX = 4*(N-1) + 2
!
!---      East node  ---
!
          NE = ICM(1,4,N)
          IF( NE.GT.0 ) THEN
            IF( NE.GT.NFLD ) THEN
              NFE = INP(NE-NFLD)
            ELSE
              NFE = NE
            ENDIF
            IRY = IBR(2,NFE)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFE)/(IBR(3,NF)+1) + 1
            NC = NC0X(IRY,IRZ)
            NE = ICM(NC,4,N)
            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DVX = ABS(XE(2,N)-XE(1,NE)) +
     &           ABS(YE(2,N)-YE(1,NE)) + ABS(ZE(2,N)-ZE(1,NE))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NE-1) + 1 )
              ENDIF
            ENDIF
!
!---        East-south node  ---
!
            NES = ICM(1,2,NE)
            IF( NES.GT.0 ) THEN
              IF( NES.GT.NFLD ) THEN
                NFES = INP(NES-NFLD)
              ELSE
                NFES = NES
              ENDIF
              IRX = IBR(1,NFES)/(IBR(1,NFE)+1) + 1
              IRZ = IBR(3,NFES)/(IBR(3,NFE)+1) + 1
              NC = NC0X(IRX,IRZ)
              NES = ICM(NC,2,NE)
              IF( IXP(NES).NE.0 .AND. INBS(2,NE).EQ.0 ) THEN
                DVX = ABS(XE(2,N)-XE(3,NES)) +
     &             ABS(YE(2,N)-YE(3,NES)) + ABS(ZE(2,N)-ZE(3,NES))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NES-1) + 3 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      South node  ---
!
          NS = ICM(1,2,N)
          IF( NS.GT.0 ) THEN
            IF( NS.GT.NFLD ) THEN
              NFS = INP(NS-NFLD)
            ELSE
              NFS = NS
            ENDIF
            IRX = IBR(1,NFS)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFS)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRX,IRZ)
            NS = ICM(NC,2,N)
            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              DVX = ABS(XE(2,N)-XE(4,NS)) +
     &           ABS(YE(2,N)-YE(4,NS)) + ABS(ZE(2,N)-ZE(4,NS))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NS-1) + 4 )
              ENDIF
            ENDIF
!
!---        South-east node  ---
!
            NSE = ICM(1,4,NS)
            IF( NSE.GT.0 ) THEN
              IF( NSE.GT.NFLD ) THEN
                NFSE = INP(NSE-NFLD)
              ELSE
                NFSE = NSE
              ENDIF
              IRY = IBR(2,NFSE)/(IBR(2,NFS)+1) + 1
              IRZ = IBR(3,NFSE)/(IBR(3,NFS)+1) + 1
              NC = NC1X(IRY,IRZ)
              NSE = ICM(NC,4,NS)
              IF( IXP(NSE).NE.0 .AND. INBS(4,NS).EQ.0 ) THEN
                DVX = ABS(XE(2,N)-XE(3,NSE)) +
     &             ABS(YE(2,N)-YE(3,NSE)) + ABS(ZE(2,N)-ZE(3,NSE))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NSE-1) + 3 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 3 (i+1,j+1,k)  ---
!
          IVX = 4*(N-1) + 4
!
!---      East node  ---
!
          NE = ICM(1,4,N)
          IF( NE.GT.0 ) THEN
            IF( NE.GT.NFLD ) THEN
              NFE = INP(NE-NFLD)
            ELSE
              NFE = NE
            ENDIF
            IRY = IBR(2,NFE)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFE)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRY,IRZ)
            NE = ICM(NC,4,N)
            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DVX = ABS(XE(4,N)-XE(3,NE)) +
     &           ABS(YE(4,N)-YE(3,NE)) + ABS(ZE(4,N)-ZE(3,NE))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NE-1) + 3 )
              ENDIF
            ENDIF
!
!---        East-north node  ---
!
            NEN = ICM(1,5,NE)
            IF( NEN.GT.0 ) THEN
              IF( NEN.GT.NFLD ) THEN
                NFEN = INP(NEN-NFLD)
              ELSE
                NFEN = NEN
              ENDIF
              IRX = IBR(1,NFEN)/(IBR(1,NFE)+1) + 1
              IRZ = IBR(3,NFEN)/(IBR(3,NFE)+1) + 1
              NC = NC0X(IRX,IRZ)
              NEN = ICM(NC,5,NE)
              IF( IXP(NEN).NE.0 .AND. INBS(5,NE).EQ.0 ) THEN
                DVX = ABS(XE(4,N)-XE(1,NEN)) +
     &             ABS(YE(4,N)-YE(1,NEN)) + ABS(ZE(4,N)-ZE(1,NEN))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NEN-1) + 1 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      North node  ---
!
          NN = ICM(1,5,N)
          IF( NN.GT.0 ) THEN
            IF( NN.GT.NFLD ) THEN
              NFN = INP(NN-NFLD)
            ELSE
              NFN = NN
            ENDIF
            IRX = IBR(1,NFN)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFN)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRX,IRZ)
            NN = ICM(NC,5,N)
            IF( IXP(NN).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              DVX = ABS(XE(4,N)-XE(2,NN)) +
     &           ABS(YE(4,N)-YE(2,NN)) + ABS(ZE(4,N)-ZE(2,NN))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NN-1) + 2 )
              ENDIF
            ENDIF
!
!---        North-east node  ---
!
            NNE = ICM(1,4,NN)
            IF( NNE.GT.0 ) THEN
              IF( NNE.GT.NFLD ) THEN
                NFNE = INP(NNE-NFLD)
              ELSE
                NFNE = NNE
              ENDIF
              IRY = IBR(2,NFNE)/(IBR(2,NFN)+1) + 1
              IRZ = IBR(3,NFNE)/(IBR(3,NFN)+1) + 1
              NC = NC2X(IRY,IRZ)
              NNE = ICM(NC,4,NN)
              IF( IXP(NNE).NE.0 .AND. INBS(4,NN).EQ.0 ) THEN
                DVX = ABS(XE(4,N)-XE(1,NNE)) +
     &             ABS(YE(4,N)-YE(1,NNE)) + ABS(ZE(4,N)-ZE(1,NNE))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NNE-1) + 1 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 4 (i,j+1,k)  ---
!
          IVX = 4*(N-1) + 3
!
!---      West node  ---
!
          NW = ICM(1,3,N)
          IF( NW.GT.0 ) THEN
            IF( NW.GT.NFLD ) THEN
              NFW = INP(NW-NFLD)
            ELSE
              NFW = NW
            ENDIF
            IRY = IBR(2,NFW)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFW)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRY,IRZ)
            NW = ICM(NC,3,N)
            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              DVX = ABS(XE(3,N)-XE(4,NW)) +
     &           ABS(YE(3,N)-YE(4,NW)) + ABS(ZE(3,N)-ZE(4,NW))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NW-1) + 4 )
              ENDIF
            ENDIF
!
!---        West-north node  ---
!
            NWN = ICM(1,5,NW)
            IF( NWN.GT.0 ) THEN
              IF( NWN.GT.NFLD ) THEN
                NFWN = INP(NWN-NFLD)
              ELSE
                NFWN = NWN
              ENDIF
              IRX = IBR(1,NFWN)/(IBR(1,NFW)+1) + 1
              IRZ = IBR(3,NFWN)/(IBR(3,NFW)+1) + 1
              NC = NC3X(IRX,IRZ)
              NWN = ICM(NC,5,NW)
              IF( IXP(NWN).NE.0 .AND. INBS(4,NW).EQ.0 ) THEN
                DVX = ABS(XE(3,N)-XE(2,NWN)) +
     &             ABS(YE(3,N)-YE(2,NWN)) + ABS(ZE(3,N)-ZE(2,NWN))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NWN-1) + 2 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      North node  ---
!
          NN = ICM(1,5,N)
          IF( NN.GT.0 ) THEN
            IF( NN.GT.NFLD ) THEN
              NFN = INP(NN-NFLD)
            ELSE
              NFN = NN
            ENDIF
            IRX = IBR(1,NFN)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFN)/(IBR(3,NF)+1) + 1
            NC = NC0X(IRX,IRZ)
            NN = ICM(NC,5,N)
            IF( IXP(NN).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DVX = ABS(XE(3,N)-XE(1,NN)) +
     &           ABS(YE(3,N)-YE(1,NN)) + ABS(ZE(3,N)-ZE(1,NN))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NN-1) + 1 )
              ENDIF
            ENDIF
!
!---        North-west node  ---
!
            NNW = ICM(1,3,NN)
            IF( NNW.GT.0 ) THEN
              IF( NNW.GT.NFLD ) THEN
                NFNW = INP(NNW-NFLD)
              ELSE
                NFNW = NNW
              ENDIF
              IRY = IBR(2,NFNW)/(IBR(2,NFN)+1) + 1
              IRZ = IBR(3,NFNW)/(IBR(3,NFN)+1) + 1
              NC = NC2X(IRY,IRZ)
              NNW = ICM(NC,3,NN)
              IF( IXP(NNW).NE.0 .AND. INBS(3,NN).EQ.0 ) THEN
                DVX = ABS(XE(3,N)-XE(2,NNW)) +
     &             ABS(YE(3,N)-YE(2,NNW)) + ABS(ZE(3,N)-ZE(2,NNW))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NNW-1) + 2 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9)') IVX
  210   CONTINUE
!
!---  YZ domain ( Points 1,2,4,3 )  ---
!
      ELSEIF( JFLD.GT.1 .AND. KFLD.GT.1 ) THEN
        DO 220 N = 1,NFLD
          IF( IXP(N).EQ.0 ) GOTO 220
          IF( N.GT.NFLD ) THEN
            NF = INP(N-NFLD)
          ELSE
            NF = N
          ENDIF
!
!---      Point 1 (i,j,k)  ---
!
          IVX = 4*(N-1) + 1
!
!---      South node  ---
!
          NS = ICM(1,2,N)
          IF( NS.GT.0 ) THEN
            IF( NS.GT.NFLD ) THEN
              NFS = INP(NS-NFLD)
            ELSE
              NFS = NS
            ENDIF
            IRX = IBR(1,NFS)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFS)/(IBR(3,NF)+1) + 1
            NC = NC0X(IRX,IRZ)
            NS = ICM(NC,2,N)
            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              DVX = ABS(XE(1,N)-XE(2,NS)) +
     &           ABS(YE(1,N)-YE(2,NS)) + ABS(ZE(1,N)-ZE(2,NS))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NS-1) + 2 )
              ENDIF
            ENDIF
!
!---        South-bottom node  ---
!
            NSB = ICM(1,1,NS)
            IF( NSB.GT.0 ) THEN
              IF( NSB.GT.NFLD ) THEN
                NFSB = INP(NSB-NFLD)
              ELSE
                NFSB = NSB
              ENDIF
              IRX = IBR(1,NFSB)/(IBR(1,NFS)+1) + 1
              IRY = IBR(2,NFSB)/(IBR(2,NFS)+1) + 1
              NC = NC2X(IRX,IRY)
              NSB = ICM(NC,1,NS)
              IF( IXP(NSB).NE.0 .AND. INBS(1,NS).EQ.0 ) THEN
                DVX = ABS(XE(1,N)-XE(4,NSB)) +
     &             ABS(YE(1,N)-YE(4,NSB)) + ABS(ZE(1,N)-ZE(4,NSB))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NSB-1) + 4 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      Bottom node  ---
!
          NB = ICM(1,1,N)
          IF( NB.GT.0 ) THEN
            IF( NB.GT.NFLD ) THEN
              NFB = INP(NB-NFLD)
            ELSE
              NFB = NB
            ENDIF
            IRX = IBR(1,NFB)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFB)/(IBR(2,NF)+1) + 1
            NC = NC0X(IRX,IRY)
            NB = ICM(NC,1,N)
            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              DVX = ABS(XE(1,N)-XE(3,NB)) +
     &           ABS(YE(1,N)-YE(3,NB)) + ABS(ZE(1,N)-ZE(3,NB))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NB-1) + 3 )
              ENDIF
            ENDIF
!
!---        Bottom-south node  ---
!
            NBS = ICM(1,2,NB)
            IF( NBS.GT.0 ) THEN
              IF( NBS.GT.NFLD ) THEN
                NFBS = INP(NBS-NFLD)
              ELSE
                NFBS = NBS
              ENDIF
              IRX = IBR(1,NFBS)/(IBR(1,NFB)+1) + 1
              IRZ = IBR(3,NFBS)/(IBR(3,NFB)+1) + 1
              NC = NC1X(IRX,IRZ)
              NBS = ICM(NC,2,NB)
              IF( IXP(NBS).NE.0 .AND. INBS(2,NB).EQ.0 ) THEN
                DVX = ABS(XE(1,N)-XE(4,NBS)) +
     &             ABS(YE(1,N)-YE(4,NBS)) + ABS(ZE(1,N)-ZE(4,NBS))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NBS-1) + 4 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 2 (i,j+1,k)  ---
!
          IVX = 4*(N-1) + 2
!
!---      North node  ---
!
          NN = ICM(1,5,N)
          IF( NN.GT.0 ) THEN
            IF( NN.GT.NFLD ) THEN
              NFN = INP(NN-NFLD)
            ELSE
              NFN = NN
            ENDIF
            IRX = IBR(1,NFN)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFN)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRX,IRZ)
            NN = ICM(NC,5,N)
            IF( IXP(NN).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              DVX = ABS(XE(3,N)-XE(1,NN)) +
     &           ABS(YE(3,N)-YE(1,NN)) + ABS(ZE(3,N)-ZE(1,NN))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NN-1) + 1 )
              ENDIF
            ENDIF
!
!---        North-bottom node  ---
!
            NNB = ICM(1,1,NN)
            IF( NNB.GT.0 ) THEN
              IF( NNB.GT.NFLD ) THEN
                NFNB = INP(NNB-NFLD)
              ELSE
                NFNB = NNB
              ENDIF
              IRX = IBR(1,NFNB)/(IBR(1,NFN)+1) + 1
              IRY = IBR(2,NFNB)/(IBR(2,NFN)+1) + 1
              NC = NC3X(IRX,IRY)
              NNB = ICM(NC,1,NN)
              IF( IXP(NNB).NE.0 .AND. INBS(1,NN).EQ.0 ) THEN
                DVX = ABS(XE(3,N)-XE(3,NNB)) +
     &             ABS(YE(3,N)-YE(3,NNB)) + ABS(ZE(3,N)-ZE(3,NNB))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NNB-1) + 3 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      Bottom node  ---
!
          NB = ICM(1,1,N)
          IF( NB.GT.0 ) THEN
            IF( NB.GT.NFLD ) THEN
              NFB = INP(NB-NFLD)
            ELSE
              NFB = NB
            ENDIF
            IRX = IBR(1,NFB)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFB)/(IBR(2,NF)+1) + 1
            NC = NC1X(IRX,IRY)
            NB = ICM(NC,1,N)
            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              DVX = ABS(XE(3,N)-XE(4,NB)) +
     &           ABS(YE(3,N)-YE(4,NB)) + ABS(ZE(3,N)-ZE(4,NB))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NB-1) + 4 )
              ENDIF
            ENDIF
!
!---        Bottom-north node  ---
!
            NBN = ICM(1,5,NB)
            IF( NBN.GT.0 ) THEN
              IF( NBN.GT.NFLD ) THEN
                NFBN = INP(NBN-NFLD)
              ELSE
                NFBN = NBN
              ENDIF
              IRX = IBR(1,NFBN)/(IBR(1,NFB)+1) + 1
              IRZ = IBR(3,NFBN)/(IBR(3,NFB)+1) + 1
              NC = NC2X(IRX,IRZ)
              NBN = ICM(NC,5,NB)
              IF( IXP(NBN).NE.0 .AND. INBS(2,NB).EQ.0 ) THEN
                DVX = ABS(XE(3,N)-XE(3,NBN)) +
     &             ABS(YE(3,N)-YE(3,NBN)) + ABS(ZE(3,N)-ZE(3,NBN))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NBN-1) + 3 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 4 (i,j+1,k+1)  ---
!
          IVX = 4*(N-1) + 4
!
!---      North node  ---
!
          NN = ICM(1,5,N)
          IF( NN.GT.0 ) THEN
            IF( NN.GT.NFLD ) THEN
              NFN = INP(NN-NFLD)
            ELSE
              NFN = NN
            ENDIF
            IRX = IBR(1,NFN)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFN)/(IBR(3,NF)+1) + 1
            NC = NC1X(IRX,IRZ)
            NN = ICM(NC,5,N)
            IF( IXP(NN).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              DVX = ABS(XE(7,N)-XE(3,NN)) +
     &           ABS(YE(7,N)-YE(3,NN)) + ABS(ZE(7,N)-ZE(3,NN))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NN-1) + 3 )
              ENDIF
            ENDIF
!
!---        North-top node  ---
!
            NNT = ICM(1,6,NN)
            IF( NNT.GT.0 ) THEN
              IF( NNT.GT.NFLD ) THEN
                NFNT = INP(NNT-NFLD)
              ELSE
                NFNT = NNT
              ENDIF
              IRX = IBR(1,NFNT)/(IBR(1,NFN)+1) + 1
              IRY = IBR(2,NFNT)/(IBR(2,NFN)+1) + 1
              NC = NC3X(IRX,IRY)
              NNT = ICM(NC,6,NN)
              IF( IXP(NNT).NE.0 .AND. INBS(6,NN).EQ.0 ) THEN
                DVX = ABS(XE(7,N)-XE(1,NNT)) +
     &             ABS(YE(7,N)-YE(1,NNT)) + ABS(ZE(7,N)-ZE(1,NNT))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NNT-1) + 1 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      Top node  ---
!
          NT = ICM(1,6,N)
          IF( NT.GT.0 ) THEN
            IF( NT.GT.NFLD ) THEN
              NFT = INP(NT-NFLD)
            ELSE
              NFT = NT
            ENDIF
            IRX = IBR(1,NFT)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFT)/(IBR(2,NF)+1) + 1
            NC = NC1X(IRX,IRY)
            NT = ICM(NC,6,N)
            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              DVX = ABS(XE(7,N)-XE(2,NT)) +
     &           ABS(YE(7,N)-YE(2,NT)) + ABS(ZE(7,N)-ZE(2,NT))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NT-1) + 2 )
              ENDIF
            ENDIF
!
!---        Top-north node  ---
!
            NTN = ICM(1,5,NT)
            IF( NTN.GT.0 ) THEN
              IF( NTN.GT.NFLD ) THEN
                NFTN = INP(NTN-NFLD)
              ELSE
                NFTN = NTN
              ENDIF
              IRX = IBR(1,NFTN)/(IBR(1,NFT)+1) + 1
              IRZ = IBR(3,NFTN)/(IBR(3,NFT)+1) + 1
              NC = NC0X(IRX,IRZ)
              NTN = ICM(NC,5,NT)
              IF( IXP(NTN).NE.0 .AND. INBS(2,NT).EQ.0 ) THEN
                DVX = ABS(XE(7,N)-XE(1,NTN)) +
     &             ABS(YE(7,N)-YE(1,NTN)) + ABS(ZE(7,N)-ZE(1,NTN))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NTN-1) + 1 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 3 (i,j,k+1)  ---
!
          IVX = 4*(N-1) + 3
!
!---      South node  ---
!
          NS = ICM(1,2,N)
          IF( NS.GT.0 ) THEN
            IF( NS.GT.NFLD ) THEN
              NFS = INP(NS-NFLD)
            ELSE
              NFS = NS
            ENDIF
            IRX = IBR(1,NFS)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFS)/(IBR(3,NF)+1) + 1
            NC = NC1X(IRX,IRZ)
            NS = ICM(NC,2,N)
            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              DVX = ABS(XE(5,N)-XE(4,NS)) +
     &           ABS(YE(5,N)-YE(4,NS)) + ABS(ZE(5,N)-ZE(4,NS))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NS-1) + 4 )
              ENDIF
            ENDIF
!
!---        South-top node  ---
!
            NST = ICM(1,6,NS)
            IF( NST.GT.0 ) THEN
              IF( NST.GT.NFLD ) THEN
                NFST = INP(NST-NFLD)
              ELSE
                NFST = NST
              ENDIF
              IRX = IBR(1,NFST)/(IBR(1,NFS)+1) + 1
              IRY = IBR(2,NFST)/(IBR(2,NFS)+1) + 1
              NC = NC1X(IRX,IRY)
              NST = ICM(NC,6,NS)
              IF( IXP(NST).NE.0 .AND. INBS(6,NS).EQ.0 ) THEN
                DVX = ABS(XE(5,N)-XE(2,NST)) +
     &             ABS(YE(5,N)-YE(2,NST)) + ABS(ZE(5,N)-ZE(2,NST))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NST-1) + 2 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      Top node  ---
!
          NT = ICM(1,6,N)
          IF( NT.GT.0 ) THEN
            IF( NT.GT.NFLD ) THEN
              NFT = INP(NT-NFLD)
            ELSE
              NFT = NT
            ENDIF
            IRX = IBR(1,NFT)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFT)/(IBR(2,NF)+1) + 1
            NC = NC3X(IRX,IRY)
            NT = ICM(NC,6,N)
            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              DVX = ABS(XE(5,N)-XE(1,NT)) +
     &           ABS(YE(5,N)-YE(1,NT)) + ABS(ZE(5,N)-ZE(1,NT))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NT-1) + 1 )
              ENDIF
            ENDIF
!
!---        Top-south node  ---
!
            NTS = ICM(1,2,NT)
            IF( NTS.GT.0 ) THEN
              IF( NTS.GT.NFLD ) THEN
                NFTS = INP(NTS-NFLD)
              ELSE
                NFTS = NTS
              ENDIF
              IRX = IBR(1,NFTS)/(IBR(1,NFT)+1) + 1
              IRZ = IBR(3,NFTS)/(IBR(3,NFT)+1) + 1
              NC = NC0X(IRX,IRZ)
              NTS = ICM(NC,2,NT)
              IF( IXP(NTS).NE.0 .AND. INBS(2,NT).EQ.0 ) THEN
                DVX = ABS(XE(5,N)-XE(2,NTS)) +
     &             ABS(YE(5,N)-YE(2,NTS)) + ABS(ZE(5,N)-ZE(2,NTS))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NTS-1) + 2 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9)') IVX
  220   CONTINUE
!
!---  XZ domain ( Points 1,2,4,3 )  ---
!
      ELSEIF( KFLD.GT.1 .AND. IFLD.GT.1 ) THEN
        DO 230 N = 1,NFLD
          IF( IXP(N).EQ.0 ) GOTO 230
          IF( N.GT.NFLD ) THEN
            NF = INP(N-NFLD)
          ELSE
            NF = N
          ENDIF
!
!---      Point 1 (i,j,k)  ---
!
          IVX = 4*(N-1) + 1
!
!---      West node  ---
!
          NW = ICM(1,3,N)
          IF( NW.GT.0 ) THEN
            IF( NW.GT.NFLD ) THEN
              NFW = INP(NW-NFLD)
            ELSE
              NFW = NW
            ENDIF
            IRY = IBR(2,NFW)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFW)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRY,IRZ)
            NW = ICM(NC,3,N)
            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              DVX = ABS(XE(1,N)-XE(2,NW)) +
     &           ABS(YE(1,N)-YE(2,NW)) + ABS(ZE(1,N)-ZE(2,NW))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NW-1) + 2 )
              ENDIF
            ENDIF
!
!---        West-bottom node  ---
!
            NWB = ICM(1,1,NW)
            IF( NWB.GT.0 ) THEN
              IF( NWB.GT.NFLD ) THEN
                NFWB = INP(NWB-NFLD)
              ELSE
                NFWB = NWB
              ENDIF
              IRX = IBR(1,NFWB)/(IBR(1,NFW)+1) + 1
              IRY = IBR(2,NFWB)/(IBR(2,NFW)+1) + 1
              NC = NC3X(IRX,IRY)
              NWB = ICM(NC,1,NW)
              IF( IXP(NWB).NE.0 .AND. INBS(1,NW).EQ.0 ) THEN
                DVX = ABS(XE(1,N)-XE(4,NWB)) +
     &             ABS(YE(1,N)-YE(4,NWB)) + ABS(ZE(1,N)-ZE(4,NWB))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NWB-1) + 4 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      Bottom node  ---
!
          NB = ICM(1,1,N)
          IF( NB.GT.0 ) THEN
            IF( NB.GT.NFLD ) THEN
              NFB = INP(NB-NFLD)
            ELSE
              NFB = NB
            ENDIF
            IRX = IBR(1,NFB)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFB)/(IBR(2,NF)+1) + 1
            NC = NC2X(IRX,IRY)
            NB = ICM(NC,1,N)
            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              DVX = ABS(XE(1,N)-XE(3,NB)) +
     &           ABS(YE(1,N)-YE(3,NB)) + ABS(ZE(1,N)-ZE(3,NB))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NB-1) + 3 )
              ENDIF
            ENDIF
!
!---        Bottom-west node  ---
!
            NBW = ICM(1,3,NB)
            IF( NBW.GT.0 ) THEN
              IF( NBW.GT.NFLD ) THEN
                NFBW = INP(NBW-NFLD)
              ELSE
                NFBW = NBW
              ENDIF
              IRY = IBR(2,NFBW)/(IBR(2,NFB)+1) + 1
              IRZ = IBR(3,NFBW)/(IBR(3,NFB)+1) + 1
              NC = NC1X(IRY,IRZ)
              NBW = ICM(NC,3,NB)
              IF( IXP(NBW).NE.0 .AND. INBS(3,NB).EQ.0 ) THEN
                DVX = ABS(XE(1,N)-XE(4,NBW)) +
     &             ABS(YE(1,N)-YE(4,NBW)) + ABS(ZE(1,N)-ZE(4,NBW))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NBW-1) + 4 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 2 (i+1,j,k)  ---
!
          IVX = 4*(N-1) + 2
!
!---      East node  ---
!
          NE = ICM(1,4,N)
          IF( NE.GT.0 ) THEN
            IF( NE.GT.NFLD ) THEN
              NFE = INP(NE-NFLD)
            ELSE
              NFE = NE
            ENDIF
            IRY = IBR(2,NFE)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFE)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRY,IRZ)
            NE = ICM(NC,4,N)
            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DVX = ABS(XE(2,N)-XE(1,NE)) +
     &           ABS(YE(2,N)-YE(1,NE)) + ABS(ZE(2,N)-ZE(1,NE))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NE-1) + 1 )
              ENDIF
            ENDIF
!
!---        East-bottom node  ---
!
            NEB = ICM(1,1,NE)
            IF( NEB.GT.0 ) THEN
              IF( NEB.GT.NFLD ) THEN
                NFEB = INP(NEB-NFLD)
              ELSE
                NFEB = NEB
              ENDIF
              IRX = IBR(1,NFEB)/(IBR(1,NFE)+1) + 1
              IRY = IBR(2,NFEB)/(IBR(2,NFE)+1) + 1
              NC = NC0X(IRX,IRY)
              NEB = ICM(NC,1,NE)
              IF( IXP(NEB).NE.0 .AND. INBS(1,NE).EQ.0 ) THEN
                DVX = ABS(XE(2,N)-XE(3,NEB)) +
     &             ABS(YE(2,N)-YE(3,NEB)) + ABS(ZE(2,N)-ZE(3,NEB))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NEB-1) + 3 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      Bottom node  ---
!
          NB = ICM(1,1,N)
          IF( NB.GT.0 ) THEN
            IF( NB.GT.NFLD ) THEN
              NFB = INP(NB-NFLD)
            ELSE
              NFB = NB
            ENDIF
            IRX = IBR(1,NFB)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFB)/(IBR(2,NF)+1) + 1
            NC = NC1X(IRX,IRY)
            NB = ICM(NC,1,N)
            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              DVX = ABS(XE(2,N)-XE(4,NB)) +
     &           ABS(YE(2,N)-YE(4,NB)) + ABS(ZE(2,N)-ZE(4,NB))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NB-1) + 4 )
              ENDIF
            ENDIF
!
!---        Bottom-east node  ---
!
            NBE = ICM(1,4,NB)
            IF( NBE.GT.0 ) THEN
              IF( NBE.GT.NFLD ) THEN
                NFBE = INP(NBE-NFLD)
              ELSE
                NFBE = NBE
              ENDIF
              IRY = IBR(2,NFBE)/(IBR(2,NFB)+1) + 1
              IRZ = IBR(3,NFBE)/(IBR(3,NFB)+1) + 1
              NC = NC1X(IRY,IRZ)
              NBE = ICM(NC,4,NB)
              IF( IXP(NBE).NE.0 .AND. INBS(4,NB).EQ.0 ) THEN
                DVX = ABS(XE(2,N)-XE(3,NBE)) +
     &             ABS(YE(2,N)-YE(3,NBE)) + ABS(ZE(2,N)-ZE(3,NBE))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NBE-1) + 3 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 4 (i+1,j,k+1)  ---
!
          IVX = 4*(N-1) + 4
!
!---      East node  ---
!
          NE = ICM(1,4,N)
          IF( NE.GT.0 ) THEN
            IF( NE.GT.NFLD ) THEN
              NFE = INP(NE-NFLD)
            ELSE
              NFE = NE
            ENDIF
            IRY = IBR(2,NFE)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFE)/(IBR(3,NF)+1) + 1
            NC = NC2X(IRY,IRZ)
            NE = ICM(NC,4,N)
            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DVX = ABS(XE(6,N)-XE(3,NE)) +
     &           ABS(YE(6,N)-YE(3,NE)) + ABS(ZE(6,N)-ZE(3,NE))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NE-1) + 3 )
              ENDIF
            ENDIF
!
!---        East-top node  ---
!
            NET = ICM(1,6,NE)
            IF( NET.GT.0 ) THEN
              IF( NET.GT.NFLD ) THEN
                NFET = INP(NET-NFLD)
              ELSE
                NFET = NET
              ENDIF
              IRX = IBR(1,NFET)/(IBR(1,NFE)+1) + 1
              IRY = IBR(2,NFET)/(IBR(2,NFE)+1) + 1
              NC = NC2X(IRX,IRY)
              NET = ICM(NC,6,NE)
              IF( IXP(NET).NE.0 .AND. INBS(6,NE).EQ.0 ) THEN
                DVX = ABS(XE(6,N)-XE(1,NET)) +
     &             ABS(YE(6,N)-YE(1,NET)) + ABS(ZE(6,N)-ZE(1,NET))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NET-1) + 1 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      Top node  ---
!
          NT = ICM(1,6,N)
          IF( NT.GT.0 ) THEN
            IF( NT.GT.NFLD ) THEN
              NFT = INP(NT-NFLD)
            ELSE
              NFT = NT
            ENDIF
            IRX = IBR(1,NFT)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFT)/(IBR(2,NF)+1) + 1
            NC = NC3X(IRX,IRY)
            NT = ICM(NC,6,N)
            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              DVX = ABS(XE(6,N)-XE(2,NT)) +
     &           ABS(YE(6,N)-YE(2,NT)) + ABS(ZE(6,N)-ZE(2,NT))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NT-1) + 2 )
              ENDIF
            ENDIF
!
!---        Top-east node  ---
!
            NTE = ICM(1,4,NT)
            IF( NTE.GT.0 ) THEN
              IF( NTE.GT.NFLD ) THEN
                NFTE = INP(NTE-NFLD)
              ELSE
                NFTE = NTE
              ENDIF
              IRY = IBR(2,NFTE)/(IBR(2,NFT)+1) + 1
              IRZ = IBR(3,NFTE)/(IBR(3,NFT)+1) + 1
              NC = NC0X(IRY,IRZ)
              NTE = ICM(NC,4,NT)
              IF( IXP(NTE).NE.0 .AND. INBS(4,NT).EQ.0 ) THEN
                DVX = ABS(XE(6,N)-XE(1,NTE)) +
     &             ABS(YE(6,N)-YE(1,NTE)) + ABS(ZE(6,N)-ZE(1,NTE))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NTE-1) + 1 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 3 (i,j,k+1)  ---
!
          IVX = 4*(N-1) + 3
!
!---      West node  ---
!
          NW = ICM(1,3,N)
          IF( NW.GT.0 ) THEN
            IF( NW.GT.NFLD ) THEN
              NFW = INP(NW-NFLD)
            ELSE
              NFW = NW
            ENDIF
            IRY = IBR(2,NFW)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFW)/(IBR(3,NF)+1) + 1
            NC = NC2X(IRY,IRZ)
            NW = ICM(NC,3,N)
            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              DVX = ABS(XE(5,N)-XE(4,NW)) +
     &           ABS(YE(5,N)-YE(4,NW)) + ABS(ZE(5,N)-ZE(4,NW))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NW-1) + 4 )
              ENDIF
            ENDIF
!
!---        West-top node  ---
!
            NWT = ICM(1,6,NW)
            IF( NWT.GT.0 ) THEN
              IF( NWT.GT.NFLD ) THEN
                NFWT = INP(NWT-NFLD)
              ELSE
                NFWT = NWT
              ENDIF
              IRX = IBR(1,NFWT)/(IBR(1,NFW)+1) + 1
              IRY = IBR(2,NFWT)/(IBR(2,NFW)+1) + 1
              NC = NC1X(IRX,IRY)
              NWT = ICM(NC,6,NW)
              IF( IXP(NWT).NE.0 .AND. INBS(6,NW).EQ.0 ) THEN
                DVX = ABS(XE(5,N)-XE(2,NWT)) +
     &             ABS(YE(5,N)-YE(2,NWT)) + ABS(ZE(5,N)-ZE(2,NWT))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,4*(NWT-1) + 2 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
!
!---      Top node  ---
!
          NT = ICM(1,6,N)
          IF( NT.GT.0 ) THEN
            IF( NT.GT.NFLD ) THEN
              NFT = INP(NT-NFLD)
            ELSE
              NFT = NT
            ENDIF
            IRX = IBR(1,NFT)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFT)/(IBR(2,NF)+1) + 1
            NC = NC0X(IRX,IRY)
            NT = ICM(NC,6,N)
            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              DVX = ABS(XE(5,N)-XE(1,NT)) +
     &           ABS(YE(5,N)-YE(1,NT)) + ABS(ZE(5,N)-ZE(1,NT))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,4*(NT-1) + 1 )
              ENDIF
            ENDIF
!
!---        Top-west node  ---
!
            NTW = ICM(1,3,NT)
            IF( NTW.GT.0 ) THEN
              IF( NTW.GT.NFLD ) THEN
                NFTW = INP(NTW-NFLD)
              ELSE
                NFTW = NTW
              ENDIF
              IRY = IBR(2,NFTW)/(IBR(2,NFT)+1) + 1
              IRZ = IBR(3,NFTW)/(IBR(3,NFT)+1) + 1
              NC = NC0X(IRY,IRZ)
              NTW = ICM(NC,3,NT)
              IF( IXP(NTW).NE.0 .AND. INBS(3,NT).EQ.0 ) THEN
                DVX = ABS(XE(5,N)-XE(2,NTW)) +
     &             ABS(YE(5,N)-YE(2,NTW)) + ABS(ZE(5,N)-ZE(2,NTW))
                IF( DVX.LT.1.D-9 ) THEN
                  IVX = MIN( IVX,8*(NTW-1) + 2 )
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9)') IVX
  230   CONTINUE
!
!---  X domain ( Points 1,2,3,4,5,6,7,8 )  ---
!
      ELSEIF( IFLD.GT.1 ) THEN
        DO 240 N = 1,NFLD
          IF( IXP(N).EQ.0 ) GOTO 240
          IF( N.GT.NFLD ) THEN
            NF = INP(N-NFLD)
          ELSE
            NF = N
          ENDIF
          I = ID(N)
!
!---      Point 1 (i,j,k)  ---
!
          IVX = 8*(N-1) + 1
!
!---      West node  ---
!
          NW = ICM(1,3,N)
          IF( NW.GT.0 ) THEN
            IF( NW.GT.NFLD ) THEN
              NFW = INP(NW-NFLD)
            ELSE
              NFW = NW
            ENDIF
            IRY = IBR(2,NFW)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFW)/(IBR(3,NF)+1) + 1
            NC = NC2X(IRY,IRZ)
            NW = ICM(NC,3,N)
            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              DVX = ABS(XE(1,N)-XE(2,NW)) +
     &           ABS(YE(1,N)-YE(2,NW)) + ABS(ZE(1,N)-ZE(2,NW))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NW-1) + 2 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 2 (i+1,j,k)  ---
!
          IVX = 8*(N-1) + 2
!
!---      East node  ---
!
          NE = ICM(1,4,N)
          IF( NE.GT.0 ) THEN
            IF( NE.GT.NFLD ) THEN
              NFE = INP(NE-NFLD)
            ELSE
              NFE = NE
            ENDIF
            IRY = IBR(2,NFE)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFE)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRY,IRZ)
            NE = ICM(NC,4,N)
            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DVX = ABS(XE(2,N)-XE(1,NE)) +
     &           ABS(YE(2,N)-YE(1,NE)) + ABS(ZE(2,N)-ZE(1,NE))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NE-1) + 1 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 3 (i+1,j+1,k)  ---
!
          IVX = 8*(N-1) + 4
!
!---      East node  ---
!
          NE = ICM(1,4,N)
          IF( NE.GT.0 ) THEN
            IF( NE.GT.NFLD ) THEN
              NFE = INP(NE-NFLD)
            ELSE
              NFE = NE
            ENDIF
            IRY = IBR(2,NFE)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFE)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRY,IRZ)
            NE = ICM(NC,4,N)
            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DVX = ABS(XE(4,N)-XE(3,NE)) +
     &           ABS(YE(4,N)-YE(3,NE)) + ABS(ZE(4,N)-ZE(3,NE))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NE-1) + 3 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 4 (i,j+1,k)  ---
!
          IVX = 8*(N-1) + 3
!
!---      West node  ---
!
          NW = ICM(1,3,N)
          IF( NW.GT.0 ) THEN
            IF( NW.GT.NFLD ) THEN
              NFW = INP(NW-NFLD)
            ELSE
              NFW = NW
            ENDIF
            IRY = IBR(2,NFW)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFW)/(IBR(3,NF)+1) + 1
            NC = NC2X(IRY,IRZ)
            NW = ICM(NC,3,N)
            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              DVX = ABS(XE(3,N)-XE(4,NW)) +
     &           ABS(YE(3,N)-YE(4,NW)) + ABS(ZE(3,N)-ZE(4,NW))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NW-1) + 4 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 5 (i,j,k+1)  ---
!
          IVX = 8*(N-1) + 5
!
!---      West node  ---
!
          NW = ICM(1,3,N)
          IF( NW.GT.0 ) THEN
            IF( NW.GT.NFLD ) THEN
              NFW = INP(NW-NFLD)
            ELSE
              NFW = NW
            ENDIF
            IRY = IBR(2,NFW)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFW)/(IBR(3,NF)+1) + 1
            NC = NC2X(IRY,IRZ)
            NW = ICM(NC,3,N)
            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              DVX = ABS(XE(5,N)-XE(6,NW)) +
     &           ABS(YE(5,N)-YE(6,NW)) + ABS(ZE(5,N)-ZE(6,NW))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NW-1) + 6 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 6 (i+1,j,k+1)  ---
!
          IVX = 8*(N-1) + 6
!
!---      East node  ---
!
          NE = ICM(1,4,N)
          IF( NE.GT.0 ) THEN
            IF( NE.GT.NFLD ) THEN
              NFE = INP(NE-NFLD)
            ELSE
              NFE = NE
            ENDIF
            IRY = IBR(2,NFE)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFE)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRY,IRZ)
            NE = ICM(NC,4,N)
            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DVX = ABS(XE(6,N)-XE(5,NE)) +
     &           ABS(YE(6,N)-YE(5,NE)) + ABS(ZE(6,N)-ZE(5,NE))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NE-1) + 5 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 7 (i+1,j+1,k+1)  ---
!
          IVX = 8*(N-1) + 8
!
!---      East node  ---
!
          NE = ICM(1,4,N)
          IF( NE.GT.0 ) THEN
            IF( NE.GT.NFLD ) THEN
              NFE = INP(NE-NFLD)
            ELSE
              NFE = NE
            ENDIF
            IRY = IBR(2,NFE)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFE)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRY,IRZ)
            NE = ICM(NC,4,N)
            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
              DVX = ABS(XE(8,N)-XE(7,NE)) +
     &           ABS(YE(8,N)-YE(7,NE)) + ABS(ZE(8,N)-ZE(7,NE))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NE-1) + 7 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 8 (i,j+1,k+1)  ---
!
          IVX = 8*(N-1) + 7
!
!---      West node  ---
!
          NW = ICM(1,3,N)
          IF( NW.GT.0 ) THEN
            IF( NW.GT.NFLD ) THEN
              NFW = INP(NW-NFLD)
            ELSE
              NFW = NW
            ENDIF
            IRY = IBR(2,NFW)/(IBR(2,NF)+1) + 1
            IRZ = IBR(3,NFW)/(IBR(3,NF)+1) + 1
            NC = NC2X(IRY,IRZ)
            NW = ICM(NC,3,N)
            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
              DVX = ABS(XE(7,N)-XE(8,NW)) +
     &           ABS(YE(7,N)-YE(8,NW)) + ABS(ZE(7,N)-ZE(8,NW))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NW-1) + 8 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9)') IVX
  240   CONTINUE
!
!---  Y domain ( Points 1,2,3,4,5,6,7,8 )  ---
!
      ELSEIF( JFLD.GT.1 ) THEN
        DO 250 N = 1,NFLD
          IF( IXP(N).EQ.0 ) GOTO 250
          IF( N.GT.NFLD ) THEN
            NF = INP(N-NFLD)
          ELSE
            NF = N
          ENDIF
          J = JD(N)
!
!---      Point 1 (i,j,k)  ---
!
          IVX = 8*(N-1) + 1
!
!---      South node  ---
!
          NS = ICM(1,2,N)
          IF( NS.GT.0 ) THEN
            IF( NS.GT.NFLD ) THEN
              NFS = INP(NS-NFLD)
            ELSE
              NFS = NS
            ENDIF
            IRX = IBR(1,NFS)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFS)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRX,IRZ)
            NS = ICM(NC,2,N)
            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              DVX = ABS(XE(1,N)-XE(3,NS)) +
     &           ABS(YE(1,N)-YE(3,NS)) + ABS(ZE(1,N)-ZE(3,NS))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NS-1) + 3 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 2 (i+1,j,k)  ---
!
          IVX = 8*(N-1) + 2
!
!---      South node  ---
!
          NS = ICM(1,2,N)
          IF( NS.GT.0 ) THEN
            IF( NS.GT.NFLD ) THEN
              NFS = INP(NS-NFLD)
            ELSE
              NFS = NS
            ENDIF
            IRX = IBR(1,NFS)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFS)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRX,IRZ)
            NS = ICM(NC,2,N)
            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              DVX = ABS(XE(2,N)-XE(4,NS)) +
     &           ABS(YE(2,N)-YE(4,NS)) + ABS(ZE(2,N)-ZE(4,NS))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NS-1) + 4 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 3 (i+1,j+1,k)  ---
!
          IVX = 8*(N-1) + 4
!
!---      North node  ---
!
          NN = ICM(1,5,N)
          IF( NN.GT.0 ) THEN
            IF( NN.GT.NFLD ) THEN
              NFN = INP(NN-NFLD)
            ELSE
              NFN = NN
            ENDIF
            IRX = IBR(1,NFN)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFN)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRX,IRZ)
            NN = ICM(NC,5,N)
            IF( IXP(NN).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              DVX = ABS(XE(4,N)-XE(2,NN)) +
     &           ABS(YE(4,N)-YE(2,NN)) + ABS(ZE(4,N)-ZE(2,NN))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NN-1) + 2 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 4 (i,j+1,k)  ---
!
          IVX = 8*(N-1) + 3
!
!---      North node  ---
!
          NN = ICM(1,5,N)
          IF( NN.GT.0 ) THEN
            IF( NN.GT.NFLD ) THEN
              NFN = INP(NN-NFLD)
            ELSE
              NFN = NN
            ENDIF
            IRX = IBR(1,NFN)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFN)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRX,IRZ)
            NN = ICM(NC,5,N)
            IF( IXP(NN).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              DVX = ABS(XE(3,N)-XE(1,NN)) +
     &           ABS(YE(3,N)-YE(1,NN)) + ABS(ZE(3,N)-ZE(1,NN))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NN-1) + 1 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 5 (i,j,k+1)  ---
!
          IVX = 8*(N-1) + 5
!
!---      South node  ---
!
          NS = ICM(1,2,N)
          IF( NS.GT.0 ) THEN
            IF( NS.GT.NFLD ) THEN
              NFS = INP(NS-NFLD)
            ELSE
              NFS = NS
            ENDIF
            IRX = IBR(1,NFS)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFS)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRX,IRZ)
            NS = ICM(NC,2,N)
            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              DVX = ABS(XE(5,N)-XE(7,NS)) +
     &           ABS(YE(5,N)-YE(7,NS)) + ABS(ZE(5,N)-ZE(7,NS))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NS-1) + 7 )
              ENDIF
            ENDIF
          ENDIF
!
!---      Point 6 (i+1,j,k+1)  ---
!
          IVX = 8*(N-1) + 6
!
!---      South node  ---
!
          NS = ICM(1,2,N)
          IF( NS.GT.0 ) THEN
            IF( NS.GT.NFLD ) THEN
              NFS = INP(NS-NFLD)
            ELSE
              NFS = NS
            ENDIF
            IRX = IBR(1,NFS)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFS)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRX,IRZ)
            NS = ICM(NC,2,N)
            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
              DVX = ABS(XE(6,N)-XE(8,NS)) +
     &           ABS(YE(6,N)-YE(8,NS)) + ABS(ZE(6,N)-ZE(8,NS))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NS-1) + 8 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 7 (i+1,j+1,k+1)  ---
!
          IVX = 8*(N-1) + 8
!
!---      North node  ---
!
          NN = ICM(1,5,N)
          IF( NN.GT.0 ) THEN
            IF( NN.GT.NFLD ) THEN
              NFN = INP(NN-NFLD)
            ELSE
              NFN = NN
            ENDIF
            IRX = IBR(1,NFN)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFN)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRX,IRZ)
            NN = ICM(NC,5,N)
            IF( IXP(NN).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              DVX = ABS(XE(8,N)-XE(6,NN)) +
     &           ABS(YE(8,N)-YE(6,NN)) + ABS(ZE(8,N)-ZE(6,NN))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NN-1) + 6 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 8 (i,j+1,k+1)  ---
!
          IVX = 8*(N-1) + 7
!
!---      North node  ---
!
          NN = ICM(1,5,N)
          IF( NN.GT.0 ) THEN
            IF( NN.GT.NFLD ) THEN
              NFN = INP(NN-NFLD)
            ELSE
              NFN = NN
            ENDIF
            IRX = IBR(1,NFN)/(IBR(1,NF)+1) + 1
            IRZ = IBR(3,NFN)/(IBR(3,NF)+1) + 1
            NC = NC3X(IRX,IRZ)
            NN = ICM(NC,5,N)
            IF( IXP(NN).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
              DVX = ABS(XE(7,N)-XE(5,NN)) +
     &           ABS(YE(7,N)-YE(5,NN)) + ABS(ZE(7,N)-ZE(5,NN))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NN-1) + 5 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9)') IVX
  250   CONTINUE
!
!---  Z domain ( Points 1,2,3,4,5,6,7,8 )  ---
!
      ELSEIF( JFLD.GT.1 ) THEN
        DO 260 N = 1,NFLD
          IF( IXP(N).EQ.0 ) GOTO 260
          IF( N.GT.NFLD ) THEN
            NF = INP(N-NFLD)
          ELSE
            NF = N
          ENDIF
          K = KD(N)
!
!---      Point 1 (i,j,k)  ---
!
          IVX = 8*(N-1) + 1
!
!---      Bottom node  ---
!
          NB = ICM(1,1,N)
          IF( NB.GT.0 ) THEN
            IF( NB.GT.NFLD ) THEN
              NFB = INP(NB-NFLD)
            ELSE
              NFB = NB
            ENDIF
            IRX = IBR(1,NFB)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFB)/(IBR(2,NF)+1) + 1
            NC = NC3X(IRX,IRY)
            NB = ICM(NC,1,N)
            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              DVX = ABS(XE(1,N)-XE(5,NB)) +
     &           ABS(YE(1,N)-YE(5,NB)) + ABS(ZE(1,N)-ZE(5,NB))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NB-1) + 5 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 2 (i+1,j,k)  ---
!
          IVX = 8*(N-1) + 2
!
!---      Bottom node  ---
!
          NB = ICM(1,1,N)
          IF( NB.GT.0 ) THEN
            IF( NB.GT.NFLD ) THEN
              NFB = INP(NB-NFLD)
            ELSE
              NFB = NB
            ENDIF
            IRX = IBR(1,NFB)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFB)/(IBR(2,NF)+1) + 1
            NC = NC3X(IRX,IRY)
            NB = ICM(NC,1,N)
            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              DVX = ABS(XE(2,N)-XE(6,NB)) +
     &           ABS(YE(2,N)-YE(6,NB)) + ABS(ZE(2,N)-ZE(6,NB))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NB-1) + 6 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 3 (i+1,j+1,k)  ---
!
          IVX = 8*(N-1) + 4
!
!---      Bottom node  ---
!
          NB = ICM(1,1,N)
          IF( NB.GT.0 ) THEN
            IF( NB.GT.NFLD ) THEN
              NFB = INP(NB-NFLD)
            ELSE
              NFB = NB
            ENDIF
            IRX = IBR(1,NFB)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFB)/(IBR(2,NF)+1) + 1
            NC = NC3X(IRX,IRY)
            NB = ICM(NC,1,N)
            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              DVX = ABS(XE(4,N)-XE(8,NB)) +
     &           ABS(YE(4,N)-YE(8,NB)) + ABS(ZE(4,N)-ZE(8,NB))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NB-1) + 8 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 4 (i,j+1,k)  ---
!
          IVX = 8*(N-1) + 3
!
!---      Bottom node  ---
!
          NB = ICM(1,1,N)
          IF( NB.GT.0 ) THEN
            IF( NB.GT.NFLD ) THEN
              NFB = INP(NB-NFLD)
            ELSE
              NFB = NB
            ENDIF
            IRX = IBR(1,NFB)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFB)/(IBR(2,NF)+1) + 1
            NC = NC3X(IRX,IRY)
            NB = ICM(NC,1,N)
            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
              DVX = ABS(XE(3,N)-XE(7,NB)) +
     &           ABS(YE(3,N)-YE(7,NB)) + ABS(ZE(3,N)-ZE(7,NB))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NB-1) + 7 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 5 (i,j,k+1)  ---
!
          IVX = 8*(N-1) + 5
!
!---      Top node  ---
!
          NT = ICM(1,6,N)
          IF( NT.GT.0 ) THEN
            IF( NT.GT.NFLD ) THEN
              NFT = INP(NT-NFLD)
            ELSE
              NFT = NT
            ENDIF
            IRX = IBR(1,NFT)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFT)/(IBR(2,NF)+1) + 1
            NC = NC0X(IRX,IRY)
            NT = ICM(NC,6,N)
            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              DVX = ABS(XE(5,N)-XE(1,NT)) +
     &           ABS(YE(5,N)-YE(1,NT)) + ABS(ZE(5,N)-ZE(1,NT))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NT-1) + 1 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 6 (i+1,j,k+1)  ---
!
          IVX = 8*(N-1) + 6
!
!---      Top node  ---
!
          NT = ICM(1,6,N)
          IF( NT.GT.0 ) THEN
            IF( NT.GT.NFLD ) THEN
              NFT = INP(NT-NFLD)
            ELSE
              NFT = NT
            ENDIF
            IRX = IBR(1,NFT)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFT)/(IBR(2,NF)+1) + 1
            NC = NC0X(IRX,IRY)
            NT = ICM(NC,6,N)
            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              DVX = ABS(XE(6,N)-XE(2,NT)) +
     &           ABS(YE(6,N)-YE(2,NT)) + ABS(ZE(6,N)-ZE(2,NT))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NT-1) + 2 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 7 (i+1,j+1,k+1)  ---
!
          IVX = 8*(N-1) + 8
!
!---      Top node  ---
!
          NT = ICM(1,6,N)
          IF( NT.GT.0 ) THEN
            IF( NT.GT.NFLD ) THEN
              NFT = INP(NT-NFLD)
            ELSE
              NFT = NT
            ENDIF
            IRX = IBR(1,NFT)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFT)/(IBR(2,NF)+1) + 1
            NC = NC0X(IRX,IRY)
            NT = ICM(NC,6,N)
            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              DVX = ABS(XE(8,N)-XE(4,NT)) +
     &           ABS(YE(8,N)-YE(4,NT)) + ABS(ZE(8,N)-ZE(4,NT))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NT-1) + 4 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9,1X,$)') IVX
!
!---      Point 8 (i,j+1,k+1)  ---
!
          IVX = 8*(N-1) + 7
!
!---      Top node  ---
!
          NT = ICM(1,6,N)
          IF( NT.GT.0 ) THEN
            IF( NT.GT.NFLD ) THEN
              NFT = INP(NT-NFLD)
            ELSE
              NFT = NT
            ENDIF
            IRX = IBR(1,NFT)/(IBR(1,NF)+1) + 1
            IRY = IBR(2,NFT)/(IBR(2,NF)+1) + 1
            NC = NC0X(IRX,IRY)
            NT = ICM(NC,6,N)
            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
              DVX = ABS(XE(7,N)-XE(3,NT)) +
     &           ABS(YE(7,N)-YE(3,NT)) + ABS(ZE(7,N)-ZE(3,NT))
              IF( DVX.LT.1.D-9 ) THEN
                IVX = MIN( IVX,8*(NT-1) + 3 )
              ENDIF
            ENDIF
          ENDIF
          WRITE(26,'(I9)') IVX
  260   CONTINUE
      ENDIF
!
!---  Close 'connect' file  ---
!
      CLOSE(UNIT=26)
!
!---  End of CONNLST group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CONNMAP
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
!     Node connection map
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 30 January 2014.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
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
      SUB_LOG(ISUB_LOG) = '/CONNMAP'
      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Bottom connections  ---
!
      DO 100 N = 1,NFLD
        I = ID(N)
        J = JD(N)
        K = KD(N)
!
!---    Bottom surface is an external boundary surface  ---
!
        IBX = 0
        IF( K.EQ.1 ) THEN
          IBX = 1
        ELSE
          NB = N-IJFLD
!
!---      Bottom surface is an internal boundary surface  ---
!
          IF( IXP(N).EQ.0 .OR. IXP(NB).EQ.0 .OR.
     &      INBS(1,N).NE.0 .OR. INBS(6,NB).NE.0 ) IBX = 1
          IF( IBR(4,N).GT.N .AND. IBX.EQ.0 ) ICM(1,1,N) = NB
          IF( IBX.EQ.0 ) ICM(5,1,N) = NB
        ENDIF
!
!---    Node refinement  ---
!
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 90 KX = 1,KRX
        IF( KX.EQ.1 .AND. IBX.EQ.1 ) CYCLE
        DO 80 JX = 1,JRX
        DO 70 IX = 1,IRX
          NX = IBR(4,N) + NDBR(IX,IRX,JX,JRX,KX)
          NC = 0
          IF( KX.EQ.1 ) THEN
!
!---        Bottom node refinement, in the x- and y-directions  ---
!
            IRBX = 2**IBR(1,NB)
            JRBX = 2**IBR(2,NB)
            KRBX = 2**IBR(3,NB)
            KBX = KRBX
            IF( JRBX.GT.JRX ) THEN
              J1X = 2*JX - 1
              J2X = 2*JX
            ELSEIF( JRBX.LT.JRX ) THEN
              J1X = (JX+1)/2
              J2X = (JX+1)/2
            ELSE
              J1X = JX
              J2X = JX
            ENDIF
            IF( IRBX.GT.IRX ) THEN
              I1X = 2*IX - 1
              I2X = 2*IX
            ELSEIF( IRBX.LT.IRX ) THEN
              I1X = (IX+1)/2
              I2X = (IX+1)/2
            ELSE
              I1X = IX
              I2X = IX
            ENDIF
            DO 60 JBX = J1X,J2X
            DO 50 IBX = I1X,I2X
              NRBX = IBR(4,NB) + NDBR(IBX,IRBX,JBX,JRBX,KBX)
              NC = NC+1
              ICM(NC,1,NX) = NRBX
   50       CONTINUE
   60       CONTINUE
          ELSE
            KBX = KX-1
            NRBX = IBR(4,N) + NDBR(IX,IRX,JX,JRX,KBX)
            NC = NC+1
            ICM(NC,1,NX) = NRBX
          ENDIF
   70   CONTINUE
   80   CONTINUE
   90   CONTINUE
  100 CONTINUE
!
!---  South connections  ---
!
      DO 200 N = 1,NFLD
        I = ID(N)
        J = JD(N)
        K = KD(N)
!
!---    South surface is an external boundary surface  ---
!
        IBX = 0
        IF( J.EQ.1 ) THEN
          IBX = 1
        ELSE
          NS = N-IFLD
!
!---      South surface is an internal boundary surface  ---
!
          IF( IXP(N).EQ.0 .OR. IXP(NS).EQ.0 .OR.
     &      INBS(2,N).NE.0 .OR. INBS(5,NS).NE.0 ) IBX = 1
          IF( IBR(4,N).GT.N .AND. IBX.EQ.0 ) ICM(1,2,N) = NS
          IF( IBX.EQ.0 ) ICM(5,2,N) = NS
        ENDIF
!
!---    Node refinement  ---
!
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 190 KX = 1,KRX
        DO 180 JX = 1,JRX
        IF( JX.EQ.1 .AND. IBX.EQ.1 ) CYCLE
        DO 170 IX = 1,IRX
          NX = IBR(4,N) + NDBR(IX,IRX,JX,JRX,KX)
          NC = 0
          IF( JX.EQ.1 ) THEN
!
!---        South node refinement, in the x- and y-directions  ---
!
            IRSX = 2**IBR(1,NS)
            JRSX = 2**IBR(2,NS)
            KRSX = 2**IBR(3,NS)
            JSX = JRSX
            IF( KRSX.GT.KRX ) THEN
              K1X = 2*KX - 1
              K2X = 2*KX
            ELSEIF( KRSX.LT.KRX ) THEN
              K1X = (KX+1)/2
              K2X = (KX+1)/2
            ELSE
              K1X = KX
              K2X = KX
            ENDIF
            IF( IRSX.GT.IRX ) THEN
              I1X = 2*IX - 1
              I2X = 2*IX
            ELSEIF( IRSX.LT.IRX ) THEN
              I1X = (IX+1)/2
              I2X = (IX+1)/2
            ELSE
              I1X = IX
              I2X = IX
            ENDIF
            DO 160 KSX = K1X,K2X
            DO 150 ISX = I1X,I2X
              NRSX = IBR(4,NS) + NDBR(ISX,IRSX,JSX,JRSX,KSX)
              NC = NC+1
              ICM(NC,2,NX) = NRSX
  150       CONTINUE
  160       CONTINUE
          ELSE
            JSX = JX-1
            NRSX = IBR(4,N) + NDBR(IX,IRX,JSX,JRX,KX)
            NC = NC+1
            ICM(NC,2,NX) = NRSX
          ENDIF
  170   CONTINUE
  180   CONTINUE
  190   CONTINUE
  200 CONTINUE
!
!---  West connections  ---
!
      DO 300 N = 1,NFLD
        I = ID(N)
        J = JD(N)
        K = KD(N)
!
!---    West surface is an external boundary surface  ---
!
        IBX = 0
        IF( I.EQ.1 ) THEN
          IBX = 1
        ELSE
          NW = N-1
!
!---      West surface is an internal boundary surface  ---
!
          IF( IXP(N).EQ.0 .OR. IXP(NW).EQ.0 .OR.
     &      INBS(3,N).NE.0 .OR. INBS(4,NW).NE.0 ) IBX = 1
          IF( IBR(4,N).GT.N .AND. IBX.EQ.0 ) ICM(1,3,N) = NW
          IF( IBX.EQ.0 ) ICM(5,3,N) = NW
        ENDIF
!
!---    Node refinement  ---
!
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 290 KX = 1,KRX
        DO 280 JX = 1,JRX
        DO 270 IX = 1,IRX
        IF( IX.EQ.1 .AND. IBX.EQ.1 ) CYCLE
          NX = IBR(4,N) + NDBR(IX,IRX,JX,JRX,KX)
          NC = 0
          IF( IX.EQ.1 ) THEN
!
!---        West node refinement, in the y- and z-directions  ---
!
            IRWX = 2**IBR(1,NW)
            JRWX = 2**IBR(2,NW)
            KRWX = 2**IBR(3,NW)
            IWX = IRWX
            IF( KRWX.GT.KRX ) THEN
              K1X = 2*KX - 1
              K2X = 2*KX
            ELSEIF( KRWX.LT.KRX ) THEN
              K1X = (KX+1)/2
              K2X = (KX+1)/2
            ELSE
              K1X = KX
              K2X = KX
            ENDIF
            IF( JRWX.GT.JRX ) THEN
              J1X = 2*JX - 1
              J2X = 2*JX
            ELSEIF( JRWX.LT.JRX ) THEN
              J1X = (JX+1)/2
              J2X = (JX+1)/2
            ELSE
              J1X = JX
              J2X = JX
            ENDIF
            DO 260 KWX = K1X,K2X
            DO 250 JWX = J1X,J2X
              NRWX = IBR(4,NW) + NDBR(IWX,IRWX,JWX,JRWX,KWX)
              NC = NC+1
              ICM(NC,3,NX) = NRWX
  250       CONTINUE
  260       CONTINUE
          ELSE
            IWX = IX-1
            NRWX = IBR(4,N) + NDBR(IWX,IRX,JX,JRX,KX)
            NC = NC+1
            ICM(NC,3,NX) = NRWX
          ENDIF
  270   CONTINUE
  280   CONTINUE
  290   CONTINUE
  300 CONTINUE
!
!---  East connections  ---
!
      DO 400 N = 1,NFLD
        I = ID(N)
        J = JD(N)
        K = KD(N)
!
!---    East surface is an external boundary surface  ---
!
        IBX = 0
        IF( I.EQ.IFLD ) THEN
          IBX = 1
        ELSE
          NE = N+1
!
!---      East surface is an internal boundary surface  ---
!
          IF( IXP(N).EQ.0 .OR. IXP(NE).EQ.0 .OR.
     &      INBS(4,N).NE.0 .OR. INBS(3,NE).NE.0 ) IBX = 1
          IF( IBR(4,N).GT.N .AND. IBX.EQ.0 ) ICM(1,4,N) = NE
          IF( IBX.EQ.0 ) ICM(5,4,N) = NE
        ENDIF
!
!---    Node refinement  ---
!
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 390 KX = 1,KRX
        DO 380 JX = 1,JRX
        DO 370 IX = 1,IRX
        IF( IX.EQ.IRX .AND. IBX.EQ.1 ) CYCLE
          NX = IBR(4,N) + NDBR(IX,IRX,JX,JRX,KX)
          NC = 0
          IF( IX.EQ.IRX ) THEN
!
!---        East node refinement, in the y- and z-directions  ---
!
            IREX = 2**IBR(1,NE)
            JREX = 2**IBR(2,NE)
            KREX = 2**IBR(3,NE)
            IEX = 1
            IF( KREX.GT.KRX ) THEN
              K1X = 2*KX - 1
              K2X = 2*KX
            ELSEIF( KREX.LT.KRX ) THEN
              K1X = (KX+1)/2
              K2X = (KX+1)/2
            ELSE
              K1X = KX
              K2X = KX
            ENDIF
            IF( JREX.GT.JRX ) THEN
              J1X = 2*JX - 1
              J2X = 2*JX
            ELSEIF( JREX.LT.JRX ) THEN
              J1X = (JX+1)/2
              J2X = (JX+1)/2
            ELSE
              J1X = JX
              J2X = JX
            ENDIF
            DO 360 KEX = K1X,K2X
            DO 350 JEX = J1X,J2X
              NREX = IBR(4,NE) + NDBR(IEX,IREX,JEX,JREX,KEX)
              NC = NC+1
              ICM(NC,4,NX) = NREX
  350       CONTINUE
  360       CONTINUE
          ELSE
            IEX = IX+1
            NREX = IBR(4,N) + NDBR(IEX,IRX,JX,JRX,KX)
            NC = NC+1
            ICM(NC,4,NX) = NREX
          ENDIF
  370    CONTINUE
  380   CONTINUE
  390   CONTINUE
  400 CONTINUE
!
!---  North connections  ---
!
      DO 500 N = 1,NFLD
        I = ID(N)
        J = JD(N)
        K = KD(N)
!
!---    North surface is an external boundary surface  ---
!
        IBX = 0
        IF( J.EQ.JFLD ) THEN
          IBX = 1
        ELSE
          NN = N+IFLD
!
!---      North surface is an internal boundary surface  ---
!
          IF( IXP(N).EQ.0 .OR. IXP(NN).EQ.0 .OR.
     &      INBS(5,N).NE.0 .OR. INBS(2,NN).NE.0 ) IBX = 1
          IF( IBR(4,N).GT.N .AND. IBX.EQ.0 ) ICM(1,5,N) = NN
          IF( IBX.EQ.0 ) ICM(5,5,N) = NN
        ENDIF
!
!---    Node refinement  ---
!
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 490 KX = 1,KRX
        DO 480 JX = 1,JRX
        IF( JX.EQ.JRX .AND. IBX.EQ.1 ) CYCLE
        DO 470 IX = 1,IRX
          NX = IBR(4,N) + NDBR(IX,IRX,JX,JRX,KX)
          NC = 0
          IF( JX.EQ.JRX ) THEN
!
!---        North node refinement, in the x- and y-directions  ---
!
            IRNX = 2**IBR(1,NN)
            JRNX = 2**IBR(2,NN)
            KRNX = 2**IBR(3,NN)
            JNX = 1
            IF( KRNX.GT.KRX ) THEN
              K1X = 2*KX - 1
              K2X = 2*KX
            ELSEIF( KRNX.LT.KRX ) THEN
              K1X = (KX+1)/2
              K2X = (KX+1)/2
            ELSE
              K1X = KX
              K2X = KX
            ENDIF
            IF( IRNX.GT.IRX ) THEN
              I1X = 2*IX - 1
              I2X = 2*IX
            ELSEIF( IRNX.LT.IRX ) THEN
              I1X = (IX+1)/2
              I2X = (IX+1)/2
            ELSE
              I1X = IX
              I2X = IX
            ENDIF
            DO 460 KNX = K1X,K2X
            DO 450 INX = I1X,I2X
              NRNX = IBR(4,NN) + NDBR(INX,IRNX,JNX,JRNX,KNX)
              NC = NC+1
              ICM(NC,5,NX) = NRNX
  450       CONTINUE
  460       CONTINUE
          ELSE
            JNX = JX+1
            NRNX = IBR(4,N) + NDBR(IX,IRX,JNX,JRX,KX)
            NC = NC+1
            ICM(NC,5,NX) = NRNX
          ENDIF
  470   CONTINUE
  480   CONTINUE
  490   CONTINUE
  500 CONTINUE
!
!---  Top connections  ---
!
      DO 600 N = 1,NFLD
        I = ID(N)
        J = JD(N)
        K = KD(N)
!
!---    Top surface is an external boundary surface  ---
!
        IBX = 0
        IF( K.EQ.KFLD ) THEN
          IBX = 1
        ELSE
          NT = N+IJFLD
!
!---      Top surface is an internal boundary surface  ---
!
          IF( IXP(N).EQ.0 .OR. IXP(NT).EQ.0 .OR.
     &      INBS(6,N).NE.0 .OR. INBS(1,NT).NE.0 ) IBX = 1
          IF( IBR(4,N).GT.N .AND. IBX.EQ.0 ) ICM(1,6,N) = NT
          IF( IBX.EQ.0 ) ICM(5,6,N) = NT
        ENDIF
!
!---    Node refinement  ---
!
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 590 KX = 1,KRX
        IF( KX.EQ.KRX .AND. IBX.EQ.1 ) CYCLE
        DO 580 JX = 1,JRX
        DO 570 IX = 1,IRX
          NX = IBR(4,N) + NDBR(IX,IRX,JX,JRX,KX)
          NC = 0
          IF( KX.EQ.KRX ) THEN
!
!---        Top node refinement, in the x- and y-directions  ---
!
            IRTX = 2**IBR(1,NT)
            JRTX = 2**IBR(2,NT)
            KRTX = 2**IBR(3,NT)
            KTX = 1
            NC = 0
            IF( JRTX.GT.JRX ) THEN
              J1X = 2*JX - 1
              J2X = 2*JX
            ELSEIF( JRTX.LT.JRX ) THEN
              J1X = (JX+1)/2
              J2X = (JX+1)/2
            ELSE
              J1X = JX
              J2X = JX
            ENDIF
            IF( IRTX.GT.IRX ) THEN
              I1X = 2*IX - 1
              I2X = 2*IX
            ELSEIF( IRTX.LT.IRX ) THEN
              I1X = (IX+1)/2
              I2X = (IX+1)/2
            ELSE
              I1X = IX
              I2X = IX
            ENDIF
            DO 560 JTX = J1X,J2X
            DO 550 ITX = I1X,I2X
              NRTX = IBR(4,NT) + NDBR(ITX,IRTX,JTX,JRTX,KTX)
              NC = NC+1
              ICM(NC,6,NX) = NRTX
  550       CONTINUE
  560       CONTINUE
          ELSE
            KTX = KX+1
            NRTX = IBR(4,N) + NDBR(IX,IRX,JX,JRX,KTX)
            NC = NC+1
            ICM(NC,6,NX) = NRTX
          ENDIF
  570   CONTINUE
  580   CONTINUE
  590   CONTINUE
  600 CONTINUE
!
!---  Face neighbor connections  ---
!
      NFNC = 0
!
!---  Maximum number of connections  ---
!
      NCMX = 0
      DO 800 N = 1,NFLD
        I = ID(N)
        J = JD(N)
        K = KD(N)
        IRX = 2**IBR(1,N)
        JRX = 2**IBR(2,N)
        KRX = 2**IBR(3,N)
        DO 790 KX = 1,KRX
        DO 780 JX = 1,JRX
        DO 770 IX = 1,IRX
          NX = IBR(4,N) + NDBR(IX,IRX,JX,JRX,KX)
          NCX = 0
          DO 760 NR = 1,4
            DO 750 NS = 1,6
              IF( ICM(NR,NS,NX).NE.0 ) THEN
                NCX = NCX + 1
                NFNC = NFNC + 1
              ENDIF
  750       CONTINUE
  760     CONTINUE
          NCMX = MAX( NCMX,NCX )
  770   CONTINUE
  780   CONTINUE
  790   CONTINUE
  800 CONTINUE
!
!---  Differential lengths, gravitational components,
!     and adjust block refinement areas for surface normal  ---
!
      IF( NFBN.GT.NFLD ) CALL AVBR2
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of CONNMAP group
!
      RETURN
      END

!!----------------------Subroutine--------------------------------------!
!!
!      SUBROUTINE CONNLST
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
!!     Write Tecplot connectivity list for finite element grid
!!
!!----------------------Authors-----------------------------------------!
!!
!!     Written by MD White, PNNL, 26 January 2012.
!!
!#ifdef 1
!!----------------------Fortran 90 Modules------------------------------!
!!
!      USE GLB_PAR
!      USE SOLTN
!      USE GRID
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
!!----------------------Executable Lines--------------------------------!
!!
!      ISUB_LOG = ISUB_LOG+1
!      SUB_LOG(ISUB_LOG) = '/CONNLST'
!      IF( INDEX(SVN_ID(277)(1:1),'$').EQ.0 ) SVN_ID(277) =
!     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!      ISUB_LOG = ISUB_LOG-1
!!
!!---  Open 'connect' file for writing  ---
!!
!      OPEN(UNIT=26, FILE='connect', STATUS='UNKNOWN', FORM='FORMATTED')
!      CLOSE(UNIT=26,STATUS='DELETE')
!      OPEN(UNIT=26, FILE='connect', STATUS='NEW', FORM='FORMATTED')
!!
!!---  XYZ domain ( Points 1,2,3,4,5,6,7,8 )  ---
!!
!      IF( (IFLD.GT.1 .AND. JFLD.GT.1 .AND. KFLD.GT.1) .OR. 
!     &  (ICS.EQ.3) .OR. (ISLC(63).EQ.1) ) THEN
!        DO 200 N = 1,NFLD
!          IF( IXP(N).EQ.0 ) GOTO 200
!          I = ID(N)
!          J = JD(N)
!          K = KD(N)
!!
!!---      Point 1 (i,j,k)  ---
!!
!          IVX = 8*(N-1) + 1
!!
!!---      South node and surface  ---
!!
!          NS = N-IFLD
!          IF( J.GT.1 ) THEN
!            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NS-1) + 3 )
!            ENDIF
!          ENDIF
!!
!!---      West node and surface  ---
!!
!          NW = N-1
!          IF( I.GT.1 ) THEN
!            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NW-1) + 2 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom node and surface  ---
!!
!          NB = N-IJFLD
!          IF( K.GT.1 ) THEN
!            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NB-1) + 5 )
!            ENDIF
!          ENDIF
!!
!!---      South-west node  ---
!!
!          NSW = NS-1
!          IF( I.GT.1 .AND. J.GT.1 ) THEN
!            IF( IXP(NSW).NE.0 .AND. 
!     &        INBS(3,NS).EQ.0 .AND. INBS(2,NW).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NSW-1) + 4 )
!               ENDIF
!              ENDIF
!!
!!---      Bottom-south node  ---
!!
!          NBS = NB-IFLD
!          IF( K.GT.1 .AND. J.GT.1 ) THEN
!            IF( IXP(NBS).NE.0 .AND. 
!     &        INBS(2,NB).EQ.0 .AND. INBS(1,NS).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NBS-1) + 7 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom-west node  ---
!!
!          NBW = NB-1
!          IF( K.GT.1 .AND. I.GT.1 ) THEN
!            IF( IXP(NBW).NE.0 .AND. 
!     &        INBS(3,NB).EQ.0 .AND. INBS(1,NW).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NBW-1) + 6 )
!              ENDIF
!          ENDIF
!!
!!---      Bottom-south-west node  ---
!!
!          NBSW = NBS-1
!          IF( K.GT.1 .AND. J.GT.1 .AND. I.GT.1 ) THEN
!            IF( IXP(NBSW).NE.0 .AND. INBS(1,NSW).EQ.0 .AND.
!     &        INBS(3,NBS).EQ.0 .AND. INBS(2,NBW).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NBSW-1) + 8 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 2 (i+1,j,k)  ---
!!
!          IVX = 8*(N-1) + 2
!!
!!---      South node and surface  ---
!!
!          NS = N-IFLD
!          IF( J.GT.1 ) THEN
!            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NS-1) + 4 )
!            ENDIF
!          ENDIF
!!
!!---      East node and surface  ---
!!
!          NE = N+1
!          IF( I.LT.IFLD ) THEN
!            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NE-1) + 1 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom node and surface  ---
!!
!          NB = N-IJFLD
!          IF( K.GT.1 ) THEN
!            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NB-1) + 6 )
!            ENDIF
!          ENDIF
!!
!!---      South-east node  ---
!!
!          NSE = NS+1
!          IF( J.GT.1 .AND. I.LT.IFLD ) THEN
!            IF( IXP(NSE).NE.0 .AND. 
!     &        INBS(4,NS).EQ.0 .AND. INBS(2,NE).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NSE-1) + 3 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom-south node  ---
!!
!          NBS = NB-IFLD
!          IF( K.GT.1 .AND. J.GT.1 ) THEN
!            IF( IXP(NBS).NE.0 .AND. 
!     &        INBS(2,NB).EQ.0 .AND. INBS(1,NS).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NBS-1) + 8 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom-east node  ---
!!
!          NBE = NB+1
!          IF( K.GT.1 .AND. I.LT.IFLD ) THEN
!            IF( IXP(NBE).NE.0 .AND. 
!     &        INBS(4,NB).EQ.0 .AND. INBS(1,NE).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NBE-1) + 5 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom-south-east node  ---
!!
!          NBSE = NBS+1
!          IF( K.GT.1 .AND. J.GT.1 .AND. I.LT.IFLD ) THEN
!            IF( IXP(NBSE).NE.0 .AND. INBS(1,NSE).EQ.0 .AND.
!     &        INBS(4,NBS).EQ.0 .AND. INBS(2,NBE).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NBSE-1) + 7 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 3 (i+1,j+1,k)  ---
!!
!          IVX = 8*(N-1) + 4
!!
!!---      North node and surface  ---
!!
!          NN = N+IFLD
!          IF( J.LT.JFLD ) THEN
!            IF( IXP(NN).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NN-1) + 2 )
!            ENDIF
!          ENDIF
!!
!!---      East node and surface  ---
!!
!          NE = N+1
!          IF( I.LT.IFLD ) THEN
!            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NE-1) + 3 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom node and surface  ---
!!
!          NB = N-IJFLD
!          IF( K.GT.1 ) THEN
!            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NB-1) + 8 )
!            ENDIF
!          ENDIF
!!
!!---      North-east node  ---
!!
!          NNE = NN+1
!          IF( J.LT.JFLD .AND. I.LT.IFLD ) THEN
!            IF( IXP(NNE).NE.0 .AND.
!     &        INBS(4,NN).EQ.0 .AND. INBS(5,NE).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NNE-1) + 1 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom-north node  ---
!!
!          NBN = NB+IFLD
!          IF( K.GT.1 .AND. J.LT.JFLD ) THEN
!            IF( IXP(NBN).NE.0 .AND.
!     &        INBS(5,NB).EQ.0 .AND. INBS(1,NN).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NBN-1) + 6 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom-east node  ---
!!
!          NBE = NB+1
!          IF( K.GT.1 .AND. I.LT.IFLD ) THEN
!            IF( IXP(NBE).NE.0 .AND.
!     &        INBS(4,NB).EQ.0 .AND. INBS(1,NE).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NBE-1) + 7 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom-north-east node  ---
!!
!          NBNE = NBN+1
!          IF( K.GT.1 .AND. J.LT.JFLD .AND. I.LT.IFLD ) THEN
!            IF( IXP(NBNE).NE.0 .AND. INBS(1,NNE).EQ.0 .AND.
!     &        INBS(4,NBN).EQ.0 .AND. INBS(5,NBE).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NBNE-1) + 5 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 4 (i,j+1,k)  ---
!!
!          IVX = 8*(N-1) + 3
!!
!!---      North node and surface  ---
!!
!          NN = N+IFLD
!          IF( J.LT.JFLD ) THEN
!            IF( IXP(NN).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NN-1) + 1 )
!            ENDIF
!          ENDIF
!!
!!---      West node and surface  ---
!!
!          NW = N-1
!          IF( I.GT.1 ) THEN
!            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NW-1) + 4 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom node and surface  ---
!!
!          NB = N-IJFLD
!          IF( K.GT.1 ) THEN
!            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NB-1) + 7 )
!            ENDIF
!          ENDIF
!!
!!---      North-west node  ---
!!
!          NNW = NN-1
!          IF( J.LT.JFLD .AND. I.GT.1 ) THEN
!            IF( IXP(NNW).NE.0 .AND.
!     &        INBS(3,NN).EQ.0 .AND. INBS(5,NW).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NNW-1) + 2 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom-north node  ---
!!
!          NBN = NB+IFLD
!          IF( K.GT.1 .AND. J.LT.JFLD ) THEN
!            IF( IXP(NBN).NE.0 .AND.
!     &        INBS(5,NB).EQ.0 .AND. INBS(1,NN).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NBN-1) + 5 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom-west node  ---
!!
!          NBW = NB-1
!          IF( K.GT.1 .AND. I.GT.1 ) THEN
!            IF( IXP(NBW).NE.0 .AND.
!     &        INBS(3,NB).EQ.0 .AND. INBS(1,NW).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NBW-1) + 8 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom-north-west node  ---
!!
!          NBNW = NBN-1
!          IF( K.GT.1 .AND. J.LT.JFLD .AND. I.GT.1 ) THEN
!            IF( IXP(NBNW).NE.0 .AND. INBS(1,NNW).EQ.0 .AND.
!     &        INBS(3,NBN).EQ.0 .AND. INBS(5,NBW).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NBNW-1) + 6 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 5 (i,j,k+1)  ---
!!
!          IVX = 8*(N-1) + 5
!!
!!---      South node and surface  ---
!!
!          NS = N-IFLD
!          IF( J.GT.1 ) THEN
!            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NS-1) + 7 )
!            ENDIF
!          ENDIF
!!
!!---      West node and surface  ---
!!
!          NW = N-1
!          IF( I.GT.1 ) THEN
!            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NW-1) + 6 )
!            ENDIF
!          ENDIF
!!
!!---      Top node and surface  ---
!!
!          NT = N+IJFLD
!          IF( K.LT.KFLD ) THEN
!            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NT-1) + 1 )
!            ENDIF
!          ENDIF
!!
!!---      South-west node  ---
!!
!          NSW = NS-1
!          IF( I.GT.1 .AND. J.GT.1 ) THEN
!            IF( IXP(NSW).NE.0 .AND. 
!     &        INBS(3,NS).EQ.0 .AND. INBS(2,NW).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NSW-1) + 8 )
!            ENDIF
!          ENDIF
!!
!!---      Top-south node  ---
!!
!          NTS = NT-IFLD
!          IF( K.LT.KFLD .AND. J.GT.1 ) THEN
!            IF( IXP(NTS).NE.0 .AND.
!     &        INBS(2,NT).EQ.0 .AND. INBS(6,NS).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NTS-1) + 3 )
!            ENDIF
!          ENDIF
!!
!!---      Top-west node  ---
!!
!          NTW = NT-1
!          IF( K.LT.KFLD .AND. I.GT.1 ) THEN
!            IF( IXP(NTW).NE.0 .AND.
!     &        INBS(3,NT).EQ.0 .AND. INBS(6,NW).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NTW-1) + 2 )
!            ENDIF
!          ENDIF
!!
!!---      Top-south-west node  ---
!!
!          NTSW = NTS-1
!          IF( K.LT.KFLD .AND. J.GT.1 .AND. I.GT.1 ) THEN
!            IF( IXP(NTSW).NE.0 .AND. INBS(6,NSW).EQ.0 .AND.
!     &        INBS(3,NTS).EQ.0 .AND. INBS(2,NTW).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NTSW-1) + 4 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 6 (i+1,j,k+1)  ---
!!
!          IVX = 8*(N-1) + 6
!!
!!---      South node and surface  ---
!!
!          NS = N-IFLD
!          IF( J.GT.1 ) THEN
!            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NS-1) + 8 )
!            ENDIF
!          ENDIF
!!
!!---      East node and surface  ---
!!
!          NE = N+1
!          IF( I.LT.IFLD ) THEN
!            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NE-1) + 5 )
!            ENDIF
!          ENDIF
!!
!!---      Top node and surface  ---
!!
!          NT = N+IJFLD
!          IF( K.LT.KFLD ) THEN
!            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NT-1) + 2 )
!            ENDIF
!          ENDIF
!!
!!---      South-east node  ---
!!
!          NSE = NS+1
!          IF( J.GT.1 .AND. I.LT.IFLD ) THEN
!            IF( IXP(NSE).NE.0 .AND.
!     &        INBS(4,NS).EQ.0 .AND. INBS(2,NE).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NSE-1) + 7 )
!            ENDIF
!          ENDIF
!!
!!---      Top-south node  ---
!!
!          NTS = NT-IFLD
!          IF( K.LT.KFLD .AND. J.GT.1 ) THEN
!            IF( IXP(NTS).NE.0 .AND.
!     &        INBS(2,NT).EQ.0 .AND. INBS(6,NS).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NTS-1) + 4 )
!            ENDIF
!          ENDIF
!!
!!---      Top-east node  ---
!!
!          NTE = NT+1
!          IF( K.LT.KFLD .AND. I.LT.IFLD ) THEN
!            IF( IXP(NTE).NE.0 .AND.
!     &        INBS(4,NT).EQ.0 .AND. INBS(6,NE).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NTE-1) + 1 )
!            ENDIF
!          ENDIF
!!
!!---      Top-south-east node  ---
!!
!          NTSE = NTS+1
!          IF( K.LT.KFLD .AND. J.GT.1 .AND. I.LT.IFLD ) THEN
!            IF( IXP(NTSE).NE.0 .AND. INBS(6,NSE).EQ.0 .AND.
!     &        INBS(4,NTS).EQ.0 .AND. INBS(2,NTE).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NTSE-1) + 3 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 7 (i+1,j+1,k+1)  ---
!!
!          IVX = 8*(N-1) + 8
!!
!!---      North node and surface  ---
!!
!          NN = N+IFLD
!          IF( J.LT.JFLD ) THEN
!            IF( IXP(NN).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NN-1) + 6 )
!            ENDIF
!          ENDIF
!!
!!---      East node and surface  ---
!!
!          NE = N+1
!          IF( I.LT.IFLD ) THEN
!            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NE-1) + 7 )
!            ENDIF
!          ENDIF
!!
!!---      Top node and surface  ---
!!
!          NT = N+IJFLD
!          IF( K.LT.KFLD ) THEN
!            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NT-1) + 4 )
!            ENDIF
!          ENDIF
!!
!!---      North-east node  ---
!!
!          NNE = NN+1
!          IF( J.LT.JFLD .AND. I.LT.IFLD ) THEN
!            IF( IXP(NNE).NE.0 .AND.
!     &        INBS(4,NN).EQ.0 .AND. INBS(5,NE).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NNE-1) + 5 )
!            ENDIF
!          ENDIF
!!
!!---      Top-north node  ---
!!
!          NTN = NT+IFLD
!          IF( K.LT.KFLD .AND. J.LT.JFLD ) THEN
!            IF( IXP(NTN).NE.0 .AND.
!     &        INBS(5,NT).EQ.0 .AND. INBS(6,NN).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NTN-1) + 2 )
!            ENDIF
!          ENDIF
!!
!!---      Top-east node  ---
!!
!          NTE = NT+1
!          IF( K.LT.KFLD .AND. I.LT.IFLD ) THEN
!            IF( IXP(NTE).NE.0 .AND.
!     &        INBS(4,NT).EQ.0 .AND. INBS(6,NE).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NTE-1) + 3 )
!            ENDIF
!          ENDIF
!!
!!---      Top-north-east node  ---
!!
!          NTNE = NTN+1
!          IF( K.LT.KFLD .AND. J.LT.JFLD .AND. I.LT.IFLD ) THEN
!            IF( IXP(NTNE).NE.0 .AND. INBS(6,NNE).EQ.0 .AND.
!     &        INBS(4,NTN).EQ.0 .AND. INBS(5,NTE).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NTNE-1) + 1 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 8 (i,j+1,k+1)  ---
!!
!          IVX = 8*(N-1) + 7
!!
!!---      North node and surface  ---
!!
!          NN = N+IFLD
!          IF( J.LT.JFLD ) THEN
!            IF( IXP(NN).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NN-1) + 5 )
!            ENDIF
!          ENDIF
!!
!!---      West node and surface  ---
!!
!          NW = N-1
!          IF( I.GT.1 ) THEN
!            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NW-1) + 8 )
!            ENDIF
!          ENDIF
!!
!!---      Top node and surface  ---
!!
!          NT = N+IJFLD
!          IF( K.LT.KFLD ) THEN
!            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NT-1) + 3 )
!            ENDIF
!          ENDIF
!!
!!---      North-west node  ---
!!
!          NNW = NN-1
!          IF( J.LT.JFLD .AND. I.GT.1 ) THEN
!            IF( IXP(NNW).NE.0 .AND.
!     &        INBS(3,NN).EQ.0 .AND. INBS(5,NW).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NNW-1) + 6 )
!            ENDIF
!          ENDIF
!!
!!---      Top-north node  ---
!!
!          NTN = NT+IFLD
!          IF( K.LT.KFLD .AND. J.LT.JFLD ) THEN
!            IF( IXP(NTN).NE.0 .AND.
!     &        INBS(5,NT).EQ.0 .AND. INBS(6,NN).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NTN-1) + 1 )
!            ENDIF
!          ENDIF
!!
!!---      Top-west node  ---
!!
!          NTW = NT-1
!          IF( K.LT.KFLD .AND. I.GT.1 ) THEN
!            IF( IXP(NTW).NE.0 .AND.
!     &        INBS(3,NT).EQ.0 .AND. INBS(6,NW).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NTW-1) + 4 )
!            ENDIF
!          ENDIF
!!
!!---      Top-north-west node  ---
!!
!          NTNW = NTN-1
!          IF( K.LT.KFLD .AND. J.LT.JFLD .AND. I.GT.1 ) THEN
!            IF( IXP(NTNW).NE.0 .AND. INBS(6,NNW).EQ.0 .AND.
!     &        INBS(3,NTN).EQ.0 .AND. INBS(5,NTW).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NTNW-1) + 2 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9)') IVX
!  200   CONTINUE
!!
!!---  XY domain ( Points 1,2,4,3 )
!!
!      ELSEIF( IFLD.GT.1 .AND. JFLD.GT.1 ) THEN
!        DO 210 N = 1,NFLD
!          IF( IXP(N).EQ.0 ) GOTO 210
!          I = ID(N)
!          J = JD(N)
!!
!!---      Point 1 (i,j,k)  ---
!!
!          IVX = 4*(N-1) + 1
!!
!!---      West node and surface  ---
!!
!          NW = N-1
!          IF( I.GT.1 ) THEN
!            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NW-1) + 2 )
!            ENDIF
!          ENDIF
!!
!!---      South node and surface  ---
!!
!          NS = N-IFLD
!          IF( J.GT.1 ) THEN
!            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NS-1) + 3 )
!            ENDIF
!          ENDIF
!!
!!---      South-west node  ---
!!
!          NSW = NS-1
!          IF( I.GT.1 .AND. J.GT.1 ) THEN
!            IF( IXP(NSW).NE.0 .AND. 
!     &        INBS(3,NS).EQ.0 .AND. INBS(2,NW).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NSW-1) + 4 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 2 (i+1,j,k)  ---
!!
!          IVX = 4*(N-1) + 2
!!
!!---      East node and surface  ---
!!
!          NE = N+1
!          IF( I.LT.IFLD ) THEN
!            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NE-1) + 1 )
!            ENDIF
!          ENDIF
!!
!!---      South node and surface  ---
!!
!          NS = N-IFLD
!          IF( J.GT.1 ) THEN
!            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NS-1) + 4 )
!            ENDIF
!          ENDIF
!!
!!---      South-east node  ---
!!
!          NSE = NS+1
!          IF( J.GT.1 .AND. I.LT.IFLD ) THEN
!            IF( IXP(NSE).NE.0 .AND. 
!     &        INBS(4,NS).EQ.0 .AND. INBS(2,NE).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NSE-1) + 3 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 4 (i+1,j+1,k)  ---
!!
!          IVX = 4*(N-1) + 4
!!
!!---      East node and surface  ---
!!
!          NE = N+1
!          IF( I.LT.IFLD ) THEN
!            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NE-1) + 3 )
!            ENDIF
!          ENDIF
!!
!!---      North node and surface  ---
!!
!          NN = N+IFLD
!          IF( J.LT.JFLD ) THEN
!            IF( IXP(NN).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NN-1) + 2 )
!            ENDIF
!          ENDIF
!!
!!---      North-east node  ---
!!
!          NNE = NN+1
!          IF( J.LT.JFLD .AND. I.LT.IFLD ) THEN
!            IF( IXP(NNE).NE.0 .AND.
!     &        INBS(4,NN).EQ.0 .AND. INBS(5,NE).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NNE-1) + 1 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 3 (i,j+1,k)  ---
!!
!          IVX = 4*(N-1) + 3
!!
!!---      West node and surface  ---
!!
!          NW = N-1
!          IF( I.GT.1 ) THEN
!            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NW-1) + 4 )
!            ENDIF
!          ENDIF
!!
!!---      North node and surface  ---
!!
!          NN = N+IFLD
!          IF( J.LT.JFLD ) THEN
!            IF( IXP(NN).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NN-1) + 1 )
!            ENDIF
!          ENDIF
!!
!!---      North-west node  ---
!!
!          NNW = NN-1
!          IF( J.LT.JFLD .AND. I.GT.1 ) THEN
!            IF( IXP(NNW).NE.0 .AND.
!     &        INBS(3,NN).EQ.0 .AND. INBS(5,NW).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NNW-1) + 2 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9)') IVX
!  210   CONTINUE
!!
!!---  YZ domain ( Points 1,2,4,3 )  ---
!!
!      ELSEIF( JFLD.GT.1 .AND. KFLD.GT.1 ) THEN
!        DO 220 N = 1,NFLD
!          IF( IXP(N).EQ.0 ) GOTO 220
!          J = JD(N)
!          K = KD(N)
!!
!!---      Point 1 (i,j,k)  ---
!!
!          IVX = 4*(N-1) + 1
!!
!!---      South node and surface  ---
!!
!          NS = N-IFLD
!          IF( J.GT.1 ) THEN
!            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NS-1) + 2 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom node and surface  ---
!!
!          NB = N-IJFLD
!          IF( K.GT.1 ) THEN
!            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NB-1) + 3 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom-south node  ---
!!
!          NBS = NB-IFLD
!          IF( K.GT.1 .AND. J.GT.1 ) THEN
!            IF( IXP(NBS).NE.0 .AND. 
!     &        INBS(2,NB).EQ.0 .AND. INBS(1,NS).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NBS-1) + 4 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 2 (i,j+1,k)  ---
!!
!          IVX = 4*(N-1) + 2
!!
!!---      North node and surface  ---
!!
!          NN = N+IFLD
!          IF( J.LT.JFLD ) THEN
!            IF( IXP(NN).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NN-1) + 1 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom node and surface  ---
!!
!          NB = N-IJFLD
!          IF( K.GT.1 ) THEN
!            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NB-1) + 4 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom-north node  ---
!!
!          NBN = NB+IFLD
!          IF( K.GT.1 .AND. J.LT.JFLD ) THEN
!            IF( IXP(NBN).NE.0 .AND.
!     &        INBS(5,NB).EQ.0 .AND. INBS(1,NN).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NBN-1) + 3 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 4 (i,j+1,k+1)  ---
!!
!          IVX = 4*(N-1) + 4
!!
!!---      North node and surface  ---
!!
!          NN = N+IFLD
!          IF( J.LT.JFLD ) THEN
!            IF( IXP(NN).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NN-1) + 3 )
!            ENDIF
!          ENDIF
!!
!!---      Top node and surface  ---
!!
!          NT = N+IJFLD
!          IF( K.LT.KFLD ) THEN
!            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NT-1) + 2 )
!            ENDIF
!          ENDIF
!!
!!---      Top-north node  ---
!!
!          NTN = NT+IFLD
!          IF( K.LT.KFLD .AND. J.LT.JFLD ) THEN
!            IF( IXP(NTN).NE.0 .AND.
!     &        INBS(5,NT).EQ.0 .AND. INBS(6,NN).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NTN-1) + 1 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 3 (i,j,k+1)  ---
!!
!          IVX = 4*(N-1) + 3
!!
!!---      South node and surface  ---
!!
!          NS = N-IFLD
!          IF( J.GT.1 ) THEN
!            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NS-1) + 4 )
!            ENDIF
!          ENDIF
!!
!!---      Top node and surface  ---
!!
!          NT = N+IJFLD
!          IF( K.LT.KFLD ) THEN
!            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NT-1) + 1 )
!            ENDIF
!          ENDIF
!!
!!---      Top-south node  ---
!!
!          NTS = NT-IFLD
!          IF( K.LT.KFLD .AND. J.GT.1 ) THEN
!            IF( IXP(NTS).NE.0 .AND.
!     &        INBS(2,NT).EQ.0 .AND. INBS(6,NS).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NTS-1) + 2 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9)') IVX
!  220   CONTINUE
!!
!!---  XZ domain ( Points 1,2,4,3 )  ---
!!
!      ELSEIF( KFLD.GT.1 .AND. IFLD.GT.1 ) THEN
!        DO 230 N = 1,NFLD
!          IF( IXP(N).EQ.0 ) GOTO 230
!          I = ID(N)
!          K = KD(N)
!!
!!---      Point 1 (i,j,k)  ---
!!
!          IVX = 4*(N-1) + 1
!!
!!---      West node and surface  ---
!!
!          NW = N-1
!          IF( I.GT.1 ) THEN
!            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NW-1) + 2 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom node and surface  ---
!!
!          NB = N-IJFLD
!          IF( K.GT.1 ) THEN
!            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NB-1) + 3 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom-west node  ---
!!
!          NBW = NB-1
!          IF( K.GT.1 .AND. I.GT.1 ) THEN
!            IF( IXP(NBW).NE.0 .AND. 
!     &        INBS(3,NB).EQ.0 .AND. INBS(1,NW).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NBW-1) + 4 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 2 (i+1,j,k)  ---
!!
!          IVX = 4*(N-1) + 2
!!
!!---      East node and surface  ---
!!
!          NE = N+1
!          IF( I.LT.IFLD ) THEN
!            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NE-1) + 1 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom node and surface  ---
!!
!          NB = N-IJFLD
!          IF( K.GT.1 ) THEN
!            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NB-1) + 4 )
!            ENDIF
!          ENDIF
!!
!!---      Bottom-east node  ---
!!
!          NBE = NB+1
!          IF( K.GT.1 .AND. I.LT.IFLD ) THEN
!            IF( IXP(NBE).NE.0 .AND. 
!     &        INBS(4,NB).EQ.0 .AND. INBS(1,NE).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NBE-1) + 3 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 4 (i+1,j,k+1)  ---
!!
!          IVX = 4*(N-1) + 4
!!
!!---      East node and surface  ---
!!
!          NE = N+1
!          IF( I.LT.IFLD ) THEN
!            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NE-1) + 3 )
!            ENDIF
!          ENDIF
!!
!!---      Top node and surface  ---
!!
!          NT = N+IJFLD
!          IF( K.LT.KFLD ) THEN
!            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NT-1) + 2 )
!            ENDIF
!          ENDIF
!!
!!---      Top-east node  ---
!!
!          NTE = NT+1
!          IF( K.LT.KFLD .AND. I.LT.IFLD ) THEN
!            IF( IXP(NTE).NE.0 .AND.
!     &        INBS(4,NT).EQ.0 .AND. INBS(6,NE).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NTE-1) + 1 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 3 (i,j,k+1)  ---
!!
!          IVX = 4*(N-1) + 3
!!
!!---      West node and surface  ---
!!
!          NW = N-1
!          IF( I.GT.1 ) THEN
!            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NW-1) + 4 )
!            ENDIF
!          ENDIF
!!
!!---      Top node and surface  ---
!!
!          NT = N+IJFLD
!          IF( K.LT.KFLD ) THEN
!            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NT-1) + 1 )
!            ENDIF
!          ENDIF
!!
!!---      Top-west node  ---
!!
!          NTW = NT-1
!          IF( K.LT.KFLD .AND. I.GT.1 ) THEN
!            IF( IXP(NTW).NE.0 .AND.
!     &        INBS(3,NT).EQ.0 .AND. INBS(6,NW).EQ.0 ) THEN
!              IVX = MIN( IVX,4*(NTW-1) + 2 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9)') IVX
!  230   CONTINUE
!!
!!---  X domain ( Points 1,2,3,4,5,6,7,8 )  ---
!!
!      ELSEIF( IFLD.GT.1 ) THEN
!        DO 240 N = 1,NFLD
!          IF( IXP(N).EQ.0 ) GOTO 240
!          I = ID(N)
!!
!!---      Point 1 (i,j,k)  ---
!!
!          IVX = 8*(N-1) + 1
!!
!!---      West node and surface  ---
!!
!          NW = N-1
!          IF( I.GT.1 ) THEN
!            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NW-1) + 2 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 2 (i+1,j,k)  ---
!!
!          IVX = 8*(N-1) + 2
!!
!!---      East node and surface  ---
!!
!          NE = N+1
!          IF( I.LT.IFLD ) THEN
!            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NE-1) + 1 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 3 (i+1,j+1,k)  ---
!!
!          IVX = 8*(N-1) + 4
!!
!!---      East node and surface  ---
!!
!          NE = N+1
!          IF( I.LT.IFLD ) THEN
!            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NE-1) + 3 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 4 (i,j+1,k)  ---
!!
!          IVX = 8*(N-1) + 3
!!
!!---      West node and surface  ---
!!
!          NW = N-1
!          IF( I.GT.1 ) THEN
!            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NW-1) + 4 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 5 (i,j,k+1)  ---
!!
!          IVX = 8*(N-1) + 5
!!
!!---      West node and surface  ---
!!
!          NW = N-1
!          IF( I.GT.1 ) THEN
!            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NW-1) + 6 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 6 (i+1,j,k+1)  ---
!!
!          IVX = 8*(N-1) + 6
!!
!!---      East node and surface  ---
!!
!          NE = N+1
!          IF( I.LT.IFLD ) THEN
!            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NE-1) + 5 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 7 (i+1,j+1,k+1)  ---
!!
!          IVX = 8*(N-1) + 8
!!
!!---      East node and surface  ---
!!
!          NE = N+1
!          IF( I.LT.IFLD ) THEN
!            IF( IXP(NE).NE.0 .AND. INBS(4,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NE-1) + 7 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 8 (i,j+1,k+1)  ---
!!
!          IVX = 8*(N-1) + 7
!!
!!---      West node and surface  ---
!!
!          NW = N-1
!          IF( I.GT.1 ) THEN
!            IF( IXP(NW).NE.0 .AND. INBS(3,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NW-1) + 8 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9)') IVX
!  240   CONTINUE
!!
!!---  Y domain ( Points 1,2,3,4,5,6,7,8 )  ---
!!
!      ELSEIF( JFLD.GT.1 ) THEN
!        DO 250 N = 1,NFLD
!          IF( IXP(N).EQ.0 ) GOTO 250
!          J = JD(N)
!!
!!---      Point 1 (i,j,k)  ---
!!
!          IVX = 8*(N-1) + 1
!!
!!---      South node and surface  ---
!!
!          NS = N-IFLD
!          IF( J.GT.1 ) THEN
!            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NS-1) + 3 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 2 (i+1,j,k)  ---
!!
!          IVX = 8*(N-1) + 2
!!
!!---      South node and surface  ---
!!
!          NS = N-IFLD
!          IF( J.GT.1 ) THEN
!            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NS-1) + 4 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 3 (i+1,j+1,k)  ---
!!
!          IVX = 8*(N-1) + 4
!!
!!---      North node and surface  ---
!!
!          NN = N+IFLD
!          IF( J.LT.JFLD ) THEN
!            IF( IXP(NN).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NN-1) + 2 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 4 (i,j+1,k)  ---
!!
!          IVX = 8*(N-1) + 3
!!
!!---      North node and surface  ---
!!
!          NN = N+IFLD
!          IF( J.LT.JFLD ) THEN
!            IF( IXP(NN).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NN-1) + 1 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 5 (i,j,k+1)  ---
!!
!          IVX = 8*(N-1) + 5
!!
!!---      South node and surface  ---
!!
!          NS = N-IFLD
!          IF( J.GT.1 ) THEN
!            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NS-1) + 7 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 6 (i+1,j,k+1)  ---
!!
!          IVX = 8*(N-1) + 6
!!
!!---      South node and surface  ---
!!
!          NS = N-IFLD
!          IF( J.GT.1 ) THEN
!            IF( IXP(NS).NE.0 .AND. INBS(2,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NS-1) + 8 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 7 (i+1,j+1,k+1)  ---
!!
!          IVX = 8*(N-1) + 8
!!
!!---      North node and surface  ---
!!
!          NN = N+IFLD
!          IF( J.LT.JFLD ) THEN
!            IF( IXP(NN).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NN-1) + 6 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 8 (i,j+1,k+1)  ---
!!
!          IVX = 8*(N-1) + 7
!!
!!---      North node and surface  ---
!!
!          NN = N+IFLD
!          IF( J.LT.JFLD ) THEN
!            IF( IXP(NN).NE.0 .AND. INBS(5,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NN-1) + 5 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9)') IVX
!  250   CONTINUE
!!
!!---  Z domain ( Points 1,2,3,4,5,6,7,8 )  ---
!!
!      ELSEIF( JFLD.GT.1 ) THEN
!        DO 260 N = 1,NFLD
!          IF( IXP(N).EQ.0 ) GOTO 260
!          K = KD(N)
!!
!!---      Point 1 (i,j,k)  ---
!!
!          IVX = 8*(N-1) + 1
!!
!!---      Bottom node and surface  ---
!!
!          NB = N-IJFLD
!          IF( K.GT.1 ) THEN
!            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NB-1) + 5 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 2 (i+1,j,k)  ---
!!
!          IVX = 8*(N-1) + 2
!!
!!---      Bottom node and surface  ---
!!
!          NB = N-IJFLD
!          IF( K.GT.1 ) THEN
!            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NB-1) + 6 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 3 (i+1,j+1,k)  ---
!!
!          IVX = 8*(N-1) + 4
!!
!!---      Bottom node and surface  ---
!!
!          NB = N-IJFLD
!          IF( K.GT.1 ) THEN
!            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NB-1) + 8 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 4 (i,j+1,k)  ---
!!
!          IVX = 8*(N-1) + 3
!!
!!---      Bottom node and surface  ---
!!
!          NB = N-IJFLD
!          IF( K.GT.1 ) THEN
!            IF( IXP(NB).NE.0 .AND. INBS(1,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NB-1) + 7 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 5 (i,j,k+1)  ---
!!
!          IVX = 8*(N-1) + 5
!!
!!---      Top node and surface  ---
!!
!          NT = N+IJFLD
!          IF( K.LT.KFLD ) THEN
!            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NT-1) + 1 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 6 (i+1,j,k+1)  ---
!!
!          IVX = 8*(N-1) + 6
!!
!!---      Top node and surface  ---
!!
!          NT = N+IJFLD
!          IF( K.LT.KFLD ) THEN
!            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NT-1) + 2 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 7 (i+1,j+1,k+1)  ---
!!
!          IVX = 8*(N-1) + 8
!!
!!---      Top node and surface  ---
!!
!          NT = N+IJFLD
!          IF( K.LT.KFLD ) THEN
!            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NT-1) + 4 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9,1X,$)') IVX
!!
!!---      Point 8 (i,j+1,k+1)  ---
!!
!          IVX = 8*(N-1) + 7
!!
!!---      Top node and surface  ---
!!
!          NT = N+IJFLD
!          IF( K.LT.KFLD ) THEN
!            IF( IXP(NT).NE.0 .AND. INBS(6,N).EQ.0 ) THEN
!              IVX = MIN( IVX,8*(NT-1) + 3 )
!            ENDIF
!          ENDIF
!          WRITE(26,'(I9)') IVX
!  260   CONTINUE
!      ENDIF
!!
!!---  Close 'connect' file  ---
!!
!      CLOSE(UNIT=26)
!!
!!---  End of CONNLST group
!!
!      RETURN
!      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CONV_XYZ
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
!     Write Tecplot connectivity list for finite element grid
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 14 February 2012.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
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
!----------------------Type Declarations-------------------------------!
!
      PARAMETER( LVERT=62800 )
      REAL*8 XMX(LVERT),YMX(LVERT),ZMX(LVERT)
      REAL*8 PORX(LVERT),PERMX(LVERT)
      REAL*8 DISTX(8)
      CHARACTER*64 FMDUM
      CHARACTER*512 CHDUM
      LOGICAL FCHK
      INTEGER IVERT(8)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/CONV_XYZ'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      ISUB_LOG = ISUB_LOG-1
!
!---  Check for external file  ---
!
      INQUIRE( FILE='properties_johansen.dat', FORM=FMDUM, EXIST=FCHK )
      IF( .NOT.FCHK ) THEN
        INDX = 4
        CHMSG = 'Missing Property File: properties_johansen.dat'
        CALL WRMSGS( INDX )
      ELSEIF( FMDUM.EQ.'unformatted' ) THEN
        INDX = 4
        CHMSG = 'Property File Format: properties_johansen.dat'
        CALL WRMSGS( INDX )
      ENDIF
      OPEN( UNIT=9,FILE='properties_johansen.dat',
     &  STATUS='OLD',FORM='FORMATTED' )
      REWIND( UNIT=9 )
      READ(9,'(A)') CHDUM
      DO 100 M = 1,LVERT
        READ(9,*) XMX(M),YMX(M),ZMX(M),PORX(M),PERMX(M)
  100 CONTINUE
      CLOSE( UNIT=9 )

!---  Search grid for nearest vertice  ---
!
      DO 200 N = 1,NFLD
        DISTX(1) = 1.D+20
        IVERT(1) = 0
        DO 140 M = 1,LVERT
          DX = SQRT( (XP(N)-XMX(M))**2 + (YP(N)-YMX(M))**2 + 
     &      (ZP(N)-ZMX(M))**2 )
          IF( DX.LE.DISTX(1) ) THEN
            DISTX(1) = DX
            IVERT(1) = M
          ENDIF
  140   CONTINUE
        PORD(1,N) = PORX(IVERT(1))
        PORD(2,N) = PERMX(IVERT(1))
  200 CONTINUE
      OPEN(UNIT=26, FILE='porosity_johansen_stomp.dat', 
     &  STATUS='UNKNOWN', FORM='FORMATTED')
      CLOSE(UNIT=26,STATUS='DELETE')
      OPEN(UNIT=26, FILE='porosity_johansen_stomp.dat', 
     &  STATUS='NEW', FORM='FORMATTED')
      DO 442 N = 1,NFLD
          WRITE(26,'(1PE12.5)') PORD(1,N)
  442 CONTINUE
      CLOSE( UNIT=26 )
      OPEN(UNIT=26, FILE='permeability_johansen_stomp.dat', 
     &  STATUS='UNKNOWN', FORM='FORMATTED')
      CLOSE(UNIT=26,STATUS='DELETE')
      OPEN(UNIT=26, FILE='permeability_johansen_stomp.dat', 
     &  STATUS='NEW', FORM='FORMATTED')
      DO 444 N = 1,NFLD
          WRITE(26,'(1PE12.5)') PORD(2,N)
  444 CONTINUE
      CLOSE( UNIT=26 )
!
!---  End of CONV_XYZ group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CROSS_3D ( X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3 )
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
!     CROSS_3D computes the cross product of two vectors in 3D.
!
!     Definition:
!
!     The cross product in 3D can be regarded as the determinant of the
!     symbolic matrix:
!
!          |  i  j  k |
!      det | x1 y1 z1 |
!          | x2 y2 z2 |
!
!      = ( y1 * z2 - z1 * y2 ) * i
!      + ( z1 * x2 - x1 * z2 ) * j
!      + ( x1 * y2 - y1 * x2 ) * k
!
!     Author:
!
!     John Burkardt
!
!     Parameters:
!
!     Input, real X1, Y1, Z1, X2, Y2, Z2, the coordinates 
!     of the vectors.
!
!     Output, real X3, Y3, Z3, the cross product vector.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 March 2006.
!     Last Modified by MD White, PNNL, 24 March 2006.
!     $Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
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
      REAL*8 X1,Y1,Z1
      REAL*8 X2,Y2,Z2
      REAL*8 X3,Y3,Z3
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/CROSS_3D'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
      X3 = Y1*Z2 - Z1*Y2
      Y3 = Z1*X2 - X1*Z2
      Z3 = X1*Y2 - Y1*X2
      ISUB_LOG = ISUB_LOG-1
!
!---  End of CROSS_3D group  ---
!
      RETURN
      END

!------------------------Function--------------------------------------!
!
      FUNCTION ENORM_3D ( X1,Y1,Z1 )
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
!     ENORM_3D computes the Euclidean norm of a vector in 3D.
!
!     Author:
!
!     John Burkardt
!
!     Parameters:
!
!     Input, real X1, Y1, Z1, the coordinates of the vector.
!
!     Output, real ENORM_3D, the Euclidean norm of the vector.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 March 2006.
!     Last Modified by MD White, PNNL, 24 March 2006.
!     $Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
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
      REAL*8 ENORM_3D
      REAL*8 X1,Y1,Z1
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/ENORM_3D'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
      ENORM_3D = SQRT ( X1*X1 + Y1*Y1 + Z1*Z1 )
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ENORM_3D group  ---
!
      RETURN
      END
      
!------------------------Function--------------------------------------!
!
      FUNCTION IDF( IX,JX,KX,NX )
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
!     I index of node
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 17 June 2015.
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER IDF
!
!----------------------Executable Lines--------------------------------!
!
      IF( MOD( NX,IX ).EQ.0 ) THEN
        IDF = IX
      ELSE
        IDF = MOD(NX,IX)
      ENDIF
!      IDF = MIN( IDF,IX )
!
!---  End of IDF group  ---
!
      RETURN
      END
      
!------------------------Function--------------------------------------!
!
      FUNCTION JDF( IX,JX,KX,NX )
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
!     J index of node
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 17 June 2015.
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER JDF
!
!----------------------Executable Lines--------------------------------!
!
      IF( MOD( NX,IX*JX ).EQ.0 ) THEN
        JDF = IX*JX
      ELSE
        JDF = MOD(NX,IX*JX)
      ENDIF
      JDF = (JDF-1)/IX + 1
!      JDF = MIN( JDF,JX )
!
!---  End of JDF group  ---
!
      RETURN
      END
      
!------------------------Function--------------------------------------!
!
      FUNCTION KDF( IX,JX,KX,NX )
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
!     K index of node
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 17 June 2015.
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER KDF
!
!----------------------Executable Lines--------------------------------!
!
      KDF = (NX-1)/(IX*JX) + 1
!      KDF = MIN( KDF,KX )
!
!---  End of KDF group  ---
!
      RETURN
      END
      
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE LOCATE( XX,N,X,J )
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
!     Given an array XX of length N, and given a value X, returns a
!     value J such that X is between XX(J) and XX(J+1).  XX must be
!     monotonic, either increasing or decreasing.  J=0 or J=N is 
!     returned to indicate that X is out of range.
!
!     Press, W.H., B.P. Flannery, S.A. Teukolsky, and W.T. Vetterling.
!     1986.  Numerical Recipes, The Art of Scientific Computing.
!     Cambridge University Press, Cambridge.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, February, 1999.
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
      REAL*8 XX(N)
!
!----------------------Executable Lines--------------------------------!
!
      JL = 0
      JU = N+1
   10 CONTINUE
      IF( JU-JL.GT.1 ) THEN
        JM = (JU+JL)/2
        IF( (XX(N).GT.XX(1)).EQV.(X.GT.XX(JM)) ) THEN
          JL = JM
        ELSE
          JU = JM
        ENDIF
        GOTO 10
      ENDIF
      J = JL
!
!---  End of LOCATE group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE LUDCMP( A,N,NP,IX,D )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Numerical Recipes, The Art of Scientific Computing
!     W.H. Press, B.P. Flannery, Saul A. Teukolsky, and W.T. Vetterling
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, August 1, 2000.
!     Last Modified by Mark White, PNNL, August 1, 2000.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
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
      REAL*8 A(NP,NP),VV(NP)
      INTEGER IX(NP)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/LUDCMP'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
      D = 1.D+0
      DO 12 I = 1,N
        AAMAX = 0.D+0
        DO 11 J = 1,N
          IF( ABS(A(I,J)).GT.AAMAX ) AAMAX = ABS(A(I,J))
   11   CONTINUE
        IF( ABS(AAMAX)/EPSL.LT.EPSL ) THEN
          INDX = 20
          CHMSG = 'Singular Matrix: '
          IMSG = NP
          CALL WRMSGS( INDX )
          ICUTTS = 1
        ENDIF
        VV(I) = 1.D+0/AAMAX
   12 CONTINUE
      IMAX = 0
      DO 19 J = 1,N
        DO 14 I = 1,J-1
          SUM = A(I,J)
          DO 13 K = 1,I-1
            SUM = SUM - A(I,K)*A(K,J)
   13     CONTINUE
          A(I,J) = SUM
   14   CONTINUE
        AAMAX = 0.D+0
        DO 16 I = J,N
          SUM = A(I,J)
          DO 15 K = 1,J-1
            SUM = SUM - A(I,K)*A(K,J)
   15     CONTINUE
          A(I,J) = SUM
          DUM = VV(I)*ABS(SUM)
          IF( DUM.GE.AAMAX ) THEN
            IMAX = I
            AAMAX = DUM
          ENDIF
   16   CONTINUE
        IF( J.NE.IMAX ) THEN
          DO 17 K = 1,N
            DUM = A(IMAX,K)
            A(IMAX,K) = A(J,K)
            A(J,K) = DUM
   17     CONTINUE
          D = -D
          VV(IMAX) = VV(J)
        ENDIF
        IX(J) = IMAX
        IF( ABS(A(J,J))/EPSL.LT.EPSL ) A(J,J) = 1.D-30
        IF( J.NE.N ) THEN
          DUM = 1.D+0/A(J,J)
          DO 18 I = J+1,N
            A(I,J) = A(I,J)*DUM
   18     CONTINUE
        ENDIF
   19 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of LUDCMP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE LUBKSB( A,N,NP,IX,B )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Numerical Recipes, The Art of Scientific Computing
!     W.H. Press, B.P. Flannery, Saul A. Teukolsky, and W.T. Vetterling
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, August 1, 2000.
!     Last Modified by Mark White, PNNL, August 1, 2000.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
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
      REAL*8 A(NP,NP),B(NP)
      INTEGER IX(NP)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/LUBKSB'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
      II = 0
      DO 12 I = 1,N
        IL = IX(I)
        SUM = B(IL)
        B(IL) = B(I)
        IF( II.NE.0 ) THEN
          DO 11 J = II,I-1
            SUM = SUM - A(I,J)*B(J)
   11     CONTINUE
        ELSEIF( ABS(SUM)/EPSL.GT.EPSL ) THEN
          II = I
        ENDIF
        B(I) = SUM
   12 CONTINUE
      DO 14 I = N,1,-1
        SUM = B(I)
        IF( I.LT.N ) THEN
          DO 13 J = I+1,N
            SUM = SUM - A(I,J)*B(J)
   13     CONTINUE
        ENDIF
        B(I) = SUM/A(I,I)
   14 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of LUBKSB group  ---
!
      RETURN
      END

!------------------------Function--------------------------------------!
!
      FUNCTION NDBR( IX,IRX,JX,JRX,KX )
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
!     Block refined node index
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 28 January.
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER NDBR
!
!----------------------Executable Lines--------------------------------!
!
      NDBR = (KX-1)*IRX*JRX + (JX-1)*IRX + IX - 1
!
!---  End of NDBR group  ---
!
      RETURN
      END
      
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE PGCNTRD ( N,PX,PY,PZ,CX,CY,CZ )
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
!     PGCNTRD computes the centroid of a polygon in 3D.
!
!
!     Method:
!
!     The centroid is the area-weighted sum of the centroids of
!     disjoint triangles that make up the polygon.
!
!     Reference:
!
!     Adrian Bowyer and John Woodwark,
!     A Programmers Geometry,
!     Butterworths, 1983.
!
!     Author:
!
!     John Burkardt
!
!     Parameters:
!
!     Input, integer N, the number of vertices of the polygon.
!
!     Input, real X(N), Y(N), Z(N), the coordinates of the vertices.
! 
!     Output, real CX, CY, CZ, the coordinates of the centroid.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 March 2006.
!     Last Modified by MD White, PNNL, 24 March 2006.
!     $Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
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
      REAL*8 AREA,AREAT
      REAL*8 CX,CY,CZ
      REAL*8 PX(N),PY(N),PZ(N)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/PGCNTRD'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
      AREA = 0.D+0
      CX = 0.D+0
      CY = 0.D+0
      CZ = 0.D+0
      DO 100 I = 1,N-2
        CALL TRGAREA ( PX(1),PY(1),PZ(1),PX(I+1),
     &    PY(I+1),PZ(I+1),PX(I+2),PY(I+2),PZ(I+2),AREAT )
        AREA = AREA + AREAT
        CX = CX + AREAT*( PX(1)+PX(I+1)+PX(I+2) )/3.D+0
        CY = CY + AREAT*( PY(1)+PY(I+1)+PY(I+2) )/3.D+0
        CZ = CZ + AREAT*( PZ(1)+PZ(I+1)+PZ(I+2) )/3.D+0
  100 CONTINUE
      CX = CX/AREA
      CY = CY/AREA
      CZ = CZ/AREA
      ISUB_LOG = ISUB_LOG-1
!
!---  End of PGCNTRD group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE PHCNTRD ( PX,PY,PZ,CX,CY,CZ,NP,N )
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
!     PHCNTRD computes the centroid of a 
!     polyhedron in 3D.
!
!     Method:
!
!     The centroid is computed as the point having the minimum
!     variation of distances to the vertices.
!
!     Reference:
!
!     Zunic, T. B., and E. Makovicky.  1996.  Determination of the
!     centroid or 'the best centre' of a coordination polyhedron.
!
!     Parameters:
!
!     Input, integer NP, the number of vertices of the polygon.
!
!     Input, real X(N), Y(N), Z(N), the coordinates of the vertices.
! 
!     Output, real CX, CY, CZ, the coordinates of the centroid.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 27 March 2006.
!     Last Modified by MD White, PNNL, 27 March 2006.
!     $Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
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
      REAL*8 CX,CY,CZ
      REAL*8 PX(NP),PY(NP),PZ(NP)
      REAL*8 AJM(3,3),BJM(3)
      INTEGER IJM(3)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/PHCNTRD'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
      SUMX = 0.D+0
      SUMX2 = 0.D+0
      SUMX3 = 0.D+0
      SUMY = 0.D+0
      SUMY2 = 0.D+0
      SUMY3 = 0.D+0
      SUMZ = 0.D+0
      SUMZ2 = 0.D+0
      SUMZ3 = 0.D+0
      SUMXY = 0.D+0
      SUMYZ = 0.D+0
      SUMZX = 0.D+0
      SUMX2Y = 0.D+0
      SUMX2Z = 0.D+0
      SUMY2Z = 0.D+0
      SUMY2X = 0.D+0
      SUMZ2X = 0.D+0
      SUMZ2Y = 0.D+0
      PXMIN = 1.D+20
      PXMAX = -1.D+20
      PYMIN = 1.D+20
      PYMAX = -1.D+20
      PZMIN = 1.D+20
      PZMAX = -1.D+20
      DO 10 I = 1,NP
        PXMIN = MIN( PX(I),PXMIN )
        PXMAX = MAX( PX(I),PXMAX )
        PYMIN = MIN( PY(I),PYMIN )
        PYMAX = MAX( PY(I),PYMAX )
        PZMIN = MIN( PZ(I),PZMIN )
        PZMAX = MAX( PZ(I),PZMAX )
   10 CONTINUE
      DO 100 I = 1,NP
        SUMX = SUMX + PX(I)
        SUMX2 = SUMX2 + PX(I)**2
        SUMX3 = SUMX3 + PX(I)**3
        SUMY = SUMY + PY(I)
        SUMY2 = SUMY2 + PY(I)**2
        SUMY3 = SUMY3 + PY(I)**3
        SUMZ = SUMZ + PZ(I)
        SUMZ2 = SUMZ2 + PZ(I)**2
        SUMZ3 = SUMZ3 + PZ(I)**3
        SUMXY = SUMXY + PX(I)*PY(I)
        SUMYZ = SUMYZ + PY(I)*PZ(I)
        SUMZX = SUMZX + PZ(I)*PX(I)
        SUMX2Y = SUMX2Y + (PX(I)**2)*PY(I)
        SUMX2Z = SUMX2Z + (PX(I)**2)*PZ(I)
        SUMY2X = SUMY2X + (PY(I)**2)*PX(I)
        SUMY2Z = SUMY2Z + (PY(I)**2)*PZ(I)
        SUMZ2X = SUMZ2X + (PZ(I)**2)*PX(I)
        SUMZ2Y = SUMZ2Y + (PZ(I)**2)*PY(I)
  100 CONTINUE
      NJM = 3
      RNX = REAL(NP)
      AJM(1,1) = 2.D+0*(SUMX2-(SUMX**2)/RNX)
      AJM(1,2) = 2.D+0*(SUMXY-(SUMX*SUMY)/RNX)
      AJM(1,3) = 2.D+0*(SUMZX-(SUMZ*SUMX)/RNX)
      BJM(1) = SUMX3 + SUMY2X + SUMZ2X - (SUMX2*SUMX)/RNX
     &  - (SUMY2*SUMX)/RNX - (SUMZ2*SUMX)/RNX
      AJM(2,1) = 2.D+0*(SUMXY-(SUMX*SUMY)/RNX)
      AJM(2,2) = 2.D+0*(SUMY2-(SUMY**2)/RNX)
      AJM(2,3) = 2.D+0*(SUMYZ-(SUMY*SUMZ)/RNX)
      BJM(2) = SUMX2Y + SUMY3 + SUMZ2Y - (SUMX2*SUMY)/RNX
     &  - (SUMY2*SUMY)/RNX - (SUMZ2*SUMY)/RNX
      AJM(3,1) = 2.D+0*(SUMZX-(SUMZ*SUMX)/RNX)
      AJM(3,2) = 2.D+0*(SUMYZ-(SUMY*SUMZ)/RNX)
      AJM(3,3) = 2.D+0*(SUMZ2-(SUMZ**2)/RNX)
      BJM(3) = SUMX2Z + SUMY2Z + SUMZ3 - (SUMX2*SUMZ)/RNX
     &  - (SUMY2*SUMZ)/RNX - (SUMZ2*SUMZ)/RNX
      CALL LUDCMP( AJM,NJM,NJM,IJM,DJM )
      CALL LUBKSB( AJM,NJM,NJM,IJM,BJM )
      CX = BJM(1)
      CY = BJM(2)
      CZ = BJM(3)
      IF( CX.LT.PXMIN .OR. CX.GT.PXMAX .OR.
     &  CY.LT.PYMIN .OR. CY.GT.PYMAX .OR.
     &  CZ.LT.PZMIN .OR. CZ.GT.PZMAX ) THEN
        PRINT *,'Node Centroid Outside of Polygon Limits'
        PRINT *,'N = ',N,' I = ',ID(N),' J = ',JD(N),' K = ',KD(N)
        PRINT *,'XP = ',CX,' XMIN = ',PXMIN,' XMAX = ',PXMAX
        PRINT *,'YP = ',CY,' YMIN = ',PYMIN,' YMAX = ',PYMAX
        PRINT *,'ZP = ',CZ,' ZMIN = ',PZMIN,' ZMAX = ',PZMAX
        PRINT *,'X = ',PX
        PRINT *,'Y = ',PY
        PRINT *,'Z = ',PZ
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of PHCNTRD group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDBR
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
!     Read input file for block refinement information.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, January 2014.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
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
      CHARACTER*64 ADUM,FDUM,UNTS
      CHARACTER*512 CHDUM
      CHARACTER*25 FORM4
      CHARACTER*23 FORM5
!
!----------------------Data Statements---------------------------------!
!
      DATA FORM4 /'(A,I3,A,I3,A,I3,A,I8,A,$)'/
      DATA FORM5 /'(A,I3,A,I3,A,I3,A,I8,A)'/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDBR'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Write card information to ouput file  ---
!
      CARD = 'Block Refinement Card'
      ICD = INDEX( CARD,'  ' )-1
      WRITE(IWR,'(//,3A,/)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of grid refinement domains  ---
!
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Block Refinement Domains'
      CALL RDINT(ISTART,ICOMMA,CHDUM,NDOMX)
!
!---  Loop over number of grid refinement domains  ---
!
      MXBRFX = 0
      MXBRFY = 0
      MXBRFZ = 0
      DO 100 NX = 1,NDOMX     
        ISTART = 1
        CALL RDINPL( CHDUM )
        CALL LCASE( CHDUM )
        VARB = 'Block Refinement Domain I-Start Index'
        CALL RDINT(ISTART,ICOMMA,CHDUM,I1X)
        VARB = 'Block Refinement Domain I-End Index'
        CALL RDINT(ISTART,ICOMMA,CHDUM,I2X)
        VARB = 'Block Refinement Domain J-Start Index'
        CALL RDINT(ISTART,ICOMMA,CHDUM,J1X)
        VARB = 'Block Refinement Domain J-End Index'
        CALL RDINT(ISTART,ICOMMA,CHDUM,J2X)
        VARB = 'Block Refinement Domain K-Start Index'
        CALL RDINT(ISTART,ICOMMA,CHDUM,K1X)
        VARB = 'Block Refinement Domain K-End Index'
        CALL RDINT(ISTART,ICOMMA,CHDUM,K2X)
        IBRFX = 0
        IBRFY = 0
        IBRFZ = 0
   10   CONTINUE
        VARB = 'Block Refinement Level'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'1x').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') '1X Block Refinement'
          IBRFX = 1
        ELSEIF( INDEX(ADUM(1:),'2x').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') '2X Block Refinement'
          IBRFX = 2
        ELSEIF( INDEX(ADUM(1:),'3x').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') '3X Block Refinement'
          IBRFX = 3
        ELSEIF( INDEX(ADUM(1:),'4x').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') '4X Block Refinement'
          IBRFX = 4
        ELSEIF( INDEX(ADUM(1:),'5x').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') '5X Block Refinement'
          IBRFX = 5
        ELSEIF( INDEX(ADUM(1:),'3x').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') '6X Block Refinement'
          IBRFX = 6
        ELSEIF( INDEX(ADUM(1:),'7x').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') '7X Block Refinement'
          IBRFX = 7
        ELSEIF( INDEX(ADUM(1:),'1y').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') '1Y Block Refinement'
          IBRFY = 1
        ELSEIF( INDEX(ADUM(1:),'2y').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') '2Y Block Refinement'
          IBRFY = 2
        ELSEIF( INDEX(ADUM(1:),'3y').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') '3Y Block Refinement'
          IBRFY = 3
        ELSEIF( INDEX(ADUM(1:),'4y').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') '4Y Block Refinement'
          IBRFY = 4
        ELSEIF( INDEX(ADUM(1:),'5y').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') '5Y Block Refinement'
          IBRFY = 5
        ELSEIF( INDEX(ADUM(1:),'6y').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') '6Y Block Refinement'
          IBRFY = 6
        ELSEIF( INDEX(ADUM(1:),'7y').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') '7Y Block Refinement'
          IBRFY = 7
        ELSEIF( INDEX(ADUM(1:),'1z').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') '1Z Block Refinement'
          IBRFZ = 1
        ELSEIF( INDEX(ADUM(1:),'2z').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') '2Z Block Refinement'
          IBRFZ = 2
        ELSEIF( INDEX(ADUM(1:),'3z').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') '3Z Block Refinement'
          IBRFZ = 3
        ELSEIF( INDEX(ADUM(1:),'4z').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') '4Z Block Refinement'
          IBRFZ = 4
        ELSEIF( INDEX(ADUM(1:),'5z').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') '5Z Block Refinement'
          IBRFZ = 5
        ELSEIF( INDEX(ADUM(1:),'6z').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') '6Z Block Refinement'
          IBRFZ = 6
        ELSEIF( INDEX(ADUM(1:),'7z').NE.0 ) THEN
          WRITE(IWR,'(2X,A)') '7Z Block Refinement'
          IBRFZ = 7
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Block Refinement Level: ' // ADUM
          CALL WRMSGS( INDX )
        ENDIF
        IF( IFLD.EQ.1 .AND. IBRFX.GT.0 ) THEN
          INDX = 4
          CHMSG = 'X-Block Refinement for 1D X-Grid Domain: ' // ADUM
          CALL WRMSGS( INDX )
        ENDIF
        IF( JFLD.EQ.1 .AND. IBRFY.GT.0 ) THEN
          INDX = 4
          CHMSG = 'Y-Block Refinement for 1D Y-Grid Domain: ' // ADUM
          CALL WRMSGS( INDX )
        ENDIF
        IF( KFLD.EQ.1 .AND. IBRFZ.GT.0 ) THEN
          INDX = 4
          CHMSG = 'Z-Block Refinement for 1D Z-Grid Domain: ' // ADUM
          CALL WRMSGS( INDX )
        ENDIF
        CALL CHKCHR(ISTART,ICOMMA,CHDUM,INDX)
        IF( INDX.EQ.1 ) GOTO 10 
        I1X = MAX( 1,I1X )
        I1X = MIN( I1X,I2X,IFLD )
        I2X = MAX( 1,I1X,I2X )
        I2X = MIN( I2X,IFLD )
        J1X = MAX( 1,J1X )
        J1X = MIN( J1X,J2X,JFLD )
        J2X = MAX( 1,J1X,J2X )
        J2X = MIN( J2X,JFLD )
        K1X = MAX( 1,K1X )
        K1X = MIN( K1X,K2X,KFLD )
        K2X = MAX( 1,K1X,K2X )
        K2X = MIN( K2X,KFLD )
        WRITE(IWR,'(4X,A,I6,A,I6)') 'I = ',I1X,' to ',I2X
        WRITE(IWR,'(4X,A,I6,A,I6)') 'J = ',J1X,' to ',J2X
        WRITE(IWR,'(4X,A,I6,A,I6,/)') 'K = ',K1X,' to ',K2X
!
!---    Assign grid refinement indexing  ---
!
        DO 20 K = K1X,K2X
        DO 20 J = J1X,J2X
        DO 20 I = I1X,I2X
          N = ND(I,J,K)
!
!---      Skip inactive nodes  ---
!
          IF( IXP(N).EQ.0 ) GOTO 20
          IBR(1,N) = IBRFX
          IBR(2,N) = IBRFY
          IBR(3,N) = IBRFZ
   20   CONTINUE
        MXBRFX = MAX( MXBRFX,IBRFX )
        MXBRFY = MAX( MXBRFY,IBRFY )
        MXBRFZ = MAX( MXBRFZ,IBRFZ )
  100 CONTINUE
!  
!---  Re-assign grid refinement indexing at node, honoring adjacent 
!     refinement levels (adjacent upgrade)  ---
!
      DO 310 M = 1,MAX(1,MXBRFX-1)
        DO 300 K = 1,KFLD
        DO 300 J = 1,JFLD
        DO 300 I = 1,IFLD
          N = ND(I,J,K)
          IF( IXP(N).EQ.0 ) GOTO 300
!  
!---      Bottom node  ---
!  
          IF( KFLD.GT.1 ) THEN
            IF( K.GT.1 ) THEN
              NB = N-IJFLD
              IBR(1,N) = MAX( IBR(1,N),IBR(1,NB)-1 )
              IBR(2,N) = MAX( IBR(2,N),IBR(2,NB)-1 )
              IBR(3,N) = MAX( IBR(3,N),IBR(3,NB)-1 )
            ENDIF
          ENDIF
!  
!---      South node  ---
!  
          IF( JFLD.GT.1 ) THEN
            IF( J.GT.1 ) THEN
              NS = N-IFLD
              IBR(1,N) = MAX( IBR(1,N),IBR(1,NS)-1 )
              IBR(2,N) = MAX( IBR(2,N),IBR(2,NS)-1 )
              IBR(3,N) = MAX( IBR(3,N),IBR(3,NS)-1 )
            ENDIF
          ENDIF
!  
!---      West node  ---
!  
          IF( IFLD.GT.1 ) THEN
            IF( I.GT.1 ) THEN
              NW = N-1
              IBR(1,N) = MAX( IBR(1,N),IBR(1,NW)-1 )
              IBR(2,N) = MAX( IBR(2,N),IBR(2,NW)-1 )
              IBR(3,N) = MAX( IBR(3,N),IBR(3,NW)-1 )
            ENDIF
          ENDIF
!  
!---      East node  ---
!  
          IF( IFLD.GT.1 ) THEN
            IF( I.LT.IFLD ) THEN
              NE = N+1
              IBR(1,N) = MAX( IBR(1,N),IBR(1,NE)-1 )
              IBR(2,N) = MAX( IBR(2,N),IBR(2,NE)-1 )
              IBR(3,N) = MAX( IBR(3,N),IBR(3,NE)-1 )
            ENDIF
          ENDIF
!  
!---      North node  ---
!  
          IF( JFLD.GT.1 ) THEN
            IF( J.LT.JFLD ) THEN
              NN = N+IFLD
              IBR(1,N) = MAX( IBR(1,N),IBR(1,NN)-1 )
              IBR(2,N) = MAX( IBR(2,N),IBR(2,NN)-1 )
              IBR(3,N) = MAX( IBR(3,N),IBR(3,NN)-1 )
            ENDIF
          ENDIF
!  
!---      Top node  ---
!  
          IF( KFLD.GT.1 ) THEN
            IF( K.LT.KFLD ) THEN
              NT = N+IJFLD
              IBR(1,N) = MAX( IBR(1,N),IBR(1,NT)-1 )
              IBR(2,N) = MAX( IBR(2,N),IBR(2,NT)-1 )
              IBR(3,N) = MAX( IBR(3,N),IBR(3,NT)-1 )
            ENDIF
          ENDIF
  300   CONTINUE
  310 CONTINUE
!  
!---  Refined node pointers  ---
!
      NC = NFLD
      NRFN = 0
      MXBR = 0
      DO 400 K = 1,KFLD
      DO 400 J = 1,JFLD
      DO 400 I = 1,IFLD
        N = ND(I,J,K)
        IF( IXP(N).EQ.0 ) GOTO 400
        NX = 1
        IF( IBR(1,N).GT.0 ) THEN
          IF( IFLD.GT.1 ) NX = NX*(2**IBR(1,N))
        ENDIF
        IF( IBR(2,N).GT.0 ) THEN
          IF( JFLD.GT.1 ) NX = NX*(2**IBR(2,N))
        ENDIF
        IF( IBR(3,N).GT.0 ) THEN
          IF( KFLD.GT.1 ) NX = NX*(2**IBR(3,N))
        ENDIF
        MXBR = MAX( MXBR,IBR(1,N),IBR(2,N),IBR(3,N) )
        IF( IBR(1,N)+IBR(2,N)+IBR(3,N).GT.0 ) THEN
          IBR(4,N) = NC+1
          IBR(5,N) = NC+NX
          NC = NC+NX
          NRFN = NRFN + 1
        ENDIF
  400 CONTINUE
!
!---  For restart simulations, check that the number of field + 
!     block refinement nodes agree in count  ---
!
      IF( IEO.EQ.2 ) THEN
        IF( NBRN.NE.NC-NFLD ) THEN
          INDX = 3
          CHMSG = 'Restart File Number of Block Refinement ' // 
     &      'Nodes Conflict'
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
      NFBN = NC
!
!---  Define the grid pointers  ---
!
      NSCX = NSX(NFLD)+1
      NSCY = NSY(NFLD)+IFLD
      NSCZ = NSZ(NFLD)+IJFLD
      DO 540 N = 1,NFLD
        IF( IBR(4,N).GT.N ) THEN
          IBRX = 2**IBR(1,N)
          JBRX = 2**IBR(2,N)
          KBRX = 2**IBR(3,N)
          DO 514 K = 1,KBRX
          DO 512 J = 1,JBRX
          DO 510 I = 1,IBRX
            NBRX = IBR(4,N) + (K-1)*IBRX*JBRX + (J-1)*IBRX + I - 1
            ID(NBRX) = I
            JD(NBRX) = J
            KD(NBRX) = K
            IXP(NBRX) = NBRX
            NSCX = NSCX + 1
            NSCY = NSCY + 1
            NSCZ = NSCZ + 1
            NSX(NBRX) = NSCX
            NSY(NBRX) = NSCY
            NSZ(NBRX) = NSCZ
            NSSX(NBRX) = NSCX + 1
            NSSY(NBRX) = NSCY + IBRX
            NSSZ(NBRX) = NSCZ + IBRX*JBRX
  510     CONTINUE
          NSCX = NSCX + 1
  512     CONTINUE
          NSCY = NSCY + IBRX
  514     CONTINUE
          NSCZ = NSCZ + IBRX*JBRX
        ENDIF
  540 CONTINUE
!
!---  Refined grid vertices  ---
!
      DO 640 N = 1,NFLD
        NBRX = IBR(4,N)
        IF( NBRX.GT.N ) THEN
          IBRX = 2**IBR(1,N)
          JBRX = 2**IBR(2,N)
          KBRX = 2**IBR(3,N)
          DO 630 K = 1,KBRX
          DO 620 J = 1,JBRX
          DO 610 I = 1,IBRX
!
!---        Vertices 1 and 2  ---
!
            X15X = XE(1,N) + (XE(5,N)-XE(1,N))*REAL(K-1)/REAL(KBRX)
            X37X = XE(3,N) + (XE(7,N)-XE(3,N))*REAL(K-1)/REAL(KBRX)
            X26X = XE(2,N) + (XE(6,N)-XE(2,N))*REAL(K-1)/REAL(KBRX)
            X48X = XE(4,N) + (XE(8,N)-XE(4,N))*REAL(K-1)/REAL(KBRX)
            XLX = X15X + (X37X-X15X)*REAL(J-1)/REAL(JBRX)
            XUX = X26X + (X48X-X26X)*REAL(J-1)/REAL(JBRX)
            XE(1,NBRX) = XLX + (XUX-XLX)*REAL(I-1)/REAL(IBRX)
            XE(2,NBRX) = XLX + (XUX-XLX)*REAL(I)/REAL(IBRX)
            Y15X = YE(1,N) + (YE(5,N)-YE(1,N))*REAL(K-1)/REAL(KBRX)
            Y37X = YE(3,N) + (YE(7,N)-YE(3,N))*REAL(K-1)/REAL(KBRX)
            Y26X = YE(2,N) + (YE(6,N)-YE(2,N))*REAL(K-1)/REAL(KBRX)
            Y48X = YE(4,N) + (YE(8,N)-YE(4,N))*REAL(K-1)/REAL(KBRX)
            YLX = Y15X + (Y37X-Y15X)*REAL(J-1)/REAL(JBRX)
            YUX = Y26X + (Y48X-Y26X)*REAL(J-1)/REAL(JBRX)
            YE(1,NBRX) = YLX + (YUX-YLX)*REAL(I-1)/REAL(IBRX)
            YE(2,NBRX) = YLX + (YUX-YLX)*REAL(I)/REAL(IBRX)
            Z15X = ZE(1,N) + (ZE(5,N)-ZE(1,N))*REAL(K-1)/REAL(KBRX)
            Z37X = ZE(3,N) + (ZE(7,N)-ZE(3,N))*REAL(K-1)/REAL(KBRX)
            Z26X = ZE(2,N) + (ZE(6,N)-ZE(2,N))*REAL(K-1)/REAL(KBRX)
            Z48X = ZE(4,N) + (ZE(8,N)-ZE(4,N))*REAL(K-1)/REAL(KBRX)
            ZLX = Z15X + (Z37X-Z15X)*REAL(J-1)/REAL(JBRX)
            ZUX = Z26X + (Z48X-Z26X)*REAL(J-1)/REAL(JBRX)
            ZE(1,NBRX) = ZLX + (ZUX-ZLX)*REAL(I-1)/REAL(IBRX)
            ZE(2,NBRX) = ZLX + (ZUX-ZLX)*REAL(I)/REAL(IBRX)
!
!---        Vertices 3 and 4  ---
!
            XLX = X15X + (X37X-X15X)*REAL(J)/REAL(JBRX)
            XUX = X26X + (X48X-X26X)*REAL(J)/REAL(JBRX)
            XE(3,NBRX) = XLX + (XUX-XLX)*REAL(I-1)/REAL(IBRX)
            XE(4,NBRX) = XLX + (XUX-XLX)*REAL(I)/REAL(IBRX)
            YLX = Y15X + (Y37X-Y15X)*REAL(J)/REAL(JBRX)
            YUX = Y26X + (Y48X-Y26X)*REAL(J)/REAL(JBRX)
            YE(3,NBRX) = YLX + (YUX-YLX)*REAL(I-1)/REAL(IBRX)
            YE(4,NBRX) = YLX + (YUX-YLX)*REAL(I)/REAL(IBRX)
            ZLX = Z15X + (Z37X-Z15X)*REAL(J)/REAL(JBRX)
            ZUX = Z26X + (Z48X-Z26X)*REAL(J)/REAL(JBRX)
            ZE(3,NBRX) = ZLX + (ZUX-ZLX)*REAL(I-1)/REAL(IBRX)
            ZE(4,NBRX) = ZLX + (ZUX-ZLX)*REAL(I)/REAL(IBRX)
!
!---        Vertices 5 and 6  ---
!
            X15X = XE(1,N) + (XE(5,N)-XE(1,N))*REAL(K)/REAL(KBRX)
            X37X = XE(3,N) + (XE(7,N)-XE(3,N))*REAL(K)/REAL(KBRX)
            X26X = XE(2,N) + (XE(6,N)-XE(2,N))*REAL(K)/REAL(KBRX)
            X48X = XE(4,N) + (XE(8,N)-XE(4,N))*REAL(K)/REAL(KBRX)
            XLX = X15X + (X37X-X15X)*REAL(J-1)/REAL(JBRX)
            XUX = X26X + (X48X-X26X)*REAL(J-1)/REAL(JBRX)
            XE(5,NBRX) = XLX + (XUX-XLX)*REAL(I-1)/REAL(IBRX)
            XE(6,NBRX) = XLX + (XUX-XLX)*REAL(I)/REAL(IBRX)
            Y15X = YE(1,N) + (YE(5,N)-YE(1,N))*REAL(K)/REAL(KBRX)
            Y37X = YE(3,N) + (YE(7,N)-YE(3,N))*REAL(K)/REAL(KBRX)
            Y26X = YE(2,N) + (YE(6,N)-YE(2,N))*REAL(K)/REAL(KBRX)
            Y48X = YE(4,N) + (YE(8,N)-YE(4,N))*REAL(K)/REAL(KBRX)
            YLX = Y15X + (Y37X-Y15X)*REAL(J-1)/REAL(JBRX)
            YUX = Y26X + (Y48X-Y26X)*REAL(J-1)/REAL(JBRX)
            YE(5,NBRX) = YLX + (YUX-YLX)*REAL(I-1)/REAL(IBRX)
            YE(6,NBRX) = YLX + (YUX-YLX)*REAL(I)/REAL(IBRX)
            Z15X = ZE(1,N) + (ZE(5,N)-ZE(1,N))*REAL(K)/REAL(KBRX)
            Z37X = ZE(3,N) + (ZE(7,N)-ZE(3,N))*REAL(K)/REAL(KBRX)
            Z26X = ZE(2,N) + (ZE(6,N)-ZE(2,N))*REAL(K)/REAL(KBRX)
            Z48X = ZE(4,N) + (ZE(8,N)-ZE(4,N))*REAL(K)/REAL(KBRX)
            ZLX = Z15X + (Z37X-Z15X)*REAL(J-1)/REAL(JBRX)
            ZUX = Z26X + (Z48X-Z26X)*REAL(J-1)/REAL(JBRX)
            ZE(5,NBRX) = ZLX + (ZUX-ZLX)*REAL(I-1)/REAL(IBRX)
            ZE(6,NBRX) = ZLX + (ZUX-ZLX)*REAL(I)/REAL(IBRX)
!
!---        Vertices 7 and 8  ---
!
            XLX = X15X + (X37X-X15X)*REAL(J)/REAL(JBRX)
            XUX = X26X + (X48X-X26X)*REAL(J)/REAL(JBRX)
            XE(7,NBRX) = XLX + (XUX-XLX)*REAL(I-1)/REAL(IBRX)
            XE(8,NBRX) = XLX + (XUX-XLX)*REAL(I)/REAL(IBRX)
            YLX = Y15X + (Y37X-Y15X)*REAL(J)/REAL(JBRX)
            YUX = Y26X + (Y48X-Y26X)*REAL(J)/REAL(JBRX)
            YE(7,NBRX) = YLX + (YUX-YLX)*REAL(I-1)/REAL(IBRX)
            YE(8,NBRX) = YLX + (YUX-YLX)*REAL(I)/REAL(IBRX)
            ZLX = Z15X + (Z37X-Z15X)*REAL(J)/REAL(JBRX)
            ZUX = Z26X + (Z48X-Z26X)*REAL(J)/REAL(JBRX)
            ZE(7,NBRX) = ZLX + (ZUX-ZLX)*REAL(I-1)/REAL(IBRX)
            ZE(8,NBRX) = ZLX + (ZUX-ZLX)*REAL(I)/REAL(IBRX)
            NBRX = NBRX+1
  610     CONTINUE
  620     CONTINUE
  630     CONTINUE
        ENDIF
  640 CONTINUE
!
!---  Assign rock/soil types for refined nodes  ---
!
      DO 710 N = 1,NFLD
        IF( IBR(4,N).GT.N ) THEN
          IBRX = 2**IBR(1,N)
          JBRX = 2**IBR(2,N)
          KBRX = 2**IBR(3,N)
          DO 700 NBRX = IBR(4,N),IBR(5,N)
            NX = NBRX-IBR(4,N)+1
            IZ(NBRX) = IZ(N)
            INP(NBRX-NFLD) = N
            IBR(1,NBRX) = IDF(IBRX,JBRX,KBRX,NX)
            IBR(2,NBRX) = JDF(IBRX,JBRX,KBRX,NX)
            IBR(3,NBRX) = KDF(IBRX,JBRX,KBRX,NX)
            IBR(4,NBRX) = NBRX
            IBR(5,NBRX) = NBRX
  700     CONTINUE
        ENDIF
  710 CONTINUE
!
!---  Record block refinement nodes to output file ---
!
      WRITE(IWR,'(A,/)') ' --- Block Refinement Node Record  ---'
      DO 730 N = 1,NFLD
        IF( IBR(4,N).GT.N ) THEN
          IBRX = 2**IBR(1,N)
          JBRX = 2**IBR(2,N)
          KBRX = 2**IBR(3,N)
          DO 720 NBRX = IBR(4,N),IBR(5,N)
            NX = NBRX-IBR(4,N)+1
            IX = IDF(IBRX,JBRX,KBRX,NX)
            JX = JDF(IBRX,JBRX,KBRX,NX)
            KX = KDF(IBRX,JBRX,KBRX,NX)
            WRITE(IWR,FORM4) ' (',IX,',',JX,',',KX,':',NBRX,') of '
            IX = ID(N)
            JX = JD(N)
            KX = KD(N)
            WRITE(IWR,FORM5) ' (',IX,',',JX,',',KX,':',N,')   '
  720     CONTINUE
        ENDIF
  730 CONTINUE
!
!---  Area and volume grid refinement nodes  ---
!
      IF( NFBN.GT.NFLD ) CALL AVBR1
!
!---  Reset subroutine string sequence  ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDBR group  ---
!
      RETURN
      END
      
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDECLGRID
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
!     Read ECLIPSE formatted grids
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 23 November 2011.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GRID
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
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: XX,YX,ZX
      REAL*8, DIMENSION(:), ALLOCATABLE :: XPETX
      REAL*8, DIMENSION(:), ALLOCATABLE :: ZCORNX
      CHARACTER*64 BDUM,BMDUM,FDUM,FMDUM,UNTS
      CHARACTER*512 BHDUM,CHDUM
      INTEGER ICX(2,2,2)
      LOGICAL BCHK,FCHK
!
!----------------------Data Statements---------------------------------!
!
      SAVE ICX
      DATA ICX / 8,7,6,5,4,3,2,1 /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDECLGRID'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Dynamic memory allocation  ---
!
      ALLOCATE( XX(1:8,1:LFY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XX'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( YX(1:8,1:LFY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: YX'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZX(1:8,1:LFY),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZX'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( XPETX(1:(LFX+1)*(LFY+1)*6),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: XPETX'
        CALL WRMSGS( INDX )
      ENDIF
      ALLOCATE( ZCORNX(1:LFX*LFY*LFZ*8),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: ZCORNX'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Read Eclipse file name  ---
!
      IFILE = 9
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      VARB = 'Generic Eclipse File Name'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
!
!---  Check for external file  ---
!
      INQUIRE( FILE=FDUM(1:NCH), FORM=FMDUM, EXIST=FCHK )
      IF( .NOT.FCHK ) THEN
        INDX = 4
        CHMSG = 'Missing Generic Eclipse File: ' // FDUM(1:NCH)
        CALL WRMSGS( INDX )
      ELSEIF( FDUM.EQ.'unformatted' ) THEN
        INDX = 4
        CHMSG = 'Generic Eclipse File Format: ' // FDUM(1:NCH)
        CALL WRMSGS( INDX )
      ENDIF
      OPEN( UNIT=9,FILE=FDUM(1:NCH),STATUS='OLD',FORM='FORMATTED' )
      REWIND( UNIT=9 )
      WRITE(IWR,'(/,3A)') VARB(1:IVR),': ',FDUM(1:NCH)
      INDX = 0
      DZMINX = 0.D+0
      VARB = 'Minimum Z Grid Spacing'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,DZMINX)
      VARB = 'Minimum Z Grid Spacing Units'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH),
     &  ': ',DZMINX
      IUNM = 1
      CALL RDUNIT( UNTS,DZMINX,INDX )
      WRITE(IWR,'(A,1PE11.4,A)') ' (',DZMINX,', m)'
!
!---  Search file for Petrel version number---
!
      IPET = 0
  100 READ(9,'(A)',END=110) CHDUM   
      CALL LCASE( CHDUM )
      IF( INDEX(CHDUM(1:),'-- exported').NE.0 ) THEN
        IF( INDEX(CHDUM(25:),'2013').NE.0 ) THEN
          IPET=2013
        ELSEIF( INDEX(CHDUM(25:),'2011').NE.0 ) THEN
          IPET=2011
        ELSE
          IPET=2009
        ENDIF
        GOTO 120
      ELSE
        GOTO 100
      ENDIF
  110 CONTINUE
!
!---  Petrel Version number not found  ---
!
      INDX = 18
      CHMSG = 'Missing Petrel Version Number'
      CALL WRMSGS( INDX )
  120 CONTINUE
!
!
!---  Search file for key word "GRIDUNIT"  ---
!
  130 READ(9,'(A)',END=140) CHDUM   
      CALL LCASE( CHDUM )
      IF( INDEX(CHDUM(1:),'gridunit').NE.0 ) THEN
        GOTO 150
      ELSE
        GOTO 130
      ENDIF
  140 CONTINUE
!
!---  Key word "GRIDUNIT" not found  ---
!
      INDX = 18
      CHMSG = 'Missing Grid Units'
      CALL WRMSGS( INDX )
  150 CONTINUE
!
!---  Read number of grid units  ---
!
      READ(9,'(A)') CHDUM
      CALL LCASE( CHDUM )
      VARX = 1.D+0
      IF( INDEX(CHDUM(1:),'metres').NE.0 ) THEN
        INDX = 0
        IUNM = 1
        UNTS = 'm'
        CALL RDUNIT(UNTS,VARX,INDX)
      ELSEIF( INDEX(CHDUM(1:),'feet').NE.0 ) THEN
        INDX = 0
        IUNM = 1
        UNTS = 'ft'
        CALL RDUNIT(UNTS,VARX,INDX)
      ELSEIF( INDEX(CHDUM(1:),'nm').NE.0 ) THEN
        INDX = 0
        IUNM = 1
        UNTS = 'nm'
        CALL RDUNIT(UNTS,VARX,INDX)
      ENDIF
!
!---  Search file for key word "SPECGRID"  ---
!
  160 READ(9,'(A)',END=170) CHDUM   
      CALL LCASE( CHDUM )
      IF( INDEX(CHDUM(1:),'specgrid').NE.0 ) THEN
        GOTO 180
      ELSE
        GOTO 160
      ENDIF
  170 CONTINUE
!
!---  Key word "SPECGRID" not found  ---
!
      INDX = 18
      CHMSG = 'Missing Grid Specification'
      CALL WRMSGS( INDX )
  180 CONTINUE
!
!---  Read number of grid cells in each direction  ---
!
      READ(9,*) IFLDX,JFLDX,KFLDX
!
!---  Check grid dimensions against STOMP input  ---
!
      IF( IFLDX.NE.IFLD .OR. JFLDX.NE.JFLD .OR. KFLDX.NE.KFLD ) THEN
        INDX = 4
        CHMSG = 'STOMP-ECLIPSE Grid Conflict'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Search file for key word "COORD"  ---
!
  190 READ(9,'(A)', END=200) CHDUM   
      CALL LCASE( CHDUM )
!
!---  Key word "COORD" found in main file  ---
!
      IF( INDEX(CHDUM(1:),'coord').NE.0 .AND.
     &  INDEX(CHDUM(1:),'coordsys').EQ.0 ) THEN
        GOTO 210
!
!---  Search for key word "COORD" found in include file  ---
!
      ELSEIF( INDEX(CHDUM(1:),'include').NE.0 ) THEN
        READ(9,'(A)',END=200) CHDUM
        ISTX = INDEX( CHDUM(1:),'''' )
        ISTX = ISTX + 1
        ISPX = INDEX( CHDUM(ISTX:),'''' ) + ISTX - 1
        BDUM = ' '
        ISPX = ISPX - 1
        NCH = ISPX-ISTX+1
        IF( NCH.LT.1 ) THEN
          INDX = 4
          CHMSG = 'Unrecognized File Name after INCLUDE'
          CALL WRMSGS( INDX )
        ENDIF
        READ(CHDUM(ISTX:ISPX),'(A)') BDUM(1:NCH)
        INQUIRE( FILE=BDUM(1:NCH), FORM=BMDUM, EXIST=BCHK )
        IF( .NOT.BCHK ) THEN
          INDX = 4
          CHMSG = 'Missing Include File: ' // BDUM(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
        OPEN( UNIT=10,FILE=BDUM(1:NCH),STATUS='OLD',FORM='FORMATTED' )
        REWIND( UNIT=10 )
  192   READ(10,'(A)',END=200) BHDUM
        CALL LCASE( BHDUM )
!
!---    Skip header information in include file  ---
!
        IF( INDEX(BHDUM(1:2),'--').NE.0 ) THEN
          GOTO 192
!
!---    Key word "COORD" found in include file  ---
!
        ELSEIF( INDEX(BHDUM(1:),'coord').NE.0 .AND.
     &    INDEX(BHDUM(1:),'coordsys').EQ.0 ) THEN
          IFILE = 10
          GOTO 210
        ELSEIF( SCAN(BHDUM(1:),'0123456789').NE.0 ) THEN 
          CLOSE( UNIT=10 )
          IFILE = 9
        ELSE
          GOTO 192
        ENDIF
      ELSE
        GOTO 190
      ENDIF
  200 CONTINUE
!
!---  Key word "COORD" not found  ---
!
      INDX = 18
      CHMSG = 'Missing Coordinate Data'
      CALL WRMSGS( INDX )
  210 CONTINUE
!
!---  For Petrel 2009/2011 read all coordinates into array and assign 
!     to X and Y and convert coordinates to meters
!
      NCORX = (IFLDX+1)*(JFLDX+1)*6
      READ(IFILE,*,END=212) (XPETX(L),L=1,NCORX)
!
!---  Close include file  ---
!
      IF( IFILE.EQ.10 ) THEN
        CLOSE( UNIT=10 )
        IFILE = 9
      ENDIF
      GOTO 214
!
!---  End-of-file error for Generic Eclipse File ---
!
  212 CONTINUE
      INDX = 18
      CHMSG = 'Generic Eclipse EOF: COORD'
      CALL WRMSGS( INDX )
!
!---  File read completed for Generic Eclipse File ---
!
  214 CONTINUE
      DO 224 K = 1,KFLD
        DO 222 J = 1,JFLD
          DO 220 I = 1,IFLD
            N = ND(I,J,K)
!
!---        Points 1 and 5  ---
!
            IX = I
            JX = J
            L = (JX-1)*(IFLDX+1)*6 + (IX-1)*6
            XE(1,N) = XPETX(L+1)*VARX
            YE(1,N) = XPETX(L+2)*VARX
            XE(5,N) = XPETX(L+1)*VARX
            YE(5,N) = XPETX(L+2)*VARX
            ZTX = -XPETX(L+3)*VARX
            ZBX = -XPETX(L+6)*VARX
            IF( ZBX.GT.ZTX ) THEN
              INDX = 7
              IMSG = (J-1)*(IFLD+1) + I
              CHMSG = 'STOMP-ECLIPSE Grid Reversal: COORD Line'
              CALL WRMSGS( INDX )
            ENDIF                 
!
!---        Points 2 and 6  ---
!
            IX = I+1
            JX = J
            L = (JX-1)*(IFLDX+1)*6 + (IX-1)*6
            XE(2,N) = XPETX(L+1)*VARX
            YE(2,N) = XPETX(L+2)*VARX
            XE(6,N) = XPETX(L+1)*VARX
            YE(6,N) = XPETX(L+2)*VARX
            ZTX = -XPETX(L+3)*VARX
            ZBX = -XPETX(L+6)*VARX
            IF( ZBX.GT.ZTX ) THEN
              INDX = 7
              IMSG = (J-1)*(IFLD+1) + I
              CHMSG = 'STOMP-ECLIPSE Grid Reversal: COORD Line'
              CALL WRMSGS( INDX )
            ENDIF                 
!
!---        Points 3 and 7  ---
!
            IX = I
            JX = J+1
            L = (JX-1)*(IFLDX+1)*6 + (IX-1)*6
            XE(3,N) = XPETX(L+1)*VARX
            YE(3,N) = XPETX(L+2)*VARX
            XE(7,N) = XPETX(L+1)*VARX
            YE(7,N) = XPETX(L+2)*VARX
            ZTX = -XPETX(L+3)*VARX
            ZBX = -XPETX(L+6)*VARX
            IF( ZBX.GT.ZTX ) THEN
              INDX = 7
              IMSG = (J-1)*(IFLD+1) + I
              CHMSG = 'STOMP-ECLIPSE Grid Reversal: COORD Line'
              CALL WRMSGS( INDX )
            ENDIF                 
!
!---        Points 4 and 8  ---
!
            IX = I+1
            JX = J+1
            L = (JX-1)*(IFLDX+1)*6 + (IX-1)*6
            XE(4,N) = XPETX(L+1)*VARX
            YE(4,N) = XPETX(L+2)*VARX
            XE(8,N) = XPETX(L+1)*VARX
            YE(8,N) = XPETX(L+2)*VARX
            ZTX = -XPETX(L+3)*VARX
            ZBX = -XPETX(L+6)*VARX
            IF( ZBX.GT.ZTX ) THEN
              INDX = 7
              IMSG = (J-1)*(IFLD+1) + I
              CHMSG = 'STOMP-ECLIPSE Grid Reversal: COORD Line'
              CALL WRMSGS( INDX )
            ENDIF                 
  220     CONTINUE
  222   CONTINUE
  224 CONTINUE
!
!---  Search file for key word "ZCORN"  ---
!
  290 READ(9,'(A)',END=300) CHDUM   
      CALL LCASE( CHDUM )
!
!---  Key word "ZCORN" found in main file  ---
!
      IF( INDEX(CHDUM(1:),'zcorn').NE.0 ) THEN
        GOTO 310
!
!---  Search for key word "ZCORN" found in include file  ---
!
      ELSEIF( INDEX(CHDUM(1:),'include').NE.0 ) THEN
        READ(9,'(A)',END=200) CHDUM
        ISTX = INDEX( CHDUM(1:),'''' )
        ISTX = ISTX + 1
        ISPX = INDEX( CHDUM(ISTX:),'''' ) + ISTX - 1
        BDUM = ' '
        ISPX = ISPX - 1
        NCH = ISPX-ISTX+1
        IF( NCH.LT.1 ) THEN
          INDX = 4
          CHMSG = 'Unrecognized File Name after INCLUDE'
          CALL WRMSGS( INDX )
        ENDIF
        READ(CHDUM(ISTX:ISPX),'(A)') BDUM(1:NCH)
        INQUIRE( FILE=BDUM(1:NCH), FORM=BMDUM, EXIST=BCHK )
        IF( .NOT.BCHK ) THEN
          INDX = 4
          CHMSG = 'Missing Include File: ' // BDUM(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
        OPEN( UNIT=10,FILE=BDUM(1:NCH),STATUS='OLD',FORM='FORMATTED' )
        REWIND( UNIT=10 )
  292   READ(10,'(A)',END=200) BHDUM
        CALL LCASE( BHDUM )
!
!---    Skip header information in include file  ---
!
        IF( INDEX(BHDUM(1:2),'--').NE.0 ) THEN
          GOTO 292
!
!---    Key word "ZCORN" found in include file  ---
!
        ELSEIF( INDEX(BHDUM(1:),'zcorn').NE.0 ) THEN
          WRITE(IWR,'(2A)') '  ZCORN Data in Include File: ',BDUM(1:NCH)
          IFILE = 10
          GOTO 310
        ELSEIF( SCAN(BHDUM(1:),'0123456789').NE.0 ) THEN 
          CLOSE( UNIT=10 )
          IFILE = 9
        ELSE
          GOTO 292
        ENDIF
      ELSE
        GOTO 290
      ENDIF
  300 CONTINUE
!
!---  Key word "ZCORN" not found  ---
!
      INDX = 18
      CHMSG = 'Missing Z-Corner Data'
      CALL WRMSGS( INDX )
  310 CONTINUE
!
!---  Loop over the z corner data for Petrel version 2011 ---
!
      NCORN = IFLD*JFLD*KFLD*8
      READ(IFILE,*,END=312) ZCORNX(1:NCORN)
!
!---  Close include file  ---
!
      IF( IFILE.EQ.10 ) THEN
        CLOSE( UNIT=10 )
        IFILE = 9
      ENDIF
      GOTO 314
!
!---  End-of-file error for Generic Eclipse File ---
!
  312 CONTINUE
      INDX = 18
      CHMSG = 'Generic Eclipse EOF: ZCORN'
      CALL WRMSGS( INDX )
  314 CONTINUE
      NX = 0
!
!---  Loop from the top tier down  ---
!
      DO 340 K = KFLD,1,-1
!
!---    Upper ZE values  ---
!
        DO 324 J = 1,JFLD
          DO 320 I = 1,IFLD
            N = ND(I,J,K)
            NX = NX + 1
            ZE(5,N) = -ZCORNX(NX)*VARX
            NX = NX + 1
            ZE(6,N) = -ZCORNX(NX)*VARX
  320     CONTINUE
          DO 322 I = 1,IFLD
            N = ND(I,J,K)
            NX = NX + 1
            ZE(7,N) = -ZCORNX(NX)*VARX
            NX = NX + 1
            ZE(8,N) = -ZCORNX(NX)*VARX
  322     CONTINUE
  324   CONTINUE
!
!---    Lower ZE values  ---
!
        DO 334 J = 1,JFLD
          DO 330 I = 1,IFLD
            N = ND(I,J,K)
            NX = NX + 1
            ZE(1,N) = -ZCORNX(NX)*VARX
            NX = NX + 1
            ZE(2,N) = -ZCORNX(NX)*VARX
  330     CONTINUE
          DO 332 I = 1,IFLD
            N = ND(I,J,K)
            NX = NX + 1
            ZE(3,N) = -ZCORNX(NX)*VARX
            NX = NX + 1
            ZE(4,N) = -ZCORNX(NX)*VARX
  332     CONTINUE
  334   CONTINUE
  340 CONTINUE
!
!---  Set minimum z grid spacing, honoring the top
!     surface  ---
!
      DO 430 J = 1,JFLD
        DO 420 I = 1,IFLD
!
!---      Loop over four vertical corners of the hexahedral node  ---
!
          DO 410 M = 1,4
!
!---        Loop over nodes vertically top to bottom  ---
!
            DO 402 K = KFLD,1,-1
              N = ND(I,J,K)
              DZX = ZE(M+4,N)-ZE(M,N)
!
!---          Spacing of vertical corner < than specified minimum  ---
!
              IF( DZX.LT.DZMINX ) THEN
!
!---            Push lower vertices downward  ---
!
                ZE(M,N) = ZE(M,N) - DZMINX
                IF( K.GT.1 ) THEN
                  DO 400 KX = K-1,1,-1
                    NX = ND(I,J,KX)
                    ZE(M+4,NX) = ZE(M+4,NX) - DZMINX
                    ZE(M,NX) = ZE(M,NX) - DZMINX
  400             CONTINUE
                ENDIF
              ENDIF
  402       CONTINUE
  410     CONTINUE
  420   CONTINUE
  430 CONTINUE
!
!---  Transpose y grid  ---
!
      DO 470 K = 1,KFLD
        DO 460 I = 1,IFLD
          DO 440 J = 1,JFLD
            N = ND(I,J,K)
            DO 432 M = 1,8
              XX(M,J) = XE(M,N)
              YX(M,J) = YE(M,N)
              ZX(M,J) = ZE(M,N)
  432       CONTINUE
  440     CONTINUE
          DO 450 J = 1,JFLD
            JX = JFLD-J+1
            N = ND(I,J,K)
            XE(1,N) = XX(3,JX)
            YE(1,N) = YX(3,JX)
            ZE(1,N) = ZX(3,JX) 
            XE(2,N) = XX(4,JX)
            YE(2,N) = YX(4,JX)
            ZE(2,N) = ZX(4,JX) 
            XE(3,N) = XX(1,JX)
            YE(3,N) = YX(1,JX)
            ZE(3,N) = ZX(1,JX) 
            XE(4,N) = XX(2,JX)
            YE(4,N) = YX(2,JX)
            ZE(4,N) = ZX(2,JX) 
            XE(5,N) = XX(7,JX)
            YE(5,N) = YX(7,JX)
            ZE(5,N) = ZX(7,JX) 
            XE(6,N) = XX(8,JX)
            YE(6,N) = YX(8,JX)
            ZE(6,N) = ZX(8,JX) 
            XE(7,N) = XX(5,JX)
            YE(7,N) = YX(5,JX)
            ZE(7,N) = ZX(5,JX) 
            XE(8,N) = XX(6,JX)
            YE(8,N) = YX(6,JX)
            ZE(8,N) = ZX(6,JX) 
  450     CONTINUE
  460   CONTINUE
  470 CONTINUE
!
!---  Dynamic memory deallocation  ---
!
      DEALLOCATE( XX,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: XX'
        CALL WRMSGS( INDX )
      ENDIF
      DEALLOCATE( YX,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: YX'
        CALL WRMSGS( INDX )
      ENDIF
      DEALLOCATE( ZX,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: ZX'
        CALL WRMSGS( INDX )
      ENDIF
      DEALLOCATE( XPETX,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: XPETX'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Close Generic Eclipse File   ---
!
      CLOSE( UNIT=9 )
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDECLGRID group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDELMGRID
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
!     Read grid elements and vertices.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 23 November 2011.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
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
      REAL*8 VDX(8),VARX(3)
      REAL*8, DIMENSION(:,:), ALLOCATABLE :: VX
      INTEGER IVX(8),JVX(8)
      CHARACTER*64 FDUM,FMDUM,UNTS
      CHARACTER*512 CHDUM
      LOGICAL FCHK
!
!----------------------Data Statements---------------------------------!
!
      SAVE JVX
      DATA JVX / 1,2,4,3,5,6,8,7 /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDELMGRID'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Read external vertices file name  ---
!
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      VARB = 'External Vertices File Name'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
!
!---  Check for external file  ---
!
      INQUIRE( FILE=FDUM(1:NCH), FORM=FMDUM, EXIST=FCHK )
      IF( .NOT.FCHK ) THEN
        INDX = 4
        CHMSG = 'Missing Vertices File: ' // FDUM(1:NCH)
        CALL WRMSGS( INDX )
      ELSEIF( FDUM.EQ.'unformatted' ) THEN
        INDX = 4
        CHMSG = 'Vertices File Format: ' // FDUM(1:NCH)
        CALL WRMSGS( INDX )
      ENDIF
      OPEN( UNIT=9,FILE=FDUM(1:NCH),STATUS='OLD',FORM='FORMATTED' )
      REWIND( UNIT=9 )
!
!---  Number of vertices  ---
!
      VARB = 'Number of Vertices'
      CALL RDINT( ISTART,ICOMMA,CHDUM,NVX )
!
!---  Dynamic memory allocation  ---
!
      ALLOCATE( VX(1:3,1:NVX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VX'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Read vertice units  ---
!
      ISTART = 1
      READ(9,'(A)') CHDUM
      CALL LCASE( CHDUM )
      ISTART = INDEX( CHDUM(ISTART:), 'x[' ) + ISTART + 1
      ICOMMA = INDEX( CHDUM(ISTART:), ']' ) + ISTART - 1
      UNTS = ' '
      NCH = ICOMMA-ISTART
      ISTOP = ICOMMA-1
      READ (CHDUM(ISTART:ISTOP), '(A)') UNTS(1:NCH)
      INDX = 0
      VARX(1) = 1.D+0
      IUNM = 1
      CALL RDUNIT(UNTS,VARX(1),INDX)
      ISTART = ICOMMA + 1
      ISTART = INDEX( CHDUM(ISTART:), 'y[' ) + ISTART + 1
      ICOMMA = INDEX( CHDUM(ISTART:), ']' ) + ISTART - 1
      UNTS = ' '
      NCH = ICOMMA-ISTART
      ISTOP = ICOMMA-1
      READ (CHDUM(ISTART:ISTOP), '(A)') UNTS(1:NCH)
      INDX = 0
      VARX(2) = 1.D+0
      IUNM = 1
      CALL RDUNIT(UNTS,VARX(2),INDX)
      ISTART = ICOMMA + 1
      ISTART = INDEX( CHDUM(ISTART:), 'z[' ) + ISTART + 1
      ICOMMA = INDEX( CHDUM(ISTART:), ']' ) + ISTART - 1
      UNTS = ' '
      NCH = ICOMMA-ISTART
      ISTOP = ICOMMA-1
      READ (CHDUM(ISTART:ISTOP), '(A)') UNTS(1:NCH)
      INDX = 0
      VARX(3) = 1.D+0
      IUNM = 1
      CALL RDUNIT(UNTS,VARX(3),INDX)
!
!---  Read vertices  ---
!
      DO 100 NX = 1,NVX
        READ(9,*) N,(VDX(M),M=1,3)
        DO 10 M = 1,3
          VX(M,N) = VDX(M)
   10   CONTINUE
  100 CONTINUE
!
!---  Close vertices file   ---
!
      CLOSE( UNIT=9 )
!
!---  Read external elements file name  ---
!
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      VARB = 'External Elements File Name'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
!
!---  Check for external file  ---
!
      INQUIRE( FILE=FDUM(1:NCH), FORM=FMDUM, EXIST=FCHK )
      IF( .NOT.FCHK ) THEN
        INDX = 4
        CHMSG = 'Missing Elements File: ' // FDUM(1:NCH)
        CALL WRMSGS( INDX )
      ELSEIF( FDUM.EQ.'unformatted' ) THEN
        INDX = 4
        CHMSG = 'Elements File Format: ' // FDUM(1:NCH)
        CALL WRMSGS( INDX )
      ENDIF
      OPEN( UNIT=9,FILE=FDUM(1:NCH),STATUS='OLD',FORM='FORMATTED' )
      REWIND( UNIT=9 )
!
!---  Assign vertices  ---
!
      DO 200 NX = 1,LFD
        READ(9,*) N,(IVX(M),M=1,8)
        DO 110 M = 1,8
          XE(M,N) = VX(1,IVX(M))*VARX(1)
          YE(M,N) = VX(2,IVX(M))*VARX(2)
          ZE(M,N) = VX(3,IVX(M))*VARX(3)
  110   CONTINUE
  200 CONTINUE
!
!---  Close elements File   ---
!
      CLOSE( UNIT=9 )
!!
!!---  Johansen grid and property  ---
!!
!      NC = 0
!      JVX(1) = 1
!      JVX(2) = 2
!      JVX(3) = 4
!      JVX(4) = 3
!      JVX(5) = 5
!      JVX(6) = 6
!      JVX(7) = 8
!      JVX(8) = 7
!      DO 206 I = 40,78
!        DO 204 J = 1,39
!          DO 202 K = 1,9
!            N = ND(I,J,K)
!            NC = NC + 1
!            IZ2(N) = NC
!            READ(9,*) NX,(IVX(M),M=1,8)
!            DO 200 M = 1,8
!              XE(JVX(M),N) = VX(1,IVX(M))*VARX(1)
!              YE(JVX(M),N) = VX(2,IVX(M))*VARX(2)
!              ZE(JVX(M),N) = VX(3,IVX(M))*VARX(3)
!  200       CONTINUE
!  202     CONTINUE
!  204   CONTINUE
!  206 CONTINUE
!      JVX(1) = 4
!      JVX(2) = 2
!      JVX(3) = 6
!      JVX(4) = 8
!      JVX(5) = 3
!      JVX(6) = 1
!      JVX(7) = 5
!      JVX(8) = 7
!      DO 216 J = 1,39
!        DO 214 K = 1,9
!          DO 212 I = 39,1,-1
!            N = ND(I,J,K)
!            NC = NC + 1
!            IZ2(N) = NC
!            READ(9,*) NX,(IVX(M),M=1,8)
!            DO 210 M = 1,8
!              XE(JVX(M),N) = VX(1,IVX(M))*VARX(1)
!              YE(JVX(M),N) = VX(2,IVX(M))*VARX(2)
!              ZE(JVX(M),N) = VX(3,IVX(M))*VARX(3)
!  210       CONTINUE
!  212     CONTINUE
!  214   CONTINUE
!  216 CONTINUE
!      JVX(1) = 2
!      JVX(2) = 1
!      JVX(3) = 5
!      JVX(4) = 6
!      JVX(5) = 4
!      JVX(6) = 3
!      JVX(7) = 7
!      JVX(8) = 8
!      DO 226 I = 40,78
!        DO 224 K = 1,9
!          DO 222 J = 40,78
!            N = ND(I,J,K)
!            NC = NC + 1
!            IZ2(N) = NC
!            READ(9,*) NX,(IVX(M),M=1,8)
!            DO 220 M = 1,8
!              XE(JVX(M),N) = VX(1,IVX(M))*VARX(1)
!              YE(JVX(M),N) = VX(2,IVX(M))*VARX(2)
!              ZE(JVX(M),N) = VX(3,IVX(M))*VARX(3)
!  220       CONTINUE
!  222     CONTINUE
!  224   CONTINUE
!  226 CONTINUE
!      JVX(1) = 6
!      JVX(2) = 2
!      JVX(3) = 1
!      JVX(4) = 5
!      JVX(5) = 8
!      JVX(6) = 4
!      JVX(7) = 3
!      JVX(8) = 7
!      DO 236 K = 1,9
!        DO 234 I = 39,1,-1
!          DO 232 J = 40,78
!            N = ND(I,J,K)
!            NC = NC + 1
!            IZ2(N) = NC
!            READ(9,*) NX,(IVX(M),M=1,8)
!            DO 230 M = 1,8
!              XE(JVX(M),N) = VX(1,IVX(M))*VARX(1)
!              YE(JVX(M),N) = VX(2,IVX(M))*VARX(2)
!              ZE(JVX(M),N) = VX(3,IVX(M))*VARX(3)
!  230       CONTINUE
!  232     CONTINUE
!  234   CONTINUE
!  236 CONTINUE
!      JVX(1) = 1
!      JVX(2) = 2
!      JVX(3) = 4
!      JVX(4) = 3
!      JVX(5) = 5
!      JVX(6) = 6
!      JVX(7) = 8
!      JVX(8) = 7
!      OPEN(UNIT=26, FILE='vertices_johansen_stomp.dat', 
!     &  STATUS='UNKNOWN', FORM='FORMATTED')
!      CLOSE(UNIT=26,STATUS='DELETE')
!      OPEN(UNIT=26, FILE='vertices_johansen_stomp.dat', 
!     &  STATUS='NEW', FORM='FORMATTED')
!      WRITE(26,'(A)') 'n       X[m]         Y[m]         Z[m]'
!      NC = 0
!      DO 242 N = 1,NFLD
!        DO 240 M = 1,8
!          NC = NC + 1
!          WRITE(26,'(I6,3(1X,1PE12.5))') NC,XE(JVX(M),N),
!     &      YE(JVX(M),N),ZE(JVX(M),N)
!  240   CONTINUE
!  242 CONTINUE
!      CLOSE( UNIT=26 )
!      OPEN(UNIT=26, FILE='elements_johansen_stomp.dat', 
!     &  STATUS='UNKNOWN', FORM='FORMATTED')
!      CLOSE(UNIT=26,STATUS='DELETE')
!      OPEN(UNIT=26, FILE='elements_johansen_stomp.dat', 
!     &  STATUS='NEW', FORM='FORMATTED')
!      DO 244 N = 1,NFLD
!        NX = (N-1)*8
!        WRITE(26,'(I6,8(1X,I6))') N,((NX+JVX(M)),M = 1,8)
!  244 CONTINUE
!      CLOSE( UNIT=26 )
!!
!!---  Close elements File   ---
!!
!      CLOSE( UNIT=9 )
!!
!!---  Check for external file  ---
!!
!      INQUIRE( FILE='properties_johansen.dat', FORM=FMDUM, EXIST=FCHK )
!      IF( .NOT.FCHK ) THEN
!        INDX = 4
!        CHMSG = 'Missing Property File: properties_johansen.dat'
!        CALL WRMSGS( INDX )
!      ELSEIF( FDUM.EQ.'unformatted' ) THEN
!        INDX = 4
!        CHMSG = 'Property File Format: properties_johansen.dat'
!        CALL WRMSGS( INDX )
!      ENDIF
!      OPEN( UNIT=9,FILE='properties_johansen.dat',
!     &  STATUS='OLD',FORM='FORMATTED' )
!      REWIND( UNIT=9 )
!      READ(9,'(A)') CHDUM
!!
!!---  Read porosity and permeability (mD)  ---
!!
!      DO 302 NX = 1,NVX
!        READ(9,*) VAX,VBX,VCX,(VDX(M),M=1,2)
!        DO 300 M = 1,2
!          VX(M,NX) = VDX(M)
!  300   CONTINUE
!  302 CONTINUE
!      CLOSE( UNIT=9 )
!      INQUIRE( FILE=FDUM(1:NCH), FORM=FMDUM, EXIST=FCHK )
!      IF( .NOT.FCHK ) THEN
!        INDX = 4
!        CHMSG = 'Missing Elements File: ' // FDUM(1:NCH)
!        CALL WRMSGS( INDX )
!      ELSEIF( FDUM.EQ.'unformatted' ) THEN
!        INDX = 4
!        CHMSG = 'Elements File Format: ' // FDUM(1:NCH)
!        CALL WRMSGS( INDX )
!      ENDIF
!      OPEN( UNIT=9,FILE=FDUM(1:NCH),STATUS='OLD',FORM='FORMATTED' )
!      REWIND( UNIT=9 )
!      JVX(1) = 1
!      JVX(2) = 2
!      JVX(3) = 4
!      JVX(4) = 3
!      JVX(5) = 5
!      JVX(6) = 6
!      JVX(7) = 8
!      JVX(8) = 7
!      DO 406 I = 40,78
!        DO 404 J = 1,39
!          DO 402 K = 1,9
!            N = ND(I,J,K)
!            READ(9,*) NX,(IVX(M),M=1,8)
!            PORD(1,N) = 0.D+0
!            PORD(2,N) = 0.D+0
!            DO 400 M = 1,8
!              PORD(1,N) = PORD(1,N) + VX(1,IVX(M))
!              PORD(2,N) = PORD(2,N) + VX(2,IVX(M))
!  400       CONTINUE
!            PORD(1,N) = PORD(1,N)/8.D+0
!            PORD(2,N) = PORD(2,N)/8.D+0
!  402     CONTINUE
!  404   CONTINUE
!  406 CONTINUE
!      JVX(1) = 4
!      JVX(2) = 2
!      JVX(3) = 6
!      JVX(4) = 8
!      JVX(5) = 3
!      JVX(6) = 1
!      JVX(7) = 5
!      JVX(8) = 7
!      DO 416 J = 1,39
!        DO 414 K = 1,9
!          DO 412 I = 39,1,-1
!            N = ND(I,J,K)
!            READ(9,*) NX,(IVX(M),M=1,8)
!            PORD(1,N) = 0.D+0
!            PORD(2,N) = 0.D+0
!            DO 410 M = 1,8
!              PORD(1,N) = PORD(1,N) + VX(1,IVX(M))
!              PORD(2,N) = PORD(2,N) + VX(2,IVX(M))
!  410       CONTINUE
!            PORD(1,N) = PORD(1,N)/8.D+0
!            PORD(2,N) = PORD(2,N)/8.D+0
!  412     CONTINUE
!  414   CONTINUE
!  416 CONTINUE
!      JVX(1) = 2
!      JVX(2) = 1
!      JVX(3) = 5
!      JVX(4) = 6
!      JVX(5) = 4
!      JVX(6) = 3
!      JVX(7) = 7
!      JVX(8) = 8
!      DO 426 I = 40,78
!        DO 424 K = 1,9
!          DO 422 J = 40,78
!            N = ND(I,J,K)
!            READ(9,*) NX,(IVX(M),M=1,8)
!            PORD(1,N) = 0.D+0
!            PORD(2,N) = 0.D+0
!            DO 420 M = 1,8
!              PORD(1,N) = PORD(1,N) + VX(1,IVX(M))
!              PORD(2,N) = PORD(2,N) + VX(2,IVX(M))
!  420       CONTINUE
!            PORD(1,N) = PORD(1,N)/8.D+0
!            PORD(2,N) = PORD(2,N)/8.D+0
!  422     CONTINUE
!  424   CONTINUE
!  426 CONTINUE
!      JVX(1) = 6
!      JVX(2) = 2
!      JVX(3) = 1
!      JVX(4) = 5
!      JVX(5) = 8
!      JVX(6) = 4
!      JVX(7) = 3
!      JVX(8) = 7
!      DO 436 K = 1,9
!        DO 434 I = 39,1,-1
!          DO 432 J = 40,78
!            N = ND(I,J,K)
!            READ(9,*) NX,(IVX(M),M=1,8)
!            PORD(1,N) = 0.D+0
!            PORD(2,N) = 0.D+0
!            DO 430 M = 1,8
!              PORD(1,N) = PORD(1,N) + VX(1,IVX(M))
!              PORD(2,N) = PORD(2,N) + VX(2,IVX(M))
!  430       CONTINUE
!            PORD(1,N) = PORD(1,N)/8.D+0
!            PORD(2,N) = PORD(2,N)/8.D+0
!  432     CONTINUE
!  434   CONTINUE
!  436 CONTINUE
!      OPEN(UNIT=26, FILE='porosity_johansen_stomp.dat', 
!     &  STATUS='UNKNOWN', FORM='FORMATTED')
!      CLOSE(UNIT=26,STATUS='DELETE')
!      OPEN(UNIT=26, FILE='porosity_johansen_stomp.dat', 
!     &  STATUS='NEW', FORM='FORMATTED')
!      DO 442 N = 1,NFLD
!          WRITE(26,'(1PE12.5)') PORD(1,N)
!  442 CONTINUE
!      CLOSE( UNIT=26 )
!      OPEN(UNIT=26, FILE='permeability_johansen_stomp.dat', 
!     &  STATUS='UNKNOWN', FORM='FORMATTED')
!      CLOSE(UNIT=26,STATUS='DELETE')
!      OPEN(UNIT=26, FILE='permeability_johansen_stomp.dat', 
!     &  STATUS='NEW', FORM='FORMATTED')
!      DO 444 N = 1,NFLD
!          WRITE(26,'(1PE12.5)') PORD(2,N)
!  444 CONTINUE
!      CLOSE( UNIT=26 )
!!
!!---  Johansen grid and property  ---
!!
!---  Dynamic memory deallocation  ---
!
      DEALLOCATE( VX,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: VX'
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDELMGRID group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDUNFGRID

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
!     Read grids with uniform spacing.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 28 November 2011.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GRID
      USE FILES
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
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDUNFGRID'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  First coordinate direction ---
!
      IF( ICS.EQ.5 ) THEN
        WRITE(IWR,'(/,A)')'X-Direction Coordinates'
      ELSEIF( ICS.EQ.6 ) THEN
        WRITE(IWR,'(/,A)')'R-Direction Coordinates'
      ENDIF
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      IF( ICS.EQ.6 ) THEN
        VARB = 'Radial Node Dimension'
      ELSE
        VARB = 'X Node Dimension'
      ENDIF
      CALL RDDPR(ISTART,ICOMMA,CHDUM,XSPC)
      IF( ICS.EQ.6 ) THEN
        VARB = 'Radial Node Dimension Units'
      ELSE
        VARB = 'X Node Dimension Units'
      ENDIF
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      I = 1
      N = ND(I,1,1)
      XE(1,N) = 0.D+0
      IF( ICS.EQ.6 ) XE(1,N) = SMALL
      XE(2,N) = XSPC
      WRITE(IWR,'(2X,A,I4,3A,1PE11.4)')'X(',I,'), ',
     &  UNTS(1:NCH),': ',XE(I,1)
      IW = 2
      DO 100 I = 2,IFLD
        N = ND(I,1,1)
        NW = N-1
        XE(1,N) = XE(2,NW)
        XE(2,N) = XE(1,N) + XSPC
        IF( IW.EQ.5 ) THEN
          IF( ICS.EQ.6 ) THEN
            WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Rad(',I,'), ',
     &        UNTS(1:NCH),': ',XE(1,N)
          ELSE
            WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'X(',I,'), ',
     &        UNTS(1:NCH),': ',XE(1,N)
          ENDIF
          IW = 1
        ELSE
          IF( ICS.EQ.6 ) THEN
            WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Rad(',I,'), ',
     &        UNTS(1:NCH),': ',XE(1,N)
          ELSE
            WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'X(',I,'), ',
     &        UNTS(1:NCH),': ',XE(1,N)
          ENDIF
          IW = IW+1
        ENDIF
 100  CONTINUE
      IF( ICS.EQ.6 ) THEN
        WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Rad(',I,'), ',
     &    UNTS(1:NCH),': ',XE(2,N)
      ELSE
        WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'X(',I,'), ',
     &    UNTS(1:NCH),': ',XE(2,N)
      ENDIF
      IF( ABS(XE(2,N))/EPSL.LT.EPSL ) THEN
        INDX = 4
        CHMSG = 'Zero X-Direction Domain'
        CALL WRMSGS( INDX )
      ENDIF
      INDX = 0
      DO 110 I = 1,IFLD
        N = ND(I,1,1)
        IUNM = 1
        CALL RDUNIT(UNTS,XE(1,N),INDX)
        IUNM = 1
        CALL RDUNIT(UNTS,XE(2,N),INDX)
 110  CONTINUE
      DO 140 K = 1,KFLD
      DO 140 J = 1,JFLD
      DO 140 I = 1,IFLD
        N = ND(I,J,K)
        NI = ND(I,1,1)
        XE(1,N) = XE(1,NI)
        XE(3,N) = XE(1,NI)
        XE(5,N) = XE(1,NI)
        XE(7,N) = XE(1,NI)
        XE(2,N) = XE(2,NI)
        XE(4,N) = XE(2,NI)
        XE(6,N) = XE(2,NI)
        XE(8,N) = XE(2,NI)
 140  CONTINUE
      DO 150 K = 1,KFLD
      DO 150 J = 1,JFLD
      DO 150 I = 1,IFLD
        N = ND(I,J,K)
        NW = N-1
        NPX = NSX(N)
        NQX = NSX(N)+1
        IF( INBS(4,N).NE.0 ) NQX = INBS(4,N)
        XP(N) = 5.D-1*(XE(1,N)+XE(2,N))
        DXGF(N) = XE(2,N)-XE(1,N)
        RP(I) = 1.D+0
        IF( ICS.EQ.6 ) RP(I) = XP(N)
        IF( I.EQ.1 .OR. INBS(3,N).EQ.0 ) THEN
          DXGP(NPX) = XP(N)-XE(1,N)
          GRVX(NPX) = GRAVX
        ENDIF
        IF( I.EQ.IFLD .OR. INBS(4,N).EQ.0 ) THEN
          DXGP(NQX) = XE(2,N)-XP(N)
          GRVX(NQX) = GRAVX
        ENDIF
        IF( I.GT.1 .AND. IFLD.GT.1 .AND. INBS(3,N).EQ.0 ) THEN
          DXGP(NPX) = XP(N)-XP(NW)
          GRVX(NPX) = GRAVX
        ENDIF
  150 CONTINUE
!
!---  Second coordinate direction  ---
!
      IF( ICS.EQ.5 ) THEN
        WRITE(IWR,'(/,A)')'Y-Direction Coordinates'
      ELSEIF( ICS.EQ.6 ) THEN
        WRITE(IWR,'(/,A)')'Azimuthal-Direction Coordinates'
      ENDIF
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      IF( ICS.EQ.6 ) THEN
        VARB = 'Azimuthal Node Dimension'
      ELSE
        VARB = 'Y Node Dimension'
      ENDIF
      CALL RDDPR(ISTART,ICOMMA,CHDUM,YSPC)
      IF( ICS.EQ.6 ) THEN
        VARB = 'Azimuthal Node Dimension Units'
      ELSE
        VARB = 'Y Node Dimension Units'
      ENDIF
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      J = 1
      N = ND(1,J,1)
      YE(1,N) = 0.D+0
      YE(3,N) = YSPC
      WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Y(',J,'), ',
     &  UNTS(1:NCH),': ',YE(1,1)
      IW = 2
      DO 200 J = 2,JFLD
        N = ND(1,J,1)
        NS = N-IFLD
        YE(1,N) = YE(3,NS)
        YE(3,N) = YE(1,N) + YSPC
        IF( IW.EQ.5 ) THEN
          IF( ICS.EQ.6 ) THEN
            WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Azim(',J,'), ',
     &        UNTS(1:NCH),': ',YE(1,N)
          ELSE
            WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Y(',J,'), ',
     &        UNTS(1:NCH),': ',YE(1,N)
          ENDIF
          IW = 1
        ELSE
          IF( ICS.EQ.6 ) THEN
            WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Azim(',J,'), ',
     &        UNTS(1:NCH),': ',YE(1,N)
          ELSE
            WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Y(',J,'), ',
     &      UNTS(1:NCH),': ',YE(1,N)
          ENDIF
          IW = IW+1
        ENDIF
 200  CONTINUE
      IF( ICS.EQ.6 ) THEN
        WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Azim(',J,'), ',
     &    UNTS(1:NCH),': ',YE(3,N)
      ELSE
        WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Y(',J,'), ',
     &    UNTS(1:NCH),': ',YE(3,N)
      ENDIF
      IF( ABS(YE(3,N))/EPSL.LT.EPSL ) THEN
        INDX = 4
        CHMSG = 'Zero Y-Direction Domain'
        CALL WRMSGS( INDX )
      ENDIF
      INDX = 0
      DO 210 J = 1,JFLD
        N = ND(1,J,1)
        IUNM = 1
        IF( ICS.EQ.6 ) IUNM = 0
        CALL RDUNIT(UNTS,YE(1,N),INDX)
        IUNM = 1
        IF( ICS.EQ.6 ) IUNM = 0
        CALL RDUNIT(UNTS,YE(3,N),INDX)
  210 CONTINUE
      DO 240 K = 1,KFLD
      DO 240 J = 1,JFLD
      DO 240 I = 1,IFLD
        N = ND(I,J,K)
        NJ = ND(1,J,1)
        YE(1,N) = YE(1,NJ)
        YE(2,N) = YE(1,NJ)
        YE(5,N) = YE(1,NJ)
        YE(6,N) = YE(1,NJ)
        YE(3,N) = YE(3,NJ)
        YE(4,N) = YE(3,NJ)
        YE(7,N) = YE(3,NJ)
        YE(8,N) = YE(3,NJ)
 240  CONTINUE
      DO 250 K = 1,KFLD
      DO 250 J = 1,JFLD
      DO 250 I = 1,IFLD
        N = ND(I,J,K)
        NS = N-IFLD
        NPY = NSY(N)
        NQY = NSY(N)+IFLD
        IF( INBS(5,N).NE.0 ) NQY = INBS(5,N)
        YP(N) = 5.D-1*(YE(1,N)+YE(3,N))
        DYGF(N) = YE(3,N)-YE(1,N)
        IF( J.EQ.1 .OR. INBS(2,N).NE.0 ) THEN
          DYGP(NPY) = YP(N)-YE(1,N)
          GRVY(NPY) = GRAVY
        ENDIF
        IF( J.EQ.JFLD .OR. INBS(5,N).NE.0 ) THEN
          DYGP(NQY) = YE(3,N)-YP(N)
          GRVY(NQY) = GRAVY
        ENDIF
        IF( J.GT.1 .AND. JFLD.GT.1 .AND. INBS(2,N).EQ.0 ) THEN
          DYGP(NPY) = YP(N)-YP(NS)
          GRVY(NPY) = GRAVY
        ENDIF
  250 CONTINUE
!
!---  Third coordinate direction  ---
!
      IF( ICS.EQ.5 ) THEN
        WRITE(IWR,'(/,A)')'Z-Direction Coordinates'
      ELSEIF( ICS.EQ.6 ) THEN
        WRITE(IWR,'(/,A)')'Vertical-Direction Coordinates'
      ENDIF
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      IF( ICS.EQ.6 ) THEN
        VARB = 'Vertical Node Dimension'
      ELSE
        VARB = 'Z Node Dimension'
      ENDIF
      CALL RDDPR(ISTART,ICOMMA,CHDUM,ZSPC)
      IF( ICS.EQ.6 ) THEN
        VARB = 'Vertical Node Dimension Units'
      ELSE
        VARB = 'Z Node Dimension Units'
      ENDIF
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      K = 1
      N = ND(1,1,K)
      ZE(1,N) = 0.D+0
      ZE(5,N) = ZSPC
      WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Z(',K,'), ',
     &  UNTS(1:NCH),': ',ZE(1,1)
      IW = 2
      DO 300 K = 2,KFLD
        N = ND(1,1,K)
        NB = N-IJFLD
        ZE(1,N) = ZE(5,NB)
        ZE(5,N) = ZE(1,N) + ZSPC
        IF( IW.EQ.5 ) THEN
          IF( ICS.EQ.6 ) THEN
            WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Vert(',K,'), ',
     &        UNTS(1:NCH),': ',ZE(1,N)
          ELSE
            WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Z(',K,'), ',
     &        UNTS(1:NCH),': ',ZE(1,N)
          ENDIF
          IW = 1
        ELSE
          IF( ICS.EQ.6 ) THEN
            WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Vert(',K,'), ',
     &        UNTS(1:NCH),': ',ZE(1,N)
          ELSE
            WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Z(',K,'), ',
     &        UNTS(1:NCH),': ',ZE(1,N)
          ENDIF
          IW = IW+1
        ENDIF
  300 CONTINUE
      IF( ICS.EQ.6 ) THEN
        WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Vert(',K,'), ',
     &    UNTS(1:NCH),': ',ZE(5,N)
      ELSE
        WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Z(',K,'), ',
     &    UNTS(1:NCH),': ',ZE(5,N)
      ENDIF
      IF( ABS(ZE(5,N))/EPSL.LT.EPSL ) THEN
        INDX = 4
        CHMSG = 'Zero Z-Direction Domain'
        CALL WRMSGS( INDX )
      ENDIF
      INDX = 0
      DO 310 K = 1,KFLD
        N = ND(1,1,K)
        IUNM = 1
        CALL RDUNIT(UNTS,ZE(1,N),INDX)
        IUNM = 1
        CALL RDUNIT(UNTS,ZE(5,N),INDX)
  310 CONTINUE
      DO 340 K = 1,KFLD
      DO 340 J = 1,JFLD
      DO 340 I = 1,IFLD
        N = ND(I,J,K)
        NK = ND(1,1,K)
        ZE(1,N) = ZE(1,NK)
        ZE(2,N) = ZE(1,NK)
        ZE(3,N) = ZE(1,NK)
        ZE(4,N) = ZE(1,NK)
        ZE(5,N) = ZE(5,NK)
        ZE(6,N) = ZE(5,NK)
        ZE(7,N) = ZE(5,NK)
        ZE(8,N) = ZE(5,NK)
 340  CONTINUE
        DO 350 K = 1,KFLD
        DO 350 J = 1,JFLD
        DO 350 I = 1,IFLD
          N = ND(I,J,K)
          NB = N-IJFLD
          NPZ = NSZ(N)
          NQZ = NSZ(N)+IJFLD
          IF( INBS(6,N).NE.0 ) NQZ = INBS(6,N)
          ZP(N) = 5.D-1*(ZE(1,N)+ZE(5,N))
          DZGF(N) = ZE(5,N)-ZE(1,N)
          IF( K.EQ.1 .OR. INBS(1,N).NE.0 ) THEN
            DZGP(NPZ) = ZP(N)-ZE(1,N)
            GRVZ(NPZ) = GRAVZ
          ENDIF
          IF( K.EQ.KFLD .OR. INBS(6,N).NE.0 ) THEN
            DZGP(NQZ) = ZE(5,N)-ZP(N)
            GRVZ(NQZ) = GRAVZ
          ENDIF
          IF( K.GT.1 .AND. KFLD.GT.1 .AND. INBS(1,N).EQ.0 ) THEN
            DZGP(NPZ) = ZP(N)-ZP(NB)
            GRVZ(NPZ) = GRAVZ
          ENDIF
  350   CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDUNFGRID group  ---
!
      RETURN
      END
      
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDHEXGRID
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
!     Read hexahedral grid.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 23 November 2011.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GRID
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
      REAL*8, DIMENSION(:,:,:), ALLOCATABLE :: VX
      CHARACTER*64 FDUM,FMDUM,UNTS
      CHARACTER*512 CHDUM
      LOGICAL FCHK
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDHEXGRID'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Read vertices file name  ---
!
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      VARB = 'Vertices File Name'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
!
!---  Check for external file  ---
!
      INQUIRE( FILE=FDUM(1:NCH), FORM=FMDUM, EXIST=FCHK )
      IF( .NOT.FCHK ) THEN
        INDX = 4
        CHMSG = 'Missing Vertices File: ' // FDUM(1:NCH)
        CALL WRMSGS( INDX )
      ELSEIF( FDUM.EQ.'unformatted' ) THEN
        INDX = 4
        CHMSG = 'Vertices File Format: ' // FDUM(1:NCH)
        CALL WRMSGS( INDX )
      ENDIF
      OPEN( UNIT=9,FILE=FDUM(1:NCH),STATUS='OLD',FORM='FORMATTED' )
      REWIND( UNIT=9 )
      WRITE(IWR,'(/,3A)') VARB(1:IVR),': ',FDUM(1:NCH)
      READ(9,*) IFLDX,JFLDX,KFLDX
      IFLDZ = IFLDX
      JFLDZ = JFLDX
      KFLDZ = KFLDX
      IF(IFLDX.GT.1) IFLDZ = IFLDX-1
      IF(JFLDX.GT.1) JFLDZ = JFLDX-1
      IF(KFLDX.GT.1) KFLDZ = KFLDX-1
      IF( IFLDZ.NE.IFLD .OR. JFLDZ.NE.JFLD .OR. KFLDZ.NE.KFLD ) THEN
        INDX = 4
        CHMSG = 'Orthogonal Grid Dimension Conflict'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Dynamic memory allocation  ---
!
      ALLOCATE( VX(1:IFLDX,1:JFLDX,1:KFLDX),STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Allocation Error: VX'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Read length units  ---
!
      VARB = 'Grid File Length Units'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      INDX = 0
      VARX = 1.D+0
      IUNM = 1
      CALL RDUNIT(UNTS,VARX,INDX)
!
!---  Read x-vertices from file  ---
!
      IF( IFLDX.GT.1 ) THEN
        READ(9,*)(((VX(I,J,K),I=1,IFLDX),J=1,JFLDX),K=1,KFLDX)
!
!---    3D-xyz domain  ---
!
        IF( JFLDX.GT.1 .AND. KFLDX.GT.1 ) THEN
          DO 100 K = 1,KFLD
          DO 100 J = 1,JFLD
          DO 100 I = 1,IFLD
            N = ND(I,J,K)
            XE(1,N) = VX(I,J,K)*VARX
            XE(2,N) = VX(I+1,J,K)*VARX
            XE(3,N) = VX(I,J+1,K)*VARX
            XE(4,N) = VX(I+1,J+1,K)*VARX
            XE(5,N) = VX(I,J,K+1)*VARX
            XE(6,N) = VX(I+1,J,K+1)*VARX
            XE(7,N) = VX(I,J+1,K+1)*VARX
            XE(8,N) = VX(I+1,J+1,K+1)*VARX
  100     CONTINUE
!
!---    2D-xy domain, unit z dimension  ---
!
        ELSEIF( JFLDX.GT.1 ) THEN
          DO 110 J = 1,JFLD
          DO 110 I = 1,IFLD
            N = ND(I,J,1)
            XE(1,N) = VX(I,J,1)*VARX
            XE(2,N) = VX(I+1,J,1)*VARX
            XE(3,N) = VX(I,J+1,1)*VARX
            XE(4,N) = VX(I+1,J+1,1)*VARX
            XE(5,N) = VX(I,J,1)*VARX
            XE(6,N) = VX(I+1,J,1)*VARX
            XE(7,N) = VX(I,J+1,1)*VARX
            XE(8,N) = VX(I+1,J+1,1)*VARX
  110     CONTINUE
!
!---    2D-xz domain, unit y dimension  ---
!
        ELSEIF( KFLDX.GT.1 ) THEN
          DO 120 K = 1,KFLD
          DO 120 I = 1,IFLD
            N = ND(I,1,K)
            XE(1,N) = VX(I,1,K)*VARX
            XE(2,N) = VX(I+1,1,K)*VARX
            XE(3,N) = VX(I,1,K)*VARX
            XE(4,N) = VX(I+1,1,K)*VARX
            XE(5,N) = VX(I,1,K+1)*VARX
            XE(6,N) = VX(I+1,1,K+1)*VARX
            XE(7,N) = VX(I,1,K+1)*VARX
            XE(8,N) = VX(I+1,1,K+1)*VARX
  120     CONTINUE
!
!---    1D-x domain  ---
!
        ELSE
          INDX = 4
          CHMSG = 'Single Dimensioned Orthogonal Grid'
          CALL WRMSGS( INDX )
        ENDIF
!
!---  2D-yz domain, unit x dimension  ---
!
      ELSE
        DO 130 N = 1,NFLD
          XE(1,N) = 0.D+0
          XE(2,N) = 1.D+0
          XE(3,N) = 0.D+0
          XE(4,N) = 1.D+0
          XE(5,N) = 0.D+0
          XE(6,N) = 1.D+0
          XE(7,N) = 0.D+0
          XE(8,N) = 1.D+0
  130   CONTINUE
      ENDIF
!
!---  Read y-vertices from file  ---
!
      IF( JFLDX.GT.1 ) THEN
        READ(9,*)(((VX(I,J,K),I=1,IFLDX),J=1,JFLDX),K=1,KFLDX)
!
!---    3D-xyz domain  ---
!
        IF( IFLDX.GT.1 .AND. KFLDX.GT.1 ) THEN
          DO 200 K = 1,KFLD
          DO 200 J = 1,JFLD
          DO 200 I = 1,IFLD
            N = ND(I,J,K)
            YE(1,N) = VX(I,J,K)*VARX
            YE(2,N) = VX(I+1,J,K)*VARX
            YE(3,N) = VX(I,J+1,K)*VARX
            YE(4,N) = VX(I+1,J+1,K)*VARX
            YE(5,N) = VX(I,J,K+1)*VARX
            YE(6,N) = VX(I+1,J,K+1)*VARX
            YE(7,N) = VX(I,J+1,K+1)*VARX
            YE(8,N) = VX(I+1,J+1,K+1)*VARX
  200     CONTINUE
!
!---    2D-xy domain, unit z dimension  ---
!
        ELSEIF( IFLDX.GT.1 ) THEN
          DO 210 J = 1,JFLD
          DO 210 I = 1,IFLD
            N = ND(I,J,1)
            YE(1,N) = VX(I,J,1)*VARX
            YE(2,N) = VX(I+1,J,1)*VARX
            YE(3,N) = VX(I,J+1,1)*VARX
            YE(4,N) = VX(I+1,J+1,1)*VARX
            YE(5,N) = VX(I,J,1)*VARX
            YE(6,N) = VX(I+1,J,1)*VARX
            YE(7,N) = VX(I,J+1,1)*VARX
            YE(8,N) = VX(I+1,J+1,1)*VARX
  210     CONTINUE
!
!---    2D-yz domain, unit x dimension  ---
!
        ELSEIF( KFLDX.GT.1 ) THEN
          DO 220 K = 1,KFLD
          DO 220 J = 1,JFLD
            N = ND(1,J,K)
            YE(1,N) = VX(1,J,K)*VARX
            YE(2,N) = VX(1,J,K)*VARX
            YE(3,N) = VX(1,J+1,K)*VARX
            YE(4,N) = VX(1,J+1,K)*VARX
            YE(5,N) = VX(1,J,K+1)*VARX
            YE(6,N) = VX(1,J,K+1)*VARX
            YE(7,N) = VX(1,J+1,K+1)*VARX
            YE(8,N) = VX(1,J+1,K+1)*VARX
  220     CONTINUE
!
!---    1D-y domain  ---
!
        ELSE
          INDX = 4
          CHMSG = 'Single Dimensioned Orthogonal Grid'
          CALL WRMSGS( INDX )
        ENDIF
!
!---  2D-xz domain, unit y dimension  ---
!
      ELSE
        DO 230 N = 1,NFLD
          YE(1,N) = 0.D+0
          YE(2,N) = 0.D+0
          YE(3,N) = 1.D+0
          YE(4,N) = 1.D+0
          YE(5,N) = 0.D+0
          YE(6,N) = 0.D+0
          YE(7,N) = 1.D+0
          YE(8,N) = 1.D+0
  230   CONTINUE
      ENDIF
!
!---  Read z-vertices from file  ---
!
      IF( KFLDX.GT.1 ) THEN
        READ(9,*)(((VX(I,J,K),I=1,IFLDX),J=1,JFLDX),K=1,KFLDX)
!
!---    3D-xyz domain  ---
!
        IF( IFLDX.GT.1 .AND. JFLDX.GT.1 ) THEN
          DO 300 K = 1,KFLD
          DO 300 J = 1,JFLD
          DO 300 I = 1,IFLD
            N = ND(I,J,K)
            ZE(1,N) = VX(I,J,K)*VARX
            ZE(2,N) = VX(I+1,J,K)*VARX
            ZE(3,N) = VX(I,J+1,K)*VARX
            ZE(4,N) = VX(I+1,J+1,K)*VARX
            ZE(5,N) = VX(I,J,K+1)*VARX
            ZE(6,N) = VX(I+1,J,K+1)*VARX
            ZE(7,N) = VX(I,J+1,K+1)*VARX
            ZE(8,N) = VX(I+1,J+1,K+1)*VARX
  300     CONTINUE
!
!---    2D-yz domain, unit x dimension  ---
!
        ELSEIF( JFLDX.GT.1 ) THEN
          DO 310 K = 1,KFLD
          DO 310 J = 1,JFLD
            N = ND(1,J,K)
            ZE(1,N) = VX(1,J,K)*VARX
            ZE(2,N) = VX(1,J,K)*VARX
            ZE(3,N) = VX(1,J+1,K)*VARX
            ZE(4,N) = VX(1,J+1,K)*VARX
            ZE(5,N) = VX(1,J,K+1)*VARX
            ZE(6,N) = VX(1,J,K+1)*VARX
            ZE(7,N) = VX(1,J+1,K+1)*VARX
            ZE(8,N) = VX(1,J+1,K+1)*VARX
  310     CONTINUE
!
!---    2D-xz domain, unit y dimension  ---
!
        ELSEIF( KFLDX.GT.1 ) THEN
          DO 320 K = 1,KFLD
          DO 320 I = 1,IFLD
            N = ND(I,1,K)
            ZE(1,N) = VX(I,1,K)*VARX
            ZE(2,N) = VX(I+1,1,K)*VARX
            ZE(3,N) = VX(I,1,K)*VARX
            ZE(4,N) = VX(I+1,1,K)*VARX
            ZE(5,N) = VX(I,1,K+1)*VARX
            ZE(6,N) = VX(I+1,1,K+1)*VARX
            ZE(7,N) = VX(I,1,K+1)*VARX
            ZE(8,N) = VX(I+1,1,K+1)*VARX
  320     CONTINUE
!
!---    1D-z domain  ---
!
        ELSE
          INDX = 4
          CHMSG = 'Single Dimensioned Orthogonal Grid'
          CALL WRMSGS( INDX )
        ENDIF
!
!---  2D-xy domain, unit z dimension  ---
!
      ELSE
        DO 330 N = 1,NFLD
          ZE(1,N) = 0.D+0
          ZE(2,N) = 0.D+0
          ZE(3,N) = 0.D+0
          ZE(4,N) = 0.D+0
          ZE(5,N) = 1.D+0
          ZE(6,N) = 1.D+0
          ZE(7,N) = 1.D+0
          ZE(8,N) = 1.D+0
  330   CONTINUE
      ENDIF
!
!---  Deallocate vertice memory  ---
!
      DEALLOCATE( VX,STAT=ISTAT )
      IF( ISTAT.NE.0 ) THEN
        INDX = 3
        CHMSG = 'Deallocation Error: VX'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Closes Vertices File  ---
!
      CLOSE(UNIT=9)
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDHEXGRID group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDVBLGRID
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
!     Read grids with variable spacing.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 28 November 2011.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GRID
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
      CHARACTER*64 UNTS
      CHARACTER*512 CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDVBLGRID'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  First coordinate direction ---
!
      IF( ICS.EQ.1 ) THEN
        WRITE(IWR,'(/,A)')'X-Direction Coordinates'
      ELSEIF( ICS.EQ.2 ) THEN
        WRITE(IWR,'(/,A)')'R-Direction Coordinates'
      ENDIF
      IC = 0
      IW = 0
  100 CONTINUE
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      IR = IFLD+1-IC
      DO 120 I = 1,IR
        ICMX = INDEX( CHDUM(ISTART:), ',' ) + ISTART - 1
        IF( ICMX.EQ.ISTART-1 ) GOTO 100
        IATX = INDEX( CHDUM(ISTART:), '@' ) + ISTART - 1
        IF( IATX.LT.ISTART .OR. IATX.GT.ICMX ) THEN
          IC = IC + 1
          IW = IW + 1
          N = ND(MIN(IC,IFLD),1,1)
          VARB = 'X Dimension'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,VARX)
          VARB = 'X Dimension Units'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( IC.EQ.IFLD+1 ) THEN
            XE(2,N) = VARX
            IF( ICS.EQ.2 ) THEN
              WRITE(IWR,'(2X,A,I4,3A,1PE11.4)')'Rad(',IC,'), ',
     &          UNTS(1:NCH),': ',XE(2,N)
            ELSE
              WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'X(',IC,'), ',
     &          UNTS(1:NCH),': ',XE(2,N)
            ENDIF
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,XE(2,N),INDX)
            GOTO 130
          ELSEIF( IW.EQ.5 ) THEN
            XE(1,N) = VARX
            IF( ICS.EQ.2 ) THEN
              WRITE(IWR,'(2X,A,I4,3A,1PE11.4)')'Rad(',IC,'), ',
     &          UNTS(1:NCH),': ',XE(1,N)
            ELSE
              WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'X(',IC,'), ',
     &          UNTS(1:NCH),': ',XE(1,N)
            ENDIF
            IW = 0
          ELSE
            XE(1,N) = VARX
            IF( ICS.EQ.2 ) THEN
              WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Rad(',IC,'), ',
     &          UNTS(1:NCH),': ',XE(1,N)
            ELSE
              WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'X(',IC,'), ',
     &          UNTS(1:NCH),': ',XE(1,N)
            ENDIF
          ENDIF
          INDX = 0
          IUNM = 1
          CALL RDUNIT(UNTS,XE(1,N),INDX)
          IF( IC.GT.1 ) THEN
            NW = N-1
            XE(2,NW) = XE(1,N)
          ENDIF
        ELSE
          CHDUM(IATX:IATX) = ','
          VARB = 'Count Integer'
          CALL RDINT(ISTART,ICOMMA,CHDUM,IATX )
          VARB = 'X Dimension'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,DXVAR )
          VARB = 'X Dimension Units'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNM = 1
          CALL RDUNIT(UNTS,DXVAR,INDX)
          DO 110 II = 1,IATX
            IC = IC + 1
            IW = IW + 1
            N = ND(MIN(IC,IFLD),1,1)
            IF( IC.EQ.1 ) THEN
              XE(1,N) = 0.D+0
              XVAR = XE(1,N)
            ELSEIF( IC.EQ.IFLD+1 ) THEN
              XE(2,N) = XE(1,N) + DXVAR
              XVAR = XE(2,N)
            ELSE
              NW = N-1
              XE(1,N) = XE(1,NW) + DXVAR
              XVAR = XE(1,N)
            ENDIF
            INDX = 1
            IUNM = 1
            CALL RDUNIT(UNTS,XVAR,INDX )
            IF( IC.EQ.IFLD+1 ) THEN
              IF( ICS.EQ.2 ) THEN
                WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Rad(',IC,'), ',
     &            UNTS(1:NCH),': ',XVAR
              ELSE
                WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'X(',IC,'), ',
     &            UNTS(1:NCH),': ',XVAR
              ENDIF
              GOTO 130
            ELSEIF( IW.EQ.5 ) THEN
              IF( ICS.EQ.2 ) THEN
                WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Rad(',IC,'), ',
     &            UNTS(1:NCH),': ',XVAR
              ELSE
                WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'X(',IC,'), ',
     &            UNTS(1:NCH),': ',XVAR
              ENDIF
              IW = 0
            ELSE
              IF( ICS.EQ.2 ) THEN
                WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Rad(',IC,'), ',
     &            UNTS(1:NCH),': ',XVAR
              ELSE
                WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'X(',IC,'), ',
     &            UNTS(1:NCH),': ',XVAR
              ENDIF
            ENDIF
            IF( IC.GT.1 ) THEN
              NW = N-1
              XE(2,NW) = XE(1,N)
            ENDIF
  110     CONTINUE
        ENDIF
  120 CONTINUE
  130 CONTINUE
      DO 140 K = 1,KFLD
      DO 140 J = 1,JFLD
      DO 140 I = 1,IFLD
        N = ND(I,J,K)
        NI = ND(I,1,1)
        XE(1,N) = XE(1,NI)
        XE(3,N) = XE(1,NI)
        XE(5,N) = XE(1,NI)
        XE(7,N) = XE(1,NI)
        XE(2,N) = XE(2,NI)
        XE(4,N) = XE(2,NI)
        XE(6,N) = XE(2,NI)
        XE(8,N) = XE(2,NI)
 140  CONTINUE
      DO 150 K = 1,KFLD
      DO 150 J = 1,JFLD
      DO 150 I = 1,IFLD
        N = ND(I,J,K)
        NW = N-1
        NPX = NSX(N)
        NQX = NSX(N)+1
        IF( INBS(4,N).NE.0 ) NQX = INBS(4,N)
        XP(N) = 5.D-1*(XE(1,N)+XE(2,N))
        DXGF(N) = XE(2,N)-XE(1,N)
        RP(I) = 1.D+0
        IF( ICS.EQ.2 ) RP(I) = XP(N)
        IF( I.EQ.1 .OR. INBS(3,N).NE.0 ) THEN
          DXGP(NPX) = XP(N)-XE(1,N)
          GRVX(NPX) = GRAVX
        ENDIF
        IF( I.EQ.IFLD .OR. INBS(4,N).NE.0 ) THEN
          DXGP(NQX) = XE(2,N)-XP(N)
          GRVX(NQX) = GRAVX
        ENDIF
        IF( I.GT.1 .AND. IFLD.GT.1 .AND. INBS(3,N).EQ.0 ) THEN
          DXGP(NPX) = XP(N)-XP(NW)
          GRVX(NPX) = GRAVX
        ENDIF
  150 CONTINUE
!
!---  Second coordinate direction  ---
!
      IF( ICS.EQ.1 ) THEN
        WRITE(IWR,'(/,A)')'Y-Direction Coordinates'
      ELSEIF( ICS.EQ.2 ) THEN
        WRITE(IWR,'(/,A)')'Azimuthal-Direction Coordinates'
      ENDIF
      JC = 0
      JW = 0
  200 CONTINUE
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      JR = JFLD+1-JC
      DO 220 J = 1,JR
        JCM = INDEX( CHDUM(ISTART:), ',' ) + ISTART - 1
        IF( JCM.EQ.ISTART-1 ) GOTO 200
        JAT = INDEX( CHDUM(ISTART:), '@' ) + ISTART - 1
        IF( JAT.LT.ISTART .OR. JAT.GT.JCM ) THEN
          JC = JC + 1
          JW = JW + 1
          N = ND(1,MIN(JC,JFLD),1)
          VARB = 'Y Dimension'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,VARX)
          VARB = 'Y Dimension Units'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( JC.EQ.JFLD+1 ) THEN
            YE(3,N) = VARX
            IF( ICS.EQ.2 ) THEN
              WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Azim(',JC,'), ',
     &          UNTS(1:NCH),': ',YE(3,N)
            ELSE
             WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Y(',JC,'), ',
     &         UNTS(1:NCH),': ',YE(3,N)
            ENDIF
            INDX = 0
            IUNM = 1
            IF( ICS.EQ.2 ) IUNM = 0
            CALL RDUNIT(UNTS,YE(3,N),INDX)
            GOTO 230
          ELSEIF( JW.EQ.5 ) THEN
            YE(1,N) = VARX
            IF( ICS.EQ.2 ) THEN
              WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Azim(',JC,'), ',
     &          UNTS(1:NCH),': ',YE(1,N)
            ELSE
             WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Y(',JC,'), ',
     &         UNTS(1:NCH),': ',YE(1,N)
            ENDIF
            JW = 0
          ELSE
            YE(1,N) = VARX
            IF( ICS.EQ.2 ) THEN
              WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Azim(',JC,'), ',
     &          UNTS(1:NCH),': ',YE(1,N)
            ELSE
             WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Y(',JC,'), ',
     &         UNTS(1:NCH),': ',YE(1,N)
            ENDIF
          ENDIF
          INDX = 0
          IUNM = 1
          IF( ICS.EQ.2 ) IUNM = 0
          CALL RDUNIT(UNTS,YE(1,N),INDX)
          IF( JC.GT.1 ) THEN
            NS = N-IFLD
            YE(3,NS) = YE(1,N)
          ENDIF
        ELSE
          CHDUM(JAT:JAT) = ','
          VARB = 'Count Integer'
          CALL RDINT(ISTART,ICOMMA,CHDUM,JAT )
          VARB = 'Y Dimension'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,DYVAR)
          VARB = 'Y Dimension Units'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNM = 1
          IF( ICS.EQ.2 ) IUNM = 0
          CALL RDUNIT(UNTS,DYVAR,INDX)
          DO 210 JJ = 1,JAT
            JC = JC + 1
            JW = JW + 1
            N = ND(1,MIN(JC,JFLD),1)
            IF( JC.EQ.1 ) THEN
              YE(1,N) = 0.D+0
              YVAR = YE(1,N)
            ELSEIF( JC.EQ.JFLD+1 ) THEN
              YE(3,N) = YE(1,N) + DYVAR
              YVAR = YE(3,N)
            ELSE
              NS = N-IFLD
              YE(1,N) = YE(1,NS) + DYVAR
              YVAR = YE(1,N)
            ENDIF
            INDX = 1
            IUNM = 1
            IF( ICS.EQ.2 ) IUNM = 0
            CALL RDUNIT(UNTS,YVAR,INDX )
            IF( JC.EQ.JFLD+1 ) THEN
              IF( ICS.EQ.2 ) THEN
                WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Azim(',JC,'), ',
     &            UNTS(1:NCH),': ',YVAR
              ELSE
                WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Y(',JC,'), ',
     &            UNTS(1:NCH),': ',YVAR
              ENDIF
              GOTO 230
            ELSEIF( JW.EQ.5 ) THEN
              IF( ICS.EQ.2 ) THEN
                WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Azim(',JC,'), ',
     &            UNTS(1:NCH),': ',YVAR
              ELSE
                WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Y(',JC,'), ',
     &            UNTS(1:NCH),': ',YVAR
              ENDIF
              JW = 0
            ELSE
              IF( ICS.EQ.2 ) THEN
                WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Azim(',JC,'), ',
     &          UNTS(1:NCH),': ',YVAR
              ELSE
                WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Y(',JC,'), ',
     &            UNTS(1:NCH),': ',YVAR
              ENDIF
            ENDIF
            IF( JC.GT.1 ) THEN
              NS = N-IFLD
              YE(3,NS) = YE(1,N)
            ENDIF
  210     CONTINUE
        ENDIF
  220 CONTINUE
  230 CONTINUE
      DO 240 K = 1,KFLD
      DO 240 J = 1,JFLD
      DO 240 I = 1,IFLD
        N = ND(I,J,K)
        NJ = ND(1,J,1)
        YE(1,N) = YE(1,NJ)
        YE(2,N) = YE(1,NJ)
        YE(5,N) = YE(1,NJ)
        YE(6,N) = YE(1,NJ)
        YE(3,N) = YE(3,NJ)
        YE(4,N) = YE(3,NJ)
        YE(7,N) = YE(3,NJ)
        YE(8,N) = YE(3,NJ)
 240  CONTINUE
      DO 250 K = 1,KFLD
      DO 250 J = 1,JFLD
      DO 250 I = 1,IFLD
        N = ND(I,J,K)
        NS = N-IFLD
        NPY = NSY(N)
        NQY = NSY(N)+IFLD
        IF( INBS(5,N).NE.0 ) NQY = INBS(5,N)
        YP(N) = 5.D-1*(YE(1,N)+YE(3,N))
        DYGF(N) = YE(3,N)-YE(1,N)
        IF( J.EQ.1 .OR. INBS(2,N).NE.0 ) THEN
          DYGP(NPY) = YP(N)-YE(1,N)
          GRVY(NPY) = GRAVY
        ENDIF
        IF( J.EQ.JFLD .OR. INBS(5,N).NE.0 ) THEN
          DYGP(NQY) = YE(3,N)-YP(N)
          GRVY(NQY) = GRAVY
        ENDIF
        IF( J.GT.1 .AND. JFLD.GT.1 .AND. INBS(2,N).EQ.0 ) THEN
          DYGP(NPY) = YP(N)-YP(NS)
          GRVY(NPY) = GRAVY
        ENDIF
  250 CONTINUE
!
!---  Third coordinate direction  ---
!
      IF( ICS.EQ.1 ) THEN
        WRITE(IWR,'(/,A)')'Z-Direction Coordinates'
      ELSEIF( ICS.EQ.2 ) THEN
        WRITE(IWR,'(/,A)')'Vertical-Direction Coordinates'
      ENDIF
      KC = 0
      KW = 0
  300 CONTINUE
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      KR = KFLD+1-KC
      DO 320 K = 1,KR
        KCM = INDEX( CHDUM(ISTART:), ',' ) + ISTART - 1
        IF( KCM.EQ.ISTART-1 ) GOTO 300
        KAT = INDEX( CHDUM(ISTART:), '@' ) + ISTART - 1
        IF( KAT.LT.ISTART .OR. KAT.GT.KCM ) THEN
          KC = KC + 1
          KW = KW + 1
          N = ND(1,1,MIN(KC,KFLD))
          VARB = 'Z Dimension'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,VARX)
          VARB = 'Z Dimension Units'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          IF( KC.EQ.KFLD+1 ) THEN
            ZE(5,N) = VARX
            IF( ICS.EQ.2 ) THEN
              WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Vert(',KC,'), ',
     &          UNTS(1:NCH),': ',ZE(5,N)
            ELSE
              WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Z(',KC,'), ',
     &          UNTS(1:NCH),': ',ZE(5,N)
            ENDIF
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,ZE(5,N),INDX)
            GOTO 330
          ELSEIF( KW.EQ.5 ) THEN
            ZE(1,N) = VARX
            IF( ICS.EQ.2 ) THEN
              WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Vert(',KC,'), ',
     &          UNTS(1:NCH),': ',ZE(1,N)
            ELSE
              WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Z(',KC,'), ',
     &          UNTS(1:NCH),': ',ZE(1,N)
            ENDIF
            KW = 0
          ELSE
            ZE(1,N) = VARX
            IF( ICS.EQ.2 ) THEN
              WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Vert(',KC,'), ',
     &          UNTS(1:NCH),': ',ZE(1,N)
            ELSE
              WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Z(',KC,'), ',
     &          UNTS(1:NCH),': ',ZE(1,N)
            ENDIF
          ENDIF
          INDX = 0
          IUNM = 1
          CALL RDUNIT(UNTS,ZE(1,N),INDX)
          IF( KC.GT.1 ) THEN
            NB = N-IJFLD
            ZE(5,NB) = ZE(1,N)
          ENDIF
        ELSE
          CHDUM(KAT:KAT) = ','
          VARB = 'Count Integer'
          CALL RDINT(ISTART,ICOMMA,CHDUM,KAT )
          VARB = 'Z Dimension'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,DZVAR)
          VARB = 'Z Dimension Units'
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          INDX = 0
          IUNM = 1
          CALL RDUNIT(UNTS,DZVAR,INDX)
          DO 310 KK = 1,KAT
            KC = KC + 1
            KW = KW + 1
            N = ND(1,1,MIN(KC,KFLD))
            IF( KC.EQ.1 ) THEN
              ZE(1,N) = 0.D+0
              ZVAR = ZE(1,N)
            ELSEIF( KC.EQ.KFLD+1 ) THEN
              ZE(5,N) = ZE(1,N) + DZVAR
              ZVAR = ZE(5,N)
            ELSE
              NB = N-IJFLD
              ZE(1,N) = ZE(1,NB) + DZVAR
              ZVAR = ZE(1,N)
            ENDIF
            INDX = 1
            IUNM = 1
            CALL RDUNIT(UNTS,ZVAR,INDX )
            IF( KC.EQ.KFLD+1 ) THEN
              IF( ICS.EQ.2 ) THEN
                WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Vert(',KC,'), ',
     &            UNTS(1:NCH),': ',ZVAR
              ELSE
                WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Z(',KC,'), ',
     &            UNTS(1:NCH),': ',ZVAR
              ENDIF
              GOTO 330
            ELSEIF( KW.EQ.5 ) THEN
              IF( ICS.EQ.2 ) THEN
                WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Vert(',KC,'), ',
     &            UNTS(1:NCH),': ',ZVAR
              ELSE
                WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Z(',KC,'), ',
     &            UNTS(1:NCH),': ',ZVAR
              ENDIF
              KW = 0
            ELSE
              IF( ICS.EQ.2 ) THEN
                WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Vert(',KC,'), ',
     &            UNTS(1:NCH),': ',ZVAR
              ELSE
                WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Z(',KC,'), ',
     &            UNTS(1:NCH),': ',ZVAR
              ENDIF
            ENDIF
            IF( KC.GT.1 ) THEN
              NB = N-IJFLD
              ZE(5,NB) = ZE(1,N)
            ENDIF
  310     CONTINUE
        ENDIF
  320 CONTINUE
  330 CONTINUE
      DO 340 K = 1,KFLD
      DO 340 J = 1,JFLD
      DO 340 I = 1,IFLD
        N = ND(I,J,K)
        NK = ND(1,1,K)
        ZE(1,N) = ZE(1,NK)
        ZE(2,N) = ZE(1,NK)
        ZE(3,N) = ZE(1,NK)
        ZE(4,N) = ZE(1,NK)
        ZE(5,N) = ZE(5,NK)
        ZE(6,N) = ZE(5,NK)
        ZE(7,N) = ZE(5,NK)
        ZE(8,N) = ZE(5,NK)
 340  CONTINUE
        DO 350 K = 1,KFLD
        DO 350 J = 1,JFLD
        DO 350 I = 1,IFLD
          N = ND(I,J,K)
          NB = N-IJFLD
          NPZ = NSZ(N)
          NQZ = NSZ(N)+IJFLD
          IF( INBS(6,N).NE.0 ) NQZ = INBS(6,N)
          ZP(N) = 5.D-1*(ZE(1,N)+ZE(5,N))
          DZGF(N) = ZE(5,N)-ZE(1,N)
          IF( K.EQ.1 .OR. INBS(1,N).NE.0 ) THEN
            DZGP(NPZ) = ZP(N)-ZE(1,N)
            GRVZ(NPZ) = GRAVZ
          ENDIF
          IF( K.EQ.KFLD .OR. INBS(6,N).NE.0 ) THEN
            DZGP(NQZ) = ZE(5,N)-ZP(N)
            GRVZ(NQZ) = GRAVZ
          ENDIF
          IF( K.GT.1 .AND. KFLD.GT.1 .AND. INBS(1,N).EQ.0 ) THEN
            DZGP(NPZ) = ZP(N)-ZP(NB)
            GRVZ(NPZ) = GRAVZ
          ENDIF
  350   CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDVBLGRID group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDXYZVERT
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
!     Read grid as eight X vertices per node, eight Y vertices per node,
!     and eight Z verticies per node.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 3 September 2014.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GRID
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
      CHARACTER*64 FDUM,FMDUM,UNTS
      CHARACTER*512 CHDUM
      LOGICAL FCHK
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDXYZVERT'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Read vertices file name  ---
!
      ISTART = 1
      CALL RDINPL( CHDUM )
      CALL LCASE( CHDUM )
      VARB = 'XYZ Vertices File Name'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
!
!---  Check for external file  ---
!
      INQUIRE( FILE=FDUM(1:NCH), FORM=FMDUM, EXIST=FCHK )
      IF( .NOT.FCHK ) THEN
        INDX = 4
        CHMSG = 'Missing XYZ Vertices File: ' // FDUM(1:NCH)
        CALL WRMSGS( INDX )
      ELSEIF( FDUM.EQ.'unformatted' ) THEN
        INDX = 4
        CHMSG = 'XYZ Vertices File Format: ' // FDUM(1:NCH)
        CALL WRMSGS( INDX )
      ENDIF
      OPEN( UNIT=9,FILE=FDUM(1:NCH),STATUS='OLD',FORM='FORMATTED' )
      REWIND( UNIT=9 )
      WRITE(IWR,'(/,3A)') VARB(1:IVR),': ',FDUM(1:NCH)
      READ(9,*) IFLDX,JFLDX,KFLDX
      IF( IFLDX.NE.IFLD .OR. JFLDX.NE.JFLD .OR. KFLDX.NE.KFLD ) THEN
        INDX = 4
        CHMSG = 'Grid Dimension Conflict with XYZ Vertices File'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Read length units  ---
!
      VARB = 'Grid File Length Units'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      INDX = 0
      VARX = 1.D+0
      IUNM = 1
      CALL RDUNIT(UNTS,VARX,INDX)
!
!---  Read x-vertices from file  ---
!
      READ(9,*)((XE(M,N),M=1,8),N=1,NFLD)
!
!---  Read y-vertices from file  ---
!
      READ(9,*)((YE(M,N),M=1,8),N=1,NFLD)
!
!---  Read z-vertices from file  ---
!
      READ(9,*)((ZE(M,N),M=1,8),N=1,NFLD)
!
!---  Convert vertice units  ---
!
      DO 110 N = 1,NFLD
        DO 100 M = 1,8
          XE(M,N) = XE(M,N)*VARX
          YE(M,N) = YE(M,N)*VARX
          ZE(M,N) = ZE(M,N)*VARX
  100   CONTINUE
  110 CONTINUE
!
!---  Closes Vertices File  ---
!
      CLOSE(UNIT=9)
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDHEXGRID group  ---
!
      RETURN
      END

!------------------------Function--------------------------------------!
!
      FUNCTION RM4DET ( AX )
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
!     RM4DET computes the determinant of a 4 by 4 matrix.
!
!     Author:
!
!     John Burkardt
!
!     Parameters:
!
!     Input, real AX(4,4), the matrix whose determinant is desired.
!
!     Output, real RM4DET, the determinant of the matrix.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 March 2006.
!     Last Modified by MD White, PNNL, 24 March 2006.
!     $Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
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
      REAL*8 RM4DET
      REAL*8 AX(4,4)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RM4DET'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
      RM4DET =
     &    AX(1,1) * (
     &    AX(2,2) * ( AX(3,3) * AX(4,4) - AX(3,4) * AX(4,3) )
     &  - AX(2,3) * ( AX(3,2) * AX(4,4) - AX(3,4) * AX(4,2) )
     &  + AX(2,4) * ( AX(3,2) * AX(4,3) - AX(3,3) * AX(4,2) ) )
     &  - AX(1,2) * (
     &    AX(2,1) * ( AX(3,3) * AX(4,4) - AX(3,4) * AX(4,3) )
     &  - AX(2,3) * ( AX(3,1) * AX(4,4) - AX(3,4) * AX(4,1) )
     &  + AX(2,4) * ( AX(3,1) * AX(4,3) - AX(3,3) * AX(4,1) ) )
     &  + AX(1,3) * (
     &    AX(2,1) * ( AX(3,2) * AX(4,4) - AX(3,4) * AX(4,2) )
     &  - AX(2,2) * ( AX(3,1) * AX(4,4) - AX(3,4) * AX(4,1) )
     &  + AX(2,4) * ( AX(3,1) * AX(4,2) - AX(3,2) * AX(4,1) ) )
     &  - AX(1,4) * (
     &    AX(2,1) * ( AX(3,2) * AX(4,3) - AX(3,3) * AX(4,2) )
     &  - AX(2,2) * ( AX(3,1) * AX(4,3) - AX(3,3) * AX(4,1) )
     &  + AX(2,3) * ( AX(3,1) * AX(4,2) - AX(3,2) * AX(4,1) ) )
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RM4DET group  ---
!
      RETURN
      END
      
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SPLIT_GRID
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
!     Define indices for internal boundary surfaces.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 1 March 2012.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GRID
      USE FILES
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
      CHARACTER*25 FORM5
!
!----------------------Data Statements---------------------------------!
!
      DATA FORM5 /'(A,I3,A,I3,A,I3,A,I8,A)'/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/SPLIT_GRID'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Record split grid to output file ---
!
      WRITE(IWR,'(//,A,/)') ' --- Split Grid Record  ---'
!
!---  Search grid for internal boundary surfaces  ---
!
      TOLX = 1.D-9
      NCX = (IFLD+1)*JFLD*KFLD
      NCY = IFLD*(JFLD+1)*KFLD
      NCZ = IFLD*JFLD*(KFLD+1)
      DO 100 N = 1,NFLD
        I = ID(N)
        J = JD(N)
        K = KD(N)
        NPX = NSX(N)
        NPY = NSY(N)
        NPZ = NSZ(N)
!
!---    Bottom surface  ---
!
        IF( K.NE.1 ) THEN
          NB = N-IJFLD
          IF( ABS(XE(1,N)-XE(5,NB)).GT.TOLX .OR. 
     &        ABS(YE(1,N)-YE(5,NB)).GT.TOLX .OR.
     &        ABS(ZE(1,N)-ZE(5,NB)).GT.TOLX .OR.
     &        ABS(XE(2,N)-XE(6,NB)).GT.TOLX .OR. 
     &        ABS(YE(2,N)-YE(6,NB)).GT.TOLX .OR.
     &        ABS(ZE(2,N)-ZE(6,NB)).GT.TOLX .OR.
     &        ABS(XE(3,N)-XE(7,NB)).GT.TOLX .OR. 
     &        ABS(YE(3,N)-YE(7,NB)).GT.TOLX .OR.
     &        ABS(ZE(3,N)-ZE(7,NB)).GT.TOLX .OR.
     &        ABS(XE(4,N)-XE(8,NB)).GT.TOLX .OR. 
     &        ABS(YE(4,N)-YE(8,NB)).GT.TOLX .OR.
     &        ABS(ZE(4,N)-ZE(8,NB)).GT.TOLX ) THEN
            INBS(1,N) = NPZ
            WRITE(FORM5(5:5),'(I1)') ICOUNT(I)
            WRITE(FORM5(10:10),'(I1)') ICOUNT(J)
            WRITE(FORM5(15:15),'(I1)') ICOUNT(K)
            WRITE(FORM5(20:20),'(I1)') ICOUNT(N)
            WRITE(IWR,FORM5) ' (',I,',',J,',',K,':',N,') Bottom'
          ENDIF
        ENDIF
!
!---    South surface  ---
!
        IF( J.NE.1 ) THEN
          NS = N-IFLD
          IF( ABS(XE(1,N)-XE(3,NS)).GT.TOLX .OR. 
     &        ABS(YE(1,N)-YE(3,NS)).GT.TOLX .OR.
     &        ABS(ZE(1,N)-ZE(3,NS)).GT.TOLX .OR.
     &        ABS(XE(2,N)-XE(4,NS)).GT.TOLX .OR. 
     &        ABS(YE(2,N)-YE(4,NS)).GT.TOLX .OR.
     &        ABS(ZE(2,N)-ZE(4,NS)).GT.TOLX .OR.
     &        ABS(XE(5,N)-XE(7,NS)).GT.TOLX .OR. 
     &        ABS(YE(5,N)-YE(7,NS)).GT.TOLX .OR.
     &        ABS(ZE(5,N)-ZE(7,NS)).GT.TOLX .OR.
     &        ABS(XE(6,N)-XE(8,NS)).GT.TOLX .OR. 
     &        ABS(YE(6,N)-YE(8,NS)).GT.TOLX .OR.
     &        ABS(ZE(6,N)-ZE(8,NS)).GT.TOLX ) THEN
            INBS(2,N) = NPY
            WRITE(FORM5(5:5),'(I1)') ICOUNT(I)
            WRITE(FORM5(10:10),'(I1)') ICOUNT(J)
            WRITE(FORM5(15:15),'(I1)') ICOUNT(K)
            WRITE(FORM5(20:20),'(I1)') ICOUNT(N)
            WRITE(IWR,FORM5) ' (',I,',',J,',',K,':',N,') South'
          ENDIF
        ENDIF
!
!---    West surface  ---
!
        IF( I.NE.1 ) THEN
          NW = N-1
          IF( ABS(XE(1,N)-XE(2,NW)).GT.TOLX .OR. 
     &        ABS(YE(1,N)-YE(2,NW)).GT.TOLX .OR.
     &        ABS(ZE(1,N)-ZE(2,NW)).GT.TOLX .OR.
     &        ABS(XE(3,N)-XE(4,NW)).GT.TOLX .OR. 
     &        ABS(YE(3,N)-YE(4,NW)).GT.TOLX .OR.
     &        ABS(ZE(3,N)-ZE(4,NW)).GT.TOLX .OR.
     &        ABS(XE(5,N)-XE(6,NW)).GT.TOLX .OR. 
     &        ABS(YE(5,N)-YE(6,NW)).GT.TOLX .OR.
     &        ABS(ZE(5,N)-ZE(6,NW)).GT.TOLX .OR.
     &        ABS(XE(7,N)-XE(8,NW)).GT.TOLX .OR. 
     &        ABS(YE(7,N)-YE(8,NW)).GT.TOLX .OR.
     &        ABS(ZE(7,N)-ZE(8,NW)).GT.TOLX ) THEN
            INBS(3,N) = NPX
            WRITE(FORM5(5:5),'(I1)') ICOUNT(I)
            WRITE(FORM5(10:10),'(I1)') ICOUNT(J)
            WRITE(FORM5(15:15),'(I1)') ICOUNT(K)
            WRITE(FORM5(20:20),'(I1)') ICOUNT(N)
            WRITE(IWR,FORM5) ' (',I,',',J,',',K,':',N,') West'
          ENDIF
        ENDIF
!
!---    East surface  ---
!
        IF( I.NE.IFLD ) THEN
          NE = N+1
          IF( ABS(XE(1,NE)-XE(2,N)).GT.TOLX .OR. 
     &        ABS(YE(1,NE)-YE(2,N)).GT.TOLX .OR.
     &        ABS(ZE(1,NE)-ZE(2,N)).GT.TOLX .OR.
     &        ABS(XE(3,NE)-XE(4,N)).GT.TOLX .OR. 
     &        ABS(YE(3,NE)-YE(4,N)).GT.TOLX .OR.
     &        ABS(ZE(3,NE)-ZE(4,N)).GT.TOLX .OR.
     &        ABS(XE(5,NE)-XE(6,N)).GT.TOLX .OR. 
     &        ABS(YE(5,NE)-YE(6,N)).GT.TOLX .OR.
     &        ABS(ZE(5,NE)-ZE(6,N)).GT.TOLX .OR.
     &        ABS(XE(7,NE)-XE(8,N)).GT.TOLX .OR. 
     &        ABS(YE(7,NE)-YE(8,N)).GT.TOLX .OR.
     &        ABS(ZE(7,NE)-ZE(8,N)).GT.TOLX ) THEN
            NCX = NCX + 1
            INBS(4,N) = NCX
            WRITE(FORM5(5:5),'(I1)') ICOUNT(I)
            WRITE(FORM5(10:10),'(I1)') ICOUNT(J)
            WRITE(FORM5(15:15),'(I1)') ICOUNT(K)
            WRITE(FORM5(20:20),'(I1)') ICOUNT(N)
            WRITE(IWR,FORM5) ' (',I,',',J,',',K,':',N,') East'
          ENDIF
        ENDIF
!
!---    North surface  ---
!
        IF( J.NE.JFLD ) THEN
          NN = N+IFLD
          IF( ABS(XE(1,NN)-XE(3,N)).GT.TOLX .OR. 
     &        ABS(YE(1,NN)-YE(3,N)).GT.TOLX .OR.
     &        ABS(ZE(1,NN)-ZE(3,N)).GT.TOLX .OR.
     &        ABS(XE(2,NN)-XE(4,N)).GT.TOLX .OR. 
     &        ABS(YE(2,NN)-YE(4,N)).GT.TOLX .OR.
     &        ABS(ZE(2,NN)-ZE(4,N)).GT.TOLX .OR.
     &        ABS(XE(5,NN)-XE(7,N)).GT.TOLX .OR. 
     &        ABS(YE(5,NN)-YE(7,N)).GT.TOLX .OR.
     &        ABS(ZE(5,NN)-ZE(7,N)).GT.TOLX .OR.
     &        ABS(XE(6,NN)-XE(8,N)).GT.TOLX .OR. 
     &        ABS(YE(6,NN)-YE(8,N)).GT.TOLX .OR.
     &        ABS(ZE(6,NN)-ZE(8,N)).GT.TOLX ) THEN
            NCY = NCY + 1
            INBS(5,N) = NCY
            WRITE(FORM5(5:5),'(I1)') ICOUNT(I)
            WRITE(FORM5(10:10),'(I1)') ICOUNT(J)
            WRITE(FORM5(15:15),'(I1)') ICOUNT(K)
            WRITE(FORM5(20:20),'(I1)') ICOUNT(N)
            WRITE(IWR,FORM5) ' (',I,',',J,',',K,':',N,') North'
          ENDIF
        ENDIF
!
!---    Top surface  ---
!
        IF( K.NE.KFLD ) THEN
          NT = N+IJFLD
          IF( ABS(XE(1,NT)-XE(5,N)).GT.TOLX .OR. 
     &        ABS(YE(1,NT)-YE(5,N)).GT.TOLX .OR.
     &        ABS(ZE(1,NT)-ZE(5,N)).GT.TOLX .OR.
     &        ABS(XE(2,NT)-XE(6,N)).GT.TOLX .OR. 
     &        ABS(YE(2,NT)-YE(6,N)).GT.TOLX .OR.
     &        ABS(ZE(2,NT)-ZE(6,N)).GT.TOLX .OR.
     &        ABS(XE(3,NT)-XE(7,N)).GT.TOLX .OR. 
     &        ABS(YE(3,NT)-YE(7,N)).GT.TOLX .OR.
     &        ABS(ZE(3,NT)-ZE(7,N)).GT.TOLX .OR.
     &        ABS(XE(4,NT)-XE(8,N)).GT.TOLX .OR. 
     &        ABS(YE(4,NT)-YE(8,N)).GT.TOLX .OR.
     &        ABS(ZE(4,NT)-ZE(8,N)).GT.TOLX ) THEN
            NCZ = NCZ + 1
            INBS(6,N) = NCZ
            WRITE(FORM5(5:5),'(I1)') ICOUNT(I)
            WRITE(FORM5(10:10),'(I1)') ICOUNT(J)
            WRITE(FORM5(15:15),'(I1)') ICOUNT(K)
            WRITE(FORM5(20:20),'(I1)') ICOUNT(N)
            WRITE(IWR,FORM5) ' (',I,',',J,',',K,':',N,') Top'
          ENDIF
        ENDIF
  100 CONTINUE
      ISUB_LOG = ISUB_LOG-1
!
!---  End of SPLIT_GRID group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE TETRVOL( X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,VOLUME )
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
!     TETRVOL computes the area of a triangle in 3D.
!
!     Author:
!
!     John Burkardt
!
!     Parameters:
!
!     Input, real X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, X4, Y4, Z4, the
!     coordinates of the corners of the tetrahedron.
!
!     Output, real VOLUME, the volume of the tetrahedron.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 March 2006.
!     Last Modified by MD White, PNNL, 24 March 2006.
!     $Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
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
      REAL*8 RM4DET
      REAL*8 X1,Y1,Z1
      REAL*8 X2,Y2,Z2
      REAL*8 X3,Y3,Z3
      REAL*8 X4,Y4,Z4
      REAL*8 AX(4,4)
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/TETRVOL'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
      AX(1,1) = X1
      AX(2,1) = X2
      AX(3,1) = X3
      AX(4,1) = X4
      AX(1,2) = Y1
      AX(2,2) = Y2
      AX(3,2) = Y3
      AX(4,2) = Y4
      AX(1,3) = Z1
      AX(2,3) = Z2
      AX(3,3) = Z3
      AX(4,3) = Z4
      AX(1,4) = 1.D+0
      AX(2,4) = 1.D+0
      AX(3,4) = 1.D+0
      AX(4,4) = 1.D+0
      VOLUME = ABS ( RM4DET( AX ) )/6.D+0
      ISUB_LOG = ISUB_LOG-1
!
!---  End of TETRVOL group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE TRGAREA( X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,AREA )
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
!     TRGAREA computes the area of a triangle in 3D.
!
!
!     Discussion:
!
!     This routine uses the fact that the norm of the cross product 
!     vector is the area of the parallelogram they form.  The triangle 
!     they form has half that area.
!
!     Reference:
!
!     Adrian Bowyer and John Woodwark,
!     A Programmers Geometry,
!     Butterworths, 1983.
!
!     Author:
!
!     John Burkardt
!
!     Parameters:
!
!     Input, real X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, 
!     the triangle vertices.
!
!     Output, real AREA, the area of the triangle.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 March 2006.
!     Last Modified by MD White, PNNL, 24 March 2006.
!     $Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
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
      REAL*8 AREA,ENORM_3D,NORM
      REAL*8 X1,Y1,Z1
      REAL*8 X2,Y2,Z2
      REAL*8 X3,Y3,Z3
      REAL*8 X4,Y4,Z4
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/TRGAREA'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
      CALL CROSS_3D ( X2-X1,Y2-Y1,Z2-Z1,X3-X1,Y3-Y1,Z3-Z1,X4,Y4,Z4 )
      NORM = ENORM_3D ( X4,Y4,Z4 )
      AREA = 5.D-1*NORM
      ISUB_LOG = ISUB_LOG-1
!
!---  End of TRGAREA group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE VCROSSP( AX,BX,CX )
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
!     Vector cross product.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 30 March 2011.
!

!----------------------Fortran 90 Modules------------------------------!
!
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 AX(3),BX(3),CX(3)
!
!----------------------Executable Lines--------------------------------!
!
      CX(1) = AX(2)*BX(3) - AX(3)*BX(2)
      CX(2) = AX(3)*BX(1) - AX(1)*BX(3)
      CX(3) = AX(1)*BX(2) - AX(2)*BX(1)
!
!---  End of VCROSSP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      FUNCTION VDOTP( AX,BX )
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
!     Vector dot product.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 30 March 2011.
!

!----------------------Fortran 90 Modules------------------------------!
!
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 AX(3),BX(3)
!
!----------------------Executable Lines--------------------------------!
!
      VDOTP = AX(1)*BX(1) + AX(2)*BX(2) + AX(3)*BX(3)
!
!---  End of VDOTP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WITHIN( XX,YX,ZX,ICWX,NX )
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
!     Determine whether an x,y,z coordinate location is within
!     the bounds of a node.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 28 March 2012.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE GRID
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
      REAL*8 XPX(5),YPX(5),ZPX(5)
      REAL*8 PAX(3),PBX(3),PCX(3),PBCX(3)
      INTEGER MSX(4,6)
      INTEGER N1X(4),N2X(4)
!
!----------------------Data Statements---------------------------------!
!
      DATA MSX / 1,2,4,3,1,5,6,2,1,3,7,5,2,6,8,4,3,4,8,7,5,7,8,6 /
      DATA N1X / 2,3,4,1 /
      DATA N2X / 1,2,3,4 /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WITHIN'
      IF( INDEX(SVN_ID(143)(1:1),'$').EQ.0 ) SVN_ID(143) =
     & '$Id: rdgrid.F 1080 2017-03-14 16:22:02Z d3c002 $'
!
!---  Loop over node surfaces,
!     (bottom,south,west,east,north,top)  ---
!
      ICWX = 0
      DO 180 NS = 1,6
!
!---    Define the five surface points, four corners
!       and one centroid---
!
        DO 110 NP = 1,4
          MX = MSX(NP,NS)
!
!---      Cylindrical coordinates  ---
!
          IF( ICS.EQ.2 .OR. ICS.EQ.6 ) THEN
            XPX(NP) = XE(MX,NX)*COS(YE(MX,NX))
            YPX(NP) = XE(MX,NX)*SIN(YE(MX,NX))
            ZPX(NP) = ZE(MX,NX)
          ELSE
            XPX(NP) = XE(MX,NX)
            YPX(NP) = YE(MX,NX)
            ZPX(NP) = ZE(MX,NX)
          ENDIF
  110   CONTINUE
        NP = 4
        CALL PGCNTRD( NP,XPX(1),YPX(1),ZPX(1),
     &    XPX(5),YPX(5),ZPX(5) )
!
!---    Loop over the four triangular planes on the 
!       surface face  ---
!
        DO 130 NT = 1,4
!
!---      Built vectors between transition point
!         and triangular plane points  ---
!
          PAX(1) = XPX(5)-XX
          PAX(2) = YPX(5)-YX
          PAX(3) = ZPX(5)-ZX
          PBX(1) = XPX(N1X(NT))-XX
          PBX(2) = YPX(N1X(NT))-YX
          PBX(3) = ZPX(N1X(NT))-ZX
          PCX(1) = XPX(N2X(NT))-XX
          PCX(2) = YPX(N2X(NT))-YX
          PCX(3) = ZPX(N2X(NT))-ZX
          CALL VCROSSP( PBX,PCX,PBCX )
          SX = VDOTP( PAX,PBCX )
!
!---      Clockwise rotation  ---
!
          IF( SX.GT.EPSL ) THEN
!
!---        Opposing rotations found, point outside hexahedron  ---
!
            IF( ICWX.EQ.-1 ) THEN
              ICWX = 0
              GOTO 200
!
!---        Similar rotations found, continue searching  ---
!
            ELSE
              ICWX = 1
            ENDIF
!
!---      Counterclockwise rotation  ---
!
          ELSEIF( SX.LT.-EPSL ) THEN
!
!---        Opposing rotations found, point outside hexahedron  ---
!
            IF( ICWX.EQ.1 ) THEN
              ICWX = 0
              GOTO 200
!
!---        Similar rotations found, continue searching  ---
!
            ELSE
              ICWX = -1
            ENDIF
          ENDIF
  130   CONTINUE
  180 CONTINUE
      ICWX = ABS( ICWX )
  200 CONTINUE            
!
!---  Reset subroutine character string ---
!
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WITHIN group  ---
!
      RETURN
      END


