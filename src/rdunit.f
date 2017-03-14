!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDUNIT( UNTS,VAR,INDX )
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
!     Unit conversions for variable VAR.
!     INDX = 0 : Convert to SI Units.
!     INDX = 1 : Convert from SI Units.
!     INDX = 2 : Convert to SI Units without a units check.
!     INDX = 3 : Convert to SI Units for output.
!     INDX = 4 : Convert from SI Units for output.
!
!     Rules for expressing units:
!       Units may only contain one divisor.
!       Components within a units character string must be separated
!       with a blank space, colon, or a divisor.
!       No spaces between component name and divisor.
!       Units raised to powers are indicated with a '^' symbol.
!       Examples:
!         'btu:in/hr ft^2 F' converts to SI units of 'w/m k'
!         'lb/hr ft' converts to SI units of 'Pa s'
!         'g/l' converts to SI units of 'kg/m^3'
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, November 1992.
!     Last Modified by MD White, Battelle, PNL, December 31, 1992.
!     Last Modified by MD White, PNNL, 25 November 2003.
!     $Id: rdunit.F 1080 2017-03-14 16:22:02Z d3c002 $
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



      PARAMETER (LUNS=119)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*4 FORM1
      CHARACTER*8 CHS(LUNS),CHD
      CHARACTER*64 UNTS
      REAL*8 CF(LUNS)
      INTEGER IUM(LUNS),IUKG(LUNS),IUS(LUNS),IUK(LUNS),IUMOL(LUNS)
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Data Statements---------------------------------!
!
      SAVE CHS,CF,IUM,IUKG,IUS,IUK,IUMOL,FORM1
      DATA CHS /'m','kg','s','j','c','pa','w','kgmol','rad',
     &  'solid','water','napl','gas','aqueous','oil','voc','sol',
     &  'ci','pci','liq','aqu','mi','mile','nm',
     &  'ft','cm','mm','yd','in','l','gal','liter',
     &  'ml','g','lb','slug','lbm','gm','gram','mg',
     &  'min','hr','d','wk','yr',
     &  'hour','day','week','year','sec',
     &  'btu','cal','hp','dynes','dyn','darcy',
     &  'k','f','r',
     &  'psi','bar','atm','wh','psf','lbf',
     &  'deg','degree','furlong','rod','rad','radian',
     &  'cp','p','hc','1','mol','mole','lbmol',
     &  'debyes','bd','n','newton','plant','langley',
     &  'kpa','mpa','gpa','upa','kmol','kmole','joule','kj','kjoule',
     &  'a','ao','ang','angstrom',
     &  'tonne','mt','mmt','md','mdarcy','ton',
     &  'ffl','ffa','ffvh_m','ffvh_ft',
     &  'ffvh_in','ffvh_yd','ffvh_cm','st','cst',
     &  'bbl','stb','scf','mscf','mmscf','bscf','tscf'/
      DATA CF  /1.D+0,1.D+0,1.D+0,1.D+0,1.D+0,1.D+0,1.D+0,1.D+0,1.D+0,
     &  1.D+0,1.D+0,1.D+0,1.D+0,1.D+0,1.D+0,1.D+0,1.D+0,
     &  1.D+0,1.D+0,1.D+0,1.D+0,1.609344D+3,1.609344D+3,1.D-9,
     &  3.048D-1,1.D-2,1.D-3,9.144D-1,2.54D-2,1.D-3,3.7854D-3,1.D-3,
     &  1.D-6,1.D-3,4.5359D-1,1.4594D+1,4.5359D-1,1.D-3,1.D-3,1.D-6,
     &  6.D+1,3.6D+3,8.64D+4,6.048D+5,3.15576D+7,
     &  3.6D+3,8.64D+4,6.048D+5,3.15576D+7,1.D+0,
     &  1.0544D+3,4.184D+0,7.457D+2,1.D-5,1.D-5,0.9869D-12,
     &  1.D+0,5.555556D-1,5.555556D-1,
     &  6.8948D+3,1.D+5,1.01325D+5,9.7935332D+03,4.7880556D+1,4.4482D+0,
     &  1.745329252D-2,1.745329252D-2,2.01168D+2,5.0292D+0,1.D+0,1.D+0,
     &  1.D-3,1.D-1,1.03910295199455D-07,1.D+0,1.D-3,1.D-3,4.5359D-1,
     &  1.D+0,1.D+0,1.D+0,1.D+0,1.D+0,4.186D+4,
     &  1.D+3,1.D+6,1.D+9,1.D-6,1.D+0,1.D+0,1.D+0,1.D+3,1.D+3,
     &  1.D-10,1.D-10,1.D-10,1.D-10,
     &  1.D+3,1.D+3,1.D+9,0.9869D-15,0.9869D-15,9.0718494D+2,
     &  109.728D+0,5351.215104D+0,5351.215104D+0,1631.050363699D+0,
     &  135.920863642D+0,4893.151091098D+0,53.51215104D+0,1.D-4,1.D-6,
     &  0.158987295D+0,0.158987295D+0,2.83168D-2,2.83168D+1,2.83168D+4,
     &  2.83168D+7,2.83168D+10/
      DATA IUM /1,0,0,2,0,-1,2,0,0,
     &  0,0,0,0,0,0,0,0,
     &  0,0,0,0,1,1,1,
     &  1,1,1,1,1,3,3,3,
     &  3,0,0,0,0,0,0,0,
     &  0,0,0,0,0,
     &  0,0,0,0,0,
     &  2,2,2,1,1,2,
     &  0,0,0,
     &  -1,-1,-1,-2,-1,1,
     &  0,0,1,1,0,0,
     &  -1,-1,0,0,0,0,0,
     &  0,-3,1,1,0,0,
     &  -1,-1,-1,-1,0,0,2,2,2,
     &  1,1,1,1,
     &  0,0,0,2,2,0,
     &  1,2,3,3,
     &  3,3,3,2,2,
     &  3,3,3,3,3,3,3/
      DATA IUKG /0,1,0,1,0,1,1,0,0,
     &  0,0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,0,
     &  0,1,1,1,1,1,1,1,
     &  0,0,0,0,0,
     &  0,0,0,0,0,
     &  1,1,1,1,1,0,
     &  0,0,0,
     &  1,1,1,1,1,1,
     &  0,0,0,0,0,0,
     &  1,1,0,0,0,0,0,
     &  0,1,1,1,0,1,
     &  1,1,1,1,0,0,1,1,1,
     &  0,0,0,0,
     &  1,1,1,0,0,1,
     &  0,0,0,0,
     &  0,0,0,0,0,
     &  0,0,0,0,0,0,0/
      DATA IUS /0,0,1,-2,0,-2,-3,0,0,
     &  0,0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,0,
     &  1,1,1,1,1,
     &  1,1,1,1,1,
     &  -2,-2,-3,-2,-2,0,
     &  0,0,0,
     &  -2,-2,-2,-2,-2,-2,
     &  0,0,0,0,0,0,
     &  -1,-1,0,0,0,0,0,
     &  0,0,-2,-2,0,-2,
     &  -2,-2,-2,-2,0,0,-2,-2,-2,
     &  0,0,0,0,
     &  0,0,0,0,0,0,
     &  0,0,0,0,
     &  0,0,0,-1,-1,
     &  0,0,0,0,0,0,0/
      DATA IUK /0,0,0,0,1,0,0,0,0,
     &  0,0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,0,
     &  0,0,0,0,0,
     &  0,0,0,0,0,
     &  0,0,0,0,0,0,
     &  1,1,1,
     &  0,0,0,0,0,0,
     &  0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,0,0,
     &  0,0,0,0,
     &  0,0,0,0,0,0,
     &  0,0,0,0,
     &  0,0,0,0,0,
     &  0,0,0,0,0,0,0/
      DATA IUMOL /0,0,0,0,0,0,0,1,0,
     &  0,0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,0,
     &  0,0,0,0,0,0,0,0,
     &  0,0,0,0,0,
     &  0,0,0,0,0,
     &  0,0,0,0,0,0,
     &  0,0,0,
     &  0,0,0,0,0,0,
     &  0,0,0,0,0,0,
     &  0,0,0,0,1,1,1,
     &  0,0,0,0,0,0,
     &  0,0,0,0,1,1,0,0,0,
     &  0,0,0,0,
     &  0,0,0,0,0,0,
     &  0,0,0,0,
     &  0,0,0,0,0,
     &  0,0,0,0,0,0,0/     
      DATA FORM1 /'(I )'/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDUNIT'
      IF( INDEX(SVN_ID(167)(1:1),'$').EQ.0 ) SVN_ID(167) =
     & '$Id: rdunit.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      NCH = INDEX( UNTS,'  ' ) - 1
      IF( UNTS(1:NCH).EQ.'null' .OR. UNTS(1:NCH).EQ.'none' ) THEN
        IUNM = 0
        IUNKG = 0
        IUNS = 0
        IUNK = 0
        IUNMOL = 0
        ISUB_LOG = ISUB_LOG-1
        RETURN
      ENDIF
!
!---  Intialize primary unit indices  ---
!
      IUMX = 0
      IUKGX = 0
      IUSX = 0
      IUKX = 0
      IUMOLX = 0
!
!---  Temperature units  --
!
      IF( UNTS(1:NCH) .EQ. 'c' ) THEN
        IUKX = 1
        GOTO 400
      ELSEIF( UNTS(1:NCH) .EQ. 'k' ) THEN
        IF( INDX.EQ.0 .OR. INDX.EQ.2 .OR. INDX.EQ.3 ) THEN
          VAR = VAR - 2.7315D+2
        ELSE
          VAR = VAR + 2.7315D+2
        ENDIF
        IUKX = 1
        GOTO 400
      ELSEIF( UNTS(1:NCH) .EQ. 'f' ) THEN
        IF( INDX.EQ.0 .OR. INDX.EQ.2 .OR. INDX.EQ.3 ) THEN
          VAR = (VAR-3.2D+1)/1.8D+0
        ELSE
          VAR = VAR*1.8D+0 + 3.2D+1
        ENDIF
        IUKX = 1
        GOTO 400
      ELSEIF( UNTS(1:NCH) .EQ. 'r' ) THEN
        IF( INDX.EQ.0 .OR. INDX.EQ.2 .OR. INDX.EQ.3 ) THEN
          VAR = (VAR-4.92D+2)/1.8D+0
        ELSE
          VAR = VAR*1.8D+0 + 4.92D+2
        ENDIF
        IUKX = 1
        GOTO 400
      ENDIF
!
!---  Decompose the units into components and convert individual
!     components  ---
!
      ISX = 1
      IDV = INDEX( UNTS(1:),'/' )-1
      IEX = INDEX( UNTS(1:),'  ' )-1
!
!---  Units without a divisor  ---
!
      IF( IDV .EQ. -1 ) THEN
  100 CONTINUE
        ISP = INDEX( UNTS(ISX:),' ' )+ISX-2
        ICO = INDEX( UNTS(ISX:),':' )+ISX-2
        IF( ICO .LT. ISX ) ICO = IEX
        IB = MIN( IEX,ISP,ICO )
        CHD = UNTS(ISX:IB)
        IC = INDEX( CHD(1:),'^' )
        IF( IC .EQ. 0 ) THEN
          IP = 1
        ELSE
          I1 = IC+1
          I2 = IB-ISX+1
          I3 = I2-I1+1
          WRITE( FORM1(3:3),'(I1)' ) I3
          READ(CHD(I1:I2),FORM1) IP
          I2 = IC-1
          CHD = CHD(1:I2)
        ENDIF
        DO 110 N = 1,LUNS
          IF( CHS(N) .EQ. CHD ) THEN
            IUMX = IUMX + IUM(N)*IP
            IUKGX = IUKGX + IUKG(N)*IP
            IUSX = IUSX + IUS(N)*IP
            IUKX = IUKX + IUK(N)*IP
            IUMOLX = IUMOLX + IUMOL(N)*IP
            IF( INDX.EQ.0 .OR. INDX.EQ.2 .OR. INDX.EQ.3 ) THEN
              VAR = VAR*(CF(N)**IP)
            ELSE
              VAR = VAR/(CF(N)**IP)
            ENDIF
            GOTO 120
          ENDIF
  110   CONTINUE
        IF( INDX.EQ.0 .OR. INDX.EQ.2 .OR. INDX.EQ.3 ) THEN
          INDX = 4
          IF( INDX.EQ.3 ) INDX = 21
          CHMSG = 'Unrecognized Units: '//VARB(1:IVR)//': '//UNTS
          CALL WRMSGS( INDX )
        ELSE
          INDX = 4
          IF( INDX.EQ.4 ) INDX = 21
          CHMSG = 'Unrecognized Units: Output Variable: '//UNTS
          CALL WRMSGS( INDX )
        ENDIF
  120   CONTINUE
        IF( IB .LT. IEX ) THEN
          ISX = IB+2
          GOTO 100
        ENDIF
!
!---  Units with a divisor  ---
!
      ELSE
!
!---  Components before the divisor  ---
!
  200 CONTINUE
        ISP = INDEX( UNTS(ISX:),' ' )+ISX-2
        ICO = INDEX( UNTS(ISX:),':' )+ISX-2
        IF( ICO .LT. ISX ) ICO = IEX
        IB = MIN( IDV,ISP,ICO )
        CHD = UNTS(ISX:IB)
        IC = INDEX( CHD(1:),'^' )
        IF( IC .EQ. 0 ) THEN
          IP = 1
        ELSE
          I1 = IC+1
          I2 = IB-ISX+1
          I3 = I2-I1+1
          WRITE( FORM1(3:3),'(I1)' ) I3
          READ(CHD(I1:I2),FORM1) IP
          I2 = IC-1
          CHD = CHD(1:I2)
        ENDIF
        DO 210 N = 1,LUNS
          IF( CHS(N) .EQ. CHD ) THEN
            IUMX = IUMX + IUM(N)*IP
            IUKGX = IUKGX + IUKG(N)*IP
            IUSX = IUSX + IUS(N)*IP
            IUKX = IUKX + IUK(N)*IP
            IUMOLX = IUMOLX + IUMOL(N)*IP
            IF( INDX.EQ.0 .OR. INDX.EQ.2 .OR. INDX.EQ.3 ) THEN
              VAR = VAR*(CF(N)**IP)
            ELSE
              VAR = VAR/(CF(N)**IP)
            ENDIF
            GOTO 220
          ENDIF
  210   CONTINUE
        IF( INDX.EQ.0 .OR. INDX.EQ.2 .OR. INDX.EQ.3 ) THEN
          INDX = 4
          IF( INDX.EQ.3 ) INDX = 21
          CHMSG = 'Unrecognized Units: '//VARB(1:IVR)//': '//UNTS
          CALL WRMSGS( INDX )
        ELSE
          INDX = 4
          IF( INDX.EQ.4 ) INDX = 21
          CHMSG = 'Unrecognized Units: Output Variable: '//UNTS
          CALL WRMSGS( INDX )
        ENDIF
  220   CONTINUE
        IF( IB .LT. IDV ) THEN
          ISX = IB+2
          GOTO 200
        ELSE
          ISX = IB+2
          GOTO 300
        ENDIF
!
!---  Components after the divisor  ---
!
  300   CONTINUE
        ISP = INDEX( UNTS(ISX:),' ' )+ISX-2
        ICO = INDEX( UNTS(ISX:),':' )+ISX-2
        IF( ICO .LT. ISX ) ICO = IEX
        IB = MIN( IEX,ISP,ICO )
        CHD = UNTS(ISX:IB)
        IC = INDEX( CHD(1:),'^' )
        IF( IC .EQ. 0 ) THEN
          IP = 1
        ELSE
          I1 = IC+1
          I2 = IB-ISX+1
          I3 = I2-I1+1
          WRITE( FORM1(3:3),'(I1)' ) I3
          READ(CHD(I1:I2),FORM1) IP
          I2 = IC-1
          CHD = CHD(1:I2)
        ENDIF
        DO 310 N = 1,LUNS
          IF( CHS(N) .EQ. CHD ) THEN
            IUMX = IUMX - IUM(N)*IP
            IUKGX = IUKGX - IUKG(N)*IP
            IUSX = IUSX - IUS(N)*IP
            IUKX = IUKX - IUK(N)*IP
            IUMOLX = IUMOLX - IUMOL(N)*IP
            IF( INDX.EQ.0 .OR. INDX.EQ.2 .OR. INDX.EQ.3 ) THEN
              VAR = VAR/(CF(N)**IP)
            ELSE
              VAR = VAR*(CF(N)**IP)
            ENDIF
            GOTO 320
          ENDIF
  310   CONTINUE
        IF( INDX.EQ.0 .OR. INDX.EQ.2 .OR. INDX.EQ.3 ) THEN
          INDX = 4
          IF( INDX.EQ.3 ) INDX = 21
          CHMSG = 'Unrecognized Units: '//VARB(1:IVR)//': '//UNTS
          CALL WRMSGS( INDX )
        ELSE
          INDX = 4
          IF( INDX.EQ.4 ) INDX = 21
          CHMSG = 'Unrecognized Units: Output Variable: '//UNTS
          CALL WRMSGS( INDX )
        ENDIF
  320   CONTINUE
        IF( IB .LT. IEX ) THEN
          ISX = IB+2
          GOTO 300
        ENDIF
      ENDIF
!
!---  Units Conversion Check  ---
!
  400 CONTINUE
      IF( INDX.EQ.2 ) THEN
        IUNM = 0
        IUNKG = 0
        IUNS = 0
        IUNK = 0
        IUNMOL = 0
      ELSEIF( IUMX.NE.IUNM .OR. IUKGX.NE.IUNKG .OR. IUSX.NE.IUNS .OR.
     &    IUKX.NE.IUNK .OR. IUMOLX.NE.IUNMOL ) THEN
        IF( INDX.EQ.0 .OR. INDX.EQ.3 ) THEN
          INDX = 4
          IF( INDX.EQ.3 ) INDX = 21
          CHMSG = 'Incompatible Units: '//VARB(1:IVR)//', '//UNTS
          CALL WRMSGS( INDX )
        ELSE
          INDX = 4
          IF( INDX.EQ.3 ) INDX = 21
          CHMSG = 'Incompatible Units: Output Variable, '//UNTS
          CALL WRMSGS( INDX )
        ENDIF
      ELSE
        IUNM = 0
        IUNKG = 0
        IUNS = 0
        IUNK = 0
        IUNMOL = 0
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDUNIT group  ---
!
      RETURN
      END

