!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRRST
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
!     Write restart files
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, February, 1993.
!     Last Modified by MD White, Battelle, PNL, October 15, 1997.




!     $Id: wrrst.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE WELL_FD
      USE WELL_CL
      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE REACT
      USE HYST
      USE GRID
      USE GEOMECH
      USE FILES
      USE FDVS
      USE FDVP
      USE FDVH
      USE FDVGC
      USE FDVD
      USE COUP_WELL
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
      CHARACTER*16 FN
      CHARACTER*4 FORM1
      CHARACTER*19 FORM2
      CHARACTER*37 FORM3
      CHARACTER*38 FORM4
      CHARACTER*20 FORM5
      CHARACTER*38 FORM6
      CHARACTER*39 FORM7
      CHARACTER*22 FORM8
      CHARACTER*40 FORM9
      CHARACTER*41 FORM10
      CHARACTER*16 FORM11
      CHARACTER*17 FORM12
      CHARACTER*39 FORM13
      CHARACTER*40 FORM14
      CHARACTER*42 FORM15




      REAL*8 SP_CX(LSPR)

!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM1,FORM2,FORM3,FORM4,FORM5,FORM6,FORM7
      SAVE FORM8,FORM9,FORM10,FORM11,FORM12,FORM13
      SAVE FORM14,FORM15
      DATA FORM1 /'(I6)'/
      DATA FORM2 /'(1(1PE22.15,1X),I3)'/
      DATA FORM3 /'(1(1PE22.15,1X),I3,1X,1(1PE22.15,1X))'/
      DATA FORM4 /'(1(1PE22.15,1X),I3,1X,10(1PE22.15,1X))'/
      DATA FORM5 /'(10(1PE22.15,1X),I3)'/
      DATA FORM6 /'(10(1PE22.15,1X),I3,1X,1(1PE22.15,1X))'/
      DATA FORM7 /'(10(1PE22.15,1X),I3,1X,10(1PE22.15,1X))'/
      DATA FORM8 /'(1(1PE22.15,1X),2(I3))'/
      DATA FORM9 /'(1(1PE22.15,1X),2(I3),1X,1(1PE22.15,1X))'/
      DATA FORM10 /'(1(1PE22.15,1X),2(I3),1X,10(1PE22.15,1X))'/
      DATA FORM11 /'(1(1PE22.15,1X))'/
      DATA FORM12 /'(10(1PE22.15,1X))'/
      DATA FORM13 /'(1(1PE22.15,1X),I3,1X,100(1PE22.15,1X))'/
      DATA FORM14 /'(10(1PE22.15,1X),I3,1X,100(1PE22.15,1X))'/
      DATA FORM15 /'(1(1PE22.15,1X),2(I3),1X,100(1PE22.15,1X))'/
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRRST'
      IF( INDEX(SVN_ID(278)(1:1),'$').EQ.0 ) SVN_ID(278) =
     & '$Id: wrrst.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Create a new restart file with number of time steps
!     as the file name extension  ---
!
      FN(1:8) = 'restart.'
      N1X = MIN( ICOUNT( MXSTEP+NRST ),9 )
      DO 10 N2X = 1,N1X
        N3X = N2X + 8
        FN(N3X:N3X) = '0'
   10 CONTINUE
      N4X = ICOUNT( NSTEP )
      WRITE(FORM1(3:3),'(I1)') N4X
      N5X = 9 + N1X - N4X
      WRITE( FN(N5X:),FORM1) NSTEP
      OPEN(UNIT=IRS, FILE=FN, STATUS='UNKNOWN', FORM='FORMATTED')
      CLOSE(UNIT=IRS, STATUS='DELETE')
      OPEN(UNIT=IRS, FILE=FN, STATUS='NEW', FORM='FORMATTED')

!
!---  Write header  ---
!
      WRITE(IRS,'(A,//)')' Welcome to ...'
      WRITE(IRS,'(A)')   '                           STOMP'
      WRITE(IRS,'(A,//)')'        Subsurface Transport Over Multiple Pha
     &ses'
      WRITE(IRS,'(A)')   ' This file was produced by STOMP, a numerical
     &simulator'
      WRITE(IRS,'(A)')   ' developed by the Pacific Northwest Laboratory
     &, with'
      WRITE(IRS,'(A)')   ' support from the VOC-Arid Integrated Demonstr
     &ation Project,'
      WRITE(IRS,'(A)')   ' Office of Technology Development, U.S. Depart
     &ment of Energy.'
      WRITE(IRS,'(A)')   ' Results from this version of STOMP should not
     & be used for'
      WRITE(IRS,'(A,/)') ' license related applications.'
      WRITE(IRS,'(A,/)') ' For inquiries or assistance:  Call (509) 372-
     &6070'
      WRITE(IRS,'(A,//)')'                       ---  RESTART  ---'

      WRITE(IRS,'(2A)') 'Version: ',CH_VRSN




!      WRITE(IRS,'(2A)') 'Date: ',CHDATE
!      WRITE(IRS,'(2A)') 'Time: ',CHTIME
      WRITE(IRS,'(A)') 'Date: '
      WRITE(IRS,'(A)') 'Time: '

!
!---  Write timing data, field data by node numbers  ---
!

      NSPR = NSPL + NSPS + NSPE + NSPG + NSPN
      WRITE(IRS,'(/,7(1PE22.15),8(I9))') TM,DT,DTMX,DTMN,DTAF,
     &  RSDMX,DTCF,NRIMX,NSTEP,NFLD,NSOLU,IOM,ISLC(5),NSPR,NFBN





!
!---  Water Operational Mode ---
!
      IF( IOM.EQ.1 ) THEN
        NRSV = 8
        IF( (NSOLU+NSPR).LE.0 ) THEN
          WRITE( FORM8(2:2),'(I1)' ) NRSV
          DO 110 N = 1,NFBN
            WRITE(IRS,FORM8) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),IPH(2,N)
  110     CONTINUE
        ELSEIF( (NSOLU+NSPR).LT.10 ) THEN
          WRITE( FORM9(2:2),'(I1)' ) NRSV
          WRITE( FORM9(26:26),'(I1)' ) NSOLU+NSPR
          DO 114 N = 1,NFBN

!
!---        Species concentrations  ---
!
            DO 112 NSP = 1,NSPR
              IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
                NSP_M = NSP-NSPL
                IF( ISP_MN(NSP).EQ.1 ) THEN
                  SP_CX(NSP) = SP_C(N,NSP)+SP_CMN(N,NSP_M)
                ELSE
                  SP_CX(NSP) = SP_C(N,NSP)
                ENDIF
              ELSE
                SP_CX(NSP) = SP_C(N,NSP)
              ENDIF
  112       CONTINUE
            WRITE(IRS,FORM9) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),IPH(2,N),
     &        (C(N,NSL),NSL=1,NSOLU),(SP_CX(NSP),NSP=1,NSPR)





  114     CONTINUE
        ELSEIF( (NSOLU+NSPR).LT.100 ) THEN
          WRITE( FORM10(2:2),'(I1)' ) NRSV
          WRITE( FORM10(26:27),'(I2)' ) NSOLU+NSPR
          DO 117 N = 1,NFBN

!
!---        Species concentrations  ---
!
            DO 116 NSP = 1,NSPR
              IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
                NSP_M = NSP-NSPL
                IF( ISP_MN(NSP).EQ.1 ) THEN
                  SP_CX(NSP) = SP_C(N,NSP)+SP_CMN(N,NSP_M)
                ELSE
                  SP_CX(NSP) = SP_C(N,NSP)
                ENDIF
              ELSE
                SP_CX(NSP) = SP_C(N,NSP)
              ENDIF
  116       CONTINUE
            WRITE(IRS,FORM10) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),IPH(2,N),
     &        (C(N,NSL),NSL=1,NSOLU),(SP_CX(NSP),NSP=1,NSPR)





  117     CONTINUE
        ELSE
          WRITE( FORM15(2:2),'(I1)' ) NRSV
          WRITE( FORM15(26:28),'(I3)' ) NSOLU+NSPR
          DO 119 N = 1,NFBN

!
!---        Species concentrations  ---
!
            DO 118 NSP = 1,NSPR
              IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
                NSP_M = NSP-NSPL
                IF( ISP_MN(NSP).EQ.1 ) THEN
                  SP_CX(NSP) = SP_C(N,NSP)+SP_CMN(N,NSP_M)
                ELSE
                  SP_CX(NSP) = SP_C(N,NSP)
                ENDIF
              ELSE
                SP_CX(NSP) = SP_C(N,NSP)
              ENDIF
  118       CONTINUE
            WRITE(IRS,FORM15) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),IPH(2,N),
     &        (C(N,NSL),NSL=1,NSOLU),(SP_CX(NSP),NSP=1,NSPR)





  119     CONTINUE
        ENDIF
!
!---  Water-Air-Energy Operational Mode ---
!
      ELSEIF( IOM.EQ.3 .AND. ISLC(5).NE.1 ) THEN
        NRSV = 11
        IF( NSOLU.LE.0 ) THEN
          WRITE( FORM5(2:3),'(I2)' ) NRSV
          DO 130 N = 1,NFBN
            WRITE(IRS,FORM5) T(2,N),PL(2,N),PG(2,N),PVW(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),SG(2,N),PVA(2,N),PCMP(N),TCMP(N),
     &        NPHAZ(2,N)
  130     CONTINUE
        ELSEIF( NSOLU.LT.10 ) THEN
          WRITE( FORM6(2:3),'(I2)' ) NRSV
          WRITE( FORM6(24:24),'(I1)' ) NSOLU
          DO 132 N = 1,NFBN
            WRITE(IRS,FORM6) T(2,N),PL(2,N),PG(2,N),PVW(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),SG(2,N),PVA(2,N),PCMP(N),TCMP(N),
     &        NPHAZ(2,N),(C(N,NSL),NSL=1,NSOLU)
  132     CONTINUE
        ELSE
          WRITE( FORM7(2:3),'(I2)' ) NRSV
          WRITE( FORM7(23:24),'(I2)' ) NSOLU
          DO 134 N = 1,NFBN
            WRITE(IRS,FORM7) T(2,N),PL(2,N),PG(2,N),PVW(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),SG(2,N),PVA(2,N),PCMP(N),TCMP(N),
     &        NPHAZ(2,N),(C(N,NSL),NSL=1,NSOLU)
  134     CONTINUE
        ENDIF
!
!---  Water-Air-Energy Operational Mode w/ Freezing Conditions ---
!
      ELSEIF( IOM.EQ.3 .AND. ISLC(5).EQ.1 ) THEN
        NRSV = 12
        IF( NSOLU.LE.0 ) THEN
          WRITE( FORM5(2:3),'(I2)' ) NRSV
          DO 136 N = 1,NFBN
            WRITE(IRS,FORM5) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),SG(2,N),PVA(2,N),SI(2,N),
     &        PI(2,N),PCMP(N),TCMP(N),NPHAZ(2,N)
  136     CONTINUE
        ELSEIF( NSOLU.LT.10 ) THEN
          WRITE( FORM6(2:3),'(I2)' ) NRSV
          WRITE( FORM6(24:24),'(I1)' ) NSOLU
          DO 137 N = 1,NFBN
            WRITE(IRS,FORM6) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),SG(2,N),PVA(2,N),SI(2,N),
     &        PI(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,NSL),NSL=1,NSOLU)
  137     CONTINUE
        ELSE
          WRITE( FORM7(2:3),'(I2)' ) NRSV
          WRITE( FORM7(23:24),'(I2)' ) NSOLU
          DO 138 N = 1,NFBN
            WRITE(IRS,FORM7) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),SG(2,N),PVA(2,N),SI(2,N),
     &        PI(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,NSL),NSL=1,NSOLU)
  138     CONTINUE
        ENDIF
!
!---  Water-Oil Operational Mode ---
!
      ELSEIF( IOM.EQ.4 ) THEN
        NRSV = 17
        IF( NSOLU.LE.0 ) THEN
          WRITE( FORM5(2:3),'(I2)' ) NRSV
          DO 140 N = 1,NFBN
            WRITE(IRS,FORM5) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),SNT(2,N),SN(2,N),XMLO(2,N),
     &        PN(2,N),SGTL(N),SGTN(N),ASTMIN(2,N),TRPNL(2,N),
     &        ASTMAX(2,N),PCMP(N),TCMP(N),NPHAZ(2,N)
  140     CONTINUE
        ELSEIF( NSOLU.LT.10 ) THEN
          WRITE( FORM6(2:3),'(I2)' ) NRSV
          WRITE( FORM6(24:24),'(I1)' ) NSOLU
          DO 142 N = 1,NFBN
            WRITE(IRS,FORM6) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),SNT(2,N),SN(2,N),XMLO(2,N),
     &        PN(2,N),SGTL(N),SGTN(N),ASTMIN(2,N),TRPNL(2,N),
     &        ASTMAX(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),
     &        (C(N,NSL),NSL=1,NSOLU)
  142     CONTINUE
        ELSE
          WRITE( FORM7(2:3),'(I2)' ) NRSV
          WRITE( FORM7(23:24),'(I2)' ) NSOLU
          DO 144 N = 1,NFBN
            WRITE(IRS,FORM7) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),SNT(2,N),SN(2,N),XMLO(2,N),
     &        PN(2,N),SGTL(N),SGTN(N),ASTMIN(2,N),TRPNL(2,N),
     &        ASTMAX(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),
     &        (C(N,NSL),NSL=1,NSOLU)
  144     CONTINUE
        ENDIF
!
!---  Water-Oil-Air Operational Mode ---
!
      ELSEIF( IOM.EQ.5 ) THEN
        NRSV = 18
        IF( NSOLU.LE.0 ) THEN
          WRITE( FORM5(2:3),'(I2)' ) NRSV
          DO 150 N = 1,NFBN
            WRITE(IRS,FORM5) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),SNT(2,N),SN(2,N),XMLO(2,N),
     &        XMLA(2,N),PN(2,N),SGTL(N),SGTN(N),ASTMIN(2,N),
     &        TRPNL(2,N),ASTMAX(2,N),PCMP(N),TCMP(N),NPHAZ(2,N)
  150     CONTINUE
        ELSEIF( NSOLU.LT.10 ) THEN
          WRITE( FORM6(2:3),'(I2)' ) NRSV
          WRITE( FORM6(24:24),'(I1)' ) NSOLU
          DO 152 N = 1,NFBN
            WRITE(IRS,FORM6) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),SNT(2,N),SN(2,N),XMLO(2,N),
     &        XMLA(2,N),PN(2,N),SGTL(N),SGTN(N),ASTMIN(2,N),
     &        TRPNL(2,N),ASTMAX(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),
     &        (C(N,NSL),NSL=1,NSOLU)
  152     CONTINUE
        ELSE
          WRITE( FORM7(2:3),'(I2)' ) NRSV
          WRITE( FORM7(23:24),'(I2)' ) NSOLU
          DO 154 N = 1,NFBN
            WRITE(IRS,FORM7) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),SNT(2,N),SN(2,N),XMLO(2,N),
     &        XMLA(2,N), PN(2,N),SGTL(N),SGTN(N),ASTMIN(2,N),
     &        TRPNL(2,N),ASTMAX(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),
     &        (C(N,NSL),NSL=1,NSOLU)
  154     CONTINUE
        ENDIF
!
!---  H2O-NaCl-CO2 [Energy] Operational Modes ---
!
      ELSEIF( IOM.EQ.32 .OR. IOM.EQ.33 ) THEN
        NRSV = 12
        IF( (NSOLU+NSPR).LE.0 ) THEN
          WRITE( FORM5(2:3),'(I2)' ) NRSV
          DO 320 N = 1,NFBN
            WRITE(IRS,FORM5) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),SG(2,N),XLA(2,N),YLS(2,N),TMS(2,N),
     &        PCMP(N),TCMP(N),NPHAZ(2,N)
  320     CONTINUE
        ELSEIF( (NSOLU+NSPR).LT.10 ) THEN
          WRITE( FORM6(2:3),'(I2)' ) NRSV
          WRITE( FORM6(24:24),'(I1)' ) NSOLU+NSPR
          DO 324 N = 1,NFBN

!
!---        Species concentrations  ---
!
            DO 322 NSP = 1,NSPR
              IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
                NSP_M = NSP-NSPL
                IF( ISP_MN(NSP).EQ.1 ) THEN
                  SP_CX(NSP) = SP_C(N,NSP)+SP_CMN(N,NSP_M)
                ELSE
                  SP_CX(NSP) = SP_C(N,NSP)
                ENDIF
              ELSE
                SP_CX(NSP) = SP_C(N,NSP)
              ENDIF
  322       CONTINUE
            WRITE(IRS,FORM6) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),SG(2,N),XLA(2,N),YLS(2,N),TMS(2,N),
     &        PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,NSL),NSL=1,NSOLU),
     &        (SP_CX(NSP),NSP=1,NSPR)





  324     CONTINUE
        ELSEIF( (NSOLU+NSPR).LT.100 ) THEN
          WRITE( FORM7(2:3),'(I2)' ) NRSV
          WRITE( FORM7(23:24),'(I2)' ) NSOLU+NSPR
          DO 327 N = 1,NFBN

!
!---        Species concentrations  ---
!
            DO 326 NSP = 1,NSPR
              IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
                NSP_M = NSP-NSPL
                IF( ISP_MN(NSP).EQ.1 ) THEN
                  SP_CX(NSP) = SP_C(N,NSP)+SP_CMN(N,NSP_M)
                ELSE
                  SP_CX(NSP) = SP_C(N,NSP)
                ENDIF
              ELSE
                SP_CX(NSP) = SP_C(N,NSP)
              ENDIF
  326       CONTINUE
            WRITE(IRS,FORM7) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),SG(2,N),XLA(2,N),YLS(2,N),TMS(2,N),
     &        PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,NSL),NSL=1,NSOLU),
     &        (SP_CX(NSP),NSP=1,NSPR)





  327     CONTINUE
        ELSE
          WRITE( FORM14(2:3),'(I2)' ) NRSV
          WRITE( FORM14(23:25),'(I3)' ) NSOLU+NSPR
          DO 329 N = 1,NFBN

!
!---        Species concentrations  ---
!
            DO 328 NSP = 1,NSPR
              IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
                NSP_M = NSP-NSPL
                IF( ISP_MN(NSP).EQ.1 ) THEN
                  SP_CX(NSP) = SP_C(N,NSP)+SP_CMN(N,NSP_M)
                ELSE
                  SP_CX(NSP) = SP_C(N,NSP)
                ENDIF
              ELSE
                SP_CX(NSP) = SP_C(N,NSP)
              ENDIF
  328       CONTINUE
            WRITE(IRS,FORM14) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),SG(2,N),XLA(2,N),YLS(2,N),TMS(2,N),
     &        PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,NSL),NSL=1,NSOLU),
     &        (SP_CX(NSP),NSP=1,NSPR)





  329     CONTINUE
        ENDIF
!
!---  HYD Operational Modes ---
!
      ELSEIF( IOM.EQ.37 ) THEN
        NRSV = 14
        IF( NSOLU.LE.0 ) THEN
          WRITE( FORM5(2:3),'(I2)' ) NRSV
          DO 360 N = 1,NFBN
            WRITE(IRS,FORM5) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SG(2,N),SH(2,N),YLS(2,N),PVA(2,N),PVO(2,N),
     &        SN(2,N),PN(2,N),YMGO(2,N),
     &        PCMP(N),TCMP(N),NPHAZ(2,N)
  360     CONTINUE
        ELSEIF( NSOLU.LT.10 ) THEN
          WRITE( FORM6(2:3),'(I2)' ) NRSV
          WRITE( FORM6(24:24),'(I1)' ) NSOLU
          DO 362 N = 1,NFBN
            WRITE(IRS,FORM6) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SG(2,N),SH(2,N),YLS(2,N),PVA(2,N),PVO(2,N),
     &        SN(2,N),PN(2,N),YMGO(2,N),
     &        PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,NSL),NSL=1,NSOLU)
  362     CONTINUE
        ELSE
          WRITE( FORM7(2:3),'(I2)' ) NRSV
          WRITE( FORM7(23:24),'(I2)' ) NSOLU
          DO 364 N = 1,NFBN
            WRITE(IRS,FORM7) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SG(2,N),SH(2,N),YLS(2,N),PVA(2,N),PVO(2,N),
     &        SN(2,N),PN(2,N),YMGO(2,N),
     &        PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,NSL),NSL=1,NSOLU)
  364     CONTINUE
        ENDIF
!
!---  HYD-KE Operational Mode ---
!
      ELSEIF( IOM.EQ.38 ) THEN
        NRSV = 19
        IF( NSOLU.LE.0 ) THEN
          WRITE( FORM5(2:3),'(I2)' ) NRSV
          DO 380 N = 1,NFBN
            WRITE(IRS,FORM5) T(2,N),PL(2,N),PG(2,N),PN(2,N),
     &        PVA(2,N),PVO(2,N),SG(2,N),SL(2,N),SH(2,N),SN(2,N),
     &        YLS(2,N),PVHA(2,N),PVHO(2,N),YMGO(2,N),YMHGO(2,N),
     &        TMHA(2,N),TMHO(2,N),PCMP(N),TCMP(N),NPHAZ(2,N)
  380     CONTINUE
        ELSEIF( NSOLU.LT.10 ) THEN
          WRITE( FORM6(2:3),'(I2)' ) NRSV
          WRITE( FORM6(24:24),'(I1)' ) NSOLU
          DO 382 N = 1,NFBN
            WRITE(IRS,FORM6) T(2,N),PL(2,N),PG(2,N),PN(2,N),
     &        PVA(2,N),PVO(2,N),SG(2,N),SL(2,N),SH(2,N),SN(2,N),
     &        YLS(2,N),PVHA(2,N),PVHO(2,N),YMGO(2,N),YMHGO(2,N),
     &        TMHA(2,N),TMHO(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),
     &        (C(N,NSL),NSL=1,NSOLU)
  382     CONTINUE
        ELSE
          WRITE( FORM7(2:3),'(I2)' ) NRSV
          WRITE( FORM7(23:24),'(I2)' ) NSOLU
          DO 384 N = 1,NFBN
            WRITE(IRS,FORM7) T(2,N),PL(2,N),PG(2,N),PN(2,N),
     &        PVA(2,N),PVO(2,N),SG(2,N),SL(2,N),SH(2,N),SN(2,N),
     &        YLS(2,N),PVHA(2,N),PVHO(2,N),YMGO(2,N),YMHGO(2,N),
     &        TMHA(2,N),TMHO(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),
     &        (C(N,NSL),NSL=1,NSOLU)
  384     CONTINUE
        ENDIF
!
!---  STOMP-HYDT-KE Operational Mode ---
!
      ELSEIF( IOM.EQ.39 ) THEN
        NRSV = 25
        IF( NSOLU.LE.0 ) THEN
          WRITE( FORM5(2:3),'(I2)' ) NRSV
          DO 390 N = 1,NFBN
            WRITE(IRS,FORM5) T(2,N),PL(2,N),PG(2,N),PN(2,N),PSO(2,N),
     &        PVA(2,N),PVO(2,N),PVN(2,N),
     &        ZMCA(2,N),ZMCO(2,N),ZMCN(2,N),SG(2,N),SL(2,N),SN(2,N),
     &        SH(2,N),SI(2,N),YLS(2,N),TMHA(2,N),TMHO(2,N),TMHN(2,N),
     &        YMHGA(2,N),YMHGO(2,N),YMHGN(2,N),PCMP(N),TCMP(N),
     &        NPHAZ(2,N)
  390     CONTINUE
        ELSEIF( NSOLU.LT.10 ) THEN
          WRITE( FORM6(2:3),'(I2)' ) NRSV
          WRITE( FORM6(24:24),'(I1)' ) NSOLU
          DO 392 N = 1,NFBN
            WRITE(IRS,FORM6) T(2,N),PL(2,N),PG(2,N),PN(2,N),PSO(2,N),
     &        PVA(2,N),PVO(2,N),PVN(2,N),
     &        ZMCA(2,N),ZMCO(2,N),ZMCN(2,N),SG(2,N),SL(2,N),SN(2,N),
     &        SH(2,N),SI(2,N),YLS(2,N),TMHA(2,N),TMHO(2,N),TMHN(2,N),
     &        YMHGA(2,N),YMHGO(2,N),YMHGN(2,N),PCMP(N),TCMP(N),
     &        NPHAZ(2,N),(C(N,NSL),NSL=1,NSOLU)
  392     CONTINUE
        ELSE
          WRITE( FORM7(2:3),'(I2)' ) NRSV
          WRITE( FORM7(23:24),'(I2)' ) NSOLU
          DO 394 N = 1,NFBN
            WRITE(IRS,FORM7) T(2,N),PL(2,N),PG(2,N),PN(2,N),PSO(2,N),
     &        PVA(2,N),PVO(2,N),PVN(2,N),
     &        ZMCA(2,N),ZMCO(2,N),ZMCN(2,N),SG(2,N),SL(2,N),SN(2,N),
     &        SH(2,N),SI(2,N),YLS(2,N),TMHA(2,N),TMHO(2,N),TMHN(2,N),
     &        YMHGA(2,N),YMHGO(2,N),YMHGN(2,N),PCMP(N),TCMP(N),
     &        NPHAZ(2,N),(C(N,NSL),NSL=1,NSOLU)
  394     CONTINUE
        ENDIF
!
!---  H2O-NaCl-NComponent-Energy Operational Mode ---
!
      ELSEIF( IOM.EQ.40 ) THEN
        NRSV = 10+NGC*2
        IF( (NSOLU+NSPR).LE.0 ) THEN
          WRITE( FORM5(2:3),'(I2)' ) NRSV
          DO 400 N = 1,NFBN
            WRITE(IRS,FORM5) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),SG(2,N),(PVC(IGC,2,N),IGC=1,NGC),
     &        (XLC(IGC,2,N),IGC=1,NGC),YLS(2,N),PCMP(N),TCMP(N),
     &        NPHAZ(2,N)
  400     CONTINUE
        ELSEIF( (NSOLU+NSPR).LT.10 ) THEN
          WRITE( FORM6(2:3),'(I2)' ) NRSV
          WRITE( FORM6(24:24),'(I1)' ) NSOLU+NSPR
          DO 404 N = 1,NFBN

!
!---        Species concentrations  ---
!
            DO 402 NSP = 1,NSPR
              IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
                NSP_M = NSP-NSPL
                IF( ISP_MN(NSP).EQ.1 ) THEN
                  SP_CX(NSP) = SP_C(N,NSP)+SP_CMN(N,NSP_M)
                ELSE
                  SP_CX(NSP) = SP_C(N,NSP)
                ENDIF
              ELSE
                SP_CX(NSP) = SP_C(N,NSP)
              ENDIF
              IF( SP_CX(NSP).LT.CMIN ) SP_CX(NSP) = 0.D+0
  402       CONTINUE
            WRITE(IRS,FORM6) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),SG(2,N),(PVC(IGC,2,N),IGC=1,NGC),
     &        (XLC(IGC,2,N),IGC=1,NGC),YLS(2,N),
     &        PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,NSL),NSL=1,NSOLU),
     &        (SP_CX(NSP),NSP=1,NSPR)






  404     CONTINUE
        ELSEIF( (NSOLU+NSPR).LT.100 ) THEN
          WRITE( FORM7(2:3),'(I2)' ) NRSV
          WRITE( FORM7(23:24),'(I2)' ) NSOLU+NSPR
          DO 407 N = 1,NFBN

!
!---        Species concentrations  ---
!
            DO 406 NSP = 1,NSPR
              IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
                NSP_M = NSP-NSPL
                IF( ISP_MN(NSP).EQ.1 ) THEN
                  SP_CX(NSP) = SP_C(N,NSP)+SP_CMN(N,NSP_M)
                ELSE
                  SP_CX(NSP) = SP_C(N,NSP)
                ENDIF
              ELSE
                SP_CX(NSP) = SP_C(N,NSP)
              ENDIF
  406       CONTINUE
            WRITE(IRS,FORM7) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),SG(2,N),(PVC(IGC,2,N),IGC=1,NGC),
     &        (XLC(IGC,2,N),IGC=1,NGC),YLS(2,N),
     &        PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,NSL),NSL=1,NSOLU),
     &        (SP_CX(NSP),NSP=1,NSPR)






  407     CONTINUE
        ELSE
          WRITE( FORM14(2:3),'(I2)' ) NRSV
          WRITE( FORM14(24:26),'(I3)' ) NSOLU+NSPR
          DO 409 N = 1,NFBN

!
!---        Species concentrations  ---
!
            DO 408 NSP = 1,NSPR
              IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
                NSP_M = NSP-NSPL
                IF( ISP_MN(NSP).EQ.1 ) THEN
                  SP_CX(NSP) = SP_C(N,NSP)+SP_CMN(N,NSP_M)
                ELSE
                  SP_CX(NSP) = SP_C(N,NSP)
                ENDIF
              ELSE
                SP_CX(NSP) = SP_C(N,NSP)
              ENDIF
  408       CONTINUE
            WRITE(IRS,FORM14) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),SG(2,N),(PVC(IGC,2,N),IGC=1,NGC),
     &        (XLC(IGC,2,N),IGC=1,NGC),YLS(2,N),
     &        PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,NSL),NSL=1,NSOLU),
     &        (SP_CX(NSP),NSP=1,NSPR)






  409     CONTINUE
        ENDIF
!
!---  EOR Operational Mode ---
!
      ELSEIF( IOM.EQ.43 ) THEN
        NRSV = 14 + 2*(NGC+2)
        IF( (NSOLU+NSPR).LE.0 ) THEN
          WRITE( FORM5(2:3),'(I2)' ) NRSV
          DO 430 N = 1,NFBN
            WRITE(IRS,FORM5) T(2,N),PG(2,N),PL(2,N),PN(2,N),POSM(2,N),
     &        PSO(2,N),PVA(2,N),SG(2,N),SL(2,N),SN(2,N),YLS(2,N),
     &        TMS(2,N),(TMC(IGC,2,N),IGC=1,NGC+2),
     &        (ZMC(IGC,2,N),IGC=1,NGC+2),PCMP(N),TCMP(N),NPHAZ(2,N)
  430     CONTINUE
        ELSEIF( (NSOLU+NSPR).LT.10 ) THEN
          WRITE( FORM6(2:3),'(I2)' ) NRSV
          WRITE( FORM6(24:24),'(I1)' ) NSOLU+NSPR
          DO 434 N = 1,NFBN

!
!---        Species concentrations  ---
!
            DO 432 NSP = 1,NSPR
              IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
                NSP_M = NSP-NSPL
                IF( ISP_MN(NSP).EQ.1 ) THEN
                  SP_CX(NSP) = SP_C(N,NSP)+SP_CMN(N,NSP_M)
                ELSE
                  SP_CX(NSP) = SP_C(N,NSP)
                ENDIF
              ELSE
                SP_CX(NSP) = SP_C(N,NSP)
              ENDIF
  432       CONTINUE
            WRITE(IRS,FORM6) T(2,N),PG(2,N),PL(2,N),PN(2,N),POSM(2,N),
     &PSO(2,N),
     &        PVA(2,N),SG(2,N),SL(2,N),SN(2,N),YLS(2,N),TMS(2,N),
     &        (TMC(IGC,2,N),IGC=1,NGC+2),(ZMC(IGC,2,N),IGC=1,NGC+2),
     &        PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,NSL),NSL=1,NSOLU),
     &        (SP_CX(NSP),NSP=1,NSPR)







  434     CONTINUE
        ELSEIF( (NSOLU+NSPR).LT.100 ) THEN
          WRITE( FORM7(2:3),'(I2)' ) NRSV
          WRITE( FORM7(23:24),'(I2)' ) NSOLU+NSPR
          DO 437 N = 1,NFBN

!
!---        Species concentrations  ---
!
            DO 436 NSP = 1,NSPR
              IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
                NSP_M = NSP-NSPL
                IF( ISP_MN(NSP).EQ.1 ) THEN
                  SP_CX(NSP) = SP_C(N,NSP)+SP_CMN(N,NSP_M)
                ELSE
                  SP_CX(NSP) = SP_C(N,NSP)
                ENDIF
              ELSE
                SP_CX(NSP) = SP_C(N,NSP)
              ENDIF
  436       CONTINUE
            WRITE(IRS,FORM7) T(2,N),PG(2,N),PL(2,N),PN(2,N),POSM(2,N),
     &PSO(2,N),
     &        PVA(2,N),SG(2,N),SL(2,N),SN(2,N),YLS(2,N),TMS(2,N),
     &        (TMC(IGC,2,N),IGC=1,NGC+2),(ZMC(IGC,2,N),IGC=1,NGC+2),
     &        PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,NSL),NSL=1,NSOLU),
     &        (SP_CX(NSP),NSP=1,NSPR)







  437     CONTINUE
        ELSE
          WRITE( FORM14(2:3),'(I2)' ) NRSV
          WRITE( FORM14(23:25),'(I3)' ) NSOLU+NSPR
          DO 439 N = 1,NFBN

!
!---        Species concentrations  ---
!
            DO 438 NSP = 1,NSPR
              IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
                NSP_M = NSP-NSPL
                IF( ISP_MN(NSP).EQ.1 ) THEN
                  SP_CX(NSP) = SP_C(N,NSP)+SP_CMN(N,NSP_M)
                ELSE
                  SP_CX(NSP) = SP_C(N,NSP)
                ENDIF
              ELSE
                SP_CX(NSP) = SP_C(N,NSP)
              ENDIF
  438       CONTINUE
            WRITE(IRS,FORM14) T(2,N),PG(2,N),PL(2,N),PN(2,N),POSM(2,N),
     &PSO(2,N),
     &        PVA(2,N),SG(2,N),SL(2,N),SN(2,N),YLS(2,N),TMS(2,N),
     &        (TMC(IGC,2,N),IGC=1,NGC+2),(ZMC(IGC,2,N),IGC=1,NGC+2),
     &        PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,NSL),NSL=1,NSOLU),
     &        (SP_CX(NSP),NSP=1,NSPR)







  439     CONTINUE
        ENDIF
      ENDIF
!
!---  Geomechanics data  ---
!
      IF( ISLC(50).NE.0 ) THEN
        WRITE(IRS,'(A)') 'Geomechanics Model Data'
        WRITE(FORM1(3:3),'(I1)') ICOUNT(NFEN_GM)
        WRITE(IRS,FORM1) NFEN_GM
        DO NFEN = 1,NFEN_GM
          WRITE(IRS,'(3(1PE22.15,1X))') U_GM(1,NFEN),V_GM(1,NFEN),
     &      W_GM(1,NFEN)
        ENDDO
      ENDIF
!
!---  Coupled-well data  ---
!
      IF( N_CW.GT.0 ) THEN
        WRITE(IRS,'(A)') 'Coupled-Well Model Data'
        WRITE(FORM1(3:3),'(I1)') ICOUNT(N_CW)
        WRITE(IRS,FORM1) N_CW
        DO NCW = 1,N_CW
          WRITE(IRS,'(1PE22.15)') P_CW(2,NCW)
        ENDDO
      ENDIF
!
!---  Close the restart file  ---
!
      CLOSE( UNIT=IRS )
      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRRST group
!
      RETURN
      END

