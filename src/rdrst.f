!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDRST( INDX )
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
!     Read restart file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, December 1992.
!     Last Modified by MD White, Battelle, PNL, October 15, 1997.




!     $Id: rdrst.F 1080 2017-03-14 16:22:02Z d3c002 $
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
      CHARACTER*512 CHDUM
      CHARACTER*64 ADUM
      LOGICAL EX
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
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM1,FORM2,FORM3,FORM4,FORM5,FORM6,FORM7
      SAVE FORM8,FORM9,FORM10,FORM11,FORM12,FORM13
      SAVE FORM14,FORM15
      SAVE NSOLUX,IROM,IRFC,NSPRX
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
      SUB_LOG(ISUB_LOG) = '/RDRST'
      IF( INDEX(SVN_ID(159)(1:1),'$').EQ.0 ) SVN_ID(159) =
     & '$Id: rdrst.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Open the restart file  ---
!
      IF( INDX.EQ.1 ) THEN
        INQUIRE( FILE=FNRS, EXIST=EX )
        IF( .NOT.EX ) THEN
          INDX = 3
          NCH = INDEX(FNRS(1:),'  ')-1
          CHMSG = 'Nonexistent Restart File: ' // FNRS(1:NCH)
          CALL WRMSGS( INDX )
        ELSE
          OPEN(UNIT=IRS, FILE=FNRS, STATUS='OLD', FORM='FORMATTED')
        ENDIF
        DO 100 N = 1,23




          READ (IRS, '(A)') CHDUM

          IF( INDEX(CHDUM(1:),'Version').NE.0 ) THEN
            NCHA = INDEX(CHDUM(1:),'  ')-1
            IF( NCHA.GT.10 ) THEN
              ADUM = CHDUM(10:NCHA)
            ELSE
              ADUM = 'Unknown'
            ENDIF
            NCH = INDEX(CH_VRSN(1:),'  ')-1
            NCHA = INDEX(ADUM(1:),'  ')-1
            IF( ADUM(1:NCHA).NE.CH_VRSN(1:NCH) ) THEN
              INDX = 1
              CHMSG = 'Restart Version Number: ' // ADUM(1:NCHA)
              CALL WRMSGS( INDX )
            ENDIF
          ENDIF
  100   CONTINUE
!
!---  Read timing data  ---
!

        READ(IRS,'(7(1PE22.15),8(I9))') TMPS(1),TMPD(1),TMPX(1),
     &    TMPN(1),TMPA(1),RSDM(1),TMPC(1),
     &    NRIM(1),NRST,NFLD,NSOLUX,IROM,IRFC,NSPRX,NFBN






        NBRN = NFBN - NFLD
        NSTEP = NRST
        IF( IROM.NE.IOM .AND. IROM.NE.ISLC(21) ) THEN
          INDX = 3
          CHMSG = 'Restart File Operational Mode Conflict'
          CALL WRMSGS( INDX )
        ENDIF
!
!---  Read field data by node numbers  ---
!
      ELSE
!
!---  Water Operational Mode ---
!
        IF( IROM.EQ.1 ) THEN
          NRSV = 8
          IF( (NSOLUX+NSPRX).LE.0 ) THEN
            WRITE( FORM8(2:2),'(I1)' ) NRSV
            DO 110 N = 1,NFBN
              READ(IRS,FORM8) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),IPH(2,N)
  110       CONTINUE
          ELSEIF( (NSOLUX+NSPRX).LT.10 ) THEN
            WRITE( FORM9(2:2),'(I1)' ) NRSV
            WRITE( FORM9(26:26),'(I1)' ) NSOLUX+NSPRX
            DO 112 N = 1,NFBN

              READ(IRS,FORM9) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),
     &          IPH(2,N),(C(N,M),M=1,NSOLUX),(SP_C(N,NSP),NSP=1,NSPRX)





  112       CONTINUE
          ELSEIF( (NSOLUX+NSPRX).LT.100 ) THEN
            WRITE( FORM10(2:2),'(I1)' ) NRSV
            WRITE( FORM10(26:27),'(I2)' ) NSOLUX+NSPRX
            DO 114 N = 1,NFBN

              READ(IRS,FORM10) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),
     &          IPH(2,N),(C(N,M),M=1,NSOLUX),(SP_C(N,NSP),NSP=1,NSPRX)





  114       CONTINUE
          ELSE
            WRITE( FORM15(2:2),'(I1)' ) NRSV
            WRITE( FORM15(26:28),'(I3)' ) NSOLUX+NSPRX
            DO 116 N = 1,NFBN

              READ(IRS,FORM15) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),
     &          IPH(2,N),(C(N,M),M=1,NSOLUX),(SP_C(N,NSP),NSP=1,NSPRX)





  116       CONTINUE
          ENDIF
!
!---  Water-Air-Energy Operational Mode ---
!
        ELSEIF( IROM.EQ.3 .AND. IRFC.NE.1 ) THEN
          NRSV = 11
          IF( NSOLUX.LE.0 ) THEN
            WRITE( FORM5(2:3),'(I2)' ) NRSV
            DO 130 N = 1,NFBN
              READ(IRS,FORM5) T(2,N),PL(2,N),PG(2,N),PVW(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),SG(2,N),PVA(2,N),PCMP(N),TCMP(N),
     &          NPHAZ(2,N)
  130       CONTINUE
          ELSEIF( NSOLUX.LT.10 ) THEN
            WRITE( FORM6(2:3),'(I2)' ) NRSV
            WRITE( FORM6(23:23),'(I1)' ) NSOLUX
            DO 132 N = 1,NFBN
              READ(IRS,FORM6) T(2,N),PL(2,N),PG(2,N),PVW(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),SG(2,N),PVA(2,N),PCMP(N),TCMP(N),
     &          NPHAZ(2,N),(C(N,M),M=1,NSOLUX)
  132       CONTINUE
          ELSE
            WRITE( FORM7(2:3),'(I2)' ) NRSV
            WRITE( FORM7(23:24),'(I2)' ) NSOLUX
            DO 134 N = 1,NFBN
              READ(IRS,FORM7) T(2,N),PL(2,N),PG(2,N),PVW(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),SG(2,N),PVA(2,N),PCMP(N),TCMP(N),
     &          NPHAZ(2,N),(C(N,M),M=1,NSOLUX)
  134       CONTINUE
          ENDIF
!
!---  Water-Air-Energy Operational Mode w/ Freezing Conditions ---
!
        ELSEIF( IROM.EQ.3 .AND. IRFC.EQ.1 ) THEN
          NRSV = 12
          IF( NSOLUX.LE.0 ) THEN
            WRITE( FORM5(2:3),'(I2)' ) NRSV
            DO 136 N = 1,NFBN
              READ(IRS,FORM5) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),SG(2,N),PVA(2,N),SI(2,N),
     &          PI(2,N),PCMP(N),TCMP(N),NPHAZ(2,N)
  136       CONTINUE
          ELSEIF( NSOLUX.LT.10 ) THEN
            WRITE( FORM6(2:3),'(I2)' ) NRSV
            WRITE( FORM6(23:23),'(I1)' ) NSOLUX
            DO 137 N = 1,NFBN
              READ(IRS,FORM6) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),SG(2,N),PVA(2,N),SI(2,N),
     &          PI(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,M),M=1,NSOLUX)
  137       CONTINUE
          ELSE
            WRITE( FORM7(2:3),'(I2)' ) NRSV
            WRITE( FORM7(23:24),'(I2)' ) NSOLUX
            DO 138 N = 1,NFBN
              READ(IRS,FORM7) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),SG(2,N),PVA(2,N),SI(2,N),
     &          PI(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,M),M=1,NSOLUX)
  138       CONTINUE
          ENDIF
!
!---  Water-Oil Operational Mode ---
!
        ELSEIF( IROM.EQ.4 ) THEN
          NRSV = 17
          IF( NSOLUX.LE.0 ) THEN
            WRITE( FORM5(2:3),'(I2)' ) NRSV
            DO 140 N = 1,NFBN
              READ(IRS,FORM5) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),SNT(2,N),SN(2,N),XMLO(2,N),
     &          PN(2,N),SGTL(N),SGTN(N),ASTMIN(2,N),TRPNL(2,N),
     &          ASTMAX(2,N),PCMP(N),TCMP(N),NPHAZ(2,N)
  140       CONTINUE
          ELSEIF( NSOLUX.LT.10 ) THEN
            WRITE( FORM6(2:3),'(I2)' ) NRSV
            WRITE( FORM6(23:23),'(I1)' ) NSOLUX
            DO 142 N = 1,NFBN
              READ(IRS,FORM6) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),SNT(2,N),SN(2,N),XMLO(2,N),
     &          PN(2,N),SGTL(N),SGTN(N),ASTMIN(2,N),TRPNL(2,N),
     &          ASTMAX(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),
     &          (C(N,M),M=1,NSOLUX)
  142       CONTINUE
          ELSE
            WRITE( FORM7(2:3),'(I2)' ) NRSV
            WRITE( FORM7(23:24),'(I2)' ) NSOLUX
            DO 144 N = 1,NFBN
              READ(IRS,FORM7) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),SNT(2,N),SN(2,N),XMLO(2,N),
     &          PN(2,N),SGTL(N),SGTN(N),ASTMIN(2,N),TRPNL(2,N),
     &          ASTMAX(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),
     &          (C(N,M),M=1,NSOLUX)
  144       CONTINUE
          ENDIF
!
!---  Water-Oil-Air Operational Mode ---
!
        ELSEIF( IROM.EQ.5 ) THEN
          NRSV = 18
          IF( NSOLUX.LE.0 ) THEN
            WRITE( FORM5(2:3),'(I2)' ) NRSV
            DO 150 N = 1,NFBN
              READ(IRS,FORM5) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),SNT(2,N),SN(2,N),XMLO(2,N),
     &          XMLA(2,N),PN(2,N),SGTL(N),SGTN(N),ASTMIN(2,N),
     &          TRPNL(2,N),ASTMAX(2,N),PCMP(N),TCMP(N),NPHAZ(2,N)
  150       CONTINUE
          ELSEIF( NSOLUX.LT.10 ) THEN
            WRITE( FORM6(2:3),'(I2)' ) NRSV
            WRITE( FORM6(23:23),'(I1)' ) NSOLUX
            DO 152 N = 1,NFBN
              READ(IRS,FORM6) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),SNT(2,N),SN(2,N),XMLO(2,N),
     &          XMLA(2,N),PN(2,N),SGTL(N),SGTN(N),ASTMIN(2,N),
     &          TRPNL(2,N),ASTMAX(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),
     &          (C(N,M),M=1,NSOLUX)
  152       CONTINUE
          ELSE
            WRITE( FORM7(2:3),'(I2)' ) NRSV
            WRITE( FORM7(23:24),'(I2)' ) NSOLUX
            DO 154 N = 1,NFBN
              READ(IRS,FORM7) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),SNT(2,N),SN(2,N),XMLO(2,N),
     &          XMLA(2,N),PN(2,N),SGTL(N),SGTN(N),ASTMIN(2,N),
     &          TRPNL(2,N),ASTMAX(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),
     &          (C(N,M),M=1,NSOLUX)
  154       CONTINUE
          ENDIF
!
!---    H2O-NaCl-CO2 [Energy] Operational Modes ---
!
        ELSEIF( IROM.EQ.32 .OR. IROM.EQ.33 ) THEN
          NRSV = 12
          IF( (NSOLUX+NSPRX).LE.0 ) THEN
            WRITE( FORM5(2:3),'(I2)' ) NRSV
            DO 320 N = 1,NFBN
              READ(IRS,FORM5) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),SG(2,N),XLA(2,N),YLS(2,N),TMS(2,N),
     &          PCMP(N),TCMP(N),NPHAZ(2,N)
  320       CONTINUE
          ELSEIF( (NSOLUX+NSPRX).LT.10 ) THEN
            WRITE( FORM6(2:3),'(I2)' ) NRSV
            WRITE( FORM6(23:23),'(I1)' ) NSOLUX+NSPRX
            DO 322 N = 1,NFBN

              READ(IRS,FORM6) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),SG(2,N),XLA(2,N),YLS(2,N),TMS(2,N),
     &          PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,M),M=1,NSOLUX),
     &          (SP_C(N,NSP),NSP=1,NSPRX)





  322       CONTINUE
          ELSEIF( (NSOLUX+NSPRX).LT.100 ) THEN
            WRITE( FORM7(2:3),'(I2)' ) NRSV
            WRITE( FORM7(23:24),'(I2)' ) NSOLUX+NSPRX
            DO 324 N = 1,NFBN

              READ(IRS,FORM7) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),SG(2,N),XLA(2,N),YLS(2,N),TMS(2,N),
     &          PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,M),M=1,NSOLUX),
     &          (SP_C(N,NSP),NSP=1,NSPRX)





  324       CONTINUE
          ELSE
            WRITE( FORM14(2:3),'(I2)' ) NRSV
            WRITE( FORM14(23:25),'(I3)' ) NSOLUX+NSPRX
            DO 326 N = 1,NFBN

              READ(IRS,FORM14) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),SG(2,N),XLA(2,N),YLS(2,N),TMS(2,N),
     &          PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,M),M=1,NSOLUX),
     &          (SP_C(N,NSP),NSP=1,NSPRX)





  326       CONTINUE
          ENDIF
!
!---  HYD Operational Modes ---
!
        ELSEIF( IROM.EQ.37 ) THEN
          NRSV = 14
          IF( NSOLUX.LE.0 ) THEN
            WRITE( FORM5(2:3),'(I2)' ) NRSV
            DO 360 N = 1,NFBN
              READ(IRS,FORM5) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SG(2,N),SH(2,N),YLS(2,N),PVA(2,N),PVO(2,N),
     &          SN(2,N),PN(2,N),YMGO(2,N),
     &          PCMP(N),TCMP(N),NPHAZ(2,N)
  360       CONTINUE
          ELSEIF( NSOLUX.LT.10 ) THEN
            WRITE( FORM6(2:3),'(I2)' ) NRSV
            WRITE( FORM6(23:23),'(I1)' ) NSOLUX
            DO 362 N = 1,NFBN
              READ(IRS,FORM6) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SG(2,N),SH(2,N),YLS(2,N),PVA(2,N),PVO(2,N),
     &          SN(2,N),PN(2,N),YMGO(2,N),
     &          PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,M),M=1,NSOLUX)
  362       CONTINUE
          ELSE
            WRITE( FORM7(2:3),'(I2)' ) NRSV
            WRITE( FORM7(23:24),'(I2)' ) NSOLUX
            DO 364 N = 1,NFBN
              READ(IRS,FORM7) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SG(2,N),SH(2,N),YLS(2,N),PVA(2,N),PVO(2,N),
     &          SN(2,N),PN(2,N),YMGO(2,N),
     &          PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,M),M=1,NSOLUX)
  364       CONTINUE
          ENDIF
!
!---  HYD-KE Operational Modes ---
!
        ELSEIF( IROM.EQ.38 ) THEN
          NRSV = 19
          IF( NSOLUX.LE.0 ) THEN
            WRITE( FORM5(2:3),'(I2)' ) NRSV
            DO 380 N = 1,NFBN
              READ(IRS,FORM5) T(2,N),PL(2,N),PG(2,N),PN(2,N),
     &        PVA(2,N),PVO(2,N),SG(2,N),SL(2,N),SH(2,N),SN(2,N),
     &        YLS(2,N),PVHA(2,N),PVHO(2,N),YMGO(2,N),YMHGO(2,N),
     &        TMHA(2,N),TMHO(2,N),PCMP(N),TCMP(N),NPHAZ(2,N)
  380       CONTINUE
          ELSEIF( NSOLUX.LT.10 ) THEN
            WRITE( FORM6(2:3),'(I2)' ) NRSV
            WRITE( FORM6(23:23),'(I1)' ) NSOLUX
            DO 382 N = 1,NFBN
              READ(IRS,FORM6) T(2,N),PL(2,N),PG(2,N),PN(2,N),
     &        PVA(2,N),PVO(2,N),SG(2,N),SL(2,N),SH(2,N),SN(2,N),
     &        YLS(2,N),PVHA(2,N),PVHO(2,N),YMGO(2,N),YMHGO(2,N),
     &        TMHA(2,N),TMHO(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),
     &        (C(N,M),M=1,NSOLUX)
  382       CONTINUE
          ELSE
            WRITE( FORM7(2:3),'(I2)' ) NRSV
            WRITE( FORM7(23:24),'(I2)' ) NSOLUX
            DO 384 N = 1,NFBN
              READ(IRS,FORM7) T(2,N),PL(2,N),PG(2,N),PN(2,N),
     &        PVA(2,N),PVO(2,N),SG(2,N),SL(2,N),SH(2,N),SN(2,N),
     &        YLS(2,N),PVHA(2,N),PVHO(2,N),YMGO(2,N),YMHGO(2,N),
     &        TMHA(2,N),TMHO(2,N),PCMP(N),TCMP(N),NPHAZ(2,N),
     &        (C(N,M),M=1,NSOLUX)
  384       CONTINUE
          ENDIF
!
!---  STOMP-HYDT-KE Operational Mode ---
!
        ELSEIF( IROM.EQ.39 ) THEN
          NRSV = 25
          IF( NSOLUX.LE.0 ) THEN
            WRITE( FORM5(2:3),'(I2)' ) NRSV
            DO 390 N = 1,NFBN
              READ(IRS,FORM5) T(2,N),PL(2,N),PG(2,N),PN(2,N),PSO(2,N),
     &        PVA(2,N),PVO(2,N),PVN(2,N),
     &        ZMCA(2,N),ZMCO(2,N),ZMCN(2,N),SG(2,N),SL(2,N),SN(2,N),
     &        SH(2,N),SI(2,N),YLS(2,N),TMHA(2,N),TMHO(2,N),TMHN(2,N),
     &        YMHGA(2,N),YMHGO(2,N),YMHGN(2,N),PCMP(N),TCMP(N),
     &        NPHAZ(2,N)
  390       CONTINUE
          ELSEIF( NSOLUX.LT.10 ) THEN
            WRITE( FORM6(2:3),'(I2)' ) NRSV
            WRITE( FORM6(23:23),'(I1)' ) NSOLUX
            DO 392 N = 1,NFBN
              READ(IRS,FORM6) T(2,N),PL(2,N),PG(2,N),PN(2,N),PSO(2,N),
     &        PVA(2,N),PVO(2,N),PVN(2,N),
     &        ZMCA(2,N),ZMCO(2,N),ZMCN(2,N),SG(2,N),SL(2,N),SN(2,N),
     &        SH(2,N),SI(2,N),YLS(2,N),TMHA(2,N),TMHO(2,N),TMHN(2,N),
     &        YMHGA(2,N),YMHGO(2,N),YMHGN(2,N),PCMP(N),TCMP(N),
     &        NPHAZ(2,N),(C(N,NSL),NSL=1,NSOLUX)
  392       CONTINUE
          ELSE
            WRITE( FORM7(2:3),'(I2)' ) NRSV
            WRITE( FORM7(23:24),'(I2)' ) NSOLUX
            DO 394 N = 1,NFBN
              READ(IRS,FORM7) T(2,N),PL(2,N),PG(2,N),PN(2,N),PSO(2,N),
     &        PVA(2,N),PVO(2,N),PVN(2,N),
     &        ZMCA(2,N),ZMCO(2,N),ZMCN(2,N),SG(2,N),SL(2,N),SN(2,N),
     &        SH(2,N),SI(2,N),YLS(2,N),TMHA(2,N),TMHO(2,N),TMHN(2,N),
     &        YMHGA(2,N),YMHGO(2,N),YMHGN(2,N),PCMP(N),TCMP(N),
     &        NPHAZ(2,N),(C(N,NSL),NSL=1,NSOLUX)
  394       CONTINUE
          ENDIF
!
!---    H2O-NaCl-CO2 [Energy] Operational Modes ---
!
        ELSEIF( IROM.EQ.40 ) THEN
          NRSV = 10+NGC*2
          IF( (NSOLUX+NSPRX).LE.0 ) THEN
            WRITE( FORM5(2:3),'(I2)' ) NRSV
            DO 400 N = 1,NFBN
              READ(IRS,FORM5) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),SG(2,N),(PVC(IGC,2,N),IGC=1,NGC),
     &        (XLC(IGC,2,N),IGC=1,NGC),YLS(2,N),
     &          PCMP(N),TCMP(N),NPHAZ(2,N)
  400       CONTINUE
          ELSEIF( (NSOLUX+NSPRX).LT.10 ) THEN
            WRITE( FORM6(2:3),'(I2)' ) NRSV
            WRITE( FORM6(23:23),'(I1)' ) NSOLUX+NSPRX
            DO 402 N = 1,NFBN

              READ(IRS,FORM6) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),SG(2,N),(PVC(IGC,2,N),IGC=1,NGC),
     &          (XLC(IGC,2,N),IGC=1,NGC),YLS(2,N),
     &          PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,M),M=1,NSOLUX),
     &          (SP_C(N,NSP),NSP=1,NSPRX)






  402       CONTINUE
          ELSEIF( (NSOLUX+NSPRX).LT.100 ) THEN
            WRITE( FORM7(2:3),'(I2)' ) NRSV
            WRITE( FORM7(23:24),'(I2)' ) NSOLUX+NSPRX
            DO 404 N = 1,NFBN

              READ(IRS,FORM7) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &          SGT(2,N),ASLMIN(2,N),SG(2,N),(PVC(IGC,2,N),IGC=1,NGC),
     &        (XLC(IGC,2,N),IGC=1,NGC),YLS(2,N),
     &          PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,M),M=1,NSOLUX),
     &          (SP_C(N,NSP),NSP=1,NSPRX)






  404       CONTINUE
          ELSE
            WRITE( FORM14(2:3),'(I2)' ) NRSV
            WRITE( FORM14(23:25),'(I3)' ) NSOLUX+NSPRX
            DO 406 N = 1,NFBN

              READ(IRS,FORM14) T(2,N),PL(2,N),PG(2,N),SL(2,N),
     &        SGT(2,N),ASLMIN(2,N),SG(2,N),(PVC(IGC,2,N),IGC=1,NGC),
     &        (XLC(IGC,2,N),IGC=1,NGC),YLS(2,N),
     &        PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,M),M=1,NSOLUX),
     &        (SP_C(N,NSP),NSP=1,NSPRX)






  406       CONTINUE
          ENDIF
!
!---    EOR Operational Mode ---
!
        ELSEIF( IROM.EQ.43 ) THEN
          NRSV = 14 + 2*(NGC+2)
          IF( (NSOLUX+NSPRX).LE.0 ) THEN
            WRITE( FORM5(2:3),'(I2)' ) NRSV
            DO 430 N = 1,NFBN
              READ(IRS,FORM5) T(2,N),PG(2,N),PL(2,N),PN(2,N),POSM(2,N),
     &        PSO(2,N),PVA(2,N),SG(2,N),SL(2,N),SN(2,N),YLS(2,N),
     &        TMS(2,N),(TMC(IGC,2,N),IGC=1,NGC+2),
     &        (ZMC(IGC,2,N),IGC=1,NGC+2),PCMP(N),TCMP(N),NPHAZ(2,N)
  430       CONTINUE
          ELSEIF( (NSOLUX+NSPRX).LT.10 ) THEN
            WRITE( FORM6(2:3),'(I2)' ) NRSV
            WRITE( FORM6(23:23),'(I1)' ) NSOLUX+NSPRX
            DO 432 N = 1,NFBN

              READ(IRS,FORM6) T(2,N),PG(2,N),PL(2,N),PN(2,N),POSM(2,N),
     &PSO(2,N),
     &        PVA(2,N),SG(2,N),SL(2,N),SN(2,N),YLS(2,N),TMS(2,N),
     &        (TMC(IGC,2,N),IGC=1,NGC+2),(ZMC(IGC,2,N),IGC=1,NGC+2),
     &        PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,M),M=1,NSOLUX),
     &        (SP_C(N,NSP),NSP=1,NSPRX)







  432       CONTINUE
          ELSEIF( (NSOLUX+NSPRX).LT.100 ) THEN
            WRITE( FORM7(2:3),'(I2)' ) NRSV
            WRITE( FORM7(23:24),'(I2)' ) NSOLUX+NSPRX
            DO 434 N = 1,NFBN

              READ(IRS,FORM7) T(2,N),PG(2,N),PL(2,N),PN(2,N),POSM(2,N),
     &PSO(2,N),
     &        PVA(2,N),SG(2,N),SL(2,N),SN(2,N),YLS(2,N),TMS(2,N),
     &        (TMC(IGC,2,N),IGC=1,NGC+2),(ZMC(IGC,2,N),IGC=1,NGC+2),
     &        PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,M),M=1,NSOLUX),
     &        (SP_C(N,NSP),NSP=1,NSPRX)







  434       CONTINUE
          ELSE
            WRITE( FORM14(2:3),'(I2)' ) NRSV
            WRITE( FORM14(23:25),'(I3)' ) NSOLUX+NSPRX
            DO 436 N = 1,NFBN

              READ(IRS,FORM14) T(2,N),PG(2,N),PL(2,N),PN(2,N),POSM(2,N),
     &PSO(2,N),
     &        PVA(2,N),SG(2,N),SL(2,N),SN(2,N),YLS(2,N),TMS(2,N),
     &        (TMC(IGC,2,N),IGC=1,NGC+2),(ZMC(IGC,2,N),IGC=1,NGC+2),
     &        PCMP(N),TCMP(N),NPHAZ(2,N),(C(N,M),M=1,NSOLUX),
     &        (SP_C(N,NSP),NSP=1,NSPRX)







  436       CONTINUE
          ENDIF
        ENDIF
!
!---    Geomechanics data  ---
!
        NFEN_GMX = 0
        READ(IRS,'(A)',END=900) CHDUM
        IF( INDEX(CHDUM(1:),'Geomechanics Model Data').NE.0 ) THEN
          READ(IRS,'(I9)') NFEN_GMX
          DO NFEN = 1,NFEN_GMX
            READ(IRS,'(3(1PE22.15,1X))') U_GM(2,NFEN),V_GM(2,NFEN),
     &        W_GM(2,NFEN)
          ENDDO
        ELSE
          BACKSPACE(IRS)
        ENDIF
!
!---    Check for compatibility in geomechanics data  ---
!
        IF( ISLC(50).NE.0 ) THEN
!
!---      Restart file does not contain geomechanical data and current
!         simulation does include geomechanics, issue warning  ---
!
          IF( NFEN_GMX.EQ.0 ) THEN
            INDX = 24
            CHMSG = 'Resart File Does Not Contain Geomechanical Data'
            IMSG = NFEN_GMX
            CALL WRMSGS( INDX )
            ISLC(50) = -ISLC(50)
          ENDIF
        ELSE
!
!---      Restart file contains geomechanical data and current
!         simulation does not include geomechanics, issue warning  ---
!
          IF( NFEN_GMX.NE.0 ) THEN
            INDX = 24
            CHMSG = 'Resart File Contains Geomechanical Data: NFEN_GM'
            IMSG = NFEN_GMX
            CALL WRMSGS( INDX )
          ENDIF
        ENDIF
!
!---    Coupled-well data  ---
!
        N_CWX = 0
        READ(IRS,'(A)',END=900) CHDUM
        IF( INDEX(CHDUM(1:),'Coupled-Well Model Data').NE.0 .AND.
     &    N_CW.GT.0 ) THEN
          READ(IRS,'(I6)') N_CWX
          DO 760 NCW = 1,N_CWX
            READ(IRS,'(1PE22.15)') P_CW(2,NCW)
  760     CONTINUE
        ENDIF
  900   CONTINUE
!
!---    Check for compatibility in number of coupled wells  ---
!
        IF( N_CW.GT.0 ) THEN
!
!---      Current simulation has more coupled wells than restart file,
!         issue warning  ---
!
          IF( N_CWX.LT.N_CW ) THEN
            INDX = 24
            CHMSG = 'Number of Coupled Wells > ' // 
     &        'Number of Coupled Wells in Restart File'
            IMSG = N_CWX
            CALL WRMSGS( INDX )
!
!---      Current simulation has fewer coupled wells than restart file,
!         issue error  ---
!
          ELSEIF( N_CWX.LT.N_CW ) THEN
            INDX = 7
            CHMSG = 'Number of Coupled Wells < ' // 
     &        'Number of Coupled Wells in Restart File'
            IMSG = N_CWX
            CALL WRMSGS( INDX )
          ENDIF
        ENDIF
!
!---  Close the restart file  ---
!
        CLOSE( UNIT=IRS )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDRST group  ---
!
      RETURN
      END

