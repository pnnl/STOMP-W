!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE WRMSGS( INDX )
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
!     Write warnings and error messages to the screen and output file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, September, 1994.
!     Last Modified by MD White, Battelle, PNL, September 4, 1998.




!     $Id: wrmsg.F 1080 2017-03-14 16:22:02Z d3c002 $
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
      EXTERNAL ICOUNT
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*9 FORM1
      CHARACTER*9 FORM2
      CHARACTER*17 FORM3
      CHARACTER*19 FORM4
      CHARACTER*9 FORM5
      CHARACTER*9 FORM6
      CHARACTER*19 FORM14
      CHARACTER*19 FORM17
      CHARACTER*19 FORM21
      CHARACTER*9 FORM25
!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM1,FORM2,FORM3,FORM4,FORM5,FORM6
      DATA FORM1 / '(/,3A,I4)' /
      DATA FORM2 / '(/,2A,I6)' /
      DATA FORM3 / '(/,2A,I6,1PE11.4)' /
      DATA FORM4 / '(/,3A,I6,A,1PE11.4)' /
      DATA FORM5 / '(/,3A,I6)' /
      DATA FORM6 / '(/,2A,I6)' /
      DATA FORM14 / '(/,A,I6,2A,1PE11.4)' /
      DATA FORM17 / '(/,A,I6,2A,1PE11.4)' /
      DATA FORM21 / '(/,A,I6,2A,1PE11.4)' /
      DATA FORM25 / '(/,3A,I6)' /
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/WRMSGS'
      IF( INDEX(SVN_ID(276)(1:1),'$').EQ.0 ) SVN_ID(276) =
     & '$Id: wrmsg.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      NCH = INDEX( CHMSG(1:),'  ' )-1
      ICSN = INDEX( SUB_LOG(1)(1:),'  ' )-1
      SUBNM(1:ICSN) = SUB_LOG(1)(1:ICSN)
      DO 10 I = 2,ISUB_LOG
        ICSNX = INDEX( SUB_LOG(I)(1:),'  ' )-1
        SUBNM(ICSN+1:ICSN+ICSNX) = SUB_LOG(I)(1:ICSNX)
        ICSN = ICSN+ICSNX
   10 CONTINUE
      IF( INDX.EQ.0 ) THEN
        WRITE(ISC,'(/,A)') CHMSG(:NCH)
        WRITE(IWR,'(/,A)') CHMSG(:NCH)
      ELSEIF( INDX.EQ.1 ) THEN
        WRITE(ISC,'(2A)') 'NOTE: ',CHMSG(:NCH)
        WRITE(IWR,'(2A)') 'NOTE: ',CHMSG(:NCH)
      ELSEIF( INDX.EQ.2 ) THEN
        WRITE(ISC,'(/,2A)') 'INPUT WARNING: ',CHMSG(:NCH)
        WRITE(IWR,'(/,2A)') 'INPUT WARNING: ',CHMSG(:NCH)
        WRITE(ISC,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        WRITE(IWR,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
      ELSEIF( INDX.EQ.3 ) THEN
        WRITE(ISC,'(/,2A)') 'EXECUTION ERROR: ',CHMSG(:NCH)
        WRITE(IWR,'(/,2A)') 'EXECUTION ERROR: ',CHMSG(:NCH)
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        CALL WRCVS



        STOP

      ELSEIF( INDX.EQ.4 ) THEN
        WRITE(ISC,'(/,2A)') 'INPUT ERROR: ',CHMSG(:NCH)
        WRITE(IWR,'(/,2A)') 'INPUT ERROR: ',CHMSG(:NCH)
        WRITE(ISC,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        WRITE(IWR,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        CALL WRCVS



        STOP

      ELSEIF( INDX.EQ.5 ) THEN
        WRITE(ISC,'(/,2A)') 'PARAMETER ERROR: ',CHMSG(:NCH)
        WRITE(IWR,'(/,2A)') 'PARAMETER ERROR: ',CHMSG(:NCH)
        WRITE(ISC,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        WRITE(IWR,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        CALL WRCVS



        STOP

      ELSEIF( INDX.EQ.6 ) THEN
        WRITE(ISC,'(/,2A)') 'PARAMETER ERROR: ',CHMSG(:NCH)
        WRITE(IWR,'(/,2A)') 'PARAMETER ERROR: ',CHMSG(:NCH)
        WRITE(ISC,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        WRITE(IWR,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
      ELSEIF( INDX.EQ.7 ) THEN
        WRITE(FORM1(8:8),'(I1)') ICOUNT( IMSG )
        WRITE(ISC,FORM1) 'INPUT ERROR: ',CHMSG(:NCH),': ',IMSG
        WRITE(IWR,FORM1) 'INPUT ERROR: ',CHMSG(:NCH),': ',IMSG
        WRITE(ISC,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        WRITE(IWR,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        CALL WRCVS



        STOP

      ELSEIF( INDX.EQ.8 ) THEN
        WRITE(ISC,'(/,2A)') 'PARAMETER ERROR: ',CHMSG(:NCH)
        WRITE(IWR,'(/,2A)') 'PARAMETER ERROR: ',CHMSG(:NCH)
        WRITE(ISC,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        WRITE(IWR,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        CALL WRCVS



        STOP

      ELSEIF( INDX.EQ.9 ) THEN
        WRITE(ISC,'(/,3A,1PE11.4)') 'INPUT ERROR: ',CHMSG(:NCH),': ',
     &    RLMSG
        WRITE(IWR,'(/,3A,1PE11.4)') 'INPUT ERROR: ',CHMSG(:NCH),': ',
     &    RLMSG
        WRITE(ISC,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        WRITE(IWR,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        CALL WRCVS



        STOP

      ELSEIF( INDX.EQ.10 ) THEN
        WRITE(ISC,'(/,3A,1PE11.4)') 'STATE CONDITION ERROR: ',
     &    CHMSG(:NCH),': ',RLMSG
        WRITE(IWR,'(/,3A,1PE11.4)') 'STATE CONDITION ERROR: ',
     &    CHMSG(:NCH),': ',RLMSG
      ELSEIF( INDX.EQ.11 ) THEN
        WRITE(ISC,'(/,3A,1PE11.4)') 'STATE CONDITION ERROR: ',
     &    CHMSG(:NCH),': ',RLMSG
        WRITE(IWR,'(/,3A,1PE11.4)') 'STATE CONDITION ERROR: ',
     &    CHMSG(:NCH),': ',RLMSG
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        CALL WRCVS



        STOP

      ELSEIF( INDX.EQ.12 ) THEN
        WRITE(FORM2(8:8),'(I1)') ICOUNT( IMSG )
        WRITE(ISC,FORM2) 'EXECUTION ERROR: ',CHMSG(:NCH),IMSG
        WRITE(IWR,FORM2) 'EXECUTION ERROR: ',CHMSG(:NCH),IMSG
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        CALL WRCVS



        STOP

      ELSEIF( INDX.EQ.13 ) THEN
        IF( N_DB.GT.0 ) THEN
          WRITE(FORM14(7:7),'(I1)') ICOUNT( N_DB )
          WRITE(ISC,FORM14) 'EXECUTION WARNING: NODE = ',
     &      N_DB,': ',CHMSG(:NCH),RLMSG
          WRITE(IWR,FORM14) 'EXECUTION WARNING: NODE = ',
     &      N_DB,': ',CHMSG(:NCH),RLMSG
        ELSE
          N_DBX = ABS(N_DB)
          WRITE(FORM14(7:7),'(I1)') ICOUNT( N_DBX )
          WRITE(ISC,FORM14) 'EXECUTION WARNING: BOUNDARY = ',
     &      N_DBX,': ',CHMSG(:NCH),RLMSG
          WRITE(IWR,FORM14) 'EXECUTION WARNING: BOUNDARY = ',
     &      N_DBX,': ',CHMSG(:NCH),RLMSG
        ENDIF
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
      ELSEIF( INDX.EQ.14 ) THEN
        IF( N_DB.GT.0 ) THEN
          WRITE(FORM14(7:7),'(I1)') ICOUNT( N_DB )
          WRITE(ISC,FORM14) 'EXECUTION ERROR: NODE = ',
     &      N_DB,': ',CHMSG(:NCH),RLMSG
          WRITE(IWR,FORM14) 'EXECUTION ERROR: NODE = ',
     &      N_DB,': ',CHMSG(:NCH),RLMSG
        ELSE
          N_DBX = ABS(N_DB)
          WRITE(FORM14(7:7),'(I1)') ICOUNT( N_DBX )
          WRITE(ISC,FORM14) 'EXECUTION ERROR: BOUNDARY = ',
     &      N_DBX,': ',CHMSG(:NCH),RLMSG
          WRITE(IWR,FORM14) 'EXECUTION ERROR: BOUNDARY = ',
     &      N_DBX,': ',CHMSG(:NCH),RLMSG
        ENDIF
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        CALL WRCVS



        STOP

      ELSEIF( INDX.EQ.15 ) THEN
        WRITE(FORM3(8:8),'(I1)') ICOUNT( IMSG )
        WRITE(ISC,FORM3) 'EXECUTION ERROR: ',CHMSG(:NCH),IMSG,RLMSG
        WRITE(IWR,FORM3) 'EXECUTION ERROR: ',CHMSG(:NCH),IMSG,RLMSG
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        CALL WRCVS



        STOP

      ELSEIF( INDX.EQ.16 ) THEN
        WRITE(FORM4(8:8),'(I1)') ICOUNT( IMSG )
        WRITE(ISC,FORM4) 'INPUT ERROR: ',CHMSG(:NCH),
     &    ': ',IMSG,': ',RLMSG
        WRITE(IWR,FORM4) 'INPUT ERROR: ',CHMSG(:NCH),
     &    ': ',IMSG,': ',RLMSG
        WRITE(ISC,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        WRITE(IWR,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        CALL WRCVS



        STOP

      ELSEIF( INDX.EQ.17 ) THEN
        IF( N_DB.GT.0 ) THEN
          WRITE(FORM17(7:7),'(I1)') ICOUNT( N_DB )
          WRITE(ISC,FORM17) 'STATE CONDIITON ERROR: NODE = ',
     &      N_DB,': ',CHMSG(:NCH),RLMSG
          WRITE(IWR,FORM17) 'STATE CONDITION ERROR: NODE = ',
     &      N_DB,': ',CHMSG(:NCH),RLMSG
        ELSE
          N_DBX = ABS(N_DB)
          WRITE(FORM17(7:7),'(I1)') ICOUNT( N_DBX )
          WRITE(ISC,FORM17) 'STATE CONDIITON ERROR: BOUNDARY = ',
     &      N_DBX,': ',CHMSG(:NCH),RLMSG
          WRITE(IWR,FORM17) 'STATE CONDITION ERROR: BOUNDARY = ',
     &      N_DBX,': ',CHMSG(:NCH),RLMSG
        ENDIF
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        CALL WRCVS



        STOP

      ELSEIF( INDX.EQ.18 ) THEN
        WRITE(ISC,'(/,2A)') 'INPUT ERROR: ',CHMSG(:NCH)
        WRITE(IWR,'(/,2A)') 'INPUT ERROR: ',CHMSG(:NCH)
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        CALL WRCVS



        STOP

      ELSEIF( INDX.EQ.19 ) THEN
        WRITE(ISC,'(/,2A,1PE11.4)')'EXECUTION ERROR: ',CHMSG(:NCH),RLMSG
        WRITE(IWR,'(/,2A,1PE11.4)')'EXECUTION ERROR: ',CHMSG(:NCH),RLMSG
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        CALL WRCVS



        STOP

      ELSEIF( INDX.EQ.20 ) THEN
        WRITE(FORM6(8:8),'(I1)') ICOUNT( IMSG )
        WRITE(ISC,FORM6) 'EXECUTION ERROR: ',CHMSG(:NCH),IMSG
        WRITE(IWR,FORM6) 'EXECUTION ERROR: ',CHMSG(:NCH),IMSG
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
      ELSEIF( INDX.EQ.21 ) THEN
        IF( N_DB.GT.0 ) THEN
          WRITE(FORM21(7:7),'(I1)') ICOUNT( N_DB )
          WRITE(ISC,FORM21) 'STATE CONDIITON ERROR: NODE = ',
     &      N_DB,': ',CHMSG(:NCH),RLMSG
          WRITE(IWR,FORM21) 'STATE CONDITION ERROR: NODE = ',
     &      N_DB,': ',CHMSG(:NCH),RLMSG
        ELSE
          N_DBX = ABS(N_DB)
          WRITE(FORM21(7:7),'(I1)') ICOUNT( N_DBX )
          WRITE(ISC,FORM21) 'STATE CONDIITON ERROR: BOUNDARY = ',
     &      N_DBX,': ',CHMSG(:NCH),RLMSG
          WRITE(IWR,FORM21) 'STATE CONDITION ERROR: BOUNDARY = ',
     &      N_DBX,': ',CHMSG(:NCH),RLMSG
        ENDIF
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
      ELSEIF( INDX.EQ.22 ) THEN
        WRITE(ISC,'(/,2A)') 'PARAMETER ERROR: ',CHMSG(:NCH)
        WRITE(IWR,'(/,2A)') 'PARAMETER ERROR: ',CHMSG(:NCH)
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        CALL WRCVS



        STOP

      ELSEIF( INDX.EQ.23 ) THEN
        WRITE(ISC,'(/,2A)') 'OUTPUT ERROR: ',CHMSG(:NCH)
        WRITE(IWR,'(/,2A)') 'OUTPUT ERROR: ',CHMSG(:NCH)
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        CALL WRCVS



        STOP

      ELSEIF( INDX.EQ.24 ) THEN
        WRITE(FORM1(8:8),'(I1)') ICOUNT( IMSG )
        WRITE(ISC,FORM1) 'INPUT WARNING: ',CHMSG(:NCH),': ',IMSG
        WRITE(IWR,FORM1) 'INPUT WARNING: ',CHMSG(:NCH),': ',IMSG
        WRITE(ISC,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        WRITE(IWR,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
      ELSEIF( INDX.EQ.25 ) THEN
        IF( N_DB.GT.0 ) THEN
          WRITE(FORM25(8:8),'(I1)') ICOUNT( N_DB )
          WRITE(ISC,FORM25) 'EXECUTION NOTE: ',CHMSG(:NCH),
     &      ': NODE =',N_DB
          WRITE(IWR,FORM25) 'EXECUTION NOTE: ',CHMSG(:NCH),
     &      ': NODE =',N_DB
        ELSE
          N_DBX = ABS(N_DB)
          WRITE(FORM25(8:8),'(I1)') ICOUNT( N_DBX )
          WRITE(ISC,FORM25) 'EXECUTION NOTE: ',CHMSG(:NCH),
     &      ': BOUNDARY =',N_DBX
          WRITE(IWR,FORM25) 'EXECUTION NOTE: ',CHMSG(:NCH),
     &      ': BOUNDARY =',N_DBX
        ENDIF
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
      ELSEIF( INDX.EQ.26 ) THEN
        WRITE(ISC,FORM6) 'EXECUTION WARNING: ',CHMSG(:NCH)
        WRITE(IWR,FORM6) 'EXECUTION WARNING: ',CHMSG(:NCH)
        WRITE(ISC,'(2A,/)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A,/)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
      ELSEIF( INDX.EQ.27 ) THEN
        WRITE(ISC,'(/,A)') '                      ---  DEMO  ---'
        WRITE(ISC,'(/,2A)') 'Number of Active Nodes > ' // 
     &    'Demonstration Limit of 1000 for ',CHMSG(:NCH)
        WRITE(ISC,'(/,2A)') 'A full license can be requested at ' //
     &    'http://stomp.pnnl.gov/licensing.stm'
        WRITE(ISC,'(/,A)') '---  End of STOMP Simulation  ---'
        WRITE(IWR,'(/,A)') '                      ---  DEMO  ---'
        WRITE(IWR,'(/,2A)') 'Number of Active Nodes > ' // 
     &    'Demonstration Limit of 1000 for ',CHMSG(:NCH)
        WRITE(IWR,'(/,2A)') 'A full license can be requested at ' //
     &    'http://stomp.pnnl.gov/licensing.stm'
        WRITE(IWR,'(/,A)') '---  End of STOMP Simulation  ---'
        STOP
      ELSEIF( INDX.EQ.28 ) THEN
        IF( N_DB.GT.0 ) THEN
          WRITE(FORM14(7:7),'(I1)') ICOUNT( N_DB )
          WRITE(ISC,FORM14) 'EXECUTION ERROR: NODE = ',
     &      N_DB,': ',CHMSG(:NCH),RLMSG
          WRITE(IWR,FORM14) 'EXECUTION ERROR: NODE = ',
     &      N_DB,': ',CHMSG(:NCH),RLMSG
        ELSE
          N_DBX = ABS(N_DB)
          WRITE(FORM14(7:7),'(I1)') ICOUNT( N_DBX )
          WRITE(ISC,FORM14) 'EXECUTION ERROR: BOUNDARY = ',
     &      N_DBX,': ',CHMSG(:NCH),RLMSG
          WRITE(IWR,FORM14) 'EXECUTION ERROR: BOUNDARY = ',
     &      N_DBX,': ',CHMSG(:NCH),RLMSG
        ENDIF
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
      ELSEIF( INDX.EQ.29 ) THEN
        IF( N_DB.GT.0 ) THEN
          WRITE(FORM14(7:7),'(I1)') ICOUNT( N_DB )
          WRITE(ISC,FORM14) 'INPUT ERROR: NODE = ',
     &      N_DB,': ',CHMSG(:NCH),RLMSG
          WRITE(IWR,FORM14) 'INPUT ERROR: NODE = ',
     &      N_DB,': ',CHMSG(:NCH),RLMSG
        ELSE
          N_DBX = ABS(N_DB)
          WRITE(FORM14(7:7),'(I1)') ICOUNT( N_DBX )
          WRITE(ISC,FORM14) 'INPUT ERROR: BOUNDARY = ',
     &      N_DBX,': ',CHMSG(:NCH),RLMSG
          WRITE(IWR,FORM14) 'INPUT ERROR: BOUNDARY = ',
     &      N_DBX,': ',CHMSG(:NCH),RLMSG
        ENDIF
        WRITE(ISC,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        WRITE(IWR,'(2A)') 'INPUT CARD: ',CARD(:ICD)
        WRITE(ISC,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        WRITE(IWR,'(2A)') 'CALLING SEQUENCE: ',SUBNM(:ICSN)
        CALL WRCVS



        STOP

      ENDIF

      ISUB_LOG = ISUB_LOG-1
!
!---  End of WRMSGS group
!
      RETURN
      END

