!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCBZ( ISLV,MU,ML,MK )
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
!     Zero the Jacobian matrix elements and solution vector.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle's Pacific Northwest Division, 1996.
!     Last Modified by MD White on September 5, 1996.




!     $Id: jcbz.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE WELL_CL
      USE SOLTN
      USE JACOB
      USE GRID
      USE GEOMECH
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
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/JCBZ'
      IF( INDEX(SVN_ID(121)(1:1),'$').EQ.0 ) SVN_ID(121) =
     & '$Id: jcbz.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      ICNV = 3
      NSPILL = LSPILL
!
!---  Number of unknowns equals number of active nodes x number of
!     coupled equations + number of coupled wells  ---
!
      IF( N_CW.GT.0 ) THEN
        NUKX = ISLV*(NFBN-NRFN-NXP) + N_CW
!
!---  Number of unknowns equals number of number of coupled equations
!     x (number of active nodes + number of wells +
!     number of surface nodes)
!
      ELSEIF( LSPILL.GT.0 ) THEN
        NUKX = ISLV*(NFBN-NRFN-NXP+NWLN+IJFLD)
!
!---  Number of unknowns equals number of number of coupled equations
!     x (number of active nodes + number of wells)
!
      ELSE
        NUKX = ISLV*(NFBN-NRFN-NXP+NWLN)
      ENDIF
!
!---  Dual-porosity option  ---
!
      IF( ISLC(11).EQ.1 ) NUKX = NUKX + ISLV*(NFBN-NRFN-NXP)
!
!---  Banded solver  ---
!
      IF( ILES.EQ.1 ) THEN
        NCOL = NUKX
        NROW = 2*MU + ML + 1
        DO 110 MCOL = 1,NCOL
          DO 100 MROW = 1,NROW
            ALU(MROW,MCOL) = 0.D+0
  100     CONTINUE
          BLU(MCOL) = 0.D+0
          ILU(MCOL) = MCOL
  110   CONTINUE
!
!---  SPLib Solver  ---
!
      ELSEIF( ILES.EQ.3 ) THEN
        NROW = NUKX
!$OMP PARALLEL DO
!$OMP& DEFAULT(NONE)
!$OMP& SHARED(DLU)
!$OMP& FIRSTPRIVATE(MK)
!$OMP& PRIVATE(MROW)
        DO 300 MROW = 1,MK
          DLU(MROW) = 0.D+0
  300   CONTINUE
!$OMP END PARALLEL DO
!$OMP PARALLEL DO
!$OMP& DEFAULT(NONE)
!$OMP& SHARED(BLU)
!$OMP& FIRSTPRIVATE(NROW)
!$OMP& PRIVATE(MROW)
        DO 310 MROW = 1,NROW
          BLU(MROW) = 0.D+0
  310   CONTINUE
!$OMP END PARALLEL DO

!
!---  Unknown Solver  ---
!
      ELSE






        INDX = 3
        CHMSG = 'Unknown Linear Equation Solver'
        CALL WRMSGS( INDX )
      ENDIF
      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCBZ group  ---
!
      RETURN
      END

