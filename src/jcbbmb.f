!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCBBMB( RS,N,MEQ )
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
!     Modify the Jacobian matrix for boundary conditions
!     (banded matrix solver).
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle's Pacific Northwest Division, 1996.
!     Last Modified by MD White on September 5, 1996.




!     $Id: jcbbmb.F 1080 2017-03-14 16:22:02Z d3c002 $
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE WELL_CL
      USE SOLTN
      USE JACOB
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RS(LUK+1)








!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/JCBBMB'
      IF( INDEX(SVN_ID(80)(1:1),'$').EQ.0 ) SVN_ID(80) =
     & '$Id: jcbbmb.F 1080 2017-03-14 16:22:02Z d3c002 $' 
      NMD = IXP(N)
      MP = IM(MEQ,NMD)
      MA = 0
!
!---  Coupled well  ---
!
      IF( IXW(N).NE.0 ) MA = MA + ISVC
!
!---  Banded solver  ---
!
      IF( ILES.EQ.1 ) THEN
        DO 110 M = 1,ISVC
          MCOL = IM(M,NMD)
          MROW = MP-MCOL+MDC
          ALU(MROW,MCOL) = ALU(MROW,MCOL) + (RS(M+1)-RS(1))/DNR(M,N)
  110   CONTINUE
!
!---  SPLib or Lis solver  ---
!
      ELSEIF( ILES.EQ.3 .OR. ILES.EQ.4 ) THEN
        DO 130 M = 1,ISVC
          MCOL = KLU(MP,M+MA)
          DLU(MCOL) = DLU(MCOL) + (RS(M+1)-RS(1))/DNR(M,N)
  130   CONTINUE
!#ifdef lis
!!
!!---  Lis solver  ---
!!
!      ELSEIF( ILES.EQ.4 ) THEN
!        IROWX = IM(MEQ,NMD)
!        DO 140 M = 1,ISVC
!          VALUEX = (RS(M+1)-RS(1))/DNR(M,N)
!          ICOLX = IM(M,NMD)
!          CALL lis_matrix_set_value(LIS_ADD_VALUE,IROWX,ICOLX,
!     &      VALUEX,FMAT,IERR)
!  140   CONTINUE
!        VALUEX = -RS(1)
!        CALL lis_vector_set_value(LIS_ADD_VALUE,IROWX,
!     &    VALUEX,FRHS_VEC,IERR)
!#endif

!
!---  Unknown solver  ---
!
      ELSE






        INDX = 3
        CHMSG = 'Unknown Linear Equation Solver'
        CALL WRMSGS( INDX )
      ENDIF
      BLU(MP) = BLU(MP) - RS(1)
      RSDL(MEQ,N) = BLU(MP)
      ISUB_LOG = ISUB_LOG-1
!
!---  End of JCBBMB group
!
      RETURN
      END

