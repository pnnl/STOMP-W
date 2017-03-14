!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINPL( CHDUM )
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
!     Read input line.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, November 10, 1999.
!     Last Modified by MD White, PNNL, November 10, 1999.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
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
      CHARACTER*(*) CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/RDINPL'
      IF( INDEX(SVN_ID(149)(1:1),'$').EQ.0 ) SVN_ID(149) =
     & '$Id: rdinpl.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Skip input lines that begin with '#' or '!'  ---
!
   10 READ(IRD,'(A)') CHDUM
      IF( CHDUM(1:1).EQ.'#' .OR. CHDUM(1:1).EQ.'!' ) GOTO 10
      ISUB_LOG = ISUB_LOG-1
!
!---  End of RDINPL group  ---
!
      RETURN
      END


