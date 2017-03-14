!----------------------Function----------------------------------------!
!
      FUNCTION XGR( I,J,K )
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
!     Computes x coordinate at the centroid of the west surface.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, June 4, 1999.
!     Last Modified by MD White, Battelle, PNL, June 4, 1999.
!     $Id: xyz.F 1079 2017-03-14 16:14:53Z d3c002 $
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
      SUB_LOG(ISUB_LOG) = '/XGR'
      IF( INDEX(SVN_ID(279)(1:1),'$').EQ.0 ) SVN_ID(279) =
     & '$Id: xyz.F 1079 2017-03-14 16:14:53Z d3c002 $' 
        N = ND(I,J,K)
        XGR = 2.5D-1*(XE(1,N)+XE(3,N)+XE(5,N)+XE(7,N))
      ISUB_LOG = ISUB_LOG-1
!
!---  End of XGR group
!
      RETURN
      END

!----------------------Function----------------------------------------!
!
      FUNCTION YGR( I,J,K )
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
!     Computes y coordinate at the centroid of the south surface.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, June 4, 1999.
!     Last Modified by MD White, Battelle, PNL, June 4, 1999.
!     $Id: xyz.F 1079 2017-03-14 16:14:53Z d3c002 $
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
      SUB_LOG(ISUB_LOG) = '/YGR'
      IF( INDEX(SVN_ID(279)(1:1),'$').EQ.0 ) SVN_ID(279) =
     & '$Id: xyz.F 1079 2017-03-14 16:14:53Z d3c002 $' 
        N = ND(I,J,K)
        YGR = 2.5D-1*(YE(1,N)+YE(2,N)+YE(5,N)+YE(6,N))
      ISUB_LOG = ISUB_LOG-1
!
!---  End of YGR group
!
      RETURN
      END

!----------------------Function----------------------------------------!
!
      FUNCTION ZGR( I,J,K )
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
!     Computes x coordinate at the centroid of the bottom surface.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, June 4, 1999.
!     Last Modified by MD White, Battelle, PNL, June 4, 1999.
!     $Id: xyz.F 1079 2017-03-14 16:14:53Z d3c002 $
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
      SUB_LOG(ISUB_LOG) = '/ZGR'
      IF( INDEX(SVN_ID(279)(1:1),'$').EQ.0 ) SVN_ID(279) =
     & '$Id: xyz.F 1079 2017-03-14 16:14:53Z d3c002 $' 
        N = ND(I,J,K)
        ZGR = 2.5D-1*(ZE(1,N)+ZE(3,N)+ZE(2,N)+ZE(4,N))
      ISUB_LOG = ISUB_LOG-1
!
!---  End of ZGR group
!
      RETURN
      END


