!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SHDP( U,V,W,DPL,DPT,DPX )
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
!     Subroutine computes hydrodynamic dispersion coefficients for
!     the aqueous and gas phases from phase velocities and user-
!     specified dispersivities.
!
!----------------------Authors-----------------------------------------!
!
!     Written by ML Rockhold, Battelle, PNL, March 1993.
!     Last Modified by MD White, Battelle, PNL, October 11, 1993.
!     $Id: shdp.F 1080 2017-03-14 16:22:02Z d3c002 $
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
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/SHDP'
      IF( INDEX(SVN_ID(191)(1:1),'$').EQ.0 ) SVN_ID(191) =
     & '$Id: shdp.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Longitudinal velocities  ---
!
      USQ = U*U
!
!---  Transverse velocities  ---
!
      VSQ = V*V
      WSQ = W*W
      XI = SQRT( USQ + VSQ + WSQ )
!
!---  Hydraulic dispersion coefficient  ---
!
      DPX = (DPL*USQ + DPT*(VSQ+WSQ))/(XI+SMALL)
!
!---  End of SHDP group  ---
!
      ISUB_LOG = ISUB_LOG-1
      RETURN
      END




