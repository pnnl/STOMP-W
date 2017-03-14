!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE BANNER
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
!     Display a banner on the screen and output file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, December 1992.
!     Last Modified by MD White, Battelle, PNL, December 31, 1992.
!     Last Modified by MD White, PNNL, 30 May 2002.




!     $Id: banner.F 1080 2017-03-14 16:22:02Z d3c002 $
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
!----------------------Type Declarations-------------------------------!
!
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ISUB_LOG = ISUB_LOG+1
      SUB_LOG(ISUB_LOG) = '/BANNER'
      IF( INDEX(SVN_ID(14)(1:1),'$').EQ.0 ) SVN_ID(14) =
     & '$Id: banner.F 1080 2017-03-14 16:22:02Z d3c002 $' 
!
!---  Output File  ---
!
      WRITE(IWR,'(A,//)')' Welcome to ...'
      WRITE(IWR,'(A)')   '                           STOMP'
      WRITE(IWR,'(A,//)')'        Subsurface Transport Over Multiple '
     &  // 'Phases'
      WRITE(IWR,'(A)')   ' This file was produced by STOMP, a '
     &  // 'numerical simulator'
      WRITE(IWR,'(A)')   ' developed by the Pacific Northwest '
     &  // 'Laboratory, with'
      WRITE(IWR,'(A)')   ' support from the VOC-Arid Integrated '
     &  // 'Demonstration Project,'
      WRITE(IWR,'(A)')   ' Office of Technology Development, U.S. '
     &  // 'Department of Energy.'
      WRITE(IWR,'(A)')   ' Results from this version of STOMP should '
     &  // 'not be used for'
      WRITE(IWR,'(A,/)') ' license related applications.'
      WRITE(IWR,'(A)')   ' For support:  Tel: 509.372.6070'
      WRITE(IWR,'(A,/)') '               E-mail:  mark.white@pnnl.gov'
      WRITE(IWR,'(A,/)') '                      ---  OUTPUT  ---'






!
!---  Screen Echo ---
!
      WRITE(ISC,'(A,//)')' Welcome to ...'
      WRITE(ISC,'(A)')   '                           STOMP'
      WRITE(ISC,'(A,//)')'        Subsurface Transport Over Multiple '
     &  // 'Phases'
      WRITE(ISC,'(A)')   ' This file was produced by STOMP, a'
     &  // ' numerical simulator'
      WRITE(ISC,'(A)')   ' developed by the Pacific Northwest '
     &  // 'Laboratory, with'
      WRITE(ISC,'(A)')   ' support from the VOC-Arid Integrated '
     &  // 'Demonstration Project,'
      WRITE(ISC,'(A)')   ' Office of Technology Development, U.S. '
     &  // 'Department of Energy.'
      WRITE(ISC,'(A)')   ' Results from this version of STOMP should '
     &  // 'not be used for'
      WRITE(ISC,'(A,/)') ' license related applications.'
      WRITE(ISC,'(A)')   ' For support:  Tel: 509.372.6070'
      WRITE(ISC,'(A,/)') '               E-mail:  mark.white@pnnl.gov'






      ISUB_LOG = ISUB_LOG-1
!
!---  End of BANNER group ---
!
      RETURN
      END

