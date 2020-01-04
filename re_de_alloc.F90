!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Description :
!!    contains routines for re/de/allocation handling
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE re_de_alloc

   USE, INTRINSIC :: ISO_FORTRAN_ENV

   USE re_de_allocsp
   USE re_de_allocdp
   USE re_de_allocqp
   USE re_de_allocisp
   USE re_de_allocidp
   USE re_de_allociqp
   USE re_de_alloccsp
   USE re_de_alloccdp
   USE re_de_alloccqp
   USE re_de_alloclogic
   USE re_de_allocchar

   IMPLICIT NONE

   PRIVATE
   PUBLIC :: alloc
   PUBLIC :: realloc
   PUBLIC :: dealloc

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !! Description:
   !!    Routines to allocate pointers
   !! Variables:
   !!    dp_array: Array pointer to be allocated
   !!    insize: Integer or N-dimensional Integer array of the dimension sizes
   !!    instartidx: Integer or N-dimensional Integer array of the starting indices
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   INTERFACE alloc
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             alloc1ds_sp, &
             alloc1d_sp, &
#if __MAX_RANK > 2
             alloc2d_sp, &
#if __MAX_RANK > 3
             alloc3d_sp, &
#if __MAX_RANK > 4
             alloc4d_sp, &
#if __MAX_RANK > 5
             alloc5d_sp, &
#if __MAX_RANK > 6
             alloc6d_sp, &
#if __MAX_RANK > 7
             alloc7d_sp, &
#if __MAX_RANK > 8
             alloc8d_sp, &
#if __MAX_RANK > 9
             alloc9d_sp, &
#if __MAX_RANK > 10
             alloc10d_sp, &
#if __MAX_RANK > 11
             alloc11d_sp, &
#if __MAX_RANK > 12
             alloc12d_sp, &
#if __MAX_RANK > 13
             alloc13d_sp, &
#if __MAX_RANK > 14
             alloc14d_sp, &
#if __MAX_RANK > 15
             alloc15d_sp, &
#else
             alloc15d_sp
#endif
#else
             alloc14d_sp
#endif
#else
             alloc13d_sp
#endif
#else
             alloc12d_sp
#endif
#else
             alloc11d_sp
#endif
#else
             alloc10d_sp
#endif
#else
             alloc9d_sp
#endif
#else
             alloc8d_sp
#endif
#else
             alloc7d_sp
#endif
#else
             alloc6d_sp
#endif
#else
             alloc5d_sp
#endif
#else
             alloc4d_sp
#endif
#else
             alloc3d_sp
#endif
#else
             alloc2d_sp
#endif
#else
             alloc1ds_sp, &
             alloc1d_sp
#endif
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             alloc1ds_dp, &
             alloc1d_dp, &
#if __MAX_RANK > 2
             alloc2d_dp, &
#if __MAX_RANK > 3
             alloc3d_dp, &
#if __MAX_RANK > 4
             alloc4d_dp, &
#if __MAX_RANK > 5
             alloc5d_dp, &
#if __MAX_RANK > 6
             alloc6d_dp, &
#if __MAX_RANK > 7
             alloc7d_dp, &
#if __MAX_RANK > 8
             alloc8d_dp, &
#if __MAX_RANK > 9
             alloc9d_dp, &
#if __MAX_RANK > 10
             alloc10d_dp, &
#if __MAX_RANK > 11
             alloc11d_dp, &
#if __MAX_RANK > 12
             alloc12d_dp, &
#if __MAX_RANK > 13
             alloc13d_dp, &
#if __MAX_RANK > 14
             alloc14d_dp, &
#if __MAX_RANK > 15
             alloc15d_dp, &
#else
             alloc15d_dp
#endif
#else
             alloc14d_dp
#endif
#else
             alloc13d_dp
#endif
#else
             alloc12d_dp
#endif
#else
             alloc11d_dp
#endif
#else
             alloc10d_dp
#endif
#else
             alloc9d_dp
#endif
#else
             alloc8d_dp
#endif
#else
             alloc7d_dp
#endif
#else
             alloc6d_dp
#endif
#else
             alloc5d_dp
#endif
#else
             alloc4d_dp
#endif
#else
             alloc3d_dp
#endif
#else
             alloc2d_dp
#endif
#else
             alloc1ds_dp, &
             alloc1d_dp
#endif
#ifdef __HAS_QP
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             alloc1ds_qp, &
             alloc1d_qp, &
#if __MAX_RANK > 2
             alloc2d_qp, &
#if __MAX_RANK > 3
             alloc3d_qp, &
#if __MAX_RANK > 4
             alloc4d_qp, &
#if __MAX_RANK > 5
             alloc5d_qp, &
#if __MAX_RANK > 6
             alloc6d_qp, &
#if __MAX_RANK > 7
             alloc7d_qp, &
#if __MAX_RANK > 8
             alloc8d_qp, &
#if __MAX_RANK > 9
             alloc9d_qp, &
#if __MAX_RANK > 10
             alloc10d_qp, &
#if __MAX_RANK > 11
             alloc11d_qp, &
#if __MAX_RANK > 12
             alloc12d_qp, &
#if __MAX_RANK > 13
             alloc13d_qp, &
#if __MAX_RANK > 14
             alloc14d_qp, &
#if __MAX_RANK > 15
             alloc15d_qp, &
#else
             alloc15d_qp
#endif
#else
             alloc14d_qp
#endif
#else
             alloc13d_qp
#endif
#else
             alloc12d_qp
#endif
#else
             alloc11d_qp
#endif
#else
             alloc10d_qp
#endif
#else
             alloc9d_qp
#endif
#else
             alloc8d_qp
#endif
#else
             alloc7d_qp
#endif
#else
             alloc6d_qp
#endif
#else
             alloc5d_qp
#endif
#else
             alloc4d_qp
#endif
#else
             alloc3d_qp
#endif
#else
             alloc2d_qp
#endif
#else
             alloc1ds_qp, &
             alloc1d_qp
#endif
#endif
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             alloc1ds_isp, &
             alloc1d_isp, &
#if __MAX_RANK > 2
             alloc2d_isp, &
#if __MAX_RANK > 3
             alloc3d_isp, &
#if __MAX_RANK > 4
             alloc4d_isp, &
#if __MAX_RANK > 5
             alloc5d_isp, &
#if __MAX_RANK > 6
             alloc6d_isp, &
#if __MAX_RANK > 7
             alloc7d_isp, &
#if __MAX_RANK > 8
             alloc8d_isp, &
#if __MAX_RANK > 9
             alloc9d_isp, &
#if __MAX_RANK > 10
             alloc10d_isp, &
#if __MAX_RANK > 11
             alloc11d_isp, &
#if __MAX_RANK > 12
             alloc12d_isp, &
#if __MAX_RANK > 13
             alloc13d_isp, &
#if __MAX_RANK > 14
             alloc14d_isp, &
#if __MAX_RANK > 15
             alloc15d_isp, &
#else
             alloc15d_isp
#endif
#else
             alloc14d_isp
#endif
#else
             alloc13d_isp
#endif
#else
             alloc12d_isp
#endif
#else
             alloc11d_isp
#endif
#else
             alloc10d_isp
#endif
#else
             alloc9d_isp
#endif
#else
             alloc8d_isp
#endif
#else
             alloc7d_isp
#endif
#else
             alloc6d_isp
#endif
#else
             alloc5d_isp
#endif
#else
             alloc4d_isp
#endif
#else
             alloc3d_isp
#endif
#else
             alloc2d_isp
#endif
#else
             alloc1ds_isp, &
             alloc1d_isp
#endif
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             alloc1ds_idp, &
             alloc1d_idp, &
#if __MAX_RANK > 2
             alloc2d_idp, &
#if __MAX_RANK > 3
             alloc3d_idp, &
#if __MAX_RANK > 4
             alloc4d_idp, &
#if __MAX_RANK > 5
             alloc5d_idp, &
#if __MAX_RANK > 6
             alloc6d_idp, &
#if __MAX_RANK > 7
             alloc7d_idp, &
#if __MAX_RANK > 8
             alloc8d_idp, &
#if __MAX_RANK > 9
             alloc9d_idp, &
#if __MAX_RANK > 10
             alloc10d_idp, &
#if __MAX_RANK > 11
             alloc11d_idp, &
#if __MAX_RANK > 12
             alloc12d_idp, &
#if __MAX_RANK > 13
             alloc13d_idp, &
#if __MAX_RANK > 14
             alloc14d_idp, &
#if __MAX_RANK > 15
             alloc15d_idp, &
#else
             alloc15d_idp
#endif
#else
             alloc14d_idp
#endif
#else
             alloc13d_idp
#endif
#else
             alloc12d_idp
#endif
#else
             alloc11d_idp
#endif
#else
             alloc10d_idp
#endif
#else
             alloc9d_idp
#endif
#else
             alloc8d_idp
#endif
#else
             alloc7d_idp
#endif
#else
             alloc6d_idp
#endif
#else
             alloc5d_idp
#endif
#else
             alloc4d_idp
#endif
#else
             alloc3d_idp
#endif
#else
             alloc2d_idp
#endif
#else
             alloc1ds_idp, &
             alloc1d_idp
#endif
#ifdef __HAS_IQP
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             alloc1ds_iqp, &
             alloc1d_iqp, &
#if __MAX_RANK > 2
             alloc2d_iqp, &
#if __MAX_RANK > 3
             alloc3d_iqp, &
#if __MAX_RANK > 4
             alloc4d_iqp, &
#if __MAX_RANK > 5
             alloc5d_iqp, &
#if __MAX_RANK > 6
             alloc6d_iqp, &
#if __MAX_RANK > 7
             alloc7d_iqp, &
#if __MAX_RANK > 8
             alloc8d_iqp, &
#if __MAX_RANK > 9
             alloc9d_iqp, &
#if __MAX_RANK > 10
             alloc10d_iqp, &
#if __MAX_RANK > 11
             alloc11d_iqp, &
#if __MAX_RANK > 12
             alloc12d_iqp, &
#if __MAX_RANK > 13
             alloc13d_iqp, &
#if __MAX_RANK > 14
             alloc14d_iqp, &
#if __MAX_RANK > 15
             alloc15d_iqp, &
#else
             alloc15d_iqp
#endif
#else
             alloc14d_iqp
#endif
#else
             alloc13d_iqp
#endif
#else
             alloc12d_iqp
#endif
#else
             alloc11d_iqp
#endif
#else
             alloc10d_iqp
#endif
#else
             alloc9d_iqp
#endif
#else
             alloc8d_iqp
#endif
#else
             alloc7d_iqp
#endif
#else
             alloc6d_iqp
#endif
#else
             alloc5d_iqp
#endif
#else
             alloc4d_iqp
#endif
#else
             alloc3d_iqp
#endif
#else
             alloc2d_iqp
#endif
#else
             alloc1ds_iqp, &
             alloc1d_iqp
#endif
#endif
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             alloc1ds_csp, &
             alloc1d_csp, &
#if __MAX_RANK > 2
             alloc2d_csp, &
#if __MAX_RANK > 3
             alloc3d_csp, &
#if __MAX_RANK > 4
             alloc4d_csp, &
#if __MAX_RANK > 5
             alloc5d_csp, &
#if __MAX_RANK > 6
             alloc6d_csp, &
#if __MAX_RANK > 7
             alloc7d_csp, &
#if __MAX_RANK > 8
             alloc8d_csp, &
#if __MAX_RANK > 9
             alloc9d_csp, &
#if __MAX_RANK > 10
             alloc10d_csp, &
#if __MAX_RANK > 11
             alloc11d_csp, &
#if __MAX_RANK > 12
             alloc12d_csp, &
#if __MAX_RANK > 13
             alloc13d_csp, &
#if __MAX_RANK > 14
             alloc14d_csp, &
#if __MAX_RANK > 15
             alloc15d_csp, &
#else
             alloc15d_csp
#endif
#else
             alloc14d_csp
#endif
#else
             alloc13d_csp
#endif
#else
             alloc12d_csp
#endif
#else
             alloc11d_csp
#endif
#else
             alloc10d_csp
#endif
#else
             alloc9d_csp
#endif
#else
             alloc8d_csp
#endif
#else
             alloc7d_csp
#endif
#else
             alloc6d_csp
#endif
#else
             alloc5d_csp
#endif
#else
             alloc4d_csp
#endif
#else
             alloc3d_csp
#endif
#else
             alloc2d_csp
#endif
#else
             alloc1ds_csp, &
             alloc1d_csp
#endif
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             alloc1ds_cdp, &
             alloc1d_cdp, &
#if __MAX_RANK > 2
             alloc2d_cdp, &
#if __MAX_RANK > 3
             alloc3d_cdp, &
#if __MAX_RANK > 4
             alloc4d_cdp, &
#if __MAX_RANK > 5
             alloc5d_cdp, &
#if __MAX_RANK > 6
             alloc6d_cdp, &
#if __MAX_RANK > 7
             alloc7d_cdp, &
#if __MAX_RANK > 8
             alloc8d_cdp, &
#if __MAX_RANK > 9
             alloc9d_cdp, &
#if __MAX_RANK > 10
             alloc10d_cdp, &
#if __MAX_RANK > 11
             alloc11d_cdp, &
#if __MAX_RANK > 12
             alloc12d_cdp, &
#if __MAX_RANK > 13
             alloc13d_cdp, &
#if __MAX_RANK > 14
             alloc14d_cdp, &
#if __MAX_RANK > 15
             alloc15d_cdp, &
#else
             alloc15d_cdp
#endif
#else
             alloc14d_cdp
#endif
#else
             alloc13d_cdp
#endif
#else
             alloc12d_cdp
#endif
#else
             alloc11d_cdp
#endif
#else
             alloc10d_cdp
#endif
#else
             alloc9d_cdp
#endif
#else
             alloc8d_cdp
#endif
#else
             alloc7d_cdp
#endif
#else
             alloc6d_cdp
#endif
#else
             alloc5d_cdp
#endif
#else
             alloc4d_cdp
#endif
#else
             alloc3d_cdp
#endif
#else
             alloc2d_cdp
#endif
#else
             alloc1ds_cdp, &
             alloc1d_cdp
#endif
#ifdef __HAS_QP
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             alloc1ds_cqp, &
             alloc1d_cqp, &
#if __MAX_RANK > 2
             alloc2d_cqp, &
#if __MAX_RANK > 3
             alloc3d_cqp, &
#if __MAX_RANK > 4
             alloc4d_cqp, &
#if __MAX_RANK > 5
             alloc5d_cqp, &
#if __MAX_RANK > 6
             alloc6d_cqp, &
#if __MAX_RANK > 7
             alloc7d_cqp, &
#if __MAX_RANK > 8
             alloc8d_cqp, &
#if __MAX_RANK > 9
             alloc9d_cqp, &
#if __MAX_RANK > 10
             alloc10d_cqp, &
#if __MAX_RANK > 11
             alloc11d_cqp, &
#if __MAX_RANK > 12
             alloc12d_cqp, &
#if __MAX_RANK > 13
             alloc13d_cqp, &
#if __MAX_RANK > 14
             alloc14d_cqp, &
#if __MAX_RANK > 15
             alloc15d_cqp, &
#else
             alloc15d_cqp
#endif
#else
             alloc14d_cqp
#endif
#else
             alloc13d_cqp
#endif
#else
             alloc12d_cqp
#endif
#else
             alloc11d_cqp
#endif
#else
             alloc10d_cqp
#endif
#else
             alloc9d_cqp
#endif
#else
             alloc8d_cqp
#endif
#else
             alloc7d_cqp
#endif
#else
             alloc6d_cqp
#endif
#else
             alloc5d_cqp
#endif
#else
             alloc4d_cqp
#endif
#else
             alloc3d_cqp
#endif
#else
             alloc2d_cqp
#endif
#else
             alloc1ds_qp, &
             alloc1d_cqp
#endif
#endif
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             alloc1ds_logic, &
             alloc1d_logic, &
#if __MAX_RANK > 2
             alloc2d_logic, &
#if __MAX_RANK > 3
             alloc3d_logic, &
#if __MAX_RANK > 4
             alloc4d_logic, &
#if __MAX_RANK > 5
             alloc5d_logic, &
#if __MAX_RANK > 6
             alloc6d_logic, &
#if __MAX_RANK > 7
             alloc7d_logic, &
#if __MAX_RANK > 8
             alloc8d_logic, &
#if __MAX_RANK > 9
             alloc9d_logic, &
#if __MAX_RANK > 10
             alloc10d_logic, &
#if __MAX_RANK > 11
             alloc11d_logic, &
#if __MAX_RANK > 12
             alloc12d_logic, &
#if __MAX_RANK > 13
             alloc13d_logic, &
#if __MAX_RANK > 14
             alloc14d_logic, &
#if __MAX_RANK > 15
             alloc15d_logic, &
#else
             alloc15d_logic
#endif
#else
             alloc14d_logic
#endif
#else
             alloc13d_logic
#endif
#else
             alloc12d_logic
#endif
#else
             alloc11d_logic
#endif
#else
             alloc10d_logic
#endif
#else
             alloc9d_logic
#endif
#else
             alloc8d_logic
#endif
#else
             alloc7d_logic
#endif
#else
             alloc6d_logic
#endif
#else
             alloc5d_logic
#endif
#else
             alloc4d_logic
#endif
#else
             alloc3d_logic
#endif
#else
             alloc2d_logic
#endif
#else
             alloc1ds_logic &
             alloc1d_logic
#endif
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             alloc1ds_char, &
             alloc1d_char, &
#if __MAX_RANK > 2
             alloc2d_char, &
#if __MAX_RANK > 3
             alloc3d_char, &
#if __MAX_RANK > 4
             alloc4d_char, &
#if __MAX_RANK > 5
             alloc5d_char, &
#if __MAX_RANK > 6
             alloc6d_char, &
#if __MAX_RANK > 7
             alloc7d_char, &
#if __MAX_RANK > 8
             alloc8d_char, &
#if __MAX_RANK > 9
             alloc9d_char, &
#if __MAX_RANK > 10
             alloc10d_char, &
#if __MAX_RANK > 11
             alloc11d_char, &
#if __MAX_RANK > 12
             alloc12d_char, &
#if __MAX_RANK > 13
             alloc13d_char, &
#if __MAX_RANK > 14
             alloc14d_char, &
#if __MAX_RANK > 15
             alloc15d_char, &
#else
             alloc15d_char
#endif
#else
             alloc14d_char
#endif
#else
             alloc13d_char
#endif
#else
             alloc12d_char
#endif
#else
             alloc11d_char
#endif
#else
             alloc10d_char
#endif
#else
             alloc9d_char
#endif
#else
             alloc8d_char
#endif
#else
             alloc7d_char
#endif
#else
             alloc6d_char
#endif
#else
             alloc5d_char
#endif
#else
             alloc4d_char
#endif
#else
             alloc3d_char
#endif
#else
             alloc2d_char
#endif
#else
             alloc1ds_char &
             alloc1d_char
#endif
   END INTERFACE alloc

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !! Description:
   !!    Routines to reallocate pointers
   !! Variables:
   !!    dp_array: Array pointer to be allocated
   !!    insize: Integer or N-dimensional Integer array of the dimension sizes
   !!    instartidx: Integer or N-dimensional Integer array of the starting indices
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   INTERFACE realloc
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             realloc1ds_sp, &
             realloc1d_sp, &
#if __MAX_RANK > 2
             realloc2d_sp, &
#if __MAX_RANK > 3
             realloc3d_sp, &
#if __MAX_RANK > 4
             realloc4d_sp, &
#if __MAX_RANK > 5
             realloc5d_sp, &
#if __MAX_RANK > 6
             realloc6d_sp, &
#if __MAX_RANK > 7
             realloc7d_sp, &
#if __MAX_RANK > 8
             realloc8d_sp, &
#if __MAX_RANK > 9
             realloc9d_sp, &
#if __MAX_RANK > 10
             realloc10d_sp, &
#if __MAX_RANK > 11
             realloc11d_sp, &
#if __MAX_RANK > 12
             realloc12d_sp, &
#if __MAX_RANK > 13
             realloc13d_sp, &
#if __MAX_RANK > 14
             realloc14d_sp, &
#if __MAX_RANK > 15
             realloc15d_sp, &
#else
             realloc15d_sp
#endif
#else
             realloc14d_sp
#endif
#else
             realloc13d_sp
#endif
#else
             realloc12d_sp
#endif
#else
             realloc11d_sp
#endif
#else
             realloc10d_sp
#endif
#else
             realloc9d_sp
#endif
#else
             realloc8d_sp
#endif
#else
             realloc7d_sp
#endif
#else
             realloc6d_sp
#endif
#else
             realloc5d_sp
#endif
#else
             realloc4d_sp
#endif
#else
             realloc3d_sp
#endif
#else
             realloc2d_sp
#endif
#else
             realloc1ds_sp, &
             realloc1d_sp
#endif
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             realloc1ds_dp, &
             realloc1d_dp, &
#if __MAX_RANK > 2
             realloc2d_dp, &
#if __MAX_RANK > 3
             realloc3d_dp, &
#if __MAX_RANK > 4
             realloc4d_dp, &
#if __MAX_RANK > 5
             realloc5d_dp, &
#if __MAX_RANK > 6
             realloc6d_dp, &
#if __MAX_RANK > 7
             realloc7d_dp, &
#if __MAX_RANK > 8
             realloc8d_dp, &
#if __MAX_RANK > 9
             realloc9d_dp, &
#if __MAX_RANK > 10
             realloc10d_dp, &
#if __MAX_RANK > 11
             realloc11d_dp, &
#if __MAX_RANK > 12
             realloc12d_dp, &
#if __MAX_RANK > 13
             realloc13d_dp, &
#if __MAX_RANK > 14
             realloc14d_dp, &
#if __MAX_RANK > 15
             realloc15d_dp, &
#else
             realloc15d_dp
#endif
#else
             realloc14d_dp
#endif
#else
             realloc13d_dp
#endif
#else
             realloc12d_dp
#endif
#else
             realloc11d_dp
#endif
#else
             realloc10d_dp
#endif
#else
             realloc9d_dp
#endif
#else
             realloc8d_dp
#endif
#else
             realloc7d_dp
#endif
#else
             realloc6d_dp
#endif
#else
             realloc5d_dp
#endif
#else
             realloc4d_dp
#endif
#else
             realloc3d_dp
#endif
#else
             realloc2d_dp
#endif
#else
             realloc1ds_dp, &
             realloc1d_dp
#endif
#ifdef __HAS_QP
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             realloc1ds_qp, &
             realloc1d_qp, &
#if __MAX_RANK > 2
             realloc2d_qp, &
#if __MAX_RANK > 3
             realloc3d_qp, &
#if __MAX_RANK > 4
             realloc4d_qp, &
#if __MAX_RANK > 5
             realloc5d_qp, &
#if __MAX_RANK > 6
             realloc6d_qp, &
#if __MAX_RANK > 7
             realloc7d_qp, &
#if __MAX_RANK > 8
             realloc8d_qp, &
#if __MAX_RANK > 9
             realloc9d_qp, &
#if __MAX_RANK > 10
             realloc10d_qp, &
#if __MAX_RANK > 11
             realloc11d_qp, &
#if __MAX_RANK > 12
             realloc12d_qp, &
#if __MAX_RANK > 13
             realloc13d_qp, &
#if __MAX_RANK > 14
             realloc14d_qp, &
#if __MAX_RANK > 15
             realloc15d_qp, &
#else
             realloc15d_qp
#endif
#else
             realloc14d_qp
#endif
#else
             realloc13d_qp
#endif
#else
             realloc12d_qp
#endif
#else
             realloc11d_qp
#endif
#else
             realloc10d_qp
#endif
#else
             realloc9d_qp
#endif
#else
             realloc8d_qp
#endif
#else
             realloc7d_qp
#endif
#else
             realloc6d_qp
#endif
#else
             realloc5d_qp
#endif
#else
             realloc4d_qp
#endif
#else
             realloc3d_qp
#endif
#else
             realloc2d_qp
#endif
#else
             realloc1ds_qp, &
             realloc1d_qp
#endif
#endif
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             realloc1ds_isp, &
             realloc1d_isp, &
#if __MAX_RANK > 2
             realloc2d_isp, &
#if __MAX_RANK > 3
             realloc3d_isp, &
#if __MAX_RANK > 4
             realloc4d_isp, &
#if __MAX_RANK > 5
             realloc5d_isp, &
#if __MAX_RANK > 6
             realloc6d_isp, &
#if __MAX_RANK > 7
             realloc7d_isp, &
#if __MAX_RANK > 8
             realloc8d_isp, &
#if __MAX_RANK > 9
             realloc9d_isp, &
#if __MAX_RANK > 10
             realloc10d_isp, &
#if __MAX_RANK > 11
             realloc11d_isp, &
#if __MAX_RANK > 12
             realloc12d_isp, &
#if __MAX_RANK > 13
             realloc13d_isp, &
#if __MAX_RANK > 14
             realloc14d_isp, &
#if __MAX_RANK > 15
             realloc15d_isp, &
#else
             realloc15d_isp
#endif
#else
             realloc14d_isp
#endif
#else
             realloc13d_isp
#endif
#else
             realloc12d_isp
#endif
#else
             realloc11d_isp
#endif
#else
             realloc10d_isp
#endif
#else
             realloc9d_isp
#endif
#else
             realloc8d_isp
#endif
#else
             realloc7d_isp
#endif
#else
             realloc6d_isp
#endif
#else
             realloc5d_isp
#endif
#else
             realloc4d_isp
#endif
#else
             realloc3d_isp
#endif
#else
             realloc2d_isp
#endif
#else
             realloc1ds_isp, &
             realloc1d_isp
#endif
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             realloc1ds_idp, &
             realloc1d_idp, &
#if __MAX_RANK > 2
             realloc2d_idp, &
#if __MAX_RANK > 3
             realloc3d_idp, &
#if __MAX_RANK > 4
             realloc4d_idp, &
#if __MAX_RANK > 5
             realloc5d_idp, &
#if __MAX_RANK > 6
             realloc6d_idp, &
#if __MAX_RANK > 7
             realloc7d_idp, &
#if __MAX_RANK > 8
             realloc8d_idp, &
#if __MAX_RANK > 9
             realloc9d_idp, &
#if __MAX_RANK > 10
             realloc10d_idp, &
#if __MAX_RANK > 11
             realloc11d_idp, &
#if __MAX_RANK > 12
             realloc12d_idp, &
#if __MAX_RANK > 13
             realloc13d_idp, &
#if __MAX_RANK > 14
             realloc14d_idp, &
#if __MAX_RANK > 15
             realloc15d_idp, &
#else
             realloc15d_idp
#endif
#else
             realloc14d_idp
#endif
#else
             realloc13d_idp
#endif
#else
             realloc12d_idp
#endif
#else
             realloc11d_idp
#endif
#else
             realloc10d_idp
#endif
#else
             realloc9d_idp
#endif
#else
             realloc8d_idp
#endif
#else
             realloc7d_idp
#endif
#else
             realloc6d_idp
#endif
#else
             realloc5d_idp
#endif
#else
             realloc4d_idp
#endif
#else
             realloc3d_idp
#endif
#else
             realloc2d_idp
#endif
#else
             realloc1ds_idp, &
             realloc1d_idp
#endif
#ifdef __HAS_IQP
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             realloc1ds_iqp, &
             realloc1d_iqp, &
#if __MAX_RANK > 2
             realloc2d_iqp, &
#if __MAX_RANK > 3
             realloc3d_iqp, &
#if __MAX_RANK > 4
             realloc4d_iqp, &
#if __MAX_RANK > 5
             realloc5d_iqp, &
#if __MAX_RANK > 6
             realloc6d_iqp, &
#if __MAX_RANK > 7
             realloc7d_iqp, &
#if __MAX_RANK > 8
             realloc8d_iqp, &
#if __MAX_RANK > 9
             realloc9d_iqp, &
#if __MAX_RANK > 10
             realloc10d_iqp, &
#if __MAX_RANK > 11
             realloc11d_iqp, &
#if __MAX_RANK > 12
             realloc12d_iqp, &
#if __MAX_RANK > 13
             realloc13d_iqp, &
#if __MAX_RANK > 14
             realloc14d_iqp, &
#if __MAX_RANK > 15
             realloc15d_iqp, &
#else
             realloc15d_iqp
#endif
#else
             realloc14d_iqp
#endif
#else
             realloc13d_iqp
#endif
#else
             realloc12d_iqp
#endif
#else
             realloc11d_iqp
#endif
#else
             realloc10d_iqp
#endif
#else
             realloc9d_iqp
#endif
#else
             realloc8d_iqp
#endif
#else
             realloc7d_iqp
#endif
#else
             realloc6d_iqp
#endif
#else
             realloc5d_iqp
#endif
#else
             realloc4d_iqp
#endif
#else
             realloc3d_iqp
#endif
#else
             realloc2d_iqp
#endif
#else
             realloc1ds_iqp, &
             realloc1d_iqp
#endif
#endif
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             realloc1ds_csp, &
             realloc1d_csp, &
#if __MAX_RANK > 2
             realloc2d_csp, &
#if __MAX_RANK > 3
             realloc3d_csp, &
#if __MAX_RANK > 4
             realloc4d_csp, &
#if __MAX_RANK > 5
             realloc5d_csp, &
#if __MAX_RANK > 6
             realloc6d_csp, &
#if __MAX_RANK > 7
             realloc7d_csp, &
#if __MAX_RANK > 8
             realloc8d_csp, &
#if __MAX_RANK > 9
             realloc9d_csp, &
#if __MAX_RANK > 10
             realloc10d_csp, &
#if __MAX_RANK > 11
             realloc11d_csp, &
#if __MAX_RANK > 12
             realloc12d_csp, &
#if __MAX_RANK > 13
             realloc13d_csp, &
#if __MAX_RANK > 14
             realloc14d_csp, &
#if __MAX_RANK > 15
             realloc15d_csp, &
#else
             realloc15d_csp
#endif
#else
             realloc14d_csp
#endif
#else
             realloc13d_csp
#endif
#else
             realloc12d_csp
#endif
#else
             realloc11d_csp
#endif
#else
             realloc10d_csp
#endif
#else
             realloc9d_csp
#endif
#else
             realloc8d_csp
#endif
#else
             realloc7d_csp
#endif
#else
             realloc6d_csp
#endif
#else
             realloc5d_csp
#endif
#else
             realloc4d_csp
#endif
#else
             realloc3d_csp
#endif
#else
             realloc2d_csp
#endif
#else
             realloc1ds_csp, &
             realloc1d_csp
#endif
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             realloc1ds_cdp, &
             realloc1d_cdp, &
#if __MAX_RANK > 2
             realloc2d_cdp, &
#if __MAX_RANK > 3
             realloc3d_cdp, &
#if __MAX_RANK > 4
             realloc4d_cdp, &
#if __MAX_RANK > 5
             realloc5d_cdp, &
#if __MAX_RANK > 6
             realloc6d_cdp, &
#if __MAX_RANK > 7
             realloc7d_cdp, &
#if __MAX_RANK > 8
             realloc8d_cdp, &
#if __MAX_RANK > 9
             realloc9d_cdp, &
#if __MAX_RANK > 10
             realloc10d_cdp, &
#if __MAX_RANK > 11
             realloc11d_cdp, &
#if __MAX_RANK > 12
             realloc12d_cdp, &
#if __MAX_RANK > 13
             realloc13d_cdp, &
#if __MAX_RANK > 14
             realloc14d_cdp, &
#if __MAX_RANK > 15
             realloc15d_cdp, &
#else
             realloc15d_cdp
#endif
#else
             realloc14d_cdp
#endif
#else
             realloc13d_cdp
#endif
#else
             realloc12d_cdp
#endif
#else
             realloc11d_cdp
#endif
#else
             realloc10d_cdp
#endif
#else
             realloc9d_cdp
#endif
#else
             realloc8d_cdp
#endif
#else
             realloc7d_cdp
#endif
#else
             realloc6d_cdp
#endif
#else
             realloc5d_cdp
#endif
#else
             realloc4d_cdp
#endif
#else
             realloc3d_cdp
#endif
#else
             realloc2d_cdp
#endif
#else
             realloc1ds_cdp, &
             realloc1d_cdp
#endif
#ifdef __HAS_QP
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             realloc1ds_cqp, &
             realloc1d_cqp, &
#if __MAX_RANK > 2
             realloc2d_cqp, &
#if __MAX_RANK > 3
             realloc3d_cqp, &
#if __MAX_RANK > 4
             realloc4d_cqp, &
#if __MAX_RANK > 5
             realloc5d_cqp, &
#if __MAX_RANK > 6
             realloc6d_cqp, &
#if __MAX_RANK > 7
             realloc7d_cqp, &
#if __MAX_RANK > 8
             realloc8d_cqp, &
#if __MAX_RANK > 9
             realloc9d_cqp, &
#if __MAX_RANK > 10
             realloc10d_cqp, &
#if __MAX_RANK > 11
             realloc11d_cqp, &
#if __MAX_RANK > 12
             realloc12d_cqp, &
#if __MAX_RANK > 13
             realloc13d_cqp, &
#if __MAX_RANK > 14
             realloc14d_cqp, &
#if __MAX_RANK > 15
             realloc15d_cqp, &
#else
             realloc15d_cqp
#endif
#else
             realloc14d_cqp
#endif
#else
             realloc13d_cqp
#endif
#else
             realloc12d_cqp
#endif
#else
             realloc11d_cqp
#endif
#else
             realloc10d_cqp
#endif
#else
             realloc9d_cqp
#endif
#else
             realloc8d_cqp
#endif
#else
             realloc7d_cqp
#endif
#else
             realloc6d_cqp
#endif
#else
             realloc5d_cqp
#endif
#else
             realloc4d_cqp
#endif
#else
             realloc3d_cqp
#endif
#else
             realloc2d_cqp
#endif
#else
             realloc1ds_qp, &
             realloc1d_cqp
#endif
#endif
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             realloc1ds_logic, &
             realloc1d_logic, &
#if __MAX_RANK > 2
             realloc2d_logic, &
#if __MAX_RANK > 3
             realloc3d_logic, &
#if __MAX_RANK > 4
             realloc4d_logic, &
#if __MAX_RANK > 5
             realloc5d_logic, &
#if __MAX_RANK > 6
             realloc6d_logic, &
#if __MAX_RANK > 7
             realloc7d_logic, &
#if __MAX_RANK > 8
             realloc8d_logic, &
#if __MAX_RANK > 9
             realloc9d_logic, &
#if __MAX_RANK > 10
             realloc10d_logic, &
#if __MAX_RANK > 11
             realloc11d_logic, &
#if __MAX_RANK > 12
             realloc12d_logic, &
#if __MAX_RANK > 13
             realloc13d_logic, &
#if __MAX_RANK > 14
             realloc14d_logic, &
#if __MAX_RANK > 15
             realloc15d_logic, &
#else
             realloc15d_logic
#endif
#else
             realloc14d_logic
#endif
#else
             realloc13d_logic
#endif
#else
             realloc12d_logic
#endif
#else
             realloc11d_logic
#endif
#else
             realloc10d_logic
#endif
#else
             realloc9d_logic
#endif
#else
             realloc8d_logic
#endif
#else
             realloc7d_logic
#endif
#else
             realloc6d_logic
#endif
#else
             realloc5d_logic
#endif
#else
             realloc4d_logic
#endif
#else
             realloc3d_logic
#endif
#else
             realloc2d_logic
#endif
#else
             realloc1ds_logic &
             realloc1d_logic
#endif
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             realloc1ds_char, &
             realloc1d_char, &
#if __MAX_RANK > 2
             realloc2d_char, &
#if __MAX_RANK > 3
             realloc3d_char, &
#if __MAX_RANK > 4
             realloc4d_char, &
#if __MAX_RANK > 5
             realloc5d_char, &
#if __MAX_RANK > 6
             realloc6d_char, &
#if __MAX_RANK > 7
             realloc7d_char, &
#if __MAX_RANK > 8
             realloc8d_char, &
#if __MAX_RANK > 9
             realloc9d_char, &
#if __MAX_RANK > 10
             realloc10d_char, &
#if __MAX_RANK > 11
             realloc11d_char, &
#if __MAX_RANK > 12
             realloc12d_char, &
#if __MAX_RANK > 13
             realloc13d_char, &
#if __MAX_RANK > 14
             realloc14d_char, &
#if __MAX_RANK > 15
             realloc15d_char, &
#else
             realloc15d_char
#endif
#else
             realloc14d_char
#endif
#else
             realloc13d_char
#endif
#else
             realloc12d_char
#endif
#else
             realloc11d_char
#endif
#else
             realloc10d_char
#endif
#else
             realloc9d_char
#endif
#else
             realloc8d_char
#endif
#else
             realloc7d_char
#endif
#else
             realloc6d_char
#endif
#else
             realloc5d_char
#endif
#else
             realloc4d_char
#endif
#else
             realloc3d_char
#endif
#else
             realloc2d_char
#endif
#else
             realloc1ds_char &
             realloc1d_char
#endif
   END INTERFACE realloc

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !! Description:
   !!    Routines to deallocate and nullify pointers
   !! Variables:
   !!    dp_array: Array pointer to be deallocated and nullified
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   INTERFACE dealloc
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             dealloc1d_sp, &
#if __MAX_RANK > 2
             dealloc2d_sp, &
#if __MAX_RANK > 3
             dealloc3d_sp, &
#if __MAX_RANK > 4
             dealloc4d_sp, &
#if __MAX_RANK > 5
             dealloc5d_sp, &
#if __MAX_RANK > 6
             dealloc6d_sp, &
#if __MAX_RANK > 7
             dealloc7d_sp, &
#if __MAX_RANK > 8
             dealloc8d_sp, &
#if __MAX_RANK > 9
             dealloc9d_sp, &
#if __MAX_RANK > 10
             dealloc10d_sp, &
#if __MAX_RANK > 11
             dealloc11d_sp, &
#if __MAX_RANK > 12
             dealloc12d_sp, &
#if __MAX_RANK > 13
             dealloc13d_sp, &
#if __MAX_RANK > 14
             dealloc14d_sp, &
#if __MAX_RANK > 15
             dealloc15d_sp, &
#else
             dealloc15d_sp
#endif
#else
             dealloc14d_sp
#endif
#else
             dealloc13d_sp
#endif
#else
             dealloc12d_sp
#endif
#else
             dealloc11d_sp
#endif
#else
             dealloc10d_sp
#endif
#else
             dealloc9d_sp
#endif
#else
             dealloc8d_sp
#endif
#else
             dealloc7d_sp
#endif
#else
             dealloc6d_sp
#endif
#else
             dealloc5d_sp
#endif
#else
             dealloc4d_sp
#endif
#else
             dealloc3d_sp
#endif
#else
             dealloc2d_sp
#endif
#else
             dealloc1d_sp
#endif
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             dealloc1d_dp, &
#if __MAX_RANK > 2
             dealloc2d_dp, &
#if __MAX_RANK > 3
             dealloc3d_dp, &
#if __MAX_RANK > 4
             dealloc4d_dp, &
#if __MAX_RANK > 5
             dealloc5d_dp, &
#if __MAX_RANK > 6
             dealloc6d_dp, &
#if __MAX_RANK > 7
             dealloc7d_dp, &
#if __MAX_RANK > 8
             dealloc8d_dp, &
#if __MAX_RANK > 9
             dealloc9d_dp, &
#if __MAX_RANK > 10
             dealloc10d_dp, &
#if __MAX_RANK > 11
             dealloc11d_dp, &
#if __MAX_RANK > 12
             dealloc12d_dp, &
#if __MAX_RANK > 13
             dealloc13d_dp, &
#if __MAX_RANK > 14
             dealloc14d_dp, &
#if __MAX_RANK > 15
             dealloc15d_dp, &
#else
             dealloc15d_dp
#endif
#else
             dealloc14d_dp
#endif
#else
             dealloc13d_dp
#endif
#else
             dealloc12d_dp
#endif
#else
             dealloc11d_dp
#endif
#else
             dealloc10d_dp
#endif
#else
             dealloc9d_dp
#endif
#else
             dealloc8d_dp
#endif
#else
             dealloc7d_dp
#endif
#else
             dealloc6d_dp
#endif
#else
             dealloc5d_dp
#endif
#else
             dealloc4d_dp
#endif
#else
             dealloc3d_dp
#endif
#else
             dealloc2d_dp
#endif
#else
             dealloc1d_dp
#endif
#ifdef __HAS_QP
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             dealloc1d_qp, &
#if __MAX_RANK > 2
             dealloc2d_qp, &
#if __MAX_RANK > 3
             dealloc3d_qp, &
#if __MAX_RANK > 4
             dealloc4d_qp, &
#if __MAX_RANK > 5
             dealloc5d_qp, &
#if __MAX_RANK > 6
             dealloc6d_qp, &
#if __MAX_RANK > 7
             dealloc7d_qp, &
#if __MAX_RANK > 8
             dealloc8d_qp, &
#if __MAX_RANK > 9
             dealloc9d_qp, &
#if __MAX_RANK > 10
             dealloc10d_qp, &
#if __MAX_RANK > 11
             dealloc11d_qp, &
#if __MAX_RANK > 12
             dealloc12d_qp, &
#if __MAX_RANK > 13
             dealloc13d_qp, &
#if __MAX_RANK > 14
             dealloc14d_qp, &
#if __MAX_RANK > 15
             dealloc15d_qp, &
#else
             dealloc15d_qp
#endif
#else
             dealloc14d_qp
#endif
#else
             dealloc13d_qp
#endif
#else
             dealloc12d_qp
#endif
#else
             dealloc11d_qp
#endif
#else
             dealloc10d_qp
#endif
#else
             dealloc9d_qp
#endif
#else
             dealloc8d_qp
#endif
#else
             dealloc7d_qp
#endif
#else
             dealloc6d_qp
#endif
#else
             dealloc5d_qp
#endif
#else
             dealloc4d_qp
#endif
#else
             dealloc3d_qp
#endif
#else
             dealloc2d_qp
#endif
#else
             dealloc1d_qp
#endif
#endif
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             dealloc1d_isp, &
#if __MAX_RANK > 2
             dealloc2d_isp, &
#if __MAX_RANK > 3
             dealloc3d_isp, &
#if __MAX_RANK > 4
             dealloc4d_isp, &
#if __MAX_RANK > 5
             dealloc5d_isp, &
#if __MAX_RANK > 6
             dealloc6d_isp, &
#if __MAX_RANK > 7
             dealloc7d_isp, &
#if __MAX_RANK > 8
             dealloc8d_isp, &
#if __MAX_RANK > 9
             dealloc9d_isp, &
#if __MAX_RANK > 10
             dealloc10d_isp, &
#if __MAX_RANK > 11
             dealloc11d_isp, &
#if __MAX_RANK > 12
             dealloc12d_isp, &
#if __MAX_RANK > 13
             dealloc13d_isp, &
#if __MAX_RANK > 14
             dealloc14d_isp, &
#if __MAX_RANK > 15
             dealloc15d_isp, &
#else
             dealloc15d_isp
#endif
#else
             dealloc14d_isp
#endif
#else
             dealloc13d_isp
#endif
#else
             dealloc12d_isp
#endif
#else
             dealloc11d_isp
#endif
#else
             dealloc10d_isp
#endif
#else
             dealloc9d_isp
#endif
#else
             dealloc8d_isp
#endif
#else
             dealloc7d_isp
#endif
#else
             dealloc6d_isp
#endif
#else
             dealloc5d_isp
#endif
#else
             dealloc4d_isp
#endif
#else
             dealloc3d_isp
#endif
#else
             dealloc2d_isp
#endif
#else
             dealloc1d_isp
#endif
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             dealloc1d_idp, &
#if __MAX_RANK > 2
             dealloc2d_idp, &
#if __MAX_RANK > 3
             dealloc3d_idp, &
#if __MAX_RANK > 4
             dealloc4d_idp, &
#if __MAX_RANK > 5
             dealloc5d_idp, &
#if __MAX_RANK > 6
             dealloc6d_idp, &
#if __MAX_RANK > 7
             dealloc7d_idp, &
#if __MAX_RANK > 8
             dealloc8d_idp, &
#if __MAX_RANK > 9
             dealloc9d_idp, &
#if __MAX_RANK > 10
             dealloc10d_idp, &
#if __MAX_RANK > 11
             dealloc11d_idp, &
#if __MAX_RANK > 12
             dealloc12d_idp, &
#if __MAX_RANK > 13
             dealloc13d_idp, &
#if __MAX_RANK > 14
             dealloc14d_idp, &
#if __MAX_RANK > 15
             dealloc15d_idp, &
#else
             dealloc15d_idp
#endif
#else
             dealloc14d_idp
#endif
#else
             dealloc13d_idp
#endif
#else
             dealloc12d_idp
#endif
#else
             dealloc11d_idp
#endif
#else
             dealloc10d_idp
#endif
#else
             dealloc9d_idp
#endif
#else
             dealloc8d_idp
#endif
#else
             dealloc7d_idp
#endif
#else
             dealloc6d_idp
#endif
#else
             dealloc5d_idp
#endif
#else
             dealloc4d_idp
#endif
#else
             dealloc3d_idp
#endif
#else
             dealloc2d_idp
#endif
#else
             dealloc1d_idp
#endif
#ifdef __HAS_IQP
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             dealloc1d_iqp, &
#if __MAX_RANK > 2
             dealloc2d_iqp, &
#if __MAX_RANK > 3
             dealloc3d_iqp, &
#if __MAX_RANK > 4
             dealloc4d_iqp, &
#if __MAX_RANK > 5
             dealloc5d_iqp, &
#if __MAX_RANK > 6
             dealloc6d_iqp, &
#if __MAX_RANK > 7
             dealloc7d_iqp, &
#if __MAX_RANK > 8
             dealloc8d_iqp, &
#if __MAX_RANK > 9
             dealloc9d_iqp, &
#if __MAX_RANK > 10
             dealloc10d_iqp, &
#if __MAX_RANK > 11
             dealloc11d_iqp, &
#if __MAX_RANK > 12
             dealloc12d_iqp, &
#if __MAX_RANK > 13
             dealloc13d_iqp, &
#if __MAX_RANK > 14
             dealloc14d_iqp, &
#if __MAX_RANK > 15
             dealloc15d_iqp, &
#else
             dealloc15d_iqp
#endif
#else
             dealloc14d_iqp
#endif
#else
             dealloc13d_iqp
#endif
#else
             dealloc12d_iqp
#endif
#else
             dealloc11d_iqp
#endif
#else
             dealloc10d_iqp
#endif
#else
             dealloc9d_iqp
#endif
#else
             dealloc8d_iqp
#endif
#else
             dealloc7d_iqp
#endif
#else
             dealloc6d_iqp
#endif
#else
             dealloc5d_iqp
#endif
#else
             dealloc4d_iqp
#endif
#else
             dealloc3d_iqp
#endif
#else
             dealloc2d_iqp
#endif
#else
             dealloc1d_iqp
#endif
#endif
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             dealloc1d_csp, &
#if __MAX_RANK > 2
             dealloc2d_csp, &
#if __MAX_RANK > 3
             dealloc3d_csp, &
#if __MAX_RANK > 4
             dealloc4d_csp, &
#if __MAX_RANK > 5
             dealloc5d_csp, &
#if __MAX_RANK > 6
             dealloc6d_csp, &
#if __MAX_RANK > 7
             dealloc7d_csp, &
#if __MAX_RANK > 8
             dealloc8d_csp, &
#if __MAX_RANK > 9
             dealloc9d_csp, &
#if __MAX_RANK > 10
             dealloc10d_csp, &
#if __MAX_RANK > 11
             dealloc11d_csp, &
#if __MAX_RANK > 12
             dealloc12d_csp, &
#if __MAX_RANK > 13
             dealloc13d_csp, &
#if __MAX_RANK > 14
             dealloc14d_csp, &
#if __MAX_RANK > 15
             dealloc15d_csp, &
#else
             dealloc15d_csp
#endif
#else
             dealloc14d_csp
#endif
#else
             dealloc13d_csp
#endif
#else
             dealloc12d_csp
#endif
#else
             dealloc11d_csp
#endif
#else
             dealloc10d_csp
#endif
#else
             dealloc9d_csp
#endif
#else
             dealloc8d_csp
#endif
#else
             dealloc7d_csp
#endif
#else
             dealloc6d_csp
#endif
#else
             dealloc5d_csp
#endif
#else
             dealloc4d_csp
#endif
#else
             dealloc3d_csp
#endif
#else
             dealloc2d_csp
#endif
#else
             dealloc1d_csp
#endif
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             dealloc1d_cdp, &
#if __MAX_RANK > 2
             dealloc2d_cdp, &
#if __MAX_RANK > 3
             dealloc3d_cdp, &
#if __MAX_RANK > 4
             dealloc4d_cdp, &
#if __MAX_RANK > 5
             dealloc5d_cdp, &
#if __MAX_RANK > 6
             dealloc6d_cdp, &
#if __MAX_RANK > 7
             dealloc7d_cdp, &
#if __MAX_RANK > 8
             dealloc8d_cdp, &
#if __MAX_RANK > 9
             dealloc9d_cdp, &
#if __MAX_RANK > 10
             dealloc10d_cdp, &
#if __MAX_RANK > 11
             dealloc11d_cdp, &
#if __MAX_RANK > 12
             dealloc12d_cdp, &
#if __MAX_RANK > 13
             dealloc13d_cdp, &
#if __MAX_RANK > 14
             dealloc14d_cdp, &
#if __MAX_RANK > 15
             dealloc15d_cdp, &
#else
             dealloc15d_cdp
#endif
#else
             dealloc14d_cdp
#endif
#else
             dealloc13d_cdp
#endif
#else
             dealloc12d_cdp
#endif
#else
             dealloc11d_cdp
#endif
#else
             dealloc10d_cdp
#endif
#else
             dealloc9d_cdp
#endif
#else
             dealloc8d_cdp
#endif
#else
             dealloc7d_cdp
#endif
#else
             dealloc6d_cdp
#endif
#else
             dealloc5d_cdp
#endif
#else
             dealloc4d_cdp
#endif
#else
             dealloc3d_cdp
#endif
#else
             dealloc2d_cdp
#endif
#else
             dealloc1d_cdp
#endif
#ifdef __HAS_QP
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             dealloc1d_cqp, &
#if __MAX_RANK > 2
             dealloc2d_cqp, &
#if __MAX_RANK > 3
             dealloc3d_cqp, &
#if __MAX_RANK > 4
             dealloc4d_cqp, &
#if __MAX_RANK > 5
             dealloc5d_cqp, &
#if __MAX_RANK > 6
             dealloc6d_cqp, &
#if __MAX_RANK > 7
             dealloc7d_cqp, &
#if __MAX_RANK > 8
             dealloc8d_cqp, &
#if __MAX_RANK > 9
             dealloc9d_cqp, &
#if __MAX_RANK > 10
             dealloc10d_cqp, &
#if __MAX_RANK > 11
             dealloc11d_cqp, &
#if __MAX_RANK > 12
             dealloc12d_cqp, &
#if __MAX_RANK > 13
             dealloc13d_cqp, &
#if __MAX_RANK > 14
             dealloc14d_cqp, &
#if __MAX_RANK > 15
             dealloc15d_cqp, &
#else
             dealloc15d_cqp
#endif
#else
             dealloc14d_cqp
#endif
#else
             dealloc13d_cqp
#endif
#else
             dealloc12d_cqp
#endif
#else
             dealloc11d_cqp
#endif
#else
             dealloc10d_cqp
#endif
#else
             dealloc9d_cqp
#endif
#else
             dealloc8d_cqp
#endif
#else
             dealloc7d_cqp
#endif
#else
             dealloc6d_cqp
#endif
#else
             dealloc5d_cqp
#endif
#else
             dealloc4d_cqp
#endif
#else
             dealloc3d_cqp
#endif
#else
             dealloc2d_cqp
#endif
#else
             dealloc1d_cqp
#endif
#endif
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             dealloc1d_logic, &
#if __MAX_RANK > 2
             dealloc2d_logic, &
#if __MAX_RANK > 3
             dealloc3d_logic, &
#if __MAX_RANK > 4
             dealloc4d_logic, &
#if __MAX_RANK > 5
             dealloc5d_logic, &
#if __MAX_RANK > 6
             dealloc6d_logic, &
#if __MAX_RANK > 7
             dealloc7d_logic, &
#if __MAX_RANK > 8
             dealloc8d_logic, &
#if __MAX_RANK > 9
             dealloc9d_logic, &
#if __MAX_RANK > 10
             dealloc10d_logic, &
#if __MAX_RANK > 11
             dealloc11d_logic, &
#if __MAX_RANK > 12
             dealloc12d_logic, &
#if __MAX_RANK > 13
             dealloc13d_logic, &
#if __MAX_RANK > 14
             dealloc14d_logic, &
#if __MAX_RANK > 15
             dealloc15d_logic, &
#else
             dealloc15d_logic
#endif
#else
             dealloc14d_logic
#endif
#else
             dealloc13d_logic
#endif
#else
             dealloc12d_logic
#endif
#else
             dealloc11d_logic
#endif
#else
             dealloc10d_logic
#endif
#else
             dealloc9d_logic
#endif
#else
             dealloc8d_logic
#endif
#else
             dealloc7d_logic
#endif
#else
             dealloc6d_logic
#endif
#else
             dealloc5d_logic
#endif
#else
             dealloc4d_logic
#endif
#else
             dealloc3d_logic
#endif
#else
             dealloc2d_logic
#endif
#else
             dealloc1d_logic
#endif
      MODULE PROCEDURE &
#if __MAX_RANK > 1
             dealloc1d_char, &
#if __MAX_RANK > 2
             dealloc2d_char, &
#if __MAX_RANK > 3
             dealloc3d_char, &
#if __MAX_RANK > 4
             dealloc4d_char, &
#if __MAX_RANK > 5
             dealloc5d_char, &
#if __MAX_RANK > 6
             dealloc6d_char, &
#if __MAX_RANK > 7
             dealloc7d_char, &
#if __MAX_RANK > 8
             dealloc8d_char, &
#if __MAX_RANK > 9
             dealloc9d_char, &
#if __MAX_RANK > 10
             dealloc10d_char, &
#if __MAX_RANK > 11
             dealloc11d_char, &
#if __MAX_RANK > 12
             dealloc12d_char, &
#if __MAX_RANK > 13
             dealloc13d_char, &
#if __MAX_RANK > 14
             dealloc14d_char, &
#if __MAX_RANK > 15
             dealloc15d_char, &
#else
             dealloc15d_char
#endif
#else
             dealloc14d_char
#endif
#else
             dealloc13d_char
#endif
#else
             dealloc12d_char
#endif
#else
             dealloc11d_char
#endif
#else
             dealloc10d_char
#endif
#else
             dealloc9d_char
#endif
#else
             dealloc8d_char
#endif
#else
             dealloc7d_char
#endif
#else
             dealloc6d_char
#endif
#else
             dealloc5d_char
#endif
#else
             dealloc4d_char
#endif
#else
             dealloc3d_char
#endif
#else
             dealloc2d_char
#endif
#else
             dealloc1d_char
#endif
   END INTERFACE dealloc

END MODULE re_de_alloc
