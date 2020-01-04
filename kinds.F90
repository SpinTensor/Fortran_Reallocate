!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Description:
!!    contains kinds for standardized precision
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE kinds

   USE, INTRINSIC :: ISO_FORTRAN_ENV

   IMPLICIT NONE

   PRIVATE

#ifdef __HAS_QP
   PUBLIC :: sp, dp, qp
   PUBLIC :: csp, cdp, cqp
#else
   PUBLIC :: csp, cdp
   PUBLIC :: sp, dp
#endif
#ifdef __HAS_IQP
   PUBLIC :: isp, idp, iqp
#else
   PUBLIC :: isp, idp
#endif

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !! Description:
   !!    Real kinds
   !! Variables:
   !!    sp: single precision
   !!    dp: double precision
   !!    qp: quadruple precision
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   INTEGER, PARAMETER :: sp = KIND(1.0)
   INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(2*PRECISION(1.0_sp))
#ifdef __HAS_QP
   INTEGER, PARAMETER :: qp = SELECTED_REAL_KIND(2*PRECISION(1.0_dp))
#endif

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !! Description:
   !!    Integer kinds
   !! Variables:
   !!    isp: single precision
   !!    idp: double precision
   !!    iqp: quadruple precision
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   INTEGER, PARAMETER :: isp = KIND(1)
   INTEGER, PARAMETER :: idp = 2*isp
#ifdef __HAS_IQP
   INTEGER, PARAMETER :: iqp = 2*idp
#endif

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !! Description:
   !!    Complex kinds
   !! Variables:
   !!    csp: single precision
   !!    cdp: double precision
   !!    cqp: quadruple precision
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   INTEGER, PARAMETER :: csp = KIND(1.0)
   INTEGER, PARAMETER :: cdp = SELECTED_REAL_KIND(2*PRECISION(1.0_sp))
#ifdef __HAS_QP
   INTEGER, PARAMETER :: cqp = SELECTED_REAL_KIND(2*PRECISION(1.0_dp))
#endif

END MODULE kinds
