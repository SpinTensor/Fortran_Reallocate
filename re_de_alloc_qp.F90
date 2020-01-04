!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Description:
!!    TODO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef __HAS_QP
MODULE re_de_allocqp

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   USE kinds, ONLY : qp
   IMPLICIT NONE

   PRIVATE
   PUBLIC :: &
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

   PUBLIC :: &
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

   PUBLIC :: &
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
             dealloc1ds_qp, &
             dealloc1d_qp
#endif

CONTAINS

#if __MAX_RANK >= 1
   SUBROUTINE alloc1ds_qp(qp_array, insize, instartidx)
   
      IMPLICIT NONE
      
      REAL(KIND=qp), DIMENSION(:), POINTER, INTENT(OUT) :: qp_array
      INTEGER, INTENT(IN) :: insize
      INTEGER, INTENT(IN), OPTIONAL :: instartidx
      INTEGER :: startidx, endidx
      
      IF (PRESENT(instartidx)) THEN
         startidx = instartidx 
      ELSE
         startidx = 1
      END IF
      endidx = insize + startidx - 1
      
      NULLIFY(qp_array)
      ALLOCATE(qp_array(startidx: endidx))
      qp_array(:) = 0.0_qp
      
      RETURN
      
   END SUBROUTINE alloc1ds_qp

   SUBROUTINE alloc1d_qp(qp_array, insize, instartidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:), POINTER, INTENT(OUT) :: qp_array
      INTEGER, DIMENSION(1), INTENT(IN) :: insize
      INTEGER, DIMENSION(1), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(1) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx = insize + startidx - 1

      NULLIFY(qp_array)
      ALLOCATE(qp_array(startidx(1): endidx(1)))
      qp_array(:) = 0.0_qp

      RETURN

   END SUBROUTINE alloc1d_qp
#endif

#if __MAX_RANK >= 2
   SUBROUTINE alloc2d_qp(qp_array, insize, instartidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:), POINTER, INTENT(OUT) :: qp_array
      INTEGER, DIMENSION(2), INTENT(IN) :: insize
      INTEGER, DIMENSION(2), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(2) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(qp_array)
      ALLOCATE(qp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2)))
      qp_array(:,:) = 0.0_qp

      RETURN

   END SUBROUTINE alloc2d_qp
#endif

#if __MAX_RANK >= 3
   SUBROUTINE alloc3d_qp(qp_array, insize, instartidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:), POINTER, INTENT(OUT) :: qp_array
      INTEGER, DIMENSION(3), INTENT(IN) :: insize
      INTEGER, DIMENSION(3), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(3) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(qp_array)
      ALLOCATE(qp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2), &
                        startidx(3):endidx(3)))
      qp_array(:,:,:) = 0.0_qp

      RETURN

   END SUBROUTINE alloc3d_qp
#endif

#if __MAX_RANK >= 4
   SUBROUTINE alloc4d_qp(qp_array, insize, instartidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:), POINTER, INTENT(OUT) :: qp_array
      INTEGER, DIMENSION(4), INTENT(IN) :: insize
      INTEGER, DIMENSION(4), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(4) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(qp_array)
      ALLOCATE(qp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2), &
                        startidx(3):endidx(3), &
                        startidx(4):endidx(4)))
      qp_array(:,:,:,:) = 0.0_qp

      RETURN

   END SUBROUTINE alloc4d_qp
#endif

#if __MAX_RANK >= 5
   SUBROUTINE alloc5d_qp(qp_array, insize, instartidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:), POINTER, INTENT(OUT) :: qp_array
      INTEGER, DIMENSION(5), INTENT(IN) :: insize
      INTEGER, DIMENSION(5), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(5) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(qp_array)
      ALLOCATE(qp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2), &
                        startidx(3):endidx(3), &
                        startidx(4):endidx(4), &
                        startidx(5):endidx(5)))
      qp_array(:,:,:,:,:) = 0.0_qp

      RETURN

   END SUBROUTINE alloc5d_qp
#endif

#if __MAX_RANK >= 6
   SUBROUTINE alloc6d_qp(qp_array, insize, instartidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:), POINTER, INTENT(OUT) :: qp_array
      INTEGER, DIMENSION(6), INTENT(IN) :: insize
      INTEGER, DIMENSION(6), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(6) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(qp_array)
      ALLOCATE(qp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2), &
                        startidx(3):endidx(3), &
                        startidx(4):endidx(4), &
                        startidx(5):endidx(5), &
                        startidx(6):endidx(6)))
      qp_array(:,:,:,:,:,:) = 0.0_qp

      RETURN

   END SUBROUTINE alloc6d_qp
#endif

#if __MAX_RANK >= 7
   SUBROUTINE alloc7d_qp(qp_array, insize, instartidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:), POINTER, INTENT(OUT) :: qp_array
      INTEGER, DIMENSION(7), INTENT(IN) :: insize
      INTEGER, DIMENSION(7), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(7) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(qp_array)
      ALLOCATE(qp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2), &
                        startidx(3):endidx(3), &
                        startidx(4):endidx(4), &
                        startidx(5):endidx(5), &
                        startidx(6):endidx(6), &
                        startidx(7):endidx(7)))
      qp_array(:,:,:,:,:,:,:) = 0.0_qp

      RETURN

   END SUBROUTINE alloc7d_qp
#endif

#if __MAX_RANK >= 8
   SUBROUTINE alloc8d_qp(qp_array, insize, instartidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:), POINTER, INTENT(OUT) :: qp_array
      INTEGER, DIMENSION(8), INTENT(IN) :: insize
      INTEGER, DIMENSION(8), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(8) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(qp_array)
      ALLOCATE(qp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2), &
                        startidx(3):endidx(3), &
                        startidx(4):endidx(4), &
                        startidx(5):endidx(5), &
                        startidx(6):endidx(6), &
                        startidx(7):endidx(7), &
                        startidx(8):endidx(8)))
      qp_array(:,:,:,:,:,:,:,:) = 0.0_qp

      RETURN

   END SUBROUTINE alloc8d_qp
#endif

#if __MAX_RANK >= 9
   SUBROUTINE alloc9d_qp(qp_array, insize, instartidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:), POINTER, INTENT(OUT) :: qp_array
      INTEGER, DIMENSION(9), INTENT(IN) :: insize
      INTEGER, DIMENSION(9), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(9) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(qp_array)
      ALLOCATE(qp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2), &
                        startidx(3):endidx(3), &
                        startidx(4):endidx(4), &
                        startidx(5):endidx(5), &
                        startidx(6):endidx(6), &
                        startidx(7):endidx(7), &
                        startidx(8):endidx(8), &
                        startidx(9):endidx(9)))
      qp_array(:,:,:,:,:,:,:,:,:) = 0.0_qp

      RETURN

   END SUBROUTINE alloc9d_qp
#endif

#if __MAX_RANK >= 10
   SUBROUTINE alloc10d_qp(qp_array, insize, instartidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(OUT) :: qp_array
      INTEGER, DIMENSION(10), INTENT(IN) :: insize
      INTEGER, DIMENSION(10), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(10) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(qp_array)
      ALLOCATE(qp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2), &
                        startidx(3):endidx(3), &
                        startidx(4):endidx(4), &
                        startidx(5):endidx(5), &
                        startidx(6):endidx(6), &
                        startidx(7):endidx(7), &
                        startidx(8):endidx(8), &
                        startidx(9):endidx(9), &
                        startidx(10):endidx(10)))
      qp_array(:,:,:,:,:,:,:,:,:,:) = 0.0_qp

      RETURN

   END SUBROUTINE alloc10d_qp
#endif

#if __MAX_RANK >= 11
   SUBROUTINE alloc11d_qp(qp_array, insize, instartidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(OUT) :: qp_array
      INTEGER, DIMENSION(11), INTENT(IN) :: insize
      INTEGER, DIMENSION(11), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(11) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(qp_array)
      ALLOCATE(qp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2), &
                        startidx(3):endidx(3), &
                        startidx(4):endidx(4), &
                        startidx(5):endidx(5), &
                        startidx(6):endidx(6), &
                        startidx(7):endidx(7), &
                        startidx(8):endidx(8), &
                        startidx(9):endidx(9), &
                        startidx(10):endidx(10), &
                        startidx(11):endidx(11)))
      qp_array(:,:,:,:,:,:,:,:,:,:,:) = 0.0_qp

      RETURN

   END SUBROUTINE alloc11d_qp
#endif

#if __MAX_RANK >= 12
   SUBROUTINE alloc12d_qp(qp_array, insize, instartidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(OUT) :: qp_array
      INTEGER, DIMENSION(12), INTENT(IN) :: insize
      INTEGER, DIMENSION(12), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(12) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(qp_array)
      ALLOCATE(qp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2), &
                        startidx(3):endidx(3), &
                        startidx(4):endidx(4), &
                        startidx(5):endidx(5), &
                        startidx(6):endidx(6), &
                        startidx(7):endidx(7), &
                        startidx(8):endidx(8), &
                        startidx(9):endidx(9), &
                        startidx(10):endidx(10), &
                        startidx(11):endidx(11), &
                        startidx(12):endidx(12)))
      qp_array(:,:,:,:,:,:,:,:,:,:,:,:) = 0.0_qp

      RETURN

   END SUBROUTINE alloc12d_qp
#endif

#if __MAX_RANK >= 13
   SUBROUTINE alloc13d_qp(qp_array, insize, instartidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(OUT) :: qp_array
      INTEGER, DIMENSION(13), INTENT(IN) :: insize
      INTEGER, DIMENSION(13), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(13) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(qp_array)
      ALLOCATE(qp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2), &
                        startidx(3):endidx(3), &
                        startidx(4):endidx(4), &
                        startidx(5):endidx(5), &
                        startidx(6):endidx(6), &
                        startidx(7):endidx(7), &
                        startidx(8):endidx(8), &
                        startidx(9):endidx(9), &
                        startidx(10):endidx(10), &
                        startidx(11):endidx(11), &
                        startidx(12):endidx(12), &
                        startidx(13):endidx(13)))
      qp_array(:,:,:,:,:,:,:,:,:,:,:,:,:) = 0.0_qp

      RETURN

   END SUBROUTINE alloc13d_qp
#endif

#if __MAX_RANK >= 14
   SUBROUTINE alloc14d_qp(qp_array, insize, instartidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(OUT) :: qp_array
      INTEGER, DIMENSION(14), INTENT(IN) :: insize
      INTEGER, DIMENSION(14), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(14) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(qp_array)
      ALLOCATE(qp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2), &
                        startidx(3):endidx(3), &
                        startidx(4):endidx(4), &
                        startidx(5):endidx(5), &
                        startidx(6):endidx(6), &
                        startidx(7):endidx(7), &
                        startidx(8):endidx(8), &
                        startidx(9):endidx(9), &
                        startidx(10):endidx(10), &
                        startidx(11):endidx(11), &
                        startidx(12):endidx(12), &
                        startidx(13):endidx(13), &
                        startidx(14):endidx(14)))
      qp_array(:,:,:,:,:,:,:,:,:,:,:,:,:,:) = 0.0_qp

      RETURN

   END SUBROUTINE alloc14d_qp
#endif

#if __MAX_RANK >= 15
   SUBROUTINE alloc15d_qp(qp_array, insize, instartidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(OUT) :: qp_array
      INTEGER, DIMENSION(15), INTENT(IN) :: insize
      INTEGER, DIMENSION(15), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(15) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(qp_array)
      ALLOCATE(qp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2), &
                        startidx(3):endidx(3), &
                        startidx(4):endidx(4), &
                        startidx(5):endidx(5), &
                        startidx(6):endidx(6), &
                        startidx(7):endidx(7), &
                        startidx(8):endidx(8), &
                        startidx(9):endidx(9), &
                        startidx(10):endidx(10), &
                        startidx(11):endidx(11), &
                        startidx(12):endidx(12), &
                        startidx(13):endidx(13), &
                        startidx(14):endidx(14), &
                        startidx(15):endidx(15)))
      qp_array(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:) = 0.0_qp

      RETURN

   END SUBROUTINE alloc15d_qp
#endif

#if __MAX_RANK >= 1
   SUBROUTINE realloc1ds_qp(qp_array, newsize, startidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:), POINTER, INTENT(INOUT) :: qp_array
      INTEGER, INTENT(IN) :: newsize
      INTEGER, INTENT(IN), OPTIONAL :: startidx

      REAL(KIND=qp), DIMENSION(:), POINTER :: tmparray
      INTEGER :: newstartidx, newendidx
      INTEGER :: oldstartidx, oldendidx
      INTEGER :: transstartidx, transendidx
      INTEGER :: i1
      
      NULLIFY(tmparray)

      IF (PRESENT(startidx)) THEN
         newstartidx = startidx
      ELSE
         newstartidx = 1
      END IF
      newendidx = newsize + newstartidx - 1

      IF (ASSOCIATED(qp_array)) THEN
         oldstartidx = LBOUND(qp_array,1)
         oldendidx =   UBOUND(qp_array,1)
         IF ((newstartidx /= oldstartidx).OR.(newendidx /= oldendidx)) THEN
#ifdef __DEBUG_MSG
            WRITE(UNIT=ERROR_UNIT, FMT=*) "Reallocating from [", oldstartidx,",",oldendidx,"]"
            WRITE(UNIT=ERROR_UNIT, FMT=*) "               to [", newstartidx,",",newendidx,"]"
#endif
            ALLOCATE(tmparray(newstartidx:newendidx))
            tmparray(:) = 0.0_qp
            transstartidx = MAX(oldstartidx,newstartidx)
            transendidx = MIN(oldendidx,newendidx)
            DO i1 = transstartidx, transendidx
               tmparray(i1) = qp_array(i1)
            END DO
            DEALLOCATE(qp_array)
            qp_array => tmparray
         END IF
      ELSE
         ALLOCATE(qp_array(newstartidx: newendidx))
         qp_array(:) = 0.0_qp
      END IF

      RETURN

   END SUBROUTINE realloc1ds_qp

   SUBROUTINE realloc1d_qp(qp_array, newsize, startidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:), POINTER, INTENT(INOUT) :: qp_array
      INTEGER, DIMENSION(1), INTENT(IN) :: newsize
      INTEGER, DIMENSION(1), INTENT(IN), OPTIONAL :: startidx

      REAL(KIND=qp), DIMENSION(:), POINTER :: tmparray
      INTEGER, DIMENSION(1) :: newstartidx, newendidx
      INTEGER, DIMENSION(1) :: oldstartidx, oldendidx
      INTEGER, DIMENSION(1) :: transstartidx, transendidx
      INTEGER :: i1
      
      NULLIFY(tmparray)

      IF (PRESENT(startidx)) THEN
         newstartidx(:) = startidx(:)
      ELSE
         newstartidx(:) = 1
      END IF
      newendidx = newsize + newstartidx - 1

      IF (ASSOCIATED(qp_array)) THEN
         oldstartidx = LBOUND(qp_array,1)
         oldendidx =   UBOUND(qp_array,1)
         IF ((ANY(oldstartidx(:) - newstartidx(:) /= 0)).OR. &
             (ANY(oldendidx(:) - newendidx(:) /= 0))) THEN
#ifdef __DEBUG_MSG
            WRITE(UNIT=ERROR_UNIT, FMT=*) "Reallocating from [", oldstartidx,",",oldendidx,"]"
            WRITE(UNIT=ERROR_UNIT, FMT=*) "               to [", newstartidx,",",newendidx,"]"
#endif
            ALLOCATE(tmparray(newstartidx(1):newendidx(1)))
            tmparray(:) = 0.0_qp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i1 = transstartidx(1), transendidx(1)
               tmparray(i1) = qp_array(i1)
            END DO
            DEALLOCATE(qp_array)
            qp_array => tmparray
         END IF
      ELSE
         ALLOCATE(qp_array(newstartidx(1): newendidx(1)))
         qp_array(:) = 0.0_qp
      END IF

      RETURN

   END SUBROUTINE realloc1d_qp
#endif

#if __MAX_RANK >= 2
   SUBROUTINE realloc2d_qp(qp_array, newsize, startidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:), POINTER, INTENT(INOUT) :: qp_array
      INTEGER, DIMENSION(2), INTENT(IN) :: newsize
      INTEGER, DIMENSION(2), INTENT(IN), OPTIONAL :: startidx

      REAL(KIND=qp), DIMENSION(:,:), POINTER :: tmparray
      INTEGER, DIMENSION(2) :: newstartidx, newendidx
      INTEGER, DIMENSION(2) :: oldstartidx, oldendidx
      INTEGER, DIMENSION(2) :: transstartidx, transendidx
      INTEGER :: i1,i2

      NULLIFY(tmparray)

      IF (PRESENT(startidx)) THEN
         newstartidx(:) = startidx(:)
      ELSE
         newstartidx(:) = 1
      END IF
      newendidx(:) = newsize(:) + newstartidx(:) - 1

      IF (ASSOCIATED(qp_array)) THEN
         oldstartidx = LBOUND(qp_array)
         oldendidx =   UBOUND(qp_array)
         IF ((ANY(oldstartidx(:) - newstartidx(:) /= 0)).OR. &
             (ANY(oldendidx(:) - newendidx(:) /= 0))) THEN
#ifdef __DEBUG_MSG
            WRITE(UNIT=ERROR_UNIT, FMT=*) "Reallocating from [", oldstartidx,",",oldendidx,"]"
            WRITE(UNIT=ERROR_UNIT, FMT=*) "               to [", newstartidx,",",newendidx,"]"
#endif
            ALLOCATE(tmparray(newstartidx(1):newendidx(1), &
                              newstartidx(2):newendidx(2)))
            tmparray(:,:) = 0.0_qp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i2 = transstartidx(2), transendidx(2)
               DO i1 = transstartidx(1), transendidx(1)
                  tmparray(i1,i2) = qp_array(i1,i2)
               END DO
            END DO
            DEALLOCATE(qp_array)
            qp_array => tmparray
         END IF
      ELSE
         ALLOCATE(qp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2)))
         qp_array(:,:) = 0.0_qp
      END IF

      RETURN

   END SUBROUTINE realloc2d_qp
#endif

#if __MAX_RANK >= 3
   SUBROUTINE realloc3d_qp(qp_array, newsize, startidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:), POINTER, INTENT(INOUT) :: qp_array
      INTEGER, DIMENSION(3), INTENT(IN) :: newsize
      INTEGER, DIMENSION(3), INTENT(IN), OPTIONAL :: startidx

      REAL(KIND=qp), DIMENSION(:,:,:), POINTER :: tmparray
      INTEGER, DIMENSION(3) :: newstartidx, newendidx
      INTEGER, DIMENSION(3) :: oldstartidx, oldendidx
      INTEGER, DIMENSION(3) :: transstartidx, transendidx
      INTEGER :: i1,i2,i3
      
      NULLIFY(qp_array)

      IF (PRESENT(startidx)) THEN
         newstartidx(:) = startidx(:)
      ELSE
         newstartidx(:) = 1
      END IF
      newendidx(:) = newsize(:) + newstartidx(:) - 1

      IF (ASSOCIATED(qp_array)) THEN
         oldstartidx = LBOUND(qp_array)
         oldendidx =   UBOUND(qp_array)
         IF ((ANY(oldstartidx(:) - newstartidx(:) /= 0)).OR. &
             (ANY(oldendidx(:) - newendidx(:) /= 0))) THEN
#ifdef __DEBUG_MSG
            WRITE(UNIT=ERROR_UNIT, FMT=*) "Reallocating from [", oldstartidx,",",oldendidx,"]"
            WRITE(UNIT=ERROR_UNIT, FMT=*) "               to [", newstartidx,",",newendidx,"]"
#endif
            ALLOCATE(tmparray(newstartidx(1):newendidx(1), &
                              newstartidx(2):newendidx(2), &
                              newstartidx(3):newendidx(3)))
            tmparray(:,:,:) = 0.0_qp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i3 = transstartidx(3), transendidx(3)
               DO i2 = transstartidx(2), transendidx(2)
                  DO i1 = transstartidx(1), transendidx(1)
                     tmparray(i1,i2,i3) = qp_array(i1,i2,i3)
                  END DO
               END DO
            END DO
            DEALLOCATE(qp_array)
            qp_array => tmparray
         END IF
      ELSE
         ALLOCATE(qp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2), &
                           newstartidx(3):newendidx(3)))
         qp_array(:,:,:) = 0.0_qp
      END IF

      RETURN

   END SUBROUTINE realloc3d_qp
#endif

#if __MAX_RANK >= 4
   SUBROUTINE realloc4d_qp(qp_array, newsize, startidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:), POINTER, INTENT(INOUT) :: qp_array
      INTEGER, DIMENSION(4), INTENT(IN) :: newsize
      INTEGER, DIMENSION(4), INTENT(IN), OPTIONAL :: startidx

      REAL(KIND=qp), DIMENSION(:,:,:,:), POINTER :: tmparray
      INTEGER, DIMENSION(4) :: newstartidx, newendidx
      INTEGER, DIMENSION(4) :: oldstartidx, oldendidx
      INTEGER, DIMENSION(4) :: transstartidx, transendidx
      INTEGER :: i1,i2,i3,i4
      
      NULLIFY(qp_array)

      IF (PRESENT(startidx)) THEN
         newstartidx(:) = startidx(:)
      ELSE
         newstartidx(:) = 1
      END IF
      newendidx(:) = newsize(:) + newstartidx(:) - 1

      IF (ASSOCIATED(qp_array)) THEN
         oldstartidx = LBOUND(qp_array)
         oldendidx =   UBOUND(qp_array)
         IF ((ANY(oldstartidx(:) - newstartidx(:) /= 0)).OR. &
             (ANY(oldendidx(:) - newendidx(:) /= 0))) THEN
#ifdef __DEBUG_MSG
            WRITE(UNIT=ERROR_UNIT, FMT=*) "Reallocating from [", oldstartidx,",",oldendidx,"]"
            WRITE(UNIT=ERROR_UNIT, FMT=*) "               to [", newstartidx,",",newendidx,"]"
#endif
            ALLOCATE(tmparray(newstartidx(1):newendidx(1), &
                              newstartidx(2):newendidx(2), &
                              newstartidx(3):newendidx(3), &
                              newstartidx(4):newendidx(4)))
            tmparray(:,:,:,:) = 0.0_qp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i4 = transstartidx(4), transendidx(4)
               DO i3 = transstartidx(3), transendidx(3)
                  DO i2 = transstartidx(2), transendidx(2)
                     DO i1 = transstartidx(1), transendidx(1)
                        tmparray(i1,i2,i3,i4) = qp_array(i1,i2,i3,i4)
                     END DO
                  END DO
               END DO
            END DO
            DEALLOCATE(qp_array)
            qp_array => tmparray
         END IF
      ELSE
         ALLOCATE(qp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2), &
                           newstartidx(3):newendidx(3), &
                           newstartidx(4):newendidx(4)))
         qp_array(:,:,:,:) = 0.0_qp
      END IF

      RETURN

   END SUBROUTINE realloc4d_qp
#endif

#if __MAX_RANK >= 5
   SUBROUTINE realloc5d_qp(qp_array, newsize, startidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array
      INTEGER, DIMENSION(5), INTENT(IN) :: newsize
      INTEGER, DIMENSION(5), INTENT(IN), OPTIONAL :: startidx

      REAL(KIND=qp), DIMENSION(:,:,:,:,:), POINTER :: tmparray
      INTEGER, DIMENSION(5) :: newstartidx, newendidx
      INTEGER, DIMENSION(5) :: oldstartidx, oldendidx
      INTEGER, DIMENSION(5) :: transstartidx, transendidx
      INTEGER :: i1,i2,i3,i4,i5
      
      NULLIFY(qp_array)

      IF (PRESENT(startidx)) THEN
         newstartidx(:) = startidx(:)
      ELSE
         newstartidx(:) = 1
      END IF
      newendidx(:) = newsize(:) + newstartidx(:) - 1

      IF (ASSOCIATED(qp_array)) THEN
         oldstartidx = LBOUND(qp_array)
         oldendidx =   UBOUND(qp_array)
         IF ((ANY(oldstartidx(:) - newstartidx(:) /= 0)).OR. &
             (ANY(oldendidx(:) - newendidx(:) /= 0))) THEN
#ifdef __DEBUG_MSG
            WRITE(UNIT=ERROR_UNIT, FMT=*) "Reallocating from [", oldstartidx,",",oldendidx,"]"
            WRITE(UNIT=ERROR_UNIT, FMT=*) "               to [", newstartidx,",",newendidx,"]"
#endif
            ALLOCATE(tmparray(newstartidx(1):newendidx(1), &
                              newstartidx(2):newendidx(2), &
                              newstartidx(3):newendidx(3), &
                              newstartidx(4):newendidx(4), &
                              newstartidx(5):newendidx(5)))
            tmparray(:,:,:,:,:) = 0.0_qp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i5 = transstartidx(5), transendidx(5)
               DO i4 = transstartidx(4), transendidx(4)
                  DO i3 = transstartidx(3), transendidx(3)
                     DO i2 = transstartidx(2), transendidx(2)
                        DO i1 = transstartidx(1), transendidx(1)
                           tmparray(i1,i2,i3,i4,i5) = qp_array(i1,i2,i3,i4,i5)
                        END DO
                     END DO
                  END DO
               END DO
            END DO
            DEALLOCATE(qp_array)
            qp_array => tmparray
         END IF
      ELSE
         ALLOCATE(qp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2), &
                           newstartidx(3):newendidx(3), &
                           newstartidx(4):newendidx(4), &
                           newstartidx(5):newendidx(5)))
         qp_array(:,:,:,:,:) = 0.0_qp
      END IF

      RETURN

   END SUBROUTINE realloc5d_qp
#endif

#if __MAX_RANK >= 6
   SUBROUTINE realloc6d_qp(qp_array, newsize, startidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array
      INTEGER, DIMENSION(6), INTENT(IN) :: newsize
      INTEGER, DIMENSION(6), INTENT(IN), OPTIONAL :: startidx

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:), POINTER :: tmparray
      INTEGER, DIMENSION(6) :: newstartidx, newendidx
      INTEGER, DIMENSION(6) :: oldstartidx, oldendidx
      INTEGER, DIMENSION(6) :: transstartidx, transendidx
      INTEGER :: i1,i2,i3,i4,i5,i6

      NULLIFY(tmparray)

      IF (PRESENT(startidx)) THEN
         newstartidx(:) = startidx(:)
      ELSE
         newstartidx(:) = 1
      END IF
      newendidx(:) = newsize(:) + newstartidx(:) - 1

      IF (ASSOCIATED(qp_array)) THEN
         oldstartidx = LBOUND(qp_array)
         oldendidx =   UBOUND(qp_array)
         IF ((ANY(oldstartidx(:) - newstartidx(:) /= 0)).OR. &
             (ANY(oldendidx(:) - newendidx(:) /= 0))) THEN
#ifdef __DEBUG_MSG
            WRITE(UNIT=ERROR_UNIT, FMT=*) "Reallocating from [", oldstartidx,",",oldendidx,"]"
            WRITE(UNIT=ERROR_UNIT, FMT=*) "               to [", newstartidx,",",newendidx,"]"
#endif
            ALLOCATE(tmparray(newstartidx(1):newendidx(1), &
                              newstartidx(2):newendidx(2), &
                              newstartidx(3):newendidx(3), &
                              newstartidx(4):newendidx(4), &
                              newstartidx(5):newendidx(5), &
                              newstartidx(6):newendidx(6)))
            tmparray(:,:,:,:,:,:) = 0.0_qp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i6 = transstartidx(6), transendidx(6)
               DO i5 = transstartidx(5), transendidx(5)
                  DO i4 = transstartidx(4), transendidx(4)
                     DO i3 = transstartidx(3), transendidx(3)
                        DO i2 = transstartidx(2), transendidx(2)
                           DO i1 = transstartidx(1), transendidx(1)
                              tmparray(i1,i2,i3,i4,i5,i6) = qp_array(i1,i2,i3,i4,i5,i6)
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
            DEALLOCATE(qp_array)
            qp_array => tmparray
         END IF
      ELSE
         ALLOCATE(qp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2), &
                           newstartidx(3):newendidx(3), &
                           newstartidx(4):newendidx(4), &
                           newstartidx(5):newendidx(5), &
                           newstartidx(6):newendidx(6)))
         qp_array(:,:,:,:,:,:) = 0.0_qp
      END IF

      RETURN

   END SUBROUTINE realloc6d_qp
#endif

#if __MAX_RANK >= 7
   SUBROUTINE realloc7d_qp(qp_array, newsize, startidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array
      INTEGER, DIMENSION(7), INTENT(IN) :: newsize
      INTEGER, DIMENSION(7), INTENT(IN), OPTIONAL :: startidx

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:), POINTER :: tmparray
      INTEGER, DIMENSION(7) :: newstartidx, newendidx
      INTEGER, DIMENSION(7) :: oldstartidx, oldendidx
      INTEGER, DIMENSION(7) :: transstartidx, transendidx
      INTEGER :: i1,i2,i3,i4,i5,i6,i7

      NULLIFY(tmparray)

      IF (PRESENT(startidx)) THEN
         newstartidx(:) = startidx(:)
      ELSE
         newstartidx(:) = 1
      END IF
      newendidx(:) = newsize(:) + newstartidx(:) - 1

      IF (ASSOCIATED(qp_array)) THEN
         oldstartidx = LBOUND(qp_array)
         oldendidx =   UBOUND(qp_array)
         IF ((ANY(oldstartidx(:) - newstartidx(:) /= 0)).OR. &
             (ANY(oldendidx(:) - newendidx(:) /= 0))) THEN
#ifdef __DEBUG_MSG
            WRITE(UNIT=ERROR_UNIT, FMT=*) "Reallocating from [", oldstartidx,",",oldendidx,"]"
            WRITE(UNIT=ERROR_UNIT, FMT=*) "               to [", newstartidx,",",newendidx,"]"
#endif
            ALLOCATE(tmparray(newstartidx(1):newendidx(1), &
                              newstartidx(2):newendidx(2), &
                              newstartidx(3):newendidx(3), &
                              newstartidx(4):newendidx(4), &
                              newstartidx(5):newendidx(5), &
                              newstartidx(6):newendidx(6), &
                              newstartidx(7):newendidx(7)))
            tmparray(:,:,:,:,:,:,:) = 0.0_qp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i7 = transstartidx(7), transendidx(7)
               DO i6 = transstartidx(6), transendidx(6)
                  DO i5 = transstartidx(5), transendidx(5)
                     DO i4 = transstartidx(4), transendidx(4)
                        DO i3 = transstartidx(3), transendidx(3)
                           DO i2 = transstartidx(2), transendidx(2)
                              DO i1 = transstartidx(1), transendidx(1)
                                 tmparray(i1,i2,i3,i4,i5,i6,i7) = qp_array(i1,i2,i3,i4,i5,i6,i7)
                              END DO
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
            DEALLOCATE(qp_array)
            qp_array => tmparray
         END IF
      ELSE
         ALLOCATE(qp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2), &
                           newstartidx(3):newendidx(3), &
                           newstartidx(4):newendidx(4), &
                           newstartidx(5):newendidx(5), &
                           newstartidx(6):newendidx(6), &
                           newstartidx(7):newendidx(7)))
         qp_array(:,:,:,:,:,:,:) = 0.0_qp
      END IF

      RETURN

   END SUBROUTINE realloc7d_qp
#endif

#if __MAX_RANK >= 8
   SUBROUTINE realloc8d_qp(qp_array, newsize, startidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array
      INTEGER, DIMENSION(8), INTENT(IN) :: newsize
      INTEGER, DIMENSION(8), INTENT(IN), OPTIONAL :: startidx

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:), POINTER :: tmparray
      INTEGER, DIMENSION(8) :: newstartidx, newendidx
      INTEGER, DIMENSION(8) :: oldstartidx, oldendidx
      INTEGER, DIMENSION(8) :: transstartidx, transendidx
      INTEGER :: i1,i2,i3,i4,i5,i6,i7,i8

      NULLIFY(tmparray)

      IF (PRESENT(startidx)) THEN
         newstartidx(:) = startidx(:)
      ELSE
         newstartidx(:) = 1
      END IF
      newendidx(:) = newsize(:) + newstartidx(:) - 1

      IF (ASSOCIATED(qp_array)) THEN
         oldstartidx = LBOUND(qp_array)
         oldendidx =   UBOUND(qp_array)
         IF ((ANY(oldstartidx(:) - newstartidx(:) /= 0)).OR. &
             (ANY(oldendidx(:) - newendidx(:) /= 0))) THEN
#ifdef __DEBUG_MSG
            WRITE(UNIT=ERROR_UNIT, FMT=*) "Reallocating from [", oldstartidx,",",oldendidx,"]"
            WRITE(UNIT=ERROR_UNIT, FMT=*) "               to [", newstartidx,",",newendidx,"]"
#endif
            ALLOCATE(tmparray(newstartidx(1):newendidx(1), &
                              newstartidx(2):newendidx(2), &
                              newstartidx(3):newendidx(3), &
                              newstartidx(4):newendidx(4), &
                              newstartidx(5):newendidx(5), &
                              newstartidx(6):newendidx(6), &
                              newstartidx(7):newendidx(7), &
                              newstartidx(8):newendidx(8)))
            tmparray(:,:,:,:,:,:,:,:) = 0.0_qp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i8 = transstartidx(8), transendidx(8)
               DO i7 = transstartidx(7), transendidx(7)
                  DO i6 = transstartidx(6), transendidx(6)
                     DO i5 = transstartidx(5), transendidx(5)
                        DO i4 = transstartidx(4), transendidx(4)
                           DO i3 = transstartidx(3), transendidx(3)
                              DO i2 = transstartidx(2), transendidx(2)
                                 DO i1 = transstartidx(1), transendidx(1)
                                    tmparray(i1,i2,i3,i4,i5,i6,i7,i8) = qp_array(i1,i2,i3,i4,i5,i6,i7,i8)
                                 END DO
                              END DO
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
            DEALLOCATE(qp_array)
            qp_array => tmparray
         END IF
      ELSE
         ALLOCATE(qp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2), &
                           newstartidx(3):newendidx(3), &
                           newstartidx(4):newendidx(4), &
                           newstartidx(5):newendidx(5), &
                           newstartidx(6):newendidx(6), &
                           newstartidx(7):newendidx(7), &
                           newstartidx(8):newendidx(8)))
         qp_array(:,:,:,:,:,:,:,:) = 0.0_qp
      END IF

      RETURN

   END SUBROUTINE realloc8d_qp
#endif

#if __MAX_RANK >= 9
   SUBROUTINE realloc9d_qp(qp_array, newsize, startidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array
      INTEGER, DIMENSION(9), INTENT(IN) :: newsize
      INTEGER, DIMENSION(9), INTENT(IN), OPTIONAL :: startidx

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:), POINTER :: tmparray
      INTEGER, DIMENSION(9) :: newstartidx, newendidx
      INTEGER, DIMENSION(9) :: oldstartidx, oldendidx
      INTEGER, DIMENSION(9) :: transstartidx, transendidx
      INTEGER :: i1,i2,i3,i4,i5,i6,i7,i8,i9

      NULLIFY(tmparray)

      IF (PRESENT(startidx)) THEN
         newstartidx(:) = startidx(:)
      ELSE
         newstartidx(:) = 1
      END IF
      newendidx(:) = newsize(:) + newstartidx(:) - 1

      IF (ASSOCIATED(qp_array)) THEN
         oldstartidx = LBOUND(qp_array)
         oldendidx =   UBOUND(qp_array)
         IF ((ANY(oldstartidx(:) - newstartidx(:) /= 0)).OR. &
             (ANY(oldendidx(:) - newendidx(:) /= 0))) THEN
#ifdef __DEBUG_MSG
            WRITE(UNIT=ERROR_UNIT, FMT=*) "Reallocating from [", oldstartidx,",",oldendidx,"]"
            WRITE(UNIT=ERROR_UNIT, FMT=*) "               to [", newstartidx,",",newendidx,"]"
#endif
            ALLOCATE(tmparray(newstartidx(1):newendidx(1), &
                              newstartidx(2):newendidx(2), &
                              newstartidx(3):newendidx(3), &
                              newstartidx(4):newendidx(4), &
                              newstartidx(5):newendidx(5), &
                              newstartidx(6):newendidx(6), &
                              newstartidx(7):newendidx(7), &
                              newstartidx(8):newendidx(8), &
                              newstartidx(9):newendidx(9)))
            tmparray(:,:,:,:,:,:,:,:,:) = 0.0_qp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i9 = transstartidx(9), transendidx(9)
               DO i8 = transstartidx(8), transendidx(8)
                  DO i7 = transstartidx(7), transendidx(7)
                     DO i6 = transstartidx(6), transendidx(6)
                        DO i5 = transstartidx(5), transendidx(5)
                           DO i4 = transstartidx(4), transendidx(4)
                              DO i3 = transstartidx(3), transendidx(3)
                                 DO i2 = transstartidx(2), transendidx(2)
                                    DO i1 = transstartidx(1), transendidx(1)
                                       tmparray(i1,i2,i3,i4,i5,i6,i7,i8,i9) = qp_array(i1,i2,i3,i4,i5,i6,i7,i8,i9)
                                    END DO
                                 END DO
                              END DO
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
            DEALLOCATE(qp_array)
            qp_array => tmparray
         END IF
      ELSE
         ALLOCATE(qp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2), &
                           newstartidx(3):newendidx(3), &
                           newstartidx(4):newendidx(4), &
                           newstartidx(5):newendidx(5), &
                           newstartidx(6):newendidx(6), &
                           newstartidx(7):newendidx(7), &
                           newstartidx(8):newendidx(8), &
                           newstartidx(9):newendidx(9)))
         qp_array(:,:,:,:,:,:,:,:,:) = 0.0_qp
      END IF

      RETURN

   END SUBROUTINE realloc9d_qp
#endif

#if __MAX_RANK >= 10
   SUBROUTINE realloc10d_qp(qp_array, newsize, startidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array
      INTEGER, DIMENSION(10), INTENT(IN) :: newsize
      INTEGER, DIMENSION(10), INTENT(IN), OPTIONAL :: startidx

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:), POINTER :: tmparray
      INTEGER, DIMENSION(10) :: newstartidx, newendidx
      INTEGER, DIMENSION(10) :: oldstartidx, oldendidx
      INTEGER, DIMENSION(10) :: transstartidx, transendidx
      INTEGER :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10

      NULLIFY(tmparray)

      IF (PRESENT(startidx)) THEN
         newstartidx(:) = startidx(:)
      ELSE
         newstartidx(:) = 1
      END IF
      newendidx(:) = newsize(:) + newstartidx(:) - 1

      IF (ASSOCIATED(qp_array)) THEN
         oldstartidx = LBOUND(qp_array)
         oldendidx =   UBOUND(qp_array)
         IF ((ANY(oldstartidx(:) - newstartidx(:) /= 0)).OR. &
             (ANY(oldendidx(:) - newendidx(:) /= 0))) THEN
#ifdef __DEBUG_MSG
            WRITE(UNIT=ERROR_UNIT, FMT=*) "Reallocating from [", oldstartidx,",",oldendidx,"]"
            WRITE(UNIT=ERROR_UNIT, FMT=*) "               to [", newstartidx,",",newendidx,"]"
#endif
            ALLOCATE(tmparray(newstartidx(1):newendidx(1), &
                              newstartidx(2):newendidx(2), &
                              newstartidx(3):newendidx(3), &
                              newstartidx(4):newendidx(4), &
                              newstartidx(5):newendidx(5), &
                              newstartidx(6):newendidx(6), &
                              newstartidx(7):newendidx(7), &
                              newstartidx(8):newendidx(8), &
                              newstartidx(9):newendidx(9), &
                              newstartidx(10):newendidx(10)))
            tmparray(:,:,:,:,:,:,:,:,:,:) = 0.0_qp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i10 = transstartidx(10), transendidx(10)
               DO i9 = transstartidx(9), transendidx(9)
                  DO i8 = transstartidx(8), transendidx(8)
                     DO i7 = transstartidx(7), transendidx(7)
                        DO i6 = transstartidx(6), transendidx(6)
                           DO i5 = transstartidx(5), transendidx(5)
                              DO i4 = transstartidx(4), transendidx(4)
                                 DO i3 = transstartidx(3), transendidx(3)
                                    DO i2 = transstartidx(2), transendidx(2)
                                       DO i1 = transstartidx(1), transendidx(1)
                                          tmparray(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10) = &
                                             qp_array(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                                       END DO
                                    END DO
                                 END DO
                              END DO
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
            DEALLOCATE(qp_array)
            qp_array => tmparray
         END IF
      ELSE
         ALLOCATE(qp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2), &
                           newstartidx(3):newendidx(3), &
                           newstartidx(4):newendidx(4), &
                           newstartidx(5):newendidx(5), &
                           newstartidx(6):newendidx(6), &
                           newstartidx(7):newendidx(7), &
                           newstartidx(8):newendidx(8), &
                           newstartidx(9):newendidx(9), &
                           newstartidx(10):newendidx(10)))
         qp_array(:,:,:,:,:,:,:,:,:,:) = 0.0_qp
      END IF

      RETURN

   END SUBROUTINE realloc10d_qp
#endif

#if __MAX_RANK >= 11
   SUBROUTINE realloc11d_qp(qp_array, newsize, startidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array
      INTEGER, DIMENSION(11), INTENT(IN) :: newsize
      INTEGER, DIMENSION(11), INTENT(IN), OPTIONAL :: startidx

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:), POINTER :: tmparray
      INTEGER, DIMENSION(11) :: newstartidx, newendidx
      INTEGER, DIMENSION(11) :: oldstartidx, oldendidx
      INTEGER, DIMENSION(11) :: transstartidx, transendidx
      INTEGER :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11

      NULLIFY(tmparray)

      IF (PRESENT(startidx)) THEN
         newstartidx(:) = startidx(:)
      ELSE
         newstartidx(:) = 1
      END IF
      newendidx(:) = newsize(:) + newstartidx(:) - 1

      IF (ASSOCIATED(qp_array)) THEN
         oldstartidx = LBOUND(qp_array)
         oldendidx =   UBOUND(qp_array)
         IF ((ANY(oldstartidx(:) - newstartidx(:) /= 0)).OR. &
             (ANY(oldendidx(:) - newendidx(:) /= 0))) THEN
#ifdef __DEBUG_MSG
            WRITE(UNIT=ERROR_UNIT, FMT=*) "Reallocating from [", oldstartidx,",",oldendidx,"]"
            WRITE(UNIT=ERROR_UNIT, FMT=*) "               to [", newstartidx,",",newendidx,"]"
#endif
            ALLOCATE(tmparray(newstartidx(1):newendidx(1), &
                              newstartidx(2):newendidx(2), &
                              newstartidx(3):newendidx(3), &
                              newstartidx(4):newendidx(4), &
                              newstartidx(5):newendidx(5), &
                              newstartidx(6):newendidx(6), &
                              newstartidx(7):newendidx(7), &
                              newstartidx(8):newendidx(8), &
                              newstartidx(9):newendidx(9), &
                              newstartidx(10):newendidx(10), &
                              newstartidx(11):newendidx(11)))
            tmparray(:,:,:,:,:,:,:,:,:,:,:) = 0.0_qp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i11 = transstartidx(11), transendidx(11)
               DO i10 = transstartidx(10), transendidx(10)
                  DO i9 = transstartidx(9), transendidx(9)
                     DO i8 = transstartidx(8), transendidx(8)
                        DO i7 = transstartidx(7), transendidx(7)
                           DO i6 = transstartidx(6), transendidx(6)
                              DO i5 = transstartidx(5), transendidx(5)
                                 DO i4 = transstartidx(4), transendidx(4)
                                    DO i3 = transstartidx(3), transendidx(3)
                                       DO i2 = transstartidx(2), transendidx(2)
                                          DO i1 = transstartidx(1), transendidx(1)
                                             tmparray(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) = &
                                                qp_array(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                                          END DO
                                       END DO
                                    END DO
                                 END DO
                              END DO
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
            DEALLOCATE(qp_array)
            qp_array => tmparray
         END IF
      ELSE
         ALLOCATE(qp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2), &
                           newstartidx(3):newendidx(3), &
                           newstartidx(4):newendidx(4), &
                           newstartidx(5):newendidx(5), &
                           newstartidx(6):newendidx(6), &
                           newstartidx(7):newendidx(7), &
                           newstartidx(8):newendidx(8), &
                           newstartidx(9):newendidx(9), &
                           newstartidx(10):newendidx(10), &
                           newstartidx(11):newendidx(11)))
         qp_array(:,:,:,:,:,:,:,:,:,:,:) = 0.0_qp
      END IF

      RETURN

   END SUBROUTINE realloc11d_qp
#endif

#if __MAX_RANK >= 12
   SUBROUTINE realloc12d_qp(qp_array, newsize, startidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array
      INTEGER, DIMENSION(12), INTENT(IN) :: newsize
      INTEGER, DIMENSION(12), INTENT(IN), OPTIONAL :: startidx

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:), POINTER :: tmparray
      INTEGER, DIMENSION(12) :: newstartidx, newendidx
      INTEGER, DIMENSION(12) :: oldstartidx, oldendidx
      INTEGER, DIMENSION(12) :: transstartidx, transendidx
      INTEGER :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12

      NULLIFY(tmparray)

      IF (PRESENT(startidx)) THEN
         newstartidx(:) = startidx(:)
      ELSE
         newstartidx(:) = 1
      END IF
      newendidx(:) = newsize(:) + newstartidx(:) - 1

      IF (ASSOCIATED(qp_array)) THEN
         oldstartidx = LBOUND(qp_array)
         oldendidx =   UBOUND(qp_array)
         IF ((ANY(oldstartidx(:) - newstartidx(:) /= 0)).OR. &
             (ANY(oldendidx(:) - newendidx(:) /= 0))) THEN
#ifdef __DEBUG_MSG
            WRITE(UNIT=ERROR_UNIT, FMT=*) "Reallocating from [", oldstartidx,",",oldendidx,"]"
            WRITE(UNIT=ERROR_UNIT, FMT=*) "               to [", newstartidx,",",newendidx,"]"
#endif
            ALLOCATE(tmparray(newstartidx(1):newendidx(1), &
                              newstartidx(2):newendidx(2), &
                              newstartidx(3):newendidx(3), &
                              newstartidx(4):newendidx(4), &
                              newstartidx(5):newendidx(5), &
                              newstartidx(6):newendidx(6), &
                              newstartidx(7):newendidx(7), &
                              newstartidx(8):newendidx(8), &
                              newstartidx(9):newendidx(9), &
                              newstartidx(10):newendidx(10), &
                              newstartidx(11):newendidx(11), &
                              newstartidx(12):newendidx(12)))
            tmparray(:,:,:,:,:,:,:,:,:,:,:,:) = 0.0_qp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i12 = transstartidx(12), transendidx(12)
               DO i11 = transstartidx(11), transendidx(11)
                  DO i10 = transstartidx(10), transendidx(10)
                     DO i9 = transstartidx(9), transendidx(9)
                        DO i8 = transstartidx(8), transendidx(8)
                           DO i7 = transstartidx(7), transendidx(7)
                              DO i6 = transstartidx(6), transendidx(6)
                                 DO i5 = transstartidx(5), transendidx(5)
                                    DO i4 = transstartidx(4), transendidx(4)
                                       DO i3 = transstartidx(3), transendidx(3)
                                          DO i2 = transstartidx(2), transendidx(2)
                                             DO i1 = transstartidx(1), transendidx(1)
                                                tmparray(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = &
                                                   qp_array(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                                             END DO
                                          END DO
                                       END DO
                                    END DO
                                 END DO
                              END DO
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
            DEALLOCATE(qp_array)
            qp_array => tmparray
         END IF
      ELSE
         ALLOCATE(qp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2), &
                           newstartidx(3):newendidx(3), &
                           newstartidx(4):newendidx(4), &
                           newstartidx(5):newendidx(5), &
                           newstartidx(6):newendidx(6), &
                           newstartidx(7):newendidx(7), &
                           newstartidx(8):newendidx(8), &
                           newstartidx(9):newendidx(9), &
                           newstartidx(10):newendidx(10), &
                           newstartidx(11):newendidx(11), &
                           newstartidx(12):newendidx(12)))
         qp_array(:,:,:,:,:,:,:,:,:,:,:,:) = 0.0_qp
      END IF

      RETURN

   END SUBROUTINE realloc12d_qp
#endif

#if __MAX_RANK >= 13
   SUBROUTINE realloc13d_qp(qp_array, newsize, startidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array
      INTEGER, DIMENSION(13), INTENT(IN) :: newsize
      INTEGER, DIMENSION(13), INTENT(IN), OPTIONAL :: startidx

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER :: tmparray
      INTEGER, DIMENSION(13) :: newstartidx, newendidx
      INTEGER, DIMENSION(13) :: oldstartidx, oldendidx
      INTEGER, DIMENSION(13) :: transstartidx, transendidx
      INTEGER :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13

      NULLIFY(tmparray)

      IF (PRESENT(startidx)) THEN
         newstartidx(:) = startidx(:)
      ELSE
         newstartidx(:) = 1
      END IF
      newendidx(:) = newsize(:) + newstartidx(:) - 1

      IF (ASSOCIATED(qp_array)) THEN
         oldstartidx = LBOUND(qp_array)
         oldendidx =   UBOUND(qp_array)
         IF ((ANY(oldstartidx(:) - newstartidx(:) /= 0)).OR. &
             (ANY(oldendidx(:) - newendidx(:) /= 0))) THEN
#ifdef __DEBUG_MSG
            WRITE(UNIT=ERROR_UNIT, FMT=*) "Reallocating from [", oldstartidx,",",oldendidx,"]"
            WRITE(UNIT=ERROR_UNIT, FMT=*) "               to [", newstartidx,",",newendidx,"]"
#endif
            ALLOCATE(tmparray(newstartidx(1):newendidx(1), &
                              newstartidx(2):newendidx(2), &
                              newstartidx(3):newendidx(3), &
                              newstartidx(4):newendidx(4), &
                              newstartidx(5):newendidx(5), &
                              newstartidx(6):newendidx(6), &
                              newstartidx(7):newendidx(7), &
                              newstartidx(8):newendidx(8), &
                              newstartidx(9):newendidx(9), &
                              newstartidx(10):newendidx(10), &
                              newstartidx(11):newendidx(11), &
                              newstartidx(12):newendidx(12), &
                              newstartidx(13):newendidx(13)))
            tmparray(:,:,:,:,:,:,:,:,:,:,:,:,:) = 0.0_qp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i13 = transstartidx(13), transendidx(13)
               DO i12 = transstartidx(12), transendidx(12)
                  DO i11 = transstartidx(11), transendidx(11)
                     DO i10 = transstartidx(10), transendidx(10)
                        DO i9 = transstartidx(9), transendidx(9)
                           DO i8 = transstartidx(8), transendidx(8)
                              DO i7 = transstartidx(7), transendidx(7)
                                 DO i6 = transstartidx(6), transendidx(6)
                                    DO i5 = transstartidx(5), transendidx(5)
                                       DO i4 = transstartidx(4), transendidx(4)
                                          DO i3 = transstartidx(3), transendidx(3)
                                             DO i2 = transstartidx(2), transendidx(2)
                                                DO i1 = transstartidx(1), transendidx(1)
                                                   tmparray(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = &
                                                      qp_array(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                                                END DO
                                             END DO
                                          END DO
                                       END DO
                                    END DO
                                 END DO
                              END DO
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
            DEALLOCATE(qp_array)
            qp_array => tmparray
         END IF
      ELSE
         ALLOCATE(qp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2), &
                           newstartidx(3):newendidx(3), &
                           newstartidx(4):newendidx(4), &
                           newstartidx(5):newendidx(5), &
                           newstartidx(6):newendidx(6), &
                           newstartidx(7):newendidx(7), &
                           newstartidx(8):newendidx(8), &
                           newstartidx(9):newendidx(9), &
                           newstartidx(10):newendidx(10), &
                           newstartidx(11):newendidx(11), &
                           newstartidx(12):newendidx(12), &
                           newstartidx(13):newendidx(13)))
         qp_array(:,:,:,:,:,:,:,:,:,:,:,:,:) = 0.0_qp
      END IF

      RETURN

   END SUBROUTINE realloc13d_qp
#endif

#if __MAX_RANK >= 14
   SUBROUTINE realloc14d_qp(qp_array, newsize, startidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array
      INTEGER, DIMENSION(14), INTENT(IN) :: newsize
      INTEGER, DIMENSION(14), INTENT(IN), OPTIONAL :: startidx

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER :: tmparray
      INTEGER, DIMENSION(14) :: newstartidx, newendidx
      INTEGER, DIMENSION(14) :: oldstartidx, oldendidx
      INTEGER, DIMENSION(14) :: transstartidx, transendidx
      INTEGER :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14

      NULLIFY(tmparray)

      IF (PRESENT(startidx)) THEN
         newstartidx(:) = startidx(:)
      ELSE
         newstartidx(:) = 1
      END IF
      newendidx(:) = newsize(:) + newstartidx(:) - 1

      IF (ASSOCIATED(qp_array)) THEN
         oldstartidx = LBOUND(qp_array)
         oldendidx =   UBOUND(qp_array)
         IF ((ANY(oldstartidx(:) - newstartidx(:) /= 0)).OR. &
             (ANY(oldendidx(:) - newendidx(:) /= 0))) THEN
#ifdef __DEBUG_MSG
            WRITE(UNIT=ERROR_UNIT, FMT=*) "Reallocating from [", oldstartidx,",",oldendidx,"]"
            WRITE(UNIT=ERROR_UNIT, FMT=*) "               to [", newstartidx,",",newendidx,"]"
#endif
            ALLOCATE(tmparray(newstartidx(1):newendidx(1), &
                              newstartidx(2):newendidx(2), &
                              newstartidx(3):newendidx(3), &
                              newstartidx(4):newendidx(4), &
                              newstartidx(5):newendidx(5), &
                              newstartidx(6):newendidx(6), &
                              newstartidx(7):newendidx(7), &
                              newstartidx(8):newendidx(8), &
                              newstartidx(9):newendidx(9), &
                              newstartidx(10):newendidx(10), &
                              newstartidx(11):newendidx(11), &
                              newstartidx(12):newendidx(12), &
                              newstartidx(13):newendidx(13), &
                              newstartidx(14):newendidx(14)))
            tmparray(:,:,:,:,:,:,:,:,:,:,:,:,:,:) = 0.0_qp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i14 = transstartidx(14), transendidx(14)
               DO i13 = transstartidx(13), transendidx(13)
                  DO i12 = transstartidx(12), transendidx(12)
                     DO i11 = transstartidx(11), transendidx(11)
                        DO i10 = transstartidx(10), transendidx(10)
                           DO i9 = transstartidx(9), transendidx(9)
                              DO i8 = transstartidx(8), transendidx(8)
                                 DO i7 = transstartidx(7), transendidx(7)
                                    DO i6 = transstartidx(6), transendidx(6)
                                       DO i5 = transstartidx(5), transendidx(5)
                                          DO i4 = transstartidx(4), transendidx(4)
                                             DO i3 = transstartidx(3), transendidx(3)
                                                DO i2 = transstartidx(2), transendidx(2)
                                                   DO i1 = transstartidx(1), transendidx(1)
                                                      tmparray(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = &
                                                         qp_array(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                                   END DO
                                                END DO
                                             END DO
                                          END DO
                                       END DO
                                    END DO
                                 END DO
                              END DO
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
            DEALLOCATE(qp_array)
            qp_array => tmparray
         END IF
      ELSE
         ALLOCATE(qp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2), &
                           newstartidx(3):newendidx(3), &
                           newstartidx(4):newendidx(4), &
                           newstartidx(5):newendidx(5), &
                           newstartidx(6):newendidx(6), &
                           newstartidx(7):newendidx(7), &
                           newstartidx(8):newendidx(8), &
                           newstartidx(9):newendidx(9), &
                           newstartidx(10):newendidx(10), &
                           newstartidx(11):newendidx(11), &
                           newstartidx(12):newendidx(12), &
                           newstartidx(13):newendidx(13), &
                           newstartidx(14):newendidx(14)))
         qp_array(:,:,:,:,:,:,:,:,:,:,:,:,:,:) = 0.0_qp
      END IF

      RETURN

   END SUBROUTINE realloc14d_qp
#endif

#if __MAX_RANK >= 15
   SUBROUTINE realloc15d_qp(qp_array, newsize, startidx)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array
      INTEGER, DIMENSION(15), INTENT(IN) :: newsize
      INTEGER, DIMENSION(15), INTENT(IN), OPTIONAL :: startidx

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER :: tmparray
      INTEGER, DIMENSION(15) :: newstartidx, newendidx
      INTEGER, DIMENSION(15) :: oldstartidx, oldendidx
      INTEGER, DIMENSION(15) :: transstartidx, transendidx
      INTEGER :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15

      NULLIFY(tmparray)

      IF (PRESENT(startidx)) THEN
         newstartidx(:) = startidx(:)
      ELSE
         newstartidx(:) = 1
      END IF
      newendidx(:) = newsize(:) + newstartidx(:) - 1

      IF (ASSOCIATED(qp_array)) THEN
         oldstartidx = LBOUND(qp_array)
         oldendidx =   UBOUND(qp_array)
         IF ((ANY(oldstartidx(:) - newstartidx(:) /= 0)).OR. &
             (ANY(oldendidx(:) - newendidx(:) /= 0))) THEN
#ifdef __DEBUG_MSG
            WRITE(UNIT=ERROR_UNIT, FMT=*) "Reallocating from [", oldstartidx,",",oldendidx,"]"
            WRITE(UNIT=ERROR_UNIT, FMT=*) "               to [", newstartidx,",",newendidx,"]"
#endif
            ALLOCATE(tmparray(newstartidx(1):newendidx(1), &
                              newstartidx(2):newendidx(2), &
                              newstartidx(3):newendidx(3), &
                              newstartidx(4):newendidx(4), &
                              newstartidx(5):newendidx(5), &
                              newstartidx(6):newendidx(6), &
                              newstartidx(7):newendidx(7), &
                              newstartidx(8):newendidx(8), &
                              newstartidx(9):newendidx(9), &
                              newstartidx(10):newendidx(10), &
                              newstartidx(11):newendidx(11), &
                              newstartidx(12):newendidx(12), &
                              newstartidx(13):newendidx(13), &
                              newstartidx(14):newendidx(14), &
                              newstartidx(15):newendidx(15)))
            tmparray(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:) = 0.0_qp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i15 = transstartidx(15), transendidx(15)
               DO i14 = transstartidx(14), transendidx(14)
                  DO i13 = transstartidx(13), transendidx(13)
                     DO i12 = transstartidx(12), transendidx(12)
                        DO i11 = transstartidx(11), transendidx(11)
                           DO i10 = transstartidx(10), transendidx(10)
                              DO i9 = transstartidx(9), transendidx(9)
                                 DO i8 = transstartidx(8), transendidx(8)
                                    DO i7 = transstartidx(7), transendidx(7)
                                       DO i6 = transstartidx(6), transendidx(6)
                                          DO i5 = transstartidx(5), transendidx(5)
                                             DO i4 = transstartidx(4), transendidx(4)
                                                DO i3 = transstartidx(3), transendidx(3)
                                                   DO i2 = transstartidx(2), transendidx(2)
                                                      DO i1 = transstartidx(1), transendidx(1)
                                                         tmparray(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) = &
                                                            qp_array(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15)
                                                      END DO
                                                   END DO
                                                END DO
                                             END DO
                                          END DO
                                       END DO
                                    END DO
                                 END DO
                              END DO
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
            DEALLOCATE(qp_array)
            qp_array => tmparray
         END IF
      ELSE
         ALLOCATE(qp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2), &
                           newstartidx(3):newendidx(3), &
                           newstartidx(4):newendidx(4), &
                           newstartidx(5):newendidx(5), &
                           newstartidx(6):newendidx(6), &
                           newstartidx(7):newendidx(7), &
                           newstartidx(8):newendidx(8), &
                           newstartidx(9):newendidx(9), &
                           newstartidx(10):newendidx(10), &
                           newstartidx(11):newendidx(11), &
                           newstartidx(12):newendidx(12), &
                           newstartidx(13):newendidx(13), &
                           newstartidx(14):newendidx(14), &
                           newstartidx(15):newendidx(15)))
         qp_array(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:) = 0.0_qp
      END IF

      RETURN

   END SUBROUTINE realloc15d_qp
#endif

#if __MAX_RANK >= 1
   SUBROUTINE dealloc1d_qp(qp_array)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:), POINTER, INTENT(INOUT) :: qp_array

      IF (ASSOCIATED(qp_array)) THEN
         DEALLOCATE(qp_array)
         NULLIFY(qp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc1d_qp
#endif

#if __MAX_RANK >= 2
   SUBROUTINE dealloc2d_qp(qp_array)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:), POINTER, INTENT(INOUT) :: qp_array

      IF (ASSOCIATED(qp_array)) THEN
         DEALLOCATE(qp_array)
         NULLIFY(qp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc2d_qp
#endif

#if __MAX_RANK >= 3
   SUBROUTINE dealloc3d_qp(qp_array)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:), POINTER, INTENT(INOUT) :: qp_array

      IF (ASSOCIATED(qp_array)) THEN
         DEALLOCATE(qp_array)
         NULLIFY(qp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc3d_qp
#endif

#if __MAX_RANK >= 4
   SUBROUTINE dealloc4d_qp(qp_array)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:), POINTER, INTENT(INOUT) :: qp_array

      IF (ASSOCIATED(qp_array)) THEN
         DEALLOCATE(qp_array)
         NULLIFY(qp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc4d_qp
#endif

#if __MAX_RANK >= 5
   SUBROUTINE dealloc5d_qp(qp_array)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array

      IF (ASSOCIATED(qp_array)) THEN
         DEALLOCATE(qp_array)
         NULLIFY(qp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc5d_qp
#endif

#if __MAX_RANK >= 6
   SUBROUTINE dealloc6d_qp(qp_array)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array

      IF (ASSOCIATED(qp_array)) THEN
         DEALLOCATE(qp_array)
         NULLIFY(qp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc6d_qp
#endif

#if __MAX_RANK >= 7
   SUBROUTINE dealloc7d_qp(qp_array)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array

      IF (ASSOCIATED(qp_array)) THEN
         DEALLOCATE(qp_array)
         NULLIFY(qp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc7d_qp
#endif

#if __MAX_RANK >= 8
   SUBROUTINE dealloc8d_qp(qp_array)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array

      IF (ASSOCIATED(qp_array)) THEN
         DEALLOCATE(qp_array)
         NULLIFY(qp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc8d_qp
#endif

#if __MAX_RANK >= 9
   SUBROUTINE dealloc9d_qp(qp_array)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array

      IF (ASSOCIATED(qp_array)) THEN
         DEALLOCATE(qp_array)
         NULLIFY(qp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc9d_qp
#endif

#if __MAX_RANK >= 10
   SUBROUTINE dealloc10d_qp(qp_array)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array

      IF (ASSOCIATED(qp_array)) THEN
         DEALLOCATE(qp_array)
         NULLIFY(qp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc10d_qp
#endif

#if __MAX_RANK >= 11
   SUBROUTINE dealloc11d_qp(qp_array)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array

      IF (ASSOCIATED(qp_array)) THEN
         DEALLOCATE(qp_array)
         NULLIFY(qp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc11d_qp
#endif

#if __MAX_RANK >= 12
   SUBROUTINE dealloc12d_qp(qp_array)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array

      IF (ASSOCIATED(qp_array)) THEN
         DEALLOCATE(qp_array)
         NULLIFY(qp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc12d_qp
#endif

#if __MAX_RANK >= 13
   SUBROUTINE dealloc13d_qp(qp_array)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array

      IF (ASSOCIATED(qp_array)) THEN
         DEALLOCATE(qp_array)
         NULLIFY(qp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc13d_qp
#endif

#if __MAX_RANK >= 14
   SUBROUTINE dealloc14d_qp(qp_array)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array

      IF (ASSOCIATED(qp_array)) THEN
         DEALLOCATE(qp_array)
         NULLIFY(qp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc14d_qp
#endif

#if __MAX_RANK >= 15
   SUBROUTINE dealloc15d_qp(qp_array)

      IMPLICIT NONE

      REAL(KIND=qp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: qp_array

      IF (ASSOCIATED(qp_array)) THEN
         DEALLOCATE(qp_array)
         NULLIFY(qp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc15d_qp
#endif

#else
MODULE re_de_allocqp
CONTAINS
#endif

END MODULE re_de_allocqp
