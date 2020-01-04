!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Description:
!!    TODO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE re_de_allocisp

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   USE kinds, ONLY : isp
   IMPLICIT NONE

   PRIVATE
   PUBLIC :: &
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

   PUBLIC :: &
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

   PUBLIC :: &
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
             dealloc1ds_isp, &
             dealloc1d_isp
#endif

CONTAINS

#if __MAX_RANK >= 1
   SUBROUTINE alloc1ds_isp(isp_array, insize, instartidx)
   
      IMPLICIT NONE
      
      INTEGER(KIND=isp), DIMENSION(:), POINTER, INTENT(OUT) :: isp_array
      INTEGER, INTENT(IN) :: insize
      INTEGER, INTENT(IN), OPTIONAL :: instartidx
      INTEGER :: startidx, endidx
      
      IF (PRESENT(instartidx)) THEN
         startidx = instartidx 
      ELSE
         startidx = 1
      END IF
      endidx = insize + startidx - 1
      
      NULLIFY(isp_array)
      ALLOCATE(isp_array(startidx: endidx))
      isp_array(:) = 0_isp
      
      RETURN
      
   END SUBROUTINE alloc1ds_isp

   SUBROUTINE alloc1d_isp(isp_array, insize, instartidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:), POINTER, INTENT(OUT) :: isp_array
      INTEGER, DIMENSION(1), INTENT(IN) :: insize
      INTEGER, DIMENSION(1), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(1) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx = insize + startidx - 1

      NULLIFY(isp_array)
      ALLOCATE(isp_array(startidx(1): endidx(1)))
      isp_array(:) = 0_isp

      RETURN

   END SUBROUTINE alloc1d_isp
#endif

#if __MAX_RANK >= 2
   SUBROUTINE alloc2d_isp(isp_array, insize, instartidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:), POINTER, INTENT(OUT) :: isp_array
      INTEGER, DIMENSION(2), INTENT(IN) :: insize
      INTEGER, DIMENSION(2), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(2) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(isp_array)
      ALLOCATE(isp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2)))
      isp_array(:,:) = 0_isp

      RETURN

   END SUBROUTINE alloc2d_isp
#endif

#if __MAX_RANK >= 3
   SUBROUTINE alloc3d_isp(isp_array, insize, instartidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:), POINTER, INTENT(OUT) :: isp_array
      INTEGER, DIMENSION(3), INTENT(IN) :: insize
      INTEGER, DIMENSION(3), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(3) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(isp_array)
      ALLOCATE(isp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2), &
                        startidx(3):endidx(3)))
      isp_array(:,:,:) = 0_isp

      RETURN

   END SUBROUTINE alloc3d_isp
#endif

#if __MAX_RANK >= 4
   SUBROUTINE alloc4d_isp(isp_array, insize, instartidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:), POINTER, INTENT(OUT) :: isp_array
      INTEGER, DIMENSION(4), INTENT(IN) :: insize
      INTEGER, DIMENSION(4), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(4) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(isp_array)
      ALLOCATE(isp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2), &
                        startidx(3):endidx(3), &
                        startidx(4):endidx(4)))
      isp_array(:,:,:,:) = 0_isp

      RETURN

   END SUBROUTINE alloc4d_isp
#endif

#if __MAX_RANK >= 5
   SUBROUTINE alloc5d_isp(isp_array, insize, instartidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:), POINTER, INTENT(OUT) :: isp_array
      INTEGER, DIMENSION(5), INTENT(IN) :: insize
      INTEGER, DIMENSION(5), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(5) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(isp_array)
      ALLOCATE(isp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2), &
                        startidx(3):endidx(3), &
                        startidx(4):endidx(4), &
                        startidx(5):endidx(5)))
      isp_array(:,:,:,:,:) = 0_isp

      RETURN

   END SUBROUTINE alloc5d_isp
#endif

#if __MAX_RANK >= 6
   SUBROUTINE alloc6d_isp(isp_array, insize, instartidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:), POINTER, INTENT(OUT) :: isp_array
      INTEGER, DIMENSION(6), INTENT(IN) :: insize
      INTEGER, DIMENSION(6), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(6) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(isp_array)
      ALLOCATE(isp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2), &
                        startidx(3):endidx(3), &
                        startidx(4):endidx(4), &
                        startidx(5):endidx(5), &
                        startidx(6):endidx(6)))
      isp_array(:,:,:,:,:,:) = 0_isp

      RETURN

   END SUBROUTINE alloc6d_isp
#endif

#if __MAX_RANK >= 7
   SUBROUTINE alloc7d_isp(isp_array, insize, instartidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:), POINTER, INTENT(OUT) :: isp_array
      INTEGER, DIMENSION(7), INTENT(IN) :: insize
      INTEGER, DIMENSION(7), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(7) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(isp_array)
      ALLOCATE(isp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2), &
                        startidx(3):endidx(3), &
                        startidx(4):endidx(4), &
                        startidx(5):endidx(5), &
                        startidx(6):endidx(6), &
                        startidx(7):endidx(7)))
      isp_array(:,:,:,:,:,:,:) = 0_isp

      RETURN

   END SUBROUTINE alloc7d_isp
#endif

#if __MAX_RANK >= 8
   SUBROUTINE alloc8d_isp(isp_array, insize, instartidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:), POINTER, INTENT(OUT) :: isp_array
      INTEGER, DIMENSION(8), INTENT(IN) :: insize
      INTEGER, DIMENSION(8), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(8) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(isp_array)
      ALLOCATE(isp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2), &
                        startidx(3):endidx(3), &
                        startidx(4):endidx(4), &
                        startidx(5):endidx(5), &
                        startidx(6):endidx(6), &
                        startidx(7):endidx(7), &
                        startidx(8):endidx(8)))
      isp_array(:,:,:,:,:,:,:,:) = 0_isp

      RETURN

   END SUBROUTINE alloc8d_isp
#endif

#if __MAX_RANK >= 9
   SUBROUTINE alloc9d_isp(isp_array, insize, instartidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:), POINTER, INTENT(OUT) :: isp_array
      INTEGER, DIMENSION(9), INTENT(IN) :: insize
      INTEGER, DIMENSION(9), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(9) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(isp_array)
      ALLOCATE(isp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2), &
                        startidx(3):endidx(3), &
                        startidx(4):endidx(4), &
                        startidx(5):endidx(5), &
                        startidx(6):endidx(6), &
                        startidx(7):endidx(7), &
                        startidx(8):endidx(8), &
                        startidx(9):endidx(9)))
      isp_array(:,:,:,:,:,:,:,:,:) = 0_isp

      RETURN

   END SUBROUTINE alloc9d_isp
#endif

#if __MAX_RANK >= 10
   SUBROUTINE alloc10d_isp(isp_array, insize, instartidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(OUT) :: isp_array
      INTEGER, DIMENSION(10), INTENT(IN) :: insize
      INTEGER, DIMENSION(10), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(10) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(isp_array)
      ALLOCATE(isp_array(startidx(1):endidx(1), &
                        startidx(2):endidx(2), &
                        startidx(3):endidx(3), &
                        startidx(4):endidx(4), &
                        startidx(5):endidx(5), &
                        startidx(6):endidx(6), &
                        startidx(7):endidx(7), &
                        startidx(8):endidx(8), &
                        startidx(9):endidx(9), &
                        startidx(10):endidx(10)))
      isp_array(:,:,:,:,:,:,:,:,:,:) = 0_isp

      RETURN

   END SUBROUTINE alloc10d_isp
#endif

#if __MAX_RANK >= 11
   SUBROUTINE alloc11d_isp(isp_array, insize, instartidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(OUT) :: isp_array
      INTEGER, DIMENSION(11), INTENT(IN) :: insize
      INTEGER, DIMENSION(11), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(11) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(isp_array)
      ALLOCATE(isp_array(startidx(1):endidx(1), &
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
      isp_array(:,:,:,:,:,:,:,:,:,:,:) = 0_isp

      RETURN

   END SUBROUTINE alloc11d_isp
#endif

#if __MAX_RANK >= 12
   SUBROUTINE alloc12d_isp(isp_array, insize, instartidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(OUT) :: isp_array
      INTEGER, DIMENSION(12), INTENT(IN) :: insize
      INTEGER, DIMENSION(12), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(12) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(isp_array)
      ALLOCATE(isp_array(startidx(1):endidx(1), &
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
      isp_array(:,:,:,:,:,:,:,:,:,:,:,:) = 0_isp

      RETURN

   END SUBROUTINE alloc12d_isp
#endif

#if __MAX_RANK >= 13
   SUBROUTINE alloc13d_isp(isp_array, insize, instartidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(OUT) :: isp_array
      INTEGER, DIMENSION(13), INTENT(IN) :: insize
      INTEGER, DIMENSION(13), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(13) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(isp_array)
      ALLOCATE(isp_array(startidx(1):endidx(1), &
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
      isp_array(:,:,:,:,:,:,:,:,:,:,:,:,:) = 0_isp

      RETURN

   END SUBROUTINE alloc13d_isp
#endif

#if __MAX_RANK >= 14
   SUBROUTINE alloc14d_isp(isp_array, insize, instartidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(OUT) :: isp_array
      INTEGER, DIMENSION(14), INTENT(IN) :: insize
      INTEGER, DIMENSION(14), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(14) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(isp_array)
      ALLOCATE(isp_array(startidx(1):endidx(1), &
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
      isp_array(:,:,:,:,:,:,:,:,:,:,:,:,:,:) = 0_isp

      RETURN

   END SUBROUTINE alloc14d_isp
#endif

#if __MAX_RANK >= 15
   SUBROUTINE alloc15d_isp(isp_array, insize, instartidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(OUT) :: isp_array
      INTEGER, DIMENSION(15), INTENT(IN) :: insize
      INTEGER, DIMENSION(15), INTENT(IN), OPTIONAL :: instartidx
      INTEGER, DIMENSION(15) :: startidx, endidx

      IF (PRESENT(instartidx)) THEN
         startidx(:) = instartidx(:)
      ELSE
         startidx(:) = 1
      END IF
      endidx(:) = insize(:) + startidx(:) - 1

      NULLIFY(isp_array)
      ALLOCATE(isp_array(startidx(1):endidx(1), &
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
      isp_array(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:) = 0_isp

      RETURN

   END SUBROUTINE alloc15d_isp
#endif

#if __MAX_RANK >= 1
   SUBROUTINE realloc1ds_isp(isp_array, newsize, startidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:), POINTER, INTENT(INOUT) :: isp_array
      INTEGER, INTENT(IN) :: newsize
      INTEGER, INTENT(IN), OPTIONAL :: startidx

      INTEGER(KIND=isp), DIMENSION(:), POINTER :: tmparray
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

      IF (ASSOCIATED(isp_array)) THEN
         oldstartidx = LBOUND(isp_array,1)
         oldendidx =   UBOUND(isp_array,1)
         IF ((newstartidx /= oldstartidx).OR.(newendidx /= oldendidx)) THEN
#ifdef __DEBUG_MSG
            WRITE(UNIT=ERROR_UNIT, FMT=*) "Reallocating from [", oldstartidx,",",oldendidx,"]"
            WRITE(UNIT=ERROR_UNIT, FMT=*) "               to [", newstartidx,",",newendidx,"]"
#endif
            ALLOCATE(tmparray(newstartidx:newendidx))
            tmparray(:) = 0_isp
            transstartidx = MAX(oldstartidx,newstartidx)
            transendidx = MIN(oldendidx,newendidx)
            DO i1 = transstartidx, transendidx
               tmparray(i1) = isp_array(i1)
            END DO
            DEALLOCATE(isp_array)
            isp_array => tmparray
         END IF
      ELSE
         ALLOCATE(isp_array(newstartidx: newendidx))
         isp_array(:) = 0_isp
      END IF

      RETURN

   END SUBROUTINE realloc1ds_isp

   SUBROUTINE realloc1d_isp(isp_array, newsize, startidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:), POINTER, INTENT(INOUT) :: isp_array
      INTEGER, DIMENSION(1), INTENT(IN) :: newsize
      INTEGER, DIMENSION(1), INTENT(IN), OPTIONAL :: startidx

      INTEGER(KIND=isp), DIMENSION(:), POINTER :: tmparray
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

      IF (ASSOCIATED(isp_array)) THEN
         oldstartidx = LBOUND(isp_array,1)
         oldendidx =   UBOUND(isp_array,1)
         IF ((ANY(oldstartidx(:) - newstartidx(:) /= 0)).OR. &
             (ANY(oldendidx(:) - newendidx(:) /= 0))) THEN
#ifdef __DEBUG_MSG
            WRITE(UNIT=ERROR_UNIT, FMT=*) "Reallocating from [", oldstartidx,",",oldendidx,"]"
            WRITE(UNIT=ERROR_UNIT, FMT=*) "               to [", newstartidx,",",newendidx,"]"
#endif
            ALLOCATE(tmparray(newstartidx(1):newendidx(1)))
            tmparray(:) = 0_isp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i1 = transstartidx(1), transendidx(1)
               tmparray(i1) = isp_array(i1)
            END DO
            DEALLOCATE(isp_array)
            isp_array => tmparray
         END IF
      ELSE
         ALLOCATE(isp_array(newstartidx(1): newendidx(1)))
         isp_array(:) = 0_isp
      END IF

      RETURN

   END SUBROUTINE realloc1d_isp
#endif

#if __MAX_RANK >= 2
   SUBROUTINE realloc2d_isp(isp_array, newsize, startidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:), POINTER, INTENT(INOUT) :: isp_array
      INTEGER, DIMENSION(2), INTENT(IN) :: newsize
      INTEGER, DIMENSION(2), INTENT(IN), OPTIONAL :: startidx

      INTEGER(KIND=isp), DIMENSION(:,:), POINTER :: tmparray
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

      IF (ASSOCIATED(isp_array)) THEN
         oldstartidx = LBOUND(isp_array)
         oldendidx =   UBOUND(isp_array)
         IF ((ANY(oldstartidx(:) - newstartidx(:) /= 0)).OR. &
             (ANY(oldendidx(:) - newendidx(:) /= 0))) THEN
#ifdef __DEBUG_MSG
            WRITE(UNIT=ERROR_UNIT, FMT=*) "Reallocating from [", oldstartidx,",",oldendidx,"]"
            WRITE(UNIT=ERROR_UNIT, FMT=*) "               to [", newstartidx,",",newendidx,"]"
#endif
            ALLOCATE(tmparray(newstartidx(1):newendidx(1), &
                              newstartidx(2):newendidx(2)))
            tmparray(:,:) = 0_isp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i2 = transstartidx(2), transendidx(2)
               DO i1 = transstartidx(1), transendidx(1)
                  tmparray(i1,i2) = isp_array(i1,i2)
               END DO
            END DO
            DEALLOCATE(isp_array)
            isp_array => tmparray
         END IF
      ELSE
         ALLOCATE(isp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2)))
         isp_array(:,:) = 0_isp
      END IF

      RETURN

   END SUBROUTINE realloc2d_isp
#endif

#if __MAX_RANK >= 3
   SUBROUTINE realloc3d_isp(isp_array, newsize, startidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:), POINTER, INTENT(INOUT) :: isp_array
      INTEGER, DIMENSION(3), INTENT(IN) :: newsize
      INTEGER, DIMENSION(3), INTENT(IN), OPTIONAL :: startidx

      INTEGER(KIND=isp), DIMENSION(:,:,:), POINTER :: tmparray
      INTEGER, DIMENSION(3) :: newstartidx, newendidx
      INTEGER, DIMENSION(3) :: oldstartidx, oldendidx
      INTEGER, DIMENSION(3) :: transstartidx, transendidx
      INTEGER :: i1,i2,i3
      
      NULLIFY(isp_array)

      IF (PRESENT(startidx)) THEN
         newstartidx(:) = startidx(:)
      ELSE
         newstartidx(:) = 1
      END IF
      newendidx(:) = newsize(:) + newstartidx(:) - 1

      IF (ASSOCIATED(isp_array)) THEN
         oldstartidx = LBOUND(isp_array)
         oldendidx =   UBOUND(isp_array)
         IF ((ANY(oldstartidx(:) - newstartidx(:) /= 0)).OR. &
             (ANY(oldendidx(:) - newendidx(:) /= 0))) THEN
#ifdef __DEBUG_MSG
            WRITE(UNIT=ERROR_UNIT, FMT=*) "Reallocating from [", oldstartidx,",",oldendidx,"]"
            WRITE(UNIT=ERROR_UNIT, FMT=*) "               to [", newstartidx,",",newendidx,"]"
#endif
            ALLOCATE(tmparray(newstartidx(1):newendidx(1), &
                              newstartidx(2):newendidx(2), &
                              newstartidx(3):newendidx(3)))
            tmparray(:,:,:) = 0_isp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i3 = transstartidx(3), transendidx(3)
               DO i2 = transstartidx(2), transendidx(2)
                  DO i1 = transstartidx(1), transendidx(1)
                     tmparray(i1,i2,i3) = isp_array(i1,i2,i3)
                  END DO
               END DO
            END DO
            DEALLOCATE(isp_array)
            isp_array => tmparray
         END IF
      ELSE
         ALLOCATE(isp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2), &
                           newstartidx(3):newendidx(3)))
         isp_array(:,:,:) = 0_isp
      END IF

      RETURN

   END SUBROUTINE realloc3d_isp
#endif

#if __MAX_RANK >= 4
   SUBROUTINE realloc4d_isp(isp_array, newsize, startidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:), POINTER, INTENT(INOUT) :: isp_array
      INTEGER, DIMENSION(4), INTENT(IN) :: newsize
      INTEGER, DIMENSION(4), INTENT(IN), OPTIONAL :: startidx

      INTEGER(KIND=isp), DIMENSION(:,:,:,:), POINTER :: tmparray
      INTEGER, DIMENSION(4) :: newstartidx, newendidx
      INTEGER, DIMENSION(4) :: oldstartidx, oldendidx
      INTEGER, DIMENSION(4) :: transstartidx, transendidx
      INTEGER :: i1,i2,i3,i4
      
      NULLIFY(isp_array)

      IF (PRESENT(startidx)) THEN
         newstartidx(:) = startidx(:)
      ELSE
         newstartidx(:) = 1
      END IF
      newendidx(:) = newsize(:) + newstartidx(:) - 1

      IF (ASSOCIATED(isp_array)) THEN
         oldstartidx = LBOUND(isp_array)
         oldendidx =   UBOUND(isp_array)
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
            tmparray(:,:,:,:) = 0_isp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i4 = transstartidx(4), transendidx(4)
               DO i3 = transstartidx(3), transendidx(3)
                  DO i2 = transstartidx(2), transendidx(2)
                     DO i1 = transstartidx(1), transendidx(1)
                        tmparray(i1,i2,i3,i4) = isp_array(i1,i2,i3,i4)
                     END DO
                  END DO
               END DO
            END DO
            DEALLOCATE(isp_array)
            isp_array => tmparray
         END IF
      ELSE
         ALLOCATE(isp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2), &
                           newstartidx(3):newendidx(3), &
                           newstartidx(4):newendidx(4)))
         isp_array(:,:,:,:) = 0_isp
      END IF

      RETURN

   END SUBROUTINE realloc4d_isp
#endif

#if __MAX_RANK >= 5
   SUBROUTINE realloc5d_isp(isp_array, newsize, startidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array
      INTEGER, DIMENSION(5), INTENT(IN) :: newsize
      INTEGER, DIMENSION(5), INTENT(IN), OPTIONAL :: startidx

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:), POINTER :: tmparray
      INTEGER, DIMENSION(5) :: newstartidx, newendidx
      INTEGER, DIMENSION(5) :: oldstartidx, oldendidx
      INTEGER, DIMENSION(5) :: transstartidx, transendidx
      INTEGER :: i1,i2,i3,i4,i5
      
      NULLIFY(isp_array)

      IF (PRESENT(startidx)) THEN
         newstartidx(:) = startidx(:)
      ELSE
         newstartidx(:) = 1
      END IF
      newendidx(:) = newsize(:) + newstartidx(:) - 1

      IF (ASSOCIATED(isp_array)) THEN
         oldstartidx = LBOUND(isp_array)
         oldendidx =   UBOUND(isp_array)
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
            tmparray(:,:,:,:,:) = 0_isp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i5 = transstartidx(5), transendidx(5)
               DO i4 = transstartidx(4), transendidx(4)
                  DO i3 = transstartidx(3), transendidx(3)
                     DO i2 = transstartidx(2), transendidx(2)
                        DO i1 = transstartidx(1), transendidx(1)
                           tmparray(i1,i2,i3,i4,i5) = isp_array(i1,i2,i3,i4,i5)
                        END DO
                     END DO
                  END DO
               END DO
            END DO
            DEALLOCATE(isp_array)
            isp_array => tmparray
         END IF
      ELSE
         ALLOCATE(isp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2), &
                           newstartidx(3):newendidx(3), &
                           newstartidx(4):newendidx(4), &
                           newstartidx(5):newendidx(5)))
         isp_array(:,:,:,:,:) = 0_isp
      END IF

      RETURN

   END SUBROUTINE realloc5d_isp
#endif

#if __MAX_RANK >= 6
   SUBROUTINE realloc6d_isp(isp_array, newsize, startidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array
      INTEGER, DIMENSION(6), INTENT(IN) :: newsize
      INTEGER, DIMENSION(6), INTENT(IN), OPTIONAL :: startidx

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:), POINTER :: tmparray
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

      IF (ASSOCIATED(isp_array)) THEN
         oldstartidx = LBOUND(isp_array)
         oldendidx =   UBOUND(isp_array)
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
            tmparray(:,:,:,:,:,:) = 0_isp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i6 = transstartidx(6), transendidx(6)
               DO i5 = transstartidx(5), transendidx(5)
                  DO i4 = transstartidx(4), transendidx(4)
                     DO i3 = transstartidx(3), transendidx(3)
                        DO i2 = transstartidx(2), transendidx(2)
                           DO i1 = transstartidx(1), transendidx(1)
                              tmparray(i1,i2,i3,i4,i5,i6) = isp_array(i1,i2,i3,i4,i5,i6)
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
            DEALLOCATE(isp_array)
            isp_array => tmparray
         END IF
      ELSE
         ALLOCATE(isp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2), &
                           newstartidx(3):newendidx(3), &
                           newstartidx(4):newendidx(4), &
                           newstartidx(5):newendidx(5), &
                           newstartidx(6):newendidx(6)))
         isp_array(:,:,:,:,:,:) = 0_isp
      END IF

      RETURN

   END SUBROUTINE realloc6d_isp
#endif

#if __MAX_RANK >= 7
   SUBROUTINE realloc7d_isp(isp_array, newsize, startidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array
      INTEGER, DIMENSION(7), INTENT(IN) :: newsize
      INTEGER, DIMENSION(7), INTENT(IN), OPTIONAL :: startidx

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:), POINTER :: tmparray
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

      IF (ASSOCIATED(isp_array)) THEN
         oldstartidx = LBOUND(isp_array)
         oldendidx =   UBOUND(isp_array)
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
            tmparray(:,:,:,:,:,:,:) = 0_isp
            transstartidx(:) = MAX(oldstartidx(:),newstartidx(:))
            transendidx(:) = MIN(oldendidx(:),newendidx(:))
            DO i7 = transstartidx(7), transendidx(7)
               DO i6 = transstartidx(6), transendidx(6)
                  DO i5 = transstartidx(5), transendidx(5)
                     DO i4 = transstartidx(4), transendidx(4)
                        DO i3 = transstartidx(3), transendidx(3)
                           DO i2 = transstartidx(2), transendidx(2)
                              DO i1 = transstartidx(1), transendidx(1)
                                 tmparray(i1,i2,i3,i4,i5,i6,i7) = isp_array(i1,i2,i3,i4,i5,i6,i7)
                              END DO
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
            DEALLOCATE(isp_array)
            isp_array => tmparray
         END IF
      ELSE
         ALLOCATE(isp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2), &
                           newstartidx(3):newendidx(3), &
                           newstartidx(4):newendidx(4), &
                           newstartidx(5):newendidx(5), &
                           newstartidx(6):newendidx(6), &
                           newstartidx(7):newendidx(7)))
         isp_array(:,:,:,:,:,:,:) = 0_isp
      END IF

      RETURN

   END SUBROUTINE realloc7d_isp
#endif

#if __MAX_RANK >= 8
   SUBROUTINE realloc8d_isp(isp_array, newsize, startidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array
      INTEGER, DIMENSION(8), INTENT(IN) :: newsize
      INTEGER, DIMENSION(8), INTENT(IN), OPTIONAL :: startidx

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:), POINTER :: tmparray
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

      IF (ASSOCIATED(isp_array)) THEN
         oldstartidx = LBOUND(isp_array)
         oldendidx =   UBOUND(isp_array)
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
            tmparray(:,:,:,:,:,:,:,:) = 0_isp
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
                                    tmparray(i1,i2,i3,i4,i5,i6,i7,i8) = isp_array(i1,i2,i3,i4,i5,i6,i7,i8)
                                 END DO
                              END DO
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
            DEALLOCATE(isp_array)
            isp_array => tmparray
         END IF
      ELSE
         ALLOCATE(isp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2), &
                           newstartidx(3):newendidx(3), &
                           newstartidx(4):newendidx(4), &
                           newstartidx(5):newendidx(5), &
                           newstartidx(6):newendidx(6), &
                           newstartidx(7):newendidx(7), &
                           newstartidx(8):newendidx(8)))
         isp_array(:,:,:,:,:,:,:,:) = 0_isp
      END IF

      RETURN

   END SUBROUTINE realloc8d_isp
#endif

#if __MAX_RANK >= 9
   SUBROUTINE realloc9d_isp(isp_array, newsize, startidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array
      INTEGER, DIMENSION(9), INTENT(IN) :: newsize
      INTEGER, DIMENSION(9), INTENT(IN), OPTIONAL :: startidx

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:), POINTER :: tmparray
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

      IF (ASSOCIATED(isp_array)) THEN
         oldstartidx = LBOUND(isp_array)
         oldendidx =   UBOUND(isp_array)
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
            tmparray(:,:,:,:,:,:,:,:,:) = 0_isp
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
                                       tmparray(i1,i2,i3,i4,i5,i6,i7,i8,i9) = isp_array(i1,i2,i3,i4,i5,i6,i7,i8,i9)
                                    END DO
                                 END DO
                              END DO
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
            DEALLOCATE(isp_array)
            isp_array => tmparray
         END IF
      ELSE
         ALLOCATE(isp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2), &
                           newstartidx(3):newendidx(3), &
                           newstartidx(4):newendidx(4), &
                           newstartidx(5):newendidx(5), &
                           newstartidx(6):newendidx(6), &
                           newstartidx(7):newendidx(7), &
                           newstartidx(8):newendidx(8), &
                           newstartidx(9):newendidx(9)))
         isp_array(:,:,:,:,:,:,:,:,:) = 0_isp
      END IF

      RETURN

   END SUBROUTINE realloc9d_isp
#endif

#if __MAX_RANK >= 10
   SUBROUTINE realloc10d_isp(isp_array, newsize, startidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array
      INTEGER, DIMENSION(10), INTENT(IN) :: newsize
      INTEGER, DIMENSION(10), INTENT(IN), OPTIONAL :: startidx

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:), POINTER :: tmparray
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

      IF (ASSOCIATED(isp_array)) THEN
         oldstartidx = LBOUND(isp_array)
         oldendidx =   UBOUND(isp_array)
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
            tmparray(:,:,:,:,:,:,:,:,:,:) = 0_isp
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
                                             isp_array(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
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
            DEALLOCATE(isp_array)
            isp_array => tmparray
         END IF
      ELSE
         ALLOCATE(isp_array(newstartidx(1):newendidx(1), &
                           newstartidx(2):newendidx(2), &
                           newstartidx(3):newendidx(3), &
                           newstartidx(4):newendidx(4), &
                           newstartidx(5):newendidx(5), &
                           newstartidx(6):newendidx(6), &
                           newstartidx(7):newendidx(7), &
                           newstartidx(8):newendidx(8), &
                           newstartidx(9):newendidx(9), &
                           newstartidx(10):newendidx(10)))
         isp_array(:,:,:,:,:,:,:,:,:,:) = 0_isp
      END IF

      RETURN

   END SUBROUTINE realloc10d_isp
#endif

#if __MAX_RANK >= 11
   SUBROUTINE realloc11d_isp(isp_array, newsize, startidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array
      INTEGER, DIMENSION(11), INTENT(IN) :: newsize
      INTEGER, DIMENSION(11), INTENT(IN), OPTIONAL :: startidx

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:), POINTER :: tmparray
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

      IF (ASSOCIATED(isp_array)) THEN
         oldstartidx = LBOUND(isp_array)
         oldendidx =   UBOUND(isp_array)
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
            tmparray(:,:,:,:,:,:,:,:,:,:,:) = 0_isp
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
                                                isp_array(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
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
            DEALLOCATE(isp_array)
            isp_array => tmparray
         END IF
      ELSE
         ALLOCATE(isp_array(newstartidx(1):newendidx(1), &
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
         isp_array(:,:,:,:,:,:,:,:,:,:,:) = 0_isp
      END IF

      RETURN

   END SUBROUTINE realloc11d_isp
#endif

#if __MAX_RANK >= 12
   SUBROUTINE realloc12d_isp(isp_array, newsize, startidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array
      INTEGER, DIMENSION(12), INTENT(IN) :: newsize
      INTEGER, DIMENSION(12), INTENT(IN), OPTIONAL :: startidx

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:), POINTER :: tmparray
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

      IF (ASSOCIATED(isp_array)) THEN
         oldstartidx = LBOUND(isp_array)
         oldendidx =   UBOUND(isp_array)
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
            tmparray(:,:,:,:,:,:,:,:,:,:,:,:) = 0_isp
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
                                                   isp_array(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
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
            DEALLOCATE(isp_array)
            isp_array => tmparray
         END IF
      ELSE
         ALLOCATE(isp_array(newstartidx(1):newendidx(1), &
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
         isp_array(:,:,:,:,:,:,:,:,:,:,:,:) = 0_isp
      END IF

      RETURN

   END SUBROUTINE realloc12d_isp
#endif

#if __MAX_RANK >= 13
   SUBROUTINE realloc13d_isp(isp_array, newsize, startidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array
      INTEGER, DIMENSION(13), INTENT(IN) :: newsize
      INTEGER, DIMENSION(13), INTENT(IN), OPTIONAL :: startidx

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER :: tmparray
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

      IF (ASSOCIATED(isp_array)) THEN
         oldstartidx = LBOUND(isp_array)
         oldendidx =   UBOUND(isp_array)
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
            tmparray(:,:,:,:,:,:,:,:,:,:,:,:,:) = 0_isp
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
                                                      isp_array(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
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
            DEALLOCATE(isp_array)
            isp_array => tmparray
         END IF
      ELSE
         ALLOCATE(isp_array(newstartidx(1):newendidx(1), &
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
         isp_array(:,:,:,:,:,:,:,:,:,:,:,:,:) = 0_isp
      END IF

      RETURN

   END SUBROUTINE realloc13d_isp
#endif

#if __MAX_RANK >= 14
   SUBROUTINE realloc14d_isp(isp_array, newsize, startidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array
      INTEGER, DIMENSION(14), INTENT(IN) :: newsize
      INTEGER, DIMENSION(14), INTENT(IN), OPTIONAL :: startidx

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER :: tmparray
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

      IF (ASSOCIATED(isp_array)) THEN
         oldstartidx = LBOUND(isp_array)
         oldendidx =   UBOUND(isp_array)
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
            tmparray(:,:,:,:,:,:,:,:,:,:,:,:,:,:) = 0_isp
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
                                                         isp_array(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
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
            DEALLOCATE(isp_array)
            isp_array => tmparray
         END IF
      ELSE
         ALLOCATE(isp_array(newstartidx(1):newendidx(1), &
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
         isp_array(:,:,:,:,:,:,:,:,:,:,:,:,:,:) = 0_isp
      END IF

      RETURN

   END SUBROUTINE realloc14d_isp
#endif

#if __MAX_RANK >= 15
   SUBROUTINE realloc15d_isp(isp_array, newsize, startidx)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array
      INTEGER, DIMENSION(15), INTENT(IN) :: newsize
      INTEGER, DIMENSION(15), INTENT(IN), OPTIONAL :: startidx

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER :: tmparray
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

      IF (ASSOCIATED(isp_array)) THEN
         oldstartidx = LBOUND(isp_array)
         oldendidx =   UBOUND(isp_array)
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
            tmparray(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:) = 0_isp
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
                                                            isp_array(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15)
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
            DEALLOCATE(isp_array)
            isp_array => tmparray
         END IF
      ELSE
         ALLOCATE(isp_array(newstartidx(1):newendidx(1), &
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
         isp_array(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:) = 0_isp
      END IF

      RETURN

   END SUBROUTINE realloc15d_isp
#endif

#if __MAX_RANK >= 1
   SUBROUTINE dealloc1d_isp(isp_array)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:), POINTER, INTENT(INOUT) :: isp_array

      IF (ASSOCIATED(isp_array)) THEN
         DEALLOCATE(isp_array)
         NULLIFY(isp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc1d_isp
#endif

#if __MAX_RANK >= 2
   SUBROUTINE dealloc2d_isp(isp_array)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:), POINTER, INTENT(INOUT) :: isp_array

      IF (ASSOCIATED(isp_array)) THEN
         DEALLOCATE(isp_array)
         NULLIFY(isp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc2d_isp
#endif

#if __MAX_RANK >= 3
   SUBROUTINE dealloc3d_isp(isp_array)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:), POINTER, INTENT(INOUT) :: isp_array

      IF (ASSOCIATED(isp_array)) THEN
         DEALLOCATE(isp_array)
         NULLIFY(isp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc3d_isp
#endif

#if __MAX_RANK >= 4
   SUBROUTINE dealloc4d_isp(isp_array)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:), POINTER, INTENT(INOUT) :: isp_array

      IF (ASSOCIATED(isp_array)) THEN
         DEALLOCATE(isp_array)
         NULLIFY(isp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc4d_isp
#endif

#if __MAX_RANK >= 5
   SUBROUTINE dealloc5d_isp(isp_array)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array

      IF (ASSOCIATED(isp_array)) THEN
         DEALLOCATE(isp_array)
         NULLIFY(isp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc5d_isp
#endif

#if __MAX_RANK >= 6
   SUBROUTINE dealloc6d_isp(isp_array)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array

      IF (ASSOCIATED(isp_array)) THEN
         DEALLOCATE(isp_array)
         NULLIFY(isp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc6d_isp
#endif

#if __MAX_RANK >= 7
   SUBROUTINE dealloc7d_isp(isp_array)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array

      IF (ASSOCIATED(isp_array)) THEN
         DEALLOCATE(isp_array)
         NULLIFY(isp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc7d_isp
#endif

#if __MAX_RANK >= 8
   SUBROUTINE dealloc8d_isp(isp_array)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array

      IF (ASSOCIATED(isp_array)) THEN
         DEALLOCATE(isp_array)
         NULLIFY(isp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc8d_isp
#endif

#if __MAX_RANK >= 9
   SUBROUTINE dealloc9d_isp(isp_array)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array

      IF (ASSOCIATED(isp_array)) THEN
         DEALLOCATE(isp_array)
         NULLIFY(isp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc9d_isp
#endif

#if __MAX_RANK >= 10
   SUBROUTINE dealloc10d_isp(isp_array)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array

      IF (ASSOCIATED(isp_array)) THEN
         DEALLOCATE(isp_array)
         NULLIFY(isp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc10d_isp
#endif

#if __MAX_RANK >= 11
   SUBROUTINE dealloc11d_isp(isp_array)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array

      IF (ASSOCIATED(isp_array)) THEN
         DEALLOCATE(isp_array)
         NULLIFY(isp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc11d_isp
#endif

#if __MAX_RANK >= 12
   SUBROUTINE dealloc12d_isp(isp_array)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array

      IF (ASSOCIATED(isp_array)) THEN
         DEALLOCATE(isp_array)
         NULLIFY(isp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc12d_isp
#endif

#if __MAX_RANK >= 13
   SUBROUTINE dealloc13d_isp(isp_array)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array

      IF (ASSOCIATED(isp_array)) THEN
         DEALLOCATE(isp_array)
         NULLIFY(isp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc13d_isp
#endif

#if __MAX_RANK >= 14
   SUBROUTINE dealloc14d_isp(isp_array)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array

      IF (ASSOCIATED(isp_array)) THEN
         DEALLOCATE(isp_array)
         NULLIFY(isp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc14d_isp
#endif

#if __MAX_RANK >= 15
   SUBROUTINE dealloc15d_isp(isp_array)

      IMPLICIT NONE

      INTEGER(KIND=isp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER, INTENT(INOUT) :: isp_array

      IF (ASSOCIATED(isp_array)) THEN
         DEALLOCATE(isp_array)
         NULLIFY(isp_array)
      END IF

      RETURN

   END SUBROUTINE dealloc15d_isp
#endif

END MODULE re_de_allocisp
