PROGRAM test_re_de_alloc_csp

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   USE kinds, ONLY : csp
   USE re_de_alloc, ONLY : alloc, &
                      realloc, &
                      dealloc

   IMPLICIT NONE

   CHARACTER(LEN=1024) :: cmdname
   
   INTEGER, DIMENSION(15) :: fstsize     = [3,2, 4, 6,  2, 1, 5, 3,  2,1, 3, 2,1,3, 2]
   INTEGER, DIMENSION(15) :: fststartidx = [2,5,-8,47,-52,32,-9,58,-52,3,69, 0,4,5,-6] 
   INTEGER, DIMENSION(15) :: fstendidx   = [4,6,-5,52,-51,32,-5,60,-51,3,71, 1,4,7,-5] 

   INTEGER, DIMENSION(15) :: sndsize     = [4,2, 4, 6,  2, 1, 4, 1,  3,6, 3, 2,1,3, 2]
   INTEGER, DIMENSION(15) :: sndstartidx = [1,5,-8,47,-52,32,-9,15,-52,3, 9, 0,4,5,-6] 
   INTEGER, DIMENSION(15) :: sndendidx   = [4,6,-5,52,-51,32,-6,15,-50,8,11, 1,4,7,-5] 

#if __MAX_RANK >= 1
   COMPLEX(KIND=csp), DIMENSION(:), POINTER :: array_1d
#if __MAX_RANK >= 2
   COMPLEX(KIND=csp), DIMENSION(:,:), POINTER :: array_2d
#if __MAX_RANK >= 3
   COMPLEX(KIND=csp), DIMENSION(:,:,:), POINTER :: array_3d
#if __MAX_RANK >= 4
   COMPLEX(KIND=csp), DIMENSION(:,:,:,:), POINTER :: array_4d
#if __MAX_RANK >= 5
   COMPLEX(KIND=csp), DIMENSION(:,:,:,:,:), POINTER :: array_5d
#if __MAX_RANK >= 6
   COMPLEX(KIND=csp), DIMENSION(:,:,:,:,:,:), POINTER :: array_6d
#if __MAX_RANK >= 7
   COMPLEX(KIND=csp), DIMENSION(:,:,:,:,:,:,:), POINTER :: array_7d
#if __MAX_RANK >= 8
   COMPLEX(KIND=csp), DIMENSION(:,:,:,:,:,:,:,:), POINTER :: array_8d
#if __MAX_RANK >= 9
   COMPLEX(KIND=csp), DIMENSION(:,:,:,:,:,:,:,:,:), POINTER :: array_9d
#if __MAX_RANK >= 10
   COMPLEX(KIND=csp), DIMENSION(:,:,:,:,:,:,:,:,:,:), POINTER :: array_10d
#if __MAX_RANK >= 11
   COMPLEX(KIND=csp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:), POINTER :: array_11d
#if __MAX_RANK >= 12
   COMPLEX(KIND=csp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:), POINTER :: array_12d
#if __MAX_RANK >= 13
   COMPLEX(KIND=csp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER :: array_13d
#if __MAX_RANK >= 14
   COMPLEX(KIND=csp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER :: array_14d
#if __MAX_RANK >= 15
   COMPLEX(KIND=csp), DIMENSION(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:), POINTER :: array_15d
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif

   INTEGER :: n
   INTEGER :: idim

   LOGICAL :: passed

   CALL GET_COMMAND_ARGUMENT(0, cmdname)

   passed = .TRUE.

#if __MAX_RANK >= 1
   ! 1D array (scalar)
   IF (passed) THEN
      n = 1
      NULLIFY(array_1d)
      CALL alloc(array_1d, fstsize(1), fststartidx(1))

      DO idim = 1, n
         passed = passed .AND. (fstsize(idim) == SIZE(array_1d, idim))
         passed = passed .AND. (fststartidx(idim) == LBOUND(array_1d,idim))
         passed = passed .AND. (fstendidx(idim) == UBOUND(array_1d,idim))
      END DO
      
      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine alloc1d_csp of module re_de_alloc"
      END IF

      CALL realloc(array_1d, sndsize(1), sndstartidx(1))
      DO idim = 1, n
         passed = passed .AND. (sndsize(idim) == SIZE(array_1d, idim))
         passed = passed .AND. (sndstartidx(idim) == LBOUND(array_1d,idim))
         passed = passed .AND. (sndendidx(idim) == UBOUND(array_1d,idim))
      END DO

      CALL dealloc(array_1d)
      
      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine realloc1d_csp of module re_de_alloc"
      END IF
   END IF

   ! 1D array
   IF (passed) THEN
      n = 1
      NULLIFY(array_1d)
      CALL alloc(array_1d, fstsize(1:n), fststartidx(1:n))
      
      DO idim = 1, n
         passed = passed .AND. (fstsize(idim) == SIZE(array_1d, idim))
         passed = passed .AND. (fststartidx(idim) == LBOUND(array_1d,idim))
         passed = passed .AND. (fstendidx(idim) == UBOUND(array_1d,idim))
      END DO
      
      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine alloc1d_csp of module re_de_alloc"
      END IF
      
      CALL realloc(array_1d, sndsize(1:n), sndstartidx(1:n))
      DO idim = 1, n
         passed = passed .AND. (sndsize(idim) == SIZE(array_1d, idim))
         passed = passed .AND. (sndstartidx(idim) == LBOUND(array_1d,idim))
         passed = passed .AND. (sndendidx(idim) == UBOUND(array_1d,idim))
      END DO
      
      CALL dealloc(array_1d)
      
      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine realloc1d_csp of module re_de_alloc"
      END IF
   END IF

#if __MAX_RANK >= 2
   ! 2D array
   IF (passed) THEN
      n = 2
      NULLIFY(array_2d)
      CALL alloc(array_2d, fstsize(1:n), fststartidx(1:n))

      DO idim = 1, n
         passed = passed .AND. (fstsize(idim) == SIZE(array_2d, idim))
         passed = passed .AND. (fststartidx(idim) == LBOUND(array_2d,idim))
         passed = passed .AND. (fstendidx(idim) == UBOUND(array_2d,idim))
      END DO
      
      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine alloc2d_csp of module re_de_alloc"
      END IF

      CALL realloc(array_2d, sndsize(1:n), sndstartidx(1:n))
      DO idim = 1, n
         passed = passed .AND. (sndsize(idim) == SIZE(array_2d, idim))
         passed = passed .AND. (sndstartidx(idim) == LBOUND(array_2d,idim))
         passed = passed .AND. (sndendidx(idim) == UBOUND(array_2d,idim))
      END DO

      CALL dealloc(array_2d)
      
      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine realloc2d_csp of module re_de_alloc"
      END IF
   END IF

#if __MAX_RANK >= 3
   ! 3D array
   IF (passed) THEN
      n = 3
      NULLIFY(array_3d)
      CALL alloc(array_3d, fstsize(1:n), fststartidx(1:n))

      DO idim = 1, n
         passed = passed .AND. (fstsize(idim) == SIZE(array_3d, idim))
         passed = passed .AND. (fststartidx(idim) == LBOUND(array_3d,idim))
         passed = passed .AND. (fstendidx(idim) == UBOUND(array_3d,idim))
      END DO
      
      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine alloc3d_csp of module re_de_alloc"
      END IF

      CALL realloc(array_3d, sndsize(1:n), sndstartidx(1:n))
      DO idim = 1, n
         passed = passed .AND. (sndsize(idim) == SIZE(array_3d, idim))
         passed = passed .AND. (sndstartidx(idim) == LBOUND(array_3d,idim))
         passed = passed .AND. (sndendidx(idim) == UBOUND(array_3d,idim))
      END DO

      CALL dealloc(array_3d)
      
      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine realloc3d_csp of module re_de_alloc"
      END IF
   END IF

#if __MAX_RANK >= 4
   ! 4D array
   IF (passed) THEN
      n = 4
      NULLIFY(array_4d)
      CALL alloc(array_4d, fstsize(1:n), fststartidx(1:n))

      DO idim = 1, n
         passed = passed .AND. (fstsize(idim) == SIZE(array_4d, idim))
         passed = passed .AND. (fststartidx(idim) == LBOUND(array_4d,idim))
         passed = passed .AND. (fstendidx(idim) == UBOUND(array_4d,idim))
      END DO

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine alloc4d_csp of module re_de_alloc"
      END IF

      CALL realloc(array_4d, sndsize(1:n), sndstartidx(1:n))
      DO idim = 1, n
         passed = passed .AND. (sndsize(idim) == SIZE(array_4d, idim))
         passed = passed .AND. (sndstartidx(idim) == LBOUND(array_4d,idim))
         passed = passed .AND. (sndendidx(idim) == UBOUND(array_4d,idim))
      END DO

      CALL dealloc(array_4d)

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine realloc4d_csp of module re_de_alloc"
      END IF
   END IF

#if __MAX_RANK >= 5
   ! 5D array
   IF (passed) THEN
      n = 5
      NULLIFY(array_5d)
      CALL alloc(array_5d, fstsize(1:n), fststartidx(1:n))

      DO idim = 1, n
         passed = passed .AND. (fstsize(idim) == SIZE(array_5d, idim))
         passed = passed .AND. (fststartidx(idim) == LBOUND(array_5d,idim))
         passed = passed .AND. (fstendidx(idim) == UBOUND(array_5d,idim))
      END DO

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine alloc5d_csp of module re_de_alloc"
      END IF

      CALL realloc(array_5d, sndsize(1:n), sndstartidx(1:n))
      DO idim = 1, n
         passed = passed .AND. (sndsize(idim) == SIZE(array_5d, idim))
         passed = passed .AND. (sndstartidx(idim) == LBOUND(array_5d,idim))
         passed = passed .AND. (sndendidx(idim) == UBOUND(array_5d,idim))
      END DO

      CALL dealloc(array_5d)

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine realloc5d_csp of module re_de_alloc"
      END IF
   END IF

#if __MAX_RANK >= 6
   ! 6D array
   IF (passed) THEN
      n = 6
      NULLIFY(array_6d)
      CALL alloc(array_6d, fstsize(1:n), fststartidx(1:n))

      DO idim = 1, n
         passed = passed .AND. (fstsize(idim) == SIZE(array_6d, idim))
         passed = passed .AND. (fststartidx(idim) == LBOUND(array_6d,idim))
         passed = passed .AND. (fstendidx(idim) == UBOUND(array_6d,idim))
      END DO

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine alloc6d_csp of module re_de_alloc"
      END IF

      CALL realloc(array_6d, sndsize(1:n), sndstartidx(1:n))
      DO idim = 1, n
         passed = passed .AND. (sndsize(idim) == SIZE(array_6d, idim))
         passed = passed .AND. (sndstartidx(idim) == LBOUND(array_6d,idim))
         passed = passed .AND. (sndendidx(idim) == UBOUND(array_6d,idim))
      END DO

      CALL dealloc(array_6d)

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine realloc6d_csp of module re_de_alloc"
      END IF
   END IF

#if __MAX_RANK >= 7
   ! 7D array
   IF (passed) THEN
      n = 7
      NULLIFY(array_7d)
      CALL alloc(array_7d, fstsize(1:n), fststartidx(1:n))

      DO idim = 1, n
         passed = passed .AND. (fstsize(idim) == SIZE(array_7d, idim))
         passed = passed .AND. (fststartidx(idim) == LBOUND(array_7d,idim))
         passed = passed .AND. (fstendidx(idim) == UBOUND(array_7d,idim))
      END DO

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine alloc7d_csp of module re_de_alloc"
      END IF

      CALL realloc(array_7d, sndsize(1:n), sndstartidx(1:n))
      DO idim = 1, n
         passed = passed .AND. (sndsize(idim) == SIZE(array_7d, idim))
         passed = passed .AND. (sndstartidx(idim) == LBOUND(array_7d,idim))
         passed = passed .AND. (sndendidx(idim) == UBOUND(array_7d,idim))
      END DO

      CALL dealloc(array_7d)

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine realloc7d_csp of module re_de_alloc"
      END IF
   END IF

#if __MAX_RANK >= 8
   ! 8D array
   IF (passed) THEN
      n = 8
      NULLIFY(array_8d)
      CALL alloc(array_8d, fstsize(1:n), fststartidx(1:n))

      DO idim = 1, n
         passed = passed .AND. (fstsize(idim) == SIZE(array_8d, idim))
         passed = passed .AND. (fststartidx(idim) == LBOUND(array_8d,idim))
         passed = passed .AND. (fstendidx(idim) == UBOUND(array_8d,idim))
      END DO

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine alloc8d_csp of module re_de_alloc"
      END IF

      CALL realloc(array_8d, sndsize(1:n), sndstartidx(1:n))
      DO idim = 1, n
         passed = passed .AND. (sndsize(idim) == SIZE(array_8d, idim))
         passed = passed .AND. (sndstartidx(idim) == LBOUND(array_8d,idim))
         passed = passed .AND. (sndendidx(idim) == UBOUND(array_8d,idim))
      END DO

      CALL dealloc(array_8d)

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine realloc8d_csp of module re_de_alloc"
      END IF
   END IF

#if __MAX_RANK >= 9
   ! 9D array
   IF (passed) THEN
      n = 9
      NULLIFY(array_9d)
      CALL alloc(array_9d, fstsize(1:n), fststartidx(1:n))

      DO idim = 1, n
         passed = passed .AND. (fstsize(idim) == SIZE(array_9d, idim))
         passed = passed .AND. (fststartidx(idim) == LBOUND(array_9d,idim))
         passed = passed .AND. (fstendidx(idim) == UBOUND(array_9d,idim))
      END DO

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine alloc9d_csp of module re_de_alloc"
      END IF

      CALL realloc(array_9d, sndsize(1:n), sndstartidx(1:n))
      DO idim = 1, n
         passed = passed .AND. (sndsize(idim) == SIZE(array_9d, idim))
         passed = passed .AND. (sndstartidx(idim) == LBOUND(array_9d,idim))
         passed = passed .AND. (sndendidx(idim) == UBOUND(array_9d,idim))
      END DO

      CALL dealloc(array_9d)

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine realloc9d_csp of module re_de_alloc"
      END IF
   END IF

#if __MAX_RANK >= 10
   ! 10D array
   IF (passed) THEN
      n = 10
      NULLIFY(array_10d)
      CALL alloc(array_10d, fstsize(1:n), fststartidx(1:n))

      DO idim = 1, n
         passed = passed .AND. (fstsize(idim) == SIZE(array_10d, idim))
         passed = passed .AND. (fststartidx(idim) == LBOUND(array_10d,idim))
         passed = passed .AND. (fstendidx(idim) == UBOUND(array_10d,idim))
      END DO

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine alloc10d_csp of module re_de_alloc"
      END IF

      CALL realloc(array_10d, sndsize(1:n), sndstartidx(1:n))
      DO idim = 1, n
         passed = passed .AND. (sndsize(idim) == SIZE(array_10d, idim))
         passed = passed .AND. (sndstartidx(idim) == LBOUND(array_10d,idim))
         passed = passed .AND. (sndendidx(idim) == UBOUND(array_10d,idim))
      END DO

      CALL dealloc(array_10d)

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine realloc10d_csp of module re_de_alloc"
      END IF
   END IF

#if __MAX_RANK >= 11
   ! 11D array
   IF (passed) THEN
      n = 11
      NULLIFY(array_11d)
      CALL alloc(array_11d, fstsize(1:n), fststartidx(1:n))

      DO idim = 1, n
         passed = passed .AND. (fstsize(idim) == SIZE(array_11d, idim))
         passed = passed .AND. (fststartidx(idim) == LBOUND(array_11d,idim))
         passed = passed .AND. (fstendidx(idim) == UBOUND(array_11d,idim))
      END DO

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine alloc11d_csp of module re_de_alloc"
      END IF

      CALL realloc(array_11d, sndsize(1:n), sndstartidx(1:n))
      DO idim = 1, n
         passed = passed .AND. (sndsize(idim) == SIZE(array_11d, idim))
         passed = passed .AND. (sndstartidx(idim) == LBOUND(array_11d,idim))
         passed = passed .AND. (sndendidx(idim) == UBOUND(array_11d,idim))
      END DO

      CALL dealloc(array_11d)

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine realloc11d_csp of module re_de_alloc"
      END IF
   END IF

#if __MAX_RANK >= 12
   ! 12D array
   IF (passed) THEN
      n = 12
      NULLIFY(array_12d)
      CALL alloc(array_12d, fstsize(1:n), fststartidx(1:n))

      DO idim = 1, n
         passed = passed .AND. (fstsize(idim) == SIZE(array_12d, idim))
         passed = passed .AND. (fststartidx(idim) == LBOUND(array_12d,idim))
         passed = passed .AND. (fstendidx(idim) == UBOUND(array_12d,idim))
      END DO

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine alloc12d_csp of module re_de_alloc"
      END IF

      CALL realloc(array_12d, sndsize(1:n), sndstartidx(1:n))
      DO idim = 1, n
         passed = passed .AND. (sndsize(idim) == SIZE(array_12d, idim))
         passed = passed .AND. (sndstartidx(idim) == LBOUND(array_12d,idim))
         passed = passed .AND. (sndendidx(idim) == UBOUND(array_12d,idim))
      END DO

      CALL dealloc(array_12d)

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine realloc12d_csp of module re_de_alloc"
      END IF
   END IF

#if __MAX_RANK >= 13
   ! 13D array
   IF (passed) THEN
      n = 13
      NULLIFY(array_13d)
      CALL alloc(array_13d, fstsize(1:n), fststartidx(1:n))

      DO idim = 1, n
         passed = passed .AND. (fstsize(idim) == SIZE(array_13d, idim))
         passed = passed .AND. (fststartidx(idim) == LBOUND(array_13d,idim))
         passed = passed .AND. (fstendidx(idim) == UBOUND(array_13d,idim))
      END DO

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine alloc13d_csp of module re_de_alloc"
      END IF

      CALL realloc(array_13d, sndsize(1:n), sndstartidx(1:n))
      DO idim = 1, n
         passed = passed .AND. (sndsize(idim) == SIZE(array_13d, idim))
         passed = passed .AND. (sndstartidx(idim) == LBOUND(array_13d,idim))
         passed = passed .AND. (sndendidx(idim) == UBOUND(array_13d,idim))
      END DO

      CALL dealloc(array_13d)

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine realloc13d_csp of module re_de_alloc"
      END IF
   END IF

#if __MAX_RANK >= 14
   ! 14D array
   IF (passed) THEN
      n = 14
      NULLIFY(array_14d)
      CALL alloc(array_14d, fstsize(1:n), fststartidx(1:n))

      DO idim = 1, n
         passed = passed .AND. (fstsize(idim) == SIZE(array_14d, idim))
         passed = passed .AND. (fststartidx(idim) == LBOUND(array_14d,idim))
         passed = passed .AND. (fstendidx(idim) == UBOUND(array_14d,idim))
      END DO

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine alloc14d_csp of module re_de_alloc"
      END IF

      CALL realloc(array_14d, sndsize(1:n), sndstartidx(1:n))
      DO idim = 1, n
         passed = passed .AND. (sndsize(idim) == SIZE(array_14d, idim))
         passed = passed .AND. (sndstartidx(idim) == LBOUND(array_14d,idim))
         passed = passed .AND. (sndendidx(idim) == UBOUND(array_14d,idim))
      END DO

      CALL dealloc(array_14d)

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine realloc14d_csp of module re_de_alloc"
      END IF
   END IF

#if __MAX_RANK >= 15
   ! 15D array
   IF (passed) THEN
      n = 15
      NULLIFY(array_15d)
      CALL alloc(array_15d, fstsize(1:n), fststartidx(1:n))

      DO idim = 1, n
         passed = passed .AND. (fstsize(idim) == SIZE(array_15d, idim))
         passed = passed .AND. (fststartidx(idim) == LBOUND(array_15d,idim))
         passed = passed .AND. (fstendidx(idim) == UBOUND(array_15d,idim))
      END DO

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine alloc15d_csp of module re_de_alloc"
      END IF

      CALL realloc(array_15d, sndsize(1:n), sndstartidx(1:n))
      DO idim = 1, n
         passed = passed .AND. (sndsize(idim) == SIZE(array_15d, idim))
         passed = passed .AND. (sndstartidx(idim) == LBOUND(array_15d,idim))
         passed = passed .AND. (sndendidx(idim) == UBOUND(array_15d,idim))
      END DO

      CALL dealloc(array_15d)

      IF (.NOT.passed) THEN
         WRITE(UNIT=OUTPUT_UNIT, FMT=*) "Fail in routine realloc15d_csp of module re_de_alloc"
      END IF
   END IF
   
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
   IF (passed) THEN
      WRITE(UNIT=OUTPUT_UNIT, FMT="(A70,2X,A8)") ADJUSTL(cmdname), "[PASSED]"
   ELSE
      WRITE(UNIT=OUTPUT_UNIT, FMT="(A70,2X,A8)") ADJUSTL(cmdname), "  [FAIL]"
   END IF
   
END PROGRAM test_re_de_alloc_csp
