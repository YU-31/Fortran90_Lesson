  !ex2_2_1 calculation of averages
PROGRAM m2_2_1

  IMPLICIT NONE
  REAL :: a, b, avg1, avg2, avg3

  PRINT *, 'Input a & b'
  READ *, a, b

  avg1 = (a + b) / 2.0            !Arighmetic mean
  avg2 = (a * b) ** 0.5           !Geometric mean
  avg3 = 2.0 / (1.0 / a + 1.0 /b) !Harmonic mean

  PRINT *, avg1, avg2, avg3

END PROGRAM m2_2_1
