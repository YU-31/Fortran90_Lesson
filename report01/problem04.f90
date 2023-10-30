!Find the n-squared roots of 1.
PROGRAM problem04

        !Initialize variables
        IMPLICIT NONE
        INTEGER :: n, k
        REAL, PARAMETER :: pi_single = 3.141592e0               !Single precision
        REAL(8), PARAMETER :: pi_double = 3.14159265358979d0    !Double precision
        COMPLEX :: a_k0                                 !Single precision
        COMPLEX(kind(0d0)) :: a_k1                      !Double precision
        COMPLEX, PARAMETER :: iunit_single = (0e0, 1e0) !Imaginary unit, Single precision
        COMPLEX(kind(0d0)), PARAMETER :: iunit_double = (0d0, 1d0)      !Double precision  
        
        !Input n
        PRINT *, 'Input an positive integer n. Then, find n-squared roots of 1'
        READ *, n

        !Determine if n is positive
        IF (n < 0) THEN
                PRINT *, 'Invalid value: n.'
        !Calculation and output by each precision
        ELSE 
                PRINT *, 'Result format: (k, a_k, (a_k)^n)'

                PRINT *, 'In single precision:'
                DO k = 0, n - 1
                        a_k0 = EXP(iunit_single * (2 * k * pi_single) / n)
                        PRINT *, '(', k, ',', a_k0, ',', a_k0 ** n, ')'
                END DO

                PRINT *, 'In double precision:'
                DO k = 0, n - 1
                        a_k1 = EXP(iunit_double * (2 * k * pi_double) / n)
                        PRINT *, '(', k, ',', a_k1, ',', a_k1 ** n, ')'
                END DO
        END IF

END PROGRAM problem04
