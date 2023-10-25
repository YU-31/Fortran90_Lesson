!Find the n-squared roots of 1.
PROGRAM problem04

        !Initialize variables
        IMPLICIT NONE
        INTEGER :: n, k
        REAL(8), PARAMETER :: pi = 3.14159265358979e0
        COMPLEX :: a_k0                                 !Single precision
        COMPLEX(kind(0d0)) :: a_k1                      !Double precision
        COMPLEX, PARAMETER :: iunit = (0e0, 1e0)        !Imaginary unit    
        
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
                        a_k0 = EXP(iunit * (2 * k * pi) / n)
                        PRINT *, '(', k, ',', a_k0, ',', a_k0 ** n, ')'
                END DO

                PRINT *, 'In double precision:'
                DO k = 0, n - 1
                        a_k1 = EXP(iunit * (2 * k * pi) / n)
                        PRINT *, '(', k, ',', a_k1, ',', a_k1 ** n, ')'
                END DO
        END IF

END PROGRAM problem04
