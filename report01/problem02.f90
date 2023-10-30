!Determine if there is an integral cube root of an integer.
PROGRAM problem02

        !Initialize variables
        IMPLICIT NONE
        INTEGER :: n, nn, ns, k, ncube
        k = 0

        !Set n as an integer
        PRINT *, 'Enter an integer.'
        READ *, n

        nn = ABS(n)             !Absolute value of n
        ns = ABS(n) / n         !Sign of n

        !Calculation
10      IF (k ** 3 == nn) THEN
                ncube = k * ns
                PRINT *, 'Integral cube root of given integer:', ncube
        ELSE
                IF (k ** 3 >= nn) THEN
                        PRINT *, 'Integral cube root of given integer does not exist.'
                ELSE
                        k = k + 1
                        GO TO 10
                END IF
        END IF

END PROGRAM problem
