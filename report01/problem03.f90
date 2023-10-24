!Calculate approximate square roots.
PROGRAM problem03

        !Initialize variables in double precision
        REAL(8) :: x, g, delta, error

        !Set x as an real nunber
        PRINT *, 'Input x:'
        READ *, x

        IF (x < 0) THEN                                         !Determine the positivity or negativity of x
                PRINT *, 'The square root does not exist in real space.'
        ELSE
                !Calculation
                g = 1.0
                error = 1.0e-10                                 !Error tolerance
10              delta = ABS(g ** 2 - x)                         !Error in approximation g
                IF (delta < error) THEN
                        PRINT '("root(x)=", f15.10)', g         !Set output length
                        STOP
                ELSE
                        g = (g + x / g) * 0.5
                END IF
                GO TO 10
        END IF

END PROGRAM problem03
