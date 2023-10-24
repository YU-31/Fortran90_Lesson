PROGRAM check02

        IMPLICIT NONE
        INTEGER :: n

        PRINT *, 'Input integer n:'
        READ *, n

        IF (MOD(n,2) == 0) THEN
                PRINT *, 'Even'
        ELSE
                PRINT *, 'Odd'
        END IF

END PROGRAM
