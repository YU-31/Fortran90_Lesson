!Calculating the area of a triangle
PROGRAM problem01

        !Initialize variables
        IMPLICIT NONE
        REAL :: a, b, c, t, x, S

        !Set the parameters
        PRINT *, 'Enter the lengths a, b, and c of the sides of the triangle.'
        PRINT *, 'a: '
        READ *, a
        PRINT *, 'b: '
        READ *, b
        PRINT *, 'c: '
        READ *, c

        !Calculation
        t = (a + b + c) / 2
        x = t * (t - a) * (t - b) * (t - c)
        IF (x < 0) THEN
                PRINT *, 'Triangle cannot be created with the entered values.'
        ELSE
                S = x ** 0.5
                PRINT *, 'Area of triangle : ', S
        END IF

END PROGRAM problem01