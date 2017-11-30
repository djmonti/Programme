PROGRAM recursive

  IMPLICIT NONE

  INTEGER :: a,b

  WRITE(*,*) 'Geben Sie eine Zahl ein.'
  READ(*,*) a

  b = fac(a)

  WRITE(*,*) 'n! =', b

CONTAINS

  RECURSIVE FUNCTION fac(n) RESULT(f)

    IMPLICIT NONE

    INTEGER :: n,f

    f = n

    IF (n > 1) THEN
      f = n * fac(n-1)
    END IF

  END FUNCTION

END PROGRAM
