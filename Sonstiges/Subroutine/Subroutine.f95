MODULE mod

  PUBLIC :: sub

  IMPLICIT NONE

CONTAINS
  SUBROUTINE sub(ein,a,b)

    IMPLICIT NONE

    INTEGER, INTENT(IN)  :: ein
    INTEGER, INTENT(OUT) :: a, b

    a = 2 * ein
    b = ein ** 2

  END SUBROUTINE
END MODULE

PROGRAM subroutine

  USE mod, ONLY : sub

  IMPLICIT NONE

  INTEGER :: x,y,z

  WRITE(*,*) 'Bitte geben Sie eine Zahl ein.'
  READ(*,*) x

  CALL sub(x,y,z)

  WRITE(*,*) '2x  =',y
  WRITE(*,*) 'x^2 =',z

END PROGRAM
