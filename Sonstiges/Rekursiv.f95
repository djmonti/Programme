program bsp
  implicit none

  integer :: zahl, ergebnis, fac

  write(*,*) "Gib eine Ganzzahl ein: "
  read(*,*) zahl

  ergebnis = fac(zahl)

  write(*,*) "Ergebnis ist: ", ergebnis
end program bsp


recursive function fac(n) result(zerg)
  implicit none
  integer, intent(in) :: n
  integer :: zerg

  ! Vereinfacht: Keine Überprüfung ob Überlauf, negative Zahl, etc.

  zerg = n

  if (n > 1) then
    zerg = n * fac(n-1)
  end if

  return
end function fac
