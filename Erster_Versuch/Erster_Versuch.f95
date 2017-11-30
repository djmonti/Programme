program Erster_Versuch
  implicit none
    
  integer :: i, a, k, j
  write(*,*) 'Bitte geben Sie die oberere Grenze ein.'
  read(*,*) a
  write(*,*) 'Die Primzahlen bis zum Grenzwert:'
  1: do (i = 2), a            ! Alle Zahlen bis a werden geprüft 
    2: do (j = 2), (a/2)        ! Alle Zahlen werden auf Teilbarkeit mit Zahlen bis a/2 geprüft
      k= i mod j
        3: if (k == 0)
          then exit
        end if 3
    end do 2
    4: if (k = 1)
      then write(*,*) i, ', '     
    end if 4
  end do 1
end program  
