PROGRAM Exponentialreihe
  IMPLICIT NONE
  
  DOUBLE PRECISION :: x, e, y, e1, e2, delta, percent
  INTEGER :: i
  
  WRITE(*,*)'Bitte geben Sie die Potenz von e ein.'
  READ(*,*) x
  
  i = 0
  e = 1
  y = 1
  
  DO
    e1 = e
    i = i + 1
    
    y = y * (x/i)
    e = e + y
    
    IF (e1 == e) EXIT
  END DO
  
  e2 = EXP(x)
  delta = e - e2
  percent = -100 + (e/e2 * 100)
  
  WRITE(*,*)'nach Taylorreihe e^x = '
    WRITE(*,*) e
  WRITE(*,*)'nach EXP e^x = ' 
    WRITE(*,*) e2
  WRITE(*,*)'Differenz'
    WRITE(*,*) delta
  WRITE(*,*) 'Prozentuale Abweichung'
    WRITE(*,*) percent
  
END PROGRAM Exponentialreihe  
    
