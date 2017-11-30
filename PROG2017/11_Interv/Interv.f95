PROGRAM Interv
  USE ERRORS
  USE IVALMOD
  IMPLICIT NONE

  REAL(KIND=pr)    :: fo, bo, deltaf, deltab, go, deltag
  TYPE(INTERVAL)  :: g1,g2,f,b


  WRITE(*,*) 'Geben Sie einen Wert für die Brennweite ein.'
  READ(*,*) fo
  WRITE(*,*) 'Geben Sie einen Wert für die Bildweite ein.'
  READ(*,*) bo
  WRITE(*,*) 'Geben Sie einen Wert für den Fehler der Brennweite ein.'
  READ(*,*) deltaf
  WRITE(*,*) 'Geben Sie einen Wert für den Fehler der Bildweite ein.'
  READ(*,*) deltab

  go     = 1/((1/fo)-(1/bo))
  deltag = ((deltaf/(1-(fo/bo))**2)+(deltab/(((bo/fo)-1)**2)))

  WRITE(*,*) 'Das Interval ist: {', go-deltag, go+deltag, '}'

END PROGRAM
