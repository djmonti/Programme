PROGRAM Hello_World
  integer :: x, y, z
  WRITE(*,*) 'Geben Sie zwei ganze Zahlen ein.'
  READ(*,*) x, y
  
  z = x + y
  
  WRITE(*,*) 'Die Summe beträgt', z
  
  WRITE(*,*) 'Hello World'
END PROGRAM Hello_World
