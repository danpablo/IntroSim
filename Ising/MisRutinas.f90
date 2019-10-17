Module MisRutinas

contains

subroutine Estado_Inicial(Nx, Ny, Estado)
  use Ziggurat
  IMPLICIT NONE
  INTEGER(KIND=4):: Nx, Ny, seed, i, j
  logical :: es
  INTEGER(KIND=4), DIMENSION(:,:), allocatable :: Estado

  inquire(file='seed.dat',exist=es)
    if(es) then
      open(unit=10,file='seed.dat',status='old')
      read(10,*) seed
      close(10)
      print *,"  * Leyendo semilla de archivo seed.dat"
    else
      seed = 24583490
    end if
  call zigset(seed)

  !Genero una matriz random
  if (Allocated(Estado)) Deallocate(Estado)
  Allocate(Estado(0:Ny-1, 0:Nx-1))

  DO j=0,Nx-1
    DO i=0,Ny-1
      Estado(i,j) = 2*int(uni() + 0.5D0) - 1
    END DO
  END DO

  open(unit=10,file='seed.dat',status='unknown')
  seed = shr3()
  write(10,*) seed
  close(10)

  return
end subroutine Estado_Inicial


subroutine CALC_ENERGIA(estado, Jacop, energia)
  Integer(kind=4), dimension(:,:), allocatable :: Estado
  real(kind=8)::Jacop, energia
  integer(kind=4):: spin, vUP, vDOWN, vLEFT, vRIGHT, iy, ix, SumaVecinos

  Nx = ubound(Estado, dim=2) + 1
  Ny = ubound(Estado, dim=1) + 1
  Energia = 0.0D0
  DO ix=lbound(estado, dim=2),ubound(estado, dim=2)
    DO iy=lbound(estado, dim=1),ubound(estado, dim=1)
      spin    = Estado(iy, ix)
      vUP     = Estado(modulo(iy + 1,Ny), ix)
      vDOWN   = Estado(modulo(iy - 1, Ny), ix)
      vLEFT   = Estado(iy, modulo(ix-1, Nx))
      vRIGHT  = Estado(iy, modulo(ix+1, Nx))

      SumaVecinos = vUP + vDOWN + vLEFT + vRIGHT

      Energia = Energia - Jacop*spin*SumaVecinos
    end do
  end do
  Energia = Energia / 2.0D0
  return
end subroutine CALC_ENERGIA

subroutine CALC_MAGNETIZACION(ESTADO, magnetizacion)
  integer(kind=4), dimension(:,:), allocatable :: Estado
  real(kind = 8) :: magnetizacion

  magnetizacion = sum(estado)


return
end subroutine CALC_MAGNETIZACION

end module MisRutinas
