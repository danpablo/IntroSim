PROGRAM Ising
  USE Ziggurat
  use MisRutinas
  IMPLICIT NONE
  REAL(8) :: Temp, Jacop, Energia, DEnergia, expDenergia, proba, magnetizacion
  REAL(8) :: dmag, SumaEnergia, SumaMag, SumaMagCuad, SumaEnergiaCuad
  INTEGER (KIND = 4) :: seed, aceptados, Nx, Ny, PasosMC, ix, iy, i, j
  INTEGER (KIND=4) ::spin, vUP, vDOWN, vLEFT, vRIGHT, SumaVecinos
  LOGICAL :: es
  character(len=*), parameter :: file_input = "input.dat"
  CHARACTER(len=*), parameter :: file_output_inst = "datosINST.dat"
  CHARACTER(len=*), parameter :: file_output_Prom = "datosPROM.dat"
  CHARACTER(len=*), parameter :: matrixIN = "matrix_in.dat"
  CHARACTER(len=*), parameter :: matrixOUT = "matrix_out.dat"
  INTEGER(kind=4), dimension(:,:), allocatable :: Estado

  !inicializo las variables
  Energia = 0.0D0
  DEnergia = 0.0D0
  expDenergia = 0.0D0
  SumaMag=0.0D0
  SumaEnergia=0.0D0
  SumaMagCuad = 0.0D0
  SumaEnergiaCuad = 0.0D0

  !Leo los datos de entrada
  inquire(file=file_input, exist=es)
  IF(es) THEN
    open(unit=10, file=file_input, status="old")
    read(10,*) PasosMC
    read(10,*) Nx, Ny ! Lee los puntos totales de la grilla
    read(10,*) Temp
    read(10,*) Jacop
    Write(6,*) PasosMC, Nx, Ny, Temp, Jacop
    close(10)
  END IF

  !Genero el etado inicial leyendo la matriz, si no existe lo genero
  inquire(file=matrixIN, exist =es)
  if (es) then
    if (Allocated(Estado)) Deallocate(Estado)
    Allocate(Estado(0:Ny-1, 0:Nx-1))
    open(unit=10, file=matrixIN, status='old')
    do i=0,Ny-1
      read(10,*) Estado(i,:)
    end do
  else
    CALL Estado_Inicial(Nx, Ny, Estado)
  end if

  !calculo energia y magnetizacion del estado inicial
  CALL CALC_ENERGIA(Estado, Jacop, Energia)
  CALL CALC_MAGNETIZACION(Estado, magnetizacion)

!%%%%%%%%% PARTE DE LA RUTINA Ziggurat %%%%%%%%%%%%%
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
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  !Sorteo las coordenadas de un Spin con dist uniforme
  !open(10, file=file_output_inst)
  aceptados = 0
  do i=1,PasosMC
    ix = int(Nx * uni()) !indice columna
    iy = int(Ny * uni()) !indice fila

    spin    = Estado(iy, ix)
    vUP     = Estado(modulo(iy + 1,Ny), ix)
    vDOWN   = Estado(modulo(iy - 1, Ny), ix)
    vLEFT   = Estado(iy, modulo(ix-1, Nx))
    vRIGHT  = Estado(iy, modulo(ix+1, Nx))

    SumaVecinos = vUP + vDOWN + vLEFT + vRIGHT

    Dmag = -2.0*spin
    DEnergia = -Jacop * (-2.0D0) * spin * SumaVecinos
    expDenergia = dexp((-1.0D0/Temp) * DEnergia)

    proba =  min(1.0D0, expDenergia)

    if (uni() .lt. proba) THEN
      !acepto
      Estado(iy, ix) = -1*Estado(iy, ix)
      Energia = Energia + DEnergia
      magnetizacion = magnetizacion + Dmag
      aceptados = aceptados + 1
      !write(6,*) aceptados
    else
      !rechazo no hago anda y me quedo con el estado anterio
    end if

    !Sumo las Energias y Magnetizaciones para calcular promedios
    SumaEnergia = SumaEnergia + energia
    SumaEnergiaCuad = SumaEnergiaCuad + energia**2
    SumaMag = SumaMag + magnetizacion
    SumaMagCuad = SumaMagCuad + magnetizacion**2
    !Grabo energia y magnetizacion instantaneas  en el archivo
    !Write(10, *) Energia, magnetizacion

  end do
  !close(10)

  !Grabo valores medios
  inquire(file=file_output_prom, exist =es)
  if (es) then

         open(10, file=file_output_prom, status="old", access="append")
  else

         open(10, file=file_output_prom, status="new")

  end if

  Write(6,*) aceptados/dfloat(PasosMC)

  Write(10,"(5G17.10)") SumaEnergia/float(PasosMC), SumaEnergiaCuad/float(PasosMC), SumaMag/float(PasosMC), SumaMagCuad/float(PasosMC), aceptados/float(PasosMC)
 
  close(10)
  

!guardo la semilla
  open(unit=10,file='seed.dat',status='unknown')
  seed = shr3()
  write(10,*) seed
  close(10)

  !guardo la matriz final
  open(unit=10,file=matrixOUT,status='unknown')
  do i=0,Ny-1
    write(10,"(20I3)") (Estado(i,j), j=0,Nx-1)
  end do
  close(10)
END PROGRAM Ising
