MAKEFILE = Makefile
exe = ising
fcomp = ifort #gfortran #ifort # /opt/intel/compiler70/ia32/bin/ifc
# Warning: the debugger doesn't get along with the optimization options
# So: not use -O3 WITH -g option
flags =  -O3
# Remote compilation
OBJS = ziggurat.o MisRutinas.o ising.o

.SUFFIXES:            # this deletes the default suffixes
.SUFFIXES: .f90 .o    # this defines the extensions I want

.f90.o:
	$(fcomp) -c $(flags) $<


$(exe):  $(OBJS) Makefile
	$(fcomp) $(flags) -o $(exe) $(OBJS)


clean:
	rm ./*.o ./*.mod


ising.o: ising.f90 ziggurat.o MisRutinas.o
