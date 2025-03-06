OMP = 
FOPTS = -O2

.SUFFIXES: .f90 .F90

OBJS = module_mp_nssl_2mom.o \
       rk3_main.o rk3_init.o rk3_plot.o rk3_rhss.o rk3_rhsu1.o \
       rk3_rhsu2.o rk3_rhsu3.o \
       rk3_rhsw.o rk3_smlstep.o rk3_coefs.o kessler.o 

.f90.o:
	gfortran $(FOPTS) $(OMP) -c $*.f90

.F90.o:
	gfortran $(FOPTS) $(OMP) -c $*.F90

rk3_moist:  $(OBJS) plotting.inc.f90 initialize.inc.f90 boundaries.inc.f90
	ncargf90 $(FOPTS) $(OMP) -o rk3_moist  $(OBJS)

rk3_main.o: rk3_main.f90 plotting.inc.f90 initialize.inc.f90 boundaries.inc.f90 dims.inc.f90
	ncargf90 -c $(FOPTS) $(OMP) rk3_main.f90

rk3_plot.o: rk3_plot.f90 dims.inc.f90
	ncargf90 -c $(FOPTS) rk3_plot.f90

module_mp_nssl_2mom.o: module_mp_nssl_2mom.F90
	gfortran $(OMP) -c module_mp_nssl_2mom.F90

clean:
	rm -f *.o *.mod rk3_moist
