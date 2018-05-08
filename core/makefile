F90=gfortran
FFLAGS=-fPIC -c

UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
AR=libtool
ARFLAGS=-o
ARPOST=2>nul
else
AR=ar
ARFLAGC=cr
ARPOST=
endif

pbl_met.a : pbl_met.o pbl_base.o pbl_stat.o pbl_time.o pbl_thermo.o pbl_evtrn.o pbl_wind.o pbl_simil.o pbl_depth.o
	$(AR) $(ARFLAGS) pbl_met.a pbl_met.o pbl_base.o pbl_stat.o pbl_time.o pbl_thermo.o pbl_evtrn.o pbl_wind.o pbl_simil.o pbl_depth.o $(ARPOST)

pbl_met.o pbl_met.mod : pbl_met.f90 pbl_base.mod pbl_stat.mod pbl_time.mod pbl_thermo.mod pbl_evtrn.mod pbl_wind.mod pbl_depth.mod
	$(F90) $(FFLAGS) pbl_met.f90

pbl_base.o pbl_base.mod : pbl_base.f90
	$(F90) -fno-range-check $(FFLAGS) pbl_base.f90
	
pbl_stat.o pbl_stat.mod : pbl_stat.f90 pbl_base.mod pbl_time.mod
	$(F90) $(FFLAGS) pbl_stat.f90
	
pbl_time.o pbl_time.mod : pbl_time.f90
	$(F90) $(FFLAGS) pbl_time.f90
	
pbl_thermo.o pbl_thermo.mod : pbl_thermo.f90 pbl_base.mod pbl_time.mod
	$(F90) $(FFLAGS) pbl_thermo.f90
	
pbl_simil.o pbl_simil.mod : pbl_simil.f90 pbl_base.mod pbl_thermo.mod pbl_wind.mod
	$(F90) $(FFLAGS) pbl_simil.f90
	
pbl_depth.o pbl_depth.mod : pbl_depth.f90 pbl_base.mod pbl_thermo.mod pbl_wind.mod pbl_simil.mod
	$(F90) $(FFLAGS) pbl_depth.f90
	
pbl_evtrn.o pbl_evtrn.mod : pbl_evtrn.f90 pbl_base.mod pbl_time.mod pbl_thermo.mod
	$(F90) $(FFLAGS) pbl_evtrn.f90
	
pbl_wind.o pbl_wind.mod : pbl_wind.f90 pbl_base.mod
	$(F90) $(FFLAGS) pbl_wind.f90
	
.PHONY: clean
clean:
	rm -f *.a
	rm -f *.o
	rm -f *.mod
