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

pbl_met.a : pbl_met.o
	$(AR) $(ARFLAGS) pbl_met.a pbl_met.o $(ARPOST)

pbl_met.o pbl_met.mod : pbl_met.f90
	$(F90) $(FFLAGS) pbl_met.f90

.PHONY: clean
clean:
	rm -f *.a
	rm -f *.o
	rm -f *.mod
