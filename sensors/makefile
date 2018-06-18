sc : sc.o modos.o
	gfortran -o sc sc.o modos.o

sc.o : sc.f90 modos.mod
	gfortran -c sc.f90
	
modos.o modos.mod : modos.f90
	gfortran -c modos.f90

clean :
	rm *.mod
	rm *.o
	rm sc
	
