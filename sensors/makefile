sc : sc.o modos.o ../pbl_met.a
	gfortran -fcheck=all -O0 -J ..  -o sc sc.o modos.o ../pbl_met.a

sc.o : sc.f90 modos.mod
	gfortran -fcheck=all -O0 -J .. -c sc.f90
	
modos.o modos.mod : modos.f90
	gfortran -fcheck=all -O0 -J .. -c modos.f90

clean :
	rm *.mod
	rm *.o
	rm sc
	
