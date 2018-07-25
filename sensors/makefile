sc : sc.o modos.o ../pbl_met.a
	gfortran -fcheck=all -O0 -J ..  -o sc sc.o modos.o ../pbl_met.a

sonicpro : sonicpro.o usa1.o ../pbl_met.a
	gfortran -fcheck=all -O0 -J ..  -o sonicpro sonicpro.o usa1.o ../pbl_met.a

sc.o : sc.f90 modos.mod
	gfortran -fcheck=all -O0 -c -I .. sc.f90
	
sonicpro.o : sonicpro.f90 usa1.mod
	gfortran -fcheck=all -O0 -c -I .. sonicpro.f90
	
modos.o modos.mod : modos.f90
	gfortran -fcheck=all -O0 -c -I .. modos.f90

usa1.o usa1.mod : usa1.f90
	gfortran -fcheck=all -O0 -c -I .. usa1.f90

clean :
	rm *.mod
	rm *.o
	rm sc
	rm sonicpro
	
