dewpoint : DewPoint.f90
	gfortran -J ../core -o dewpoint DewPoint.f90 ../core/pbl_met.a

radgen : radgen.f90
	gfortran -J ../core -o radgen radgen.f90 ../core/pbl_met.a

test :
	./radgen 45.00 10.00 1 2018-01-01 2018-12-31 3600 0 rgSim.csv

test.simple :
	./radgen 45.00 10.00 1 2018-01-01 2018-12-31 3600 0 20. 75. 1013. rgSimple.csv

test1 :
	./radgen 45.00 10.00 1 2018-01-01 2018-12-31 3600 1 rgSim1.csv

test2 :
	./radgen 45.00 10.00 1 2018-01-01 2018-12-31 3600 2 rgSim2.csv
