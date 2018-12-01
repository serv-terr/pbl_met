dewpoint : DewPoint.f90
	gfortran -J ../core -o dewpoint DewPoint.f90 ../core/pbl_met.a

alamo : alamo.o alamo_config.o alamo_emis.o alamo_meteo.o
	gfortran -J ../core -o alamo alamo.o alamo_config.o alamo_emis.o alamo_meteo.o ../core/pbl_met.a

alamo.o : Alamo.f90 configuration.mod
	gfortran -J ../core -c Alamo.f90

alamo_config.o configuration.mod : Alamo_Config.f90 emission.mod
	gfortran -J ../core -c Alamo_Config.f90

alamo_emis.o emission.mod : Alamo_Emis.f90
	gfortran -J ../core -c Alamo_Emis.f90

alamo_meteo.o meteo.mod : Alamo_Meteo.f90
	gfortran -J ../core -c Alamo_Meteo.f90

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
	
test_alamo:
	./alamo ./alamo.ini
