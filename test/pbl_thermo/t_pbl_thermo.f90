! Test program for 'pbl_thermo' sub-module.
!
! This code is part of the pbl_met library.
!
! This is open-source code, covered by the lGPL 3.0 license.
!
program t_pbl_thermo

	use pbl_met
	
	implicit none
	
	! Locals
	! --none--
	
	! Perform tests
	call tst_BruntVaisala()
	call tst_GlobalRadiation_MPDA()
	
contains

	subroutine tst_BruntVaisala()
	
		! Routine arguments
		! --none--
		
		! Locals
		real	:: Tpot, Temp, N
		real	:: Tpot1, N1, Tpot2, N2
		integer	:: i
		real	:: z
		real	:: rLapseRate
		
		! Test 1: Behavior with increasing height
		print *, "Test 1: Brunt-Vaisala frequency at increasing height, same potential temperature"
		print *
		Tpot = 20.0
		print *,"Z, Td, N"
		do i = 1, 16
			z = i * 100.
			Temp = Tpot - 0.0098*z
			N = BruntVaisala(Temp, z)
			print "(f5.0,',',f5.1,',',e15.7)", z, Temp, N
		end do
		print *
		
		! Test 2: Behavior with increasing height
		print *, "Test 2: Brunt-Vaisala frequency at increasing height, same dry-bulb temperature"
		print *
		Temp = 20.0
		print *,"Z, Td, N"
		do i = 1, 16
			z = i * 100.
			Tpot = Temp + 0.0098*z
			N = BruntVaisala(Temp, z)
			print "(f5.0,',',f5.1,',',e15.7)", z, Temp, N
		end do
		print *
		
		! Test 3: Behavior with constant height
		print *, "Test 3: Brunt-Vaisala frequency at constant height, increasing temperature"
		print *
		z = 1500.
		print *,"Z, Td, N"
		do i = 1, 16
			Temp = 5.0 + 2*i
			N = BruntVaisala(Temp, z)
			print "(f5.0,',',f5.1,',',e15.7)", z, Temp, N
		end do
		print *
		
		! Test 4: Behavior with constant height, more realistic temperature
		print *, "Test 4: Brunt-Vaisala frequency at constant height, increasing temperature, more realistic"
		print *
		z = 1500.
		print *,"Z, Td, N"
		do i = 1, 16
			Temp = -15.0 + 2*i
			N = BruntVaisala(Temp, z)
			print "(f5.0,',',f5.1,',',e15.7)", z, Temp, N
		end do
		print *
		
		! Test 5: Behavior with constant height, more realistic temperature, and different assumptions about temperature lapse rate
		print *, "Test 5: Brunt-Vaisala frequency at constant height, increasing temperature, different lapse rates"
		print *
		z = 1500.
		print *,"Z, Td, N(0.0098), N(0.0090), N(0.0110)"
		do i = 1, 16
			Temp = -15.0 + 2*i
			N = BruntVaisala(Temp, z)
			Tpot1 = Temp + 273.15 + 0.009 * z
			N1    = sqrt(abs(9.807/Tpot1 * 0.009))
			Tpot2 = Temp + 273.15 + 0.011 * z
			N2    = sqrt(abs(9.807/Tpot2 * 0.011))
			print "(f5.0,',',f5.1,3(',',e15.7))", z, Temp, N, N1, N2
		end do
		print *
		
		! Test 6: Behavior with constant height, constant temperature, and changing lapse rate
		print *, "Test 6: Brunt-Vaisala frequency at constant height and temperature, with different lapse rates"
		print *
		z = 1500.
		Temp = 0.
		print *,"dT/dz, N"
		do i = 1, 16
			rLapseRate = 0.0098 + (i-8) * 0.0002
			Tpot1 = Temp + 273.15 + rLapseRate * z
			N1    = sqrt(abs(9.807/Tpot1 * rLapseRate))
			print "(f6.4,',',e15.7)", rLapseRate, N1
		end do
		print *
		
		! Test 7: Boundary: One or two inputs invalid
		print *, "Test 6: Brunt-Vaisala frequency at constant height and temperature, with different lapse rates"
		print *
		z = 1500.
		Temp = 0.
		print *,"Case 1: invalid temperature:   result = ", BruntVaisala(NaN, 1000.), "   (Expected: NaN)"
		print *,"Case 2: invalid height:        result = ", BruntVaisala(1., NaN), "   (Expected: NaN)"
		print *,"Case 3: invalid height & temp: result = ", BruntVaisala(NaN, NaN), "   (Expected: NaN)"
		print *
		
	end subroutine tst_BruntVaisala


	subroutine tst_GlobalRadiation_MPDA()
	
		! Routine arguments
		! --none--
		
		! Locals
		real	:: sinBeta
		integer	:: iHour
		real	:: Rg_0
		real	:: Rg_1
		
		! Test 1: Determine the fork min-max cloud on 01. 01. 2000
		print *,"Test 1, GlobalRadiation_MPDA: 01. 01. 2000"
		print *
		print *, "Hour, sin(Beta), Rg(Cloud=100%), Rg(Cloud=0%)"
		do iHour = 0, 23
			sinBeta = SinSolarElevation(2000, 1, 1, iHour, 0, 0, 45.5, 10.0, 1, 3600)
			Rg_1 = GlobalRadiation_MPDA(1., sinBeta)
			Rg_0 = GlobalRadiation_MPDA(0., sinBeta)
			print "(i2,',',f7.4,2(',',f6.1))", iHour, sinBeta, Rg_1, Rg_0
		end do
		print *
		
		! Test 2: Determine the fork min-max cloud on 21. 12. 2000
		print *,"Test 2, GlobalRadiation_MPDA: 21. 12. 2000"
		print *
		print *, "Hour, sin(Beta), Rg(Cloud=100%), Rg(Cloud=0%)"
		do iHour = 0, 23
			sinBeta = SinSolarElevation(2000, 12, 21, iHour, 0, 0, 45.5, 10.0, 1, 3600)
			Rg_1 = GlobalRadiation_MPDA(1., sinBeta)
			Rg_0 = GlobalRadiation_MPDA(0., sinBeta)
			print "(i2,',',f7.4,2(',',f6.1))", iHour, sinBeta, Rg_1, Rg_0
		end do
		print *
		
		! Test 3: Determine the fork min-max cloud on 21. 03. 2000
		print *,"Test 2, GlobalRadiation_MPDA: 21. 03. 2000"
		print *
		print *, "Hour, sin(Beta), Rg(Cloud=100%), Rg(Cloud=0%)"
		do iHour = 0, 23
			sinBeta = SinSolarElevation(2000, 3, 21, iHour, 0, 0, 45.5, 10.0, 1, 3600)
			Rg_1 = GlobalRadiation_MPDA(1., sinBeta)
			Rg_0 = GlobalRadiation_MPDA(0., sinBeta)
			print "(i2,',',f7.4,2(',',f6.1))", iHour, sinBeta, Rg_1, Rg_0
		end do
		print *
		
	end subroutine tst_GlobalRadiation_MPDA
	
end program t_pbl_thermo
