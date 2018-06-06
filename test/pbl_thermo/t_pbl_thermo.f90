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
	call tst_NetRadiation_MPDA()
	
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
		integer	:: i
		integer	:: iYear
		integer	:: iMonth
		integer	:: iDay
		integer	:: iHour
		integer	:: iMinute
		integer	:: iSecond
		real	:: Rg_0
		real	:: Rg_1
		integer	:: iCurTime
		
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
		print *,"Test 3, GlobalRadiation_MPDA: 21. 03. 2000"
		print *
		print *, "Hour, sin(Beta), Rg(Cloud=100%), Rg(Cloud=0%)"
		do iHour = 0, 23
			sinBeta = SinSolarElevation(2000, 3, 21, iHour, 0, 0, 45.5, 10.0, 1, 3600)
			Rg_1 = GlobalRadiation_MPDA(1., sinBeta)
			Rg_0 = GlobalRadiation_MPDA(0., sinBeta)
			print "(i2,',',f7.4,2(',',f6.1))", iHour, sinBeta, Rg_1, Rg_0
		end do
		print *
		
		! Test 4: Determine the fork min-max cloud on 21. 06. 2000
		print *,"Test 4, GlobalRadiation_MPDA: 21. 06. 2000"
		print *
		print *, "Hour, sin(Beta), Rg(Cloud=100%), Rg(Cloud=0%)"
		do iHour = 0, 23
			sinBeta = SinSolarElevation(2000, 6, 21, iHour, 0, 0, 45.5, 10.0, 1, 3600)
			Rg_1 = GlobalRadiation_MPDA(1., sinBeta)
			Rg_0 = GlobalRadiation_MPDA(0., sinBeta)
			print "(i2,',',f7.4,2(',',f6.1))", iHour, sinBeta, Rg_1, Rg_0
		end do
		print *
		
		! Test 5: Determine the fork min-max cloud on 21. 09. 2000
		print *,"Test 5, GlobalRadiation_MPDA: 21. 09. 2000"
		print *
		print *, "Hour, sin(Beta), Rg(Cloud=100%), Rg(Cloud=0%)"
		do iHour = 0, 23
			sinBeta = SinSolarElevation(2000, 9, 21, iHour, 0, 0, 45.5, 10.0, 1, 3600)
			Rg_1 = GlobalRadiation_MPDA(1., sinBeta)
			Rg_0 = GlobalRadiation_MPDA(0., sinBeta)
			print "(i2,',',f7.4,2(',',f6.1))", iHour, sinBeta, Rg_1, Rg_0
		end do
		print *
		
		! Test 6: Determine the fork min-max cloud on all days in 2000 at noon
		print *,"Test 6, GlobalRadiation_MPDA: all days in 2000, noon"
		print *
		print *, "Date/Time, sin(Beta), Rg(Cloud=100%), Rg(Cloud=0%)"
		call PackTime(iCurTime, 2000, 1, 1, 12, 0, 0)
		do i = 0, 365
			call UnpackTime(iCurTime + i*3600*24, iYear, iMonth, iDay, iHour, iMinute, iSecond)
			sinBeta = SinSolarElevation(iYear, iMonth, iDay, iHour, iMinute, iSecond, 45.5, 10.0, 1, 3600)
			Rg_1 = GlobalRadiation_MPDA(1., sinBeta)
			Rg_0 = GlobalRadiation_MPDA(0., sinBeta)
			print "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2),',',f7.4,2(',',f6.1))", &
				iYear, iMonth, iDay, iHour, iMinute, iSecond, sinBeta, Rg_1, Rg_0
		end do
		print *
		
		! Test 7: Determine the cloud cover effect on date/time 2000-06-21 12:00:00
		print *,"Test 7, GlobalRadiation_MPDA: cloud cover effect on date/time 2000-06-21 12:00:00"
		print *
		print *, "Cloud.Cover, Rg"
		sinBeta = SinSolarElevation(2000, 6, 21, 12, 0, 0, 45.5, 10.0, 1, 3600)
		do i = 0, 32
			Rg_0 = GlobalRadiation_MPDA(i/32., sinBeta)
			print "(f6.4,',',f6.1)", i/32., Rg_0
		end do
		print *
		
		! Test 8: Boundary: Check what happens when one or both the input parameters are invalid
		print *,"Test 8, effect of invalid parameters"
		print *
		print *, "Case 1: Invalid cloud cover"
		sinBeta = SinSolarElevation(2000, 6, 21, 12, 0, 0, 45.5, 10.0, 1, 3600)
		print *, "Actually obtained: ", GlobalRadiation_MPDA(NaN, sinBeta), "  (expected: NaN)"
		print *
		print *, "Case 2: Invalid sine of solar elevation angle"
		print *, "Actually obtained: ", GlobalRadiation_MPDA(0.2, NaN), "  (expected: NaN)"
		print *
		print *, "Case 3: Invalid sine of solar elevation angle and cloud cover"
		print *, "Actually obtained: ", GlobalRadiation_MPDA(NaN, NaN), "  (expected: NaN)"
		print *
		
		! Test 9: Boundary: Check what happens when one input parameter is valid but makes no sense
		print *,"Test 9, effect of invalid parameters"
		print *
		print *, "Case 1: Wrong cloud cover: -0.01"
		sinBeta = SinSolarElevation(2000, 6, 21, 12, 0, 0, 45.5, 10.0, 1, 3600)
		print *, "Actually obtained: ", GlobalRadiation_MPDA(-0.01, sinBeta), "  (expected: NaN)"
		print *
		print *, "Case 2: Wrong sine of solar elevation angle: -1.1"
		print *, "Actually obtained: ", GlobalRadiation_MPDA(0.2, -1.1), "  (expected: NaN)"
		print *
		print *, "Case 3: Wrong sine of solar elevation angle: 1.1"
		print *, "Actually obtained: ", GlobalRadiation_MPDA(0.2, 1.1), "  (expected: NaN)"
		print *
		
	end subroutine tst_GlobalRadiation_MPDA
	

	subroutine tst_NetRadiation_MPDA()
	
		! Routine arguments
		! --none--
		
		! Locals
		real	:: sinBeta
		integer	:: i
		integer	:: iYear
		integer	:: iMonth
		integer	:: iDay
		integer	:: iHour
		integer	:: iMinute
		integer	:: iSecond
		real	:: Rg_0
		real	:: Rg_1
		real	:: Rn_0
		real	:: Rn_1
		integer	:: iCurTime
		
		! Test 1: Determine the fork min-max cloud on 21. 06. 2000
		print *,"Test 1, NetRadiation_MPDA: 21. 06. 2000"
		print *
		print *, "Hour, sin(Beta), Rg(Cloud=100%), Rg(Cloud=0%), Rn(Cloud=100%), Rn(Cloud=0%)"
		do iHour = 0, 23
			sinBeta = SinSolarElevation(2000, 6, 21, iHour, 0, 0, 45.5, 10.0, 1, 3600)
			Rg_1 = GlobalRadiation_MPDA(1., sinBeta)
			Rg_0 = GlobalRadiation_MPDA(0., sinBeta)
			Rn_1 = NetRadiation_MPDA(4, 0.5, 20., Rg_1, 1., 0.05, 10., 2.5)
			Rn_0 = NetRadiation_MPDA(4, 0.5, 20., Rg_0, 0., 0.05, 10., 2.5)
			print "(i2,',',f7.4,4(',',f6.1))", iHour, sinBeta, Rg_1, Rg_0, Rn_1, Rn_0
		end do
		print *
		
		! Test 2: Determine the fork min-max cloud on 21. 09. 2000
		print *,"Test 2, NetRadiation_MPDA: 21. 09. 2000"
		print *
		print *, "Hour, sin(Beta), Rg(Cloud=100%), Rg(Cloud=0%), Rn(Cloud=100%), Rn(Cloud=0%)"
		do iHour = 0, 23
			sinBeta = SinSolarElevation(2000, 9, 21, iHour, 0, 0, 45.5, 10.0, 1, 3600)
			Rg_1 = GlobalRadiation_MPDA(1., sinBeta)
			Rg_0 = GlobalRadiation_MPDA(0., sinBeta)
			Rn_1 = NetRadiation_MPDA(4, 0.5, 20., Rg_1, 1., 0.05, 10., 2.5)
			Rn_0 = NetRadiation_MPDA(4, 0.5, 20., Rg_0, 0., 0.05, 10., 2.5)
			print "(i2,',',f7.4,4(',',f6.1))", iHour, sinBeta, Rg_1, Rg_0, Rn_1, Rn_0
		end do
		print *
		
	end subroutine tst_NetRadiation_MPDA
	
end program t_pbl_thermo
