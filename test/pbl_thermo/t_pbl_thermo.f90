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
	
contains

	subroutine tst_BruntVaisala()
	
		! Routine arguments
		! --none--
		
		! Locals
		real	:: Tpot, Temp, N
		integer	:: i
		real	:: z
		
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
		
	end subroutine tst_BruntVaisala
	
end program t_pbl_thermo
