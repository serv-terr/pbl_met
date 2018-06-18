! Program SodarChecker - Test program for 'modos.f90' module.
!
! This program is part of the pbl_met library.
!
! Copyright 2018 by Servizi Territorio srl
!
! This is open-source code, covered by lGPL 3.0 license
!
program SodarChecker

	use modos
	
	implicit none
	
	! Locals
	integer			:: iRetCode
	type(ModosData)	:: tSodar
	
	! Test 1: Read SDR data from SODAR-only station
	iRetCode = tSodar % load(10, "0616.sdr")
	print *, "Test 1: Get SDR data from SODAR-only station"
	print *, "Return code: ", iRetCode, "  (expected:0)"

end program SodarChecker
