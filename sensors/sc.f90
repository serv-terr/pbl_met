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
	use pbl_met
	
	implicit none
	
	! Locals
	integer								:: iRetCode
	integer								:: i
	type(ModosData)						:: tSodar
	type(DateTime)						:: tDate
	integer								:: iSensorType
	integer								:: iNumChanges
	real(8), dimension(:), allocatable	:: rvTimeStamp
	integer, dimension(:), allocatable	:: ivTimeStep
	character(len=23)					:: sDateTime
	
	! Test 1: Read SDR data from SODAR-only station
	iRetCode = tSodar % load(10, "0616.sdr")
	print *, "Test 1: Get SDR data from SODAR-only station"
	print *, "Return code: ", iRetCode, "  (expected:0)"
	iSensorType = tSodar % getSensorType()
	print *, "Sensor type = ", iSensorType, "  (expected:", MDS_SODAR, ")"
	print *
	
	! Test 2: Count heights changes (a subset of configuration changes)
	print *, "Test 2: Count height changes"
	iRetCode = tSodar % getNumHeightChanges(iNumChanges)
	print *, "Return code: ", iRetCode, "  (expected:0)"
	print *, "Num.changes: ", iNumChanges, "  (expected:0)"
	print *
	
	! Test 3: Get list of block time stamp and durations
	print *, "Test 3: Get block time stamps and delta times"
	print *
	iRetCode = tSodar % getBlockInfo(rvTimeStamp, ivTimeStep)
	print *, "Return code: ", iRetCode, "  (expected:0)"
	print *
	print *, "Date, Time.Step"
	do i = 1, size(rvTimeStamp)
		iRetCode = tDate % fromEpoch(rvTimeStamp(i))
		sDateTime = tDate % toISO()
		print *, sDateTime, ",", ivTimeStep(i)
	end do
	
	! Test 4: Read all spectra, in sequence
	print *, "Test 4: Read spectra in sequence"
	print *
	print *, "Date, Ret.Code, Time.Step"
	do i = 1, size(rvTimeStamp)
		iRetCode = tDate % fromEpoch(rvTimeStamp(i))
		sDateTime = tDate % toISO()
		iRetCode = tSodar % getSodarSpectra(i)
		print *, sDateTime, ",", iRetCode, ",", ivTimeStep(i)
	end do
	print *

end program SodarChecker
