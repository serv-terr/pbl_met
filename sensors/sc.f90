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
	integer								:: i, j
	type(ModosData)						:: tSodar
	type(DateTime)						:: tDate
	integer								:: iSensorType
	integer								:: iNumChanges
	real(8), dimension(:), allocatable	:: rvTimeStamp
	integer, dimension(:), allocatable	:: ivTimeStep
	character(len=23)					:: sDateTime
	real, dimension(6)					:: rvAvailability
	real, dimension(6)					:: rvNoise
	real, dimension(6)					:: rvTop
	real, dimension(6)					:: rvBottom
	real, dimension(6)					:: rvMeanVariation
	real, dimension(:), allocatable		:: rvVel070
	real, dimension(:), allocatable		:: rvDir070
	real, dimension(:), allocatable		:: rvVel170
	real, dimension(:), allocatable		:: rvDir170
	real, dimension(:), allocatable		:: rvVel270
	real, dimension(:), allocatable		:: rvDir270
	real, dimension(:), allocatable		:: rvVel370
	real, dimension(:), allocatable		:: rvDir370
	
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
		iRetCode = tSodar % sodarSpectraAvailability(rvAvailability)
		iRetCode = tSodar % sodarSpectraNoiseIndicators(rvNoise, rvTop, rvBottom, rvMeanVariation)
		print *
		do j = 1, 6
			if(rvAvailability(j) > 0.) then
				print *, j, rvAvailability(j), rvNoise(j), rvTop(j)-rvBottom(j), rvMeanVariation(j)
			end if
		end do
		print *
	end do
	print *
	
	! Test 5: Generate horizontal wind time series at various heights
	print *, "Test 5: Generate wind series"
	iRetCode = tSodar % sodarWindSeries(70, 10, rvTimeStamp, rvVel070, rvDir070)
	print *, "Return code for  70m: ", iRetCode, "  (expected: 0)"
	iRetCode = tSodar % sodarWindSeries(170, 10, rvTimeStamp, rvVel170, rvDir170)
	print *, "Return code for 170m: ", iRetCode, "  (expected: 0)"
	iRetCode = tSodar % sodarWindSeries(270, 10, rvTimeStamp, rvVel270, rvDir270)
	print *, "Return code for 270m: ", iRetCode, "  (expected: 0)"
	iRetCode = tSodar % sodarWindSeries(370, 10, rvTimeStamp, rvVel370, rvDir370)
	print *, "Return code for 370m: ", iRetCode, "  (expected: 0)"
	print *
	print *, "Date, Vel(70m), Dir(70m), Vel(170m), Dir(170m), Vel(270m), Dir(270m), Vel(370m), Dir(370m)"
	do i = 1, size(rvTimeStamp)
		iRetCode = tDate % fromEpoch(rvTimeStamp(i))
		sDateTime = tDate % toISO()
		print "(a, 8(',',f6.1))", &
			sDateTime, &
			rvVel070(i), rvDir070(i), &
			rvVel170(i), rvDir170(i), &
			rvVel270(i), rvDir270(i), &
			rvVel370(i), rvDir370(i)
	end do
	print *

end program SodarChecker
