! Test program for pbl_time module routines
!
! This code is part of the pbl_met library.
!
! This is open-source code, covered by the lGPL 3.0 license.
!
program test_pbl_time

	use pbl_met
	
	implicit none
	
	! Locals
	integer				:: iYear, iMonth, iDay, iHour, iMinute, iSecond
	type(DateTime)		:: dt
	character(len=23)	:: sDateTime
	integer				:: iRetCode
	real(8)				:: rEpoch
	integer				:: iEpoch
	integer				:: iNumHours
	integer				:: i
	
	real(8), dimension(:), allocatable	:: rvTimeStamp
	integer, dimension(:), allocatable	:: ivTimeStamp
	integer, dimension(:), allocatable	:: ivTimeCode1
	integer, dimension(:), allocatable	:: ivTimeCode2
	integer, dimension(:), allocatable	:: ivYear1
	integer, dimension(:), allocatable	:: ivYear2
	
	! Time stamp related tests
	print *, "Test 1 - Maximum/minimum date/time values."
	call UnpackTime(0, iYear, iMonth, iDay, iHour, iMinute, iSecond)
	print "('Min date: ',i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))", iYear, iMonth, iDay, iHour, iMinute, iSecond
	call UnpackTime(huge(iYear), iYear, iMonth, iDay, iHour, iMinute, iSecond)
	print "('Max date: ',i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))", iYear, iMonth, iDay, iHour, iMinute, iSecond
		
	! Test 2: which is the largest representable date?
	print *, "Test 2 - Common date and time manipulations related to REAL(8) time stamps"
	iRetCode  = dt % fromEpoch(253402300799.999d0)
	sDateTime = dt % toIso()
	print *, "Maximum date-time: ", trim(sDateTime), "  (Return code = ", iRetCode, "  - Expected = 0)"
	dt = DateTime(10000, 1, 1, 0, 0, 0.d0)	! Modern Fortran default constructor!
	rEpoch = dt % toEpoch()
	print *, "Maximum Epoch value: ", rEpoch, "  (Return code = ", iRetCode, "  - Expected = 0)"
	iRetCode  = dt % fromEpoch(253402300800.0d0)
	sDateTime = dt % toIso()
	print *, "Invalid date-time: ", trim(sDateTime), "  (Return code = ", iRetCode, "  - Expected = 1)"
	iRetCode  = dt % fromEpoch(0.0d0)
	sDateTime = dt % toIso()
	print *, "Zero-epoch date-time: ", trim(sDateTime), "  (Return code = ", iRetCode, "  - Expected = 0)"
	iRetCode  = dt % fromEpoch(-1.0d0)
	sDateTime = dt % toIso()
	print *, "Invalid date-time: ", trim(sDateTime), "  (Return code = ", iRetCode, "  - Expected = 1)"
	print *
	
	! Test 3: Check the two forms of "timeEncode"
	iNumHours = 72
	allocate(rvTimeStamp(iNumHours), ivTimeStamp(iNumHours))
	dt = DateTime(2018, 3, 8, 0, 0, 0)
	rEpoch = dt % toEpoch()
	iEpoch = floor(rEpoch)
	rvTimeStamp = [(rEpoch + (i-1)*3600.d0, i = 1, iNumHours)]
	ivTimeStamp = [(iEpoch + (i-1)*3600, i = 1, iNumHours)]
	! Daily span, hourly step
	iRetCode = timeEncode(rvTimeStamp, 86400, 3600, ivTimeCode1)
	iRetCode = timeEncode(ivTimeStamp, 86400, 3600, ivTimeCode2)
	iRetCode = timeGetYear(rvTimeStamp, ivYear1)
	iRetCode = timeGetYear(ivTimeStamp, ivYear2)
	print *, "i,  Idx.1,  Idx.2, Year.1, Year.2"
	do i = 1, iNumHours
		print *, i, ivTimeCode1(i), ivTimeCode2(i), ivYear1(i), ivYear2(i)
	end do
	print *
	
	! Test 4: Some invalid values
	rvTimeStamp(3) = NaN_8
	ivTimeStamp(3) = 0
	! Daily span, hourly step
	iRetCode = timeEncode(rvTimeStamp, 86400, 3600, ivTimeCode1)
	iRetCode = timeEncode(ivTimeStamp, 86400, 3600, ivTimeCode2)
	iRetCode = timeGetYear(rvTimeStamp, ivYear1)
	iRetCode = timeGetYear(ivTimeStamp, ivYear2)
	print *, "i,  Idx.1,  Idx.2, Year.1, Year.2"
	do i = 1, iNumHours
		print *, i, ivTimeCode1(i), ivTimeCode2(i), ivYear1(i), ivYear2(i)
	end do
	print *
	deallocate(rvTimeStamp, ivTimeStamp, ivTimeCode1, ivTimeCode2, ivYear1, ivYear2)
	
end program test_pbl_time
