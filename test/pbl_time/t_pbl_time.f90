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
	print *, "Maximum Epoc value: ", rEpoch, "  (Return code = ", iRetCode, "  - Expected = 0)"
	iRetCode  = dt % fromEpoch(253402300800.0d0)
	sDateTime = dt % toIso()
	print *, "Invalid date-time: ", trim(sDateTime), "  (Return code = ", iRetCode, "  - Expected = 1)"
	iRetCode  = dt % fromEpoch(0.0d0)
	sDateTime = dt % toIso()
	print *, "Invalid date-time: ", trim(sDateTime), "  (Return code = ", iRetCode, "  - Expected = 0)"
	iRetCode  = dt % fromEpoch(-1.0d0)
	sDateTime = dt % toIso()
	print *, "Invalid date-time: ", trim(sDateTime), "  (Return code = ", iRetCode, "  - Expected = 1)"
	print *
	
end program test_pbl_time
