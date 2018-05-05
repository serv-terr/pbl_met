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
	type(DateTime)		:: dt
	character(len=23)	:: sDateTime
	integer				:: iRetCode
	real(8)				:: rEpoch
	
	! Test 1: which is the largest representable date?
	print *, "Test 1 - Common date and time manipulations related to REAL(8) time stamps"
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
