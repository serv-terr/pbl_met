! Test program for pbl_stat module routines
!
! This code is part of the pbl_met library.
!
! This is open-source code, covered by the lGPL 3.0 license.
!
program test_pbl_stat

	use pbl_met
	
	implicit none
	
	! Locals
	real, dimension(:), allocatable	:: rvX
	real, dimension(:), allocatable	:: rvY

	! RangeInvalidate
	! -1- Normal case
	allocate(rvX(5))
	rvX = [-70., -40., 0., 60., 180.]
	call RangeInvalidate(rvX, -40., 60.)
	print *, "RangeInvalidate - Test 1"
	print *, "Expected: ", [NaN, -40., 0., 60., NaN]
	print *, "Found:    ", rvX
	print *
	! -1- Normal case
	rvX = [-70., -40., NaN, 60., 180.]
	call RangeInvalidate(rvX, -40., 60.)
	print *, "RangeInvalidate - Test 2"
	print *, "Expected: ", [NaN, -40., NaN, 60., NaN]
	print *, "Found:    ", rvX
	print *
	! -1- Boundary case: signal is not allocated
	deallocate(rvX)
	call RangeInvalidate(rvX, -40., 60.)
	print *, "RangeInvalidate - Test 3"
	print *, "Expected: "
	print *, "Found:    ", rvX
	print *
	! -1- Boundary case: zero-length signal
	deallocate(rvX)
	allocate(rvX(0))
	call RangeInvalidate(rvX, -40., 60.)
	print *, "RangeInvalidate - Test 4"
	print *, "Expected: "
	print *, "Found:    ", rvX
	print *
	
end program test_pbl_stat
