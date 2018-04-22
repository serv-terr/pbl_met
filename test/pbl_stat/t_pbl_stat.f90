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
	
	! Perform actual tests
	
	call testRangeInvalidate()
	
contains

	subroutine testRangeInvalidate()
	
		! Routine arguments
		! -none-
		
		! Locals
		real, dimension(:), allocatable	:: rvX
		
		! Normal case
		allocate(rvX(5))
		rvX = [-70., -40., 0., 60., 180.]
		call RangeInvalidate(rvX, -40., 60.)
		print *, "RangeInvalidate - Test 1"
		print *, "Expected: ", [NaN, -40., 0., 60., NaN]
		print *, "Found:    ", rvX
		print *
		
		! Normal case
		rvX = [-70., -40., NaN, 60., 180.]
		call RangeInvalidate(rvX, -40., 60.)
		print *, "RangeInvalidate - Test 2"
		print *, "Expected: ", [NaN, -40., NaN, 60., NaN]
		print *, "Found:    ", rvX
		print *
		
		! Boundary case: signal is not allocated
		!     Dropped from test list, as this code block
		!     is illegal in Fortran.
		!deallocate(rvX)
		!call RangeInvalidate(rvX, -40., 60.)
		!print *, "RangeInvalidate - Test 3"
		!print *, "Expected: "
		!print *, "Found:    ", rvX
		!print *
		
		! Boundary case: zero-length signal
		deallocate(rvX)
		allocate(rvX(0))
		call RangeInvalidate(rvX, -40., 60.)
		print *, "RangeInvalidate - Test 3"
		print *, "Expected: "
		print *, "Found:    ", rvX
		print *
		
	end subroutine testRangeInvalidate
	
end program test_pbl_stat
