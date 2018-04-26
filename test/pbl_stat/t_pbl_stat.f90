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
	call testPairInvalidate()
	call testRangeClip()
	call testGetValidOnly()
	
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
		print *, "Valid:    ", .valid. rvX
		print *, "Invalid:  ", .invalid. rvX
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
		
		! Leave
		deallocate(rvX)
		
	end subroutine testRangeInvalidate
	
	
	subroutine testRangeClip()
	
		! Routine arguments
		! -none-
		
		! Locals
		real, dimension(:), allocatable	:: rvX
		
		! Normal case
		allocate(rvX(5))
		rvX = [-70., -40., 0., 60., 180.]
		call RangeClip(rvX, -40., 60.)
		print *, "RangeClip - Test 1"
		print *, "Expected: ", [-40., -40., 0., 60., 60.]
		print *, "Found:    ", rvX
		print *
		
		! Normal case
		rvX = [-70., -40., NaN, 60., 180.]
		call RangeClip(rvX, -40., 60.)
		print *, "RangeClip - Test 2"
		print *, "Expected: ", [-40., -40., NaN, 60., 60.]
		print *, "Found:    ", rvX
		print *
		
		! Boundary case: zero-length signal
		deallocate(rvX)
		allocate(rvX(0))
		call RangeClip(rvX, -40., 60.)
		print *, "RangeClip - Test 3"
		print *, "Expected: "
		print *, "Found:    ", rvX
		print *
		
		! Leave
		deallocate(rvX)
		
	end subroutine testRangeClip
	
	
	subroutine testPairInvalidate()
	
		! Routine arguments
		! -none-
		
		! Locals
		real, dimension(:), allocatable	:: rvX
		real, dimension(:), allocatable	:: rvY
		
		! Normal case
		allocate(rvX(5))
		allocate(rvY(5))
		rvX = [1., 2., 3., 4., 5.]
		rvY = [5., 4., 3., 2., 1.]
		print *, "PairInvalidate - Test 1"
		print *, "rvX = ", rvX
		print *, "rvY = ", rvY
		call PairInvalidate(rvX, rvY)
		print *, "Expected rvX = ",[1.,2.,3.,4.,5.]
		print *, "Expected rvY = ",[5.,4.,3.,2.,1.]
		print *, "Found rvX    = ",rvX
		print *, "Found rvY    = ",rvY
		print *
		
		! Normal case
		rvX = [1., NaN, 3., 4., 5.]
		rvY = [5., 4., 3., NaN, 1.]
		print *, "PairInvalidate - Test 2"
		print *, "rvX = ", rvX
		print *, "rvY = ", rvY
		call PairInvalidate(rvX, rvY)
		print *, "Expected rvX = ",[1.,NaN,3.,NaN,5.]
		print *, "Expected rvY = ",[5.,NaN,3.,NaN,1.]
		print *, "Found rvX    = ",rvX
		print *, "Found rvY    = ",rvY
		print *
		
		! Boundary case
		deallocate(rvX)
		deallocate(rvY)
		allocate(rvX(5))
		allocate(rvY(4))
		rvX = [1., NaN, 3., 4., 5.]
		rvY = [5., 4., 3., NaN]
		print *, "PairInvalidate - Test 3"
		print *, "rvX = ", rvX
		print *, "rvY = ", rvY
		call PairInvalidate(rvX, rvY)
		print *, "Expected rvX = ",[1.,NaN,3.,NaN,5.]
		print *, "Expected rvY = ",[5.,NaN,3.,NaN]
		print *, "Found rvX    = ",rvX
		print *, "Found rvY    = ",rvY
		print *
		
		! Boundary case
		deallocate(rvX)
		deallocate(rvY)
		allocate(rvX(4))
		allocate(rvY(5))
		rvX = [1., NaN, 3., 4.]
		rvY = [5., 4., 3., NaN, 1.]
		print *, "PairInvalidate - Test 4"
		print *, "rvX = ", rvX
		print *, "rvY = ", rvY
		call PairInvalidate(rvX, rvY)
		print *, "Expected rvX = ",[1.,NaN,3.,NaN]
		print *, "Expected rvY = ",[5.,NaN,3.,NaN, 1.]
		print *, "Found rvX    = ",rvX
		print *, "Found rvY    = ",rvY
		print *
		
		! Leave
		deallocate(rvX)
		deallocate(rvY)
		
	end subroutine testPairInvalidate
	
	
	subroutine testGetValidOnly()
	
		! Routine arguments
		! -none-
		
		! Locals
		real, dimension(:), allocatable	:: rvX
		real, dimension(:), allocatable	:: rvY
		
		! Normal case
		allocate(rvX(5))
		rvX = [-70., -40., NaN, 60., 180.]
		rvY = GetValidOnly(rvX)
		print *, "GetValidOnly - Test 1"
		print *, "Expected: ", [-70., -40., 60., 180.]
		print *, "Found:    ", rvY
		print *
		
		! Boundary case
		rvX = [NaN, NaN, NaN, NaN, NaN]
		rvY = GetValidOnly(rvX)
		print *, "GetValidOnly - Test 2"
		print *, "Expected: "
		print *, "Found:    ", rvY
		print *
		
		! Boundary case
		if(allocated(rvY)) deallocate(rvY)		! To show a good practice prior of calling
		rvX = [1.,2.,3.,4.,5.]
		rvY = GetValidOnly(rvX)
		print *, "GetValidOnly - Test 3"
		print *, "Expected: ", [1.,2.,3.,4.,5.]
		print *, "Found:    ", rvY
		print *
		
		! Leave
		deallocate(rvX)
		
	end subroutine testGetValidOnly
	
end program test_pbl_stat
