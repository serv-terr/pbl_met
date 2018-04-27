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
	call testMean()
	call testStdDev()
	
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
	
	
	subroutine testMean()
	
		! Routine arguments
		! -none-
		
		! Locals
		real, dimension(:), allocatable	:: rvX
		real							:: rMean, rValid
		integer							:: i
		
		! Normal case
		allocate(rvX(5))
		rvX = [-180., -60., NaN, 60., 180.]
		rMean = Mean(rvX)
		print *, "Mean - Test 1"
		print *, "Expected: ", 0.
		print *, "Found:    ", rMean
		print *
		
		! Normal case
		rMean = Mean(rvX, rValid)
		print *, "Mean - Test 2"
		print *, "Expected: ", 0., 0.8
		print *, "Found:    ", rMean, rValid
		print *
		
		! Boundary case
		rvX = NaN
		rMean = Mean(rvX, rValid)
		print *, "Mean - Test 3"
		print *, "Expected: ", NaN, 0.
		print *, "Found:    ", rMean, rValid
		print *
		
		! Mass-test
		print *, "Mean - Test 4"
		do i = 1, 15
			deallocate(rvX)
			allocate(rvX(2**(i+1)))
			call random_number(rvX)
			call RangeInvalidate(rvX, 0., 0.5)
			rMean = Mean(rvX, rValid)
			print *, i, rMean, rValid
		end do
		print *
		
		! Leave
		deallocate(rvX)
		
	end subroutine testMean
	
	
	subroutine testStdDev()
	
		! Routine arguments
		! -none-
		
		! Locals
		real, dimension(:), allocatable	:: rvX
		real							:: rSd, rMean, rValid
		integer							:: i
		real, dimension(-5:5), parameter	:: rvStd = [ &
			134.257, 134.224, 134.198, 134.179, 134.168, &
			134.164, &
			134.168, 134.179, 134.198, 134.224, 134.257 &
		]
		
		! Normal case
		allocate(rvX(5))
		rvX = [-180., -60., NaN, 60., 180.]
		rSd = StdDev(rvX)
		print *, "StdDev - Test 1"
		print *, "Expected: ", 120.*sqrt(5.)
		print *, "Found:    ", rSd
		print *
		
		! Normal case
		rSd = StdDev(rvX, rValidFraction=rValid)
		print *, "StdDev - Test 2"
		print *, "Expected: ", 120.*sqrt(5./4.), 0.8
		print *, "Found:    ", rSd, rValid
		print *
		
		! Normal case
		rSd = StdDev(rvX, rMeanIn=0., rValidFraction=rValid)
		print *, "StdDev - Test 3"
		print *, "Expected: ", 120.*sqrt(5./4.), 0.8
		print *, "Found:    ", rSd, rValid
		print *
		
		! Boundary case
		rvX = NaN
		rSd = StdDev(rvX, rValidFraction=rValid)
		print *, "StdDev - Test 4"
		print *, "Expected: ", NaN, 0.
		print *, "Found:    ", rSd, rValid
		print *
		
		! Boundary case
		rvX = [-180., -60., NaN, 60., 180.]
		rSd = StdDev(rvX, rMeanIn = NaN)
		print *, "StdDev - Test 5"
		print *, "Expected: ", NaN
		print *, "Found:    ", rSd
		print *
		
		! Mass-test
		print *, "StdDev - Test 6"
		print *, "Warning: comparison values known to 3 decimals"
		do i = -5, 5
			rSd = StdDev(rvX, rMeanIn=float(i))
			print *, i, rSd, rvStd(i)
		end do
		print *
		
		! Mass-test
		print *, "StdDev - Test 7"
		do i = 1, 15
			deallocate(rvX)
			allocate(rvX(2**(i+1)))
			call random_number(rvX)
			call RangeInvalidate(rvX, 0., 0.5)
			rSd = StdDev(rvX, rValidFraction=rValid)
			print *, i, rSd, rValid
		end do
		print *
		
		! Leave
		deallocate(rvX)
		
	end subroutine testStdDev
	
end program test_pbl_stat
