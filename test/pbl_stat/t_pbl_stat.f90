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
	call testCov()
	call testAutoCov()
	call testAutoCorr()
	call testPartialAutoCorr()
	call testCrossCov()
	
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
			print "(i2,2(1x,f7.3))", i, rSd, rvStd(i)
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
	
	
	subroutine testCov()
	
		! Routine arguments
		! -none-
		
		! Locals
		real, dimension(:), allocatable	:: rvX
		real, dimension(:), allocatable	:: rvY
		real							:: rCov
		
		! Normal case
		allocate(rvX(5))
		allocate(rvY(5))
		rvX = [1., 2., 1., 2., 1.]
		rvY = [2., 1., 2., 1., 2.]
		rCov = Cov(rvX, rvY)
		print *, "Cov - Test 1"
		print *, "Expected: ", -0.3
		print *, "Found:    ", rCov
		print *
		
		! Normal case
		rvX = [1., 2., NaN, 2., 1.]
		rCov = Cov(rvX, rvY)
		print *, "Cov - Test 2"
		print *, "Expected: ", -0.3333333
		print *, "Found:    ", rCov
		print *
		
		! Boundary case
		rvX = [NaN, 2., NaN, 2., 1.]
		rvY = [2., NaN, NaN, 1., NaN]
		rCov = Cov(rvX, rvY)
		print *, "Cov - Test 3"
		print *, "Expected: ", NaN
		print *, "Found:    ", rCov
		print *
		
		! Normal case
		deallocate(rvX)
		allocate(rvX(6))
		rvX = [1., 2., 1., 2., 1., 7.]
		rvY = [2., 1., 2., 1., 2.]
		rCov = Cov(rvX, rvY)
		print *, "Cov - Test 4"
		print *, "Expected: ", NaN
		print *, "Found:    ", rCov
		print *
		
		! Leave
		deallocate(rvY)
		deallocate(rvX)
		
	end subroutine testCov
	
	
	subroutine testAutoCov()
	
		! Routine arguments
		! -none-
		
		! Locals
		real, dimension(:), allocatable	:: rvX
		real, dimension(:), allocatable	:: rvAcov
		real, dimension(:), allocatable	:: rvAcovRef
		real, dimension(:), allocatable	:: rvAcov2nd
		integer							:: i
		integer							:: iRetCode
		
		! Normal case
		allocate(rvX(5))
		allocate(rvACov(0:4))
		allocate(rvACovRef(0:4))
		allocate(rvACov2nd(0:4))
		rvX = [1.0, 1.9, 3.1, 3.9, 5.2]
		rvAcovRef = [2.17360,0.83232,-0.19456,-0.84384,-0.88072]
		iRetCode = AutoCov(rvX, rvACov)
		print *, "AutoCov - Test 1: return code = ", iRetCode
		iRetCode = AutoCov(rvX, rvACov2nd, ACV_2ND_ORDER)
		print *, "AutoCov - Test 2: return code = ", iRetCode
		print *, "Printing combined results"
		print *, "Lag Expected Found.2nd.ord Found.general"
		do i = 0, 4
			print "(i1,3(4x,f8.5))", i, rvAcovRef(i), rvACov2nd(i), rvACov(i)
		end do
		print *
		
		! Normal case
		rvX = [1.0, 1.9, NaN, 3.9, 5.2]
		iRetCode = AutoCov(rvX, rvACov)
		print *, "AutoCov - Test 3: return code = ", iRetCode
		iRetCode = AutoCov(rvX, rvACov2nd, ACV_2ND_ORDER)
		print *, "AutoCov - Test 4: return code = ", iRetCode
		print *, "Printing combined results"
		print *, "Lag Found.2nd.ord Found.general"
		do i = 0, 4
			print "(i1,2(4x,f8.5))", i, rvACov2nd(i), rvACov(i)
		end do
		print *
		
		! Boundary
		rvX = NaN
		iRetCode = AutoCov(rvX, rvACov)
		print *, "AutoCov - Test 5: return code = ", iRetCode
		iRetCode = AutoCov(rvX, rvACov2nd, ACV_2ND_ORDER)
		print *, "AutoCov - Test 6: return code = ", iRetCode
		print *, "Printing combined results"
		print *, "Lag Found.2nd.ord Found.general"
		do i = 0, 4
			print "(i1,2(4x,f8.5))", i, rvACov2nd(i), rvACov(i)
		end do
		print *
		
		! Leave
		deallocate(rvACov2nd)
		deallocate(rvACovRef)
		deallocate(rvACov)
		deallocate(rvX)
		
	end subroutine testAutoCov
	
	
	subroutine testAutoCorr()
	
		! Routine arguments
		! -none-
		
		! Locals
		real, dimension(:), allocatable	:: rvX
		real, dimension(:), allocatable	:: rvAcor
		real, dimension(:), allocatable	:: rvAcorRef
		real, dimension(:), allocatable	:: rvAcor2nd
		integer							:: i
		integer							:: iRetCode
		
		! Normal case
		allocate(rvX(5))
		allocate(rvACor(0:4))
		allocate(rvACorRef(0:4))
		allocate(rvACor2nd(0:4))
		rvX = [1.0, 1.9, 3.1, 3.9, 5.2]
		rvAcorRef = [1.00000000,0.38292234,-0.08951049,-0.38822230,-0.40518955]
		rvACor    = 0.
		rvACor2nd = 0.
		iRetCode = AutoCorr(rvX, rvACor)
		print *, "AutoCorr - Test 1: return code = ", iRetCode
		iRetCode = AutoCorr(rvX, rvACor2nd, ACV_2ND_ORDER)
		print *, "AutoCorr - Test 2: return code = ", iRetCode
		print *, "Printing combined results"
		print *, "Lag Expected Found.2nd.ord Found.general"
		do i = 0, 4
			print "(i1,5(4x,f9.7))", i, rvAcorRef(i), rvACor2nd(i), rvACor(i), &
			                            rvAcorRef(i)-rvACor2nd(i),(rvAcorRef(i)-rvACor2nd(i))/rvAcorRef(i)
		end do
		print *
		
		! Normal case
		rvX = [1.0, 1.9, NaN, 3.9, 5.2]
		rvACor    = 0.
		rvACor2nd = 0.
		iRetCode = AutoCorr(rvX, rvACor)
		print *, "AutoCorr - Test 3: return code = ", iRetCode
		iRetCode = AutoCorr(rvX, rvACor2nd, ACV_2ND_ORDER)
		print *, "AutoCorr - Test 4: return code = ", iRetCode
		print *, "Printing combined results"
		print *, "Lag Found.2nd.ord Found.general"
		do i = 0, 4
			print "(i1,2(4x,f8.5))", i, rvACor2nd(i), rvACor(i)
		end do
		print *
		
		! Boundary
		rvX = NaN
		rvACor    = 0.
		rvACor2nd = 0.
		iRetCode = AutoCorr(rvX, rvACor)
		print *, "AutoCorr - Test 5: return code = ", iRetCode
		iRetCode = AutoCorr(rvX, rvACor2nd, ACV_2ND_ORDER)
		print *, "AutoCorr - Test 6: return code = ", iRetCode
		print *, "Printing combined results"
		print *, "Lag Found.2nd.ord Found.general"
		do i = 0, 4
			print "(i1,2(4x,f8.5))", i, rvACor2nd(i), rvACor(i)
		end do
		print *
		
		! Leave
		deallocate(rvACor2nd)
		deallocate(rvACorRef)
		deallocate(rvACor)
		deallocate(rvX)
		
	end subroutine testAutoCorr

	
	subroutine testPartialAutoCorr()
	
		! Routine arguments
		! -none-
		
		! Locals
		real, dimension(:), allocatable	:: rvX
		real, dimension(:), allocatable	:: rvAcov
		real, dimension(:), allocatable	:: rvAcovRef
		real, dimension(:), allocatable	:: rvPacf
		real, dimension(:), allocatable	:: rvPacfRef
		integer							:: i
		integer							:: iRetCode
		
		! Normal case
		allocate(rvX(5))
		allocate(rvACov(0:4))
		allocate(rvACovRef(0:4))
		allocate(rvPacf(4))
		allocate(rvPacfRef(4))
		rvX = [1.0, 1.9, 3.1, 3.9, 5.2]
		rvAcovRef = [2.17360,0.83232,-0.19456,-0.84384,-0.88072]
		rvPacfRef = [0.3829223,-0.2767145,-0.3026571,-0.2005165]
		iRetCode = AutoCov(rvX, rvACov, ACV_2ND_ORDER)
		print *, "Autocovariance computed, return code = ", iRetCode
		rvPacf   = PartialAutoCorr(rvACov)
		print *, "PartialAutoCorr - Test 1"
		print *, "Lag Expected Found"
		do i = 1, 4
			print "(i1,2(2x,f9.7))", i, rvPacfRef(i), rvPacf(i)
		end do
		print *
		
		! Leave
		deallocate(rvPacfRef)
		deallocate(rvPacf)
		deallocate(rvACovRef)
		deallocate(rvACov)
		deallocate(rvX)
		
	end subroutine testPartialAutoCorr
	
	
	subroutine testCrossCov()
	
		! Routine arguments
		! -none-
		
		! Locals
		real, dimension(:), allocatable	:: rvX
		real, dimension(:), allocatable	:: rvy
		real, dimension(:), allocatable	:: rvCcovRef
		real, dimension(:), allocatable	:: rvCcov
		real, dimension(:), allocatable	:: rvCcov2nd
		integer							:: i
		integer							:: iRetCode
		
		! Normal case
		allocate(rvX(9))
		allocate(rvY(9))
		allocate(rvCCovRef(-4:4))
		allocate(rvCCov2nd(-4:4))
		allocate(rvCCov(-4:4))
		rvX = [1.,2.,3.,4.,5.,4.,3.,2.,1.]
		rvY = [3.,4.,5.,4.,3.,2.,1.,1.,1.]
		rvCCovRef = [-0.6872428,-1.1728395,-1.3374486,-0.6748971,0.4814815,1.1522634,1.3415638,0.8271605,0.1399177]
		iRetCode = CrossCov(rvX, rvY, rvCCov)
		print *, "CrossCov - Test 1: return code = ", iRetCode
		iRetCode = CrossCov(rvX, rvY, rvCCov2nd, ACV_2ND_ORDER)
		print *, "CrossCov - Test 2: return code = ", iRetCode
		print *, "Printing results"
		print *, "Lag Expected Found.2nd.ord Found"
		do i = -4, 4
			print "(i2,3(4x,f8.5))", i, rvCCovRef(i), rvCCov2nd(i), rvCCov(i)
		end do
		print *
		
		! Boundary
		rvX = [1.,2.,3.,4.,NaN,4.,3.,2.,1.]
		rvY = [3.,4.,5.,4.,3.,2.,1.,1.,1.]
		iRetCode = CrossCov(rvX, rvY, rvCCov)
		print *, "CrossCov - Test 3: return code = ", iRetCode
		print *, "Printing results"
		print *, "Lag Found"
		do i = -4, 4
			print "(i2,2(4x,f8.5))", i, rvCCov(i)
		end do
		print *
		
		! Boundary
		rvX = NaN
		rvY = [3.,4.,5.,4.,3.,2.,1.,1.,1.]
		iRetCode = CrossCov(rvX, rvY, rvCCov)
		print *, "CrossCov - Test 4: return code = ", iRetCode
		print *, "Printing results"
		print *, "Lag Found"
		do i = -4, 4
			print "(i2,2(4x,f8.5))", i, rvCCov(i)
		end do
		print *
		
		! Leave
		deallocate(rvCCov)
		deallocate(rvCCov2nd)
		deallocate(rvCCovRef)
		deallocate(rvY)
		deallocate(rvX)
		
	end subroutine testCrossCov
	
	
end program test_pbl_stat
