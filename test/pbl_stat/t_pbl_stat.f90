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
	call testCrossCorr()
	call testEulerianTime()
	call testRemoveLinearTrend()
	call testTimeSeries()
	call testQuantile()
	call testSkewness()
	
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
	
	
	subroutine testCrossCorr()
	
		! Routine arguments
		! -none-
		
		! Locals
		real, dimension(:), allocatable	:: rvX
		real, dimension(:), allocatable	:: rvy
		real, dimension(:), allocatable	:: rvCcorRef
		real, dimension(:), allocatable	:: rvCcor
		real, dimension(:), allocatable	:: rvCcor2nd
		integer							:: i
		integer							:: iRetCode
		
		! Normal case
		allocate(rvX(9))
		allocate(rvY(9))
		allocate(rvCCorRef(-4:4))
		allocate(rvCCor2nd(-4:4))
		allocate(rvCCor(-4:4))
		rvX = [1.,2.,3.,4.,5.,4.,3.,2.,1.]
		rvY = [3.,4.,5.,4.,3.,2.,1.,1.,1.]
		rvCCorRef = [-0.36963551,-0.63081510,-0.71935055,-0.36299536,0.25896620,0.61974817,0.72156394,0.44489065,0.07525513]
		iRetCode = CrossCorr(rvX, rvY, rvCCor)
		print *, "CrossCorr - Test 1: return code = ", iRetCode
		iRetCode = CrossCorr(rvX, rvY, rvCCor2nd, ACV_2ND_ORDER)
		print *, "CrossCorr - Test 2: return code = ", iRetCode
		print *, "Printing results"
		print *, "Lag Expected Found.2nd.ord Found"
		do i = -4, 4
			print "(i2,3(4x,f8.5))", i, rvCCorRef(i), rvCCor2nd(i), rvCCor(i)
		end do
		print *
		
		! Boundary
		rvX = [1.,2.,3.,4.,NaN,4.,3.,2.,1.]
		rvY = [3.,4.,5.,4.,3.,2.,1.,1.,1.]
		iRetCode = CrossCorr(rvX, rvY, rvCCor)
		print *, "CrossCorr - Test 3: return code = ", iRetCode
		print *, "Printing results"
		print *, "Lag Found"
		do i = -4, 4
			print "(i2,2(4x,f8.5))", i, rvCCor(i)
		end do
		print *
		
		! Boundary
		rvX = NaN
		rvY = [3.,4.,5.,4.,3.,2.,1.,1.,1.]
		iRetCode = CrossCorr(rvX, rvY, rvCCor)
		print *, "CrossCorr - Test 4: return code = ", iRetCode
		print *, "Printing results"
		print *, "Lag Found"
		do i = -4, 4
			print "(i2,2(4x,f8.5))", i, rvCCor(i)
		end do
		print *
		
		! Leave
		deallocate(rvCCor)
		deallocate(rvCCor2nd)
		deallocate(rvCCorRef)
		deallocate(rvY)
		deallocate(rvX)
		
	end subroutine testCrossCorr
	
	
	subroutine testEulerianTime()
	
		! Routine arguments
		! --none--
		
		! Locals
		real, dimension(:), allocatable	:: rvX
		real, dimension(:), allocatable	:: rvACorr
		integer							:: iRetCode
		character(len=32)				:: sBuffer
		integer							:: iNumData
		integer							:: iData
		integer							:: i
		real							:: rSamplingRate
		real							:: rEuler
		
		! Gather all data from test file (generated using
		! routine 'test.eulerian.time' in 'ref_t_pbl_stat.R'
		! -1- Phase 1: Count data
		open(10, file='euler.dat', status='old', action='read')
		iNumData = -1	! This starting limit to account for the first line (a header), to be skipped
		do
			read(10, "(a)", iostat=iRetCode) sBuffer
			if(iRetCode /= 0) exit
			iNumData = iNumData + 1
		end do
		! -1- Phase 2: Reserve workspace and read data into it
		rewind(10)
		read(10, "(a)") sBuffer	! Skip header
		allocate(rvX(iNumData))
		do iData = 1, iNumData
			read(10, *) rvX(iData)
		end do
		close(10)
		
		! Add some (fictive) context data
		rSamplingRate = 10.0
		
		! Estimate Eulerian decorrelation time for test 1
		iRetCode = EulerianTime(rSamplingRate, rvX, 60., rEuler, rvACorr)
		print *, "EulerianTime - Test 1 - Return code: ", iRetCode
		print *, "Estimated value = ", rEuler, "  (expected any positive value)"
		print *
		print *,'Lag  ACF   Significance.95'
		do i = 0, size(rvACorr)
			print *, i, rvACorr(i), 1.96 / sqrt(float(iNumData) - i)
			if(rvACorr(i) < 1.96 / sqrt(float(iNumData) - i)) exit
		end do
		print *
		
		! Normal: uncorrelated time series
		call random_number(rvX)
		iRetCode = EulerianTime(rSamplingRate, rvX, 60., rEuler)
		print *, "EulerianTime - Test 2 - Return code: ", iRetCode
		print *, "Estimated value = ", rEuler, "  - Expected:", 0.0
		print *
		
		! Normal: constant time series
		rvX = 1.
		iRetCode = EulerianTime(rSamplingRate, rvX, 60., rEuler)
		print *, "EulerianTime - Test 3 - Return code: ", iRetCode
		print *, "Estimated value = ", rEuler, "  - Expected:", 60.0, " with positive (error) return code"
		print *
		
		! Boundary: holed time series
		rvX(1) = NaN
		iRetCode = EulerianTime(rSamplingRate, rvX, 60., rEuler)
		print *, "EulerianTime - Test 4 - Return code: ", iRetCode
		print *, "Estimated value = ", rEuler, "  - Expected:", NaN, " with positive (error) return code"
		print *
		
		deallocate(rvACorr)
		deallocate(rvX)
		
	end subroutine testEulerianTime
	
	
	subroutine testRemoveLinearTrend()
	
		! Routine arguments
		! -none-
		
		! Locals
		real(8), dimension(-100:100)	:: rvX
		real, dimension(-100:100)		:: rvY
		integer							:: i
		integer							:: iRetCode
		real(8)							:: rMultiplier
		real(8)							:: rOffset
		
		! Test 1 - Normal
		rvX = [(0.1d0*(i+100), i=-100,100)]
		rvY = [(10.+0.1*i, i=-100,100)]
		print *, 'RemoveLinearTrend - Test 1'
		print *, 'Mean before detrending:  ', sum(rvY) / size(rvY)
		print *, 'Slope before detrending: ', 0.1
		iRetCode = RemoveLinearTrend(rvX, rvY, rMultiplier, rOffset)
		print *, 'Mean after detrending:  ', sum(rvY) / size(rvY)
		print *, 'Slope after detrending: ', (rvY(100) - rvY(-100)) / (rvX(100) - rvX(-100))
		print *, 'Multiplier:             ', sngl(rMultiplier)
		print *, 'Offset:                 ', sngl(rOffset)
		print *
		
		! Test 2 - Boundary
		rvX(0) = NaN_8
		print *, 'RemoveLinearTrend - Test 2'
		iRetCode = RemoveLinearTrend(rvX, rvY, rMultiplier, rOffset)
		print *, 'Result: ', iRetCode, '  Expected: any non-zero'
		
		! Test 3 - Boundary
		rvX(0) = -100.d0
		rvY(0) = NaN
		print *, 'RemoveLinearTrend - Test 2'
		iRetCode = RemoveLinearTrend(rvX, rvY, rMultiplier, rOffset)
		print *, 'Result: ', iRetCode, '  Expected: any non-zero'
		
	end subroutine testRemoveLinearTrend
	
	
	subroutine testTimeSeries()
	
		! Routine arguments
		! -none-
		
		! Locals
		integer				:: iRetCode
		character(len=256)	:: sInputFile
		type(TimeSeries)	:: ts, tsCopy, tsReduced
		type(DateTime)		:: tm
		real				:: rValid, rMin, rMean, rStdDev, rMax
		integer				:: i
		integer				:: iNumData
		integer				:: iNumLines
		integer				:: iLine
		character(len=128)	:: sBuffer
		real(8)				:: rMinDelta, rDelta, rMaxDelta
		real(8)				:: hold8_1
		real				:: hold4_1
		real(8)				:: hold8_2
		real				:: hold4_2
		real(8)				:: rTimeStep
		real(8)				:: rTimeStamp
		real				:: rValue
		real(8)				:: rMinTimeStamp
		real(8)				:: rMaxTimeStamp
		integer				:: iNumGaps
		integer				:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		character(len=23)	:: sTimeStamp
		real(8), dimension(:), allocatable	:: rvTimeStamp
		real, dimension(:), allocatable		:: rvValue
		real, dimension(:), allocatable		:: rvMean
		real, dimension(:), allocatable		:: rvStDev
		real, dimension(:), allocatable		:: rvMin
		real, dimension(:), allocatable		:: rvMax
		integer								:: iFirstComma
		integer								:: iCurrentTime
		integer, dimension(:), allocatable	:: ivNumData
	
		! Assign file name(s)
		sInputFile = "./Moggio_Temp.csv"
	
		! Get data, assuming ARPA Lombardy format
		open(10, file=sInputFile, status='old', action='read', iostat=iRetCode)
		if(iRetCode /= 0) then
			print *, "tStat:: error: Input file not opened - check name, position and access rights"
			stop
		end if
		iNumLines = -1	! Take header into account
		iFirstComma = 0
		do
			read(10, "(a)", iostat=iRetCode) sBuffer
			if(iRetCode /= 0) exit
			iNumLines = iNumLines + 1
			if(iNumLines == 1) iFirstComma = index(sBuffer, ",")
		end do
		if(iNumLines <= 0) then
			print *, "tStat:: error: No useable data in file"
		end if
		allocate(rvTimeStamp(iNumLines), rvValue(iNumLines))
		rewind(10)
		read(10, "(a)") sBuffer	! Skip header
		do iLine = 1, iNumLines
			read(10, "(a)") sBuffer
			read(sBuffer(iFirstComma+1:iFirstComma+16), "(i4,4(1x,i2))") &
				iYear, iMonth, iDay, iHour, iMinute
			call PackTime(iCurrentTime, iYear, iMonth, iDay, iHour, iMinute, 0)
			rvTimeStamp(iLine) = iCurrentTime
			read(sBuffer(iFirstComma+18:), *) rvValue(iLine)
		end do
		close(10)
		
		! Check time stamps are ordered, and whether they form a gap-less series (they should
		! for ARPA Lombardy proper data files)
		rMinDelta =  huge(rMinDelta)
		rMaxDelta = -huge(rMaxDelta)
		do iLine = 2, size(rvTimeStamp)
			rDelta = rvTimeStamp(iLine) - rvTimeStamp(iLine-1)
			rMinDelta = min(rMinDelta, rDelta)
			rMaxDelta = max(rMaxDelta, rDelta)
		end do
		if(rMinDelta /= rMaxDelta) then
			print *,"tStat:: error: Time stamps are not well-ordered as in ARPA Lombardy files"
			stop
		end if
		
		! Populate time series from input file
		iRetCode = ts % createFromDataVector(rvValue, rvTimeStamp(1), rDelta)
		if(iRetCode /= 0) then
			print *, "tStat:: error: Creation of time series failed with return code = ", iRetCode
			stop
		end if
		
		! Range-invalidate data, assuming they are temperature reading (other quantities will demand different limits
		! (this code is for illustration only)
		call ts % rangeInvalidate(-40., +60.)
		
		! Compute and print a short summary on values
		call ts % summary(iNumData, rValid, rMin, rMean, rStdDev, rMax)
		print *,"Summary on data values"
		print *,"======================"
		print *
		print *,"Number of data (valid and invalid) = ", iNumData
		print *,"Fraction of valid data             = ", rValid, "%"
		print *,"Minimum                            = ", rMin
		print *,"Mean                               = ", rMean
		print *,"Standard deviation                 = ", rStdDev
		print *,"Maximum                            = ", rMax
		print *
		
		! Check time is monotonic
		print *,'Test 2 - Check time stamp strict monotonicity (increasing)'
		print *,'Monotonic? ', ts % timeIsMonotonic(), "    (Expected: T)"
		print *
	
		! Check time is quasi monotonic
		print *,'Test 2 - Check time stamp weak monotonicity (non-decreasing)'
		print *,'Monotonic? ', ts % timeIsQuasiMonotonic(), "    (Expected: T)"
		print *
		
		! Check linear aggregator, by computing yearly average
		print *, 'Test 3.1 - Check linear aggregateLinear'
		print *, 'Yearly average'
		iRetCode = ts % aggregateLinear(TDELTA_YEAR, FUN_MEAN, tsReduced)
		print *, 'Ret.code = ', iRetCode
		print *, 'Resulting time series size = ', tsReduced % size()
		print *, 'TimeStamp, Mean'
		do i = 1, tsReduced % size()
			iRetCode = tsReduced % getSingleItem(i, rTimeStamp, rValue)
			iRetCode = tm % fromEpoch(rTimeStamp)
			sTimeStamp = tm % toIso()
			print *, sTimeStamp, rValue
		end do
		print *, 'Monthly average'
		iRetCode = ts % aggregateLinear(TDELTA_YEARMONTH, FUN_MEAN, tsReduced, ivNumData)
		print *, 'Ret.code = ', iRetCode
		print *, 'Resulting time series size = ', tsReduced % size()
		print *, 'TimeStamp, Mean, Num.Data'
		do i = 1, tsReduced % size()
			iRetCode = tsReduced % getSingleItem(i, rTimeStamp, rValue)
			iRetCode = tm % fromEpoch(rTimeStamp)
			sTimeStamp = tm % toIso()
			print *, sTimeStamp, rValue, ivNumData(i)
		end do
		print *, 'Daily average'
		iRetCode = ts % aggregateLinear2(TDELTA_ONEDAY, rvTimeStamp, rvMean, rvStDev, rvMin, rvMax, ivNumData)
		print *, 'Ret.code = ', iRetCode
		print *, 'Resulting time series size = ', size(rvTimeStamp)
		print *, 'TimeStamp, Mean, StDev, Min, Max, Num.Data'
		do i = 1, size(rvTimeStamp)
			iRetCode = tm % fromEpoch(rvTimeStamp(i))
			sTimeStamp = tm % toIso()
			print *, sTimeStamp, rvMean(i), rvStDev(i), rvMin(i), rvMax(i), ivNumData(i)
		end do
		print *, 'Approx weekly average'
		iRetCode = ts % aggregateLinear(TDELTA_ONEDAY*7, FUN_MEAN, tsReduced, ivNumData)
		print *, 'Ret.code = ', iRetCode
		print *, 'Resulting time series size = ', tsReduced % size()
		print *, 'TimeStamp, Mean, Num.Data'
		do i = 1, tsReduced % size()
			iRetCode = tsReduced % getSingleItem(i, rTimeStamp, rValue)
			iRetCode = tm % fromEpoch(rTimeStamp)
			sTimeStamp = tm % toIso()
			print *, sTimeStamp, rValue, ivNumData(i)
		end do
		print *
		
		print *, 'Test 3.2 - Check aggregatePeriodic'
		print *, 'Typical day'
		iRetCode = ts % aggregatePeriodic(TDELTA_ONEDAY, TDELTA_ONEHOUR, rvMean, rvStDev, rvMin, rvMax, ivNumData)
		print *, 'Ret.code = ', iRetCode
		print *, 'Resulting time series size = ', size(rvMean)
		print *, 'Time.Index, Mean, StDev, Min, Max, Num.Data'
		do i = 1, size(rvMean)
			print *, i-1, rvMean(i), rvStDev(i), rvMin(i), rvMax(i), ivNumData(i)
		end do
		print *, 'Monthly typical days'
		do iMonth = 1, 12
			iRetCode = tsCopy % getMonth(ts, iMonth)
			print *, "Month = ", iMonth, "  - Return code = ", iRetCode
			if(iRetCode == 0) then
				iRetCode = tsCopy % aggregatePeriodic(TDELTA_ONEDAY, TDELTA_ONEHOUR, rvMean, rvStDev, rvMin, rvMax, ivNumData)
				print *, 'Ret.code = ', iRetCode
				print *, 'Resulting time series size = ', size(rvMean)
				print *, 'Time.Index, Mean, StDev, Min, Max, Num.Data'
				do i = 1, size(rvMean)
					print *, i-1, rvMean(i), rvStDev(i), rvMin(i), rvMax(i), ivNumData(i)
				end do
			end if
		end do
		print *, 'Boundary: monthly typical days with empty data set'
		iMonth = 1
		iRetCode = tsCopy % getMonth(ts, iMonth)
		print *, "Month = ", iMonth, "  - Return code = ", iRetCode
		call tsCopy % rangeInvalidate(100., 200.)	! Making sure all data are invalid
		iRetCode = tsCopy % getSingleItem(1, rTimeStamp, rValue)
		iRetCode = tsCopy % putSingleItem(1, rTimeStamp, 1.0)	! Make value at index 1 valid, so that the time series is non-empty
		if(iRetCode == 0) then
			iRetCode = tsCopy % aggregatePeriodic(TDELTA_ONEDAY, TDELTA_ONEHOUR, rvMean, rvStDev, rvMin, rvMax, ivNumData)
			print *, 'Ret.code = ', iRetCode
			print *, 'Resulting time series size = ', size(rvMean)
			print *, 'Time.Index, Mean, StDev, Min, Max, Num.Data'
			do i = 1, size(rvMean)
					print *, i-1, rvMean(i), rvStDev(i), rvMin(i), rvMax(i), ivNumData(i)
			end do
		end if
		print *
		
		! Make time stamp non-monotonic by exchanging the monotonic's first two elements
		iRetCode = ts % getSingleItem(1, hold8_1, hold4_1)
		iRetCode = ts % getSingleItem(2, hold8_2, hold4_2)
		iRetCode = ts % putSingleItem(1, hold8_2, hold4_2)
		iRetCode = ts % putSingleItem(2, hold8_1, hold4_1)
	
		! Check time is monotonic
		print *,'Test 4 - Check time stamp strict monotonicity (increasing)'
		print *,'Monotonic? ', ts % timeIsMonotonic(), "    (Expected: F)"
		print *
	
		! Check time is quasi monotonic
		print *,'Test 5 - Check time stamp weak monotonicity (non-decreasing)'
		print *,'Monotonic? ', ts % timeIsQuasiMonotonic(), "    (Expected: F)"
		print *
		
		! Check time is gapless
		print *, 'Test 6 - Check time stamp vector to be gapless'
		iRetCode = ts % populateFromTimeAndDataVectors(rvTimeStamp, rvValue)
		print *, 'Gapless? ', ts % timeIsGapless(), '   (Expected: T)'
		iRetCode = ts % getSingleItem(2, hold8_2, hold4_2)
		iRetCode = ts % putSingleItem(2, NaN_8, hold4_2)
		print *, 'And now? ', ts % timeIsGapless(), '   (Expected: F)'
		iRetCode = ts % putSingleItem(2, hold8_2, hold4_2)
		print *, 'And now? ', ts % timeIsGapless(), '   (Expected: T)'
		print *
		
		print *, 'Test 7 - Exercise member function getTimeSpan()'
		iRetCode = ts % populateFromTimeAndDataVectors(rvTimeStamp, rvValue)
		iRetCode = ts % getTimeSpan(rMinTimeStamp, rMaxTimeStamp)
		print *, 'No gaps: ', rMinTimeStamp, ' to ', rMaxTimeStamp, '   Return code = ', iRetCode,' (Expected: 0)'
		iRetCode = ts % getSingleItem(2, hold8_2, hold4_2)
		iRetCode = ts % putSingleItem(2, NaN_8, hold4_2)
		iRetCode = ts % getTimeSpan(rMinTimeStamp, rMaxTimeStamp)
		print *, 'One gap: ', rMinTimeStamp, ' to ', rMaxTimeStamp, '   Return code = ', iRetCode,' (Expected: 0)'
		rvTimeStamp = NaN_8
		iRetCode = ts % populateFromTimeAndDataVectors(rvTimeStamp, rvValue)
		iRetCode = ts % getTimeSpan(rMinTimeStamp, rMaxTimeStamp)
		print *, 'All gaps:', rMinTimeStamp, ' to ', rMaxTimeStamp, '   Return code = ', iRetCode,' (Expected: 1)'
		
		deallocate(rvTimeStamp, rvValue)
		
		allocate(rvTimeStamp(5), rvValue(5))
		print *, 'Test 8 - Exercise member function timeIsWellSpaced()'
		rvTimeStamp = [1.d0, 2.d0, 3.d0, 4.d0, 5.d0]	! Perfect: well-spaced, no gaps
		rvValue     = 1.	! Any value would be also good: we're looking to time now, not value
		iRetCode = ts % createFromTimeAndDataVectors(rvTimeStamp, rvValue)
		print *, 'Well spacing state: ', ts % timeIsWellSpaced(rTimeStep, iNumGaps), ' (expected=0)'
		print *, '     Time step: ', rTimeStep, '  (Expected: 1)'
		print *, '     Num. gaps: ', iNumGaps,  '  (Expected: 0)'
		rvTimeStamp = [1.d0, 2.d0, 4.d0, 5.d0, 6.d0]	! Almost perfect: well-spaced, one gap
		iRetCode = ts % populateFromTimeAndDataVectors(rvTimeStamp, rvValue)
		print *, 'Well spacing state: ', ts % timeIsWellSpaced(rTimeStep, iNumGaps), ' (expected=1)'
		print *, '     Time step: ', rTimeStep, '  (Expected: 1)'
		print *, '     Num. gaps: ', iNumGaps,  '  (Expected: 1)'
		rvTimeStamp = [1.13d0, 2.06d0, 3.61d0, 4.15d0, 5.70d0]	! Not well-spaced, one gap
		iRetCode = ts % populateFromTimeAndDataVectors(rvTimeStamp, rvValue)
		print *, 'Well spacing state: ', ts % timeIsWellSpaced(rTimeStep, iNumGaps), ' (expected=2)'
		print *, '     Time step: ', rTimeStep, '  (Expected: NaN)'
		print *, '     Num. gaps: ', iNumGaps,  '  (Expected: -1)'
		print *
		
		! Check copy constructor
		print *, "Test 9 - Exercise the copy constructor"
		rvTimeStamp = [1.d0, 2.d0, 3.d0, 4.d0, 5.d0]	! Perfect: well-spaced, no gaps
		rvValue     = 1.	! Any value would be also good: we're looking to time now, not value
		iRetCode = ts % createFromTimeAndDataVectors(rvTimeStamp, rvValue)
		iRetCode = tsCopy % createFromTimeSeries(ts)
		print *, "Return code from copy: ", iRetCode
		print *, "Expected time stamps:  ", rvTimeStamp
		print *, "Expected values:       ", rvValue
		iRetCode = tsCopy % getTimeStamp(rvTimeStamp)
		iRetCode = tsCopy % getValues(rvValue)
		print *, "Actual time stamps:    ", rvTimeStamp
		print *, "Actual values:         ", rvValue
		call tsCopy % timeShift(1.d0)
		iRetCode = tsCopy % getTimeStamp(rvTimeStamp)
		print *, "Shifted time stamps:   ", rvTimeStamp
		print *
		
		! Check copy constructor
		print *, "Test 10 - Exercise the time-based selector"
		print *, "-- Normal case"
		rvTimeStamp = [1.d0, 2.d0, 3.d0, 4.d0, 5.d0]	! Perfect: well-spaced, no gaps
		rvValue     = 1.	! Any value would be also good: we're looking to time now, not value
		iRetCode = ts % createFromTimeAndDataVectors(rvTimeStamp, rvValue)
		iRetCode = tsCopy % getTimeSubset(ts, 2.d0, 4.d0)
		print *, "Expected time stamps:  [2, 3, 4]"
		print *, "Expected values:       [1, 1, 1]"
		iRetCode = tsCopy % getTimeStamp(rvTimeStamp)
		iRetCode = tsCopy % getValues(rvValue)
		print *, "Actual time stamps:    ", rvTimeStamp
		print *, "Actual values:         ", rvValue
		print *, "-- Boundary case"
		deallocate(rvTimeStamp)
		deallocate(rvValue)
		allocate(rvTimeStamp(5))
		allocate(rvValue(5))
		rvTimeStamp = [1.d0, NaN_8, 3.d0, 4.d0, 5.d0]	! Not perfect: well-spaced, but one gap
		rvValue     = 1.	! Any value would be also good: we're looking to time now, not value
		iRetCode = ts % createFromTimeAndDataVectors(rvTimeStamp, rvValue)
		iRetCode = ts % getTimeStamp(rvTimeStamp)
		iRetCode = ts % getValues(rvValue)
		iRetCode = tsCopy % getTimeSubset(ts, 2.d0, 4.d0)
		print *, "Expected time stamps:  [3, 4]"
		print *, "Expected values:       [1, 1]"
		iRetCode = tsCopy % getTimeStamp(rvTimeStamp)
		iRetCode = tsCopy % getValues(rvValue)
		print *, "Actual time stamps:    ", rvTimeStamp
		print *, "Actual values:         ", rvValue
		print *
		
		! Leave
		deallocate(rvTimeStamp, rvValue)
		
	end subroutine testTimeSeries
	
	
	subroutine testQuantile()
	
		! Routine arguments
		! -none-
		
		! Locals
		integer					:: i, j
		real, dimension(32)		:: rvX
		real, dimension(32)		:: rvY
		real, dimension(9,12)	:: rmQtest, rmQref
		character(len=128)		:: sBuffer
		real					:: rQtest
		
		! Constants
		real, dimension(12), parameter	:: rvProb = [ &
			0.0000, 0.0001, 0.0010, 0.0100, 0.1000, &
			0.5000, 0.7500, 0.9000, 0.9500, 0.9980, &
			0.9999, 1.0000                          &
		]
		real, dimension(12), parameter	:: rvProb2 = [ &
			0.0000, 0.0001, NaN,    0.0100, 0.1000, &
			0.5000, 0.7500, 0.9000, 0.9500, 0.9980, &
			0.9999, 1.0000                          &
		]
		real, dimension(12), parameter	:: rvProb3 = [ &
			NaN,    NaN,    NaN,    NaN,    NaN, &
			NaN,    NaN,    NaN,    NaN,    NaN, &
			NaN,    NaN &
		]
		
		! Read data files
		open(10, file="quantile.test.csv", status='old', action='read')
		do i = 1, 32
			read(10, *) rvX(i)
		end do
		close(10)
		open(10, file="quantile.result.csv", status='old', action='read')
		read(10, "(a)") sBuffer
		do i = 1, 9
			read(10, *) (rmQref(i,j),j=1,12)
		end do
		close(10)
		
		! Test 1: Compute quantiles according to the existing methods; scalar form
		do i = 1, 9
			do j = 1, 12
				rmQtest(i,j) = Quantile(rvX, rvProb(j), i)
			end do
		end do
		print *, "Quantile - Test 1 - Test against R precomputed results - Scalar version"
		print *, "Type, Mean abs diff, Max abs diff"
		do i = 1, 9
			print *, i, sum(abs(rmQtest(i,:) - rmQref(i,:))) / 12, maxval(abs(rmQtest(i,:) - rmQref(i,:))), &
				maxloc(abs(rmQtest(i,:) - rmQref(i,:)))
		end do
		open(10, file="quantile.pblmet.1.csv", status='unknown', action='write')
		write(10, "(a)") trim(sBuffer)
		do i = 1, 9
			write(10, "(f11.9,11(',',f11.9))") (rmQtest(i,j), j = 1, 12)
		end do
		close(10)
		print *
		
		! Test 2: boundary - Invalid quantile level
		rQtest = Quantile(rvX, NaN)
		print *, "Quantile - Test 2 - Test against invalid quantile level; test also default type - Scalar"
		print *, "Quantile = ", rQtest, "  (expected: NaN)"
		print *
		
		! Test 3: Compute quantiles according to the existing methods: vector form
		do i = 1, 9
			rmQtest(i,:) = Quantile(rvX, rvProb, i)
		end do
		print *, "Quantile - Test 3 - Test against R precomputed results - Vector version"
		print *, "Type, Mean abs diff, Max abs diff"
		do i = 1, 9
			print *, i, sum(abs(rmQtest(i,:) - rmQref(i,:))) / 12, maxval(abs(rmQtest(i,:) - rmQref(i,:)))
		end do
		open(10, file="quantile.pblmet.2.csv", status='unknown', action='write')
		write(10, "(a)") trim(sBuffer)
		do i = 1, 9
			write(10, "(f11.9,11(',',f11.9))") (rmQtest(i,j), j = 1, 12)
		end do
		close(10)
		print *
		
		! Test 4: boundary - Invalid quantile level
		rmQtest(8,:) = Quantile(rvX, rvProb2)
		print *, "Quantile - Test 4 - Test against one invalid quantile level; test also default type - vector"
		print *, "Quantile = ", rmQtest(8,:), "  (expected: third quantile value in vector is NaN)"
		print *
		
		! Test 5: boundary - All quantile levels invalid
		rmQtest(8,:) = Quantile(rvX, rvProb3)
		print *, "Quantile - Test 5 - Test against all invalid quantile levels; test also default type - vector"
		print *, "Quantile = ", rmQtest(8,:), "  (expected: all NaN)"
		print *
		
		! Test 6: Normal: One data value is invalid, scalar case
		rvY = rvX
		rvY(13) = NaN
		do i = 1, 9
			do j = 1, 12
				rmQtest(i,j) = Quantile(rvY, rvProb(j), i)
			end do
		end do
		print *, "Quantile - Test 6 - Test against R precomputed results - Scalar version, one NaN"
		print *, " - Expected: *differs* from ideal case, but no NaNs"
		print *, "Type, Mean abs diff, Max abs diff"
		do i = 1, 9
			print *, i, sum(abs(rmQtest(i,:) - rmQref(i,:))) / 12, maxval(abs(rmQtest(i,:) - rmQref(i,:))), &
				maxloc(abs(rmQtest(i,:) - rmQref(i,:)))
		end do
		print *
		
		! Test 7: Normal: One data value is invalid, scalar case
		do i = 1, 9
			rmQtest(i,:) = Quantile(rvY, rvProb, i)
		end do
		print *, "Quantile - Test 7 - Test against R precomputed results - Vector version, one NaN"
		print *, "Type, Mean abs diff, Max abs diff"
		do i = 1, 9
			print *, i, sum(abs(rmQtest(i,:) - rmQref(i,:))) / 12, maxval(abs(rmQtest(i,:) - rmQref(i,:)))
		end do
		
	end subroutine testQuantile
	
	
	subroutine testSkewness()
	
		! Routine arguments
		! --none--
		
		! Locals
		real, dimension(16)	:: rvX, rvY
		integer				:: i
		
		! Assign test values
		rvX = [(float(i), i = 1, 16)]
		rvY = (rvX - 8.)**2
		
		! Test 1: Normal: Skewness, against R values
		print *, "Skewness - Test 1 - X and Y cases, compared to R result"
		print *, "  Skewness(X) = ", Skew(rvX), "  (expected: 0)"
		print *, "  Skewness(Y) = ", Skew(rvY), "  (expected: 0.7002017)"
		print *
		
		! Test 2: Normal: Skewness with mean, stdev and both
		print *, "Skewness - Test 1 - Y case, with external mean and stddev"
		print *, "  Skewness(Y, Mean(Y)) = ", Skew(rvY, rMeanIn=Mean(rvY)), "  (expected: 0.7002017)"
		print *, "  Skewness(Y, Stddev(Y)) = ", Skew(rvY, rStdDevIn=StdDev(rvY)), "  (expected: 0.7002017)"
		print *, "  Skewness(Y, Mean(Y), Stddev(Y)) = ", Skew(rvY, rMeanIn=Mean(rvY), rStdDevIn=StdDev(rvY)), &
			"  (expected: 0.7002017)"
		
	end subroutine testSkewness
	
end program test_pbl_stat
