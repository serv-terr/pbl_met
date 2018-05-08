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
		type(TimeSeries)	:: ts, tsCopy
		real				:: rValid, rMin, rMean, rStdDev, rMax
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
		real(8)				:: rMinTimeStamp
		real(8)				:: rMaxTimeStamp
		integer				:: iNumGaps
		integer				:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		real(8), dimension(:), allocatable	:: rvTimeStamp
		real, dimension(:), allocatable		:: rvValue
		integer								:: iFirstComma
		integer								:: iCurrentTime
	
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
		
		! Make time stamp non-monotonic by exchanging the monotonic's first two elements
		iRetCode = ts % getSingleItem(1, hold8_1, hold4_1)
		iRetCode = ts % getSingleItem(2, hold8_2, hold4_2)
		iRetCode = ts % putSingleItem(1, hold8_2, hold4_2)
		iRetCode = ts % putSingleItem(2, hold8_1, hold4_1)
	
		! Check time is monotonic
		print *,'Test 3 - Check time stamp strict monotonicity (increasing)'
		print *,'Monotonic? ', ts % timeIsMonotonic(), "    (Expected: F)"
		print *
	
		! Check time is quasi monotonic
		print *,'Test 3 - Check time stamp weak monotonicity (non-decreasing)'
		print *,'Monotonic? ', ts % timeIsQuasiMonotonic(), "    (Expected: F)"
		print *
		
		! Check time is gapless
		print *, 'Test 4 - Check time stamp vector to be gapless'
		iRetCode = ts % populateFromTimeAndDataVectors(rvTimeStamp, rvValue)
		print *, 'Gapless? ', ts % timeIsGapless(), '   (Expected: T)'
		iRetCode = ts % getSingleItem(2, hold8_2, hold4_2)
		iRetCode = ts % putSingleItem(2, NaN_8, hold4_2)
		print *, 'And now? ', ts % timeIsGapless(), '   (Expected: F)'
		iRetCode = ts % putSingleItem(2, hold8_2, hold4_2)
		print *, 'And now? ', ts % timeIsGapless(), '   (Expected: T)'
		print *
		
		print *, 'Test 5 - Exercise member function getTimeSpan()'
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
		print *, 'Test 6 - Exercise member function timeIsWellSpaced()'
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
		print *, "Test 7 - Exercise the copy constructor"
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
		
		! Leave
		deallocate(rvTimeStamp, rvValue)
		
	end subroutine testTimeSeries
	
end program test_pbl_stat
