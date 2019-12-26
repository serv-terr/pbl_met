! Peak Sgamator 3.0
!
! Just an attempt, in the moment, based on AMPD method by Felix Scholkmann et al. in "An Efficient Algorithm for Automatic Peak Detection in 
! Noisy Periodic and Quasi-Periodic Signals", Algorithms 2012, 5, 588-603.
!
! Original implementation in Python, by Luca Cerina (https://github.com/LucaCerina/ampdLib)
!
! The translation to Fortran is mine. (Patti M. Favaron, 2019-12-26)
!
module ps3

	use pbl_met
	
	implicit none
	
	private
	
	! Public interface
	public	::	ampd
	
contains

	function ampd(rvX, rLimit, ivPks) result(iRetCode)
	
		! Routine arguments
		real, dimension(:), intent(in)					:: rvX
		real, intent(in), optional						:: rLimit
		integer, dimension(:), allocatable, intent(out)	:: ivPks
		integer											:: iRetCode
		
		! Locals
		integer	:: i, l, n
		real	:: rNumerator
		real	:: rDenominator
		real	:: rMeanIdx
		real	:: rMean
		real, dimension(:), allocatable			:: rvTrendlessX
		integer(8), dimension(:,:), allocatable	:: imLSM
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Remove trend, by subtracting the linear regression value
		n = size(rvX)
		allocate(rvTrendlessX)
		if(n <= 0) then
			iRetCode = 1
			return
		end if
		rNumerator   = 0
		rDenominator = (real(n)**2 - 1.) / 12.
		rMeanIdx     = (real(n) + 1.) / 2.
		rMean        = sum(rvX) / n
		do i = 1, n
			rNumerator = rNumerator + (i - rMeanIdx) * (rvX(i) - rMean)
		end do
		rBeta  = rNumerator / rDenominator
		rAlpha = rMean - rBeta*rMeanIdx
		rvTrendlessX = rvX - (rAlpha + rBeta*[i, (i=1, n)])
		
		! Generate the initial matrix
		l = int(ceiling(n * rLimit / 2.)) - 1
		allocate(imLSM(l, n))
		imLSM = 1
		
		! Update matrix, and extract local minima
		do k = 1, l
			do j = 1, n
				if() then
					rmLSM(k,j) = 0
				end if
			end do
		end do
	
		! Leave
		deallocate(rmLSM)
		deallocate(rvTrendlessX)
	
	
	# Local minima extraction
	for k in range(1, L):
		LSM[k - 1, np.where((dtrSignal[k:N - k - 1] > dtrSignal[0: N - 2 * k - 1]) & (dtrSignal[k:N - k - 1] > dtrSignal[2 * k: N - 1]))[0]+k] = 0
	
	pks = np.where(np.sum(LSM[0:np.argmin(np.sum(LSM, 1)), :], 0)==0)[0]
	return pks
		
	end function ampd
	
end module ps3
