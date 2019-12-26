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
		integer	:: i, j, l, n
		integer	:: jm, jp
		integer	:: iNumPks
		real	:: rNumerator
		real	:: rDenominator
		real	:: rMeanIdx
		real	:: rMean
		real, dimension(:), allocatable			:: rvTrendlessX
		integer, dimension(:,:), allocatable	:: imLSM
		integer, dimension(:), allocatable		:: ivCount
		integer, dimension(:), allocatable		:: ivAux
		integer, dimension(1)					:: ivPos
		
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
		allocate(imLSM(l, n), ivCount(l), ivAux(n))
		imLSM = 1
		
		! Update matrix
		do k = 1, l
			do j = k+1, n-k
				jm = j - k
				jp = j + k
				if(rvTrendlessX(j) > rvTrendlessX(jm) .and. rvTrendlessX(j) > rvTrendlessX(jp)) then
					imLSM(k,j) = 0
				end if
			end do
		end do
		
		! Find local extrema
		ivCount = sum(imLSM, dim=2)
		ivPos = minloc(ivCount)
		ivAux = sum(imLSM(1:ivPos(1),:), dim=1)
		iNumPks = count(ivAux == 0)
		if(allocated(ivPks)) deallocate(ivPks)
		allocate(ivPks(iNumPks))
		j = 0
		do i = 1, n
			if(ivAux(i) == 0) then
				j = j + 1
				ivPks(j) = i
			end if
		end do
	
		! Leave
		deallocate(imLSM, ivCount, ivAux)
		deallocate(rvTrendlessX)
	
	end function ampd
	
end module ps3
