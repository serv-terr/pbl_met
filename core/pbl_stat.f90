! pbl_stat  : Fortran module, providing support to elementary statistical
!             functions.
!
! This module is part of the pbl_met library.
!
! This is open-source code, covered by the lGPL 3.0 license.
!
module pbl_stat

    use pbl_base

    implicit none
    
    private
    
    ! Public interface
    ! 1. Off-range and invalid data management
    public	:: RangeInvalidate
    public	:: PairInvalidate
    public	:: RangeClip
    public	:: GetValidOnly
    ! 2. Basic statistics
    public  :: Cov
    ! 3. Autocovariance, autocorrelation and related
    public	:: AutoCov
    public	:: AutoCorr
    ! 4. Crosscovariance, crosscorrelation and related
    
contains

	! Make data outside a specified range invalid, by replacing their value with NaN
	subroutine RangeInvalidate(rvX, rMin, rMax)
	
		! Routine arguments
		real, dimension(:), intent(inout)	:: rvX
		real, intent(in)					:: rMin
		real, intent(in)					:: rMax
		
		! Locals
		integer	:: i
		
		! Validate by range
		do i = 1, size(rvX)
			if(rvX(i) < rMin) then
				rvX(i) = NaN
			elseif(rvX(i) > rMax) then
				rvX(i) = NaN
			end if
		end do
		
	end subroutine RangeInvalidate
	

	! Make invalid data in a vector invalid if those of another also are, and viceversa.
	! After 
	subroutine PairInvalidate(rvX, rvY)
	
		! Routine arguments
		real, dimension(:), intent(inout)	:: rvX
		real, dimension(:), intent(inout)	:: rvY
		
		! Locals
		integer	:: i
		
		! Validate by range
		do i = 1, size(rvX)
			if(isnan(rvX(i))) then
				rvY(i) = NaN
			elseif(isnan(rvY(i))) then
				rvX(i) = NaN
			end if
		end do
		
	end subroutine PairInvalidate
	

	! Force data to be within a specified range invalid, clipping to extremal values
	subroutine RangeClip(rvX, rMin, rMax)
	
		! Routine arguments
		real, dimension(:), intent(inout)	:: rvX
		real, intent(in)					:: rMin
		real, intent(in)					:: rMax
		
		! Locals
		integer	:: i
		
		! Validate by range
		do i = 1, size(rvX)
			if(rvX(i) < rMin) then
				rvX(i) = rMin
			elseif(rvX(i) > rMax) then
				rvX(i) = rMax
			end if
		end do
		
	end subroutine RangeClip
	
	
	! Pack a vector to another vector containing only valid (i.e. non-NaN) data
	function GetValidOnly(rvX) result(rvValidX)
	
		! Routine arguments
		real, dimension(:), intent(in)	:: rvX
		real, dimension(:), allocatable	:: rvValidX
		
		! Locals
		integer	:: iNumValid
		integer	:: i, j
		
		! Count valid data, and check something is to be made
		iNumValid = count(.not.isnan(rvX))
		if(allocated(rvValidX)) deallocate(rvValidX)
		if(size(rvX) <= 0 .or. iNumValid <= 0) return
		
		! Loop over data, copying valids only to the new vector
		if(allocated(rvValidX)) deallocate(rvValidX)
		allocate(rvValidX(iNumValid))
		j = 0
		do i = 1, size(rvX)
			if(.not.isnan(rvX(i))) then
				j = j + 1
				rvValidX(j) = rvX(i)
			end if
		end do
		
	end function GetValidOnly
	

    ! Compute covariance between two signal samples; these samples should
    ! be the same size, and "error-paired", that is, whenever rvX(i) == NaN,
    ! then rvY(i) == NaN, and vice-versa.
	function Cov(rvX, rvY) result(rCov)
	
		! Routine arguments
		real, dimension(:), intent(in)	:: rvX
		real, dimension(:), intent(in)	:: rvY
		real							:: rCov
		
		! Locals
        integer :: n
		real(8)	:: rAvgX
		real(8)	:: rAvgY
        
        ! Check it makes sense to proceed
        n = count(.not.isnan(rvX))  ! Valid also for 'rvY' since the error-pairing assumption
        if(n <= 0) then
            rCov = NaN
            return
        end if
		
		! Compute averages
		rAvgX = sum(rvX, mask=.not.isnan(rvX))/n
		rAvgY = sum(rvY, mask=.not.isnan(rvY))/n
		
		! Compute the covariance (using a very simpli definition):
		rCov = sum((rvX-rAvgX)*(rvY-rAvgY), mask=.not.isnan(rvX))/n
		
	end function Cov
	
	
	! Compute the autocovariance of a signal up the specified number of lags,
	! by using the direct summation method.
	!
	! Warning: On call to this routine, a vector rvACov having dimension
	! ======== 0:n can be used without any restraint. Inside AutoCov, this
	!          vector will be indexed 1:n+1, but the convention adopted
	!          (i.e. index 1 to mean lag 0, index 2 lag 1, ...) ensures
	!          full consistency
	!
	function AutoCov(rvX, rvACov) result(iRetCode)
	
		! Routine arguments
		real, dimension(:), intent(in)	:: rvX			! Signal (may contain NaN values)
		real, dimension(:), intent(out)	:: rvACov		! Vector containing the desired values (rvACov(1) refers to lag 0, rvACov(2) to lag 1, ...)
		integer							:: iRetCode		! Flag indicating success (value = 0) or failure.
		
		! Locals
		integer	:: iLag
		integer	:: i
		integer	:: n
		real	:: rAvgA
		real	:: rAvgB
		integer	:: iNum
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check parameters
		if(size(rvX) <= 0 .OR. size(rvACov) <= 0 .OR. size(rvACov) > size(rvX)/2) then
			iRetCode = 1
			return
		end IF
		n = size(rvX)
		
		! Compute autocovariance for each lag
		do iLag = 0, size(rvACov)-1
			rAvgA = 0.
			rAvgB = 0.
			iNum = 0
			do i = 1, n - iLag
				if(.not.isnan(rvX(i)) .and. .not.isnan(rvX(i+iLag))) then
					iNum = iNum + 1
					rAvgA = rAvgA + rvX(i)
					rAvgB = rAvgB + rvX(i+iLag)
				end if
			end do
			if(iNum > 0) then
				rAvgA = rAvgA / iNum
				rAvgB = rAvgB / iNum
				rvACov(iLag+1) = 0.
				do i = 1, n - iLag
					if(.not.isnan(rvX(i)) .and. .not.isnan(rvX(i+iLag))) then
						rvACov(iLag+1) = rvACov(iLag+1) + (rvX(i) - rAvgA)*(rvX(i+iLag) - rAvgB)
					end if
				end do
				rvACov(iLag+1) = rvACov(iLag+1)/iNum			
			else
				rvACov(iLag+1) = NaN
			end if
		end do
		
	end function AutoCov
	
	
	! Compute the autocorrelation of a signal up the specified number of lags,
	! by using the direct summation method.
	function AutoCorr(rvX, rvACorr) result(iRetCode)
	
		! Routine arguments
		real, dimension(:), intent(in)	:: rvX			! Signal (may contain NaN values)
		real, dimension(:), intent(out)	:: rvACorr		! Vector containing the desired values (rvACorr(1) refers to lag 0, rvACorr(2) to lag 1, ...)
		integer							:: iRetCode		! Flag indicating success (value = 0) or failure.
		
		! Locals
		integer	:: iLag
		integer	:: i
		integer	:: n
		real	:: rAvgA
		real	:: rAvgB
		real	:: rSum2A
		real	:: rSum2B
		real	:: rStdA
		real	:: rStdB
		integer	:: iNum
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check parameters
		if(size(rvX) <= 0 .OR. size(rvACorr) <= 0 .OR. size(rvACorr) > size(rvX)/2) then
			iRetCode = 1
			return
		end IF
		n = size(rvX)
		
		! Compute autocovariance for each lag
		do iLag = 0, size(rvACorr)-1
			rAvgA = 0.
			rAvgB = 0.
			rSum2A = 0.
			rSum2B = 0.
			iNum = 0
			do i = 1, n - iLag
				if(.not.isnan(rvX(i)) .and. .not.isnan(rvX(i+iLag))) then
					iNum = iNum + 1
					rAvgA = rAvgA + rvX(i)
					rAvgB = rAvgB + rvX(i+iLag)
					rSum2A = rSum2A + rvX(i)**2
					rSum2B = rSum2B + rvX(i+iLag)**2
				end if
			end do
			if(iNum > 0) then
				rAvgA = rAvgA / iNum
				rAvgB = rAvgB / iNum
				rSum2A = rSum2A / iNum
				rSum2B = rSum2B / iNum
				rStdA  = sqrt(rSum2A - rAvgA**2)
				rStdB  = sqrt(rSum2B - rAvgB**2)
				rvACorr(iLag+1) = 0.
				do i = 1, n - iLag
					if(.not.isnan(rvX(i)) .and. .not.isnan(rvX(i+iLag))) then
						rvACorr(iLag+1) = rvACorr(iLag+1) + (rvX(i) - rAvgA)*(rvX(i+iLag) - rAvgB)
					end if
				end do
				rvACorr(iLag+1) = (rvACorr(iLag+1)/iNum) / (rStdA * rStdB)
			else
				rvACorr(iLag+1) = NaN
			end if
		end do
		
	end function AutoCorr
	
	
	function EulerianTime(rFcv, rvX, iMaxLag) RESULT(rEul)
	
		! Routine arguments
		real, intent(in)				:: rFcv
		real, dimension(:), intent(in)	:: rvX
		integer, intent(in)				:: iMaxLag
		real							:: rEul
		
		! Locals
		real, dimension(0:iMaxLag)	:: rvC
		real, dimension(0:iMaxLag)	:: rvK
		integer						:: iErrCode
		integer						:: i
		real						:: rDelta
		real						:: rNum
		real						:: rDen
		integer						:: iNumPoints
		
		! Auxiliary constants
		real, parameter		:: TOL = 1.e-4
		integer, parameter	:: MAX_ITER = 16
		
		! Compute autocovariances and scale them to autocorrelations
		iErrCode = AutoCov(rvX, rvC)
		rvC = rvC/rvC(0)
		
		! Initialize the auxiliary vector
		rvK = (/ (float(i),i=0,iMaxLag) /)
		
		! Compute the linear regression estimate of the Eulerian time
		rDelta = 1. / rFcv
		iNumPoints = 0
		rNum = 0.
		rDen = 0.
		do i = 0, iMaxLag
			if(rvC(i) > 0.) then
				iNumPoints = iNumPoints + 1
				rNum = rNum + rvK(i)**2
				rDen = rDen - rvK(i)*LOG(rvC(i))
			end IF
		end do
		if(iNumPoints > 2) then
			rEul = rDelta * rNum / rDen
		ELSE
			rEul = -9999.9
		end IF
		
	end function EulerianTime
	
	
	function PolyEval(rvP, rX, rDy) RESULT(rY)
	
		! Routine arguments
		real, dimension(:), intent(in)	:: rvP
		real, intent(in)				:: rX
		real, intent(out)				:: rDy
		real							:: rY
		
		! Locals
		integer	:: i
		integer	:: n
		real	:: rTemp
		
		! Evaluate the polynomial, using Ruffini-Horner scheme
		n = size(rvP)-1
		rY = rvP(n)
		rDy = 0.0
		do i = n-1, 0, -1
			rDy = rDy*rX + rY
			rY  = rvP(i) + rX*rY
		end do
		
	end function PolyEval
	
	
	SUBROUTINE Detrend(rvX, rvY, rMultiplier, rOffset)
	
		! Routine argument
		real, dimension(:), intent(in)		:: rvX
		real, dimension(:), INTENT(INOUT)	:: rvY
		real, intent(out)					:: rMultiplier
		real, intent(out)					:: rOffset
		
		! Locals
		integer	:: N
		real	:: rSx
		real	:: rSy
		real	:: rSxx
		real	:: rSxy
		real	:: rDelta
		
		! Compute counts and sums
		N    = size(rvX)
		rSx  = SUM(rvX)
		rSy  = SUM(rvY)
		rSxx = doT_PRODUCT(rvX,rvX)
		rSxy = doT_PRODUCT(rvX,rvY)
		
		! Compute multiplier and offset
		rDelta      = N*rSxx - rSx**2
		rOffset     = (rSxx*rSy - rSx*rSxy)/rDelta
		rMultiplier = (N*rSxy - rSx*rSy)/rDelta
		
		! Subtract the linear trend
		rvY = rvY - rMultiplier*(rvX - rSx/N)
	
	end SUBROUTINE Detrend
	
	
end module pbl_stat
