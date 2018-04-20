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
    public	:: PartialAutoCorr
    public	:: EulerianTime
    ! 4. Cross-covariance, cross-correlation and related
    public	:: CrossCov
    public	:: CrossCorr
    ! 5. Utilities
    public	:: RemoveLinearTrend
    
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
	
	
	! Compute the autocovariance of a signal up the specified number of lags,
	! by using the direct summation method.
	!
	! Warning: On call to this routine, a vector rvACov having dimension
	! ======== 0:n can be used without any restraint. Inside AutoCov, this
	!          vector will be indexed 1:n+1, but the convention adopted
	!          (i.e. index 1 to mean lag 0, index 2 lag 1, ...) ensures
	!          full consistency
	!
	function CrossCov(rvX, rvY, rvCCov) result(iRetCode)
	
		! Routine arguments
		real, dimension(:), intent(in)	:: rvX			! First signal (may contain NaN values)
		real, dimension(:), intent(in)	:: rvY			! Second signal (may contain NaN values)
		real, dimension(:), intent(out)	:: rvCCov		! Vector containing the desired values (rvCCov(1) refers to lag 0, rvCCov(2) to lag 1, ...)
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
		if(size(rvX) <= 0 .OR. size(rvY) <= 0 .OR. size(rvCCov) <= 0 .OR. size(rvCCov) > size(rvX)/2) then
			iRetCode = 1
			return
		end IF
		n = size(rvX)
		
		! Compute autocovariance for each lag
		do iLag = 0, size(rvCCov)-1
			rAvgA = 0.
			rAvgB = 0.
			iNum = 0
			do i = 1, n - iLag
				if(.not.isnan(rvX(i)) .and. .not.isnan(rvY(i+iLag))) then
					iNum = iNum + 1
					rAvgA = rAvgA + rvX(i)
					rAvgB = rAvgB + rvY(i+iLag)
				end if
			end do
			if(iNum > 0) then
				rAvgA = rAvgA / iNum
				rAvgB = rAvgB / iNum
				rvCCov(iLag+1) = 0.
				do i = 1, n - iLag
					if(.not.isnan(rvX(i)) .and. .not.isnan(rvY(i+iLag))) then
						rvCCov(iLag+1) = rvCCov(iLag+1) + (rvX(i) - rAvgA)*(rvY(i+iLag) - rAvgB)
					end if
				end do
				rvCCov(iLag+1) = rvCCov(iLag+1)/iNum			
			else
				rvCCov(iLag+1) = NaN
			end if
		end do
		
	end function CrossCov
	
	
	! Compute the autocorrelation of a signal up the specified number of lags,
	! by using the direct summation method.
	function CrossCorr(rvX, rvY, rvCCorr) result(iRetCode)
	
		! Routine arguments
		real, dimension(:), intent(in)	:: rvX			! First signal (may contain NaN values)
		real, dimension(:), intent(in)	:: rvY			! Second signal (may contain NaN values)
		real, dimension(:), intent(out)	:: rvCCorr		! Vector containing the desired values (rvCCorr(1) refers to lag 0, rvCCorr(2) to lag 1, ...)
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
		if(size(rvX) <= 0 .OR. size(rvY) <= 0 .OR. size(rvCCorr) <= 0 .OR. size(rvCCorr) > size(rvX)/2) then
			iRetCode = 1
			return
		end IF
		n = size(rvX)
		
		! Compute autocovariance for each lag
		do iLag = 0, size(rvCCorr)-1
			rAvgA = 0.
			rAvgB = 0.
			rSum2A = 0.
			rSum2B = 0.
			iNum = 0
			do i = 1, n - iLag
				if(.not.isnan(rvX(i)) .and. .not.isnan(rvY(i+iLag))) then
					iNum = iNum + 1
					rAvgA = rAvgA + rvX(i)
					rAvgB = rAvgB + rvY(i+iLag)
					rSum2A = rSum2A + rvX(i)**2
					rSum2B = rSum2B + rvY(i+iLag)**2
				end if
			end do
			if(iNum > 0) then
				rAvgA = rAvgA / iNum
				rAvgB = rAvgB / iNum
				rSum2A = rSum2A / iNum
				rSum2B = rSum2B / iNum
				rStdA  = sqrt(rSum2A - rAvgA**2)
				rStdB  = sqrt(rSum2B - rAvgB**2)
				rvCCorr(iLag+1) = 0.
				do i = 1, n - iLag
					if(.not.isnan(rvX(i)) .and. .not.isnan(rvY(i+iLag))) then
						rvCCorr(iLag+1) = rvCCorr(iLag+1) + (rvX(i) - rAvgA)*(rvY(i+iLag) - rAvgB)
					end if
				end do
				rvCCorr(iLag+1) = (rvCCorr(iLag+1)/iNum) / (rStdA * rStdB)
			else
				rvCCorr(iLag+1) = NaN
			end if
		end do
		
	end function CrossCorr
	
	
	! Partial autocorrelation values, from autocorrelation. Useful, to determine
	! the order of an autoregressive process.
	function PartialAutoCorr(rvACorr) result(rvPACorr)
		
		! Routine arguments
		real, dimension(:), intent(in)	:: rvACorr		! Autocorrelation values (rvACorr(1) refers to lag 0, rvACorr(2) to lag 1, ...)
		real, dimension(size(rvACorr))	:: rvPACorr		! Partial autocorrelation, same indexing convention as above
		
		! Locals
		real, dimension(size(rvACorr) - 1)	:: p, a
		integer								:: l
		integer								:: i
		integer								:: j
		integer								:: k
		integer								:: lp
		real								:: q
		real								:: u
		real								:: v
		real								:: hold
		
		! Compute partial autocorrelation coefficients
		p = rvACorr(2:)
		lp = size(p)
		do i = 1, lp
			if(i < 2) then
				q = p(i)
				v = 1.0 - q*q
			else
				q = u/v
				v = v*(1.0 - q*q)
				l = (i-1)/2
				if(l /= 0) then
					do j = 1, l
						hold = a(j)
						k = i-j
						a(j) = a(j) - q*a(k)
						a(k) = a(k) - q*hold
					end do
				end if
				if(2*l < i-1) a(l+i) = a(l+i)*(1.0 - q)
			end if
			a(i) = -q
			u = p(i+1)
			do j = 1, i
				u = u + a(j) * p(i-j+1)
			end do
			rvPACorr(i) = q
		end do
		
	end function PartialAutoCorr
	
	
	! Estimate the Euleriam decorrelation time of a signal
	!
	! Routine originally developed by Roberto Sozzi
	!
	function EulerianTime(rFcv, rvX, iMaxLag) result(rEul)
	
		! Routine arguments
		real, intent(in)				:: rFcv		! Data acquisition rate (Hz)
		real, dimension(:), intent(in)	:: rvX		! Signal (any unit)
		integer, intent(in)				:: iMaxLag	! Maximum lag to consider
		real							:: rEul		! Estimate of Eulerian decorrelation time (s)
		
		! Locals
		real, dimension(0:iMaxLag)	:: rvC		! On exchange with AutoCov indices run 1 to iMaxLag+1
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
		
		! Compute autocorrelations
		iErrCode = AutoCorr(rvX, rvC)
		
		! Initialize the auxiliary vector
		rvK = (/ (float(i),i=0,iMaxLag) /)
		
		! Compute the linear regression estimate of the Eulerian time
		rDelta = 1. / rFcv
		iNumPoints = 0
		rNum = 0.
		rDen = 0.
		do i = 0, iMaxLag
			if(.not.isnan(rvC(i)) .and. rvC(i) > 0.) then
				iNumPoints = iNumPoints + 1
				rNum = rNum + rvK(i)**2
				rDen = rDen - rvK(i)*log(rvC(i))
			end if
		end do
		if(iNumPoints > 2) then
			rEul = rDelta * rNum / rDen
		else
			rEul = NaN
		end if
		
	end function EulerianTime
	
	
	! Remove linear trend, if any, from a signal.
	!
	! The signal is 
	subroutine RemoveLinearTrend(rvX, rvY, rMultiplier, rOffset)
	
		! Routine argument
		real, dimension(:), intent(in)		:: rvX			! Index signal (typically time, in floating point form)
		real, dimension(:), intent(inout)	:: rvY			! Signal to remove the trend from
		real, intent(out)					:: rMultiplier	! Multiplier of trend line
		real, intent(out)					:: rOffset		! Offset of trend line
		
		! Locals
		integer	:: n
		real	:: rSx
		real	:: rSy
		real	:: rSxx
		real	:: rSxy
		real	:: rDelta
		
		! Compute counts and sums
		n    = size(rvX)
		rSx  = sum(rvX, mask=.not.isnan(rvX))
		rSy  = SUM(rvY, mask=.not.isnan(rvy))
		rSxx = doT_product(rvX,rvX)
		rSxy = doT_product(rvX,rvY)
		
		! Compute multiplier and offset
		rDelta      = n*rSxx - rSx**2
		rOffset     = (rSxx*rSy - rSx*rSxy)/rDelta
		rMultiplier = (n*rSxy - rSx*rSy)/rDelta
		
		! Subtract the linear trend
		rvY = rvY - rMultiplier*(rvX - rSx/n)
	
	end subroutine RemoveLinearTrend
	
end module pbl_stat
