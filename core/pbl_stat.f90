! pbl_stat  : Fortran module, providing support to elementary statistical
!             functions.
!
! This module is part of the pbl_met library.
!
! This is open-source code, covered by the lGPL 3.0 license.
!
module pbl_stat

    use pbl_base
    use pbl_time

    implicit none
    
    private
    
    ! Public interface
    ! 1. Off-range and invalid data management
    public	:: RangeInvalidate
    public	:: PairInvalidate
    public	:: RangeClip
    public	:: GetValidOnly
    ! 2. Basic statistics
    public	:: Mean
    public	:: StdDev
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
    ! 6. Time series
    public	:: TimeSeries
    
    ! Data types
    
    type TimeSeries
    	real(8), dimension(:), allocatable, private	:: rvTimeStamp
    	real, dimension(:), allocatable, private	:: rvValue
    contains
    	! Constructors
    	procedure, public	:: createEmpty						=> tsCreateEmpty
    	procedure, public	:: createFromDataVector				=> tsCreateFromDataVector
    	procedure, public	:: createFromTimeAndDataVectors		=> tsCreateFromTimeAndDataVectors
    	! Modifiers and reshapers
    	procedure, public	:: populateFromDataVector			=> tsCreateFromDataVector
    	procedure, public	:: populateFromTimeAndDataVectors	=> tsCreateFromTimeAndDataVectors
    	! Selectors
    	procedure, public	:: getSingleItem					=> tsGetSingleItem
    	! Assigners
    	procedure, public	:: putSingleItem					=> tsPutSingleItem
    	! Summary generators
    	procedure, public	:: summary							=> tsSummary
    	procedure, public	:: rangeInvalidate					=> tsRangeInvalidate
    	! State interrogations
    	procedure, public	:: isEmpty							=> tsIsEmpty
    	procedure, public	:: timeIsMonotonic					=> tsTimeMonotonic
    	procedure, public	:: timeIsQuasiMonotonic				=> tsTimeQuasiMonotonic
    	procedure, public	:: timeIsGapless					=> tsTimeGapless
    end type TimeSeries
    
contains

	! Make data outside a specified range invalid, by replacing their value with NaN
	subroutine RangeInvalidate(rvX, rMin, rMax)
	
		! Routine arguments
		real, dimension(:), intent(inout)	:: rvX		! Vector of data to range-invalidate
		real, intent(in)					:: rMin		! Minimum allowed value
		real, intent(in)					:: rMax		! Maximum allowed value
		
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
		real, dimension(:), intent(inout)	:: rvX		! Vector to pair-invalidate
		real, dimension(:), intent(inout)	:: rvY		! Vector to pair-invalidate
		
		! Locals
		integer	:: i
		integer	:: iMin, iMax
		
		! Compute loop limits from array dimensions
		iMin = max(lbound(rvX,dim=1), lbound(rvY,dim=1))
		iMax = min(ubound(rvX,dim=1), ubound(rvY,dim=1))
		
		! Ensure invalid positions in one vector are propagated to the other
		do i = iMin, iMax
			if(.invalid. rvX(i)) then
				rvY(i) = NaN
			elseif(.invalid. rvY(i)) then
				rvX(i) = NaN
			end if
		end do
		
	end subroutine PairInvalidate
	

	! Force data to be within a specified range invalid, clipping to extremal values
	subroutine RangeClip(rvX, rMin, rMax)
	
		! Routine arguments
		real, dimension(:), intent(inout)	:: rvX		! Vector of data to range-clip
		real, intent(in)					:: rMin		! Minimum allowed value
		real, intent(in)					:: rMax		! Maximum allowed value
		
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
		real, dimension(:), intent(in)	:: rvX			! Vector of data containing zero or more NaN
		real, dimension(:), allocatable	:: rvValidX		! The same vector, with all NaN values stripped
		
		! Locals
		integer	:: iNumValid
		integer	:: i, j
		
		! Count valid data, and check something is to be made
		iNumValid = count(.not.isnan(rvX))
		if(allocated(rvValidX)) deallocate(rvValidX)
		if(size(rvX) <= 0 .or. iNumValid <= 0) then
			allocate(rvValidX(0))
			return
		end if
		
		! Loop over data, copying valids only to the new vector
		allocate(rvValidX(iNumValid))
		j = 0
		do i = 1, size(rvX)
			if(.not.isnan(rvX(i))) then
				j = j + 1
				rvValidX(j) = rvX(i)
			end if
		end do
		
	end function GetValidOnly
	
	
	! Compute the mean of a signal
	function Mean(rvX, rValidFraction) result(rMean)
	
		! Routine arguments
		real, dimension(:), intent(in)	:: rvX				! Signal, whose mean is needed
		real, intent(out), optional		:: rValidFraction	! Fraction of valid to total signal data (optional)
		real							:: rMean			! Mean (NaN if not possible to evaluate)
		
		! Locals
		integer	:: n
		
		! Check something is to be made
		if(size(rvX) <= 0) then
			rMean = NaN
			return
		end if
		
		! Compute the arithmetic mean
		n = count(.not.isnan(rvX))
		if(n > 0) then
			rMean = sum(rvX, mask=.not.isnan(rvX)) / n
		else
			rMean = NaN
		end if
		
		! Compute diagnostic quantities, if present
		if(present(rValidFraction)) then
			rValidFraction = float(n) / size(rvX)
		end if
		
	end function Mean
	

	! Compute the population standard deviation of a signal
	function StdDev(rvX, rMeanIn, rValidFraction) result(rStdDev)
	
		! Routine arguments
		real, dimension(:), intent(in)	:: rvX				! Signal, whose standard deviation is needed
		real, intent(in), optional		:: rMeanIn			! Mean value, as computed by "Mean" function (optional, recomputed if missing)
		real, intent(out), optional		:: rValidFraction	! Fraction of valid to total signal data (optional)
		real							:: rStdDev
		
		! Locals
		integer	:: n
		real	:: rMean
		
		! Check something is to be made
		if(size(rvX) <= 0) then
			rMean = NaN
			return
		end if
		
		! Compute the arithmetic mean, if missing; or, get its value
		n = count(.not.isnan(rvX))
		if(present(rMeanIn)) then
			rMean = rMeanIn
		else
			if(n > 0) then
				rMean = sum(rvX, mask=.not.isnan(rvX)) / n
			else
				rMean = NaN
			end if
		end if
		
		! Compute the standard deviation
		if(n > 0) then
			rStdDev = sqrt(sum((rvX - rMean)**2, mask=.not.isnan(rvX)) / n)
		else
			rStdDev = NaN
		end if
		
		! Compute diagnostic quantities, if present
		if(present(rValidFraction)) then
			rValidFraction = float(n) / size(rvX)
		end if
		
	end function StdDev
	

    ! Compute the sampling covariance between two signal samples; these samples should
    ! be the same size, and "error-paired", that is, whenever rvX(i) == NaN,
    ! then rvY(i) == NaN, and vice-versa.
	function Cov(rvX, rvY) result(rCov)
	
		! Routine arguments
		real, dimension(:), intent(in)	:: rvX
		real, dimension(:), intent(in)	:: rvY
		real							:: rCov
		
		! Locals
        integer :: n
        integer :: i
		real(8)	:: rSumX
		real(8)	:: rSumY
		real(8)	:: rSumXY
        
        ! Check it makes sense to proceed
        if(size(rvX) /= size(rvY)) then
        	rCov = NaN
        	return
        end if
        n = 0
        do i = 1, size(rvX)
        	if((.valid.rvX(i)) .and. (.valid.rvY(i))) n = n + 1
        end do
        if(n <= 1) then
            rCov = NaN
            return
        end if
		
		! Accumulate sums
		rSumX  = 0.d0
		rSumY  = 0.d0
		rSumXY = 0.d0
        do i = 1, size(rvX)
        	if((.valid.rvX(i)) .and. (.valid.rvY(i))) then
        		rSumX  = rSumX + rvX(i)
        		rSumY  = rSumY + rvY(i)
        		rSumXY = rSumXY + rvX(i)*rvY(i)
        	end if
        end do
		
		! Convert counts to covariance
		rCov = rSumXY/(n-1) - (rSumX/n)*(rSumY/n)*(float(n)/(n-1))
		
	end function Cov
	
	
	! Compute the autocovariance of a signal up the specified number of lags,
	! by using the standard and the 2nd-stationary definitions, as given
	! respectively in R.B. Stull, "An Introduction to Boundary Layer Meteorology", Kluwer
	! Acedemic Publishers, 1988, and
	! W.N. Venables, B.D. Ripley, "Modern Applied Statistics with S", Springer, 2002.
	!
	function AutoCov(rvX, rvACov, iType) result(iRetCode)
	
		! Routine arguments
		real, dimension(:), intent(in)		:: rvX			! Signal (may contain NaN values)
		real, dimension(0:), intent(out)	:: rvACov		! Vector containing the desired values
		integer, intent(in), optional		:: iType		! Type of ACV (ACV_GENERAL, default: no stationarity assumption, ACV_2ND_ORDER:2nd order stationarity assumed (as in W. N. Venables, 
		integer								:: iRetCode		! Flag indicating success (value = 0) or failure.
		
		! Locals
		integer	:: iLag
		integer	:: i
		integer	:: n
		real(8)	:: rSumA
		real(8)	:: rSumB
		real(8)	:: rSumAB
		real(8)	:: rMean
		integer	:: iNum
		logical	:: lGeneral
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check parameters
		if(size(rvX) <= 0 .OR. size(rvACov) <= 0 .OR. size(rvACov) > size(rvX)) then
			rvACov = NaN
			iRetCode = 1
			return
		end IF
		n = size(rvX)
		
		! Determine type of processing
		if(present(iType)) then
			lGeneral = (iType == ACV_GENERAL)
		else
			lGeneral = .TRUE.
		end if
		
		! Compute autocovariance for each lag
		if(lGeneral) then
		
			! Compute means and averages on each lag set independently,
			! making no assumption on the stationarity of data set
			do iLag = 0, size(rvACov)-1
			
				! Compute means
				rSumA  = 0.d0
				rSumB  = 0.d0
				iNum = 0
				do i = 1, n - iLag
					if(.valid.rvX(i) .and. .valid.rvX(i+iLag)) then
						iNum   = iNum + 1
						rSumA  = rSumA + rvX(i)
						rSumB  = rSumB + rvX(i+iLag)
					end if
				end do
				
				! Compute autocovariance
				if(iNum > 0) then
					rSumAB = 0.d0
					do i = 1, n - iLag
						if(.valid.rvX(i) .and. .valid.rvX(i+iLag)) then
							rSumAB = rSumAB + (rvX(i) - rSumA/iNum) * (rvX(i+iLag) - rSumB/iNum)
						end if
					end do
					rvACov(iLag) = rSumAB/iNum
				else
					rvACov(iLag) = NaN
				end if
				
			end do
			
		else
		
			! Compute overall mean, assuming 2nd-order stationarity
			iNum  = 0
			rMean = 0.d0
			do i = 1, n
				if(.valid.rvX(i)) then
					iNum = iNum + 1
					rMean = rMean + rvX(i)
				end if
			end do
			rMean = rMean / iNum
			
			! Compute autocovariances with respect to the same overall mean
			do iLag = 0, size(rvACov)-1
				rSumAB = 0.d0
				do i = 1, n - iLag
					if(.valid.rvX(i) .and. .valid.rvX(i+iLag)) then
						rSumAB = rSumAB + (rvX(i) - rMean) * (rvX(i+iLag) - rMean)
					end if
				end do
				if(iNum > 0) then
					rvACov(iLag) = rSumAB/iNum
				else
					rvACov(iLag) = NaN
				end if
			end do
			
		end if
		
	end function AutoCov
	
	
	! Compute the autocorrellation of a signal up the specified number of lags,
	! by using the standard and the 2nd-stationary definitions, as given
	! respectively in R.B. Stull, "An Introduction to Boundary Layer Meteorology", Kluwer
	! Acedemic Publishers, 1988, and
	! W.N. Venables, B.D. Ripley, "Modern Applied Statistics with S", Springer, 2002.
	!
	function AutoCorr(rvX, rvACorr, iType) result(iRetCode)
	
		! Routine arguments
		real, dimension(:), intent(in)		:: rvX			! Signal (may contain NaN values)
		real, dimension(0:), intent(out)	:: rvACorr		! Vector containing the desired values
		integer, intent(in), optional		:: iType		! Type of ACV (ACV_GENERAL, default: no stationarity assumption, ACV_2ND_ORDER:2nd order stationarity assumed (as in W. N. Venables, 
		integer								:: iRetCode		! Flag indicating success (value = 0) or failure.
		
		! Locals
		real, dimension(:), allocatable	:: rvACov
		
		! Compute the autocovariance
		allocate(rvACov(0:(size(rvACorr)-1)))
		iRetCode = AutoCov(rvX, rvACov, iType)
		if(iRetCode /= 0) then
			deallocate(rvACov)
			return
		end if
		
		! Scale autocovariance to autocorrelation
		if(abs(rvACov(0)) > 1.e-6) then
			rvACorr(1:) = rvACov(1:) / rvACov(0)
			rvACorr(0)  = 1.
		else
			rvACorr = NaN
		end if
		
	end function AutoCorr
	
	
	! Compute the autocovariance of a signal up the specified number of lags,
	! by using the direct summation method under the mandatory assumption of
	! second-order statoinarity.
	!
	! Warning: The formula used provides the same result as in R, and is
	! described in [Venables, 2002]. The actual equation, in section 14.1
	! of [Venables, 2002] is however incorrect: the lower summation limit
	! is stated to be MAX(1,-iLag), which should be MAX(1,1-iLag) instead.
	! The R implementation is correct however: the bug is in the manual only.
	!
	function CrossCov(rvX, rvY, rvCCov, iType) result(iRetCode)
	
		! Routine arguments
		real, dimension(:), intent(in)	:: rvX			! First signal (should contain no NaN values)
		real, dimension(:), intent(in)	:: rvY			! Second signal (should contain no NaN values)
		real, dimension(:), intent(out)	:: rvCCov		! Vector containing the desired values, dimensioned (-iLagMax:iLagMax) where iLagMax > 0
		integer, intent(in), optional	:: iType		! Type of ACV (ACV_GENERAL, default: no stationarity assumption, ACV_2ND_ORDER:2nd order stationarity assumed (as in W. N. Venables, 
		integer							:: iRetCode		! Flag indicating success (value = 0) or failure.
		
		! Locals
		logical	:: lGeneral
		integer	:: iLag
		integer	:: iLagMax
		integer	:: i
		integer	:: iMin
		integer	:: iMax
		integer	:: n
		real(8)	:: rMeanX
		real(8)	:: rMeanY
		real(8)	:: rSumAB
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check parameters
		if(size(rvX) <= 0 .OR. size(rvY) <= 0) then
			rvCCov = NaN
			iRetCode = 1
			return
		end IF
		if(size(rvX) /= size(rvY)) then
			rvCCov = NaN
			iRetCode = 2
			return
		end IF
		if(size(rvCCov) <= 0 .OR. size(rvCCov) > size(rvX)) then
			rvCCov = NaN
			iRetCode = 3
			return
		end IF
		if(mod(size(rvCCov),2) /= 1) then
			rvCCov = NaN
			iRetCode = 4
			return
		end IF
		if(any(.invalid.rvX) .OR. any(.invalid.rvY)) then
			rvCCov = NaN
			iRetCode = 5
			return
		end IF
		n       = size(rvX)
		iLagMax = (size(rvCCov) - 1) / 2
		
		! Determine type of processing
		if(present(iType)) then
			lGeneral = (iType == ACV_GENERAL)
		else
			lGeneral = .TRUE.
		end if
		
		! Compute autocovariance for each lag
		if(lGeneral) then
				
			! Compute autocovariances with respect to the same overall mean
			do iLag = -iLagMax, iLagMax
				iMin = max(1,1-iLag)
				iMax = min(n - iLag,n)
				rMeanX = sum(rvX(iMin:iMax)) / (iMax-iMin)
				rMeanY = sum(rvY(iMin:iMax)) / (iMax-iMin)
				rSumAB = 0.d0
				do i = iMin, iMax
					rSumAB = rSumAB + (rvX(i+iLag) - rMeanX) * (rvY(i) - rMeanY)
				end do
				rvCCov(iLag+iLagMax+1) = rSumAB / (iMax-iMin)
			end do
		
		else
		
			! Compute overall mean, assuming 2nd-order stationarity
			rMeanX = sum(rvX) / n
			rMeanY = sum(rvY) / n
		
			! Compute autocovariances with respect to the same overall mean
			do iLag = -iLagMax, iLagMax
				rSumAB = 0.d0
				do i = max(1,1-iLag), min(n - iLag,n)
					rSumAB = rSumAB + (rvX(i+iLag) - rMeanX) * (rvY(i) - rMeanY)
				end do
				rvCCov(iLag+iLagMax+1) = rSumAB / n
			end do
		
		end if
		
	end function CrossCov
	
	
	! Compute the autocorrelation of a signal up the specified number of lags,
	! by using the direct summation method.
	function CrossCorr(rvX, rvY, rvCCorr, iType) result(iRetCode)
	
		! Routine arguments
		real, dimension(:), intent(in)	:: rvX			! First signal (may contain NaN values)
		real, dimension(:), intent(in)	:: rvY			! Second signal (may contain NaN values)
		real, dimension(:), intent(out)	:: rvCCorr		! Vector containing the desired values, dimensioned (-iLagMax:iLagMax) where iLagMax > 0
		integer, intent(in), optional	:: iType		! Type of ACV (ACV_GENERAL, default: no stationarity assumption, ACV_2ND_ORDER:2nd order stationarity assumed (as in W. N. Venables, 
		integer							:: iRetCode		! Flag indicating success (value = 0) or failure.
		
		! Locals
		logical	:: lGeneral
		integer	:: iLag
		integer	:: iLagMax
		integer	:: i
		integer	:: iMin
		integer	:: iMax
		integer	:: n
		real(8)	:: rMeanX
		real(8)	:: rMeanY
		real(8)	:: rSigmaX
		real(8)	:: rSigmaY
		real(8)	:: rSumAB
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check parameters
		if(size(rvX) <= 0 .OR. size(rvY) <= 0) then
			rvCCorr = NaN
			iRetCode = 1
			return
		end IF
		if(size(rvX) /= size(rvY)) then
			rvCCorr = NaN
			iRetCode = 2
			return
		end IF
		if(size(rvCCorr) <= 0 .OR. size(rvCCorr) > size(rvX)) then
			rvCCorr = NaN
			iRetCode = 3
			return
		end IF
		if(mod(size(rvCCorr),2) /= 1) then
			rvCCorr = NaN
			iRetCode = 4
			return
		end IF
		if(any(.invalid.rvX) .OR. any(.invalid.rvY)) then
			rvCCorr = NaN
			iRetCode = 5
			return
		end IF
		n       = size(rvX)
		iLagMax = (size(rvCCorr) - 1) / 2
		
		! Determine type of processing
		if(present(iType)) then
			lGeneral = (iType == ACV_GENERAL)
		else
			lGeneral = .TRUE.
		end if
		
		! Compute autocovariance for each lag
		if(lGeneral) then
				
			! Compute autocovariances with respect to the same overall mean
			do iLag = -iLagMax, iLagMax
				iMin = max(1,1-iLag)
				iMax = min(n - iLag,n)
				rMeanX  = sum(rvX(iMin:iMax)) / (iMax-iMin)
				rMeanY  = sum(rvY(iMin:iMax)) / (iMax-iMin)
				rSigmaX = sqrt(sum((rvX(iMin:iMax) - rMeanX)**2) / (iMax-iMin))
				rSigmaY = sqrt(sum((rvY(iMin:iMax) - rMeanY)**2) / (iMax-iMin))
				rSumAB = 0.d0
				do i = iMin, iMax
					rSumAB = rSumAB + (rvX(i+iLag) - rMeanX) * (rvY(i) - rMeanY)
				end do
				rvCCorr(iLag+iLagMax+1) = (rSumAB / (iMax-iMin)) / (rSigmaX*rSigmaY)
			end do
		
		else
		
			! Compute overall mean and std.dev., assuming 2nd-order stationarity
			rMeanX  = sum(rvX) / n
			rMeanY  = sum(rvY) / n
			rSigmaX = sqrt(sum((rvX - rMeanX)**2) / n)
			rSigmaY = sqrt(sum((rvY - rMeanY)**2) / n)
		
			! Compute autocovariances with respect to the same overall mean
			do iLag = -iLagMax, iLagMax
				rSumAB = 0.d0
				do i = max(1,1-iLag), min(n - iLag,n)
					rSumAB = rSumAB + (rvX(i+iLag) - rMeanX) * (rvY(i) - rMeanY)
				end do
				rvCCorr(iLag+iLagMax+1) = (rSumAB / n) / (rSigmaX*rSigmaY)
			end do
		
		end if
		
	end function CrossCorr
	
	
	! Partial autocorrelation values, from autocorrelation. Useful, to determine
	! the order of an autoregressive process.
	function PartialAutoCorr(rvACov) result(rvPACorr)
		
		! Routine arguments
		real, dimension(0:), intent(in)	:: rvACov		! Autocovariance values
		real, dimension(size(rvACov)-1)	:: rvPACorr		! Partial autocorrelation, same indexing convention as above
		
		! Locals
		real, dimension(:), allocatable			:: phi
		real, dimension(:), allocatable			:: phiNew
		real, dimension(:), allocatable			:: rho
		real									:: numer
		real									:: denom
		real									:: total
		integer									:: j
		integer									:: k
		integer									:: km1
		integer									:: n
		
		! Check no gaps exist in autocorrelation, and they constitute a non-negative
		! decreasing sequence (the PACF makes sense in case of autoregressive
		! processes)
		if(any(.invalid.rvACov)) then
			rvPACorr = NaN
			return
		end if
		n = size(rvACov) - 1
		if(rvACov(0) <= 0.) then
			rvPACorr = NaN
			return
		end if
		
		! Reserve workspace
		allocate(phi(n), phiNew(n), rho(n))
		phi = 0.d0
		rho = 0.d0
		
		! Compute partial autocorrelation by Durbin-Levinson algorithm
		! (see [Brockwell, 2002], section 2.5.1, for clarifications).
		! The implementation follows prof. G.R. Ihaka's (see [Ihaka, web1])
		rho = rvACov(1:n)/rvACov(0)
		phi(1) = rho(1)
		rvPACorr(1) = phi(1)
		
		do k = 2, n
			km1 = k - 1
			total = 0.d0
			do j = 1, km1
				total = total + phi(j)*rho(km1-j+1)
			end do
			numer = rho(k) - total
			denom = 1.d0 - dot_product(phi(1:km1),rho(1:km1))
			phi(k) = numer / denom
			do j = 1, km1
				phiNew(j) = phi(j) - phi(k) * phi(km1-j+1)
			end do
			phi(1:km1) = phiNew(1:km1)
			rvPACorr(k) = phi(k)
		end do
		
		! Leave
		deallocate(phi, phiNew, rho)
		
	end function PartialAutoCorr
	
	
	! Estimate the Euleriam decorrelation time of a signal
	!
	function EulerianTime(rDataRate, rvX, rMaxEulerianTime, rEulerianTime, rvACorr) result(iRetCode)
	
		! Routine arguments
		real, intent(in)				:: rDataRate			! Data acquisition rate (Hz)
		real, dimension(:), intent(in)	:: rvX					! Signal (any unit)
		real, intent(in)				:: rMaxEulerianTime		! Maximum Eulerian time to consider
		real, intent(out)				:: rEulerianTime		! Estimate of Eulerian decorrelation time (s)
		real, dimension(:), allocatable, optional	:: rvACorr	! Autocorrelation found, for diagnostic purposes
		integer							:: iRetCode
		
		! Locals
		real, dimension(:), allocatable	:: rvC
		integer							:: iErrCode
		integer							:: iMaxLag
		integer							:: i
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check parameters
		if(rDataRate <= 0. .or. rMaxEulerianTime <= 0. .or. size(rvX) <= 0) then
			rEulerianTime = NaN
			iRetCode = 1
			return
		end if
		if(any(.invalid.rvX)) then
			rEulerianTime = NaN
			iRetCode = 2
			return
		end if
		
		! Compute the maximum lag
		iMaxLag = rMaxEulerianTime * rDataRate
		if(iMaxLag >= size(rvX)) then
			rEulerianTime = NaN
			iRetCode = 3
			return
		end if
		if(iMaxLag <= 0) then
			rEulerianTime = 0.
			iRetCode = 4
			return
		end if
		
		! Compute autocorrelations
		allocate(rvC(0:iMaxLag))
		iErrCode = AutoCorr(rvX, rvC, ACV_2ND_ORDER)
		if(iErrCode /= 0) then
			rEulerianTime = NaN
			iRetCode = 5
			deallocate(rvC)
			return
		end if
		
		! Estimate the Eulerian decorrelation time
		rEulerianTime = iMaxLag / rDataRate
		do i = 1, iMaxLag - 1
			if(rvC(i) < 1.96/sqrt(float(size(rvX) - i))) then
				rEulerianTime = (i-1) / rDataRate
				if(present(rvACorr)) then
					if(allocated(rvACorr)) deallocate(rvACorr)
					allocate(rvACorr(0:iMaxLag))
					rvACorr = rvC
				end if
				deallocate(rvC)
				return
			end if
		end do
		deallocate(rvC)
		iRetCode = 6
		
	end function EulerianTime
	
	
	! Remove linear trend, if any, from a signal.
	!
	! The signal is modified by eliminating the trend found, but
	! leaving the riginal mean unchanged.
	function RemoveLinearTrend(rvX, rvY, rMultiplier, rOffset) result(iRetCode)
	
		! Routine argument
		real(8), dimension(:), intent(in)	:: rvX			! Index signal (typically time, in floating point form)
		real, dimension(:), intent(inout)	:: rvY			! Signal to remove the trend from
		real(8), intent(out)				:: rMultiplier	! Multiplier of trend line
		real(8), intent(out)				:: rOffset		! Offset of trend line
		integer								:: iRetCode
		
		! Locals
		integer	:: n
		real(8)	:: rSx
		real(8)	:: rSy
		real(8)	:: rSxx
		real(8)	:: rSxy
		real(8)	:: rDelta
		real(8)	:: rMeanBeforeDetrend
		real(8)	:: rMeanAfterDetrend
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check input parameters
		if(size(rvX) <= 0 .or. size(rvY) <= 0) then
			iRetCode = 1
			return
		end if
		if(size(rvX) /= size(rvY)) then
			iRetCode = 2
			return
		end if
		if(any(.invalid.rvX) .or. any(.invalid.rvY)) then
			iRetCode = 3
			return
		end if
		
		! Compute counts and sums
		n    = size(rvX)
		rSx  = sum(rvX)
		rSy  = sum(dble(rvY))
		rSxx = dot_product(rvX,rvX)
		rSxy = dot_product(rvX,dble(rvY))
		rMeanBeforeDetrend = rSy / n
		
		! Compute multiplier and offset
		rDelta      = n*rSxx - rSx**2
		if(rDelta <= 0.d0) then
			iRetCode = 4
			return
		end if
		rOffset     = (rSxx*rSy - rSx*rSxy)/rDelta
		rMultiplier = (n*rSxy - rSx*rSy)/rDelta
		
		! Subtract the linear trend
		rvY = rvY - rMultiplier*(rvX - rSx/n)
		
		! Remove residual average, and add back the original mean
		rMeanAfterDetrend = sum(dble(rvY)) / n
		rvY = rvY - rMeanAfterDetrend + rMeanBeforeDetrend
		rOffset = rOffset - rMeanAfterDetrend + rMeanBeforeDetrend
	
	end function RemoveLinearTrend
	
	
	function tsCreateEmpty(this, n) result(iRetCode)
	
		! Routine arguments
		class(TimeSeries), intent(inout)	:: this			! Current time series
		integer, intent(in)					:: n			! Number of elements (must be positive)
		integer								:: iRetCode		! Return code (0 if successful completion; any non-zero in case of error(s))
		
		! Locals
		integer	:: iErrCode
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check input parameters
		if(n <= 0) then
			iRetCode = 1
			return
		end if
		
		! Reserve workspace
		if(allocated(this % rvTimeStamp)) deallocate(this % rvTimeStamp)
		if(allocated(this % rvValue)) deallocate(this % rvValue)
		allocate(this % rvTimeStamp(n), stat = iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 2
			return
		end if
		allocate(this % rvValue(n), stat = iErrCode)
		if(iErrCode /= 0) then
			deallocate(this % rvTimeStamp)
			iRetCode = 2
			return
		end if
		
		! Fill with appropriate initial values
		this % rvTimeStamp = NaN_8
		this % rvValue     = NaN
		
	end function tsCreateEmpty
	
	
	function tsIsEmpty(this) result(lIsEmpty)
	
		! Routine arguments
		class(TimeSeries), intent(in)	:: this			! Current time series
		logical							:: lIsEmpty		! Flag indicating (.true.) whether a time series is still unallocated or empty, or (.true.) not
		
		! Locals
		! --none--
		
		! Check allocation state
		if(.not.allocated(this % rvTimeStamp) .or. .not.allocated(this % rvValue)) then
			! Not yet allocated
			lIsEmpty = .true.
		else
			if(size(this % rvTimeStamp) <= 0 .or. size(this % rvValue) <= 0) then
				lIsEmpty = .true.
			else
				! Both vectors are allocated: do they contain something?
				lIsEmpty = all(.invalid.this % rvValue)
			end if
		end if
		
	end function tsIsEmpty
	
	
	function tsCreateFromDataVector(this, rvValues, rTimeFrom, rDeltaTime) result(iRetCode)
	
		! Routine arguments
		class(TimeSeries), intent(inout)	:: this			! Current time series
		real, dimension(:), intent(in)		:: rvValues		! Data values
		real(8), intent(in)					:: rTimeFrom	! Initial date-time (s since the Epoch)
		real(8), intent(in), optional		:: rDeltaTime	! Time difference between two any series elements (default: 1.d0; must be positive if present)
		integer								:: iRetCode		! Return code (0 if successful completion; any non-zero in case of error(s))
		
		! Locals
		integer	:: iErrCode
		integer	:: i
		integer	:: n
		real(8)	:: rTimeIncrement
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check input parameters
		n = size(rvValues)
		if(n <= 0) then
			iRetCode = 1
			return
		end if
		if(present(rDeltaTime)) then
			if(rDeltaTime <= 0.d0) then
				iRetCode = 2
				return
			else
				rTimeIncrement = rDeltaTime
			end if
		else
			rTimeIncrement = 1.d0
		end if
		
		! Reserve workspace
		if(allocated(this % rvTimeStamp)) deallocate(this % rvTimeStamp)
		if(allocated(this % rvValue)) deallocate(this % rvValue)
		allocate(this % rvTimeStamp(n), stat = iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 3
			return
		end if
		allocate(this % rvValue(n), stat = iErrCode)
		if(iErrCode /= 0) then
			deallocate(this % rvTimeStamp)
			iRetCode = 3
			return
		end if
		
		! Fill with appropriate initial values
		this % rvTimeStamp = [(rTimeFrom + rTimeIncrement*(i-1), i = 1, n)]
		this % rvValue     = rvValues
		
	end function tsCreateFromDataVector
	
	
	function tsCreateFromTimeAndDataVectors(this, rvTimeStamp, rvValues) result(iRetCode)
	
		! Routine arguments
		class(TimeSeries), intent(inout)	:: this			! Current time series
		real(8), dimension(:), intent(in)	:: rvTimeStamp	! Time stamp values
		real, dimension(:), intent(in)		:: rvValues		! Data values (rvValue(i) corresponds to rvTimeStamp(i), i=1,...)
		integer								:: iRetCode		! Return code (0 if successful completion; any non-zero in case of error(s))
		
		! Locals
		integer	:: iErrCode
		integer	:: i
		integer	:: n
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check input parameters
		n = size(rvValues)
		if(n <= 0) then
			iRetCode = 1
			return
		end if
		if(n /= size(rvTimeStamp)) then
			iRetCode = 2
			return
		end if
		
		! Reserve workspace
		if(allocated(this % rvTimeStamp)) deallocate(this % rvTimeStamp)
		if(allocated(this % rvValue)) deallocate(this % rvValue)
		allocate(this % rvTimeStamp(n), stat = iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 3
			return
		end if
		allocate(this % rvValue(n), stat = iErrCode)
		if(iErrCode /= 0) then
			deallocate(this % rvTimeStamp)
			iRetCode = 3
			return
		end if
		
		! Fill with appropriate initial values
		do i = 1, n
			this % rvTimeStamp(i) = rvTimeStamp(i)
			this % rvValue(i)     = rvValues(i)
		end do
		
	end function tsCreateFromTimeAndDataVectors
	
	
	function tsGetSingleItem(this, iItemIdx, rTimeStamp, rValue) result(iRetCode)
	
		! Routine arguments
		class(TimeSeries), intent(in)	:: this
		integer, intent(in)				:: iItemIdx
		real(8), intent(out)			:: rTimeStamp
		real, intent(out)				:: rValue
		integer							:: iRetCode
		
		! Locals
		! -none-
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check parameters
		if(this % isEmpty()) then
			rTimeStamp = NaN_8
			rValue     = NaN
			iRetCode   = 1
			return
		end if
		if(iItemIdx <= 0 .or. iItemIdx > size(this % rvTimeStamp)) then
			rTimeStamp = NaN_8
			rValue     = NaN
			iRetCode   = 2
			return
		end if
		
		! Gather value
		rTimeStamp = this % rvTimeStamp(iItemIdx)
		rValue     = this % rvValue(iItemIdx)
		
	end function tsGetSingleItem
	
	
	function tsPutSingleItem(this, iItemIdx, rTimeStamp, rValue) result(iRetCode)
	
		! Routine arguments
		class(TimeSeries), intent(inout)	:: this
		integer, intent(in)					:: iItemIdx
		real(8), intent(in)					:: rTimeStamp
		real, intent(in)					:: rValue
		integer								:: iRetCode
		
		! Locals
		! -none-
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check parameters
		if(this % isEmpty()) then
			iRetCode   = 1
			return
		end if
		if(iItemIdx <= 0 .or. iItemIdx > size(this % rvTimeStamp)) then
			iRetCode   = 2
			return
		end if
		
		! Gather value
		this % rvTimeStamp(iItemIdx) = rTimeStamp
		this % rvValue(iItemIdx)     = rValue
		
	end function tsPutSingleItem
	
	
	subroutine tsSummary(this, iNumValues, rValidPercentage, rMin, rMean, rStdDev, rMax)
	
		! Routine arguments
		class(TimeSeries), intent(in)	:: this
		integer, intent(out)			:: iNumValues
		real, intent(out)				:: rValidPercentage
		real, intent(out)				:: rMin
		real, intent(out)				:: rMean
		real, intent(out)				:: rStdDev
		real, intent(out)				:: rMax
		
		! Locals
		! -none-
		
		! Get size and valid count
		if(.not.allocated(this % rvValue)) then
			iNumValues       = 0.
			rValidPercentage = 0.
			rMin             = NaN
			rMean            = NaN
			rStdDev          = NaN
			rMax             = NaN
		else
			iNumValues = size(this % rvValue)
			if(iNumValues > 0) then
				rValidPercentage = 100.0 * count(.valid. this % rvValue) / iNumValues
				rMin             = minval(this % rvValue, mask = .valid. this % rvValue)
				rMean            = sum(this % rvValue, mask = .valid. this % rvValue) / iNumValues
				rStdDev          = sqrt(sum((this % rvValue - rMean)**2, mask = .valid. this % rvValue) / iNumValues)
				rMax             = maxval(this % rvValue, mask = .valid. this % rvValue)
			else
				rValidPercentage = 0.
				rMin             = NaN
				rMean            = NaN
				rStdDev          = NaN
				rMax             = NaN
			end if
		end if
		
	end subroutine tsSummary
	
	
	subroutine tsRangeInvalidate(this, rMin, rMax)
	
		! Routine arguments
		class(TimeSeries), intent(inout)	:: this
		real, intent(in)					:: rMin
		real, intent(in)					:: rMax
		
		! Locals
		real	:: rMinVal, rMaxVal
		
		! Ensure limits ordering
		if(rMin <= rMax) then
			rMinVal = rMin
			rMaxVal = rMax
		else
			rMinVal = rMax
			rMaxVal = rMin
		end if
		
		! Invalidate by Range
		if(.not.this % isEmpty()) then
			call RangeInvalidate(this % rvValue, rMinVal, rMaxVal)
		end if
		
	end subroutine tsRangeInvalidate
	
	
	function tsTimeMonotonic(this) result(lIsMonotonic)
	
		! Routine arguments
		class(TimeSeries), intent(in)	:: this
		logical							:: lIsMonotonic
		
		! Locals
		integer		:: n
		integer		:: i
		
		! Check parameters
		if(this % isEmpty()) then
			lIsMonotonic = .false.
			return
		end if
		n = size(this % rvTimeStamp)
		if(n <= 1) then
			lIsMonotonic = .false.
			return
		end if
		
		! Check time stamps are strictly increasing
		lIsMonotonic = .true.
		do i = 2, n
			if(this % rvTimeStamp(i-1) >= this % rvTimeStamp(i)) then
				lIsMonotonic = .false.
				return
			end if
		end do
	
	end function tsTimeMonotonic
	
	
	function tsTimeQuasiMonotonic(this) result(lIsMonotonic)
	
		! Routine arguments
		class(TimeSeries), intent(in)	:: this
		logical							:: lIsMonotonic
		
		! Locals
		integer		:: n
		integer		:: i
		
		! Check parameters
		if(this % isEmpty()) then
			lIsMonotonic = .false.
			return
		end if
		n = size(this % rvTimeStamp)
		if(n <= 1) then
			lIsMonotonic = .false.
			return
		end if
		
		! Check time stamps are strictly increasing
		lIsMonotonic = .true.
		do i = 2, n
			if(this % rvTimeStamp(i-1) > this % rvTimeStamp(i)) then
				lIsMonotonic = .false.
				return
			end if
		end do
	
	end function tsTimeQuasiMonotonic
	
	
	function tsTimeGapless(this) result(lIsGapless)
	
		! Routine arguments
		class(TimeSeries), intent(in)	:: this
		logical							:: lIsGapless
		
		! Locals
		integer		:: n
		integer		:: i
		
		! Check parameters
		if(this % isEmpty()) then
			lIsGapless = .false.
			return
		end if
		n = size(this % rvTimeStamp)
		if(n <= 1) then
			lIsGapless = .false.
			return
		end if
		
		! Check time stamps are strictly increasing
		lIsGapless = .true.
		do i = 1, n
			if(.invalid.this % rvTimeStamp(i)) then
				lIsGapless = .false.
				return
			end if
		end do
	
	end function tsTimeGapless
	
end module pbl_stat
