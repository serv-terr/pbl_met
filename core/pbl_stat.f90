! pbl_stat  : Fortran module, providing support to elementary statistics.
!
! This module is part of the pbl_met library.
!
! This is open-source code, covered by the lGPL 3.0 license.
!
! Author(s): Patrizia Favaron
!
module pbl_stat

    use ieee_arithmetic
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
    public	:: Quantile
    public	:: Skew
    public	:: Kurt
	! 3. US-EPA validation statistics
	public	:: FB
	public	:: FAC2
    ! 4. Autocovariance, autocorrelation and related
    public	:: AutoCov
    public	:: AutoCorr
    public	:: PartialAutoCorr
    public	:: EulerianTime
    ! 5. Cross-covariance, cross-correlation and related
    public	:: CrossCov
    public	:: CrossCorr
    ! 6. Peak detection
    ! 7. Utilities
    public	:: RemoveLinearTrend
	public	:: SimpleLinearRegression
    ! 8. Time series
    public	:: TimeSeries
    public	:: TDELTA_YEAR
    public	:: TDELTA_MONTH
    public	:: TDELTA_YEARMONTH
    public	:: TDELTA_ONEMINUTE
    public	:: TDELTA_ONEHOUR
    public	:: TDELTA_ONEDAY
    public	:: FUN_MEAN
    public	:: FUN_STDEV
    public	:: FUN_MIN
    public	:: FUN_MAX
    public	:: QUANT_POPULATION	! Population quantile
    public	:: QUANT_1			! Sample quantile type 1 (R-1, SAS-3, Maple-1; inverse of edf)
    public	:: QUANT_2			! Sample quantile type 2 (R-2, SAS-5, Maple-2; same as R-1, with averaging at discontinuities)
    public	:: QUANT_3			! Sample quantile type 3 (R-3, Closest observation)
    public	:: QUANT_3_R		! Synonym of QUANT_3
    public	:: QUANT_3_SAS		! Sample quantile type 3 (SAS-2; Closest observation, but with an own definition of "closest integer")
    public	:: QUANT_4			! Sample quantile type 4 (R-4, SAS-1, Maple-3; Linear interpolation of edf)
    public	:: QUANT_5			! Sample quantile type 5 (R-5, Maple-4; piecewise linear function with nodes at midway of edf values)
    public	:: QUANT_6			! Sample quantile type 6 (R-6, SAS-4, Maple-5, Excel; Linear interpolation of order statistics for uniform distribution on [0,1])
    public	:: QUANT_7			! Sample quantile type 7 (R-7, Maple-6, Excel, NumPy; Linear interpolation of the modes of the order statistics for uniform distribution on [0,1])
    public	:: QUANT_8			! Sample quantile type 8 (R-8, Maple-7; ***DEFAULT***; Linear interpolation of approximate medians of order statistics; Distribution-independent)
    public	:: QUANT_9			! Sample quantile type 9 (R-9, Maple-8; Defined so that the resulting quantile estimates are approximately unbiased for the expected order statistics; Valid if data are normally distributed)
    public	:: MA_ALLDATA		! Use all available data when computing centered moving averages
    public	:: MA_STRICT		! Discard incomplete upper and lower time tails when computing centered moving averages
	! 9. Multivariate series
	public	:: MultiSeries

    ! Data types

    type TimeSeries
    	real(8), dimension(:), allocatable, private	:: rvTimeStamp
    	real, dimension(:), allocatable, private	:: rvValue
    contains
    	! Constructors
    	procedure, public	:: createEmpty						=> tsCreateEmpty
    	procedure, public	:: createFromTimeSeries				=> tsCreateFromTimeSeries
    	procedure, public	:: createFromDataVector				=> tsCreateFromDataVector
    	procedure, public	:: createFromTimeAndDataVectors		=> tsCreateFromTimeAndDataVectors
    	! Modifiers and reshapers
    	procedure, public	:: populateFromDataVector			=> tsCreateFromDataVector
    	procedure, public	:: populateFromTimeAndDataVectors	=> tsCreateFromTimeAndDataVectors
    	procedure, public	:: rangeInvalidate					=> tsRangeInvalidate
    	procedure, public	:: timeShift						=> tsTimeShift
    	procedure, public	:: timeReorder						=> tsTimeReorder
    	! Selectors
    	procedure, public	:: getSingleItem					=> tsGetSingleItem
    	procedure, public	:: getTimeStamp						=> tsGetTimeStamp
    	procedure, public	:: getValues						=> tsGetValues
    	procedure, public	:: getTimeSubset					=> tsGetTimeSubset
    	procedure, public	:: getMonth							=> tsGetMonth
    	! Assigners
    	procedure, public	:: putSingleItem					=> tsPutSingleItem
    	! Summary generators
    	procedure, public	:: size								=> tsSize
    	procedure, public	:: isSameTimes						=> tsIsSameTimes
    	procedure, public	:: summary							=> tsSummary
    	procedure, public	:: getTimeSpan						=> tsGetTimeSpan
    	! State interrogations
    	procedure, public	:: isEmpty							=> tsIsEmpty
    	procedure, public	:: timeIsMonotonic					=> tsTimeMonotonic
    	procedure, public	:: timeIsQuasiMonotonic				=> tsTimeQuasiMonotonic
    	procedure, public	:: timeIsGapless					=> tsTimeGapless
    	procedure, public	:: timeIsWellSpaced					=> tsTimeWellSpaced
    	! Aggregators
    	procedure, public	:: aggregateLinear					=> tsAggregateLinear
    	procedure, public	:: aggregateLinear2					=> tsAggregateLinear2
    	procedure, public	:: aggregatePeriodic				=> tsAggregatePeriodic
    	! Smoothers
    	procedure, public	:: movingAverage					=> tsMovingAverage
    	procedure, public	:: movingStdDev						=> tsMovingStdDev
    	! Gap fillers
    	procedure, public	:: fillGaps							=> tsFillGaps
    end type TimeSeries


    type MultiSeries
    	real(8), dimension(:), allocatable, private				:: rvTimeStamp
		character(len=16), dimension(:), allocatable, private	:: svColumn
    	real, dimension(:,:), allocatable, private				:: rmValue
    contains
    	! Constructors
    	procedure, public	:: createEmpty						=> msCreateEmpty
    	procedure, public	:: addTimeSeries					=> msAddTimeSeries
    	! Selectors
    	procedure, public	:: getTimeSeries					=> msGetTimeSeries
    	procedure, public	:: getTimeStamp						=> msGetTimeStamp
    	procedure, public	:: getVector						=> msGetVector
    	! State interrogations
    	procedure, public	:: isEmpty							=> msIsEmpty
    end type MultiSeries


	type TwoDimensionalField
		real(8), dimension(:,:), allocatable	:: rmValue
		real(8), dimension(:), allocatable		:: rvX
		real(8), dimension(:), allocatable		:: rvY
		integer, dimension(:,:), allocatable	:: imNumAdjacent
		integer, dimension(:,:,:), allocatable	:: iaAdjacent
		real(8), dimension(:,:,:), allocatable	:: raDistance
	contains
		procedure, public	:: clean				=> dfClean
		procedure, public	:: initialize			=> dfInitialize
		procedure, public	:: evaluate				=> dfEvaluate
	end type TwoDimensionalField

    ! Constants

    integer, parameter	:: TDELTA_YEARMONTH  =    -3
    integer, parameter	:: TDELTA_YEAR       =    -2
    integer, parameter	:: TDELTA_MONTH      =    -1
    integer, parameter	:: TDELTA_ONEMINUTE  =    60
    integer, parameter	:: TDELTA_ONEHOUR    =  3600
    integer, parameter	:: TDELTA_ONEDAY     = 86400
    integer, parameter	:: FUN_MEAN          =     0
    integer, parameter	:: FUN_STDEV         =     1
    integer, parameter	:: FUN_MIN           =     2
    integer, parameter	:: FUN_MAX           =     3
    integer, parameter	:: QUANT_POPULATION  =     0
    integer, parameter	:: QUANT_1           =     1
    integer, parameter	:: QUANT_2           =     2
    integer, parameter	:: QUANT_3           =     3
    integer, parameter	:: QUANT_3_R         =     QUANT_3
    integer, parameter	:: QUANT_3_SAS       =    10
    integer, parameter	:: QUANT_4           =     4
    integer, parameter	:: QUANT_5           =     5
    integer, parameter	:: QUANT_6           =     6
    integer, parameter	:: QUANT_7           =     7
    integer, parameter	:: QUANT_8           =     8
    integer, parameter	:: QUANT_9           =     9
    integer, parameter	:: MA_ALLDATA        =     0
    integer, parameter	:: MA_STRICT         =     1

    ! Polymorphic interfaces

    interface RangeInvalidate
    	module procedure	:: RangeInvalidate4
    	module procedure	:: RangeInvalidate8
    end interface RangeInvalidate

    interface PairInvalidate
    	module procedure	:: PairInvalidate4
    	module procedure	:: PairInvalidate8
    end interface PairInvalidate

    interface RangeClip
    	module procedure	:: RangeClip4
    	module procedure	:: RangeClip8
    end interface RangeClip

    interface GetValidOnly
    	module procedure	:: GetValidOnly4
    	module procedure	:: GetValidOnly8
    end interface GetValidOnly

    interface Quantile
    	module procedure	:: QuantileScalar
    	module procedure	:: QuantileVector
    end interface Quantile

    interface SimpleLinearRegression
    	module procedure	:: SimpleLinearRegression4
    	module procedure	:: SimpleLinearRegression8
    end interface SimpleLinearRegression

    interface FAC2
    	module procedure	:: FAC2_4
    	module procedure	:: FAC2_8
    end interface FAC2

    interface FB
    	module procedure	:: FB_4
    	module procedure	:: FB_8
    end interface FB

contains

	! Make data outside a specified range invalid, by replacing their value with NaN
	subroutine RangeInvalidate4(rvX, rMin, rMax)

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

	end subroutine RangeInvalidate4


		! Make data outside a specified range invalid, by replacing their value with NaN
	subroutine RangeInvalidate8(rvX, rMin, rMax)

		! Routine arguments
		real(8), dimension(:), intent(inout)	:: rvX		! Vector of data to range-invalidate
		real(8), intent(in)						:: rMin		! Minimum allowed value
		real(8), intent(in)						:: rMax		! Maximum allowed value

		! Locals
		integer	:: i

		! Validate by range
		do i = 1, size(rvX)
			if(rvX(i) < rMin) then
				rvX(i) = NaN_8
			elseif(rvX(i) > rMax) then
				rvX(i) = NaN_8
			end if
		end do

	end subroutine RangeInvalidate8


	! Make invalid data in a vector invalid if those of another also are, and viceversa.
	subroutine PairInvalidate4(rvX, rvY)

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

	end subroutine PairInvalidate4


	! Make invalid data in a vector invalid if those of another also are, and viceversa.
	subroutine PairInvalidate8(rvX, rvY)

		! Routine arguments
		real(8), dimension(:), intent(inout)	:: rvX		! Vector to pair-invalidate
		real(8), dimension(:), intent(inout)	:: rvY		! Vector to pair-invalidate

		! Locals
		integer	:: i
		integer	:: iMin, iMax

		! Compute loop limits from array dimensions
		iMin = max(lbound(rvX,dim=1), lbound(rvY,dim=1))
		iMax = min(ubound(rvX,dim=1), ubound(rvY,dim=1))

		! Ensure invalid positions in one vector are propagated to the other
		do i = iMin, iMax
			if(.invalid. rvX(i)) then
				rvY(i) = NaN_8
			elseif(.invalid. rvY(i)) then
				rvX(i) = NaN_8
			end if
		end do

	end subroutine PairInvalidate8


	! Force data to be within a specified range invalid, clipping to extremal values
	subroutine RangeClip4(rvX, rMin, rMax)

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

	end subroutine RangeClip4


	! Force data to be within a specified range invalid, clipping to extremal values
	subroutine RangeClip8(rvX, rMin, rMax)

		! Routine arguments
		real(8), dimension(:), intent(inout)	:: rvX		! Vector of data to range-clip
		real(8), intent(in)						:: rMin		! Minimum allowed value
		real(8), intent(in)						:: rMax		! Maximum allowed value

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

	end subroutine RangeClip8


	! Pack a vector to another vector containing only valid (i.e. non-NaN) data
	function GetValidOnly4(rvX) result(rvValidX)

		! Routine arguments
		real, dimension(:), intent(in)	:: rvX			! Vector of data containing zero or more NaN
		real, dimension(:), allocatable	:: rvValidX		! The same vector, with all NaN values stripped

		! Locals
		integer	:: iNumValid
		integer	:: i, j

		! Count valid data, and check something is to be made
		iNumValid = count(.not.ieee_is_nan(rvX))
		if(allocated(rvValidX)) deallocate(rvValidX)
		if(size(rvX) <= 0 .or. iNumValid <= 0) then
			allocate(rvValidX(0))
			return
		end if

		! Loop over data, copying valids only to the new vector
		allocate(rvValidX(iNumValid))
		j = 0
		do i = 1, size(rvX)
			if(.not.ieee_is_nan(rvX(i))) then
				j = j + 1
				rvValidX(j) = rvX(i)
			end if
		end do

	end function GetValidOnly4


	! Pack a vector to another vector containing only valid (i.e. non-NaN) data
	function GetValidOnly8(rvX) result(rvValidX)

		! Routine arguments
		real(8), dimension(:), intent(in)	:: rvX			! Vector of data containing zero or more NaN
		real(8), dimension(:), allocatable	:: rvValidX		! The same vector, with all NaN values stripped

		! Locals
		integer	:: iNumValid
		integer	:: i, j

		! Count valid data, and check something is to be made
		iNumValid = count(.not.ieee_is_nan(rvX))
		if(allocated(rvValidX)) deallocate(rvValidX)
		if(size(rvX) <= 0 .or. iNumValid <= 0) then
			allocate(rvValidX(0))
			return
		end if

		! Loop over data, copying valids only to the new vector
		allocate(rvValidX(iNumValid))
		j = 0
		do i = 1, size(rvX)
			if(.not.ieee_is_nan(rvX(i))) then
				j = j + 1
				rvValidX(j) = rvX(i)
			end if
		end do

	end function GetValidOnly8


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
		n = count(.not.ieee_is_nan(rvX))
		if(n > 0) then
			rMean = sum(rvX, mask=.not.ieee_is_nan(rvX)) / n
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
		n = count(.not.ieee_is_nan(rvX))
		if(present(rMeanIn)) then
			rMean = rMeanIn
		else
			if(n > 0) then
				rMean = sum(rvX, mask=.not.ieee_is_nan(rvX)) / n
			else
				rMean = NaN
			end if
		end if

		! Compute the standard deviation
		if(n > 0) then
			rStdDev = sqrt(sum((rvX - rMean)**2, mask=.not.ieee_is_nan(rvX)) / n)
		else
			rStdDev = NaN
		end if

		! Compute diagnostic quantities, if present
		if(present(rValidFraction)) then
			rValidFraction = float(n) / size(rvX)
		end if

	end function StdDev


	! Compute the population skewness of a signal
	function Skew(rvX, rMeanIn, rStdDevIn, rValidFraction) result(rSkewness)

		! Routine arguments
		real, dimension(:), intent(in)	:: rvX				! Signal, whose skewness is needed
		real, intent(in), optional		:: rMeanIn			! Mean value, as computed by "Mean" function (optional, recomputed if missing)
		real, intent(in), optional		:: rStdDevIn		! Standard deviation, as computed by "StdDev" function (optional, recomputed if missing)
		real, intent(out), optional		:: rValidFraction	! Fraction of valid to total signal data (optional)
		real							:: rSkewness

		! Locals
		integer	:: n
		real	:: rMean
		real	:: rStdDev
		real	:: m3

		! Check something is to be made
		if(size(rvX) <= 0) then
			rMean = NaN
			return
		end if

		! Compute the arithmetic mean, if missing; or, get its value
		n = count(.valid.rvX)
		if(present(rMeanIn)) then
			rMean = rMeanIn
		else
			if(n > 0) then
				rMean = sum(rvX, mask = .valid.rvX) / n
			else
				rMean = NaN
			end if
		end if
		if(present(rStdDevIn)) then
			rStdDev = rStdDevIn
		else
			if(n > 0) then
				rStdDev = sqrt(sum((rvX - rMean)**2, mask = .valid.rvX) / n)
			else
				rStdDev = NaN
			end if
		end if

		! Compute the skewness
		if(n > 0) then
			m3        = sum((rvX - rMean)**3, mask = .valid.rvX) / n
			rSkewness = m3 / rStdDev**3
		else
			rSkewness = NaN
		end if

		! Compute diagnostic quantities, if present
		if(present(rValidFraction)) then
			rValidFraction = float(n) / size(rvX)
		end if

	end function Skew


	! Compute the population kurtosis of a signal
	function Kurt(rvX, rMeanIn, rStdDevIn, rValidFraction) result(rKurtosis)

		! Routine arguments
		real, dimension(:), intent(in)	:: rvX				! Signal, whose kurtosis is needed
		real, intent(in), optional		:: rMeanIn			! Mean value, as computed by "Mean" function (optional, recomputed if missing)
		real, intent(in), optional		:: rStdDevIn		! Standard deviation, as computed by "StdDev" function (optional, recomputed if missing)
		real, intent(out), optional		:: rValidFraction	! Fraction of valid to total signal data (optional)
		real							:: rKurtosis

		! Locals
		integer	:: n
		real	:: rMean
		real	:: rStdDev
		real	:: m4

		! Check something is to be made
		if(size(rvX) <= 0) then
			rMean = NaN
			return
		end if

		! Compute the arithmetic mean, if missing; or, get its value
		n = count(.valid.rvX)
		if(present(rMeanIn)) then
			rMean = rMeanIn
		else
			if(n > 0) then
				rMean = sum(rvX, mask = .valid.rvX) / n
			else
				rMean = NaN
			end if
		end if
		if(present(rStdDevIn)) then
			rStdDev = rStdDevIn
		else
			if(n > 0) then
				rStdDev = sqrt(sum((rvX - rMean)**2, mask = .valid.rvX) / n)
			else
				rStdDev = NaN
			end if
		end if

		! Compute the skewness
		if(n > 0) then
			m4        = sum((rvX - rMean)**4, mask = .valid.rvX) / n
			rKurtosis = m4 / rStdDev**4 - 3.
		else
			rKurtosis = NaN
		end if

		! Compute diagnostic quantities, if present
		if(present(rValidFraction)) then
			rValidFraction = float(n) / size(rvX)
		end if

	end function Kurt


	! FB validation index
	function FB_4(rvO, rvP) result(rFB)

		! Routine arguments
		real, dimension(:), intent(in)								:: rvO
		real, dimension(:), intent(in)								:: rvP
		real														:: rFB

		! Locals
        integer :: n
        integer :: i
		real	:: rBase
		real	:: rSums
		real	:: rDifferences
		real, dimension(:), allocatable	:: rvX
		real, dimension(:), allocatable	:: rvY

        ! Check it makes sense to proceed
        if(size(rvO) /= size(rvP)) then
        	rFB = NaN
        	return
        end if
        n = 0
        do i = 1, size(rvO)
        	if((.valid.rvO(i)) .and. (.valid.rvP(i))) n = n + 1
        end do
        if(n <= 1) then
            rFB = NaN
            return
        end if

		! Scale values to ensure positivity (validation indices apply to positive values)
		rBase = huge(rBase)
		do i = 1, size(rvO)
        	if((.valid.rvO(i)) .and. (.valid.rvP(i))) then
				rBase = min(rvO(i), rvP(i), rBase)
			end if
		end do
		if(rBase > 0.5 * huge(rBase)) then
			rFB = NaN
			return
		end if
		rBase = rBase + 1.
		allocate(rvX(size(rvO)))
		allocate(rvY(size(rvP)))
		rvX = rvO + rBase
		rvY = rvP + rBase

		! Compute accumulators
		rDifferences = 0.
		rSums        = 0.
        do i = 1, size(rvX)
        	if((.valid.rvX(i)) .and. (.valid.rvY(i))) then
				rDifferences = rDifferences + (rvX(i) - rvY(i))
				rSums        = rSums        + (rvX(i) + rvY(i))
			end if
        end do

		! Convert counts to FAC2
		rFB = rDifferences / (0.5 * rSums)

	end function FB_4


	! FB validation index
	function FB_8(rvO, rvP) result(rFB)

		! Routine arguments
		real(8), dimension(:), intent(in)								:: rvO
		real(8), dimension(:), intent(in)								:: rvP
		real(8)															:: rFB

		! Locals
        integer :: n
        integer :: i
		real(8)	:: rBase
		real(8)	:: rSums
		real(8)	:: rDifferences
		real(8), dimension(:), allocatable	:: rvX
		real(8), dimension(:), allocatable	:: rvY

        ! Check it makes sense to proceed
        if(size(rvO) /= size(rvP)) then
        	rFB = NaN_8
        	return
        end if
        n = 0
        do i = 1, size(rvO)
        	if((.valid.rvO(i)) .and. (.valid.rvP(i))) n = n + 1
        end do
        if(n <= 1) then
            rFB = NaN_8
            return
        end if

		! Scale values to ensure positivity (validation indices apply to positive values)
		rBase = huge(rBase)
		do i = 1, size(rvO)
        	if((.valid.rvO(i)) .and. (.valid.rvP(i))) then
				rBase = min(rvO(i), rvP(i), rBase)
			end if
		end do
		if(rBase > 0.5d0 * huge(rBase)) then
			rFB = NaN_8
			return
		end if
		rBase = rBase + 1.d0
		allocate(rvX(size(rvO)))
		allocate(rvY(size(rvP)))
		rvX = rvO + rBase
		rvY = rvP + rBase

		! Compute accumulators
		rDifferences = 0.d0
		rSums        = 0.d0
        do i = 1, size(rvX)
        	if((.valid.rvX(i)) .and. (.valid.rvY(i))) then
				rDifferences = rDifferences + (rvX(i) - rvY(i))
				rSums        = rSums        + (rvX(i) + rvY(i))
			end if
        end do

		! Convert counts to FB
		rFB = rDifferences / (0.5d0 * rSums)

	end function FB_8


	! FAC2 validation index
	function FAC2_4(rvO, rvP, rFactorIn, lvIncluded) result(rFAC2)

		! Routine arguments
		real, dimension(:), intent(in)								:: rvO
		real, dimension(:), intent(in)								:: rvP
		real, intent(in), optional									:: rFactorIn
		logical, dimension(:), allocatable, intent(out), optional	:: lvIncluded
		real														:: rFAC2

		! Locals
        integer :: m
        integer :: n
        integer :: i
		real	:: rFactorMin
		real	:: rFactorMax
		real	:: rBase
		real, dimension(:), allocatable	:: rvX
		real, dimension(:), allocatable	:: rvY

        ! Check it makes sense to proceed
        if(size(rvO) /= size(rvP)) then
        	rFAC2 = NaN
        	return
        end if
        n = 0
        do i = 1, size(rvO)
        	if((.valid.rvO(i)) .and. (.valid.rvP(i))) n = n + 1
        end do
        if(n <= 1) then
            rFAC2 = NaN
            return
        end if

		! Set factors
		if(present(rFactorIn)) then
			if(rFactorIn <= 0.) then
				rFAC2 = NaN
				return
			else
				if(rFactorIn <= 1.) then
					rFactorMin = rFactorIn
					rFactorMax = 1. / rFactorIn
				else
					rFactorMin = 1. / rFactorIn
					rFactorMax = rFactorIn
				end if
			end if
		else
			rFactorMin = 0.5
			rFactorMax = 2.0
		end if

		! Scale values to ensure positivity (validation indices apply to positive values)
		rBase = huge(rBase)
		do i = 1, size(rvO)
        	if((.valid.rvO(i)) .and. (.valid.rvP(i))) then
				rBase = min(rvO(i), rvP(i), rBase)
			end if
		end do
		if(rBase > 0.5 * huge(rBase)) then
			rFAC2 = NaN
			return
		end if
		rBase = rBase + 1.
		allocate(rvX(size(rvO)))
		allocate(rvY(size(rvP)))
		rvX = rvO + rBase
		rvY = rvP + rBase

		! Compute accumulators
		if(allocated(lvIncluded)) deallocate(lvIncluded)
		allocate(lvIncluded(size(rvO)))
		m = 0
		n = 0
        do i = 1, size(rvX)
        	if((.valid.rvX(i)) .and. (.valid.rvY(i))) then
				m = m + 1
				if(rFactorMin * rvX(i) <= rvY(i) .and. rvY(i) <= rFactorMax * rvX(i)) then
					n = n + 1
					if(present(lvIncluded)) then
						lvIncluded(i) = .true.
					end if
				else
					if(present(lvIncluded)) then
						lvIncluded(i) = .false.
					end if
				end if
			else
				if(present(lvIncluded)) then
					lvIncluded(i) = .false.
				end if
			end if
        end do

		! Convert counts to FAC2
		rFAC2 = real(n, kind=4) / real(m, kind=8)

	end function FAC2_4


	! FAC2 validation index
	function FAC2_8(rvO, rvP, rFactorIn, lvIncluded) result(rFAC2)

		! Routine arguments
		real(8), dimension(:), intent(in)								:: rvO
		real(8), dimension(:), intent(in)								:: rvP
		real(8), intent(in), optional									:: rFactorIn
		logical, dimension(:), allocatable, intent(out), optional		:: lvIncluded
		real(8)															:: rFAC2

		! Locals
        integer :: m
        integer :: n
        integer :: i
		real(8)	:: rFactorMin
		real(8)	:: rFactorMax
		real(8)	:: rBase
		real(8), dimension(:), allocatable	:: rvX
		real(8), dimension(:), allocatable	:: rvY

        ! Check it makes sense to proceed
        if(size(rvO) /= size(rvP)) then
        	rFAC2 = NaN_8
        	return
        end if
        n = 0
        do i = 1, size(rvO)
        	if((.valid.rvO(i)) .and. (.valid.rvP(i))) n = n + 1
        end do
        if(n <= 1) then
            rFAC2 = NaN_8
            return
        end if

		! Set factors
		if(present(rFactorIn)) then
			if(rFactorIn <= 0.d0) then
				rFAC2 = NaN_8
				return
			else
				if(rFactorIn <= 1.d0) then
					rFactorMin = rFactorIn
					rFactorMax = 1.d0 / rFactorIn
				else
					rFactorMin = 1.d0 / rFactorIn
					rFactorMax = rFactorIn
				end if
			end if
		else
			rFactorMin = 0.5d0
			rFactorMax = 2.0d0
		end if

		! Scale values to ensure positivity (validation indices apply to positive values)
		rBase = huge(rBase)
		do i = 1, size(rvO)
        	if((.valid.rvO(i)) .and. (.valid.rvP(i))) then
				rBase = min(rvO(i), rvP(i), rBase)
			end if
		end do
		if(rBase > 0.5d0 * huge(rBase)) then
			rFAC2 = NaN_8
			return
		end if
		rBase = rBase + 1.d0
		allocate(rvX(size(rvO)))
		allocate(rvY(size(rvP)))
		rvX = rvO + rBase
		rvY = rvP + rBase

		! Compute accumulators
		if(allocated(lvIncluded)) deallocate(lvIncluded)
		allocate(lvIncluded(size(rvO)))
		m = 0
		n = 0
        do i = 1, size(rvX)
        	if((.valid.rvX(i)) .and. (.valid.rvY(i))) then
				m = m + 1
				if(rFactorMin * rvX(i) <= rvY(i) .and. rvY(i) <= rFactorMax * rvX(i)) then
					n = n + 1
					if(present(lvIncluded)) then
						lvIncluded(i) = .true.
					end if
				else
					if(present(lvIncluded)) then
						lvIncluded(i) = .false.
					end if
				end if
			else
				if(present(lvIncluded)) then
					lvIncluded(i) = .false.
				end if
			end if
        end do

		! Convert counts to FAC2
		rFAC2 = real(n, kind=8) / real(m, kind=8)

	end function FAC2_8


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


	function QuantileScalar(rvX, rQuantile, iType) result(rQvalue)

		! Routine argument
		real, dimension(:), intent(in)	:: rvX			! Data vector
		real, intent(in)				:: rQuantile	! Quantile fraction (in [0.,1.] interval, inclusive)
		integer, intent(in), optional	:: iType		! Quantile type (QUANT_POPULATION, QUANT_1, ..., QUANT_9; see constant declaration for meaning)
		real							:: rQvalue		! Quantile value

		! Locals
		real, dimension(:), allocatable	:: rvXsorted
		integer							:: iQuantileType
		real							:: h
		real							:: m
		integer							:: n
		real							:: p
		integer							:: j
		real							:: g
		real							:: gamma

		! Check something is to be made
		if(size(rvX) == 1) then
			rQvalue = rvX(1)
			return
		elseif(size(rvX) < 1) then
			rQvalue = NaN
			return
		end if
		if(all(.invalid.rvX)) then
			rQvalue = NaN
			return
		end if
		if(.invalid.rQuantile) then
			rQvalue = NaN
			return
		end if

		! Answer for trivial cases
		if(rQuantile <= 0.) then
			rQvalue = minval(rvX, mask=.valid.rvX)
			return
		elseif(rQuantile >= 1.) then
			rQvalue = maxval(rvX, mask=.valid.rvX)
			return
		end if

		! Contract data vector to valid data only, and sort it
		rvXsorted = GetValidOnly(rvX)
		if(size(rvXsorted) == 1) then
			rQvalue = rvXsorted(1)
			return
		elseif(size(rvXsorted) < 1) then
			rQvalue = NaN
			return
		end if
		call quicksort(rvXsorted)

		! Assign actual quantile type
		if(present(iType)) then
			iQuantileType = iType
			if(iQuantileType == QUANT_POPULATION .and. size(rvXsorted) < size(rvx)) iQuantileType = QUANT_8
		else
			iQuantileType = QUANT_8
		end if

		! Compute the quantile value
		n = size(rvXsorted)
		p = rQuantile

		select case(iQuantileType)
		case(QUANT_POPULATION)
			h = n * p
			if(floor(h) == ceiling(h)) then
				! h is integer
				j = floor(h)
				if(j < 1) then
					rQvalue = rvXsorted(1)
				elseif(j >= n) then
					rQvalue = rvXsorted(n)
				else
					rQvalue = 0.5*(rvXsorted(j) + rvXsorted(j + 1))
				end if
			else
				! h is not integer
				j = ceiling(h)
				if(j < 1) then
					rQvalue = rvXsorted(1)
				elseif(j >= n) then
					rQvalue = rvXsorted(n)
				else
					rQvalue = rvXsorted(j)
				end if
			end if
		case(QUANT_1)
			h = n * p
			j = ceiling(h)
			if(j < 1) then
				rQvalue = rvXsorted(1)
			elseif(j > n) then
				rQvalue = rvXsorted(n)
			else
				rQvalue = rvXsorted(ceiling(h))
			end if
		case(QUANT_2)
			m = 0.
			j = floor(n*p + m)
			if(j >= 1 .and. j < n) then
				g = n*p + m - j
				if(g>1.e-6) then
					gamma = 1.
				else
					gamma = 0.5
				end if
				rQvalue = (1.-gamma)*rvXsorted(j) + gamma*rvXsorted(j+1)
			elseif(j < 1) then
				rQvalue = rvXsorted(1)
			else
				rQvalue = rvXsorted(n)
			end if
		case(QUANT_3)
			j = nint(n * p)
			if(j < 1) then
				rQvalue = rvXsorted(1)
			elseif(j > n) then
				rQvalue = rvXsorted(n)
			else
				rQvalue = rvXsorted(j)
			end if
		case(QUANT_3_SAS)
			m = -0.5
			j = floor(n*p + m)
			if(j >= 1 .and. j < n) then
				g = n*p + m - j
				if(g<1.e-6 .and. mod(j,2)==0) then
					gamma = 1.
				else
					gamma = 0.
				end if
				rQvalue = (1.-gamma)*rvXsorted(j) + gamma*rvXsorted(j+1)
			elseif(j < 1) then
				rQvalue = rvXsorted(1)
			else
				rQvalue = rvXsorted(n)
			end if
		case(QUANT_4)
			m = 0.
			j = floor(n*p + m)
			if(j >= 1 .and. j < n) then
				g = n*p + m - j
				gamma = g
				rQvalue = (1.-gamma)*rvXsorted(j) + gamma*rvXsorted(j+1)
			elseif(j < 1) then
				rQvalue = rvXsorted(1)
			else
				rQvalue = rvXsorted(n)
			end if
		case(QUANT_5)
			m = 1./2.
			j = floor(n*p + m)
			if(j >= 1 .and. j < n) then
				g = n*p + m - j
				gamma = g
				rQvalue = (1.-gamma)*rvXsorted(j) + gamma*rvXsorted(j+1)
			elseif(j < 1) then
				rQvalue = rvXsorted(1)
			else
				rQvalue = rvXsorted(n)
			end if
		case(QUANT_6)
			m = 0.
			j = floor((n+1)*p + m)
			if(j >= 1 .and. j < n) then
				g = (n+1)*p + m - j
				gamma = g
				rQvalue = (1.-gamma)*rvXsorted(j) + gamma*rvXsorted(j+1)
			elseif(j < 1) then
				rQvalue = rvXsorted(1)
			else
				rQvalue = rvXsorted(n)
			end if
		case(QUANT_7)
			m = 1.
			j = floor((n-1)*p + m)
			if(j >= 1 .and. j < n) then
				g = (n-1)*p + m - j
				gamma = g
				rQvalue = (1.-gamma)*rvXsorted(j) + gamma*rvXsorted(j+1)
			elseif(j < 1) then
				rQvalue = rvXsorted(1)
			else
				rQvalue = rvXsorted(n)
			end if
		case(QUANT_8)
			m = 1./3.
			j = floor((n+1./3.)*p + m)
			if(j >= 1 .and. j < n) then
				g = (n+1./3.)*p + m - j
				gamma = g
				rQvalue = (1.-gamma)*rvXsorted(j) + gamma*rvXsorted(j+1)
			elseif(j < 1) then
				rQvalue = rvXsorted(1)
			else
				rQvalue = rvXsorted(n)
			end if
		case(QUANT_9)
			m = 3./8.
			j = floor((n+1./4.)*p + m)
			if(j >= 1 .and. j < n) then
				g = (n+1./4.)*p + m - j
				gamma = g
				rQvalue = (1.-gamma)*rvXsorted(j) + gamma*rvXsorted(j+1)
			elseif(j < 1) then
				rQvalue = rvXsorted(1)
			else
				rQvalue = rvXsorted(n)
			end if
		case default
			rQvalue = NaN
		end select

	end function QuantileScalar


	function QuantileVector(rvX, rvQuantile, iType) result(rvQvalue)

		! Routine argument
		real, dimension(:), intent(in)		:: rvX			! Data vector
		real, dimension(:), intent(in)		:: rvQuantile	! Quantile fraction (in [0.,1.] interval, inclusive)
		integer, intent(in), optional		:: iType		! Quantile type (QUANT_POPULATION, QUANT_1, ..., QUANT_9; see constant declaration for meaning)
		real, dimension(size(rvQuantile))	:: rvQvalue		! Quantile value

		! Locals
		real, dimension(:), allocatable	:: rvXsorted
		integer							:: iQuantileType
		real							:: h
		integer							:: iQuantile
		real							:: m
		integer							:: n
		real							:: p
		integer							:: j
		real							:: g
		real							:: gamma

		! Check something is to be made
		if(size(rvQuantile) <= 0) then
			return	! No defined return value can be assigned here - rvQvalue does not exist
		end if
		if(size(rvX) == 1) then
			rvQvalue = rvX(1)
			return
		elseif(size(rvX) < 1) then
			rvQvalue = NaN
			return
		end if
		if(all(.invalid.rvX)) then
			rvQvalue = NaN
			return
		end if

		! Contract data vector to valid data only, and sort it
		rvXsorted = GetValidOnly(rvX)
		if(size(rvXsorted) == 1) then
			rvQvalue = rvXsorted(1)
			return
		elseif(size(rvXsorted) < 1) then
			rvQvalue = NaN
			return
		end if
		call quicksort(rvXsorted)

		! Assign actual quantile type
		if(present(iType)) then
			iQuantileType = iType
			if(iQuantileType == QUANT_POPULATION .and. size(rvXsorted) < size(rvX)) iQuantileType = QUANT_8
		else
			iQuantileType = QUANT_8
		end if

		! Main loop: iterate over quantiles
		do iQuantile = 1, size(rvQuantile)

			! Check something is to be made
			if(.invalid.rvQuantile(iQuantile)) then
				rvQvalue(iQuantile) = NaN
				cycle
			end if

			! Answer for trivial cases
			if(rvQuantile(iQuantile) <= 0.) then
				rvQvalue(iQuantile) = minval(rvX, mask=.valid.rvX)
				cycle
			elseif(rvQuantile(iQuantile) >= 1.) then
				rvQvalue(iQuantile) = maxval(rvX, mask=.valid.rvX)
				cycle
			end if

			! Compute the quantile value
			n = size(rvXsorted)
			p = rvQuantile(iQuantile)

			! Compute the value of h
			select case(iQuantileType)
			case(QUANT_POPULATION)
				h = n * p
				if(floor(h) == ceiling(h)) then
					! h is integer
					j = floor(h)
					if(j < 1) then
						rvQvalue(iQuantile) = rvXsorted(1)
					elseif(j >= n) then
						rvQvalue(iQuantile) = rvXsorted(n)
					else
						rvQvalue(iQuantile) = 0.5*(rvXsorted(j) + rvXsorted(j + 1))
					end if
				else
					! h is not integer
					j = ceiling(h)
					if(j < 1) then
						rvQvalue(iQuantile) = rvXsorted(1)
					elseif(j >= n) then
						rvQvalue(iQuantile) = rvXsorted(n)
					else
						rvQvalue(iQuantile) = rvXsorted(j)
					end if
				end if
			case(QUANT_1)
				h = n * p
				j = ceiling(h)
				if(j < 1) then
					rvQvalue(iQuantile) = rvXsorted(1)
				elseif(j > n) then
					rvQvalue(iQuantile) = rvXsorted(n)
				else
					rvQvalue(iQuantile) = rvXsorted(j)
				end if
			case(QUANT_2)
				m = 0.
				j = floor(n*p + m)
				if(j >= 1 .and. j < n) then
					g = n*p + m - j
					if(g>1.e-6) then
						gamma = 1.
					else
						gamma = 0.5
					end if
					rvQvalue(iQuantile) = (1.-gamma)*rvXsorted(j) + gamma*rvXsorted(j+1)
				elseif(j < 1) then
					rvQvalue(iQuantile) = rvXsorted(1)
				else
					rvQvalue(iQuantile) = rvXsorted(n)
				end if
			case(QUANT_3)
				j = nint(n * p)
				if(j < 1) then
					rvQvalue(iQuantile) = rvXsorted(1)
				elseif(j > n) then
					rvQvalue(iQuantile) = rvXsorted(n)
				else
					rvQvalue(iQuantile) = rvXsorted(j)
				end if
			case(QUANT_3_SAS)
				m = -0.5
				j = floor(n*p + m)
				if(j >= 1 .and. j < n) then
					g = n*p + m - j
					if(g<1.e-6 .and. mod(j,2)==0) then
						gamma = 1.
					else
						gamma = 0.
					end if
					rvQvalue(iQuantile) = (1.-gamma)*rvXsorted(j) + gamma*rvXsorted(j+1)
				elseif(j < 1) then
					rvQvalue(iQuantile) = rvXsorted(1)
				else
					rvQvalue(iQuantile) = rvXsorted(n)
				end if
			case(QUANT_4)
				m = 0.
				j = floor(n*p + m)
				if(j >= 1 .and. j < n) then
					g = n*p + m - j
					gamma = g
					rvQvalue(iQuantile) = (1.-gamma)*rvXsorted(j) + gamma*rvXsorted(j+1)
				elseif(j < 1) then
					rvQvalue(iQuantile) = rvXsorted(1)
				else
					rvQvalue(iQuantile) = rvXsorted(n)
				end if
				if(rvQuantile(iQuantile) < 1./size(rvXsorted)) then
					rvQvalue(iQuantile) = rvXsorted(1)
				else
					h = size(rvXsorted) * rvQuantile(iQuantile)
					rvQvalue(iQuantile) = rvXsorted(floor(h)) + (h - floor(h))*(rvXsorted(floor(h)+1) - rvXsorted(floor(h)))
				end if
			case(QUANT_5)
				m = 1./2.
				j = floor(n*p + m)
				if(j >= 1 .and. j < n) then
					g = n*p + m - j
					gamma = g
					rvQvalue(iQuantile) = (1.-gamma)*rvXsorted(j) + gamma*rvXsorted(j+1)
				elseif(j < 1) then
					rvQvalue(iQuantile) = rvXsorted(1)
				else
					rvQvalue(iQuantile) = rvXsorted(n)
				end if
			case(QUANT_6)
				m = 0.
				j = floor((n+1)*p + m)
				if(j >= 1 .and. j < n) then
					g = (n+1)*p + m - j
					gamma = g
					rvQvalue(iQuantile) = (1.-gamma)*rvXsorted(j) + gamma*rvXsorted(j+1)
				elseif(j < 1) then
					rvQvalue(iQuantile) = rvXsorted(1)
				else
					rvQvalue(iQuantile) = rvXsorted(n)
				end if
			case(QUANT_7)
				m = 1.
				j = floor((n-1)*p + m)
				if(j >= 1 .and. j < n) then
					g = (n-1)*p + m - j
					gamma = g
					rvQvalue(iQuantile) = (1.-gamma)*rvXsorted(j) + gamma*rvXsorted(j+1)
				elseif(j < 1) then
					rvQvalue(iQuantile) = rvXsorted(1)
				else
					rvQvalue(iQuantile) = rvXsorted(n)
				end if
			case(QUANT_8)
				m = 1./3.
				j = floor((n+1./3.)*p + m)
				if(j >= 1 .and. j < n) then
					g = (n+1./3.)*p + m - j
					gamma = g
					rvQvalue(iQuantile) = (1.-gamma)*rvXsorted(j) + gamma*rvXsorted(j+1)
				elseif(j < 1) then
					rvQvalue(iQuantile) = rvXsorted(1)
				else
					rvQvalue(iQuantile) = rvXsorted(n)
				end if
			case(QUANT_9)
				m = 3./8.
				j = floor((n+1./4.)*p + m)
				if(j >= 1 .and. j < n) then
					g = (n+1./4.)*p + m - j
					gamma = g
					rvQvalue(iQuantile) = (1.-gamma)*rvXsorted(j) + gamma*rvXsorted(j+1)
				elseif(j < 1) then
					rvQvalue(iQuantile) = rvXsorted(1)
				else
					rvQvalue(iQuantile) = rvXsorted(n)
				end if
			case default
				rvQvalue(iQuantile) = NaN
			end select

		end do

	end function QuantileVector


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
					if((.valid.rvX(i)) .and. (.valid.rvX(i+iLag))) then
						iNum   = iNum + 1
						rSumA  = rSumA + rvX(i)
						rSumB  = rSumB + rvX(i+iLag)
					end if
				end do

				! Compute autocovariance
				if(iNum > 0) then
					rSumAB = 0.d0
					do i = 1, n - iLag
						if((.valid.rvX(i)) .and. (.valid.rvX(i+iLag))) then
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
					if((.valid.rvX(i)) .and. (.valid.rvX(i+iLag))) then
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


	! Compute the simple regression.

	function SimpleLinearRegression4(rvX, rvY, rMultiplier, rOffset, rvEstimatedY) result(iRetCode)

		! Routine argument
		real, dimension(:), intent(in)							:: rvX			! Index signal (typically time, in floating point form)
		real, dimension(:), intent(in)							:: rvY			! Experimental values to regress on
		real, intent(out)										:: rMultiplier	! Multiplier of trend line
		real, intent(out)										:: rOffset		! Offset of trend line
		real, dimension(:), allocatable, optional, intent(out)	:: rvEstimatedY	! Estimated signal
		integer													:: iRetCode

		! Locals
		integer	:: n
		real	:: rSx
		real	:: rSy
		real	:: rSxx
		real	:: rSxy
		real	:: rDelta

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
		rSxy = dot_product(rvX,rvY)

		! Compute multiplier and offset
		rDelta      = n*rSxx - rSx**2
		if(rDelta <= 0.d0) then
			iRetCode = 4
			return
		end if
		rOffset     = (rSxx*rSy - rSx*rSxy)/rDelta
		rMultiplier = (n*rSxy - rSx*rSy)/rDelta

		! Estimate data
		if(present(rvEstimatedY)) then
			if(allocated(rvEstimatedY)) deallocate(rvEstimatedY)
			allocate(rvEstimatedY(size(rvX)))
			rvEstimatedY = rMultiplier * rvX + rOffset
		end if

	end function SimpleLinearRegression4

	function SimpleLinearRegression8(rvX, rvY, rMultiplier, rOffset, rvEstimatedY) result(iRetCode)

		! Routine argument
		real(8), dimension(:), intent(in)							:: rvX			! Index signal (typically time, in floating point form)
		real(8), dimension(:), intent(in)							:: rvY			! Experimental values to regress on
		real(8), intent(out)										:: rMultiplier	! Multiplier of trend line
		real(8), intent(out)										:: rOffset		! Offset of trend line
		real(8), dimension(:), allocatable, optional, intent(out)	:: rvEstimatedY	! Estimated signal
		integer														:: iRetCode

		! Locals
		integer	:: n
		real(8)	:: rSx
		real(8)	:: rSy
		real(8)	:: rSxx
		real(8)	:: rSxy
		real(8)	:: rDelta

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
		rSxy = dot_product(rvX,rvY)

		! Compute multiplier and offset
		rDelta      = n*rSxx - rSx**2
		if(rDelta <= 0.d0) then
			iRetCode = 4
			return
		end if
		rOffset     = (rSxx*rSy - rSx*rSxy)/rDelta
		rMultiplier = (n*rSxy - rSx*rSy)/rDelta

		! Estimate data
		if(present(rvEstimatedY)) then
			if(allocated(rvEstimatedY)) deallocate(rvEstimatedY)
			allocate(rvEstimatedY(size(rvX)))
			rvEstimatedY = rMultiplier * rvX + rOffset
		end if

	end function SimpleLinearRegression8

	! ********************************
	! * Members of Time<series class *
	! ********************************

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


	! Copy constructor, creates a duplicate of current time series
	function tsCreateFromTimeSeries(this, ts, lForceWellSpacedMonotonic) result(iRetCode)

		! Routine arguments
		class(TimeSeries), intent(out)		:: this
		type(TimeSeries), intent(in)		:: ts
		logical, intent(in), optional		:: lForceWellSpacedMonotonic	! If .false. (default) just copy the series as it is. If .true., rearrange the original series so that time stamps form a type-0 well spaced sequence
		integer								:: iRetCode

		! Locals
		integer	:: i
		integer	:: n
		integer	:: m
		integer	:: idx
		integer	:: iErrCode
		real(8)	:: rDeltaTime
		real(8)	:: rMinTimeStamp
		real(8)	:: rMaxTimeStamp
		integer	:: iWellSpaced
		real(8), dimension(:), allocatable	:: rvTimeStamp
		real(4), dimension(:), allocatable	:: rvValues
		logical								:: lTimeExpand

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Decide which type of processing to do
		if(present(lForceWellSpacedMonotonic)) then
			lTimeExpand = lForceWellSpacedMonotonic
		else
			lTimeExpand = .false.
		end if

		! Check there is something to copy (leave current series unchanged if not)
		n = ts % size()
		if(n <= 0) then
			iRetCode = 1
			return
		end if

		! Dispatch processing according to type
		if(allocated(this % rvTimeStamp)) deallocate(this % rvTimeStamp)
		if(allocated(this % rvValue)) deallocate(this % rvValue)
		if(lTimeExpand) then

			! Check conditions on time stamp (in particular, well-spacedness) to be
			! true enough for the expansion-while-copy to occur
			iWellSpaced = ts % timeIsWellSpaced(rDeltaTime)
			select case(iWellSpaced)

			case(0)	! Well-spaced, no gaps: just copy (reordering made)

				! Reserve workspace in copy, based on original
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
				iRetCode           = ts % getTimeStamp(rvTimeStamp)
				this % rvTimeStamp = rvTimeStamp
				iRetCode           = ts % getValues(rvValues)
				this % rvValue     = rvValues

				! Reorder with respect to time, to make sure of quasi-monotonicity (which becomes monotonicity, if
				! well-spacing with a positive delta time is guaranteed
				call this % timeReorder()

			case(1)	! Well-spaced, but with at least one gap: time-expand (incidentally, result is monotonic)

				! Make resulting series time-regular
				iRetCode = ts % getTimeStamp(rvTimeStamp)
				if(iRetCode /= 0) then
					iRetCode = 3
					return
				end if
				iRetCode = ts % getValues(rvValues)
				if(iRetCode /= 0) then
					iRetCode = 3
					return
				end if
				rMinTimeStamp = minval(rvTimeStamp, mask=.valid.rvTimeStamp)
				rMaxTimeStamp = maxval(rvTimeStamp, mask=.valid.rvTimeStamp)
				if((.invalid.rMinTimeStamp) .or. (.invalid.rMaxTimeStamp)) then
					iRetCode = 4
					return
				end if

				! Count time-expanded size, and reserve workspace based on it
				m = nint((rMaxTimeStamp - rMinTimeStamp) / rDeltaTime) + 1
				if(m <= 0) then
					iRetCode = 5
					return
				end if
				allocate(this % rvTimeStamp(m), stat = iErrCode)
				if(iErrCode /= 0) then
					iRetCode = 2
					return
				end if
				allocate(this % rvValue(m), stat = iErrCode)
				if(iErrCode /= 0) then
					deallocate(this % rvTimeStamp)
					iRetCode = 2
					return
				end if

				! Initialize value vector to invalid, so that any non-filled value will make self-evident as a gap
				this % rvValue = NaN

				! Transfer data by their time index
				do i = 1, n
					idx = nint((rvTimeStamp(i) - rMinTimeStamp) / rDeltaTime) + 1
					if(idx < 1 .or. idx > m) cycle
					this % rvValue(idx) = rvValues(i)
				end do

				! Build time stamp vector
				this % rvTimeStamp = [(rMinTimeStamp + rDeltaTime*(i-1), i = 1, m)]

			case default	! -1 and 2 cases: no time regularity, abandon match

				iRetCode = 4
				return

			end select

		else

			! Just-copy-it path

			! Reserve workspace in copy, based on original
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
			iRetCode           = ts % getTimeStamp(rvTimeStamp)
			this % rvTimeStamp = rvTimeStamp
			iRetCode           = ts % getValues(rvValues)
			this % rvValue     = rvValues

		end if

	end function tsCreateFromTimeSeries


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
				lIsEmpty = all(.invalid.this % rvTimeStamp) .or. all(.invalid.this % rvValue)
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


	! Shift time stamp values by a given time difference (useful for example when changing a posticipated
	! time stamp to an anticipated one
	subroutine tsTimeShift(this, deltaTime)

		! Routine arguments
		class(TimeSeries), intent(inout)	:: this
		real(8), intent(in)					:: deltaTime

		! Locals
		! --none--

		! Apply shift operator in place
		this % rvTimeStamp = this % rvTimeStamp + deltaTime

	end subroutine tsTimeShift


	! Reorder time stamp increasing, sorting values in the meanwhile
	subroutine tsTimeReorder(this)

		! Routine arguments
		class(TimeSeries), intent(inout)	:: this

		! Locals
		integer								:: n, i
		integer, dimension(:), allocatable	:: ivIdx
		real, dimension(:), allocatable		:: rvValue2

		! Check something is to be made
		n = size(this % rvTimeStamp)
		if(n <= 1) return	! Do nothing for empty time stamp vector

		! Reindex time stamp vector while sorting it
		allocate(ivIdx(n), rvValue2(n))
		ivIdx = [(i, i=1, n)]
		call quicksort_idx_8(this % rvTimeStamp, ivIdx)
		do i = 1, n
			rvValue2(i) = this % rvValue(ivIdx(i))
		end do
		this % rvValue = rvValue2
		deallocate(ivIdx, rvValue2)

	end subroutine tsTimeReorder


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
		if(size(this % rvTimeStamp) <= 0) then
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


	function tsGetTimeStamp(this, rvTimeStamp) result(iRetCode)

		! Routine arguments
		class(TimeSeries), intent(in)					:: this
		real(8), dimension(:), allocatable, intent(out)	:: rvTimeStamp
		integer											:: iRetCode

		! Locals
		integer	:: n

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Check something is to be made
		n = size(this % rvTimeStamp)
		if(n <= 0) then
			iRetCode = 1
			return
		end if

		! Reserve workspace
		if(allocated(rvTimeStamp)) deallocate(rvTimeStamp)
		allocate(rvTimeStamp(n))

		! Transfer values
		rvTimeStamp = this % rvTimeStamp

	end function tsGetTimeStamp


	function tsGetValues(this, rvValues) result(iRetCode)

		! Routine arguments
		class(TimeSeries), intent(in)					:: this
		real(4), dimension(:), allocatable, intent(out)	:: rvValues
		integer											:: iRetCode

		! Locals
		integer	:: n

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Check something is to be made
		n = size(this % rvValue)
		if(n <= 0) then
			iRetCode = 1
			return
		end if

		! Reserve workspace
		if(allocated(rvValues)) deallocate(rvValues)
		allocate(rvValues(n))

		! Transfer values
		rvValues = this % rvValue

	end function tsGetValues


	function tsGetTimeSpan(this, rMinTimeStamp, rMaxTimeStamp) result(iRetCode)

		! Routine arguments
		class(TimeSeries), intent(in)	:: this
		real(8), intent(out)			:: rMinTimeStamp
		real(8), intent(out)			:: rMaxTimeStamp
		integer							:: iRetCode

		! Locals
		! - --none--

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Check parameters
		if(this % isEmpty()) then
			rMinTimeStamp = NaN_8
			rMaxTimeStamp = NaN_8
			iRetCode   = 1
			return
		end if

		! Compute time bounds, even if some invalid time stamps exist
		rMinTimeStamp = minval(this % rvTimeStamp, mask = .valid.this % rvTimeStamp)
		rMaxTimeStamp = maxval(this % rvTimeStamp, mask = .valid.this % rvTimeStamp)

	end function tsGetTimeSpan


	function tsGetTimeSubset(this, ts, timeFrom, timeTo) result(iRetCode)

		! Routine arguments
		class(TimeSeries), intent(out)		:: this
		type(TimeSeries), intent(in)		:: ts
		real(8), intent(in)					:: timeFrom
		real(8), intent(in)					:: timeTo
		integer								:: iRetCode

		! Locals
		integer	:: n, m
		integer	:: i, j
		integer	:: iErrCode
		real(8)	:: rMinTime
		real(8)	:: rMaxTime
		real(8), dimension(:), allocatable	:: rvTimeStamp
		real(4), dimension(:), allocatable	:: rvValues

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Reserve workspace in copy, based on original
		n = ts % size()
		if(n <= 0) then
			iRetCode = 1
			return
		end if

		! Fill with appropriate initial values
		iRetCode           = ts % getTimeStamp(rvTimeStamp)
		this % rvTimeStamp = rvTimeStamp
		iRetCode           = ts % getValues(rvValues)
		this % rvValue     = rvValues

		! Count subset size, and if zero return doing nothing
		rMinTime = min(timeFrom, timeTo)	! Just a safeguard
		rMaxTime = max(timeFrom, timeTo)	! Just a safeguard
		m = count(rvTimeStamp >= rMinTime .and. rvTimeStamp <= rMaxTime)
		if(m <= 0) then
			iRetCode = 2
			return
		end if

		! Reserve workspace
		if(allocated(this % rvTimeStamp)) deallocate(this % rvTimeStamp)
		if(allocated(this % rvValue)) deallocate(this % rvValue)
		allocate(this % rvTimeStamp(m), stat = iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 3
			return
		end if
		allocate(this % rvValue(m), stat = iErrCode)
		if(iErrCode /= 0) then
			deallocate(this % rvTimeStamp)
			iRetCode = 3
			return
		end if

		! Fill with data in time range, preserving their order
		j = 0
		do i = 1, n
			if(rvTimeStamp(i) >= rMinTime .and. rvTimeStamp(i) <= rMaxTime) then
				j = j + 1
				this % rvTimeStamp(j) = rvTimeStamp(i)
				this % rvValue(j)     = rvValues(i)
			end if
		end do

	end function tsGetTimeSubset


	function tsGetMonth(this, ts, iMonth) result(iRetCode)

		! Routine arguments
		class(TimeSeries), intent(out)		:: this
		type(TimeSeries), intent(in)		:: ts
		integer, intent(in)					:: iMonth
		integer								:: iRetCode

		! Locals
		integer	:: n, m
		integer	:: i, j
		integer	:: iErrCode
		real(8), dimension(:), allocatable	:: rvTimeStamp
		real(4), dimension(:), allocatable	:: rvValues
		integer, dimension(:), allocatable	:: ivMonth

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Reserve workspace in copy, based on original
		n = ts % size()
		if(n <= 0) then
			iRetCode = 1
			return
		end if

		! Check parameters
		if(iMonth < 1 .or. iMonth > 12) then
			iRetCode = 2
			return
		end if

		! Fill with appropriate initial values
		iRetCode           = ts % getTimeStamp(rvTimeStamp)
		this % rvTimeStamp = rvTimeStamp
		iRetCode           = ts % getValues(rvValues)
		this % rvValue     = rvValues
		iErrCode = timeGetMonth(rvTimeStamp, ivMonth)

		! Count subset size, and if zero return doing nothing
		m = count(ivMonth == iMonth)
		if(m <= 0) then
			iRetCode = 3
			return
		end if

		! Reserve workspace
		if(allocated(this % rvTimeStamp)) deallocate(this % rvTimeStamp)
		if(allocated(this % rvValue)) deallocate(this % rvValue)
		allocate(this % rvTimeStamp(m), stat = iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 4
			return
		end if
		allocate(this % rvValue(m), stat = iErrCode)
		if(iErrCode /= 0) then
			deallocate(this % rvTimeStamp)
			iRetCode = 5
			return
		end if

		! Fill with data in time range, preserving their order
		j = 0
		do i = 1, n
			if(ivMonth(i) == iMonth) then
				j = j + 1
				this % rvTimeStamp(j) = rvTimeStamp(i)
				this % rvValue(j)     = rvValues(i)
			end if
		end do

	end function tsGetMonth


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
		if(size(this % rvTimeStamp) <= 0) then
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


	function tsSize(this) result(iNumValues)

		! Routine arguments
		class(TimeSeries), intent(in)	:: this
		integer							:: iNumValues

		! Locals
		! --none--

		! Get the information desired
		if(this % isEmpty()) then
			iNumValues = 0
		else
			iNumValues = min(size(this % rvTimeStamp), size(this % rvValue))
		end if

	end function tsSize


	! Check the current time series has the same time stamps of another, in the very
	! same order. If the answer is .true., the two series are non-empty, their time stamps
	! are always valid, and may be thought as components of a larger multivariate series.
	function tsIsSameTimes(this, ts) result(lTimesAreSame)

		! Routine arguments
		class(TimeSeries), intent(in)	:: this
		type(TimeSeries), intent(in)	:: ts
		logical							:: lTimesAreSame

		! Locals
		integer								:: iErrCode
		integer								:: i
		real(8), dimension(:), allocatable	:: rvTimeStamp

		! Get the information desired
		if(this % isEmpty() .or. ts % isEmpty()) then
			lTimesAreSame = .false.
			return
		end if
		iErrCode = ts % getTimeStamp(rvTimeStamp)
		if(iErrCode /= 0) then
			lTimesAreSame = .false.
			if(allocated(rvTimeStamp)) deallocate(rvTimeStamp)
			return
		end if
		if(any(.invalid.rvTimeStamp) .or. any(.invalid.this % rvTimeStamp)) then
			lTimesAreSame = .false.
			if(allocated(rvTimeStamp)) deallocate(rvTimeStamp)
			return
		end if
		lTimesAreSame = .true.
		do i = 1, size(rvTimeStamp)
			lTimesAreSame = lTimesAreSame .and. (abs(rvTimeStamp(i) - this % rvTimeStamp(i)) < 4.*epsilon(rvTimeStamp(i)))
		end do

	end function tsIsSameTimes


	subroutine tsSummary(this, iNumValues, rValidPercentage, rMin, rMean, rStdDev, rMax, rSkew, rKurt)

		! Routine arguments
		class(TimeSeries), intent(in)	:: this
		integer, intent(out)			:: iNumValues
		real, intent(out)				:: rValidPercentage
		real, intent(out)				:: rMin
		real, intent(out)				:: rMean
		real, intent(out)				:: rStdDev
		real, intent(out)				:: rMax
		real, intent(out), optional		:: rSkew
		real, intent(out), optional		:: rKurt

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
			if(present(rSkew)) rSkew = NaN
			if(present(rKurt)) rKurt = NaN
		else
			iNumValues = size(this % rvValue)
			if(iNumValues > 0) then
				rValidPercentage = 100.0 * count(.valid. this % rvValue) / iNumValues
				rMin             = minval(this % rvValue, mask = .valid. this % rvValue)
				rMean            = sum(this % rvValue, mask = .valid. this % rvValue) / iNumValues
				rStdDev          = sqrt(sum((this % rvValue - rMean)**2, mask = .valid. this % rvValue) / iNumValues)
				rMax             = maxval(this % rvValue, mask = .valid. this % rvValue)
				if(present(rSkew)) rSkew = Skew(this % rvValue, rMeanIn=rMean, rStdDevIn=rStdDev)
				if(present(rKurt)) rKurt = Kurt(this % rvValue, rMeanIn=rMean, rStdDevIn=rStdDev)
			else
				rValidPercentage = 0.
				rMin             = NaN
				rMean            = NaN
				rStdDev          = NaN
				rMax             = NaN
				if(present(rSkew)) rSkew = NaN
				if(present(rKurt)) rKurt = NaN
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


	! Note: Time well-spacing implies strict monotonicity by construction
	function tsTimeWellSpaced(this, rTimeStep, iNumGaps) result(iWellSpacingType)

		! Routine arguments
		class(TimeSeries), intent(in)	:: this
		real(8), intent(out), optional	:: rTimeStep			! NaN in case of non-well-spaced data
		integer, intent(out), optional	:: iNumGaps				! -1 in case of non-well-spaced data
		integer							:: iWellSpacingType		! -1:well-spacing cannot be determined; 0:well-spaced, no gaps;
																! 1:well-spaced, with at least one gap;
		! Locals												! 2:not well-spaced (irregular time step)
		integer		:: i
		integer		:: n
		real(8)		:: rDelta
		real(8)		:: rQuotient
		real(8)		:: rMinDelta
		integer		:: iQuotient
		integer		:: iMaxQuotient
		integer		:: iNumGapsFound

		! Check parameters
		if(this % isEmpty()) then
			iWellSpacingType = -1
			if(present(rTimeStep)) rTimeStep = NaN_8
			if(present(iNumGaps))  iNumGaps  = -1
			return
		end if
		n = size(this % rvTimeStamp)
		if(n <= 1) then
			! Degenerate case: less than two data available, success assumed
			iWellSpacingType = 0
			if(present(rTimeStep)) rTimeStep = 0.d0
			if(present(iNumGaps))  iNumGaps  = 0
			return
		end if

		! First pass: find the minimum positive difference between any two consecutive time stamps
		! (zero differences are allowed to occur, due to coarse-grained resolution of some
		! data acquisition timing systems; an example of data sets for which zero time differences
		! are allowed to occur is the SonicLib format
		rMinDelta = huge(rMinDelta)
		do i = 2, n
			rDelta = this % rvTimeStamp(i) - this % rvTimeStamp(i-1)
			if(rDelta > 0.d0) rMinDelta = min(rDelta, rMinDelta)
		end do

		! Second pass: check all the positive time differences are integer multiples of the minimum
		! delta
		iNumGapsFound = 0
		do i = 2, n
			rDelta = this % rvTimeStamp(i) - this % rvTimeStamp(i-1)
			rQuotient = rDelta / rMinDelta
			iQuotient = floor(rQuotient)
			if(rQuotient - iQuotient <= 10.0*epsilon(rQuotient)) then
				iMaxQuotient = max(iQuotient, iMaxQuotient)
				if(iQuotient > 1) iNumGapsFound = iNumGapsFound + 1
			else
				iWellSpacingType = 2	! Not well-spaced
				if(present(rTimeStep)) rTimeStep = NaN_8
				if(present(iNumGaps))  iNumGaps  = -1
				return
			end if
		end do

		! Decide the final result based on counters evaluated so far
		if(iNumGapsFound > 0) then
			iWellSpacingType = 1	! Well-spaced, with one gap
			if(present(rTimeStep)) rTimeStep = rMinDelta
			if(present(iNumGaps))  iNumGaps  = iNumGapsFound
		else
			iWellSpacingType = 0	! Well-spaced, no time gaps (ideal condition)
			if(present(rTimeStep)) rTimeStep = rMinDelta
			if(present(iNumGaps))  iNumGaps  = 0
		end if

	end function tsTimeWellSpaced


	! Aggregate data of a time series according to a positive time difference,
	! or a negative code indicating time divisions like month and year.
	! Result is a time series, containing the aggregated values and time
	! stamps spaced according to the time difference selected.
	function tsAggregateLinear(this, iTimeDelta, iFunction, ts, ivNumDataOut) result(iRetCode)

		! Routine arguments
		class(TimeSeries), intent(in)					:: this
		integer, intent(in)								:: iTimeDelta	! A positive time difference, or TDELTA_YEARMONTH, or TDELTA_YEAR
		integer, intent(in), optional					:: iFunction	! Function code: FUN_MEAN (default), FUN_STDEV, FUN_MIN, FUN_MAX
		type(TimeSeries), intent(out)					:: ts			! The resulting time series
		integer, dimension(:), allocatable, optional	:: ivNumDataOut	! Number of valid data contributing to classes
		integer											:: iRetCode

		! Locals
		integer								:: iErrCode
		integer								:: n
		integer								:: m
		integer								:: i
		integer								:: j
		integer								:: iProcessing
		integer								:: iYear
		integer								:: iMonth
		integer								:: iMinTimeIndex
		type(DateTime)						:: tDateTime
		real(8), dimension(:), allocatable	:: rvTimeStamp
		integer, dimension(:), allocatable	:: ivTimeIndex
		real, dimension(:), allocatable		:: rvValue
		real(8), dimension(:), allocatable	:: rvTimeStamp_Reduced
		integer, dimension(:), allocatable	:: ivNumData
		real, dimension(:), allocatable		:: rvMin
		real, dimension(:), allocatable		:: rvMax
		real, dimension(:), allocatable		:: rvMean
		real, dimension(:), allocatable		:: rvStDev

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Check something is to be made
		if(this % isEmpty()) then
			iRetCode = 1
			return
		end if

		! Check delta time validity
		if(iTimeDelta == 0) then
			iRetCode = 2
		elseif(iTimeDelta < 0) then
			if(iTimeDelta /= TDELTA_YEARMONTH .and. iTimeDelta /= TDELTA_YEAR) then
				iRetCode = 2
			end if
		end if
		if(iRetCode /= 0) return

		! Retrieve time stamp and data vectors from original time series
		iErrCode = this % getTimeStamp(rvTimeStamp)
		if(iErrCode /= 0) then
			iRetCode = 3
			return
		end if
		iErrCode = this % getValues(rvValue)
		if(iErrCode /= 0) then
			iRetCode = 4
			deallocate(rvTimeStamp)
			return
		end if
		n = size(rvTimeStamp)

		! Index time, based on the desired time delta
		if(iTimeDelta > 0) then
			allocate(ivTimeIndex(size(rvTimeStamp)))
			where(.valid.rvTimeStamp)
				ivTimeIndex = floor(rvTimeStamp / iTimeDelta) + 1
			elsewhere
				ivTimeIndex = 0
			end where
		else
			select case(iTimeDelta)
			case(TDELTA_YEAR)
				iErrCode = timeGetYear(rvTimeStamp, ivTimeIndex)
			case(TDELTA_YEARMONTH)
				iErrCode = timeGetYearMonth(rvTimeStamp, ivTimeIndex)
			end select
			if(iErrCode /= 0) then
				iRetCode = 5
				deallocate(rvValue)
				deallocate(rvTimeStamp)
				return
			end if
		end if
		if(count(ivTimeIndex > 0) <= 0) then
			iRetCode = 6
			deallocate(rvValue)
			deallocate(rvTimeStamp)
			return
		end if

		! Count maximum index, and use it to reserve workspace
		iMinTimeIndex = minval(ivTimeIndex, mask = ivTimeIndex > 0)
		m = maxval(ivTimeIndex) - iMinTimeIndex + 1
		allocate(rvTimeStamp_Reduced(m), ivNumData(m), rvMin(m), rvMax(m), rvMean(m), rvStDev(m))

		! Change time indicator to a true, 1-based index
		do i = 1, n
			if(ivTimeIndex(i) > 0) then
				ivTimeIndex(i) = ivTimeIndex(i) - iMinTimeIndex + 1
			end if
		end do

		! Form time stamp for new time series; note: 2, not 1, is subtracted
		! from time index. This might sound counter-intuitive, but finds its motivation
		! in the fact that the time index is 1-based (so one 1 to subtract), and
		! j also is (another 1 to subtract). That's is...
		if(iTimeDelta > 0) then
			do j = 1, m
				rvTimeStamp_Reduced(j) = dble((iMinTimeIndex + j - 2)) * iTimeDelta
			end do
		else
			select case(iTimeDelta)
			case(TDELTA_YEAR)
				do j = 1, m
					iYear = iMinTimeIndex + j - 1
					tDateTime = DateTime(iYear, 1, 1, 0, 0, 0.d0)
					rvTimeStamp_Reduced(j) = tDateTime % toEpoch()
				end do
			case(TDELTA_YEARMONTH)
				do j = 1, m
					iMonth = mod(iMinTimeIndex + j - 1, 12) + 1
					iYear  = (iMinTimeIndex + j - 1) / 12
					tDateTime = DateTime(iYear, iMonth, 1, 0, 0, 0.d0)
					rvTimeStamp_Reduced(j) = tDateTime % toEpoch()
				end do
			end select
		end if

		! Update counts
		ivNumData =  0
		rvMin     =  huge(1.)
		rvMax     = -huge(1.)
		rvMean    =  0.
		rvStDev   =  0.
		do i = 1, n
			if((ivTimeIndex(i) > 0) .and. (.valid.rvTimeStamp(i)) .and. (.valid.rvValue(i))) then
				j = ivTimeIndex(i)
				ivNumData(j) = ivNumData(j) + 1
				rvMin(j)     = min(rvMin(j), rvValue(i))
				rvMax(j)     = max(rvMax(j), rvValue(i))
				rvMean(j)    = rvMean(j) + rvValue(i)
				rvStDev(j)   = rvStDev(j) + rvValue(i)**2
			end if
		end do

		! Transform mean and standard deviation counts in nominal quantities.
		! Here I use a little trick, based on non-signalling NaNs: rvMean is computed
		! by specifically discriminating between norman and invalid case. But StDev,
		! on the other side, is computed directly counting on the fact that non
		! signalling NaNs combine algebraically with valid values yielding NaNs
		! (because of IEEE rules).
		where(ivNumData > 0)
			rvMean = rvMean / ivNumData
		elsewhere
			rvMean = NaN
		end where
		rvStDev = sqrt(rvStDev/ivNumData - rvMean**2)

		! Make sure minima and maxima are NaN when class is empty
		where(ivNumData <= 0)
			rvMin = NaN
			rvMax = NaN
		end where

		! Of all quantities computed, transmit (horrible inefficiency) the one desired
		! to the resulting time series
		if(present(iFunction)) then
			iProcessing = iFunction
		else
			iProcessing = FUN_MEAN
		end if
		select case(iFunction)
		case(FUN_MEAN)
			iErrCode = ts % createFromTimeAndDataVectors(rvTimeStamp_Reduced, rvMean)
		case(FUN_STDEV)
			iErrCode = ts % createFromTimeAndDataVectors(rvTimeStamp_Reduced, rvStDev)
		case(FUN_MIN)
			iErrCode = ts % createFromTimeAndDataVectors(rvTimeStamp_Reduced, rvMin)
		case(FUN_MAX)
			iErrCode = ts % createFromTimeAndDataVectors(rvTimeStamp_Reduced, rvMax)
		end select
		if(iErrCode /= 0) then
			iRetCode = 7
		end if

		! Transmit number of data, if desired
		if(present(ivNumDataOut)) then
			if(allocated(ivNumDataOut)) deallocate(ivNumDataOut)
			allocate(ivNumDataOut(size(ivNumData)))
			ivNumDataOut = ivNumData
		end if

		! Leave
		deallocate(rvTimeStamp_Reduced, ivNumData, rvMin, rvMax, rvMean, rvStDev)

	end function tsAggregateLinear


	! Aggregate data of a time series according to a positive time difference,
	! or a negative code indicating time divisions like month and year.
	! Result is a time series, containing the aggregated values and time
	! stamps spaced according to the time difference selected.
	function tsAggregateLinear2( &
		this, iTimeDelta, &
		rvTimeStamp_Reduced, rvMean, &
		rvStDevOut, rvMinOut, rvMaxOut, ivNumDataOut &
	) result(iRetCode)

		! Routine arguments
		class(TimeSeries), intent(in)					:: this
		integer, intent(in)								:: iTimeDelta			! A positive time difference, or TDELTA_YEARMONTH, or TDELTA_YEAR
		real(8), dimension(:), allocatable				:: rvTimeStamp_Reduced	! The output time stamp
		real, dimension(:), allocatable					:: rvMean				! Mean value
		real, dimension(:), allocatable, optional		:: rvStDevOut			! Standard deviation
		real, dimension(:), allocatable, optional		:: rvMinOut				! Minimum
		real, dimension(:), allocatable, optional		:: rvMaxOut				! Maximum
		integer, dimension(:), allocatable, optional	:: ivNumDataOut			! Number of valid data contributing to classes
		integer											:: iRetCode

		! Locals
		integer								:: iErrCode
		integer								:: n
		integer								:: m
		integer								:: i
		integer								:: j
		integer								:: iProcessing
		integer								:: iYear
		integer								:: iMonth
		integer								:: iMinTimeIndex
		type(DateTime)						:: tDateTime
		integer, dimension(:), allocatable	:: ivTimeIndex
		real, dimension(:), allocatable		:: rvValue
		real(8), dimension(:), allocatable	:: rvTimeStamp
		integer, dimension(:), allocatable	:: ivNumData
		real, dimension(:), allocatable		:: rvMin
		real, dimension(:), allocatable		:: rvMax
		real, dimension(:), allocatable		:: rvStDev

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Check something is to be made
		if(this % isEmpty()) then
			iRetCode = 1
			return
		end if

		! Check delta time validity
		if(iTimeDelta == 0) then
			iRetCode = 2
		elseif(iTimeDelta < 0) then
			if(iTimeDelta /= TDELTA_YEARMONTH .and. iTimeDelta /= TDELTA_YEAR) then
				iRetCode = 2
			end if
		end if
		if(iRetCode /= 0) return

		! Retrieve time stamp and data vectors from original time series
		iErrCode = this % getTimeStamp(rvTimeStamp)
		if(iErrCode /= 0) then
			iRetCode = 3
			return
		end if
		iErrCode = this % getValues(rvValue)
		if(iErrCode /= 0) then
			iRetCode = 4
			deallocate(rvTimeStamp)
			return
		end if
		n = size(rvTimeStamp)

		! Index time, based on the desired time delta
		if(iTimeDelta > 0) then
			allocate(ivTimeIndex(size(rvTimeStamp)))
			where(.valid.rvTimeStamp)
				ivTimeIndex = floor(rvTimeStamp / iTimeDelta) + 1
			elsewhere
				ivTimeIndex = 0
			end where
		else
			select case(iTimeDelta)
			case(TDELTA_YEAR)
				iErrCode = timeGetYear(rvTimeStamp, ivTimeIndex)
			case(TDELTA_YEARMONTH)
				iErrCode = timeGetYearMonth(rvTimeStamp, ivTimeIndex)
			end select
			if(iErrCode /= 0) then
				iRetCode = 5
				deallocate(rvValue)
				deallocate(rvTimeStamp)
				return
			end if
		end if
		if(count(ivTimeIndex > 0) <= 0) then
			iRetCode = 6
			deallocate(rvValue)
			deallocate(rvTimeStamp)
			return
		end if

		! Count maximum index, and use it to reserve workspace
		iMinTimeIndex = minval(ivTimeIndex, mask = ivTimeIndex > 0)
		m = maxval(ivTimeIndex) - iMinTimeIndex + 1
		if(allocated(rvTimeStamp_Reduced)) deallocate(rvTimeStamp_Reduced)
		allocate(rvTimeStamp_Reduced(m))
		if(allocated(rvMean)) deallocate(rvMean)
		allocate(rvMean(m))
		allocate(ivNumData(m), rvMin(m), rvMax(m), rvStDev(m))

		! Change time indicator to a true, 1-based index
		do i = 1, n
			if(ivTimeIndex(i) > 0) then
				ivTimeIndex(i) = ivTimeIndex(i) - iMinTimeIndex + 1
			end if
		end do

		! Form time stamp for new time series; note: 2, not 1, is subtracted
		! from time index. This might sound counter-intuitive, but finds its motivation
		! in the fact that the time index is 1-based (so one 1 to subtract), and
		! j also is (another 1 to subtract). That's is...
		if(iTimeDelta > 0) then
			do j = 1, m
				rvTimeStamp_Reduced(j) = dble((iMinTimeIndex + j - 2)) * iTimeDelta
			end do
		else
			select case(iTimeDelta)
			case(TDELTA_YEAR)
				do j = 1, m
					iYear = iMinTimeIndex + j - 1
					tDateTime = DateTime(iYear, 1, 1, 0, 0, 0.d0)
					rvTimeStamp_Reduced(j) = tDateTime % toEpoch()
				end do
			case(TDELTA_YEARMONTH)
				do j = 1, m
					iMonth = mod(iMinTimeIndex + j - 1, 12) + 1
					iYear  = (iMinTimeIndex + j - 1) / 12
					tDateTime = DateTime(iYear, iMonth, 1, 0, 0, 0.d0)
					rvTimeStamp_Reduced(j) = tDateTime % toEpoch()
				end do
			end select
		end if

		! Update counts
		ivNumData =  0
		rvMin     =  huge(1.)
		rvMax     = -huge(1.)
		rvMean    =  0.
		rvStDev   =  0.
		do i = 1, n
			if((ivTimeIndex(i) > 0) .and. (.valid.rvTimeStamp(i)) .and. (.valid.rvValue(i))) then
				j = ivTimeIndex(i)
				ivNumData(j) = ivNumData(j) + 1
				rvMin(j)     = min(rvMin(j), rvValue(i))
				rvMax(j)     = max(rvMax(j), rvValue(i))
				rvMean(j)    = rvMean(j) + rvValue(i)
				rvStDev(j)   = rvStDev(j) + rvValue(i)**2
			end if
		end do

		! Transform mean and standard deviation counts in nominal quantities.
		! Here I use a little trick, based on non-signalling NaNs: rvMean is computed
		! by specifically discriminating between norman and invalid case. But StDev,
		! on the other side, is computed directly counting on the fact that non
		! signalling NaNs combine algebraically with valid values yielding NaNs
		! (because of IEEE rules).
		where(ivNumData > 0)
			rvMean = rvMean / ivNumData
		elsewhere
			rvMean = NaN
		end where
		rvStDev = sqrt(rvStDev/ivNumData - rvMean**2)

		! Make sure minima and maxima are NaN when class is empty
		where(ivNumData <= 0)
			rvMin = NaN
			rvMax = NaN
		end where

		! Transmit desired quantities
		if(present(rvStDevOut)) then
			if(allocated(rvStDevOut)) deallocate(rvStDevOut)
			allocate(rvStDevOut(size(ivNumData)))
			rvStDevOut = rvStDev
		end if
		if(present(rvMinOut)) then
			if(allocated(rvMinOut)) deallocate(rvMinOut)
			allocate(rvMinOut(size(ivNumData)))
			rvMinOut = rvMin
		end if
		if(present(rvMaxOut)) then
			if(allocated(rvMaxOut)) deallocate(rvMaxOut)
			allocate(rvMaxOut(size(ivNumData)))
			rvMaxOut = rvMax
		end if
		if(present(ivNumDataOut)) then
			if(allocated(ivNumDataOut)) deallocate(ivNumDataOut)
			allocate(ivNumDataOut(size(ivNumData)))
			ivNumDataOut = ivNumData
		end if

		! Leave
		deallocate(ivNumData, rvMin, rvMax, rvStDev, ivTimeIndex)

	end function tsAggregateLinear2

	! Build typical periods (days, months, ...) by applying a periodic aggregation
	function tsAggregatePeriodic( &
		this, iPeriodLength, iTimeDelta, &
		rvMean, &
		rvStDevOut, rvMinOut, rvMaxOut, ivNumDataOut &
	) result(iRetCode)

		! Routine arguments
		class(TimeSeries), intent(in)					:: this
		integer, intent(in)								:: iPeriodLength		! The length of period considered (positive)
		integer, intent(in)								:: iTimeDelta			! A positive time difference (smaller than period length, and preferably an integer divisor of it)
		real, dimension(:), allocatable					:: rvMean				! Mean value
		real, dimension(:), allocatable, optional		:: rvStDevOut			! Standard deviation
		real, dimension(:), allocatable, optional		:: rvMinOut				! Minimum
		real, dimension(:), allocatable, optional		:: rvMaxOut				! Maximum
		integer, dimension(:), allocatable, optional	:: ivNumDataOut			! Number of valid data contributing to classes
		integer											:: iRetCode

		! Locals
		integer								:: iErrCode
		integer								:: n
		integer								:: m
		integer								:: i
		integer								:: j
		integer								:: iProcessing
		integer								:: iYear
		integer								:: iMonth
		integer								:: iMinTimeIndex
		type(DateTime)						:: tDateTime
		integer, dimension(:), allocatable	:: ivTimeIndex
		real, dimension(:), allocatable		:: rvValue
		real(8), dimension(:), allocatable	:: rvTimeStamp
		integer, dimension(:), allocatable	:: ivNumData
		real, dimension(:), allocatable		:: rvMin
		real, dimension(:), allocatable		:: rvMax
		real, dimension(:), allocatable		:: rvStDev

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Check something is to be made
		if(this % isEmpty()) then
			iRetCode = 1
			return
		end if

		! Check period and delta time validity
		if(iPeriodLength <= 0) then
			iRetCode = 2
			return
		end if
		if(iTimeDelta <= 0 .or. iTimeDelta > iPeriodLength) then
			iRetCode = 3
			return
		end if

		! Retrieve time stamp and data vectors from original time series
		iErrCode = this % getTimeStamp(rvTimeStamp)
		if(iErrCode /= 0) then
			iRetCode = 3
			return
		end if
		iErrCode = this % getValues(rvValue)
		if(iErrCode /= 0) then
			iRetCode = 4
			deallocate(rvTimeStamp)
			return
		end if
		n = size(rvTimeStamp)

		! Index time, based on the desired time delta
		iErrCode = timeEncode(rvTimeStamp, iPeriodLength, iTimeDelta, ivTimeIndex)
		if(iErrCode /= 0) then
			iRetCode = 6
			deallocate(rvValue)
			deallocate(rvTimeStamp)
			return
		end if

		! Compute minimum and maximum indices, and use the latter to reserve workspace
		iMinTimeIndex = 1
		m = iPeriodLength / iTimeDelta
		if(allocated(rvMean)) deallocate(rvMean)
		allocate(rvMean(m))
		allocate(ivNumData(m), rvMin(m), rvMax(m), rvStDev(m))

		! Update counts
		ivNumData =  0
		rvMin     =  huge(1.)
		rvMax     = -huge(1.)
		rvMean    =  0.
		rvStDev   =  0.
		do i = 1, n
			if((ivTimeIndex(i) > 0) .and. (.valid.rvTimeStamp(i)) .and. (.valid.rvValue(i))) then
				j = ivTimeIndex(i)
				ivNumData(j) = ivNumData(j) + 1
				rvMin(j)     = min(rvMin(j), rvValue(i))
				rvMax(j)     = max(rvMax(j), rvValue(i))
				rvMean(j)    = rvMean(j) + rvValue(i)
				rvStDev(j)   = rvStDev(j) + rvValue(i)**2
			end if
		end do

		! Transform mean and standard deviation counts in nominal quantities.
		! Here I use a little trick, based on non-signalling NaNs: rvMean is computed
		! by specifically discriminating between norman and invalid case. But StDev,
		! on the other side, is computed directly counting on the fact that non
		! signalling NaNs combine algebraically with valid values yielding NaNs
		! (because of IEEE rules).
		where(ivNumData > 0)
			rvMean = rvMean / ivNumData
		elsewhere
			rvMean = NaN
		end where
		rvStDev = sqrt(rvStDev/ivNumData - rvMean**2)

		! Make sure minima and maxima are NaN when class is empty
		where(ivNumData <= 0)
			rvMin = NaN
			rvMax = NaN
		end where

		! Transmit desired quantities
		if(present(rvStDevOut)) then
			if(allocated(rvStDevOut)) deallocate(rvStDevOut)
			allocate(rvStDevOut(size(ivNumData)))
			rvStDevOut = rvStDev
		end if
		if(present(rvMinOut)) then
			if(allocated(rvMinOut)) deallocate(rvMinOut)
			allocate(rvMinOut(size(ivNumData)))
			rvMinOut = rvMin
		end if
		if(present(rvMaxOut)) then
			if(allocated(rvMaxOut)) deallocate(rvMaxOut)
			allocate(rvMaxOut(size(ivNumData)))
			rvMaxOut = rvMax
		end if
		if(present(ivNumDataOut)) then
			if(allocated(ivNumDataOut)) deallocate(ivNumDataOut)
			allocate(ivNumDataOut(size(ivNumData)))
			ivNumDataOut = ivNumData
		end if

		! Leave
		deallocate(ivNumData, rvMin, rvMax, rvStDev, ivTimeIndex)

	end function tsAggregatePeriodic


	! Create a new time series which is the moving-averaged version of another
	function tsMovingAverage(this, ts, rTimeWidth, iMode) result(iRetCode)

		! Routine arguments
		class(TimeSeries), intent(out)	:: this			! The time series we want to build
		type(TimeSeries), intent(in)	:: ts			! Time series containing the original data
		real(8), intent(in)				:: rTimeWidth	! Width of the entire time span desired (s)
		integer, intent(in), optional	:: iMode		! MA_ALLDATA (default): use all data; MA_STRICT: use only data with whole left and right sub-intervals
		integer							:: iRetCode

		! Locals
		real(8)								:: rDeltaTime
		real(8), dimension(:), allocatable	:: rvTimeStamp
		real, dimension(:), allocatable		:: rvValue
		real, dimension(:), allocatable		:: rvMeanValue
		integer, dimension(:), allocatable	:: ivNumValid
		integer								:: iFirst, iLast
		integer								:: iFrom, iTo
		integer								:: iNumValues
		integer								:: n, i, j
		integer								:: iWellSpaced
		integer								:: iErrCode
		type(TimeSeries)					:: ts1

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Check parameters
		if(rTimeWidth <= 0.d0) then
			iRetCode = 1
			return
		end if

		! First of all, check the input time series is well spaced; if it is, but with
		! hidden gaps, make them evident
		iWellSpaced = ts % timeIsWellSpaced(rDeltaTime)
		if(iWellSpaced == 0) then
			iErrCode = ts % getTimeStamp(rvTimeStamp)
			if(iErrCode /= 0) then
				iRetCode = 3
				return
			end if
			iErrCode = ts % getValues(rvValue)
			if(iErrCode /= 0) then
				iRetCode = 4
				return
			end if
		elseif(iWellSpaced == 1) then
			iErrCode = ts1 % CreateFromTimeSeries(ts, .true.)
			if(iErrCode /= 0) then
				iRetCode = 2
				return
			end if
			iErrCode = ts1 % getTimeStamp(rvTimeStamp)
			if(iErrCode /= 0) then
				iRetCode = 3
				return
			end if
			iErrCode = ts1 % getValues(rvValue)
			if(iErrCode /= 0) then
				iRetCode = 4
				return
			end if
		elseif(iWellSpaced < 0 .or. iWellSpaced > 1) then
			iRetCode = 5
			return
		end if
		if(size(rvTimeStamp) < 1) then
			iRetCode = 6
			deallocate(rvTimeStamp, rvValue)
			return
		end if
		! Post-condition: rvTimeStamp and rvValue both allocated, and with at least one element;
		!                 additionally, rvTimeStamp is well-spaced and monotonic, and the rvValue
		! vector "may" contain gaps.

		! Convert time width in the number of items to take before and after the current
		! time series element. If it is zero or less, copy the input series as it is
		n = size(rvTimeStamp)
		iNumValues = floor(rTimeWidth / (2.*rDeltaTime))
		if(iNumValues < 1) then
			if(allocated(this % rvTimeStamp)) deallocate(this % rvTimeStamp)
			if(allocated(this % rvValue))     deallocate(this % rvValue)
			allocate(this % rvTimeStamp(size(rvTimeStamp)))
			allocate(this % rvValue(size(rvValue)))
			this % rvTimeStamp = rvTimeStamp
			this % rvValue     = rvValue
			deallocate(rvTimeStamp, rvValue)
			return
		end if

		! Set initial and final indices to consider
		if(present(iMode)) then
			if(iMode == MA_STRICT) then
				iFirst = 1 + iNumValues
				iLast  = n - iNumValues
				if(iFirst >= iLast) then
					iRetCode = 7
					deallocate(rvTimeStamp, rvValue)
					return
				end if
			elseif(iMode == MA_ALLDATA) then
				iFirst = 1
				iLast  = n
			else
				iRetCode = 7
				deallocate(rvTimeStamp, rvValue)
				return
			end if
		else	! Default: MA_ALLDATA
			iFirst = 1
			iLast  = n
		end if

		! Compute the desired time series, taking into account
		! the number of valid values in averaging
		allocate(ivNumValid(iLast-iFirst+1), rvMeanValue(iLast-iFirst+1))
		j = 1
		do i = iFirst, iLast
			iFrom = max(i - iNumValues, 1)
			iTo   = min(i + iNumValues, n)
			ivNumValid(j) = count(.valid.rvValue(iFrom:iTo))
			if(ivNumValid(j) > 0) then
				rvMeanValue(j) = sum(rvValue(iFrom:iTo), mask = .valid.rvValue(iFrom:iTo)) / ivNumValid(j)
			else
				rvMeanValue(j) = NaN
			end if
			j = j + 1
		end do

		! Fill current series with new averaged values
		if(allocated(this % rvTimeStamp)) deallocate(this % rvTimeStamp)
		if(allocated(this % rvValue))     deallocate(this % rvValue)
		allocate(this % rvTimeStamp(size(rvTimeStamp)))
		allocate(this % rvValue(size(rvValue)))
		this % rvTimeStamp = rvTimeStamp(iFirst:iLast)
		this % rvValue     = rvMeanValue

	end function tsMovingAverage


	! Create a new time series which is the moving-stddev of another
	function tsMovingStdDev(this, ts, rTimeWidth, iMode) result(iRetCode)

		! Routine arguments
		class(TimeSeries), intent(out)	:: this			! The time series we want to build
		type(TimeSeries), intent(in)	:: ts			! Time series containing the original data
		real(8), intent(in)				:: rTimeWidth	! Width of the entire time span desired (s)
		integer, intent(in), optional	:: iMode		! MA_ALLDATA (default): use all data; MA_STRICT: use only data with whole left and right sub-intervals
		integer							:: iRetCode

		! Locals
		real(8)								:: rDeltaTime
		real(8), dimension(:), allocatable	:: rvTimeStamp
		real, dimension(:), allocatable		:: rvValue
		real, dimension(:), allocatable		:: rvMeanValue
		real, dimension(:), allocatable		:: rvMeanSquaredValue
		integer, dimension(:), allocatable	:: ivNumValid
		integer								:: iFirst, iLast
		integer								:: iFrom, iTo
		integer								:: iNumValues
		integer								:: n, i, j
		integer								:: iWellSpaced
		integer								:: iErrCode
		type(TimeSeries)					:: ts1

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Check parameters
		if(rTimeWidth <= 0.d0) then
			iRetCode = 1
			return
		end if

		! First of all, check the input time series is well spaced; if it is, but with
		! hidden gaps, make them evident
		iWellSpaced = ts % timeIsWellSpaced(rDeltaTime)
		if(iWellSpaced == 0) then
			iErrCode = ts % getTimeStamp(rvTimeStamp)
			if(iErrCode /= 0) then
				iRetCode = 3
				return
			end if
			iErrCode = ts % getValues(rvValue)
			if(iErrCode /= 0) then
				iRetCode = 4
				return
			end if
		elseif(iWellSpaced == 1) then
			iErrCode = ts1 % CreateFromTimeSeries(ts, .true.)
			if(iErrCode /= 0) then
				iRetCode = 2
				return
			end if
			iErrCode = ts1 % getTimeStamp(rvTimeStamp)
			if(iErrCode /= 0) then
				iRetCode = 3
				return
			end if
			iErrCode = ts1 % getValues(rvValue)
			if(iErrCode /= 0) then
				iRetCode = 4
				return
			end if
		elseif(iWellSpaced < 0 .or. iWellSpaced > 1) then
			iRetCode = 5
			return
		end if
		if(size(rvTimeStamp) < 1) then
			iRetCode = 6
			deallocate(rvTimeStamp, rvValue)
			return
		end if
		! Post-condition: rvTimeStamp and rvValue both allocated, and with at least one element;
		!                 additionally, rvTimeStamp is well-spaced and monotonic, and the rvValue
		! vector "may" contain gaps.

		! Convert time width in the number of items to take before and after the current
		! time series element. If it is zero or less, copy the input series as it is
		n = size(rvTimeStamp)
		iNumValues = floor(rTimeWidth / (2.*rDeltaTime))
		if(iNumValues < 1) then
			if(allocated(this % rvTimeStamp)) deallocate(this % rvTimeStamp)
			if(allocated(this % rvValue))     deallocate(this % rvValue)
			allocate(this % rvTimeStamp(size(rvTimeStamp)))
			allocate(this % rvValue(size(rvValue)))
			this % rvTimeStamp = rvTimeStamp
			this % rvValue     = rvValue
			deallocate(rvTimeStamp, rvValue)
			return
		end if

		! Set initial and final indices to consider
		if(present(iMode)) then
			if(iMode == MA_STRICT) then
				iFirst = 1 + iNumValues
				iLast  = n - iNumValues
				if(iFirst >= iLast) then
					iRetCode = 7
					deallocate(rvTimeStamp, rvValue)
					return
				end if
			elseif(iMode == MA_ALLDATA) then
				iFirst = 1
				iLast  = n
			else
				iRetCode = 7
				deallocate(rvTimeStamp, rvValue)
				return
			end if
		else	! Default: MA_ALLDATA
			iFirst = 1
			iLast  = n
		end if

		! Compute the desired time series, taking into account
		! the number of valid values in averaging
		allocate(ivNumValid(iLast-iFirst+1), rvMeanValue(iLast-iFirst+1), rvMeanSquaredValue(iLast-iFirst+1))
		j = 1
		do i = iFirst, iLast
			iFrom = max(i - iNumValues, 1)
			iTo   = min(i + iNumValues, n)
			ivNumValid(j) = count(.valid.rvValue(iFrom:iTo))
			if(ivNumValid(j) > 0) then
				rvMeanValue(j)        = sum(rvValue(iFrom:iTo), mask = .valid.rvValue(iFrom:iTo)) / ivNumValid(j)
				rvMeanSquaredValue(j) = sum(rvValue(iFrom:iTo)**2, mask = .valid.rvValue(iFrom:iTo)) / ivNumValid(j)
			else
				rvMeanValue(j)        = NaN
				rvMeanSquaredValue(j) = NaN
			end if
			j = j + 1
		end do

		! Fill current series with new averaged values
		if(allocated(this % rvTimeStamp)) deallocate(this % rvTimeStamp)
		if(allocated(this % rvValue))     deallocate(this % rvValue)
		allocate(this % rvTimeStamp(size(rvTimeStamp)))
		allocate(this % rvValue(size(rvValue)))
		this % rvTimeStamp = rvTimeStamp(iFirst:iLast)
		this % rvValue     = sqrt(rvMeanSquaredValue - rvMeanValue**2)

	end function tsMovingStdDev


	function tsFillGaps(this, iDaysRadius, lvOriginal) result(iRetCode)

		! Routine arguments
		class(TimeSeries), intent(inout)				:: this			! The time series we want to gap-fill
		integer, intent(in)								:: iDaysRadius
		logical, dimension(:), allocatable, intent(out)	:: lvOriginal
		integer											:: iRetCode

		! Locals
		integer								:: iErrCode
		integer								:: iNumItemsPerDay
		integer								:: iNumDays
		integer								:: iCurDay
		integer								:: i
		real(8)								:: rDeltaTime
		integer								:: iIsWellSpaced
		integer								:: iNumData
		integer								:: iNumGaps
		real(8)								:: rBaseDay
		real(8)								:: rWindowBegin
		real(8)								:: rWindowEnd
		integer, dimension(:), allocatable	:: ivNumValues
		logical, dimension(:), allocatable	:: lvCurDay
		integer, dimension(:), allocatable	:: ivTimeIndex
		real(8), dimension(:), allocatable	:: rvSumValues

		! Constants
		real(8), parameter	:: ONE_HOUR = 3600.d0
		real(8), parameter	:: ONE_DAY  = 24*ONE_HOUR

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Check parameters
		if(this % isEmpty()) then
			iRetCode = 1
			return
		end if
		iIsWellSpaced = this % timeIsWellSpaced(rDeltaTime, iNumGaps)
		if(iIsWellSpaced /= 0) then
			iRetCode = 1
			return
		end if

		! Reserve workspace
		iNumData = size(this % rvTimeStamp)
		if(allocated(lvOriginal)) deallocate(lvOriginal)
		allocate(lvOriginal(iNumData))
		allocate(lvCurDay(iNumData))

		! How many data come in a day?
		if(rDeltaTime <= 0.d0) then
			iRetCode = 2
			return
		end if
		iNumItemsPerDay = floor(ONE_DAY / rDeltaTime)

		! How many days in data set?
		rBaseDay = timeFloorDay(this % rvTimeStamp(1))
		iNumDays = floor((timeFloorDay(this % rvTimeStamp(iNumData)) - rBaseDay + ONE_DAY) / ONE_DAY) + 1
		if(iNumDays <= 0) then
			iRetCode = 3
			return
		end if

		! Reserve temporary workspace
		allocate(ivNumValues(iNumItemsPerDay), rvSumValues(iNumItemsPerDay), STAT=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 4
			return
		end if
		rvSumValues = 0.
		ivNumValues = 0

		! Encode time to typical-day index
		iErrCode = timeEncode(this % rvTimeStamp, int(ONE_DAY, kind=4), int(rDeltaTime, kind=4), ivTimeIndex)
		if(iErrCode /= 0) then
			iRetCode = 5
			return
		end if

		! Iterate over days
		do iCurDay = 1, iNumDays

			! Delimit day
			rWindowBegin = rBaseDay
			rWindowEnd   = rWindowBegin + ONE_DAY
			lvCurDay     = this % rvTimeStamp >= rWindowBegin .and. this % rvTimeStamp <= rWindowEnd

			! Check whether something is to be made on this day
			iNumGaps = count((.invalid.this % rvValue) .and. lvCurDay)
			if(iNumGaps > 0) then

				! Update counters for the typical day
				rWindowBegin = rBaseDay + (iCurDay-1)*ONE_DAY - iDaysRadius*ONE_DAY
				rWindowEnd   = rBaseDay + (iCurDay-1)*ONE_DAY + (iDaysRadius+1)*ONE_DAY
				do i = 1, size(this % rvTimeStamp)
					if(this % rvTimeStamp(i) >= rWindowBegin .and. this % rvTimeStamp(i) <= rWindowEnd) then
						if(ivTimeIndex(i) > 0) then
							ivNumValues(ivTimeIndex(i)) = ivNumValues(ivTimeIndex(i)) + 1
							rvSumValues(ivTimeIndex(i)) = rvSumValues(ivTimeIndex(i)) + this % rvValue(i)
						end if
					end if
				end do

				! Render the typical day

			end if

		end do

		! Leave
		deallocate(ivNumValues, rvSumValues)
		deallocate(lvCurDay)

	end function tsFillGaps


	function dfClean(this) result(iRetCode)

		! Routine arguments
		class(TwoDimensionalField), intent(out)	:: this
		integer									:: iRetCode

		! Locals
		! --none--

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Reclaim workspace, if any
		if(allocated(this % rmValue))       deallocate(this % rmValue)
		if(allocated(this % rvX))           deallocate(this % rvX)
		if(allocated(this % rvY))           deallocate(this % rvY)
		if(allocated(this % imNumAdjacent)) deallocate(this % imNumAdjacent)
		if(allocated(this % iaAdjacent))    deallocate(this % iaAdjacent)
		if(allocated(this % raDistance))    deallocate(this % raDistance)

	end function dfClean


	function dfInitialize(this, rXsw, rYsw, rDx, rDy, iNx, iNy, rvX, rvY, rThreshold) result(iRetCode)

		! Routine arguments
		class(TwoDimensionalField), intent(inout)	:: this
		real(8), intent(in)							:: rXsw
		real(8), intent(in)							:: rYsw
		real(8), intent(in)							:: rDx
		real(8), intent(in)							:: rDy
		integer, intent(in)							:: iNx
		integer, intent(in)							:: iNy
		real, dimension(:), intent(in)				:: rvX
		real, dimension(:), intent(in)				:: rvY
		real(8), intent(in)							:: rThreshold
		integer										:: iRetCode

		! Locals
		integer		:: iErrCode
		integer		:: i, j, k
		integer		:: n, m

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Clean workspace
		iErrCode = this % clean()
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if

		! Check parameters
		if(.not.(.valid.rXsw) .or. .not.(.valid.rYsw)) then
			iRetCode = 2
			return
		end if
		if(iNx <= 0 .or. iNy <= 0) then
			iRetCode = 2
			return
		end if
		if(rDx <= 0.d0 .or. rDy <= 0.d0) then
			iRetCode = 2
			return
		end if

		! Check the input data make some sense
		if(size(rvX) <= 0 .or. size(rvY) <= 0) then
			iRetCode = 3
			return
		end if
		if(size(rvX) /= size(rvY)) then
			iRetCode = 4
			return
		end if
		if(any(.invalid.rvX) .or. any(.invalid.rvY)) then
			iRetCode = 5
			return
		end if
		n = size(rvX)	! Which is, by the preceding tests, the same as size(rvY)
		if(rThreshold <= 0.d0) then
			iRetCode = 6
			return
		end if

		! Reserve workspace
		allocate(this % rmValue(iNx, iNy))
		allocate(this % rvX(iNx))
		allocate(this % rvY(iNy))
		allocate(this % imNumAdjacent(iNx, iNy))
		allocate(this % iaAdjacent(n, iNx, iNy))
		allocate(this % raDistance(n, iNx, iNy))

		! Fill workspace
		this % rmValue = 0.d0
		this % rvX     = [(rXsw + rDx * (i-1), i=1,iNx)]
		this % rvY     = [(rYsw + rDy * (i-1), i=1,iNy)]

		! Build the distance matrix
		allocate(this % raDistance(n, iNx, iNy))
		do k = 1, iNy
			do j = 1, iNx
				do i = 1, n
					this % raDistance(i,j,k) = sqrt((rvX(i) - this % rvX(j))**2 + (rvY(i) - this % rvY(k))**2)
				end do
			end do
		end do

		! Compare to distance threshold and build the adjacency lists
		do k = 1, iNy
			do j = 1, iNx
				m = 0
				do i = 1, n
					if(this % raDistance(i,j,k) >= rThreshold) then
						m = m + 1
						this % imNumAdjacent(j,k)  = m
						this % iaAdjacent(m, j, k) = i
					end if
				end do
			end do
		end do

		! Check some grid point is not covered by at least one experimental point
		if(any(this % imNumAdjacent <= 0)) then
			iRetCode = 7
			return
		end if
		! Post-condition: we have distances, and adjacency lists: success

	end function dfInitialize


	function dfEvaluate(this, rvValue) result(iRetCode)

		! Routine arguments
		class(TwoDimensionalField), intent(inout)	:: this
		real, dimension(:), intent(in)				:: rvValue
		integer										:: iRetCode

		! Locals
		integer		:: iErrCode
		integer		:: iNx
		integer		:: iNy

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Check the 2D field to make some sense
		if(.not.allocated(this % rmValue) .or. .not.allocated(this % rvX) .or. .not.allocated(this % rvY)) then
			iRetCode = 1
			return
		end if
		if(size(this % rvX) <= 0 .or. size(this % rvY) <= 0) then
			iRetCode = 2
			return
		end if

		! Generate weights


	end function dfEvaluate

	! ********************************
	! * Members of MultiSeries class *
	! ********************************

	function msCreateEmpty(this) result(iRetCode)

		! Routine arguments
		class(MultiSeries), intent(inout)	:: this			! Current time series
		integer								:: iRetCode		! Return code (0 if successful completion; any non-zero in case of error(s))

		! Locals
		integer	:: iErrCode

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Clean workspace
		if(allocated(this % rvTimeStamp)) deallocate(this % rvTimeStamp)
		if(allocated(this % rmValue)) deallocate(this % rmValue)

	end function msCreateEmpty


	function msAddTimeSeries(this, sColumn, tSeries) result(iRetCode)

		! Routine arguments
		class(MultiSeries), intent(inout)	:: this
		character(len=*), intent(in)		:: sColumn
		type(TimeSeries), intent(in)		:: tSeries
		integer								:: iRetCode

		! Locals
		integer											:: iErrCode
		integer											:: i
		integer											:: iNumData
		integer											:: iNumCols
		character(len=16), dimension(:), allocatable	:: svColumn
		real, dimension(:,:), allocatable				:: rmValue
		real(8), dimension(:), allocatable				:: rvTimeStamp
		real, dimension(:), allocatable					:: rvData

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Check input parameters
		if(tSeries % isEmpty()) then
			iRetCode = 1
			return
		end if
		if(sColumn == ' ') then
			iRetCode = 2
			return
		end if
		iNumData = tSeries % size()

		! Check the multiseries is empty or not, and act accordingly
		if(this % isEmpty()) then	! The multivariate time series is empty: create it

			! Reserve workspace
			iErrCode = this % CreateEmpty()		! force deallocation of "all" data - just out of prudence
			allocate(this % rvTimeStamp(iNumData), stat = iErrCode)
			if(iErrCode /= 0) then
				iRetCode = 3
				return
			end if
			allocate(this % rmValue(iNumData, 1), stat = iErrCode)
			if(iErrCode /= 0) then
				iRetCode = 4
				return
			end if
			allocate(this % svColumn(1), stat = iErrCode)
			if(iErrCode /= 0) then
				iRetCode = 5
				return
			end if

			! Get data from time series
			iErrCode = tSeries % getTimeStamp(rvTimeStamp)
			if(iErrCode /= 0) then
				iRetCode = 6
				return
			end if
			iErrCode = tSeries % getValues(rvData)
			if(iErrCode /= 0) then
				iRetCode = 7
				return
			end if

			! Put data to multivariate time series (with one column)
			this % rvTimeStamp  = rvTimeStamp
			this % rmValue(:,1) = rvData
			this % svColumn(1)  = sColumn
			
		else	! The multivariate series is non-empty

			! Check the multiseries makes sense (this is just for defensive programming)
			if(.not.allocated(this % svColumn) .or. .not.allocated(this % rmValue)) then
				iRetCode = 8
				return
			end if

			! Check the time series to add is compatible by size and time to the multiseries
			iNumData = size(this % rmValue, dim=1)
			iNumCols = size(this % rmValue, dim=2)
			if(tSeries % size() /= iNumData) then
				iRetCode = 9
				return
			end if
			iErrCode = tSeries % getTimeStamp(rvTimeStamp)
			if(iErrCode /= 0) then
				iRetCode = 10
				return
			end if
			do i = 1, iNumData
				if(abs(this % rvTimeStamp(i) - rvTimeStamp(i)) > 1.d-1) then
					iRetCode = 11
					return
				end if
			end do

			! Get a copy of data from current multiseries
			allocate(svColumn(iNumCols + 1), stat=iErrCode)
			if(iErrCode /= 0) then
				iRetCode = 12
				return
			end if
			allocate(rmValue(iNumData, iNumCols + 1), stat=iErrCode)
			if(iErrCode /= 0) then
				iRetCode = 13
				return
			end if
			svColumn(1:iNumCols) = this % svColumn
			rmValue(1:iNumData, 1:iNumCols) = this % rmValue

			! Add temporary vectors the time series data
			iErrCode = tSeries % GetValues(rvData)
			if(iErrCode /= 0) then
				iRetCode = 14
				return
			end if
			svColumn(iNumCols + 1) = sColumn
			rmValue(1:iNumData, iNumCols + 1) = rvData

			! Re-allocate multiseries space, and fill it with new data
			deallocate(this % svColumn)
			deallocate(this % rmValue)
			allocate(this % svColumn(iNumCols + 1), stat=iErrCode)
			if(iErrCode /= 0) then
				iRetCode = 15
				return
			end if
			allocate(this % rmValue(iNumData, iNumCols + 1), stat=iErrCode)
			if(iErrCode /= 0) then
				iRetCode = 16
				return
			end if
			this % svColumn = svColumn
			this % rmValue  = rmValue
			
		end if

		! Reclaim workspace
		if(allocated(rmValue))  deallocate(rmValue)
		if(allocated(svColumn)) deallocate(svColumn)
		deallocate(rvTimeStamp)
		deallocate(rvData)

	end function msAddTimeSeries


	function msGetTimeSeries(this, sColumn, tSeries) result(iRetCode)

		! Routine arguments
		class(MultiSeries), intent(in)	:: this
		character(len=*), intent(in)	:: sColumn
		type(TimeSeries), intent(out)	:: tSeries
		integer							:: iRetCode

		! Locals
		integer		:: iErrCode
		integer		:: iPos
		integer		:: i

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Check all this makes sense
		if(this % isEmpty()) then
			iRetCode = 1
			return
		end if

		! Find column in name set
		iPos = 0
		do i = 1, size(this % rvTimeStamp)
			if(sColumn == this % svColumn(i)) then
				iPos = i
				exit
			end if
		end do
		if(iPos <= 0) then
			iRetCode = 2
			return
		end if

		! Create workspace from data
		iErrCode = tSeries % createFromTimeAndDataVectors(this % rvTimeStamp, this % rmValue(:, iPos))
		if(iErrCode /= 0) then
			iRetCode = 3
			return
		end if

	end function msGetTimeSeries


	function msGetTimeStamp(this, rvTimeStamp) result(iRetCode)

		! Routine arguments
		class(MultiSeries), intent(in)					:: this
		real(8), dimension(:), allocatable, intent(out)	:: rvTimeStamp
		integer											:: iRetCode

		! Locals
		integer		:: iErrCode

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Check all this makes sense
		if(this % isEmpty()) then
			iRetCode = 1
			return
		end if

		! Create workspace from data
		if(allocated(rvTimeStamp)) deallocate(rvTimeStamp)
		allocate(rvTimeStamp(size(this % rvTimeStamp)), stat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 3
			return
		end if
		rvTimeStamp = this % rvTimeStamp

	end function msGetTimeStamp


	function msGetVector(this, sColumn, rvVector) result(iRetCode)

		! Routine arguments
		class(MultiSeries), intent(in)					:: this
		character(len=*), intent(in)					:: sColumn
		real(8), dimension(:), allocatable, intent(out)	:: rvVector
		integer											:: iRetCode

		! Locals
		integer		:: iErrCode
		integer		:: iPos
		integer		:: i

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Check all this makes sense
		if(this % isEmpty()) then
			iRetCode = 1
			return
		end if

		! Find column in name set
		iPos = 0
		do i = 1, size(this % rvTimeStamp)
			if(sColumn == this % svColumn(i)) then
				iPos = i
				exit
			end if
		end do
		if(iPos <= 0) then
			iRetCode = 2
			return
		end if

		! Create workspace from data
		if(allocated(rvVector)) deallocate(rvVector)
		allocate(rvVector(size(this % rmValue, dim=1)), stat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 3
			return
		end if
		rvVector = this % rmValue(:,iPos)

	end function msGetVector


	function msIsEmpty(this) result(lIsEmpty)

		! Routine arguments
		class(MultiSeries), intent(in)	:: this
		logical							:: lIsEmpty

		! Locals
		! --none--

		! Check emptyness
		lIsEmpty = .not.allocated(this % rvTimeStamp)

	end function msIsEmpty


	! *********************
	! * Internal routines *
	! *********************

	! quicksort.f -*-f90-*-
	! Author: t-nissie, some tweaks by 1AdAstra1, and some others by Mauri Favaron
	! License: GPLv3
	! Gist: https://gist.github.com/t-nissie/479f0f16966925fa29ea
	!
	recursive subroutine quicksort(a)

		! Routine arguments
		real, dimension(:), intent(inout)	:: a

		! Locals
		real	:: x, t
		integer :: first = 1, last
		integer :: i, j

		! Initialization
		last = size(a)
		if(last <= 1) return	! Nothing to do
		x = a( (first+last) / 2 )
		i = first
		j = last

		! Exploration phase
		do
			do while (a(i) < x)
				i=i+1
			end do
			do while (x < a(j))
				j=j-1
			end do
			if (i >= j) exit
			t = a(i);  a(i) = a(j);  a(j) = t
			i=i+1
			j=j-1
		end do

		! Recursion phase
		if (first < i - 1) call quicksort(a(first : i - 1))
		if (j + 1 < last)  call quicksort(a(j + 1 : last))

	end subroutine quicksort


	! Computes the rank index of an array
	recursive subroutine quicksort_idx_4(a, idx)

		! Routine arguments
		real, dimension(:), intent(inout)		:: a	! On entry the vector to sort
		integer, dimension(:), intent(inout)	:: idx	! On entry, the identity permutation denoted as [1, 2, 3, ..., n]

		! Locals
		real	:: x, t
		integer :: first = 1, last
		integer :: i, j
		integer	:: iHold

		! Initialization
		last = size(a)
		if(last <= 1) return	! Nothing to do
		x = a( (first+last) / 2 )
		i = first
		j = last

		! Exploration phase
		do
			do while (a(i) < x)
				i=i+1
			end do
			do while (x < a(j))
				j=j-1
			end do
			if (i >= j) exit
			t = a(i);  a(i) = a(j);  a(j) = t
			iHold = idx(i); idx(i) = idx(j); idx(j) = iHold
			i=i+1
			j=j-1
		end do

		! Recursion phase
		if (first < i - 1) call quicksort_idx_4(a(first : i - 1), idx(first : i - 1))
		if (j + 1 < last)  call quicksort_idx_4(a(j + 1 : last), idx(j + 1 : last))

	end subroutine quicksort_idx_4


	! Computes the rank index of an array
	recursive subroutine quicksort_idx_8(a, idx)

		! Routine arguments
		real(8), dimension(:), intent(inout)	:: a	! On entry the vector to sort
		integer, dimension(:), intent(inout)	:: idx	! On entry, the identity permutation denoted as [1, 2, 3, ..., n]

		! Locals
		real(8)	:: x, t
		integer :: first = 1, last
		integer :: i, j
		integer	:: iHold

		! Initialization
		last = size(a)
		if(last <= 1) return	! Nothing to do
		x = a( (first+last) / 2 )
		i = first
		j = last

		! Exploration phase
		do
			do while (a(i) < x)
				i=i+1
			end do
			do while (x < a(j))
				j=j-1
			end do
			if (i >= j) exit
			t = a(i);  a(i) = a(j);  a(j) = t
			iHold = idx(i); idx(i) = idx(j); idx(j) = iHold
			i=i+1
			j=j-1
		end do

		! Recursion phase
		if (first < i - 1) call quicksort_idx_8(a(first : i - 1), idx(first : i - 1))
		if (j + 1 < last)  call quicksort_idx_8(a(j + 1 : last), idx(j + 1 : last))

	end subroutine quicksort_idx_8

end module pbl_stat
