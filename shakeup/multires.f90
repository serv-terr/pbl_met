! Preliminary version of multi-resolution decomposer; will
! finish into pbl_met, eventually.
!
! By: Patrizia Favaron
!
module multires

    use pbl_met

    implicit none
    
    private
    
    ! Public interface
    public  :: signal
    
    ! Data types
    type signal
        logical                             :: lIsComplete = .false.
        real(8), dimension(:), allocatable  :: rvTimeStamp
        real, dimension(:), allocatable     :: rvData
        real, dimension(:), allocatable     :: rvVariance
        real, dimension(:), allocatable     :: rvResidual
        real, dimension(:,:), allocatable   :: rmData
    contains
        ! Constructors
        procedure   :: create                => sg_create
        procedure   :: create_from_series    => sg_create_from_series
        ! Query status
        procedure   :: get_times             => sg_get_times
        ! Extract useful information
        procedure   :: approximate           => sg_approximate
        procedure   :: get_variances         => sg_get_variances
        procedure   :: get_total_variation   => sg_get_total_variation
        procedure   :: get_partial_variation => sg_get_partial_variation
    end type signal
    
contains

    function sg_create(this, rvTimeStamp, rvData, iNumHalvingsIn) result(iRetCode)
    
        ! Routine arguments
        class(signal), intent(out)          :: this
        real(8), dimension(:), intent(in)   :: rvTimeStamp
        real, dimension(:), intent(in)      :: rvData
        integer, intent(in), optional       :: iNumHalvingsIn
        integer                             :: iRetCode
        
        ! Locals
        integer                             :: n
        integer                             :: i
        logical                             :: lIsMonotonic
        real(8)                             :: rMinTimeStamp
        real(8)                             :: rMaxTimeStamp
        integer                             :: iNumHalvings
        integer                             :: iHalving
        integer                             :: iMaxBlocks
        integer                             :: iNumBlocks
        integer                             :: iBlock
        integer                             :: iNumData
        integer, dimension(:), allocatable  :: ivBeginBlock
        integer, dimension(:), allocatable  :: ivEndBlock
        integer, dimension(:), allocatable  :: ivBlock
        real(8)                             :: rDelta
        real                                :: rMean
        real                                :: rVariance
        real, dimension(:), allocatable     :: rvVariance
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Check size of vectors is the same
        n = size(rvTimeStamp)
        if(n <= 0 .or. size(rvData) /= n) then
            iRetCode = 1
            return
        end if
        ! Post-condition: size(rvTimeStamp) = size(rvData) > 0
        ! So, it makes sense to go on.
        
        ! Check time stamps form a non-decreasing monotonic series
        lIsMonotonic = .true.
        do i = 2, n
            if(rvTimeStamp(i-1) > rvTimeStamp(i)) then
                lIsMonotonic = .false.
                exit
            end if
        end do
        if(.not.lIsMonotonic) then
            iRetCode = 2
            return
        end if
        
        ! Get minimum and maximum time stamps, and decide the
        ! overall time interval based on them; the overall time interval will be
        ! used in halving
        rMinTimeStamp = minval(rvTimeStamp)
        rMaxTimeStamp = maxval(rvTimeStamp) + 0.001d0   ! The added millisecond is to ensure time conditions like Ta <= T < Tb
        if(rMaxTimeStamp - rMinTimeStamp < 0.1d0) then
            iRetCode = 3
            return
        end if
        if(present(iNumHalvingsIn)) then
            iNumHalvings = min(max(2, iNumHalvingsIn), floor(log(rMaxTimeStamp - rMinTimeStamp)/log(2.d0)))
        else
            iNumHalvings  = floor(log(rMaxTimeStamp - rMinTimeStamp)/log(2.d0))
        end if
        iMaxBlocks    = 2**iNumHalvings
        allocate(ivBeginBlock(iMaxBlocks))
        allocate(ivEndBlock(iMaxBlocks))
        allocate(ivBlock(n))
        allocate(rvVariance(iNumHalvings+1))
        
        ! Reserve module data space
        if(allocated(this % rvTimeStamp)) deallocate(this % rvTimeStamp)
        if(allocated(this % rvData)) deallocate(this % rvData)
        if(allocated(this % rvResidual)) deallocate(this % rvResidual)
        if(allocated(this % rmData)) deallocate(this % rmData)
        if(allocated(this % rvVariance)) deallocate(this % rvVariance)
        allocate(this % rvTimeStamp(n))
        allocate(this % rvData(n))
        allocate(this % rvResidual(n))
        allocate(this % rmData(n,iNumHalvings))
        allocate(this % rvVariance(iNumHalvings))
        
        ! Save data in their original form
        this % rvTimeStamp = rvTimeStamp
        this % rvData      = rvData
        this % rvVariance  = 0.
        
        ! Successive halvings
        do iHalving = 0, iNumHalvings - 1
        
            ! Determine the block index
            iNumBlocks = 2**iHalving
            rDelta     = (rMaxTimeStamp - rMinTimeStamp) / iNumBlocks
            ivBlock    = int((rvTimeStamp - rvTimeStamp(1)) / rDelta) + 1
            ! Post-condition: as 'rvTimeStamp' is monotonic, also 'ivBlock' is
            
            ! Iterate over block indices and find the block limits
            iBlock = 1
            ivBeginBlock(iBlock) = 1
            do i = 2, n
                if(ivBlock(i) /= ivBlock(i-1)) then
                    ivEndBlock(iBlock)   = i - 1
                    iBlock               = iBlock + 1
                    if(iBlock > iNumBlocks) then
                        iRetCode = 4
                        return
                    end if
                    ivBeginBlock(iBlock) = i
                end if
            end do
            ivEndBlock(iBlock) = n
            if(iBlock /= iNumBlocks) then
                iRetCode = 5
                return
            end if
            ! Post-condition: all expected blocks obtained their bounding indices
            
            ! Compute means and residuals
            if(iHalving == 0) then
                rMean                = sum(this % rvData) / n
                rVariance            = sum((this % rvData - rMean)**2) / n  ! Total signal variance
                this % rvResidual    = this % rvData - rMean
                this % rmData(:, 1)  = rMean
                rvVariance(1)        = rVariance
            else
                do iBlock = 1, iNumBlocks
                    iNumData  = ivEndBlock(iBlock) - ivBeginBlock(iBlock) + 1
                    rMean     = sum(this % rvResidual(ivBeginBlock(iBlock):ivEndBlock(iBlock))) / iNumData
                    this % rmData(ivBeginBlock(iBlock):ivEndBlock(iBlock), iHalving+1) = rMean
                end do
                this % rvResidual = this % rvResidual - this % rmData(:, iHalving+1)
                rMean = sum(this % rmData(:, iHalving+1)) / n
                rvVariance(iHalving+1) = sum((this % rmData(:, iHalving+1) - rMean)**2) / n
            end if
            
        end do
        
        ! Add variance of residual
        rMean                      = sum(this % rvResidual) / n
        rvVariance(iNumHalvings+1) = sum((this % rvResidual - rMean)**2) / n
        
        ! Save variance
        this % rvVariance = rvVariance
        
        ! Set the completion flag to .true. informing users the structure is ready for use
        this % lIsComplete = .true.
        
    end function sg_create
    
    
    function sg_create_from_series(this, tSeries) result(iRetCode)

        ! Routine arguments
        class(signal), intent(out)          :: this
        type(TimeSeries), intent(in)        :: tSeries
        integer                             :: iRetCode
        
        ! Locals
        integer                             :: iErrCode
        integer                             :: n
        real(8), dimension(:), allocatable  :: rvTimeStamp
        real, dimension(:), allocatable     :: rvValue
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Check the time series is non-empty
        n = tSeries % size()
        if(n <= 0) then
            iRetCode = 1
            return
        end if
        ! Post-condition: Tiem series is non-empty, it makes sense to proceed
        
        ! Get time series data; as 'n' > 0, there is no need to check errors
        iRetCode = tSeries % getTimeStamp(rvTimeStamp)
        iRetCode = tSeries % getValues(rvValue)
        
        ! Create data set the normal way
        iErrCode = this % create(rvTimeStamp, rvValue)
        if(iErrCode /= 0) then
            iRetCode = 2 + iRetCode
            return
        end if
        
    end function sg_create_from_series
    
    
    function sg_approximate(this, iHalving, rvApproxSignal) result(iRetCode)
    
        ! Routine aguments
        class(signal), intent(in)                       :: this
        integer, intent(in)                             :: iHalving
        real, dimension(:), allocatable, intent(out)    :: rvApproxSignal
        integer                                         :: iRetCode
        
        ! Locals
        integer :: iLine
        integer :: i
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Check the signal is complete and we can use it
        if(.not.this % lIsComplete) then
            iRetCode = 1
            return
        end if
        
        ! Check parameters
        if(iHalving < 0 .or. iHalving > size(this % rmData, dim=2) - 1) then
            iRetCode = 2
            return
        end if
        
        ! Reserve workspace
        if(allocated(rvApproxSignal)) deallocate(rvApproxSignal)
        allocate(rvApproxSignal(size(this % rmData, dim=1)))
        
        ! Build the approximation
        do iLine = 1, size(this % rmData, dim=1)
            rvApproxSignal(iLine) = 0.
            do i = 1, iHalving + 1
                rvApproxSignal(iLine) = rvApproxSignal(iLine) + this % rmData(iLine, i)
            end do
        end do
        
    end function sg_approximate


    function sg_get_times(this, rvDeltaTime) result(iRetCode)
        
        ! Routine arguments
        class(signal), intent(in)                       :: this
        real(8), dimension(:), allocatable, intent(out) :: rvDeltaTime
        integer                                         :: iRetCode
        
        ! Locals
        real(8)     :: rDeltaTime
        integer     :: iNumHalvings
        integer     :: i
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Check something can really be made
        if(.not. this % lIsComplete) then
            iRetCode = 1
            return
        end if
        
        ! Compute number of halvings, and the overall time interval length
        rDeltaTime   = this % rvTimeStamp(size(this % rvTimeStamp)) - this % rvTimeStamp(1)
        iNumHalvings = size(this % rmData, dim=2)
        
        ! Reserve workspace
        if(allocated(rvDeltaTime)) deallocate(rvDeltaTime)
        allocate(rvDeltaTime(iNumHalvings))
        
        ! Set delta times as appropriate
        rvDeltaTime(1) = maxval(this % rvTimeStamp) - minval(this % rvTimeStamp)
        do i = 2, iNumHalvings
            rvDeltaTime(i) = rvDeltaTime(i-1) / 2.d0
        end do
    
    end function sg_get_times
    
    
    function sg_get_variances(this, rOriginalVariance, rvVariance, rResidualVariance) result(iRetCode)
    
        ! Routine arguments
        class(signal), intent(in)                       :: this
        real, intent(out)                               :: rOriginalVariance    ! Total variance of original signal
        real, dimension(:), allocatable, intent(out)    :: rvVariance           ! Variances of the various halvings
        real, intent(out)                               :: rResidualVariance    ! Variance of final residual
        integer                                         :: iRetCode
        
        ! Locals
        ! --none--
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Check something can be done
        if(.not. this % lIsComplete) then
            iRetCode = 1
            return
        end if
        
        ! Reserve workspace
        if(allocated(rvVariance)) deallocate(rvVariance)
        allocate(rvVariance(size(this % rvVariance) - 1))
        
        ! Get the information desired
        rvVariance = this % rvVariance(1:size(this % rvVariance) - 1)
        rOriginalVariance = rvVariance(1)
        rResidualVariance = this % rvVariance(size(this % rvVariance))
        rvVariance(1) = 0.
        
    end function sg_get_variances
    
    
    function sg_get_total_variation(this, rOriginalTotVar, rvTotVar, rResidualTotVar) result(iRetCode)
    
        ! Routine arguments
        class(signal), intent(in)                       :: this
        real, intent(out)                               :: rOriginalTotVar      ! Total variation of original signal
        real, dimension(:), allocatable, intent(out)    :: rvTotVar             ! Total variation of the various halvings
        real, intent(out)                               :: rResidualTotVar      ! Total variation of final residual
        integer                                         :: iRetCode
        
        ! Locals
        integer :: m
        integer :: n
        integer :: i
        integer :: iHalving
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Check something can be done
        if(.not. this % lIsComplete) then
            iRetCode = 1
            return
        end if
        
        ! Reserve workspace
        m = size(this % rmData, dim=1)
        n = size(this % rvVariance) - 1
        if(allocated(rvTotVar)) deallocate(rvTotVar)
        allocate(rvTotVar(n))
        
        ! Get the information desired
        rOriginalTotVar = 0.0
        do i = 1, m - 1
            rOriginalTotVar = rOriginalTotVar + abs(this % rvData(i+1) - this % rvData(i))
        end do
        rvTotVar(1) = 0.
        do iHalving = 2, n
            rvTotVar(iHalving) = 0.
            do i = 1, m
                rvTotVar(iHalving) = rvTotVar(iHalving) + abs(this % rmData(i+1, iHalving) - this % rmData(i, iHalving))
            end do
        end do
        rResidualTotVar = 0.0
        do i = 1, m - 1
            rResidualTotVar = rResidualTotVar + abs(this % rvResidual(i+1) - this % rvResidual(i))
        end do
        
    end function sg_get_total_variation
    
    
    function sg_get_partial_variation(this, rvPartVar) result(iRetCode)
    
        ! Routine arguments
        class(signal), intent(in)                       :: this
        real, dimension(:), allocatable, intent(out)    :: rvPartVar             ! Partial variation of the various halvings
        integer                                         :: iRetCode
        
        ! Locals
        integer :: m
        integer :: n
        integer :: i
        integer :: iHalving
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Check something can be done
        if(.not. this % lIsComplete) then
            iRetCode = 1
            return
        end if
        
        ! Reserve workspace
        m = size(this % rmData, dim=1)
        n = size(this % rmData, dim=2)
        if(allocated(rvPartVar)) deallocate(rvPartVar)
        allocate(rvPartVar(n))
        
        ! Get the information desired
        rvPartVar(1) = 0.
        do iHalving = 2, n
            rvPartVar(iHalving) = 0.
            do i = 1, m
                if(this % rmData(i+1, iHalving) /= this % rmData(i, iHalving)) then
                    rvPartVar(iHalving) = max(rvPartVar(iHalving), abs(this % rmData(i+1, iHalving) - this % rmData(i, iHalving)))
                end if
            end do
        end do
        
    end function sg_get_partial_variation
    
end module multires

