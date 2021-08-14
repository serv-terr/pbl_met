! Unit test program for gap fillers
program test_gap_filling

    use pbl_met

    implicit none

    ! Locals
    real, dimension(:), allocatable         :: rvData
    real(8), dimension(:), allocatable      :: rvTimeStamp
    logical, dimension(:), allocatable      :: lvOriginal
    type(DateTime)                          :: tDateTime
    type(TimeSeries)                        :: tOriginalTimeSeries
    type(TimeSeries)                        :: tProcessedTimeSeries
    integer                                 :: iRetCode
    integer                                 :: n
    integer                                 :: i
    real(8)                                 :: rTimeStamp1
    real                                    :: rValue1
    real(8)                                 :: rTimeStamp2
    real                                    :: rValue2

    ! Initialize year 2021
    n = 8760
    allocate(rvData(n))
    allocate(rvTimeStamp(n))
    tDateTime = DateTime(2021, 1, 1, 0, 0, 0.d0)
    rvTimeStamp(1) = tDateTime % toEpoch()
    do i = 2, n
        rvTimeStamp(i) = 3600.d0 * (i-1) + rvTimeStamp(1)
    end do

    ! Test 1: Some gaps, on a random signal with mean 0.0

    ! Initialize data: no gaps for the moment
    call random_number(rvData)
    rvData = rvData - 0.5

    ! Set some values to invalid
    rvData(12:16) = NaN

    ! Use vectors to form time series
    iRetCode = tOriginalTimeSeries % createFromTimeAndDataVectors(rvTimeStamp, rvData)
    if(iRetCode /= 0) then
        print *, 'Time series 1 not created. Return code = ', iRetCode
        stop
    end if

    ! Gnerate processed series
    iRetCode = tProcessedTimeSeries % createFromTimeAndDataVectors(rvTimeStamp, rvData)
    if(iRetCode /= 0) then
        print *, 'Time series 2 not created. Return code = ', iRetCode
        stop
    end if

    ! Fill gaps
    iRetCode = tProcessedTimeSeries % FillGaps(30, lvOriginal)
    if(iRetCode /= 0) then
        print *, 'Gaps not filled. Return code = ', iRetCode
        stop
    end if

    ! Compare data on day 1
    print *
    print *, 'Test 1,DeltaTime,Original,Processed'
    do i = 1, 24
        iRetCode = tOriginalTimeSeries % getSingleItem(i, rTimeStamp1, rValue1)
        iRetCode = tProcessedTimeSeries % getSingleItem(i, rTimeStamp2, rValue2)
        print *, i-1, abs(rTimeStamp2 - rTimeStamp1), rValue1, rValue2
    end do
    
    ! Test 2: Some gaps, on a random signal with mean 0.0; and, a systematic gap on hour 14 of each day

    ! Initialize data: no gaps for the moment
    call random_number(rvData)
    rvData = rvData - 0.5

    ! Set some values to invalid
    rvData(12:16) = NaN
    do i = 14, n, 24
        rvData(i) = NaN
    end do

    ! Use vectors to form time series
    iRetCode = tOriginalTimeSeries % createFromTimeAndDataVectors(rvTimeStamp, rvData)
    if(iRetCode /= 0) then
        print *, 'Time series 1 not created. Return code = ', iRetCode
        stop
    end if

    ! Gnerate processed series
    iRetCode = tProcessedTimeSeries % createFromTimeAndDataVectors(rvTimeStamp, rvData)
    if(iRetCode /= 0) then
        print *, 'Time series 2 not created. Return code = ', iRetCode
        stop
    end if

    ! Fill gaps
    iRetCode = tProcessedTimeSeries % FillGaps(30, lvOriginal)
    if(iRetCode /= 0) then
        print *, 'Gaps not filled. Return code = ', iRetCode
        stop
    end if

    ! Compare data on day 1
    print *
    print *, 'Test 2,DeltaTime,Original,Processed'
    do i = 1, 24
        iRetCode = tOriginalTimeSeries % getSingleItem(i, rTimeStamp1, rValue1)
        iRetCode = tProcessedTimeSeries % getSingleItem(i, rTimeStamp2, rValue2)
        print *, i-1, abs(rTimeStamp2 - rTimeStamp1), rValue1, rValue2
    end do
    
    ! Test 3: Some gaps, on a random signal with mean 0.0; and, a systematic gap on hours 13:15 of each day

    ! Initialize data: no gaps for the moment
    call random_number(rvData)
    rvData = rvData - 0.5

    ! Set some values to invalid
    rvData(12:16) = NaN
    do i = 13, n, 24
        rvData(i) = NaN
    end do
    do i = 14, n, 24
        rvData(i) = NaN
    end do
    do i = 15, n, 24
        rvData(i) = NaN
    end do

    ! Use vectors to form time series
    iRetCode = tOriginalTimeSeries % createFromTimeAndDataVectors(rvTimeStamp, rvData)
    if(iRetCode /= 0) then
        print *, 'Time series 1 not created. Return code = ', iRetCode
        stop
    end if

    ! Gnerate processed series
    iRetCode = tProcessedTimeSeries % createFromTimeAndDataVectors(rvTimeStamp, rvData)
    if(iRetCode /= 0) then
        print *, 'Time series 2 not created. Return code = ', iRetCode
        stop
    end if

    ! Fill gaps
    iRetCode = tProcessedTimeSeries % FillGaps(30, lvOriginal)
    if(iRetCode /= 0) then
        print *, 'Gaps not filled. Return code = ', iRetCode
        stop
    end if

    ! Compare data on day 1
    print *
    print *, 'Test 3,DeltaTime,Original,Processed'
    do i = 1, 24
        iRetCode = tOriginalTimeSeries % getSingleItem(i, rTimeStamp1, rValue1)
        iRetCode = tProcessedTimeSeries % getSingleItem(i, rTimeStamp2, rValue2)
        print *, i-1, abs(rTimeStamp2 - rTimeStamp1), rValue1, rValue2
    end do
    
    ! Test 4: Some gaps, on a random signal with mean 0.0; and, a systematic gap on hours 1:3 of each day

    ! Initialize data: no gaps for the moment
    call random_number(rvData)
    rvData = rvData - 0.5

    ! Set some values to invalid
    rvData(1:3) = NaN
    do i = 1, n, 24
        rvData(i) = NaN
    end do
    do i = 2, n, 24
        rvData(i) = NaN
    end do
    do i = 3, n, 24
        rvData(i) = NaN
    end do

    ! Use vectors to form time series
    iRetCode = tOriginalTimeSeries % createFromTimeAndDataVectors(rvTimeStamp, rvData)
    if(iRetCode /= 0) then
        print *, 'Time series 1 not created. Return code = ', iRetCode
        stop
    end if

    ! Gnerate processed series
    iRetCode = tProcessedTimeSeries % createFromTimeAndDataVectors(rvTimeStamp, rvData)
    if(iRetCode /= 0) then
        print *, 'Time series 2 not created. Return code = ', iRetCode
        stop
    end if

    ! Fill gaps
    iRetCode = tProcessedTimeSeries % FillGaps(30, lvOriginal)
    if(iRetCode /= 0) then
        print *, 'Gaps not filled. Return code = ', iRetCode
        stop
    end if

    ! Compare data on day 1
    print *
    print *, 'Test 4,DeltaTime,Original,Processed'
    do i = 1, 24
        iRetCode = tOriginalTimeSeries % getSingleItem(i, rTimeStamp1, rValue1)
        iRetCode = tProcessedTimeSeries % getSingleItem(i, rTimeStamp2, rValue2)
        print *, i-1, abs(rTimeStamp2 - rTimeStamp1), rValue1, rValue2
    end do
    
    ! Test 5: Some gaps, on a random signal with mean 0.0; and, a systematic gap on hours 22:24 of each day

    ! Initialize data: no gaps for the moment
    call random_number(rvData)
    rvData = rvData - 0.5

    ! Set some values to invalid
    rvData(22:24) = NaN
    do i = 22, n, 24
        rvData(i) = NaN
    end do
    do i = 23, n, 24
        rvData(i) = NaN
    end do
    do i = 24, n, 24
        rvData(i) = NaN
    end do

    ! Use vectors to form time series
    iRetCode = tOriginalTimeSeries % createFromTimeAndDataVectors(rvTimeStamp, rvData)
    if(iRetCode /= 0) then
        print *, 'Time series 1 not created. Return code = ', iRetCode
        stop
    end if

    ! Gnerate processed series
    iRetCode = tProcessedTimeSeries % createFromTimeAndDataVectors(rvTimeStamp, rvData)
    if(iRetCode /= 0) then
        print *, 'Time series 2 not created. Return code = ', iRetCode
        stop
    end if

    ! Fill gaps
    iRetCode = tProcessedTimeSeries % FillGaps(30, lvOriginal)
    if(iRetCode /= 0) then
        print *, 'Gaps not filled. Return code = ', iRetCode
        stop
    end if

    ! Compare data on day 1
    print *
    print *, 'Test 5,DeltaTime,Original,Processed'
    do i = 1, 24
        iRetCode = tOriginalTimeSeries % getSingleItem(i, rTimeStamp1, rValue1)
        iRetCode = tProcessedTimeSeries % getSingleItem(i, rTimeStamp2, rValue2)
        print *, i-1, abs(rTimeStamp2 - rTimeStamp1), rValue1, rValue2
    end do
    
    ! Test 6: All gaps

    ! Set some values to invalid
    rvData = NaN

    ! Use vectors to form time series
    iRetCode = tOriginalTimeSeries % createFromTimeAndDataVectors(rvTimeStamp, rvData)
    if(iRetCode /= 0) then
        print *, 'Time series 1 not created. Return code = ', iRetCode
        stop
    end if

    ! Gnerate processed series
    iRetCode = tProcessedTimeSeries % createFromTimeAndDataVectors(rvTimeStamp, rvData)
    if(iRetCode /= 0) then
        print *, 'Time series 2 not created. Return code = ', iRetCode
        stop
    end if

    ! Fill gaps
    iRetCode = tProcessedTimeSeries % FillGaps(30, lvOriginal)
    if(iRetCode /= 0) then
        print *, 'Test 6: no gaps to fill. Return code = ', iRetCode
    else
    ! Compare data on day 1
        print *
        print *, 'Test 6,DeltaTime,Original,Processed'
        do i = 1, 24
            iRetCode = tOriginalTimeSeries % getSingleItem(i, rTimeStamp1, rValue1)
            iRetCode = tProcessedTimeSeries % getSingleItem(i, rTimeStamp2, rValue2)
            print *, i-1, abs(rTimeStamp2 - rTimeStamp1), rValue1, rValue2
        end do
    end if
    
    ! Test 7: All gaps but one

    ! Initialize data: no gaps for the moment
    rvData = NaN
    rvData(1) = 1.0

    ! Use vectors to form time series
    iRetCode = tOriginalTimeSeries % createFromTimeAndDataVectors(rvTimeStamp, rvData)
    if(iRetCode /= 0) then
        print *, 'Time series 1 not created. Return code = ', iRetCode
        stop
    end if

    ! Gnerate processed series
    iRetCode = tProcessedTimeSeries % createFromTimeAndDataVectors(rvTimeStamp, rvData)
    if(iRetCode /= 0) then
        print *, 'Time series 2 not created. Return code = ', iRetCode
        stop
    end if

    ! Fill gaps
    iRetCode = tProcessedTimeSeries % FillGaps(30, lvOriginal)
    if(iRetCode /= 0) then
        print *, 'Gaps not filled. Return code = ', iRetCode
        stop
    end if

    ! Compare data on day 1
    print *
    print *, 'Test 7,DeltaTime,Original,Processed'
    do i = 1, 24
        iRetCode = tOriginalTimeSeries % getSingleItem(i, rTimeStamp1, rValue1)
        iRetCode = tProcessedTimeSeries % getSingleItem(i, rTimeStamp2, rValue2)
        print *, i-1, abs(rTimeStamp2 - rTimeStamp1), rValue1, rValue2
    end do
    
    ! Test 8: Some gaps again, on a random signal with mean 0.0; and, a systematic gap on hours 22:24 of each day

    ! Initialize data: no gaps for the moment
    call random_number(rvData)
    rvData = rvData - 0.5

    ! Set some values to invalid
    rvData(22:24) = NaN
    do i = 22, n, 24
        rvData(i) = NaN
    end do
    do i = 23, n, 24
        rvData(i) = NaN
    end do
    do i = 24, n, 24
        rvData(i) = NaN
    end do

    ! Use vectors to form time series
    iRetCode = tOriginalTimeSeries % createFromTimeAndDataVectors(rvTimeStamp, rvData)
    if(iRetCode /= 0) then
        print *, 'Time series 1 not created. Return code = ', iRetCode
        stop
    end if

    ! Gnerate processed series
    iRetCode = tProcessedTimeSeries % createFromTimeAndDataVectors(rvTimeStamp, rvData)
    if(iRetCode /= 0) then
        print *, 'Time series 2 not created. Return code = ', iRetCode
        stop
    end if

    ! Fill gaps
    iRetCode = tProcessedTimeSeries % FillGaps(30, lvOriginal)
    if(iRetCode /= 0) then
        print *, 'Gaps not filled. Return code = ', iRetCode
        stop
    end if

    ! Compare data on day 1
    print *
    print *, 'Test 8,DeltaTime,Original,Processed'
    do i = 1, 24
        iRetCode = tOriginalTimeSeries % getSingleItem(i, rTimeStamp1, rValue1)
        iRetCode = tProcessedTimeSeries % getSingleItem(i, rTimeStamp2, rValue2)
        print *, i-1, abs(rTimeStamp2 - rTimeStamp1), rValue1, rValue2, lvOriginal(i)
    end do
    
end program test_gap_filling
