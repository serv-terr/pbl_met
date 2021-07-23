! 'shakeup' - Module, supporting data acquisition from SHAKEUP stations
module shakeup

    use pbl_met

    implicit none

    private

    ! Public interface
    public  :: StationList

    ! Data types

    type StationList
        character(len=32), dimension(:), allocatable    :: svName
        real, dimension(:), allocatable                 :: rvLat
        real, dimension(:), allocatable                 :: rvLon
        integer, dimension(:), allocatable              :: ivZone
        real, dimension(:), allocatable                 :: rvAlt
        character(len=16), dimension(:), allocatable    :: svQuantity
        integer, dimension(:,:), allocatable            :: imQuantityIdx
        logical                                         :: lComplete = .false.
    contains
        procedure   :: Read              => StationsRead
        procedure   :: GetData           => StationsGetData
        procedure   :: GetQuantityIdx    => StationsGetQuantityIdx
        procedure   :: GetQuantitySeries => StationsGetQuantitySeries
    end type StationList

contains

    function StationsRead(this, sFileName) result(iRetCode)

        ! Routine arguments
        class(StationList), intent(out)     :: this
        character(len=*), intent(in)        :: sFileName
        integer                             :: iRetCode

        ! Locals
        integer     :: iLUN
        integer     :: iErrCode
        integer     :: iNumStations
        integer     :: iStation
        integer     :: jStation
        integer     :: iNumData
        integer     :: iData
        integer     :: jData
        integer     :: i
        character(len=256)                              :: sBuffer
        character(len=16), dimension(:), allocatable    :: svFieldNames
        character(len=16), dimension(:), allocatable    :: svField

        ! Assume success (will falsify on failure)
        iRetCode = 0
        this % lComplete = .false.

        ! Get file header
        open(newunit=iLUN, file=sFileName, status='old', action='read', iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 1
            return
        end if
        read(iLUN, "(a)", iostat=iErrCode) sBuffer
        if(iErrCode /= 0) then
            close(iLUN)
            iRetCode = 2
            return
        end if
        iErrCode = splitString(sBuffer, svFieldNames)
        if(iErrCode /= 0) then
            close(iLUN)
            iRetCode = 3
            return
        end if
        if(size(svFieldNames) < 6) then
            close(iLUN)
            iRetCode = 4
            return
        end if

        ! Adjust fields to left, to prevent ambiguities
        do i = 1, size(svFieldNames)
            svFieldNames(i) = adjustl(svFieldNames(i))
        end do

        ! Save header data
        iNumData = size(svFieldNames) - 5
        if(allocated(this % svQuantity)) deallocate(this % svQuantity)
        allocate(this % svQuantity(iNumData))
        this % svQuantity = svFieldNames(6:)

        ! Count stations and allocate workspace
        iNumStations = 0
        do
            read(iLUN, "(a)", iostat=iErrCode) sBuffer
            if(iErrCode /= 0) exit
            iNumStations = iNumStations + 1
        end do
        if(iNumStations <= 0) then
            iRetCode = 5
            close(iLUN)
            return
        end if
        if(allocated(this % svName))         deallocate(this % svName)
        if(allocated(this % rvLon))          deallocate(this % rvLon)
        if(allocated(this % rvLat))          deallocate(this % rvLat)
        if(allocated(this % ivZone))         deallocate(this % ivZone)
        if(allocated(this % rvAlt))          deallocate(this % rvAlt)
        if(allocated(this % imQuantityIdx))  deallocate(this % imQuantityIdx)
        allocate(this % svName(iNumStations))
        allocate(this % rvLon(iNumStations))
        allocate(this % rvLat(iNumStations))
        allocate(this % ivZone(iNumStations))
        allocate(this % rvAlt(iNumStations))
        allocate(this % imQuantityIdx(iNumData, iNumStations))

        ! Read stations data
        rewind(iLUN)
        read(iLUN, "(a)") sBuffer   ! Skip header
        do iStation = 1, iNumStations
            read(iLUN, "(a)") sBuffer
            iErrCode = splitString(sBuffer, svField)
            if(iErrCode /= 0) then
                close(iLUN)
                iRetCode = 6
                return
            end if
            if(size(svField) /= iNumData + 5) then
                close(iLUN)
                iRetCode = 7
                return
            end if
            this % svName(iStation) = svField(1)
            read(svField(2), *, iostat=iErrCode) this % rvLon(iStation)
            if(iErrCode /= 0) then
                close(iLUN)
                iRetCode = 8
                return
            end if
            read(svField(3), *, iostat=iErrCode) this % rvLat(iStation)
            if(iErrCode /= 0) then
                close(iLUN)
                iRetCode = 9
                return
            end if
            read(svField(4), *, iostat=iErrCode) this % ivZone(iStation)
            if(iErrCode /= 0) then
                close(iLUN)
                iRetCode = 8
                return
            end if
            read(svField(5), *, iostat=iErrCode) this % rvAlt(iStation)
            if(iErrCode /= 0) then
                close(iLUN)
                iRetCode = 9
                return
            end if
            do iData = 1, iNumData
                read(svField(iData + 5), *, iostat=iErrCode) this % imQuantityIdx(iData, iStation)
                if(iErrCode /= 0) then
                    close(iLUN)
                    iRetCode = 10
                    return
                end if
            end do
        end do

        ! Ensure no two stations have the same name, nor an empty name
        do iStation = 1, iNumStations
            if(this % svName(iStation) == ' ') then
                iRetCode = 11
                return
            end if
            do jStation = iStation+1, iNumStations
                if(this % svName(iStation) == this % svName(jStation)) then
                    iRetCode = 12
                    return
                end if
            end do
        end do

        ! Ensure no two sensor IDs are the same
        ! Note the little trick in 'jStation' loop: it starts on 'iStation', and
        ! not 'iStation+1', to force a check no two IDs are repeated _on the same station_.
        do iStation = 1, iNumStations
            do jStation = iStation, iNumStations
                do iData = 1, iNumData
                    do jData = 1, iNumData
                        if(this % imQuantityIdx(iData, iStation) == this % imQuantityIdx(jData, jStation)) then
                            iRetCode = 13
                            return
                        end if
                    end do
                end do
            end do
        end do

        ! Confirm the read was correct
        this % lComplete = .true.

    end function StationsRead


    function StationsGetData( &
        this, &
        sStationName, &
        rLon, &
        rLat, &
        iZone, &
        rAlt, &
        iStationIdxOut &
    ) result(iRetCode)

        ! Routime arguments
        class(StationList), intent(in)  :: this
        character(len=*), intent(in)    :: sStationName
        real, intent(out)               :: rLon
        real, intent(out)               :: rLat
        integer, intent(out)            :: iZone
        real, intent(out)               :: rAlt
        integer, intent(out), optional  :: iStationIdxOut
        integer                         :: iRetCode

        ! Locals
        integer     :: iErrCode
        integer     :: iStationIdx
        integer     :: i

        ! Assume success (will falsify on failure)
        iRetCode = 0

        ! Check the stations list is complete
        if(.not. this % lComplete) then
            iRetCode = 1
            return
        end if
        ! Post-condition: Yes, it is, and it makes sense to access its data

        ! Search station by name
        iStationIdx = 0
        do i = 1, size(this % svName)
            if(this % svName(i) == sStationName) then
                iStationIdx = i
                exit
            end if
        end do
        if(iStationIdx <= 0) then
            iRetCode = 2
            return
        end if
        ! Post-condition: Station found

        ! Gather data
        rAlt  = this % rvLat(iStationIdx)
        rLon  = this % rvLon(iStationIdx)
        iZone = this % ivZone(iStationIdx)
        rAlt  = this % rvAlt(iStationIdx)
        if(present(iStationIdxOut)) iStationIdxOut = iStationIdx
        
    end function StationsGetData


    function StationsGetQuantityIdx(this, sStationName, svQuantityNames, ivQuantityIdx) result(iRetCode)

        ! Routine arguments
        class(StationList), intent(in)                  :: this
        character(len=*), intent(in)                    :: sStationName
        character(len=*), dimension(:), intent(in)      :: svQuantityNames
        integer, dimension(:), allocatable, intent(out) :: ivQuantityIdx
        integer                                         :: iRetCode

        ! Locals
        integer :: iErrCode
        integer :: i
        integer :: j
        integer :: iStationIdx
        integer, dimension(:), allocatable  :: ivQuantityPos

        ! Assue success (will falsify on failure)
        iRetCode = 0

        ! Check the stations list is complete
        if(.not. this % lComplete) then
            iRetCode = 1
            return
        end if
        ! Post-condition: Yes, it is, and it makes sense to access its data

        ! Search station by name
        iStationIdx = 0
        do i = 1, size(this % svName)
            if(this % svName(i) == sStationName) then
                iStationIdx = i
                exit
            end if
        end do
        if(iStationIdx <= 0) then
            iRetCode = 2
            return
        end if
        ! Post-condition: Station found

        ! Check which indices do quantities have
        allocate(ivQuantityPos(size(svQuantityNames)))
        do i = 1, size(svQuantityNames)
            ivQuantityPos(i) = 0
            do j = 1, size(this % svQuantity)
                if(svQuantityNames(i) == this % svQuantity(j)) then
                    ivQuantityPos(i) = j
                    exit
                end if
            end do
            if(ivQuantityPos(i) <= 0) then
                iRetCode = 3
                return
            end if
        end do
        ! Post-condition: All variables found

        ! Get actual quantity indices
        if(allocated(ivQuantityIdx)) deallocate(ivQuantityIdx)
        allocate(ivQuantityIdx(size(svQuantityNames)))
        do i = 1, size(svQuantityNames)
            ivQuantityIdx(i) = this % imQuantityIdx(ivQuantityPos(i), iStationIdx)
        end do

    end function StationsGetQuantityIdx


    function StationsGetQuantitySeries( &
        this, &
        sDataPath, &
        sStationName, &
        sAvgPeriod, &
        svQuantityNames, &
        rvQualityThreshold, &
        tMultiSeries &
    ) result(iRetCode)

        ! Routine arguments
        class(StationList), intent(in)                  :: this
        character(len=*), intent(in)                    :: sDataPath
        character(len=*), intent(in)                    :: sStationName
        character(len=*), dimension(:), intent(in)      :: svQuantityNames
        real(8), dimension(:), intent(in)               :: rvQualityThreshold
        character(len=*), intent(in)                    :: sAvgPeriod
        type(MultiSeries), intent(out)                  :: tMultiSeries
        integer                                         :: iRetCode

        ! Locals
        integer :: iErrCode
        integer :: iData
        integer :: i
        integer :: j
        integer :: n
        integer :: iStationIdx
        integer, dimension(:), allocatable  :: ivQuantityPos
        integer, dimension(:), allocatable  :: ivQuantityIdx
        type(TimeSeries)                    :: tSeries

        ! Assue success (will falsify on failure)
        iRetCode = 0

        ! Check parameters
        if(sDataPath == ' ' .or. sStationName == ' ') then
            iRetCode = 1
            return
        end if
        n = size(svQuantityNames)
        if(size(rvQualityThreshold) /= n) then
            iRetCode = 2
            return
        end if
        if(minval(rvQualityThreshold) < 0.d0 .or. maxval(rvQualityThreshold) > 1000.d0) then
            iRetCode = 3
            return
        end if

        ! Check the stations list is complete
        if(.not. this % lComplete) then
            iRetCode = 4
            return
        end if
        ! Post-condition: Yes, it is, and it makes sense to access its data

        ! Search station by name
        iStationIdx = 0
        do i = 1, size(this % svName)
            if(this % svName(i) == sStationName) then
                iStationIdx = i
                exit
            end if
        end do
        if(iStationIdx <= 0) then
            iRetCode = 5
            return
        end if
        ! Post-condition: Station found

        ! Check which indices do quantities have
        allocate(ivQuantityPos(size(svQuantityNames)))
        do i = 1, size(svQuantityNames)
            ivQuantityPos(i) = 0
            do j = 1, size(this % svQuantity)
                if(svQuantityNames(i) == this % svQuantity(j)) then
                    ivQuantityPos(i) = j
                    exit
                end if
            end do
            if(ivQuantityPos(i) <= 0) then
                iRetCode = 6
                return
            end if
        end do
        ! Post-condition: All variables found

        ! Get actual quantity indices
        if(allocated(ivQuantityIdx)) deallocate(ivQuantityIdx)
        allocate(ivQuantityIdx(size(svQuantityNames)))
        do i = 1, size(svQuantityNames)
            ivQuantityIdx(i) = this % imQuantityIdx(ivQuantityPos(i), iStationIdx)
        end do

        ! Get first series (it must exist), and compose the multivariate time series
        ! starting from it
        iErrCode = get_data(sDataPath, sAvgPeriod, ivQuantityIdx(1), tSeries, rvQualityThreshold(1))
        if(iErrCode /= 0) then
            iRetCode = 7
            return
        end if
        iErrCode = tMultiSeries % CreateEmpty()
        if(iErrCode /= 0) then
            iRetCode = 8
            return
        end if
        iErrCode = tMultiSeries % addTimeSeries(svQuantityNames(1), tSeries)
        if(iErrCode /= 0) then
            iRetCode = 9
            return
        end if

        ! Get all other series, if any
        do iData = 2, size(svQuantityNames)

            iErrCode = get_data(sDataPath, sAvgPeriod, ivQuantityIdx(iData), tSeries, rvQualityThreshold(iData))
            if(iErrCode /= 0) then
                iRetCode = 10
                return
            end if

            iErrCode = tMultiSeries % addTimeSeries(svQuantityNames(iData), tSeries)
            if(iErrCode /= 0) then
                iRetCode = 11
                return
            end if
    
        end do

    end function StationsGetQuantitySeries

    ! *********************
    ! * Internal routines *
    ! *********************

    function get_data(sDataPath, sAvgPeriod, iDataIdx, tSeries, rQualityThreshold) result(iRetCode)

        ! Routine arguments
        character(len=256), intent(in)      :: sDataPath
        character(len=1), intent(in)        :: sAvgPeriod
        integer, intent(in)                 :: iDataIdx
        type(TimeSeries), intent(out)       :: tseries
        real(8), intent(in), optional       :: rQualityThreshold
        integer                             :: iRetCode

        ! Locals
        integer             :: iErrCode
        integer             :: iLUN
        character(len=256)  :: sFileName
        character(len=8)    :: sFileNumber
        integer             :: iNumData
        integer             :: iData
        character(len=256)  :: sBuffer
        integer             :: i
        logical             :: lFileExists
        character(len=19)   :: sDateTime
        real                :: rValue
        real                :: rQuality
        integer             :: iPos
        type(DateTime)      :: tDateTime
        integer             :: iYear, iMonth, iDay, iHour, iMinute, iSecond
        real(8)             :: rMinQuality
        logical             :: lDisableQuality
        real(8), dimension(:), allocatable  :: rvDataTime
        real, dimension(:), allocatable     :: rvData

        ! Assume success (will falsify on failure)
        iRetCode = 0

        ! Set quality threshold
        if(present(rQualityThreshold)) then
            if(rQualityThreshold <= 0.d0) then
                rMinQuality = 0.d0
                lDisableQuality = .true.
            else
                rMinQuality = rQualityThreshold
                lDisableQuality = .false.
            end if
        else
            rMinQuality = 75.
            lDisableQuality = .false.
        end if

        ! Build data file name
        write(sFileNumber, "(i8)") iDataIdx
        do i = 1, 9
            write(sFileName, "(a,'/',a,'_',a,'_',i1,'.txt')") trim(sDataPath), trim(adjustl(sFileNumber)), trim(sAvgPeriod), i
            inquire(file=sFileName, exist=lFileExists)
            if(lFileExists) exit
        end do
        if(.not.lFileExists) then
            iRetCode = 1
            return
        end if

        ! Access data
        open(newunit=iLUN, file=sFileName, status='old', action='read', iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 2
            return
        end if
        iNumData = 0
        do
            read(iLUN, "(a)", iostat=iErrCode) sBuffer
            if(iErrCode /= 0) exit
            iNumData = iNumData + 1
        end do
        if(iNumData <= 0) then
            close(iLUN)
            iRetCode = 3
            return
        end if
        allocate(rvDataTime(iNumData))
        allocate(rvData(iNumData))
        rewind(iLUN)
        do iData = 1, iNumData

            ! Get data line
            read(iLUN, "(a)") sBuffer

            ! Change all TABs into commas (not strictly necessary, but helpful if not to understand)
            do i = 1, len_trim(sBuffer)
                if(sBuffer(i:i) == char(9)) sBuffer(i:i) = ','
            end do

            ! Locate the first item in data line, and clear it: it's "just" the sensor ID
            iPos = index(sBuffer, ',')
            sBuffer = sBuffer(iPos+1:)

            ! The second item, the time stamp, is fixed length, and can be dequeued
            ! from input string "by length"
            sDateTime = sBuffer(1:19)
            sBuffer = sBuffer(25:)
            read(sDateTime, "(i4,5(1x,i2))") iYear, iMonth, iDay, iHour, iMinute, iSecond
            tDateTime = DateTime(iYear, iMonth, iDay, iHour, iMinute, dble(iSecond))

            ! Anything else on data line is "just a number", and may be decoded in free form
            iPos = index(sBuffer, ",")
            if(len_trim(sBuffer) > iPos + 1) then
                read(sBuffer, *) rValue, rQuality
            else
                read(sBuffer, *) rValue
                rQuality = -999.9
            end if
            if(lDisableQuality) then
                if(rValue < -990.0) rValue = NaN
            else
                if(rValue < -990.0 .or. rQuality < rMinQuality) rValue = NaN
            end if

            ! Convert date and time to epoch form, 'a la pbl_met
            rvDataTime(iData) = tDateTime % toEpoch(CLP_SECOND)

            ! Save data
            rvData(iData) = rValue

        end do
        close(iLUN)

        ! Create time series
        iErrCode = tSeries % createFromTimeAndDataVectors(rvDataTime, rvData)
        if(iErrCode /= 0) then
            iRetCode = 4
            return
        end if

    end function get_data

end module shakeup
