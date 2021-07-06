! 'shakeup' - Module, supporting data acquisition from SHAKEUP stations
module shakeup

    use pbl_met

    implicit none

    private

    ! Public interface
    public  :: DataSet

    ! Data types
    type DataSet
        real                                :: lat
        real                                :: lon
        integer                             :: zone
        real                                :: alt
        real(8), dimension(:), allocatable  :: time_stamp_begin
        real(8), dimension(:), allocatable  :: time_stamp_end
        real(8), dimension(:), allocatable  :: Vel
        real(8), dimension(:), allocatable  :: Temp
        real(8), dimension(:), allocatable  :: Hrel
        real(8), dimension(:), allocatable  :: Pa
        real(8), dimension(:), allocatable  :: Rg
        real(8), dimension(:), allocatable  :: Rn
        real(8), dimension(:), allocatable  :: ustar
        real(8), dimension(:), allocatable  :: H0
        real(8), dimension(:), allocatable  :: L
    contains
        procedure   :: read
    end type DataSet

contains

    function read(this, sDataPath, sStationName, sAvgPeriod) result(iRetCode)

        ! Routine arguments
        class(DataSet), intent(out)     :: this
        character(len=256), intent(in)  :: sDataPath
        character(len=256), intent(in)  :: sStationName
        character(len=1), intent(in)    :: sAvgPeriod
        integer                         :: iRetCode

        ! Locals
        character(len=256), dimension(:), allocatable   :: svStationName
        real, dimension(:), allocatable                 :: rvLat
        real, dimension(:), allocatable                 :: rvLon
        integer, dimension(:), allocatable              :: ivZone
        real, dimension(:), allocatable                 :: rvAlt
        integer, dimension(:), allocatable              :: ivIdxVel
        integer, dimension(:), allocatable              :: ivIdxTemp
        integer, dimension(:), allocatable              :: ivIdxHrel
        integer, dimension(:), allocatable              :: ivIdxPa
        integer, dimension(:), allocatable              :: ivIdxRg
        integer, dimension(:), allocatable              :: ivIdxRn
        integer, dimension(:), allocatable              :: ivIdxUstar
        integer, dimension(:), allocatable              :: ivIdxH0
        integer, dimension(:), allocatable              :: ivIdxZOverL
        real(8), dimension(:), allocatable              :: rvTimeVel
        real(8), dimension(:), allocatable              :: rvTimeTemp
        real(8), dimension(:), allocatable              :: rvTimeHrel
        real(8), dimension(:), allocatable              :: rvTimePa
        real(8), dimension(:), allocatable              :: rvTimeRg
        real(8), dimension(:), allocatable              :: rvTimeRn
        real(8), dimension(:), allocatable              :: rvTimeUstar
        real(8), dimension(:), allocatable              :: rvTimeH0
        real(8), dimension(:), allocatable              :: rvTimeZoverL
        real(8), dimension(:), allocatable              :: rvVel
        real(8), dimension(:), allocatable              :: rvTemp
        real(8), dimension(:), allocatable              :: rvHrel
        real(8), dimension(:), allocatable              :: rvPa
        real(8), dimension(:), allocatable              :: rvRg
        real(8), dimension(:), allocatable              :: rvRn
        real(8), dimension(:), allocatable              :: rvUstar
        real(8), dimension(:), allocatable              :: rvH0
        real(8), dimension(:), allocatable              :: rvZoverL
        character(len=256)                              :: sBuffer
        integer                                         :: iLUN
        integer                                         :: iErrCode
        integer                                         :: iStation
        integer                                         :: iNumStations
        integer                                         :: iPos
        integer                                         :: iFill
        integer                                         :: iStationIdx
        real(8), dimension(7)                           :: rvTimeStamps
        integer                                         :: n
        integer                                         :: i
        logical                                         :: lEqualTimeStamps
        real(8)                                         :: rTimeDelta

        ! Constants
        character(len=256), parameter   :: sStationsFile = "./shakeup_stations.csv"

        ! Assume success (will falsify on failure)
        iRetCode = 0

        ! Get station list from configuration file
        open(newunit=iLUN, file=sStationsFile, status='old', action='read', iostat=iErrCode)
        if(iErrCode /= 0) then
            iRetCode = 1
            return
        end if
        iNumStations = -1   ! Skip header line implicitly
        do
            read(iLUN, "(a)", iostat=iErrCode) sBuffer
            if(iErrCode /= 0) exit
            iNumStations = iNumStations + 1
        end do
        if(iNumStations <= 0) then
            close(iLUN)
            iRetCode = 2
            return
        end if
        allocate(svStationName(iNumStations))
        allocate(rvLat(iNumStations))
        allocate(rvLon(iNumStations))
        allocate(ivZone(iNumStations))
        allocate(rvAlt(iNumStations))
        allocate(ivIdxVel(iNumStations))
        allocate(ivIdxTemp(iNumStations))
        allocate(ivIdxHrel(iNumStations))
        allocate(ivIdxPa(iNumStations))
        allocate(ivIdxRg(iNumStations))
        allocate(ivIdxRn(iNumStations))
        allocate(ivIdxUstar(iNumStations))
        allocate(ivIdxH0(iNumStations))
        allocate(ivIdxZOverL(iNumStations))
        rewind(iLUN)
        read(iLUN, "(a)") sBuffer
        ! Data indexing in header:
        ! Temp,RelH,Prec,Rg,Rn,Vel_Conv,Dir_Conv,Vel_Scalar,Dir_Unit,Vel_Sonic,Dir_Sonic,Temp_Sonic,Ustar,H0,z_over_L,Tstar,Sigma_U,Sigma_V,Sigma_W,Sigma_T,TKE,T_Surface
        ! 1    2    3    4  5  6        7        8          9        10        11        12         13    14 15       16    17      18      19      20      21  22
        do iStation = 1, iNumStations
            read(iLUN, "(a)") sBuffer
            iPos = index(sBuffer, ",")
            svStationName(iStation) = sBuffer(:iPos-1)
            read(sBuffer(iPos+1:), *) &
                rvLon(iStation), rvLat(iStation), ivZone(iStation), rvAlt(iStation), &
                ivIdxTemp(iStation), &  !  1
                ivIdxHrel(iStation), &  !  2
                iFill, &                !  3
                ivIdxRg(iStation), &    !  4
                ivIdxRn(iStation), &    !  5
                iFill, &                !  6
                iFill, &                !  7
                iFill, &                !  8
                iFill, &                !  9
                ivIdxVel(iStation), &   ! 10
                iFill, &                ! 11
                iFill, &                ! 12
                ivIdxUstar(iStation), & ! 13
                ivIdxH0(iStation), &    ! 14
                ivIdxZOverL(iStation)   ! 15
        end do
        close(iLUN)

        ! Lookup desired station by name
        iStationIdx = 0
        do iStation = 1, iNumStations
            if(svStationName(iStation) == sStationName) then
                iStationIdx = iStation
                exit
            end if
        end do
        if(iStationIdx <= 0) then
            iRetCode = 3
            return
        end if

        ! Get the desired data
        iErrCode = get_data(sDataPath, sAvgPeriod, ivIdxVel(iStationIdx), rvTimeVel, rvVel)
        if(iErrCode /= 0) then
            iRetCode = 4
            return
        end if
        iErrCode = get_data(sDataPath, sAvgPeriod, ivIdxTemp(iStationIdx), rvTimeTemp, rvTemp)
        if(iErrCode /= 0) then
            iRetCode = 5
            return
        end if
        iErrCode = get_data(sDataPath, sAvgPeriod, ivIdxHrel(iStationIdx), rvTimeHrel, rvHrel)
        if(iErrCode /= 0) then
            iRetCode = 6
            return
        end if
        iErrCode = get_data(sDataPath, sAvgPeriod, ivIdxRg(iStationIdx), rvTimeRg, rvRg)
        if(iErrCode /= 0) then
            iRetCode = 6
            return
        end if
        iErrCode = get_data(sDataPath, sAvgPeriod, ivIdxRn(iStationIdx), rvTimeRn, rvRn)
        if(iErrCode /= 0) then
            iRetCode = 7
            return
        end if
        iErrCode = get_data(sDataPath, sAvgPeriod, ivIdxUstar(iStationIdx), rvTimeUstar, rvUstar, 0.d0)
        if(iErrCode /= 0) then
            iRetCode = 8
            return
        end if
        iErrCode = get_data(sDataPath, sAvgPeriod, ivIdxH0(iStationIdx), rvTimeH0, rvH0, 0.d0)
        if(iErrCode /= 0) then
            iRetCode = 9
            return
        end if
        iErrCode = get_data(sDataPath, sAvgPeriod, ivIdxZoverL(iStationIdx), rvTimeZoverL, rvZoverL)
        if(iErrCode /= 0) then
            iRetCode = 10
            return
        end if

        ! Check all the time stamp vectors coincide (they should)
        n = size(rvTimeUstar)
        if( &
            size(rvTimeVel) /= n .or. &
            size(rvTimeTemp) /= n .or. &
            size(rvTimeRg) /= n .or. &
            size(rvTimeRn) /= n .or. &
            size(rvTimeH0) /= n .or. &
            size(rvTimeZoverL) /= n &
        ) then
            iRetCode = 11
            return
        end if
        lEqualTimeStamps = .true.
        do i = 1, n
            rvTimeStamps = [ &
                rvTimeVel(i), &
                rvTimeTemp(i), &
                rvTimeRg(i), &
                rvTimeRn(i), &
                rvTimeUstar(i), &
                rvTimeH0(i), &
                rvTimeZoverL(i) &
            ]
            if(abs(maxval(rvTimeStamps) - minval(rvTimeStamps)) > 1.d-1 ) then
                lEqualTimeStamps = .false.
                exit
            end if
        end do
        if(.not.lEqualTimeStamps) then
            iRetCode = 12
            return
        end if

        ! Estimate pressure
        allocate(rvTimePa(n))
        allocate(rvPa(n))
        rvTimePa = rvTimeUstar
        do i = 1, n
            rvPa(i) = AirPressure( &
                rvAlt(iStationIdx), &
                real(rvTemp(i), kind=4), &
                10.0, &
                ASCE_MEANTEMPERATURE &
            )
        end do

        ! Ustar-specific processing
        where(rvUstar <= 0.05)
            rvUstar = 0.05
        endwhere

        ! Save data to data set
        this % lat  = rvLat(iStationIdx)
        this % lon  = rvLon(iStationIdx)
        this % zone = ivZone(iStationIdx)
        this % alt  = rvAlt(iStationIdx)
        if(sAvgPeriod == 'H') then
            rTimeDelta = 3600.d0
        else
            rTimeDelta =  600.d0
        end if
        if(allocated(this % time_stamp_begin)) deallocate(this % time_stamp_begin)
        if(allocated(this % time_stamp_end))   deallocate(this % time_stamp_end)
        if(allocated(this % Vel))              deallocate(this % Vel)
        if(allocated(this % Temp))             deallocate(this % Temp)
        if(allocated(this % Hrel))             deallocate(this % Hrel)
        if(allocated(this % Pa))               deallocate(this % Pa)
        if(allocated(this % Rg))               deallocate(this % Rg)
        if(allocated(this % Rn))               deallocate(this % Rn)
        if(allocated(this % ustar))            deallocate(this % ustar)
        if(allocated(this % H0))               deallocate(this % H0)
        if(allocated(this % L))                deallocate(this % L)
        allocate(this % time_stamp_begin(n))
        allocate(this % time_stamp_end(n))
        allocate(this % Vel(n))
        allocate(this % Temp(n))
        allocate(this % Hrel(n))
        allocate(this % Pa(n))
        allocate(this % Rg(n))
        allocate(this % Rn(n))
        allocate(this % ustar(n))
        allocate(this % H0(n))
        allocate(this % L(n))
        this % time_stamp_begin = rvTimeUstar - rTimeDelta
        this % time_stamp_end   = rvTimeUstar
        this % Vel              = rvVel
        this % Temp             = rvTemp
        this % Hrel             = rvHrel
        this % Pa               = rvPa
        this % Rg               = rvRg
        this % Rn               = rvRn
        this % ustar            = rvUstar
        this % H0               = rvH0
        where(abs(rvZoverL) < 1.d-20)
            this % L            = NaN_8
        elsewhere
            this % L            = 10.d0 / rvZoverL
        endwhere

    end function read


    function get_data(sDataPath, sAvgPeriod, iDataIdx, rvDataTime, rvData, rQualityThreshold) result(iRetCode)

        ! Routine arguments
        character(len=256), intent(in)                      :: sDataPath
        character(len=1), intent(in)                        :: sAvgPeriod
        integer, intent(in)                                 :: iDataIdx
        real(8), dimension(:), allocatable, intent(out)     :: rvDataTime
        real(8), dimension(:), allocatable, intent(out)     :: rvData
        real(8), intent(in), optional                       :: rQualityThreshold
        integer                                             :: iRetCode

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
            read(sBuffer, *) rValue, rQuality
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

    end function get_data

end module shakeup
