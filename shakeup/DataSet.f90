module DataSet

    use shakeup
    use pbl_met

    implicit none

    private

    ! Public interface
    public  :: MetProDataSet
    public  :: ZiDataSet
    public  :: H0vsRnDataSet

    ! User defined types

    type MetProDataSet
        real                                :: lat
        real                                :: lon
        integer                             :: zone
        real                                :: alt
        real(8), dimension(:), allocatable  :: time_stamp_begin
        real(8), dimension(:), allocatable  :: time_stamp_end
        real(8), dimension(:), allocatable  :: Vel
        real(8), dimension(:), allocatable  :: Dir
        real(8), dimension(:), allocatable  :: Temp
        real(8), dimension(:), allocatable  :: RelH
        real(8), dimension(:), allocatable  :: ustar
        real(8), dimension(:), allocatable  :: H0
    contains
        procedure   :: Read    => mp_DataRead
    end type MetProDataSet

    type ZiDataSet
        real                                :: lat
        real                                :: lon
        integer                             :: zone
        real                                :: alt
        real(8), dimension(:), allocatable  :: time_stamp_begin
        real(8), dimension(:), allocatable  :: time_stamp_end
        real(8), dimension(:), allocatable  :: Vel
        real(8), dimension(:), allocatable  :: Temp
        real(8), dimension(:), allocatable  :: ustar
        real(8), dimension(:), allocatable  :: H0
    contains
        procedure   :: Read => zi_DataRead
    end type ZiDataSet

    type H0vsRnDataSet
        real(8), dimension(:), allocatable  :: time_stamp_begin
        real(8), dimension(:), allocatable  :: time_stamp_end
        real(8), dimension(:), allocatable  :: Rn
        real(8), dimension(:), allocatable  :: H0
    contains
        procedure   :: Read => H0vsRnDataRead
    end type H0vsRnDataSet

contains

    function mp_DataRead(this, sDataPath, sStationName, sAvgPeriod) result(iRetCode)

        ! Routine arguments
        class(MetProDataSet), intent(out)   :: this
        character(len=256), intent(in)      :: sDataPath
        character(len=256), intent(in)      :: sStationName
        character(len=1), intent(in)        :: sAvgPeriod
        integer                             :: iRetCode

        ! Locals
        type(StationList)                   :: tStations
        type(MultiSeries)                   :: tMultiSeries
        integer                             :: iErrCode
        real(8), dimension(:), allocatable  :: rvTimeStamp
        real(8)                             :: rTimeDelta

        ! Steering constants
        character(len=*), dimension(6), parameter   :: COL_NAME = [ &
            'Temp     ', &
            'RelH     ', &
            'Vel_Sonic', &
            'Dir_Sonic', &
            'Ustar    ', &
            'H0       ' &
        ]
        real(8), dimension(6), parameter            :: COL_THRESHOLD = [ &
            9.d0, &
            9.d0, &
            9.d0, &
            9.d0, &
            0.d0, &
            0.d0 &
        ]
        
        integer, dimension(6), parameter            :: COL_FILL = [ &
            FILL_LINEAR, &
            FILL_LINEAR, &
            FILL_LINEAR, &
            FILL_CIRCULAR, &
            FILL_LINEAR, &
            FILL_LINEAR  &
        ]

        ! Constants
        character(len=256), parameter   :: sStationsFile = "./shakeup_stations.csv"

        ! Assume success (will falsify on failure)
        iRetCode = 0

        ! Get station list from configuration file; no "intelligent" things are done here,
        ! only the reading of station data
        iErrCode = tStations % read(sStationsFile)
        if(iErrCode /= 0) then
            print *, "DataSet % Read:: Error: Station list read - Return code = ", iErrCode
            iRetCode = 1
            return
        end if
        ! Post-condition: at least one station contains valid data

        ! Get the position of desired station: if the attempt
        ! fails, we may stop here as the desired station does not exist.
        iErrCode = tStations % getData( &
            sStationName, &
            this % lon, &
            this % lat, &
            this % zone, &
            this % alt &
        )
        if(iErrCode /= 0) then
            print *, "DataSet % Read:: Error: Station data read - Return code = ", iErrCode
            iRetCode = 2
            return
        end if
        ! Post-condition: station exist, and its position is now known

        ! Get desired data, and compose a dataframe-like structure with them
        iErrCode = tStations % GetQuantitySeries( &
            sDataPath, &
            sStationName, &
            sAvgPeriod, &
            COL_NAME, &
            COL_THRESHOLD, &
            tMultiSeries &
        )
        if(iErrCode /= 0) then
            print *, "DataSet % Read:: Error: Desired data read - Return code = ", iErrCode
            iRetCode = 3
            return
        end if
        ! Post-condition: station data have been found, and their time stamp assigned

        ! Retrieve time stamp
        iErrCode = tMultiSeries % getTimeStamp(rvTimeStamp)
        if(iErrCode /= 0) then
            print *, "DataSet % Read:: Error: Time stamp not gathered - Return code = ", iErrCode
            iRetCode = 4
            return
        end if
        if(sAvgPeriod == 'H') then
            rTimeDelta = 3600.d0
        else
            rTimeDelta =  600.d0
        end if
        if(allocated(this % time_stamp_begin)) deallocate(this % time_stamp_begin)
        if(allocated(this % time_stamp_end))   deallocate(this % time_stamp_end)
        allocate(this % time_stamp_begin(size(rvTimeStamp)))
        allocate(this % time_stamp_end(size(rvTimeStamp)))
        this % time_stamp_begin = rvTimeStamp - rTimeDelta
        this % time_stamp_end   = rvTimeStamp
        
        ! Fill gaps
        iErrCode = tMultiSeries % fillGaps(COL_NAME, COL_FILL, 15)
        if(iErrCode /= 0) then
            print *, "DataSet % Read:: Error: Gaps not filled - Return code = ", iErrCode
            iRetCode = 5
            return
        end if

        ! Retrieve the four vectors
        iErrCode = tMultiSeries % getVector('Temp     ', this % temp)
        iErrCode = tMultiSeries % getVector('RelH     ', this % relh)
        iErrCode = tMultiSeries % getVector('Vel_Sonic', this % vel)
        iErrCode = tMultiSeries % getVector('Dir_Sonic', this % dir)
        iErrCode = tMultiSeries % getVector('Ustar    ', this % ustar)
        iErrCode = tMultiSeries % getVector('H0       ', this % H0)

        ! Ustar-specific processing
        where(this % ustar <= 0.05)
            this % ustar = 0.05
        endwhere

    end function mp_DataRead


    function zi_DataRead(this, sDataPath, sStationName, sAvgPeriod) result(iRetCode)

        ! Routine arguments
        class(ZiDataSet), intent(out)   :: this
        character(len=256), intent(in)  :: sDataPath
        character(len=256), intent(in)  :: sStationName
        character(len=1), intent(in)    :: sAvgPeriod
        integer                         :: iRetCode

        ! Locals
        type(StationList)                   :: tStations
        type(MultiSeries)                   :: tMultiSeries
        integer                             :: iErrCode
        real(8), dimension(:), allocatable  :: rvTimeStamp
        real(8)                             :: rTimeDelta

        ! Steering constants
        character(len=*), dimension(4), parameter   :: COL_NAME = [ &
            'Temp     ', &
            'Vel_Sonic', &
            'Ustar    ', &
            'H0       ' &
        ]
        real(8), dimension(4), parameter            :: COL_THRESHOLD = [ &
            9.d0, &
            9.d0, &
            0.d0, &
            0.d0 &
        ]

        ! Constants
        character(len=256), parameter   :: sStationsFile = "./shakeup_stations.csv"

        ! Assume success (will falsify on failure)
        iRetCode = 0

        ! Get station list from configuration file; no "intelligent" things are done here,
        ! only the reading of station data
        iErrCode = tStations % read(sStationsFile)
        if(iErrCode /= 0) then
            print *, "ZiDataSet % Read:: Error: Station list read - Return code = ", iErrCode
            iRetCode = 1
            return
        end if
        ! Post-condition: at least one station contains valid data

        ! Get the position of desired station: if the attempt
        ! fails, we may stop here as the desired station does not exist.
        iErrCode = tStations % getData( &
            sStationName, &
            this % lon, &
            this % lat, &
            this % zone, &
            this % alt &
        )
        if(iErrCode /= 0) then
            print *, "ZiDataSet % Read:: Error: Station data read - Return code = ", iErrCode
            iRetCode = 2
            return
        end if
        ! Post-condition: station exist, and its position is now known

        ! Get desired data, and compose a dataframe-like structure with them
        iErrCode = tStations % GetQuantitySeries( &
            sDataPath, &
            sStationName, &
            sAvgPeriod, &
            COL_NAME, &
            COL_THRESHOLD, &
            tMultiSeries &
        )
        if(iErrCode /= 0) then
            print *, "ZiDataSet % Read:: Error: Desired data read - Return code = ", iErrCode
            iRetCode = 3
            return
        end if
        ! Post-condition: station data have been found, and their time stamp assigned

        ! Retrieve time stamp
        iErrCode = tMultiSeries % getTimeStamp(rvTimeStamp)
        if(iErrCode /= 0) then
            print *, "ZiDataSet % Read:: Error: Time stamp not gathered - Return code = ", iErrCode
            iRetCode = 4
            return
        end if
        if(sAvgPeriod == 'H') then
            rTimeDelta = 3600.d0
        else
            rTimeDelta =  600.d0
        end if
        if(allocated(this % time_stamp_begin)) deallocate(this % time_stamp_begin)
        if(allocated(this % time_stamp_end))   deallocate(this % time_stamp_end)
        allocate(this % time_stamp_begin(size(rvTimeStamp)))
        allocate(this % time_stamp_end(size(rvTimeStamp)))
        this % time_stamp_begin = rvTimeStamp - rTimeDelta
        this % time_stamp_end   = rvTimeStamp

        ! Retrieve the four vectors
        iErrCode = tMultiSeries % getVector('Temp     ', this % temp)
        iErrCode = tMultiSeries % getVector('Vel_Sonic', this % vel)
        iErrCode = tMultiSeries % getVector('Ustar    ', this % ustar)
        iErrCode = tMultiSeries % getVector('H0       ', this % H0)

        ! Ustar-specific processing
        where(this % ustar <= 0.05)
            this % ustar = 0.05
        endwhere

    end function zi_DataRead


    function H0vsRnDataRead(this, sDataPath, sStationName, sAvgPeriod) result(iRetCode)

        ! Routine arguments
        class(H0vsRnDataSet), intent(out)   :: this
        character(len=256), intent(in)      :: sDataPath
        character(len=256), intent(in)      :: sStationName
        character(len=1), intent(in)        :: sAvgPeriod
        integer                             :: iRetCode

        ! Locals
        type(StationList)                   :: tStations
        type(MultiSeries)                   :: tMultiSeries
        integer                             :: iErrCode
        real(8), dimension(:), allocatable  :: rvTimeStamp
        real(8)                             :: rTimeDelta

        ! Steering constants
        character(len=*), dimension(2), parameter   :: COL_NAME = [ &
            'Rn       ', &
            'H0       ' &
        ]
        real(8), dimension(2), parameter            :: COL_THRESHOLD = [ &
            9.d0, &
            0.d0 &
        ]

        ! Constants
        character(len=256), parameter   :: sStationsFile = "./shakeup_stations.csv"

        ! Assume success (will falsify on failure)
        iRetCode = 0

        ! Get station list from configuration file; no "intelligent" things are done here,
        ! only the reading of station data
        iErrCode = tStations % read(sStationsFile)
        if(iErrCode /= 0) then
            print *, "h0_vs_rn % Read:: Error: Station list read - Return code = ", iErrCode
            iRetCode = 1
            return
        end if
        ! Post-condition: at least one station contains valid data

        ! Get desired data, and compose a dataframe-like structure with them
        iErrCode = tStations % GetQuantitySeries( &
            sDataPath, &
            sStationName, &
            sAvgPeriod, &
            COL_NAME, &
            COL_THRESHOLD, &
            tMultiSeries &
        )
        if(iErrCode /= 0) then
            iRetCode = 2
            return
        end if
        ! Post-condition: station data have been found, and their time stamp assigned

        ! Retrieve time stamp
        iErrCode = tMultiSeries % getTimeStamp(rvTimeStamp)
        if(iErrCode /= 0) then
            iRetCode = 3
            return
        end if
        if(sAvgPeriod == 'H') then
            rTimeDelta = 3600.d0
        else
            rTimeDelta =  600.d0
        end if
        if(allocated(this % time_stamp_begin)) deallocate(this % time_stamp_begin)
        if(allocated(this % time_stamp_end))   deallocate(this % time_stamp_end)
        allocate(this % time_stamp_begin(size(rvTimeStamp)))
        allocate(this % time_stamp_end(size(rvTimeStamp)))
        this % time_stamp_begin = rvTimeStamp - rTimeDelta
        this % time_stamp_end   = rvTimeStamp

        ! Retrieve the four vectors
        iErrCode = tMultiSeries % getVector('Rn       ', this % Rn)
        iErrCode = tMultiSeries % getVector('H0       ', this % H0)

    end function H0vsRnDataRead

end module DataSet
