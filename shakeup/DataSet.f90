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
        real                                 :: lat
        real                                 :: lon
        integer                              :: zone
        real                                 :: alt
        real(8), dimension(:), allocatable   :: time_stamp_begin
        real(8), dimension(:), allocatable   :: time_stamp_end
        integer                              :: iDeltaTime
        real(8), dimension(:), allocatable   :: Z
        real(8), dimension(:,:), allocatable :: U
        real(8), dimension(:,:), allocatable :: V
        real(8), dimension(:,:), allocatable :: T
        real(8), dimension(:), allocatable   :: Vel
        real(8), dimension(:), allocatable   :: Dir
        real(8), dimension(:), allocatable   :: Temp
        real(8), dimension(:), allocatable   :: RelH
        real(8), dimension(:), allocatable   :: Prec
        real(8), dimension(:), allocatable   :: Pa
        real(8), dimension(:), allocatable   :: Rg
        real(8), dimension(:), allocatable   :: ustar
        real(8), dimension(:), allocatable   :: H0
        real(8), dimension(:), allocatable   :: L
        real(8), dimension(:), allocatable   :: Cover
        real(8), dimension(:), allocatable   :: Zi
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
        integer                             :: i
        integer                             :: iDeltaTime
        real(8), dimension(:), allocatable  :: rvTimeStamp
        real(8), dimension(:), allocatable  :: rvU
        real(8), dimension(:), allocatable  :: rvV
        real(8), dimension(:), allocatable  :: rvT
        real(8)                             :: rTimeDelta
        real(8)                             :: rL
        real(8)                             :: rWT
        real(8)                             :: rTstar

        ! Steering constants
        character(len=*), dimension(8), parameter   :: COL_NAME = [ &
            'Temp     ', &
            'RelH     ', &
            'Rg       ', &
            'Prec     ', &
            'Vel_Sonic', &
            'Dir_Sonic', &
            'Ustar    ', &
            'H0       ' &
        ]
        real(8), dimension(8), parameter            :: COL_THRESHOLD = [ &
            9.d0, &
            9.d0, &
            9.d0, &
            0.d0, &
            9.d0, &
            9.d0, &
            0.d0, &
            0.d0 &
        ]
        
        integer, dimension(8), parameter            :: COL_FILL = [ &
            FILL_LINEAR, &
            FILL_LINEAR, &
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
        this % iDeltaTime       = nint(rvTimeStamp(2) - rvTimeStamp(1))
        
        ! Fill gaps
        iErrCode = tMultiSeries % fillGaps(COL_NAME, COL_FILL, 15)
        if(iErrCode /= 0) then
            print *, "DataSet % Read:: Error: Gaps not filled - Return code = ", iErrCode
            iRetCode = 5
            return
        end if

        ! Retrieve the vectors
        iErrCode = tMultiSeries % getVector('Temp     ', this % temp)
        iErrCode = tMultiSeries % getVector('RelH     ', this % relh)
        iErrCode = tMultiSeries % getVector('Rg       ', this % rg)
        iErrCode = tMultiSeries % getVector('Prec     ', this % prec)
        iErrCode = tMultiSeries % getVector('Vel_Sonic', this % vel)
        iErrCode = tMultiSeries % getVector('Dir_Sonic', this % dir)
        iErrCode = tMultiSeries % getVector('Ustar    ', this % ustar)
        iErrCode = tMultiSeries % getVector('H0       ', this % H0)

        ! Ustar-specific processing
        where(this % ustar <= 0.05)
            this % ustar = 0.05
        endwhere
        
        ! Estimate pressure
        if(allocated(this % Pa)) deallocate(this % Pa)
        allocate(this % Pa(size(this % time_stamp_begin)))
        this % Pa = AirPressure(this % alt)
        
        ! Estimate mixing height
        if(allocated(this % Zi)) deallocate(this % Zi)
        allocate(this % Zi(size(this % time_stamp_begin)))
        iDeltaTime = int(rTimeDelta / 60.0)
        iErrCode = EstimateZi( &
            this % time_stamp_begin, &
            this % zone, &
            this % lat, &
            this % lon, &
            iDeltaTime, &
            this % Temp, &
            this % ustar, &
            this % H0, &
            rvZi = this % Zi &
        )
        if(iErrCode /= 0) then
            print *, "DataSet % Read:: Error: Zi not estimated - Return code = ", iErrCode
            iRetCode = 6
            return
        end if
        
        ! Assign cloud cover
        if(allocated(this % Cover)) deallocate(this % Cover)
        allocate(this % Cover(size(this % time_stamp_begin)))
        where(this % Prec > 0.)
            this % Cover = 1.0
        elsewhere
            this % Cover = 0.2
        endwhere
        
        ! Assign heights and calculate vertical profiles
        if(allocated(this % Zi)) deallocate(this % Zi)
        allocate(this % Zi(size(this % time_stamp_begin)))
        if(allocated(this % L)) deallocate(this % L)
        allocate(this % L(size(this % time_stamp_begin)))
        if(allocated(this % Z)) deallocate(this % Z)
        if(allocated(rvU)) deallocate(rvU)
        if(allocated(rvV)) deallocate(rvV)
        if(allocated(rvT)) deallocate(rvT)
        if(allocated(this % U)) deallocate(this % U)
        if(allocated(this % V)) deallocate(this % V)
        if(allocated(this % T)) deallocate(this % T)
        allocate(this % Z(15))
        allocate(rvU(15))
        allocate(rvV(15))
        allocate(rvT(15))
        allocate(this % U(15, size(this % time_stamp_begin)))
        allocate(this % V(15, size(this % time_stamp_begin)))
        allocate(this % T(15, size(this % time_stamp_begin)))
        this % Z = [(15.*i, i = 1, 15)]
        do i = 1, size(this % time_stamp_begin)

            ! Calculate Obukhov length and temperature scale
            rWT = this % H0(i) / RhoCp(real(this % Temp(i), kind=4) + 273.15)
            rL  = -this % ustar(i) ** 3 * (this % temp(i) + 273.15) / (0.4 * 9.807 * rWT)
            if(.invalid.rL) rL = 100000.0
            this % L(i) = min(max(rL, -100000.0), 100000.0)
            rTstar = rWT / this % ustar(i)
        
            ! Estimate vertical wind profile
            iErrCode = WindProfile( &
                1, &
                this % Z, &
                10.d0, &
                this % Vel(i), &
                this % Dir(i), &
                0.023d0, &
                this % Zi(i), &
                this % ustar(i), &
                10.0d0/this % L(i), &
                rvU, &
                rvV &
            )
            if(iErrCode /= 0) then
                print *, "DataSet % Read:: Error: wind profile not estimated - Return code = ", iErrCode
                iRetCode = 7
                return
            end if
            
            ! Estimate vertical temperature profile
            iErrCode = TempProfile( &
                this % Z, &
                0.023d0, &
                10.d0, &
                this % Temp(i) + 273.15, &
                0.0098d0, &
                this % Zi(i), &
                rTstar, &
                this % ustar(i), &
                1.0d0/this % L(i), &
                rvT &
            )
            if(iErrCode /= 0) then
                print *, "DataSet % Read:: Error: temperature profile not estimated - Return code = ", iErrCode
                iRetCode = 8
                return
            end if
            
            ! Assign vertical wind and temperature profiles
            this % U(:,i) = rvU
            this % V(:,i) = rvV
            this % T(:,i) = rvT
            
        end do

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
