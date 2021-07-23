! zi_shakeup - Program for estimating mixing height from existing SHAKEUP data
!
! Copyright by Patrizia Favaron
! This is open-source code, covered by MIT licebse.
!
program zi_shakeup

    use DataSet
    use pbl_met

    implicit none

    ! Locals
    integer                         :: iRetCode
    character(len=256)              :: sDataPath
    character(len=256)              :: sStationName
    character(len=1)                :: sAvgPeriod
    character(len=256)              :: sOutputFile
    character(len=256)              :: sDailyOutputFile
    type(ZiDataSet)                 :: tData
    integer                         :: n
    integer                         :: i
    real(8), dimension(:), allocatable  :: rvUstar
    real(8), dimension(:), allocatable  :: rvH0
    real(8), dimension(:), allocatable  :: rvZi
    real(8), dimension(:), allocatable  :: rvDailyTimeStamp
    real, dimension(:), allocatable     :: rvMaxZi
    real, dimension(:), allocatable     :: rvMaxPlm
    real                            :: averagingPeriod
    type(DateTime)                  :: tDateTime
    character(len=23)               :: sTimeStamp
    integer                         :: iLUN
    real                            :: rTa
    real                            :: rTw
    real                            :: rEa
    integer                         :: iDeltaTime

    ! Get parameters
    if(command_argument_count() /= 5) then
        print *, "zi_shakeup - Procedure for calculating basic diagnostic indices on SonicLib data files"
        print *
        print *, "Usage:"
        print *
        print *, "  zi_shakeup <Data_Path> <Station_Name> <Averaging_Period> <Results_File> <Daily_File>"
        print *
        print *, "where"
        print *
        print *, "  <Averaging_Period> = H (hourly), t (10 minutes)"
        print *
        print *, "Copyright 2021 by Patrizia Favaron"
        print *, "This is open-source software, covered by the MIT license."
        print *
        stop
    end if
    call get_command_argument(1, sDataPath)         ! Data path
    call get_command_argument(2, sStationName)      ! Name of desired station, as in "./shakeup_stations.csv"
    call get_command_argument(3, sAvgPeriod)        ! Averaging period symbol (H = hourly, t = Ten minutes)
    call get_command_argument(4, sOutputFile)       ! Hourly Output, in CSV form
    call get_command_argument(5, sDailyOutputFile)  ! Daily Output, in CSV form

    ! Get data
    iRetCode = tData % read(sDataPath, sStationName, sAvgPeriod)
    if(iRetCode /= 0) then
        print *, "zi_shakeup:: error: Station data not read - Return code = ", iRetCode
        stop
    end if

    ! Reserve workspace
    n = size(tData % time_stamp_begin)
    allocate(rvUstar(n))
    allocate(rvH0(n))

    ! Estimate mixing height using measured PBL parameters
    print *, "Estimate mixing height"
    rvUstar    = tData % Ustar
    rvH0       = tData % H0
    iDeltaTime = 60
    iRetCode = EstimateZi( &
        tData % time_stamp_begin, &
        tData % zone, &
        tData % lat, &
        tData % lon, &
        iDeltaTime, &
        tData % Temp, &
        rvUstar, &
        rvH0, &
        rvZi    = rvZi &
    )
    if(iRetCode /= 0) then
        print *, "zi_shakeup:: error: Mixing height estimation failed - Return code = ", iRetCode
        stop
    end if

    ! Write hourly report
    print *, "Print hourly data"
    open(newunit=iLUN, file=sOutputFile, status='unknown', action='write')
    write(iLUN, "(a,5(',',a))") 'Date.Time', &
                                 'Temp', &
                                 'Vel', &
                                 'Ustar', &
                                 'H0', &
                                 'Zi'
    do i = 1, n
        iRetCode = tDateTime % fromEpoch(tData % time_stamp_begin(i))
        write(iLUN, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2),5(',',f9.3))") &
            tDateTime % iYear, tDateTime % iMonth, tDateTime % iDay, &
            tDateTime % iHour, tDateTime % iMinute, int(tDateTime % rSecond), &
            tData % Temp(i), &
            tData % Vel(i), &
            tData % Ustar(i), &
            tData % H0(i), &
            rvZi(i)
    end do
    close(iLUN)

    ! Generate daily report
    iRetCode = ZiDailySynthesis( &
        tData % time_stamp_begin, &
        real(tData % Vel, kind=4), &
        real(rvZi, kind=4), &
        rvDailyTimeStamp, &
        rvMaxZi, &
        rvMaxPlm &
    )
    if(iRetCode /= 0) then
        print *, "zi_shakeup:: error: Impossible to compute daily synthesis - Return code = ", iRetCode
        stop
    end if

    ! Write daily report
    print *, "Print daily data"
    open(newunit=iLUN, file=sDailyOutputFile, status='unknown', action='write')
    write(iLUN, "(a,2(',',a))") 'Date', &
                                 'Max.Zi', &
                                 'Max.Plm'
    do i = 1, size(rvDailyTimeStamp)
        iRetCode = tDateTime % fromEpoch(rvDailyTimeStamp(i))
        write(iLUN, "(i4.4,2('-',i2.2),2(',',f12.3))") &
            tDateTime % iYear, tDateTime % iMonth, tDateTime % iDay, &
            rvMaxZi(i), &
            rvMaxPlm(i)
    end do
    close(iLUN)

end program zi_shakeup
