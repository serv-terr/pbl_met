! h0_vs_rn - Program for comparing H0 to net radiation from existing SHAKEUP data
!
! Copyright by Patrizia Favaron
! This is open-source code, covered by MIT licebse.
!
program h0_vs_rn

    use DataSet
    use pbl_met

    implicit none

    ! Locals
    integer                         :: iRetCode
    character(len=256)              :: sDataPath
    character(len=256)              :: sStationName
    character(len=1)                :: sAvgPeriod
    character(len=256)              :: sOutputFile
    type(H0vsRnDataSet)             :: tData
    integer                         :: n
    integer                         :: i
    real(8), dimension(:), allocatable  :: rvRn
    real(8), dimension(:), allocatable  :: rvH0
    real                            :: averagingPeriod
    type(DateTime)                  :: tDateTime
    character(len=23)               :: sTimeStamp
    integer                         :: iLUN
    integer                         :: iDeltaTime

    ! Get parameters
    if(command_argument_count() /= 4) then
        print *, "h0_vs_rn - Procedure for comparing H0 to Rn"
        print *
        print *, "Usage:"
        print *
        print *, "  h0_vs_rn <Data_Path> <Station_Name> <Averaging_Period> <Results_File>"
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

    ! Get data
    iRetCode = tData % read(sDataPath, sStationName, sAvgPeriod)
    if(iRetCode /= 0) then
        print *, "h0_vs_rn:: error: Station data not read - Return code = ", iRetCode
        stop
    end if

    ! Reserve workspace
    n = size(tData % time_stamp_begin)
    allocate(rvRn(n))
    allocate(rvH0(n))

    ! Estimate mixing height using measured PBL parameters
    print *, "Get Rn and H0 readings"
    rvRn = tData % Rn
    rvH0 = tData % H0

    ! Write hourly report
    print *, "Print hourly data"
    open(newunit=iLUN, file=sOutputFile, status='unknown', action='write')
    write(iLUN, "(a,2(',',a))") 'Date.Time', &
                                 'Rn', &
                                 'H0'
    do i = 1, n
        iRetCode = tDateTime % fromEpoch(tData % time_stamp_begin(i))
        write(iLUN, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2),2(',',f9.3))") &
            tDateTime % iYear, tDateTime % iMonth, tDateTime % iDay, &
            tDateTime % iHour, tDateTime % iMinute, int(tDateTime % rSecond), &
            tData % Rn(i), &
            tData % H0(i)
    end do
    close(iLUN)

end program h0_vs_rn
