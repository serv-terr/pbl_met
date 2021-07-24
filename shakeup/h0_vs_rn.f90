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
    integer                             :: iRetCode
    character(len=256)                  :: sDataPath
    character(len=256)                  :: sStationName
    character(len=1)                    :: sAvgPeriod
    character(len=256)                  :: sOutputFile
    type(H0vsRnDataSet)                 :: tData
    integer                             :: n
    integer                             :: i
    real(8), dimension(:), allocatable  :: rvRn
    real(8), dimension(:), allocatable  :: rvH0
    real(8), dimension(:), allocatable  :: rvValidRn
    real(8), dimension(:), allocatable  :: rvValidH0
    real(8), dimension(:), allocatable  :: rvEstimatedH0
    logical, dimension(:), allocatable  :: lvIncluded
    real(8)                             :: rAlpha
    real(8)                             :: rOffset
    real(8)                             :: rFB
    real(8)                             :: rFAC2
    real                                :: averagingPeriod
    type(DateTime)                      :: tDateTime
    character(len=23)                   :: sTimeStamp
    integer                             :: iLUN
    integer                             :: iDeltaTime

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

    ! Use the pbl_met clipping functions to remove invalids in either series
    print *, "Taking valid data only"
    n = size(tData % Rn)
    allocate(rvRn(n))
    allocate(rvH0(n))
    rvRn = tData % Rn
    rvH0 = tData % H0
    call PairInvalidate(rvRn, rvH0)
    rvValidRn = GetValidOnly(rvRn)
    rvValidH0 = GetValidOnly(rvH0)
    if(size(rvValidRn) <= 0 .or. size(rvValidH0) <= 0 .or. size(rvValidRn) /= size(rvValidH0)) then
        print *, "h0_vs_rn:: error: No valid data found"
        stop
    end if

    ! Regress H0 on Rn, to compute 'alpha' (the regression multiplier); the offset, in this case,
    ! acts as an error indication: ideally, its value is 0.
    print *, "Get Rn and H0 readings"
    iRetCode = SimpleLinearRegression(rvValidRn, rvValidH0, rAlpha, rOffset, rvEstimatedH0)
    if(iRetCode /= 0) then
        print *, "h0_vs_rn:: error: Regression not computed - Return code = ", iRetCode
        stop
    end if

    ! Compute validation statistics on measured and regressed H0
    print *, "Compute validation statistics"
    rFAC2 = FAC2(rvValidH0, rvEstimatedH0, rFactorIn=10.d0, lvIncluded = lvIncluded)
    rFB   = FB(rvValidH0, rvEstimatedH0)

    ! Write hourly report
    print *, "Print hourly data"
    open(newunit=iLUN, file=sOutputFile, status='unknown', action='write')
    write(iLUN, "(a,4(',',a))") 'Date.Time', &
        'Rn', &
        'H0', &
        'H0.Estimate', &
        'H0.Included'
    do i = 1, size(rvValidRn)
        iRetCode = tDateTime % fromEpoch(tData % time_stamp_begin(i))
        write(iLUN, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2),3(',',f9.3),',',l1)") &
            tDateTime % iYear, tDateTime % iMonth, tDateTime % iDay, &
            tDateTime % iHour, tDateTime % iMinute, int(tDateTime % rSecond), &
            rvValidRn(i), &
            rvValidH0(i), &
            rvEstimatedH0(i), &
            lvIncluded(i)
    end do
    close(iLUN)

    ! Print parameters
    print *, 'Alpha  = ', rAlpha
    print *, 'Offset = ', rOffset
    print *, 'FB     = ', rFB
    print *, 'FAC2   = ', rFAC2

    deallocate(rvH0)
    deallocate(rvRn)

end program h0_vs_rn
