! zi - Program for estimating mixing height from existing SHAKEUP data
!
! Copyright by Patrizia Favaron
! This is open-source code, covered by MIT licebse.
!
program zi

    use shakeup
    use pbl_met

    implicit none

    ! Locals
    integer                         :: iRetCode
    character(len=256)              :: sDataPath
    character(len=256)              :: sStationName
    character(len=1)                :: sAvgPeriod
    character(len=256)              :: sOutputFile
    type(DataSet)                   :: tData
    integer                         :: n
    integer                         :: i
    real, dimension(:), allocatable :: rvCloud
    real, dimension(:), allocatable :: rvRg_LowTurbidity
    real, dimension(:), allocatable :: rvRg_HighTurbidity
    real, dimension(:), allocatable :: rvN
    real, dimension(:), allocatable :: rvCloudiness
    real, dimension(:), allocatable :: rvRn_LowTurbidity
    real, dimension(:), allocatable :: rvRn_HighTurbidity
    real, dimension(:), allocatable :: rvRn_2
    real, dimension(:), allocatable :: rvUstar_LowTurbidity
    real, dimension(:), allocatable :: rvUstar_HighTurbidity
    real, dimension(:), allocatable :: rvUstar_2
    real, dimension(:), allocatable :: rvUstar_3
    real(8), dimension(:), allocatable :: rvUstar
    real, dimension(:), allocatable :: rvTstar_LowTurbidity
    real, dimension(:), allocatable :: rvTstar_HighTurbidity
    real, dimension(:), allocatable :: rvTstar_2
    real, dimension(:), allocatable :: rvTstar_3
    real, dimension(:), allocatable :: rvH0_LowTurbidity
    real, dimension(:), allocatable :: rvH0_HighTurbidity
    real, dimension(:), allocatable :: rvH0_2
    real, dimension(:), allocatable :: rvH0_3
    real(8), dimension(:), allocatable :: rvH0
    real, dimension(:), allocatable :: rvHlm1_LowTurbidity
    real, dimension(:), allocatable :: rvHlm1_HighTurbidity
    real, dimension(:), allocatable :: rvHlm1_2
    real, dimension(:), allocatable :: rvHlm1_3
    real(8), dimension(:), allocatable :: rvZi_HighTurbidity
    real(8), dimension(:), allocatable :: rvZi_LowTurbidity
    real(8), dimension(:), allocatable :: rvZi_2
    real(8), dimension(:), allocatable :: rvZi_3
    real(8), dimension(:), allocatable :: rvZi
    real                            :: averagingPeriod
    type(DateTime)                  :: tDateTime
    character(len=23)               :: sTimeStamp
    integer                         :: iLUN
    real                            :: rTa
    real                            :: rTw
    real                            :: rEa
    integer                         :: iDeltaTime

    ! Get parameters
    if(command_argument_count() /= 4) then
        print *, "zi - Procedure for calculating basic diagnostic indices on SonicLib data files"
        print *
        print *, "Usage:"
        print *
        print *, "  julia zi.jl <Data_Path> <Station_Name> <Averaging_Period> <Results_File>"
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
    call get_command_argument(1, sDataPath)     ! Data path
    call get_command_argument(2, sStationName)  ! Name of desired station, as in "./shakeup_stations.csv"
    call get_command_argument(3, sAvgPeriod)    ! Averaging period symbol (H = hourly, t = Ten minutes)
    call get_command_argument(4, sOutputFile)   ! Output, in CSV form

    ! Get data
    iRetCode = tData % read(sDataPath, sStationName, sAvgPeriod)
    if(iRetCode /= 0) then
        print *, "zi:: error: Station data not read - ", iRetCode
        stop
    end if

    ! Reserve workspace
    n = size(tData % time_stamp_begin)
    allocate(rvCloud(n))
    allocate(rvRg_LowTurbidity(n))
    allocate(rvRg_HighTurbidity(n))
    allocate(rvRn_LowTurbidity(n))
    allocate(rvRn_HighTurbidity(n))
    allocate(rvRn_2(n))
    allocate(rvUstar(n))
    allocate(rvUstar_LowTurbidity(n))
    allocate(rvUstar_HighTurbidity(n))
    allocate(rvUstar_2(n))
    allocate(rvUstar_3(n))
    allocate(rvTstar_LowTurbidity(n))
    allocate(rvTstar_HighTurbidity(n))
    allocate(rvTstar_2(n))
    allocate(rvTstar_3(n))
    allocate(rvH0_LowTurbidity(n))
    allocate(rvH0_HighTurbidity(n))
    allocate(rvH0_2(n))
    allocate(rvH0_3(n))
    allocate(rvH0(n))
    allocate(rvHlm1_LowTurbidity(n))
    allocate(rvHlm1_HighTurbidity(n))
    allocate(rvHlm1_2(n))
    allocate(rvHlm1_3(n))
    allocate(rvZi_LowTurbidity(n))
    allocate(rvZi_HighTurbidity(n))
    allocate(rvZi_2(n))
    allocate(rvZi_3(n))
    allocate(rvZi(n))

    ! Assume zero cloud cover
    rvCloud = 0.

    ! Estimate global radiation
    print *, "Global radiation"
    if(sAvgPeriod == 'H') then
        averagingPeriod = 3600.0
    else
        averagingPeriod =  600.0
    end if
    iDeltaTime = nint(tData % time_stamp_end(1) - tData % time_stamp_begin(1))
    do i = 1, n
        iRetCode = tDateTime % fromEpoch(tData % time_stamp_begin(i))
        sTimeStamp = tDateTime % ToIso()
        rvRg_HighTurbidity(i) = ClearSkyRg_Accurate( &
            sTimeStamp, &
            averagingPeriod, &
            tData % lat, &
            tData % lon, &
            0.0, &
            real(tData % Pa(i), kind=4), &
            real(tData % Temp(i), kind=4), &
            real(tData % Hrel(i), kind=4), &
            0.5 &
        )
        rvRg_LowTurbidity(i) = ClearSkyRg_Accurate( &
            sTimeStamp, &
            averagingPeriod, &
            tData % lat, &
            tData % lon, &
            0.0, &
            real(tData % Pa(i), kind=4), &
            real(tData % Temp(i), kind=4), &
            real(tData % Hrel(i), kind=4), &
            1.0 &
        )
    end do

    ! Estimate cloudiness factor
    print *, "Estimate cloudiness factor"
    iRetCode = Cloudiness(tData % time_stamp_begin, rvRg_LowTurbidity, real(tData % Rg, kind=4), rvCloudiness)

    ! Estimate cloud cover
    print *, "Estimate cloud cover"
    iRetCode = CloudCover(tData % time_stamp_begin, rvRg_LowTurbidity, real(tData % Rg, kind=4), rvN)

    ! Print cloud cover and cloudiness
    do i = 1, size(rvN)
        iRetCode = tDateTime % fromEpoch(tData % time_stamp_begin(i))
        write(*,"(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2),3(',',f10.4))") &
            tDateTime % iYear, tDateTime % iMonth, tDateTime % iDay, &
            tDateTime % iHour, tDateTime % iMinute, nint(tDateTime % rSecond), &
            tData % Rg(i), &
            rvCloudiness(i), &
            rvN(i)
    end do

    ! Estimate net radiation
    print *, "Estimate net radiation"
    do i = 1, n
        rTa = real(tData % Temp(i), kind=4) + 273.15
        rTw = WetBulbTemperature(rTa, real(tData % Hrel(i), kind=4), real(tData % Pa(i), kind=4), Method=2)
        rEa = WaterVaporPressure(rTw, rTa, real(tData % Pa(i), kind=4))
        rvRn_HighTurbidity(i) = NetRadiation( &
            rvRg_HighTurbidity(i), &
            0.023, &                    ! SHAKEUP stations assumed "rural"
            1.0, &
            rEa, &
            rTa &
        )
        rvRn_LowTurbidity(i) = NetRadiation( &
            rvRg_LowTurbidity(i), &
            0.023, &                    ! SHAKEUP stations assumed "rural"
            1.0, &
            rEa, &
            rTa &
        )
        rvRn_2(i) = NetRadiation( &
            real(tData % Rg(i), kind=4), &
            0.023, &                    ! SHAKEUP stations assumed "rural"
            rvCloudiness(i), &
            rEa, &
            rTa &
        )
    end do

    ! Estimate PBL parameters
    print *, "Estimate PBL parameters"
    do i = 1, n
        iRetCode = PBL_Parameters( &
            4, &
            0.023, &
            10., &
            real(tData % Vel(i), kind=4), &
            real(tData % Temp(i), kind=4), &
            rvRn_HighTurbidity(i), &
            0.0, &
            rvUstar_HighTurbidity(i), &
            rvTstar_HighTurbidity(i), &
            rvH0_HighTurbidity(i), &
            rvHlm1_HighTurbidity(i) &
        )
        iRetCode = PBL_Parameters( &
            4, &
            0.023, &
            10., &
            real(tData % Vel(i), kind=4), &
            real(tData % Temp(i), kind=4), &
            rvRn_LowTurbidity(i), &
            0.0, &
            rvUstar_LowTurbidity(i), &
            rvTstar_LowTurbidity(i), &
            rvH0_LowTurbidity(i), &
            rvHlm1_LowTurbidity(i) &
        )
        iRetCode = PBL_Parameters( &
            4, &
            0.023, &
            10., &
            real(tData % Vel(i), kind=4), &
            real(tData % Temp(i), kind=4), &
            rvRn_2(i), &
            0.0, &
            rvUstar_2(i), &
            rvTstar_2(i), &
            rvH0_2(i), &
            rvHlm1_2(i) &
        )
        iRetCode = PBL_Parameters( &
            4, &
            0.023, &
            10., &
            real(tData % Vel(i), kind=4), &
            real(tData % Temp(i), kind=4), &
            real(tData % Rn(i), kind=4), &
            0.0, &
            rvUstar_3(i), &
            rvTstar_3(i), &
            rvH0_3(i), &
            rvHlm1_3(i) &
        )
    end do

    ! Estimate mixing height using estimated and measured PBL parameters
    print *, "Estimate mixing height"
    rvUstar = rvUstar_LowTurbidity
    rvH0    = rvH0_LowTurbidity
    iRetCode = EstimateZi( &
        tData % time_stamp_begin, &
        tData % zone, &
        tData % lat, &
        tData % lon, &
        iDeltaTime, &
        tData % Temp, &
        rvUstar, &
        rvH0, &
        rvZi    = rvZi_LowTurbidity &
    )
    rvUstar = rvUstar_HighTurbidity
    rvH0    = rvH0_HighTurbidity
    iRetCode = EstimateZi( &
        tData % time_stamp_begin, &
        tData % zone, &
        tData % lat, &
        tData % lon, &
        iDeltaTime, &
        tData % Temp, &
        rvUstar, &
        rvH0, &
        rvZi    = rvZi_HighTurbidity &
    )
    rvUstar = rvUstar_2
    rvH0    = rvH0_2
    iRetCode = EstimateZi( &
        tData % time_stamp_begin, &
        tData % zone, &
        tData % lat, &
        tData % lon, &
        iDeltaTime, &
        tData % Temp, &
        rvUstar, &
        rvH0, &
        rvZi    = rvZi_2 &
    )
    rvUstar = rvUstar_3
    rvH0    = rvH0_3
    iRetCode = EstimateZi( &
        tData % time_stamp_begin, &
        tData % zone, &
        tData % lat, &
        tData % lon, &
        iDeltaTime, &
        tData % Temp, &
        rvUstar, &
        rvH0, &
        rvZi    = rvZi_3 &
    )
    rvUstar = tData % Ustar
    rvH0    = tData % H0
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

    ! Write hourly report
    print *, "Print data"
    open(newunit=iLUN, file=sOutputFile, status='unknown', action='write')
    write(iLUN, "(a,27(',',a))") 'Date.Time', &
                                 'Temp', &
                                 'Rel.H', &
                                 'Pa', &
                                 'Rg.High.Turbidity', &
                                 'Rg.Low.Turbidity', &
                                 'Rg', &
                                 'Rn.High.Turbidity', &
                                 'Rn.Low.Turbidity', &
                                 'Rn.2', &
                                 'Rn', &
                                 'Ustar.High.Turbidity', &
                                 'Ustar.Low.Turbidity', &
                                 'Ustar.2', &
                                 'Ustar.3', &
                                 'Ustar', &
                                 'Tstar.High.Turbidity', &
                                 'Tstar.Low.Turbidity', &
                                 'H0.High.Turbidity', &
                                 'H0.Low.Turbidity', &
                                 'H0.2', &
                                 'H0.3', &
                                 'H0', &
                                 'Zi.High.Turbidity', &
                                 'Zi.Low.Turbidity', &
                                 'Zi.2', &
                                 'Zi.3', &
                                 'Zi.4'
    do i = 1, n
        iRetCode = tDateTime % fromEpoch(tData % time_stamp_begin(i))
        write(iLUN, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2),27(',',f9.3))") &
            tDateTime % iYear, tDateTime % iMonth, tDateTime % iDay, &
            tDateTime % iHour, tDateTime % iMinute, int(tDateTime % rSecond), &
            tData % Temp(i), &
            tData % Hrel(i), &
            tData % Pa(i), &
            rvRg_HighTurbidity(i), &
            rvRg_LowTurbidity(i), &
            tData % Rg(i), &
            rvRn_HighTurbidity(i), &
            rvRn_LowTurbidity(i), &
            rvRn_2(i), &
            tData % Rn(i), &
            rvUstar_HighTurbidity(i), &
            rvUstar_LowTurbidity(i), &
            rvUstar_2(i), &
            rvUstar_3(i), &
            tData % Ustar(i), &
            rvTstar_HighTurbidity(i), &
            rvTstar_LowTurbidity(i), &
            rvH0_HighTurbidity(i), &
            rvH0_LowTurbidity(i), &
            rvH0_2(i), &
            rvH0_3(i), &
            tData % H0(i), &
            rvZi_HighTurbidity(i), &
            rvZi_LowTurbidity(i), &
            rvZi_2(i), &
            rvZi_3(i), &
            rvZi(i)
    end do
    close(iLUN)

end program zi
