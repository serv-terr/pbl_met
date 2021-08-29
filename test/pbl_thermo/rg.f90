! Unit test program for global radiation
program test_radiation

    use pbl_met

    implicit none

    ! Locals
    real(8), dimension(:), allocatable  :: rvTimeStamp
    real, dimension(:), allocatable     :: rvRa
    real, dimension(:), allocatable     :: rvRg
    real, dimension(:), allocatable     :: rvRg0
    type(DateTime)                      :: tDateTime
    real                                :: rLat  = 46.4664
    real                                :: rLon  = 10.3705
    real                                :: rZone = 1.
    real                                :: rAltitude1 = 1.223
    real                                :: rAltitude2 = 0.
    real                                :: rAveragingTime = 3600.
    real                                :: rTimeStep      =   10.
    integer                             :: i
    integer                             :: iRetCode
    character(len=10)                   :: sDateTime
    
    ! Generate initial time stamp
    allocate(rvTimeStamp(8760), rvRa(8760), rvRg(8760), rvRg0(8760))

    ! Assign time stamp
    tDateTime = DateTime(2021, 1, 1, 0, 0, 0)
    rvTimeStamp(1) = tDateTime % ToEpoch()
    do i = 1, 8760
        rvTimeStamp(i) = rvTimeStamp(1) + (i-1)*3600.d0
    end do

    ! Estimate extraterrestrial and global radiation
    do i = 1, 8760
        rvRa(i)  = ExtraterrestrialRadiation(rvTimeStamp(i), rAveragingTime, rTimeStep, rLat, rLon, rZone)
        rvRg(i)  = ClearSkyRg_Accurate(rvTimeStamp(i), rAveragingTime, rTimeStep, rLat, rLon, rZone, rAltitude1)
        rvRg0(i) = ClearSkyRg_Accurate(rvTimeStamp(i), rAveragingTime, rTimeStep, rLat, rLon, rZone, rAltitude2)
    end do

    do i = 1, 8760, 24
        iRetCode = tDateTime % fromEpoch(rvTimeStamp(i))
        sDateTime = tDateTime % toISO()
        print *, sDateTime, maxval(rvRa(i:i+23)), maxval(rvRg(i:i+23)), maxval(rvRg0(i:i+23))
    end do

    ! Leave
    deallocate(rvTimeStamp, rvRg)
    
end program test_radiation
