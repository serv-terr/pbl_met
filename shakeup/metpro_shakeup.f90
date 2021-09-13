! metpro_shakeup - Program for producing met input to dispersion models from existing SHAKEUP data
!
! Copyright by Patrizia Favaron
! This is open-source code, covered by MIT licebse.
!
program metpro_shakeup

    use DataSet
    use model_meteo
    use pbl_met

    implicit none

    ! Locals
    integer                             :: iRetCode
    character(len=256)                  :: sDataPath
    character(len=256)                  :: sStationName
    character(len=1)                    :: sAvgPeriod
    character(len=256)                  :: sOutputPrefix
    type(MetProDataSet)                 :: tData
    integer                             :: n
    real(8), dimension(:), allocatable  :: rvTimeStamp
    real(8), dimension(:), allocatable  :: rvTemp
    real(8), dimension(:), allocatable  :: rvRelH
    real(8), dimension(:), allocatable  :: rvVel
    real(8), dimension(:), allocatable  :: rvDir
    real(8), dimension(:), allocatable  :: rvUstar
    real(8), dimension(:), allocatable  :: rvH0
    real(8), dimension(:), allocatable  :: rvZi
    integer                             :: iDeltaTime

    ! Get parameters
    if(command_argument_count() /= 4) then
        print *, "shakemet - SHAKEUP meteorological 'lightweight' processor"
        print *
        print *, "Usage:"
        print *
        print *, "  shakemet <Data_Path> <Station_Name> <Averaging_Period> <Results_Prefix>"
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
    call get_command_argument(4, sOutputPrefix)     ! Prefix used to generate output files
    print *
    print *, "shakemet - SHAKEUP meteorological 'lightweight' processor"

    ! Get data
    iRetCode = tData % read(sDataPath, sStationName, sAvgPeriod)
    if(iRetCode /= 0) then
        print *, "shakemet:: error: Station data not read - Return code = ", iRetCode
        stop
    end if
    print *, "Data read and gap filled"

    ! Prepare Calpuff and Calmet meteorological data
    iRetCode = calpuff(sOutputPrefix, tData, 3600, 10.0, 0.023)
    if(iRetCode /= 0) then
        print *, "shakemet:: error: Calpuff data files not produced - Return code = ", iRetCode
        stop
    end if
    print *, "Output Calpuff/Calmet data files created"
    
    ! Leave
    print *, "*** END JOB ***"

end program metpro_shakeup
