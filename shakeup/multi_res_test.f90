! Example program illustrating the use of pbl_met for processing
! ultrasonic anemometer raw data using a multi-resolution approach.
!
! The purpose of this work is decomposing a signal in successive steps
! using a progressively decreasing averaging mesh, to check at which time
! scale the internal variance concentrates (or distributes).
!
! This information may in turn be used to assess a signal stationarity, or
! the characteristic time scales (down to 1 second, the coarsest resolution
! of time stamps in SHAKEUP-and-friends data.
!
! In this case, data are read from Ostiglia daily files. The processing
! takes place on each hour (or any other averaging time T_avg, subject to
! the condition of being a divisor of 3600 seconds). I did my best to separate
! the data read and decoding from the processing, to allow for modification
! would the case be.
!
! Patrizia Favaron - 2021
!
program multi_res_test

    use multires
    use pbl_met
    
    implicit none
    
    ! Locals
    ! -1- Generic
    integer             :: iRetCode
    character(len=128)  :: sBuffer
    character(len=256)  :: sInputFileName
    character(len=256)  :: sOutputFileName
    integer             :: iNumLines
    integer             :: iLine
    integer             :: iNumHalvings
    integer             :: iHalving
    integer             :: iAvgTime
    type(DateTime)      :: tDateTime
    integer             :: iYear, iMonth, iDay, iHour, iMinute, iSecond
    integer             :: iU, iV, iW, iT
    real(8)             :: rSecond
    type(signal)        :: tSignal
    real                :: rOriginalVariance
    real                :: rResidualVariance
    real                :: rOriginalTotalVar
    real                :: rResidualTotalVar
    ! -1- Temporary workspace
    real(8), dimension(:), allocatable  :: rvTimeStamp
    real, dimension(:), allocatable     :: rvU
    real, dimension(:), allocatable     :: rvV
    real, dimension(:), allocatable     :: rvW
    real, dimension(:), allocatable     :: rvT
    real, dimension(:), allocatable     :: rvVariance
    real, dimension(:), allocatable     :: rvTotalVar
    real, dimension(:), allocatable     :: rvPartialVar
    real(8), dimension(:), allocatable  :: rvTimeScale
    real, dimension(:), allocatable     :: rvCoarseGrain
    real, dimension(:), allocatable     :: rvFineGrain
    
    ! Get command line arguments
    if(command_argument_count() /= 3) then
        print *, "mini_eddy_cov - Eddy covariance procedure"
        print *
        print *, "Usage:"
        print *
        print *, "  ./mini_eddy_cov <Averaging_Time> <Data_File> <Output_File>"
        print *
        print *, "Copyright 2021 by Patrizia Favaron"
        print *, "                  This is open-source code, covered by the MIT license"
        stop
    end if
    call get_command_argument(1, sBuffer)
    read(sBuffer, *) iAvgTime
    if(mod(3600, iAvgTime) /= 0) then
        print *, "Selected averaging time does not divide 3600"
        stop
    end if
    call get_command_argument(2, sInputFileName)
    call get_command_argument(3, sOutputFileName)
    
    ! ***********************************
    ! * Data is read and organized here *
    ! ***********************************
    !
    ! Here the procedure gathers data into an allocatable workspace.
    !
    ! Accordingly to a common idiom of mine, reading is performed
    ! in two steps: the first counting the data lines, and the second performing
    ! the actual read.
    !
    ! Sure this way of doing is "inefficient", but offers the benefit of being
    ! easy to understand, and to use memory in a tight way.
    !
    ! Reading inefficiency can be overcome by using self-descripting file
    ! formats as NetCDF for example. But this is not the case with files composed
    ! by automatic data acquisition systems encoded as CSV.
    !
    ! Once read, data are organized in "hours" according to their rime stamps.
    ! No data moving is actually made: time stamp sequence is first tested for
    ! monotonicity, and once this is guaranteed hour begin-end indices are
    ! generated using min/max on same index hours. Again, this technique was
    ! chisen by its conceptual simplicity against possibly more efficient but trickier
    ! methods like runs detection.
    !
    ! Why "simplicity and ease of understanding", instead of "bare efficiency"?
    ! Well, first and foremost this is an offline procedure, whose main purpose
    ! is to act as a mold for other, student-provided procedures, running on devices
    ! vastly exceeding the performances of Cray supercomputers in my early
    ! "cub of mathematician" days, but consuming orders of magnitude of electric
    ! current less: we can afford tiny inefficiencies.
    !
    ! In exchange, procedures which are "easy to understand" may contribute to
    ! prevent you to get mad at (me and) the code when you are in a hurry to get
    ! your children at school or the like. It's better you concentrate on real problems, instead of
    ! trying to decode some "clever" algorithm. This is a concern very mine
    ! When I write code, I tend to respect the machine much less than prospective users
    ! having normal human "troubles". You're not obliged doing the same, however. :)
    
    ! Step 1: Count data lines and reserve workspace
    open(10, file=sInputFileName, status='old', action='read', iostat=iRetCode)
    if(iRetCode /= 0) then
        print *, "Input file not opened: check name"
        stop
    end if
    print *, "Begin data reading"
    iNumLines = 0   ! No header lines in data files, we may count from zero
    do
    
        ! Get one line "acritically"
        read(10, "(a)", iostat=iRetCode) sBuffer
        if(iRetCode /= 0) exit
        
        ! Now, exclude the case the line length is not 58 characters (which
        ! might be for non-data lines from Metek anemometers)
        if(len_trim(sBuffer) /= 58) cycle
        
        ! We're sure the line has the right length: does it also contain
        ! sensible data? The maximum nominal range for wind components is +/-60 m/s
        ! for wind components, and +/- 60 °C for temperature. Would any of
        ! these quantities violate these constraints, the whole dala line
        ! can be considered invalid, and we may ignore it
        read(sBuffer, "(i4,5i2,4x,4(4x,i6))", iostat=iRetCode) &
            iYear, iMonth, iDay, iHour, iMinute, iSecond, iV, iU, iW, iT
        if(iRetCode /= 0) cycle
        if(abs(iU) > 6000 .or. abs(iV) > 6000 .or. abs(iW) > 6000 .or. abs(iT) > 6000) cycle
        
        ! Data line is valid, finally! We can count it
        iNumLines = iNumLines + 1
        
    end do
    if(iNumLines <= 2) then
        print *, "No data in input file..."
        stop
    end if
    allocate(rvTimeStamp(iNumLines), rvU(iNumLines), rvV(iNumLines), rvW(iNumLines), rvT(iNumLines))
    
    ! Step 2: actual data read (and decoding to numbers)
    rewind(10)
    iLine = 0
    do
    
        ! Get line
        read(10, "(a)", iostat=iRetCode) sBuffer
        if(iRetCode /= 0) exit
        
        ! Filter out invalidity conditions
        if(len_trim(sBuffer) /= 58) cycle
        read(sBuffer, "(i4,5i2,4x,4(4x,i6))", iostat=iRetCode) &
            iYear, iMonth, iDay, iHour, iMinute, iSecond, iV, iU, iW, iT
        if(iRetCode /= 0) cycle
        if(abs(iU) > 6000 .or. abs(iV) > 6000 .or. abs(iW) > 6000 .or. abs(iT) > 6000) cycle
        
        ! Save data, finally! That is, reserve its index and do anything needed (in the
        ! code lines following)
        iLine = iLine + 1
        
        ! Convert date to epoch, using the time functions of pbl_met
        rSecond = iSecond
        tDateTime = DateTime(iYear, iMonth, iDay, iHour, iMinute, rSecond)
        rvTimeStamp(iLine) = tDateTime % toEpoch()
        
        ! Convert other data
        rvU(iLine) = iU / 100.
        rvV(iLine) = iV / 100.
        rvW(iLine) = iW / 100.
        rvT(iLine) = iT / 100.
        
    end do
    close(10)
    print *, "Data read completed"
    
    ! ******************************************
    ! * Perform multi-resolution decomposition *
    ! ******************************************
    
    ! Create multi-resolution object
    iRetCode = tSignal % create(rvTimeStamp, rvT)
    if(iRetCode /= 0) then
        print *, "Signal not created, with return code = ", iRetCode
        stop
    end if
    
    ! Get the variances and total variations, and print them
    iRetCode = tSignal % get_variances(rOriginalVariance, rvVariance, rResidualVariance)  !No check on errors! I'm sure this is useless, in *this* case
    iRetCode = tSignal % get_total_variation(rOriginalTotalVar, rvTotalVar, rResidualTotalVar)  !No check on errors! I'm sure this is useless, in *this* case
    iRetCode = tSignal % get_partial_variation(rvPartialVar)  !No check on errors! I'm sure this is useless, in *this* case
    
    ! Get time scales associated to variances
    iRetCode = tSignal % get_times(rvTimeScale)
    iNumHalvings = size(rvTimeScale)
    print *, "Total Variance:  ", rOriginalVariance
    print *, "Total Variation: ", rOriginalTotalVar
    do iHalving = 1, iNumHalvings
        print "('H: ',i2,'  T: ',f11.4,'  Var = ',f9.7,'  TotalVar = ',e15.7,'  PartialVar = ',e15.7)", &
            iHalving - 1, rvTimeScale(iHalving), rvVariance(iHalving), rvTotalVar(iHalving), rvPartialVar(iHalving)
    end do
    print *, "Variance of residual: ", rResidualVariance
    print *, "Total variation of residual: ", rResidualTotalVar
    print *, "Error of variance:    ", rOriginalVariance - (sum(rvVariance) + rResidualVariance)
    
    ! Get some approximations
    iRetCode = tSignal % approximate(iNumHalvings / 4, rvCoarseGrain)
    iRetCode = tSignal % approximate(iNumHalvings-1, rvFineGrain)
    open(10, file = sOutputFileName, status='unknown', action='write')
    write(10, "('Time.Stamp, Original, Coarse.Grained, Fine.Grained, Residual')")
    do iLine = 1, iNumLines
        iRetCode = tDateTime % fromEpoch(rvTimeStamp(iLine))
        write(10, fmt="(a, 4(',',e15.7))") &
            tDateTime % toIso(), &
            rvT(iLine), &
            rvCoarseGrain(iLine), &
            rvFineGrain(iLine), &
            tSignal % rvResidual(iLine)
    end do
    close(10)
    
    ! Leave
    deallocate(rvTimeStamp, rvU, rvV, rvW, rvT)
    print *, '*** End Job ***'

end program multi_res_test
