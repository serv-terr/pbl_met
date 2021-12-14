! Example program illustrating the use of pbl_met for processing
! ultrasonic anemometer raw data.
!
! In this case, data are read from Ostiglia daily files. The processing
! takes place on each hour (or any other averaging time T_avg, subject to
! the condition of being a divisor of 3600 seconds). I did my best to separate
! the data read and decoding from the processing, to allow for modification
! would the case be.
!
! Patrizia Favaron - 2021
!
program mini_eddy_cov

    use pbl_met
    
    implicit none
    
    ! Locals
    ! -1- Generic
    integer             :: iRetCode
    character(len=128)  :: sBuffer
    character(len=256)  :: sInputFileName
    character(len=256)  :: sOutputFileName
    integer             :: iAvgTime
    integer             :: iNumLines
    integer             :: iLine
    integer             :: iNumBlocks
    integer             :: iBlock
    integer             :: i
    type(DateTime)      :: tDateTime
    integer             :: iYear, iMonth, iDay, iHour, iMinute, iSecond
    integer             :: iU, iV, iW, iT
    real(8)             :: rSecond
    real(8)             :: rAvgTime
    real(8)             :: rBeginBlockTimeStamp
    character(len=23)   :: sTimeStamp
    type(SonicData)     :: tSonicData
    type(EddyCovData)   :: tEddyCov
    ! -1- Temporary workspace
    real(8), dimension(:), allocatable  :: rvTimeStamp
    real, dimension(:), allocatable     :: rvU
    real, dimension(:), allocatable     :: rvV
    real, dimension(:), allocatable     :: rvW
    real, dimension(:), allocatable     :: rvT
    integer, dimension(:), allocatable  :: ivBlockNumber
    integer, dimension(:), allocatable  :: ivHourFirstIndex
    integer, dimension(:), allocatable  :: ivHourLastIndex
    ! -1- Processing results
    real(8), dimension(:), allocatable  :: rvOutTimeStamp
    real, dimension(:,:), allocatable   :: rmOutWind
    real(8), dimension(:), allocatable  :: rvOutTemp
    real(4), dimension(:), allocatable  :: rvOutScalarVel
    real(8), dimension(:), allocatable  :: rvOutUstar
    real(8), dimension(:), allocatable  :: rvOutUstar_3
    real(8), dimension(:), allocatable  :: rvOutH0
    real(8), dimension(:), allocatable  :: rvOutHe
    
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
    
    ! Check time stamp monotonicity: ideally, time stamps should form a non-decreasing series.
    ! We could say much more than this: the difference between successive-index time stamps
    ! should be 0.0 or 1.0 (or larger, would some "long" acquisition stops have occurred). But
    ! this is much more than needed!
    do iLine = 2, iNumLines
        if(rvTimeStamp(iLine - 1) > rvTimeStamp(iLine)) then
            print *, "Time stamps in file do not form a non-decreasing sequence"
            stop
        end if
    end do
    
    ! If execution arrives here, then we can find the block index corresponding to any
    ! time stamp, given the desired averaging time;
    ! I'm using some simple integer arithmetics and Fortran 95 style array operations here
    allocate(ivBlockNumber(iNumLines))
    ivBlockNumber = floor(rvTimeStamp) / iAvgTime
    ivBlockNumber = ivBlockNumber - ivBlockNumber(1) + 1
    iNumBlocks = maxval(ivBlockNumber)
    
    ! Compute the lower and upper index limits for each block; by monotonicity, we
    ! already know all blocks are "separated", so all what we need to do is to get
    ! the minimum and maximum index values for each block
    allocate(ivHourFirstIndex(iNumBlocks), ivHourLastIndex(iNumBlocks))
    ivHourFirstIndex =  huge(ivHourFirstIndex)
    ivHourLastIndex  = -huge(ivHourLastIndex)
    do iLine = 1, iNumLines
        ivHourFirstIndex(ivBlockNumber(iLine)) = min(ivHourFirstIndex(ivBlockNumber(iLine)), iLine)
        ivHourLastIndex(ivBlockNumber(iLine))  = max(ivHourLastIndex(ivBlockNumber(iLine)), iLine)
    end do
    
    ! Inform users
    print *, "Data read completed. ", iNumLines, " valid data lines found"
    
    ! *******************
    ! * Processing part *
    ! *******************
    
    ! Processing here means doing the eddy covariance calculations on each block,
    ! and save the (interesting) results. Other may be added in future.
    !
    ! "Blocks" were defined by the averaging time. Any of them may, in principle, be empty
    ! (in which case the initial index is larger than the initial). These empty blocks
    ! should not be processed.
    !
    ! Processing is made using pbl_met structures and functions.
    
    ! Main loop: iterate over blocks, process and write data
    rAvgTime = iAvgTime
    print *, 'Num blocks = ', iNumBlocks
    open(10, file=sOutputFileName, status='unknown', action='write')
    write(10, "('Date.Time, Vel, Scalar.Vel, Dir, W, Temp, Ustar, H0')")
    do iBlock = 1, iNumBlocks
        if(ivHourFirstIndex(iBlock) <= ivHourLastIndex(iBlock)) then    ! Block is non-empty
        
            ! Get block's starting time stamp, by "flooring" it with respect to averaging time;
            ! then convert it to printable form
            rBeginBlockTimeStamp = timeFloor(rvTimeStamp(ivHourFirstIndex(iBlock)), rAvgTime)
            iRetCode = tDateTime % fromEpoch(rBeginBlockTimeStamp)
            sTimeStamp = tDateTime % toISO()
            
            ! Fill the data set with data from vectors, relative to this block
            iRetCode = tSonicData % buildFromVectors( &
                rvTimeStamp(ivHourFirstIndex(iBlock):ivHourLastIndex(iBlock)), &
                rvU(ivHourFirstIndex(iBlock):ivHourLastIndex(iBlock)), &
                rvV(ivHourFirstIndex(iBlock):ivHourLastIndex(iBlock)), &
                rvW(ivHourFirstIndex(iBlock):ivHourLastIndex(iBlock)), &
                rvT(ivHourFirstIndex(iBlock):ivHourLastIndex(iBlock)) &
            )
            if(iRetCode /= 0) then
                print * , "Warning: Block ", iBlock, " was not copied to processing data set"
                cycle
            end if
            
            ! Perform standard data cleaning prior to engage eddy covariance, that is,
            ! trend and spike removal. Trend removal is performed first.
            ! Spikes are treated by "clipping", that is reducing to the range of +/- 3 standard
            ! deviations from the mean (the factor "3" is quite a tradition among eddy covariance
            ! practitioners: I felt no special reason to change it). And, I used clipping instead
            ! of invalidation as this is a less aggressive method, less likely to change the
            ! trend after removal has been made.
            !
            ! Note to the "cubs" whi will extend this procedure: I have not stored the
            ! trend and spike treatment results, but, "you" may do in future. This
            ! information is routinely produced by the two routines used here: I just avoided
            ! to mention them - they're optional parameters. In general, the trend and
            ! spike removal routines can have important diagnostic uses.
            iRetCode = tSonicData % removeTrend(iAvgTime)
            if(iRetCode /= 0) then
                print *, "Warning: Block ", iBlock, " did not allow trend removal"
                cycle
            end if
            iRetCode = tSonicData % treatSpikes(iAvgTime, SPK_CLIP, 3.)
            if(iRetCode /= 0) then
                print *, "Warning: Block ", iBlock, " did not allow spike treatment"
                cycle
            end if
            
            ! Compute the means, variances and covariances to be used later on
            ! when computing eddy covariance
            iRetCode = tSonicData % averages(iAvgTime, tEddyCov)
            if(iRetCode /= 0) then
                print *, "Warning: Block ", iBlock, " did not allow averaging"
                cycle
            end if
            
            ! And last, perform eddy covariance step, using 2 rotations and assuming
            ! the sonic anemometer operates at 10 m above ground level (might be read from
            ! the command line in a next version)
            iRetCode = tEddyCov % Process(2, 10.d0)
            if(iRetCode /= 0) then
                print *, "Warning: Block ", iBlock, " did not allow eddy covariance computing"
                cycle
            end if
            
            ! Get processing results and write them
            iRetCode = tEddyCov % getTimeStamp(rvOutTimeStamp)
            if(iRetCode /= 0) then
                print *, "Warning: Block ", iBlock, " did not allow gethering time stamps"
                cycle
            end if
            iRetCode = tEddyCov % getWind(rmOutWind)
            if(iRetCode /= 0) then
                print *, "Warning: Block ", iBlock, " did not allow gethering wind data"
                cycle
            end if
            iRetCode = tEddyCov % getTemp(rvOutTemp)
            if(iRetCode /= 0) then
                print *, "Warning: Block ", iBlock, " did not allow gethering temperature data"
                cycle
            end if
            iRetCode = tEddyCov % getScalarWind(rvOutScalarVel)
            if(iRetCode /= 0) then
                print *, "Warning: Block ", iBlock, " did not allow gethering scalar velocity data"
                cycle
            end if
            iRetCode = tEddyCov % getUstar(rvOutUstar, rvOutUstar_3)
            if(iRetCode /= 0) then
                print *, "Warning: Block ", iBlock, " did not allow gethering friction velocity data"
                cycle
            end if
            iRetCode = tEddyCov % getHeatFluxes(rvOutH0, rvOutHe)
            if(iRetCode /= 0) then
                print *, "Warning: Block ", iBlock, " did not allow gethering heat flux data"
                cycle
            end if
            
            ! Write results
            
            ! Compose time stamp in string form, ready for printing
            iRetCode = tDateTime % fromEpoch(rvOutTimeStamp(i))
            sTimeStamp = tDateTime % toISO()
            
            ! Write actual data. A warning is due here: the eddy-covariance routines
            ! were designed to operate on a hourly basis, possibly with a sub-hourly
            ! averaging time. I might have had used them this way, but in sake of
            ! simplicity I preferred to operate on sub-hourly averaging intervals
            ! directly. Used this way, the eddy covariance routines still think to be
            ! operate hourly, but with an initial time stamp which corresponds to the
            ! beginning of the averaging time, not a whole hour. The net result is that
            ! in this "hour" only the first sub-averaging interval contains useful
            ! information. This explain the time-index "1" used in the following write:
            write(10, "(a,7(',',f7.3))") &
                sTimeStamp(1:19), &
                rmOutWind(1,1), &
                rvOutScalarVel(1), &
                rmOutWind(1,2), &
                rmOutWind(1,3), &
                rvOutTemp(1), &
                rvOutUstar(1), &
                rvOutH0(1)
            
        end if
    end do
    close(10)
    
    ! Leave
    deallocate(rvTimeStamp, rvU, rvV, rvW, rvT)
    deallocate(ivBlockNumber)
    deallocate(ivHourFirstIndex, ivHourLastIndex)
    print *, '*** End Job ***'

end program mini_eddy_cov
