program Raw_Encoder
    
    use shakeup_data
    
    implicit none
    
    ! Locals
    integer                                         :: iRetCode
    character(len=256)                              :: sInPathName
    character(len=256)                              :: sOutPathName
    character(len=64)                               :: sBuffer
    character(len=256), dimension(:), allocatable   :: svFile
    type(shakeup_raw_data)                          :: tShk
    integer                                         :: iYear
    integer                                         :: iFile
    character(len=256)                              :: sFileName
    
    ! Check command line parameters
    if(command_argument_count() /= 3) then
        print *, "renc - Raw MeteoFlux Core V2 sonic data encoder"
        print *
        print *, "Usage:"
        print *
        print *, "    ./renc <InputPath> <OutputPath> <Year_of_data>"
        print *
        print *, "Copyright 2022 by Patrizia Favaron"
        print *, "                  This is open-source code, covered by the MIT license."
        print *
        stop
    end if
    call get_command_argument(1, sInPathName)
    call get_command_argument(2, sOutPathName)
    if(sInPathName == sOutPathName) then
        print *, "Error: input and output pathnames coincide - they should not"
        stop
    end if
    call get_command_argument(3, sBuffer)
    read(sBuffer, *, iostat=iRetCode) iYear
    if(iRetCode /= 0) then
        print *, "Error: year is nor a valid integer"
        stop
    end if
    
    ! Map data files in input pathname
    iRetCode = meteoflux_map_dir(sInPathName, iYear, svFile)
    if(iRetCode /= 0) then
        print *, "Error: no SHAKEUP raw data file in input path"
        stop
    end if
    print *, size(svFile), " files found"
    
    ! Generate Metek-style sub-dirs in output path
    iRetCode = metek_build_dirs(sOutPathName, iYear)
    if(iRetCode /= 0) then
        print *, "Error: Directories not created in output path"
        stop
    end if
    
    ! Main loop: iterate over files and converte them
    do iFile = 1, size(svFile)
        
        ! Read file
        iRetCode = tShk % read_mfc2_file(sInPathName, svFile(iFile))
        if(iRetCode /= 0) cycle
        
        ! Write file
        write(sFileName, "(a,'.qs')") trim(svFile(iFile))
        iRetCode = tShk % write_qs_file(sOutPathName, sFileName)
        if(iRetCode /= 0) cycle
        
        ! Inform users of progress
        print *, "Processed: ", trim(sInPathName), "/", trim(sFileName), "  (", tShk % get_size(), " data)"
        
    end do
    
    ! Inform users completion has occurred
    print *, "*** END JOB ***"
    
end program Raw_Encoder
