! multires_stat - procedure for computing variance stats using multi-resolution
program multires_stat
    
    use pbl_met
    use multires
    use shakeup_data
    
    implicit none
    
    ! Locals
    character(len=256)                              :: sInPath
    character(len=256)                              :: sOutFile
    character(len=16)                               :: sBuffer
    character(len=256), dimension(:), allocatable   :: svFile
    integer                                         :: iYear
    integer                                         :: iFile
    type(shakeup_raw_data)                          :: tShk
    integer                                         :: iRetCode
    real(8), dimension(:), allocatable              :: rvTimeStamp
    real, dimension(:), allocatable                 :: rvU
    real, dimension(:), allocatable                 :: rvV
    real, dimension(:), allocatable                 :: rvW
    real, dimension(:), allocatable                 :: rvT
    type(signal)                                    :: tSignalU
    type(signal)                                    :: tSignalV
    type(signal)                                    :: tSignalW
    type(signal)                                    :: tSignalT
    real                                            :: rOriginalVarU
    real, dimension(:), allocatable                 :: rvVarU
    real                                            :: rResidualVarU
    real                                            :: rOriginalVarV
    real, dimension(:), allocatable                 :: rvVarV
    real                                            :: rResidualVarV
    real                                            :: rOriginalVarW
    real, dimension(:), allocatable                 :: rvVarW
    real                                            :: rResidualVarW
    real                                            :: rOriginalVarT
    real, dimension(:), allocatable                 :: rvVarT
    real                                            :: rResidualVarT
    real(8)                                         :: rMultiplierU
    real(8)                                         :: rMultiplierV
    real(8)                                         :: rMultiplierW
    real(8)                                         :: rMultiplierT
    real(8)                                         :: rOffsetU
    real(8)                                         :: rOffsetV
    real(8)                                         :: rOffsetW
    real(8)                                         :: rOffsetT
    integer                                         :: iLUN
    integer                                         :: iFileNameLen
    
    ! Get command line arguments
    if(command_argument_count() /= 3) then
        print *, "multires_stat - Procedure for multi-resolution analysis of a fast sonic data set"
        print *
        print *, "Usage:"
        print *
        print *, "  ./multires_stat <Input_Path> <Year> <Output_File>"
        print *
        print *, "Copyright 2022 by Patrizia Favaron"
        print *, "This is open-source software, covered by the MIT license"
        print *
        stop
    end if
    call get_command_argument(1, sInPath)
    call get_command_argument(2, sBuffer)
    read(sBuffer, *, iostat=iRetCode) iYear
    call get_command_argument(3, sOutFile)
    
    ! Map "fast sonic" data files
    iRetCode = meteoflux_map_dir(sInPath, iYear, svFile, iFileTypeIn=2)
    if(iRetCode /= 0) then
        print *, "Error: No file mapped in directory - Return code = ", iRetCode
        stop
    end if
    print *, "Files mapped: ", size(svFile)
    
    ! Main loop: iterate over files
    open(newunit=iLUN, file=sOutFile, status='unknown', action='write')
    write(iLUN, "('Time.Stamp, Utv, Umv, Urv, Vtv, Vmv, Vrv, Wtv, Wmv, Wrv, Ttv, Tmv, Trv')")
    do iFile = 1, size(svFile)
        
        ! Gather data from hourly file, and check them for time stamp monotonicity
        iRetCode = tShk % read_qs_file(sInPath, svFile(iFile))
        if(iRetCode /= 0) then
            print *, "Error accessing file ", trim(sInPath), "/", trim(svFile(iFile)), " - Return code = ", iRetCode
            cycle
        end if
        
        ! Extract valid data and their time stamps
        iRetCode = tShk % access_data(rvTimeStamp, rvU, rvV, rvW, rvT)
        if(iRetCode /= 0) then
            print *, "Error extracting valid data - Return code = ", iRetCode
            cycle
        end if
        
        ! Remove trend
        iRetCode = RemoveLinearTrend(rvTimeStamp, rvU, rMultiplierU, rOffsetU)
        if(iRetCode /= 0) then
            print *, "Error removing trend on U - Return code = ", iRetCode
            cycle
        end if
        iRetCode = RemoveLinearTrend(rvTimeStamp, rvV, rMultiplierV, rOffsetV)
        if(iRetCode /= 0) then
            print *, "Error removing trend on V - Return code = ", iRetCode
            cycle
        end if
        iRetCode = RemoveLinearTrend(rvTimeStamp, rvW, rMultiplierW, rOffsetW)
        if(iRetCode /= 0) then
            print *, "Error removing trend on W - Return code = ", iRetCode
            cycle
        end if
        iRetCode = RemoveLinearTrend(rvTimeStamp, rvT, rMultiplierT, rOffsetT)
        if(iRetCode /= 0) then
            print *, "Error removing trend on T - Return code = ", iRetCode
            cycle
        end if
        
        ! Create multi-resolution objects
        iRetCode = tSignalU % create(rvTimeStamp, rvU)
        if(iRetCode /= 0) then
            print *, "Signal for U not created, with return code = ", iRetCode
            cycle
        end if
        iRetCode = tSignalV % create(rvTimeStamp, rvV)
        if(iRetCode /= 0) then
            print *, "Signal for V not created, with return code = ", iRetCode
            cycle
        end if
        iRetCode = tSignalW % create(rvTimeStamp, rvW)
        if(iRetCode /= 0) then
            print *, "Signal for W not created, with return code = ", iRetCode
            cycle
        end if
        iRetCode = tSignalT % create(rvTimeStamp, rvT)
        if(iRetCode /= 0) then
            print *, "Signal for T not created, with return code = ", iRetCode
            cycle
        end if
        
        ! Extract and print information
        iRetCode = tSignalU % get_variances(rOriginalVarU, rvVarU, rResidualVarU)
        if(iRetCode /= 0) then
            print *, "Variances of U not created, with return code = ", iRetCode
            cycle
        end if
        iRetCode = tSignalV % get_variances(rOriginalVarV, rvVarV, rResidualVarV)
        if(iRetCode /= 0) then
            print *, "Variances of V not created, with return code = ", iRetCode
            cycle
        end if
        iRetCode = tSignalW % get_variances(rOriginalVarW, rvVarW, rResidualVarW)
        if(iRetCode /= 0) then
            print *, "Variances of W not created, with return code = ", iRetCode
            cycle
        end if
        iRetCode = tSignalT % get_variances(rOriginalVarT, rvVarT, rResidualVarT)
        if(iRetCode /= 0) then
            print *, "Variances of T not created, with return code = ", iRetCode
            cycle
        end if
        
        ! Write data
        iFileNameLen = len_trim(svFile(iFile))
        write(iLUN, "(a,2('-',a),1x,a,':00:00',12(',',e15.7))") &
            svFile(iFile)(iFileNameLen-14:iFileNameLen-11), &
            svFile(iFile)(iFileNameLen-10:iFileNameLen-9), &
            svFile(iFile)(iFileNameLen-8:iFileNameLen-7), &
            svFile(iFile)(iFileNameLen-5:iFileNameLen-4), &
            rOriginalVarU, maxval(rvVarU), rResidualVarU, &
            rOriginalVarV, maxval(rvVarV), rResidualVarV, &
            rOriginalVarW, maxval(rvVarW), rResidualVarW, &
            rOriginalVarT, maxval(rvVarT), rResidualVarT
        
        ! Inform user
        print *, "Processed: ", trim(svFile(iFile))
        
    end do
    close(iLUN)
    
    ! Inform users the job is done
    print *, "*** END JOB ***"

end program multires_stat
