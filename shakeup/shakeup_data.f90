! Module supporting the scan and gathering of uncompressed
! SHAKEUP raw data.
!
! SHAKEUP raw data files may contain "analog" values, which
! should not be retained.
!
module shakeup_data
    
    implicit none
    
    private
    
    ! Public interface
    public  :: meteoflux_map_dir    ! Procedure, to list all MeteoFlux Core V2 raw data files
    public  :: shakeup_raw_data     ! Data type to read and write MeteoFlux Core V2 in various forms
    public  :: metek_build_dirs     ! Procedure generating Metek-style sub-directories
    
    ! Data types
    
    type shakeup_raw_data
        private
        logical                                         :: lIsComplete = .false.
        integer, dimension(:), allocatable              :: ivTimeStamp
        real, dimension(:), allocatable                 :: rvU
        real, dimension(:), allocatable                 :: rvV
        real, dimension(:), allocatable                 :: rvW
        real, dimension(:), allocatable                 :: rvT
    contains
        procedure   :: read_mfc2_file   => shrd_read_mfc2_file
        procedure   :: write_qs_file    => shrd_write_qs_file
        procedure   :: get_size         => shrd_get_size
    end type shakeup_raw_data
    
contains

    function meteoflux_map_dir(sPathName, iYear, svFile) result(iRetCode)
        
        ! Routine arguments
        character(len=*), intent(in)                                :: sPathName
        integer, intent(in)                                         :: iYear
        character(len=256), dimension(:), allocatable, intent(out)  :: svFile
        integer                                                     :: iRetCode
        
        ! Locals
        character(len=256)  :: sFileName
        character(len=256)  :: sMetekName
        integer             :: iNumFiles
        integer             :: iFile
        integer             :: iMonth
        integer             :: iDay
        integer             :: iHour
        logical             :: lFileExists
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! First pass: count all files in Metek-style sub-directories within path;
        ! the count will be used to reserve file name space
        iNumFiles = 0
        do iMonth = 1, 12
            
            ! Loop over possible days (1 to 31, with no calendar computation:
            ! quite harsh, but it saves some time and stays simple
            do iDay = 1, 31
                
                ! Loop over hours
                do iHour = 0, 23
                    
                    ! Form file name (with full path, according to "Metek" convention
                    write(sFileName, "(a,'/',i4.4,i2.2,'/',i4.4,i2.2,i2.2,'.',i2.2,'R')") &
                        trim(sPathName), iYear, iMonth, &
                        iYear, iMonth, iDay, iHour
                        
                    ! Check file exists, and increment file counter if so
                    inquire(file=sFileName, exist=lFileExists)
                    if(.not.lFileExists) cycle
                    iNumFiles = iNumFiles + 1
                        
                end do
                
            end do
            
        end do
        
        ! Reserve file names space
        if(iNumFiles <= 0) then
            iRetCode = 1
            return
        end if
        if(allocated(svFile)) deallocate(svFile)
        allocate(svFile(iNumFiles))
        
        ! Second pass: generate all existing file names
        iFile = 0
        do iMonth = 1, 12
            
            ! Loop over possible days (1 to 31, with no calendar computation:
            ! quite harsh, but it saves some time and stays simple
            do iDay = 1, 31
                
                ! Loop over hours
                do iHour = 0, 23
                    
                    ! Form file name (with full path, according to "Metek" convention
                    write(sMetekName, "(i4.4,i2.2,'/',i4.4,i2.2,i2.2,'.',i2.2,'R')") &
                        iYear, iMonth, &
                        iYear, iMonth, iDay, iHour
                    write(sFileName, "(a,'/',a)") &
                        trim(sPathName), trim(sMetekName)
                        
                    ! Check file exists, and save it if in case
                    inquire(file=sFileName, exist=lFileExists)
                    if(.not.lFileExists) cycle
                    iFile = iFile + 1
                    svFile(iFile) = sMetekName
                    
                end do
                
            end do
            
        end do
        
    end function meteoflux_map_dir
    
    
    function shrd_read_mfc2_file(this, sDataPath, sFile) result(iRetCode)
        
        ! Routine arguments
        class(shakeup_raw_data), intent(out)    :: this
        character(len=*), intent(in)            :: sDataPath
        character(len=*), intent(in)            :: sFile
        integer                                 :: iRetCode
        
        ! Locals
        integer             :: iErrCode
        character(len=256)  :: sFileName
        integer             :: iLUN
        integer(2)          :: iTimeStamp
        integer(2)          :: iU
        integer(2)          :: iV
        integer(2)          :: iW
        integer(2)          :: iT
        integer             :: iNumData
        integer             :: iData
        character(len=256)  :: sErrMsg
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Form file name, and use it to get data
        write(sFileName, "(a,'/',a)") trim(sDataPath), trim(sFile)
        ! -1- First pass: count data
        open(newunit=iLUN, file=sFileName, status='old', action='read', access='stream', iostat=iErrCode, iomsg=sErrMsg)
        if(iErrCode /= 0) then
            print *, trim(sErrMsg)
            iRetCode = 1
            return
        end if
        iNumData = 0
        do
            read(iLUN, iostat=iErrCode) iTimeStamp, iV, iU, iW, iT
            if(iErrCode /= 0) exit
            if(iTimeStamp < 5000) iNumData = iNumData + 1
        end do
        if(iNumData <= 0) then
            iRetCode = 2
            return
        end if
        rewind(iLUN)
        ! -1- Reserve workspace
        if(allocated(this % ivTimeStamp)) deallocate(this % ivTimeStamp)
        if(allocated(this % rvU))         deallocate(this % rvU)
        if(allocated(this % rvV))         deallocate(this % rvV)
        if(allocated(this % rvW))         deallocate(this % rvW)
        if(allocated(this % rvT))         deallocate(this % rvT)
        allocate(this % ivTimeStamp(iNumData))
        allocate(this % rvU(iNumData))
        allocate(this % rvV(iNumData))
        allocate(this % rvW(iNumData))
        allocate(this % rvT(iNumData))
        ! -1- Get actual data
        iData = 0
        do
            read(iLUN, iostat=iErrCode) iTimeStamp, iV, iU, iW, iT
            if(iErrCode /= 0) exit
            if(iTimeStamp < 5000) then
                iData = iData + 1
                this % ivTimeStamp(iData) = iTimeStamp
                this % rvU(iData) = iU / 100.0
                this % rvV(iData) = iV / 100.0
                this % rvW(iData) = iW / 100.0
                this % rvT(iData) = iT / 100.0
            end if
        end do
        close(iLUN)
        
        ! Inform users all was right
        this % lIsComplete = .true.
        
    end function shrd_read_mfc2_file
    
    
    function shrd_write_qs_file(this, sDataPath, sFile) result(iRetCode)
        
        ! Routine arguments
        class(shakeup_raw_data), intent(in)     :: this
        character(len=*), intent(in)            :: sDataPath
        character(len=*), intent(in)            :: sFile
        integer                                 :: iRetCode
        
        ! Locals
        integer             :: iErrCode
        character(len=256)  :: sFileName
        integer             :: iLUN
        character(len=256)  :: sErrMsg
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Check something can be made
        if(.not.this % lIsComplete) then
            iRetCode = 1
            return
        end if
        
        ! Form file name, and use it to store data
        write(sFileName, "(a,'/',a)") trim(sDataPath), trim(sFile)
        open(newunit=iLUN, file=sFileName, status='unknown', action='write', access='stream', iostat=iErrCode, iomsg=sErrMsg)
        if(iErrCode /= 0) then
            print *, trim(sErrMsg)
            iRetCode = 2
            return
        end if
        
        ! Write data header
        write(iLUN, iostat=iErrCode, iomsg=sErrMsg) size(this % ivTimeStamp)
        if(iErrCode /= 0) then
            print *, trim(sErrMsg)
            iRetCode = 3
            return
        end if
        
        ! Write actual data
        write(iLUN, iostat=iErrCode, iomsg=sErrMsg) &
            this % ivTimeStamp, &
            this % rvU, &
            this % rvV, &
            this % rvW, &
            this % rvT
        if(iErrCode /= 0) then
            print *, trim(sErrMsg)
            iRetCode = 4
            return
        end if
        
        ! Leave
        close(iLUN)

    end function shrd_write_qs_file
    
    
    function shrd_get_size(this) result(iSize)
        
        ! Routine arguments
        class(shakeup_raw_data), intent(in)     :: this
        integer                                 :: iSize
        
        ! Locals
        ! --none--
        
        ! Get the information desired
        if(.not.this % lIsComplete) then
            iSize = 0
        else
            iSize = size(this % ivTimeStamp)
        end if
        
    end function shrd_get_size
    
    
    function metek_build_dirs(sDataPath, iYear) result(iRetCode)
        
        ! Routine arguments
        character(len=*), intent(in)    :: sDataPath
        integer, intent(in)             :: iYear
        integer                         :: iRetCode
        
        ! Locals
        character(len=256)  :: sPathName
        integer             :: iMonth
        
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        ! Iterate over months in data directory, and create their directories
        do iMonth = 1, 12
            write(sPathName, "(a,'/',i4.4,i2.2)") trim(sDataPath), iYear, iMonth
            call system('mkdir ' // trim(sPathName))
        end do
        
    end function metek_build_dirs
    
end module shakeup_data
