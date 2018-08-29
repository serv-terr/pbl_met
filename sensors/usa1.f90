! Module usa1 - Access to Metek ultrasonic anemometer data, as prepared by the
! WindRecorder and MeteoFlux Core V2 data loggers, by Servizi Territorio srl.
!
! This module is part of the pbl_met library.
!
! Copyright 2018 by Servizi Territorio srl
!
! This is open-source code, covered by lGPL 3.0 license
!
module usa1

	use pbl_met

	implicit none
	
	private
	
	! Public interface
	! 1. Data types
	public	:: Usa1DataDir
	! 2. Constants
	public	:: DE_FIRST
	public	:: DE_NEXT
	public	:: DE_ERR
	public	:: LOGGER_WINDRECORDER
	public	:: LOGGER_METEOFLUXCORE_V2
	
	! Constants
	integer, parameter	:: DE_FIRST = 0
	integer, parameter	:: DE_NEXT  = 1
	integer, parameter	:: DE_ERR   = 2
	integer, parameter	:: LOGGER_WINDRECORDER     = 1
	integer, parameter	:: LOGGER_METEOFLUXCORE_V2 = 2
	
	! Data types
	
	type Usa1DataDir
	
		! Information contents
		character(len=256), private								:: sDataPath
		real(8), private										:: rTimeBase
		integer, private										:: iNumHours
		logical, private										:: lHasSubdirs
		character(len=256), dimension(:), allocatable, private	:: svFileName
		real(8), dimension(:), allocatable, private				:: rvTimeStamp
		
		! Internal state
		integer													:: iPos
		
	contains
	
		procedure	:: mapFiles		=> up_MapFiles
		procedure	:: getFile		=> up_GetFile
		procedure	:: size			=> up_Size
		
	end type Usa1DataDir
	
contains

	function up_MapFiles(this, sDataPath, rTimeBase, iNumHours, lHasSubdirs, iLoggerType) result(iRetCode)
	
		! Routine arguments
		class(Usa1DataDir), intent(inout)	:: this
		character(len=*), intent(in)		:: sDataPath
		real(8), intent(in)					:: rTimeBase
		integer, intent(in)					:: iNumHours
		logical, intent(in)					:: lHasSubdirs
		integer, intent(in), optional		:: iLoggerType	! May be LOGGER_WINDRECORDER (default) or LOGGER_METEOFLUXCORE_V2 (in which case data are assumed to be uncompressed)
		integer								:: iRetCode
		
		! Locals
		integer				:: iErrCode
		integer				:: iDataLogger
		integer				:: iHour
		real(8)				:: rCurTime
		type(DateTime)		:: tDt
		integer				:: iNumFiles
		integer				:: iFile
		character(len=256)	:: sBuffer
		logical				:: lIsFile
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check something can be made
		if(.invalid.rTimeBase .or. iNumHours <= 0 .or. rTimeBase < 0.d0) then
			iRetCode = 1
			return
		end if
		if(present(iLoggerType)) then
			if(iLoggerType <= 0 .or. iLoggerType > 2) then
				iRetCode = 2
				return
			end if
			iDataLogger = iLoggerType
		else
			iDataLogger = LOGGER_WINDRECORDER
		end if
		
		! Ensure the time base is aligned to an hour
		this % rTimeBase   = nint(rTimeBase / 3600.d0, kind=8) * 3600.d0

		! Set type's other fixed parameters
		this % sDataPath   = sDataPath
		this % iNumHours   = iNumHours
		this % lHasSubdirs = lHasSubdirs
		
		! Count the files matching the WindRecorder naming convention
		iNumFiles = 0
		do iHour = 1, iNumHours
		
			! Compute current hour
			rCurTime = rTimeBase + 3600.d0*(iHour - 1)
			iErrCode = tDt % FromEpoch(rCurTime)
			if(iErrCode /= 0) then
				iRetCode = 2
				return
			end if
			
			! Compose file name according to WindRecorder convention
			if(iDataLogger == LOGGER_WINDRECORDER) then
				if(this % lHasSubdirs) then
					write(sBuffer, "(a, '/', i4.4, i2.2, '/', i4.4, 2i2.2, '.', i2.2)") &
						trim(sDataPath), &
						tDt % iYear, tDt % iMonth, &
						tDt % iYear, tDt % iMonth, tDt % iDay, tDt % iHour
				else
					write(sBuffer, "(a, '/', i4.4, 2i2.2, '.', i2.2)") &
						trim(sDataPath), &
						tDt % iYear, tDt % iMonth, tDt % iDay, tDt % iHour
				end if
			elseif(iDataLogger == LOGGER_METEOFLUXCORE_V2) then
				if(this % lHasSubdirs) then
					write(sBuffer, "(a, '/', i4.4, i2.2, '/', i4.4, 2i2.2, '.', i2.2, 'R')") &
						trim(sDataPath), &
						tDt % iYear, tDt % iMonth, &
						tDt % iYear, tDt % iMonth, tDt % iDay, tDt % iHour
				else
					write(sBuffer, "(a, '/', i4.4, 2i2.2, '.', i2.2, 'R')") &
						trim(sDataPath), &
						tDt % iYear, tDt % iMonth, tDt % iDay, tDt % iHour
				end if
			end if
			
			! Check expected file exists and update map size
			inquire(file=sBuffer, exist=lIsFile)
			if(lIsFile) iNumFiles = iNumFiles + 1
			
		end do
		
		! Reserve workspace
		if(iNumFiles <= 0) then
			iRetCode = 3
			return
		end if
		if(allocated(this % svFileName)) deallocate(this % svFileName)
		allocate(this % svFileName(iNumFiles))
		
		! Populate the file map
		iFile = 0
		do iHour = 1, iNumHours
		
			! Compute current hour
			rCurTime = rTimeBase + 3600.d0*(iHour - 1)
			iErrCode = tDt % FromEpoch(rCurTime)
			if(iErrCode /= 0) then
				iRetCode = 2
				return
			end if
			
			! Compose file name according to the appropriate convention
			if(iDataLogger == LOGGER_WINDRECORDER) then
				if(this % lHasSubdirs) then
					write(sBuffer, "(a, '/', i4.4, i2.2, '/', i4.4, 2i2.2, '.', i2.2)") &
						trim(sDataPath), &
						tDt % iYear, tDt % iMonth, &
						tDt % iYear, tDt % iMonth, tDt % iDay, tDt % iHour
				else
					write(sBuffer, "(a, '/', i4.4, 2i2.2, '.', i2.2)") &
						trim(sDataPath), &
						tDt % iYear, tDt % iMonth, tDt % iDay, tDt % iHour
				end if
			elseif(iDataLogger == LOGGER_METEOFLUXCORE_V2) then
				if(this % lHasSubdirs) then
					write(sBuffer, "(a, '/', i4.4, i2.2, '/', i4.4, 2i2.2, '.', i2.2, 'R')") &
						trim(sDataPath), &
						tDt % iYear, tDt % iMonth, &
						tDt % iYear, tDt % iMonth, tDt % iDay, tDt % iHour
				else
					write(sBuffer, "(a, '/', i4.4, 2i2.2, '.', i2.2, 'R')") &
						trim(sDataPath), &
						tDt % iYear, tDt % iMonth, tDt % iDay, tDt % iHour
				end if
			end if
			
			! Check expected file exists and update map size
			inquire(file=sBuffer, exist=lIsFile)
			if(lIsFile) then
				iFile = iFile + 1
				this % svFileName(iFile) = sBuffer
			end if
			
		end do
		
	end function up_MapFiles
	
	
	function up_GetFile(this, iMode, sFileName) result(iRetCode)
	
		! Routine arguments
		class(Usa1DataDir), intent(inout)	:: this
		integer, intent(inout)				:: iMode
		character(len=*), intent(out)		:: sFileName
		integer								:: iRetCode
		
		! Locals
		integer	:: n
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check parameters
		if(iMode < DE_FIRST .or. iMode > DE_ERR) then
			iRetCode = 1
			return
		end if
		
		! Set position according to current mode
		select case(iMode)
		case(DE_FIRST)
			this % iPos = 1
			if(allocated(this % svFileName)) then
				n = size(this % svFileName)
				if(this % iPos > n) then
					iMode     = DE_ERR
					sFileName = ""
				else
					sFileName = this % svFileName(this % iPos)
					iMode     = DE_NEXT
				end if
			else
				iRetCode = 2
			end if
		case(DE_NEXT)
			this % iPos = this % iPos + 1
			if(allocated(this % svFileName)) then
				n = size(this % svFileName)
				if(this % iPos > n) then
					iMode     = DE_ERR
					sFileName = ""
				else
					sFileName = this % svFileName(this % iPos)
					iMode     = DE_NEXT
				end if
			else
				iRetCode = 2
			end if
		case(DE_ERR)
			sFileName = ""
		end select
		
	end function up_GetFile
	
	
	function up_Size(this) result(iSize)
	
		! Routine arguments
		class(Usa1DataDir), intent(in)	:: this
		integer							:: iSize
		
		! Locals
		! --none--
		
		! Get the information desired
		if(allocated(this % svFileName)) then
			iSize = size(this % svFileName)
		else
			iSize = 0
		end if
		
	end function up_Size
	
	
end module usa1
