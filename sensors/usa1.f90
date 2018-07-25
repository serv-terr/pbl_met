! Module usa1 - Access to Metek ultrasonic anemometer data, as prepared by the
! WindRecorder data logger, by Servizi Territorio srl.
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
	public	:: Usa1DataDir
	
	! Data types
	
	type Usa1DataDir
		character(len=256), private								:: sDataPath
		real(8), private										:: rTimeBase
		integer, private										:: iNumHours
		logical, private										:: lHasSubdirs
		character(len=256), dimension(:), allocatable, private	:: svFileName
		real(8), dimension(:), allocatable, private				:: rvTimeStamp
	contains
		procedure	:: mapFiles		=> up_MapFiles
	end type Usa1DataDir
	
contains

	function up_MapFiles(this, sDataPath, rTimeBase, iNumHours, lHasSubdirs) result(iRetCode)
	
		! Routine arguments
		class(Usa1DataDir), intent(inout)	:: this
		character(len=*), intent(in)		:: sDataPath
		real(8), intent(in)					:: rTimeBase
		integer, intent(in)					:: iNumHours
		logical, intent(in)					:: lHasSubdirs
		integer								:: iRetCode
		
		! Locals
		integer				:: iErrCode
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
			
			! Compose file name according to WindRecorder convention
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
			
			! Check expected file exists and update map size
			inquire(file=sBuffer, exist=lIsFile)
			if(lIsFile) then
				iFile = iFile + 1
				this % svFileName(iFile) = sBuffer
			end if
			
		end do
		
	end function up_MapFiles

end module usa1
