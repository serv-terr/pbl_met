module Meteo

	use pbl_met

	implicit none
	
	type MetData
		real(8), dimension(:), allocatable		:: rvEpoch
		real(8), dimension(:), allocatable		:: rvTemp
		real(8), dimension(:), allocatable		:: rvVel
		real(8), dimension(:), allocatable		:: rvDir
		real(8), dimension(:), allocatable		:: rvUstar
		real(8), dimension(:), allocatable		:: rvH0
		real(8), dimension(:), allocatable		:: rvZi
	contains
		procedure	:: read => metRead
	end type MetData
	
contains

	function metRead(this, iLUN, sFileName) result(iRetCode)
	
		! Routine arguments
		class(MetData), intent(out)		:: this
		integer, intent(in)				:: iLUN
		character(len=*), intent(in)	:: sFileName
		integer							:: iRetCode
		
		! Locals
		integer				:: iErrCode
		integer				:: iNumData
		integer				:: iData
		character(len=256)	:: sBuffer
		integer				:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		real(8)				:: rSecond
		type(DateTime)		:: tDateTime
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Count data
		iNumData = 0
		open(iLUN, file=sFileName, status='old', action='read', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		do
			read(iLUN, '(a)', iostat=iErrCode) sBuffer
			if(iErrCode /= 0) exit
			iNumData = iNumData + 1
		end do
		if(iNumData <= 0) then
			iRetCode = 2
			close(iLUN)
			return
		end if
		
		! Reserve workspace
		if(allocated(this % rvEpoch)) deallocate(this % rvEpoch)
		if(allocated(this % rvTemp))  deallocate(this % rvTemp)
		if(allocated(this % rvVel))   deallocate(this % rvVel)
		if(allocated(this % rvDir))   deallocate(this % rvDir)
		if(allocated(this % rvUstar)) deallocate(this % rvUstar)
		if(allocated(this % rvH0))    deallocate(this % rvH0)
		if(allocated(this % rvZi))    deallocate(this % rvZi)
		allocate(this % rvEpoch(iNumData))
		allocate(this % rvTemp(iNumData))
		allocate(this % rvVel(iNumData))
		allocate(this % rvDir(iNumData))
		allocate(this % rvUstar(iNumData))
		allocate(this % rvH0(iNumData))
		allocate(this % rvZi(iNumData))

		! Read data		
		rewind(iLUN)
		do iData = 1, iNumData
			read(iLUN, '(a)') sBuffer
			read(sBuffer(1:19), '(i4,5(1x,i2))') iYear, iMonth, iDay, iHour, iMinute, iSecond
			rSecond = iSecond
			tDateTime = DateTime(iYear, iMonth, iDay, iHour, iMinute, rSecond)
			this % rvEpoch(iData) = tDateTime % toEpoch()
			read(sBuffer(21:), *, iostat=iErrCode) &
				this % rvTemp(iData), &
				this % rvVel(iData), &
				this % rvDir(iData), &
				this % rvUstar(iData), &
				this % rvH0(iData), &
				this % rvZi(iData)
		end do
		close(iLUN)
		
		! Perform range-based and other basic plausibility check
		do iData = 1, iNumData
			if(.valid. this % rvTemp(iData)) then
				this % rvTemp(iData) = min(max(this % rvTemp(iData), -40.), 60.)
			else
				iRetCode = 4
				return
			end if
			if(.valid. this % rvVel(iData)) then
				this % rvVel(iData) = min(max(this % rvVel(iData), 0.), 60.)
			else
				iRetCode = 5
				return
			end if
			if(.valid. this % rvDir(iData)) then
				if(this % rvDir(iData) < 0.) then
					this % rvDir(iData) = this % rvDir(iData) + 360.0
				elseif(this % rvDir(iData) > 360.) then
					this % rvDir(iData) = this % rvDir(iData) - 360.0
				end if
				this % rvTemp(iData) = min(max(this % rvTemp(iData), 0.), 360.)
			else
				iRetCode = 6
				return
			end if
			if(.valid. this % rvUstar(iData)) then
				this % rvUstar(iData) = min(max(this % rvUstar(iData), 0.), this % rvVel(iData))
			else
				iRetCode = 7
				return
			end if
			if(.valid. this % rvH0(iData)) then
				this % rvH0(iData) = min(max(this % rvH0(iData), -600.), 1500.)
			else
				iRetCode = 8
				return
			end if
			if(.valid. this % rvZi(iData)) then
				this % rvZi(iData) = min(max(this % rvZi(iData), 10.), 2000.)
			else
				iRetCode = 9
				return
			end if
		end do
		
	end function metRead

end module Meteo
