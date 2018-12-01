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
		
	end function metRead

end module Meteo
