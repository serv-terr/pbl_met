module Meteo

	use pbl_met

	implicit none
	
	type MetData
		! Original (as-read) data
		real(8), dimension(:), allocatable		:: rvEpoch
		real(8), dimension(:), allocatable		:: rvTemp
		real(8), dimension(:), allocatable		:: rvVel
		real(8), dimension(:), allocatable		:: rvDir
		real(8), dimension(:), allocatable		:: rvU
		real(8), dimension(:), allocatable		:: rvV
		real(8), dimension(:), allocatable		:: rvUstar
		real(8), dimension(:), allocatable		:: rvH0
		real(8), dimension(:), allocatable		:: rvZi
		! Thickened-basis data
		real(8), dimension(:), allocatable		:: rvExtEpoch
		real(8), dimension(:), allocatable		:: rvExtTemp
		real(8), dimension(:), allocatable		:: rvExtVel
		real(8), dimension(:), allocatable		:: rvExtDir
		real(8), dimension(:), allocatable		:: rvExtU
		real(8), dimension(:), allocatable		:: rvExtV
		real(8), dimension(:), allocatable		:: rvExtUstar
		real(8), dimension(:), allocatable		:: rvExtH0
		real(8), dimension(:), allocatable		:: rvExtZi
	contains
		procedure	:: read => metRead
	end type MetData
	
contains

	function metRead(this, iLUN, sFileName, iAveragingTime, iSubSteps, sFineMeteoFile) result(iRetCode)
	
		! Routine arguments
		class(MetData), intent(out)		:: this
		integer, intent(in)				:: iLUN
		character(len=*), intent(in)	:: sFileName
		integer, intent(in)				:: iAveragingTime
		integer, intent(in)				:: iSubSteps
		character(len=*), intent(in)	:: sFineMeteoFile
		integer							:: iRetCode
		
		! Locals
		integer				:: iErrCode
		integer				:: iNumData
		integer				:: iData
		integer				:: i
		integer				:: k
		character(len=256)	:: sBuffer
		integer				:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		real(8)				:: rSecond
		type(DateTime)		:: tDateTime
		real(8)				:: rDelta
		real(8)				:: rNewDelta
		integer				:: iNumNewAvgs
		integer				:: iNumNewData
		character(len=23)	:: sDateTime
		real, dimension(2)					:: polar
		real(8), dimension(:), allocatable	:: rvTimeIndices
		type(Spline)						:: tSpline
		
		! Constants
		real(8), parameter	:: PI = atan(1.d0)*4.d0
		
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
		if(allocated(this % rvV))     deallocate(this % rvV)
		if(allocated(this % rvU))     deallocate(this % rvU)
		if(allocated(this % rvUstar)) deallocate(this % rvUstar)
		if(allocated(this % rvH0))    deallocate(this % rvH0)
		if(allocated(this % rvZi))    deallocate(this % rvZi)
		allocate(this % rvEpoch(iNumData))
		allocate(this % rvTemp(iNumData))
		allocate(this % rvVel(iNumData))
		allocate(this % rvDir(iNumData))
		allocate(this % rvU(iNumData))
		allocate(this % rvV(iNumData))
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
		
		! Check time stamps form a regularly-spaced series
		if(iNumData > 1) then
			
			! Internal workspace
			allocate(rvTimeIndices(size(this % rvEpoch)))
			
			! Compute the time indices: they should be 0.d0, 1.d0, ..., k.d0 with
			! a first difference always equal to 1.d0 - ideally. A small shift due
			! to rounding may be accepted
			rDelta = this % rvEpoch(2) - this % rvEpoch(1)
			if(rDelta <= 0.d0) then
				iRetCode = 3
				deallocate(rvTimeIndices)
				return
			end if
			rvTimeIndices = (this % rvEpoch - this % rvEpoch(1)) / rDelta
			
			! Check the time index is really as expected
			do iData = 2, iNumData
				if((this % rvEpoch(iData) - this % rvEpoch(iData-1))/rDelta - 1.d0 > 1.d-4) then
					iRetCode = 3
					deallocate(rvTimeIndices)
					return
				end if
			end do
			
			! Prepare to exit time stamp check code block
			deallocate(rvTimeIndices)
			
		end if
		
		! Perform range-based and other basic plausibility check
		do iData = 1, iNumData
			if(.valid. this % rvTemp(iData)) then
				this % rvTemp(iData) = min(max(this % rvTemp(iData), -40.d0), 60.d0)
			else
				iRetCode = 4
				return
			end if
			if(.valid. this % rvVel(iData)) then
				this % rvVel(iData) = min(max(this % rvVel(iData), 0.d0), 60.d0)
			else
				iRetCode = 5
				return
			end if
			if(.valid. this % rvDir(iData)) then
				if(this % rvDir(iData) < 0.d0) then
					this % rvDir(iData) = this % rvDir(iData) + 360.0d0
				elseif(this % rvDir(iData) > 360.d0) then
					this % rvDir(iData) = this % rvDir(iData) - 360.0d0
				end if
				this % rvTemp(iData) = min(max(this % rvTemp(iData), 0.d0), 360.d0)
			else
				iRetCode = 6
				return
			end if
			if(.valid. this % rvUstar(iData)) then
				this % rvUstar(iData) = min(max(this % rvUstar(iData), 0.d0), this % rvVel(iData))
			else
				iRetCode = 7
				return
			end if
			if(.valid. this % rvH0(iData)) then
				this % rvH0(iData) = min(max(this % rvH0(iData), -600.d0), 1500.d0)
			else
				iRetCode = 8
				return
			end if
			if(.valid. this % rvZi(iData)) then
				this % rvZi(iData) = min(max(this % rvZi(iData), 10.d0), 2000.d0)
			else
				iRetCode = 9
				return
			end if
		end do
		
		! Generate wind components
		this % rvU = this % rvVel * sin(this % rvDir * PI / 180.d0)
		this % rvV = this % rvVel * cos(this % rvDir * PI / 180.d0)
		
		! Reserve workspace
		iNumNewAvgs = floor((this % rvEpoch(size(this % rvEpoch)) - this % rvEpoch(1)) / iAveragingTime) + 1
		iNumNewData = iNumNewAvgs * iSubSteps
		rNewDelta   = iAveragingTime / iSubSteps
		if(allocated(this % rvExtEpoch)) deallocate(this % rvExtEpoch)
		if(allocated(this % rvExtTemp))  deallocate(this % rvExtTemp)
		if(allocated(this % rvExtVel))   deallocate(this % rvExtVel)
		if(allocated(this % rvExtDir))   deallocate(this % rvExtDir)
		if(allocated(this % rvExtV))     deallocate(this % rvExtV)
		if(allocated(this % rvExtU))     deallocate(this % rvExtU)
		if(allocated(this % rvExtUstar)) deallocate(this % rvExtUstar)
		if(allocated(this % rvExtH0))    deallocate(this % rvExtH0)
		if(allocated(this % rvExtZi))    deallocate(this % rvExtZi)
		allocate(this % rvExtEpoch(iNumNewData))
		allocate(this % rvExtTemp(iNumNewData))
		allocate(this % rvExtVel(iNumNewData))
		allocate(this % rvExtDir(iNumNewData))
		allocate(this % rvExtU(iNumNewData))
		allocate(this % rvExtV(iNumNewData))
		allocate(this % rvExtUstar(iNumNewData))
		allocate(this % rvExtH0(iNumNewData))
		allocate(this % rvExtZi(iNumNewData))

		! Generate thickened time stamp series
		k = 0
		do iData = 1, iNumNewAvgs
			do i = 1, iSubSteps
				k = k + 1
				this % rvExtEpoch(k) = this % rvEpoch(1) + rNewDelta * (k-1)
			end do
		end do
		
		! Generate series of time sub-steps by spline interpolation
		iErrCode = tSpline % init(this % rvEpoch, this % rvTemp)
		if(iErrCode /= 0) then
			iRetCode = 10
			return
		end if
		iErrCode = tSpline % vectEval(this % rvExtEpoch, this % rvExtTemp)
		if(iErrCode /= 0) then
			iRetCode = 11
			return
		end if
		iErrCode = tSpline % init(this % rvEpoch, this % rvU)
		if(iErrCode /= 0) then
			iRetCode = 10
			return
		end if
		iErrCode = tSpline % vectEval(this % rvExtEpoch, this % rvExtU)
		if(iErrCode /= 0) then
			iRetCode = 11
			return
		end if
		iErrCode = tSpline % init(this % rvEpoch, this % rvV)
		if(iErrCode /= 0) then
			iRetCode = 10
			return
		end if
		iErrCode = tSpline % vectEval(this % rvExtEpoch, this % rvExtV)
		if(iErrCode /= 0) then
			iRetCode = 11
			return
		end if
		iErrCode = tSpline % init(this % rvEpoch, this % rvUstar)
		if(iErrCode /= 0) then
			iRetCode = 10
			return
		end if
		iErrCode = tSpline % vectEval(this % rvExtEpoch, this % rvExtUstar)
		if(iErrCode /= 0) then
			iRetCode = 11
			return
		end if
		iErrCode = tSpline % init(this % rvEpoch, this % rvH0)
		if(iErrCode /= 0) then
			iRetCode = 10
			return
		end if
		iErrCode = tSpline % vectEval(this % rvExtEpoch, this % rvExtH0)
		if(iErrCode /= 0) then
			iRetCode = 11
			return
		end if
		iErrCode = tSpline % init(this % rvEpoch, this % rvZi)
		if(iErrCode /= 0) then
			iRetCode = 10
			return
		end if
		iErrCode = tSpline % vectEval(this % rvExtEpoch, this % rvExtZi)
		if(iErrCode /= 0) then
			iRetCode = 11
			return
		end if
		
		! Compute polar wind given vector
		do i = 1, size(this % rvExtEpoch)
			polar = CartesianToPolar2( real([this % rvExtU(i), this % rvExtV(i)], kind=4) )
			this % rvExtVel(i) = polar(1)
			this % rvExtDir(i) = polar(2)
		end do
		
		! Print meteo data, if requested
		if(sFineMeteoFile /= ' ') then
			open(iLUN, file=sFineMeteoFile, status='unknown', action='write', iostat=iErrCode)
			if(iErrCode /= 0) then
				iRetCode = 12
				return
			end if
			do i = 1, size(this % rvExtEpoch)
				iErrCode = tDateTime % fromEpoch(this % rvExtEpoch(i))
				sDateTime = tDateTime % toISO()
				write(iLUN, "(a, 6(',', f9.4))") &
					sDateTime, &
					this % rvExtTemp(i), &
					this % rvExtVel(i), &
					this % rvExtDir(i), &
					this % rvExtUstar(i), &
					this % rvExtH0(i), &
					this % rvExtZi(i)
			end do
			close(iLUN)
		end if
			
		! Construct profiles
		
	end function metRead
	
	! **********************
	! * Auxiliary routines *
	! **********************

end module Meteo
