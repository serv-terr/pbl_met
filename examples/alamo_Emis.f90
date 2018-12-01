module Emission

	implicit none
	
	private
	
	! Public interface
	public	:: PointSource
	
	! Data types
	
	type PointSource
		real(8)		:: x
		real(8)		:: y
		real(8)		:: z
		real(8)		:: radius
		real(8)		:: vel
		real(8)		:: temp
		real(8)		:: q
	contains
		procedure	:: read				=> ptsRead
		procedure	:: isOutOfDomain	=> ptsOffDomain
	end type PointSource
	
contains

	function ptsRead(this, iLUN) result(iRetCode)
	
		! Routine's arguments
		class(PointSource), intent(out)	:: this
		integer, intent(in)				:: iLUN
		integer							:: iRetCode
		
		! Locals
		integer	:: iErrCode
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Get one data line
		read(iLUN, *, iostat=iErrCode) &
			this % x, this % y, this % z, &
			this % q, &
			this % radius, &
			this % temp, &
			this % vel
		if(iErrCode /= 0) then
			iRetCode = -1
			return
		end if
		
		! Perform basic validation
		if(this % q < 0.d0) then
			iRetCode = 1
			return
		end if
		if(this % radius < 0.d0) then
			iRetCode = 2
			return
		end if
		if(this % temp < -100.d0) then
			iRetCode = 3
			return
		end if
		if(this % vel < 0.d0) then
			iRetCode = 4
			return
		end if
		
	end function ptsRead
	
	
	function ptsOffDomain(this, x0, x1, y0, y1, zmax) result(lIsOff)
	
		! Routine arguments
		class(PointSource), intent(in)	:: this
		real(8), intent(in)				:: x0
		real(8), intent(in)				:: x1
		real(8), intent(in)				:: y0
		real(8), intent(in)				:: y1
		real(8), intent(in)				:: zmax
		logical							:: lIsOff
		
		! Locals
		! --none--
		
		! Get the information desired
		lIsOff = (this % x < x0 .or. this % x > x1 .or. this % y < y0 .or. this % y > y1 .or. this % z < 0.d0 .or. this % z > zmax)
		
	end function ptsOffDomain
	
end module Emission
