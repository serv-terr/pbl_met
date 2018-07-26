! pbl_simil  : Fortran module, providing support to Monin-Obukhov
!              (surface layer) similarity.
!
! This module is part of the pbl_met library.
!
! This is open-source code, covered by the lGPL 3.0 license.
!
module pbl_simil

    use pbl_base
    use pbl_wind
    use pbl_thermo

    implicit none
    
    private
    
    ! Public interface
    ! 0.Constants
    public	:: USTAR_PERMISSIVE
    public	:: USTAR_FINICKY
    ! 1.Turbulence
    public	:: FrictionVelocity
    public	:: SensibleHeatFlux
    ! 2.Stability
    
    ! Constants (please do not change)
    
    integer, parameter	:: USTAR_PERMISSIVE = 0
    integer, parameter	:: USTAR_FINICKY    = 1
    
contains

	function FrictionVelocity(tEc, iMode, rvUstar) result(iRetCode)
	
		! Routine arguments
		type(EddyCovData), intent(in)					:: tEc
		integer, intent(in), optional					:: iMode
		real, dimension(:), allocatable, intent(out)	:: rvUstar
		integer											:: iRetCode
		
		! Locals
		integer							:: i
		integer							:: iErrCode
		integer							:: iModeIn
		integer							:: n
		real, dimension(:), allocatable	:: rvUW
		real, dimension(:), allocatable	:: rvVW
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check something can be made
		if(.not. tEc % isFull()) then
			iRetCode = 1
			return
		end if
		
		! Assign mode
		if(present(iMode)) then
			iModeIn = iMode
		else
			iModeIn = USTAR_PERMISSIVE
		end if
		
		! Reserve workspace
		iErrCode = tEc % getRotCovVel(1,3,rvUW)
		if(iErrCode /= 0) then
			iRetCode = 2
			return
		end if
		iErrCode = tEc % getRotCovVel(2,3,rvVW)
		if(iErrCode /= 0) then
			iRetCode = 2
			return
		end if
		n = size(rvUW)
		if(allocated(rvUstar)) deallocate(rvUstar)
		allocate(rvUstar(n))
		
		! Compute the friction velocity
		select case(iMode)
		case(USTAR_PERMISSIVE)
			rvUstar = (rvUW**2 + rvVW**2)**0.25
		case(USTAR_FINICKY)
			where(rvUW < 0.)
				rvUstar = sqrt(-rvUW)
			elsewhere
				rvUstar = NaN
			endwhere
		case default
			iRetCode = 2
		end select
		
	end function FrictionVelocity
    

	function SensibleHeatFlux(tEc, rvH0) result(iRetCode)
	
		! Routine arguments
		type(EddyCovData), intent(in)					:: tEc
		real, dimension(:), allocatable, intent(out)	:: rvH0
		integer											:: iRetCode
		
		! Locals
		integer							:: iErrCode
		integer							:: iModeIn
		integer							:: i
		integer							:: n
		real, dimension(:), allocatable	:: rvWT
		real, dimension(:), allocatable	:: rvTemp
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check something can be made
		if(.not. tEc % isFull()) then
			iRetCode = 1
			return
		end if
		
		! Reserve workspace
		iErrCode = tEc % getRotCovT(3,rvWT)
		if(iErrCode /= 0) then
			iRetCode = 2
			return
		end if
		iErrCode = tEc % getTemp(rvTemp)
		if(iErrCode /= 0) then
			iRetCode = 2
			return
		end if
		n = size(rvWT)
		if(allocated(rvH0)) deallocate(rvH0)
		allocate(rvH0(n))
		
		! Compute the sensible heat flux on vertical direction
		do i = 1, n
			rvH0(i) = rvWT(i) * RhoCp(rvTemp(i) + 273.15)
		end do
		
	end function SensibleHeatFlux
    
end module pbl_simil
