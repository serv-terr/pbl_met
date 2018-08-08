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
    public	:: WindCorrelation
    ! 2.Stability
    
    ! Constants (please do not change)
    
    integer, parameter	:: USTAR_PERMISSIVE = 0
    integer, parameter	:: USTAR_FINICKY    = 1
    
contains

	function FrictionVelocity(tEc, iMode, rvUstar) result(iRetCode)
	
		! Routine arguments
		type(EddyCovData), intent(in)					:: tEc
		integer, intent(in), optional					:: iMode
		real(8), dimension(:), allocatable, intent(out)	:: rvUstar
		integer											:: iRetCode
		
		! Locals
		integer							:: i
		integer							:: iErrCode
		integer							:: iModeIn
		integer							:: n
		real(8), dimension(:), allocatable	:: rvUW
		real(8), dimension(:), allocatable	:: rvVW
		
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
			rvUstar = (rvUW**2 + rvVW**2)**0.25d0
		case(USTAR_FINICKY)
			where(rvUW < 0.)
				rvUstar = sqrt(-rvUW)
			elsewhere
				rvUstar = NaN_8
			endwhere
		case default
			iRetCode = 2
		end select
		
	end function FrictionVelocity
    

	function SensibleHeatFlux(tEc, rvH0) result(iRetCode)
	
		! Routine arguments
		type(EddyCovData), intent(in)					:: tEc
		real(8), dimension(:), allocatable, intent(out)	:: rvH0
		integer											:: iRetCode
		
		! Locals
		integer								:: iErrCode
		integer								:: iModeIn
		integer								:: i
		integer								:: n
		real(8), dimension(:), allocatable	:: rvWT
		real(8), dimension(:), allocatable	:: rvTemp
		
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
			rvH0(i) = rvWT(i) * RhoCp(real(rvTemp(i),kind=4) + 273.15)
		end do
		
	end function SensibleHeatFlux
    

	function WindCorrelation(tEc, raWindCorr) result(iRetCode)
	
		! Routine arguments
		type(EddyCovData), intent(in)						:: tEc
		real(8), dimension(:,:,:), allocatable, intent(out)	:: raWindCorr
		integer												:: iRetCode
		
		! Locals
		integer								:: iErrCode
		integer								:: iModeIn
		integer								:: i
		integer								:: j
		integer								:: k
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check something can be made
		if(.not. tEc % isFull()) then
			iRetCode = 1
			return
		end if
		
		! Reserve workspace
		iErrCode = tEc % getRotCovWind(raWindCorr)
		if(iErrCode /= 0) then
			iRetCode = 2
			return
		end if
		
		! Compute the three correlation coefficients, and leave variances unscaled
		! (so transform is reversible)
		do i = 1, size(raWindCorr, dim=1)
			do j = 1, 2
				do k = j+1, 3
					raWindCorr(i,j,k) = raWindCorr(i,j,k) / sqrt(raWindCorr(i,j,j)*raWindCorr(i,k,k))
					raWindCorr(i,k,j) = raWindCorr(i,j,k)
				end do
			end do
		end do
		
	end function WindCorrelation
    
end module pbl_simil
