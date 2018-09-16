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
    public	:: MTD_BUSINGER
    public	:: MTD_VANULDEN_HOLTSLAG
    public	:: MTD_BELIJAARS_HOLTSLAG
    public	:: MTD_CARL
    public	:: MOL_H2O
    public	:: MOL_CO2
    public	:: MOL_AIR
    
    ! 1.Turbulence
    public	:: FrictionVelocity
    public	:: SensibleHeatFlux
    public	:: WaterCarbonDioxide
    public	:: WindCorrelation
    ! 2.Stability, and stability-related
    public	:: wStar
    ! 3.Universal similarity functions
    public	:: psih
    public	:: psim
    
    ! Constants (please do not change)
    
    integer, parameter	:: USTAR_PERMISSIVE = 0
    integer, parameter	:: USTAR_FINICKY    = 1
    
    integer, parameter	:: MTD_BUSINGER           = 1
    integer, parameter	:: MTD_VANULDEN_HOLTSLAG  = 2
    integer, parameter	:: MTD_BELIJAARS_HOLTSLAG = 3
    integer, parameter	:: MTD_CARL               = 4
    
    real(8), parameter	:: MOL_AIR = 28.96d0
    real(8), parameter	:: MOL_H2O = 18.0153d0
    real(8), parameter	:: MOL_CO2 = 44.0100d0
    
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
	
	
	function WaterCarbonDioxide(tEc, rZ, rvFqMolar, rvFqMass, rvFcMolar, rvFcMass, rvH0, rvHe) result(iRetCode)

		! Routine arguments
		type(EddyCovData), intent(in)					:: tEc
		real(8), intent(in)								:: rZ			! [m]
		real(8), dimension(:), allocatable, intent(out)	:: rvFqMolar	! [mmol/(m2 s)]
		real(8), dimension(:), allocatable, intent(out)	:: rvFqMass		! [mg/(m2 s)]
		real(8), dimension(:), allocatable, intent(out)	:: rvFcMolar	! [mmol/(m2 s)]
		real(8), dimension(:), allocatable, intent(out)	:: rvFcMass		! [mg/(m2 s)]
		real(8), dimension(:), allocatable, intent(out)	:: rvH0			! [W/m2]
		real(8), dimension(:), allocatable, intent(out)	:: rvHe			! [W/m2]
		integer											:: iRetCode
		
		! Locals
		integer									:: iErrCode
		integer									:: iModeIn
		integer									:: i
		integer									:: n
		real(8), dimension(:), allocatable		:: rvTa
		real(8)									:: tc
		real(8)									:: qd
		real(8)									:: qc
		real(8)									:: cd
		real(8)									:: cdv
		real(8)									:: mix_factor
		real(8)									:: wt_cor
		real(8)									:: lambda
		real(8), dimension(:), allocatable		:: rvRotCovT
		real(8), dimension(:), allocatable		:: rvQ
		real(8), dimension(:,:), allocatable	:: rmCovQ
		real(8), dimension(:), allocatable		:: rvVarQ
		real(8), dimension(:,:), allocatable	:: rmRotCovQ
		real(8), dimension(:), allocatable		:: rvC
		real(8), dimension(:,:), allocatable	:: rmCovC
		real(8), dimension(:), allocatable		:: rvVarC
		real(8), dimension(:,:), allocatable	:: rmRotCovC
		integer, dimension(:), allocatable		:: ivNumData
		real									:: rPa
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check something can be made
		if(.not. tEc % isFull()) then
			iRetCode = 1
			return
		end if
		
		! Retrieve EddyCovData values to work on
		iErrCode = tEc % getTemp(rvTa)
		if(iErrCode /= 0) then
			iRetCode = 2
			return
		end if
		rvTa = rvTa + 273.15
		iErrCode = tEc % getRotCovT(3, rvRotCovT)
		if(iErrCode /= 0) then
			iRetCode = 3
			return
		end if
		iErrCode = tEc % getInputGases(ivNumData, rvQ, rmCovQ, rvVarQ, rvC, rmCovC, rvVarC)
		if(iErrCode /= 0) then
			iRetCode = 4
			return
		end if
		iErrCode = tEc % getOutputGases(rmRotCovQ, rmRotCovC)
		if(iErrCode /= 0) then
			iRetCode = 5
			return
		end if
		
		! Reserve workspace
		n = tEc % getSize()
		if(allocated(rvFqMolar)) deallocate(rvFqMolar)
		if(allocated(rvFqMass))  deallocate(rvFqMass)
		if(allocated(rvH0))     deallocate(rvH0)
		if(allocated(rvHe))     deallocate(rvHe)
		allocate(rvFqMolar(n))
		allocate(rvFqMass(n))
		allocate(rvH0(n))
		allocate(rvHe(n))
		
		! Main loop: period processing
		rPa = AirPressure(real(rZ, kind=4))
		do i = 1, n
		
			! Air molar concentration
			cd = 1000.0d0*AirDensity(real(rvTa(i), kind=4), real(rPa, kind=4))/MOL_Air	! [g/m3] / [g/mmol] = [g/m3] * [mmol/g] = [mmol/m3]

			! Water specific processing
			qd  = rvQ(i) / cd														! Adimensional ratio
			cdv = cd + rvQ(i)														! Molar concentration of moist air [mmol/m3]
			tc  = cdv * rvRotCovT(i) / rvTa(i)										! [mmol/m3] [m K/s] / [K] = [mmol/(m2 s)]
			rvFqMolar(i) = rmRotCovQ(i,3) + qd * (tc + rvRotCovT(i))				! [mmol/(m2 s)]
			rvFqMass(i)  = MOL_H2O * rvFqMolar(i)									! [mg/(m2 s)]
    
			! Thermal effect correction (Schotanus)
			mix_factor = MOL_H2O/AirDensity(real(rvTa(i), kind=4), real(rPa, kind=4))/1000.d0
			wt_cor     = rvRotCovT(i) - 0.51d0 * mix_factor * rvTa(i) * rmRotCovQ(i,3)
			rvH0(i)    = RhoCp(real(rvTa(i), kind=4), real(rPa, kind=4)) * wt_cor

			! Latent heat
			lambda     = 2500.8d0 - 2.36d0 * rvTa(i) + 0.0016d0 * rvTa(i)**2 - 0.00006d0 * rvTa(i)**3	! Latent condensation heat foe water [J/g] (temperature in °C)
			rvHe(i)    = lambda/1000.d0 * rvFqMass(i)
    
			! Carbon dioxide specific processing
			qc = rvC(i) / cd											! Dimensionless ratio
			rvFcMolar(i) = rmRotCovC(i,3) + qc * (tc + rmRotCovQ(i,3))	! [mmol/(m2 s)]
			rvFcMass(i)  = MOL_CO2 * rvFcMolar(i)						! [mg/(m2 s)]
    
  		end do

	end function WaterCarbonDioxide
    

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
	

	Function wStar(Ta,H0,zi) result(ws)
	
		! Routine arguments
		real, intent(in)	:: Ta	! Air temperature (K)
		real, intent(in)	:: H0	! Turbulent sensible heat flux (W/m2)
		real, intent(in)	:: zi	! Mixing height (mù
		real				:: ws
		
		! Locals
		real	:: rc
		
		! Constants
		real, parameter	:: G = 9.807
		
		! Check something can be made
		ws = NaN
		if(Ta <= 0. .or. zi < 0.) return
		
		! Compute the Deardoff velocity
		if(H0 < 0.) then
			ws = 0.
		else
			rc = RhoCp(Ta)
			if(.invalid.rc) return
			ws = (G * zi * H0 / (rc * Ta))**0.33333
		end if

	end function wStar
      
      
	function psih(zr, L, method) result(rPsiH)
	
		! Routine arguments
		real, intent(in)				:: zr		! Reference height (m)
		real, intent(in)				:: L		! Obukhov length (m)
		integer, intent(in), optional	:: method	! Method for stable part (MTD_BUSINGER: Businger; MTD_VANULDEN_HOLTSLAG:van Ulden-Holtslag, default; MTD_BELIJAARS_HOLTSLAG:Belijaars-Holtslag)
		real							:: rPsiH
		
		! Locals
		real	:: S	! Stability parameter at height 'zr'
		real	:: y
		real	:: exz
		real	:: c1
		real	:: c2
		real	:: c3
		integer	:: iMethod
		
		! Check something can be made
		rPsiH = NaN
		if(L /= 0.0) return
		S     = zr/L
		
		! Compute the desired quantity
		if(L < 0.) then
		
			! Convective part by Businger relation
			y = sqrt(1.-16.*S)
			rPsiH = 2.*alog((1.+y)/2.)
			
		else
		
			! Assign actual method
			if(present(method)) then
				iMethod = method
			else
				iMethod = MTD_VANULDEN_HOLTSLAG
			end if
			
			! Perform computing
			select case(iMethod)
			case(MTD_BUSINGER)
				rPsiH = -5. * S
			case(MTD_VANULDEN_HOLTSLAG)
				rPsiH = -17. * (1.-exp(-0.29*S))
			case(MTD_BELIJAARS_HOLTSLAG)
				exz = exp(-0.35*S)
				c1  = 1.+0.666667*S
				c2  = S-5./0.35
				c3  = 0.667*5./0.35
				rPsiH = -c1**1.5 - 0.667*c2*exz - c3 + 1.
			end select
		
		end if
		
	end function psih
    

	function psim(zr, L, stableMethod, convectiveMethod) result(rPsiM)
	
		! Routine arguments
		real, intent(in)				:: zr				! Reference height (m)
		real, intent(in)				:: L				! Obukhov length (m)
		integer, intent(in), optional	:: stableMethod		! Method for stable part (MTD_BUSINGER: Businger; MTD_VANULDEN_HOLTSLAG:van Ulden-Holtslag, default; MTD_BELIJAARS_HOLTSLAG:Belijaars-Holtslag)
		integer, intent(in), optional	:: convectiveMethod	! Method for convective part (MTD_BUSINGER: Businger, default; MTD_VANULDEN_HOLTSLAG:van Ulden-Holtslag; MTD_CARL:Carl)
		real							:: rPsiM
		
		! Locals
		real	:: S	! Stability parameter at height 'zr'
		real	:: x
		integer	:: iMethodStable
		integer	:: iMethodConvective
		
		! Constant
		real, parameter :: a = 0.7
		real, parameter :: b = 0.75
		real, parameter :: c = 5.
		real, parameter :: d = 0.35
		
		! Check something can be made
		rPsiM = NaN
		if(L /= 0.0) return
		S     = zr/L
		
		! Compute the desired quantity
		if(L < 0.) then ! Convective part
		
			! Assign actual method
			if(present(convectiveMethod)) then
				iMethodConvective = convectiveMethod
			else
				iMethodConvective = MTD_BUSINGER
			end if
			
			! Perform computing
			select case(iMethodConvective)
			case(MTD_BUSINGER)
				x    = (1.-16.*S)**0.25
				rPsiM = log((1.+x*x)/2.*((1.+x)/2.)**2) - 2.*atan(x) + 1.570796
			case(MTD_VANULDEN_HOLTSLAG)
				rPsiM = (1.-16.*S)**0.25 - 1.
			case(MTD_BELIJAARS_HOLTSLAG)
				x      = (1.-16.*S)**0.3333
				rPsiM = 1.5 * log(x*x+x+1.) - 1.7320508 * atan((2.*x+1)/1.7320508) + 0.165881
			end select
			
		else ! Stable
		
			! Assign actual method
			if(present(stableMethod)) then
				iMethodStable = stableMethod
			else
				iMethodStable = MTD_VANULDEN_HOLTSLAG
			end if
			
			! Perform computing
			select case(iMethodStable)
			case(MTD_BUSINGER)
				rPsiM = -5.*S
			case(MTD_VANULDEN_HOLTSLAG)
				rPsiM = -17. * (1.-exp(-0.29*S))
			case(MTD_CARL)
				rPsiM = -a*S-b*(S-c/d)*exp(-d*S)-b*c/d
			end select
		
		end if
		
	end function psim
	      
end module pbl_simil
