! pbl_evtrn - Fortran module, containing computations pertaining to
!             Planetary Boundary Layer (PBL) quantities, encompassing
! evapotranspiration and related quantities.
!
! This module is part of the pbl_met library.
!
! This is open-source code, covered by the lGPL 3.0 license.
!
module pbl_evtrn

	use pbl_base
	use pbl_time
	use pbl_thermo

	implicit none
	
	private
	
	! Public interface
	! 1. Evapotranspiration and related quantities
	public	:: ColtureLAI
	public	:: AerodynamicResistance
	public	:: Evapotranspiration
	
contains
	
	! Estimation of leaf area index (LAI) based on vegetation height and type,
	! for coltures, as from the ASCE Evapotranspiration Standard Equation.
	function ColtureLAI(rVegetationHeight, iColtureType) result(rLAI)

		implicit none

		! Routine arguments
		real, intent(in)	:: rVegetationHeight	! (m)
		integer, intent(in)	:: iColtureType			! LAI_GRASS, LAI_ALFALFA
		real				:: rLAI

		! Locals
		! -none-

		! Compute the information desired
		select case(iColtureType)
		case(LAI_GRASS)
			rLAI = 24.0 * rVegetationHeight
		case(LAI_ALFALFA)
			rLAI = 5.5 + 1.5 * LOG(rVegetationHeight)
		case default
			rLAI = NaN
		end select

	end function ColtureLAI


	function AerodynamicResistance(zw, zh, u, h) result(ra)

		implicit none

		! Routine arguments
		real, intent(in)	:: zw	! Height above ground at which wind is measured (m)
		real, intent(in)	:: zh	! Height above ground at which temperature/humidity are measured (m)
		real, intent(in)	:: u	! Wind speed (m/s)
		real, intent(in)	:: h	! Vegetation height (h)
		real				:: ra	! Aerodynamic resistance

		! Locals
		real	:: d	! Displacement height
		real	:: z0m	! Roughness length governing momentum transfer (m)
		real	:: z0h	! Roughness length governing heat transfer (m)

		! Constant
		real, parameter	:: k = 0.41	! von Karman constant

		! Compute the information desired
		if(u > 0.) then
			d = 0.67 * h
			z0m = 0.123 * h
			z0h = 0.0123 * h
			ra = LOG((zw-d)/z0m) * LOG((zh-d)/z0h) / (k**2 * u)
		else
			ra = NaN
		end if

	end function AerodynamicResistance


	function Evapotranspiration(Pres, Temp, Vel, Rn, G, es, ea, Zr, vegType) result(ET)

		implicit none

		! Routine arguments
		real, intent(in)	:: Pres		! Air pressure (hPa)
		real, intent(in)	:: Temp		! Air temperature (°C)
		real, intent(in)	:: Vel		! Wind speed (m / s)
		real, intent(in)	:: Rn		! Net radiation (W / m2)
		real, intent(in)	:: G		! Ground heat flux (W / m2)
		real, intent(in)	:: es		! Saturation vapor pressure (hPa)
		real, intent(in)	:: ea		! Water vapor pressure (hPa)
		real, intent(in)	:: Zr		! Anemometer measurement height (m above ground)
		integer, intent(in)	:: vegType	! Vegetation type (ASCE_GRASS, ASCE_ALFALFA)
		real				:: ET		! Evapotranspiration (mm/h)

		! Locals
		real	:: Delta	! Slope (first derivative) of saturation vapor pressure relation
		real	:: gam		! Psychrometric constant (kPa / °C)
		real	:: Vel2		! Wind speed at 2 m above ground
		real	:: h		! Vegetation height (m)
		real	:: d		! Displacement height (m)
		real	:: z0		! Aerodynamic roughness length (m)
		real	:: cd
		real	:: cn

		! Estimate coefficients based on reference vegetation type
		select case(vegType)
		case(ASCE_GRASS)
			h  =  0.12
			cn = 37.0
			if(Rn > 0.) then
				cd = 0.24
			else
				cd = 0.96
			end if
		case(ASCE_ALFALFA)
			h  =  0.50
			cn = 66.0
			if(Rn > 0.) then
				cd = 0.25
			else
				cd = 1.70
			end if
		case default
			ET = NaN
			return
		end select

		! Compute evapotranspiration
		Delta = 2503.0 * EXP(17.27*Temp/(Temp + 237.3)) / (Temp + 237.3)**2
		gam   = 0.0000665*Pres
		d     = 0.67 * h
		z0    = 0.123 * h
		Vel2  = Vel * LOG((2.0 - d)/z0) / LOG((Zr - d) / z0)
		ET = (&
			(0.408*Delta*(Rn-G)*3600.0/1.e6 + gam*cn/(Temp + 273.0)*Vel2*0.1*(es-ea)) / &
			(Delta + gam*(1.0 - cd*Vel2)) &
		)

	end function Evapotranspiration
	
end module pbl_evtrn
