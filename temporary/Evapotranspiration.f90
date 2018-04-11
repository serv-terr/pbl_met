module Evapotranspiration

	use nan_support
	
	implicit none
	
	private
	
	! Public interface
	! -1- Parameters
	public	:: LAI_GRASS
	public	:: LAI_ALFALFA
	public	:: ASCE_STANDARDATMOSPHERE
	public	:: ASCE_STANDARDEQ
	public	:: ASCE_MEANTEMPERATURE
	public	:: ASCE_GRASS
	public	:: ASCE_ALFALFA
	! -1- Constants
	public	:: SetApparentLAI
	public	:: ASCE_ColtureLAI
	public	:: ASCE_LatentVaporizationHeat
	public	:: ASCE_AirPressure
	public	:: ASCE_VirtualTemperature
	public	:: ASCE_MJm2h
	public	:: ASCE_AerodynamicResistance
	public	:: ASCE_Evapotranspiration_Ref
	
	! Constants
	integer, parameter	:: LAI_GRASS               = 0
	integer, parameter	:: LAI_ALFALFA             = 1
	integer, parameter	:: ASCE_STANDARDATMOSPHERE = 0
	integer, parameter	:: ASCE_STANDARDEQ         = 1
	integer, parameter	:: ASCE_MEANTEMPERATURE    = 2
	integer, parameter	:: ASCE_GRASS              = 1
	integer, parameter	:: ASCE_ALFALFA            = 2

contains

	! Based on Choudhury et al. (1987) and Choudhury (1989), and reported
	! in ASCE Standard Reference Evapotranspiration Equation, formula B.13;
	! starting from original formula I've rearranged terms and obtained a
	! rough estimate of LAI, assuming net radiation and ground heat flux are
	! correct. (Mauri Favaron, 2017).
	! See also Santanello, Friedl, 2003 in references.
	!
	! Note: This estimate yields values which increasingly change over year
	!       as latitude's absolute value grows. Also, variability in Rn measurements
	!       affect results changes importantly. So, uding an Rn *estimate*
	!       close to the Equator is likely to yield a value constant over year.
	!
	function SetApparentLAI(ivTimeStamp, iDeltaTime, rvRn, rvG0) result(rvLAI)

		implicit none

		! Routine arguments
		integer, dimension(:), intent(in)	:: ivTimeStamp
		integer, intent(in)					:: iDeltaTime
		real, dimension(:), intent(in)		:: rvRn
		real, dimension(:), intent(in)		:: rvG0
		real, dimension(SIZE(rvRn))			:: rvLAI

		! Locals
		integer	:: iDataPerDay
		real	:: KG
		integer	:: i, j
		integer	:: iNumValid
		real	:: ratio
		real	:: rLAI

		! Compute a rough estimate of LAI by applying Choudhury relationship (rearranged)
		do i = 1, SIZE(ivTimeStamp)
			if(rvRn(i) > 0.0) then
				KG = 0.4
			else
				KG = 2.0
			end if
			if(ABS(rvRn(i)) > 0.001) then
				ratio = rvG0(i)/(KG*rvRn(i))
				if(ratio > 0.001) then
					rvLAI(i) = -2.0*LOG(ratio)
				else
					rvLAI(i) = NaN
				end if
			else
				rvLAI(i) = NaN
			end if
		end do

		! Smooth initial LAI estimation by applying daily averages
		iDataPerDay = (24*3600) / iDeltaTime
		do i = 1, SIZE(ivTimeStamp), iDataPerDay

			! Accumulate LAI over current day, considering only valid data
			iNumValid = 0
			rLAI      = 0.
			do j = i, i+iDataPerDay-1
				if(.not.ISNAN(rvLAI(j))) then
					iNumValid = iNumValid + 1
					rLAI      = rLAI + rvLAI(j)
				end if
			end do
			if(iNumValid > 0) then
				rLAI = rLAI / iNumValid
			else
				rLAI = NaN
			end if

			! Distribute result over current day
			do j = i, i+iDataPerDay-1
				rvLAI(j) = rLAI
			end do

		end do

	end function SetApparentLAI


	! Estimation of leaf area index (LAI) based on vegetation height and type,
	! for coltures, as from the ASCE Evapotranspiration Standard Equation.
	function ASCE_ColtureLAI(rVegetationHeight, iColtureType) result(rLAI)

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

	end function ASCE_ColtureLAI


	function ASCE_LatentVaporizationHeat(rTemp, iCalculationType) result(rLambda)

		implicit none

		! Routine arguments
		real, intent(in)	:: rTemp			! (°C)
		integer, intent(in)	:: iCalculationType	! ASCE_STANDARDEQ, ASCE_MEANTEMPERATURE
		real				:: rLambda			! (MJ/kg)

		! Locals
		! -none-

		! Compute the information desired
		select case(iCalculationType)
		case(ASCE_STANDARDEQ)
			rLambda = 2.45
		case(ASCE_MEANTEMPERATURE)
			rLambda = 2.501 - 2.361e-3 * rTemp
		case default
			rLambda = NaN
		end select


	end function ASCE_LatentVaporizationHeat


	function ASCE_AirPressure(rZ, rvTemp, rZr, iCalculationType) result(rPk)

		implicit none

		! Routine arguments
		real, intent(in)				:: rZ		! Reference height at which pressure is desired (m)
		real, dimension(:), intent(in)	:: rvTemp	! Air temperature (°C)
		real, intent(in)				:: rZr		! Height at which temperature measurements are taken (m)
		integer, intent(in)				:: iCalculationType	! ASCE_STANDARDATMOSPHERE, ASCE_STANDARDEQ, ASCE_MEANTEMPERATURE
		real							:: rPk		! Estimated pressure (kPa, as used in ASCE equations; multiply by 10 for normal use and reporting)

		! Locals
		real	:: rTK0	! Reference temperature (K)
		integer	:: i
		integer	:: iNumValid

		! Constants
		real, parameter	:: P0 = 101.3		! Pressure at reference height (kPa)
		real, parameter	:: g  = 9.807		! Gravitation acceleration (m/s2)
		real, parameter	:: z0 = 0.			! Reference altitude for expressing pressure (m above msl)
		real, parameter	:: R  = 287.0		! Specific gas constant (J/kg/K)
		real, parameter	:: Alpha1 = 0.0065	! Constant lapse rate of moist air (K/m)

		! Reference temperature
		select case(iCalculationType)
		case(ASCE_STANDARDATMOSPHERE)
			rTK0 = 288.0
		case(ASCE_STANDARDEQ)
			rTK0 = 293.0
		case(ASCE_MEANTEMPERATURE)
			rTK0 = 0.
			iNumValid = 0
			do i = 1, SIZE(rvTemp)
				if(.not.ISNAN(rvTemp(i))) then
					iNumValid = iNumValid + 1
					rTK0      = rTK0 + rvTemp(i)
				end if
			end do
			if(iNumValid > 0) then
				rTK0 = rTK0 / iNumValid + 273.15
			else
				rPk = NaN
				return
			end if
		case default
			rPk = NaN
			return
		end select

		! Compute pressure
		rPk = P0*((rTK0 - Alpha1*(rZ - z0))/rTK0)**(g/(Alpha1*R))

	end function ASCE_AirPressure


	function ASCE_VirtualTemperature(Temp, ea, P) result(Tv)

		implicit none

		! Routine arguments
		real, intent(in)	:: Temp		! (°C)
		real, intent(in)	:: ea		! (kPa)
		real, intent(in)	:: P		! (kPa)
		real				:: Tv		! (K)

		! Locals
		! -none-

		! Compute the information desired
		Tv = (Temp + 273.16)/(1.0 - 0.378*ea/P)

	end function ASCE_VirtualTemperature


	function ASCE_MJm2h(Wm2) result(MJm2h)

		implicit none

		! Routine arguments
		real, intent(in)	:: Wm2			! Radiation (W/m2)
		real				:: MJm2h		! Radiation (MJ/m2 h)

		! Locals
		! -none-

		! Convert unit
		MJm2h = 3600.0 * Wm2 / 1.e6

	end function ASCE_MJm2h


	function ASCE_AerodynamicResistance(zw, zh, u, h) result(ra)

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

	end function ASCE_AerodynamicResistance


	function ASCE_Evapotranspiration_Ref(Pres, Temp, Vel, Rn, G, es, ea, Zr, vegType) result(ET)

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
		real	:: gamma	! Psychrometric constant (kPa / °C)
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
		gamma = 0.0000665*Pres
		d     = 0.67 * h
		z0    = 0.123 * h
		Vel2  = Vel * LOG((2.0 - d)/z0) / LOG((Zr - d) / z0)
		ET = (&
			(0.408*Delta*ASCE_MJm2h(Rn-G) + gamma*cn/(Temp + 273.0)*Vel2*0.1*(es-ea)) / &
			(Delta + gamma*(1.0 - cd*Vel2)) &
		)

	end function ASCE_Evapotranspiration_Ref

end module Evapotranspiration

