! pbl_thermo - Fortran module, containing computations related to
!              Planetary Boundary Layer (PBL) quantities, encompassing
! the lower atmosphere thermodynamics, energy balance, psychrometry,
! and some astronomical formulae dealing with sunset/sunrise and apparent
! solar position.
!
! This module is part of the pbl_met library.
!
! This is open-source code, covered by the lGPL 3.0 license.
!
! Author(s): Patrizia Favaron
!
module pbl_thermo

	use ieee_arithmetic
	use pbl_base
	use pbl_time

	implicit none

	private

	! Public interface
	! 1. Thermodynamics and psychrometry
	PUBLIC	:: WaterSaturationPressure		! Saturation vapor pressure at a given temperature
	PUBLIC	:: E_SAT_1						! Saturation water vapor pressure, from old PBL_MET
	PUBLIC	:: D_E_SAT						! Derivative of saturation water vapor pressure, from old PBL_MET
	PUBLIC	:: PrecipitableWater			! Estimate the amount of precipitable water
	PUBLIC	:: WaterVaporPressure			! Water vapor partial pressure
	PUBLIC	:: RelativeHumidity				! Relative humidity
	PUBLIC	:: AbsoluteHumidity				! Absolute humidity (i.e. density of water vapor in air)
	PUBLIC	:: AirDensity					! Density of air, given temperature and pressure
	PUBLIC	:: RhoCp						! Product of air density and constant pressure thermal capacity of air
	PUBLIC	:: LatentVaporizationHeat		! Latent vaporization heat at given temperature
	PUBLIC	:: DewPointTemperature			! Approximate dew point temperature
	PUBLIC	:: WetBulbTemperature			! Wet bulb temperature estimate, given dry bulb temperature, relative humidity and pressure
	PUBLIC	:: AirPressure					! Estimate atmospheric pressure from height and temperature
	PUBLIC	:: VirtualTemperature			! Virtual temperature given water vapor pressure and air pressure
	PUBLIC	:: SonicTemperature				! Estimate ultrasonic temperature given dry bulb temperature, relative humidity and pressure
	! 2. Energy balance at ground-atmosphere contact (new method, as from ASCE Evapotranspiration Equation
 	public	:: ClearSkyRg_Simple			! Simple estimate of global solar radiation under clear sky conditions
	public	:: ClearSkyRg_Accurate			! More accurate estimate of global solar radiation under clear sky conditions
	public	:: GlobalRadiation				! Global radiation estimate, obtained by correcting the clear sky estimate by cloud cover
	public	:: ExtraterrestrialRadiation	! Estimate of extraterrestrial radiation (i.e., global radiation above the Earth atmosphere)
	public	:: NetRadiation					! Estimate of solar net radiation
	public	:: CloudCover					! Estimate cloud cover using estimated and measured global radiatiobs
	public	:: Cloudiness					! Estimate cloudiness factor (see ASCE report for definitions)
	public	:: GroundHeatFlux				! Estimates surface heat flux (W/m2)
	! 3. Energy balance at ground atmosphere contact (old PBL_MET method)
	public	:: GlobalRadiation_MPDA			! Supersedes SUN_RAD2 in old PBL_MET
	public	:: CloudCover_MPDA				! Supersedes CLOUD_RG in old PBL_MET
	public	:: NetRadiation_MPDA			! Supersedes R_NET_D and R_NET_N in ECOMET legacy code
	! 4. Atmospheric scaling quantities
	public	:: BruntVaisala					! Estimate of Brunt-Vaisala frequency, given temperature and height
	! 5. PBL parameters
	public	:: PBL_Parameters				! Estimate PBL parameters, given surface data

	! Polymorphic (Fortran-90-art) routines

	interface AirPressure
		module procedure AirPressure1
		module procedure AirPressure2
	end interface AirPressure

	interface AirDensity
	    module procedure AirDensity_4
	    module procedure AirDensity_8
	end interface AirDensity

	interface RhoCp
	    module procedure RhoCp_4
	    module procedure RhoCp_8
	end interface RhoCp

contains

	! Estimation of clear sky radiation by the simplified method
	!
	! Input:
	!
	!	Ra		Extraterrestrial radiation (W/m2)
	!
	!	z		Site elevation above mean sea level (m)
	!
	! Output:
	!
	!	Rso		Clear sky radiation (W/m2)
	!
	function ClearSkyRg_Simple(Ra, z) result(Rso)

		implicit none

		! Routine arguments
		real, intent(in)	:: Ra
		real, intent(in)	:: z
		real				:: Rso

		! Locals
		! -none-

		! Compute the information item desired
		Rso = Ra * (0.75 + 2.0e-5*z)

	end function ClearSkyRg_Simple


	! Estimation of clear sky radiation by the extended, more accurate method
	!
	! Input:
	!
	!	timeStamp			String, in form "YYYY-MM-DD HH:MM:SS" indicating time on *beginning* of averaging period
	!						(beware: many Italian weather station use a time stamp on *end* of averaging period:
	!						if so, subtract one hour)
	!
	!	averagingPeriod		Length of averaging period (s)
	!
	!	lat					Local latitude (degrees, positive northwards)
	!
	!	lon					Local longitude (degrees, positive eastwards)
	!
	!	zone				Time zone number (hours, positive Eastwards, in range -12 to 12)
	!
	!	Pa					Local pressure, that is, pressure not reduced to mean sea level (hPa)
	!
	!	Temp				Local temperature (Celsius degrees)
	!
	!	Hrel				Relative humidity (%)
	!
	!	Kt					Turbidity coefficient (dimensionless, 0 excluded to 1 included;
	!						value 1 corresponds to perfectly clean air; for extremelyturbid,
	!						dusty or polluted air 0.5 may be assumed; recommended value lacking
	!						better data: 1, the default)
	!
	! Output:
	!
	!	Rso					Clear sky radiation (W/m2)
	!
	function ClearSkyRg_Accurate(timeStamp, averagingPeriod, lat, lon, zone, Pa, Temp, Hrel, Kt_In) result(Rso)

		implicit none

		! Routine arguments
		character(len=*), intent(in)	:: timeStamp
		real, intent(in)				:: averagingPeriod, lat, lon, zone, Pa, Temp, Hrel
		real, intent(in), optional		:: Kt_In
		real							:: Rso

		! Locals
		real	:: Kt

		real	:: Kb, Kd, Ra
		real	:: beta, sinBeta, W
		real	:: e, es, Ta
		integer	:: ss, mm, hh, yy, mo, dy, iDayOfYear
		real	:: dr
		real	:: omega, omega1, omega2, omegaS
		real	:: timenow, JD, t, Sc, b, t1
		real	:: solarDeclination, centralMeridianLongitude, localLongitude
		real	:: delta_lon, intermediate, sign
		integer	:: iErrCode

		! Constants
		real, parameter	:: SOLAR_CONSTANT = 1.e6*4.92/3600.0		! W/m2
		real, parameter	:: PI             = 3.1415927

		! Get optional parameter (assign default if missing)
		if(present(Kt_In)) then
			Kt = Kt_In
		else
			Kt = 1.0
		end if

		! get date and time
		read(timeStamp, "(i4,5(1x,i2))", iostat=iErrCode) yy, mo, dy, hh, mm, ss
		if(iErrCode /= 0) then
			Rso = NaN
			return
		end if
		iDayOfYear = DoY(yy,mo,dy)

		! Compute solar declination
		solarDeclination = 0.409*SIN(2*PI/365*iDayOfYear - 1.39)

		! Compute Julian day
		timenow = hh + mm/60.0 + ss/3600.0 - zone
		JD = calcJD(yy, mo, dy)

		! Inverse squared relative distance factor for Sun-Earth
		dr = 1.0 + 0.033*COS(2*PI*iDayOfYear/365.0)

		! Calculate geographical positioning parameters (with a "-" sign for longitudes, according to ASCE conventions)
		centralMeridianLongitude = -zone*15.0
		if(centralMeridianLongitude < 0.0) then
			centralMeridianLongitude = centralMeridianLongitude + 360.0
		end if
		localLongitude = -lon
		if(localLongitude < 0.0) then
			localLongitude = localLongitude + 360.0
		end if

		! Compute hour at mid of averaging time
		t1 = averagingPeriod / 3600.0
		t = timenow + zone + 0.5*t1

		! Calculate seasonal correction for solar time
		b  = 2.*PI*(iDayOfYear-81)/364.0
		Sc = 0.1645*SIN(2.0*b) - 0.1255*COS(b) - 0.025*SIN(b)

		! Solar time angle at midpoint of averaging time
		delta_lon = MOD(ABS(centralMeridianLongitude - localLongitude), 360.0)
		if(delta_lon > 180.0) then
			intermediate = 360.0 - delta_lon
		else
			intermediate = delta_lon
		end if
		if(((delta_lon > 0.0) .and. (delta_lon <= 180.0)) .or. ((delta_lon <= -180.0) .and. (delta_lon >= -360.0))) then
			sign =  1.0
		else
			sign = -1.0
		end if
		delta_lon = sign * intermediate
		omega = (PI/12.0) * ((t + 0.06667*(delta_lon) + Sc) - 12.0)

		! Solar time angle at beginning and end of averaging period
		omega1 = omega - PI*t1/24.0
		omega2 = omega + PI*t1/24.0

		! Adjust angular end points to exclude nighttime hours
		omegaS = ACOS(-TAN(lat*PI/180.0)*TAN(solarDeclination))	! Sunset angle
		if(omega1 < -omegaS) then
			omega1 = -omegaS
		end if
		if(omega2 < -omegaS) then
			omega2 = -omegaS
		end if
		if(omega1 > omegaS) then
			omega1 = omegaS
		end if
		if(omega2 > omegaS) then
			omega2 = omegaS
		end if
		if(omega1 > omega2) then
			omega1 = omega2
		end if

		! Compute extraterrestrial radiation
		Ra = 12/PI * SOLAR_CONSTANT * dr * ( &
				(omega2-omega1)*SIN(lat*PI/180.0)*SIN(solarDeclination) + &
				COS(lat*PI/180.0)*COS(solarDeclination)*(SIN(omega2) - SIN(omega1)) &
		)

		! Estimate the amount of precipitable water
		Ta = Temp + 273.15
		es = E_SAT_1(Temp)
		e  = Hrel*es/100.0
		W  = PrecipitableWater(e, Pa)

		! Compute solar elevation (refractive correction is not applied, in compliance with ASCE standard evapotranspiration equation)
		sinBeta = SIN(lat*PI/180.0)*SIN(solarDeclination) + COS(lat*PI/180.0)*COS(solarDeclination)*COS(omega)
		if(sinBeta > 0.0) then

			! Estimate the clearness index for direct beam radiation
			Kb = 0.98*EXP(-0.000149*Pa/(Kt*sinBeta) - 0.075*(W/sinBeta)**0.4)

			! Estimate the transmissivity index for diffuse radiation
			if(Kb >= 0.15) then
				Kd = 0.35 - 0.36*Kb
			else
				Kd = 0.18 + 0.82*Kb
			end if

		else

			! Assume null clearness and transmissivity on night-time
			Kb = 0.0
			Kd = 0.18

		end if

		! Last, estimate clear-sky radiation
		Rso = Ra * (Kb + Kd)

	end function ClearSkyRg_Accurate


	! Estimation of global radiation given clear sky radiation and cloud cover
	!
	! Input:
	!
	!	Rcs					Clear sky radiation (W/m2)
	!
	!	N					Cloud cover (real, 0.0 to 1.0; values outside this interval are forced within of it)
	!
	! Output:
	!
	!	rg					Global radiation (W/m2)
	!
	function GlobalRadiation(Rcs, N) result(Rg)

		implicit none

		! Routine arguments
		real, intent(in)	:: Rcs
		real, intent(in)	:: N
		real				:: Rg

		! Locals
		real	:: Cloud

		! Force N within limits
		Cloud = min(max(0.0, N), 1.0)

		! Compute global radiation
		Rg = Rcs * (1. - 0.75*Cloud**3.4)

	end function GlobalRadiation


	! Estimation of cloud cover given clear sky low turbidity radiation and global radiation
	!
	! Input:
	!
	!	rvTimeStamp				Time stamp, in epoch form
	!
	!	rvRcs					Clear sky radiation (W/m2)
	!
	!	rvRg					Global radiation (W/m2)
	!
	! Output:
	!
	!	rvN					Cloud cover (real, 0.0 to 1.0)
	!
	! A dutiful warning:
	!
	!	I'm not really sure this routine "works". It may provide sensible
	!	enough answers on daytime when global radiation turned "good", but,
	!	on night-time, it is completely blind to reality. I've tried to
	!	overcome this by a simple trick, that is, assigning the cloud cover
	!	daily mean to all nocturnal and valid values, and NaN elsewhere.
	!	I'm quite sure it does not work, but if you just have a global
	!	radiation sensor, I don't in the moment know what else could we do.
	!
	function CloudCover(rvTimeStamp, rvRcs, rvRg, rvN) result(iRetCode)

		implicit none

		! Routine arguments
		real(8), dimension(:), intent(in)				:: rvTimeStamp
		real, dimension(:), intent(in)					:: rvRcs
		real, dimension(:), intent(in)					:: rvRg
		real, dimension(:), allocatable, intent(out)	:: rvN
		integer											:: iRetCode

		! Locals
		integer	:: iNumData
		integer	:: i
		integer	:: iDay
		integer	:: iHour
		real	:: rAvgN
		integer	:: iNumN
		integer	:: iNumDays
		integer(8), dimension(:), allocatable	:: ivDay
		logical, dimension(:), allocatable		:: lvValid
		integer, dimension(:), allocatable		:: ivDayBegin
		integer, dimension(:), allocatable		:: ivDayEnd

		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check parameters
		iNumData = size(rvTimeStamp)
		if(size(rvRcs) /= iNumData .or. size(rvRg) /= iNumData) then
			iRetCode = 1
			return
		end if

		! Check time stamp increase monotonically
		do i = 1, iNumData-1
			if(rvTimeStamp(i) >= rvTimeStamp(i+1)) then
				iRetCode = 2
				return
			end if
		end do

		! *** Anything potentially wrong excluded: we have a "go" for calculations

		! Reserve workspace
		if(allocated(rvN)) deallocate(rvN)
		allocate(rvN(iNumData))

		! Get day index
		allocate(ivDay(iNumData))
		ivDay = floor(rvTimeStamp / 86400.)
		ivDay = ivDay - minval(ivDay) + 1

		! Compute begin and end of each day
		iNumDays = maxval(ivDay)
		allocate(ivDayBegin(iNumDays), ivDayEnd(iNumDays))
		ivDayBegin(1)      = 1
		ivDayEnd(iNumDays) = iNumData
		iDay = 1
		do i = 1, iNumData - 1
			if(ivDay(i) /= ivDay(i+1)) then
				ivDayEnd(iDay)   = i
				iDay             = iDay + 1
				ivDayBegin(iDay) = i + 1
			end if
		end do

		! Main loop: iterate over days
		do iDay = 1, iNumDays

			! Perform estimation of N over "valid" hours
			rAvgN = 0.
			iNumN = 0
			do i = ivDayBegin(iDay), ivDayEnd(iDay)
				if(rvRcs(i) == rvRcs(i) .and. rvRg(i) == rvRg(i) .and. rvRcs(i) > 0.) then
					if(rvRg(i) >= rvRcs(i)) then
						rvN(i) = 0.0
					elseif(rvRg(i) <= 0.) then
						rvN(i) = 1.0
					else
						rvN(i) = 1.0882951337107805*((rvRcs(i)-rvRg(i)) / rvRcs(i)) ** (1./3.4)
					end if
					rvN(i) = min(max(0.0, rvN(i)), 1.0)
					rAvgN = rAvgN + rvN(i)
					iNumN = iNumN + 1
				end if
			end do
			if(iNumN > 0) then
				rAvgN = rAvgN / iNumN
			else
				rAvgN = NaN
			end if

			! Propagate mean N over "invalid" hours
			do i = ivDayBegin(iDay), ivDayEnd(iDay)
				if(rvRcs(i) /= rvRcs(i) .or. rvRg(i) /= rvRg(i)) then
					rvN(i) = NaN
				elseif(rvRcs(i) <= 0.) then
					rvN(i) = rAvgN
				end if
			end do

		end do

		! Leave
		deallocate(ivDayBegin, ivDayEnd)
		deallocate(ivDay)

	end function CloudCover


	! Accurate estimate of extraterrestrial solar radiation
	!
	! Input:
	!
	!	timeStamp			String, in form "YYYY-MM-DD HH:MM:SS" indicating time on *beginning* of averaging period
	!						(beware: many Italian weather station use a time stamp on *end* of averaging period:
	!						if so, subtract one hour)
	!
	!	averagingPeriod		Length of averaging period (s)
	!
	!	lat					Local latitude (degrees, positive northwards)
	!
	!	lon					Local longitude (degrees, positive eastwards)
	!
	!	zone				Time zone number (hours, positive Eastwards, in range -12 to 12)
	!
	! Output:
	!
	!	ra					Extraterrestrial radiation (W/m2)
	!
	function ExtraterrestrialRadiation(timeStamp, averagingPeriod, lat, lon, zone) result(ra)

		implicit none

		! Routine arguments
		character(len=*), intent(in)	:: timeStamp
		real, intent(in)				:: averagingPeriod, lat, lon, zone
		real							:: ra

		! Locals
		integer	:: iErrCode
		integer	:: ss, mm, hh, yy, mo, dy, iDayOfYear
		real	:: dr
		real	:: omega, omega1, omega2, omegaS
		real	:: timenow, JD, t, Sc, b, t1
		real	:: solarDeclination, centralMeridianLongitude, localLongitude
		real	:: delta_lon, intermediate, sign

		! Constants
		real, parameter	:: SOLAR_CONSTANT = 1.e6*4.92/3600.0		! W/m2
		real, parameter	:: PI             = 3.1415927

		! Get date and time
		read(timeStamp, "(i4,5(1x,i2))", iostat=iErrCode) yy, mo, dy, hh, mm, ss
		if(iErrCode /= 0) then
			Ra = NaN
			return
		end if
		iDayOfYear = DoY(yy,mo,dy)

		! Compute solar declination
		solarDeclination = 0.409*SIN(2*PI/365*iDayOfYear - 1.39)

		! Compute Julian day
		timenow = hh + mm/60.0 + ss/3600.0 - zone
		JD = calcJD(yy, mo, dy)

		! Inverse squared relative distance factor for Sun-Earth
		dr = 1.0 + 0.033*COS(2*PI*iDayOfYear/365.0)

		! Calculate geographical positioning parameters (with a "-" sign for longitudes, according to ASCE conventions)
		centralMeridianLongitude = -zone*15.0
		if(centralMeridianLongitude < 0.0) then
			centralMeridianLongitude = centralMeridianLongitude + 360.0
		end if
		localLongitude = -lon
		if(localLongitude < 0.0) then
			localLongitude = localLongitude + 360.0
		end if

		! Compute hour at mid of averaging time
		t1 = averagingPeriod / 3600.0
		t = timenow + zone + 0.5*t1

		! Calculate seasonal correction for solar time
		b  = 2.*PI*(iDayOfYear-81)/364.0
		Sc = 0.1645*SIN(2.0*b) - 0.1255*COS(b) - 0.025*SIN(b)

		! Solar time angle at midpoint of averaging time
		delta_lon = MOD(ABS(centralMeridianLongitude - localLongitude), 360.0)
		if(delta_lon > 180.0) then
			intermediate = 360.0 - delta_lon
		else
			intermediate = delta_lon
		end if
		if(((delta_lon > 0.0) .and. (delta_lon <= 180.0)) .or. ((delta_lon <= -180.0) .and. (delta_lon >= -360.0))) then
			sign =  1.0
		else
			sign = -1.0
		end if
		delta_lon = sign * intermediate
		omega = (PI/12.0) * ((t + 0.06667*(delta_lon) + Sc) - 12.0)

		! Solar time angle at beginning and end of averaging period
		omega1 = omega - PI*t1/24.0
		omega2 = omega + PI*t1/24.0

		! Adjust angular end points to exclude nighttime hours
		omegaS = ACOS(-TAN(lat*PI/180.0)*TAN(solarDeclination))	! Sunset angle
		if(omega1 < -omegaS) then
			omega1 = -omegaS
		end if
		if(omega2 < -omegaS) then
			omega2 = -omegaS
		end if
		if(omega1 > omegaS) then
			omega1 = omegaS
		end if
		if(omega2 > omegaS) then
			omega2 = omegaS
		end if
		if(omega1 > omega2) then
			omega1 = omega2
		end if

		! Compute extraterrestrial radiation
		ra = 12/PI * SOLAR_CONSTANT * dr * ( &
				(omega2-omega1)*SIN(lat*PI/180.0)*SIN(solarDeclination) + &
				COS(lat*PI/180.0)*COS(solarDeclination)*(SIN(omega2) - SIN(omega1)) &
			)

		! Clip to interval [0,+infinity), as radiation cannot be negative
		ra = max(ra, 0.)

	end function ExtraterrestrialRadiation


	! Estimation of net radiation not using cloud cover, as from ASCE standardized reference evapotranspiration equation.
	!
	! Input:
	!
	!	Rg		Measured or estimated global radiation (W/m2)
	!
	!	albedo	Albedo at site (dimensionless)
	!
	!	fcd		Cloudiness function (dimensionless, 0 to 1)
	!
	!	Ea		Water vapor pressure (hPa)
	!
	!	Ta		Air temperature (K)
	!
	! Output:
	!
	!	Rn		Net radiation (W/m2)
	!
	! Note 1 (fcd):
	!
	! An accurate evaluation of the cloudiness function is critical for Rn estimate to yield
	! sensible results. fcd is defined as
	!
	!	fcd = 1.35*(Rg/Rgc) - 0.35
	!
	! where Rg is global radiation, and Rgc the clear-sky radiation computed when solar elevation
	! exceeds a given safety threshold (typically assumed to 0.3 radians computed on mid-averaging
	! period). Defined this way, fcd value is valid only on center-daytime, and undefined elsewhere.
	! But, it may be prolonged by computing an appropriate value on the preceding day's.
	!
	! Alternatively, fcd may be assumed to be fixed to some reference value, derived e.g. by the statistical
	! study of data from a nearby met station equipped with a reliable Rg measurement, and then used to
	! estimate Rg from Rgc:
	!
	!	Rg = Rgc * (fcd + 0.35) / 1.35
	!
	! Although dangerous, the last way may be the only resort when no global radiation measurement
	! is available at met station site.
	!
	! Note 2 (why not cloud cover?):
	!
	! Old PBL_MET estimates made extensive use of cloud cover, a notoriously difficult quantity to get.
	! In this formulation, the information coming from the cloud cover is jointly proxied by fcd, the
	! relatively slowly changing cloudiness function, and Ea, the water vapor pressure (which in case of
	! strong cloud cover will tend to approach saturation pressure, and whose value is intuitively
	! related to cloud cover to some extent).
	!
	function NetRadiation(Rg, albedo, fcd, Ea, Ta) result(Rn)

		implicit none

		! Routine arguments
		real, intent(in)	:: Rg
		real, intent(in)	:: albedo
		real, intent(in)	:: fcd
		real, intent(in)	:: Ea
		real, intent(in)	:: Ta
		real				:: Rn

		! Locals
		real	:: Rns, Rnl		! Short- and long-wave components of net radiation

		! Short-wave component of net radiation is the part which is not reflected
		Rns = Rg*(1.0 - albedo)

		! Long-wave component depends on various things
		Rnl = 5.6722e-8 * fcd * (0.34 - 0.14*SQRT(Ea/10.0)) * Ta**4		! 5.6722e-8 = sigma[MJ / m2 h] * = 2.042e-10 * 1000000 / 3600

		! Finally, the Net Radiation:
		Rn = Rns - Rnl

	end function NetRadiation


	function Cloudiness(rvTimeStamp, rvRcs, rvRg, rvFcd) result(iRetCode)

		implicit none

		! Routine arguments
		real(8), dimension(:), intent(in)				:: rvTimeStamp
		real, dimension(:), intent(in)					:: rvRcs
		real, dimension(:), intent(in)					:: rvRg
		real, dimension(:), allocatable, intent(out)	:: rvFcd
		integer											:: iRetCode

		! Locals
		integer	:: iNumData
		integer	:: i
		integer	:: iDay
		integer	:: iHour
		real	:: rAvgFcd
		integer	:: iNumFcd
		integer	:: iNumDays
		integer(8), dimension(:), allocatable	:: ivDay
		logical, dimension(:), allocatable		:: lvValid
		integer, dimension(:), allocatable		:: ivDayBegin
		integer, dimension(:), allocatable		:: ivDayEnd
	
		! Assume success (will falsify on failure)
		iRetCode = 0
	
		! Check parameters
		iNumData = size(rvTimeStamp)
		if(size(rvRcs) /= iNumData .or. size(rvRg) /= iNumData) then
			iRetCode = 1
			return
		end if

		! Check time stamp increase monotonically
		do i = 1, iNumData-1
			if(rvTimeStamp(i) >= rvTimeStamp(i+1)) then
				iRetCode = 2
				return
			end if
		end do

		! *** Anything potentially wrong excluded: we have a "go" for calculations

		! Reserve workspace
		if(allocated(rvFcd)) deallocate(rvFcd)
		allocate(rvFcd(iNumData))

		! Get day index
		allocate(ivDay(iNumData))
		ivDay = floor(rvTimeStamp / 86400.)
		ivDay = ivDay - minval(ivDay) + 1

		! Compute begin and end of each day
		iNumDays = maxval(ivDay)
		allocate(ivDayBegin(iNumDays), ivDayEnd(iNumDays))
		ivDayBegin(1)      = 1
		ivDayEnd(iNumDays) = iNumData
		iDay = 1
		do i = 1, iNumData - 1
			if(ivDay(i) /= ivDay(i+1)) then
				ivDayEnd(iDay)   = i
				iDay             = iDay + 1
				ivDayBegin(iDay) = i + 1
			end if
		end do
	
		! Main loop: iterate over days
		do iDay = 1, iNumDays

			! Perform estimation of cloudiness factor over "valid" hours
			rAvgFcd = 0.
			iNumFcd = 0
			do i = ivDayBegin(iDay), ivDayEnd(iDay)
				if(rvRcs(i) == rvRcs(i) .and. rvRg(i) == rvRg(i) .and. rvRcs(i) > 0.) then
					if(rvRg(i) >= rvRcs(i)) then
						rvFcd(i) = 0.0
					elseif(rvRg(i) <= 0.) then
						rvFcd(i) = 1.0
					else
						rvFcd(i) = 1.35*(rvRg(i) / rvRcs(i)) - 0.35
					end if
					rvFcd(i) = min(max(0.0, rvFcd(i)), 1.0)
					rAvgFcd = rAvgFcd + rvFcd(i)
					iNumFcd = iNumFcd + 1
				end if
			end do
			if(iNumFcd > 0) then
				rAvgFcd = rAvgFcd / iNumFcd
			else
				rAvgFcd = NaN
			end if

			! Propagate mean N over "invalid" hours
			do i = ivDayBegin(iDay), ivDayEnd(iDay)
				rvFcd(i) = rAvgFcd
			end do

		end do

		! Leave
		deallocate(ivDayBegin, ivDayEnd)
		deallocate(ivDay)
	
	end function Cloudiness


	function GroundHeatFlux(Rn, LAI) result(G)

		! Routine arguments
		real, intent(in)	:: Rn	! Net radiation (W/m2)
		real, intent(in)	:: LAI	! Leaf Area Index
		real				:: G

		! Locals
		real	:: Kg

		! Compute the desired quantity
		if(Rn > 0.) then
			Kg = 0.4
		else
			Kg = 2.0
		end if
		G = Kg * Rn * exp(-0.5*LAI)

	end function GroundHeatFlux


	! Water vapor saturation pressure, given temperature
	FUNCTION WaterSaturationPressure(Ta) RESULT(es)

		! Routine arguments
		REAL, INTENT(IN)	:: Ta	! Air temperature (K)
		REAL			:: es	! Saturation vapor pressure (hPa)

		! Locals
		! -none-

		! Compute water saturation pressure according to the basic definition
		IF(Ta > 273.15) THEN
			es = EXP(-6763.6/Ta - 4.9283*LOG(Ta) + 54.23)
		ELSE
			es = EXP(-6141.0/Ta + 24.3)
		END IF

	END FUNCTION WaterSaturationPressure


	! Saturation water vapor pressure given air temperature, using
	! ASCE formula, a variant (up to constants decimals) of
	! Clausius-Clapeyron formula. This routine is the recommended
	! replacement of E_SAT.
	!
	!     Input: T = air temperature (∞C)
	!
	!     Output: ESAT = saturation vapor pression (hPa)
	!
	function E_SAT_1(T) result(rEsat)

		! Routine arguments
		real, intent(in)	:: T
		real			:: rEsat

		! Locals
		! -none-

		! Compute the data item required
		rEsat = 6.108*EXP(17.27*T/(T+237.3))

	end function E_SAT_1


	! Precipitable water given water vapor pressure
	!
	!	Input:
	!
	!		Ea		Actual water vapor pressure (hPa)
	!
	!		Pa		Actual pressure at measurement altitude (i.e. not reduced to mean sea level) (hPa)
	!
	!	Output:
	!
	!		W		Precipitable water (mm)
	!
	function PrecipitableWater(Ea, Pa) result(W)

		! Routine arguments
		real, intent(in)	:: Ea, Pa
		real				:: W

		! Locals
		! -none-

		! Compute the data item required
		W = 0.0014*Ea*Pa + 2.1

	end function PrecipitableWater


    ! Compute the derivative of the saturation vapor pressure multiplied
    ! by P/0.622; the input temperature is in ∞K.
	FUNCTION D_E_SAT(T) RESULT(DEsat)

	    ! Routine arguments
	    REAL, INTENT(IN)    :: T
	    REAL                :: DEsat

	    ! Locals
	    REAL, PARAMETER :: E0 =   0.6112
	    REAL, PARAMETER :: a  =  17.67
	    REAL, PARAMETER :: T0 = 273.15
	    REAL, PARAMETER :: Tb =  29.66

	    ! Compute the saturation vapor tension
	    DEsat = E0*a*(1./(T-Tb) + (T-T0)/(T-Tb)**2)*EXP(a*(T-T0)/(T-Tb))
!
	END FUNCTION D_E_SAT


	! Water vapor partial pressure, given wet and dry bulb temperatures and
	! air pressure.
	!
	FUNCTION WaterVaporPressure(Tw, Td, Pa) RESULT(Ew)

		! Routine arguments
		REAL, INTENT(IN)	:: Tw	! Wet bulb temperature (K)
		REAL, INTENT(IN)	:: Td	! Dry bulb temperature (K)
		REAL, INTENT(IN)	:: Pa	! Atmospheric pressure (hPa)
		REAL			:: Ew	! Water vapor partial pressure (hPa)

		! Locals
		REAL	:: TwetCelsius
		REAL	:: ExcessTemp
		REAL	:: FractionalDeltaP

		! Compute the information desired
		TwetCelsius = Tw - 273.15
		ExcessTemp  = Td - Tw		! In Nature dry bulb temperature is greater or equal to wet bulb temperature
		IF(ExcessTemp > 0.) THEN
			FractionalDeltaP = (0.00066/10.) * (1. + 0.00115*TwetCelsius)*ExcessTemp
			Ew               = WaterSaturationPressure(Tw) - FractionalDeltaP * Pa
		ELSE
			Ew               = NaN
		END IF

	END FUNCTION WaterVaporPressure


	! Relative humidity, given wet and dry bulb temperatures and
	! air pressure.
	!
	FUNCTION RelativeHumidity(Tw, Td, Pa) RESULT(RelH)

		! Routine arguments
		REAL, INTENT(IN)	:: Tw	! Wet bulb temperature (K)
		REAL, INTENT(IN)	:: Td	! Dry bulb temperature (K)
		REAL, INTENT(IN)	:: Pa	! Atmospheric pressure (hPa)
		REAL			:: RelH	! Relative humidity (%)

		! Locals
		! --none--

		! Compute the information desired
		RelH = 100. * WaterVaporPressure(Tw, Td, Pa) / WaterSaturationPressure(Td)

	END FUNCTION RelativeHumidity


	! Absolute humidity given dry bulb temperature and water vapor pressure.
	!
	FUNCTION AbsoluteHumidity(Td, Ea) RESULT(RhoW)

		! Routine arguments
		REAL, INTENT(IN)	:: Td	! Dry bulb temperature (K)
		REAL, INTENT(IN)	:: Ea	! Water vapor pressure (hPa)
		REAL			:: RhoW	! Absolute humidity (kg/m3)

		! Locals
		! --none--

		! Compute the information desired
		RhoW = 100.0*Ea/(461.5*Td)

	END FUNCTION AbsoluteHumidity


	! Air density given dry bulb temperature and atmospheric pressure.
	!
	FUNCTION AirDensity_4(Td, Pa) RESULT(Rho)

		! Routine arguments
		REAL, INTENT(IN)	:: Td	! Dry bulb temperature (K)
		REAL, INTENT(IN)	:: Pa	! Atmospheric pressure (hPa)
		REAL			    :: Rho	! Air density (kg/m3)

		! Locals
		! --none--

		! Compute the information desired
		Rho = 100.0*Pa/(287.*Td)

	END FUNCTION AirDensity_4


	! Air density given dry bulb temperature and atmospheric pressure.
	!
	! Double precision version.
	!
	FUNCTION AirDensity_8(Td, Pa) RESULT(Rho)

		! Routine arguments
		REAL(8), INTENT(IN)	:: Td	! Dry bulb temperature (K)
		REAL(8), INTENT(IN)	:: Pa	! Atmospheric pressure (hPa)
		REAL(8) 			:: Rho	! Air density (kg/m3)

		! Locals
		! --none--

		! Compute the information desired
		Rho = 100.0d0*Pa/(287.d0*Td)

	END FUNCTION AirDensity_8


	! Product of air density and the constant-pressure atmospheric thermal capacity,
	! given dry bulb temperature and atmospheric pressure.
	!
	FUNCTION RhoCp_4(Td, Pa) RESULT(rRhoCp)

		! Routine arguments
		REAL, INTENT(IN)		    :: Td		! Dry bulb temperature (K)
		REAL, INTENT(IN), OPTIONAL	:: Pa		! Air pressure (hPa)
		REAL				        :: rRhoCp	! Product of air density and
								                ! constant-pressure thermal
								                ! capacity

		! Locals
		REAL	:: Rho
		REAL	:: Cp

		! Compute the information desired
		IF(PRESENT(Pa)) THEN
			! Pressure is available: use complete formula
			Rho = AirDensity(Td, Pa)
			Cp  = 1005.0 + (Td - 250.0)**2/3364.0	! From Garratt, 1992
			rRhoCp = Rho * Cp
		ELSE
			! Pressure not available on entry: use the simplified relation
			rRhoCp = 1305. * 273.15/Td
		END IF

	END FUNCTION RhoCp_4


	! Product of air density and the constant-pressure atmospheric thermal capacity,
	! given dry bulb temperature and atmospheric pressure.
	!
	! Double precision version.
	!
	FUNCTION RhoCp_8(Td, Pa) RESULT(rRhoCp)

		! Routine arguments
		REAL(8), INTENT(IN)		        :: Td		! Dry bulb temperature (K)
		REAL(8), INTENT(IN), OPTIONAL	:: Pa		! Air pressure (hPa)
		REAL(8) 				        :: rRhoCp	! Product of air density and
								                    ! constant-pressure thermal
								                    ! capacity

		! Locals
		REAL(8)	:: Rho
		REAL(8)	:: Cp

		! Compute the information desired
		IF(PRESENT(Pa)) THEN
			! Pressure is available: use complete formula
			Rho = AirDensity_8(Td, Pa)
			Cp  = 1005.0d0 + (Td - 250.0d0)**2/3364.0d0	! From Garratt, 1992
			rRhoCp = Rho * Cp
		ELSE
			! Pressure not available on entry: use the simplified relation
			rRhoCp = 1305.d0 * 273.15d0/Td
		END IF

	END FUNCTION RhoCp_8


	! Latent vaporization heat given temperature,
	! computed according the ASCE Report.
	function LatentVaporizationHeat(rTemp, iCalculationType) result(rLambda)

		! Routine arguments
		real, intent(in)	:: rTemp			! (°C)
		integer, intent(in)	:: iCalculationType	! ASCE_STANDARDEQ, ASCE_MEANTEMPERATURE
		real				:: rLambda			! (W/m2)

		! Locals
		! -none-

		! Compute the information desired
		select case(iCalculationType)
		case(ASCE_STANDARDEQ)
			rLambda = 2.45 * 1.e6 / 3600.0
		case(ASCE_MEANTEMPERATURE)
			rLambda = (2.501 - 2.361e-3 * rTemp) * 1.e6 / 3600.0
		case default
			rLambda = NaN
		end select

	end function LatentVaporizationHeat


	! Estimate wet bulb temperature from dry bulb temperature, relative
	! humidity and pressure.
	!
	! The estimation is computed by solving the equation
	!
	!	Delta(Tw, Td, Ur, Pa) = 0
	!
	! for "Tw", where "Delta" is found later in the auxiliary functions
	! part of this module. As "Delta" is not everywhere differentiable with
	! respect to "Tw", for prudence a derivative-independent solver is used.
	! Actually, a two-stage approach has been followed: in the first stage
	! an initial rough bracketing of the solution is progressively made
	! smaller by bisection method. In second stage, the final solution is
	! found by secant method.
	!
	! Usage note:	Rough and fine tolerances, "RoughTol" and "FineTol", are
	! ===========	typically set to 0.1 and 0.001 respectively. In my feeling
	!		there is no real need to change them, so I made both parameters
	! optional with appropriate defaults. But on occasions you may want to experiment
	! with different values. In this case, you should ensure that
	!
	!	RoughTol << FineTol
	!
	! I recommend the fine tolerance to be some orders of magnitude smaller than
	! the rough tolerance; the smaller the rough tolerance, the higher iteration count
	! will be in bisection phase (which is more robust than secant method, but less
	! "efficient", in the sense convergence is slower).
	!
	FUNCTION WetBulbTemperature(Td, Ur, Pa, RoughTol, FineTol, MaxIter, Method) RESULT(Tw)

		! Routine arguments
		REAL, INTENT(IN)		:: Td		! Dry bulb (that is "ordinary") temperature (K)
		REAL, INTENT(IN)		:: Ur		! Relative humidity (%)
		REAL, INTENT(IN)		:: Pa		! Air pressure (hPa)
		REAL, INTENT(IN), OPTIONAL	:: RoughTol	! Maximum bracketing step error admitted on wet bulb temperature (K, default 0.1)
		REAL, INTENT(IN), OPTIONAL	:: FineTol	! Maximum refinement step error admitted on wet bulb temperature (K, default 0.001)
		INTEGER, INTENT(IN), OPTIONAL	:: MaxIter	! Maximum number of iterations (default: 100)
		INTEGER, INTENT(IN), OPTIONAL	:: Method	! Method used for performing calculations (1:Standard (default), 2:Simplified - see R. Stull, "Wet bulb temperature from relative humidity and air temperature", Bulletin of the AMS, Nov 2011)
		REAL				:: Tw		! Wet bulb temperature (K)

		! Locals
		REAL	:: rRoughTol
		REAL	:: rFineTol
		INTEGER	:: iMaxIter
		INTEGER	:: iMethod
		REAL	:: a, b			! Minimum and maximum of bracketing interval
		REAL	:: da, db		! Delta values corresponding to a and b respectively
		real	:: T

		! Set default input parameters
		IF(PRESENT(RoughTol)) THEN
			rRoughTol = RoughTol
		ELSE
			rRoughTol = 0.1
		END IF
		IF(PRESENT(FineTol)) THEN
			rFineTol = FineTol
		ELSE
			rFineTol = 0.001
		END IF
		IF(PRESENT(MaxIter)) THEN
			iMaxIter = MaxIter
		ELSE
			iMaxIter = 100
		END IF
		IF(PRESENT(Method)) THEN
			iMethod = Method
		ELSE
			iMethod = 1
		END IF

		! Dispatch execution based on method
		SELECT CASE(iMethod)

		CASE(1)

			! Bracket solution using bisection method first
			CALL Bisect(0., Td, Ur, Pa, rRoughTol, a, b, da, db)
			Tw = Secant(a, b, da, db, Td, Ur, Pa, rFineTol, iMaxIter)

		CASE(2)

			! Stull simplified method
			T  = Td - 273.15
			Tw = T * atan(0.151977 * sqrt(Ur + 8.313659)) + atan(T + Ur) - atan(Ur - 1.676331) + &
				 0.00391838*Ur**(3./2.) * atan(0.023101 * Ur) - 4.686035 + 273.15

		CASE DEFAULT

			! Bracket solution using bisection method first
			CALL Bisect(0., Td, Ur, Pa, rRoughTol, a, b, da, db)
			Tw = Secant(a, b, da, db, Td, Ur, Pa, rFineTol, iMaxIter)

		END SELECT

	END FUNCTION WetBulbTemperature
	!
	! Motivations and whys - I've chosen a two-staged approach in which first is
	! ====================   bisection because this algorithm is sturdy, although
	!                        inefficient. As "Delta" is a monotonically increasing
	! function, but with one essential discontinuity at 0 °C (just where we need it
	! the most) I preferred this approach to bracket the solution to a tiny interval
	! so that the chance of finding adverse effects due to the discontinuity are
	! minimized. Once the search interval is well reduced
	! the final solution is found by secant method, more efficient but
	! somewhat less robust than bisection.
	!
	! That "Delta" is really increasing with "Tw" you can check on yourself by
	! direct inspection or testing (I've used both). Anyway, monotonicity of
	! "Delta" is essential for this routine to work as intended.
	!
	! Note about Stull method. As you can see I've implemented Stull's new simplified
	! method (non-default parameter Method==2). Then I've tested it, and found it to
	! depart quite significantly from the true value; on occasions I've noticed the
	! predicted wet bulb temperature to exceed dry bulb, which cannot be for physical reasons.
	! Investigations should be performed to check where is Stull method best suited. I guess
	! the range will depend on pressure being close to reference value.


	! Estimate atmospheric pressure given height
	function AirPressure1(rZ) result(rPk)

		implicit none

		! Routine arguments
		real, intent(in)	:: rZ				! Altitude at which pressure is desired (m above msl)
		real				:: rPk				! Estimated pressure (hPa)

		! Locals
		real		:: rTK0		! Reference temperature (K)

		! Constants
		real, parameter	:: P0 = 1013.		! Pressure at reference altitude (hPa)
		real, parameter	:: g  = 9.807		! Gravitation acceleration (m/s2)
		real, parameter	:: z0 = 0.			! Reference altitude for expressing pressure (m above msl)
		real, parameter	:: R  = 287.0		! Specific gas constant (J/kg/K)
		real, parameter	:: Alpha1 = 0.0065	! Constant lapse rate of moist air (K/m)

		! Reference temperature
		rTK0 = 293.15

		! Compute pressure
		rPk = P0*((rTK0 - Alpha1*(rZ - z0))/rTK0)**(g/(Alpha1*R))

	end function AirPressure1


	! Estimate atmospheric pressure given height and temperature
	function AirPressure2(rZ, rTemp, rZr, iCalculationType) result(rPk)

		implicit none

		! Routine arguments
		real, intent(in)	:: rZ				! Altitude at which pressure is desired (m above msl)
		real, intent(in)	:: rTemp			! Air temperature (°C)
		real, intent(in)	:: rZr				! Height at which temperature measurements are taken (m)
		integer, intent(in)	:: iCalculationType	! ASCE_STANDARDATMOSPHERE, ASCE_STANDARDEQ, ASCE_MEANTEMPERATURE
		real				:: rPk				! Estimated pressure (hPa)

		! Locals
		real		:: rTK0		! Reference temperature (K)

		! Constants
		real, parameter	:: P0 = 1013.		! Pressure at reference altitude (hPa)
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
			rTK0 = rTemp + 273.15
		case default
			rPk = NaN
			return
		end select

		! Compute pressure
		rPk = P0*((rTK0 - Alpha1*(rZ - z0))/rTK0)**(g/(Alpha1*R))

	end function AirPressure2


	function VirtualTemperature(Temp, ea, P) result(Tv)

		implicit none

		! Routine arguments
		real, intent(in)	:: Temp		! (°C)
		real, intent(in)	:: ea		! (hPa)
		real, intent(in)	:: P		! (hPa)
		real				:: Tv		! (°C)

		! Locals
		! -none-

		! Compute the information desired
		Tv = (Temp + 273.15)/(1.0 - 0.378*ea/P) - 273.15

	end function VirtualTemperature


	! Estimate dew point temperature using Magnus formula enhanced using Arden Buck equation
	FUNCTION DewPointTemperature(Td, Ur) RESULT(Dp)

		! Routine arguments
		REAL, INTENT(IN)				:: Td		! Dry bulb (that is "ordinary") temperature (K)
		REAL, INTENT(IN)				:: Ur		! Relative humidity (%)
		REAL						    :: Dp       ! Dew point (K)

		! Locals
		REAL, PARAMETER	:: a =   6.112
		REAL, PARAMETER	:: b =  17.62
		REAL, PARAMETER	:: c = 243.12
		REAL, PARAMETER	:: d = 234.5
		REAL		:: T, G

		! Convert temperature to °C (all relations we use assume Celsius degrees)
		! and then obtain dew point temperature
		T  = Td - 273.15
		G  = LOG(Ur/100.0*EXP((b-T/d)*(T/(c+T))))
		Dp = c*G/(b-G) + 273.15

	END FUNCTION DewPointTemperature


	! Estimate sonic temperature given dry bulb ("normal") temperature, relative
	! humidity and atmospheric pressure.
	!
	! Routine "SonicTemperature" must compute wet bulb temperature estimate prior
	! to compute the desired sonic temperature value. The most apparent consequence
	! is tolerances and method are necessary too. The second effect is the resulting
	! estimate, based itself on estimates, may be quite poor.
	!
	! See documentation of "WetBulbTemperature" for clarifications.
	!
	FUNCTION SonicTemperature(Td, Ur, Pa, RoughTol, FineTol, MaxIter, Method) RESULT(Ts)

		! Routine arguments
		REAL, INTENT(IN)		:: Td		! Dry bulb (that is "ordinary") temperature (K)
		REAL, INTENT(IN)		:: Ur		! Relative humidity (%)
		REAL, INTENT(IN)		:: Pa		! Air pressure (hPa)
		REAL, INTENT(IN), OPTIONAL	:: RoughTol	! Maximum bracketing step error admitted on wet bulb temperature (K, default 0.1)
		REAL, INTENT(IN), OPTIONAL	:: FineTol	! Maximum refinement step error admitted on wet bulb temperature (K, default 0.001)
		INTEGER, INTENT(IN), OPTIONAL	:: MaxIter	! Maximum number of iterations (default: 100)
		INTEGER, INTENT(IN), OPTIONAL	:: Method	! Method used for performing calculations (1:Standard (default), 2:Simplified - see R. Stull, "Wet bulb temperature from relative humidity and air temperature", Bulletin of the AMS, Nov 2011)
		REAL				:: Ts		! Sonic temperature (K)

		! Locals
		REAL	:: rRoughTol
		REAL	:: rFineTol
		INTEGER	:: iMaxIter
		INTEGER	:: iMethod
		REAL	:: Tw

		! Set default input parameters
		IF(PRESENT(RoughTol)) THEN
			rRoughTol = RoughTol
		ELSE
			rRoughTol = 0.1
		END IF
		IF(PRESENT(FineTol)) THEN
			rFineTol = FineTol
		ELSE
			rFineTol = 0.001
		END IF
		IF(PRESENT(MaxIter)) THEN
			iMaxIter = MaxIter
		ELSE
			iMaxIter = 100
		END IF
		IF(PRESENT(Method)) THEN
			iMethod = Method
		ELSE
			iMethod = 1
		END IF

		! Compute the ultrasonic anemometer temperature estimate by
		! applying the direct definition
		Tw = WetBulbTemperature(Td, Ur, Pa, RoughTol, FineTol, MaxIter, Method)
		Ts = Td*(1.+0.51*0.622*WaterVaporPressure(Tw, Td, Pa)/Pa)

	END FUNCTION SonicTemperature


	! Estimate solar global radiation using the MPDA method
	function GlobalRadiation_MPDA(C, sinPsi) result(Rg)

		! Routine arguments
		real, intent(in)	:: C		! Cloud cover fraction (0 to 1)
		real, intent(in)	:: sinPsi	! Sine of solar elevation angle (° above horizon; negative below)
		real				:: Rg		! Estimate of the global solar radiation (W/m2)1-0.75

		! Locals
		real	:: rSinMin
		real	:: rCloud

		! Constants
		real, parameter	:: a1 = 990.
		real, parameter	:: a2 = -30.
		real, parameter	:: b1 =  -0.75
		real, parameter	:: b2 =   3.4

		! Check input parameter make sense
		if((.invalid.C) .or. (.invalid.sinPsi)) then
			Rg = NaN
			return
		end if
		if(C < 0. .or. sinPsi < -1. .or. sinPsi > 1.) then
			Rg = NaN
			return
		end if

		! Constrain cloud cover to senseful interval
		rCloud = max(min(C, 1.), 0.)

		! Estimate the minimum sine of solar elevation
		rSinMin = - a2 / a1
		if(sinPsi >= rSinMin) then
			Rg = (a1*sinPsi*exp(-0.057/sinPsi))*(1.+b1*rCloud**b2)
		else
			Rg = 0.
		end if

	end function GlobalRadiation_MPDA


	! Estimate the cloud cover using MPDA method
	function CloudCover_MPDA(Rg, sinPsi) result(C)

		! Routine arguments
		real, intent(in)	:: Rg		! Global radiation (W/m2)
		real, intent(in)	:: sinPsi	! Sine of solar elevaton angle
		real				:: C		! Cloud cover fraction

		! Locals
		real	:: rSinMin
		real	:: maxRg

		! Constants
		real, parameter	:: a1 = 990.
		real, parameter	:: a2 = -30.
		real, parameter	:: b1 =  -0.75
		real, parameter	:: b2 =   3.4

		rSinMin = -a2/a1
		if(sinPsi >= rSinMin) then
			maxRg =  a1*sinPsi * exp(-0.057/sinPsi)
			if(Rg >= maxRg) then
				C = 0.
			else
				C = (1./b1*(Rg/maxRg-1.))**(1./b2)
			end if
		else
			C = 0.5
		end if

	end function CloudCover_MPDA


	! Estimate the net radiation by MPDA method. The relation used,
	! based on a grey body approximation, tends to be accurate when
	! the global radiation is greater than zero, then on daytime.
	! Over night-time (actually, when the value returned by this
	! function is negative), the function NighttimeNetRadiation
	! should be called instead
	function NetRadiation_MPDA(land, albedo, Td, Rg, C, z0, zr, vel) result(Rn)

		! Routine arguments
		integer, intent(in)	:: land			! Simplified land use code:
											!   1: Desert
											!   2: Dry rural
											!   3: Dense urban fabric
											!   4: Sparse urban fabric, sub-urban
											!   5: Forests, prairies, irrigated coltures
											!   6: Water bodies
		real, intent(in)	:: albedo		! Albedo coefficient
		real, intent(in)	:: Td			! Dry bulb (ordinary) temperature (°C)
		real, intent(in)	:: Rg			! Global radiation (W/m2)
		real, intent(in)	:: C			! Cloud cover fraction (0 to 1)
		real, intent(in)	:: z0			! Aerodynamic roughness length (m)
		real, intent(in)	:: zr			! Anemometer height above ground (m)
		real, intent(in)	:: vel			! Wind speed (m/s)
		real				:: Rn			! Estimated net radiation (W/m2)

		! Locals
		real	:: Ta
		real	:: a
		real	:: s
		real	:: tt
		real	:: c3
		real	:: u2
		integer	:: k

		! Constant parameters
		real, dimension(6), parameter	:: alpha = [0.1, 0.3, 0.5, 0.8, 1.0, 1.4]
		real, dimension(9), parameter	:: a0    = [-96.1,-101.5,-76.1,-80.1,-53.5,-45.3,-35.5,-23.0,-9.9]
		real, dimension(4), parameter	:: a1    = [-16.4,-12.6,-13.0,-9.8]
		real, dimension(4), parameter	:: a2    = [1.35,0.99,1.16,0.9]
		real, dimension(4), parameter	:: a3    = [100.e-15,104.e-15,66.e-15,72.e-15]
		real, parameter					:: c1    = 5.31e-13
		real, parameter					:: c2    = 60.
		real, parameter					:: sigma = 5.67e-08
		real, parameter					:: pi    = 3.14159265

		! Check parameters
		if(C < 0. .or. C > 1.) then
			Rn = NaN
			return
		end if

		! Compute a preliminary estimate of net radiation
		Ta = Td + 273.15
		a  = alpha(land)
		s  = 1.05*exp((6.42-Td)/17.78)
		c3 = 0.38*((1.-a)+1.)/(1.+s)
		Rn = ((1.-albedo)*Rg+c1*Ta**6-sigma*Ta**4+c2*C)/(1.+c3)

		! If the preliminary estimate is negative apply the "nocturnal" formula
		! to get a more refined estimate
		if(Rn < 0.) then
			k = nint(C*8.) + 1		! 'k' belongs to range 1..9
			if(k > 4) then
				Rn = a0(k)
			else
				u2 = log(2./z0)/log(zr/z0) * vel
				Rn = a0(k) + a1(k)*u2 + a2(k)*u2*u2 + a3(k)*Ta**6
			end if
		end if

	end function NetRadiation_MPDA


	! Indicative evaluation of Brunt-Vaisala frequency. Notice this function yields a valid value
	! even under unstable situations, when in principle the Brunt-Vaisala frequency is not
	! defined: this is intentional, and may be overcome by programmatically invalidate the values obtained under
	! neutral and unstable conditions.
	function BruntVaisala(Td, z) result(N)

		! Routine arguments
		real, intent(in)	:: Td	! Virtual temperature (°C)
		real, intent(in)	:: z	! Height above ground level (m)
		real				:: N	! Brunt-Vaisala frequency (Hz)

		! Locals
		real	:: Tpot	! Potential temperature (K)

		! Compute the information desired
		if(z < 0. .OR. Td < -100.) then
			N = NaN
			return
		endif
		Tpot = Td + 273.15 + 0.0098 * z
		N    = sqrt(abs(9.807/Tpot * 0.0098))

	end function BruntVaisala


	! Estimation of PBL parameters given net radiation, wind speed and temperature
	!
	! Input:
	!
	!	iLandUse	: Land use code (integer, 1 to 6)
	!	z0_in		: Aerodynamic surface roughness (real, m)
	!	zr			: Anemometer height above ground (real, m)
	!	Vel			: Horizontal wind speed (real, m/s)
	!	T			: Air temperature (real, °C)
	!	Rn			: Net radiation (real, W/m2)
	!	N			: Cloud cover fraction (real, 0.0 to 1.0)
	!
	! Output:
	!
	!	u_star		: Friction velocity (real, m/s)
	!	T_star		: Scale temperature (real, °C)
	!	H0			: Turbulent sensible heat flux (real, W/m2)
	!	hlm1		: Stability parameter (= zr/L, with L the Obukhov length) (real, dimensionless)
	!
	! Function return value: Error code (always 0, "success", in current version)
	!
	function PBL_Parameters(iLandUse_in, z0_in, zr, Vel, T, Rn, N, u_star, T_star, H0, hlm1) result(iRetCode)

		! Routine arguments
		integer, intent(in)	:: iLandUse_in
		real, intent(in)	:: z0_in
		real, intent(in)	:: zr
		real, intent(in)	:: Vel
		real, intent(in)	:: T
		real, intent(in)	:: Rn
		real, intent(in)	:: N
		real, intent(out)	:: u_star
		real, intent(out)	:: T_star
		real, intent(out)	:: H0
		real, intent(out)	:: hlm1
		integer				:: iRetCode

		! Locals
		real	:: z0
		integer	:: iLandUse
		real	:: rhoCp
		real	:: r_ground
		real	:: alu
		real	:: aln
		real	:: usn
		real	:: S
		real	:: a
		real	:: d1
		real	:: d2
		real	:: d3
		real	:: uss
		real	:: uuu
		real	:: zz0

		! Constants
		real, dimension(6), parameter	:: alpha = [0.1, 0.3, 0.5, 0.7, 1.0, 1.4]
		real, parameter					:: beta = 20.
		real, parameter					:: k    =  0.4
		real, parameter					:: g    =  9.807

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Force parameters in limits
		z0 = z0_in
		if(z0_in <= 0.) z0 = 0.023
		iLandUse = iLandUse_in
		if(iLandUse < 1 .or. iLandUse > 6) iLandUse = 4

		! Initializations
		rhoCp     = 1305.*273.16/(T + 273.15)
		r_ground  = 0.8
		alu       = ALOG(zr/z0)
		S         = 1.05*EXP( (6.42-T)/17.78 )
		a         = alpha(iLandUse)
  
		! Estimate the turbulent sensible heat flux
		H0 = (1.0 - a + S)/(1 + S) * r_ground * Rn - beta
  
		if(H0 > 0.) then	! "Convective"

			! Rough estimate of u* from the logarithmic profile
			usn = k * Vel / alu

			! Refine u* estimation
			zz0 = z0/zr
			aln = alog(z0/zr)
			if(zz0 <= 0.01) then
				d1 = 0.128 + 0.005 * aln
			else
				d1 = 0.107
			end if
			d2 = 1.95 + 32.6 * zz0**0.45
			IF(h0 <= 0.) then
				d3 = 0.
			else
				d3 = H0/rhoCp * (k*g*zr)/((T + 273.15) * usn**3)
			end if
			u_star = max(usn * (1. + d1*alog(1. + d2*d3)), 0.05)

			! Compute T*
			T_star = -H0 / rhoCp / u_star

			! Compute stability parameter
			hlm1 = k * g / (T+273.15) * T_star / u_star**2

		else

			! Estimate T*
			T_star = min(0.09 * (1. - 0.5*N**2), k * (T+273.15) * Vel**2 / (18.8 * zr * g * alu))

			! Estimate u*, H0, hlm1
			uss = 0.5 * k * Vel / alu
			uuu = 1. - 4. * 4.7 * g * zr * T_star * alu / (k * (T+273.15) * Vel**2)
			if(uuu <= 0.) then
				hlm1   = 0.2
				u_star = k * Vel / (alu + 4.7 * zr * hlm1)
				u_star = max(u_star, k / alu * Vel)
				H0     = -rhoCp * u_star * T_star
			else
				u_star = max(uss * (1+sqrt(uuu)), k / alu * Vel)
				H0     = -rhoCp * u_star * T_star
				hlm1   = k * g / (T+273.15) * T_star / u_star**2
			end if

		end if

	end function PBL_Parameters



	! ***************************************
	! * Auxiliary functions (not accessible *
	! * through public interface)           *
	! ***************************************


	! Auxiliary function used by "TWET" for estimating wet bulb temperature. Given dry
	! bulb temperature, relative humidity and air pressure the wet bulb temperature is
	! the value of "Tw" at which the auxiliary function is 0.
	!
	! A simple analysis may show the auxiliary function to be monotonically increasing
	! with Tw in the interval 0 <= Tw <= Td.
	!
	! It is useful to understand where "Delta" comes from. The starting point is the
	! equation giving water vapor partial pressure,
	!
	!	E = ESAT(Tw) - 0.00066*(1.+0.00115*(Tw-273.15))*(Pa/10.)*(Td-Tw)				       (1)
	!
	! where "Tw" and "Td" are wet and dry bulb temperatures, "Pa" is air pressure,
	! and ESAT(T) the water vapor saturation pressure at temperature T.
	!
	! Now, let's consider water vapor partial pressure: the following definition
	! connects it to relative humidity, "Ur", and ESAT(Td).
	!
	!	Ur = 100.*E/ESAT(Td)
	!
	! This relation is the same as
	!
	!	E = (Ur/100.)*ESAT(Td)
	!
	! which, upon replacing in formula (1) yields
	!
	!	(Ur/100.)*ESAT(Td) = ESAT(Tw) - 0.00066*(1.+0.00115*(Tw-273.15))*(Pa/10.)*(Td-Tw)
	!
	! or
	!
	!	ESAT(Tw) - 0.00066*(1.+0.00115*(Tw-273.15))*(Pa/10.)*(Td-Tw) - (Ur/100.)*ESAT(Td) = 0  (2)
	!
	! This latter is an equation in "Tw", whose solution is the desired wet bulb
	! temperature. Monotonicity with respect to "Tw" then guarantees the solution
	! uniqueness (not its existence, however: ESAT(T) has a discontinuity at 0 °C
	! at which existence cannot be ensured a priori).
	!
	! The left member of equation (2) can be considered a function in "Tw", whose
	! value starts negative, to reach zero at wet bulb temperature, and finally
	! becomes positive. By solving it numerically, we get the desired wet bulb temperature.
	!
	FUNCTION Delta(Tw, Td, Ur, Pa) RESULT(d)

		! Routine arguments
		REAL, INTENT(IN)	:: Tw	! Tentative wet bulb temperature (K)
		REAL, INTENT(IN)	:: Td	! Known dry bulb temperature (K)
		REAL, INTENT(IN)	:: Ur	! Known relative humidity (%)
		REAL, INTENT(IN)	:: Pa	! Known atmospheric pressure (hPa)
		REAL			:: d	! The corresponding value of auxiliary function.

		! Locals
		! -none-

		! Compute the information desired
		d = WaterVaporPressure(Tw, Td, Pa) - (Ur/100.)*WaterSaturationPressure(Td)

	END FUNCTION Delta


	! Dedicated implementation of bisection method. It differs from the
	! standard algorithm by:
	!
	!	1)	It solves *only* equation "Delta() == 0" (see above)
	!	2)	No limit on iteration count
	!
	! The reason of the second point above is that the number of iterations
	! required, O(log2(273,15/Tol)), is always small if accuracy is in the
	! expected range 0.1-0.01.
	!
	! Bisection is used to restrict the initial solution bracketing interval
	! [0,Td] to [TwMin,TwMax] where
	!
	!	TwMax - TwMin <= Tol
	!
	! so that further use of secant method is guaranteed to easily converge.
	! Subroutine interface of Bisect is designed to provide all initialization
	! data (namely, including "da" and "db") to Secant saving a couple of function
	! evaluation: it *is* redundant, but this redundancy is desired.
	!
	SUBROUTINE Bisect(TdMin, TdMax, Ur, Pa, Tol, a, b, da, db)

		! Routine arguments
		REAL, INTENT(IN)	:: TdMin	! Initial lower bound of temperature (K)
		REAL, INTENT(IN)	:: TdMax	! Dry bulb temperature (K)
		REAL, INTENT(IN)	:: Ur		! Relative humidity (%)
		REAL, INTENT(IN)	:: Pa		! Atmospheric pressure (hPa)
		REAL, INTENT(IN)	:: Tol		! Tolerance (K)
		REAL, INTENT(OUT)	:: a		! Lower bound on temperature (K)
		REAL, INTENT(OUT)	:: b		! Upper bound on temperature (K)
		REAL, INTENT(OUT)	:: da		! Value of "Delta" at "a"
		REAL, INTENT(OUT)	:: db		! Value of "Delta" at "b"

		! Locals
		REAL	:: p
		REAL	:: dp

		! Initialize
		a  = TdMin
		da = Delta(a, TdMax, Ur, Pa)
		b  = TdMax
		db = Delta(b, TdMax, Ur, Pa)

		! Main loop: bisect interval until rough tolerance is met, or an error is found
		DO
			p  = a + (b-a)/2
			dp = Delta(p, TdMax, Ur, Pa)
			IF(da*dp > 0.) THEN
				a  = p
				da = dp
			ELSE
				b  = p
				db = dp
			END IF
			IF((b-a)/2. < Tol) EXIT
		END DO

	END SUBROUTINE Bisect


	! Dedicated routine for refining the estimate of wet bulb temperature
	! obtained from Bisect.
	FUNCTION Secant(a0, b0, da0, db0, Td, Ur, Pa, Tol, MaxIter) RESULT(Tw)

		! Routine arguments
		REAL, INTENT(IN)	:: a0		! Initial lower bound of wet bulb temperature (K)
		REAL, INTENT(IN)	:: b0		! Initial upper bound of wet bulb temperature (K)
		REAL, INTENT(IN)	:: da0		! Value of "Delta" at "a0"
		REAL, INTENT(IN)	:: db0		! Value of "Delta" at "b0"
		REAL, INTENT(IN)	:: Td		! Dry bulb temperature (K)
		REAL, INTENT(IN)	:: Ur		! Relative humidity (%)
		REAL, INTENT(IN)	:: Pa		! Atmospheric pressure (hPa)
		REAL, INTENT(IN)	:: Tol		! Tolerance (K)
		INTEGER, INTENT(IN)	:: MaxIter	! Maximum number of iterations
		REAL			:: Tw		! Wet bulb temperature (K)

		! Locals
		REAL	:: p
		REAL	:: dp
		REAL	:: a
		REAL	:: da
		REAL	:: b
		REAL	:: db
		INTEGER	:: Iteration

		! Initialization
		a  = a0
		da = da0
		b  = b0
		db = db0
		Iteration = 1

		! Main loop
		DO
			p = b - db*(b-a)/(db-da)
			IF(ABS(p - b) < Tol) EXIT
			a  = b
			da = db
			b  = p
			db = Delta(p, Td, Ur, Pa)
			Iteration = Iteration + 1
			IF(Iteration >= MaxIter) EXIT
		END DO

		! Transmit result and leave
		Tw = p

	END FUNCTION Secant

end module pbl_thermo
