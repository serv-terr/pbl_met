! pbl_base - Fortran module, containing computations related to
!            Planetary Boundary Layer (PBL) quantities, encompassing
! the lower atmosphere thermodynamics, energy balance, psychrometry,
! and some astronomical formulae dealing with sunset/sunrise and apparent
! solar position.
!
! All former relationships dealing with net and global radiation
! are a legacy from the original PBL_MET. The ones following here are
! based on a different approach, coming from:
!
!	R.G. Allen et al, "The ASCE Standardized Reference Evapotranspiration Equation",
!	American Society of Civil Engineers, 2005
!
! This new approach (respect to old PBL_MET way) does basically replace cloud cover by water vapor pressure (which
! can easily obtained from a thermo-hygrometer) and the "cloudiness function", a
! dimensionless value obtainable from measurement of global radiation or, these missing,
! by statistical means.
!
! Written by: Mauri Favaron
!
! Routines in former PBL_MET have been written by Roberto Sozzi and Daniele Fraternali
!
module pbl_base

	use nan_support
	use psychrometry

	implicit none
	
	private
	
	! Public interface
	public	:: calcJD						! Julian day, defined according to NOAA conventions
	public	:: Leap							! Check a year is leap or not
	public	:: J_Day						! Day-in-year, from the old PBL_MET (revised)
	public	:: calcTimeJulianCent			! Compute "Julian century" from Julian day
	public	:: ClearSkyRg_Simple			! Simple estimate of global solar radiation under clear sky conditions
	public	:: ClearSkyRg_Accurate			! More accurate estimate of global solar radiation under clear sky conditions
	public	:: ExtraterrestrialRadiation	! Estimate of extraterrestrial radiation (i.e., global radiation above the Earth atmosphere)
	public	:: NetRadiation					! Estimate of solar net radiation
	public	:: Cloudiness					! Estimate cloudiness factor (see ASCE report for definitions)
	public	:: SunRiseSunSet				! Old PBL_MET evaluation of sun rise and sun set times (revised)
	public	:: SinSolarElevation			! Compute the sine of solar elevation angle
	public	:: SolarDeclination				! Compute the solar declination

contains

	! Julian day, according to NOAA conventions
	function calcJD(year, month, day) result(jd)

		! Routine arguments
		integer, intent(in)	:: year, month, day
		real				:: jd

		! Locals
		integer	:: A
		integer	:: B
		integer	:: yy
		integer	:: mm

		! Compute the Julian day corresponding to passed date
		yy = year
		mm = month
		if(mm <= 2) then
			yy = yy - 1
			mm = mm + 12
		end if
		A = yy/100
		B = 2 - A + A/4
		jd = FLOOR(365.25*(yy + 4716)) + FLOOR(30.6001*(mm+1)) + day + B - 1524.5

	end function calcJD
	
	
	! Definition of even-leap year
	function Leap(ia) result(isLeap)
	
		! Routine arguments
		integer, intent(in)	:: ia
		logical				:: isLeap
		
		! Locals
		! --none--
		
		! Check the year is leap according to the standard definition
		if(mod(ia,4) /= 0) then
			! Year, not divisible by 4, is surely even
			isLeap = .false.
		else
			! Year is divisible by 4
			if(mod(ia,100) == 0) then
				if(mod(ia,400) == 0) then
					isLeap = .true.
				else
					isLeap = .false.
				end if
			else
				isLeap = .true.
			end if
		end if
		
	end function Leap
	
	
	! Day in year from the old PBL_MET (revised). The day-in-year was
	! incorrectly called "Julian day" in old PBL_MET: I've decided to
	! retain the old routine name, as it is used extensively in extant
	! routines.
	function J_Day(ia,im,ig) result(doy)

		! Routine arguments
		integer, intent(in)	:: ia	! Year (with century)
		integer, intent(in)	:: im	! Month
		integer, intent(in)	:: id	! Day
		integer				:: doy	! Day in year
		
		! Locals
		! --none--
		
		! Parameters
		integer, dimension(13,2), parameter	:: ngm = reshape( &
			[0,31,60,91,121,152,182,213,244,274,305,335,366, &
			 0,31,59,90,120,151,181,212,243,273,304,334,365], [13,2])

		if(Leap(ia)) then
			! Leap year
			doy = ig+ngm(im,1)
		else
			! Even year
			doy = ig+ngm(im,2)
		end if

	end function J_Day


	! Convert between Julian day and Julian century (unit
	! of common use in astronomy)
	function calcTimeJulianCent(jd) result(T)

		! Routine arguments
		real, intent(in)	:: jd
		real				:: T

		! Locals
		! -none-

		! Compute the Julian century
		T = (jd - 2451545.0)/36525.0

	end function calcTimeJulianCent


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
		integer	:: ss, mm, hh, yy, mo, dy, doy
		real	:: dr
		real	:: omega, omega1, omega2, omegaS
		real	:: timenow, JD, t, Sc, b, t1
		real	:: solarDeclination, centralMeridianLongitude, localLongitude
		integer	:: iErrCode

		! Constants
		real, parameter	:: SOLAR_CONSTANT = 1.e5*49.2/3600.0		! W/m2
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
		doy = J_DAY(yy,mo,dy)

		! Compute solar declination
		solarDeclination = 0.409*SIN(2*PI/365*doy - 1.39)

		! Compute Julian day
		timenow = hh + mm/60.0 + ss/3600.0 - zone
		JD = calcJD(yy, mo, dy)

		! Inverse squared relative distance factor for Sun-Earth
		dr = 1.0 + 0.033*COS(2*PI*doy/365.0)

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
		b  = 2.*PI*(doy-81)/364.0
		Sc = 0.1645*SIN(2.0*b) - 0.1255*COS(b) - 0.025*SIN(b)

		! Solar time angle at midpoint of averaging time
		omega = (PI/12.0) * ((t + 0.06667*(centralMeridianLongitude - localLongitude) + Sc) - 12.0)

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
		integer	:: ss, mm, hh, yy, mo, dy, doy
		real	:: dr
		real	:: omega, omega1, omega2, omegaS
		real	:: timenow, JD, t, Sc, b, t1
		real	:: solarDeclination, centralMeridianLongitude, localLongitude

		! Constants
		real, parameter	:: SOLAR_CONSTANT = 1.e5*49.2/3600.0		! W/m2
		real, parameter	:: PI             = 3.1415927

		! Get date and time
		read(timeStamp, "(i4,5(1x,i2))", iostat=iErrCode) yy, mo, dy, hh, mm, ss
		if(iErrCode /= 0) then
			Ra = NaN
			return
		end if
		doy = J_DAY(yy,mo,dy)

		! Compute solar declination
		solarDeclination = 0.409*SIN(2*PI/365*doy - 1.39)

		! Compute Julian day
		timenow = hh + mm/60.0 + ss/3600.0 - zone
		JD = calcJD(yy, mo, dy)

		! Inverse squared relative distance factor for Sun-Earth
		dr = 1.0 + 0.033*COS(2*PI*doy/365.0)

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
		b  = 2.*PI*(doy-81)/364.0
		Sc = 0.1645*SIN(2.0*b) - 0.1255*COS(b) - 0.025*SIN(b)

		! Solar time angle at midpoint of averaging time
		omega = (PI/12.0) * ((t + 0.06667*(centralMeridianLongitude - localLongitude) + Sc) - 12.0)

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


	function Cloudiness(rvElAng, rvRg, rvRg3, rSunElevThreshold, rvFcd) result(iRetCode)

		implicit none

		! Routine arguments
		real, dimension(:), intent(in)	:: rvElAng
		real, dimension(:), intent(in)	:: rvRg
		real, dimension(:), intent(in)	:: rvRg3
		real, intent(in)				:: rSunElevThreshold
		real, dimension(:), intent(out)	:: rvFcd
		integer							:: iRetCode

		! Locals
		integer	:: i
		integer	:: iErrCode
		real	:: rFcdOld
		real	:: rFcdFirst
		real	:: rPhi
		real	:: rRatio
		logical:: lIsFirst = .true.

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Iterate over all radiation readings, assumed valid
		rFcdOld   = NaN
		rFcdFirst = NaN
		do i = 1, SIZE(rvRg)
			rPhi = rvElAng(i)
			if(rPhi > rSunElevThreshold) then
				rRatio = MAX(MIN(rvRg(i) / rvRg3(i), 1.0), 0.0)
				rvFcd(i) = 1.35 * rRatio - 0.35
				rFcdOld  = rvFcd(i)
			else
				rvFcd(i) = rFcdOld
			end if
			if(lIsFirst) then
				if(.not.ISNAN(rvFcd(i))) then
					rFcdFirst = rvFcd(i)
					lIsFirst  = .false.
				end if
			end if
		end do
		! Typically, first data items cloudiness remains unassigned
		if(ISNAN(rFcdOld)) then
			iRetCode = 1
			return
		end if

		! Locate first NaNs, and replace them with first over-threshold Cloudiness
		do i = 1, SIZE(rvRg)
			if(ISNAN(rvFcd(i))) then
				rvFcd(i) = rFcdFirst
			end if
		end do

	end function Cloudiness


	function SunRiseSunSet(yy, mo, dy, lat, lon, zone) result(sunRiseSet)

		implicit none

		! Routine arguments
		integer, intent(in)	:: yy, mo, dy
		real, intent(in)	:: lat, lon
		integer, intent(in)	:: zone
		real, dimension(2)	:: sunRiseSet

		! Locals
		integer	:: doy
		real	:: solarDeclination
		real	:: t, b, Sc
		real	:: centralMeridianLongitude
		real	:: localLongitude
		real	:: omegaZeroElev, tZeroElev1, tZeroElev2

		! Parameters
		real, parameter	:: PI = 3.1415927

		! Compute solar declination
		doy = J_DAY(yy,mo,dy)
		solarDeclination = 0.409*SIN(2*PI/365*doy - 1.39)

		! Calculate geographical positioning parameters (with a "-" sign for longitudes, according to ASCE conventions)
		centralMeridianLongitude = -zone*15.0
		if(centralMeridianLongitude < 0.0) then
			centralMeridianLongitude = centralMeridianLongitude + 360.0
		end if
		localLongitude = -lon
		if(localLongitude < 0.0) then
			localLongitude = localLongitude + 360.0
		end if

		! Calculate seasonal correction for solar time
		b  = 2.*PI*(doy-81)/364.0
		Sc = 0.1645*SIN(2.0*b) - 0.1255*COS(b) - 0.025*SIN(b)

		! Sunrise and sunset angles
		omegaZeroElev = ACOS(-TAN(lat*PI/180.0)*TAN(solarDeclination))
		tZeroElev1 =  omegaZeroElev * 12 / PI + 12.0 - Sc - 0.06667*(centralMeridianLongitude - localLongitude)
		if(tZeroElev1 < 0.) tZeroElev1 = tZeroElev1 + 12.0
		tZeroElev2 = -omegaZeroElev * 12 / PI + 12.0 - Sc - 0.06667*(centralMeridianLongitude - localLongitude)
		if(tZeroElev2 < 0.) tZeroElev2 = tZeroElev2 + 12.0
		sunRiseSet(1) = MIN(tZeroElev1, tZeroElev2)
		sunRiseSet(2) = MAX(tZeroElev1, tZeroElev2)

	end function SunRiseSunSet


	function SinSolarElevation(yy, mo, dy, hh, mm, ss, lat, lon, zone, averagingPeriod) result(sinBeta)

		implicit none

		! Routine arguments
		integer, intent(in)	:: yy, mo, dy, hh, mm, ss
		real, intent(in)	:: lat, lon
		integer, intent(in)	:: zone
		integer, intent(in)	:: averagingPeriod
		real				:: sinBeta

		! Locals
		integer	:: doy
		real	:: solarDeclination
		real	:: t, b, Sc
		real	:: centralMeridianLongitude
		real	:: localLongitude
		real	:: omega

		! Parameters
		real, parameter	:: PI = 3.1415927

		! Compute solar declination
		doy = J_DAY(yy,mo,dy)
		solarDeclination = 0.409*SIN(2*PI/365*doy - 1.39)

		! Compute current hour at mid of averaging period
		t = hh + mm/60.0 + ss/3600.0 + 0.5 * averagingPeriod / 3600.0

		! Calculate geographical positioning parameters (with a "-" sign for longitudes, according to ASCE conventions)
		centralMeridianLongitude = -zone*15.0
		if(centralMeridianLongitude < 0.0) then
			centralMeridianLongitude = centralMeridianLongitude + 360.0
		end if
		localLongitude = -lon
		if(localLongitude < 0.0) then
			localLongitude = localLongitude + 360.0
		end if

		! Calculate seasonal correction for solar time
		b  = 2.*PI*(doy-81)/364.0
		Sc = 0.1645*SIN(2.0*b) - 0.1255*COS(b) - 0.025*SIN(b)

		! Solar time angle at midpoint of averaging time
		omega = (PI/12.0) * ((t + 0.06667*(centralMeridianLongitude - localLongitude) + Sc) - 12.0)

		! Sine of solar elevation angle
		sinBeta = SIN(lat*PI/180.0)*SIN(solarDeclination) + COS(lat*PI/180.0)*COS(solarDeclination)*COS(omega)

	end function SinSolarElevation


	function SolarDeclination(yy, mo, dy) result(sunDecl)

		implicit none

		! Routine arguments
		integer, intent(in)	:: yy, mo, dy
		real				:: sunDecl

		! Locals
		integer	:: doy

		! Parameters
		real, parameter	:: PI = 3.1415927

		! Compute solar declination
		doy = J_DAY(yy,mo,dy)
		sunDecl = 0.409*SIN(2.*PI/365.*doy - 1.39)

	end function SolarDeclination

end module pbl_base
