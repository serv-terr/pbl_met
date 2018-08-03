! pbl_time - Fortran module, containing computations related to
!            Planetary Boundary Layer (PBL) quantities, encompassing
! date and time processing, and some astronomical formulae dealing
! with sunset/sunrise and apparent solar position.
!
! This module is part of the pbl_met library.
!
! This is open-source code, covered by the lGPL 3.0 license.
!
module pbl_time

	use pbl_base

	implicit none
	
	private
	
	! Public interface
	! 0. Constants
	public	:: DELTA_1_HOUR
	public	:: DELTA_8_HOURS
	public	:: DELTA_1_DAY
	! 1. Date and time management
	public	:: JulianDay					! Integer-valued Julian day
	public	:: UnpackDate					! Inverse of integer-valued Julian day
	public	:: DoW							! Day-of-week
	public	:: DoY							! Day-of-year, as in old PBL_MET "J_DAY" routine
	public	:: Leap							! Check a year is leap or not
	public	:: PackTime						! Date and time to epoch
	public	:: UnpackTime					! Epoch to date and time
	! 2. Basic astronomical computations
	public	:: calcJD						! Fractional Julian day, defined according to NOAA conventions
	public	:: calcTimeJulianCent			! Fractional Julian century, defined according to NOAA conventions
	public	:: SinSolarElevation			! Compute the sine of solar elevation angle
	public	:: SolarDeclination				! Compute the solar declination
	public	:: SunRiseSunSet				! Old PBL_MET evaluation of sun rise and sun set times (revised)
	! 3. Data types
	public	:: DateTime						! Class, supporting the new REAL(8) time stamps used in Time Series
	! 4. Support for time stamps as vectors
	public	:: timeEncode					! Compute aggregation indices for a time stamp vector
	public	:: timeLinearIndex				! Compute a linear aggregation index for a time stamp vector
	public	:: timeGetYear					! Extract year from a time stamp vector (may be used to obtain an index)
	public	:: timeGetMonth					! Extract month from a time stamp vector (may be used to obtain an index)
	public	:: timeGetYearMonth				! Extract a year-month value from a time stamp vector (may be used to obtain an index)
	
	! Constants
	integer, parameter	:: DELTA_1_HOUR  = 3600
	integer, parameter	:: DELTA_8_HOURS = DELTA_1_HOUR * 8
	integer, parameter	:: DELTA_1_DAY   = DELTA_1_HOUR * 24
	
	! Data types
	type DateTime
		integer	:: iYear
		integer	:: iMonth
		integer	:: iDay
		integer	:: iHour
		integer	:: iMinute
		real(8)	:: rSecond
	contains
		! Constructors
		procedure	:: fromEpoch	=> dtFromEpoch
		! Converters
		procedure	:: toEpoch		=> dtToEpoch
		! Printers
		procedure	:: toIso		=> dtToIso
	end type DateTime
	
	! Internal constants
	real(8), parameter	:: TIME_MIN =            0.d0
	real(8), parameter	:: TIME_MAX = 253402300800.d0
	
	! Polymorphic interfaces
	
	interface timeEncode
		module procedure	:: timeEncode1
		module procedure	:: timeEncode2
	end interface timeEncode
	
	
	interface timeLinearIndex
		module procedure	:: timeLinearIndex1
		module procedure	:: timeLinearIndex2
	end interface timeLinearIndex
	
	
	interface timeGetYear
		module procedure	:: timeGetYear1
		module procedure	:: timeGetYear2
	end interface timeGetYear
	
	
	interface timeGetMonth
		module procedure	:: timeGetMonth1
		module procedure	:: timeGetMonth2
	end interface timeGetMonth
	
	
	interface timeGetYearMonth
		module procedure	:: timeGetYearMonth1
		module procedure	:: timeGetYearMonth2
	end interface timeGetYearMonth
	
contains

	function JulianDay(iYear, iMonth, iDay) result(iJulianDay)

		! Routine arguments
        integer, intent(in) :: iYear
        integer, intent(in) :: iMonth
        integer, intent(in) :: iDay
        integer             :: iJulianDay

        ! Locals
        integer     		:: iAuxYear
        integer     		:: iAuxMonth
        integer     		:: iCentury
        integer     		:: iTryJulianDay
        integer     		:: iNumDays
        integer, parameter  :: DATE_REFORM_DAY = 588829 ! 15 October 1582, with 31-days months
        integer, parameter  :: BASE_DAYS       = 1720995

        ! Check year against invalid values. Only positive
        ! years are supported in this version. Year "0" does
        ! not exist.
        if(iYear <= 0) then
            iJulianDay = -9999
            return
        end if

        ! Check month and day to look valid (a rough, non-month-aware
        ! test is intentionally adopted in sake of simplicity)
        if((.not.(1<=iMonth .and. iMonth<=12)) .or. (.not.(1<=iDay .and. iDay<=31))) then
            iJulianDay = -9999
            return
        end if

        ! Preliminary estimate the Julian day, based on
        ! the average duration of year and month in days.
        if(iMonth > 2) then
            iAuxYear  = iYear
            iAuxMonth = iMonth + 1
        else
            iAuxYear  = iYear - 1
            iAuxMonth = iMonth + 13
        end if
        iTryJulianDay = floor(YEAR_DURATION * iAuxYear) + &
                        floor(MONTH_DURATION * iAuxMonth) + &
                        iDay + BASE_DAYS

        ! Correct estimate if later than the date reform day
        iNumDays = iDay + 31*iMonth + 372*iYear
        if(iNumDays >= DATE_REFORM_DAY) then
            iCentury = 0.01*iAuxYear
            iJulianDay = iTryJulianDay - iCentury + iCentury/4 + 2
        else
            iJulianDay = iTryJulianDay
        end if

	end function JulianDay


    subroutine UnpackDate(iJulianDay, iYear, iMonth, iDay)

        ! Routine arguments
        integer, intent(in)     :: iJulianDay
        integer, intent(out)    :: iYear
        integer, intent(out)    :: iMonth
        integer, intent(out)    :: iDay

        ! Locals
        integer :: iDeviation
        integer :: iPreJulianDay
        integer :: iPostJulianDay
        integer :: iYearIndex
        integer :: iMonthIndex
        integer :: iDayIndex
        integer, parameter  :: LIMIT_JULIAN_DAY = 2299161
        integer, parameter  :: CORRECTION_DAYS  = 1524

        ! Unwind Pope Gregorius' day correction
        if(iJulianDay >= LIMIT_JULIAN_DAY) then
            iDeviation = floor(((iJulianDay-1867216)-0.25)/36524.25)
            iPreJulianDay = iJulianDay + iDeviation - iDeviation/4 + 1
        else
            iPreJulianDay = iJulianDay
        end if
        iPostJulianDay = iPreJulianDay + CORRECTION_DAYS

        ! Compute time indices
        iYearIndex  = floor(6680+((iPostJulianDay-2439870)-122.1)/YEAR_DURATION)
        iDayIndex   = 365*iYearIndex + iYearIndex/4
        iMonthIndex = floor((iPostJulianDay - iDayIndex)/MONTH_DURATION)

        ! Deduce preliminary date from time indices
        iDay = iPostJulianDay - floor(MONTH_DURATION*iMonthIndex) - iDayIndex
        if(iMonthIndex > 13) then
            iMonth = iMonthIndex - 13
        else
            iMonth = iMonthIndex - 1
        end if
        iYear = iYearIndex - 4715
        if(iMonth > 2) iYear = iYear - 1

    end subroutine UnpackDate


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
	
	
    function DoW(iJulianDay) result(iDayOfWeek)

        ! Routine arguments
        integer, intent(in) :: iJulianDay
        integer         	:: iDayOfWeek

        ! Locals
        ! -none-

        ! Compute the desired quantity
        iDayOfWeek = mod(iJulianDay, 7)

    end function DoW


	! Day of year
	function DoY(ia,im,id) result(iDayOfYear)

		! Routine arguments
		integer, intent(in)	:: ia			! Year (with century)
		integer, intent(in)	:: im			! Month
		integer, intent(in)	:: id			! Day
		integer				:: iDayOfYear	! Day in year
		
		! Locals
		! --none--
		
		! Parameters
		integer, dimension(13,2), parameter	:: ngm = reshape( &
			[0,31,60,91,121,152,182,213,244,274,305,335,366, &
			 0,31,59,90,120,151,181,212,243,273,304,334,365], [13,2])

		if(Leap(ia)) then
			! Leap year
			iDayOfYear = id+ngm(im,1)
		else
			! Even year
			iDayOfYear = id+ngm(im,2)
		end if

	end function DoY


    subroutine PackTime(iTime, iYear, iMonth, iDay, iInHour, iInMinute, iInSecond)

        ! Routine arguments
        integer, intent(out)            :: iTime
        integer, intent(in)             :: iYear
        integer, intent(in)             :: iMonth
        integer, intent(in)             :: iDay
        integer, intent(in), optional   :: iInHour
        integer, intent(in), optional   :: iInMinute
        integer, intent(in), optional   :: iInSecond

        ! Locals
        integer :: iHour
        integer :: iMinute
        integer :: iSecond
        integer :: iJulianDay
        integer :: iJulianSecond

        ! Check for optional parameters; assign defaults if necessary
        if(present(iInHour)) then
            iHour = iInHour
        else
            iHour = 0
        end if
        if(present(iInMinute)) then
            iMinute = iInMinute
        else
            iMinute = 0
        end if
        if(present(iInSecond)) then
            iSecond = iInSecond
        else
            iSecond = 0
        end if
        
        ! Check input parameters for validity
        if( &
            iYear   <= 0 .OR. &
            iMonth  < 1 .OR. iMonth  > 12 .OR. &
            iDay    < 1 .OR. iDay    > 31 .OR. &
            iHour   < 0 .OR. iHour   > 23 .OR. &
            iMinute < 0 .OR. iMinute > 59 .OR. &
            iSecond < 0 .OR. iSecond > 59 &
        ) then
            iTime = -1
            return
        end if

        ! Compute based Julian day
        iJulianDay = JulianDay(iYear, iMonth, iDay) - BASE_DAY

        ! Convert based Julian day to second, and add seconds from time,
        ! regardless of hour type.
        iJulianSecond = iJulianDay * 24 * 3600
        iTime = iJulianSecond + iSecond + 60*(iMinute + 60*iHour)

    end subroutine PackTime


    subroutine UnpackTime(iTime, iYear, iMonth, iDay, iHour, iMinute, iSecond)

        ! Routine arguments
        integer, intent(in)     :: iTime
        integer, intent(out)    :: iYear
        integer, intent(out)    :: iMonth
        integer, intent(out)    :: iDay
        integer, intent(out)    :: iHour
        integer, intent(out)    :: iMinute
        integer, intent(out)    :: iSecond

        ! Locals
        integer :: iJulianDay
        integer :: iTimeSeconds

        ! Check parameter
        if(iTime < 0) then
            iYear   = 1970
            iMonth  = 1
            iDay    = 1
            iHour   = 0
            iMinute = 0
            iSecond = 0
            return
        end if

        ! Isolate the date and time parts
        iJulianDay = iTime/(24*3600) + BASE_DAY
        iTimeSeconds = mod(iTime, 24*3600)

        ! Process the date part
        call UnpackDate(iJulianDay, iYear, iMonth, iDay)

        ! Extract time from the time part
        iSecond = mod(iTimeSeconds,60)
        iTimeSeconds = iTimeSeconds/60
        iMinute = mod(iTimeSeconds,60)
        iHour   = iTimeSeconds/60

    end subroutine UnpackTime


	! Fractional Julian day, according to NOAA conventions
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


	function SunRiseSunSet(yy, mo, dy, lat, lon, zone) result(sunRiseSet)

		implicit none

		! Routine arguments
		integer, intent(in)	:: yy, mo, dy
		real, intent(in)	:: lat, lon
		integer, intent(in)	:: zone
		real, dimension(2)	:: sunRiseSet

		! Locals
		integer	:: iDayOfYear
		real	:: solarDeclination
		real	:: t, b, Sc
		real	:: centralMeridianLongitude
		real	:: localLongitude
		real	:: omegaZeroElev, tZeroElev1, tZeroElev2

		! Parameters
		real, parameter	:: PI = 3.1415927

		! Compute solar declination
		iDayOfYear = DoY(yy,mo,dy)
		solarDeclination = 0.409*SIN(2*PI/365*iDayOfYear - 1.39)

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
		b  = 2.*PI*(iDayOfYear-81)/364.0
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
		integer	:: iDayOfYear
		real	:: solarDeclination
		real	:: t, b, Sc
		real	:: centralMeridianLongitude
		real	:: localLongitude
		real	:: omega

		! Parameters
		real, parameter	:: PI = 3.1415927

		! Compute solar declination
		iDayOfYear = DoY(yy,mo,dy)
		solarDeclination = 0.409*SIN(2*PI/365*iDayOfYear - 1.39)

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
		b  = 2.*PI*(iDayOfYear-81)/364.0
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
		integer	:: iDayOfYear

		! Parameters
		real, parameter	:: PI = 3.1415927

		! Compute solar declination
		iDayOfYear = DoY(yy,mo,dy)
		sunDecl = 0.409*SIN(2.*PI/365.*iDayOfYear - 1.39)

	end function SolarDeclination
		
	! **************************************
	! * Member functions of DateTime class *
	! **************************************

    subroutine UnpackDate8(iJulianDay, iYear, iMonth, iDay)

        ! Routine arguments
        integer(8), intent(in)     :: iJulianDay
        integer, intent(out)       :: iYear
        integer, intent(out)       :: iMonth
        integer, intent(out)       :: iDay

        ! Locals
        integer(8) :: iDeviation
        integer(8) :: iPreJulianDay
        integer(8) :: iPostJulianDay
        integer(8) :: iYearIndex
        integer(8) :: iMonthIndex
        integer(8) :: iDayIndex
        integer(8), parameter  :: LIMIT_JULIAN_DAY = 2299161
        integer(8), parameter  :: CORRECTION_DAYS  = 1524

        ! Unwind Pope Gregorius' day correction
        if(iJulianDay >= LIMIT_JULIAN_DAY) then
            iDeviation = floor(((iJulianDay-1867216_8)-0.25d0)/36524.25d0, kind=8)
            iPreJulianDay = iJulianDay + iDeviation - iDeviation/4_8 + 1_8
        else
            iPreJulianDay = iJulianDay
        end if
        iPostJulianDay = iPreJulianDay + CORRECTION_DAYS

        ! Compute time indices
        iYearIndex  = floor(6680_8+((iPostJulianDay-2439870_8)-122.1d0)/YEAR_DURATION, kind=8)
        iDayIndex   = 365_8*iYearIndex + iYearIndex/4_8
        iMonthIndex = floor((iPostJulianDay - iDayIndex)/MONTH_DURATION, kind=8)

        ! Deduce preliminary date from time indices
        iDay = iPostJulianDay - floor(MONTH_DURATION*iMonthIndex, kind=8	) - iDayIndex
        if(iMonthIndex > 13_8) then
            iMonth = iMonthIndex - 13_8
        else
            iMonth = iMonthIndex - 1_8
        end if
        iYear = iYearIndex - 4715_8
        if(iMonth > 2) iYear = iYear - 1

    end subroutine UnpackDate8


    function dtFromEpoch(this, rEpoch) result(iRetCode)

        ! Routine arguments
        class(DateTime), intent(out)	:: this
        real(8), intent(in)				:: rEpoch
        integer							:: iRetCode

        ! Locals
        integer(8)	:: iJulianDay
        real(8) 	:: rTimeSeconds
        
        ! Assume success (will falsify on failure)
        iRetCode = 0

        ! Check parameter
        if(rEpoch < TIME_MIN .or. rEpoch >= TIME_MAX) then
			iRetCode = 1
			this % iYear   = 1970
			this % iMonth  = 1
			this % iDay    = 1
			this % iHour   = 0
			this % iMinute = 0
			this % rSecond = 0.d0
            return
        elseif(.invalid.rEpoch) then
			iRetCode = 1
			this % iYear   = 1970
			this % iMonth  = 1
			this % iDay    = 1
			this % iHour   = 0
			this % iMinute = 0
			this % rSecond = 0.d0
            return
        end if

        ! Isolate the date and time parts
        iJulianDay = floor(rEpoch/(24.d0*3600.d0), kind=8) + BASE_DAY_8
        rTimeSeconds = mod(rEpoch, 24.d0*3600.d0)

        ! Process the date part
        call UnpackDate8(iJulianDay, this % iYear, this % iMonth, this % iDay)

        ! Extract time from the time part
        this % rSecond = mod(rTimeSeconds,60.d0)
        rTimeSeconds = rTimeSeconds/60.d0
        this % iMinute = mod(floor(rTimeSeconds, kind=8),60_8)
        this % iHour   = floor(rTimeSeconds, kind=8)/60_8

    end function dtFromEpoch
    

    function dtToEpoch(this) result(rEpoch)

        ! Routine arguments
        class(DateTime), intent(in)		:: this
        real(8)							:: rEpoch

        ! Locals
        integer    :: iJulianDay
        integer(8) :: iJulianSecond
        
        ! Check input parameters for validity
        if( &
            this % iYear   <= 0   .or. &
            this % iMonth  < 1    .or. this % iMonth  >  12 .OR. &
            this % iDay    < 1    .or. this % iDay    >  31 .OR. &
            this % iHour   < 0    .or. this % iHour   >  23 .OR. &
            this % iMinute < 0    .or. this % iMinute >  59 .OR. &
            this % rSecond < 0.d0 .or. this % rSecond >= 60.d0 &
        ) then
            rEpoch = NaN_8
            return
        end if

        ! Compute based Julian day
        iJulianDay = JulianDay(this % iYear, this % iMonth, this % iDay) - BASE_DAY

        ! Convert based Julian day to second, and add seconds from time,
        ! regardless of hour type.
        iJulianSecond = iJulianDay * 24_8 * 3600_8
        rEpoch = iJulianSecond + this % rSecond + 60_4*(this % iMinute + 60_4*this % iHour)

    end function dtToEpoch

    
    function dtToIso(this) result(sDateTime)
    
		! Routine arguments
		class(DateTime), intent(in)	:: this
		character(len=23)			:: sDateTime
		
		! Locals
		integer	:: iSecond
		real(8)	:: rSubSecond
		
		! Write the data desired
		iSecond = floor(this % rSecond)
		rSubSecond = this % rSecond - iSecond
		write(sDateTime, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2),f4.3)") &
			this % iYear, &
			this % iMonth, &
			this % iDay, &
			this % iHour, &
			this % iMinute, &
			iSecond, rSubSecond
			
    end function dtToIso

	! *********************
	! * Internal routines *
	! *********************
	
	function timeEncode1(rvTimeStamp, iPeriodLength, iStepSize, ivTimeCode) result(iRetCode)
	
		! Routine arguments
		real(8), intent(in), dimension(:)				:: rvTimeStamp
		integer, intent(in)								:: iPeriodLength
		integer, intent(in)								:: iStepSize
		integer, intent(out), dimension(:), allocatable	:: ivTimeCode
		integer											:: iRetCode
		
		! Locals
		integer		:: n
		integer		:: i
		integer(8)	:: iTimeStamp
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Reserve workspace
		if(allocated(ivTimeCode)) deallocate(ivTimeCode)
		n = size(rvTimeStamp)
		if(n <= 0) then
			iRetCode = 1
			return
		end if
		allocate(ivTimeCode(n))
		
		! Iterate over time stamps, and assign codes
		do i = 1, n
			if(.valid.rvTimeStamp(i)) then
				iTimeStamp = floor(rvTimeStamp(i), kind=8)
				ivTimeCode(i) = mod(iTimeStamp, iPeriodLength) / iStepSize + 1
			else
				ivTimeCode(i) = 0	! Special "invalid" code
			end if
		end do
		
	end function timeEncode1

	
	function timeEncode2(ivTimeStamp, iPeriodLength, iStepSize, ivTimeCode) result(iRetCode)
	
		! Routine arguments
		integer, intent(in), dimension(:)				:: ivTimeStamp
		integer, intent(in)								:: iPeriodLength
		integer, intent(in)								:: iStepSize
		integer, intent(out), dimension(:), allocatable	:: ivTimeCode
		integer											:: iRetCode
		
		! Locals
		integer		:: n
		integer		:: i
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Reserve workspace
		if(allocated(ivTimeCode)) deallocate(ivTimeCode)
		n = size(ivTimeStamp)
		if(n <= 0) then
			iRetCode = 1
			return
		end if
		allocate(ivTimeCode(n))
		
		! Iterate over time stamps, and assign codes
		do i = 1, n
			if(ivTimeStamp(i) > 0) then
				ivTimeCode(i) = mod(ivTimeStamp(i), iPeriodLength) / iStepSize + 1
			else
				ivTimeCode(i) = 0	! Special "invalid" code
			end if
		end do
		
	end function timeEncode2

	
	function timeLinearIndex1(rvTimeStamp, iStepSize, ivTimeCode, rvBaseTimeStamp) result(iRetCode)
	
		! Routine arguments
		real(8), intent(in), dimension(:)							:: rvTimeStamp
		integer, intent(in)											:: iStepSize
		integer, intent(out), dimension(:), allocatable				:: ivTimeCode
		real(8), intent(out), dimension(:), allocatable, optional	:: rvBaseTimeStamp
		integer														:: iRetCode
		
		! Locals
		integer		:: n
		integer		:: i
		integer(8)	:: iTimeStamp
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Reserve workspace
		if(allocated(ivTimeCode)) deallocate(ivTimeCode)
		n = size(rvTimeStamp)
		if(n <= 0) then
			iRetCode = 1
			return
		end if
		allocate(ivTimeCode(n))
		if(present(rvBaseTimeStamp)) then
			if(allocated(rvBaseTimeStamp)) deallocate(rvBaseTimeStamp)
			allocate(rvBaseTimeStamp(n))
		end if
		
		! Iterate over time stamps, and assign codes
		do i = 1, n
			if(.valid.rvTimeStamp(i)) then
				iTimeStamp = floor(rvTimeStamp(i), kind=8)
				ivTimeCode(i) = iTimeStamp / iStepSize + 1
			else
				ivTimeCode(i) = 0	! Special "invalid" code
			end if
		end do
		if(present(rvBaseTimeStamp)) then
			where(ivTimeCode > 0)
				rvBaseTimeStamp = int(ivTimeCode-1, kind=8) * iStepSize
			elsewhere
				rvBaseTimeStamp = NaN_8
			endwhere
		end if
		where(ivTimeCode > 0)
			ivTimeCode = ivTimeCode - minval(ivTimeCode, mask=ivTimeCode > 0) + 1
		end where
		
	end function timeLinearIndex1

	
	function timeLinearIndex2(ivTimeStamp, iStepSize, ivTimeCode, ivBaseTimeStamp) result(iRetCode)
	
		! Routine arguments
		integer, intent(in), dimension(:)							:: ivTimeStamp
		integer, intent(in)											:: iStepSize
		integer, intent(out), dimension(:), allocatable				:: ivTimeCode
		integer, intent(out), dimension(:), allocatable, optional	:: ivBaseTimeStamp
		integer														:: iRetCode
		
		! Locals
		integer		:: n
		integer		:: i
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Reserve workspace
		if(allocated(ivTimeCode)) deallocate(ivTimeCode)
		n = size(ivTimeStamp)
		if(n <= 0) then
			iRetCode = 1
			return
		end if
		allocate(ivTimeCode(n))
		if(present(ivBaseTimeStamp)) then
			if(allocated(ivBaseTimeStamp)) deallocate(ivBaseTimeStamp)
			allocate(ivBaseTimeStamp(n))
		end if
		
		do i = 1, n
			if(ivTimeStamp(i) > 0) then
				ivTimeCode(i) = ivTimeStamp(i) / iStepSize + 1
			else
				ivTimeCode(i) = 0	! Special "invalid" code
			end if
		end do
		if(present(ivBaseTimeStamp)) then
			where(ivTimeCode > 0)
				ivBaseTimeStamp = (ivTimeCode-1) * iStepSize
			elsewhere
				ivBaseTimeStamp = 0
			endwhere
		end if
		where(ivTimeCode > 0)
			ivTimeCode = ivTimeCode - minval(ivTimeCode) + 1
		end where
		
	end function timeLinearIndex2

	
	function timeGetYear1(rvTimeStamp, ivYear) result(iRetCode)
	
		! Routine arguments
		real(8), intent(in), dimension(:)				:: rvTimeStamp
		integer, intent(out), dimension(:), allocatable	:: ivYear
		integer											:: iRetCode
		
		! Locals
		integer			:: n
		integer			:: i
		type(DateTime)	:: dt
		integer			:: iErrCode
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Reserve workspace
		if(allocated(ivYear)) deallocate(ivYear)
		n = size(rvTimeStamp)
		if(n <= 0) then
			iRetCode = 1
			return
		end if
		allocate(ivYear(n))
		
		! Iterate over time stamps, and assign codes
		do i = 1, n
			if(.valid.rvTimeStamp(i)) then
				iErrCode = dt % fromEpoch(rvTimeStamp(i))
				if(iErrCode == 0) then
					ivYear(i) = dt % iYear
				else
					ivYear(i) = -9999	! Special "invalid" code
				end if
			else
				ivYear(i) = -9999	! Special "invalid" code
			end if
		end do
		
	end function timeGetYear1

	
	function timeGetYear2(ivTimeStamp, ivYear) result(iRetCode)
	
		! Routine arguments
		integer, intent(in), dimension(:)				:: ivTimeStamp
		integer, intent(out), dimension(:), allocatable	:: ivYear
		integer											:: iRetCode
		
		! Locals
		integer		:: n
		integer		:: i
		integer		:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Reserve workspace
		if(allocated(ivYear)) deallocate(ivYear)
		n = size(ivTimeStamp)
		if(n <= 0) then
			iRetCode = 1
			return
		end if
		allocate(ivYear(n))
		
		! Iterate over time stamps, and assign codes
		do i = 1, n
			if(ivTimeStamp(i) > 0) then
				call UnpackTime(ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
				ivYear(i) = iYear
			else
				ivYear(i) = -9999	! Special "invalid" code
			end if
		end do
		
	end function timeGetYear2

	
	function timeGetMonth1(rvTimeStamp, ivMonth) result(iRetCode)
	
		! Routine arguments
		real(8), intent(in), dimension(:)				:: rvTimeStamp
		integer, intent(out), dimension(:), allocatable	:: ivMonth
		integer											:: iRetCode
		
		! Locals
		integer			:: n
		integer			:: i
		type(DateTime)	:: dt
		integer			:: iErrCode
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Reserve workspace
		if(allocated(ivMonth)) deallocate(ivMonth)
		n = size(rvTimeStamp)
		if(n <= 0) then
			iRetCode = 1
			return
		end if
		allocate(ivMonth(n))
		
		! Iterate over time stamps, and assign codes
		do i = 1, n
			if(.valid.rvTimeStamp(i)) then
				iErrCode = dt % fromEpoch(rvTimeStamp(i))
				if(iErrCode == 0) then
					ivMonth(i) = dt % iMonth
				else
					ivMonth(i) = -9999	! Special "invalid" code
				end if
			else
				ivMonth(i) = -9999	! Special "invalid" code
			end if
		end do
		
	end function timeGetMonth1

	
	function timeGetMonth2(ivTimeStamp, ivMonth) result(iRetCode)
	
		! Routine arguments
		integer, intent(in), dimension(:)				:: ivTimeStamp
		integer, intent(out), dimension(:), allocatable	:: ivMonth
		integer											:: iRetCode
		
		! Locals
		integer		:: n
		integer		:: i
		integer		:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Reserve workspace
		if(allocated(ivMonth)) deallocate(ivMonth)
		n = size(ivTimeStamp)
		if(n <= 0) then
			iRetCode = 1
			return
		end if
		allocate(ivMonth(n))
		
		! Iterate over time stamps, and assign codes
		do i = 1, n
			if(ivTimeStamp(i) > 0) then
				call UnpackTime(ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
				ivMonth(i) = iMonth
			else
				ivMonth(i) = -9999	! Special "invalid" code
			end if
		end do
		
	end function timeGetMonth2

	
	function timeGetYearMonth1(rvTimeStamp, ivYearMonth) result(iRetCode)
	
		! Routine arguments
		real(8), intent(in), dimension(:)				:: rvTimeStamp
		integer, intent(out), dimension(:), allocatable	:: ivYearMonth
		integer											:: iRetCode
		
		! Locals
		integer			:: n
		integer			:: i
		type(DateTime)	:: dt
		integer			:: iErrCode
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Reserve workspace
		if(allocated(ivYearMonth)) deallocate(ivYearMonth)
		n = size(rvTimeStamp)
		if(n <= 0) then
			iRetCode = 1
			return
		end if
		allocate(ivYearMonth(n))
		
		! Iterate over time stamps, and assign codes
		do i = 1, n
			if(.valid.rvTimeStamp(i)) then
				iErrCode = dt % fromEpoch(rvTimeStamp(i))
				if(iErrCode == 0) then
					ivYearMonth(i) = 12 * dt % iYear + dt % iMonth - 1
				else
					ivYearMonth(i) = -9999	! Special "invalid" code
				end if
			else
				ivYearMonth(i) = -9999	! Special "invalid" code
			end if
		end do
		
	end function timeGetYearMonth1

	
	function timeGetYearMonth2(ivTimeStamp, ivYearMonth) result(iRetCode)
	
		! Routine arguments
		integer, intent(in), dimension(:)				:: ivTimeStamp
		integer, intent(out), dimension(:), allocatable	:: ivYearMonth
		integer											:: iRetCode
		
		! Locals
		integer		:: n
		integer		:: i
		integer		:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Reserve workspace
		if(allocated(ivYearMonth)) deallocate(ivYearMonth)
		n = size(ivTimeStamp)
		if(n <= 0) then
			iRetCode = 1
			return
		end if
		allocate(ivYearMonth(n))
		
		! Iterate over time stamps, and assign codes
		do i = 1, n
			if(ivTimeStamp(i) > 0) then
				call UnpackTime(ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
				ivYearMonth(i) = 12 * iYear + iMonth - 1
			else
				ivYearMonth(i) = -9999	! Special "invalid" code
			end if
		end do
		
	end function timeGetYearMonth2

end module pbl_time
