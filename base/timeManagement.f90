!------------------------------------------------------------------
!
! TimeManagement
!
! Module, supporting the treatment of date and time information, up to
! the resolution of 1s; routines include the astronomical computations
! necessary to find apparent Sun position, used in micro-meteorological
! applications.
!
! Copyright (C) 2015 by Servizi Territorio srl
!
! Written by: Mauri Favaron, based on original code from R. Sozzi and D. Fraternali
! e-mail:     mafavaron@mac.com
!
! Licensing: This module is distribute under the LGPL V3.0 license.
!
!------------------------------------------------------------------

MODULE TimeManagement

    IMPLICIT NONE

    PRIVATE

    ! Public interface
    ! -1- Geographic position
    PUBLIC  :: GeoPoint
    PUBLIC  :: OPERATOR(.VALIDSITE.)
    ! -1- DateTime data type and related functions
    PUBLIC  :: DateTime
    PUBLIC  :: OPERATOR(.VALIDTIME.)
    PUBLIC  :: Year
    PUBLIC  :: Month
    PUBLIC  :: Day
    PUBLIC  :: Hour
    PUBLIC  :: Minute
    PUBLIC  :: TimeShift
    PUBLIC  :: ToEpoch
    PUBLIC  :: ToDateTime
    PUBLIC  :: DayInYear
    PUBLIC  :: ToString
    ! -1- Non-refractive solar position
    PUBLIC  :: SolarDeclination
    PUBLIC  :: SolarElevationAngleSine
    PUBLIC  :: SunriseSunset
    ! -1- Constants
    INTEGER, PARAMETER  :: HOUR_00_23 = 0
    PUBLIC              :: HOUR_00_23
    INTEGER, PARAMETER  :: HOUR_01_24 = 1
    PUBLIC              :: HOUR_01_24

    ! Data types
    
    TYPE GeoPoint
        REAL    :: lon      ! Longitude angle (positive for West, negative for East)
        REAL    :: lat      ! Latitude (positive for North, negative for South)
        REAL    :: fuse     ! Local solar hour deviation from UTC (+1 for CET, ...)
    END TYPE GeoPoint

    TYPE DateTime
        INTEGER(2)  :: year
        INTEGER(2)  :: month
        INTEGER(2)  :: day
        INTEGER(2)  :: hour
        INTEGER(2)  :: minute
        INTEGER(2)  :: second
    END TYPE DateTime
    
    ! Public procedures

    INTERFACE OPERATOR(.VALIDTIME.)
        MODULE PROCEDURE IsValid
    END INTERFACE OPERATOR(.VALIDTIME.)

    INTERFACE OPERATOR(.VALIDSITE.)
        MODULE PROCEDURE IsValidSite
    END INTERFACE OPERATOR(.VALIDSITE.)

    INTERFACE Year
        MODULE PROCEDURE YearD, YearE
    END INTERFACE Year

    INTERFACE Month
        MODULE PROCEDURE MonthD, MonthE
    END INTERFACE Month

    INTERFACE Day
        MODULE PROCEDURE DayD, DayE
    END INTERFACE Day

    INTERFACE Hour
        MODULE PROCEDURE HourD, HourE
    END INTERFACE Hour

    INTERFACE Minute
        MODULE PROCEDURE MinuteD, MinuteE
    END INTERFACE Minute

    INTERFACE DayInYear
        MODULE PROCEDURE DayInYearD, DayInYearE
    END INTERFACE DayInYear

    INTERFACE TimeShift
        MODULE PROCEDURE TimeShiftD, TimeShiftE
    END INTERFACE TimeShift
    
    INTERFACE ToString
        MODULE PROCEDURE ToStringD, ToStringE
    END INTERFACE ToString

    INTERFACE SolarDeclination
        MODULE PROCEDURE SolarDeclinationD, SolarDeclinationE
    END INTERFACE SolarDeclination

    INTERFACE SolarElevationAngleSine
        MODULE PROCEDURE SolarElevationAngleSineD, SolarElevationAngleSineE
    END INTERFACE SolarElevationAngleSine

    INTERFACE SunriseSunset
        MODULE PROCEDURE SunriseSunsetD, SunriseSunsetE
    END INTERFACE SunriseSunset

    ! Internal parameters
    REAL, PARAMETER     :: YEAR_DURATION   = 365.25
    REAL, PARAMETER     :: MONTH_DURATION  = 30.6001
    INTEGER, PARAMETER  :: BASE_DAY        = 2440588    ! 01. 01. 1970
    REAL, PARAMETER     :: PI              = 3.14159265
    
CONTAINS

    FUNCTION IsValid(tm) RESULT(valid)

        ! Routine arguments
        TYPE(DateTime), INTENT(IN)  :: tm
        LOGICAL                     :: valid

        ! Locals
        LOGICAL :: isLeap
        INTEGER :: i

        ! Internal parameters
        INTEGER(2), DIMENSION(12), PARAMETER    :: DaysInMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        
        ! Check year to occur not earlier than the Epoch
        IF (tm%year < 1970) THEN
            valid = .FALSE.
            RETURN
        END IF
        
        ! Check month to make sense
        IF (tm % month < 1 .OR. tm % month > 12) THEN
            valid = .FALSE.
            RETURN
        ENDIF

        ! Check day
        IF (tm % month /= 2) THEN

            ! All months except February: check day is in fixed range
            IF (tm % day < 1 .OR. tm % day > DaysInMonth(tm % month)) THEN
                valid = .FALSE.
                RETURN
            ENDIF

        ELSE

            ! February: decide whether year is leap or not, and use the upper limit accordingly
            IF (MOD(tm % year, 4) == 0) THEN
                ! Decide whether this year is leap or regular
                IF (MOD(tm % year, 100) == 0) THEN
                    IF (MOD(tm % year, 400) == 0) THEN
                        isLeap = .TRUE.
                    ELSE
                        isLeap = .FALSE.
                    ENDIF
                ELSE
                    isLeap = .TRUE.
                ENDIF
            ELSE
                isLeap = .FALSE.
            ENDIF

            ! Check correct number of days based on year leapness
            IF (isLeap) THEN
                IF (tm % day < 1 .OR. tm % day > 29) THEN
                    valid = .FALSE.
                    RETURN
                ENDIF
            ELSE
                IF (tm % day < 1 .OR. tm % day > 28) THEN
                    valid = .FALSE.
                    RETURN
                ENDIF
            ENDIF

        ENDIF
        ! Post-condition: if execution arrives here no invaidity reason has been found in date part;
        !                 something not nice may occur with time however: see next code lines

        ! Check hour, minute and second
        valid = tm % hour >= 0 .AND. tm % hour <= 23 .AND. &
                tm % minute >= 0 .AND. tm % minute <= 59 .AND. &
                tm % second >= 0 .AND. tm % second <= 59

    END FUNCTION IsValid


    FUNCTION IsValidSite(pt) RESULT(valid)

        ! Routine arguments
        TYPE(GeoPoint), INTENT(IN)  :: pt
        LOGICAL                     :: valid
        
        ! Locals
        ! -none-
        
        ! Check site data validity
        valid = .TRUE.
        IF(pt % lat < -85. .OR. pt % lat > +85.) THEN
            valid = .FALSE.
        ELSEIF(pt % lon <= -180. .OR. pt % lon > 180.) THEN
            valid = .FALSE.
        ELSEIF(pt % fuse < -12. .OR. pt % fuse > 12.) THEN
            valid = .FALSE.
        END IF

    END FUNCTION IsValidSite
    

    FUNCTION YearD(d) RESULT(e)

        ! Routine arguments
        TYPE(DateTime), INTENT(IN)  :: d
        TYPE(DateTime)              :: e

        ! Locals
        ! -none-

        ! Form a new DateTime with original's year part
        e % year   = d % year
        e % month  = 1
        e % day    = 1
        e % hour   = 0
        e % minute = 0
        e % second = 0

    END FUNCTION YearD


    FUNCTION YearE(d) RESULT(e)

        ! Routine arguments
        INTEGER, INTENT(IN) :: d
        INTEGER             :: e

        ! Locals
        TYPE(DateTime)  :: tm

        ! Transform epoch value to DateTime, get base year and change back to epoch
        tm = ToDateTime(d)
        tm % month  = 1
        tm % day    = 1
        tm % hour   = 0
        tm % minute = 0
        tm % second = 0
        e = ToEpoch(tm)

    END FUNCTION YearE


    FUNCTION MonthD(d) RESULT(e)

        ! Routine arguments
        TYPE(DateTime), INTENT(IN)  :: d
        TYPE(DateTime)              :: e

        ! Locals
        ! -none-

        ! Form a new DateTime with original's month part
        e % year   = d % year
        e % month  = d % month
        e % day    = 1
        e % hour   = 0
        e % minute = 0
        e % second = 0

    END FUNCTION MonthD


    FUNCTION MonthE(d) RESULT(e)

        ! Routine arguments
        INTEGER, INTENT(IN) :: d
        INTEGER             :: e

        ! Locals
        TYPE(DateTime)  :: tm

        ! Transform epoch value to DateTime, get base month and change back to epoch
        tm = ToDateTime(d)
        tm % day    = 1
        tm % hour   = 0
        tm % minute = 0
        tm % second = 0
        e = ToEpoch(tm)

    END FUNCTION MonthE


    FUNCTION DayD(d) RESULT(e)

        ! Routine arguments
        TYPE(DateTime), INTENT(IN)  :: d
        TYPE(DateTime)              :: e

        ! Locals
        ! -none-

        ! Form a new DateTime with original's day part
        e % year   = d % year
        e % month  = d % month
        e % day    = d % day
        e % hour   = 0
        e % minute = 0
        e % second = 0

    END FUNCTION DayD


    FUNCTION DayE(d) RESULT(e)

        ! Routine arguments
        INTEGER, INTENT(IN) :: d
        INTEGER             :: e

        ! Locals
        TYPE(DateTime)  :: tm

        ! Transform epoch value to DateTime, get base day and change back to epoch
        tm = ToDateTime(d)
        tm % hour   = 0
        tm % minute = 0
        tm % second = 0
        e = ToEpoch(tm)

    END FUNCTION DayE


    FUNCTION HourD(d) RESULT(e)

        ! Routine arguments
        TYPE(DateTime), INTENT(IN)  :: d
        TYPE(DateTime)              :: e

        ! Locals
        ! -none-

        ! Form a new DateTime with original's hour part
        e % year   = d % year
        e % month  = d % month
        e % day    = d % day
        e % hour   = d % hour
        e % minute = 0
        e % second = 0

    END FUNCTION HourD


    FUNCTION HourE(d) RESULT(e)

        ! Routine arguments
        INTEGER, INTENT(IN) :: d
        INTEGER             :: e

        ! Locals
        INTEGER :: iTemporary

        ! Reduce to desired hour
        iTemporary = d / 3600
        e = iTemporary * 3600

    END FUNCTION HourE


    FUNCTION MinuteD(d) RESULT(e)

        ! Routine arguments
        TYPE(DateTime), INTENT(IN)  :: d
        TYPE(DateTime)              :: e

        ! Locals
        ! -none-

        ! Form a new DateTime with original's minute part
        e % year   = d % year
        e % month  = d % month
        e % day    = d % day
        e % hour   = d % hour
        e % minute = d % minute
        e % second = 0

    END FUNCTION MinuteD


    FUNCTION MinuteE(d) RESULT(e)

        ! Routine arguments
        INTEGER, INTENT(IN) :: d
        INTEGER             :: e

        ! Locals
        INTEGER :: iTemporary

        ! Reduce to desired minute
        iTemporary = d / 60
        e = iTemporary * 60

    END FUNCTION MinuteE
    
    
    ! Shift date/time values by the specified number of seconds;
    ! warning: in case of negative value of 'iSeconds', the resulting
    ! epoch or DateTime value may make no sense.
    
    FUNCTION TimeShiftD(d, iSeconds) RESULT(e)
    
        ! Routine arguments
        TYPE(DateTime), INTENT(IN)  :: d
        INTEGER, INTENT(IN)         :: iSeconds
        TYPE(DateTime)              :: e
        
        ! Locals
        INTEGER :: iEpochD
        
        ! Compute the desired value
        iEpochD = ToEpoch(d)
        e       = ToDateTime(iEpochD + iSeconds)
        
    END FUNCTION TimeShiftD


    FUNCTION TimeShiftE(d, iSeconds) RESULT(e)
    
        ! Routine arguments
        INTEGER, INTENT(IN) :: d
        INTEGER, INTENT(IN) :: iSeconds
        INTEGER             :: e
        
        ! Locals
        ! -none-
        
        ! Compute the desired value
        e = d + iSeconds
        
    END FUNCTION TimeShiftE


    FUNCTION ToStringD(d) RESULT(s)
    
        ! Routine arguments
        TYPE(DateTime), INTENT(IN)  :: d
        CHARACTER(LEN=19)           :: s
        
        ! Locals
        ! -none-
        
        ! Compute the desired value
        WRITE(s, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))") &
            d % year, &
            d % month, &
            d % day, &
            d % hour, &
            d % minute, &
            d % second
        
    END FUNCTION ToStringD


    FUNCTION ToStringE(e) RESULT(s)
    
        ! Routine arguments
        INTEGER, INTENT(IN) :: e
        CHARACTER(LEN=19)   :: s
        
        ! Locals
        TYPE(DateTime)  :: d
        
        ! Compute the desired value
        d = ToDateTime(e)
        WRITE(s, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))") &
            d % year, &
            d % month, &
            d % day, &
            d % hour, &
            d % minute, &
            d % second
        
    END FUNCTION ToStringE


    ! Convert a date time to number of seconds since the "epoch",
    ! 1970-01-01 00:00:00. Useful when packing a DateTime structure
    ! to a single, "easy" value to be used as time stamp.
    !
    ! This computation is approximate, as it does not take into account
    ! exotic time management details like leap seconds, yet provides sufficient accuracy
    ! for practical applications in range 1970-01-01 to, say, 2100-01-01.
    !
    ! By then I hope some deveoper will have provided something better by then.
    !
    ! Note: the Epoch is intended solar-local in this function and module.
    !
    FUNCTION ToEpoch(d) RESULT(iEpoch)

        ! Routine arguments
        TYPE(DateTime), INTENT(IN) :: d
        INTEGER                    :: iEpoch

        ! Locals
        INTEGER     :: iAuxYear
        INTEGER     :: iAuxMonth
        INTEGER     :: iCentury
        INTEGER     :: iJulianDayEstimate
        INTEGER     :: iDaysFromEpoch

        ! Check input
        IF(.NOT. .VALIDTIME. d) THEN
            iEpoch = -9999
            RETURN
        END IF

        ! Preliminary estimate the Julian day, based on the average duration of year and month in days.
        IF(d % month > 2) THEN
            iAuxYear  = d % year
            iAuxMonth = d % month + 1
        ELSE
            iAuxYear  = d % year - 1
            iAuxMonth = d % month + 13
        END IF
        iJulianDayEstimate = FLOOR(YEAR_DURATION * iAuxYear) + FLOOR(MONTH_DURATION * iAuxMonth) + d % day + 1720995

        ! Correct estimate later than the date reform day (15 October 1582)
        iCentury = 0.01*iAuxYear
        iDaysFromEpoch = iJulianDayEstimate - iCentury + iCentury/4 + 2 - BASE_DAY
        iEpoch = iDaysFromEpoch * 24 * 3600 + d % second + 60*(d % minute + 60*d % hour)

    END FUNCTION ToEpoch


    ! Inverse function of "ToEpoch"
    !
    FUNCTION ToDateTime(iEpoch) RESULT(d)

        ! Routine arguments
        INTEGER, INTENT(IN)     :: iEpoch
        TYPE(DateTime)          :: d

        ! Locals
        INTEGER :: iDeviation
        INTEGER :: iPreJulianDay
        INTEGER :: iPostJulianDay
        INTEGER :: iYearIndex
        INTEGER :: iMonthIndex
        INTEGER :: iDayIndex
        INTEGER :: iYear, iMonth, iDay, iHour, iMinute, iSecond
        INTEGER :: iJulianDay
        INTEGER :: iTimeSeconds

        ! Check parameters
        IF(iEpoch < 0) THEN
            d = DateTime(1970, 1, 1, 0, 0, 0)
            RETURN
        END IF

        ! Unwind Pope Gregorius' day correction
        iJulianDay = iEpoch/(24*3600) + BASE_DAY
        iTimeSeconds = MOD(iEpoch, 24*3600)
        iDeviation = FLOOR(((iJulianDay-1867216)-0.25)/36524.25)
        iPreJulianDay = iJulianDay + iDeviation - iDeviation/4 + 1
        iPostJulianDay = iPreJulianDay + 1524

        ! Compute time indices
        iYearIndex  = FLOOR(6680+((iPostJulianDay-2439870)-122.1)/YEAR_DURATION)
        iDayIndex   = 365*iYearIndex + iYearIndex/4
        iMonthIndex = FLOOR((iPostJulianDay - iDayIndex)/MONTH_DURATION)

        ! Deduce preliminary date from time indices
        iDay = iPostJulianDay - FLOOR(MONTH_DURATION*iMonthIndex) - iDayIndex
        IF(iMonthIndex > 13) THEN
            iMonth = iMonthIndex - 13
        ELSE
            iMonth = iMonthIndex - 1
        END IF
        iYear = iYearIndex - 4715
        IF(iMonth > 2) iYear = iYear - 1

        ! Extract time from the time part
        iSecond = MOD(iTimeSeconds,60)
        iTimeSeconds = iTimeSeconds/60
        iMinute = MOD(iTimeSeconds,60)
        iHour   = iTimeSeconds/60

        ! Pack all time data together and leave
        d = DateTime(iYear, iMonth, iDay, iHour, iMinute, iSecond)

    END FUNCTION ToDateTime


    FUNCTION DayInYearD(d) RESULT(rDayInYear)

        ! Routine arguments
        TYPE(DateTime), INTENT(IN)  :: d
        REAL                        :: rDayInYear

        ! Locals
        INTEGER         :: iEpochD
        INTEGER         :: iEpochY

        ! Compute the information desired
        iEpochD = ToEpoch(d)
        iEpochY = YearE(iEpochD)
        rDayInYear = (iEpochD - iEpochY) / 86400.0 + 1.0

    END FUNCTION DayInYearD


    FUNCTION DayInYearE(d) RESULT(rDayInYear)

        ! Routine arguments
        INTEGER, INTENT(IN) :: d
        REAL                :: rDayInYear

        ! Locals
        INTEGER         :: iEpochY

        ! Compute the information desired
        iEpochY = YearE(d)
        rDayInYear = (d - iEpochY) / 86400.0 + 1.0

    END FUNCTION DayInYearE
    
    
    FUNCTION SolarDeclinationD(d) RESULT(rDeclination)
    
        ! Routine arguments
        TYPE(DateTime), INTENT(IN)  :: d
        REAL                        :: rDeclination
        
        ! Locals
        REAL    :: jd
        
        ! Calculate the desired quantity
        jd = DayInYearD(d)
        rDeclination = 0.409 * COS(2.*PI*(jd-173.)/YEAR_DURATION)
        
    END FUNCTION SolarDeclinationD


    FUNCTION SolarDeclinationE(d) RESULT(rDeclination)
    
        ! Routine arguments
        INTEGER, INTENT(IN) :: d
        REAL                :: rDeclination
        
        ! Locals
        REAL    :: jd
        
        ! Calculate the desired quantity
        jd = DayInYearE(d)
        rDeclination = 0.409 * COS(2.*PI*(jd-173.)/YEAR_DURATION)
        
    END FUNCTION SolarDeclinationE


    FUNCTION SolarElevationAngleSineD(p, d) RESULT(rSinSolarElevation)
    
        ! Routine arguments
        TYPE(GeoPoint), INTENT(IN)  :: p
        TYPE(DateTime), INTENT(IN)  :: d
        REAL                        :: rSinSolarElevation
        
        ! Locals
        INTEGER :: e
        REAL    :: jd
        REAL    :: rHour
        REAL    :: rLat, rLon
        REAL    :: rFuse
        REAL    :: rDecl
        
        ! Get positional data, in radiant form
        rFuse = p % fuse
        rLon  = PI * p % lon / 180.0
        rLat  = PI * p % lat / 180.0
        
        ! Calculate the desired quantity
        e     = ToEpoch(d)
        jd    = DayInYearE(e)
        rHour = (e - HourE(e))/3600.0
        rDecl = SolarDeclinationE(e)
        rSinSolarElevation = SIN(rLat)*SIN(rDecl) - COS(rLat)*COS(rDecl)* &
                                COS((PI*(rHour-rFuse)/12.)-rLon)
        
    END FUNCTION SolarElevationAngleSineD


    FUNCTION SolarElevationAngleSineE(p, d) RESULT(rSinSolarElevation)
    
        ! Routine arguments
        TYPE(GeoPoint), INTENT(IN)  :: p
        INTEGER, INTENT(IN)         :: d
        REAL                        :: rSinSolarElevation
        
        ! Locals
        INTEGER :: e
        REAL    :: jd
        REAL    :: rHour
        REAL    :: rLat, rLon
        REAL    :: rFuse
        REAL    :: rDecl
        
        ! Get positional data, in radiant form
        rFuse = p % fuse
        rLon  = PI * p % lon / 180.0
        rLat  = PI * p % lat / 180.0
        
        ! Calculate the desired quantity
        jd    = DayInYearE(d)
        rHour = (d - HourE(d))/3600.0
        rDecl = SolarDeclinationE(d)
        rSinSolarElevation = SIN(rLat)*SIN(rDecl) - COS(rLat)*COS(rDecl)* &
                                COS((PI*(rHour-rFuse)/12.)-rLon)
        
    END FUNCTION SolarElevationAngleSineE
    
    
    FUNCTION SunriseSunsetD(p, d) RESULT(sunRiseSet)
    
        ! Routine arguments
        TYPE(GeoPoint), INTENT(IN)      :: p
        TYPE(DateTime), INTENT(IN)      :: d
        TYPE(DateTime), DIMENSION(2)    :: sunRiseSet
        
        ! Locals
        INTEGER :: e
        REAL    :: jd
        REAL    :: iCurrentDay
        REAL    :: sl
        REAL    :: decl
        REAL    :: h
        REAL    :: tau1, tau2, t1, t2
        REAL    :: s_rise, s_set
        REAL    :: rHour
        REAL    :: rLat, rLon
        REAL    :: rFuse
        INTEGER :: iSunRise, iSunSet
        
        ! Get positional data, in radiant form
        rFuse = p % fuse
        rLon  = PI * p % lon / 180.0
        rLat  = PI * p % lat / 180.0
        
        ! Calculate the desired quantity
        e           = ToEpoch(d)
        iCurrentDay = DayE(e)
        jd          = DayInYearE(e)
        
        ! Compute sunrise and sunset hours
        sl   = 4.871 + 0.0175*jd + 0.0330*SIN(0.0175*jd)
        decl = ASIN(0.398 * SIN(sl))
        h    = ACOS(-TAN(rLat)*TAN(decl))
        tau1 = ( h+rLon-0.043*SIN(2.*sl)+0.033*sin(0.0175*jd)+PI)/.262
        tau2 = (-h+rLon-0.043*SIN(2.*sl)+0.033*sin(0.0175*jd)+PI)/.262
        t1   = rFuse + tau1
        t2   = rFuse + tau2
        IF(t1 < 0.) t1 = t1 + 24.
        IF(t2 < 0.) t2 = t2 + 24.
        s_rise = MIN(t1,t2)
        s_set  = MAX(t1,t2)
        
        ! Transform sun rise and sun set hours to epoch values
        iSunRise = iCurrentDay + s_rise*3600.0
        iSunSet  = iCurrentDay + s_set*3600.0
        
        ! Write output vector
        sunRiseSet = [ToDateTime(iSunRise), ToDateTime(iSunSet)]
    
    END FUNCTION SunriseSunsetD

    
    FUNCTION SunriseSunsetE(p, e) RESULT(sunRiseSet)
    
        ! Routine arguments
        TYPE(GeoPoint), INTENT(IN)  :: p
        INTEGER, INTENT(IN)         :: e
        INTEGER, DIMENSION(2)       :: sunRiseSet
        
        ! Locals
        REAL    :: jd
        REAL    :: iCurrentDay
        REAL    :: sl
        REAL    :: decl
        REAL    :: h
        REAL    :: tau1, tau2, t1, t2
        REAL    :: s_rise, s_set
        REAL    :: rHour
        REAL    :: rLat, rLon
        REAL    :: rFuse
        INTEGER :: iSunRise, iSunSet
        
        ! Get positional data, in radiant form
        rFuse = p % fuse
        rLon  = PI * p % lon / 180.0
        rLat  = PI * p % lat / 180.0
        
        ! Calculate the desired quantity
        iCurrentDay = DayE(e)
        jd          = DayInYearE(e)
        
        ! Compute sunrise and sunset hours
        sl   = 4.871 + 0.0175*jd + 0.0330*SIN(0.0175*jd)
        decl = ASIN(0.398 * SIN(sl))
        h    = ACOS(-TAN(rLat)*TAN(decl))
        tau1 = ( h+rLon-0.043*SIN(2.*sl)+0.033*sin(0.0175*jd)+PI)/.262
        tau2 = (-h+rLon-0.043*SIN(2.*sl)+0.033*sin(0.0175*jd)+PI)/.262
        t1   = rFuse + tau1
        t2   = rFuse + tau2
        IF(t1 < 0.) t1 = t1 + 24.
        IF(t2 < 0.) t2 = t2 + 24.
        s_rise = MIN(t1,t2)
        s_set  = MAX(t1,t2)
        
        ! Transform sun rise and sun set hours to epoch values
        iSunRise = iCurrentDay + s_rise*3600.0
        iSunSet  = iCurrentDay + s_set*3600.0
        
        ! Write output vector
        sunRiseSet = [iSunRise, iSunSet]
    
    END FUNCTION SunriseSunsetE

END MODULE TimeManagement
