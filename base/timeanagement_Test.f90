! Test driver for "TimeManagement" module.
!
! By: Mauri Favaron  (24. 08. 2015)
!
! License: LGPL V3.0

PROGRAM TimeManagement_Test

    USE timeManagement
    
    IMPLICIT NONE
    
    ! Locals
    TYPE(GeoPoint)  :: tGeoPoint
    TYPE(DateTime)  :: tDateTime
    TYPE(DateTime), DIMENSION(2)    :: tvSunRiseSet
    INTEGER         :: iDateTime
    TYPE(DateTime), DIMENSION(8784) :: tvFullYear
    INTEGER         :: i
    INTEGER         :: iEpoch
    
    ! Test no.1: Check the couple ToEpoch() / ToDateTime() yields
    !            an identity function after composition
    tDateTime = DateTime(2015, 10, 20, 13, 22, 50)  ! A date-time, chosen carelessly
    PRINT *,'Test no.1'
    PRINT *, ToString(tDateTime)
    iEpoch = ToEpoch(tDateTime)
    PRINT *, iEpoch
    tDateTime = ToDateTime(iEpoch)
    PRINT *, ToString(tDateTime)
    iEpoch = ToEpoch(tDateTime)
    PRINT *, iEpoch
    PRINT *
    
    ! Test no.2: Check time shift
    PRINT *,'Test no.2'
    tDateTime = TimeShift(tDateTime, 3600*0)
    PRINT *, ToString(tDateTime)
    tDateTime = TimeShift(tDateTime, 3600*10)
    PRINT *, ToString(tDateTime)
    tDateTime = TimeShift(tDateTime, 3600*100)
    PRINT *, ToString(tDateTime)
    
    ! Test no.3: Generate a full even year
    PRINT *,'Test no.3'
    tDateTime = DateTime(2015, 10, 20, 13, 22, 50)  ! A date-time, chosen carelessly
    ! -1- Set to year beginning
    tDateTime = Year(tDateTime)
    PRINT *, ToString(tDateTime)
    ! -1- Iterate all hours in year
    DO i = 0, 8759
        tvFullYear(i+1) = TimeShift(tDateTime, 3600*i)
    END DO
    ! -1- Build file, containing all generated dates and times and their day-in-year numbers
    OPEN(10, FILE='Year_2015.csv', STATUS='UNKNOWN', ACTION='WRITE')
    WRITE(10, "('date, doy')")
    DO i = 1, 8760
        WRITE(10, "(a,',',f7.3)") &
            ToString(tvFullYear(i)), &
            DayInYear(tvFullYear(i))
    END DO
    CLOSE(10)

    ! Test no.4: Generate a full leap year
    PRINT *,'Test no.4'
    tDateTime = DateTime(2016, 1, 1, 0, 0, 0)   ! A date-time, chosen carelessly
    ! -1- Set to year beginning
    tDateTime = Year(tDateTime)
    PRINT *, ToString(tDateTime)
    ! -1- Iterate all hours in year
    DO i = 0, 8783
        tvFullYear(i+1) = TimeShift(tDateTime, 3600*i)
    END DO
    ! -1- Build file, containing all generated dates and times and their day-in-year numbers
    OPEN(10, FILE='Year_2016.csv', STATUS='UNKNOWN', ACTION='WRITE')
    WRITE(10, "('date, doy')")
    DO i = 1, 8784
        WRITE(10, "(a,',',f7.3)") &
            ToString(tvFullYear(i)), &
            DayInYear(tvFullYear(i))
    END DO
    CLOSE(10)
    
    ! Test no.5: Generate a full leap year, but using Epoch function version
    PRINT *,'Test no.5'
    iDateTime = ToEpoch(DateTime(2016, 1, 1, 0, 0, 0))
    ! -1- Set to year beginning
    iDateTime = Year(iDateTime)
    PRINT *, ToString(iDateTime)
    ! -1- Iterate all hours in year
    DO i = 0, 8783
        tvFullYear(i+1) = ToDateTime(TimeShift(iDateTime, 3600*i))
    END DO
    ! -1- Build file, containing all generated dates and times and their day-in-year numbers
    OPEN(10, FILE='Year_2016_2.csv', STATUS='UNKNOWN', ACTION='WRITE')
    WRITE(10, "('date, doy')")
    DO i = 1, 8784
        WRITE(10, "(a,',',f7.3)") &
            ToString(tvFullYear(i)), &
            DayInYear(tvFullYear(i))
    END DO
    CLOSE(10)
    
    ! Generate time equation for various localities
    PRINT *,'Test no.6'
    tGeoPoint = GeoPoint(-9.16, 45.5, 1.)   ! Milan
    OPEN(10, FILE='Year_2016_Milan.csv', STATUS='UNKNOWN', ACTION='WRITE')
    WRITE(10, "('date, sun.decl, sun.elev, sun.rise, sun.set')")
    DO i = 1, 8784
        tvSunRiseSet = SunriseSunset(tGeoPoint, tvFullYear(i))
        WRITE(10, "(a,',',f8.5,',',f8.3,',',a,',',a)") &
            ToString(tvFullYear(i)), &
            SolarDeclination(tvFullYear(i)), &
            ASIN(SolarElevationAngleSine(tGeoPoint, tvFullYear(i)))*180.0/3.14159265, &
            ToString(tvSunRiseSet(1)), &
            ToString(tvSunRiseSet(2))
    END DO
    CLOSE(10)
    tGeoPoint = GeoPoint(-12.5, 42.0, -1.)  ! Rome
    OPEN(10, FILE='Year_2016_Rome.csv', STATUS='UNKNOWN', ACTION='WRITE')
    WRITE(10, "('date, sun.decl, sun.elev, sun.rise, sun.set')")
    DO i = 1, 8784
        tvSunRiseSet = SunriseSunset(tGeoPoint, tvFullYear(i))
        WRITE(10, "(a,',',f8.5,',',f8.3,',',a,',',a)") &
            ToString(tvFullYear(i)), &
            SolarDeclination(tvFullYear(i)), &
            ASIN(SolarElevationAngleSine(tGeoPoint, tvFullYear(i)))*180.0/3.14159265, &
            ToString(tvSunRiseSet(1)), &
            ToString(tvSunRiseSet(2))
    END DO
    CLOSE(10)
    tGeoPoint = GeoPoint(-2.33, 48.8, -1.)  ! Paris
    OPEN(10, FILE='Year_2016_Paris.csv', STATUS='UNKNOWN', ACTION='WRITE')
    WRITE(10, "('date, sun.decl, sun.elev, sun.rise, sun.set')")
    DO i = 1, 8784
        tvSunRiseSet = SunriseSunset(tGeoPoint, tvFullYear(i))
        WRITE(10, "(a,',',f8.5,',',f8.3,',',a,',',a)") &
            ToString(tvFullYear(i)), &
            SolarDeclination(tvFullYear(i)), &
            ASIN(SolarElevationAngleSine(tGeoPoint, tvFullYear(i)))*180.0/3.14159265, &
            ToString(tvSunRiseSet(1)), &
            ToString(tvSunRiseSet(2))
    END DO
    CLOSE(10)
    tGeoPoint = GeoPoint(-13.5, 52.5, -1.)  ! Berlin
    OPEN(10, FILE='Year_2016_Berlin.csv', STATUS='UNKNOWN', ACTION='WRITE')
    WRITE(10, "('date, sun.decl, sun.elev, sun.rise, sun.set')")
    DO i = 1, 8784
        tvSunRiseSet = SunriseSunset(tGeoPoint, tvFullYear(i))
        WRITE(10, "(a,',',f8.5,',',f8.3,',',a,',',a)") &
            ToString(tvFullYear(i)), &
            SolarDeclination(tvFullYear(i)), &
            ASIN(SolarElevationAngleSine(tGeoPoint, tvFullYear(i)))*180.0/3.14159265, &
            ToString(tvSunRiseSet(1)), &
            ToString(tvSunRiseSet(2))
    END DO
    CLOSE(10)

END PROGRAM TimeManagement_Test
