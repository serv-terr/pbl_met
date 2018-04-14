!------------------------------------------------------------------
!
!   MODOS.f90 - Fortran 90 module, supporting the basic read-and-decode
!               of MODOS-formatted data files, produced by SODAR and
!               SODAR/RASS of types MODOS and PCS-2000, manufactured
!               by Metek GmbH.
!
!   The main data type (ModosRecordType) is implemented as an
!   abstract data type.
!
!   This will not in general affect the *usage* of the module.
!
!   Copyright (C) 2018 by Servizi Territorio srl
!   This is open source software, covered by the license lGPL V3,0
!
!   Written by: Mauri Favaron
!   e-mail:     mafavaron@mac.com
!
MODULE MODOS

    IMPLICIT NONE
    PRIVATE

    ! Public return codes
    INTEGER, PARAMETER  :: MODOS_SUCCESS    = 0
    PUBLIC              :: MODOS_SUCCESS
    INTEGER, PARAMETER  :: MODOS_EOF        = -1
    PUBLIC              :: MODOS_EOF
    INTEGER, PARAMETER  :: MODOS_IVHEADER   = 1
    PUBLIC              :: MODOS_IVHEADER

    ! Internal data
    ! -1- Internal constants
    INTEGER, PARAMETER      :: REC_LEN = 1024
    INTEGER, PARAMETER      :: MAX_VERTICAL_STEPS = 40
    INTEGER, PARAMETER      :: MAX_BEAMS          =  6
    INTEGER, PARAMETER      :: MAX_COMPONENTS     =  3
    INTEGER, PARAMETER      :: NUM_SPECTRAL_ITEMS = 32
    REAL, PARAMETER         :: INVALID_VALUE = -9999.9
    ! -1- Save space for last record header encountered
    CHARACTER(LEN=REC_LEN)  :: sHeader = ' '

    ! Main data type: general MODOS record. Actual instances of MODOS data
    ! records are subsets of this.
    TYPE ModosRecordType
        ! Restrict external access using standard routines,
        ! and ensure in-memory ordering
        PRIVATE
        SEQUENCE
        ! Time stamp
        INTEGER                                             :: iYear
        INTEGER                                             :: iMonth
        INTEGER                                             :: iDay
        INTEGER                                             :: iHour
        INTEGER                                             :: iMinute
        INTEGER                                             :: iSecond
        ! Operations data
        INTEGER                                             :: iAverage
        REAL                                                :: rMinHeight
        REAL                                                :: rMaxHeight
        REAL                                                :: rNoiseHeight
        REAL                                                :: rDeltaHeight
        INTEGER                                             :: iNumVolumes
        INTEGER, DIMENSION(MAX_BEAMS)                       :: ivVolume
        INTEGER                                             :: iNumFrequencies
        REAL                                                :: rSodarFrequency
        REAL                                                :: rRassFrequency
        REAL, DIMENSION(MAX_BEAMS)                          :: rvMixingFrequency
        REAL                                                :: rSodarSamplingFrequency
        REAL                                                :: rRassSamplingFrequency
        INTEGER                                             :: iNumBeams
        REAL, DIMENSION(MAX_BEAMS-1)                        :: rvAzimuth
        REAL, DIMENSION(MAX_BEAMS-1)                        :: rvZenith
        REAL                                                :: rRefTemperature
        REAL                                                :: rCrosstalk
        ! Heights (m) - Mandatory
        INTEGER                                             :: iNumHeights
        REAL, DIMENSION(MAX_VERTICAL_STEPS)                 :: rvHeight
        REAL                                                :: rLowerNoiseHeight
        REAL                                                :: rUpperNoiseHeight
        ! Spectra (dB)
        LOGICAL, DIMENSION(MAX_BEAMS,NUM_SPECTRAL_ITEMS)    :: lmSpectrumAvailable
        INTEGER, DIMENSION(MAX_BEAMS,NUM_SPECTRAL_ITEMS)    :: imNumSpectrum
        REAL, DIMENSION(MAX_BEAMS,NUM_SPECTRAL_ITEMS,MAX_VERTICAL_STEPS)    :: raSpectrum
        ! Power (dB)
        LOGICAL, DIMENSION(MAX_BEAMS)                       :: lvPowerAvailable
        INTEGER, DIMENSION(MAX_BEAMS)                       :: ivNumPower
        REAL, DIMENSION(MAX_BEAMS,MAX_VERTICAL_STEPS)       :: rmPower
        ! Reflectivity (dB)
        LOGICAL, DIMENSION(MAX_BEAMS)                       :: lvReflectivityAvailable
        INTEGER, DIMENSION(MAX_BEAMS)                       :: ivNumReflectivity
        REAL, DIMENSION(MAX_BEAMS,MAX_VERTICAL_STEPS)       :: rmReflectivity
        ! Radial wind components (m/s)
        LOGICAL, DIMENSION(MAX_BEAMS)                       :: lvRadialWindAvailable
        INTEGER, DIMENSION(MAX_BEAMS)                       :: ivNumRadialWind
        REAL, DIMENSION(MAX_BEAMS,MAX_VERTICAL_STEPS)       :: rmRadialWind
        ! Vector wind component (m/s)
        LOGICAL, DIMENSION(MAX_COMPONENTS)                  :: lvVectorWindAvailable
        INTEGER, DIMENSION(MAX_COMPONENTS)                  :: ivNumVectorWind
        REAL, DIMENSION(MAX_COMPONENTS,MAX_VERTICAL_STEPS)  :: rmVectorWind
        ! Wind speed (m/s) and direction (°)
        LOGICAL                                             :: lWindSpeedAvailable
        INTEGER                                             :: iNumWindSpeed
        REAL, DIMENSION(MAX_VERTICAL_STEPS)                 :: rvWindSpeed
        LOGICAL                                             :: lWindDirectionAvailable
        INTEGER                                             :: iNumWindDirection
        REAL, DIMENSION(MAX_VERTICAL_STEPS)                 :: rvWindDirection
        ! Sigmas of radial components (m/s)
        LOGICAL, DIMENSION(MAX_BEAMS)                       :: lvRadialSigmaAvailable
        INTEGER, DIMENSION(MAX_BEAMS)                       :: ivNumRadialSigma
        REAL, DIMENSION(MAX_BEAMS,MAX_VERTICAL_STEPS)       :: rmRadialSigma
        ! Sigma of the wind inclination (°)
        LOGICAL                                             :: lPhiSigmaAvailable
        INTEGER                                             :: iNumPhiSigma
        REAL, DIMENSION(MAX_VERTICAL_STEPS)                 :: rvPhiSigma
        ! Diffusion classes ("A", ..., "F")
        LOGICAL                                             :: lDiffusionClassAvailable
        INTEGER                                             :: iNumDiffusionClass
        CHARACTER, DIMENSION(MAX_VERTICAL_STEPS)            :: cvDiffusionClass
        ! Temperature (°C)
        LOGICAL                                             :: lTemperatureAvailable
        INTEGER                                             :: iNumTemperature
        REAL, DIMENSION(MAX_VERTICAL_STEPS)                 :: rvTemperature
        ! Availability (%)
        LOGICAL, DIMENSION(MAX_BEAMS)                       :: lvAvailabilityAvailable
        INTEGER, DIMENSION(MAX_BEAMS)                       :: ivNumAvailability
        REAL, DIMENSION(MAX_BEAMS,MAX_VERTICAL_STEPS)       :: rmAvailability
        ! Signal to Noise ratio (dB)
        LOGICAL, DIMENSION(MAX_BEAMS)                       :: lvSignalToNoiseAvailable
        INTEGER, DIMENSION(MAX_BEAMS)                       :: ivNumSignalToNoise
        REAL, DIMENSION(MAX_BEAMS,MAX_VERTICAL_STEPS)       :: rmSignalToNoise
        ! Error Codes
        LOGICAL, DIMENSION(MAX_BEAMS)                       :: lvErrorAvailable
        INTEGER, DIMENSION(MAX_BEAMS)                       :: ivNumError
        INTEGER, DIMENSION(MAX_BEAMS,MAX_VERTICAL_STEPS)    :: imError
    END TYPE ModosRecordType
    PUBLIC :: ModosRecordType

    ! Polimorphic interfaces
    INTERFACE ModosGetProfile
        MODULE PROCEDURE GetProfile
        MODULE PROCEDURE GetProfileChar
        MODULE PROCEDURE GetProfile1d
        MODULE PROCEDURE GetProfile1dInt
        MODULE PROCEDURE GetProfile2d
    END INTERFACE ModosGetProfile

    ! Public measured data codes
    INTEGER, PARAMETER  :: MODOS_D_SPECTRUM        =  1
    PUBLIC              :: MODOS_D_SPECTRUM
    INTEGER, PARAMETER  :: MODOS_D_POWER           =  2
    PUBLIC              :: MODOS_D_POWER
    INTEGER, PARAMETER  :: MODOS_D_REFLECTIVITY    =  3
    PUBLIC              :: MODOS_D_REFLECTIVITY
    INTEGER, PARAMETER  :: MODOS_D_RADIALWIND      =  4
    PUBLIC              :: MODOS_D_RADIALWIND
    INTEGER, PARAMETER  :: MODOS_D_VECTORWIND      =  5
    PUBLIC              :: MODOS_D_VECTORWIND
    INTEGER, PARAMETER  :: MODOS_D_WINDSPEED       =  6
    PUBLIC              :: MODOS_D_WINDSPEED
    INTEGER, PARAMETER  :: MODOS_D_WINDDIRECTION   =  7
    PUBLIC              :: MODOS_D_WINDDIRECTION
    INTEGER, PARAMETER  :: MODOS_D_RADIALSIGMA     =  8
    PUBLIC              :: MODOS_D_RADIALSIGMA
    INTEGER, PARAMETER  :: MODOS_D_PHISIGMA        =  9
    PUBLIC              :: MODOS_D_PHISIGMA
    INTEGER, PARAMETER  :: MODOS_D_DIFFUSIONCLASS  = 10
    PUBLIC              :: MODOS_D_DIFFUSIONCLASS
    INTEGER, PARAMETER  :: MODOS_D_TEMPERATURE     = 11
    PUBLIC              :: MODOS_D_TEMPERATURE
    INTEGER, PARAMETER  :: MODOS_D_AVAILABILITY    = 12
    PUBLIC              :: MODOS_D_AVAILABILITY
    INTEGER, PARAMETER  :: MODOS_D_SIGNALTONOISE   = 13
    PUBLIC              :: MODOS_D_SIGNALTONOISE
    INTEGER, PARAMETER  :: MODOS_D_ERROR           = 14
    PUBLIC              :: MODOS_D_ERROR
    
    ! Public exclusion codes (common values to "iExclude"
    ! argument of some variants of "ModosGetProfile"
    ! routine)
    ! -1- Primitive codes
    INTEGER, PARAMETER  :: MODOS_E_NONE             =    0
    PUBLIC              :: MODOS_E_NONE
    INTEGER, PARAMETER  :: MODOS_E_SATURATION       =    1
    PUBLIC              :: MODOS_E_SATURATION
    INTEGER, PARAMETER  :: MODOS_E_WHITENOISE       =    2
    PUBLIC              :: MODOS_E_WHITENOISE
    INTEGER, PARAMETER  :: MODOS_E_STRONGSPECTRA    =    4
    PUBLIC              :: MODOS_E_STRONGSPECTRA
    INTEGER, PARAMETER  :: MODOS_E_LOWSIGNALTONOISE =   16
    PUBLIC              :: MODOS_E_LOWSIGNALTONOISE
    INTEGER, PARAMETER  :: MODOS_E_LOWSIGNIFSIGNAL  =   32
    PUBLIC              :: MODOS_E_LOWSIGNIFSIGNAL
    INTEGER, PARAMETER  :: MODOS_E_LOWSIGNIFNOISE   =   64
    PUBLIC              :: MODOS_E_LOWSIGNIFNOISE
    INTEGER, PARAMETER  :: MODOS_E_SLIMSPECTRUM     =  256
    PUBLIC              :: MODOS_E_SLIMSPECTRUM
    INTEGER, PARAMETER  :: MODOS_E_WIDESPECTRUM     =  512
    PUBLIC              :: MODOS_E_WIDESPECTRUM
    INTEGER, PARAMETER  :: MODOS_E_SMALLSIGNALSPCTR = 1024
    PUBLIC              :: MODOS_E_SMALLSIGNALSPCTR
    INTEGER, PARAMETER  :: MODOS_E_IMAGINARYSTDDEV  = 2048
    PUBLIC              :: MODOS_E_IMAGINARYSTDDEV
    ! -1- Combined codes (common exclusion criteria
    !     arranged in increasingly restrictive severity)
    INTEGER, PARAMETER  :: MODOS_E_BADMEASUREMENT   = &
                            MODOS_E_LOWSIGNALTONOISE + &
                            MODOS_E_LOWSIGNIFSIGNAL  + &
                            MODOS_E_LOWSIGNIFNOISE   + &
                            MODOS_E_SLIMSPECTRUM     + &
                            MODOS_E_WIDESPECTRUM     + &
                            MODOS_E_SMALLSIGNALSPCTR
    PUBLIC              :: MODOS_E_BADMEASUREMENT
    INTEGER, PARAMETER  :: MODOS_E_INVALID          = &
                            MODOS_E_BADMEASUREMENT   + &
                            MODOS_E_IMAGINARYSTDDEV
    PUBLIC              :: MODOS_E_INVALID
    INTEGER, PARAMETER  :: MODOS_E_SUSPECT          = &
                            MODOS_E_INVALID          + &
                            MODOS_E_SATURATION       + &
                            MODOS_E_WHITENOISE       + &
                            MODOS_E_STRONGSPECTRA
    PUBLIC              :: MODOS_E_SUSPECT

    ! Public procedures
    PUBLIC  :: ModosReadRecord
    PUBLIC  :: ModosRewind
    PUBLIC  :: ModosGetTimeString
    PUBLIC  :: ModosGetTime
    PUBLIC  :: ModosGetAveragingTime
    PUBLIC  :: ModosGetHeights
    PUBLIC  :: ModosGetEmissionParameters
    PUBLIC  :: ModosGetFrequencies
    PUBLIC  :: ModosGetBeamGeometry
    PUBLIC  :: ModosGetEmissionVolumes
    PUBLIC  :: ModosGetTemperature
    PUBLIC  :: ModosGetCrosstalk
    PUBLIC  :: ModosGetProfile
    PUBLIC  :: ModosGetLimitHeight
    PUBLIC  :: ModosAnalyzeError
    PUBLIC  :: ModosUpdateErrorCounter
    
CONTAINS

    FUNCTION ModosReadRecord(iLUN, tRecord, lPresent) RESULT(iRetCode)

        ! Routine arguments
        INTEGER, INTENT(IN)                     :: iLUN
        TYPE(ModosRecordType), INTENT(INOUT)    :: tRecord
        LOGICAL, INTENT(OUT)                    :: lPresent
        INTEGER                                 :: iRetCode

        ! Locals
        CHARACTER(LEN=REC_LEN)  :: sBuffer

        ! Assume success (will falsify on failure), and ensure a clean
        ! data record.
        iRetCode = MODOS_SUCCESS
        lPresent = .FALSE.
        CALL Clean(tRecord)

        ! Advance to the first available header line ("SDR")
        IF(sHeader == ' ') THEN
            DO
                READ(iLUN, "(a)", IOSTAT=iRetCode) sBuffer
                IF(iRetCode /= 0) THEN
                    iRetCode = MODOS_EOF
                    sHeader = ' '
                    RETURN
                END IF
                IF(sBuffer(1:3)=='SDR') EXIT
            END DO
        ELSE
            sBuffer = sHeader
        END IF

        ! Parse header
        lPresent = .TRUE.
        CALL ParseHeader(sBuffer, tRecord)
        
        ! Parse all remaining data, until another header file exists
        DO
            ! Get a data line
            READ(iLUN, "(a)", IOSTAT=iRetCode) sBuffer
            IF(iRetCode /= 0) THEN
                iRetCode = MODOS_EOF
                sHeader = ' '
                EXIT
            END IF
            IF(sBuffer(1:3) == 'SDR') THEN
                sHeader = sBuffer
                EXIT
            END IF
            ! Parse data line
            CALL ParseDataLine(sBuffer, tRecord)
        END DO

    END FUNCTION ModosReadRecord
    
    
    FUNCTION ModosRewind(iLUN) RESULT(iRetCode)

        ! Routine arguments
        INTEGER, INTENT(IN)                     :: iLUN
        INTEGER                                 :: iRetCode

        ! Locals
        ! -none-
        
        ! Rewind file and clean header
        REWIND(iLUN)
        sHeader = ' '
        
    END FUNCTION ModosRewind
    
    
    FUNCTION ModosGetTimeString(tRecord) RESULT(sDateTime)
    
        ! Routine arguments
        TYPE(ModosRecordType), INTENT(IN)   :: tRecord
        CHARACTER(LEN=19)                   :: sDateTime
        
        ! Locals
        INTEGER :: iYear
        
        ! Format date and time from internal time stamp
        IF(tRecord%iMonth > 0) THEN
            IF(tRecord % iYear > 69) THEN
                iYear = tRecord % iYear + 1900
            ELSE
                iYear = tRecord % iYear + 2000
            END IF
            WRITE(sDateTime,"(2(i2.2,'/'),i4.4,1x,2(i2.2,':'),i2.2)") &
                tRecord % iDay, &
                tRecord % iMonth, &
                iYear, &
                tRecord % iHour, &
                tRecord % iMinute, &
                tRecord % iSecond
        ELSE
            sDateTime = ' '
        END IF
        
    END FUNCTION ModosGetTimeString
    
    
    SUBROUTINE ModosGetTime(tRecord, iYear, iMonth, iDay, iHour, iMinute, iSecond)
    
        ! Routine arguments
        TYPE(ModosRecordType), INTENT(IN)   :: tRecord
        INTEGER, INTENT(OUT)                :: iYear
        INTEGER, INTENT(OUT)                :: iMonth
        INTEGER, INTENT(OUT)                :: iDay
        INTEGER, INTENT(OUT)                :: iHour
        INTEGER, INTENT(OUT)                :: iMinute
        INTEGER, INTENT(OUT)                :: iSecond
        
        ! Locals
        ! -none-

        ! Get the information desired        
        IF(tRecord%iMonth > 0) THEN
            IF(tRecord % iYear > 69) THEN
                iYear = tRecord % iYear + 1900
            ELSE
                iYear = tRecord % iYear + 2000
            END IF
            iMonth  = tRecord%iMonth
            iDay    = tRecord%iDay
            iHour   = tRecord%iHour
            iMinute = tRecord%iMinute
            iSecond = tRecord%iSecond
        ELSE
            iYear   = -1
            iMonth  = -1
            iDay    = -1
            iHour   = -1
            iMinute = -1
            iSecond = -1
        END IF
        
    END SUBROUTINE ModosGetTime
    


    FUNCTION ModosGetAveragingTime(tRecord) RESULT(iAveragingTime)
    
        ! Routine arguments
        TYPE(ModosRecordType), INTENT(IN)   :: tRecord
        INTEGER                             :: iAveragingTime
        
        ! Locals
        ! -none-
        
        ! Get the information required
        iAveragingTime = tRecord % iAverage
        
    END FUNCTION ModosGetAveragingTime
    
    
    SUBROUTINE ModosGetHeights(tRecord, rMinHeight, rMaxHeight, rNoiseHeight, rLowerNoiseHeight, rUpperNoiseHeight, rDeltaHeight)
    
        ! Routine arguments
        TYPE(ModosRecordType), INTENT(IN)   :: tRecord
        REAL, INTENT(OUT)                   :: rMinHeight
        REAL, INTENT(OUT)                   :: rMaxHeight
        REAL, INTENT(OUT)                   :: rNoiseHeight
        REAL, INTENT(OUT)                   :: rLowerNoiseHeight
        REAL, INTENT(OUT)                   :: rUpperNoiseHeight
        REAL, INTENT(OUT)                   :: rDeltaHeight
        
        ! Locals
        ! -none-
        
        ! Retrieve the information argument
        rMinHeight        = tRecord % rMinHeight
        rMaxHeight        = tRecord % rMaxHeight
        rNoiseHeight      = tRecord % rNoiseHeight
        rLowerNoiseHeight = tRecord % rLowerNoiseHeight
        rUpperNoiseHeight = tRecord % rUpperNoiseHeight
        rDeltaHeight      = tRecord % rDeltaHeight
        
    END SUBROUTINE ModosGetHeights
    
    
    SUBROUTINE ModosGetEmissionParameters( &
        tRecord, &
        iNumVolumes, &
        ivVolume, &
        iNumFrequencies, &
        rSodarFrequency, &
        rRassFrequency, &
        rvMixingFrequency, &
        rSodarSamplingFrequency, &
        rRassSamplingFrequency, &
        iNumBeams, &
        rvAzimuth, &
        rvZenith, &
        rRefTemperature, &
        rCrosstalk &
    )

        ! Routine arguments
        TYPE(ModosRecordType), INTENT(IN)           :: tRecord
        INTEGER, INTENT(OUT)                        :: iNumVolumes
        INTEGER, DIMENSION(MAX_BEAMS), INTENT(OUT)  :: ivVolume
        INTEGER, INTENT(OUT)                        :: iNumFrequencies
        REAL, INTENT(OUT)                           :: rSodarFrequency
        REAL, INTENT(OUT)                           :: rRassFrequency
        REAL, DIMENSION(MAX_BEAMS), INTENT(OUT)     :: rvMixingFrequency
        REAL, INTENT(OUT)                           :: rSodarSamplingFrequency
        REAL, INTENT(OUT)                           :: rRassSamplingFrequency
        INTEGER, INTENT(OUT)                        :: iNumBeams
        REAL, DIMENSION(MAX_BEAMS-1), INTENT(OUT)   :: rvAzimuth
        REAL, DIMENSION(MAX_BEAMS-1), INTENT(OUT)   :: rvZenith
        REAL, INTENT(OUT)                           :: rRefTemperature
        REAL, INTENT(OUT)                           :: rCrosstalk
        
        ! Locals
        ! -none-
        
        ! Get the information required
        iNumVolumes             = tRecord % iNumVolumes
        ivVolume                = tRecord % ivVolume
        iNumFrequencies         = tRecord % iNumFrequencies
        rSodarFrequency         = tRecord % rSodarFrequency
        rRassFrequency          = tRecord % rRassFrequency
        rvMixingFrequency       = tRecord % rvMixingFrequency
        rSodarSamplingFrequency = tRecord % rSodarSamplingFrequency
        rRassSamplingFrequency  = tRecord % rRassSamplingFrequency
        iNumBeams               = tRecord % iNumBeams
        rvAzimuth               = tRecord % rvAzimuth
        rvZenith                = tRecord % rvZenith
        rRefTemperature         = tRecord % rRefTemperature
        rCrosstalk              = tRecord % rCrosstalk
        
    END SUBROUTINE ModosGetEmissionParameters
    
        
    SUBROUTINE ModosGetFrequencies( &
        tRecord, &
        rSodarFrequency, &
        rRassFrequency, &
        rSodarSamplingFrequency, &
        rRassSamplingFrequency &
    )

        ! Routine arguments
        TYPE(ModosRecordType), INTENT(IN)           :: tRecord
        REAL, INTENT(OUT)                           :: rSodarFrequency
        REAL, INTENT(OUT)                           :: rRassFrequency
        REAL, INTENT(OUT)                           :: rSodarSamplingFrequency
        REAL, INTENT(OUT)                           :: rRassSamplingFrequency
        
        ! Locals
        ! -none-
        
        ! Get the information required
        rSodarFrequency         = tRecord % rSodarFrequency
        rRassFrequency          = tRecord % rRassFrequency
        rSodarSamplingFrequency = tRecord % rSodarSamplingFrequency
        rRassSamplingFrequency  = tRecord % rRassSamplingFrequency
        
    END SUBROUTINE ModosGetFrequencies
    
        
    SUBROUTINE ModosGetBeamGeometry( &
        tRecord, &
        rvAzimuth, &
        rvZenith &
    )

        ! Routine arguments
        TYPE(ModosRecordType), INTENT(IN)           :: tRecord
        REAL, DIMENSION(MAX_BEAMS-1), INTENT(OUT)   :: rvAzimuth
        REAL, DIMENSION(MAX_BEAMS-1), INTENT(OUT)   :: rvZenith
        
        ! Locals
        ! -none-
        
        ! Get the information required
        rvAzimuth = tRecord % rvAzimuth
        rvZenith  = tRecord % rvZenith
        
    END SUBROUTINE ModosGetBeamGeometry
    

    SUBROUTINE ModosGetEmissionVolumes( &
        tRecord, &
        iNumVolumes, &
        ivVolume &
    )

        ! Routine arguments
        TYPE(ModosRecordType), INTENT(IN)           :: tRecord
        INTEGER, INTENT(OUT)                        :: iNumVolumes
        INTEGER, DIMENSION(MAX_BEAMS), INTENT(OUT)  :: ivVolume
        
        ! Locals
        ! -none-
        
        ! Get the information required
        iNumVolumes             = tRecord % iNumVolumes
        ivVolume                = tRecord % ivVolume
        
    END SUBROUTINE ModosGetEmissionVolumes
    
        
    FUNCTION ModosGetTemperature(tRecord) RESULT(rTemperature)
    
        ! Routine arguments
        TYPE(ModosRecordType), INTENT(IN)   :: tRecord
        REAL                                :: rTemperature
        
        ! Locals
        ! -none-
        
        ! Get the information required
        rTemperature = tRecord % rRefTemperature
        
    END FUNCTION ModosGetTemperature
    
            
    FUNCTION ModosGetCrosstalk(tRecord) RESULT(rCrosstalk)
    
        ! Routine arguments
        TYPE(ModosRecordType), INTENT(IN)   :: tRecord
        REAL                                :: rCrosstalk
        
        ! Locals
        ! -none-
        
        ! Get the information required
        rCrosstalk = tRecord % rCrosstalk
        
    END FUNCTION ModosGetCrosstalk
    
            
    SUBROUTINE GetProfile(tRecord, iData, iExclude, iNumHeights, rvHeight, rvValue)
    
        ! Routine arguments
        TYPE(ModosRecordType), INTENT(IN)                   :: tRecord
        INTEGER, INTENT(IN)                                 :: iData
        INTEGER, INTENT(IN)                                 :: iExclude
        INTEGER, INTENT(OUT)                                :: iNumHeights
        REAL, DIMENSION(MAX_VERTICAL_STEPS), INTENT(OUT)    :: rvHeight
        REAL, DIMENSION(MAX_VERTICAL_STEPS), INTENT(OUT)    :: rvValue
        
        ! Locals
        INTEGER :: i
        INTEGER :: iActualNumHeights

        ! Get all data matching no bits in exclusion criterium
        iNumHeights = tRecord % iNumHeights
        SELECT CASE(iData)
        CASE(MODOS_D_SPECTRUM)
            iNumHeights = 0
        CASE(MODOS_D_POWER)
            iNumHeights = 0
        CASE(MODOS_D_REFLECTIVITY)
            iNumHeights = 0
        CASE(MODOS_D_RADIALWIND)
            iNumHeights = 0
        CASE(MODOS_D_VECTORWIND)
            iNumHeights = 0
        CASE(MODOS_D_WINDSPEED)
            IF(tRecord % lWindSpeedAvailable) THEN
                WHERE( &
                    IAND(tRecord % imError(1,1:iNumHeights),iExclude) == 0 .AND. &
                    IAND(tRecord % imError(2,1:iNumHeights),iExclude) == 0 &
                )
                    rvValue(1:iNumHeights) = tRecord % rvWindSpeed(1:iNumHeights)
                ELSEWHERE
                    rvValue(1:iNumHeights) = INVALID_VALUE
                ENDWHERE
            ELSE
                iNumHeights = 0
                RETURN
            END IF
        CASE(MODOS_D_WINDDIRECTION)
            IF(tRecord % lWindDirectionAvailable) THEN
                WHERE( &
                    IAND(tRecord % imError(1,1:iNumHeights),iExclude) == 0 .AND. &
                    IAND(tRecord % imError(2,1:iNumHeights),iExclude) == 0 &
                )
                    rvValue(1:iNumHeights) = tRecord % rvWindDirection(1:iNumHeights)
                ELSEWHERE
                    rvValue(1:iNumHeights) = INVALID_VALUE
                ENDWHERE
            ELSE
                iNumHeights = 0
                RETURN
            END IF
        CASE(MODOS_D_RADIALSIGMA)
            iNumHeights = 0
        CASE(MODOS_D_PHISIGMA)
            IF(tRecord % lPhiSigmaAvailable) THEN
                WHERE( &
                    IAND(tRecord % imError(1,1:iNumHeights),iExclude) == 0 .AND. &
                    IAND(tRecord % imError(2,1:iNumHeights),iExclude) == 0 .AND. &
                    IAND(tRecord % imError(3,1:iNumHeights),iExclude) == 0 &
                )
                    rvValue(1:iNumHeights) = tRecord % rvPhiSigma(1:iNumHeights)
                ELSEWHERE
                    rvValue(1:iNumHeights) = INVALID_VALUE
                ENDWHERE
            ELSE
                iNumHeights = 0
                RETURN
            END IF
        CASE(MODOS_D_DIFFUSIONCLASS)
            iNumHeights = 0
        CASE(MODOS_D_TEMPERATURE)
            IF(tRecord % lTemperatureAvailable) THEN
                WHERE(IAND(tRecord % imError(6,1:iNumHeights),iExclude) == 0)
                    rvValue(1:iNumHeights) = tRecord % rvTemperature(1:iNumHeights)
                ELSEWHERE
                    rvValue(1:iNumHeights) = INVALID_VALUE
                ENDWHERE
            ELSE
                iNumHeights = 0
                RETURN
            END IF
        CASE(MODOS_D_AVAILABILITY)
            iNumHeights = 0
        CASE(MODOS_D_SIGNALTONOISE)
            iNumHeights = 0
        CASE(MODOS_D_ERROR)
            iNumHeights = 0
        END SELECT
        
        ! Pack heights, and get their values
        iActualNumHeights = 0
        DO i=1,iNumHeights
            IF(rvValue(iNumHeights-i+1) > INVALID_VALUE + 10.) THEN
                iActualNumHeights = iNumHeights-i+1
                EXIT
            END IF
        END DO
        iNumHeights = iActualNumHeights
        rvHeight(1:iNumHeights) = tRecord % rvHeight(1:iNumHeights)

    END SUBROUTINE GetProfile


    ! Warning: this routine only applies to diffusion class
    SUBROUTINE GetProfileChar(tRecord, iExclude, iNumHeights, rvHeight, cvDiffusionClass)
    
        ! Routine arguments
        TYPE(ModosRecordType), INTENT(IN)                       :: tRecord
        INTEGER, INTENT(IN)                                     :: iExclude
        INTEGER, INTENT(OUT)                                    :: iNumHeights
        REAL, DIMENSION(MAX_VERTICAL_STEPS), INTENT(OUT)        :: rvHeight
        CHARACTER, DIMENSION(MAX_VERTICAL_STEPS), INTENT(OUT)   :: cvDiffusionClass
        
        ! Locals
        INTEGER :: i
        INTEGER :: iActualNumHeights

        ! Get all data.
        iNumHeights = tRecord % iNumHeights
        IF(tRecord % lDiffusionClassAvailable) THEN
            WHERE(IAND(tRecord % imError(3,1:iNumHeights),iExclude) == 0)
                cvDiffusionClass(1:iNumHeights) = tRecord % cvDiffusionClass(1:iNumHeights)
            ELSEWHERE
                cvDiffusionClass(1:iNumHeights) = "*"
            ENDWHERE
        ELSE
            iNumHeights = 0
            RETURN
        END IF

        ! Pack heights, and get their values
        iActualNumHeights = 0
        DO i=1,iNumHeights
            IF(INDEX("ABCDEF",cvDiffusionClass(iNumHeights-i+1)) /= 0) THEN
                iActualNumHeights = iNumHeights-i+1
                EXIT
            END IF
        END DO
        iNumHeights = iActualNumHeights
        rvHeight(1:iNumHeights) = tRecord % rvHeight(1:iNumHeights)

    END SUBROUTINE GetProfileChar


    ! Warning: the exclusion criterium, "iExclude", is ignored for
    !          availability and signal-to-noise data
    SUBROUTINE GetProfile1d(tRecord, iData, iIndex, iExclude, iNumHeights, rvHeight, rvValue)
    
        ! Routine arguments
        TYPE(ModosRecordType), INTENT(IN)                   :: tRecord
        INTEGER, INTENT(IN)                                 :: iData
        INTEGER, INTENT(IN)                                 :: iIndex
        INTEGER, INTENT(IN)                                 :: iExclude
        INTEGER, INTENT(OUT)                                :: iNumHeights
        REAL, DIMENSION(MAX_VERTICAL_STEPS), INTENT(OUT)    :: rvHeight
        REAL, DIMENSION(MAX_VERTICAL_STEPS), INTENT(OUT)    :: rvValue
        
        ! Locals
        INTEGER :: i
        INTEGER :: iActualNumHeights

        ! Check index to be in range
        IF(.NOT.(1<=iIndex .AND. iIndex<=MAX_BEAMS)) THEN
            iNumHeights = 0
            RETURN
        ELSEIF(iData==MODOS_D_VECTORWIND .AND. .NOT.(1<=iIndex .AND. iIndex<=MAX_COMPONENTS)) THEN
            iNumHeights = 0
            RETURN
        END IF
        
        ! Get all data matching no bits in exclusion criterium
        iNumHeights = tRecord % iNumHeights
        SELECT CASE(iData)
        CASE(MODOS_D_SPECTRUM)
            iNumHeights = 0
        CASE(MODOS_D_POWER)
            IF(tRecord % lvPowerAvailable(iIndex)) THEN
                WHERE(IAND(tRecord % imError(iIndex,1:iNumHeights),iExclude) == 0)
                    rvValue(1:iNumHeights) = tRecord % rmPower(iIndex, 1:iNumHeights)
                ELSEWHERE
                    rvValue(1:iNumHeights) = INVALID_VALUE
                ENDWHERE
            ELSE
                iNumHeights = 0
                RETURN
            END IF
        CASE(MODOS_D_REFLECTIVITY)
            IF(tRecord % lvReflectivityAvailable(iIndex)) THEN
                rvValue(1:iNumHeights) = tRecord % rmReflectivity(iIndex, 1:iNumHeights)
            ELSE
                iNumHeights = 0
                RETURN
            END IF
        CASE(MODOS_D_RADIALWIND)
            IF(tRecord % lvRadialWindAvailable(iIndex)) THEN
                WHERE(IAND(tRecord % imError(iIndex,1:iNumHeights),iExclude) == 0)
                    rvValue(1:iNumHeights) = tRecord % rmRadialWind(iIndex, 1:iNumHeights)
                ELSEWHERE
                    rvValue(1:iNumHeights) = INVALID_VALUE
                ENDWHERE
            ELSE
                iNumHeights = 0
                RETURN
            END IF
        CASE(MODOS_D_VECTORWIND)
            IF(tRecord % lvVectorWindAvailable(iIndex)) THEN
                DO i=1, iNumHeights
                    IF(VectorOr(tRecord % imError(:,i))) THEN
                        rvValue(i) = tRecord % rmVectorWind(iIndex, i)
                    ELSE
                        rvValue(i) = INVALID_VALUE
                    END IF
                END DO
            ELSE
                iNumHeights = 0
                RETURN
            END IF
        CASE(MODOS_D_WINDSPEED)
            iNumHeights = 0
        CASE(MODOS_D_WINDDIRECTION)
            iNumHeights = 0
        CASE(MODOS_D_RADIALSIGMA)
            IF(tRecord % lvRadialSigmaAvailable(iIndex)) THEN
                WHERE(IAND(tRecord % imError(iIndex,1:iNumHeights),iExclude) == 0)
                    rvValue(1:iNumHeights) = tRecord % rmRadialSigma(iIndex, 1:iNumHeights)
                ELSEWHERE
                    rvValue(1:iNumHeights) = INVALID_VALUE
                ENDWHERE
            ELSE
                iNumHeights = 0
                RETURN
            END IF
        CASE(MODOS_D_PHISIGMA)
            iNumHeights = 0
        CASE(MODOS_D_DIFFUSIONCLASS)
            iNumHeights = 0
        CASE(MODOS_D_TEMPERATURE)
            iNumHeights = 0
        CASE(MODOS_D_AVAILABILITY)
            IF(tRecord % lvAvailabilityAvailable(iIndex)) THEN
                rvValue(1:iNumHeights) = tRecord % rmAvailability(iIndex, 1:iNumHeights)
            ELSE
                iNumHeights = 0
                RETURN
            END IF
        CASE(MODOS_D_SIGNALTONOISE)
            IF(tRecord % lvSignalToNoiseAvailable(iIndex)) THEN
                rvValue(1:iNumHeights) = tRecord % rmSignalToNoise(iIndex, 1:iNumHeights)
            ELSE
                iNumHeights = 0
                RETURN
            END IF
        CASE(MODOS_D_ERROR)
            iNumHeights = 0
        END SELECT
        
        ! Pack heights, and get their values
        iActualNumHeights = 0
        DO i=1,iNumHeights
            IF(rvValue(iNumHeights-i+1) > INVALID_VALUE + 10.) THEN
                iActualNumHeights = iNumHeights-i+1
                EXIT
            END IF
        END DO
        iNumHeights = iActualNumHeights
        rvHeight(1:iNumHeights) = tRecord % rvHeight(1:iNumHeights)

    END SUBROUTINE GetProfile1d


    ! Warning: this routine only applies to errors
    SUBROUTINE GetProfile1dInt(tRecord, iBeamIndex, iNumHeights, rvHeight, ivError)
    
        ! Routine arguments
        TYPE(ModosRecordType), INTENT(IN)                   :: tRecord
        INTEGER, INTENT(IN)                                 :: iBeamIndex
        INTEGER, INTENT(OUT)                                :: iNumHeights
        REAL, DIMENSION(MAX_VERTICAL_STEPS), INTENT(OUT)    :: rvHeight
        INTEGER, DIMENSION(MAX_VERTICAL_STEPS), INTENT(OUT) :: ivError
        
        ! Locals
        ! -none-

        ! Check index to be in range
        IF(.NOT.(1<=iBeamIndex .AND. iBeamIndex<=MAX_BEAMS)) THEN
            iNumHeights = 0
            RETURN
        END IF
        
        ! Get all data.
        iNumHeights = tRecord % iNumHeights
        IF(tRecord % lvErrorAvailable(iBeamIndex)) THEN
            ivError = tRecord % imError(iBeamIndex, :)
        ELSE
            iNumHeights = 0
            RETURN
        END IF
        rvHeight = tRecord % rvHeight

    END SUBROUTINE GetProfile1dInt


    ! Warning: this routine only applies to spectrum.
    SUBROUTINE GetProfile2d(tRecord, iBeamIndex, iNumHeights, rvHeight, rmSpectrum)
    
        ! Routine arguments
        TYPE(ModosRecordType), INTENT(IN)                                   :: tRecord
        INTEGER, INTENT(IN)                                                 :: iBeamIndex
        INTEGER, INTENT(OUT)                                                :: iNumHeights
        REAL, DIMENSION(MAX_VERTICAL_STEPS), INTENT(OUT)                    :: rvHeight
        REAL, DIMENSION(NUM_SPECTRAL_ITEMS,MAX_VERTICAL_STEPS), INTENT(OUT) :: rmSpectrum
        
        ! Locals
        ! -none-

        ! Check indices to be in range
        IF(.NOT.(1<=iBeamIndex .AND. iBeamIndex<=MAX_BEAMS)) THEN
            iNumHeights = 0
            RETURN
        END IF
        
        ! Get all data. No exclusion criterium is used for spectrum.
        IF(ANY(tRecord % lmSpectrumAvailable(iBeamIndex,:))) THEN
            iNumHeights = tRecord % iNumHeights
            rmSpectrum = tRecord % raSpectrum(iBeamIndex,:,:)
        ELSE
            iNumHeights = 0
        END IF
        rvHeight = tRecord % rvHeight

    END SUBROUTINE GetProfile2d
    

    FUNCTION ModosGetLimitHeight(tRecord, iExclude) RESULT(rLimitHeight)
    
        ! Routine arguments
        TYPE(ModosRecordType), INTENT(IN)   :: tRecord
        INTEGER, INTENT(IN)                 :: iExclude
        REAL                                :: rLimitHeight
        
        ! Locals
        INTEGER                     :: i
        INTEGER, DIMENSION(40)      :: ivError
        LOGICAL, DIMENSION(40)      :: lvIncluded
        INTEGER                     :: iNumHeights
        INTEGER                     :: iHeight
        REAL, DIMENSION(MAX_BEAMS)  :: rvTentativeHeight
        INTEGER                     :: iBeam
        
        ! Get the tentative limit heights, defined as the uppermost
        ! height not satisfying the exclusion criteria per each beam
        iBeam = 0
        DO i = 1, MAX_BEAMS
            IF(tRecord % lvErrorAvailable(i)) THEN
                iBeam = iBeam + 1
                iNumHeights = tRecord % iNumHeights
                ivError = tRecord % imError(i, :)
                lvIncluded(1:iNumHeights) = (IAND(ivError(1:iNumHeights), iExclude) == 0)
                rvTentativeHeight(iBeam) = 0.
                DO iHeight = iNumHeights, 1, -1
                    IF(lvIncluded(iHeight)) THEN
                        rvTentativeHeight(iBeam) = tRecord%rvHeight(iHeight)
                        EXIT
                    END IF
                END DO
            END IF
        END DO
        IF(iBeam == 0) THEN
            rLimitHeight = 0.
        END IF
        
        ! The actual limit height is the minimum tentative limit height
        rLimitHeight = MINVAL(rvTentativeHeight(1:iBeam))
        
    END FUNCTION ModosGetLimitHeight
    
    
    SUBROUTINE ModosAnalyzeError(iError, iNumErrors, svError)
    
        ! Routine arguments
        INTEGER, INTENT(IN)                             :: iError
        INTEGER, INTENT(OUT)                            :: iNumErrors
        CHARACTER(LEN=74), DIMENSION(12), INTENT(OUT)   :: svError
        
        ! Locals
        CHARACTER(LEN=74), DIMENSION(0:11), PARAMETER    :: svErrorNames = (/ &
            "Receiver saturated for at least one sample                                ", &
            "High level of white noise for at least one sample                         ", &
            "Local maximum in spectra too strong                                       ", &
            "Reserved                                                                  ", &
            "Signal to Noise Ratio too low                                             ", &
            "Low statistical significance of the measured backscattered acoustic signal", &
            "Low statistical significance of the measured ambient noise                ", &
            "Reserved                                                                  ", &
            "Maximum of averaged signal spectrum too slim respect to theory            ", &
            "Maximum of averaged signal spectrum too wide                              ", &
            "Maximum of averaged signal spectrum smaller than maximum noise spectrum   ", &
            "Standard deviation from width of signal spectrum has an imaginary value   " &
        /)
        INTEGER :: i
        INTEGER :: iOne = 1
        
        ! Check all error bits in order, and add a description for
        ! any bit set
        iNumErrors = 0
        DO i=0, 11
            IF(IAND(LSHIFT(iOne,i),iError) /= 0) THEN
                iNumErrors = iNumErrors + 1
                svError(iNumErrors) = svErrorNames(i)
            END IF
        END DO
        
    END SUBROUTINE ModosAnalyzeError


    SUBROUTINE ModosUpdateErrorCounter(iNumHeights, imError, lvPresentBeam, iErrorCode, ivCount, ivTotal)

		! Routine arguments
		INTEGER, INTENT(IN)										:: iNumHeights
		INTEGER, DIMENSION(iNumHeights,MAX_BEAMS), INTENT(IN)	:: imError
		LOGICAL, DIMENSION(MAX_BEAMS), INTENT(IN)				:: lvPresentBeam
		INTEGER, INTENT(IN)										:: iErrorCode
		INTEGER, DIMENSION(iNumHeights), INTENT(INOUT)			:: ivCount
		INTEGER, DIMENSION(iNumHeights), INTENT(INOUT)			:: ivTotal
		
		! Locals
		INTEGER :: iBeam
		INTEGER	:: iHeight

		! Update counters whenever an error code matches the input code
		DO iBeam = 1, MAX_BEAMS
			IF(lvPresentBeam(iBeam)) THEN
				DO iHeight=1,iNumHeights
					ivTotal(iHeight) = ivTotal(iHeight) + 1
					IF(IAND(imError(iHeight,iBeam),iErrorCode)/=0) THEN
						ivCount(iHeight) = ivCount(iHeight) + 1
					END IF
				END DO
			END IF
		END DO

	END SUBROUTINE ModosUpdateErrorCounter

	! *********************
    ! * Internal routines *
    ! *********************
    
    SUBROUTINE Clean(tRecord)
    
        ! Routine arguments
        TYPE(ModosRecordType), INTENT(INOUT)    :: tRecord

        ! Locals
        ! -none-
        
        ! Clean data record
        tRecord % iYear   = 0
        tRecord % iMonth  = 0
        tRecord % iDay    = 0
        tRecord % iHour   = 0
        tRecord % iMinute = 0
        tRecord % iSecond = 0
        tRecord % iAverage = 0
        tRecord % rMinHeight = 0.0
        tRecord % rMaxHeight = 0.0
        tRecord % rNoiseHeight = 0.0
        tRecord % rDeltaHeight = 0.0
        tRecord % iNumVolumes = 0
        tRecord % ivVolume = 0
        tRecord % iNumFrequencies = 0
        tRecord % rSodarFrequency = 0.0
        tRecord % rRassFrequency = 0.0
        tRecord % rvMixingFrequency = 0.0
        tRecord % rSodarSamplingFrequency = 0.0
        tRecord % rRassSamplingFrequency = 0.0
        tRecord % iNumBeams = 0
        tRecord % rvAzimuth = 0.0
        tRecord % rvZenith = 0.0
        tRecord % rRefTemperature = 0.0
        tRecord % rCrosstalk = 0.0
        tRecord % iNumHeights = 0
        tRecord % rLowerNoiseHeight = 0.0
        tRecord % rUpperNoiseHeight = 0.0
        tRecord % lmSpectrumAvailable = .FALSE.
        tRecord % lvReflectivityAvailable = .FALSE.
        tRecord % lvRadialWindAvailable = .FALSE.
        tRecord % lvVectorWindAvailable = .FALSE.
        tRecord % lWindSpeedAvailable = .FALSE.
        tRecord % lWindDirectionAvailable = .FALSE.
        tRecord % lvRadialSigmaAvailable = .FALSE.
        tRecord % lPhiSigmaAvailable = .FALSE.
        tRecord % lDiffusionClassAvailable = .FALSE.
        tRecord % lTemperatureAvailable = .FALSE.
        tRecord % lvAvailabilityAvailable = .FALSE.
        tRecord % lvSignalToNoiseAvailable = .FALSE.
        tRecord % lvErrorAvailable = .FALSE.
        tRecord % imError = 0
        
    END SUBROUTINE Clean
    
    
    SUBROUTINE ParseHeader(sBuffer, tRecord)
    
        ! Routine arguments
        CHARACTER(LEN=*), INTENT(IN)            :: sBuffer
        TYPE(ModosRecordType), INTENT(INOUT)    :: tRecord
        
        ! Locals
        CHARACTER(LEN=3), DIMENSION(15), PARAMETER  :: svItem = (/ &
            "AVE", "MIN", "MAX", "NOI", "STP", &
            "VOL", "XMT", "MIX", "SMP", "AZI", &
            "ZEN", "TMP", "FEC", "DST", "XTL"  &
        /)
        INTEGER, DIMENSION(15)                      :: ivItemPos
        INTEGER                                     :: iRetCode
        INTEGER                                     :: iFirstPos
        INTEGER                                     :: i

        ! Get time stamp
        READ(sBuffer(5:16),"(6i2)") &
            tRecord % iYear, &
            tRecord % iMonth, &
            tRecord % iDay, &
            tRecord % iHour, &
            tRecord % iMinute, &
            tRecord % iSecond

        ! Get positions of header items, and check they are all present and
        ! in increasing order.
        DO i=1,SIZE(svItem)
            ivItemPos(i) = INDEX(sBuffer,svItem(i))
        END DO
        DO i=1,SIZE(svItem)
            IF(ivItemPos(i) <= 0) THEN
                sHeader = ' '
                iRetCode = MODOS_IVHEADER
                RETURN
            END IF
        END DO
        DO i=2,SIZE(svItem)
            IF(ivItemPos(i-1) >= ivItemPos(i)) THEN
                sHeader = ' '
                iRetCode = MODOS_IVHEADER
                RETURN
            END IF
        END DO
        
        ! Get averaging period (s)
        READ(sBuffer(ivItemPos(1)+3:ivItemPos(2)-2), *, IOSTAT=iRetCode) tRecord % iAverage
        
        ! Get minimum and maximum heights
        tRecord % rMinHeight = ReadSingle(sBuffer(ivItemPos(2)+3:ivItemPos(3)-2))
        tRecord % rMaxHeight = ReadSingle(sBuffer(ivItemPos(3)+3:ivItemPos(4)-2))

        ! Get noise timing reference height
        tRecord % rNoiseHeight = ReadSingle(sBuffer(ivItemPos(4)+3:ivItemPos(5)-2))
        
        ! Get step
        tRecord % rDeltaHeight = ReadSingle(sBuffer(ivItemPos(5)+3:ivItemPos(6)-2))
        
        ! Get pulse volumes, expressed as 12-bit unsigned integers (4095 = 100%)
        tRecord % iNumVolumes = (ivItemPos(7)-ivItemPos(6))/6
        iFirstPos = ivItemPos(6)+3
        DO i=1, tRecord % iNumVolumes
            READ(sBuffer(iFirstPos:iFirstPos+5),*) tRecord % ivVolume(i)
            iFirstPos = iFirstPos + 6
        END DO
        
        ! Get nominal transmission frequencies (Hz)
        tRecord % iNumFrequencies = (ivItemPos(8)-ivItemPos(7))/6
        IF(tRecord % iNumFrequencies == 1) THEN
            ! SODAR only
            tRecord % rSodarFrequency = ReadSingle(sBuffer(ivItemPos(7)+3:ivItemPos(8)-2))
            tRecord % rRassFrequency  = 0
        ELSE
            ! SODAR + RASS
            iFirstPos = ivItemPos(7)+3
            tRecord % rSodarFrequency = ReadSingle(sBuffer(iFirstPos:iFirstPos+5))
            iFirstPos = iFirstPos + 6
            tRecord % rRassFrequency  = ReadSingle(sBuffer(iFirstPos:iFirstPos+5))
        END IF

        ! Get mixing frequencies
        iFirstPos = ivItemPos(8)+3
        DO i=1, tRecord % iNumVolumes
            tRecord % rvMixingFrequency(i) = ReadSingle(sBuffer(iFirstPos:iFirstPos+5))
            iFirstPos = iFirstPos + 6
        END DO
        
        ! Get sampling frequencies (Hz)
        IF(tRecord % iNumFrequencies == 1) THEN
            ! SODAR only
            tRecord % rSodarSamplingFrequency = ReadSingle(sBuffer(ivItemPos(9)+3:ivItemPos(10)-2))
            tRecord % rRassSamplingFrequency  = 0
        ELSE
            ! SODAR + RASS
            iFirstPos = ivItemPos(9)+3
            tRecord % rSodarSamplingFrequency = ReadSingle(sBuffer(iFirstPos:iFirstPos+5))
            iFirstPos = iFirstPos + 6
            tRecord % rRassSamplingFrequency  = ReadSingle(sBuffer(iFirstPos:iFirstPos+5))
        END IF

        ! Get azimuths and zeniths
        tRecord % iNumBeams = (ivItemPos(11)-ivItemPos(10))/6
        iFirstPos = ivItemPos(10)+3
        DO i=1, tRecord % iNumBeams
            tRecord % rvAzimuth(i) = ReadSingle(sBuffer(iFirstPos:iFirstPos+5))
            iFirstPos = iFirstPos + 6
        END DO
        iFirstPos = ivItemPos(11)+3
        DO i=1, tRecord % iNumBeams
            tRecord % rvZenith(i) = ReadSingle(sBuffer(iFirstPos:iFirstPos+5))
            iFirstPos = iFirstPos + 6
        END DO
        
        ! Get reference thermocouple temperature
        tRecord % rRefTemperature = ReadSingle(sBuffer(ivItemPos(12)+3:ivItemPos(13)-2))
        
        ! Get crosstalk
        tRecord % rCrosstalk = ReadSingle(sBuffer(ivItemPos(15)+3:ivItemPos(15)+8))
        
    END SUBROUTINE ParseHeader

    
    SUBROUTINE ParseDataLine(sBuffer, tRecord)
    
        ! Routine arguments
        CHARACTER(LEN=*), INTENT(IN)                :: sBuffer
        TYPE(ModosRecordType), INTENT(INOUT)        :: tRecord
        
        ! Locals
        CHARACTER(LEN=3), DIMENSION(15), PARAMETER  :: svRecordType = (/ &
            "H  ", "F  ", "P  ", "R  ", "VR ", &
            "VV ", "V  ", "D  ", "S  ", "SD ", &
            "DC ", "TMP", "DA ", "SN ", "ER "  &
        /)
        INTEGER, DIMENSION(15), PARAMETER           :: ivRecordTypeLength = (/ &
            1, 1, 2, 2, 2, 2, 3, 3, 2, 2, 2, 3, 2, 2, 2  &
        /)
        INTEGER, DIMENSION(15), PARAMETER           :: ivIndexTypeLength = (/ &
            0, 2, 1, 1, 1, 3, 0, 0, 1, 0, 4, 0, 1, 1, 5  &
        /)
        CHARACTER(LEN=*), PARAMETER                 :: sBeamCode = "12345R"
        CHARACTER(LEN=*), PARAMETER                 :: sSpectrumCode = "0123456789ABCDEFGHIJKLMNOPQRSTUV"
        CHARACTER(LEN=*), PARAMETER                 :: sComponentCode = "UVW"
        REAL, DIMENSION(MAX_VERTICAL_STEPS + 2)     :: rvValues
        CHARACTER, DIMENSION(MAX_VERTICAL_STEPS)    :: cvValues
        INTEGER, DIMENSION(MAX_VERTICAL_STEPS)      :: ivValues
        INTEGER :: iNumValues
        INTEGER :: iBeamIndex
        INTEGER :: iSpectrumIndex
        INTEGER :: iComponentIndex
        INTEGER :: i
        
        ! Classify it based on its first characters.
        DO i=1, SIZE(svRecordType)
            IF(sBuffer(1:ivRecordTypeLength(i))==svRecordType(i)(1:ivRecordTypeLength(i))) THEN
                SELECT CASE(ivIndexTypeLength(i))
                CASE(0)
                    ! No indices, real values
                    CALL ReadVectorSingle(sBuffer(4:),rvValues,iNumValues)
                    SELECT CASE(i)
                    CASE(1)
                        ! Height
                        tRecord % iNumHeights = MAX(iNumValues-2, 0)
                        IF(tRecord % iNumHeights > 0) THEN
                            tRecord % rvHeight(1:iNumValues-2) = rvValues(1:iNumValues-2)
                            tRecord % rLowerNoiseHeight = rvValues(iNumValues-1)
                            tRecord % rUpperNoiseHeight = rvValues(iNumValues)
                        END IF
                    CASE(7)
                        ! Wind speed
                        tRecord % iNumWindSpeed = MAX(iNumValues, 0)
                        IF(tRecord % iNumWindSpeed > 0) THEN
                            tRecord % rvWindSpeed(1:iNumValues) = rvValues(1:iNumValues)
                            tRecord % lWindSpeedAvailable = .TRUE.
                        END IF
                    CASE(8)
                        ! Wind direction
                        tRecord % iNumWindDirection = MAX(iNumValues, 0)
                        IF(tRecord % iNumWindDirection > 0) THEN
                            tRecord % rvWindDirection(1:iNumValues) = rvValues(1:iNumValues)
                            tRecord % lWindDirectionAvailable = .TRUE.
                        END IF
                    CASE(10)
                        ! Phi Sigma
                        tRecord % iNumPhiSigma = MAX(iNumValues, 0)
                        IF(tRecord % iNumPhiSigma > 0) THEN
                            tRecord % rvPhiSigma(1:iNumValues) = rvValues(1:iNumValues)
                            tRecord % lPhiSigmaAvailable = .TRUE.
                        END IF
                    CASE(12)
                        ! Temperature
                        tRecord % iNumTemperature = MAX(iNumValues, 0)
                        IF(tRecord % iNumTemperature > 0) THEN
                            tRecord % rvTemperature(1:iNumValues) = rvValues(1:iNumValues)
                            tRecord % lTemperatureAvailable = .TRUE.
                        END IF
                    END SELECT
                CASE(1)
                    ! Beam index on third character, real values
                    CALL ReadVectorSingle(sBuffer(4:),rvValues,iNumValues)
                    iBeamIndex = INDEX(sBeamCode,sBuffer(3:3))
                    IF(iBeamIndex > 0) THEN
                        SELECT CASE(i)
                        CASE(3)
                            ! Power
                            tRecord % ivNumPower(iBeamIndex) = MAX(iNumValues, 0)
                            IF(tRecord % ivNumPower(iBeamIndex) > 0) THEN
                                tRecord % rmPower(iBeamIndex,1:iNumValues) = rvValues(1:iNumValues)
                                tRecord % lvPowerAvailable(iBeamIndex) = .TRUE.
                            END IF
                        CASE(4)
                            ! Reflectivity
                            tRecord % ivNumReflectivity(iBeamIndex) = MAX(iNumValues, 0)
                            IF(tRecord % ivNumReflectivity(iBeamIndex) > 0) THEN
                                tRecord % rmReflectivity(iBeamIndex,1:iNumValues) = rvValues(1:iNumValues)
                                tRecord % lvReflectivityAvailable(iBeamIndex) = .TRUE.
                            END IF
                        CASE(5)
                            ! Radial velocity
                            tRecord % ivNumRadialWind(iBeamIndex) = MAX(iNumValues, 0)
                            IF(tRecord % ivNumRadialWind(iBeamIndex) > 0) THEN
                                tRecord % rmRadialWind(iBeamIndex,1:iNumValues) = rvValues(1:iNumValues)
                                tRecord % lvRadialWindAvailable(iBeamIndex) = .TRUE.
                            END IF
                        CASE(9)
                            ! Sigma of radial component
                            tRecord % ivNumRadialSigma(iBeamIndex) = MAX(iNumValues, 0)
                            IF(tRecord % ivNumRadialSigma(iBeamIndex) > 0) THEN
                                tRecord % rmRadialSigma(iBeamIndex,1:iNumValues) = rvValues(1:iNumValues)
                                tRecord % lvRadialSigmaAvailable(iBeamIndex) = .TRUE.
                            END IF
                        CASE(13)
                            ! Data availability
                            tRecord % ivNumAvailability(iBeamIndex) = MAX(iNumValues, 0)
                            IF(tRecord % ivNumAvailability(iBeamIndex) > 0) THEN
                                tRecord % rmAvailability(iBeamIndex,1:iNumValues) = rvValues(1:iNumValues)
                                tRecord % lvAvailabilityAvailable(iBeamIndex) = .TRUE.
                            END IF
                        CASE(14)
                            ! Signal to noise
                            tRecord % ivNumSignalToNoise(iBeamIndex) = MAX(iNumValues, 0)
                            IF(tRecord % ivNumSignalToNoise(iBeamIndex) > 0) THEN
                                tRecord % rmSignalToNoise(iBeamIndex,1:iNumValues) = rvValues(1:iNumValues)
                                tRecord % lvSignalToNoiseAvailable(iBeamIndex) = .TRUE.
                            END IF
                        END SELECT
                    END IF
                CASE(2)
                    ! Spectrum index on second character
                    ! Beam index on third character
                    ! Real values: Spectrum
                    CALL ReadVectorSingle(sBuffer(4:),rvValues,iNumValues)
                    iSpectrumIndex = INDEX(sSpectrumCode,sBuffer(2:2))
                    iBeamIndex = INDEX(sBeamCode,sBuffer(3:3))
                    IF(iSpectrumIndex > 0 .AND. iBeamIndex > 0) THEN
                        tRecord % imNumSpectrum(iBeamIndex,iSpectrumIndex) = MAX(iNumValues, 0)
                        IF(tRecord % imNumSpectrum(iBeamIndex,iSpectrumIndex) > 0) THEN
                            tRecord % raSpectrum(iBeamIndex,iSpectrumIndex,1:iNumValues) = rvValues(1:iNumValues)
                            tRecord % lmSpectrumAvailable(iBeamIndex,iSpectrumIndex) = .TRUE.
                        END IF
                    END IF
                CASE(3)
                    ! Component index on third character, real values:
                    ! Vector wind
                    CALL ReadVectorSingle(sBuffer(4:),rvValues,iNumValues)
                    iComponentIndex = INDEX(sComponentCode,sBuffer(3:3))
                    IF(iComponentIndex > 0) THEN
                        tRecord % ivNumVectorWind(iComponentIndex) = MAX(iNumValues, 0)
                        IF(tRecord % ivNumVectorWind(iComponentIndex) > 0) THEN
                            tRecord % rmVectorWind(iComponentIndex,1:iNumValues) = rvValues(1:iNumValues)
                            tRecord % lvVectorWindAvailable(iComponentIndex) = .TRUE.
                        END IF
                    END IF
                CASE(4)
                    ! Character values: Diffusion class
                    CALL ReadVectorCharacter(sBuffer(4:),cvValues,iNumValues)
                    tRecord % iNumDiffusionClass = MAX(iNumValues, 0)
                    IF(tRecord % iNumDiffusionClass > 0) THEN
                        tRecord % cvDiffusionClass(1:iNumValues) = cvValues(1:iNumValues)
                        tRecord % lDiffusionClassAvailable = .TRUE.
                    END IF
                CASE(5)
                    ! Beam index on third character, integer values:
                    ! Error code
                    CALL ReadVectorInteger(sBuffer(4:),ivValues,iNumValues)
                    iBeamIndex = INDEX(sBeamCode,sBuffer(3:3))
                    IF(iBeamIndex > 0) THEN
                        tRecord % ivNumError(iBeamIndex) = MAX(iNumValues, 0)
                        IF(tRecord % ivNumError(iBeamIndex) > 0) THEN
                            tRecord % imError(iBeamIndex,1:iNumValues) = ivValues(1:iNumValues)
                            tRecord % lvErrorAvailable(iBeamIndex) = .TRUE.
                        END IF
                    END IF
                END SELECT
            END IF
        END DO
        
    END SUBROUTINE ParseDataLine
    
    
    FUNCTION ReadSingle(sString) RESULT(rValue)
    
        ! Routine arguments
        CHARACTER(LEN=*), INTENT(IN)    :: sString
        REAL                            :: rValue
        
        ! Locals
        INTEGER :: iValue
        INTEGER :: iRetCode
        
        ! Discriminate the input number between integer and floating
        ! point, and get it with a free format.
        IF(LEN_TRIM(sString) > 0) THEN
            IF(INDEX(sString,'.') > 0) THEN
                READ(sString, *, IOSTAT=iRetCode) rValue
            ELSE
                READ(sString, *, IOSTAT=iRetCode) iValue
                rValue = iValue
            END IF
            IF(iRetCode /= 0) rValue = INVALID_VALUE
        ELSE
            rValue = INVALID_VALUE
        END IF
        
    END FUNCTION ReadSingle
    
    
    SUBROUTINE ReadVectorSingle(sString, rvValues, iNumValues)
    
        ! Routine arguments
        CHARACTER(LEN=*), INTENT(IN)    :: sString
        REAL, DIMENSION(:), INTENT(OUT) :: rvValues
        INTEGER, INTENT(OUT)            :: iNumValues
        
        ! Locals
        INTEGER :: i
        
        ! Decode Values
        iNumValues = LEN_TRIM(sString) / 6
        rvValues = INVALID_VALUE
        DO i=1, iNumValues
            rvValues(i) = ReadSingle(sString(1+(i-1)*6:i*6))
        END DO
        
    END SUBROUTINE ReadVectorSingle

    
    SUBROUTINE ReadVectorCharacter(sString, cvValues, iNumValues)
    
        ! Routine arguments
        CHARACTER(LEN=*), INTENT(IN)            :: sString
        CHARACTER, DIMENSION(:), INTENT(OUT)    :: cvValues
        INTEGER, INTENT(OUT)                    :: iNumValues
        
        ! Locals
        INTEGER :: i
        
        ! Decode Values
        iNumValues = LEN_TRIM(sString) / 6
        cvValues = " "
        DO i=1, iNumValues
            cvValues(i) = sString(i*6:i*6)
        END DO
        
    END SUBROUTINE ReadVectorCharacter

    
    SUBROUTINE ReadVectorInteger(sString, ivValues, iNumValues)
    
        ! Routine arguments
        CHARACTER(LEN=*), INTENT(IN)        :: sString
        INTEGER, DIMENSION(:), INTENT(OUT)  :: ivValues
        INTEGER, INTENT(OUT)                :: iNumValues
        
        ! Locals
        INTEGER :: iRetCode
        INTEGER :: i
        
        ! Decode Values
        iNumValues = LEN_TRIM(sString) / 6
        ivValues = 0
        DO i=1, iNumValues
            READ(sString(1+(i-1)*6:i*6),"(o6)",IOSTAT=iRetCode) ivValues(i)
            IF(iRetCode /= 0) ivValues(i) = -9999
        END DO
        
    END SUBROUTINE ReadVectorInteger
    
    
    FUNCTION VectorOr(ivValues) RESULT(iResultOR)
    
        ! Routine arguments
        INTEGER, DIMENSION(:), INTENT(IN)   :: ivValues
        INTEGER                             :: iResultOR
        
        ! Locals
        INTEGER :: i
        
        ! Compute the overall .OR.
        iResultOR = 0
        DO i=1, SIZE(ivValues)
            iResultOR = IOR(iResultOR, ivValues(i))
        END DO
        
    END FUNCTION VectorOr

END MODULE MODOS




