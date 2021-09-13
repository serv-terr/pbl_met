! model_meteo - Fortran module incorporating knowledge of Calpuff meteo formats
!
! Copyright 2021 by Patrizia Favaron
!
! This is open-source software, covered by the MIT license
!
module model_meteo

    use pbl_met
    use DataSet
    
    implicit none

    private

    ! Public interface
    public  :: calpuff

contains

    function calpuff(sFilePrefix, tMetPro, iDeltaTime, rZr, rZ0) result(iRetCode)
	
        ! Routine arguments
        character(len=*), intent(in)    :: sFilePrefix
        type(MetProDataSet), intent(in) :: tMetPro
        integer, intent(in)             :: iDeltaTime
        real, intent(in)                :: rZr
        real, intent(in)                :: rZ0
        integer                         :: iRetCode
		
        ! Locals
        integer             :: iErrCode
        character(len=256)  :: sFileName_Srf_6
        character(len=256)  :: sFileName_Srf_7
        character(len=256)  :: sFileName_Srf_M
        character(len=256)  :: sFileName_Upr_6
        character(len=256)  :: sFileName_Upr_7
        character(len=256)  :: sFileName_Upr_M
        integer             :: iLUN1
        integer             :: iLUN2
        integer             :: iLUN3
        integer             :: i, j, k, n
        character(len=256)  :: sLineRaw
        character(len=256)  :: sLineOut
        integer             :: iYearFrom, iMonthFrom, iDayFrom, iHourFrom, iMinuteFrom, iSecondFrom
        integer             :: iYearTo, iMonthTo, iDayTo, iHourTo, iMinuteTo, iSecondTo
        integer             :: iYear, iMonth, iDay, iHour, iMinute, iSecond
        integer             :: iJday
        integer             :: iJdayFrom
        integer             :: iJdayTo
        integer             :: iPrecCode
        integer             :: iCeiling
        real                :: rNewVel, rNewDir, rNewTemp
        real                :: rPmin
        real                :: rPlimit
        real                :: rL, rZi
        real, dimension(2)  :: cartesian
        real, dimension(2)  :: polar
        type(DateTime)      :: tDateTime
        
        ! Constants
        real, dimension(4), parameter	:: P_ALLOWED = [850., 700., 500., 100.]
		
        ! Assume success (will falsify on failure)
        iRetCode = 0
        
        rPmin = minval(abs(tMetPro % Pa))
        rPlimit = 0.
        n = size(tMetPro % Z)
        do i = 1, SIZE(P_ALLOWED)
            if(P_ALLOWED(i) < rPmin) then
                rPlimit = P_ALLOWED(i)
                exit
            end if
        end do
        if(rPlimit <= 0.) then
            iRetCode = 1
            return
        end if
        
        ! Generate file names
        write(sFileName_Srf_6, "(a,'_Calpuff6.srf')") trim(sFilePrefix)
        write(sFileName_Srf_7, "(a,'_Calpuff7.srf')") trim(sFilePrefix)
        write(sFileName_Srf_M, "(a,'_Calmet.srf')") trim(sFilePrefix)
        write(sFileName_Upr_6, "(a,'_Calpuff6.upr')") trim(sFilePrefix)
        write(sFileName_Upr_7, "(a,'_Calpuff7.upr')") trim(sFilePrefix)
        write(sFileName_Upr_M, "(a,'_Calmet.upr')") trim(sFilePrefix)
		
        ! Write surface data in CTDM sub-hourly form
        open(newunit=iLUN1, file=sFileName_Srf_6, status='unknown', action='write')
        open(newunit=iLUN2, file=sFileName_Srf_7, status='unknown', action='write')
        open(newunit=iLUN3, file=sFileName_Srf_M, status='unknown', action='write')
      	
        ! Header
        iErrCode    = tDateTime % fromEpoch(tMetPro % time_stamp_begin(1))
        iYearFrom   = tDateTime % iYear
        iMonthFrom  = tDateTime % iMonth
        iDayFrom    = tDateTime % iDay
        iHourFrom   = tDateTime % iHour
        iMinuteFrom = tDateTime % iMinute
        iSecondFrom = tDateTime % rSecond
        iJDayFrom   = DoY(iYearFrom, iMonthFrom, iDayFrom)
        iErrCode    = tDateTime % fromEpoch(tMetPro % time_stamp_end(1))
        iYearTo     = tDateTime % iYear
        iMonthTo    = tDateTime % iMonth
        iDayTo      = tDateTime % iDay
        iHourTo     = tDateTime % iHour
        iMinuteTo   = tDateTime % iMinute
        iSecondTo   = tDateTime % rSecond
        iJDayTo     = DoY(iYearTo, iMonthTo, iDayTo)
        ! -1- Calpuff 6
        write(iLUN1,"('SURFACE.DAT     2.1             Processed data')")
        write(iLUN1,"('   1')")
        write(iLUN1,"('Prepared by SHAKEUP')")
        write(iLUN1,"('NONE')")
        write(iLUN1,"(i4,1x,i2,1x,i2,1x,i2,1x,i4,1x,i4,1x,i2,1x,i2,1x,i2,1x,i4)") &
            iYearFrom, iMonthFrom, iDayFrom, iHourFrom+1, 60*iMinuteFrom+iSecondFrom, &
            iYearTo, iMonthTo, iDayTo, iHourTo+1, 60*iMinuteTo+iSecondTo + iDeltaTime
        ! -1- Calpuff 7
        write(iLUN2,"('SURFACE.DAT     2.1             Processed data')")
        write(iLUN2,"('   1')")
        write(iLUN2,"('Prepared by SHAKEUP')")
        write(iLUN2,"('NONE')")
        write(iLUN2,"('UTC+0100')")
        write(iLUN2,"(i4,1x,i2,1x,i2,1x,i2,1x,i4,1x,i4,1x,i2,1x,i2,1x,i2,1x,i4)") &
            iYearFrom, iMonthFrom, iDayFrom, iHourFrom+1, 60*iMinuteFrom+iSecondFrom, &
            iYearTo, iMonthTo, iDayTo, iHourTo+1, 60*iMinuteTo+iSecondTo + iDeltaTime
        ! -1- Calmet
        write(iLUN3,"('SURF.DAT        2.1             Hour Start and End Times with Seconds')")
        write(iLUN3,"('   1')")
        write(iLUN3,"('Produced by SHAKEUP')")
        write(iLUN3,"('NONE')")
        write(iLUN3,"('UTC+0000')")
        write(iLUN3, "(9i5)") &
            iYearFrom, iJDayFrom, iHourFrom, iMinuteFrom*60+iSecondFrom, &
            iYearTo, iJDayTo, iHourTo, iMinuteTo*60+iSecondTo + tMetPro % iDeltaTime, &
            1		! Last value is number of stations
        write(iLUN3,"(i8)") 0	! This is station ID, assumed 0 on single run
			
        ! Data
        do i = 1, SIZE(tMetPro % time_stamp_begin)
      	
            ! Current date and time
            iErrCode = tDateTime % fromEpoch(tMetPro % time_stamp_end(1))
            iYear    = tDateTime % iYear
            iMonth   = tDateTime % iMonth
            iDay     = tDateTime % iDay
            iHour    = tDateTime % iHour
            iMinute  = tDateTime % iMinute
            iSecond  = tDateTime % rSecond
            iJday = DoY(iYear, iMonth, iDay)

            ! Get Obukhov length
            rL = tMetPro % L(i)
			
            ! Adjust Zi
            rZi = min(max(tMetPro % Zi(i), 100.), 5000.0)
			
            ! Assign precipitation code
            if(tMetPro % Prec(i) > 0.) then
                iPrecCode = 1
            else
                iPrecCode = 0
            end if
			
            ! Write data
            if(tMetPro % Prec(i) > 0.) then
                iPrecCode = 1
            else
                iPrecCode = 0
            end if
            ! -1- Calpuff 6
            write(iLUN1,"(2(i4,1x,2(i2,1x),i3,1x,i2,1x,i4,1x),2(i4,1x),1x,f7.3,1x,e15.7,1x,e15.7,1x,i2,1x,f6.2,1x,f8.2,1x,i3)") &
                iYear, iMonth, iDay, iJday, iHour+1, 60*iMinute+iSecond, &
                iYear, iMonth, iDay, iJday, iHour+1, 60*iMinute+iSecond + iDeltaTime, &
                int(rZi), int(rZi), &
                max(tMetPro % ustar(i), 0.001), rL, &
                rZ0, iPrecCode, tMetPro % Prec(i), tMetPro % Rg(i), floor(tMetPro % RelH(i))
            ! -1- Calpuff 7
            write(iLUN2,"(2(i4,1x,2(i2,1x),i3,1x,i2,1x,i4,1x),2(i4,1x),1x,f7.3,1x,e15.7,1x,e15.7,1x,i2,1x,f6.2,1x,f8.2,1x,i3)") &
                iYear, iMonth, iDay, iJday, iHour+1, 60*iMinute+iSecond, &
                iYear, iMonth, iDay, iJday, iHour+1, 60*iMinute+iSecond + iDeltaTime, &
                int(rZi), int(rZi), &
                max(tMetPro % ustar(i), 0.001), rL, &
                rZ0, iPrecCode, tMetPro % Prec(i), tMetPro % Rg(i), floor(tMetPro % RelH(i))
            ! -1- Calmet
            write(iLUN3, "(8i5)") &
                iYear, iJDay, iHour, 60*iMinute + iSecond, iYear, iJDay, iHour, 60*iMinute + iSecond + tMetPro % iDeltaTime
                iCeiling = floor(tMetPro % Zi(i)/(0.3048*100.))
            write(iLUN3, "(2(1x,f8.3),2(1x,i4),1x,f8.3,1x,i4,1x,f8.3,1x,i3)") &
                tMetPro % Vel(i), tMetPro % Dir(i), iCeiling, &
                floor(10.*tMetPro % Cover(i)), tMetPro % Temp(i) + 273.15, &
                nint(tMetPro % RelH(i)), tMetPro % Pa(i), iPrecCode
				
        end do
        close(iLUN3)
        close(iLUN2)
        close(iLUN1)
      	
      	! Write profile data in CTDM sub-hourly form
        open(newunit=iLUN1, file=sFileName_Upr_6, status='unknown', action='write')
        open(newunit=iLUN2, file=sFileName_Upr_7, status='unknown', action='write')
        open(newunit=iLUN3, file=sFileName_Upr_M, status='unknown', action='write')
      	
      	! Header
        ! -1- Calpuff 6
        write(iLUN1,"('PROFILE.DAT     2.1             Processed data')")
        write(iLUN1,"('   1')")
        write(iLUN1,"('Prepared by SHAKEUP')")
        write(iLUN1,"('NONE')")
        write(iLUN1,"(i4,1x,i2,1x,i2,1x,i2,1x,i4,1x,i4,1x,i2,1x,i2,1x,i2,1x,i4)") &
            iYearFrom, iMonthFrom, iDayFrom, iHourFrom+1, 60*iMinuteFrom+iSecondFrom, &
            iYearTo, iMonthTo, iDayTo, iHourTo+1, 60*iMinuteTo+iSecondTo + iDeltaTime
        ! -1- Calpuff 7
        write(iLUN2,"('PROFILE.DAT     2.1             Processed data')")
        write(iLUN2,"('   1')")
        write(iLUN2,"('Prepared by SHAKEUP')")
        write(iLUN2,"('NONE')")
        write(iLUN2,"('UTC+0100')")
        write(iLUN2,"(i4,1x,i2,1x,i2,1x,i2,1x,i4,1x,i4,1x,i2,1x,i2,1x,i2,1x,i4)") &
            iYearFrom, iMonthFrom, iDayFrom, iHourFrom+1, 60*iMinuteFrom+iSecondFrom, &
            iYearTo, iMonthTo, iDayTo, iHourTo+1, 60*iMinuteTo+iSecondTo + iDeltaTime
        ! CalMet
        write(iLUN3, "('UP.DAT          2.1             Hour Start and End Times with Seconds')")
        write(iLUN3,"('   1')")
        write(iLUN3,"('Produced by SHAKEUP')")
        write(iLUN3,"('NONE')")
        write(iLUN3,"('UTC+0000')")
        write(iLUN3,"(1x,8i5,f5.0,'    1    1')") &
            iYearFrom, iJDayFrom, iHourFrom, 60*iMinuteFrom + iSecondFrom, &
            iYearTo, iJDayTo, iHourTo, 60*iMinuteTo + iSecondTo + tMetPro % iDeltaTime, &
            rPlimit
        write(iLUN3,"('     F    F    F    F')")
			
        ! Data
        do i = 1, size(tMetPro % time_stamp_begin)
      	
            ! First (close to ground) line (for Calpuff 6 and 7)
            iErrCode = tDateTime % fromEpoch(tMetPro % time_stamp_end(1))
            iYear    = tDateTime % iYear
            iMonth   = tDateTime % iMonth
            iDay     = tDateTime % iDay
            iHour    = tDateTime % iHour
            iMinute  = tDateTime % iMinute
            iSecond  = tDateTime % rSecond
            write(iLUN1, "(i4,1x,2(i2,1x),i2,1x,f6.1,1x,i1,1x,f7.1,1x,f8.2,1x,f8.2,4(1x,f6.1))") &
                iYear, iMonth, iDay, iHour+1, &
                rZr, 0, tMetPro % Dir(i), tMetPro % Vel(i), &
                tMetPro % Temp(i) + 273.15, &
                -999.9, -999.9, -999.9, -999.9
            write(iLUN2, "(2(i4,1x,2(i2,1x),i2,1x,i4,1x),f6.1,1x,i1,1x,f7.1,1x,f8.2,1x,f8.2,4(1x,f6.1))") &
                iYear, iMonth, iDay, iHour+1, 60*iMinute+iSecond, &
                iYear, iMonth, iDay, iHour+1, 60*iMinute+iSecond + iDeltaTime, &
                rZr, 0, tMetPro % Dir(i), tMetPro % Vel(i), &
                tMetPro % Temp(i) + 273.15, &
                -999.9, -999.9, -999.9, -999.9
			
            ! Upper lines (for Calpuff 6 and 7)
            do j = 1, n - 1
                cartesian = [tMetPro % U(j,i), tMetPro % V(j,i)]
                polar = CartesianToPolar2(cartesian, WCONV_FLOW_TO_PROVENANCE)
                rNewVel = polar(1)
                rNewDir = polar(2)
                rNewTemp = tMetPro % T(j,i)
                if(rNewTemp /= rNewTemp) then
                    rNewTemp = NaN
                else
                    if(rNewTemp < -263.15) then
                        rNewTemp = NaN
                    end if
                end if
                if(.valid. rNewTemp) then
                    write(iLUN1, "(i4,1x,2(i2,1x),i2,1x,f6.1,1x,i1,1x,f7.1,1x,f8.2,1x,f8.2,3(1x,f6.1))") &
                        iYear, iMonth, iDay, iHour+1, &
                        tMetPro % Z(j), 0, rNewDir, rNewVel, &
                        rNewTemp + 273.15, &
                        -999.9, -999.9, -999.9
                    write(iLUN2, "(2(i4,1x,2(i2,1x),i2,1x,i4,1x),f6.1,1x,i1,1x,f7.1,1x,f8.2,1x,f8.2,3(1x,f6.1))") &
                        iYear, iMonth, iDay, iHour+1, 60*iMinute+iSecond, &
                        iYear, iMonth, iDay, iHour+1, 60*iMinute+iSecond + iDeltaTime, &
                        tMetPro % Z(j), 0, rNewDir, rNewVel, &
                        rNewTemp + 273.15, &
                        -999.9, -999.9, -999.9
                else
                    write(iLUN1, "(i4,1x,2(i2,1x),i2,1x,f6.1,1x,i1,1x,f7.1,1x,f8.2,1x,f8.2,3(1x,f6.1))") &
                        iYear, iMonth, iDay, iHour+1, &
                        tMetPro % Z(j), 0, rNewDir, rNewVel, &
                        -9999.9, &
                        -999.9, -999.9, -999.9
                    write(iLUN2, "(2(i4,1x,2(i2,1x),i2,1x,i4,1x),f6.1,1x,i1,1x,f7.1,1x,f8.2,1x,f8.2,3(1x,f6.1))") &
                        iYear, iMonth, iDay, iHour+1, 60*iMinute+iSecond, &
                        iYear, iMonth, iDay, iHour+1, 60*iMinute+iSecond + iDeltaTime, &
                        tMetPro % Z(j), 0, rNewDir, rNewVel, &
                        -9999.9, &
                        -999.9, -999.9, -999.9
                end if
                
            end do
            
            ! Last line (for Calpuff 6 and 7)
            cartesian = [tMetPro % U(n,i), tMetPro % V(n,i)]
            polar = CartesianToPolar2(cartesian, WCONV_FLOW_TO_PROVENANCE)
            rNewVel = polar(1)
            rNewDir = polar(2)
            rNewTemp = tMetPro % T(n,i)
            if(rNewTemp /= rNewTemp) then
                rNewTemp = NaN
            else
                if(rNewTemp < -263.15) then
                    rNewTemp = NaN
                end if
            end if
            if(rNewTemp >= -263.15) then
                write(iLUN1, "(i4,1x,2(i2,1x),i2,1x,f6.1,1x,i1,1x,f7.1,1x,f8.2,1x,f8.2,3(1x,f6.1))") &
                    iYear, iMonth, iDay, iHour+1, &
                    tMetPro % Z(n), 1, rNewDir, rNewVel, &
                    rNewTemp + 273.15, &
                    -999.9, -999.9, -999.9
                write(iLUN2, "(2(i4,1x,2(i2,1x),i2,1x,i4,1x),f6.1,1x,i1,1x,f7.1,1x,f8.2,1x,f8.2,3(1x,f6.1))") &
                    iYear, iMonth, iDay, iHour+1, 60*iMinute+iSecond, &
                    iYear, iMonth, iDay, iHour+1, 60*iMinute+iSecond + iDeltaTime, &
                    tMetPro % Z(n), 1, rNewDir, rNewVel, &
                    rNewTemp + 273.15, &
                    -999.9, -999.9, -999.9
            else
                write(iLUN1, "(i4,1x,2(i2,1x),i2,1x,f6.1,1x,i1,1x,f7.1,1x,f8.2,1x,f8.2,3(1x,f6.1))") &
                    iYear, iMonth, iDay, iHour+1, &
                    tMetPro % Z(n), 1, rNewDir, rNewVel, &
                    -9999.9, &
                    -999.9, -999.9, -999.9
                write(iLUN2, "(2(i4,1x,2(i2,1x),i2,1x,i4,1x),f6.1,1x,i1,1x,f7.1,1x,f8.2,1x,f8.2,3(1x,f6.1))") &
                    iYear, iMonth, iDay, iHour+1, 60*iMinute+iSecond, &
                    iYear, iMonth, iDay, iHour+1, 60*iMinute+iSecond + iDeltaTime, &
                    tMetPro % Z(n), 1, rNewDir, rNewVel, &
                    -9999.9, &
                    -999.9, -999.9, -999.9
            end if
            
            ! Calmet
            write(iLUN3, "(3x,i4,2x,i8,2(4x,i4,i4,i3,i3,i5),2x,i5,1x,i5)") &
                6201, &																		! Data format type
                0, &																		! 5 letter station ID
                iYear, iMonth, iDay, iHour, 60*iMinute + iSecond, &							! Beginning time stamp
                iYear, iMonth, iDay, iHour, 60*iMinute + iSecond + tMetPro % iDeltaTime, &  ! Ending time stamp
                n, n																		! Number of data in profiles
            do j = 1, n, 4
                cartesian = [tMetPro % U(n,i), tMetPro % V(n,i)]
                polar = CartesianToPolar2(cartesian, WCONV_FLOW_TO_PROVENANCE)
                rNewVel = polar(1)
                rNewDir = polar(2)
                rNewTemp = tMetPro % T(n,i)
                if(rNewTemp /= rNewTemp) then
                    rNewTemp = NaN
                else
                    if(rNewTemp < -263.15) then
                        rNewTemp = NaN
                    end if
                end if
                if(rNewTemp >= -263.15) then
                    write(iLUN3, "(4(3x,f6.1,'/',f5.0,'/',f5.1,'/',i3,'/',i3))") &
                        ( &
                            tMetPro % Pa(k), &
                            tMetPro % Z(k), &
                            tMetPro % T(k,i) + 273.15, &
                            nint(rNewDir), &
                            nint(rNewVel), &
                            k=j,MIN(j+3,n) &
                        )
                else
                    write(iLUN3, "(4(3x,f6.1,'/',f5.0,'/',f5.1,'/',i3,'/',i3))") &
                        ( &
                            tMetPro % Pa(k), &
                            tMetPro % Z(k), &
                            tMetPro % T(k,i) + 273.15, &
                            999, &
                            999, &
                            k=j,MIN(j+3,n) &
                        )
                end if
            end do
            
        end do
        
		! Add (by replication of last) a profile block to allow CALMET upper air data bracketing
        iErrCode = tDateTime % fromEpoch(tMetPro % time_stamp_end(1))
        iYear    = tDateTime % iYear
        iMonth   = tDateTime % iMonth
        iDay     = tDateTime % iDay
        iHour    = tDateTime % iHour
        iMinute  = tDateTime % iMinute
        iSecond  = tDateTime % rSecond
        write(15, "(3x,i4,2x,i8,2(4x,i4,i4,i3,i3,i5),2x,i5,1x,i5)") &
            6201, &																		! Data format type
            0, &																		! 5 letter station ID
            iYear, iMonth, iDay, iHour, 60*iMinute + iSecond, &							! Beginning time stamp
            iYear, iMonth, iDay, iHour, 60*iMinute + iSecond + tMetPro % iDeltaTime, &	! Ending time stamp
            n, n																		! Number of data in profiles
        do j = 1, n, 4
            cartesian = [tMetPro % U(n,i), tMetPro % V(n,i)]
            polar = CartesianToPolar2(cartesian, WCONV_FLOW_TO_PROVENANCE)
            rNewVel = polar(1)
            rNewDir = polar(2)
            rNewTemp = tMetPro % T(n,i)
            if(rNewTemp /= rNewTemp) then
                rNewTemp = NaN
            else
                if(rNewTemp < -263.15) then
                    rNewTemp = NaN
                end if
            end if
            if(rNewTemp >= -263.15) then
                write(iLUN3, "(4(3x,f6.1,'/',f5.0,'/',f5.1,'/',i3,'/',i3))") &
                    ( &
                        tMetPro % Pa(k), &
                        tMetPro % Z(k), &
                        tMetPro % T(k,i) + 273.15, &
                        nint(rNewDir), &
                        nint(rNewVel), &
                        k=j,MIN(j+3,n) &
                    )
            else
                write(iLUN3, "(4(3x,f6.1,'/',f5.0,'/',f5.1,'/',i3,'/',i3))") &
                    ( &
                        tMetPro % Pa(k), &
                        tMetPro % Z(k), &
                        tMetPro % T(k,i) + 273.15, &
                        999, &
                        999, &
                        k=j,MIN(j+3,n) &
                    )
            end if

		end do
        
        close(iLUN3)
        close(iLUN2)
        close(iLUN1)

    end function calpuff
	
end module model_meteo

