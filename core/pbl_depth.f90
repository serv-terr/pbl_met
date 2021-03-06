! pbl_depth  : Fortran module, providing support to the estimation of
!              mixing height and other quantities in the full depth of
!              the Planetary Boundary Layer.
!
! This module is part of the pbl_met library.
!
! This is open-source code, covered by the lGPL 3.0 license.
!
! Author(s): Patrizia Favaron
!
module pbl_depth

    use pbl_base
    use pbl_wind
    use pbl_simil
    use pbl_time
    use pbl_thermo

    implicit none

    private

    ! Public interface
    public	:: EstimateZi
    public	:: LapseRateSpec
	public	:: ZiDailySynthesis

    ! Data types

    type LapseRateSpec
    	real(8)	:: A
    	real(8)	:: B
    	real(8)	:: C
    contains
    	procedure	:: setDefault	=> lrSetDefault
    	procedure	:: getLapseRate	=> lrGetLapseRate
    end type LapseRateSpec

contains

	function EstimateZi(rvTimeStamp, iZone, rLat, rLon, iDeltaTime, rvTemp, rvUstar, rvH0, rvN, nStep, tLrate, rvZi) result(iRetCode)

		! Routine arguments
		real(8), dimension(:), allocatable, intent(in)				:: rvTimeStamp
		integer, intent(in)											:: iZone
		real, intent(in)											:: rLat
		real, intent(in)											:: rLon
		integer, intent(in)											:: iDeltaTime
		real(8), dimension(:), allocatable, intent(in)				:: rvTemp
		real(8), dimension(:), allocatable, intent(in)				:: rvUstar
		real(8), dimension(:), allocatable, intent(in)				:: rvH0
		real(8), dimension(:), allocatable, intent(in), optional	:: rvN
		integer, intent(in), optional								:: nStep
		type(LapseRateSpec), intent(in), optional					:: tLrate
		real(8), dimension(:), allocatable, intent(out)				:: rvZi
		integer														:: iRetCode

		! Locals
		integer								:: iErrCode
		integer								:: i
		real, dimension(2)					:: rvSunRiseSet
		type(DateTime)						:: tStamp
		real(8)								:: rSunRise
		real(8)								:: rSunSet
		real(8)								:: rHour
		real(8)								:: rN
		real(8)								:: rTa
		real(8)								:: rRc
		real(8)								:: rZiMec
		real(8)								:: rZiConv
		real(8)								:: rWprimeThetaprime
		real(8)								:: rL
		integer								:: n_step
		integer, dimension(5)				:: ivVectorLength
		integer, dimension(5)				:: ivValidNum

		! Constants
		real(8), parameter	:: k = 0.4d0
		real(8), parameter	:: g = 9.81d0

		! Assume success (will falsify on failure)
		iRetCode = 0

		! Check something can be made
		if( &
			.not.allocated(rvTimeStamp) .or. &
			.not.allocated(rvTemp) .or. &
			.not.allocated(rvUstar) .or. &
			.not.allocated(rvH0) &
		) then
			iRetCode = 1
			return
		end if
		if(present(rvN)) then
			if(.not.allocated(rvTimeStamp)) then
				iRetCode = 1
				return
			end if
		end if
		ivVectorLength(1) = size(rvTimeStamp)
		ivVectorLength(2) = size(rvTemp)
		ivVectorLength(3) = size(rvUstar)
		ivVectorLength(4) = size(rvH0)
		ivVectorLength(5) = size(rvTimeStamp)
		if(present(rvN)) ivVectorLength(5) = size(rvN)
		if(minval(ivVectorLength) <= 0) then
			iRetCode = 2
			return
		end if
		if(minval(ivVectorLength) /= maxval(ivVectorLength)) then
			iRetCode = 3
			return
		end if
		if(present(nStep)) then
			if(nStep <= 0) then
				iRetCode = 4
				return
			end if
			n_step = nStep
		else
			n_step = 60
		end if
		ivValidNum(1) = count(.valid.rvTimeStamp)
		ivValidNum(2) = count(.valid.rvTemp)
		ivValidNum(3) = count(.valid.rvUstar)
		ivValidNum(4) = count(.valid.rvH0)
		ivValidNum(5) = count(.valid.rvTimeStamp)
		if(present(rvN)) ivValidNum(5) = count(.valid.rvN)
		if(minval(ivValidNum) <= 0) then
			iRetCode = 5
			return
		end if

		! Reserve workspace
		if(allocated(rvZi)) deallocate(rvZi)
		allocate(rvZi(size(rvTimeStamp)))

		! Main loop: process mixing heights
    	do i = 1, size(rvTimeStamp)

    		if(.invalid.rvTimeStamp(i)) then
    			rvZi(i) = NaN_8
    			cycle
    		end if

			! Get time stamp at official value (to help tracking events in data files;
			! true time stamp at mid of averagin interval will be computed immediately after)
			iErrCode = tStamp % fromEpoch(rvTimeStamp(i) + iDeltaTime / 2.d0)
			if(iErrCode /= 0) then
    			rvZi(i) = NaN_8
    			cycle
			end if
			if(.not.(.sensible. tStamp)) then
    			rvZi(i) = NaN_8
    			cycle
			end if

			! Get time stamp at *mid* of averaging interval
			rHour = tStamp % iHour + tStamp % iMinute / 60.0d0 + tStamp % rSecond / 3600.0d0

			! Get astronomical indicators
			rvSunRiseSet = SunRiseSunSet(tStamp % iYear, tStamp % iMonth, tStamp % iDay, rLat, rLon, iZone)
			rSunRise = rvSunRiseSet(1)
			rSunSet  = rvSunRiseSet(2)

			! Estimate mixing height
			rTa    = rvTemp(i) + 273.15d0
			rRc    = RhoCp(rTa)
			rZiMec = 1330.*rvUstar(i)
			if(rHour > rSunRise .and. rHour < rSunSet) then
				rZiConv = MAX(rZiConv, 0.d0)
				if(present(tLrate)) then
					rZiConv = ConvectiveZi(dble(iDeltaTime),rvH0(i),rvUstar(i),rTa,rRc,rZiConv,n_step,tLrate)
				else
					rZiConv = ConvectiveZi(dble(iDeltaTime),rvH0(i),rvUstar(i),rTa,rRc,rZiConv,n_step)
				end if
				rvZi(i) = max(rZiMec, rZiConv)
			else
				rZiConv = 0.
				rWprimeThetaprime = rvH0(i) / rRc
				rL = - rvUstar(i)**3 * rTa / (k*g*rWprimeThetaprime)
				if(present(rvN)) then
					rN = rvN(i)
				else
					rN = 0.012
				end if
				rvZi(i) = StableZi( &
					rLat, &
					rvTemp(i), &
					rvH0(i), &
					rvUstar(i), &
					rL, &
					rN &
				)
			end if

		end do

	end function EstimateZi

	! *********************
	! * Internal routines *
	! *********************

	function ConvectiveZi(dtime,H0,us,Temp,rc,hold,n_step, tLrate) result(hmix)

		! Routine arguments
		real(8), intent(in)							:: dtime		! Time step (s)
		real(8), intent(in)							:: H0			! Turbulent sensible heat flux (W/m2)
		real(8), intent(in)							:: us			! Friction velocity (m/s)
		real(8), intent(in)							:: Temp			! Temperature (°C)
		real(8), intent(in)							:: rc			! RhoCp
		real(8), intent(in)							:: hold			! Mixing height on previous time step (m)
		integer, intent(in)							:: n_step		! Number of sub-steps within a whole step
		type(LapseRateSpec), intent(in), optional	:: tLrate		! If specified, lapse rate is computed from here instead of assumed as constant (default behavior)
		real(8)										:: hmix			! Mixing height past current time step (m)

		! Locals
		real(8)		:: dt
		real(8)		:: Ta
		real(8)		:: gamma
		real(8)		:: tempZi
		real(8)		:: hk1
		real(8)		:: hk2
		real(8)		:: hk3
		real(8)		:: hk4
		integer		:: i

		! Check something can be made
		if(n_step <= 0) then
			hmix = NaN_8
			return
		end if

		! Initialize
		Hmix = NaN_8
		dt   = dtime/n_step
		if(.invalid.rc) return
		Ta = Temp + 273.15d0

		! Runge-Kutta step
		if(present(tLrate)) then
			gamma = tLrate % getLapseRate(hold)
		else
			gamma = 0.005d0
		end if
		tempZi  = hold
		do i=1,n_step
			hk1  = dt * GryningBatchvarovaStep(rc,Ta,gamma,us,H0,tempZi)
			hk2  = dt * GryningBatchvarovaStep(rc,Ta,gamma,us,H0,tempZi+hk1/2.d0)
			hk3  = dt * GryningBatchvarovaStep(rc,Ta,gamma,us,H0,tempZi+hk2/2.d0)
			hk4  = dt * GryningBatchvarovaStep(rc,Ta,gamma,us,H0,tempZi+hk3/2.d0)
			tempZi  = tempZi + (hk1+2.d0*(hk2+hk3)+hk4)/6.d0
			if(present(tLrate)) then
				gamma = tLrate % getLapseRate(tempZi)
			else
				gamma = 0.005d0
			end if
		end do
		Hmix = tempZi

	end function ConvectiveZi


	function GryningBatchvarovaStep(rc,Tm,gg,us,h0,hm) result(F)

		! Routine arguments
		real(8), intent(in)	:: rc
		real(8), intent(in)	:: Tm
		real(8), intent(in)	:: gg
		real(8), intent(in)	:: us
		real(8), intent(in)	:: h0
		real(8), intent(in)	:: hm
		real(8)				:: F

		! Locals
		real(8)	:: hL
		real(8)	:: cost
		real(8)	:: f1
		real(8)	:: f2

		! Constants
		real(8), parameter	:: hk = 0.4d0
		real(8), parameter	:: g  = 9.81d0
		real(8), parameter	:: A  = 0.2d0
		real(8), parameter	:: B  = 2.5d0
		real(8), parameter	:: C  = 8.0d0

		! Compute the desired quantity
		hL   = -rc*Tm*us*us*us/(hk*g*h0)
		cost = B*hk*hL
		f1   = (1.+2.*A)*hm-2.*cost
		f2   = gg*g*((1.+A)*hm-cost)
		F    = hm*hm/f1 + C*us*us*Tm/f2
		F    = h0/(rc*gg*F)

	end function GryningBatchvarovaStep


	function StableZi(Lat, Temp, H0, Ustar, L, N) result(Zi)

		! Routine arguments
		real, intent(in)	:: Lat		! Latitude (degrees)
		real(8), intent(in)	:: Temp		! Air temperature (°C)
		real(8), intent(in)	:: H0		! Turbulent sensible heat flux (W/m2)
		real(8), intent(in)	:: Ustar	! Friction velocity (m/s)
		real(8), intent(in)	:: L		! Obukhov length (m)
		real(8), intent(in)	:: N		! Brunt-Vaisala frequency (Hz)
		real(8)				:: Zi

		! Locals
		real(8)				:: rLat
		real(8)				:: f
		real(8)				:: Ta
		real(8)				:: a
		real(8)				:: b1
		real(8)				:: b2
		real(8)				:: b3
		real(8)				:: b
		real(8)				:: wt
		real(8)				:: rc

		! Constants
		real(8), parameter	:: g = 9.807d0

		! Check something is to be done
		if(L < 1.e-5 .or. Ustar < 1.e-5 .or. Temp < -40.0) then
			Zi = 1330.0 * Ustar		! Degrade to purely mechanical rough estimate
			return
		end if
		! From now on, strong stability is guaranteed

		! Compute Coriolis parameter
		rLat = Lat * 3.14159265358979d0 / 180.d0
		f    = 2.d0*7.29d-5*SIN(rLat)

		! Compute temperature in K
		Ta = Temp + 273.15d0

		! Compute w't'
		rc = 1305.d0 * 273.16d0/Ta
		wt = H0 / rc

		! Compute Zilitinkevich non-advective function parts
		a  = (f/(0.5d0*Ustar))**2
		b1 = 0.1d0 / L
		b2 = N / (26.d0*Ustar)
		b3 = SQRT(ABS(g*f*wt/Ta)) / (1.7d0*Ustar)
		b  = b1 + b2 + b3

		! Compute stable estimate of mixing height
		Zi = (SQRT(4.d0*a + b**2) - b)/(2.d0*a)

		! Accept only within 100% difference from purely mechanical approx
		if(Zi > 2.0*1330.0*Ustar .or. Zi < 0.5*1330.0*Ustar) then
			Zi = 1330.0*Ustar
		else
			Zi = MAX(Zi, 1330.0*Ustar)
		end if

	end function StableZi


	subroutine lrSetDefault(this)

		! Routine arguments
		class(LapseRateSpec), intent(out)	:: this

		! Locals
		! --none--

		! Assign default values (from, I guess, Mexico City campaign, circa 1998)
		this % A = 3.d0
		this % B = 1.98d-3
		this % C = 2.27d-6

	end subroutine lrSetDefault


	function lrGetLapseRate(this, z) result(gamma)

		! Routine arguments
		class(LapseRateSpec), intent(in)	:: this
		real(8), intent(in)					:: z
		real(8)								:: gamma

		! Locals
		! --none--

		! Evaluate lapse rate
		if(z > 0.0) then
			gamma = this % A / (z + 1.d0) - this % B + this % C * z
		else
			gamma = this % A - this % B
		end if

	end function lrGetLapseRate


	function ZiDailySynthesis(rvTimeStamp, rvVel, rvZi, rvDailyTimeStamp, rvMaxZi, rvMaxPlm) result(iRetCode)

		implicit none

		! Routine arguments
		real(8), dimension(:), intent(in)				:: rvTimeStamp
		real, dimension(:), intent(in)					:: rvVel
		real, dimension(:), intent(in)					:: rvZi
		real(8), dimension(:), allocatable, intent(out)	:: rvDailyTimeStamp
		real, dimension(:), allocatable, intent(out)	:: rvMaxZi			! Maximum mixing height (m)
		real, dimension(:), allocatable, intent(out)	:: rvMaxPlm			! Palmieri Index (vel*zi) (m^2/s)
		integer											:: iRetCode

		! Locals
		integer	:: iNumData
		integer	:: i
		integer	:: iDay
		integer	:: iHour
		real	:: rMaxZi
		real	:: rMaxPlm
		integer	:: iNumDays
		integer(8), dimension(:), allocatable	:: ivDay
		logical, dimension(:), allocatable		:: lvValid
		integer, dimension(:), allocatable		:: ivDayBegin
		integer, dimension(:), allocatable		:: ivDayEnd

		! Assume success (will falsify on failure)
		iRetCode = 0
	
		! Check parameters
		iNumData = size(rvTimeStamp)
		if(size(rvVel) /= iNumData .or. size(rvZi) /= iNumData) then
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
		if(allocated(rvDailyTimeStamp)) deallocate(rvDailyTimeStamp)
		if(allocated(rvMaxZi))          deallocate(rvMaxZi)
		if(allocated(rvMaxPlm))         deallocate(rvMaxPlm)
		allocate(rvDailyTimeStamp(iNumData))
		allocate(rvMaxZi(iNumData))
		allocate(rvMaxPlm(iNumData))

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
		rvMaxZi  = -huge(rvMaxZi)
		rvMaxPlm = -huge(rvMaxPlm)
		do iDay = 1, iNumDays

			! Calculate daily initial time stamp
			rvDailyTimeStamp(iDay) = rvTimeStamp(ivDayBegin(iDay))
			rvDailyTimeStamp(iDay) = floor(rvDailyTimeStamp(iDay) / 86400.d0) * 86400.d0

			! Calculate maximum Zi and Palmieri index
			do i = ivDayBegin(iDay), ivDayEnd(iDay)
				if(rvZi(i) == rvZi(i)) then
					rvMaxZi(iDay)  = max(rvMaxZi(iDay),  rvZi(i))
				end if
				if(rvVel(i) == rvVel(i) .and. rvZi(i) == rvZi(i)) then
					rvMaxPlm(iDay) = max(rvMaxPlm(iDay), rvZi(i) * rvVel(i))
				end if
			end do

		end do

		! Invalidate non-positive Zi and Plm
		where(rvMaxZi <= 0.)
			rvMaxZi = NaN
		end where
		where(rvMaxPlm <= 0.)
			rvMaxPlm = NaN
		end where

		! Leave
		deallocate(ivDayBegin, ivDayEnd)
		deallocate(ivDay)
	
	end function ZiDailySynthesis

end module pbl_depth
