! pbl_depth  : Fortran module, providing support to the estimation of
!              mixing height and other quantities in the full depth of
!              the Planetary Boundary Layer.
!
! This module is part of the pbl_met library.
!
! This is open-source code, covered by the lGPL 3.0 license.
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

contains

	function EstimateZi(rvTimeStamp, iZone, rLat, rLon, iDeltaTime, rvTemp, rvUstar, rvH0, rvN, nStep, rvZi) result(iRetCode)
	
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
			if(.not..sensible. tStamp) then
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
				rZiConv = MAX(rZiConv, 0.)
				rZiConv = ConvectiveZi(dble(iDeltaTime),rvH0(i),rvUstar(i),rTa,rRc,rZiConv,n_step)
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

	function ConvectiveZi(dtime,H0,us,Temp,rc,hold,n_step) result(hmix)
	
		! Routine arguments
		real(8), intent(in)				:: dtime		! Time step (s)
		real(8), intent(in)				:: H0			! Turbulent sensible heat flux (W/m2)
		real(8), intent(in)				:: us			! Friction velocity (m/s)
		real(8), intent(in)				:: Temp			! Temperature (°C)
		real(8), intent(in)				:: rc			! RhoCp
		real(8), intent(in)				:: hold			! Mixing height on previous time step (m)
		integer, intent(in)				:: n_step		! Number of sub-steps within a whole step
		real(8)							:: hmix			! Mixing height past current time step (m)
		
		! Locals
		real(8)		:: dt
		real(8)		:: Ta
		real(8)		:: ggmm
		real(8)		:: hmm
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
		ggmm = 0.005d0
		hmm  = hold
		do i=1,n_step
			hk1  = dt * GryningBatchvarovaStep(rc,Ta,ggmm,us,H0,hmm)
			hk2  = dt * GryningBatchvarovaStep(rc,Ta,ggmm,us,H0,hmm+hk1/2.d0)
			hk3  = dt * GryningBatchvarovaStep(rc,Ta,ggmm,us,H0,hmm+hk2/2.d0)
			hk4  = dt * GryningBatchvarovaStep(rc,Ta,ggmm,us,H0,hmm+hk3/2.d0)
			hmm  = hmm + (hk1+2.d0*(hk2+hk3)+hk4)/6.d0
		end do
		Hmix = hmm

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

		! Accept only if >= than purely mechanical approx
		if(Zi > 2.0*1330.0*Ustar .or. Zi < 0.5*1330.0*Ustar) then
			Zi = 1330.0*Ustar
		else
			Zi = MAX(Zi, 1330.0*Ustar)
		end if

	end function StableZi
    
end module pbl_depth
