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

    implicit none
    
    private
    
    ! Public interface

contains

	function ConvectiveZi(dtime,H0,us,Tm,rc,hold) result(hmix)
	
		! Routine arguments
		real, intent(in)		:: dtime		! Time step (s)
		real, intent(in)		:: H0			! Turbulent sensible heat flux (W/m2)
		real, intent(in)		:: us			! Friction velocity (m/s)
		real, intent(in)		:: Tm			! Temperature (K)
		real, intent(in)		:: rc			! RhoCp
		real, intent(in)		:: hold			! Mixing height on previous time step (m)
		real					:: hmix			! Mixing height past current time step (m)
		
		! Locals
		real		:: dt
		real		:: ggmm
		real		:: hmm
		real		:: hk1
		real		:: hk2
		real		:: hk3
		real		:: hk4
		integer		:: i
		
		! Constant parameters
		integer, parameter	:: n_step = 60
		
		! Initialize
		Hmix = NaN
		dt   = dtime/n_step
		if(.invalid.rc) return

		! Runge-Kutta step
		ggmm = 0.005
		hmm  = hold
		do i=1,n_step
			hk1  = dt * GryningBatchvarovaStep(rc,Tm,ggmm,us,H0,hmm)
			hk2  = dt * GryningBatchvarovaStep(rc,Tm,ggmm,us,H0,hmm+hk1/2.)
			hk3  = dt * GryningBatchvarovaStep(rc,Tm,ggmm,us,H0,hmm+hk2/2.)
			hk4  = dt * GryningBatchvarovaStep(rc,Tm,ggmm,us,H0,hmm+hk3/2.)
			hmm  = hmm + (hk1+2.*(hk2+hk3)+hk4)/6.
		end do
		Hmix = hmm

	end function ConvectiveZi


	function GryningBatchvarovaStep(rc,Tm,gg,us,h0,hm) result(F)
	
		! Routine arguments
		real, intent(in)	:: rc
		real, intent(in)	:: Tm
		real, intent(in)	:: gg
		real, intent(in)	:: us
		real, intent(in)	:: h0
		real, intent(in)	:: hm
		real				:: F
		
		! Locals
		real	:: hL
		real	:: cost
		real	:: f1
		real	:: f2
		
		! Constants
		real, parameter	:: hk = 0.4
		real, parameter	:: g  = 9.81
		real, parameter	:: A  = 0.2
		real, parameter	:: B  = 2.5
		real, parameter	:: C  = 8.0

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
		real, intent(in)	:: Temp		! Air temperature (°C)
		real, intent(in)	:: H0		! Turbulent sensible heat flux (W/m2)
		real, intent(in)	:: Ustar	! Friction velocity (m/s)
		real, intent(in)	:: L		! Obukhov length (m)
		real, intent(in)	:: N		! Brunt-Vaisala frequency (Hz)
		real				:: Zi

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
		! From now on, stability is guaranteed

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
