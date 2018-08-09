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
    
end module pbl_depth
