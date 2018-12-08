module Particles
	
	use pbl_met
	use Configuration
	
	implicit none

	type Particle
		real(8)	:: EmissionTime
		real(8)	:: Xp, Yp, Zp	! Position
		real(8)	:: up, vp, wp	! Velocity
		real(8)	:: Qp, Tp		! Mass, temperature
		real(8)	:: sh, sz		! Horizontal, vertical sigmas for Gaussian kernel
	contains
		procedure	:: Emit => parEmit
	end type Particle

	type ParticlePool
		type(Particle), dimension(:), allocatable	:: tvPart
		integer										:: next
	contains
		procedure	:: Emit => pplEmit
	end type ParticlePool
	
contains

	! *******************
	! * Single particle *
	! *******************
	
	function parEmit(this) result(iRetCode)
	
		! Routine arguments
		class(Particle), intent(out)	:: this
		integer							:: iRetCode
		
		! Locals
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
	end function parEmit
	
	! *****************
	! * Particle pool *
	! *****************

	function pplEmit(this) result(iRetCode)
	
		! Routine arguments
		class(Particle), intent(out)	:: this
		integer							:: iRetCode
		
		! Locals
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
	end function pplEmit
	
end module Particles
