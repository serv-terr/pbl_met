module Particles
	
	use pbl_met
	use Configuration
	
	implicit none

	type Particle
		logical	:: filled
		real(8)	:: EmissionTime
		real(8)	:: Xp, Yp, Zp	! Position
		real(8)	:: up, vp, wp	! Velocity
		real(8)	:: Qp, Tp		! Mass, temperature
		real(8)	:: sh, sz		! Horizontal, vertical sigmas for Gaussian kernel
	contains
		procedure	:: Emit => parEmit
	end type Particle

	type ParticlePool
		! Particle domain
		real(8)										:: xmin
		real(8)										:: xmax
		real(8)										:: ymin
		real(8)										:: ymax
		real(8)										:: zmin
		real(8)										:: zmax
		! Timing
		real(8)										:: T_step
		! Particle pool
		type(Particle), dimension(:), allocatable	:: tvPart
		integer										:: maxpart
		integer										:: next
	contains
		procedure	:: Initialize => pplInit
		procedure	:: Emit       => pplEmit
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
	
	function pplInit(this, cfg) result(iRetCode)
	
		! Routine arguments
		class(ParticlePool), intent(out)	:: this
		type(Config), intent(in)			:: cfg
		integer								:: iRetCode
		
		! Locals
		integer	:: i
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Assign domain
		this % xmin = cfg % x0
		this % xmax = cfg % x1
		this % ymin = cfg % y0
		this % ymax = cfg % y1
		this % zmin = 0.d0
		this % zmax = cfg % zmax
		
		! Assign timing
		this % T_step = cfg % Tmed / cfg % Nstep
		
		! Initialize particle pool
		this % maxpart = cfg % maxpart
		if(allocated(this % tvPart)) deallocate(this % tvPart)
		allocate(this % tvPart(this % maxpart))
		do i = 1, this % maxpart
			this % tvPart(i) % filled = .false.
		end do
		
	end function pplInit
	
	
	function pplEmit(this) result(iRetCode)
	
		! Routine arguments
		class(ParticlePool), intent(out)	:: this
		integer								:: iRetCode
		
		! Locals
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
	end function pplEmit
	
end module Particles
