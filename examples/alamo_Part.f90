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
		! Gridded receptor coordinates
		integer										:: nx
		integer										:: ny
		real(8), dimension(:), allocatable			:: xrec
		real(8), dimension(:), allocatable			:: yrec
		! Concentrations
		real(8), dimension(:,:), allocatable		:: C
		! Particle pool
		type(Particle), dimension(:), allocatable	:: tvPart
		integer										:: maxpart
		integer										:: next
	contains
		procedure	:: Initialize => pplInit
		procedure	:: ResetConc  => pplResetC
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
		
		! Initialise receptor coordinates
		this % nx = cfg % nx
		this % ny = cfg % ny
		if(allocated(this % xrec)) deallocate(this % xrec)
		if(allocated(this % yrec)) deallocate(this % yrec)
		allocate(this % xrec(cfg % nx))
		allocate(this % yrec(cfg % ny))
		this % xrec = [(this % xmin + (i-1) * cfg % dx, i = 1, cfg % nx)]
		this % yrec = [(this % ymin + (i-1) * cfg % dy, i = 1, cfg % ny)]
		
		! Initialize concentration array
		if(allocated(this % C)) deallocate(this % C)
		allocate(this % C(this % nx, this % ny))
		this % C = 0.d0
		
		! Assign timing constants
		this % T_step = cfg % Tmed / cfg % Nstep
		
		! Initialise particle space
		this % maxpart = cfg % maxpart
		if(allocated(this % tvPart)) deallocate(this % tvPart)
		allocate(this % tvPart(this % maxpart))
		do i = 1, this % maxpart
			this % tvPart(i) % filled = .false.
		end do
		this % next = 1
		
	end function pplInit
	
	
	function pplResetC(this) result(iRetCode)
	
		! Routine arguments
		class(ParticlePool), intent(out)	:: this
		integer								:: iRetCode
		
		! Locals
		! --none--
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Reset concentration matrix
		if(allocated(this % C)) then
			this % C = 0.d0
		else
			iRetCode = 1
		end if
		
	end function pplResetC
	
	
	function pplEmit(this) result(iRetCode)
	
		! Routine arguments
		class(ParticlePool), intent(out)	:: this
		integer								:: iRetCode
		
		! Locals
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
	end function pplEmit
	
end module Particles
