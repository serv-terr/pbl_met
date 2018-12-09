module Particles
	
	use pbl_met
	use Configuration
	use ziggurat
	
	implicit none

	type Particle
		logical	:: filled       = .false.
		real(8)	:: EmissionTime
		real(8)	:: Xp, Yp, Zp	! Position
		real(8)	:: up, vp, wp	! Velocity
		real(8)	:: Qp, Tp		! Mass, age
		real(8)	:: sh, sz		! Horizontal, vertical sigmas for Gaussian kernel
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
		real(8)										:: T_substep
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
		integer										:: partIdx
	contains
		procedure	:: Initialize => pplInit
		procedure	:: ResetConc  => pplResetC
		procedure	:: Emit       => pplEmit
		procedure	:: Move       => pplMove
		procedure	:: Count      => pplCount
	end type ParticlePool
	
contains

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
		this % T_substep = cfg % Tmed / cfg % Nstep
		
		! Initialise particle space
		this % maxpart = cfg % maxpart
		if(allocated(this % tvPart)) deallocate(this % tvPart)
		allocate(this % tvPart(this % maxpart))
		do i = 1, this % maxpart
			this % tvPart(i) % filled = .false.
		end do
		this % partIdx = 0
		
	end function pplInit
	
	
	function pplResetC(this) result(iRetCode)
	
		! Routine arguments
		class(ParticlePool), intent(inout)	:: this
		integer								:: iRetCode
		
		! Locals
		logical	:: isAllocated
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Reset concentration matrix
		isAllocated = allocated(this % C)
		if(isAllocated) then
			this % C = 0.d0
		else
			iRetCode = 1
		end if
		
	end function pplResetC
	
	
	function pplEmit(this, cfg, prf) result(iRetCode)
	
		! Routine arguments
		class(ParticlePool), intent(inout)	:: this
		type(Config), intent(in)			:: cfg
		type(MetProfiles), intent(in)		:: prf
		integer								:: iRetCode
		
		! Locals
		integer				:: iErrCode
		integer				:: iPart
		integer				:: iSource
		real(8)				:: z
		real(8)				:: u
		real(8)				:: v
		real(8)				:: su2
		real(8)				:: sv2
		real(8)				:: sw2
		real(8)				:: dsw2
		real(8)				:: eps
		real(8)				:: alpha
		real(8)				:: beta
		real(8)				:: gamma
		real(8)				:: delta
		real(8)				:: alpha_u
		real(8)				:: alpha_v
		real(8)				:: deltau
		real(8)				:: deltav
		real(8)				:: deltat
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Static point sources
		do iSource = 1, size(cfg % tvPointStatic)
		
			! Get source height, and use it to get the new particles' atmospheric parameters
			z = cfg % tvPointStatic(iSource) % z
			iErrCode = prf % evaluate( &
				cfg, &
				z, &
				u, v, &
				su2, sv2, sw2, dsw2, &
				eps, &
				alpha, beta, gamma, delta, &
				alpha_u, alpha_v, &
				deltau, deltav, &
				deltat &
			)
			if(iErrCode /= 0) then
				iRetCode = 1
				return
			end if
		
			do iPart = 1, cfg % Np
			
				! Compute index of current particle in pool
				this % partIdx = this % partIdx + 1
				if(this % partIdx > this % maxpart) this % partIdx = 1
				
				! Assign particle initial position as source center plus a source radius dependent random shift
				this % tvPart(this % partIdx) % Xp = &
					cfg % tvPointStatic(iSource) % x + &
					rnor() * cfg % tvPointStatic(iSource) % radius
				this % tvPart(this % partIdx) % Yp = &
					cfg % tvPointStatic(iSource) % y + &
					rnor() * cfg % tvPointStatic(iSource) % radius
				this % tvPart(this % partIdx) % Zp = &
					cfg % tvPointStatic(iSource) % z + &
					rnor() * cfg % tvPointStatic(iSource) % radius
				if(this % tvPart(this % partIdx) % Zp < 0.d0) &
					this % tvPart(this % partIdx) % Zp = -this % tvPart(this % partIdx) % Zp
					
				! Assign particle initial velocity
				this % tvPart(this % partIdx) % up = rnor() * sqrt(su2)
				this % tvPart(this % partIdx) % vp = rnor() * sqrt(sv2)
				this % tvPart(this % partIdx) % wp = rnor() * sqrt(sw2)
				
				! Assign mass, creation time and age
				this % tvPart(this % partIdx) % Qp = &
					cfg % tvPointStatic(iSource) % q * &
					this % T_substep / cfg % Np
				this % tvPart(this % partIdx) % EmissionTime = prf % rEpoch
				this % tvPart(this % partIdx) % Tp           = 0.d0
				this % tvPart(this % partIdx) % filled       = .true.
				
				! Assign initial Gaussian kernel "sizes" to 0 - the particles on their beginning have a defined position
				this % tvPart(this % partIdx) % sh = 0.d0
				this % tvPart(this % partIdx) % sz = 0.d0
			
			end do
			
		end do
		
	end function pplEmit
	
	
	function pplMove(this, cfg, prf, iSubStep) result(iRetCode)
	
		! Routine arguments
		class(ParticlePool), intent(inout)	:: this
		type(Config), intent(in)			:: cfg
		type(MetProfiles), intent(in)		:: prf
		integer, intent(in)					:: iSubStep
		integer								:: iRetCode
		
		! Locals
		integer				:: iErrCode
		integer				:: iPart
		integer				:: iSource
		real(8)				:: z
		real(8)				:: u
		real(8)				:: v
		real(8)				:: su2
		real(8)				:: sv2
		real(8)				:: sw2
		real(8)				:: dsw2
		real(8)				:: eps
		real(8)				:: alpha
		real(8)				:: beta
		real(8)				:: gamma
		real(8)				:: delta
		real(8)				:: alpha_u
		real(8)				:: alpha_v
		real(8)				:: deltau
		real(8)				:: deltav
		real(8)				:: deltat
		real(8)				:: curPeriod
		real(8)				:: zi
		real(8)				:: h0
		real(8)				:: rootDeltat
		real(8)				:: vel
		real(8)				:: sina
		real(8)				:: cosa
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		do iPart = 1, cfg % Np
		
			! Here we follow the time evolution of each particle in sequence
			curPeriod = this % T_substep
			do while(curPeriod > 0.d0)
			
				! **************
				! * Initialise *
				! **************
				
				! Set met environment and, what counts no less, the ideal time step
				z = this % tvPart(iPart) % Zp
				iErrCode = prf % evaluate( &
					cfg, &
					z, &
					u, v, &
					su2, sv2, sw2, dsw2, &
					eps, &
					alpha, beta, gamma, delta, &
					alpha_u, alpha_v, &
					deltau, deltav, &
					deltat &
				)
				if(iErrCode /= 0) then
					iRetCode = 1
					return
				end if
				
				! Compute time delta, as the ideal time step but possibly for end value
				if(curPeriod > deltat) then
					curPeriod = curPeriod - deltat
				else
					deltat = curPeriod
					curPeriod = 0.d0	! Will force inner cycle to terminate
				end if
				
				! *************************
				! * Drift along mean wind *
				! *************************
				
				! Get wind direction through its directing cosines
				! (no need of trig function calls)
				vel  = sqrt(u**2 + v**2)
				sina = v/vel
				cosa = u/vel
				
				! Update particle position
				this % tvPart(iPart) % Xp = this % tvPart(iPart) % Xp + &
					(u + this % tvPart(iPart) % up * cosa - this % tvPart(iPart) % vp * sina) * deltat
				this % tvPart(iPart) % Yp = this % tvPart(iPart) % Yp + &
					(v + this % tvPart(iPart) % up * sina + this % tvPart(iPart) % vp * cosa) * deltat
				this % tvPart(iPart) % Zp = this % tvPart(iPart) % Zp + &
					this % tvPart(iPart) % wp * deltat
					
				! Check if reflections occurred at ground or Zi
				if(this % tvPart(iPart) % Zp < 0.d0) then
					this % tvPart(iPart) % Zp = -this % tvPart(iPart) % Zp
					this % tvPart(iPart) % wp = -this % tvPart(iPart) % wp
				end if
				zi = cfg % tMeteo % rvExtZi(iSubStep)
				h0 = cfg % tMeteo % rvExtH0(iSubStep)
				if(this % tvPart(iPart) % Zp > zi .and. h0 > 0.d0) then
					this % tvPart(iPart) % Zp = 2.*zi - this % tvPart(iPart) % Zp
					this % tvPart(iPart) % wp = -this % tvPart(iPart) % wp
				end if
				
				! Label the particle as lost if, after application of preceding checks, it
				! is found below ground surface (which in principle might happen due to
				! a high vertical intrinsic velocity)
				if(this % tvPart(iPart) % Zp < 0.d0) then
					this % tvPart(iPart) % filled = .false.
					cycle ! Abandon this particle to its dire destiny, avoiding it
						  ! to enter the expensive Langevin step
				end if
				
				! **************************
				! * Monte-Carlo simulation *
				! * of Langevin part       *
				! **************************
				
				! Compute Langevin time scale
				rootDeltat = sqrt(deltat)
				
				! Model's Langevin formulation is different within and above the PBL
				if(this % tvPart(iPart) % Zp < zi) then
				
					this % tvPart(iPart) % wp = &
						(alpha * this % tvPart(iPart) % wp ** 2 + gamma) * deltat + &
						exp(beta * deltat) * this % tvPart(iPart) % wp + &
						delta * rootDeltat * rnor()
					this % tvPart(iPart) % up = &
						exp(alpha_u * deltat) * this % tvPart(iPart) % up + &
						deltau * rootDeltat * rnor()
					this % tvPart(iPart) % vp = &
						exp(alpha_v * deltat) * this % tvPart(iPart) % vp + &
						deltav * rootDeltat * rnor()
						
				else
				
					this % tvPart(iPart) % wp = exp(alpha * deltat) * this % tvPart(iPart) % wp + delta * rootDeltat * rnor()
					this % tvPart(iPart) % up = exp(alpha_u * deltat) * this % tvPart(iPart) % up + deltau * rootDeltat * rnor()
					this % tvPart(iPart) % vp = exp(alpha_v * deltat) * this % tvPart(iPart) % vp + deltav * rootDeltat * rnor()
					
				end if

				! Update particle age
				this % tvPart(iPart) % Tp = this % tvPart(iPart) % Tp + deltat
				
			end do
			
		end do
		
	end function pplMove
	
	
	function pplCount(this) result(iNumPart)

		! Routine arguments
		class(ParticlePool), intent(inout)	:: this
		integer								:: iNumPart
		
		! Locals
		integer	:: i
		
		! Get the information desired
		iNumPart = 0
		if(allocated(this % tvPart)) then
			do i = 1, size(this % tvPart)
				if(this % tvPart(i) % filled) iNumPart = iNumPart + 1
			end do
		end if
		
	end function pplCount
	
end module Particles
