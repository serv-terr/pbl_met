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
		real(8)										:: dx
		real(8)										:: dy
		real(8)										:: maxAge
		! Current particle pool position in time
		real(8)										:: rTimeStamp
		! Timing
		real(8)										:: T_substep
		! Gridded receptor coordinates
		integer										:: nx
		integer										:: ny
		real(8), dimension(:), allocatable			:: xrec
		real(8), dimension(:), allocatable			:: yrec
		! Concentrations
		real(8), dimension(:,:), allocatable		:: C
		real(8), dimension(:,:), allocatable		:: Cmax
		real(8), dimension(:,:), allocatable		:: Csum
		! Particle pool
		type(Particle), dimension(:), allocatable	:: tvPart
		integer										:: maxpart
		integer										:: partIdx
		integer										:: partNum
		! Snapshot generation
		character(len=256)							:: sSnapFile
		character(len=256)							:: sSnapGridFile
		character(len=256)							:: sSnapListFile
		! Run times
		real										:: rTimeDrift
		real										:: rTimeDiffusion
		real										:: rTimeExpansion
		! Particle dynamics related counts
		integer										:: iNanEmit
		integer										:: iOutEmit
		integer										:: iNanDrift
		integer										:: iOutDrift
		integer										:: iNanDiffusion
		integer										:: iOutDiffusion
		integer										:: iNanLangevin
		integer										:: iOutLangevin
		integer										:: iNanExpansion
		integer										:: iOutExpansion
	contains
		procedure	:: Initialize => pplInit
		procedure	:: ResetConc  => pplResetC
		procedure	:: Emit       => pplEmit
		procedure	:: Move       => pplMove
		procedure	:: UpdateConc => pplConc
		procedure	:: Count      => pplCount
		procedure	:: SnapInit   => pplSnapInit
		procedure	:: SnapTake   => pplSnapTake
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
		this % xmin   = cfg % x0
		this % xmax   = cfg % x1
		this % ymin   = cfg % y0
		this % ymax   = cfg % y1
		this % zmin   = 0.d0
		this % zmax   = cfg % zmax
		this % dx     = cfg % dx
		this % dy     = cfg % dy
		this % maxAge = cfg % MaxAge
		
		! Assign snapshot path and related data files
		this % sSnapFile     = cfg % frameFile
		this % sSnapGridFile = trim(cfg % frameFile) // ".grd"
		this % sSnapListFile = trim(cfg % frameFile) // ".lst"
		
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
		if(allocated(this % Cmax)) deallocate(this % Cmax)
		allocate(this % Cmax(this % nx, this % ny))
		if(allocated(this % Csum)) deallocate(this % Csum)
		allocate(this % Csum(this % nx, this % ny))
		this % C    = 0.d0
		this % Cmax = 0.d0
		this % Csum = 0.d0
		
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
		this % partNum = 0
		
		! Initialize time
		this % rTimeStamp = 0.0d0
		
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
		type(MetProfValues)	:: met
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Assign current time stamp
		this % rTimeStamp = prf % rEpoch
		
		! Static point sources
		this % iNanEmit = 0
		this % iOutEmit = 0
		do iSource = 1, size(cfg % tvPointStatic)
		
			! Get source height, and use it to get the new particles' atmospheric parameters
			z = cfg % tvPointStatic(iSource) % z
			iErrCode = prf % evaluate( &
				cfg, &
				z, &
				met &
			)
			if(iErrCode /= 0) then
				iRetCode = 1
				return
			end if
		
			do iPart = 1, cfg % Np
			
				! Compute index of current particle in pool
				this % partIdx = this % partIdx + 1
				if(this % partIdx > this % maxpart) this % partIdx = 1
				this % partNum = min(this % partNum + 1, this % maxpart)
				
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
				this % tvPart(this % partIdx) % up = rnor() * sqrt(met % su2)
				this % tvPart(this % partIdx) % vp = rnor() * sqrt(met % sv2)
				this % tvPart(this % partIdx) % wp = rnor() * sqrt(met % sw2)
				
				! Check whether come speed initialization went not right
				if( &
					isnan(this % tvPart(this % partIdx) % up) .or. &
					isnan(this % tvPart(this % partIdx) % vp) .or. &
					isnan(this % tvPart(this % partIdx) % wp) &
				) then
					this % iNanEmit = this % iNanEmit + 1
				end if
				if( &
					abs(this % tvPart(this % partIdx) % up) > 1.d2 .or. &
					abs(this % tvPart(this % partIdx) % vp) > 1.d2 .or. &
					abs(this % tvPart(this % partIdx) % wp) > 1.d2 &
				) then
					this % iOutEmit = this % iOutEmit + 1
				end if
				
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
		type(MetProfValues)	:: met
		real(8)				:: deltat
		real(8)				:: zi
		real(8)				:: h0
		real(8)				:: rootDeltat
		real(8)				:: vel
		real(8)				:: sina
		real(8)				:: cosa
		real(8)				:: Coe
		real(8)				:: Tlh
		real(8)				:: Tlw
		real(8)				:: x0, y0, zbot
		real(8)				:: x1, y1, ztop
		real(8)				:: ampliX
		real(8)				:: ampliY
		real(8)				:: ampliZ
		real				:: rTime0
		real				:: rTime1
		real(8)				:: tota
		real(8)				:: minXp
		real(8)				:: maxXp
		real(8)				:: minYp
		real(8)				:: maxYp
		real(8)				:: minZp
		real(8)				:: maxZp
		real(8)				:: minUp
		real(8)				:: maxUp
		real(8)				:: minVp
		real(8)				:: maxVp
		real(8)				:: minWp
		real(8)				:: maxWp
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Here we follow the time evolution of each particle in sequence
		deltat = this % T_substep
		
		! Initializations
		this % rTimeDrift     = 0.
		this % rTimeDiffusion = 0.
		this % rTimeExpansion = 0.
		this % iNanDrift      = 0
		this % iOutDrift      = 0
		this % iNanDiffusion  = 0
		this % iOutDiffusion  = 0
		this % iNanExpansion  = 0
		this % iOutExpansion  = 0
		this % iNanLangevin   = 0
		this % iOutLangevin   = 0
		
		! Extra-domain boundaries
		ampliX = this % xmax - this % xmin
		ampliY = this % ymax - this % ymin
		ampliZ = this % zmax - this % zmin
		x0   = this % xmin - ampliX / 2.d0
		x1   = this % xmax + ampliX / 2.d0
		y0   = this % ymin - ampliY / 2.d0
		y1   = this % ymax + ampliY / 2.d0
		zbot = this % zmin - ampliZ / 2.d0
		ztop = this % zmax + ampliZ / 2.d0
			
		! ***********************
		! * Movement along wind *
		! ***********************
		
		call cpu_time(rTime0)
		do iPart = 1, this % partNum
		
			! Ensure the particle is alive before to proceed
			if(.not. this % tvPart(iPart) % filled) cycle
		
			! Set met environment
			z = this % tvPart(iPart) % Zp
			iErrCode = prf % evaluate( &
				cfg, &
				z, &
				met &
			)
			if(iErrCode /= 0) then
				iRetCode = 1
				return
			end if
			
			! Get wind direction through its directing cosines
			! (no need of trig function calls)
			vel  = sqrt(met % u**2 + met % v**2)
			if(vel > 1.d-2) then
				sina = met % v/vel
				cosa = met % u/vel
			else
				! In this case the Langevin dynamics would not strictly apply...
				sina = 0.d0
				cosa = 0.d0
				do
					call random_number(sina)
					call random_number(cosa)
					sina = 2.d0 * sina - 1.d0
					cosa = 2.d0 * cosa - 1.d0
					if(abs(sina) > 0.d0 .or. abs(cosa) > 0.d0) then
						tota = sqrt(sina**2 + cosa**2)
						sina = sina / tota
						cosa = cosa / tota
						exit
					end if
				end do
			end if
			
			! Get other useful data
			zi = cfg % tMeteo % rvExtZi(iSubStep)
			h0 = cfg % tMeteo % rvExtH0(iSubStep)
			
			! Update particle position
			this % tvPart(iPart) % Xp = this % tvPart(iPart) % Xp + &
				(met % u + this % tvPart(iPart) % up * cosa - this % tvPart(iPart) % vp * sina) * deltat
			this % tvPart(iPart) % Yp = this % tvPart(iPart) % Yp + &
				(met % v + this % tvPart(iPart) % up * sina + this % tvPart(iPart) % vp * cosa) * deltat
			this % tvPart(iPart) % Zp = this % tvPart(iPart) % Zp + &
				this % tvPart(iPart) % wp * deltat
				
			! Check if reflections occurred at ground or Zi
			if(this % tvPart(iPart) % Zp < 0.d0) then
				this % tvPart(iPart) % Zp = -this % tvPart(iPart) % Zp
				this % tvPart(iPart) % wp = -this % tvPart(iPart) % wp
			end if
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
			
		end do
		
		if(cfg % debug >= 3) then
			minXp =  huge(minXp)
			maxXp = -huge(maxXp)
			minYp =  huge(minYp)
			maxYp = -huge(maxYp)
			minZp =  huge(minZp)
			maxZp = -huge(maxZp)
			do iPart = 1, this % partNum
				if(this % tvPart(iPart) % filled) then
					minXp = min(minXp, this % tvPart(iPart) % Xp)
					maxXp = max(maxXp, this % tvPart(iPart) % Xp)
					minYp = min(minYp, this % tvPart(iPart) % Yp)
					maxYp = max(maxYp, this % tvPart(iPart) % Yp)
					minZp = min(minZp, this % tvPart(iPart) % Zp)
					maxZp = max(maxZp, this % tvPart(iPart) % Zp)
				end if
			end do
			print *, 'Particles positions report'
			print *, 'Xp> ', minXp, maxXp
			print *, 'Yp> ', minYp, maxYp
			print *, 'Zp> ', minZp, maxZp
			print *
		end if
		do iPart = 1, this % partNum
			if(.not. this % tvPart(iPart) % filled) cycle
			if(isnan(this % tvPart(iPart) % Xp) .or. isnan(this % tvPart(iPart) % Yp) .or. isnan(this % tvPart(iPart) % Zp)) then
				this % tvPart(iPart) % filled = .false.
				this % iNanDrift              = this % iNanDrift + 1
			end if
			if( &
				this % tvPart(iPart) % Xp < x0 .or. &
				this % tvPart(iPart) % Xp > x1 .or. &
				this % tvPart(iPart) % Yp < y0 .or. &
				this % tvPart(iPart) % Yp > y1 .or. &
				this % tvPart(iPart) % Zp < zbot .or. &
				this % tvPart(iPart) % Zp > ztop &
			) then
				this % tvPart(iPart) % filled = .false.
				this % iOutDrift              = this % iOutDrift + 1
			end if
		end do
		call cpu_time(rTime1)
		this % rTimeDrift = this % rTimeDrift + (rTime1 - rTime0)
		
		! **************************
		! * Monte-Carlo simulation *
		! * of Langevin part       *
		! **************************
			
		call cpu_time(rTime0)
		do iPart = 1, this % partNum
		
			! Ensure the particle is alive before to proceed
			if(.not. this % tvPart(iPart) % filled) cycle
		
			! Set met environment at drifted positions
			z = this % tvPart(iPart) % Zp
			iErrCode = prf % evaluate( &
				cfg, &
				z, &
				met &
			)
			if(iErrCode /= 0) then
				iRetCode = 1
				return
			end if
			
			! Get wind direction through its directing cosines
			! (no need of trig function calls)
			vel  = sqrt(met % u**2 + met % v**2)
			sina = met % v/vel
			cosa = met % u/vel
			
			! Get other useful data
			zi = cfg % tMeteo % rvExtZi(iSubStep)
			h0 = cfg % tMeteo % rvExtH0(iSubStep)
			
			! Compute Langevin time scale
			rootDeltat = sqrt(deltat)
			
			! Langevin equations
			this % tvPart(iPart) % up = met % Au * this % tvPart(iPart) % up + met % deltau * rootDeltat * rnor()
			this % tvPart(iPart) % vp = met % Av * this % tvPart(iPart) % vp + met % deltav * rootDeltat * rnor()
			if(this % tvPart(iPart) % Zp < zi) then
				this % tvPart(iPart) % wp = met % B * this % tvPart(iPart) % wp + &
					(met % alfa * this % tvPart(iPart) % wp ** 2 + met % gamma) * deltat + &
					met % delta * rootDeltat * rnor()
			else
				this % tvPart(iPart) % wp = met % A  * this % tvPart(iPart) % wp + met % delta * rootDeltat * rnor()
			end if
				
			! Check whether some speed initialization went not right
			if( &
				isnan(this % tvPart(this % partIdx) % up) .or. &
				isnan(this % tvPart(this % partIdx) % vp) .or. &
				isnan(this % tvPart(this % partIdx) % wp) &
			) then
				this % iNanLangevin = this % iNanLangevin + 1
			end if
			if( &
				abs(this % tvPart(this % partIdx) % up) > 1.d1 .or. &
				abs(this % tvPart(this % partIdx) % vp) > 1.d1 .or. &
				abs(this % tvPart(this % partIdx) % wp) > 1.d1 &
			) then
				this % iOutLangevin = this % iOutLangevin + 1
			end if
				
			! Update particle age
			this % tvPart(iPart) % Tp = this % tvPart(iPart) % Tp + deltat
			
		end do
				
		if(cfg % debug >= 3) then
			minUp =  huge(minUp)
			maxUp = -huge(maxUp)
			minVp =  huge(minVp)
			maxVp = -huge(maxVp)
			minWp =  huge(minWp)
			maxWp = -huge(maxWp)
			do iPart = 1, this % partNum
				if(this % tvPart(iPart) % filled) then
					minUp = min(minUp, this % tvPart(iPart) % up)
					maxUp = max(maxUp, this % tvPart(iPart) % up)
					minVp = min(minVp, this % tvPart(iPart) % vp)
					maxVp = max(maxVp, this % tvPart(iPart) % vp)
					minWp = min(minWp, this % tvPart(iPart) % wp)
					maxWp = max(maxWp, this % tvPart(iPart) % wp)
				end if
			end do
			print *, 'Velocity report'
			print *, 'Up> ', minUp, maxUp
			print *, 'Vp> ', minVp, maxVp
			print *, 'Wp> ', minWp, maxWp
			print *
		end if
		do iPart = 1, this % partNum
			if(.not. this % tvPart(iPart) % filled) cycle
			if(isnan(this % tvPart(iPart) % Xp) .or. isnan(this % tvPart(iPart) % Yp) .or. isnan(this % tvPart(iPart) % Zp)) then
				this % tvPart(iPart) % filled = .false.
				this % iNanDiffusion          = this % iNanDiffusion + 1
			end if
			if( &
				this % tvPart(iPart) % Xp < x0 .or. &
				this % tvPart(iPart) % Xp > x1 .or. &
				this % tvPart(iPart) % Yp < y0 .or. &
				this % tvPart(iPart) % Yp > y1 .or. &
				this % tvPart(iPart) % Zp < zbot .or. &
				this % tvPart(iPart) % Zp > ztop &
			) then
				this % tvPart(iPart) % filled = .false.
				this % iOutDiffusion          = this % iOutDiffusion + 1
			end if
		end do
		call cpu_time(rTime1)
		this % rTimeDiffusion = this % rTimeDiffusion + (rTime1 - rTime0)
				
		! *************************************
		! * Update the Gaussian kernel sigmas *
		! * (if Gaussian kernel desired)      *
		! *************************************
		
		call cpu_time(rTime0)
		if(cfg % iExecutionMode == 0) then
		
			do iPart = 1, this % partNum
		
				! Ensure the particle is alive before to proceed
				if(.not. this % tvPart(iPart) % filled) cycle
		
				! Set met environment at drifted positions
				z = this % tvPart(iPart) % Zp
				iErrCode = prf % evaluate( &
					cfg, &
					z, &
					met &
				)
				if(iErrCode /= 0) then
					iRetCode = 1
					return
				end if
			
				! Get wind direction through its directing cosines
				! (no need of trig function calls)
				vel  = sqrt(met % u**2 + met % v**2)
				sina = met % v/vel
				cosa = met % u/vel
			
				! Get other useful data
				zi = cfg % tMeteo % rvExtZi(iSubStep)
				h0 = cfg % tMeteo % rvExtH0(iSubStep)
			
				! Update particle sigmas
				Coe = 3.d0 * met % eps
				TLh = 2.d0 * met % su2 / Coe
				TLw = 2.d0 * met % sw2 / Coe
				if(this % tvPart(iPart) % Tp < TLh) then
					this % tvPart(iPart) % sh = this % tvPart(iPart) % sh + sqrt(met % su2) * deltat 
				else
					this % tvPart(iPart) % sh = sqrt(this % tvPart(iPart) % sh ** 2 + 2.d0 * TLh * met % su2 * deltat)
				end if
				if(this % tvPart(iPart) % Tp < TLw) then
					this % tvPart(iPart) % sz = this % tvPart(iPart) % sz + sqrt(met % sw2) * deltat 
				else
					this % tvPart(iPart) % sz = sqrt(this % tvPart(iPart) % sz ** 2 + 2.d0 * met % sw2 * deltat)
				end if
				
			end do
		
			!NaN_Idx = 0
			!do iPart = 1, this % partNum
			!	if(isnan(this % tvPart(iPart) % Zp) .or. isnan(this % tvPart(iPart) % Xp) .or. isnan(this % tvPart(iPart) % Zp)) then
			!		NaN_Idx = iPart
			!		print *, 'Expansion, NaN'
			!		print *
			!		print *, '  Index = ', NaN_Idx
			!		print *, '  zi    = ', zi
			!		print *, '  H0    = ', H0
			!	end if
			!end do
			do iPart = 1, this % partNum
				if(.not. this % tvPart(iPart) % filled) cycle
				if(isnan(this % tvPart(iPart) % Xp) .or. isnan(this % tvPart(iPart) % Yp) .or. isnan(this % tvPart(iPart) % Zp)) then
					this % tvPart(iPart) % filled = .false.
					this % iNanExpansion          = this % iNanExpansion + 1
				end if
				if( &
					this % tvPart(iPart) % Xp < x0 .or. &
					this % tvPart(iPart) % Xp > x1 .or. &
					this % tvPart(iPart) % Yp < y0 .or. &
					this % tvPart(iPart) % Yp > y1 .or. &
					this % tvPart(iPart) % Zp < zbot .or. &
					this % tvPart(iPart) % Zp > ztop &
				) then
					this % tvPart(iPart) % filled = .false.
					this % iOutExpansion          = this % iOutExpansion + 1
				end if
			end do
		
		else
		
			do iPart = 1, this % partNum
		
				this % tvPart(iPart) % sh = 0.d0
				this % tvPart(iPart) % sz = 0.d0
			
			end do
			
		end if
		call cpu_time(rTime1)
		this % rTimeExpansion = this % rTimeExpansion + (rTime1 - rTime0)
				
	end function pplMove


	! Add particle contribution to concentration on end of time substep, using one of the following methods:
	! 0: Gaussian kernel (ref: Yamada-Bunker, 1988)
	! 1: Direct count between 0 and 2 m,
	! 2: Direct count regardless of particle elevation above ground,
	! These processing types are determined by a configuration key ('exec_mode')
	! in the [General] section.
	function pplConc(this, cfg, iSubStep) result(iRetCode)
	
		! Routine arguments
		class(ParticlePool), intent(inout)	:: this
		type(Config), intent(in)			:: cfg
		integer, intent(in)					:: iSubStep
		integer								:: iRetCode
		
		! Locals
		integer				:: iErrCode
		integer				:: iPart
		integer				:: ix
		integer				:: iy
		real(8)				:: sigh4
		real(8)				:: zi
		real(8)				:: H0
		real(8)				:: dx
		real(8)				:: dy
		real(8)				:: ex
		real(8)				:: ey
		real(8)				:: ez
		real(8)				:: Cx
		real(8)				:: Cy
		real(8)				:: Cz
		real(8)				:: C0
		
		! Constants
		real(8), parameter	:: pi   = atan(1.d0)*4.d0
		real(8), parameter	:: pi2  = 2.d0*pi
		real(8), parameter	:: pi2r = (2.d0*pi)*sqrt(2.d0*pi)
		real(8), parameter	:: amin = 0.045d0
		real(8), parameter	:: amax = 15.d0
		real(8), parameter	:: zr   = 1.d0
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Decide the processing type
		if(cfg % iExecutionMode == 0) then
		
			! Main loop: iterate over particles, and add their contribution to receptors
			zi = cfg % tMeteo % rvExtZi(iSubStep)
			H0 = cfg % tMeteo % rvExtH0(iSubStep)
			do iPart = 1, size(this % tvPart)
				if(this % tvPart(iPart) % filled) then
				
					! Gaussian kernel is null at 4 sigma
					sigh4 = 4.d0 * this % tvPart(iPart) % sh
					
					do ix = 1, this % nx
						dx = this % xrec(ix) - this % tvPart(iPart) % Xp
						if(abs(dx) <= sigh4) then
							ex = 0.5d0 * (dx/this % tvPart(iPart) % sh)**2
							if(ex < amin) then
								Cx = 1.d0 - ex
							elseif(ex > amax) then
								Cx = 0.d0
							else
								Cx = exp(-ex)
							end if
							do iy = 1, this % ny
								dy = this % yrec(iy) - this % tvPart(iPart) % Yp
								if(abs(dy) <= sigh4) then
									ey = 0.5d0*(dy/this % tvPart(iPart) % sh)**2
									if(ey < amin) then
										Cy = 1.d0 - ey
									elseif(ey > amax) then
										Cy = 0.d0
									else
										Cy = exp(-ey)
									end if
									if(this % tvPart(iPart) % Zp < zi .and. H0 > 0.d0 .and. this % tvPart(iPart) % sz > 0.8d0*zi) then
										C0 = cfg % fat * this % tvPart(iPart) % Qp/(pi2 * this % tvPart(iPart) % sh ** 2)
										Cz = 1.d0/zi
									else
										C0 = cfg % fat * &
											this % tvPart(iPart) % Qp / &
											(pi2r * this % tvPart(iPart) % sh ** 2 * this % tvPart(iPart) % sz)
										ez = 0.5d0*(this % tvPart(iPart) % Zp / this % tvPart(iPart) % sz)**2
										if(ez < amin) then
											Cz = 1.d0 - ez
										elseif(ez > amax) then
											Cz = 0.d0
										else
											Cz = exp(-ez)
										end if
										Cz = 2.d0 * Cz
									end if
									this % C(ix,iy) = this % C(ix,iy) + C0 * Cx * Cy * Cz
								end if
							end do
						end if
					end do
		
				end if
			end do
		
		elseif(cfg % iExecutionMode == 1) then
		
			! Main loop: iterate over valid particles
			do iPart = 1, size(this % tvPart)
				if(this % tvPart(iPart) % filled) then
				
					! Check the particle is interesting
					if(this % tvPart(iPart) % Zp < 2.d0) then
						ix = nint((this % tvPart(iPart) % Xp - this % xmin) / this % Dx) + 1
						iy = nint((this % tvPart(iPart) % Yp - this % ymin) / this % Dy) + 1
						if(ix >= 1 .and. ix <= this % nx .and. iy >= 1 .and. iy <= this % ny) then
							this % C(ix,iy) = this % C(ix,iy) + &
												cfg % fat * this % tvPart(iPart) % Qp / (2.d0 * this % Dx * this % Dy)
						end if
					end if
				
				end if
			end do
				
		elseif(cfg % iExecutionMode == 2) then
		
			do iPart = 1, size(this % tvPart)
				if(this % tvPart(iPart) % filled) then
				
					! Particles are always interesting in this mode
					ix = nint((this % tvPart(iPart) % Xp - this % xmin) / this % Dx) + 1
					iy = nint((this % tvPart(iPart) % Yp - this % ymin) / this % Dy) + 1
					if(ix >= 1 .and. ix <= this % nx .and. iy >= 1 .and. iy <= this % ny) then
						this % C(ix,iy) = this % C(ix,iy) + cfg % fat * this % tvPart(iPart) % Qp / &
							(this % Dx * this % Dy * (this % zmax - this % zmin))
					end if
				
				end if
			end do
				
		end if
		
	end function pplConc

	
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
	
	
	function pplSnapInit(this, iLUN, rEpoch) result(iRetCode)
	
		! Routine arguments
		class(ParticlePool), intent(in)	:: this
		integer, intent(in)				:: iLUN
		real(8)							:: rEpoch	! Date and time of simulation beginning
		integer							:: iRetCode
		
		! Locals
		integer				:: iErrCode
		character(len=256)	:: sSnapGuideFile
		type(DateTime)		:: tDateTime
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check something is to be made
		if(this % sSnapFile == " ") return
		
		! Set date and time
		iErrCode = tDateTime % fromEpoch(rEpoch)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		
		! Check movie directory really exists
		sSnapGuideFile = trim(this % sSnapFile) // ".guide.txt"
		open(iLUN, file=sSnapGuideFile, status='unknown', action='write', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		write(iLUN, "(10(1x,e15.7),5(1x,i4),1x,f6.3)", iostat=iErrCode) &
			this % xmin, this % xmax, &
			this % ymin, this % ymax, &
			this % zmin, this % zmax, &
			this % dx,   this % dy, &
			this % maxAge, this % T_substep, &
			tDateTime % iYear, tDateTime % iMonth, tDateTime % iDay, &
			tDateTime % iHour, tDateTime % iMinute, tDateTime % rSecond
		if(iErrCode /= 0) then
			close(iLUN)
			iRetCode = 1
			return
		end if
		close(iLUN)
		! Post-condition: Guide file has been created, so likely the snaps will be too
		
		! Create snap list with empty contents
		open(iLUN, file = this % sSnapListFile, status='unknown', action='write')
		write(iLUN, "('Date, Total.Particles, In.Domain.Particles')")
		close(iLUN)
		
		! Create snap grid file
		open(iLUN, file = this % sSnapGridFile, status='unknown', action='write')
		write(iLUN, "(f10.2,5(',',f10.2))") &
			this % xmin, &
			this % xmax, &
			this % ymin, &
			this % ymax, &
			this % zmin, &
			this % zmax
		close(iLUN)
		
		! Create particle pool snap file, and fill its header
		open(iLUN, file = this % sSnapFile, status='unknown', action='write', access='stream')
		write(iLUN) this % xmin, this % xmax, this % ymin, this % ymax, this % zmin, this % zmax
		close(iLUN)
		
	end function pplSnapInit
	
	
	function pplSnapTake(this, iLUN, iSnap) result(iRetCode)
	
		! Routine arguments
		class(ParticlePool), intent(in)	:: this
		integer, intent(in)				:: iLUN
		integer, intent(in)				:: iSnap
		integer							:: iRetCode
		
		! Locals
		integer				:: iErrCode
		integer				:: iPart
		integer				:: iNumFilledPart
		integer				:: iNumPart
		type(DateTime)		:: dt
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check if something is to be made
		if(this % sSnapFile == " ") return
		
		! Count in-grid particles
		iNumPart       = 0
		iNumFilledPart = 0
		do iPart = 1, size(this % tvPart)
			if(this % tvPart(iPart) % filled) then
				iNumFilledPart = iNumFilledPart + 1
				if( &
					this % xmin <= this % tvPart(iPart) % Xp .and. this % tvPart(iPart) % Xp <= this % xmax .and. &
					this % ymin <= this % tvPart(iPart) % Yp .and. this % tvPart(iPart) % Yp <= this % ymax .and. &
					this % zmin <= this % tvPart(iPart) % Zp .and. this % tvPart(iPart) % Zp <= this % zmax &
				) then
					iNumPart = iNumPart + 1
				end if
			end if
		end do
		
		! Write active in-grid particles to file
		open(iLUN, file = this % sSnapFile, status='unknown', action='write', access='stream', position='append')
		write(iLUN) this % rTimeStamp, iNumPart
		do iPart = 1, size(this % tvPart)
			if(this % tvPart(iPart) % filled) then
				if( &
					this % xmin <= this % tvPart(iPart) % Xp .and. this % tvPart(iPart) % Xp <= this % xmax .and. &
					this % ymin <= this % tvPart(iPart) % Yp .and. this % tvPart(iPart) % Yp <= this % ymax .and. &
					this % zmin <= this % tvPart(iPart) % Zp .and. this % tvPart(iPart) % Zp <= this % zmax &
				) then
					write(iLUN) &
						real(this % tvPart(iPart) % Xp, kind=4), &
						real(this % tvPart(iPart) % Yp, kind=4), &
						real(this % tvPart(iPart) % Zp, kind=4), &
						real(this % tvPart(iPart) % Qp, kind=4), &
						real(this % tvPart(iPart) % Tp, kind=4)
				end if
			end if
		end do
		close(iLUN)
		
		! Append row to snapshot list
		open(iLUN, file=this % sSnapListFile, status='old', action='write', position='append')
		iErrCode = dt % fromEpoch(this % rTimeStamp)
		write(iLUN, "(a23, 2(',', i10))") dt % toISO(), iNumFilledPart, iNumPart
		close(iLUN)
		
	end function pplSnapTake
	
end module Particles
