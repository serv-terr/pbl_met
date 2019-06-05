module Configuration
	
	use pbl_met
	use pbl_simil
	use Emission
	use Meteo
	
	implicit none
	
	type Config
		! Status
		logical				:: lIsFull = .false.
		! General
		integer				:: debug
		character(len=256)	:: diag
		integer				:: frameInterval
		character(len=256)	:: frameFile
		character(len=256)	:: profilePath
		integer				:: iExecutionMode	! 0: Gaussian kernel; 1:Direct count "at ground"; 2:Direct count regardless of Z value
		! Grid data
		real(8)				:: x0
		real(8)				:: y0
		integer				:: nx
		integer 			:: ny
		real(8)				:: dx
		real(8)				:: dy
		integer				:: nz
		real(8)				:: dz
		! Timing
		integer				:: Tmed		! Averaging time (s)
		integer				:: Nstep	! Number of substeps in an averaging period
		integer				:: Np		! Number of particles released per source per substep
		integer				:: MaxAge	! Maximum particle age (s)
		! Static and dynamic emissions
		character(len=256)	:: Filemis	
		character(len=256)	:: Fileprofemi	
		! Meteo data files
		character(len=256)	:: Filemeteo
		character(len=256)	:: FilemeteoOut	! May be an empty string
		character(len=256)	:: metDiaFile
		! Site parameters of meteorological file
		real(8)				:: zlev
		real(8)				:: z0
		real(8)				:: zr
		real(8)				:: zt
		real(8)				:: gamma
		integer				:: hemisphere	! 0:Southern, 1:Northern
		! Output
		character(len=256)	:: Fileout
		character(len=256)	:: FileMean
		character(len=256)	:: FileGridAvg
		character(len=256)	:: FileGridMax
		real(8)				:: fat
		! Computed parameters
		real(8)				:: x1
		real(8)				:: y1
		real(8)				:: zmax
		integer				:: maxpart
		! Emissions space
		type(PointSource), dimension(:), allocatable	:: tvPointStatic
		! Meteo data
		type(MetData)									:: tMeteo
	contains
		procedure			:: read               => cfgRead
		procedure			:: getNumTimeSteps    => cfgGetTimeSteps
		procedure			:: getNumTimeSubSteps => cfgGetTimeSubSteps
		procedure			:: getNumMeteo        => cfgGetMeteoSize
	end type Config
	
	
	type MetProfiles
		! Time stamp
		real(8)								:: rEpoch	! Time stamp of current profile set
		! Primitive profiles
		real(8), dimension(:), allocatable	:: z		! Levels' height above ground (m)
		real(8), dimension(:), allocatable	:: u		! U components (m/s)
		real(8), dimension(:), allocatable	:: v		! V components (m/s)
		real(8), dimension(:), allocatable	:: T		! Temperatures (K)
		real(8), dimension(:), allocatable	:: su2		! var(U) values (m2/s2)
		real(8), dimension(:), allocatable	:: sv2		! var(V) values (m2/s2)
		real(8), dimension(:), allocatable	:: sw2		! var(W) values (m2/s2)
		real(8), dimension(:), allocatable	:: dsw2		! d var(W) / dz (m/s2)
		real(8), dimension(:), allocatable	:: eps		! TKE dissipation rate
		real(8), dimension(:), allocatable	:: alfa		! Langevin equation coefficient
		real(8), dimension(:), allocatable	:: beta		! Langevin equation coefficient
		real(8), dimension(:), allocatable	:: gamma	! Langevin equation coefficient
		real(8), dimension(:), allocatable	:: delta	! Langevin equation coefficient
		real(8), dimension(:), allocatable	:: alfa_u	! Langevin equation coefficient
		real(8), dimension(:), allocatable	:: alfa_v	! Langevin equation coefficient
		real(8), dimension(:), allocatable	:: deltau	! Langevin equation coefficient
		real(8), dimension(:), allocatable	:: deltav	! Langevin equation coefficient
		real(8), dimension(:), allocatable	:: deltat	! Langevin equation coefficient
		! Convenience derived values
		real(8), dimension(:), allocatable	:: Au		! exp(alfa_u*dt)
		real(8), dimension(:), allocatable	:: Av		! exp(alfa_v*dt)
		real(8), dimension(:), allocatable	:: A		! exp(alfa*dt)
		real(8), dimension(:), allocatable	:: B		! exp(beta*dt)
	contains
		procedure	:: clean     => metpClean
		procedure	:: alloc     => metpAlloc
		procedure	:: create    => metpCreate
		procedure	:: evaluate  => metpEvaluate
		procedure	:: dump      => metpDump
	end type MetProfiles
	
	
	type MetProfValues
		real(8)	:: rEpoch	! Time stamp of current profile set
		real(8)	:: z		! Levels' height above ground (m)
		real(8)	:: u		! U components (m/s)
		real(8)	:: v		! V components (m/s)
		real(8)	:: T		! Temperatures (K)
		real(8)	:: su2		! var(U) values (m2/s2)
		real(8)	:: sv2		! var(V) values (m2/s2)
		real(8)	:: sw2		! var(W) values (m2/s2)
		real(8)	:: dsw2		! d var(W) / dz (m/s2)
		real(8)	:: eps		! TKE dissipation rate
		real(8)	:: alfa		! Langevin equation coefficient
		real(8)	:: beta		! Langevin equation coefficient
		real(8)	:: gamma	! Langevin equation coefficient
		real(8)	:: delta	! Langevin equation coefficient
		real(8)	:: alfa_u	! Langevin equation coefficient
		real(8)	:: alfa_v	! Langevin equation coefficient
		real(8)	:: deltau	! Langevin equation coefficient
		real(8)	:: deltav	! Langevin equation coefficient
		real(8)	:: deltat	! Langevin equation coefficient
		real(8)	:: Au		! exp(alfa_u*dt)
		real(8)	:: Av		! exp(alfa_v*dt)
		real(8)	:: A		! exp(alfa*dt)
		real(8)	:: B		! exp(beta*dt)
	end type MetProfValues
	
contains

	function cfgRead(this, iLUN, iLUN1, sFileName) result(iRetCode)
	
		! Routine arguments
		class(Config), intent(out)		:: this
		integer, intent(in)				:: iLUN
		integer, intent(in)				:: iLUN1
		character(len=*), intent(in)	:: sFileName
		integer							:: iRetCode
		
		! Locals
		integer				:: iErrCode
		type(IniFile)		:: cfg
		character(len=128)	:: sBuffer
		integer				:: iNumData
		integer				:: iData
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		this % lIsFull = .false.
		
		! Get configuration file, and prepare to parse it
		iErrCode = cfg % read(10, sFileName)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		
		! Gather configuration data
		! -1- General
		iErrCode = cfg % getInteger("General", "debug_level", this % debug, 0)
		if(iErrCode /= 0) then
			iRetCode = 2
			return
		end if
		iErrCode = cfg % getString("General", "diafile", this % diag, "")
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'diag_file' in [General]"
			return
		end if
		iErrCode = cfg % getInteger("General", "frame_interval", this % frameInterval, 0)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'frame_interval' in [General]"
			return
		end if
		iErrCode = cfg % getString("General", "frame_name", this % frameFile, "")
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'frame_name' in [General]"
			return
		end if
		iErrCode = cfg % getString("General", "profile_path", this % profilePath, "")
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'profile_path' in [General]"
			return
		end if
		iErrCode = cfg % getInteger("General", "exec_mode", this % iExecutionMode, 0)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'exec_mode' in [General]"
			return
		end if
		! -1- Timing
		iErrCode = cfg % getInteger("Timing", "avgtime", this % Tmed, 3600)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'avgtime' in [Timing]"
			return
		end if
		iErrCode = cfg % getInteger("Timing", "nstep", this % Nstep, 360)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'nstep' in [Timing]"
			return
		end if
		iErrCode = cfg % getInteger("Timing", "npart", this % Np, 100)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'npart' in [Timing]"
			return
		end if
		iErrCode = cfg % getInteger("Timing", "maxage", this % MaxAge, 5*3600)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'maxage' in [Timing]"
			return
		end if
		! -1- Emission
		iErrCode = cfg % getString("Emission", "static", this % Filemis, "")
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'static' in [Emission]"
			return
		end if
		iErrCode = cfg % getString("Emission", "dynamic", this % Fileprofemi, "")
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'dynamic' in [Emission]"
			return
		end if
		! -1- Meteo
		iErrCode = cfg % getString("Meteo", "inpfile", this % Filemeteo, "")
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'inpfile' in [Meteo]"
			return
		end if
		iErrCode = cfg % getString("Meteo", "outfile", this % FilemeteoOut, "")
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'inpfile' in [Meteo]"
			return
		end if
		iErrCode = cfg % getString("Meteo", "diafile", this % metDiaFile, "")
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'inpfile' in [Meteo]"
			return
		end if
		iErrCode = cfg % getReal8("Meteo", "height", this % zlev, -9999.9d0)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'height' in [Meteo]"
			return
		end if
		iErrCode = cfg % getReal8("Meteo", "z0", this % z0, 0.02d0)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'z0' in [Meteo]"
			return
		end if
		iErrCode = cfg % getReal8("Meteo", "zr", this % zr, 10.d0)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'zr' in [Meteo]"
			return
		end if
		iErrCode = cfg % getReal8("Meteo", "zt", this % zt, 2.d0)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'zt' in [Meteo]"
			return
		end if
		iErrCode = cfg % getReal8("Meteo", "gamma", this % gamma, -0.0098d0)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'gamma' in [Meteo]"
			return
		end if
		iErrCode = cfg % getInteger("Meteo", "hemisphere", this % hemisphere, 1)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'hemisphere' in [Meteo]"
			return
		end if
		! -1- Output
		iErrCode = cfg % getString("Output", "conc", this % Fileout, "")
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'conc' in [Output]"
			return
		end if
		iErrCode = cfg % getString("Output", "mean", this % FileMean, "")
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'mean' in [Output]"
			return
		end if
		iErrCode = cfg % getString("Output", "gridAvg", this % FileGridAvg, "")
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'gridMax' in [Output]"
			return
		end if
		iErrCode = cfg % getString("Output", "gridMax", this % FileGridMax, "")
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'gridMax' in [Output]"
			return
		end if
		iErrCode = cfg % getReal8("Output", "x0", this % x0, -9999.9d0)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'x0' in [Output]"
			return
		end if
		iErrCode = cfg % getReal8("Output", "y0", this % y0, -9999.9d0)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'y0' in [Output]"
			return
		end if
		iErrCode = cfg % getInteger("Output", "nx", this % nx, -9999)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'nx' in [Output]"
			return
		end if
		iErrCode = cfg % getInteger("Output", "ny", this % ny, -9999)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'ny' in [Output]"
			return
		end if
		iErrCode = cfg % getReal8("Output", "dx", this % dx, -9999.9d0)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'nx' in [Output]"
			return
		end if
		iErrCode = cfg % getReal8("Output", "dy", this % dy, -9999.9d0)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'ny' in [Output]"
			return
		end if
		iErrCode = cfg % getInteger("Output", "nz", this % nz, -9999)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'nz' in [Output]"
			return
		end if
		iErrCode = cfg % getReal8("Output", "dz", this % dz, -9999.9d0)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'dz' in [Output]"
			return
		end if
		iErrCode = cfg % getReal8("Output", "factor", this % fat, -9999.9d0)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'factor' in [Output]"
			return
		end if
		
		! Validate configuration data
		! -1- Timing
		if(this % Tmed <= 0 .or. this % Tmed > 3600 .or. mod(3600, this % Tmed) /= 0) then
			iRetCode = 3
			if(this % debug > 0) print *, "alamo:: error: Invalid value of 'avgtime' in [Timing]"
			return
		end if
		if(this % Nstep < 1 .or. mod(this % Tmed, this % Nstep) /= 0) then
			iRetCode = 3
			if(this % debug > 0) print *, "alamo:: error: Invalid value of 'nstep' in [Timing]"
			return
		end if
		if(this % Np < 1) then
			iRetCode = 3
			if(this % debug > 0) print *, "alamo:: error: Invalid value of 'npart' in [Timing]"
			return
		end if
		if(this % MaxAge < 1) then
			iRetCode = 3
			if(this % debug > 0) print *, "alamo:: error: Invalid value of 'maxage' in [Timing]"
			return
		end if
		if(this % debug > 1) print *, "alamo:: info: [Timing] section check done"
		! -1- Output
		if(this % x0 < -9990.d0 .or. this % y0 < -9990.d0) then
			iRetCode = 3
			if(this % debug > 0) print *, "alamo:: error: Invalid value of 'x0' or 'y0' in [Output]"
			return
		end if
		if(this % nx <= 0 .or. this % ny <= 0) then
			iRetCode = 3
			if(this % debug > 0) print *, "alamo:: error: Invalid value of 'nx' or 'ny' in [Output]"
			return
		end if
		if(this % dx <= 0.d0 .or. this % dy <= 0.d0) then
			iRetCode = 3
			if(this % debug > 0) print *, "alamo:: error: Invalid value of 'dx' or 'dy' in [Output]"
			return
		end if
		if(this % nz <= 1) then
			iRetCode = 3
			if(this % debug > 0) print *, "alamo:: error: Invalid value of 'nz' in [Output]"
			return
		end if
		if(this % dz <= 0.d0) then
			iRetCode = 3
			if(this % debug > 0) print *, "alamo:: error: Invalid value of 'dz' in [Output]"
			return
		end if
		if(this % fat <= 0.d0) then
			iRetCode = 3
			if(this % debug > 0) print *, "alamo:: error: Invalid value of 'factor' in [Output]"
			return
		end if
		this % x1   = this % x0 + this % dx * (this % nx - 1)
		this % y1   = this % y0 + this % dy * (this % ny - 1)
		this % zmax = this % dz * (this % nz - 1)
		if(this % debug > 1) print *, "alamo:: info: [Output] section check done"
		! -1- Static emissions
		if(allocated(this % tvPointStatic)) deallocate(this % tvPointStatic)
		if(this % Filemis /= ' ') then
			open(iLUN1, file=this % Filemis, status='old', action='read', iostat=iErrCode)
			if(iErrCode /= 0) then
				iRetCode = 3
				if(this % debug > 0) print *, "alamo:: error: File 'static' in [Emission] is missing / cannot be opened"
				return
			end if
			iNumData = 0
			do
				read(iLUN1, "(a)", iostat=iErrCode) sBuffer
				if(iErrCode /= 0) exit
				iNumData = iNumData + 1
			end do
			if(iNumData <= 0) then
				iRetCode = 3
				if(this % debug > 0) print *, "alamo:: error: File 'static' in [Emission] exists but is empty"
				close(iLUN1)
				return
			end if
			rewind(iLUN1)
			allocate(this % tvPointStatic(iNumData))
			do iData = 1, iNumData
				iErrCode = this % tvPointStatic(iData) % read(iLUN1)
				if(this % tvPointStatic(iData) % isOutOfDomain(this % x0,this % x1,this % y0,this % y1,this % zmax)) then
					iRetCode = 3
					if(this % debug > 0) &
						print *, "alamo:: error: ", iData, "-th emission in file 'static' in [Emission] is off domain"
					close(iLUN1)
					return
				end if
			end do
			close(iLUN1)
			if(this % debug > 1) print *, "alamo:: info: [Emission] section check done for static sources"
		else
			allocate(this % tvPointStatic(0))
		end if
		! -1- Meteorological data
		if(this % zlev < 0.d0) then
			iRetCode = 3
			if(this % debug > 0) print *, "alamo:: error: Invalid value of 'height' in [Meteo]"
			return
		end if
		if(this % z0 < 0.d0) then
			iRetCode = 3
			if(this % debug > 0) print *, "alamo:: error: Invalid value of 'z0' in [Meteo]"
			return
		end if
		if(this % zr <= 0.d0) then
			iRetCode = 3
			if(this % debug > 0) print *, "alamo:: error: Invalid value of 'zr' in [Meteo]"
			return
		end if
		if(this % zt <= 0.d0) then
			iRetCode = 3
			if(this % debug > 0) print *, "alamo:: error: Invalid value of 'zt' in [Meteo]"
			return
		end if
		if(this % gamma >= 0.d0) then
			iRetCode = 3
			if(this % debug > 0) print *, "alamo:: error: Invalid value of 'gamma' in [Meteo]"
			return
		end if
		if(this % hemisphere < 0 .or. this % hemisphere > 1) then
			iRetCode = 3
			if(this % debug > 0) print *, "alamo:: error: Invalid value of 'hemisphere' in [Meteo]"
			return
		end if
		iErrCode = this % tMeteo % read(iLUN1, this % Filemeteo, this % Tmed, this % Nstep, this % FilemeteoOut)
		if(iErrCode /= 0) then
			iRetCode = 3
			if(this % debug > 0) print *, "alamo:: error: Meteo data not read, with return code ", iErrCode
			return
		end if
		if(this % debug > 1) print *, "alamo:: info: [Meteo] section check done"
		! -1- General
		if(this % frameInterval < 0) then
			iRetCode = 3
			if(this % debug > 0) print *, "alamo:: error: Invalid value of 'frame_interval' in [General]"
			return
		end if
		if(this % iExecutionMode < 0 .or. this % iExecutionMode > 2) then
			iRetCode = 3
			if(this % debug > 0) print *, "alamo:: error: Invalid value of 'exec_mode' in [General]"
			return
		end if
		
		if(this % debug > 1) print *, "alamo:: info: [General] section check done"
		
		! Compute the number of particles per step
		this % maxpart = this % Np * size(this % tvPointStatic) * this % Nstep * this % MaxAge / this % Tmed
		if(this % debug > 1) print *, "alamo:: info: Maximum number of particles equal to ", this % maxpart
	
		! Leave
		this % lIsFull = .true.
		
	end function cfgRead
	
	
	function cfgGetMeteoSize(this) result(iMeteoSize)
	
		! Routine arguments
		class(Config), intent(in)	:: this
		integer						:: iMeteoSize
		
		! Locals
		! --none--
		
		! Get the information piece desired
		if(this % lIsFull) then
			iMeteoSize = size(this % tMeteo % rvExtEpoch)
		else
			iMeteoSize = 0
		end if
		
	end function cfgGetMeteoSize
	
	
	function cfgGetTimeSteps(this) result(iMeteoSize)
	
		! Routine arguments
		class(Config), intent(in)	:: this
		integer						:: iMeteoSize
		
		! Locals
		! --none--
		
		! Get the information piece desired
		if(this % lIsFull) then
			iMeteoSize = size(this % tMeteo % rvEpoch)
		else
			iMeteoSize = 0
		end if
		
	end function cfgGetTimeSteps
	
	
	function cfgGetTimeSubSteps(this) result(iMeteoSize)
	
		! Routine arguments
		class(Config), intent(in)	:: this
		integer						:: iMeteoSize
		
		! Locals
		! --none--
		
		! Get the information piece desired
		if(this % lIsFull) then
			iMeteoSize = size(this % tMeteo % rvExtEpoch) / size(this % tMeteo % rvEpoch)
		else
			iMeteoSize = 0
		end if
		
	end function cfgGetTimeSubSteps
	
	
	function metpClean(this) result(iRetCode)
	
		! Routine arguments
		class(MetProfiles), intent(out)		:: this
		integer								:: iRetCode
		
		! Locals
		! --none--
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Clean-up workspace
		if(allocated(this % z))      deallocate(this % z)
		if(allocated(this % u))      deallocate(this % u)
		if(allocated(this % v))      deallocate(this % v)
		if(allocated(this % T))      deallocate(this % T)
		if(allocated(this % su2))    deallocate(this % su2)
		if(allocated(this % sv2))    deallocate(this % sv2)
		if(allocated(this % sw2))    deallocate(this % sw2)
		if(allocated(this % dsw2))   deallocate(this % dsw2)
		if(allocated(this % eps))    deallocate(this % eps)
		if(allocated(this % alfa))   deallocate(this % alfa)
		if(allocated(this % beta))   deallocate(this % beta)
		if(allocated(this % gamma))  deallocate(this % gamma)
		if(allocated(this % delta))  deallocate(this % delta)
		if(allocated(this % alfa_u)) deallocate(this % alfa_u)
		if(allocated(this % alfa_v)) deallocate(this % alfa_v)
		if(allocated(this % deltau)) deallocate(this % deltau)
		if(allocated(this % deltav)) deallocate(this % deltav)
		if(allocated(this % deltat)) deallocate(this % deltat)
		if(allocated(this % Au))     deallocate(this % Au)
		if(allocated(this % Av))     deallocate(this % Av)
		if(allocated(this % A))      deallocate(this % A)
		if(allocated(this % B))      deallocate(this % B)
		
	end function metpClean
	
	
	function metpAlloc(this, iNumData) result(iRetCode)
	
		! Routine arguments
		class(MetProfiles), intent(out)		:: this
		integer, intent(in)					:: iNumData
		integer								:: iRetCode
		
		! Locals
		integer		:: iErrCode
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check parameters
		if(iNumData <= 0) then
			iRetCode = 1
			return
		end if
		
		! Reserve workspace
		allocate( &
			this % z(iNumData), &
			this % u(iNumData), &
			this % v(iNumData), &
			this % T(iNumData), &
			this % su2(iNumData), &
			this % sv2(iNumData), &
			this % sw2(iNumData), &
			this % dsw2(iNumData), &
			this % eps(iNumData), &
			this % alfa(iNumData), &
			this % beta(iNumData), &
			this % gamma(iNumData), &
			this % delta(iNumData), &
			this % alfa_u(iNumData), &
			this % alfa_v(iNumData), &
			this % deltau(iNumData), &
			this % deltav(iNumData), &
			this % deltat(iNumData), &
			this % Au(iNumData), &
			this % AV(iNumData), &
			this % A(iNumData), &
			this % B(iNumData), &
			stat = iErrCode &
		)
		if(iRetCode /= 0) then
			iRetCode = 2
			return
		end if
		
	end function metpAlloc
	
	
	function metpCreate( &
		this, &
		cfg, &
		i &		! Index of current row in 'met'
	) result(iRetCode)
	
		! Routine arguments
		class(MetProfiles), intent(out)		:: this
		type(Config), intent(in)			:: cfg
		integer, intent(in)					:: i
		integer								:: iRetCode
		
		! Locals
		integer	:: n		! Max number of met data
		integer	:: m		! Number of levels
		integer	:: j
		integer	:: iErrCode
		real(8)	:: Ta		! Absolute temperature (K)
		real(8)	:: Pres		! Air pressure (hPa)
		real(8)	:: rc		! rho*Cp
		real(8)	:: wT		! mean(w'T')
		real(8)	:: hL		! 1/L
		real(8)	:: Ts		! Scale temperature (°C)
		real(8)	:: ws		! Deardoff velocity (m/s)
		real(8)	:: C0u
		real(8)	:: C0v
		real(8)	:: C0w
		real(8)	:: C0uu
		real(8)	:: C0vv
		real(8)	:: C0ww
		real(8)	:: ssw2_2
		real(8)	:: dt
		type(DateTime)	:: tStamp
		character(len=23)	:: sTimeStamp
		
		! Constants
		real(8), parameter	:: K    = 0.4d0		! von Karman constant
		real(8), parameter	:: G    = 9.81d0	! Universal gravity constant
		real(8), parameter	:: P0   = 1013.d0	! Pressure assumed at 0m MSL
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check critical parameters
		n = size(cfg % tMeteo % rvExtEpoch)
		if(i < 1 .or. i > n) then
			iRetCode = 1
			return
		end if
		
		! Initialize
		m = cfg % nz
		iErrCode = this % clean()
		if(iErrCode /= 0) then
			iRetCode = 2
			return
		end if
		iErrCode = this % alloc(m)
		if(iErrCode /= 0) then
			iRetCode = 3
			return
		end if
		this % z = [(cfg % z0 + (j-1) * cfg % dz, j = 1, m)]
		Ta = cfg % tMeteo % rvExtTemp(i) + 273.15d0
		
		! Assign time stamp
		this % rEpoch = cfg % tMeteo % rvExtEpoch(i)
		
		! Estimate ground pressure at site
		Pres = P0 * exp(-0.0342d0 * cfg % zlev / Ta)

		! Estimation of RhoCp and wT (harmless, and not passed as 'met' data to avoid clutter)
		rc = 350.125d0 * Pres / Ta
		wT = cfg % tMeteo % rvExtH0(i) / rc
		
		! Reciprocal of Obukhov length
		hL = -K*G/Ta * wT / cfg % tMeteo % rvExtUstar(i)**3

		! Scale temperature
		Ts = -wT / cfg % tMeteo % rvExtUstar(i)

		! Deardoff velocity
		ws = wStar(real(Ta,kind=4), real(cfg % tMeteo % rvExtH0(i),kind=4), real(cfg % tMeteo % rvExtZi(i),kind=4))
		
		! Estimate wind and temperature profiles, based on SL similarity
		iErrCode = WindProfile( &
			cfg % hemisphere, &
			this % z, &
			cfg % zr, &
			cfg % tMeteo % rvExtVel(i), &
			cfg % tMeteo % rvExtDir(i), &
			cfg % z0, &
			cfg % tMeteo % rvExtZi(i), &
			cfg % tMeteo % rvExtUstar(i), &
			hL, &
			this % u, &
			this % v &
		)
		if(iErrCode /= 0) then
			iRetCode = 4
			return
		end if
		iErrCode = TempProfile( &
			this % z, &
			cfg % z0, &
			cfg % zt, &
			Ta, &
			-cfg % gamma, &
			cfg % tMeteo % rvExtZi(i), &
			Ts, &
			cfg % tMeteo % rvExtUstar(i), &
			hL, &
			this % T &
		)
		if(iErrCode /= 0) then
			iRetCode = 5
			return
		end if
		
		! Estimate vertical and horizontal sigmas
		iErrCode = VerticalWindVarProfile( &
			this % z, &
			cfg % tMeteo % rvExtUstar(i), &
			ws, &
			cfg % z0, &
			cfg % tMeteo % rvExtZi(i), &
			this % sw2, &
			this % dsw2 &
		)
		if(iErrCode /= 0) then
			iRetCode = 6
			return
		end if
		iErrCode = HorizontalWindVarProfile( &
			this % z, &
			cfg % tMeteo % rvExtUstar(i), &
			ws, &
			cfg % tMeteo % rvExtZi(i), &
			this % su2, &
			this % sv2 &
		)
		if(iErrCode /= 0) then
			iRetCode = 7
			return
		end if

		! TKE dissipation
		iErrCode = TKEDissipationProfile( &
			this % z, &
			cfg % tMeteo % rvExtUstar(i), &
			ws, &
			cfg % z0, &
			cfg % tMeteo % rvExtZi(i), &
			this % eps &
		)
		if(iErrCode /= 0) then
			iRetCode = 8
			return
		end if

		! Kolmogorov coefficients, used in further calculations
		iErrCode = KolmogorovConstants(ws, C0u, C0v, C0w)
		if(iErrCode /= 0) then
			print *, iErrCode
			iRetCode = 9
			return
		end if

		! Langevin coefficients and optimal time step (a function
		! of vertical Lagrangian decorrelation time)
		do j = 1, m
			if(ws > 0.) then
				! Convective
				C0uu   = C0u * this % eps(j)
				C0vv   = C0v * this % eps(j)
				C0ww   = C0w * this % eps(j)
				ssw2_2 = 2.d0 * this % sw2(j)
				if(this % z(j) <= cfg % tMeteo % rvExtZi(i)) then
					! Inside the PBL
					
					! Langevin coefficients for W component
					this % alfa(j)   = this % dsw2(j) / ssw2_2 
					this % beta(j)   = -C0ww / ssw2_2 		
					this % gamma(j)  = 0.5d0 * this % dsw2(j)
					this % delta(j)  = sqrt(C0ww)
					
					! Optimal time step
					this % deltat(j) = 0.1d0 * ssw2_2 / C0ww
					
					! Langevin coefficients for U, V component
					this % alfa_u(j) = -C0uu / (2.d0 * this % su2(j))
					this % alfa_v(j) = -C0vv / (2.d0 * this % sv2(j))
					this % deltau(j) = sqrt(C0uu)
					this % deltav(j) = sqrt(C0vv)
					
				else
					! Above the PBL
				
					! Langevin coefficients for W component
					this % alfa(j)   = 0.d0
					this % beta(j)   = -C0ww / ssw2_2 		
					this % gamma(j)  = 0.d0
					this % delta(j)  = sqrt(C0ww)
					
					! Optimal time step
					this % deltat(j) = 100.d0	! Not used, in reality: just an indication
					
					! Langevin coefficients for U, V component
					this % alfa_u(j) = -C0uu / (2.d0 * this % su2(j))
					this % alfa_v(j) = -C0vv / (2.d0 * this % sv2(j))
					this % deltau(j) = sqrt(C0uu)
					this % deltav(j) = sqrt(C0vv)
					
				end if
				
			else
				! Stable

				! Langevin coefficients for W component
				C0ww             = C0w * this % eps(j)
				ssw2_2           = 2.d0 * this % sw2(j)
				this % alfa(j)   = this % dsw2(j) / ssw2_2 
				this % beta(j)   = -C0ww / ssw2_2 		
				this % gamma(j)  = 0.5d0 * this % dsw2(j)
				this % delta(j)  = sqrt(C0ww)
				
				! Optimal time step
				this % deltat(j) = 0.1d0 * ssw2_2/C0ww
			
				! Langevin coefficients for U, V component
				C0uu             = C0u * this % eps(j)
				C0vv             = C0v * this % eps(j)
				this % alfa_u(j) = -C0uu / (2.d0 * this % su2(j))
				this % alfa_v(j) = -C0vv / (2.d0 * this % sv2(j))
				this % deltau(j) = sqrt(C0uu)
				this % deltav(j) = sqrt(C0vv)
				
			end if
			
		end do
		
		! Convenience values
		dt        = real(cfg % Tmed, kind=8) / real(cfg % Nstep, kind=8)
		this % Au = exp(this % alfa_u * dt)
		this % Av = exp(this % alfa_v * dt)
		this % A  = exp(this % alfa * dt)
		this % B  = exp(this % beta * dt)
		
		! Diagnostic printouts (provisional)
		if(cfg % debug >= 3) then
			iErrCode = tStamp % fromEpoch(cfg % tMeteo % rvExtEpoch(i))
			sTimeStamp = tStamp % ToISO()
			print *, "Meteo profiles range report for step at ", sTimeStamp
			print *, "U>      ", minval(this % u), maxval(this % u)
			print *, "V>      ", minval(this % v), maxval(this % v)
			print *, "Vel>    ", minval(sqrt(this % u**2 + this % v**2)), maxval(sqrt(this % u**2 + this % v**2))
			print *, "T>      ", minval(this % T), maxval(this % T)
			print *, "sU2>    ", minval(this % su2), maxval(this % su2)
			print *, "sV2>    ", minval(this % sv2), maxval(this % sv2)
			print *, "sW2>    ", minval(this % sw2), maxval(this % sw2)
			print *, "dsW2>   ", minval(this % dsW2), maxval(this % dsW2)
			print *, "eps>    ", minval(this % eps), maxval(this % eps)
			print *, "alpha>  ", minval(this % alfa), maxval(this % alfa)
			print *, "alphau> ", minval(this % alfa_u), maxval(this % alfa_u)
			print *, "alphav> ", minval(this % alfa_v), maxval(this % alfa_v)
			print *, "beta>   ", minval(this % beta), maxval(this % beta)
			print *, "gamma>  ", minval(this % gamma), maxval(this % gamma)
			print *, "delta>  ", minval(this % delta), maxval(this % delta)
			print *, "deltau> ", minval(this % deltau), maxval(this % deltau)
			print *, "deltav> ", minval(this % deltav), maxval(this % deltav)
			print *, "A>      ", minval(this % A), maxval(this % A)
			print *, "Au>     ", minval(this % Au), maxval(this % Au)
			print *, "Av>     ", minval(this % Av), maxval(this % Av)
			print *, "B>      ", minval(this % B), maxval(this % B)
			print *
		end if
		
	end function metpCreate


	function metpEvaluate( &
		this, &		! Current meteo profiles
		cfg, &		! Configuration parameters
		zp, &		! Reference height at which to evaluate
		met &
	) result(iRetCode)
	
		! Routine arguments
		class(MetProfiles), intent(in)		:: this
		type(Config), intent(in)			:: cfg
		real(8), intent(in)					:: zp
		type(MetProfValues), intent(out)	:: met
		integer								:: iRetCode
		
		! Locals
		integer	:: n
		real(8)	:: zpp
		integer	:: izFrom
		integer	:: izTo
		
		! Assume success (will falsify on failure
		iRetCode = 0
		
		! Identify the indices bounding the desired height
		n = size(this % z)
		if(zp <= this % z(1)) then
			izFrom = 1
			izTo   = 1
		elseif(zp >= this % z(n)) then
			izFrom = n
			izTo   = n
		else ! Entry condition: z(1) < zp < z(n)
			izFrom = floor((zp - cfg % z0) / cfg % dz) + 1
			izTo   = ceiling((zp - cfg % z0) / cfg % dz) + 1
			if(izFrom < 1 .or. izFrom > n) then
				print *, 'iZfrom = ', izFrom
				print *, '         ', zp, cfg % z0, cfg % dz
			end if
		end if
		
		! Evaluate linear interpolation coefficients
		zpp = (zp - this % z(izFrom)) / cfg % dz
		
		! Compute linear interpolation
		met % u      = this % u(izFrom)      + zpp * (this % u(izTo)      - this % u(izFrom))
		met % v      = this % v(izFrom)      + zpp * (this % v(izTo)      - this % v(izFrom))
		met % su2    = this % su2(izFrom)    + zpp * (this % su2(izTo)    - this % su2(izFrom))
		met % sv2    = this % sv2(izFrom)    + zpp * (this % sv2(izTo)    - this % sv2(izFrom))
		met % sw2    = this % sw2(izFrom)    + zpp * (this % sw2(izTo)    - this % sw2(izFrom))
		met % dsw2   = this % dsw2(izFrom)   + zpp * (this % dsw2(izTo)   - this % dsw2(izFrom))
		met % eps    = this % eps(izFrom)    + zpp * (this % eps(izTo)    - this % eps(izFrom))
		met % alfa   = this % alfa(izFrom)   + zpp * (this % alfa(izTo)   - this % alfa(izFrom))
		met % beta   = this % beta(izFrom)   + zpp * (this % beta(izTo)   - this % beta(izFrom))
		met % gamma  = this % gamma(izFrom)  + zpp * (this % gamma(izTo)  - this % gamma(izFrom))
		met % delta  = this % delta(izFrom)  + zpp * (this % delta(izTo)  - this % delta(izFrom))
		met % alfa_u = this % alfa_u(izFrom) + zpp * (this % alfa_u(izTo) - this % alfa_u(izFrom))
		met % alfa_v = this % alfa_v(izFrom) + zpp * (this % alfa_v(izTo) - this % alfa_v(izFrom))
		met % deltau = this % deltau(izFrom) + zpp * (this % deltau(izTo) - this % deltau(izFrom))
		met % deltav = this % deltav(izFrom) + zpp * (this % deltav(izTo) - this % deltav(izFrom))
		met % deltat = this % deltat(izFrom) + zpp * (this % deltat(izTo) - this % deltat(izFrom))
		met % Au     = this % Au(izFrom)     + zpp * (this % Au(izTo)     - this % Au(izFrom))
		met % Av     = this % Av(izFrom)     + zpp * (this % Av(izTo)     - this % Av(izFrom))
		met % A      = this % A(izFrom)      + zpp * (this % A(izTo)      - this % A(izFrom))
		met % B      = this % B(izFrom)      + zpp * (this % B(izTo)      - this % B(izFrom))

	end function metpEvaluate

	
	function metpDump(this, iLUN, sProfilePath) result(iRetCode)
	
		! Routine arguments
		class(MetProfiles), intent(in)	:: this
		integer, intent(in)				:: iLUN
		character(len=*), intent(in)	:: sProfilePath
		integer							:: iRetCode
		
		! Locals
		integer				:: iErrCode
		character(len=256)	:: sFileName
		character(len=23)	:: sDateTime
		type(DateTime)		:: tTimeStamp
		integer				:: i
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check something should really be done
		if(sProfilePath == " ") return
			
		! Generate file name
		iErrCode = tTimeStamp % fromEpoch(this % rEpoch)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		sDateTime = tTimeStamp % toISO()
		do i = 1, len_trim(sDateTime)
			if(sDateTime(i:i) == ' ') then
				sDateTime(i:i) = '_'
			end if
		end do
		write(sFileName, "(a,'/',a,'.csv')") trim(sProfilePath), sDateTime
		
		! Write profiles
		open(iLUN, file=sFileName, status='unknown', action='write', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 2
			return
		end if
		write(iLUN, "(a6, 21(',',a11))") &
			'z', 'u', 'v', 'T', 'su2', 'sv2', 'sw2', 'dsw2', &
			'eps', 'alpha', 'beta', 'gamma', 'delta', 'alpha.u', 'alpha.v', &
			'delta.u', 'delta.v', 'delta.t', 'A.u', 'A.v', 'A', 'B'
		do i = 1, size(this % z)
			write(iLUN, "(f6.1, 21(',',f11.6))") &
				this % z(i), &
				this % u(i), this % v(i), &
				this % T(i), &
				this % su2(i), this % sv2(i), this % sw2(i), this % dsw2(i), &
				this % eps(i), &
				this % alfa(i), this % beta(i), this % gamma(i), this % delta(i), &
				this % alfa_u(i), this % alfa_v(i), &
				this % deltau(i), this % deltav(i), this % deltat(i), &
				this % Au(i), this % Av(i), this % A(i), this % B(i)
		end do
		close(iLUN)
		
	end function metpDump
	
end module Configuration
