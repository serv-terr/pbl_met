module Configuration
	
	use pbl_met
	use Emission
	use Meteo
	
	implicit none
	
	type Config
		! Status
		logical				:: lIsFull = .false.
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
		real(8)				:: fat
		! General
		integer				:: debug
		character(len=256)	:: diag
		integer				:: frameInterval
		character(len=256)	:: framePath
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
		procedure			:: read        => cfgRead
		procedure			:: getNumMeteo => cfgGetMeteoSize
	end type Config
	
	
	type MetProfiles
		real(8)								:: rEpoch	! Time stamp of current profile set
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
	contains
		procedure	:: clean     => metpClean
		procedure	:: alloc     => metpAlloc
		procedure	:: create    => metpCreate
		procedure	:: evaluate  => metpEvaluate
		procedure	:: summarize => metpSummarize
	end type MetProfiles
	
	
	type Summary
		real(8)	:: rTimeStamp
		real(8)	:: u, uMin, uMax
		real(8)	:: v, vMin, vMax
		real(8)	:: su2, su2Min, su2Max
		real(8)	:: sv2, sv2Min, sv2Max
		real(8)	:: sw2, sw2Min, sw2Max
		real(8)	:: eps, epsMin, epsMax
		real(8)	:: alfa, alfaMin, alfaMax
		real(8)	:: beta, betaMin, betaMax
		real(8)	:: gamma, gammaMin, gammaMax
		real(8)	:: delta, deltaMin, deltaMax
		real(8)	:: alfa_u, alfa_uMin, alfa_uMax
		real(8)	:: alfa_v, alfa_vMin, alfa_vMax
		real(8)	:: deltau, deltauMin, deltauMax
		real(8)	:: deltav, deltavMin, deltavMax
		real(8)	:: deltat, deltatMin, deltatMax
	contains
		procedure	:: printHeader => sumHeader
		procedure	:: printLine   => sumLine
	end type Summary
	
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
		iErrCode = cfg % getString("General", "frame_path", this % framePath, "")
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'frame_path' in [General]"
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
		if(this % Fileout == "") then
			iRetCode = 3
			if(this % debug > 0) print *, "alamo:: error: Invalid value of 'conc' in [Output]"
			return
		end if
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
		! of vertical Lagrangian decorrelation time
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
	
	end function metpCreate


	function metpEvaluate( &
		this, &		! Current meteo profiles
		cfg, &		! Configuration parameters
		zp, &		! Reference height at which to evaluate
		u, &
		v, &
		su2, &
		sv2, &
		sw2, &
		eps, &
		alfa, &
		beta, &
		gamma, &
		delta, &
		alfa_u, &
		alfa_v, &
		deltau, &
		deltav, &
		deltat &
	) result(iRetCode)
	
		! Routine arguments
		class(MetProfiles), intent(out)		:: this
		type(Config), intent(in)			:: cfg
		real(8), intent(in)					:: zp
		real(8), intent(out)				:: u
		real(8), intent(out)				:: v
		real(8), intent(out)				:: su2
		real(8), intent(out)				:: sv2
		real(8), intent(out)				:: sw2
		real(8), intent(out)				:: eps
		real(8), intent(out)				:: alfa
		real(8), intent(out)				:: beta
		real(8), intent(out)				:: gamma
		real(8), intent(out)				:: delta
		real(8), intent(out)				:: alfa_u
		real(8), intent(out)				:: alfa_v
		real(8), intent(out)				:: deltau
		real(8), intent(out)				:: deltav
		real(8), intent(out)				:: deltat
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
			izFrom = floor((zp - cfg % z0) / cfg % dz)
			izTo   = ceiling((zp - cfg % z0) / cfg % dz)
		end if
		
		! Evaluate linear interpolation coefficients
		zpp = (zp - this % z(izFrom)) / cfg % dz
		
		! Compute linear interpolation
		u   = this % u(izFrom) + zpp * (this % u(izTo) - this % u(izFrom))
		v   = this % v(izFrom) + zpp * (this % v(izTo) - this % v(izFrom))
		su2 = this % su2(izFrom) + zpp * (this % su2(izTo) - this % su2(izFrom))
		sv2 = this % sv2(izFrom) + zpp * (this % sv2(izTo) - this % sv2(izFrom))
		sw2 = this % sw2(izFrom) + zpp * (this % sw2(izTo) - this % sw2(izFrom))
		eps = this % eps(izFrom) + zpp * (this % eps(izTo) - this % eps(izFrom))
		alfa = this % alfa(izFrom) + zpp * (this % alfa(izTo) - this % alfa(izFrom))
		beta = this % beta(izFrom) + zpp * (this % beta(izTo) - this % beta(izFrom))
		gamma = this % gamma(izFrom) + zpp * (this % gamma(izTo) - this % gamma(izFrom))
		delta = this % delta(izFrom) + zpp * (this % delta(izTo) - this % delta(izFrom))
		alfa_u = this % alfa_u(izFrom) + zpp * (this % alfa_u(izTo) - this % alfa_u(izFrom))
		alfa_v = this % alfa_v(izFrom) + zpp * (this % alfa_v(izTo) - this % alfa_v(izFrom))
		deltau = this % deltau(izFrom) + zpp * (this % deltau(izTo) - this % deltau(izFrom))
		deltav = this % deltav(izFrom) + zpp * (this % deltav(izTo) - this % deltav(izFrom))
		deltat = this % deltat(izFrom) + zpp * (this % deltat(izTo) - this % deltat(izFrom))

	end function metpEvaluate


	function metpSummarize(this, report) result(iRetCode)
	
		! Routine arguments
		class(MetProfiles), intent(in)	:: this
		type(Summary), intent(out)		:: report
		integer							:: iRetCode
		
		! Locals
		integer	:: n
		
		! Assume success (will falsify on failure)
		iRetCode = 0
	
		! Compute report values
		n = size(this % z)
		report % rTimeStamp = this % rEpoch
		report % u          = sum(this % u) / n
		report % uMin       = minval(this % u)
		report % uMax       = maxval(this % u)
		report % v          = sum(this % v) / n
		report % vMin       = minval(this % v)
		report % vMax       = maxval(this % v)
		report % su2        = sum(this % su2) / n
		report % su2Min     = minval(this % su2)
		report % su2Max     = maxval(this % su2)
		report % sv2        = sum(this % sv2) / n
		report % sv2Min     = minval(this % sv2)
		report % sv2Max     = maxval(this % sv2)
		report % sw2        = sum(this % sw2) / n
		report % sw2Min     = minval(this % sw2)
		report % sw2Max     = maxval(this % sw2)
		report % eps        = sum(this % eps) / n
		report % epsMin     = minval(this % eps)
		report % epsMax     = maxval(this % eps)
		report % alfa       = sum(this % alfa) / n
		report % alfaMin    = minval(this % alfa)
		report % alfaMax    = maxval(this % alfa)
		report % beta       = sum(this % beta) / n
		report % betaMin    = minval(this % beta)
		report % betaMax    = maxval(this % beta)
		report % gamma      = sum(this % gamma) / n
		report % gammaMin   = minval(this % gamma)
		report % gammaMax   = maxval(this % gamma)
		report % delta      = sum(this % delta) / n
		report % deltaMin   = minval(this % delta)
		report % deltaMax   = maxval(this % delta)
		report % alfa_u     = sum(this % alfa_u) / n
		report % alfa_uMin  = minval(this % alfa_u)
		report % alfa_vMax  = maxval(this % alfa_u)
		report % alfa_v     = sum(this % alfa_v) / n
		report % alfa_vMin  = minval(this % alfa_v)
		report % alfa_vMax  = maxval(this % alfa_v)
		report % deltau     = sum(this % deltau) / n
		report % deltauMin  = minval(this % deltau)
		report % deltauMax  = maxval(this % deltau)
		report % deltav     = sum(this % deltav) / n
		report % deltavMin  = minval(this % deltav)
		report % deltavMax  = maxval(this % deltav)
		report % deltat     = sum(this % deltat) / n
		report % deltatMin  = minval(this % deltat)
		report % deltatMax  = maxval(this % deltat)
		
	end function metpSummarize
	
	
	function sumHeader(this, iLUN) result(iRetCode)
	
		! Routine arguments
		class(Summary), intent(in)	:: this
		integer, intent(in)			:: iLUN
		integer						:: iRetCode
		
		! Locals
		integer	:: iErrCode
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Attempt printing header
		write(iLUN, "(a,45(',',a))", iostat=iErrCode) &
			'date', &
			'u.Min', 'u.Mean', 'u.Max', &
			'v.Min', 'v.Mean', 'v.Max', &
			'uu.Min', 'uu.Mean', 'uu.Max', &
			'vv.Min', 'vv.Mean', 'vv.Max', &
			'ww.Min', 'ww.Mean', 'ww.Max', &
			'eps.Min', 'eps.Mean', 'eps.Max', &
			'alpha.Min', 'alpha.Mean', 'alpha.Max', &
			'beta.Min', 'beta.Mean', 'beta.Max', &
			'gamma.Min', 'gamma.Mean', 'gamma.Max', &
			'delta.Min', 'delta.Mean', 'delta.Max', &
			'alpha.u.Min', 'alpha.u.Mean', 'alpha.u.Max', &
			'alpha.v.Min', 'alpha.v.Mean', 'alpha.v.Max', &
			'delta.u.Min', 'delta.u.Mean', 'delta.u.Max', &
			'delta.v.Min', 'delta.v.Mean', 'delta.v.Max', &
			'delta.t.Min', 'delta.t.Mean', 'delta.t.Max'
		if(iErrCode /= 0) then
			iRetCode = 1
		end if
		
	end function sumHeader
	
	
	function sumLine(this, iLUN) result(iRetCode)
	
		! Routine arguments
		class(Summary), intent(in)	:: this
		integer, intent(in)			:: iLUN
		integer						:: iRetCode
		
		! Locals
		integer				:: iErrCode
		type(DateTime)		:: tStamp
		character(len=23)	:: sDateTime
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Attempt printing current data line
		iErrCode = tStamp % fromEpoch(this % rTimeStamp)
		sDateTime = tStamp % toISO()
		write(iLUN, "(a, 45(',', e15.7))") &
			sDateTime, &
			this % uMin, &
			this % u, &
			this % uMax, &
			this % vMin, &
			this % v, &
			this % vMax, &
			this % su2Min, &
			this % su2, &
			this % su2Max, &
			this % sv2Min, &
			this % sv2, &
			this % sv2Max, &
			this % sw2Min, &
			this % sw2, &
			this % sw2Max, &
			this % epsMin, &
			this % eps, &
			this % epsMax, &
			this % alfaMin, &
			this % alfa, &
			this % alfaMax, &
			this % betaMin, &
			this % beta, &
			this % betaMax, &
			this % gammaMin, &
			this % gamma, &
			this % gammaMax, &
			this % deltaMin, &
			this % delta, &
			this % deltaMax, &
			this % alfa_uMin, &
			this % alfa_u, &
			this % alfa_uMax, &
			this % alfa_vMin, &
			this % alfa_v, &
			this % alfa_vMax, &
			this % deltauMin, &
			this % deltau, &
			this % deltauMax, &
			this % deltavMin, &
			this % deltav, &
			this % deltavMax, &
			this % deltatMin, &
			this % deltat, &
			this % deltatMax
		
		end function sumLine
		
	end module Configuration
