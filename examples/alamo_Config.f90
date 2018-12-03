module Configuration
	
	use pbl_met
	use Emission
	use Meteo
	
	implicit none
	
	type Config
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
		! Particles emitted per substep
		integer				:: Np
		! Static and dynamic emissions
		character(len=256)	:: Filemis	
		character(len=256)	:: Fileprofemi	
		! Meteo data files
		character(len=256)	:: Filemeteo
		character(len=256)	:: FilemeteoOut	! May be an empty string
		! Site parameters of meteorological file
		real(8)				:: zlev
		real(8)				:: z0
		real(8)				:: zr
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
		! Emissions space
		type(PointSource), dimension(:), allocatable	:: tvPointStatic
		! Meteo data
		type(MetData)									:: tMeteo
	contains
		procedure			:: read => cfgRead
	end type Config
	
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
		iErrCode = cfg % getString("General", "diag_file", this % diag, "")
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
		iErrCode = cfg % getReal8("Meteo", "height", this % zlev, -9999.9d0)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'height' in [Emission]"
			return
		end if
		iErrCode = cfg % getReal8("Meteo", "z0", this % z0, 0.02d0)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'z0' in [Emission]"
			return
		end if
		iErrCode = cfg % getReal8("Meteo", "zr", this % zr, 10.d0)
		if(iErrCode /= 0) then
			iRetCode = 2
			if(this % debug > 0) print *, "alamo:: error: Invalid 'zr' in [Emission]"
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
		if(this % nz <= 0) then
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
		! -1- Static emissions
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
			if(allocated(this % tvPointStatic)) deallocate(this % tvPointStatic)
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
		end if
		! -1- Meteorological data
		iErrCode = this % tMeteo % read(iLUN1, this % Filemeteo, this % Tmed, this % Nstep, this % FilemeteoOut)
		if(iErrCode /= 0) then
			iRetCode = 3
			return
		end if
	
	end function cfgRead
	
end module Configuration
