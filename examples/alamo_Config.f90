module Configuration
	
	use pbl_met
	
	implicit none
	
	type Config
		! Grid data
		real(8)				:: x0
		real(8)				:: y0
		integer				:: nx
		integer 			:: ny
		real(8)				:: dx
		real(8)				:: dy
		real(8)				:: Zmax	
		real(8)				:: dz
		! Timing
		real(8)				:: Tmed		! Averaging time (s)
		integer				:: Nstep	! Number of substeps in an averaging period
		! Particles emitted per substep
		integer				:: Np
		! Static and dynamic emissions
		character(len=256)	:: Filemis	
		character(len=256)	:: Fileprofemi	
		! Meteo data file
		character(len=256)	:: Filemeteo
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
	contains
		procedure			:: read => cfgRead
	end type Config
	
contains

	function cfgRead(this, iLUN, sFileName) result(iRetCode)
	
		! Routine arguments
		class(Config), intent(out)		:: this
		integer, intent(in)				:: iLUN
		character(len=*), intent(in)	:: sFileName
		integer							:: iRetCode
		
		! Locals
		integer				:: iErrCode
		type(IniFile)		:: cfg
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Get configuration file, and prepare to parse it
		iErrCode = cfg % read(10, sFileName)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		
		! Gather configuration data
		! -1- Timing
		iErrCode = cfg % getReal8("Timing", "avgtime", this % Tmed, 3600.d0)
		if(iErrCode /= 0) then
			iRetCode = 2
			return
		end if
		iErrCode = cfg % getInteger("Timing", "nstep", this % Nstep, 360)
		if(iErrCode /= 0) then
			iRetCode = 3
			return
		end if
		iErrCode = cfg % getInteger("Timing", "npart", this % Nstep, 100)
		if(iErrCode /= 0) then
			iRetCode = 4
			return
		end if
		! -1- Emission
		iErrCode = cfg % getString("Emission", "static", this % Filemis, "")
		if(iErrCode /= 0) then
			iRetCode = 5
			return
		end if
		iErrCode = cfg % getString("Emission", "dynamic", this % Fileprofemi, "")
		if(iErrCode /= 0) then
			iRetCode = 6
			return
		end if
		! -1- Meteo
		iErrCode = cfg % getString("Meteo", "inpfile", this % Filemeteo, "")
		if(iErrCode /= 0) then
			iRetCode = 7
			return
		end if
		iErrCode = cfg % getReal8("Meteo", "height", this % zlev, -9999.9d0)
		if(iErrCode /= 0) then
			iRetCode = 8
			return
		end if
		iErrCode = cfg % getReal8("Meteo", "z0", this % z0, 0.02d0)
		if(iErrCode /= 0) then
			iRetCode = 9
			return
		end if
		iErrCode = cfg % getReal8("Meteo", "zr", this % zr, 10.d0)
		if(iErrCode /= 0) then
			iRetCode = 10
			return
		end if
		! -1- Output
		iErrCode = cfg % getString("Output", "conc", this % Fileout, "")
		if(iErrCode /= 0) then
			iRetCode = 11
			return
		end if
		iErrCode = cfg % getReal8("Output", "x0", this % x0, -9999.9d0)
		if(iErrCode /= 0) then
			iRetCode = 12
			return
		end if
		iErrCode = cfg % getReal8("Output", "y0", this % y0, -9999.9d0)
		if(iErrCode /= 0) then
			iRetCode = 13
			return
		end if
		iErrCode = cfg % getInteger("Output", "nx", this % nx, -9999)
		if(iErrCode /= 0) then
			iRetCode = 14
			return
		end if
		iErrCode = cfg % getInteger("Output", "ny", this % ny, -9999)
		if(iErrCode /= 0) then
			iRetCode = 15
			return
		end if
		iErrCode = cfg % getReal8("Output", "dx", this % dx, -9999.9d0)
		if(iErrCode /= 0) then
			iRetCode = 16
			return
		end if
		iErrCode = cfg % getReal8("Output", "dy", this % dy, -9999.9d0)
		if(iErrCode /= 0) then
			iRetCode = 17
			return
		end if
		iErrCode = cfg % getReal8("Output", "zmax", this % Zmax, -9999.9d0)
		if(iErrCode /= 0) then
			iRetCode = 18
			return
		end if
		iErrCode = cfg % getReal8("Output", "dz", this % dz, -9999.9d0)
		if(iErrCode /= 0) then
			iRetCode = 19
			return
		end if
		! -1- General
		iErrCode = cfg % getInteger("General", "debug_level", this % debug, 0)
		if(iErrCode /= 0) then
			iRetCode = 20
			return
		end if
		iErrCode = cfg % getString("General", "diag_file", this % diag, "")
		if(iErrCode /= 0) then
			iRetCode = 21
			return
		end if
		iErrCode = cfg % getInteger("General", "frame_interval", this % frameInterval, 0)
		if(iErrCode /= 0) then
			iRetCode = 22
			return
		end if
		iErrCode = cfg % getString("General", "frame_path", this % framePath, "")
		if(iErrCode /= 0) then
			iRetCode = 23
			return
		end if
	
	end function cfgRead
	
end module Configuration
