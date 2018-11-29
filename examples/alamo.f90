!							  Program ALAMO v2
!			          (Air LAgrangian dispersion MOdel)
!
!	One-dimensional Lagrangian particle model for flat terrain, and a chemically inert tracer.
!
!						       prof. R. Sozzi - 2006
!
! 2018: Nitty-gritty program engineering, and integration to the new pbl_met, by Mauri Favaron.
! The first version of "alamo" was already in the open source, but on those very far time
! we did not really know how to make it really visible. It is my (Mauri's) opinion "alamo"
! value is much far beyond an "example" of pbl_met use: it's a viable, interesting and
! useable conventional particle model, very useful when use of directly measured data
! is possible. Some engineering is still in demand to make "alamo" competitive - but, really,
! nothing impossible.
!
program Alamo
	
	use pbl_met
	
	implicit none
	
	character(len=256)	:: sCfgFile
	type(IniFile)		:: cfg
	integer				:: iRetCode
	
	type Particle
		real	::	Xp, Yp, Zp	! Position
		real	::	up, vp, wp	! Velocity
		real	::	Qp, Tp		! Mass, temperature
		real	::	sh, sz		! Horizontal, vertical sigmas for Gaussian kernel
	end type Particle
	type(Particle), dimension(:), allocatable	:: Part
	
	type Environment
		real, dimension(:), allocatable	:: z							! Heights (m above ground)
		real, dimension(:), allocatable	:: u, v							! Wind velocity (w assumed zero in the moment) (m/s)
		real, dimension(:), allocatable	:: T							! Air temperature (°C)
		real, dimension(:), allocatable	:: su2, sv2, sw2				! Wind velocity 
		real, dimension(:), allocatable	:: dsw2
		real, dimension(:), allocatable	:: eps
		real, dimension(:), allocatable	:: alfa, beta, gamma, delta
		real, dimension(:), allocatable	:: alfa_u, alfa_v
		real, dimension(:), allocatable	:: deltau, deltav
		real, dimension(:), allocatable	:: deltat
	end type Environment
	type(Environment)	:: air
	
	type Configuration
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
	end type Configuration
	type(Configuration)	:: config
	
	! Get parameters
	if(command_argument_count() /= 1) then
		print *, "alamo - Air LAgrangian particle MOdel"
		print *
		print *, "Usage:"
		print *
		print *, "  ./alamo <Config_File>"
		print *
		print *, "Copyright 2018 by Roberto Sozzi"
		print *, "This is open-source software"
		print *
		stop
	end if
	call get_command_argument(1, sCfgFile)
	
	! Read configuration file
	iRetCode = cfg % read(10, sCfgFile)
	if(iRetCode /= 0) then
		print *, "alamo:: error: Missing or invalid configuration file"
		stop
	end if
	
end program Alamo
