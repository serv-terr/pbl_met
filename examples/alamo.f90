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
	use Configuration
	
	implicit none
	
	character(len=256)	:: sCfgFile
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
	
	type(Config)		:: cfg
	
	! Get parameters
	if(command_argument_count() /= 1) then
		print *, "alamo - Air LAgrangian particle MOdel"
		print *
		print *, "Usage:"
		print *
		print *, "  ./alamo <Config_File>"
		print *
		print *, "Copyright 2018 by Servizi Territorio srl"
		print *, "This is open-source software"
		print *
		stop
	end if
	call get_command_argument(1, sCfgFile)
	
	! Read configuration
	iRetCode = cfg % read(10, 11, sCfgFile)
	if(iRetCode /= 0) then
		print *, ''
	end if
	
end program Alamo
