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
	integer				:: i
	
	type Particle
		real	::	Xp, Yp, Zp	! Position
		real	::	up, vp, wp	! Velocity
		real	::	Qp, Tp		! Mass, temperature
		real	::	sh, sz		! Horizontal, vertical sigmas for Gaussian kernel
	end type Particle
	type(Particle), dimension(:), allocatable	:: Part
	
	type(Config)		:: cfg
	type(MetProfiles)	:: prf
	type(Summary)		:: prfSummary
	type(DateTime)		:: curTime
	
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
		print *, 'alamo:: error: Missing or invalid configuration - Return code = ', iRetCode
		stop
	end if
	
	! Iterate over all time steps
	if(cfg % metDiaFile /= "") then
		open(10, file=cfg % metDiaFile, status='unknown', action='write')
		iRetCode = prfSummary % printHeader(10)
	end if
	do i = 1, cfg % getNumMeteo()
	
		! Gather meteo profiles for current time step
		iRetCode = prf % create(cfg, i)
		if(iRetCode /= 0) then
			print *, 'alamo:: error: Profile not created - Return code = ', iRetCode
			stop
		end if
		
		! Add summary to summary file, if requested
		if(cfg % metDiaFile /= "") then
			iRetCode = prf % summarize(prfSummary)
			if(iRetCode /= 0) then
				print *, 'alamo:: error: Meteo profile summary not built - Return code = ', iRetCode
				stop
			end if
			iRetCode = prfSummary % printLine(10)
		end if
		
		! Inform of progress, if requested
		if(cfg % debug > 0) then
			iRetCode = curTime % fromEpoch(cfg % tMeteo % rvExtEpoch(i))
			print *, "Processed: ", curTime % toISO()
		end if
		
	end do
	if(cfg % metDiaFile /= "") then
		close(10)
	end if
	
end program Alamo
