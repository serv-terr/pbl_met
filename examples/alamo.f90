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
	use Particles
	
	implicit none
	
	character(len=256)	:: sCfgFile
	integer				:: iRetCode
	integer				:: iStep
	integer				:: iSubStep
	integer				:: i
	
	type(Config)		:: cfg
	type(MetProfiles)	:: prf
	type(Summary)		:: prfSummary
	type(DateTime)		:: curTime
	type(ParticlePool)	:: part
	
	real				:: timeFrom1
	real				:: timeTo1
	real				:: timeFrom2
	real				:: timeTo2
	real				:: timeSpentOnStep
	real				:: timeSpentOnMeteo
	real				:: timeSpentOnParticleEmission
	real				:: timeSpentOnParticleMovement
	real				:: timeSpentOnConcentrations
	real				:: timeSpentOnWriting
	
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
	
	! Initialize particle pool
	iRetCode = part % Initialize(cfg)
	if(iRetCode /= 0) then
		print *, 'alamo:: error: Error initializing particle pool - Return code = ', iRetCode
		stop
	end if
	
	! Main loop: get meteo information, and care for particles
	if(cfg % metDiaFile /= "") then
		open(100, file=cfg % metDiaFile, status='unknown', action='write')
		write(100, "(a)") &
			"Date.Time,               Num.Part,  C.Med,          C.Max,          " // &
			"Time.Step,      Time.Meteo,     Time.Emission   Time.Movement,  " // &
			"Time.Drift,     Time.Diffusion, Time.Expansion, " // &
			"Time.Concentr,  Time.Writing"
	end if
	i = 0	! Actual time index
	iRetCode = part % SnapInit(10)
	if(iRetCode /= 0) then
		print *, "alamo:: error: Impossible to generate snapshot data files - Return code = ", iRetCode
		stop
	end if
	open(11, file=cfg % Fileout, access='stream', action='write', status='unknown')
	do iStep = 1, cfg % getNumTimeSteps()
	
		call cpu_time(timeFrom1)
	
		timeSpentOnConcentrations = 0.
		timeSpentOnWriting        = 0.
		
		! Reset concentration matrix
		iRetCode = part % ResetConc()
		call cpu_time(timeFrom2)
		if(iRetCode /= 0) then
			print *, "alamo:: error: Concentration matrix was not allocated before use"
			stop
		end if
		call cpu_time(timeTo2)
		timeSpentOnConcentrations = timeSpentOnConcentrations + (timeTo2 - timeFrom2)
	
		! Iterate over substeps in current time step
		timeSpentOnMeteo            = 0.
		timeSpentOnParticleEmission = 0.
		timeSpentOnParticleMovement = 0.
		do iSubStep = 1, cfg % getNumTimeSubSteps()
			i = i + 1
			
			! Gather meteo profiles for current time step
			call cpu_time(timeFrom2)
			iRetCode = prf % create(cfg, i)
			if(iRetCode /= 0) then
				print *, 'alamo:: error: Profile not created - Return code = ', iRetCode
				stop
			end if
			call cpu_time(timeTo2)
			timeSpentOnMeteo = timeSpentOnMeteo + (timeTo2 - timeFrom2)
			
			! Add summary to summary file, if requested
			if(cfg % metDiaFile /= "") then
				iRetCode = prf % summarize(prfSummary)
				if(iRetCode /= 0) then
					print *, 'alamo:: error: Meteo profile summary not built - Return code = ', iRetCode
					stop
				end if
				iRetCode = prfSummary % printLine(10)
			end if
			
			! Emit new particles
			call cpu_time(timeFrom2)
			iRetCode = part % Emit(cfg, prf)
			if(iRetCode /= 0) then
				print *, 'alamo:: error: Particle emission failed - Return code = ', iRetCode
				stop
			end if
			call cpu_time(timeTo2)
			timeSpentOnParticleEmission = timeSpentOnParticleEmission + (timeTo2 - timeFrom2)
			
			! Move particles
			call cpu_time(timeFrom2)
			iRetCode = part % Move(cfg, prf, i)
			if(iRetCode /= 0) then
				print *, 'alamo:: error: Particle move failed - Return code = ', iRetCode
				stop
			end if
			call cpu_time(timeTo2)
			timeSpentOnParticleMovement = timeSpentOnParticleMovement + (timeTo2 - timeFrom2)
			
			! Add particles contributions to concentration
			call cpu_time(timeFrom2)
			iRetCode = part % UpdateConc(cfg, i)
			if(iRetCode /= 0) then
				print *, 'alamo:: error: Concentration update failed - Return code = ', iRetCode
				stop
			end if
			call cpu_time(timeTo2)
			timeSpentOnConcentrations = timeSpentOnConcentrations + (timeTo2 - timeFrom2)
			
			call cpu_time(timeFrom2)
			iRetCode = part % SnapTake(10, i)
			call cpu_time(timeTo2)
			timeSpentOnWriting = timeSpentOnWriting + (timeTo2 - timeFrom2)
			
		end do
		
		! Compute mean concentration
		part % C = part % C / cfg % getNumTimeSubSteps()
		
		! Write concentration to file (in 01 form)
		iRetCode = curTime % fromEpoch(cfg % tMeteo % rvEpoch(iStep))
		write(11) &
			curTime % iYear, curTime % iMonth, curTime % iDay, &
			curTime % iHour*100 + curTime % iMinute
		write(11) part % C
		
		! Inform of progress, if requested
		if(cfg % debug > 0) then
			print "(a,1x,i8,2(1x,e15.7),1x,f4.1,1x,f6.1)", &
				curTime % toISO(), &
				part % count(), &
				sum(part % C) / (part % nx * part % ny), &
				maxval(part % C), &
				cfg % tMeteo % rvVel(iStep), &
				cfg % tMeteo % rvZi(iStep)
		end if
		
		call cpu_time(timeTo1)
		timeSpentOnStep = timeTo1 - timeFrom1
	
		if(cfg % metDiaFile /= "") then
			write(100, "(a,',',i10,11(',',e15.7))") &
				curTime % toISO(), &
				part % count(), &
				real(sum(part % C) / (part % nx * part % ny), kind=4), &
				real(maxval(part % C), kind=4), &
				timeSpentOnStep, &
				timeSpentOnMeteo, &
				timeSpentOnParticleEmission, &
				timeSpentOnParticleMovement, &
				part % rTimeDrift, &
				part % rTimeDiffusion, &
				part % rTimeExpansion, &
				timeSpentOnConcentrations, &
				timeSpentOnWriting
			flush(100)
		end if
	
	end do
	close(11)
	if(cfg % metDiaFile /= "") then
		close(100)
	end if
	
end program Alamo
