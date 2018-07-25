! Program sonicpro - Minimalistic eddy covariance processing, illustrating use of Usa1 module and
! pbl_met, for use on data produced by Servizi Territorio srl's "WindRecorder" loggers.
!
! This module is part of the pbl_met library.
!
! Copyright 2018 by Servizi Territorio srl
!
! This is open-source code, covered by lGPL 3.0 license
!
program sonicpro

	use pbl_met
	use usa1
	
	implicit none
	
	! Locals
	character(len=256)	:: sDataPath
	character(len=256)	:: sOutputFile
	character(len=19)	:: sFirstDateTime
	character(len=19)	:: sLastDateTime
	integer				:: iRetCode
	integer				:: iNumHours
	type(DateTime)		:: tFrom
	type(DateTime)		:: tTo
	real(8)				:: rFrom
	real(8)				:: rTo
	
	! Get parameters
	if(command_argument_count() /= 4) then
		print *, "sonicpro - Minimalistic eddy covariance calculator"
		print *
		print *, "Usage:"
		print *
		print *, "  ./sonicpro <DataPath> <DateTimeFrom> <DateTimeTo> <OutputFile>"
		print *
		print *, "Program 'sonicpro' is part of the pbl_met project, and is"
		print *, "released under an lGPL-3.0 open source license."
		print *
		stop
	end if
	call get_command_argument(1, sDataPath)
	call get_command_argument(2, sFirstDateTime)
	call get_command_argument(3, sLastDateTime)
	call get_command_argument(4, sOutputFile)

end program sonicpro
