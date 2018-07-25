! Program sonicpro - Minimalistic eddy covariance processing, illustrating use of Usa1 module and
! pbl_met, for use on data produced by Servizi Territorio srl's "WindRecorder" loggers.
!
! This module is part of the pbl_met library.
!
! Copyright 2018 by Servizi Territorio srl
!
! This is open-source code, covered by lGPL 3.0 license.
!
! Author: Mauri Favaron
!
program sonicpro

	use pbl_met
	use usa1
	
	implicit none
	
	! Locals
	character(len=256)	:: sDataPath
	character(len=256)	:: sFileName
	character(len=256)	:: sOutputFile
	character(len=19)	:: sFirstDateTime
	character(len=19)	:: sLastDateTime
	integer				:: iRetCode
	integer				:: iNumHours
	type(DateTime)		:: tFrom
	type(DateTime)		:: tTo
	type(Usa1DataDir)	:: tDir
	real(8)				:: rFrom
	real(8)				:: rTo
	real(8)				:: rHold
	integer				:: iMode
	
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
	
	! Convert dates and times to DateTime values
	read(sFirstDateTime, "(i4.4,3(1x,i2.2))", iostat=iRetCode) &
		tFrom % iYear, &
		tFrom % iMonth, &
		tFrom % iDay, &
		tFrom % iHour
	if(iRetCode /= 0) then
		print *, "Invalid <DateTimeFrom>"
		stop
	end if
	tFrom % iMinute = 0
	tFrom % rSecond = 0.d0
	rFrom = tFrom % toEpoch()
	read(sLastDateTime, "(i4.4,3(1x,i2.2))", iostat=iRetCode) &
		tTo % iYear, &
		tTo % iMonth, &
		tTo % iDay, &
		tTo % iHour
	if(iRetCode /= 0) then
		print *, "Invalid <DateTimeTo>"
		stop
	end if
	tTo % iMinute = 0
	tTo % rSecond = 0.d0
	rTo = tTo % toEpoch()
	if(rTo < rFrom) then
		rHold = rFrom
		rFrom = rTo
		rTo   = rHold
	end if
	
	! Compute number of hours in set
	iNumHours = nint((rTo - rFrom)/3600.d0) + 1
	
	! Map data files
	iRetCode = tDir % mapFiles(sDataPath, rFrom, iNumHours, .true.)
	if(iRetCode /= 0) then
		print *, "Error searching for WindRecorder/USA1 data files"
		stop
	end if
	
	! Iterate over data files
	iMode = DE_FIRST
	iRetCode = tDir % getFile(iMode, sFileName)
	if(iRetCode /= 0) then
		print *, "Error accessing file list - Return code = ", iRetCode
		stop
	end if
	do while(iMode /= DE_ERR)
		print *, trim(sFileName)
		iRetCode = tDir % getFile(iMode, sFileName)
		if(iRetCode /= 0) then
			print *, "Error accessing file list - Return code = ", iRetCode
			stop
		end if
	end do

end program sonicpro
