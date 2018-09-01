! Program mfc2cvt - Conversion of MeteoFlux Core uncompressed data files to SonicLib form.
!
! This module is part of the pbl_met library.
!
! Copyright 2018 by Servizi Territorio srl
!
! This is open-source code, covered by lGPL 3.0 license.
!
! Author: Mauri Favaron
!
program mfc2cvt

	use pbl_met
	use usa1
	
	implicit none
	
	! Locals
	integer									:: iRetCode
	integer									:: iParam
	character(len=256)						:: sFileName
	character(len=256)						:: sOutputFile
	character(len=32)						:: sBuffer
	integer									:: iIdxQ
	real									:: rMultiplierQ
	real									:: rOffsetQ
	integer									:: iIdxC
	real									:: rMultiplierC
	real									:: rOffsetC
	integer									:: iDelay
	type(SonicData)							:: tSd
	
	! Get parameters
	iParam = command_argument_count()
	if(iParam /= 2 .and. iParam /= 6 .and. iParam /= 9) then
		print *, "mfc2cvt - Convert an uncompressed MeteoFlux Core V2 raw sonic file to SonicLib form"
		print *
		print *, "Usage:"
		print *
		print *, &
		"  ./mfc2cvt <MfcFile> [<H2O_Index> <H2O_Mult> <H2O_Off> [<CO2_Index> <CO2_Mult> <CO2_Off>] <LagsDelay>] <OutputFile>"
		print *
		print *
		print *, "Program 'mfc2cvt' is part of the pbl_met project, and is"
		print *, "released under an lGPL-3.0 open source license."
		print *
		stop
	end if
	if(iParam == 2) then
		call get_command_argument(1, sFileName)
		call get_command_argument(2, sOutputFile)
		iIdxQ = 0
		iIdxC = 0
	elseif(iParam == 6) then
		call get_command_argument(1, sFileName)
		call get_command_argument(2, sBuffer)
		read(sBuffer, *, iostat=iRetCode) iIdxQ
		if(iRetCode /= 0) then
			print *, "mfc2cvt:: error: Invalid index for H2O"
			stop
		end if
		if(iIdxQ <= 4 .or. iIdxQ > 14) then
			print *, "mfc2cvt:: error: Invalid index value for H2O (expected: 5 to 14)"
			stop
		end if
		call get_command_argument(3, sBuffer)
		read(sBuffer, *, iostat=iRetCode) rMultiplierQ
		if(iRetCode /= 0) then
			print *, "mfc2cvt:: error: Invalid multiplier for H2O"
			stop
		end if
		if(rMultiplierQ <= 0.) then
			print *, "mfc2cvt:: error: Invalid multiplier value for H2O (expected positive)"
			stop
		end if
		call get_command_argument(4, sBuffer)
		read(sBuffer, *, iostat=iRetCode) rOffsetQ
		if(iRetCode /= 0) then
			print *, "mfc2cvt:: error: Invalid offset for H2O"
			stop
		end if
		call get_command_argument(5, sBuffer)
		read(sBuffer, *, iostat=iRetCode) iDelay
		if(iRetCode /= 0) then
			print *, "mfc2cvt:: error: Invalid delay"
			stop
		end if
		if(iDelay < 0 .or. iDelay > 400) then
			print *, "mfc2cvt:: error: Invalid index value for H2O (expected: 0 to 400)"
			stop
		end if
		call get_command_argument(6, sOutputFile)
		iIdxC = 0
	elseif(iParam == 8) then
		call get_command_argument(1, sFileName)
		call get_command_argument(2, sBuffer)
		read(sBuffer, *, iostat=iRetCode) iIdxQ
		if(iRetCode /= 0) then
			print *, "mfc2cvt:: error: Invalid index for H2O"
			stop
		end if
		if(iIdxQ <= 4 .or. iIdxQ > 14) then
			print *, "mfc2cvt:: error: Invalid index value for H2O (expected: 5 to 14)"
			stop
		end if
		call get_command_argument(3, sBuffer)
		read(sBuffer, *, iostat=iRetCode) rMultiplierQ
		if(iRetCode /= 0) then
			print *, "mfc2cvt:: error: Invalid multiplier for H2O"
			stop
		end if
		if(rMultiplierQ <= 0.) then
			print *, "mfc2cvt:: error: Invalid multiplier value for H2O (expected positive)"
			stop
		end if
		call get_command_argument(4, sBuffer)
		read(sBuffer, *, iostat=iRetCode) rOffsetQ
		if(iRetCode /= 0) then
			print *, "mfc2cvt:: error: Invalid offset for H2O"
			stop
		end if
		call get_command_argument(5, sBuffer)
		read(sBuffer, *, iostat=iRetCode) iIdxC
		if(iRetCode /= 0) then
			print *, "mfc2cvt:: error: Invalid index for CO2"
			stop
		end if
		if(iIdxC <= 4 .or. iIdxC > 14 .or. iIdxC == iIdxQ) then
			print *, "mfc2cvt:: error: Invalid index value for H2O (expected: 5 to 14)"
			stop
		end if
		call get_command_argument(6, sBuffer)
		read(sBuffer, *, iostat=iRetCode) rMultiplierC
		if(iRetCode /= 0) then
			print *, "mfc2cvt:: error: Invalid multiplier for CO2"
			stop
		end if
		if(rMultiplierC <= 0.) then
			print *, "mfc2cvt:: error: Invalid multiplier value for CO2 (expected positive)"
			stop
		end if
		call get_command_argument(7, sBuffer)
		read(sBuffer, *, iostat=iRetCode) rOffsetC
		if(iRetCode /= 0) then
			print *, "mfc2cvt:: error: Invalid offset for CO2"
			stop
		end if
		call get_command_argument(8, sBuffer)
		read(sBuffer, *, iostat=iRetCode) iDelay
		if(iRetCode /= 0) then
			print *, "mfc2cvt:: error: Invalid delay"
			stop
		end if
		if(iDelay < 0 .or. iDelay > 400) then
			print *, "mfc2cvt:: error: Invalid index value for H2O (expected: 0 to 400)"
			stop
		end if
		call get_command_argument(9, sOutputFile)
	else
		print *, "mfc2cvt:: error: Unknown parameter error"
		stop
	end if
	
	! Get data from MFC2 file
	if(iParam == 2) then
		iRetCode = tSd % readMeteoFlux( &
			10, &
			sFileName &
		)
	elseif(iParam == 6) then
		iRetCode = tSd % readMeteoFlux( &
			10, &
			sFileName, &
			iIndexQ = iIdxQ, &
			rMultiplierQ = rMultiplierQ, &
			rOffsetQ = rOffsetQ, &
			iDelayLag = iDelay &
		)
	elseif(iParam == 9) then
		iRetCode = tSd % readMeteoFlux( &
			10, &
			sFileName, &
			iIndexQ = iIdxQ, &
			rMultiplierQ = rMultiplierQ, &
			rOffsetQ = rOffsetQ, &
			iIndexC = iIdxC, &
			rMultiplierC = rMultiplierC, &
			rOffsetC = rOffsetC, &
			iDelayLag = iDelay &
		)
	end if
	if(iRetCode /= 0) then
		print *, "mfc2cvt:: error: Input file not read - Return code = ", iRetCode
		stop
	end if
	
	! Write data just read to output file
	iRetCode = tSd % writeSonicLib(10, sOutputFile)
	if(iRetCode /= 0) then
		print *, "mfc2cvt:: error: Output file not written - Return code = ", iRetCode
		stop
	end if
	
end program mfc2cvt
