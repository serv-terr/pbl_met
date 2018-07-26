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
	character(len=256)					:: sDataPath
	character(len=256)					:: sFileName
	character(len=256)					:: sOutputFile
	character(len=19)					:: sFirstDateTime
	character(len=19)					:: sLastDateTime
	character(len=16)					:: sBuffer
	integer								:: iRetCode
	integer								:: iNumHours
	type(DateTime)						:: tFrom
	type(DateTime)						:: tTo
	type(DateTime)						:: tCurTime
	type(Usa1DataDir)					:: tDir
	type(SonicData)						:: tSonic
	type(EddyCovData)					:: tEc
	real(8)								:: rFrom
	real(8)								:: rTo
	real(8)								:: rHold
	integer								:: iMode
	integer								:: i
	integer								:: iAvgTime
	character(len=23)					:: sDateTime
	integer, dimension(:), allocatable	:: ivNumData
	real(8), dimension(:), allocatable	:: rvTimeStamp
	real, dimension(:), allocatable		:: rvTheta
	real, dimension(:), allocatable		:: rvPhi
	real, dimension(:), allocatable		:: rvPsi
	real, dimension(:), allocatable		:: rvT
	real, dimension(:), allocatable		:: rvVarT
	real, dimension(:,:), allocatable	:: rmNrotVel
	real, dimension(:,:), allocatable	:: rmVel
	real, dimension(:,:,:), allocatable	:: raCovVel
	real, dimension(:,:,:), allocatable	:: raNrotCovVel
	real, dimension(:,:), allocatable	:: rmCovT
	real, dimension(:,:), allocatable	:: rmNrotCovT
	real, dimension(3)					:: cartesian
	real, dimension(3)					:: polar
	
	! Get parameters
	if(command_argument_count() /= 5) then
		print *, "sonicpro - Minimalistic eddy covariance calculator"
		print *
		print *, "Usage:"
		print *
		print *, "  ./sonicpro <DataPath> <DateTimeFrom> <DateTimeTo> <AveragingTime> <OutputFile>"
		print *
		print *
		print *, "Program 'sonicpro' is part of the pbl_met project, and is"
		print *, "released under an lGPL-3.0 open source license."
		print *
		stop
	end if
	call get_command_argument(1, sDataPath)
	call get_command_argument(2, sFirstDateTime)
	call get_command_argument(3, sLastDateTime)
	call get_command_argument(4, sBuffer)
	read(sBuffer, *, iostat=iRetCode) iAvgTime
	if(iRetCode /= 0) then
		print *, "Invalid averaging time"
		stop
	end if
	if(mod(3600, iAvgTime) /= 0) then
		print *, "Averaging time is not a divisor of 3600"
		stop
	end if
	call get_command_argument(5, sOutputFile)
	
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
	
	! Iterate over data files, and process them in sequence
	iMode = DE_FIRST
	iRetCode = tDir % getFile(iMode, sFileName)
	if(iRetCode /= 0) then
		print *, "Error accessing file list - Return code = ", iRetCode
		stop
	end if
	open(10, file=sOutputFile, status='unknown', iostat = iRetCode)
	if(iRetCode /= 0) then
		print *, "Error accessing output file in write mode"
		stop
	end if
	write(10,"('date, dir, vel, temp, theta, phi, w.nrot, uu, uv, uw, vv, vw, ww, ut, vt, wt')")
	do while(iMode /= DE_ERR)
	
		! Process file
		print *, 'Processing ', trim(sFileName)
		
		! Get next file name, if exists; the value of iMode parameter is changed automatically,
		! so there is no need to set it directly
		iRetCode = tDir % getFile(iMode, sFileName)
		if(iRetCode /= 0) then
			print *, "Error accessing file list - Return code = ", iRetCode
			cycle
		end if
		
		! Read data to hourly SonicData object
		iRetCode = tSonic % readWindRecorder(11, sFileName, OS_UNIX, SONIC_USA1)
		if(iRetCode /= 0) then
			print *, "Error reading file - Return code = ", iRetCode
			cycle
		end if
		
		! Compute averages
		iRetCode = tSonic % averages(iAvgTime, tEc)
		if(iRetCode /= 0) then
			print *, "Error performing averaging calculations - Return code = ", iRetCode
			cycle
		end if
		
		! Compute eddy-covariance statistics
		iRetCode = tEc % process(2)
		if(iRetCode /= 0) then
			print *, "Error performing eddy covariance calculations - Return code = ", iRetCode
			cycle
		end if
		
		! Retrieve time stamp
		iRetCode = tEc % getTimeStamp(rvTimeStamp)
		if(iRetCode /= 0) then
			print *, "Time stamp retrieval failed - Return code = ", iRetCode
			cycle
		end if
		
		! Retrieve inputs
		iRetCode = tEc % getInputData(ivNumData, rmNrotVel, rvT, raNrotCovVel, rmNrotCovT, rvVarT)
		if(iRetCode /= 0) then
			print *, "Eddy cov inputs retrieval failed - Return code = ", iRetCode
			cycle
		end if
		
		! Retrieve eddy covariance precursors
		iRetCode = tEc % getOutputData(rvTheta, rvPhi, rvPsi, rmVel, raCovVel, rmCovT)
		if(iRetCode /= 0) then
			print *, "Eddy cov precursors retrieval failed - Return code = ", iRetCode
			cycle
		end if
		
		! Write data
		do i = 1, size(rvTimeStamp)
		
			iRetCode  = tCurTime % fromEpoch(rvTimeStamp(i))
			sDateTime = tCurTime % toIso()
			
			cartesian = rmNrotVel(i,:)
			polar = CartesianToPolar3(cartesian, WCONV_PROVENANCE_TO_FLOW)
			
			write(10,"(a,',', f5.1, 5(',', f6.2), 9(',', f7.4))") &
				sDateTime, &
				polar(2), &
				polar(1), &
				rvT(i), &
				rvTheta(i), rvPhi(i), &
				rmNrotVel(i,3), &
				raCovVel(i,1,1), raCovVel(i,1,2), raCovVel(i,1,3), &
				raCovVel(i,2,2), raCovVel(i,2,3), raCovVel(i,3,3), &
				rmCovT(i,1), rmCovT(i,2), rmCovT(i,3)
			
		end do
		
	end do

end program sonicpro
