! Program am_test - Eddy-covariance program, devoted to testing pbl_met routines towards
! the Ameriflux Gold Files.
!
! This module is part of the pbl_met library.
!
! Copyright 2018 by Servizi Territorio srl
!
! This is open-source code, covered by lGPL 3.0 license.
!
! Author: Mauri Favaron
!
program am_test

	use pbl_met
	use usa1
	
	implicit none
	
	! Locals
	character(len=256)						:: sDataPath
	character(len=256)						:: sFileName
	character(len=256)						:: sOutputFile
	character(len=19)						:: sFirstDateTime
	character(len=19)						:: sLastDateTime
	character(len=16)						:: sBuffer
	integer									:: iRetCode
	integer									:: iNumHours
	type(DateTime)							:: tFrom
	type(DateTime)							:: tCurTime
	type(Usa1DataDir)						:: tDir
	type(SonicData)							:: tSonic
	type(EddyCovData)						:: tEc
	real(8)									:: rFrom
	real(8)									:: rTo
	real(8)									:: rHold
	integer									:: iMode
	integer									:: i
	integer									:: iAvgTime
	character(len=23)						:: sDateTime
	integer, dimension(:), allocatable		:: ivNumData
	real(8), dimension(:), allocatable		:: rvTimeStamp
	real(8), dimension(:), allocatable		:: rvTheta
	real(8), dimension(:), allocatable		:: rvPhi
	real(8), dimension(:), allocatable		:: rvPsi
	real(8), dimension(:), allocatable		:: rvT
	real(8), dimension(:), allocatable		:: rvVarT
	real(8), dimension(:,:), allocatable	:: rmNrotVel
	real(8), dimension(:,:), allocatable	:: rmVel
	real(8), dimension(:,:,:), allocatable	:: raCovVel
	real(8), dimension(:,:,:), allocatable	:: raCovWind
	real(8), dimension(:,:,:), allocatable	:: raNrotCovVel
	real(8), dimension(:,:), allocatable	:: rmCovT
	real(8), dimension(:,:), allocatable	:: rmNrotCovT
	real, dimension(3)						:: cartesian
	real, dimension(3)						:: polar
	real(8), dimension(:), allocatable		:: rvUstar
	real(8), dimension(:), allocatable		:: rvH0
	integer									:: iDayIdx
	logical									:: lIsWater
	logical									:: lIsCarbonDioxide
	
	! Constants
	character(len=19), dimension(2), parameter	:: svAfDate  = ["2015-04-14 00:00:00", "2015-06-30 00:00:00"]
	character(len=19), dimension(2), parameter	:: svOutFile = ["AF_20150414.csv", "AF_20150630.csv"]
	
	! Get parameters
	if(command_argument_count() /= 0) then
		print *, "sonicpro - Minimalistic eddy covariance calculator"
		print *
		print *, "Usage:"
		print *
		print *, "  ./am_test"
		print *
		print *
		print *, "Program 'am_test' is part of the pbl_met project, and is"
		print *, "released under an lGPL-3.0 open source license."
		print *
		stop
	end if
	sDataPath = "./"
	iAvgTime  = 1800	! Ameriflux prescribed test condition
	
	! Iterate over the two test days
	do iDayIdx = 1, size(svAfDate)
	
		sOutputFile = svOutFile(iDayIdx)
	
		! Convert dates and times to DateTime values
		read(svAfDate(iDayIdx), "(i4.4,3(1x,i2.2))", iostat=iRetCode) &
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
		rTo = rFrom + 23.d0 * 3600.d0
		if(rTo < rFrom) then
			rHold = rFrom
			rFrom = rTo
			rTo   = rHold
		end if
	
		! Compute number of hours in set
		iNumHours = nint((rTo - rFrom)/3600.d0) + 1
	
		! Map data files
		iRetCode = tDir % mapFiles(sDataPath, rFrom, iNumHours, .false., iLoggerType = LOGGER_SONICLIB_MFC2)
		if(iRetCode /= 0) then
			print *, "Error searching for AmeriFlux Golden data files"
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
		write(10,"('date, dir, vel, temp, theta, phi, w.nrot, uu, uv, uw, vv, vw, ww, ut, vt, wt, u.star, H0, Corr.UW')")
		do while(iMode /= DE_ERR)
	
			! Read data to hourly SonicData object
			iRetCode = tSonic % readSonicLib(11, sFileName, OS_UNIX)
			if(iRetCode /= 0) then
				print *, "Error reading file - Return code = ", iRetCode
				cycle
			end if
		
			! Inform users what just happened, before moving on
			lIsWater         = tSonic % isWater()
			lIsCarbonDioxide = tSonic % isCarbonDioxide()
			print *, 'Processing ', trim(sFileName), " - ", lIsWater, ',', lIsCarbonDioxide
			
			! Compute averages
			iRetCode = tSonic % averages(1800, tEc)
			if(iRetCode /= 0) then
				print *, "Error averaging file - Return code = ", iRetCode
				cycle
			end if
		
			! Get next file name, if exists; the value of iMode parameter is changed automatically,
			! so there is no need to set it directly
			iRetCode = tDir % getFile(iMode, sFileName)
			if(iRetCode /= 0) then
				print *, "Error accessing file list - Return code = ", iRetCode
				cycle
			end if
		
		end do
		
		! End with current output file
		close(10)

	end do

end program am_test
