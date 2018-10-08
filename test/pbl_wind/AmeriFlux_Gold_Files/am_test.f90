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
	character(len=23)						:: sCurTime
	integer									:: iRetCode
	integer									:: iNumHours
	type(DateTime)							:: tFrom
	type(DateTime)							:: tCurTime
	type(Usa1DataDir)						:: tDir
	type(SonicData)							:: tSonic
	type(EddyCovData)						:: tEc
	type(EddyCovData), dimension(2)			:: tvDay
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
	real(8), dimension(:), allocatable		:: rvQ
	real(8), dimension(:), allocatable		:: rvC
	real(8), dimension(:), allocatable		:: rvVarT
	real(8), dimension(:), allocatable		:: rvVarQ
	real(8), dimension(:), allocatable		:: rvVarC
	real(8), dimension(:,:), allocatable	:: rmNrotVel
	real(8), dimension(:,:), allocatable	:: rmVel
	real, dimension(:,:), allocatable		:: rmPolar
	real(8), dimension(:,:,:), allocatable	:: raCovVel
	real(8), dimension(:,:,:), allocatable	:: raCovWind
	real(8), dimension(:,:,:), allocatable	:: raNrotCovVel
	real(8), dimension(:,:), allocatable	:: rmCovT
	real(8), dimension(:,:), allocatable	:: rmRotCovT
	real(8), dimension(:,:), allocatable	:: rmCovQ
	real(8), dimension(:,:), allocatable	:: rmCovC
	real(8), dimension(:,:), allocatable	:: rmNrotCovT
	real, dimension(3)						:: cartesian
	real, dimension(3)						:: polar
	real(8), dimension(:), allocatable		:: rvUstar
	real(8), dimension(:), allocatable		:: rvUstar_3
	real(8), dimension(:), allocatable		:: rvH0
	real(8), dimension(:), allocatable		:: rvHe
	real(8), dimension(:), allocatable		:: rvFqMolar
	real(8), dimension(:), allocatable		:: rvFqMass
	real(8), dimension(:), allocatable		:: rvFcMolar
	real(8), dimension(:), allocatable		:: rvFcMass
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
	
		! Compute number of hours in set, and use to reserve workspace
		iNumHours = nint((rTo - rFrom)/3600.d0) + 1
		iRetCode = tvDay(iDayIdx) % createEmpty(iNumHours, 1800)
	
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
		write(10, &
			"('date, u, v, w, dir, vel, temp, " // &
			"qq, uq, vq, wq, " // &
			"cc, uc, vc, wc, " // &
			"uu, uv, uw, vv, vw, ww, " // &
			"tt, ut, vt, wt, " // &
			"theta, phi, " // &
			"rot.uu, rot.uv, rot.uw, rot.vv, rot.vw, rot.ww, " // &
			"rot.ut, rot.vt, rot.wt, " // &
			"u.star, H0, " // &
			"He, Q, Fq.Molar, Fq.Mass, " // &
			"C, Fc.Molar, Fc.Mass')" &
		)
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
			
			! Compute averages, and prime the 'tEc' EddyCovData object with them.
			iRetCode = tSonic % averages(1800, tEc)
			if(iRetCode /= 0) then
				print *, "Error averaging file - Return code = ", iRetCode
				cycle
			end if
			
			! Perform eddy covariance processing
			iRetCode = tEc % process(iNumRot=2)
			if(iRetCode /= 0) then
				print *, "Error performing basic eddy-covariance calculations - Return code = ", iRetCode
				cycle
			end if
			iRetCode = tEc % getWind(rmPolar, WCONV_FLOW_TO_PROVENANCE)
			
			! Save results
			iRetCode = tvDay(iDayIdx) % add(rFrom, tEc)
			if(iRetCode /= 0) then
				print *, "Error performing data addition to multi-hour set - Return code = ", iRetCode
				cycle
			end if
			
			! Get next file
			iRetCode = tDir % getFile(iMode, sFileName)
			
		end do
			
		! Write results to local routine workspace
		iRetCode = tvDay(iDayIdx) % getTimeStamp(rvTimeStamp)
		iRetCode = tvDay(iDayIdx) % getNumData(ivNumData)
		iRetCode = tvDay(iDayIdx) % getWindVector(rmVel)
		iRetCode = tvDay(iDayIdx) % getWind(rmPolar, WCONV_FLOW_TO_PROVENANCE)
		iRetCode = tvDay(iDayIdx) % getCovWind(raNrotCovVel)
		iRetCode = tvDay(iDayIdx) % getRotCovWind(raCovVel)
		iRetCode = tvDay(iDayIdx) % getTemp(rvT)
		iRetCode = tvDay(iDayIdx) % getVarT(rvVarT)
		iRetCode = tvDay(iDayIdx) % getCovT(rmCovT)
		iRetCode = tvDay(iDayIdx) % getRotAngles(rvTheta, rvPhi, rvPsi)
		iRetCode = tvDay(iDayIdx) % getUstar(rvUstar, rvUstar_3)
		iRetCode = tvDay(iDayIdx) % getHeatFluxes(rvH0, rvHe)
		iRetCode = tvDay(iDayIdx) % getH2O(rvQ, rvFqMolar, rvFqMass)
		iRetCode = tvDay(iDayIdx) % getCO2(rvC, rvFcMolar, rvFcMass)
		iRetCode = tvDay(iDayIdx) % getInputGases(ivNumData, rvQ, rmCovQ, rvVarQ, rvC, rmCovC, rvVarC)
		iRetCode = tvDay(iDayIdx) % getRotCovTemp(rmRotCovT)
		
		! Print results
		do i = 1, size(rvTimeStamp)
			iRetCode = tCurTime % fromEpoch(rvTimeStamp(i))
			sCurTime = tCurTime % toISO()
			write(10,"(a,44(',',e15.7))") &
				sCurTime, &
				rmVel(i,1), rmVel(i,2), rmVel(i,3), &
				rmPolar(i,2), rmPolar(i,1), &
				rvT(i), &
				rvVarQ(i), &
				rmCovQ(i,1), rmCovQ(i,2), rmCovQ(i,3), &
				rvVarC(i), &
				rmCovC(i,1), rmCovC(i,2), rmCovC(i,3), &
				raNrotCovVel(i,1,1), raNrotCovVel(i,1,2), raNrotCovVel(i,1,3), &
				raNrotCovVel(i,2,2), raNrotCovVel(i,2,3), &
				raNrotCovVel(i,3,3), &
				rvVarT(i), rmCovT(i,1), rmCovT(i,2), rmCovT(i,3), &
				rvTheta(i), rvPhi(i), &
				raCovVel(i,1,1), raCovVel(i,1,2), raCovVel(i,1,3), &
				raCovVel(i,2,2), raCovVel(i,2,3), &
				raCovVel(i,3,3), &
				rmRotCovT(i,1), rmRotCovT(i,2), rmRotCovT(i,3), &
				rvUstar(i), &
				rvH0(i), rvHe(i), &
				rvQ(i), rvFqMolar(i), rvFqMass(i), &
				rvC(i), rvFcMolar(i), rvFcMass(i)
		end do
		
		! Get next file name, if exists; the value of iMode parameter is changed automatically,
		! so there is no need to set it directly
		iRetCode = tDir % getFile(iMode, sFileName)
		if(iRetCode /= 0) then
			print *, "Error accessing file list - Return code = ", iRetCode
			cycle
		end if
	
		! End with current output file
		close(10)

	end do

end program am_test
