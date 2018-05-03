! Program illustrating some data cleaning and statistical processing
!
! Written by: Mauri Favaron
!
! This program is part of the pbl_met documentation and testing project.
!
! This is open-source code, covered by the lGPL 3.0 license.
!
program tStat

	use pbl_met
	
	implicit none
	
	! Locals
	integer				:: iRetCode
	character(len=256)	:: sInputFile
	character(len=256)	:: sOutputFile
	type(TimeSeries)	:: ts
	real				:: rValid, rMin, rMean, rStdDev, rMax
	integer				:: iNumData
	integer				:: iNumLines
	integer				:: iLine
	character(len=128)	:: sBuffer
	real(8)				:: rMinDelta, rDelta, rMaxDelta
	real(8), dimension(:), allocatable	:: rvTimeStamp
	real, dimension(:), allocatable		:: rvValue
	integer								:: iYear, iMonth, iDay, iHour, iMinute
	integer								:: iFirstComma
	integer								:: iCurrentTime
	
	! Constants
	integer, parameter	:: COL_TIMESTAMP = 2
	integer, parameter	:: COL_DATA      = 3
	
	! Get input parameters
	if(command_argument_count() /= 2) then
		print *,'tStat - Program demonstrating use of time series functions'
		print *
		print *,'Usage:'
		print *
		print *,'  ./tStat <input_file> <output_file>'
		print *
		print *,'where'
		print *
		print *,'  <input_file> is the name of an ARPA Lombardia meteo file.'
		print *
		print *,'This program is part of pbl_met documentation and testing project.'
		print *
		print *,'his is open-source code, covered by the lGPL 3.0 license.'
		print *
		stop
	end if
	call get_command_argument(1, sInputFile)
	call get_command_argument(2, sOutputFile)
	
	! Get data, assuming ARPA Lombardy format
	open(10, file=sInputFile, status='old', action='read', iostat=iRetCode)
	if(iRetCode /= 0) then
		print *, "tStat:: error: Input file not opened - check name, position and access rights"
		stop
	end if
	iNumLines = -1	! Take header into account
	iFirstComma = 0
	do
		read(10, "(a)", iostat=iRetCode) sBuffer
		if(iRetCode /= 0) exit
		iNumLines = iNumLines + 1
		if(iNumLines == 1) iFirstComma = index(sBuffer, ",")
	end do
	if(iNumLines <= 0) then
		print *, "tStat:: error: No useable data in file"
	end if
	allocate(rvTimeStamp(iNumLines), rvValue(iNumLines))
	rewind(10)
	read(10, "(a)") sBuffer	! Skip header
	do iLine = 1, iNumLines
		read(10, "(a)") sBuffer
		read(sBuffer(iFirstComma+1:iFirstComma+16), "(i4,4(1x,i2))") &
			iYear, iMonth, iDay, iHour, iMinute
		call PackTime(iCurrentTime, iYear, iMonth, iDay, iHour, iMinute, 0)
		rvTimeStamp(iLine) = iCurrentTime
		read(sBuffer(iFirstComma+18:), *) rvValue(iLine)
	end do
	close(10)
	
	! Check time stamps are ordered, and whether they form a gap-less series (they should
	! for ARPA Lombardy proper data files)
	rMinDelta =  huge(rMinDelta)
	rMaxDelta = -huge(rMaxDelta)
	do iLine = 2, size(rvTimeStamp)
		rDelta = rvTimeStamp(iLine) - rvTimeStamp(iLine-1)
		rMinDelta = min(rMinDelta, rDelta)
		rMaxDelta = max(rMaxDelta, rDelta)
	end do
	if(rMinDelta /= rMaxDelta) then
		print *,"tStat:: error: Time stamps are not well-ordered as in ARPA Lombardy files"
		stop
	end if
	
	! Populate time series from input file
	iRetCode = ts % createFromDataVector(rvValue, rvTimeStamp(1), rDelta)
	deallocate(rvTimeStamp, rvValue)
	if(iRetCode /= 0) then
		print *, "tStat:: error: Creation of time series failed with return code = ", iRetCode
		stop
	end if
	
	! Range-invalidate data, assuming they are temperature reading (other quantities will demand different limits
	! (this code is for illustration only)
	call ts % rangeInvalidate(-40., +60.)
	
	! Compute and print a short summary on values
	call ts % summary(iNumData, rValid, rMin, rMean, rStdDev, rMax)
	print *,"Summary on dava values"
	print *,"======================"
	print *
	print *,"Number of data (valid and invalid) = ", iNumData
	print *,"Fraction of valid data             = ", rValid, "%"
	print *,"Minimum                            = ", rMin
	print *,"Mean                               = ", rMean
	print *,"Standard deviation                 = ", rStdDev
	print *,"Maximum                            = ", rMax
	print *

end program tStat
