! Simple example program producing reference values for global radiation, given! time interval and position.
! Results is a CSV file, containing the desired values. The form allows comparison to e.g. measured data.
!
! This is open-source code, covered by the MIT license.
!
! Author: Patrizia Favaron

program RadGen

	use pbl_met
	
	implicit none
	
	! Locals
	integer								:: iRetCode
	character(len=256)					:: sOutFile
	character(len=23)					:: sDateTime
	real								:: rLat
	real								:: rLon
	integer								:: iTimeZone
	real(8)								:: rDateFrom
	real(8)								:: rDateTo
	integer								:: iTimeStep
	integer								:: iTimeStampOption
	character(len=256)					:: sBuffer
	integer								:: i
	integer								:: iYear
	integer								:: iMonth
	integer								:: iDay
	type(DateTime)						:: tDateTime
	integer								:: iTimeDelta
	real(8), dimension(:), allocatable	:: rvTimeStamp
	real, dimension(:), allocatable		:: rvRa
	real, dimension(:), allocatable		:: rvRg
	real								:: rZ
	
	! Get parameters
	if(command_argument_count() /= 9) then
		print *, "radgen - Generator for position- and time-aware simulated global radiation data"
		print *
		print *, "Usage:"
		print *
		print *, "  ./radgen <lat> <lon> <zone> <dfrom> <dto> <step> <toption> <Z> <out_file>"
		print *
		print *, "where"
		print *
		print *, "   - <lat> and <lon> are latitude and longitude, in decimal degrees form; longitude is positive eastwards"
		print *
		print *, "   - <zone> is an integer number stating the hour shift from UTC, e.g. 1 for Berlin, Paris and Rome"
		print *
		print *, "   - <dfrom> and <dto> are initial and final dates in ISO form, i.e. YYYY-MM-DD"
		print *
		print *, "   - <step> is the length of time sampling period, in seconds"
		print *
		print *, "   - <toption> may be 0 (anticipated time stamp), 1 (middle) or 2 (posticipated time stamp)"
		print *
		print *, "   - <Z> is the height above mean sea level, in m"
		print *
		print *, "   - <out_file> is a valid file name for your operating system"
		print *
		print *, "Copyright 2020 by Patrizia Favaron"
		print *, "                  This is open-source software, covered by MIT license."
		print *
		stop
	end if
	call get_command_argument(1, sBuffer)
	read(sBuffer, *, iostat=iRetCode) rLat
	if(iRetCode /= 0) then
		print *, "radgen:: error: Invalid value specified for <lat>"
		stop
	end if
	if(rLat < -90.0 .or. rLat > 90.0) then
		print *, "radgen:: error: Off-range value specified for <lat> (should be between -90 and +90)"
		stop
	end if
	call get_command_argument(2, sBuffer)
	read(sBuffer, *, iostat=iRetCode) rLon
	if(iRetCode /= 0) then
		print *, "radgen:: error: Invalid value specified for <lon>"
		stop
	end if
	if(rLat < -180.0 .or. rLat > 360.0) then
		print *, "radgen:: error: Off-range value specified for <lon> (should be between -180 and +360)"
		stop
	end if
	call get_command_argument(3, sBuffer)
	read(sBuffer, *, iostat=iRetCode) iTimeZone
	if(iRetCode /= 0) then
		print *, "radgen:: error: Invalid value specified for <zone> (should be -12 to 23)"
		stop
	end if
	if(iTimeZone <= 0) then
		print *, "radgen:: error: Value specified for <zone> is not positive"
		stop
	end if
	call get_command_argument(4, sBuffer)
	read(sBuffer, "(i4,2(1x,i2))", iostat=iRetCode) iYear, iMonth, iDay
	if(iRetCode /= 0) then
		print *, "radgen:: error: Invalid value specified for <dfrom> (should be like 2018-01-01)"
		stop
	end if
	tDateTime = DateTime(iYear, iMonth, iDay, 0, 0, 0.d0)
	rDateFrom = tDateTime % toEpoch()
	call get_command_argument(5, sBuffer)
	read(sBuffer, "(i4,2(1x,i2))", iostat=iRetCode) iYear, iMonth, iDay
	if(iRetCode /= 0) then
		print *, "radgen:: error: Invalid value specified for <dto> (should be like 2018-12-31)"
		stop
	end if
	tDateTime = DateTime(iYear, iMonth, iDay, 0, 0, 0.d0)
	rDateTo = tDateTime % toEpoch() + 24*3600.d0
	if(rDateFrom >= rDateTo) then
		print *, "radgen:: error: <dfrom> is larger of, or equals, <dto>"
		stop
	end if
	call get_command_argument(6, sBuffer)
	read(sBuffer, *, iostat=iRetCode) iTimeStep
	if(iRetCode /= 0) then
		print *, "radgen:: error: Invalid value specified for <step>"
		stop
	end if
	if(iTimeStep <= 0) then
		print *, "radgen:: error: Value specified for <step> is not positive"
		stop
	end if
	call get_command_argument(7, sBuffer)
	read(sBuffer, *, iostat=iRetCode) iTimeStampOption
	if(iRetCode /= 0) then
		print *, "radgen:: error: Invalid value specified for <option>"
		stop
	end if
	if(iTimeStampOption < 0 .or. iTimeStampOption > 2) then
		print *, "radgen:: error: Value specified for <option> (should be 0 to 2)"
		stop
	end if
	call get_command_argument(8, sBuffer)
	read(sBuffer, *, iostat=iRetCode) rZ
	if(iRetCode /= 0) then
		print *, "radgen:: error: Invalid value specified for <Z>"
		stop
	end if
	if(rZ < -500.0 .or. rZ > 8848.0) then
		print *, "radgen:: error: Value specified for <Z> is not in range [-500,+8848]"
		stop
	end if
	call get_command_argument(9, sOutFile)
	
	! Generate the sequence of time stamps to which global radiation will be estimated.
	iRetCode = timeSequence(rDateFrom, rDateTo, iTimeStep, .false., rvTimeStamp)
	if(iRetCode /= 0) then
		print *, "radgen:: error: Empty or otherwise wrong times sequence - iRetCode = ", iRetCode
		stop
	end if
	
	! Adjust time stamp to anticipated
	if(iTimeStampOption == 1) then
		rvTimeStamp = rvTimeStamp - iTimeStep / 2.0d0
	elseif(iTimeStampOption == 2) then
		rvTimeStamp = rvTimeStamp - iTimeStep
	end if
	
	! Reserve space for radiation (result) vectors
	allocate(rvRa(size(rvTimeStamp)))
	allocate(rvRg(size(rvTimeStamp)))
	
	! Predict clear-sky radiation using ASCE "accurate" method
	do i = 1, size(rvTimeStamp)
		iRetCode = tDateTime % fromEpoch(rvTimeStamp(i))
		sDateTime = tDateTime % toISO()
		rvRa(i) = ExtraterrestrialRadiation( &
			sDateTime, &
			float(iTimeStep), &
			rLat, &
			rLon, &
			float(iTimeZone) &
		)
		rvRg(i) = ClearSkyRg_Simple( rvRa(i), rZ )
	end do
	
	! Set time stamp back, if needed
	if(iTimeStampOption == 1) then
		rvTimeStamp = rvTimeStamp + iTimeStep / 2.0d0
	elseif(iTimeStampOption == 2) then
		rvTimeStamp = rvTimeStamp + iTimeStep
	end if
	
	! Write results
	open(10, file=sOutFile, status='unknown', action='write')
	write(10, "('Time.Stamp,Ra,Rg')")
	do i = 1, size(rvTimeStamp)
		iRetCode = tDateTime % fromEpoch(rvTimeStamp(i))
		write(10, "(a,2(',',f6.1))") &
			tDateTime % toISO(), &
			rvRa(i) / 10., &
			rvRg(i) / 10.
	end do
	close(10)
	
	deallocate(rvRg)
	deallocate(rvRa)
	deallocate(rvTimeStamp)
	
end program RadGen
