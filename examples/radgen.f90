! Simple example program producing reference values for global radiation, given! time interval and position.
! Results is a CSV file, containing the desired values. The form allows comparison to e.g. measured data.

program RadGen

	use pbl_met
	
	implicit none
	
	! Locals
	integer								:: iRetCode
	character(len=256)					:: sOutFile
	real								:: rLat
	real								:: rLon
	integer								:: iTimeZone
	real(8)								:: rDateFrom
	real(8)								:: rDateTo
	integer								:: iTimeStep
	integer								:: iTimeStampOption
	character(len=256)					:: sBuffer
	integer								:: iYear
	integer								:: iMonth
	integer								:: iDay
	type(DateTime)						:: tDateTime
	integer								:: iTimeDelta
	real(8), dimension(:), allocatable	:: rvTimeStamp
	
	! Get parameters
	if(command_argument_count() /= 8) then
		print *, "radgen - Generator for position- and time-aware simulated global radiation data"
		print *
		print *, "Usage:"
		print *
		print *, "  ./radgen <lat> <lon> <time_zone> <day_from> <day_to> <time_step> <time_stamp_option> <out_file_name>"
		print *
		print *, "where"
		print *
		print *, "   - <lat> and <lon> are latitude and longitude, in decimal degrees form; longitude is positive eastwards"
		print *
		print *, "   - <time_zone> is an integer number stating the hour shift from UTC, e.g. 1 for Berlin, Paris and Rome"
		print *
		print *, "   - <day_from> and <day_to> are dates in ISO form, i.e. YYYY-MM-DD"
		print *
		print *, "   - <time_step> is the length of time sampling period, in seconds"
		print *
		print *, "   - <time_stamp_option> may be 0 (anticipated time stamp), 1 (middle) or 2 (posticipated time stamp)"
		print *
		print *, "   - <out_file_name> is a valid file name for your operating system"
		print *
		print *, "Copyright 2018 by Mauri Favaron"
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
		print *, "radgen:: error: Invalid value specified for <time_zone> (should be -12 to 23)"
		stop
	end if
	if(iTimeStep <= 0) then
		print *, "radgen:: error: Value specified for <time_step> is not positive"
		stop
	end if
	call get_command_argument(4, sBuffer)
	read(sBuffer, "(i4,2(1x,i2))", iostat=iRetCode) iYear, iMonth, iDay
	if(iRetCode /= 0) then
		print *, "radgen:: error: Invalid value specified for <day_from> (should be like 2018-01-01)"
		stop
	end if
	tDateTime = DateTime(iYear, iMonth, iDay, 0, 0, 0.d0)
	rDateFrom = tDateTime % toEpoch()
	call get_command_argument(5, sBuffer)
	read(sBuffer, "(i4,2(1x,i2))", iostat=iRetCode) iYear, iMonth, iDay
	if(iRetCode /= 0) then
		print *, "radgen:: error: Invalid value specified for <day_to> (should be like 2018-12-31)"
		stop
	end if
	tDateTime = DateTime(iYear, iMonth, iDay, 0, 0, 0.d0)
	rDateTo = tDateTime % toEpoch()
	if(rDateFrom >= rDateTo) then
		print *, "radgen:: error: <date_from> is larger of, or equals, <date_to>"
		stop
	end if
	call get_command_argument(6, sBuffer)
	read(sBuffer, *, iostat=iRetCode) iTimeStep
	if(iRetCode /= 0) then
		print *, "radgen:: error: Invalid value specified for <time_step> (should be like 2018-12-31)"
		stop
	end if
	if(iTimeStep <= 0) then
		print *, "radgen:: error: Value specified for <time_step> is not positive"
		stop
	end if
	call get_command_argument(7, sBuffer)
	read(sBuffer, *, iostat=iRetCode) iTimeStampOption
	if(iRetCode /= 0) then
		print *, "radgen:: error: Invalid value specified for <time_stamp_option>"
		stop
	end if
	if(iTimeStampOption <= 0) then
		print *, "radgen:: error: Value specified for <time_stamp_option> (should be 0 to 2)"
		stop
	end if
	call get_command_argument(8, sOutFile)
	
	! Generate the sequence of time stamps to which global radiation will be estimated.
	iRetCode = timeSequence(rDateFrom, rDateTo, iTimeStep, .true., rvTimeStamp)
	if(iRetCode /= 0) then
		print *, "radgen:: error: Empty or otherwise wrong times sequence"
		stop
	end if
	
end program RadGen
