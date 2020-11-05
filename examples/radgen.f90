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
	integer								:: iYear
	integer								:: iMonth
	integer								:: iDay
	integer								:: i
	integer								:: j
	integer								:: k
	integer								:: l
	integer								:: ij
	integer								:: ik
	integer								:: il
	type(DateTime)						:: tDateTime
	integer								:: iTimeDelta
	real(8), dimension(:), allocatable	:: rvTimeStamp
	real, dimension(:), allocatable		:: rvRgMin
	real, dimension(:), allocatable		:: rvRgMax
	real, dimension(:), allocatable		:: rvRgMean0
	real, dimension(:), allocatable		:: rvRgStdDev0
	real, dimension(:), allocatable		:: rvRgMean1
	real, dimension(:), allocatable		:: rvRgStdDev1
	real, dimension(11,11,15)			:: raRg0
	real, dimension(11,11,15)			:: raRg1
	real								:: rMinRg0
	real								:: rMaxRg0
	real								:: rMinRg1
	real								:: rMaxRg1
	real								:: rMinRg
	real								:: rMaxRg
	logical								:: lEasyMode
	real								:: rTemp
	real								:: rRelH
	real								:: rPres
	
	! Get parameters
	if(command_argument_count() /= 8 .and. command_argument_count() /= 11) then
		print *, "radgen - Generator for position- and time-aware simulated global radiation data"
		print *
		print *, "Usage:"
		print *
		print *, "  ./radgen <lat> <lon> <zone> <dfrom> <dto> <step> <toption> [<Temp> <RelH> <Pres>] <out_file>"
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
		print *, "   - <Temp> is temperature, in °C"
		print *
		print *, "   - <RelH> is relative humiidity, in %"
		print *
		print *, "   - <Pres> is air pressure, in hPa"
		print *
		print *, "   - <out_file> is a valid file name for your operating system"
		print *
		print *, "Square brackets indicate an optional part."
		print *
		print *, "Copyright 2018 by Patrizia Favaron"
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
	if(command_argument_count() == 8) then
		call get_command_argument(8, sOutFile)
		lEasyMode = .false.
	else
		call get_command_argument(8, sBuffer)
		read(sBuffer, *, iostat=iRetCode) rTemp
		if(iRetCode /= 0) then
			print *, "radgen:: error: Invalid value specified for <Temp>"
			stop
		end if
		if(rTemp < -40.0 .or. rTemp > 60.0) then
			print *, "radgen:: error: Value specified for <Temp> is not in range [-40,+60]"
			stop
		end if
		call get_command_argument(9, sBuffer)
		read(sBuffer, *, iostat=iRetCode) rRelH
		if(iRetCode /= 0) then
			print *, "radgen:: error: Invalid value specified for <RelH>"
			stop
		end if
		if(rRelH < 0.0 .or. rRelH > 100.0) then
			print *, "radgen:: error: Value specified for <RelH> is not in range [0,100]"
			stop
		end if
		call get_command_argument(10, sBuffer)
		read(sBuffer, *, iostat=iRetCode) rPres
		if(iRetCode /= 0) then
			print *, "radgen:: error: Invalid value specified for <Pres>"
			stop
		end if
		if(rPres < 800.0 .or. rPres > 1100.0) then
			print *, "radgen:: error: Value specified for <Pres> is not in range [800,1100]"
			stop
		end if
		call get_command_argument(11, sOutFile)
		lEasyMode = .true.
	end if
	
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
	allocate(rvRgMin(size(rvTimeStamp)))
	allocate(rvRgMax(size(rvTimeStamp)))
	allocate(rvRgMean0(size(rvTimeStamp)))
	allocate(rvRgStdDev0(size(rvTimeStamp)))
	allocate(rvRgMean1(size(rvTimeStamp)))
	allocate(rvRgStdDev1(size(rvTimeStamp)))
	
	! Predict clear-sky radiation using ASCE "accurate" method
	do i = 1, size(rvTimeStamp)
		iRetCode = tDateTime % fromEpoch(rvTimeStamp(i))
		sDateTime = tDateTime % toISO()
		print *, "Processing time step ", sDateTime
		if(lEasyMode) then
			rvRgMin(i) = ClearSkyRg_Accurate( &
				sDateTime, &
				float(iTimeStep), &
				rLat, &
				rLon, &
				float(iTimeZone), &
				rPres, &
				rTemp, &
				rRelH, &
				0.5 &
			) / 10.0
			rvRgMax(i) = ClearSkyRg_Accurate( &
				sDateTime, &
				float(iTimeStep), &
				rLat, &
				rLon, &
				float(iTimeZone), &
				rPres, &
				rTemp, &
				rRelH, &
				1.0 &
			) / 10.0
		else
			ij = 0
			do j = -40, 60, 10
				ij = ij + 1
				ik = 0
				do k = 980, 1030, 5
					ik = ik + 1
					il = 0
					do l = 30, 100, 5
						il = il + 1
						raRg0(ij,ik,il) = ClearSkyRg_Accurate( &
							sDateTime, &
							float(iTimeStep), &
							rLat, &
							rLon, &
							float(iTimeZone), &
							float(k), &
							float(j), &
							float(l), &
							0.5 &
						) / 10.0
						raRg1(ij,ik,il) = ClearSkyRg_Accurate( &
							sDateTime, &
							float(iTimeStep), &
							rLat, &
							rLon, &
							float(iTimeZone), &
							float(k), &
							float(j), &
							float(l), &
							1.0 &
						) / 10.0
					end do
				end do
			end do
			rMinRg0 = minval(raRg0)
			rMaxRg0 = maxval(raRg0)
			rMinRg1 = minval(raRg1)
			rMaxRg1 = maxval(raRg1)
			rMinRg  = min(rMinRg0, rMinRg1)
			rMaxRg  = max(rMaxRg0, rMaxRg1)
			rvRgMin(i) = rMinRg
			rvRgMax(i) = rMaxRg
			rvRgMean0(i) = sum(raRg0) / (11*11*15)
			rvRgMean1(i) = sum(raRg1) / (11*11*15)
			rvRgStdDev0(i) = sqrt(sum((raRg0-rvRgMean0(i))**2) / (11*11*15))
			rvRgStdDev1(i) = sqrt(sum((raRg1-rvRgMean1(i))**2) / (11*11*15))
		end if
	end do
	
	! Set time stamp back, if needed
	if(iTimeStampOption == 1) then
		rvTimeStamp = rvTimeStamp + iTimeStep / 2.0d0
	elseif(iTimeStampOption == 2) then
		rvTimeStamp = rvTimeStamp + iTimeStep
	end if
	
	! Write results
	open(10, file=sOutFile, status='unknown', action='write')
	if(lEasyMode) then
		write(10, "('Time.Stamp,Rg.Min,Rg.Max')")
		do i = 1, size(rvTimeStamp)
			iRetCode = tDateTime % fromEpoch(rvTimeStamp(i))
			write(10, "(a,2(',',f6.1))") &
				tDateTime % toISO(), &
				rvRgMin(i), &
				rvRgMax(i)
		end do
	else
		write(10, "('Time.Stamp,Rg.Min,Rg.Max,Rg.Mean.0,RgStdDev.0,Rg.Mean.1,RgStdDev.1')")
		do i = 1, size(rvTimeStamp)
			iRetCode = tDateTime % fromEpoch(rvTimeStamp(i))
			write(10, "(a,6(',',f6.1))") &
				tDateTime % toISO(), &
				rvRgMin(i), &
				rvRgMax(i), &
				rvRgMean0(i), &
				rvRgStdDev0(i), &
				rvRgMean1(i), &
				rvRgStdDev1(i)
		end do
	end if
	close(10)
	
	deallocate(rvRgStdDev1)
	deallocate(rvRgMean1)
	deallocate(rvRgStdDev0)
	deallocate(rvRgMean0)
	deallocate(rvRgMax)
	deallocate(rvRgMin)
	deallocate(rvTimeStamp)
	
end program RadGen
