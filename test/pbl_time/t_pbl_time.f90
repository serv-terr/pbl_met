! Test program for pbl_time module routines
!
! This code is part of the pbl_met library.
!
! This is open-source code, covered by the lGPL 3.0 license.
!
program test_pbl_time

	use pbl_met
	
	implicit none
	
	call tst_Generic()
	call tst_LinearIndex()
	
contains

	subroutine tst_Generic()
	
		! Routine arguments
		! --none--
	
		! Locals
		integer				:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		type(DateTime)		:: dt
		character(len=23)	:: sDateTime
		integer				:: iRetCode
		real(8)				:: rEpoch
		integer				:: iEpoch
		integer				:: iNumHours
		integer				:: i
	
		real(8), dimension(:), allocatable	:: rvTimeStamp
		integer, dimension(:), allocatable	:: ivTimeStamp
		integer, dimension(:), allocatable	:: ivTimeCode1
		integer, dimension(:), allocatable	:: ivTimeCode2
		integer, dimension(:), allocatable	:: ivYear1
		integer, dimension(:), allocatable	:: ivYear2
		integer, dimension(:), allocatable	:: ivMonth1
		integer, dimension(:), allocatable	:: ivMonth2
		integer, dimension(:), allocatable	:: ivYearMonth1
		integer, dimension(:), allocatable	:: ivYearMonth2
	
		! Time stamp related tests
		print *, "Test 1 - Maximum/minimum date/time values."
		call UnpackTime(0, iYear, iMonth, iDay, iHour, iMinute, iSecond)
		print "('Min date: ',i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))", iYear, iMonth, iDay, iHour, iMinute, iSecond
		call UnpackTime(huge(iYear), iYear, iMonth, iDay, iHour, iMinute, iSecond)
		print "('Max date: ',i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))", iYear, iMonth, iDay, iHour, iMinute, iSecond
		
		! Test 2: which is the largest representable date?
		print *, "Test 2 - Common date and time manipulations related to REAL(8) time stamps"
		iRetCode  = dt % fromEpoch(253402300799.999d0)
		sDateTime = dt % toIso()
		print *, "Maximum date-time: ", trim(sDateTime), "  (Return code = ", iRetCode, "  - Expected = 0)"
		dt = DateTime(10000, 1, 1, 0, 0, 0.d0)	! Modern Fortran default constructor!
		rEpoch = dt % toEpoch()
		print *, "Maximum Epoch value: ", rEpoch, "  (Return code = ", iRetCode, "  - Expected = 0)"
		iRetCode  = dt % fromEpoch(253402300800.0d0)
		sDateTime = dt % toIso()
		print *, "Invalid date-time: ", trim(sDateTime), "  (Return code = ", iRetCode, "  - Expected = 1)"
		iRetCode  = dt % fromEpoch(0.0d0)
		sDateTime = dt % toIso()
		print *, "Zero-epoch date-time: ", trim(sDateTime), "  (Return code = ", iRetCode, "  - Expected = 0)"
		iRetCode  = dt % fromEpoch(-1.0d0)
		sDateTime = dt % toIso()
		print *, "Invalid date-time: ", trim(sDateTime), "  (Return code = ", iRetCode, "  - Expected = 1)"
		print *
	
		! Test 3: Check the two forms of "timeEncode"
		iNumHours = 72
		allocate(rvTimeStamp(iNumHours), ivTimeStamp(iNumHours))
		dt = DateTime(2018, 3, 8, 0, 0, 0)
		rEpoch = dt % toEpoch()
		iEpoch = floor(rEpoch)
		rvTimeStamp = [(rEpoch + (i-1)*3600.d0, i = 1, iNumHours)]
		ivTimeStamp = [(iEpoch + (i-1)*3600, i = 1, iNumHours)]
		! Daily span, hourly step
		iRetCode = timeEncode(rvTimeStamp, 86400, 3600, ivTimeCode1)
		iRetCode = timeEncode(ivTimeStamp, 86400, 3600, ivTimeCode2)
		iRetCode = timeGetYear(rvTimeStamp, ivYear1)
		iRetCode = timeGetYear(ivTimeStamp, ivYear2)
		iRetCode = timeGetMonth(rvTimeStamp, ivMonth1)
		iRetCode = timeGetMonth(ivTimeStamp, ivMonth2)
		iRetCode = timeGetYearMonth(rvTimeStamp, ivYearMonth1)
		iRetCode = timeGetYearMonth(ivTimeStamp, ivYearMonth2)
		print *, "i,  Idx.1,  Idx.2, Year.1, Year.2, Month.1, Month.2, Yr.Mo.1, Yr.Mo.2"
		do i = 1, iNumHours
			print *, i, &
				ivTimeCode1(i), ivTimeCode2(i), &
				ivYear1(i), ivYear2(i), &
				ivMonth1(i), ivMonth2(i), &
				ivYearMonth1(i), ivYearMonth2(i)
		end do
		print *
	
		! Test 4: Some invalid values
		rvTimeStamp(3) = NaN_8
		ivTimeStamp(3) = 0
		! Daily span, hourly step
		iRetCode = timeEncode(rvTimeStamp, 86400, 3600, ivTimeCode1)
		iRetCode = timeEncode(ivTimeStamp, 86400, 3600, ivTimeCode2)
		iRetCode = timeGetYear(rvTimeStamp, ivYear1)
		iRetCode = timeGetYear(ivTimeStamp, ivYear2)
		iRetCode = timeGetMonth(rvTimeStamp, ivMonth1)
		iRetCode = timeGetMonth(ivTimeStamp, ivMonth2)
		iRetCode = timeGetYearMonth(rvTimeStamp, ivYearMonth1)
		iRetCode = timeGetYearMonth(ivTimeStamp, ivYearMonth2)
		print *, "i,  Idx.1,  Idx.2, Year.1, Year.2, Month.1, Month.2, Yr.Mo.1, Yr.Mo.2"
		do i = 1, iNumHours
			print *, i, &
				ivTimeCode1(i), ivTimeCode2(i), &
				ivYear1(i), ivYear2(i), &
				ivMonth1(i), ivMonth2(i), &
				ivYearMonth1(i), ivYearMonth2(i)
		end do
		deallocate(rvTimeStamp, ivTimeStamp, ivTimeCode1, ivTimeCode2, ivYear1, ivYear2, ivMonth1, ivMonth2, ivYearMonth1, ivYearMonth2)
		
	end subroutine tst_Generic
	
	
	subroutine tst_LinearIndex()
	
		! Routine arguments
		! --none--
		
		! Locals
		integer, dimension(:), allocatable	:: ivTimeCode
		integer, dimension(:), allocatable	:: ivTimeStamp
		real(8), dimension(:), allocatable	:: rvTimeStamp
		integer, dimension(:), allocatable	:: ivBaseTimeStamp
		real(8), dimension(:), allocatable	:: rvBaseTimeStamp
		type(DateTime)						:: dt
		integer								:: iCurTime
		integer								:: i
		integer								:: iRetCode
		integer								:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		character(len=19)					:: sDateTime1
		character(len=19)					:: sDateTime2
		character(len=23)					:: sDateTime3
		character(len=23)					:: sDateTime4
		
		! Test 1: Linear index under normal conditions, legacy time stamp
		call PackTime(iCurTime, 2000, 1, 1, 0, 0, 0)
		allocate(ivTimeStamp(48), rvTimeStamp(48))
		do i = 0, 47
			ivTimeStamp(i+1) = iCurTime + 3600*i
			rvTimeStamp(i+1) = ivTimeStamp(i+1)
		end do
		print *, "Test 1: Linear index, daily, legacy time stamp"
		iRetCode = timeLinearIndex(ivTimeStamp, DELTA_1_DAY, ivTimeCode, ivBaseTimeStamp)
		print *, "Return code: ", iRetCode, " (expected: 0)"
		print *
		print *, "Time.Stamp, Time.Index, Base.Time.Stamp"
		do i = 1, 48
			call UnpackTime(ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			write(sDateTime1, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))") iYear, iMonth, iDay, iHour, iMinute, iSecond
			call UnpackTime(ivBaseTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			write(sDateTime2, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))") iYear, iMonth, iDay, iHour, iMinute, iSecond
			print "(a,',',i8,', ',a)", sDateTime1, ivTimeCode(i), sDateTime2
		end do
		print *
		
		! Test 2: Linear index under normal conditions, new time stamp
		print *, "Test 2: Linear index, daily, new time stamp"
		iRetCode = timeLinearIndex(rvTimeStamp, DELTA_1_DAY, ivTimeCode, rvBaseTimeStamp)
		print *, "Return code: ", iRetCode, " (expected: 0)"
		print *
		print *, "Time.Stamp, Time.Index, Base.Time.Stamp"
		do i = 1, 48
			iRetCode = dt % fromEpoch(rvTimeStamp(i))
			sDateTime3 = dt % toIso()
			iRetCode = dt % fromEpoch(rvBaseTimeStamp(i))
			sDateTime4 = dt % toIso()
			print "(a,',',i8,', ',a)", sDateTime3, ivTimeCode(i), sDateTime4
		end do
		print *
		
		! Test 3: Linear index under normal conditions, legacy time stamp
		print *, "Test 3: Linear index, 8-hourly, legacy time stamp"
		iRetCode = timeLinearIndex(ivTimeStamp, DELTA_8_HOURS, ivTimeCode, ivBaseTimeStamp)
		print *, "Return code: ", iRetCode, " (expected: 0)"
		print *
		print *, "Time.Stamp, Time.Index, Base.Time.Stamp"
		do i = 1, 48
			call UnpackTime(ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			write(sDateTime1, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))") iYear, iMonth, iDay, iHour, iMinute, iSecond
			call UnpackTime(ivBaseTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			write(sDateTime2, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))") iYear, iMonth, iDay, iHour, iMinute, iSecond
			print "(a,',',i8,', ',a)", sDateTime1, ivTimeCode(i), sDateTime2
		end do
		print *
		
		! Test 4: Linear index under normal conditions, new time stamp
		print *, "Test 4: Linear index, 8-hourly, new time stamp"
		iRetCode = timeLinearIndex(rvTimeStamp, DELTA_8_HOURS, ivTimeCode, rvBaseTimeStamp)
		print *, "Return code: ", iRetCode, " (expected: 0)"
		print *
		print *, "Time.Stamp, Time.Index, Base.Time.Stamp"
		do i = 1, 48
			iRetCode = dt % fromEpoch(rvTimeStamp(i))
			sDateTime3 = dt % toIso()
			iRetCode = dt % fromEpoch(rvBaseTimeStamp(i))
			sDateTime4 = dt % toIso()
			print "(a,',',i8,', ',a)", sDateTime3, ivTimeCode(i), sDateTime4
		end do
		print *
		
		! Test 5: Linear index under normal conditions, legacy time stamp
		print *, "Test 5: Linear index, hourly, legacy time stamp"
		iRetCode = timeLinearIndex(ivTimeStamp, DELTA_1_HOUR, ivTimeCode, ivBaseTimeStamp)
		print *, "Return code: ", iRetCode, " (expected: 0)"
		print *
		print *, "Time.Stamp, Time.Index, Base.Time.Stamp"
		do i = 1, 48
			call UnpackTime(ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			write(sDateTime1, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))") iYear, iMonth, iDay, iHour, iMinute, iSecond
			call UnpackTime(ivBaseTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			write(sDateTime2, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))") iYear, iMonth, iDay, iHour, iMinute, iSecond
			print "(a,',',i8,', ',a)", sDateTime1, ivTimeCode(i), sDateTime2
		end do
		print *
		
		! Test 6: Linear index under normal conditions, new time stamp
		print *, "Test 6: Linear index, hourly, new time stamp"
		iRetCode = timeLinearIndex(rvTimeStamp, DELTA_1_HOUR, ivTimeCode, rvBaseTimeStamp)
		print *, "Return code: ", iRetCode, " (expected: 0)"
		print *
		print *, "Time.Stamp, Time.Index, Base.Time.Stamp"
		do i = 1, 48
			iRetCode = dt % fromEpoch(rvTimeStamp(i))
			sDateTime3 = dt % toIso()
			iRetCode = dt % fromEpoch(rvBaseTimeStamp(i))
			sDateTime4 = dt % toIso()
			print "(a,',',i8,', ',a)", sDateTime3, ivTimeCode(i), sDateTime4
		end do
		print *
		
		! Test 7: Linear index under normal conditions, legacy time stamp
		print *, "Test 7: Linear index, sub-hourly, legacy time stamp"
		iRetCode = timeLinearIndex(ivTimeStamp, 600, ivTimeCode, ivBaseTimeStamp)
		print *, "Return code: ", iRetCode, " (expected: 0)"
		print *
		print *, "Time.Stamp, Time.Index, Base.Time.Stamp"
		do i = 1, 48
			call UnpackTime(ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			write(sDateTime1, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))") iYear, iMonth, iDay, iHour, iMinute, iSecond
			call UnpackTime(ivBaseTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			write(sDateTime2, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))") iYear, iMonth, iDay, iHour, iMinute, iSecond
			print "(a,',',i8,', ',a)", sDateTime1, ivTimeCode(i), sDateTime2
		end do
		print *
		
		! Test 8: Linear index under normal conditions, new time stamp
		print *, "Test 8: Linear index, sub-hourly, new time stamp"
		iRetCode = timeLinearIndex(rvTimeStamp, 600, ivTimeCode, rvBaseTimeStamp)
		print *, "Return code: ", iRetCode, " (expected: 0)"
		print *
		print *, "Time.Stamp, Time.Index, Base.Time.Stamp"
		do i = 1, 48
			iRetCode = dt % fromEpoch(rvTimeStamp(i))
			sDateTime3 = dt % toIso()
			iRetCode = dt % fromEpoch(rvBaseTimeStamp(i))
			sDateTime4 = dt % toIso()
			print "(a,',',i8,', ',a)", sDateTime3, ivTimeCode(i), sDateTime4
		end do
		print *
		
		! Test 9: Linear index under normal conditions, legacy time stamp, one invalid
		print *, "Test 9: Linear index, hourly, legacy time stamp, one invalid"
		ivTimeStamp(46) = 0
		iRetCode = timeLinearIndex(ivTimeStamp, DELTA_1_HOUR, ivTimeCode, ivBaseTimeStamp)
		print *, "Return code: ", iRetCode, " (expected: 0)"
		print *
		print *, "Time.Stamp, Time.Index, Base.Time.Stamp"
		do i = 1, 48
			call UnpackTime(ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			write(sDateTime1, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))") iYear, iMonth, iDay, iHour, iMinute, iSecond
			call UnpackTime(ivBaseTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			write(sDateTime2, "(i4.4,2('-',i2.2),1x,i2.2,2(':',i2.2))") iYear, iMonth, iDay, iHour, iMinute, iSecond
			print "(a,',',i8,', ',a)", sDateTime1, ivTimeCode(i), sDateTime2
		end do
		print *
		
		! Test 10: Linear index under normal conditions, new time stamp, one invalid
		print *, "Test 10: Linear index, hourly, new time stamp, one invalid"
		rvTimeStamp(46) = NaN_8
		iRetCode = timeLinearIndex(rvTimeStamp, DELTA_1_HOUR, ivTimeCode, rvBaseTimeStamp)
		print *, "Return code: ", iRetCode, " (expected: 0)"
		print *
		print *, "Time.Stamp, Time.Index, Base.Time.Stamp"
		do i = 1, 48
			iRetCode = dt % fromEpoch(rvTimeStamp(i))
			sDateTime3 = dt % toIso()
			iRetCode = dt % fromEpoch(rvBaseTimeStamp(i))
			sDateTime4 = dt % toIso()
			print "(a,',',i8,', ',a)", sDateTime3, ivTimeCode(i), sDateTime4
		end do
		print *
		
		! Test 11: Linear index under boundary conditions, legacy time stamp, zero-length input
		print *, "Test 11: Linear index, hourly, legacy time stamp, zero-length input"
		deallocate(ivTimeStamp, rvTimeStamp)
		allocate(ivTimeStamp(0), rvTimeStamp(0))
		iRetCode = timeLinearIndex(ivTimeStamp, DELTA_1_HOUR, ivTimeCode)
		print *, "Return code: ", iRetCode, " (expected: non-zero)"
		print *
		
		! Test 12: Linear index under boundary conditions, new time stamp, zero-length input
		print *, "Test 11: Linear index, hourly, new time stamp, zero-length input"
		iRetCode = timeLinearIndex(rvTimeStamp, DELTA_1_HOUR, ivTimeCode)
		print *, "Return code: ", iRetCode, " (expected: non-zero)"
		print *
		
		! Leave
		deallocate(ivTimeStamp, rvTimeStamp)
		
	end subroutine tst_LinearIndex
	
end program test_pbl_time
