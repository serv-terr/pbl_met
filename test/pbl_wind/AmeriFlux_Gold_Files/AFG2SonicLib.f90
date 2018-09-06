! AFG2SonicLib - Program converting from "RAW" CSV Ameriflux form to
!                SonicLib form, for eddy covariance program testing.
!
! This module is part of the pbl_met library.
!
! Copyright 2018 by Servizi Territorio srl
!
! This is open-source code, covered by lGPL 3.0 license.
!
! Author: Mauri Favaron
!
program AFG2SonicLib

	implicit none

	! Locals
	integer				:: iRetCode
	integer				:: iDayNumber
	integer				:: iDay
	integer				:: iHour
	integer				:: iHalfHour
	character(len=256)	:: sSourceFile
	character(len=256)	:: sDestinationFile
	real				:: u, v, w, t, h2o, co2
	integer				:: iDataNum
	integer				:: iTimeStamp
	
	! Constants
	integer, dimension(2), parameter	:: ivDayNo = [104, 181]
	
	! Get parameters
	if(command_argument_count() /= 0) then
		print *, "ToUsaSim - Program generating binary file for UsaSim"
		print *
		print *, "Usage:"
		print *
		print *, "  ./ToSonicLib"
		print *
		print *, "Copyright 2018 by Servizi Territorio srl"
		print *
		print *, "This is open-source code, covered by lGPL 3.0 license"
		print *
		print *, "Written by: Mauri Favaron"
		print *, ""
		stop
	end if

	! Main loop: iterate over days and hours in day
	do iDay = 1, size(ivDayNo)
		do iHour = 0, 23
			iDayNumber = ivDayNo(iDay)
	
			! Identify output file and create it
			if(iDayNumber == 104) then
				write(sDestinationFile, "('20150414.',i2.2,'R.csv')") iHour
			else
				write(sDestinationFile, "('20150630.',i2.2,'R.csv')") iHour
			end if
			open(11, file=sDestinationFile, status='unknown', action='write')
			write(11,"('time.stamp, u, v, w, t, q, c')")
		
			! Loop over the two halves of an hour
			do iHalfHour = 0, 30, 30
		
				! Get input file, convert it to MFC V2 form and write in encoded form
				write(sSourceFile, "('G',i3.3,i2.2,i2.2,'.RAW')") iDayNumber, iHour, iHalfHour
				open(10, file=sSourceFile, status='old', action='read')
				iDataNum = 0
				do
					read(10, *, iostat=iRetCode) w, u, v, t, h2o, co2
					if(iRetCode /= 0) exit
					iTimeStamp = iDataNum / 10
					write(11,"(i4,4(',',f6.2),2(',',f7.3))") iTimeStamp, u, v, w, t, h2o, co2
					iDataNum = iDataNum + 1
				end do
				close(10)
			
			end do
		
			close(11)
		
		end do
	end do
	
end program AFG2SonicLib
