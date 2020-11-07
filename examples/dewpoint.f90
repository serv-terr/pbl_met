! Simple example program showing how to compute the dew-point, given "regular"
! (dry bulb) temperature and relative humidity - just the stuff you may hope
! to get from a thermo-hygrometer.
!
! Authors: Daniele Fraternali, Patrizia Favaron
!

program DewPoint
	
	use pbl_met
	
	implicit none
	
	! Locals
	integer				:: iRetCode
	character(len=16)	:: sBuffer
	real				:: temp		! Temperature, in °C
	real				:: relh		! Relative humidity, in %
	
	! Get temperature and relative humidity from the command line (or print a helpful
	! message if something looks wrong
	if(command_argument_count() /= 2) then
		call helpfulMessage()
		stop
	end if
	call get_command_argument(1, sBuffer)
	read(sBuffer, *, iostat=iRetCode) temp
	if(iRetCode /= 0) then
		print *, "Error: invalid temperature!"
		print *
		call helpfulMessage()
		stop
	end if
	call get_command_argument(2, sBuffer)
	read(sBuffer, *, iostat=iRetCode) relh
	if(iRetCode /= 0) then
		print *, "Error: invalid relative humidity!"
		print *
		call helpfulMessage()
		stop
	end if
	
	! Perform the calculation using pbl_met, and print it on the fly
	print *, "Dew point temperature = ", DewPointTemperature(temp + 273.15, relh) - 273.15, " °C"

contains

	subroutine helpfulMessage()
	
		! Routine arguments
		! --none--
		
		! Locals
		! --none--
		
		! Print some helpful info
		print *, "dewpoint - Simple dew point calculator given temperature in °C"
		print *, "           and relative humidity in %"
		print *, ""
		print *, "Usage:"
		print *, ""
		print *, "  ./dewpoint <temperature> <relative_humidity>"
		print *, ""
		print *, "The program prints the answer, in °C."
		print *, ""
		print *, "This is a simple example of _pbl_met_ use."
		print *, ""
		
	end subroutine helpfulMessage

end program DewPoint
