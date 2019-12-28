program ps3_test

	use pbl_met
	use ps3
	
	implicit none
	
	! Locals
	integer								:: iRetCode
	real, dimension(16384)				:: rvX
	integer, dimension(:), allocatable	:: ivPks
	integer								:: i
	
	! Generate a noisy signal, and overlap some peaks with different shapes in different positions
	! -1- Main signal
	call random_number(rvX)
	! -1- First peak: triangular, at 3500
	do i = -50, 50
		rvX(i+3500) = rvX(i+3500) + 50 - 50*abs(i)
	end do
	! -1- Second peak: Gaussian, at 7000
	do i = -50, 50
		rvX(i+7000) = rvX(i+7000) + 50*exp(-i**2/50.)
	end do
	! -1- Third peak: flat, at 10000
	do i = -50, 50
		rvX(i+10000) = rvX(i+10000) + 50.
	end do
	
	! Write test data to file for plotting
	open(10, file="test.csv", status="unknown", action="write")
	write(10, "('Idx,X')")
	do i = 1, size(rvX)
		write(10, "(1x,i6,',',e15.7)") i, rvX(i)
	end do
	close(10)
	
	! Add some false positives
	rvX(12000) = rvX(12000) + 1000.
	rvX(14000) = rvX(14000) +  500.
	
	! Attempt processing
	iRetCode = ampd(rvX, 1.0, ivPks)
	if(iRetCode /= 0) then
		print *, "Error: return code = ", iRetCode
		stop
	end if
	print *, "Peak indices:"
	print *, ivPks

end program ps3_test
