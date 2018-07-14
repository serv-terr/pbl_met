! Program for experimenting with peak detection algorithms
!
! This code is part of the pbl_met library.
!
! This is open-source code, covered by the lGPL 3.0 license.
!
program play_with_peak_detect

	use pbl_met

	implicit none
	
	! Locals
	integer								:: iRetCode
	integer								:: i
	real, dimension(1024)				:: rvX
	integer, dimension(:), allocatable	:: signals
	real, dimension(:), allocatable		:: avgFilter
	real, dimension(:), allocatable		:: stdFilter
	integer								:: iThreshold
	real								:: threshold
	character(len=256)					:: sBuffer
	character(len=256)					:: sFileName
	
	! Get signal
	open(10, file="peaky.csv", status="old", action="read")
	read(10, "(a)") sBuffer
	do i = 1, 1024
		read(10, *) rvX(i)
	end do
	close(10)
	
	! Locate spikes using increasing threshold
	open(10, file="peaky.out.csv", status="unknown", action="write")
	write(10,"('Threshold, Num.Negative.Peaks, Num.Positive.Peaks')")
	do iThreshold = 1, 500
		threshold = iThreshold / 10.
		iRetCode = FindPeaks_Simple(rvX, 10, threshold, 0.1, signals, avgFilter, stdFilter)
		write(10, "(f4.1, 2(',', i4))") threshold, count(signals < 0), count(signals > 0)
		if(mod(iThreshold, 100) == 0 .or. iThreshold==30) then
			write(sFileName, "('peaky.',i2.2,'.csv')") iThreshold/10
			open(11, file=sFileName, status='unknown', action='write')
			write(11, "('Index, Value, PeakType, Avg.Filter, Std.Filter')")
			do i = 1, 1024
				write(11,"(i4,',',f9.4,',',i2,2(',',f9.4))") i, rvX(i), signals(i), avgFilter(i), stdFilter(i)
			end do
			close(11)
		end if
	end do
	close(10)

	! Locate spikes using increasing threshold
	open(10, file="peaky.beta1.out.csv", status="unknown", action="write")
	write(10,"('Threshold, Num.Negative.Peaks, Num.Positive.Peaks')")
	do iThreshold = 1, 500
		threshold = iThreshold / 10.
		iRetCode = FindPeaks_Simple(rvX, 10, threshold, 1.0, signals, avgFilter, stdFilter)
		write(10, "(f4.1, 2(',', i4))") threshold, count(signals < 0), count(signals > 0)
		if(mod(iThreshold, 100) == 0 .or. iThreshold==30) then
			write(sFileName, "('peaky.beta1.',i2.2,'.csv')") iThreshold/10
			open(11, file=sFileName, status='unknown', action='write')
			write(11, "('Index, Value, PeakType, Avg.Filter, Std.Filter')")
			do i = 1, 1024
				write(11,"(i4,',',f9.4,',',i2,2(',',f9.4))") i, rvX(i), signals(i), avgFilter(i), stdFilter(i)
			end do
			close(11)
		end if
	end do
	close(10)

	! Locate spikes using increasing threshold
	open(10, file="peaky.beta0.out.csv", status="unknown", action="write")
	write(10,"('Threshold, Num.Negative.Peaks, Num.Positive.Peaks')")
	do iThreshold = 1, 500
		threshold = iThreshold / 10.
		iRetCode = FindPeaks_Simple(rvX, 10, threshold, 0.0, signals, avgFilter, stdFilter)
		write(10, "(f4.1, 2(',', i4))") threshold, count(signals < 0), count(signals > 0)
		if(mod(iThreshold, 100) == 0 .or. iThreshold==30) then
			write(sFileName, "('peaky.beta0.',i2.2,'.csv')") iThreshold/10
			open(11, file=sFileName, status='unknown', action='write')
			write(11, "('Index, Value, PeakType, Avg.Filter, Std.Filter')")
			do i = 1, 1024
				write(11,"(i4,',',f9.4,',',i2,2(',',f9.4))") i, rvX(i), signals(i), avgFilter(i), stdFilter(i)
			end do
			close(11)
		end if
	end do
	close(10)

contains

	! Find peaks in a data vector, assuming a normal distribution. This program is the Fortran
	! translation of "Smoothed z-score algo (very robust threshold algorithm)"; see
	!
	!    https://stackoverflow.com/questions/22583391/peak-signal-detection-in-realtime-timeseries-data
	!
	function FindPeaks_Simple( &
		rvX, &
		lag, &
		threshold, &
		beta, &
		signals, &
		avgFilter, &
		stdFilter &
	) result(iRetCode)
	
		! Routine arguments
		real, dimension(:), intent(in)					:: rvX			! Input signal
		integer, intent(in)								:: lag			! Depth (less than size of input signal)
		real, intent(in)								:: threshold	! Number of standard deviations about the smoothed signal for peak detection
		real, intent(in)								:: beta			! AR(1) filter parameter (0 < beta < 1; value decreases with increasing smoothing)
		integer, dimension(:), allocatable, intent(out)	:: signals		! 1 if positive peak; -1 if negative peak; 0 if non-peak
		real,  dimension(:), allocatable, intent(out)	:: avgFilter
		real,  dimension(:), allocatable, intent(out)	:: stdFilter
		integer											:: iRetCode
		
		! Locals
		integer	:: n
		integer	:: i
		real	:: rAvg
		real	:: rStd
		real	:: rSumX
		real	:: rSumX2
		real, dimension(:), allocatable	:: filteredY
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check something can be made
		n = size(rvX)
		if(n < 2) then
			iRetCode = 1
			return
		end if
		
		! Reserve workspace
		if(allocated(signals)) deallocate(signals)
		if(allocated(avgFilter)) deallocate(avgFilter)
		if(allocated(stdFilter)) deallocate(stdFilter)
		allocate(signals(n))
		allocate(avgFilter(n))
		allocate(stdFilter(n))
		allocate(filteredY(n))
		
		! Initialize data
		signals = 0
		filteredY(1:lag+1) = rvX(1:lag+1)
		avgFilter(1:lag) = 0.	! Not used, really
		stdFilter(1:lag) = 0.	! Not used, really
		
		! Compute mean and standard deviation of signal beginning
		rSumX = sum(rvX(1:lag+1))
		rSumX2 = sum(rvX(1:lag+1)**2)
		rAvg = rSumX / (lag+1)
		rStd = sqrt(rSumX2/(lag+1) - rAvg**2)
		avgFilter(lag+1) = rAvg
		stdFilter(lag+1) = rStd
		
		! Main loop: process all remaining time
		do i = lag+2, n
		
			! Locate peak
			if(abs(rvX(i)-avgFilter(i-1)) > threshold*stdFilter(i-1)) then
				if(rvX(i) > avgFilter(i-1)) then
					signals(i) = 1
				else
					signals(i) = -1
				end if
				filteredY(i) = beta*rvX(i)+(1.-beta)*filteredY(i-1)
			else
				signals(i) = 0
				filteredY(i) = rvX(i)
			end if
			
			! Update comparison values
			rSumX = sum(filteredY(i-lag:i))
			rSumX2 = sum(filteredY(i-lag:i)**2)
			rAvg = rSumX / (lag+1)
			rStd = sqrt(rSumX2/(lag+1) - rAvg**2)
			avgFilter(i) = rAvg
			stdFilter(i) = rStd
			
		end do
		
	end function FindPeaks_Simple
	
end program play_with_peak_detect

	
