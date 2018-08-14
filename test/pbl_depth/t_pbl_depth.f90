! t_pbl_deptb - Test procedure for pbl_depth sub-library
!
! This module is part of the pbl_met library.
!
! Copyright 2018 by Servizi Territorio srl
!
! This is open-source code, covered by the lGPL 3.0 license.
!
program t_pbl_depth

	use pbl_met
	
	implicit none
	
	! Locals
	real(8), dimension(:), allocatable	:: rvTimeStamp
	real(8), dimension(:), allocatable	:: rvTemp
	real(8), dimension(:), allocatable	:: rvUstar
	real(8), dimension(:), allocatable	:: rvH0
	real(8), dimension(:), allocatable	:: rvN
	real(8), dimension(:), allocatable	:: rvZi
	character(len=23)					:: sISOdate
	type(DateTime)						:: tStamp
	integer								:: iRetCode
	integer								:: i
	integer								:: j
	
	! Test 1: Nominal case
	iRetCode = Synthetize(24)
	iRetCode = EstimateZi(rvTimeStamp, 0, 0., 0., 3600, rvTemp, rvUstar, rvH0, rvN, 60, rvZi)
	open(10, file="Zi_Test1_Variant.csv", status="unknown", action="write")
	write(10, "('Date.Time, Temp, U.star, H0, N, Zi')")
	do i = 1, size(rvTimeStamp)
		iRetCode = tStamp % fromEpoch(rvTimeStamp(i))
		sISOdate = tStamp % toISO()
		write(10, "(a,5(',',f8.4))") sISOdate, rvTemp(i), rvUstar(i), rvH0(i), rvN(i), rvZi(i)
	end do
	close(10)

	! Test 2: Effect of number of substeps
	iRetCode = Synthetize(24)
	open(10, file="Zi_Test2.csv", status="unknown", action="write")
	write(10, "('N.steps, Zi(00), Zi(14), Zi(17), Zi(23)')")
	do j = 1, 101, 10
		iRetCode = EstimateZi(rvTimeStamp, 0, 0., 0., 3600, rvTemp, rvUstar, rvH0, rvN, j, rvZi)
		write(10, "(i3,4(',',f8.4))") j, rvZi(1), rvZi(15), rvZi(18), rvZi(24)
	end do
	close(10)
	
	! Test 3: Effect of temperature shift
	open(10, file="Zi_Test3.csv", status="unknown", action="write")
	write(10, "('Temp, Zi(00), Zi(14), Zi(17), Zi(23)')")
	do j = -40, 60, 10
		iRetCode = Synthetize(24)
		rvTemp = rvTemp + j
		iRetCode = EstimateZi(rvTimeStamp, 0, 0., 0., 3600, rvTemp, rvUstar, rvH0, rvN, 60, rvZi)
		write(10, "(i3,4(',',f9.4))") j, rvZi(1), rvZi(15), rvZi(18), rvZi(24)
	end do
	close(10)
	
	! Test 4: Effect of temperature scaling
	open(10, file="Zi_Test4.csv", status="unknown", action="write")
	write(10, "('Temp.Factor, Zi(00), Zi(14), Zi(17), Zi(23)')")
	do j = 0, 20, 1
		iRetCode = Synthetize(24)
		rvTemp = rvTemp * j/10.d0
		iRetCode = EstimateZi(rvTimeStamp, 0, 0., 0., 3600, rvTemp, rvUstar, rvH0, rvN, 60, rvZi)
		write(10, "(f3.1,4(',',f9.4))") j/10.d0, rvZi(1), rvZi(15), rvZi(18), rvZi(24)
	end do
	close(10)
	
	! Test 5: Effect of friction velocity shift
	open(10, file="Zi_Test5.csv", status="unknown", action="write")
	write(10, "('U.star.shift, Zi(00), Zi(14), Zi(17), Zi(23)')")
	do j = 0, 100, 5
		iRetCode = Synthetize(24)
		rvUstar = rvUstar + j / 100.d0
		iRetCode = EstimateZi(rvTimeStamp, 0, 0., 0., 3600, rvTemp, rvUstar, rvH0, rvN, 60, rvZi)
		write(10, "(f4.2,4(',',f9.4))") j / 100.d0, rvZi(1), rvZi(15), rvZi(18), rvZi(24)
	end do
	close(10)
	
	! Test 6: Effect of friction velocity scaling
	open(10, file="Zi_Test6.csv", status="unknown", action="write")
	write(10, "('U.star.Factor, Zi(00), Zi(14), Zi(17), Zi(23)')")
	do j = 0, 20, 1
		iRetCode = Synthetize(24)
		rvUstar = rvUstar * j/10.d0
		iRetCode = EstimateZi(rvTimeStamp, 0, 0., 0., 3600, rvTemp, rvUstar, rvH0, rvN, 60, rvZi)
		write(10, "(f3.1,4(',',f9.4))") j/10.d0, rvZi(1), rvZi(15), rvZi(18), rvZi(24)
	end do
	close(10)
	
	! Test 7: Effect of sensible heat flux shift
	open(10, file="Zi_Test7.csv", status="unknown", action="write")
	write(10, "('H0.shift, Zi(00), Zi(14), Zi(17), Zi(23)')")
	do j = -100, 100, 10
		iRetCode = Synthetize(24)
		rvH0 = rvH0 + j
		iRetCode = EstimateZi(rvTimeStamp, 0, 0., 0., 3600, rvTemp, rvUstar, rvH0, rvN, 60, rvZi)
		write(10, "(f6.1,4(',',f9.4))") dble(j), rvZi(1), rvZi(15), rvZi(18), rvZi(24)
	end do
	close(10)
	
contains

	function Synthetize(n) result(iRetCode)
	
		! Routine arguments
		integer, intent(in)	:: n
		integer				:: iRetCode
		
		! Locals
		integer								:: iErrCode
		type(DateTime)						:: tStamp
		real(8)								:: rBaseTime
		real(8)								:: rTimeDelta
		integer								:: i
		
		! Locals
		real(8), parameter	:: PI = atan(1.d0)*4.d0
		
		! Assume success
		iRetCode = 0
		
		! Check inputs
		if(n < 1) then
			iRetCode = 1
			return
		end if
		
		! Reserve workspace
		if(allocated(rvTimeStamp)) deallocate(rvTimeStamp)
		if(allocated(rvTemp))      deallocate(rvTemp)
		if(allocated(rvUstar))     deallocate(rvUstar)
		if(allocated(rvH0))        deallocate(rvH0)
		if(allocated(rvN))         deallocate(rvN)
		if(allocated(rvZi))        deallocate(rvZi)
		allocate(rvTimeStamp(n))
		allocate(rvTemp(n))
		allocate(rvUstar(n))
		allocate(rvH0(n))
		allocate(rvN(n))
		allocate(rvZi(n))
		
		! Build time stamp
		tStamp = DateTime(2018, 3, 21, 0, 0, 0.0d0)
		rBaseTime = tStamp % toEpoch()
		rTimeDelta  = 24.d0*3600.0d0 / n
		rvTimeStamp = [(rBaseTime + (i-1)*rTimeDelta, i = 1, n)]
		
		! Simulate daily temperature
		rvTemp = [(20.d0 - 10.d0*cos(2.d0*PI*(i-1)/dble(n)), i = 1, n)]
		
		! Set 'ustar' to constant, compatibly with constant wind
		rvUstar = 0.2
		
		! Simulate daily sensible surface heat flux
		rvH0 = [(-60.d0*cos(2.d0*PI*(i-1)/dble(n)), i = 1, n)]
		where(rvH0 < -10.d0)
			rvH0 = -10.d0
		end where
		
		! Assume constant Brunt-Vaisala frequency
		rvN = 0.011
		
	end function Synthetize
	
end program t_pbl_depth
