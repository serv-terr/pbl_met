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
	
	! Test 1: Nominal case
	iRetCode = Synthetize(24)
	iRetCode = EstimateZi(rvTimeStamp, 0, 0., 0., 3600, rvTemp, rvUstar, rvH0, rvN, rvZi)
	open(10, file="Zi_Test1_Variant.csv", status="unknown", action="write")
	write(10, "('Date.Time, Temp, U.star, H0, N, Zi')")
	do i = 1, size(rvTimeStamp)
		iRetCode = tStamp % fromEpoch(rvTimeStamp(i))
		sISOdate = tStamp % toISO()
		write(10, "(a,5(',',f8.4))") sISOdate, rvTemp(i), rvUstar(i), rvH0(i), rvN(i), rvZi(i)
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
