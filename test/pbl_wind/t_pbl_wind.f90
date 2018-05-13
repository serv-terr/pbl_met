! t_pbl_wind - Test procedure for pbl_wind sub-library
!
! This module is part of the pbl_met library.
!
! Copyright 2018 by Servizi Territorio srl
!
! This is open-source code, covered by the lGPL 3.0 license.
!
program t_pbl_wind

	use pbl_met
	
	implicit none
	
	! Locals
	
	! Constants
	real, parameter	:: pi = 3.1415927
	
	! Perform actual tests
	call polar_cartesian()
	
contains

	subroutine polar_cartesian()
	
		! Routine arguments
		! --none--
		
		! Locals
		real, dimension(2)	:: rvCartesian2
		real, dimension(3)	:: rvCartesian3
		real, dimension(2)	:: rvPolar2
		real, dimension(3)	:: rvPolar3
		integer				:: i
		real				:: dir
		real				:: e, n
		
		print *, "Test 1 - Check Cartesian to Polar conversions - Same convention, speed 1"
		print *, "Expected.Vel, Expected.Dir, Result.Vel, Result.Dir"
		do i = 0, 7
			dir = 360./8. * i
			e   = sin(dir*pi/180.)
			n   = cos(dir*pi/180.)
			rvCartesian2 = [e, n]
			rvPolar2 = CartesianToPolar2(rvCartesian2, WCONV_SAME)
			print "('  1.0, ',f4.0,', ',f6.3,', ',f8.3)", &
				dir, rvPolar2
		end do
		print *
		
		print *, "Test 2 - Check Flow Cartesian to Provenance Polar conversions - Speed 1"
		print *, "Expected.Vel, Expected.Dir, Result.Vel, Result.Dir"
		do i = 0, 7
			dir = 360./8. * i
			e   = sin(dir*pi/180.)
			n   = cos(dir*pi/180.)
			rvCartesian2 = [e, n]
			rvPolar2 = CartesianToPolar2(rvCartesian2, WCONV_FLOW_TO_PROVENANCE)
			dir = dir + 180.0
			if(dir >= 360.0) dir = dir - 360.0
			print "('  1.0, ',f4.0,', ',f6.3,', ',f8.3)", &
				dir, rvPolar2
		end do
		print *
		
		print *, "Test 3 - Check Provenance Cartesian to Flow Polar conversions - Speed 1"
		print *, "Expected.Vel, Expected.Dir, Result.Vel, Result.Dir"
		do i = 0, 7
			dir = 360./8. * i
			e   = sin(dir*pi/180.)
			n   = cos(dir*pi/180.)
			rvCartesian2 = [e, n]
			rvPolar2 = CartesianToPolar2(rvCartesian2, WCONV_PROVENANCE_TO_FLOW)
			dir = dir + 180.0
			if(dir >= 360.0) dir = dir - 360.0
			print "('  1.0, ',f4.0,', ',f6.3,', ',f8.3)", &
				dir, rvPolar2
		end do
		print *
		
		print *, "Test 4 - Check zero wind - Speed 0"
		print *, "Expected.Vel, Expected.Dir, Result.Vel, Result.Dir"
		e   = 0.
		n   = 0.
		rvCartesian2 = [e, n]
		rvPolar2 = CartesianToPolar2(rvCartesian2, WCONV_PROVENANCE_TO_FLOW)
		print "('  0.0, NaN, ',f6.3,', ',f8.3)", &
			rvPolar2
		print *
		
		print *, "Test 5 - Check 3D Cartesian to Polar conversions - Same convention, speed 1"
		print *, "Expected.Vel, Expected.Dir, Result.Vel, Result.Dir"
		do i = 0, 7
			dir = 360./8. * i
			e   = sin(dir*pi/180.)
			n   = cos(dir*pi/180.)
			rvCartesian3 = [e, n, 1.]
			rvPolar3 = CartesianToPolar3(rvCartesian3, WCONV_SAME)
			print "('  1.0, ',f4.0,',1.0, ',f6.3,', ',f8.3,', ',f8.3)", &
				dir, rvPolar3
		end do
		print *
		
		print *, "Test 6 - Check 3D Flow Cartesian to Provenance Polar conversions - Speed 1"
		print *, "Expected.Vel, Expected.Dir, Z, Result.Vel, Result.Dir"
		do i = 0, 7
			dir = 360./8. * i
			e   = sin(dir*pi/180.)
			n   = cos(dir*pi/180.)
			rvCartesian3 = [e, n, 1.]
			rvPolar3 = CartesianToPolar3(rvCartesian3, WCONV_FLOW_TO_PROVENANCE)
			dir = dir + 180.0
			if(dir >= 360.0) dir = dir - 360.0
			print "('  1.0, ',f4.0,', 1.0, ',f6.3,', ',f8.3,', ',f8.3)", &
				dir, rvPolar3
		end do
		print *
		
		print *, "Test 7 - Check Provenance 3D Cartesian to Flow Polar conversions - Speed 1"
		print *, "Expected.Vel, Expected.Dir, Z, Result.Vel, Result.Dir"
		do i = 0, 7
			dir = 360./8. * i
			e   = sin(dir*pi/180.)
			n   = cos(dir*pi/180.)
			rvCartesian3 = [e, n, 1.0]
			rvPolar3 = CartesianToPolar3(rvCartesian3, WCONV_PROVENANCE_TO_FLOW)
			dir = dir + 180.0
			if(dir >= 360.0) dir = dir - 360.0
			print "('  1.0, ',f4.0,', 1.0, ',f6.3,', ',f8.3,', ',f8.3)", &
				dir, rvPolar3
		end do
		print *
		
		print *, "Test 8 - Check 3D zero wind - Speed 0"
		print *, "Expected.Vel, Expected.Dir, Result.Vel, Result.Dir"
		e   = 0.
		n   = 0.
		rvCartesian3 = [e, n, 1.0]
		rvPolar3 = CartesianToPolar3(rvCartesian3, WCONV_PROVENANCE_TO_FLOW)
		print "('  0.0, NaN, 1.0, ',f6.3,', ',f8.3,', ',f8.3)", &
			rvPolar3
		print *
		
		print *, "Test 9 - Check Polar to Cartesian conversions - Same convention, speed 1"
		print *, "Vel, Dir, Result.u, Result.v"
		do i = 0, 7
			dir = 360./8. * i
			rvPolar2 = [1.0, dir]
			rvCartesian2 = PolarToCartesian2(rvPolar2, WCONV_SAME)
			print "('  1.0, ',f4.0,', ',f6.3,', ',f6.3)", &
				dir, rvCartesian2
		end do
		print *
		
		print *, "Test 10 - Check Polar to Cartesian conversions - Flow to provenance, speed 1"
		print *, "Vel, Dir, Result.u, Result.v"
		do i = 0, 7
			dir = 360./8. * i
			rvPolar2 = [1.0, dir]
			rvCartesian2 = PolarToCartesian2(rvPolar2, WCONV_FLOW_TO_PROVENANCE)
			print "('  1.0, ',f4.0,', ',f6.3,', ',f6.3)", &
				dir, rvCartesian2
		end do
		print *
		
		print *, "Test 11 - Check Polar to Cartesian conversions - Provenance to flow, speed 1"
		print *, "Vel, Dir, Result.u, Result.v"
		do i = 0, 7
			dir = 360./8. * i
			rvPolar2 = [1.0, dir]
			rvCartesian2 = PolarToCartesian2(rvPolar2, WCONV_PROVENANCE_TO_FLOW)
			print "('  1.0, ',f4.0,', ',f6.3,', ',f6.3)", &
				dir, rvCartesian2
		end do
		print *
		
	end subroutine polar_cartesian

end program t_pbl_wind
