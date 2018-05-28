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
	call tst_classwindscalar()
	call tst_classwindVector()
	call tst_classdirscalar()
	call tst_classdirvector()
	call tst_windVectorScalar()
	
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
		
		print *, "Test 12 - Check Polar to Cartesian conversions - Speed 0"
		print *, "Vel, Dir, Expected.u, Expected.v, Result.u, Result.v"
		dir = 360./8.
		rvPolar2 = [0.0, dir]
		rvCartesian2 = PolarToCartesian2(rvPolar2, WCONV_PROVENANCE_TO_FLOW)
		print "('  0.0, ',f4.0,', NaN, NaN, ',f6.3,', ',f6.3)", &
			dir, rvCartesian2
		print *
		
		print *, "Test 13 - Check 3D Polar to Cartesian conversions - Same convention, speed 1"
		print *, "Vel, Dir, w, Result.u, Result.v, Result.w"
		do i = 0, 7
			dir = 360./8. * i
			rvPolar3 = [1.0, dir, 1.0]
			rvCartesian3 = PolarToCartesian3(rvPolar3, WCONV_SAME)
			print "('  1.0, ',f4.0,', 1.0, ',f6.3,', ',f6.3,', ',f6.3)", &
				dir, rvCartesian3
		end do
		print *
		
		print *, "Test 14 - Check 3D Polar to Cartesian conversions - Flow to provenance, speed 1"
		print *, "Vel, Dir, w, Result.u, Result.v, Result.w"
		do i = 0, 7
			dir = 360./8. * i
			rvPolar3 = [1.0, dir, 1.0]
			rvCartesian3 = PolarToCartesian3(rvPolar3, WCONV_FLOW_TO_PROVENANCE)
			print "('  1.0, ',f4.0,', 1.0, ',f6.3,', ',f6.3,', ',f6.3)", &
				dir, rvCartesian3
		end do
		print *
		
		print *, "Test 15 - Check 3D Polar to Cartesian conversions - Provenance to flow, speed 1"
		print *, "Vel, Dir, w, Result.u, Result.v"
		do i = 0, 7
			dir = 360./8. * i
			rvPolar3 = [1.0, dir, 1.0]
			rvCartesian3 = PolarToCartesian3(rvPolar3, WCONV_PROVENANCE_TO_FLOW)
			print "('  1.0, ',f4.0,', 1.0, ',f6.3,', ',f6.3,', ',f6.3)", &
				dir, rvCartesian3
		end do
		print *
		
		print *, "Test 16 - Check 3D Polar to Cartesian conversions - Speed 0"
		print *, "Vel, Dir, w, Expected.u, Expected.v, Result.u, Result.v, Result.w"
		dir = 360./8.
		rvPolar3 = [0.0, dir, 1.0]
		rvCartesian3 = PolarToCartesian3(rvPolar3, WCONV_PROVENANCE_TO_FLOW)
		print "('  0.0, ',f4.0,', NaN, NaN, 1.0, ',f6.3,', ',f6.3,', ',f6.3)", &
			dir, rvCartesian3
		print *
		
	end subroutine polar_cartesian
	
	
	subroutine tst_classwindscalar()
	
		! Locals
		real, dimension(:), allocatable		:: rvVel
		real, dimension(:), allocatable		:: rvVelClass
		integer								:: i
		integer								:: iClass
		
		! Generate normal test set
		allocate(rvVel(32))
		rvVel = [(i/2., i=1, size(rvVel))]
		
		! Test 1, normal condition
		print *, "Test 1 - Check ClassVelScalar under normal conditions"
		print *, 'Vel, Class'
		do i = 1, size(rvVel)
			iClass = ClassVel(rvVel(i), [1.,2.,3.,5.,7.])
			print *, rvVel(i), iClass
		end do
		print *
		
		! Test 2, normal condition, scrambled class limits
		! (as expected, empty classes may result: it is then better,
		! although not mandatory, that class limits are sorted in
		! ascending order)
		print *, "Test 2 - Check ClassVelScalar under scrambled class limits"
		print *, 'Vel, Class'
		do i = 1, size(rvVel)
			iClass = ClassVel(rvVel(i), [1.,2.,7.,5.,3.])
			print *, rvVel(i), iClass
		end do
		print *
		print *, 'Vel, Class'
		do i = 1, size(rvVel)
			iClass = ClassVel(rvVel(i), [1.,2.,5.,4.,7.])
			print *, rvVel(i), iClass
		end do
		print *
		
		! Test 3, boundary condition
		! (As expected, one class is not represented)
		print *, "Test 3 - Check ClassVelScalar under one invalid class limit"
		print *, 'Vel, Class'
		do i = 1, size(rvVel)
			iClass = ClassVel(rvVel(i), [1.,2.,NaN,5.,7.])
			print *, rvVel(i), iClass
		end do
		print *
		
		! Test 4, boundary condition
		! (As expected, one class is not represented)
		print *, "Test 4 - Check ClassVelScalar under all invalid class limits"
		print *, 'Vel, Class'
		do i = 1, size(rvVel)
			iClass = ClassVel(rvVel(i), [NaN,NaN,NaN,NaN,NaN])
			print *, rvVel(i), iClass
		end do
		print *
		
		! Test 5, boundary condition
		! (As expected, one class is not represented)
		print *, "Test 5 - Check ClassVelScalar under empty limits vector"
		allocate(rvVelClass(0))
		print *, 'Vel, Class'
		do i = 1, size(rvVel)
			iClass = ClassVel(rvVel(i), rvVelClass)
			print *, rvVel(i), iClass
		end do
		print *
		
		! Leave
		deallocate(rvVelClass)
		deallocate(rvVel)
		
	end subroutine tst_classwindscalar
	
	
	subroutine tst_classwindVector()
	
		! Locals
		real, dimension(:), allocatable		:: rvVel
		real, dimension(:), allocatable		:: rvVelClass
		integer								:: i
		integer, dimension(:), allocatable	:: ivClass
		
		! Generate normal test set
		allocate(rvVel(32))
		rvVel = [(i/2., i=1, size(rvVel))]
		
		! Test 1, normal condition
		print *, "Test 1 - Check ClassVelVector under normal conditions"
		print *, 'Vel, Class'
		ivClass = ClassVel(rvVel, [1.,2.,3.,5.,7.])
		do i = 1, size(rvVel)
			print *, rvVel(i), ivClass(i)
		end do
		print *
		
		! Test 2, normal condition, scrambled class limits
		! (as expected, empty classes may result: it is then better,
		! although not mandatory, that class limits are sorted in
		! ascending order)
		print *, "Test 2 - Check ClassVelVector under scrambled class limits"
		print *, 'Vel, Class'
		ivClass = ClassVel(rvVel, [1.,2.,7.,5.,3.])
		do i = 1, size(rvVel)
			print *, rvVel(i), ivClass(i)
		end do
		print *
		print *, 'Vel, Class'
		ivClass = ClassVel(rvVel, [1.,2.,5.,4.,7.])
		do i = 1, size(rvVel)
			print *, rvVel(i), ivClass(i)
		end do
		print *
		
		! Test 3, boundary condition
		! (As expected, one class is not represented)
		print *, "Test 3 - Check ClassVelVector under one invalid class limit"
		print *, 'Vel, Class'
		ivClass = ClassVel(rvVel, [1.,2.,NaN,5.,7.])
		do i = 1, size(rvVel)
			print *, rvVel(i), ivClass(i)
		end do
		print *
		
		! Test 4, boundary condition
		! (As expected, no class is represented)
		print *, "Test 4 - Check ClassVelVector under all invalid class limits"
		print *, 'Vel, Class'
		ivClass = ClassVel(rvVel, [NaN,NaN,NaN,NaN,NaN])
		do i = 1, size(rvVel)
			print *, rvVel(i), ivClass(i)
		end do
		print *
		
		! Test 5, boundary condition
		! (As expected, no class is represented)
		print *, "Test 5 - Check ClassVelVector under empty limits vector"
		allocate(rvVelClass(0))
		ivClass = ClassVel(rvVel, rvVelClass)
		print *, 'Vel, Class'
		do i = 1, size(rvVel)
			print *, rvVel(i), ivClass(i)
		end do
		print *
		
		! Test 6, boundary condition
		! (As expected, no class is represented)
		print *, "Test 6 - Check ClassVelVector under empty data vector"
		ivClass = ClassVel(rvVelClass, [1.,2.,3.,5.,7.])
		print *, 'Vel, Class - None expected'
		do i = 1, size(rvVelClass)
			print *, rvVelClass(i), ivClass(i)
		end do
		print *
		
		! Leave
		deallocate(rvVelClass)
		
	end subroutine tst_classwindVector

	
	subroutine tst_classdirscalar()
	
		! Locals
		real, dimension(:), allocatable		:: rvDir
		integer, dimension(:), allocatable	:: ivExpectedClass
		integer								:: i
		integer								:: iClass
		
		! Generate normal test set
		allocate(rvDir(4), ivExpectedClass(4))
		rvDir = [359., 89., 179., 269.]
		
		! Test 1, normal condition
		print *, "Test 1 - Check ClassDirScalar under normal conditions - Centered sectors"
		ivExpectedClass = [1, 5, 9, 13]
		print *, 'Dir, Class, Expected.Class'
		do i = 1, size(rvDir)
			iClass = ClassDir(rvDir(i), 16, WDCLASS_ZERO_CENTERED)
			print *, rvDir(i), iClass, ivExpectedClass(i)
		end do
		print *
				
		! Test 2, normal condition
		print *, "Test 2 - Check ClassDirScalar under normal conditions - Zero-based sectors"
		ivExpectedClass = [16, 4, 8, 12]
		print *, 'Dir, Class, Expected.Class'
		do i = 1, size(rvDir)
			iClass = ClassDir(rvDir(i), 16, WDCLASS_ZERO_BASED)
			print *, rvDir(i), iClass, ivExpectedClass(i)
		end do
		print *
				
		! Test 3, boundary condition
		print *, "Test 3 - Check ClassDirScalar under boundary conditions - Centered sectors - Dirs > [0,359.9999]"
		rvDir = [719., 360.+89., 360.+179., 360.+269.]
		ivExpectedClass = [1, 5, 9, 13]
		print *, 'Dir, Class, Expected.Class'
		do i = 1, size(rvDir)
			iClass = ClassDir(rvDir(i), 16, WDCLASS_ZERO_CENTERED)
			print *, rvDir(i), iClass, ivExpectedClass(i)
		end do
		print *
		
		! Test 4, boundary condition
		print *, "Test 4 - Check ClassDirScalar under boundary conditions - Centered sectors - Dirs < [0,359.9999]"
		rvDir = [-1., -360.+89., -360.+179., -360.+269.]
		ivExpectedClass = [1, 5, 9, 13]
		print *, 'Dir, Class, Expected.Class'
		do i = 1, size(rvDir)
			iClass = ClassDir(rvDir(i), 16, WDCLASS_ZERO_CENTERED)
			print *, rvDir(i), iClass, ivExpectedClass(i)
		end do
		print *
		
		! Test 5, boundary condition
		print *, "Test 5 - Check ClassDirScalar under one NaN direction"
		rvDir = [359., NaN, 179., 269.]
		ivExpectedClass = [1, -9999, 9, 13]
		print *, 'Dir, Class, Expected.Class'
		do i = 1, size(rvDir)
			iClass = ClassDir(rvDir(i), 16)
			print *, rvDir(i), iClass, ivExpectedClass(i)
		end do
		print *
				
		! Leave
		deallocate(ivExpectedClass)
		deallocate(rvDir)
		
	end subroutine tst_classdirscalar
	
	
	subroutine tst_classdirvector()
	
		! Locals
		real, dimension(:), allocatable		:: rvDir
		integer, dimension(:), allocatable	:: ivExpectedClass
		integer								:: i
		integer, dimension(:), allocatable	:: ivClass
		
		! Generate normal test set
		allocate(rvDir(4), ivExpectedClass(4))
		rvDir = [359., 89., 179., 269.]
		
		! Test 1, normal condition
		print *, "Test 1 - Check ClassDirVector under normal conditions - Centered sectors"
		ivExpectedClass = [1, 5, 9, 13]
		print *, 'Dir, Class, Expected.Class'
		ivClass = ClassDir(rvDir, 16, WDCLASS_ZERO_CENTERED)
		do i = 1, size(rvDir)
			print *, rvDir(i), ivClass(i), ivExpectedClass(i)
		end do
		print *
				
		! Test 2, normal condition
		print *, "Test 2 - Check ClassDirVector under normal conditions - Zero-based sectors"
		ivExpectedClass = [16, 4, 8, 12]
		print *, 'Dir, Class, Expected.Class'
		ivClass = ClassDir(rvDir, 16, WDCLASS_ZERO_BASED)
		do i = 1, size(rvDir)
			print *, rvDir(i), ivClass(i), ivExpectedClass(i)
		end do
		print *
				
		! Test 3, boundary condition
		print *, "Test 3 - Check ClassDirVector under boundary conditions - Centered sectors - Dirs > [0,359.9999]"
		rvDir = [719., 360.+89., 360.+179., 360.+269.]
		ivExpectedClass = [1, 5, 9, 13]
		print *, 'Dir, Class, Expected.Class'
		ivClass = ClassDir(rvDir, 16, WDCLASS_ZERO_CENTERED)
		do i = 1, size(rvDir)
			print *, rvDir(i), ivClass(i), ivExpectedClass(i)
		end do
		print *
		
		! Test 4, boundary condition
		print *, "Test 4 - Check ClassDirVector under boundary conditions - Centered sectors - Dirs < [0,359.9999]"
		rvDir = [-1., -360.+89., -360.+179., -360.+269.]
		ivExpectedClass = [1, 5, 9, 13]
		print *, 'Dir, Class, Expected.Class'
		ivClass = ClassDir(rvDir, 16, WDCLASS_ZERO_CENTERED)
		do i = 1, size(rvDir)
			print *, rvDir(i), ivClass(i), ivExpectedClass(i)
		end do
		print *
		
		! Test 5, boundary condition
		print *, "Test 5 - Check ClassDirVector under one NaN direction"
		rvDir = [359., NaN, 179., 269.]
		ivExpectedClass = [1, -9999, 9, 13]
		print *, 'Dir, Class, Expected.Class'
		ivClass = ClassDir(rvDir, 16)
		do i = 1, size(rvDir)
			print *, rvDir(i), ivClass(i), ivExpectedClass(i)
		end do
		print *
				
		! Test 6, boundary condition
		print *, "Test 6 - Check ClassDirVector under zero-length direction vector"
		deallocate(ivExpectedClass)
		deallocate(rvDir)
		allocate(rvDir(0))
		print *, 'Dir, Class (expected: none printed)'
		ivClass = ClassDir(rvDir, 16)
		do i = 1, size(rvDir)
			print *, rvDir(i), ivClass(i)
		end do
		print *
				
		! Leave
		deallocate(rvDir)
		
	end subroutine tst_classdirvector
	
	
	subroutine tst_windVectorScalar()
	
		! Routine arguments
		! -none-
		
		! Locals
		real, dimension(:), allocatable	:: rvVel, rvDir
		real							:: rVel, rDir, rScalarVel
		real, dimension(2)				:: rvPolar
		integer							:: i
		
		! Test 1 - Normal case - Vector velocity and direction for rotating wind
		allocate(rvVel(32), rvDir(32))
		rvVel = 1.
		rvDir = [(360.0*(i-1)/32., i = 1, 32)]
		rvPolar = VectorDirVel(rvVel, rvDir)
		rVel = rvPolar(1)
		rDir = rvPolar(2)
		print *, "Test 1 - Vector vel and dir from rotating wind"
		print *, "Vel = ", rVel, "  (expected: 0.)"
		print *, "Dir = ", rDir, "  (expected: anything)"
		print *
		
		! Test 2 - Normal case - Scalar velocity for rotating wind
		rScalarVel = ScalarVel(rvVel)
		print *, "Test 2 - Scalar vel from rotating wind"
		print *, "Scalar.Vel = ", rScalarVel, "  (expected: 1.)"
		print *
		
		! Test 3 - Normal case - Vector velocity and direction for wind fluctuating around 0°
		rvVel = 1.
		call random_number(rvDir)
		rvDir = rvDir - 0.5
		where(rvDir < 0.)
			rvDir = rvDir + 360.
		end where
		rvPolar = VectorDirVel(rvVel, rvDir)
		rVel = rvPolar(1)
		rDir = rvPolar(2)
		print *, "Test 3 - Vector vel and dir from wind fluctuating around 0°"
		print *, "Vel = ", rVel, "  (expected: close to 1.)"
		print *, "Dir = ", rDir, "  (expected: close to 0. or 360.)"
		print *
		
		! Leave
		deallocate(rvVel, rvDir)
		
	end subroutine tst_windVectorScalar

	
end program t_pbl_wind
