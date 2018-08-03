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
	call tst_UnitDir()
	call tst_WindRose()
	call tst_CompareWindRoses()
	call tst_VelDirMean()
	call tst_SonicData()
	
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
		
		! Test 4 - Normal case - Scalar velocity for wind fluctuating around 0°
		deallocate(rvVel, rvDir)
		allocate(rvVel(2048), rvDir(2048))
		rScalarVel = ScalarVel(rvVel)
		print *, "Test 4 - Scalar vel from wind fluctuating around 0°"
		print *, "Scalar.Vel = ", rScalarVel, "  (expected: 1.)"
		print *
		
		! Test 5 - Normal case - Vector to scalar speed ratio for various span angles
		if(.false.) then
			deallocate(rvVel, rvDir)
			allocate(rvVel(2048000), rvDir(2048000))
			rvVel = 1.
			print *, "Test 5 - Vector to scalar speed ratio for various span angles"
			print *, "Span, Vel, Scalar.vel, Vel/Scalar.Vel"
			do i = 5, 125, 5
				call random_number(rvDir)
				rvDir = rvDir - 0.5
				rvDir = rvDir * i
				where(rvDir < 0.)
					rvDir = rvDir + 360.
				end where
				rvPolar = VectorDirVel(rvVel, rvDir)
				rVel = rvPolar(1)
				rDir = rvPolar(2)
				rScalarVel = ScalarVel(rvVel)
				print "(f4.0,2(',',f6.4),',',f8.6)", float(i), rVel, rScalarVel, rVel/rScalarVel
			end do
		else
			print *, "Omitting slow test 5"
		end if
		print *
		
		! Test 6 - Normal case - Vector velocity and direction for wind fluctuating around 90°
		deallocate(rvVel, rvDir)
		allocate(rvVel(32), rvDir(32))
		rvVel = 1.
		call random_number(rvDir)
		rvDir = rvDir - 0.5 + 90.
		where(rvDir < 0.)
			rvDir = rvDir + 360.
		end where
		rvPolar = VectorDirVel(rvVel, rvDir)
		rVel = rvPolar(1)
		rDir = rvPolar(2)
		print *, "Test 6 - Vector vel and dir from wind fluctuating around 90°"
		print *, "Vel = ", rVel, "  (expected: close to 1.)"
		print *, "Dir = ", rDir, "  (expected: close to 90.)"
		print *
		
		! Test 7 - Normal case - Vector velocity and direction for wind fluctuating around 180°
		deallocate(rvVel, rvDir)
		allocate(rvVel(32), rvDir(32))
		rvVel = 1.
		call random_number(rvDir)
		rvDir = rvDir - 0.5 + 180.
		where(rvDir < 0.)
			rvDir = rvDir + 360.
		end where
		rvPolar = VectorDirVel(rvVel, rvDir)
		rVel = rvPolar(1)
		rDir = rvPolar(2)
		print *, "Test 7 - Vector vel and dir from wind fluctuating around 180°"
		print *, "Vel = ", rVel, "  (expected: close to 1.)"
		print *, "Dir = ", rDir, "  (expected: close to 180.)"
		print *
		
		! Test 8 - Normal case - Vector velocity and direction for wind fluctuating around 270°
		deallocate(rvVel, rvDir)
		allocate(rvVel(32), rvDir(32))
		rvVel = 1.
		call random_number(rvDir)
		rvDir = rvDir - 0.5 + 270.
		where(rvDir < 0.)
			rvDir = rvDir + 360.
		end where
		rvPolar = VectorDirVel(rvVel, rvDir)
		rVel = rvPolar(1)
		rDir = rvPolar(2)
		print *, "Test 8 - Vector vel and dir from wind fluctuating around 270°"
		print *, "Vel = ", rVel, "  (expected: close to 1.)"
		print *, "Dir = ", rDir, "  (expected: close to 270.)"
		print *
		
		! Test 9 - Normal case - Vector velocity and direction for wind fluctuating around 270°, one direction NaN value
		deallocate(rvVel, rvDir)
		allocate(rvVel(32), rvDir(32))
		rvVel = 1.
		call random_number(rvDir)
		rvDir = rvDir - 0.5 + 270.
		where(rvDir < 0.)
			rvDir = rvDir + 360.
		end where
		rvDir(8) = NaN
		rvPolar = VectorDirVel(rvVel, rvDir)
		rVel = rvPolar(1)
		rDir = rvPolar(2)
		print *, "Test 9 - Vector vel and dir from wind fluctuating around 270° with one direction NaN"
		print *, "Vel = ", rVel, "  (expected: close to 1.)"
		print *, "Dir = ", rDir, "  (expected: close to 270.)"
		print *
		
		! Test 10 - Normal case - Vector velocity and direction for wind fluctuating around 270° with one speed NaN
		deallocate(rvVel, rvDir)
		allocate(rvVel(32), rvDir(32))
		rvVel = 1.
		call random_number(rvDir)
		rvDir = rvDir - 0.5 + 270.
		where(rvDir < 0.)
			rvDir = rvDir + 360.
		end where
		rvVel(8) = NaN
		rvPolar = VectorDirVel(rvVel, rvDir)
		rVel = rvPolar(1)
		rDir = rvPolar(2)
		print *, "Test 10 - Vector vel and dir from wind fluctuating around 270° with one speed NaN"
		print *, "Vel = ", rVel, "  (expected: close to 1.)"
		print *, "Dir = ", rDir, "  (expected: close to 270.)"
		print *
		
		! Test 11 - Boundary case - Vector velocity and direction for all direction NaN
		deallocate(rvVel, rvDir)
		allocate(rvVel(32), rvDir(32))
		rvVel = 1.
		rvDir = NaN
		rvPolar = VectorDirVel(rvVel, rvDir)
		rVel = rvPolar(1)
		rDir = rvPolar(2)
		print *, "Test 11 - Vector vel and dir with all direction NaN"
		print *, "Vel = ", rVel, "  (expected: NaN)"
		print *, "Dir = ", rDir, "  (expected: NaN)"
		print *
		
		! Test 12 - Boundary case - Vector velocity and direction for all speed NaN
		deallocate(rvVel, rvDir)
		allocate(rvVel(32), rvDir(32))
		rvVel = NaN
		rvDir = 0.
		rvPolar = VectorDirVel(rvVel, rvDir)
		rVel = rvPolar(1)
		rDir = rvPolar(2)
		print *, "Test 12 - Vector vel and dir with all speed NaN"
		print *, "Vel = ", rVel, "  (expected: NaN)"
		print *, "Dir = ", rDir, "  (expected: NaN)"
		print *
		
		! Test 13 - Boundary case - Vector velocity and direction for zero-length data vectors
		deallocate(rvVel, rvDir)
		allocate(rvVel(0), rvDir(0))
		rvPolar = VectorDirVel(rvVel, rvDir)
		rVel = rvPolar(1)
		rDir = rvPolar(2)
		print *, "Test 13 - Vector vel and dir with zero-length data vectors"
		print *, "Vel = ", rVel, "  (expected: NaN)"
		print *, "Dir = ", rDir, "  (expected: NaN)"
		print *
		
		! Test 14 - Boundary case - Vector velocity and direction for unequal-length data vectors
		deallocate(rvVel, rvDir)
		allocate(rvVel(10), rvDir(20))
		rvVel = 1.
		rvDir = 0.
		rvPolar = VectorDirVel(rvVel, rvDir)
		rVel = rvPolar(1)
		rDir = rvPolar(2)
		print *, "Test 14 - Vector vel and dir with zero-length data vectors"
		print *, "Vel = ", rVel, "  (expected: NaN)"
		print *, "Dir = ", rDir, "  (expected: NaN)"
		print *
		
		! Test 15 - Normal case - Scalar speed for wind with one speed NaN
		deallocate(rvVel, rvDir)
		allocate(rvVel(32), rvDir(32))
		rvVel = 1.
		rvVel(8) = NaN
		rScalarVel = ScalarVel(rvVel)
		print *, "Test 15 - Scalar vel from wind with one speed NaN"
		print *, "S.Vel = ", rScalarVel, "  (expected: 1.)"
		print *
		
		! Test 16 - Boundary case - Scalar speed for wind with all NaN
		deallocate(rvVel, rvDir)
		allocate(rvVel(32), rvDir(32))
		rvVel = NaN
		rScalarVel = ScalarVel(rvVel)
		print *, "Test 16 - Scalar vel from wind with all NaN"
		print *, "S.Vel = ", rScalarVel, "  (expected: NaN)"
		print *
		
		! Test 17 - Boundary case - Scalar speed for zero-length vector
		deallocate(rvVel, rvDir)
		allocate(rvVel(0), rvDir(0))
		rScalarVel = ScalarVel(rvVel)
		print *, "Test 17 - Scalar vel from null-length vector"
		print *, "S.Vel = ", rScalarVel, "  (expected: NaN)"
		print *
		
		! Leave
		deallocate(rvVel, rvDir)
		
	end subroutine tst_windVectorScalar
	
	
	subroutine tst_UnitDir()
	
		! Routine arguments
		! -none-
		
		! Locals
		real, dimension(:), allocatable	:: rvDir
		real							:: rDir
		integer							:: i
		
		! Test 1 - Normal case - Unit direction for rotating wind
		allocate(rvDir(32))
		rvDir = [(360.0*(i-1)/32., i = 1, 32)]
		rDir = UnitDir(rvDir)
		print *, "Test 1 - Unit dir from rotating wind"
		print *, "Dir = ", rDir, "  (expected: anything)"
		print *
		
		! Test 2 - Normal case - Unit direction for wind fluctuating around 0°
		call random_number(rvDir)
		rvDir = rvDir - 0.5
		where(rvDir < 0.)
			rvDir = rvDir + 360.
		end where
		rDir = UnitDir(rvDir)
		print *, "Test 2 - Unit dir from wind fluctuating around 0°"
		print *, "Dir = ", rDir, "  (expected: close to 0. or 360.)"
		print *
		
		! Test 3 - Normal case - Unit direction for wind fluctuating around 90°
		call random_number(rvDir)
		rvDir = rvDir - 0.5 + 90.
		where(rvDir < 0.)
			rvDir = rvDir + 360.
		end where
		rDir = UnitDir(rvDir)
		print *, "Test 3 - Unit dir from wind fluctuating around 90°"
		print *, "Dir = ", rDir, "  (expected: close to 90.)"
		print *
		
		! Test 4 - Normal case - Unit direction for wind fluctuating around 180°
		call random_number(rvDir)
		rvDir = rvDir - 0.5 + 180.
		where(rvDir < 0.)
			rvDir = rvDir + 360.
		end where
		rDir = UnitDir(rvDir)
		print *, "Test 4 - Unit dir from wind fluctuating around 180°"
		print *, "Dir = ", rDir, "  (expected: close to 180.)"
		print *
		
		! Test 5 - Normal case - Unit direction for wind fluctuating around 270°
		call random_number(rvDir)
		rvDir = rvDir - 0.5 + 270.
		where(rvDir < 0.)
			rvDir = rvDir + 360.
		end where
		rDir = UnitDir(rvDir)
		print *, "Test 5 - Unit dir from wind fluctuating around 270°"
		print *, "Dir = ", rDir, "  (expected: close to 270.)"
		print *
		
		! Test 6 - Normal case - Unit direction for wind fluctuating around 270°, with one NaN
		call random_number(rvDir)
		rvDir = rvDir - 0.5 + 270.
		where(rvDir < 0.)
			rvDir = rvDir + 360.
		end where
		rvDir(7) = NaN
		rDir = UnitDir(rvDir)
		print *, "Test 6 - Unit dir from wind fluctuating around 270°, one NaN"
		print *, "Dir = ", rDir, "  (expected: close to 270.)"
		print *
		
		! Test 7 - Boundary case - Unit direction for all NaN wind dir
		rvDir = NaN
		rDir = UnitDir(rvDir)
		print *, "Test 7 - Unit dir from wind dir always NaN"
		print *, "Dir = ", rDir, "  (expected: NaN)"
		print *
		
		! Test 8 - Boundary case - Unit direction for zero-sized wind dir
		deallocate(rvDir)
		allocate(rvDir(0))
		rDir = UnitDir(rvDir)
		print *, "Test 8 - Unit dir from zero-size wind dir"
		print *, "Dir = ", rDir, "  (expected: NaN)"
		print *
		
		! Leave
		deallocate(rvDir)
		
	end subroutine tst_UnitDir
	
	
	subroutine tst_WindRose()
	
		! Routine arguments
		! -none-
		
		! Locals
		real, dimension(:), allocatable		:: rvVel
		real, dimension(:), allocatable		:: rvDir
		real, dimension(:,:), allocatable	:: rmWindRose
		integer								:: iRetCode
		integer, dimension(2)				:: imPos
		
		! Test 1: Nominal, single-class case
		allocate(rvVel(1), rvDir(1))
		rvVel = 1.1
		rvDir = 179.1
		print *, "Test 1 - Wind rose, nominal, single value"
		print *
		print *, "Case 1: zero-centered classes"
		iRetCode = WindRose(rvVel, rvDir, [0.5, 1., 2., 3., 4.5, 10.], 16, WDCLASS_ZERO_CENTERED, rmWindRose)
		print *, "Position of max value: ", maxloc(rmWindRose), "  (expected: 3, 9) - Ret.code = ", iRetCode
		print *, "Num.zeros: ", count(rmWindRose<=0.), " (expected: 111)"
		print *
		print *, "Case 2: zero-based classes"
		iRetCode = WindRose(rvVel, rvDir, [0.5, 1., 2., 3., 4.5, 10.], 16, WDCLASS_ZERO_BASED, rmWindRose)
		print *, "Position of max value: ", maxloc(rmWindRose), "  (expected: 3, 8) - Ret.code = ", iRetCode
		print *, "Num.zeros: ", count(rmWindRose<=0.), " (expected: 111)"
		print *
		
		! Test 2: Nominal, uniform case
		deallocate(rvVel, rvDir)
		allocate(rvVel(16384), rvDir(16384))
		call random_number(rvVel)
		call random_number(rvDir)
		rvVel = rvVel*12.
		rvDir = rvDir*360.
		print *, "Test 2 - Wind rose, nominal, uniform case"
		print *
		print *, "Case 1: All data valid"
		iRetCode = WindRose(rvVel, rvDir, [0.5, 1., 2., 3., 4.5, 10.], 16, WDCLASS_ZERO_CENTERED, rmWindRose)
		print *, "Sum of all values: ", sum(rmWindRose), "  (expected: nearly 1.0) - Ret.code = ", iRetCode
		print *, "Num.zeros: ", count(rmWindRose<=0.), " (expected: few, if any at all)"
		print *
		print *, "Case 2: One invalid speed"
		rvVel(1024) = NaN
		iRetCode = WindRose(rvVel, rvDir, [0.5, 1., 2., 3., 4.5, 10.], 16, WDCLASS_ZERO_CENTERED, rmWindRose)
		print *, "Sum of all values: ", sum(rmWindRose), "  (expected: nearly 1.0) - Ret.code = ", iRetCode
		print *, "Num.zeros: ", count(rmWindRose<=0.), " (expected: few, if any at all)"
		print *
		print *, "Case 3: One invalid speed, one invalid direction"
		rvVel(1024) = NaN
		rvDir(2048) = NaN
		iRetCode = WindRose(rvVel, rvDir, [0.5, 1., 2., 3., 4.5, 10.], 16, WDCLASS_ZERO_CENTERED, rmWindRose)
		print *, "Sum of all values: ", sum(rmWindRose), "  (expected: nearly 1.0) - Ret.code = ", iRetCode
		print *, "Num.zeros: ", count(rmWindRose<=0.), " (expected: few, if any at all)"
		print *
		
		! Test 3: Boundary, all-invalid
		print *, "Test 3 - Wind rose, nominal, uniform case"
		print *
		print *, "Case 1: All speeds invalid"
		rvVel = NaN
		call random_number(rvDir)
		rvDir = rvDir*360.
		iRetCode = WindRose(rvVel, rvDir, [0.5, 1., 2., 3., 4.5, 10.], 16, WDCLASS_ZERO_CENTERED, rmWindRose)
		print *, "Sum of all values: ", sum(rmWindRose), "  (expected: nearly 0.0) - Ret.code = ", iRetCode
		print *, "Num.zeros: ", count(rmWindRose<=0.), " (expected: 112)"
		print *
		print *, "Case 2: All directions invalid"
		rvVel = rvVel*12.
		call random_number(rvVel)
		rvDir = NaN
		iRetCode = WindRose(rvVel, rvDir, [0.5, 1., 2., 3., 4.5, 10.], 16, WDCLASS_ZERO_CENTERED, rmWindRose)
		print *, "Sum of all values: ", sum(rmWindRose), "  (expected: nearly 0.0) - Ret.code = ", iRetCode
		print *, "Num.zeros: ", count(rmWindRose<=0.), " (expected: 112)"
		print *
		print *, "Case 2: All speeds and directions invalid"
		rvVel = NaN
		rvDir = NaN
		iRetCode = WindRose(rvVel, rvDir, [0.5, 1., 2., 3., 4.5, 10.], 16, WDCLASS_ZERO_CENTERED, rmWindRose)
		print *, "Sum of all values: ", sum(rmWindRose), "  (expected: nearly 0.0) - Ret.code = ", iRetCode
		print *, "Num.zeros: ", count(rmWindRose<=0.), " (expected: 112)"
		print *
		
		! Test 4: Boundary, different vector lengths
		deallocate(rvVel, rvDir)
		allocate(rvVel(16384), rvDir(16385))
		call random_number(rvVel)
		call random_number(rvDir)
		rvVel = rvVel*12.
		rvDir = rvDir*360.
		print *, "Test 4 - Wind rose, differing data vector lengths"
		print *
		print *, "Case 1: Data vector lengths differ"
		iRetCode = WindRose(rvVel, rvDir, [0.5, 1., 2., 3., 4.5, 10.], 16, WDCLASS_ZERO_CENTERED, rmWindRose)
		print *, "Ret.code = ", iRetCode
		print *
		deallocate(rvVel, rvDir)
		allocate(rvVel(0), rvDir(0))
		print *, "Case 2: Data vector lengths equal to 0"
		iRetCode = WindRose(rvVel, rvDir, [0.5, 1., 2., 3., 4.5, 10.], 16, WDCLASS_ZERO_CENTERED, rmWindRose)
		print *, "Sum of all values: ", sum(rmWindRose), "  (expected: nearly 0.0) - Ret.code = ", iRetCode
		print *, "Num.zeros: ", count(rmWindRose<=0.), " (expected: 112)"
		print *
		
	end subroutine tst_WindRose
	
	
	subroutine tst_CompareWindRoses()
	
		! Routine arguments
		! --none--
		
		! Locals
		real, dimension(:), allocatable		:: vel1, dir1, vel2, dir2
		real, dimension(:,:), allocatable	:: rmWindRose1, rmWindRose2
		integer								:: iRetCode
		real								:: rProb, rChiSquareSum
		integer								:: iDegreesOfFreedom
		
		! Test 1: Identical roses
		print *, "Test 1: Identical roses"
		allocate(vel1(1024),dir1(1024),vel2(1024),dir2(1024))
		call random_number(vel1)
		call random_number(dir1)
		vel1 = 10.*vel1
		dir1 = 360.*dir1
		vel2 = vel1
		dir2 = dir1
		iRetCode = CompareWindRoses(&
			vel1, dir1, vel2, dir2, &
			[1., 2., 4.5, 7.], &
			16, WDCLASS_ZERO_CENTERED, &
			rmWindRose1, rmWindRose2, rProb, &
			rChiSquareSum, iDegreesOfFreedom &
		)
		print *, "Ret.code = ", iRetCode, "  (expected: 0)"
		print *, "Prob = ", rProb
		print *, "DF = ", iDegreesOfFreedom, "   Chi2_Sum =", rChiSquareSum
		print *
		
		! Test 2: Two roses all zero but on different classes
		print *, "Test 2: Quite unrelated roses"
		vel1 = 1.5
		dir1 = 0.
		vel2 = 10.
		dir2 = 180.
		iRetCode = CompareWindRoses(&
			vel1, dir1, vel2, dir2, &
			[1., 2., 4.5, 7.], &
			16, WDCLASS_ZERO_CENTERED, &
			rmWindRose1, rmWindRose2, rProb, &
			rChiSquareSum, iDegreesOfFreedom &
		)
		print *, "Ret.code = ", iRetCode, "  (expected: 0)"
		print *, "Prob = ", rProb
		print *, "DF = ", iDegreesOfFreedom, "   Chi2_Sum =", rChiSquareSum
		print *
		
		! Test 3: Two unrelated roses
		print *, "Test 3: Unrelated roses"
		call random_number(vel1)
		call random_number(dir1)
		vel1 = 10.*vel1
		dir1 = 360.*dir1
		vel2 = 10.
		dir2 = 180.
		iRetCode = CompareWindRoses(&
			vel1, dir1, vel2, dir2, &
			[1., 2., 4.5, 7.], &
			16, WDCLASS_ZERO_CENTERED, &
			rmWindRose1, rmWindRose2, rProb, &
			rChiSquareSum, iDegreesOfFreedom &
		)
		print *, "Ret.code = ", iRetCode, "  (expected: 0)"
		print *, "Prob = ", rProb
		print *, "DF = ", iDegreesOfFreedom, "   Chi2_Sum =", rChiSquareSum
		print *
		
		! Leave
		deallocate(vel1,dir1,vel2,dir2)
		
	end subroutine tst_CompareWindRoses
	
	
	subroutine tst_VelDirMean()
	
		! Routine arguments
		! --none--
		
		! Locals
		real, dimension(:), allocatable		:: vel, dir, scalar
		integer								:: iRetCode
		integer								:: i
		real, dimension(:,:), allocatable	:: rmMean
		
		! Test 1: Classify a velocity-dependent scalar
		allocate(vel(16384), dir(16384), scalar(16384))
		call random_number(dir)
		dir = dir * 360.
		call random_number(vel)
		vel = vel * 5.
		scalar = vel
		print *, 'Test 1: Classify velocity-dependent scalar'
		iRetCode = VelDirMean(vel, dir, scalar, [0.5, 1.5, 2.5, 3.5, 4.5], 16, WDCLASS_ZERO_BASED, rmMean)
		print *, 'Return code: ', iRetCode, '   (expected: 0)'
		print *
		print *, 'Speed.cls, Min(mean(scalar)), Max(mean(scalar))'
		do i = 1, 6
			print *, i, minval(rmMean(i,:)), maxval(rmMean(i,:))
		end do
		print *
		
		! Test 2: Classify a direction-dependent scalar
		call random_number(dir)
		dir = dir * 360.
		call random_number(vel)
		vel = vel * 5.
		scalar = dir
		print *, 'Test 2: Classify direction-dependent scalar'
		iRetCode = VelDirMean(vel, dir, scalar, [0.5, 1.5, 2.5, 3.5, 4.5], 16, WDCLASS_ZERO_BASED, rmMean)
		print *, 'Return code: ', iRetCode, '   (expected: 0)'
		print *
		print *, 'Dir.cls, Min(mean(scalar)), Max(mean(scalar))'
		do i = 1, 16
			print *, i, minval(rmMean(:,i)), maxval(rmMean(:,i))
		end do
		print *
		
		! Test 3: Classify a direction-dependent scalar, with an invalid wind
		call random_number(dir)
		dir = dir * 360.
		call random_number(vel)
		vel = vel * 5.
		vel(1) = NaN
		scalar = dir
		print *, 'Test 3: Classify direction-dependent scalar, with one invalid wind'
		iRetCode = VelDirMean(vel, dir, scalar, [0.5, 1.5, 2.5, 3.5, 4.5], 16, WDCLASS_ZERO_BASED, rmMean)
		print *, 'Return code: ', iRetCode, '   (expected: 0)'
		print *
		print *, 'Dir.cls, Min(mean(scalar)), Max(mean(scalar))'
		do i = 1, 16
			print *, i, minval(rmMean(:,i)), maxval(rmMean(:,i))
		end do
		print *
		
		! Test 4: Classify a direction-dependent scalar, with an invalid scalar
		call random_number(dir)
		dir = dir * 360.
		call random_number(vel)
		vel = vel * 5.
		scalar = dir
		scalar(1) = NaN
		print *, 'Test 4: Classify direction-dependent scalar, with one invalid scalar'
		iRetCode = VelDirMean(vel, dir, scalar, [0.5, 1.5, 2.5, 3.5, 4.5], 16, WDCLASS_ZERO_BASED, rmMean)
		print *, 'Return code: ', iRetCode, '   (expected: 0)'
		print *
		print *, 'Dir.cls, Min(mean(scalar)), Max(mean(scalar))'
		do i = 1, 16
			print *, i, minval(rmMean(:,i)), maxval(rmMean(:,i))
		end do
		print *
		
		! Test 5: Classify a direction-dependent scalar, with all invalid scalar
		call random_number(dir)
		dir = dir * 360.
		call random_number(vel)
		vel = vel * 5.
		scalar = NaN
		print *, 'Test 5: Classify direction-dependent scalar, with all invalid scalar'
		iRetCode = VelDirMean(vel, dir, scalar, [0.5, 1.5, 2.5, 3.5, 4.5], 16, WDCLASS_ZERO_BASED, rmMean)
		print *, 'Return code: ', iRetCode, '   (expected: 0)'
		print *
		print *, 'Dir.cls, Min(mean(scalar)), Max(mean(scalar))'
		do i = 1, 16
			print *, i, minval(rmMean(:,i)), maxval(rmMean(:,i))
		end do
		print *
		
		! Test 6: Classify a direction-dependent scalar, with zero-length speed
		deallocate(vel)
		allocate(vel(0))
		call random_number(dir)
		dir = dir * 360.
		scalar = dir
		print *, 'Test 6: Classify direction-dependent scalar, with zero-length speed'
		iRetCode = VelDirMean(vel, dir, scalar, [0.5, 1.5, 2.5, 3.5, 4.5], 16, WDCLASS_ZERO_BASED, rmMean)
		print *, 'Return code: ', iRetCode, '   (expected: non-zero)'
		print *
		
		! Test 7: Classify a direction-dependent scalar, with zero-length direction
		deallocate(vel,dir)
		allocate(vel(16384),dir(0))
		call random_number(vel)
		vel = vel * 5.
		scalar = vel
		print *, 'Test 7: Classify direction-dependent scalar, with zero-length direction'
		iRetCode = VelDirMean(vel, dir, scalar, [0.5, 1.5, 2.5, 3.5, 4.5], 16, WDCLASS_ZERO_BASED, rmMean)
		print *, 'Return code: ', iRetCode, '   (expected: non-zero)'
		print *
		
		! Test 8: Classify a direction-dependent scalar, with zero-length scalar
		deallocate(vel,dir,scalar)
		allocate(vel(16384),dir(16384), scalar(0))
		call random_number(vel)
		vel = vel * 5.
		call random_number(dir)
		dir = dir * 360.
		print *, 'Test 8: Classify direction-dependent scalar, with zero-length scalar'
		iRetCode = VelDirMean(vel, dir, scalar, [0.5, 1.5, 2.5, 3.5, 4.5], 16, WDCLASS_ZERO_BASED, rmMean)
		print *, 'Return code: ', iRetCode, '   (expected: non-zero)'
		print *
		
		! Test 9: Classify a direction-dependent scalar, with different-length inputs
		deallocate(vel,dir,scalar)
		allocate(vel(16384),dir(16384), scalar(16385))
		call random_number(vel)
		vel = vel * 5.
		call random_number(dir)
		dir = dir * 360.
		scalar = 1.
		print *, 'Test 9: Classify a direction-dependent scalar, with different-length inputs'
		iRetCode = VelDirMean(vel, dir, scalar, [0.5, 1.5, 2.5, 3.5, 4.5], 16, WDCLASS_ZERO_BASED, rmMean)
		print *, 'Return code: ', iRetCode, '   (expected: non-zero)'
		print *
		
		! Test 10: Classify a direction-dependent scalar, with all zero-length inputs
		deallocate(vel,dir,scalar)
		allocate(vel(0),dir(0), scalar(0))
		print *, 'Test 10: Classify direction-dependent scalar, with all zero-length inputs'
		iRetCode = VelDirMean(vel, dir, scalar, [0.5, 1.5, 2.5, 3.5, 4.5], 16, WDCLASS_ZERO_BASED, rmMean)
		print *, 'Return code: ', iRetCode, '   (expected: non-zero)'
		print *
		
		! Leave
		deallocate(vel, dir, scalar)
		
	end subroutine tst_VelDirMean
	
	
	subroutine tst_SonicData()
	
		! Routine arguments
		! --none--
		
		! Locals
		type(SonicData)						:: tSonic
		type(EddyCovData)					:: tEc
		type(EddyCovData)					:: tMultiEc
		integer								:: iRetCode
		integer								:: i
		integer								:: iHour
		type(DateTime)						:: dt
		real(8), dimension(:), allocatable	:: rvTimeSt
		real, dimension(:), allocatable		:: rvU
		real, dimension(:), allocatable		:: rvV
		real, dimension(:), allocatable		:: rvW
		real, dimension(:), allocatable		:: rvTemp
		real(8)								:: rBaseTime
		type(TrendData)						:: tTrend
		
		! Test 1: Read and count an existing SonicLib file name
		print *, "Test 1: Read SonicLib file"
		iRetCode = tSonic % readSonicLib(10, "20130308.12.csv", OS_UNIX)
		print *, "Return code: ", iRetCode, " (expected: 0)"
		print *, "Size:        ", tSonic % size(), " (expected: > 0)"
		print *, "Valid:       ", tSonic % valid(), " (expected: > 0)"
		print *
		
		! Test 2: Compute hourly means on SonicLib file read
		print *, "Test 2: Hourly means from SonicLib file"
		iRetCode = tSonic % averages( &
			3600, &
			tEc &
		)
		print *, "Return code = ", iRetCode, "  (expected: 0)"
		iRetCode = tEc % dump()
		print *
		
		! Test 3: Compute 10-minutes means on SonicLib file read
		print *, "Test 3: 10-minutes means from SonicLib file"
		iRetCode = tSonic % averages( &
			600, &
			tEc &
		)
		print *, "Return code = ", iRetCode, "  (expected: 0)"
		iRetCode = tEc % dump()
		print *
		
		! Test 4: Compute 7-min means on SonicLib file read
		print *, "Test 4: 7-minutes means from SonicLib file"
		iRetCode = tSonic % averages( &
			420, &
			tEc &
		)
		print *, "Return code = ", iRetCode, "  (expected: non-zero, as 420s does not divide 3600s exactly)"
		print *
		
		! Test 5: hourly mean on artificial data
		allocate(rvTimeSt(60), rvU(60), rvV(60), rvW(60), rvTemp(60))
		dt = DateTime(2000, 3, 8, 12, 0, 0.0d0)
		rvTimeSt = [(dt % toEpoch() + (i-1)*60.d0, i = 1, 60)]
		rvU      = [(real((i-1)/15), i = 1, 60)]
		rvV      = [(real((i-1)/30), i = 1, 60)]
		rvW      = [(real((i-1)/10), i = 1, 60)]
		rvTemp   = 0.
		iRetCode = tSonic % buildFromVectors(rvTimeSt, rvU, rvV, rvW, rvTemp)
		print *, "Test 5: hourly means from artificial data"
		iRetCode = tSonic % averages( &
			3600, &
			tEc &
		)
		print *, "Return code = ", iRetCode, "  (expected: 0)"
		iRetCode = tEc % dump()
		print *
		
		! Test 6: 30-min mean on artificial data
		print *, "Test 6: 30-min means from artificial data"
		iRetCode = tSonic % averages( &
			1800, &
			tEc &
		)
		print *, "Return code = ", iRetCode, "  (expected: 0)"
		iRetCode = tEc % dump()
		print *
		
		! Test 7: 15-min mean on artificial data
		print *, "Test 7: 15-min means from artificial data"
		iRetCode = tSonic % averages( &
			900, &
			tEc &
		)
		print *, "Return code = ", iRetCode, "  (expected: 0)"
		iRetCode = tEc % dump()
		print *
		
		! Test 8: 10-min mean on artificial data
		print *, "Test 8: 10-min means from artificial data"
		iRetCode = tSonic % averages( &
			600, &
			tEc &
		)
		print *, "Return code = ", iRetCode, "  (expected: 0)"
		iRetCode = tEc % dump()
		print *
		
		! Test 9: 5-min mean on artificial data
		print *, "Test 9: 5-min means from artificial data"
		iRetCode = tSonic % averages( &
			300, &
			tEc &
		)
		print *, "Return code = ", iRetCode, "  (expected: 0)"
		iRetCode = tEc % dump()
		print *
		deallocate(rvTimeSt, rvU, rvV, rvW, rvTemp)
		
		! Test 10: Natural data, as in test no.1, processed every 5 minutes, and count
		! of valid inputs
		print *, "Test 10: Check valid data count in input section of EddyCovData"
		iRetCode = tSonic % readSonicLib(10, "20130308.12.csv", OS_UNIX)
		iRetCode = tSonic % averages(300, tEc)
		print *, 'Number of valid input data in EddyCovData: ', tEc % getNumValidInput(), '   (expected: ',3600/300,')'
		print *
		
		! Test 11: Count of valid inputs on an empty EddyCovData object
		print *, "Test 11: Check valid data count in clean input section of EddyCovData"
		iRetCode = tEc % clean()
		print *, 'Number of valid input data in clean EddyCovData: ', tEc % getNumValidInput(), '   (expected: 0)'
		print *

		! Test 12: Count on valid inputs in half-empty EddyCovData object		
		allocate(rvTimeSt(60), rvU(60), rvV(60), rvW(60), rvTemp(60))
		dt = DateTime(2000, 3, 8, 12, 0, 0.0d0)
		rvTimeSt   = [(dt % toEpoch() + (i-1)*60.d0, i = 1, 60)]
		rvU(1:30)  = [(real((i-1)/15), i = 1, 30)]
		rvU(31:60) = NaN
		rvV        = [(real((i-1)/30), i = 1, 60)]
		rvW        = [(real((i-1)/10), i = 1, 60)]
		rvTemp     = 0.
		iRetCode   = tSonic % buildFromVectors(rvTimeSt, rvU, rvV, rvW, rvTemp)
		print *, "Test 12: Check valid data count in half-empty input section of EddyCovData"
		iRetCode = tSonic % averages( &
			1800, &
			tEc &
		)
		print *, "Return code = ", iRetCode, "  (expected: 0)"
		print *, 'Number of valid input data in half-empty EddyCovData: ', tEc % getNumValidInput(), '   (expected: 1)'
		print *
		
		! Test 13: Count on valid inputs in empty EddyCovData object		
		dt = DateTime(2000, 3, 8, 12, 0, 0.0d0)
		rvTimeSt   = [(dt % toEpoch() + (i-1)*60.d0, i = 1, 60)]
		rvU        = NaN
		rvV        = [(real((i-1)/30), i = 1, 60)]
		rvW        = [(real((i-1)/10), i = 1, 60)]
		rvTemp     = 0.
		iRetCode   = tSonic % buildFromVectors(rvTimeSt, rvU, rvV, rvW, rvTemp)
		print *, "Test 13: Check valid data count in empty input section of EddyCovData"
		iRetCode = tSonic % averages( &
			1800, &
			tEc &
		)
		print *, "Return code = ", iRetCode, "  (expected: 2)"
		print *, 'Number of valid input data in empty EddyCovData: ', tEc % getNumValidInput(), '   (expected: 0)'
		print *
		
		! Test 14: compose multi-hourly set
		print *, "Test 14: composition of multi-hourly set from artificial data"
		deallocate(rvTimeSt, rvU, rvV, rvW, rvTemp)
		allocate(rvTimeSt(60), rvU(60), rvV(60), rvW(60), rvTemp(60))
		dt = DateTime(2000, 3, 8, 12, 0, 0.0d0)
		rBaseTime = dt % toEpoch()
		iRetCode = tMultiEc % createEmpty(4, 3600)
		do iHour = 12, 15
			dt = DateTime(2000, 3, 8, iHour, 0, 0.0d0)
			rvTimeSt = [(dt % toEpoch() + (i-1)*60.d0, i = 1, 60)]
			rvU      = float(iHour)
			rvV      = float(iHour)*2.
			rvW      = float(iHour)*3.
			rvTemp   = float(iHour) + 10.
			iRetCode = tSonic % buildFromVectors(rvTimeSt, rvU, rvV, rvW, rvTemp)
			iRetCode = tSonic % averages( &
				3600, &
				tEc &
			)
			iRetCode = tMultiEc % add(rBaseTime, tEc)
			print *, "Hour: ",iHour, "  Return code: ", iRetCode, "   (expected: 0)"
		end do
		iRetCode = tMultiEc % dump()
		print *
		
		! Test 15: compose multi-hourly set
		print *, "Test 15: composition of multi-hourly set from artificial data, 30-min avgs"
		dt = DateTime(2000, 3, 8, 12, 0, 0.0d0)
		rBaseTime = dt % toEpoch()
		iRetCode = tMultiEc % createEmpty(4, 1800)
		do iHour = 12, 15
			dt = DateTime(2000, 3, 8, iHour, 0, 0.0d0)
			rvTimeSt = [(dt % toEpoch() + (i-1)*60.d0, i = 1, 60)]
			rvU      = float(iHour)
			rvV      = float(iHour)*2.
			rvW      = float(iHour)*3.
			rvTemp   = float(iHour) + 10.
			iRetCode = tSonic % buildFromVectors(rvTimeSt, rvU, rvV, rvW, rvTemp)
			iRetCode = tSonic % averages( &
				1800, &
				tEc &
			)
			iRetCode = tMultiEc % add(rBaseTime, tEc)
			print *, "Hour: ",iHour, "  Return code: ", iRetCode, "   (expected: 0)"
		end do
		iRetCode = tMultiEc % dump()
		print *
		
		! Test 16: compose multi-hourly set, with off-time data
		print *, "Test 16: composition of multi-hourly set from artificial data, 30-min avgs, 1 off-time hour"
		dt = DateTime(2000, 3, 8, 12, 0, 0.0d0)
		rBaseTime = dt % toEpoch()
		iRetCode = tMultiEc % createEmpty(4, 1800)
		do iHour = 12, 16
			dt = DateTime(2000, 3, 8, iHour, 0, 0.0d0)
			rvTimeSt = [(dt % toEpoch() + (i-1)*60.d0, i = 1, 60)]
			rvU      = float(iHour)
			rvV      = float(iHour)*2.
			rvW      = float(iHour)*3.
			rvTemp   = float(iHour) + 10.
			iRetCode = tSonic % buildFromVectors(rvTimeSt, rvU, rvV, rvW, rvTemp)
			iRetCode = tSonic % averages( &
				1800, &
				tEc &
			)
			iRetCode = tMultiEc % add(rBaseTime, tEc)
			if(iHour < 16) then
				print *, "Hour: ",iHour, "  Return code: ", iRetCode, "   (expected:  0)"
			else
				print *, "Hour: ",iHour, "  Return code: ", iRetCode, "   (expected: -1)"
			end if
		end do
		iRetCode = tMultiEc % dump()
		print *
		
		! Test 17: compose multi-hourly set, with all off-time data
		print *, "Test 17: composition of multi-hourly set from artificial data, 30-min avgs, off-time hours"
		dt = DateTime(2000, 3, 8, 12, 0, 0.0d0)
		rBaseTime = dt % toEpoch()
		iRetCode = tMultiEc % createEmpty(4, 1800)
		do iHour = 16, 18
			dt = DateTime(2000, 3, 8, iHour, 0, 0.0d0)
			rvTimeSt = [(dt % toEpoch() + (i-1)*60.d0, i = 1, 60)]
			rvU      = float(iHour)
			rvV      = float(iHour)*2.
			rvW      = float(iHour)*3.
			rvTemp   = float(iHour) + 10.
			iRetCode = tSonic % buildFromVectors(rvTimeSt, rvU, rvV, rvW, rvTemp)
			iRetCode = tSonic % averages( &
				1800, &
				tEc &
			)
			iRetCode = tMultiEc % add(rBaseTime, tEc)
			if(iHour < 16) then
				print *, "Hour: ",iHour, "  Return code: ", iRetCode, "   (expected:  0)"
			else
				print *, "Hour: ",iHour, "  Return code: ", iRetCode, "   (expected: -1)"
			end if
		end do
		iRetCode = tMultiEc % dump()
		print *
		
		! Test 18: Processing, 2 rot, case 1
		print *, "Test 18: Processing, case 1"
		dt = DateTime(2000, 3, 8, 12, 0, 0.0d0)
		rvTimeSt = [(dt % toEpoch() + (i-1)*60.d0, i = 1, 60)]
		rvU      = 1.
		rvV      = 0.
		rvW      = 0.
		rvTemp   = 0.
		iRetCode = tSonic % buildFromVectors(rvTimeSt, rvU, rvV, rvW, rvTemp)
		iRetCode = tSonic % averages( &
			3600, &
			tEc &
		)
		print *, "Data assembly: return code = ", iRetCode, "  (expected: 0)"
		iRetCode = tEc % process(2)
		print *, "Processing: return code = ", iRetCode, "  (expected: 0)"
		iRetCode = tEc % dump()
		print *
		
		! Test 19: Processing, 2 rot, case 2
		print *, "Test 19: Processing, case 2"
		dt = DateTime(2000, 3, 8, 12, 0, 0.0d0)
		rvTimeSt = [(dt % toEpoch() + (i-1)*60.d0, i = 1, 60)]
		rvU      = 0.
		rvV      = 1.
		rvW      = 0.
		rvTemp   = 0.
		iRetCode = tSonic % buildFromVectors(rvTimeSt, rvU, rvV, rvW, rvTemp)
		iRetCode = tSonic % averages( &
			3600, &
			tEc &
		)
		print *, "Data assembly: return code = ", iRetCode, "  (expected: 0)"
		iRetCode = tEc % process(2)
		print *, "Processing: return code = ", iRetCode, "  (expected: 0)"
		iRetCode = tEc % dump()
		print *
		
		! Test 20: Processing, 2 rot, case 3
		print *, "Test 20: Processing, case 3"
		dt = DateTime(2000, 3, 8, 12, 0, 0.0d0)
		rvTimeSt = [(dt % toEpoch() + (i-1)*60.d0, i = 1, 60)]
		rvU      = 1./sqrt(2.)
		rvV      = 1./sqrt(2.)
		rvW      = 1.
		rvTemp   = 0.
		iRetCode = tSonic % buildFromVectors(rvTimeSt, rvU, rvV, rvW, rvTemp)
		iRetCode = tSonic % averages( &
			3600, &
			tEc &
		)
		print *, "Data assembly: return code = ", iRetCode, "  (expected: 0)"
		iRetCode = tEc % process(2)
		print *, "Processing: return code = ", iRetCode, "  (expected: 0)"
		iRetCode = tEc % dump()
		print *
		
		! Test 21: Processing, 2 rot, case 4
		print *, "Test 21: Processing, case 4"
		dt = DateTime(2000, 3, 8, 12, 0, 0.0d0)
		rvTimeSt = [(dt % toEpoch() + (i-1)*60.d0, i = 1, 60)]
		rvU      = 1./sqrt(2.)
		rvV      = 1./sqrt(2.)
		rvW      = -1.
		rvTemp   = 0.
		iRetCode = tSonic % buildFromVectors(rvTimeSt, rvU, rvV, rvW, rvTemp)
		iRetCode = tSonic % averages( &
			3600, &
			tEc &
		)
		print *, "Data assembly: return code = ", iRetCode, "  (expected: 0)"
		iRetCode = tEc % process(2)
		print *, "Processing: return code = ", iRetCode, "  (expected: 0)"
		iRetCode = tEc % dump()
		print *
		
		! Test 22: Detrending, from artificial case with known results
		deallocate(rvTimeSt, rvU, rvV, rvW, rvTemp)
		allocate(rvTimeSt(15), rvU(15), rvV(15), rvW(15), rvTemp(15))
		rvTimeSt = [1.47, 1.50, 1.52, 1.55, 1.57, 1.60, 1.63, 1.65, 1.68, 1.70, 1.73, 1.75, 1.78, 1.80, 1.83]
		rvU      = [52.21, 53.12, 54.48, 55.84, 57.20, 58.57, 59.93, 61.29, 63.11, 64.47, 66.28, 68.10, 69.92, 72.19, 74.46]
		rvV      = 2.*rvU
		rvW      = 3.*rvU
		rvTemp   = 4.*rvU
		iRetCode = tSonic % buildFromVectors(rvTimeSt, rvU, rvV, rvW, rvTemp)
		iRetCode = tSonic % removeTrend( &
			3600, &
			tTrend &
		)
		print *, "Test 22: Detrending, with synthetic data having known characteristics"
		print *
		print *, "Number of data:      ", tTrend % ivNumData(1)
		print *, "Alpha U (intercept): ", tTrend % rvAlphaU(1),   "  (expected: -39.062)"
		print *, "Beta U  (slope):     ", tTrend % rvBetaU(1),    "  (expected:  61.272)"
		print *, "S2eps U:             ", tTrend % rvS2epsU(1),   "  (expected:   0.5762)"
		print *, "S2alpha U:           ", tTrend % rvS2alphaU(1), "  (expected:   8.63185)"
		print *, "S2beta U:            ", tTrend % rvS2betaU(1),  "  (expected:   3.1539)"
		print *
		print *, "Alphas: ", tTrend % rvAlphaU, tTrend % rvAlphaV, tTrend % rvAlphaW, tTrend % rvAlphaT
		print *, "Betas:  ", tTrend % rvBetaU, tTrend % rvBetaV, tTrend % rvBetaW, tTrend % rvBetaT
		print *, "S2eps:  ", tTrend % rvS2epsU, tTrend % rvS2epsV, tTrend % rvS2epsW, tTrend % rvS2epsT
		print *, "S2alpha:", tTrend % rvS2alphaU, tTrend % rvS2alphaV, tTrend % rvS2alphaW, tTrend % rvS2alphaT
		print *, "S2beta: ", tTrend % rvS2betaU, tTrend % rvS2betaV, tTrend % rvS2betaW, tTrend % rvS2betaT
		print *
		
		! Test 23: Detrending, with artificial data having known behavior, part 2
		deallocate(rvTimeSt, rvU, rvV, rvW, rvTemp)
		allocate(rvTimeSt(3600), rvU(3600), rvV(3600), rvW(3600), rvTemp(3600))
		rvTimeSt = [(real(i-1, kind=8), i = 1, 3600)]
		rvU      = rvTimeSt
		rvV      = rvU / 2.
		rvW      = rvV / 2.
		rvTemp   = rvW / 2.
		iRetCode = tSonic % buildFromVectors(rvTimeSt, rvU, rvV, rvW, rvTemp)
		iRetCode = tSonic % removeTrend( &
			3600, &
			tTrend &
		)
		print *, "Test 23: Detrending, with synthetic data having known characteristics"
		print *
		print *, "Number of data:      ", tTrend % ivNumData(1)
		print *, "Alpha U (intercept): ", tTrend % rvAlphaU(1),   "  (expected:   0.000)"
		print *, "Beta U  (slope):     ", tTrend % rvBetaU(1),    "  (expected:   1.000)"
		print *, "S2eps U:             ", tTrend % rvS2epsU(1)
		print *, "S2alpha U:           ", tTrend % rvS2alphaU(1)
		print *, "S2beta U:            ", tTrend % rvS2betaU(1)
		print *
		print *, "Alphas: ", tTrend % rvAlphaU, tTrend % rvAlphaV, tTrend % rvAlphaW, tTrend % rvAlphaT
		print *, "Betas:  ", tTrend % rvBetaU, tTrend % rvBetaV, tTrend % rvBetaW, tTrend % rvBetaT
		print *, "S2eps:  ", tTrend % rvS2epsU, tTrend % rvS2epsV, tTrend % rvS2epsW, tTrend % rvS2epsT
		print *, "S2alpha:", tTrend % rvS2alphaU, tTrend % rvS2alphaV, tTrend % rvS2alphaW, tTrend % rvS2alphaT
		print *, "S2beta: ", tTrend % rvS2betaU, tTrend % rvS2betaV, tTrend % rvS2betaW, tTrend % rvS2betaT
		print *
		
		! Test 24: as Test 23, but with 1800s averaging time instead of 3600
		iRetCode = tSonic % buildFromVectors(rvTimeSt, rvU, rvV, rvW, rvTemp)
		iRetCode = tSonic % removeTrend( &
			1800, &
			tTrend &
		)
		print *, "Test 24: Detrending, with synthetic data having known characteristics"
		print *
		print *, "Number of data:      ", tTrend % ivNumData
		print *, "Alpha U (intercept): ", tTrend % rvAlphaU,   "  (expected:   0.000)"
		print *, "Beta U  (slope):     ", tTrend % rvBetaU,    "  (expected:   1.000)"
		print *, "S2eps U:             ", tTrend % rvS2epsU
		print *, "S2alpha U:           ", tTrend % rvS2alphaU
		print *, "S2beta U:            ", tTrend % rvS2betaU
		print *
		print *, "Alphas: ", tTrend % rvAlphaU, tTrend % rvAlphaV, tTrend % rvAlphaW, tTrend % rvAlphaT
		print *, "Betas:  ", tTrend % rvBetaU, tTrend % rvBetaV, tTrend % rvBetaW, tTrend % rvBetaT
		print *, "S2eps:  ", tTrend % rvS2epsU, tTrend % rvS2epsV, tTrend % rvS2epsW, tTrend % rvS2epsT
		print *, "S2alpha:", tTrend % rvS2alphaU, tTrend % rvS2alphaV, tTrend % rvS2alphaW, tTrend % rvS2alphaT
		print *, "S2beta: ", tTrend % rvS2betaU, tTrend % rvS2betaV, tTrend % rvS2betaW, tTrend % rvS2betaT
		print *
		
		! Test 25: as Test 23, but with 1800s averaging time instead of 3600, and one invalid value
		rvU(1800) = NaN
		iRetCode = tSonic % buildFromVectors(rvTimeSt, rvU, rvV, rvW, rvTemp)
		iRetCode = tSonic % removeTrend( &
			1800, &
			tTrend &
		)
		print *, "Test 25: Detrending, with synthetic data having known characteristics"
		print *
		print *, "Number of data:      ", tTrend % ivNumData
		print *, "Alpha U (intercept): ", tTrend % rvAlphaU,   "  (expected:   0.000)"
		print *, "Beta U  (slope):     ", tTrend % rvBetaU,    "  (expected:   1.000)"
		print *, "S2eps U:             ", tTrend % rvS2epsU
		print *, "S2alpha U:           ", tTrend % rvS2alphaU
		print *, "S2beta U:            ", tTrend % rvS2betaU
		print *
		print *, "Alphas: ", tTrend % rvAlphaU, tTrend % rvAlphaV, tTrend % rvAlphaW, tTrend % rvAlphaT
		print *, "Betas:  ", tTrend % rvBetaU, tTrend % rvBetaV, tTrend % rvBetaW, tTrend % rvBetaT
		print *, "S2eps:  ", tTrend % rvS2epsU, tTrend % rvS2epsV, tTrend % rvS2epsW, tTrend % rvS2epsT
		print *, "S2alpha:", tTrend % rvS2alphaU, tTrend % rvS2alphaV, tTrend % rvS2alphaW, tTrend % rvS2alphaT
		print *, "S2beta: ", tTrend % rvS2betaU, tTrend % rvS2betaV, tTrend % rvS2betaW, tTrend % rvS2betaT
		print *
		
		! Test 26: as Test 23, but with 1800s averaging time instead of 3600, and one invalid value + time stamp
		rvU(1800) = NaN
		rvTimeSt(1800) = NaN_8
		iRetCode = tSonic % buildFromVectors(rvTimeSt, rvU, rvV, rvW, rvTemp)
		iRetCode = tSonic % removeTrend( &
			1800, &
			tTrend &
		)
		print *, "Test 26: Detrending, with synthetic data having known characteristics"
		print *
		print *, "Number of data:      ", tTrend % ivNumData
		print *, "Alpha U (intercept): ", tTrend % rvAlphaU,   "  (expected:   0.000)"
		print *, "Beta U  (slope):     ", tTrend % rvBetaU,    "  (expected:   1.000)"
		print *, "S2eps U:             ", tTrend % rvS2epsU
		print *, "S2alpha U:           ", tTrend % rvS2alphaU
		print *, "S2beta U:            ", tTrend % rvS2betaU
		print *
		print *, "Alphas: ", tTrend % rvAlphaU, tTrend % rvAlphaV, tTrend % rvAlphaW, tTrend % rvAlphaT
		print *, "Betas:  ", tTrend % rvBetaU, tTrend % rvBetaV, tTrend % rvBetaW, tTrend % rvBetaT
		print *, "S2eps:  ", tTrend % rvS2epsU, tTrend % rvS2epsV, tTrend % rvS2epsW, tTrend % rvS2epsT
		print *, "S2alpha:", tTrend % rvS2alphaU, tTrend % rvS2alphaV, tTrend % rvS2alphaW, tTrend % rvS2alphaT
		print *, "S2beta: ", tTrend % rvS2betaU, tTrend % rvS2betaV, tTrend % rvS2betaW, tTrend % rvS2betaT
		print *
		
	end subroutine tst_SonicData
	
end program t_pbl_wind
