! pbl_wind - Fortran module, containing routines related to wind
!            in the Planetary Boundary Layer.
!
! This module is part of the pbl_met library.
!
! Copyright 2018 by Servizi Territorio srl
!
! This is open-source code, covered by the lGPL 3.0 license.
!
module pbl_wind

	use pbl_base
	use pbl_time
	
	implicit none
	
	private
	
	! Public interface
	! 0. Useful constants and symbols
	public	:: WCONV_SAME
	public	:: WCONV_PROVENANCE_TO_FLOW
	public	:: WCONV_FLOW_TO_PROVENANCE
	public	:: WDCLASS_ZERO_CENTERED
	public	:: WDCLASS_ZERO_BASED
	! 1. Conventions
	public	:: PolarToCartesian2
	public	:: PolarToCartesian3
	public	:: CartesianToPolar2
	public	:: CartesianToPolar3
	! 2. Classification
	public	:: ClassVel
	public	:: ClassDir
	! 3. Descriptive statistics
	public	:: VectorDirVel
	public	:: ScalarVel
	public	:: UnitDir
	public	:: WindRose
	public	:: CompareWindRoses
	public	:: VelDirMean
	public	:: VelMean
	public	:: DirMean
	! 4. Data types
	public	:: SonicData
	public	:: EddyCovData
	
	! Public constants
	integer, parameter	:: WCONV_SAME               = 0
	integer, parameter	:: WCONV_PROVENANCE_TO_FLOW = 1
	integer, parameter	:: WCONV_FLOW_TO_PROVENANCE = 2
	integer, parameter	:: WDCLASS_ZERO_CENTERED    = 0
	integer, parameter	:: WDCLASS_ZERO_BASED       = 1
	
	! Internal constants
	real, parameter		:: Pi = 3.1415927
	real, parameter		:: ToRad = Pi/180.
	real, parameter		:: ToDeg = 180./Pi
	
	! Polymorphic interfaces
	
	interface ClassVel
		module procedure ClassVelScalar
		module procedure ClassVelVector
	end interface ClassVel
	
	interface ClassDir
		module procedure ClassDirScalar
		module procedure ClassDirVector
	end interface ClassDir
	
	! Data types
	
	type SonicData
		logical, private							:: isValid
		real(8), dimension(:), allocatable, private	:: rvTimeStamp
		real, dimension(:), allocatable, private	:: rvU
		real, dimension(:), allocatable, private	:: rvV
		real, dimension(:), allocatable, private	:: rvW
		real, dimension(:), allocatable, private	:: rvT
	contains
		procedure	:: buildFromVectors	=> sd_BuildFromVectors
		procedure	:: readSonicLib		=> sd_ReadSonicLib
		procedure	:: size				=> sd_Size
		procedure	:: valid			=> sd_Valid
		procedure	:: averages			=> sd_Averages
	end type SonicData
	
	type EddyCovData
		! Status section
		logical, private								:: isPrimed			! .true. when "averages" are available
		logical, private								:: isFilled			! .true. when eddy covariance data are available
		integer, private								:: averagingTime	! Averaging time, in seconds
		! Common-to-all data
		real(8), dimension(:), allocatable, private		:: rvTimeStamp		! Time stamp averages
		integer, dimension(:), allocatable, private		:: ivNumData		! Number of (valid) data having contributed to the "averages"
		! Input section (data entering here through SonicData % averages(...) member function
		real, dimension(:,:), allocatable, private		:: rmVel			! Time series of mean velocities (m/s)
		real, dimension(:), allocatable, private		:: rvT				! Time series of mean temperatures (°C)
		real, dimension(:,:,:), allocatable, private	:: raCovVel			! Time series of momentum covariances (m2/s2)
		real, dimension(:,:), allocatable, private		:: rmCovT			! Time series of covariances between velocities and temperature (m°C/s)
		real, dimension(:), allocatable, private		:: rvVarT			! Time series of temperature variances (°C2)
		! Output section (data entering here through EddyCovData % process(...) member function
		! 1) Basic, rotated
		real, dimension(:), allocatable, private		:: rvTheta			! Time series of first rotation angles (°)
		real, dimension(:), allocatable, private		:: rvPhi			! Time series of second rotation angles (°)
		real, dimension(:), allocatable, private		:: rvPsi			! Time series of third rotation angles (°) (always 0 if two rotations selected in eddy covariance)
		real, dimension(:,:), allocatable, private		:: rmRotVel			! Time series of rotated mean velocities (m/s)
		real, dimension(:,:,:), allocatable, private	:: raRotCovVel		! Time series of rotated momentum covariances (m2/s2)
		real, dimension(:,:), allocatable, private		:: rmRotCovT		! Time series of rotated covariances between velocities and temperature (m°C/s)
		! 2) Derived, pre eddy-covariance
		! 3) Derived, common turbulence indicators
	contains
		procedure	:: clean			=> ec_Clean					! Make an EddyCovData object "clean", that is, with vectors unallocated and status logicals .false.
		procedure	:: reserve	 		=> ec_Allocate				! Reserve workspace for vectors (all types)
		procedure	:: dump				=> ec_Dump					! Print contents of an EddyCovData object to screen (mainly for testing)
		procedure	:: getSize			=> ec_getSize				! Get allocated size of an EddyCovData object
		procedure	:: getAvgTime		=> ec_getAvgTime			! Get averaging time (as it is)
		procedure	:: getNumValidInput	=> ec_getNumValidInput		! Count number of valid data in an EddyCovData object
		procedure	:: getInputData		=> ec_getInputData			! Get a copy of input vectors
		procedure	:: createEmpty		=> ec_CreateEmpty			! Create an empty EddyCovData object, that is, with allocated vectors but .false. status logicals; mainly for multi-hour e.c. sets
		procedure	:: isClean			=> ec_IsClean				! Check whether an EddyCovData object is clean
		procedure	:: isEmpty			=> ec_IsEmpty				! Check whether an EddyCovData object is empty
		procedure	:: isReady			=> ec_IsPrimed				! Check whether an EddyCovData object is primed (contains some input)
		procedure	:: isFull			=> ec_IsFilled				! Check whether an EddyCovData object is primed (contains some input and processed data)
		procedure	:: isHourly			=> ec_IsHourly				! Check an EddyCovData object is hourly, or not
		procedure	:: getTimeStamp		=> ec_GetTimeStamp			! Retrieve a copy of the object's internal time stamp
		procedure	:: add				=> ec_AddHourly				! Add a hourly EddyCovData object to an existing multi-hourly one
	end type EddyCovData
	
contains

	function PolarToCartesian2(polar, interpretation) result(cartesian)
	
		! Routine arguments
		real, dimension(2), intent(in)	:: polar				! Wind in polar form (vel=polar(1), dir=polar(2))
		real, dimension(2)				:: cartesian			! Wind in cartesian form (u=cartesian(1), v=cartesian(2))
		integer, intent(in), optional	:: interpretation
		
		! Locals
		! --none--
		
		! Convert, taking the desired convention into account
		if(polar(1) > 1.e-6) then
			if(present(interpretation)) then
				if(interpretation == WCONV_SAME) then
					! Same interpretation for input and output: no sign change
					call uvWind(polar(1), polar(2), cartesian(1), cartesian(2))
				elseif(interpretation == WCONV_PROVENANCE_TO_FLOW .or. interpretation == WCONV_FLOW_TO_PROVENANCE) then
					! Different interpretation for input and output: sign change
					call uvWind(polar(1), polar(2) - 180., cartesian(1), cartesian(2))
				else
					! Wrong convention
					cartesian = [NaN, NaN]
				end if
			else
				! Same interpretation for input and output: no sign change
				call uvWind(polar(1), polar(2), cartesian(1), cartesian(2))
			end if
		else
			cartesian = [NaN, NaN]
		end if
		
	end function PolarToCartesian2
	

	function PolarToCartesian3(polar, interpretation) result(cartesian)
	
		! Routine arguments
		real, dimension(3), intent(in)	:: polar				! Wind in polar form (vel=polar(1), dir=polar(2), w=polar(3))
		real, dimension(3)				:: cartesian			! Wind in cartesian form (u=cartesian(1), v=cartesian(2), w=cartesian(3))
		integer, intent(in), optional	:: interpretation
		
		! Locals
		! --none--
		
		! Convert, taking the desired convention into account
		if(polar(1) > 1.e-6) then
			if(present(interpretation)) then
				if(interpretation == WCONV_SAME) then
					! Same interpretation for input and output: no sign change
					call uvWind(polar(1), polar(2), cartesian(1), cartesian(2))
					cartesian(3) = polar(3)
				elseif(interpretation == WCONV_PROVENANCE_TO_FLOW .or. interpretation == WCONV_FLOW_TO_PROVENANCE) then
					! Different interpretation for input and output: sign change
					call uvWind(polar(1), polar(2) - 180., cartesian(1), cartesian(2))
					cartesian(3) = polar(3)
				else
					! Wrong convention
					cartesian = [NaN, NaN, polar(3)]
				end if
			else
				! Same interpretation for input and output: no sign change
				call uvWind(polar(1), polar(2), cartesian(1), cartesian(2))
			end if
		else
			cartesian = [NaN, NaN, polar(3)]
		end if
		
	end function PolarToCartesian3
	

	function CartesianToPolar2(cartesian, interpretation) result(polar)
	
		! Routine arguments
		real, dimension(2), intent(in)	:: cartesian			! Wind in cartesian form (u=cartesian(1), v=cartesian(2))
		real, dimension(2)				:: polar				! Wind in polar form (vel=polar(1), dir=polar(2))
		integer, intent(in), optional	:: interpretation
		
		! Locals
		! --none--
		
		! Convert, taking the desired convention into account
		if(dot_product(cartesian, cartesian) > 1.e-6) then
			if(present(interpretation)) then
				if(interpretation == WCONV_SAME) then
					! Same interpretation for input and output: no sign change
					call veldirWind(cartesian(1), cartesian(2), polar(1), polar(2))
				elseif(interpretation == WCONV_PROVENANCE_TO_FLOW .or. interpretation == WCONV_FLOW_TO_PROVENANCE) then
					! Different interpretation for input and output: sign change
					call veldirWind(-cartesian(1), -cartesian(2), polar(1), polar(2))
				else
					! Wrong convention
					polar = [NaN, NaN]
				end if
			else
				! Same interpretation for input and output: no sign change
				call veldirWind(cartesian(1), cartesian(2), polar(1), polar(2))
			end if
		else
			polar = [0., NaN]
		end if
		
	end function CartesianToPolar2
	
	
	function CartesianToPolar3(cartesian, interpretation) result(polar)
	
		! Routine arguments
		real, dimension(3), intent(in)	:: cartesian			! Wind in cartesian form (u=cartesian(1), v=cartesian(2), w=cartesian(3))
		real, dimension(3)				:: polar				! Wind in polar form (vel=polar(1), dir=polar(2), w=polar(3))
		integer, intent(in), optional	:: interpretation
		
		! Locals
		! --none--
		
		! Convert, taking the desired convention into account
		if(dot_product(cartesian(1:2), cartesian(1:2)) > 1.e-6) then
			if(present(interpretation)) then
				if(interpretation == WCONV_SAME) then
					! Same interpretation for input and output: no sign change
					call veldirWind(cartesian(1), cartesian(2), polar(1), polar(2))
					polar(3) = cartesian(3)
				elseif(interpretation == WCONV_PROVENANCE_TO_FLOW .or. interpretation == WCONV_FLOW_TO_PROVENANCE) then
					! Different interpretation for input and output: sign change
					call veldirWind(-cartesian(1), -cartesian(2), polar(1), polar(2))
					polar(3) = cartesian(3)
				else
					! Wrong convention
					polar = [NaN, NaN, cartesian(3)]
				end if
			else
				! Same interpretation for input and output: no sign change
				call veldirWind(cartesian(1), cartesian(2), polar(1), polar(2))
			end if
		else
			polar = [0., NaN, cartesian(3)]
		end if
		
	end function CartesianToPolar3
	
	
	function ClassVelScalar(vel, rvVel) result(iClass)
		
		! Routine arguments
		real, intent(in)				:: vel			! Wind speed to classify
		real, dimension(:), intent(in)	:: rvVel		! Vector, containing upper class limits in increasing order
		integer							:: iClass		! Speed class to which the wind belongs (-9999 if not assignable)
		
		! Locals
		integer		:: n
		integer		:: i
		
		! Check class limit validity
		if(size(rvVel) <= 0.) then
			iClass = -9999
			return
		end if
		if(all(.invalid.rvVel)) then
			iClass = -9999
			return
		end if
		
		! Check something is to be made: leave, if not
		if(.invalid.vel) then
			iClass = -9999
			return
		end if
		
		! Check added on input vector size
		if(size(rvVel) <= 0) then
			iClass = -9999
			return
		end if
		
		! Perform a simple table lookup
		n = size(rvVel)
		do i = 1, n
			if(vel <= rvVel(i)) then
				iClass = i
				return
			end if
		end do
		
		! Execution reaches this point if no match is found, so
		iClass = n + 1

	end function ClassVelScalar


	function ClassVelVector(vel, rvVel) result(ivClass)
		
		! Routine arguments
		real, dimension(:), intent(in)	:: vel			! Wind speed to classify
		real, dimension(:), intent(in)	:: rvVel		! Vector, containing upper class limits in increasing order
		integer, dimension(size(vel))	:: ivClass		! Speed class to which the wind belongs (-9999 if not assignable)
		
		! Locals
		integer		:: n
		integer		:: i, j
		
		! Check class limit validity
		if(size(rvVel) <= 0.) then
			ivClass = -9999
			return
		end if
		if(all(.invalid.rvVel)) then
			ivClass = -9999
			return
		end if
		
		! Main loop: iterate over speed values
		do j = 1, size(vel)
		
			! Check class can be assigned
			if(.invalid.vel(j)) then
				ivClass(j) = -9999
			else
			
				! Perform a simple table lookup
				n = size(rvVel)
				ivClass(j) = n + 1
				do i = 1, n
					if(vel(j) <= rvVel(i)) then
						ivClass(j) = i
						exit
					end if
				end do
			
			end if
			
		end do

	end function ClassVelVector


	function ClassDirScalar(dir, iNumClasses, iClassType) result(iClass)
	
		! Routine arguments
		real, intent(in)				:: dir				! Wind direction to classify (°)
		integer, intent(in)				:: iNumClasses		! Number of desired classes
		integer, intent(in), optional	:: iClassType		! Class type (WDCLASS_ZERO_CENTERED (default): first class is zero-centered; WDCLASS_ZERO_BASED: first class starts at zero)
		integer							:: iClass			! Direction class to which the wind belongs (-9999 if no class is assignable)
		
		! Locals
		real	:: classWidth
		real	:: d
		integer	:: iClsType
		
		! Check something is to be made: leave, if not
		if(isnan(dir)) then
			iClass = -9999
			return
		end if
		
		! If missing 'iClassType' assign default, otherwise get it
		if(present(iClassType)) then
			iClsType = iClassType
		else
			iClsType = WDCLASS_ZERO_CENTERED
		end if
		
		! Compute the fixed-size class width, and in case of zero-centere classes use it to adjust direction
		if(iNumClasses <= 0) then
			iClass = -9999
			return
		end if
		classWidth = 360. / iNumClasses
		d = dir
		if(iClsType == WDCLASS_ZERO_CENTERED) d = d + classWidth / 2.
		
		! Adjust wind direction to the range 0-360
		d = mod(d, 360.)
		if(d < 0.) d = d + 360.
		
		! Assign class by division
		iClass = floor(d / classWidth) + 1
		
	end function ClassDirScalar
	

	function ClassDirVector(dir, iNumClasses, iClassType) result(ivClass)
	
		! Routine arguments
		real, dimension(:), intent(in)	:: dir				! Wind direction to classify (°)
		integer, intent(in)				:: iNumClasses		! Number of desired classes
		integer, intent(in), optional	:: iClassType		! Class type (WDCLASS_ZERO_CENTERED (default): first class is zero-centered; WDCLASS_ZERO_BASED: first class starts at zero)
		integer, dimension(size(dir))	:: ivClass			! Direction class to which the wind belongs (-9999 if no class is assignable)
		
		! Locals
		real						:: classWidth
		real, dimension(size(dir))	:: d
		integer						:: iClsType
		
		! Check something is to be made: leave, if not
		if(iNumClasses <= 0) then
			ivClass = -9999
			return
		end if
		
		! If missing 'iClassType' assign default, otherwise get it
		if(present(iClassType)) then
			iClsType = iClassType
		else
			iClsType = WDCLASS_ZERO_CENTERED
		end if
		
		! Compute the fixed-size class width, and in case of zero-centere classes use it to adjust direction
		classWidth = 360. / iNumClasses
		d = dir
		if(iClsType == WDCLASS_ZERO_CENTERED) d = d + classWidth / 2.
		where(isnan(d))
		
			ivClass = -9999
			
		elsewhere
		
			! Adjust wind direction to the range 0-360
			d = mod(d, 360.)
			where(d < 0.)
				d = d + 360.
			endwhere
		
			! Assign class by division
			ivClass = floor(d / classWidth) + 1
			
		end where
		
	end function ClassDirVector
	
	
	function VectorDirVel(rvVel, rvDir) result(polar)
	
		! Routine arguments
		real, dimension(:), intent(in)	:: rvVel
		real, dimension(:), intent(in)	:: rvDir
		real, dimension(2)				:: polar		! [vel,dir]
		
		! Locals
		real, dimension(size(rvVel))	:: rvU
		real, dimension(size(rvVel))	:: rvV
		integer							:: n
		real							:: rU
		real							:: rV
		
		! Check input parameters make sense
		if(size(rvVel) /= size(rvDir)) then
			polar = [NaN, NaN]
			return
		end if
		
		! Transform horizontal wind from polar to Cartesian form. In this case
		! it is irrelevant whether the wind directio interpretation is of
		! flow or provenance convention: the transformed vectors will be
		! back-transformed by the same interpretation.
		rvU = rvVel * sin(rvDir * ToRad)
		rvV = rvVel * cos(rvDir * ToRad)
		
		! Compute the Cartesian average of wind vectors
		n = count(.not.isnan(rvU))
		if(n > 0) then
			! At least one element: compute the vector mean
			rU = sum(rvU, mask=.not.isnan(rvU)) / n
			rV = sum(rvV, mask=.not.isnan(rvU)) / n
		else
			rU = NaN
			rV = NaN
		end if
		
		! Convert the Cartesian average to mean wind in polar form
		polar = [sqrt(rU**2 + rV**2), atan2(rU,rV)*ToDeg]
		if(polar(2) < 0) polar(2) = polar(2) + 360.0
		
	end function VectorDirVel
	
	
	function ScalarVel(rvVel) result(vel)
	
		! Routine arguments
		real, dimension(:), intent(in)	:: rvVel
		real							:: vel
		
		! Locals
		integer	:: n
		
		! Compute the wind scalar speed
		n = count(.not.isnan(rvVel))
		if(n > 0) then
			! At least one element: compute the scalar mean
			vel = sum(rvVel, mask=.not.isnan(rvVel)) / n
		else
			vel = NaN
		end if
		
	end function ScalarVel
	

	function UnitDir(rvDir) result(dir)
	
		! Routine arguments
		real, dimension(:), intent(in)	:: rvDir
		real							:: dir
		
		! Locals
		real, dimension(size(rvDir))	:: rvU
		real, dimension(size(rvDir))	:: rvV
		integer							:: n
		real							:: rU
		real							:: rV
		
		! Transform horizontal wind from polar to Cartesian form. In this case
		! it is irrelevant whether the wind directio interpretation is of
		! flow or provenance convention: the transformed vectors will be
		! back-transformed by the same interpretation.
		rvU = sin(rvDir * ToRad)
		rvV = cos(rvDir * ToRad)
		
		! Compute the Cartesian average of wind vectors
		n = count(.not.isnan(rvU))
		if(n > 0) then
			! At least one element: compute the vector mean
			rU = sum(rvU, mask=.not.isnan(rvU)) / n
			rV = sum(rvV, mask=.not.isnan(rvU)) / n
		else
			rU = NaN
			rV = NaN
		end if
		
		! Convert the Cartesian average to mean wind in polar form
		dir = atan2(rU,rV)*ToDeg
		if(dir < 0.) dir = dir + 360.
		
	end function UnitDir
	
	
	! Compute the joint frequency function of wind speed and direction, that is, the
	! "wind rose" as it is commonly named.
	!
	! In principle, in place of wind speed any other scalar may be used - for example
	! a pollutant concentration: in this case a, say, concentration rose is obtained.
	!
	! The graphical rendering of the wind (concentration , ...) rose is not among
	! the scopes of pbl_met, but nevertheless you may find excellent routines and
	! packages to accomplish this task in the open source.
	function WindRose(vel, dir, rvVel, iNumClasses, iClassType, rmWindRose) result(iRetCode)
	
		! Routine arguments
		real, dimension(:), intent(in)					:: vel			! Wind speed observations (m/s)
		real, dimension(:), intent(in)					:: dir			! Wind direction observations (°)
		real, dimension(:), intent(in)					:: rvVel		! Wind speed class limits as in ClassVel (m/s)
		integer, intent(in)								:: iNumClasses	! Number of direction classes as in ClassDir
		integer, intent(in), optional					:: iClassType	! Type of direction classes as in ClassDir (WDCLASS_ZERO_CENTERED (default), or WDCLASS_ZERO_BASED)
		real, dimension(:,:), allocatable, intent(out)	:: rmWindRose	! Joint frequency table of wind speed and direction, aka "wind rose" (in tabular form) 
		integer											:: iRetCode
		
		! Locals
		integer, dimension(size(vel))	:: ivVelClass
		integer, dimension(size(dir))	:: ivDirClass
		integer							:: i
		integer							:: m
		integer							:: n
		integer							:: l
		real							:: rTotal
		integer							:: iDirClassType
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check data vectors have the same size
		l = size(vel)
		if(size(dir) /= l) then
			iRetCode = 1
			return
		end if
		
		! Check classification parameters, and use it to reserve workspace if correct
		if(size(rvVel) <= 0) then
			iRetCode = 2
			return
		end if
		m = size(rvVel) + 1
		if(iNumClasses <= 0) then
			iRetCode = 3
			return
		end if
		n = iNumClasses
		if(allocated(rmWindRose)) deallocate(rmWindRose)
		allocate(rmWindRose(m,n))
		
		! Clean up, and check the call makes sense
		rmWindRose = 0.
		
		! Get direction class type
		if(present(iClassType)) then
			iDirClassType = iClassType
		else
			iDirClassType = WDCLASS_ZERO_CENTERED
		end if
		
		! Classify wind speed and direction
		ivVelClass = ClassVelVector(vel, rvVel)
		ivDirClass = ClassDirVector(dir, iNumClasses, iDirClassType)
		
		! Count occurrences in any class
		do i = 1, size(vel)
			if(ivVelClass(i) > 0 .and. ivDirClass(i) > 0) &
				rmWindRose(ivVelClass(i),ivDirClass(i)) = rmWindRose(ivVelClass(i),ivDirClass(i)) + 1.
		end do
		
		! Convert counts to frequency
		rTotal = sum(rmWindRose)	! That is, number of valid data
		if(rTotal > 0.) rmWindRose = rmWindRose / rTotal
		
	end function WindRose
	

	function CompareWindRoses( &
		vel1, dir1, &
		vel2, dir2, &
		rvVel, iNumClasses, iClassType, &
		rmWindRose1, rmWindRose2, &
		rProb, rChiSquareOut, iDegreesOfFreedomOut &
	) result(iRetCode)
	
		! Routine arguments
		real, dimension(:), intent(in)					:: vel1					! First wind speed observations (m/s)
		real, dimension(:), intent(in)					:: dir1					! First wind direction observations (°)
		real, dimension(:), intent(in)					:: vel2					! Second wind speed observations (m/s)
		real, dimension(:), intent(in)					:: dir2					! Second wind direction observations (°)
		real, dimension(:), intent(in)					:: rvVel				! Wind speed class limits as in ClassVel (m/s)
		integer, intent(in)								:: iNumClasses			! Number of direction classes as in ClassDir
		integer, intent(in), optional					:: iClassType			! Type of direction classes as in ClassDir (WDCLASS_ZERO_CENTERED (default), or WDCLASS_ZERO_BASED)
		real, dimension(:,:), allocatable, intent(out)	:: rmWindRose1			! First joint frequency table of wind speed and direction, aka "wind rose" (in tabular form) 
		real, dimension(:,:), allocatable, intent(out)	:: rmWindRose2			! Second joint frequency table of wind speed and direction, aka "wind rose" (in tabular form) 
		real, intent(out)								:: rProb				! Probability associated with Chi-square equality of distribution test, applied to the two wind roses
		real, intent(out), optional						:: rChiSquareOut		! Chi square sum
		integer, intent(out), optional					:: iDegreesOfFreedomOut	! Chi square degrees of freedom
		integer											:: iRetCode
		
		! Locals
		integer	:: i, j
		integer	:: iErrCode
		integer	:: iDegreesOfFreedom
		real	:: rChiSquare
		real	:: R, S, RS, SR
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Compute the two wind roses with respect to the same classes
		iErrCode = WindRose(vel1, dir1, rvVel, iNumClasses, iClassType, rmWindRose1)
		if(iErrCode /= 0) then
			if(allocated(rmWindRose1)) deallocate(rmWindRose1)
			if(allocated(rmWindRose2)) deallocate(rmWindRose2)
			rProb = NaN
			iRetCode = 1
			return
		end if
		iErrCode = WindRose(vel2, dir2, rvVel, iNumClasses, iClassType, rmWindRose2)
		if(iErrCode /= 0) then
			if(allocated(rmWindRose1)) deallocate(rmWindRose1)
			if(allocated(rmWindRose2)) deallocate(rmWindRose2)
			rProb = NaN
			iRetCode = 1
			return
		end if
		
		! Rescale the wind roses from fraction to count form
		rmWindRose1 = rmWindRose1 * size(vel1)
		rmWindRose2 = rmWindRose2 * size(vel2)
		
		! Compute the coefficients adjusting for different wind vector sizes
		R  = real(size(vel1))
		S  = real(size(vel2))
		RS = sqrt(R/S)
		SR = sqrt(S/R)
		
		! Compute chi-square variable
		iDegreesOfFreedom = -1	! Take into account the normalization made on wind roses expressed as fraction
		rChiSquare        =  0.
		do i = 1, size(rmWindRose1, dim=1)
			do j = 1, size(rmWindRose1, dim=2)
				if(rmWindRose1(i,j) > 0. .or. rmWindRose2(i,j) > 0.) then
					iDegreesOfFreedom = iDegreesOfFreedom + 1
					rChiSquare = rChiSquare + (SR*rmWindRose1(i,j) - RS*rmWindRose2(i,j))**2 / (rmWindRose1(i,j) + rmWindRose2(i,j))
				end if
			end do
		end do
		
		! Calculate probability
		rProb = 1. - gammaP(0.5*iDegreesOfFreedom, 0.5*rChiSquare)
		
		! Return auxiliary quantities, if desired
		if(present(rChiSquareOut))        rChiSquareOut = rChiSquare
		if(present(iDegreesOfFreedomOut)) iDegreesOfFreedomOut = iDegreesOfFreedom
		
	end function CompareWindRoses
	

	function VelDirMean(vel, dir, scalar, rvVel, iNumClasses, iClassType, rmMean) result(iRetCode)
	
		! Routine arguments
		real, dimension(:), intent(in)					:: vel			! Wind speed observations (m/s)
		real, dimension(:), intent(in)					:: dir			! Wind direction observations (°)
		real, dimension(:), intent(in)					:: scalar		! Any scalar quantity (any unit; invalid values as NaN)
		real, dimension(:), intent(in)					:: rvVel		! Wind speed class limits as in ClassVel (m/s)
		integer, intent(in)								:: iNumClasses	! Number f direction classes as in ClassDir
		integer, intent(in)								:: iClassType	! Type of direction classes as in ClassDir
		real, dimension(:,:), allocatable, intent(out)	:: rmMean		! Mean of scalar according to wind speed and direction classes
		integer											:: iRetCode
		
		! Locals
		integer, dimension(size(vel))			:: ivVelClass
		integer, dimension(size(dir))			:: ivDirClass
		integer									:: i
		integer, dimension(:,:), allocatable	:: imNumValues
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Clean up, and check the call makes sense
		if(size(dir) <= 0 .or. size(vel) <= 0 .or. size(scalar) <= 0) then
			iRetCode = 1
			return
		end if
		if(size(dir) /= size(vel) .or. size(scalar) /= size(vel)) then
			iRetCode = 2
			return
		end if
		if(size(rvVel) <= 0 .or. iNumClasses <= 0) then
			iRetCode = 3
			return
		end if
		if(allocated(rmMean)) deallocate(rmMean)
		allocate(rmMean(size(rvVel)+1,iNumClasses))
		allocate(imNumValues(size(rvVel)+1,iNumClasses))
		
		! Classify wind speed and direction
		ivVelClass = ClassVelVector(vel, rvVel)
		ivDirClass = ClassDirVector(dir, iNumClasses, iClassType)
		
		! Count occurrences in any class
		rmMean      = 0.
		imNumValues = 0
		do i = 1, size(vel)
			if(ivVelClass(i) > 0 .and. ivDirClass(i) > 0 .and. (.not.isnan(scalar(i)))) then
				rmMean(ivVelClass(i),ivDirClass(i))      = rmMean(ivVelClass(i),ivDirClass(i)) + scalar(i)
				imNumValues(ivVelClass(i),ivDirClass(i)) = imNumValues(ivVelClass(i),ivDirClass(i)) + 1
			end if
		end do
		
		! Convert counts to means
		where(imNumValues > 0)
			rmMean = rmMean / imNumValues
		elsewhere
			rmMean = NaN
		end where
		
	end function VelDirMean


	function VelMean(vel, scalar, rvVel) result(rvMean)
	
		! Routine arguments
		real, dimension(:), intent(in)	:: vel			! Wind speed observations (m/s)
		real, dimension(:), intent(in)	:: scalar		! Any scalar quantity (any unit; invalid values as NaN)
		real, dimension(:), intent(in)	:: rvVel		! Wind speed class limits as in ClassVel (m/s)
		real, dimension(size(rvVel)+1)	:: rvMean		! Mean of scalar according to wind speed and direction classes 
		
		! Locals
		integer, dimension(size(vel))		:: ivVelClass
		integer, dimension(size(rvVel)+1)	:: ivNumValues
		integer								:: i
		
		! Clean up, and check the call makes sense
		rvMean = NaN
		ivNumValues = 0
		if(size(scalar) /= size(vel)) return
		
		! Classify wind speed and direction
		ivVelClass = ClassVelVector(vel, rvVel)
		
		! Count occurrences in any class
		rvMean = 0.
		do i = 1, size(vel)
			if(ivVelClass(i) > 0 .and. (.not.isnan(scalar(i)))) then
				rvMean(ivVelClass(i)) = rvMean(ivVelClass(i)) + scalar(i)
				ivNumValues(ivVelClass(i)) = ivNumValues(ivVelClass(i)) + 1
			end if
		end do
		
		! Convert counts to means
		where(ivNumValues > 0)
			rvMean = rvMean / ivNumValues
		elsewhere
			rvMean = NaN
		end where
		
	end function VelMean


	function DirMean(dir, scalar, iNumClasses, iClassType) result(rvMean)
	
		! Routine arguments
		real, dimension(:), intent(in)	:: dir			! Wind direction observations (°)
		real, dimension(:), intent(in)	:: scalar		! Any scalar quantity (any unit; invalid values as NaN)
		integer, intent(in)				:: iNumClasses	! Number f direction classes as in ClassDir
		integer, intent(in)				:: iClassType	! Type of direction classes as in ClassDir
		real, dimension(iNumClasses)	:: rvMean		! Mean of scalar according to wind speed and direction classes 
		
		! Locals
		integer, dimension(size(dir))		:: ivDirClass
		integer, dimension(iNumClasses)		:: ivNumValues
		integer								:: i
		
		! Clean up, and check the call makes sense
		rvMean = NaN
		ivNumValues = 0
		if(size(scalar) /= size(dir)) return
		
		! Classify wind speed and direction
		ivDirClass = ClassDirVector(dir, iNumClasses, iClassType)
		
		! Count occurrences in any class
		rvMean = 0.
		do i = 1, size(dir)
			if(ivDirClass(i) > 0 .and. (.not.isnan(scalar(i)))) then
				rvMean(ivDirClass(i)) = rvMean(ivDirClass(i)) + scalar(i)
				ivNumValues(ivDirClass(i)) = ivNumValues(ivDirClass(i)) + 1
			end if
		end do
		
		! Convert counts to means
		where(ivNumValues > 0)
			rvMean = rvMean / ivNumValues
		elsewhere
			rvMean = NaN
		end where
		
	end function DirMean
	
	
	function sd_BuildFromVectors(this, rvTimeStamp, rvU, rvV, rvW, rvT) result(iRetCode)
	
		! Routine arguments
		class(SonicData), intent(out)		:: this
		real(8), dimension(:), intent(in)	:: rvTimeStamp	! Time stamp, in Epoch new form
		real, dimension(:), intent(in)		:: rvU			! Eastward wind component (m/s)
		real, dimension(:), intent(in)		:: rvV			! Northward wind component (m/s)
		real, dimension(:), intent(in)		:: rvW			! Verticalward wind component (m/s)
		real, dimension(:), intent(in)		:: rvT			! Sonic temperature (°C)
		integer								:: iRetCode
		
		! Locals
		integer	::nstamp, nu, nv, nw, nt, n
		
		! Assume succes (will falsify on failure)
		iRetCode = 0
		
		! Get and check vector sizes
		nstamp = size(rvTimeStamp)
		nu     = size(rvU)
		nv     = size(rvV)
		nw     = size(rvW)
		nt     = size(rvT)
		if(maxval(abs(nstamp-[nu,nv,nw,nt])) > 0) then
			! Just a fancy way to pack all together the equal-size comparisons: if all are equal, then the differences
			! (componentwise) between [nu,nv,nw,nt] and nstamp are all zero, and then their absolute maximum is zero, too.
			! Conversely, if the absolute maximum is > 0 then at least one of the nu,nv,nw,nt differs from nstamp.
			! Notice in this case the routine does nothing on dthe SonicData structure, which is left unchanged.
			iRetCode = 1
			return
		end if
		n = nstamp
		! Post-condition: nstamp == nu == nv == nw == nt == n
		
		! Reserve workspace
		if(allocated(this % rvTimeStamp)) deallocate(this % rvTimeStamp)
		if(allocated(this % rvU))         deallocate(this % rvU)
		if(allocated(this % rvV))         deallocate(this % rvV)
		if(allocated(this % rvW))         deallocate(this % rvW)
		if(allocated(this % rvT))         deallocate(this % rvT)
		allocate(this % rvTimeStamp(n))
		allocate(this % rvU(n))
		allocate(this % rvV(n))
		allocate(this % rvW(n))
		allocate(this % rvT(n))
		
		! Assign values
		this % rvTimeStamp = rvTimeStamp
		this % rvU         = rvU
		this % rvV         = rvV
		this % rvW         = rvW
		this % rvT         = rvT
		this % isValid     = .true.
		
	end function sd_BuildFromVectors
	
	
	function sd_ReadSonicLib(this, iLUN, sFileName, iOS) result(iRetCode)
	
		! Routine arguments
		class(SonicData), intent(out)	:: this
		integer, intent(in)				:: iLUN
		character(len=*), intent(in)	:: sFileName
		integer, intent(in)				:: iOS
		integer							:: iRetCode
		
		! Locals
		integer				:: iErrCode
		logical				:: lExist
		character(len=15)	:: sBaseName
		character(len=64)	:: sBuffer
		integer				:: iPos
		type(DateTime)		:: tStamp
		real(8)				:: rTimeBase
		integer				:: iNumData
		integer				:: i
		integer				:: iYear, iMonth, iDay, iHour
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check file exists
		inquire(file=sFileName, exist=lExist)
		if(.not.lExist) then
			iRetCode = 1
			return
		end if
		
		! Check file name to be a valid SonicLib, and obtain its time base
		sBaseName = baseName(sFileName, iOS)
		if(len_trim(sBaseName) /= 15) then
			iRetCode = 2
			return
		end if
		read(sBaseName, "(i4,2i2,1x,i2)", iostat=iErrCode) iYear, iMonth, iDay, iHour
		if(iErrCode /= 0) then
			iRetCode = 3
			return
		end if
		open(iLUN, file=sBaseName, status='old', action='read', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 4
			close(iLUN)
			return
		end if
		tStamp = DateTime(iYear, iMonth, iDay, iHour, 0, 0.d0)
		rTimeBase = tStamp % toEpoch()
		
		! Count data and reserve workspace
		iNumData = -1	! Implicitly consider the 1-line header, not contributing to data count
		do
			read(iLUN, "(a)", iostat=iErrCode) sBuffer
			if(iErrCode /= 0) exit
			iNumData = iNumData + 1
		end do
		if(iNumData <= 0) then
			iRetCode = 5
			close(iLUN)
			return
		end if
		if(allocated(this % rvTimeStamp)) deallocate(this % rvTimeStamp)
		if(allocated(this % rvU)) deallocate(this % rvU)
		if(allocated(this % rvV)) deallocate(this % rvV)
		if(allocated(this % rvW)) deallocate(this % rvW)
		if(allocated(this % rvT)) deallocate(this % rvT)
		allocate(this % rvTimeStamp(iNumData))
		allocate(this % rvU(iNumData))
		allocate(this % rvV(iNumData))
		allocate(this % rvW(iNumData))
		allocate(this % rvT(iNumData))
		
		! Read actual data
		rewind(iLUN)
		read(iLUN, "(a)") sBuffer	! Skip header line
		do i = 1, iNumData
			read(iLUN, *) &
				this % rvTimeStamp(i), &
				this % rvU(i), &
				this % rvV(i), &
				this % rvW(i), &
				this % rvT(i)
		end do
		close(iLUN)
		
		! Shift the time stamps read (representing second) by the base time,
		! obtaining full time stamps
		this % rvTimeStamp = this % rvTimeStamp + rTimeBase
		
		! Inform users all was OK
		this % isValid = .true.
		
	end function sd_ReadSonicLib
	
	
	function sd_Size(this) result(iSize)
	
		! Routine arguments
		class(SonicData), intent(in)	:: this
		integer							:: iSize
		
		! Locals
		! --none--
		
		! Get the information desired
		if(this % isValid) then
			iSize = size(this % rvTimeStamp)
		else
			iSize = 0
		end if
		
	end function sd_Size
	
	
	function sd_Valid(this) result(iValid)
	
		! Routine arguments
		class(SonicData), intent(in)	:: this
		integer							:: iValid
		
		! Locals
		integer	:: i
		
		! Scan data set, and count all totally valid records
		iValid = 0
		do i = 1, size(this % rvTimeStamp)
			if( &
				.valid. this % rvTimeStamp(i) .and. &
				.valid. this % rvU(i) .and. &
				.valid. this % rvV(i) .and. &
				.valid. this % rvW(i) .and. &
				.valid. this % rvT(i) &
			) then
				iValid = iValid + 1
			end if
		end do
		
	end function sd_Valid
	
	
	function sd_Averages(this, iAveragingTime, tEc) result(iRetCode)
	
		! Routine arguments
		class(SonicData), intent(in)						:: this				! Current ultrasonic anemometer data set
		integer, intent(in)									:: iAveragingTime	! Averaging period (s, positive, proper divisor of 3600)
		type(EddyCovData), intent(out)						:: tEc				! Eddy covariance data, input fields only are output
		integer												:: iRetCode
		
		! Locals
		integer								:: iErrCode
		integer, dimension(:), allocatable	:: ivTimeIndex
		real(8), dimension(:), allocatable	:: rvAggregTimeStamp
		integer								:: i
		integer								:: iIndex
		integer								:: iMaxBlock
		integer								:: iNumBlocks
		real(8)								:: rBaseTime
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Pre-clean the EddyCovData object, so that we're sure anything bad happens results in a
		! defined state
		iErrCode = tEc % clean()
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		
		! Check something is to be done
		if(this % valid() <= 0) then
			iRetCode = 2
			return
		end if
		if(iAveragingTime <= 0 .or. mod(3600, iAveragingTime) /= 0) then
			iRetCode = 3
			return
		end if
		iNumBlocks = 3600 / iAveragingTime
		
		! Construct time-based index, and allocate workspace based on it
		iErrCode = timeLinearIndex(this % rvTimeStamp, iAveragingTime, ivTimeIndex, rvAggregTimeStamp)
		if(iErrCode /= 0) then
			iRetCode = 4
			return
		end if
		iMaxBlock = maxval(ivTimeIndex)
		if(iMaxBlock <= 0) then
			iRetCode = 5
			return
		end if
		if(tEc % reserve(iNumBlocks) /= 0) then
			iRetCode = 6
			return
		end if
		
		! Pre-assign time stamps
		rBaseTime = real(floor(minval(this % rvTimeStamp, mask=.valid. this % rvTimeStamp) / iAveragingTime, kind=8) &
						* iAveragingTime, kind=8)
		tEc % rvTimeStamp = [(rBaseTime + (i-1)*real(iAveragingTime, kind=8), i = 1, iNumBlocks)]
		
		! Compute the desired statistics
		! -1- Phase one: Accumulate
		tEc % ivNumData = 0
		tEc % rmVel     = 0.
		tEc % rvT       = 0.
		tEc % raCovVel  = 0.
		tEc % rmCovT    = 0.
		tEc % rvVarT    = 0.
		tEc % isPrimed  = .true.
		do i = 1, size(ivTimeIndex)
			if(ivTimeIndex(i) > 0) then
				iIndex = ivTimeIndex(i)
				if( &
					.valid. this % rvTimeStamp(i) .and. &
					.valid. this % rvU(i) .and. &
					.valid. this % rvV(i) .and. &
					.valid. this % rvW(i) .and. &
					.valid. this % rvT(i) &
				) then
					! Update count
					tEc % ivNumData(iIndex) = tEc % ivNumData(iIndex) + 1
					! Update first order accumulators
					tEc % rmVel(iIndex, 1)       = tEc % rmVel(iIndex, 1)       + this % rvU(i)
					tEc % rmVel(iIndex, 2)       = tEc % rmVel(iIndex, 2)       + this % rvV(i)
					tEc % rmVel(iIndex, 3)       = tEc % rmVel(iIndex, 3)       + this % rvW(i)
					tEc % rvT(iIndex)            = tEc % rvT(iIndex)            + this % rvT(i)
					! Update second order accumulators
					tEc % raCovVel(iIndex, 1, 1) = tEc % raCovVel(iIndex, 1, 1) + this % rvU(i) ** 2
					tEc % raCovVel(iIndex, 2, 2) = tEc % raCovVel(iIndex, 2, 2) + this % rvV(i) ** 2
					tEc % raCovVel(iIndex, 3, 3) = tEc % raCovVel(iIndex, 3, 3) + this % rvW(i) ** 2
					tEc % rvVarT(iIndex)         = tEc % rvVarT(iIndex)         + this % rvT(i) ** 2
					tEc % raCovVel(iIndex, 1, 2) = tEc % raCovVel(iIndex, 1, 2) + this % rvU(i) * this % rvV(i)
					tEc % raCovVel(iIndex, 1, 3) = tEc % raCovVel(iIndex, 1, 3) + this % rvU(i) * this % rvW(i)
					tEc % raCovVel(iIndex, 2, 3) = tEc % raCovVel(iIndex, 2, 3) + this % rvV(i) * this % rvW(i)
					tEc % rmCovT(iIndex, 1)      = tEc % rmCovT(iIndex, 1)      + this % rvU(i) * this % rvT(i)
					tEc % rmCovT(iIndex, 2)      = tEc % rmCovT(iIndex, 2)      + this % rvV(i) * this % rvT(i)
					tEc % rmCovT(iIndex, 3)      = tEc % rmCovT(iIndex, 3)      + this % rvW(i) * this % rvT(i)
				end if
			end if
		end do
		! -1- Phase two: Render
		do i = 1, iMaxBlock
			if(tEc % ivNumData(i) > 0) then
				tEc % rmVel(i,:)      = tEc % rmVel(i,:) / tEc % ivNumData(i)
				tEc % rvT(i)          = tEc % rvT(i) / tEc % ivNumData(i)
				tEc % raCovVel(i,1,1) = tEc % raCovVel(i,1,1) / tEc % ivNumData(i) - tEc % rmVel(i, 1) ** 2
				tEc % raCovVel(i,2,2) = tEc % raCovVel(i,2,2) / tEc % ivNumData(i) - tEc % rmVel(i, 2) ** 2
				tEc % raCovVel(i,3,3) = tEc % raCovVel(i,3,3) / tEc % ivNumData(i) - tEc % rmVel(i, 3) ** 2
				tEc % rvVarT(i)       = tEc % rvVarT(i) / tEc % ivNumData(i) - tEc % rvT(i) ** 2
				tEc % raCovVel(i,1,2) = tEc % raCovVel(i,1,2) / tEc % ivNumData(i) - tEc % rmVel(i, 1) * tEc % rmVel(i, 2)
				tEc % raCovVel(i,1,3) = tEc % raCovVel(i,1,3) / tEc % ivNumData(i) - tEc % rmVel(i, 1) * tEc % rmVel(i, 3)
				tEc % raCovVel(i,2,3) = tEc % raCovVel(i,2,3) / tEc % ivNumData(i) - tEc % rmVel(i, 2) * tEc % rmVel(i, 3)
				tEc % raCovVel(i,2,1) = tEc % raCovVel(i,1,2)
				tEc % raCovVel(i,3,1) = tEc % raCovVel(i,1,3)
				tEc % raCovVel(i,3,2) = tEc % raCovVel(i,2,3)
				tEc % rmCovT(i,1)     = tEc % rmCovT(i,1) / tEc % ivNumData(i) - tEc % rmVel(i, 1) * tEc % rvT(i)
				tEc % rmCovT(i,2)     = tEc % rmCovT(i,2) / tEc % ivNumData(i) - tEc % rmVel(i, 2) * tEc % rvT(i)
				tEc % rmCovT(i,3)     = tEc % rmCovT(i,3) / tEc % ivNumData(i) - tEc % rmVel(i, 3) * tEc % rvT(i)
			else
				tEc % rmVel(i,:)      = NaN
				tEc % rvT(i)          = NaN
				tEc % raCovVel(i,:,:) = NaN
				tEc % rvVarT(i)       = NaN
				tEc % rmCovT(i,:)     = NaN
			end if
		end do
		
		! Perfection status
		tEc % averagingTime = iAveragingTime
		
	end function sd_Averages
	
	
	function ec_Clean(this) result(iRetCode)
	
		! Routine arguments
		class(EddyCovData), intent(inout)	:: this
		integer								:: iRetCode
		
		! Locals
		! --none--
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Set completion indicators to .false.
		this % isPrimed = .false.
		this % isFilled = .false.
		
		! Clean out the input part
		if(allocated(this % rvTimeStamp)) deallocate(this % rvTimeStamp)
		if(allocated(this % ivNumData))   deallocate(this % ivNumData)
		if(allocated(this % rmVel))       deallocate(this % rmVel)
		if(allocated(this % rvT))         deallocate(this % rvT)
		if(allocated(this % raCovVel))    deallocate(this % raCovVel)
		if(allocated(this % rmCovT))      deallocate(this % rmCovT)
		if(allocated(this % rvVarT))      deallocate(this % rvVarT)
		
		! Clean outputs
		if(allocated(this % rvTheta))     deallocate(this % rvTheta)
		if(allocated(this % rvPhi))       deallocate(this % rvPhi)
		if(allocated(this % rvPsi))       deallocate(this % rvPsi)
		if(allocated(this % rmRotVel))    deallocate(this % rmRotVel)
		if(allocated(this % raRotCovVel)) deallocate(this % raRotCovVel)
		if(allocated(this % rmRotCovT))   deallocate(this % rmRotCovT)
		
	end function ec_Clean
	
	
	function ec_Allocate(this, iNumData) result(iRetCode)
	
		! Routine arguments
		class(EddyCovData), intent(inout)	:: this
		integer, intent(in)					:: iNumdata
		integer								:: iRetCode
		
		! Locals
		integer	:: iErrCode
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Try reserving workspace (failure admittedly possible, and checked for)
		aLlocate( &
			this % rvTimeStamp(iNumData), &
			this % ivNumData(iNumData), &
			this % rmVel(iNumData,3), &
			this % rvT(iNumData), &
			this % raCovVel(iNumData,3,3), &
			this % rmCovT(iNumData,3), &
			this % rvVarT(iNumData), &
			this % rvTheta(iNumData), &
			this % rvPhi(iNumData), &
			this % rvPsi(iNumData), &
			this % rmRotVel(iNumData,3), &
			this % raRotCovVel(iNumData,3,3), &
			this % rmRotCovT(iNumData,3), &
			stat = iErrCode &
		)
		if(iErrCode /= 0) then
			iRetCode = 1
		end if
		
	end function ec_Allocate
	
	
	function ec_Dump(this) result(iRetCode)
	
		! Routine arguments
		class(EddyCovData), intent(in)	:: this
		integer							:: iRetCode
		
		! Locals
		integer			:: i
		integer			:: j
		type(DateTime)	:: dt
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check there is something to dump
		if(.not. this % isPrimed) then
			print *, '-- Structure has not been primed with input data --'
			print *
			iRetCode = 1
			return
		end if
		
		! Print
		print *, "====================================================="
		print *, "Num time steps = ", size(this % rvTimeStamp)
		print *, "Averaging time = ", this % averagingTime
		do i = 1, size(this % rvTimeStamp)
			iRetCode = dt % fromEpoch(this % rvTimeStamp(i))
			print *, dt % toISO(), '   Number of raw data in current step : ', this % ivNumData(i)
			print *, 'Input section ---------------------------------------'
			print "(a, f6.2, 2(1x, f6.2))", "Wind: ", this % rmVel(i,:)
			print "(a, f6.2, 2(1x, f6.2))", "Temp: ", this % rvT(i)
			print *, "Cov(vel):"
			do j = 1, 3
				print "(f7.4,2(',',f7.4))", this % raCovVel(i,j,:)
			end do
			print "(a, f7.4, 2(1x, f7.4))", "Cov(Temp): ", this % rmCovT(i,:)
			print "(a, f7.4, 2(1x, f7.4))", "Var(Temp): ", this % rvVarT(i)
			if(this % isFilled) then
				print *, 'Output section --------------------------------------'
				print "('Theta, Psi, Phi:',3(1x,f8.4))", this % rvTheta(i), this % rvPhi(i), this % rvPsi(i)
				print "(a, f6.2, 2(1x, f6.2))", "Rotated wind: ", this % rmRotVel(i,:)
				print *, "Rotated cov(vel):"
				do j = 1, 3
					print "(f7.4,2(',',f7.4))", this % raRotCovVel(i,j,:)
				end do
				print "(a, f7.4, 2(1x, f7.4))", "Rotated cov(Temp): ", this % rmRotCovT(i,:)
			end if
		end do
		
		! Leave
		print *, "====================================================="
		
	end function ec_Dump
	
	
	function ec_getSize(this) result(iNumData)
	
		! Routine arguments
		class(EddyCovData), intent(in)	:: this
		integer							:: iNumData
		
		! Locals
		! --none--
		
		! Check something is in input section
		if(.not.this % isPrimed) then
			iNumData = 0
			return
		end if
		
		! Count number of non-missing data in input
		iNumData = size(this % ivNumData)
		
	end function ec_getSize
	
	
	function ec_getAvgTime(this) result(iAveragingTime)
	
		! Routine arguments
		class(EddyCovData), intent(in)	:: this
		integer							:: iAveragingTime
		
		! Locals
		! --none--
		
		! Retrieve the contents of averaging time field, whatever its contents
		iAveragingTime = this % averagingTime
		
	end function ec_getAvgTime
	
	
	function ec_getNumValidInput(this) result(iNumValid)
	
		! Routine arguments
		class(EddyCovData), intent(in)	:: this
		integer							:: iNumValid
		
		! Locals
		! --none--
		
		! Check something is in input section
		if(.not.this % isPrimed) then
			iNumValid = 0
			return
		end if
		
		! Count number of non-missing data in input
		iNumValid = count(this % ivNumData > 0)
		
	end function ec_getNumValidInput
	
	
	! Create an empty multi-hour set, that is an EddyCovData object whose reason-to-be
	! is accepting data from single-hour EddyCovData objects created with SonicData % averages(...).
	!
	! Data from SonidData % averages(...) may then be accepted both right after averaging, and
	! after processing, as you like. Just, let me assume you will be consistent in choosing your
	! way of access, as I'll decide how to assign the logical status flags based on what will
	! be found in the first EddyCovData set you will present.
	!
	! In my view, the cleanest and fastest way to do is:
	!
	!	Invoke EddyCovData % createEmpty(...) for current data set (assuming you know how many hours you have, gaps included)
	!	Loop over all hours:
	!		Read one hourly file into a SonicData object
	!		Average its contents into an hourly EddyCovData object using SonidData % averages(...)
	!		Transfer contents of hourly EddyCovData to the multi-hour EddyCovData using EddyCovData % copy(...)
	!	Once loop done, invoke EddyCovData % process(...)
	!
	! By doing so, you can minimize the code cluttering, and alternating between averaging and processing
	! (of course in my opinion - if you think differently, feel free to act the way you like; in case, I advise you
	! having a look into ec_Copy(...) and figure out what will happen in your specific case)
	!		
	function ec_CreateEmpty(this, iNumHours, iAveragingTime) result(iRetCode)
	
		! Routine arguments
		class(EddyCovData), intent(out)	:: this
		integer, intent(in)				:: iNumHours		! Number of hours to be contained in this set (1 or more; I'm assuming we know it in advance, but if you like may specify a coarse "large" estimate, 'a la Fortran IV)
		integer, intent(in)				:: iAveragingTime	! Number of seconds in an averaging period (positive, must divide exactly 3600)
		integer							:: iRetCode
		
		! Locals
		integer	:: iErrCode
		integer	:: iNumBlocks
		integer	:: iNumData
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Clean this object preliminarily (just to make sure in case of failure)
		iErrCode = this % clean()
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		
		! Check inputs make sense
		if(iNumHours <= 0) then
			iRetCode = 2
			return
		end if
		if(iAveragingTime <= 0) then
			iRetCode = 3
			return
		end if
		if(mod(3600, iAveragingTime) /= 0) then
			iRetCode = 4
			return
		end if
		
		! Compute vector sizes, and allocate them preliminarily
		iNumData = iNumHours * (3600 / iAveragingTime)
		
		! Reserve vector space
		iErrCode = this % reserve(iNumData)
		if(iErrCode /= 0) then
			iRetCode = 5
			return
		end if
		
		! Set completion indicators to .false.
		this % isPrimed = .false.
		this % isFilled = .false.
		
		! Initialize all inputs to make any gaps evident in future
		this % rvTimeStamp = NaN_8
		this % ivNumData   = 0
		this % rmVel       = NaN
		this % rvT         = NaN
		this % raCovVel    = NaN
		this % rmCovT      = NaN
		this % rvVarT      = NaN
		
		! Initialize all outputs to make any gaps evident in future
		this % rvTheta     = NaN
		this % rvPhi       = NaN
		this % rvPsi       = NaN
		this % rmRotVel    = NaN
		this % raRotCovVel = NaN
		this % rmRotCovT   = NaN
		
	end function ec_CreateEmpty
	
	
	function ec_IsClean(this) result(lPredicateValue)
	
		! Routine arguments
		class(EddyCovData), intent(in)	:: this
		logical							:: lPredicateValue
		
		! Locals
		! --none--
		
		! Check the object is empty
		lPredicateValue = (.not. this % isPrimed) .and. (.not. this % isFilled) .and. (.not. allocated(this % rvTimeStamp))
		
	end function ec_IsClean
	
	
	function ec_IsEmpty(this) result(lPredicateValue)
	
		! Routine arguments
		class(EddyCovData), intent(in)	:: this
		logical							:: lPredicateValue
		
		! Locals
		! --none--
		
		! Check the object is empty
		lPredicateValue = (.not. this % isPrimed) .and. (.not. this % isFilled) .and. allocated(this % rvTimeStamp)
		
	end function ec_IsEmpty
	
	
	function ec_IsPrimed(this) result(lPredicateValue)
	
		! Routine arguments
		class(EddyCovData), intent(in)	:: this
		logical							:: lPredicateValue
		
		! Locals
		! --none--
		
		! Check the object is empty
		lPredicateValue = this % isPrimed
		
	end function ec_IsPrimed
	
	
	function ec_IsFilled(this) result(lPredicateValue)
	
		! Routine arguments
		class(EddyCovData), intent(in)	:: this
		logical							:: lPredicateValue
		
		! Locals
		! --none--
		
		! Check the object is empty
		lPredicateValue = this % isFilled
		
	end function ec_IsFilled
	
	
	function ec_IsHourly(this) result(lPredicateValue)
	
		! Routine arguments
		class(EddyCovData), intent(in)	:: this
		logical							:: lPredicateValue
		
		! Locals
		integer								:: iErrCode
		real(8)								:: rMinStamp
		real(8)								:: rMaxStamp
		integer, dimension(:), allocatable	:: ivYears
		
		! Check the answer is a trivial .false.
		if(.not. this % isReady()) then
			lPredicateValue = .false.
			return
		end if
		! Post-condition: now we know the object contains some time stamps
		
		! Check some *valid* time stamp is present
		if(all(.invalid.this % rvTimeStamp)) then
			lPredicateValue = .false.
			return
		end if
		
		! Find embedding time stamps, and check they span an interval shorter than an hour
		rMinStamp = minval(this % rvTimeStamp, mask = .valid. this % rvTimeStamp)
		rMaxStamp = maxval(this % rvTimeStamp, mask = .valid. this % rvTimeStamp)
		if(rMaxStamp - rMinStamp >= 3600.d0) then
			lPredicateValue = .false.
			return
		end if
		
		! Check the embedding time stamps belong to the same hour
		iErrCode = timeGetYear([rMinStamp, rMaxStamp], ivYears)
		if(size(ivYears) /= 2) then
			lPredicateValue = .false.
			return
		end if
		if(ivYears(1) /= ivYears(2)) then
			lPredicateValue = .false.
			return
		end if
		
		! Excluded all the .invalid. causes, we can do nothing else than
		! accepting the truth
		lPredicateValue = .true.
		
	end function ec_IsHourly
	
	
	function ec_GetTimeStamp(this, rvTimeStamp, iDeltaTime) result(iRetCode)
	
		! Routine argument
		class(EddyCovData), intent(in)						:: this			! A multi-hour object
		real(8), dimension(:), allocatable, intent(out)		:: rvTimeStamp	! The desired copy of object's time stamp (or nothing in case of error)
		integer, intent(out), optional						:: iDeltaTime	! The object's time stap
		integer												:: iRetCode
		
		! Locals
		integer	:: n
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check something can be made
		if(.not.allocated(this % rvTimeStamp)) then
			iRetCode = 1
			return
		end if
		n = size(this % rvTimeStamp)
		if(n < 0) then
			iRetCode = 2
			return
		end if
		if(this % averagingTime <= 0) then
			iRetCode = 3
			return
		end if
		
		! Get time stamp
		if(allocated(rvTimeStamp)) deallocate(rvTimeStamp)
		allocate(rvTimeStamp(n))
		rvTimeStamp = this % rvTimeStamp
		
		! Retrieve delta time, if requested
		if(present(iDeltaTime)) then
			iDeltaTime = this % averagingTime
		end if
		
	end function ec_GetTimeStamp
	
	
	function ec_GetInputData(this, ivNumData, rmVel, rvT, raCovVel, rmCovT, rvVarT) result(iRetCode)
	
		! Routine arguments
		class(EddyCovData), intent(in)						:: this
		integer, dimension(:), allocatable, intent(out)		:: ivNumData
		real, dimension(:,:), allocatable, intent(out)		:: rmVel
		real, dimension(:), allocatable, intent(out)		:: rvT
		real, dimension(:,:,:), allocatable, intent(out)	:: raCovVel
		real, dimension(:,:), allocatable, intent(out)		:: rmCovT
		real, dimension(:), allocatable, intent(out)		:: rvVarT
		integer												:: iRetCode
		
		! Locals
		integer	:: n
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check something can be made
		if(.not. this % isPrimed) then
			iRetCode = 1
			return
		end if
		
		! Clean output data
		if(allocated(ivNumData)) deallocate(ivNumData)
		if(allocated(rmVel)) deallocate(rmVel)
		if(allocated(rvT)) deallocate(rvT)
		if(allocated(raCovVel)) deallocate(raCovVel)
		if(allocated(rmCovT)) deallocate(rmCovT)
		if(allocated(rvVarT)) deallocate(rvVarT)
		
		! Get array size, and reserve workspace
		n = size(rvT)
		allocate(ivNumData(n))
		allocate(rmVel(n,3))
		allocate(rvT(n))
		allocate(raCovVel(n,3,3))
		allocate(rmCovT(n,3))
		allocate(rvVarT(n))
		
		! Retrieve data
		ivNumData = this % ivNumData
		rmVel     = this % rmVel
		rvT       = this % rvT
		raCovVel  = this % raCovVel
		rmCovT    = this % rmCovT
		rvVarT    = this % rvVarT
		
	end function ec_GetInputData

	
	function ec_AddHourly(this, rBaseTime, tEc) result(iRetCode)
	
		! Routine arguments
		class(EddyCovData), intent(inout)	:: this			! A multi-hour object
		real(8), intent(in)					:: rBaseTime	! The desired multi-hour object starting time stamp - must be an entire hour; forced to the lowest would it not be
		type(EddyCovData), intent(in)		:: tEc			! The hourly EddyCovData object whose contents should be added to the multi-hourly one
		integer								:: iRetCode
		
		! Locals
		integer								:: iErrCode
		integer								:: n
		integer								:: i
		integer								:: iDeltaTime
		real(8)								:: rTimeStart
		type(DateTime)						:: tDateTimeStart
		real(8), dimension(:), allocatable	:: rvTimeStamp
		integer, dimension(:), allocatable	:: ivTimeIndex
		integer, dimension(:), allocatable	:: ivNumData
		real, dimension(:,:), allocatable	:: rmVel
		real, dimension(:), allocatable		:: rvT
		real, dimension(:,:,:), allocatable	:: raCovVel
		real, dimension(:,:), allocatable	:: rmCovT
		real, dimension(:), allocatable		:: rvVarT
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check it makes sense to proceed
		if(tEc % getNumValidInput() <= 0) then
			iRetCode = 1
			return
		end if
		if(.not. tEc % isHourly()) then
			iRetCode = 2
			return
		end if
		
		! Round the starting time step to its beginning hour, and check it's compatible
		! with a correct DateTime value
		rTimeStart = (int(rBaseTime / 3600.0d0, kind=8) * 3600.0d0)
		iErrCode = tDateTimeStart % fromEpoch(rTimeStart)
		if(iErrCode /= 0) then
			iRetCode = 3
			return
		end if
		
		! Compute the hourly data's time indexes respect to the time start, as obtained in
		! the preceding step
		iErrCode = tEc % getTimeStamp(rvTimeStamp, iDeltaTime)
		if(iErrCode /= 0) then
			iRetCode = 4
			return
		end if
		iErrCode = timeLinearIndex(rvTimeStamp, iDeltaTime, ivTimeIndex)
		if(iErrCode /= 0) then
			iRetCode = 5
			return
		end if
		ivTimeIndex = ivTimeIndex + (minval(rvTimeStamp, mask = .valid.rvTimeStamp) - rTimeStart) / iDeltaTime
		! Post-condition: A finite time index is available
		
		! Limit time index to range of current object's time stamp
		n = this % getSize()
		do i = 1, n
			if(ivTimeIndex(i) > n) ivTimeIndex(i) = 0
		end do
		
		! Check some transfer remains to do (it might not, would the hour be off-range)
		if(count(ivTimeIndex > 0) <= 0) then
			iRetCode = -1
			return
		end if
		
		! Check averaging time is left unchanged (i.i., same as desired .and. no configuration change)
		if(this % averagingTime /= tEc % getAvgTime()) then
			iRetCode = 6
			return
		end if
		
		! Perform transfers
		where(ivTimeIndex > 0)
			this % rvTimeStamp = rvTimeStamp
		endwhere
		
		! Update state incorporating inputs
		iErrCode = tEc % getInputData(ivNumData, rmVel, rvT, raCovVel, rmCovT, rvVarT)
		if(iErrCode /= 0) then
			iRetCode = 7
			return
		end if
		if(.not. this % isPrimed) then
			this % isPrimed = .true.
			this % isFilled = .false.
		end if
		
		! Update the part of state incorporating output, if it is defined
		
	end function ec_AddHourly
	
	
	! *********************
	! * Internal routines *
	! *********************
	
	! Transform horizontal wind components wind from polar to cartesian form
		! (speed is surely greater than 1.e-6)
	subroutine uvWind(vel, dir, u, v)
	
		! Routine arguments
		real, intent(in)	:: vel
		real, intent(in)	:: dir
		real, intent(out)	:: u
		real, intent(out)	:: v
		
		! Locals
		! --none--
		
		! Perform the transform desired
		u = vel * sin(dir*ToRad)
		v = vel * cos(dir*ToRad)
		
	end subroutine uvWind
	
	
	! Transform horizontal wind components wind from cartesian to polar form
	! (speed is surely greater than 1.e-6)
	subroutine veldirWind(u, v, vel, dir)
	
		! Routine arguments
		real, intent(in)	:: u
		real, intent(in)	:: v
		real, intent(out)	:: vel
		real, intent(out)	:: dir
		
		! Locals
		! --none--
		
		! Perform the transform desired
		vel = sqrt(u**2 + v**2)
		dir = atan2(u, v)*ToDeg
		dir = mod(dir, 360.)
		if(dir < 0.) dir = dir + 360.
		
	end subroutine veldirWind
	
end module pbl_wind
