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
		real, dimension(3), intent(in)	:: polar				! Wind in polar form (vel=polar(1), dir=polar(2))
		real, dimension(3)				:: cartesian			! Wind in cartesian form (u=cartesian(1), v=cartesian(2))
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
		real, dimension(2), intent(in)	:: cartesian			! Wind in cartesian form (u=cartesian(1), v=cartesian(2), w=cartesian(3))
		real, dimension(2)				:: polar				! Wind in polar form (vel=polar(1), dir=polar(2), w=polar(3))
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
			polar = [NaN, NaN]
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
				elseif(interpretation == WCONV_PROVENANCE_TO_FLOW .or. interpretation == WCONV_FLOW_TO_PROVENANCE) then
					! Different interpretation for input and output: sign change
					call veldirWind(-cartesian(1), -cartesian(2), polar(1), polar(2))
				else
					! Wrong convention
					polar = [NaN, NaN, cartesian(3)]
				end if
			else
				! Same interpretation for input and output: no sign change
				call veldirWind(cartesian(1), cartesian(2), polar(1), polar(2))
			end if
		else
			polar = [NaN, NaN, cartesian(3)]
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
		
		! Check something is to be made: leave, if not
		if(isnan(vel)) then
			iClass = -9999
			return
		end if
		
		! Perform a simple table lookup
		n = size(rvVel) + 1
		do i = 1, n-1
			if(vel <= rvVel(i)) then
				iClass = i
				return
			end if
		end do
		
		! Execution reaches this point if no match is found, so
		iClass = n

	end function ClassVelScalar


	function ClassVelVector(vel, rvVel) result(ivClass)
		
		! Routine arguments
		real, dimension(:), intent(in)	:: vel			! Wind speed to classify
		real, dimension(:), intent(in)	:: rvVel		! Vector, containing upper class limits in increasing order
		integer, dimension(size(vel))	:: ivClass		! Speed class to which the wind belongs (-9999 if not assignable)
		
		! Locals
		integer		:: n
		integer		:: i, j
		
		! Main loop: iterate over speed values
		do j = 1, size(vel)
		
			! Check class can be assigned
			if(isnan(vel(j))) then
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
		real, intent(in)	:: dir				! Wind direction to classify (°)
		integer, intent(in)	:: iNumClasses		! Number of desired classes
		integer, intent(in)	:: iClassType		! Class type (0: first class is zero-centered; 1: first class starts at zero)
		integer				:: iClass			! Direction class to which the wind belongs (-9999 if no class is assignable)
		
		! Locals
		real	:: classWidth
		real	:: d
		
		! Check something is to be made: leave, if not
		if(isnan(dir)) then
			iClass = -9999
			return
		end if
		
		! Compute the fixed-size class width, and in case of zero-centere classes use it to adjust direction
		if(iNumClasses <= 0) then
			iClass = -9999
			return
		end if
		classWidth = 360. / iNumClasses
		d = dir
		if(iClassType == WDCLASS_ZERO_CENTERED) d = d + classWidth / 2.
		
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
		integer, intent(in)				:: iClassType		! Class type (0: first class is zero-centered; 1: first class starts at zero)
		integer, dimension(size(dir))	:: ivClass			! Direction class to which the wind belongs (-9999 if no class is assignable)
		
		! Locals
		real						:: classWidth
		real, dimension(size(dir))	:: d
		
		! Check something is to be made: leave, if not
		if(iNumClasses <= 0) then
			ivClass = -9999
			return
		end if
		
		! Compute the fixed-size class width, and in case of zero-centere classes use it to adjust direction
		classWidth = 360. / iNumClasses
		d = dir
		if(iClassType == WDCLASS_ZERO_CENTERED) d = d + classWidth / 2.
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
