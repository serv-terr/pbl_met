! nan_support.f90 - Fortran module supporting IEEE NaN. The reason-of-being
!                   of this module is to replace treatment of invalid values
! based on -9999.9 (or any other normal floating point number) with non-signaling NaNs.
!
! This is preferable, because a -9999.9 may be a valid value for some physical quantities
! (e.g. Obukhov length).
!
! This module is part of the new PBL_MET library.
!
module nan_support

	implicit none
	
	private
	
	! Public interface
	public	:: NaN
	
	! A ready-to-use non-signalling NaN value:
    real, parameter	:: NaN      = Z'7FC00000'
    
end module nan_support
