! pbl_base - Fortran module, containing useful symbols and constants.
!
! This module is part of the pbl_met library.
!
! Copyright 2018 by Servizi Territorio srl
!
! This is open-source code, covered by the lGPL 3.0 license.
!
module pbl_base

	implicit none
	
	private
	
	! Public interface
	! 0. Useful constants and symbols
	public	:: NaN							! Non-signalling single precision NaN (generates other NaNs when combined with other values)
	public	:: NaN_8						! Non-signalling double precision NaN (generates other NaNs when combined with other values)
	public	:: LAI_GRASS
	public	:: LAI_ALFALFA
	public	:: ASCE_STANDARDATMOSPHERE
	public	:: ASCE_STANDARDEQ
	public	:: ASCE_MEANTEMPERATURE
	public	:: ASCE_GRASS
	public	:: ASCE_ALFALFA
	public	:: ACV_GENERAL
	public	:: ACV_2ND_ORDER
	public	:: YEAR_DURATION
	public	:: MONTH_DURATION
	public	:: BASE_DAY
	public	:: BASE_DAY_8
	! 1. Operators and procedures
	public	:: operator(.valid.)
	public	:: operator(.invalid.)
	public	:: toUpper
	public	:: toLower
	
	! Constants
    real, parameter		:: NaN				       = Z'7FC00000'			! Special case of non-signalling NaN (single precision)
    real(8), parameter	:: NaN_8			       = Z'7FF8000000000000'	! Special case of non-signalling NaN (double precision)
	real, parameter		:: YEAR_DURATION	       = 365.25
	real, parameter		:: MONTH_DURATION	       = 30.6001
	integer, parameter	:: BASE_DAY			       = 2440588		! 01. 01. 1970
	integer, parameter	:: BASE_DAY_8		       = 2440588_8		! 01. 01. 1970
	integer, parameter	:: LAI_GRASS               = 0
	integer, parameter	:: LAI_ALFALFA             = 1
	integer, parameter	:: ASCE_STANDARDATMOSPHERE = 0
	integer, parameter	:: ASCE_STANDARDEQ         = 1
	integer, parameter	:: ASCE_MEANTEMPERATURE    = 2
	integer, parameter	:: ASCE_GRASS              = 1
	integer, parameter	:: ASCE_ALFALFA            = 2
	integer, parameter	:: ACV_GENERAL             = 0
	integer, parameter	:: ACV_2ND_ORDER           = 1
	
	! Operators
	
	interface operator(.valid.)
		module procedure isValid
		module procedure isValid8
	end interface operator(.valid.)
	
	interface operator(.invalid.)
		module procedure isInvalid
		module procedure isInvalid8
	end interface operator(.invalid.)
	
contains

	! Check a value is valid, that is, not a NaN.
	
	pure elemental function isValid(value) result(valid)
	
		! Routine arguments
		real, intent(in)	:: value	! Value to check
		logical				:: valid	! Check result
		
		! Locals
		! -none-
		
		! Check validity
		valid = .not.isnan(value)
		
	end function isValid
	
	pure elemental function isValid8(value) result(valid)
	
		! Routine arguments
		real(8), intent(in)	:: value	! Value to check
		logical				:: valid	! Check result
		
		! Locals
		! -none-
		
		! Check validity
		valid = .not.isnan(value)
		
	end function isValid8
	

	! Check a value is invalid, that is, a NaN.

	pure elemental function isInvalid(value) result(invalid)
	
		! Routine arguments
		real, intent(in)	:: value	! Value to check
		logical				:: invalid	! Check result
		
		! Locals
		! -none-
		
		! Check validity
		invalid = isnan(value)
		
	end function isInvalid

	pure elemental function isInvalid8(value) result(invalid)
	
		! Routine arguments
		real(8), intent(in)	:: value	! Value to check
		logical				:: invalid	! Check result
		
		! Locals
		! -none-
		
		! Check validity
		invalid = isnan(value)
		
	end function isInvalid8
	
	
	subroutine toUpper(sString)
	
		! Routine arguments
		character(len=*), intent(inout)	:: sString
		
		! Locals
		integer		:: i
		character	:: c
		
		! Change all alphabetic letters to uppercase
		do i = 1, len_trim(sString)
			c = sString(i:i)
			if(c >= 'a' .and. c <= 'z') then
				c = char(ichar(c) - ichar('a') + ichar('A'))
			end if
		end do
		
	end subroutine toUpper
	

	subroutine toLower(sString)
	
		! Routine arguments
		character(len=*), intent(inout)	:: sString
		
		! Locals
		integer		:: i
		character	:: c
		
		! Change all alphabetic letters to lowercase
		do i = 1, len_trim(sString)
			c = sString(i:i)
			if(c >= 'A' .and. c <= 'Z') then
				c = char(ichar(c) - ichar('A') + ichar('a'))
			end if
		end do
		
	end subroutine toLower
	
end module pbl_base
