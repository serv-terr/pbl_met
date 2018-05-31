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
	! 2. Data types
	public	:: IniFile
	
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
	
	! Data types
	
	type IniFile
		logical, private										:: lIsUseable
		integer, private										:: iNumKeys
		character(len=256), dimension(:), allocatable, private	:: svLine
		character(len=256), dimension(:), allocatable, private	:: svKey
		character(len=256), dimension(:), allocatable, private	:: svValue
	contains
		! Constructor
		procedure, public	:: read       => iniRead
		procedure, public	:: getString  => iniGetString
		procedure, public	:: getReal4   => iniGetReal4
		procedure, public	:: getReal8   => iniGetReal8
		procedure, public	:: getInteger => iniGetInteger
		procedure, public	:: getVector4 => iniGetVector4
		procedure, public	:: getVector8 => iniGetVector8
	end type IniFile
	
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
	
	
	function iniRead(this, iLUN, sIniFileName) result(iRetCode)
	
		! Routine arguments
		class(IniFile), intent(inout)	:: this
		integer, intent(in)				:: iLUN
		character(len=*), intent(in)	:: sIniFileName
		integer							:: iRetCode
		
		! Locals
		integer				:: iErrCode
		character(len=256)	:: sBuffer
		character(len=256)	:: sCurrentSection
		character(len=256)	:: sCurSection
		integer				:: iNumLines
		integer				:: iLine
		integer				:: iPos
		integer				:: iNumKeys
		integer				:: i
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Clean state before to proceed
		this % lIsUseable = .false.
		if(allocated(this % svLine)) deallocate(this % svLine)
		if(allocated(this % svKey)) deallocate(this % svKey)
		if(allocated(this % svValue)) deallocate(this % svValue)
		
		! Now, count lines excluding comments
		open(iLUN, file=sIniFileName, status='old', action='read', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		iNumLines = 0
		do
		
			! Try gathering a line, and if acquired replace all characters
			! from the first '#' on with blanks
			read(iLun, "(a)", iostat=iErrCode) sBuffer
			if(iErrCode /= 0) exit
			iPos = index(sBuffer, "#")
			if(iPos > 0) sBuffer(iPos:) = ' '
			
			! Replace TABs and other spaces with regular blanks
			do i = 1, len(sBuffer)
				if(ichar(sBuffer(i:i)) < 32) sBuffer(i:i) = ' ' 
			end do
			if(sBuffer == ' ') cycle
			! Post-condition: something remains
			
			! Increment line count, remembering lines which may be subject to parsing
			iNumLines = iNumLines + 1
			
		end do
		if(iNumLines <= 0) then
			close(iLun)
			iRetCode = 2
			return
		end if
		rewind(iLUN)
		
		! Reserve workspace, and populate it with non-comment lines
		allocate(this % svLine(iNumLines), this % svKey(iNumLines), this % svValue(iNumLines))
		iLine = 0
		do
		
			! Try gathering a line, and if acquired replace all characters
			! from the first '#' on with blanks
			read(iLun, "(a)", iostat=iErrCode) sBuffer
			if(iErrCode /= 0) exit
			iPos = index(sBuffer, "#")
			if(iPos > 0) sBuffer(iPos:) = ' '
			
			! Replace TABs and other spaces with regular blanks
			do i = 1, len(sBuffer)
				if(ichar(sBuffer(i:i)) < 32) sBuffer(i:i) = ' ' 
			end do
			if(sBuffer == ' ') cycle
			! Post-condition: something remains
			
			! Add next line
			iLine = iLine + 1
			this % svLine(iLine) = sBuffer
			
		end do
		close(iLUN)
		! Post-condition: Some lines found
		
		! Parse line contents
		sCurrentSection = ""
		iNumKeys        = 0
		do iLine = 1, iNumLines
			
			! Check string is a section, and if so assign it
			if(isSection(this % svLine(iLine), sCurSection)) then
				sCurrentSection = sCurSection
			else
				! Not a section: may contain an equal sign, that is, to be a name = value couple
				iPos = index(this % svLine(iLine), "=")
				if(iPos > 0) then
					iNumKeys = iNumKeys + 1
					write(this % svKey(iNumKeys), "(a,'@',a)") &
						trim(sCurrentSection), adjustl(this % svLine(iLine)(1:(iPos-1)))
					this % svValue(iNumKeys) = adjustl(this % svLine(iLine)((iPos+1):))
				end if
			end if
			
		end do
		
	end function iniRead
	
	
	function iniGetString(this, sKey, sValue, sDefault) result(iRetCode)
	
		! Routine arguments
		class(IniFile), intent(inout)			:: this
		character(len=*), intent(in)			:: sKey
		character(len=*), intent(out)			:: sValue
		character(len=*), intent(in), optional	:: sDefault
		integer									:: iRetCode
		
		! Locals
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
	end function iniGetString
	
	
	function iniGetReal4(this, sKey, rValue, rDefault) result(iRetCode)
	
		! Routine arguments
		class(IniFile), intent(inout)			:: this
		character(len=*), intent(in)			:: sKey
		real, intent(out)						:: rValue
		real, intent(in), optional				:: rDefault
		integer									:: iRetCode
		
		! Locals
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
	end function iniGetReal4
	
	
	function iniGetReal8(this, sKey, rValue, rDefault) result(iRetCode)
	
		! Routine arguments
		class(IniFile), intent(inout)			:: this
		character(len=*), intent(in)			:: sKey
		real(8), intent(out)					:: rValue
		real(8), intent(in), optional			:: rDefault
		integer									:: iRetCode
		
		! Locals
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
	end function iniGetReal8
	
	
	function iniGetInteger(this, sKey, iValue, iDefault) result(iRetCode)
	
		! Routine arguments
		class(IniFile), intent(inout)			:: this
		character(len=*), intent(in)			:: sKey
		integer, intent(out)					:: iValue
		integer, intent(in), optional			:: iDefault
		integer									:: iRetCode
		
		! Locals
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
	end function iniGetInteger
	
	
	function iniGetVector4(this, sKey, rvValue, rvDefault) result(iRetCode)
	
		! Routine arguments
		class(IniFile), intent(inout)				:: this
		character(len=*), intent(in)				:: sKey
		real, dimension(:), allocatable, intent(in)	:: rvValue
		real, dimension(:), intent(in), optional	:: rvDefault
		integer										:: iRetCode
		
		! Locals
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
	end function iniGetVector4
	
	
	function iniGetVector8(this, sKey, rvValue, rvDefault) result(iRetCode)
	
		! Routine arguments
		class(IniFile), intent(inout)					:: this
		character(len=*), intent(in)					:: sKey
		real(8), dimension(:), allocatable, intent(in)	:: rvValue
		real(8), dimension(:), intent(in), optional		:: rvDefault
		integer											:: iRetCode
		
		! Locals
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
	end function iniGetVector8
	
	! **********************
	! * Internal functions *
	! **********************
	
	function isSection(sString, sSection) result(lIsSection)
	
		! Routine arguments
		character(len=*), intent(in)	:: sString
		character(len=*), intent(out)	:: sSection
		logical							:: lIsSection
		
		! Locals
		integer		:: iPos
		integer		:: iLast
		
		! Check first and last character are compatible with a section-type string
		iPos = verify(sString, ' ')
		iLast = len_trim(sString)
		if(iPos >= 1 .and. iPos <= iLast) then
			! Some blanks before the string real beginning: parse from there
			lIsSection = sString(iPos:iPos) == '[' .and. sString(iLast:iLast) == ']'
			if(lIsSection) then
				sSection = sString((iPos+1):(iLast-1))
			else
				sSection = ' '
			end if
		else
			! String begins with a non-blank
			lIsSection = sString(1:1) == '[' .and. sString(iLast:iLast) == ']'
			if(lIsSection) then
				sSection = sString(1:(iLast-1))
			else
				sSection = ' '
			end if
		end if
		
	end function isSection
	
end module pbl_base
