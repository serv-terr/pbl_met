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
	public	:: OS_UNIX
	public	:: OS_WIN
	! 1. Operators and procedures
	public	:: operator(.valid.)
	public	:: operator(.invalid.)
	public	:: toUpper
	public	:: toLower
	public	:: baseName
	public	:: gammaP	! Lower incomplete gamma function P(a,x)
	! 2. Data types
	public	:: IniFile
	public	:: Spline
	
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
	integer, parameter	:: OS_UNIX      	       = 0
	integer, parameter	:: OS_WIN		           = 1
	
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
		procedure, public	:: dump       => iniDump
		procedure, public	:: getString  => iniGetString
		procedure, public	:: getReal4   => iniGetReal4
		procedure, public	:: getReal8   => iniGetReal8
		procedure, public	:: getInteger => iniGetInteger
	end type IniFile
	
	
	type Spline
		! Spline coefficients
		real(8), private, dimension(:), allocatable	:: x
		real(8), private, dimension(:), allocatable	:: y
		real(8), private, dimension(:), allocatable	:: b
		real(8), private, dimension(:), allocatable	:: c
		real(8), private, dimension(:), allocatable	:: d
		logical										:: lEquallySpaced
		logical										:: lIsOK
	contains
		procedure, public	:: init     => splInit
		procedure, public	:: evaluate => splEval
	end type Spline
	
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
	
	
	function baseName(sFileName, iOS) result(sName)
	
		! Routine arguments
		character(len=*), intent(in)	:: sFileName
		integer, intent(in)				:: iOS			! Operating system type (OS_WIN: Windows; OS_UNIX: Linux, UNIX, OS/X)
		character(len=256)				:: sName
		
		! Locals
		integer		:: iPos
		character	:: sDelimiter
		
		! Set delimiter based on OS type
		if(iOS == OS_UNIX) then
			sDelimiter = "/"
		elseif(iOS == OS_WIN) then
			sDelimiter = "\"
		else
			sName = sFileName
			return
		end if
		
		! Find last delimiter in string, and if found return all characters on right of it;
		! otherwise, return the input string unchanged
		iPos = index(sFileName, sDelimiter, back=.true.)
		if(iPos > 0) then
			sName = sFileName(iPos+1:)
		else
			sName = sFileName
		end if
		
	end function baseName
	
	
	function gammaP(a, x, iMaxIterations) result(gP)
	
		! Routine arguments
		real, intent(in)				:: a
		real, intent(in)				:: x
		integer, intent(in), optional	:: iMaxIterations
		real							:: gP
		
		! Locals
		integer	:: i
		integer	:: iMaxIter
		real	:: delta
		real	:: accumulator
		real	:: p, b, c, d, h, tmp
		real	:: fpmin
		
		! Check input parameters
		if(x < 0. .or. a <= 0.) then
			gP = NaN
			return
		end if
		if(present(iMaxIterations)) then
			if(iMaxIterations < 10) then
				gP = NaN
				return
			end if
		end if
		
		! Assign maximum iteration number
		if(present(iMaxIterations)) then
			iMaxIter = iMaxIterations
		else
			iMaxIter = 100
		end if
		
		! Dispatch execution based on parameter value
		if(x < a + 1.) then
			! More convenient to use the series expansion here
			p           = a
			delta       = 1./a
			accumulator = delta
			gP          = NaN
			do i = 1, iMaxIter
				p           = p + 1.
				delta       = delta * x / p
				accumulator = accumulator + delta
				if(abs(delta) < abs(accumulator)*epsilon(delta)*4.) then
					gP = accumulator * exp(-x + a*log(x) - log_gamma(a))
					exit
				end if
			end do
		else
			! Here it is better to use the continued fraction approximation
			fpmin = 4./huge(fpmin)
			b     = x + 1.0 - a
			c     = 1. / fpmin
			d     = 1. / b
			h     = d
			gP    = NaN
			do i = 1, iMaxIter
				tmp = -i * (i - a)
				b   = b + 2.
				d   = tmp*d + b
				if(abs(d) < fpmin) d = fpmin
				c   = b + tmp/c
				if(abs(c) < fpmin) c = fpmin
				d   = 1. / d
				delta = d * c
				h   = h * delta
				if(abs(delta - 1.) < epsilon(delta)*4.) then
					gP = h * exp(-x + a*log(x) - log_gamma(a))
					exit
				end if
			end do
			gP = 1. - gP
		end if
		
	end function gammaP
	
	
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
					call removeChar(this % svValue(iNumKeys), '"')
				end if
			end if
			
		end do
		
		! Confirm successful completion
		this % lIsUseable = .true.
		this % iNumKeys   = iNumKeys
		
	end function iniRead
	
	
	function iniDump(this) result(iRetCode)
	
		! Routine arguments
		class(IniFile), intent(in)	:: this
		integer						:: iRetCode
		
		! Locals
		integer	:: i
		integer	:: iKeyLen
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check whether the dump is to be make in full,
		! that is, the INI file read has completed successfully
		! and the data structures have been filled
		if(this % lIsUseable) then
		
			! Check length to constrain keys to when printing
			iKeyLen = 0
			do i = 1, this % iNumKeys
				iKeyLen = max(iKeyLen, len_trim(this % svKey(i)))
			end do
		
			! Print all keys, and their associated values. To print
			! keys in column the maximum key length is used, along with
			! the fact that in Fortran all strings in an array share
			! the same length and are blank-filled on right. The approach
			! I've followed would have *not* worked in C and other
			! variable-length string languages.
			do i = 1, this % iNumKeys
				print "(a,' -> ',a)", this % svKey(i)(1:iKeyLen), trim(this % svValue(i))
			end do
		
		else
		
			print *, "INI data contents has not yet been assigned, nothing to print"
			iRetCode = 1
			
		end if
		
	end function iniDump
	
	
	function iniGetString(this, sSection, sKey, sValue, sDefault) result(iRetCode)
	
		! Routine arguments
		class(IniFile), intent(inout)			:: this
		character(len=*), intent(in)			:: sSection
		character(len=*), intent(in)			:: sKey
		character(len=*), intent(out)			:: sValue
		character(len=*), intent(in), optional	:: sDefault
		integer									:: iRetCode
		
		! Locals
		integer				:: i
		character(len=256)	:: sFullKey
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check something is to be made
		if(this % iNumKeys > 0) then
		
			! Yes: there are data lines to scan
			write(sFullKey, "(a, '@', a)") trim(sSection), trim(sKey)
			do i = 1, this % iNumKeys
				if(this % svKey(i) == sFullKey) then
					sValue = this % svValue(i)
					return
				end if
			end do
			
			! Nothing found if execution reaches here: in case,
			! yield the default (if present) or a blank (otherwise).
			if(present(sDefault)) then
				sValue = sDefault
			else
				sValue = ' '
			end if
			
		else
			
			! No INI data available: flag an error condition.
			if(present(sDefault)) then
				sValue = sDefault
			else
				sValue = ' '
			end if
			iRetCode = 1
			
		end if
		
	end function iniGetString
	
	
	function iniGetReal4(this, sSection, sKey, rValue, rDefault) result(iRetCode)
	
		! Routine arguments
		class(IniFile), intent(inout)			:: this
		character(len=*), intent(in)			:: sSection
		character(len=*), intent(in)			:: sKey
		real, intent(out)						:: rValue
		real, intent(in), optional				:: rDefault
		integer									:: iRetCode
		
		! Locals
		character(len=32)	:: sValue
		real				:: rReplace
		integer				:: iErrCode
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Assign the replacement value based on rDefault
		if(present(rDefault)) then
			rReplace = rDefault
		else
			rReplace = NaN
		end if
		
		! Gather the string supposedly containing the floating point value to transfer
		iErrCode = this % getString(sSection, sKey, sValue)
		if(iErrCode /= 0) then
			rValue = rReplace
			iRetCode = 1
			return
		end if
		! Post-condition: iRetCode was 0 from now on
		
		! Check the value found to be not empty
		if(sValue == ' ') then
			rValue = rReplace
			iRetCode = 2
			return
		end if
		
		! Ok, something was found: but, it might not be a floating point value:
		! try converting it and, in case of failure, yield an error
		read(sValue, *, iostat=iErrCode) rValue
		if(iErrCode /= 0) then
			rValue = rReplace
			iRetCode = 3
		end if
		! Post-condition: 'rValue' has been assigned correctly, and on
		! function exit will be restituted regularly
		
	end function iniGetReal4
	
	
	function iniGetReal8(this, sSection, sKey, rValue, rDefault) result(iRetCode)
	
		! Routine arguments
		class(IniFile), intent(inout)			:: this
		character(len=*), intent(in)			:: sSection
		character(len=*), intent(in)			:: sKey
		real(8), intent(out)					:: rValue
		real(8), intent(in), optional			:: rDefault
		integer									:: iRetCode
		
		! Locals
		character(len=32)	:: sValue
		real(8)				:: rReplace
		integer				:: iErrCode
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Assign the replacement value based on rDefault
		if(present(rDefault)) then
			rReplace = rDefault
		else
			rReplace = NaN_8
		end if
		
		! Gather the string supposedly containing the floating point value to transfer
		iErrCode = this % getString(sSection, sKey, sValue)
		if(iErrCode /= 0) then
			rValue = rReplace
			iRetCode = 1
			return
		end if
		! Post-condition: iRetCode was 0 from now on
		
		! Check the value found to be not empty
		if(sValue == ' ') then
			rValue = rReplace
			iRetCode = 2
			return
		end if
		
		! Ok, something was found: but, it might not be a floating point value:
		! try converting it and, in case of failure, yield an error
		read(sValue, *, iostat=iErrCode) rValue
		if(iErrCode /= 0) then
			rValue = rReplace
			iRetCode = 3
		end if
		! Post-condition: 'rValue' has been assigned correctly, and on
		! function exit will be restituted regularly
		
	end function iniGetReal8
	
	
	function iniGetInteger(this, sSection, sKey, iValue, iDefault) result(iRetCode)
	
		! Routine arguments
		class(IniFile), intent(inout)			:: this
		character(len=*), intent(in)			:: sSection
		character(len=*), intent(in)			:: sKey
		integer, intent(out)					:: iValue
		integer, intent(in), optional			:: iDefault
		integer									:: iRetCode
		
		! Locals
		character(len=32)	:: sValue
		integer				:: iReplace
		integer				:: iErrCode
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Assign the replacement value based on rDefault
		if(present(iDefault)) then
			iReplace = iDefault
		else
			iReplace = -9999
		end if
		
		! Gather the string supposedly containing the floating point value to transfer
		iErrCode = this % getString(sSection, sKey, sValue)
		if(iErrCode /= 0) then
			iValue = iReplace
			iRetCode = 1
			return
		end if
		! Post-condition: iRetCode was 0 from now on
		
		! Check the value found to be not empty
		if(sValue == ' ') then
			iValue = iReplace
			iRetCode = 2
			return
		end if
		
		! Ok, something was found: but, it might not be a floating point value:
		! try converting it and, in case of failure, yield an error
		read(sValue, *, iostat=iErrCode) iValue
		if(iErrCode /= 0) then
			iValue = iReplace
			iRetCode = 3
		end if
		! Post-condition: 'iValue' has been assigned correctly, and on
		! function exit will be restituted regularly
		
	end function iniGetInteger
	
	
	function splInit(this, rvX, rvY) result(iRetCode)
	
		! Routine arguments
		class(Spline), intent(out)			:: this
		real(8), dimension(:), intent(in)	:: rvX
		real(8), dimension(:), intent(in)	:: rvY
		integer								:: iRetCode
		
		! Locals
		integer		:: n
		integer		:: i, j, gap
		real(8)		:: h
		logical		:: lNoGaps
		real(8)		:: rDelta
		real(8)		:: rIndex
		
		! Assume success (will falsify on failure)
		iRetCode = 0

		! Check parameters
		n = size(rvX)
		if(n < 2) then
			iRetCode = 1
			return
		end if
		if(size(rvY) /= n) then
			iRetCode = 2
			return
		end if
		lNoGaps = .true.
		do i = 1, n
			if(.invalid.rvX(i) .or. .invalid.rvY(i)) then
				lNoGaps = .false.
				exit
			end if
		end do
		if(.not.lNoGaps) then
			iRetCode = 3
			return
		end if
		do i = 2, n
			if(rvX(i) <= rvX(i-1)) then
				iRetCode = 4
				return
			end if
		end do
		
		! Check the rvX values are equally spaced (a particularly important special case)
		rDelta = rvX(2) - rvX(1)
		this % lEquallySpaced = .false.
		do i = 2, n
			rIndex = (rvX(i)-rvX(1)) / rDelta
			if(rIndex - floor(rIndex) >= 1.d-3) then
				this % lEquallySpaced = .false.
				exit
			end if
		end do
		
		! Reserve workspace
		if(allocated(this % x)) deallocate(this % x)
		if(allocated(this % y)) deallocate(this % y)
		if(allocated(this % b)) deallocate(this % b)
		if(allocated(this % c)) deallocate(this % c)
		if(allocated(this % d)) deallocate(this % d)
		
		! Save data points
		this % x = rvX
		this % y = rvY
		
		! Check the number of points allows the existence of a third-degree spline
		if(n < 3) then
			! Not enough points: perform plain linear interpolation
			this % b(1) = (rvY(2)-rvY(1))/(rvX(2)-rvX(1))
			this % c(1) = 0.
			this % d(1) = 0.
			this % b(2) = this % b(1)
			this % c(2) = 0.
			this % d(2) = 0.
			return
		end if

		! Prepare
		gap = n-1
		this % d(1) = this % x(2) - this % x(1)
		this % c(2) = (this % y(2) - this % y(1))/this % d(1)
		do i = 2, gap
			this % d(i) = this % x(i+1) - this % x(i)
			this % b(i) = 2.0*(this % d(i-1) + this % d(i))
			this % c(i+1) = (this % y(i+1) - this % y(i))/this % d(i)
			this % c(i) = this % c(i+1) - this % c(i)
		end do

		! Enforce boundary conditions
		this % b(1) = -this % d(1)
		this % b(n) = -this % d(n-1)
		this % c(1) = 0.0
		this % c(n) = 0.0
		if(n /= 3) then
			this % c(1) = this % c(3) / (this % x(4) - this % x(2)) - this % c(2) / (this % x(3) - this % x(1))
			this % c(n) = this % c(n-1) / (this % x(n) - this % x(n-2)) - this % c(n-2) / (this % x(n-1) - this % x(n-3))
			this % c(1) = this % c(1) * this % d(1)**2 / (this % x(4) - this % x(1))
			this % c(n) = -this % c(n) * this % d(n-1)**2 / (this % x(n) - this % x(n-3))
		end if
		
		! Solution step 1: forward substitution
		do i = 2, n
			h = this % d(i-1) / this % b(i-1)
			this % b(i) = this % b(i) - h * this % d(i-1)
			this % c(i) = this % c(i) - h * this % c(i-1)
		end do

		! Solution step 2: back substitution
		this % c(n) = this % c(n) / this % b(n)
		do j = 1, gap
			i = n-j
			this % c(i) = (this % c(i) - this % d(i) * this % c(i+1)) / this % b(i)
		end do

		! Final coefficients calculation
		this % b(n) = (this % y(n) - this % y(gap)) / this % d(gap) + this % d(gap)*(this % c(gap) + 2.d0 * this % c(n))
		do i = 1, gap
			this % b(i) = (this % y(i+1) - this % y(i)) / this % d(i) - this % d(i) * (this % c(i+1) + 2.d0 * this % c(i))
			this % d(i) = (this % c(i+1) - this % c(i)) / this % d(i)
			this % c(i) = 3.d0 * this % c(i)
		end do
		this % c(n) = 3.d0 * this % c(n)
		this % d(n) = this % d(n-1)
		
		! Inform users evaluations may occur
		this % lIsOK = .true.

	end function splInit


	function splEval(this, rX) result(rValue)
	
		! Routine arguments
		class(Spline), intent(in)	:: this
		real(8), intent(in)			:: rX
		real(8)						:: rValue
		
		! Locals
		integer	:: n
		integer	:: i
		integer	:: j
		integer	:: k
		real(8)	:: dx

		! Use boundary values outside the 'x' interval
		n = size(this % x)
		if(rX < this % x(1)) then
			rValue = this % y(1)
			return
		elseif(rX > this % x(n)) then
			rValue = this % y(n)
			return
		end if

		! Find index i such that x(i) <= rX <= x(i+1)
		if(this % lEquallySpaced) then
			! Find index directly
			i = floor((rX - this % x(1)) / (this % x(2) - this % x(1))) + 1
		else
			! Find index by binary search
			i = 1
			j = n+1
			do while(j > i+1)
				k = (i+j)/2
				if(rX < this % x(k)) then
					j=k
				else
					i=k
				end if
			end do
		end if
		
		! Evaluate spline
		dx = rX - this % x(i)
		rValue = this % y(i) + dx * (this % b(i) + dx * (this % c(i) + dx * this % d(i)))
		
	end function splEval

	
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
	
	
	subroutine removeChar(sString, cChar)
	
		! Routine arguments
		character(len=*), intent(inout)	:: sString
		character, intent(in)			:: cChar
		
		! Locals
		integer	:: i, j, n
		
		! Copy all desired characters, and them only, to the string, in place
		n = len_trim(sString)
		j = 0
		do i = 1, n
			if(sString(i:i) /= cChar) then
				j = j + 1
				if(j /= i) then
					sString(j:j) = sString(i:i)
				end if
			end if
		end do
		if(j < n) then
			sString((j+1):n) = ' '
		end if
		
	end subroutine removeChar
	
end module pbl_base
