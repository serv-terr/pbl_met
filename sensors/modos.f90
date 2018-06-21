! Module Modos - Access to Metek "MODOS" SODAR, SODAR/RASS and MRR2 data.
!
! This module is part of the pbl_met library.
!
! Copyright 2018 by Servizi Territorio srl
!
! This is open-source code, covered by lGPL 3.0 license
!
module Modos

	use pbl_met

	implicit none
	
	private
	
	! Public interface
	! -1- Data types
	public	:: ModosData
	! -1- Constants
	public	:: MDS_SODAR
	public	:: MDS_SODAR_RASS
	public	:: MDS_MRR2
	
	! Data types
	
	type ModosData
		character(len=512), dimension(:), allocatable, private	:: svLines
		integer, private										:: iSensorType = 0	! MDS_SODAR, MDS_SODAR_RASS, MDS_MRR2
		integer, dimension(:), allocatable, private				:: ivBlockIdx		! Indices of block starts
		integer, dimension(:), allocatable, private				:: ivBlockLen		! Indices of block lengths
		integer, dimension(6), private							:: ivErrorMask		! Error mask for each antenna
		logical, dimension(6), private							:: lvPresent		! Spectra, antenna activation mask
		real, dimension(32,44,6), private						:: raPower			! Spectra, actual back-scattered power
		integer, private										:: iNumSpHeights	! Number of heights used for spectra (include the two noise heights)
	contains
		! Non-default constructors
		procedure	:: load	         		=> mds_load
		! Basic enquiry
		procedure	:: getSensorType 		=> mds_getSensorType
		procedure	:: getNumHeightChanges	=> mds_getNumHeightChanges
		procedure	:: getBlockInfo			=> mds_getBlockInfo
		! Data retrieval
		procedure	:: setErrorMasks 		=> mds_setErrorMasks
		procedure	:: getSodarSpectra		=> mds_getSodarSpectra
		! Low-level auxiliaries
		procedure	:: getLineIndex			=> mds_getLineIndex
	end type ModosData
	
	! Constants
	
	integer, parameter	:: MDS_SODAR      = 1
	integer, parameter	:: MDS_SODAR_RASS = 2
	integer, parameter	:: MDS_MRR2       = 3
	
contains

	function mds_load(this, iLUN, sFileName) result(iRetCode)
	
		! Routine arguments
		class(ModosData), intent(out)	:: this
		integer, intent(in)				:: iLUN
		character(len=*), intent(in)	:: sFileName
		integer							:: iRetCode
		
		! Locals
		integer					:: iErrCode
		character(len=512)		:: sBuffer
		integer					:: iNumLines
		integer					:: iLine
		integer					:: i
		integer					:: iNumBlocks
		integer					:: iBlock
		integer, dimension(3)	:: ivBlockTypeCount
		integer					:: iRassLine
		integer					:: iFrom, iTo
		character(len=6)		:: sTemp
		real					:: rTemp
		integer					:: iNumPositiveCounts
		
		! Assume success (will falsify on failure); and, initialize type of sensor to unknown, just in case of failure
		iRetCode           = 0
		this % iSensorType = 0
		this % ivErrorMask = 0	! All data transmitted (default)
		
		! Count lines in MODOS file
		open(iLUN, file=sFileName, status='old', action='read', iostat=iErrCode)
		if(iErrCode /= 0) then
			iRetCode = 1
			return
		end if
		iNumLines = 0
		do
			read(iLUN, "(a)", iostat=iErrCode) sBuffer
			if(iErrCode /= 0) exit
			iNumLines = iNumLines + 1
		end do
		if(iNumLines <= 0) then
			iRetCode = 2
			close(iLUN)
			return
		end if
		! Post-condition: Input file contains at least 'iNumLines' text lines
		
		! Reserve workspace
		if(allocated(this % svlines)) deallocate(this % svLines)
		allocate(this % svLines(iNumLines))
		
		! Read lines into workspace
		rewind(iLUN)
		do iLine = 1, iNumLines
			read(iLUN, "(a)") this % svLines(iLine)
		end do
		close(iLUN)
		
		! Locate SDR and MRR lines - that is, block headers, into file
		iNumBlocks = 0
		do iLine = 1, iNumLines
			if(this % svLines(iLine)(1:3) == 'MRR' .or. this % svLines(iLine)(1:3) == 'SDR') then
				iNumBlocks = iNumBlocks + 1
			end if
		end do
		if(iNumBlocks <= 0) then
			iRetCode = 3
			return
		end if
		if(allocated(this % ivBlockIdx)) deallocate(this % ivBlockIdx)
		if(allocated(this % ivBlockLen)) deallocate(this % ivBlockLen)
		allocate(this % ivBlockIdx(iNumBlocks))
		allocate(this % ivBlockLen(iNumBlocks))
		iBlock = 0
		do iLine = 1, iNumLines
			if(this % svLines(iLine)(1:3) == 'MRR' .or. this % svLines(iLine)(1:3) == 'SDR') then
				iBlock = iBlock + 1
				this % ivBlockIdx(iBlock) = iLine
			end if
		end do
		do iBlock = 1, iNumBlocks - 1
			this % ivBlockLen(iBlock) = this % ivBlockIdx(iBlock+1) - this % ivBlockIdx(iBlock)
		end do
		this % ivBlockLen(iNumBlocks) = iNumLines - this % ivBlockIdx(iNumBlocks) + 1
		
		! Check whether all blocks are the same
		ivBlockTypeCount = 0
		do iBlock = 1, iNumBlocks
			if(this % svLines(this % ivBlockIdx(iBlock))(1:3) == "SDR") then
				iRassLine = 0
				iFrom     = this % ivBlockIdx(iBlock) + this % ivBlockLen(iBlock) - 1
				iTo       = max(this % ivBlockIdx(iBlock) + this % ivBlockLen(iBlock) - 4, this % ivBlockIdx(iBlock))
				do i = iFrom, iTo, -1
					if(this % svLines(i)(1:3) == 'ERR') then
						iRassLine = i
						exit
					end if
				end do
				if(iRassLine > 0) then
					ivBlockTypeCount(MDS_SODAR_RASS) = ivBlockTypeCount(MDS_SODAR_RASS) + 1
				else
					ivBlockTypeCount(MDS_SODAR) = ivBlockTypeCount(MDS_SODAR) + 1
				end if
			elseif(this % svLines(this % ivBlockIdx(iBlock))(1:3) == "MRR") then
				ivBlockTypeCount(MDS_MRR2) = ivBlockTypeCount(MDS_MRR2) + 1
			end if
		end do
		if(sum(ivBlockTypeCount) /= iNumBlocks) then
			iRetCode = 4
			return
		end if
		iNumPositiveCounts = count(ivBlockTypeCount > 0)
		if(iNumPositiveCounts /= 1) then
			iRetCode = 5
			return
		end if
		if(ivBlockTypeCount(MDS_SODAR) > 0) then
			this % iSensorType = MDS_SODAR
		elseif(ivBlockTypeCount(MDS_SODAR_RASS) > 0) then
			this % iSensorType = MDS_SODAR_RASS
		elseif(ivBlockTypeCount(MDS_MRR2) > 0) then
			this % iSensorType = MDS_MRR2
		else
			this % iSensorType = 0
		end if
		
		! Set initial number of spectra heights to zero, indicating
		! block spectra have not yet been retrieved
		this % iNumSpHeights = 0
		
	end function mds_load
	
	
	function mds_getSensorType(this) result(iSensorType)
	
		! Routine arguments
		class(ModosData), intent(in)	:: this
		integer							:: iSensorType
		
		! Locals
		! --none--
		
		! Retrieve the information piece desired
		iSensorType = this % iSensorType
		
	end function mds_getSensorType
	
	
	function mds_getNumHeightChanges(this, iNumChanges) result(iRetCode)
	
		! Routine arguments
		class(ModosData), intent(in)	:: this
		integer, intent(out)			:: iNumChanges
		integer							:: iRetCode
		
		! Locals
		integer, dimension(:), allocatable	:: ivNewHeight, ivOldHeight
		integer								:: iErrCode
		integer								:: i
		integer								:: iHeightIdx
		integer								:: iBlock
		logical								:: lDiffer
		
		! Assume success (will falsify on failure)
		iRetCode    = 0
		iNumChanges = 0
		
		! Check something can be made
		if(this % iSensorType <= 0) then
			iRetCode = 1
			return
		end if
		if(.not.allocated(this % svLines) .or. .not.allocated(this % ivBlockIdx) .or. .not.allocated(this % ivBlockLen)) then
			iRetCode = 2
			return
		end if
		
		! Get heights from first block
		iHeightIdx = this % getLineIndex(1, 'H  ')
		iErrCode = GetHeights(this % svLines(iHeightIdx), this % iSensorType, ivOldHeight)
		if(iErrCode /= 0) then
			iRetCode = 3
			return
		end if
		
		! Check all heights from following blocks are the same as the preceding one, counting the
		! changes
		do iBlock = 2, size(this % ivBlockIdx)
		
			! Get new height set
			iHeightIdx = this % getLineIndex(iBlock, 'H  ')
			iErrCode = GetHeights(this % svLines(iHeightIdx), this % iSensorType, ivNewHeight)
			if(iErrCode /= 0) then
				iRetCode = 4
				return
			end if
			
			! Compare old height set: if different size, or different contents, then a
			! configuration change has occurred
			if(size(ivNewHeight) /= size(ivOldHeight)) then
				lDiffer = .true.
				deallocate(ivOldHeight)
				allocate(ivOldHeight(size(ivNewHeight)))
				ivOldHeight = ivNewHeight
			elseif(any(ivNewHeight /= ivOldHeight)) then
				lDiffer = .true.
				ivOldHeight = ivNewHeight
			else
				lDiffer = .false.
			end if
			
			! Increase count, if necessary
			if(lDiffer) iNumChanges = iNumChanges + 1
			
		end do
		
	end function mds_getNumHeightChanges
	
	
	function mds_getBlockInfo(this, rvTimeStamp, ivTimeStep) result(iRetCode)
	
		! Routine arguments
		class(ModosData), intent(in)					:: this
		real(8), dimension(:), allocatable, intent(out)	:: rvTimeStamp
		integer, dimension(:), allocatable, intent(out)	:: ivTimeStep
		integer											:: iRetCode
		
		! Locals
		integer			:: iErrCode
		integer			:: i
		integer			:: iBlkIdx
		integer			:: n
		integer			:: iDeltaTime
		integer			:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		real(8)			:: rSecond
		type(DateTime)	:: tDate
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check something can be made
		if(this % iSensorType <= 0) then
			iRetCode = 1
			return
		end if
		if(.not.allocated(this % svLines) .or. .not.allocated(this % ivBlockIdx) .or. .not.allocated(this % ivBlockLen)) then
			iRetCode = 2
			return
		end if
		n = size(this % ivBlockIdx)
		
		! Reserve workspace
		if(allocated(rvTimeStamp)) deallocate(rvTimeStamp)
		if(allocated(ivTimeStep)) deallocate(ivTimeStep)
		allocate(rvTimeStamp(n))
		allocate(ivTimeStep(n))
		
		! Iterate over block prefixes, and retrieve the information desired
		do i = 1, size(this % ivBlockIdx)
			iBlkIdx = this % ivBlockIdx(i)
			read(this % svLines(iBlkIdx), "(4x,6i2,8x,i6)", iostat=iErrCode) iYear, iMonth, iDay, iHour, iMinute, iSecond, iDeltaTime
			if(iErrCode == 0) then
				iYear = iYear + 2000
				rSecond = iSecond
				tDate = DateTime(iYear, iMonth, iDay, iHour, iMinute, rSecond)
				rvTimeStamp(i) = tDate % toEpoch()
				ivTimeStep(i)  = iDeltaTime
			else
				rvTimeStamp(i) = NaN_8
				ivTimeStep(i)  = -9999
			end if
		end do
		
	end function mds_getBlockInfo
	
	
	function mds_getSodarSpectra(this, iBlock) result(iRetCode)
	
		! Routine arguments
		class(ModosData), intent(inout)		:: this
		integer, intent(in)					:: iBlock
		integer								:: iRetCode
		
		! Locals
		integer								:: iErrCode
		integer								:: n
		integer								:: i
		integer								:: iLevel
		integer								:: iAntenna
		character							:: cLevel
		character							:: cAntenna
		integer, dimension(:), allocatable	:: ivHeights
		real, dimension(:), allocatable		:: rvValues
		character(len=3)					:: sLineType
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check something can be made
		if(.not.allocated(this % ivBlockIdx)) then
			! Modos data have not yet been loaded from file
			iRetCode = 1
			return
		end if
		if(this % iSensorType <= 0 .or. this % iSensorType == MDS_MRR2) then
			! Data are filled, but, this is surely not a SODAR or SODAR/RASS
			iRetCode = 2
			return
		end if
		n = size(this % ivBlockIdx)
		if(n <= 0) then
			! Data are filled, of SODAR or SODAR/RASS type, but no complete/valid block was loaded from file
			! (may happen, occasionally)
			iRetCode = 3
			return
		end if
		if(iBlock < 1 .or. iBlock > n) then
			! Data are filled, of SODAR(/RASS) type, and some blocks exist, but the block
			! index for which we want to retrieve data is wrong
			iRetCode = 4
			return
		end if
		
		! Load spectral data
		this % raPower = NaN
		iErrCode = GetHeights(this % svLines(this % ivBlockIdx(iBlock) + 1), MDS_SODAR, ivHeights)
		if(iErrCode /= 0) then
			iRetCode = 5
			return
		end if
		this % iNumSpHeights = size(ivHeights)
		i = this % ivBlockIdx(iBlock) + 2
		do
			sLineType = this % svLines(i)(1:3)
			if(sLineType(1:1) == 'F') then
			
				! This is a spectral line. Characters in positions 2 and 3 of sLineType indicate which
				! line, and which antenna
				cLevel = sLineType(2:2)
				if(cLevel >= '0' .and. cLevel <= '9') then
					iLevel = ichar(cLevel) - ichar('0') + 1
				else
					iLevel = ichar(cLevel) - ichar('A') + 1 + 10
				end if
				cAntenna = sLineType(3:3)
				if(cAntenna /= 'R') then
					iAntenna = ichar(cAntenna) - ichar('0') + 1
				else
					iAntenna = 6
				end if
				
				! Load actual data
				iErrCode = CaptureValues( &
					this % svLines(i), &
					this % iNumSpHeights, &
					MDS_SODAR, &
					rvValues &
				)
				if(iErrCode /= 0) then
					iRetCode = 6
					return
				end if
				this % raPower(iLevel, 1:this % iNumSpHeights, iAntenna) = rvValues
				
				! Scan next line
				i = i + 1
				
			else
			
				! Block ended: exit, so that scan may proceed to the next
				exit
				
			end if
		end do
		
	end function mds_getSodarSpectra
	
	
	function mds_setErrorMasks(this, iMask1, iMask2, iMask3, iMask4, iMask5, iMaskR) result(iRetCode)
	
		! Routine arguments
		class(ModosData), intent(out)	:: this
		integer, intent(in)				:: iMask1
		integer, intent(in)				:: iMask2
		integer, intent(in)				:: iMask3
		integer, intent(in)				:: iMask4
		integer, intent(in)				:: iMask5
		integer, intent(in)				:: iMaskR
		integer							:: iRetCode
		
		! Locals
		! --none--
		
		! Constants
		integer, parameter	:: MSK_ERRORS = b'111111111111'	! Only 12 less significant bits used in MODOS (SODAR/RASS) files
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Assign masks, after having set their unused bits to 0
		this % ivErrorMask(1) = IAND(iMask1, MSK_ERRORS)
		this % ivErrorMask(2) = IAND(iMask2, MSK_ERRORS)
		this % ivErrorMask(3) = IAND(iMask3, MSK_ERRORS)
		this % ivErrorMask(4) = IAND(iMask4, MSK_ERRORS)
		this % ivErrorMask(5) = IAND(iMask5, MSK_ERRORS)
		this % ivErrorMask(6) = IAND(iMaskR, MSK_ERRORS)
		
	end function mds_setErrorMasks
	
	
	function mds_getLineIndex(this, iBlock, sLineType) result(iLineIdx)
	
		! Routine arguments
		class(ModosData), intent(in)	:: this
		integer, intent(in)				:: iBlock
		character(len=3), intent(in)	:: sLineType
		integer							:: iLineIdx
		
		! Locals
		integer		:: i
		
		! Assume the index is not found (will set it on success)
		iLineIdx = -9999
		
		! Search the line in block beginning with 'sLineType'
		do i = this % ivBlockIdx(1), this % ivBlockIdx(1) + this % ivBlockLen(1) - 1
			if(this % svLines(i)(1:3) == sLineType) then
				iLineIdx = i
				exit
			end if
		end do
		
	end function mds_getLineIndex
	
	! **********************
	! * Internal functions *
	! **********************
	
	! Parse heights, without knowing how many they are in advance
	function GetHeights(sLine, iSensorType, ivHeights) result(iRetCode)
	
		! Routine arguments
		character(len=*), intent(in)					:: sLine
		integer, intent(in)								:: iSensorType
		integer, dimension(:), allocatable, intent(out)	:: ivHeights
		integer											:: iRetCode
		
		! Locals
		integer				:: iFieldLen
		character(len=16)	:: sField
		integer				:: iFrom, iTo
		integer				:: i
		integer				:: iNumFields
		integer				:: iErrCode
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check line length to be compatible with the identified file type
		if(iSensorType == MDS_MRR2) then
			iFieldLen = 7
		elseif(iSensorType == MDS_SODAR .or. iSensorType == MDS_SODAR_RASS) then
			iFieldLen = 6
		else
			iRetCode = 1
			return
		end if
		if(mod(len_trim(sLine)-3, iFieldLen) /= 0) then
			iRetCode = 2
			return
		end if
		iNumFields = (len_trim(sLine)-3) / iFieldLen
		if(iNumFields <= 0) then
			iRetCode = 3
			return
		end if
		if(allocated(ivHeights)) deallocate(ivHeights)
		allocate(ivHeights(iNumFields))
		
		! Try decoding all fields
		do i = 1, iNumFields
			iFrom  = 3 + (i-1)*iFieldLen
			iTo    = iFrom + iFieldLen - 1
			sField = sLine(iFrom:iTo)
			if(sField == ' ') then
				ivHeights(i) = -9999
				cycle
			end if
			read(sField, *, iostat=iErrCode) ivHeights(i)
			if(iErrCode /= 0) then
				ivHeights(i) = -9999
				cycle
			end if
		end do
		
		! Check all data read are valid (they are expected to be for this call)
		if(any(ivHeights <= -9999)) then
			iRetCode = 4
			return
		end if
		! Post condition: No data was found invalid, just return
		
	end function GetHeights
	
	
	! Capture (i.e. attempt reading, and assign invalid if missing) floating point values from
	! a data line, knowing in advance how many they are
	function CaptureValues(sLine, iNumFields, iSensorType, rvValues) result(iRetCode)
	
		! Routine arguments
		character(len=*), intent(in)					:: sLine
		integer, intent(in)								:: iNumFields
		integer, intent(in)								:: iSensorType
		real, dimension(:), allocatable, intent(out)	:: rvValues
		integer											:: iRetCode
		
		! Locals
		integer				:: iFieldLen
		character(len=16)	:: sField
		integer				:: iFrom, iTo
		integer				:: i
		integer				:: iErrCode
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check line length to be compatible with the identified file type
		if(iSensorType == MDS_MRR2) then
			iFieldLen = 7
		elseif(iSensorType == MDS_SODAR .or. iSensorType == MDS_SODAR_RASS) then
			iFieldLen = 6
		else
			iRetCode = 1
			return
		end if
		if(mod(len_trim(sLine)-3, iFieldLen) /= 0) then
			iRetCode = 2
			return
		end if
		if(allocated(rvValues)) deallocate(rvValues)
		allocate(rvValues(iNumFields))
		
		! Try decoding all fields
		do i = 1, iNumFields
			iFrom  = 3 + (i-1)*iFieldLen
			iTo    = iFrom + iFieldLen - 1
			sField = sLine(iFrom:iTo)
			if(sField == ' ') then
				rvValues(i) = NaN
				cycle
			end if
			read(sField, *, iostat=iErrCode) rvValues(i)
			if(iErrCode /= 0) then
				rvValues(i) = NaN
				cycle
			end if
		end do
		
	end function CaptureValues


	! Parse heights, without knowing how many they are in advance
	function GetErrors(sLine, iSensorType, ivErrors) result(iRetCode)
	
		! Routine arguments
		character(len=*), intent(in)					:: sLine
		integer, intent(in)								:: iSensorType
		integer, dimension(:), allocatable, intent(out)	:: ivErrors
		integer											:: iRetCode
		
		! Locals
		integer				:: iFieldLen
		character(len=16)	:: sField
		integer				:: iFrom, iTo
		integer				:: i
		integer				:: iNumFields
		integer				:: iErrCode
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check line length to be compatible with the identified file type
		if(iSensorType == MDS_MRR2) then
			iRetCode = 1	! Error codes not supported in MRR-2
			return
		elseif(iSensorType == MDS_SODAR .or. iSensorType == MDS_SODAR_RASS) then
			iFieldLen = 6
		else
			iRetCode = 2
			return
		end if
		if(mod(len_trim(sLine)-3, iFieldLen) /= 0) then
			iRetCode = 3
			return
		end if
		iNumFields = (len_trim(sLine)-3) / iFieldLen
		if(iNumFields <= 0) then
			iRetCode = 4
			return
		end if
		if(allocated(ivErrors)) deallocate(ivErrors)
		allocate(ivErrors(iNumFields))
		
		! Try decoding all fields
		do i = 1, iNumFields
			iFrom  = 3 + (i-1)*iFieldLen
			iTo    = iFrom + iFieldLen - 1
			sField = sLine(iFrom:iTo)
			if(sField == ' ') then
				ivErrors(i) = -9999
				cycle
			end if
			read(sField, "(o6)", iostat=iErrCode) ivErrors(i)
			if(iErrCode /= 0) then
				ivErrors(i) = -9999
				cycle
			end if
		end do
		
		! Check all data read are valid (they are expected to be for this call)
		if(any(ivErrors <= -9999)) then
			iRetCode = 5
			return
		end if
		! Post condition: No data was found invalid, just return
		
	end function GetErrors

end module Modos

