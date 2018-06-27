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
		integer, dimension(6)									:: ivMixing			! Emission (carrier) frequency
		real, dimension(2)										:: rvSampling		! Spectral bandwidth
		real													:: rTemperature		! Reference temperature
		real, dimension(32,6), private							:: rmFrequency		! Frequency, associated to spectral lines
		real, dimension(32,6), private							:: rmDopplerSpeed	! Speed corresponding to the Doppler shift at computed frequencies
		integer, private										:: iNumSpHeights	! Number of heights used for spectra (include the two noise heights)
	contains
		! Non-default constructors
		procedure	:: load	         			   => mds_load
		! Basic enquiry
		procedure	:: getSensorType 			   => mds_getSensorType
		procedure	:: getNumHeightChanges		   => mds_getNumHeightChanges
		procedure	:: getBlockInfo				   => mds_getBlockInfo
		! Data retrieval
		procedure	:: setErrorMasks 			   => mds_setErrorMasks
		procedure	:: getSodarSpectra			   => mds_getSodarSpectra
		! Spectra evaluation and diagnostics
		procedure	:: sodarSpectraAvailability	   => mds_SodarSpectraAvailability
		procedure	:: sodarSpectraNoiseIndicators => mds_SodarSpectraNoiseIndicators
		procedure	:: sodarWindSeries             => mds_SodarWindSeries
		procedure	:: sodarTestRadialSpeed        => mds_SodarTestRadialSpeed
		! Low-level auxiliaries
		procedure	:: getLineIndex				   => mds_getLineIndex
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
		real								:: rBandwidth
		
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
					iAntenna = ichar(cAntenna) - ichar('0')
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
		
		! Get frequencies
		read(this % svLines(this % ivBlockIdx(iBlock)), "(130x,6i6,4x,2f6.1,72x,f6.2)", iostat=iErrCode) &
			this % ivMixing, this % rvSampling, this % rTemperature
		if(iErrCode == 0) then
			do iAntenna = 1, 6
				if(iAntenna <= 5) then
					rBandwidth = this % rvSampling(1)
				else
					rBandwidth = this % rvSampling(2)
				end if
				do i = 1, 32
					this % rmFrequency(i, iAntenna) = this % ivMixing(iAntenna) - rBandwidth / 2. + (i-1)*rBandwidth/32.
					this % rmDopplerSpeed(i, iAntenna) = &
						(this % rmFrequency(i, iAntenna) - this % ivMixing(iAntenna)) / &
						(this % rmFrequency(i, iAntenna) + this % ivMixing(iAntenna)) * &
						20.05*sqrt(this % rTemperature + 273.15)
				end do
			end do
		else
			this % rmFrequency    = NaN
			this % rmDopplerSpeed = NaN
		end if
		
	end function mds_getSodarSpectra
	
	
	function mds_SodarTestRadialSpeed( &
		this, &
		iBlock, &
		iRadial, &
		ivSpectralLineMax, &
		rvRadialVel, &
		rvMeasuredRadialVel, &
		ivHeights, &
		rvDelta &
	) result(iRetCode)
	
		! Routine arguments
		class(ModosData), intent(in)					:: this
		integer, intent(in)								:: iBlock
		integer, intent(in)								:: iRadial
		integer, dimension(:), allocatable, intent(out)	:: ivSpectralLineMax
		real, dimension(:), allocatable, intent(out)	:: rvRadialVel
		real, dimension(:), allocatable, intent(out)	:: rvMeasuredRadialVel
		integer, dimension(:), allocatable, intent(out)	:: ivHeights
		real, dimension(:), allocatable, intent(out)	:: rvDelta
		integer											:: iRetCode
		
		! Locals
		integer					:: iErrCode
		integer					:: i
		integer					:: j
		integer					:: n
		integer, dimension(1)	:: ivPos
		integer					:: iLastHeight
		real					:: c			! Speed of sound
		real					:: rFcarrier	! Carrier frequency
		real					:: rFmax		! Frequency corresponding to the maximum power found
		
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
		if(size(this % ivBlockIdx) <= 0) then
			! Data are filled, of SODAR or SODAR/RASS type, but no complete/valid block was loaded from file
			! (may happen, occasionally)
			iRetCode = 3
			return
		end if
		if(iRadial < 1 .or. iRadial > 6) then
			iRetCode = 4
			return
		end if
		
		! Reserve workspace
		n = this % iNumSpHeights - 2
		if(allocated(ivSpectralLineMax))   deallocate(ivSpectralLineMax)
		if(allocated(rvRadialVel))         deallocate(rvRadialVel)
		if(allocated(rvMeasuredRadialVel)) deallocate(rvMeasuredRadialVel)
		if(allocated(ivHeights))           deallocate(ivHeights)
		if(allocated(rvDelta))             deallocate(ivHeights)
		allocate(ivSpectralLineMax(n))
		allocate(rvRadialVel(n))
		allocate(rvDelta(n))
		
		! Iterate over frequency lines
		c = 20.05 * sqrt(this % rTemperature + 273.15)
		rFcarrier = this % ivMixing(iRadial)
		if(count(.valid.this % raPower(:,:,iRadial)) > 0) then
			do i = 1, n
				ivPos = maxloc(this % raPower(:,i,iRadial), mask=.valid.this % raPower(:,i,iRadial))
				if(ivPos(1) > 0) then
					rFmax = this % rmFrequency(ivPos(1),iRadial)
					rvRadialVel(i) = (rFmax - rFcarrier) / (rFmax + rFcarrier) * c
					ivSpectralLineMax(i) = ivPos(1)
				else
					rvRadialVel(i) = NaN
					ivSpectralLineMax(i) = 0
				end if
			end do
		end if
		
		! Get velocity along radial 1, as in standard Metek test for SODAR driver
		j = this % getLineIndex(iBlock, "VR1")
		iLastHeight = this % iNumSpHeights - 2
		if(j > 0) then
			iErrCode = CaptureValues(this % svLines(j), iLastHeight, this % iSensorType, rvMeasuredRadialVel)
			if(iErrCode /= 0) then
				iRetCode = 5
				return
			end if
		end if
		
		! Get heights
		iErrCode = GetHeights(this % svLines(this % ivBlockIdx(iBlock) + 1), this % iSensorType, ivHeights)
		if(iErrCode /= 0) then
			iRetCode = 6
			return
		end if
		
		! Compute the difference between estimated and measured speed
		rvDelta = rvMeasuredRadialVel - rvRadialVel
		
	end function mds_SodarTestRadialSpeed
	
	
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
		
		! Check something can be made
		if(.not.allocated(this % ivBlockIdx) .or. .not.allocated(this % ivBlockLen)) return
		if(iBlock <= 0 .or. iBlock > size(this % ivBlockIdx)) return
		
		! Search the line in block beginning with 'sLineType'
		do i = this % ivBlockIdx(iBlock), this % ivBlockIdx(iBlock) + this % ivBlockLen(iBlock) - 1
			if(this % svLines(i)(1:3) == sLineType) then
				iLineIdx = i
				exit
			end if
		end do
		
	end function mds_getLineIndex
	
	
	function mds_SodarSpectraAvailability(this, rvAvailability) result(iRetCode)
	
		! Routine arguments
		class(ModosData), intent(in)	:: this
		real, dimension(6), intent(out)	:: rvAvailability
		integer							:: iRetCode
		
		! Locals
		integer		:: i
		integer		:: iNumValid
		integer		:: iNumTotal
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check something can be made
		if(this % iNumSpHeights <= 0) then
			iRetCode = 1
			return
		end if
		
		! Count valid and total data, per single antenna, and convert them to
		! a unique availability fraction (0 to 1 valued)
		iNumTotal = 32*this % iNumSpHeights
		do i = 1, 6
			iNumValid = count(.valid.this % raPower(:,:,i))
			rvAvailability(i) = float(iNumValid) / float(iNumTotal)
		end do
		
	end function mds_SodarSpectraAvailability
	
	
	function mds_SodarSpectraNoiseIndicators( &
		this, &
		rvNoiseLevel, &
		rvTop, &
		rvBottom, &
		rvMeanVariation &
	) result(iRetCode)
	
		! Routine arguments
		class(ModosData), intent(in)	:: this
		real, dimension(6), intent(out)	:: rvNoiseLevel
		real, dimension(6), intent(out)	:: rvTop
		real, dimension(6), intent(out)	:: rvBottom
		real, dimension(6), intent(out)	:: rvMeanVariation
		integer							:: iRetCode
		
		! Locals
		integer					:: iAntenna
		integer					:: iNoiseIndex1
		integer					:: iNoiseIndex2
		logical, dimension(6)	:: lvPresent
		real, dimension(32)		:: rvNoise1
		real, dimension(32)		:: rvNoise2
		integer					:: n
		integer					:: i
		integer					:: iNumValid
		real					:: rDelta
		real					:: rMean1
		real					:: rMean2
		real					:: rPeak1
		real					:: rPeak2
		real					:: rBase1
		real					:: rBase2
		real					:: rTotVar1
		real					:: rTotVar2
		real, dimension(6)		:: rvMeanVar1
		real, dimension(6)		:: rvMeanVar2
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check something can be made
		if(this % iNumSpHeights <= 0) then
			iRetCode = 1
			return
		end if
		
		! Set-up the noise indices
		iNoiseIndex1 = this % iNumSpHeights - 1
		iNoiseIndex2 = this % iNumSpHeights
		
		! Scan antennas in spectra, and check they are active or not
		do iAntenna = 1, 6
			lvPresent(iAntenna) = count(.valid.this % raPower(:,:,iAntenna)) > 0
		end do
		
		! Retrive noise-related quantities
		do iAntenna = 1, 6
			if(lvPresent(iAntenna)) then
				rvNoise1 = this % raPower(:,iNoiseIndex1,iAntenna)
				rvNoise2 = this % raPower(:,iNoiseIndex2,iAntenna)
				n = count(.valid.rvNoise1)
				if(n > 0) then
					rMean1 = sum(rvNoise1, mask=.valid.rvNoise1) / n
					rPeak1 = maxval(rvNoise1, mask=.valid.rvNoise1)
					rBase1 = minval(rvNoise1, mask=.valid.rvNoise1)
				else
					rMean1 = NaN
					rPeak1 = NaN
					rBase1 = NaN
				end if
				n = count(.valid.rvNoise2)
				if(n > 0) then
					rMean2 = sum(rvNoise2, mask=.valid.rvNoise2) / n
					rPeak2 = maxval(rvNoise2, mask=.valid.rvNoise2)
					rBase2 = minval(rvNoise2, mask=.valid.rvNoise2)
				else
					rMean2 = NaN
					rPeak2 = NaN
					rBase2 = NaN
				end if
				rTotVar1 = 0.
				rTotVar2 = 0.
			else
				rMean1 = NaN
				rPeak1 = NaN
				rBase1 = NaN
				rMean2 = NaN
				rPeak2 = NaN
				rBase2 = NaN
			end if
			rvMeanVar1(iAntenna) = 0.
			iNumValid            = 0
			do i = 2, 32
				rDelta = abs(rvNoise1(i) - rvNoise1(i-1))
				if(.valid.rDelta) then
					rvMeanVar1(iAntenna) = rvMeanVar1(iAntenna) + rDelta
					iNumValid            = iNumValid + 1
				end if
			end do
			if(iNumValid > 0) then
				rvMeanVar1(iAntenna) = rvMeanVar1(iAntenna) / iNumValid
			end if
			rvMeanVar2(iAntenna) = 0.
			iNumValid            = 0
			do i = 2, 32
				rDelta = abs(rvNoise2(i) - rvNoise2(i-1))
				if(.valid.rDelta) then
					rvMeanVar1(iAntenna) = rvMeanVar1(iAntenna) + rDelta
					iNumValid            = iNumValid + 1
				end if
			end do
			if(iNumValid > 0) then
				rvMeanVar2(iAntenna) = rvMeanVar2(iAntenna) / iNumValid
			end if
			rvNoiseLevel(iAntenna) = (rMean1 + rMean2) / 2.
			rvTop(iAntenna)        = max(rPeak1, rPeak2)
			rvBottom(iAntenna)     = min(rPeak1, rPeak2)
		end do
		rvMeanVariation = (rvMeanVar1 + rvMeanVar2) / 2.0
		
	end function mds_SodarSpectraNoiseIndicators
	
	
	function mds_SodarWindSeries(this, iHeight, iHeightDelta, rvTimeStamp, rvVel, rvDir) result(iRetCode)
	
		! Routine arguments
		class(ModosData), intent(in)					:: this
		integer, intent(in)								:: iHeight
		integer, intent(in)								:: iHeightDelta
		real(8), dimension(:), allocatable, intent(out)	:: rvTimeStamp
		real, dimension(:), allocatable, intent(out)	:: rvVel
		real, dimension(:), allocatable, intent(out)	:: rvDir
		integer											:: iRetCode
		
		! Locals
		integer								:: iErrCode
		integer								:: iLastHeight
		integer								:: n
		integer								:: i
		integer								:: j
		integer								:: iClosestHeight
		integer, dimension(1)				:: ivClosest
		integer, dimension(:), allocatable	:: ivTimeStep
		integer, dimension(:), allocatable	:: ivHeights
		real, dimension(:), allocatable		:: rvValues
		integer, dimension(:), allocatable	:: ivErrors
		integer, dimension(:), allocatable	:: ivErr1
		integer, dimension(:), allocatable	:: ivErr2
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
		! Check something can be made
		if(.not.allocated(this % ivBlockIdx) .or. .not.allocated(this % ivBlockLen)) then
			iRetCode = 1
			return
		end if
		if(size(this % ivBlockIdx) <= 0 .or. size(this % ivBlockLen) <= 0) then
			iRetCode = 2
			return
		end if
		n = size(this % ivBlockIdx)
		if(this % iSensorType <= 0 .or. this % iSensorType == MDS_MRR2) then
			! Data are filled, but, this is surely not a SODAR or SODAR/RASS
			iRetCode = 3
			return
		end if
		
		! Get block information
		iErrCode = this % getBlockInfo(rvTimeStamp, ivTimeStep)
		
		! Reserve any other workspace
		if(allocated(rvVel)) deallocate(rvVel)
		if(allocated(rvDir)) deallocate(rvDir)
		if(allocated(ivErr1)) deallocate(ivErr1)
		if(allocated(ivErr2)) deallocate(ivErr2)
		allocate(rvVel(n))
		allocate(rvDir(n))
		allocate(ivErr1(n))
		allocate(ivErr2(n))
		
		! Iterate over blocks, and get data from height in range
		do i = 1, n
		
			! Get this block's heights, and locate the closest among them to the one desired
			iErrCode = GetHeights(this % svLines(this % ivBlockIdx(i) + 1), this % iSensorType, ivHeights)
			if(iErrCode /= 0) then
				rvVel(i) = NaN
				rvDir(i) = NaN
				cycle
			end if
			ivClosest = minloc(abs(ivHeights - iHeight))
			iClosestHeight = ivHeights(ivClosest(1))
			if(abs(iClosestHeight - iHeight) > iHeightDelta) then
				rvVel(i) = NaN
				rvDir(i) = NaN
				cycle
			end if
			iLastHeight = size(ivHeights) - 2
			
			! Find wind speed and direction
			j = this % getLineIndex(i, "V  ")
			if(j > 0) then
				iErrCode = CaptureValues(this % svLines(j), iLastHeight, this % iSensorType, rvValues)
				if(iErrCode == 0) then
					rvVel(i) = rvValues(ivClosest(1))
				else
					rvVel(i) = NaN
				end if
			else
				rvVel(i) = NaN
			end if
			j = this % getLineIndex(i, "D  ")
			if(j > 0) then
				iErrCode = CaptureValues(this % svLines(j), iLastHeight, this % iSensorType, rvValues)
				if(iErrCode == 0) then
					rvDir(i) = rvValues(ivClosest(1))
				else
					rvDir(i) = NaN
				end if
			else
				rvDir(i) = NaN
			end if
			
			! Get error lines for antennas 1 and 2
			j = this % getLineIndex(i, "ER1")
			if(j > 0) then
				iErrCode = GetErrors(this % svLines(j), this % iSensorType, ivErrors)
				if(iErrCode == 0) then
					ivErr1(i) = ivErrors(ivClosest(1))
				else
					ivErr1(i) = 0
				end if
			else
				ivErr1(i) = 0
			end if
			j = this % getLineIndex(i, "ER2")
			if(j > 0) then
				iErrCode = GetErrors(this % svLines(j), this % iSensorType, ivErrors)
				if(iErrCode == 0) then
					ivErr2(i) = ivErrors(ivClosest(1))
				else
					ivErr2(i) = 0
				end if
			else
				ivErr2(i) = 0
			end if
			
			! Invalidate value, if error for this data match with error mask
			if(ior(iand(ivErr1(i), this % ivErrorMask(1)), iand(ivErr2(i), this % ivErrorMask(2))) > 0) then
				rvVel(i) = NaN
				rvDir(i) = NaN
			end if
			
		end do

	end function mds_SodarWindSeries
	
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
			iFrom  = 3 + (i-1)*iFieldLen + 1
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
			iFrom  = 3 + (i-1)*iFieldLen + 1
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
			iFrom  = 3 + (i-1)*iFieldLen + 1
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

