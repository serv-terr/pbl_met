! Module Modos - Access to Metek "MODOS" SODAR, SODAR/RASS and MRR2 data.
!
! This module is part of the pbl_met library.
!
! Copyright 2018 by Servizi Territorio srl
!
! This is open-source code, covered by lGPL 3.0 license
!
module Modos

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
		integer, private										:: iSensorType	! MDS_SODAR, MDS_SODAR_RASS, MDS_MRR2
		integer, dimension(:), allocatable, private				:: ivBlockIdx	! Indices of block starts
		integer, dimension(:), allocatable, private				:: ivBlockLen	! Indices of block lengths
		integer, dimension(6)									:: ivErrorMask	! Error mask for each antenna
	contains
		procedure	:: load	         => mds_load
		procedure	:: getSensorType => mds_getSensorType
		procedure	:: setErrorMasks => mds_setErrorMasks
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

end module Modos

