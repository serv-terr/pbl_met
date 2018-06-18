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
		character(len=512), dimension(:), allocatable	:: svLines
		integer											:: iSensorType	! MDS_SODAR, MDS_SODAR_RASS, MDS_MRR2
		integer, dimension(:), allocatable				:: ivBlockIdx	! Indices of block starts
		integer, dimension(:), allocatable				:: ivBlockLen	! Indices of block lengths
	end type ModosData

end module Modos

