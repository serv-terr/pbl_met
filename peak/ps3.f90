! Peak Sgamator 3.0
!
! Just an attempt, in the moment, based on AMPD method by Felix Scholkmann et al. in "An Efficient Algorithm for Automatic Peak Detection in 
! Noisy Periodic and Quasi-Periodic Signals", Algorithms 2012, 5, 588-603.
!
! Original implementation in Python, by Luca Cerina (https://github.com/LucaCerina/ampdLib)
!
! The translation to Fortran is mine. (Patti M. Favaron, 2019-12-26)
!
module ps3

	use pbl_met
	
	implicit none
	
	private
	
	! Public interface
	public	::	ampd
	
contains

	function ampd(rvX, rLimit) result(iRetCode)
	
		! Routine arguments
		real, dimension(:), intent(in)	:: rvX
		real, intent(in)				:: rLimit
		integer							:: iRetCode
		
		! Locals
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		
	end function ampd
	
end module ps3
