! pbl_stat  : Fortran module, providing elementary support to statistical
!             functions.
!
! Nothing really original here, just routines used in support of
! eddy covariance and other statistically-grounded calculations.
!
module pbl_stat

    use pbl_base

    implicit none
    
    private
    
    ! Public interface
    public  :: Cov
    
contains

    ! Compute covariance between two signal samples; these samples should
    ! be the same size, and "error-paired", that is, whenever rvX(i) == NaN,
    ! then rvY(i) == NaN, and vice-versa.
	function Cov(rvX, rvY) result(rCov)
	
		! Routine arguments
		real, dimension(:), intent(in)	:: rvX
		real, dimension(:), intent(in)	:: rvY
		real							:: rCov
		
		! Locals
        integer :: n
		real	:: rAvgX
		real	:: rAvgY
        
        ! Check it makes sense to proceed
        n = count(.not.isnan(rvX))  ! Valid also for 'rvY' since the error-pairing assumption
        if(n <= 0) then
            rCov = NaN
            return
        end if
		
		! Compute averages
		rAvgX = sum(rvX, mask=.not.isnan(rvX))/n
		rAvgY = sum(rvY, mask=.not.isnan(rvY))/n
		
		! Compute the covariance (using a very simpli definition):
		rCov = dot_product(rvX-rAvgX, rvY-rAvgY, mask=.not.isnan(rvX))/n
		
	end function Cov
	
end module pbl_stat
