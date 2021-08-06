! Unit test program for directional statistics functions
program test_dir

    use pbl_met

    implicit none

    ! Locals
    real, dimension(:), allocatable         :: rvDir4
    real                                    :: rDir4
    real(8), dimension(:), allocatable      :: rvDir8
    real(8)                                 :: rDir8
    integer                                 :: iRetCode
    integer                                 :: n
    integer                                 :: i

    ! Tests of direction mean

    ! Test 1 - Without invalids
    n = 10
    print *, "Test 1-4"
    allocate(rvDir4(n))
    do i = 0, 360, 45
        rvDir4 = i
        rDir4 = AngleMean(rvDir4)
        print *, 'Expected direction = ', i, '   Computed direction = ', rDir4
    end do
    print *
    print *, "Test 1-8"
    allocate(rvDir8(n))
    do i = 0, 360, 45
        rvDir8 = i
        rDir8 = AngleMean(rvDir8)
        print *, 'Expected direction = ', i, '   Computed direction = ', rDir8
    end do
    print *

end program test_dir
