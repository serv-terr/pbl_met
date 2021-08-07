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

    ! Test 2 - With some invalids
    n = 10
    print *, "Test 2-4"
    if(allocated(rvDir4)) deallocate(rvDir4)
    allocate(rvDir4(n))
    do i = 0, 360, 45
        rvDir4 = i
        rvDir4(5) = NaN
        rvDir4(6) = NaN
        rDir4 = AngleMean(rvDir4)
        print *, 'Expected direction = ', i, '   Computed direction = ', rDir4, '   Expected = about ', i
    end do
    print *
    print *, "Test 2-8"
    if(allocated(rvDir8)) deallocate(rvDir8)
    allocate(rvDir8(n))
    do i = 0, 360, 45
        rvDir8 = i
        rvDir8(5) = NaN_8
        rvDir8(6) = NaN_8
        rDir8 = AngleMean(rvDir8)
        print *, 'Expected direction = ', i, '   Computed direction = ', rDir8, '   Expected = about ', i
    end do
    print *

    ! Test 3 - All invalids
    n = 10
    print *, "Test 3-4"
    if(allocated(rvDir4)) deallocate(rvDir4)
    allocate(rvDir4(n))
    rvDir4 = NaN
    rDir4 = AngleMean(rvDir4)
    print *, 'Result = ', rDir4, '    Expected = NaN '
    print *
    print *, "Test 3-8"
    if(allocated(rvDir8)) deallocate(rvDir8)
    allocate(rvDir8(n))
    rvDir8 = NaN_8
    rDir8 = AngleMean(rvDir8)
    print *, 'Result = ', rDir8, '    Expected = NaN '
    print *

    ! Test 4 - Empty vectors
    n = 0
    print *, "Test 4-4"
    if(allocated(rvDir4)) deallocate(rvDir4)
    allocate(rvDir4(n))
    rDir4 = AngleMean(rvDir4)
    print *, 'Result = ', rDir4, '    Expected = NaN '
    print *
    print *, "Test 4-8"
    if(allocated(rvDir8)) deallocate(rvDir8)
    allocate(rvDir8(n))
    rDir8 = AngleMean(rvDir8)
    print *, 'Result = ', rDir8, '    Expected = NaN '
    print *

    ! Test 5 - Without invalids, but with noise
    n = 10
    print *, "Test 5-4"
    if(allocated(rvDir4)) deallocate(rvDir4)
    allocate(rvDir4(n))
    do i = 0, 360, 45
        call random_number(rvDir4)
        rvDir4 = rvDir4 * 2. - 1.
        rvDir4 = rvDir4 + i
        rDir4 = AngleMean(rvDir4)
        print *, 'Expected direction = ', i, '   Computed direction = ', rDir4
    end do
    print *
    print *, "Test 5-8"
    if(allocated(rvDir8)) deallocate(rvDir8)
    allocate(rvDir8(n))
    do i = 0, 360, 45
        call random_number(rvDir8)
        rvDir8 = rvDir8 * 2. - 1.
        rvDir8 = rvDir8 + i
        rDir8 = AngleMean(rvDir8)
        print *, 'Expected direction = ', i, '   Computed direction = ', rDir8
    end do
    print *

end program test_dir
