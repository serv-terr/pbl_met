! Unit test program for sampling and selection functions
program test_sampling

    use pbl_met

    implicit none

    ! Locals
    integer, dimension(:), allocatable  :: ivSampleIdx
    real, dimension(:), allocatable     :: rvP4
    real, dimension(:), allocatable     :: rvA4
    real, dimension(:), allocatable     :: rvS4
    real(8), dimension(:), allocatable  :: rvP8
    real(8), dimension(:), allocatable  :: rvA8
    real(8), dimension(:), allocatable  :: rvS8
    integer                             :: iRetCode
    integer                             :: n, m
    integer                             :: i

    ! Tests of sampling index

    ! Test 1 - Without repetitions, as of default
    n = 10
    m = 5
    print *, "Test 1"
    print *, "Expected return code = 0"
    print *, "Sample: ", m, " values"
    do i = 1, 5
        iRetCode = Sample(n, m, ivSampleIdx)
        print *, iRetCode, ' - ', ivSampleIdx
    end do
    print *

    ! Test 2 - Without repetitions, as of request
    n = 10
    m = 5
    print *, "Test 2"
    print *, "Expected return code = 0"
    print *, "Sample: ", m, " values"
    do i = 1, 5
        iRetCode = Sample(n, m, ivSampleIdx, iSampleType=SAMPLING_WITHOUT_REPETITIONS)
        print *, iRetCode, ' - ', ivSampleIdx
    end do
    print *

    ! Test 3 - With repetitions, as of request
    n = 10
    m = 5
    print *, "Test 3"
    print *, "Expected return code = 0"
    print *, "Sample: ", m, " values"
    do i = 1, 5
        iRetCode = Sample(n, m, ivSampleIdx, iSampleType=SAMPLING_WITH_REPETITIONS)
        print *, iRetCode, ' - ', ivSampleIdx
    end do
    print *

    ! Test 4 - m > n
    n = 10
    m = 15
    print *, "Test 4"
    iRetCode = Sample(n, m, ivSampleIdx)
    print *, "Expected return code /= 0 - Actual: ", iRetCode
    print *

    ! Test 5 - n < 0
    n = -10
    m =   5
    print *, "Test 5"
    iRetCode = Sample(n, m, ivSampleIdx)
    print *, "Expected return code /= 0 - Actual: ", iRetCode
    print *

    ! Test 6 - m < 0
    n =  10
    m =  -5
    print *, "Test 6"
    iRetCode = Sample(n, m, ivSampleIdx)
    print *, "Expected return code /= 0 - Actual: ", iRetCode
    print *

    ! Test 7 - Invalid sampling type
    n =  10
    m =   5
    print *, "Test 7"
    iRetCode = Sample(n, m, ivSampleIdx, iSampleType = 5)
    print *, "Expected return code /= 0 - Actual: ", iRetCode
    print *

    ! Test of sampling - real*4

    ! Test 1 - Without repetitions, as of default
    n = 5
    m = 3
    print *, "Test 1"
    if(allocated(rvP4)) deallocate(rvP4)
    allocate(rvP4(n))
    call random_number(rvP4)
    print *, "Expected return code = 0"
    print *, "Sample: ", m, " values"
    do i = 1, 5
        iRetCode = Sample(rvP4, m, rvS4)
        print *, iRetCode, ' - ', rvS4
    end do
    print *

    ! Test 2 - Without repetitions, as of request
    n = 5
    m = 3
    print *, "Test 2"
    if(allocated(rvP4)) deallocate(rvP4)
    allocate(rvP4(n))
    call random_number(rvP4)
    print *, "Expected return code = 0"
    print *, "Sample: ", m, " values"
    do i = 1, 5
        iRetCode = Sample(rvP4, m, rvS4, iSampleType = SAMPLING_WITHOUT_REPETITIONS)
        print *, iRetCode, ' - ', rvS4
    end do
    print *

    ! Test 3 - With repetitions, as of request
    n = 5
    m = 3
    print *, "Test 3"
    if(allocated(rvP4)) deallocate(rvP4)
    allocate(rvP4(n))
    call random_number(rvP4)
    print *, "Expected return code = 0"
    print *, "Sample: ", m, " values"
    do i = 1, 5
        iRetCode = Sample(rvP4, m, rvS4, iSampleType = SAMPLING_WITH_REPETITIONS)
        print *, iRetCode, ' - ', rvS4
    end do
    print *

    ! Test 4 - Excessive sample size
    n = 5
    m = 13
    print *, "Test 4"
    if(allocated(rvP4)) deallocate(rvP4)
    allocate(rvP4(n))
    call random_number(rvP4)
    iRetCode = Sample(rvP4, m, rvS4)
    print *, "Sample: ", m, " values"
    print *, "Expected return code /= 0 - Actual = ", iRetCode
    print *

    ! Test 5 - Empty population size
    n = 0
    m = 3
    print *, "Test 5"
    if(allocated(rvP4)) deallocate(rvP4)
    allocate(rvP4(n))
    call random_number(rvP4)
    iRetCode = Sample(rvP4, m, rvS4)
    print *, "Sample: ", m, " values"
    print *, "Expected return code /= 0 - Actual = ", iRetCode
    print *

    ! Test 6 - Negative m
    n = 5
    m = -3
    print *, "Test 6"
    if(allocated(rvP4)) deallocate(rvP4)
    allocate(rvP4(n))
    call random_number(rvP4)
    iRetCode = Sample(rvP4, m, rvS4)
    print *, "Sample: ", m, " values"
    print *, "Expected return code /= 0 - Actual = ", iRetCode
    print *

    ! Test 7 - Invalid sample type
    n = 5
    m = 3
    print *, "Test 7"
    if(allocated(rvP4)) deallocate(rvP4)
    allocate(rvP4(n))
    call random_number(rvP4)
    iRetCode = Sample(rvP4, m, rvS4, iSampleType = -8)
    print *, "Sample: ", m, " values"
    print *, "Expected return code /= 0 - Actual = ", iRetCode
    print *

    ! Test of sampling - real*8

    ! Test 1 - Without repetitions, as of default
    n = 5
    m = 3
    print *, "Test 1"
    if(allocated(rvP8)) deallocate(rvP8)
    allocate(rvP8(n))
    call random_number(rvP8)
    print *, "Expected return code = 0"
    print *, "Sample: ", m, " values"
    do i = 1, 5
        iRetCode = Sample(rvP8, m, rvS8)
        print *, iRetCode, ' - ', rvS8
    end do
    print *

    ! Test 2 - Without repetitions, as of request
    n = 5
    m = 3
    print *, "Test 2"
    if(allocated(rvP8)) deallocate(rvP8)
    allocate(rvP8(n))
    call random_number(rvP8)
    print *, "Expected return code = 0"
    print *, "Sample: ", m, " values"
    do i = 1, 5
        iRetCode = Sample(rvP8, m, rvS8, iSampleType = SAMPLING_WITHOUT_REPETITIONS)
        print *, iRetCode, ' - ', rvS8
    end do
    print *

    ! Test 3 - With repetitions, as of request
    n = 5
    m = 3
    print *, "Test 3"
    if(allocated(rvP8)) deallocate(rvP8)
    allocate(rvP8(n))
    call random_number(rvP8)
    print *, "Expected return code = 0"
    print *, "Sample: ", m, " values"
    do i = 1, 5
        iRetCode = Sample(rvP8, m, rvS8, iSampleType = SAMPLING_WITH_REPETITIONS)
        print *, iRetCode, ' - ', rvS8
    end do
    print *

    ! Test 4 - Excessive sample size
    n = 5
    m = 13
    print *, "Test 4"
    if(allocated(rvP8)) deallocate(rvP8)
    allocate(rvP8(n))
    call random_number(rvP8)
    iRetCode = Sample(rvP8, m, rvS8)
    print *, "Sample: ", m, " values"
    print *, "Expected return code /= 0 - Actual = ", iRetCode
    print *

    ! Test 5 - Empty population size
    n = 0
    m = 3
    print *, "Test 5"
    if(allocated(rvP8)) deallocate(rvP8)
    allocate(rvP8(n))
    call random_number(rvP8)
    iRetCode = Sample(rvP8, m, rvS8)
    print *, "Sample: ", m, " values"
    print *, "Expected return code /= 0 - Actual = ", iRetCode
    print *

    ! Test 6 - Negative m
    n = 5
    m = -3
    print *, "Test 6"
    if(allocated(rvP8)) deallocate(rvP8)
    allocate(rvP8(n))
    call random_number(rvP8)
    iRetCode = Sample(rvP8, m, rvS8)
    print *, "Sample: ", m, " values"
    print *, "Expected return code /= 0 - Actual = ", iRetCode
    print *

    ! Test 7 - Invalid sample type
    n = 5
    m = 3
    print *, "Test 7"
    if(allocated(rvP8)) deallocate(rvP8)
    allocate(rvP8(n))
    call random_number(rvP8)
    iRetCode = Sample(rvP8, m, rvS8, iSampleType = -8)
    print *, "Sample: ", m, " values"
    print *, "Expected return code /= 0 - Actual = ", iRetCode
    print *

    ! Test of selection - real*4

    ! Test 1 - Without repetitions, as of default
    n = 10
    print *, "Test 1"
    if(allocated(rvP4)) deallocate(rvP4)
    allocate(rvP4(n))
    if(allocated(rvA4)) deallocate(rvA4)
    allocate(rvA4(n))
    do i = 1, n
        rvP4(i) = i
    end do
    rvA4 = 0.
    rvA4(5) = 1.
    rvA4(6) = 2.
    print *, "Expected return code = 0"
    iRetCode = Select(rvP4, rvA4, 0.5, NaN, rvS4)
    print *, iRetCode, ' - ', size(rvS4), ' - ', rvS4
    print *

    ! Test 2 - Without repetitions, as of default
    n = 10
    print *, "Test 2"
    if(allocated(rvP4)) deallocate(rvP4)
    allocate(rvP4(n))
    if(allocated(rvA4)) deallocate(rvA4)
    allocate(rvA4(n))
    do i = 1, n
        rvP4(i) = i
    end do
    rvA4 = 0.
    rvA4(5) = 1.
    rvA4(6) = 2.
    print *, "Expected return code = 0"
    iRetCode = Select(rvP4, rvA4, NaN, 0.5, rvS4)
    print *, iRetCode, ' - ', size(rvS4), ' - ', rvS4
    print *

    ! Test 3 - Without repetitions, as of default
    n = 10
    print *, "Test 3"
    if(allocated(rvP4)) deallocate(rvP4)
    allocate(rvP4(n))
    if(allocated(rvA4)) deallocate(rvA4)
    allocate(rvA4(n))
    do i = 1, n
        rvP4(i) = i
    end do
    rvA4 = 0.
    rvA4(5) = 1.
    rvA4(6) = 2.
    print *, "Expected return code = 0"
    iRetCode = Select(rvP4, rvA4, 0.5, 1.5, rvS4)
    print *, iRetCode, ' - ', size(rvS4), ' - ', rvS4
    print *

    ! Test 4 - Without repetitions, as of default
    n = 10
    print *, "Test 4"
    if(allocated(rvP4)) deallocate(rvP4)
    allocate(rvP4(n))
    if(allocated(rvA4)) deallocate(rvA4)
    allocate(rvA4(n))
    do i = 1, n
        rvP4(i) = i
    end do
    rvA4 = 0.
    rvA4(5) = 1.
    rvA4(6) = 2.
    print *, "Expected return code = 0"
    iRetCode = Select(rvP4, rvA4, 1.0, 1.0, rvS4)
    print *, iRetCode, ' - ', size(rvS4), ' - ', rvS4
    print *

    ! Test 4 - Without repetitions, as of default
    n = 10
    print *, "Test 4"
    if(allocated(rvP4)) deallocate(rvP4)
    allocate(rvP4(n))
    if(allocated(rvA4)) deallocate(rvA4)
    allocate(rvA4(n))
    do i = 1, n
        rvP4(i) = i
    end do
    rvA4 = 0.
    rvA4(5) = 1.
    rvA4(6) = 2.
    print *, "Expected return code = 0"
    iRetCode = Select(rvP4, rvA4, 3.0, NaN, rvS4)
    print *, iRetCode, ' - ', size(rvS4), ' - ', rvS4
    print *

end program test_sampling
