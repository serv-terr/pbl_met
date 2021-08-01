! Unit test program for sampling and selection functions
program test_sampling

    use pbl_met

    implicit none

    ! Locals
    integer, dimension(:), allocatable  :: ivSampleIdx
    real, dimension(:), allocatable     :: rvP4
    real, dimension(:), allocatable     :: rvS4
    real(8), dimension(:), allocatable  :: rvP8
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

end program test_sampling
