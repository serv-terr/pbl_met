! Unit test program for statistical validation indices
program test_validate

    use pbl_met

    implicit none

    ! Locals
    real, dimension(:), allocatable     :: rvO4
    real, dimension(:), allocatable     :: rvP4
    real(8), dimension(:), allocatable  :: rvO8
    real(8), dimension(:), allocatable  :: rvP8
    real                                :: rFAC2_4
    real(8)                             :: rFAC2_8
    real                                :: rFB_4
    real(8)                             :: rFB_8
    real                                :: rNMSE_4
    real(8)                             :: rNMSE_8
    real                                :: rR_4
    real(8)                             :: rR_8
    integer                             :: i

    ! Tests on FAC2

    ! Test 1. Identical vectors
    allocate(rvO4(16), rvP4(16))
    allocate(rvO8(16), rvP8(16))
    call random_number(rvO4)
    rvP4 = rvO4
    call random_number(rvO8)
    rvP8 = rvO8
    rFAC2_4 = FAC2(rvO4, rvP4)
    rFAC2_8 = FAC2(rvO8, rvP8)
    print *, "Test 1"
    print *, "FAC2(rvX,rvX) = ", rFAC2_4, "   Expected: 1.0"
    print *, "FAC2(rvX,rvX) = ", rFAC2_8, "   Expected: 1.0"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 2. Similar vectors
    allocate(rvO4(16), rvP4(16))
    allocate(rvO8(16), rvP8(16))
    call random_number(rvO4)
    rvP4 = 4. * rvO4
    call random_number(rvO8)
    rvP8 = 4.d0 * rvO8
    rFAC2_4 = FAC2(rvO4, rvP4)
    rFAC2_8 = FAC2(rvO8, rvP8)
    print *, "Test 2"
    print *, "FAC2(rvX,4*rvX) = ", rFAC2_4, "   Expected: 0.0"
    print *, "FAC2(rvX,4*rvX) = ", rFAC2_8, "   Expected: 0.0"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 3. Similar vectors, extended range
    allocate(rvO4(16), rvP4(16))
    allocate(rvO8(16), rvP8(16))
    call random_number(rvO4)
    rvP4 = 4. * rvO4
    call random_number(rvO8)
    rvP8 = 4.d0 * rvO8
    rFAC2_4 = FAC2(rvO4, rvP4, rFactorIn = 5.)
    rFAC2_8 = FAC2(rvO8, rvP8, rFactorIn = 5.d0)
    print *, "Test 3"
    print *, "FAC5(rvX,4*rvX) = ", rFAC2_4, "   Expected: 1.0"
    print *, "FAC5(rvX,4*rvX) = ", rFAC2_8, "   Expected: 1.0"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 4. Identical vectors with negatives
    allocate(rvO4(16), rvP4(16))
    allocate(rvO8(16), rvP8(16))
    call random_number(rvO4)
    rvO4 = rvO4 - 0.5
    rvP4 = rvO4
    call random_number(rvO8)
    rvO8 = rvO8 - 0.5
    rvP8 = rvO8
    rFAC2_4 = FAC2(rvO4, rvP4)
    rFAC2_8 = FAC2(rvO8, rvP8)
    print *, "Test 4"
    print *, "FAC2(rvX,rvX) = ", rFAC2_4, "   Expected: 1.0"
    print *, "FAC2(rvX,rvX) = ", rFAC2_8, "   Expected: 1.0"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 5. Opposite vectors with negatives
    allocate(rvO4(16), rvP4(16))
    allocate(rvO8(16), rvP8(16))
    call random_number(rvO4)
    rvO4 =  rvO4 - 0.5
    rvP4 = -rvO4
    call random_number(rvO8)
    rvO8 =  rvO8 - 0.5
    rvP8 = -rvO8
    rFAC2_4 = FAC2(rvO4, rvP4)
    rFAC2_8 = FAC2(rvO8, rvP8)
    print *, "Test 5"
    print *, "FAC2(rvX,-rvX) = ", rFAC2_4, "   Expected: 0.0"
    print *, "FAC2(rvX,-rvX) = ", rFAC2_8, "   Expected: 0.0"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 6. Translated vectors
    allocate(rvO4(16), rvP4(10))
    allocate(rvO8(16), rvP8(10))
    rvO4 = [(real(i, kind=4) - 0.5, i=1,10)]
    rvP4 = [(real(i, kind=4) + 0.5, i=1,10)]
    rvO8 = [(real(i, kind=8) - 0.5d0, i=1,10)]
    rvP8 = [(real(i, kind=8) + 0.5d0, i=1,10)]
    rFAC2_4 = FAC2(rvO4, rvP4)
    rFAC2_8 = FAC2(rvO8, rvP8)
    print *, "Test 6"
    print *, "FAC2(rvX-0.5,rvY+0.5) = ", rFAC2_4, "   Expected: < 1.0"
    print *, "FAC2(rvX-0.5,rvY+0.5) = ", rFAC2_8, "   Expected: < 1.0"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 7. Vectors of differing lengths
    allocate(rvO4(16), rvP4(16))
    allocate(rvO8(16), rvP8(16))
    rvO4 = [(real(i, kind=4) - 0.5, i=1,10)]
    rvP4 = [(real(i, kind=4) + 0.5, i=1,16)]
    rvO8 = [(real(i, kind=8) - 0.5d0, i=1,10)]
    rvP8 = [(real(i, kind=8) + 0.5d0, i=1,16)]
    rFAC2_4 = FAC2(rvO4, rvP4)
    rFAC2_8 = FAC2(rvO8, rvP8)
    print *, "Test 7"
    print *, "FAC2(rvX(10),rvY(16)) = ", rFAC2_4, "   Expected: NaN"
    print *, "FAC2(rvX(10),rvY(16)) = ", rFAC2_8, "   Expected: NaN"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 8. Identical vectors with NaN values
    allocate(rvO4(16), rvP4(16))
    allocate(rvO8(16), rvP8(16))
    call random_number(rvO4)
    rvP4 = rvO4
    rvO4(3) = NaN
    rvP4(4) = NaN
    call random_number(rvO8)
    rvP8 = rvO8
    rvO8(3) = NaN_8
    rvP8(4) = NaN_8
    rFAC2_4 = FAC2(rvO4, rvP4)
    rFAC2_8 = FAC2(rvO8, rvP8)
    print *, "Test 8"
    print *, "FAC2(rvX,rvX) = ", rFAC2_4, "   Expected: 1.0"
    print *, "FAC2(rvX,rvX) = ", rFAC2_8, "   Expected: 1.0"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 9. NaN vectors
    allocate(rvO4(16), rvP4(16))
    allocate(rvO8(16), rvP8(16))
    rvO4 = NaN
    rvP4 = NaN
    rvO8 = NaN_8
    rvP8 = NaN_8
    rFAC2_4 = FAC2(rvO4, rvP4)
    rFAC2_8 = FAC2(rvO8, rvP8)
    print *, "Test 9"
    print *, "FAC2(NaN,NaN) = ", rFAC2_4, "   Expected: NaN"
    print *, "FAC2(NaN,NaN) = ", rFAC2_8, "   Expected: NaN"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 10. one NaN and a non-NaN vectors
    allocate(rvO4(16), rvP4(16))
    allocate(rvO8(16), rvP8(16))
    call random_number(rvO4)
    rvP4 = NaN
    call random_number(rvO8)
    rvP8 = NaN_8
    rFAC2_4 = FAC2(rvO4, rvP4)
    rFAC2_8 = FAC2(rvO8, rvP8)
    print *, "Test 10"
    print *, "FAC2(rvO,NaN) = ", rFAC2_4, "   Expected: NaN"
    print *, "FAC2(rvO,NaN) = ", rFAC2_8, "   Expected: NaN"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 11. one NaN and a non-NaN vectors
    allocate(rvO4(16), rvP4(16))
    allocate(rvO8(16), rvP8(16))
    rvO4 = NaN
    call random_number(rvP4)
    rvO8 = NaN_8
    call random_number(rvP8)
    rFAC2_4 = FAC2(rvO4, rvP4)
    rFAC2_8 = FAC2(rvO8, rvP8)
    print *, "Test 11"
    print *, "FAC2(NaN,rvP) = ", rFAC2_4, "   Expected: NaN"
    print *, "FAC2(NaN,rvP) = ", rFAC2_8, "   Expected: NaN"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Tests on FB

    ! Test 1. Identical vectors
    allocate(rvO4(16), rvP4(16))
    allocate(rvO8(16), rvP8(16))
    call random_number(rvO4)
    rvP4 = rvO4
    call random_number(rvO8)
    rvP8 = rvO8
    rFB_4 = FB(rvO4, rvP4)
    rFB_8 = FB(rvO8, rvP8)
    print *, "Test 1"
    print *, "FB(rvX,rvX) = ", rFB_4, "   Expected: 0.0"
    print *, "FB(rvX,rvX) = ", rFB_8, "   Expected: 0.0"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 2. Identical vectors with NaN
    allocate(rvO4(16), rvP4(16))
    allocate(rvO8(16), rvP8(16))
    call random_number(rvO4)
    rvP4 = rvO4
    rvO4( 4) = NaN
    rvP4(10) = NaN
    call random_number(rvO8)
    rvP8 = rvO8
    rvO8( 4) = NaN_8
    rvP8(10) = NaN_8
    rFB_4 = FB(rvO4, rvP4)
    rFB_8 = FB(rvO8, rvP8)
    print *, "Test 2"
    print *, "FB(rvX,rvX) = ", rFB_4, "   Expected: 0.0"
    print *, "FB(rvX,rvX) = ", rFB_8, "   Expected: 0.0"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 3. Scaled vectors
    allocate(rvO4(1638400), rvP4(1638400))
    allocate(rvO8(1638400), rvP8(1638400))
    call random_number(rvO4)
    rvP4 = rvO4 * 2.0
    call random_number(rvO8)
    rvP8 = rvO8 * 2.0d0
    rFB_4 = FB(rvO4, rvP4)
    rFB_8 = FB(rvO8, rvP8)
    print *, "Test 3"
    print *, "FB(rvX,2*rvX) = ", rFB_4, "   Expected: about 0.25"
    print *, "FB(rvX,2*rvX) = ", rFB_8, "   Expected: about 0.25"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 4. Translated vectors
    allocate(rvO4(1638400), rvP4(1638400))
    allocate(rvO8(1638400), rvP8(1638400))
    call random_number(rvO4)
    rvP4 = rvO4 + 1.0
    call random_number(rvO8)
    rvP8 = rvO8 + 1.0d0
    rFB_4 = FB(rvO4, rvP4)
    rFB_8 = FB(rvO8, rvP8)
    print *, "Test 4"
    print *, "FB(rvX,rvX+1) = ", rFB_4, "   Expected: about -0.5"
    print *, "FB(rvX,rvX+1) = ", rFB_8, "   Expected: about -0.5"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 5. Independent uniformly distributed vectors
    allocate(rvO4(1638400), rvP4(1638400))
    allocate(rvO8(1638400), rvP8(1638400))
    call random_number(rvO4)
    call random_number(rvP4)
    call random_number(rvO8)
    call random_number(rvP8)
    rFB_4 = FB(rvO4, rvP4)
    rFB_8 = FB(rvO8, rvP8)
    print *, "Test 5"
    print *, "FB(rvX,rvY) = ", rFB_4, "   Expected: about -0.5"
    print *, "FB(rvX,rvY) = ", rFB_8, "   Expected: about -0.5"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 6. Independent uniformly distributed vectors having different lengths
    allocate(rvO4(16), rvP4(32))
    allocate(rvO8(16), rvP8(32))
    call random_number(rvO4)
    call random_number(rvP4)
    call random_number(rvO8)
    call random_number(rvP8)
    rFB_4 = FB(rvO4, rvP4)
    rFB_8 = FB(rvO8, rvP8)
    print *, "Test 6"
    print *, "FB(rvX,rvY) = ", rFB_4, "   Expected: NaN"
    print *, "FB(rvX,rvY) = ", rFB_8, "   Expected: NaN"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 7. Independent uniformly distributed vectors with some NaN
    allocate(rvO4(32), rvP4(32))
    allocate(rvO8(32), rvP8(32))
    call random_number(rvO4)
    call random_number(rvP4)
    rvO4(3) = NaN
    rvP4(8) = NaN
    call random_number(rvO8)
    call random_number(rvP8)
    rvO8(3) = NaN_8
    rvP8(8) = NaN_8
    rFB_4 = FB(rvO4, rvP4)
    rFB_8 = FB(rvO8, rvP8)
    print *, "Test 7"
    print *, "FB(rvX,rvY) = ", rFB_4, "   Expected: about 0"
    print *, "FB(rvX,rvY) = ", rFB_8, "   Expected: about 0"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 8. One NaN vector
    allocate(rvO4(32), rvP4(32))
    allocate(rvO8(32), rvP8(32))
    call random_number(rvO4)
    rvP4 = NaN
    call random_number(rvO8)
    rvP8 = NaN_8
    rFB_4 = FB(rvO4, rvP4)
    rFB_8 = FB(rvO8, rvP8)
    print *, "Test 8"
    print *, "FB(rvX,NaN) = ", rFB_4, "   Expected: NaN"
    print *, "FB(rvX,NaN) = ", rFB_8, "   Expected: NaN"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 9. Identical vectors, with negatives
    allocate(rvO4(32), rvP4(32))
    allocate(rvO8(32), rvP8(32))
    call random_number(rvO4)
    call random_number(rvO8)
    rvO4 = rvO4 - 0.5
    rvP4 = rvO4
    rvO8 = rvO8 - 0.5d0
    rvP8 = rvO8
    rFB_4 = FB(rvO4, rvP4)
    rFB_8 = FB(rvO8, rvP8)
    print *, "Test 9"
    print *, "FB(rvX,rvX) = ", rFB_4, "   Expected: 0"
    print *, "FB(rvX,rvX) = ", rFB_8, "   Expected: 0"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 10. Identical vectors, with negatines
    allocate(rvO4(65536), rvP4(65536))
    allocate(rvO8(65536), rvP8(65536))
    call random_number(rvO4)
    call random_number(rvP4)
    call random_number(rvO8)
    call random_number(rvP8)
    rvO4 = rvO4 - 0.5
    rvP4 = rvP4 - 0.5
    rvO8 = rvO8 - 0.5d0
    rvP8 = rvP8 - 0.5d0
    rFB_4 = FB(rvO4, rvP4)
    rFB_8 = FB(rvO8, rvP8)
    print *, "Test 10"
    print *, "FB(rvX,rvY) = ", rFB_4, "   Expected: 0"
    print *, "FB(rvX,rvY) = ", rFB_8, "   Expected: 0"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Tests on NMSE

    ! Test 1. Identical vectors
    allocate(rvO4(16), rvP4(16))
    allocate(rvO8(16), rvP8(16))
    call random_number(rvO4)
    rvP4 = rvO4
    call random_number(rvO8)
    rvP8 = rvO8
    rNMSE_4 = NMSE(rvO4, rvP4)
    rNMSE_8 = NMSE(rvO8, rvP8)
    print *, "Test 1"
    print *, "NMSE(rvX,rvX) = ", rNMSE_4, "   Expected: 0.0"
    print *, "NMSE(rvX,rvX) = ", rNMSE_8, "   Expected: 0.0"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 2. Identical vectors with NaN
    allocate(rvO4(16), rvP4(16))
    allocate(rvO8(16), rvP8(16))
    call random_number(rvO4)
    rvP4 = rvO4
    rvO4( 4) = NaN
    rvP4(10) = NaN
    call random_number(rvO8)
    rvP8 = rvO8
    rvO8( 4) = NaN_8
    rvP8(10) = NaN_8
    rNMSE_4 = NMSE(rvO4, rvP4)
    rNMSE_8 = NMSE(rvO8, rvP8)
    print *, "Test 2"
    print *, "NMSE(rvX,rvX) = ", rNMSE_4, "   Expected: 0.0"
    print *, "NMSE(rvX,rvX) = ", rNMSE_8, "   Expected: 0.0"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 3. Scaled vectors
    allocate(rvO4(1638400), rvP4(1638400))
    allocate(rvO8(1638400), rvP8(1638400))
    call random_number(rvO4)
    rvP4 = rvO4 * 2.0
    call random_number(rvO8)
    rvP8 = rvO8 * 2.0d0
    rNMSE_4 = NMSE(rvO4, rvP4)
    rNMSE_8 = NMSE(rvO8, rvP8)
    print *, "Test 3"
    print *, "NMSE(rvX,2*rvX) = ", rNMSE_4, "   Expected: about 0.125"
    print *, "NMSE(rvX,2*rvX) = ", rNMSE_8, "   Expected: about 0.125"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 4. Translated vectors
    allocate(rvO4(1638400), rvP4(1638400))
    allocate(rvO8(1638400), rvP8(1638400))
    call random_number(rvO4)
    rvP4 = rvO4 + 1.0
    call random_number(rvO8)
    rvP8 = rvO8 + 1.0d0
    rNMSE_4 = NMSE(rvO4, rvP4)
    rNMSE_8 = NMSE(rvO8, rvP8)
    print *, "Test 4"
    print *, "NMSE(rvX,rvX+1) = ", rNMSE_4, "   Expected: about 0"
    print *, "NMSE(rvX,rvX+1) = ", rNMSE_8, "   Expected: about 0"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 5. Independent uniformly distributed vectors
    allocate(rvO4(1638400), rvP4(1638400))
    allocate(rvO8(1638400), rvP8(1638400))
    call random_number(rvO4)
    call random_number(rvP4)
    call random_number(rvO8)
    call random_number(rvP8)
    rNMSE_4 = NMSE(rvO4, rvP4)
    rNMSE_8 = NMSE(rvO8, rvP8)
    print *, "Test 5"
    print *, "NMSE(rvX,rvY) = ", rNMSE_4, "   Expected: about 0"
    print *, "NMSE(rvX,rvY) = ", rNMSE_8, "   Expected: about 0"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 6. Independent uniformly distributed vectors having different lengths
    allocate(rvO4(16), rvP4(32))
    allocate(rvO8(16), rvP8(32))
    call random_number(rvO4)
    call random_number(rvP4)
    call random_number(rvO8)
    call random_number(rvP8)
    rNMSE_4 = NMSE(rvO4, rvP4)
    rNMSE_8 = NMSE(rvO8, rvP8)
    print *, "Test 6"
    print *, "NMSE(rvX,rvY) = ", rNMSE_4, "   Expected: NaN"
    print *, "NMSE(rvX,rvY) = ", rNMSE_8, "   Expected: NaN"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 7. Independent uniformly distributed vectors with some NaN
    allocate(rvO4(32), rvP4(32))
    allocate(rvO8(32), rvP8(32))
    call random_number(rvO4)
    call random_number(rvP4)
    rvO4(3) = NaN
    rvP4(8) = NaN
    call random_number(rvO8)
    call random_number(rvP8)
    rvO8(3) = NaN_8
    rvP8(8) = NaN_8
    rNMSE_4 = NMSE(rvO4, rvP4)
    rNMSE_8 = NMSE(rvO8, rvP8)
    print *, "Test 7"
    print *, "NMSE(rvX,rvY) = ", rNMSE_4, "   Expected: about 0"
    print *, "NMSE(rvX,rvY) = ", rNMSE_8, "   Expected: about 0"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 8. One NaN vector
    allocate(rvO4(32), rvP4(32))
    allocate(rvO8(32), rvP8(32))
    call random_number(rvO4)
    rvP4 = NaN
    call random_number(rvO8)
    rvP8 = NaN_8
    rNMSE_4 = NMSE(rvO4, rvP4)
    rNMSE_8 = NMSE(rvO8, rvP8)
    print *, "Test 8"
    print *, "NMSE(rvX,NaN) = ", rNMSE_4, "   Expected: NaN"
    print *, "NMSE(rvX,NaN) = ", rNMSE_8, "   Expected: NaN"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 9. Identical vectors, with negatives
    allocate(rvO4(32), rvP4(32))
    allocate(rvO8(32), rvP8(32))
    call random_number(rvO4)
    call random_number(rvO8)
    rvO4 = rvO4 - 0.5
    rvP4 = rvO4
    rvO8 = rvO8 - 0.5d0
    rvP8 = rvO8
    rNMSE_4 = NMSE(rvO4, rvP4)
    rNMSE_8 = NMSE(rvO8, rvP8)
    print *, "Test 9"
    print *, "NMSE(rvX,rvX) = ", rNMSE_4, "   Expected: 0"
    print *, "NMSE(rvX,rvX) = ", rNMSE_8, "   Expected: 0"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Test 10. Identical vectors, with negatines
    allocate(rvO4(65536), rvP4(65536))
    allocate(rvO8(65536), rvP8(65536))
    call random_number(rvO4)
    call random_number(rvP4)
    call random_number(rvO8)
    call random_number(rvP8)
    rvO4 = rvO4 - 0.5
    rvP4 = rvP4 - 0.5
    rvO8 = rvO8 - 0.5d0
    rvP8 = rvP8 - 0.5d0
    rNMSE_4 = NMSE(rvO4, rvP4)
    rNMSE_8 = NMSE(rvO8, rvP8)
    print *, "Test 10"
    print *, "NMSE(rvX,rvY) = ", rNMSE_4, "   Expected: 0"
    print *, "NMSE(rvX,rvY) = ", rNMSE_8, "   Expected: 0"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

    ! Correlation coefficient

    ! Test 1. Identical vectors
    allocate(rvO4(16), rvP4(16))
    allocate(rvO8(16), rvP8(16))
    call random_number(rvO4)
    rvP4 = rvO4
    call random_number(rvO8)
    rvP8 = rvO8
    rR_4 = Corr(rvO4, rvP4)
    rR_8 = Corr(rvO8, rvP8)
    print *, "Test 1"
    print *, "R(rvX,rvX) = ", rR_4, "   Expected: 1.0"
    print *, "R(rvX,rvX) = ", rR_8, "   Expected: 1.0"
    print *
    deallocate(rvO8, rvP8)
    deallocate(rvO4, rvP4)

  

end program test_validate
