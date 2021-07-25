! Unit test program for statistical validation indices
program test_validate

    use pbl_met

    implicit none

    ! Locals
    real, dimension(:), allocatable     :: rvO4
    real, dimension(:), allocatable     :: rvP4
    real(8), dimension(:), allocatable  :: rvO8
    real(8), dimension(:), allocatable  :: rvP8
    real                                :: rFB_4
    real(8)                             :: rFB_8
    real                                :: rFAC2_4
    real(8)                             :: rFAC2_8
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

end program test_validate
