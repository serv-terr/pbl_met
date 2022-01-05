! multires_test2 - procedure for computing multires stats in various situations
program multires_test2
    
    use multires
    
    implicit none
    
    ! Steering constants (please do not change)
    integer, parameter  :: N      = 32768
    integer, parameter  :: HALVES =    10
    
    ! Locals
    type(signal)                        :: tMultires
    character(len=256)                  :: sOutData
    character(len=256)                  :: sOutStats
    integer                             :: iRetCode
    integer                             :: i
    real(8), dimension(:), allocatable  :: rvTimeStamp
    real, dimension(:), allocatable     :: rvSignal
    real, dimension(:), allocatable     :: rvApprox2
    real, dimension(:), allocatable     :: rvApprox5
    real, dimension(:), allocatable     :: rvApprox8
    real                                :: rOriginalVariance
    real, dimension(:), allocatable     :: rvVariance
    real                                :: rResidualVariance
    real                                :: rOriginalVariation
    real, dimension(:), allocatable     :: rvTotalVariation
    real                                :: rResidualVariation
    real                                :: rOriginalPartialVariation
    real, dimension(:), allocatable     :: rvPartialVariation
    real                                :: rResidualPartialVariation
    integer                             :: iLUN
    
    ! Allocate vectors and preset time stamps
    allocate(rvTimeStamp(N))
    allocate(rvSignal(N))
    rvTimeStamp = [(real(i-1, kind=8), i=1, N)]
    
    ! Test 1: constant function
    sOutData  = "test_data/t1_data.csv"
    sOutStats = "test_data/t1_stat.csv"
    call clean
    call constant
    iRetCode = tMultires % create(rvTimeStamp, rvSignal, iNumHalvingsIn=HALVES)
    iRetCode = tMultires % approximate(2, rvApprox2)
    iRetCode = tMultires % approximate(5, rvApprox5)
    iRetCode = tMultires % approximate(8, rvApprox8)
    iRetCode = tMultires % get_variances(rOriginalVariance, rvVariance, rResidualVariance)
    iRetCode = tMultires % get_total_variation(rOriginalVariation, rvTotalVariation, rResidualVariation)
    iRetCode = tMultires % get_partial_variation(rvPartialVariation)
    rOriginalPartialVariation = 0.
    rResidualPartialVariation = 0.
    open(newunit=iLUN, file=sOutData, status='unknown', action='write')
    write(iLUN, "('Time.Stamp, Original, Approx.2, Approx.5, Approx.8, Residual')")
    do i = 1, N
        write(iLUN, "(f6.0,5(',',e15.7))") &
            rvTimeStamp(i), &
            rvSignal(i), &
            rvApprox2(i), &
            rvApprox5(i), &
            rvApprox8(i), &
            tMultires % rvResidual(i)
    end do
    close(iLUN)
    open(newunit=iLUN, file=sOutStats, status='unknown', action='write')
    write(iLUN, "('Halving, Variance, Total.Variation, Partial.Variation')")
    write(iLUN, "('-1,', e15.7,2(',',e15.7))") rOriginalVariance, rOriginalVariation, rOriginalPartialVariation
    do i = 1, size(rvVariance)
        write(iLUN, "(i2, ',', e15.7,2(',',e15.7))") i-1, rvVariance(i), rvTotalVariation(i), rvPartialVariation(i)
    end do
    write(iLUN, "(i2, ',', e15.7,2(',',e15.7))") size(rvVariance), rResidualVariance, rResidualVariation, rResidualPartialVariation
    close(iLUN)
    print *, "Test 1"
    
    ! Test 2: order-1 stationary
    sOutData  = "test_data/t2_data.csv"
    sOutStats = "test_data/t2_stat.csv"
    call clean
    call noise
    iRetCode = tMultires % create(rvTimeStamp, rvSignal, iNumHalvingsIn=HALVES)
    iRetCode = tMultires % approximate(2, rvApprox2)
    iRetCode = tMultires % approximate(5, rvApprox5)
    iRetCode = tMultires % approximate(8, rvApprox8)
    iRetCode = tMultires % get_variances(rOriginalVariance, rvVariance, rResidualVariance)
    iRetCode = tMultires % get_total_variation(rOriginalVariation, rvTotalVariation, rResidualVariation)
    iRetCode = tMultires % get_partial_variation(rvPartialVariation)
    rOriginalPartialVariation = 0.
    rResidualPartialVariation = 0.
    open(newunit=iLUN, file=sOutData, status='unknown', action='write')
    write(iLUN, "('Time.Stamp, Original, Approx.2, Approx.5, Approx.8, Residual')")
    do i = 1, N
        write(iLUN, "(f6.0,5(',',e15.7))") &
            rvTimeStamp(i), &
            rvSignal(i), &
            rvApprox2(i), &
            rvApprox5(i), &
            rvApprox8(i), &
            tMultires % rvResidual(i)
    end do
    close(iLUN)
    open(newunit=iLUN, file=sOutStats, status='unknown', action='write')
    write(iLUN, "('Halving, Variance, Total.Variation, Partial.Variation')")
    write(iLUN, "('-1,', e15.7,2(',',e15.7))") rOriginalVariance, rOriginalVariation, rOriginalPartialVariation
    do i = 1, size(rvVariance)
        write(iLUN, "(i2, ',', e15.7,2(',',e15.7))") i-1, rvVariance(i), rvTotalVariation(i), rvPartialVariation(i)
    end do
    write(iLUN, "(i2, ',', e15.7,2(',',e15.7))") size(rvVariance), rResidualVariance, rResidualVariation, rResidualPartialVariation
    close(iLUN)
    print *, "Test 2"
    
    ! Test 3: sine wave
    sOutData  = "test_data/t3_data.csv"
    sOutStats = "test_data/t3_stat.csv"
    call clean
    call sinewave
    iRetCode = tMultires % create(rvTimeStamp, rvSignal, iNumHalvingsIn=HALVES)
    iRetCode = tMultires % approximate(2, rvApprox2)
    iRetCode = tMultires % approximate(5, rvApprox5)
    iRetCode = tMultires % approximate(8, rvApprox8)
    iRetCode = tMultires % get_variances(rOriginalVariance, rvVariance, rResidualVariance)
    iRetCode = tMultires % get_total_variation(rOriginalVariation, rvTotalVariation, rResidualVariation)
    iRetCode = tMultires % get_partial_variation(rvPartialVariation)
    rOriginalPartialVariation = 0.
    rResidualPartialVariation = 0.
    open(newunit=iLUN, file=sOutData, status='unknown', action='write')
    write(iLUN, "('Time.Stamp, Original, Approx.2, Approx.5, Approx.8, Residual')")
    do i = 1, N
        write(iLUN, "(f6.0,5(',',e15.7))") &
            rvTimeStamp(i), &
            rvSignal(i), &
            rvApprox2(i), &
            rvApprox5(i), &
            rvApprox8(i), &
            tMultires % rvResidual(i)
    end do
    close(iLUN)
    open(newunit=iLUN, file=sOutStats, status='unknown', action='write')
    write(iLUN, "('Halving, Variance, Total.Variation, Partial.Variation')")
    write(iLUN, "('-1,', e15.7,2(',',e15.7))") rOriginalVariance, rOriginalVariation, rOriginalPartialVariation
    do i = 1, size(rvVariance)
        write(iLUN, "(i2, ',', e15.7,2(',',e15.7))") i-1, rvVariance(i), rvTotalVariation(i), rvPartialVariation(i)
    end do
    write(iLUN, "(i2, ',', e15.7,2(',',e15.7))") size(rvVariance), rResidualVariance, rResidualVariation, rResidualPartialVariation
    close(iLUN)
    print *, "Test 3"

    ! Test 4: sine wave + order 1 stationary
    sOutData  = "test_data/t4_data.csv"
    sOutStats = "test_data/t4_stat.csv"
    call clean
    call sinewave
    call noise
    iRetCode = tMultires % create(rvTimeStamp, rvSignal, iNumHalvingsIn=HALVES)
    iRetCode = tMultires % approximate(2, rvApprox2)
    iRetCode = tMultires % approximate(5, rvApprox5)
    iRetCode = tMultires % approximate(8, rvApprox8)
    iRetCode = tMultires % get_variances(rOriginalVariance, rvVariance, rResidualVariance)
    iRetCode = tMultires % get_total_variation(rOriginalVariation, rvTotalVariation, rResidualVariation)
    iRetCode = tMultires % get_partial_variation(rvPartialVariation)
    rOriginalPartialVariation = 0.
    rResidualPartialVariation = 0.
    open(newunit=iLUN, file=sOutData, status='unknown', action='write')
    write(iLUN, "('Time.Stamp, Original, Approx.2, Approx.5, Approx.8, Residual')")
    do i = 1, N
        write(iLUN, "(f6.0,5(',',e15.7))") &
            rvTimeStamp(i), &
            rvSignal(i), &
            rvApprox2(i), &
            rvApprox5(i), &
            rvApprox8(i), &
            tMultires % rvResidual(i)
    end do
    close(iLUN)
    open(newunit=iLUN, file=sOutStats, status='unknown', action='write')
    write(iLUN, "('Halving, Variance, Total.Variation, Partial.Variation')")
    write(iLUN, "('-1,', e15.7,2(',',e15.7))") rOriginalVariance, rOriginalVariation, rOriginalPartialVariation
    do i = 1, size(rvVariance)
        write(iLUN, "(i2, ',', e15.7,2(',',e15.7))") i-1, rvVariance(i), rvTotalVariation(i), rvPartialVariation(i)
    end do
    write(iLUN, "(i2, ',', e15.7,2(',',e15.7))") size(rvVariance), rResidualVariance, rResidualVariation, rResidualPartialVariation
    close(iLUN)
    print *, "Test 4"
    
    ! Test 5: amplitude-modulated order 1 stationary
    sOutData  = "test_data/t5_data.csv"
    sOutStats = "test_data/t5_stat.csv"
    call clean
    call constant
    call noise
    call modulate
    iRetCode = tMultires % create(rvTimeStamp, rvSignal, iNumHalvingsIn=HALVES)
    iRetCode = tMultires % approximate(2, rvApprox2)
    iRetCode = tMultires % approximate(5, rvApprox5)
    iRetCode = tMultires % approximate(8, rvApprox8)
    iRetCode = tMultires % get_variances(rOriginalVariance, rvVariance, rResidualVariance)
    iRetCode = tMultires % get_total_variation(rOriginalVariation, rvTotalVariation, rResidualVariation)
    iRetCode = tMultires % get_partial_variation(rvPartialVariation)
    rOriginalPartialVariation = 0.
    rResidualPartialVariation = 0.
    open(newunit=iLUN, file=sOutData, status='unknown', action='write')
    write(iLUN, "('Time.Stamp, Original, Approx.2, Approx.5, Approx.8, Residual')")
    do i = 1, N
        write(iLUN, "(f6.0,5(',',e15.7))") &
            rvTimeStamp(i), &
            rvSignal(i), &
            rvApprox2(i), &
            rvApprox5(i), &
            rvApprox8(i), &
            tMultires % rvResidual(i)
    end do
    close(iLUN)
    open(newunit=iLUN, file=sOutStats, status='unknown', action='write')
    write(iLUN, "('Halving, Variance, Total.Variation, Partial.Variation')")
    write(iLUN, "('-1,', e15.7,2(',',e15.7))") rOriginalVariance, rOriginalVariation, rOriginalPartialVariation
    do i = 1, size(rvVariance)
        write(iLUN, "(i2, ',', e15.7,2(',',e15.7))") i-1, rvVariance(i), rvTotalVariation(i), rvPartialVariation(i)
    end do
    write(iLUN, "(i2, ',', e15.7,2(',',e15.7))") size(rvVariance), rResidualVariance, rResidualVariation, rResidualPartialVariation
    close(iLUN)
    print *, "Test 5"
    
    ! Test 6: peak
    sOutData  = "test_data/t6_data.csv"
    sOutStats = "test_data/t6_stat.csv"
    call clean
    call constant
    call peak
    iRetCode = tMultires % create(rvTimeStamp, rvSignal, iNumHalvingsIn=HALVES)
    iRetCode = tMultires % approximate(2, rvApprox2)
    iRetCode = tMultires % approximate(5, rvApprox5)
    iRetCode = tMultires % approximate(8, rvApprox8)
    iRetCode = tMultires % get_variances(rOriginalVariance, rvVariance, rResidualVariance)
    iRetCode = tMultires % get_total_variation(rOriginalVariation, rvTotalVariation, rResidualVariation)
    iRetCode = tMultires % get_partial_variation(rvPartialVariation)
    rOriginalPartialVariation = 0.
    rResidualPartialVariation = 0.
    open(newunit=iLUN, file=sOutData, status='unknown', action='write')
    write(iLUN, "('Time.Stamp, Original, Approx.2, Approx.5, Approx.8, Residual')")
    do i = 1, N
        write(iLUN, "(f6.0,5(',',e15.7))") &
            rvTimeStamp(i), &
            rvSignal(i), &
            rvApprox2(i), &
            rvApprox5(i), &
            rvApprox8(i), &
            tMultires % rvResidual(i)
    end do
    close(iLUN)
    open(newunit=iLUN, file=sOutStats, status='unknown', action='write')
    write(iLUN, "('Halving, Variance, Total.Variation, Partial.Variation')")
    write(iLUN, "('-1,', e15.7,2(',',e15.7))") rOriginalVariance, rOriginalVariation, rOriginalPartialVariation
    do i = 1, size(rvVariance)
        write(iLUN, "(i2, ',', e15.7,2(',',e15.7))") i-1, rvVariance(i), rvTotalVariation(i), rvPartialVariation(i)
    end do
    write(iLUN, "(i2, ',', e15.7,2(',',e15.7))") size(rvVariance), rResidualVariance, rResidualVariation, rResidualPartialVariation
    close(iLUN)
    print *, "Test 6"

contains

    subroutine clean
        rvSignal = 0.
    end subroutine clean

    subroutine constant
        rvSignal = rvSignal + 1.
    end subroutine constant
    
    subroutine noise
        real, dimension(:), allocatable :: rvNoise
        allocate(rvNoise(N))
        call random_number(rvNoise)
        rvSignal=rvSignal + 2.*(rvNoise - 0.5)
        deallocate(rvNoise)
    end subroutine noise
    
    subroutine sinewave
        integer :: i
        do i = 1, N
            rvSignal(i) = rvSignal(i) + sin(2.*3.1415926535*(i-1)/32768.)
        end do
    end subroutine sinewave
    
    subroutine modulate
        integer :: i
        do i = 1, N
            rvSignal(i) = rvSignal(i) * abs(sin(2.*3.1415926535*(i-1)/32768.))
        end do
    end subroutine modulate

    subroutine peak
        integer :: i
        real    :: u
        do i = 1, N
            u = (i-1-N/2)/32768.
            rvSignal(i) = rvSignal(i) + exp(-u**2/(2.*0.05**2))
        end do
    end subroutine peak
    
end program multires_test2
