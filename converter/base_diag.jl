import Glob
import CSV
import DataFrames
import Printf

# Useful declarations

struct Block
    time_stamp::Vector{Int}
    x::Vector{Float64}
end

struct DataSet
    data::Vector{Block}
end

linreg(x, y) = hcat(fill!(similar(x), 1), x) \ y

# Internal functions

function check_time_ordering(
    time_stamp::Vector{Int},        # Vector containing the time stamps
    sampling_rate::Int = 10,        # Sampling rate, in Hertz
    max_data_per_second::Int = 12   # Should be larger than the sampling rate
)

    # Check range
    min_t = minimum(time_stamp)
    max_t = maximum(time_stamp)
    is_proper_range = (min_t == 0) && (max_t == 3599)

    # Count data having the same time stamps (will be compared with 0
    # and 'max_data_per_second')
    num_data_per_second = Vector{Int}(undef, 3600)
    for i in 1:3600
        num_data_per_second[i] = 0
    end
    for i in 1:length(time_stamp)
        this_time_index = time_stamp[i] + 1
        num_data_per_second[this_time_index] += 1
    end

    # Check no second has no data
    is_without_gaps = !(any(x->x<=0, num_data_per_second))

    # Check no second contains more data than 'max_data_per_second'
    is_not_overfilled = !(any(x->x>max_data_per_second, num_data_per_second))

    # Synthesize the integer non-quality score
    non_quality = 0     # Under ideal conditions...
    if !is_proper_range
        non_quality += 1
    end
    if !is_without_gaps
        non_quality += 2
    end
    if !is_not_overfilled
        non_quality += 4
    end

    return non_quality
    
end


function check_stationarity(
    time_stamp::Vector{Int},        # Vector containing the time stamps
    x::Vector{Float64},             # Vector containing the signal values
    sampling_rate::Int = 10,        # Sampling rate, in Hertz
    averaging_time::Int = 600,      # Averaging time, in Seconds
    sub_avg_time::Int = 60          # Sub-averaging time, used for checking stationarity
)

    # Compute reference sizes
    num_blocks = 3600 ÷ averaging_time
    num_sub_blocks = averaging_time ÷ sub_avg_time

    # Distribute time stamps and data among blocks, each having the 'averaging_time'
    # size (with respect to time stamp)
    d = DataSet(Vector{Block}())
    for block_idx in 1:num_blocks
        push!(d.data, Block(Vector{Int}(), Vector{Float64}()))
    end
    for i in 1:length(x)
        block_idx = time_stamp[i] ÷ averaging_time + 1
        push!(d.data[block_idx].time_stamp, time_stamp[i])
        push!(d.data[block_idx].x,          x[i])
    end

    # Compute trending slope for each block
    slope_x = zeros(Union{Float64,Missing}, num_blocks)
    for block_idx in 1:num_blocks
        regression = linreg(d.data[block_idx].time_stamp, d.data[block_idx].x)
        slope_x[block_idx] = abs(regression[2])
    end

    # Compute block statistics (mean and variance): they will be used as references
    # for testing against
    mean_x = zeros(Union{Float64,Missing}, num_blocks)
    std_x  = zeros(Union{Float64,Missing}, num_blocks)
    num_x  = fill(0, num_blocks)
    for block_idx in 1:num_blocks
        for i in 1:length(d.data[block_idx].x)
            num_x[block_idx]  += 1
            mean_x[block_idx] += d.data[block_idx].x[i]
            std_x[block_idx]  += d.data[block_idx].x[i]*d.data[block_idx].x[i]
        end
    end
    for block_idx in 1:num_blocks
        if num_x[block_idx] > 0
            mean_x[block_idx] /= num_x[block_idx]
            var = std_x[block_idx] / num_x[block_idx] - mean_x[block_idx]^2
            if var > 0.0
                std_x[block_idx] = sqrt(var)
            else
                std_x[block_idx] = 0.0
            end
        else
            mean_x[block_idx] = missing
            std_x[block_idx]  = missing
        end
    end

    # Compute sub-block statistics
    shift_mean_x = zeros(Union{Float64,Missing}, num_blocks)
    shift_std_x  = zeros(Union{Float64,Missing}, num_blocks)
    sub_mean_x = zeros(Union{Float64,Missing}, num_sub_blocks)
    sub_std_x  = zeros(Union{Float64,Missing}, num_sub_blocks)
    sub_num_x  = fill(0, num_sub_blocks)
    for block_idx in 1:num_blocks
        n = length(d.data[block_idx].time_stamp)
        if n > 0
            base_time = minimum(d.data[block_idx].time_stamp)
            for i in 1:n
                sub_block_idx = (d.data[block_idx].time_stamp[i] - base_time) ÷ sub_avg_time + 1
                sub_num_x[sub_block_idx]  += 1
                sub_mean_x[sub_block_idx] += d.data[block_idx].x[i]
                sub_std_x[sub_block_idx]  += d.data[block_idx].x[i] * d.data[block_idx].x[i]
            end
            for sub_block_idx in 1:num_sub_blocks
                if sub_num_x[sub_block_idx] > 0
                    sub_mean_x[sub_block_idx] /= sub_num_x[sub_block_idx]
                    var = sub_std_x[sub_block_idx] / sub_num_x[sub_block_idx] - sub_mean_x[sub_block_idx]^2
                    if !ismissing(var) && var > 0.0
                        sub_std_x[sub_block_idx] = sqrt(var)
                    else
                        sub_std_x[sub_block_idx] = 0.0
                    end
                else
                    sub_mean_x[sub_block_idx] = missing
                    sub_std_x[sub_block_idx]  = missing
                end
            end
            shift_mean_x[block_idx] = maximum(abs.(sub_mean_x .- mean_x[block_idx]))
            shift_std_x[block_idx]  = maximum(abs.(sub_std_x  .- std_x[block_idx]))
        else
            shift_mean_x[block_idx] = missing
            shift_std_x[block_idx]  = missing
        end
    end

    # Yield results
    return (maximum(shift_mean_x), maximum(shift_std_x), maximum(slope_x))

end


if length(ARGS) != 2
    println("base_diag.jl - Procedure for calculating basic diagnostic indices on SonicLib data files")
    println()
    println("Usage:")
    println()
    println("  julia base_diag.jl <SonicLib_Data_Path> <Report_File>")
    println()
    println("Copyright 2021 by Patrizia Favaron")
    println("This is open-source software, covered by the MIT license.")
    println()
    exit(1)
end

data_path = ARGS[1]
out_file  = ARGS[2]

# Try accessing output file...
open(out_file, "w") do g

    # Output file header
    write(g, "File, Time.Non.Quality, Max_Avg_Shift, Max_Std_Shift, Max_Slope\n")

    # Iterate the directory/subdirectories structure
    dirs = Glob.glob(data_path * "/??????")
    for dir in dirs
        files = Glob.glob(dir * "/*.csv")
        for file in files

            # Gather data, as they are. By the way the SonicLib files have been formed, they all
            # contain only valid data. So, there is no reason to check for invalids, greatly
            # simplifying code
            d = CSV.read(file, DataFrames.DataFrame)

            # Check time ordering
            non_time_quality = check_time_ordering(d[!, "t.stamp"])

            # Compute stationarity indicators for wind components
            (max_shift_avg_u, max_shift_std_u, max_slope_u) = check_stationarity(d[!, "t.stamp"], d[!, "u"])
            (max_shift_avg_v, max_shift_std_v, max_slope_v) = check_stationarity(d[!, "t.stamp"], d[!, "v"])
            (max_shift_avg_w, max_shift_std_w, max_slope_w) = check_stationarity(d[!, "t.stamp"], d[!, "w"])
            (max_shift_avg_t, max_shift_std_t, max_slope_t) = check_stationarity(d[!, "t.stamp"], d[!, "t"])
            max_avg_shift = maximum( [max_shift_avg_u, max_shift_avg_v, max_shift_avg_w, max_shift_avg_t] )
            max_std_shift = maximum( [max_shift_std_u, max_shift_std_v, max_shift_std_w, max_shift_std_t] )
            max_slope     = maximum( [max_slope_u, max_slope_v, max_slope_w, max_slope_t] )

            # Write results
            data_line = "$file, $non_time_quality, $max_avg_shift, $max_std_shift, $max_slope\n"
            write(g, data_line)
            println(file)

        end
    end

end
