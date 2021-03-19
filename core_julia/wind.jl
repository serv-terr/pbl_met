
module Pbl_wind

    using DataFrames
    using CSV
    using Statistics

    export read_raw
    export average_raw

    # Read one SonicLib file into memory
    function read_raw(file_name::String)
        # Assume success (will falsify on failure)
        iRetCode = 0
        sMsg = "Success"

        # Gather actual data
        d = CSV.read(file_name, DataFrame)
        summary(d)

        # Check this is really a raw SonicLib data set
        nm = names(d)
        is_raw = (nm[1] == "time.stamp" && nm[2] == "u   " && nm[3] == "v   " && nm[4] == "w   " && nm[5] == "t")
        if !(is_raw)
            iRetCode = 1
            sMsg = "Input dataframe does not correspond to a SonicLib file"
            return (iRetCode, sMsg, nothing)
        end

        # Force names to a syntax which is more Julia-kind
        rename!(d, [:time_stamp, :u, :v, :w, :t])

        return (iRetCode, sMsg, d)

    end


    # Calculate basic statistics in preparation of eddy covariance
    function average_raw(d::DataFrame, avg_time::Int64)

        # Assume success (will falsify on failure)
        iRetCode = 0
        sMsg = "Success"

        # Preallocate new dataframe to hold results
        seconds_from, seconds_to = partition_hour(avg_time)
        iNumRows = length(seconds_from)
        e = DataFrames.DataFrame(
            [Float64,Float64,Float64,Float64,Float64,Float64,Float64,Float64,Float64,Float64,Float64,Float64,Float64,Float64,Float64],
            [:date, :u, :v, :w, :t, :uu, :vv, :ww, :uv, :uw, :vw, :tt, :ut, :vt, :wt],
            iNumRows
        )

        # Calculate averages
        for iRow in 1:iNumRows
            current_block = findall((seconds_from[iRow] .<= d.time_stamp) .* (d.time_stamp .< seconds_to[iRow]))
            e.date[iRow] = minimum(d.time_stamp[current_block])
            e.u[iRow]  = mean(d.u[current_block])
            e.v[iRow]  = mean(d.v[current_block])
            e.w[iRow]  = mean(d.w[current_block])
            e.t[iRow]  = mean(d.t[current_block])
            e.uu[iRow] = cov(d.u[current_block], d.u[current_block])
            e.vv[iRow] = cov(d.v[current_block], d.v[current_block])
            e.ww[iRow] = cov(d.w[current_block], d.w[current_block])
            e.tt[iRow] = cov(d.t[current_block], d.t[current_block])
            e.uv[iRow] = cov(d.u[current_block], d.v[current_block])
            e.uw[iRow] = cov(d.u[current_block], d.w[current_block])
            e.vw[iRow] = cov(d.v[current_block], d.w[current_block])
            e.ut[iRow] = cov(d.u[current_block], d.t[current_block])
            e.vt[iRow] = cov(d.v[current_block], d.t[current_block])
            e.wt[iRow] = cov(d.w[current_block], d.t[current_block])
        end

        return e

    end

    #####################
    # Internal routines #
    #####################

    # Partition a hour in blocks of specified length
    function partition_hour(seconds::Int64)
        num_blocks = 3600 ÷ seconds
        seconds_from = Array{Int16,1}(undef, num_blocks)
        seconds_to   = Array{Int16,1}(undef, num_blocks)
        for block in 1:num_blocks
            seconds_from[block] = seconds * (block - 1)
            seconds_to[block]   = seconds_from[block] + seconds
        end
        return(seconds_from, seconds_to)
    end

end
