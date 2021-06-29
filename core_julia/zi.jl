#!/Applications/Julia-1.6.app/Contents/Resources/julia/bin/julia
# Sorry, this is Patti's crazy shebang; you may replace with yours, if you like

# "zi.jl" - Procedure for estimating mixing height (both 10 minutes, and daily maximum)
#
# Copyright 2021 By ARPA Lombardia
# This is open-source software, covered by the MIT license.
#
# Written by: Patrizia Favaron - Feel free to address any quotes, thanks, and blames, to her... :)

import Dates
import CSV
import DataFrames
import Printf
import Glob

# Useful declarations

struct DataSet_In
    lat::Float64
    lon::Float64
    zone::Int64
    alt::Float64
    time_stamp_begin::Vector{Dates.DateTime}
    time_stamp_end::Vector{Dates.DateTime}
    vel::Vector{Float64}
    u_star::Vector{Float64}
    H0::Vector{Float64}
    L::Vector{Float64}
end

struct Daily_Values
    time_stamp::Vector{Dates.DateTime}
    zi_stable::Vector{Float64}
    zi_mechanical::Vector{Float64}
    zi_convective::Vector{Float64}
end

struct DataSet_Out
    time_stamp::Vector{Dates.Date}
    vel_max::Vector{Float64}
    zi_max::Vector{Float64}
    zi::Vector{Daily_Values}
end

# Simple linear regression in idiomatic Julia (not mine... :) Surely, it "may be useful" at the price of one code line)
linreg(x, y) = hcat(fill!(similar(x), 1), x) \ y


# Read one single ARPA data file
function read_arpa(
    data_path::String,
    data_id::Int64,
    avg_period::String
)

    # Assume success (will falsify on failure)
    iRetCode = 0
    sErrMsg  = ""

    # Form file name pattern
    file_pattern = joinpath(data_path, "$(data_id)_$(avg_period)_*.txt")

    # Search pattern
    file_name_vector = Glob.glob(file_pattern)
    if length(file_name_vector) != 1
        iRetCode = 1
        sErrMsg  = "read_arpa:: error: More files found, or none at all"
        return (iRetCode, sErrMsg, Nothing, Nothing)
    end
    file_name = file_name_vector[1]
    if !isfile(file_name)
        iRetCode = 2
        sErrMsg  = "read_arpa:: error: Data file not found"
        return (iRetCode, sErrMsg, Nothing, Nothing)
    end

    # Get file to dataframe; as ARPA files are headerless, also assign names
    data = CSV.read(file_name, DataFrames.DataFrame, header=false, delim="\t")
    DataFrames.rename!(data, :Column1 => :Sensor_ID)
    DataFrames.rename!(data, :Column2 => :Text_Time_Stamp)
    DataFrames.rename!(data, :Column3 => :Value)
    DataFrames.rename!(data, :Column4 => :Validity)

    # Convert time stamps from string to DateTime
    n = length(data.Value)
    time_stamp_end = [Dates.DateTime(data.Text_Time_Stamp[i], "y-m-d H:M:S.s")  for i in 1:n]
    out_data = DataFrames.DataFrame()
    if avg_period == "H"
        out_data.Time_Stamp_Begin = time_stamp_end .- Dates.Hour(1)
    else
        out_data.Time_Stamp_Begin = time_stamp_end .- Dates.Minute(10)
    end
    out_data.Time_Stamp_End = time_stamp_end

    # Get all other relevant information
    out_data.Value = data.Value
    out_data.Validity = data.Validity

    # Leave, yielding results
    return(iRetCode, sErrMsg, out_data.Value, out_data.Time_Stamp_Begin, out_data.Time_Stamp_End)

end


# Read data from input files, and compose input data set from it
#
function data_read(
    data_path::String,
    station_name::String,
    avg_period::String = "H"
)

    # Assume success (will falsify on failure)
    iRetCode = 0
    sErrMsg  = ""

    # Get station cards file, if it exists
    stations_list = "./shakeup_stations.csv"
    if !isfile(stations_list)
        iRetCode = 1
        sErrMsg  = "data_read:: error: Stations cards file, 'shakeup_stations.csv', not found in current directory"
        return (iRetCode, sErrMsg, Nothing)
    end
    stn = CSV.read(stations_list, DataFrames.DataFrame)

    # Lookup station in list, by name
    stn_idx = 0
    for i in 1:length(stn[!, "Station_Name"])
        if stn[i, "Station_Name"] == station_name
            stn_idx = i
            break
        end
    end
    if stn_idx <= 0
        iRetCode = 2
        sErrMsg  = "data_read:: error: Station name not found"
        return (iRetCode, sErrMsg, Nothing)
    end

    # Get station position
    lon  = stn.Lon[stn_idx]
    lat  = stn.Lat[stn_idx]
    zone = stn.Zone[stn_idx]
    alt  = stn.Elevation[stn_idx]

    # Get IDs of relevant data
    vel_id   = stn.Vel_Sonic[stn_idx]
    ustar_id = stn.Ustar[stn_idx]
    h0_id    = stn.H0[stn_idx]
    zlm1_id  = stn.z_over_L[stn_idx]

    # Get vectors
    (iErrCode, sErrMsg, vel, time_vel, time_end) = read_arpa(data_path, vel_id, avg_period)
    if iErrCode != 0
        iRetCode = 3
        return (iRetCode, sErrMsg, Nothing)
    end
    (iErrCode, sErrMsg, ustar, time_ustar, time_end) = read_arpa(data_path, ustar_id, avg_period)
    if iErrCode != 0
        iRetCode = 4
        return (iRetCode, sErrMsg, Nothing)
    end
    (iErrCode, sErrMsg, h0, time_h0, time_end) = read_arpa(data_path, h0_id, avg_period)
    if iErrCode != 0
        iRetCode = 5
        return (iRetCode, sErrMsg, Nothing)
    end
    (iErrCode, sErrMsg, zlm1, time_zlm1, time_end) = read_arpa(data_path, zlm1_id, avg_period)
    if iErrCode != 0
        iRetCode = 6
        return (iRetCode, sErrMsg, Nothing)
    end

    # Check the data vectors time stamps are identical
    n = length(time_vel)
    for i in 1:n
        if (time_vel[i] != time_ustar[i]) && 
            (time_vel[i] != time_h0[i]) && 
            (time_vel[i] != time_zlm1[i]) && 
            (time_ustar[i] != time_h0[i]) && 
            (time_ustar[i] != time_zlm1[i]) && 
            (time_h0[i] != time_zlm1[i])
            iRetCode = 7
            sErrMsg = "data_read:: error: Data time stamps are not compatible"
            return (iRetCode, sErrMsg, Nothing)
        end
    end

    # Compute Obukhov length from stability parameter; we're dealing with
    # SHAKEUP stations, so we know the in advance the "reference height" is 10m.
    L = Vector{Float64}(undef, n)
    for i in 1:n
        if abs(zlm1[i]) > 1.e-30 && zlm1[i] > -990.0
            L[i] = 10.0 ./ zlm1[i]
            if L[i] > 900.0
                L[i] = 900.0
            end
            if L[i] < -900.0
                L[i] = -900.0
            end
        elseif zlm1[i] < -990.0
            L[i] = -999.0
        elseif abs(zlm1[i]) <= 1.e-30
            if zlm1[i] >= 0.0
                L[i] =  900.0
            else
                L[i] = -900.0
            end
        end
    end

    # Compose output
    data_set = DataSet_In(
        lat,
        lon,
        zone,
        alt,
        time_vel,
        time_end,
        vel,
        ustar,
        h0,
        L
    )

    return (iRetCode, sErrMsg, data_set)


end


################
# Main program #
################

# Get parameters
if length(ARGS) != 4
    println("zi.jl - Procedure for calculating basic diagnostic indices on SonicLib data files")
    println()
    println("Usage:")
    println()
    println("  julia zi.jl <Data_Path> <Station_Name> <Averaging_Period> <Results_File>")
    println()
    println("where")
    println()
    println("  <Averaging_Period> = H (hourly), t (10 minutes)")
    println()
    println("Copyright 2021 by ARPA Lombardia")
    println("This is open-source software, covered by the MIT license.")
    println("Written by: Patrizia 'Patti' Favaron")
    println()
    exit(1)
end
input_path   = ARGS[1]  # Data path
station_name = ARGS[2]  # Name of desired station, as in "./shakeup_stations.csv"
avg_period   = ARGS[3]  # Averaging period symbol (H = hourly, t = Ten minutes)
output_file  = ARGS[4]  # Output, in CSV form

# Get data
(iRetCode, sErrMsg, d) = data_read(input_path, station_name, avg_period)
if iRetCode != 0
    println(sErrMsg)
    println("Terminating execution")
    exit(2)
end

println(d.lat)
println(d.lon)
println(d.zone)
println(d.alt)

test = DataFrames.DataFrame()
test.begin = d.time_stamp_begin
test.end   = d.time_stamp_end
test.vel   = d.vel
test.ustar = d.u_star
test.h0    = d.H0
test.l     = d.L
println(test)
