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

# Useful declarations

struct DataSet_In
    time_stamp::Vector{Dates.DateTime}
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

# Read data from input ile, and compose input data set from it
#
# WARNING: We'll soon replace it to support reading data from the
# =======  standard ARPA formats: my current version is just a
#          foot-behind-door.
#
function data_read(
    file_name::String
)

    # Assume success (will falsify on failure)
    iRetCode = 0
    sErrMsg  = ""

    # Get file to data frame (very curious to see it really does, with so large a file...)
    d = CSV.read(file_name, DataFrames.DataFrame)

    return (iRetCode, sErrMsg, d)

end


################
# Main program #
################

# Get parameters
if length(ARGS) != 2
    println("zi.jl - Procedure for calculating basic diagnostic indices on SonicLib data files")
    println()
    println("Usage:")
    println()
    println("  julia zi.jl <Data_File> <Results_File>")
    println()
    println("Copyright 2021 by ARPA Lombardia")
    println("This is open-source software, covered by the MIT license.")
    println("Written by: Patrizia 'Patti' Favaron")
    println()
    exit(1)
end
input_file  = ARGS[1]   # Data, FOR THE MOMENT in MeteoFlux Core V2 "aggregate base processed" form - with wrong time stamp!
output_file = ARGS[2]   # Output, in CSV form

# Get data
(iRetCode, sErrMsg, d) = data_read(input_file)

print(length(d[!, "Date.Time"]))    # Just to ckeck it read something
