# "zi.jl" - Procedure for estimating mixing height (both 10 minutes, and daily maximum)
#
# Copyright 2021 By Patrizia Favaron
# This is open-source software, vovered by the MIT license.

import DateTime
import CSV
import DataFrames
import Printf

# Useful declarations

struct DataSet_In
    time_stamp::Vector{DateTime}
    vel::Vector{Float64}
    u_star::Vector{Float64}
    H0::Vector{Float64}
    L::Vector{Float64}
end

struct Daily_Values
    time_stamp::Vector{DateTime}
    zi_stable::Vector{Float64}
    zi_mechanical::Vector{Float64}
    zi_convective::Vector{Float64}
end

struct DataSet_Out
    time_stamp::Vector{Date}
    vel_max::Vector{Float64}
    zi_max::Vector{Float64}
    zi::Vector{DailyValues}
end

linreg(x, y) = hcat(fill!(similar(x), 1), x) \ y

