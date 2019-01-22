#!/usr/bin/env julia

#import Pkg
#Pkg.add("CSV")
using CSV

# Get parameters
if length(ARGS) != 2

	println("alamo_SnapPlot.jl - Program generating particles scatter plots for movie production")
	println()
	println("Usage:")
	println()
	println("    ./alamo_SnapPlot.jl <SnapDataPath> <SnapPlotPath>")
	println()
	println("This file is part of the pbl_met project.")
	exit(1)
	
end
sSnapPath = ARGS[1]
sOutPath  = ARGS[2]

# Get boundary information

# Iterate over input path contents, and process them
files = readdir(sSnapPath)
for file in files
	sFileName = sSnapPath * "/" * file
	data = CSV.read(sFileName)
	println(sFileName)
end
