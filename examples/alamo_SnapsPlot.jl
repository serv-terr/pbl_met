#!/usr/bin/env julia

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

