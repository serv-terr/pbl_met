#!/usr/bin/env python3

import os
import sys
import glob
import numpy as np
import matplotlib as plt

if __name__ == "__main__":

	# Check input parameters
	if len(sys.argv) != 3:
		print("alamo_snapsPlot.py:: error: Invalid argument list")
		print(" ")
		print("Usage:")
		print(" ")
		print("  ./alamo_snapsPlot.py <snaps_path> <images_path>")
		print(" ")
		print("by: Mauri Favaron - 2018")
		sys.exit(1)
	snapsPath  = sys.argv[1]
	imagesPath = sys.argv[2]
	
	# Find snaps in snaps path
	snaps = glob.glob(os.path.join(snapsPath, "snap_*.csv"))
	if len(snaps) <= 0:
		print("alamo_snapsPlot.py:: error: Snaps path contains no snaps")
		sys.exit(2)
	try:
		f = open(os.path.join(snapsPath, "guide.txt"), "r")
		lines = f.readlines()
		f.close()
	except:
		print("alamo_snapsPlot.py:: error: No or invalid guide file")
		sys.exit(3)
	if len(lines) <= 0:
		print("alamo_snapsPlot.py:: error: No or invalid guide file")
		sys.exit(3)
