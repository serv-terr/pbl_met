#!/usr/bin/env python3

import os
import sys
import time
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.animation as anima
import struct

FFMpegWriter = anima.writers['ffmpeg']
metadata = dict(title="Airflow Movie", artist="ALAMO", comment="Movie generated automatically")
writer = FFMpegWriter(fps=30, metadata=metadata)


if __name__ == "__main__":

	# Check input parameters
	if len(sys.argv) != 3:
		print("alamo_movie.py:: error: Invalid argument list")
		print(" ")
		print("Usage:")
		print(" ")
		print("  ./alamo_movie.py <alamo_snaps_file> <alamo_movie_file>")
		print(" ")
		print("by: Mauri Favaron - 2019")
		sys.exit(1)
	snapsFile = sys.argv[1]
	movieFile = sys.argv[2]
	
	# Find snaps in snaps path
	try:
		guideFile = snapsFile + ".guide.txt"
		f = open(guideFile, "r")
		lines = f.readlines()
		f.close()
	except:
		print("alamo_movie.py:: error: No or invalid guide file")
		sys.exit(3)
	if len(lines) <= 0:
		print("alamo_movie.py:: error: No or invalid guide file")
		sys.exit(3)
		
	# Get plotting extrema from the guide file
	data = np.loadtxt(guideFile)
	xmin = data[0]
	xmax = data[1]
	ymin = data[2]
	ymax = data[3]
	zmin = data[4]
	zmax = data[5]
	dx   = data[6]
	dy   = data[7]
	amax = data[8]
	
	# Get number of snapshots
	listFile = snapsFile + ".lst"
	g = open(listFile, "r")
	lines = g.readlines()
	g.close()
	numSnaps = len(lines)
	
	# Process snapshots file
	fig = plt.figure()
	f = open(snapsFile, "rb")
	header = f.read(6*8)
	(xmin, xmax, ymin, ymax, zmin, zmax) = struct.unpack("dddddd", header)
	with writer.saving(fig, movieFile, dpi=100):
		while True:
	
			# Get start of block info
			try:
				blkStart = f.read(12)
			except:
				break
			(timeStamp, numData) = struct.unpack("di", blkStart)
			timeString = time.strftime("%Y-%m-%d %H:%M:%S", time.gmtime(timeStamp))
		
			# Get data from snapshot, and retrieve particles coordinates from it
			snapBlock = f.read(5*numData*4)
			fmt       = "@%df" % (5*numData)
			snapData = np.array(struct.unpack(fmt, snapBlock))
			if snapData.size > 0:
				snapMatrix = snapData.reshape((-1,5), order='C')
				xp = snapMatrix[:,0]
				yp = snapMatrix[:,1]
				ap = snapMatrix[:,4] / amax
				xpmin    = np.min(xp)
				ypmin    = np.min(yp)
				xpmax    = np.max(xp)	
				ypmax    = np.max(yp)	
			else:
				xp       = np.array([])
				yp       = np.array([])
				ap       = np.array([])
				xpmin    = 0.0
				ypmin    = 0.0
				xpmax    = 0.0
				ypmax    = 0.0
		
			# Generate frame
			ax = fig.add_subplot(111, aspect='equal')
			levels = np.array([(0,0,0,a) for a in ap])
			ax.scatter(xp, yp, s=0.1, color=levels)
			plt.xlim(xmin,xmax)
			plt.ylim(ymin,ymax)
			plt.title(timeString[0:13])
			writer.grab_frame()
			fig.delaxes(ax)
		
			# Inform users
			print("Processed: %s - Parts: %d - Min: (%f,%f)  Max: (%f,%f)" % (timeString, snapData.size, xpmin, ypmin, xpmax, ypmax))
		
	# Leave
	plt.close()
	f.close()
