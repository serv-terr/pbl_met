#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import ampdLib

if __name__ == "__main__":
	
	# Get test file
	f = open("./test.csv", "r")
	lines = f.readlines()
	f.close()
	rvX = []
	for line in lines[1:]:
		blks = line[:-1].split(",")
		rvX.append(float(blks[1]))
	
	# Run test 1: default
	peaks, LCM = ampdLib.ampd(rvX)
	print(LCM.shape)
	print(LCM[0:8190,6:11])
	print(len(peaks))
	print(peaks)
	print(" ")
	
