#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import ampdLib
import math
import random

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
	peaks = ampdLib.ampd(rvX)
	print("Test 1")
	print(len(peaks))
	print(peaks)
	print(" ")
	
	# Test 2: 0.9
	peaks = ampdLib.ampd(rvX, 0.9)
	print("Test 2")
	print(len(peaks))
	print(peaks)
	print(" ")
	
	# Test 3: 0.5
	peaks = ampdLib.ampd(rvX, 0.5)
	print("Test 3")
	print(len(peaks))
	print(peaks)
	print(" ")
	
	# Test 4: 0.2
	peaks = ampdLib.ampd(rvX, 0.2)
	print("Test 4")
	print(len(peaks))
	print(peaks)
	print(" ")
	
	# Test 5: 0.01
	peaks = ampdLib.ampd(rvX, 0.01)
	print("Test 5")
	print(len(peaks))
	print(peaks)
	print(" ")
	
	# Generate new set
	rvX = [math.sin((2*3.1415927*i)/50) for i in range(16384)]

	# Run test 6: default
	peaks = ampdLib.ampd(rvX)
	print("Test 6")
	print(len(peaks))
	print(peaks)
	print(" ")
	
	# Add some noise
	random.seed(1)
	for i in range(len(rvX)):
		rvX[i] += random.random()/5.0

	# Run test 7: default
	peaks = ampdLib.ampd(rvX)
	print("Test 7")
	print(len(peaks))
	print(peaks)
	print(" ")
	

	
