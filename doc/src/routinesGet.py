#!/usr/bin/python

import os
import sys
import glob

if __name__ == "__main__":

	files = sorted(glob.glob("*.f90"))
	for file in files:
	
		f = open(file, "r")
		lines = f.readlines()
		f.close()
		
		print("## Source: " + file)
		print
		for lIdx in range(len(lines)):
			lne = lines[lIdx].replace("\n","").lower()
			if "!" in lne:
				commentPos = lne.find("!")
				lne = lne[0:commentPos]
			if ("subroutine" in lne or "function" in lne) and "(" in lne:
				print lines[lIdx].strip()
		print
	