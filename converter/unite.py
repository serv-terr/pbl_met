#!/usr/bin/env python3

import glob
import os
import sys

if __name__ == "__main__":

    # Get parameters
    if len(sys.argv) != 3:
        print("unite.py  - Procedure for uniting Shakeup MeteoFlux processed data to a unique file\n")
        print("Usage:\n")
        print("  python unite.py <In_Path> <Out_File>\n")
        print("Copyright 2021 by Patrizia Favaron - This is open-source software,")
        print("covered by the MIT license.\n")
        sys.exit()
    in_path  = sys.argv[1]
    out_file = sys.argv[2]

    # Prepare to write reporting data to file
    h = open(out_file, "w")
    first_file_still_to_read = True

    # Search all data directories and files
    dirs = sorted(glob.glob(os.path.join(in_path, "??????")))
    for dir in dirs:

        # Get files in directory
        files = sorted(glob.glob(os.path.join(dir, "*")))

        # Process files
        for file in files:

            # Gather header and data lines to memory
            f = open(file, "r")
            lines = f.readlines()
            f.close()

            # If this is the first file gathered, save its header
            # as first line in new file
            if first_file_still_to_read:
                h.write(lines[0])
                first_file_still_to_read = False

            # Save all other data
            for idx in range(1,len(lines)):
                h.write(lines[idx])

    h.close()
