import glob
import os
import sys
import gzip
import struct

if __name__ == "__main__":

    # Get parameters
    if len(sys.argv) != 3:
        print("shakeup.py  - Procedure for converting Shakeup MeteoFlux raw data to SonicLib\n")
        print("Usage:\n")
        print("  python shakeup.py <In_Path> <Out_Path\n")
        print("Copyright 2021 by Patrizia Favaron - This is open-source software,")
        print("covered by the MIT license.\n")
        sys.exit()
    in_path  = sys.argv[1]
    out_path = sys.argv[2]

    # Search all data directories and files
    dirs = sorted(glob.glob(os.path.join(in_path, "raw", "??????")))
    for dir in dirs:

        # Create subdir in output path, if necessary
        subdir = os.path.basename(dir)
        try:
            os.makedirs(os.path.join(out_path,subdir))
        except:
            pass # Directory is already present

        # Get files in directory
        files = sorted(glob.glob(os.path.join(dir, "*")))

        # Process files
        for file in files:

            (root, ext) = os.path.splitext(file)
            can_process = True
            if ext == ".gz":
                print("GZIPPED " + file)
                try:
                    f = gzip.open(file, "rb")
                    contents = f.read()
                    f.close()
                except:
                    can_process = False
            else:
                print("NORMAL  " + file)
                try:
                    f = open(file, "rb")
                    contents = f.read()
                    f.close()
                except:
                    can_process = False

            if can_process:

                # Convert bytes to actual data
                time_stamp = []
                u          = []
                v          = []
                w          = []
                t          = []
                num_data   = len(contents) // (5 * 2)   # Each record contains 5 INTEGER*2 values
                for data_idx in range(num_data):
                    i_pos = data_idx * 5 * 2

                    t_stamp = struct.unpack("@h", contents[i_pos  :i_pos+ 2])[0]
                    u_val =   struct.unpack("@h", contents[i_pos+4:i_pos+ 6])[0] # Note U and V are exchanged in MFC V2 raw data
                    v_val =   struct.unpack("@h", contents[i_pos+2:i_pos+ 4])[0] # Note U and V are exchanged in MFC V2 raw data
                    w_val =   struct.unpack("@h", contents[i_pos+6:i_pos+ 8])[0]
                    t_val =   struct.unpack("@h", contents[i_pos+8:i_pos+10])[0]

                    if t_stamp < 5000:

                        time_stamp.append(t_stamp)
                        u.append(         u_val/100.0)
                        v.append(         v_val/100.0)
                        w.append(         w_val/100.0)
                        t.append(         t_val/100.0)

                # Generate output file name
                this_file_name = os.path.basename(file).replace(".gz", "")
                out_file_name  = os.path.join(out_path, subdir, this_file_name) + ".csv"

                # Write data to SonicLib format
                g = open(out_file_name, "w")
                g.write("t.stamp,u,v,w,t\n")
                for data_idx in range(len(time_stamp)):
                    g.write("%d,%f,%f,%f,%f\n" % (time_stamp[data_idx], u[data_idx], v[data_idx], w[data_idx], t[data_idx]))
                g.close()

            else:

                print("FAILED  " + file)

