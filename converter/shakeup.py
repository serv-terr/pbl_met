import glob
import os
import sys
import gzip
import struct
import numpy as np

if __name__ == "__main__":

    # Get parameters
    if len(sys.argv) != 4:
        print("shakeup.py  - Procedure for converting Shakeup MeteoFlux raw data to SonicLib\n")
        print("Usage:\n")
        print("  python shakeup.py <In_Path> <Out_Path> <Report_File>\n")
        print("Copyright 2021 by Patrizia Favaron - This is open-source software,")
        print("covered by the MIT license.\n")
        sys.exit()
    in_path     = sys.argv[1]
    out_path    = sys.argv[2]
    report_file = sys.argv[3]

    # Prepare to write reporting data to file
    h = open(report_file, "w")
    h.write("File,N.Data,U.Min,U.Avg,U.Max,V.Min,V.Avg,V.Max,W.Min,W.Avg,W.Max,T.Min,T.Avg,T.Max\n")

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
                try:
                    f = gzip.open(file, "rb")
                    contents = f.read()
                    f.close()
                    print("GZIPPED " + file + "  Size: " + str(len(contents)))
                except:
                    can_process = False
            else:
                try:
                    f = open(file, "rb")
                    contents = f.read()
                    f.close()
                    print("NORMAL " + file + "  Size: " + str(len(contents)))
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

                    # Convert only valid ultrasonic quadruples (leave analog data and invalids out)
                    if t_stamp < 5000 and u_val > -9000 and v_val > -9000 and w_val > -9000 and t_val > -9000:

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

                # Add data to report
                num_valid_sonic_data = len(time_stamp)
                u_fast = np.array(u)
                v_fast = np.array(v)
                w_fast = np.array(w)
                t_fast = np.array(t)
                u_min  = u_fast.min()
                u_avg  = u_fast.mean()
                u_max  = u_fast.max()
                v_min  = v_fast.min()
                v_avg  = v_fast.mean()
                v_max  = v_fast.max()
                w_min  = w_fast.min()
                w_avg  = w_fast.mean()
                w_max  = w_fast.max()
                t_min  = t_fast.min()
                t_avg  = t_fast.mean()
                t_max  = t_fast.max()
                if num_valid_sonic_data > 0:
                    h.write(
                        "%s,%d,%6.3f,%6.3f,%6.3f,%6.3f,%6.3f,%6.3f,%6.3f,%6.3f,%6.3f,%6.3f,%6.3f,%6.3f\n" % 
                        (
                            os.path.basename(out_file_name),
                            num_valid_sonic_data,
                            u_min, u_avg, u_max,
                            v_min, v_avg, v_max,
                            w_min, w_avg, w_max,
                            t_min, t_avg, t_max
                        )
                    )
                else:
                    h.write(
                        "%s,%d,-9999.9,-9999.9,-9999.9,-9999.9,-9999.9,-9999.9,-9999.9,-9999.9,-9999.9\n" % (
                            os.path.basename(out_file_name),
                            num_valid_sonic_data
                        )
                    )

            else:

                print("FAILED  " + file)

    h.close()
