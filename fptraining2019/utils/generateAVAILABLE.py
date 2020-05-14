""" Generate AVAILABLE file for meteo files in METEO_DIR.
	AVAILABLE is written to METEO_DIR, so script can be called from anywhere.
	Only meteo files valid for METEO_PREFIX are included.
"""

import glob
import os
import sys

#METEO_DIR = "/eunadics_data/mmulder/GV_ecm_0p2/"
#METEO_DIR = "/eunadics_data/mmulder/eyja/ecm_data/"
#METEO_DIR = "/eunadics_data/mmulder/gfs_gv/"
METEO_DIR = "/home/cwet/kurs17/user_mdm/exercises/volc/gfs_data/"

# prefix in regex format
#METEO_PREFIX = "EN*"
#METEO_PREFIX = "fnl_*"
METEO_PREFIX = "gfsanl_4_*"

#For DUST:
#METEO_PREFIX = METEO_DIR+"EN*"

# Test if dir exists
if os.path.isdir(METEO_DIR):
  os.chdir(METEO_DIR)
else:
  print("\nDirectory does not exist.")
  sys.exit()

# Load all files and sort as in AVAILABLE
meteo_files = glob.glob(METEO_PREFIX)
meteo_files.sort()

# Check if any meteo file exists
if not meteo_files:
  print("\nDirectory does not contain meteo files.")
  sys.exit()

#AVAILABLE = open('AVAILABLE', 'w')
AVAILABLE = open(METEO_DIR+'/AVAILABLE', 'w')
AVAILABLE.write("-\n-\n-\n")

for meteo_file in meteo_files:
#    print meteo_file
#    print meteo_file[20]
    # Sample line format "20130802 000000      EN13080200"
#    AVAILABLE.write("20%s %s%s00      %s\n" % (meteo_file[8:14], meteo_file[15:17], meteo_file[19:21], meteo_file))
#    AVAILABLE.write("20%s %s0000      %s\n" % (meteo_file[2:8], meteo_file[8:10], meteo_file)) # for ecm 0p2
#    AVAILABLE.write("20%s %s0000      %s\n" % (meteo_file[11:17], meteo_file[18:20], meteo_file)) # for gfs fnl
#    AVAILABLE.write("20%s %s0000      %s\n" % (meteo_file[11:17], str(int(meteo_file[18:20])+int(meteo_file[25])), meteo_file)) # for gfsanl
#    print meteo_file[11:17]
#    print meteo_file[18:20]
#    print "0"+str(int(meteo_file[18:20])+int(meteo_file[25])) if(int(meteo_file[18:20])+int(meteo_file[25]))<12 else str(int(meteo_file[18:20])+int(meteo_file[25]))
    AVAILABLE.write("20%s %s0000      %s\n" % (meteo_file[11:17], "0"+str(int(meteo_file[18:20])+int(meteo_file[25])) if(int(meteo_file[18:20])+int(meteo_file[25]))<12 else str(int(meteo_file[18:20])+int(meteo_file[25])), meteo_file)) # for gfsanl


AVAILABLE.close()

print("AVAILABLE file created in %s.\nPress Enter to exit." % METEO_DIR)
raw_input()
