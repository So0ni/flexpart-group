# -*- coding: utf-8 -*-
"""
NAME
    download_gfs
KEYWORD ARGUMENTS
    Start and End date (Format: YYYYMMDD)
    Earliest date 1st January 2007 (20070101),
    there are no weather data before that date to download (status May 2019)
KEYWORD ARGUMENTS optional
    path in which the file should be placed
DESCRIPTION
    download_gfs downloads weather data (analysis 0.5 degree grid) from nomads.ncdc.noaa.gov
    linux os is required
NOTE
    The files will not be renamed and not be checked if the download succeded!

    Comments / improvements are welcome.

CONTACT
    florian.geyer@zamg.ac.at
    Version 1, May 2019

DISCLAIMER
    This software has been developed for flexpart workshop 2019 @ ZAMG

"""

def check_date(date):
    """
        checks if date is really a date
    """
    import datetime
    correctDate = None
    date = str(date)
    
    if (len(date)!=8):
        return False
    year = int(date[0:4])
    month = int(date[4:6])
    day = int(date[6:8])
    try:
        datetime.datetime(year,month,day)
        correctDate = True
    except ValueError:
        correctDate = False
    return correctDate

def convert_str2date(date):
    """
        converts date (as integer) to python datetime
    """
    import datetime
    date = str(date)
    year = int(date[0:4])
    month = int(date[4:6])
    day = int(date[6:8])
    return datetime.datetime(year,month,day)

def create_filenames(date):
    """
        creates filenames for givn date,
        from the runs at 0, 6, 12 and 18 UTC
        only t=0 and t=3 will be downloaded
    """
    
    filelist = list()
    for t in ["0000", "0600", "1200", "1800"]:
        for t2 in ["000", "003"]:
            filelist.append("gfsanl_4_"+d.strftime('%Y%m%d')+"_"+t+"_"+t2+".grb2")
    return filelist

def download_file(path, filename, destination):
    """
        downloads files from path and saves to destination
    """
    import os
    command = "wget -q -O "+destination+"/"+filename+" ftp://nomads.ncdc.noaa.gov/"+path+"/"+filename
    os.system(command)

def check_path(path):
    """
        checks if path exists
    """
    import os
    if not os.path.exists(path):
        print("Path does not exist")
        print("")
        sys.exit()

def start_ftp_date():
    """
        from this date 0.5 degree grid data exists on ftp
    """
    return 20070101
        
import sys
from ftplib import FTP
import datetime

print("")

# checking input arguments (1 and 2 are start and end date
try:
    start = sys.argv[1]
    end   = sys.argv[2]
except:
    print("Please give me a start and end date (format: YYYYMMDD)")
    print("")
    sys.exit()

# checking destination (optinal), if no destination givn,
# files will be saved in current directory
try:
    destination = sys.argv[3]
    if (destination[-1]!="/"):
      destination = destination+"/"
except:
    destination = "./"

# print input arguments
print("Start: ", start)
print("End:   ", end)
print("Path: ", destination)
print("")

check_path(destination)

start_ftp_date = start_ftp_date()

# check if dates are in correct format
if (check_date(start)==False):
    print("Wrong start date, please try again (Format should be YYYYMMDD)!")
    print("")
    sys.exit()
if (check_date(end)==False):
    print("Wrong end date, please try again (Format should be YYYYMMDD)!")
    print("")
    sys.exit()
start_date = convert_str2date(start)
end_date   = convert_str2date(end)
start_ftp_date   = convert_str2date(start_ftp_date)

# check if dates are in correct order
if (start_date>end_date):
    print("Your start date was after the end date, I will turn it around!")
    print("")
    start_date, end_date = end_date, start_date
    
if (start_date<start_ftp_date):
    print("The FTP-Server only provides data after the 1st January 2007, please choose another date!")
    print("")
    sys.exit()

print("start downloading from ", start_date, "to", end_date)

# create date list
date_list = [start_date + datetime.timedelta(days=x) for x in range(0, (end_date-start_date).days+1)]

# connect to ftp
ftp = FTP('nomads.ncdc.noaa.gov')
ftp.login()

# iter through date_list and download
for d in date_list:
    path = "GFS/analysis_only/"+d.strftime('%Y%m')+"/"+d.strftime('%Y%m%d')+"/"
    try:
        ftp.cwd(path)
        print(path, "found")
        file_list_ftp = ftp.nlst()
        file_list = create_filenames(d)
        for f in file_list:
            if f in file_list_ftp:
                print("  ", f, "found, download as "+destination+f)
                download_file(path, f, destination)
            else:
                print("  ", f, "not found")
        ftp.cwd("../../../../")
    except:
        print(d.strftime('%Y/%m/%d'), "Directory not found")
ftp.quit()
print("")
