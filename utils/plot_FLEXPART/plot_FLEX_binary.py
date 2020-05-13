#!/usr/bin/env python

'''
    plot_FLEX_binary.py

    Using FLEXPART binary output files, creates 2D plots of the data.

    Original Author contact information:

    Delia Arnold
    delia.arnold-arias@zamg.ac.at

    Marie Danielle Mulder
    marie.mulder@zamg.ac.at

    Christian Maurer
    christian.maurer@zamg.ac.at

    Disclaimer:

    This software is presented freely to the public with
    no restrictions on its use.  However, it would be
    appreciated if any use of the software or methods in
    part or in full acknowledges the source. This software 
    has been developed for flexpart workshop 2019 @ ZAMG.
    No liabilty taken by developers.

'''
import ast
import sys
import os
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap
from matplotlib.colors import LinearSegmentedColormap
from matplotlib import ticker
import numpy as np

# Importing various functions to read in
# and work with FLEXPART binary output
#   --- based on pflexible (J. BUkhardt)
#   --- possible to use f2py for faster reading
import read_header as readH
import read_grid as readG

import warnings
warnings.filterwarnings("ignore")


def main(argv = sys.argv):
    # DEFAULT values
    
    num_SPECIES = [1]
    factor = 1.e-12
    LEVEL = 0
    RELEASE = 0
    projection = 'cyl'
    depo = False
    plotting_region = []
    style = 'mesh'
    plot_markers = False

    print '!!!!!!!!!!!!!!!!!DEFAULT values!!!!!!!!!!!!!!'
    print 'Number of species: ', num_SPECIES
    print 'Multiplication factor for output: ', factor
    print 'Selected output levels ', LEVEL
    print 'Selected release: ', RELEASE
    print 'All available date-times are considered'
    print 'Projection: ', projection
    print 'Deposition: ', depo
    print 'Plotting region according to OUTGRID'
    print 'Plotting style: ', style
    print 'Plot markers: ', plot_markers

    # Minimum 3 arguments (including script name)
    if len(argv) < 3:
        print 'Argvs: <Flexpart output dir> <nest> <namesSPECIES> [<LEVEL> <RELEASE> <d_list/alldates> <projection> [<depo> <plotting_region (ll_lon,ur_lon,ll_lat,ur_lat)> <style> <plot_markers>]]'
        print 'Example nuclear: ./output/ True (or False) ["nuc1","nuc2"] 0 0 20170925000000,20170925010000 (or alldates) cyl (or lcc) True/False 110,120,20,25 contour True (or False)'
        print 'Marker locations will be taken from file markers.dat'
        print 'MIND: LEVEL and RELEASE start 0 (python style), no sum of LEVELS or RELEASES coded! d_list and plotting_region elements have to be comma-separated! namesSPECIES should contain: ash [g/m2], tracer [kg/m3], so2 [DU] or nuc [Bq/m3].'
        sys.exit(0)

    FLEXPARTOutputDir = argv[1]
    isNested = ast.literal_eval(argv[2])
    namesSPECIES = list(argv[3].split(','))
#    lastSPECIES = str(namesSPECIES[-1])
#    lastSPECIESnum = lastSPECIES[-1]
    numSPECIES = []
    for sp in namesSPECIES:
         numSPECIES.append(sp[-1])

    # Read the general header information common to all files
    H = readH.read_header(FLEXPARTOutputDir, nested = isNested)
    print 'Arguments entered: ',FLEXPARTOutputDir,isNested, namesSPECIES

    if len(argv) >= 4:
#        num_SPECIES = list(argv[3].split(','))
        LEVEL = int(argv[4])
        RELEASE = int(argv[5])
        if argv[6] != 'alldates':
            d_list = list(argv[6].split(','))
        else:
            # if 'alldates': take list of available date-times
            d_list = H.available_dates
        projection = argv[7]
        if len(argv) > 8: 
            depo = ast.literal_eval(argv[8])
            plotting_region = list(argv[9].split(','))
            style = argv[10]
            plot_markers = ast.literal_eval(argv[11])
        
            if plot_markers == True:
                lon_stat = []
                lat_stat = []
                name_stat = []
                if os.path.isfile('./markers.dat'):
                    print 'Reading marker locations'
                    stations = open('./markers.dat', "r")
                    lines = stations.readlines()
                    for i in range(len(lines)):
                        print lines[i].split(' ')
                        lon_stat.append(float(lines[i].split(' ')[1]))
                        lat_stat.append(float(lines[i].split(' ')[2]))
                        name_stat.append(lines[i].split(' ')[0])
                    stations.close()
                else:
                    print 'ERROR: File markers.dat not found!'
                    sys.exit(0)

        
        print 'DEFAULT values UPDATED to: '
        print len(namesSPECIES), LEVEL, RELEASE, d_list, projection, depo, plotting_region, style, plot_markers
        
        
    savepath = FLEXPARTOutputDir

    # Get something needed from header
    outputFiletype = H.unit  # This will determine name of file (conc, time, pptv)
    simDirection = H.direction  # backward or forward

    dates_file = FLEXPARTOutputDir + 'dates'
    instance = 0

    with open(dates_file) as dfile:
        # substitute dfile-list with d_list-list if given
        if len(d_list) > 0:
            dfile = d_list
        for date in dfile:
            current_time = date.strip() 
            print 'Plotting timestamp: ' + str(current_time)

            if os.path.isfile(FLEXPARTOutputDir + 'grid_' + outputFiletype + '_' + str(current_time)+'_001'):
                # Sum over species
                theHorizslice=np.zeros((int(H.numxgrid), int(H.numygrid)))
                theHorizslice_wetdep=np.zeros((int(H.numxgrid), int(H.numygrid)))
                theHorizslice_drydep=np.zeros((int(H.numxgrid), int(H.numygrid)))
                theHorizslice_depo=np.zeros((int(H.numxgrid), int(H.numygrid)))
#                for sp in range(len(namesSPECIES)):
#                for sp in range(int(lastSPECIESnum)):
                for sp in numSPECIES:
                    if isNested == False:
#                        f =  FLEXPARTOutputDir + 'grid_' + outputFiletype + '_' + str(current_time) + '_' + str(sp+1).zfill(3)
                        f =  FLEXPARTOutputDir + 'grid_' + outputFiletype + '_' + str(current_time) + '_' + str(sp).zfill(3)
                    else:
#                        f =  FLEXPARTOutputDir + 'grid_' + outputFiletype + '_nest_' + str(current_time) + '_' + str(sp+1).zfill(3)
                        f =  FLEXPARTOutputDir + 'grid_' + outputFiletype + '_nest_' + str(current_time) + '_' + str(sp).zfill(3)
                    print f
                    grid, wetdep, drydep, itime = readG._readgridBF(H, f)
                    theHorizslice+=grid[:,:,LEVEL,RELEASE,0] # last dimension is AGECLASS -> hard coded for one AGECLASS
                    theHorizslice_wetdep+=wetdep[:,:,RELEASE,0]
                    theHorizslice_drydep+=drydep[:,:,RELEASE,0]
                    theHorizslice_depo=theHorizslice_wetdep+theHorizslice_drydep

                theHorizslice=np.transpose(theHorizslice)
                theHorizslice_depo=np.transpose(theHorizslice_depo)
                theHorizslice_wetdep=np.transpose(theHorizslice_wetdep)
                theHorizslice_drydep=np.transpose(theHorizslice_drydep)
                if simDirection == 'forward':
                    # Convert units
                    if 'so2' in namesSPECIES[0]:
                        # Conversion to Dobson Units:1 g/m2 = 1g/m2* (1/ 64,1300) mol/g 
                        # = (1/ 64,1300) mol/m2 * 6.02e23 molecules/mol = 9.38718E+21 molecules/m2 
                        # = 9.38718E+21/2.6867E20 = 34.93944244 DU. 1 DU is 2.69E20 molecules per meter squared.
                        theHorizslice = theHorizslice*34.93944244
                        theHorizslice_depo=theHorizslice_depo*1e-9
                        theHorizslice_wetdep=theHorizslice_wetdep*1e-9
                        theHorizslice_drydep=theHorizslice_drydep*1e-9
                    elif 'ash' in namesSPECIES[0]:
                        theHorizslice = theHorizslice*1e-9 # Conversion to g/m2
                        theHorizslice_depo=theHorizslice_depo*1e-9
                        theHorizslice_wetdep=theHorizslice_wetdep*1e-9
                        theHorizslice_drydep=theHorizslice_drydep*1e-9
                    else:
                        theHorizslice = theHorizslice*factor
                        theHorizslice_depo=theHorizslice_depo*factor
                        theHorizslice_wetdep=theHorizslice_wetdep*factor
                        theHorizslice_drydep=theHorizslice_drydep*factor

                # To get a total colum for ash and SO2
                if 's' in namesSPECIES[0]:
                    if LEVEL == 0:
                        theHorizslice = theHorizslice*H.layerthickness
                        theHorizslice_depo=theHorizslice_depo*H.layerthickness
                        theHorizslice_wetdep=theHorizslice_wetdep*H.layerthickness
                        theHorizslice_drydep=theHorizslice_drydep*H.layerthickness
                    else:
                        print 'ERROR: change code when summing over more than 1 level'
                        sys.exit()


                print 'Max: ' +str(theHorizslice.max().max())
                print 'Min: ' +str(theHorizslice.min().min())
                if depo == True:
                    print 'Max total deposition: ' + str(theHorizslice_depo.max().max())
                    print 'Min total deposition: ' + str(theHorizslice_depo.min().min())
                    print 'Max wet deposition: ' + str(theHorizslice_wetdep.max().max())
                    print 'Min wet deposition: ' + str(theHorizslice_wetdep.min().min())
                    print 'Max dry deposition: ' + str(theHorizslice_drydep.max().max())
                    print 'Min dry deposition: ' + str(theHorizslice_drydep.min().min())
                
                if instance == 0:

                    instance=1

                    # Get number of grid points, longitudes and latitudes
                    numygrid=H.numygrid
                    numxgrid=H.numxgrid
                    lat=H.latitude
                    lon=H.longitude

                    # use confined plotting region if given
                    if len(plotting_region) > 0:
                        lon_ll=float(plotting_region[0])
                        lon_ur=float(plotting_region[1])
                        lat_ll=float(plotting_region[2])
                        lat_ur=float(plotting_region[3])
                    # use default OUTGRID plotting region
                    else: 
                        lat_ll=lat[0]
                        lon_ll=lon[0]
                        lat_ur=lat[numygrid-1]
                        lon_ur=lon[numxgrid-1]


                    if projection == 'lcc':
 
                        # LAMBERT
                        lon_0 = (lon_ur-lon_ll)/2.0+lon_ll
                        lat_0 = (lat_ur-lat_ll)/2.0+lat_ll
                        lat_1 = abs((lat_ur-lat_ll)/3.0+lat_ll)
                        lat_2 = abs(2.0*(lat_ur-lat_ll)/3.0+lat_ll)
                        #print 'Lon_0: ', lon_0, 'Lat_0: ', lat_0, 'Lat_1: ', lat_1, 'Lat_2: ', lat_2
                        m = Basemap(projection='lcc', lat_1 = lat_1 , lat_2 = lat_2, lon_0 = lon_0, lat_0 = lat_0,\
                            width=6500000, height= 6500000, area_thresh = 7000. ,resolution='l')
                        m1=m

                    elif projection == 'cyl':


                        # CYLINDRIC
                        m = Basemap(projection='cyl',\
                            llcrnrlat=lat_ll, urcrnrlat=lat_ur,\
                            llcrnrlon=lon_ll, urcrnrlon=lon_ur,\
                            resolution='l',area_thresh=7000.)
                        m1=m
                        m2=m
                        m3=m
  
                    if (lon_ll <= -175.0 and lon_ur >= 175.0) and (lat_ll <= -80.0 or lat_ur >= 80.0):
                        delta=30.
                    else:
                        delta=10.
                
                    cyl_basemap = Basemap(projection='cyl',llcrnrlat=lat[0],urcrnrlat=lat[numygrid-1],llcrnrlon=lon[0],urcrnrlon=lon[numxgrid-1],resolution=None )
                    lons,lats = cyl_basemap.makegrid(numxgrid,numygrid)
                    x,y = m(lons,lats)

                fig = plt.figure(figsize=(12,12))
                m.drawcoastlines()
                m.drawstates()
                m.drawcountries()
                parallels = np.arange(lat_ll,lat_ur,delta)
                m.drawparallels(parallels,labels=[1,0,0,0],fontsize=10)
                meridians = np.arange(lon_ll,lon_ur,delta)
                m.drawmeridians(meridians,labels=[0,0,0,1],fontsize=10)

                colors =  [('White')]+[('White')]+[('White')]+[('LavenderBlush')]+[('LavenderBlush')]+[('LavenderBlush')]+[('PowderBlue')]+[('PowderBlue')]+[('LightBlue')]+[('LightBlue')]+[('LightSkyBlue')]+[('LightSkyblue')]+[('SkyBlue')]+[('Skyblue')]+[('DodgerBlue')]+[('DodgerBlue')]+[('RoyalBlue')]+[('RoyalBlue')]+[('MediumBlue')]+[('MediumBlue')]+[(plt.cm.jet(i)) for i in xrange(20,256)]
                delia_cmap2 = LinearSegmentedColormap.from_list('new_map', colors, N=256)
       
                # Mark release point
                releaseXLOC = (H.xp1[RELEASE] +    \
                H.xp2[RELEASE] ) / 2.0
                releaseYLOC = (H.yp1[RELEASE] +    \
                H.yp2[RELEASE] ) / 2.0

                # additional markers
                if plot_markers == True:
                    x1, y1 = m(lon_stat, lat_stat)
                    plt.plot(x1, y1, marker = 'o', linestyle = 'None', color = 'darkred', markersize=6)
                    for i in range(len(lon_stat)): # lable markers
                        x11, y11 = m(lon_stat[i]-2.0, lat_stat[i]+1.0)
                        plt.annotate(name_stat[i], xy = (x1[i], y1[i]), xytext = (x11, y11),color='darkred',fontsize=10,zorder=3)

                x1, y1 = m(releaseXLOC, releaseYLOC)
                plt.plot(x1, y1, 'r^', markersize=8)
                if theHorizslice.max().max() > 1.0E-30:
                    if style == 'contour':
                        cs = m.contourf(x,y,theHorizslice,cmap=delia_cmap2,locator=ticker.LogLocator(),zorder=1)
                    else:
                        cs = m.pcolormesh(x,y,theHorizslice,cmap=delia_cmap2,vmin=0.,vmax=theHorizslice.max().max(),zorder=1)
#                        cs = m.pcolormesh(x,y,theHorizslice,cmap=delia_cmap2,vmin=0.,vmax=0.15,zorder=1)
                    cbar = m.colorbar(cs, location='bottom', pad=0.3)
                    if simDirection == 'backward':
                        cbar.set_label('Residence Time (s)')
                    else:
                        if 'nuc' in namesSPECIES[0]:
                            cbar.set_label('Concentration (Bq/m3)')
                        elif 'tracer' in namesSPECIES[0]:
                            cbar.set_label('Concentration (kg/m3)')
                        elif 'ash' in namesSPECIES[0]:
                            cbar.set_label('Total column (g/m2)')
                        else:
                            cbar.set_label('Total column (DU)')
                plt.title('2D slice at time '+ str(current_time))
                # sum species if several species indices are entered
                if len (namesSPECIES) > 1:
                    num_SPECIES_help = 'SUM'
                else:
                    num_SPECIES_help = num_SPECIES[0]
                filename = savepath + '2D_' + str(current_time) + '_species' + str(num_SPECIES_help) + '_release' + str(RELEASE) + '_level' + str(LEVEL)
                if isNested == False:
                    filename+='.png'
                else:
                    filename+='_nest.png'
                plt.savefig(filename, bbox_inches = 'tight')
                print 'Saving figure as ' + filename
                plt.close()
                if depo == True:
                    if theHorizslice_depo.max().max() > 1.0E-30:
                        # total depo
                        fig = plt.figure(figsize=(12,12))
                        m1.drawcoastlines()
                        m1.drawstates()
                        m1.drawcountries()
                        m1.drawparallels(parallels,labels=[1,0,0,0],fontsize=10)
                        m1.drawmeridians(meridians,labels=[0,0,0,1],fontsize=10)
                        x1, y1 = m1(releaseXLOC, releaseYLOC)
                        plt.plot(x1, y1, 'r^', markersize=8)
                        if style == 'contour':
                            cs = m1.contourf(x,y,theHorizslice_depo,cmap=delia_cmap2,locator=ticker.LogLocator(),zorder=1)
                        else:
                            cs = m1.pcolormesh(x,y,theHorizslice_depo,cmap=delia_cmap2,vmin=0.,vmax=theHorizslice_depo.max().max(),zorder=1)
                        cbar = m1.colorbar(cs, location='bottom', pad=0.3)
                        if 'nuc' in namesSPECIES:
                            cbar.set_label('Total deposition (Bq/m2)')
                        elif 'tracer' in namesSPECIES:
                            cbar.set_label('Total deposition (kg/m2)')
                        else:
                            cbar.set_label('Total deposition (g/m2)')
                        plt.title('2D total deposition slice at time '+ str(current_time))
                        filename = savepath + '2D_depo_' + str(current_time) + '_species' + str(num_SPECIES_help) + '_release' + str(RELEASE) + '_level' + str(LEVEL)
                        if isNested == False:
                            filename+='.png'
                        else:
                            filename+='_nest.png'
                        plt.savefig(filename, bbox_inches = 'tight')
                        print 'Saving figure as ' + filename
                        plt.close()
                    if theHorizslice_wetdep.max().max() > 1.0E-30:
                        # wet depo
                        fig = plt.figure(figsize=(12,12))
                        m2.drawcoastlines()
                        m2.drawstates()
                        m2.drawcountries()
                        m2.drawparallels(parallels,labels=[1,0,0,0],fontsize=10)
                        m2.drawmeridians(meridians,labels=[0,0,0,1],fontsize=10)
                        x1, y1 = m1(releaseXLOC, releaseYLOC)
                        plt.plot(x1, y1, 'r^', markersize=8)
                        if style == 'contour':
                            cs = m2.contourf(x,y,theHorizslice_wetdep,cmap=delia_cmap2,locator=ticker.LogLocator(),zorder=1)
                        else:
                            cs = m2.pcolormesh(x,y,theHorizslice_wetdep,cmap=delia_cmap2,vmin=0.,vmax=theHorizslice_wetdep.max().max(),zorder=1)
                        cbar = m2.colorbar(cs, location='bottom', pad=0.3)
                        if 'nuc' in namesSPECIES:
                            cbar.set_label('Wet deposition (Bq/m2)')
                        elif 'tracer' in namesSPECIES:
                            cbar.set_label('Wet deposition (kg/m2)')
                        else:
                            cbar.set_label('Wet deposition (g/m2)')
                        plt.title('2D wet deposition slice at time '+ str(current_time))
                        filename = savepath + '2D_wetdep_' + str(current_time) + '_species' + str(num_SPECIES_help) + '_release' + str(RELEASE) + '_level' + str(LEVEL)
                        if isNested == False:
                            filename+='.png'
                        else:
                            filename+='_nest.png'
                        plt.savefig(filename, bbox_inches = 'tight')
                        print 'Saving figure as ' + filename
                        plt.close()
                    if theHorizslice_drydep.max().max() > 1.0E-30:
                        # dry depo
                        fig = plt.figure(figsize=(12,12))
                        m3.drawcoastlines()
                        m3.drawstates()
                        m3.drawcountries()
                        m3.drawparallels(parallels,labels=[1,0,0,0],fontsize=10)
                        m3.drawmeridians(meridians,labels=[0,0,0,1],fontsize=10)
                        x1, y1 = m1(releaseXLOC, releaseYLOC)
                        plt.plot(x1, y1, 'r^', markersize=8)
                        if style == 'contour':
                            cs = m3.contourf(x,y,theHorizslice_drydep,cmap=delia_cmap2,locator=ticker.LogLocator(),zorder=1)
                        else:
                            cs = m3.pcolormesh(x,y,theHorizslice_drydep,cmap=delia_cmap2,vmin=0.,vmax=theHorizslice_drydep.max().max(),zorder=1)
                        cbar = m3.colorbar(cs, location='bottom', pad=0.3)
                        if 'nuc' in namesSPECIES:
                            cbar.set_label('Dry deposition (Bq/m2)')
                        elif 'tracer' in namesSPECIES:
                            cbar.set_label('Dry deposition (kg/m2)')
                        else:
                            cbar.set_label('Dry deposition (g/m2)')
                        plt.title('2D dry deposition slice at time '+ str(current_time))
                        filename = savepath + '2D_drydep_' + str(current_time) + '_species' + str(num_SPECIES_help) + '_release' + str(RELEASE) + '_level' + str(LEVEL)
                        if isNested == False:
                            filename+='.png'
                        else:
                            filename+='_nest.png'
                        plt.savefig(filename, bbox_inches = 'tight')
                        print 'Saving figure as ' + filename
                        plt.close()


            else:
                print 'File does not exist: ' + str(FLEXPARTOutputDir) + 'grid_' + outputFiletype + '_(nest)' + str(current_time) + '_001'
    print 'SUCCESSFULLY FINISHED PLOTTING OF FLEXPART BINARY OUTPUT!'

#----------------------------------------------
if __name__ == "__main__":

    main()

























