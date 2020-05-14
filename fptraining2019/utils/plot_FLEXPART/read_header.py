#!/usr/bin/env python
"""
D. Arnold 


This is taken from pflexible (contact J. Bukhardt for details)
and adapted so that we only have the reading of the header
for FLEXPART version 8.X and further. Some other things have
been modified. Classes and helper functions also from pflexible


"""



#builtin imports
import pdb
import os 
import struct
import re
import traceback
import time
import datetime
import traceback

#Dependencies:
# Numpy
import numpy as np




def read_header(pathname, **kwargs):
    """
    The readheader function returns a special class (Structure) which behaves
    like a dictionary. It contains all the metadata from the simulation which
    is contained in the "header" or "header_nest" binary files from the model 
    output.
    
    .. warning::
        It is recommended to use the :class:`Header` class: H = pf.Header(path)

    This version is using the BinaryFile class rather than FortFlex. 

    Usage::

        > H = readheader(inputpath)

    Returns a dictionary

        H = dictionary like object with all the run metadata. TODO: Fill in keys.

    Arguments

      .. tabularcolumns::  |l|L|

      =============       ========================================
      keyword             Description [default]
      =============       ========================================
      pathname            FLEXPART run output directory
      readp               read release points 0=no, [1]=y
      readp_ff            readp_ff (read releases using Fortran [False]
      nested              nested output [False] or True
      version             version of FLEXPART, default = 'V8'
      =============       ========================================

    .. note::
        **This function is in development**

        This function is being developed so that there is no dependence on
        using f2py to compile the FortFlex module. So far it seems to work, but is
        notably slower than FortFlex. Please report any bugs found.



    """


    OPS = Structure()
    OPS.readp = True 
    OPS.readp_ff = False
    OPS.nested = False  
    OPS.ltopo = 1 # 1 for AGL, 0 for ASL
    OPS.version = 'V8'
    OPS.headerfile = None

    #BW compat fixes
    if 'nest' in kwargs.keys():
       raise IOError("nest is no longer a valid keyword, see docs. \n Now use nested=True or nested=False")


    if 'nested' in kwargs.keys():
       if kwargs['nested'] is 1: 
           print("Warning, use of nested=1, deprecated converting to nested=True")
           kwargs['nested'] = True

    if 'nested' in kwargs.keys():
       if kwargs['nested'] is 0: 
           print("Warning, use of nested=0, deprecated converting to nested=False")
           kwargs['nested'] = False
       
    OPS.update(kwargs)


    #print "Reading Header with:\n"
    
    #for o in OPS:
    #    print "%s ==> %s" % (o, OPS[o])


    # Utility functions
    skip = lambda n = 8 : bf.seek(n, 1)
    getbin = lambda dtype, n = 1 : bf.read(dtype, (n,))

    #H={} #create dictionary for header
    h = Structure()

    if OPS.nested is True:
        filename = os.path.join(pathname, 'header_nest')
        h['nested'] = True;
    else:
        filename = os.path.join(pathname, 'header')
        h['nested'] = False;


    if OPS.headerfile:
        filename = os.path.join(pathname, OPS.headerfile);
 
    # Open header file in binary format
    if not os.path.exists(filename):   
        raise IOError("No such file: {0}".format(filename))

    else:
        try:
            bf = BinaryFile(filename, order="fortran")
        except:
            raise IOError("Error opening: {0} with BinaryFile class".format(filename))

    #Get available_dates from dates file in same directory as header
    datefile = os.path.join(pathname, 'dates')


    if not os.path.exists(datefile):
        raise IOError("No such file: {0}".format(datefile))
    else:
        try:
            fd = file(datefile, 'r').readlines()
        except:
            raise IOError("Could not read datefile: {0}".format(datefile))

    #get rid of any duplicats (a fix for the forecast system)
    fd = sorted(list(set(fd)))
    h['available_dates'] = [d.strip('\n') for d in fd]

    #Define Header format and create Dictionary Keys
    I = {0:'rl0', 1:'ibdate', 2:'ibtime', 3:'version', \
         4:'rl1', 5:'loutstep', 6:'loutaver', 7:'loutsample', \
         8:'rl2', 9:'outlon0', 10:'outlat0', 11:'numxgrid', \
         12:'numygrid', 13:'dxout', 14:'dyout', 15:'rl3', 16:'numzgrid', \
         }
    #format for binary reading first part of the header file
    Dfmt = ['i', 'i', 'i', '256S', '2i', 'i', 'i', 'i', '2i', 'f', 'f', 'i', 'i', 'f', 'f', '2i', 'i']
    if bf:
        a = [bf.read(fmt) for fmt in Dfmt]
        for j in range(len(a)):
            h[I[j]] = a[j]
        #add items to the dictionary
        ss_start = datetime.datetime.strptime(str(h['ibdate']) + str(h['ibtime']).zfill(6), \
                                           '%Y%m%d%H%M%S')

        h['simulationstart'] = ss_start
        h['pathname'] = pathname
        h['decayconstant'] = 0  
        h['outheight'] = np.array([getbin('f') for i in range(h['numzgrid'])])
        skip()
        h['jjjjmmdd'] = getbin('i')
        h['hhmmss'] = getbin('i')  
        skip()
        h['nspec'] = getbin('i') / 3
        h['numpointspec'] = getbin('i')
        skip()
        #Read in the species names and levels for each nspec
        h['numzgrid'] = []
        h['species'] = [] 
        #temp dictionaries
        for i in range(h['nspec']):
            one = getbin('i'); # input skipped ??
            wd = getbin('c', 10);  # input skipped ??
            skip();
            one = getbin('i'); # input skipped ??
            dd = getbin('c', 10);  # input skipped ??
            skip();
            h['numzgrid'].append(getbin('i'))
            h['species'].append(''.join([getbin('c') for i in range(10)]).strip())
            skip();
        h['numpoint'] = getbin('i')
        # initialise release fields
        I = {2:'kindz', 3:'xp1', 4:'yp1', 5:'xp2', \
           6:'yp2', 7:'zpoint1', 8:'zpoint2', 9:'npart', 10:'mpart'}
        h['ireleasestart'] = []
        h['ireleaseend'] = []  
        for k, v in I.iteritems():
            h[v] = np.zeros(h['numpoint']) #create zero-filled lists in H dict
        h['compoint'] = []
        h['xmass'] = np.zeros((h['numpoint'], h['nspec']))
        r1 = getbin('i')
        for i in range(h['numpoint']):
            r2 = getbin('i')
            i1 = getbin('i')
            i2 = getbin('i')
            #h['ireleasestart'].append( ss_start + datetime.timedelta(seconds=float(i1)) )
            #h['ireleaseend'].append( ss_start + datetime.timedelta(seconds=float(i2)) )  
            h['ireleasestart'].append(i1)
            h['ireleaseend'].append(i2)  
            h['kindz'][i] = getbin('h') # This is an int16, might need to to change something
            skip() #get xp, yp,...

            h['xp1'][i] = getbin('f')
            h['yp1'][i] = getbin('f')
            h['xp2'][i] = getbin('f')
            h['yp2'][i] = getbin('f')
            h['zpoint1'][i] = getbin('f')
            h['zpoint2'][i] = getbin('f')

            skip() #get n/mpart
            h['npart'][i] = getbin('i')
            h['mpart'][i] = getbin('i')

            r3 = getbin('i')
            # initialise release fields

            l = getbin('i')#get compoint length?
            gt = bf.tell() + l #create 'goto' point
            sp = ''
            while re.search("\w", getbin('c')): #collect the characters for the compoint
                bf.seek(-1, 1)
                sp = sp + getbin('c')

            bf.seek(gt) #skip ahead to gt point

            h['compoint'].append(sp) #species names in dictionary for each nspec
            #h['compoint'].append(''.join([getbin('c') for i in range(45)]))
            r1 = getbin('i')
            #now loop for nspec to get xmass
            for v in range(h['nspec']):
                Dfmt = ['i', 'f', '2i', 'f', '2i', 'f', 'i']
                a = [bf.read(fmt) for fmt in Dfmt]
                h['xmass'][i, v] = a[1]

            if OPS.readp is False:
                """
                We get the first set of release points here in order
                to get some information, but skip reading the rest  
                """
                bf.seek(before_readp)
                bf.seek(119 * h['numpoint'] + (h['nspec'] * 36) * h['numpoint'] + 4, 1)
                break


        rl = getbin('i')
        jnk_method = getbin('i')
        h['lsubgrid'] = getbin('i')
        h['lconvection'] = getbin('i')
        if rl == 20:
            h['ind_source'] = getbin('i')
            h['ind_receptor'] = getbin('i')
        skip()
        h['nageclass'] = getbin('i')
        Lage_fmt = ['i'] * h.nageclass
        jnk_lage = [bf.read(fmt) for fmt in Lage_fmt]
        nx = h['numxgrid']
        ny = h['numygrid']
        Dfmt = ['f'] * nx 
        h['oro'] = np.zeros((nx, ny), np.float)
        skip()
        for ix in range(nx):
            #h['oro'][ix]=[getbin('f') for jx in range(ny)]
            # The next is *much* faster!
            h['oro'][ix] = getbin('f', ny)
            skip()
        if h['loutstep'] < 0:
            h['nspec'] = h['numpoint']



        lons = np.arange(h['outlon0'], h['outlon0'] + (h['dxout'] * h['numxgrid']), h['dxout'])
        lats = np.arange(h['outlat0'], h['outlat0'] + (h['dyout'] *  h['numygrid']),h['dyout'])
        h['latitude']= lats
        h['longitude']= lons

        bf.close()

        # Calculate Height (outheight + topography)
        # There is an offset issue here related to the 0-indexing. Be careful.
        Z = h['oro'] #z is a numpy array
        nz = h['numzgrid'][0]
        Heightnn = np.zeros((nx, ny, nz), np.float)
        for ix in range(nx):
            if OPS.ltopo == 1:
                Heightnn[ix, :, 0] = Z[ix, :]
            else:
                Heightnn[ix, :, 0] = np.zeros(ny)
            for iz in range(nz):
                if OPS.ltopo == 1:
                    Heightnn[ix, :, iz] = [h['outheight'][iz] + Z[ix, y] for y in range(ny)]
                else:
                    Heightnn[ix, :, iz] = h['outheight'][iz]
        h['Area'] = gridarea(h)
        h['Heightnn'] = Heightnn


    #############  A FEW ADDITIONS ###########
    # add a few default attributes
    # optionally, use fortran routine to read
    # release points (deprecated)
    h.path = pathname
    if OPS.readp_ff:
        h = _read_headerFF(filename, h,
                          nxmax=h.numxgrid, nymax=h.numygrid, nzmax=h.numzgrid,
                          maxspec=h.nspec, maxageclass=h.nageclass,
                          maxpoint=h.numpoint)

    # Convert ireleasestart and ireleaseend to datetimes
    if OPS.readp:
        releasestart, releaseend = [], []
        for i in range(h.numpointspec):
            releasestart.append(h.simulationstart + \
                                 datetime.timedelta(seconds=int(h.ireleasestart[i])))
            releaseend.append(h.simulationstart + \
                               datetime.timedelta(seconds=int(h.ireleaseend[i])))
        h.releasestart = releasestart
        h.releaseend = releaseend[:h.numpointspec]
        h.releasetimes = [b - ((b - a) / 2) for a, b in zip(h.releasestart, h.releaseend)]

    # Add datetime objects for dates
    available_dates_dt = []
    for i in h.available_dates:
        available_dates_dt.append(datetime.datetime(
            int(i[:4]), int(i[4:6]), int(i[6:8]), int(i[8:10]), int(i[10:12]), int(i[12:])))
    h.available_dates_dt = available_dates_dt
    h.first_date = available_dates_dt[0]
    h.last_date = available_dates_dt[-1]
    h.ageclasses = np.array([act - h.simulationstart for act in h.available_dates_dt])
    h.numageclasses = len(h.ageclasses)


    # Add other helpful attributes
    h.nxmax = h.numxgrid
    h.nymax = h.numygrid
    h.nzmax = h.numzgrid
    h.maxspec = h.nspec
    h.maxpoint = h.numpoint
    h.area = h.Area

    if OPS.readp:
        h.xpoint = h.xp1
        h.ypoint = h.yp1

    # Add release unit derived from kindz
    if 'kindz' not in h.keys():
        h.kindz = [0]
        h.alt_unit = 'unkn.'
    if 3 in h.kindz:
        h.alt_unit = 'hPa'
    elif 2 in h.kindz:
        h.alt_unit = 'm.a.s.l.'
    elif 1 in h.kindz:
        h.alt_unit = 'm.a.g.l.'

    #
    if h.loutstep > 0:
        h.direction = 'forward'
        h.unit = 'conc' #could be pptv
        h.plot_unit = 'ppb'
    else:
        h.direction = 'backward'
        h.unit = 'time'
        h.plot_unit = 'ns / kg' #Not sure about this


    # Units based on Table 1, ACP 2005
    if h.direction == 'forward':
        if h.ind_source == 1:
            if h.ind_receptor == 1:
                h.output_unit = 'ng m-3'
            if h.ind_receptor == 2:
                h.output_unit = 'pptm'
        if h.ind_source == 2:
            if h.ind_receptor == 1:
                h.output_unit = 'ng m-3'
            if h.ind_receptor == 2:
                h.output_unit = 'pptm'
    if h.direction == 'backward':
        if h.ind_source == 1:
            if h.ind_receptor == 1:
                h.output_unit = 's'
            if h.ind_receptor == 2:
                h.output_unit = 's m^3 kg-1'
        if h.ind_source == 2:
            if h.ind_receptor == 1:
                h.output_unit = 's kg m-3'
            if h.ind_receptor == 2:
                h.output_unit = 's'



    # Add layer thickness
    layerthickness = [h.outheight[0]]
    for i, lh in enumerate(h.outheight[1:]):
        layerthickness.append(lh - h.outheight[i])
    h.layerthickness = layerthickness

    h.options = OPS





    print 'Header read: %s' % filename






    return h

#########
def gridarea(H):
    """returns an array of area corresponding to each nx,ny,nz

    Usage::

        > area = gridarea(H)


    Returns
        OUT = array area corresponding to nx,ny,nz

    Arguments
        H  = :class:`Header` object from readheader function.

    """


    pih = np.pi / 180.
    r_earth = 6.371e6
    cosfunc = lambda y : np.cos(y * pih) * r_earth
    nz = H['numzgrid']
    nx = H['numxgrid']
    ny = H['numygrid']
    outlat0 = H['outlat0']
    dyout = H['dyout']
    dxout = H['dxout']
    area = np.zeros((nx, ny))

    for iy in range(ny):
        ylata = outlat0 + (float(iy) + 0.5) * dyout #NEED TO Check this, iy since arrays are 0-index
        ylatp = ylata + 0.5 * dyout
        ylatm = ylata - 0.5 * dyout
        if (ylatm < 0 and ylatp > 0): hzone = dyout * r_earth * pih
        else:
            cosfact = cosfunc(ylata)
            cosfactp = cosfunc(ylatp)
            cosfactm = cosfunc(ylatm)
            if cosfactp < cosfactm:
                hzone = np.sqrt(r_earth ** 2 - cosfactp ** 2) - np.sqrt(r_earth ** 2 - cosfactm ** 2)
            else:
                hzone = np.sqrt(r_earth ** 2 - cosfactm ** 2) - np.sqrt(r_earth ** 2 - cosfactp ** 2)

        gridarea = 2.*np.pi * r_earth * hzone * dxout / 360.
        for ix in range(nx):
            area[ix, iy] = gridarea

    return area





######### HELPER CLASSES##############




class BinaryFile(object):

    """
    
    BinaryFile: A class for accessing data to/from large binary files
   

    The data is meant to be read/write sequentially from/to a binary file.
    One can request to read a piece of data with a specific type and shape
    from it.  Also, it supports the notion of Fortran and C ordered data,
    so that the returned data is always well-behaved (C-contiguous and
    aligned).

    This class is seeking capable.

    :Author:   Francesc Alted
    :Contact:  faltet@pytables.org
    :Created:  2010-03-18
    :Acknowledgment: Funding for the development of this code is provided
         through the Norwegian Research Council VAUUAV project #184724, 2010

    """


    # Common types whose conversion can be accelerated via the struct
    # module
    structtypes = {
        'i1': 'b', 'i2': 'h', 'i4': 'i',
        'f4': 'f', 'f8': 'd',
        }

    def __init__(self, filename, mode="r", order="fortran"):
        """Open the `filename` for write/read binary files.

        The `mode` can be 'r', 'w' or 'a' for reading (default),
        writing or appending.  The file will be created if it doesn't
        exist when opened for writing or appending; it will be
        truncated when opened for writing.  Add a '+' to the mode to
        allow simultaneous reading and writing.

        `order` specifies whether the file is is written in 'fortran'
        or 'c' order.
        """
        self.mode = mode + "b"
        self.file = open(filename, mode=self.mode, buffering=1)
        """The file handler."""
        if order not in ['fortran', 'c']:
            raise ValueError, "order should be either 'fortran' or 'c'."
        self.order = order
        """The order for file ('c' or 'fortran')."""


    def read(self, dtype, shape=(1,)):
        """Read an array of `dtype` and `shape` from current position.

        `shape` must be any tuple made of integers or even () for scalars.

        The current position will be updated to point to the end of
        read data.
        """
        if not isinstance(dtype, np.dtype):
            dtype = np.dtype(dtype)
        if type(shape) is int:
            shape = (shape,)  
        if type(shape) is not tuple:
            raise ValueError, "shape must be a tuple"
        length = dtype.itemsize
        rank = len(shape)
        if rank == 1:
            length *= shape[0]
        elif rank > 1:
            length *= np.array(shape).prod()

        # Correct the shape in case dtype is multi-dimensional
        if shape != (1,):
            shape = shape + dtype.shape
        else:
            shape = dtype.shape
        rank = len(shape)

        if shape in (1, (1,)):
            order = "c"
        else:
            order = self.order

        # Read the data from file
        data = self.file.read(length)
        if len(data) < length:
            raise EOFError, "Asking for more data than available in file."

        # Convert read string into a regular array, or scalar
        dts = dtype.base.str[1:]
        if rank == 0:
            if dts[1] == "S":
                data = str(data)
            elif dts in self.structtypes:
                data = struct.unpack(self.structtypes[dts], data)[0]
        else:
            data = np.ndarray(shape=shape, buffer=data, dtype=dtype.base)
            if rank == 0:
                # Retrieve the scalar out of the 0-dim array
                data = data[()]

        if rank > 1:
            # If original data file is in fortran mode, reverse the
            # shape first
            if order == "fortran":
                shape = [i for i in shape[::-1]]
            data = data.reshape(shape)
            # If original data file is in fortran mode, do a transpose.
            # As the shape was reversed previously, we get the original
            # shape again.
            if self.order == "fortran":
                data = data.transpose().copy()
            # Do an additional copy just in case the array is not
            # well-behaved (i.e., it is not aligned or not contiguous).
            elif not data.flags.behaved:
                data = data.copy()
        return data
    def write(self, arr):
        """Write an `arr` to current position.

        The current position will be updated to point to the end of
        written data.
        """
        # Transpose data if case we need to
        if (self.order == "fortran") != (arr.flags.fortran):
            arr = arr.transpose().copy()
        # Write the data to file
        self.file.write(arr.data)


    def seek(self, offset, whence=0):
        """Move to new file position.

        Argument offset is a byte count.  Optional argument whence
        defaults to 0 (offset from start of file, offset should be >=
        0); other values are 1 (move relative to current position,   
        positive or negative), and 2 (move relative to end of file,  
        usually negative, although many platforms allow seeking beyond
        the end of a file).  If the file is opened in text mode, only 
        offsets returned by tell() are legal.  Use of other offsets   
        causes undefined behavior.
        """
        self.file.seek(offset, whence)


    def tell(self):
        "Returns current file position, an integer (may be a long integer)."
        return self.file.tell()


    def flush(self):
        "Flush buffers to file."
        self.file.flush()


    def close(self):
        "End access to file."
        self.file.close()








class Structure(dict):
    """ A 'fancy' dictionary that provides 'MatLab' structure-like
    referencing. 

    .. warning::
        may be replaced with a pure dict in future release.
        
    """ 
    def __getattr__(self, attr):
        # Fake a __getstate__ method that returns None
        if attr == "__getstate__":
            return lambda: None   
        return self[attr]

    def __setattr__(self, attr, value):
        self[attr] = value

    def set_with_dict(self, D):
        """ set attributes with a dict """
        for k in D.keys():
            self.__setattr__(k, D[k])


class Header(Structure):
    """This is the primary starting point for processing FLEXPART output.
    The Header class ( :class:`Structure` ) behaves like a dictionary.   
    It contains all the metadata from the simulation run as read from the
    "header" or "header_nest" binary files from the model output.

    This version is using the BinaryFile class rather than FortFlex. 

    Usage::

        > H = pf.Header(inputpath)
        > H.keys() #provides a list of keys available

    Returns a dictionary

        H = dictionary like object with all the run metadata. TODO: Fill in keys.

        
    Arguments

      .. tabularcolumns::  |l|L|

      ==============        ========================================
      keyword               Description [default]
      ==============        ========================================
      path                  path to the run directory
      headerfile            name of the header file if non standard
      readheader_ops        optional dictionary to pass readheader 
      ==============        ========================================

    Arguments for readheader_ops

      .. tabularcolumns::  |l|L|

      =============       ========================================
      keyword             Description [default]
      =============       ========================================
      pathname            FLEXPART run output directory
      readp               read release points 0=no, [1]=y
      readp_ff            readp_ff (read releases using Fortran [False]
      nested              nested output True or [False]
      version             version of FLEXPART, default = 'V8'
      =============       ========================================
  
    
    .. note::
        **This function is in development**

        This function is being developed so that there is no dependence on
        using f2py to compile the FortFlex module. It is working using the
        :class:`BinaryFile`, but is notably slower than :class:`FortFlex`. 
        Please report any bugs found.

  
    """
    def __init__(self, path=None, headerfile=None, version='V8', **readheader_ops):
        """

        
        """
        if version == 'V6':
        
            print("Reading output as Version 6")
            try:
                print("Trying to read header:\n{0}\n with readheaderV6".format(path))
                h = readheaderV6(path, **readheader_ops)
                self.set_with_dict(h)
                self.lonlat()
                self.version = 'V6'
            except:
                traceback.print_exc()
                raise IOError('Could not set header variables. Does the path exist?\n\n{0}\n'.format(path))
        
        elif version == 'V8':
            try:
                h = readheader(path, **readheader_ops)
                self.set_with_dict(h)
                self.lonlat()
                self.version = 'V8'
            except:
                traceback.print_exc()
                raise IOError('''
                Could not set header variables.
                Is the output version 8 and does the path exist?\n{0}'''.format(path))
            


    def lonlat(self):
        """ Add longitude and latitude attributes using data from header """
        lons = np.arange(self.outlon0, self.outlon0 + (self.dxout * self.numxgrid), self.dxout)
        lats = np.arange(self.outlat0, self.outlat0 + (self.dyout * self.numygrid), self.dyout)
        self.longitude = lons
        self.latitude = lats

    def read_grid(self, **kwargs):   
        """ see :func:`read_grid` """
        self.FD = read_grid(self, **kwargs)

    def fill_backward(self, **kwargs):   
        """ see :func:`fill_backward` """
        fill_backward(self, add_attributes=True, **kwargs)

    def add_trajectory(self, **kwargs):
        """ see :func:`read_trajectories` """
        self.trajectory = read_trajectories(self)

    def add_fires(self, **kwargs):
        """ uses the :mod:`emissions` module to read the MODIS hotspot data and
        add it to the header class as a 'fires' attribute.
        """

        from nilu.pflexpart import emissions as em
        self.fires = None
        for day in self.available_dates_dt:
            # day = day[:8]
            firedata = em.MODIS_hotspot(day)
            daily = firedata.daily
            #pdb.set_trace() 
            if daily is None:
                continue
            else:
                if self.fires == None:
                    self.fires = daily
                else:
#                    print day
#                    print self.fires.shape
#                    print daily.shape

                    self.fires = np.hstack((self.fires, daily)).view(np.recarray)

    def closest_dates(self, dateval, fmt=None, take_set=False):
        """ given an iterable of datetimes, finds the closest dates.
            if passed a list, assumes it is a list of datetimes
            
            if take_set=True, then a set of unique values will be returned.
            This can be used with H.read_grid as the value for time_ret to 
            return only the grids matching the array of times.
            See (e.g. `extract_curtain`).
        """

        try:
            vals = [closest(d, self['available_dates_dt']) for d in dateval]
            if take_set:
                return list(set(vals))
            else:
                return vals

        except IOError:
            print('If dateval is iterable, must contain datetimes')


    def closest_date(self, dateval, fmt=None):
        """ given a datestring or datetime, tries to find the closest date.
            if passed a list, assumes it is a list of datetimes
            
        """ 

        if isinstance(dateval, str):
            if not fmt:
                if len(dateval) == 8:
                    fmt = '%Y%m%d'   
                if len(dateval) == 14:
                    fmt = '%Y%m%d%H%M%S'
                else:
                    raise IOError("no format provided for datestring")
            print("Assuming date format: {0}".format(fmt))
            dateval = datetime.datetime.strptime(dateval, fmt)

        return closest(dateval, self['available_dates_dt'])

