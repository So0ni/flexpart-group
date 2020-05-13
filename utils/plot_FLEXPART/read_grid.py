#!/usr/bin/env python
"""
D. Arnold

This is taken from pflexible (contact J. Bukhardt for details)
and adapted so that we only have the reading of the header
for FLEXPART version 8.X and further. Some other things have
been modified. Classes and helper functions also from pflexible

"""

# Imported modules
import os
import struct
import traceback
import datetime
import traceback

#Dependencies:
# Numpy
import numpy as np

def read_grid(H, **kwargs):
    """
    Accepts a header object as input, returns dictionary of Grid values
    keyed by species and datestring from H['available_dates']. A grid instance from this
    dictionary may be passed to the get_slabs function to return a dictionary of slabs
    suitable for plotting with the plotting routines. See below for more information.

    **DEPENDENCY**
        Requires FortFlex.so module compiled using f2py. See FortFlex.f for more details.

    Usage::

        > FLEXDATA = read_grid(H,**kwargs)

    Returns:

        A grid dictionary key by tuples with (species,available_dates), where species is
        and integer. See grid.keys()

        FLEXDATA[(s,datestring)]['grid']
        FLEXDATA[(s,datestring)]['itime']
        FLEXDATA[(s,datestring)]['shape']
        FLEXDATA[(s,datestring)]['max']
        FLEXDATA[(s,datestring)]['min']
        FLEXDATA[(s,datestring)]['timestamp']
        FLEXDATA[(s,datestring)]['species']
        FLEXDATA[(s,datestring)]['gridfile']
        FLEXDATA[(s,datestring)]['rel_i']
        FLEXDATA[(s,datestring)]['spec_i']

    Arguments

      .. tabularcolumns::  |l|L|

      =============         ========================================
      keyword               Description [default]
      =============         ========================================
      date                  which yyyymmddhhmmss from available_dates
                            or use (time_ret)
      time_ret              index to time
      unit                  'conc', 'pptv', ['time'], 'footprint'
      nspec_ret             numspecies
      pspec_ret             index to ???
      age_ret               index to ageclass
      nested                obtained from H['nested']
      BinaryFile            Use BinaryFile vs. FortFlex [False]
      getwet                True, [False]
      getdry                True, [False]
      scaledepo             A float value to scale deposition [1.0]
      scaleconc             A float value to scale conc [1.0]
      decayconstant         A float for decay const. [9e6]
      calcfoot              Will cause footprint to be calculated
                            [False]
      verbose               more output
      version               A string 'V8' or 'V6', ['V8']
      =============         ========================================


    .. note::
        most arguments are able to be extracted fro the header "H"


    """
 
    H.version == 'V8'
    if H.version == 'V8':
        return readgridV8(H, **kwargs)
    #if H.version == 'V6':
    #    return readgridV6(H, **kwargs)
    else:
        raise IOError("No version attribute defined for Header.")

def readgridV8(H, **kwargs):

    """
    Accepts a header object as input, and selects appropriate readgrid function
    to use for reading in data from the flexpart binary Fortran files.

    See the :func:`read_grid` for more information on keyword arguments

    This is the 'V8' version of the function.

    """
    ## OPS is the options Structure, sets defaults, then update w/ kwargs
    #print 'cda before Structure'
    OPS = Structure()
    #print 'cda start defining a Structure'
    OPS.unit = H.unit
    OPS.getwet = False
    OPS.getdry = False
    OPS.nspec_ret = 0
    OPS.npspec_int = False  # allows to select an index of npsec when calling readgrid
    OPS.pspec_ret = 0
    OPS.age_ret = 0
    OPS.time_ret = 0
    OPS.scaledepo = 1.0
    OPS.scaleconc = 1.0
    OPS.decayconstant = 9e6
    OPS.date = None
    OPS.calcfoot = False
    OPS.verbose = False
    OPS.BinaryFile = False
    OPS.version = 'V8'
    ## add keyword overides and options to header
    OPS.update(kwargs)
    #H.update(OPS)


    ## set up the return dictionary (FLEXDATA updates fd, fd is returned)
    FLEXDATA = {}
    fd = Structure()
    fd.options = Structure()

    ## What direction is the run?
    unit = OPS.unit

    if H['loutstep'] > 0:
        forward = True
        if unit == 'time':
            ## default forward unit
            unit = 'conc'
            OPS.unit = unit
    else:
        forward = False

    ## What species to return?
    nspec_ret = OPS.nspec_ret
    if isinstance(nspec_ret, int):
        nspec_ret = [nspec_ret]
    assert iter(nspec_ret), "nspec_ret must be iterable."

    ## get times to return
    get_dates = None
    if OPS.time_ret is not None:
        get_dates = []
        time_ret = OPS.time_ret
        if isinstance(time_ret, int) == True:
            time_ret = [time_ret]

        if time_ret[0] < 0:
            if forward == False:
                ## get all dates for calculating footprint.
                time_ret = np.arange(len(H.available_dates))
            else:
                raise ValueError("Must enter a positive time_ret for forward runs")

        for t in time_ret:
            get_dates.append(H.available_dates[t])


    ## define what dates to extract if user has explicitly defined a 'date'
    if OPS.date != None:
        date = OPS.date
        if time_ret is not None:
            Warning("overwriting time_ret variable, date was requested")
        get_dates = []
        if not isinstance(date, list):
            date = date.strip().split(',')
        for d in date:
            try:
                get_dates.append(H.available_dates[H.available_dates.index(d)])
                time_ret = None
            except:
                _shout("Cannot find date: %s in H['available_dates']\n" % d)

    if get_dates is None:
        raise ValueError("Must provide either time_ret or date value.")
    else:
        ## assign grid dates for indexing fd
        fd.grid_dates = get_dates[:]

    print 'getting grid for: ', get_dates
    # Some predifinitions
    fail = 0
    # set filename prefix
    prefix = ['grid_conc_', 'grid_pptv_', \
              'grid_time_', 'footprint_', 'footprint_total', \
              'grid_conc_nest_', 'grid_pptv_nest_', \
              'grid_time_nest_', 'footprint_nest_', 'footprint_total_nest'
              ]

    units = ['conc', 'pptv', 'time', 'footprint', 'footprint_total']
    unit_i = units.index(unit)


    # Determine what module to read, try to use FortFlex, then dumpgrid, lastly pure Python
    # import the FortFlex / Fortran module
    try:
        if OPS.version == 'V6':
            from FortFlex import readgrid_v6 as readgrid
            from FortFlex import sumgrid
            useFortFlex = True
            print 'using FortFlex VERSION 6'
        else:
            print('Assumed V8 Flexpart')
            from FortFlex import readgrid, sumgrid
            useFortFlex = True
    except:
        # get the original module (no memory allocation)
        try:
            from nilu.pflexpart.FortFlex import readgrid, sumgrid
            useFortFlex = True
            print 'using nilu.pflexpart FortFlex'
        except:
            useFortFlex = False
            print('Cannot load FortFlex, reverting to BinaryFile.')
    if not useFortFlex:
        readgrid = _readgridBF
        OPS.BinaryFile = True


   # reserve output fields
    print 'Information from Header in readGrid'
    print H.numxgrid, H.numygrid, H.numzgrid, OPS.nspec_ret, OPS.pspec_ret, OPS.age_ret, len(get_dates), H.numpoint

    # -------------------------------------------------

    ## add the requests to the fd object to be returned
    OPS.unit = unit
    fd.options.update(OPS)

    #--------------------------------------------------
    # Loop over all times, given in field H['available_dates']
    #--------------------------------------------------


    for date_i in range(len(get_dates)):
        datestring = get_dates[date_i]
        print datestring
        for s in nspec_ret: #range(OPS.nspec_ret,OPS.nspec_ret+1):A

            FLEXDATA[(s, datestring)] = Structure()
            spec_fid = '_' + str(s + 1).zfill(3)

            if unit_i != 4:
                filename = os.path.join(H['pathname'], \
                            prefix[(unit_i) + (H.nested * 5)] + datestring + spec_fid)
                H.zdims = H.numzgrid[0]

            else:
                #grid total footprint
                print "Total footprint"
                filename = os.path.join(H['pathname'], \
                            prefix[(unit_i) + (H.nested * 5)] + spec_fid)
                H.zdims = 1

            if os.path.exists(filename):
                H.filename = filename
                #print 'reading: ' + filename
                if OPS.verbose:
                    print 'with values:'
                    inputvars = ['filename', 'numxgrid', 'numygrid',
                                 'zdims', 'numpoint', 'nageclass', \
                                 'scaledepo', 'scaleconc',
                                 'decayconstant', 'numpointspec']
                    for v in inputvars:
                        print v, " ==> ", H[v]


                if OPS.BinaryFile:
                    gridT, wetgrid, drygrid, itime = _readgridBF(H, filename)
                else:
                    ## Quick fix for Sabine's Ship releases, added nspec_int so that only one
                    ## field of the nspec dimension is actually read
                    if OPS.npspec_int is not False:
                        npspec_int = OPS.npspec_int
                        numpointspec = 1
                    else:
                        npspec_int = 0
                        numpointspec = H.numpointspec

                    gridT, wetgrid, drygrid, itime = readgrid(filename, \
                                                  H.numxgrid, H.numygrid,
                                                  H.zdims, numpointspec, H.nageclass, \
                                                  OPS.scaledepo, OPS.scaleconc, H.decayconstant, npspec_int)


                #print 'CDA'
                #print gridT
                #print 'CDA'
                
                if OPS.getwet:
                    return wetgrid
                if OPS.getdry:
                    return drygrid
                if forward:
                    zplot = gridT[:, :, :, :, 0]
                
                else:
                    zplot = gridT[:, :, :, :, 0]

                if OPS.calcfoot:

                    zplot = sumgrid(zplot, gridT, \
                                    H.area, H.Heightnn)


                ## get the total column and prep the grid
                if H.direction == 'forward':
                    #not trying to do anything here... must be done
                    #after retrieving the grid
                    #D = get_slabs(H,np.squeeze(zplot))
                    rel_i = H.available_dates.index(datestring)
                    D = zplot

                else:
                    D = zplot
                    rel_i = 'k'

                ## NOTE:
                ## If you're changing things here, you might want to change
                ## them in fill_backward as well, yes I know... something is
                ## poorly designed ;(

                FLEXDATA[(s, datestring)]['grid'] = D #zplot

                FLEXDATA[(s, datestring)]['itime'] = itime

                FLEXDATA[(s, datestring)]['shape'] = zplot.shape

                FLEXDATA[(s, datestring)]['max'] = zplot.max()

                FLEXDATA[(s, datestring)]['min'] = zplot.min()
                FLEXDATA[(s, datestring)]['timestamp'] = datetime.datetime.strptime(datestring, '%Y%m%d%H%M%S')
                FLEXDATA[(s, datestring)]['species'] = H['species'][s]
                FLEXDATA[(s, datestring)]['gridfile'] = filename
                FLEXDATA[(s, datestring)]['rel_i'] = rel_i
                FLEXDATA[(s, datestring)]['spec_i'] = s


            else:
                _shout('***ERROR: file %s not found! \n' % filename)
                fail = 1


        fd.set_with_dict(FLEXDATA)
        try:
            # just for testing, set the first available grid as a shortcut
            # this will be removed.
            qind = (nspec_ret[0], fd.grid_dates[0])
            fd.grid = fd[qind][fd[qind].keys()[0]].grid
        except:
            pass


    return fd


def _readgridBF(H, filename):
    """ Read grid using BinaryFile class"""
    # Utility functions
    skip = lambda n = 8 : f2.seek(n, 1)
    getbin = lambda dtype, n = 1 : f2.read(dtype, (n,))


    def getdump(n, fmt='f'):
        """ function to get the dump values for the sparse format """
        skip()
        #Dfmt=[fmt]*n
#        a=[struct.unpack(ft,f.read(struct.calcsize(ft))) for ft in Dfmt]
        a = f2.read(fmt, n)
#        dumplist=[a[j][0] for j in range(len(a))]
        #dumplist=[a[j] for j in range(len(a))]
        #dumplist = []
        #for j in range( len(a) ):
        #    dumplist.append( a[j] )
        return a #dumplist
        #return dumplist

    def key2var(D, key):
        cmd = "global %s; %s = D['%s'];" % (key, key, key)
        exec(cmd)

    def _dumpgrid(dmp_i, cnt_r, dmp_r, grd, k, nage, nx, ny):
        """ function to dump sparse elements into grid, fall back method if
        pflexcy.so module (cython) not available -- it is SLOW """
        conc = False
        if len(grd.shape) == 5:
            conc = True
        ii = 0
        fact = 1
        pos = 0
        #print 'cnt_r: ' + str(cnt_r)
        for ir in range(cnt_r):

            if conc:
                #print 'dmp_r: ' + str(dmp_r)
                #print 'length of dmp_r: ' + str(len(dmp_r))
                if dmp_r[ir] * fact > 0:
                    n = dmp_i[ii]
                    ii = ii + 1
                    fact = fact * -1.
                else:
                    n = n + 1

                kz = n / (H.numxgrid * H.numygrid)
                jy = (n - kz * H.numxgrid * H.numygrid) / H.numxgrid
                ix = n - H.numxgrid * H.numygrid * kz - H.numxgrid * jy
                grd[ix, jy, kz - 1, k, nage] = abs(dmp_r[ir])

#
#                print "n  ==> ix,jy,kz,k,nage"
#                print "%s ==> %s,%s,%s,%s,%s" % (n,ix,jy,kz,k,nage)
#                print grd.shape
#                print grd[0,0,0,0,0]


            else:
                if dmp_r[ir] * fact > 0:
                    n = dmp_i[ii]
                    ii = ii + 1
                    fact = fact * -1.
                else:
                    n = n + 1
                    #pos = pos + 1
                jy = n / H.numxgrid
                ix = n - H.numxgrid * jy
                grd[ix, jy, k, nage] = abs(dmp_r[ir])

        return grd #flipud(grd.transpose())

    ## Import pflexcy.so (cython compiled version of dumpgrid)
    try:
        from pflexcy import dumpdatagrid, dumpdepogrid
        #print 'using pflexcy'
    except:
        pass
        #print """WARNING: Using PURE Python to readgrid, execution will be slow.
        # Try compiling the FortFlex module or the pflexcy module
        # for your machine. For more information see the
        # pflexible/f2py_build directory or use cython with pflexcy.pyx
        #"""
        dumpdatagrid = _dumpgrid
        dumpdepogrid = _dumpgrid

    dat_cnt = 0
    nage = 1
    ##datagrid=np.zeros((numxgrid,numygrid,numzgrid[nspec-1],nspec,nageclass),np.float)
    wetgrid = np.zeros((H.numxgrid, H.numygrid, H.numpointspec, 1), np.float)
    drygrid = np.zeros((H.numxgrid, H.numygrid, H.numpointspec, 1), np.float)
    datagrid = np.zeros((H.numxgrid, H.numygrid, H.numzgrid[0], H.numpointspec, nage), np.float)
    #f = file(filename,'rb')
    #print filename
    f2 = BinaryFile(filename, order='fortran')
    #read data:
    skip(4)
    itime = getbin('i')


    for na in range(nage):

        for ks in range(H.numpointspec):
            #Read Wet Depostion
            skip()
            cnt_i = getbin('i')
            dmp_i = getdump(cnt_i, 'i')
            skip()
            cnt_r = getbin('i')
            dmp_r = getdump(cnt_r)
            if dmp_r.any():
                #print dmp_r, dmp_i
                wetgrid = dumpdepogrid(dmp_i, cnt_r, dmp_r, wetgrid, ks, na, H.numxgrid, H.numygrid)

            #Read Dry Deposition
            skip()
            cnt_i = getbin('i')
            dmp_i = getdump(cnt_i, 'i')
            skip()
            cnt_r = getbin('i')
            dmp_r = getdump(cnt_r)
            if dmp_r.any():
                #print dmp_r, dmp_i
                drygrid = dumpdepogrid(dmp_i, cnt_r, dmp_r, drygrid, ks, na, H.numxgrid, H.numygrid)

            #Read Concentrations
            skip()
            cnt_i = getbin('i')
            ### BEGIN DJM CHANGE ###
            ## ORIGINAL dmp_i = getdump(cnt_i, 'i')
            if cnt_i == 1:
                dmp_i = []
                dmp_i.append( getdump(cnt_i, 'i') )
            else:
                dmp_i = getdump(cnt_i, 'i')
            ### END DJM CHANGE ###
            skip()
            cnt_r = getbin('i')
            ### BEGIN DJM CHANGE ###
            ## ORIGINAL dmp_r = getdump(cnt_r)
            if cnt_r == 1:
                dmp_r = []
                dmp_r.append( getdump(cnt_r) )
            else:
                dmp_r = getdump(cnt_r)
            ### END DJM CHANGE ###

            #print len(dmp_r),len(dmp_i)
            #print cnt_r,cnt_i
            #print dmp_i
            #print type(dmp_i),type(cnt_r),type(dmp_r),type(datagrid),type(ks),type(na)
            #print type(H.numxgrid),type(H.numygrid)
            datagrid = dumpdatagrid(dmp_i, cnt_r, dmp_r, datagrid, ks, na, H.numxgrid, H.numygrid)

    #G[H['ibtime']].append(concgrid)
    f2.close()

    return  datagrid, wetgrid, drygrid, itime








########### HELPER FUNCTIONS ##########

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

def closest(num, numlist):
    """ returns the index of the *closest* value in a list """
    # check if we're using datetimes
    dates = False
    if isinstance(num, datetime.datetime):
        dates = True
    if dates:
        num = date2num(num)
        assert isinstance(numlist[0], datetime.datetime), \
               "num is date, numlist must be a list of dates"
        numlist = date2num(numlist)

    return (np.abs(numlist - num)).argmin()




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



def data_range(data, min='median'):
    """
    return a data range for flexpart data

    optional keyword min = ['median', 'mean', 'min']
    """
    dmax = np.nanmax(data)
    if np.isnan(dmax):
        dmax = 1e5

    if min == 'mean':
        dmin = np.mean(data[data.nonzero()])
    elif min == 'median':
        dmin = np.median(data[data.nonzero()])
    else:
        dmin = np.nanmin(data[data.nonzero()])

    if np.isnan(dmin):
        dmin = 1e-5

    return [dmin, dmax]

