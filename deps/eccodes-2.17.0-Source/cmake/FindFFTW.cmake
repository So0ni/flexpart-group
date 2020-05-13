# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

##############################################################################
#.rst:
#
# FindFFTW
# ========
#
# Find the FFTW library. ::
#
#   find_package(FFTW [REQUIRED] [QUIET]
#                [COMPONENTS [single] [double] [long_double] [quad]])
#
# By default, search for the double precision library ``fftw3``
#
# Components
# ----------
#
# If a different version or multiple versions of the library are required,
# these need to be specified as ``COMPONENTS``. Note that double must be given
# explicitly if any ``COMPONENTS`` are specified.
#
# The libraries corresponding to each of the ``COMPONENTS`` are:
#
# :single:      ``FFTW::fftw3f``
# :double:      ``FFTW::fftw3``
# :long_double: ``FFTW::fftw3l``
# :quad:        ``FFTW::fftw3q``
#
# Output variables
# ----------------
#
# The following CMake variables are set on completion:
#
# :FFTW_FOUND:            true if FFTW is found on the system
# :FFTW_LIBRARIES:        full paths to requested FFTW libraries
# :FFTW_INCLUDE_DIRS:     FFTW include directory
#
# Input variables
# ---------------
#
# The following CMake variables are checked by the function:
#
# :FFTW_USE_STATIC_LIBS:  if true, only static libraries are found
# :FFTW_ROOT:             if set, this path is exclusively searched
# :FFTW_DIR:              equivalent to FFTW_ROOT
# :FFTW_PATH:             equivalent to FFTW_ROOT
# :FFTW_LIBRARIES:        User overriden FFTW libraries
# :FFTW_INCLUDE_DIRS:     User overriden FFTW includes directories
#
##############################################################################

list( APPEND _possible_components double single long_double quad )

if( NOT ${CMAKE_FIND_PACKAGE_NAME}_FIND_COMPONENTS )
  ecbuild_debug( "FindFFTW: no components specified, looking for double precision (fftw3)" )
  set( ${CMAKE_FIND_PACKAGE_NAME}_FIND_COMPONENTS double )
else()
  ecbuild_debug( "FindFFTW: looking for components: ${${CMAKE_FIND_PACKAGE_NAME}_FIND_COMPONENTS}" )
endif()

set( ${CMAKE_FIND_PACKAGE_NAME}_double_LIBRARY_NAME fftw3 )
set( ${CMAKE_FIND_PACKAGE_NAME}_single_LIBRARY_NAME fftw3f )
set( ${CMAKE_FIND_PACKAGE_NAME}_long_double_LIBRARY_NAME fftw3l )
set( ${CMAKE_FIND_PACKAGE_NAME}_quad_LIBRARY_NAME fftw3q )

if( (NOT FFTW_ROOT) AND EXISTS $ENV{FFTW_ROOT} )
  set( FFTW_ROOT $ENV{FFTW_ROOT} )
endif()
if( (NOT FFTW_ROOT) AND FFTW_DIR )
  set( FFTW_ROOT ${FFTW_DIR} )
endif()
if( (NOT FFTW_ROOT) AND EXISTS $ENV{FFTW_DIR} )
  set( FFTW_ROOT $ENV{FFTW_DIR} )
endif()
if( (NOT FFTW_ROOT) AND FFTWDIR )
  set( FFTW_ROOT ${FFTWDIR} )
endif()
if( (NOT FFTW_ROOT) AND EXISTS $ENV{FFTWDIR} )
  set( FFTW_ROOT $ENV{FFTWDIR} )
endif()
if( (NOT FFTW_ROOT) AND FFTW_PATH )
  set( FFTW_ROOT ${FFTW_PATH} )
endif()
if( (NOT FFTW_ROOT) AND EXISTS $ENV{FFTW_PATH})
  set( FFTW_ROOT $ENV{FFTW_PATH} )
endif()

if( FFTW_ROOT ) # On cc[a|b|t] FFTW_DIR is set to the lib directory :(
  get_filename_component(_dirname ${FFTW_ROOT} NAME)
  if( _dirname MATCHES "lib" )
    set( FFTW_ROOT "${FFTW_ROOT}/.." )
  endif()
endif()

if( NOT FFTW_ROOT )
  # Check if we can use PkgConfig
  find_package(PkgConfig)

  #Determine from PKG
  if( PKG_CONFIG_FOUND AND NOT FFTW_ROOT )
    pkg_check_modules( PKG_FFTW QUIET "fftw3" )
  endif()
endif()

#Check whether to search static or dynamic libs
set( CMAKE_FIND_LIBRARY_SUFFIXES_SAV ${CMAKE_FIND_LIBRARY_SUFFIXES} )

if( ${FFTW_USE_STATIC_LIBS} )
  set( CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_STATIC_LIBRARY_SUFFIX} )
else()
  set( CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_SHARED_LIBRARY_SUFFIX} )
endif()


if( FFTW_ROOT )
  set( _default_paths NO_DEFAULT_PATH )
  set( _lib_paths ${FFTW_ROOT} )
  set( _include_paths ${FFTW_ROOT} )
else()
  set( _lib_paths ${PKG_FFTW_LIBRARY_DIRS} ${LIB_INSTALL_DIR} )
  set( _include_paths ${PKG_FFTW_INCLUDE_DIRS} ${INCLUDE_INSTALL_DIR} )
endif()

# find includes

if( NOT FFTW_INCLUDE_DIRS AND FFTW_INCLUDES ) # allow user to override with FFTW_INCLUDE_DIRS
  set( FFTW_INCLUDE_DIRS ${FFTW_INCLUDES} )
endif()

if( NOT FFTW_INCLUDE_DIRS ) # allow user to override with FFTW_INCLUDES

    find_path(
      FFTW_INCLUDE_DIRS
      NAMES "fftw3.h"
      PATHS ${_include_paths}
      PATH_SUFFIXES "include"
      ${_default_paths}
    )

    if( NOT FFTW_INCLUDE_DIRS )
      if( NOT ${CMAKE_FIND_PACKAGE_NAME}_FIND_QUIETLY OR ${CMAKE_FIND_PACKAGE_NAME}_FIND_REQUIRED )
        ecbuild_warn("FindFFTW: fftw include headers not found")
      endif()
    endif()

endif()

# find libs

foreach( _component ${${CMAKE_FIND_PACKAGE_NAME}_FIND_COMPONENTS} )

  find_library(
    ${CMAKE_FIND_PACKAGE_NAME}_${_component}_LIB
    NAMES ${${CMAKE_FIND_PACKAGE_NAME}_${_component}_LIBRARY_NAME}
    PATHS ${_lib_paths}
    PATH_SUFFIXES "lib" "lib64"
    ${_default_paths}
  )
  if( NOT ${CMAKE_FIND_PACKAGE_NAME}_${_component}_LIB )
    if( NOT ${CMAKE_FIND_PACKAGE_NAME}_FIND_QUIETLY OR ${CMAKE_FIND_PACKAGE_NAME}_FIND_REQUIRED )
        ecbuild_warn("FindFFTW: ${_component} precision required, but ${${CMAKE_FIND_PACKAGE_NAME}_${_component}_LIBRARY_NAME} was not found")
    endif()
    set( ${CMAKE_FIND_PACKAGE_NAME}_${_component}_FOUND FALSE )
  else()
    set( _target FFTW::${${CMAKE_FIND_PACKAGE_NAME}_${_component}_LIBRARY_NAME} )
    if (NOT TARGET ${_target} )
      add_library( ${_target} UNKNOWN IMPORTED)
      set_target_properties( ${_target} PROPERTIES
        IMPORTED_LOCATION "${${CMAKE_FIND_PACKAGE_NAME}_${_component}_LIB}"
        INTERFACE_INCLUDE_DIRECTORIES "${FFTW_INCLUDE_DIRS}")
    endif()
    set( ${CMAKE_FIND_PACKAGE_NAME}_${_component}_FOUND TRUE )
    list( APPEND FFTW_LIBRARIES ${${CMAKE_FIND_PACKAGE_NAME}_${_component}_LIB} )
  endif()

endforeach()

if( NOT ${CMAKE_FIND_PACKAGE_NAME}_FIND_QUIETLY AND FFTW_LIBRARIES )
  ecbuild_info( "FFTW targets:" )
  foreach( _component ${${CMAKE_FIND_PACKAGE_NAME}_FIND_COMPONENTS} )
    set( _target FFTW::${${CMAKE_FIND_PACKAGE_NAME}_${_component}_LIBRARY_NAME} )
    ecbuild_info("    ${_target} (${_component} precision)  [${${CMAKE_FIND_PACKAGE_NAME}_${_component}_LIB}]")
  endforeach()
endif()

set( CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_FIND_LIBRARY_SUFFIXES_SAV} )

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args( FFTW
                                   REQUIRED_VARS FFTW_INCLUDE_DIRS FFTW_LIBRARIES
                                   HANDLE_COMPONENTS )

set( FFTW_INCLUDES ${FFTW_INCLUDE_DIRS} ) # deprecated
set( FFTW_LIB ${${CMAKE_FIND_PACKAGE_NAME}_double_LIB} ) # deprecated

mark_as_advanced(FFTW_INCLUDE_DIRS FFTW_LIBRARIES FFTW_LIB FFTWF_LIB FFTWL_LIB FFTWQ_LIB)
