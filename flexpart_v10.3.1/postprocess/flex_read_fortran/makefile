SHELL = /bin/bash

FC       = gfortran
#FFLAGS   = -g -fcheck=all -Wall -fbacktrace  -O2 -m64 -mcmodel=medium -fconvert=little-endian -frecord-marker=4 


all: flex_read_compare2 printheader printgrid

%.o: %.f90
	+$(FC) -c $<		
flex_read_compare2:readheader.o readgrid.o juldate.o caldate.o flex_read_compare2.o 
	$(FC) readheader.o readgrid.o juldate.o caldate.o flex_read_compare2.o -o flex_read_compare2
printheader:readheader1090.o juldate.o caldate.o readheader_stdout90.o 
	$(FC) readheader1090.o juldate.o caldate.o readheader_stdout90.o -o printheader
printgrid:readheader.o readgrid.o juldate.o caldate.o printgrid.o 
	$(FC) readheader.o readgrid.o juldate.o caldate.o printgrid.o -o printgrid

clean:
	        rm *.o

reset: 
	rm  *.o printgrid printheader flex_read_compare2

output=../../output/
test: 
	./printgrid $(output) conc
	./printheader $(output)

