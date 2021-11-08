#! /bin/sh -v

#------------
#GNU compiler
#------------
FC = gfortran
GNU10 = #-fallow-argument-mismatch -fallow-invalid-boz
FFLAGS = -ffree-line-length-none ${GNU10} #-fbacktrace -ggdb -fcheck=bounds,do,mem,pointer -ffpe-trap=invalid,zero,overflow

#--------------
#INTEL compiler
#--------------
#FC = ifort
#FFLAGS = -mcmodel medium # needed for intel error message "failed to convert GOTPCREL relocation"

LIBS = -L$(NETCDF)/lib -lnetcdff -lnetcdf
INCS = -I$(NETCDF)/include

OBJS = \
       netcdf_mod.o \
       goes_abi_converter.o

all: goes_abi_convert

goes_abi_convert: ${OBJS}
	${FC} -o goes_abi_converter.x ${FFLAGS} ${OBJS} ${LIBS}

goes_abi_converter.o : goes_abi_converter.f90 netcdf_mod.o
netcdf_mod.o : netcdf_mod.f90

.SUFFIXES : .F90 .f90 .o

.F90.o :
	${FC} ${FFLAGS} ${INCS} -c $<

.f90.o :
	${FC} ${FFLAGS} ${INCS} -c $<

%.o: %.mod

clean:
	rm -f *.o *.mod *.exe *.x
