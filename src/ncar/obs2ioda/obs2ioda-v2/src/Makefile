#! /bin/sh -v

#------------
#GNU compiler
#------------
#FC = gfortran
#BUFR_LIB = -L/glade/u/home/hclin/extlib -lbufr
#GNU10 = #-fallow-argument-mismatch -fallow-invalid-boz
#FFLAGS = -ffree-line-length-none ${GNU10} #-fbacktrace -ggdb -fcheck=bounds,do,mem,pointer -ffpe-trap=invalid,zero,overflow

#--------------
#INTEL compiler
#--------------
FC = ifort
BUFR_LIB = -L/glade/u/home/hclin/extlib/intel -lbufr
FFLAGS = -mcmodel medium # needed for intel error message "failed to convert GOTPCREL relocation"

LIBS = -L$(NETCDF)/lib -lnetcdff -lnetcdf ${BUFR_LIB}
INCS = -I$(NETCDF)/include

OBJS = \
       define_mod.o \
       gnssro_mod.o \
       hsd.o \
       satwnd_mod.o \
       kinds.o \
       main.o \
       ncio_mod.o \
       netcdf_mod.o \
       prepbufr_mod.o \
       radiance_mod.o \
       ufo_variables_mod.o \
       utils_mod.o

all: obs2ioda

obs2ioda: ${OBJS}
	${FC} -o obs2ioda-v2.x ${FFLAGS} ${OBJS} ${LIBS}

kinds.o : kinds.f90
define_mod.o : define_mod.f90 kinds.o ufo_variables_mod.o
gnssro_mod.o : gnssro_mod.f90
hsd.o : hsd.f90 kinds.o define_mod.o ufo_variables_mod.o ncio_mod.o utils_mod.o
main.o : main.f90 define_mod.f90 prepbufr_mod.o ncio_mod.o radiance_mod.o gnssro_mod.o hsd.o satwnd_mod.o
ncio_mod.o : ncio_mod.f90 kinds.o prepbufr_mod.o netcdf_mod.o ufo_variables_mod.o define_mod.o
netcdf_mod.o : netcdf_mod.f90
prepbufr_mod.o : prepbufr_mod.f90 kinds.o ufo_variables_mod.o utils_mod.o define_mod.o
radiance_mod.o : radiance_mod.f90 kinds.o define_mod.o ufo_variables_mod.o utils_mod.o
satwnd_mod.o : satwnd_mod.f90 kinds.o define_mod.o ufo_variables_mod.o
ufo_variables_mod.o : ufo_variables_mod.F90
utils_mod.o : utils_mod.f90

.SUFFIXES : .F90 .f90 .o

.F90.o :
	${FC} ${FFLAGS} ${INCS} -c $<

.f90.o :
	${FC} ${FFLAGS} ${INCS} -c $<

%.o: %.mod

clean:
	rm -f *.o *.mod *.exe *.x
