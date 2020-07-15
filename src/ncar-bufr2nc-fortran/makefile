#! /bin/sh -v

MPIFC = mpif90
MPILD = mpif90
FC = gfortran
LD = gfortran

LIBS = -L$(NETCDF)/lib -lnetcdff -lnetcdf -L/${HOME}/extlib -lbufr
INCS = -I$(NETCDF)/include
FFLAGS  = -fbacktrace -ggdb -fcheck=bounds,do,mem,pointer -ffpe-trap=invalid,zero,overflow
LDFLAGS = $(FFLAGS) $(LIBS)

OBJS = \
       constants.o \
       define_types_mod.o \
       gnssro_mod.o \
       kinds.o \
       main.o \
       mpisetup.o \
       ncio_mod.o \
       netcdf_mod.o \
       prepbufr_mod.o \
       radiance_mod.o \
       ufo_variables_mod.o \
       utils_mod.o

all: bufr2nc

bufr2nc: ${OBJS}
	${MPIFC} -o bufr2nc.exe ${FFLAGS} ${OBJS} ${LIBS}

constants.o : constants.f90 kinds.o
kinds.o : kinds.f90
define_types_mod.o : define_types_mod.f90 kinds.o ufo_variables_mod.o
gnssro_mod.o : gnssro_mod.f90
main.o : main.f90 define_types_mod.f90 prepbufr_mod.o ncio_mod.o mpisetup.o radiance_mod.o gnssro_mod.o
mpisetup.o : mpisetup.f90 kinds.o
ncio_mod.o : ncio_mod.f90 kinds.o prepbufr_mod.o netcdf_mod.o ufo_variables_mod.o define_types_mod.o
netcdf_mod.o : netcdf_mod.f90 mpisetup.o
prepbufr_mod.o : prepbufr_mod.f90 kinds.o ufo_variables_mod.o utils_mod.o define_types_mod.o
radiance_mod.o : radiance_mod.f90 kinds.o define_types_mod.o ufo_variables_mod.o
ufo_variables_mod.o : ufo_variables_mod.F90
utils_mod.o : utils_mod.f90

.SUFFIXES : .F90 .f90 .o

.F90.o :
	${MPIFC} ${FFLAGS} ${INCS} -c $<

.f90.o :
	${MPIFC} ${FFLAGS} ${INCS} -c $<

%.o: %.mod

clean:
	rm -f *.o *.mod *.exe
