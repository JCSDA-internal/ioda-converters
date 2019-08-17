#! /bin/sh -v

 GSILIB=/scratch3/BMC/wrfruc/mhu/rapcode/git_local/work/GSI_201707b_5c5800c/lib
# Define the fortran compiler and options
 FC  =  ifort
 CC  =  icc
 FLAGS=  -g -traceback -ftz -O3
#
 INCLD=   -I../baselib
 LIBS =   -L${GSILIB} -lbufr_i4r8

#
OBJSBASE = ../baselib/kinds.o ../baselib/module_time.o

OBJS = module_obs_base.o module_obs_conv_pt.o \
       module_prepbufr.o get_obs.o

all: get_obs

get_obs: ${OBJS}
	${FC} -o get_obs.exe ${FLAGS} ${OBJSBASE} ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

clean:
	rm -f *.o *.mod *.exe
