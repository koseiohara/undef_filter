EXE = exe
OBJS = NaNchecker.o fileio.o globals.o namelist.o undef_filter.o main.o

FC = ifort
FLAGS = -O0 -warn all -convert little_endian -assume byterecl -traceback

%.o : %.f90
	${FC} -c $< ${FLAGS}

all : ${EXE}

${EXE} : ${OBJS}
	${FC} -o $@ $^


.PHONY : clean re

clean :
	rm -fv *.o *.mod ${EXE}

re : clean all
	
