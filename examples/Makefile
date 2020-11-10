include ../make.config

all: fortran_write fortran_write

../lib/libtrexio.so ../src/trexio.f90:
	make -C ../src

trexio.f90: ../src/trexio.f90
	cp $^ $@

fortran_write: fortran_write.f90 trexio.o
	$(FC) -o $@ $^ -L../lib -ltrexio -lz


clean:
	rm -rf trexio.o trexio.f90 trexio.mod fortran_write trex_file
