include ../make.config

EXE=fortran_write fortran_read

all: $(EXE)
	@echo '----------------------------------------------------------'
	@echo "Before running the examples you need to do:"
	@echo 'export LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$(TREXIO_ROOT)/lib'
	@echo '----------------------------------------------------------'


../lib/libtrexio.so ../src/trexio.f90:
	make -C ../src

trexio.f90: ../src/trexio.f90
	cp $^ $@

fortran_read: fortran_read.f90 trexio.o
	$(FC) -o $@ $^ -L../lib -ltrexio -lz

fortran_write: fortran_write.f90 trexio.o
	$(FC) -o $@ $^ -L../lib -ltrexio -lz


clean:
	rm -rf trexio.o trexio.f90 trexio.mod $(EXE) trex_file
