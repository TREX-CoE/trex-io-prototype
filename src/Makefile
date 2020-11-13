include ../make.config

TARGET= ../lib/libtrexio.so

all: $(TARGET)

../lib/libezfio.a: ../trex.config
	$(MAKE) -C ..

$(TARGET): trexio.o trexio_functions.o ../lib/libezfio.a
	$(FC) -shared -o ../lib/libtrexio.so $^ ../lib/libezfio.a -lz

trexio.f90 trexio_functions.f90: ../trex.config generate.py
	./generate.py $^

clean:
	rm -f trexio_functions.f90 trexio.f90 trexio.mod trexio_functions.o trexio.o $(TARGET)

