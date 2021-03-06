include make.config

all: lib/libezfio.a Python/ezfio.py OCaml/ezfio.ml Bash/ezfio.sh
	$(MAKE) -C src
	$(MAKE) -C examples

EZFIO/lib/libezfio.a: trex.config
	[ ! -L EZFIO/config/trex.cfg ] && ln -s $$PWD/trex.config EZFIO/config/trex.cfg || :
	make -C EZFIO

lib/libezfio.a: EZFIO/lib/libezfio.a
	cp $^ $@

Python/ezfio.py: EZFIO/Python/ezfio.py
	cp $^ $@

OCaml/ezfio.ml: EZFIO/Ocaml/ezfio.ml
	cp $^ $@

Bash/ezfio.sh: EZFIO/Bash/ezfio.sh
	cp $^ $@

doc:
	./make_doc.sh

clean:
	$(MAKE) -C src clean
	$(MAKE) -C EZFIO clean
	$(MAKE) -C examples clean
	$(RM) -r lib/*
