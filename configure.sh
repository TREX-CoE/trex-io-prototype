cat << EOF > make.config
.EXPORT_ALL_VARIABLES:

TREXIO_ROOT=$PWD
IRPF90=\$(TREXIO_ROOT)/irpf90/bin/irpf90

FC=gfortran -g
FFLAGS=-fPIC  -fcheck=all -Waliasing -Wampersand -Wconversion -Wsurprising -Wintrinsics-std -Wno-tabs -Wintrinsic-shadow -Wline-truncation -Wreal-q-constant -Wuninitialized  -fbacktrace -ffpe-trap=zero,overflow,underflow -finit-real=nan

%.o: %.f90
	\$(FC) \$(FCFLAGS) -c \$*.f90 -o \$*.o

.DEFAULT_GOAL := all
EOF

echo "Done. Now edit  make.config  to finish the configuration"
