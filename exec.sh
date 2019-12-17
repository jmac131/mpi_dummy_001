#!/bin/bash

mpif90 mpi_mod.f90 dummy.f90 -o t 
echo "make done"
mpirun -np 3 ./t 

