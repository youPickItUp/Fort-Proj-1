FF= gfortran 
FFLAGS= -ffree-form -std=f2008 -fimplicit-none -Wall -pedantic
FMODULES= ./modules/gau_jor_elim.f90 ./modules/coefficients_gen.f90 ./modules/measure_error.f90 


.PHONY: all
all: avg_errs


avg_errs: main.f90
	$(FF) -o avg_errs $(FMODULES) main.f90 $(FFLAGS)


.PHONY: clean
clean:
	rm -f *.mod avg_errs

.PHONY: example
example:
	./avg_errs 100 10
