kdlib = ../libkdtree.a

CC = gcc
CFLAGS = -std=c89 -pedantic -Wall -g -I..
LDFLAGS = $(kdlib) -lm

FC = gfortran
FFLAGS = -pedantic -Wall -I..

% : %.f90
	$(FC) $(FFLAGS) $^ -o $@

#.f90:
#	$(FC) $(FFLAGS) -o $@ $<

.PHONY: all
all: test test2 ftest ftest2

test: test.c $(LDFLAGS) 
test2: test2.c $(LDFLAGS)
ftest: ftest.f90 $(LDFLAGS)
ftest2: ftest2.f90 $(LDFLAGS)

.PHONY: clean
clean:
	rm -f test test2 ftest ftest2
