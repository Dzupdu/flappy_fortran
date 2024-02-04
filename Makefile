all:
	gcc -c get_key.c
	gfortran -c mod_flappy.f90 main.f90
	gfortran -o flappy *.o

clean:
	rm *.o
	rm *.mod
	rm flappy
