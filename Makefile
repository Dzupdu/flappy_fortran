all:
	gcc -c src/get_key.c
	gfortran -c src/mod_flappy.f90 src/main.f90
	gfortran -o flappy *.o
	mkdir -p build
	mv *.o build
	mv *.mod build

clean:
	rm -r build
	rm flappy
