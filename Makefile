.PHONY: all run clean

all: ether.py

run: ether.py
	python3 ether-gui.py

ether.py: ether.fut functions/*.fut lib
	futhark-pyopencl --library ether.fut

lib: futhark.pkg
	futhark-pkg sync

clean:
	rm -rf ether.py __pycache__
