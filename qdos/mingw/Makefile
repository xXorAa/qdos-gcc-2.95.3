
CC = i686-w64-mingw32-g++

ALL: as.exe qdos-ar.exe

as.exe : as.cpp
	$(CC) -static -o $@ $<
	-cp as.exe ../../gcc/

qdos-ar.exe : qdos-ar.cpp
	$(CC) -static -o $@ $<

