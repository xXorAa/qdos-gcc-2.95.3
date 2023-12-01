
CC = i686-w64-mingw32-g++

ALL: as.exe qdos-ar.exe

as.exe : as.cpp
	$(CC) -static -s -o $@ $<

qdos-ar.exe : qdos-ar.cpp
	$(CC) -static -s -o $@ $<

clean:
	-rm *.exe

