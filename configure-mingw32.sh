#! /bin/sh

CC=i686-w64-mingw32-gcc CPP=i686-w64-mingw32-cpp CFLAGS=-Wno-implicit-int ac_exeext=.exe ./configure --target=qdos --host=i686-mingw32 --prefix=/qdos-gcc/

