#! /bin/sh
#

sed -i "s|ac_exeext=$|ac_exeext=.exe|" libiberty/configure
sed -i "s|ac_exeext=$|ac_exeext=.exe|" gcc/configure
sed -i "s|USE_COLLECT2 = .*|USE_COLLECT2 =|" gcc/Makefile.in

CFLAGS=-Wno-implicit-int CPP=i686-w64-mingw32-cpp CC=i686-w64-mingw32-gcc ./configure --prefix=/qdos/ --host=i686-mingw32 --target=qdos

