#! /bin/sh
#

git checkout .

sed -i "s|ac_exeext=$|ac_exeext=.exe|" libiberty/configure
sed -i "s|ac_exeext=$|ac_exeext=.exe|" gcc/configure
sed -i "s|USE_COLLECT2 = .*|USE_COLLECT2 =|" gcc/Makefile.in
patch -p1 < qdos/mingw/0001-gcc-config-m68k-qdos.h-alter-paths-for-mingw-port.patch

CFLAGS=-Wno-implicit-int CPP=i686-w64-mingw32-cpp CC=i686-w64-mingw32-gcc ./configure --prefix=/qdos-gcc/ --host=i686-mingw32 --target=qdos --exec-prefix=/

