#! /bin/sh
#

git checkout .

sed -i "s|ac_exeext=$|ac_exeext=.exe|" libiberty/configure
sed -i "s|ac_exeext=$|ac_exeext=.exe|" gcc/configure
sed -i "s|USE_COLLECT2 = .*|USE_COLLECT2 =|" gcc/Makefile.in
sed -i "s|/usr/lib/gcc/|/qdos-gcc/bin/|" gcc/gcc.c
patch -p1 < qdos/mingw/0001-gcc-config-m68k-qdos.h-alter-paths-for-mingw-port.patch

pushd qdos/mingw
make
popd

./configure --prefix=/qdos-gcc/ --host=i686-mingw32 --target=qdos

