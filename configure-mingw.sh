#! /bin/sh
#

git checkout .

sed -i "s|ac_exeext=$|ac_exeext=.exe|" libiberty/configure
sed -i "s|ac_exeext=$|ac_exeext=.exe|" gcc/configure
sed -i "s|USE_COLLECT2 = .*|USE_COLLECT2 =|" gcc/Makefile.in
patch -p1 < qdos/mingw/0001-gcc-config-m68k-qdos.h-alter-paths-for-mingw-port.patch

pushd qdos/mingw
make
popd

CFLAGS="-Wno-implicit-int" ./configure --prefix=/qdos-gcc/ --host=i686-mingw32 --target=qdos --exec-prefix=/

