#! /bin/sh
#

sed -i "s|ac_exeext=|ac_exeext=.exe|" libiberty/configure
sed -i "s|ac_exeext=|ac_exeext=.exe|" gcc/configure
sed -i "s|USE_COLLECT2 = .*|USE_COLLECT2 =|" gcc/Makefile.in

FLAGS=-Wno-implicit-int ./configure --host=i686-mingw32 --target=qdos --disable-use_collect2

