echo Configuring GCC for Windows NT on %2
rem This batch file assumes a unix-type "sed" program

if %2.==alpha. echo #include "alpha/xm-alpha.h" >config.h
if %2.==alpha. echo #include "winnt/xm-winnt.h" >>config.h
if %2.==alpha. echo #include "alpha/xm-winnt.h" >>config.h
if not %2.==alpha.  echo #include "%2/xm-winnt.h" >config.h
copy config.h hconfig.h
copy config.h tconfig.h

if %2.==alpha. echo #define TARGET_CPU_DEFAULT 64 >tm.h
if %2.==alpha. echo #include "alpha/alpha.h" >>tm.h
if %2.==alpha. echo #include "alpha/win-nt.h" >>tm.h
if not %2.==alpha. echo #include "%2/win-nt.h" >tm.h

rem This batch file assumes a unix-type "sed" program

echo # Makefile generated by "config-nt.bat"> Makefile
echo all.nt: cpp.exe cc1.exe cc1obj.exe xgcc.exe ld.exe stmp-headers libgcc.lib stmp-float_h specs stamp-objlist>> Makefile
sed -f config/%2/config-nt.sed -f config/winnt/config-nt.sed Makefile.in >> Makefile

set LANG=

echo # >specs.h
echo # >options.h

if not exist cp\make-lang.in goto no_cp
if exist cp\lang-specs.h echo #include "cp/lang-specs.h">>specs.h
if exist cp\lang-options.h echo #include "cp/lang-options.h">>options.h
sed -f config/%2/config-nt.sed -f config/winnt/config-nt.sed cp\make-lang.in >> Makefile
sed -f config/%2/config-nt.sed -f config/winnt/config-nt.sed cp\makefile.in > cp\Makefile
set LANG=%LANG% c++.#
:no_cp

if not exist ada\make-lang.in goto no_ada
if exist ada\lang-specs.h echo #include "ada/lang-specs.h">>specs.h
if exist ada\lang-options.h echo #include "ada/lang-options.h">>options.h
sed -f config/%2/config-nt.sed -f config/winnt/config-nt.sed ada\make-lang.in >> Makefile
sed -f config/%2/config-nt.sed -f config/winnt/config-nt.sed ada\makefile.in > ada\Makefile
set LANG=%LANG% ada.#
:no_ada

if not exist f\make-lang.in goto no_f
if exist f\lang-specs.h echo #include "f/lang-specs.h">>specs.h
if exist f\lang-options.h echo #include "f/lang-options.h">>options.h
sed -f config/%2/config-nt.sed -f config/winnt/config-nt.sed f\make-lang.in >> Makefile
sed -f config/%2/config-nt.sed -f config/winnt/config-nt.sed f\makefile.in > f\Makefile
set LANG=%LANG% f.#
:no_f

echo lang.mostlyclean: %LANG% | sed "s/#/mostlyclean/g" >> Makefile
echo lang.clean: %LANG% | sed "s/#/clean/g" >> Makefile
echo lang.distclean: %LANG% | sed "s/#/distclean/g" >> Makefile
echo lang.realclean: %LANG% | sed "s/#/realclean/g" >> Makefile

echo #define MULTILIB_SELECT ". ;" > multilib.h1
copy multilib.h1 multilib.h
del multilib.h1
