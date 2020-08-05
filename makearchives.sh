#!/bin/sh -e

rm -rf Release/MCAmiga
mkdir Release/MCAmiga
rm -f Release/*.lha
rm -rf lib
mkdir lib


cp LICENSE Release/MCAmiga
cp LICENSE.info Release/MCAmiga
cp Readme.txt Release/MCAmiga
cp Readme.txt.info Release/MCAmiga
mkdir Release/MCAmiga/C
cp mc Release/MCAmiga/C/mc
cp mcview Release/MCAmiga/C/mcview
cp mcdiff Release/MCAmiga/C/mcdiff


rm -f MCAmiga
# AROS
fpc4aros.sh -B -dRELEASE -FUlib MCAmiga.pas
mkdir Release/MCAmiga/i386-aros
cp Release/MCAmiga.info Release/MCAmiga/i386-aros.info
cp MCAmiga Release/MCAmiga/i386-aros
cp MCAmiga.info Release/MCAmiga/i386-aros

rm -f MCAmiga
# AROS arm
fpc4arosarm.sh -B -dRELEASE -FUlib MCAmiga.pas
mkdir Release/MCAmiga/arm-aros
cp Release/MCAmiga.info Release/MCAmiga/arm-aros.info
cp MCAmiga Release/MCAmiga/arm-aros
cp MCAmiga.info Release/MCAmiga/arm-aros

rm -f MCAmiga
# AROS64
fpc4aros64.sh -B -dRELEASE -FUlib MCAmiga.pas
mkdir Release/MCAmiga/x86_64-aros
cp Release/MCAmiga.info Release/MCAmiga/x86_64-aros.info
cp MCAmiga Release/MCAmiga/x86_64-aros
cp MCAmiga.info Release/MCAmiga/x86_64-aros

rm -f MCAmiga
# MorphOS
fpc4mos.sh -B -dRELEASE -FUlib MCAmiga.pas
mkdir Release/MCAmiga/powerpc-morphos
cp Release/MCAmiga.info Release/MCAmiga/powerpc-morphos.info
cp MCAmiga Release/MCAmiga/powerpc-morphos
cp MCAmiga.info Release/MCAmiga/powerpc-morphos/MCAmiga.info

rm -f MCAmiga
# AmigaOS4
fpc4os4.sh -B -dRELEASE -FUlib MCAmiga.pas
mkdir Release/MCAmiga/powerpc-amiga
cp Release/MCAmiga.info Release/MCAmiga/powerpc-amiga.info
cp MCAmiga Release/MCAmiga/powerpc-amiga
cp MCAmiga.info Release/MCAmiga/powerpc-amiga/MCAmiga.info

rm -f MCAmiga
# amiga000
fpc4amiga000.sh -B -dRELEASE -FUlib MCAmiga.pas
mkdir Release/MCAmiga/m68000-amiga
cp Release/MCAmiga.info Release/MCAmiga/m68000-amiga.info
cp MCAmiga Release/MCAmiga/m68000-amiga
cp MCAmiga.info Release/MCAmiga/m68000-amiga

rm -f MCAmiga
# amiga
fpc4amiga.sh -B -dRELEASE -FUlib MCAmiga.pas
mkdir Release/MCAmiga/m68020-amiga
cp Release/MCAmiga.info Release/MCAmiga/m68020-amiga.info
cp MCAmiga Release/MCAmiga/m68020-amiga
cp MCAmiga.info Release/MCAmiga/m68020-amiga


cd Release
lha co50 MCAmigaVxx.lha MCAmiga MCAmiga.info
cd ..
