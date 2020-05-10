#!/bin/sh

rm -rf Release/MCAmiga
mkdir Release/MCAmiga
rm -f Release/*.lha


cp LICENSE Release/MCAmiga
cp LICENSE.info Release/MCAmiga
cp Readme.txt Release/MCAmiga
cp Readme.txt.info Release/MCAmiga

# amiga
fpc4amiga.sh -B MCAmiga.pas
mkdir Release/MCAmiga/m68k-amiga
cp Release/MCAmiga.info Release/MCAmiga/m68k-amiga.info
cp MCAmiga Release/MCAmiga/m68k-amiga
cp MCAmiga.info Release/MCAmiga/m68k-amiga

# AROS
fpc4aros.sh -B MCAmiga.pas
mkdir Release/MCAmiga/i386-aros
cp Release/MCAmiga.info Release/MCAmiga/i386-aros.info
cp MCAmiga Release/MCAmiga/i386-aros
cp MCAmiga.info Release/MCAmiga/i386-aros

# AROS arm
fpc4arosarm.sh -B MCAmiga.pas
mkdir Release/MCAmiga/arm-aros
cp Release/MCAmiga.info Release/MCAmiga/arm-aros.info
cp MCAmiga Release/MCAmiga/arm-aros
cp MCAmiga.info Release/MCAmiga/arm-aros

# AROS64
fpc4aros64.sh -B MCAmiga.pas
mkdir Release/MCAmiga/x86_64-aros
cp Release/MCAmiga.info Release/MCAmiga/x86_64-aros.info
cp MCAmiga Release/MCAmiga/x86_64-aros
cp MCAmiga.info Release/MCAmiga/x86_64-aros

# MorphOS
fpc4mos.sh -B MCAmiga.pas
mkdir Release/MCAmiga/powerpc-morphos
cp Release/MCAmiga.info Release/MCAmiga/powerpc-morphos.info
cp MCAmiga Release/MCAmiga/powerpc-morphos
cp MCAmiga.info Release/MCAmiga/powerpc-morphos

# AmigaOS4
fpc4os4.sh -B MCAmiga.pas
mkdir Release/MCAmiga/powerpc-amiga
cp Release/MCAmiga.info Release/MCAmiga/powerpc-amiga.info
cp MCAmiga Release/MCAmiga/powerpc-amiga
cp MCAmiga.info Release/MCAmiga/powerpc-amiga

cd Release
lha co50 MCAmigaV02.lha MCAmiga MCAmiga.info
cd ..





