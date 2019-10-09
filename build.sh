#!/bin/sh
mkdir -p bin
cd deps/Sparse
./build.sh
cp bin/sparse.dll ../../bin/
cd ../..
cat build_list.txt | xargs -d '\n' fsharpc --nologo -r bin/sparse.dll --out:bin/qm.exe
