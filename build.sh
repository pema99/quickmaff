#!/bin/sh
cat build_list.txt | xargs -d '\n' fsharpc --out:bin/qm.exe
