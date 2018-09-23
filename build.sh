#! /bin/sh

mkdir build
cd src
ghc -threaded -outputdir ../build -o ../build/xmodm --make Main -debug -g -lpam

