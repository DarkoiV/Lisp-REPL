#! /bin/bash

ghc -O1 -hidir build -odir build Main.hs && ./Main
