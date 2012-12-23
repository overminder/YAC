#!/bin/bash

echo "$0: Running python test..."
py.test
cd haskell

echo "$0: Running haskell test..."
make test

echo "$0: Done"
