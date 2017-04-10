#!/usr/bin/env bash
#
# Create the mnist training files.

mkdir -p mnist
cd mnist/
wget 'http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz'
wget 'http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz'
wget 'http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz'
wget 'http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz'
gunzip *.gz

