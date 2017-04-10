#!/usr/bin/env bash

# Create the mnist training files.
mkdir -p mnist
cd mnist/
wget 'http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz'
wget 'http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz'
wget 'http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz'
wget 'http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz'
gunzip *.gz

# Create the sample weight files.
wget 'https://github.com/oreilly-japan/deep-learning-from-scratch/blob/master/ch03/sample_weight.pkl?raw=true' -O sample_weight.pkl

mkdir -p ch3-sample-weight/

python3 - <<END

import pickle
import numpy
import yaml

network_file = 'sample_weight.pkl'
print("read: %(network_file)s" % locals())
with open(network_file, 'rb') as f:
    network = pickle.load(f)

for key in network.keys():
    dst = "%(key)s.yml" % locals()
    print("write: %(dst)s" % locals())
    with open(dst, 'w') as f:
        f.write(yaml.dump(network[key].tolist()))

END
