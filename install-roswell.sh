#!/bin/sh

echo "Installing Roswell..."
git clone -b release https://github.com/snmsts/roswell ~/roswell
cd ~/roswell
sh bootstrap
./configure
make
sudo make install

ros --version

case "$LISP" in
    ccl)
        echo "Installing Clozure CL..."
        ros install ccl-bin
        ;;
    sbcl|*)
        echo "Installing SBCL..."
        ros install sbcl-bin
        ;;
esac

ros run -- --version
