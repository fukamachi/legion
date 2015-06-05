#!/bin/sh

log () {
    echo "$ $1"
    echo `$1`
}

ROSWELL_TARBALL_PATH=$HOME/roswell.tar.gz
ROSWELL_DIR=$HOME/roswell

echo "Installing Roswell..."

curl --no-progress-bar --retry 10 -o $ROSWELL_TARBALL_PATH -L https://github.com/snmsts/roswell/archive/release.tar.gz
mkdir $ROSWELL_DIR
tar -C $ROSWELL_DIR --strip-components 1 -xf $ROSWELL_TARBALL_PATH
cd $ROSWELL_DIR
sh bootstrap
./configure
make
sudo make install

echo "Roswell has been installed."
log "ros --version"

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

ros -e '(format t "~%~a ~a up and running! (ASDF ~a)~%~%"
                (lisp-implementation-type)
                (lisp-implementation-version)
                (asdf:asdf-version))'

# Setup ASDF source regisry
ASDF_SR_CONF_DIR="$HOME/.config/common-lisp/source-registry.conf.d"
LOCAL_LISP_TREE="$HOME/lisp"

echo "(:tree \"$TRAVIS_BUILD_DIR/\")" > "$ASDF_SR_CONF_FILE"
echo "(:tree \"$LOCAL_LISP_TREE/\")" >> "$ASDF_SR_CONF_FILE"
