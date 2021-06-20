TARBALL=hyperbole-8.0.0pre0.20210605.220551

curl -O https://elpa.gnu.org/devel/$TARBALL.tar
tar -xf $TARBALL.tar
ln -s $TARBALL hyperbole
cd hyperbole
make bin
