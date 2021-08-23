curl -O https://elpa.gnu.org/devel/hyperbole.tar
tar -xf hyperbole.tar
rm hyperbole.tar
ln -s hyperbole* hyperbole
cd hyperbole
make bin
