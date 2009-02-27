cp dot-emacs.el ../.emacs
emacs -batch -f batch-byte-compile ../.emacs
#(cd lib/cedet && emacs -Q -l cedet-build.el -f cedet-build)
#find . -path '*/.svn' -prune -o -type d -exec emacs -batch -f batch-byte-compile '{}'/*.el ';'
echo Remainder of byte compilation is up to you