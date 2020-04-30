#!/bin/sh

case `uname` in
Darwin)
    EMACS="/Applications/Emacs.app/Contents/MacOS/emacs"
    ;;
Linux|FreeBSD|SunOS)
    EMACS="emacs"
    ;;
*)
    Echo "Byte Compile not run, unknown system!"
    ;;
esac

echo "Removing old *.elc files from ${HOME}/.emacs.d/elisp"
find ${HOME}/.emacs.d/elisp -type f -name "*.elc" -exec rm {} \;

#for i in ${HOME}/.emacs.d/elisp/mc ${HOME}/.emacs.d/elisp/nav  ${HOME}/.emacs.d/elisp/w3m  ${HOME}/.emacs.d/elisp/yasnippet ${HOME}/.emacs.d/elisp/iedit ${HOME}/.emacs.d/elisp
#do
    echo "Byte Compiling ${i}/*.el files"
#    cd  ${i}
    ${EMACS} -Q -L . -batch -f batch-byte-compile *.el > /dev/null 2>&1
#    ${EMACS} -Q -L . -batch -f batch-byte-compile ${HOME}/.emacs.d/emacs-init.el
#done
