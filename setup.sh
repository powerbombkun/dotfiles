#!/bin/sh
for f in .emacs .emacs.d .zshrc .gdbinit .vimperatorrc .vimperator .gitconfig
do
    rm -rf ~/$f | ln -si $PWD/$f ~/$f
done

