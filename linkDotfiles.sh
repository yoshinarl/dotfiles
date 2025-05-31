#!/bin/sh

PWD=$(cd $(dirname $0);pwd)
ln -is $PWD/.emacs.d ~/.emacs.d
ln -is $PWD/.zshrc ~/.zshrc
ln -is $PWD/.gitconfig ~/.gitconfig
ln -is $PWD/.gitignore ~/.gitignore
ln -is $PWD/.inputrc ~/.inputrc
ln -is $PWD/.pryrc ~/.pryrc
ln -is $PWD/.Brewfile ~/.Brewfile
ln -is $PWD/.tigrc ~/.tigrc
ln -is $PWD/.wezterm.lua ~/.wezterm.lua
