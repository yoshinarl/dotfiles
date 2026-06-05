#!/bin/sh

PWD=$(cd $(dirname $0);pwd)
ln -is $PWD/.emacs.d ~/.emacs.d
ln -is $PWD/.zshrc ~/.zshrc
ln -is $PWD/.zprofile ~/.zprofile
ln -is $PWD/.gitconfig ~/.gitconfig
ln -is $PWD/.gitignore ~/.gitignore
ln -is $PWD/.inputrc ~/.inputrc
ln -is $PWD/.pryrc ~/.pryrc
ln -is $PWD/.Brewfile ~/.Brewfile
ln -is $PWD/.tigrc ~/.tigrc
ln -is $PWD/.wezterm.lua ~/.wezterm.lua
ln -is $PWD/.tmux.conf ~/.tmux.conf
ln -is $PWD/.npmrc ~/.npmrc

mkdir -p ~/.config
ln -is $PWD/.config/ghostty ~/.config/ghostty

mkdir -p ~/.config/mise
ln -is $PWD/.config/mise/config.toml ~/.config/mise/config.toml

mkdir -p ~/.local/bin
ln -is $PWD/ruby-lsp-op ~/.local/bin/ruby-lsp-op
