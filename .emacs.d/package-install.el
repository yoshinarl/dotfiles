(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(package-refresh-contents)

(defvar my/favorite-packages
  '(
    linum
    auto-complete
    flycheck
    helm
    rbenv
    enh-ruby-mode
    ruby-electric
    ruby-block
    rubocop
    projectile
    projectile-rails
    ag
    js2-mode
    haml-mode
    hiwin
    minibuf-isearch session
    git-gutter-fringe+
    emmet-mode
    elscreen
    web-mode
    ))

;; my/favorite-packagesからインストールしていないパッケージをインストール
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))
