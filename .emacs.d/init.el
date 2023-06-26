;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My init.el.

;;; Code:

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/{{pkg}}/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

;;  ---------------
;; |   共通設定    |
;;  ---------------
(leaf default-directory
  :setq ((default-directory . "~/")
         (command-line-default-directory . "~/")))

;; ロードパス
(leaf load-path
  :config
  (setq load-path (cons "~/.emacs.d/elisp" load-path)))

;; 環境変数パス
(leaf env-path
  :config
  (dolist (dir
           (list "/sbin" "/usr/sbin" "/bin" "/usr/bin" "/usr/local/bin"
                 (expand-file-name "~/bin")
                 (expand-file-name "~/.emacs.d/bin")))
    (when (and
           (file-exists-p dir)
           (not (member dir exec-path)))
      (setenv "PATH"
              (concat dir ":"
                      (getenv "PATH")))
      (setq exec-path (append
                       (list dir)
                       exec-path)))))

;; homebrew でインストールしたツールを使う
;; (leaf homebrew-path
;;   :config
;;   (Add-to-list 'exec-path
;;                (expand-file-name "/usr/local/bin")))

(leaf general
  :bind (
         ;; Returnでオートインデント
         ("" . newline-and-indent)
         ;; C-hをBSに
         ("" . backward-delete-char)
         ;; C-x C-gでM-x goto-line
         ("" . goto-line))
  :hook (
         ;; 保存時に行末の空白を削除
         (before-save-hook . delete-trailing-whitespace))
  :setq (
         ;; バックアップファイルを作らない
         (backup-inhibited . t)
         ;; 終了時にオートセーブファイルを消す
         (delete-auto-save-files . t)
         ;; optionキーをMetaキーとして利用
         (mac-option-modifier quote meta)
         ;; 1行ずつスクロールする
         (scroll-step . 1)
         ;; ウィンドウ分割時も右端で折り返す
         (truncate-partial-width-windows)
         ;; emacs終了時に確認する
         (confirm-kill-emacs quote y-or-n-p)
         ;; 最終行に必ず一行挿入する
         (require-final-newline . t)
         ;; mini buffer に改行コードを表示する
         (eol-mnemonic-dos . "(CRLF)")
         (eol-mnemonic-mac . "(CR)")
         (eol-mnemonic-unix . "(LF)"))
  :bind(
        ;; C-z を無効化
        (""))
  :config
  ;; 対応する括弧を光らせる
  (show-paren-mode 1)
  ;; 選択範囲をハイライト表示
  (transient-mark-mode t)
  ;; ファイルに変更があったら自動で再読み込みする
  (global-auto-revert-mode 1)
  ;; タイトルバーにファイルのフルパス表示
  (setq frame-title-format (format "%%b - %%f")))

;; 音を出さない
(leaf ignore-ring-bell
  :setq ((ring-bell-function quote ignore)))

;; メニューバー非表示
(leaf hide-menu-bar
  :config
  (tool-bar-mode 0)
  (menu-bar-mode 0))

;; 分割した画面間をShift+矢印で移動
(leaf windmove
  :setq ((windmove-wrap-around . t))
  :config
  (windmove-default-keybindings))

;; 日本語設定
(leaf japanese
  :config
  (set-language-environment 'Japanese)
  (set-default-coding-systems 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))

(leaf tab
  :setq (
         ;; タブ幅の倍数を設定
         (tab-stop-list quote
                        (2 4 6 8 10 12 14 16 18 20)))
  :setq-default (
                 ;; タブ幅を2に設定
                 (tab-width . 2)
                 ;; タブを使わずスペースにする
                 (indent-tabs-mode)))

;; 起動時のサイズ、表示位置、フォントを設定
(leaf default-frame
  :setq ((default-frame-alist . initial-frame-alist))
  :config
  (setq initial-frame-alist (append
                             (list
                              '(width . 175)
                              '(height . 110)
                              '(top . 0)
                              '(left . 0)
                              '(right . 0))
                             initial-frame-alist)))

;; 括弧の自動補完
(leaf skelton-pair
  :bind (("(" . skeleton-pair-insert-maybe)
         ("{" . skeleton-pair-insert-maybe)
         ("[" . skeleton-pair-insert-maybe)
         ("\"" . skeleton-pair-insert-maybe)
         ("'" . skeleton-pair-insert-maybe))
  :setq ((skeleton-pair . 1)))

;; フォント変更->Ricty
(leaf font
  :config
  (let* ((size 13)
         (asciifont "Ricty")
         (jpfont "Ricty")
         (h (* size 11))
         (fontspec (font-spec :family asciifont))
         (jp-fontspec (font-spec :family jpfont)))
    (set-face-attribute 'default nil :family asciifont :height h)
    (set-fontset-font nil 'japanese-jisx0213\.2004-1 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
    (set-fontset-font nil
                      '(128 . 591)
                      fontspec)
    (set-fontset-font nil
                      '(880 . 1023)
                      fontspec)))

;; フレームの3分割
;;  C-x@で縦3分割。C-x#で横3分割
(leaf split-3-frame
  :preface
  (defun split-window-vertically-n (num_wins)
    (interactive "p")
    (if (= num_wins 2)
        (split-window-vertically)
      (progn
        (split-window-vertically
         (-
          (window-height)
          (/
           (window-height)
           num_wins)))
        (split-window-vertically-n
         (- num_wins 1)))))
  (defun split-window-horizontally-n (num_wins)
    (interactive "p")
    (if (= num_wins 2)
        (split-window-horizontally)
      (progn
        (split-window-horizontally
         (-
          (window-width)
          (/
           (window-width)
           num_wins)))
        (split-window-horizontally-n
         (- num_wins 1)))))
  :config
  (global-set-key "@"
                  '(lambda nil
                     (interactive)
                     (split-window-vertically-n 3)))
  (global-set-key "#"
                  '(lambda nil
                     (interactive)
                     (split-window-horizontally-n 3))))

;; リージョン内の行数と文字数をモードラインに表示する
(leaf count-lines-and-chars
  :preface
  (defun count-lines-and-chars nil
    (if mark-active
        (format "%d lines,%d chars "
                (count-lines
                 (region-beginning)
                 (region-end))
                (-
                 (region-end)
                 (region-beginning)))
      ;; エコーエリアがちらつくのでコメントアウト
      ;; (count-lines-region
      ;;  (region-beginning)
      ;;  (region-end))
      "")))

;; スクロールを加速させない
(leaf scroll-acceleration
  :config
  (global-set-key
   [wheel-up]
   '(lambda nil
      ""
      (interactive)
      (scroll-down 1)))
  (global-set-key
   [wheel-down]
   '(lambda nil
      ""
      (interactive)
      (scroll-up 1)))
  (global-set-key
   [double-wheel-up]
   '(lambda nil
      ""
      (interactive)
      (scroll-down 1)))
  (global-set-key
   [double-wheel-down]
   '(lambda nil
      ""
      (interactive)
      (scroll-up 1)))
  (global-set-key
   [triple-wheel-up]
   '(lambda nil
      ""
      (interactive)
      (scroll-down 2)))
  (global-set-key
   [triple-wheel-down]
   '(lambda nil
      ""
      (interactive)
      (scroll-up 2))))

;; 各種色設定
(leaf color
  :when window-system
  :config
  ;; 文字の色を設定します。
  (add-to-list 'default-frame-alist
               '(foreground-color . "#EEEEEE"))
  ;; 背景色を設定します。
  (add-to-list 'default-frame-alist
               '(background-color . "#333333"))
  ;; 背景透明
  (set-frame-parameter nil 'alpha 95)
  ;; カーソルの色を設定します。
  (add-to-list 'default-frame-alist
               '(cursor-color . "SlateBlue2"))
  ;; マウスポインタの色を設定します。
  ;; (add-to-list 'default-frame-alist
  ;;              '(mouse-color . "SlateBlue2"))
  ;; モードラインの文字の色を設定します。
  ;; (set-face-foreground 'modeline "white")
  ;; モードラインの背景色を設定します。
  ;; (set-face-background 'modeline "MediumPurple2")
  ;; 選択中のリージョンの色を設定します。
  ;; (set-face-background 'region "LightSteelBlue1")
  ;; モードライン（アクティブでないバッファ）の文字色を設定します。
  ;; (set-face-foreground 'mode-line-inactive "gray30")
  ;; モードライン（アクティブでないバッファ）の背景色を設定します。
  ;; (set-face-background 'mode-line-inactive "gray85")
  )

;; C-a で空白を除く行頭へ移動
;; インデント文字を飛ばした行頭に戻る。
;; ただし、ポイントから行頭までの間にインデント文字しかない場合は、行頭に戻る。
(leaf beggining-of-indented-line
  :preface
  (defun beggining-of-indented-line (current-point)
    (interactive "d")
    (if (string-match "^[ 	]+$"
                      (save-excursion
                        (buffer-substring-no-properties
                         (progn
                           (beginning-of-line)
                           (point))

                         current-point)))
        (beginning-of-line)
      (back-to-indentation)))
  :bind (("" . beggining-of-indented-line)))

;; 行の移動を実装
;; Shift-Alt-p: 上へ移動
;; Shift-Alt-n: 下へ移動
(leaf move-line
  :preface
  (defun move-line-down nil
    (interactive)
    (let ((col (current-column)))
      (save-excursion
        (forward-line)
        (transpose-lines 1))
      (forward-line)))

  (defun move-line-up nil
    (interactive)
    (let ((col (current-column)))
      (save-excursion
        (forward-line)
        (transpose-lines -1))
      (move-to-column col)
      (previous-line)))

  :bind (("M-P" . move-line-up)
         ("M-N" . move-line-down)))

;; whitespace
;; 空白関係を可視化させる
(leaf whitespace-color
  :require whitespace
  :setq ((whitespace-style quote
                           (
                            face                   ; faceで可視化
                            trailing             ; 行末
                            tabs                   ; タブ
                            empty               ; 先頭/末尾の空行
                            space-mark     ; 表示のマッピング
                            tab-mark
                            ))
         (whitespace-display-mappings quote
                                      ((tab-mark 9
                                                 [187 9]
                                                 [92 9])))
         (my/bg-color . "#333333"))
  :config
  (global-whitespace-mode 1)
  (set-face-attribute 'whitespace-trailing nil :background my/bg-color :foreground "DeepPink" :underline t)
  (set-face-attribute 'whitespace-tab nil :background my/bg-color :foreground "gray36" :underline t)
  (set-face-attribute 'whitespace-space nil :background my/bg-color :foreground "GreenYellow" :weight 'bold)
  (set-face-attribute 'whitespace-empty nil :background my/bg-color))

;; M-backspace で kill-ring に追加しない
(leaf M-BS-kill-ring
  :preface
  (defun delete-word (arg)
    "Delete characters forward until encountering the end of a word.\nWith argument, do this that many times."
    (interactive "p")
    (delete-region
     (point)
     (progn
       (forward-word arg)
       (point))))

  (defun backward-delete-word (arg)
    "Delete characters backward until encountering the end of a word.\nWith argument, do this that many times."
    (interactive "p")
    (delete-word
     (- arg)))

  (defun align-regexp-repeated (start stop regexp)
    "Like align-regexp, but repeated for multiple columns. See http://www.emacswiki.org/emacs/AlignCommands"
    (interactive "r\nsAlign regexp: ")
    (let ((spacing 1)
          (old-buffer-size (buffer-size)))
      ;; If our align regexp is just spaces, then we don't need any
      ;; extra spacing.
      (when (string-match regexp " ")
        (setq spacing 0))
      (align-regexp start stop
                    ;; add space at beginning of regexp
                    (concat "\\([[:space:]]*\\)" regexp)
                    1 spacing t)
      ;; modify stop because align-regexp will add/remove characters
      (align-regexp start
                    (+ stop
                       (-
                        (buffer-size)
                        old-buffer-size))
                    ;; add space at end of regexp
                    (concat regexp "\\([[:space:]]*\\)")
                    1 spacing t)))

  :config
  (global-set-key
   (read-kbd-macro "<M-DEL>")
   'backward-delete-word))

;;  ---------------
;; |    package    |
;;  ---------------

;; 行番号を表示
(require 'linum)
(global-linum-mode t)
(setq linum-format "%5d")

;; auto-complete-mode
(defun load-auto-complete ()
  (require 'auto-complete)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
  (add-to-list 'ac-modes 'text-mode)
  (add-to-list 'ac-modes 'enh-ruby-mode)
  (setq ac-use-menu-map t)
  (setq ac-use-fuzzy t))

;; flycheck
(require 'flycheck)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'enh-ruby-mode-hook 'flycheck-mode)
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))))

;; helm
(require 'helm-config)
(helm-mode 1)
(define-key global-map (kbd "C-x C-h") 'helm-mini)

;; rbenv-mode
(require 'rbenv)
(global-rbenv-mode)
(setq rbenv-installation-dir "/usr/local/Cellar/rbenv")

;; enh-ruby-mode
(require 'enh-ruby-mode)
(autoload 'enh-ruby-mode "enh-ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

;; "encoding を自動挿入しない"
(defun remove-enh-magic-comment ()
  (remove-hook 'before-save-hook 'enh-ruby-mode-set-encoding t))
(add-hook 'enh-ruby-mode-hook 'remove-enh-magic-comment)
(setq ruby-insert-encoding-magic-comment nil)
(add-hook 'enh-ruby-mode-hook
  '(lambda ()
    (setq ruby-indent-level tab-width)
    (setq enh-ruby-deep-indent-paren nil)
    (define-key ruby-mode-map [return] 'ruby-reindent-then-newline-and-indent))
    (load-auto-complete)
    (setenv "LC_ALL" "ja_JP.UTF-8"))

;; ruby-electric
(require 'ruby-electric)
(add-hook 'enh-ruby-mode-hook '(lambda () (ruby-electric-mode t)))
(setq ruby-electric-expand-delimiters-list nil)

;; ruby-block
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; rubocop
;; (require 'rubocop)
;; (add-hook 'enh-ruby-mode-hook 'rubocop-mode)

;; js2-mode
(require 'js2-mode)
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . js2-jsx-mode))
(add-hook 'js2-jsx-mode-hook 'emmet-mode)
(setq-default js2-basic-offset 2)

;; typescript-mode
(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-hook 'typescript-mode-hook
  '(lambda ()
     (setq typescript-indent-level 2)))

;; haml-mode
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(add-hook 'haml-mode-hook
  (lambda ()
    (define-key haml-mode-map "\C-m" 'newline-and-indent)))

;; C−x C-f C-rで開くファイルを履歴からインクリメンタルサーチする。
(require 'minibuf-isearch)
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; git-gutter-fringe+
(require 'git-gutter-fringe+)
(global-git-gutter+-mode t)

;; hiwin-mode
;; 非アクティブのバッファの色を変える
;; (require 'hiwin)
;; (hiwin-activate)
;; (set-face-background 'hiwin-face "#666666")

;; emmet-mode
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; マークアップ言語全部で使う
(setq emmet-expand-jsx-className? t) ;; jsx で使う
(setq emmet-expand-tsx-className? t) ;; tsx で使う
(eval-after-load "emmet-mode"
  '(define-key emmet-mode-keymap (kbd "C-j") nil)) ;; C-j は newline のままにしておく
(keyboard-translate ?\C-i ?\H-i) ;;C-i と Tabの被りを回避
(define-key emmet-mode-keymap (kbd "H-i") 'emmet-expand-line) ;; C-i で展開
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.
(setq emmet-move-cursor-between-quotes t) ;; default nil

;; elscreen
(require 'elscreen)
;;; プレフィクスキーはC-z
(setq elscreen-prefix-key (kbd "C-z"))
(elscreen-start)
;;; タブの先頭に[X]を表示しない
(setq elscreen-tab-display-kill-screen nil)
;;; header-lineの先頭に[<->]を表示しない
(setq elscreen-tab-display-control nil)
;;; バッファ名・モード名からタブに表示させる内容を決定する(デフォルト設定)
(setq elscreen-buffer-to-nickname-alist
      '(("^dired-mode$" .
         (lambda ()
           (format "Dired(%s)" dired-directory)))
        ("^Info-mode$" .
         (lambda ()
           (format "Info(%s)" (file-name-nondirectory Info-current-file))))
        ("^mew-draft-mode$" .
         (lambda ()
           (format "Mew(%s)" (buffer-name (current-buffer)))))
        ("^mew-" . "Mew")
        ("^irchat-" . "IRChat")
        ("^liece-" . "Liece")
        ("^lookup-" . "Lookup")))
(setq elscreen-mode-to-nickname-alist
      '(("[Ss]hell" . "shell")
        ("compilation" . "compile")
        ("-telnet" . "telnet")
        ("dict" . "OnlineDict")
        ("*WL:Message*" . "Wanderlust")))

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[jt]sx\\'" . web-mode))
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook
          '(lambda ()
             (setq web-mode-attr-indent-offset nil)
             (setq web-mode-markup-indent-offset 2)
             (setq web-mode-css-indent-offset 2)
             (setq web-mode-code-indent-offset 2)
             (setq web-mode-sql-indent-offset 2)
             (setq indent-tabs-mode nil)
             (setq tab-width 2)))

;; rufo.el
(require 'rufo)
(add-hook 'enh-ruby-mode-hook 'rufo-minor-mode)
(setq rufo-minor-mode-use-bundler t)

;; protobuf-mode
(require 'protobuf-mode)
(setq auto-mode-alist (append '(("\\.proto$" . protobuf-mode)) auto-mode-alist))
(defconst my-protobuf-style
  '((c-basic-offset . 4)
    (indent-tabs-mode . nil)))
(add-hook 'protobuf-mode-hook
  (lambda () (c-add-style "my-style" my-protobuf-style t)))

;; lsp-mode
(require 'lsp-mode)
(add-hook 'web-mode-hook #'lsp)

;; exec-path-from-shell
(setq exec-path-from-shell-shell-name "zsh")
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-envs '("PATH" "GOPATH" "LANG"))

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(blackout el-get hydra leaf-keywords leaf)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
