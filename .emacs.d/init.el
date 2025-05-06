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

;; バックアップファイルを作らない
(setq make-backup-files nil)

;; タイトルバーにファイルのフルパス表示
(setq frame-title-format (format "%%b - %%f"))

(leaf general
  :bind (
         ;; Returnでオートインデント
         ("" . newline-and-indent)
         ;; C-hをBSに
         ("" . backward-delete-char)
         ;; C-x C-gでM-x goto-line
         ("" . goto-line)
         ;; C-z を無効化
         (""))
  :hook (
         ;; 保存時に行末の空白を削除
         (before-save-hook . delete-trailing-whitespace))
  :setq (
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
  :config
  ;; 対応する括弧を光らせる
  (show-paren-mode 1)
  ;; 選択範囲をハイライト表示
  (transient-mark-mode t)
  ;; ファイルに変更があったら自動で再読み込みする
  (global-auto-revert-mode 1))

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

;; 括弧の自動補完
;; leaf-convert-region するとなぜかうごかない
(setq skeleton-pair t)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "`") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)

;; フォント変更->HackGen Console NF
(leaf font
  :config
  (let* ((size 12)
         (asciifont "HackGen Console NF")
         (jpfont "HackGen Console NF")
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
(leaf default-frame-alist
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
(leaf linum
  :doc "display line numbers in the left margin"
  :tag "builtin"
  :added "2023-06-26"
  :require linum
  :setq ((linum-format . "%5d"))
  :config
  (global-linum-mode t))


;; flycheck
(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "emacs-25.1" "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11"
  :tag "tools" "languages" "convenience" "emacs>=25.1"
  :url "http://www.flycheck.org"
  :added "2023-06-28"
  :emacs>= 25.1
  :ensure t
  :hook ((after-init-hook . global-flycheck-mode)
         (enh-ruby-mode-hook . flycheck-mode))
  :require flycheck
  :setq ((flycheck-check-syntax-automatically quote
                                              (mode-enabled save)))
  :config
  (with-eval-after-load 'flycheck
    (custom-set-variables
     '(flycheck-disabled-checkers
       '(javascript-jshint javascript-jscs)))))

;; vertico
(leaf vertico
  :doc "VERTical Interactive COmpletion"
  :req "emacs-27.1" "compat-29.1.4.0"
  :tag "completion" "matching" "files" "convenience" "emacs>=27.1"
  :url "https://github.com/minad/vertico"
  :added "2023-07-01"
  :emacs>= 27.1
  :ensure t
  :init
  (vertico-mode)
  :setq ((vertico-cycle . t))
  :bind ((vertico-map
          ("C-r" . vertico-previous))
         (vertico-map
          ("C-s" . vertico-next))))

;; savehist
;; Persist history over Emacs restarts. Vertico sorts by history position.
(leaf savehist
  :doc "Save minibuffer history"
  :tag "builtin"
  :added "2023-07-01"
  :init
  (savehist-mode))

;; orderless
(leaf orderless
  :doc "Completion style for matching regexps in any order"
  :req "emacs-26.1"
  :tag "extensions" "emacs>=26.1"
  :url "https://github.com/oantolin/orderless"
  :added "2023-07-01"
  :emacs>= 26.1
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(leaf marginalia
  :doc "Enrich existing commands with completion annotations"
  :req "emacs-27.1" "compat-29.1.4.0"
  :tag "completion" "matching" "help" "docs" "emacs>=27.1"
  :url "https://github.com/minad/marginalia"
  :added "2023-07-03"
  :emacs>= 27.1
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  ;;:bind (:map minibuffer-local-map
  ;;            ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be actived in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; consult
(leaf consult
  :doc "Consulting completing-read"
  :req "emacs-27.1" "compat-29.1.4.1"
  :tag "completion" "files" "matching" "emacs>=27.1"
  :url "https://github.com/minad/consult"
  :added "2023-07-01"
  :emacs>= 27.1
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer))
  )

;; recentf
(leaf recentf
  :doc "setup a menu of recently opened files"
  :tag "builtin"
  :added "2023-07-01"
  :ensure t
  :init
  (recentf-mode)
  :custom ((recentf-max-saved-items . 2000)
           (recentf-auto-cleanup    . 'never)))

;; enh-ruby-mode
(leaf enh-ruby-mode
  :doc "Major mode for editing Ruby files"
  :req "emacs-25.1"
  :tag "ruby" "elisp" "languages" "emacs>=25.1"
  :url "https://github.com/zenspider/Enhanced-Ruby-Mode"
  :added "2023-06-28"
  :emacs>= 25.1
  :ensure t
  :mode (("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
  :interpreter (("ruby" . enh-ruby-mode))
  :require enh-ruby-mode)

;; "encoding を自動挿入しない"
(leaf remove-enh-magic-comment
  :preface
  (defun remove-enh-magic-comment nil
    (remove-hook 'before-save-hook 'enh-ruby-mode-set-encoding t))

  :hook ((enh-ruby-mode-hook . remove-enh-magic-comment))
  :setq ((ruby-insert-encoding-magic-comment))
  :config
  (add-hook 'enh-ruby-mode-hook
            '(lambda nil
               (setq ruby-indent-level tab-width)
               (setq enh-ruby-deep-indent-paren nil)
               (define-key ruby-mode-map
                 [return]
                 'ruby-reindent-then-newline-and-indent))
            (load-auto-complete)
            (setenv "LC_ALL" "ja_JP.UTF-8")))

;; ruby-electric
(leaf ruby-electric
  :doc "Minor mode for electrically editing ruby code"
  :tag "ruby" "languages"
  :url "https://github.com/ruby/elisp-ruby-electric"
  :added "2023-06-28"
  :ensure t
  :require ruby-electric
  :setq ((ruby-electric-expand-delimiters-list))
  :config
  (add-hook 'enh-ruby-mode-hook
            '(lambda nil
               (ruby-electric-mode t))))

;; ruby-block
(leaf ruby-clock
  :added "2023-06-28"
  :el-get h3poteto/ruby-block.el
  :require ruby-block
  :setq ((ruby-block-highlight-toggle . t))
  :config
  (ruby-block-mode t))

;; rubocop
;; (require 'rubocop)
;; (add-hook 'enh-ruby-mode-hook 'rubocop-mode)

;; C−x C-f C-rで開くファイルを履歴からインクリメンタルサーチする。
(leaf minibuf-isearch
  :doc "incremental search on minibuffer history"
  :tag "incremental search" "history" "minibuffer"
  :added "2023-06-28"
  :ensure t
  :require minibuf-isearch)

(leaf session
  :doc "use variables, registers and buffer places across sessions"
  :tag "tools" "data" "desktop" "session management" "session"
  :url "http://emacs-session.sourceforge.net/"
  :added "2023-06-28"
  :ensure t
  :hook ((after-init-hook . session-initialize))
  :require session)

;; git-gutter-fringe+
(leaf git-gutter-fringe+
  :doc "Fringe version of git-gutter+.el"
  :req "git-gutter+-0.1" "fringe-helper-1.0.1"
  :url "https://github.com/nonsequitur/git-gutter-fringe-plus"
  :added "2023-06-28"
  :ensure t
  :require git-gutter-fringe+
  :config
  (global-git-gutter+-mode t))

;; elscreen
(leaf elscreen
  :doc "Emacs window session manager"
  :req "emacs-24"
  :tag "convenience" "window" "emacs>=24"
  :url "https://github.com/knu/elscreen"
  :added "2023-06-28"
  :emacs>= 24
  :ensure t
  :require elscreen
  :setq (
         ;; タブの先頭に[X]を表示しない
         (elscreen-tab-display-kill-screen)
         ;; header-lineの先頭に[<->]を表示しない
         (elscreen-tab-display-control)
         ;; バッファ名・モード名からタブに表示させる内容を決定する(デフォルト設定)
         (elscreen-buffer-to-nickname-alist quote
                                            (("^dired-mode$" lambda nil
                                              (format "Dired(%s)" dired-directory))
                                             ("^Info-mode$" lambda nil
                                              (format "Info(%s)"
                                                      (file-name-nondirectory Info-current-file)))
                                             ("^mew-draft-mode$" lambda nil
                                              (format "Mew(%s)"
                                                      (buffer-name
                                                       (current-buffer))))
                                             ("^mew-" . "Mew")
                                             ("^irchat-" . "IRChat")
                                             ("^liece-" . "Liece")
                                             ("^lookup-" . "Lookup")))
         (elscreen-mode-to-nickname-alist quote
                                          (("[Ss]hell" . "shell")
                                           ("compilation" . "compile")
                                           ("-telnet" . "telnet")
                                           ("dict" . "OnlineDict")
                                           ("*WL:Message*" . "Wanderlust"))))
  :config
  ;; プレフィクスキーはC-z
  (setq elscreen-prefix-key (kbd "C-z"))
  (elscreen-start))

;; protobuf-mode
(leaf protobuf-mode
  :doc "major mode for editing protocol buffers."
  :tag "languages" "protobuf" "google"
  :added "2023-06-28"
  :ensure t
  :require protobuf-mode
  :config
  (setq auto-mode-alist (append
                         '(("\\.proto$" . protobuf-mode))
                         auto-mode-alist))
  (defconst my-protobuf-style
    '((c-basic-offset . 4)
      (indent-tabs-mode)))
  (add-hook 'protobuf-mode-hook
            (lambda nil
              (c-add-style "my-style" my-protobuf-style t))))

;; markdown-mode
(leaf markdown-mode :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (setopt markdown-command '("pandoc" "--from=markdown" "--to=html5"))
  (setopt markdown-fontify-code-blocks-natively t)
  (setopt markdown-header-scaling t)
  (setopt markdown-indent-on-enter 'indent-and-new-item)
  (setopt markdown-list-indent-width 2))

;; exec-path-from-shell
(leaf exec-path-from-shell
  :doc "Get environment variables such as $PATH from the shell"
  :req "emacs-24.1" "cl-lib-0.6"
  :tag "environment" "unix" "emacs>=24.1"
  :url "https://github.com/purcell/exec-path-from-shell"
  :added "2023-06-28"
  :emacs>= 24.1
  :ensure t
  :setq ((exec-path-from-shell-shell-name . "zsh"))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH" "GOPATH" "LANG")))

(leaf corfu
  :doc "COmpletion in Region FUnction"
  :req "emacs-27.1" "compat-29.1.4.4"
  :tag "text" "completion" "matching" "convenience" "abbrev" "emacs>=27.1"
  :url "https://github.com/minad/corfu"
  :added "2025-04-05"
  :emacs>= 27.1
  :ensure t
  :custom ((corfu-auto . t)
           (corfu-preview-current . t)
           (corfu-cycle . t)
           (corfu-quit-no-match . nil)
           (corfu-preselect . 'prompt)
           (tab-always-indent . 'complete))
  :init
  (global-corfu-mode)
  )

(leaf codeium
  :tag "out-of-MELPA"
  :added "2025-05-02"
  :el-get "Exafunction/codeium.el"
  :require t)

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
 '(package-selected-packages '(blackout el-get hydra leaf-keywords leaf))
 '(session-use-package t nil (session)))
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
