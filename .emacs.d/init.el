;;  ---------------
;; |   共通設定    |
;;  ---------------

;; ロードパス
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; 対応する括弧を光らせる
(show-paren-mode 1)

;; バックアップファイルを作らない
(setq backup-inhibited t)

;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;; optionキーをMetaキーとして利用
(setq mac-option-modifier 'meta)

;; Returnでオートインデント
(global-set-key "\C-m" 'newline-and-indent)

;; C-hをBSに
(global-set-key "\C-h" 'backward-delete-char)

;; 音を出さない
(setq ring-bell-function 'ignore)
(put 'upcase-region 'disabled nil)

;; 日本語設定
(set-language-environment 'Japanese)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; メニューバー非表示
(tool-bar-mode 0)
(menu-bar-mode 0)

;; 選択範囲をハイライト表示
(transient-mark-mode t)

;; パッケージ管理を利用
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


;;  -------------------
;; |   各モード設定    |
;;  -------------------

;; タブ幅を4に設定
;;(setq-default tab-width 4)
;;タブ幅の倍数を設定
;;(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))

;; 行番号を表示
;;(require 'linum)
;;(global-linum-mode t)
;;(setq linum-format "%5d")

;; スクロールを加速させない
;; (global-set-key [wheel-up] '(lambda () "" (interactive) (scroll-down 1)))
;; (global-set-key [wheel-down] '(lambda () "" (interactive) (scroll-up 1)))
;; (global-set-key [double-wheel-up] '(lambda () "" (interactive) (scroll-down 1)))
;; (global-set-key [double-wheel-down] '(lambda () "" (interactive) (scroll-up 1)))
;; (global-set-key [triple-wheel-up] '(lambda () "" (interactive) (scroll-down 2)))
;; (global-set-key [triple-wheel-down] '(lambda () "" (interactive) (scroll-up 2)))

;; フォント設定
;;(if (eq window-system 'mac) (require 'carbon-font))
;;(fixed-width-set-fontset "hirakaku_w3" 14)
;;(setq fixed-width-rescale nil)

;; タブ表示
;; APELを入れないと動かない？emacs23での動作が確認されていないからかもしれない。要調査
;; ElScreen
;;(require 'elscreen)
;;(if window-system
;;    (define-key elscreen-map "\C-z" 'iconify-or-deiconify-frame)
;;  (define-key elscreen-map "\C-z" 'suspend-emacs))

;; 起動時のサイズ、表示位置、フォントを設定
;;(setq initial-frame-alist
;;	  (append (list
;;			   '(width . 156)
;;			   '(height . 72)
;;			   '(top . 0)
;;			   '(left . 0)
;;			   '(right . 0)
;;			   )
;;			  initial-frame-alist))
;;(setq default-frame-alist initial-frame-alist)

;; 各種色設定
;;(if window-system (progn

;; 文字の色を設定します。
;;  (add-to-list 'default-frame-alist '(foreground-color . "gray10"))
;;  (add-to-list 'default-frame-alist '(foreground-color . "#EEEEEE"))
  ;; 背景色を設定します。
  ;;(add-to-list 'default-frame-alist '(background-color . "#D0D0D0"))
;;  (add-to-list 'default-frame-alist '(background-color . "#333333"))
  ;; 背景透明
;;  (set-frame-parameter nil 'alpha 95)
  ;; カーソルの色を設定します。
;;  (add-to-list 'default-frame-alist '(cursor-color . "SlateBlue2"))
  ;; マウスポインタの色を設定します。
;;  (add-to-list 'default-frame-alist '(mouse-color . "SlateBlue2"))
  ;; モードラインの文字の色を設定します。
;;  (set-face-foreground 'modeline "white")
  ;; モードラインの背景色を設定します。
;;  (set-face-background 'modeline "MediumPurple2")
  ;; 選択中のリージョンの色を設定します。
;;  (set-face-background 'region "LightSteelBlue1")
  ;; モードライン（アクティブでないバッファ）の文字色を設定します。
;;  (set-face-foreground 'mode-line-inactive "gray30")
  ;; モードライン（アクティブでないバッファ）の背景色を設定します。
;;  (set-face-background 'mode-line-inactive "gray85")
;;))

;; コンパイルモード設定
;; 下段ウィンドウサイズ
;;(setq compilation-window-height 30)

;;; 分割した画面間をShift+矢印で移動
;;(setq windmove-wrap-around t)
;;(windmove-default-keybindings)

;; フレームの3分割
;; C-xで縦3分割。C-x#で横3分割
;;(defun split-window-vertically-n (num_wins)
;;  (interactive "p")
;;  (if (= num_wins 2)
;;      (split-window-vertically)
;;    (progn
;;      (split-window-vertically
;;       (- (window-height) (/ (window-height) num_wins)))
;;      (split-window-vertically-n (- num_wins 1)))))
;;(defun split-window-horizontally-n (num_wins)
;;  (interactive "p")
;;  (if (= num_wins 2)
;;      (split-window-horizontally)
;;    (progn
;;      (split-window-horizontally
;;       (- (window-width) (/ (window-width) num_wins)))
;;      (split-window-horizontally-n (- num_wins 1)))))
;;(global-set-key "\C-x@" '(lambda ()
;;                           (interactive)
;;                           (split-window-vertically-n 3)))
;;(global-set-key "\C-x#" '(lambda ()
;;                           (interactive)
;;                           (split-window-horizontally-n 3)))
;;
;; C-x C-gでM-x goto-line
;;(global-set-key "\C-x\C-g" 'goto-line)

;; ウィンドウ分割時も右端で折り返す
;;(setq truncate-partial-width-windows nil)

;; フォント変更->Ricty
;;(let* ((size 14)
;;           (asciifont "Ricty") ; ASCII fonts
;;           (jpfont "Ricty") ; Japanese fonts
;;           (h (* size 10))
;;           (fontspec (font-spec :family asciifont))
;;           (jp-fontspec (font-spec :family jpfont)))
;;      (set-face-attribute 'default nil :family asciifont :height h)
;;      (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
;;      (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
;;      (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
;;      (set-fontset-font nil '(#x0080 . #x024F) fontspec)
;;      (set-fontset-font nil '(#x0370 . #x03FF) fontspec))

;; multi-term
;;(require 'multi-term)

;; auto-complete-mode
;;(require 'auto-complete-config)
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
;;(ac-config-default)

;; eshellでclearを実装
;;(defun eshell/clear ()
;;  "Clear the current buffer, leacing one prompt at the top."
;;  (interactive)
;;  (let ((inhibit-read-only t))
;;	(erase-buffer)))

;; C−x C-f C-rで開くファイルを履歴からインクリメンタルサーチする。
;;(require 'minibuf-isearch)
;;(require 'session)
;;(add-hook 'after-init-hook 'session-initialize)

;; 1行ずつスクロールする
;;(setq scroll-step 1)

;;;; yatex
;;(setq auto-mode-alist
;;      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
;;(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;;(setq dvi2-command "open -a TeXShop"
;;(setq dvi2-command "open -a Skim"
;;      tex-command "~/Library/TeXShop/bin/platex2pdf-utf8"
;;      YaTeX-kanji-code nil)
;;(dolist (dir (list
;;              "/sbin"
;;              "/usr/sbin"
;;              "/bin"
;;              "/usr/bin"
;;              "/opt/local/bin"
;;              "/sw/bin"
;;              "/usr/local/bin"
;;              "/usr/texbin"
;;              (expand-file-name "~/bin")
;;              (expand-file-name "~/.emacs.d/bin")
;;              ))
;; ;; PATH と exec-path に同じ物を追加します
;; (when (and (file-exists-p dir) (not (member dir exec-path)))
;;   (setenv "PATH" (concat dir ":" (getenv "PATH")))
;;   (setq exec-path (append (list dir) exec-path))))

;; 自動改行しない
;;(add-hook 'yatex-mode-hook'(lambda ()(setq auto-fill-function nil)))

;; リージョン内の行数と文字数をモードラインに表示する
;;(defun count-lines-and-chars ()
;;  (if mark-active
;;      (format "%d lines,%d chars "
;;              (count-lines (region-beginning) (region-end))
;;              (- (region-end) (region-beginning)))
;;      ;;(count-lines-region (region-beginning) (region-end)) ;; これだとエコーエリアがチラつく
;;    ""))

;;(add-to-list 'default-mode-line-format
;;             '(:eval (count-lines-and-chars)))

;; Emmet
;; (require 'emmet-mode)
;; (add-hook 'sgml-mode-hook 'emmet-mode) ;; マークアップ言語全部で使う
;; (add-hook 'css-mode-hook  'emmet-mode) ;; CSSにも使う
;; (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 4))) ;; indent はスペース2個
;;(add-hook 'sgml-mode-hook 'emmet-mode) ;; マークアップモードで自動的に emmet-mode をたちあげる
;;(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 4))) ;; indent 4 spaces
;;(setq emmet-move-cursor-between-quotes t) ;; 最初のクオートの中にカーソルをぶちこむ
;;(eval-after-load "emmet-mode"
;;  '(define-key emmet-mode-keymap (kbd "C-j") nil)) ;; C-j は newline のままにしておく
;;(keyboard-translate ?\C-i ?\H-i) ;;C-i と Tabの被りを回避
;;(define-key emmet-mode-keymap (kbd "H-i") 'emmet-expand-line) ;; C-i で展開
;;

;; 括弧の自動補完
;;(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;;(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
;;(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
;;(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
;;(global-set-key (kbd "\'") 'skeleton-pair-insert-maybe)
;;(setq skeleton-pair 1)

;; markdown-mode
;;(autoload 'markdown-mode "markdown-mode"
;;"Major mode for editing Markdown files" t)
;;(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;;(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; web-mode
;;(require 'web-mode)
;; 拡張子の設定
;;(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
;;(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
;; インデント関係
;;(defun web-mode-hook ()
;;  "Hooks for Web mode."
;;  (setq web-mode-html-offset   4)
;;  (setq web-mode-css-offset    4)
;;  (setq web-mode-script-offset 4)
;;  (setq web-mode-php-offset    4)
;;  (setq web-mode-java-offset   4)
;;  (setq web-mode-asp-offset    4)
;;  (setq indent-tabs-mode t)
;;  (setq tab-width 4))
;;(add-hook 'web-mode-hook 'web-mode-hook)
