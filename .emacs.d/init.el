;;  ---------------
;; |   共通設定    |
;;  ---------------

;; ロードパス
(add-to-list 'load-path "~/.emacs.d/elpa/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; homebrwe でインストールしたツールを使う
(add-to-list 'exec-path (expand-file-name "/usr/local/bin"))

;; auto-install
(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install/"))
(add-to-list 'load-path "~/.emacs.d/auto-install/")
(require 'auto-install)
(auto-install-update-emacswiki-package-name t)
(setq auto-install-directory "~/.emacs.d/auto-install/")
(auto-install-compatibility-setup)

;; anything
(require 'anything-startup)
(define-key global-map (kbd "C-x C-a") 'anything)

;; 対応する括弧を光らせる
(show-paren-mode 1)

;; バックアップファイルを作らない
(setq backup-inhibited t)

;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;; 保存時に行末の空白を削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;; optionキーをMetaキーとして利用
(setq mac-option-modifier 'meta)

;;;;;; Returnでオートインデント
(global-set-key "\C-m" 'newline-and-indent)

;; C-hをBSに
(global-set-key "\C-h" 'backward-delete-char)

;; C-x C-gでM-x goto-line
(global-set-key "\C-x\C-g" 'goto-line)

;; 分割した画面間をShift+矢印で移動
(setq windmove-wrap-around t)
(windmove-default-keybindings)

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
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; タブ幅を4に設定。タブを使わずスペースにする
(setq-default tab-width 4 indent-tabs-mode nil)
;; タブ幅の倍数を設定
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))

;; 起動時のサイズ、表示位置、フォントを設定
(setq initial-frame-alist
      (append (list
               '(width . 156)
               '(height . 72)
               '(top . 0)
               '(left . 0)
               '(right . 0)
               )
              initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

;; 1行ずつスクロールする
(setq scroll-step 1)

;; 括弧の自動補完
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\'") 'skeleton-pair-insert-maybe)
(setq skeleton-pair 1)


;; C−x C-f C-rで開くファイルを履歴からインクリメンタルサーチする。
(require 'minibuf-isearch)
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; フォント変更->Ricty
(let* ((size 13)
           (asciifont "Ricty") ; ASCII fonts
           (jpfont "Ricty") ; Japanese fonts
           (h (* size 10))
           (fontspec (font-spec :family asciifont))
           (jp-fontspec (font-spec :family jpfont)))
      (set-face-attribute 'default nil :family asciifont :height h)
      (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
      (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
      (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
      (set-fontset-font nil '(#x0080 . #x024F) fontspec)
      (set-fontset-font nil '(#x0370 . #x03FF) fontspec))

;; ウィンドウ分割時も右端で折り返す
(setq truncate-partial-width-windows nil)

;; フレームの3分割
;;  C-xで縦3分割。C-x#で横3分割
(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))
(global-set-key "\C-x@" '(lambda ()
                           (interactive)
                           (split-window-vertically-n 3)))
(global-set-key "\C-x#" '(lambda ()
                           (interactive)
                           (split-window-horizontally-n 3)))

;; 行末が長くなっても自動改行しない
(add-hook 'yatex-mode-hook'(lambda ()(setq auto-fill-function nil)))

;; リージョン内の行数と文字数をモードラインに表示する
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines,%d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
      ;;(count-lines-region (region-beginning) (region-end)) ;; これだとエコーエリアがチラつく
    ""))

(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))

;; スクロールを加速させない
(global-set-key [wheel-up] '(lambda () "" (interactive) (scroll-down 1)))
(global-set-key [wheel-down] '(lambda () "" (interactive) (scroll-up 1)))
(global-set-key [double-wheel-up] '(lambda () "" (interactive) (scroll-down 1)))
(global-set-key [double-wheel-down] '(lambda () "" (interactive) (scroll-up 1)))
(global-set-key [triple-wheel-up] '(lambda () "" (interactive) (scroll-down 2)))
(global-set-key [triple-wheel-down] '(lambda () "" (interactive) (scroll-up 2)))

;; 各種色設定
(if window-system (progn
;; 文字の色を設定します。
  (add-to-list 'default-frame-alist '(foreground-color . "#EEEEEE"))
  ;; 背景色を設定します。
  (add-to-list 'default-frame-alist '(background-color . "#333333"))
  ;; 背景透明
;;  (set-frame-parameter nil 'alpha 95)
  ;; カーソルの色を設定します。
  (add-to-list 'default-frame-alist '(cursor-color . "SlateBlue2"))
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
))

;; C-a で空白を除く行頭へ移動
;; インデント文字を飛ばした行頭に戻る。
;; ただし、ポイントから行頭までの間にインデント文字しかない場合は、行頭に戻る。
(global-set-key "\C-a" 'beggining-of-indented-line)
(defun beggining-of-indented-line (current-point)
  (interactive "d")
  (if (string-match
       "^[ \t]+$"
       (save-excursion
         (buffer-substring-no-properties
          (progn (beginning-of-line) (point))
          current-point)))
      (beginning-of-line)
    (back-to-indentation)))

;; emacs終了時に確認する
(setq confirm-kill-emacs 'y-or-n-p)

;; 最終行に必ず一行挿入する
(setq require-final-newline t)


;;  -------------------
;; |   各モード設定    |
;;  -------------------

;; 行番号を表示
(require 'linum)
(global-linum-mode t)
(setq linum-format "%5d")

;; auto-complete-mode
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(add-to-list 'ac-modes 'text-mode)

;; Emmet
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; マークアップ言語全部で使う
(add-hook 'css-mode-hook  'emmet-mode) ;; CSSにも使う
;(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 4))) ;; indent はスペース4個
(add-hook 'sgml-mode-hook 'emmet-mode) ;; マークアップモードで自動的に emmet-mode をたちあげる
;(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 4))) ;; indent 4 spaces
(add-hook 'web-mode-hook 'emmet-mode) ;; web-mode で自動的に emmet-mode を立ち上げる
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 4))) ;; indent 4 spaces
(setq emmet-move-cursor-between-quotes t) ;; 最初のクオートの中にカーソルをぶちこむ
(eval-after-load "emmet-mode"
  '(define-key emmet-mode-keymap (kbd "C-j") nil)) ;; C-j は newline のままにしておく
(keyboard-translate ?\C-i ?\H-i) ;;C-i と Tabの被りを回避
(define-key emmet-mode-keymap (kbd "H-i") 'emmet-expand-line) ;; C-i で展開


;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
"Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(custom-set-variables '(markdown-command "/usr/local/bin/pandoc"))
(add-hook 'markdown-mode-hook
          '(lambda ()
             (electric-indent-local-mode -1)))

;; web-mode
(require 'web-mode)
;; 拡張子の設定
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
;; インデント関係
(defun web-mode-hook ()
;;  "Hooks for Web mode."
  (setq web-mode-html-offset   4)
  (setq web-mode-css-offset    4)
  (setq web-mode-script-offset 4)
  (setq web-mode-php-offset    4)
  (setq web-mode-java-offset   4)
  (setq web-mode-asp-offset    4)
  (setq web-mode-code-offset   4)
  (setq indent-tabs-mode t)
  (setq tab-width 4))
(add-hook 'web-mode-hook 'web-mode-hook)
(add-hook 'emmet-mode-hook 'web-mode-hock)

;; python 用インデント設定
(add-hook 'python-mode-hook
    '(lambda ()
;;        (setq indent-line-function 'tab-to-tab-stop)
        (setq python-indent 4)
        (setq indent-level 4)
        (setq indent-tabs-mode nil)
    ))

;; go-lang 用
(add-to-list 'exec-path (expand-file-name "~/.go/bin"))
;; auto-complete
(add-to-list 'load-path "~/.go/src/github.com/nsf/gocode/emacs/")
(require 'go-autocomplete)
;; go-flymake
(add-to-list 'load-path "~/.go/src/github.com/dougm/goflymake")
(require 'go-flymake)
;; M-. で godef
(add-hook 'go-mode-hook (lambda () (local-set-key (kbd "M-.") 'godef-jump)))

;; js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; jade-mode
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;; scss-mode
(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

;; インデント幅を2にする
;; コンパイルは compass watchで行うので自動コンパイルをオフ
(defun scss-custom ()
  "scss-mode-hook"
  (and
   (set (make-local-variable 'css-indent-offset) 2)
   (set (make-local-variable 'scss-compile-at-save) nil)
   )
  )
(add-hook 'scss-mode-hook
  '(lambda() (scss-custom)))

;; whitespace
;; 空白関係を可視化させる
(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))

(setq whitespace-display-mappings
      '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

(global-whitespace-mode 1)

(defvar my/bg-color "#333333")
(set-face-attribute 'whitespace-trailing nil
                    :background my/bg-color
                    :foreground "DeepPink"
                    :underline t)
(set-face-attribute 'whitespace-tab nil
                    :background my/bg-color
                    :foreground "LightSkyBlue"
                    :underline t)
(set-face-attribute 'whitespace-space nil
                    :background my/bg-color
                    :foreground "GreenYellow"
                    :weight 'bold)
(set-face-attribute 'whitespace-empty nil
                    :background my/bg-color)
