;;  ---------------
;; |   共通設定    |
;;  ---------------

;; ロードパス
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; 環境変数パス
(dolist (dir (list
              "/sbin"
              "/usr/sbin"
              "/bin"
              "/usr/bin"
              "/usr/local/bin"
              (expand-file-name "~/bin")
              (expand-file-name "~/.emacs.d/bin")
              ))
 (when (and (file-exists-p dir) (not (member dir exec-path)))
   (setenv "PATH" (concat dir ":" (getenv "PATH")))
   (setq exec-path (append (list dir) exec-path))))

;; homebrwe でインストールしたツールを使う
(add-to-list 'exec-path (expand-file-name "/usr/local/bin"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; el-get
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;; homebrwe でインストールしたツールを使う
(add-to-list 'exec-path (expand-file-name "/usr/local/bin"))

;; 対応する括弧を光らせる
(show-paren-mode 1)

;; バックアップファイルを作らない
(setq backup-inhibited t)

;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;; 保存時に行末の空白を削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; optionキーをMetaキーとして利用
(setq mac-option-modifier 'meta)

;; Returnでオートインデント
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

;; タブ幅を4に設定。タブを使わずスペースにする
;;(setq-default tab-width 4 indent-tabs-mode nil)
;; タブ幅の倍数を設定
(setq tab-stop-list '(2 4 6 8 10 12 14 16 18 20))
(setq-default tab-width 2 indent-tabs-mode nil)

;; 起動時のサイズ、表示位置、フォントを設定
(setq initial-frame-alist
      (append (list
               '(width . 175)
               '(height . 110)
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

;; フォント変更->Ricty
(let* ((size 13)
           (asciifont "Ricty") ; ASCII fonts
           (jpfont "Ricty") ; Japanese fonts
           (h (* size 11))
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
;;  C-x@で縦3分割。C-x#で横3分割
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

;; ファイルに変更があったら自動で再読み込みする
(global-auto-revert-mode 1)

;; 行の移動を実装
;; Shift-Alt-p: 上へ移動
;; Shift-Alt-n: 下へ移動
(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)
    (previous-line)))

(global-set-key (kbd "M-P") 'move-line-up)
(global-set-key (kbd "M-N") 'move-line-down)

;; C-z を無効化
(global-unset-key "\C-z")

;; タイトルバーにファイルのフルパス表示
(setq frame-title-format
      (format "%%b - %%f"))

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
                    :foreground "gray36"
                    :underline t)
(set-face-attribute 'whitespace-space nil
                    :background my/bg-color
                    :foreground "GreenYellow"
                    :weight 'bold)
(set-face-attribute 'whitespace-empty nil
                    :background my/bg-color)

;;  --------------
;; |    el-get    |
;;  --------------

;; 行番号を表示
(el-get-bundle linum-ex
  (global-linum-mode t)
  (setq linum-format "%5d"))

;; auto-complete-mode
(defun load-auto-complete ()
  (el-get-bundle auto-complete)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
  (add-to-list 'ac-modes 'text-mode)
  (add-to-list 'ac-modes 'enh-ruby-mode)
  (setq ac-use-menu-map t)
  (setq ac-use-fuzzy t))

;; flycheck
(el-get-bundle flycheck)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'enh-ruby-mode-hook 'flycheck-mode)

;; anything
(el-get-bundle anything)
(require 'anything-startup)
(define-key global-map (kbd "C-x C-a") 'anything)

;; enh-ruby-mode
(el-get-bundle enh-ruby-mode)
(autoload 'enh-ruby-mode "enh-ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
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
(el-get-bundle ruby-electric)
(add-hook 'enh-ruby-mode-hook '(lambda () (ruby-electric-mode t)))
(setq ruby-electric-expand-delimiters-list nil)

;; ruby-block
(el-get-bundle ruby-block)
(setq ruby-block-highlight-toggle t)

;; rubocop
(el-get-bundle rubocop)
(add-hook 'enh-ruby-mode-hook 'rubocop-mode)

;; Projectile Rails
(el-get-bundle projectile)
(projectile-global-mode)
(el-get-bundle projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (inflections))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ag
(el-get-bundle ag)
(setq ag-highlight-search t)
(setq ag-reuse-buffers t)

;; js2-mode
(el-get-bundle js2-mode)
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; haml-mode
(el-get-bundle haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(add-hook 'haml-mode-hook
  (lambda ()
    (define-key haml-mode-map "\C-m" 'newline-and-indent)))

;;  ---------------
;; |    package    |
;;  ---------------

;; C−x C-f C-rで開くファイルを履歴からインクリメンタルサーチする。
(require 'minibuf-isearch)
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; git-gutter-fringe+
(require 'git-gutter-fringe+)
(global-git-gutter+-mode t)
