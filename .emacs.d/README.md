#emacs 設定

* github から dotfile を落とす

```
$ git clone http://atsushi-yoshinari@alex.ntj.local:7990/scm/corp/mocha-dp-specification.git
```

## auto-install を入れる
* auto-install.elをインストールするディレクトリ~/.emacs.d/auto-installを作る

```
$ mkdir -p ~/.emacs.d/auto-install
$ cd ~/.emacs.d/auto-install
```

* auto-install.elをダウンロード

```
$ wget http://www.emacswiki.org/emacs/download/auto-install.el
```

* ダウンロードしたauto-install.elを、バイトコンパイル

```
$ emacs --batch -Q -f batch-byte-compile auto-install.el
```

*  ~/.emacsに設定を追加します。
  * 下記を追記

```
;; auto-install
(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install/"))
(require 'auto-install)
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
```

* M-x auto-install-batch(パッケージ名を指定してインストール)やM-x auto-install-from-emacswiki(EmacsWikiに登録されているパッケージをインストール)を使って、Emacs Lispのプログラムをインストールすることができるようになります。


## anything を入れる
* パッケージをインストールする
  * M-x auto-install-batchを実行して、パッケージ名としてanythingを入力するとanything.elに必要なファイル(複数)をダウンロードしてくれます。あとはダウンロードしたファイル名が次々と表示されるので、C-c C-cでバイトコンパイルすればインストールされます。


* emacsに設定を追加する
  * M-x anythingとタイプすると、anything.elが起動。
    * 毎回M-x anythingとタイプするのは面倒なので、C-x C-a にキーを割り当てる


```
;; anything
(require 'anything-startup)
(define-key global-map (kbd "C-x C-a") 'anything)
```


## markdown mode
* M-x package-list-packagesでパッケージの一覧を表示し、markdown-modeにIを押してインストールマークを付けた後Xキーを押してインストールを実行する
  * ただし、PC に markdown コマンドがない場合は別途入れる

```
;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
"Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
```

* C-c C-c p でプレビューを開く際に、なぜか command not found markdown と言われるので、pandoc コマンドを入れて下記を設定に追加

```
(custom-set-variables '(markdown-command "/usr/local/bin/pandoc"))
```

## scss-mode
* M-x package-list-packages で scss-mode をインストール

```
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
```

## js2-mode
* google の[公式サイト](https://code.google.com/p/js2-mode/)から js2.el をダウンロード
* js2.el を site-lisp 下に配置する
* site-lisp に移動してバイトコンパイル

```
$ cd ~/.emacs.d/site-lisp
$ emacs -batch -f batch-byte-compile js2-mode.el
```

* 設定を追加

```
;; js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
```

## Go-lang
### M-x package-list-packages で go-mode をインストール
* homebrew で入れたコマンドも使えるように homebrew のパスを追加

```
(add-to-list 'exec-path (expand-file-name "/usr/local/bin"))
```

* go get で入れたツールを Emacs から使うために設定に $GOPATH/bin を追加

```
;; Go-lang
(add-to-list 'exec-path (expand-file-name "~/.go/bin"))
```

### Go-lang 用の autocomplete を設定
* gocode をインストール

```
$ go get -u github.com/nsf/gocode
```

* 設定を追加

```
;; auto-complete
(add-to-list 'load-path "~/.go/src/github.com/nsf/gocode/emacs/")
(require 'go-autocomplete)
```


### M-. で godef が効くようにする
* godef をインストール

```
$ go get code.google.com/p/rog-go/exp/cmd/godef
```

* 設定を追加

```
(add-hook 'go-mode-hook (lambda () (local-set-key (kbd "M-.") 'godef-jump)))
```

## Flycheck
* M-x package-list-packages で flycheck をインストール
* 設定を追加

```
(add-hook 'after-init-hook #'global-flycheck-mode)
```

## white-space
* 設定を追加

```
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
```

## elixir-mode
* M-x package-list-packages で elixir-mode, alchemist をインストール
* 設定を追加

```
(require 'elixir-mode)
(require 'alchemist)
(add-hook 'elixir-mode-hook 'ac-alchemist-setup)
```
