# CLAUDE.md

このファイルはClaude Code (claude.ai/code) がこのリポジトリのコードを扱う際のガイダンスを提供します。

## リポジトリ構造

これは開発ツールとシェル環境の設定ファイルを管理する個人用dotfilesリポジトリです。主要なコンポーネントは以下の通りです：

- **Emacs設定** (`.emacs.d/`) - `leaf`パッケージマネージャーを使用した786行の包括的なEmacs設定
- **シェル設定** (`.zshrc`) - 履歴、プロンプト、補完設定を含むZshシェル設定
- **開発ツール** - Git、Tig、Pry（Ruby）、WezTermターミナル、その他のCLIツール
- **パッケージ管理** (`Brewfile`) - macOSパッケージインストール用のHomebrewバンドルファイル

## セットアップコマンド

### 初期セットアップ
```bash
# dotfilesをクローンしてリンク
cd .dotfiles
sh linkDotfiles.sh
```

### パッケージインストール（macOS）
```bash
# Brewfileで定義されたすべてのパッケージをインストール
brew bundle install

# Emacsパッケージをインストール（dotfilesリンク後）
emacs ~/.emacs.d/package-install.el
# その後Emacsで：M-x eval-buffer
```

## 設定アーキテクチャ

### Emacsセットアップ
- 設定に`leaf`パッケージマネージャーを使用
- パッケージソース：MELPA、GNU ELPA、Org ELPA
- `el-get`と`package.el`による依存関係管理
- `leaf`宣言によるモジュラー設定
- メイン設定ファイル：`.emacs.d/init.el`（786行）

### シェル環境
- カスタムプロンプトと履歴設定を持つZsh
- `compinit`による補完システム
- 開発ワークフロー向けに最適化された履歴設定
- `.zshenv`で定義された環境変数

### ファイルリンク戦略
`linkDotfiles.sh`スクリプトはリポジトリファイルからホームディレクトリ内の場所へのシンボリックリンクを作成します：
- `.emacs.d` → `~/.emacs.d`
- `.zshrc` → `~/.zshrc`
- `.gitconfig` → `~/.gitconfig`
- その他のdotfiles

## 含まれる開発ツール

Brewfileには開発重視のパッケージが含まれています：
- **エディタ**：Emacs（railwaycat/emacsmacport）、Visual Studio Code
- **バージョン管理**：Git、GitHub CLI、Tig
- **言語**：各種コンパイラ（GCC、ARMクロスコンパイル）、Ruby（anyenv経由）
- **データベース**：MySQL、PostgreSQL、Redis
- **クラウド**：AWS CLI、Google Cloud SDK、Docker
- **ターミナル**：WezTerm設定を含む