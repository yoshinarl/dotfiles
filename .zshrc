# ===== 基本設定 =====
autoload -U compinit
compinit

autoload -U select-word-style
select-word-style bash

# ===== 履歴設定 =====
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt hist_ignore_dups     # ignore duplication command history list
setopt share_history        # share command history data
setopt hist_ignore_all_dups
setopt hist_save_no_dups

# ===== 一般オプション =====
setopt auto_pushd
setopt pushd_ignore_dups
setopt correct
setopt No_beep

# ===== プロンプト設定 =====
case ${UID} in
0)
    PROMPT="%B%{[31m%}%/#%{[m%}%b "
    PROMPT2="%B%{[31m%}%_#%{[m%}%b "
    SPROMPT="%B%{[31m%}%r is correct? [n,y,a,e]:%{[m%}%b "
    [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
        PROMPT="%{[37m%}${HOST%%.*} ${PROMPT}"
    ;;
*)
    PROMPT="%{[31m%}%/%%%{[m%} "
    PROMPT2="%{[31m%}%_%%%{[m%} "
    SPROMPT="%{[31m%}%r is correct? [n,y,a,e]:%{[m%} "
    [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
        PROMPT="%{[37m%}${HOST%%.*} ${PROMPT}"
    ;;
esac

# ===== 色設定 =====
export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# ===== エイリアス設定 =====
alias ...="cd ..."
alias ..="cd .."
alias be="bundle exec"
alias c="pbcopy"
alias cp="cp -i"
alias g="git"
alias ll="ls -lG"
alias ls="ls -G"
alias mv="mv -i"

# ===== PATH設定 =====
export PATH=/usr/local/bin:$PATH

# Added by LM Studio CLI (lms)
export PATH="$PATH:~/.lmstudio/bin"
# End of LM Studio CLI section

# Added by Windsurf
export PATH="~/.codeium/windsurf/bin:$PATH"

# TypeScript language tools (.dotfiles npm prefix)
ts_ls_bin="$HOME/.dotfiles/npm/typescript-ls/node_modules/.bin"
if [ -d "$ts_ls_bin" ]; then
  export PATH="$ts_ls_bin:$PATH"
fi

# ===== Git設定 =====
autoload -Uz VCS_INFO_get_data_git; VCS_INFO_get_data_git 2> /dev/null

function rprompt-git-current-branch {
    local name st color gitdir action
    if [[ "$PWD" =~ '/\.git(/.*)?$' ]]; then
        return
    fi

    name=`git rev-parse --abbrev-ref=loose HEAD 2> /dev/null`
    if [[ -z $name ]]; then
        return
    fi

    gitdir=`git rev-parse --git-dir 2> /dev/null`
    action=`VCS_INFO_git_getaction "$gitdir"` && action="($action)"

    if [[ -e "$gitdir/rprompt-nostatus" ]]; then
        echo "$name$action "
        return
    fi

    st=`git status 2> /dev/null`
    if [[ -n `echo "$st" | grep "^nothing to"` ]]; then
        color=%F{green}
    elif [[ -n `echo "$st" | grep "^nothing added"` ]]; then
        color=%F{yellow}
    elif [[ -n `echo "$st" | grep "^# Untracked"` ]]; then
        color=%B%F{red}
    else
        color=%F{red}
    fi

    echo "$color$name$action%f%b"
}

# プロンプトが表示されるたびにプロンプト文字列を評価、置換する
setopt prompt_subst

RPROMPT='[`rprompt-git-current-branch`]'

# ===== Git エイリアス =====
alias gst="git status -s -b"
alias giff="git diff"
alias gitwip="git add . && git commit -m 'WIP'"

# ===== 外部ツール設定 =====
export YVM_DIR=/usr/local/opt/yvm
[ -r $YVM_DIR/yvm.sh ] && . $YVM_DIR/yvm.sh

export PATH="/usr/local/sbin:$PATH"

# anyenv (プログラミング言語バージョン管理)
if [ -e "$HOME/.anyenv" ]
then
  export PATH="${HOME}/.anyenv/bin:$PATH"
  if command -v anyenv 1>/dev/null 2>&1
  then
    eval "$(anyenv init -)"
  fi
fi

# mise
if [ -e "$HOME/.local/bin/mise" ]
then
    eval "$($HOME/.local/bin/mise activate zsh)"
fi

# ===== プロジェクト固有設定 =====

# NRFSDK v12.3.0 用パス
# https://www.nordicsemi.com/Software-and-Tools/Software/nRF5-SDK/Download から DL されていること
export NRFSDK12_ROOT=~/Workspace/nRF5_SDK_12.3.0_d7731ad

# ===== プラットフォーム固有設定 =====
if uname -a | grep -sq "Ubuntu"; then
  # Ubuntu
  alias rm='trash'
elif [ "$(uname)" = "Darwin" ]; then
  # macOS
  eval "$(/opt/homebrew/bin/brew shellenv)"
elif [ -n "$WSL_DISTRO_NAME" ]; then
  # WSL2
fi

# ===== 起動時処理 =====
# シェル立ち上げ時に稼働時間を出力
uptime
