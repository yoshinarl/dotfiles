autoload -U compinit
compinit

export LANG=ja_JP.UTF-8

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt hist_ignore_dups     # ignore duplication command history list
setopt share_history        # share command history data
setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt auto_pushd
setopt pushd_ignore_dups
setopt correct
setopt No_beep

# prompt setting
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

# color setting
export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'

# alias
alias ls="ls -G"
alias rm="rmtrash"
export JAVA_HOME='/Library/Java/JavaVirtualMachines/jdk1.8.0_31.jdk/Contents/Home'
export JAVA_OPTS='-DFile.encoding=UTF-8'

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# pyenv PATH Setting
export PYENV_ROOT="${HOME}/.pyenv"
if [ -d "${PYENV_ROOT}" ]; then
    export PATH=${PYENV_ROOT}/bin:$PATH
    eval "$(pyenv init -)"
fi
export PATH=/usr/local/bin:$PATH
export PATH=$PATH:/Users/yoshinari/.nodebrew/current/bin

# virtualenvwrapper PATH Setting
if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
    export WORKON_HOME=$HOME/.virtualenvs
    source /usr/local/bin/virtualenvwrapper.sh
fi

# golang
export GOROOT=/usr/local/opt/go/libexec
export GOPATH=$HOME/.go
export PATH=$PATH:$GOPATH/bin
