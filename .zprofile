# mise (PATH初期化後に配置)
if [[ -x "$HOME/.local/bin/mise" || -n $(command -v mise 2>/dev/null) ]]
then
    eval "$(mise activate zsh --shims)"
fi
