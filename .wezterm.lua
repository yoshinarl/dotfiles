local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.use_ime = true
config.font = wezterm.font "HackGen Console NF"
-- ↑の設定で日本語入力が動かないことがあったので以下も追加しておく
wezterm.font_with_fallback({
  "Noto Sans Mono CJK JP",
  "HackGen Console NF"
})

return config