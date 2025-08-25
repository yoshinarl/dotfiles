local wezterm = require 'wezterm'
local config = wezterm.config_builder()

local os_name = wezterm.target_triple:match("darwin") and "macos"
             or wezterm.target_triple:match("linux") and "linux"
             or wezterm.target_triple:match("windows") and "windows"

if os_name == "macos" then
elseif os_name == "linux" then
  config.enable_wayland = false
elseif os_name == "windows" then
end

config.use_ime = true
config.font = wezterm.font "HackGen Console NF"
config.font_size = 14
-- ↑の設定で日本語入力が動かないことがあったので以下も追加しておく
wezterm.font_with_fallback({
  "Noto Sans Mono CJK JP",
  "HackGen Console NF"
})

config.keys = {
  -- +D で横分割
  {
    key = "D",
    mods = "ALT",
    action = wezterm.action.SplitHorizontal { domain = "CurrentPaneDomain" },
  },
  -- Cmd+Shift+D で縦分割
  {
    key = "D",
    mods = "ALT|SHIFT",
    action = wezterm.action.SplitVertical { domain = "CurrentPaneDomain" },
  },
}

return config
