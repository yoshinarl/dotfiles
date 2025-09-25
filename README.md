# How to use
---
* Clone this repository.
* Using linkDotfiles.sh to make simbolic link.

```
cd .dotfiles
sh linkeDotfiles.sh
```

## Language servers

### TypeScript

Install the language server into a user-local npm prefix so `eglot` can find it. Versions are tracked in `npm/typescript-ls/package.json`:

```
./scripts/install-typescript-ls.sh
```

`~/.local/lib/typescript-ls/node_modules/.bin` is added to `PATH` by `.zshrc`, so Emacs and shells automatically see `typescript-language-server` after installation.
