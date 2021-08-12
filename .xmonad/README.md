# XMonad

- [ghcup](https://www.haskell.org/ghcup)
- [Article for reference](https://brianbuccola.com/how-to-install-xmonad-and-xmobar-via-stack)

## Setup

**Run all commands from `.xmonad`!**

```sh
# Clone repositories
git clone "https://github.com/xmonad/xmonad" xmonad-git
git clone "https://github.com/xmonad/xmonad-contrib" xmonad-contrib-git
git clone "https://github.com/jaor/xmobar" xmobar-git

# Build and install everything
stack install

# XMonad is ready to be recompiled
xmonad --recompile && xmonad --restart
```

## Update

- Pull `xmonad-git`, `xmonad-contrib-git` and `xmobar-git` repositories
- Run `stack install` from `.xmonad`
