# Neovim

- Install `vim-plug`

```sh
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
```

- Install plugins with `:PlugInstall`

- Install all necessary language servers with `:LspInstall <name>` 

  List of servers: `html, typescript, bash, yaml, json, css, vim, diagnosticls`
