# Neovim

1. Install `vim-plug`

```sh
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
```

2. Install plugins with `:PlugInstall`

3. Install all necessary language servers with `:LspInstall <name>` 

List of servers: 
- bash
- css
- diagnosticls
- html
- json
- typescript
- vim
- yaml
