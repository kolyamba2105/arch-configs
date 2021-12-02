# Neovim

## Neovim with native LSP

Run `nvim-lsp` to use all new features and plugins of Neovim 0.5.

### Plugins

- [nvim-cmp](https://github.com/hrsh7th/nvim-cmp)
  - [cmp-buffer](https://github.com/hrsh7th/cmp-buffer)
  - [cmp-path](https://github.com/hrsh7th/cmp-path)
  - [cmp-nvim-lsp](https://github.com/hrsh7th/cmp-nvim-lsp)
- [formatter.nvim](https://github.com/mhartington/formatter.nvim)
- [nvim-lsp-installer](https://github.com/williamboman/nvim-lsp-installer)
- [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig)
- [nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter)


### Install language servers

```sh
yarn global add \
  @elm-tooling/elm-language-server \
  @tailwindcss/language-server \
  bash-language-server \
  typescript-language-server \
  vscode-langservers-extracted \
  yaml-language-server
```

- Rust language server (`rust_analyzer`) is installed via Pacman.

## Neovim with CoC

Run `nvim-coc` to use some new features and plugins of Neovim 0.5 together with
CoC.

### CoC extensions

All extensions are listed in `./vim/coc.vim` and installed into
`$HOME/.config/coc/extensions`.

## Common plugins

- [barbar.nvim](https://github.com/romgrk/barbar.nvim)
- [indent-blankline.nvim](https://github.com/lukas-reineke/indent-blankline.nvim)
- [lualine.nvim](https://github.com/hoob3rt/lualine.nvim)
- [nvim-tree.lua](https://github.com/kyazdani42/nvim-tree.lua)
- [telescope.nvim](https://github.com/nvim-telescope/telescope.nvim)
