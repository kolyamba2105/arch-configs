# Neovim

## Initial setup

- Install [vim-plug](https://github.com/junegunn/vim-plug)
- Install plugins with `:PlugInstall`

## Neovim with native LSP

Run `nvim` to use all new features and plugins of Neovim 0.5.

- Native LSP
  - [lspsaga.nvim](https://github.com/glepnir/lspsaga.nvim)
  - [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig)
  - [nvim-lspinstall](https://github.com/kabouzeid/nvim-lspinstall)
- Autocompletion
  - [nvim-compe](https://github.com/hrsh7th/nvim-compe)

List of language servers:

- bash
- css
- diagnosticls
- html
- json
- typescript
- vim
- yaml

## Neovim with CoC

Run `vim` to use some new features and plugins of Neovim 0.5 together with CoC.

### CoC extensions

All extensions are installed into `$HOME/.config/coc/extensions`.

`CocInstall coc-css coc-cssmodules coc-diagnostic coc-eslint coc-git coc-html coc-json coc-markdownlint coc-prettier coc-sh coc-spell-checker coc-tsserver coc-vimlsp coc-yaml`

### Common plugins

- [barbar.nvim](https://github.com/romgrk/barbar.nvim)
- [lualine.nvim](https://github.com/hoob3rt/lualine.nvim)
- [nvim-tree.lua](https://github.com/kyazdani42/nvim-tree.lua)
- [nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter)
- [telescope.nvim](https://github.com/nvim-telescope/telescope.nvim)
