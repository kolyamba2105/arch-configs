# Neovim

## Initial setup

- Install [vim-plug](https://github.com/junegunn/vim-plug)
- Install plugins with `:PlugInstall`

## Neovim with native LSP

Run `nvim-lsp` to use all new features and plugins of Neovim 0.5.

### Plugins

- [nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter)
- [lspsaga.nvim](https://github.com/glepnir/lspsaga.nvim)
- [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig)
- [nvim-lspinstall](https://github.com/kabouzeid/nvim-lspinstall)
- [nvim-compe](https://github.com/hrsh7th/nvim-compe)

### List of language servers

- bash
- css
- diagnosticls
- elm
- html
- json
- typescript
- vim
- yaml

## Neovim with CoC

Run `nvim-coc` to use some new features and plugins of Neovim 0.5 together with
CoC.

### CoC extensions

All extensions are listed in `./vim/coc.vim` and installed into
`$HOME/.config/coc/extensions`.

### Common plugins

- [barbar.nvim](https://github.com/romgrk/barbar.nvim)
- [indent-blankline.nvim](https://github.com/lukas-reineke/indent-blankline.nvim)
- [lualine.nvim](https://github.com/hoob3rt/lualine.nvim)
- [nvim-tree.lua](https://github.com/kyazdani42/nvim-tree.lua)
- [telescope.nvim](https://github.com/nvim-telescope/telescope.nvim)
