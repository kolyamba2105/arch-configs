# Neovim

## Initial setup

- Install [vim-plug](https://github.com/junegunn/vim-plug)
- Install plugins with `:PlugInstall`

## Neovim with native LSP

Run `nvim-lsp` to use all new features and plugins of Neovim 0.5.

- Syntax highlighting
  - [nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter)
  - [nvim-treesitter-textobjects](https://github.com/nvim-treesitter/nvim-treesitter-textobjects)

- Native LSP
  - [lspsaga.nvim](https://github.com/glepnir/lspsaga.nvim)
  - [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig)
  - [nvim-lspinstall](https://github.com/kabouzeid/nvim-lspinstall)

- Autocompletion
  - [nvim-compe](https://github.com/hrsh7th/nvim-compe)

### List of language servers

- bash
- css
- diagnosticls
- html
- json
- typescript
- vim
- yaml

## Neovim with CoC

Run `nvim` to use some new features and plugins of Neovim 0.5 together with CoC.

### CoC extensions

All extensions are installed into `$HOME/.config/coc/extensions`.

```vim
CocInstall coc-css coc-cssmodules coc-diagnostic coc-eslint coc-html coc-json
coc-markdownlint coc-prettier coc-sh coc-spell-checker coc-tsserver coc-vimlsp
coc-yaml
```

### Common plugins

- [barbar.nvim](https://github.com/romgrk/barbar.nvim)
- [lualine.nvim](https://github.com/hoob3rt/lualine.nvim)
- [nvim-tree.lua](https://github.com/kyazdani42/nvim-tree.lua)
- [telescope.nvim](https://github.com/nvim-telescope/telescope.nvim)
