call plug#begin(stdpath('data') . 'vimplug')

Plug 'arcticicestudio/nord-vim'
Plug 'glepnir/lspsaga.nvim'
Plug 'hoob3rt/lualine.nvim'
Plug 'hrsh7th/nvim-compe'
Plug 'jiangmiao/auto-pairs'
Plug 'kabouzeid/nvim-lspinstall'
Plug 'kyazdani42/nvim-tree.lua'
Plug 'kyazdani42/nvim-web-devicons'
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'nvim-treesitter/nvim-treesitter-textobjects'
Plug 'romgrk/barbar.nvim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'

call plug#end()

colorscheme nord

source ~/.config/nvim/basics.vim
source ~/.config/nvim/buffer-line.vim
source ~/.config/nvim/mappings.vim
source ~/.config/nvim/nvim-tree.vim
source ~/.config/nvim/telescope.vim

lua << EOF
require('completion')
require('fuzzy-finder')
require('lsp')
require('status-line')
require('treesitter')
EOF
