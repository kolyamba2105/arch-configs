call plug#begin(stdpath('data') . 'vimplug')

Plug 'hoob3rt/lualine.nvim'
Plug 'kyazdani42/nvim-tree.lua'
Plug 'kyazdani42/nvim-web-devicons'
Plug 'lukas-reineke/indent-blankline.nvim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-treesitter/nvim-treesitter', {'branch': '0.5-compat', 'do': ':TSUpdate'}
Plug 'romgrk/barbar.nvim'
Plug 'sainnhe/gruvbox-material'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'voldikss/vim-floaterm'

call plug#end()

source ~/.config/nvim/vim/basics.vim
source ~/.config/nvim/vim/coc.vim
source ~/.config/nvim/vim/mappings.vim

lua << EOF
require('buffer-line')
require('color-scheme')
require('file-tree')
require('fugitive')
require('fuzzy-finder')
require('indent-line')
require('status-line')
require('term')
require('treesitter')
EOF
