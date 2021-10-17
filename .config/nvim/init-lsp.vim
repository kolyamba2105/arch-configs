call plug#begin(stdpath('data') . 'vimplug')

Plug 'hoob3rt/lualine.nvim'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/nvim-cmp'
Plug 'kyazdani42/nvim-tree.lua'
Plug 'kyazdani42/nvim-web-devicons'
Plug 'lukas-reineke/indent-blankline.nvim'
Plug 'mhartington/formatter.nvim'
Plug 'neovim/nvim-lspconfig'
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
source ~/.config/nvim/vim/buffer-line.vim
source ~/.config/nvim/vim/fugitive.vim
source ~/.config/nvim/vim/mappings.vim
source ~/.config/nvim/vim/term.vim

lua << EOF
require('color-scheme')
require('completion')
require('file-tree')
require('format')
require('fuzzy-finder')
require('indent-line')
require('lsp')
require('status-line')
require('treesitter')
EOF
