call plug#begin(stdpath('data') . 'vimplug')

Plug 'arcticicestudio/nord-vim'
Plug 'glepnir/lspsaga.nvim'
Plug 'hoob3rt/lualine.nvim'
Plug 'hrsh7th/nvim-compe'
Plug 'jiangmiao/auto-pairs'
Plug 'kabouzeid/nvim-lspinstall'
Plug 'kien/ctrlp.vim'
Plug 'kyazdani42/nvim-tree.lua'
Plug 'kyazdani42/nvim-web-devicons'
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'nvim-treesitter/nvim-treesitter-textobjects'
Plug 'romgrk/barbar.nvim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'unkiwii/vim-nerdtree-sync'

call plug#end()

" Basic Vim settings
colorscheme nord
syntax on

set clipboard+=unnamedplus
set colorcolumn=80
set completeopt=menuone,noselect
set cursorline
set expandtab
set foldmethod=syntax
set ignorecase
set incsearch
set laststatus=2
set nobackup
set noerrorbells
set nofoldenable
set noswapfile
set nowrap
set number
set relativenumber
set scrolloff=12
set shiftwidth=2
set showtabline=2
set signcolumn=yes
set smartcase
set smartindent
set splitbelow
set splitright
set tabstop=2 softtabstop=2
set termguicolors
set undodir=~/.vim/undo
set undofile
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.svg,*.png,*.jpg,*.gif,node_modules

" Disable arrow keys
no <down>   <Nop>
no <left>   <Nop>
no <right>  <Nop>
no <up>     <Nop>

ino <down>  <Nop>
ino <left>  <Nop>
ino <right> <Nop>
ino <up>    <Nop>

vno <down>  <Nop>
vno <left>  <Nop>
vno <right> <Nop>
vno <up>    <Nop>

" Set space as a leader key
map <Space> <leader>

" Move between windows (splits)
nnoremap <silent> <C-A-h> :wincmd h<CR>
nnoremap <silent> <C-A-j> :wincmd j<CR>
nnoremap <silent> <C-A-k> :wincmd k<CR>
nnoremap <silent> <C-A-l> :wincmd l<CR>

" Create splits
nnoremap <leader>h :sp<CR>
nnoremap <leader>v :vsp<CR>

" Move between buffers
nnoremap <silent> <C-h> :bp<CR>
nnoremap <silent> <C-l> :bn<CR>
nnoremap <silent> <C-w> :bd<CR>

" Close current buffer/window
nnoremap <leader>c :close<CR>

" Save file
nnoremap <leader>w :w<CR>

" Clear search input
nnoremap <silent> Z :noh<CR>

" Quickly insert an empty new line without entering insert mode
nnoremap <leader>o o<Esc>
nnoremap <leader>O O<Esc>

" Ctrl-p
let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']

" LSP
nnoremap <silent> <C-j> <cmd>Lspsaga diagnostic_jump_next<CR>
nnoremap <silent> <C-k> <cmd>Lspsaga diagnostic_jump_prev<CR>
nnoremap <silent> K     <cmd>Lspsaga hover_doc<CR>
nnoremap <silent> ga    <cmd>Lspsaga code_action<CR>
nnoremap <silent> gd    <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> gf    <cmd>lua vim.lsp.buf.formatting()<CR>
nnoremap <silent> gn    <cmd>lua vim.lsp.buf.rename()<CR>
nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR>

" Directory tree
let g:nvim_tree_auto_close = 1
let g:nvim_tree_auto_open = 1
let g:nvim_tree_follow = 1
let g:nvim_tree_highlight_opened_files = 1
let g:nvim_tree_ignore = ['.git']
let g:nvim_tree_quit_on_open = 1
let g:nvim_tree_update_cwd = 1
let g:nvim_tree_width = 50

nnoremap <C-n>      :NvimTreeToggle<CR>
nnoremap <leader>r  :NvimTreeRefresh<CR>
nnoremap <leader>n  :NvimTreeFindFile<CR>

" Buffer line
let bufferline = get(g:, 'bufferline', {})

let bufferline.animation = v:false
let bufferline.auto_hide = v:true
let bufferline.clickable = v:false
let bufferline.closable = v:false
let bufferline.icon_close_tab = ''
let bufferline.icon_close_tab_modified = ''
let bufferline.icon_separator_active = ''
let bufferline.icon_separator_inactive = ''
let bufferline.no_name_title = 'Buffer'
let bufferline.maximum_padding = 12
let bufferline.tabpages = v:false

lua << EOF
require("completion")
require("lsp")
require("status-line")
require("treesitter")
EOF
