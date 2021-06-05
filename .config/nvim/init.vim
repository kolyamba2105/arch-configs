call plug#begin('~/.vim/plugged')

Plug 'Yggdroot/indentLine'
Plug 'arcticicestudio/nord-vim'
Plug 'cocopon/iceberg.vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/goyo.vim'
Plug 'leafgarland/typescript-vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'unkiwii/vim-nerdtree-sync'
Plug 'vim-airline/vim-airline'

call plug#end()

" Basic Vim settings
colorscheme nord
syntax on

set clipboard+=unnamedplus
set colorcolumn=100
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
set wildignore +=*/tmp/*,*.so,*.swp,*.zip,*.svg,*.png,*.jpg,*.gif,node_modules

" Cursor settings
let &t_SI ="\<Esc>[6 q"
let &t_SR ="\<Esc>[4 q"
let &t_EI ="\<Esc>[2 q"

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
nmap <silent> <C-A-h> :wincmd h<CR>
nmap <silent> <C-A-j> :wincmd j<CR>
nmap <silent> <C-A-k> :wincmd k<CR>
nmap <silent> <C-A-l> :wincmd l<CR>

" Create splits
nmap <silent> <C-s> :vsp<CR>
nmap <silent> <C-A-s> :sp<CR>

" Move between buffers
nmap <silent> <C-h> :bp<CR>
nmap <silent> <C-l> :bn<CR>
nmap <silent> <C-w> :bd<CR>

" Close current buffer/window
nmap <leader>c :close<CR>

" Save file
nmap <silent> <leader>w :w<CR>

" Clear search input
nmap <silent> Z :noh<CR>

" Quickly insert an empty new line without entering insert mode
nnoremap <leader>o o<Esc>
nnoremap <leader>O O<Esc>

" Airline settings
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'
let g:airline_extensions = ["branch", "ctrlp", "tabline", "whitespace", "wordcount"]
let g:airline_powerline_fonts = 1

" NERDTree settings
let g:NERDTreeShowHidden=1
let g:NERDTreeWinSize=45
let g:nerdtree_sync_cursorline = 1

nmap <silent> <C-n> :NERDTreeToggle<CR>

" Ctrl-p settings
let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']

" Goyo settings
let g:goyo_linenr = 1
let g:goyo_width = 100

nmap <leader>g :Goyo<CR>

" CoC settings
nmap <leader>a <Plug>(coc-codeaction)
nmap <leader>f :call CocAction('format')<CR>
nmap <leader>i :CocCommand tsserver.organizeImports<CR>
nmap <leader>l :CocCommand tsserver.openTsServerLog<CR>
nmap <leader>R :CocCommand tsserver.restart<CR>
nmap <leader>rn <Plug>(coc-rename)

nmap <silent> <C-j> <Plug>(coc-diagnostic-next)
nmap <silent> <C-k> <Plug>(coc-diagnostic-prev)
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> gy <Plug>(coc-type-definition)

nnoremap <silent> K :call <SID>show_documentation()<CR>

inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
inoremap <silent><expr> <TAB> pumvisible() ? "\<C-n>" : <SID>check_back_space() ? "\<TAB>" : coc#refresh()
inoremap <silent><expr> <c-space> coc#refresh()

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction
