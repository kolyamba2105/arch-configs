call plug#begin('~/.vim/plugged')

Plug 'arcticicestudio/nord-vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'itchyny/lightline.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'leafgarland/typescript-vim'
Plug 'mengelbrecht/lightline-bufferline'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'Yggdroot/indentLine'

call plug#end()

colorscheme nord
syntax on

set colorcolumn=100
set cursorline
set expandtab
set ignorecase
set incsearch
set laststatus=2
set nobackup
set noerrorbells
set noswapfile
set nowrap
set nu
set scrolloff=12
set shiftwidth=2
set showtabline=2
set smartcase
set smartindent
set tabstop=2 softtabstop=2
set termguicolors
set undodir=~/.vim/undo
set undofile
set wildignore +=*/tmp/*,*.so,*.swp,*.zip,*.svg,*.png,*.jpg,*.gif,node_modules

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

map <Space> <leader>

nmap <silent> <C-A-h> :wincmd h<CR>
nmap <silent> <C-A-j> :wincmd j<CR>
nmap <silent> <C-A-k> :wincmd k<CR>
nmap <silent> <C-A-l> :wincmd l<CR>

nmap <silent> <F3> :set hlsearch!<CR>

nmap <silent> <C-h> :bp<CR>
nmap <silent> <C-l> :bn<CR>
nmap <silent> <C-w> :bd<CR>

nmap <silent> <leader>w :w<CR>

nmap <leader>f :call CocAction('format')<CR>
nmap <leader>o :call CocActionAsync('runCommand', 'tsserver.organizeImports')<CR>

nmap <silent> <C-n> :NERDTreeToggle<CR>

nmap <leader>a      <Plug>(coc-codeaction)
nmap <leader>rn     <Plug>(coc-rename)
nmap <silent> <C-j> <Plug>(coc-diagnostic-next)
nmap <silent> <C-k> <Plug>(coc-diagnostic-prev)
nmap <silent> gd    <Plug>(coc-definition)
nmap <silent> gi    <Plug>(coc-implementation)
nmap <silent> gr    <Plug>(coc-references)
nmap <silent> gy    <Plug>(coc-type-definition)

nnoremap <silent> <leader>e  :<C-u>CocList diagnostics<cr>

let g:NERDTreeWinSize=45

let g:lightline = {
  \ 'colorscheme': 'nord',
  \ 'active': {
  \   'left': [ [ 'mode', 'paste' ], [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
  \ },
  \ 'tabline': {
  \   'left': [ [ 'buffers' ] ],
  \   'right': [ [ 'close' ] ]
  \ },
  \ 'component_expand': {
  \   'buffers': 'lightline#bufferline#buffers'
  \ },
  \ 'component_function': {
  \   'gitbranch': 'FugitiveHead'
  \ },
  \ 'component_type': {
  \   'buffers': 'tabsel'
  \ },
  \ }
let g:lightline#bufferline#unnamed           ='[No Name]'
let g:lightline#bufferline#filename_modifier =':t'

let &t_SI ="\<Esc>[6 q"
let &t_SR ="\<Esc>[4 q"
let &t_EI ="\<Esc>[2 q"

nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()

inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

setlocal formatprg=hindent
 
