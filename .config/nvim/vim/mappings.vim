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

" Close current buffer/window
nnoremap <leader>c :close<CR>

" Save file
nnoremap <leader>w :w<CR>
nnoremap <leader>W :wall<CR>

" Clear search input
nnoremap <silent> Z :noh<CR>

" Quickly insert an empty new line without entering insert mode
nnoremap <leader>o o<Esc>
nnoremap <leader>O O<Esc>

" Search for visually selected text
vnoremap // y/\V<C-R>=escape(@",'/\')<CR><CR>
