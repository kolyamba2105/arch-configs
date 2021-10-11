let g:floaterm_height=0.8
let g:floaterm_width=0.8


nnoremap <silent> tj :FloatermPrev<CR>
nnoremap <silent> tk :FloatermNext<CR>
nnoremap <silent> tn :FloatermNew<CR>
nnoremap <silent> tt :FloatermToggle<CR>

tnoremap <silent> tj <C-\><C-n>:FloatermPrev<CR>
tnoremap <silent> tk <C-\><C-n>:FloatermNext<CR>
tnoremap <silent> tn <C-\><C-n>:FloatermNew<CR>
tnoremap <silent> tt <C-\><C-n>:FloatermToggle<CR>
