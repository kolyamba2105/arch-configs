nmap <silent> <C-j> <Plug>(coc-diagnostic-next)
nmap <silent> <C-k> <Plug>(coc-diagnostic-prev)
nmap <silent> K     :call <SID>show_documentation()<CR>
nmap <silent> ga    <Plug>(coc-codeaction)
nmap <silent> gd    <Plug>(coc-definition)
nmap <silent> gf    :call CocAction('format')<CR>
nmap <silent> gi    :CocCommand tsserver.organizeImports<CR>
nmap <silent> gn    <Plug>(coc-rename)
nmap <silent> gr    <Plug>(coc-references)
nmap <silent> gy    <Plug>(coc-type-definition)

inoremap          <expr> <S-TAB>    pumvisible() ? "\<C-p>" : "\<C-h>"
inoremap <silent> <expr> <TAB>      pumvisible() ? "\<C-n>" : <SID>check_back_space() ? "\<TAB>" : coc#refresh()
inoremap <silent> <expr> <c-space>  coc#refresh()

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

