nmap <silent> <C-j> <Plug>(coc-diagnostic-next)
nmap <silent> <C-k> <Plug>(coc-diagnostic-prev)
nmap <silent> K     :call <SID>show_documentation()<CR>
nmap <silent> ga    <Plug>(coc-codeaction)
nmap <silent> gd    <Plug>(coc-definition)
nmap <silent> gn    <Plug>(coc-rename)
nmap <silent> gr    <Plug>(coc-references)

inoremap          <expr> <S-TAB>    pumvisible() ? "\<C-p>" : "\<C-h>"
inoremap <silent> <expr> <TAB>      pumvisible() ? "\<C-n>" : <SID>check_back_space() ? "\<TAB>" : coc#refresh()
inoremap <silent> <expr> <c-space>  coc#refresh()

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1] =~# '\s'
endfunction

inoremap <silent> <nowait> <expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>"  : "\<Left>"
inoremap <silent> <nowait> <expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>"  : "\<Right>"
nnoremap <silent> <nowait> <expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0)                : "\<C-b>"
nnoremap <silent> <nowait> <expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1)                : "\<C-f>"
vnoremap <silent> <nowait> <expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0)                : "\<C-b>"
vnoremap <silent> <nowait> <expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1)                : "\<C-f>"

command! -nargs=0 Format    :call CocAction('format')
command! -nargs=0 Organize  :call CocAction('runCommand', 'editor.action.organizeImport')
command! -nargs=? Fold      :call CocAction('fold', <f-args>)
