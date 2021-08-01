let g:nvim_tree_auto_open = 0
let g:nvim_tree_follow = 1
let g:nvim_tree_highlight_opened_files = 1
let g:nvim_tree_ignore = ['.git']
let g:nvim_tree_update_cwd = 1
let g:nvim_tree_width = 60

nnoremap <C-n>      :lua require'toggle-tree'.toggle()<CR>
nnoremap <leader>r  :NvimTreeRefresh<CR>
nnoremap <leader>n  :NvimTreeFindFile<CR>
