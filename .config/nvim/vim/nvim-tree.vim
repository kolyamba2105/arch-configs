let g:nvim_tree_follow = 1
let g:nvim_tree_git_hl = 1
let g:nvim_tree_highlight_opened_files = 1
let g:nvim_tree_ignore = ['.git', 'node_modules']
let g:nvim_tree_update_cwd = 1
let g:nvim_tree_width = 45

nnoremap <C-n>      :lua require'toggle-tree'.toggle()<CR>
nnoremap <leader>r  :NvimTreeRefresh<CR>
nnoremap <leader>n  :NvimTreeFindFile<CR>
