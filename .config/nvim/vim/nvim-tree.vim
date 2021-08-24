let g:nvim_tree_follow = 1
let g:nvim_tree_git_hl = 1
let g:nvim_tree_highlight_opened_files = 1
let g:nvim_tree_ignore = ['.git', 'node_modules']
let g:nvim_tree_update_cwd = 1
let g:nvim_tree_width = 60

command! ToggleDefault lua require'toggle-tree'.toggle_default()
command! ToggleCentered lua require'toggle-tree'.toggle_centered()

nnoremap <C-a>      :ToggleCentered<CR>
nnoremap <C-n>      :ToggleDefault<CR>
nnoremap <leader>n  :NvimTreeFindFile<CR>
nnoremap <leader>r  :NvimTreeRefresh<CR>
