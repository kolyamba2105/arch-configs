vim.cmd [[
  let g:nvim_tree_follow = 1
  let g:nvim_tree_git_hl = 1
  let g:nvim_tree_highlight_opened_files = 1
  let g:nvim_tree_ignore = ['.git', 'node_modules']
]]

require('nvim-tree').setup {
  update_focused_file = {
    enable = true,
  },
  view = {
    width = 60,
  }
}

vim.cmd [[
  command! ToggleDefault lua require'toggle-tree'.toggle_default()

  nnoremap <C-n>      :ToggleDefault<CR>
  nnoremap <leader>n  :NvimTreeFindFile<CR>
  nnoremap <leader>r  :NvimTreeRefresh<CR>
]]
