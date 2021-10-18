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

local opts = { noremap = true, silent = true }

vim.api.nvim_set_keymap('n', '<C-n>', '<cmd>NvimTreeToggle<CR>', opts)
