vim.g.bufferline = {
  animation = false,
  auto_hide = true,
  clickable = false,
  closable = false,
  icon_close_tab = '',
  icon_close_tab_modified = '',
  icon_separator_active = '',
  icon_separator_inactive = '',
  maximum_padding = 12,
  no_name_title = nil,
  tabpages = false
}

local opts = { noremap = true, silent = true }

vim.api.nvim_set_keymap('n', '<C-c>', '<cmd>BufferClose<CR>', opts)
vim.api.nvim_set_keymap('n', '<C-h>', '<cmd>BufferPrevious<CR>', opts)
vim.api.nvim_set_keymap('n', '<C-l>', '<cmd>BufferNext<CR>', opts)
vim.api.nvim_set_keymap('n', '<C-p>', '<cmd>BufferPick<CR>', opts)
