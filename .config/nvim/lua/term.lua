vim.g.floaterm_height = 0.8
vim.g.floaterm_width = 0.8

local opts = { noremap = true, silent = true }

vim.api.nvim_set_keymap('n', '<F12>', '<cmd>FloatermToggle<CR>', opts)
vim.api.nvim_set_keymap('t', '<F12>', '<C-\\><C-n><cmd>FloatermToggle<CR>', opts)
