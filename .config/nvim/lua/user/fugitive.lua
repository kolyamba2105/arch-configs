local opts = { noremap = true, silent = true }

vim.api.nvim_set_keymap('n', '<leader>gg', '<cmd>Git<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>gb', '<cmd>Git blame<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>gp', '<cmd>Git push<CR>', opts)

vim.api.nvim_set_keymap('n', '<leader>gf', '<cmd>diffget //2<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>gj', '<cmd>diffget //3<CR>', opts)
