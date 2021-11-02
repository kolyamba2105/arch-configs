local opts = { noremap = true, silent = true }

vim.api.nvim_set_keymap('n', 'mb', '<cmd>Git blame<CR>', opts)
vim.api.nvim_set_keymap('n', 'mf', '<cmd>diffget //2<CR>', opts)
vim.api.nvim_set_keymap('n', 'mj', '<cmd>diffget //3<CR>', opts)
vim.api.nvim_set_keymap('n', 'mm', '<cmd>Git<CR>', opts)
