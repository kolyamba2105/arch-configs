local opts = { noremap = true, silent = true }

vim.api.nvim_set_keymap('n', '<leader>gb', '<cmd>Telescope git_branches<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>gc', '<cmd>Telescope git_commits<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>gs', '<cmd>Telescope git_status<CR>', opts)

vim.api.nvim_set_keymap('n', '<leader>gg', '<cmd>Git<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>gf', '<cmd>diffget //2<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>gj', '<cmd>diffget //3<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>gx', '<cmd>Git blame<CR>', opts)
