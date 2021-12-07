local opts = { noremap = true, silent = true }

vim.api.nvim_set_keymap('n', 'fc', '<cmd>Telescope command_history<CR>', opts)
vim.api.nvim_set_keymap('n', 'ff', '<cmd>Telescope find_files<CR>', opts)
vim.api.nvim_set_keymap('n', 'fg', '<cmd>Telescope live_grep<CR>', opts)
vim.api.nvim_set_keymap('n', 'fh', '<cmd>Telescope help_tags<CR>', opts)
vim.api.nvim_set_keymap('n', 'fj', '<cmd>Telescope jumplist<CR>', opts)
vim.api.nvim_set_keymap('n', 'fm', '<cmd>Telescope marks<CR>', opts)
vim.api.nvim_set_keymap('n', 'fs', '<cmd>Telescope search_history<CR>', opts)
vim.api.nvim_set_keymap('n', 'ft', '<cmd>Telescope file_browser<CR>', opts)

require('telescope').setup {
  pickers = {
    find_files = {
      hidden = true,
    },
    file_browser = {
      hidden = true,
    },
  },
}
