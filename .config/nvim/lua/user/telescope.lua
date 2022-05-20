local opts = { noremap = true, silent = true }

vim.api.nvim_set_keymap('n', '<leader>tb', '<cmd>Telescope buffers<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>tc', '<cmd>Telescope command_history<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>tf', '<cmd>Telescope find_files<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>tg', '<cmd>Telescope live_grep<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>th', '<cmd>Telescope help_tags<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>tj', '<cmd>Telescope jumplist<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>tm', '<cmd>Telescope marks<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>tr', '<cmd>Telescope grep_string<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>ts', '<cmd>Telescope search_history<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>tt', '<cmd>Telescope file_browser<CR>', opts)

-- Git
vim.api.nvim_set_keymap('n', '<leader>gc', '<cmd>Telescope git_commits<CR>', opts)
vim.api.nvim_set_keymap('n', '<leader>gs', '<cmd>Telescope git_status<CR>', opts)

require('telescope').setup {
  defaults = {
    layout_strategy = 'vertical',
  },
  pickers = {
    buffers = {
      mappings = {
        i = {
          ['<c-d>'] = "delete_buffer",
        },
      },
    },
    find_files = {
      hidden = true,
    },
    file_browser = {
      hidden = true,
    },
  },
}

require('telescope').load_extension('ui-select')
