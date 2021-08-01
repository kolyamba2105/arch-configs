local opts = { noremap = true, silent = true }

vim.api.nvim_set_keymap('n', 'ff', '<cmd>Telescope find_files<CR>', opts)
vim.api.nvim_set_keymap('n', 'fg', '<cmd>Telescope live_grep<CR>',  opts)
vim.api.nvim_set_keymap('n', 'fh', '<cmd>Telescope help_tags<CR>',  opts)

require('telescope').setup {
  defaults = {
    layout_strategy = 'vertical'
  },
  pickers = {
    find_files = {
      hidden = true,
      previewer = false
    }
  }
}

