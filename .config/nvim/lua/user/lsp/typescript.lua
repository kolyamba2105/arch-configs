local common = require('user.lsp.common')

require('typescript').setup({
  disable_formatting = true,
  server = {
    capabilities = common.capabilities,
    on_attach = function(client, bufnr)
      common.on_attach(client, bufnr)

      vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>lo', '<cmd>TypescriptOrganizeImports<CR>', { noremap = true, silent = true })
      vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>lu', '<cmd>TypescriptRemoveUnused<CR>', { noremap = true, silent = true })
    end,
  }
})
