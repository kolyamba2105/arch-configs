local common = require('user.lsp.common')

_G.organize_imports = function ()
  vim.lsp.buf.execute_command {
    command = '_typescript.organizeImports',
    arguments = {
      vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf())
    }
  }
end

return {
  capabilities = common.capabilities,
  on_attach = function (client, bufnr)
    common.on_attach(client, bufnr)
    common.disable_formatting(client)

    vim.cmd('command! Organize lua organize_imports()')
    vim.api.nvim_buf_set_keymap(bufnr or 0, 'n', '<leader>lo', '<cmd>Organize<CR>', { noremap = true, silent = true })
  end,
}
