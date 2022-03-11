local common = require('lsp/common')

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
  end,
}
