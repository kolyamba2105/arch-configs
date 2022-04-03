local common = require('user.lsp.common')

return {
  capabilities = common.capabilities,
  filetypes = { 'html', 'css', 'javascriptreact', 'typescriptreact' }
}
