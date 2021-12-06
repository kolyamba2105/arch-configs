local common = require('lsp/common')

-- TypeScript
_G.organize_imports = function ()
  vim.lsp.buf.execute_command {
    command = '_typescript.organizeImports',
    arguments = {
      vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf())
    }
  }
end

-- Setup
require('lspconfig').bashls.setup(common.default_config)
require('lspconfig').cssls.setup(common.no_formatting_config)
require('lspconfig').elmls.setup(common.default_config)
require('lspconfig').eslint.setup(common.no_formatting_config)
require('lspconfig').hls.setup(common.default_config)
require('lspconfig').html.setup(common.no_formatting_config)
require('lspconfig').jsonls.setup(common.no_formatting_config)
require('lspconfig').tailwindcss.setup(common.default_config)
require('lspconfig').tsserver.setup {
  on_attach = function (client)
    common.on_attach()
    common.disable_formatting(client)
    vim.cmd('command! Organize lua organize_imports()')
  end,
  capabilities = common.capabilities
}
require('lspconfig').yamlls.setup(common.no_formatting_config)
