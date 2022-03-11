local common = require('lsp/common')

local prettier_config = {
  formatCommand = 'prettier --find-config-path --stdin-filepath ${INPUT}',
  formatStdin = true,
}

return {
  cmd = { 'efm-langserver' },
  capabilities = common.capabilities,
  init_options = { documentFormatting = true },
  on_attach = common.on_attach,
  settings = {
    languages = {
      css = { prettier_config },
      html = { prettier_config },
      javascript = { prettier_config },
      javascriptreact = { prettier_config },
      json = { prettier_config },
      sass = { prettier_config },
      scss = { prettier_config },
      typescript = { prettier_config },
      typescriptreact = { prettier_config },
      yaml = { prettier_config }
    },
    rootMarkers = { '.git', 'package.json' },
  },
}
