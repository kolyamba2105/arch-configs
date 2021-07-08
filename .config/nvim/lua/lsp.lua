local lsp_config = require'lspconfig'
local lsp_install = require'lspinstall'

local function setup_servers()
  lsp_install.setup()
  local servers = lsp_install.installed_servers()
  for _, server in pairs(servers) do
    lsp_config[server].setup {}
  end
end

setup_servers()

lsp_install.post_install_hook = function()
  setup_servers()
  vim.cmd('bufdo e')
end

vim.lsp.handlers['textDocument/publishDiagnostics'] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    virtual_text = {
      prefix = '',
      spacing = 0,
    },
    signs = true,
    underline = true,
  }
)

local on_attach = function(client)
  if client.resolved_capabilities.document_formatting then
    vim.api.nvim_command [[augroup Format]]
    vim.api.nvim_command [[autocmd! * <buffer>]]
    vim.api.nvim_command [[autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_seq_sync()]]
    vim.api.nvim_command [[augroup END]]
  end
end

lsp_config.tsserver.setup {
  on_attach = on_attach,
  filetypes = {
    'typescript',
    'typescriptreact'
  }
}

lsp_config.diagnosticls.setup {
  on_attach = on_attach,
  filetypes = {
    'typescript',
    'typescriptreact'
  },
  init_options = {
    filetypes = {
      typescript = 'prettier',
      typescriptreact = 'prettier'
    },
    formatters = {
      prettier = {
        command = 'prettier',
        args = { '--stdin-filepath', '%filename' }
      }
    },
    formatFiletypes = {
      typescript = 'prettier',
      typescriptreact = 'prettier'
    }
  },
}
