local lsp_config = require'lspconfig'
local lsp_install = require'lspinstall'

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    virtual_text = {
      prefix = "",
      spacing = 0,
    },
    signs = true,
    underline = true,
  }
)

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
  vim.cmd("bufdo e")
end
