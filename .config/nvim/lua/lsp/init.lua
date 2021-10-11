local common = require('lsp/common')
local diagnostic_settings = require('lsp/diagnostic')
local elm_config = require('lsp/elm')
local haskell_config = require('lsp/haskell')
local html_settings = require('lsp/html')
local json_settings = require('lsp/json')
local typescript_settings = require('lsp/typescript')

local function setup_servers()
  require'lspinstall'.setup()

  require('lspconfig/configs').haskell = haskell_config
  require('lspconfig').haskell.setup { on_attach = common.on_attach }

  require('lspconfig/configs').elmLS = elm_config
  require('lspconfig').elmLS.setup { on_attach = common.on_attach }

  local servers = require'lspinstall'.installed_servers()

  for _, server in pairs(servers) do
    local config = {
      on_attach = common.on_attach,
      capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
    }

    if server == 'diagnosticls' then
      config = diagnostic_settings(common.on_attach)
    end

    if server == 'json' then
      config = json_settings(common.on_attach)
    end

    if server == 'html' then
      config = html_settings(common.on_attach)
    end

    if server == 'typescript' then
      config = typescript_settings(common.on_attach)
    end

    require'lspconfig'[server].setup(config)
  end
end

setup_servers()

require'lspinstall'.post_install_hook = function ()
  setup_servers()
  vim.cmd('bufdo e')
end
