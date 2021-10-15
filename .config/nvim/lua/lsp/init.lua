local common = require('lsp/common')
local elm_config = require('lsp/elm')
local haskell_config = require('lsp/haskell')
local html_settings = require('lsp/html')
local json_settings = require('lsp/json')
local typescript_settings = require('lsp/typescript')

local config = {
  on_attach = common.on_attach,
  capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
}

require('lspconfig/configs').haskell = haskell_config
require('lspconfig').haskell.setup(config)

require('lspconfig/configs').elmLS = elm_config
require('lspconfig').elmLS.setup(config)

require('nvim-lsp-installer').on_server_ready(function (server)
  local config = {
    on_attach = common.on_attach,
    capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
  }

  if server.name == 'json' then
    config = json_settings(common.on_attach)
  end

  if server.name == 'html' then
    config = html_settings(common.on_attach)
  end

  if server.name == 'tsserver' then
    config = typescript_settings(common.on_attach)
  end

  server:setup(config)

  vim.cmd [[ do User LspAttachBuffer ]]
end)

vim.cmd('command! InstallServers LspInstall bashls cssls eslint html jsonls tsserver vimls yamlls')
