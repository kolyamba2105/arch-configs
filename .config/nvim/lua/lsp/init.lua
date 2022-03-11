local common = require('lsp/common')

common.diagnostic_config()

local efm_config = require('lsp/efm')
local typescript_config = require('lsp/typescript')

require('lspconfig').bashls.setup(common.default_config)
require('lspconfig').cssls.setup(common.no_formatting_config)
require('lspconfig').efm.setup(efm_config)
require('lspconfig').elmls.setup(common.default_config)
require('lspconfig').eslint.setup(common.no_formatting_config)
require('lspconfig').hls.setup(common.default_config)
require('lspconfig').html.setup(common.no_formatting_config)
require('lspconfig').jsonls.setup(common.no_formatting_config)
require('lspconfig').tailwindcss.setup(common.default_config)
require('lspconfig').tsserver.setup(typescript_config)
require('lspconfig').yamlls.setup(common.no_formatting_config)
