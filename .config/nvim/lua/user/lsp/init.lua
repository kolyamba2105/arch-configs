local common = require('user.lsp.common')

common.diagnostic_config()

require('lspconfig').bashls.setup(common.default_config)
require('lspconfig').cssls.setup(common.no_formatting_config)
require('lspconfig').cssmodules_ls.setup(common.default_config)
require('lspconfig').efm.setup(require('user.lsp.efm'))
require('lspconfig').emmet_ls.setup(require('user.lsp.emmet'))
require('lspconfig').eslint.setup(common.no_formatting_config)
require('lspconfig').html.setup(common.no_formatting_config)
require('lspconfig').jsonls.setup(common.no_formatting_config)
require('lspconfig').sumneko_lua.setup(require('user.lsp.lua'))
require('lspconfig').tailwindcss.setup(common.default_config)
require('lspconfig').yamlls.setup(common.no_formatting_config)

require('user.lsp.typescript')
