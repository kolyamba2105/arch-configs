local common = require('common')

vim.lsp.handlers['textDocument/publishDiagnostics'] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    virtual_text = {
      prefix = '',
      spacing = 0
    },
    signs = true,
    underline = true,
    update_in_insert = false
  }
)

local function define_signs()
  vim.fn.sign_define('LspDiagnosticsSignError', { text='', texthl='LspDiagnosticsError' })
  vim.fn.sign_define('LspDiagnosticsSignHint', { text='', texthl='LspDiagnosticsHint' })
  vim.fn.sign_define('LspDiagnosticsSignInformation', { text='', texthl='LspDiagnosticsInformation' })
  vim.fn.sign_define('LspDiagnosticsSignWarning', { text='', texthl='LspDiagnosticsWarning' })
end

local M = {}

M.on_attach = function()
  define_signs()

  common.buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  local opts = { noremap = true, silent = true }

  common.buf_set_keymap('n', '<C-j>', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  common.buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  common.buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  common.buf_set_keymap('n', 'ga', '<cmd>Telescope lsp_code_actions<CR>', opts)
  common.buf_set_keymap('n', 'gd', '<cmd>Telescope lsp_definitions<CR>', opts)
  common.buf_set_keymap('n', 'ge', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  common.buf_set_keymap('n', 'gf', '<cmd>Format<CR>', opts)
  common.buf_set_keymap('n', 'gl', '<cmd>Telescope lsp_document_diagnostics<CR>', opts)
  common.buf_set_keymap('n', 'gn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  common.buf_set_keymap('n', 'gr', '<cmd>Telescope lsp_references<CR>', opts)
  common.buf_set_keymap('n', 'gs', '<cmd>Telescope lsp_document_symbols<CR>', opts)
  common.buf_set_keymap('n', 'gt', '<cmd>Telescope lsp_type_definitions<CR>', opts)
  common.buf_set_keymap('n', 'gw', '<cmd>Telescope lsp_dynamic_workspace_symbols<CR>', opts)
end

M.disable_formatting = function (client)
  client.resolved_capabilities.document_formatting = false
  client.resolved_capabilities.document_range_formatting = false
end

M.capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())

M.default_config = {
  on_attach = M.on_attach,
  capabilities = M.capabilities
}

M.no_formatting_config = {
  on_attach = function (client)
    M.on_attach()
    M.disable_formatting(client)
  end,
  capabilities = M.capabilities
}

return M
