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
  vim.fn.sign_define('LspDiagnosticsSignWarning', { text='', texthl='LspDiagnosticsWarning' })
end

local M = {}

M.on_attach = function(client, bufnr)
  define_signs()

  common.buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  local opts = { noremap = true, silent = true }

  common.buf_set_keymap('n', '<C-j>', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  common.buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  common.buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  common.buf_set_keymap('n', 'ga', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  common.buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  common.buf_set_keymap('n', 'ge', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  common.buf_set_keymap('n', 'gn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  common.buf_set_keymap('n', 'gq', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  common.buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)

  vim.cmd('command! Format lua vim.lsp.buf.formatting_sync(nil, 5000)')
  vim.cmd('command! Type lua vim.lsp.buf.type_definition()')

  if client.resolved_capabilities.document_formatting then
    vim.api.nvim_command [[augroup Format]]
    vim.api.nvim_command [[autocmd! * <buffer>]]
    vim.api.nvim_command [[autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync(nil, 5000)]]
    vim.api.nvim_command [[augroup END]]
  end
end

return M
