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

local M = {}

M.on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  local opts = { noremap = true, silent = true }

  buf_set_keymap('n', '<C-j>',  ':Lspsaga diagnostic_jump_next<CR>zzzv',  opts)
  buf_set_keymap('n', '<C-k>',  ':Lspsaga diagnostic_jump_prev<CR>zzzv',  opts)
  buf_set_keymap('n', 'K',      ':Lspsaga hover_doc<CR>',                 opts)
  buf_set_keymap('n', 'ga',     ':Lspsaga code_action<CR>',               opts)
  buf_set_keymap('n', 'gd',     '<cmd>lua vim.lsp.buf.definition()<CR>',  opts)
  buf_set_keymap('n', 'ge',     ':Lspsaga show_line_diagnostics<CR>',     opts)
  buf_set_keymap('n', 'gn',     '<cmd>lua vim.lsp.buf.rename()<CR>',      opts)
  buf_set_keymap('n', 'gr',     '<cmd>lua vim.lsp.buf.references()<CR>',  opts)

  vim.cmd('command! Format lua vim.lsp.buf.formatting_sync()')
  vim.cmd('command! Type lua vim.lsp.buf.type_definition()')

  if client.resolved_capabilities.document_formatting then
    vim.api.nvim_command [[augroup Format]]
    vim.api.nvim_command [[autocmd! * <buffer>]]
    vim.api.nvim_command [[autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()]]
    vim.api.nvim_command [[augroup END]]
  end
end

return M
