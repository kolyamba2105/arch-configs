local M = {}

M.diagnostic_config = function()
  vim.diagnostic.config {
    float = {
      border = 'single',
      show_header = false,
      source = 'always'
    },
    severity_sort = true,
    signs = true,
    underline = true,
    update_in_insert = false,
    virtual_text = false
  }

  vim.lsp.handlers['textDocument/signatureHelp'] = vim.lsp.with(vim.lsp.handlers.signature_help, {
    border = 'single'
  })
  vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, {
    border = 'single',
  })
end

M.on_attach = function(client, bufnr)
  local buf_set_keymap = function(...) vim.api.nvim_buf_set_keymap(bufnr or 0, ...) end
  local buf_set_option = function(...) vim.api.nvim_buf_set_option(bufnr or 0, ...) end

  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  local opts = { noremap = true, silent = true }

  buf_set_keymap('n', ']g', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '[g', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', '<leader>k', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', '<leader>la', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', '<leader>ld', '<cmd>Telescope lsp_definitions<CR>', opts)
  buf_set_keymap('n', '<leader>le', '<cmd>lua vim.diagnostic.open_float(0, { scope="line" })<CR>', opts)
  buf_set_keymap('n', '<leader>lf', '<cmd>lua vim.lsp.buf.format({ async = true })<CR>', opts)
  buf_set_keymap('n', '<leader>ll', '<cmd>Telescope diagnostics<CR>', opts)
  buf_set_keymap('n', '<leader>ln', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<leader>lr', '<cmd>Telescope lsp_references<CR>', opts)
  buf_set_keymap('n', '<leader>ls', '<cmd>Telescope lsp_document_symbols<CR>', opts)
  buf_set_keymap('n', '<leader>lt', '<cmd>Telescope lsp_type_definitions<CR>', opts)
  buf_set_keymap('n', '<leader>lw', '<cmd>Telescope lsp_dynamic_workspace_symbols<CR>', opts)

  if client.server_capabilities.document_formatting then
    vim.api.nvim_command [[augroup Format]]
    vim.api.nvim_command [[autocmd! * <buffer>]]
    vim.api.nvim_command [[autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_seq_sync()]]
    vim.api.nvim_command [[augroup END]]
  end
end

M.disable_formatting = function(client)
  client.server_capabilities.document_formatting = false
  client.server_capabilities.document_range_formatting = false
end

M.capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())

M.default_config = {
  on_attach = M.on_attach,
  capabilities = M.capabilities
}

M.no_formatting_config = {
  on_attach = function(client, bufnr)
    M.on_attach(client, bufnr)
    M.disable_formatting(client)
  end,
  capabilities = M.capabilities
}

return M
