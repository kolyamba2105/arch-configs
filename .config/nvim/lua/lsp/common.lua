local function diagnostic_config()
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

local M = {}

M.on_attach = function()
  diagnostic_config()

  local buf_set_keymap = function(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local buf_set_option = function(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  local opts = { noremap = true, silent = true }

  buf_set_keymap('n', ']g', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '[g', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'ga', '<cmd>Telescope lsp_code_actions<CR>', opts)
  buf_set_keymap('n', 'gd', '<cmd>Telescope lsp_definitions<CR>', opts)
  buf_set_keymap('n', 'ge', '<cmd>lua vim.diagnostic.open_float(0, { scope="line" })<CR>', opts)
  buf_set_keymap('n', 'gl', '<cmd>Telescope lsp_document_diagnostics<CR>', opts)
  buf_set_keymap('n', 'gn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>Telescope lsp_references<CR>', opts)
  buf_set_keymap('n', 'gs', '<cmd>Telescope lsp_document_symbols<CR>', opts)
  buf_set_keymap('n', 'gt', '<cmd>Telescope lsp_type_definitions<CR>', opts)
  buf_set_keymap('n', 'gw', '<cmd>Telescope lsp_dynamic_workspace_symbols<CR>', opts)
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
