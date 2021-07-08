vim.lsp.handlers['textDocument/publishDiagnostics'] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    virtual_text = {
      prefix = '',
      spacing = 0,
    },
    signs = true,
    underline = true,
  }
)

local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  local opts = { noremap = true, silent = true }

  buf_set_keymap('n', '<C-j>',  '<cmd>Lspsaga diagnostic_jump_next<CR>',  opts)
  buf_set_keymap('n', '<C-k>',  '<cmd>Lspsaga diagnostic_jump_prev<CR>',  opts)
  buf_set_keymap('n', 'K',      '<cmd>Lspsaga hover_doc<CR>',             opts)
  buf_set_keymap('n', 'ga',     '<cmd>Lspsaga code_action<CR>',           opts)
  buf_set_keymap('n', 'gd',     '<cmd>lua vim.lsp.buf.definition()<CR>',  opts)
  buf_set_keymap('n', 'gn',     '<cmd>lua vim.lsp.buf.rename()<CR>',      opts)
  buf_set_keymap('n', 'gr',     '<cmd>lua vim.lsp.buf.references()<CR>',  opts)

  if client.resolved_capabilities.document_formatting then
    buf_set_keymap('n', 'gf', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
  end
end

local typescript_settings = {
  on_attach = on_attach,
  filetypes = {
    'typescript',
    'typescriptreact'
  }
}

local diagnostic_settings = {
  on_attach = on_attach,
  filetypes = {
    'typescript',
    'typescriptreact'
  },
  init_options = {
    filetypes = {
      typescript = 'prettier',
      typescriptreact = 'prettier'
    },
    formatters = {
      prettier = {
        command = 'prettier',
        args = { '--stdin-filepath', '%filename' }
      }
    },
    formatFiletypes = {
      typescript = 'prettier',
      typescriptreact = 'prettier'
    }
  },
}

local function setup_servers()
  require'lspinstall'.setup()

  local servers = require'lspinstall'.installed_servers()

  for _, server in pairs(servers) do
    local config = {}

    if server == "tsserver" then
      config = typescript_settings
    end
    if server == "diagnosticls" then
      config = diagnostic_settings
    end

    require'lspconfig'[server].setup(config)
  end
end

setup_servers()

require'lspinstall'.post_install_hook = function ()
  setup_servers()
  vim.cmd("bufdo e")
end
