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

_G.organize_imports = function ()
  local bufnr = vim.api.nvim_get_current_buf()

  local params = {
    command = '_typescript.organizeImports',
    arguments = {
      vim.api.nvim_buf_get_name(bufnr)
    }
  }

  vim.lsp.buf.execute_command(params)
end

local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  local opts = { noremap = true, silent = true }

  buf_set_keymap('n', '<C-j>',  ':Lspsaga diagnostic_jump_next<CR>',      opts)
  buf_set_keymap('n', '<C-k>',  ':Lspsaga diagnostic_jump_prev<CR>',      opts)
  buf_set_keymap('n', 'K',      ':Lspsaga hover_doc<CR>',                 opts)
  buf_set_keymap('n', 'ga',     ':Lspsaga code_action<CR>',               opts)
  buf_set_keymap('n', 'gd',     '<cmd>lua vim.lsp.buf.definition()<CR>',  opts)
  buf_set_keymap('n', 'ge',     ':Lspsaga show_line_diagnostics<CR>',     opts)
  buf_set_keymap('n', 'gn',     '<cmd>lua vim.lsp.buf.rename()<CR>',      opts)
  buf_set_keymap('n', 'gr',     '<cmd>lua vim.lsp.buf.references()<CR>',  opts)

  vim.cmd('command! Format lua vim.lsp.buf.formatting_seq_sync()')
  vim.cmd('command! Organize lua organize_imports()')

  if client.resolved_capabilities.document_formatting then
    vim.api.nvim_command [[augroup Format]]
    vim.api.nvim_command [[autocmd! * <buffer>]]
    vim.api.nvim_command [[autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_seq_sync()]]
    vim.api.nvim_command [[augroup END]]
  end
end

local capabilities = vim.lsp.protocol.make_client_capabilities()

local typescript_settings = {
  capabilities = capabilities,
  on_attach = on_attach,
  filetypes = {
    'typescript',
    'typescriptreact'
  }
}

local diagnostic_settings = {
  capabilities = capabilities,
  on_attach = on_attach,
  filetypes = {
    'css',
    'json',
    'markdown',
    'scss',
    'typescript',
    'typescriptreact'
  },
  init_options = {
    filetypes = {
      typescript = 'eslint',
      typescriptreact = 'eslint'
    },
    formatters = {
      prettier = {
        command = 'prettier',
        args = { '--stdin-filepath', '%filename' }
      }
    },
    formatFiletypes = {
      css = 'prettier',
      json = 'prettier',
      markdown = 'prettier',
      scss = 'prettier',
      typescript = 'prettier',
      typescriptreact = 'prettier'
    },
    linters = {
      eslint = {
        args = { '--stdin', '--stdin-filename', '%filepath', '--format', 'json' },
        command = 'eslint_d',
        debounce = 100,
        rootPatterns = { '.git' },
        parseJson = {
          column = 'column',
          endColumn = 'endColumn',
          endLine = 'endLine',
          errorsRoot = '[0].messages',
          line = 'line',
          message = '[ESLint] ${message} [${ruleId}]',
          security = 'severity'
        },
        securities = {
          [1] = 'warning',
          [2] = 'error'
        },
        sourceName = 'eslint_d'
      }
    }
  },
}

local haskell_config = {
  default_config = {
    cmd = { 'haskell-language-server-wrapper', '--lsp' },
    filetypes = { 'haskell', 'lhaskell' },
    root_dir = require('lspconfig/util').root_pattern('*.cabal', 'stack.yaml', 'cabal.project', 'package.yaml', 'hie.yaml')
  }
}

local function setup_servers()
  require'lspinstall'.setup()

  require('lspconfig/configs').haskell = haskell_config
  require('lspconfig').haskell.setup { on_attach = on_attach }

  local servers = require'lspinstall'.installed_servers()

  for _, server in pairs(servers) do
    local config = { on_attach = on_attach }

    if server == 'tsserver' then
      config = typescript_settings
    end
    if server == 'diagnosticls' then
      config = diagnostic_settings
    end

    require'lspconfig'[server].setup(config)
  end
end

setup_servers()

require'lspinstall'.post_install_hook = function ()
  setup_servers()
  vim.cmd('bufdo e')
end
