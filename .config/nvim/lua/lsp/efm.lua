local prettier = {
  formatCommand = 'prettier ${--config-precedence:configPrecedence}'
}

local eslint = {
  lintCommand = 'eslint --format unix --stdin --stdin-filename ${INPUT}',
  lintFormats = {'%f:%l:%c: %m'},
  lintIgnoreExitCode = true,
  lintStdin = true
}

local languages = {
  css = { prettier },
  json = { prettier },
  markdown = { prettier },
  scss = { prettier },
  typescript = { prettier, eslint },
  typescriptreact = { prettier, eslint }
}

return function (on_attach)
  return {
    on_attach = on_attach,
    filetypes = vim.tbl_keys(languages),
    init_options = { codeAction = true, documentFormatting = true },
    settings = {
      languages = languages,
      log_level = 1,
      log_file = '~/efm.log'
    },
    root_dir = require('lspconfig').util.root_pattern('yarn.lock', '.git')
  }
end
