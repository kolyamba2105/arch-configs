return function (on_attach)
  return {
    on_attach = on_attach,
    filetypes = {
      'css',
      'javascript',
      'javascriptreact',
      'json',
      'markdown',
      'scss',
      'typescript',
      'typescriptreact'
    },
    init_options = {
      filetypes = {
        javascript = 'eslint',
        javascriptreact = 'eslint',
        typescript = 'eslint',
        typescriptreact = 'eslint'
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
          sourceName = 'eslint'
        }
      }
    },
  }
end
