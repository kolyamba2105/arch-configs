return function (on_attach)
  return {
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
          command = 'eslint',
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
