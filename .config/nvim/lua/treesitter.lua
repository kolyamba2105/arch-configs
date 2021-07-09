require'nvim-treesitter.configs'.setup {
    ensure_installed = {
      'bash',
      'comment',
      'css',
      'dockerfile',
      'fish',
      'html',
      'javascript',
      'jsdoc',
      'json',
      'jsonc',
      'lua',
      'scss',
      'tsx',
      'typescript',
      'yaml'
    },
    highlight = {
      enable = true,
    },
    incremental_selection = {
      enable = true,
      disable = { 'lua' },
      keymaps = {
        init_selection = 'gnn',
        node_decremental = 'grm',
        node_incremental = 'grn',
        scope_incremental = 'grc'
      }
    },
    indent = {
      enable = true,
    },
    textobjects = {
      select = {
        enable = true,
        keymaps = {
          ['bi'] = '@block.inner',
          ['bo'] = '@block.outer',
          ['fi'] = '@function.inner',
          ['fo'] = '@function.outer',
          ['si'] = '@statement.inner',
          ['so'] = '@statement.outer'
        }
      }
    }
}
