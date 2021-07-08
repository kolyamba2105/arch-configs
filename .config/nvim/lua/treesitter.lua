require'nvim-treesitter.configs'.setup {
    ensure_installed = 'all',
    highlight = {
      enable = true,
    },
    incremental_selection = {
      enable = true,
      disable = {
        'cpp',
        'lua'
      },
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
