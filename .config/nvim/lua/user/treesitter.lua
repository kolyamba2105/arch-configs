require('nvim-treesitter.configs').setup {
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
    enable = false,
  },
  indent = {
    enable = true,
  },
  textobjects = {
    select = {
      enable = true,
      lookahead = true,
      keymaps = {
        ["ac"] = "@class.outer",
        ["af"] = "@function.outer",
        ["ic"] = "@class.inner",
        ["if"] = "@function.inner",
      },
    },
  },
}

vim.wo.foldmethod = 'expr'
vim.wo.foldexpr = 'nvim_treesitter#foldexpr()'
