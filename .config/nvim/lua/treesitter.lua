vim.cmd [[
  set foldmethod=expr
  set foldexpr=nvim_treesitter#foldexpr()
]]

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
    enable = true
  },
  incremental_selection = {
    enable = false
  },
  indent = {
    enable = true
  }
}
