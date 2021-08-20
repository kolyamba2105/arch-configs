local lualine = require'lualine'

local config = {
  options = {
    theme = 'gruvbox_material'
  },
  sections = {
    lualine_b = {},
    lualine_c = {},
    lualine_x = {}
  }
}

vim.cmd [[au BufEnter,BufWinEnter,WinEnter,CmdwinEnter * if bufname('%') == "NvimTree" | set laststatus=0 | else | set laststatus=2 | endif]]

table.insert(config.sections.lualine_b, { 'branch' })

table.insert(config.sections.lualine_b, {
  'diff',
  symbols = { added = '  ', modified = '  ', removed = '  ' }
})

table.insert(config.sections.lualine_c, { 'filename' })

table.insert(config.sections.lualine_x, {
  'diagnostics',
  sources = { 'nvim_lsp' },
  symbols = { error = '  ', hint = '  ', info = '  ', warn = '  ' }
})

table.insert(config.sections.lualine_x, { 'filetype' })

lualine.setup(config)
