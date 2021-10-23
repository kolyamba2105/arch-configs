local lualine = require'lualine'

local setup_config = function ()
  local config = {
    options = {
      theme = 'gruvbox-material'
    },
    sections = {
      lualine_b = {},
      lualine_c = {},
      lualine_x = {}
    }
  }

  table.insert(config.sections.lualine_b, { 'branch' })

  table.insert(config.sections.lualine_b, {
    'diff',
    symbols = { added = '  ', modified = '  ', removed = '  ' }
  })

  table.insert(config.sections.lualine_c, { 'filename' })

  -- Use with native LSP
  table.insert(config.sections.lualine_x, {
    'diagnostics',
    sources = { 'nvim_lsp' },
    symbols = { error = '  ', hint = '  ', info = '  ', warn = '  ' }
  })

  -- Use with CoC
  -- table.insert(config.sections.lualine_x, { 'g:coc_status' })

  table.insert(config.sections.lualine_x, { 'filetype' })


  return config
end

lualine.setup(setup_config())

vim.cmd [[au BufEnter,BufWinEnter,WinEnter,CmdwinEnter * if bufname('%') == "NvimTree" | set laststatus=0 | else | set laststatus=2 | endif]]
