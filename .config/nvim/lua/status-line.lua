local lualine = require('lualine')

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
  table.insert(config.sections.lualine_b, { 'diff' })
  table.insert(config.sections.lualine_c, { 'filename' })
  table.insert(config.sections.lualine_x, { 'diagnostics', sources = { 'nvim_lsp' } })
  table.insert(config.sections.lualine_x, { 'filetype' })

  return config
end

lualine.setup(setup_config())
