local lualine = require('lualine')

local setup_config = function()
  local config = {
    options = {
      globalstatus = true,
      section_separators = { left = '', right = '' },
      component_separators = { left = '', right = '' },
      theme = 'gruvbox-material',
    },
    sections = {
      lualine_b = {},
      lualine_c = {},
      lualine_x = {},
    }
  }

  table.insert(config.sections.lualine_b, { 'branch' })
  table.insert(config.sections.lualine_b, { 'diff' })
  table.insert(config.sections.lualine_c, { 'filename' })
  table.insert(config.sections.lualine_c, { 'diagnostics', sources = { 'nvim_diagnostic' } })
  table.insert(config.sections.lualine_x, { 'filetype' })

  return config
end

lualine.setup(setup_config())
