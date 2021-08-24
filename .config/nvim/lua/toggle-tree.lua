local bufferline_state = require'bufferline.state'
local nvim_tree = require'nvim-tree'
local nvim_tree_view = require'nvim-tree.view'

local toggle = function (size)
  if type(size) == 'number' then
    if nvim_tree_view.win_open() then
      bufferline_state.set_offset(0)
      nvim_tree.close()
    else
      bufferline_state.set_offset(size, 'FileTree')
      nvim_tree.find_file(true)
      vim.api.nvim_command(':NvimTreeResize ' .. size)
    end
  end
end

local M = {}

M.toggle_default = function ()
  return toggle(60)
end

M.toggle_centered = function ()
  return toggle(math.floor(vim.o.columns * 0.33))
end

return M
