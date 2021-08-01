local bufferline_state = require'bufferline.state'
local nvim_tree = require'nvim-tree'
local nvim_tree_view = require'nvim-tree.view'

local tree = {
  toggle = function ()
    if nvim_tree_view.win_open() then
      bufferline_state.set_offset(0)
      nvim_tree.close()
    else
      bufferline_state.set_offset(60, 'FileTree')
      nvim_tree.find_file(true)
    end
  end
}

return tree 
