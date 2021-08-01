local lualine = require'lualine'

lualine.setup {
  options = {
    -- theme = 'nord'
  }
}

vim.cmd [[au BufEnter,BufWinEnter,WinEnter,CmdwinEnter * if bufname('%') == "NvimTree" | set laststatus=0 | else | set laststatus=2 | endif]]
