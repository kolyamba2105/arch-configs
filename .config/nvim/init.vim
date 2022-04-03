source ~/.config/nvim/vim/basics.vim
source ~/.config/nvim/vim/mappings.vim
source ~/.config/nvim/vim/netrw.vim
source ~/.config/nvim/vim/plugins.vim

lua << EOF
require('user.cmp')
require('user.colorscheme')
require('user.fugitive')
require('user.gitsigns')
require('user.indent-blankline')
require('user.lsp')
require('user.lualine')
require('user.nvim-autopairs')
require('user.telescope')
require('user.treesitter')
EOF
