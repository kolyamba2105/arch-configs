source ~/.config/nvim/vim/basics.vim
source ~/.config/nvim/vim/mappings.vim
source ~/.config/nvim/vim/plugins.vim

lua << EOF
require('color-scheme')
require('completion')
require('git-integration')
require('indent-line')
require('lsp')
require('status-line')
require('tele-scope')
require('treesitter')
EOF
