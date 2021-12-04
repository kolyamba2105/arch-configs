source ~/.config/nvim/vim/basics.vim
source ~/.config/nvim/vim/mappings.vim
source ~/.config/nvim/vim/plugins.vim

lua << EOF
require('buffer-line')
require('color-scheme')
require('completion')
require('format')
require('git-integration')
require('indent-line')
require('lsp')
require('status-line')
require('tele-scope')
require('term')
require('treesitter')
EOF
