let bufferline = get(g:, 'bufferline', {})

let bufferline.animation = v:false
let bufferline.auto_hide = v:true
let bufferline.clickable = v:false
let bufferline.closable = v:false
let bufferline.icon_close_tab = ''
let bufferline.icon_close_tab_modified = ''
let bufferline.icon_separator_active = ''
let bufferline.icon_separator_inactive = ''
let bufferline.maximum_padding = 12
let bufferline.no_name_title = 'Buffer'
let bufferline.tabpages = v:false

nnoremap <silent> <C-c> :BufferClose<CR>
nnoremap <silent> <C-h> :BufferPrevious<CR>
nnoremap <silent> <C-l> :BufferNext<CR>
nnoremap <silent> <C-p> :BufferPick<CR>
